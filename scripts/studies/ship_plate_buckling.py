# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Bounded research adapter over existing plate/panel buckling implementations.

No ingestion pipeline, engineering acceptance, local-patch model or solver dispatch.
Outputs must be retained by private digitalmodel-data, per existing owner contract.
"""
import argparse
import hashlib
import itertools
import json
import math
import platform
import subprocess
from datetime import datetime, timezone
from pathlib import Path

from digitalmodel.structural.structural_analysis.models import STEEL_AH36, PlateGeometry
from digitalmodel.structural.structural_analysis.buckling import PlateBucklingAnalyzer
from digitalmodel.structural.structural_analysis.panel_buckling import (
    StiffenerGeometry, StiffenedPanelGeometry, StiffenedPanelBucklingAnalyzer,
)

ROOT = Path(__file__).resolve().parents[2]
PROFILES = {'none': None, 'flatbar-200x10': (200, 10, 0, 0, 'flatbar'),
            'welded-tee-200x10-100x12': (200, 10, 100, 12, 'tee')}


def analyzer():
    return PlateBucklingAnalyzer(STEEL_AH36)


def eligibility(c):
    required = ('length_mm', 'breadth_mm', 'initial_thickness_mm', 'sigma_x_MPa',
                'sigma_y_MPa', 'tau_MPa', 'gamma_m', 'web_loss_mm')
    if set(c) != set(required) | {'structure', 'profile', 'support', 'loss_model'}:
        return 'Case must match the exact input contract; extra parameters are not silently ignored'
    if any(k not in c or isinstance(c[k], bool) or not isinstance(c[k], (int, float))
           or not math.isfinite(c[k]) for k in required):
        return 'Missing or nonfinite numerical input'
    if any(c[k] <= 0 for k in required[:4]) or c['gamma_m'] < 1 or c['web_loss_mm'] < 0:
        return 'Positive geometry/compression, gamma >= 1 and nonnegative web loss required'
    if c['initial_thickness_mm'] < .5:
        return 'Initial thickness below 0.5 mm numerical evaluation bound'
    if c.get('support') != 'simply_supported' or c.get('loss_model') != 'whole_field_uniform':
        return 'Only actual simply-supported whole-field uniform thinning is evaluated'
    if c['sigma_y_MPa'] != 0 or c['tau_MPa'] != 0:
        return 'Transverse stress ignored by existing route; shear not qualified in this study'
    if c['length_mm'] < c['breadth_mm']:
        return 'Study envelope requires length/breadth >= 1; k=4 approximation'
    profile = c.get('profile')
    if profile not in PROFILES or c.get('structure') not in ('plate', 'panel'):
        return 'Unsupported structure/profile geometry'
    if c['structure'] == 'plate' and (profile != 'none' or c['web_loss_mm'] != 0):
        return 'Plate requires profile none and zero web loss'
    if c['structure'] == 'panel':
        p = PROFILES[profile]
        if p is None or c['web_loss_mm'] >= p[1]:
            return 'Panel requires a positive remaining web thickness'
    return None


def evaluate(c, thickness):
    if not math.isfinite(thickness) or not 0 < thickness <= c['initial_thickness_mm']:
        raise ValueError('remaining thickness outside positive initial range')
    plate = PlateGeometry(c['length_mm'], c['breadth_mm'], thickness)
    if c['structure'] == 'plate':
        a = analyzer()
        r = a.check_plate_buckling(plate, c['sigma_x_MPa'], gamma_m=c['gamma_m'])
        return dict(utilization=float(r.utilization), governing_mode='plate',
                    critical_MPa=float(r.critical_stress), elastic_MPa=float(a.elastic_buckling_stress(plate)))
    h, tw, bf, tf, st = PROFILES[c['profile']]
    g = StiffenedPanelGeometry(c['length_mm'], thickness,
        StiffenerGeometry(h, tw-c['web_loss_mm'], bf, tf, c['breadth_mm'], st), c['length_mm'])
    a = StiffenedPanelBucklingAnalyzer(STEEL_AH36)
    r = a.check_panel(g, c['sigma_x_MPa'], gamma_m=c['gamma_m'])
    return dict(utilization=float(r.utilization), governing_mode=r.governing_mode,
                critical_MPa=float(r.critical_stress), section=a.effective_section(g))


def compute(c):
    error = eligibility(c)
    if error:
        return dict(case=c, status='inapplicable', reason=error,
                    minimum_remaining_thickness_mm=None)
    nominal = c['initial_thickness_mm']
    # Bounded numerical envelope; this is not a practical minimum fabrication gauge.
    low = .5
    curve = [dict(remaining_thickness_mm=t, metal_loss_mm=nominal-t, **evaluate(c, t))
             for t in sorted(set([low, 2., 4., 6., 8., 10., nominal])) if t <= nominal]
    utils = [r['utilization'] for r in curve]
    if not all(math.isfinite(u) and u >= 0 for u in utils):
        return dict(case=c, status='inapplicable', reason='Nonfinite solver output',
                    minimum_remaining_thickness_mm=None)
    if any(b > a+1e-9 for a, b in zip(utils, utils[1:])):
        return dict(case=c, status='inapplicable', reason='Nonmonotone sampled response',
                    minimum_remaining_thickness_mm=None)
    result = dict(case=c, curve=curve, minimum_remaining_thickness_mm=None,
                  material=dict(name=STEEL_AH36.name, fy_MPa=STEEL_AH36.yield_strength,
                                E_MPa=STEEL_AH36.youngs_modulus, nu=STEEL_AH36.poissons_ratio),
                  load_basis='fixed_stress_not_fixed_force', fca_mm=0.,
                  criterion='utilization <= 1; existing DNV-RP-C201:2010 implementation',
                  scope='synthetic model screen; no asset or API579 acceptance')
    if utils[-1] > 1:
        return dict(result, status='no_passing_thickness')
    if utils[0] <= 1:
        return dict(result, status='threshold_below_evaluated_range')
    hi = nominal
    for _ in range(50):
        mid = (low+hi)/2
        if evaluate(c, mid)['utilization'] <= 1:
            hi = mid
        else:
            low = mid
    r = evaluate(c, hi)
    return dict(result, status='computed_model_screen' if c['structure'] == 'plate'
                else 'illustrative_panel_model', minimum_remaining_thickness_mm=hi if c['structure']=='plate' else None,
                illustrative_threshold_mm=hi if c['structure']=='panel' else None,
                threshold_utilization=r['utilization'], governing_mode=r['governing_mode'],
                validation='plate elastic formula, scaling and JO transition; panel intermediates only')


def lookup(rows, case):
    return next((r for r in rows if r['case'] == case),
                dict(status='uncomputed', minimum_remaining_thickness_mm=None,
                     reason='No exact precomputed combination; no interpolation or execution'))


def cases():
    # Author-selected synthetic geometry; no manufacturer product qualification implied.
    for profile, length, breadth, stress in itertools.product(PROFILES, (600.,1200.), (400.,600.), (50.,100.)):
        structure = 'plate' if profile == 'none' else 'panel'
        for web_loss in ((0.,) if structure == 'plate' else (0.,1.)):
            yield dict(structure=structure, profile=profile, length_mm=length,
                breadth_mm=breadth, initial_thickness_mm=12., sigma_x_MPa=stress,
                sigma_y_MPa=0., tau_MPa=0., support='simply_supported',
                loss_model='whole_field_uniform', gamma_m=1.15, web_loss_mm=web_loss)


REFERENCE_METHOD = 'mit_elastic_plate_reference_v1'
REFERENCE_SCHEMA = ROOT / 'data/ship_structure_reference/elastic_reference.schema.json'
REFERENCE_LIMIT = (
    'Ideal thin isotropic elastic plate only; elastic range, imperfections, '
    'renewal rules and design factors are unqualified. No asset FFS acceptance.'
)


def read_reference_json(path):
    """Read strict JSON without ambiguous keys or nonfinite numbers (RFC 8259)."""
    def unique(pairs):
        result = {}
        for key, value in pairs:
            if key in result:
                raise ValueError('Duplicate JSON object key')
            result[key] = value
        return result

    data = json.loads(Path(path).read_text(encoding='utf-8'), object_pairs_hook=unique)
    json.dumps(data, allow_nan=False)
    return data


def validate_reference_study(data):
    """Validate synthetic inputs, units and provenance without downloading sources."""
    from jsonschema import Draft202012Validator, FormatChecker, ValidationError
    from urllib.parse import urlsplit

    # RFC 8259 (2017), section 6: JSON excludes NaN and Infinity.
    data = json.loads(json.dumps(data, allow_nan=False))
    schema = json.loads(REFERENCE_SCHEMA.read_text(encoding='utf-8'))
    try:
        Draft202012Validator(schema, format_checker=FormatChecker()).validate(data)
    except ValidationError:
        # Validation diagnostics must not echo arbitrary input records or paths.
        raise ValueError('Invalid elastic reference input contract') from None
    keys = [s['key'] for s in data['sources']]
    if len(keys) != len(set(keys)):
        raise ValueError('Duplicate source identities')
    for source in data['sources']:
        pointer = urlsplit(source['url'])
        if not pointer.hostname or pointer.username or pointer.password:
            raise ValueError('Source requires a public URL without credentials')
        _ = pointer.port
    for material in data['materials'].values():
        if not set(material['source_keys']) <= set(keys):
            raise ValueError('Unknown material equation source')
    if any(c['material_id'] not in data['materials'] for c in data['cases']):
        raise ValueError('Unknown material identity')
    return data


def _reference_inapplicable(case):
    """Keep unsupported loss and loading models outside the reference domain."""
    if case['structure'] != 'plate':
        return 'panel thresholds unqualified; plate, web and flange losses remain separate inputs'
    if case['profile'] != 'none':
        return 'profile unsupported for a bare plate reference'
    if case['web_loss_mm'] or case['flange_loss_mm']:
        return 'stiffener loss cannot be applied to a bare plate'
    if case['loss_model'] != 'whole_field_uniform':
        return 'local loss requires independently validated embedded-damage supports'
    if case['support'] != 'simply_supported_free_transverse_expansion':
        return 'support must be simply supported with free transverse in-plane expansion'
    if case['load_basis'] != 'fixed_stress':
        return 'fixed stress required; fixed force is a different loading path'
    if case['sigma_y_MPa'] or case['tau_MPa']:
        return 'uniaxial longitudinal compression only'
    if case['plate_loss_mm'] >= case['initial_thickness_mm']:
        return 'positive remaining thickness required'
    return None


def _reference_row(case, material):
    """Compute elastic eigenvalues, never a design or renewal verdict."""
    from digitalmodel.structural.plate_capacity.elastic_buckling import (
        square_plate_elastic_buckling_stress,
    )

    row = dict(case=case, material=material, engineering_status='NOT_EVALUATED',
               minimum_remaining_thickness_mm=None,
               elastic_reference_crossing_mm=None, limitation=REFERENCE_LIMIT)
    reason = _reference_inapplicable(case)
    if reason:
        return dict(row, reference_status='INAPPLICABLE', reason=reason)
    try:
        alpha = case['span_mm'] / case['breadth_mm']
        if not math.isfinite(alpha) or alpha <= 0:
            raise ValueError('Unrepresentable aspect ratio')
        # MIT 2.080J (2013), Lecture 11, Eqs. 11.16-11.17:
        # k=(m/alpha+alpha/m)^2, n=1; minimum at continuous m=alpha.
        # The positive integer minimum is among floor(alpha), ceil(alpha),
        # with m>=1. No empirical coefficient or aspect-ratio cutoff is used.
        modes = {max(1, math.floor(alpha)), max(1, math.ceil(alpha))}
        k, mode = min(((m/alpha + alpha/m)**2, m) for m in modes)
        nominal = case['initial_thickness_mm']
        remaining = nominal - case['plate_loss_mm']
        # Reuse the independently tested square-plate kernel; multiply by k/4
        # from MIT Lecture 11 (2013), Eqs. 11.15-11.16 (square m=n=1: k=4).
        nominal_stress = square_plate_elastic_buckling_stress(
            youngs_modulus=material['youngs_modulus_MPa'],
            poisson_ratio=material['poisson_ratio'], thickness=nominal,
            side_length=case['breadth_mm']) * (k/4)
        # MIT Lecture 4 (2013), Eq. 4.60: D proportional to t^3, hence
        # sigma_cr=Ncr/t proportional to t^2 at fixed E, nu, a, b.
        retained = (remaining/nominal)**2
        critical = nominal_stress * retained
        # Algebraic inversion of the same equations at fixed imposed stress;
        # no safety factor, fabrication gauge, yield or acceptance limit.
        crossing = nominal * math.sqrt(case['sigma_x_MPa']/nominal_stress)
        utilization = case['sigma_x_MPa']/critical
        if not all(math.isfinite(x) and x > 0 for x in
                   (k, nominal_stress, critical, crossing, utilization, retained)):
            raise ValueError('Unrepresentable elastic reference result')
    except (ValueError, OverflowError, ZeroDivisionError):
        return dict(row, reference_status='INAPPLICABLE',
                    reason='Inputs or results outside floating-point evaluation range')
    within = crossing <= nominal
    return dict(row, reference_status='COMPUTED' if within else 'NO_ELASTIC_CROSSING_IN_RANGE',
                remaining_thickness_mm=remaining, longitudinal_half_waves=mode,
                buckling_coefficient=k, elastic_critical_stress_MPa=critical,
                nominal_elastic_critical_stress_MPa=nominal_stress,
                elastic_capacity_retained_fraction=retained,
                elastic_stress_ratio=utilization,
                elastic_reference_crossing_mm=crossing if within else None,
                reason='Analytical reference only; engineering qualification pending')


def compute_reference_study(data):
    """Precompute the explicitly supplied synthetic cases; no implicit grid."""
    data = validate_reference_study(data)
    return dict(schema_version=1, method=REFERENCE_METHOD, inputs=data,
                engineering_status='NOT_EVALUATED', limitation=REFERENCE_LIMIT,
                evidence_repository='digitalmodel-data',
                rows=[_reference_row(c, data['materials'][c['material_id']])
                      for c in data['cases']])


def lookup_reference(saved, selection):
    """Retrieve an exact saved combination without calculation or interpolation."""
    missing = dict(reference_status='UNCOMPUTED', engineering_status='NOT_EVALUATED',
                   minimum_remaining_thickness_mm=None, elastic_reference_crossing_mm=None,
                   reason='No exact saved combination; no interpolation or execution')
    if saved.get('method') != REFERENCE_METHOD or not isinstance(selection, dict):
        return missing
    for row in saved.get('rows', []):
        case = row['case']
        # Python regards True==1: reject booleans explicitly before comparing
        # numerical controls; integer and floating representations may match.
        if set(selection) != set(case) or any(isinstance(v, bool) for v in selection.values()):
            continue
        if selection == case:
            return row
    return missing


def render_reference(saved, output):
    """Render controls over saved rows only, using no external script or service."""
    import html

    rows = saved['rows']
    controls = []
    for key in rows[0]['case']:
        options = list(dict.fromkeys(row['case'][key] for row in rows))
        values = ''.join(f'<option value="{html.escape(json.dumps(v), quote=True)}">'
                         f'{html.escape(str(v))}</option>' for v in options)
        controls.append(f'<label>{html.escape(key)} <select data-key="{key}">{values}</select></label>')
    # Escape '<' before embedding JSON in a script to prevent closing tags.
    payload = json.dumps(rows, allow_nan=False).replace('<', '\\u003c')
    page = ''.join(('''<!doctype html><html lang="en"><meta charset="utf-8">
<title>Elastic plate reference lookup</title>
<style>body{font:16px sans-serif;max-width:70em;margin:2em auto;padding:1em}
#controls{display:flex;flex-wrap:wrap;gap:1em}label{display:grid}pre{white-space:pre-wrap}</style>
<h1>Saved elastic plate references</h1>
<p>Engineering minimum thickness: NOT_EVALUATED. Ideal elastic model only;
no renewal or asset FFS acceptance. Controls retrieve saved combinations only.</p>
<div id="controls">''', ''.join(controls), '''</div><pre id="result" role="status"></pre>
<script>
const rows=''', payload, ''';
const controls=[...document.querySelectorAll('select')];
function show(){
 const selection=Object.fromEntries(controls.map(s=>[s.dataset.key,JSON.parse(s.value)]));
 const row=rows.find(r=>Object.entries(selection).every(([k,v])=>r.case[k]===v));
 document.getElementById('result').textContent=row?
 JSON.stringify(row,(k,v)=>typeof v==='number'?Number(v.toFixed(1)):v,2):
 'UNCOMPUTED: no exact saved combination; no interpolation or execution. Engineering minimum thickness: NOT_EVALUATED.';
}
controls.forEach(s=>s.addEventListener('change',show));show();
</script></html>
'''))
    output.write_text(page, encoding='utf-8', newline='\n')


def write_reference_study(source, output):
    """Write a new evidence bundle without overwriting existing computation data."""
    if output.exists() and any(output.iterdir()):
        raise ValueError('Output must be new/empty to preserve previous runs')
    saved = compute_reference_study(read_reference_json(source))
    output.mkdir(parents=True, exist_ok=True)
    result = output / 'reference.json'
    result.write_text(json.dumps(saved, indent=2, allow_nan=False) + '\n', encoding='utf-8', newline='\n')
    render_reference(saved, output / 'lookup.html')
    paths = [Path(__file__), REFERENCE_SCHEMA,
             ROOT / 'src/digitalmodel/structural/plate_capacity/elastic_buckling.py']
    manifest = dict(method=REFERENCE_METHOD, evidence_repository='digitalmodel-data',
                    retention_status='execution_replica_not_archived',
                    engineering_status='NOT_EVALUATED',
                    input_sha256=hashlib.sha256(source.read_bytes()).hexdigest(),
                    result_sha256=hashlib.sha256(result.read_bytes()).hexdigest(),
                    lookup_sha256=hashlib.sha256((output / 'lookup.html').read_bytes()).hexdigest(),
                    workflow_revision=subprocess.check_output(
                        ['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
                    workflow_dirty=bool(subprocess.check_output(
                        ['git', 'diff', '--name-only'], cwd=ROOT, text=True).strip()),
                    executed_utc=datetime.now(timezone.utc).isoformat(),
                    algorithms={p.relative_to(ROOT).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
                                for p in paths}, rows=len(saved['rows']))
    (output / 'manifest.json').write_text(json.dumps(manifest, indent=2) + '\n', encoding='utf-8', newline='\n')
    return saved


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--output', type=Path)
    group = p.add_mutually_exclusive_group()
    group.add_argument('--reference-input', type=Path)
    group.add_argument('--reference-lookup', type=Path)
    p.add_argument('--selection', type=Path)
    args = p.parse_args(argv)
    if args.reference_lookup:
        if args.selection is None or args.output is not None:
            p.error('Saved lookup requires --selection and no --output')
        saved = read_reference_json(args.reference_lookup)
        manifest = read_reference_json(args.reference_lookup.parent / 'manifest.json')
        digest = hashlib.sha256(args.reference_lookup.read_bytes()).hexdigest()
        if digest != manifest['result_sha256']:
            raise ValueError('Saved reference digest mismatch')
        print(json.dumps(lookup_reference(saved, read_reference_json(args.selection)), allow_nan=False))
        return 0
    if args.selection is not None or args.output is None:
        p.error('Computation requires --output and no --selection')
    if args.reference_input:
        saved = write_reference_study(args.reference_input, args.output)
        print(json.dumps(dict(rows=len(saved['rows']), engineering_status='NOT_EVALUATED')))
        return 0
    rows = [compute(c) for c in cases()]
    args.output.mkdir(parents=True, exist_ok=True)
    output = args.output/'precomputed.json'
    output.write_text(json.dumps(dict(schema_version=1, rows=rows), indent=2, allow_nan=False), encoding='utf-8', newline='\n')
    code_paths = [Path(__file__), ROOT/'src/digitalmodel/structural/structural_analysis/buckling.py',
                  ROOT/'src/digitalmodel/structural/structural_analysis/panel_buckling.py',
                  ROOT/'src/digitalmodel/structural/structural_analysis/models.py']
    result_digest = hashlib.sha256(output.read_bytes()).hexdigest()
    revision = subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip()
    manifest = dict(schema_version=1, owner_repo='digitalmodel-data', run_id='ship-plate-synthetic-'+result_digest[:16],
        executed_utc=datetime.now(timezone.utc).isoformat(), workflow_git_revision=revision,
        workflow_dirty=bool(subprocess.check_output(['git','status','--porcelain'],cwd=ROOT,text=True).strip()),
        evaluated_lower_thickness_mm=.5, threshold_bracket_resolution_upper_bound_mm=(12-.5)/2**50,
        actual_host=platform.node(), platform=platform.platform(), python=platform.python_version(),
        result_sha256=result_digest, rows=len(rows),
        algorithms={f.relative_to(ROOT).as_posix():hashlib.sha256(f.read_bytes()).hexdigest() for f in code_paths},
        local_damage_status='inapplicable_unvalidated_imaginary_patch_supports',
        fixture_provenance='author-selected synthetic, no project/client/manufacturer input',
        material_provenance='existing models.STEEL_AH36; assumed isotropic engineering constants',
        class_rule_status='DNV current landing edition 2023-09 differs from implementation 2010; no clause audit',
        publication_scope='internal research; no engineering acceptance')
    (args.output/'manifest.json').write_text(json.dumps(manifest,indent=2), encoding='utf-8', newline='\n')
    render(rows, args.output/'lookup.html')
    print(json.dumps(dict(rows=len(rows), host=platform.node(), manifest=str(args.output/'manifest.json'))))


def render(rows, output):
    # Reuse installed Plotly reporting library; data-only controls, no solver/network calls.
    import plotly.graph_objects as go
    fig = go.Figure()
    for i, row in enumerate(rows):
        c = row['case']
        label = f"{c['structure']} / {c['profile']} / L={c['length_mm']:g}, b={c['breadth_mm']:g} mm / {c['sigma_x_MPa']:g} MPa / web loss {c['web_loss_mm']:g} mm"
        curve = row.get('curve',[])
        fig.add_trace(go.Scatter(x=[x['metal_loss_mm'] for x in curve],
            y=[x['utilization'] for x in curve], mode='lines+markers', visible=i==0, name=label))
    fig.add_hline(y=1,line_dash='dash')
    fig.update_layout(title='Precomputed synthetic buckling model screens', xaxis_title='Uniform plate thickness loss (mm)',
        yaxis_title='Model utilization', margin=dict(t=80))
    html=fig.to_html(include_plotlyjs=True,full_html=True,div_id='plate-chart')
    banner='<div style="font:16px sans-serif;padding:20px"><b>Research only. Buckling model is not asset FFS acceptance.</b><p>Fixed longitudinal stress, simply-supported whole field; AH36 assumed; t0=12 mm; gamma=1.15; FCA=0. Model criterion DNV-RP-C201:2010; current edition mapping pending. Panel results illustrative: full plate width; no pressure, effective-width reduction or girder interaction.</p><p>Dropdown contains exact precomputed combinations only. Any other geometry/load is UNCOMPUTED. Local damage length/breadth, bulb and angle profiles are INAPPLICABLE pending method qualification. Panel web loss is total thickness deduction; flange unchanged.</p></div>'
    choices = {'structure':['plate','panel'], 'profile':list(PROFILES)+['bulb-flat-unqualified'],
               'length_mm':[600,1200,900], 'breadth_mm':[400,600,500],
               'sigma_x_MPa':[50,100,150], 'web_loss_mm':[0,1,2],
               'loss_model':['whole_field_uniform','local_damage_unqualified']}
    controls='<div id="filters" style="font:16px sans-serif;padding:20px;display:flex;flex-wrap:wrap;gap:16px">'
    for key, options in choices.items():
        controls+=f'<label>{key.replace("_"," ")}<select data-key="{key}">'
        controls+=''.join(f'<option value="{x}">{x}</option>' for x in options)+'</select></label>'
    controls+='</div><p id="lookup-status" role="status" style="font:16px sans-serif;padding:20px"></p>'
    payload=json.dumps(rows,allow_nan=False).replace('<','\\u003c')
    js='''<script>
const savedRows=PAYLOAD;
const selectors=[...document.querySelectorAll('#filters select')];
function showSaved(){
 const selected=Object.fromEntries(selectors.map(s=>[s.dataset.key,s.value]));
 let index=savedRows.findIndex(r=>Object.entries(selected).every(([k,v])=>String(r.case[k])===v));
 const invalid=selected.loss_model==='local_damage_unqualified'||selected.profile==='bulb-flat-unqualified';
 if(invalid) index=-1;
 const row=index>=0?savedRows[index]:null;
 let status=invalid?'INAPPLICABLE: local damage and bulb geometry require qualified methods.':
   row?row.status:'UNCOMPUTED: no exact saved combination; no interpolation or solver execution.';
 if(row && row.minimum_remaining_thickness_mm!==null) status+=' | Minimum remaining thickness satisfying plate model: '+row.minimum_remaining_thickness_mm.toFixed(4)+' mm';
 if(row && row.illustrative_threshold_mm!==null && row.illustrative_threshold_mm!==undefined) status+=' | Illustrative panel crossing: '+row.illustrative_threshold_mm.toFixed(4)+' mm; minimum not independently established';
 document.querySelector('#lookup-status').textContent=status;
 Plotly.restyle('plate-chart',{visible:savedRows.map((_,i)=>i===index)});
 Plotly.relayout('plate-chart',{'title.text':row?'Saved model curve — '+row.status:'No computed curve for this selection'});
}
selectors.forEach(s=>s.addEventListener('change',showSaved));
showSaved();
</script>'''.replace('PAYLOAD',payload)
    html=html.replace('<body>','<body>'+banner+controls).replace('</body>',js+'</body>')
    output.write_text(html,encoding='utf-8',newline='\n')


if __name__ == '__main__':
    main()
