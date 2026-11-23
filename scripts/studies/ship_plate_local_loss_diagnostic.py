"""Precompute isolated-patch wrapper diagnostics; no embedded-damage acceptance.

This uses existing algorithms only. It neither repairs nor endorses the wrapper's
API579/conservatism claims. Original whole-field study artifacts remain untouched.
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

from digitalmodel.structural.structural_analysis.models import PlateGeometry, STEEL_AH36
from digitalmodel.structural.structural_analysis.plate_metal_loss_ffs import (
    assess_plate_local_loss, assess_plate_uniform_loss,
)

ROOT = Path(__file__).resolve().parents[2]
PARENT = PlateGeometry(1200., 600., 12.)
LOSSES = (0., 4., 8.)


def compute(patch_length, patch_breadth, stress):
    values = (patch_length, patch_breadth, stress)
    if any(isinstance(v, bool) or not isinstance(v, (int, float)) or
           not math.isfinite(v) or v <= 0 for v in values):
        raise ValueError('Finite positive dimensions and longitudinal stress required')
    if patch_length > PARENT.length or patch_breadth > PARENT.width:
        raise ValueError('Patch exceeds parent field')
    nominal = assess_plate_uniform_loss(PARENT, STEEL_AH36, 0., sigma_x=stress)
    curve = []
    for loss in LOSSES:
        patch = assess_plate_local_loss(PARENT, STEEL_AH36, loss, patch_length,
                                       patch_breadth, sigma_x=stress)
        uniform = assess_plate_uniform_loss(PARENT, STEEL_AH36, loss, sigma_x=stress)
        # Project only diagnostic quantities. Never publish wrapper verdicts,
        # Level2/RSF framing, automatic thresholds or unsupported conservatism.
        curve.append(dict(metal_loss_mm=loss, remaining_thickness_mm=12.-loss,
            isolated_patch_utilization=float(patch.utilization),
            isolated_patch_critical_MPa=float(patch.details['critical_stress_reduced_MPa']),
            whole_field_uniform_comparison_utilization=float(uniform.utilization),
            whole_field_uniform_comparison_critical_MPa=float(uniform.details['critical_stress_reduced_MPa']),
            parent_nominal_utilization=float(nominal.utilization)))
    return dict(case=dict(parent_length_mm=1200., parent_breadth_mm=600.,
        initial_thickness_mm=12., patch_length_mm=patch_length,
        patch_breadth_mm=patch_breadth, patch_aspect_ratio=patch_length/patch_breadth,
        sigma_x_MPa=stress, sigma_y_MPa=0., tau_MPa=0., fca_mm=0., gamma_m=1.15,
        parent_support='simply_supported', patch_support='artificial_simply_supported_edges',
        material='assumed models.STEEL_AH36; E206000 MPa, fy355 MPa, nu0.3',
        load_basis='fixed_stress_not_fixed_force'),
        status='illustrative_isolated_patch_diagnostic',
        acceptance_status='inapplicable_unvalidated_local_patch',
        minimum_remaining_thickness_mm=None, maximum_accepted_loss_mm=None,
        curve=curve,
        comparison_note='Uniform comparison thins entire parent; neither response is a validated embedded-damage solution')


def render(rows, path):
    import plotly.graph_objects as go
    fig = go.Figure()
    for i, row in enumerate(rows):
        c = row['case']; curve = row['curve']
        label = f"Patch {c['patch_length_mm']:g} x {c['patch_breadth_mm']:g} mm, stress {c['sigma_x_MPa']:g} MPa"
        for field, name in [('isolated_patch_utilization','isolated supported patch'),
                            ('whole_field_uniform_comparison_utilization','uniform full-parent comparison')]:
            fig.add_trace(go.Scatter(x=[p['metal_loss_mm'] for p in curve],
                y=[p[field] for p in curve], mode='lines+markers', name=name,
                visible=i==0, meta=label))
    fig.update_layout(title='Saved local-patch diagnostics: no damage acceptance',
        xaxis_title='Selected local thickness deduction (mm)', yaxis_title='Existing model utilization')
    html = fig.to_html(include_plotlyjs=True, full_html=True, div_id='diagnostic-chart')
    controls = '<div style="font:16px sans-serif;padding:20px"><b>ILLUSTRATIVE ONLY — LOCAL-DAMAGE ACCEPTANCE INAPPLICABLE.</b><p>Parent 1200 x 600 x 12 mm; assumed AH36; gamma1.15; FCA0; fixed stress; zero transverse stress/shear. Patch edges are artificial simply-supported boundaries. The second curve uniformly thins the entire parent as a comparison, not a validated bound. No minimum accepted thickness or embedded-damage capacity is provided.</p>'
    for key, options in [('patch_length_mm',(300,600,1200,900)), ('patch_breadth_mm',(150,300,600,450)), ('sigma_x_MPa',(50,100,150))]:
        controls += '<label>'+key.replace('_',' ')+'<select data-key="'+key+'">'
        controls += ''.join(f'<option value="{v}">{v}</option>' for v in options)+'</select></label> '
    controls += '<p id="diagnostic-status" role="status"></p></div>'
    js = '''<script>
const diagnosticRows=PAYLOAD;
const diagnosticSelects=[...document.querySelectorAll('select[data-key]')];
function retrieveDiagnostic(){
 const values=Object.fromEntries(diagnosticSelects.map(s=>[s.dataset.key,Number(s.value)]));
 const index=diagnosticRows.findIndex(r=>Object.entries(values).every(([k,v])=>r.case[k]===v));
 const row=diagnosticRows[index];
 document.querySelector('#diagnostic-status').textContent=row?
 'INAPPLICABLE for acceptance | Saved isolated-patch response. Zero-loss patch utilization '+row.curve[0].isolated_patch_utilization.toFixed(4)+'; parent '+row.curve[0].parent_nominal_utilization.toFixed(4)+'. Parent modes are omitted.':
 'UNCOMPUTED diagnostic combination. Acceptance remains INAPPLICABLE. No interpolation or solver execution.';
 Plotly.restyle('diagnostic-chart',{visible:diagnosticRows.flatMap((_,i)=>[i===index,i===index])});
}
diagnosticSelects.forEach(s=>s.addEventListener('change',retrieveDiagnostic));retrieveDiagnostic();
</script>'''.replace('PAYLOAD',json.dumps(rows,allow_nan=False).replace('<','\\u003c'))
    path.write_text(html.replace('<body>','<body>'+controls).replace('</body>',js+'</body>'),encoding='utf-8')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output',type=Path,required=True)
    args = parser.parse_args()
    if args.output.exists() and any(args.output.iterdir()):
        parser.error('Output must be new/empty to preserve previous runs')
    args.output.mkdir(parents=True,exist_ok=True)
    rows = [compute(a,b,s) for a,b,s in itertools.product((300.,600.,1200.),(150.,300.,600.),(50.,100.))]
    result = args.output/'precomputed.json'
    result.write_text(json.dumps(dict(schema_version=1,rows=rows),indent=2,allow_nan=False),encoding='utf-8')
    render(rows,args.output/'lookup.html')
    files = [Path(__file__),ROOT/'src/digitalmodel/structural/structural_analysis/plate_metal_loss_ffs.py',
             ROOT/'src/digitalmodel/structural/structural_analysis/buckling.py',ROOT/'src/digitalmodel/structural/structural_analysis/models.py']
    digest = hashlib.sha256(result.read_bytes()).hexdigest()
    manifest = dict(schema_version=1,run_id='ship-local-diagnostic-'+digest[:16],owner_repo='digitalmodel-data',
        executed_utc=datetime.now(timezone.utc).isoformat(),actual_host=platform.node(),python=platform.python_version(),
        workflow_revision=subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),
        workflow_dirty=bool(subprocess.check_output(['git','status','--porcelain'],cwd=ROOT,text=True).strip()),
        result_sha256=digest,lookup_sha256=hashlib.sha256((args.output/'lookup.html').read_bytes()).hexdigest(),
        rows=len(rows),points=sum(len(r['curve']) for r in rows),
        algorithm_hashes={p.relative_to(ROOT).as_posix():hashlib.sha256(p.read_bytes()).hexdigest() for p in files},
        applicability='isolated supported patch numerical diagnostic only; embedded local-damage acceptance inapplicable',
        inputs='authored synthetic geometry/stress; no client or manufacturer stock inputs')
    (args.output/'manifest.json').write_text(json.dumps(manifest,indent=2),encoding='utf-8')
    print(json.dumps(dict(run_id=manifest['run_id'],rows=len(rows),actual_host=platform.node())))


if __name__ == '__main__':
    main()
