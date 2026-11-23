# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent mechanics and eligibility checks for the bounded study."""
import importlib.util
import math
from pathlib import Path

import pytest

PATH = Path(__file__).parents[2] / 'scripts/studies/ship_plate_buckling.py'
spec = importlib.util.spec_from_file_location('ship_study', PATH)
study = importlib.util.module_from_spec(spec)
spec.loader.exec_module(study)


def case(**updates):
    c = dict(structure='plate', profile='none', length_mm=600., breadth_mm=600.,
             initial_thickness_mm=12., sigma_x_MPa=50., sigma_y_MPa=0., tau_MPa=0.,
             support='simply_supported', loss_model='whole_field_uniform',
             gamma_m=1.15, web_loss_mm=0.)
    c.update(updates)
    return c


def test_elastic_square_plate_and_scaling():
    # Hand formula: k*pi^2*E/[12*(1-nu^2)]*(t/b)^2.
    expected = 4*math.pi**2*206000/(12*(1-.3**2))*(6/600)**2
    assert study.evaluate(case(), 6)['elastic_MPa'] == pytest.approx(expected)
    assert study.evaluate(case(), 3)['elastic_MPa'] == pytest.approx(expected/4)


def test_threshold_independent_elastic_solution():
    # At 50 MPa*1.15 < fy/2 the critical point lies on the elastic branch.
    expected = 600*math.sqrt(57.5*12*(1-.3**2)/(4*math.pi**2*206000))
    r = study.compute(case())
    assert r['minimum_remaining_thickness_mm'] == pytest.approx(expected, abs=1e-5)
    assert r['threshold_utilization'] == pytest.approx(1, abs=1e-5)


@pytest.mark.parametrize('updates', [dict(sigma_y_MPa=1), dict(tau_MPa=1),
    dict(support='clamped'), dict(loss_model='local_patch'), dict(profile='bulb'),
    dict(length_mm=float('nan')), dict(sigma_x_MPa=0), dict(web_loss_mm=-1)])
def test_unsupported_inputs_are_not_computed(updates):
    assert study.compute(case(**updates))['status'] == 'inapplicable'


def test_failed_nominal_has_no_threshold():
    r = study.compute(case(sigma_x_MPa=1000))
    assert r['status'] == 'no_passing_thickness'
    assert r['minimum_remaining_thickness_mm'] is None


def test_jo_transition_is_continuous():
    a = study.analyzer()
    assert a.johnson_ostenfeld(355/2) == pytest.approx(355/2)
    assert a.johnson_ostenfeld(355/2+1e-6) == pytest.approx(355/2, abs=2e-6)


def test_lookup_never_interpolates_or_computes():
    row = study.compute(case())
    assert study.lookup([row], case()) == row
    assert study.lookup([row], case(length_mm=601))['status'] == 'uncomputed'


def test_invalid_panel_web_loss_rejected():
    r = study.compute(case(structure='panel', profile='flatbar-200x10', web_loss_mm=10))
    assert r['status'] == 'inapplicable'


@pytest.mark.parametrize('updates', [dict(material='Grade A'), dict(fca_mm=2),
    dict(torsional_restraint_spacing_mm=100), dict(initial_thickness_mm=.1)])
def test_fixed_assumptions_cannot_be_overridden_silently(updates):
    assert study.compute(case(**updates))['status'] == 'inapplicable'


def test_booleans_are_not_physical_numeric_parameters():
    assert study.compute(case(gamma_m=True))['status'] == 'inapplicable'


# All new mechanics fixtures are authored synthetic inputs. Equations are from
# MIT 2.080J (Fall 2013), Lecture 11, Eqs. 11.15-11.17, and Lecture 4, Eq. 4.60.
def reference_input():
    import json
    return json.loads((PATH.parents[2] / 'data/ship_structure_reference/'
                       'elastic_reference_inputs.json').read_text(encoding='utf-8'))


def test_reference_square_hand_calculation_and_loss_scaling():
    """Hand calculation with synthetic E=120000 MPa, nu=0, a=b=100 mm.

    D=E*t^3/12=10000*t^3 N mm. Ncr=4*pi^2*D/100^2;
    sigma_cr=4*pi^2*t^2 MPa. At t=2,1,0.5 mm the stresses are
    157.9136704174297, 39.4784176043574, 9.86960440108936 MPa.
    At fixed 10 MPa, t_cross=sqrt(10/(4*pi^2))=0.5032921210 mm.
    These are elastic eigenvalues, with no yield or acceptance criterion.
    """
    result = study.compute_reference_study(reference_input())
    rows = result['rows'][:3]
    for row, expected in zip(rows, (157.9136704174297, 39.4784176043574,
                                    9.86960440108936)):
        assert row['elastic_critical_stress_MPa'] == pytest.approx(expected)
        assert row['elastic_reference_crossing_mm'] == pytest.approx(0.5032921210)
        assert row['minimum_remaining_thickness_mm'] is None
        assert row['engineering_status'] == 'NOT_EVALUATED'
    assert rows[1]['elastic_capacity_retained_fraction'] == pytest.approx(0.25)
    assert rows[2]['elastic_capacity_retained_fraction'] == pytest.approx(0.0625)
    assert result['engineering_status'] == 'NOT_EVALUATED'


def test_reference_rectangle_uses_integer_modes():
    """MIT Lecture 11 (2013), Eq. 11.16; synthetic a/b=1.5.

    m=1: k=(2/3+3/2)^2=169/36. m=2: k=(4/3+3/4)^2=625/144.
    m>=3 increases k, so m=2 governs. E=120000, nu=0, b=100, t=1:
    sigma_cr=(625/144)*pi^2=42.8368246575 MPa. The legacy k=4
    approximation would give 39.4784176044 MPa at this aspect ratio.
    """
    data = reference_input()
    data['cases'] = [dict(data['cases'][1], span_mm=150.0)]
    row = study.compute_reference_study(data)['rows'][0]
    assert row['longitudinal_half_waves'] == 2
    assert row['buckling_coefficient'] == pytest.approx(625/144)
    assert row['elastic_critical_stress_MPa'] == pytest.approx(42.8368246575)


def test_reference_material_is_explicit_and_controls_result():
    """MIT Lecture 4 (2013), Eq. 4.60: nu=0.3 divides stress by 0.91."""
    data = reference_input()
    data['cases'] = [dict(data['cases'][1], material_id='synthetic-nu03')]
    row = study.compute_reference_study(data)['rows'][0]
    assert row['elastic_critical_stress_MPa'] == pytest.approx(43.3828764883049)
    assert row['material'] == data['materials']['synthetic-nu03']


@pytest.mark.parametrize('updates,reason', [
    ({'structure': 'panel', 'profile': 'synthetic-tee', 'web_loss_mm': 0.2}, 'panel'),
    ({'flange_loss_mm': 0.2}, 'stiffener'),
    ({'support': 'clamped'}, 'support'),
    ({'loss_model': 'local_patch'}, 'local'),
    ({'load_basis': 'fixed_force'}, 'fixed'),
    ({'sigma_y_MPa': 1}, 'uniaxial'),
    ({'tau_MPa': 1}, 'uniaxial'),
    ({'plate_loss_mm': 2}, 'remaining'),
    ({'profile': 'bulb'}, 'profile'),
])
def test_reference_unqualified_combinations_never_yield_threshold(updates, reason):
    data = reference_input()
    data['cases'] = [dict(data['cases'][0], **updates)]
    row = study.compute_reference_study(data)['rows'][0]
    assert row['reference_status'] == 'INAPPLICABLE'
    assert reason in row['reason']
    assert row['elastic_reference_crossing_mm'] is None
    assert row['minimum_remaining_thickness_mm'] is None


@pytest.mark.parametrize('field,value', [
    ('span_mm', True), ('breadth_mm', 0), ('sigma_x_MPa', -1),
    ('plate_loss_mm', -0.1), ('web_loss_mm', float('nan')),
    ('flange_loss_mm', float('inf')), ('unknown', 10),
    ('material_id', 'unknown-material'),
])
def test_reference_rejects_invalid_or_ignored_inputs(field, value):
    data = reference_input()
    data['cases'] = [dict(data['cases'][0], **{field: value})]
    with pytest.raises(ValueError):
        study.compute_reference_study(data)


@pytest.mark.parametrize('field', ['edition', 'retrieved_on', 'rights_status',
                                    'catalog_source_id', 'issuer', 'url', 'locator'])
def test_reference_requires_source_provenance(field):
    data = reference_input()
    del data['sources'][0][field]
    with pytest.raises(ValueError):
        study.compute_reference_study(data)


@pytest.mark.parametrize('change', ['units', 'source_link', 'duplicate_source',
                                    'material_basis', 'empty', 'date', 'nu', 'modulus'])
def test_reference_schema_and_relationships(change):
    data = reference_input()
    if change == 'units':
        data['units']['length'] = 'in'
    elif change == 'source_link':
        data['materials']['synthetic-nu0']['source_keys'] = ['missing']
    elif change == 'duplicate_source':
        data['sources'].append(data['sources'][0])
    elif change == 'material_basis':
        del data['materials']['synthetic-nu0']['condition']
    elif change == 'empty':
        data['cases'] = []
    elif change == 'date':
        data['sources'][0]['retrieved_on'] = '2026-02-30'
    elif change == 'nu':
        data['materials']['synthetic-nu0']['poisson_ratio'] = 0.5
    else:
        data['materials']['synthetic-nu0']['youngs_modulus_MPa'] = False
    with pytest.raises(ValueError):
        study.compute_reference_study(data)


def test_reference_lookup_is_exact_and_never_executes(monkeypatch):
    import copy
    result = study.compute_reference_study(reference_input())
    selection = copy.deepcopy(result['rows'][1]['case'])
    def forbidden(*args, **kwargs):
        pytest.fail('Lookup executed a calculation')
    monkeypatch.setattr(study, 'compute_reference_study', forbidden)
    monkeypatch.setattr(study, 'evaluate', forbidden)
    assert study.lookup_reference(result, selection) == result['rows'][1]
    for key, value in [('span_mm', 101), ('material_id', 'missing'),
                       ('support', 'clamped'), ('plate_loss_mm', 0.3),
                       ('extra', 1), ('sigma_x_MPa', True)]:
        altered = dict(selection, **{key: value})
        row = study.lookup_reference(result, altered)
        assert row['reference_status'] == 'UNCOMPUTED'
        assert row['minimum_remaining_thickness_mm'] is None
    del selection['support']
    assert study.lookup_reference(result, selection)['reference_status'] == 'UNCOMPUTED'


def test_reference_input_is_not_mutated():
    import copy
    data = reference_input()
    before = copy.deepcopy(data)
    study.compute_reference_study(data)
    assert data == before


def test_reference_no_nominal_crossing_is_explicit():
    data = reference_input()
    data['cases'] = [dict(data['cases'][0], sigma_x_MPa=200)]
    row = study.compute_reference_study(data)['rows'][0]
    assert row['reference_status'] == 'NO_ELASTIC_CROSSING_IN_RANGE'
    assert row['elastic_reference_crossing_mm'] is None
    assert row['minimum_remaining_thickness_mm'] is None


def test_reference_cli_roundtrip_and_saved_only_view(tmp_path, capsys):
    import json
    source = PATH.parents[2] / 'data/ship_structure_reference/elastic_reference_inputs.json'
    output = tmp_path / 'results'
    assert study.main(['--reference-input', str(source), '--output', str(output)]) == 0
    capsys.readouterr()
    saved = json.loads((output / 'reference.json').read_text(encoding='utf-8'))
    manifest = json.loads((output / 'manifest.json').read_text(encoding='utf-8'))
    import hashlib
    assert manifest['result_sha256'] == hashlib.sha256(
        (output / 'reference.json').read_bytes()).hexdigest()
    assert manifest['input_sha256'] == hashlib.sha256(source.read_bytes()).hexdigest()
    html = (output / 'lookup.html').read_text(encoding='utf-8')
    for field in saved['rows'][0]['case']:
        assert f'data-key="{field}"' in html
    assert 'NOT_EVALUATED' in html
    assert 'fetch(' not in html
    selection = tmp_path / 'selection.json'
    selection.write_text(json.dumps(saved['rows'][0]['case']), encoding='utf-8', newline='\n')
    assert study.main(['--reference-lookup', str(output / 'reference.json'),
                       '--selection', str(selection)]) == 0
    assert json.loads(capsys.readouterr().out)['reference_status'] == 'COMPUTED'
    with pytest.raises(ValueError, match='empty'):
        study.main(['--reference-input', str(source), '--output', str(output)])


def test_reference_cli_rejects_duplicate_keys(tmp_path):
    source = tmp_path / 'bad.json'
    source.write_text('{"schema_version": 1, "schema_version": 1}', encoding='utf-8', newline='\n')
    with pytest.raises(ValueError, match='Duplicate'):
        study.main(['--reference-input', str(source), '--output', str(tmp_path / 'out')])


@pytest.mark.parametrize('material_id', [
    'synthetic-nu0', 'PAYLOAD', 'prefixPAYLOADsuffix', 'CONTROLS', 'SCRIPT',
])
def test_reference_view_numeric_controls_execute_saved_lookup(tmp_path, material_id):
    """Execute the shipped view script with a minimal DOM, without a browser.

    Numeric HTML controls are strings; their JSON values must be restored before
    comparing saved numbers. Template tokens in material IDs must stay literal.
    A missing combination must clear the old result.
    """
    import json
    import shutil
    import subprocess
    from html.parser import HTMLParser

    node = shutil.which('node')
    if node is None:
        pytest.skip('Node is unavailable for the standalone view script check')
    data = reference_input()
    data['materials'][material_id] = data['materials'].pop('synthetic-nu0')
    for item in data['cases']:
        if item['material_id'] == 'synthetic-nu0':
            item['material_id'] = material_id
    saved = study.compute_reference_study(data)
    output = tmp_path / 'lookup.html'
    study.render_reference(saved, output)

    class Controls(HTMLParser):
        def __init__(self):
            super().__init__()
            self.controls = []
            self.current = None

        def handle_starttag(self, tag, attrs):
            attrs = dict(attrs)
            if tag == 'select':
                self.current = {'dataset': {'key': attrs['data-key']}, 'options': []}
                self.controls.append(self.current)
            if tag == 'option':
                self.current['options'].append(attrs['value'])

    parser = Controls()
    page = output.read_text(encoding='utf-8')
    parser.feed(page)
    script = page.split('<script>')[1].split('</script>')[0]
    harness = ''.join(('''
const vm=require('vm');
const controls=''', json.dumps(parser.controls), ''';
for(const c of controls){c.value=c.options[0];c.addEventListener=()=>{};}
const display={textContent:''};
const context={document:{querySelectorAll:()=>controls,getElementById:()=>display}};
vm.createContext(context);
vm.runInContext(''', json.dumps(script), ''',context);
const first=JSON.parse(display.textContent);
if(first.reference_status!=='COMPUTED')throw Error('Initial saved row not displayed');
if(first.minimum_remaining_thickness_mm!==null)throw Error('Engineering minimum exposed');
controls.find(c=>c.dataset.key==='plate_loss_mm').value='1.5';
vm.runInContext('show()',context);
const thin=JSON.parse(display.textContent);
if(thin.elastic_critical_stress_MPa!==9.9)throw Error('Wrong saved thinning case');
controls.find(c=>c.dataset.key==='span_mm').value='150';
vm.runInContext('show()',context);
if(!display.textContent.startsWith('UNCOMPUTED'))throw Error('Stale row retained');
'''))
    subprocess.run([node, '-e', harness], check=True, capture_output=True, text=True)


def test_reference_script_does_not_embed_input_markup(tmp_path):
    data = reference_input()
    data['cases'][0]['profile'] = '</script><b>unsafe</b>'
    saved = study.compute_reference_study(data)
    output = tmp_path / 'lookup.html'
    study.render_reference(saved, output)
    page = output.read_text(encoding='utf-8')
    assert '</script><b>unsafe</b>' not in page
    assert page.count('</script>') == 1


@pytest.mark.parametrize('updates', [dict(span_mm=1e308, breadth_mm=1e-308),
                                     dict(breadth_mm=1e-308),
                                     dict(initial_thickness_mm=1e-300)])
def test_reference_unrepresentable_results_are_inapplicable(updates):
    data = reference_input()
    data['cases'] = [dict(data['cases'][0], **updates)]
    row = study.compute_reference_study(data)['rows'][0]
    assert row['reference_status'] == 'INAPPLICABLE'
    assert row['minimum_remaining_thickness_mm'] is None


def test_reference_saved_bundle_digest_is_checked(tmp_path):
    source = PATH.parents[2] / 'data/ship_structure_reference/elastic_reference_inputs.json'
    output = tmp_path / 'results'
    study.main(['--reference-input', str(source), '--output', str(output)])
    result = output / 'reference.json'
    result.write_text(result.read_text(encoding='utf-8') + '\n', encoding='utf-8', newline='\n')
    with pytest.raises(ValueError, match='digest mismatch'):
        study.main(['--reference-lookup', str(result), '--selection', str(source)])
