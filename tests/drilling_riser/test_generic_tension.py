"""Value checks for the ruling; no native interface is imported."""
from pathlib import Path
import math
import pytest
from digitalmodel.drilling_riser import generic_campaign as campaign
from digitalmodel.drilling_riser import generic_tension as tension

ROOT = Path(__file__).resolve().parents[2]


def test_recorded_arrangement_changes_every_check():
    spec = campaign.read_spec(ROOT)
    assert spec['tensioner_arrangement']['n_units'] == 4
    basis = tension.tension_basis(spec)
    spec['tensioner_arrangement'].update(n_units=6, n_fail=1, fwt=1.10)
    changed = tension.tension_basis(spec)
    assert changed['required_vertical_kn'] == pytest.approx(6/5*(changed['tsrmin_kn']+changed['carried_weight_kn']))
    assert changed['tsrmin_kn'] - basis['tsrmin_kn'] == pytest.approx(0.05*basis['ws_kn'])
    values = (12000, 50, 8700, 5000, 300, 1)
    checks = tension.checks(*values, basis=changed)
    assert checks['check_b_margin_kn'] == pytest.approx(1250)
    assert checks['bottom_margin_kn'] == pytest.approx(-200)
    spec['tensioner_arrangement']['n_fail'] = 2
    assert tension.checks(*values, basis=tension.tension_basis(spec))['check_b_margin_kn'] == pytest.approx(-750)
    from digitalmodel.drilling_riser.generic_tension_channels import summarize_samples
    rows = [dict(vertical_kn=12000, carried_kn=50, bottom_kn=300,
                 member_min_kn=1, table_clearance_m=0.2)]
    assert summarize_samples(rows, basis=changed)['support_loss_min_kn'] == pytest.approx(9950)


@pytest.mark.parametrize('function,args', [
    (tension.surviving_share, ()),
    (tension.weight_reserve, ()),
    (tension.checks, (12000, 50, 8700, 5000, 300, 1)),
    ('summarize_samples', ([],)),
    ('extract', ({}, None, 'B')),
])
def test_basis_is_required_by_keyword(function, args):
    from digitalmodel.drilling_riser import generic_tension_channels as channels
    if isinstance(function, str):
        function = getattr(channels, function)
    with pytest.raises(TypeError, match='basis'):
        function(*args)
    with pytest.raises(TypeError):
        function(*args, {'n_units': 4, 'n_fail': 1, 'fwt': 1.05})


def test_compensated_member_sums_are_order_independent():
    spec = campaign.read_spec(ROOT)
    from unittest.mock import patch
    parts = [dict(ws_kn=v, internal_area_m2=0.1, head_kn=v, ea_kn=v, length_m=1)
             for v in (1e16, 1.0, -1e16)]
    with patch.object(tension, 'member_basis', side_effect=parts), \
            patch.object(tension, 'link_geometry', return_value=(10000, [5]*4, 0.85)):
        basis = tension.tension_basis(spec)
    assert basis['ws_kn'] == 1.0
    assert basis['effective_weight_kn'] == 2.0
    assert basis['stiffness_kn_m'] == 1.0


def test_generated_includes_preserve_non_tensioner_bytes():
    import hashlib
    import json
    import re
    folder = campaign.DEST / 'models'
    # Original prepared bytes at bd4f67ca, with spring-tension tables removed.
    # Retain hashes so shallow/offline test checkouts do not need Git history.
    expected = json.loads((Path(__file__).parent / 'fixtures/generic_non_tensioner_sha256.json').read_text())
    def without_tensions(text):
        return re.sub(r'(SpringLength, SpringTension:\n)(?:  - - .*\n    - .*\n)+', r'\1', text)
    for path in sorted((ROOT / folder).glob('*/includes/20_generic_objects.yml')):
        assert hashlib.sha256(without_tensions(path.read_text()).encode()).hexdigest() == expected[path.parts[-3]]


def test_all_masters_link_lengths_within_scaled_tables():
    import yaml
    spec = campaign.read_spec(ROOT)
    table = campaign.case_table(ROOT)
    fractions = []
    for case in table['cases']:
        folder = ROOT / campaign.DEST / 'models' / case['id']
        master = yaml.safe_load((folder / 'master.yml').read_text())
        assert len(master) == 3
        assert all((folder / row['includefile']).is_file() for row in master)
        objects = yaml.safe_load((folder / 'includes/20_generic_objects.yml').read_text())
        links = {link['Name']: link for link in objects['Links']}
        estimate = tension.hand_estimate(spec, table['tension_basis'],
                                        case['offset_m']['value'], case['current_mps']['value'])
        for name, length in zip(tension.LINKS, estimate['link_lengths_m']):
            curve = links[name]['SpringLength, SpringTension']
            low, high = curve[0][0], curve[-1][0]
            assert low <= length <= high
            fractions.append((length-low)/(high-low))
    assert len(fractions) == 96
    assert max(fractions) < 0.54


def test_followup_record_native_comparators_and_correction():
    import json
    record = json.loads((ROOT / campaign.DEST / 'tension-followup.json').read_text())
    assert record['spring_table_factor'] == 1.215
    assert len(record['links']) == 96
    assert record['largest_table_fraction'] == max(row['table_fraction'] for row in record['links'])
    for row in record['links']:
        assert 0 <= row['table_fraction'] <= 1
    assert record['cases'][0]['recorded_native_old_vertical_kn'] == 9801.7
    assert record['cases'][1]['recorded_native_old_vertical_kn'] == 11684.9
    reference = record['cases'][0]
    basis = tension.tension_basis(campaign.read_spec(ROOT))
    share = tension.surviving_share(basis=basis)
    assert record['vertical_correction_kn'] == -reference['hand_minus_native_kn']
    assert record['check_b_after_correction_kn'] == pytest.approx(
        record['check_b_before_correction_kn'] - share*reference['hand_minus_native_kn'], abs=1e-9)
    assert record['check_b_after_ratio_calibration_kn'] == pytest.approx(
        record['check_b_before_correction_kn'] + share*reference['raised_hand']['vertical_kn']
        * (reference['recorded_native_old_vertical_kn']/reference['old_hand']['vertical_kn']-1), abs=1e-9)
    assert record['check_b_after_correction_kn'] > 32
    assert 32 < record['check_b_after_ratio_calibration_kn'] < 34
    html = (ROOT / campaign.DEST / 'tension-followup.html').read_text()
    assert '11,684.9' in html and 'Native statics decide' in html
    assert 'not representative' in html
    import importlib.util
    spec = importlib.util.spec_from_file_location('followup_builder',
        ROOT / 'scripts/reporting/build_generic_riser_tension_followup.py')
    builder = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(builder)
    assert builder.followup_record(ROOT, ROOT / campaign.DEST) == record


def test_model_derived_basis_and_common_factor():
    basis = tension.tension_basis(campaign.read_spec(ROOT))
    assert basis['internal_area_m2'] == pytest.approx(0.3032649917265301)
    assert basis['ws_kn'] == pytest.approx(4889.232532101141, abs=0.01)
    assert basis['tsrmin_kn'] == pytest.approx(8760.255203118819, abs=1e-6)
    assert basis['spring_table_factor'] == 1.215
    assert basis['mechanical_hydraulic_efficiency'] == 1
    assert basis['system_factor'] == pytest.approx(4 / 3)
    assert basis['fleet_adjusted_system_factor'] == pytest.approx(4 / (3 * basis['fleet_cosine']))
    assert basis['required_vertical_kn'] == pytest.approx(4/3*(basis['tsrmin_kn']+basis['carried_weight_kn']))


def test_binding_checks_values_and_boundary():
    basis = campaign.read_spec(ROOT)['tensioner_arrangement']
    result = tension.checks(12000, 50, 8700, 5000, 300, 1, basis=basis)
    assert result['check_a_margin_kn'] == 3250
    assert result['check_b_margin_kn'] == 250
    assert result['bottom_margin_kn'] == pytest.approx(50)
    assert result['check_a_pass'] and result['check_b_pass']
    assert not tension.checks(10000, 50, 8700, 5000, 300, 1, basis=basis)['check_b_pass']
    assert not tension.checks(12000, 50, 8700, 5000, 300, -0.001, basis=basis)['check_a_pass']
    assert not tension.checks(12000, 50, 8700, 5000, 249, 1, basis=basis)['check_a_pass']
    with pytest.raises(ValueError):
        tension.checks(math.nan, 50, 8700, 5000, 300, 1, basis=basis)


def test_hand_equilibrium_factor_and_table_bounds():
    spec = campaign.read_spec(ROOT)
    basis = tension.tension_basis(spec)
    old = tension.hand_estimate(spec, basis, 0, 0, factor=1)
    raised = tension.hand_estimate(spec, basis, 0, 0)
    assert old['check_b_margin_kn'] < -1000
    assert raised['check_b_margin_kn'] >= -1e-6
    assert raised['check_b_margin_kn'] > 49
    assert raised['check_b_margin_kn'] - 0.75 * 22.2 > 32
    assert all(0 < value < 16 for value in raised['link_lengths_m'])
    edge = tension.hand_estimate(spec, basis, 51, 1.5)
    assert edge['check_b_margin_kn'] > 0
    assert all(0 < value < 16 for value in edge['link_lengths_m'])


def test_native_sample_aggregation_and_fail_closed():
    from digitalmodel.drilling_riser.generic_tension_channels import summarize_samples
    basis = campaign.read_spec(ROOT)['tensioner_arrangement']
    rows = [dict(vertical_kn=12000, carried_kn=50, bottom_kn=300,
                 member_min_kn=1, table_clearance_m=0.2),
            dict(vertical_kn=11000, carried_kn=100, bottom_kn=280,
                 member_min_kn=-1, table_clearance_m=-0.1)]
    result = summarize_samples(rows, basis=basis)
    assert result['support_intact_min_kn'] == 10900
    assert result['support_loss_min_kn'] == 8150
    assert result['bottom_sum_min_kn'] == 280
    assert result['member_min_kn'] == -1
    assert result['table_clearance_min_m'] == -0.1
    with pytest.raises(ValueError):
        summarize_samples([], basis=basis)
    with pytest.raises(ValueError):
        summarize_samples([{}], basis=basis)


def test_native_adapter_projection_sign_profiles_and_table_excursion():
    from types import SimpleNamespace
    from digitalmodel.drilling_riser.generic_tension_channels import extract
    from digitalmodel.drilling_riser.generic_tension import LINKS, MEMBERS
    basis = campaign.read_spec(ROOT)['tensioner_arrangement']
    class Object:
        Mass = 2
        SpringLength = [0, 16]
        def TimeHistory(self, variable, period, *args):
            return {'Tension': [100, 200], 'Length': [5, 17], 'End A Z': [4, 15],
                    'End B Z': [1, 0], 'End GZ force': [10, 20],
                    'Effective tension': [200, 300]}[variable]
        def StaticResult(self, variable, *args):
            return self.TimeHistory(variable, None, *args)[0]
        def RangeGraph(self, variable, *args):
            return SimpleNamespace(Min=[-2, 10]) if args else SimpleNamespace(Mean=[1, 10])
    model = {name: Object() for name in (*LINKS, *MEMBERS, 'Tensioner Ring', 'Riser Slip Joint')}
    result = extract(model, 'main stage', 'B', basis=basis)
    assert result['support_intact_min_kn'] == pytest.approx(240 - 2*9.81 - 10)
    assert result['support_loss_min_kn'] == pytest.approx(180 - 2*9.81 - 10)
    assert result['bottom_sum_min_kn'] == 600
    assert result['member_min_kn'] == -2
    assert result['table_clearance_min_m'] == -1
    static = extract(model, None, 'B', static=True, basis=basis)
    assert static['support_intact_min_kn'] == pytest.approx(240-2*9.81-10)
    assert static['member_min_kn'] == 1
    model[LINKS[0]].TimeHistory = lambda *args: [1]
    with pytest.raises(ValueError, match='Mismatched'):
        extract(model, 'main stage', 'B', basis=basis)


def test_all_24_hand_cases_and_four_common_tables():
    import yaml
    from digitalmodel.drilling_riser.generic_tension import LINKS
    spec = campaign.read_spec(ROOT)
    table = campaign.case_table(ROOT)
    expected = {l['name']: l['properties']['SpringLength, SpringTension']
                for l in spec['generic']['links']}
    for case in table['cases']:
        hand = case['tension_hand_estimate']['value']
        assert hand['check_a_margin_kn'] > 2900
        assert hand['check_b_margin_kn'] > 4
        assert hand['check_a_pass'] and hand['check_b_pass']
        assert all(0 < value < 16 for value in hand['link_lengths_m'])
        path = ROOT / campaign.DEST / 'models' / case['id'] / 'includes/20_generic_objects.yml'
        objects = yaml.safe_load(path.read_text())
        links = {link['Name']: link for link in objects['Links']}
        for name in LINKS:
            for original, raised in zip(expected[name], links[name]['SpringLength, SpringTension']):
                assert raised[0] == original[0]
                assert raised[1] == pytest.approx(original[1]*1.215)


def test_driver_binding_rows_and_missing_evidence():
    import importlib.util
    import json
    path = ROOT / 'scripts/orcaflex/run_generic_drilling_riser_campaign.py'
    spec = importlib.util.spec_from_file_location('ruling_driver', path)
    driver = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(driver)
    result = json.loads((ROOT/'tests/drilling_riser/fixtures/generic_synthetic_result.json').read_text())
    criteria, _ = campaign.load_criteria(ROOT/'tests/drilling_riser/fixtures/generic_synthetic_criteria.json', campaign.case_table(ROOT))
    all_rows = driver.criterion_rows(result, criteria)
    assert len(all_rows) == 12
    rows = {row[0]: row for row in all_rows}
    assert rows['check A intact'][3] == pytest.approx(8760.255203118819)
    assert rows['check A intact'][-1] == pytest.approx(8760.255203118819/12000)
    assert rows['check B one link lost'][-1] == pytest.approx(8760.255203118819/9000)
    assert rows['check A bottom'][3] == pytest.approx(244.46162660505706)
    assert rows['check A member nonnegative'][-1] <= 1
    criteria['fwt']['value'] = 1.10
    changed = {row[0]: row for row in driver.criterion_rows(result, criteria)}
    assert changed['check A bottom'][3] == pytest.approx(0.10*criteria['ws_kn']['value'])
    result['support_loss_min_kn'] = 8000
    assert next(row[-1] for row in driver.criterion_rows(result, criteria) if row[0]=='check B one link lost') > 1
    result.pop('support_loss_min_kn')
    with pytest.raises(ValueError, match='support_loss_min_kn'):
        driver.validate_result(result)
