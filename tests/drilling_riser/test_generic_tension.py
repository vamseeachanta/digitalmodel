"""Value checks for the ruling; no native interface is imported."""
from pathlib import Path
import math
import pytest
from digitalmodel.drilling_riser import generic_campaign as campaign
from digitalmodel.drilling_riser import generic_tension as tension

ROOT = Path(__file__).resolve().parents[2]


def test_model_derived_basis_and_common_factor():
    basis = tension.tension_basis(campaign.read_spec(ROOT))
    assert basis['internal_area_m2'] == pytest.approx(0.3032649917265301)
    assert basis['ws_kn'] == pytest.approx(4889.232532101141, abs=0.01)
    assert basis['tsrmin_kn'] == pytest.approx(8760.255203118819, abs=1e-6)
    assert basis['spring_table_factor'] == 1.208
    assert basis['mechanical_hydraulic_efficiency'] == 1
    assert basis['system_factor'] == pytest.approx(4 / (3 * basis['fleet_cosine']))
    assert basis['required_vertical_kn'] == pytest.approx(4/3*(basis['tsrmin_kn']+basis['carried_weight_kn']))


def test_binding_checks_values_and_boundary():
    result = tension.checks(12000, 50, 8700, 5000, 300, 1)
    assert result['check_a_margin_kn'] == 3250
    assert result['check_b_margin_kn'] == 250
    assert result['bottom_margin_kn'] == pytest.approx(50)
    assert result['check_a_pass'] and result['check_b_pass']
    assert not tension.checks(10000, 50, 8700, 5000, 300, 1)['check_b_pass']
    assert not tension.checks(12000, 50, 8700, 5000, 300, -0.001)['check_a_pass']
    assert not tension.checks(12000, 50, 8700, 5000, 249, 1)['check_a_pass']
    with pytest.raises(ValueError):
        tension.checks(math.nan, 50, 8700, 5000, 300, 1)


def test_hand_equilibrium_factor_and_table_bounds():
    spec = campaign.read_spec(ROOT)
    basis = tension.tension_basis(spec)
    old = tension.hand_estimate(spec, basis, 0, 0, factor=1)
    raised = tension.hand_estimate(spec, basis, 0, 0)
    assert old['check_b_margin_kn'] < -1000
    assert raised['check_b_margin_kn'] >= -1e-6
    assert raised['vertical_kn'] == pytest.approx(11752.427217477049, abs=0.01)
    assert all(0 < value < 16 for value in raised['link_lengths_m'])
    edge = tension.hand_estimate(spec, basis, 51, 1.5)
    assert edge['check_b_margin_kn'] > 0
    assert all(0 < value < 16 for value in edge['link_lengths_m'])


def test_native_sample_aggregation_and_fail_closed():
    from digitalmodel.drilling_riser.generic_tension_channels import summarize_samples
    rows = [dict(vertical_kn=12000, carried_kn=50, bottom_kn=300,
                 member_min_kn=1, table_clearance_m=0.2),
            dict(vertical_kn=11000, carried_kn=100, bottom_kn=280,
                 member_min_kn=-1, table_clearance_m=-0.1)]
    result = summarize_samples(rows)
    assert result['support_intact_min_kn'] == 10900
    assert result['support_loss_min_kn'] == 8150
    assert result['bottom_sum_min_kn'] == 280
    assert result['member_min_kn'] == -1
    assert result['table_clearance_min_m'] == -0.1
    with pytest.raises(ValueError):
        summarize_samples([])
    with pytest.raises(ValueError):
        summarize_samples([{}])


def test_native_adapter_projection_sign_profiles_and_table_excursion():
    from types import SimpleNamespace
    from digitalmodel.drilling_riser.generic_tension_channels import extract
    from digitalmodel.drilling_riser.generic_tension import LINKS, MEMBERS
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
    result = extract(model, 'main stage', 'B')
    assert result['support_intact_min_kn'] == pytest.approx(240 - 2*9.81 - 10)
    assert result['support_loss_min_kn'] == pytest.approx(180 - 2*9.81 - 10)
    assert result['bottom_sum_min_kn'] == 600
    assert result['member_min_kn'] == -2
    assert result['table_clearance_min_m'] == -1
    static = extract(model, None, 'B', static=True)
    assert static['support_intact_min_kn'] == pytest.approx(240-2*9.81-10)
    assert static['member_min_kn'] == 1
    model[LINKS[0]].TimeHistory = lambda *args: [1]
    with pytest.raises(ValueError, match='Mismatched'):
        extract(model, 'main stage', 'B')


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
                assert raised[1] == pytest.approx(original[1]*1.208)


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
    result['support_loss_min_kn'] = 8000
    assert next(row[-1] for row in driver.criterion_rows(result, criteria) if row[0]=='check B one link lost') > 1
    result.pop('support_loss_min_kn')
    with pytest.raises(ValueError, match='support_loss_min_kn'):
        driver.validate_result(result)
