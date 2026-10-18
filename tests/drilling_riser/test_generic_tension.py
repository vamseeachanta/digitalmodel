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
    parts = [dict(ws_kn=v, internal_area_m2=0.1, head_kn=v, ea_kn=v, length_m=1, mud_density_te_m3=2.2, hm_m=1020, hw_m=1000)
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
    assert basis['tsrmin_kn'] == pytest.approx(8760.265744747303, abs=1e-6)
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
        def __init__(self, name):
            self.name = name
        Mass = 2
        SpringLength = [0, 16]
        def TimeHistory(self, variable, period, *args):
            if variable == 'End GZ force' and self.name in MEMBERS:
                assert args == ('A',)
                # Independent ring free body: upward links balance ring weight,
                # slip reaction and all three member reactions, sample by sample.
                return [(240-2*9.81-10)/3, (4*200*15/17-2*9.81-20)/3]
            return {'Tension': [100, 200], 'Length': [5, 17], 'End A Z': [4, 15],
                    'End B Z': [1, 0], 'End GZ force': [10, 20],
                    'Effective tension': [200, 300]}[variable]
        def StaticResult(self, variable, *args):
            return self.TimeHistory(variable, None, *args)[0]
        def RangeGraph(self, variable, *args):
            return SimpleNamespace(Min=[-2, 10]) if args else SimpleNamespace(Mean=[1, 10])
    model = {name: Object(name) for name in (*LINKS, *MEMBERS, 'Tensioner Ring', 'Riser Slip Joint')}
    result = extract(model, 'main stage', 'B', end_a='A', basis=basis)
    assert result['support_intact_min_kn'] == pytest.approx(240 - 2*9.81 - 10)
    assert result['support_loss_min_kn'] == pytest.approx(180 - 2*9.81 - 10)
    assert result['bottom_sum_min_kn'] == 600
    assert result['member_min_kn'] == -2
    assert result['table_clearance_min_m'] == -1
    static = extract(model, None, 'B', end_a='A', static=True, basis=basis)
    assert static['support_intact_min_kn'] == pytest.approx(240-2*9.81-10)
    assert static['member_min_kn'] == 1
    assert result['ring_equilibrium_max_abs_kn'] < 1e-12
    assert static['ring_equilibrium_max_fraction'] < 1e-12
    # Reversing only the slip End B force previously overstated support.
    slip = model['Riser Slip Joint']
    original = slip.TimeHistory
    slip.TimeHistory = lambda variable, *args: ([-10, -20] if variable == 'End GZ force'
                                                else original(variable, *args))
    for static_mode in (False, True):
        with pytest.raises(ValueError, match='Ring vertical equilibrium failed'):
            extract(model, None, 'B', end_a='A', static=static_mode, basis=basis)
    slip.TimeHistory = lambda variable, *args: ([10.1, 20.1] if variable == 'End GZ force'
                                                  else original(variable, *args))
    residual = extract(model, None, 'B', end_a='A', basis=basis)
    assert residual['ring_equilibrium_max_abs_kn'] == pytest.approx(0.1)
    assert residual['ring_equilibrium_max_fraction'] == pytest.approx(0.1/240)
    slip.TimeHistory = lambda variable, *args: ([11, 21] if variable == 'End GZ force'
                                               else original(variable, *args))
    with pytest.raises(ValueError, match='Ring vertical equilibrium failed'):
        extract(model, None, 'B', end_a='A', basis=basis)
    slip.TimeHistory = original
    model[LINKS[0]].TimeHistory = lambda *args: [1]
    with pytest.raises(ValueError, match='Mismatched'):
        extract(model, 'main stage', 'B', end_a='A', basis=basis)


def test_all_24_hand_cases_and_four_common_tables():
    import yaml
    from digitalmodel.drilling_riser.generic_tension import LINKS
    spec = campaign.read_spec(ROOT)
    table = campaign.case_table(ROOT)
    expected = {link['name']: link['properties']['SpringLength, SpringTension']
                for link in spec['generic']['links']}
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
    assert rows['check A intact'][3] == pytest.approx(8760.265744747303)
    assert rows['check A intact'][-1] == pytest.approx(8760.265744747303/12000)
    assert rows['check B one link lost'][-1] == pytest.approx(8760.265744747303/9000)
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


@pytest.mark.parametrize('density', [2.0, 2.4])
def test_minimum_tension_tracks_specification_mud_density(density):
    spec = campaign.read_spec(ROOT)
    old = tension.tension_basis(spec)
    for line in spec['generic']['lines']:
        if line['name'] in tension.MEMBERS:
            line['properties']['ContentsDensity'] = density
    new = tension.tension_basis(spec)
    # ABS 2017 §3/3 iii Eq (3.2): ΔT_SRmin = Ai Δrho g Hm.
    assert new['tsrmin_kn']-old['tsrmin_kn'] == pytest.approx(
        old['internal_area_m2']*(density-old['mud_density_te_m3'])*tension.G*old['hm_m'])
    assert new['effective_weight_kn']-old['effective_weight_kn'] == pytest.approx(
        new['tsrmin_kn']-old['tsrmin_kn'])


def test_mismatched_mud_densities_fail_closed():
    spec = campaign.read_spec(ROOT)
    next(member for member in spec['generic']['lines'] if member['name']=='Choke')['properties']['ContentsDensity'] = 2.4
    with pytest.raises(ValueError, match='ContentsDensity values must agree'):
        tension.tension_basis(spec)


def test_specification_heads_wet_sections_and_drag_geometry():
    spec = campaign.read_spec(ROOT)
    # Move the ring to z=21, the flex joint to z=-999, preserving length.
    for buoy in spec['generic']['buoys_6d']:
        if buoy['name'] in ('Tensioner Ring', 'Flex Joint'):
            buoy['initial_position'][2] += 1
    part = tension.member_basis(spec, 'Riser')
    assert part['hm_m'] == 1020
    assert part['hw_m'] == 999
    assert part['wet_section_lengths_m'] == [999]
    assert part['cd_d_wet_integral_m2'] == pytest.approx(1.2*0.65*999)
    basis = tension.tension_basis(spec)
    assert basis['ring_elevation_m'] == 21
    assert basis['point_z_m'] == -999
    assert basis['hw_m'] == pytest.approx(999)
    assert basis['datum'] == 'mean sea level z=0 m; positive z upward'
    # With multiple sections, the first 21 m are in air, not the last 21 m.
    line = next(member for member in spec['generic']['lines'] if member['name']=='Riser')
    line['properties']['LineType, Length, TargetSegmentLength'] = [
        ['24" RISER', 20, 5], ['24" RISER', 1000, 5]]
    assert tension.member_basis(spec, 'Riser')['wet_section_lengths_m'] == [0, 999]
    base = tension.hand_estimate(spec, tension.tension_basis(spec), 0, 1)
    kind = next(t for t in spec['generic']['line_types'] if t['name']=='24" RISER')
    kind['properties']['Cdn'] *= 2
    changed = tension.hand_estimate(spec, tension.tension_basis(spec), 0, 1)
    assert changed['ring_z_m'] != pytest.approx(base['ring_z_m'], abs=1e-6)
    assert tension.hand_estimate(spec, tension.tension_basis(spec), 0, 0)['ring_z_m'] > 21


def test_wet_weight_uses_each_section_elevation():
    from copy import deepcopy
    spec = campaign.read_spec(ROOT)
    line = next(member for member in spec['generic']['lines'] if member['name']=='Riser')
    upper = next(t for t in spec['generic']['line_types'] if t['name']=='24" RISER')
    lower = deepcopy(upper)
    lower.update(name='thicker lower section', outer_diameter=0.75)
    spec['generic']['line_types'].append(lower)
    line['properties']['LineType, Length, TargetSegmentLength'] = [
        ['24" RISER', 20, 5], ['thicker lower section', 1000, 5]]
    area_top = math.pi/4*(upper['outer_diameter']**2-upper['inner_diameter']**2)
    area_bottom = math.pi/4*(lower['outer_diameter']**2-lower['inner_diameter']**2)
    # Archimedes: the first 20 m are dry; all 1000 m of the lower section are wet.
    expected = tension.G*(area_top*upper['properties']['MaterialDensity']*20
        + area_bottom*(lower['properties']['MaterialDensity']-spec['environment']['water']['density'])*1000)
    assert tension.member_basis(spec, 'Riser')['ws_kn'] == pytest.approx(expected, abs=1e-8)



def test_rotated_attachment_frame_requires_another_geometry_basis():
    spec = campaign.read_spec(ROOT)
    ring = next(b for b in spec['generic']['buoys_6d'] if b['name']=='Tensioner Ring')
    ring['properties']['InitialAttitude'] = [0, 5, 0]
    with pytest.raises(ValueError, match='unrotated attachment frames'):
        tension.tension_basis(spec)
