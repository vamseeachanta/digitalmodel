# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent statics and hydrostatics, with hand calculations at each assertion."""

from dataclasses import replace
import importlib.util
import json
import math
from pathlib import Path

import pytest

from digitalmodel.geotechnical.spudcan import (
    LayeredProfile, SoilLayer, Spudcan, penetration_curve,
)
from digitalmodel.marine_ops.preload_sequence import (
    HullGeometry, MethodStage, compare_preload_methods, hull_response,
    preload_method_stages,
)


# Triangle centroid and waterplane centroid coincide. Unit density makes
# buoyancy equal to displaced volume. This small geometry admits exact checks.
HULL = HullGeometry(6, 6, 5, ((0, 2), (-2, -1), (2, -1)), 1)


def curve():
    # Circular area pi; strong-to-weak clay: surface resistance
    # (6.34*10 + 3*80*3/2)*pi = 423.4*pi. At z=3, the weak
    # layer gives (6.34*10 + 1*3)*pi = 66.4*pi. No implementation
    # result is used to set an expected value.
    return penetration_curve(LayeredProfile((
        SoilLayer('clay', 0, 3, 1, su_kpa=80),
        SoilLayer('clay', 3, 20, 1, su_kpa=10),
    )), Spudcan(2), [0, 3, 20])


def test_fully_immersed_tide_changes_each_reaction_by_displaced_volume():
    stage = MethodStage('test', 1, (100, 100, 100), 1, 0)
    result = hull_response(HULL, stage, tide_m=0.5)
    # Volume 6*6*1.5=54; centred buoyancy takes 54/3 off each leg.
    assert result.buoyancy_kn == pytest.approx(54)
    assert result.reactions_kn == pytest.approx((82, 82, 82))
    assert result.centre_draft_m == pytest.approx(1.5)
    assert result.trim_deg == pytest.approx(0)
    assert result.heel_deg == pytest.approx(0)
    assert result.contact_valid


def test_prescribed_fore_leg_settlement_changes_trim_and_moment():
    stage = MethodStage('test', 1, (100, 100, 100), 1, 0)
    result = hull_response(HULL, stage, failed_leg=0, settlement_m=0.3)
    # Plane through leg displacements: w=0.1+0.1*y.
    # Volume=36*1.1=39.6; first moment about x=0.1*6*6^3/12=10.8.
    # Buoyancy allocated to fore leg: (39.6+10.8)/3=16.8.
    # Aft shares=(39.6-16.8)/2=11.4. Sum R+B=300.
    assert result.reactions_kn == pytest.approx((83.2, 88.6, 88.6))
    assert result.reaction_changes_kn == pytest.approx((-4.8, 0.6, 0.6))
    assert result.buoyancy_kn == pytest.approx(39.6)
    assert result.buoyancy_first_moment_y_kn_m == pytest.approx(10.8)
    assert result.trim_deg == pytest.approx(math.degrees(math.atan(0.1)))
    assert result.heel_deg == pytest.approx(0)
    assert sum(result.reactions_kn) + result.buoyancy_kn == pytest.approx(300)
    assert 2 * result.reactions_kn[0] - sum(result.reactions_kn[1:]) + 10.8 == pytest.approx(0)


def test_partial_reimmersion_has_exact_wedge_buoyancy():
    stage = MethodStage('test', 1, (100, 100, 100), -0.1, 0)
    result = hull_response(HULL, stage, failed_leg=0, settlement_m=0.3)
    # d=0.1*y, wet on 0<y<3. Volume=6*integral(0.1*y)=2.7.
    # First moment=6*integral(0.1*y^2)=5.4. Fore share=(2.7+5.4)/3=2.7.
    assert result.buoyancy_kn == pytest.approx(2.7)
    assert result.buoyancy_first_moment_y_kn_m == pytest.approx(5.4)
    assert result.reactions_kn == pytest.approx((97.3, 100, 100))
    assert result.wetted_area_m2 == pytest.approx(18)


def test_elevated_hull_redistributes_no_load_before_water_contact():
    stage = MethodStage('test', 1, (100, 100, 100), -2, 0)
    result = hull_response(HULL, stage, failed_leg=0, settlement_m=0.3)
    # Three prescribed support levels uniquely set hull plane. With no water
    # contact and no horizontal CG motion, force/moment balance is unchanged.
    assert result.buoyancy_kn == pytest.approx(0)
    assert result.reactions_kn == pytest.approx((100, 100, 100))
    assert result.reaction_changes_kn == pytest.approx((0, 0, 0))


@pytest.mark.parametrize('leg', [0, 1, 2])
def test_each_failed_leg_preserves_force_and_both_moments(leg):
    stage = MethodStage('test', 1, (100, 90, 80), 1, leg)
    result = hull_response(HULL, stage, tide_m=-0.25, failed_leg=leg, settlement_m=0.3)
    assert sum(result.reactions_kn) + result.buoyancy_kn == pytest.approx(270)
    for axis, moment in [(0, result.buoyancy_first_moment_x_kn_m),
                         (1, result.buoyancy_first_moment_y_kn_m)]:
        before = sum(load * xy[axis] for load, xy in zip(stage.dry_leg_loads_kn, HULL.leg_xy_m))
        after = sum(load * xy[axis] for load, xy in zip(result.reactions_kn, HULL.leg_xy_m))
        assert after + moment == pytest.approx(before)


def test_ballast_limit_and_six_stage_schedule():
    stages = preload_method_stages(target_kn=300, reserve_kn=120,
                                   maximum_weight_kn=720, wet_draft_m=1,
                                   elevated_air_gap_m=1, early_fraction=0.6)
    one = [s for s in stages if s.method == 'one_leg_at_a_time']
    all_legs = [s for s in stages if s.method == 'all_legs']
    staged = [s for s in stages if s.method == 'staged_in_water_then_elevated']
    assert len(one) == 3
    assert len(all_legs) == 1
    # Public 2021 case paper, Example Rig Move 1, p. 5, describes six
    # stages with the first four in water. Each assumed stage here sweeps
    # all three legs sequentially; actual within-stage leg order is unpublished.
    assert len(staged) == 18
    assert {s.number for s in staged} == set(range(1, 7))
    assert [s.reference_draft_m for s in staged] == [1] * 12 + [-1] * 6
    # Available extra=720-3*120=360. Simultaneous extra demand=3*180;
    # scale 360/540=2/3, so every leg gets 120+120=240.
    assert all_legs[0].dry_leg_loads_kn == pytest.approx((240, 240, 240))
    for index in range(3):
        expected = [120, 120, 120]
        expected[index] = 300
        assert one[index].dry_leg_loads_kn == pytest.approx(expected)
        assert staged[index + 12].dry_leg_loads_kn == pytest.approx(expected)
        expected[index] = 180  # 0.6*300
        assert staged[index].dry_leg_loads_kn == pytest.approx(expected)
        assert staged[index].reference_draft_m == 1
        assert staged[index + 12].reference_draft_m == -1


def test_capacity_margins_use_each_curve_peak_and_trough_not_global_recovery():
    stages = (MethodStage('test', 1, (300, 300, 300), -2, 0),)
    rows = compare_preload_methods((curve(),) * 3, HULL, stages,
                                   tide_levels_m=(0,), target_kn=300, settlement_m=0.3)
    assert len(rows) == 6  # three legs, before and after prescribed loss
    for row in rows:
        # First peak at surface=423.4*pi; trough at interface=66.4*pi.
        assert row.peak_headroom_kn == pytest.approx(423.4 * math.pi - 300)
        assert row.post_peak_reserve_kn == pytest.approx(66.4 * math.pi - 300)
        assert row.capacity_drop_kn == pytest.approx(357 * math.pi)
        assert row.target_margin_kn == pytest.approx(0)
        assert row.post_peak_reserve_kn < 0


def test_monotonic_curve_has_no_invented_post_peak_capacity():
    monotonic = penetration_curve(LayeredProfile((
        SoilLayer('clay', 0, 20, 1, su_kpa=10),
    )), Spudcan(2), [0, 20])
    rows = compare_preload_methods((monotonic,) * 3, HULL,
                                   (MethodStage('test', 1, (300,) * 3, -2, 0),),
                                   tide_levels_m=(0,), target_kn=300, settlement_m=0.3)
    for row in rows:
        assert row.post_peak_reserve_kn is None
        assert row.capacity_drop_kn == pytest.approx(0)
        # Maximum of monotonic interval=(6.34*10+20)*pi.
        assert row.peak_headroom_kn == pytest.approx(83.4 * math.pi - 300)
        assert not row.within_evaluated_capacity


def test_uplift_is_exposed_without_silent_rebalancing():
    result = hull_response(HULL, MethodStage('test', 1, (1,) * 3, 1, 0))
    assert result.reactions_kn == pytest.approx((-11,) * 3)
    assert not result.contact_valid


@pytest.mark.parametrize('change', [
    {'length_m': 0}, {'breadth_m': float('nan')}, {'depth_m': -1},
    {'water_weight_kn_m3': True}, {'leg_xy_m': ((0, 0), (1, 1), (2, 2))},
    {'leg_xy_m': ((0, 0), (1, 1))},
])
def test_invalid_geometry_rejected(change):
    with pytest.raises(ValueError):
        hull_response(replace(HULL, **change), MethodStage('test', 1, (100,) * 3, 1, 0))


@pytest.mark.parametrize('kwargs', [
    {'tide_m': float('inf')}, {'settlement_m': -1}, {'failed_leg': 3},
    {'failed_leg': True}, {'failed_leg': 1.5}, {'settlement_m': 1},
])
def test_invalid_response_controls_rejected(kwargs):
    with pytest.raises(ValueError):
        hull_response(HULL, MethodStage('test', 1, (100,) * 3, 1, 0), **kwargs)


def test_deck_immersion_rejected():
    with pytest.raises(ValueError, match='deck'):
        hull_response(HULL, MethodStage('test', 1, (100,) * 3, 4.9, 0), tide_m=0.2)


@pytest.mark.parametrize('kwargs', [
    {'target_kn': 0}, {'reserve_kn': 301}, {'maximum_weight_kn': 359},
    {'early_fraction': 1.1}, {'early_fraction': 0.1}, {'wet_draft_m': 0},
    {'elevated_air_gap_m': float('nan')},
])
def test_invalid_schedule_rejected(kwargs):
    args = dict(target_kn=300, reserve_kn=120, maximum_weight_kn=720,
                wet_draft_m=1, elevated_air_gap_m=1, early_fraction=0.6)
    args.update(kwargs)
    with pytest.raises(ValueError):
        preload_method_stages(**args)


def test_offline_example_outputs_all_profiles_and_methods(tmp_path):
    source = Path('examples/domains/geotechnical/jackup_preload_methods.py')
    spec = importlib.util.spec_from_file_location('preload_example', source)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    module.write_examples(tmp_path)
    import csv
    with (tmp_path / 'comparison.csv').open() as stream:
        rows = list(csv.DictReader(stream))
    assert {row['profile'] for row in rows} == {'stiff_over_soft', 'sand_over_soft', 'uniform_soft'}
    assert {row['method'] for row in rows} == {
        'one_leg_at_a_time', 'all_legs', 'staged_in_water_then_elevated'}
    # 3 profiles * 3 tides * 3 legs * (21 sequential states*2 + 1 all-leg state*4).
    # The all-leg state has one nominal condition plus a separate loss of each leg.
    assert len(rows) == 1242
    for name in ('stiff_over_soft', 'sand_over_soft', 'uniform_soft'):
        figure = json.loads((tmp_path / f'{name}.figure.json').read_text())
        assert figure['data']
        assert figure['layout']['meta']['basis'] == 'assumed, based on previous experience'
    summary = json.loads((tmp_path / 'summary.json').read_text())
    for case in summary['profiles'].values():
        for method in case.values():
            # Dry elevated hull at reference tide: sequential reaches 30000;
            # simultaneous cap gives (72000/3)=24000 kN per leg.
            expected = 24000 if method['method'] == 'all_legs' else 30000
            assert method['maximum_nominal_reaction_per_leg_kn'] == pytest.approx([expected] * 3)
    assert (tmp_path / 'assumptions.json').is_file()


def test_asymmetric_loss_changes_both_trim_and_heel():
    result = hull_response(HULL, MethodStage('test', 1, (100,) * 3, 1, 1),
                           failed_leg=1, settlement_m=0.3)
    # Port displacement yields w=0.1-0.075*x-0.05*y. With Ixx=Iyy=108,
    # buoyancy=39.6, first moments=-8.1,-5.4. Barycentric shares follow
    # Nfore=(1+y)/3 and Nport=1/3-x/4-y/6, giving 11.4,16.125,12.075.
    assert result.reactions_kn == pytest.approx((88.6, 83.875, 87.925))
    assert result.trim_deg == pytest.approx(math.degrees(math.atan(-0.05)))
    assert result.heel_deg == pytest.approx(math.degrees(math.atan(-0.075)))


def test_nonsquare_waterplane_uses_longitudinal_length():
    hull = replace(HULL, length_m=8, breadth_m=6)
    result = hull_response(hull, MethodStage('test', 1, (100,) * 3, 1, 0),
                           failed_leg=0, settlement_m=0.3)
    # d=1.1+0.1*y. Volume=6*8*1.1=52.8; My=0.1*6*8^3/12=25.6.
    assert result.buoyancy_kn == pytest.approx(52.8)
    assert result.buoyancy_first_moment_y_kn_m == pytest.approx(25.6)


def test_tide_sensitivity_uses_minimum_and_maximum_not_only_reference():
    stages = (MethodStage('test', 1, (100,) * 3, 1, 0),)
    rows = compare_preload_methods((curve(),) * 3, HULL, stages,
                                   tide_levels_m=(-0.5, 0, 0.5), target_kn=100, settlement_m=0.3)
    for tide in (-0.5, 0, 0.5):
        nominal = [r for r in rows if r.tide_m == tide and r.condition == 'nominal']
        # Immersed centred waterplane: reaction=100-(36/3)*(1+tide).
        assert [r.reaction_kn for r in nominal] == pytest.approx([88 - 12*tide] * 3)
        assert [r.target_margin_kn for r in nominal] == pytest.approx([-12 - 12*tide] * 3)


def test_example_accepts_a_tide_range_omitting_zero(tmp_path):
    source = Path('examples/domains/geotechnical/jackup_preload_methods.py')
    spec = importlib.util.spec_from_file_location('preload_example', source)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    module.write_examples(tmp_path, tide_levels_m=(-0.5, 0.5))
    data = json.loads((tmp_path / 'assumptions.json').read_text())
    assert data['tide_levels_m'] == [-0.5, 0, 0.5]


def test_simultaneous_state_trials_each_leg_without_repeating_nominal():
    stages = (MethodStage('all_legs', 1, (100,) * 3, 1, None),)
    rows = compare_preload_methods((curve(),) * 3, HULL, stages,
                                   tide_levels_m=(0,), target_kn=100, settlement_m=0.3)
    # One nominal state plus three independent losses, each reported for 3 legs.
    assert len(rows) == 12
    assert len([r for r in rows if r.condition == 'nominal']) == 3
    assert {r.trial_leg for r in rows if r.condition != 'nominal'} == {0, 1, 2}
