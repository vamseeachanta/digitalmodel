# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculations for the generic jack-up example.

All numerical fixtures here are synthetic assumptions. Capacity and response
oracles are derived below from geometry, equilibrium and elementary mechanics;
none is obtained by running the implementation to populate expected values.
"""

import importlib.util
import json
import math
from dataclasses import replace
from pathlib import Path

import pytest

from digitalmodel.geotechnical.spudcan import (
    CapacityDrop, LayeredPenetrationCurve, LayeredProfile, PenetrationPoint, SoilLayer, Spudcan,
)
from digitalmodel.marine_ops.jackup_failure_sequence import (
    JackupInputs, member_capacity, punch_consequence, screen_jackup,
)


def synthetic_curve(recovered=True, loss_kn=4000):
    peak = PenetrationPoint(2, 200, 20000, 'synthetic_peak')
    trough = PenetrationPoint(3, (20000-loss_kn)/100, 20000-loss_kn, 'synthetic_trough')
    event = CapacityDrop(peak, trough, loss_kn, loss_kn/200, 6 if recovered else None, None, None)
    return LayeredPenetrationCurve((peak, trough), peak, peak, loss_kn, (event,),
                                   None, None, None, (2, 10))


def uniform():
    return LayeredProfile((SoilLayer('clay', 0, 24, 6, su_kpa=15),))


def run(**changes):
    args = dict(profile=uniform(), spudcan=Spudcan(12, 1.5),
                tip_depths_m=[1.5, 24], inputs=JackupInputs(), phase='storm',
                hull_state='elevated', air_gap_m=10, load_factors=[0, 1, 2, 4])
    args.update(changes)
    return screen_jackup(**args)


def trace(result, name):
    return next(m for m in result.sequence.modes if m.name == name)


def test_tubular_capacity_yield_and_elastic_buckling_hand_calculation():
    # Tube area pi/4*(D^2-d^2); I=pi/64*(D^4-d^4).
    # Stress uses kPa, force kN, and effective length already includes K.
    area = math.pi/4 * (0.6**2 - 0.55**2)
    inertia = math.pi/64 * (0.6**4 - 0.55**4)
    for length in (8, 80):
        result = member_capacity(0.6, 0.025, length, 210e6, 355e3)
        assert result.area_m2 == pytest.approx(area)
        assert result.second_moment_m4 == pytest.approx(inertia)
        assert result.axial_capacity_kn == pytest.approx(min(area*355e3, math.pi**2*210e6*inertia/length**2))


def test_elevated_tilt_and_bending_after_independent_four_metre_drop():
    p = JackupInputs()
    result = punch_consequence(synthetic_curve(), p, 'elevated', 10)
    # Distance from first leg (20,0) to line x=-10 is 30 m.
    theta = math.atan(4/30)
    a = math.pi/4 * (0.6**2 - 0.55**2)
    i = math.pi/64 * (0.6**4 - 0.55**4)
    leg_i = 3*i + 1.5*a*8**2
    length = p.water_depth_m + 10
    expected_moment = 3*210e6*leg_i*math.tan(theta)/length
    assert result.travel_m == 4
    assert result.tilt_deg == pytest.approx(math.degrees(theta))
    assert result.elastic_leg_moment_knm == pytest.approx(expected_moment)
    # Hull edge x=30 is 40 m from the pivot line: descent=4*40/30.
    assert result.remaining_air_gap_m == pytest.approx(10 - 4*40/30)
    assert not result.travel_is_lower_bound


def test_buoyancy_balance_uses_moment_lever_and_reserve_not_force_alone():
    p = JackupInputs()
    result = punch_consequence(synthetic_curve(), p, 'in_water', 0)
    # Lost reaction 4 MN at lever 30 m requires 12 MN at hull centre lever 10 m.
    # Hull-centre sinkage is failed-leg travel/3: delta=9*loss/(gamma*A).
    gamma = 1.025 * 9.81
    delta = 9*4000/(gamma*2200)
    assert result.buoyancy_replacement_kn == 12000
    assert result.buoyancy_balance_travel_m == pytest.approx(delta)
    assert result.travel_m == pytest.approx(delta)
    assert result.buoyancy_can_balance_loss
    # Freeboard=6 m, edge lever=40 m, centre lever=10 m: centre sinkage=1.5 m.
    # Reserve=1.025*9.81*2200*1.5=33182.325 kN before edge immersion.
    assert result.reserve_buoyancy_kn == pytest.approx(33182.325)
    assert result.initial_buoyancy_kn == pytest.approx(gamma*2200)
    # Full loss of initial buoyancy must be replaced by that much ballast.
    assert result.ballast_to_replace_initial_buoyancy_kn == pytest.approx(gamma*2200)


def test_rotating_edge_immersion_prevents_a_false_buoyancy_balance():
    result = punch_consequence(synthetic_curve(False, 20000), JackupInputs(), 'in_water', 0)
    # Pivot x=-10 m; failed leg x=20 m, centre x=0 m and edge x=30 m.
    # Moment equilibrium: 20000*30/10=60000 kN required at the centre.
    # Required travel=3*60000/(1.025*9.81*2200)=8.136862019 m.
    # Edge descent=travel*40/30=10.849149359 m exceeds freeboard=6 m.
    assert not result.buoyancy_can_balance_loss
    assert result.buoyancy_replacement_kn == pytest.approx(60000)
    assert result.buoyancy_balance_travel_m == pytest.approx(8.136862019)
    assert result.buoyancy_balance_travel_m*40/30 == pytest.approx(10.849149359)
    assert result.buoyancy_replacement_kn > result.reserve_buoyancy_kn
    assert result.travel_is_lower_bound
    assert result.travel_m == 1  # Unresolved peak-to-trough travel, not arrest.


@pytest.mark.parametrize('changes, reserve_kn', [
    ({}, 33182.325),
    ({'hull_length_m': 80}, 26545.860),
    ({'leg_plan_radius_m': 30}, 44243.100),
    ({'draft_m': 2, 'hull_depth_m': 5, 'dry_weight_kn': 50000}, 16591.1625),
])
def test_rotating_reserve_tracks_edge_lever_and_freeboard(changes, reserve_kn):
    # Centre sinkages at immersion: 6*10/40=1.5, 6*10/50=1.2,
    # 6*15/45=2.0 and 3*10/40=0.75 m; multiply each by 1.025*9.81*2200.
    p = replace(JackupInputs(), **changes)
    result = punch_consequence(synthetic_curve(False, 20000), p, 'in_water', 0)
    assert result.reserve_buoyancy_kn == pytest.approx(reserve_kn)
    assert not result.buoyancy_can_balance_loss
    assert result.travel_is_lower_bound


@pytest.mark.parametrize('loss_kn, balances', [(10000, True), (12000, False)])
def test_buoyancy_balance_requires_clearance_at_the_rotating_edge(loss_kn, balances):
    result = punch_consequence(synthetic_curve(False, loss_kn), JackupInputs(), 'in_water', 0)
    # Max travel before immersion=6*30/40=4.5 m; allowable loss=33182.325/3 kN.
    assert result.buoyancy_can_balance_loss is balances
    assert (result.buoyancy_balance_travel_m <= 4.5) is balances
    assert result.travel_is_lower_bound is not balances


def test_no_soil_recovery_stays_a_lower_bound_unless_buoyancy_can_balance():
    elevated = punch_consequence(synthetic_curve(False), JackupInputs(), 'elevated', 10)
    assert elevated.travel_is_lower_bound
    assert elevated.travel_m == 1  # trough minus peak, not a fabricated arrest
    wet = punch_consequence(synthetic_curve(False), JackupInputs(), 'in_water', 0)
    assert wet.buoyancy_can_balance_loss
    assert not wet.travel_is_lower_bound  # explicitly quasi-static buoyancy balance


def test_uniform_soil_capacity_and_preload_gravity_do_not_scale_to_zero():
    p = replace(JackupInputs(), preload_hs_m=0, preload_wind_m_s=0, preload_current_m_s=0)
    result = run(inputs=p, phase='preload')
    # At zero there is still 36 MN weight: 12 MN per leg.
    # At factor one add 18 MN to each leg: 30 MN reaction per leg.
    # Uniform clay limit at tip=24, bearing=22.5: (6.34*15+6*22.5)*pi*12^2/4.
    capacity = (6.34*15 + 6*22.5) * math.pi*12**2/4
    row = trace(result, 'leg_penetration')
    assert row.utilizations[:2] == pytest.approx([12000/capacity, 30000/capacity])
    assert row.first_failure_factor == pytest.approx((capacity-12000)/18000)
    assert 'spudcan_punch_through' in result.not_applicable


def test_single_leg_ballast_equilibrium_and_all_leg_ballast_difference():
    p = replace(JackupInputs(), preload_hs_m=0, preload_wind_m_s=0, preload_current_m_s=0)
    all_legs = run(inputs=p, phase='preload', preload_pattern='all_legs')
    single = run(inputs=p, phase='preload', preload_pattern='single_leg')
    assert all_legs.reactions_at_one_kn == pytest.approx([30000]*3)
    assert single.reactions_at_one_kn == pytest.approx([30000, 12000, 12000])
    assert all_legs.ballast_at_one_kn == 54000
    assert single.ballast_at_one_kn == 18000
    wet = run(inputs=p, phase='preload', hull_state='in_water', air_gap_m=0)
    assert wet.ballast_to_match_dry_preload_at_one_kn == pytest.approx(54000+1.025*9.81*2200)
    assert trace(wet, 'ballast_replacement_reserve').first_failure_factor == pytest.approx(
        (120000-1.025*9.81*2200)/54000)


def test_air_gap_and_overturning_use_independent_static_hand_calculations():
    # Wave H=0: only wind force, at depth+gap. No hydrodynamic imports needed.
    p = replace(JackupInputs(), storm_hs_m=0, storm_current_m_s=0)
    result = run(inputs=p)
    wind = 0.5*1.225*1.2*1800*40**2/1000
    moment = wind*(60+10)
    # Adverse direction uplifts x=20 leg: M=V*10 at loss of contact.
    assert trace(result, 'overturning').first_failure_factor == pytest.approx(36000*10/moment)
    assert trace(result, 'air_gap').utilizations == pytest.approx([1/10]*4)
    waved = run()
    # Assumed crest Hs/2=3 m and tide=1 m; g=10 => f=3.
    assert trace(waved, 'air_gap').first_failure_factor == pytest.approx(3)


def test_wet_hull_swell_jacking_and_ballast_are_screened():
    p = JackupInputs()
    result = run(hull_state='in_water', air_gap_m=0, load_factors=[0, 1, 2, 2.5])
    # Hull vertical swell proxy gamma*Aeffective*Hs/2.
    fz = 1.025*9.81*150*6/2
    assert result.actions['hull_vertical_swell_kn'] == pytest.approx(fz)
    # Independently check the holding system with wind only, so all forces
    # are obtained directly from drag and equilibrium rather than this model.
    wind_only = run(inputs=replace(p, storm_hs_m=0, storm_current_m_s=0),
                    hull_state='in_water', air_gap_m=0, load_factors=[0, 1, 2])
    wind = 0.5*1.225*1.2*1800*40**2/1000
    weight = 36000 - 1.025*9.81*2200
    reactions = [weight/3-2*wind*60/(3*20), weight/3+wind*60/(3*20)]
    assert trace(wind_only, 'leg_hull_holding').utilizations[1] == pytest.approx(
        max(abs(v) for v in reactions)/35000)
    assert 'hull_swell' in {m.name for m in result.sequence.modes}
    assert 'ballast_reserve' in {m.name for m in result.sequence.modes}
    assert 'air_gap' in result.not_applicable
    assert result.sequence.governing


@pytest.mark.parametrize('change', [
    {'hull_state': 'other'}, {'phase': 'other'}, {'preload_pattern': 'other'},
    {'air_gap_m': 0}, {'hull_state': 'in_water', 'air_gap_m': 1},
])
def test_inconsistent_states_rejected(change):
    with pytest.raises(ValueError):
        run(**change)


@pytest.mark.parametrize('changes', [{'draft_m': 7}, {'chord_thickness_m': 0.4},
                                      {'dry_weight_kn': float('nan')}, {'tide_m': True}])
def test_invalid_inputs_rejected(changes):
    with pytest.raises(ValueError):
        replace(JackupInputs(), **changes)


def test_portable_export_has_case_selector_and_gap_transition_brackets(tmp_path):
    path = Path('examples/domains/geotechnical/jackup_what_breaks_first.py')
    spec = importlib.util.spec_from_file_location('failure_example', path)
    example = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(example)
    example.write_examples(tmp_path)
    summary = json.loads((tmp_path / 'summary.json').read_text(encoding='utf-8'))
    figure = json.loads((tmp_path / 'utilization.figure.json').read_text(encoding='utf-8'))
    inputs = json.loads((tmp_path / 'inputs.json').read_text(encoding='utf-8'))
    assert len(summary['cases']) == 54  # 9 soils * (2 preload patterns + storm) * 2 hull states
    assert len(inputs['soil_cases']) == 9
    assert all(case['ordered_thresholds'] for case in summary['cases'])
    assert all(case['not_reached'] is not None for case in summary['cases'])
    for case in summary['cases']:
        consequence = case['consequence_at_first_soil_peak']
        expected_reserve = 33182.325 if case['name'].endswith('in_water') else 0
        assert consequence['reserve_buoyancy_kn'] == pytest.approx(expected_reserve)
    assert len(figure['layout']['updatemenus'][0]['buttons']) == 54
    assert len(figure['layout']['updatemenus'][0]['buttons'][0]['args'][0]['visible']) == len(figure['data'])
    assert summary['air_gap_changes']
    assert all(change['lower_gap_m'] < change['upper_gap_m'] for change in summary['air_gap_changes'])
    # No URLs or paths are needed to evaluate the exported Plotly data arrays.
    assert all('x' in trace and 'y' in trace for trace in figure['data'])
    # Independent geometry oracle: every level-water effective weight is positive.
    assert 36000 - 1.025*9.81*2200 > 0


def test_chord_brace_and_sliding_ratios_from_independent_wind_statics():
    p = replace(JackupInputs(), storm_hs_m=0, storm_current_m_s=0)
    result = run(inputs=p)
    wind = 0.5*1.225*1.2*1800*40**2/1000
    reaction = 12000 + wind*70/60
    chord_area = math.pi/4*(0.6**2-0.55**2)
    chord_i = math.pi/64*(0.6**4-0.55**4)
    chord_limit = min(chord_area*355e3, math.pi**2*210e6*chord_i/12**2)
    chord_force = reaction/3 + 2*(wind/3*70)/(3*8)
    brace_area = math.pi/4*(0.25**2-0.214**2)
    brace_i = math.pi/64*(0.25**4-0.214**4)
    brace_limit = min(brace_area*355e3, math.pi**2*210e6*brace_i/7**2)
    brace_force = wind/(6*math.sin(math.radians(35)))
    assert trace(result, 'chord_buckling').utilizations[1] == pytest.approx(chord_force/chord_limit)
    assert trace(result, 'brace_buckling').utilizations[1] == pytest.approx(brace_force/brace_limit)
    assert trace(result, 'sliding').utilizations[1] == pytest.approx(wind/(0.25*36000))


def test_conditional_damage_starts_at_the_independent_soil_threshold(monkeypatch):
    # Synthetic peak 20 MN, base reaction 12 MN, increment 18 MN/factor.
    # The soil threshold is (20-12)/18=4/9. Imposed run exceeds elastic
    # capacity at that instant, so it cannot be reported as an earlier failure.
    import digitalmodel.marine_ops.jackup_failure_sequence as module
    monkeypatch.setattr(module, 'penetration_curve', lambda *args: synthetic_curve())
    p = replace(JackupInputs(), preload_hs_m=0, preload_current_m_s=0, preload_wind_m_s=0)
    result = run(inputs=p, phase='preload')
    assert trace(result, 'spudcan_punch_through').first_failure_factor == pytest.approx(4/9)
    assert trace(result, 'post_punch_chord').first_failure_factor == pytest.approx(4/9)
    assert trace(result, 'post_punch_brace').first_failure_factor == pytest.approx(4/9)
    assert trace(result, 'post_punch_chord').failure_utilization > 1
