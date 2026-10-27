# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic arithmetic and missing-evidence contracts."""

from dataclasses import replace
import math

import pytest

from digitalmodel.marine_ops.liftboat_screening import (
    Basis, BendingInput, Check, HydrostaticPoint, StabilityCriteria,
    StabilityInput, StormInput, WaterStrip, WindPanel, combine_checks,
    screen_bending, screen_stability, screen_storm,
)


BASIS = Basis('synthetic-v1', 'synthetic arithmetic', 'illustrative-v1',
              'exact synthetic point; no measured uncertainty',
              'synthetic static model only')
SUPPORTS = ((-5.0, -3.0), (5.0, -3.0), (0.0, 6.0))


def storm():
    """One strip per leg: 1 kN drag plus 1 kN inertia at z=5 m."""
    strip = WaterStrip(5, 1, 1, 1, math.sqrt(2), 1, 4 / math.pi)
    return StormInput(
        basis=BASIS, case='synthetic-x', supports_m=SUPPORTS,
        vertical_load_kn=900, gravity_moments_knm=(0, 0), foundation_moments_knm=((0, 0),) * 3, heading_deg=0,
        water_density_kg_m3=1000, air_density_kg_m3=2,
        strips_by_leg=((strip,),) * 3,
        wind_panels=(WindPanel(10, 1, 1, 2),),
        horizontal_shares=(1 / 3,) * 3,
        vertical_allowables_kn=(500,) * 3,
        friction_coefficients=(0.5,) * 3,
        horizontal_allowables_kn=(100,) * 3,
        foundation_moment_allowables_knm=(1000,) * 3,
        preload_proven_kn=(450,) * 3,
        hull_underside_m=12, governing_surface_m=10,
        minimum_clearance_m=1, load_factor=1,
        return_basis='synthetic load case only',
        support_evidence='identical stiffness, rigid hull',
        dynamics_evidence='static synthetic actions only',
        morison_evidence='prescribed slender isolated strips',
        foundation_evidence='synthetic separate combined-load allowables',
    )


def bending():
    """Fixed foundation cantilever with a rotational spring at the hull guide."""
    return BendingInput(
        basis=BASIS, leg_type='tubular', axial_kn=300, lateral_loads=((10, 5),),
        guide_height_m=10, top_moment_knm=0, elastic_modulus_kpa=2e8,
        second_moment_m4=0.01, guide_stiffness_knm_rad=600000,
        axial_allowable_kn=1000, moment_allowable_knm=100,
        shear_allowable_kn=100, guide_moment_allowable_knm=100,
        deflection_allowable_m=1, euler_load_kn=10000,
        condition_evidence='synthetic intact tubular section',
        restraint_evidence='fixed foundation, hull rotation restrained',
        capacity_evidence='synthetic interaction includes member buckling',
        fatigue_evidence='separate synthetic duty assessment',
    )


def stability():
    """Triangular GZ in radians: area 0..30 = 0.15 m rad; ratio=1.5."""
    return StabilityInput(
        basis=BASIS, table=(HydrostaticPoint(0, 0),
            HydrostaticPoint(30, 0.6), HydrostaticPoint(60, 0)),
        gm_solid_m=1.2, free_surface_correction_m=0.2,
        downflooding_deg=50, legs_raised=True,
        criteria=StabilityCriteria(0.8, 40, ((0, 30, 0.1),),
                                   ((0, 30, 30, 50, 1.0),)),
        loading_state='synthetic-transit', displacement_kn=900,
        draft_m=1.5, trim_deg=0, leg_position_evidence='raised and secured',
        geometry_evidence='synthetic watertight hull and openings',
        weight_evidence='synthetic mass and centres',
        heeling_evidence='zero heeling actions in synthetic still water',
        damage_evidence='outside synthetic intact-only basis',
    )


def test_status_aggregation_never_hides_missing_or_failed_evidence():
    assert combine_checks((Check('a', 'met'),)) == 'met'
    assert combine_checks(()) == 'unresolved'
    assert combine_checks((Check('a', 'met'), Check('b', 'unresolved'))) == 'unresolved'
    assert combine_checks((Check('a', 'not_met'), Check('b', 'unresolved'))) == 'not_met'


def test_storm_morison_wind_equilibrium_and_per_leg_margins():
    result = screen_storm(storm())
    assert result.status == 'met'
    # Sum horizontal = 3*(1+1)+0.004 kN; moment = 30+0.04 kNm.
    assert result.metrics['base_shear_kn'] == pytest.approx(6.004)
    assert result.metrics['moment_y_knm'] == pytest.approx(30.04)
    reactions = result.metrics['reactions_kn']
    assert sum(reactions) == pytest.approx(900)
    assert sum(x * r for (x, _), r in zip(SUPPORTS, reactions)) == pytest.approx(30.04)
    assert sum(y * r for (_, y), r in zip(SUPPORTS, reactions)) == pytest.approx(0)
    assert result.metrics['water_force_by_leg_kn'] == pytest.approx((2, 2, 2))
    assert result.metrics['minimum_overturning_margin_kn'] == min(reactions)
    assert result.by_name('leg_1.sliding').utilization == pytest.approx(2.001333333 / 100)


@pytest.mark.parametrize('changes,check', [
    ({'vertical_allowables_kn': (100,) * 3}, 'leg_1.vertical'),
    ({'friction_coefficients': (0.001,) * 3}, 'leg_1.sliding'),
    ({'hull_underside_m': 10}, 'hull_clearance'),
    ({'preload_proven_kn': (100,) * 3}, 'leg_1.preload'),
    ({'gravity_moments_knm': (0, 5000)}, 'leg_1.contact'),
])
def test_storm_failing_cases(changes, check):
    result = screen_storm(replace(storm(), **changes))
    assert result.status == 'not_met'
    assert result.by_name(check).status == 'not_met'


@pytest.mark.parametrize('field', [
    'water_density_kg_m3', 'strips_by_leg', 'gravity_moments_knm',
    'vertical_allowables_kn', 'friction_coefficients', 'minimum_clearance_m',
    'return_basis', 'support_evidence', 'dynamics_evidence', 'morison_evidence',
    'foundation_evidence', 'preload_proven_kn', 'load_factor',
])
def test_storm_missing_input_never_passes(field):
    assert screen_storm(replace(storm(), **{field: None})).status == 'unresolved'


def test_bending_independent_rotational_spring_solution_and_limits():
    result = screen_bending(bending())
    assert result.status == 'met'
    # EI=2,000,000 kNm²; theta_free=P*a²/(2*EI)=0.0000625 rad.
    # kL/EI=3; Mguide=k*theta_free/(1+3)=9.375 kNm.
    assert result.metrics['guide_moment_knm'] == pytest.approx(9.375)
    assert result.metrics['guide_rotation_rad'] == pytest.approx(0.000015625)
    assert result.metrics['base_moment_knm'] == pytest.approx(40.625)
    assert result.metrics['first_order_max_moment_knm'] == pytest.approx(40.625)
    assert result.metrics['first_order_guide_deflection_m'] == pytest.approx(0.000286458333333)
    assert result.metrics['amplification'] == pytest.approx(1 / 0.97)
    assert result.by_name('axial_bending').utilization == pytest.approx(0.3 + 0.40625 / 0.97)


@pytest.mark.parametrize('changes,check', [
    ({'moment_allowable_knm': 10}, 'axial_bending'),
    ({'guide_moment_allowable_knm': 1}, 'guide_connection'),
    ({'shear_allowable_kn': 1}, 'shear'),
    ({'deflection_allowable_m': 0.0001}, 'deflection'),
    ({'euler_load_kn': 200}, 'instability'),
])
def test_bending_failing_cases(changes, check):
    result = screen_bending(replace(bending(), **changes))
    assert result.status == 'not_met'
    assert result.by_name(check).status == 'not_met'


@pytest.mark.parametrize('field', [
    'guide_stiffness_knm_rad', 'moment_allowable_knm', 'euler_load_kn',
    'condition_evidence', 'restraint_evidence', 'capacity_evidence', 'fatigue_evidence',
])
def test_bending_missing_evidence_never_passes(field):
    assert screen_bending(replace(bending(), **{field: None})).status == 'unresolved'


def test_bending_free_and_rigid_guide_limits():
    free = screen_bending(replace(bending(), guide_stiffness_knm_rad=0))
    stiff = screen_bending(replace(bending(), guide_stiffness_knm_rad=1e18))
    assert free.metrics['guide_moment_knm'] == 0
    assert free.metrics['base_moment_knm'] == 50
    assert stiff.metrics['guide_moment_knm'] == pytest.approx(12.5)
    assert stiff.metrics['guide_rotation_rad'] == pytest.approx(0, abs=1e-15)


def test_stability_corrects_entire_curve_and_interpolates_in_radians():
    result = screen_stability(stability())
    assert result.status == 'met'
    assert result.metrics['gm_corrected_m'] == 1
    # Corrected ordinate at 30 deg = 0.6 - 0.2*sin(30 deg) = 0.5 m.
    assert result.metrics['corrected_gz_m'][1] == pytest.approx(0.5)
    assert result.metrics['areas_m_rad'][0] == pytest.approx(math.pi / 24)
    assert result.metrics['positive_range_deg'] == pytest.approx(30 + 30 * 0.5 / (0.5 + 0.2 * math.sin(math.pi / 3)))
    assert result.metrics['area_ratios'][0] > 1


@pytest.mark.parametrize('changes,check', [
    ({'gm_solid_m': 0.3}, 'gm'),
    ({'downflooding_deg': 20}, 'positive_range'),
    ({'legs_raised': False}, 'raised_legs'),
    ({'criteria': StabilityCriteria(0.8, 40, ((0, 30, 1),), ())}, 'area_1'),
    ({'criteria': StabilityCriteria(0.8, 40, (), ((0, 30, 30, 50, 100),))}, 'area_ratio_1'),
])
def test_stability_failing_cases(changes, check):
    result = screen_stability(replace(stability(), **changes))
    assert result.status == 'not_met'
    assert result.by_name(check).status == 'not_met'


@pytest.mark.parametrize('field', [
    'table', 'gm_solid_m', 'free_surface_correction_m', 'downflooding_deg',
    'legs_raised', 'criteria', 'leg_position_evidence', 'geometry_evidence',
    'weight_evidence', 'heeling_evidence', 'damage_evidence',
])
def test_stability_missing_evidence_never_passes(field):
    assert screen_stability(replace(stability(), **{field: None})).status == 'unresolved'


def test_stability_does_not_extrapolate_or_integrate_past_flooding():
    short = replace(stability(), table=stability().table[:2])
    assert screen_stability(short).by_name('positive_range').status == 'unresolved'
    flood = screen_stability(replace(stability(), downflooding_deg=20))
    assert flood.by_name('area_1').status == 'unresolved'
    truncated = replace(stability(), table=(HydrostaticPoint(0, 0),
        HydrostaticPoint(10, -0.1), HydrostaticPoint(30, 0.5), HydrostaticPoint(60, 0)))
    assert screen_stability(truncated).by_name('positive_range').status == 'not_met'


@pytest.mark.parametrize('maker,screen,changes', [
    (storm, screen_storm, {'load_factor': float('nan')}),
    (storm, screen_storm, {'horizontal_shares': (1, 1, 1)}),
    (storm, screen_storm, {'supports_m': ((0, 0),) * 3}),
    (storm, screen_storm, {'vertical_load_kn': True}),
    (bending, screen_bending, {'guide_stiffness_knm_rad': -1}),
    (bending, screen_bending, {'lateral_loads': ((10, 11),)}),
    (stability, screen_stability, {'gm_solid_m': float('inf')}),
    (stability, screen_stability, {'table': tuple(reversed(stability().table))}),
])
def test_invalid_inputs_are_rejected(maker, screen, changes):
    with pytest.raises(ValueError):
        screen(replace(maker(), **changes))


@pytest.mark.parametrize('maker,screen', [
    (storm, screen_storm), (bending, screen_bending), (stability, screen_stability),
])
@pytest.mark.parametrize('field', ['configuration', 'provenance', 'criteria_version',
                                   'uncertainty', 'applicability'])
def test_shared_basis_missing_fields_block_passing(maker, screen, field):
    value = maker()
    assert screen(replace(value, basis=replace(BASIS, **{field: None}))).status == 'unresolved'


def preload():
    """Flat-pad synthetic mobilization uses independently calculated sand factors."""
    from digitalmodel.geotechnical.spudcan import UniformSand
    from digitalmodel.marine_ops.liftboat_screening import PreloadInput
    from digitalmodel.marine_ops.preload_sequence import PreloadCriteria, PreloadSample, PreloadStage
    phi = math.pi / 6
    nq = math.exp(math.pi * math.tan(phi)) * math.tan(math.pi / 4 + phi / 2)**2
    ngamma = 1.5 * (nq - 1) * math.tan(phi)
    at_half = (0.3 * 10 * 2 * ngamma + 1.2 * 10 * 0.5 * 1.025 * nq) * math.pi
    depth = 100 / at_half * 0.5
    stages = (
        PreloadStage('preparation', (PreloadSample(0, 0, 0),)),
        PreloadStage('preload', (PreloadSample(0.1, 50, depth / 2), PreloadSample(0.2, 100, depth))),
        PreloadStage('holding', (PreloadSample(0.25, 100, depth), PreloadSample(1.25, 100, depth))),
        PreloadStage('discharge', (PreloadSample(2, 10, depth),)),
    )
    return PreloadInput(
        basis=BASIS, leg='leg_1', foundation_type='pad', preload_method='ballast',
        profiles=(UniformSand(2, 10, 30), UniformSand(2, 8, 30)),
        depths_m=(0, 0.5, 1), stages=stages, target_kn=100, resistance_factor=1,
        criteria=PreloadCriteria(1, 0.25, 0.001, 0.01, 0.05),
        depth_deviation_allowable_m=0.1, available_travel_m=1,
        jacking_allowable_kn=150, governing_load_kn=90,
        foundation_condition='synthetic intact flat circular pad',
        soil_evidence='synthetic homogeneous sand, level undisturbed seabed; no cycles',
        support_evidence='three determinate vertical reactions; separate frame compatibility',
        monitoring_plan='hold at target; record travel and settlement; review capacity drop',
    )


def test_preload_pass_reuses_bearing_and_sequence_and_bounds_penetration():
    from digitalmodel.marine_ops.liftboat_screening import screen_preload
    result = screen_preload(preload())
    assert result.status == 'met'
    assert len(result.metrics['histories']) == 2
    low, high = result.metrics['predicted_penetration_range_m']
    assert low > 0
    assert high > low
    assert result.metrics['curves'][0][0].load_kn == 0
    assert all(h.hold_met for h in result.metrics['histories'])
    assert result.by_name('soil_1.target_travel').demand == pytest.approx(
        preload().stages[2].samples[0].penetration_m)


@pytest.mark.parametrize('changes,check', [
    ({'available_travel_m': 0.001}, 'observed_travel'),
    ({'jacking_allowable_kn': 50}, 'jacking'),
    ({'governing_load_kn': 120}, 'governing_preload'),
    ({'depth_deviation_allowable_m': 0.00001}, 'soil_2.depth_deviation'),
])
def test_preload_failing_cases(changes, check):
    from digitalmodel.marine_ops.liftboat_screening import screen_preload
    result = screen_preload(replace(preload(), **changes))
    assert result.status == 'not_met'
    assert result.by_name(check).status == 'not_met'


@pytest.mark.parametrize('field', [
    'foundation_type', 'preload_method', 'profiles', 'depths_m', 'stages',
    'target_kn', 'criteria', 'depth_deviation_allowable_m', 'available_travel_m',
    'jacking_allowable_kn', 'governing_load_kn', 'foundation_condition',
    'soil_evidence', 'support_evidence', 'monitoring_plan',
])
def test_preload_missing_input_never_passes(field):
    from digitalmodel.marine_ops.liftboat_screening import screen_preload
    assert screen_preload(replace(preload(), **{field: None})).status == 'unresolved'


def test_pad_does_not_use_conical_calibration_and_no_equilibrium_is_unresolved():
    from digitalmodel.geotechnical.spudcan import SandOverClay
    from digitalmodel.marine_ops.liftboat_screening import screen_preload
    conical = SandOverClay(10, math.pi * 25, 3, 30, 1, 10, 6, 0.6)
    result = screen_preload(replace(preload(), profiles=(conical,)))
    assert result.status == 'unresolved'
    high = screen_preload(replace(preload(), governing_load_kn=100000))
    assert high.by_name('soil_1.governing_travel').status == 'unresolved'


def plan():
    """Equal stage reactions balance total weight, including an afloat origin."""
    from digitalmodel.marine_ops.liftboat_screening import PreloadLoadCase, PreloadPlan
    cases = tuple(PreloadLoadCase(stage.name, sample.time_h, sample.applied_load_kn * 3,
                                 0, (0, 0), (0, 0), (0, 0))
                  for stage in preload().stages for sample in stage.samples)
    return PreloadPlan(BASIS, SUPPORTS, cases, 0.01)


def legs():
    return tuple(replace(preload(), leg=f'leg_{i}') for i in range(1, 4))


def test_staged_equilibrium_pass_fail_and_missing():
    from digitalmodel.marine_ops.liftboat_screening import screen_preload_plan
    result = screen_preload_plan(plan(), legs())
    assert result.status == 'met'
    assert len(result.metrics['stage_reactions_kn']) == 6
    changed = replace(plan().cases[1], weight_centre_m=(1, 0))
    bad = replace(plan(), cases=(plan().cases[0], changed, *plan().cases[2:]))
    assert screen_preload_plan(bad, legs()).status == 'not_met'
    assert screen_preload_plan(replace(plan(), cases=plan().cases[:-1]), legs()).status == 'unresolved'
    assert screen_preload_plan(replace(plan(), reaction_tolerance_kn=None), legs()).status == 'unresolved'


def integrated():
    """Each integrated calculation receives the same elevated configuration."""
    from digitalmodel.marine_ops.liftboat_screening import screen_liftboat
    return screen_liftboat(
        preloads=legs(), preload_plan=plan(),
        storm=replace(storm(), vertical_load_kn=240),
        bending_templates=(replace(bending(), guide_height_m=12),) * 3, transit=stability(),
        interface_evidence='synthetic ballast discharged before raised-leg transit',
        transit_weight_change_kn=660, interface_weight_tolerance_kn=0.01,
        adjacent_evidence={},
    )


def test_integrated_case_preserves_coupling_and_never_becomes_overall_approval():
    result = integrated()
    assert result.status == 'unresolved'  # Resistance and adjacent work remain open.
    assert result.by_name('resistance').status == 'unresolved'
    assert result.metrics['preload_plan'].status == 'met'
    assert result.metrics['storm'].status == 'met'
    reactions = result.metrics['storm'].metrics['reactions_kn']
    base_moments = result.metrics['foundation_moments_knm']
    external = result.metrics['storm'].metrics['external_moment_y_knm']
    assert sum(x * r for (x, _), r in zip(SUPPORTS, reactions)) + sum(m[1] for m in base_moments) == pytest.approx(external)
    for i, response in enumerate(result.metrics['bending']):
        assert response.metrics['axial_kn'] == pytest.approx(reactions[i])
    assert result.metrics['preloads'][0].by_name('governing_preload').demand >= reactions[0]


def test_integrated_missing_configuration_link_and_unproven_preload_are_unresolved():
    from digitalmodel.marine_ops.liftboat_screening import screen_liftboat
    result = screen_liftboat(
        preloads=legs(), preload_plan=plan(), storm=storm(),
        bending_templates=(replace(bending(), guide_height_m=12),) * 3, transit=stability(),
        interface_evidence=None, transit_weight_change_kn=None,
        interface_weight_tolerance_kn=0.01, adjacent_evidence={},
    )
    assert result.by_name('configuration_link').status == 'unresolved'
    # A missing preload history cannot be replaced by the storm input's claimed preload.
    result = screen_liftboat(
        preloads=(replace(legs()[0], stages=None), *legs()[1:]),
        preload_plan=plan(), storm=storm(), bending_templates=(replace(bending(), guide_height_m=12),) * 3,
        transit=stability(), interface_evidence='synthetic phase link',
        transit_weight_change_kn=0, interface_weight_tolerance_kn=0.01, adjacent_evidence={},
    )
    assert result.metrics['storm'].status == 'unresolved'


def test_preload_short_hold_settlement_and_missing_window():
    from digitalmodel.marine_ops.liftboat_screening import screen_preload
    value = preload()
    holding = value.stages[2]
    last = holding.samples[-1]
    short = replace(holding, samples=(holding.samples[0], replace(last, time_h=0.75)))
    result = screen_preload(replace(value, stages=(*value.stages[:2], short, value.stages[3])))
    assert result.by_name('soil_1.holding').status == 'not_met'
    moving = replace(holding, samples=(holding.samples[0], replace(last, penetration_m=last.penetration_m + 0.01)))
    result = screen_preload(replace(value, stages=(*value.stages[:2], moving, value.stages[3])))
    assert result.by_name('soil_1.holding').status == 'not_met'
    criteria = replace(value.criteria, settlement_window_h=2)
    assert screen_preload(replace(value, criteria=criteria)).by_name('soil_1.holding').status == 'unresolved'


def test_conical_can_flags_capacity_drop_without_claiming_pad_applicability():
    from digitalmodel.geotechnical.spudcan import SandOverClay
    from digitalmodel.marine_ops.liftboat_screening import screen_preload
    profile = SandOverClay(10, math.pi * 25, 3, 30, 1, 10, 6, 0.6)
    result = screen_preload(replace(preload(), foundation_type='spudcan',
                                   profiles=(profile,), depths_m=(0, 1, 3, 6)))
    assert result.by_name('soil_1.penetration_hazard').status == 'not_met'
    out_of_range = replace(profile, sand_thickness_m=0.1)
    assert screen_preload(replace(preload(), foundation_type='spudcan', profiles=(out_of_range,))).status == 'unresolved'


def test_resistance_factor_is_an_explicit_capacity_input():
    from digitalmodel.marine_ops.liftboat_screening import screen_preload
    full = screen_preload(preload())
    reduced = screen_preload(replace(preload(), resistance_factor=0.5))
    assert reduced.metrics['curves'][0][1].load_kn == pytest.approx(full.metrics['curves'][0][1].load_kn / 2)
    assert screen_preload(replace(preload(), resistance_factor=None)).status == 'unresolved'
    with pytest.raises(ValueError):
        screen_preload(replace(preload(), resistance_factor=1.1))


def test_storm_heading_signed_kinematics_and_per_leg_shear_do_not_cancel():
    value = replace(storm(), heading_deg=90)
    result = screen_storm(value)
    assert result.metrics['moment_x_knm'] == pytest.approx(-30.04)
    assert result.metrics['moment_y_knm'] == pytest.approx(0, abs=1e-12)
    reversed_strip = replace(value.strips_by_leg[0][0], velocity_m_s=-math.sqrt(2),
                             acceleration_m_s2=-4 / math.pi)
    reverse = screen_storm(replace(value, strips_by_leg=((reversed_strip,),) * 3,
                                  wind_panels=(replace(value.wind_panels[0], velocity_m_s=-2),)))
    assert reverse.metrics['moment_x_knm'] == pytest.approx(30.04)
    opposing = replace(value, strips_by_leg=((reversed_strip,), value.strips_by_leg[1], value.strips_by_leg[2]))
    opposing_result = screen_storm(opposing)
    assert sum(opposing_result.metrics['horizontal_demands_kn']) > opposing_result.metrics['base_shear_kn']


def test_storm_missing_foundation_moments_or_local_kinematics_is_unresolved():
    assert screen_storm(replace(storm(), foundation_moments_knm=None)).status == 'unresolved'
    assert screen_storm(replace(storm(), strips_by_leg=((), (), ()))).status == 'unresolved'


def test_bending_multiple_loads_top_couple_and_truss_boundary():
    # Opposing forces give zero base shear but nonzero internal shear/moment.
    value = replace(bending(), lateral_loads=((10, 10), (-10, 5)), top_moment_knm=5)
    result = screen_bending(value)
    assert result.metrics['foundation_shear_kn'] == 0
    assert result.metrics['first_order_max_shear_kn'] == 10
    expected_rotation = (10 * 100 / 2 - 10 * 25 / 2 + 5 * 10) / (2000000 + 600000 * 10)
    assert result.metrics['guide_rotation_rad'] == pytest.approx(expected_rotation)
    assert result.metrics['base_moment_knm'] == pytest.approx(55 - 600000 * expected_rotation)
    assert screen_bending(replace(value, leg_type='truss')).status == 'unresolved'
    assert screen_bending(replace(value, leg_type=None)).status == 'unresolved'


def test_gz_nonpositive_ratio_denominator_and_missing_criterion_are_unresolved():
    # The corrected GZ crosses zero near 52 deg; this denominator is negative.
    criteria = StabilityCriteria(0.8, 40, (), ((0, 30, 55, 60, 1),))
    result = screen_stability(replace(stability(), criteria=criteria, downflooding_deg=60))
    assert result.by_name('area_ratio_1').status == 'unresolved'
    assert screen_stability(replace(stability(), criteria=replace(criteria, minimum_gm_m=None))).status == 'unresolved'


def test_all_example_cases_run_with_no_files_and_no_overall_approval():
    import importlib.util
    from pathlib import Path
    path = Path(__file__).resolve().parents[2] / 'examples/domains/geotechnical/liftboat_screening.py'
    spec = importlib.util.spec_from_file_location('synthetic_liftboat_example', path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    results = module.run_example()
    assert len(results) == 8
    for result in results.values():
        assert result.status == 'unresolved'
        assert result.metrics['storm'].status == 'met'
        assert result.metrics['transit'].status == 'met'
        assert all(p.status == 'met' for p in result.metrics['preloads'])
        assert all(b.status == 'met' for b in result.metrics['bending'])
    display = module.summary(results)
    assert display['heading_0_stiffness_0']['base_shear_kn'] == 144.7
    assert display['heading_0_stiffness_0']['reactions_kn'] == [2000, 2000, 2000]


def test_missing_nested_coefficients_and_gz_are_unresolved():
    strip = replace(storm().strips_by_leg[0][0], acceleration_m_s2=None)
    assert screen_storm(replace(storm(), strips_by_leg=((strip,),) * 3)).status == 'unresolved'
    table = (HydrostaticPoint(0, 0), HydrostaticPoint(30, None), HydrostaticPoint(60, 0))
    assert screen_stability(replace(stability(), table=table)).status == 'unresolved'


def test_phase_weight_balance_requires_numerical_reconciliation():
    from digitalmodel.marine_ops.liftboat_screening import screen_liftboat
    result = screen_liftboat(
        preloads=legs(), preload_plan=plan(), storm=replace(storm(), vertical_load_kn=240),
        bending_templates=(replace(bending(), guide_height_m=12),) * 3, transit=stability(),
        interface_evidence='synthetic configuration link', transit_weight_change_kn=0,
        interface_weight_tolerance_kn=0.01, adjacent_evidence={},
    )
    assert result.by_name('phase_weight_balance').status == 'not_met'
    assert result.status == 'not_met'


def test_predicted_travel_and_area_ratio_have_independent_expectations():
    from digitalmodel.marine_ops.liftboat_screening import screen_preload
    assert screen_preload(replace(preload(), available_travel_m=0.001)).by_name('soil_1.target_travel').status == 'not_met'
    corrected_at_50 = 0.5 - (0.5 + 0.2 * math.sin(math.pi / 3)) * 20 / 30
    numerator = 0.5 * 0.5 * math.pi / 6
    denominator = (0.5 + corrected_at_50) / 2 * math.pi / 9
    assert screen_stability(stability()).metrics['area_ratios'][0] == pytest.approx(numerator / denominator)


def test_foundation_moment_and_overturning_utilizations_are_explicit():
    value = replace(storm(), foundation_moments_knm=((0, 10),) * 3,
                    foundation_moment_allowables_knm=(5,) * 3)
    result = screen_storm(value)
    assert result.by_name('leg_1.foundation_moment').status == 'not_met'
    assert result.by_name('leg_1.foundation_moment').utilization == 2
    assert screen_storm(replace(value, foundation_moment_allowables_knm=None)).status == 'unresolved'
    assert screen_storm(storm()).by_name('leg_1.foundation_moment').status == 'met'
    normal = screen_storm(storm())
    assert normal.by_name('leg_1.overturning').utilization == pytest.approx(
        (300 - normal.metrics['reactions_kn'][0]) / 300)
    failed = screen_storm(replace(storm(), gravity_moments_knm=(0, 5000)))
    assert failed.by_name('leg_1.overturning').status == 'not_met'


def test_preload_plan_rejects_duplicate_times_instead_of_collapsing_them():
    from digitalmodel.marine_ops.liftboat_screening import screen_preload_plan
    value = legs()[0]
    stage = replace(value.stages[1], samples=(value.stages[1].samples[0],) * 2)
    changed = replace(value, stages=(value.stages[0], stage, *value.stages[2:]))
    with pytest.raises(ValueError):
        screen_preload_plan(plan(), (changed, *legs()[1:]))
