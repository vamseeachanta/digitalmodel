# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Generic three-leg example: synthetic data, without operating authorization.

Run with PYTHONPATH=src python examples/domains/geotechnical/liftboat_screening.py.
JSON is printed to stdout; redirect it to a scratch location when needed.
All numerical inputs are assumed, based on previous experience, solely for
arithmetic demonstration. No class-rule or measured capacity is represented.
"""

from __future__ import annotations

import json
from dataclasses import replace

from digitalmodel.geotechnical.spudcan import UniformSand, penetration_curve
from digitalmodel.marine_ops.liftboat_screening import (
    Basis, BendingInput, HydrostaticPoint, PreloadInput, PreloadLoadCase,
    PreloadPlan, Screen, StabilityCriteria, StabilityInput, StormInput,
    WaterStrip, WindPanel, screen_liftboat,
)
from digitalmodel.marine_ops.preload_sequence import PreloadCriteria, PreloadSample, PreloadStage


def example_inputs() -> tuple:
    """Return synthetic phase inputs; constructed readings are not observations."""
    basis = Basis(
        'synthetic-elevated-v1', 'synthetic assumptions; no measured inputs',
        'illustrative-criteria-v1; no class compliance claim',
        'soil weight 8 to 10 kN/m³; guide stiffness 0 to 1,000,000 kNm/rad; four headings',
        'circular flat pads, homogeneous sand; fixed-base prismatic tubular legs; static aligned actions',
    )
    supports = ((-8.0, -5.0), (8.0, -5.0), (0.0, 10.0))
    profiles = (UniformSand(4, 10, 30), UniformSand(4, 8, 30))
    depths = (0, 0.4, 0.8)
    # Explicitly assume linear mobilization to the first positive grid point.
    # Construct a nominal settled history from that assumption; this does not
    # model actual penetration rate or delayed settlement.
    reference = penetration_curve(profiles[0], list(depths))[1]

    def reading(time: float, load: float) -> PreloadSample:
        return PreloadSample(time, load, load / reference.load_kn * reference.depth_m)

    stages = (
        PreloadStage('preparation', (reading(0, 500),)),
        PreloadStage('preload', (reading(0.1, 1500), reading(0.2, 3000))),
        PreloadStage('holding', (reading(0.25, 3000), reading(1.25, 3000))),
        PreloadStage('discharge', (reading(2, 2000),)),
    )
    leg = PreloadInput(
        basis=basis, leg='leg_1', foundation_type='pad', preload_method='combined',
        profiles=profiles, depths_m=depths, stages=stages, target_kn=3000, resistance_factor=1,
        criteria=PreloadCriteria(1, 0.25, 0.001, 0.01, 0.05),
        depth_deviation_allowable_m=0.2, available_travel_m=2,
        jacking_allowable_kn=6000, governing_load_kn=2000,
        foundation_condition='assumed intact circular pads; no slope or nearby disturbance',
        soil_evidence='assumed homogeneous shallow sand; no scour or cyclic degradation in synthetic case',
        support_evidence='three rigid-hull vertical supports; identical leg stiffness; no preload base moments',
        monitoring_plan='hold at target for duration and settlement checks; stop for abrupt penetration or travel exceedance',
    )
    legs = tuple(replace(leg, leg=f'leg_{i}') for i in range(1, 4))
    # Weight includes the entire unit and ballast. Buoyancy acts at the origin.
    cases = tuple(PreloadLoadCase(
        stage.name, sample.time_h, max(6000, 3 * sample.applied_load_kn),
        max(0, 6000 - 3 * sample.applied_load_kn), (0, 0), (0, 0), (0, 0))
        for stage in stages for sample in stage.samples)
    plan = PreloadPlan(basis, supports, cases, 0.1)
    strips = tuple(WaterStrip(z, 2, 1, 1, 1, 2, 0.3) for z in (1, 3, 5, 7, 9))
    storm = StormInput(
        basis=basis, case='synthetic-heading-0', supports_m=supports,
        vertical_load_kn=6000, gravity_moments_knm=(0, 0),
        foundation_moments_knm=((0, 0),) * 3, heading_deg=0,
        water_density_kg_m3=1025, air_density_kg_m3=1.225,
        strips_by_leg=(strips,) * 3,
        wind_panels=(WindPanel(21, 200, 1, 25), WindPanel(23, 100, 1, 25)),
        horizontal_shares=(1 / 3,) * 3, vertical_allowables_kn=(5000,) * 3,
        friction_coefficients=(0.4,) * 3, horizontal_allowables_kn=(1000,) * 3,
        foundation_moment_allowables_knm=(3000,) * 3,
        preload_proven_kn=(3000,) * 3, hull_underside_m=20,
        governing_surface_m=12, minimum_clearance_m=2, load_factor=1,
        return_basis='synthetic simultaneous snapshot; no return-period qualification',
        support_evidence='fixed foundations and level rigid hull; identical lateral share; no yaw or torsion',
        dynamics_evidence='static synthetic snapshot; no dynamic adequacy established',
        morison_evidence='assumed isolated slender strips; prescribed kinematics and growth-inclusive diameter',
        foundation_evidence='synthetic combined vertical, horizontal and moment capacities only',
    )
    bending = BendingInput(
        basis=basis, leg_type='tubular', axial_kn=2000, lateral_loads=((0, 0),), guide_height_m=20,
        top_moment_knm=0, elastic_modulus_kpa=200000000, second_moment_m4=0.08,
        guide_stiffness_knm_rad=1000000, axial_allowable_kn=8000,
        moment_allowable_knm=6000, shear_allowable_kn=500,
        guide_moment_allowable_knm=3000, deflection_allowable_m=0.2,
        euler_load_kn=80000,
        condition_evidence='synthetic intact uniform tubular member; no truss equivalence',
        restraint_evidence='fixed base, rotational spring at level hull; first-order equilibrium',
        capacity_evidence='illustrative combined member and connection allowables; not a versioned class method',
        fatigue_evidence='synthetic static-only scope; real duty history remains required',
    )
    transit = StabilityInput(
        basis=replace(basis, configuration='synthetic-transit-v1',
                      uncertainty='exact synthetic hydrostatic table; real weight and opening uncertainty unresolved',
                      applicability='symmetric intact hydrostatics, raised and secured legs; no damage claim'),
        table=(HydrostaticPoint(0, 0), HydrostaticPoint(30, 0.6), HydrostaticPoint(60, 0)),
        gm_solid_m=1.2, free_surface_correction_m=0.2, downflooding_deg=50, legs_raised=True,
        criteria=StabilityCriteria(0.8, 40, ((0, 30, 0.1),), ((0, 30, 30, 50, 1),)),
        loading_state='synthetic raised-leg transit', displacement_kn=6000, draft_m=1.7, trim_deg=0,
        leg_position_evidence='raised legs assumed secured; their mass included in synthetic GZ table',
        geometry_evidence='synthetic watertight hull; assumed opening angle',
        weight_evidence='synthetic lightship, tank and leg weights; no weight survey',
        heeling_evidence='zero heeling actions assumed for arithmetic demonstration',
        damage_evidence='intact-only synthetic scope; applicability to transit unresolved',
    )
    return legs, plan, storm, (bending,) * 3, transit


def run_example() -> dict[str, Screen]:
    """Exercise headings and guide stiffness without omitting open requirements."""
    legs, plan, storm, templates, transit = example_inputs()
    results = {}
    for stiffness in (0, 1000000):
        for heading in (0, 90, 180, 270):
            name = f'heading_{heading}_stiffness_{stiffness}'
            results[name] = screen_liftboat(
                preloads=legs, preload_plan=plan,
                storm=replace(storm, heading_deg=heading, case=name),
                bending_templates=tuple(replace(t, guide_stiffness_knm_rad=stiffness) for t in templates),
                transit=transit,
                interface_evidence='synthetic ballast removed before legs raised; same 6,000 kN total transit and storm weight',
                transit_weight_change_kn=0, interface_weight_tolerance_kn=0.1,
                adjacent_evidence={},
            )
    return results


def summary(results: dict[str, Screen]) -> dict:
    """Round display values only; complete calculation results retain precision."""
    output = {}
    for name, result in results.items():
        storm = result.metrics['storm']
        governing = result.metrics['governing_check']
        output[name] = {
            'status': result.status,
            'preload_status': [p.status for p in result.metrics['preloads']],
            'storm_status': storm.status,
            'bending_status': [b.status for b in result.metrics['bending']],
            'transit_status': result.metrics['transit'].status,
            'base_shear_kn': round(storm.metrics['base_shear_kn'], 1),
            'reactions_kn': [round(r, 1) for r in storm.metrics['reactions_kn']],
            'governing_check': governing.name,
            'governing_utilization_percent': round(100 * governing.utilization, 1),
            'unresolved_checks': [c.name for c in result.checks if c.status == 'unresolved'],
        }
    return output


if __name__ == '__main__':
    print(json.dumps(summary(run_example()), indent=2, allow_nan=False))
