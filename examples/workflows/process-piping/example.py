"""Illustrative NPS 6 Class 300 joint and NPS 4 Sch 40 water line.

Input record version 1, synthetic engineering demonstration (2026-11-02).
No plant data or qualified design package is represented. No table of
material allowables, flange ratings or gasket factors is embedded.

Dimension spot checks: NPS 6 OD 6.625in, Sch40 .280in, NPS4 Sch40
.237in wall, existing B36.10M catalog and Wheatland A53 submittal chart:
https://www.wheatland.com/wp-content/uploads/2017/12/ASTM-A53-Schedule-40-Submittal-Sheet.pdf
NPS6 Class300: twelve 3/4in fasteners, ASME B16.5 bolting dimensions:
https://apiint.com/flange-dimensions/bolt-stud-torque-charts/ansi-asme-b16-5-bolt-chart/
All stress/allowance/friction/gasket/flange inputs below are illustrative
case assumptions, not sourced material properties or product limits.
Methods and source clauses are in the imported calculation docstrings.
"""

from dataclasses import asdict, replace
import json

from digitalmodel.materials.line_pipe import od_for_nps, wt_for_schedule
from digitalmodel.process_piping import (
    PressureDesign,
    pressure_thickness,
    hydrotest_pressure,
    BoltUp,
    check_bolt_up,
    tightening_passes,
    joint_leak_checks,
    Flow,
    evaluate_line,
    size_line,
    service_velocity_limit,
)


def run_example() -> dict:
    """Return deterministic results for explicitly assumed inputs (SI)."""
    design = PressureDesign(
        pressure_pa=4e6,
        outside_diameter_m=od_for_nps(6) * 0.0254,
        allowable_stress_pa=138e6,
        quality_factor=1,
        weld_strength_factor=1,
        y=0.4,
        corrosion_allowance_m=0.003,
        mechanical_allowance_m=0,
        mill_tolerance_fraction=0.125,
    )
    thickness = pressure_thickness(design)
    wall = wt_for_schedule(6, "40") * 0.0254
    test = hydrotest_pressure(
        design_pressure_pa=design.pressure_pa,
        design_allowable_pa=138e6,
        test_allowable_pa=150e6,
        design_temperature_k=423.15,
        test_temperature_k=293.15,
        outside_diameter_m=design.outside_diameter_m,
        minimum_test_wall_m=wall * (1 - design.mill_tolerance_fraction),
        test_yield_pa=240e6,
        other_longitudinal_stress_pa=0,
        component_test_limit_pa=7.5e6,
    )
    joint = BoltUp(
        facing="rf_spiral_wound",
        bolt_count=12,
        bolt_area_m2=0.00021580602,
        bolt_diameter_m=0.01905,
        target_bolt_stress_pa=300e6,
        maximum_bolt_stress_pa=450e6,
        minimum_bolt_stress_pa=100e6,
        nut_factor=0.18,
        gasket_contact_area_m2=0.008,
        gasket_seating_min_pa=80e6,
        gasket_operating_min_pa=30e6,
        gasket_max_pa=200e6,
        retained_load_fraction=0.8,
        flange_load_limit_n=1.5e6,
        pressure_pa=4e6,
        gasket_inside_diameter_m=0.17,
        effective_gasket_width_m=0.006,
        gasket_reaction_diameter_m=0.19,
        gasket_m=3,
        gasket_y_pa=69e6,
    )
    water = Flow(
        phase="liquid",
        flow_m3_s=50 / 3600,
        density_kg_m3=998.2,
        viscosity_pa_s=0.001002,
        length_m=100,
        roughness_m=0.000045,
    )
    water_line = evaluate_line(water, nps=4, schedule="40", velocity_limit_m_s=2)
    picked = size_line(water, velocity_limit_m_s=2, maximum_pressure_drop_pa=30000)
    gas = Flow(
        phase="gas",
        flow_m3_s=0.05,
        density_kg_m3=10,
        viscosity_pa_s=1.1e-5,
        length_m=4000,
        roughness_m=0.000045,
        inlet_pressure_pa_abs=1e6,
    )
    mixture = Flow(
        phase="two_phase",
        flow_m3_s=0.01,
        density_kg_m3=100,
        viscosity_pa_s=None,
        length_m=100,
        roughness_m=0,
        erosional_c_us=100,
        erosional_c_basis="API RP 14E 1991 2.5(a); continuous, sand-free, noncorrosive assumption",
    )
    return {
        "input_version": 1,
        "input_status": "synthetic demonstration; unqualified material and joint assumptions",
        "thickness": asdict(thickness),
        "nps6_schedule40_nominal_wall_m": wall,
        "wall_meets_pressure_only_requirement": wall
        >= thickness.minimum_nominal_thickness_m,
        "hydrotest": asdict(test),
        "bolt_up_design": asdict(check_bolt_up(joint)),
        "bolt_up_test": asdict(
            check_bolt_up(replace(joint, pressure_pa=test.target_pressure_pa))
        ),
        "passes": [asdict(p) for p in tightening_passes(12)],
        "leak_diagnostic_priorities": joint_leak_checks(),
        "water_nps4": asdict(water_line),
        "selected_water_line": asdict(picked),
        "general_water_velocity_guidance": asdict(
            service_velocity_limit("general_water")
        ),
        "gas_nps4": asdict(
            evaluate_line(gas, nps=4, schedule="40", velocity_limit_m_s=40)
        ),
        "mixture_nps4": asdict(
            evaluate_line(mixture, nps=4, schedule="40", velocity_limit_m_s=10)
        ),
    }


if __name__ == "__main__":
    print(json.dumps(run_example(), indent=2, allow_nan=False))
