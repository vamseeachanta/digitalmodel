# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run the manufactured H1 basis in docs/domains/risers/lift-riser-analysis.md.

Run from the repository root with PYTHONPATH=src python
examples/mining_lift_riser/analysis.py. Every number below is an explicit H1
assumption or a mesh control, not a physical material property or default.
"""

import json

from digitalmodel.subsea.lift_design_basis import (
    BASIS_FIELDS,
    CASE_FIELDS,
    LiftDesignBasis,
    LoadCase,
    Source,
    SourcedQuantity,
    screen_case,
)
from digitalmodel.subsea.lift_flow_screening import (
    Calibration,
    Interval,
    airlift_energy_screen,
    deposition_screen,
    settling_screen,
    slugging_screen,
    vertical_transport_screen,
)
from digitalmodel.subsea.mining_riser import axial_modes


# F1 supports only the equation. The artificial inputs are authored in H1;
# a point envelope here is an arithmetic fixture, not a physical calibration.
H1 = Source(
    "https://eng.libretexts.org/@go/page/29202",
    "Synthetic basis H1, companion lift-riser analysis guide",
    "Manufactured arithmetic only; not qualified physical inputs",
    True,
)


def example_basis() -> LiftDesignBasis:
    """Inputs: a fully immersed artificial tube and two explicit snapshots."""
    values = dict(
        water_depth_m=2.0,
        gravity_m_s2=10.0,
        external_density_kg_m3=1.0,
        outer_diameter_m=2.0,
        wall_thickness_m=0.5,
        material_density_kg_m3=8.0,
        youngs_modulus_pa=800.0,
        contents_inertia_fraction=1.0,
        additional_mass_kg_m=0.0,
        additional_displaced_area_m2=0.0,
        axial_added_mass_kg_m=0.0,
    )
    operating = dict(
        liquid_density_kg_m3=1.0,
        solids_density_kg_m3=5.0,
        solids_volume_fraction=0.25,
        mixture_velocity_m_s=2.0,
        significant_wave_height_m=1.0,
        peak_wave_period_s=5.0,
        current_speed_m_s=1.0,
        top_heave_amplitude_m=0.5,
        top_heave_period_s=5.0,
        lower_end_force_n=0.0,
        lower_end_mass_kg=0.0,
        pump_differential_pressure_pa=10.0,
    )
    survival = operating | {"mixture_velocity_m_s": 0.0}
    cases = tuple(
        LoadCase(
            kind,
            kind,
            {k: SourcedQuantity(v, CASE_FIELDS[k].unit, H1) for k, v in case.items()},
        )
        for kind, case in (("operating", operating), ("survival", survival))
    )
    return LiftDesignBasis(
        {k: SourcedQuantity(v, BASIS_FIELDS[k].unit, H1) for k, v in values.items()},
        cases,
        SourcedQuantity(350.0, "N", H1),
    )


def point_calibration(model, values, **derived):
    return Calibration(
        model, H1, {k: Interval(v, v) for k, v in (values | derived).items()}
    )


def run_example() -> dict:
    """Method: density inventory, gravity equilibrium, rod modes and F1-F4."""
    basis = example_basis()
    result = screen_case(basis, "operating")
    # H1 mesh target: 32 elements, first mode; validate convergence separately.
    modes = axial_modes(
        [basis.section("operating")],
        gravity_m_s2=10.0,
        lower_end_force_n=0.0,
        lower_end_mass_kg=0.0,
        elements_per_section=32,
        mode_count=1,
    )
    settling_inputs = dict(
        particle_diameter_m=0.25,
        solids_density_kg_m3=10.0,
        liquid_density_kg_m3=2.0,
        dynamic_viscosity_pa_s=0.5,
        gravity_m_s2=6.0,
        drag_coefficient=2.0,
    )
    settling = settling_screen(
        **settling_inputs,
        calibration=point_calibration(
            "isolated_sphere", settling_inputs, particle_reynolds=2.0
        ),
    )
    deposition_inputs = {
        k: v for k, v in settling_inputs.items() if k != "drag_coefficient"
    } | dict(
        pipe_id_m=2.0,
        solids_volume_fraction=0.25,
        deposition_factor=0.5,
    )
    deposition = deposition_screen(
        **deposition_inputs,
        calibration=point_calibration("horizontal_deposition", deposition_inputs),
    )
    vertical_inputs = dict(
        slip_velocity_m_s=2.0,
        solids_volume_fraction=0.25,
        required_solids_velocity_m_s=1.0,
    )
    vertical = vertical_transport_screen(
        **vertical_inputs,
        calibration=point_calibration("vertical_slip", vertical_inputs),
    )
    airlift_inputs = dict(
        injection_pressure_abs_pa=8.0,
        outlet_pressure_abs_pa=2.0,
        gas_flow_at_outlet_m3_s=3.0,
        liquid_density_kg_m3=2.0,
        gravity_m_s2=6.0,
        lift_above_surface_m=4.0,
        lift_efficiency=0.5,
    )
    airlift = airlift_energy_screen(
        **airlift_inputs,
        calibration=point_calibration("isothermal_energy", airlift_inputs),
    )
    return {
        "top_tension_n": result.static.effective_tension_n[0],
        "dry_weight_n": result.weights.dry_weight_n,
        "contents_weight_n": result.weights.contents_weight_n,
        "buoyancy_n": result.weights.buoyancy_n,
        "reference_extension_m": result.static.extension_m,
        "tension_status": result.tension_status,
        "first_axial_period_s": modes.periods_s[0],
        "settling_velocity_m_s": settling.value,
        "horizontal_deposition_velocity_m_s": deposition.value,
        "vertical_transport_flux_m_s": vertical.value,
        "isothermal_gas_power_w": airlift.gas_power_w,
        "energy_limited_liquid_flow_m3_s": airlift.liquid_flow_m3_s,
        "slugging_status": slugging_screen().status,
    }


if __name__ == "__main__":
    answers = run_example()
    # Presentation only: one decimal place, without changing calculation values.
    print(
        json.dumps(
            {k: round(v, 1) if isinstance(v, float) else v for k, v in answers.items()},
            indent=2,
            allow_nan=False,
        )
    )
