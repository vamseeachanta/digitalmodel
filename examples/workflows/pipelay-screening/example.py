"""Synthetic SI example: run from the repository with PYTHONPATH=src."""

from dataclasses import asdict
import json

from digitalmodel.installation.pipelay.screening import (
    ar_cable_tension,
    displacement_controlled_check,
    lay_geometry,
    pressure_screen,
)


def worked_example() -> dict:
    """Illustrative inputs only; no measured data or installation clearance."""
    s_lay = lay_geometry(
        method="S-Lay", outer_diameter_m=0.5, suspended_height_m=100,
        submerged_weight_n_per_m=1000, horizontal_tension_n=200000,
        stinger_radius_m=250,
    )
    j_lay = lay_geometry(
        method="J-Lay", outer_diameter_m=0.5, suspended_height_m=1000,
        submerged_weight_n_per_m=1000, horizontal_tension_n=50000,
    )
    pressure = pressure_screen(
        outer_diameter_m=0.5, wall_thickness_m=0.02, yield_strength_pa=400e6,
        youngs_modulus_pa=200e9, poisson_ratio=0.3, fabrication_factor=1,
        ovality=0.005, external_pressure_pa=2e6, minimum_internal_pressure_pa=0,
        gamma_m=1.15, gamma_sc=1.1,
    )
    # Separate hypothetical section with established displacement control.
    # .01 capacity, .8 exponent and all factors are illustrative design inputs.
    # No displacement-control classification is inferred for the free sagbend.
    interaction = displacement_controlled_check(
        design_compressive_strain=0.0015, characteristic_strain_capacity=0.01,
        gamma_e=2, net_external_pressure_pa=pressure.net_external_pressure_pa,
        collapse_pressure_pa=pressure.collapse_pa, gamma_m=1.15, gamma_sc=1.1,
        interaction_exponent=0.8, displacement_controlled=True,
    )
    # Separate A&R-stage forces, not forces transferred from the lay geometry.
    cable = ar_cable_tension(
        horizontal_end_force_n=30000, downward_end_force_n=40000,
        suspended_cable_length_m=100, cable_submerged_weight_n_per_m=100,
        submerged_head_weight_n=0, dynamic_factor=1.2,
    )
    return {
        "input_readiness": "synthetic illustration only",
        "s_lay": asdict(s_lay), "j_lay": asdict(j_lay),
        "pressure": asdict(pressure),
        "conditional_displacement_interaction": asdict(interaction),
        "ar_cable": asdict(cable),
        "native_comparison": "NOT_EVALUATED",
    }


if __name__ == "__main__":
    print(json.dumps(worked_example(), indent=2, allow_nan=False))
