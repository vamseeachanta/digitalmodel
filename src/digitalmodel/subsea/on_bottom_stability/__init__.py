# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""On-bottom stability static checks and compatibility wrappers.

The canonical F109 surface implements clause 3.6.2 Eqs. (3.38)-(3.39)
with explicit factors, eligible initial-state resistance and peak demands.
Generalised displacement and dynamic methods remain OPEN; the historical
``generalized_stability_check`` name is a deprecated static alias.

Package-level ``lift_force_per_meter`` and ``StabilityResult`` retain the
simplified signatures. Import ``dnv_rp_f109`` for the canonical signatures.
"""
from __future__ import annotations

# Canonical surface — DNV-RP-F109 clause-referenced
from digitalmodel.subsea.on_bottom_stability.dnv_rp_f109 import (
    C_D_ROUGH,
    C_D_SMOOTH,
    C_L_ROUGH,
    C_L_SMOOTH,
    C_M_ROUGH,
    C_M_SMOOTH,
    GAMMA_SC_NORMAL,
    AbsolutePeakLoads,
    AbsoluteStabilityMargin,
    absolute_peak_loads,
    diameter_mean_current,
    load_reduction_factor,
    low_kc_horizontal_coefficient,
    transverse_current_at_height,
    absolute_stability_check,
    absolute_stability_margin,
    generalized_stability_check,
    hydrodynamic_force_per_meter,
    submerged_weight_per_meter,
)

# Simplified surface — kN wrappers, combined hydrodynamic loads,
# vertical stability. Ported from former
# digitalmodel.geotechnical.on_bottom_stability (workspace-hub#2694).
from digitalmodel.subsea.on_bottom_stability.simplified import (
    DEFAULT_CL,
    G,
    STANDARD,
    HydrodynamicLoadResult,
    StabilityResult,
    SubmergedWeightResult,
    VerticalStabilityResult,
    check_lateral_stability,
    check_vertical_stability,
    drag_force_per_meter,
    hydrodynamic_loads,
    inertia_force_per_meter,
    lateral_stability_check,
    lift_force_per_meter,
    submerged_weight,
)

__all__ = [
    # Canonical (dnv_rp_f109)
    "C_D_ROUGH",
    "C_D_SMOOTH",
    "C_L_ROUGH",
    "C_L_SMOOTH",
    "C_M_ROUGH",
    "C_M_SMOOTH",
    "GAMMA_SC_NORMAL",
    "AbsolutePeakLoads",
    "AbsoluteStabilityMargin",
    "absolute_peak_loads",
    "diameter_mean_current",
    "load_reduction_factor",
    "low_kc_horizontal_coefficient",
    "transverse_current_at_height",
    "absolute_stability_check",
    "absolute_stability_margin",
    "generalized_stability_check",
    "hydrodynamic_force_per_meter",
    "submerged_weight_per_meter",
    # Simplified (kN + combined surface, ported from geotechnical)
    "DEFAULT_CL",
    "G",
    "STANDARD",
    "HydrodynamicLoadResult",
    "StabilityResult",
    "SubmergedWeightResult",
    "VerticalStabilityResult",
    "check_lateral_stability",
    "check_vertical_stability",
    "drag_force_per_meter",
    "hydrodynamic_loads",
    "inertia_force_per_meter",
    "lateral_stability_check",
    "lift_force_per_meter",
    "submerged_weight",
]
