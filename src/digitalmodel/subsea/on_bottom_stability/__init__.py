"""On-bottom stability design for submarine pipelines.

Implements pipeline on-bottom stability checks per DNV-RP-F109 (2021),
including hydrodynamic loading, submerged weight, and lateral stability
assessment (absolute and generalized methods).
"""
from __future__ import annotations

from digitalmodel.subsea.on_bottom_stability.dnv_rp_f109 import (
    C_D_ROUGH,
    C_D_SMOOTH,
    C_L_ROUGH,
    C_L_SMOOTH,
    C_M_ROUGH,
    C_M_SMOOTH,
    GAMMA_SC_NORMAL,
    StabilityResult,
    absolute_stability_check,
    generalized_stability_check,
    hydrodynamic_force_per_meter,
    lift_force_per_meter,
    submerged_weight_per_meter,
)

__all__ = [
    "C_D_ROUGH",
    "C_D_SMOOTH",
    "C_L_ROUGH",
    "C_L_SMOOTH",
    "C_M_ROUGH",
    "C_M_SMOOTH",
    "GAMMA_SC_NORMAL",
    "StabilityResult",
    "absolute_stability_check",
    "generalized_stability_check",
    "hydrodynamic_force_per_meter",
    "lift_force_per_meter",
    "submerged_weight_per_meter",
]
