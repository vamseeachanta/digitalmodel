"""On-bottom stability design for submarine pipelines.

Implements pipeline on-bottom stability checks per DNV-RP-F109 (2021),
including hydrodynamic loading, submerged weight, and lateral stability
assessment (absolute and generalized methods).

This package exposes two complementary surfaces:

- ``dnv_rp_f109`` submodule — the canonical, clause-referenced API with
  separate ``absolute_stability_check`` (§4.3.1 Eq 4.1) and
  ``generalized_stability_check`` (§4.3.2 Eq 4.5). NamedTuple results
  named ``utilisation`` (UK spelling).
- ``simplified`` submodule — kN-wrapper and combined-component surface
  ported from the former ``digitalmodel.geotechnical.on_bottom_stability``
  module (workspace-hub#2694 unification). Dataclass results named
  ``utilization`` (US spelling) with ``utilisation`` property alias.
  Routes to ``generalized_stability_check(F_R=0)`` for the lateral check.

The package-level re-exports below favour the **simplified** surface for
the symbol names that overlap (``lift_force_per_meter``,
``StabilityResult``). Import directly from ``.dnv_rp_f109`` to access the
canonical signatures.
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
    absolute_stability_check,
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
    "absolute_stability_check",
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
