"""Unbonded flexible pipe modelling (API 17B / API 17J / ISO 13628-2).

Deterministic, native-Python cross-section and design-check core for
unbonded flexible dynamic risers / flowlines:

* ``cross_section`` — layered cross-section builder (mass/EA/EI per length).
* ``design_checks_17J`` — API 17J tensile-armour utilisation + MBR checks.
* ``bend_stiffener`` — bend-stiffener MBR screening (developed radius).

Deferred (follow-on): full API 17B rainflow + tensile-armour S-N fatigue,
detailed tapered bend-stiffener taper design, carcass collapse / pressure
armour capacity, and catenary/lazy-wave solver integration.
"""

from .cross_section import (
    GRAVITY,
    WATER_DENSITY,
    FlexiblePipeSection,
    Layer,
    LayerType,
)
from .design_checks_17J import (
    UTILISATION_FACTORS,
    FlexCheckResult,
    LoadCase,
    TensileArmour,
    allowable_wire_stress,
    armour_bending_stress,
    armour_tension_stress,
    verify_flexible_pipe,
)
from .bend_stiffener import (
    BendStiffenerResult,
    developed_min_radius,
    required_effective_ei,
    screen_bend_stiffener,
)

__all__ = [
    # cross_section
    "GRAVITY",
    "WATER_DENSITY",
    "FlexiblePipeSection",
    "Layer",
    "LayerType",
    # design_checks_17J
    "UTILISATION_FACTORS",
    "FlexCheckResult",
    "LoadCase",
    "TensileArmour",
    "allowable_wire_stress",
    "armour_bending_stress",
    "armour_tension_stress",
    "verify_flexible_pipe",
    # bend_stiffener
    "BendStiffenerResult",
    "developed_min_radius",
    "required_effective_ei",
    "screen_bend_stiffener",
]
