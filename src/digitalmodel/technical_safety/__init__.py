"""Cited offshore screening: ship-impact energy and fire heat-flux selection.

See docs/domains/fire_safety/technical-safety-screens.html for worked examples
and the required distinction between test criteria and design inputs.
"""

from .accidental_loads import ShipImpactInputs, ShipImpactResult, screen_ship_impact
from .criteria import CitedValue, EditionCriteria, get_criteria
from .fire_heat_load import (
    FireHeatLoadResult,
    FireScenario,
    FireWaterEvidence,
    screen_fire_heat_load,
    select_fire_types,
)

__all__ = [
    "ShipImpactInputs",
    "ShipImpactResult",
    "screen_ship_impact",
    "CitedValue",
    "EditionCriteria",
    "get_criteria",
    "FireScenario",
    "FireWaterEvidence",
    "screen_fire_heat_load",
    "select_fire_types",
    "FireHeatLoadResult",
]
