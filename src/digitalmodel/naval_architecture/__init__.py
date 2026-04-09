# ABOUTME: Naval architecture package — stability, resistance, seakeeping, compliance
# ABOUTME: TDD-driven from USNA EN400, PNA, IMO IS Code worked examples

from digitalmodel.naval_architecture.gyradius import (
    gyradius_for_fleet_vessel,
)
from digitalmodel.naval_architecture.hull_properties import (
    capacity_plan,
    hull_hydrostatics,
    hull_weight_groups,
    stability_curve_estimate,
)
from digitalmodel.naval_architecture.ship_data import (
    estimate_vessel_hydrostatics,
    get_cross_curves,
    get_curves_of_form,
    get_ship,
    list_ships,
    normalize_drilling_rig_record,
    normalize_fleet_record,
    register_fleet_vessels,
)
from digitalmodel.naval_architecture.ship_dimensions import (
    default_dimension_template_path,
    load_dimension_template,
    merge_template_into_registry,
    validate_vessel_entry,
)

__all__ = [
    "capacity_plan",
    "default_dimension_template_path",
    "estimate_vessel_hydrostatics",
    "get_cross_curves",
    "get_curves_of_form",
    "get_ship",
    "gyradius_for_fleet_vessel",
    "hull_hydrostatics",
    "hull_weight_groups",
    "list_ships",
    "load_dimension_template",
    "merge_template_into_registry",
    "normalize_drilling_rig_record",
    "normalize_fleet_record",
    "register_fleet_vessels",
    "stability_curve_estimate",
    "validate_vessel_entry",
]
