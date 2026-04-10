"""Drilling riser calculations — stackup, operability, tool passage, damping."""

from digitalmodel.drilling_riser.damping import (
    modal_damping_equivalent,
    rayleigh_damping_coefficients,
    structural_damping_ratio,
)
from digitalmodel.drilling_riser.operability import (
    operability_fraction,
    significant_wave_height_limit,
    watch_circle_radius_m,
)
from digitalmodel.drilling_riser.stackup import (
    effective_tension,
    minimum_slip_ring_tension,
    top_tension_required,
    wall_thickness_required,
)
from digitalmodel.drilling_riser.adapter import (
    compute_riser_string_weight_kn,
    normalize_riser_component_record,
    register_riser_components,
)
from digitalmodel.drilling_riser.tool_passage import (
    annular_clearance_mm,
    minimum_riser_id_required,
    spacing_requirement_m,
)

__all__ = [
    "top_tension_required",
    "wall_thickness_required",
    "effective_tension",
    "minimum_slip_ring_tension",
    "significant_wave_height_limit",
    "operability_fraction",
    "watch_circle_radius_m",
    "annular_clearance_mm",
    "minimum_riser_id_required",
    "spacing_requirement_m",
    "structural_damping_ratio",
    "rayleigh_damping_coefficients",
    "modal_damping_equivalent",
    "normalize_riser_component_record",
    "register_riser_components",
    "compute_riser_string_weight_kn",
]
