# ABOUTME: Drawing element primitives for field development schematics
"""Element primitives: icons, annotations, seabed."""

from .icons import (
    make_fpso_patch,
    make_platform_patch,
    make_template_patch,
    make_well_symbol,
    FPSO_WIDTH,
    FPSO_HEIGHT,
    TEMPLATE_WIDTH,
    TEMPLATE_HEIGHT,
    WELL_RADIUS,
)
from .annotations import (
    add_scale_bar,
    add_depth_label,
    add_field_name_label,
    add_north_arrow,
)
from .seabed import (
    draw_seabed_line,
    compute_water_column_height,
    SEABED_HATCH,
)

__all__ = [
    "make_fpso_patch",
    "make_platform_patch",
    "make_template_patch",
    "make_well_symbol",
    "FPSO_WIDTH",
    "FPSO_HEIGHT",
    "TEMPLATE_WIDTH",
    "TEMPLATE_HEIGHT",
    "WELL_RADIUS",
    "add_scale_bar",
    "add_depth_label",
    "add_field_name_label",
    "add_north_arrow",
    "draw_seabed_line",
    "compute_water_column_height",
    "SEABED_HATCH",
]
