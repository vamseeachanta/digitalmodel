# ABOUTME: Visualization subpackage for dynacard diagnostics.
# ABOUTME: SVG schematics, diagnostic galleries, and annotated card renderers.

from .card_renderer import (
    CardRenderer,
    generate_card,
    render_all_individual_cards,
    render_single_card,
)
from .diagnostic_annotator import (
    DiagnosticAnnotator,
    generate_sample_diagnostic,
)
from .gallery_renderer import (
    GalleryRenderer,
    generate_gallery,
)
from .rod_pump_schematic import (
    RodPumpSchematicGenerator,
    generate_rod_pump_schematic,
)
from .svg_primitives import COLORS, MODE_LABELS, MODE_TIERS, MODES_BY_TIER, TIER_INFO

__all__ = [
    # Primitives
    "COLORS",
    "MODE_LABELS",
    "MODE_TIERS",
    "MODES_BY_TIER",
    "TIER_INFO",
    # Card rendering
    "CardRenderer",
    "generate_card",
    "render_single_card",
    "render_all_individual_cards",
    # Gallery
    "GalleryRenderer",
    "generate_gallery",
    # Rod pump schematic
    "RodPumpSchematicGenerator",
    "generate_rod_pump_schematic",
    # Diagnostic annotator
    "DiagnosticAnnotator",
    "generate_sample_diagnostic",
]
