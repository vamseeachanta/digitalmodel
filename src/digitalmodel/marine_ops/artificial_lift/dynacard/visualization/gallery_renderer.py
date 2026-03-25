# ABOUTME: Renders the 18-card diagnostic gallery as a single SVG grid.
# ABOUTME: Organized by tier with colored header bands, 3 columns per row.

from __future__ import annotations

from pathlib import Path

from ..card_generators import ALL_GENERATORS
from .card_renderer import CardRenderer, generate_card
from .svg_primitives import (
    COLORS,
    MODES_BY_TIER,
    TIER_INFO,
    svg_footer,
    svg_group,
    svg_group_end,
    svg_header,
    svg_rect,
    svg_text,
)

# Gallery layout constants
COLUMNS = 3
CELL_WIDTH = 290
CELL_HEIGHT = 210
CELL_PAD = 10
HEADER_HEIGHT = 36
GALLERY_MARGIN = 15


class GalleryRenderer:
    """Renders the complete 18-card diagnostic gallery as a single SVG.

    Layout:
    - 3 columns per row
    - Full-width colored tier header bands between groups
    - Each card auto-scaled to fill its cell
    """

    def __init__(self, seed: int = 42) -> None:
        self.seed = seed
        self.renderer = CardRenderer()

    def _calculate_dimensions(self) -> tuple[int, int]:
        """Calculate total SVG dimensions based on tier layout."""
        total_width = GALLERY_MARGIN * 2 + COLUMNS * (CELL_WIDTH + CELL_PAD) - CELL_PAD
        total_height = GALLERY_MARGIN

        for tier_num in [1, 2, 3]:
            modes = MODES_BY_TIER[tier_num]
            n_rows = (len(modes) + COLUMNS - 1) // COLUMNS
            # Header + rows for this tier
            total_height += HEADER_HEIGHT + n_rows * (CELL_HEIGHT + CELL_PAD)

        total_height += GALLERY_MARGIN
        return int(total_width), int(total_height)

    def render(self) -> str:
        """Render the complete gallery SVG."""
        width, height = self._calculate_dimensions()
        svg = svg_header(width, height)

        # Title
        svg += svg_text(
            width / 2, GALLERY_MARGIN + 2,
            "DynaCard AI — 18 Failure Mode Diagnostic Gallery",
            color=COLORS["text"], size=16, weight="bold",
        )

        y_offset = GALLERY_MARGIN + 20

        for tier_num in [1, 2, 3]:
            tier = TIER_INFO[tier_num]
            modes = MODES_BY_TIER[tier_num]

            # Tier header band
            svg += svg_rect(
                GALLERY_MARGIN, y_offset,
                width - 2 * GALLERY_MARGIN, HEADER_HEIGHT,
                "none", fill=tier["color"], rx=4,
            )
            svg += svg_text(
                width / 2, y_offset + HEADER_HEIGHT * 0.65,
                tier["label"],
                color="#ffffff", size=14, weight="bold",
            )
            y_offset += HEADER_HEIGHT + CELL_PAD

            # Render cards in rows of COLUMNS
            for i, mode_name in enumerate(modes):
                col = i % COLUMNS
                row = i // COLUMNS
                x = GALLERY_MARGIN + col * (CELL_WIDTH + CELL_PAD)
                y = y_offset + row * (CELL_HEIGHT + CELL_PAD)

                card = generate_card(mode_name, seed=self.seed)
                svg += svg_group(x, y)
                svg += self.renderer.render_compact(
                    card, mode_name,
                    cell_width=CELL_WIDTH, cell_height=CELL_HEIGHT,
                )
                svg += svg_group_end()

            n_rows = (len(modes) + COLUMNS - 1) // COLUMNS
            y_offset += n_rows * (CELL_HEIGHT + CELL_PAD)

        svg += svg_footer()
        return svg


def generate_gallery(output_path: Path, seed: int = 42) -> Path:
    """Generate and save the diagnostic gallery SVG."""
    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    gallery = GalleryRenderer(seed=seed)
    svg_content = gallery.render()
    output_path.write_text(svg_content)
    return output_path


__all__ = [
    "GalleryRenderer",
    "generate_gallery",
]
