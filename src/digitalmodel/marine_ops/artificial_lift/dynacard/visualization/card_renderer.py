# ABOUTME: Renders a single dynamometer card as standalone SVG or embeddable fragment.
# ABOUTME: Supports all 18 failure modes with auto-scaled axes.

from __future__ import annotations

from pathlib import Path

from ..card_generators import ALL_GENERATORS
from ..models import CardData
from .svg_primitives import (
    COLORS,
    MODE_LABELS,
    MODE_TIERS,
    TIER_INFO,
    CoordMapper,
    draw_card_axes,
    svg_footer,
    svg_group,
    svg_group_end,
    svg_header,
    svg_polyline,
    svg_rect,
    svg_text,
)


class CardRenderer:
    """Renders a single dynamometer card as SVG.

    Supports two modes:
    - render(): standalone SVG document with axes, title, and frame
    - render_compact(): SVG <g> fragment for embedding in a gallery grid
    """

    def __init__(self, width: int = 280, height: int = 200, margin: int = 45) -> None:
        self.width = width
        self.height = height
        self.margin = margin

    def render(
        self,
        card: CardData,
        mode_name: str,
        title: str | None = None,
    ) -> str:
        """Render a standalone SVG document for a single card."""
        title = title or MODE_LABELS.get(mode_name, mode_name)
        tier = MODE_TIERS.get(mode_name, 1)
        tier_color = TIER_INFO[tier]["color"]

        pos = card.position
        load = card.load

        pos_min, pos_max = min(pos), max(pos)
        load_min, load_max = min(load), max(load)

        # Add 5% padding to ranges
        pos_pad = max((pos_max - pos_min) * 0.05, 1.0)
        load_pad = max((load_max - load_min) * 0.05, 100.0)

        mapper = CoordMapper(
            pos_min - pos_pad, pos_max + pos_pad,
            load_min - load_pad, load_max + load_pad,
            float(self.margin), float(self.width - 15),
            float(35), float(self.height - self.margin),
        )

        svg = svg_header(self.width, self.height)

        # Title with tier-colored underline
        svg += svg_text(
            self.width / 2, 18, title,
            color=tier_color, size=13, weight="bold",
        )
        svg += svg_text(
            self.width / 2, 30, f"[{TIER_INFO[tier]['label']}]",
            color=tier_color, size=8,
        )

        # Axes and grid
        svg += draw_card_axes(mapper)

        # Card trace
        points = [(mapper.x(p), mapper.y(l)) for p, l in zip(pos, load)]
        svg += svg_polyline(points, COLORS["card_trace"], width=2.0, close=True)

        svg += svg_footer()
        return svg

    def render_compact(
        self,
        card: CardData,
        mode_name: str,
        cell_width: float = 280,
        cell_height: float = 200,
    ) -> str:
        """Render a compact SVG <g> fragment for gallery embedding.

        Returns an SVG group (not a full document) sized to fit within
        the given cell dimensions.
        """
        title = MODE_LABELS.get(mode_name, mode_name)
        tier = MODE_TIERS.get(mode_name, 1)
        tier_color = TIER_INFO[tier]["color"]

        pos = card.position
        load = card.load

        pos_min, pos_max = min(pos), max(pos)
        load_min, load_max = min(load), max(load)

        pos_pad = max((pos_max - pos_min) * 0.05, 1.0)
        load_pad = max((load_max - load_min) * 0.05, 100.0)

        # Compact margins
        left_margin = 35
        right_margin = 8
        top_margin = 30
        bottom_margin = 20

        mapper = CoordMapper(
            pos_min - pos_pad, pos_max + pos_pad,
            load_min - load_pad, load_max + load_pad,
            float(left_margin), cell_width - right_margin,
            float(top_margin), cell_height - bottom_margin,
        )

        svg = ""

        # Card background
        svg += svg_rect(0, 0, cell_width, cell_height, COLORS["frame"], fill="#fafafa", rx=4)

        # Title
        svg += svg_text(
            cell_width / 2, 16, title,
            color=tier_color, size=11, weight="bold",
        )

        # Axes (simplified for compact view)
        svg += svg_rect(
            mapper.sx_min, mapper.sy_min,
            mapper.sx_max - mapper.sx_min,
            mapper.sy_max - mapper.sy_min,
            COLORS["grid"],
        )

        # Card trace
        points = [(mapper.x(p), mapper.y(l)) for p, l in zip(pos, load)]
        svg += svg_polyline(points, COLORS["card_trace"], width=1.5, close=True)

        return svg


def generate_card(mode_name: str, seed: int = 42) -> CardData:
    """Generate a synthetic card for the given failure mode."""
    gen = ALL_GENERATORS.get(mode_name)
    if gen is None:
        raise ValueError(f"Unknown failure mode: {mode_name}")
    return gen(seed=seed)


def render_single_card(mode_name: str, output_path: Path, seed: int = 42) -> Path:
    """Generate and save a single card SVG."""
    card = generate_card(mode_name, seed=seed)
    renderer = CardRenderer()
    svg_content = renderer.render(card, mode_name)
    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(svg_content)
    return output_path


def render_all_individual_cards(output_dir: Path, seed: int = 42) -> list[Path]:
    """Generate and save individual SVGs for all 18 failure modes."""
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    renderer = CardRenderer()
    paths = []
    for mode_name in ALL_GENERATORS:
        card = generate_card(mode_name, seed=seed)
        filename = mode_name.lower() + ".svg"
        filepath = output_dir / filename
        svg_content = renderer.render(card, mode_name)
        filepath.write_text(svg_content)
        paths.append(filepath)
    return paths


__all__ = [
    "CardRenderer",
    "generate_card",
    "render_single_card",
    "render_all_individual_cards",
]
