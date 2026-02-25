# ABOUTME: Generates a rod pump system cross-section SVG schematic.
# ABOUTME: Shows surface unit, wellbore, pump components, and a normal dynamometer card.

from __future__ import annotations

import math
from pathlib import Path

from ..card_generators import generate_normal_card
from .card_renderer import CardRenderer
from .svg_primitives import (
    COLORS,
    CoordMapper,
    draw_card_axes,
    svg_arc_path,
    svg_circle,
    svg_footer,
    svg_group,
    svg_group_end,
    svg_header,
    svg_line,
    svg_polyline,
    svg_rect,
    svg_text,
)

WIDTH = 920
HEIGHT = 680
WELL_X = 180  # Center of wellbore
CARD_X = 580  # Left edge of card panel


class RodPumpSchematicGenerator:
    """Generates a rod pump system cross-section SVG.

    Left side (60%): vertical cross-section from surface unit down to reservoir
    Right side (40%): normal dynamometer card with data flow annotation
    """

    def render(self) -> str:
        """Render the complete rod pump schematic SVG."""
        svg = svg_header(WIDTH, HEIGHT)

        # Title
        svg += svg_text(
            WIDTH / 2, 22,
            "Sucker Rod Pump System — Surface Card to Downhole Diagnosis",
            color=COLORS["text"], size=16, weight="bold",
        )

        # Left panel: well schematic
        svg += self._draw_well_schematic()

        # Right panel: dynamometer card
        svg += self._draw_card_panel()

        # Data flow arrow connecting well to card
        svg += self._draw_data_flow()

        svg += svg_footer()
        return svg

    def _draw_well_schematic(self) -> str:
        """Draw the vertical well cross-section."""
        svg = ""

        # Ground line
        svg += svg_line(30, 200, 350, 200, "#8B4513", 2.5)
        svg += svg_text(340, 195, "Ground Level", size=9, anchor="end", color="#8B4513")

        # --- Surface equipment ---

        # Pumping unit base
        svg += svg_rect(100, 185, 160, 15, COLORS["frame"], fill="#d0d0d0")

        # Sampson post (vertical support)
        svg += svg_rect(230, 100, 12, 85, COLORS["frame"], fill="#b0b0b0")

        # Walking beam (horizontal beam)
        svg += svg_line(120, 105, 280, 105, COLORS["text"], 4)

        # Walking beam pivot
        svg += svg_circle(236, 105, 6, COLORS["text"], fill="#888888")

        # Horsehead (left end of beam)
        svg += svg_arc_path(120, 105, 20, 180, 270, COLORS["text"], 3)

        # Polished rod (from horsehead down into well)
        svg += svg_line(120, 125, 120, 200, COLORS["card_trace"], 2)
        svg += svg_text(95, 165, "Polished", size=8, anchor="end", color=COLORS["text"])
        svg += svg_text(95, 175, "Rod", size=8, anchor="end", color=COLORS["text"])

        # Counterweight (right end of beam)
        svg += svg_rect(265, 108, 20, 30, COLORS["frame"], fill="#999999")

        # Crank arm
        svg += svg_line(275, 138, 295, 175, COLORS["text"], 2.5)

        # Motor
        svg += svg_rect(280, 175, 35, 20, COLORS["frame"], fill="#aaaaaa")
        svg += svg_text(298, 189, "Motor", size=7, color=COLORS["text"])

        # Stuffing box at surface
        svg += svg_rect(110, 196, 20, 10, COLORS["frame"], fill="#c0c0c0")
        svg += svg_text(80, 210, "Stuffing Box", size=7, anchor="end", color=COLORS["text"])

        # Pumping unit label
        svg += svg_text(200, 80, "Pumping Unit", size=11, weight="bold", color=COLORS["text"])

        # Surface card measurement indicator
        svg += svg_line(140, 150, 155, 150, COLORS["tier_1"], 1.5, dash="3,2")
        svg += svg_line(155, 140, 155, 160, COLORS["tier_1"], 1.5)
        svg += svg_text(168, 148, "Surface Card", size=7, anchor="start", color=COLORS["tier_1"])
        svg += svg_text(168, 158, "Measurement", size=7, anchor="start", color=COLORS["tier_1"])

        # --- Wellbore ---

        # Casing (outer tube)
        casing_left = WELL_X - 25
        casing_right = WELL_X + 25
        svg += svg_line(casing_left, 206, casing_left, 600, "#8B6914", 2)
        svg += svg_line(casing_right, 206, casing_right, 600, "#8B6914", 2)

        # Tubing (inner tube)
        tubing_left = WELL_X - 10
        tubing_right = WELL_X + 10
        svg += svg_line(tubing_left, 206, tubing_left, 520, COLORS["frame"], 1.5)
        svg += svg_line(tubing_right, 206, tubing_right, 520, COLORS["frame"], 1.5)

        # Rod string (center line, dashed to show length)
        svg += svg_line(WELL_X, 206, WELL_X, 510, COLORS["card_trace"], 1.5, dash="8,4")

        # Rod string label
        svg += svg_text(
            casing_right + 15, 350, "Rod String",
            size=9, anchor="start", color=COLORS["text"],
        )
        svg += svg_text(
            casing_right + 15, 362, "(up to 8000 ft)",
            size=7, anchor="start", color=COLORS["frame"],
        )

        # Tubing label
        svg += svg_text(
            casing_left - 8, 350, "Tubing",
            size=9, anchor="end", color=COLORS["text"],
        )

        # Casing label
        svg += svg_text(
            casing_left - 8, 300, "Casing",
            size=9, anchor="end", color="#8B6914",
        )

        # --- Pump assembly ---

        pump_top = 510
        pump_bottom = 580

        # Traveling valve
        svg += svg_rect(tubing_left, pump_top, 20, 10, COLORS["tier_2"], fill="#e8d8c8", width=1.5)
        svg += svg_text(
            casing_right + 15, pump_top + 8, "Traveling Valve (TV)",
            size=8, anchor="start", color=COLORS["tier_2"],
        )

        # Pump barrel
        svg += svg_rect(tubing_left - 3, pump_top + 10, 26, 40, COLORS["card_trace"], fill="#e8f0f8", width=1.5)
        svg += svg_text(
            casing_right + 15, pump_top + 35, "Pump Barrel",
            size=8, anchor="start", color=COLORS["card_trace"],
        )

        # Plunger (inside barrel)
        svg += svg_rect(tubing_left + 1, pump_top + 12, 18, 15, COLORS["annotation"], fill="#d8c8e8", width=1)

        # Standing valve
        svg += svg_rect(tubing_left, pump_top + 50, 20, 10, COLORS["tier_3"], fill="#e8d8c8", width=1.5)
        svg += svg_text(
            casing_right + 15, pump_top + 58, "Standing Valve (SV)",
            size=8, anchor="start", color=COLORS["tier_3"],
        )

        # --- Perforations and reservoir ---

        perf_y = 590
        for i in range(5):
            y = perf_y + i * 3
            svg += svg_line(casing_left - 8, y, casing_left, y, "#8B6914", 1)
            svg += svg_line(casing_right, y, casing_right + 8, y, "#8B6914", 1)

        svg += svg_text(
            casing_left - 12, perf_y + 8, "Perforations",
            size=8, anchor="end", color="#8B6914",
        )

        # Reservoir zone
        svg += svg_rect(
            casing_left - 30, 610, casing_right - casing_left + 60, 25,
            "none", fill="#f5e6d3", rx=3,
        )
        svg += svg_text(WELL_X, 627, "Reservoir", size=10, color="#8B6914", weight="bold")

        # Fluid flow arrows (upward inside tubing)
        for y_pos in [280, 380, 470]:
            svg += svg_text(WELL_X, y_pos, "▲", size=10, color=COLORS["ideal_card"])

        svg += svg_text(
            casing_left - 8, 475, "Fluid",
            size=7, anchor="end", color=COLORS["ideal_card"],
        )
        svg += svg_text(
            casing_left - 8, 485, "Flow ▲",
            size=7, anchor="end", color=COLORS["ideal_card"],
        )

        return svg

    def _draw_card_panel(self) -> str:
        """Draw the normal dynamometer card on the right side."""
        svg = ""

        # Panel label
        svg += svg_text(
            CARD_X + 160, 55, "Normal Dynamometer Card",
            size=13, weight="bold", color=COLORS["text"],
        )
        svg += svg_text(
            CARD_X + 160, 70, "(Downhole / Pump Card)",
            size=10, color=COLORS["frame"],
        )

        # Generate normal card
        card = generate_normal_card(seed=42)
        pos = card.position
        load = card.load

        pos_min, pos_max = min(pos), max(pos)
        load_min, load_max = min(load), max(load)
        pos_pad = (pos_max - pos_min) * 0.08
        load_pad = (load_max - load_min) * 0.08

        mapper = CoordMapper(
            pos_min - pos_pad, pos_max + pos_pad,
            load_min - load_pad, load_max + load_pad,
            float(CARD_X + 50), float(CARD_X + 310),
            float(90), float(320),
        )

        # Draw axes
        svg += draw_card_axes(mapper, n_x_ticks=4, n_y_ticks=4)

        # Card trace
        points = [(mapper.x(p), mapper.y(l)) for p, l in zip(pos, load)]
        svg += svg_polyline(points, COLORS["card_trace"], width=2.5, close=True)

        # Upstroke / downstroke labels
        mid_x = (mapper.sx_min + mapper.sx_max) / 2
        svg += svg_text(mid_x, mapper.sy_min + 30, "UPSTROKE →", size=9, color=COLORS["tier_1"])
        svg += svg_text(mid_x, mapper.sy_max - 15, "← DOWNSTROKE", size=9, color=COLORS["tier_2"])

        # Physics engine flow description
        flow_y = 380
        flow_items = [
            ("1.", "Surface Card Measured", COLORS["text"]),
            ("2.", "Physics Engine (Gibbs / FD)", COLORS["text"]),
            ("3.", "Downhole Card Reconstructed", COLORS["text"]),
            ("4.", "ML Classifier → 18 Modes", COLORS["tier_1"]),
        ]
        svg += svg_text(CARD_X + 160, flow_y, "Analysis Pipeline", size=12, weight="bold", color=COLORS["text"])
        for i, (num, text, color) in enumerate(flow_items):
            y = flow_y + 22 + i * 20
            svg += svg_text(CARD_X + 60, y, num, size=10, anchor="start", color=color, weight="bold")
            svg += svg_text(CARD_X + 80, y, text, size=10, anchor="start", color=color)
            if i < len(flow_items) - 1:
                svg += svg_text(CARD_X + 65, y + 13, "↓", size=10, anchor="start", color=COLORS["grid"])

        # Key metrics box
        box_y = 510
        svg += svg_rect(CARD_X + 30, box_y, 280, 120, COLORS["frame"], fill="#f8f8f8", rx=6)
        svg += svg_text(CARD_X + 170, box_y + 20, "Key Engineering Outputs", size=11, weight="bold", color=COLORS["text"])
        metrics = [
            "• Pump fillage & production rate",
            "• Gearbox torque (API 11E)",
            "• Rod buckling detection",
            "• Ideal card comparison",
            "• Counterbalance optimization",
            "• Confidence score (0–100%)",
        ]
        for i, metric in enumerate(metrics):
            svg += svg_text(
                CARD_X + 50, box_y + 38 + i * 14, metric,
                size=9, anchor="start", color=COLORS["text"],
            )

        return svg

    def _draw_data_flow(self) -> str:
        """Draw the data flow arrow from well to card."""
        svg = ""

        # Curved arrow from polished rod area to card
        svg += svg_line(200, 150, 530, 150, COLORS["tier_1"], 1.5, dash="6,3")
        # Arrowhead
        svg += svg_polyline(
            [(520, 145), (530, 150), (520, 155)],
            COLORS["tier_1"], width=1.5,
        )
        svg += svg_text(
            365, 142, "Dynamometer Data →",
            size=9, color=COLORS["tier_1"],
        )

        return svg


def generate_rod_pump_schematic(output_path: Path) -> Path:
    """Generate and save the rod pump schematic SVG."""
    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    gen = RodPumpSchematicGenerator()
    svg_content = gen.render()
    output_path.write_text(svg_content)
    return output_path


__all__ = [
    "RodPumpSchematicGenerator",
    "generate_rod_pump_schematic",
]
