# ABOUTME: Renders an annotated dynamometer card with ML diagnostic overlay.
# ABOUTME: Shows classification result, confidence score, and differential diagnosis.

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
    svg_line,
    svg_polyline,
    svg_rect,
    svg_text,
)


WIDTH = 700
HEIGHT = 400


class DiagnosticAnnotator:
    """Renders a dynamometer card with ML diagnostic annotations.

    Shows the card trace alongside a classification results panel
    with confidence score, differential diagnosis, and feature callouts.
    """

    def render(
        self,
        card: CardData,
        mode_name: str,
        confidence: float = 0.923,
        differential: list[dict[str, object]] | None = None,
    ) -> str:
        """Render annotated diagnostic card SVG."""
        if differential is None:
            differential = [
                {"mode": mode_name, "probability": confidence},
                {"mode": "GAS_INTERFERENCE", "probability": 0.041},
                {"mode": "DELAYED_TV_CLOSURE", "probability": 0.018},
            ]

        title = MODE_LABELS.get(mode_name, mode_name)
        tier = MODE_TIERS.get(mode_name, 1)
        tier_color = TIER_INFO[tier]["color"]

        pos = card.position
        load = card.load

        pos_min, pos_max = min(pos), max(pos)
        load_min, load_max = min(load), max(load)
        pos_pad = max((pos_max - pos_min) * 0.05, 1.0)
        load_pad = max((load_max - load_min) * 0.05, 100.0)

        # Card takes left 55% of width
        card_right = int(WIDTH * 0.55)
        mapper = CoordMapper(
            pos_min - pos_pad, pos_max + pos_pad,
            load_min - load_pad, load_max + load_pad,
            60.0, float(card_right - 20),
            60.0, float(HEIGHT - 50),
        )

        svg = svg_header(WIDTH, HEIGHT)

        # Title
        svg += svg_text(
            WIDTH / 2, 25,
            f"ML Diagnostic: {title}",
            color=COLORS["text"], size=16, weight="bold",
        )
        svg += svg_text(
            WIDTH / 2, 42,
            f"AI-Driven Pump Diagnosis — {TIER_INFO[tier]['label']}",
            color=tier_color, size=10,
        )

        # Draw card axes and trace
        svg += draw_card_axes(mapper, n_x_ticks=4, n_y_ticks=4)
        points = [(mapper.x(p), mapper.y(l)) for p, l in zip(pos, load)]
        svg += svg_polyline(points, COLORS["card_trace"], width=2.5, close=True)

        # Annotation callout — find the point of interest
        # For fluid pound: the sharp drop; for others: midpoint of upstroke
        n = len(pos)
        half = n // 2
        if mode_name == "FLUID_POUND":
            # Find the max load diff point
            max_diff_idx = half
            max_diff = 0
            for i in range(half, n - 1):
                diff = abs(load[i + 1] - load[i])
                if diff > max_diff:
                    max_diff = diff
                    max_diff_idx = i
            callout_idx = max_diff_idx
            callout_text = "Sharp load drop"
        elif mode_name == "GAS_INTERFERENCE":
            callout_idx = int(n * 0.75)
            callout_text = "Gas compression"
        elif mode_name == "PUMP_TAGGING":
            callout_idx = max(range(n), key=lambda i: load[i])
            callout_text = "Load spike"
        else:
            callout_idx = n // 4
            callout_text = "Diagnostic feature"

        cx = mapper.x(pos[callout_idx])
        cy = mapper.y(load[callout_idx])
        svg += svg_line(cx, cy, cx + 30, cy - 25, COLORS["annotation"], 1.5)
        svg += svg_rect(cx + 30, cy - 38, 90, 18, COLORS["annotation"], fill="#f3e5f5", rx=3)
        svg += svg_text(
            cx + 75, cy - 25, callout_text,
            size=8, color=COLORS["annotation"],
        )

        # --- Right panel: Classification results ---
        panel_x = card_right + 20
        panel_width = WIDTH - panel_x - 15

        # Classification box
        svg += svg_rect(panel_x, 55, panel_width, 40, tier_color, fill="#f8f8f8", rx=5)
        svg += svg_text(
            panel_x + panel_width / 2, 75,
            f"Classification: {title}",
            size=12, weight="bold", color=tier_color,
        )

        # Confidence meter
        conf_y = 110
        svg += svg_text(panel_x + 5, conf_y, "Confidence:", size=10, anchor="start", color=COLORS["text"])
        bar_x = panel_x + 5
        bar_w = panel_width - 50
        svg += svg_rect(bar_x, conf_y + 5, bar_w, 14, COLORS["frame"], fill="#eeeeee", rx=3)
        svg += svg_rect(bar_x, conf_y + 5, bar_w * confidence, 14, "none", fill=tier_color, rx=3)
        svg += svg_text(
            bar_x + bar_w + 5, conf_y + 16,
            f"{confidence:.1%}",
            size=10, anchor="start", color=tier_color, weight="bold",
        )

        # Differential diagnosis
        diff_y = conf_y + 40
        svg += svg_text(
            panel_x + 5, diff_y,
            "Differential Diagnosis:",
            size=10, anchor="start", color=COLORS["text"], weight="bold",
        )

        for i, entry in enumerate(differential[:3]):
            y = diff_y + 20 + i * 22
            mode = entry["mode"]
            prob = entry["probability"]
            label = MODE_LABELS.get(mode, mode)

            # Probability bar
            svg += svg_text(
                panel_x + 10, y, f"{i + 1}.",
                size=9, anchor="start", color=COLORS["text"],
            )
            svg += svg_text(
                panel_x + 25, y, label,
                size=9, anchor="start", color=COLORS["text"],
            )
            # Mini bar
            mini_bar_w = (panel_width - 20) * 0.5
            svg += svg_rect(panel_x + 5, y + 4, mini_bar_w, 8, COLORS["grid"], fill="#f0f0f0", rx=2)
            svg += svg_rect(panel_x + 5, y + 4, mini_bar_w * prob, 8, "none", fill=tier_color, rx=2)
            svg += svg_text(
                panel_x + 10 + mini_bar_w, y + 11,
                f"{prob:.1%}",
                size=8, anchor="start", color=COLORS["frame"],
            )

        # Model info box
        info_y = diff_y + 100
        svg += svg_rect(panel_x, info_y, panel_width, 70, COLORS["grid"], fill="#fafafa", rx=4)
        info_items = [
            ("Model:", "GradientBoosting v1.0"),
            ("Features:", "Bezerra Projections"),
            ("Training:", "5,400 synthetic cards"),
            ("Accuracy:", "89.4% cross-validated"),
        ]
        for i, (label, value) in enumerate(info_items):
            y = info_y + 16 + i * 14
            svg += svg_text(panel_x + 10, y, label, size=8, anchor="start", color=COLORS["frame"], weight="bold")
            svg += svg_text(panel_x + 65, y, value, size=8, anchor="start", color=COLORS["text"])

        svg += svg_footer()
        return svg


def generate_sample_diagnostic(
    output_path: Path,
    mode_name: str = "FLUID_POUND",
    seed: int = 42,
) -> Path:
    """Generate and save a sample annotated diagnostic SVG."""
    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    gen = ALL_GENERATORS[mode_name]
    card = gen(seed=seed)

    annotator = DiagnosticAnnotator()
    svg_content = annotator.render(card, mode_name)
    output_path.write_text(svg_content)
    return output_path


__all__ = [
    "DiagnosticAnnotator",
    "generate_sample_diagnostic",
]
