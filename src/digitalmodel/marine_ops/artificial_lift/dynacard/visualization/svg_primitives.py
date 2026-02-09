# ABOUTME: SVG primitive helpers and coordinate mapping for dynacard visualization.
# ABOUTME: Provides CoordMapper, SVG drawing functions, and color scheme constants.

from __future__ import annotations

from typing import Optional
from xml.sax.saxutils import escape as _xml_escape


# ---------------------------------------------------------------------------
# Color scheme
# ---------------------------------------------------------------------------

COLORS = {
    "background": "#ffffff",
    "grid": "#e0e0e0",
    "text": "#333333",
    "frame": "#666666",
    "card_trace": "#1a3a5c",
    "card_fill": "#e8f0f8",
    "ideal_card": "#00bcd4",
    "tier_1": "#2e7d32",
    "tier_2": "#f57f17",
    "tier_3": "#c62828",
    "annotation": "#6a1b9a",
}

# Tier labels for the diagnostic gallery
TIER_INFO = {
    1: {"label": "Tier 1 — Core Conditions", "color": COLORS["tier_1"]},
    2: {"label": "Tier 2 — Common Field Failures", "color": COLORS["tier_2"]},
    3: {"label": "Tier 3 — Mechanical & Environmental", "color": COLORS["tier_3"]},
}

# Map each failure mode to its tier
MODE_TIERS: dict[str, int] = {
    "NORMAL": 1,
    "GAS_INTERFERENCE": 1,
    "FLUID_POUND": 1,
    "PUMP_TAGGING": 1,
    "TUBING_MOVEMENT": 1,
    "VALVE_LEAK_TV": 1,
    "VALVE_LEAK_SV": 1,
    "ROD_PARTING": 2,
    "STUCK_PUMP": 2,
    "WORN_BARREL": 2,
    "GAS_LOCK": 2,
    "DELAYED_TV_CLOSURE": 2,
    "EXCESSIVE_FRICTION": 3,
    "PLUNGER_UNDERTRAVEL": 3,
    "PARAFFIN_RESTRICTION": 3,
    "BENT_BARREL": 3,
    "SAND_ABRASION": 3,
    "EXCESSIVE_VIBRATION": 3,
}

# Human-readable labels for each failure mode
MODE_LABELS: dict[str, str] = {
    "NORMAL": "Normal",
    "GAS_INTERFERENCE": "Gas Interference",
    "FLUID_POUND": "Fluid Pound",
    "PUMP_TAGGING": "Pump Tagging",
    "TUBING_MOVEMENT": "Tubing Movement",
    "VALVE_LEAK_TV": "Traveling Valve Leak",
    "VALVE_LEAK_SV": "Standing Valve Leak",
    "ROD_PARTING": "Rod Parting",
    "STUCK_PUMP": "Stuck Pump",
    "WORN_BARREL": "Worn Barrel",
    "GAS_LOCK": "Gas Lock",
    "DELAYED_TV_CLOSURE": "Delayed TV Closure",
    "EXCESSIVE_FRICTION": "Excessive Friction",
    "PLUNGER_UNDERTRAVEL": "Plunger Undertravel",
    "PARAFFIN_RESTRICTION": "Paraffin Restriction",
    "BENT_BARREL": "Bent Barrel",
    "SAND_ABRASION": "Sand Abrasion",
    "EXCESSIVE_VIBRATION": "Excessive Vibration",
}

# Ordered list of modes by tier (for gallery layout)
MODES_BY_TIER: dict[int, list[str]] = {
    1: [
        "NORMAL", "GAS_INTERFERENCE", "FLUID_POUND",
        "PUMP_TAGGING", "TUBING_MOVEMENT", "VALVE_LEAK_TV",
        "VALVE_LEAK_SV",
    ],
    2: [
        "ROD_PARTING", "STUCK_PUMP", "WORN_BARREL",
        "GAS_LOCK", "DELAYED_TV_CLOSURE",
    ],
    3: [
        "EXCESSIVE_FRICTION", "PLUNGER_UNDERTRAVEL", "PARAFFIN_RESTRICTION",
        "BENT_BARREL", "SAND_ABRASION", "EXCESSIVE_VIBRATION",
    ],
}


# ---------------------------------------------------------------------------
# Coordinate mapping
# ---------------------------------------------------------------------------


class CoordMapper:
    """Maps from world coordinates to SVG pixel coordinates."""

    def __init__(
        self,
        world_x_min: float,
        world_x_max: float,
        world_y_min: float,
        world_y_max: float,
        svg_x_min: float,
        svg_x_max: float,
        svg_y_min: float,
        svg_y_max: float,
    ) -> None:
        self.wx_min = world_x_min
        self.wx_max = world_x_max
        self.wy_min = world_y_min
        self.wy_max = world_y_max
        self.sx_min = svg_x_min
        self.sx_max = svg_x_max
        self.sy_min = svg_y_min
        self.sy_max = svg_y_max

        wx_range = self.wx_max - self.wx_min
        wy_range = self.wy_max - self.wy_min
        self._sx_scale = (self.sx_max - self.sx_min) / wx_range if wx_range else 1.0
        self._sy_scale = (self.sy_max - self.sy_min) / wy_range if wy_range else 1.0

    def x(self, world_x: float) -> float:
        """Map world x to SVG x."""
        return self.sx_min + (world_x - self.wx_min) * self._sx_scale

    def y(self, world_y: float) -> float:
        """Map world y to SVG y (inverted: higher world y = lower SVG y)."""
        return self.sy_max - (world_y - self.wy_min) * self._sy_scale


# ---------------------------------------------------------------------------
# SVG helper functions
# ---------------------------------------------------------------------------


def svg_header(width: int, height: int) -> str:
    """SVG document header with xmlns for GitHub compatibility."""
    return (
        f'<svg xmlns="http://www.w3.org/2000/svg" '
        f'width="{width}" height="{height}" '
        f'viewBox="0 0 {width} {height}">\n'
        f'<rect width="{width}" height="{height}" '
        f'fill="{COLORS["background"]}"/>\n'
    )


def svg_footer() -> str:
    """SVG document footer."""
    return "</svg>"


def svg_group(x: float = 0, y: float = 0) -> str:
    """Open a translated SVG group."""
    return f'<g transform="translate({x:.1f},{y:.1f})">\n'


def svg_group_end() -> str:
    """Close an SVG group."""
    return "</g>\n"


def svg_line(
    x1: float,
    y1: float,
    x2: float,
    y2: float,
    color: str,
    width: float = 1.0,
    dash: Optional[str] = None,
) -> str:
    """SVG line element."""
    style = f'stroke="{color}" stroke-width="{width}" fill="none"'
    if dash:
        style += f' stroke-dasharray="{dash}"'
    return f'<line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" {style}/>\n'


def svg_polyline(
    points: list[tuple[float, float]],
    color: str,
    width: float = 1.5,
    fill: str = "none",
    close: bool = False,
) -> str:
    """SVG polyline or polygon element."""
    pts = " ".join(f"{x:.1f},{y:.1f}" for x, y in points)
    tag = "polygon" if close else "polyline"
    return (
        f'<{tag} points="{pts}" '
        f'stroke="{color}" stroke-width="{width}" fill="{fill}"/>\n'
    )


def svg_text(
    x: float,
    y: float,
    text: str,
    color: str = COLORS["text"],
    size: int = 12,
    anchor: str = "middle",
    rotate: Optional[float] = None,
    weight: str = "normal",
) -> str:
    """SVG text element."""
    transform = ""
    if rotate is not None:
        transform = f' transform="rotate({rotate},{x:.1f},{y:.1f})"'
    return (
        f'<text x="{x:.1f}" y="{y:.1f}" '
        f'font-family="sans-serif" font-size="{size}" font-weight="{weight}" '
        f'fill="{color}" text-anchor="{anchor}"{transform}>'
        f"{_xml_escape(text)}</text>\n"
    )


def svg_rect(
    x: float,
    y: float,
    w: float,
    h: float,
    color: str,
    width: float = 1.0,
    fill: str = "none",
    rx: float = 0,
) -> str:
    """SVG rectangle element."""
    rx_attr = f' rx="{rx:.1f}"' if rx > 0 else ""
    return (
        f'<rect x="{x:.1f}" y="{y:.1f}" width="{w:.1f}" height="{h:.1f}" '
        f'stroke="{color}" stroke-width="{width}" fill="{fill}"{rx_attr}/>\n'
    )


def svg_circle(
    cx: float,
    cy: float,
    r: float,
    color: str,
    width: float = 1.0,
    fill: str = "none",
) -> str:
    """SVG circle element."""
    return (
        f'<circle cx="{cx:.1f}" cy="{cy:.1f}" r="{r:.1f}" '
        f'stroke="{color}" stroke-width="{width}" fill="{fill}"/>\n'
    )


def svg_arc_path(
    cx: float,
    cy: float,
    r: float,
    start_angle: float,
    end_angle: float,
    color: str,
    width: float = 1.5,
) -> str:
    """SVG arc path element (for the walking beam curve)."""
    import math
    x1 = cx + r * math.cos(math.radians(start_angle))
    y1 = cy + r * math.sin(math.radians(start_angle))
    x2 = cx + r * math.cos(math.radians(end_angle))
    y2 = cy + r * math.sin(math.radians(end_angle))
    large_arc = 1 if abs(end_angle - start_angle) > 180 else 0
    return (
        f'<path d="M {x1:.1f},{y1:.1f} A {r:.1f},{r:.1f} 0 {large_arc},1 '
        f'{x2:.1f},{y2:.1f}" stroke="{color}" stroke-width="{width}" fill="none"/>\n'
    )


def draw_card_axes(
    mapper: CoordMapper,
    x_label: str = "Position (in)",
    y_label: str = "Load (lbs)",
    n_x_ticks: int = 3,
    n_y_ticks: int = 3,
) -> str:
    """Draw axis frame, grid lines, and labels for a dynamometer card."""
    svg = ""

    # Frame
    svg += svg_rect(
        mapper.sx_min,
        mapper.sy_min,
        mapper.sx_max - mapper.sx_min,
        mapper.sy_max - mapper.sy_min,
        COLORS["frame"],
    )

    wx_range = mapper.wx_max - mapper.wx_min
    wy_range = mapper.wy_max - mapper.wy_min

    # Vertical grid lines + x-axis tick labels
    for i in range(n_x_ticks + 1):
        wx = mapper.wx_min + i * wx_range / n_x_ticks
        sx = mapper.x(wx)
        svg += svg_line(sx, mapper.sy_min, sx, mapper.sy_max, COLORS["grid"], 0.5)
        svg += svg_text(sx, mapper.sy_max + 14, f"{wx:.0f}", size=8)

    # Horizontal grid lines + y-axis tick labels
    for i in range(n_y_ticks + 1):
        wy = mapper.wy_min + i * wy_range / n_y_ticks
        sy = mapper.y(wy)
        svg += svg_line(mapper.sx_min, sy, mapper.sx_max, sy, COLORS["grid"], 0.5)
        svg += svg_text(
            mapper.sx_min - 4, sy + 3, f"{wy:.0f}", size=8, anchor="end"
        )

    # Axis labels
    mid_x = (mapper.sx_min + mapper.sx_max) / 2
    svg += svg_text(mid_x, mapper.sy_max + 26, x_label, size=9)
    mid_y = (mapper.sy_min + mapper.sy_max) / 2
    svg += svg_text(
        mapper.sx_min - 30, mid_y, y_label, size=9, rotate=-90
    )

    return svg


__all__ = [
    "COLORS",
    "TIER_INFO",
    "MODE_TIERS",
    "MODE_LABELS",
    "MODES_BY_TIER",
    "CoordMapper",
    "svg_header",
    "svg_footer",
    "svg_group",
    "svg_group_end",
    "svg_line",
    "svg_polyline",
    "svg_text",
    "svg_rect",
    "svg_circle",
    "svg_arc_path",
    "draw_card_axes",
]
