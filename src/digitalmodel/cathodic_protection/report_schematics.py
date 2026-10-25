# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Proportional SVG views from explicit dimensions, with missing geometry stated.

Drawing offsets, font sizes and stroke widths are display units, not engineering
inputs. Both axes use one scale. A longitudinal pipeline view has a separate,
labelled cross-section detail so its diameter remains readable at long lengths.
"""

from __future__ import annotations

import math
from html import escape
from typing import Any, Mapping

# Display palette follows the report's navy, teal and pale blue presentation.
NAVY = "#123047"
TEAL = "#087e8b"
PALE = "#e8f3f7"
GOLD = "#bc7415"


def _positive(value: Any) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError("dimension must be a positive finite number")
    value = float(value)
    if not math.isfinite(value) or value <= 0:
        raise ValueError("dimension must be a positive finite number")
    return value


def _text(x: float, y: float, label: str) -> str:
    return f'<text x="{x:g}" y="{y:g}" fill="{NAVY}" font-size="13">{escape(label)}</text>'


def _line(x1: float, y1: float, x2: float, y2: float, css: str = "leader") -> str:
    return (f'<line class="{css}" x1="{x1:g}" y1="{y1:g}" x2="{x2:g}" '
            f'y2="{y2:g}" stroke="{TEAL}" stroke-width="1"/>')


def _dimension(x: float, y: float, width: float, label: str) -> str:
    return (_line(x, y, x + width, y, "dimension")
            + _line(x, y - 7, x, y + 7) + _line(x + width, y - 7, x + width, y + 7)
            + _text(x, y - 12, label))


def _vertical(x: float, y: float, height: float, label: str) -> str:
    return (_line(x, y, x, y + height, "dimension")
            + _line(x - 7, y, x + 7, y) + _line(x - 7, y + height, x + 7, y + height)
            + _text(x + 12, y + height / 2, label))


def _rect(x: float, y: float, width: float, height: float, **attributes: Any) -> str:
    extra = " ".join(f'{escape(k)}="{escape(str(v), quote=True)}"' for k, v in attributes.items())
    return (f'<rect x="{x:g}" y="{y:g}" width="{width:g}" height="{height:g}" '
            f'fill="{PALE}" stroke="{NAVY}" stroke-width="1.5" {extra}/>')


def _svg(title: str, content: str, scale: float, *, height: float = 650) -> str:
    return (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 800 {height:g}" '
            f'role="img" aria-labelledby="schematic-title" data-scale-x="{scale:g}" '
            f'data-scale-y="{scale:g}"><title id="schematic-title">{escape(title)}</title>'
            '<style>text{font-family:system-ui,sans-serif} .leader{stroke-dasharray:3 2}</style>'
            + _text(35, 28, title) + content + '</svg>\n')


def render_monopile(geometry: Mapping[str, Any]) -> str:
    """Draw a cylindrical exterior using a supplied diameter and exposed height."""
    diameter, height = _positive(geometry["diameter_m"]), _positive(geometry["height_m"])
    scale = min(280 / diameter, 410 / height)
    width, depth = diameter * scale, height * scale
    content = _rect(220, 100, width, depth)
    content += _dimension(220, 75, width, f"Diameter {diameter:g} m")
    content += _vertical(220 + width + 30, 100, depth, f"Exposed height {height:g} m")
    content += _line(220 + width / 2, 100 + depth, 120, 560)
    content += _text(35, 580, "Deepest exterior position; remote faces require survey access")
    return _svg("Monopile exterior elevation", content, scale)


def render_manifold(geometry: Mapping[str, Any]) -> str:
    """Draw a supplied frame envelope; do not infer member or anode layout."""
    width, height = _positive(geometry["width_m"]), _positive(geometry["height_m"])
    scale = min(420 / width, 340 / height)
    w, h = width * scale, height * scale
    content = _rect(100, 110, w, h)
    content += _dimension(100, 75, w, f"Frame width {width:g} m")
    content += _vertical(130 + w, 110, h, f"Frame height {height:g} m")
    content += _line(100 + w, 110 + h, 100 + w, 520)
    content += _text(35, 555, "Watch remote faces and the deepest lower frame positions")
    content += _text(35, 585, "Envelope only; member and anode coordinates must be supplied")
    return _svg("Subsea manifold envelope", content, scale)


def render_pipeline(geometry: Mapping[str, Any]) -> str:
    """Draw true-scale longitudinal geometry and a separately scaled detail."""
    diameter = _positive(geometry["diameter_m"])
    length = _positive(geometry["length_m"])
    spacing = _positive(geometry["spacing_m"])
    bracelet_length = _positive(geometry["bracelet_length_m"])
    # A missing thickness does not justify an assumed product dimension.
    thickness = _positive(geometry["bracelet_thickness_m"]) if "bracelet_thickness_m" in geometry else 0
    if spacing > length or bracelet_length > spacing:
        raise ValueError("bracelet length and spacing must fit the pipeline")
    scale = min(600 / length, 140 / (diameter + 2 * thickness))
    content = _rect(80, 120, length * scale, diameter * scale)
    # Nominal cell centres communicate spacing, not installation coordinates.
    count = math.floor(length / spacing + 0.5)
    if count > 2000:
        raise ValueError("too many nominal bracelet cells for a report drawing")
    for index in range(count):
        x = 80 + (index + 0.5) * spacing * scale
        content += _rect(x - bracelet_length * scale / 2, 120 - thickness * scale,
                         bracelet_length * scale, (diameter + 2 * thickness) * scale)
    content += _dimension(80, 80, length * scale, f"Length {length:g} m")
    content += _dimension(80, 180, spacing * scale, f"Nominal spacing {spacing:g} m")
    content += _line(80, 120, 80, 60) + _line(80 + length * scale, 120, 80 + length * scale, 60)
    detail_scale = min(180 / diameter, 220 / bracelet_length)
    content += (f'<g data-detail-scale-x="{detail_scale:g}" data-detail-scale-y="{detail_scale:g}">'
                + _rect(100, 300, bracelet_length * detail_scale, diameter * detail_scale)
                + _dimension(100, 270, bracelet_length * detail_scale, f"Bracelet length {bracelet_length:g} m")
                + _vertical(130 + bracelet_length * detail_scale, 300, diameter * detail_scale,
                            f"Pipe diameter {diameter:g} m") + '</g>')
    content += _text(35, 540, "Longitudinal view and enlarged diameter detail use separate stated scales")
    content += _text(35, 570, "Bracelets at nominal cell centres; installation end offsets are not recorded")
    content += _text(35, 600, "Survey remote end positions and midpoints between bracelets")
    return _svg("Bracelet pipeline exterior", content, scale)


def render_riser(geometry: Mapping[str, Any]) -> str:
    """Draw only explicitly supplied riser geometry and family elevations."""
    diameter, length = _positive(geometry["diameter_m"]), _positive(geometry["length_m"])
    scale = min(180 / diameter, 400 / length)
    content = _rect(230, 100, diameter * scale, length * scale)
    content += _vertical(280 + diameter * scale, 100, length * scale, f"Length {length:g} m")
    content += _dimension(230, 70, diameter * scale, f"Diameter {diameter:g} m")
    for name, position in geometry.get("family_positions_m", {}).items():
        distance = _positive(position)
        if distance > length:
            raise ValueError("family position lies outside the riser")
        y = 100 + distance * scale
        content += _line(230, y, 80, y) + _text(35, y - 8, f"{name}: {distance:g} m from top")
    content += _text(35, 580, "Deepest and remote family positions require measured potentials")
    return _svg("Riser family elevation", content, scale)


def render_schematic(kind: str, geometry: Mapping[str, Any]) -> str:
    """Dispatch a drawing, retaining a visible missing-data state when needed."""
    renderers = {"monopile": render_monopile, "manifold": render_manifold,
                 "pipeline": render_pipeline, "riser": render_riser}
    required = {"monopile": {"diameter_m", "height_m"}, "manifold": {"width_m", "height_m"},
                "pipeline": {"diameter_m", "length_m", "spacing_m", "bracelet_length_m"},
                "riser": {"diameter_m", "length_m"}}
    if kind not in renderers:
        raise ValueError("unsupported schematic structure")
    if not required[kind] <= geometry.keys():
        content = _text(35, 90, "NOT_EVALUATED: overall dimensions and coordinates are not recorded")
        content += _text(35, 120, "No overall structure geometry or anode allocation is inferred from surface area")
        detail = geometry.get("anode_detail", {})
        if "length_m" in detail and "radius_m" in detail:
            length, radius = _positive(detail["length_m"]), _positive(detail["radius_m"])
            scale = min(330 / length, 200 / (2 * radius))
            content += _rect(100, 210, length * scale, 2 * radius * scale)
            content += _dimension(100, 180, length * scale, f"Anode length {length:g} m")
            content += _vertical(130 + length * scale, 210, 2 * radius * scale,
                                 f"Anode diameter {2 * radius:g} m")
            content += _text(35, 500, "Anode detail only; exterior structure dimensions are required for its elevation")
            return _svg(f"{kind.capitalize()}: geometry data gap and proportional anode detail", content, scale)
        return _svg(f"{kind.capitalize()}: geometry data gap", content, 1)
    return renderers[kind](geometry)
