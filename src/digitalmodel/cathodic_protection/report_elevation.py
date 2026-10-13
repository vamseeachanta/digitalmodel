# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Validated display geometry, scaled jacket elevation and zone surface areas."""

from __future__ import annotations

import math
import textwrap
from collections.abc import Mapping
from dataclasses import dataclass
from html import escape
from typing import Any

from digitalmodel.reporting.spec import Block, FigureBlock, TableBlock, TextBlock

NO_GEOMETRY = (
    "Member geometry and zone elevations are not supplied for this case; "
    "no elevation is drawn."
)
FIG_ELEVATION = "fig-cp-jacket-elevation"
ZONE_NAMES = {"atmospheric", "splash", "submerged", "buried"}
Point = tuple[float, float]


@dataclass(frozen=True)
class Member:
    """A named display member with endpoints and diameter."""

    name: str
    kind: str
    start: Point
    end: Point
    diameter: float
    true_length: float
    count: int


@dataclass(frozen=True)
class Zone:
    """A labelled exposure interval in the display geometry."""

    name: str
    lower: float
    upper: float
    calculation_zone: str


@dataclass(frozen=True)
class Geometry:
    """Validated display geometry for the jacket elevation."""

    datum: str
    basis: str
    waterline: float
    mudline: float
    zones: tuple[Zone, ...]
    members: tuple[Member, ...]
    brace_levels: tuple[float, ...]
    citation: str
    attribution: str
    licence: str


def _mapping(raw: Any, fields: set[str], required: set[str]) -> Mapping[str, Any]:
    if not isinstance(raw, Mapping):
        raise ValueError("geometry item must be a mapping")
    if set(raw) - fields or required - set(raw):
        raise ValueError("geometry item has unknown or missing fields")
    return raw


def _number(raw: Any) -> float:
    if isinstance(raw, bool) or not isinstance(raw, (int, float)):
        raise ValueError("geometry coordinate must be a finite number")
    try:
        value = float(raw)
    except OverflowError as error:
        raise ValueError("geometry coordinate must be a finite number") from error
    if not math.isfinite(value):
        raise ValueError("geometry coordinate must be a finite number")
    return value


def _text(raw: Any) -> str:
    if not isinstance(raw, str) or not raw.strip():
        raise ValueError("geometry label must be a non-empty string")
    return raw.strip()


def _list(raw: Any) -> list[Any]:
    if not isinstance(raw, list):
        raise ValueError("geometry collection must be a list")
    return raw


def _point(raw: Any) -> Point:
    values = _list(raw)
    if len(values) != 2:
        raise ValueError("member endpoint requires x and elevation coordinates")
    return _number(values[0]), _number(values[1])


def _member(raw: Any, kind: str) -> Member:
    required = {"name", "start", "end", "diameter_m"}
    item = _mapping(raw, required | {"true_length_m", "count"}, required)
    start, end = _point(item["start"]), _point(item["end"])
    diameter = _number(item["diameter_m"])
    if diameter <= 0 or (start == end and "true_length_m" not in item):
        raise ValueError("member requires positive diameter and non-zero length")
    if not math.isfinite(math.dist(start, end) * math.pi * diameter):
        raise ValueError("member surface area must be finite")
    if kind == "leg" and start[1] <= end[1]:
        raise ValueError("leg start must be above its end")
    length = _number(item.get("true_length_m", math.dist(start, end)))
    count = item.get("count", 1)
    if length < math.dist(start, end) or length <= 0:
        raise ValueError("true length must be positive and at least the projection length")
    if isinstance(count, bool) or not isinstance(count, int) or count < 1:
        raise ValueError("member count must be a positive integer")
    try:
        area = length * count * diameter * math.pi
    except OverflowError as error:
        raise ValueError("member surface area must be finite") from error
    if not math.isfinite(area):
        raise ValueError("member surface area must be finite")
    return Member(_text(item["name"]), kind, start, end, diameter, length, count)


def _zones(raw: Any) -> tuple[Zone, ...]:
    zones = []
    for item in _list(raw):
        fields = {"name", "lower_m", "upper_m"}
        item = _mapping(item, fields | {"calculation_zone"}, fields)
        zone = Zone(
            _text(item["name"]), _number(item["lower_m"]), _number(item["upper_m"]),
            _text(item.get("calculation_zone", item["name"])),
        )
        if ("calculation_zone" not in item and zone.name not in ZONE_NAMES) or zone.lower >= zone.upper:
            raise ValueError("zone requires a calculation id and ordered elevations")
        zones.append(zone)
    if not zones or len({zone.calculation_zone for zone in zones}) != len(zones):
        raise ValueError("zone names must be unique and non-empty")
    for lower, upper in zip(zones, zones[1:]):
        if lower.upper > upper.lower:
            raise ValueError("zone intervals must be ordered without overlap")
    return tuple(zones)


def _braces(raw: Any) -> tuple[list[Member], tuple[float, ...]]:
    members, levels = [], []
    for item in _list(raw):
        fields = {"elevation_m", "members"}
        item = _mapping(item, fields, fields)
        elevation = _number(item["elevation_m"])
        level_members = [
            _member(member, "horizontal") for member in _list(item["members"])
        ]
        if not level_members or any(
            member.start[1] != elevation or member.end[1] != elevation
            for member in level_members
        ):
            raise ValueError("brace members must lie at their stated level")
        members.extend(level_members)
        levels.append(elevation)
    if len(set(levels)) != len(levels):
        raise ValueError("brace levels must be unique")
    return members, tuple(levels)


def validate_geometry(raw: Any) -> Geometry:
    """Validate optional report geometry without modifying calculation inputs."""
    required = {
        "datum",
        "waterline_m",
        "mudline_m",
        "zones",
        "legs",
        "brace_levels",
        "diagonals",
        "basis",
    }
    fields = required | {"conductors", "risers", "citation", "attribution", "licence"}
    item = _mapping(raw, fields, required)
    basis = _text(item["basis"])
    sourced = basis in {"published_source", "openly_licensed_reference"}
    if basis not in {"design_data", "illustrative_regression_case", "published_source", "openly_licensed_reference"}:
        raise ValueError("geometry basis must identify design or illustrative data")
    citation = _text(item.get("citation")) if sourced else ""
    attribution = _text(item.get("attribution")) if sourced else ""
    licence = _text(item.get("licence")) if basis == "openly_licensed_reference" else ""
    waterline = _number(item["waterline_m"])
    mudline = _number(item["mudline_m"])
    if mudline >= waterline:
        raise ValueError("mudline must be below waterline")
    members = [_member(member, "leg") for member in _list(item["legs"])]
    if not members:
        raise ValueError("geometry requires at least one supplied leg")
    braces, levels = _braces(item["brace_levels"])
    members.extend(braces)
    for kind, key in (
        ("diagonal", "diagonals"),
        ("conductor", "conductors"),
        ("riser", "risers"),
    ):
        members.extend(_member(member, kind) for member in _list(item.get(key, [])))
    if len({member.name for member in members}) != len(members):
        raise ValueError("member names must be unique")
    if not math.isfinite(sum(m.true_length * m.count * m.diameter * math.pi for m in members)):
        raise ValueError("aggregate member area must be finite")
    geometry = Geometry(
        _text(item["datum"]),
        basis,
        waterline,
        mudline,
        _zones(item["zones"]),
        tuple(members),
        levels,
        citation,
        attribution,
        licence,
    )
    minimum_x, maximum_x, bottom, top = _extent(geometry)
    if not all(
        math.isfinite(value * 5) for value in (maximum_x - minimum_x, top - bottom)
    ):
        raise ValueError("scaled geometry extent must be finite")
    return geometry


def _fraction(member: Member, zone: Zone, top: float) -> float:
    low, high = sorted((member.start[1], member.end[1]))
    if low == high:
        return float(zone.lower <= low < zone.upper or low == zone.upper == top)
    return max(0.0, min(high, zone.upper) - max(low, zone.lower)) / (high - low)


def geometry_zone_areas(raw: Any) -> dict[str, float]:
    """Cylinder lateral area by clipped centreline length; no end-cap allowance."""
    geometry = raw if isinstance(raw, Geometry) else validate_geometry(raw)
    top = max(zone.upper for zone in geometry.zones)
    return {
        zone.calculation_zone: sum(
            math.pi
            * member.diameter
            * member.true_length
            * member.count
            * _fraction(member, zone, top)
            for member in geometry.members
        )
        for zone in geometry.zones
    }


def _extent(geometry: Geometry) -> tuple[float, float, float, float]:
    xs: list[float] = []
    zs = [geometry.waterline, geometry.mudline]
    for member in geometry.members:
        for x, z in (member.start, member.end):
            xs.extend((x - member.diameter / 2, x + member.diameter / 2))
            zs.extend((z - member.diameter / 2, z + member.diameter / 2))
    zs.extend(value for zone in geometry.zones for value in (zone.lower, zone.upper))
    return min(xs), max(xs), min(zs), max(zs)


def _n(value: float) -> str:
    return format(value, ".17g")


def _label(x: float, y: float, text: str) -> str:
    return f'<text x="{_n(x)}" y="{_n(y)}" font-size="10" fill="black">{escape(text)}</text>'


def _patterns(geometry: Geometry) -> str:
    patterns = []
    for index, _ in enumerate(geometry.zones):
        angle = index * 35
        patterns.append(
            f'<pattern id="elevation-zone-{index}" width="9" height="9" patternUnits="userSpaceOnUse" patternTransform="rotate({angle})">'
            '<rect width="9" height="9" fill="white"/>'
            '<path d="M 0 0 L 0 9" stroke="#888" stroke-width="0.35"/>'
            "</pattern>"
        )
    return "<defs>" + "".join(patterns) + "</defs>"


def _zone_svg(
    geometry: Geometry, left: float, width: float, top: float, scale: float
) -> str:
    parts = []
    for index, zone in enumerate(geometry.zones):
        y = 35 + (top - zone.upper) * scale
        height = (zone.upper - zone.lower) * scale
        parts.append(
            f'<g data-zone="{escape(zone.name, quote=True)}" data-lower-m="{_n(zone.lower)}" data-upper-m="{_n(zone.upper)}">'
            f'<rect x="{_n(left)}" y="{_n(y)}" width="{_n(width)}" height="{_n(height)}" fill="url(#elevation-zone-{index})" stroke="#888" stroke-width="0.5"/>'
            + _label(
                left + width + 10,
                y + height / 2 + 3,
                f"{zone.name}: {zone.lower:g} to {zone.upper:g} m",
            )
            + "</g>"
        )
    return "".join(parts)


def _member_svg(geometry: Geometry, minimum_x: float, top: float, scale: float) -> str:
    parts = []
    for member in geometry.members:
        x1 = 55 + (member.start[0] - minimum_x) * scale
        y1 = 35 + (top - member.start[1]) * scale
        x2 = 55 + (member.end[0] - minimum_x) * scale
        y2 = 35 + (top - member.end[1]) * scale
        parts.append(
            f'<line data-member="{escape(member.name, quote=True)}" data-kind="{member.kind}" x1="{_n(x1)}" y1="{_n(y1)}" x2="{_n(x2)}" y2="{_n(y2)}" stroke="black" stroke-width="{_n(member.diameter * scale)}"/>'
        )
    for level in geometry.brace_levels:
        parts.append(f'<g data-kind="brace-level" data-elevation-m="{_n(level)}"/>')
    return "".join(parts)


def _datum_lines(
    geometry: Geometry, left: float, width: float, top: float, scale: float
) -> str:
    parts = []
    for name, elevation, dash in (
        ("waterline", geometry.waterline, "7 3"),
        ("mudline", geometry.mudline, "2 3"),
    ):
        y = 35 + (top - elevation) * scale
        parts.append(
            f'<g data-level="{name}" data-elevation-m="{_n(elevation)}">'
            f'<line x1="{_n(left)}" x2="{_n(left + width)}" y1="{_n(y)}" y2="{_n(y)}" stroke="black" stroke-width="1" stroke-dasharray="{dash}"/>'
            + _label(left + width + 10, y - 4, f"{name}: {elevation:g} m")
            + "</g>"
        )
    return "".join(parts)


def _svg(geometry: Geometry) -> str:
    minimum_x, maximum_x, bottom, top = _extent(geometry)
    scale = 5.0 if not geometry.citation else min(500 / (top - bottom), 400 / (maximum_x - minimum_x))
    width = (maximum_x - minimum_x) * scale
    height = (top - bottom) * scale
    source_lines = textwrap.wrap(
        f"{geometry.basis.replace('_', ' ').upper()}; {geometry.attribution}; "
        f"{geometry.licence}; {geometry.citation}", width=40,
    ) if geometry.citation else []
    parts = [
        f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {_n(width + 290)} {_n(height + 95 + 14 * len(source_lines))}" data-scale="{_n(scale)}" preserveAspectRatio="xMidYMid meet" role="img" aria-label="Jacket elevation">'
    ]
    parts.extend(
        (
            _patterns(geometry),
            _zone_svg(geometry, 45, width + 20, top, scale),
            _member_svg(geometry, minimum_x, top, scale),
            _datum_lines(geometry, 45, width + 20, top, scale),
        )
    )
    parts.append(_label(55, 20, f"Datum: {geometry.datum}"))
    for index, line in enumerate(source_lines):
        parts.append(_label(45, height + 100 + index * 14, line))
    y = height + 65
    parts.append(
        f'<path d="M 55 {y:g} h {5 * scale:g} m {-5 * scale:g} -3 v 6 m {5 * scale:g} -6 v 6" stroke="black" fill="none"/>'
    )
    parts.append(_label(55, y + 17, "5 m; common horizontal and vertical scale"))
    parts.append("</svg>")
    return "".join(parts)


def _raw_geometry(inputs: Mapping[str, Any]) -> Any:
    structure = inputs.get("structure", {})
    return structure.get("report_geometry") if isinstance(structure, Mapping) else None


def design_blocks(inputs: Mapping[str, Any]) -> list[Block]:
    """Return an elevation only when member geometry is supplied."""
    raw = _raw_geometry(inputs)
    if raw is None:
        return [TextBlock(markdown=NO_GEOMETRY + " See Appendix A.")]
    try:
        geometry = validate_geometry(raw)
    except (ValueError, TypeError, KeyError) as error:
        return [TextBlock(markdown=f"Elevation not evaluated: {error}.")]
    blocks: list[Block] = [
        FigureBlock(
            title="Jacket elevation",
            figure_id=FIG_ELEVATION,
            svg=_svg(geometry),
            caption=(
                f"Datum: {geometry.datum}. "
                + ("Elevation derived from the cited member dimensions. " if geometry.citation else "Elevation drawn from the design data of the case. ")
                + (f"Basis: {geometry.basis}; {geometry.attribution}; {geometry.licence}; {geometry.citation}. " if geometry.citation else "")
                +
                "See Appendix A for the geometry basis and surface-area comparison."
            ),
        )
    ]
    if geometry.citation:
        blocks.append(TableBlock(
            title="Member dimensions and area derivation",
            columns=["Member", "Count", "True length", "Diameter", "Lateral area"],
            units=["-", "-", "m", "m", "m2"],
            rows=[[m.name, m.count, m.true_length, m.diameter,
                   math.pi * m.diameter * m.true_length * m.count] for m in geometry.members],
        ))
    return blocks


def geometry_caveats(
    inputs: Mapping[str, Any], results: Mapping[str, Any]
) -> list[str]:
    """Return geometry limitations and area comparisons for the appendix."""
    raw = _raw_geometry(inputs)
    if raw is None:
        return [NO_GEOMETRY]
    try:
        geometry = validate_geometry(raw)
    except (ValueError, TypeError, KeyError) as error:
        return [f"Elevation not evaluated: {error}."]
    caveats = []
    if geometry.basis == "illustrative_regression_case":
        caveats.append(
            "The supplied member coordinates are illustrative geometry of the "
            "regression case, not surveyed structure geometry."
        )
    caveats.append(
        "External steel area is the cylinder lateral surface area π × diameter × "
        "centreline length, clipped at each zone boundary. End caps and member "
        "intersections are not included. Horizontal members at an internal "
        "boundary belong to the upper zone; those at the highest boundary belong "
        "to the highest zone. The calculation uses the stated zone areas."
    )
    areas = geometry_zone_areas(geometry)
    total_area = sum(math.pi * m.diameter * m.true_length * m.count for m in geometry.members)
    unassigned = max(0.0, total_area - sum(areas.values()))
    if unassigned > 1e-8:
        caveats.append(f"Unassigned member area: {unassigned:.3f} m²; zone limits do not cover all supplied members.")
    stated = results.get("surface_areas_m2", {})
    for name, area in areas.items():
        reference = stated.get(name) if isinstance(stated, Mapping) else None
        if (
            reference is None
            or not math.isfinite(float(reference))
            or float(reference) <= 0
        ):
            caveats.append(
                f"{name}: area comparison not evaluated; "
                "a positive stated zone area is needed."
            )
        else:
            difference = (area - float(reference)) / float(reference) * 100
            caveats.append(
                f"{name}: drawn-member area {area:.3f} m²; "
                f"stated area {float(reference):.3f} m²; "
                f"percentage difference {difference:+.3f}% relative to the stated area."
            )
    return caveats
