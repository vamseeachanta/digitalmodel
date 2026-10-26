# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Optional geometric CP distribution screen, independent of potential solving.

Coordinates are Cartesian triples in metres in a common frame. Coverage applies
only to the supplied surface/governing points. Adjacent spacing is the longest
edge of the Euclidean minimum spanning tree of zone-local anodes; its limit is
twice the reach. This connects separated clusters without using the full array
span as an adjacent gap. Obstructions, shielding and continuity need engineering
assessment and field confirmation even when this screen passes.
"""

from collections.abc import Mapping, Sequence
import math
from typing import Any


DEFAULT_REACH_M = 4.0
DEFAULT_REACH_BASIS = (
    "Illustrative geometric scale only: half the 8 m actual_max_spacing_m in "
    "tests/fixtures/cathodic_protection/workflow_inputs/ships.yml. "
    "This is not a validated offshore electrical reach; "
    "user to confirm against the governing code."
)


def _reach(value: Any) -> float:
    try:
        number = float(value)
    except (ValueError, TypeError) as exc:
        raise ValueError("reach_m must be a positive finite number") from exc
    if isinstance(value, bool) or not math.isfinite(number) or number <= 0:
        raise ValueError("reach_m must be a positive finite number")
    return number


def _points(raw: Any) -> list[tuple[float, float, float]]:
    if not isinstance(raw, list):
        raise ValueError("coordinates must be a list of Cartesian triples in metres")
    result = []
    for point in raw:
        if not isinstance(point, (list, tuple)) or len(point) != 3:
            raise ValueError("coordinates must contain three finite numbers")
        try:
            xyz = tuple(float(v) for v in point)
        except (ValueError, TypeError) as exc:
            raise ValueError("coordinates must contain three finite numbers") from exc
        if any(isinstance(v, bool) for v in point) or not all(math.isfinite(v) for v in xyz):
            raise ValueError("coordinates must contain three finite numbers")
        result.append((xyz[0], xyz[1], xyz[2]))
    return result


def _adjacent_spacing(anodes: list[tuple[float, float, float]]) -> float:
    """Return the longest spanning-tree edge with quadratic time, linear storage."""
    if len(anodes) < 2:
        return 0.0
    remaining = set(range(1, len(anodes)))
    nearest = {i: math.dist(anodes[0], anodes[i]) for i in remaining}
    longest = 0.0
    while remaining:
        chosen = min(remaining, key=lambda i: (nearest[i], i))
        longest = max(longest, nearest[chosen])
        remaining.remove(chosen)
        for i in remaining:
            nearest[i] = min(nearest[i], math.dist(anodes[chosen], anodes[i]))
    return longest


def _zone_screen(raw: Mapping[str, Any], reach: float) -> dict[str, Any]:
    anodes = _points(raw.get("anode_positions_m", []))
    points = _points(raw.get("governing_points_m", []))
    spacing = _adjacent_spacing(anodes) if anodes else None
    point_results = []
    for point in points:
        nearest = min(range(len(anodes)), key=lambda i: math.dist(point, anodes[i])) if anodes else None
        distance = math.dist(point, anodes[nearest]) if nearest is not None else None
        point_results.append({
            "coordinates_m": list(point), "nearest_anode_index": nearest,
            "distance_m": distance,
            "margin_m": reach - distance if distance is not None else None,
        })
    reasons = []
    if not anodes:
        reasons.append("No zone-local anode positions supplied")
    if not points:
        reasons.append("No governing surface points supplied")
    reach_margin = min(p["margin_m"] for p in point_results) if anodes and points else None
    spacing_margin = 2 * reach - spacing if spacing is not None else None
    if reach_margin is not None and reach_margin < 0:
        reasons.append("Governing surface point exceeds reach")
    if spacing_margin is not None and spacing_margin < 0:
        reasons.append("Adjacent anode spacing exceeds twice the reach")
    margin = (
        min(reach_margin, spacing_margin)
        if reach_margin is not None and spacing_margin is not None else None
    )
    return {
        "status": "FLAG" if reasons else "PASS", "reach_m": reach,
        "max_spacing_m": spacing, "spacing_limit_m": 2 * reach,
        "spacing_margin_m": spacing_margin, "reach_margin_m": reach_margin,
        "margin_m": margin,
        "anode_count": len(anodes), "points": point_results, "reasons": reasons,
    }


def screen_current_distribution(
    config: Mapping[str, Any], protected_zones: Sequence[str],
) -> dict[str, Any]:
    """Return PASS/FLAG and signed margins for every protected zone.

    Missing geometry yields FLAG and an unknown margin, never an inferred PASS.
    A supplied reach overrides the illustrative default; zone reach overrides
    the global reach. Positive margins indicate clearance to the chosen limit.
    """
    if set(config) - {"reach_m", "zones"}:
        raise ValueError("current_distribution contains unknown input keys")
    reach = _reach(config.get("reach_m", DEFAULT_REACH_M))
    zones = config.get("zones", {})
    if not isinstance(zones, Mapping):
        raise ValueError("current_distribution.zones must be a mapping")
    if set(zones) - set(protected_zones):
        raise ValueError("current_distribution contains an unknown protected zone")
    results = {}
    for name in protected_zones:
        raw = zones.get(name, {})
        if not isinstance(raw, Mapping):
            raise ValueError("current_distribution zone must be a mapping")
        if set(raw) - {"reach_m", "anode_positions_m", "governing_points_m"}:
            raise ValueError("current_distribution zone contains unknown input keys")
        results[name] = _zone_screen(raw, _reach(raw.get("reach_m", reach)))
    basis = (
        "User input; user to confirm against the governing code"
        if "reach_m" in config else DEFAULT_REACH_BASIS
    )
    return {
        "reach_m": reach,
        "reach_basis": basis,
        "spacing_method": "Longest edge of zone-local Euclidean minimum spanning tree",
        "scope": "Governing points only; shielding, potential and continuity are not computed",
        "zones": results,
    }
