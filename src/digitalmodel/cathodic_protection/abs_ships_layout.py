"""ABS ship-hull anode distribution and spacing checks."""

from __future__ import annotations

import math
from typing import Any, Mapping


def _positive(value: Any, name: str) -> float:
    try:
        number = float(value)
    except (TypeError, ValueError) as exc:
        raise ValueError(f"{name} must be a positive finite number") from exc
    if not math.isfinite(number) or number <= 0.0:
        raise ValueError(f"{name} must be a positive finite number")
    return number


def _positive_integer(value: Any, name: str) -> int:
    if isinstance(value, bool):
        raise ValueError(f"{name} must be a positive integer")
    number = _positive(value, name)
    if not number.is_integer():
        raise ValueError(f"{name} must be a positive integer")
    return int(number)


def _boolean(value: Any, name: str) -> bool:
    if not isinstance(value, bool):
        raise ValueError(f"{name} must be a boolean")
    return value


def _required_inputs(layout: Mapping[str, Any]) -> list[str]:
    required = {
        "actual_max_spacing_m",
        "selected_locations",
        "anodes_per_location",
        "high_current_or_low_resistivity",
        "mechanical_damage_risk",
        "uniform_distribution_confirmed",
        "bilge_damage_avoided",
        "bilge_keel_fitted",
    }
    return sorted(required.difference(layout))


def _dimensions(layout: Mapping[str, Any]) -> tuple[float, int, int, int]:
    spacing = _positive(layout["actual_max_spacing_m"], "actual_max_spacing_m")
    locations = _positive_integer(layout["selected_locations"], "selected_locations")
    per_location = _positive_integer(
        layout["anodes_per_location"], "anodes_per_location"
    )
    minimum = _positive_integer(
        layout.get("minimum_anodes_per_location", 1), "minimum_anodes_per_location"
    )
    return spacing, locations, per_location, minimum


def _flags(layout: Mapping[str, Any]) -> tuple[bool, bool, bool, bool, bool, bool]:
    high_demand = _boolean(
        layout["high_current_or_low_resistivity"],
        "high_current_or_low_resistivity",
    )
    mechanical_risk = _boolean(
        layout["mechanical_damage_risk"], "mechanical_damage_risk"
    )
    uniform = _boolean(
        layout["uniform_distribution_confirmed"], "uniform_distribution_confirmed"
    )
    bilge_safe = _boolean(layout["bilge_damage_avoided"], "bilge_damage_avoided")
    bilge_fitted = _boolean(layout["bilge_keel_fitted"], "bilge_keel_fitted")
    bilge_alternating = _boolean(
        layout.get("bilge_keel_alternation_confirmed", False),
        "bilge_keel_alternation_confirmed",
    )
    return (
        high_demand,
        mechanical_risk,
        uniform,
        bilge_safe,
        bilge_fitted,
        bilge_alternating,
    )


def _spacing_limit(layout: Mapping[str, Any], high: bool, mechanical: bool) -> float:
    if not mechanical:
        return 5.0 if high else 8.0
    limit = _positive(layout.get("project_spacing_limit_m"), "project_spacing_limit_m")
    if limit >= 5.0:
        raise ValueError("mechanical-damage spacing limit must be less than 5 m")
    return limit


def evaluate_layout(layout: Mapping[str, Any]) -> dict[str, Any]:
    """Evaluate ABS Section 3/5.2 general distribution requirements."""
    missing = _required_inputs(layout)
    if missing:
        return {
            "status": "NOT_EVALUATED",
            "reason": f"missing layout inputs: {', '.join(missing)}",
        }
    spacing, locations, per_location, minimum = _dimensions(layout)
    high, mechanical, uniform, bilge_safe, bilge_fitted, bilge_alternating = _flags(
        layout
    )
    if bilge_fitted and "bilge_keel_alternation_confirmed" not in layout:
        return {
            "status": "NOT_EVALUATED",
            "reason": "missing layout inputs: bilge_keel_alternation_confirmed",
        }
    limit = _spacing_limit(layout, high, mechanical)
    checks = {
        "spacing": spacing <= limit,
        "project_location_minimum": per_location >= minimum,
        "uniform_distribution": uniform,
        "bilge_damage_avoided": bilge_safe,
        "bilge_keel_alternation": not bilge_fitted or bilge_alternating,
    }
    return {
        "status": "PASS" if all(checks.values()) else "FAIL",
        "actual_max_spacing_m": spacing,
        "spacing_limit_m": limit,
        "selected_locations": locations,
        "anodes_per_location": per_location,
        "selected_anode_count": locations * per_location,
        "minimum_anodes_per_location": minimum,
        "minimum_layout_count": locations * minimum,
        "assessment_scope": "Section 3/5.2 general distribution and bilge spacing",
        "checks": checks,
        "special_geometry_checks": {
            "stern_propeller_openings": (
                "NOT_EVALUATED: applicability and geometry not supplied"
            )
        },
        "citation": "ABS GN Ships 2017 Section 3/5.2",
    }


__all__ = ["evaluate_layout"]
