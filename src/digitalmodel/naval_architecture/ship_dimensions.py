# ABOUTME: Ship dimensions template loader — populate registry from YAML
# ABOUTME: Validates entries and normalizes them into the ship_data registry
"""
Ship dimensions template loading and validation.

Loads vessel principal dimensions from YAML templates (filled manually
or extracted from ship plans) and merges them into the ``ship_data``
registry used by the naval architecture knowledge modules.

Usage:
    uv run python -c "
    from digitalmodel.naval_architecture.ship_dimensions import (
        load_dimension_template, merge_template_into_registry,
    )
    vessels = load_dimension_template()
    added, skipped = merge_template_into_registry(vessels)
    "
"""

from __future__ import annotations

from pathlib import Path
from typing import Any, Mapping

import yaml

from digitalmodel.naval_architecture.ship_data import _SHIPS, get_ship

_DEFAULT_TEMPLATE_PATH = (
    Path(__file__).resolve().parents[3] / "data" / "ship_dimensions_template.yaml"
)
_REQUIRED_FIELDS = ("hull_id", "name", "loa_ft", "beam_ft", "draft_ft")
_POSITIVE_NUMERIC_FIELDS = (
    "loa_ft",
    "lwl_ft",
    "beam_ft",
    "draft_ft",
    "displacement_lt",
    "full_load_lt",
    "kg_ft",
    "km_ft",
    "gm_ft",
)

__all__ = [
    "default_dimension_template_path",
    "load_dimension_template",
    "validate_vessel_entry",
    "merge_template_into_registry",
]


def default_dimension_template_path() -> Path:
    """Return the repository template shipped for manual ship-dimension entry."""
    return _DEFAULT_TEMPLATE_PATH


def load_dimension_template(path: str | Path | None = None) -> list[dict[str, Any]]:
    """Load vessel dimensions from a YAML template file.

    Supports either the checked-in repository template or a custom YAML file.
    The YAML root may be either ``{"vessels": [...]}`` or a raw list of vessel
    dictionaries.
    """
    template_path = (
        Path(path).expanduser() if path is not None else default_dimension_template_path()
    )
    if not template_path.exists():
        raise FileNotFoundError(f"Ship dimensions template not found: {template_path}")

    with template_path.open(encoding="utf-8") as stream:
        data = yaml.safe_load(stream)

    if data is None:
        return []
    if isinstance(data, list):
        vessels = data
    elif isinstance(data, dict):
        vessels = data.get("vessels", [])
    else:
        raise ValueError(
            "Ship dimensions template must contain a 'vessels' mapping or a list of vessels."
        )

    if not isinstance(vessels, list):
        raise ValueError("'vessels' must be a list of vessel entries.")

    normalized_vessels: list[dict[str, Any]] = []
    for index, vessel in enumerate(vessels, start=1):
        if not isinstance(vessel, dict):
            raise ValueError(f"Vessel entry #{index} must be a mapping, got {type(vessel)!r}.")
        normalized_vessels.append(dict(vessel))

    return normalized_vessels


def validate_vessel_entry(vessel: Mapping[str, Any]) -> list[str]:
    """Validate a vessel dimension entry.

    Returns a list of error strings. An empty list means the entry is valid.
    ``hull_number`` is accepted as an alias for ``hull_id`` for consistency
    with the in-memory ship registry.
    """
    errors: list[str] = []
    hull_id = _resolve_hull_id(vessel)
    if not hull_id:
        errors.append("Missing required field: hull_id")

    name = vessel.get("name")
    if not isinstance(name, str) or not name.strip():
        errors.append("Missing required field: name")

    for field in ("loa_ft", "beam_ft", "draft_ft"):
        value = vessel.get(field)
        if value is None:
            errors.append(f"Missing required field: {field}")
        elif not _is_positive_number(value):
            errors.append(f"{field} must be positive, got {value}")

    for field in _POSITIVE_NUMERIC_FIELDS:
        value = vessel.get(field)
        if value is not None and not _is_positive_number(value):
            errors.append(f"{field} must be positive, got {value}")

    cb = vessel.get("cb")
    if cb is not None:
        if not _is_number(cb) or not 0 < float(cb) <= 1:
            errors.append(f"cb must be between 0 and 1, got {cb}")

    displacement = vessel.get("displacement_lt")
    full_load = vessel.get("full_load_lt")
    if (
        _is_positive_number(displacement)
        and _is_positive_number(full_load)
        and float(full_load) < float(displacement)
    ):
        errors.append("full_load_lt must be greater than or equal to displacement_lt")

    return errors


def merge_template_into_registry(
    vessels: list[Mapping[str, Any]], *, overwrite: bool = False
) -> tuple[int, int]:
    """Merge validated vessel entries into the ``ship_data`` registry.

    Returns ``(added_count, skipped_count)``.

    By default, vessels that already exist in the registry are skipped.
    Set ``overwrite=True`` to replace an existing registry entry.
    """
    added = 0
    skipped = 0

    for vessel in vessels:
        hull_id = _resolve_hull_id(vessel)
        if not hull_id:
            skipped += 1
            continue

        if get_ship(hull_id) is not None and not overwrite:
            skipped += 1
            continue

        errors = validate_vessel_entry(vessel)
        if errors:
            skipped += 1
            continue

        _SHIPS[hull_id] = _normalize_registry_entry(vessel, hull_id)
        added += 1

    return added, skipped


def _resolve_hull_id(vessel: Mapping[str, Any]) -> str:
    """Return a normalized hull ID from either ``hull_id`` or ``hull_number``."""
    hull_id = vessel.get("hull_id", vessel.get("hull_number"))
    if not isinstance(hull_id, str):
        return ""
    return hull_id.strip()


def _normalize_registry_entry(vessel: Mapping[str, Any], hull_id: str) -> dict[str, Any]:
    """Convert a template entry into the registry schema used by ``ship_data``."""
    entry = {
        key: value
        for key, value in dict(vessel).items()
        if key not in {"hull_id", "hull_number", "source"}
    }
    if isinstance(entry.get("name"), str):
        entry["name"] = entry["name"].strip()
    entry["hull_number"] = hull_id
    return entry


def _is_number(value: Any) -> bool:
    """Return True for real numeric values, excluding booleans."""
    return isinstance(value, (int, float)) and not isinstance(value, bool)


def _is_positive_number(value: Any) -> bool:
    """Return True for positive real numeric values."""
    return _is_number(value) and float(value) > 0
