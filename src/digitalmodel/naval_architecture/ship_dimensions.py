# ABOUTME: Ship dimensions template loader — populate registry from YAML
# ABOUTME: Validates entries and merges into ship_data registry
"""
Ship dimensions template loading and validation.

Loads vessel principal dimensions from YAML templates (filled manually
or extracted from ship plans) and merges them into the ship_data registry.

Usage:
    uv run python -c "
    from digitalmodel.naval_architecture.ship_dimensions import (
        load_dimension_template, merge_template_into_registry,
    )
    vessels = load_dimension_template('ship_dimensions.yaml')
    added, skipped = merge_template_into_registry(vessels)
    "
"""

from pathlib import Path

import yaml

from digitalmodel.naval_architecture.ship_data import _SHIPS, get_ship

_REQUIRED_FIELDS = ["hull_id", "name", "loa_ft", "beam_ft", "draft_ft"]
_DIMENSION_FIELDS = [
    "loa_ft", "lwl_ft", "beam_ft", "draft_ft",
    "displacement_lt", "full_load_lt",
]


def load_dimension_template(path: str) -> list[dict]:
    """Load vessel dimensions from a YAML template file."""
    with open(path) as f:
        data = yaml.safe_load(f) or {}
    return data.get("vessels", [])


def validate_vessel_entry(vessel: dict) -> list[str]:
    """Validate a vessel dimension entry. Returns list of error strings."""
    errors = []
    for field in _REQUIRED_FIELDS:
        if field not in vessel:
            errors.append(f"Missing required field: {field}")

    for field in _DIMENSION_FIELDS:
        val = vessel.get(field)
        if val is not None and (not isinstance(val, (int, float)) or val <= 0):
            errors.append(f"{field} must be positive, got {val}")

    return errors


def merge_template_into_registry(vessels: list[dict]) -> tuple[int, int]:
    """Merge validated vessel entries into the ship_data registry.

    Returns (added_count, skipped_count).
    Skips vessels that already exist in the registry.
    """
    added = 0
    skipped = 0

    for vessel in vessels:
        hull_id = vessel.get("hull_id", "")
        if not hull_id:
            skipped += 1
            continue

        if get_ship(hull_id) is not None:
            skipped += 1
            continue

        errors = validate_vessel_entry(vessel)
        if errors:
            skipped += 1
            continue

        entry = {k: v for k, v in vessel.items() if k != "source"}
        _SHIPS[hull_id] = entry
        added += 1

    return added, skipped
