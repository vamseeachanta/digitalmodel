"""Drilling-riser component adapter — worldenergydata CSV → normalized SI dicts."""

from __future__ import annotations

import csv
import math
from collections.abc import Iterable, Mapping
from pathlib import Path
from typing import Any

_KIPS_TO_KN = 4.44822
_IN_TO_MM = 25.4
_FT_TO_M = 0.3048
_PSI_TO_MPA = 0.00689476
_FT_KIP_PER_DEG_TO_KN_M_PER_DEG = _FT_TO_M * _KIPS_TO_KN

_REGISTRY: dict[str, dict[str, Any]] = {}


def _safe_float(value: Any) -> float | None:
    """Return *value* as a finite float, else ``None``."""
    if value is None or isinstance(value, bool):
        return None
    try:
        numeric = float(value)
    except (TypeError, ValueError):
        return None
    if not math.isfinite(numeric):
        return None
    return numeric


def _safe_positive_float(value: Any) -> float | None:
    """Return *value* as a positive float, else ``None``."""
    numeric = _safe_float(value)
    if numeric is not None and numeric > 0:
        return numeric
    return None


def _safe_nonneg_float(value: Any) -> float | None:
    """Return *value* as a non-negative float, else ``None``."""
    numeric = _safe_float(value)
    if numeric is not None and numeric >= 0:
        return numeric
    return None


def _safe_int(value: Any) -> int | None:
    """Return *value* as an int if it represents a whole finite number."""
    numeric = _safe_float(value)
    if numeric is not None and numeric == int(numeric):
        return int(numeric)
    return None


def _convert(value: Any, factor: float, *, positive: bool = False) -> float | None:
    """Convert a numeric value by *factor* if it passes validation."""
    numeric = _safe_positive_float(value) if positive else _safe_float(value)
    if numeric is None:
        return None
    return numeric * factor


_SI_FLOAT_FIELDS: tuple[tuple[str, str, float, bool], ...] = (
    ("OD_IN", "od_mm", _IN_TO_MM, True),
    ("ID_IN", "id_mm", _IN_TO_MM, True),
    ("WALL_THICKNESS_IN", "wall_thickness_mm", _IN_TO_MM, True),
    ("LENGTH_FT", "length_m", _FT_TO_M, True),
    ("WEIGHT_AIR_KIPS", "weight_air_kn", _KIPS_TO_KN, True),
    ("WEIGHT_WATER_KIPS", "submerged_weight_kn", _KIPS_TO_KN, False),
    ("BUOYANCY_OD_IN", "buoyancy_od_mm", _IN_TO_MM, True),
    ("PRESSURE_RATING_PSI", "pressure_mpa", _PSI_TO_MPA, True),
    ("BORE_SIZE_IN", "bore_size_mm", _IN_TO_MM, True),
    ("HEIGHT_FT", "height_m", _FT_TO_M, True),
    (
        "STIFFNESS_FT_KIP_PER_DEG",
        "stiffness_kn_m_per_deg",
        _FT_KIP_PER_DEG_TO_KN_M_PER_DEG,
        True,
    ),
    ("STROKE_FT", "stroke_m", _FT_TO_M, True),
    ("OUTER_BARREL_LENGTH_FT", "outer_barrel_length_m", _FT_TO_M, True),
    ("INNER_BARREL_LENGTH_FT", "inner_barrel_length_m", _FT_TO_M, True),
)

_NONNEG_FLOAT_FIELDS: tuple[tuple[str, str], ...] = (
    ("BUOYANCY_COVERAGE_PCT", "buoyancy_coverage_pct"),
    ("MAX_ANGLE_DEG", "max_angle_deg"),
)

_INT_FIELDS: tuple[tuple[str, str], ...] = (
    ("ANNULAR_COUNT", "annular_count"),
    ("SHEAR_RAM_COUNT", "shear_ram_count"),
    ("PIPE_RAM_COUNT", "pipe_ram_count"),
)

_STR_FIELDS: tuple[tuple[str, str], ...] = (
    ("COMPONENT_TYPE", "component_type"),
    ("MANUFACTURER", "manufacturer"),
    ("MODEL", "model"),
    ("GRADE", "grade"),
    ("CONNECTION_TYPE", "connection_type"),
    ("BOP_TYPE", "bop_type"),
    ("CASING_SHEAR_RAM", "casing_shear_ram"),
    ("POSITION", "position"),
    ("CONNECTOR_TYPE", "connector_type"),
    ("DATA_SOURCE", "data_source"),
    ("NOTES", "notes"),
)


def normalize_riser_component_record(record: Mapping[str, Any]) -> dict[str, Any] | None:
    """Convert one CSV row to a calculation-ready SI dict.

    Returns ``None`` if ``COMPONENT_ID`` is missing or blank.
    """
    component_id = record.get("COMPONENT_ID")
    if not isinstance(component_id, str) or not component_id.strip():
        return None

    entry: dict[str, Any] = {"component_id": component_id.strip()}

    for src, dst in _STR_FIELDS:
        value = record.get(src)
        if isinstance(value, str) and value.strip():
            entry[dst] = value.strip()

    for src, dst, factor, positive in _SI_FLOAT_FIELDS:
        converted = _convert(record.get(src), factor, positive=positive)
        if converted is not None:
            entry[dst] = converted

    for src, dst in _NONNEG_FLOAT_FIELDS:
        value = _safe_nonneg_float(record.get(src))
        if value is not None:
            entry[dst] = value

    for src, dst in _INT_FIELDS:
        value = _safe_int(record.get(src))
        if value is not None:
            entry[dst] = value

    return entry


def _iter_records(
    records: Iterable[Mapping[str, Any]] | Path,
) -> Iterable[Mapping[str, Any]]:
    """Yield riser records from an iterable or a CSV path."""
    if isinstance(records, Path):
        with records.open(newline="", encoding="utf-8") as handle:
            yield from csv.DictReader(handle)
        return
    yield from records


def register_riser_components(
    records: Iterable[Mapping[str, Any]] | Path,
    *,
    registry: dict[str, dict[str, Any]] | None = None,
) -> tuple[int, int]:
    """Batch-normalize records and merge them into a registry.

    Returns ``(added_count, skipped_count)``.
    """
    target_registry = _REGISTRY if registry is None else registry
    added = 0
    skipped = 0

    for record in _iter_records(records):
        entry = normalize_riser_component_record(record)
        if entry is None:
            skipped += 1
            continue
        target_registry[entry["component_id"]] = entry
        added += 1

    return added, skipped


def compute_riser_string_weight_kn(components: Iterable[Mapping[str, Any]]) -> float:
    """Sum submerged weight across all components in kN."""
    total = 0.0
    for component in components:
        if "submerged_weight_kn" not in component:
            raise ValueError(
                f"Component {component.get('component_id', '?')} lacks submerged_weight_kn"
            )
        total += float(component["submerged_weight_kn"])
    return total
