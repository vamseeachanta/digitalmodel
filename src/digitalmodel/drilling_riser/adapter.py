"""Drilling-riser component adapter — worldenergydata CSV → normalised dicts.

Mirrors the adapter pattern in ``naval_architecture.ship_data`` but operates
on drilling-riser component records that are already in imperial units.
"""

from __future__ import annotations

import csv
import math
from pathlib import Path
from typing import Any, Optional


# ── Constants ───────────────────────────────────────────────────────
_FT_PER_M: float = 1.0 / 0.3048
_KN_PER_KIP: float = 4.44822  # 1 kip-force = 4.44822 kN


# ── Helpers ─────────────────────────────────────────────────────────

def _safe_float(value: Any) -> Optional[float]:
    """Return *value* as a float if it is a finite number, else ``None``."""
    if value is None or isinstance(value, bool):
        return None
    try:
        f = float(value)
    except (TypeError, ValueError):
        return None
    if not math.isfinite(f):
        return None
    return f


def _safe_positive_float(value: Any) -> Optional[float]:
    """Return *value* as a positive float, else ``None``."""
    f = _safe_float(value)
    if f is not None and f > 0:
        return f
    return None


def _safe_nonneg_float(value: Any) -> Optional[float]:
    """Return *value* as a non-negative float (>= 0), else ``None``."""
    f = _safe_float(value)
    if f is not None and f >= 0:
        return f
    return None


def _safe_int(value: Any) -> Optional[int]:
    """Return *value* as an int if it represents a whole number, else ``None``."""
    f = _safe_float(value)
    if f is not None and f == int(f):
        return int(f)
    return None


# ── Normaliser ──────────────────────────────────────────────────────

# Numeric fields that must be positive (dimensions / weights / ratings).
_POSITIVE_FLOAT_FIELDS: list[tuple[str, str]] = [
    ("OD_IN", "od_in"),
    ("ID_IN", "id_in"),
    ("WALL_THICKNESS_IN", "wall_thickness_in"),
    ("LENGTH_FT", "length_ft"),
    ("WEIGHT_AIR_KIPS", "weight_air_kips"),
    ("PRESSURE_RATING_PSI", "pressure_rating_psi"),
    ("BUOYANCY_OD_IN", "buoyancy_od_in"),
    ("BORE_SIZE_IN", "bore_size_in"),
    ("HEIGHT_FT", "height_ft"),
    ("STIFFNESS_FT_KIP_PER_DEG", "stiffness_ft_kip_per_deg"),
    ("STROKE_FT", "stroke_ft"),
    ("OUTER_BARREL_LENGTH_FT", "outer_barrel_length_ft"),
    ("INNER_BARREL_LENGTH_FT", "inner_barrel_length_ft"),
]

# Numeric fields that may be negative (buoyant weight).
_FLOAT_FIELDS: list[tuple[str, str]] = [
    ("WEIGHT_WATER_KIPS", "weight_water_kips"),
]

# Percentage fields (0–100 range, positive).
_PCT_FIELDS: list[tuple[str, str]] = [
    ("BUOYANCY_COVERAGE_PCT", "buoyancy_coverage_pct"),
    ("MAX_ANGLE_DEG", "max_angle_deg"),
]

# Integer count fields.
_INT_FIELDS: list[tuple[str, str]] = [
    ("ANNULAR_COUNT", "annular_count"),
    ("SHEAR_RAM_COUNT", "shear_ram_count"),
    ("PIPE_RAM_COUNT", "pipe_ram_count"),
]

# String metadata fields.
_STR_FIELDS: list[tuple[str, str]] = [
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
]


def normalize_riser_component_record(row: dict[str, Any]) -> Optional[dict[str, Any]]:
    """Convert a raw CSV row into the normalised riser-component shape.

    Returns ``None`` if the row has no usable ``COMPONENT_ID``.
    """
    cid = row.get("COMPONENT_ID")
    if not isinstance(cid, str) or not cid.strip():
        return None

    entry: dict[str, Any] = {"component_id": cid.strip()}

    for src, dst in _POSITIVE_FLOAT_FIELDS:
        val = _safe_positive_float(row.get(src))
        if val is not None:
            entry[dst] = val

    for src, dst in _FLOAT_FIELDS:
        val = _safe_float(row.get(src))
        if val is not None:
            entry[dst] = val

    for src, dst in _PCT_FIELDS:
        val = _safe_nonneg_float(row.get(src))
        if val is not None:
            entry[dst] = val

    for src, dst in _INT_FIELDS:
        val = _safe_int(row.get(src))
        if val is not None:
            entry[dst] = val

    for src, dst in _STR_FIELDS:
        val = row.get(src)
        if isinstance(val, str) and val.strip():
            entry[dst] = val.strip()

    return entry


# ── Registration ────────────────────────────────────────────────────

def register_riser_components(csv_path: Path) -> list[dict[str, Any]]:
    """Read a drilling-riser component CSV and return normalised records.

    Skips rows that fail normalisation (missing ``COMPONENT_ID``).
    """
    components: list[dict[str, Any]] = []
    with open(csv_path, newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        for row in reader:
            rec = normalize_riser_component_record(row)
            if rec is not None:
                components.append(rec)
    return components


# ── Computation ─────────────────────────────────────────────────────

def compute_riser_string_weight_kn(
    components: list[dict[str, Any]],
    lengths_m: list[float],
) -> float:
    """Compute total riser-string weight in kN from component linear densities.

    For each *(component, length_m)* pair the weight contribution is::

        (weight_air_kips / length_ft) × (length_m × FT_PER_M) × KN_PER_KIP

    Components missing ``weight_air_kips`` or ``length_ft`` raise
    :class:`KeyError` so callers can decide how to handle incomplete data.

    Returns ``0.0`` for an empty component list.
    """
    if not components:
        return 0.0

    if len(components) != len(lengths_m):
        raise ValueError(
            f"components ({len(components)}) and lengths_m ({len(lengths_m)}) "
            f"must have the same length"
        )

    total_kn = 0.0
    for comp, length_m in zip(components, lengths_m):
        weight_air = comp["weight_air_kips"]
        length_ft = comp["length_ft"]
        if length_ft == 0:
            raise ValueError(
                f"Component {comp.get('component_id', '?')} has length_ft=0"
            )
        linear_density = weight_air / length_ft  # kips / ft
        total_kn += linear_density * (length_m * _FT_PER_M) * _KN_PER_KIP

    return total_kn
