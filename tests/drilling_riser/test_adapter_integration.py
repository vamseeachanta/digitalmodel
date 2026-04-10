"""Integration tests for the drilling-riser component adapter."""

from __future__ import annotations

import pytest

from digitalmodel.drilling_riser.adapter import (
    compute_riser_string_weight_kn,
    normalize_riser_component_record,
    register_riser_components,
)

# ── Test: normalise a single record ─────────────────────────────────


def test_normalize_single_record(raw_csv_rows: list[dict]) -> None:
    """First CSV row (RJ-21-75-BARE) normalises to expected values."""
    row = raw_csv_rows[0]
    rec = normalize_riser_component_record(row)
    assert rec is not None
    assert rec["component_id"] == "RJ-21-75-BARE"
    assert rec["component_type"] == "riser_joint"
    assert rec["od_in"] == pytest.approx(21.0)
    assert rec["length_ft"] == pytest.approx(75.0)
    assert rec["weight_air_kips"] == pytest.approx(22.5)


# ── Test: register loads CSV (expect 36) ────────────────────────────


def test_register_loads_csv(csv_path) -> None:
    """register_riser_components returns exactly 36 records."""
    components = register_riser_components(csv_path)
    assert len(components) == 36


# ── Test: normalised schema keys ────────────────────────────────────

_REQUIRED_KEYS = {"component_id", "component_type"}


def test_normalized_schema_keys(raw_csv_rows: list[dict]) -> None:
    """Every normalised record contains at least component_id and component_type."""
    for row in raw_csv_rows:
        rec = normalize_riser_component_record(row)
        if rec is None:
            continue
        assert _REQUIRED_KEYS.issubset(
            rec.keys()
        ), f"Missing keys in {rec.get('component_id')}"


# ── Test: string weight basic case ──────────────────────────────────

_KN_PER_KIP = 4.44822
_FT_PER_M = 1.0 / 0.3048


def test_string_weight_basic_case() -> None:
    """Two identical 75-ft joints at 100 m each yield correct kN."""
    comp = {
        "component_id": "RJ-21-75-BARE",
        "weight_air_kips": 22.5,
        "length_ft": 75.0,
    }
    components = [comp, comp]
    lengths_m = [100.0, 100.0]

    result = compute_riser_string_weight_kn(components, lengths_m)

    # Expected: 2 × (22.5/75) × (100 × FT_PER_M) × KN_PER_KIP
    linear_density = 22.5 / 75.0  # kips/ft
    expected = 2 * linear_density * (100.0 * _FT_PER_M) * _KN_PER_KIP
    assert result == pytest.approx(expected, rel=1e-6)


# ── Test: empty list returns 0.0 ────────────────────────────────────


def test_string_weight_empty_list() -> None:
    """Empty component list returns 0.0."""
    assert compute_riser_string_weight_kn([], []) == 0.0


# ── Test: missing field behaviour ───────────────────────────────────


def test_string_weight_missing_field() -> None:
    """Components missing weight_air_kips or length_ft raise KeyError."""
    incomplete = [{"component_id": "MISSING"}]
    with pytest.raises(KeyError):
        compute_riser_string_weight_kn(incomplete, [100.0])
