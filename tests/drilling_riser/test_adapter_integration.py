"""Integration tests for the drilling-riser component adapter."""

from __future__ import annotations

import pytest

from digitalmodel.drilling_riser.adapter import (
    compute_riser_string_weight_kn,
    normalize_riser_component_record,
    register_riser_components,
)
from digitalmodel.drilling_riser.stackup import top_tension_required
from digitalmodel.drilling_riser.tool_passage import annular_clearance_mm

_KIPS_TO_KN = 4.44822
_IN_TO_MM = 25.4
_FT_TO_M = 0.3048
_PSI_TO_MPA = 0.00689476


def test_normalize_riser_joint_21in_bare() -> None:
    """Bare 21 in riser joint normalizes to SI-style fields."""
    record = {
        "COMPONENT_ID": "RJ-21-75-BARE",
        "COMPONENT_TYPE": "riser_joint",
        "OD_IN": 21.0,
        "ID_IN": 19.5,
        "WALL_THICKNESS_IN": 0.75,
        "LENGTH_FT": 75.0,
        "WEIGHT_AIR_KIPS": 22.5,
        "WEIGHT_WATER_KIPS": 3.8,
        "GRADE": "X-80",
        "PRESSURE_RATING_PSI": 5000.0,
    }

    result = normalize_riser_component_record(record)

    assert result is not None
    assert result["component_id"] == "RJ-21-75-BARE"
    assert result["component_type"] == "riser_joint"
    assert result["od_mm"] == pytest.approx(21.0 * _IN_TO_MM, rel=1e-6)
    assert result["id_mm"] == pytest.approx(19.5 * _IN_TO_MM, rel=1e-6)
    assert result["wall_thickness_mm"] == pytest.approx(0.75 * _IN_TO_MM, rel=1e-6)
    assert result["length_m"] == pytest.approx(75.0 * _FT_TO_M, rel=1e-6)
    assert result["submerged_weight_kn"] == pytest.approx(3.8 * _KIPS_TO_KN, rel=1e-6)
    assert result["pressure_mpa"] == pytest.approx(5000.0 * _PSI_TO_MPA, rel=1e-6)


def test_normalize_bop_record() -> None:
    """BOP records normalize their SI bore, height, and submerged weight fields."""
    record = {
        "COMPONENT_ID": "BOP-SUB-18.75-15K",
        "COMPONENT_TYPE": "bop",
        "WEIGHT_AIR_KIPS": 400.0,
        "WEIGHT_WATER_KIPS": 340.0,
        "BORE_SIZE_IN": 18.75,
        "HEIGHT_FT": 35.0,
        "PRESSURE_RATING_PSI": 15000.0,
    }

    result = normalize_riser_component_record(record)

    assert result is not None
    assert result["component_type"] == "bop"
    assert result["submerged_weight_kn"] == pytest.approx(340.0 * _KIPS_TO_KN, rel=1e-6)
    assert result["height_m"] == pytest.approx(35.0 * _FT_TO_M, rel=1e-6)
    assert result["bore_size_mm"] == pytest.approx(18.75 * _IN_TO_MM, rel=1e-6)


def test_normalize_returns_none_for_missing_id() -> None:
    """Records without COMPONENT_ID are skipped."""
    assert (
        normalize_riser_component_record({"COMPONENT_TYPE": "riser_joint", "OD_IN": 21.0})
        is None
    )


def test_register_riser_components_all_36(
    all_csv_records: list[dict[str, str]],
) -> None:
    """All CSV rows are accounted for during batch registration."""
    registry: dict[str, dict[str, object]] = {}

    added, skipped = register_riser_components(all_csv_records, registry=registry)

    assert added + skipped == 36
    assert added == 36
    assert skipped == 0
    assert "RJ-21-75-BARE" in registry
    assert registry["RJ-21-75-BARE"]["length_m"] == pytest.approx(75.0 * _FT_TO_M, rel=1e-6)


def test_riser_string_weight_20_bare_joints() -> None:
    """String weight uses submerged weights directly, without per-length recomputation."""
    components = [{"component_id": f"joint-{idx}", "submerged_weight_kn": 3.8 * _KIPS_TO_KN} for idx in range(20)]

    total = compute_riser_string_weight_kn(components)

    assert total == pytest.approx(20 * 3.8 * _KIPS_TO_KN, rel=1e-6)
    assert top_tension_required(total) == pytest.approx(total * 1.25, rel=1e-6)


def test_tool_passage_real_specs() -> None:
    """Real riser-joint dimensions still feed tool-passage calculations correctly."""
    clearance = annular_clearance_mm(19.5 * _IN_TO_MM, 12.25 * _IN_TO_MM)
    assert clearance == pytest.approx(92.075, rel=1e-6)
