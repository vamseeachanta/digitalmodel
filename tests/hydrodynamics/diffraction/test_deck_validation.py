"""Tests for license-free hydrostatic mesh validation (#896).

The Capytaine solve is exercised only when capytaine is importable; otherwise
the validator degrades gracefully (asserted) and the solve test is skipped.
"""

from __future__ import annotations

import importlib.util

import pytest

from digitalmodel.hydrodynamics.diffraction.deck_validation import validate_mesh
from digitalmodel.hydrodynamics.diffraction.vessel_deck_builder import (
    MESH_FILE,
    build_deck,
)
from digitalmodel.marine_ops.vessel_db.loader import Record

_HAS_CAPYTAINE = importlib.util.find_spec("capytaine") is not None


def _fpso(tmp_path):
    rec = Record(
        name="Val FPSO",
        scope="floating",
        layer="particulars",
        vessel_type="FPSO",
        raw_fields={
            "loa_m": 258.0,
            "beam_m": 46.0,
            "draft_m": 14.0,
            "depth_m": 28.0,
            "displacement_t": 160000.0,
        },
        citations=[{"fields": ["all"], "source": "test", "access": "public"}],
    )
    res = build_deck(rec, tmp_path)
    return res.deck_path.parent / MESH_FILE


def test_degrades_without_mesh_or_capytaine():
    out = validate_mesh("definitely_missing.gdf")
    # either capytaine missing (available False) or mesh missing (error) — never raises
    assert out.get("available") is False or "error" in out


@pytest.mark.skipif(not _HAS_CAPYTAINE, reason="capytaine not installed")
def test_parametric_fpso_mesh_is_physical(tmp_path):
    gdf = _fpso(tmp_path)
    res = validate_mesh(gdf, expected_waterplane_m2=258.0 * 46.0 * 0.95)
    assert res["available"] is True
    assert res["k33_positive"] is True, f"inverted normals? K33={res['k33']}"
    # heave stiffness within a plausible band of box waterplane theory
    assert 0.7 <= res["k33_ratio"] <= 1.2
    assert res["passed"] is True
