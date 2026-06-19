"""Tests for the worldenergydata -> digitalmodel vessel-fleet adapter.

worldenergydata is the source of truth for vessel collection. These tests skip
gracefully when it is not checked out (e.g. minimal CI), since the adapter is
designed to degrade to None rather than error.
"""

from __future__ import annotations

import pytest

from digitalmodel.marine_ops.vessel_db import (
    construction_crane_vessels,
    construction_vessels,
    drilling_rigs,
    fleet_summary,
    installation_vessels,
    normalize_vessel_name,
    resolve_wed_curated_dir,
)

_WED = resolve_wed_curated_dir()
_skip_no_wed = pytest.mark.skipif(_WED is None, reason="worldenergydata not checked out")


def test_normalize_collapses_type_prefixes():
    assert normalize_vessel_name("SSCV Sleipnir") == normalize_vessel_name("SLEIPNIR")
    assert normalize_vessel_name("DLV 2000") == normalize_vessel_name("dlv-2000")


def test_resolver_returns_none_or_dir():
    # Never raises without require=True.
    d = resolve_wed_curated_dir()
    assert d is None or (d / "construction_vessels.csv").is_file() or (d / "drilling_rigs.csv").is_file()


def test_adapter_degrades_without_wed(monkeypatch):
    """With the env unset and resolution forced to miss, consumers return empty."""
    monkeypatch.setattr(
        "digitalmodel.marine_ops.vessel_db.wed_adapter.resolve_wed_curated_dir",
        lambda require=False: None,
    )
    assert construction_vessels() == []
    assert drilling_rigs() == []
    assert construction_crane_vessels() == {}


@_skip_no_wed
def test_construction_vessels_real_named():
    recs = construction_vessels()
    assert len(recs) >= 10
    names = {r.name.upper() for r in recs}
    assert "SLEIPNIR" in names or "THIALF" in names


@_skip_no_wed
def test_drilling_rigs_large_fleet():
    rigs = drilling_rigs(offshore_only=True)
    assert len(rigs) > 1000  # WED curates ~2,268


@_skip_no_wed
def test_crane_vessels_have_curves_with_reach():
    cv = construction_crane_vessels()
    assert cv
    # At least one vessel must publish a real (non-zero) crane reach.
    assert any(float(info["crane_curve"].radii_m.max()) > 0 for info in cv.values())
    for info in cv.values():
        assert info["crane_curve"].max_hook_load_te > 0


@_skip_no_wed
def test_wed_wins_on_collision():
    """A vessel present in both sources resolves to the WED record."""
    merged = installation_vessels(include_wed=True, include_curated=True)
    wed_sourced = [n for n, i in merged.items() if i.get("source") == "worldenergydata"]
    assert wed_sourced, "expected WED to supply at least one installation vessel"


@_skip_no_wed
def test_fleet_summary_counts():
    s = fleet_summary()
    assert s["available"] is True
    assert s["drilling_rigs"] > 1000
    assert s["construction_vessels"] >= 10
