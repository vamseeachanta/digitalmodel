"""Tests for the worldenergydata -> digitalmodel vessel-fleet adapter.

worldenergydata is the source of truth for vessel collection. These tests skip
gracefully when it is not checked out (e.g. minimal CI), since the adapter is
designed to degrade to None rather than error.
"""

from __future__ import annotations

import logging
from pathlib import Path

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
from digitalmodel.marine_ops.vessel_db import wed_adapter

_WED = resolve_wed_curated_dir()
_skip_no_wed = pytest.mark.skipif(_WED is None, reason="worldenergydata not checked out")

_MIN_CSV = (
    "VESSEL_NAME,VESSEL_TYPE,OWNER,YEAR_BUILT,IMO_NUMBER,"
    "MAIN_CRANE_CAPACITY_T,MAIN_CRANE_REACH_M,DP_CLASS,DATA_SOURCE,DATA_SOURCE_URL\n"
    "Test Lifter,Heavy Lift Vessel,TestCo,2020,1234567,"
    "5000,40,DP3,unit-test,https://example.test\n"
)


def _make_curated(d: Path) -> Path:
    """Create a curated dir holding a minimal construction_vessels.csv."""
    d.mkdir(parents=True, exist_ok=True)
    (d / "construction_vessels.csv").write_text(_MIN_CSV)
    return d


@pytest.fixture
def isolated_resolution(monkeypatch):
    """Blind the resolver to env vars and real local WED checkouts."""
    monkeypatch.delenv("WED_VESSEL_FLEET_PATH", raising=False)
    monkeypatch.delenv("WORLDENERGYDATA_ROOT", raising=False)
    monkeypatch.setattr(wed_adapter, "_local_wed_roots", lambda: [])
    return monkeypatch


def test_normalize_collapses_type_prefixes():
    assert normalize_vessel_name("SSCV Sleipnir") == normalize_vessel_name("SLEIPNIR")
    assert normalize_vessel_name("DLV 2000") == normalize_vessel_name("dlv-2000")


def test_resolver_returns_none_or_dir():
    # Never raises without require=True.
    d = resolve_wed_curated_dir()
    assert d is None or (d / "construction_vessels.csv").is_file() or (d / "drilling_rigs.csv").is_file()


def test_resolution_order_env_then_legacy_then_package(tmp_path, isolated_resolution):
    """Probe order: WED_VESSEL_FLEET_PATH -> legacy data/ layout -> package layout."""
    monkeypatch = isolated_resolution
    root = tmp_path / "wed"
    legacy = root / wed_adapter._CURATED_REL
    package = root / wed_adapter._CURATED_REL_PACKAGE
    monkeypatch.setattr(wed_adapter, "_local_wed_roots", lambda: [root])

    # Nothing on disk -> None (no exception).
    assert wed_adapter.resolve_wed_curated_dir() is None

    # Package layout only (the current WED monorepo split) -> found there.
    _make_curated(package)
    assert wed_adapter.resolve_wed_curated_dir() == package

    # Legacy layout appears too -> legacy wins (backwards compatible).
    _make_curated(legacy)
    assert wed_adapter.resolve_wed_curated_dir() == legacy

    # WORLDENERGYDATA_ROOT beats local-clone fallbacks (package layout under it).
    root2 = tmp_path / "wed2"
    pkg2 = _make_curated(root2 / wed_adapter._CURATED_REL_PACKAGE)
    monkeypatch.setenv("WORLDENERGYDATA_ROOT", str(root2))
    assert wed_adapter.resolve_wed_curated_dir() == pkg2

    # WED_VESSEL_FLEET_PATH beats everything, pointing straight at a curated dir…
    direct = _make_curated(tmp_path / "direct")
    monkeypatch.setenv("WED_VESSEL_FLEET_PATH", str(direct))
    assert wed_adapter.resolve_wed_curated_dir() == direct

    # …or at a WED repo root laid out package-style.
    root3 = tmp_path / "wed3"
    pkg3 = _make_curated(root3 / wed_adapter._CURATED_REL_PACKAGE)
    monkeypatch.setenv("WED_VESSEL_FLEET_PATH", str(root3))
    assert wed_adapter.resolve_wed_curated_dir() == pkg3


def test_missing_curated_dir_warns_but_returns_none(tmp_path, isolated_resolution, caplog):
    monkeypatch = isolated_resolution
    monkeypatch.setattr(wed_adapter, "_local_wed_roots", lambda: [tmp_path / "nowhere"])
    with caplog.at_level(logging.WARNING, logger=wed_adapter.__name__):
        assert wed_adapter.resolve_wed_curated_dir() is None
    assert "curated vessel fleet not found" in caplog.text
    # require=True still raises an actionable error instead of warning.
    with pytest.raises(FileNotFoundError, match="WED_VESSEL_FLEET_PATH"):
        wed_adapter.resolve_wed_curated_dir(require=True)


def test_installation_vessels_merges_wed_fixture(tmp_path, isolated_resolution):
    """installation_vessels(include_wed=True) merges rows from a fixture curated dir."""
    monkeypatch = isolated_resolution
    curated = _make_curated(tmp_path / "curated")
    monkeypatch.setenv("WED_VESSEL_FLEET_PATH", str(curated))

    merged = installation_vessels(include_wed=True, include_curated=True)
    assert "Test Lifter" in merged
    info = merged["Test Lifter"]
    assert info["source"] == "worldenergydata"
    assert info["crane_curve"].max_hook_load_te == pytest.approx(5000.0)
    assert float(info["crane_curve"].radii_m.max()) == pytest.approx(40.0)
    # The in-repo curated records are still merged alongside the WED rows.
    assert any(i.get("source") == "curated" for i in merged.values())


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
