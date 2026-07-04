"""#1346 (C3): the drilling-riser von-Mises dynamic-amplification STUB library.

Verifies the committed stub atlas round-trips, self-identifies as a STUB at both
the status and query surfaces, escalates outside coverage, and — critically —
that bumping the declared solver version off "STUB" makes the committed stub
read STALE (the self-surfacing tripwire that prompts a real licensed run).
"""
from __future__ import annotations

import pytest

from digitalmodel.parametric import query, refresh
from digitalmodel.parametric.atlas import Atlas
from digitalmodel.parametric.query import DEFAULT_ATLAS_ROOT

BASENAME = "drilling_riser_envelope"
_COVERED = {"mode": "drilling", "offset_pct": 2.0, "current_speed_mps": 1.0, "hs_m": 3.0, "tp_s": 12.0}


def _atlas() -> Atlas:
    return Atlas.load(DEFAULT_ATLAS_ROOT, BASENAME)


def test_committed_stub_atlas_round_trips():
    atlas = _atlas()
    assert atlas.response == "von_mises_daf"
    assert len(atlas.grid) == 3 * 5 * 5 * 4 * 3  # modes x offset x current x Hs x Tp
    assert atlas.provenance["kind"] == "library"
    assert atlas.provenance["solver"]["licensed"] is False
    assert atlas.provenance["solver"]["version"] == "STUB"
    assert atlas.categorical_axis.name == "mode"


def test_library_status_reports_current_stub():
    status = refresh.library_status(BASENAME)
    assert status["stale"] is False
    assert "STUB" in status["reason"]


def test_version_bump_makes_stub_read_stale(monkeypatch):
    # The self-surfacing tripwire: declare a real solver version and the
    # committed STUB immediately reads stale -> escalate -> prompts a real run.
    bumped = dict(refresh.LIBRARY_EXPECTATIONS[BASENAME], solver_version="2025.1-licensed")
    monkeypatch.setitem(refresh.LIBRARY_EXPECTATIONS, BASENAME, bumped)
    status = refresh.library_status(BASENAME)
    assert status["stale"] is True
    assert "version" in status["reason"]


def test_query_covered_point_carries_stub_solver_block():
    result = query._handle_value(_atlas(), _COVERED)
    assert result["in_range"] is True
    assert result["value"] >= 1.0  # a dynamic amplification factor
    # governance: the STUB self-identifies at the query surface (#1346 fix)
    assert result["provenance"]["solver"]["licensed"] is False
    assert result["provenance"]["solver"]["version"] == "STUB"


def test_query_unknown_mode_escalates():
    result = query._handle_value(_atlas(), {**_COVERED, "mode": "survival"})
    assert result["in_range"] is False
    assert result["action"] == "escalate"


def test_query_out_of_grid_escalates_never_extrapolates():
    result = query._handle_value(_atlas(), {**_COVERED, "offset_pct": 99.0})
    assert result["in_range"] is False
    assert result["action"] == "escalate"
    assert result["value"] is None


def test_stale_library_escalates_at_query_router(monkeypatch):
    bumped = dict(refresh.LIBRARY_EXPECTATIONS[BASENAME], solver_version="2025.1-licensed")
    monkeypatch.setitem(refresh.LIBRARY_EXPECTATIONS, BASENAME, bumped)
    reason = query._staleness(_atlas())
    assert reason is not None and "stale" in reason.lower()
