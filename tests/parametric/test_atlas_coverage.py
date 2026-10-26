"""System-wide invariants over EVERY committed parametric atlas.

Generalises the hand-picked checks in test_refresh.py to all parametric
workflows, so a newly added atlas that lacks a query handler, drifts, or fails to
interpolate a mid-grid point fails CI here — rather than NotImplementedError-ing
at query time. See docs/plans/parametric-refresh/adding-an-atlas.md.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.parametric import refresh
from digitalmodel.parametric.atlas import Atlas
from digitalmodel.parametric.generate import RESPONSE_FUNCS
from digitalmodel.parametric.query import _HANDLERS


def _rows_by_id() -> dict[str, dict]:
    return {row["id"]: row for row in refresh._registry_rows()}


def _basename(wid: str) -> str:
    return _rows_by_id()[wid]["basename"]


@pytest.mark.parametrize("basename", sorted(RESPONSE_FUNCS))
def test_every_response_evaluates_a_registered_point(basename, tmp_path, monkeypatch):
    # Supply citation metadata as in the watch-circle workflow tests; no
    # standard values or numerical responses are replaced by this fixture.
    page = tmp_path / "wikis/engineering-standards/wiki/standards/dnv-rp-0360.md"
    page.parent.mkdir(parents=True)
    page.write_text("---\ncode_id: DNV-RP-0360\npublisher: DNV\nrevision: 2016\n---\n")
    monkeypatch.setenv("LLM_WIKI_PATH", str(tmp_path))
    row = next(row for row in refresh._registry_rows() if row["basename"] == basename)
    block = row["parametric"]
    point = {axis["name"]: axis.get("values", axis.get("grid"))[0]
             for axis in block["axes"]}
    value = RESPONSE_FUNCS[basename](point, **block.get("response_kwargs", {}))
    assert math.isfinite(value)
    assert value >= 0


@pytest.mark.parametrize("scf", [1.0, 1.15])
def test_riser_response_preserves_reference_damage(scf):
    value = RESPONSE_FUNCS["riser_fatigue"]({"scf": scf, "viv_stress_scale": 1.0})
    assert value == pytest.approx({1.0: 2.93e-4, 1.15: 5.37e-4}[scf], rel=0.005)


def test_some_atlases_are_registered():
    # guards against the discovery itself silently returning nothing
    assert len(refresh.parametric_workflow_ids()) >= 15


def test_every_parametric_atlas_has_a_query_handler():
    """Every workflow that owns an atlas must be serveable by parametric_query."""
    missing = [
        (wid, _basename(wid))
        for wid in refresh.parametric_workflow_ids()
        if _basename(wid) not in _HANDLERS
    ]
    assert not missing, f"parametric atlases without a _HANDLERS entry: {missing}"


def test_every_committed_atlas_is_current():
    """No committed atlas may be stale relative to its build basis."""
    stale = []
    for wid in refresh.parametric_workflow_ids():
        status = refresh.refresh_status(wid)
        if status.get("stale"):
            stale.append((wid, status.get("reason")))
    assert not stale, f"stale committed atlases: {stale}"


def test_every_atlas_interpolates_a_mid_grid_point_in_range():
    """Each committed atlas loads and predicts an interior point in-range — the
    grid is complete and the interpolator is well-formed for every atlas."""
    failures = []
    for wid in refresh.parametric_workflow_ids():
        atlas = Atlas.load(refresh.DEFAULT_ATLAS_ROOT, _basename(wid))
        point = {}
        for axis in atlas.axes:
            if axis.is_categorical:
                point[axis.name] = axis.values[len(axis.values) // 2]
            else:
                point[axis.name] = axis.grid[len(axis.grid) // 2]
        prediction = atlas.predict(point)
        if not prediction.in_range:
            failures.append((wid, prediction.reason))
    assert not failures, f"atlases that failed a mid-grid prediction: {failures}"
