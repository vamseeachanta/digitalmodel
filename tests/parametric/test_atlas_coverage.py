"""System-wide invariants over EVERY committed parametric atlas.

Generalises the hand-picked checks in test_refresh.py to all parametric
workflows, so a newly added atlas that lacks a query handler, drifts, or fails to
interpolate a mid-grid point fails CI here — rather than NotImplementedError-ing
at query time. See docs/plans/parametric-refresh/adding-an-atlas.md.
"""

from __future__ import annotations

from digitalmodel.parametric import refresh
from digitalmodel.parametric.atlas import Atlas
from digitalmodel.parametric.query import _HANDLERS


def _rows_by_id() -> dict[str, dict]:
    return {row["id"]: row for row in refresh._registry_rows()}


def _basename(wid: str) -> str:
    return _rows_by_id()[wid]["basename"]


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
