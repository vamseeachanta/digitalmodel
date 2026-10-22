"""Serve a sweep-built atlas via the parametric_query workflow (parametrics #975).

End-to-end, offline: a synthetic Cartesian summary -> atlas_from_summary ->
Atlas.save -> parametric_query. Range and validation gates are tested without
requiring a pilot's external citation data. No physical response is qualified.
"""

from __future__ import annotations

from itertools import product
from pathlib import Path

from digitalmodel.parametric.atlas import Atlas
from digitalmodel.parametric.query import router as parametric_query
from digitalmodel.parametrics import (
    CaseResult, ParameterSweep, ParametricResultsSummary, ParametricStudy,
)
from digitalmodel.parametrics.atlas_bridge import atlas_from_summary

RADII = (15.0, 25.0, 35.0)
MBR = (4.0, 5.0, 6.0)


def _save_fowt_atlas(atlas_root: Path) -> None:
    # Synthetic x+y responses exercise serving independently of a solver pilot.
    summary = ParametricResultsSummary(results=[
        CaseResult(case_id=str(i),
                   parameters={"watch_circle_radius": radius, "mbr_limit_m": mbr},
                   max_utilisation=radius + mbr, status="completed")
        for i, (radius, mbr) in enumerate(product(RADII, MBR))
    ])
    study = ParametricStudy(
        name="orcaflex-fowt-watch-circle",
        parameters=[
            ParameterSweep(name="watch_circle_radius", values=list(RADII)),
            ParameterSweep(name="mbr_limit_m", values=list(MBR)),
        ],
    )
    atlas = atlas_from_summary(
        study,
        summary,
        "max_utilisation",
        basename="fowt_mooring",
        atlas_id="fowt-mbr-uc-test",
    )
    atlas.save(atlas_root)


def _query(atlas_root: Path, point: dict, out_dir: Path) -> dict:
    cfg = {
        "basename": "parametric_query",
        "parametric_query": {
            "atlas": "fowt_mooring",
            "atlas_root": str(atlas_root),
            "point": point,
            "policy": {"on_out_of_range": "escalate"},
            "output_dir": str(out_dir),
        },
    }
    return parametric_query(cfg)["parametric_query"]["result"]


def test_in_range_query_without_holdout_escalates(tmp_path: Path):
    _save_fowt_atlas(tmp_path / "atlases")
    result = _query(
        tmp_path / "atlases",
        {"watch_circle_radius": 20.0, "mbr_limit_m": 4.5},  # interpolated interior
        tmp_path / "out",
    )
    assert result["in_range"] is False
    assert result["action"] == "escalate"
    assert result["reason"] == "validation unavailable"
    assert result["value"] is None
    assert "confidence" not in result


def test_out_of_range_query_escalates(tmp_path: Path):
    _save_fowt_atlas(tmp_path / "atlases")
    atlas = Atlas.load(tmp_path / "atlases", "fowt_mooring")
    prediction = atlas.predict({"watch_circle_radius": 80.0, "mbr_limit_m": 5.0})
    assert prediction.in_range is False
    assert "watch_circle_radius=80" in prediction.reason
    assert "outside" in prediction.reason
    # Supply synthetic measured validation so the query's range gate is reached.
    atlas.validation = {"max_rel_error": 0.0}
    atlas.save(tmp_path / "atlases")
    result = _query(
        tmp_path / "atlases",
        {"watch_circle_radius": 80.0, "mbr_limit_m": 5.0},  # beyond [15, 35]
        tmp_path / "out",
    )
    assert result["in_range"] is False
    assert "watch_circle_radius=80" in result["reason"]
    assert "outside" in result["reason"]
