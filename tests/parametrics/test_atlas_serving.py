"""Serve a sweep-built atlas via the parametric_query workflow (parametrics #975).

End-to-end, offline: parametrics FOWT sweep -> atlas_from_summary -> Atlas.save
-> parametric_query router serves an interpolated utilisation answer, and gates
out-of-range points to escalation.
"""

from __future__ import annotations

from pathlib import Path

from digitalmodel.parametric.query import router as parametric_query
from digitalmodel.parametrics import ParameterSweep, ParametricStudy
from digitalmodel.parametrics.atlas_bridge import atlas_from_summary
from digitalmodel.parametrics.pilots import orcaflex_fowt_watch_circle_sweep

RADII = (15.0, 25.0, 35.0)
MBR = (4.0, 5.0, 6.0)


def _save_fowt_atlas(atlas_root: Path) -> None:
    summary = orcaflex_fowt_watch_circle_sweep(
        watch_circle_radii_m=RADII, mbr_limits_m=MBR
    )
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


def test_in_range_query_serves_utilisation(tmp_path: Path):
    _save_fowt_atlas(tmp_path / "atlases")
    result = _query(
        tmp_path / "atlases",
        {"watch_circle_radius": 20.0, "mbr_limit_m": 4.5},  # interpolated interior
        tmp_path / "out",
    )
    assert result["in_range"] is True
    assert result["response"] == "utilisation"
    assert result["screening_status"] == "pass"  # UC ~0.03 << 1.0
    assert result["value"] > 0.0


def test_out_of_range_query_escalates(tmp_path: Path):
    _save_fowt_atlas(tmp_path / "atlases")
    result = _query(
        tmp_path / "atlases",
        {"watch_circle_radius": 80.0, "mbr_limit_m": 5.0},  # beyond [15, 35]
        tmp_path / "out",
    )
    assert result["in_range"] is False
    assert "watch_circle_radius" in str(result.get("reason", ""))
