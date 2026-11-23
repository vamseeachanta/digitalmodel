"""Bridge a parametric sweep into the parametric Atlas (parametrics P2, #971)."""

from __future__ import annotations

import pytest

from digitalmodel.parametrics.atlas_bridge import atlas_from_summary
from digitalmodel.parametrics.pilots import orcaflex_fowt_watch_circle_sweep


def _fowt_atlas():
    study_radii = (15.0, 25.0, 35.0)
    study_mbr = (4.0, 5.0, 6.0)
    summary = orcaflex_fowt_watch_circle_sweep(
        watch_circle_radii_m=study_radii, mbr_limits_m=study_mbr
    )
    # Rebuild the matching study for axis definitions (same params/values).
    from digitalmodel.parametrics import ParameterSweep, ParametricStudy

    study = ParametricStudy(
        name="orcaflex-fowt-watch-circle",
        parameters=[
            ParameterSweep(name="watch_circle_radius", values=list(study_radii)),
            ParameterSweep(name="mbr_limit_m", values=list(study_mbr)),
        ],
    )
    atlas = atlas_from_summary(
        study,
        summary,
        "max_utilisation",
        basename="fowt_mooring",
        atlas_id="fowt-mbr-uc-v0",
    )
    return atlas, summary


def test_atlas_builds_with_two_axes():
    atlas, _ = _fowt_atlas()
    assert atlas.response == "max_utilisation"
    assert [ax.name for ax in atlas.axes] == ["watch_circle_radius", "mbr_limit_m"]
    assert len(atlas.grid) == 9  # 3 x 3 full factorial


def test_predict_at_grid_node_matches_swept_value():
    atlas, summary = _fowt_atlas()
    df = summary.to_dataframe()
    row = df[(df["watch_circle_radius"] == 25.0) & (df["mbr_limit_m"] == 5.0)].iloc[0]
    pred = atlas.predict({"watch_circle_radius": 25.0, "mbr_limit_m": 5.0})
    assert pred.in_range
    assert pred.value == pytest.approx(float(row["max_utilisation"]), rel=1e-6)


def test_predict_interpolates_interior_point():
    atlas, _ = _fowt_atlas()
    pred = atlas.predict({"watch_circle_radius": 20.0, "mbr_limit_m": 4.5})
    assert pred.in_range
    # UC is linear in mbr_limit; 4.5 is the midpoint of 4.0 and 6.0 nodes at r=20.
    lo = atlas.predict({"watch_circle_radius": 20.0, "mbr_limit_m": 4.0}).value
    hi = atlas.predict({"watch_circle_radius": 20.0, "mbr_limit_m": 6.0}).value
    assert lo < pred.value < hi


def test_predict_out_of_range_is_gated():
    atlas, _ = _fowt_atlas()
    pred = atlas.predict({"watch_circle_radius": 80.0, "mbr_limit_m": 5.0})
    assert not pred.in_range
    assert "watch_circle_radius" in pred.reason


def test_atlas_from_summary_rejects_unknown_response():
    atlas, summary = _fowt_atlas()
    from digitalmodel.parametrics import ParameterSweep, ParametricStudy

    study = ParametricStudy(
        name="x",
        parameters=[
            ParameterSweep(name="watch_circle_radius", values=[15.0, 25.0, 35.0]),
            ParameterSweep(name="mbr_limit_m", values=[4.0, 5.0, 6.0]),
        ],
    )
    with pytest.raises(ValueError, match="not in result columns"):
        atlas_from_summary(study, summary, "no_such_metric", basename="b", atlas_id="i")
