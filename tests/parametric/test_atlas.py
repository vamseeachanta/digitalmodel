"""Unit tests for the parametric atlas engine (epic #794, pilot #796).

Covers the log-log interpolation accuracy and the two hard safety rails:
out-of-range never extrapolates, and an unknown categorical value escalates.
"""

from __future__ import annotations

import pytest

from digitalmodel.parametric.atlas import Atlas
from digitalmodel.parametric.generate import (
    RESPONSE_FUNCS,
    Axis,
    generate_atlas,
)


def _small_atlas() -> Atlas:
    return generate_atlas(
        basename="mooring_fatigue",
        physics="log_log",
        response="damage",
        axes=[
            Axis(name="tension_range_kN", scale="log",
                 grid=[150, 200, 250, 300, 350, 400]),
            Axis(name="n_cycles", scale="log", grid=[1e4, 1e5, 1e6]),
            Axis(name="area_mm2", scale="log",
                 grid=[4000, 5000, 6000, 7000, 8000]),
            Axis(name="sn_curve", values=["D", "F"]),
        ],
        response_kwargs={"environment": "seawater_cp"},
        tolerance=0.10,
    )


def test_holdout_validation_under_tolerance():
    atlas = _small_atlas()
    assert atlas.validation["passes"] is True
    assert atlas.max_rel_error <= 0.10


def test_grid_point_is_exact():
    atlas = _small_atlas()
    fn = RESPONSE_FUNCS["mooring_fatigue"]
    point = {"tension_range_kN": 250, "n_cycles": 1e5, "area_mm2": 6000, "sn_curve": "D"}
    pred = atlas.predict(point)
    assert pred.in_range
    assert pred.value == pytest.approx(fn(point, environment="seawater_cp"), rel=1e-9)


def test_n_cycles_is_exactly_linear():
    # damage is linear in n_cycles, so an off-grid n_cycles must stay near-exact
    atlas = _small_atlas()
    fn = RESPONSE_FUNCS["mooring_fatigue"]
    point = {"tension_range_kN": 250, "n_cycles": 4.2e5, "area_mm2": 6000, "sn_curve": "D"}
    pred = atlas.predict(point)
    assert pred.value == pytest.approx(fn(point, environment="seawater_cp"), rel=1e-6)


def test_out_of_range_does_not_extrapolate():
    atlas = _small_atlas()
    over = atlas.predict(
        {"tension_range_kN": 999, "n_cycles": 1e5, "area_mm2": 6000, "sn_curve": "D"}
    )
    assert over.in_range is False
    assert "outside" in over.reason

    under = atlas.predict(
        {"tension_range_kN": 250, "n_cycles": 1e5, "area_mm2": 1000, "sn_curve": "D"}
    )
    assert under.in_range is False


def test_unknown_category_escalates():
    atlas = _small_atlas()
    pred = atlas.predict(
        {"tension_range_kN": 250, "n_cycles": 1e5, "area_mm2": 6000, "sn_curve": "Z9"}
    )
    assert pred.in_range is False
    assert "sn_curve" in pred.reason


def test_save_load_roundtrip(tmp_path):
    atlas = _small_atlas()
    atlas.save(tmp_path)
    reloaded = Atlas.load(tmp_path, "mooring_fatigue")
    assert reloaded.atlas_id == atlas.atlas_id
    point = {"tension_range_kN": 275, "n_cycles": 2e5, "area_mm2": 6000, "sn_curve": "F"}
    assert reloaded.predict(point).value == pytest.approx(atlas.predict(point).value)
