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


def _code_check_atlas(grid_max_tension=2000) -> Atlas:
    return generate_atlas(
        basename="code_check",
        physics="utilization_threshold",
        response="utilisation",
        axes=[
            Axis(name="outer_diameter", scale="linear", grid=[0.20, 0.28, 0.36]),
            Axis(name="wall_thickness", scale="linear", grid=[0.015, 0.025, 0.035]),
            Axis(name="effective_tension_kN", scale="linear",
                 grid=[0, grid_max_tension // 2, grid_max_tension]),
            Axis(name="bending_moment_kNm", scale="linear", grid=[0, 200, 400]),
            Axis(name="smys", values=[359e6, 448e6]),
        ],
        response_kwargs={"design_factor": 0.67, "smts_ratio": 1.185},
        tolerance=0.10,
    )


def test_code_check_atlas_accuracy():
    from digitalmodel.parametric.generate import RESPONSE_FUNCS

    atlas = _code_check_atlas()
    # on a grid knot the interpolation is exact (denser grids clear the 0.10
    # publish gate; this 3-knot test grid is deliberately coarse)
    point = {"outer_diameter": 0.28, "wall_thickness": 0.025,
             "effective_tension_kN": 1000, "bending_moment_kNm": 200, "smys": 448e6}
    pred = atlas.predict(point)
    truth = RESPONSE_FUNCS["code_check"](point, design_factor=0.67, smts_ratio=1.185)
    assert pred.value == pytest.approx(truth, rel=1e-6)


def test_code_check_straddle_escalates():
    from digitalmodel.parametric.query import _handle_utilisation

    atlas = _code_check_atlas()
    # weak section + load that drives utilisation to ~1.0: band must straddle 1.0
    result = _handle_utilisation(
        atlas,
        {"outer_diameter": 0.20, "wall_thickness": 0.015,
         "effective_tension_kN": 1600, "bending_moment_kNm": 80, "smys": 359e6},
    )
    assert result["in_range"] is False
    assert "straddles" in result["reason"]


def test_rao_atlas_is_linear_exact():
    import math

    from digitalmodel.parametric.generate import RESPONSE_FUNCS

    db = "examples/workflows/rao-tabulation/input.yml"
    atlas = generate_atlas(
        basename="rao_tabulation",
        physics="linear",
        response="heave_m",
        axes=[
            Axis(name="frequency_rad_s", scale="linear",
                 grid=[0.349066, 0.448799, 0.628319, 1.047198]),
            Axis(name="heading_deg", scale="linear", grid=[0.0, 90.0]),
        ],
        response_kwargs={"database_path": db},
        tolerance=0.05,
    )
    # linear interpolation of a linear surface reproduces the workflow exactly
    assert atlas.max_rel_error == pytest.approx(0.0, abs=1e-6)
    point = {"frequency_rad_s": 2 * math.pi / 8.0, "heading_deg": 45.0}
    truth = RESPONSE_FUNCS["rao_tabulation"](point, database_path=db)
    assert atlas.predict(point).value == pytest.approx(truth, rel=1e-6)


def test_capacity_demand_handler_applies_load_case():
    from digitalmodel.parametric.generate import RESPONSE_FUNCS
    from digitalmodel.parametric.query import _handle_capacity_demand

    atlas = generate_atlas(
        basename="pile_capacity",
        physics="linear",
        response="capacity_kN",
        axes=[
            Axis(name="diameter_m", scale="linear", grid=[0.5, 1.0, 1.5, 2.0, 2.5]),
            Axis(name="embedded_length_m", scale="linear", grid=[10, 20, 30, 40, 50]),
            Axis(name="Su_kpa", scale="linear", grid=[30, 60, 90, 120, 150]),
            Axis(name="sigma_v_kpa", scale="linear", grid=[50, 150, 250, 350, 450]),
        ],
        tolerance=0.10,
    )
    assert atlas.validation["passes"]
    point = {"diameter_m": 1.0, "embedded_length_m": 30.0, "Su_kpa": 60.0,
             "sigma_v_kpa": 150.0, "demand_kN": 2000.0, "factor_of_safety": 2.0}
    result = _handle_capacity_demand(atlas, point)
    cap = RESPONSE_FUNCS["pile_capacity"](point)
    assert result["in_range"] is True
    assert result["capacity_kN"] == pytest.approx(cap, rel=1e-6)
    assert result["value"] == pytest.approx(2000.0 * 2.0 / cap, rel=1e-6)


def test_free_span_utilisation_atlas():
    from digitalmodel.parametric.generate import RESPONSE_FUNCS

    atlas = generate_atlas(
        basename="free_span",
        physics="utilization_threshold",
        response="span_utilisation",
        axes=[
            Axis(name="span_length_m", scale="linear", grid=[10, 20, 30, 40, 50, 60, 70]),
            Axis(name="od_m", scale="linear", grid=[0.20, 0.30, 0.40, 0.50, 0.60]),
            Axis(name="wt_m", scale="linear", grid=[0.012, 0.018, 0.024, 0.030]),
            Axis(name="current_velocity_ms", scale="linear", grid=[0.2, 0.5, 0.8, 1.1, 1.4]),
        ],
        tolerance=0.10,
    )
    assert atlas.validation["passes"]
    point = {"span_length_m": 30.0, "od_m": 0.30, "wt_m": 0.018, "current_velocity_ms": 0.8}
    pred = atlas.predict(point)
    truth = RESPONSE_FUNCS["free_span"](point)
    assert pred.value == pytest.approx(truth, rel=1e-6)


def test_save_load_roundtrip(tmp_path):
    atlas = _small_atlas()
    atlas.save(tmp_path)
    reloaded = Atlas.load(tmp_path, "mooring_fatigue")
    assert reloaded.atlas_id == atlas.atlas_id
    point = {"tension_range_kN": 275, "n_cycles": 2e5, "area_mm2": 6000, "sn_curve": "F"}
    assert reloaded.predict(point).value == pytest.approx(atlas.predict(point).value)
