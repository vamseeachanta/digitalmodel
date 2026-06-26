"""Tests for the deepwater dual-gradient drilling-window screen (issue #1037).

Closed-form / license-free. Physical-relationship checks (DGD pressure profile,
wider window, fewer casing strings, riser margin) plus a direct ``router(cfg)``
test that does NOT go through engine.py.
"""

from pathlib import Path

import pytest
import yaml

from digitalmodel.well.drilling.dual_gradient import (
    SEAWATER_EMW_PPG,
    DrillingWindow,
    DualGradientScreen,
    router,
)

REPO_ROOT = Path(__file__).resolve().parents[2]
INPUT_YML = REPO_ROOT / "examples" / "workflows" / "dual-gradient" / "input.yml"

# Realistic 2000 m water-depth narrow window (TVD from rig floor, EMW ppg).
_AIR_GAP = 75.0
_WATER_DEPTH = 6561.7
_DML = _AIR_GAP + _WATER_DEPTH  # mud line depth, ft


def _window():
    return DrillingWindow(
        depths_ft=[_DML + 50, _DML + 2000, _DML + 5000, _DML + 8000, _DML + 11000],
        pore_emw_ppg=[8.6, 9.2, 10.6, 12.4, 13.8],
        fracture_emw_ppg=[9.6, 11.4, 12.9, 14.4, 15.6],
    )


def _screen(**overrides):
    base = dict(
        water_depth_ft=_WATER_DEPTH,
        window=_window(),
        air_gap_ft=_AIR_GAP,
        seawater_emw_ppg=SEAWATER_EMW_PPG,
        kick_margin_ppg=0.3,
        fracture_margin_ppg=0.3,
    )
    base.update(overrides)
    return DualGradientScreen(**base)


# -- pressure / EMW profile -------------------------------------------------


def test_dual_gradient_emw_below_conventional_for_same_mud():
    """Below the mud line the seawater column pulls the DGD EMW below the
    constant single-gradient EMW for the same deep mud weight."""
    s = _screen()
    mw = 12.0
    d = _DML + 5000
    assert s.emw_dual_gradient(d, mw) < s.emw_conventional(d, mw)
    assert s.emw_conventional(d, mw) == pytest.approx(mw)


def test_dual_gradient_above_mud_line_is_seawater():
    s = _screen()
    d = _DML - 500  # above the mud line: pure seawater column
    assert s.emw_dual_gradient(d, 14.0) == pytest.approx(SEAWATER_EMW_PPG)


def test_dual_gradient_bhp_matches_two_column_hydrostatic():
    s = _screen()
    mw = 13.0
    d = _DML + 6000
    expected = 0.052 * (SEAWATER_EMW_PPG * _DML + mw * (d - _DML))
    assert s.bhp_dual_gradient(d, mw) == pytest.approx(expected)
    # and the DGD BHP is below the single-gradient BHP for the same mud
    assert s.bhp_dual_gradient(d, mw) < s.bhp_conventional(d, mw)


def test_dgd_emw_approaches_mud_weight_at_great_depth():
    s = _screen()
    mw = 13.0
    shallow = s.emw_dual_gradient(_DML + 500, mw)
    deep = s.emw_dual_gradient(_DML + 30000, mw)
    assert shallow < deep < mw  # converges toward MW but never reaches it


# -- window widening --------------------------------------------------------


def test_window_scaling_factor():
    """DGD admissible mud-weight band == conventional band * D / (D - Dml)."""
    s = _screen()
    for d in (_DML + 250, _DML + 3000, _DML + 9000):
        conv = s.available_window_ppg("conventional", d)
        dgd = s.available_window_ppg("dual_gradient", d)
        assert dgd == pytest.approx(conv * d / (d - _DML))
        assert dgd > conv  # strictly wider below the mud line


def test_dgd_min_window_at_least_as_wide():
    s = _screen()
    assert s.min_available_window_ppg("dual_gradient") >= s.min_available_window_ppg(
        "conventional"
    )


# -- casing strings ---------------------------------------------------------


def test_dgd_needs_fewer_or_equal_casing_strings():
    s = _screen()
    conv = s.casing_design("conventional")
    dgd = s.casing_design("dual_gradient")
    assert dgd.n_strings <= conv.n_strings


def test_dgd_strictly_fewer_strings_in_narrow_window():
    s = _screen()
    conv = s.casing_design("conventional")
    dgd = s.casing_design("dual_gradient")
    assert dgd.n_strings < conv.n_strings
    # window stays open for both modes with these inputs
    assert not conv.window_violated
    assert not dgd.window_violated
    # seats are ordered and reach TD
    assert conv.seat_depths_ft == sorted(conv.seat_depths_ft)
    assert conv.seat_depths_ft[-1] == pytest.approx(s.window.td_ft)
    assert dgd.seat_depths_ft[-1] == pytest.approx(s.window.td_ft)


# -- riser margin -----------------------------------------------------------


def test_riser_margin_equals_dual_gradient_emw():
    """Losing the riser replaces mud above the mud line with seawater, i.e. the
    BHP falls to the dual-gradient value."""
    s = _screen()
    mw = 12.4
    rm = s.riser_margin(mw)
    assert rm.emw_after_riser_loss_ppg == pytest.approx(
        s.emw_dual_gradient(s.window.td_ft, mw)
    )


def test_deepwater_conventional_lacks_riser_margin():
    """In this narrow deepwater window the conventional operating mud cannot
    keep the well dead if the riser is lost (classic deepwater problem)."""
    s = _screen()
    rm = s.riser_margin(12.4)
    assert rm.margin_ppg < 0.0
    assert rm.well_dead_after_loss is False


def test_riser_margin_well_dead_when_emw_exceeds_pore():
    s = _screen()
    # A very heavy deep mud restores the dual-gradient EMW above pore at TD.
    rm = s.riser_margin(20.0)
    assert rm.well_dead_after_loss is True
    assert rm.margin_ppg >= 0.0


# -- validation -------------------------------------------------------------


def test_window_rejects_fracture_below_pore():
    with pytest.raises(ValueError):
        DrillingWindow(
            depths_ft=[7000.0, 9000.0],
            pore_emw_ppg=[9.0, 10.0],
            fracture_emw_ppg=[8.5, 11.0],
        )


def test_screen_rejects_td_above_mud_line():
    w = DrillingWindow(
        depths_ft=[100.0, 500.0],
        pore_emw_ppg=[8.6, 8.7],
        fracture_emw_ppg=[9.0, 9.5],
    )
    with pytest.raises(ValueError):
        DualGradientScreen(water_depth_ft=_WATER_DEPTH, window=w)


# -- router (direct, not via engine.py) -------------------------------------


def test_router_runs_and_writes_summary(tmp_path):
    cfg = {
        "_config_dir_path": str(tmp_path),
        "dual_gradient": {
            "geometry": {
                "water_depth_ft": _WATER_DEPTH,
                "air_gap_ft": _AIR_GAP,
                "seawater_emw_ppg": SEAWATER_EMW_PPG,
            },
            "window": {
                "depths_ft": [_DML + 50, _DML + 2000, _DML + 5000, _DML + 8000, _DML + 11000],
                "pore_emw_ppg": [8.6, 9.2, 10.6, 12.4, 13.8],
                "fracture_emw_ppg": [9.6, 11.4, 12.9, 14.4, 15.6],
            },
            "margins": {"kick_margin_ppg": 0.3, "fracture_margin_ppg": 0.3},
            "step_ft": 250.0,
            "mud_weight_ppg": 12.4,
            "outputs": {"directory": "results/dual_gradient"},
        },
    }
    out = router(cfg)
    result = out["dual_gradient"]["result"]

    cd = result["casing_design"]
    assert cd["dual_gradient"]["n_strings"] < cd["conventional"]["n_strings"]
    assert cd["strings_saved_by_dgd"] == (
        cd["conventional"]["n_strings"] - cd["dual_gradient"]["n_strings"]
    )
    assert (
        result["min_window_ppg"]["dual_gradient"]
        >= result["min_window_ppg"]["conventional"]
    )
    assert result["riser_margin"]["conventional"]["well_dead_after_loss"] is False

    summary_path = Path(out["dual_gradient"]["summary_json"])
    assert summary_path.exists()
    assert summary_path.name == "dual_gradient_summary.json"


def test_router_accepts_linear_gradient_block(tmp_path):
    cfg = {
        "_config_dir_path": str(tmp_path),
        "dual_gradient": {
            "geometry": {"water_depth_ft": _WATER_DEPTH, "air_gap_ft": _AIR_GAP},
            "window": {
                "gradients": {
                    "top_depth_ft": _DML + 50,
                    "bottom_depth_ft": _DML + 11000,
                    "pore_top_ppg": 8.6,
                    "pore_bottom_ppg": 13.8,
                    "fracture_top_ppg": 9.6,
                    "fracture_bottom_ppg": 15.6,
                }
            },
        },
    }
    out = router(cfg)
    assert out["dual_gradient"]["result"]["casing_design"]["dual_gradient"][
        "n_strings"
    ] >= 1


def test_example_input_yaml_parses_and_runs(tmp_path):
    cfg = yaml.safe_load(INPUT_YML.read_text())
    cfg["_config_dir_path"] = str(tmp_path)
    assert cfg["basename"] == "dual_gradient"
    out = router(cfg)
    cd = out["dual_gradient"]["result"]["casing_design"]
    assert cd["dual_gradient"]["n_strings"] < cd["conventional"]["n_strings"]
