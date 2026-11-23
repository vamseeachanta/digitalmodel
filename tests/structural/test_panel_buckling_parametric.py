# ABOUTME: Tests for the parametric stiffened-panel buckling sweep (DNV-RP-C201).
# ABOUTME: Validates case count, utilisation/status consistency, governing modes, index round-trip.
"""Tests for digitalmodel.structural.panel_buckling_parametric."""

from __future__ import annotations

import pytest

from digitalmodel.structural.panel_buckling_parametric import (
    DEFAULT_PANEL_SWEEP,
    PanelSweepConfig,
    VALID_MODES,
    run_sweep,
    utility_curves,
    write_outputs,
    _index_key,
)


# A small, fast sub-sweep so the suite stays quick.
SMALL = PanelSweepConfig(
    plate_thicknesses=[10.0, 14.0, 20.0],
    stiffener_spacings=[600.0, 800.0],
    profiles=[
        {"name": "T250x90", "web_height": 250.0, "web_thickness": 10.0,
         "flange_width": 90.0, "flange_thickness": 12.0},
        {"name": "T600x200", "web_height": 600.0, "web_thickness": 10.0,
         "flange_width": 200.0, "flange_thickness": 20.0},
    ],
    load_cases=[(120.0, 0.0), (150.0, 40.0)],
    grades=["Grade A", "AH36"],
)


@pytest.fixture(scope="module")
def small_rows():
    return run_sweep(SMALL)


def test_case_count_equals_product(small_rows):
    assert len(small_rows) == SMALL.n_cases()
    assert len(small_rows) == 2 * 3 * 2 * 2 * 2  # grades*t*spacing*profile*load


def test_n_cases_helper_matches_default():
    rows = run_sweep(DEFAULT_PANEL_SWEEP)
    assert len(rows) == DEFAULT_PANEL_SWEEP.n_cases()
    assert DEFAULT_PANEL_SWEEP.n_cases() == 3 * 7 * 3 * 3 * 4  # == 756


def test_utilization_positive(small_rows):
    assert all(r["utilization"] > 0 for r in small_rows)


def test_status_util_consistency(small_rows):
    for r in small_rows:
        if r["utilization"] <= 1.0:
            assert r["status"] == "PASS"
        else:
            assert r["status"] == "FAIL"


def test_governing_mode_always_valid(small_rows):
    for r in small_rows:
        assert r["governing_mode"] in VALID_MODES


def test_governing_util_is_max_component(small_rows):
    # Total utilisation equals the largest of the three component utilisations.
    for r in small_rows:
        comp_max = max(r["plate_util"], r["column_util"], r["torsional_util"])
        assert r["utilization"] == pytest.approx(comp_max, rel=1e-3, abs=1e-3)


def test_thicker_plate_lowers_plate_mode_util():
    # Hold everything fixed but plate thickness; plate-field utilisation must drop.
    cfg = PanelSweepConfig(
        plate_thicknesses=[8.0, 12.0, 20.0],
        stiffener_spacings=[800.0],
        profiles=[{"name": "T250x90", "web_height": 250.0, "web_thickness": 10.0,
                   "flange_width": 90.0, "flange_thickness": 12.0}],
        load_cases=[(150.0, 0.0)],
        grades=["AH36"],
    )
    rows = sorted(run_sweep(cfg), key=lambda r: r["thickness_mm"])
    plate_utils = [r["plate_util"] for r in rows]
    assert plate_utils[0] > plate_utils[-1]
    assert all(a >= b for a, b in zip(plate_utils, plate_utils[1:]))


def test_index_round_trip(tmp_path):
    rows = run_sweep(SMALL)
    curves = utility_curves(SMALL)
    paths = write_outputs(rows, curves, tmp_path, gamma_m=SMALL.gamma_m)

    import json
    payload = json.loads(paths["results_json"].read_text())
    assert set(payload) >= {"meta", "lookup", "index", "index_mode", "curves"}
    assert payload["meta"]["n_cases"] == len(rows)
    assert payload["meta"]["validated"] is True

    for r in rows:
        key = _index_key(r)
        assert payload["index"][key] == r["utilization"]
        assert payload["index_mode"][key] == r["governing_mode"]


def test_write_outputs_creates_files(tmp_path):
    rows = run_sweep(SMALL)
    curves = utility_curves(SMALL)
    paths = write_outputs(rows, curves, tmp_path)
    assert paths["cases_csv"].exists()
    assert paths["results_json"].exists()
    assert paths["cases_csv"].stat().st_size > 0


def test_curves_mode_split_sums_to_n_cases():
    curves = utility_curves(SMALL)
    split = curves["mode_split"]
    assert set(split) <= set(VALID_MODES)
    assert sum(split.values()) == SMALL.n_cases()


def test_curves_series_lengths_match_thicknesses():
    curves = utility_curves(SMALL)
    n = len(SMALL.plate_thicknesses)
    for grade, s in curves["grades"].items():
        assert len(s["thickness_mm"]) == n
        assert len(s["utilization"]) == n
        assert len(s["governing_mode"]) == n
        assert all(m in VALID_MODES for m in s["governing_mode"])
