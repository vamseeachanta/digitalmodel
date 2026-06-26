# ABOUTME: Tests for the parametric ship-plate buckling sweep (digitalmodel.structural.buckling_parametric).
# ABOUTME: Validates case counts, status/utilization consistency, JSON index round-trip, thickness monotonicity.
"""Tests for buckling_parametric."""

import json

import pytest

from digitalmodel.structural.buckling_parametric import (
    BucklingSweepConfig,
    DEFAULT_SHIP_PLATE_SWEEP,
    run_sweep,
    utility_curves,
    write_outputs,
    slenderness_beta,
    _index_key,
)


@pytest.fixture
def tiny_cfg():
    return BucklingSweepConfig(
        thicknesses=[10.0, 15.0, 20.0],
        widths=[600.0, 800.0],
        plate_lengths=[2400.0],
        load_cases=[(150.0, 0.0, 0.0), (120.0, 0.0, 40.0)],
        grades=["Grade A", "AH36"],
        gamma_m=1.15,
    )


def test_case_count_matches_product(tiny_cfg):
    rows = run_sweep(tiny_cfg)
    expected = 3 * 2 * 1 * 2 * 2  # thk * width * length * load * grade
    assert len(rows) == expected == tiny_cfg.n_cases()


def test_row_schema_complete(tiny_cfg):
    rows = run_sweep(tiny_cfg)
    required = {
        "grade", "fy", "thickness_mm", "width_mm", "length_mm", "aspect",
        "slenderness_beta", "sigma_x", "sigma_y", "tau", "utilization",
        "critical_stress_mpa", "safety_factor", "governing", "status",
    }
    assert required <= set(rows[0].keys())
    assert all(r["governing"] == "plate_buckling" for r in rows)


def test_utilization_positive(tiny_cfg):
    rows = run_sweep(tiny_cfg)
    assert all(r["utilization"] > 0 for r in rows)


def test_status_consistent_with_utilization(tiny_cfg):
    rows = run_sweep(tiny_cfg)
    for r in rows:
        if r["utilization"] <= 1.0:
            assert r["status"] == "PASS"
        else:
            assert r["status"] == "FAIL"


def test_thicker_plate_lowers_utilization(tiny_cfg):
    """At fixed grade/width/load, utilization must fall as thickness grows."""
    rows = run_sweep(tiny_cfg)
    grade, width, load = "AH36", 800.0, (150.0, 0.0, 0.0)
    subset = sorted(
        (
            r for r in rows
            if r["grade"] == grade
            and r["width_mm"] == width
            and (r["sigma_x"], r["sigma_y"], r["tau"]) == load
        ),
        key=lambda r: r["thickness_mm"],
    )
    utils = [r["utilization"] for r in subset]
    assert len(utils) == 3
    assert utils == sorted(utils, reverse=True)
    assert all(a > b for a, b in zip(utils, utils[1:]))


def test_higher_grade_lowers_utilization(tiny_cfg):
    """Stronger steel should not buckle worse at identical geometry/load."""
    rows = run_sweep(tiny_cfg)
    key = dict(thickness_mm=10.0, width_mm=800.0, sigma_x=150.0, sigma_y=0.0, tau=0.0)

    def util_for(grade):
        for r in rows:
            if r["grade"] == grade and all(r[k] == v for k, v in key.items()):
                return r["utilization"]
        raise AssertionError("case not found")

    assert util_for("AH36") <= util_for("Grade A")


def test_slenderness_beta_formula():
    # beta = (b/t) sqrt(fy/E)
    beta = slenderness_beta(800.0, 10.0, 355.0, 206000.0)
    assert beta == pytest.approx((800.0 / 10.0) * (355.0 / 206000.0) ** 0.5, rel=1e-9)


def test_utility_curves_shape(tiny_cfg):
    curves = utility_curves(tiny_cfg)
    assert set(curves["grades"]) == {"Grade A", "AH36"}
    for g in curves["grades"].values():
        n = len(tiny_cfg.thicknesses)
        assert len(g["thickness_mm"]) == n
        assert len(g["utilization"]) == n
        assert len(g["critical_stress_mpa"]) == n
        assert len(g["slenderness_beta"]) == n


def test_write_outputs_files_and_keys(tiny_cfg, tmp_path):
    rows = run_sweep(tiny_cfg)
    curves = utility_curves(tiny_cfg)
    paths = write_outputs(rows, curves, tmp_path, gamma_m=tiny_cfg.gamma_m)
    assert paths["cases_csv"].exists()
    assert paths["results_json"].exists()
    payload = json.loads(paths["results_json"].read_text())
    assert set(payload.keys()) >= {"meta", "lookup", "index", "curves"}
    assert payload["meta"]["standard"] == "DNV-RP-C201"
    assert payload["meta"]["n_cases"] == len(rows)
    assert payload["meta"]["preliminary"] is False


def test_index_round_trip(tiny_cfg, tmp_path):
    rows = run_sweep(tiny_cfg)
    curves = utility_curves(tiny_cfg)
    paths = write_outputs(rows, curves, tmp_path)
    payload = json.loads(paths["results_json"].read_text())
    # Pick a known case and confirm its key maps to its utilization.
    r = rows[0]
    key = _index_key(r)
    assert key in payload["index"]
    assert payload["index"][key] == pytest.approx(r["utilization"])
    assert payload["index_status"][key] == r["status"]


def test_default_sweep_case_count():
    rows = run_sweep(DEFAULT_SHIP_PLATE_SWEEP)
    assert len(rows) == DEFAULT_SHIP_PLATE_SWEEP.n_cases() == 972
    assert {r["grade"] for r in rows} == {"Grade A", "AH36", "EH40"}
