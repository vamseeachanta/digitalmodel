# ABOUTME: Tests for the demo_04 SQLite Results Store (additive; prefixed check columns).
# ABOUTME: Rebuild-identity (per-cell+typeof), 999.0/None->NULL round-trip, composite PK, golden green.
"""Tests for the demo_04 SQLite results store.

Run with:
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest \\
        examples/demos/gtm/tests/test_demo_04_results_store.py -q
"""
from __future__ import annotations

import csv
import os
import sqlite3
import sys
from pathlib import Path

import pytest

HERE = Path(os.path.dirname(os.path.abspath(__file__)))
GTM_DIR = HERE.parent
SRC_DIR = GTM_DIR.parent.parent.parent / "src"

sys.path.insert(0, str(GTM_DIR))

from results_store_demo04 import (  # noqa: E402
    _BOOL_COLUMNS,
    _CASE_COL_NAMES,
    _CHECK_SPECS,
    _check_col,
    _q,
    rebuild_db,
    write_run,
)
import demo_04_shallow_water_pipelay as demo  # noqa: E402
from sweep_config_demo04 import load_demo04_config  # noqa: E402

_BASELINE_CONFIG_PATH = GTM_DIR / "inputs" / "demo_04_pipelay.yml"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _demo_root(results_dir: Path) -> Path:
    return results_dir / "parametric" / "demo_04"


def _dump_cells(db_path: Path):
    """Return {pk-tuple: {col: (value, typeof)}} for every cases row."""
    cols = list(_CASE_COL_NAMES)
    typeof_select = ", ".join(f"{_q(c)}, typeof({_q(c)})" for c in cols)
    conn = sqlite3.connect(str(db_path))
    try:
        rows = conn.execute(f"SELECT {typeof_select} FROM cases").fetchall()
    finally:
        conn.close()
    out = {}
    for row in rows:
        cell = {c: (row[2 * i], row[2 * i + 1]) for i, c in enumerate(cols)}
        pk = (
            cell["run_id"][0],
            cell["vessel_id"][0],
            cell["pipe_size"][0],
            cell["water_depth_m"][0],
        )
        out[pk] = cell
    return out


# Every check column name (everything that must go NULL on the ERROR path's checks={}).
_CHECK_COLS = [
    _check_col(prefix, suffix)
    for prefix, suffixes in _CHECK_SPECS
    for suffix, _typ in suffixes
]


def _good_case(vessel_id, pipe_size, depth, status="GO", max_util=0.25):
    """A full demo_04 record with all five check dicts populated (feasible)."""
    return {
        "vessel_id": vessel_id,
        "vessel_name": "Large PLV",
        "pipe_size": pipe_size,
        "pipe_od_mm": 219.1,
        "pipe_wt_mm": 8.18,
        "water_depth_m": depth,
        "overall_status": status,
        "max_utilisation": max_util,
        "governing_check": "sagbend",
        "H_chosen_kn": 37.4,
        "T_top_te": 3.9,
        "departure_angle_deg": 8.0,
        "sagbend_util": max_util,
        "overbend_util": 0.0699,
        "tension_util": 0.0064,
        "sagbend_stress_mpa": 29.2,
        "overbend_stress_mpa": 22.5,
        "R_sag_m": 1017.7,
        "checks": {
            "departure_angle": {
                "required_angle_deg": 8.0, "stinger_min_deg": 2.0,
                "stinger_max_deg": 8.0, "in_range": True, "status": "PASS",
            },
            "top_tension": {
                "T_top_kn": 37.8, "T_top_te": 3.9, "tensioner_capacity_te": 600,
                "utilisation": 0.0064, "status": "PASS",
            },
            "sagbend": {
                "R_sag_m": 1017.7, "kappa_sag": 0.00098257,
                "sigma_bending_mpa": 22.3, "sigma_axial_mpa": 6.9,
                "sigma_combined_mpa": 29.2, "stress_basis": "combined",
                "utilisation": max_util, "status": "PASS",
            },
            "overbend": {
                "R_overbend_m": 1006.0, "sigma_overbend_mpa": 22.5,
                "utilisation": 0.0699, "status": "PASS",
            },
            "vessel_capability": {
                "pipe_od_in": 8.6, "max_vessel_diameter_in": 60,
                "diameter_ok": True, "depth_ok": True, "status": "PASS",
            },
        },
    }


def _infeasible_case(vessel_id, pipe_size, depth):
    """A feasible-schema record carrying the 999.0 infeasible utilisation sentinels
    (mirrors demo._build_infeasible_result: the checks dicts ARE populated but the
    utilisations are the 999.0 sentinel and the *_ok flags are False)."""
    return {
        "vessel_id": vessel_id,
        "vessel_name": "Large PLV",
        "pipe_size": pipe_size,
        "pipe_od_mm": 219.1,
        "pipe_wt_mm": 8.18,
        "water_depth_m": depth,
        "overall_status": "NO_GO",
        "max_utilisation": 9.99,
        "governing_check": "vessel_capability",
        "H_chosen_kn": 0.0,
        "T_top_te": 0,
        "departure_angle_deg": 0,
        "sagbend_util": 999.0,
        "overbend_util": 999.0,
        "tension_util": 999.0,
        "sagbend_stress_mpa": 0,
        "overbend_stress_mpa": 0,
        "R_sag_m": 0,
        "checks": {
            "departure_angle": {
                "status": "FAIL", "required_angle_deg": 0, "in_range": False,
                "stinger_min_deg": 2.0, "stinger_max_deg": 8.0,
            },
            "top_tension": {
                "status": "FAIL", "T_top_kn": 0, "T_top_te": 0,
                "tensioner_capacity_te": 600, "utilisation": 999.0,
            },
            "sagbend": {
                "status": "FAIL", "utilisation": 999.0, "sigma_combined_mpa": 0,
                "sigma_bending_mpa": 0, "sigma_axial_mpa": 0, "R_sag_m": 0,
                "kappa_sag": 0, "stress_basis": "combined",
            },
            "overbend": {
                "status": "FAIL", "utilisation": 999.0, "sigma_overbend_mpa": 0,
                "R_overbend_m": 0,
            },
            "vessel_capability": {
                "status": "FAIL", "diameter_ok": False, "depth_ok": False,
                "pipe_od_in": 8.6, "max_vessel_diameter_in": 60,
            },
        },
    }


def _error_case(vessel_id, pipe_size, depth):
    """The normalized ERROR record: checks={}, max_utilisation/sub-utils None
    (mirrors run_parametric_sweep's except branch)."""
    return {
        "vessel_id": vessel_id,
        "vessel_name": "Large PLV",
        "pipe_size": pipe_size,
        "pipe_od_mm": 219.1,
        "pipe_wt_mm": 8.18,
        "water_depth_m": depth,
        "overall_status": "ERROR",
        "max_utilisation": None,
        "governing_check": "N/A",
        "H_chosen_kn": 0,
        "T_top_te": 0,
        "departure_angle_deg": 0,
        "sagbend_util": None,
        "overbend_util": None,
        "tension_util": None,
        "sagbend_stress_mpa": 0,
        "overbend_stress_mpa": 0,
        "R_sag_m": 0,
        "checks": {},
    }


class _Cfg:
    """Minimal stand-in for ResolvedDemo04Config for synthetic write_run calls."""
    demo_id = "demo_04"
    code_ref = ""
    vessels: list = ["PLV-001", "PLV-002"]
    pipe_sizes: list = [("8in", 8.18), ("24in", 9.53)]
    water_depths: list = [7, 30]
    grade = "X65"
    smys_pa = 448e6
    smts_pa = 531e6
    seawater_density_kg_m3 = 1025.0
    gravity_m_s2 = 9.80665
    steel_density_kg_m3 = 7850.0
    youngs_modulus_pa = 207e9
    stress_limit_factor = 0.72
    tension_margin = 1.10
    go_threshold = 0.85
    nogo_utilisation = 1.0
    sagbend_stress_basis = "combined"
    util_color_green_below = 0.70
    util_color_amber_below = 0.90
    vessels_path = Path("synthetic/pipelay_vessels.json")
    pipelines_path = Path("synthetic/pipelines.json")
    results_root = Path("synthetic/results")
    output_root = Path("synthetic/output")
    source_path = Path("synthetic")


# ---------------------------------------------------------------------------
# Schema / column-pinning tests
# ---------------------------------------------------------------------------

def test_column_count_and_names_pinned(tmp_path):
    """46 columns; the prefixed check columns are present, bare keys are NOT."""
    results = [_good_case("PLV-001", "8in", 10)]
    base = tmp_path / "results"
    write_run("baseline", _Cfg(), results, base)
    db = _demo_root(base) / "results.db"
    conn = sqlite3.connect(str(db))
    try:
        cols = [r[1] for r in conn.execute("PRAGMA table_info(cases)")]
        assert len(cols) == 46
        assert cols == _CASE_COL_NAMES
        for c in (
            "departure_angle_in_range", "departure_angle_status",
            "top_tension_utilisation", "sagbend_utilisation",
            "sagbend_sigma_combined_mpa", "sagbend_stress_basis",
            "overbend_utilisation", "vessel_capability_depth_ok",
            "vessel_capability_diameter_ok",
        ):
            assert c in cols, f"missing prefixed column {c}"
        # Bare repeated check-dict keys never collide into a single column (they only
        # ever appear prefixed). T_top_te IS a legitimate top-level column, so it is NOT
        # in this list.
        for bare in ("status", "utilisation", "in_range", "stress_basis", "depth_ok"):
            assert bare not in cols, f"bare key {bare} should not be a column"
        # water_depth_m is INTEGER; status/stress_basis are TEXT.
        assert conn.execute(
            "SELECT DISTINCT typeof(water_depth_m) FROM cases"
        ).fetchall() == [("integer",)]
        assert conn.execute(
            "SELECT DISTINCT typeof(\"sagbend_status\") FROM cases"
        ).fetchall() == [("text",)]
        assert conn.execute(
            "SELECT DISTINCT typeof(\"sagbend_stress_basis\") FROM cases"
        ).fetchall() == [("text",)]
        # BOOLEAN-origin flags are stored as INTEGER 0/1.
        assert conn.execute(
            "SELECT DISTINCT typeof(\"departure_angle_in_range\") FROM cases"
        ).fetchall() == [("integer",)]
    finally:
        conn.close()


def test_csv_header_equals_declared_columns(tmp_path):
    """The per-run cases.csv header == declared columns (minus run_id)."""
    results = [_good_case("PLV-001", "8in", 10)]
    base = tmp_path / "results"
    write_run("baseline", _Cfg(), results, base)
    cases_csv = _demo_root(base) / "baseline" / "cases.csv"
    with cases_csv.open("r", newline="", encoding="utf-8") as fh:
        header = next(csv.reader(fh))
    assert header == [c for c in _CASE_COL_NAMES if c != "run_id"]


def test_composite_pk_uniqueness(tmp_path):
    """Two records with the same (run_id, vessel_id, pipe_size, water_depth_m) collapse
    to one row (composite PK enforced), the later write winning the delete-then-insert."""
    # Same PK, different status — the executemany INSERT must raise on the dupe within a run.
    results = [
        _good_case("PLV-001", "8in", 10, status="GO"),
        _good_case("PLV-001", "8in", 10, status="MARGINAL"),
    ]
    base = tmp_path / "results"
    with pytest.raises(sqlite3.IntegrityError):
        write_run("baseline", _Cfg(), results, base)

    # Distinct PKs across the four key axes all coexist.
    results = [
        _good_case("PLV-001", "8in", 10),
        _good_case("PLV-002", "8in", 10),
        _good_case("PLV-001", "24in", 10),
        _good_case("PLV-001", "8in", 30),
    ]
    base2 = tmp_path / "results2"
    write_run("baseline", _Cfg(), results, base2)
    db = _demo_root(base2) / "results.db"
    conn = sqlite3.connect(str(db))
    try:
        assert conn.execute("SELECT COUNT(*) FROM cases").fetchone()[0] == 4
    finally:
        conn.close()


# ---------------------------------------------------------------------------
# Core identity test: direct-write db == rebuild-from-cases.csv db (per-cell + typeof).
# ---------------------------------------------------------------------------

def test_rebuild_identity_cell_for_cell(tmp_path):
    """Direct-write db == rebuild-from-cases.csv db, every cell + every typeof."""
    results = [
        _good_case("PLV-001", "8in", 10, status="GO", max_util=0.09),
        _good_case("PLV-002", "12in", 20, status="MARGINAL", max_util=0.88),
        _infeasible_case("PLV-001", "8in", 7),
        _error_case("PLV-002", "24in", 30),
    ]
    base = tmp_path / "results"
    write_run("baseline", _Cfg(), results, base)
    db = _demo_root(base) / "results.db"
    before = _dump_cells(db)
    assert len(before) == 4
    rebuild_db(base)
    after = _dump_cells(db)
    assert before == after, "rebuild not per-cell + per-typeof identical"


# ---------------------------------------------------------------------------
# The headline reviewer finding: 999.0 sentinel + None->NULL round-trip.
# ---------------------------------------------------------------------------

def test_999_sentinel_and_none_null_round_trip(tmp_path):
    """The 999.0 infeasible sentinel stays REAL 999.0 and None stays SQL NULL, on
    BOTH the direct-write path AND the rebuild-from-cases.csv path."""
    results = [
        _infeasible_case("PLV-001", "8in", 7),
        _error_case("PLV-002", "24in", 30),
    ]
    base = tmp_path / "results"
    write_run("baseline", _Cfg(), results, base)
    db = _demo_root(base) / "results.db"

    direct = _dump_cells(db)
    rebuild_db(base)
    rebuilt = _dump_cells(db)
    assert direct == rebuilt, "rebuild not per-cell + per-typeof identical (sentinel/NULL)"

    # --- The infeasible row: 999.0 utilisations stay REAL 999.0. ---
    inf_pk = ("baseline", "PLV-001", "8in", 7)
    inf = direct[inf_pk]
    for col in (
        "sagbend_util", "overbend_util", "tension_util",
        "top_tension_utilisation", "sagbend_utilisation", "overbend_utilisation",
    ):
        value, tof = inf[col]
        assert value == 999.0, f"{col}: expected 999.0, got {value!r}"
        assert tof == "real", f"{col}: expected typeof real, got {tof}"
    # The *_ok flags went False -> INTEGER 0 (not NULL, not the string "False").
    assert inf["vessel_capability_diameter_ok"] == (0, "integer")
    assert inf["vessel_capability_depth_ok"] == (0, "integer")
    assert inf["departure_angle_in_range"] == (0, "integer")
    # max_utilisation 9.99 is a real number (capped display value, not a sentinel).
    assert inf["max_utilisation"] == (9.99, "real")

    # --- The ERROR row: None -> SQL NULL (never 0, never "nan"). ---
    err_pk = ("baseline", "PLV-002", "24in", 30)
    err = direct[err_pk]
    # Every check column is NULL (checks={}).
    for col in _CHECK_COLS:
        value, tof = err[col]
        assert value is None, f"ERROR {col}: expected NULL, got {value!r}"
        assert tof == "null", f"ERROR {col}: expected typeof null, got {tof}"
    # The None top-level utils are NULL.
    for col in ("max_utilisation", "sagbend_util", "overbend_util", "tension_util"):
        value, tof = err[col]
        assert value is None, f"ERROR {col}: expected NULL, got {value!r}"
        assert tof == "null", f"ERROR {col}: expected typeof null, got {tof}"
    # Real top-level fields survive.
    assert err["overall_status"] == ("ERROR", "text")
    assert err["governing_check"] == ("N/A", "text")
    assert err["water_depth_m"] == (30, "integer")  # BD-3 INTEGER even on ERROR

    # --- A True bool flag round-trips as INTEGER 1 (sanity vs the False=0 above). ---
    good_results = [_good_case("PLV-001", "8in", 10)]
    base2 = tmp_path / "results_good"
    write_run("baseline", _Cfg(), good_results, base2)
    db2 = _demo_root(base2) / "results.db"
    good = _dump_cells(db2)[("baseline", "PLV-001", "8in", 10)]
    assert good["departure_angle_in_range"] == (1, "integer")
    assert good["vessel_capability_depth_ok"] == (1, "integer")
    rebuild_db(base2)
    good_rb = _dump_cells(db2)[("baseline", "PLV-001", "8in", 10)]
    assert good == good_rb


# ---------------------------------------------------------------------------
# Artifacts + run counts.
# ---------------------------------------------------------------------------

def test_write_run_emits_four_artifacts_and_counts(tmp_path):
    results = [
        _good_case("PLV-001", "8in", 10, status="GO"),
        _good_case("PLV-001", "12in", 20, status="MARGINAL", max_util=0.9),
        _infeasible_case("PLV-001", "8in", 7),
        _error_case("PLV-002", "24in", 30),
    ]
    base = tmp_path / "results"
    write_run("baseline", _Cfg(), results, base)
    root = _demo_root(base)
    assert (root / "baseline" / "cases.csv").exists()
    assert (root / "baseline" / "manifest.json").exists()
    assert (root / "index.csv").exists()
    assert (root / "results.db").exists()

    db = root / "results.db"
    conn = sqlite3.connect(str(db))
    try:
        run_id, total, n_go, n_marg, n_nogo, n_err = conn.execute(
            "SELECT run_id, total_cases, n_go, n_marginal, n_nogo, n_error FROM runs"
        ).fetchone()
        assert run_id == "baseline"
        assert total == 4
        assert (n_go, n_marg, n_nogo, n_err) == (1, 1, 1, 1)
        assert n_go + n_marg + n_nogo + n_err == total
    finally:
        conn.close()


# ---------------------------------------------------------------------------
# The baseline run's cases.csv matches what the demo produces from the baseline config.
# ---------------------------------------------------------------------------

def test_baseline_cases_csv_matches_demo_output(tmp_path):
    """write_run from the baseline config's sweep produces the same cases.csv content
    (header + every data row) as the committed baseline cases.csv."""
    config = load_demo04_config(_BASELINE_CONFIG_PATH)
    vessels = demo.load_vessels(config=config)
    pipes = demo.load_pipes(config=config)
    results, _df = demo.run_parametric_sweep(vessels, pipes, config=config)

    base = tmp_path / "results"
    write_run("baseline", config, results, base)
    produced = (_demo_root(base) / "baseline" / "cases.csv").read_text(encoding="utf-8")

    committed = (
        GTM_DIR / "results" / "parametric" / "demo_04" / "baseline" / "cases.csv"
    ).read_text(encoding="utf-8")
    assert produced == committed


def test_baseline_rebuild_identity(tmp_path):
    """The full 60-case baseline run rebuilds per-cell + per-typeof identically.

    Note: the baseline run contains no 999.0-sentinel / None-ERROR cases (its
    NO_GO cases carry real computed values), so the sentinel/NULL round-trip is
    covered by the synthetic test_999_sentinel_and_none_null_round_trip, not here.
    """
    config = load_demo04_config(_BASELINE_CONFIG_PATH)
    vessels = demo.load_vessels(config=config)
    pipes = demo.load_pipes(config=config)
    results, _df = demo.run_parametric_sweep(vessels, pipes, config=config)

    base = tmp_path / "results"
    write_run("baseline", config, results, base)
    db = _demo_root(base) / "results.db"
    before = _dump_cells(db)
    assert len(before) == 60
    rebuild_db(base)
    after = _dump_cells(db)
    assert before == after
