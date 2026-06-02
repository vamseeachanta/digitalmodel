# ABOUTME: Tests for the demo_03 SQLite Results Store (additive; prefixed phase columns).
# ABOUTME: Rebuild-identity (per-cell+typeof), BD-1 ERROR null/typeof, BD-3 int depth, golden green.
"""Tests for the demo_03 SQLite results store.

Run with:
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest \\
        examples/demos/gtm/tests/test_demo_03_results_store.py -q
"""
from __future__ import annotations

import json
import os
import sqlite3
import subprocess
import sys
from pathlib import Path

import pytest

HERE = Path(os.path.dirname(os.path.abspath(__file__)))
GTM_DIR = HERE.parent
SRC_DIR = GTM_DIR.parent.parent.parent / "src"
GOLDEN = HERE / "fixtures" / "golden" / "demo_03_baseline_results.json"

sys.path.insert(0, str(GTM_DIR))

from results_store_demo03 import (  # noqa: E402
    _CASE_COL_NAMES,
    _PHASE_SPECS,
    _phase_col,
    _q,
    rebuild_db,
    write_run,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _demo_env() -> dict:
    env = dict(os.environ)
    env["PYTHONPATH"] = os.pathsep.join([str(GTM_DIR), str(SRC_DIR)])
    env["PYTHONUNBUFFERED"] = "1"
    return env


def _run_demo(results_dir: Path) -> None:
    subprocess.run(
        [
            sys.executable,
            str(GTM_DIR / "demo_03_deepwater_mudmat_installation.py"),
            "--results-dir",
            str(results_dir),
        ],
        check=True,
        env=_demo_env(),
        cwd=str(GTM_DIR.parent.parent.parent),
    )


def _demo_root(results_dir: Path) -> Path:
    return results_dir / "parametric" / "demo_03"


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
            cell["structure_id"][0],
            cell["water_depth_m"][0],
            cell["hs_m"][0],
        )
        out[pk] = cell
    return out


# The full set of phase + utilisation column names (everything that must go NULL
# on the ERROR path's phases={}).
_PHASE_COLS = [
    _phase_col(slug, suffix)
    for slug, _disp, suffixes in _PHASE_SPECS
    for suffix, _typ in suffixes
]


def _good_case(vessel_id, structure_id, depth, hs, status="GO", max_util=0.25):
    """A full demo_03 record with all five phase dicts populated."""
    return {
        "vessel_id": vessel_id,
        "vessel_name": "Large CSV",
        "structure_id": structure_id,
        "structure_name": "Mudmat-S-50te",
        "mass_te": 50.0,
        "water_depth_m": depth,
        "hs_m": hs,
        "overall_status": status,
        "max_utilisation": max_util,
        "governing_phase": "Landing",
        "phases": {
            "Lift-off": {
                "phase": "Lift-off", "hook_load_kn": 539.4, "crane_swl_kn": 27458.6,
                "operating_radius_m": 40.0, "daf": 1.1, "utilisation": 0.0196,
                "status": "PASS",
            },
            "In-air": {
                "phase": "In-air", "tilt_deg": 0.0, "tilt_limit_deg": 5.0,
                "cog_offset_m": 0.0, "hook_height_m": 7.7, "utilisation": 0.0,
                "status": "PASS",
            },
            "Splash zone": {
                "phase": "Splash zone", "hs_m": hs, "tp_s": 4.0,
                "v_lowering_m_s": 0.167, "v_wave_m_s": 0.785, "v_rel_m_s": 0.952,
                "f_slam_kn": 83.6, "f_var_buoyancy_kn": 180.9, "f_drag_kn": 0.3,
                "w_air_kn": 490.3, "max_hook_splash_kn": 755.1, "daf_splash": 1.3,
                "design_hook_kn": 981.7, "crane_swl_kn": 27458.6,
                "utilisation": 0.0358, "status": "PASS",
            },
            "Lowering": {
                "phase": "Lowering", "depth_m": depth, "w_sub_kn": 445.1,
                "w_cable_kn": 430.6, "snap_load_kn": 53.9, "max_tension_kn": 929.6,
                "wire_mbl_kn": 63743.2, "allowable_tension_kn": 54181.7,
                "heave_amp_m": 0.4, "omega_rad_s": 1.5708, "utilisation": 0.0172,
                "status": "PASS",
            },
            "Landing": {
                "phase": "Landing", "w_sub_kn": 445.1, "a_base_m2": 36.0,
                "applied_bearing_pressure_kpa": 12.36, "q_ult_kpa": 62.91,
                "q_allow_kpa": 31.46, "su_kpa": 10.0, "nc": 5.14, "sc": 1.2,
                "dc": 1.02, "factor_of_safety": 2.0,
                "utilisation": max_util, "status": "PASS",
            },
        },
    }


def _error_case(vessel_id, structure_id, depth, hs):
    """The normalized ERROR record: phases={} (mirrors run_parametric_sweep)."""
    return {
        "vessel_id": vessel_id,
        "vessel_name": "Large CSV",
        "structure_id": structure_id,
        "structure_name": "Mudmat-S-50te",
        "mass_te": 50.0,
        "water_depth_m": depth,
        "hs_m": hs,
        "overall_status": "ERROR",
        "max_utilisation": None,
        "governing_phase": "N/A",
        "phases": {},
    }


class _Cfg:
    """Minimal stand-in for ResolvedDemo03Config for synthetic write_run calls."""
    demo_id = "demo_03"
    code_ref = ""
    vessels: list = ["Large CSV", "Medium CSV"]
    depths: list = [500, 1000]
    mudmats: list = ["Mudmat-S-50te"]
    hs: list = [1.0, 2.0]
    daf_liftoff = 1.1
    daf_splash = 1.3
    wire_mbl_sf = 0.85
    tilt_limit_deg = 5.0
    operating_radius_m = 40.0
    reference_hs = 2.0
    tp_coefficient = 4.0
    go_marginal_threshold = 0.8
    nogo_utilisation = 1.0
    seawater_density_kg_m3 = 1025.0
    gravity_m_s2 = 9.80665
    steel_density_kg_m3 = 7850.0
    undrained_shear_strength_su_kpa = 10.0
    bearing_capacity_factor_nc = 5.14
    apply_shape_factor = True
    apply_depth_factor = True
    factor_of_safety = 2.0
    vessels_path = Path("synthetic/vessels.json")
    mudmats_path = Path("synthetic/mudmats.json")
    results_root = Path("synthetic/results")
    output_root = Path("synthetic/output")
    source_path = Path("synthetic")


@pytest.fixture(scope="module")
def baseline_dir(tmp_path_factory):
    results_dir = tmp_path_factory.mktemp("results")
    _run_demo(results_dir)
    return results_dir


# ---------------------------------------------------------------------------
# Artifact + schema tests
# ---------------------------------------------------------------------------

def test_baseline_writes_four_artifacts(baseline_dir):
    root = _demo_root(baseline_dir)
    run_dir = root / "baseline"
    assert (run_dir / "cases.csv").exists()
    assert (run_dir / "manifest.json").exists()
    assert (root / "index.csv").exists()
    assert (root / "results.db").exists()


def test_cases_has_prefixed_phase_columns(baseline_dir):
    db = _demo_root(baseline_dir) / "results.db"
    conn = sqlite3.connect(str(db))
    try:
        cols = [r[1] for r in conn.execute("PRAGMA table_info(cases)")]
        assert len(cols) == 66
        # Hyphen-preserving prefixed columns are present; bare keys are NOT.
        # §4: the landing phase emits the derived-capacity columns (applied/q_ult/q_allow/...).
        for c in (
            "lift-off_hook_load_kn", "in-air_tilt_deg", "splash_zone_f_slam_kn",
            "lowering_depth_m", "landing_applied_bearing_pressure_kpa",
            "landing_q_ult_kpa", "landing_q_allow_kpa", "landing_su_kpa",
            "landing_nc", "landing_sc", "landing_dc", "landing_factor_of_safety",
            "lift-off_status", "landing_status", "lift-off_utilisation",
            "landing_utilisation",
        ):
            assert c in cols, f"missing prefixed column {c}"
        # The old flat-pressure landing columns are GONE (§4 renamed them).
        for gone in ("landing_bearing_kpa", "landing_bearing_limit_kpa"):
            assert gone not in cols, f"stale landing column {gone} should be removed"
        for bare in ("phase", "status", "utilisation", "crane_swl_kn", "w_sub_kn",
                     "depth_m"):
            assert bare not in cols, f"bare key {bare} should not be a column"
        # BD-3: water_depth_m + lowering depth_m are INTEGER columns.
        assert conn.execute(
            "SELECT DISTINCT typeof(water_depth_m) FROM cases"
        ).fetchall() == [("integer",)]
        assert conn.execute(
            "SELECT DISTINCT typeof(\"lowering_depth_m\") FROM cases"
        ).fetchall() == [("integer",)]
        # status/phase/governing_phase are TEXT, never INTEGER.
        assert conn.execute(
            "SELECT DISTINCT typeof(\"lift-off_status\") FROM cases"
        ).fetchall() == [("text",)]
        assert conn.execute(
            "SELECT DISTINCT typeof(governing_phase) FROM cases"
        ).fetchall() == [("text",)]
    finally:
        conn.close()


def test_sample_select(baseline_dir):
    """A sample SELECT returns exactly the one expected baseline row."""
    db = _demo_root(baseline_dir) / "results.db"
    conn = sqlite3.connect(str(db))
    try:
        rows = conn.execute(
            "SELECT run_id, overall_status, governing_phase FROM cases "
            "WHERE vessel_id = ? AND structure_id = ? "
            "AND water_depth_m = ? AND hs_m = ?",
            ("CSV-001", "MUD-S", 500, 1.0),
        ).fetchall()
        assert len(rows) == 1
        assert rows[0][0] == "baseline"
        assert rows[0][1] in ("GO", "MARGINAL", "NO_GO", "ERROR")
        assert isinstance(rows[0][2], str)
    finally:
        conn.close()


def test_runs_table_counts(baseline_dir):
    db = _demo_root(baseline_dir) / "results.db"
    conn = sqlite3.connect(str(db))
    try:
        run_id, total, n_go, n_marg, n_nogo, n_err = conn.execute(
            "SELECT run_id, total_cases, n_go, n_marginal, n_nogo, n_error FROM runs"
        ).fetchone()
        assert run_id == "baseline"
        assert total == 180
        # The four buckets partition the total exactly.
        assert n_go + n_marg + n_nogo + n_err == total
        # Each bucket equals the SELECT count by overall_status.
        for col, status in (
            (n_go, "GO"), (n_marg, "MARGINAL"), (n_nogo, "NO_GO"), (n_err, "ERROR"),
        ):
            got = conn.execute(
                "SELECT COUNT(*) FROM cases WHERE overall_status = ?", (status,)
            ).fetchone()[0]
            assert col == got, f"{status}: runs={col} cases={got}"
    finally:
        conn.close()


# ---------------------------------------------------------------------------
# Rebuild-identity (per-cell + per-typeof)
# ---------------------------------------------------------------------------

def test_rebuild_identity_180_rows(baseline_dir):
    db = _demo_root(baseline_dir) / "results.db"
    before = _dump_cells(db)
    assert len(before) == 180
    rebuild_db(baseline_dir)
    after = _dump_cells(db)
    assert before == after, "rebuild not per-cell + per-typeof identical"


def test_rerun_idempotent(baseline_dir):
    _run_demo(baseline_dir)
    db = _demo_root(baseline_dir) / "results.db"
    conn = sqlite3.connect(str(db))
    try:
        assert conn.execute("SELECT COUNT(*) FROM cases").fetchone()[0] == 180
        assert conn.execute("SELECT COUNT(*) FROM runs").fetchone()[0] == 1
    finally:
        conn.close()
    index_lines = (_demo_root(baseline_dir) / "index.csv").read_text().splitlines()
    data_rows = [r for r in index_lines[1:] if r.strip()]
    assert len(data_rows) == 1
    assert data_rows[0].split(",")[0] == "baseline"


# ---------------------------------------------------------------------------
# BD-1: synthetic ERROR record (phases={}) -> every phase/util cell NULL on
# BOTH the direct-write and rebuild paths.
# ---------------------------------------------------------------------------

def test_bd1_error_record_all_phase_cells_null(tmp_path):
    results = [
        _good_case("CSV-001", "MUD-S", 500, 1.0, status="GO"),
        _error_case("CSV-001", "MUD-S", 1000, 2.0),
    ]
    base = tmp_path / "results"
    write_run("baseline", _Cfg(), results, base)
    db = _demo_root(base) / "results.db"
    direct = _dump_cells(db)
    rebuild_db(base)
    rebuilt = _dump_cells(db)
    assert direct == rebuilt, "rebuild not per-cell + per-typeof identical (ERROR record)"

    err_pk = ("baseline", "CSV-001", "MUD-S", 1000, 2.0)
    cell = direct[err_pk]
    for col in _PHASE_COLS:
        value, tof = cell[col]
        assert value is None, f"ERROR path {col} expected NULL, got {value!r}"
        assert tof == "null", f"ERROR path {col} typeof expected null, got {tof}"
    # The 11 top-level keys still carry their real values (only phases went NULL).
    assert cell["overall_status"] == ("ERROR", "text")
    assert cell["governing_phase"] == ("N/A", "text")
    assert cell["max_utilisation"] == (None, "null")
    assert cell["water_depth_m"] == (1000, "integer")  # BD-3 INTEGER even on ERROR
    assert cell["hs_m"][1] == "real"

    # The good case keeps populated phase cells (sanity: schema survives both).
    good_pk = ("baseline", "CSV-001", "MUD-S", 500, 1.0)
    good = direct[good_pk]
    assert good["lift-off_status"] == ("PASS", "text")
    assert good["lowering_depth_m"] == (500, "integer")  # BD-3 lowering INTEGER


# ---------------------------------------------------------------------------
# BD-3: typeof('water_depth_m') == 'integer' on both the direct-write and
# rebuild paths for a populated case.
# ---------------------------------------------------------------------------

def test_bd3_integer_depth_both_paths(tmp_path):
    results = [_good_case("CSV-001", "MUD-S", 1500, 2.5)]
    base = tmp_path / "results"
    write_run("baseline", _Cfg(), results, base)
    db = _demo_root(base) / "results.db"

    def _depth_typeofs():
        conn = sqlite3.connect(str(db))
        try:
            return conn.execute(
                "SELECT typeof(water_depth_m), typeof(\"lowering_depth_m\") FROM cases"
            ).fetchall()
        finally:
            conn.close()

    assert _depth_typeofs() == [("integer", "integer")]
    rebuild_db(base)
    assert _depth_typeofs() == [("integer", "integer")]


# ---------------------------------------------------------------------------
# §A golden contract: additive store does not alter the results JSON.
# demo_03 top-level key is `cases` (NOT `results`).
# ---------------------------------------------------------------------------

def test_golden_unchanged(baseline_dir):
    produced_path = baseline_dir / "demo_03_mudmat_installation_results.json"
    produced = json.loads(produced_path.read_text())["cases"]
    golden = json.loads(GOLDEN.read_text())["cases"]
    assert produced == golden


def test_golden_fixture_file_readable():
    import hashlib
    sha = hashlib.sha256(GOLDEN.read_bytes()).hexdigest()
    assert len(sha) == 64
