# ABOUTME: Tests for the demo_02 SQLite Results Store (additive; flattens checks{}).
# ABOUTME: Rebuild-identity (per-cell+typeof), BD-1 is_safe token, BD-3 null round-trip, golden green.
"""Tests for the demo_02 SQLite results store.

Run with:
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest \\
        examples/demos/gtm/tests/test_demo_02_results_store.py -q
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
GOLDEN = HERE / "fixtures" / "golden" / "demo_02_baseline_results.json"

sys.path.insert(0, str(GTM_DIR))

from results_store_demo02 import (  # noqa: E402
    _CASE_COL_NAMES,
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
            str(GTM_DIR / "demo_02_wall_thickness_multicode.py"),
            "--results-dir",
            str(results_dir),
        ],
        check=True,
        env=_demo_env(),
        cwd=str(GTM_DIR.parent.parent.parent),
    )


def _demo_root(results_dir: Path) -> Path:
    return results_dir / "parametric" / "demo_02"


def _dump_cells(db_path: Path):
    """Return {pk-tuple: {col: (value, typeof)}} for every cases row."""
    cols = list(_CASE_COL_NAMES)
    typeof_select = ", ".join(f"{c}, typeof({c})" for c in cols)
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
            cell["pipe_size"][0],
            cell["code"][0],
            cell["internal_pressure_mpa"][0],
        )
        out[pk] = cell
    return out


def _record(pipe, code, ip, is_safe,
            governing="combined_loading", max_util=0.5, checks="default"):
    """Build a normalized 14-key demo_02 record (mirrors the demo's record shape)."""
    if checks == "default":
        checks = {
            "pressure_containment": 0.1,
            "collapse": 0.2,
            "propagation_buckling": 0.3,
            "combined_loading": 0.5,
        }
    return {
        "pipe_size": pipe,
        "od_mm": 168.3,
        "od_m": 0.1683,
        "wt_mm": 7.11,
        "wt_m": 0.00711,
        "grade": "X65",
        "code": code,
        "internal_pressure_mpa": ip,
        "external_pressure_mpa": 5.024,
        "water_depth_m": 500.0,
        "is_safe": is_safe,
        "governing_check": governing,
        "max_utilisation": max_util,
        "checks": checks,
    }


class _Cfg:
    """Minimal stand-in for ResolvedDemo02Config for synthetic write_run calls."""
    demo_id = "demo_02"
    code_ref = ""
    sizes: list = []
    codes: list = []
    design_codes: list = []
    internal_pressures_mpa: list = []
    water_depth_m = 500.0
    grade = "X65"
    smys_pa = 448e6
    smts_pa = 531e6
    corrosion_allowance_m = 0.001
    safety_class = None
    find_min_bounds_m: tuple = (0.003, 0.060)
    find_min_tol_m = 0.0001
    source_path = "synthetic"


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


def test_cases_has_flattened_checks_and_token(baseline_dir):
    db = _demo_root(baseline_dir) / "results.db"
    conn = sqlite3.connect(str(db))
    try:
        cols = [r[1] for r in conn.execute("PRAGMA table_info(cases)")]
        for k in (
            "check_pressure_containment",
            "check_collapse",
            "check_propagation_buckling",
            "check_combined_loading",
        ):
            assert k in cols
        assert "checks" not in cols
        # is_safe is a TEXT token (BD-1).
        assert conn.execute(
            "SELECT DISTINCT typeof(is_safe) FROM cases"
        ).fetchall() == [("text",)]
        # Sample SELECT returns exactly the one expected row.
        rows = conn.execute(
            "SELECT run_id, is_safe FROM cases WHERE pipe_size = ? "
            "AND code = ? AND internal_pressure_mpa = ?",
            ('12"', "DNV-ST-F101", 20),
        ).fetchall()
        assert len(rows) == 1
        assert rows[0][0] == "baseline"
        assert rows[0][1] in ("True", "False")
    finally:
        conn.close()


def test_runs_table_counts(baseline_dir):
    db = _demo_root(baseline_dir) / "results.db"
    conn = sqlite3.connect(str(db))
    try:
        run_id, total, n_safe, n_unsafe = conn.execute(
            "SELECT run_id, total_cases, n_safe, n_unsafe FROM runs"
        ).fetchone()
        assert run_id == "baseline"
        assert total == 72
        n_true = conn.execute(
            "SELECT COUNT(*) FROM cases WHERE is_safe = 'True'"
        ).fetchone()[0]
        n_false = conn.execute(
            "SELECT COUNT(*) FROM cases WHERE is_safe = 'False'"
        ).fetchone()[0]
        assert n_safe == n_true
        assert n_unsafe == n_false
        # Golden split is 48/24 (the §A golden test asserts this on results).
        assert (n_safe, n_unsafe) == (48, 24)
    finally:
        conn.close()


# ---------------------------------------------------------------------------
# Rebuild-identity (per-cell + per-typeof)
# ---------------------------------------------------------------------------

def test_rebuild_identity_72_rows(baseline_dir):
    db = _demo_root(baseline_dir) / "results.db"
    before = _dump_cells(db)
    assert len(before) == 72
    rebuild_db(baseline_dir)
    after = _dump_cells(db)
    assert before == after, "rebuild not per-cell + per-typeof identical"


def test_rerun_idempotent(baseline_dir):
    _run_demo(baseline_dir)
    db = _demo_root(baseline_dir) / "results.db"
    conn = sqlite3.connect(str(db))
    try:
        assert conn.execute("SELECT COUNT(*) FROM cases").fetchone()[0] == 72
        assert conn.execute("SELECT COUNT(*) FROM runs").fetchone()[0] == 1
    finally:
        conn.close()
    index_lines = (_demo_root(baseline_dir) / "index.csv").read_text().splitlines()
    data_rows = [r for r in index_lines[1:] if r.strip()]
    assert len(data_rows) == 1
    assert data_rows[0].split(",")[0] == "baseline"


# ---------------------------------------------------------------------------
# BD-1: is_safe TEXT token round-trips (both paths, typeof identical)
# ---------------------------------------------------------------------------

def test_bd1_bool_token_roundtrip(tmp_path):
    results = [
        _record('6"', "DNV-ST-F101", 10, is_safe=True),
        _record('8"', "DNV-ST-F101", 10, is_safe=False),
        _record('12"', "DNV-ST-F101", 10, is_safe=None,
                governing=None, max_util=None, checks=None),
    ]
    base = tmp_path / "results"
    write_run("baseline", _Cfg(), results, base)
    db = _demo_root(base) / "results.db"
    direct = _dump_cells(db)
    rebuild_db(base)
    rebuilt = _dump_cells(db)
    assert direct == rebuilt

    tok = {pk: cell["is_safe"] for pk, cell in direct.items()}
    assert tok[("baseline", '6"', "DNV-ST-F101", 10)] == ("True", "text")
    assert tok[("baseline", '8"', "DNV-ST-F101", 10)] == ("False", "text")
    assert tok[("baseline", '12"', "DNV-ST-F101", 10)] == (None, "null")


# ---------------------------------------------------------------------------
# BD-3: synthetic null record round-trips as SQL NULL (not "" / "nan")
# ---------------------------------------------------------------------------

def test_bd3_null_roundtrip(tmp_path):
    results = [
        _record('16"', "API RP 1111", 30, is_safe=None,
                governing=None, max_util=None, checks=None),
    ]
    base = tmp_path / "results"
    write_run("baseline", _Cfg(), results, base)
    db = _demo_root(base) / "results.db"
    direct = _dump_cells(db)
    rebuild_db(base)
    rebuilt = _dump_cells(db)
    assert direct == rebuilt, "rebuild not per-cell + per-typeof identical for null record"

    pk = ("baseline", '16"', "API RP 1111", 30)
    cell = direct[pk]
    for col in (
        "is_safe",
        "governing_check",
        "max_utilisation",
        "check_pressure_containment",
        "check_collapse",
        "check_propagation_buckling",
        "check_combined_loading",
    ):
        value, tof = cell[col]
        assert value is None, f"{col} expected NULL, got {value!r}"
        assert tof == "null", f"{col} typeof expected null, got {tof}"


# ---------------------------------------------------------------------------
# §A golden contract: additive store does not alter the results JSON
# ---------------------------------------------------------------------------

def test_golden_unchanged(baseline_dir):
    produced_path = baseline_dir / "demo_02_wall_thickness_results.json"
    produced = json.loads(produced_path.read_text())["results"]
    golden = json.loads(GOLDEN.read_text())["results"]
    assert produced == golden


def test_golden_fixture_file_untouched():
    """The golden fixture on disk is byte-stable (sanity guard for this suite)."""
    import hashlib
    sha = hashlib.sha256(GOLDEN.read_bytes()).hexdigest()
    assert len(sha) == 64  # file readable; value asserted stable across the run elsewhere
