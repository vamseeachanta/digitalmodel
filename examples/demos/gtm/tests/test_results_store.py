# ABOUTME: Tests for the demo_01 Results Store (SQLite + per-run CSV/manifest, ADR-0003).
# ABOUTME: Asserts artifacts, the 3 promoted axes, 15-key JSON invariant, and rebuild byte-identity.
"""Results Store tests.

Run with:
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest examples/demos/gtm/tests/test_results_store.py -q
"""
from __future__ import annotations

import csv
import json
import shutil
import sqlite3
from pathlib import Path

import pytest

import demo_01_dnv_freespan_viv as demo
import results_store as rs
from sweep_config import load_sweep_config

_GTM_DIR = Path(demo.__file__).resolve().parent
_BASELINE_CONFIG_PATH = _GTM_DIR / "inputs" / "demo_01_freespan.yml"
_GOLDEN_PATH = _GTM_DIR / "tests" / "fixtures" / "golden" / "demo_01_baseline_results.json"


@pytest.fixture(scope="module")
def baseline_run(tmp_path_factory):
    """Run the Baseline sweep and write it to an isolated Store under a tmp base_dir."""
    base_dir = tmp_path_factory.mktemp("store")
    _pd, pipe_catalog = demo.load_pipe_catalog()
    _jd, jumper_props = demo.load_jumper_catalog()
    config = load_sweep_config(_BASELINE_CONFIG_PATH)
    results, df = demo.run_parametric_sweep(pipe_catalog, jumper_props, config)
    summary_df = demo.build_summary_stats(df)
    run_dir = rs.write_run(
        run_id=demo.BASELINE_RUN_ID,
        resolved_config=config,
        df=df,
        summary_df=summary_df,
        base_dir=base_dir,
    )
    return {
        "base_dir": base_dir,
        "run_dir": run_dir,
        "results": results,
        "df": df,
        "summary_df": summary_df,
        "config": config,
    }


# ---------------------------------------------------------------------------
# Artifacts exist
# ---------------------------------------------------------------------------

def test_four_artifacts_exist(baseline_run):
    run_dir = baseline_run["run_dir"]
    base_dir = baseline_run["base_dir"]
    store = base_dir / "parametric" / "demo_01"
    assert (run_dir / "cases.csv").is_file()
    assert (run_dir / "manifest.json").is_file()
    assert (store / "index.csv").is_file()
    assert (store / "results.db").is_file()
    assert run_dir == store / "baseline"


# ---------------------------------------------------------------------------
# Promoted axes present; JSON still 15-key
# ---------------------------------------------------------------------------

def test_promoted_axes_in_csv_and_sqlite(baseline_run):
    run_dir = baseline_run["run_dir"]
    base_dir = baseline_run["base_dir"]
    promoted = {"content_density_kg_m3", "boundary_condition", "wt_selection"}

    with (run_dir / "cases.csv").open(encoding="utf-8") as fh:
        header = next(csv.reader(fh))
    assert promoted.issubset(set(header))

    conn = sqlite3.connect(base_dir / "parametric" / "demo_01" / "results.db")
    cols = {r[1] for r in conn.execute("PRAGMA table_info(cases)")}
    conn.close()
    assert promoted.issubset(cols)


def test_json_results_still_15_key(baseline_run):
    """The JSON cases[] dicts must stay exactly the 15 frozen keys (no enrichment leak)."""
    for case in baseline_run["results"]:
        assert set(case.keys()) == set(demo.FROZEN_RESULT_KEYS)
    assert len(demo.FROZEN_RESULT_KEYS) == 15


def test_baseline_axis_values_are_module_defaults(baseline_run):
    """The promoted-axis columns carry the Baseline's frozen values."""
    df = baseline_run["df"]
    assert set(df["content_density_kg_m3"].unique()) == {demo.CONTENT_DENSITY}
    assert set(df["boundary_condition"].unique()) == {"pinned"}
    assert set(df["wt_selection"].unique()) == {"thinnest"}


# ---------------------------------------------------------------------------
# Existing golden test still green (re-run inline)
# ---------------------------------------------------------------------------

def test_existing_golden_still_green(baseline_run):
    """Re-run the byte-identity oracle: produced cases[] == frozen golden cases[]."""
    with _GOLDEN_PATH.open(encoding="utf-8") as fh:
        golden_cases = json.load(fh)["cases"]
    assert baseline_run["results"] == golden_cases


# ---------------------------------------------------------------------------
# rebuild_db byte-identity (incl. typeof), with a jumper mid-water case present
# ---------------------------------------------------------------------------

def _dump_cells_with_typeof(db_path: Path):
    conn = sqlite3.connect(db_path)
    cols = ", ".join(n for n, _ in rs._CASE_COLUMNS)
    typs = ", ".join(f"typeof({n})" for n, _ in rs._CASE_COLUMNS)
    cases = conn.execute(
        f"SELECT {cols}, {typs} FROM cases ORDER BY run_id, case_id"
    ).fetchall()
    runs = conn.execute("SELECT * FROM runs ORDER BY run_id").fetchall()
    conn.close()
    return cases, runs


def test_rebuild_db_identical_per_cell_including_typeof(baseline_run, tmp_path):
    base_dir = baseline_run["base_dir"]
    direct_db = base_dir / "parametric" / "demo_01" / "results.db"

    # Fixture MUST include a jumper mid-water (e_over_d == "inf") case.
    df = baseline_run["df"]
    assert (df["e_over_d"].astype(str) == "inf").any(), "fixture lacks a mid-water case"

    # Snapshot the direct-write db, then rebuild from text and compare.
    direct_copy = tmp_path / "direct.db"
    shutil.copy(direct_db, direct_copy)
    rebuilt = rs.rebuild_db(base_dir)

    d_cases, d_runs = _dump_cells_with_typeof(direct_copy)
    r_cases, r_runs = _dump_cells_with_typeof(rebuilt)
    assert d_cases == r_cases
    assert d_runs == r_runs
    assert len(d_cases) == 680


def test_jumper_midwater_e_over_d_is_text_on_both_paths(baseline_run, tmp_path):
    base_dir = baseline_run["base_dir"]
    direct_db = base_dir / "parametric" / "demo_01" / "results.db"
    direct_copy = tmp_path / "direct2.db"
    shutil.copy(direct_db, direct_copy)
    rebuilt = rs.rebuild_db(base_dir)

    q = (
        "SELECT e_over_d, typeof(e_over_d) FROM cases "
        "WHERE run_id='baseline' AND e_over_d='inf' LIMIT 1"
    )
    for dbp in (direct_copy, rebuilt):
        conn = sqlite3.connect(dbp)
        row = conn.execute(q).fetchone()
        conn.close()
        assert row == ("inf", "text")


# ---------------------------------------------------------------------------
# Sample lookup
# ---------------------------------------------------------------------------

def test_sample_lookup(baseline_run):
    base_dir = baseline_run["base_dir"]
    conn = sqlite3.connect(base_dir / "parametric" / "demo_01" / "results.db")
    rows = conn.execute(
        "SELECT status, a_over_d, max_allowable_span_m, span_m FROM cases "
        "WHERE run_id='baseline' AND nominal_size='12in' AND v_current_ms=0.6 "
        "AND e_over_d='1.0' ORDER BY span_m"
    ).fetchall()
    conn.close()
    # 8 spans (10..80); span_m=10 PASSes, the rest are FAIL_CF; max allowable span is 17.4 m.
    assert len(rows) == 8
    assert rows[0] == ("PASS", 0.0, 17.4, 10)
    assert all(r[2] == 17.4 for r in rows)
    assert [r[0] for r in rows[1:]] == ["FAIL_CF"] * 7


# ---------------------------------------------------------------------------
# Re-running a run_id replaces (no duplicate cases, one index row)
# ---------------------------------------------------------------------------

def test_rerun_does_not_duplicate(baseline_run):
    base_dir = baseline_run["base_dir"]
    rs.write_run(
        run_id=demo.BASELINE_RUN_ID,
        resolved_config=baseline_run["config"],
        df=baseline_run["df"],
        summary_df=baseline_run["summary_df"],
        base_dir=base_dir,
    )
    conn = sqlite3.connect(base_dir / "parametric" / "demo_01" / "results.db")
    n_cases = conn.execute(
        "SELECT COUNT(*) FROM cases WHERE run_id='baseline'"
    ).fetchone()[0]
    n_runs = conn.execute(
        "SELECT COUNT(*) FROM runs WHERE run_id='baseline'"
    ).fetchone()[0]
    conn.close()
    assert n_cases == 680
    assert n_runs == 1

    with (base_dir / "parametric" / "demo_01" / "index.csv").open(encoding="utf-8") as fh:
        index_rows = [r for r in csv.DictReader(fh)]
    assert sum(1 for r in index_rows if r["run_id"] == "baseline") == 1


# ---------------------------------------------------------------------------
# D1: rebuild_db ALSO regenerates index.csv from per-run manifests
# ---------------------------------------------------------------------------

def test_rebuild_db_regenerates_index_csv(baseline_run):
    """index.csv is a derived view: rebuild_db rebuilds it from scratch from manifests."""
    base_dir = baseline_run["base_dir"]
    index_path = base_dir / "parametric" / "demo_01" / "index.csv"

    # Capture the direct-write index row for the baseline run.
    with index_path.open(encoding="utf-8") as fh:
        direct = {r["run_id"]: r for r in csv.DictReader(fh)}
    assert "baseline" in direct

    # Delete index.csv entirely, then rebuild — it must come back from the manifest alone.
    index_path.unlink()
    assert not index_path.exists()
    rs.rebuild_db(base_dir)
    assert index_path.is_file(), "rebuild_db must regenerate index.csv"

    with index_path.open(encoding="utf-8") as fh:
        rebuilt = {r["run_id"]: r for r in csv.DictReader(fh)}
    assert "baseline" in rebuilt, "rebuilt index.csv must carry the baseline row"
    # The derived row reconciles the manifest's counts/total.
    assert int(rebuilt["baseline"]["total_cases"]) == 680
    # Same columns + same baseline values the direct-write path produced.
    assert set(rebuilt["baseline"].keys()) == set(rs._INDEX_COLUMNS)
    for col in ("total_cases", "n_pass", "n_inline", "n_fail_cf", "n_lockin"):
        assert rebuilt["baseline"][col] == direct["baseline"][col]


# ---------------------------------------------------------------------------
# D5: a NaN gap token is rejected, not silently serialized as "nan"
# ---------------------------------------------------------------------------

def test_e_over_d_token_rejects_nan():
    with pytest.raises(ValueError):
        rs._e_over_d_token(float("nan"))
    with pytest.raises(ValueError):
        rs._e_over_d_token("nan")
    # +inf and finite tokens still resolve as before (no regression).
    assert rs._e_over_d_token(float("inf")) == "inf"
    assert rs._e_over_d_token("inf") == "inf"
    assert rs._e_over_d_token(0.5) == "0.5"
    assert rs._e_over_d_token("1.0") == "1.0"
