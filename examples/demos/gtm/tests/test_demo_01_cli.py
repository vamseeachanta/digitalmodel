# ABOUTME: Tests for the demo_01 run CLI — argv-sniff dispatch, per-run report, lookup, rebuild-db.
# ABOUTME: Guards BD-1 (golden sweep path untouched) and exercises BD-2/BD-3 lookup semantics.
"""demo_01 run CLI tests.

Run with:
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest examples/demos/gtm/tests/test_demo_01_cli.py -q
"""
from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path

import pytest

import demo_01_dnv_freespan_viv as demo
import results_store as rs
from sweep_config import load_sweep_config

_GTM_DIR = Path(demo.__file__).resolve().parent
_SCRIPT = _GTM_DIR / "demo_01_dnv_freespan_viv.py"
_BASELINE_CONFIG_PATH = _GTM_DIR / "inputs" / "demo_01_freespan.yml"
_GOLDEN_PATH = _GTM_DIR / "tests" / "fixtures" / "golden" / "demo_01_baseline_results.json"


def _run_cli(*args: str, cwd: Path = _GTM_DIR):
    """Invoke the demo script as a subprocess with the right PYTHONPATH."""
    env = {
        "PYTHONPATH": f"{_GTM_DIR}:{_GTM_DIR.parent.parent.parent / 'src'}",
        "PATH": __import__("os").environ.get("PATH", ""),
    }
    return subprocess.run(
        [sys.executable, str(_SCRIPT), *args],
        capture_output=True, text=True, timeout=180, cwd=str(cwd), env=env,
    )


@pytest.fixture(scope="module")
def baseline_store(tmp_path_factory):
    """Build an isolated Results Store with a baseline run for read-only lookup tests."""
    base_dir = tmp_path_factory.mktemp("clistore")
    _pd, pipe_catalog = demo.load_pipe_catalog()
    _jd, jumper_props = demo.load_jumper_catalog()
    config = load_sweep_config(_BASELINE_CONFIG_PATH)
    _results, df = demo.run_parametric_sweep(pipe_catalog, jumper_props, config)
    summary_df = demo.build_summary_stats(df)
    rs.write_run(
        run_id=demo.BASELINE_RUN_ID,
        resolved_config=config,
        df=df,
        summary_df=summary_df,
        base_dir=base_dir,
    )
    return base_dir


# ---------------------------------------------------------------------------
# BD-1: the sweep path is untouched by the new subcommand machinery
# ---------------------------------------------------------------------------

def test_from_cache_still_exits_zero():
    """--from-cache exits 0 with the lookup/rebuild-db machinery present (BD-1 regression)."""
    result = _run_cli("--from-cache")
    assert result.returncode == 0, result.stderr[-800:]
    assert "ImportError" not in result.stderr
    assert "ModuleNotFoundError" not in result.stderr


def test_argv_sniff_does_not_intercept_sweep_flags():
    """The sweep parser accepts the existing flags unchanged + the new --run-id default."""
    sys_argv = sys.argv
    try:
        sys.argv = ["demo", "--from-cache"]
        parsed = demo.parse_args()
        assert parsed.from_cache is True
        assert parsed.run_id == demo.BASELINE_RUN_ID
    finally:
        sys.argv = sys_argv


# ---------------------------------------------------------------------------
# Named run: distinct run dir + SQLite run + per-run report.html
# ---------------------------------------------------------------------------

def test_named_run_writes_distinct_artifacts():
    """A named run writes its own store dir, a distinct SQLite run row, and a per-run report."""
    result = _run_cli("--run-id", "testrun")
    try:
        assert result.returncode == 0, result.stderr[-800:]

        run_dir = _GTM_DIR / "results" / "parametric" / "demo_01" / "testrun"
        report = _GTM_DIR / "output" / "parametric" / "demo_01" / "testrun" / "report.html"
        assert (run_dir / "cases.csv").is_file()
        assert (run_dir / "manifest.json").is_file()
        assert report.is_file(), "named run must write output/parametric/demo_01/testrun/report.html"

        import sqlite3
        db = _GTM_DIR / "results" / "parametric" / "demo_01" / "results.db"
        conn = sqlite3.connect(f"file:{db}?mode=ro", uri=True)
        try:
            runs = {r[0] for r in conn.execute("SELECT run_id FROM runs")}
            n = conn.execute(
                "SELECT COUNT(*) FROM cases WHERE run_id='testrun'"
            ).fetchone()[0]
        finally:
            conn.close()
        assert "testrun" in runs
        assert n == 680
    finally:
        # Clean up the named run from the real store + rebuild the cache to baseline-only.
        import shutil
        shutil.rmtree(_GTM_DIR / "results" / "parametric" / "demo_01" / "testrun",
                      ignore_errors=True)
        shutil.rmtree(_GTM_DIR / "output" / "parametric" / "demo_01" / "testrun",
                      ignore_errors=True)
        rs.rebuild_db(_GTM_DIR / "results")


# ---------------------------------------------------------------------------
# BD-2/BD-3: lookup filter semantics against an isolated baseline store
# ---------------------------------------------------------------------------

def test_lookup_filter_combo(baseline_store, capsys):
    """A pipeline filter combo returns the expected rows + count."""
    rc = demo.lookup_main([
        "--base-dir", str(baseline_store), "--run-id", "baseline",
        "--size", "12in", "--current", "0.6", "--gap", "1.0",
    ])
    out = capsys.readouterr().out
    assert rc == 0
    assert "8 case(s) matched." in out
    assert "PL-0170" in out and "PASS" in out and "17.4" in out


def test_lookup_gap_inf_matches_jumper_midwater(baseline_store, capsys):
    """--gap inf canonicalizes to the 'inf' TEXT token and matches the jumper mid-water rows."""
    rc = demo.lookup_main([
        "--base-dir", str(baseline_store), "--run-id", "baseline", "--gap", "inf",
    ])
    out = capsys.readouterr().out
    assert rc == 0
    assert "40 case(s) matched." in out
    assert "8in-jumper" in out
    assert "inf" in out


def test_lookup_zero_match_message(baseline_store, capsys):
    """A filter matching no rows prints the distinct zero-match message (not the unknown-run one)."""
    rc = demo.lookup_main([
        "--base-dir", str(baseline_store), "--run-id", "baseline", "--size", "99in",
    ])
    out = capsys.readouterr().out
    assert rc == 0
    assert "0 cases match the given filters." in out


def test_lookup_unknown_run(baseline_store, capsys):
    """An unknown run_id prints the available-runs message and exits non-zero."""
    rc = demo.lookup_main(["--base-dir", str(baseline_store), "--run-id", "nope"])
    out = capsys.readouterr().out
    assert rc != 0
    assert "run_id 'nope' not found" in out
    assert "baseline" in out


def test_lookup_missing_db_does_not_create(tmp_path, capsys):
    """BD-3: a missing db prints the run-a-sweep message, exits non-zero, and creates NO db."""
    rc = demo.lookup_main(["--base-dir", str(tmp_path), "--run-id", "baseline"])
    out = capsys.readouterr().out
    assert rc != 0
    assert "No results.db" in out
    assert "run a sweep first" in out
    assert not (tmp_path / "parametric" / "demo_01" / "results.db").exists()


# ---------------------------------------------------------------------------
# rebuild-db reports counts
# ---------------------------------------------------------------------------

def test_rebuild_db_reports(baseline_store, capsys):
    rc = demo.rebuild_main(["--base-dir", str(baseline_store)])
    out = capsys.readouterr().out
    assert rc == 0
    assert "Results Store rebuilt" in out
    assert "runs: 1" in out
    assert "cases: 680" in out
    assert "baseline: 680 cases" in out


# ---------------------------------------------------------------------------
# D1: run_id validation (path-traversal / empty / collision)
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("bad_id", ["../../evil", "a/b", "", "."])
def test_named_sweep_rejects_bad_run_id_no_outside_dir(bad_id, tmp_path):
    """A bad --run-id is rejected non-zero by the sweep path and creates NO dir outside the store."""
    result = _run_cli("--run-id", bad_id)
    assert result.returncode != 0, result.stdout[-800:]
    assert "Traceback" not in result.stderr
    # Path-traversal target: ../../evil from the store root would land here.
    assert not (_GTM_DIR.parent.parent / "evil").exists()
    assert not (_GTM_DIR / "results" / "parametric" / "demo_01" / "evil").exists()


@pytest.mark.parametrize("good_id", ["baseline", "acme_q2"])
def test_validate_run_id_accepts_good(good_id):
    """validate_run_id accepts the Baseline id and ordinary client run names."""
    assert demo.validate_run_id(good_id) == good_id


@pytest.mark.parametrize("bad_id", ["../../evil", "a/b", "", ".", ".."])
def test_validate_run_id_rejects_bad(bad_id):
    """validate_run_id raises ValueError for traversal / separators / empty / dot ids."""
    with pytest.raises(ValueError):
        demo.validate_run_id(bad_id)


@pytest.mark.parametrize("bad_id", ["../../evil", "a/b", "", ".", ".."])
def test_write_run_rejects_bad_run_id(bad_id, tmp_path):
    """Defense-in-depth: results_store.write_run rejects a bad run_id and writes nothing outside its tree."""
    _pd, pipe_catalog = demo.load_pipe_catalog()
    _jd, jumper_props = demo.load_jumper_catalog()
    config = load_sweep_config(_BASELINE_CONFIG_PATH)
    _results, df = demo.run_parametric_sweep(pipe_catalog, jumper_props, config)
    summary_df = demo.build_summary_stats(df)
    with pytest.raises(ValueError):
        rs.write_run(run_id=bad_id, resolved_config=config, df=df,
                     summary_df=summary_df, base_dir=tmp_path)
    assert not (tmp_path.parent / "evil").exists()


def test_lookup_rejects_bad_run_id(baseline_store, capsys):
    """lookup rejects a traversal run_id non-zero with a clean message (no traceback)."""
    rc = demo.lookup_main(["--base-dir", str(baseline_store), "--run-id", "../../evil"])
    out = capsys.readouterr().out
    assert rc != 0
    assert "invalid --run-id" in out


# ---------------------------------------------------------------------------
# D2: clean error for a bad --gap value
# ---------------------------------------------------------------------------

def test_lookup_bad_gap_clean_error(baseline_store, capsys):
    """lookup --gap notanumber exits non-zero with a clean message and no traceback."""
    rc = demo.lookup_main([
        "--base-dir", str(baseline_store), "--run-id", "baseline", "--gap", "notanumber",
    ])
    out = capsys.readouterr().out
    assert rc != 0
    assert "invalid --gap value: notanumber (expected a number or 'inf')" in out
    assert "Traceback" not in out


# ---------------------------------------------------------------------------
# Golden still green (re-run inline)
# ---------------------------------------------------------------------------

def test_golden_still_green():
    """The byte-identity oracle still holds with the CLI changes present."""
    _pd, pipe_catalog = demo.load_pipe_catalog()
    _jd, jumper_props = demo.load_jumper_catalog()
    config = load_sweep_config(_BASELINE_CONFIG_PATH)
    results, _df = demo.run_parametric_sweep(pipe_catalog, jumper_props, config)
    with _GOLDEN_PATH.open(encoding="utf-8") as fh:
        golden_cases = json.load(fh)["cases"]
    assert results == golden_cases
