# ABOUTME: Tests for the demo_02 run CLI — argv-sniff dispatch, per-run report, lookup, rebuild-db.
# ABOUTME: Guards the golden sweep path untouched and exercises lookup/rebuild semantics.
"""demo_02 run CLI tests.

Run with:
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest examples/demos/gtm/tests/test_demo_02_cli.py -q
"""
from __future__ import annotations

import json
import os
import sqlite3
import subprocess
import sys
from pathlib import Path

import pytest

import demo_02_wall_thickness_multicode as demo

_GTM_DIR = Path(demo.__file__).resolve().parent
_REPO_ROOT = _GTM_DIR.parent.parent.parent
_SRC_DIR = _REPO_ROOT / "src"
_SCRIPT = _GTM_DIR / "demo_02_wall_thickness_multicode.py"
_GOLDEN_PATH = _GTM_DIR / "tests" / "fixtures" / "golden" / "demo_02_baseline_results.json"


def _env() -> dict:
    env = dict(os.environ)
    env["PYTHONPATH"] = os.pathsep.join([str(_GTM_DIR), str(_SRC_DIR)])
    env["PYTHONUNBUFFERED"] = "1"
    return env


def _run_cli(*args: str, results_dir: Path | None = None):
    """Invoke the demo script as a subprocess (cwd = repo root, like the other demo_02 tests)."""
    extra = ["--results-dir", str(results_dir)] if results_dir is not None else []
    return subprocess.run(
        [sys.executable, str(_SCRIPT), *args, *extra],
        capture_output=True, text=True, timeout=300,
        cwd=str(_REPO_ROOT), env=_env(),
    )


@pytest.fixture(scope="module")
def baseline_store(tmp_path_factory):
    """Build an isolated Results Store with a baseline run for read-only lookup tests."""
    base_dir = tmp_path_factory.mktemp("clistore_d2")
    result = _run_cli(results_dir=base_dir)
    assert result.returncode == 0, result.stderr[-800:]
    return base_dir


# ---------------------------------------------------------------------------
# Back-compat: the sweep path is untouched by the new subcommand machinery
# ---------------------------------------------------------------------------

def test_from_cache_still_exits_zero():
    """--from-cache exits 0 with the lookup/rebuild-db machinery present (BD back-compat)."""
    result = _run_cli("--from-cache")
    assert result.returncode == 0, result.stderr[-800:]
    assert "ImportError" not in result.stderr
    assert "ModuleNotFoundError" not in result.stderr


def test_no_arg_sweep_still_72(tmp_path):
    """A no-subcommand sweep is not intercepted by the argv-sniff and still produces 72 cases."""
    result = _run_cli(results_dir=tmp_path)
    assert result.returncode == 0, result.stderr[-800:]
    assert "Total cases analysed:  72" in result.stdout
    db = tmp_path / "parametric" / "demo_02" / "results.db"
    conn = sqlite3.connect(f"file:{db}?mode=ro", uri=True)
    try:
        n = conn.execute("SELECT COUNT(*) FROM cases WHERE run_id='baseline'").fetchone()[0]
    finally:
        conn.close()
    assert n == 72


# ---------------------------------------------------------------------------
# Named run: distinct run dir + SQLite run + per-run report.html
# ---------------------------------------------------------------------------

def test_named_run_writes_distinct_artifacts(tmp_path):
    """A named run writes its own store dir, a distinct SQLite run row, and a per-run report."""
    result = _run_cli("--run-id", "testrun", results_dir=tmp_path)
    assert result.returncode == 0, result.stderr[-800:]

    run_dir = tmp_path / "parametric" / "demo_02" / "testrun"
    report = _GTM_DIR / "output" / "parametric" / "demo_02" / "testrun" / "report.html"
    try:
        assert (run_dir / "cases.csv").is_file()
        assert (run_dir / "manifest.json").is_file()
        assert report.is_file(), \
            "named run must write output/parametric/demo_02/testrun/report.html"

        db = tmp_path / "parametric" / "demo_02" / "results.db"
        conn = sqlite3.connect(f"file:{db}?mode=ro", uri=True)
        try:
            runs = {r[0] for r in conn.execute("SELECT run_id FROM runs")}
            n = conn.execute(
                "SELECT COUNT(*) FROM cases WHERE run_id='testrun'"
            ).fetchone()[0]
        finally:
            conn.close()
        assert "testrun" in runs
        assert n == 72
    finally:
        # The report lands in the demo's real output/ tree (OUTPUT_DIR is module-level);
        # clean it up so the worktree stays tidy.
        import shutil
        shutil.rmtree(_GTM_DIR / "output" / "parametric" / "demo_02" / "testrun",
                      ignore_errors=True)


# ---------------------------------------------------------------------------
# lookup filter semantics against an isolated baseline store
# ---------------------------------------------------------------------------

def test_lookup_safe_count(baseline_store, capsys):
    """--safe returns the 48 passing baseline cases (is_safe TEXT token 'True')."""
    rc = demo.lookup_main(["--base-dir", str(baseline_store), "--run-id", "baseline", "--safe"])
    out = capsys.readouterr().out
    assert rc == 0
    assert "48 case(s) matched." in out


def test_lookup_unsafe_count(baseline_store, capsys):
    """--unsafe returns the 24 failing baseline cases (is_safe TEXT token 'False')."""
    rc = demo.lookup_main(["--base-dir", str(baseline_store), "--run-id", "baseline", "--unsafe"])
    out = capsys.readouterr().out
    assert rc == 0
    assert "24 case(s) matched." in out


def test_lookup_code_spaced_display_string(baseline_store, capsys):
    """--code with the SPACED display string returns rows."""
    rc = demo.lookup_main([
        "--base-dir", str(baseline_store), "--run-id", "baseline", "--code", "API RP 1111",
    ])
    out = capsys.readouterr().out
    assert rc == 0
    assert "24 case(s) matched." in out
    assert "API RP 1111" in out


def test_lookup_code_hyphen_unknown(baseline_store, capsys):
    """--code with a hyphenated (non-display) string is an unknown-code error, non-zero."""
    rc = demo.lookup_main([
        "--base-dir", str(baseline_store), "--run-id", "baseline", "--code", "API-RP-1111",
    ])
    out = capsys.readouterr().out
    assert rc != 0
    assert "unknown code; valid:" in out
    assert "Traceback" not in out


def test_lookup_pipe_size_literal_quote(baseline_store, capsys):
    """--pipe-size with the literal '\"' character binds verbatim and matches rows."""
    rc = demo.lookup_main([
        "--base-dir", str(baseline_store), "--run-id", "baseline", "--pipe-size", '12"',
    ])
    out = capsys.readouterr().out
    assert rc == 0
    assert "12 case(s) matched." in out


def test_lookup_safe_and_unsafe_mutually_exclusive(baseline_store):
    """--safe --unsafe together is a parser error (SystemExit)."""
    with pytest.raises(SystemExit):
        demo.lookup_main([
            "--base-dir", str(baseline_store), "--run-id", "baseline", "--safe", "--unsafe",
        ])


def test_lookup_zero_match_message(baseline_store, capsys):
    """A filter matching no rows prints the distinct zero-match message."""
    rc = demo.lookup_main([
        "--base-dir", str(baseline_store), "--run-id", "baseline", "--pressure", "999",
    ])
    out = capsys.readouterr().out
    assert rc == 0
    assert "0 cases match." in out


def test_lookup_unknown_run(baseline_store, capsys):
    """An unknown run_id prints the available-runs message and exits non-zero."""
    rc = demo.lookup_main(["--base-dir", str(baseline_store), "--run-id", "nope"])
    out = capsys.readouterr().out
    assert rc != 0
    assert "run_id 'nope' not found" in out
    assert "baseline" in out


def test_lookup_missing_db_does_not_create(tmp_path, capsys):
    """A missing db prints the run-a-sweep message, exits non-zero, and creates NO db."""
    rc = demo.lookup_main(["--base-dir", str(tmp_path), "--run-id", "baseline"])
    out = capsys.readouterr().out
    assert rc != 0
    assert "No results.db" in out
    assert "run a sweep first" in out
    assert not (tmp_path / "parametric" / "demo_02" / "results.db").exists()


def test_lookup_rejects_bad_run_id(baseline_store, capsys):
    """lookup rejects a traversal run_id non-zero with a clean message (no traceback)."""
    rc = demo.lookup_main(["--base-dir", str(baseline_store), "--run-id", "../../evil"])
    out = capsys.readouterr().out
    assert rc != 0
    assert "invalid run_id" in out
    assert "Traceback" not in out


# ---------------------------------------------------------------------------
# rebuild-db reports counts
# ---------------------------------------------------------------------------

def test_rebuild_db_reports(baseline_store, capsys):
    rc = demo.rebuild_main(["--base-dir", str(baseline_store)])
    out = capsys.readouterr().out
    assert rc == 0
    assert "Results Store rebuilt" in out
    assert "runs: 1" in out
    assert "cases: 72" in out
    assert "baseline: 72 cases" in out


# ---------------------------------------------------------------------------
# run_id validation (path-traversal / empty / collision)
# ---------------------------------------------------------------------------

def test_named_sweep_rejects_bad_run_id_no_outside_dir(tmp_path):
    """A bad --run-id is rejected non-zero by the sweep path and creates NO dir outside the store."""
    result = _run_cli("--run-id", "../../evil", results_dir=tmp_path)
    assert result.returncode != 0, result.stdout[-800:]
    assert "Traceback" not in result.stderr
    # Path-traversal target: ../../evil from the store root would land here.
    assert not (tmp_path.parent / "evil").exists()
    assert not (tmp_path / "parametric" / "demo_02" / "evil").exists()


@pytest.mark.parametrize("good_id", ["baseline", "acme_q2"])
def test_validate_run_id_accepts_good(good_id):
    """validate_run_id accepts the Baseline id and ordinary client run names."""
    assert demo.validate_run_id(good_id) == good_id


@pytest.mark.parametrize("bad_id", ["../../evil", "a/b", "", ".", ".."])
def test_validate_run_id_rejects_bad(bad_id):
    """validate_run_id raises ValueError for traversal / separators / empty / dot ids."""
    with pytest.raises(ValueError):
        demo.validate_run_id(bad_id)


# ---------------------------------------------------------------------------
# Golden still green (re-run inline via subprocess into an isolated store)
# ---------------------------------------------------------------------------

def test_golden_still_green(tmp_path):
    """The byte-identity oracle still holds with the CLI changes present."""
    result = _run_cli(results_dir=tmp_path)
    assert result.returncode == 0, result.stderr[-800:]
    produced = json.loads((tmp_path / "demo_02_wall_thickness_results.json").read_text())["results"]
    golden = json.loads(_GOLDEN_PATH.read_text())["results"]
    assert produced == golden
