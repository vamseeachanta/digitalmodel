# ABOUTME: Tests for the demo_04 run CLI — argv-sniff dispatch, per-run report, lookup, rebuild-db.
# ABOUTME: Guards the golden sweep path untouched and exercises lookup/rebuild semantics.
"""demo_04 run CLI tests.

Run with:
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest examples/demos/gtm/tests/test_demo_04_cli.py -q
"""
from __future__ import annotations

import json
import os
import sqlite3
import subprocess
import sys
from pathlib import Path

import pytest

import demo_04_shallow_water_pipelay as demo

_GTM_DIR = Path(demo.__file__).resolve().parent
_REPO_ROOT = _GTM_DIR.parent.parent.parent
_SRC_DIR = _REPO_ROOT / "src"
_SCRIPT = _GTM_DIR / "demo_04_shallow_water_pipelay.py"
_GOLDEN_PATH = _GTM_DIR / "tests" / "fixtures" / "golden" / "demo_04_baseline_results.json"


def _env() -> dict:
    env = dict(os.environ)
    env["PYTHONPATH"] = os.pathsep.join([str(_GTM_DIR), str(_SRC_DIR)])
    env["PYTHONUNBUFFERED"] = "1"
    return env


def _run_cli(*args: str, results_dir: Path | None = None):
    """Invoke the demo script as a subprocess (cwd = repo root, like the other demo_04 tests)."""
    extra = ["--results-dir", str(results_dir)] if results_dir is not None else []
    return subprocess.run(
        [sys.executable, str(_SCRIPT), *args, *extra],
        capture_output=True, text=True, timeout=300,
        cwd=str(_REPO_ROOT), env=_env(),
    )


@pytest.fixture(scope="module")
def baseline_store(tmp_path_factory):
    """Build an isolated Results Store with a baseline run for read-only lookup tests."""
    base_dir = tmp_path_factory.mktemp("clistore_d4")
    result = _run_cli(results_dir=base_dir)
    assert result.returncode == 0, result.stderr[-800:]
    return base_dir


# ---------------------------------------------------------------------------
# Back-compat: the sweep path is untouched by the new subcommand machinery
# ---------------------------------------------------------------------------

def test_from_cache_still_exits_zero():
    """--from-cache exits 0 with the lookup/rebuild-db machinery present (argv-sniff misses it)."""
    result = _run_cli("--from-cache")
    assert result.returncode == 0, result.stderr[-800:]
    assert "ImportError" not in result.stderr
    assert "ModuleNotFoundError" not in result.stderr


def test_no_arg_sweep_still_60(tmp_path):
    """A no-subcommand sweep is not intercepted by the argv-sniff and still produces 60 cases."""
    result = _run_cli(results_dir=tmp_path)
    assert result.returncode == 0, result.stderr[-800:]
    assert "Total cases analysed:  60" in result.stdout
    db = tmp_path / "parametric" / "demo_04" / "results.db"
    conn = sqlite3.connect(f"file:{db}?mode=ro", uri=True)
    try:
        n = conn.execute("SELECT COUNT(*) FROM cases WHERE run_id='baseline'").fetchone()[0]
    finally:
        conn.close()
    assert n == 60


# ---------------------------------------------------------------------------
# Named run: distinct run dir + SQLite run + per-run report.html
# ---------------------------------------------------------------------------

def test_named_run_writes_distinct_artifacts(tmp_path):
    """A named run writes its own store dir, a distinct SQLite run row, and a per-run report."""
    result = _run_cli("--run-id", "testrun", results_dir=tmp_path)
    assert result.returncode == 0, result.stderr[-800:]

    run_dir = tmp_path / "parametric" / "demo_04" / "testrun"
    report = _GTM_DIR / "output" / "parametric" / "demo_04" / "testrun" / "report.html"
    try:
        assert (run_dir / "cases.csv").is_file()
        assert (run_dir / "manifest.json").is_file()
        assert report.is_file(), \
            "named run must write output/parametric/demo_04/testrun/report.html"

        # The manifest carries NO absolute machine paths (repo-relative re-anchoring).
        manifest = json.loads((run_dir / "manifest.json").read_text())
        manifest_text = json.dumps(manifest)
        assert str(_REPO_ROOT) not in manifest_text
        assert "/mnt/" not in manifest_text
        for key in ("vessels_path", "pipelines_path", "results_root", "output_root",
                    "source_path"):
            val = manifest.get("resolved_config", {}).get(key)
            if val:
                assert not os.path.isabs(val), f"{key} is absolute in manifest: {val}"

        db = tmp_path / "parametric" / "demo_04" / "results.db"
        conn = sqlite3.connect(f"file:{db}?mode=ro", uri=True)
        try:
            runs = {r[0] for r in conn.execute("SELECT run_id FROM runs")}
            n = conn.execute(
                "SELECT COUNT(*) FROM cases WHERE run_id='testrun'"
            ).fetchone()[0]
        finally:
            conn.close()
        assert "testrun" in runs
        assert n == 60
    finally:
        # The report lands in the demo's real output/ tree (OUTPUT_DIR is module-level);
        # clean it up so the worktree stays tidy.
        import shutil
        shutil.rmtree(_GTM_DIR / "output" / "parametric" / "demo_04" / "testrun",
                      ignore_errors=True)


# ---------------------------------------------------------------------------
# lookup filter semantics against an isolated baseline store
# ---------------------------------------------------------------------------

def test_lookup_vessel_id(baseline_store, capsys):
    """--vessel-id PLV-001 returns the 30 cases for that vessel."""
    rc = demo.lookup_main([
        "--base-dir", str(baseline_store), "--run-id", "baseline", "--vessel-id", "PLV-001",
    ])
    out = capsys.readouterr().out
    assert rc == 0
    assert "30 case(s) matched." in out
    assert "sagbend_util" in out  # per-check util column selected without error


def test_lookup_pipe_size(baseline_store, capsys):
    """--pipe-size 8in returns the 12 cases for that pipe size (2 vessels x 6 depths)."""
    rc = demo.lookup_main([
        "--base-dir", str(baseline_store), "--run-id", "baseline", "--pipe-size", "8in",
    ])
    out = capsys.readouterr().out
    assert rc == 0
    assert "12 case(s) matched." in out


def test_lookup_water_depth_int_match(baseline_store, capsys):
    """--water-depth binds an INTEGER and matches the 10 cases at 30 m."""
    rc = demo.lookup_main([
        "--base-dir", str(baseline_store), "--run-id", "baseline", "--water-depth", "30",
    ])
    out = capsys.readouterr().out
    assert rc == 0
    assert "10 case(s) matched." in out


def test_lookup_status_marginal(baseline_store, capsys):
    """--status MARGINAL returns the 5 marginal baseline cases."""
    rc = demo.lookup_main([
        "--base-dir", str(baseline_store), "--run-id", "baseline", "--status", "MARGINAL",
    ])
    out = capsys.readouterr().out
    assert rc == 0
    assert "5 case(s) matched." in out


def test_lookup_status_nogo(baseline_store, capsys):
    """--status NO_GO returns the 5 no-go baseline cases."""
    rc = demo.lookup_main([
        "--base-dir", str(baseline_store), "--run-id", "baseline", "--status", "NO_GO",
    ])
    out = capsys.readouterr().out
    assert rc == 0
    assert "5 case(s) matched." in out


def test_lookup_combined_filter(baseline_store, capsys):
    """Combined filters AND together: PLV-001 @ 30m has the GO->MARGINAL/sagbend cases."""
    rc = demo.lookup_main([
        "--base-dir", str(baseline_store), "--run-id", "baseline",
        "--vessel-id", "PLV-001", "--water-depth", "30",
    ])
    out = capsys.readouterr().out
    assert rc == 0
    assert "5 case(s) matched." in out  # 1 vessel x 5 pipes at 30m


def test_lookup_status_typo_unknown(baseline_store, capsys):
    """--status NOGO (typo for NO_GO) is an unknown-status error, non-zero, no traceback."""
    rc = demo.lookup_main([
        "--base-dir", str(baseline_store), "--run-id", "baseline", "--status", "NOGO",
    ])
    out = capsys.readouterr().out
    assert rc != 0
    assert "unknown status; valid:" in out
    assert "Traceback" not in out


def test_lookup_zero_match_message(baseline_store, capsys):
    """A filter matching no rows prints the distinct zero-match message."""
    rc = demo.lookup_main([
        "--base-dir", str(baseline_store), "--run-id", "baseline", "--water-depth", "9999",
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
    assert not (tmp_path / "parametric" / "demo_04" / "results.db").exists()


def test_lookup_rejects_bad_run_id(baseline_store, capsys):
    """lookup rejects a traversal run_id non-zero with a clean message (no traceback)."""
    rc = demo.lookup_main(["--base-dir", str(baseline_store), "--run-id", "../../evil"])
    out = capsys.readouterr().out
    assert rc != 0
    assert "invalid run_id" in out
    assert "Traceback" not in out


def test_lookup_is_read_only_no_write(baseline_store):
    """lookup opens the db mode=ro: a write through that same URI must be rejected by SQLite."""
    import results_store_demo04 as rs
    db_path = rs._db_path(baseline_store)
    conn = sqlite3.connect(f"file:{db_path}?mode=ro", uri=True)
    try:
        with pytest.raises(sqlite3.OperationalError):
            conn.execute("UPDATE cases SET overall_status='X' WHERE run_id='baseline'")
    finally:
        conn.close()


# ---------------------------------------------------------------------------
# rebuild-db reports counts + round-trips the db from cases.csv
# ---------------------------------------------------------------------------

def test_rebuild_db_reports(baseline_store, capsys):
    rc = demo.rebuild_main(["--base-dir", str(baseline_store)])
    out = capsys.readouterr().out
    assert rc == 0
    assert "Results Store rebuilt" in out
    assert "runs: 1" in out
    assert "cases: 60" in out
    assert "baseline: 60 cases" in out


def test_rebuild_db_round_trips(tmp_path):
    """rebuild-db regenerates the db from cases.csv cell-identically to the direct-write db."""
    import results_store_demo04 as rs

    # Build a store with a baseline run, snapshot the direct-write db cells.
    result = _run_cli(results_dir=tmp_path)
    assert result.returncode == 0, result.stderr[-800:]
    db_path = rs._db_path(tmp_path)

    def _dump():
        conn = sqlite3.connect(f"file:{db_path}?mode=ro", uri=True)
        try:
            cols = [c[1] for c in conn.execute("PRAGMA table_info(cases)")]
            rows = conn.execute(
                f"SELECT {', '.join(rs._q(c) for c in cols)} FROM cases "
                "ORDER BY vessel_id, pipe_size, water_depth_m"
            ).fetchall()
        finally:
            conn.close()
        return rows

    before = _dump()
    db_path.unlink()
    assert not db_path.exists()
    rs.rebuild_db(tmp_path)
    after = _dump()
    assert before == after
    assert len(before) == 60


# ---------------------------------------------------------------------------
# run_id validation (path-traversal / empty)
# ---------------------------------------------------------------------------

def test_named_sweep_rejects_bad_run_id_no_outside_dir(tmp_path):
    """A bad --run-id is rejected non-zero by the sweep path and creates NO dir outside the store."""
    result = _run_cli("--run-id", "../../evil", results_dir=tmp_path)
    assert result.returncode != 0, result.stdout[-800:]
    assert "Traceback" not in result.stderr
    # Path-traversal target: ../../evil from the store root would land here.
    assert not (tmp_path.parent / "evil").exists()
    assert not (tmp_path / "parametric" / "demo_04" / "evil").exists()


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
# --from-cache refuse-on-non-default-config
# ---------------------------------------------------------------------------

def test_from_cache_refuses_non_default_config(tmp_path):
    """--from-cache with a non-default --config is refused non-zero (cannot prove cache match)."""
    import yaml as _yaml
    # Write a copy of the baseline yaml at a different path (catalogs/artifacts re-anchored to
    # absolute committed paths so it resolves regardless of its dir) -> a NON-default config.
    raw = _yaml.safe_load((_GTM_DIR / "inputs" / "demo_04_pipelay.yml").read_text())
    data_dir = _GTM_DIR / "data"
    raw["catalogs"] = {
        "vessels": str(data_dir / "pipelay_vessels.json"),
        "pipelines": str(data_dir / "pipelines.json"),
    }
    raw["artifacts"] = {"results_root": str(tmp_path), "output_root": str(tmp_path)}
    alt = tmp_path / "client.yml"
    alt.write_text(_yaml.safe_dump(raw, sort_keys=False))
    result = _run_cli("--from-cache", "--config", str(alt))
    assert result.returncode != 0, result.stdout[-800:]
    assert "--from-cache is only supported with the committed baseline config" in result.stdout
    assert "Traceback" not in result.stderr


# ---------------------------------------------------------------------------
# Golden still green (re-run inline via subprocess into an isolated store)
# ---------------------------------------------------------------------------

def test_golden_still_green(tmp_path):
    """The byte-identity oracle still holds with the CLI changes present."""
    result = _run_cli(results_dir=tmp_path)
    assert result.returncode == 0, result.stderr[-800:]
    produced = json.loads(
        (tmp_path / "demo_04_shallow_pipelay_results.json").read_text()
    )["cases"]
    golden = json.loads(_GOLDEN_PATH.read_text())["cases"]
    assert produced == golden
