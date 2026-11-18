"""Matched runner timings from a two-run, three-job offline snapshot."""

import io
import json
import subprocess
from pathlib import Path
from zipfile import ZipFile

import pytest

from scripts.ci import step_timings as timing
from scripts.ci.geotechnical_timing import solver_timing
from scripts.ci.shard_log_timings import shard_phases

FIXTURE = json.loads((Path(__file__).parent / "fixtures/step_timings.json").read_text())


def test_report_matched_timings(tmp_path):
    for name, payload in FIXTURE["snapshot"].items():
        (tmp_path / name).write_text(json.dumps(payload))
    data = timing.report(tmp_path)
    first, second, partial = data["jobs"]
    assert first["setup_pct"] == 20 and second["test_step_pct"] == 60
    assert first["depth"] == 0
    assert partial["setup_pct"] is None and partial["classes"]["checkout"] is None
    assert data["summary"]["sweep/one"]["checkout"] == dict(n=2, unknown=0, median=15, p90=20)
    assert data["summary"]["sweep/two"]["checkout"]["unknown"] == 1


def test_missing_run_and_test_step_fail_closed(tmp_path):
    (tmp_path / "index.json").write_text(json.dumps({"runs": [{"id": 1, "workflow": "pr"}]}))
    with pytest.raises(FileNotFoundError):
        timing.report(tmp_path)
    assert timing.measure({"id": 1, "name": "tests-x", "steps": []}, {}, tmp_path)["test_step_pct"] is None


def test_solver_log_isolates_command_and_times_both_phases():
    log = "\n".join(FIXTURE["solver"])
    assert solver_timing(log) == {
        "install_s": 3.0, "pytest_s": 10.51, "collected": 42, "passed": 40,
    }


def test_solver_truncation_never_borrows_counts_or_install_time():
    log = "\n".join(FIXTURE["truncated"])
    assert solver_timing(log) == dict.fromkeys(("install_s", "pytest_s", "collected", "passed"))


def test_gate_log_separates_checkout_install_pytest_and_progress():
    log = "\n".join(FIXTURE["domain"])
    assert shard_phases(log) == dict(depth=0, install_s=2.0, pytest_s=5.01, geo_progress_s=3.0)


def test_unknown_gate_and_checkout_are_null():
    assert all(v is None for v in shard_phases("pytest 99 passed in 5s").values())


def test_fetch_and_resume_preserve_run_and_attempt(tmp_path, monkeypatch):
    """Simulate a quota break between metadata and logs."""

    def get(path):
        if "actions/jobs/" in path:
            raise subprocess.CalledProcessError(1, "gh", stderr=b"quota")
        row = dict(id=42, run_id=7, run_attempt=2, name="tests-specialized",
                   conclusion="success", started_at=None, completed_at=None,
                   steps=[{"name": "Run geotechnical solver benchmarks", "conclusion": "success"}])
        return json.dumps({"total_count": 1, "jobs": [row]}).encode()

    run = {"id": 7, "workflow": "pr"}
    monkeypatch.setattr(timing, "get", get)
    with pytest.raises(subprocess.CalledProcessError):
        timing.save_run(tmp_path, run)
    assert json.loads((tmp_path / "7.json").read_text())["jobs"][0]["run_attempt"] == 2

    def log_only(path):
        assert path == "actions/jobs/42/logs"
        return b"log"

    monkeypatch.setattr(timing, "get", log_only)
    monkeypatch.setattr(timing, "fetch_run_phases", lambda *args: None)
    timing.save_run(tmp_path, run)
    assert (tmp_path / "42.log.gz").is_file()


def test_matched_phase_shares_require_install_and_test(tmp_path):
    row = dict(id=4, name="tests-x", conclusion="success", started_at="2026-11-17T00:00:00Z",
               completed_at="2026-11-17T00:00:40Z")
    row["steps"] = [dict(name="Checkout code", started_at=row["started_at"],
                         completed_at="2026-11-17T00:00:10Z", conclusion="success"),
                    dict(name="Run domain gate", started_at="2026-11-17T00:00:10Z",
                         completed_at=row["completed_at"], conclusion="success")]
    evidence = {"depth": 1, "install_s": 5, "pytest_s": 20}
    job = timing.measure(row, {}, tmp_path, evidence)
    assert (job["setup_with_install_pct"], job["pytest_pct"]) == (37.5, 50)
    assert timing.measure(row, {}, tmp_path)["setup_with_install_pct"] is None


def test_retry_leaves_combined_gate_phase_times_unknown():
    log = "\n".join(FIXTURE["retry"])
    assert all(value is None for value in shard_phases(log).values())


def test_pending_run_is_visible_and_refreshes_on_resume(tmp_path, monkeypatch):
    run = {"id": 3, "workflow": "pr", "status": "queued"}
    (tmp_path / "index.json").write_text(json.dumps({"runs": [run]}))
    responses = iter([{"status": "queued"}, {"status": "completed"}, {"jobs": [], "total_count": 0}])
    monkeypatch.setattr(timing, "get", lambda _: json.dumps(next(responses)).encode())
    timing.save_run(tmp_path, run)
    assert timing.report(tmp_path)["pending_runs"] == [3]
    timing.save_run(tmp_path, run)
    assert timing.report(tmp_path)["pending_runs"] == []


def test_cancelled_test_step_does_not_lower_full_duration(tmp_path):
    start, end = "2026-11-17T00:00:00Z", "2026-11-17T00:00:05Z"
    row = dict(started_at=start, completed_at=end, conclusion="cancelled")
    job = dict(row, id=31, name="tests-x", steps=[
        dict(row, name="Checkout code", conclusion="success"), dict(row, name="Run domain gate")])
    result = timing.measure(job, {}, tmp_path)
    assert result["classes"] == {"checkout": 5, "domain_gate": None}
    assert result["wall_s"] is None and result["setup_pct"] is None


def test_paginated_get_requires_all_distinct_ids(monkeypatch):
    first = [{"id": i} for i in range(100)]
    final = [{"id": 100}]

    def get(path):
        rows = first if "page=1&" in path else final
        return json.dumps({"jobs": rows, "total_count": 101}).encode()

    monkeypatch.setattr(timing, "get", get)
    assert len(timing.pages("jobs", "jobs")) == 101
    final[0]["id"] = 1
    with pytest.raises(ValueError, match="Duplicate"):
        timing.pages("jobs", "jobs")
    final.clear()
    with pytest.raises(ValueError, match="Incomplete"):
        timing.pages("jobs", "jobs")


def test_same_shard_name_in_different_attempt_archives(tmp_path):
    def get(path):
        attempt = int(path.split("/")[-2])
        if attempt == 3:
            raise subprocess.CalledProcessError(1, "gh", stderr=b"expired (HTTP 410)")
        archive = io.BytesIO()
        with ZipFile(archive, "w") as z:
            z.writestr("2_tests-x.txt", "2026-11-17T00:00:00Z ##[group]Run actions/checkout@v4\n"
                       f"2026-11-17T00:00:00Z   fetch-depth: {attempt}\n2026-11-17T00:00:01Z ##[endgroup]")
        return archive.getvalue()

    jobs = [dict(id=10 * i, name="tests-x", run_attempt=i) for i in (1, 2, 3)]
    timing.fetch_run_phases(tmp_path, {"id": 7}, jobs, get)
    results = json.loads((tmp_path / "7-phases.json").read_text())
    assert {k: v["depth"] for k, v in results.items()} == {"10": 1, "20": 2}


def test_quiet_output_preserves_missing_collection_count():
    lines = [s for s in FIXTURE["solver"] if "test session starts" not in s and "collected" not in s]
    before_summary = next(i for i, s in enumerate(lines) if "40 passed" in s)
    lines[before_summary:before_summary] = [
        "2026-11-18T12:00:15Z tests/geotechnical/test_solver.py . [100%]",
        "2026-11-18T12:00:16Z Installed 99 packages in 5s"]
    assert solver_timing("\n".join(lines)) == dict(install_s=3.0, pytest_s=10.51, collected=None, passed=40)
