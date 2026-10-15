# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise exact failure identities and fail-closed baseline comparisons."""

import importlib.util
import json
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("ci_baseline", ROOT / "scripts/ci/ci_baseline.py")
ci = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(ci)
SCR = "tests/orcaflex/test_generic_scr_slwr.py::test_analytical_reference_reproduces_from_basis"
NOW = datetime(2026, 10, 15, 12, tzinfo=timezone.utc)


def result(shard="example", ids=(SCR,), conclusion="failure", **updates):
    """Build a synthetic completed shard result."""
    row = dict(shard=shard, sha="abc123", run_id="123", attempt=1,
               completed_at=NOW.isoformat(), conclusion=conclusion, parsed=True,
               failed=list(ids), errored=[], collected=len(ids))
    return dict(row, **updates)


def write_results(tmp_path, *rows):
    """Write isolated input artifacts."""
    directory = tmp_path / "results"
    directory.mkdir(exist_ok=True)
    for row in rows:
        (directory / f"result-{row['shard']}.json").write_text(json.dumps(row))
    return directory


def baseline(tmp_path, row=None):
    """Publish a synthetic main observation."""
    directory = write_results(tmp_path, row or result())
    output = tmp_path / "baseline.json"
    assert ci.main(["publish", "--results", str(directory), "--previous", "none",
                    "--out", str(output)]) == 0
    return output


def compare(tmp_path, base, rows):
    """Compare at the fixed observation time."""
    directory = write_results(tmp_path, *rows)
    summary, verdict = tmp_path / "summary.md", tmp_path / "verdict.json"
    status = ci.compare_results(base, directory, 24, summary, verdict, now=NOW)
    return status, summary.read_text(), json.loads(verdict.read_text())


def test_all_preexisting_names_main_run(tmp_path):
    base = baseline(tmp_path)
    status, summary, verdict = compare(tmp_path, base, [result()])
    assert status == 0
    assert SCR in summary and "123" in summary
    assert verdict["pre_existing"] == [SCR]


def test_new_id_alongside_preexisting_fails(tmp_path):
    base = baseline(tmp_path)
    assert compare(tmp_path, base, [result(ids=(SCR, "tests/x.py::test_new"))])[0] == 1


@pytest.mark.parametrize("updates", [
    {"parsed": False}, {"failed": [], "collected": 0},
    {"conclusion": "cancelled"}, {"conclusion": "timed_out"},
])
def test_unproven_result_fails(tmp_path, updates):
    base = baseline(tmp_path)
    assert compare(tmp_path, base, [result(**updates)])[0] == 1


@pytest.mark.parametrize("kind", ["stale", "success", "missing", "future", "unparsed"])
def test_unproven_baseline_fails(tmp_path, kind):
    row = result()
    if kind == "stale":
        row["completed_at"] = (NOW - timedelta(hours=25)).isoformat()
    elif kind == "future":
        row["completed_at"] = (NOW + timedelta(seconds=1)).isoformat()
    elif kind == "success":
        row.update(conclusion="success", failed=[])
    elif kind == "unparsed":
        row["parsed"] = False
    base = baseline(tmp_path, row)
    if kind == "missing":
        base = tmp_path / "absent.json"
    assert compare(tmp_path, base, [result()])[0] == 1


def extract(tmp_path, text, kind="extract", conclusion="failure"):
    """Extract a synthetic JUnit file or hosted log."""
    source, output = tmp_path / "input", tmp_path / "output.json"
    source.write_text(text)
    flag = "--junit" if kind == "extract" else "--log"
    assert ci.main([kind, flag, str(source), "--shard", "example", "--sha", "abc123",
                    "--run", "123", "--attempt", "1", "--conclusion", conclusion,
                    "--completed-at", NOW.isoformat(), "--out", str(output)]) == 0
    return json.loads(output.read_text())


@pytest.mark.parametrize("content", ["", "<broken>"])
def test_empty_or_invalid_junit_fails(tmp_path, content):
    assert extract(tmp_path, content)["parsed"] is False


def test_missing_junit_is_unparsed(tmp_path):
    output = tmp_path / "out.json"
    ci.main(["extract", "--junit", str(tmp_path / "absent"), "--shard", "example",
             "--sha", "abc", "--run", "1", "--attempt", "1", "--conclusion", "failure",
             "--out", str(output)])
    assert json.loads(output.read_text())["parsed"] is False


def test_junit_and_log_scr_id_agree(tmp_path):
    xml = ('<testsuites><testsuite tests="1" failures="1" errors="0">'
           '<testcase classname="tests.orcaflex.test_generic_scr_slwr" '
           'name="test_analytical_reference_reproduces_from_basis"><failure/></testcase>'
           '</testsuite></testsuites>')
    assert extract(tmp_path, xml)["failed"] == [SCR]
    log = f"2026-10-15T10:00:00.000Z FAILED {SCR} - AssertionError\n"
    assert extract(tmp_path, log, "extract-log")["failed"] == [SCR]


def test_collection_error_and_parameter_spaces_roundtrip(tmp_path):
    node = "tests/x.py::TestThing::test_case[a b - c]"
    text = f"ERROR tests/x.py - ImportError\nFAILED {node} - AssertionError\n"
    row = extract(tmp_path, text, "extract-log")
    assert row["errored"] == ["tests/x.py"] and row["failed"] == [node]
    base = baseline(tmp_path, row)
    assert compare(tmp_path, base, [row])[0] == 0
    xml = ('<testsuites><testsuite tests="2" errors="1" failures="1">'
           '<testcase classname="" name="tests/x.py"><error/></testcase>'
           '<testcase classname="tests.x.TestThing" file="tests/x.py" '
           'name="test_case[a b - c]"><failure/></testcase></testsuite></testsuites>')
    assert extract(tmp_path, xml)["failed"] == [node]
    assert extract(tmp_path, xml)["errored"] == ["tests/x.py"]


def test_publish_carries_cancelled_shard_timestamp(tmp_path):
    base = baseline(tmp_path)
    directory = write_results(tmp_path, result(conclusion="cancelled", parsed=False))
    output = tmp_path / "next.json"
    ci.main(["publish", "--results", str(directory), "--previous", str(base), "--out", str(output)])
    assert json.loads(output.read_text()) == json.loads(base.read_text())


def test_publish_success_clears_old_failures(tmp_path):
    base = baseline(tmp_path)
    directory = write_results(tmp_path, result(ids=(), conclusion="success"))
    output = tmp_path / "next.json"
    ci.main(["publish", "--results", str(directory), "--previous", str(base), "--out", str(output)])
    assert json.loads(output.read_text())["tests"] == {}


def test_missing_or_invalid_results_fail_closed(tmp_path):
    base = baseline(tmp_path)
    empty = tmp_path / "empty"
    empty.mkdir()
    assert ci.compare_results(base, empty, 24, tmp_path / "s", tmp_path / "v", now=NOW) == 1
    (empty / "result-bad.json").write_text("{")
    assert ci.compare_results(base, empty, 24, tmp_path / "s", tmp_path / "v", now=NOW) == 1


def test_workflow_keeps_required_name_and_infrastructure_failures():
    workflow = yaml.safe_load((ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text())
    job = workflow["jobs"]["domain-tests-required"]
    assert job["name"] == "Domain test aggregate"
    step = next(s for s in job["steps"] if s["name"] == "Enforce required domain result")
    assert step["env"]["CI_BASELINE_MODE"] == "report"
    for variable in ("DETECT_RESULT", "HARNESS_RESULT"):
        assert f'if [ "${variable}" != "success" ]; then' in step["run"]
        block = step["run"].split(f'if [ "${variable}" != "success" ]; then')[1].split("fi")[0]
        assert "exit 1" in block
    assert '"$CI_BASELINE_MODE" = "report"' in step["run"]
    assert "tests/scripts/test_ci_baseline.py" in str(workflow["jobs"]["ci-harness-tests"])


def test_junit_missing_failure_details_is_unparsed(tmp_path):
    text = '<testsuites><testsuite tests="1" failures="1" errors="0"><testcase name="test_x" classname="tests.test_x"/></testsuite></testsuites>'
    assert extract(tmp_path, text)["parsed"] is False


def test_same_id_in_different_shard_is_new(tmp_path):
    base = baseline(tmp_path)
    assert compare(tmp_path, base, [result(shard="other")])[0] == 1


def test_test_metadata_must_match_shard_observation(tmp_path):
    base = baseline(tmp_path)
    value = json.loads(base.read_text())
    value["tests"][SCR]["run_id"] = "older"
    base.write_text(json.dumps(value))
    assert compare(tmp_path, base, [result()])[0] == 1


def test_publish_absent_shard_carries_forward(tmp_path):
    base = baseline(tmp_path)
    before = json.loads(base.read_text())
    empty = tmp_path / "empty"
    empty.mkdir()
    output = tmp_path / "next.json"
    ci.main(["publish", "--results", str(empty), "--previous", str(base), "--out", str(output)])
    assert json.loads(output.read_text()) == before


def test_publish_new_unparsed_observation_replaces_old_red(tmp_path):
    base = baseline(tmp_path)
    directory = write_results(tmp_path, result(parsed=False, run_id="456"))
    output = tmp_path / "next.json"
    ci.main(["publish", "--results", str(directory), "--previous", str(base), "--out", str(output)])
    assert compare(tmp_path, output, [result()])[0] == 1


def test_missing_failed_job_artifact_is_materialized(tmp_path):
    base = baseline(tmp_path)
    directory = tmp_path / "downloads"
    directory.mkdir()
    jobs = [dict(name="tests-example", completed_at=NOW.isoformat(), conclusion="failure")]
    ci.reconcile_results(directory, jobs, "tests-", "abc123", "123", 1)
    rows = ci.load_results(directory)
    assert len(rows) == 1 and rows[0]["parsed"] is False
    assert ci.compare_results(base, directory, 24, tmp_path / "s", tmp_path / "v", now=NOW) == 1


def test_duplicate_shard_artifacts_fail_closed(tmp_path):
    base = baseline(tmp_path)
    directory = tmp_path / "downloads"
    for sub in ("a", "b"):
        folder = directory / sub
        folder.mkdir(parents=True)
        (folder / "result-example.json").write_text(json.dumps(result()))
    assert ci.compare_results(base, directory, 24, tmp_path / "s", tmp_path / "v", now=NOW) == 1


def test_unparsed_main_does_not_resolve_prior_failure(tmp_path):
    base = baseline(tmp_path)
    directory = write_results(tmp_path, result(parsed=False, ids=(), run_id="456"))
    output = tmp_path / "next.json"
    ci.main(["publish", "--results", str(directory), "--previous", str(base), "--out", str(output)])
    value = json.loads(output.read_text())
    assert SCR in value["tests"]
    assert value["shards"]["example"]["parsed"] is False
    assert compare(tmp_path, output, [result()])[0] == 1


def test_pytest_xunit2_collection_module_matches_log(tmp_path):
    xml = ('<testsuites><testsuite tests="1" failures="0" errors="1">'
           '<testcase classname="" name="tests.x">'
           '<error message="collection failure"/></testcase></testsuite></testsuites>')
    row = extract(tmp_path, xml)
    assert row["parsed"] is True
    assert row["errored"] == ["tests/x.py"]
    assert extract(tmp_path, "ERROR tests/x.py - ImportError", "extract-log")["errored"] == row["errored"]


def test_reader_rejects_artifact_older_than_newest_completed_main_run(tmp_path, monkeypatch):
    artifact = dict(id=10, expired=False, workflow_run=dict(head_branch="main", id=100))
    def metadata(endpoint):
        if "actions/artifacts?" in endpoint:
            return {"artifacts": [artifact]}
        if "workflows/" in endpoint:
            return {"workflow_runs": [{"id": 101}]}
        return dict(id=100, path=".github/workflows/full-matrix-sweep.yml", head_branch="main")
    monkeypatch.setattr(ci, "gh_json", metadata)
    assert ci.download_baseline("example/repository", tmp_path / "baseline.json", require_latest=True) is False
    assert not (tmp_path / "baseline.json").exists()


def test_prior_attempt_result_cannot_prove_current_failure(tmp_path):
    directory = write_results(tmp_path, result(attempt=1))
    jobs = [dict(name="tests-example", completed_at=NOW.isoformat(), conclusion="failure")]
    ci.reconcile_results(directory, jobs, "tests-", "abc123", "123", 2)
    assert ci.load_results(directory)[0]["parsed"] is False
