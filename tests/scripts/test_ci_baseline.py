# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise exact failure identities and fail-closed baseline comparisons."""

import importlib.util
import json
import os
import subprocess
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


EVIDENCE_URL = "https://github.com/example/repository/actions/runs/99"
EVIDENCE_NODE = "tests/test_new.py::test_racy"


def evidence_response(endpoint, **changes):
    if endpoint == "repos/example/repository":
        return {"id": 123}
    if "/jobs?" in endpoint:
        return {"jobs": [dict(id=7, name="tests-example", run_id=99, head_sha="mainsha",
                              status="completed", conclusion="failure")]}
    if endpoint == "repos/example/repository/actions/jobs/7":
        return evidence_response("/jobs?")["jobs"][0]
    return dict(dict(id=99, event="push", head_branch="main", status="completed",
                     head_sha="mainsha", head_repository={"full_name": "example/repository"}), **changes)


@pytest.fixture(autouse=True)
def evidence_metadata(monkeypatch):
    monkeypatch.delenv("GITHUB_EVENT_NAME", raising=False)
    monkeypatch.setenv("GITHUB_REPOSITORY", "example/repository")
    monkeypatch.setattr(ci, "gh_json", evidence_response)
    monkeypatch.setattr(ci, "gh_text", lambda endpoint: f"FAILED {EVIDENCE_NODE} - assertion\n")


def run_health_writer(body, *, active=True, existing=True):
    """Execute the shipped health script with isolated GitHub and file doubles."""
    workflow = yaml.safe_load((ROOT / '.github/workflows/full-matrix-sweep.yml').read_text())
    step = next(s for s in workflow['jobs']['sweep-result']['steps']
                if s.get('name') == 'Update main health')
    harness = r'''
    const input = JSON.parse(require('fs').readFileSync(0, 'utf8'));
    const calls = [], notices = [];
    const baseline = {tests: input.active ? {'tests/example.py::test_case':
      {shard: 'example', first_seen_run: 1, run_id: 10}} : {},
      shards: {example: {parsed: true, conclusion: 'success'}}};
    const fakeFs = {existsSync: () => false, readFileSync: () => JSON.stringify(baseline)};
    const issue = {number: 1, title: 'main-health: example', body: input.body};
    const github = {paginate: async (method) => method === 'issues' && input.existing ? [issue] : [],
      rest: {issues: {listForRepo: 'issues', listLabelsForRepo: 'labels',
        update: async value => calls.push({kind: 'update', ...value}),
        create: async value => calls.push({kind: 'create', ...value}),
        createLabel: async value => calls.push({kind: 'label', ...value})}}};
    const context = {repo: {owner: 'example', repo: 'example'},
      runNumber: 10, runId: 100, sha: 'abc123'};
    const core = {notice: value => notices.push(value)};
    const AsyncFunction = Object.getPrototypeOf(async function(){}).constructor;
    new AsyncFunction('require', 'github', 'context', 'core', input.script)(
      name => {if (name !== 'fs') throw Error(name); return fakeFs;}, github, context, core
    ).then(() => console.log(JSON.stringify({calls, notices})));
    '''
    completed = subprocess.run(
        ['node', '-e', harness], input=json.dumps(dict(
            script=step['with']['script'], body=body, active=active, existing=existing)),
        text=True, capture_output=True, check=True, timeout=30)
    return json.loads(completed.stdout)


@pytest.mark.parametrize('active', [True, False])
def test_health_older_sweep_never_updates_or_closes(active):
    body = '<!-- main-health-order: run_number=11 run_id=101 sha=abc123 -->'
    output = run_health_writer(body, active=active)
    assert output['calls'] == []
    assert len(output['notices']) == 1
    assert '11' in output['notices'][0] and '10' in output['notices'][0]


@pytest.mark.parametrize('body', [
    '<!-- main-health-order: run_number=10 run_id=100 sha=abc123 -->',
    '<!-- main-health-order: run_number=9 run_id=99 sha=abc123 -->',
    'Legacy issue body',
    '<!-- main-health-order: run_number=invalid run_id=101 sha=abc123 -->',
    '<!-- main-health-order: run_number=11 run_id=invalid sha=abc123 -->',
    None,
])
def test_health_equal_older_or_unparsed_marker_writes(body):
    output = run_health_writer(body)
    assert output['notices'] == []
    assert len(output['calls']) == 1
    update = output['calls'][0]
    assert update['kind'] == 'update' and update['state'] == 'open'
    assert '<!-- main-health-order: run_number=10 run_id=100 sha=abc123 -->' in update['body']
    assert run_health_writer(update['body'])['calls'][0]['body'] == update['body']


@pytest.mark.parametrize('active,existing,kinds', [
    (False, True, ['update']), (True, False, ['label', 'create']), (False, False, []),
])
def test_health_preserves_close_and_create_rules(active, existing, kinds):
    output = run_health_writer('', active=active, existing=existing)
    assert [call['kind'] for call in output['calls']] == kinds
    if existing:
        assert output['calls'][0]['state'] == 'closed'
    elif active:
        assert output['calls'][-1]['labels'] == ['main-health']
        assert 'main-health-order: run_number=10' in output['calls'][-1]['body']


def test_health_workflow_names_and_soft_failure_are_preserved():
    workflow = yaml.safe_load((ROOT / '.github/workflows/full-matrix-sweep.yml').read_text())
    assert {key: job['name'] for key, job in workflow['jobs'].items()} == {
        'full-matrix': 'Build full shard matrix',
        'sweep-tests': 'sweep-tests-${{ matrix.domain }}',
        'sweep-result': 'Full matrix sweep result',
    }
    steps = workflow['jobs']['sweep-result']['steps']
    assert [step['name'] for step in steps] == [
        'Summarize shard results', 'Checkout baseline tools', 'Download sweep results',
        'Publish main baseline', 'Upload main baseline', 'Update main health',
        'Enforce full sweep result',
    ]
    step = next(s for s in steps if s['name'] == 'Update main health')
    assert step['continue-on-error'] is True
    assert step['if'] == "github.ref == 'refs/heads/main'"


def result(shard="example", ids=(SCR,), conclusion="failure", **updates):
    """Build a synthetic completed shard result."""
    row = dict(shard=shard, sha="abc123", run_id="123", attempt=1,
               completed_at=NOW.isoformat(), conclusion=conclusion, parsed=True,
               failed=list(ids), errored=[], collected=len(ids), exit_code=1)
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


def extract(tmp_path, text, kind="extract", conclusion="failure", exit_code=1):
    """Extract a synthetic JUnit file or hosted log."""
    source, output = tmp_path / "input", tmp_path / "output.json"
    source.write_text(text)
    flag = "--junit" if kind == "extract" else "--log"
    assert ci.main([kind, flag, str(source), "--shard", "example", "--sha", "abc123",
                    "--run", "123", "--attempt", "1", "--conclusion", conclusion,
                    "--exit-code", str(exit_code),
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
    assert compare(tmp_path, base, [row])[0] == 1
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


def test_reader_falls_back_when_newest_sweep_was_cancelled(tmp_path, monkeypatch):
    runs = [sweep(101, conclusion="cancelled"), sweep()]
    mock_download(monkeypatch, runs, [artifact()], verified=runs[1])
    assert ci.download_baseline("example/repository", tmp_path / "baseline.json", True, NOW)
    assert (tmp_path / "baseline.json").exists()


@pytest.mark.parametrize("conclusion", ["success", "failure"])
@pytest.mark.parametrize("cancelled", [False, True])
@pytest.mark.parametrize("expired", [False, True])
def test_reader_stops_at_unproven_completed_sweep(tmp_path, monkeypatch, capsys,
                                                 conclusion, cancelled, expired):
    runs = [sweep(101, conclusion=conclusion), sweep()]
    if cancelled:
        runs.insert(0, sweep(102, conclusion="cancelled"))
    items = [artifact()]
    if expired:
        items.append(artifact(101, expired=True))
    mock_download(monkeypatch, runs, items)
    output = tmp_path / "baseline.json"
    assert ci.download_baseline("example/repository", output, True, NOW) is False
    assert "101" in capsys.readouterr().out
    assert not output.exists()
    assert compare(tmp_path, output, [result()])[0] == 1


@pytest.mark.parametrize("updates", [
    {"event": "pull_request"},
    {"head_repository": {"full_name": "fork/repository"}},
])
def test_reader_ignores_newer_unproven_run_without_provenance(tmp_path, monkeypatch, updates):
    runs = [sweep(101, **updates), sweep()]
    mock_download(monkeypatch, runs, [artifact()], verified=runs[1])
    assert ci.download_baseline("example/repository", tmp_path / "baseline.json", True, NOW)


def test_prior_attempt_result_cannot_prove_current_failure(tmp_path):
    directory = write_results(tmp_path, result(attempt=1))
    jobs = [dict(name="tests-example", completed_at=NOW.isoformat(), conclusion="failure")]
    ci.reconcile_results(directory, jobs, "tests-", "abc123", "123", 2)
    assert ci.load_results(directory)[0]["parsed"] is False


@pytest.mark.parametrize("updates", [
    {"exit_code": 2}, {"exit_code": None}, {"exit_code": 5},
    {"collected": 0}, {"failed": ["tests/x.py"]},
])
def test_incomplete_execution_fails_closed(tmp_path, updates):
    base = baseline(tmp_path)
    assert compare(tmp_path, base, [result(**updates)])[0] == 1


def test_missing_exit_code_fails_closed(tmp_path):
    base = baseline(tmp_path)
    row = result()
    del row["exit_code"]
    assert compare(tmp_path, base, [row])[0] == 1


def test_only_success_rows_pass_comparison(tmp_path):
    base = baseline(tmp_path)
    assert compare(tmp_path, base, [result(conclusion="success", ids=(), exit_code=0, collected=1)])[0] == 0


def sweep(run_id=100, **updates):
    """Build a same-repository completed main sweep."""
    row = dict(id=run_id, path=".github/workflows/full-matrix-sweep.yml",
               head_branch="main", event="push", head_repository={"full_name": "example/repository"},
               status="completed", conclusion="success", head_sha="abc123",
               updated_at=(NOW - timedelta(hours=1)).isoformat())
    return dict(row, **updates)


def artifact(run_id=100, **updates):
    """Build a baseline artifact bound to its producing run."""
    row = dict(id=run_id, name="main-baseline", expired=False,
               created_at=(NOW - timedelta(minutes=30)).isoformat(),
               workflow_run={"id": run_id, "head_sha": "abc123"})
    return dict(row, **updates)


@pytest.mark.parametrize("updates,accepted", [
    ({"event": "pull_request", "head_repository": {"full_name": "fork/repository"}}, False),
    ({"event": "pull_request"}, False), ({"event": "workflow_dispatch"}, False),
    ({"event": "push"}, True), ({"event": "schedule"}, True),
    ({"head_branch": "develop"}, False), ({"path": "other.yml"}, False),
])
def test_selection_requires_main_sweep_provenance(updates, accepted):
    selected = ci.select_baseline_run([sweep(**updates)], [artifact()], "example/repository", NOW)
    assert (selected is not None) is accepted


@pytest.mark.parametrize("newest,expected", [
    (sweep(101, conclusion="cancelled"), sweep()), (sweep(101), None),
])
def test_selection_falls_back_when_newest_has_no_baseline(newest, expected):
    assert ci.select_baseline_run([newest, sweep()], [artifact()], "example/repository", NOW) == expected


def test_selection_does_not_fall_back_to_stale_sweep():
    old = sweep(updated_at=(NOW - timedelta(hours=25)).isoformat())
    assert ci.select_baseline_run([sweep(101, conclusion="cancelled"), old],
                                  [artifact()], "example/repository", NOW) is None


def test_newest_published_sweep_wins_over_older_red():
    runs = [sweep(conclusion="failure"), sweep(101)]
    assert ci.select_baseline_run(runs, [artifact(), artifact(101)], "example/repository", NOW)["id"] == 101


@pytest.mark.parametrize("change", [
    {"expired": True}, {"created_at": (NOW + timedelta(seconds=1)).isoformat()},
])
def test_unavailable_artifact_is_not_selected(change):
    assert ci.select_baseline_run([sweep()], [artifact(**change)], "example/repository", NOW) is None


def test_extraction_does_not_invent_execution_count(tmp_path):
    assert extract(tmp_path, f"FAILED {SCR} - AssertionError", "extract-log")["collected"] == 0


def test_workflows_capture_exit_and_preserve_gate_conclusion():
    for path, job_id in (("full-matrix-sweep.yml", "sweep-tests"),
                         ("quality-gates-by-domain.yml", "domain-tests")):
        workflow = yaml.safe_load((ROOT / ".github/workflows" / path).read_text())
        steps = workflow["jobs"][job_id]["steps"]
        gate = next(s for s in steps if s["name"] == "Run domain gate")
        assert "set +e" in gate["run"] and 'exit "$code"' in gate["run"]
        assert 'echo "code=$code" >> "$GITHUB_OUTPUT"' in gate["run"]
        extraction = next(s for s in steps if s["name"] == "Extract shard result")
        assert "--exit-code" in extraction["run"]
    aggregate = workflow["jobs"]["domain-tests-required"]["steps"]
    enforce = next(s for s in aggregate if s["name"] == "Enforce required domain result")
    assert "GITHUB_EVENT_NAME" in enforce["run"] and '"pull_request"' in enforce["run"]
    upload = next(s for s in aggregate if s.get("with", {}).get("name") == "baseline-comparison")
    assert upload["if"] == "always()"
    assert set(upload["with"]["path"].split()) == {"baseline-verdict.json", "baseline-summary.md"}


def mock_download(monkeypatch, runs, artifacts, verified=None):
    """Serve metadata and a minimal archive through the existing CLI boundary."""
    import io
    import zipfile
    payload = io.BytesIO()
    with zipfile.ZipFile(payload, "w") as archive:
        archive.writestr("baseline.json", json.dumps({"tests": {}, "shards": {}}))
    def metadata(endpoint):
        if "artifacts?" in endpoint:
            return {"artifacts": artifacts}
        if "workflows/" in endpoint:
            return {"workflow_runs": runs}
        return verified or runs[0]
    monkeypatch.setattr(ci, "gh_json", metadata)
    monkeypatch.setattr(ci.subprocess, "check_output", lambda *args, **kwargs: payload.getvalue())


@pytest.mark.parametrize("event", ["push", "schedule"])
def test_reader_downloads_verified_own_main(tmp_path, monkeypatch, event):
    run = sweep(event=event)
    mock_download(monkeypatch, [run], [artifact()], verified=dict(run, extra="detail"))
    assert ci.download_baseline("example/repository", tmp_path / "baseline.json", True, NOW)
    assert json.loads((tmp_path / "baseline.json").read_text()) == {"tests": {}, "shards": {}}


def test_reader_rejects_changed_provenance(tmp_path, monkeypatch):
    mock_download(monkeypatch, [sweep()], [artifact()], verified=sweep(event="pull_request"))
    with pytest.raises(ValueError):
        ci.download_baseline("example/repository", tmp_path / "baseline.json", True, NOW)


def test_reader_does_not_fall_back_from_mismatched_artifact(tmp_path, monkeypatch):
    items = [artifact(101, workflow_run={"id": 101, "head_sha": "different"}), artifact()]
    mock_download(monkeypatch, [sweep(101), sweep(conclusion="failure")], items)
    with pytest.raises(ValueError):
        ci.download_baseline("example/repository", tmp_path / "baseline.json", True, NOW)


@pytest.mark.parametrize("change", [{"exit_code": None}, {"exit_code": 2}, {"collected": 0}])
def test_main_execution_must_also_be_proven(tmp_path, change):
    base = baseline(tmp_path, result(**change))
    assert compare(tmp_path, base, [result()])[0] == 1


def test_reader_stale_only_fallback_fails_closed(tmp_path, monkeypatch):
    runs = [sweep(101, conclusion="cancelled"),
            sweep(updated_at=(NOW - timedelta(hours=25)).isoformat())]
    mock_download(monkeypatch, runs, [artifact()])
    assert ci.download_baseline("example/repository", tmp_path / "baseline.json", True, NOW) is False


def test_n2_successful_recovery_is_compared_as_new(tmp_path):
    base = baseline(tmp_path)
    node = "tests/test_new.py::test_racy"
    row = result(ids=(node,), conclusion="success", exit_code=0,
                 first_attempt_exit_code=1, recovered=[node],
                 first_attempt_failed={node: "first assertion"})
    status, summary, verdict = compare(tmp_path, base, [row])
    assert status == 1
    assert verdict["shards"][0]["ids"][node] == "NEW"
    assert node in summary


def test_n11_partial_execution_is_never_preexisting(tmp_path):
    base = baseline(tmp_path)
    status, _, verdict = compare(tmp_path, base, [result(exit_code=2)])
    assert status == 1
    assert verdict["pre_existing"] == []


def test_publish_recovered_main_failure_is_not_open(tmp_path):
    row = result(conclusion="success", exit_code=0, recovered=[SCR],
                 first_attempt_exit_code=1, first_attempt_failed={SCR: "assertion"})
    base = baseline(tmp_path, row)
    assert json.loads(base.read_text())["tests"] == {}


def test_workflows_pilot_only_orcaflex_in_report_mode():
    for name, job in (("quality-gates-by-domain.yml", "domain-tests"),
                      ("full-matrix-sweep.yml", "sweep-tests")):
        workflow = yaml.safe_load((ROOT / ".github/workflows" / name).read_text())
        gate = next(step for step in workflow["jobs"][job]["steps"]
                    if step["name"] == "Run domain gate")
        run = gate["run"]
        assert gate["env"]["GH_TOKEN"] == "${{ github.token }}"
        assert run.count("scripts/ci/run_shard.py") == 1
        assert 'if [ "${{ matrix.domain }}" = "orcaflex" ]; then' in run
        assert "--mode report" in run and "--with pyyaml" in run
        assert "--mode enforce" not in run
        assert "${{ matrix.domain }}" in workflow["jobs"][job]["name"]
        import runpy
        assert "contracts" in runpy.run_path(str(ROOT / "scripts/ci/run_shard.py"))["NO_RETRY_SHARDS"]
        direct = '${{ steps.gate.outputs.command }} --junitxml=reports/junit-${{ matrix.domain }}.xml -o junit_family=xunit2'
        assert run.split("else\n")[1].split("\n")[0].strip() == direct


def test_aggregate_compares_successful_shards_too():
    workflow = yaml.safe_load((ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text())
    steps = workflow["jobs"]["domain-tests-required"]["steps"]
    for name in ("Checkout baseline tools", "Download domain results"):
        step = next(step for step in steps if step["name"] == name)
        assert "domain-tests.result" not in step["if"]
    enforce = next(step for step in steps if step["name"] == "Enforce required domain result")
    assert 'if [ "$HAS_DOMAINS" = "true" ]; then' in enforce["run"]


@pytest.mark.parametrize("recovered", [False, True])
@pytest.mark.parametrize("evidence", [[], [EVIDENCE_URL]])
def test_comparison_requires_recovery_and_register_evidence(tmp_path, recovered, evidence):
    node = "tests/test_new.py::test_racy"
    base = baseline(tmp_path)
    row = result(ids=(node,), first_attempt_failed={node: "assertion"},
                 recovered=[node] if recovered else [])
    path = tmp_path / "register.yml"
    path.write_text(json.dumps([dict(id=node, status="open", evidence=evidence)]))
    verdict = tmp_path / "verdict.json"
    status = ci.compare_results(base, write_results(tmp_path, row), 24,
                                tmp_path / "summary.md", verdict, NOW, path)
    assert status == (0 if recovered and evidence else 1)
    assert json.loads(verdict.read_text())["shards"][0]["ids"][node] == (
        "FLAKY" if recovered and evidence else "NEW")


def test_successful_recovery_can_be_preexisting_on_same_shard(tmp_path):
    base = baseline(tmp_path)
    row = result(ids=(), conclusion="success", exit_code=0, collected=1, first_attempt_exit_code=1,
                 first_attempt_failed={SCR: "assertion"}, recovered=[SCR])
    status, _, verdict = compare(tmp_path, base, [row])
    assert status == 0 and verdict["pre_existing"] == [SCR]


def test_missing_baseline_still_reports_recovered_new_id(tmp_path):
    node = "tests/test_new.py::test_racy"
    row = result(ids=(), conclusion="success", exit_code=0, collected=1, first_attempt_exit_code=1,
                 first_attempt_failed={node: "assertion"}, recovered=[node])
    status, summary, verdict = compare(tmp_path, tmp_path / "missing.json", [row])
    assert status == 1 and "baseline unavailable" in summary
    assert verdict["shards"][0]["ids"][node] == "NEW"


@pytest.mark.parametrize("conclusion", ["success", "failure"])
def test_extract_retry_evidence_keeps_original_messages(tmp_path, conclusion):
    node = "tests/test_new.py::test_racy"
    code = 0 if conclusion == "success" else 1
    retry = dict(sha="abc123", run_id="123", exit_code=code,
                 first_attempt_exit_code=1, first_attempt_failed={node: "first message"}, recovered=[node])
    (tmp_path / "flaky-example.json").write_text(json.dumps(retry))
    xml = ('<testsuite tests="1" failures="1" errors="0">'
           '<testcase classname="tests.test_new" name="test_racy">'
           '<failure message="first message"/></testcase></testsuite>')
    row = extract(tmp_path, xml, conclusion=conclusion, exit_code=code)
    assert row["parsed"] is True
    assert row["first_attempt_failed"] == {node: "first message"}
    assert row["failed"] == [] and row["recovered"] == [node]


def test_extract_mismatched_retry_record_fails_closed(tmp_path):
    (tmp_path / "flaky-example.json").write_text(json.dumps(dict(sha="wrong")))
    xml = ('<testsuite tests="1" failures="1" errors="0">'
           '<testcase classname="tests.test_new" name="test_racy"><failure/></testcase></testsuite>')
    assert extract(tmp_path, xml)["parsed"] is False


@pytest.mark.parametrize("content", ["[]\n", None, '[{"id": "base-id", "status": "open", "evidence": ["main run"]}]\n'])
def test_pr_register_comes_from_fetched_base(tmp_path, monkeypatch, content):
    monkeypatch.setenv("GITHUB_EVENT_NAME", "pull_request")
    monkeypatch.setenv("GITHUB_BASE_REF", "main")
    monkeypatch.chdir(tmp_path)
    local = tmp_path / ".github/ci/flaky-register.yml"
    local.parent.mkdir(parents=True)
    local.write_text('[{"id": "self-certified"}]')
    calls = []

    def git_output(command, **kwargs):
        calls.append(command)
        if command[1] == "fetch":
            return ""
        if command[1] == "rev-parse":
            return "a" * 40 + "\n"
        if command[1] == "ls-tree":
            return ".github/ci/flaky-register.yml\n" if content is not None else ""
        assert command == ["git", "show", "a" * 40 + ":.github/ci/flaky-register.yml"]
        return content

    monkeypatch.setattr(ci.subprocess, "check_output", git_output)
    output = tmp_path / "selected.yml"
    assert ci.main(["register", "--out", str(output)]) == 0
    assert output.read_text() == (content if content is not None else "[]\n")
    assert calls[0] == ["git", "fetch", "--no-tags", "origin", "refs/heads/main"]
    assert local.read_text() == '[{"id": "self-certified"}]'


def test_non_pr_register_uses_checkout(tmp_path, monkeypatch):
    monkeypatch.setenv("GITHUB_EVENT_NAME", "push")
    monkeypatch.chdir(tmp_path)
    source = tmp_path / ".github/ci/flaky-register.yml"
    source.parent.mkdir(parents=True)
    source.write_text("[]\n")
    output = tmp_path / "selected.yml"
    assert ci.main(["register", "--out", str(output)]) == 0
    assert output.read_bytes() == source.read_bytes()


def test_workflow_register_is_selected_before_retry_and_comparison():
    for name, job in (("quality-gates-by-domain.yml", "domain-tests"),
                      ("full-matrix-sweep.yml", "sweep-tests")):
        workflow = yaml.safe_load((ROOT / ".github/workflows" / name).read_text())
        steps = workflow["jobs"][job]["steps"]
        select = next(s for s in steps if s["name"] == "Select trusted register")
        gate = next(s for s in steps if s["name"] == "Run domain gate")
        assert steps.index(select) < steps.index(gate)
        assert "ci_baseline.py register" in select["run"]
        assert select["env"]["GITHUB_BASE_REF"] == "${{ github.base_ref }}"
        assert '--register "$RUNNER_TEMP/flaky-register.yml"' in gate["run"]
    workflow = yaml.safe_load((ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text())
    steps = workflow["jobs"]["domain-tests-required"]["steps"]
    enforce = next(s for s in steps if s["name"] == "Enforce required domain result")
    assert "ci_baseline.py register" in enforce["run"]
    assert '--register "$RUNNER_TEMP/flaky-register.yml"' in enforce["run"]


@pytest.mark.parametrize("failure", ["fetch", "show"])
def test_register_transport_failure_never_falls_back(tmp_path, monkeypatch, failure):
    monkeypatch.setenv("GITHUB_EVENT_NAME", "pull_request")
    monkeypatch.setenv("GITHUB_BASE_REF", "main")
    output = tmp_path / "selected.yml"

    def git_output(command, **kwargs):
        if command[1] == failure:
            raise subprocess.CalledProcessError(1, command)
        if command[1] == "rev-parse":
            return "a" * 40
        return ".github/ci/flaky-register.yml"

    monkeypatch.setattr(ci.subprocess, "check_output", git_output)
    with pytest.raises(subprocess.CalledProcessError):
        ci.select_register(output)
    assert not output.exists()


@pytest.mark.parametrize("failure", ["register", "metadata"])
@pytest.mark.parametrize("mode,domain,expected", [
    ("report", "success", 0), ("report", "failure", 1),
    ("enforce", "success", 1), ("enforce", "failure", 1),
])
def test_baseline_transport_failure_obeys_mode(tmp_path, mode, domain, expected, failure):
    workflow = yaml.safe_load((ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text())
    step = next(s for s in workflow["jobs"]["domain-tests-required"]["steps"]
                if s["name"] == "Enforce required domain result")
    # Fail the runtime boundary, without network access or project synchronization.
    for executable in ("python", "uv"):
        path = tmp_path / executable
        code = 0 if executable == "python" and failure == "metadata" else 7
        path.write_text(f"#!/bin/sh\nexit {code}\n")
        path.chmod(0o755)
    env = dict(os.environ, PATH=str(tmp_path) + os.pathsep + os.environ["PATH"],
               DETECT_RESULT="success", HARNESS_RESULT="success", HAS_DOMAINS="true",
               DOMAIN_RESULT=domain, GITHUB_EVENT_NAME="pull_request", CI_BASELINE_MODE=mode,
               RUNNER_TEMP=str(tmp_path), GITHUB_STEP_SUMMARY=str(tmp_path / "summary"))
    completed = subprocess.run(["bash", "-e", "-c", step["run"]], cwd=tmp_path,
                               env=env, text=True, capture_output=True)
    assert completed.returncode == expected, completed.stdout + completed.stderr
    if mode == "report":
        assert "baseline unavailable (report mode)" in completed.stdout


def test_comparison_runtime_is_isolated_and_pinned():
    workflow = yaml.safe_load((ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text())
    step = next(s for s in workflow["jobs"]["domain-tests-required"]["steps"]
                if s["name"] == "Enforce required domain result")
    assert "--no-sources" not in step["run"]
    assert step["run"].count("--no-project --with pyyaml==6.0.2") == 2


def test_enforce_applies_comparison_to_successful_domains():
    workflow = yaml.safe_load((ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text())
    step = next(s for s in workflow["jobs"]["domain-tests-required"]["steps"]
                if s["name"] == "Enforce required domain result")
    assert 'elif [ "$CI_BASELINE_MODE" = "enforce" ]; then' in step["run"]


@pytest.mark.parametrize("change", [
    {"event": "pull_request"}, {"head_branch": "feature"}, {"status": "in_progress"},
    {"head_repository": {"full_name": "other/repository"}}, {"head_repository": None},
])
def test_pr_or_untrusted_run_evidence_is_new(tmp_path, monkeypatch, change):
    monkeypatch.setattr(ci, "gh_json", lambda endpoint: evidence_response(endpoint, **change))
    node = "tests/test_new.py::test_racy"
    path = tmp_path / "register.yml"
    path.write_text(json.dumps([dict(id=node, status="open", evidence=[EVIDENCE_URL])]))
    row = result(ids=(), conclusion="success", exit_code=0, collected=1, first_attempt_exit_code=1,
                 first_attempt_failed={node: "assertion"}, recovered=[node])
    verdict = tmp_path / "verdict.json"
    assert ci.compare_results(baseline(tmp_path), write_results(tmp_path, row), 24,
                              tmp_path / "s", verdict, NOW, path) == 1
    assert json.loads(verdict.read_text())["shards"][0]["ids"][node] == "NEW"


def test_pr_added_register_cannot_certify_comparison(tmp_path, monkeypatch):
    monkeypatch.setenv("GITHUB_EVENT_NAME", "pull_request")
    monkeypatch.setattr(ci, "select_register", lambda out: out.write_text("[]\n"))
    node = "tests/test_new.py::test_racy"
    path = tmp_path / "register.yml"
    path.write_text(json.dumps([dict(id=node, status="open", evidence=[EVIDENCE_URL])]))
    row = result(ids=(), conclusion="success", exit_code=0, collected=1, first_attempt_exit_code=1,
                 first_attempt_failed={node: "assertion"}, recovered=[node])
    assert ci.compare_results(baseline(tmp_path), write_results(tmp_path, row), 24,
                              tmp_path / "s", tmp_path / "v", NOW, path) == 1


@pytest.mark.parametrize("field,value", [
    ("name", "tests-other"), ("conclusion", "success"), ("status", "in_progress"),
    ("run_id", 100), ("head_sha", "different"),
])
def test_evidence_requires_failed_same_shard_job(monkeypatch, field, value):
    def metadata(endpoint):
        response = evidence_response(endpoint)
        if "jobs" in response:
            response["jobs"][0][field] = value
        return response
    monkeypatch.setattr(ci, "gh_json", metadata)
    assert ci.verified_evidence(EVIDENCE_URL, "example", EVIDENCE_NODE) is False


@pytest.mark.parametrize("event", ["push", "schedule"])
@pytest.mark.parametrize("url", [
    EVIDENCE_URL,
    "https://github.com/example/repository/actions/jobs/7",
    "https://api.github.com/repos/example/repository/actions/runs/99",
    "https://api.github.com/repos/example/repository/actions/jobs/7",
    "https://api.github.com/repositories/123/actions/runs/99",
    "https://api.github.com/repositories/123/actions/jobs/7",
])
def test_main_run_evidence_is_accepted(monkeypatch, event, url):
    monkeypatch.setattr(ci, "gh_json", lambda endpoint: evidence_response(endpoint, event=event))
    assert ci.verified_evidence(url, "example", EVIDENCE_NODE) is True


def test_evidence_transport_failure_cannot_authorize_recovery(monkeypatch):
    def unavailable(endpoint):
        raise subprocess.CalledProcessError(1, ["gh", "api"])
    monkeypatch.setattr(ci, "gh_json", unavailable)
    assert ci.verified_evidence(EVIDENCE_URL, "example", EVIDENCE_NODE) is False


@pytest.mark.parametrize("kind,number", [("runs", 99), ("jobs", 7)])
def test_evidence_rejects_wrong_repository_id(tmp_path, kind, number):
    path = tmp_path / "register.yml"
    url = f"https://api.github.com/repositories/456/actions/{kind}/{number}"
    path.write_text(json.dumps([dict(id=EVIDENCE_NODE, status="open", evidence=[url])]))
    assert ci.register_decisions(path, [EVIDENCE_NODE], "example")[EVIDENCE_NODE] == (
        "missing open entry with evidence")


@pytest.mark.parametrize("different", ["tests/test_new.py::test_other", EVIDENCE_NODE + "[case]"])
def test_same_shard_evidence_of_different_test_is_rejected(tmp_path, different):
    path = tmp_path / "register.yml"
    nodes = [EVIDENCE_NODE, different]
    path.write_text(json.dumps([dict(id=node, status="open", evidence=[EVIDENCE_URL])
                                for node in nodes]))
    decisions = ci.register_decisions(path, nodes, "example")
    assert decisions[EVIDENCE_NODE] == "registered with evidence"
    assert decisions[different] == "missing open entry with evidence"


@pytest.mark.parametrize("outcome", ["FAILED", "ERROR"])
@pytest.mark.parametrize("node", [EVIDENCE_NODE, EVIDENCE_NODE + "[with space.a]"])
def test_evidence_accepts_exact_failed_or_errored_id(monkeypatch, outcome, node):
    log = f"2026-10-15T12:00:00.000Z \x1b[31m{outcome} {node} - assertion\x1b[0m\n"
    monkeypatch.setattr(ci, "gh_text", lambda endpoint: log)
    assert ci.verified_evidence(EVIDENCE_URL, "example", node) is True


@pytest.mark.parametrize("log", [
    "", f"PASSED {EVIDENCE_NODE}\n", f"{EVIDENCE_NODE} PASSED [100%]\n",
    f"FAILED {EVIDENCE_NODE}[other] - assertion\n",
])
def test_evidence_without_exact_failure_summary_is_rejected(monkeypatch, log):
    monkeypatch.setattr(ci, "gh_text", lambda endpoint: log)
    assert ci.verified_evidence(EVIDENCE_URL, "example", EVIDENCE_NODE) is False


def test_evidence_log_transport_failure_cannot_authorize_recovery(monkeypatch):
    def unavailable(endpoint):
        raise subprocess.CalledProcessError(1, ["gh", "api"])
    monkeypatch.setattr(ci, "gh_text", unavailable)
    assert ci.verified_evidence(EVIDENCE_URL, "example", EVIDENCE_NODE) is False


@pytest.mark.parametrize("changes", [{"parsed": False}, {"exit_code": 2},
                                      {"collected": 0}, {"first_attempt_exit_code": 1}])
def test_success_without_complete_execution_stays_red(tmp_path, changes):
    row = result(conclusion="success", ids=(), **dict({"exit_code": 0, "collected": 1}, **changes))
    assert compare(tmp_path, baseline(tmp_path), [row])[0] == 1


@pytest.mark.parametrize("comparison", [0, 1])
def test_enforce_returns_verdict_for_green_domains(tmp_path, comparison):
    workflow = yaml.safe_load((ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text())
    step = next(s for s in workflow["jobs"]["domain-tests-required"]["steps"]
                if s["name"] == "Enforce required domain result")
    python = tmp_path / "python"
    python.write_text("#!/bin/sh\nexit 0\n")
    python.chmod(0o755)
    uv = tmp_path / "uv"
    uv.write_text("#!/bin/sh\ncase \"$*\" in *'ci_baseline.py compare'*) "
                  f"echo comparison > baseline-summary.md; exit {comparison};; "
                  "*) echo 2026-10-15T12:00:00Z > baseline-time.txt; exit 0;; esac\n")
    uv.chmod(0o755)
    env = dict(os.environ, PATH=str(tmp_path) + os.pathsep + os.environ["PATH"],
               DETECT_RESULT="success", HARNESS_RESULT="success", HAS_DOMAINS="true",
               DOMAIN_RESULT="success", GITHUB_EVENT_NAME="pull_request", CI_BASELINE_MODE="enforce",
               RUNNER_TEMP=str(tmp_path), GITHUB_STEP_SUMMARY=str(tmp_path / "summary"))
    completed = subprocess.run(["bash", "-e", "-c", step["run"]], cwd=tmp_path,
                               env=env, text=True, capture_output=True)
    assert completed.returncode == comparison, completed.stdout + completed.stderr


@pytest.mark.parametrize("name", ["quality-gates-by-domain.yml", "full-matrix-sweep.yml"])
def test_register_runtime_dependency_is_pinned(name):
    workflow = (ROOT / ".github/workflows" / name).read_text()
    assert "--with pyyaml " not in workflow
    assert "--with pyyaml==6.0.2 " in workflow
