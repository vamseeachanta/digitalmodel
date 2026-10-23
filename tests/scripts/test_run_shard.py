# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise retry decisions and subprocess behaviour without running a solver."""

import importlib.util
import json
import os
import subprocess
import sys
from pathlib import Path

import pytest

SCRIPT = Path(__file__).resolve().parents[2] / "scripts/ci/run_shard.py"


EVIDENCE_URL = "https://github.com/example/repository/actions/runs/99"


@pytest.fixture(autouse=True)
def evidence_metadata(tmp_path, monkeypatch):
    """Serve authenticated metadata locally; never query hosted evidence in tests."""
    bin_path = tmp_path / "bin"
    bin_path.mkdir()
    gh = bin_path / "gh"
    gh.write_text(
        f"#!{sys.executable}\nimport json,sys\n"
        "endpoint = sys.argv[-1]\n"
        "run = dict(id=99, event='push', head_branch='main', status='completed', head_sha='mainsha', "
        "head_repository=dict(full_name='example/repository'))\n"
        "jobs = [dict(name='tests-' + shard, run_id=99, head_sha='mainsha', status='completed', "
        "conclusion='failure') for shard in ('sample', 'real')]\n"
        "print(json.dumps(dict(jobs=jobs) if '/jobs?' in endpoint else run))\n"
    )
    gh.chmod(0o755)
    monkeypatch.setenv("PATH", str(bin_path) + os.pathsep + os.environ["PATH"])
    monkeypatch.delenv("GITHUB_EVENT_NAME", raising=False)
    monkeypatch.setenv("GITHUB_REPOSITORY", "example/repository")


def module():
    spec = importlib.util.spec_from_file_location("run_shard", SCRIPT)
    loaded = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    return loaded


def fake_gate(tmp_path, first=1, second=0, count=1, mode="failure"):
    """Create a fake Python interpreter that also accepts retry pytest arguments."""
    gate = tmp_path / "gate"
    gate.write_text(
        f"#!{sys.executable}\n"
        "import json, pathlib, sys\n"
        "from xml.etree.ElementTree import Element, SubElement, ElementTree\n"
        "args = sys.argv[1:]\n"
        "state = pathlib.Path('attempts.json')\n"
        "history = json.loads(state.read_text()) if state.exists() else []\n"
        "history.append(args)\n"
        "state.write_text(json.dumps(history))\n"
        f"code = {first} if len(history) == 1 else {second}\n"
        "print('attempt', len(history), flush=True)\n"
        f"mode = {mode!r}\n"
        "target = next((a.split('=', 1)[1] for a in args if a.startswith('--junitxml=')), None)\n"
        "if target and mode not in {'missing', 'stale'}:\n"
        "    root = Element('testsuites')\n"
        f"    suite = SubElement(root, 'testsuite', tests=str({count}))\n"
        f"    for i in range({count}):\n"
        "        case = SubElement(suite, 'testcase', classname='tests.test_fake', name=f'test_case_{i}')\n"
        "        if code and not (mode == 'partial' and len(history) == 2 and i == 0):\n"
        "            tag = 'error' if mode == 'collection' else 'failure'\n"
        "            SubElement(case, tag, message='collection failure' if mode == 'collection' else ('Timeout exceeded' if mode == 'timeout' else 'assert mismatch'))\n"
        "        if mode == 'retry_skipped' and len(history) == 2:\n"
        "            SubElement(case, 'skipped')\n"
        "    suite.set('failures', str(len(suite.findall('testcase/failure'))))\n"
        "    suite.set('errors', str(len(suite.findall('testcase/error'))))\n"
        "    pathlib.Path(target).parent.mkdir(parents=True, exist_ok=True)\n"
        "    ElementTree(root).write(target)\n"
        "sys.exit(code)\n"
    )
    gate.chmod(0o755)
    (tmp_path / "tests").mkdir()
    (tmp_path / "tests/test_fake.py").write_text("")
    return gate


def child_environment(summary=None):
    env = {k: v for k, v in os.environ.items()
           if k not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GITHUB_STEP_SUMMARY"}}
    if summary is not None:
        env["GITHUB_STEP_SUMMARY"] = str(summary)
    return env


def invoke(tmp_path, gate, signal=False, shard="sample", options=(), summary=None):
    args = [sys.executable, str(SCRIPT), "--shard", shard, "--junit", "reports/junit-sample.xml", *options]
    if signal:
        args.append("--signal-retry")
    return subprocess.run(
        args + ["--", str(gate), "-m", "pytest", "tests/", "-q", "-p", "no:sugar", "-p", "no:randomly", "--tb=line", "-o", "junit_family=xunit2", "--junitxml=reports/junit-sample.xml"],
        cwd=tmp_path, text=True, capture_output=True, check=False,
        env=child_environment(summary),
    )


def history(tmp_path):
    return json.loads((tmp_path / "attempts.json").read_text())


def register(tmp_path, evidence=None, status="open"):
    path = tmp_path / "register.yml"
    path.write_text(json.dumps([dict(id="tests/test_fake.py::test_case_0",
                                    status=status, evidence=[EVIDENCE_URL] if evidence is None else evidence)]))
    return ("--mode", "enforce", "--register", str(path))


def test_failure_then_pass_records_and_warns(tmp_path, monkeypatch):
    monkeypatch.setenv("GITHUB_STEP_SUMMARY", str(tmp_path / "summary.md"))
    result = invoke(tmp_path, fake_gate(tmp_path), options=register(tmp_path),
                    summary=tmp_path / "summary.md")
    assert result.returncode == 0, result.stderr
    record = json.loads((tmp_path / "reports/flaky-sample.json").read_text())
    assert record["ids"] == ["tests/test_fake.py::test_case_0"]
    message = record["first_attempt_failed"]["tests/test_fake.py::test_case_0"]
    assert len(message) <= 500
    assert message.startswith("assert mismatch")
    assert {"sha", "run_id", "runner_cpu"} <= record.keys()
    assert "::warning::flaky, passed on retry: tests/test_fake.py::test_case_0" in result.stdout
    assert "attempt 1" in result.stdout and "attempt 2" in result.stdout
    summary = (tmp_path / "summary.md").read_text()
    assert "assert mismatch" in summary and "registered with evidence" in summary
    assert "passed on retry" in summary and "returned 0" in summary


def test_failure_twice_stays_red(tmp_path):
    result = invoke(tmp_path, fake_gate(tmp_path, second=1))
    assert result.returncode == 1
    assert len(history(tmp_path)) == 2


def test_partial_recovery_is_recorded_while_shard_stays_red(tmp_path):
    result = invoke(tmp_path, fake_gate(tmp_path, second=1, count=2, mode="partial"))
    assert result.returncode == 1
    record = json.loads((tmp_path / "reports/flaky-sample.json").read_text())
    assert record["ids"] == ["tests/test_fake.py::test_case_0"]


def test_skipped_retry_does_not_count_as_a_pass(tmp_path):
    result = invoke(tmp_path, fake_gate(tmp_path, mode="retry_skipped"))
    assert result.returncode == 1
    assert json.loads((tmp_path / "reports/flaky-sample.json").read_text())["ids"] == []


@pytest.mark.parametrize("count,mode", [(21, "failure"), (1, "collection"), (1, "missing")])
def test_unproven_or_excess_failures_never_retry(tmp_path, count, mode):
    assert invoke(tmp_path, fake_gate(tmp_path, count=count, mode=mode)).returncode == 1
    assert len(history(tmp_path)) == 1


def test_signal_abort_then_pass(tmp_path):
    result = invoke(tmp_path, fake_gate(tmp_path, first=134), signal=True)
    assert result.returncode == 134
    assert len(history(tmp_path)) == 2
    assert json.loads((tmp_path / "reports/flaky-sample.json").read_text())["kind"] == "signal-exit"


def test_signal_normal_failure_never_retries(tmp_path):
    assert invoke(tmp_path, fake_gate(tmp_path), signal=True).returncode == 1
    assert len(history(tmp_path)) == 1


@pytest.mark.parametrize("mode", ["report", "enforce"])
@pytest.mark.parametrize("code", [134, 139])
def test_signal_abort_preserves_first_exit(tmp_path, mode, code):
    result = invoke(tmp_path, fake_gate(tmp_path, first=code, second=0), options=("--mode", mode))
    assert result.returncode == code
    assert len(history(tmp_path)) == 1
    report = tmp_path / "reports/junit-sample.xml"
    assert "assert mismatch" in report.read_text()
    record = json.loads((tmp_path / "reports/flaky-sample.json").read_text())
    assert record["first_attempt_exit_code"] == code
    assert record["exit_code"] == code
    assert record["first_attempt_failed"] == {"tests/test_fake.py::test_case_0": "assert mismatch"}


def test_invoke_does_not_inherit_hosted_summary(tmp_path, monkeypatch):
    summary = tmp_path / "hosted-summary.md"
    monkeypatch.setenv("GITHUB_STEP_SUMMARY", str(summary))
    assert invoke(tmp_path, fake_gate(tmp_path)).returncode == 1
    assert not summary.exists()


def test_unidentified_failure_is_reported(tmp_path):
    summary = tmp_path / "summary.md"
    gate = fake_gate(tmp_path)
    gate.write_text(gate.read_text().replace("classname='tests.test_fake'", "classname='missing.module'"))
    result = invoke(tmp_path, gate, summary=summary)
    assert result.returncode == 1
    assert len(history(tmp_path)) == 1
    text = summary.read_text()
    assert "1 failing cases without an established ID" in text
    assert "Unidentified failure" in text
    assert "missing.module" in text and "test_case_0" in text


def test_atlas_keeps_direct_gate():
    workflow = SCRIPT.parents[2] / ".github/workflows/parametric-atlas-drift.yml"
    assert "run_shard.py" not in workflow.read_text()
    assert "run: python -m digitalmodel.parametric.refresh" in workflow.read_text()
    assert "atlas-retry-evidence" not in workflow.read_text()


def test_retry_contains_only_failed_ids_and_original_flags(tmp_path):
    invoke(tmp_path, fake_gate(tmp_path))
    assert history(tmp_path)[1] == [
        "-m", "pytest", "tests/test_fake.py::test_case_0", "-q", "-p", "no:sugar", "-p", "no:randomly",
        "--tb=line", "-o", "junit_family=xunit2", "--junitxml=reports/junit-sample-retry.xml",
    ]


def test_exit_five_is_unchanged(tmp_path):
    assert invoke(tmp_path, fake_gate(tmp_path, first=5)).returncode == 5
    assert len(history(tmp_path)) == 1


@pytest.mark.parametrize("code", [124, 137, 143])
def test_timeout_or_termination_never_retries(tmp_path, code):
    assert invoke(tmp_path, fake_gate(tmp_path, first=code), signal=True).returncode == code
    assert len(history(tmp_path)) == 1


def test_malformed_report_fails_closed(tmp_path):
    path = tmp_path / "bad.xml"
    path.write_text("not xml")
    assert module().junit_decision(path, 1, 20)["retry"] is False


def test_log_decisions_fail_closed():
    decide = module().log_decision
    assert decide("Process completed with exit code 134.", 20)["kind"] == "signal-exit"
    assert decide("FAILED tests/test_a.py::test_a\n1 failed, 2 passed\nProcess completed with exit code 1.", 20)["retry"]
    assert not decide("ERROR collecting tests/test_a.py\nProcess completed with exit code 1.", 20)["retry"]
    assert not decide("FAILED tests/test_a.py::test_a\nProcess completed with exit code 1.", 20)["retry"]


@pytest.mark.parametrize("mode,registered", [("report", False), ("enforce", False), ("enforce", True)])
def test_n4_marker_file_stays_red_with_xunit2_parameter_ids(tmp_path, monkeypatch, mode, registered):
    inherited_summary = tmp_path / "hosted-summary.md"
    monkeypatch.setenv("GITHUB_STEP_SUMMARY", str(inherited_summary))
    tests = tmp_path / "tests"
    tests.mkdir()
    (tests / "test_example.py").write_text(
        "from pathlib import Path\nimport pytest\n"
        "class TestExample:\n"
        "    @pytest.mark.parametrize('value', ['a.b', 'with space'])\n"
        "    def test_once(self, value):\n"
        "        marker = Path(value)\n"
        "        existed = marker.exists()\n"
        "        marker.write_text('seen')\n"
        "        assert existed\n"
    )
    expected_ids = [
        "tests/test_example.py::TestExample::test_once[a.b]",
        "tests/test_example.py::TestExample::test_once[with space]",
    ]
    options = [] if mode == "report" else ["--mode", mode]
    if registered:
        path = tmp_path / "register.yml"
        path.write_text(json.dumps([dict(id=node, status="open", evidence=[EVIDENCE_URL])
                                    for node in expected_ids]))
        options.extend(["--register", str(path)])
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--shard", "real", "--junit", "reports/junit-real.xml", *options, "--",
         sys.executable, "-m", "pytest", "tests/", "-q", "-p", "no:cacheprovider", "-p", "no:randomly",
         "--junitxml=reports/junit-real.xml", "-o", "junit_family=xunit2"],
        cwd=tmp_path, text=True, capture_output=True, check=False, env=child_environment(),
    )
    assert not inherited_summary.exists()
    assert result.returncode == (0 if registered else 1), result.stdout + result.stderr
    record = json.loads((tmp_path / "reports/flaky-real.json").read_text())
    assert record["ids"] == expected_ids


@pytest.mark.parametrize("options", [(), ("--mode", "enforce")])
def test_n2_unregistered_recovery_stays_red(tmp_path, options):
    result = invoke(tmp_path, fake_gate(tmp_path), options=options)
    assert result.returncode == 1
    assert len(history(tmp_path)) == 2


@pytest.mark.parametrize("options", [(), ("--mode", "enforce")])
def test_n3_shared_state_passes_alone_but_stays_red(tmp_path, options):
    tests = tmp_path / "tests"
    tests.mkdir()
    (tests / "test_state.py").write_text(
        "state = []\n"
        "def test_a():\n    state.append('changed')\n"
        "def test_b():\n    assert not state\n"
    )
    result = invoke(tmp_path, Path(sys.executable), options=options)
    assert result.returncode == 1, result.stdout + result.stderr
    record = json.loads((tmp_path / "reports/flaky-sample.json").read_text())
    assert record["ids"] == ["tests/test_state.py::test_b"]


def test_n5_contracts_never_retry(tmp_path):
    result = invoke(tmp_path, fake_gate(tmp_path), shard="contracts")
    assert result.returncode == 1
    assert len(history(tmp_path)) == 1


@pytest.mark.parametrize("code", [2, 3, 4, 139])
def test_n11_original_exit_preserved(tmp_path, code):
    assert invoke(tmp_path, fake_gate(tmp_path, first=code)).returncode == code
    assert len(history(tmp_path)) == 1


@pytest.mark.parametrize("evidence,status", [([], "open"), ([""], "open"), (["run"], "fixed")])
def test_register_without_open_evidence_stays_red(tmp_path, evidence, status):
    options = register(tmp_path, evidence, status)
    assert invoke(tmp_path, fake_gate(tmp_path), options=options).returncode == 1
    assert len(history(tmp_path)) == 2


@pytest.mark.parametrize("content", [None, "[", "{}", '[{"id": "x"}, {"id": "x"}]'])
def test_unreadable_register_stays_red(tmp_path, content):
    path = tmp_path / "register.yml"
    if content is not None:
        path.write_text(content)
    options = ("--mode", "enforce", "--register", str(path))
    result = invoke(tmp_path, fake_gate(tmp_path), options=options)
    assert result.returncode == 1
    assert len(history(tmp_path)) == 2
    assert "register unreadable" in result.stdout


def test_enforce_unregistered_recovery_stays_red(tmp_path):
    result = invoke(tmp_path, fake_gate(tmp_path), options=("--mode", "enforce"))
    assert result.returncode == 1
    assert len(history(tmp_path)) == 2


def test_report_registered_recovery_preserves_original_exit(tmp_path):
    options = register(tmp_path)
    options = ("--mode", "report", *options[2:])
    assert invoke(tmp_path, fake_gate(tmp_path), options=options).returncode == 1
    record = json.loads((tmp_path / "reports/flaky-sample.json").read_text())
    assert record["enforce_exit_code"] == 0
    assert record["mode"] == "report"


def test_summary_keeps_message_for_each_failure(tmp_path, monkeypatch):
    summary = tmp_path / "summary.md"
    monkeypatch.setenv("GITHUB_STEP_SUMMARY", str(summary))
    assert invoke(tmp_path, fake_gate(tmp_path, count=2), summary=summary).returncode == 1
    payload = json.loads((tmp_path / "reports/flaky-sample.json").read_text())
    assert len(payload["first_attempt_failed"]) == 2
    for node, message in payload["first_attempt_failed"].items():
        assert node in summary.read_text() and message in summary.read_text()
    assert summary.read_text().count("not run") == 0
    assert "missing" in summary.read_text() or "unreadable" in summary.read_text()


def test_stale_report_does_not_retry_or_change_exit(tmp_path):
    gate = fake_gate(tmp_path, first=3, mode="missing")
    report = tmp_path / "reports/junit-sample.xml"
    report.parent.mkdir()
    report.write_text('<testsuite><testcase classname="tests.test_fake" name="test_case_0"><failure/></testcase></testsuite>')
    os.utime(report, (1, 1))
    assert invoke(tmp_path, gate).returncode == 3
    assert len(history(tmp_path)) == 1


def test_contracts_signal_retry_is_also_denied(tmp_path):
    assert invoke(tmp_path, fake_gate(tmp_path, first=139), shard="contracts", signal=True).returncode == 139
    assert len(history(tmp_path)) == 1


def test_contracts_signal_exit_never_retries(tmp_path):
    assert invoke(tmp_path, fake_gate(tmp_path, first=139), shard="contracts").returncode == 139
    assert len(history(tmp_path)) == 1


@pytest.mark.parametrize("registered", [False, True, None])
def test_recovery_extract_compare_publish_roundtrip(tmp_path, monkeypatch, registered):
    monkeypatch.setenv("GITHUB_SHA", "abc123")
    monkeypatch.setenv("GITHUB_RUN_ID", "123")
    gate = fake_gate(tmp_path)
    source = tmp_path / ".github/ci/flaky-register.yml"
    source.parent.mkdir(parents=True)
    entry = dict(id="tests/test_fake.py::test_case_0", status="open", evidence=[EVIDENCE_URL])
    source.write_text(json.dumps([entry]))
    spec = importlib.util.spec_from_file_location("ci", SCRIPT.with_name("ci_baseline.py"))
    selector = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(selector)
    monkeypatch.setenv("GITHUB_EVENT_NAME", "pull_request")
    monkeypatch.setenv("GITHUB_BASE_REF", "main")
    monkeypatch.chdir(tmp_path)
    calls = []

    def base_output(command, **kwargs):
        calls.append(command)
        if command[1] == "fetch":
            return ""
        if command[1] == "rev-parse":
            return "a" * 40
        if command[1] == "ls-tree":
            return ".github/ci/flaky-register.yml" if registered is not None else ""
        assert command == ["git", "show", "a" * 40 + ":.github/ci/flaky-register.yml"]
        return json.dumps([entry] if registered else [])

    original_check_output = subprocess.check_output
    monkeypatch.setattr(selector.subprocess, "check_output", base_output)
    selected = tmp_path / "register.yml"
    selector.select_register(selected)
    assert calls[0] == ["git", "fetch", "--no-tags", "origin", "refs/heads/main"]
    monkeypatch.setattr(subprocess, "check_output", original_check_output)
    monkeypatch.delenv("GITHUB_EVENT_NAME")
    options = ("--mode", "enforce", "--register", str(selected))
    outcome = invoke(tmp_path, gate, options=options)
    assert outcome.returncode == (0 if registered else 1)
    spec = importlib.util.spec_from_file_location("ci", SCRIPT.with_name("ci_baseline.py"))
    ci = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(ci)
    monkeypatch.setattr(ci, "gh_json", lambda endpoint: json.loads(
        subprocess.check_output(["gh", "api", endpoint], text=True)))
    reports = tmp_path / "reports"
    output = reports / "result-sample.json"
    ci.main(["extract", "--junit", str(reports / "junit-sample.xml"), "--shard", "sample",
             "--sha", "abc123", "--run", "123", "--attempt", "1",
             "--conclusion", "success" if registered else "failure",
             "--exit-code", str(outcome.returncode), "--out", str(output)])
    row = json.loads(output.read_text())
    node = "tests/test_fake.py::test_case_0"
    assert row["parsed"] is True and row["recovered"] == [node]
    assert row["first_attempt_failed"] == {node: "assert mismatch"}
    assert row["first_attempt_exit_code"] == 1
    assert row["failed"] == []
    base = tmp_path / "baseline.json"
    base.write_text(json.dumps(dict(tests={}, shards={})))
    verdict = tmp_path / "verdict.json"
    assert ci.compare_results(base, reports, 24, tmp_path / "comparison.md", verdict,
                              register=tmp_path / "register.yml") == (0 if registered else 1)
    assert json.loads(verdict.read_text())["shards"][0]["ids"][node] == ("FLAKY" if registered else "NEW")
    ci.publish(reports, "none", base)
    assert json.loads(base.read_text())["tests"] == {}


def test_signal_retry_summary_preserves_both_exits(tmp_path):
    summary = tmp_path / "summary.md"
    result = invoke(tmp_path, fake_gate(tmp_path, first=139), signal=True, summary=summary)
    assert result.returncode == 139
    text = summary.read_text()
    assert "signal exit 139" in text and "retry exit: 0" in text and "returned 139" in text
    record = json.loads((tmp_path / "reports/flaky-sample.json").read_text())
    assert record["first_attempt_exit_code"] == 139 and record["retry_exit_code"] == 0


def test_wrapper_and_extractor_share_identity(tmp_path, monkeypatch):
    from xml.etree.ElementTree import fromstring
    monkeypatch.chdir(tmp_path)
    # Explicit file identity need not exist in the extraction workspace.
    case = fromstring('<testcase file="tests/test_outer/test_inner.py" '
                     'classname="tests.test_outer.test_inner.TestCase" name="test_value[a.b]"/>')
    assert module().node_id(case) == "tests/test_outer/test_inner.py::TestCase::test_value[a.b]"


def test_pr_added_register_cannot_clear_wrapper(tmp_path, monkeypatch):
    options = register(tmp_path)
    monkeypatch.setenv("GITHUB_EVENT_NAME", "pull_request")
    monkeypatch.setenv("GITHUB_BASE_REF", "main")
    # There is no fetched base in this isolated directory. Never trust the PR file.
    assert invoke(tmp_path, fake_gate(tmp_path), options=options).returncode == 1


def test_pr_run_evidence_cannot_clear_wrapper(tmp_path):
    gate = fake_gate(tmp_path)
    gh = tmp_path / "bin/gh"
    gh.write_text(gh.read_text().replace("event='push'", "event='pull_request'"))
    assert invoke(tmp_path, gate, options=register(tmp_path)).returncode == 1


def test_failure_then_pass_records_and_warns_but_stays_red(tmp_path):
    result = invoke(tmp_path, fake_gate(tmp_path))
    assert result.returncode == 1, result.stderr
    record = json.loads((tmp_path / "reports/flaky-sample.json").read_text())
    assert record["ids"] == ["tests/test_fake.py::test_case_0"]
    assert len(record["first_attempt_failed"]["tests/test_fake.py::test_case_0"]) <= 500
    assert record["first_attempt_failed"]["tests/test_fake.py::test_case_0"].startswith("assert mismatch")
    assert {"sha", "run_id", "runner_cpu"} <= record.keys()
    assert "::warning::flaky, passed on retry: tests/test_fake.py::test_case_0" in result.stdout
    assert "attempt 1" in result.stdout and "attempt 2" in result.stdout


@pytest.mark.parametrize("code", [2, 3, 4, 134, 139])
@pytest.mark.parametrize("mode", ["missing", "failure"])
def test_non_test_exit_is_preserved_without_signal_retry(tmp_path, code, mode):
    result = invoke(tmp_path, fake_gate(tmp_path, first=code, mode=mode))
    assert result.returncode == code
    assert len(history(tmp_path)) == 1


@pytest.mark.parametrize("code", [2, 3, 4, 134, 139])
def test_retry_error_preserves_original_failure(tmp_path, code):
    result = invoke(tmp_path, fake_gate(tmp_path, second=code))
    assert result.returncode == 1
    assert len(history(tmp_path)) == 2


def test_stale_junit_never_retries(tmp_path):
    gate = fake_gate(tmp_path, mode="stale")
    report = tmp_path / "reports/junit-sample.xml"
    report.parent.mkdir()
    report.write_text('<testsuites><testsuite><testcase classname="tests.test_fake" '
                      'name="test_case_0"><failure/></testcase></testsuite></testsuites>')
    os.utime(report, ns=(1, 1))
    assert invoke(tmp_path, gate).returncode == 1
    assert len(history(tmp_path)) == 1


def test_timeout_failure_text_never_retries(tmp_path):
    assert invoke(tmp_path, fake_gate(tmp_path, mode="timeout")).returncode == 1
    assert len(history(tmp_path)) == 1


def test_failure_twice_does_not_claim_recovery(tmp_path):
    assert invoke(tmp_path, fake_gate(tmp_path, second=1)).returncode == 1
    assert json.loads((tmp_path / "reports/flaky-sample.json").read_text())["recovered"] == []


def test_signal_failure_twice_does_not_claim_recovery(tmp_path):
    assert invoke(tmp_path, fake_gate(tmp_path, first=134, second=139), signal=True).returncode == 134
    assert len(history(tmp_path)) == 2
    record = json.loads((tmp_path / "reports/flaky-sample.json").read_text())
    assert record["first_attempt_exit_code"] == 134
    assert record["retry_exit_code"] == 139
    assert record["exit_code"] == 134
