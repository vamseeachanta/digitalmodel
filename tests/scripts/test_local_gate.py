"""Tests for the local pull-request gate (scripts/ci/local_gate.py).

No test here reaches the network, a remote host or the GitHub API. Setup
commands and clocks use fakes; process cleanup and URL expansion use local subprocesses.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import os
import shlex
import signal
import subprocess
import sys
import time
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SHA = "a" * 40
TIP = "b" * 40


def _load():
    spec = importlib.util.spec_from_file_location("local_gate", ROOT / "scripts/ci/local_gate.py")
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules["local_gate"] = module
    spec.loader.exec_module(module)
    return module


gate = _load()
PLAN = {
    "harness": "pytest tests/scripts/test_run_shard.py",
    "gates": {
        "tests-contracts": "pytest tests/contracts/",
        "tests-subsea": "pytest tests/subsea/",
        "tests-orcaflex": "pytest tests/orcaflex/",
        "tests-specialized": "pytest tests/specialized/",
    },
}


def _matrix(*domains: str) -> dict:
    return {"include": [{"domain": d, "runner": "x"} for d in domains]}


class Clock:
    def __init__(self) -> None:
        self.now = 0.0

    def __call__(self) -> float:
        return self.now


def test_plan_always_has_lint_and_harness() -> None:
    modules = gate.plan_modules(_matrix(), PLAN, TIP, "/scratch")
    assert [m.name for m in modules] == ["lint", "ci-harness"]
    assert any("check_text_encoding.py" in c and TIP in c for c in modules[0].commands)
    assert any("check-engineering-register.py" in c for c in modules[0].commands)
    assert modules[1].commands == [PLAN["harness"]]
    for suite in ("tests/scripts/test_check_text_encoding.py", "tests/repo_structure"):
        command = next(c for c in modules[0].commands if suite in c)
        assert "python -m pytest" in command
        assert "--with pyyaml" in command and "--with pytest" in command
        assert "--basetemp=" in command and "-p no:cacheprovider" in command
    assert any("--with ruff==0.15.1" in c and "test_check_text_encoding.py" in c
               for c in modules[0].commands)
    assert any("--with pytest-asyncio" in c and "tests/repo_structure" in c
               for c in modules[0].commands)


def test_plan_mirrors_workflow_special_cases() -> None:
    modules = {m.name: m for m in gate.plan_modules(
        _matrix("contracts", "subsea", "orcaflex", "specialized"), PLAN, TIP, "/scratch")}
    assert "tests/riser_fatigue/ tests/riser_bom/" in modules["subsea"].commands[-1]
    assert modules["subsea"].commands[-1].endswith(
        "--junitxml=reports/junit-subsea.xml -o junit_family=xunit2")
    assert any("prettier@4.0.0-alpha.8" in c for c in modules["contracts"].commands)
    assert "SOURCE_LOCATORS_PRETTIER=" in modules["contracts"].commands[-1]
    assert "run_shard.py" in " ".join(modules["orcaflex"].commands)
    assert "tests/geotechnical" in modules["specialized"].commands[-1]


def test_touched_domain_without_command_is_an_error() -> None:
    with pytest.raises(gate.GateError, match="no gate command"):
        gate.plan_modules(_matrix("structural"), PLAN, TIP, "/scratch")


def test_hostile_domain_name_is_refused() -> None:
    with pytest.raises(gate.GateError, match="unexpected domain"):
        gate.plan_modules(_matrix("x; rm -rf /"), PLAN, TIP, "/scratch")


def test_module_stops_at_first_failing_command(tmp_path: Path) -> None:
    seen: list[str] = []

    def shell(command, cwd, env, log, timeout):
        seen.append(command)
        return 1 if command == "two" else 0

    result = gate.run_module(gate.Module("m", ["one", "two", "three"]), tmp_path, {}, tmp_path,
                             deadline=100.0, module_timeout=50.0, shell=shell, clock=Clock())
    assert (result.status, result.code, seen) == ("fail", 1, ["one", "two"])


def test_module_past_budget_is_not_run(tmp_path: Path) -> None:
    clock = Clock()
    clock.now = 200.0
    result = gate.run_module(gate.Module("m", ["one"]), tmp_path, {}, tmp_path, deadline=100.0,
                             module_timeout=50.0, shell=lambda *a: 0, clock=clock)
    assert result.status == "not-run"


def test_module_timeout_is_capped_by_budget_and_reported(tmp_path: Path) -> None:
    limits: list[float] = []

    def shell(command, cwd, env, log, timeout):
        limits.append(timeout)
        return 124

    result = gate.run_module(gate.Module("m", ["one"]), tmp_path, {}, tmp_path, deadline=30.0,
                             module_timeout=900.0, shell=shell, clock=Clock())
    assert limits == [30.0]
    assert result.status == "timeout"


def test_summary_success_requires_every_module() -> None:
    passed = [gate.Result("lint", "pass", 3, 0), gate.Result("contracts", "pass", 9, 0)]
    state, text = gate.summarize(passed, 12.4, TIP)
    assert state == "success"
    assert text == f"base {TIP}: 2/2 modules passed in 12 s (lint, contracts)"
    for status in ("fail", "timeout", "not-run"):
        state, text = gate.summarize(passed + [gate.Result("subsea", status)], 20, TIP)
        assert state == "failure"
        assert f"subsea ({status})" in text
    assert gate.summarize([], 0, TIP)[0] == "error"


def test_summary_fits_the_status_description_limit() -> None:
    many = [gate.Result(f"domain-number-{i}", "fail") for i in range(40)]
    # Independent literal for the documented local compatibility policy.
    assert gate.DESCRIPTION_LIMIT == 140
    assert len(gate.summarize(many, 1, TIP)[1]) <= 140


@pytest.mark.parametrize("status", ["pass", "fail"])
def test_summary_always_names_the_full_tested_base_first(status: str) -> None:
    many = [gate.Result(f"domain-number-{i}", status) for i in range(40)]
    assert gate.summarize(many, 1, TIP)[1].startswith(f"base {TIP}: ")


def test_run_modules_keeps_plan_order() -> None:
    modules = [gate.Module(str(i), []) for i in range(8)]
    results = gate.run_modules(modules, 3, lambda m: gate.Result(m.name, "pass"))
    assert [r.name for r in results] == [str(i) for i in range(8)]


def test_post_status_targets_the_exact_commit() -> None:
    calls: list[list[str]] = []
    gate.post_status("o/r", SHA, "success", "x" * 500, run=lambda cmd: calls.append(cmd))
    cmd = calls[0]
    assert f"repos/o/r/statuses/{SHA}" in cmd
    assert "context=local-gate" in cmd and "state=success" in cmd
    assert f"description={'x' * 140}" in cmd


@pytest.mark.parametrize("bad", ["abc", "A" * 40, "a" * 39])
def test_short_or_malformed_sha_is_rejected(bad: str, tmp_path: Path) -> None:
    with pytest.raises(SystemExit):
        gate.parse_args(["--pr", "1", "--sha", bad, "--work-root", str(tmp_path)])


def _main(monkeypatch, tmp_path: Path, outcome, argv_extra=()):
    posts: list[tuple] = []
    monkeypatch.setattr(gate, "post_status", lambda repo, sha, state, text: posts.append((sha, state, text)))
    monkeypatch.setattr(gate, "remove_tree", lambda root, wt: None)

    def fake_gate(args: argparse.Namespace, run_dir: Path, worktree: Path):
        if isinstance(outcome, Exception):
            raise outcome
        return outcome

    monkeypatch.setattr(gate, "gate", fake_gate)
    code = gate.main(["--pr", "7", "--sha", SHA, "--repo", "fixture/repository",
                      "--work-root", str(tmp_path), *argv_extra])
    return code, posts


def test_main_posts_success_and_writes_result(monkeypatch, tmp_path: Path) -> None:
    code, posts = _main(monkeypatch, tmp_path, ("success", "ok", [gate.Result("lint", "pass", 1, 0)]))
    assert code == 0 and posts == [(SHA, "success", "ok")]
    result = json.loads(next((tmp_path / "runs").glob("*/result.json")).read_text(encoding="utf-8"))
    assert result["state"] == "success" and result["modules"][0]["name"] == "lint"


def test_main_failure_returns_nonzero(monkeypatch, tmp_path: Path) -> None:
    code, posts = _main(monkeypatch, tmp_path, ("failure", "1/2", []))
    assert code == 1 and posts[0][1] == "failure"


def test_main_host_problem_posts_error_never_success(monkeypatch, tmp_path: Path) -> None:
    code, posts = _main(monkeypatch, tmp_path, gate.GateError("work volume has 3 GB free"))
    assert code == 2 and posts[0][1] == "error" and "3 GB" in posts[0][2]


def test_main_merge_conflict_posts_failure(monkeypatch, tmp_path: Path) -> None:
    code, posts = _main(monkeypatch, tmp_path, gate.MergeConflict("head does not merge into base"))
    assert code == 1 and posts[0][1] == "failure"


def test_main_stale_head_posts_nothing(monkeypatch, tmp_path: Path) -> None:
    code, posts = _main(monkeypatch, tmp_path, gate.StaleHead("moved"))
    assert code == 3 and posts == []


def test_main_no_post_flag(monkeypatch, tmp_path: Path) -> None:
    code, posts = _main(monkeypatch, tmp_path, ("success", "ok", []), ["--no-post"])
    assert code == 0 and posts == []


def test_gate_refuses_low_disk_before_any_fetch(monkeypatch, tmp_path: Path) -> None:
    monkeypatch.setattr(gate, "free_gb", lambda path: 5.0)
    monkeypatch.setattr(gate, "pull_request", lambda *a, **kw: pytest.fail("no API call below the floor"))
    args = gate.parse_args(["--pr", "7", "--sha", SHA, "--repo", "fixture/repository",
                            "--work-root", str(tmp_path)])
    with pytest.raises(gate.GateError, match="below the 20 GB floor"):
        gate.gate(args, tmp_path / "run", tmp_path / "wt")


@pytest.mark.parametrize("info, error", [
    ({"head": "c" * 40, "head_repo": "fixture/repository", "base": "main", "state": "open"}, "StaleHead"),
    ({"head": SHA, "head_repo": "fixture/repository", "base": "main", "state": "closed"}, "StaleHead"),
    ({"head": SHA, "head_repo": "other/fork", "base": "main", "state": "open"}, "GateError"),
])
def test_gate_refuses_moved_closed_and_fork_heads(monkeypatch, tmp_path: Path, info, error) -> None:
    monkeypatch.setattr(gate, "free_gb", lambda path: 500.0)
    monkeypatch.setattr(gate, "pull_request", lambda *a, **kw: info)
    monkeypatch.setattr(gate, "prepare_tree", lambda *a, **kw: pytest.fail("no checkout for a refused head"))
    args = gate.parse_args(["--pr", "7", "--sha", SHA, "--repo", "fixture/repository",
                            "--work-root", str(tmp_path)])
    with pytest.raises(gate.GateError) as caught:
        gate.gate(args, tmp_path / "run", tmp_path / "wt")
    assert type(caught.value).__name__ == error


def _running(pid: int) -> bool:
    try:
        # An orphan zombie has exited; only an executing descendant is a leak.
        return Path(f"/proc/{pid}/stat").read_text(encoding="utf-8").split()[2] != "Z"
    except FileNotFoundError:
        return False


def _stubborn_child_command(pidfile: Path) -> str:
    child = (
        "import os, signal, time; "
        "signal.signal(signal.SIGTERM, signal.SIG_IGN); "
        f"open({str(pidfile)!r}, 'w', encoding='utf-8').write(str(os.getpid())); "
        "time.sleep(60)"
    )
    leader = (
        "import subprocess, sys, time; "
        f"subprocess.Popen([sys.executable, '-c', {child!r}]); "
        "time.sleep(60)"
    )
    return shlex.join([sys.executable, "-c", leader])


def test_shell_timeout_kills_descendant_after_leader_exits(tmp_path: Path) -> None:
    pidfile = tmp_path / "child.pid"
    pid = None
    try:
        code = gate.run_shell(_stubborn_child_command(pidfile), tmp_path, dict(os.environ),
                              tmp_path / "shell.log", timeout=1.0)
        pid = int(pidfile.read_text(encoding="utf-8"))
        until = time.monotonic() + 1.0
        while _running(pid) and time.monotonic() < until:
            time.sleep(0.01)
        assert code == 124
        assert not _running(pid), "TERM-ignoring descendant survived the timeout"
    finally:
        if pid is None and pidfile.exists():
            pid = int(pidfile.read_text(encoding="utf-8"))
        if pid is not None and _running(pid):
            os.kill(pid, signal.SIGKILL)


def test_capture_timeout_kills_descendant_and_closes_pipes(tmp_path: Path) -> None:
    pidfile = tmp_path / "capture-child.pid"
    pid = None
    try:
        with pytest.raises(gate.GateError, match="timed out"):
            gate.capture(["bash", "-c", _stubborn_child_command(pidfile)], timeout=1.0)
        pid = int(pidfile.read_text(encoding="utf-8"))
        until = time.monotonic() + 1.0
        while _running(pid) and time.monotonic() < until:
            time.sleep(0.01)
        assert not _running(pid)
    finally:
        if pid is None and pidfile.exists():
            pid = int(pidfile.read_text(encoding="utf-8"))
        if pid is not None and _running(pid):
            os.kill(pid, signal.SIGKILL)


def test_deadline_runner_recomputes_budget_before_every_helper() -> None:
    clock = Clock()
    limits = []

    def capture(cmd, **kwargs):
        limits.append(kwargs["timeout"])
        clock.now += 4.0
        return subprocess.CompletedProcess(cmd, 0, "ok", "")

    run = gate.deadline_runner(10.0, run=capture, clock=clock)
    for _ in range(2):
        assert run(["helper"]).stdout == "ok"
    with pytest.raises(gate.GateError, match="budget expired during helper"):
        run(["last-helper"])
    with pytest.raises(gate.GateError, match="budget expired before helper"):
        run(["must-not-start"])
    assert limits == [10.0, 6.0, 2.0]


@pytest.mark.parametrize("expire_at", ["api", "clone", "fetch", "merge", "detect", "reader", "checkout"])
def test_setup_and_checkout_helpers_obey_overall_deadline(monkeypatch, tmp_path: Path,
                                                         expire_at: str) -> None:
    clock = Clock()
    seen = []
    expired = False

    def capture(cmd, **kwargs):
        nonlocal expired
        assert not expired, "another helper launched after budget expiration"
        assert 0 < kwargs["timeout"] <= 100.0
        if cmd[0] == "gh":
            phase = "api"
            stdout = json.dumps({"head": SHA, "head_repo": "fixture/repository",
                                 "base": "main", "state": "open"})
        elif "clone" in cmd:
            phase, stdout = "clone", ""
        elif "fetch" in cmd:
            phase, stdout = "fetch", ""
        elif "merge" in cmd:
            phase, stdout = "merge", ""
        elif "scripts/ci/detect_touched_domains.py" in cmd:
            phase, stdout = "detect", json.dumps(_matrix())
        elif gate.PLAN_READER in cmd:
            phase, stdout = "reader", json.dumps(PLAN)
        elif "worktree" in cmd and "add" in cmd and str(tmp_path / "wt" / "plan") not in cmd:
            phase, stdout = "checkout", ""
        else:
            phase = "other"
            stdout = SHA if "refs/local-gate/pr-7" in cmd else TIP
        seen.append(phase)
        if phase == expire_at:
            expired = True
            clock.now = 101.0
        return subprocess.CompletedProcess(cmd, 0, stdout, "")

    request = gate.pull_request

    def pull_request(repo, number, run=capture):
        return request(repo, number, run=run)

    monkeypatch.setattr(gate, "pull_request", pull_request)
    monkeypatch.setattr(gate, "capture", capture)
    monkeypatch.setattr(gate.time, "monotonic", clock)
    monkeypatch.setattr(gate, "free_gb", lambda path: 500.0)
    # Serialize test fixtures so the fake clock cannot race another checkout.
    args = gate.parse_args(["--pr", "7", "--sha", SHA, "--repo", "fixture/repository",
                            "--work-root", str(tmp_path), "--jobs", "1", "--budget-seconds", "100"])
    with pytest.raises(gate.GateError, match="budget expired"):
        gate.gate(args, tmp_path / "run", tmp_path / "wt")
    assert expire_at in seen


@pytest.mark.parametrize("inherited", [None, "other/repository"])
def test_gate_supplies_repository_for_dependency_url_expansion(monkeypatch, tmp_path: Path,
                                                              inherited: str | None) -> None:
    if inherited is None:
        monkeypatch.delenv("GITHUB_REPOSITORY", raising=False)
    else:
        monkeypatch.setenv("GITHUB_REPOSITORY", inherited)
    monkeypatch.setattr(gate, "free_gb", lambda path: 500.0)
    monkeypatch.setattr(gate, "pull_request", lambda *a, **kw: {
        "head": SHA, "head_repo": "fixture/repository", "base": "main", "state": "open"})
    monkeypatch.setattr(gate, "prepare_tree", lambda *a, **kw: TIP)

    def capture(cmd, **kwargs):
        if "scripts/ci/detect_touched_domains.py" in cmd:
            stdout = json.dumps(_matrix())
        elif gate.PLAN_READER in cmd:
            stdout = json.dumps(PLAN)
        else:
            stdout = TIP
        return subprocess.CompletedProcess(cmd, 0, stdout, "")

    monkeypatch.setattr(gate, "capture", capture)
    seen = []

    def module(module, tree, env, logs, deadline, module_timeout):
        expanded = tmp_path / f"{module.name}.url"
        command = ('printf %s "git+https://github.com/${GITHUB_REPOSITORY%/*}/assetutilities.git@main"'
                   f" > {shlex.quote(str(expanded))}")
        assert gate.run_shell(command, tmp_path, env, logs / f"{module.name}.log", 5.0) == 0
        assert expanded.read_text(encoding="utf-8") == (
            "git+https://github.com/fixture/assetutilities.git@main")
        seen.append(module.name)
        return gate.Result(module.name, "pass", 0.0, 0)

    monkeypatch.setattr(gate, "run_module", module)
    args = gate.parse_args(["--pr", "7", "--sha", SHA, "--repo", "fixture/repository",
                            "--work-root", str(tmp_path), "--jobs", "1"])
    state, _, _ = gate.gate(args, tmp_path / "run", tmp_path / "wt")
    assert state == "success" and seen == ["lint", "ci-harness"]


def test_repository_default_uses_runtime_environment(monkeypatch, tmp_path: Path) -> None:
    monkeypatch.setenv("GITHUB_REPOSITORY", "fixture/repository")
    args = gate.parse_args(["--pr", "7", "--sha", SHA, "--work-root", str(tmp_path)])
    assert args.repo == "fixture/repository"


def test_repository_must_be_provided_when_environment_is_absent(monkeypatch, tmp_path: Path) -> None:
    monkeypatch.delenv("GITHUB_REPOSITORY", raising=False)
    with pytest.raises(SystemExit):
        gate.parse_args(["--pr", "7", "--sha", SHA, "--work-root", str(tmp_path)])


def test_prepare_tree_uses_existing_git_identity(tmp_path: Path) -> None:
    calls = []

    def capture(cmd, **kwargs):
        calls.append(cmd)
        stdout = SHA if "refs/local-gate/pr-7" in cmd else TIP
        return subprocess.CompletedProcess(cmd, 0, stdout, "")

    args = gate.parse_args(["--pr", "7", "--sha", SHA, "--repo", "fixture/repository",
                            "--work-root", str(tmp_path)])
    tree = tmp_path / "tree"
    assert gate.prepare_tree(args, "main", tree, run=capture) == TIP
    merge = next(cmd for cmd in calls if "merge" in cmd)
    # Git obtains identity from its normal runtime configuration and environment.
    assert merge == ["git", "-C", str(tree), "merge", "--no-ff", "--no-edit", SHA]
