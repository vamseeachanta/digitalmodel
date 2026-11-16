"""Tests for the local pull-request gate (scripts/ci/local_gate.py).

No test here reaches the network, a remote host or the GitHub API: the
command runner, the shell and the clock are replaced by fakes.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import sys
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
    assert len(gate.summarize(many, 1, TIP)[1]) <= gate.DESCRIPTION_LIMIT


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
    assert f"description={'x' * gate.DESCRIPTION_LIMIT}" in cmd


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
    code = gate.main(["--pr", "7", "--sha", SHA, "--work-root", str(tmp_path), *argv_extra])
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
    monkeypatch.setattr(gate, "pull_request", lambda *a: pytest.fail("no API call below the floor"))
    args = gate.parse_args(["--pr", "7", "--sha", SHA, "--work-root", str(tmp_path)])
    with pytest.raises(gate.GateError, match="below the 20 GB floor"):
        gate.gate(args, tmp_path / "run", tmp_path / "wt")


@pytest.mark.parametrize("info, error", [
    ({"head": "c" * 40, "head_repo": "vamseeachanta/digitalmodel", "base": "main", "state": "open"}, "StaleHead"),
    ({"head": SHA, "head_repo": "vamseeachanta/digitalmodel", "base": "main", "state": "closed"}, "StaleHead"),
    ({"head": SHA, "head_repo": "someone/fork", "base": "main", "state": "open"}, "GateError"),
])
def test_gate_refuses_moved_closed_and_fork_heads(monkeypatch, tmp_path: Path, info, error) -> None:
    monkeypatch.setattr(gate, "free_gb", lambda path: 500.0)
    monkeypatch.setattr(gate, "pull_request", lambda *a: info)
    monkeypatch.setattr(gate, "prepare_tree", lambda *a: pytest.fail("no checkout for a refused head"))
    args = gate.parse_args(["--pr", "7", "--sha", SHA, "--work-root", str(tmp_path)])
    with pytest.raises(gate.GateError) as caught:
        gate.gate(args, tmp_path / "run", tmp_path / "wt")
    assert type(caught.value).__name__ == error
