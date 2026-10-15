# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Regression checks for gate triggers on stacked pull requests."""

from pathlib import Path
import json
import os
import re
import subprocess
import sys
from typing import Any

import pytest
import yaml


WORKFLOWS = Path(__file__).resolve().parents[2] / ".github" / "workflows"
GATES = (
    "quality-gates-by-domain.yml",
    "quality-gates.yml",
    "parametric-atlas-drift.yml",
    "gmsh-meshing-tests.yml",
)


def load_workflow(filename: str) -> dict:
    """Read a workflow without changing YAML 1.1 boolean-key handling."""
    return yaml.safe_load((WORKFLOWS / filename).read_text(encoding="utf-8"))


@pytest.mark.parametrize("filename", GATES)
def test_pull_request_accepts_any_base(filename: str) -> None:
    """Branch filters must not exclude stacked pull requests."""
    workflow = load_workflow(filename)
    triggers = workflow["on"] if "on" in workflow else workflow[True]
    assert "pull_request" in triggers
    pull_request = triggers["pull_request"] or {}
    assert "branches" not in pull_request
    assert "branches-ignore" not in pull_request
    assert triggers["push"]["branches"] == ["main", "develop"]


@pytest.mark.parametrize("filename", GATES)
def test_pull_request_activity_types(filename: str) -> None:
    """Retargets and ordinary edits start the same gates as code updates."""
    workflow = load_workflow(filename)
    triggers = workflow["on"] if "on" in workflow else workflow[True]
    assert triggers["pull_request"]["types"] == [
        "opened", "synchronize", "reopened", "edited"
    ]
    expected_keys = {"types", "paths"} if filename == "gmsh-meshing-tests.yml" else {"types"}
    assert set(triggers["pull_request"]) == expected_keys


@pytest.mark.parametrize("filename", GATES)
def test_edits_cannot_skip_jobs_or_steps(filename: str) -> None:
    """Edit-specific conditions cannot replace failing checks with skips."""
    for job in load_workflow(filename)["jobs"].values():
        for entry in [job, *job.get("steps", [])]:
            condition = str(entry.get("if", ""))
            assert not re.search(r"github\s*\.\s*event\s*\.\s*(action|changes)\b", condition)


def evaluate_expression(expression: str, context: dict[str, Any]) -> Any:
    """Evaluate the small Actions expression subset used by concurrency."""
    expression = expression.strip()
    for operator in (" || ", " && ", " == "):
        if operator in expression:
            left, right = expression.split(operator, 1)
            value = evaluate_expression(left, context)
            if operator == " || ":
                return value or evaluate_expression(right, context)
            if operator == " && ":
                return value and evaluate_expression(right, context)
            return value == evaluate_expression(right, context)
    formatted = re.fullmatch(r"format\('([^']+)', (github\.[\w.]+)\)", expression)
    if formatted:
        template, variable = formatted.groups()
        return template.format(context[variable])
    if expression.startswith("'") and expression.endswith("'"):
        return expression[1:-1]
    return context[expression]


def concurrency_result(
    filename: str, event: str, run_id: int, number: int = 1
) -> tuple[str, bool]:
    """Render the actual workflow group and cancellation expression."""
    workflow = load_workflow(filename)
    concurrency = workflow["concurrency"]
    context = {
        "github.workflow": workflow["name"],
        "github.event_name": event,
        "github.event.pull_request.number": number,
        "github.ref": "refs/heads/main",
        "github.run_id": run_id,
    }
    group = re.sub(
        r"\$\{\{(.*?)\}\}",
        lambda match: str(evaluate_expression(match[1], context)),
        concurrency["group"],
    )
    cancel = concurrency["cancel-in-progress"]
    assert isinstance(cancel, str)
    assert cancel.startswith("${{") and cancel.endswith("}}")
    result = evaluate_expression(cancel[3:-2], context)
    assert isinstance(result, bool)
    return group, result


@pytest.mark.parametrize("filename", GATES)
def test_pull_request_concurrency(filename: str) -> None:
    """Only updates of the same pull request supersede earlier runs."""
    first = concurrency_result(filename, "pull_request", 101, number=1)
    update = concurrency_result(filename, "pull_request", 102, number=1)
    other = concurrency_result(filename, "pull_request", 103, number=2)
    assert first == update
    assert first[0] != other[0]
    assert first[1] is True
    assert other[1] is True


@pytest.mark.parametrize("filename", GATES)
@pytest.mark.parametrize("event", ("push", "workflow_dispatch"))
def test_non_pr_runs_are_never_cancelled(filename: str, event: str) -> None:
    """Push and manual runs keep separate groups identified by run ID."""
    first = concurrency_result(filename, event, 101)
    second = concurrency_result(filename, event, 102)
    assert first == (f"{load_workflow(filename)['name']}-run-101", False)
    assert second == (f"{load_workflow(filename)['name']}-run-102", False)


@pytest.mark.parametrize(
    "filename,job_id,name",
    (
        ("quality-gates-by-domain.yml", "domain-tests-required", "Domain test aggregate"),
        ("quality-gates.yml", "quality-gates-aggregate", "Run Quality Gates"),
    ),
)
def test_required_check_names(filename: str, job_id: str, name: str) -> None:
    """Keep the check names consumed by branch protection stable."""
    assert load_workflow(filename)["jobs"][job_id]["name"] == name


def test_meshing_path_filters_are_preserved() -> None:
    """Meshing checks retain their existing incremental path selection."""
    workflow = load_workflow("gmsh-meshing-tests.yml")
    triggers = workflow["on"] if "on" in workflow else workflow[True]
    assert triggers["pull_request"]["paths"] == triggers["push"]["paths"]
    assert "src/digitalmodel/solvers/gmsh_meshing/**" in triggers["pull_request"]["paths"]
    assert ".github/workflows/gmsh-meshing-tests.yml" in triggers["pull_request"]["paths"]


def test_trigger_tests_run_in_harness() -> None:
    """The harness executes trigger regressions on every pull request."""
    workflow = load_workflow("quality-gates-by-domain.yml")
    commands = "\n".join(
        step.get("run", "") for step in workflow["jobs"]["ci-harness-tests"]["steps"]
    )
    assert "tests/workflows/test_ci_triggers.py" in commands


def test_full_sweep_keeps_nightly_schedule() -> None:
    """The full sweep runs nightly without changing pull request selection."""
    workflow = load_workflow("full-matrix-sweep.yml")
    triggers = workflow["on"] if "on" in workflow else workflow[True]
    assert triggers["schedule"] == [{"cron": "17 7 * * *"}]
    assert triggers["push"]["branches"] == ["main"]
    assert "pull_request" not in triggers
    assert "workflow_dispatch" in triggers


@pytest.mark.parametrize("event", ("push", "schedule", "workflow_dispatch"))
def test_full_sweep_separates_nightly_from_pushes(event: str) -> None:
    """Every sweep keeps its own group and cannot cancel another sweep."""
    concurrency = load_workflow("full-matrix-sweep.yml")["concurrency"]
    groups = []
    for run_id in (101, 102):
        context = {
            "github.ref": "refs/heads/main",
            "github.event_name": event,
            "github.run_id": run_id,
        }
        groups.append(
            re.sub(
                r"\$\{\{(.*?)\}\}",
                lambda match: str(evaluate_expression(match[1], context)),
                concurrency["group"],
            )
        )
    assert groups == [
        "full-matrix-sweep-refs/heads/main-101",
        "full-matrix-sweep-refs/heads/main-102",
    ]
    assert concurrency["cancel-in-progress"] is False


def test_full_sweep_bounds_each_shard() -> None:
    """A hung shard releases its runner within forty minutes."""
    shard = load_workflow("full-matrix-sweep.yml")["jobs"]["sweep-tests"]
    assert shard["timeout-minutes"] == 40
    assert shard["strategy"]["fail-fast"] is False


def repository_checkouts(filename: str) -> list[tuple[dict, int]]:
    """Locate every checkout of this repository, excluding pinned dependencies."""
    return [
        (job, index)
        for job in load_workflow(filename)["jobs"].values()
        for index, step in enumerate(job.get("steps", []))
        if step.get("uses", "").startswith("actions/checkout@")
        and "repository" not in step.get("with", {})
    ]


@pytest.mark.parametrize("filename", GATES)
def test_repository_checkouts_verify_current_merge(filename: str) -> None:
    """Every tested tree must bind the current base tip to the event head."""
    resolver_scripts = []
    for job, index in repository_checkouts(filename):
        steps = job["steps"]
        assert index > 0
        resolve, checkout, guard = steps[index - 1:index + 2]
        assert resolve["id"] == "resolve_merge"
        for step in (resolve, guard):
            assert step["if"] == "github.event_name == 'pull_request'"
        assert "if" not in checkout
        assert checkout["with"]["ref"] == "${{ steps.resolve_merge.outputs.sha || github.sha }}"
        assert resolve["env"]["BASE_REF"] == "${{ github.event.pull_request.base.ref }}"
        assert resolve["env"]["PR_HEAD_SHA"] == "${{ github.event.pull_request.head.sha }}"
        script = resolve["run"]
        resolver_scripts.append(script)
        assert 'git/ref/heads/${BASE_REF}' in script
        assert "base.sha" not in script
        assert "MERGE_RESOLVE_ATTEMPTS:-10" in script
        assert "MERGE_RESOLVE_DELAY:-6" in script
        assert '"${parents[0]}" = "$tip"' in script
        assert '"${parents[1]}" = "$PR_HEAD_SHA"' in script
        assert '${#parents[@]}' in script
        assert "git cat-file -p HEAD" in guard["run"]
        assert "git rev-parse 'HEAD^{tree}'" in guard["run"]
        assert "event_base=" in guard["run"]
        for key, output in (("MERGE_SHA", "sha"), ("BASE_TIP", "tip"), ("PR_HEAD_SHA", "head")):
            assert guard["env"][key] == f"${{{{ steps.resolve_merge.outputs.{output} }}}}"
        if "defaults" in job:
            assert resolve["working-directory"] == "${{ github.workspace }}"
    assert resolver_scripts
    assert all(script == resolver_scripts[0] for script in resolver_scripts)


def write_gh_fixture(directory: Path, tip: str, parent: str, head: str, mergeable: bool) -> None:
    """Provide the three API responses independently of the resolver script."""
    fixture = {
        "tip": {"object": {"sha": tip}},
        "pull": {"mergeable": mergeable, "merge_commit_sha": "c" * 40},
        "commit": {"parents": [{"sha": parent}, {"sha": head}]},
    }
    (directory / "fixture.json").write_text(json.dumps(fixture), encoding="utf-8")
    stub = directory / "gh"
    stub.write_text(
        f"#!{sys.executable}\n"
        "import json, os, pathlib, sys\n"
        "root = pathlib.Path(os.environ['GH_FIXTURES'])\n"
        "data = json.loads((root / 'fixture.json').read_text())\n"
        "endpoint = sys.argv[2]\n"
        "with (root / 'calls').open('a') as log: log.write(endpoint + '\\n')\n"
        "if '/git/ref/heads/' in endpoint:\n"
        "    print(data['tip']['object']['sha'])\n"
        "elif '/pulls/' in endpoint:\n"
        "    print(json.dumps(data['pull']))\n"
        "elif '/commits/' in endpoint:\n"
        "    print(' '.join(p['sha'] for p in data['commit']['parents']))\n"
        "else: sys.exit(2)\n",
        encoding="utf-8",
    )
    stub.chmod(0o755)


@pytest.mark.parametrize("filename", GATES)
@pytest.mark.parametrize(
    "case,parent,head,mergeable,success,attempts",
    (
        ("event-base-is-stale", "a" * 40, "b" * 40, True, True, 1),
        ("old-parent-merge", "d" * 40, "b" * 40, True, False, 2),
        ("wrong-head-parent", "a" * 40, "e" * 40, True, False, 2),
        ("conflicting-pull", "a" * 40, "b" * 40, False, False, 1),
    ),
)
def test_merge_resolver_behavior(
    filename: str, case: str, parent: str, head: str, mergeable: bool,
    success: bool, attempts: int, tmp_path: Path,
) -> None:
    """Execute the shipped resolver against current and stale merge objects."""
    write_gh_fixture(tmp_path, "a" * 40, parent, head, mergeable)
    job, index = repository_checkouts(filename)[0]
    assert index > 0, "checkout has no merge resolver"
    assert job["steps"][index - 1].get("id") == "resolve_merge"
    script = tmp_path / "resolve.sh"
    script.write_text(job["steps"][index - 1].get("run", ""), encoding="utf-8")
    output = tmp_path / "output"
    env = dict(os.environ, PATH=f"{tmp_path}{os.pathsep}{os.environ['PATH']}",
               GH_FIXTURES=str(tmp_path), GITHUB_OUTPUT=str(output),
               REPOSITORY="example/repository", BASE_REF="main", PR_NUMBER="1",
               PR_HEAD_SHA="b" * 40, EVENT_BASE_SHA="d" * 40,
               MERGE_RESOLVE_ATTEMPTS="2", MERGE_RESOLVE_DELAY="0")
    result = subprocess.run(["bash", "-e", str(script)], env=env, cwd=tmp_path,
                            capture_output=True, text=True, timeout=15)
    assert (result.returncode == 0) is success, (case, result.stdout, result.stderr)
    if success:
        assert output.read_text().splitlines() == [
            f"sha={'c' * 40}", f"tip={'a' * 40}", f"head={'b' * 40}",
        ]
    else:
        assert not output.exists()
        assert "::error::" in result.stdout
    calls = (tmp_path / "calls").read_text().splitlines()
    assert calls.count("repos/example/repository/git/ref/heads/main") == attempts
    if not mergeable:
        assert "not mergeable" in result.stdout
        assert not any("/commits/" in call for call in calls)
