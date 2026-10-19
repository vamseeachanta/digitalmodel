# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Regression checks for gate triggers on stacked pull requests."""

from pathlib import Path
import re
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
PUSH_PR_WORKFLOWS = GATES + (
    "docs.yml",
    "full-matrix-sweep.yml",
    "pages.yml",
    "repo-structure.yml",
    "store-data-tests.yml",
    "workflow-automation-tests.yml",
)
WRITERS = {
    "full-matrix-sweep.yml",  # Publishes the baseline and updates main health.
    "pages.yml",  # Deploys Pages.
    "quality-gates.yml",  # Posts pull request comments.
    "workflow-automation-tests.yml",  # Publishes coverage to Codecov.
}


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
    filename: str, event: str, run_id: int, number: int = 1,
    ref: str = "refs/heads/main",
) -> tuple[str, bool]:
    """Render the actual workflow group and cancellation expression."""
    workflow = load_workflow(filename)
    concurrency = workflow["concurrency"]
    context = {
        "github.workflow": workflow["name"],
        "github.event_name": event,
        "github.event.pull_request.number": number if event == "pull_request" else None,
        "github.ref": ref,
        "github.run_id": run_id,
    }
    group = re.sub(
        r"\$\{\{(.*?)\}\}",
        lambda match: str(evaluate_expression(match[1], context)),
        concurrency["group"],
    )
    cancel = concurrency["cancel-in-progress"]
    if isinstance(cancel, bool):
        return group, cancel
    assert isinstance(cancel, str)
    assert cancel.startswith("${{") and cancel.endswith("}}")
    result = evaluate_expression(cancel[3:-2], context)
    assert isinstance(result, bool)
    return group, result


@pytest.mark.parametrize("filename", PUSH_PR_WORKFLOWS)
def test_pull_request_concurrency(filename: str) -> None:
    """Only updates of the same pull request supersede earlier runs."""
    first = concurrency_result(filename, "pull_request", 101, number=1)
    update = concurrency_result(filename, "pull_request", 102, number=1)
    other = concurrency_result(filename, "pull_request", 103, number=2)
    assert first == update
    assert first[0] != other[0]
    assert first[0] == f"{load_workflow(filename)['name']}-1"
    assert first[1] is (filename not in WRITERS)
    assert other[1] is (filename not in WRITERS)


@pytest.mark.parametrize("filename", PUSH_PR_WORKFLOWS)
@pytest.mark.parametrize("event", ("push", "workflow_dispatch"))
def test_non_pr_runs_are_never_cancelled(filename: str, event: str) -> None:
    """Retain the test name; only workflows with external writes avoid cancellation."""
    first = concurrency_result(filename, event, 101)
    second = concurrency_result(filename, event, 102)
    expected = (f"{load_workflow(filename)['name']}-refs/heads/main", filename not in WRITERS)
    assert first == expected
    assert second == expected


@pytest.mark.parametrize("filename", PUSH_PR_WORKFLOWS)
def test_concurrency_isolates_refs(filename: str) -> None:
    """Different branches and pull requests cannot supersede one another."""
    main = concurrency_result(filename, "push", 101)
    branch = concurrency_result(filename, "push", 102, ref="refs/heads/develop")
    pull_request = concurrency_result(filename, "pull_request", 103)
    assert len({main[0], branch[0], pull_request[0]}) == 3


def test_push_pr_workflow_inventory() -> None:
    """Every push or pull request workflow has the exact requested policy."""
    found = set()
    for path in WORKFLOWS.iterdir():
        if path.suffix not in {".yml", ".yaml"}:
            continue
        workflow = load_workflow(path.name)
        triggers = workflow.get("on", workflow.get(True))
        if {"push", "pull_request"}.intersection(triggers):
            found.add(path.name)
            assert workflow["concurrency"] == {
                "group": "${{ github.workflow }}-${{ github.event.pull_request.number || github.ref }}",
                "cancel-in-progress": path.name not in WRITERS,
            }
    assert found == set(PUSH_PR_WORKFLOWS)


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


@pytest.mark.parametrize("filename,job_id", (
    ("quality-gates-by-domain.yml", "domain-tests"),
    ("full-matrix-sweep.yml", "sweep-tests"),
    ("publish.yml", "publish"),
))
def test_subsea_checkout_includes_pinned_manifest_history(filename: str, job_id: str) -> None:
    """Every execution of the manifest contract needs older commit objects."""
    steps = load_workflow(filename)["jobs"][job_id]["steps"]
    checkout = next(step for step in steps if step.get("uses", "").startswith("actions/checkout@"))
    depth = checkout.get("with", {}).get("fetch-depth", 1)
    if isinstance(depth, str) and depth.startswith("${{"):
        depth = evaluate_expression(depth[3:-2], {"matrix.domain": "subsea"})
    assert int(depth) == 0, "Subsea must fetch history to verify the pinned manifest commit"


@pytest.mark.parametrize("event", ("push", "schedule", "workflow_dispatch"))
def test_full_sweep_separates_nightly_from_pushes(event: str) -> None:
    """Retain the test name; sweeps share a ref group without interrupting writes."""
    concurrency = load_workflow("full-matrix-sweep.yml")["concurrency"]
    groups = []
    for run_id in (101, 102):
        context = {
            "github.workflow": load_workflow("full-matrix-sweep.yml")["name"],
            "github.event.pull_request.number": None,
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
        "Full Matrix Sweep-refs/heads/main",
        "Full Matrix Sweep-refs/heads/main",
    ]
    assert concurrency["cancel-in-progress"] is False


def test_full_sweep_bounds_each_shard() -> None:
    """A hung shard releases its runner within forty minutes."""
    shard = load_workflow("full-matrix-sweep.yml")["jobs"]["sweep-tests"]
    assert shard["timeout-minutes"] == 40
    assert shard["strategy"]["fail-fast"] is False


def test_manifest_history_requirement_matches_domain_mapping() -> None:
    """Keep the manifest tests in the shard that fetches their pinned history."""
    domains = (WORKFLOWS.parents[1] / "tests" / "DOMAINS.md").read_text(encoding="utf-8")
    owners = [
        row.split("|")[1].strip()
        for row in domains.splitlines()
        if "`tests/drilling_riser/`" in row
    ]
    assert owners == ["subsea"]
