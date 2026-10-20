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
WORKFLOW_EVENTS = {
    path.name: yaml.load(path.read_text(encoding="utf-8"), Loader=yaml.BaseLoader)["on"]
    for path in sorted((*WORKFLOWS.glob("*.yml"), *WORKFLOWS.glob("*.yaml")))
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
    filename: str, event: str, run_id: int, number: int = 1, ref: str | None = None,
) -> tuple[str, bool]:
    """Render the actual workflow group and cancellation expression."""
    workflow = load_workflow(filename)
    concurrency = workflow["concurrency"]
    context = {
        "github.workflow": workflow["name"],
        "github.event_name": event,
        "github.event.pull_request.number": number,
        "github.ref": ref or (
            f"refs/pull/{number}/merge" if event == "pull_request" else "refs/heads/main"
        ),
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


@pytest.mark.parametrize("filename", [
    filename for filename, events in WORKFLOW_EVENTS.items() if "pull_request" in events
])
def test_pull_request_concurrency(filename: str) -> None:
    """Only updates of the same pull request supersede earlier runs."""
    first = concurrency_result(filename, "pull_request", 101, number=1)
    update = concurrency_result(filename, "pull_request", 102, number=1)
    other = concurrency_result(filename, "pull_request", 103, number=2)
    assert first == update
    assert first[0] != other[0]
    assert first[0] == f"{load_workflow(filename)['name']}-refs/pull/1/merge"
    assert first[1] is True
    assert other[1] is True


@pytest.mark.parametrize("filename", [
    filename for filename, events in WORKFLOW_EVENTS.items() if "push" in events
])
def test_push_updates_cancel_only_the_same_ref(filename: str) -> None:
    """A newer main push supersedes the old run without cancelling another ref."""
    first = concurrency_result(filename, "push", 101)
    update = concurrency_result(filename, "push", 102)
    other = concurrency_result(filename, "push", 103, ref="refs/heads/develop")
    assert first == update == (f"{load_workflow(filename)['name']}-refs/heads/main", True)
    assert other == (f"{load_workflow(filename)['name']}-refs/heads/develop", True)


@pytest.mark.parametrize("filename,event", [
    (filename, event)
    for filename, events in WORKFLOW_EVENTS.items()
    if filename != "full-matrix-sweep.yml"
    for event in ("schedule", "workflow_dispatch") if event in events
])
def test_scheduled_and_manual_runs_are_never_cancelled(filename: str, event: str) -> None:
    """Independent groups protect manual and scheduled runs from push updates."""
    first = concurrency_result(filename, event, 101)
    second = concurrency_result(filename, event, 102)
    assert first == (f"{load_workflow(filename)['name']}-refs/heads/main-101", False)
    assert second == (f"{load_workflow(filename)['name']}-refs/heads/main-102", False)
    events = WORKFLOW_EVENTS[filename]
    if "push" in events:
        assert first[0] != concurrency_result(filename, "push", 103)[0]
    if "pull_request" in events:
        assert first[0] != concurrency_result(filename, "pull_request", 104)[0]


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
    assert set(triggers) == {"schedule", "workflow_dispatch"}


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


@pytest.mark.parametrize("event", ("schedule", "workflow_dispatch"))
def test_full_sweep_replaces_older_sweep_on_same_ref(event: str) -> None:
    """The diagnostic sweep has an explicit per-ref cancellation policy."""
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
    assert "github.run_id" not in concurrency["group"]
    assert groups == ["full-matrix-sweep-refs/heads/main"] * 2
    assert concurrency["cancel-in-progress"] is True


def test_full_sweep_still_builds_every_shard() -> None:
    """Nightly and manual sweeps keep the full detector and failure aggregate."""
    jobs = load_workflow("full-matrix-sweep.yml")["jobs"]
    builder = next(step for step in jobs["full-matrix"]["steps"] if step.get("id") == "matrix")
    assert "scripts/ci/detect_touched_domains.py" in builder["run"]
    assert "--mode full" in builder["run"]
    assert "--mode touched" not in builder["run"]
    assert "if" not in jobs["full-matrix"]
    assert "if" not in jobs["sweep-tests"]
    assert jobs["sweep-result"]["if"] == "always()"
    assert jobs["sweep-result"]["needs"] == ["full-matrix", "sweep-tests"]
    enforce = jobs["sweep-result"]["steps"][-1]
    assert '"$MATRIX_RESULT" != "success"' in enforce["run"]
    assert '"$SWEEP_RESULT" != "success"' in enforce["run"]
    assert enforce["run"].count("exit 1") == 2


@pytest.mark.parametrize("event", ("pull_request", "push", "workflow_dispatch"))
@pytest.mark.parametrize("domains", ([], [{"domain": "subsea", "runner": "ubuntu-latest"}]))
def test_domain_detection_routes_events(event: str, domains: list, tmp_path: Path) -> None:
    """Execute the shipped routing shell with a recording detector substitute."""
    workflow = load_workflow("quality-gates-by-domain.yml")
    assert workflow["on"]["push"]["branches"] == ["main", "develop"]
    detect = next(
        step for step in workflow["jobs"]["detect-domains"]["steps"]
        if step.get("id") == "detect"
    )
    assert detect["env"] == {
        "EVENT_NAME": "${{ github.event_name }}",
        "PR_BASE_SHA": "${{ github.event.pull_request.base.sha }}",
        "PR_HEAD_SHA": "${{ github.event.pull_request.head.sha }}",
        "PUSH_BEFORE_SHA": "${{ github.event.before }}",
        "PUSH_AFTER_SHA": "${{ github.event.after }}",
    }
    # Only replace the uv command boundary; execute the actual branch logic.
    uv = tmp_path / "uv"
    uv.write_text(f"#!{sys.executable}\n" + '''
import json
import os
from pathlib import Path
import sys

args = sys.argv[1:]
assert args[:3] == ["run", "--no-sources", "python"]
if args[3] == "scripts/ci/detect_touched_domains.py":
    Path(os.environ["DETECTOR_ARGS"]).write_text(json.dumps(args[4:]))
    print(os.environ["DETECTOR_MATRIX"])
else:
    assert args[3] == "-c"
    os.execv(sys.executable, [sys.executable, *args[3:]])
''', encoding="utf-8")
    uv.chmod(0o755)
    matrix = json.dumps({"include": domains}, separators=(",", ":"))
    arguments = tmp_path / "arguments.json"
    output = tmp_path / "output"
    environment = {
        **os.environ,
        "PATH": str(tmp_path) + os.pathsep + os.environ["PATH"],
        "EVENT_NAME": event,
        "PR_BASE_SHA": "pr-base", "PR_HEAD_SHA": "pr-head",
        "PUSH_BEFORE_SHA": "push-before", "PUSH_AFTER_SHA": "push-after",
        "DETECTOR_ARGS": str(arguments), "DETECTOR_MATRIX": matrix,
        "GITHUB_OUTPUT": str(output),
    }
    subprocess.run(["bash", "-e", "-o", "pipefail", "-c", detect["run"]],
                   env=environment, check=True, capture_output=True, text=True)
    expected = ["--mode", "full"]
    if event == "pull_request":
        expected = ["--mode", "touched", "--base", "pr-base", "--head", "pr-head"]
    elif event == "push":
        expected = ["--mode", "touched", "--base", "push-before", "--head", "push-after",
                    "--on-missing-base", "full"]
    expected += ["--domains-file", "tests/DOMAINS.md", "--output-format", "json-matrix"]
    assert json.loads(arguments.read_text()) == expected
    assert output.read_text().splitlines() == [
        f"matrix={matrix}", f"has_domains={str(bool(domains)).lower()}",
    ]


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
