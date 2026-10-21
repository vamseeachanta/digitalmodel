# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Keep pull-request checks gated on readiness without losing check names."""

import json
import os
from pathlib import Path
import subprocess

import pytest
import yaml


ROOT = Path(__file__).resolve().parents[2]
WORKFLOWS = ROOT / ".github" / "workflows"
SNAPSHOT = json.loads((ROOT / "tests/data/workflow_job_names.json").read_text())
DRAFT_GUARD = (
    "github.event_name != 'pull_request' || github.event.pull_request.draft == false"
)


@pytest.fixture
def register_repository(tmp_path: Path) -> tuple[Path, dict, str]:
    """Capture the checker boundary in disposable Git metadata without commits."""
    env = {key: value for key, value in os.environ.items()
           if not key.startswith("GIT_")}
    repository = tmp_path / "repository"
    subprocess.run(
        ["git", "clone", "--shared", "--no-checkout", str(ROOT), str(repository)],
        env=env, capture_output=True, text=True, check=True,
    )
    base = subprocess.run(
        ["git", "rev-parse", "HEAD"], cwd=repository, env=env,
        capture_output=True, text=True, check=True,
    ).stdout.strip()
    subprocess.run(
        ["git", "update-ref", "refs/remotes/origin/main", base], cwd=repository,
        env=env, capture_output=True, text=True, check=True,
    )
    checker = repository / "scripts/enforcement/check-engineering-register.py"
    checker.parent.mkdir(parents=True)
    checker.write_text(
        "import json, sys\nprint(json.dumps(sys.argv[1:]))\n", encoding="utf-8",
    )
    return repository, env, base


@pytest.mark.parametrize("event,base,success", (
    ("workflow_dispatch", None, True),
    ("workflow_dispatch", "", True),
    ("push", "0" * 40, True),
    ("push", "HEAD", True),
    ("pull_request", "HEAD", True),
    ("pull_request", "", False),
    ("workflow_dispatch", "f" * 40, False),
    ("workflow_dispatch", "--help", False),
))
def test_register_diff_base(register_repository, event, base, success) -> None:
    """Manual runs derive a commit base; explicit bases remain fail-closed."""
    repository, env, head = register_repository
    env = {key: value for key, value in env.items() if key != "REGISTER_BASE"}
    env["GITHUB_EVENT_NAME"] = event
    if base is not None:
        env["REGISTER_BASE"] = base
    workflow = yaml.load(
        (WORKFLOWS / "quality-gates.yml").read_text(encoding="utf-8"),
        Loader=yaml.BaseLoader,
    )
    step = next(step for step in workflow["jobs"]["quality-gates"]["steps"]
                if step.get("name") == "Check engineering register changes")
    result = subprocess.run(
        ["bash", "-euo", "pipefail", "-c", step["run"]], cwd=repository,
        env=env, capture_output=True, text=True,
    )
    assert (result.returncode == 0) == success, result.stderr
    if success:
        assert json.loads(result.stdout) == ["--diff", "HEAD" if base == "HEAD" else head]
    else:
        assert result.stdout.strip() == "" or "::error::" in result.stdout


def test_snapshot_workflows_are_present() -> None:
    """Removing an entire workflow must also fail the job inventory check."""
    assert set(SNAPSHOT) <= {path.name for path in WORKFLOWS.iterdir()}


def has_draft_guard(condition, guard: str) -> bool:
    """Require the guard to gate the entire condition, respecting quoted text."""
    if condition == guard:
        return True
    prefix = f"({guard}) && "
    if not isinstance(condition, str) or not condition.startswith(prefix):
        return False
    remainder = condition[len(prefix):]
    if not remainder.startswith("(") or not remainder[1:-1].strip():
        return False
    depth, quoted = 0, False
    for index, character in enumerate(remainder):
        if character == "'":
            quoted = not quoted
        elif not quoted:
            if character == "(":
                depth += 1
            elif character == ")":
                depth -= 1
                if depth <= 0 and index != len(remainder) - 1:
                    return False
    return depth == 0 and not quoted


@pytest.mark.parametrize(
    "path", sorted((*WORKFLOWS.glob("*.yml"), *WORKFLOWS.glob("*.yaml"))),
    ids=lambda path: path.name,
)
def test_pull_request_draft_policy(path: Path) -> None:
    """Parse all workflows and preserve the base pull-request job inventory."""
    # BaseLoader keeps the Actions event key 'on' as a string.
    workflow = yaml.load(path.read_text(encoding="utf-8"), Loader=yaml.BaseLoader)
    triggers = workflow["on"]
    if isinstance(triggers, str):
        triggers = {triggers: None}
    elif isinstance(triggers, list):
        triggers = dict.fromkeys(triggers)
    pr_events = [event for event in ("pull_request", "pull_request_target")
                 if event in triggers]
    assert (path.name in SNAPSHOT) == bool(pr_events)
    if not pr_events:
        return

    expected_types = [
        "opened", "reopened", "synchronize", "ready_for_review",
    ]
    if path.name in {
        "gmsh-meshing-tests.yml", "parametric-atlas-drift.yml",
        "quality-gates-by-domain.yml", "quality-gates.yml",
    }:
        expected_types = [
            "opened", "synchronize", "reopened", "edited", "ready_for_review",
        ]
    for event in pr_events:
        assert (triggers[event] or {}).get("types") == expected_types
    assert "workflow_dispatch" in triggers
    jobs = workflow["jobs"]
    assert {job_id: job.get("name", job_id) for job_id, job in jobs.items()} == (
        SNAPSHOT[path.name]
    )
    guard = " && ".join(f"github.event_name != '{event}'" for event in pr_events)
    guard += " || github.event.pull_request.draft == false"
    for job in jobs.values():
        assert has_draft_guard(job.get("if"), guard)


def test_draft_policy_runs_in_ci_harness() -> None:
    """Execute this regression check alongside the existing trigger checks."""
    workflow = yaml.load(
        (WORKFLOWS / "quality-gates-by-domain.yml").read_text(encoding="utf-8"),
        Loader=yaml.BaseLoader,
    )
    commands = "\n".join(
        step.get("run", "") for step in workflow["jobs"]["ci-harness-tests"]["steps"]
    )
    assert "tests/workflows/test_ci_draft_policy.py" in commands


def policy_workflow(tmp_path: Path, triggers=None, condition=DRAFT_GUARD) -> Path:
    """Write a disposable workflow retaining the snapshotted documentation job."""
    workflow = yaml.load(
        (WORKFLOWS / "docs.yml").read_text(encoding="utf-8"), Loader=yaml.BaseLoader,
    )
    if triggers is not None:
        workflow["on"] = triggers
    workflow["jobs"]["docs"]["if"] = condition
    path = tmp_path / "docs.yml"
    path.write_text(yaml.safe_dump(workflow), encoding="utf-8")
    return path


@pytest.mark.parametrize("condition", (
    DRAFT_GUARD + " || always()",
    "always() || " + DRAFT_GUARD,
    f"always() || ({DRAFT_GUARD}) && (success())",
    f"({DRAFT_GUARD}) && (success()) || (always())",
    f"!({DRAFT_GUARD})",
    f'"{DRAFT_GUARD}"',
    DRAFT_GUARD + " && (always())",
    f"({DRAFT_GUARD}) && ()",
    f"({DRAFT_GUARD}) && (always()",
    "",
    None,
))
def test_draft_guard_rejects_bypasses(tmp_path: Path, condition) -> None:
    """Guard text alone must not let draft-running conditions pass the policy."""
    path = policy_workflow(tmp_path, condition=condition)
    with pytest.raises(AssertionError):
        test_pull_request_draft_policy(path)


@pytest.mark.parametrize("condition", (
    DRAFT_GUARD,
    f"({DRAFT_GUARD}) && (always())",
    f"({DRAFT_GUARD}) && (contains(')''(', '(') || success())",
))
def test_draft_guard_accepts_gated_conditions(tmp_path: Path, condition) -> None:
    """Retain ordinary and aggregate guards, including quoted parentheses."""
    test_pull_request_draft_policy(policy_workflow(tmp_path, condition=condition))


@pytest.mark.parametrize("triggers", (
    "pull_request", ["pull_request"],
    "pull_request_target", ["pull_request_target", "workflow_dispatch"],
))
def test_pr_shorthand_requires_explicit_types(tmp_path: Path, triggers) -> None:
    """Shorthand PR events fail the types policy instead of raising TypeError."""
    with pytest.raises(AssertionError):
        test_pull_request_draft_policy(policy_workflow(tmp_path, triggers))


@pytest.mark.parametrize("triggers", (
    "push", ["push", "workflow_dispatch"], "pull_request_target_extra",
))
def test_non_pr_shorthand_uses_exact_event_names(tmp_path: Path, triggers) -> None:
    """Non-PR event names must not be mistaken for a pull-request subscription."""
    path = policy_workflow(tmp_path, triggers).rename(tmp_path / "non-pr.yml")
    test_pull_request_draft_policy(path)


@pytest.mark.parametrize("both_events", (False, True))
def test_target_event_requires_its_own_guard(tmp_path: Path, both_events) -> None:
    """Target workflows obey the same readiness policy for every subscribed event."""
    triggers = {
        "pull_request_target": {
            "types": ["opened", "reopened", "synchronize", "ready_for_review"],
        },
        "workflow_dispatch": None,
    }
    guard = (
        "github.event_name != 'pull_request_target'"
        " || github.event.pull_request.draft == false"
    )
    if both_events:
        triggers["pull_request"] = triggers["pull_request_target"]
        guard = "github.event_name != 'pull_request' && " + guard
    test_pull_request_draft_policy(policy_workflow(tmp_path, triggers, guard))
    with pytest.raises(AssertionError):
        test_pull_request_draft_policy(policy_workflow(tmp_path, triggers))
