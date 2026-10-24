# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Keep pull-request checks gated on readiness without losing check names."""

import json
from pathlib import Path

import pytest
import yaml


ROOT = Path(__file__).resolve().parents[2]
WORKFLOWS = ROOT / ".github" / "workflows"
SNAPSHOT = json.loads((ROOT / "tests/data/workflow_job_names.json").read_text())
POLICY = json.loads((ROOT / "docs/ci/runtime-policy.json").read_text())
PR_WORKFLOWS = {
    "docs.yml", "gmsh-meshing-tests.yml", "parametric-atlas-drift.yml",
    "quality-gates-by-domain.yml", "quality-gates.yml", "repo-structure.yml",
    "store-data-tests.yml", "workflow-automation-tests.yml",
}
DRAFT_GUARD = (
    "github.event_name != 'pull_request' || github.event.pull_request.draft == false"
)
REQUIRED_AGGREGATES = {"domain-tests-required", "quality-gates-aggregate"}


def test_snapshot_workflows_are_present() -> None:
    """Removing an entire workflow must also fail the job inventory check."""
    assert set(SNAPSHOT) == {
        path.name for path in WORKFLOWS.iterdir() if path.suffix in {".yml", ".yaml"}
    }


def test_runtime_policy_covers_every_job() -> None:
    """Every limit needs a documented allocation and an explicit rationale."""
    assert POLICY["basis"] == "explicit_resource_policy"
    assignments = POLICY["jobs"]
    assert set(assignments) == set(SNAPSHOT)
    assert all(set(assignments[name]) == set(jobs) for name, jobs in SNAPSHOT.items())
    used_classes = {name for jobs in assignments.values() for name in jobs.values()}
    assert used_classes == set(POLICY["budgets"])
    for budget in POLICY["budgets"].values():
        assert budget["rationale"].strip()
        assert budget["allocations_minutes"]
        assert all(type(value) is int and value > 0
                   for value in budget["allocations_minutes"].values())
        assert type(budget["margin_minutes"]) is int
        assert budget["margin_minutes"] >= 0


def budget_minutes(name: str) -> int:
    """Derive the cap from the documented work allocations and margin."""
    budget = POLICY["budgets"][name]
    return sum(budget["allocations_minutes"].values()) + budget["margin_minutes"]


def test_runtime_policy_missing_job_is_rejected(monkeypatch) -> None:
    """Policy-derived parametrization cannot hide a removed job assignment."""
    monkeypatch.setitem(POLICY["jobs"], "docs.yml", {})
    with pytest.raises(AssertionError):
        test_runtime_policy_covers_every_job()


@pytest.mark.parametrize(
    "path", sorted((*WORKFLOWS.glob("*.yml"), *WORKFLOWS.glob("*.yaml"))),
    ids=lambda path: path.name,
)
def test_pull_request_draft_policy(path: Path) -> None:
    """Parse every workflow and preserve all job identifiers and check names."""
    # BaseLoader keeps the Actions event key 'on' as a string.
    workflow = yaml.load(path.read_text(encoding="utf-8"), Loader=yaml.BaseLoader)
    triggers = workflow["on"]
    jobs = workflow["jobs"]
    assert {job_id: job.get("name", job_id) for job_id, job in jobs.items()} == (
        SNAPSHOT[path.name]
    )
    assert (path.name in PR_WORKFLOWS) == ("pull_request" in triggers)
    if "pull_request" not in triggers:
        return

    assert (triggers["pull_request"] or {}).get("types") == [
        "opened", "synchronize", "reopened", "edited", "ready_for_review",
    ]
    assert "workflow_dispatch" in triggers
    for job_id, job in jobs.items():
        if job_id in REQUIRED_AGGREGATES:
            assert job["if"] == "always()"
        else:
            assert DRAFT_GUARD in job.get("if", "")


@pytest.mark.parametrize(
    "filename,job_id,minutes",
    [
        (filename, job_id, budget_minutes(budget))
        for filename, jobs in POLICY["jobs"].items()
        for job_id, budget in jobs.items()
    ],
)
def test_job_timeout(filename: str, job_id: str, minutes: int) -> None:
    """Bound each job, retaining the existing twenty and forty minute limits."""
    workflow = yaml.load(
        (WORKFLOWS / filename).read_text(encoding="utf-8"), Loader=yaml.BaseLoader,
    )
    assert workflow["jobs"][job_id].get("timeout-minutes") == str(minutes)


def test_timeout_inventory_covers_every_job() -> None:
    """A new job needs an explicit timeout even before its budget is pinned."""
    for filename in SNAPSHOT:
        workflow = yaml.load(
            (WORKFLOWS / filename).read_text(encoding="utf-8"), Loader=yaml.BaseLoader,
        )
        for job_id, job in workflow["jobs"].items():
            limit = budget_minutes("publication" if filename == "publish.yml" else "standard")
            assert 0 < int(job.get("timeout-minutes", "0")) <= limit, (filename, job_id)


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


@pytest.mark.parametrize("filename,job_id", [
    (filename, job_id) for filename, jobs in SNAPSHOT.items() for job_id in jobs
])
@pytest.mark.parametrize("mutation", ["missing", "zero", "excessive"])
def test_timeout_mutations_are_rejected(filename: str, job_id: str, mutation: str, monkeypatch) -> None:
    """Every committed job limit has negative controls without editing workflows."""
    workflow = yaml.load((WORKFLOWS / filename).read_text(), Loader=yaml.BaseLoader)
    job = workflow["jobs"][job_id]
    minutes = int(job["timeout-minutes"])
    if mutation == "missing":
        del job["timeout-minutes"]
    else:
        job["timeout-minutes"] = "0" if mutation == "zero" else str(minutes + 1)
    monkeypatch.setattr(yaml, "load", lambda *args, **kwargs: workflow)
    with pytest.raises(AssertionError):
        test_job_timeout(filename, job_id, minutes)


@pytest.mark.parametrize("filename,job_id", [
    (filename, job_id) for filename, jobs in SNAPSHOT.items() for job_id in jobs
])
def test_check_name_mutations_are_rejected(filename: str, job_id: str, monkeypatch) -> None:
    """Every job name remains bound to the committed inventory."""
    path = WORKFLOWS / filename
    workflow = yaml.load(path.read_text(), Loader=yaml.BaseLoader)
    workflow["jobs"][job_id]["name"] = "changed-check"
    monkeypatch.setattr(yaml, "load", lambda *args, **kwargs: workflow)
    with pytest.raises(AssertionError):
        test_pull_request_draft_policy(path)


@pytest.mark.parametrize("filename", sorted(PR_WORKFLOWS))
@pytest.mark.parametrize("event", ["edited", "ready_for_review"])
def test_readiness_event_mutations_are_rejected(filename: str, event: str, monkeypatch) -> None:
    """Deleting either required activity cannot silently disable its checks."""
    path = WORKFLOWS / filename
    workflow = yaml.load(path.read_text(), Loader=yaml.BaseLoader)
    workflow["on"]["pull_request"]["types"] = [
        value for value in workflow["on"]["pull_request"]["types"] if value != event
    ]
    monkeypatch.setattr(yaml, "load", lambda *args, **kwargs: workflow)
    with pytest.raises(AssertionError):
        test_pull_request_draft_policy(path)
