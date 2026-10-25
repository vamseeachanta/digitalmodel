# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Keep pull-request checks gated on readiness without losing check names."""

import json
from pathlib import Path

import pytest
import yaml


ROOT = Path(__file__).resolve().parents[2]
WORKFLOWS = ROOT / ".github" / "workflows"
SNAPSHOT = json.loads((ROOT / "tests/data/workflow_job_names.json").read_text(encoding="utf-8"))
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
    # Provisional execution allocations, excluding queue time, not measurements:
    # control jobs: 5 setup + 5 evaluation + 5 service calls = 15 minutes;
    # test/build jobs: 15 dependencies + 40 checks + 5 reporting = 60 minutes;
    # publication: 120 tests + 30 dependency/build + 30 publication = 180 minutes.
    # The existing 20- and 40-minute caps remain unchanged; no measured runtime
    # or equivalence of the sweep and pull request budgets is established.
    [
        ("capability-card-links.yml", "card-links", 15),
        ("docs.yml", "docs", 60),
        ("full-matrix-sweep.yml", "full-matrix", 15),
        ("full-matrix-sweep.yml", "sweep-tests", 40),
        ("full-matrix-sweep.yml", "sweep-result", 15),
        ("gmsh-meshing-tests.yml", "focused-tests", 20),
        ("pages.yml", "build", 60),
        ("pages.yml", "deploy", 15),
        ("parametric-atlas-drift.yml", "atlas-drift", 15),
        ("publish.yml", "publish", 180),
        ("quality-gates-by-domain.yml", "detect-domains", 15),
        ("quality-gates-by-domain.yml", "ci-harness-tests", 60),
        ("quality-gates-by-domain.yml", "domain-tests", 60),
        ("quality-gates-by-domain.yml", "domain-tests-required", 15),
        ("quality-gates.yml", "quality-gates-aggregate", 15),
        ("quality-gates.yml", "quality-gates", 60),
        ("repo-structure.yml", "repo-structure", 15),
        ("store-data-tests.yml", "content-store", 60),
        ("workflow-automation-tests.yml", "test", 60),
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
            limit = 180 if filename == "publish.yml" else 60
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
    workflow = yaml.load((WORKFLOWS / filename).read_text(encoding="utf-8"), Loader=yaml.BaseLoader)
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
    workflow = yaml.load(path.read_text(encoding="utf-8"), Loader=yaml.BaseLoader)
    workflow["jobs"][job_id]["name"] = "changed-check"
    monkeypatch.setattr(yaml, "load", lambda *args, **kwargs: workflow)
    with pytest.raises(AssertionError):
        test_pull_request_draft_policy(path)


@pytest.mark.parametrize("filename", sorted(PR_WORKFLOWS))
@pytest.mark.parametrize("event", ["edited", "ready_for_review"])
def test_readiness_event_mutations_are_rejected(filename: str, event: str, monkeypatch) -> None:
    """Deleting either required activity cannot silently disable its checks."""
    path = WORKFLOWS / filename
    workflow = yaml.load(path.read_text(encoding="utf-8"), Loader=yaml.BaseLoader)
    workflow["on"]["pull_request"]["types"] = [
        value for value in workflow["on"]["pull_request"]["types"] if value != event
    ]
    monkeypatch.setattr(yaml, "load", lambda *args, **kwargs: workflow)
    with pytest.raises(AssertionError):
        test_pull_request_draft_policy(path)
