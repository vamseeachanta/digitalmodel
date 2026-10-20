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
PR_WORKFLOWS = {
    "docs.yml", "gmsh-meshing-tests.yml", "parametric-atlas-drift.yml",
    "quality-gates-by-domain.yml", "quality-gates.yml", "repo-structure.yml",
    "store-data-tests.yml", "workflow-automation-tests.yml",
}
DRAFT_GUARD = (
    "github.event_name != 'pull_request' || github.event.pull_request.draft == false"
)


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
        "opened", "reopened", "synchronize", "ready_for_review",
    ]
    assert "workflow_dispatch" in triggers
    for job in jobs.values():
        assert DRAFT_GUARD in job.get("if", "")


@pytest.mark.parametrize(
    "filename,job_id,minutes",
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
        ("publish.yml", "publish", 60),
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
            assert 0 < int(job.get("timeout-minutes", "0")) <= 60, (filename, job_id)


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
