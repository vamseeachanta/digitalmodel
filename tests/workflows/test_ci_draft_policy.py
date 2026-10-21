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
DRAFT_GUARD = (
    "github.event_name != 'pull_request' || github.event.pull_request.draft == false"
)


def test_snapshot_workflows_are_present() -> None:
    """Removing an entire workflow must also fail the job inventory check."""
    assert set(SNAPSHOT) <= {path.name for path in WORKFLOWS.iterdir()}


@pytest.mark.parametrize(
    "path", sorted((*WORKFLOWS.glob("*.yml"), *WORKFLOWS.glob("*.yaml"))),
    ids=lambda path: path.name,
)
def test_pull_request_draft_policy(path: Path) -> None:
    """Parse all workflows and preserve the base pull-request job inventory."""
    # BaseLoader keeps the Actions event key 'on' as a string.
    workflow = yaml.load(path.read_text(encoding="utf-8"), Loader=yaml.BaseLoader)
    triggers = workflow["on"]
    assert (path.name in SNAPSHOT) == ("pull_request" in triggers)
    if "pull_request" not in triggers:
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
    assert (triggers["pull_request"] or {}).get("types") == expected_types
    assert "workflow_dispatch" in triggers
    jobs = workflow["jobs"]
    assert {job_id: job.get("name", job_id) for job_id, job in jobs.items()} == (
        SNAPSHOT[path.name]
    )
    for job in jobs.values():
        assert DRAFT_GUARD in job.get("if", "")


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
