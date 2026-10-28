# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Contracts for trusted PR and scheduled runner selection."""

from typing import Any
import re

import pytest

from tests.workflows.test_ci_triggers import WORKFLOWS, evaluate_expression, load_workflow


ROUTED_JOBS = (
    ("quality-gates-by-domain.yml", "detect-domains"),
    ("quality-gates-by-domain.yml", "ci-harness-tests"),
    ("quality-gates-by-domain.yml", "domain-tests"),
    ("quality-gates-by-domain.yml", "domain-tests-required"),
    ("repo-structure.yml", "repo-structure"),
    ("store-data-tests.yml", "content-store"),
    ("workflow-automation-tests.yml", "test"),
    ("full-matrix-sweep.yml", "full-matrix"),
    ("full-matrix-sweep.yml", "sweep-tests"),
    ("full-matrix-sweep.yml", "sweep-result"),
    ("quality-gates.yml", "quality-gates-aggregate"),
    ("quality-gates.yml", "quality-gates"),
    ("docs.yml", "docs"),
    ("parametric-atlas-drift.yml", "atlas-drift"),
)
GMSH_JOB = ("gmsh-meshing-tests.yml", "focused-tests")
RUNNER_SETTINGS = (
    None, "", "unknown", '"unknown"', '"ubuntu-latest"',
    '["self-hosted","Linux","ARM64"]', "linux-arm64", "self-hosted-arm64",
    "SeLf-HoStEd-ArM64",
)
TRUST_CASES = (
    ("pull_request", "base/repo", False, True),
    ("pull_request", "base/repo", True, True),
    ("pull_request", "fork/repo", False, False),
    ("pull_request", "fork/repo", True, False),
    ("push", "", False, True),
    ("workflow_dispatch", "", False, True),
    ("schedule", "", False, True),
)


def runner_context(
    event: str, head: str, setting: str | None, draft: bool = False,
) -> dict[str, Any]:
    """Use synthetic repositories and the existing hosted matrix defaults."""
    context = {
        "github.repository": "base/repo",
        "github.event_name": event,
        "github.event.pull_request.head.repo.full_name": head,
        "github.event.pull_request.draft": draft,
        "matrix.domain": "contracts",
        "matrix.runner": "ubuntu-latest",
        "matrix.os": "ubuntu-latest",
    }
    if setting is not None:
        context["vars.PR_GATE_RUNNER"] = setting
    return context


def selected_runner(filename: str, job_id: str, context: dict[str, Any]) -> Any:
    """Evaluate the actual job selection, including Actions' unset variable."""
    expression = load_workflow(filename)["jobs"][job_id]["runs-on"]
    if not expression.startswith("${{"):
        return expression
    assert expression.endswith("}}")
    return evaluate_expression(expression[3:-2], {"vars.PR_GATE_RUNNER": "", **context})


def test_routing_inventory_covers_prs_and_reusable_full_sweep_jobs() -> None:
    """Discover every executable job reachable from PRs or the full schedule."""
    reached = set()
    jobs = set()

    def visit(filename):
        if filename in reached:
            return
        reached.add(filename)
        for job_id, job in load_workflow(filename)["jobs"].items():
            if "uses" in job:
                assert job["uses"].startswith("./.github/workflows/")
                visit(job["uses"].rsplit("/", 1)[-1])
            else:
                jobs.add((filename, job_id))

    for path in WORKFLOWS.glob("*.yml"):
        document = load_workflow(path.name)
        triggers = document.get("on", document.get(True))
        assert "pull_request_target" not in triggers
        if "pull_request" in triggers or {"cron": "17 */12 * * *"} in triggers.get("schedule", []):
            visit(path.name)
    assert jobs == {*ROUTED_JOBS, GMSH_JOB}


@pytest.mark.parametrize("filename,job_id", ROUTED_JOBS)
def test_routing_preserves_job_conditions(filename: str, job_id: str) -> None:
    """Runner selection cannot turn executed gates into successful skips."""
    conditions = {
        ("quality-gates-by-domain.yml", "ci-harness-tests"):
            "needs.detect-domains.outputs.has-harness == 'true'",
        ("quality-gates-by-domain.yml", "domain-tests"):
            "needs.detect-domains.outputs.has-domains == 'true'",
        ("quality-gates-by-domain.yml", "domain-tests-required"): "always()",
        ("quality-gates.yml", "quality-gates-aggregate"): "always()",
        ("full-matrix-sweep.yml", "sweep-result"): "always()",
    }
    job = load_workflow(filename)["jobs"][job_id]
    assert job.get("if") == conditions.get((filename, job_id))


@pytest.mark.parametrize("filename,job_id", ROUTED_JOBS)
@pytest.mark.parametrize("setting", RUNNER_SETTINGS)
@pytest.mark.parametrize("event,head,draft,trusted", TRUST_CASES)
def test_runner_selection_uses_only_trusted_allowlisted_overrides(
    filename: str, job_id: str, setting: str | None,
    event: str, head: str, draft: bool, trusted: bool,
) -> None:
    """Unknown values stay hosted; drafts execute and forks remain hosted."""
    expected = "ubuntu-latest"
    if trusted and setting in ("self-hosted-arm64", "SeLf-HoStEd-ArM64"):
        expected = ["self-hosted", "Linux", "ARM64"]
    context = runner_context(event, head, setting, draft)
    assert selected_runner(filename, job_id, context) == expected
    condition = load_workflow(filename)["jobs"][job_id].get("if", "")
    assert "head.repo" not in condition and "PR_GATE_RUNNER" not in condition


@pytest.mark.parametrize("filename,job_id,matrix_key", (
    ("quality-gates-by-domain.yml", "domain-tests", "matrix.runner"),
    ("full-matrix-sweep.yml", "sweep-tests", "matrix.runner"),
    ("workflow-automation-tests.yml", "test", "matrix.os"),
))
@pytest.mark.parametrize("setting", (None, "", "unknown", "self-hosted-arm64"))
def test_trusted_matrix_fallback_is_preserved(
    filename: str, job_id: str, matrix_key: str, setting: str | None,
) -> None:
    """Unconfigured matrices keep their existing runner, including other labels."""
    context = runner_context("workflow_dispatch", "", setting)
    context[matrix_key] = "ubuntu-22.04"
    expected = ["self-hosted", "Linux", "ARM64"] if setting == "self-hosted-arm64" else "ubuntu-22.04"
    assert selected_runner(filename, job_id, context) == expected


@pytest.mark.parametrize("filename", sorted({name for name, _ in ROUTED_JOBS} | {GMSH_JOB[0]}))
def test_drafts_cannot_skip_jobs_or_steps(filename: str) -> None:
    """Draft state cannot replace checks with skipped jobs or steps."""
    for job_id, job in load_workflow(filename)["jobs"].items():
        for entry in [job, *job.get("steps", [])]:
            condition = str(entry.get("if", ""))
            assert not re.search(r"\bdraft\b", condition, re.IGNORECASE), (
                filename, job_id, entry.get("name"), condition,
            )


@pytest.mark.parametrize("filename,job_id", (
    ("quality-gates-by-domain.yml", "domain-tests"),
    ("full-matrix-sweep.yml", "sweep-tests"),
))
@pytest.mark.parametrize("domain", (
    "solver-smoke", "specialized", "marine-engineering", "subsea",
    "infrastructure-other",
))
@pytest.mark.parametrize("setting", (None, "", "unknown", "self-hosted-arm64"))
@pytest.mark.parametrize("event,head,draft", (
    ("pull_request", "base/repo", False),
    ("pull_request", "base/repo", True),
    ("schedule", "", False),
))
def test_arm_incompatible_shards_keep_original_matrix_runner(
    filename: str, job_id: str, domain: str, setting: str | None,
    event: str, head: str, draft: bool,
) -> None:
    """Native builds, binary fixtures and timing baselines keep the fallback."""
    context = runner_context(event, head, setting, draft)
    context.update({"matrix.domain": domain, "matrix.runner": "ubuntu-22.04"})
    assert selected_runner(filename, job_id, context) == "ubuntu-22.04"


@pytest.mark.parametrize("filename,job_id,matrix_key", (
    ("quality-gates-by-domain.yml", "domain-tests", "matrix.runner"),
    ("full-matrix-sweep.yml", "sweep-tests", "matrix.runner"),
    ("workflow-automation-tests.yml", "test", "matrix.os"),
))
@pytest.mark.parametrize("domain", ("subsea", "solver-smoke", "specialized"))
@pytest.mark.parametrize("runner", ("self-hosted", ["self-hosted", "Linux", "ARM64"]))
@pytest.mark.parametrize("setting", (None, "self-hosted-arm64"))
@pytest.mark.parametrize("draft", (False, True))
def test_fork_matrix_cannot_select_self_hosted_runner(
    filename: str, job_id: str, matrix_key: str, domain: str,
    runner: Any, setting: str | None, draft: bool,
) -> None:
    """Fork-controlled matrix output cannot bypass the job-level guard."""
    context = runner_context("pull_request", "fork/repo", setting, draft)
    context.update({"matrix.domain": domain, matrix_key: runner})
    assert selected_runner(filename, job_id, context) == "ubuntu-latest"


def test_focused_gmsh_job_stays_hosted() -> None:
    """The dedicated Gmsh job cannot run on ARM64."""
    assert load_workflow(GMSH_JOB[0])["jobs"][GMSH_JOB[1]]["runs-on"] == "ubuntu-latest"


@pytest.mark.parametrize("filename", sorted({name for name, _ in ROUTED_JOBS} | {GMSH_JOB[0]}))
def test_checkout_credentials_and_pr_permissions(filename: str) -> None:
    """PR code gets read-only tokens and never retains checkout credentials."""
    document = load_workflow(filename)
    triggers = document.get("on", document.get(True))
    if "pull_request" in triggers:
        assert document["permissions"]
        assert set(document["permissions"].values()) == {"read"}
    for job in document["jobs"].values():
        if "pull_request" in triggers:
            assert set(job.get("permissions", document["permissions"]).values()) <= {"read"}
            assert "secrets" not in job
            for step in job.get("steps", []):
                assert "secrets." not in str(step)
        for step in job.get("steps", []):
            if step.get("uses", "").startswith("actions/checkout@"):
                assert step["with"]["persist-credentials"] is False


def test_routing_contract_runs_in_harness() -> None:
    """The independent harness runs routing contracts when CI files change."""
    job = load_workflow("quality-gates-by-domain.yml")["jobs"]["ci-harness-tests"]
    commands = "\n".join(step.get("run", "") for step in job["steps"])
    assert "tests/workflows/test_ci_runner_routing.py" in commands


@pytest.mark.parametrize("filename,job_id", (
    ("quality-gates-by-domain.yml", "domain-tests"),
    ("full-matrix-sweep.yml", "sweep-tests"),
))
def test_shard_uv_uses_configured_python(filename: str, job_id: str) -> None:
    """Managed uv interpreters must match the shard's Python setup."""
    job = load_workflow(filename)["jobs"][job_id]
    setup = next(step for step in job["steps"]
                 if step.get("uses", "").startswith("actions/setup-python@"))
    assert job["env"]["UV_PYTHON"] == setup["with"]["python-version"] == "3.11"
