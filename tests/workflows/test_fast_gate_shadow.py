"""The fast gate runs in shadow: advisory, beside the domain matrix, changing no existing job."""

import pytest

from tests.workflows.test_ci_triggers import job_state, load_workflow

WORKFLOW = "quality-gates-by-domain.yml"
SHADOW = ("fast-gate-shadow", "fast-gate-shadow-agreement")
JOBS = load_workflow(WORKFLOW)["jobs"]


def needs(job):
    value = job.get("needs", [])
    return {value} if isinstance(value, str) else set(value)


def test_no_existing_job_depends_on_the_shadow():
    for job_id, job in JOBS.items():
        if job_id not in SHADOW:
            assert not needs(job) & set(SHADOW), job_id
    assert needs(JOBS["domain-tests-required"]) == {"detect-domains", "ci-harness-tests", "domain-tests"}


def test_only_the_agreement_record_reads_the_fast_gate():
    readers = {job_id for job_id, job in JOBS.items() if "fast-gate-shadow" in needs(job)}
    assert readers == {"fast-gate-shadow-agreement"}
    assert needs(JOBS["fast-gate-shadow-agreement"]) == {"detect-domains", "domain-tests", "fast-gate-shadow"}


@pytest.mark.parametrize("job_id,name", [
    ("fast-gate-shadow", "fast-gate (shadow)"),
    ("fast-gate-shadow-agreement", "fast-gate (shadow) agreement"),
])
def test_shadow_check_names_say_shadow(job_id, name):
    assert job_state(WORKFLOW, job_id, "code update")[0] == name
    assert JOBS[job_id]["runs-on"] == "ubuntu-latest"


def test_fast_gate_records_its_verdict_instead_of_failing():
    """The test step runs the selector's own runner, which exits 0 whatever pytest returns."""
    steps = {step["name"]: step for step in JOBS["fast-gate-shadow"]["steps"]}
    run = steps["Run selected test files in one job"]["run"]
    assert "scripts/ci/fast_gate.py run" in run
    assert "continue-on-error" not in JOBS["fast-gate-shadow"], "the verdict, not the job result, carries a failure"
    upload = steps["Upload fast gate result"]
    assert upload["if"] == "always()" and upload["with"]["name"] == "fast-gate-shadow"


def test_agreement_runs_after_the_matrix_whatever_its_result():
    job = JOBS["fast-gate-shadow-agreement"]
    assert job["if"].startswith("${{ always() && ")
    record = next(step for step in job["steps"] if step["name"] == "Record verdict agreement")
    assert record["env"]["MATRIX_RESULT"] == "${{ needs.domain-tests.result }}"
    assert record["env"]["FAST_RESULT"] == "${{ needs.fast-gate-shadow.result }}"
    assert "fast_gate.py agree" in record["run"]
