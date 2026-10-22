# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Pin draft guards and preserve the existing sweep and runtime policies."""

import json
import os
from pathlib import Path
import subprocess
import sys

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
WORKFLOWS = ROOT / ".github/workflows"
SNAPSHOT = json.loads((ROOT / "tests/data/workflow_job_names.json").read_text())
PR_WORKFLOWS = {
    "docs.yml", "gmsh-meshing-tests.yml", "parametric-atlas-drift.yml",
    "quality-gates-by-domain.yml", "quality-gates.yml", "repo-structure.yml",
    "store-data-tests.yml", "workflow-automation-tests.yml",
}
DRAFT_GUARD = (
    "github.event_name != 'pull_request' || github.event.pull_request.draft == false"
)
PR_TYPES = {
    filename: ["opened", "synchronize", "reopened", *(
        ["edited"] if filename in {
            "gmsh-meshing-tests.yml", "parametric-atlas-drift.yml",
            "quality-gates-by-domain.yml", "quality-gates.yml",
        } else []
    ), "ready_for_review"]
    for filename in PR_WORKFLOWS
}
AGGREGATES = {
    "quality-gates-by-domain.yml": "domain-tests-required",
    "quality-gates.yml": "quality-gates-aggregate",
}
# Preserve the two pre-existing budgets; new limits require separate evidence.
TIMEOUTS = {
    "full-matrix-sweep.yml": {"sweep-tests": 40},
    "gmsh-meshing-tests.yml": {"focused-tests": 20},
}
RUN_GROUP = (
    "${{ github.workflow }}-${{ github.event_name == 'pull_request' && "
    "format('pr-{0}', github.event.pull_request.number) || format('run-{0}', github.run_id) }}"
)
PR_CANCEL = "${{ github.event_name == 'pull_request' }}"
CONCURRENCY = {
    filename: {"group": RUN_GROUP, "cancel-in-progress": PR_CANCEL}
    for filename in {
        "quality-gates-by-domain.yml", "quality-gates.yml",
        "gmsh-meshing-tests.yml", "parametric-atlas-drift.yml",
    }
}
CONCURRENCY.update({
    "pages.yml": {"group": "pages", "cancel-in-progress": "true"},
    "repo-structure.yml": {
        "group": "${{ github.workflow }}-${{ github.event.pull_request.number || github.ref }}",
        "cancel-in-progress": "true",
    },
    "full-matrix-sweep.yml": {
        "group": "full-matrix-sweep-${{ github.ref }}-${{ github.run_id }}",
        "cancel-in-progress": "false",
    },
})


class UniqueLoader(yaml.BaseLoader):
    """Keep Actions scalar spelling while rejecting repeated mapping keys."""

    def construct_mapping(self, node, deep=False):
        keys = [self.construct_object(key, deep=deep) for key, _ in node.value]
        if len(keys) != len(set(keys)):
            raise ValueError("duplicate YAML key")
        return super().construct_mapping(node, deep=deep)


def read_workflows():
    return {
        path.name: yaml.load(path.read_text(encoding="utf-8"), Loader=UniqueLoader)
        for path in sorted(WORKFLOWS.iterdir()) if path.suffix in {".yml", ".yaml"}
    }


def assert_policy(workflows):
    """Assert complete expressions so an added OR cannot defeat a draft guard."""
    assert set(workflows) == set(SNAPSHOT)
    for filename, workflow in workflows.items():
        jobs = workflow["jobs"]
        assert {key: job.get("name", key) for key, job in jobs.items()} == SNAPSHOT[filename]
        for job_id, job in jobs.items():
            timeout = TIMEOUTS.get(filename, {}).get(job_id)
            assert job.get("timeout-minutes") == (str(timeout) if timeout else None)
        events = workflow["on"]
        assert isinstance(events, dict), "explicit event mappings are required"
        assert "pull_request_target" not in events
        assert ("pull_request" in events) == (filename in PR_WORKFLOWS)
        assert "workflow_dispatch" in events
        if filename in PR_WORKFLOWS:
            assert events["pull_request"]["types"] == PR_TYPES[filename]
            for job_id, job in jobs.items():
                if job_id == AGGREGATES.get(filename):
                    assert job.get("if") == "always()"
                    assert job.get("continue-on-error", "false") == "false"
                elif job_id == "domain-tests":
                    assert job.get("if") == (
                        f"({DRAFT_GUARD}) && (needs.detect-domains.outputs.has-domains == 'true')"
                    )
                else:
                    assert job.get("if") == DRAFT_GUARD
        assert workflow.get("concurrency") == CONCURRENCY.get(filename)

    sweep = workflows["full-matrix-sweep.yml"]
    assert sweep["on"] == {
        "push": {"branches": ["main"]},
        "schedule": [{"cron": "17 7 * * *"}], "workflow_dispatch": "",
    }
    assert workflows["workflow-automation-tests.yml"]["on"]["push"].get("branches") == [
        "main", "develop",
    ]
    builder = next(step for step in sweep["jobs"]["full-matrix"]["steps"]
                   if step.get("id") == "matrix")
    assert "--mode full" in builder["run"] and "--mode touched" not in builder["run"]
    assert "if" not in sweep["jobs"]["full-matrix"]
    assert "if" not in sweep["jobs"]["sweep-tests"]
    assert sweep["jobs"]["sweep-result"]["if"] == "always()"
    domains = workflows["quality-gates-by-domain.yml"]
    detect = next(step for step in domains["jobs"]["detect-domains"]["steps"]
                  if step.get("id") == "detect")
    assert detect["env"]["PR_BASE_SHA"] == "${{ steps.build_merge.outputs.tip }}"
    assert detect["env"]["PUSH_BEFORE_SHA"] == "${{ github.event.before }}"
    harness = "\n".join(step.get("run", "") for step in domains["jobs"]["ci-harness-tests"]["steps"])
    assert "tests/workflows/test_ci_draft_policy.py" in harness
    register = next(step for step in workflows["quality-gates.yml"]["jobs"]["quality-gates"]["steps"]
                    if step.get("name") == "Check engineering register changes")
    assert register["env"]["REGISTER_BASE"] == (
        "${{ github.event_name == 'pull_request' && steps.build_merge.outputs.tip || "
        "github.event.before }}"
    )
    assert register["env"]["EVENT_NAME"] == "${{ github.event_name }}"
    assert 'REGISTER_BASE=$(git merge-base origin/main HEAD)' in register["run"]
    assert 'python scripts/enforcement/check-engineering-register.py --diff "$REGISTER_BASE"' in register["run"]
    assert "--max-age-hours 24" in "\n".join(
        step.get("run", "") for step in domains["jobs"]["domain-tests-required"]["steps"]
    )


def test_workflow_policy():
    assert_policy(read_workflows())


@pytest.mark.parametrize("filename", sorted(SNAPSHOT))
@pytest.mark.parametrize("position", ["top", "job", "step"])
def test_duplicate_key_negative_controls(filename, position):
    """Mutate raw YAML at three depths; parsing must reject identical keys too."""
    text = (WORKFLOWS / filename).read_text()
    if position == "top":
        text += "\nname: duplicate\n"
    elif position == "job":
        job_id = next(iter(SNAPSHOT[filename]))
        text = text.replace(f"  {job_id}:\n", f"  {job_id}:\n    runs-on: duplicate\n", 1)
    else:
        text = text.replace("    steps:\n", "    steps: []\n    steps:\n", 1)
    assert text != (WORKFLOWS / filename).read_text()
    with pytest.raises(ValueError, match="duplicate YAML key"):
        yaml.load(text, Loader=UniqueLoader)


JOB_CASES = [(filename, job_id) for filename, jobs in SNAPSHOT.items() for job_id in jobs]


@pytest.mark.parametrize("filename,job_id", JOB_CASES)
@pytest.mark.parametrize("mutation", ["timeout", "name", "remove-job"])
def test_job_inventory_negative_controls(filename, job_id, mutation):
    workflows = read_workflows()
    assert_policy(workflows)
    if mutation == "remove-job":
        del workflows[filename]["jobs"][job_id]
    elif mutation == "name":
        workflows[filename]["jobs"][job_id]["name"] = "renamed"
    else:
        job = workflows[filename]["jobs"][job_id]
        if "timeout-minutes" in job:
            del job["timeout-minutes"]
        else:
            job["timeout-minutes"] = "15"
    with pytest.raises(AssertionError):
        assert_policy(workflows)


@pytest.mark.parametrize("filename", sorted(PR_WORKFLOWS))
@pytest.mark.parametrize("mutation", ["ready-event", "edited-event", "dispatch"])
def test_draft_policy_negative_controls(filename, mutation):
    workflows = read_workflows()
    assert_policy(workflows)
    workflow = workflows[filename]
    if mutation.endswith("event"):
        event = "ready_for_review" if mutation == "ready-event" else "edited"
        types = workflow["on"]["pull_request"]["types"]
        if event in types:
            types.remove(event)
        else:
            types.append(event)
    elif mutation == "dispatch":
        del workflow["on"]["workflow_dispatch"]
    with pytest.raises(AssertionError):
        assert_policy(workflows)


@pytest.mark.parametrize("filename,job_id", [
    (filename, job_id) for filename, job_id in JOB_CASES
    if filename in PR_WORKFLOWS and job_id != AGGREGATES.get(filename)
])
@pytest.mark.parametrize("mutation", ["missing", "permissive-or"])
def test_every_draft_guard_negative_controls(filename, job_id, mutation):
    workflows = read_workflows()
    assert_policy(workflows)
    job = workflows[filename]["jobs"][job_id]
    if mutation == "missing":
        del job["if"]
    else:
        job["if"] += " || always()"
    with pytest.raises(AssertionError):
        assert_policy(workflows)


@pytest.mark.parametrize("filename", sorted(AGGREGATES))
@pytest.mark.parametrize("mutation", ["aggregate-guard", "push-cancel", "push-group"])
def test_required_gate_negative_controls(filename, mutation):
    workflows = read_workflows()
    assert_policy(workflows)
    workflow = workflows[filename]
    if mutation == "aggregate-guard":
        workflow["jobs"][AGGREGATES[filename]]["if"] = f"({DRAFT_GUARD}) && always()"
    elif mutation == "push-cancel":
        workflow["concurrency"]["cancel-in-progress"] = "true"
    else:
        workflow["concurrency"]["group"] = "${{ github.workflow }}-${{ github.ref }}"
    with pytest.raises(AssertionError):
        assert_policy(workflows)


@pytest.mark.parametrize("filename,group", [("pages.yml", "unserialized"), ("publish.yml", "per-run")])
def test_deployment_group_negative_controls(filename, group):
    workflows = read_workflows()
    assert_policy(workflows)
    workflows[filename].setdefault("concurrency", {})["group"] = group
    with pytest.raises(AssertionError):
        assert_policy(workflows)


@pytest.mark.parametrize("mutation", [
    "sweep-push", "sweep-mode", "sweep-group", "sweep-cancel", "repo-group",
    "stale-pr-base", "missing-manual-base", "remove-workflow", "target-event", "list-events",
    "automation-branch-push", "baseline-age",
])
def test_routing_negative_controls(mutation):
    workflows = read_workflows()
    assert_policy(workflows)
    sweep = workflows["full-matrix-sweep.yml"]
    if mutation == "sweep-push":
        del sweep["on"]["push"]
    elif mutation == "sweep-mode":
        step = next(step for step in sweep["jobs"]["full-matrix"]["steps"] if step.get("id") == "matrix")
        step["run"] = step["run"].replace("--mode full", "--mode touched")
    elif mutation == "sweep-group":
        sweep["concurrency"]["group"] = "full-matrix-sweep-${{ github.ref }}"
    elif mutation == "sweep-cancel":
        sweep["concurrency"]["cancel-in-progress"] = "true"
    elif mutation == "repo-group":
        workflows["repo-structure.yml"]["concurrency"]["group"] = "changed"
    elif mutation == "stale-pr-base":
        step = next(step for step in workflows["quality-gates-by-domain.yml"]["jobs"]["detect-domains"]["steps"]
                    if step.get("id") == "detect")
        step["env"]["PR_BASE_SHA"] = "${{ github.event.pull_request.base.sha }}"
    elif mutation == "missing-manual-base":
        step = next(step for step in workflows["quality-gates.yml"]["jobs"]["quality-gates"]["steps"]
                    if step.get("name") == "Check engineering register changes")
        step["run"] = step["run"].replace("git merge-base origin/main HEAD", "echo origin/main")
    elif mutation == "automation-branch-push":
        del workflows["workflow-automation-tests.yml"]["on"]["push"]["branches"]
    elif mutation == "baseline-age":
        job = workflows["quality-gates-by-domain.yml"]["jobs"]["domain-tests-required"]
        for step in job["steps"]:
            if "run" in step:
                step["run"] = step["run"].replace("--max-age-hours 24", "--max-age-hours 30")
    elif mutation == "remove-workflow":
        del workflows["docs.yml"]
    elif mutation == "target-event":
        workflows["docs.yml"]["on"]["pull_request_target"] = {}
    else:
        workflows["docs.yml"]["on"] = ["pull_request"]
    with pytest.raises(AssertionError):
        assert_policy(workflows)


@pytest.mark.parametrize("filename", sorted(AGGREGATES))
@pytest.mark.parametrize("result,domains,expected", [
    ("skipped", "", False),  # Draft: all heavy dependencies skipped.
    ("cancelled", "true", False), ("failure", "true", False),
    ("success", "true", True), ("success", "false", True),
])
def test_required_aggregate_executes_and_fails_closed(filename, result, domains, expected):
    workflow = read_workflows()[filename]
    job = workflow["jobs"][AGGREGATES[filename]]
    assert job["if"] == "always()"
    step_name = "Verify matrix result" if filename == "quality-gates.yml" else "Enforce required domain result"
    enforce = next(step for step in job["steps"] if step.get("name") == step_name)
    assert enforce.get("if", "always()") == "always()"
    assert enforce.get("continue-on-error", "false") == "false"
    environment = dict(os.environ, QUALITY_GATES_RESULT=result, DETECT_RESULT=result,
                       HARNESS_RESULT=result, DOMAIN_RESULT=result, HAS_DOMAINS=domains,
                       GITHUB_EVENT_NAME="push", CI_BASELINE_MODE="report")
    completed = subprocess.run(["bash", "-e", "-o", "pipefail", "-c", enforce["run"]],
                               env=environment, capture_output=True, text=True, timeout=10)
    assert (completed.returncode == 0) is expected


@pytest.mark.parametrize("result_key", ["DETECT_RESULT", "HARNESS_RESULT", "DOMAIN_RESULT"])
@pytest.mark.parametrize("result", ["skipped", "failure", "cancelled"])
def test_each_domain_dependency_is_required(result_key, result):
    job = read_workflows()["quality-gates-by-domain.yml"]["jobs"]["domain-tests-required"]
    enforce = next(step for step in job["steps"] if step.get("name") == "Enforce required domain result")
    environment = dict(os.environ, DETECT_RESULT="success", HARNESS_RESULT="success",
                       DOMAIN_RESULT="success", HAS_DOMAINS="true", GITHUB_EVENT_NAME="push")
    environment[result_key] = result
    completed = subprocess.run(["bash", "-e", "-o", "pipefail", "-c", enforce["run"]],
                               env=environment, capture_output=True, text=True, timeout=10)
    assert completed.returncode != 0


@pytest.mark.parametrize("event", ["pull_request", "push", "workflow_dispatch"])
@pytest.mark.parametrize("domains", [[], [{"domain": "example", "runner": "ubuntu-latest"}]])
def test_detector_routes_actual_shell(event, domains, tmp_path):
    """Replace the command boundary; execute the checked-in routing shell."""
    job = read_workflows()["quality-gates-by-domain.yml"]["jobs"]["detect-domains"]
    step = next(step for step in job["steps"] if step.get("id") == "detect")
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
''')
    uv.chmod(0o755)
    output, arguments = tmp_path / "output", tmp_path / "arguments.json"
    matrix = json.dumps({"include": domains}, separators=(",", ":"))
    environment = dict(os.environ, PATH=str(tmp_path) + os.pathsep + os.environ["PATH"],
                       EVENT_NAME=event, PR_BASE_SHA="merge-tip", PR_HEAD_SHA="pr-head",
                       PUSH_BEFORE_SHA="push-before", PUSH_AFTER_SHA="push-after",
                       DETECTOR_ARGS=str(arguments), DETECTOR_MATRIX=matrix, GITHUB_OUTPUT=str(output))
    subprocess.run(["bash", "-e", "-o", "pipefail", "-c", step["run"]],
                   env=environment, check=True, capture_output=True, text=True, timeout=10)
    expected = ["--mode", "full"]
    if event == "pull_request":
        expected = ["--mode", "touched", "--base", "merge-tip", "--head", "pr-head"]
    elif event == "push":
        expected = ["--mode", "touched", "--base", "push-before", "--head", "push-after",
                    "--on-missing-base", "full"]
    expected += ["--domains-file", "tests/DOMAINS.md", "--output-format", "json-matrix"]
    assert json.loads(arguments.read_text()) == expected
    assert output.read_text().splitlines() == [
        f"matrix={matrix}", f"has_domains={str(bool(domains)).lower()}",
    ]
