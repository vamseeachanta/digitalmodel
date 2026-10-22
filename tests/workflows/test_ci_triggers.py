# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Regression checks for gate triggers on stacked pull requests."""

from pathlib import Path
from copy import deepcopy
import shlex
import shutil
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


class UniqueKeyLoader(yaml.SafeLoader):
    """Reject duplicate workflow keys instead of silently keeping the last."""


def unique_mapping(loader, node, deep=False):
    """Check root and nested mapping entries before constructing a dictionary."""
    result = {}
    for key, value in loader.construct_pairs(node, deep=deep):
        if key in result:
            raise yaml.constructor.ConstructorError(
                "while constructing a mapping", node.start_mark,
                f"duplicate key: {key}", node.start_mark,
            )
        result[key] = value
    return result


UniqueKeyLoader.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, unique_mapping)


def load_workflow(filename: str) -> dict:
    """Read a workflow without changing YAML 1.1 boolean-key handling."""
    return yaml.load((WORKFLOWS / filename).read_text(encoding="utf-8"), Loader=UniqueKeyLoader)


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
        "github.event.pull_request.number": number if event == "pull_request" else None,
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
    if isinstance(cancel, bool):
        return group, cancel
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
    suffix = "1" if filename == "repo-structure.yml" else "refs/pull/1/merge"
    assert first[0] == f"{load_workflow(filename)['name']}-{suffix}"
    assert first[1] is True
    assert other[1] is True


@pytest.mark.parametrize("filename", [
    filename for filename, events in WORKFLOW_EVENTS.items() if "push" in events
])
def test_push_updates_cancel_only_the_same_ref(filename: str) -> None:
    """Incremental gates retain every push; read-only checks may supersede runs."""
    first = concurrency_result(filename, "push", 101)
    update = concurrency_result(filename, "push", 102)
    other = concurrency_result(filename, "push", 103, ref="refs/heads/develop")
    name = load_workflow(filename)["name"]
    if filename in {"quality-gates-by-domain.yml", "quality-gates.yml"}:
        assert first == (f"{name}-refs/heads/main-101", False)
        assert update == (f"{name}-refs/heads/main-102", False)
        assert other == (f"{name}-refs/heads/develop-103", False)
    elif filename == "pages.yml":
        assert first == update == other == ("pages", False)
    else:
        assert first == update == (f"{name}-refs/heads/main", True)
        assert other == (f"{name}-refs/heads/develop", True)


@pytest.mark.parametrize("filename,event", [
    (filename, event)
    for filename, events in WORKFLOW_EVENTS.items()
    if filename not in {"full-matrix-sweep.yml", "pages.yml", "publish.yml", "repo-structure.yml"}
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


@pytest.mark.parametrize("filename,group", [("pages.yml", "pages"), ("publish.yml", "publish")])
def test_deployments_are_serialized(filename: str, group: str) -> None:
    """Manual and push deployments share one uncancelled group across refs."""
    for event in WORKFLOW_EVENTS[filename]:
        for run_id, ref in [(101, "refs/heads/main"), (102, "refs/heads/other")]:
            assert concurrency_result(filename, event, run_id, ref=ref) == (group, False)


@pytest.mark.parametrize("event", ["push", "pull_request", "workflow_dispatch"])
def test_repository_structure_keeps_existing_group(event: str) -> None:
    """The read-only structure contract retains its shared ref or PR group."""
    suffix = "1" if event == "pull_request" else "refs/heads/main"
    expected = (f"Repository Structure-{suffix}", True)
    assert concurrency_result("repo-structure.yml", event, 101) == expected
    assert concurrency_result("repo-structure.yml", event, 102) == expected


@pytest.mark.parametrize("filename,job_id,step_name", [
    ("quality-gates-by-domain.yml", "domain-tests-required", "Enforce required domain result"),
    ("quality-gates.yml", "quality-gates-aggregate", "Verify matrix result"),
])
def test_required_aggregate_fails_on_draft_skips(
    filename: str, job_id: str, step_name: str, tmp_path: Path,
) -> None:
    """A draft's skipped workers must yield a failing named required check."""
    job = load_workflow(filename)["jobs"][job_id]
    assert job["if"] == "always()"
    step = next(step for step in job["steps"] if step["name"] == step_name)
    environment = dict(os.environ, GITHUB_EVENT_NAME="pull_request",
                       DETECT_RESULT="skipped", HARNESS_RESULT="skipped",
                       DOMAIN_RESULT="skipped", QUALITY_GATES_RESULT="skipped",
                       HAS_DOMAINS="", GITHUB_STEP_SUMMARY=str(tmp_path / "summary"))
    result = subprocess.run(["bash", "-e", "-o", "pipefail", "-c", step["run"]],
                            cwd=tmp_path, env=environment, capture_output=True, text=True)
    assert result.returncode == 1, result.stdout + result.stderr


@pytest.mark.parametrize("document", [
    "concurrency: {group: first}\nconcurrency: {group: second}\n",
    "jobs:\n  detect:\n    env:\n      BASE: first\n      BASE: second\n",
])
def test_workflow_loader_rejects_duplicate_keys(document: str, tmp_path: Path, monkeypatch) -> None:
    """Raw duplicate-key controls must fail before a mapping can discard evidence."""
    (tmp_path / "duplicate.yml").write_text(document, encoding="utf-8")
    monkeypatch.setattr(sys.modules[__name__], "WORKFLOWS", tmp_path)
    with pytest.raises(yaml.constructor.ConstructorError, match="duplicate"):
        load_workflow("duplicate.yml")


@pytest.mark.parametrize("path", sorted(WORKFLOWS.glob("*.yml")), ids=lambda path: path.name)
def test_workflows_have_unique_mapping_keys(path: Path) -> None:
    """Every workflow is parsed with duplicate-key rejection enabled."""
    load_workflow(path.name)


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
        "PR_BASE_SHA": "${{ steps.build_merge.outputs.tip }}",
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


@pytest.mark.parametrize("filename,policy,args", [
    ("quality-gates-by-domain.yml", test_push_updates_cancel_only_the_same_ref, ("quality-gates-by-domain.yml",)),
    ("quality-gates.yml", test_push_updates_cancel_only_the_same_ref, ("quality-gates.yml",)),
    ("pages.yml", test_deployments_are_serialized, ("pages.yml", "pages")),
    ("publish.yml", test_deployments_are_serialized, ("publish.yml", "publish")),
    ("repo-structure.yml", test_repository_structure_keeps_existing_group, ("workflow_dispatch",)),
    ("docs.yml", test_pull_request_concurrency, ("docs.yml",)),
    ("docs.yml", test_scheduled_and_manual_runs_are_never_cancelled, ("docs.yml", "workflow_dispatch")),
])
@pytest.mark.parametrize("field", ["group", "cancel-in-progress"])
def test_concurrency_mutations_are_rejected(filename, policy, args, field, monkeypatch) -> None:
    """Negative controls exercise the same concurrency assertions used in CI."""
    workflow = deepcopy(load_workflow(filename))
    concurrency = workflow["concurrency"]
    concurrency[field] = "shared" if field == "group" else (
        False if filename in {"repo-structure.yml", "docs.yml"} and len(args) == 1 else True
    )
    monkeypatch.setattr(sys.modules[__name__], "load_workflow", lambda name: workflow)
    with pytest.raises(AssertionError):
        policy(*args)


@pytest.mark.parametrize("filename,job_id,step_name", [
    ("quality-gates-by-domain.yml", "domain-tests-required", "Enforce required domain result"),
    ("quality-gates.yml", "quality-gates-aggregate", "Verify matrix result"),
])
def test_guarded_aggregate_mutations_are_rejected(filename, job_id, step_name, tmp_path, monkeypatch) -> None:
    """Restoring a draft guard must be detected before skipped checks can pass."""
    workflow = deepcopy(load_workflow(filename))
    workflow["jobs"][job_id]["if"] = "github.event.pull_request.draft == false && always()"
    monkeypatch.setattr(sys.modules[__name__], "load_workflow", lambda name: workflow)
    with pytest.raises(AssertionError):
        test_required_aggregate_fails_on_draft_skips(filename, job_id, step_name, tmp_path)


def test_full_sweep_trigger_mutation_is_rejected(monkeypatch) -> None:
    """A restored push trigger must not duplicate incremental domain checks."""
    workflow = deepcopy(load_workflow("full-matrix-sweep.yml"))
    triggers = workflow.get("on", workflow.get(True))
    triggers["push"] = {"branches": ["main"]}
    monkeypatch.setattr(sys.modules[__name__], "load_workflow", lambda name: workflow)
    with pytest.raises(AssertionError):
        test_full_sweep_keeps_nightly_schedule()


def test_full_sweep_mode_mutation_is_rejected(monkeypatch) -> None:
    """A partial nightly matrix must fail the full-sweep policy."""
    workflow = deepcopy(load_workflow("full-matrix-sweep.yml"))
    step = next(step for step in workflow["jobs"]["full-matrix"]["steps"] if step.get("id") == "matrix")
    step["run"] = step["run"].replace("--mode full", "--mode touched")
    monkeypatch.setattr(sys.modules[__name__], "load_workflow", lambda name: workflow)
    with pytest.raises(AssertionError):
        test_full_sweep_still_builds_every_shard()


def fixture_email() -> str:
    """Build a synthetic identity for disposable repositories only."""
    return "@".join(("test", "example.invalid"))


def repository_checkouts(filename: str) -> list[tuple[dict, int]]:
    """Locate repository checkouts, including heads held in forks."""
    return [
        (job, index)
        for job in load_workflow(filename)["jobs"].values()
        for index, step in enumerate(job.get("steps", []))
        if step.get("uses", "").startswith("actions/checkout@")
        and ("repository" not in step.get("with", {})
             or "head.repo" in step["with"]["repository"])
    ]


@pytest.mark.parametrize("filename", GATES)
def test_repository_checkouts_verify_current_merge(filename: str) -> None:
    """Every checkout builds and guards a merge of the live tip and head."""
    scripts = []
    for job, index in repository_checkouts(filename):
        checkout, merge = job["steps"][index:index + 2]
        assert "if" not in checkout
        assert checkout["with"]["ref"] == "${{ github.event.pull_request.head.sha || github.sha }}"
        assert checkout["with"]["repository"] == "${{ github.event.pull_request.head.repo.full_name || github.repository }}"
        assert merge["id"] == "build_merge"
        assert merge["if"] == "github.event_name == 'pull_request'"
        assert merge["env"] == {
            "BASE_REF": "${{ github.event.pull_request.base.ref }}",
            "BASE_REPOSITORY_URL": "${{ github.event.pull_request.base.repo.clone_url }}",
            "HEAD_REPOSITORY_URL": "${{ github.event.pull_request.head.repo.clone_url }}",
            "PR_HEAD_SHA": "${{ github.event.pull_request.head.sha }}",
            "EVENT_BASE_SHA": "${{ github.event.pull_request.base.sha }}",
        }
        script = merge["run"]
        scripts.append(script)
        assert "${{" not in script
        assert "git merge --no-ff --no-edit" in script
        assert "git cat-file -p HEAD" in script
        assert "git rev-parse 'HEAD^{tree}'" in script
        assert "event_base=" in script
        assert '"${#parents[@]}" -ne 2' in script
        assert '"${parents[0]:-missing}" != "$tip"' in script
        assert '"${parents[1]:-missing}" != "$PR_HEAD_SHA"' in script
    assert scripts
    assert len(set(scripts)) == 1
    workflow = (WORKFLOWS / filename).read_text()
    assert "merge_commit_sha" not in workflow
    assert "resolve_merge" not in workflow
    assert not re.search(r"gh\s+api", "\n".join(scripts))
    for script in scripts:
        assert all("EVENT_BASE_SHA" not in line for line in script.splitlines()
                   if re.search(r"^\s*(?:if\b|elif\b|test\b|\[)", line))


@pytest.mark.parametrize("filename", GATES)
def test_merge_fetches_disable_automatic_maintenance(filename: str) -> None:
    """Prevent detached repacking from rewriting shallow boundaries during fetch."""
    for job, index in repository_checkouts(filename):
        script = job["steps"][index + 1]["run"]
        first_fetch = script.index("git fetch ")
        for setting in ("gc.auto 0", "maintenance.auto false"):
            assert script.index(f"git config {setting}") < first_fetch


def git(directory: Path, *args: str) -> str:
    """Run fixture Git without inheriting bindings to the caller checkout."""
    env = {key: value for key, value in os.environ.items()
           if not key.startswith("GIT_")}
    result = subprocess.run(["git", "-C", str(directory), *args], env=env,
                            capture_output=True, text=True, check=True)
    return result.stdout.strip()


def fixture_commit(directory: Path, filename: str, content: str) -> str:
    """Commit a text change in the disposable fixture only."""
    (directory / filename).write_text(content, encoding="utf-8")
    git(directory, "add", filename)
    git(directory, "commit", "-m", "Update fixture content")
    return git(directory, "rev-parse", "HEAD")


def advance_base(origin: Path, count: int) -> str:
    """Add base-only history in one Git import, in the disposable bare repository."""
    records = []
    parent = git(origin, "rev-parse", "main")
    for index in range(count):
        content = f"base {index}\n"
        records.append(
            "commit refs/heads/main\n"
            f"committer Test <{fixture_email()}> {1700000000 + index} +0000\n"
            "data 12\nAdvance base\n"
            + (f"from {parent}\n" if index == 0 else "")
            +
            f"M 100644 inline base\ndata {len(content)}\n{content}\n"
        )
    env = {key: value for key, value in os.environ.items()
           if not key.startswith("GIT_")}
    subprocess.run(["git", "-C", str(origin), "fast-import", "--quiet"],
                   input="".join(records), text=True, env=env, capture_output=True,
                   check=True)
    return git(origin, "rev-parse", "main")


def merge_fixture(root: Path, case: str) -> tuple[Path, Path, Path, str, str, str]:
    """Create independent base and head histories for a shallow runner clone."""
    source = root / "source"
    source.mkdir()
    git(source, "init", "-b", "main")
    # These identities belong only to disposable test repositories.
    git(source, "config", "user.name", "Test")
    git(source, "config", "user.email", fixture_email())
    event_base = fixture_commit(source, "shared", "initial\n")
    git(source, "checkout", "-b", "head")
    fixture_commit(source, "shared" if case == "conflict" else "head", "head\n")
    origin = root / "origin.git"
    git(root, "clone", "--bare", "--single-branch", "--branch=main", str(source), str(origin))
    count = {"shallow": 180, "fork-shallow": 180, "full-history": 1600}.get(case, 1)
    tip = advance_base(origin, count)
    if case == "conflict":
        git(source, "checkout", "main")
        git(source, "fetch", str(origin), "main")
        git(source, "merge", "--ff-only", "FETCH_HEAD")
        tip = fixture_commit(source, "shared", "base conflict\n")
        git(source, "push", str(origin), "main")
        git(source, "checkout", "head")
    if case == "descendant":
        git(source, "fetch", str(origin), "main")
        git(source, "merge", "--no-ff", "--no-edit", "FETCH_HEAD")
    head = git(source, "rev-parse", "HEAD")
    head_origin = origin
    if case in ("fork-shallow", "full-history"):
        head_origin = root / "fork.git"
        git(root, "clone", "--bare", "--single-branch", "--branch=head", str(source), str(head_origin))
    else:
        git(source, "push", str(origin), "head")
    runner = root / "runner"
    git(root, "clone", "--depth=1", "--branch=head", head_origin.as_uri(), str(runner))
    git(runner, "checkout", "--detach", head)
    assert git(runner, "rev-parse", "--is-shallow-repository") == "true"
    probe = subprocess.run(["git", "-C", str(runner), "merge-base", tip, head],
                           capture_output=True, text=True,
                           env={key: value for key, value in os.environ.items()
                                if not key.startswith("GIT_")})
    assert probe.returncode != 0
    return runner, origin, head_origin, event_base, tip, head


@pytest.mark.parametrize("filename", GATES)
@pytest.mark.parametrize("case", (
    "stale-event", "conflict", "shallow", "fork-shallow", "full-history",
    "descendant", "no-api", "missing-parent", "wrong-base-parent",
    "wrong-head-parent", "extra-parent",
))
def test_local_merge_behavior(filename: str, case: str, tmp_path: Path) -> None:
    """Run the shipped script against real shallow repositories and conflicts."""
    runner, origin, head_origin, event_base, tip, head = merge_fixture(tmp_path, case)
    job, index = repository_checkouts(filename)[0]
    merge = job["steps"][index + 1]
    assert merge.get("id") == "build_merge"
    script = tmp_path / "merge.sh"
    script.write_text(merge["run"], encoding="utf-8")
    # Only Git and the text tools used by the script are available, never API clients.
    tools = tmp_path / "bin"
    tools.mkdir()
    for command in ("git", "sed", "tr"):
        executable = shutil.which(command)
        assert executable is not None
        (tools / command).symlink_to(executable)
    env = {key: value for key, value in os.environ.items()
           if not key.startswith("GIT_")}
    output = tmp_path / "output"
    env.update(PATH=str(tools), BASE_REF="main", PR_HEAD_SHA=head,
               BASE_REPOSITORY_URL=origin.as_uri(), HEAD_REPOSITORY_URL=head_origin.as_uri(),
               EVENT_BASE_SHA=event_base, GITHUB_OUTPUT=str(output))
    bash = shutil.which("bash")
    assert bash is not None
    invalid_parents = {
        "missing-parent": [tip],
        "wrong-base-parent": [event_base, head],
        "wrong-head-parent": [tip, event_base],
        "extra-parent": [tip, head, event_base],
    }.get(case)
    if invalid_parents is not None:
        # Replace only the merge result with a real malformed commit. All fetches
        # and the shipped parent guard still execute against real Git objects.
        real_git = shutil.which("git")
        assert real_git is not None
        quoted_git = shlex.quote(real_git)
        parent_args = " ".join(f"-p {parent}" for parent in invalid_parents)
        wrapper = tools / "git"
        wrapper.unlink()
        wrapper.write_text(
            f"#!{bash}\nset -euo pipefail\n"
            f'{quoted_git} "$@"\n'
            'if [ "${1:-}" = merge ] && [ "${2:-}" = --no-ff ]; then\n'
            f"  tree=$({quoted_git} rev-parse 'HEAD^{{tree}}')\n"
            f'  malformed=$({quoted_git} commit-tree "$tree" {parent_args} '
            '<<< "Malformed merge fixture")\n'
            f'  {quoted_git} reset --hard "$malformed" >/dev/null\n'
            'fi\n',
            encoding="utf-8",
        )
        wrapper.chmod(0o755)
    result = subprocess.run([bash, "-x", str(script)], cwd=runner, env=env,
                            capture_output=True, text=True, timeout=60)
    if case == "full-history":
        assert "--unshallow" in result.stderr, result.stderr
    if case in ("shallow", "fork-shallow"):
        assert "--deepen=" in result.stderr
    if invalid_parents is not None:
        assert result.returncode != 0, (result.stdout, result.stderr)
        assert "::error::Local merge must have exactly" in result.stdout
        assert "tested=" not in result.stdout
        assert not output.exists()
        assert git(runner, "status", "--porcelain") == ""
        assert git(runner, "show", "-s", "--format=%P", "HEAD").split() == invalid_parents
    elif case == "conflict":
        assert result.returncode != 0
        assert "::error::" in result.stdout
        assert not output.exists()
        assert git(runner, "status", "--porcelain") == ""
        assert git(runner, "rev-parse", "HEAD") == tip
    else:
        assert result.returncode == 0, (result.stdout, result.stderr)
        assert git(runner, "show", "-s", "--format=%P", "HEAD").split() == [tip, head]
        tested = git(runner, "rev-parse", "HEAD")
        tree = git(runner, "rev-parse", "HEAD^{tree}")
        assert f"tested={tested} first_parent={tip} second_parent={head} tip={tip} event_base={event_base} tree={tree}" in result.stdout
        assert output.read_text().splitlines() == [f"tip={tip}"]
        assert (runner / "head").read_text() == "head\n"
        assert (runner / "base").read_text().startswith("base ")


def test_base_consumers_use_local_tip() -> None:
    """Detection and register checks use the base repository even for forks."""
    workflow = load_workflow("quality-gates-by-domain.yml")
    detect = next(step for step in workflow["jobs"]["detect-domains"]["steps"]
                  if step.get("id") == "detect")
    assert detect["env"]["PR_BASE_SHA"] == "${{ steps.build_merge.outputs.tip }}"
    quality = load_workflow("quality-gates.yml")
    register = next(step for step in quality["jobs"]["quality-gates"]["steps"]
                    if step.get("name") == "Check engineering register changes")
    assert register["env"]["REGISTER_BASE"] == "${{ github.event_name == 'pull_request' && steps.build_merge.outputs.tip || github.event.before }}"




@pytest.mark.parametrize("changed_path,expected", [
    ("README.md", []),
    ("docs/maps/example.md", ["contracts"]),
    ("src/digitalmodel/cathodic_protection/example.py", ["cathodic-protection"]),
])
def test_real_diff_keeps_required_checks_active(changed_path, expected, tmp_path) -> None:
    """Docs and code diffs retain the harness, quality matrix and failing gates."""
    root = WORKFLOWS.parents[1]
    repo = tmp_path / "repository"
    repo.mkdir()
    git(repo, "init", "-b", "main")
    git(repo, "config", "user.name", "Test")
    git(repo, "config", "user.email", fixture_email())
    base = fixture_commit(repo, "initial", "initial\n")
    (repo / changed_path).parent.mkdir(parents=True, exist_ok=True)
    head = fixture_commit(repo, changed_path, "changed\n")
    detected = subprocess.run(
        [sys.executable, str(root / "scripts/ci/detect_touched_domains.py"),
         "--mode", "touched", "--base", base, "--head", head,
         "--domains-file", str(root / "tests/DOMAINS.md"),
         "--output-format", "json-matrix"],
        cwd=repo, capture_output=True, text=True, check=True,
    )
    selected = json.loads(detected.stdout)["include"]
    assert [item["domain"] for item in selected] == expected
    domain = load_workflow("quality-gates-by-domain.yml")
    quality = load_workflow("quality-gates.yml")
    for workflow in (domain, quality):
        triggers = workflow.get("on", workflow.get(True))
        assert "paths" not in triggers["pull_request"]
        assert "paths-ignore" not in triggers["pull_request"]
    for workflow, worker in ((domain, "ci-harness-tests"), (quality, "quality-gates")):
        job = workflow["jobs"][worker]
        assert job["if"] == (
            "github.event_name != 'pull_request' || github.event.pull_request.draft == false"
        )
        assert not job.get("continue-on-error", False)
    assert domain["jobs"]["ci-harness-tests"]["needs"] == "detect-domains"
    assert "has-domains == 'true'" in domain["jobs"]["domain-tests"]["if"]
    assert quality["jobs"]["quality-gates"]["strategy"]["matrix"] == {
        "python-version": ["3.11", "3.12"],
    }
    for workflow, aggregate, worker, variable, step_name in (
        (domain, "domain-tests-required", "ci-harness-tests", "HARNESS_RESULT",
         "Enforce required domain result"),
        (quality, "quality-gates-aggregate", "quality-gates", "QUALITY_GATES_RESULT",
         "Verify matrix result"),
    ):
        job = workflow["jobs"][aggregate]
        assert job["if"] == "always()"
        assert worker in job["needs"]
        step = next(step for step in job["steps"] if step.get("name") == step_name)
        # Use push semantics to avoid the separate, report-only baseline download.
        environment = dict(os.environ, GITHUB_EVENT_NAME="push", DETECT_RESULT="success",
                           HARNESS_RESULT="success", QUALITY_GATES_RESULT="success",
                           HAS_DOMAINS=str(bool(selected)).lower(),
                           DOMAIN_RESULT="success" if selected else "skipped")
        for result in ("success", "failure", "cancelled", "skipped"):
            completed = subprocess.run(
                ["bash", "-e", "-o", "pipefail", "-c", step["run"]], cwd=repo,
                env={**environment, variable: result}, capture_output=True, text=True,
            )
            assert (completed.returncode == 0) == (result == "success")
        if selected and workflow is domain:
            environment["DOMAIN_RESULT"] = "failure"
            completed = subprocess.run(
                ["bash", "-e", "-o", "pipefail", "-c", step["run"]],
                cwd=repo, env=environment, capture_output=True, text=True,
            )
            assert completed.returncode == 1
