# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Regression checks for gate triggers on stacked pull requests."""

from pathlib import Path
import os
import re
import subprocess
import shutil
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


@pytest.mark.parametrize("filename", GATES)
def test_pull_request_activity_types(filename: str) -> None:
    """Retargets and ordinary edits start the same gates as code updates."""
    workflow = load_workflow(filename)
    triggers = workflow["on"] if "on" in workflow else workflow[True]
    assert triggers["pull_request"]["types"] == [
        "opened", "synchronize", "reopened", "edited"
    ]
    expected_keys = {"types", "paths"} if filename == "gmsh-meshing-tests.yml" else {"types"}
    assert set(triggers["pull_request"]) == expected_keys


@pytest.mark.parametrize("filename", GATES)
def test_edits_cannot_skip_jobs_or_steps(filename: str) -> None:
    """Edit-specific conditions cannot replace failing checks with skips."""
    for job in load_workflow(filename)["jobs"].values():
        for entry in [job, *job.get("steps", [])]:
            condition = str(entry.get("if", ""))
            assert not re.search(r"github\s*\.\s*event\s*\.\s*(action|changes)\b", condition)


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
    filename: str, event: str, run_id: int, number: int = 1
) -> tuple[str, bool]:
    """Render the actual workflow group and cancellation expression."""
    workflow = load_workflow(filename)
    concurrency = workflow["concurrency"]
    context = {
        "github.workflow": workflow["name"],
        "github.event_name": event,
        "github.event.pull_request.number": number,
        "github.ref": "refs/heads/main",
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


@pytest.mark.parametrize("filename", GATES)
def test_pull_request_concurrency(filename: str) -> None:
    """Only updates of the same pull request supersede earlier runs."""
    first = concurrency_result(filename, "pull_request", 101, number=1)
    update = concurrency_result(filename, "pull_request", 102, number=1)
    other = concurrency_result(filename, "pull_request", 103, number=2)
    assert first == update
    assert first[0] != other[0]
    assert first[1] is True
    assert other[1] is True


@pytest.mark.parametrize("filename", GATES)
@pytest.mark.parametrize("event", ("push", "workflow_dispatch"))
def test_non_pr_runs_are_never_cancelled(filename: str, event: str) -> None:
    """Push and manual runs keep separate groups identified by run ID."""
    first = concurrency_result(filename, event, 101)
    second = concurrency_result(filename, event, 102)
    assert first == (f"{load_workflow(filename)['name']}-run-101", False)
    assert second == (f"{load_workflow(filename)['name']}-run-102", False)


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
            f"committer Test <test@example.invalid> {1700000000 + index} +0000\n"
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
    git(source, "config", "user.email", "test@example.invalid")
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
@pytest.mark.parametrize("case", ("stale-event", "conflict", "shallow", "fork-shallow", "full-history", "descendant", "no-api"))
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
    result = subprocess.run([bash, "-x", str(script)], cwd=runner, env=env,
                            capture_output=True, text=True, timeout=60)
    if case == "full-history":
        assert "--unshallow" in result.stderr
    if case in ("shallow", "fork-shallow"):
        assert "--deepen=" in result.stderr
    if case == "conflict":
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


def test_trigger_tests_run_in_harness() -> None:
    """The harness executes trigger regressions on every pull request."""
    workflow = load_workflow("quality-gates-by-domain.yml")
    commands = "\n".join(
        step.get("run", "") for step in workflow["jobs"]["ci-harness-tests"]["steps"]
    )
    assert "tests/workflows/test_ci_triggers.py" in commands


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
