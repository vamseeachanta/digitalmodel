# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Regression checks for gate triggers on stacked pull requests."""

from pathlib import Path
import json
import os
import re
import subprocess
import shutil
import shlex
from typing import Any

import pytest
import yaml


WORKFLOWS = Path(__file__).resolve().parents[2] / ".github" / "workflows"
GATES = (
    "quality-gates-by-domain.yml",
    "gmsh-meshing-tests.yml",
)
MERGE_GATES = (*GATES, "quality-gates.yml", "parametric-atlas-drift.yml")


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
    """Evaluate the bounded Actions subset used by concurrency and routing.

    Split at top level so quoted JSON and parenthesized trust guards retain
    operator precedence and short-circuit evaluation.
    """
    expression = expression.strip()
    for operator in (" || ", " && ", " == ", " != "):
        depth = 0
        for match in re.finditer(r"'[^']*'|[()]| \|\| | && | == | != ", expression):
            token = match[0]
            if token == "(":
                depth += 1
            elif token == ")":
                depth -= 1
            elif depth == 0 and token == operator:
                left, right = expression[:match.start()], expression[match.end():]
                value = evaluate_expression(left, context)
                if operator == " || ":
                    return value or evaluate_expression(right, context)
                if operator == " && ":
                    return value and evaluate_expression(right, context)
                other = evaluate_expression(right, context)
                if isinstance(value, str) and isinstance(other, str):
                    value, other = value.casefold(), other.casefold()
                return (value == other) if operator == " == " else (value != other)
    if expression.startswith("(") and expression.endswith(")"):
        return evaluate_expression(expression[1:-1], context)
    decoded = re.fullmatch(r"fromJSON\((.*)\)", expression)
    if decoded:
        return json.loads(evaluate_expression(decoded[1], context))
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
        "github.event.pull_request.number": number if event == "pull_request" else None,
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
    """Push and manual runs never cancel an active run for the same ref."""
    first = concurrency_result(filename, event, 101)
    second = concurrency_result(filename, event, 102)
    assert first == (f"{load_workflow(filename)['name']}-refs/heads/main", False)
    assert second == first


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


@pytest.mark.parametrize("filename", MERGE_GATES)
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
    workflow = (WORKFLOWS / filename).read_text(encoding="utf-8")
    assert "merge_commit_sha" not in workflow
    assert "resolve_merge" not in workflow
    assert not re.search(r"gh\s+api", "\n".join(scripts))
    for script in scripts:
        assert all("EVENT_BASE_SHA" not in line for line in script.splitlines()
                   if re.search(r"^\s*(?:if\b|elif\b|test\b|\[)", line))


@pytest.mark.parametrize("filename", MERGE_GATES)
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


@pytest.mark.parametrize("filename", MERGE_GATES)
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
    assert register["env"]["REGISTER_BASE"] == "${{ github.event.before || 'HEAD^' }}"


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
    assert triggers["schedule"] == [{"cron": "17 */12 * * *"}]
    assert "push" not in triggers
    assert "pull_request" not in triggers
    assert "workflow_dispatch" in triggers


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


@pytest.mark.parametrize("event", ("push", "schedule", "workflow_dispatch"))
def test_full_sweep_separates_nightly_from_pushes(event: str) -> None:
    """Every sweep keeps its own group and cannot cancel another sweep."""
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
    assert groups == [
        "full-matrix-sweep-refs/heads/main-101",
        "full-matrix-sweep-refs/heads/main-102",
    ]
    assert concurrency["cancel-in-progress"] is False


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


@pytest.mark.parametrize("run_id", ("101", "102"))
@pytest.mark.parametrize("event,ref,number,expected", (
    ("pull_request", "refs/pull/1/merge", "1", "Repository Structure-1"),
    ("pull_request", "refs/pull/2/merge", "2", "Repository Structure-2"),
    ("push", "refs/heads/main", None, "Repository Structure-refs/heads/main"),
    ("push", "refs/heads/topic", None, "Repository Structure-refs/heads/topic"),
    ("push", "refs/heads/1", None, "Repository Structure-refs/heads/1"),
    ("workflow_dispatch", "refs/heads/main", None,
     "Repository Structure-refs/heads/main"),
))
def test_repo_structure_cancels_superseded_runs(
    event: str, ref: str, number: str | None, expected: str, run_id: str,
) -> None:
    """Derive groups by hand from synthetic context values, not the evaluator.

    Basis: GitHub Actions workflow syntax, concurrency (unversioned), and
    evaluate-expressions-in-workflows-and-actions, Operators (unversioned):
    https://docs.github.com/en/actions/reference/workflow-syntax-for-github-actions#concurrency
    https://docs.github.com/en/actions/reference/evaluate-expressions-in-workflows-and-actions#operators

    Hand substitution: prefix the workflow name and a hyphen to the PR number
    when present, otherwise to the full ref. Thus PRs 1 and 2 give suffixes
    '1' and '2'; pushes give 'refs/heads/main', 'refs/heads/topic' and
    'refs/heads/1'. A manual main run shares the main push group. Synthetic
    run IDs 101 and 102 never enter the result, so updates supersede work
    only in the same workflow/ref group. The branch named '1' remains distinct
    from PR 1. Only pull request updates cancel earlier runs.
    """
    workflow = load_workflow("repo-structure.yml")
    concurrency = workflow["concurrency"]
    context = {
        "github.workflow": workflow["name"],
        "github.event_name": event,
        "github.ref": ref,
        "github.event.pull_request.number": number if event == "pull_request" else None,
        "github.run_id": run_id,
    }
    group = re.sub(
        r"\$\{\{(.*?)\}\}",
        lambda match: str(evaluate_expression(match[1], context)),
        concurrency["group"],
    )
    assert group == expected
    assert concurrency["cancel-in-progress"] == "${{ github.event_name == 'pull_request' }}"
    cancellation = evaluate_expression(concurrency["cancel-in-progress"][3:-2], context)
    assert cancellation is (event == "pull_request")
