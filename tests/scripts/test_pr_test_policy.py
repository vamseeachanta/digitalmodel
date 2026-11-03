# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check the PR and scheduled test policy without contacting GitHub."""

from pathlib import Path
import importlib.util
import json
import os
import subprocess
import sys

import pytest
import yaml


ROOT = Path(__file__).resolve().parents[2]
WORKFLOWS = ROOT / ".github/workflows"


def workflow(name):
    """Parse Actions YAML, including its unquoted on key."""
    return yaml.safe_load((WORKFLOWS / name).read_text(encoding="utf-8"))


def events(document):
    """Return event configuration under either YAML boolean convention."""
    return document.get("on", document.get(True))


def test_pr_cancellation_is_scoped_and_main_is_preserved():
    """Cancel superseded PRs while preserving main runs."""
    for path in WORKFLOWS.glob("*.yml"):
        document = workflow(path.name)
        triggers = events(document)
        if "pull_request" not in triggers:
            continue
        concurrency = document["concurrency"]
        expected_group = (
            "${{ github.workflow }}-${{ github.event_name == 'pull_request' && "
            "format('pr-{0}', github.event.pull_request.number) || format('run-{0}', github.run_id) }}"
            if path.name in {"quality-gates-by-domain.yml", "gmsh-meshing-tests.yml",
                             "workflow-automation-tests.yml"}
            else "${{ github.workflow }}-${{ github.event.pull_request.number || github.ref }}"
        )
        assert concurrency["group"] == expected_group
        expected = "${{ github.event_name == 'pull_request' }}" if "push" in triggers else True
        assert concurrency["cancel-in-progress"] == expected


@pytest.mark.parametrize("name", [
    "quality-gates.yml", "full-matrix-sweep.yml", "docs.yml", "parametric-atlas-drift.yml",
])
def test_whole_repository_suites_do_not_run_on_prs(name):
    """Retain manual access to every displaced full check."""
    triggers = events(workflow(name))
    assert "pull_request" not in triggers
    assert "workflow_dispatch" in triggers
    if name in {"quality-gates.yml", "parametric-atlas-drift.yml"}:
        assert "push" not in triggers


def test_reusable_concurrency_groups_are_distinct():
    """Keep called workflows independent when github.workflow names the caller."""
    groups = [workflow(name)["concurrency"]["group"] for name in (
        "full-matrix-sweep.yml", "quality-gates.yml", "parametric-atlas-drift.yml",
    )]
    assert len(set(groups)) == len(groups)
    assert all("github.workflow" not in group for group in groups)


def test_full_batch_schedule_and_issue_permissions():
    """Run one batch twice daily with serialized issue updates."""
    document = workflow("full-matrix-sweep.yml")
    triggers = events(document)
    assert triggers == {"schedule": [{"cron": "17 */12 * * *"}], "workflow_dispatch": None}
    assert document["concurrency"]["cancel-in-progress"] is False
    jobs = document["jobs"]
    for job, filename in (
        ("full-quality-gates", "quality-gates.yml"),
        ("api-docs", "docs.yml"),
        ("atlas-drift", "parametric-atlas-drift.yml"),
    ):
        assert jobs[job]["uses"] == f"./.github/workflows/{filename}"
        assert "workflow_call" in events(workflow(filename))
        assert job in jobs["sweep-result"]["needs"]
    result = jobs["sweep-result"]
    assert result["if"] == "always()"
    assert result["permissions"]["issues"] == "write"
    assert result["concurrency"] == {
        "group": "scheduled-full-test-health", "cancel-in-progress": False,
    }
    health = next(step for step in result["steps"] if step.get("name") == "Update main health")
    assert "always()" in health["if"]
    assert "continue-on-error" not in health
    assert "Scheduled full test run failing on main" in health["with"]["script"]


def test_bridge_command_keeps_existing_smoke_registration():
    """Retain smoke registration in the executable bridge test command."""
    target = "tests/solvers/smoke/test_ci_registration.py"
    jobs = workflow("gmsh-meshing-tests.yml")["jobs"]
    command = next(step["run"] for job in jobs.values() for step in job["steps"]
                   if step.get("name") == "Run bridge tests")
    assert target in command.split()
    assert (ROOT / target).is_file()


def test_reusable_quality_workflow_has_no_unreachable_pr_steps():
    """Full quality checks retain only steps reachable through their triggers."""
    document = workflow("quality-gates.yml")
    assert set(events(document)) == {"workflow_call", "workflow_dispatch"}
    for job in document["jobs"].values():
        for step in job["steps"]:
            assert "pull_request" not in step.get("if", "")


@pytest.mark.parametrize("filename", ["quality-gates.yml", "parametric-atlas-drift.yml"])
def test_full_validation_callers_cannot_be_triggered_by_prs(filename):
    """Check caller events as well as the reusable workflow's own events."""
    documents = {path.name: workflow(path.name) for path in WORKFLOWS.iterdir()
                 if path.suffix in {".yml", ".yaml"}}
    pending, checked = [filename], set()
    while pending:
        target = pending.pop()
        if target in checked:
            continue
        checked.add(target)
        configured = events(documents[target])
        triggers = {configured} if isinstance(configured, str) else set(configured)
        assert not {"pull_request", "pull_request_target"}.intersection(triggers)
        for name, document in documents.items():
            if any(job.get("uses", "").split("@", 1)[0].endswith(f"/.github/workflows/{target}")
                   for job in document["jobs"].values()):
                pending.append(name)
    assert "full-matrix-sweep.yml" in checked


@pytest.mark.parametrize("trigger", ["pull_request", "[pull_request]", "{pull_request: null}"])
def test_caller_guard_rejects_yaml_pr_callers(trigger, tmp_path, monkeypatch):
    """Accept a manual caller, then reject every Actions event syntax in YAML."""
    monkeypatch.setattr(sys.modules[__name__], "WORKFLOWS", tmp_path)
    (tmp_path / "quality-gates.yml").write_text(
        "on: workflow_call\njobs: {check: {runs-on: ubuntu-latest, steps: []}}\n",
        encoding="utf-8", newline="\n",
    )
    (tmp_path / "full-matrix-sweep.yml").write_text(
        "on: workflow_dispatch\njobs:\n  check:\n"
        "    uses: ./.github/workflows/quality-gates.yml\n",
        encoding="utf-8", newline="\n",
    )
    test_full_validation_callers_cannot_be_triggered_by_prs("quality-gates.yml")
    (tmp_path / "caller.yaml").write_text(
        f"on: {trigger}\njobs:\n  check:\n"
        "    uses: ./.github/workflows/full-matrix-sweep.yml\n",
        encoding="utf-8", newline="\n",
    )
    with pytest.raises(AssertionError):
        test_full_validation_callers_cannot_be_triggered_by_prs("quality-gates.yml")


def test_policy_javascript_runtime_is_provisioned_before_validation():
    """Executable issue-writer regressions require an explicit Node runtime."""
    steps = workflow("full-matrix-sweep.yml")["jobs"]["full-matrix"]["steps"]
    setup = next(index for index, step in enumerate(steps)
                 if step.get("uses", "").startswith("actions/setup-node@"))
    validation = next(index for index, step in enumerate(steps)
                      if step.get("name") == "Validate CI policy")
    assert setup < validation
    assert steps[setup]["with"]["node-version"] == "22"


@pytest.mark.parametrize("labelled", [False, True])
@pytest.mark.parametrize("active", [False, True])
@pytest.mark.parametrize("newer", [False, True])
def test_health_writer_uses_only_labelled_health_issues(labelled, active, newer):
    """An unrelated matching title is preserved through failure and recovery."""
    health = next(step for step in workflow("full-matrix-sweep.yml")["jobs"]
                  ["sweep-result"]["steps"] if step.get("name") == "Update main health")
    harness = r'''
    const input = JSON.parse(require('fs').readFileSync(0, 'utf8'));
    const calls = [], queries = [], methods = [];
    const title = 'Scheduled full test run failing on main';
    const issues = [{number: 1, title, body: '', labels: []},
      {number: 3, title, body: '', labels: ['main-health'], pull_request: {}}];
    if (input.labelled) issues.push({number: 2, title, labels: ['main-health'],
      body: input.newer ? '<!-- main-health-order: run_number=11 run_id=101 sha=abc123 -->' : '',
      state: input.active ? 'closed' : 'open'});
    const github = {
      paginate: async (method, options) => {
        if (method === 'labels') return [{name: 'main-health'}];
        methods.push(method);
        queries.push(options);
        return issues.filter(issue => !options.labels || issue.labels.includes(options.labels));
      },
      rest: {issues: {listForRepo: 'issues', listLabelsForRepo: 'labels',
        update: async value => calls.push({kind: 'update', ...value}),
        create: async value => calls.push({kind: 'create', ...value})}}
    };
    const baseline = {tests: input.active ? {'tests/example.py::test_case':
      {shard: 'example', first_seen_run: 1, run_id: 10}} : {},
      shards: {example: {parsed: true, conclusion: 'success'}}};
    const fs = {existsSync: () => false, readFileSync: () => JSON.stringify(baseline)};
    const context = {repo: {owner: 'example', repo: 'example'},
      runNumber: 10, runId: 100, sha: 'abc123'};
    const AsyncFunction = Object.getPrototypeOf(async function(){}).constructor;
    new AsyncFunction('require', 'github', 'context', 'core', input.script)(
      () => fs, github, context, {notice: () => {}}
    ).then(() => console.log(JSON.stringify({calls, queries, methods})));
    '''
    result = subprocess.run(
        ["node", "-e", harness], input=json.dumps({
            "script": health["with"]["script"], "labelled": labelled,
            "active": active, "newer": newer,
        }), env=dict(os.environ, BATCH_RESULTS="{}", REPORTING_STATUS="success"),
        encoding="utf-8", capture_output=True, check=True, timeout=30,
    )
    output = json.loads(result.stdout)
    assert output["methods"] == ["issues"]
    assert output["queries"][0].get("labels") == "main-health"
    assert output["queries"][0]["state"] == "all"
    if labelled and newer:
        assert output["calls"] == []
    elif labelled:
        assert len(output["calls"]) == 1
        assert output["calls"][0]["kind"] == "update"
        assert output["calls"][0]["issue_number"] == 2
        assert output["calls"][0]["state"] == ("open" if active else "closed")
    elif active:
        assert len(output["calls"]) == 1
        assert output["calls"][0]["kind"] == "create"
        assert output["calls"][0]["labels"] == ["main-health"]
    else:
        assert output["calls"] == []


def test_pages_matches_main_routing_and_cancellation():
    """Preserve main's Pages runner labels and superseded-run cancellation."""
    document = workflow("pages.yml")
    assert document["concurrency"] == {
        "group": "pages", "cancel-in-progress": True,
    }
    assert set(document["jobs"]) == {"build", "deploy"}
    for job in document["jobs"].values():
        assert job["runs-on"] == ["self-hosted", "spark"]


def test_harness_can_be_skipped_only_when_not_touched():
    """Require detection and every selected check in the aggregate."""
    jobs = workflow("quality-gates-by-domain.yml")["jobs"]
    assert jobs["ci-harness-tests"]["if"] == "needs.detect-domains.outputs.has-harness == 'true'"
    aggregate = jobs["domain-tests-required"]
    assert aggregate["name"] == "Domain test aggregate"
    assert aggregate["if"] == "always()"
    assert set(aggregate["needs"]) == {"detect-domains", "ci-harness-tests", "domain-tests"}
    enforcement = next(step for step in aggregate["steps"]
                       if step.get("name") == "Enforce required domain result")
    assert "HAS_HARNESS" in enforcement["env"]
    assert '"$HARNESS_RESULT" != "skipped"' in enforcement["run"]
    assert '"$DOMAIN_RESULT" != "success"' in enforcement["run"]


def load_policy():
    """Import the shipped selector as a standalone CI script."""
    spec = importlib.util.spec_from_file_location("pr_test_policy", ROOT / "scripts/ci/pr_test_policy.py")
    module = importlib.util.module_from_spec(spec)
    sys.path.insert(0, str(ROOT / "scripts/ci"))
    spec.loader.exec_module(module)
    return module


@pytest.mark.parametrize("paths,expected", [
    (["README.md"], False),
    (["src/digitalmodel/citations/registry.py"], False),
    ([".github/workflows/quality-gates-by-domain.yml"], True),
    (["scripts/ci/pr_test_policy.py"], True),
    (["tests/scripts/test_ci_baseline.py"], True),
    (["tests/conftest.py"], True),
    (["pyproject.toml"], True),
    ([".github/ci/flaky-register.yml"], True),
    (["scripts/implementation_map_search.py"], True),
])
def test_harness_selection(paths, expected):
    """Recognize harness inputs without selecting unrelated source changes."""
    assert load_policy().needs_harness(paths) is expected


def test_dry_trace_docs_and_module_dependents():
    """Trace docs and a module with known cross-domain dependents."""
    policy = load_policy()
    docs = policy.trace(["docs/unpinned-note.md"], pinned_domains={})
    assert docs["domains"] == []
    assert docs["harness"] is False
    assert docs["required"] == ["Domain test aggregate", "Repository structure contract"]
    module = policy.trace(["src/digitalmodel/orcaflex/installation_analysis.py"], pinned_domains={})
    assert module["domains"] == ["marine-ops-other", "orcaflex", "workflows"]
    assert module["harness"] is False


def test_trace_includes_record_readers_from_both_revisions(monkeypatch):
    """Keep docs-only changes sensitive to contract pins on either revision."""
    policy = load_policy()
    routes = {"base": {"README.md": {"contracts"}},
              "head": {"README.md": {"workflows"}}}
    monkeypatch.setattr(policy, "git_pinned_domains", lambda ref, domains: routes[ref])
    result = policy.trace(["README.md"], base="base", head="head")
    assert result["domains"] == ["contracts", "workflows"]
    assert result["record_pins_checked"] is True


def test_trace_pin_read_error_selects_every_domain(monkeypatch):
    """Match the production detector's conservative evidence fallback."""
    policy = load_policy()

    def unavailable(ref, domains):
        raise OSError("Record pins unavailable")

    monkeypatch.setattr(policy, "git_pinned_domains", unavailable)
    result = policy.trace(["README.md"])
    assert result["domains"] == [d.name for d in policy.parse_domains(ROOT / "tests/DOMAINS.md")]
    assert result["record_pins_checked"] is False


@pytest.mark.parametrize("selection,result,expected", [
    ("true", "success", 0), ("true", "failure", 1), ("true", "skipped", 1),
    ("true", "cancelled", 1), ("false", "skipped", 0), ("false", "success", 1),
    ("", "skipped", 1),
])
def test_aggregate_enforces_harness_selection(selection, result, expected, tmp_path):
    """Execute the real aggregate shell with successful detection and no domains."""
    aggregate = workflow("quality-gates-by-domain.yml")["jobs"]["domain-tests-required"]
    step = next(step for step in aggregate["steps"]
                if step.get("name") == "Enforce required domain result")
    env = dict(os.environ, HAS_HARNESS=selection, HARNESS_RESULT=result,
               DETECT_RESULT="success", HAS_DOMAINS="false", DOMAIN_RESULT="skipped")
    completed = subprocess.run(["bash", "-e", "-c", step["run"]], cwd=tmp_path,
                               env=env, capture_output=True, encoding="utf-8", check=False)
    assert completed.returncode == expected, completed.stdout + completed.stderr


@pytest.mark.parametrize("path", [
    "scripts/unmapped_example.py", "tests/unmapped_example.py",
    "config/unmapped.yml", ".github/ci/unmapped.json",
    "src/other_package/module.py", "tools/check", "unmapped.toml",
    "tests/unmapped/fixture.md",
])
@pytest.mark.parametrize("mixed", [False, True])
def test_unowned_inputs_select_full_matrix_and_enforce_aggregate(path, mixed, tmp_path):
    """Unowned inputs require every domain, even beside a known module."""
    policy = load_policy()
    paths = [path]
    if mixed:
        paths.append("src/digitalmodel/orcaflex/installation_analysis.py")
    result = policy.trace(paths, pinned_domains={})
    domains = policy.parse_domains(ROOT / "tests/DOMAINS.md")
    assert result["domains"] == [domain.name for domain in domains]
    assert all(f"tests-{domain.name}" in result["jobs"] for domain in domains)
    aggregate = workflow("quality-gates-by-domain.yml")["jobs"]["domain-tests-required"]
    step = next(step for step in aggregate["steps"]
                if step.get("name") == "Enforce required domain result")
    for domain_result, expected in (("success", 0), ("skipped", 1), ("failure", 1)):
        env = dict(os.environ, HAS_HARNESS=str(result["harness"]).lower(),
                   HARNESS_RESULT="success" if result["harness"] else "skipped",
                   DETECT_RESULT="success", HAS_DOMAINS="true", DOMAIN_RESULT=domain_result)
        completed = subprocess.run(["bash", "-e", "-c", step["run"]], cwd=tmp_path,
                                   env=env, capture_output=True, encoding="utf-8", check=False)
        assert completed.returncode == expected, completed.stdout + completed.stderr


@pytest.mark.parametrize("paths", [
    ["README.md"], ["docs/unpinned-note.md"], ["docs/guide.rst"],
    ["README.md", "src/digitalmodel/orcaflex/installation_analysis.py"],
])
def test_unpinned_prose_preserves_narrow_selection(paths):
    """Explicit prose exclusions do not widen known module routes."""
    result = load_policy().trace(paths, pinned_domains={})
    expected = ["marine-ops-other", "orcaflex", "workflows"] if len(paths) > 1 else []
    assert result["domains"] == expected
