# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check the PR and scheduled test policy without contacting GitHub."""

from pathlib import Path
import importlib.util
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
        assert concurrency["group"] == (
            "${{ github.workflow }}-${{ github.event.pull_request.number || github.ref }}"
        )
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


def test_pages_finishes_active_deploy():
    """Preserve the active Pages deployment."""
    assert workflow("pages.yml")["concurrency"] == {
        "group": "pages", "cancel-in-progress": False,
    }


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
                               env=env, capture_output=True, text=True, check=False)
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
                                   env=env, capture_output=True, text=True, check=False)
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
