# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Regression contracts for the workflow automation CI job (issue #1958).

Expected dependencies come from the workflow's coverage and benchmark consumers
and the repository's optional-dependency declarations, rather than the local
environment. The daily heartbeat and dependency-file triggers implement #1958.
No engineering equations or empirical acceptance limits are used here.
"""

import configparser
import re
import shlex
import tomllib
from pathlib import Path

import pytest
import yaml


REPO = Path(__file__).resolve().parents[3]
WORKFLOW_PATH = ".github/workflows/workflow-automation-tests.yml"


@pytest.fixture
def workflow():
    """Read Actions keys as strings, including YAML 1.1's ambiguous `on`."""
    return yaml.load((REPO / WORKFLOW_PATH).read_text(), Loader=yaml.BaseLoader)


@pytest.mark.parametrize("plugin", ["pytest", "pytest-cov", "pytest-benchmark"])
def test_install_selected_extras_include_test_plugins(workflow, plugin):
    """The actual install must select the plugins consumed by this test suite.

    Hand derivation: `--cov` requires pytest-cov; the `benchmark` fixture in
    test_performance_benchmarks.py requires pytest-benchmark; both require
    pytest. Resolve the selected extras against pyproject.toml, so moving a
    plugin out of an extra cannot silently break the job again.
    """
    steps = workflow["jobs"]["test"]["steps"]
    install = next(
        step["run"] for step in steps if step.get("name") == "Install dependencies"
    )
    requirements = shlex.split(install)
    editable = requirements[requirements.index("-e") + 1]
    assert editable.startswith(".[") and editable.endswith("]")
    extras = editable[2:-1].split(",")
    project = tomllib.loads((REPO / "pyproject.toml").read_text())["project"]
    dependencies = list(project["dependencies"])
    for extra in extras:
        dependencies.extend(project["optional-dependencies"][extra])
    names = {re.split(r"[<>=!~;\[ ]", dep, maxsplit=1)[0] for dep in dependencies}
    assert plugin in names, f"{plugin} is absent from the installed extras {extras}"


def test_heartbeat_runs_daily_without_path_filters(workflow):
    """Issue #1958 requires daily execution even without matching changes.

    Owner ruling, 28 Oct 2026: the daily run is the cron-dispatched "Main full
    run" (full-matrix-sweep.yml), which calls this workflow without path
    filters; GitHub `schedule:` triggers are not used.
    """
    assert "workflow_call" in workflow["on"]
    assert "schedule" not in workflow["on"]
    sweep = yaml.load((REPO / ".github/workflows/full-matrix-sweep.yml").read_text(),
                      Loader=yaml.BaseLoader)
    callers = [job for job in sweep["jobs"].values()
               if job.get("uses") == "./" + WORKFLOW_PATH]
    assert len(callers) == 1
    # A path/event condition at job level could silently suppress the heartbeat.
    assert "if" not in workflow["jobs"]["test"]
    assert "if" not in callers[0]


def test_manual_dispatch_is_available(workflow):
    """Allow maintainers to obtain a fresh result without a source change."""
    assert "workflow_dispatch" in workflow["on"]


def test_matrix_versions_finish_independently(workflow):
    """A failure on one interpreter must not cancel evidence from another."""
    assert workflow["jobs"]["test"]["strategy"]["fail-fast"] == "false"


@pytest.mark.parametrize("name", ["Test CLI commands", "Verify package installation"])
def test_diagnostics_run_after_failures_unless_cancelled(workflow, name):
    """Keep CLI and import outcomes visible after an earlier step fails."""
    steps = workflow["jobs"]["test"]["steps"]
    step = next(step for step in steps if step.get("name") == name)
    assert step.get("if") == "${{ !cancelled() }}"


@pytest.mark.parametrize("event", ["pull_request"])
def test_dependency_and_workflow_changes_trigger_ci(workflow, event):
    """Each listed file can alter installation, collection or the job itself."""
    paths = workflow["on"][event]["paths"]
    assert {
        "src/digitalmodel/workflows/workflow_automation/**",
        "tests/workflows/workflow_automation/**",
        WORKFLOW_PATH,
        "pyproject.toml",
        ".coveragerc",
        "uv.lock",
        "pytest.ini",
    }.issubset(paths)


def test_coverage_collection_and_upload_remain_enabled(workflow):
    """Preserve the original coverage intent while repairing installation."""
    steps = workflow["jobs"]["test"]["steps"]
    run_tests = next(
        step["run"]
        for step in steps
        if step.get("name") == "Run workflow automation tests"
    )
    assert {
        "tests/workflows/workflow_automation/",
        "--cov=digitalmodel.workflows.workflow_automation",
        "--cov-report=xml",
        "--cov-report=term",
    }.issubset(shlex.split(run_tests))
    upload = next(step for step in steps if step.get("name") == "Upload coverage")
    assert upload["uses"].startswith("codecov/codecov-action@")
    assert upload["with"]["file"] == "./coverage.xml"


def test_workflow_selects_repository_coverage_gate(workflow):
    """Collect with repository settings without masking the test outcome."""
    job = workflow["jobs"]["test"]
    step = next(
        step
        for step in job["steps"]
        if step.get("name") == "Run workflow automation tests"
    )
    assert "continue-on-error" not in job
    assert "continue-on-error" not in step
    assert shlex.split(step["run"]) == [
        "uv",
        "run",
        "--no-sync",
        "pytest",
        "tests/workflows/workflow_automation/",
        "-v",
        "--cov=digitalmodel.workflows.workflow_automation",
        "--cov-config=pyproject.toml",
        "--cov-fail-under=0",
        "--cov-report=xml",
        "--cov-report=term",
    ]
    coverage = tomllib.loads((REPO / "pyproject.toml").read_text())["tool"]["coverage"]
    assert coverage["report"]["fail_under"] == 80.0


def test_final_step_enforces_configured_coverage_after_diagnostics(workflow):
    """The unchanged threshold must fail independently of tests and imports."""
    steps = workflow["jobs"]["test"]["steps"]
    gate = steps[-1]
    assert "continue-on-error" not in gate
    assert gate.get("name") == "Enforce coverage threshold"
    assert gate.get("if") == "${{ !cancelled() }}"
    assert shlex.split(gate["run"]) == [
        "uv",
        "run",
        "--no-sync",
        "coverage",
        "report",
        "--rcfile=pyproject.toml",
    ]
    for name in [
        "Run workflow automation tests",
        "Upload coverage",
        "Test CLI commands",
        "Verify package installation",
    ]:
        assert any(step.get("name") == name for step in steps[:-1])


@pytest.mark.parametrize(
    "section", tomllib.loads((REPO / "pyproject.toml").read_text())["tool"]["coverage"]
)
def test_coverage_configurations_agree(section):
    """Each TOML-declared setting must agree with the default INI configuration.

    INI-only settings and sections are outside this comparison.
    """
    coverage = tomllib.loads((REPO / "pyproject.toml").read_text())["tool"]["coverage"]
    config = configparser.ConfigParser(interpolation=None)
    config.read(REPO / ".coveragerc")
    for option, expected in coverage[section].items():
        assert config.has_option(section, option), f"{section}.{option} is missing"
        if isinstance(expected, bool):
            actual = config.getboolean(section, option)
        elif isinstance(expected, (int, float)):
            actual = config.getfloat(section, option)
        elif isinstance(expected, list):
            actual = config.get(section, option).splitlines()
            actual = [line.strip() for line in actual if line.strip()]
        else:
            actual = config.get(section, option)
        assert actual == expected, f"{section}.{option} differs between configurations"
