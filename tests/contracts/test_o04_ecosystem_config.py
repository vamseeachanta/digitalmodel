"""Regression contracts for owner board O04 reconciliation."""

import os
import subprocess
import tomllib
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]


def test_publishing_is_manual_only():
    workflow = yaml.safe_load((ROOT / ".github/workflows/publish.yml").read_text())
    assert workflow.get("on", workflow.get(True)) == {"workflow_dispatch": None}


def test_matrix_artifacts_are_unique():
    workflow = yaml.safe_load(
        (ROOT / ".github/workflows/quality-gates.yml").read_text()
    )
    job = workflow["jobs"]["quality-gates"]
    assert job["strategy"]["matrix"]["python-version"] == ["3.11", "3.12"]
    for step in job["steps"]:
        if step.get("uses", "").startswith("actions/upload-artifact@"):
            assert "matrix.python-version" in step["with"]["name"]


def test_required_quality_gate_context_fails_closed():
    workflow = yaml.safe_load(
        (ROOT / ".github/workflows/quality-gates.yml").read_text()
    )
    matches = [
        job
        for job in workflow["jobs"].values()
        if job.get("name") == "Run Quality Gates"
    ]
    assert len(matches) == 1
    job = matches[0]
    assert job["needs"] == "quality-gates"
    assert job["if"] == "always()"
    assert "strategy" not in job
    assert "continue-on-error" not in job
    assert job["permissions"] == {}
    assert len(job["steps"]) == 1
    step = job["steps"][0]
    assert "continue-on-error" not in step
    command = step["run"]
    assert step["env"]["QUALITY_GATES_RESULT"] == ("${{ needs.quality-gates.result }}")
    assert "${{" not in command
    for result in ("success", "failure", "cancelled", "skipped", ""):
        completed = subprocess.run(
            ["bash", "-c", command],
            env={**os.environ, "QUALITY_GATES_RESULT": result},
            capture_output=True,
        )
        assert (completed.returncode == 0) == (result == "success")


def test_formatter_commands_do_not_target_all_source():
    config = tomllib.loads((ROOT / "pyproject.toml").read_text())
    assert config["tool"]["scripts"]["format"] == "make format"
    text = (ROOT / "Makefile").read_text()
    assert "ruff format src/" not in text
    assert "ruff check --fix src/" not in text


def test_mkdocs_navigation_targets_exist():
    config = yaml.safe_load((ROOT / "mkdocs.yml").read_text())

    def visit(items):
        for item in items:
            for value in item.values():
                if isinstance(value, list):
                    visit(value)
                else:
                    assert (ROOT / config["docs_dir"] / value).is_file(), value

    visit(config["nav"])


def test_lockfile_matches_project_version():
    project = tomllib.loads((ROOT / "pyproject.toml").read_text())
    locked = tomllib.loads((ROOT / "uv.lock").read_text())
    package = next(p for p in locked["package"] if p["name"] == "digitalmodel")
    assert package["version"] == project["project"]["version"]


def test_global_lint_preserves_main_default_rules():
    config = tomllib.loads((ROOT / "pyproject.toml").read_text())
    assert set(config["tool"]["ruff"]["lint"]["select"]) == {"E4", "E7", "E9", "F"}
