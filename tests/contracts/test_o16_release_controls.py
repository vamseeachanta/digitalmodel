"""Release controls for owner board O16 and PR 2309."""

import os
import subprocess
import sys
import tomllib
from pathlib import Path

import yaml
import pytest

ROOT = Path(__file__).resolve().parents[2]
VERSION = tomllib.loads((ROOT / "pyproject.toml").read_text(encoding="utf-8"))[
    "project"
]["version"]


def test_cli_version_matches_project():
    result = subprocess.run(
        [sys.executable, "-m", "digitalmodel", "--version"],
        cwd=ROOT,
        env={**os.environ, "PYTHONPATH": str(ROOT / "src")},
        capture_output=True,
        text=True,
        check=True,
    )
    assert result.stdout.strip() == f"digitalmodel {VERSION}"


def test_cli_help_matches_project():
    result = subprocess.run(
        [sys.executable, "-m", "digitalmodel", "--help"],
        cwd=ROOT,
        env={**os.environ, "PYTHONPATH": str(ROOT / "src")},
        capture_output=True,
        text=True,
        check=True,
    )
    assert f"digitalmodel v{VERSION}" in result.stdout


def test_package_version_matches_project():
    from digitalmodel import __version__

    assert __version__ == VERSION


def test_uninstalled_source_version_matches_project():
    result = subprocess.run(
        [
            sys.executable,
            "-S",
            "-c",
            "from importlib import metadata; "
            "exec('def missing(name):\\n raise metadata.PackageNotFoundError(name)'); "
            "metadata.version = missing; "
            "import digitalmodel; print(digitalmodel.__version__)",
        ],
        cwd=ROOT,
        env={**os.environ, "PYTHONPATH": str(ROOT / "src")},
        capture_output=True,
        text=True,
        check=True,
    )
    assert result.stdout.strip() == VERSION


def test_installed_metadata_matches_project():
    from importlib.metadata import PackageNotFoundError, version

    try:
        installed_version = version("digitalmodel")
    except PackageNotFoundError:
        pytest.skip("Uninstalled checkout: metadata is not available")
    assert installed_version == VERSION


def test_publish_requires_main_and_pypi_environment():
    workflow = yaml.safe_load(
        (ROOT / ".github/workflows/publish.yml").read_text(encoding="utf-8")
    )
    assert workflow.get("on", workflow.get(True)) == {"workflow_dispatch": None}
    job = workflow["jobs"]["publish"]
    assert job["if"] == "github.ref == 'refs/heads/main'"
    assert job["environment"] == "pypi"
