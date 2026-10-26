# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Installed console-script contracts for the offline wall-thickness group."""

import os
import shutil
import subprocess
import sysconfig
import tomllib
from importlib import metadata
from pathlib import Path

import pytest


COMMAND = "wall-thickness"  # Source: pyproject.toml [project.scripts].
# Harness policy: allow 60 seconds for this offline help command, then fail.
HELP_TIMEOUT_SECONDS = 60


@pytest.fixture
def project(pytestconfig):
    """Read the distribution name and target from the checkout declaration."""
    with (pytestconfig.rootpath / "pyproject.toml").open("rb") as stream:
        return tomllib.load(stream)["project"]


def _installed_entry_points(project):
    # conftest adds src/ to sys.path; do not mistake source egg-info for an install.
    site_paths = {sysconfig.get_path("purelib"), sysconfig.get_path("platlib")}
    matches = [
        dist
        for dist in metadata.distributions(path=site_paths)
        if dist.metadata["Name"] == project["name"]
    ]
    # A distribution installed into this interpreter must have unique metadata.
    assert len(matches) == 1, f"Install {project['name']} in the test environment"
    (dist,) = matches
    return dist.entry_points


def _assert_entry_point_target(entry_points, declared_target):
    matches = [
        entry
        for entry in entry_points
        if entry.group == "console_scripts" and entry.name == COMMAND
    ]
    # Each [project.scripts] key declares exactly one console-script target.
    assert len(matches) == 1, f"Expected one installed {COMMAND} entry point"
    (entry,) = matches
    assert entry.value == declared_target, (
        f"Installed {COMMAND} target differs from pyproject.toml: "
        f"{entry.value!r} != {declared_target!r}"
    )


def _installed_executable(scripts_dir):
    executable = shutil.which(COMMAND, path=str(scripts_dir))
    assert executable is not None, f"{COMMAND} executable missing from test environment"
    path = Path(executable).resolve()
    assert path.parent == scripts_dir.resolve(), (
        f"{COMMAND} executable is outside the test environment"
    )
    return path


def _run_help(executable, working_dir):
    env = os.environ.copy()
    env.pop("PYTHONPATH", None)
    return subprocess.run(
        [str(executable), "--help"],
        cwd=working_dir,
        env=env,
        capture_output=True,
        text=True,
        timeout=HELP_TIMEOUT_SECONDS,
        check=True,
    )


def test_wall_thickness_entry_point_matches_project(project):
    """Installed metadata must agree with the declared console-script target."""
    _assert_entry_point_target(
        _installed_entry_points(project), project["scripts"][COMMAND]
    )


def test_installed_wall_thickness_help(tmp_path, pytestconfig, monkeypatch):
    """The installed executable must work away from the checkout and PYTHONPATH."""
    assert not tmp_path.resolve().is_relative_to(pytestconfig.rootpath.resolve())
    executable = _installed_executable(Path(sysconfig.get_path("scripts")))

    # This module raises if the child accidentally inherits the source search path.
    poison_dir = tmp_path / "pythonpath"
    poison_dir.mkdir()
    (poison_dir / "digitalmodel.py").write_text(
        "raise RuntimeError('PYTHONPATH was inherited')\n", encoding="utf-8"
    )
    monkeypatch.setenv("PYTHONPATH", str(poison_dir))
    working_dir = tmp_path / "working"
    working_dir.mkdir()
    result = _run_help(executable, working_dir)

    help_text = result.stdout.strip()
    assert help_text, f"Empty help output; stderr: {result.stderr}"
    assert f"Usage: {COMMAND}" in help_text
    # Documented help contract: structural/analysis/cli.py::cli and @cli.command.
    assert "Pipeline Wall Thickness Design Checks" in help_text
    assert "Commands:" in help_text
    commands_text = help_text.split("Commands:", maxsplit=1)[-1]
    commands = {line.split()[0] for line in commands_text.splitlines() if line.strip()}
    for command in ("check", "sweep", "phases", "list-grades", "lookup", "report"):
        assert command in commands


def test_harness_rejects_absent_executable(tmp_path, monkeypatch):
    """An empty script directory must fail even with an installed command on PATH."""
    monkeypatch.setenv("PATH", sysconfig.get_path("scripts"))
    with pytest.raises(AssertionError, match="executable missing"):
        _installed_executable(tmp_path)


def test_harness_rejects_mismatched_entry_point(project):
    """A stale installed target must fail even when the command name agrees."""
    with pytest.raises(AssertionError, match="target differs"):
        _assert_entry_point_target(
            _installed_entry_points(project), project["scripts"][COMMAND] + "_stale"
        )
