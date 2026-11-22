# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Installed script inventory and offline wall-thickness help contracts."""

import os
import shutil
import subprocess
import sys
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
    # uv overlays add installation directories without changing the base scheme.
    site_paths = {
        Path(sysconfig.get_path(name)).resolve() for name in ("purelib", "platlib")
    }
    site_paths.update(
        Path(path).resolve()
        for path in sys.path
        if Path(path).name in {"site-packages", "dist-packages"}
    )
    matches = [
        dist
        for dist in metadata.distributions(path=site_paths)
        if dist.metadata["Name"] == project["name"]
    ]
    # A distribution installed into this interpreter must have unique metadata.
    assert len(matches) == 1, f"Install {project['name']} in the test environment"
    (dist,) = matches
    return dist.entry_points


def _assert_entry_point_target(entry_points, declared_target, command=COMMAND):
    matches = [
        entry
        for entry in entry_points
        if entry.group == "console_scripts" and entry.name == command
    ]
    # Each [project.scripts] key declares exactly one console-script target.
    assert len(matches) == 1, f"Expected one installed {command} entry point"
    (entry,) = matches
    assert entry.value == declared_target, (
        f"Installed {command} target differs from pyproject.toml: "
        f"{entry.value!r} != {declared_target!r}"
    )


def _installed_executable(scripts_dir, command=COMMAND):
    executable = shutil.which(command, path=str(scripts_dir))
    assert executable is not None, f"{command} executable missing from test environment"
    path = Path(executable).resolve()
    assert path.parent == scripts_dir.resolve(), (
        f"{command} executable is outside the test environment"
    )
    return path


def _assert_declared_scripts(project, *, scripts_dir=None):
    entry_points = _installed_entry_points(project)
    if scripts_dir is None:
        scripts_dir = Path(sysconfig.get_path("scripts"))
    errors = []
    for command, target in sorted(project["scripts"].items()):
        try:
            _assert_entry_point_target(entry_points, target, command)
        except AssertionError as error:
            errors.append(str(error))
        try:
            _installed_executable(scripts_dir, command)
        except AssertionError as error:
            errors.append(str(error))
    assert not errors, "Installed console-script inventory differs:\n" + "\n".join(errors)


def _run_help(executable, working_dir):
    env = {
        key: value
        for key, value in os.environ.items()
        if key != "PYTHONPATH" and not key.startswith("GIT_")
    }
    return subprocess.run(
        [str(executable), "--help"],
        cwd=working_dir,
        env=env,
        capture_output=True,
        text=True,
        timeout=HELP_TIMEOUT_SECONDS,
        check=True,
        creationflags=subprocess.CREATE_NO_WINDOW if sys.platform == "win32" else 0,
    )


def test_all_declared_scripts_are_installed(project):
    """Check every declared target and executable without loading or running it."""
    _assert_declared_scripts(project)


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


@pytest.fixture
def metadata_layout(tmp_path, monkeypatch, project):
    """Build real metadata with separate source, base and overlay directories."""
    source = tmp_path / "src"
    base = tmp_path / "base" / "site-packages"
    overlay = tmp_path / "overlay" / "site-packages"
    for directory in (source, base, overlay):
        directory.mkdir(parents=True)
    get_path = sysconfig.get_path
    monkeypatch.setattr(
        sysconfig, "get_path",
        lambda name: str(base) if name in ("purelib", "platlib") else get_path(name),
    )
    monkeypatch.setattr(sys, "path", [str(source), str(overlay), str(base)])

    def write_metadata(directory, *, source_only=False, scripts=None):
        suffix = "egg-info" if source_only else "dist-info"
        info = directory / f"{project['name']}-1.0.{suffix}"
        info.mkdir()
        metadata_name = "PKG-INFO" if source_only else "METADATA"
        (info / metadata_name).write_text(
            f"Name: {project['name']}\nVersion: 1.0\n", encoding="utf-8"
        )
        if scripts is None:
            scripts = {COMMAND: project["scripts"][COMMAND]}
        (info / "entry_points.txt").write_text(
            "[console_scripts]\n"
            + "".join(f"{name} = {target}\n" for name, target in scripts.items()),
            encoding="utf-8",
        )

    return source, base, overlay, write_metadata


def test_entry_point_discovery_in_overlay(metadata_layout, project):
    """An overlay installation is visible even outside the base scheme paths."""
    source, _, overlay, write_metadata = metadata_layout
    write_metadata(source, source_only=True)
    write_metadata(overlay)
    _assert_entry_point_target(
        _installed_entry_points(project), project["scripts"][COMMAND]
    )


def test_entry_point_discovery_rejects_source_only(metadata_layout, project):
    """Source egg-info alone cannot satisfy the installed metadata contract."""
    source, _, _, write_metadata = metadata_layout
    write_metadata(source, source_only=True)
    with pytest.raises(AssertionError, match="Install digitalmodel"):
        _installed_entry_points(project)


def test_entry_point_discovery_rejects_duplicate_installs(metadata_layout, project):
    """Different installation directories must still have unique metadata."""
    _, base, overlay, write_metadata = metadata_layout
    write_metadata(base)
    write_metadata(overlay)
    with pytest.raises(AssertionError, match="Install digitalmodel"):
        _installed_entry_points(project)


@pytest.fixture
def script_inventory(metadata_layout, project, tmp_path, monkeypatch):
    """Declare multiple commands with unimportable targets and inert executables."""
    _, base, _, write_metadata = metadata_layout
    scripts = {
        COMMAND: project["scripts"][COMMAND],
        "new-command": "unimportable_inventory_fixture:main",
        "another-command": "unimportable_inventory_fixture:other",
    }
    declaration = {**project, "scripts": scripts}
    scripts_dir = tmp_path / "scripts"
    scripts_dir.mkdir()
    suffix = ".exe" if sys.platform == "win32" else ""
    for name in scripts:
        executable = scripts_dir / f"{name}{suffix}"
        executable.write_text("Inventory fixture; never execute.\n", encoding="utf-8")
        executable.chmod(0o755)
    get_path = sysconfig.get_path
    monkeypatch.setattr(
        sysconfig, "get_path",
        lambda name: str(scripts_dir) if name == "scripts" else get_path(name),
    )
    return declaration, base, scripts_dir, write_metadata


def test_inventory_accepts_multiple_commands_without_loading(script_inventory):
    declaration, base, _, write_metadata = script_inventory
    write_metadata(base, scripts=declaration["scripts"])
    _assert_declared_scripts(declaration)


def test_inventory_rejects_missing_distribution(script_inventory):
    declaration, _, _, _ = script_inventory
    with pytest.raises(AssertionError, match="Install digitalmodel"):
        _assert_declared_scripts(declaration)


def test_inventory_rejects_new_declaration_without_metadata(script_inventory):
    declaration, base, _, write_metadata = script_inventory
    # Existing single-command coverage would accept this incomplete installation.
    write_metadata(base)
    with pytest.raises(AssertionError, match="Expected one installed new-command"):
        _assert_declared_scripts(declaration)


def test_inventory_rejects_mismatched_target(script_inventory):
    declaration, base, _, write_metadata = script_inventory
    scripts = {**declaration["scripts"], "new-command": "stale_inventory_fixture:main"}
    write_metadata(base, scripts=scripts)
    with pytest.raises(AssertionError, match="new-command target differs"):
        _assert_declared_scripts(declaration)


@pytest.mark.parametrize("group", ["console_scripts", "gui_scripts"])
def test_inventory_requires_one_console_entry(script_inventory, group):
    declaration, base, _, write_metadata = script_inventory
    scripts = {
        name: target for name, target in declaration["scripts"].items()
        if name != "new-command"
    }
    write_metadata(base, scripts=scripts)
    info = next(base.glob("*.dist-info")) / "entry_points.txt"
    entry = f"new-command = {declaration['scripts']['new-command']}\n"
    with info.open("a", encoding="utf-8") as stream:
        stream.write(entry * 2 if group == "console_scripts" else f"[{group}]\n{entry}")
    with pytest.raises(AssertionError, match="Expected one installed new-command"):
        _assert_declared_scripts(declaration)


def test_inventory_rejects_missing_executable(script_inventory, monkeypatch):
    declaration, base, scripts_dir, write_metadata = script_inventory
    write_metadata(base, scripts=declaration["scripts"])
    # The command still exists on PATH; only the selected directory is empty.
    monkeypatch.setenv("PATH", str(scripts_dir))
    empty_dir = scripts_dir / "empty"
    empty_dir.mkdir()
    get_path = sysconfig.get_path
    monkeypatch.setattr(
        sysconfig, "get_path",
        lambda name: str(empty_dir) if name == "scripts" else get_path(name),
    )
    with pytest.raises(AssertionError, match="new-command executable missing"):
        _assert_declared_scripts(declaration)


def test_inventory_reports_all_failures_in_name_order(script_inventory):
    declaration, base, scripts_dir, write_metadata = script_inventory
    # Both new commands are missing metadata and executables.
    write_metadata(base)
    declaration["scripts"] = dict(reversed(declaration["scripts"].items()))
    empty_dir = scripts_dir / "empty"
    empty_dir.mkdir()
    with pytest.raises(AssertionError) as error:
        _assert_declared_scripts(declaration, scripts_dir=empty_dir)
    message = str(error.value)
    for name in ("another-command", "new-command"):
        assert f"Expected one installed {name} entry point" in message
        assert f"{name} executable missing" in message
    assert message.index("another-command") < message.index("new-command")


@pytest.mark.parametrize("platform", ["linux", "win32"])
def test_help_child_clears_git_bindings(tmp_path, monkeypatch, platform):
    """Repository settings from the parent must not reach the installed command."""
    monkeypatch.setattr(sys, "platform", platform)
    if platform == "win32":
        monkeypatch.setattr(subprocess, "CREATE_NO_WINDOW", 0x08000000, raising=False)
    bindings = ("GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_COMMON_DIR")
    for name in bindings:
        monkeypatch.setenv(name, str(tmp_path / "parent-repository"))
    monkeypatch.setenv("GIT_CONFIG_COUNT", "1")
    monkeypatch.setenv("GIT_CONFIG_KEY_0", "core.worktree")
    monkeypatch.setenv("GIT_CONFIG_VALUE_0", str(tmp_path))

    def capture_run(command, **kwargs):
        assert not any(key.startswith("GIT_") for key in kwargs["env"])
        expected_flags = subprocess.CREATE_NO_WINDOW if sys.platform == "win32" else 0
        assert kwargs["creationflags"] == expected_flags
        assert kwargs["timeout"] == HELP_TIMEOUT_SECONDS
        assert command == [str(tmp_path / COMMAND), "--help"]

    monkeypatch.setattr(subprocess, "run", capture_run)
    _run_help(tmp_path / COMMAND, tmp_path)
