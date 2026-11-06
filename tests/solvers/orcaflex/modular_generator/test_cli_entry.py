# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Exercise the offline entry point with independent synthetic inputs."""

import os
from pathlib import Path
import subprocess
import sys
import tomllib

import pytest
import yaml
from yaml.tokens import AliasToken, AnchorToken

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator


ROOT = next(parent for parent in Path(__file__).resolve().parents
            if (parent / "pyproject.toml").is_file())
MODULE = "digitalmodel.solvers.orcaflex.modular_generator.cli"
# A successful process returns zero; failures return nonzero (POSIX convention).
# Each child is bounded to one minute so a regression cannot hang the suite.
CHILD_TIMEOUT = 60
BOOTSTRAP = """
import importlib
import importlib.abc
import sys

class RejectSolver(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split('.')[0].casefold() in {'orcfxapi', 'orcawave'}:
            raise AssertionError('Licensed solver import attempted')

sys.meta_path.insert(0, RejectSolver())
module, name = sys.argv.pop(1).split(':')
getattr(importlib.import_module(module), name)()
"""


def run_cli(*args, target=f"{MODULE}:main", cwd):
    """Run a fresh process that rejects licensed solver imports before loading."""
    env = dict(os.environ, PYTHONDONTWRITEBYTECODE="1")
    env["PYTHONPATH"] = os.pathsep.join(
        filter(None, (str(ROOT / "src"), env.get("PYTHONPATH")))
    )
    return subprocess.run(
        [sys.executable, "-c", BOOTSTRAP, target, *map(str, args)],
        cwd=cwd,
        env=env,
        text=True,
        capture_output=True,
        timeout=CHILD_TIMEOUT,
        check=False,
    )


@pytest.fixture
def spec_data():
    """Use a synthetic water column; no engineering result is asserted.

    The prescribed depth is 100 m, chosen as two 50 m layers by hand.
    Empty stiffness uses schema defaults; the generic model has no objects.
    """
    return {
        "metadata": {
            "name": "synthetic",
            "description": "Synthetic CLI input",
            "structure": "generic",
            "operation": "generic",
            "project": "generic",
        },
        "environment": {"water": {"depth": 100}, "seabed": {"stiffness": {}}},
        "generic": {},
    }


@pytest.fixture
def spec_file(tmp_path, spec_data):
    """Save a spec with a space in its name to exercise argument boundaries."""
    path = tmp_path / "synthetic spec.yml"
    path.write_text(yaml.safe_dump(spec_data), encoding="utf-8")
    return path


def snapshot(directory):
    """Compare every published file, including the master and parameters."""
    return {
        path.relative_to(directory): path.read_bytes()
        for path in directory.rglob("*")
        if path.is_file()
    }


def test_console_entry_point(tmp_path):
    scripts = tomllib.loads((ROOT / "pyproject.toml").read_text())["project"]["scripts"]
    assert scripts["modular-generator"] == f"{MODULE}:main"
    result = run_cli("--help", target=scripts["modular-generator"], cwd=tmp_path)
    assert result.returncode == 0, result.stderr
    assert "spec" in result.stdout
    assert "--output" in result.stdout


def test_help_is_offline(tmp_path):
    result = run_cli("--help", cwd=tmp_path)
    assert result.returncode == 0, result.stderr
    assert "spec" in result.stdout
    assert "--output" in result.stdout
    assert "--allow-validation-errors" not in result.stdout
    assert not list(tmp_path.iterdir())


@pytest.mark.parametrize("output_flag", ["--output", "-o"])
def test_synthetic_spec_matches_direct_generation(spec_file, tmp_path, output_flag):
    output = tmp_path / "cli output"
    result = run_cli(spec_file, output_flag, output, cwd=tmp_path)
    assert result.returncode == 0, result.stderr
    direct = tmp_path / "direct"
    ModularModelGenerator(spec_file).generate(direct)
    actual = snapshot(output)
    assert actual == snapshot(direct)
    assert Path("master.yml") in actual
    environment = yaml.safe_load(actual[Path("includes/03_environment.yml")])
    # Independent prescribed input: two 50 m layers give a 100 m water column.
    # Native key contract: builders/test_environment_builder.py's
    # test_water_depth_default_key example.
    assert environment["Environment"]["SeabedOriginDepth"] == 100
    for content in actual.values():
        assert not any(isinstance(token, (AliasToken, AnchorToken))
                       for token in yaml.scan(content))


def test_absent_input_fails(tmp_path):
    output = tmp_path / "output"
    result = run_cli(tmp_path / "absent.yml", "--output", output, cwd=tmp_path)
    assert result.returncode != 0
    assert "Input file not found" in result.stderr
    assert not output.exists()


@pytest.mark.parametrize("args", [[], ["spec.yml"], ["--output", "output"]])
def test_required_arguments_fail(tmp_path, args):
    result = run_cli(*args, cwd=tmp_path)
    assert result.returncode != 0
    assert "required" in result.stderr
    assert not (tmp_path / "output").exists()


@pytest.mark.parametrize("content", ["", "[", "[]", "null", "metadata: {}"])
def test_invalid_input_fails(tmp_path, content):
    path = tmp_path / "invalid.yml"
    path.write_text(content, encoding="utf-8")
    output = tmp_path / "output"
    result = run_cli(path, "--output", output, cwd=tmp_path)
    assert result.returncode != 0
    assert "ERROR:" in result.stderr
    assert "Traceback" not in result.stderr
    assert not output.exists()


@pytest.mark.parametrize("existing", [False, True])
def test_post_validation_failure_does_not_publish(spec_data, tmp_path, existing):
    # Case-insensitive name collision, following test_validation_gate.py's
    # independently specified invalid-input case; no solver is required.
    spec_data["generic"] = {
        "line_types": [{"name": "shared"}],
        "lines": [{"name": "SHARED"}],
    }
    path = tmp_path / "invalid.yml"
    path.write_text(yaml.safe_dump(spec_data), encoding="utf-8")
    output = tmp_path / "output"
    if existing:
        output.mkdir()
        (output / "master.yml").write_text("previous master\n", encoding="utf-8")
    before = snapshot(output)
    result = run_cli(path, "--output", output, cwd=tmp_path)
    assert result.returncode != 0
    assert "Post-generation validation failed" in result.stderr
    assert snapshot(output) == before
    assert output.exists() == existing


@pytest.mark.parametrize("command", ["generate", "validate"])
def test_existing_commands_still_work(spec_file, tmp_path, command):
    args = [command, "--input", spec_file]
    output = tmp_path / "legacy"
    if command == "generate":
        args.extend(["--output", output])
    result = run_cli(*args, cwd=tmp_path)
    assert result.returncode == 0, result.stderr
    assert (output / "master.yml").exists() == (command == "generate")
