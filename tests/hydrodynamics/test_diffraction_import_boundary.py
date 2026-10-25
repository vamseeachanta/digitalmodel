"""Fresh-process coverage of the offline diffraction import boundary.

No interface substitutes are installed. A rejecting finder and import wrapper
record requests before Python can load a licensed interface. Expected requests
are the empty set, derived from the offline contract in issue #2526.
"""

from __future__ import annotations

import os
from pathlib import Path
import subprocess
import sys
import textwrap

import pytest


ROOT = Path(__file__).resolve().parents[2]
PACKAGE = "digitalmodel.hydrodynamics.diffraction"
OFFLINE_TESTS = [
    "test_analytical_cylinder.py",
    "test_diffraction_units.py",
    "test_mesh_orientation.py",
    "test_mesh_open_boundary.py",
    "test_mesh_pipeline.py",
    "test_reverse_parsers.py",
    "test_aqwa_ah1_parser.py",
    "test_result_extractor.py",
    "test_report_generator.py",
    "test_benchmark_runner.py",
]

_GUARD = """
import builtins
import importlib.abc
import sys

attempts = []
roots = {"OrcFxAPI", "OrcFxAPIConfig", "OrcaWave", "OrcaFlex", "ansys"}

class ForbiddenInterface(BaseException):
    pass

def check(name):
    if name.split(".")[0] in roots:
        attempts.append(name)
        raise ForbiddenInterface(name)

assert not any(name.split(".")[0] in roots for name in sys.modules)
original_import = builtins.__import__

def guarded_import(name, *args, **kwargs):
    check(name)
    return original_import(name, *args, **kwargs)

class RejectInterfaces(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        check(fullname)
        return None

builtins.__import__ = guarded_import
sys.meta_path.insert(0, RejectInterfaces())
"""


_MISSING_INTERFACE = _GUARD + """
from importlib.machinery import PathFinder

original_find_spec = PathFinder.find_spec

def missing_interface_spec(fullname, path=None, target=None):
    if fullname.split(".")[0] == "OrcFxAPI":
        return None
    return original_find_spec(fullname, path, target)

PathFinder.find_spec = missing_interface_spec

def check(name):
    if name.split(".")[0] == "OrcFxAPI":
        attempts.append(name)
        return
    if name.split(".")[0] in roots:
        raise ForbiddenInterface(name)
"""


def _run_guarded(code: str, tmp_path: Path, guard: str = _GUARD) -> None:
    env = os.environ.copy()
    env["PYTHONPATH"] = os.pathsep.join([str(ROOT / "src"), env.get("PYTHONPATH", "")])
    # This is a harness timeout, not an engineering acceptance limit.
    result = subprocess.run(
        [sys.executable, "-c", guard + textwrap.dedent(code)],
        cwd=ROOT,
        env=env,
        text=True,
        capture_output=True,
        timeout=300,
    )
    (tmp_path / "output.txt").write_text(result.stdout + result.stderr)
    assert result.returncode == 0, result.stdout + result.stderr


@pytest.mark.parametrize(
    "module",
    [
        PACKAGE,
        f"{PACKAGE}.solver",
        f"{PACKAGE}.solver.report_extractors",
        f"{PACKAGE}.result_extractor",
        f"{PACKAGE}.report_generator",
        f"{PACKAGE}.cli",
    ],
)
def test_offline_imports_never_request_interfaces(module, tmp_path):
    _run_guarded(
        f"""
        import importlib
        module = importlib.import_module({module!r})
        assert module is importlib.import_module({module!r})
        assert attempts == []
    """,
        tmp_path,
    )


def test_offline_numerical_parsing_and_mesh_nodes(tmp_path):
    paths = [f"tests/hydrodynamics/diffraction/{name}" for name in OFFLINE_TESTS]
    _run_guarded(
        f"""
        import pytest
        result = pytest.main({paths!r} + [
            "-q", "-rA", "-p", "no:cacheprovider",
            "--basetemp=" + {str(tmp_path / "nodes")!r},
        ])
        assert result == pytest.ExitCode.OK
        assert attempts == []
    """,
        tmp_path,
    )


def test_offline_public_helpers_remain_callable(tmp_path):
    """Public helper identities must survive the import-boundary change."""
    _run_guarded(
        """
        from digitalmodel.hydrodynamics import diffraction as package
        from digitalmodel.hydrodynamics.diffraction import solver
        from digitalmodel.hydrodynamics.diffraction.report_generator import (
            build_report_data_from_solver_results,
        )
        assert "OrcaWaveConverter" in dir(package)
        assert "OrcFxAPI" in dir(solver)
        assert callable(package.MockDataGenerator)
        assert callable(package.create_test_vessel)
        assert build_report_data_from_solver_results is solver.build_report_data_from_solver_results
        for module in (package, solver):
            try:
                module.unknown_export
            except AttributeError:
                pass
            else:
                raise AssertionError("unknown export did not raise AttributeError")
        assert attempts == []
    """,
        tmp_path,
    )


def test_worked_example_without_interfaces(tmp_path):
    _run_guarded(
        """
        import runpy
        runpy.run_path("examples/demos/diffraction/offline_imports.py", run_name="__main__")
        assert attempts == []
    """,
        tmp_path,
    )


def test_missing_interface_flags_and_exports(tmp_path):
    """Normal import discovery returns no interface, rather than raising a guard."""
    _run_guarded(
        """
        import importlib
        from digitalmodel.hydrodynamics import diffraction as package
        from digitalmodel.hydrodynamics.diffraction import result_extractor, solver

        assert attempts == []
        assert package.ORCAWAVE_AVAILABLE is False
        assert package.TEST_UTILITIES_AVAILABLE is False
        assert result_extractor.CONVERTER_AVAILABLE is False
        for name in (
            "OrcaWaveConverter", "convert_orcawave_results",
            "OrcaWaveDataExtractor", "extract_all_rao_data",
            "extract_all_added_mass", "extract_all_damping",
        ):
            assert getattr(package, name) is None
            assert getattr(solver, name) is None
        try:
            importlib.import_module(solver.__name__ + ".orcawave_converter")
        except ImportError:
            pass
        else:
            raise AssertionError("direct converter import did not raise ImportError")
        assert attempts and set(attempts) == {"OrcFxAPI"}
        assert not any(name.split(".")[0] in roots for name in sys.modules)
    """,
        tmp_path,
        guard=_MISSING_INTERFACE,
    )


@pytest.mark.parametrize(
    "export, submodule",
    [
        ("OrcaWaveConverter", "orcawave_converter"),
        ("convert_orcawave_results", "orcawave_converter"),
        ("OrcaWaveDataExtractor", "orcawave_data_extraction"),
        ("extract_all_rao_data", "orcawave_data_extraction"),
        ("extract_all_added_mass", "orcawave_data_extraction"),
        ("extract_all_damping", "orcawave_data_extraction"),
    ],
)
def test_converter_errors_propagate(export, submodule, tmp_path):
    _run_guarded(
        f"""
        from digitalmodel.hydrodynamics import diffraction as package
        from digitalmodel.hydrodynamics.diffraction import solver

        class BrokenConverterFinder(importlib.abc.MetaPathFinder):
            def find_spec(self, fullname, path=None, target=None):
                if fullname == solver.__name__ + "." + {submodule!r}:
                    raise RuntimeError("converter import failed")
                return None

        sys.meta_path.insert(0, BrokenConverterFinder())
        for module in (package, solver):
            try:
                getattr(module, {export!r})
            except RuntimeError as exc:
                assert str(exc) == "converter import failed"
            else:
                raise AssertionError("converter error was swallowed")
        assert attempts == []
    """,
        tmp_path,
    )


def test_cli_help_and_collection_never_request_interfaces(tmp_path):
    _run_guarded(
        f"""
        import pytest
        from click.testing import CliRunner
        from digitalmodel.hydrodynamics.diffraction.cli import cli

        runner = CliRunner()
        for command in ([], ["convert-orcawave"], ["compare"], ["plot-raos"]):
            result = runner.invoke(cli, command + ["--help"])
            assert result.exit_code == 0, result.output
        result = pytest.main([
            "tests/hydrodynamics/diffraction/test_cli_integration.py",
            "tests/hydrodynamics/diffraction/test_cli_run_validation.py",
            "--collect-only", "-q", "-p", "no:cacheprovider",
            "--basetemp=" + {str(tmp_path / "collection")!r},
        ])
        assert result == pytest.ExitCode.OK
        assert attempts == []
    """,
        tmp_path,
    )


def test_cli_missing_interface_fails_at_command_time(tmp_path):
    _run_guarded(
        f"""
        from pathlib import Path
        from click.testing import CliRunner
        from digitalmodel.hydrodynamics.diffraction.cli import cli

        assert attempts == []
        model = Path({str(tmp_path / "model.sim")!r})
        model.write_text("test input")
        runner = CliRunner()
        for command in (
            ["convert-orcawave", str(model)],
            ["compare", str(model.parent), str(model), "test"],
            ["plot-raos", str(model)],
        ):
            attempts.clear()
            result = runner.invoke(cli, command + ["--water-depth", "100"])
            assert result.exit_code == 1, result.output
            assert "OrcFxAPI not available" in result.output
            assert attempts and set(attempts) == {{"OrcFxAPI"}}
        assert not any(name.split(".")[0] in roots for name in sys.modules)
    """,
        tmp_path,
        guard=_MISSING_INTERFACE,
    )


def test_cli_resolves_converter_when_commands_run(tmp_path):
    """Exercise command resolution with a test double, without an interface."""
    _run_guarded(
        f"""
        import importlib
        from pathlib import Path
        from unittest.mock import MagicMock
        from click.testing import CliRunner
        from digitalmodel.hydrodynamics import diffraction as package

        cli_module = importlib.import_module(package.__name__ + ".cli")
        package.ORCAWAVE_AVAILABLE = True
        package.OrcaWaveConverter = MagicMock(
            side_effect=RuntimeError("converter reached")
        )
        cli_module.AQWAConverter = MagicMock()
        model = Path({str(tmp_path / "model.sim")!r})
        model.write_text("test input")
        runner = CliRunner()
        for command in (
            ["convert-orcawave", str(model)],
            ["compare", str(model.parent), str(model), "test"],
            ["plot-raos", str(model)],
        ):
            package.OrcaWaveConverter.reset_mock()
            result = runner.invoke(cli_module.cli, command + ["--water-depth", "100"])
            assert result.exit_code == 1, result.output
            assert "converter reached" in result.output
            package.OrcaWaveConverter.assert_called_once()
        assert attempts == []
    """,
        tmp_path,
    )
