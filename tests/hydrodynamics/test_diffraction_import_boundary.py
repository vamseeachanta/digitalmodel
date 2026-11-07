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


@pytest.mark.parametrize("option", ["--tolerance", "-t"])
def test_compare_retired_tolerance_never_requests_interface(option, tmp_path):
    _run_guarded(
        f"""
        from pathlib import Path
        from click.testing import CliRunner
        from digitalmodel.hydrodynamics.diffraction.cli import cli

        model = Path({str(tmp_path / "model.sim")!r})
        model.write_text("test input")
        result = CliRunner().invoke(cli, [
            "compare", str(model.parent), str(model), "test",
            "--water-depth", "100", {option!r}, "0.1",
        ])
        assert result.exit_code == 2, result.output
        assert "tolerance requires an explicit comparison policy" in result.output
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
        resolved_attempts = None
        for command in (
            ["convert-orcawave", str(model)],
            ["compare", str(model.parent), str(model), "test"],
            ["plot-raos", str(model)],
        ):
            result = runner.invoke(cli, command + ["--water-depth", "100"])
            assert result.exit_code == 1, result.output
            assert "OrcaWave converter unavailable" in result.output
            assert "OrcFxAPI or a converter dependency failed to import" in result.output
            assert attempts and set(attempts) == {{"OrcFxAPI"}}
            if resolved_attempts is None:
                resolved_attempts = attempts.copy()
            assert attempts == resolved_attempts
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
        import ast
        from unittest.mock import MagicMock, patch
        from click.testing import CliRunner
        from digitalmodel.hydrodynamics import diffraction as package

        cli_module = importlib.import_module(package.__name__ + ".cli")
        package.ORCAWAVE_AVAILABLE = True
        # Exercise the patch target actually declared by the interface-only test.
        tree = ast.parse(Path("tests/specialized/cli/test_diffraction_cli.py").read_text())
        conversion = next(
            node for node in ast.walk(tree)
            if isinstance(node, ast.FunctionDef)
            and node.name == "test_basic_orcawave_conversion"
        )
        target = next(
            decorator.args[0].value for decorator in conversion.decorator_list
            if isinstance(decorator, ast.Call)
            and isinstance(decorator.func, ast.Name)
            and decorator.func.id == "patch"
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
            with patch(target, side_effect=RuntimeError("converter reached")) as converter:
                result = runner.invoke(cli_module.cli, command + ["--water-depth", "100"])
                assert result.exit_code == 1, result.output
                assert "converter reached" in result.output
                converter.assert_called_once()
                if command[0] == "compare":
                    assert "Comparison mode: diagnostic; no acceptance policy supplied" in result.output
                    assert "Tolerance:" not in result.output
        assert attempts and set(attempts) == {{"OrcFxAPI"}}
    """,
        tmp_path,
        guard=_MISSING_INTERFACE,
    )


@pytest.mark.parametrize(
    "unavailable, converter_available, utilities_available",
    [
        ("orcawave_converter", False, True),
        ("orcawave_data_extraction", True, False),
        (None, True, True),
    ],
)
def test_flags_reflect_resolved_exports(
    unavailable, converter_available, utilities_available, tmp_path,
):
    """Explicit resolution uses import fixtures; offline numerical checks do not."""
    _run_guarded(
        f"""
        import importlib.machinery
        from pathlib import Path
        from click.testing import CliRunner
        from digitalmodel.hydrodynamics import diffraction as package
        from digitalmodel.hydrodynamics.diffraction import result_extractor, solver
        from digitalmodel.hydrodynamics.diffraction.batch_processor import BatchProcessor
        from digitalmodel.hydrodynamics.diffraction.cli import cli

        assert attempts == []

        class OptionalDependencyFinder(importlib.abc.MetaPathFinder, importlib.abc.Loader):
            def find_spec(self, fullname, path=None, target=None):
                if fullname == solver.__name__ + "." + str({unavailable!r}):
                    raise ImportError("optional converter dependency unavailable")
                if fullname == "OrcFxAPI" or fullname in {{
                    solver.__name__ + ".orcawave_converter",
                    solver.__name__ + ".orcawave_data_extraction",
                }}:
                    return importlib.machinery.ModuleSpec(fullname, self)
                return None

            def create_module(self, spec):
                return None

            def exec_module(self, module):
                for name in solver._LICENSED_EXPORTS:
                    setattr(module, name, object())

        sys.meta_path.insert(0, OptionalDependencyFinder())
        assert solver.OrcFxAPI is not None
        assert package.ORCAWAVE_AVAILABLE is {converter_available!r}
        assert package.TEST_UTILITIES_AVAILABLE is {utilities_available!r}
        assert result_extractor.CONVERTER_AVAILABLE is {converter_available!r}
        for name in ("OrcFxAPI", *solver._LICENSED_EXPORTS):
            value = getattr(solver, name)
            assert name in vars(solver)
            assert vars(solver)[name] is value
            assert getattr(solver, name) is value
        if not {converter_available!r}:
            model = Path({str(tmp_path / "model.sim")!r})
            model.write_text("test input")
            runner = CliRunner()
            for command in (
                ["convert-orcawave", str(model)],
                ["compare", str(model.parent), str(model), "test"],
                ["plot-raos", str(model)],
            ):
                result = runner.invoke(cli, command + ["--water-depth", "100"])
                assert result.exit_code == 1, result.output
                assert "OrcaWave converter unavailable" in result.output
                assert "OrcFxAPI or a converter dependency failed to import" in result.output
                assert "NoneType" not in result.output
            try:
                BatchProcessor([])._process_orcawave(None)
            except ImportError as exc:
                assert "OrcaWave converter unavailable" in str(exc)
                assert "OrcFxAPI or a converter dependency failed to import" in str(exc)
            else:
                raise AssertionError("batch conversion accepted an unavailable converter")
    """,
        tmp_path,
    )


def test_flags_reject_exports_when_first_read_after_teardown(tmp_path):
    """Reject cached converter exports when the interface is already absent."""
    _check_flags_after_interface_teardown(False, tmp_path)


def test_flags_keep_interface_cached_during_stand_in_lifetime(tmp_path):
    """Record the documented stale-cache limitation, not a required contract."""
    _check_flags_after_interface_teardown(True, tmp_path)


def _check_flags_after_interface_teardown(read_before_teardown, tmp_path):
    """Check the two access orders in separate interpreters."""
    _run_guarded(
        f"""
        import importlib
        from types import ModuleType
        from digitalmodel.hydrodynamics import diffraction as package
        from digitalmodel.hydrodynamics.diffraction import result_extractor, solver

        # Model the converter tests' temporary interface and module imports.
        sys.modules["OrcFxAPI"] = ModuleType("OrcFxAPI")
        converter = importlib.import_module(solver.__name__ + ".orcawave_converter")
        extractor = importlib.import_module(solver.__name__ + ".orcawave_data_extraction")
        assert solver.OrcaWaveConverter is converter.OrcaWaveConverter
        assert solver.OrcaWaveDataExtractor is extractor.OrcaWaveDataExtractor
        if {read_before_teardown!r}:
            assert package.ORCAWAVE_AVAILABLE is True
            assert package.TEST_UTILITIES_AVAILABLE is True
            assert result_extractor.CONVERTER_AVAILABLE is True
            assert solver.OrcFxAPI is sys.modules["OrcFxAPI"]
        del sys.modules["OrcFxAPI"]

        assert (solver.OrcFxAPI is not None) is {read_before_teardown!r}
        for _ in range(2):
            assert package.ORCAWAVE_AVAILABLE is {read_before_teardown!r}
            assert package.TEST_UTILITIES_AVAILABLE is {read_before_teardown!r}
            assert result_extractor.CONVERTER_AVAILABLE is {read_before_teardown!r}
        assert attempts and set(attempts) == {{"OrcFxAPI"}}
    """,
        tmp_path,
        guard=_MISSING_INTERFACE,
    )


@pytest.mark.parametrize(
    "export",
    [
        "OrcFxAPI", "OrcaWaveConverter", "convert_orcawave_results",
        "OrcaWaveDataExtractor", "extract_all_rao_data",
        "extract_all_added_mass", "extract_all_damping",
    ],
)
def test_solver_caches_unavailable_exports(export, tmp_path):
    _run_guarded(
        f"""
        from digitalmodel.hydrodynamics import diffraction as package
        from digitalmodel.hydrodynamics.diffraction import solver

        requests = []
        requested_module = (
            "OrcFxAPI" if {export!r} == "OrcFxAPI"
            else solver.__name__ + solver._LICENSED_EXPORTS[{export!r}]
        )

        class MissingDependencyFinder(importlib.abc.MetaPathFinder):
            def find_spec(self, fullname, path=None, target=None):
                if fullname == requested_module:
                    requests.append(fullname)
                    raise ImportError("optional dependency unavailable")
                return None

        sys.meta_path.insert(0, MissingDependencyFinder())
        assert getattr(solver, {export!r}) is None
        assert getattr(solver, {export!r}) is None
        if {export!r} != "OrcFxAPI":
            assert getattr(package, {export!r}) is None
        assert requests == [requested_module], requests
        assert {export!r} in vars(solver)
        assert attempts == []
    """,
        tmp_path,
    )
