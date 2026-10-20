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


def _run_guarded(code: str, tmp_path: Path) -> None:
    env = os.environ.copy()
    env["PYTHONPATH"] = os.pathsep.join([str(ROOT / "src"), env.get("PYTHONPATH", "")])
    # This is a harness timeout, not an engineering acceptance limit.
    result = subprocess.run(
        [sys.executable, "-c", _GUARD + textwrap.dedent(code)],
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
