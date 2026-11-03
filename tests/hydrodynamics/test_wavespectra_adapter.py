# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic regressions for angular-frequency to wavespectra conversion.

All numerical inputs are artificial test data. The expected variances below
are independent trapezoid hand sums, not outputs of a spectrum generator.
The conversion uses the definition of one cycle as two pi radians.
"""

import doctest
import math
import os
from pathlib import Path
import shlex
import subprocess
import sys
import tomllib

import numpy as np
from packaging.requirements import Requirement
import pytest
import yaml

from tests.scripts._pytest_selection import pytest_selects_target


def assert_wavespectra_command(gate, path, pytestconfig):
    assert gate["enabled"]
    assert pytest_selects_target(
        pytestconfig, gate["command"], path, [path], require_explicit_target=True,
    )
    assert ".[test]" in shlex.split(gate["command"])


def test_wavespectra_command_guard_rejects_disabled_gate(pytestconfig):
    path = "tests/hydrodynamics/test_wavespectra_adapter.py"
    gate = {
        "enabled": False,
        "command": f"uv run --with-editable '.[test]' python -m pytest {path}",
    }
    with pytest.raises(AssertionError):
        assert_wavespectra_command(gate, path, pytestconfig)


@pytest.mark.parametrize("surface", ["ownership", "test_roots", "command"])
def test_wavespectra_adapter_runs_in_hydrodynamics_ci(surface, pytestconfig):
    root = Path(__file__).resolve().parents[2]
    path = Path(__file__).resolve().relative_to(root).as_posix()
    if surface == "ownership":
        rows = (root / "tests/DOMAINS.md").read_text(encoding="utf-8").splitlines()
        owners = [
            row.split("|")[1].strip()
            for row in rows
            if row.startswith("|") and f"`{path}`" in row
        ]
        assert owners == ["hydrodynamics-other"]
        return

    gates = yaml.safe_load(
        (root / ".claude/quality-gates.yaml").read_text(encoding="utf-8")
    )["gates"]
    gate = gates["tests-hydrodynamics-other"]
    assert gate["enabled"]
    if surface == "test_roots":
        owners = [
            name for name, item in gates.items()
            if any(path == entry or path.startswith(entry.rstrip("/") + "/")
                   for entry in item.get("test_roots", []))
        ]
        assert owners == ["tests-hydrodynamics-other"]
    else:
        assert_wavespectra_command(gate, path, pytestconfig)


def test_wavespectra_is_a_test_only_dependency():
    root = Path(__file__).resolve().parents[2]
    project = tomllib.loads(
        (root / "pyproject.toml").read_text(encoding="utf-8")
    )["project"]
    tests = {Requirement(item).name for item in project["optional-dependencies"]["test"]}
    runtime = {Requirement(item).name for item in project["dependencies"]}
    assert "wavespectra" in tests
    assert "wavespectra" not in runtime


@pytest.mark.parametrize(
    "omega, expected_m0",
    [
        # Half endpoint sums times widths: 1 + 1.5 + 0.5 = 3.
        ([1.0, 2.0, 3.0, 4.0], 3.0),
        # Nonuniform widths: 1 + 3 + 1.5 = 5.5.
        ([1.0, 2.0, 4.0, 7.0], 5.5),
    ],
)
def test_wavespectra_adapter_preserves_hand_integrated_m0(omega, expected_m0):
    from digitalmodel.hydrodynamics.wavespectra_adapter import to_wavespectra

    density = [0.0, 2.0, 1.0, 0.0]
    spectrum = to_wavespectra(omega, density)

    def hand_sum(grid, values):
        return sum(
            (right - left) * (lower + upper) / 2
            for left, right, lower, upper in zip(
                grid[:-1], grid[1:], values[:-1], values[1:]
            )
        )

    assert hand_sum(omega, density) == pytest.approx(expected_m0)
    assert hand_sum(spectrum.freq.values, spectrum.values) == pytest.approx(
        expected_m0
    )


def test_wavespectra_adapter_units_and_accessor():
    import xarray as xr
    from wavespectra.specarray import SpecArray

    from digitalmodel.hydrodynamics.wavespectra_adapter import to_wavespectra

    # Synthetic irregular grid and densities; one cycle = two pi radians.
    omega = np.array([1.0, 2.0, 4.0, 7.0])
    density = np.array([0.0, 2.0, 1.0, 0.0])
    spectrum = to_wavespectra(omega, density)
    assert isinstance(spectrum, xr.DataArray)
    assert spectrum.dims == ("freq",)
    assert spectrum.name == "ef"
    assert spectrum.freq.attrs == {
        "standard_name": "sea_surface_wave_frequency",
        "units": "Hz",
    }
    assert spectrum.attrs == {
        "standard_name": "sea_surface_wave_variance_spectral_density",
        "units": "m2 s",
    }
    np.testing.assert_allclose(spectrum.freq.values, omega / (2 * math.pi))
    np.testing.assert_allclose(spectrum.values, density * (2 * math.pi))
    assert isinstance(spectrum.spec, SpecArray)
    xr.testing.assert_identical(spectrum.spec.oned(), spectrum)
    # Conversion preserves caller order and does not mutate the inputs.
    np.testing.assert_array_equal(omega, [1.0, 2.0, 4.0, 7.0])
    np.testing.assert_array_equal(density, [0.0, 2.0, 1.0, 0.0])


@pytest.mark.parametrize(
    "omega, density",
    [
        ([1.0, 2.0], [1.0]),
        ([], []),
        ([1.0], [1.0]),
        (1.0, 1.0),
        ([[1.0, 2.0]], [1.0, 1.0]),
        ([1.0, 2.0], [[1.0, 1.0]]),
        ([[1.0], [2.0]], [[1.0], [1.0]]),
        ([0.0, 1.0], [1.0, 1.0]),
        ([-1.0, 1.0], [1.0, 1.0]),
        ([2.0, 1.0], [1.0, 1.0]),
        ([1.0, 1.0], [1.0, 1.0]),
        ([1.0, float("nan")], [1.0, 1.0]),
        ([1.0, float("inf")], [1.0, 1.0]),
        ([float("-inf"), 1.0], [1.0, 1.0]),
        ([1.0, 2.0], [-1.0, 1.0]),
        ([1.0, 2.0], [float("nan"), 1.0]),
        ([1.0, 2.0], [float("inf"), 1.0]),
        ([1.0, 2.0], [float("-inf"), 1.0]),
        ([1.0, 2.0], ["invalid", 1.0]),
        (["invalid", 2.0], [1.0, 1.0]),
        ([1.0 + 1.0j, 2.0], [1.0, 1.0]),
        ([1.0, 2.0], [1.0 + 1.0j, 1.0]),
        ([True, True], [1.0, 1.0]),
        ([1.0, 2.0], [True, False]),
    ],
)
def test_wavespectra_adapter_invalid_arrays(omega, density):
    from digitalmodel.hydrodynamics.wavespectra_adapter import to_wavespectra

    with pytest.raises(ValueError):
        to_wavespectra(omega, density)


def test_wavespectra_adapter_zero_density():
    from digitalmodel.hydrodynamics.wavespectra_adapter import to_wavespectra

    spectrum = to_wavespectra([1.0, 2.0], [0.0, 0.0])
    np.testing.assert_array_equal(spectrum.values, [0.0, 0.0])


@pytest.mark.parametrize(
    "omega, density",
    [
        # Machine limits, derived from float64 rather than physical constants.
        ([1.0, 2.0], [np.finfo(float).max, 0.0]),
        ([np.nextafter(0.0, 1.0), 1.0], [1.0, 1.0]),
    ],
)
def test_wavespectra_adapter_rejects_unrepresentable_conversion(omega, density):
    from digitalmodel.hydrodynamics.wavespectra_adapter import to_wavespectra

    with pytest.raises(ValueError, match="represent"):
        to_wavespectra(omega, density)


@pytest.mark.parametrize("missing", ["wavespectra", "xarray"])
def test_wavespectra_adapter_lazy_import_and_missing_dependency(missing, tmp_path):
    # A fresh interpreter prevents an earlier test's registered accessor from
    # concealing an eager optional import. No optional-package stub is used.
    code = '''
import importlib.abc
import sys

missing = sys.argv[1]

class BlockOptional(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split(".")[0] in {"wavespectra", "xarray"}:
            raise ModuleNotFoundError("blocked optional import", name=fullname)

sys.meta_path.insert(0, BlockOptional())
import digitalmodel
import digitalmodel.hydrodynamics
from digitalmodel.hydrodynamics.wavespectra_adapter import (
    MissingWavespectraDependencyError, to_wavespectra,
)
assert "wavespectra" not in sys.modules
assert "xarray" not in sys.modules

# Block just one dependency for the call, exercising each missing import.
sys.meta_path.pop(0)
class BlockOne(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split(".")[0] == missing:
            raise ModuleNotFoundError("blocked optional import", name=fullname)
sys.meta_path.insert(0, BlockOne())
try:
    to_wavespectra([1.0, 2.0], [0.0, 1.0])
except MissingWavespectraDependencyError as exc:
    assert "wavespectra" in str(exc)
    assert "xarray" in str(exc)
    assert "install" in str(exc)
else:
    raise AssertionError("missing dependency was accepted")
'''
    result = subprocess.run(
        [sys.executable, "-c", code, missing],
        check=False,
        capture_output=True,
        text=True,
        cwd=tmp_path,
        # The test command's relative PYTHONPATH must work outside the repo.
        env={
            **os.environ,
            "PYTHONPATH": str(Path(__file__).resolve().parents[2] / "src"),
        },
    )
    assert result.returncode == 0, result.stderr


def test_wavespectra_adapter_docstring_example():
    from digitalmodel.hydrodynamics import wavespectra_adapter

    result = doctest.testmod(wavespectra_adapter, raise_on_error=True)
    assert result.attempted > 0
    assert result.failed == 0


def test_wavespectra_adapter_registers_accessor_on_first_call(tmp_path):
    # Access the accessor before any test-side import of the optional packages.
    code = '''
import sys
from digitalmodel.hydrodynamics.wavespectra_adapter import to_wavespectra
assert "wavespectra" not in sys.modules
# Existing optional hydrodynamics initialization can already import xarray.
spectrum = to_wavespectra([1.0, 2.0], [0.0, 1.0])
assert spectrum.spec.oned().identical(spectrum)
assert "wavespectra" in sys.modules
assert "xarray" in sys.modules
'''
    result = subprocess.run(
        [sys.executable, "-c", code],
        check=False,
        capture_output=True,
        text=True,
        cwd=tmp_path,
        env={
            **os.environ,
            "PYTHONPATH": str(Path(__file__).resolve().parents[2] / "src"),
        },
    )
    assert result.returncode == 0, result.stderr
