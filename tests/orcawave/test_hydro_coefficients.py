# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Tests for orcawave.hydro_coefficients module."""

import os
from pathlib import Path
import re
import subprocess
import sys

import numpy as np
import pytest

from digitalmodel.orcawave.hydro_coefficients import (
    ExcitationForceVector,
    HydroCoefficients,
    HydroMatrix6x6,
    create_hydrostatic_restoring,
    from_native_added_mass,
    interpolate_excitation_at_frequency,
    interpolate_matrix_at_frequency,
    to_native_added_mass,
    to_native_damping,
)
from digitalmodel.orcawave.qa_gates import check_hydrodynamics
from digitalmodel.orcawave.rao_processing import RAOTable


def _make_matrix(freq: float, diag_val: float) -> HydroMatrix6x6:
    """Helper: create a diagonal 6x6 matrix at given frequency."""
    vals = (np.eye(6) * diag_val).tolist()
    return HydroMatrix6x6(frequency_rad_s=freq, values=vals)


def _make_coefficients() -> HydroCoefficients:
    """Helper: create a simple set of hydro coefficients."""
    freqs = [0.3, 0.5, 0.8, 1.0, 1.5]
    added_mass = [_make_matrix(f, 1e6 * (1.0 + 0.5 / f)) for f in freqs]
    damping = [_make_matrix(f, 5e4 * f) for f in freqs]
    return HydroCoefficients(
        body_name="TestBody",
        mass_kg=1e7,
        cog=[0.0, 0.0, -5.0],
        frequencies_rad_s=freqs,
        added_mass=added_mass,
        radiation_damping=damping,
    )


class TestHydroMatrix6x6:
    """Test the HydroMatrix6x6 model."""

    def test_to_numpy(self):
        m = _make_matrix(0.5, 100.0)
        arr = m.to_numpy()
        assert arr.shape == (6, 6)
        np.testing.assert_allclose(arr[0, 0], 100.0)
        np.testing.assert_allclose(arr[0, 1], 0.0)

    def test_frequency_positive(self):
        m = _make_matrix(0.1, 50.0)
        assert m.frequency_rad_s == 0.1

    def test_asymmetric_added_mass_is_refused_by_gate(self):
        """Reciprocity, sidecar C1/Q1; #2748 section 15.

        Assumed A12=30, A21=0 and diagonal=200 give error 30/200=15%,
        exceeding the declared pack tolerance of 2%. Raw input is retained
        for diagnostics, then refused for use by check_hydrodynamics.
        """
        vals = np.eye(6) * 200.0
        vals[0, 1] = 30.0
        m = HydroMatrix6x6(frequency_rad_s=0.5, values=vals.tolist())
        coefficients = HydroCoefficients(
            added_mass=[m], radiation_damping=[_make_matrix(0.5, 1)]
        )
        heave = RAOTable(
            dof="Heave", unit="m/m", periods=[30], headings_deg=[0],
            amplitudes=[[1]], phases_deg=[[0]],
        )
        with pytest.raises(ValueError, match="asymmetry"):
            check_hydrodynamics(coefficients, heave)


class TestInterpolation:
    """Test frequency-dependent matrix interpolation."""

    def test_interpolate_at_known_frequency(self):
        matrices = [_make_matrix(0.5, 100.0), _make_matrix(1.0, 200.0)]
        result = interpolate_matrix_at_frequency(matrices, 0.5)
        np.testing.assert_allclose(result[0, 0], 100.0, atol=1e-10)

    def test_interpolate_midpoint(self):
        matrices = [_make_matrix(0.5, 100.0), _make_matrix(1.0, 200.0)]
        result = interpolate_matrix_at_frequency(matrices, 0.75)
        np.testing.assert_allclose(result[0, 0], 150.0, atol=1e-10)

    def test_interpolate_clamp_below(self):
        matrices = [_make_matrix(0.5, 100.0), _make_matrix(1.0, 200.0)]
        result = interpolate_matrix_at_frequency(matrices, 0.1)
        np.testing.assert_allclose(result[0, 0], 100.0, atol=1e-10)

    def test_excitation_interpolation(self):
        forces = [
            ExcitationForceVector(
                frequency_rad_s=0.5,
                heading_deg=0.0,
                amplitudes=[100, 0, 200, 0, 50, 0],
                phases_deg=[0, 0, -30, 0, -10, 0],
            ),
            ExcitationForceVector(
                frequency_rad_s=1.0,
                heading_deg=0.0,
                amplitudes=[200, 0, 400, 0, 100, 0],
                phases_deg=[0, 0, -60, 0, -20, 0],
            ),
        ]
        amps, phases = interpolate_excitation_at_frequency(forces, 0.0, 0.75)
        assert len(amps) == 6
        # Surge amplitude should be between 100 and 200
        assert 100 < amps[0] < 200


class TestWAMITFormat:
    """Test WAMIT format conversion."""

    def test_wamit_roundtrip(self):
        coeffs = _make_coefficients()
        text = to_native_added_mass(coeffs)
        matrices = from_native_added_mass(text)
        assert len(matrices) == len(coeffs.added_mass)
        # Check first matrix diagonal
        orig = coeffs.added_mass[0].to_numpy()
        parsed = matrices[0].to_numpy()
        np.testing.assert_allclose(parsed[0, 0], orig[0, 0], rtol=1e-4)

    def test_wamit_damping_export(self):
        coeffs = _make_coefficients()
        text = to_native_damping(coeffs)
        lines = [line for line in text.strip().splitlines() if line.strip()]
        assert len(lines) > 0

    def test_hydrostatic_restoring(self):
        """C2, WAMIT 7.5 manual 3.1; assumed #2748 section 15 inputs.

        C33=1025*9.81*3000=30165750;
        C44=1025*9.81*10000*2=201105000;
        C55=1025*9.81*10000*100=10055250000, in SI units.
        rel=1e-12 is the issue acceptance, not an engineering tolerance.
        """
        c = create_hydrostatic_restoring(
            displaced_volume_m3=10000.0,
            waterplane_area_m2=3000.0,
            gm_t=2.0,
            gm_l=100.0,
        )
        arr = np.array(c)
        assert arr.shape == (6, 6)
        np.testing.assert_allclose(
            np.diag(arr)[2:5], [30165750, 201105000, 10055250000],
            rtol=1e-12, atol=0,
        )
        # Surge restoring = 0
        assert arr[0, 0] == 0.0


@pytest.mark.parametrize("row,reason", [
    ("1 3 3", "four columns"), ("1 3 3 4 extra", "four columns"),
    ("1 0 3 4", "indices"), ("1 3 7 4", "indices"),
    ("1 bad 3 4", "numeric"), ("nan 3 3 4", "finite"), ("1 3 3 inf", "finite"),
])
def test_native_parser_rejects_invalid_row_with_line_context(row, reason):
    with pytest.raises(ValueError, match=rf"line 3:.*{reason}"):
        from_native_added_mass("1 3 3 4\n\n" + row + "\n")


def test_native_parser_preserves_valid_rows_among_blank_lines():
    matrices = from_native_added_mass("\n1 3 3 4\n\n")
    assert len(matrices) == 1
    assert matrices[0].values[2][2] == 4


@pytest.mark.parametrize(
    "omega",
    [1.0, 2.0, 3.0, 4.5, 5.0],
    ids=["lower-knot", "interior-first", "middle-knot", "interior-second", "upper-knot"],
)
def test_interpolate_full_symmetric_affine_matrix(omega):
    """Check every entry against the example's independent hand calculation."""
    # Source of all synthetic constants: Hand calculation in
    # docs/examples/hydro-coefficient-interpolation.md. No solver data is used.
    intercept = np.array([
        [100, 12, 13, 14, 15, 16],
        [12, 200, 23, 24, 25, 26],
        [13, 23, 300, 34, 35, 36],
        [14, 24, 34, 400, 45, 46],
        [15, 25, 35, 45, 500, 56],
        [16, 26, 36, 46, 56, 600],
    ])
    slope = np.array([
        [2, -1, -2, -3, -4, -5],
        [-1, 4, -1, -2, -3, -4],
        [-2, -1, 6, -1, -2, -3],
        [-3, -2, -1, 8, -1, -2],
        [-4, -3, -2, -1, 10, -1],
        [-5, -4, -3, -2, -1, 12],
    ])
    matrices = [
        HydroMatrix6x6(
            frequency_rad_s=knot,
            values=(intercept + knot * slope).tolist(),
        )
        for knot in (1.0, 3.0, 5.0)
    ]
    # Scalar oracle: evaluate the stated affine law, not the interpolation rule.
    expected = np.array([
        [
            100 * i + 2 * i * omega if i == j
            else 10 * min(i, j) + max(i, j) + (min(i, j) - max(i, j)) * omega
            for j in range(1, 7)
        ]
        for i in range(1, 7)
    ])

    result = interpolate_matrix_at_frequency(matrices, omega)

    # Binary-exact inputs and arithmetic need no numerical tolerance here.
    np.testing.assert_array_equal(result, expected)
    np.testing.assert_array_equal(result, result.T)


def test_hydro_coefficient_interpolation_example(tmp_path):
    """Execute the published snippet in isolation without a licensed solver."""
    root = Path(__file__).resolve().parents[2]
    document = (root / "docs/examples/hydro-coefficient-interpolation.md").read_text(
        encoding="utf-8"
    )
    snippets = re.findall(r"```python\n(.*?)\n```", document, re.DOTALL)
    assert len(snippets) == 1
    guard = """
import importlib.abc
import sys

class RejectLicensedSolver(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split('.')[0] == 'OrcFxAPI':
            raise AssertionError('The example must not import a licensed solver')
        return None

sys.meta_path.insert(0, RejectLicensedSolver())
"""
    result = subprocess.run(
        [sys.executable, "-c", guard + snippets[0]],
        cwd=tmp_path,
        env={**os.environ, "PYTHONPATH": str(root / "src")},
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout.strip() == "Interpolation audit passed."
