"""Tests for orcawave.hydro_coefficients module."""

import numpy as np
import pytest

from digitalmodel.orcawave.hydro_coefficients import (
    ExcitationForceVector,
    HydroCoefficients,
    HydroMatrix6x6,
    create_hydrostatic_restoring,
    from_wamit_added_mass,
    interpolate_excitation_at_frequency,
    interpolate_matrix_at_frequency,
    to_wamit_added_mass,
    to_wamit_damping,
)


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

    def test_matrix_symmetry(self):
        vals = np.eye(6) * 200.0
        vals[0, 1] = vals[1, 0] = 30.0
        m = HydroMatrix6x6(frequency_rad_s=0.5, values=vals.tolist())
        arr = m.to_numpy()
        np.testing.assert_allclose(arr, arr.T, atol=1e-10)


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
        text = to_wamit_added_mass(coeffs)
        matrices = from_wamit_added_mass(text)
        assert len(matrices) == len(coeffs.added_mass)
        # Check first matrix diagonal
        orig = coeffs.added_mass[0].to_numpy()
        parsed = matrices[0].to_numpy()
        np.testing.assert_allclose(parsed[0, 0], orig[0, 0], rtol=1e-4)

    def test_wamit_damping_export(self):
        coeffs = _make_coefficients()
        text = to_wamit_damping(coeffs)
        lines = [l for l in text.strip().splitlines() if l.strip()]
        assert len(lines) > 0

    def test_hydrostatic_restoring(self):
        c = create_hydrostatic_restoring(
            displaced_volume_m3=10000.0,
            waterplane_area_m2=3000.0,
            gm_t=2.0,
            gm_l=100.0,
        )
        arr = np.array(c)
        assert arr.shape == (6, 6)
        # Heave restoring > 0
        assert arr[2, 2] > 0
        # Roll restoring > 0
        assert arr[3, 3] > 0
        # Pitch restoring > 0
        assert arr[4, 4] > 0
        # Surge restoring = 0
        assert arr[0, 0] == 0.0
