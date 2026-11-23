"""
Tests for forward_speed.py — encounter frequency, wave number,
RAO speed correction, and strip-theory corrections.

References: DNV-RP-C205 §7.4, Salvesen-Tuck-Faltinsen (1970).
"""

from __future__ import annotations

from types import SimpleNamespace
from unittest.mock import patch

import numpy as np
import pytest

from digitalmodel.hydrodynamics.parametric_hull_analysis.forward_speed import (
    correct_rao_for_speed,
    encounter_frequency,
    strip_theory_speed_correction,
    wave_number,
)

G = 9.81  # gravitational acceleration [m/s²]


# ===================================================================
# wave_number tests
# ===================================================================


class TestWaveNumber:
    """Tests for the linear dispersion relation solver."""

    def test_deep_water_limit(self):
        """Deep water: k = omega^2 / g exactly."""
        omega = np.array([0.5, 1.0, 1.5, 2.0])
        k = wave_number(omega, water_depth=np.inf)
        expected = omega**2 / G
        np.testing.assert_allclose(k, expected, rtol=1e-10)

    def test_deep_water_default(self):
        """Default water_depth is inf (deep water)."""
        omega = np.array([1.0])
        k_default = wave_number(omega)
        k_inf = wave_number(omega, water_depth=np.inf)
        np.testing.assert_array_equal(k_default, k_inf)

    def test_finite_depth_greater_than_deep(self):
        """Finite-depth wave number > deep-water wave number."""
        omega = np.array([0.8, 1.0, 1.2])
        k_deep = wave_number(omega, water_depth=np.inf)
        k_shallow = wave_number(omega, water_depth=10.0)
        assert np.all(k_shallow >= k_deep), (
            "Finite depth k should be >= deep water k"
        )

    def test_very_deep_water_converges_to_deep(self):
        """Very large depth converges to deep-water formula."""
        omega = np.array([0.5, 1.0, 1.5])
        k_deep = wave_number(omega, water_depth=np.inf)
        k_very_deep = wave_number(omega, water_depth=1e6)
        np.testing.assert_allclose(k_very_deep, k_deep, rtol=1e-6)

    def test_dispersion_relation_satisfied(self):
        """Newton solution satisfies omega^2 = g * k * tanh(k*h)."""
        omega = np.array([0.5, 0.8, 1.0, 1.5])
        h = 20.0
        k = wave_number(omega, water_depth=h)
        lhs = omega**2
        rhs = G * k * np.tanh(k * h)
        np.testing.assert_allclose(lhs, rhs, rtol=1e-9)

    def test_scalar_input(self):
        """Scalar omega should work (via atleast_1d)."""
        k = wave_number(1.0, water_depth=np.inf)
        assert k.shape == (1,)
        np.testing.assert_allclose(k[0], 1.0**2 / G)

    def test_zero_omega(self):
        """omega=0 should give k=0."""
        k = wave_number(np.array([0.0]))
        np.testing.assert_allclose(k, [0.0], atol=1e-10)

    def test_shallow_water_different_depths(self):
        """Shallower water should give larger k for same omega."""
        omega = np.array([1.0])
        k_50 = wave_number(omega, water_depth=50.0)
        k_5 = wave_number(omega, water_depth=5.0)
        assert k_5[0] > k_50[0], "Shallower water should have larger k"

    def test_custom_gravity(self):
        """Custom g changes result correctly."""
        omega = np.array([1.0])
        k_earth = wave_number(omega, g=9.81)
        k_moon = wave_number(omega, g=1.62)
        # k = omega^2 / g, so smaller g → larger k
        assert k_moon[0] > k_earth[0]


# ===================================================================
# encounter_frequency tests
# ===================================================================


class TestEncounterFrequency:
    """Tests for encounter frequency transformation."""

    def test_head_seas_increases_frequency(self):
        """Head seas (beta=pi): omega_e > omega for forward speed.

        The formula is omega_e = omega - k*U*cos(beta).
        With beta=pi, cos(pi)=-1, so omega_e = omega + k*U > omega.
        (Standard seakeeping convention: beta=180° is head seas.)
        """
        omega = np.array([1.0])
        heading = np.array([np.pi])  # head seas
        omega_e = encounter_frequency(omega, forward_speed=5.0, heading_rad=heading)
        assert omega_e[0, 0] > omega[0], (
            "Encounter freq should exceed wave freq in head seas"
        )

    def test_following_seas_decreases_frequency(self):
        """Following seas (beta=0): omega_e < omega for forward speed.

        With beta=0, cos(0)=1, so omega_e = omega - k*U < omega.
        """
        omega = np.array([1.0])
        heading = np.array([0.0])  # following seas
        omega_e = encounter_frequency(omega, forward_speed=5.0, heading_rad=heading)
        assert omega_e[0, 0] < omega[0], (
            "Encounter freq should be less than wave freq in following seas"
        )

    def test_beam_seas_unchanged(self):
        """Beam seas (beta=pi/2): omega_e ≈ omega (cos(pi/2)=0)."""
        omega = np.array([1.0])
        heading = np.array([np.pi / 2])
        omega_e = encounter_frequency(omega, forward_speed=5.0, heading_rad=heading)
        np.testing.assert_allclose(omega_e[0, 0], omega[0], atol=1e-10)

    def test_zero_speed_no_change(self):
        """Zero speed: omega_e = omega for all headings."""
        omega = np.array([0.5, 1.0, 1.5])
        headings = np.array([0.0, np.pi / 2, np.pi])
        omega_e = encounter_frequency(omega, forward_speed=0.0, heading_rad=headings)
        for j in range(len(headings)):
            np.testing.assert_allclose(omega_e[:, j], omega, atol=1e-12)

    def test_output_shape(self):
        """Output shape is (n_omega, n_heading)."""
        omega = np.array([0.5, 1.0, 1.5, 2.0])
        headings = np.array([0.0, np.pi / 4, np.pi / 2])
        omega_e = encounter_frequency(omega, forward_speed=3.0, heading_rad=headings)
        assert omega_e.shape == (4, 3)

    def test_formula_deep_water(self):
        """Verify formula omega_e = omega - k*U*cos(beta) in deep water."""
        omega = np.array([1.0])
        U = 5.0
        heading = np.array([np.pi / 4])
        k = omega**2 / G
        expected = omega[0] - k[0] * U * np.cos(heading[0])
        omega_e = encounter_frequency(omega, U, heading, water_depth=np.inf)
        np.testing.assert_allclose(omega_e[0, 0], expected, rtol=1e-10)

    def test_multiple_headings_physics(self):
        """Head seas (pi) > beam seas (pi/2) > following seas (0) for encounter freq."""
        omega = np.array([1.0])
        # following=0, beam=pi/2, head=pi
        headings = np.array([0.0, np.pi / 2, np.pi])
        omega_e = encounter_frequency(omega, forward_speed=3.0, heading_rad=headings)
        assert omega_e[0, 2] > omega_e[0, 1], "Head (pi) > Beam (pi/2)"
        assert omega_e[0, 1] > omega_e[0, 0], "Beam (pi/2) > Following (0)"


# ===================================================================
# strip_theory_speed_correction tests
# ===================================================================


class TestStripTheorySpeedCorrection:
    """Tests for Salvesen-Tuck-Faltinsen speed corrections."""

    @staticmethod
    def _make_matrices(n_omega=5):
        """Create synthetic 6-DOF added mass and damping matrices."""
        rng = np.random.default_rng(42)
        A = rng.uniform(100, 1000, (n_omega, 6, 6))
        B = rng.uniform(10, 100, (n_omega, 6, 6))
        omegas = np.linspace(0.3, 2.0, n_omega)
        return A, B, omegas

    def test_zero_speed_unchanged(self):
        """Zero speed returns exact copies of input."""
        A, B, omegas = self._make_matrices()
        A_corr, B_corr = strip_theory_speed_correction(A, B, omegas, 0.0, 100.0)
        np.testing.assert_array_equal(A_corr, A)
        np.testing.assert_array_equal(B_corr, B)

    def test_heave_pitch_coupling_modified(self):
        """Non-zero speed modifies A[2,4] (heave-pitch coupling)."""
        A, B, omegas = self._make_matrices()
        A_corr, B_corr = strip_theory_speed_correction(A, B, omegas, 5.0, 100.0)
        # At least one frequency should have changed A[2,4]
        assert not np.allclose(A_corr[:, 2, 4], A[:, 2, 4])

    def test_surge_pitch_coupling_modified(self):
        """Non-zero speed modifies A[0,4] (surge-pitch coupling)."""
        A, B, omegas = self._make_matrices()
        A_corr, _ = strip_theory_speed_correction(A, B, omegas, 5.0, 100.0)
        assert not np.allclose(A_corr[:, 0, 4], A[:, 0, 4])

    def test_sway_yaw_coupling_modified(self):
        """Non-zero speed modifies A[1,5] (sway-yaw coupling)."""
        A, B, omegas = self._make_matrices()
        A_corr, _ = strip_theory_speed_correction(A, B, omegas, 5.0, 100.0)
        assert not np.allclose(A_corr[:, 1, 5], A[:, 1, 5])

    def test_correction_formula_heave_pitch(self):
        """Verify A_35(U) = A_35(0) - U * A_33(0) / omega exactly."""
        n = 3
        A = np.ones((n, 6, 6)) * 10.0
        B = np.ones((n, 6, 6)) * 1.0
        omegas = np.array([0.5, 1.0, 2.0])
        U = 3.0
        A_corr, _ = strip_theory_speed_correction(A, B, omegas, U, 100.0)
        for i, w in enumerate(omegas):
            expected = 10.0 - U * 10.0 / w  # A_35(0) - U * A_33(0) / w
            np.testing.assert_allclose(A_corr[i, 2, 4], expected, rtol=1e-12)

    def test_does_not_mutate_input(self):
        """Input arrays should not be modified in place."""
        A, B, omegas = self._make_matrices()
        A_orig = A.copy()
        B_orig = B.copy()
        strip_theory_speed_correction(A, B, omegas, 5.0, 100.0)
        np.testing.assert_array_equal(A, A_orig)
        np.testing.assert_array_equal(B, B_orig)

    def test_correction_proportional_to_speed(self):
        """Doubling speed doubles the correction delta."""
        A, B, omegas = self._make_matrices()
        A_u1, _ = strip_theory_speed_correction(A, B, omegas, 2.0, 100.0)
        A_u2, _ = strip_theory_speed_correction(A, B, omegas, 4.0, 100.0)
        delta_1 = A_u1[:, 2, 4] - A[:, 2, 4]
        delta_2 = A_u2[:, 2, 4] - A[:, 2, 4]
        np.testing.assert_allclose(delta_2, 2.0 * delta_1, rtol=1e-10)

    def test_diagonal_elements_unchanged(self):
        """Diagonal added mass elements should not be modified."""
        A, B, omegas = self._make_matrices()
        A_corr, _ = strip_theory_speed_correction(A, B, omegas, 5.0, 100.0)
        for dof in range(6):
            np.testing.assert_array_equal(A_corr[:, dof, dof], A[:, dof, dof])


# ===================================================================
# correct_rao_for_speed tests (with mock RAOResult)
# ===================================================================


class TestCorrectRaoForSpeed:
    """Tests for RAO encounter-frequency remapping."""

    @staticmethod
    def _mock_rao_result():
        """Create a minimal mock RAOResult object."""
        n_omega, n_heading, n_dof = 20, 3, 6
        omegas = np.linspace(0.3, 2.0, n_omega)
        headings = np.array([0.0, np.pi / 2, np.pi])
        # Simple synthetic RAO: amplitude = 1.0 everywhere
        amp = np.ones((n_omega, n_heading, n_dof))
        phase = np.zeros((n_omega, n_heading, n_dof))
        return SimpleNamespace(
            omegas=omegas,
            headings=headings,
            rao_amplitude=amp,
            rao_phase=phase,
            dof_names=["surge", "sway", "heave", "roll", "pitch", "yaw"],
        )

    def test_zero_speed_returns_same_freqs(self):
        """Zero speed should produce encounter freqs ~ wave freqs."""
        mock = self._mock_rao_result()
        # Patch the RAOResult import inside correct_rao_for_speed
        FakeRAOResult = type("RAOResult", (), {"__init__": lambda self, **kw: self.__dict__.update(kw)})
        with patch(
            "digitalmodel.hydrodynamics.parametric_hull_analysis.forward_speed.RAOResult",
            FakeRAOResult,
            create=True,
        ):
            # The function imports RAOResult from capytaine.models, we need
            # to patch it at the import point
            import digitalmodel.hydrodynamics.capytaine.models as cap_mod
            original = getattr(cap_mod, "RAOResult", None)
            cap_mod.RAOResult = FakeRAOResult
            try:
                result = correct_rao_for_speed(mock, forward_speed=0.0)
                # Encounter freqs should be close to original freqs
                np.testing.assert_allclose(
                    result.omegas[0], mock.omegas[0], rtol=0.05
                )
            finally:
                if original is not None:
                    cap_mod.RAOResult = original

    def test_none_data_returns_unchanged(self):
        """RAO with None data returns the same object."""
        mock = SimpleNamespace(
            omegas=None,
            headings=None,
            rao_amplitude=None,
            rao_phase=None,
            dof_names=None,
        )
        result = correct_rao_for_speed(mock, forward_speed=5.0)
        assert result is mock
