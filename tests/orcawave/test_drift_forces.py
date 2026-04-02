"""Tests for orcawave.drift_forces module."""

import numpy as np
import pytest

from digitalmodel.orcawave.drift_forces import (
    MeanDriftCoefficients,
    QTFMatrix,
    compute_mean_drift_force,
    compute_wind_current_drift,
    full_qtf_slowly_varying,
    newman_approximation,
)
from digitalmodel.orcawave.wave_spectrum import pierson_moskowitz


def _make_drift_coeffs() -> MeanDriftCoefficients:
    """Helper: create realistic drift coefficients."""
    freqs = [0.3, 0.4, 0.5, 0.6, 0.8, 1.0, 1.2, 1.5]
    # Typical mean drift: increases with frequency squared (short waves)
    surge = [10 * f**2 for f in freqs]
    sway = [8 * f**2 for f in freqs]
    yaw = [50 * f**2 for f in freqs]
    return MeanDriftCoefficients(
        heading_deg=0.0,
        frequencies_rad_s=freqs,
        surge_kn_m2=surge,
        sway_kn_m2=sway,
        yaw_knm_m2=yaw,
    )


class TestMeanDriftForce:
    """Test mean drift force computation."""

    def test_positive_drift(self):
        coeffs = _make_drift_coeffs()
        omega = np.linspace(0.1, 2.0, 200)
        s_wave = pierson_moskowitz(omega, hs=3.0, tp=10.0)
        surge, sway, yaw = compute_mean_drift_force(coeffs, s_wave, omega)
        assert surge > 0  # positive drift coefficients -> positive force
        assert sway > 0

    def test_zero_spectrum_zero_drift(self):
        coeffs = _make_drift_coeffs()
        omega = np.linspace(0.1, 2.0, 200)
        s_wave = np.zeros_like(omega)
        surge, sway, yaw = compute_mean_drift_force(coeffs, s_wave, omega)
        np.testing.assert_allclose(surge, 0.0, atol=1e-15)

    def test_larger_hs_larger_drift(self):
        coeffs = _make_drift_coeffs()
        omega = np.linspace(0.1, 2.0, 200)
        s1 = pierson_moskowitz(omega, hs=2.0, tp=10.0)
        s2 = pierson_moskowitz(omega, hs=5.0, tp=10.0)
        surge1, _, _ = compute_mean_drift_force(coeffs, s1, omega)
        surge2, _, _ = compute_mean_drift_force(coeffs, s2, omega)
        assert surge2 > surge1


class TestNewmanApproximation:
    """Test Newman's approximation for slowly-varying forces."""

    def test_sv_spectrum_positive(self):
        coeffs = _make_drift_coeffs()
        omega = np.linspace(0.1, 2.0, 50)
        s_wave = pierson_moskowitz(omega, hs=3.0, tp=10.0)
        sv = newman_approximation(coeffs, s_wave, omega, dof="surge")
        assert np.all(sv >= 0)

    def test_sv_spectrum_length(self):
        coeffs = _make_drift_coeffs()
        omega = np.linspace(0.1, 2.0, 50)
        s_wave = pierson_moskowitz(omega, hs=3.0, tp=10.0)
        sv = newman_approximation(coeffs, s_wave, omega, dof="surge")
        assert len(sv) == len(omega)

    def test_sv_zero_spectrum(self):
        coeffs = _make_drift_coeffs()
        omega = np.linspace(0.1, 2.0, 50)
        s_wave = np.zeros_like(omega)
        sv = newman_approximation(coeffs, s_wave, omega, dof="surge")
        np.testing.assert_allclose(sv, 0.0, atol=1e-15)


class TestFullQTF:
    """Test full QTF slowly-varying computation."""

    def test_full_qtf_positive_std(self):
        freqs = [0.3, 0.5, 0.8, 1.0]
        n = len(freqs)
        # Create simple diagonal-dominant QTF
        real_part = np.eye(n) * 10.0
        imag_part = np.zeros((n, n))
        qtf = QTFMatrix(
            dof="surge",
            heading_deg=0.0,
            frequencies_rad_s=freqs,
            real_part=real_part.tolist(),
            imag_part=imag_part.tolist(),
        )
        omega = np.array(freqs)
        s_wave = pierson_moskowitz(omega, hs=3.0, tp=10.0)
        sigma = full_qtf_slowly_varying(qtf, s_wave, omega)
        assert sigma >= 0


class TestWindCurrentDrift:
    """Test wind and current drift estimation."""

    def test_head_wind(self):
        result = compute_wind_current_drift(
            wind_speed_m_s=20.0,
            current_speed_m_s=1.0,
            heading_deg=0.0,
        )
        assert result.wind_force_surge_kn != 0
        assert result.total_surge_kn != 0

    def test_beam_wind_sway(self):
        result = compute_wind_current_drift(
            wind_speed_m_s=20.0,
            current_speed_m_s=0.0,
            heading_deg=90.0,
        )
        # At 90 deg, sway should be dominant
        assert abs(result.wind_force_sway_kn) > abs(result.wind_force_surge_kn)

    def test_zero_wind_zero_force(self):
        result = compute_wind_current_drift(
            wind_speed_m_s=0.0,
            current_speed_m_s=0.0,
            heading_deg=0.0,
        )
        np.testing.assert_allclose(result.total_surge_kn, 0.0)
        np.testing.assert_allclose(result.total_sway_kn, 0.0)

    def test_higher_wind_higher_force(self):
        r1 = compute_wind_current_drift(10.0, 0.0, 0.0)
        r2 = compute_wind_current_drift(30.0, 0.0, 0.0)
        assert abs(r2.wind_force_surge_kn) > abs(r1.wind_force_surge_kn)
