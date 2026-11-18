"""Tests for orcawave.wave_spectrum module."""

import math

import numpy as np
import pytest
from scipy.integrate import quad

from digitalmodel.orcawave.wave_spectrum import (
    SpectrumParameters,
    bretschneider,
    compute_spectral_moments,
    generate_spectrum,
    issc_spectrum,
    jonswap,
    pierson_moskowitz,
    spectral_periods,
    heuristic_two_peak,
)

_trapezoid = getattr(np, "trapezoid", None) or np.trapz


class TestPiersonMoskowitz:
    """Test Pierson-Moskowitz spectrum."""

    def test_hs_preservation(self):
        omega = np.linspace(0.05, 3.0, 500)
        s = pierson_moskowitz(omega, hs=3.0, tp=10.0)
        m0 = _trapezoid(s, omega)
        hs_computed = 4.0 * np.sqrt(m0)
        np.testing.assert_allclose(hs_computed, 3.0, rtol=0.05)

    def test_spectrum_positive(self):
        omega = np.linspace(0.1, 2.0, 100)
        s = pierson_moskowitz(omega, hs=2.0, tp=8.0)
        assert np.all(s >= 0)

    def test_peak_near_tp(self):
        omega = np.linspace(0.1, 3.0, 500)
        s = pierson_moskowitz(omega, hs=2.0, tp=10.0)
        peak_idx = np.argmax(s)
        tp_computed = 2.0 * np.pi / omega[peak_idx]
        np.testing.assert_allclose(tp_computed, 10.0, rtol=0.15)


class TestJONSWAP:
    """Test JONSWAP spectrum."""

    def test_hs_preservation(self):
        omega = np.linspace(0.05, 3.0, 500)
        s = jonswap(omega, hs=4.0, tp=12.0, gamma=3.3)
        m0 = _trapezoid(s, omega)
        hs_computed = 4.0 * np.sqrt(m0)
        np.testing.assert_allclose(hs_computed, 4.0, rtol=0.05)

    def test_gamma_effect(self):
        omega = np.linspace(0.05, 3.0, 500)
        s1 = jonswap(omega, hs=3.0, tp=10.0, gamma=1.0)
        s3 = jonswap(omega, hs=3.0, tp=10.0, gamma=5.0)
        # Higher gamma = narrower peak = higher peak value
        assert np.max(s3) > np.max(s1)

    def test_gamma_1_similar_to_pm(self):
        omega = np.linspace(0.05, 3.0, 500)
        s_j = jonswap(omega, hs=3.0, tp=10.0, gamma=1.0)
        s_pm = pierson_moskowitz(omega, hs=3.0, tp=10.0)
        # With gamma=1 and renormalization, shapes should be similar
        m0_j = _trapezoid(s_j, omega)
        m0_pm = _trapezoid(s_pm, omega)
        np.testing.assert_allclose(m0_j, m0_pm, rtol=0.1)


class TestOtherSpectra:
    """Test Bretschneider, ISSC, and Torsethaugen."""

    def test_bretschneider_same_as_pm(self):
        omega = np.linspace(0.1, 3.0, 200)
        s_bs = bretschneider(omega, hs=2.5, tp=9.0)
        s_pm = pierson_moskowitz(omega, hs=2.5, tp=9.0)
        np.testing.assert_allclose(s_bs, s_pm, atol=1e-10)

    def test_issc_positive(self):
        omega = np.linspace(0.1, 3.0, 200)
        s = issc_spectrum(omega, hs=3.0, tz=7.0)
        assert np.all(s >= 0)
        m0 = _trapezoid(s, omega)
        assert m0 > 0

    @pytest.mark.parametrize("tp", [8.0, 16.0])
    def test_heuristic_two_peak_independent_ordinates(self, tp):
        # Scalar equations and adaptive quadrature do not call spectrum helpers.
        hs = 4.0
        threshold = 6.6 * hs ** (1 / 3)
        components = (
            [(hs, tp, 3.3), (hs / 10, threshold, 1.0)]
            if tp <= threshold
            else [(hs / 10, threshold, 3.3), (hs, tp, 1.0)]
        )

        def density(w):
            result = 0.0
            for height, period, gamma in components:
                peak = 2 * math.pi / period
                sigma = 0.07 if w <= peak else 0.09
                enhancement = gamma ** math.exp(
                    -0.5 * ((w - peak) / (sigma * peak)) ** 2
                )
                result += (
                    (1 - 0.287 * math.log(gamma)) * 5 / 16 * height**2
                    * peak**4 / w**5 * math.exp(-1.25 * (peak / w)**4)
                    * enhancement
                )
            return result

        omega = np.linspace(0.05, 3.0, 20000)
        variance, error = quad(
            density,
            omega[0],
            omega[-1],
            points=[2 * math.pi / period for _, period, _ in components],
            epsabs=1e-10,
        )
        assert error < 1e-8
        actual = heuristic_two_peak(omega, hs=hs, tp=tp)
        indices = np.searchsorted(omega, [0.3, 0.4, 0.5, 0.6, 0.8, 1.0, 1.5])
        expected = [density(omega[i]) * (hs**2 / 16) / variance for i in indices]
        np.testing.assert_allclose(actual[indices], expected, rtol=1e-6)


class TestSpectralMoments:
    """Test spectral moment computation."""

    def test_moments_pm(self):
        omega = np.linspace(0.05, 3.0, 500)
        s = pierson_moskowitz(omega, hs=3.0, tp=10.0)
        moments = compute_spectral_moments(omega, s)
        # m0 ~ Hs^2/16
        np.testing.assert_allclose(moments.m0, 3.0**2 / 16.0, rtol=0.05)
        # All moments should be positive
        assert moments.m0 > 0
        assert moments.m1 > 0
        assert moments.m2 > 0

    def test_spectral_periods(self):
        omega = np.linspace(0.05, 3.0, 500)
        s = pierson_moskowitz(omega, hs=3.0, tp=10.0)
        moments = compute_spectral_moments(omega, s)
        periods = spectral_periods(moments)
        assert "Tz" in periods
        assert "T01" in periods
        # Tz should be reasonable (< Tp typically)
        assert 5.0 < periods["Tz"] < 15.0


class TestGenerateSpectrum:
    """Test high-level generate_spectrum function."""

    def test_jonswap_generation(self):
        params = SpectrumParameters(spectrum_type="JONSWAP", hs=3.0, tp=10.0)
        result = generate_spectrum(params)
        np.testing.assert_allclose(result.hs_computed, 3.0, rtol=0.1)
        assert len(result.frequencies) == 200

    def test_pm_generation(self):
        params = SpectrumParameters(spectrum_type="PM", hs=2.0, tp=8.0)
        result = generate_spectrum(params)
        assert result.hs_computed > 0
        assert result.tz > 0

    def test_unknown_type_raises(self):
        params = SpectrumParameters(spectrum_type="INVALID", hs=1.0, tp=5.0)
        with pytest.raises(ValueError, match="Unknown spectrum type"):
            generate_spectrum(params)
