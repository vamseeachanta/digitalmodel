"""Tests for orcawave.motion_statistics module."""

import numpy as np
import pytest

from digitalmodel.orcawave.motion_statistics import (
    MSIResult,
    ResponseSpectrum,
    ScatterCell,
    compute_response_spectrum,
    long_term_extreme,
    motion_sickness_incidence,
    rayleigh_exceedance,
    rayleigh_quantile,
    short_term_statistics,
)
from digitalmodel.orcawave.wave_spectrum import pierson_moskowitz


class TestResponseSpectrum:
    """Test response spectrum computation."""

    def test_response_spectrum_shape(self):
        omega = np.linspace(0.1, 2.0, 100)
        rao = np.ones(100) * 0.5  # constant RAO
        s_wave = pierson_moskowitz(omega, hs=2.0, tp=8.0)
        resp = compute_response_spectrum(omega, rao, s_wave)
        assert len(resp.spectral_density) == 100
        assert resp.m0 > 0

    def test_unit_rao_preserves_spectrum(self):
        omega = np.linspace(0.1, 2.0, 100)
        rao = np.ones(100)  # unit RAO
        s_wave = pierson_moskowitz(omega, hs=3.0, tp=10.0)
        resp = compute_response_spectrum(omega, rao, s_wave)
        # With unit RAO, response m0 should equal wave m0
        wave_m0 = np.trapz(s_wave, omega)
        np.testing.assert_allclose(resp.m0, wave_m0, rtol=0.01)

    def test_zero_rao_gives_zero_response(self):
        omega = np.linspace(0.1, 2.0, 100)
        rao = np.zeros(100)
        s_wave = pierson_moskowitz(omega, hs=3.0, tp=10.0)
        resp = compute_response_spectrum(omega, rao, s_wave)
        np.testing.assert_allclose(resp.m0, 0.0, atol=1e-20)


class TestShortTermStatistics:
    """Test short-term statistics calculation."""

    def test_significant_response(self):
        omega = np.linspace(0.1, 2.0, 200)
        rao = np.ones(200) * 0.8
        s_wave = pierson_moskowitz(omega, hs=3.0, tp=10.0)
        resp = compute_response_spectrum(omega, rao, s_wave)
        stats = short_term_statistics(resp)
        # Significant response ~ 0.8 * Hs (since RAO = 0.8)
        assert 1.0 < stats.significant < 4.0
        assert stats.rms > 0
        assert stats.mean_zero_crossing_period > 0

    def test_maximum_exceeds_significant(self):
        omega = np.linspace(0.1, 2.0, 200)
        rao = np.ones(200)
        s_wave = pierson_moskowitz(omega, hs=4.0, tp=10.0)
        resp = compute_response_spectrum(omega, rao, s_wave)
        stats = short_term_statistics(resp)
        assert stats.most_probable_maximum_3hr > stats.significant
        assert stats.expected_maximum_3hr >= stats.most_probable_maximum_3hr

    def test_bandwidth_parameter(self):
        omega = np.linspace(0.1, 2.0, 200)
        rao = np.ones(200)
        s_wave = pierson_moskowitz(omega, hs=2.0, tp=8.0)
        resp = compute_response_spectrum(omega, rao, s_wave)
        stats = short_term_statistics(resp)
        # Bandwidth should be between 0 and 1
        assert 0 <= stats.spectral_bandwidth <= 1


class TestRayleigh:
    """Test Rayleigh distribution helpers."""

    def test_exceedance_at_zero(self):
        p = rayleigh_exceedance(0.0, sigma=1.0)
        np.testing.assert_allclose(p, 1.0, atol=1e-10)

    def test_exceedance_decreasing(self):
        p1 = rayleigh_exceedance(1.0, sigma=1.0)
        p2 = rayleigh_exceedance(2.0, sigma=1.0)
        assert p1 > p2

    def test_quantile_roundtrip(self):
        sigma = 1.5
        amplitude = 3.0
        p = rayleigh_exceedance(amplitude, sigma)
        amp_back = rayleigh_quantile(p, sigma)
        np.testing.assert_allclose(amp_back, amplitude, rtol=1e-6)

    def test_quantile_zero_probability(self):
        result = rayleigh_quantile(0.0, 1.0)
        assert result == 0.0


class TestMSI:
    """Test Motion Sickness Incidence."""

    def test_msi_positive(self):
        result = motion_sickness_incidence(
            vertical_acc_rms=1.0,
            exposure_hours=2.0,
            dominant_frequency_hz=0.15,
        )
        assert result.msi_percent > 0
        assert result.exposure_hours == 2.0

    def test_msi_increases_with_acceleration(self):
        r1 = motion_sickness_incidence(0.5, 2.0, 0.15)
        r2 = motion_sickness_incidence(2.0, 2.0, 0.15)
        assert r2.msi_percent > r1.msi_percent

    def test_msi_capped_at_100(self):
        result = motion_sickness_incidence(100.0, 24.0, 0.167)
        assert result.msi_percent <= 100.0
