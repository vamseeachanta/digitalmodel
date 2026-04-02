"""Tests for digitalmodel.orcaflex.environment module."""

import math

import numpy as np
import pytest

from digitalmodel.orcaflex.environment import (
    CurrentProfile,
    CurrentProfileType,
    EnvironmentalLoadCase,
    EnvironmentalMatrix,
    WaveSpectrumParams,
    WaveSpectrumType,
    WindProfile,
    WindProfileType,
)


class TestCurrentProfile:
    """Tests for current profile generation."""

    def test_uniform_profile_constant_speed(self):
        """Uniform profile should have constant speed at all depths."""
        cp = CurrentProfile(
            profile_type=CurrentProfileType.UNIFORM,
            surface_speed=1.2,
            water_depth=500.0,
            n_points=11,
        )
        profile = cp.generate_profile()
        assert len(profile) == 11
        for pt in profile:
            assert pt.speed == pytest.approx(1.2, abs=1e-6)

    def test_power_law_profile_decreases_with_depth(self):
        """Power-law profile speed should decrease with depth."""
        cp = CurrentProfile(
            profile_type=CurrentProfileType.POWER_LAW,
            surface_speed=1.5,
            water_depth=1000.0,
            power_law_exponent=1 / 7.0,
            n_points=21,
        )
        profile = cp.generate_profile()
        # Surface speed should be near 1.5
        assert profile[0].speed == pytest.approx(1.5, abs=0.01)
        # Speed at bottom should be near zero
        assert profile[-1].speed < 0.01
        # Should decrease monotonically
        for i in range(len(profile) - 1):
            assert profile[i].speed >= profile[i + 1].speed

    def test_linear_profile_interpolation(self):
        """Linear profile should interpolate between surface and seabed speeds."""
        cp = CurrentProfile(
            profile_type=CurrentProfileType.LINEAR,
            surface_speed=2.0,
            water_depth=100.0,
            seabed_speed_fraction=0.5,
            n_points=3,
        )
        profile = cp.generate_profile()
        assert profile[0].speed == pytest.approx(2.0, abs=1e-6)
        assert profile[1].speed == pytest.approx(1.5, abs=1e-6)  # midpoint
        assert profile[2].speed == pytest.approx(1.0, abs=1e-6)  # seabed

    def test_orcaflex_dict_export(self):
        """to_orcaflex_dict should return valid structure."""
        cp = CurrentProfile(surface_speed=1.0, n_points=5)
        d = cp.to_orcaflex_dict()
        assert "CurrentDepth" in d
        assert "CurrentSpeed" in d
        assert "CurrentDirection" in d
        assert len(d["CurrentDepth"]) == 5


class TestWaveSpectrum:
    """Tests for wave spectrum calculations."""

    def test_jonswap_peak_frequency(self):
        """Peak frequency should equal 1/Tp."""
        ws = WaveSpectrumParams(hs=4.0, tp=10.0, gamma=3.3)
        assert ws.fp == pytest.approx(0.1, abs=1e-10)

    def test_spectral_density_positive(self):
        """JONSWAP spectral density should be non-negative."""
        ws = WaveSpectrumParams(hs=3.0, tp=9.0)
        f = np.linspace(0.01, 0.5, 100)
        S = ws.spectral_density(f)
        assert np.all(S >= 0)

    def test_pm_spectrum_no_gamma(self):
        """Pierson-Moskowitz spectrum should not use gamma enhancement."""
        ws = WaveSpectrumParams(
            spectrum_type=WaveSpectrumType.PIERSON_MOSKOWITZ,
            hs=5.0,
            tp=12.0,
        )
        f = np.linspace(0.01, 0.5, 200)
        S = ws.spectral_density(f)
        # Should have a peak near fp
        peak_idx = np.argmax(S)
        assert 0.05 < f[peak_idx] < 0.15

    def test_orcaflex_dict_format(self):
        """Wave spectrum should export to OrcaFlex dict."""
        ws = WaveSpectrumParams(hs=3.0, tp=10.0)
        d = ws.to_orcaflex_dict()
        assert d["WaveHs"] == 3.0
        assert d["WaveTp"] == 10.0


class TestWindProfile:
    """Tests for wind profile calculations."""

    def test_power_law_at_reference_height(self):
        """Wind speed at reference height should equal reference speed."""
        wp = WindProfile(reference_speed=25.0, reference_height=10.0)
        assert wp.speed_at_height(10.0) == pytest.approx(25.0, abs=1e-6)

    def test_power_law_increases_with_height(self):
        """Wind speed should increase with height (power law)."""
        wp = WindProfile(reference_speed=20.0, reference_height=10.0)
        assert wp.speed_at_height(50.0) > wp.speed_at_height(10.0)

    def test_log_law_at_reference_height(self):
        """Log law should return reference speed at reference height."""
        wp = WindProfile(
            profile_type=WindProfileType.LOG_LAW,
            reference_speed=20.0,
            reference_height=10.0,
        )
        assert wp.speed_at_height(10.0) == pytest.approx(20.0, abs=1e-6)

    def test_generate_profile_returns_correct_count(self):
        """generate_profile should return heights and speeds."""
        wp = WindProfile()
        profile = wp.generate_profile([10, 50, 100])
        assert len(profile) == 3
        assert profile[0][0] == 10


class TestEnvironmentalMatrix:
    """Tests for environmental load combination matrix."""

    def test_uls_case_generation(self):
        """ULS cases should be generated for all headings."""
        em = EnvironmentalMatrix(headings=[0, 90, 180, 270])
        cases = em.generate_uls_cases()
        # 2 cases per heading (A + B)
        assert len(cases) == 8

    def test_als_cases_damaged_condition(self):
        """ALS cases should have damaged condition."""
        em = EnvironmentalMatrix(headings=[0, 180])
        cases = em.generate_als_cases()
        for c in cases:
            assert c.condition == "damaged"

    def test_full_matrix_size(self):
        """Full matrix should include both ULS and ALS."""
        em = EnvironmentalMatrix(headings=[0, 90, 180, 270])
        all_cases = em.generate_all_cases()
        # 8 ULS + 4 ALS
        assert len(all_cases) == 12
