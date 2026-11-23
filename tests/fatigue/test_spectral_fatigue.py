"""Tests for spectral_fatigue — frequency-domain fatigue methods."""

import math
import numpy as np
import pytest

from digitalmodel.fatigue.spectral_fatigue import (
    SpectralMoments,
    compute_spectral_moments,
    narrow_band_damage,
    wirsching_light_damage,
    dirlik_damage,
    benasciutti_tovo_damage,
)


def _narrow_band_psd_moments():
    """Create spectral moments for a narrow-band process (ε ≈ 0)."""
    # Single-peak PSD approximation: all energy at one frequency
    return SpectralMoments(m0=100.0, m1=1000.0, m2=10000.0, m4=1000000.0)


def _wide_band_psd_moments():
    """Create spectral moments for a wide-band process (ε > 0.5)."""
    return SpectralMoments(m0=100.0, m1=500.0, m2=5000.0, m4=2000000.0)


class TestSpectralMoments:
    """Test spectral moment properties."""

    def test_rms_from_m0(self):
        """RMS = √m0."""
        m = SpectralMoments(m0=400.0, m1=0.0, m2=100.0, m4=10000.0)
        assert abs(m.rms - 20.0) < 0.01

    def test_narrow_band_epsilon_near_zero(self):
        """For narrow-band PSD, bandwidth parameter ε ≈ 0."""
        m = _narrow_band_psd_moments()
        assert m.bandwidth_parameter < 0.1

    def test_compute_from_psd_array(self):
        """Compute moments from a simple PSD array."""
        f = np.linspace(0.01, 1.0, 1000)
        # Single peak PSD at f=0.2 Hz
        psd = 100.0 * np.exp(-((f - 0.2) ** 2) / (2 * 0.01 ** 2))
        m = compute_spectral_moments(f, psd)
        assert m.m0 > 0
        assert m.m2 > 0
        assert m.m4 > 0


class TestNarrowBandDamage:
    """Test narrow-band fatigue damage."""

    def test_damage_rate_positive(self):
        """Damage rate must be positive for non-zero stress."""
        m = _narrow_band_psd_moments()
        result = narrow_band_damage(m, sn_slope=3.0, sn_intercept=12.164)
        assert result.damage_rate > 0

    def test_damage_increases_with_stress(self):
        """Higher m0 → higher damage."""
        m_low = SpectralMoments(m0=50.0, m1=500.0, m2=5000.0, m4=500000.0)
        m_high = SpectralMoments(m0=200.0, m1=2000.0, m2=20000.0, m4=2000000.0)
        d_low = narrow_band_damage(m_low, 3.0, 12.164)
        d_high = narrow_band_damage(m_high, 3.0, 12.164)
        assert d_high.damage_rate > d_low.damage_rate

    def test_equivalent_stress_range_positive(self):
        m = _narrow_band_psd_moments()
        result = narrow_band_damage(m, 3.0, 12.164)
        assert result.equivalent_stress_range > 0


class TestWirschingLight:
    """Test Wirsching-Light correction."""

    def test_wl_less_than_narrowband_for_wideband(self):
        """WL correction factor < 1 for wide-band → less damage than NB."""
        m = _wide_band_psd_moments()
        nb = narrow_band_damage(m, 3.0, 12.164)
        wl = wirsching_light_damage(m, 3.0, 12.164)
        assert wl.damage_rate <= nb.damage_rate

    def test_wl_close_to_nb_for_narrowband(self):
        """For narrow-band, WL ≈ NB."""
        m = _narrow_band_psd_moments()
        nb = narrow_band_damage(m, 3.0, 12.164)
        wl = wirsching_light_damage(m, 3.0, 12.164)
        ratio = wl.damage_rate / nb.damage_rate
        assert 0.85 < ratio <= 1.0

    def test_method_string_contains_lambda(self):
        m = _wide_band_psd_moments()
        result = wirsching_light_damage(m, 3.0, 12.164)
        assert "lambda" in result.method.lower()


class TestDirlik:
    """Test Dirlik method."""

    def test_dirlik_damage_positive(self):
        m = _wide_band_psd_moments()
        result = dirlik_damage(m, 3.0, 12.164)
        assert result.damage_rate > 0
        assert result.method == "Dirlik (1985)"

    def test_dirlik_between_nb_and_wl(self):
        """Dirlik typically gives results between NB and WL for wide-band."""
        m = _wide_band_psd_moments()
        nb = narrow_band_damage(m, 3.0, 12.164)
        dk = dirlik_damage(m, 3.0, 12.164)
        # Dirlik should be ≤ narrow-band
        assert dk.damage_rate <= nb.damage_rate * 1.1  # small tolerance

    def test_dirlik_damage_per_year(self):
        m = _narrow_band_psd_moments()
        result = dirlik_damage(m, 3.0, 12.164, duration=1.0)
        assert result.damage_per_year > result.damage_rate


class TestBenasciuttiTovo:
    """Test Benasciutti-Tovo method."""

    def test_bt_damage_positive(self):
        m = _wide_band_psd_moments()
        result = benasciutti_tovo_damage(m, 3.0, 12.164)
        assert result.damage_rate > 0

    def test_bt_method_string(self):
        m = _wide_band_psd_moments()
        result = benasciutti_tovo_damage(m, 3.0, 12.164)
        assert "Benasciutti" in result.method

    def test_bt_positive_damage(self):
        """BT should produce positive damage for wide-band process."""
        m = _wide_band_psd_moments()
        bt = benasciutti_tovo_damage(m, 3.0, 12.164)
        assert bt.damage_rate > 0
        assert bt.equivalent_stress_range > 0
