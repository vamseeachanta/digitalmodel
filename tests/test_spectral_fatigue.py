"""Tests for spectral fatigue -- DNV-RP-C203 spectral method."""
import math

import pytest


class TestSpectralMoments:
    def test_m0_from_spectrum(self):
        """m0 = integral of S(f) df -- area under spectrum."""
        from digitalmodel.structural.spectral_fatigue import spectral_moment

        # Simple rectangular spectrum: S(f)=1.0 from f=0.05 to f=0.25
        freqs = [0.05, 0.10, 0.15, 0.20, 0.25]
        spectrum = [1.0, 1.0, 1.0, 1.0, 1.0]
        m0 = spectral_moment(freqs, spectrum, n=0)
        assert abs(m0 - 0.20) < 0.01  # area = 1.0 * 0.20

    def test_m2_from_spectrum(self):
        """m2 = integral of f^2 * S(f) df"""
        from digitalmodel.structural.spectral_fatigue import spectral_moment

        freqs = [0.05, 0.10, 0.15, 0.20, 0.25]
        spectrum = [1.0, 1.0, 1.0, 1.0, 1.0]
        m2 = spectral_moment(freqs, spectrum, n=2)
        assert m2 > 0

    def test_m4_from_spectrum(self):
        """m4 = integral of f^4 * S(f) df"""
        from digitalmodel.structural.spectral_fatigue import spectral_moment

        freqs = [0.05, 0.10, 0.15, 0.20, 0.25]
        spectrum = [1.0, 1.0, 1.0, 1.0, 1.0]
        m4 = spectral_moment(freqs, spectrum, n=4)
        assert m4 > 0

    def test_moment_order_increases(self):
        """Higher-order moments weight high frequencies more."""
        from digitalmodel.structural.spectral_fatigue import spectral_moment

        freqs = [0.05, 0.10, 0.15, 0.20, 0.25]
        spectrum = [1.0, 1.0, 1.0, 1.0, 1.0]
        m0 = spectral_moment(freqs, spectrum, n=0)
        m2 = spectral_moment(freqs, spectrum, n=2)
        m4 = spectral_moment(freqs, spectrum, n=4)
        # For flat spectrum over [0.05, 0.25], m0 > m2 > m4
        # because f < 1 so f^n decreases the integrand
        assert m0 > m2 > m4

    def test_empty_spectrum_raises(self):
        """Empty inputs should raise ValueError."""
        from digitalmodel.structural.spectral_fatigue import spectral_moment

        with pytest.raises(ValueError):
            spectral_moment([], [], n=0)


class TestSpectralParameters:
    def test_zero_crossing_period(self):
        """Tz = sqrt(m0/m2)"""
        from digitalmodel.structural.spectral_fatigue import zero_crossing_period

        tz = zero_crossing_period(m0=1.0, m2=0.04)
        assert abs(tz - 5.0) < 0.01  # sqrt(1/0.04) = 5.0

    def test_bandwidth_parameter(self):
        """epsilon = sqrt(1 - m2^2/(m0*m4))"""
        from digitalmodel.structural.spectral_fatigue import bandwidth_parameter

        eps = bandwidth_parameter(m0=1.0, m2=0.5, m4=0.3)
        expected = math.sqrt(1 - 0.5**2 / (1.0 * 0.3))
        assert abs(eps - expected) < 1e-6

    def test_narrow_band_epsilon_zero(self):
        """Narrow-band: m2^2 = m0*m4 -> epsilon = 0"""
        from digitalmodel.structural.spectral_fatigue import bandwidth_parameter

        eps = bandwidth_parameter(m0=1.0, m2=1.0, m4=1.0)
        assert abs(eps) < 1e-10

    def test_zero_crossing_period_zero_m2_raises(self):
        """Zero m2 should raise ValueError."""
        from digitalmodel.structural.spectral_fatigue import zero_crossing_period

        with pytest.raises(ValueError):
            zero_crossing_period(m0=1.0, m2=0.0)


class TestNarrowBandDamage:
    def test_narrow_band_fatigue_damage(self):
        """Narrow-band damage estimate per DNV-RP-C203 Sec 4.3"""
        from digitalmodel.structural.spectral_fatigue import narrow_band_damage

        result = narrow_band_damage(
            m0=100.0,  # (MPa)^2
            tz_seconds=8.0,
            duration_seconds=3600 * 24 * 365 * 25,  # 25 years
            sn_log_a=12.164,  # DNV D-curve in seawater with CP
            sn_m=3.0,
        )
        assert result.damage > 0
        assert result.standard == "DNV-RP-C203"
        assert hasattr(result, "cycles")
        assert result.cycles > 0
        assert result.stress_range_rms > 0

    def test_zero_m0_gives_zero_damage(self):
        """Zero variance means no stress -> zero damage."""
        from digitalmodel.structural.spectral_fatigue import narrow_band_damage

        result = narrow_band_damage(
            m0=0.0,
            tz_seconds=8.0,
            duration_seconds=3600 * 24 * 365,
            sn_log_a=12.164,
            sn_m=3.0,
        )
        assert result.damage == 0.0

    def test_damage_increases_with_duration(self):
        """Longer exposure means more damage."""
        from digitalmodel.structural.spectral_fatigue import narrow_band_damage

        r1 = narrow_band_damage(
            m0=100.0, tz_seconds=8.0,
            duration_seconds=3600 * 24 * 365,
            sn_log_a=12.164, sn_m=3.0,
        )
        r2 = narrow_band_damage(
            m0=100.0, tz_seconds=8.0,
            duration_seconds=3600 * 24 * 365 * 10,
            sn_log_a=12.164, sn_m=3.0,
        )
        assert r2.damage > r1.damage

    def test_damage_increases_with_m0(self):
        """Higher stress variance means more damage."""
        from digitalmodel.structural.spectral_fatigue import narrow_band_damage

        r1 = narrow_band_damage(
            m0=50.0, tz_seconds=8.0,
            duration_seconds=3600 * 24 * 365,
            sn_log_a=12.164, sn_m=3.0,
        )
        r2 = narrow_band_damage(
            m0=200.0, tz_seconds=8.0,
            duration_seconds=3600 * 24 * 365,
            sn_log_a=12.164, sn_m=3.0,
        )
        assert r2.damage > r1.damage
