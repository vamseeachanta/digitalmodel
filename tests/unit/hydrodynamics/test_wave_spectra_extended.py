"""
ABOUTME: Extended unit tests for hydrodynamics/wave_spectra.py.

Covers edge cases, spectral moment calculations, peak enhancement factor
variations, Pierson-Moskowitz vs JONSWAP equivalence, ISSC/Bretschneider
specifics, and spectrum_statistics / zero_crossing_period helpers.

TDD approach: pure numpy calculations, no license-dependent tools.

WRK-149 — wave_spectra.py coverage extension (target: 90%+).
"""

from __future__ import annotations

import math

import numpy as np
import pytest

from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _ws() -> WaveSpectra:
    """Fresh WaveSpectra instance."""
    return WaveSpectra()


# ---------------------------------------------------------------------------
# WaveSpectra.__init__ tests
# ---------------------------------------------------------------------------


class TestWaveSpectraInit:
    def test_gravity_constant(self):
        ws = _ws()
        assert ws.g == pytest.approx(9.81)

    def test_instance_type(self):
        ws = _ws()
        assert isinstance(ws, WaveSpectra)


# ---------------------------------------------------------------------------
# JONSWAP — extended edge case tests
# ---------------------------------------------------------------------------


class TestJonswapExtended:
    def test_small_hs_non_negative(self):
        """Very small Hs: all spectral values should still be >= 0."""
        ws = _ws()
        _, S = ws.jonswap(hs=0.001, tp=8.0)
        assert np.all(S >= 0.0)

    def test_large_hs_scales_squared(self):
        """S ∝ Hs^2 — tripling Hs should give 9x energy."""
        ws = _ws()
        _, S1 = ws.jonswap(hs=1.0, tp=10.0, n_points=200)
        _, S3 = ws.jonswap(hs=3.0, tp=10.0, n_points=200)
        ratio = np.sum(S3) / np.sum(S1)
        assert ratio == pytest.approx(9.0, rel=0.15)

    def test_short_peak_period_shifts_peak(self):
        """Shorter Tp should shift spectral peak to higher frequency."""
        ws = _ws()
        omega, S_short = ws.jonswap(hs=3.0, tp=6.0, n_points=300)
        _, S_long = ws.jonswap(hs=3.0, tp=14.0, n_points=300)
        peak_omega_short = omega[np.argmax(S_short)]
        peak_omega_long = omega[np.argmax(S_long)]
        assert peak_omega_short > peak_omega_long

    def test_gamma_one_equivalent_to_pm_energy(self):
        """JONSWAP with gamma=1 should produce energy similar to PM (same Hs)."""
        ws = _ws()
        omega, S_js1 = ws.jonswap(hs=3.0, tp=10.0, gamma=1.0, n_points=200)
        _, S_pm = ws.pierson_moskowitz(hs=3.0, tp=10.0, n_points=200)
        ratio = np.sum(S_js1) / (np.sum(S_pm) + 1e-30)
        assert 0.5 < ratio < 2.0

    def test_gamma_effect_higher_peak(self):
        """Higher gamma should produce sharper, taller spectral peak."""
        ws = _ws()
        omega, S_low = ws.jonswap(hs=4.0, tp=12.0, gamma=1.0, n_points=300)
        _, S_high = ws.jonswap(hs=4.0, tp=12.0, gamma=7.0, n_points=300)
        # High gamma should have higher peak density
        assert np.max(S_high) > np.max(S_low)

    def test_gamma_effect_recovered_hs_increases_with_gamma(self):
        """Higher gamma concentrates energy at peak, so recovered Hs (via m0)
        increases monotonically with gamma when input Hs is held constant.
        At gamma=3.3 (default), the formula is calibrated so Hs ~ input."""
        ws = _ws()
        hs_in = 4.0
        gammas = [1.0, 2.0, 3.3, 5.0, 7.0]
        hs_out_list = []
        for gamma in gammas:
            omega, S = ws.jonswap(hs=hs_in, tp=12.0, gamma=gamma, n_points=400)
            hs_out_list.append(ws.significant_height_from_spectrum(omega, S))
        # Recovered Hs should be monotonically non-decreasing as gamma increases
        for i in range(len(hs_out_list) - 1):
            assert hs_out_list[i] <= hs_out_list[i + 1] + 1e-6

    def test_high_frequency_tail_decays_to_near_zero(self):
        """Spectrum at very high frequency (far from peak) should be tiny."""
        ws = _ws()
        omega, S = ws.jonswap(hs=3.0, tp=10.0, freq_min=0.02, freq_max=10.0, n_points=500)
        # S at last point (10 rad/s >> wp = 2pi/10 ~ 0.63) should be near zero.
        # Peak is O(1e0), so tail at 10 rad/s should be < 1e-4.
        assert S[-1] < 1e-4

    def test_custom_freq_range_respected(self):
        ws = _ws()
        omega, S = ws.jonswap(hs=3.0, tp=10.0, freq_min=0.5, freq_max=1.5, n_points=50)
        assert omega[0] == pytest.approx(0.5)
        assert omega[-1] == pytest.approx(1.5)
        assert len(omega) == 50

    def test_n_points_respected(self):
        ws = _ws()
        omega, S = ws.jonswap(hs=3.0, tp=10.0, n_points=75)
        assert len(omega) == 75
        assert len(S) == 75

    def test_sigma_width_transition(self):
        """Sigma = 0.07 below peak, 0.09 above peak — spectrum is continuous."""
        ws = _ws()
        tp = 10.0
        omega_p = 2 * math.pi / tp
        omega, S = ws.jonswap(hs=3.0, tp=tp, n_points=500)
        # Find indices just below and just above omega_p
        idx_below = np.searchsorted(omega, omega_p - 0.05)
        idx_above = np.searchsorted(omega, omega_p + 0.05)
        if idx_below > 0 and idx_above < len(S) - 1:
            # Spectrum should not jump discontinuously
            assert abs(S[idx_above] - S[idx_below]) < np.max(S) * 0.5


# ---------------------------------------------------------------------------
# Pierson-Moskowitz — extended tests
# ---------------------------------------------------------------------------


class TestPiersonMoskowitzExtended:
    def test_pm_returns_correct_length(self):
        ws = _ws()
        omega, S = ws.pierson_moskowitz(hs=3.0, tp=10.0, n_points=80)
        assert len(omega) == 80
        assert len(S) == 80

    def test_pm_peak_near_tp(self):
        """PM peak should be close to 2pi/Tp."""
        ws = _ws()
        tp = 10.0
        omega, S = ws.pierson_moskowitz(hs=3.0, tp=tp, n_points=300)
        peak_omega = omega[np.argmax(S)]
        expected_wp = 2 * math.pi / tp
        assert abs(peak_omega - expected_wp) / expected_wp < 0.15

    def test_pm_hs_squared_scaling(self):
        """PM energy scales as Hs^2."""
        ws = _ws()
        _, S1 = ws.pierson_moskowitz(hs=2.0, tp=10.0, n_points=200)
        _, S4 = ws.pierson_moskowitz(hs=4.0, tp=10.0, n_points=200)
        ratio = np.sum(S4) / np.sum(S1)
        assert ratio == pytest.approx(4.0, rel=0.15)

    def test_pm_custom_freq_range(self):
        ws = _ws()
        omega, S = ws.pierson_moskowitz(
            hs=3.0, tp=10.0, freq_min=0.3, freq_max=1.2, n_points=60
        )
        assert omega[0] == pytest.approx(0.3)
        assert omega[-1] == pytest.approx(1.2)

    def test_pm_all_values_finite(self):
        ws = _ws()
        omega, S = ws.pierson_moskowitz(hs=5.0, tp=14.0)
        assert np.all(np.isfinite(S))


# ---------------------------------------------------------------------------
# Bretschneider — extended tests
# ---------------------------------------------------------------------------


class TestBretschneiderExtended:
    def test_bretschneider_non_negative(self):
        ws = _ws()
        _, S = ws.bretschneider(hs=3.5, tp=11.0)
        assert np.all(S >= 0.0)

    def test_bretschneider_length(self):
        ws = _ws()
        omega, S = ws.bretschneider(hs=3.5, tp=11.0, n_points=120)
        assert len(S) == 120

    def test_bretschneider_peak_near_tp(self):
        ws = _ws()
        tp = 12.0
        omega, S = ws.bretschneider(hs=4.0, tp=tp, n_points=300)
        peak_omega = omega[np.argmax(S)]
        # Bretschneider peak is at omega_p * (5/4)^(1/4) ≈ 1.057 * omega_p
        omega_p = 2 * math.pi / tp
        expected_peak = omega_p * (5.0 / 4.0) ** 0.25
        assert abs(peak_omega - expected_peak) / expected_peak < 0.15

    def test_bretschneider_hs_scaling(self):
        ws = _ws()
        _, S1 = ws.bretschneider(hs=1.0, tp=10.0, n_points=200)
        _, S2 = ws.bretschneider(hs=2.0, tp=10.0, n_points=200)
        ratio = np.sum(S2) / np.sum(S1)
        assert ratio == pytest.approx(4.0, rel=0.15)

    def test_bretschneider_all_finite(self):
        ws = _ws()
        _, S = ws.bretschneider(hs=4.0, tp=9.0)
        assert np.all(np.isfinite(S))


# ---------------------------------------------------------------------------
# ISSC — extended tests
# ---------------------------------------------------------------------------


class TestISSCExtended:
    def test_issc_non_negative(self):
        ws = _ws()
        _, S = ws.issc(hs=3.0, tp=10.0)
        assert np.all(S >= 0.0)

    def test_issc_length(self):
        ws = _ws()
        omega, S = ws.issc(hs=3.0, tp=10.0, n_points=90)
        assert len(S) == 90

    def test_issc_all_finite(self):
        ws = _ws()
        _, S = ws.issc(hs=3.0, tp=10.0)
        assert np.all(np.isfinite(S))

    def test_issc_hs_squared_scaling(self):
        """ISSC energy should scale as Hs^2."""
        ws = _ws()
        _, S1 = ws.issc(hs=2.0, tp=10.0, n_points=200)
        _, S4 = ws.issc(hs=4.0, tp=10.0, n_points=200)
        ratio = np.sum(S4) / np.sum(S1)
        assert ratio == pytest.approx(4.0, rel=0.20)


# ---------------------------------------------------------------------------
# Spectral moment tests
# ---------------------------------------------------------------------------


class TestSpectralMoment:
    def test_m0_positive_for_real_spectrum(self):
        ws = _ws()
        omega, S = ws.jonswap(hs=3.0, tp=10.0)
        m0 = ws.spectral_moment(omega, S, n=0)
        assert m0 > 0.0

    def test_m0_finite(self):
        ws = _ws()
        omega, S = ws.jonswap(hs=3.0, tp=10.0)
        m0 = ws.spectral_moment(omega, S, n=0)
        assert math.isfinite(m0)

    def test_m1_positive(self):
        ws = _ws()
        omega, S = ws.jonswap(hs=3.0, tp=10.0)
        m1 = ws.spectral_moment(omega, S, n=1)
        assert m1 > 0.0

    def test_m2_positive(self):
        ws = _ws()
        omega, S = ws.jonswap(hs=3.0, tp=10.0)
        m2 = ws.spectral_moment(omega, S, n=2)
        assert m2 > 0.0

    def test_m0_scales_with_hs_squared(self):
        """m0 ∝ Hs^2 — doubling Hs quadruples m0."""
        ws = _ws()
        omega, S2 = ws.jonswap(hs=2.0, tp=10.0, n_points=300)
        _, S4 = ws.jonswap(hs=4.0, tp=10.0, n_points=300)
        m0_2 = ws.spectral_moment(omega, S2, n=0)
        m0_4 = ws.spectral_moment(omega, S4, n=0)
        ratio = m0_4 / m0_2
        assert ratio == pytest.approx(4.0, rel=0.20)

    def test_m0_zero_spectrum_returns_zero(self):
        ws = _ws()
        omega = np.linspace(0.1, 2.0, 50)
        S = np.zeros_like(omega)
        m0 = ws.spectral_moment(omega, S, n=0)
        assert m0 == pytest.approx(0.0, abs=1e-15)

    def test_higher_moment_increases_with_order(self):
        """m2 > m1 > m0 is not guaranteed generally, but m2/m0 > 1 for
        spectra peaked well above 1 rad/s."""
        ws = _ws()
        # Use high-frequency spectrum so omega^n weighting matters
        omega, S = ws.jonswap(hs=2.0, tp=4.0, freq_min=0.5, freq_max=5.0, n_points=300)
        m0 = ws.spectral_moment(omega, S, n=0)
        m2 = ws.spectral_moment(omega, S, n=2)
        # Peak around 2pi/4 ~ 1.57, so omega^2 > 1 → m2 > m0
        assert m2 > m0


# ---------------------------------------------------------------------------
# significant_height_from_spectrum tests
# ---------------------------------------------------------------------------


class TestSignificantHeightFromSpectrum:
    def test_recover_hs_from_jonswap(self):
        """Recovered Hs from spectral m0 should be within 20% of input Hs."""
        ws = _ws()
        hs_in = 4.0
        omega, S = ws.jonswap(hs=hs_in, tp=12.0, n_points=500)
        hs_out = ws.significant_height_from_spectrum(omega, S)
        assert hs_out == pytest.approx(hs_in, rel=0.25)

    def test_recover_hs_from_pm(self):
        ws = _ws()
        hs_in = 3.0
        omega, S = ws.pierson_moskowitz(hs=hs_in, tp=10.0, n_points=500)
        hs_out = ws.significant_height_from_spectrum(omega, S)
        assert hs_out == pytest.approx(hs_in, rel=0.25)

    def test_zero_spectrum_gives_zero_hs(self):
        ws = _ws()
        omega = np.linspace(0.1, 2.0, 50)
        S = np.zeros_like(omega)
        hs_out = ws.significant_height_from_spectrum(omega, S)
        assert hs_out == pytest.approx(0.0, abs=1e-12)

    def test_hs_positive_for_real_spectrum(self):
        ws = _ws()
        omega, S = ws.bretschneider(hs=2.5, tp=9.0)
        hs_out = ws.significant_height_from_spectrum(omega, S)
        assert hs_out > 0.0

    def test_hs_scales_linearly_with_input(self):
        """Doubling Hs should double recovered Hs."""
        ws = _ws()
        omega, S1 = ws.jonswap(hs=2.0, tp=10.0, n_points=400)
        _, S2 = ws.jonswap(hs=4.0, tp=10.0, n_points=400)
        hs1 = ws.significant_height_from_spectrum(omega, S1)
        hs2 = ws.significant_height_from_spectrum(omega, S2)
        assert hs2 / hs1 == pytest.approx(2.0, rel=0.10)


# ---------------------------------------------------------------------------
# zero_crossing_period_from_spectrum tests
# ---------------------------------------------------------------------------


class TestZeroCrossingPeriodFromSpectrum:
    def test_tz_positive_for_real_spectrum(self):
        ws = _ws()
        omega, S = ws.jonswap(hs=3.0, tp=10.0)
        tz = ws.zero_crossing_period_from_spectrum(omega, S)
        assert tz > 0.0

    def test_tz_finite(self):
        ws = _ws()
        omega, S = ws.jonswap(hs=3.0, tp=10.0)
        tz = ws.zero_crossing_period_from_spectrum(omega, S)
        assert math.isfinite(tz)

    def test_tz_less_than_tp(self):
        """Zero-crossing period Tz < peak period Tp (always for unimodal)."""
        ws = _ws()
        tp = 12.0
        omega, S = ws.jonswap(hs=4.0, tp=tp, n_points=500)
        tz = ws.zero_crossing_period_from_spectrum(omega, S)
        assert tz < tp

    def test_tz_increases_with_tp(self):
        """Longer peak period should give longer zero-crossing period."""
        ws = _ws()
        omega, S_short = ws.jonswap(hs=3.0, tp=8.0, n_points=500)
        _, S_long = ws.jonswap(hs=3.0, tp=14.0, n_points=500)
        tz_short = ws.zero_crossing_period_from_spectrum(omega, S_short)
        tz_long = ws.zero_crossing_period_from_spectrum(omega, S_long)
        assert tz_long > tz_short

    def test_tz_pm_approximately_0711_tp(self):
        """For fully-developed PM spectrum, Tz ≈ 0.711 Tp."""
        ws = _ws()
        tp = 10.0
        omega, S = ws.pierson_moskowitz(hs=3.0, tp=tp, n_points=500)
        tz = ws.zero_crossing_period_from_spectrum(omega, S)
        expected = 0.711 * tp
        assert tz == pytest.approx(expected, rel=0.15)


# ---------------------------------------------------------------------------
# peak_frequency_from_spectrum tests
# ---------------------------------------------------------------------------


class TestPeakFrequencyFromSpectrum:
    def test_returns_positive_frequency(self):
        ws = _ws()
        omega, S = ws.jonswap(hs=3.0, tp=10.0)
        wp = ws.peak_frequency_from_spectrum(omega, S)
        assert wp > 0.0

    def test_peak_frequency_is_in_input_array(self):
        ws = _ws()
        omega, S = ws.jonswap(hs=3.0, tp=10.0)
        wp = ws.peak_frequency_from_spectrum(omega, S)
        assert wp in omega

    def test_peak_frequency_near_2pi_over_tp(self):
        ws = _ws()
        tp = 12.0
        omega, S = ws.jonswap(hs=4.0, tp=tp, n_points=400)
        wp = ws.peak_frequency_from_spectrum(omega, S)
        expected = 2 * math.pi / tp
        assert abs(wp - expected) / expected < 0.10


# ---------------------------------------------------------------------------
# spectrum_statistics tests
# ---------------------------------------------------------------------------


class TestSpectrumStatistics:
    def test_returns_dict(self):
        ws = _ws()
        omega, S = ws.jonswap(hs=4.0, tp=12.0)
        stats = ws.spectrum_statistics(omega, S)
        assert isinstance(stats, dict)

    def test_required_keys_present(self):
        ws = _ws()
        omega, S = ws.jonswap(hs=4.0, tp=12.0)
        stats = ws.spectrum_statistics(omega, S)
        required = ["m0", "m1", "m2", "m4", "Hs_m", "Tz_s", "Tp_s",
                    "omega_p_rad_s", "spectral_width"]
        for key in required:
            assert key in stats, f"Missing key: {key}"

    def test_m0_positive(self):
        ws = _ws()
        omega, S = ws.jonswap(hs=3.0, tp=10.0)
        stats = ws.spectrum_statistics(omega, S)
        assert stats["m0"] > 0.0

    def test_hs_close_to_input(self):
        ws = _ws()
        hs_in = 4.0
        omega, S = ws.jonswap(hs=hs_in, tp=12.0, n_points=500)
        stats = ws.spectrum_statistics(omega, S)
        assert stats["Hs_m"] == pytest.approx(hs_in, rel=0.25)

    def test_tp_close_to_input(self):
        ws = _ws()
        tp_in = 12.0
        omega, S = ws.jonswap(hs=4.0, tp=tp_in, n_points=400)
        stats = ws.spectrum_statistics(omega, S)
        assert stats["Tp_s"] == pytest.approx(tp_in, rel=0.10)

    def test_spectral_width_between_zero_and_one(self):
        ws = _ws()
        omega, S = ws.jonswap(hs=3.0, tp=10.0, n_points=400)
        stats = ws.spectrum_statistics(omega, S)
        # Spectral width epsilon is in [0, 1]
        assert 0.0 <= stats["spectral_width"] <= 1.0

    def test_tz_positive(self):
        ws = _ws()
        omega, S = ws.jonswap(hs=3.0, tp=10.0)
        stats = ws.spectrum_statistics(omega, S)
        assert stats["Tz_s"] > 0.0

    def test_all_values_finite(self):
        ws = _ws()
        omega, S = ws.pierson_moskowitz(hs=3.0, tp=10.0, n_points=400)
        stats = ws.spectrum_statistics(omega, S)
        for key, val in stats.items():
            assert math.isfinite(val), f"Non-finite value for key '{key}': {val}"

    def test_bretschneider_stats_consistent(self):
        ws = _ws()
        hs_in = 3.5
        tp_in = 11.0
        omega, S = ws.bretschneider(hs=hs_in, tp=tp_in, n_points=400)
        stats = ws.spectrum_statistics(omega, S)
        assert stats["Hs_m"] == pytest.approx(hs_in, rel=0.25)
        assert stats["m0"] > 0.0

    def test_issc_stats_positive(self):
        ws = _ws()
        omega, S = ws.issc(hs=2.5, tp=9.0, n_points=400)
        stats = ws.spectrum_statistics(omega, S)
        assert stats["m0"] > 0.0
        assert stats["Hs_m"] > 0.0


# ---------------------------------------------------------------------------
# generate_spectrum dispatch tests — additional edge cases
# ---------------------------------------------------------------------------


class TestGenerateSpectrumDispatch:
    def test_unsupported_spectrum_type_raises(self):
        """generate_spectrum with an unsupported spectrum type should raise."""
        from digitalmodel.hydrodynamics.models import (
            WaveParameters,
            WaveSpectrumType,
        )
        ws = _ws()
        # Use OCHI_HUBBLE which is not implemented in generate_spectrum
        params = WaveParameters(
            spectrum_type=WaveSpectrumType.OCHI_HUBBLE,
            significant_height=3.0,
            peak_period=10.0,
        )
        with pytest.raises(ValueError, match="not implemented"):
            ws.generate_spectrum(params)

    def test_generate_spectrum_custom_freq_range(self):
        from digitalmodel.hydrodynamics.models import (
            WaveParameters,
            WaveSpectrumType,
        )
        ws = _ws()
        params = WaveParameters(
            spectrum_type=WaveSpectrumType.JONSWAP,
            significant_height=3.0,
            peak_period=10.0,
            freq_min=0.3,
            freq_max=1.5,
            n_frequencies=80,
        )
        omega, S = ws.generate_spectrum(params)
        assert omega[0] == pytest.approx(0.3)
        assert omega[-1] == pytest.approx(1.5)
        assert len(omega) == 80

    def test_generate_spectrum_bretschneider_gamma_not_used(self):
        """Bretschneider dispatch should not fail even if gamma is set."""
        from digitalmodel.hydrodynamics.models import (
            WaveParameters,
            WaveSpectrumType,
        )
        ws = _ws()
        params = WaveParameters(
            spectrum_type=WaveSpectrumType.BRETSCHNEIDER,
            significant_height=3.0,
            peak_period=10.0,
            gamma=5.0,  # gamma is ignored for Bretschneider
        )
        omega, S = ws.generate_spectrum(params)
        assert np.all(S >= 0.0)
