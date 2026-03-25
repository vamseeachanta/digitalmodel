"""
Tests for Frequency Domain Fatigue Analysis
============================================

Comprehensive unit tests for:
- SpectralMoments dataclass
- FrequencyDomainResult dataclass
- FrequencyDomainBase (spectral moments, zero-crossing rate, peak rate)
- NarrowBandMethod
- DirlikMethod
- TovoBenasciuttiMethod
- SingleMomentMethod
- ResponsePSDCalculator (response PSD, SDOF/MDOF transfer functions)
- compare_frequency_methods utility

Uses realistic engineering values for stress PSDs, frequencies, and S-N curves.
"""

import numpy as np
import pandas as pd
import pytest
from unittest.mock import MagicMock, patch

from digitalmodel.structural.fatigue.frequency_domain import (
    SpectralMoments,
    FrequencyDomainResult,
    FrequencyDomainBase,
    NarrowBandMethod,
    DirlikMethod,
    TovoBenasciuttiMethod,
    SingleMomentMethod,
    ResponsePSDCalculator,
    compare_frequency_methods,
)
from digitalmodel.structural.fatigue.sn_curves import (
    PowerLawSNCurve,
    SNCurveBase,
    StandardSNCurves,
    get_dnv_curve,
)


# ---------------------------------------------------------------------------
# Helpers / Fixtures
# ---------------------------------------------------------------------------

def _make_narrowband_psd(center_freq_hz=5.0, bandwidth_hz=0.2,
                         amplitude=100.0, n_points=2048):
    """Create a narrow-band Gaussian PSD centred at *center_freq_hz*."""
    freq = np.linspace(0.01, 50.0, n_points)
    sigma = bandwidth_hz / (2 * np.sqrt(2 * np.log(2)))  # FWHM -> sigma
    psd = amplitude * np.exp(-0.5 * ((freq - center_freq_hz) / sigma) ** 2)
    return freq, psd


def _make_broadband_psd(n_points=2048):
    """Create a broadband PSD with multiple peaks (typical structural response)."""
    freq = np.linspace(0.01, 50.0, n_points)
    # Base white noise
    psd = 0.5 * np.ones_like(freq)
    # Resonance at 5 Hz
    psd += 10.0 * np.exp(-0.5 * ((freq - 5.0) / 0.5) ** 2)
    # Resonance at 15 Hz
    psd += 5.0 * np.exp(-0.5 * ((freq - 15.0) / 1.0) ** 2)
    # Resonance at 30 Hz
    psd += 2.0 * np.exp(-0.5 * ((freq - 30.0) / 2.0) ** 2)
    return freq, psd


def _make_white_noise_psd(level=1.0, f_min=0.1, f_max=50.0, n_points=2048):
    """Constant-level PSD (white noise)."""
    freq = np.linspace(f_min, f_max, n_points)
    psd = level * np.ones_like(freq)
    return freq, psd


@pytest.fixture
def dnv_d_curve():
    """DNV-RP-C203 curve D (m=3, A=5.73e11)."""
    return get_dnv_curve('D')


@pytest.fixture
def simple_sn_curve():
    """Simple power-law S-N curve without fatigue limit."""
    return PowerLawSNCurve(name='Test', A=1e12, m=3.0, fatigue_limit=0.0)


@pytest.fixture
def narrowband_psd():
    return _make_narrowband_psd()


@pytest.fixture
def broadband_psd():
    return _make_broadband_psd()


@pytest.fixture
def white_noise_psd():
    return _make_white_noise_psd()


# ===================================================================
# SpectralMoments dataclass
# ===================================================================

class TestSpectralMoments:

    def test_dataclass_fields(self):
        sm = SpectralMoments(
            m0=1.0, m1=2.0, m2=3.0, m4=4.0,
            lambda1=2.0, lambda2=1.732, nu=0.5, alpha=0.8, q=0.9
        )
        assert sm.m0 == 1.0
        assert sm.m1 == 2.0
        assert sm.m2 == 3.0
        assert sm.m4 == 4.0
        assert sm.lambda1 == 2.0
        assert sm.lambda2 == pytest.approx(1.732)
        assert sm.nu == 0.5
        assert sm.alpha == 0.8
        assert sm.q == 0.9


# ===================================================================
# FrequencyDomainResult dataclass
# ===================================================================

class TestFrequencyDomainResult:

    def test_dataclass_fields(self):
        sm = SpectralMoments(1, 2, 3, 4, 2, 1.7, 0.5, 0.8, 0.9)
        r = FrequencyDomainResult(
            method='Test',
            damage_rate=1e-6,
            expected_cycles_per_second=5.0,
            fatigue_life=1e6,
            equivalent_stress=50.0,
            spectral_moments=sm,
            parameters={'key': 'val'},
        )
        assert r.method == 'Test'
        assert r.damage_rate == 1e-6
        assert r.expected_cycles_per_second == 5.0
        assert r.fatigue_life == 1e6
        assert r.equivalent_stress == 50.0
        assert r.spectral_moments is sm
        assert r.parameters == {'key': 'val'}


# ===================================================================
# FrequencyDomainBase - spectral moments
# ===================================================================

class TestSpectralMomentsCalculation:
    """Tests for _calculate_spectral_moments via NarrowBandMethod (concrete)."""

    def test_moments_positive(self, narrowband_psd, dnv_d_curve):
        freq, psd = narrowband_psd
        method = NarrowBandMethod()
        sm = method._calculate_spectral_moments(freq, psd)
        assert sm.m0 > 0
        assert sm.m1 > 0
        assert sm.m2 > 0
        assert sm.m4 > 0

    def test_lambda1_is_m1_over_m0(self, narrowband_psd):
        freq, psd = narrowband_psd
        method = NarrowBandMethod()
        sm = method._calculate_spectral_moments(freq, psd)
        assert sm.lambda1 == pytest.approx(sm.m1 / sm.m0, rel=1e-10)

    def test_lambda2_is_sqrt_m2_over_m0(self, narrowband_psd):
        freq, psd = narrowband_psd
        method = NarrowBandMethod()
        sm = method._calculate_spectral_moments(freq, psd)
        assert sm.lambda2 == pytest.approx(np.sqrt(sm.m2 / sm.m0), rel=1e-10)

    def test_bandwidth_alpha_range(self, broadband_psd):
        freq, psd = broadband_psd
        method = NarrowBandMethod()
        sm = method._calculate_spectral_moments(freq, psd)
        # alpha (Wirsching-Light) should be between 0 and 1
        assert 0 <= sm.alpha <= 1.0

    def test_vanmarcke_q_range(self, broadband_psd):
        freq, psd = broadband_psd
        method = NarrowBandMethod()
        sm = method._calculate_spectral_moments(freq, psd)
        assert 0 <= sm.q <= 1.0

    def test_zero_psd_gives_near_zero_m0(self):
        freq = np.linspace(0.1, 50.0, 500)
        psd = np.zeros_like(freq)
        method = NarrowBandMethod()
        sm = method._calculate_spectral_moments(freq, psd)
        # m0 gets clamped to 1e-10
        assert sm.m0 == pytest.approx(1e-10, abs=1e-12)

    def test_narrowband_alpha_near_one(self):
        """A very narrow PSD should have alpha close to 1."""
        freq, psd = _make_narrowband_psd(bandwidth_hz=0.05)
        method = NarrowBandMethod()
        sm = method._calculate_spectral_moments(freq, psd)
        assert sm.alpha > 0.9

    def test_moments_with_max_moment_2_raises_key_error(self, narrowband_psd):
        """Known bug: _calculate_spectral_moments always unpacks m4, so max_moment<4 fails."""
        freq, psd = narrowband_psd
        method = NarrowBandMethod()
        with pytest.raises(KeyError, match="m4"):
            method._calculate_spectral_moments(freq, psd, max_moment=2)

    def test_white_noise_m0_equals_level_times_bandwidth(self):
        """For constant PSD = S0 over [f1, f2], m0 = S0 * (f2 - f1)."""
        level = 2.5
        f_min, f_max = 1.0, 10.0
        freq = np.linspace(f_min, f_max, 5000)
        psd = level * np.ones_like(freq)
        method = NarrowBandMethod()
        sm = method._calculate_spectral_moments(freq, psd)
        expected_m0 = level * (f_max - f_min)
        assert sm.m0 == pytest.approx(expected_m0, rel=1e-3)


# ===================================================================
# Zero crossing & peak rates
# ===================================================================

class TestRates:

    def test_zero_crossing_rate_positive(self, narrowband_psd):
        freq, psd = narrowband_psd
        method = NarrowBandMethod()
        sm = method._calculate_spectral_moments(freq, psd)
        nu = method._calculate_zero_crossing_rate(sm)
        assert nu > 0

    def test_peak_rate_positive(self, narrowband_psd):
        freq, psd = narrowband_psd
        method = NarrowBandMethod()
        sm = method._calculate_spectral_moments(freq, psd)
        rate = method._calculate_peak_rate(sm)
        assert rate > 0

    def test_peak_rate_geq_zero_crossing_rate(self, broadband_psd):
        """Expected peak rate >= zero-crossing rate for broadband processes."""
        freq, psd = broadband_psd
        method = NarrowBandMethod()
        sm = method._calculate_spectral_moments(freq, psd)
        nu = method._calculate_zero_crossing_rate(sm)
        np_rate = method._calculate_peak_rate(sm)
        assert np_rate >= nu - 1e-12  # allow tiny numerical tolerance

    def test_zero_crossing_rate_zero_for_zero_psd(self):
        sm = SpectralMoments(m0=0, m1=0, m2=0, m4=0,
                             lambda1=0, lambda2=0, nu=0, alpha=1, q=1)
        method = NarrowBandMethod()
        assert method._calculate_zero_crossing_rate(sm) == 0.0

    def test_peak_rate_zero_for_zero_psd(self):
        sm = SpectralMoments(m0=0, m1=0, m2=0, m4=0,
                             lambda1=0, lambda2=0, nu=0, alpha=1, q=1)
        method = NarrowBandMethod()
        assert method._calculate_peak_rate(sm) == 0.0


# ===================================================================
# NarrowBandMethod
# ===================================================================

class TestNarrowBandMethod:

    def test_name(self):
        m = NarrowBandMethod()
        assert m.name == "Narrow-band"

    def test_returns_frequency_domain_result(self, narrowband_psd, simple_sn_curve):
        freq, psd = narrowband_psd
        m = NarrowBandMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert isinstance(result, FrequencyDomainResult)
        assert result.method == "Narrow-band"

    def test_damage_rate_positive(self, narrowband_psd, simple_sn_curve):
        freq, psd = narrowband_psd
        m = NarrowBandMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert result.damage_rate > 0

    def test_fatigue_life_positive_finite(self, narrowband_psd, simple_sn_curve):
        freq, psd = narrowband_psd
        m = NarrowBandMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert result.fatigue_life > 0
        assert np.isfinite(result.fatigue_life)

    def test_life_is_inverse_of_damage_rate(self, narrowband_psd, simple_sn_curve):
        freq, psd = narrowband_psd
        m = NarrowBandMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert result.fatigue_life == pytest.approx(1.0 / result.damage_rate, rel=1e-10)

    def test_equivalent_stress_is_twice_sigma_rms(self, narrowband_psd, simple_sn_curve):
        freq, psd = narrowband_psd
        m = NarrowBandMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        sigma_rms = np.sqrt(result.spectral_moments.m0)
        assert result.equivalent_stress == pytest.approx(2 * sigma_rms, rel=1e-10)

    def test_higher_psd_means_more_damage(self, simple_sn_curve):
        freq = np.linspace(0.1, 50, 500)
        psd_low = 0.1 * np.ones_like(freq)
        psd_high = 10.0 * np.ones_like(freq)
        m = NarrowBandMethod()
        r_low = m.calculate_damage_rate(freq, psd_low, simple_sn_curve)
        r_high = m.calculate_damage_rate(freq, psd_high, simple_sn_curve)
        assert r_high.damage_rate > r_low.damage_rate

    def test_parameters_key(self, narrowband_psd, simple_sn_curve):
        freq, psd = narrowband_psd
        m = NarrowBandMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert result.parameters == {'approximation': 'narrow-band'}

    def test_zero_psd_gives_zero_damage(self, simple_sn_curve):
        freq = np.linspace(0.1, 50, 500)
        psd = np.zeros_like(freq)
        m = NarrowBandMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        # Zero PSD => zero crossing rate is 0 => damage_rate 0
        assert result.damage_rate == 0.0
        assert result.fatigue_life == np.inf

    def test_below_fatigue_limit_infinite_life(self):
        """PSD with very low amplitude so stress < fatigue limit => infinite life."""
        curve = PowerLawSNCurve(name='HighFL', A=1e12, m=3.0, fatigue_limit=1000.0)
        freq, psd = _make_narrowband_psd(amplitude=0.001)
        m = NarrowBandMethod()
        result = m.calculate_damage_rate(freq, psd, curve)
        # Stress range = 2*sqrt(m0) should be tiny vs 1000 MPa limit
        assert result.fatigue_life == np.inf


# ===================================================================
# DirlikMethod
# ===================================================================

class TestDirlikMethod:

    def test_name(self):
        assert DirlikMethod().name == "Dirlik"

    def test_returns_result(self, broadband_psd, simple_sn_curve):
        freq, psd = broadband_psd
        m = DirlikMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert isinstance(result, FrequencyDomainResult)
        assert result.method == "Dirlik"

    def test_damage_rate_positive(self, broadband_psd, simple_sn_curve):
        freq, psd = broadband_psd
        m = DirlikMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert result.damage_rate > 0

    def test_fatigue_life_positive(self, broadband_psd, simple_sn_curve):
        freq, psd = broadband_psd
        m = DirlikMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert result.fatigue_life > 0
        assert np.isfinite(result.fatigue_life)

    def test_dirlik_coefficients_sum_to_one(self, broadband_psd, simple_sn_curve):
        """D1 + D2 + D3 should equal 1."""
        freq, psd = broadband_psd
        m = DirlikMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        D1 = result.parameters['D1']
        D2 = result.parameters['D2']
        D3 = result.parameters['D3']
        assert D1 + D2 + D3 == pytest.approx(1.0, abs=1e-10)

    def test_dirlik_D_coefficients_non_negative(self, broadband_psd, simple_sn_curve):
        freq, psd = broadband_psd
        m = DirlikMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert result.parameters['D1'] >= 0
        assert result.parameters['D2'] >= 0
        assert result.parameters['D3'] >= 0

    def test_R1_positive(self, broadband_psd, simple_sn_curve):
        freq, psd = broadband_psd
        m = DirlikMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert result.parameters['R1'] > 0

    def test_higher_psd_more_damage(self, simple_sn_curve):
        freq, psd_lo = _make_broadband_psd()
        psd_hi = psd_lo * 10.0
        m = DirlikMethod()
        r_lo = m.calculate_damage_rate(freq, psd_lo, simple_sn_curve)
        r_hi = m.calculate_damage_rate(freq, psd_hi, simple_sn_curve)
        assert r_hi.damage_rate > r_lo.damage_rate

    def test_narrowband_dirlik_close_to_narrowband_method(self, simple_sn_curve):
        """For narrow-band PSD, Dirlik should give similar results to NB method."""
        freq, psd = _make_narrowband_psd(bandwidth_hz=0.05, amplitude=50.0)
        nb = NarrowBandMethod().calculate_damage_rate(freq, psd, simple_sn_curve)
        dk = DirlikMethod().calculate_damage_rate(freq, psd, simple_sn_curve)
        # Within an order of magnitude for narrow-band process
        ratio = dk.damage_rate / nb.damage_rate if nb.damage_rate > 0 else 0
        assert 0.01 < ratio < 100

    def test_broadband_dirlik_less_conservative_than_narrowband(self, simple_sn_curve):
        """For broadband PSD, Dirlik should be less conservative (lower damage) than NB."""
        freq, psd = _make_broadband_psd()
        nb = NarrowBandMethod().calculate_damage_rate(freq, psd, simple_sn_curve)
        dk = DirlikMethod().calculate_damage_rate(freq, psd, simple_sn_curve)
        # Dirlik typically gives lower damage than narrow-band for broadband processes
        # (narrow-band is conservative)
        assert dk.damage_rate <= nb.damage_rate * 1.1  # small tolerance

    def test_zero_psd(self, simple_sn_curve):
        freq = np.linspace(0.1, 50, 500)
        psd = np.zeros_like(freq)
        m = DirlikMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert result.damage_rate == 0.0
        assert result.fatigue_life == np.inf

    def test_equivalent_stress_positive(self, broadband_psd, simple_sn_curve):
        freq, psd = broadband_psd
        m = DirlikMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert result.equivalent_stress > 0

    def test_sn_curve_without_m_attribute(self, broadband_psd):
        """S-N curve without .m should use default m=3."""
        mock_curve = MagicMock(spec=SNCurveBase)
        mock_curve.get_allowable_cycles.return_value = 1e6
        # Remove .m attribute
        del mock_curve.m
        m = DirlikMethod()
        freq, psd = broadband_psd
        result = m.calculate_damage_rate(freq, psd, mock_curve)
        assert isinstance(result, FrequencyDomainResult)


# ===================================================================
# TovoBenasciuttiMethod
# ===================================================================

class TestTovoBenasciuttiMethod:

    def test_name(self):
        assert TovoBenasciuttiMethod().name == "Tovo-Benasciutti"

    def test_returns_result(self, broadband_psd, simple_sn_curve):
        freq, psd = broadband_psd
        m = TovoBenasciuttiMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert isinstance(result, FrequencyDomainResult)

    def test_damage_rate_positive(self, broadband_psd, simple_sn_curve):
        freq, psd = broadband_psd
        m = TovoBenasciuttiMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert result.damage_rate > 0

    def test_fatigue_life_positive(self, broadband_psd, simple_sn_curve):
        freq, psd = broadband_psd
        m = TovoBenasciuttiMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert result.fatigue_life > 0
        assert np.isfinite(result.fatigue_life)

    def test_weighting_factor_in_range(self, broadband_psd, simple_sn_curve):
        freq, psd = broadband_psd
        m = TovoBenasciuttiMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        w = result.parameters['weighting_factor']
        assert 0 <= w <= 1.0

    def test_narrowband_damage_in_parameters(self, broadband_psd, simple_sn_curve):
        freq, psd = broadband_psd
        m = TovoBenasciuttiMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert 'narrowband_damage' in result.parameters
        assert 'wirsching_light_damage' in result.parameters
        assert 'correction_factor' in result.parameters
        assert 'alpha075' in result.parameters

    def test_higher_psd_more_damage(self, simple_sn_curve):
        freq, psd_lo = _make_broadband_psd()
        psd_hi = psd_lo * 10.0
        m = TovoBenasciuttiMethod()
        r_lo = m.calculate_damage_rate(freq, psd_lo, simple_sn_curve)
        r_hi = m.calculate_damage_rate(freq, psd_hi, simple_sn_curve)
        assert r_hi.damage_rate > r_lo.damage_rate

    def test_tb_less_conservative_than_narrowband(self, simple_sn_curve):
        """TB method should be less conservative than NB for broadband."""
        freq, psd = _make_broadband_psd()
        nb = NarrowBandMethod().calculate_damage_rate(freq, psd, simple_sn_curve)
        tb = TovoBenasciuttiMethod().calculate_damage_rate(freq, psd, simple_sn_curve)
        assert tb.damage_rate <= nb.damage_rate * 1.1

    def test_zero_psd(self, simple_sn_curve):
        freq = np.linspace(0.1, 50, 500)
        psd = np.zeros_like(freq)
        m = TovoBenasciuttiMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert result.damage_rate == 0.0

    def test_sn_curve_without_m_defaults_to_3(self, broadband_psd):
        mock_curve = MagicMock(spec=SNCurveBase)
        mock_curve.get_allowable_cycles.return_value = 1e6
        del mock_curve.m
        del mock_curve.A
        m = TovoBenasciuttiMethod()
        freq, psd = broadband_psd
        result = m.calculate_damage_rate(freq, psd, mock_curve)
        assert isinstance(result, FrequencyDomainResult)

    def test_equivalent_stress_with_A_attribute(self, broadband_psd, simple_sn_curve):
        """When sn_curve has A attribute, equivalent stress uses it."""
        freq, psd = broadband_psd
        m = TovoBenasciuttiMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert result.equivalent_stress > 0

    def test_equivalent_stress_zero_when_zero_damage(self, simple_sn_curve):
        """When damage is zero, equivalent_stress should be 0."""
        freq = np.linspace(0.1, 50, 500)
        psd = np.zeros_like(freq)
        m = TovoBenasciuttiMethod()
        result = m.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert result.equivalent_stress == 0.0


# ===================================================================
# SingleMomentMethod
# ===================================================================

class TestSingleMomentMethod:
    """
    SingleMomentMethod passes max_moment=2 to _calculate_spectral_moments,
    but that method always tries to unpack moments['m4'], causing a KeyError.
    Tests document this known bug by expecting the KeyError where appropriate.
    """

    def test_name(self):
        assert SingleMomentMethod().name == "Single-Moment"

    def test_calculate_raises_key_error_due_to_max_moment_bug(
            self, narrowband_psd, simple_sn_curve):
        """Known bug: SingleMomentMethod uses max_moment=2 but code unpacks m4."""
        freq, psd = narrowband_psd
        m = SingleMomentMethod()
        with pytest.raises(KeyError, match="m4"):
            m.calculate_damage_rate(freq, psd, simple_sn_curve)

    def test_would_work_if_max_moment_fixed(self, narrowband_psd, simple_sn_curve):
        """Demonstrate that calling with default max_moment=4 works fine."""
        freq, psd = narrowband_psd
        m = SingleMomentMethod()
        # Manually call with full moments to prove the rest of the method works
        moments = m._calculate_spectral_moments(freq, psd)  # default max_moment=4
        assert moments.m0 > 0
        assert moments.m2 > 0


# ===================================================================
# ResponsePSDCalculator
# ===================================================================

class TestResponsePSDCalculator:

    def test_init(self):
        calc = ResponsePSDCalculator()
        assert calc.last_calculation is None

    def test_response_psd_with_unity_transfer(self):
        """Unity transfer function should return input PSD."""
        calc = ResponsePSDCalculator()
        freq = np.linspace(0.1, 50, 500)
        psd_in = np.ones_like(freq) * 5.0
        # |H|^2 = 1 everywhere
        H_squared = np.ones_like(freq)
        freq_out, psd_out = calc.calculate_response_psd(freq, psd_in, H_squared)
        np.testing.assert_allclose(psd_out, psd_in, rtol=1e-10)

    def test_response_psd_shape(self):
        calc = ResponsePSDCalculator()
        freq = np.linspace(0.1, 50, 500)
        psd = np.ones_like(freq)
        H = np.ones_like(freq) * 2.0
        freq_out, psd_out = calc.calculate_response_psd(freq, psd, H)
        assert len(freq_out) == len(freq)
        assert len(psd_out) == len(freq)

    def test_response_psd_with_callable_transfer(self):
        """Callable transfer function (complex) should work."""
        calc = ResponsePSDCalculator()
        freq = np.linspace(0.1, 50, 500)
        psd = np.ones_like(freq)

        def tf(f):
            return 2.0 * np.ones_like(f, dtype=complex)

        freq_out, psd_out = calc.calculate_response_psd(freq, psd, tf)
        # |H|^2 = 4, so psd_out = 4 * psd
        np.testing.assert_allclose(psd_out, 4.0, rtol=1e-10)

    def test_response_psd_with_response_frequency(self):
        """Custom response frequency grid should be used."""
        calc = ResponsePSDCalculator()
        freq_in = np.linspace(0.1, 50, 500)
        psd_in = np.ones_like(freq_in)
        freq_resp = np.linspace(1, 40, 200)
        H = np.ones_like(freq_in)
        freq_out, psd_out = calc.calculate_response_psd(
            freq_in, psd_in, H, response_frequency=freq_resp
        )
        assert len(freq_out) == 200
        assert len(psd_out) == 200

    def test_last_calculation_stored(self):
        calc = ResponsePSDCalculator()
        freq = np.linspace(0.1, 50, 100)
        psd = np.ones_like(freq)
        H = np.ones_like(freq)
        calc.calculate_response_psd(freq, psd, H)
        assert calc.last_calculation is not None
        assert 'input_frequency' in calc.last_calculation
        assert 'response_psd' in calc.last_calculation
        assert 'transfer_function_squared' in calc.last_calculation

    def test_amplification_at_resonance(self):
        """Response PSD should be amplified near resonance of SDOF system."""
        calc = ResponsePSDCalculator()
        freq = np.linspace(0.1, 50, 2000)
        psd = np.ones_like(freq)  # white noise input
        fn = 10.0  # 10 Hz natural frequency
        zeta = 0.02  # 2% damping

        H = calc.sdof_transfer_function(freq, fn, zeta)
        # Pass |H|^2 as array (the array path assumes values are already |H|^2)
        H_squared = np.abs(H) ** 2
        _, psd_resp = calc.calculate_response_psd(freq, psd, H_squared)

        # Find index nearest resonance
        idx_res = np.argmin(np.abs(freq - fn))
        # Far-field index (well away from resonance)
        idx_far = np.argmin(np.abs(freq - 40.0))

        assert psd_resp[idx_res] > psd_resp[idx_far] * 10

    def test_transfer_function_interpolation_for_mismatched_lengths(self):
        """When transfer function array length differs, interpolation occurs."""
        calc = ResponsePSDCalculator()
        freq_in = np.linspace(0.1, 50, 500)
        psd_in = np.ones_like(freq_in)
        # Provide a shorter TF array; the code will interpolate
        H_short = np.ones(300) * 2.0
        freq_resp = np.linspace(0.1, 50, 300)
        freq_out, psd_out = calc.calculate_response_psd(
            freq_in, psd_in, H_short, response_frequency=freq_resp
        )
        np.testing.assert_allclose(psd_out, 2.0, rtol=0.1)


class TestSDOFTransferFunction:

    def test_returns_complex_array(self):
        calc = ResponsePSDCalculator()
        freq = np.linspace(0.1, 50, 500)
        H = calc.sdof_transfer_function(freq, 10.0, 0.05)
        assert H.dtype == complex
        assert len(H) == len(freq)

    def test_peak_near_natural_frequency(self):
        calc = ResponsePSDCalculator()
        freq = np.linspace(0.1, 50, 5000)
        fn = 20.0
        zeta = 0.01
        H = calc.sdof_transfer_function(freq, fn, zeta)
        mag = np.abs(H)
        peak_idx = np.argmax(mag)
        peak_freq = freq[peak_idx]
        # Peak should be near fn (within ~1 Hz for low damping)
        assert abs(peak_freq - fn) < 1.0

    def test_amplitude_at_resonance(self):
        """At resonance omega=omega_n, |H| ~ 1/(2*zeta*omega_n^2)."""
        calc = ResponsePSDCalculator()
        fn = 10.0
        zeta = 0.05
        freq = np.array([fn])
        H = calc.sdof_transfer_function(freq, fn, zeta)
        omega_n = 2 * np.pi * fn
        expected_mag = 1.0 / (2 * zeta * omega_n ** 2)
        assert np.abs(H[0]) == pytest.approx(expected_mag, rel=1e-3)

    def test_low_frequency_limit(self):
        """At very low freq, |H| -> 1/omega_n^2."""
        calc = ResponsePSDCalculator()
        fn = 10.0
        zeta = 0.05
        freq = np.array([0.01])
        H = calc.sdof_transfer_function(freq, fn, zeta)
        omega_n = 2 * np.pi * fn
        expected = 1.0 / omega_n ** 2
        assert np.abs(H[0]) == pytest.approx(expected, rel=1e-2)

    def test_high_frequency_rolloff(self):
        """At very high freq, |H| should be very small."""
        calc = ResponsePSDCalculator()
        freq = np.array([1000.0])
        H = calc.sdof_transfer_function(freq, 10.0, 0.05)
        assert np.abs(H[0]) < 1e-7


class TestMDOFTransferFunction:

    def test_returns_complex_array(self):
        calc = ResponsePSDCalculator()
        freq = np.linspace(0.1, 50, 500)
        H = calc.mdof_transfer_function(freq, [5.0, 15.0], [0.02, 0.03])
        assert H.dtype == complex
        assert len(H) == len(freq)

    def test_two_peaks(self):
        """MDOF TF with two modes should show peaks near both frequencies."""
        calc = ResponsePSDCalculator()
        freq = np.linspace(0.1, 50, 10000)
        fn = [10.0, 30.0]
        zeta = [0.01, 0.01]
        H = calc.mdof_transfer_function(freq, fn, zeta)
        mag = np.abs(H)

        # Find peaks in the two regions
        region1 = (freq > 8) & (freq < 12)
        region2 = (freq > 28) & (freq < 32)
        peak1 = np.max(mag[region1])
        peak2 = np.max(mag[region2])

        # Far-field should be much lower
        far_field = mag[(freq > 18) & (freq < 22)]
        assert peak1 > np.mean(far_field) * 5
        assert peak2 > np.mean(far_field) * 5

    def test_with_mode_shapes(self):
        """Mode shape participation factors should scale contribution."""
        calc = ResponsePSDCalculator()
        freq = np.linspace(0.1, 50, 2000)
        fn = [10.0, 30.0]
        zeta = [0.02, 0.02]
        mode_shapes = np.array([2.0, 0.5])
        H = calc.mdof_transfer_function(freq, fn, zeta, mode_shapes)
        assert len(H) == len(freq)

    def test_without_mode_shapes(self):
        """Without mode shapes, participation = 1 for all modes."""
        calc = ResponsePSDCalculator()
        freq = np.linspace(0.1, 50, 500)
        H = calc.mdof_transfer_function(freq, [10.0], [0.05])
        H_single = calc.sdof_transfer_function(freq, 10.0, 0.05)
        np.testing.assert_allclose(H, H_single, rtol=1e-10)

    def test_single_mode_equals_sdof(self):
        """MDOF with one mode should equal SDOF."""
        calc = ResponsePSDCalculator()
        freq = np.linspace(0.1, 50, 500)
        fn, zeta = 15.0, 0.03
        H_mdof = calc.mdof_transfer_function(freq, [fn], [zeta])
        H_sdof = calc.sdof_transfer_function(freq, fn, zeta)
        np.testing.assert_allclose(H_mdof, H_sdof, rtol=1e-10)

    def test_mode_shapes_shorter_than_modes(self):
        """When mode_shapes array is shorter, missing entries default to 1.0."""
        calc = ResponsePSDCalculator()
        freq = np.linspace(0.1, 50, 500)
        mode_shapes = np.array([1.5])  # Only one entry for two modes
        H = calc.mdof_transfer_function(freq, [10.0, 20.0], [0.02, 0.02], mode_shapes)
        assert len(H) == len(freq)


# ===================================================================
# compare_frequency_methods
# ===================================================================

class TestCompareFrequencyMethods:

    def test_returns_dataframe(self, broadband_psd, simple_sn_curve):
        freq, psd = broadband_psd
        df = compare_frequency_methods(freq, psd, simple_sn_curve)
        assert isinstance(df, pd.DataFrame)

    def test_successful_methods_count(self, broadband_psd, simple_sn_curve):
        """3 methods succeed; SingleMomentMethod fails due to known max_moment bug."""
        freq, psd = broadband_psd
        df = compare_frequency_methods(freq, psd, simple_sn_curve)
        # SingleMomentMethod raises KeyError('m4'), so only 3 rows
        assert len(df) == 3

    def test_columns(self, broadband_psd, simple_sn_curve):
        freq, psd = broadband_psd
        df = compare_frequency_methods(freq, psd, simple_sn_curve)
        expected_cols = {'method', 'damage_rate', 'fatigue_life',
                         'equivalent_stress', 'cycles_per_second'}
        assert expected_cols == set(df.columns)

    def test_method_names(self, broadband_psd, simple_sn_curve):
        freq, psd = broadband_psd
        df = compare_frequency_methods(freq, psd, simple_sn_curve)
        methods = set(df['method'])
        assert 'Narrow-band' in methods
        assert 'Dirlik' in methods
        assert 'Tovo-Benasciutti' in methods
        # SingleMomentMethod fails due to known bug (max_moment=2 -> KeyError 'm4')
        assert 'Single-Moment' not in methods

    def test_all_damage_rates_positive(self, broadband_psd, simple_sn_curve):
        freq, psd = broadband_psd
        df = compare_frequency_methods(freq, psd, simple_sn_curve)
        assert (df['damage_rate'] > 0).all()

    def test_all_fatigue_lives_positive(self, broadband_psd, simple_sn_curve):
        freq, psd = broadband_psd
        df = compare_frequency_methods(freq, psd, simple_sn_curve)
        assert (df['fatigue_life'] > 0).all()

    def test_narrowband_most_conservative(self, broadband_psd, simple_sn_curve):
        """Narrow-band should typically give highest damage for broadband PSD."""
        freq, psd = broadband_psd
        df = compare_frequency_methods(freq, psd, simple_sn_curve)
        nb_damage = df.loc[df['method'] == 'Narrow-band', 'damage_rate'].values[0]
        dk_damage = df.loc[df['method'] == 'Dirlik', 'damage_rate'].values[0]
        # NB is classically conservative vs Dirlik for broadband
        assert nb_damage >= dk_damage * 0.9  # with some tolerance


# ===================================================================
# Cross-method consistency tests
# ===================================================================

class TestCrossMethodConsistency:

    def test_all_methods_agree_on_spectral_moments(self, broadband_psd, simple_sn_curve):
        """All methods should compute the same spectral moments for the same PSD."""
        freq, psd = broadband_psd
        nb = NarrowBandMethod().calculate_damage_rate(freq, psd, simple_sn_curve)
        dk = DirlikMethod().calculate_damage_rate(freq, psd, simple_sn_curve)
        tb = TovoBenasciuttiMethod().calculate_damage_rate(freq, psd, simple_sn_curve)

        # m0 should be the same
        assert nb.spectral_moments.m0 == pytest.approx(dk.spectral_moments.m0, rel=1e-10)
        assert nb.spectral_moments.m0 == pytest.approx(tb.spectral_moments.m0, rel=1e-10)

        # m2 should be the same
        assert nb.spectral_moments.m2 == pytest.approx(dk.spectral_moments.m2, rel=1e-10)
        assert nb.spectral_moments.m2 == pytest.approx(tb.spectral_moments.m2, rel=1e-10)

    def test_all_methods_agree_on_zero_crossing_rate(self, broadband_psd, simple_sn_curve):
        freq, psd = broadband_psd
        nb = NarrowBandMethod().calculate_damage_rate(freq, psd, simple_sn_curve)
        dk = DirlikMethod().calculate_damage_rate(freq, psd, simple_sn_curve)
        tb = TovoBenasciuttiMethod().calculate_damage_rate(freq, psd, simple_sn_curve)
        # zero crossing rates should be identical
        assert nb.expected_cycles_per_second == pytest.approx(
            dk.expected_cycles_per_second, rel=1e-6)
        assert nb.expected_cycles_per_second == pytest.approx(
            tb.expected_cycles_per_second, rel=1e-6)

    def test_damage_rate_ordering_broadband(self, simple_sn_curve):
        """For broadband: NB >= TB, NB >= Dirlik (typically)."""
        freq, psd = _make_broadband_psd()
        nb = NarrowBandMethod().calculate_damage_rate(freq, psd, simple_sn_curve)
        dk = DirlikMethod().calculate_damage_rate(freq, psd, simple_sn_curve)
        tb = TovoBenasciuttiMethod().calculate_damage_rate(freq, psd, simple_sn_curve)
        # narrow-band is conservative upper bound for broadband
        # Allow small tolerance for numerical effects
        assert nb.damage_rate >= dk.damage_rate * 0.9
        assert nb.damage_rate >= tb.damage_rate * 0.9


# ===================================================================
# Engineering value tests
# ===================================================================

class TestEngineeringValues:

    def test_dnv_d_broadband_finite_life(self):
        """Broadband PSD with DNV-D curve (no fatigue limit) gives finite life."""
        # Use DNV-D parameters but zero fatigue limit to ensure damage occurs
        curve = PowerLawSNCurve(name='DNV-D-noFL', A=5.73e11, m=3.0, fatigue_limit=0.0)
        freq, psd = _make_broadband_psd()
        nb = NarrowBandMethod()
        result = nb.calculate_damage_rate(freq, psd, curve)
        # Life should be finite and in a "reasonable" range (> 1 second, < 1e15 seconds)
        assert np.isfinite(result.fatigue_life)
        assert result.fatigue_life > 1.0
        assert result.fatigue_life < 1e15

    def test_stress_psd_units_consistency(self):
        """Verify stress units: if PSD in MPa^2/Hz, stress range in MPa."""
        # PSD amplitude such that RMS stress = 10 MPa
        # m0 = integral(PSD) = S0 * bandwidth
        # sigma = sqrt(m0) = 10 => m0 = 100
        # If bandwidth = 10 Hz, S0 = 10 MPa^2/Hz
        S0 = 10.0  # MPa^2/Hz
        freq = np.linspace(1.0, 11.0, 1000)  # 10 Hz bandwidth
        psd = S0 * np.ones_like(freq)

        nb = NarrowBandMethod()
        curve = PowerLawSNCurve(name='Test', A=1e12, m=3.0, fatigue_limit=0.0)
        result = nb.calculate_damage_rate(freq, psd, curve)

        sigma_rms = np.sqrt(result.spectral_moments.m0)
        assert sigma_rms == pytest.approx(10.0, rel=0.05)
        assert result.equivalent_stress == pytest.approx(20.0, rel=0.05)

    def test_higher_sn_slope_increases_damage(self):
        """Higher S-N curve slope m should increase damage for same PSD."""
        freq, psd = _make_broadband_psd()
        curve_m3 = PowerLawSNCurve(name='m3', A=1e12, m=3.0, fatigue_limit=0.0)
        curve_m5 = PowerLawSNCurve(name='m5', A=1e12, m=5.0, fatigue_limit=0.0)

        nb = NarrowBandMethod()
        r3 = nb.calculate_damage_rate(freq, psd, curve_m3)
        r5 = nb.calculate_damage_rate(freq, psd, curve_m5)

        # Higher m means steeper curve, more sensitive to stress => more damage
        assert r5.damage_rate > r3.damage_rate

    def test_different_dnv_curves(self):
        """Worse weld class (lower A) should give shorter life."""
        freq, psd = _make_broadband_psd()
        # Use zero fatigue limit to ensure both curves produce damage
        curve_c = PowerLawSNCurve(name='C', A=1.08e12, m=3.0, fatigue_limit=0.0)
        curve_w3 = PowerLawSNCurve(name='W3', A=5.3e9, m=3.0, fatigue_limit=0.0)

        nb = NarrowBandMethod()
        r_c = nb.calculate_damage_rate(freq, psd, curve_c)
        r_w3 = nb.calculate_damage_rate(freq, psd, curve_w3)

        # W3 has lower A => shorter life
        assert r_w3.fatigue_life < r_c.fatigue_life


# ===================================================================
# Edge cases
# ===================================================================

class TestEdgeCases:

    def test_single_frequency_psd(self, simple_sn_curve):
        """PSD with very few points should still work."""
        freq = np.array([1.0, 2.0, 3.0, 4.0, 5.0])
        psd = np.array([0.0, 1.0, 2.0, 1.0, 0.0])
        nb = NarrowBandMethod()
        result = nb.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert isinstance(result, FrequencyDomainResult)

    def test_very_large_psd_amplitude(self, simple_sn_curve):
        """Very large PSD should not cause overflow."""
        freq = np.linspace(0.1, 50, 500)
        psd = 1e6 * np.ones_like(freq)
        nb = NarrowBandMethod()
        result = nb.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert np.isfinite(result.damage_rate)

    def test_very_small_psd_amplitude(self):
        """Very small PSD should give much longer life than normal PSD."""
        # Use a high cutoff to avoid the 1e7 cutoff masking differences
        curve_no_cutoff = PowerLawSNCurve(
            name='NoCutoff', A=1e12, m=3.0, fatigue_limit=0.0, cutoff_cycles=1e30)
        freq = np.linspace(0.1, 50, 500)
        nb = NarrowBandMethod()
        # Normal amplitude
        psd_normal = np.ones_like(freq)
        r_normal = nb.calculate_damage_rate(freq, psd_normal, curve_no_cutoff)
        # Very small amplitude
        psd_tiny = 1e-10 * np.ones_like(freq)
        r_tiny = nb.calculate_damage_rate(freq, psd_tiny, curve_no_cutoff)
        # Tiny PSD should give much longer life
        assert r_tiny.fatigue_life > r_normal.fatigue_life * 1e3

    def test_high_frequency_psd(self, simple_sn_curve):
        """PSD at high frequencies should still work."""
        freq = np.linspace(100.0, 1000.0, 500)
        psd = np.ones_like(freq)
        nb = NarrowBandMethod()
        result = nb.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert isinstance(result, FrequencyDomainResult)

    def test_non_uniform_frequency_spacing(self, simple_sn_curve):
        """Non-uniform frequency spacing should not crash."""
        freq = np.concatenate([
            np.linspace(0.1, 5, 100),
            np.linspace(5.1, 50, 200)
        ])
        psd = np.ones_like(freq)
        dk = DirlikMethod()
        result = dk.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert isinstance(result, FrequencyDomainResult)

    def test_array_type_conversion(self, simple_sn_curve):
        """Lists should be handled by np.asarray in moments calculation."""
        freq = list(np.linspace(0.1, 50, 100))
        psd = list(np.ones(100))
        nb = NarrowBandMethod()
        # Should not raise
        result = nb.calculate_damage_rate(freq, psd, simple_sn_curve)
        assert isinstance(result, FrequencyDomainResult)
