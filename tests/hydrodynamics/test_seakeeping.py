#!/usr/bin/env python3
"""
ABOUTME: Tests for the hydrodynamics seakeeping module — 6-DOF motion analysis.
ABOUTME: Covers response spectra, spectral moments, motion statistics, exceedance, operability.
"""

import math

import numpy as np
import pytest

from digitalmodel.hydrodynamics.seakeeping import (
    compute_response_spectrum,
    motion_exceedance,
    operability_analysis,
    significant_amplitude,
    spectral_moments,
)
from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def simple_rao():
    """Unity RAO across all frequencies — response = wave spectrum."""
    frequencies = np.linspace(0.1, 2.0, 50)
    rao_amplitudes = np.ones_like(frequencies)
    return frequencies, rao_amplitudes


@pytest.fixture
def constant_spectrum():
    """Flat (white-noise) wave spectrum for analytical verification."""
    frequencies = np.linspace(0.1, 2.0, 200)
    # S(omega) = 1.0 m^2*s across the band
    spectrum = np.ones_like(frequencies)
    return frequencies, spectrum


@pytest.fixture
def jonswap_spectrum():
    """Realistic JONSWAP spectrum: Hs=3m, Tp=10s."""
    ws = WaveSpectra()
    return ws.jonswap(hs=3.0, tp=10.0, n_points=200)


# ---------------------------------------------------------------------------
# compute_response_spectrum
# ---------------------------------------------------------------------------

class TestComputeResponseSpectrum:

    def test_unity_rao_returns_wave_spectrum(self, constant_spectrum):
        freq, S_wave = constant_spectrum
        rao = np.ones_like(freq)
        S_resp = compute_response_spectrum(rao, S_wave)
        np.testing.assert_allclose(S_resp, S_wave)

    def test_zero_rao_returns_zero_spectrum(self, constant_spectrum):
        freq, S_wave = constant_spectrum
        rao = np.zeros_like(freq)
        S_resp = compute_response_spectrum(rao, S_wave)
        np.testing.assert_allclose(S_resp, np.zeros_like(S_wave))

    def test_scaling_by_rao_squared(self, constant_spectrum):
        freq, S_wave = constant_spectrum
        rao = np.full_like(freq, 2.0)
        S_resp = compute_response_spectrum(rao, S_wave)
        np.testing.assert_allclose(S_resp, 4.0 * S_wave)

    def test_mismatched_lengths_raises(self):
        with pytest.raises(ValueError, match="same length"):
            compute_response_spectrum(np.ones(5), np.ones(10))


# ---------------------------------------------------------------------------
# spectral_moments
# ---------------------------------------------------------------------------

class TestSpectralMoments:

    def test_m0_of_flat_spectrum(self, constant_spectrum):
        freq, S = constant_spectrum
        # m0 = integral(S dw) ≈ 1.0 * (2.0 - 0.1) = 1.9
        moments = spectral_moments(freq, S, orders=[0])
        assert moments[0] == pytest.approx(1.9, rel=0.02)

    def test_m2_is_larger_than_m0(self, constant_spectrum):
        freq, S = constant_spectrum
        moments = spectral_moments(freq, S, orders=[0, 2])
        assert moments[2] > moments[0]

    def test_default_orders(self, constant_spectrum):
        freq, S = constant_spectrum
        moments = spectral_moments(freq, S)
        assert set(moments.keys()) == {0, 2, 4}

    def test_jonswap_m0_matches_hs(self, jonswap_spectrum):
        """For a well-formed spectrum, Hs = 4*sqrt(m0)."""
        freq, S = jonswap_spectrum
        moments = spectral_moments(freq, S, orders=[0])
        hs_recovered = 4.0 * math.sqrt(moments[0])
        assert hs_recovered == pytest.approx(3.0, rel=0.05)

    def test_negative_order_accepted(self, constant_spectrum):
        """Negative spectral moment orders are valid (used in crest analysis)."""
        freq, S = constant_spectrum
        moments = spectral_moments(freq, S, orders=[-1, 0])
        assert -1 in moments


# ---------------------------------------------------------------------------
# significant_amplitude
# ---------------------------------------------------------------------------

class TestSignificantAmplitude:

    def test_known_m0(self):
        """s_{1/3} = 2 * sqrt(m0) for single-sided."""
        m0 = 4.0
        assert significant_amplitude(m0) == pytest.approx(4.0)  # 2*sqrt(4)

    def test_zero_m0(self):
        assert significant_amplitude(0.0) == 0.0

    def test_negative_m0_raises(self):
        with pytest.raises(ValueError, match="non-negative"):
            significant_amplitude(-1.0)


# ---------------------------------------------------------------------------
# motion_exceedance
# ---------------------------------------------------------------------------

class TestMotionExceedance:

    def test_zero_threshold_gives_unity(self):
        """P(X > 0) = 1 for Rayleigh."""
        assert motion_exceedance(m0=1.0, threshold=0.0) == pytest.approx(1.0)

    def test_high_threshold_near_zero(self):
        """Very large threshold should give near-zero exceedance."""
        p = motion_exceedance(m0=1.0, threshold=100.0)
        assert p < 1e-10

    def test_known_rayleigh_value(self):
        """P(X > sqrt(2*m0)) = exp(-1) for Rayleigh."""
        m0 = 2.0
        threshold = math.sqrt(2 * m0)  # = 2.0
        expected = math.exp(-1)
        assert motion_exceedance(m0, threshold) == pytest.approx(expected, rel=1e-6)

    def test_negative_m0_raises(self):
        with pytest.raises(ValueError, match="positive"):
            motion_exceedance(m0=-1.0, threshold=1.0)


# ---------------------------------------------------------------------------
# operability_analysis
# ---------------------------------------------------------------------------

class TestOperabilityAnalysis:

    @pytest.fixture
    def scatter_diagram(self):
        """Minimal scatter diagram: (Hs, Tp, probability)."""
        return [
            {"hs": 1.0, "tp": 6.0, "probability": 0.30},
            {"hs": 2.0, "tp": 8.0, "probability": 0.40},
            {"hs": 3.0, "tp": 10.0, "probability": 0.20},
            {"hs": 5.0, "tp": 12.0, "probability": 0.10},
        ]

    @pytest.fixture
    def unity_raos(self):
        """RAO amplitude = 1 at all frequencies."""
        frequencies = np.linspace(0.1, 2.0, 100)
        rao_amplitudes = np.ones_like(frequencies)
        return frequencies, rao_amplitudes

    def test_very_large_criterion_all_operable(self, scatter_diagram, unity_raos):
        """If the motion criterion is very large, operability ~ 100%."""
        criteria = {"heave": 999.0}
        result = operability_analysis(
            rao_freqs=unity_raos[0],
            rao_amplitudes=unity_raos[1],
            scatter_diagram=scatter_diagram,
            criteria=criteria,
        )
        assert result["operability_pct"] == pytest.approx(100.0, abs=0.1)

    def test_very_small_criterion_low_operability(self, scatter_diagram, unity_raos):
        """If the motion criterion is tiny, operability should be low."""
        criteria = {"heave": 0.001}
        result = operability_analysis(
            rao_freqs=unity_raos[0],
            rao_amplitudes=unity_raos[1],
            scatter_diagram=scatter_diagram,
            criteria=criteria,
        )
        assert result["operability_pct"] < 10.0

    def test_result_keys(self, scatter_diagram, unity_raos):
        criteria = {"heave": 2.0}
        result = operability_analysis(
            rao_freqs=unity_raos[0],
            rao_amplitudes=unity_raos[1],
            scatter_diagram=scatter_diagram,
            criteria=criteria,
        )
        assert "operability_pct" in result
        assert "sea_state_results" in result

    def test_probabilities_must_be_nonneg(self, unity_raos):
        bad_scatter = [{"hs": 1.0, "tp": 6.0, "probability": -0.1}]
        with pytest.raises(ValueError, match="probability"):
            operability_analysis(
                rao_freqs=unity_raos[0],
                rao_amplitudes=unity_raos[1],
                scatter_diagram=bad_scatter,
                criteria={"heave": 2.0},
            )
