#!/usr/bin/env python3
"""
ABOUTME: Tests for OpenFOAM spectral analysis - validates FFT/Welch dominant
natural-frequency extraction against synthetic signals of known frequency and
cross-checks the analytical prismatic-tank tanh sloshing formula.
"""

import math

import numpy as np
import pytest

from digitalmodel.solvers.openfoam.spectral_analysis import (
    GRAVITY,
    SpectrumResult,
    compute_fft_spectrum,
    extract_natural_frequency,
    prismatic_tank_natural_frequency,
)


SEED = 12345


# ============================================================================
# Synthetic signal helpers
# ============================================================================


def _make_time(duration: float, sample_rate: float) -> np.ndarray:
    n = int(round(duration * sample_rate))
    return np.arange(n) / sample_rate


# ============================================================================
# (a) Single-tone recovery - the core oracle
# ============================================================================


def test_single_tone_fft_recovers_known_frequency():
    """sin(2*pi*f*t) at known f -> FFT peak within one frequency bin."""
    f_true = 1.3  # Hz
    fs = 50.0
    duration = 60.0
    t = _make_time(duration, fs)
    signal = 2.5 * np.sin(2.0 * np.pi * f_true * t)

    result = extract_natural_frequency(signal, times=t, method="fft", min_frequency=0.1)

    bin_width = fs / signal.size
    assert isinstance(result, SpectrumResult)
    assert abs(result.dominant_frequency - f_true) <= bin_width
    # Period sanity.
    assert abs(result.dominant_period - 1.0 / f_true) < 0.05


def test_single_tone_welch_recovers_known_frequency():
    """Welch PSD also recovers the known tone within its (coarser) bin."""
    f_true = 0.8  # Hz
    fs = 40.0
    duration = 80.0
    t = _make_time(duration, fs)
    signal = np.sin(2.0 * np.pi * f_true * t)

    result = extract_natural_frequency(
        signal, sample_rate=fs, method="welch", min_frequency=0.05
    )
    # Welch with nperseg=256 -> bin width fs/256.
    welch_bin = fs / 256
    assert abs(result.dominant_frequency - f_true) <= 2 * welch_bin


def test_fft_amplitude_is_preserved():
    """Hann-windowed one-sided FFT preserves the tone amplitude (~A)."""
    f_true = 2.0
    fs = 64.0
    amp_true = 3.7
    t = _make_time(40.0, fs)
    signal = amp_true * np.sin(2.0 * np.pi * f_true * t)

    freqs, spectrum = compute_fft_spectrum(signal, fs)
    peak_amp = spectrum[np.argmax(spectrum)]
    assert peak_amp == pytest.approx(amp_true, rel=0.05)


def test_dc_offset_does_not_become_dominant():
    """A large mean offset must be detrended away, not reported as f_n."""
    f_true = 1.1
    fs = 50.0
    t = _make_time(60.0, fs)
    signal = 500.0 + np.sin(2.0 * np.pi * f_true * t)

    result = extract_natural_frequency(signal, times=t, method="fft", min_frequency=0.1)
    bin_width = fs / signal.size
    assert abs(result.dominant_frequency - f_true) <= bin_width


# ============================================================================
# (b) Multi-tone: top-2 peaks
# ============================================================================


def test_multitone_top_two_peaks():
    """Two-tone signal -> top-2 detected peaks match the two input freqs."""
    rng = np.random.default_rng(SEED)
    f1, f2 = 0.7, 2.4  # Hz; f1 stronger
    fs = 50.0
    t = _make_time(120.0, fs)
    signal = (
        3.0 * np.sin(2.0 * np.pi * f1 * t)
        + 1.5 * np.sin(2.0 * np.pi * f2 * t)
        + 0.05 * rng.standard_normal(t.size)
    )

    result = extract_natural_frequency(
        signal, times=t, method="fft", n_peaks=2, min_frequency=0.1
    )
    bin_width = fs / signal.size
    top2 = sorted(result.top_frequencies(2))

    assert len(top2) == 2
    assert abs(top2[0] - f1) <= 2 * bin_width
    assert abs(top2[1] - f2) <= 2 * bin_width
    # Stronger tone (f1) is dominant.
    assert abs(result.dominant_frequency - f1) <= 2 * bin_width


# ============================================================================
# (c) Analytical tanh dispersion + FFT of a sloshing-like signal
# ============================================================================


def test_prismatic_tank_deep_water_limit():
    """Deep water (h >> L): tanh -> 1, so omega^2 -> g*k (deep-water wave)."""
    L = 1.0
    h = 100.0  # h/L = 100 -> tanh ~ 1
    f_n = prismatic_tank_natural_frequency(L, h, mode=1)
    k = math.pi / L
    omega_deep = math.sqrt(GRAVITY * k)
    assert f_n == pytest.approx(omega_deep / (2 * math.pi), rel=1e-6)


def test_prismatic_tank_shallow_water_limit():
    """Shallow water (h << L): tanh(x)~x, omega^2 -> g*k^2*h => c = sqrt(g*h)."""
    L = 100.0
    h = 0.5  # h/L = 0.005
    f_n = prismatic_tank_natural_frequency(L, h, mode=1)
    k = math.pi / L
    omega_shallow = k * math.sqrt(GRAVITY * h)  # omega = k*sqrt(g h)
    assert f_n == pytest.approx(omega_shallow / (2 * math.pi), rel=1e-3)


def test_analytical_matches_fft_of_synthetic_sloshing_signal():
    """Build eta(t) at the analytical f_n; FFT must recover that frequency.

    This closes the validation loop required by the issue: the analytical
    tanh formula provides the known frequency, and the FFT of a synthetic
    free-surface-elevation signal at that frequency must recover it.
    """
    L = 2.0  # tank length (m)
    h = 0.5  # fill depth (m)
    f_n = prismatic_tank_natural_frequency(L, h, mode=1)

    fs = 100.0
    t = _make_time(120.0, fs)
    # Synthetic first-mode free-surface elevation oscillating at f_n.
    eta = 0.02 * np.cos(2.0 * np.pi * f_n * t)

    result = extract_natural_frequency(eta, times=t, method="fft", min_frequency=0.05)
    bin_width = fs / eta.size
    assert abs(result.dominant_frequency - f_n) <= 2 * bin_width


def test_fill_level_monotonicity():
    """Natural frequency increases monotonically with fill depth (fixed L)."""
    L = 2.0
    depths = [0.1, 0.3, 0.6, 1.0, 1.5]
    freqs = [prismatic_tank_natural_frequency(L, h) for h in depths]
    assert all(b > a for a, b in zip(freqs, freqs[1:]))


def test_higher_modes_increase_frequency():
    """Mode n=2 has a higher natural frequency than n=1."""
    L, h = 3.0, 1.0
    f1 = prismatic_tank_natural_frequency(L, h, mode=1)
    f2 = prismatic_tank_natural_frequency(L, h, mode=2)
    assert f2 > f1


# ============================================================================
# Input validation
# ============================================================================


def test_requires_times_or_sample_rate():
    with pytest.raises(ValueError):
        extract_natural_frequency(np.zeros(10))


def test_non_increasing_time_rejected():
    t = np.array([0.0, 0.1, 0.1, 0.2])
    with pytest.raises(ValueError):
        extract_natural_frequency(np.zeros(4), times=t)


@pytest.mark.parametrize("bad", [(0.0, 1.0, 1), (1.0, 0.0, 1), (1.0, 1.0, 0)])
def test_analytical_validation(bad):
    L, h, mode = bad
    with pytest.raises(ValueError):
        prismatic_tank_natural_frequency(L, h, mode=mode)
