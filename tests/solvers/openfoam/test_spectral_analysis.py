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
    SloshingFrequencyResult,
    SpectrumResult,
    compute_fft_spectrum,
    extract_natural_frequency,
    prismatic_tank_natural_frequency,
    sloshing_natural_frequency,
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


# ============================================================================
# (d) sloshing_natural_frequency wiring = the regression gate (#660)
# ============================================================================
#
# Tolerance rationale: the FFT resolves frequency onto discrete bins of width
# df = fs / N. The measured dominant frequency can therefore differ from the
# true/analytical value by up to ~one bin. We size each synthetic record so
# that df is a small fraction of f_n and assert agreement within a stated
# percent tolerance that is comfortably above the bin-quantisation floor.


def test_sloshing_pipeline_pure_sinusoid_at_analytical_fn():
    """Pure first-mode signal at the analytical f_n -> measured == analytical.

    Regression gate: the wired pipeline must recover the analytical tanh
    frequency from a clean sinusoid within the FFT bin resolution.
    """
    L, h = 2.0, 0.5
    f_n = prismatic_tank_natural_frequency(L, h, mode=1)

    fs = 100.0
    duration = 200.0
    t = _make_time(duration, fs)
    eta = 0.02 * np.sin(2.0 * np.pi * f_n * t)

    res = sloshing_natural_frequency(
        eta, length=L, fill_depth=h, times=t, method="fft", min_frequency=0.05
    )

    bin_width = fs / eta.size  # ~0.005 Hz
    tol_pct = 100.0 * bin_width / f_n  # bin-resolution floor as a percentage
    assert isinstance(res, SloshingFrequencyResult)
    assert res.analytical_frequency == pytest.approx(f_n, rel=1e-12)
    # Measured recovers the analytical value within one FFT bin.
    assert res.abs_percent_error <= max(tol_pct, 1.0)
    assert res.ratio == pytest.approx(1.0, abs=max(tol_pct, 1.0) / 100.0)


def test_sloshing_pipeline_decaying_signal_within_tolerance():
    """Decaying sloshing-like transient at f_n -> recovered within <=3%.

    A real free-decay sloshing record rings down; the exponential envelope
    broadens the spectral peak. We assert the wired pipeline still recovers
    the analytical natural frequency within 3% (well above the bin floor).
    """
    L, h = 3.0, 1.2
    f_n = prismatic_tank_natural_frequency(L, h, mode=1)

    fs = 100.0
    duration = 240.0
    t = _make_time(duration, fs)
    # Decaying first-mode elevation: A * exp(-zeta*w*t) * cos(w t), light damping.
    omega = 2.0 * np.pi * f_n
    zeta = 0.01  # 1% critical -> lightly damped, peak stays sharp
    eta = 0.03 * np.exp(-zeta * omega * t) * np.cos(omega * t)

    res = sloshing_natural_frequency(
        eta, length=L, fill_depth=h, times=t, method="fft", min_frequency=0.02
    )

    assert res.abs_percent_error <= 3.0
    assert 0.97 <= res.ratio <= 1.03


def test_sloshing_pipeline_accepts_sample_rate_without_times():
    """dt/sample_rate path works without an explicit time vector."""
    L, h = 2.5, 0.8
    f_n = prismatic_tank_natural_frequency(L, h, mode=1)
    fs = 80.0
    t = _make_time(160.0, fs)
    signal = np.cos(2.0 * np.pi * f_n * t)

    res = sloshing_natural_frequency(
        signal, length=L, fill_depth=h, sample_rate=fs, min_frequency=0.02
    )
    assert res.abs_percent_error <= 3.0


def test_sloshing_pipeline_superposed_modes_locks_dominant():
    """Two superposed modes (n=1 stronger) -> pipeline locks the n=1 analytical.

    The stronger first-mode tone must dominate the spectrum and agree with the
    n=1 analytical value; the weaker n=2 tone must not capture the report.
    """
    L, h = 4.0, 1.5
    f1 = prismatic_tank_natural_frequency(L, h, mode=1)
    f2 = prismatic_tank_natural_frequency(L, h, mode=2)

    fs = 120.0
    t = _make_time(240.0, fs)
    eta = (
        0.04 * np.sin(2.0 * np.pi * f1 * t)  # dominant first mode
        + 0.015 * np.sin(2.0 * np.pi * f2 * t)  # weaker second mode
    )

    res = sloshing_natural_frequency(
        eta, length=L, fill_depth=h, mode=1, times=t, min_frequency=0.02
    )
    # Dominant measured frequency is the first mode, matching the n=1 analytical.
    assert res.abs_percent_error <= 3.0
    # Second-mode analytical is materially higher, confirming we locked mode 1.
    assert f2 > 1.5 * f1


def test_sloshing_pipeline_percent_error_is_signed():
    """percent_error carries the sign of (measured - analytical).

    Drive the signal at a frequency deliberately ~10% above the analytical f_n
    and confirm the reported percent_error is positive and ~+10%.
    """
    L, h = 2.0, 0.5
    f_n = prismatic_tank_natural_frequency(L, h, mode=1)
    f_drive = 1.10 * f_n  # 10% high on purpose

    fs = 100.0
    t = _make_time(200.0, fs)
    signal = np.sin(2.0 * np.pi * f_drive * t)

    res = sloshing_natural_frequency(
        signal, length=L, fill_depth=h, times=t, min_frequency=0.02
    )
    assert res.percent_error > 0.0
    assert res.percent_error == pytest.approx(10.0, abs=1.0)
    assert res.ratio == pytest.approx(1.10, abs=0.01)
