# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Hand-derived checks for a rectangular mask on the real DFT.

Basis: for integer k, sum(exp(2*pi*i*k*n/N), n=0..N-1) is N when
k is a multiple of N and zero otherwise (finite geometric sum). Thus
distinct exact-bin sine/cosine components are orthogonal; retaining a bin
retains its original component with its phase and amplitude. Expected
traces below use those components directly, never an FFT reference result.

All record lengths, rates, amplitudes, bin indices, bounds and invalid
examples are synthetic test controls. The absolute tolerance of 1e-12
allows accumulated double-precision trigonometric and transform roundoff;
relative tolerance is zero so rejected components receive no relative slack.
"""

import numpy as np
import pytest

from digitalmodel.signal_processing.signal_analysis.filters.frequency import (
    FrequencyFilter,
)


@pytest.mark.parametrize(
    "low_cutoff, high_cutoff, retained",
    [
        (None, 12, (0, 1)),
        (12, None, (1, 2)),
        (4, 12, (0, 1)),
        (12, 12, (1,)),
        (5, 11, ()),
    ],
    ids=["lowpass", "highpass", "bandpass", "single-bin", "empty-band"],
)
def test_fft_mask_recovers_selected_tones(low_cutoff, high_cutoff, retained):
    """N=fs=64 gives 1 Hz bins: 2*sin(4), 3*cos(12), 5*sin(24).

    The low/high bounds coincide with component bins to check inclusivity.
    A phase offset pi/3 on the cosine also checks phase preservation.
    Orthogonality leaves exactly the listed retained components.
    """
    time = np.arange(64) / 64
    components = [
        2 * np.sin(2 * np.pi * 4 * time),
        3 * np.cos(2 * np.pi * 12 * time + np.pi / 3),
        5 * np.sin(2 * np.pi * 24 * time),
    ]
    signal = sum(components)
    original = signal.copy()
    expected = sum((components[k] for k in retained), np.zeros_like(signal))

    filtered = FrequencyFilter().fft_filter(signal, 64, low_cutoff, high_cutoff)

    np.testing.assert_allclose(filtered, expected, rtol=0, atol=1e-12)
    np.testing.assert_array_equal(signal, original)
    assert filtered.shape == signal.shape
    assert np.isrealobj(filtered)


@pytest.mark.parametrize(
    "low_cutoff, high_cutoff, component",
    [(None, None, "all"), (0, 31.5, "all"), (None, 0, "dc"),
     (0, 0, "dc"), (5, 5, "tone")],
)
def test_fft_mask_preserves_odd_length_and_dc(low_cutoff, high_cutoff, component):
    """N=fs=63: DC=7, tone bin 5; Nyquist=fs/2=31.5 is not a bin.

    An inverse without explicit N would return 62 samples. Orthogonality
    makes the zero bin the mean, so selecting only zero returns constant 7.
    """
    tone = 2 * np.sin(2 * np.pi * 5 * np.arange(63) / 63)
    signal = 7 + tone
    original = signal.copy()
    expected = {"all": signal, "dc": np.full(63, 7), "tone": tone}[component]

    filtered = FrequencyFilter().fft_filter(signal, 63, low_cutoff, high_cutoff)

    assert filtered.shape == signal.shape
    np.testing.assert_allclose(filtered, expected, rtol=0, atol=1e-12)
    np.testing.assert_array_equal(signal, original)


@pytest.mark.parametrize(
    "low_cutoff, high_cutoff, component",
    [(None, None, "all"), (0, 32, "all"), (32, None, "nyquist"),
     (32, 32, "nyquist"), (0, 0, "dc"), (None, 32, "all")],
)
def test_fft_mask_preserves_dc_and_even_nyquist(low_cutoff, high_cutoff, component):
    """N=fs=64: DC=7; cos(pi*n)=(-1)**n has bin 32 and amplitude 3.

    These real endpoint bins must not be doubled, halved or dropped.
    """
    nyquist = 3 * (-1.0) ** np.arange(64)
    signal = 7 + nyquist
    original = signal.copy()
    expected = {"all": signal, "nyquist": nyquist, "dc": np.full(64, 7)}[component]

    filtered = FrequencyFilter().fft_filter(signal, 64, low_cutoff, high_cutoff)

    np.testing.assert_allclose(filtered, expected, rtol=0, atol=1e-12)
    np.testing.assert_array_equal(signal, original)


@pytest.mark.parametrize("signal", [[7], [1, -2, 3], np.array([1, -2, 3], dtype=np.float32)])
def test_fft_mask_identity_accepts_real_lists_and_arrays(signal):
    """Unbounded masks retain every bin, including a singleton's DC bin."""
    original = np.array(signal, copy=True)
    filtered = FrequencyFilter().fft_filter(signal, 8)
    np.testing.assert_allclose(filtered, original, rtol=0, atol=1e-12)
    np.testing.assert_array_equal(signal, original)
    assert isinstance(filtered, np.ndarray)
    assert not np.shares_memory(filtered, np.asarray(signal))


@pytest.mark.parametrize(
    "sampling_rate",
    [np.nextafter(0.0, 1.0), np.finfo(float).tiny, np.finfo(float).max],
)
def test_fft_mask_accepts_extreme_finite_sampling_rates(sampling_rate):
    """Identity is independent of the positive rate, even at float limits."""
    signal = np.array([1, -2, 3])
    np.testing.assert_allclose(
        FrequencyFilter().fft_filter(signal, sampling_rate), signal, rtol=0, atol=1e-12
    )


@pytest.mark.parametrize(
    "length, sampling_rate, cutoff", [(49, 49, 1), (14, 0.6, 0.3)]
)
def test_fft_mask_includes_boundary_bins_at_other_record_sizes(
    length, sampling_rate, cutoff
):
    """Hand bins: fs=N=49 gives 1 Hz; fs=0.6, N=14 gives Nyquist 0.3.

    These controls exercise rounding in the frequency grid. A single-bin
    mask at the cosine's frequency must retain the entire cosine.
    """
    signal = np.cos(2 * np.pi * cutoff * np.arange(length) / sampling_rate)
    original = signal.copy()
    filtered = FrequencyFilter().fft_filter(signal, sampling_rate, cutoff, cutoff)
    np.testing.assert_allclose(filtered, original, rtol=0, atol=1e-12)
    np.testing.assert_array_equal(signal, original)


@pytest.mark.parametrize(
    "signal, sampling_rate, low_cutoff, high_cutoff, error",
    [
        ([], 8, None, None, "signal"),
        (1, 8, None, None, "signal"),
        ([[1, 2]], 8, None, None, "signal"),
        ([1 + 0j], 8, None, None, "signal"),
        ([np.nan], 8, None, None, "signal"),
        ([np.inf], 8, None, None, "signal"),
        ([-np.inf], 8, None, None, "signal"),
        (["1"], 8, None, None, "signal"),
        (np.array([1], dtype=object), 8, None, None, "signal"),
        ([1], 0, None, None, "sampling_rate"),
        ([1], -1, None, None, "sampling_rate"),
        ([1], np.nan, None, None, "sampling_rate"),
        ([1], np.inf, None, None, "sampling_rate"),
        ([1], -np.inf, None, None, "sampling_rate"),
        ([1], 8 + 0j, None, None, "sampling_rate"),
        ([1], [8], None, None, "sampling_rate"),
        ([1], "8", None, None, "sampling_rate"),
        ([1], None, None, None, "sampling_rate"),
        ([1], 8, -1, None, "low_cutoff"),
        ([1], 8, 5, None, "low_cutoff"),
        ([1], 8, None, -1, "high_cutoff"),
        ([1], 8, None, 5, "high_cutoff"),
        ([1], 8, np.nan, None, "low_cutoff"),
        ([1], 8, np.inf, None, "low_cutoff"),
        ([1], 8, None, np.nan, "high_cutoff"),
        ([1], 8, None, -np.inf, "high_cutoff"),
        ([1], 8, 1 + 0j, None, "low_cutoff"),
        ([1], 8, None, [1], "high_cutoff"),
        ([1], 8, "1", None, "low_cutoff"),
        ([1], 8, 3, 2, "low_cutoff.*high_cutoff"),
    ],
)
def test_fft_mask_rejects_invalid_inputs(
    signal, sampling_rate, low_cutoff, high_cutoff, error
):
    """At the control rate fs=8, the permitted closed interval is [0, 4]."""
    original = np.array(signal, copy=True)
    with pytest.raises(ValueError, match=error):
        FrequencyFilter().fft_filter(signal, sampling_rate, low_cutoff, high_cutoff)
    np.testing.assert_array_equal(signal, original)


def test_fft_mask_leaves_coefficient_filter_state_unchanged():
    """FFT masking must not replace the coefficients of an existing design."""
    filt = FrequencyFilter()
    filt.design_filter(8, 64)
    coefficients = filt.last_filter
    filt.fft_filter([1, -2, 3], 64)
    assert filt.last_filter is coefficients
