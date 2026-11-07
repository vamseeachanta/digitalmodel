# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Hand-derived checks for inverse FFT metadata and boundary coefficients."""

import numpy as np
import pandas as pd
import pytest

from digitalmodel.signal_processing.signal_analysis.core.spectral import (
    SpectralAnalyzer,
)


@pytest.mark.parametrize("n_samples", [1, 2, 15, 16])
def test_inverse_preserves_dc_and_highest_frequency(n_samples):
    """Synthetic x[j]=3+2*cos(2*pi*floor(N/2)*j/N), fs=N.

    For even N the cosine is (-1)**j, whose Nyquist coefficient is 2*N.
    For odd N its conjugate pair has coefficients N at +/-floor(N/2).
    DC is 3*N (N=1 instead gives the single sample 5). Substitution in
    the inverse DFT gives the stated samples. Absolute tolerance 1e-12
    allows roundoff, with zero relative tolerance. N=1,2 check endpoints.
    """
    samples = np.arange(n_samples)
    expected = 3 + 2 * np.cos(2 * np.pi * (n_samples // 2) * samples / n_samples)
    analyzer = SpectralAnalyzer(sampling_rate=n_samples)
    spectrum = analyzer.compute_spectrum(expected, detrend="none")

    # Metadata must remain usable by another analyzer after a DataFrame copy.
    restored = SpectralAnalyzer().inverse_transform(spectrum.copy())

    np.testing.assert_allclose(restored, expected, rtol=0, atol=1e-12)


@pytest.mark.parametrize(
    "filter_type, cutoffs, keep_nyquist",
    [
        ("lowpass", {"high_freq": 7}, False),
        ("lowpass", {"high_freq": 8}, True),
        ("highpass", {"low_freq": 8}, True),
        ("highpass", {"low_freq": 9}, False),
        ("bandpass", {"low_freq": 7, "high_freq": 8}, True),
        ("bandpass", {"low_freq": 1, "high_freq": 7}, False),
        ("bandstop", {"low_freq": 7, "high_freq": 8}, False),
        ("bandstop", {"low_freq": 1, "high_freq": 7}, True),
    ],
)
def test_filtered_inverse_applies_cutoffs_to_nyquist(filter_type, cutoffs, keep_nyquist):
    """N=fs=16, x[j]=2*(-1)**j has only X[8]=32, at Nyquist 8 Hz.

    Inclusive passband and exclusive stopband limits therefore retain the
    original samples or yield zero. Absolute tolerance 1e-12 allows only
    roundoff. Filtering must not mutate the original spectrum's metadata.
    """
    samples = 2.0 * (-1.0) ** np.arange(16)
    analyzer = SpectralAnalyzer(sampling_rate=16)
    spectrum = analyzer.compute_spectrum(samples, detrend="none")
    filtered = analyzer.filter_spectrum(spectrum, filter_type, **cutoffs)
    expected = samples if keep_nyquist else np.zeros(16)

    np.testing.assert_allclose(
        analyzer.inverse_transform(filtered), expected, rtol=0, atol=1e-12
    )
    np.testing.assert_allclose(
        analyzer.inverse_transform(spectrum), samples, rtol=0, atol=1e-12
    )


def test_inverse_without_metadata_retains_legacy_convention():
    """Coefficients [4,2,0] imply [4,2,0,2], so inverse is [2,1,0,1].

    This hand-specified Hermitian spectrum uses the existing convention
    that its final supplied bin is Nyquist. Absolute tolerance 1e-12 allows
    roundoff and zero relative tolerance prevents amplitude-dependent slack.
    """
    spectrum = pd.DataFrame({"fft_complex": [4 + 0j, 2 + 0j, 0 + 0j]})

    np.testing.assert_allclose(
        SpectralAnalyzer().inverse_transform(spectrum),
        [2, 1, 0, 1],
        rtol=0,
        atol=1e-12,
    )
