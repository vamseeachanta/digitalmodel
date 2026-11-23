# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""White-noise density from an exact finite ensemble, calculated by hand.

Independent hand calculation: take every length-8 record of signs -2 or +2
once, with equal weight 1/256. The Cartesian product has 2**8 = 256 records;
each sign has zero mean, and independence gives E[x_n*x_m] = 4 for n=m
and zero otherwise. Expanding the squared DFT magnitude therefore gives
E[|X_k|**2] = sum(n,m, E[x_n*x_m]*exp(-2*pi*i*k*(n-m)/8)) = 8*4 = 32.
Subtracting the record mean removes DC only: the geometric sum of the
Fourier factors is zero for every nonzero bin, including Nyquist.

A boxcar has sum(w**2) = 8, so density normalization at fs=8 Hz is
1/(fs*8) = 1/64. One-sided interior bins combine positive and negative
frequencies and double; DC and Nyquist are single bins and do not double.
Thus frequencies [0,1,2,3,4] have ensemble density [0,1,1,1,1/2].
At fs=16 Hz the independent normalization is 1/128: frequencies are
[0,2,4,6,8] and densities [0,1/2,1/2,1/2,1/4]. Bin widths are respectively
1 and 2 Hz, and the bin-sum integral is 7/2 at either rate. A single Welch
segment of length 8 with zero overlap has the same boxcar normalization.

Independently in time, every record has mean-square 4. Expansion of the
squared record mean gives E[mean(x)**2] = (8*4)/8**2 = 1/2. Consequently
E[mean((x-mean(x))**2)] = 4 - 1/2 = 7/2 after constant detrending.
The literal expectations below use these calculations, never another PSD.

Roundoff allowance, fixed before evaluation: budget 8 real operations for
each of 8**2 complex multiply/add terms of a direct short DFT, 256 ensemble
additions and 5 bin additions. Use gamma_m = m*eps/(1-m*eps) for this
conservative operation budget. Detrended samples have magnitude at most 4,
so |X| <= 8*4 = 32 and one-sided density <= 2*32**2/64 = 32 at the lower
rate. Density tolerance is 32*gamma_m; integral tolerance allows 5 bins of
width at most 2 Hz. Relative tolerance is zero; no sampling error applies.
"""

from itertools import product

import numpy as np
import pytest

from digitalmodel.signal_processing.signal_analysis.core.spectral import (
    SpectralAnalyzer,
)

_EPS = np.finfo(np.float64).eps
_ROUNDING_STEPS = 8 * 8**2 + 256 + 5
_GAMMA = _ROUNDING_STEPS * _EPS / (1 - _ROUNDING_STEPS * _EPS)
_DENSITY_ATOL = 32 * _GAMMA
_INTEGRAL_ATOL = 5 * 2 * _DENSITY_ATOL


def _ensemble_densities(method, sampling_rate, expected_frequencies):
    """Evaluate each equally weighted record once and check call invariants."""
    records = np.array(list(product((-2.0, 2.0), repeat=8)))
    original = records.copy()
    assert records.shape == (256, 8)
    assert np.unique(records, axis=0).shape == (256, 8)
    # Start with the other PSD method so restoration cannot hard-code FFT.
    original_method = "welch" if method == "periodogram" else "periodogram"
    analyzer = SpectralAnalyzer(sampling_rate=sampling_rate, method=original_method)
    options = {"window": "boxcar"}
    if method == "welch":
        options.update(nperseg=8, noverlap=0)

    densities = []
    for index, record in enumerate(records):
        result = analyzer.compute_psd(record, method=method, **options)
        assert analyzer.method == original_method
        np.testing.assert_array_equal(record, original[index])
        np.testing.assert_array_equal(result.frequency.to_numpy(), expected_frequencies)
        density = result.psd.to_numpy()
        assert np.all(density >= 0)
        densities.append(density)

    np.testing.assert_array_equal(records, original)
    densities = np.array(densities)
    assert densities.shape == (256, 5)
    return records, densities


@pytest.mark.parametrize("method", ["periodogram", "welch"])
@pytest.mark.parametrize(
    "sampling_rate, expected_frequencies, expected_density",
    [
        (8, [0, 1, 2, 3, 4], [0, 1, 1, 1, 1 / 2]),
        (16, [0, 2, 4, 6, 8], [0, 1 / 2, 1 / 2, 1 / 2, 1 / 4]),
    ],
)
def test_complete_noise_ensemble_has_hand_density(
    method, sampling_rate, expected_frequencies, expected_density
):
    """Check every bin against the finite-ensemble derivation above."""
    _, densities = _ensemble_densities(method, sampling_rate, expected_frequencies)
    mean_density = densities.mean(axis=0)
    np.testing.assert_allclose(
        mean_density, expected_density, rtol=0, atol=_DENSITY_ATOL
    )
    # DC is removed; the unpaired Nyquist bin has half the interior level.
    assert mean_density[0] == pytest.approx(0, rel=0, abs=_DENSITY_ATOL)
    np.testing.assert_allclose(
        mean_density[-1], mean_density[1:-1] / 2, rtol=0, atol=_DENSITY_ATOL
    )


@pytest.mark.parametrize("method", ["periodogram", "welch"])
@pytest.mark.parametrize(
    "sampling_rate, expected_frequencies, bin_width",
    [(8, [0, 1, 2, 3, 4], 1), (16, [0, 2, 4, 6, 8], 2)],
)
def test_noise_density_integral_matches_detrended_mean_square(
    method, sampling_rate, expected_frequencies, bin_width
):
    """Check 4 - E[mean(x)**2] = 4 - 1/2 = 7/2 without sampling slack."""
    records, densities = _ensemble_densities(
        method, sampling_rate, expected_frequencies
    )
    record_means = records.mean(axis=1, keepdims=True)
    residual_mean_square = ((records - record_means) ** 2).mean(axis=1)
    # These binary fractions are exactly representable, including their means.
    assert np.mean(records**2) == 4
    assert np.mean(record_means**2) == 1 / 2
    assert residual_mean_square.mean() == 7 / 2

    integrals = densities.sum(axis=1) * bin_width
    np.testing.assert_allclose(
        integrals, residual_mean_square, rtol=0, atol=_INTEGRAL_ATOL
    )
    assert integrals.mean() == pytest.approx(7 / 2, rel=0, abs=_INTEGRAL_ATOL)
