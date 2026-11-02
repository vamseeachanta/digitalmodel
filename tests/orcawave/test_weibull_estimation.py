# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Estimator controls based on independent moments and probability transforms."""

import math

import numpy as np
import pytest
from scipy.stats import weibull_min

from digitalmodel.orcawave.weibull_fit import (
    WeibullDistribution,
    distribution_from_moments,
    fit_weibull,
)


def test_population_moments_exponential():
    """NIST 1.3.6.6.8: shifted exponential (1,2,3) has mean=5,var=4,skew=2.

    E[Z^j]=j! gives central third moment 6-3*2+2=2 for unit Z.
    Root tolerance is floating arithmetic, not a fit acceptance threshold.
    """
    law = distribution_from_moments(5, 4, 2, shape_bounds=(0.5, 4))
    assert law.shape == pytest.approx(1)
    assert law.scale == pytest.approx(2)
    assert law.location == pytest.approx(3)


def test_probability_plot_exact_line():
    """NIST 1.3.3.30: p_i=(i-.3)/(n+.4), y_i=ln[-ln(1-p_i)].

    Construct x_i=3+2*[-ln(1-p_i)]^(1/2). Then y_i=2*ln(x_i-3)-2ln2
    exactly. Three-parameter least squares must recover (2,2,3).
    Absolute tolerance 1e-5 covers scalar optimizer termination and logs.
    """
    p = (np.arange(1, 41) - 0.3) / (40 + 0.4)
    samples = 3 + 2 * np.sqrt(-np.log1p(-p))
    fit = fit_weibull(
        samples,
        method="probability_plot",
        location_bounds=(1, 3.1),
        shape_bounds=(0.5, 5),
    )
    np.testing.assert_allclose(
        [fit.distribution.shape, fit.distribution.scale, fit.distribution.location],
        [2, 2, 3],
        atol=1e-5,
        rtol=0,
    )
    assert fit.objective < 1e-10  # Squared residuals of the exact line above.


@pytest.mark.parametrize("method", ["mle", "moments", "probability_plot"])
def test_seeded_recovery(method):
    """Inverse transform of NIST 1.3.6.6.8 with k=2, lambda=3, gamma=1.

    n=20000: exponential Z^k has SE(mean)=1/sqrt(n)=0.0071. Location
    estimation and sample skew amplify error; a deliberately conservative
    10% shape/scale and 0.3-response-unit location envelope is used, about
    fourteen SEs of the exponential mean. This constructed sampling control
    is not an engineering acceptance limit or a confidence statement.
    """
    rng = np.random.default_rng(2026)  # Arbitrary reproducibility input.
    samples = 1 + 3 * (-np.log1p(-rng.random(20000))) ** 0.5
    fit = fit_weibull(
        samples,
        method=method,
        location_bounds=(0, np.nextafter(samples.min(), -math.inf)),
        shape_bounds=(0.5, 5),
    )
    assert fit.distribution.shape == pytest.approx(2, rel=0.1)
    assert fit.distribution.scale == pytest.approx(3, rel=0.1)
    assert fit.distribution.location == pytest.approx(1, abs=0.3)
    assert fit.method == method


def test_sample_moment_identity():
    """For [1,2,3,4,8], mean=18/5; variance and skew from direct sums.

    Check all three matched moments, not just estimator self-consistency.
    Source: NIST 1.3.6.6.8 gamma moments, expanded in the module docstring.
    """
    samples = np.array([1, 2, 3, 4, 8])
    mean = 18 / 5
    variance = sum((x - mean) ** 2 for x in samples) / 5
    third = sum((x - mean) ** 3 for x in samples) / 5
    fit = fit_weibull(
        samples, method="moments", location_bounds=(-10, 0.99), shape_bounds=(0.2, 10)
    )
    law = fit.distribution
    assert law.mean == pytest.approx(mean)
    assert law.variance == pytest.approx(variance)
    central_third = law.moment(3) - 3 * mean * law.moment(2) + 2 * mean**3
    assert central_third == pytest.approx(third)


def test_mle_scipy_crosscheck_only():
    """Secondary implementation check; primary expectations are the controls above.

    NIST W log likelihood is maximized for both, with an interior solution.
    0.1% allows different optimizer termination criteria, not sampling error.
    """
    samples = 1 + 3 * np.random.default_rng(42).weibull(2.5, 5000)
    fit = fit_weibull(
        samples, method="mle", location_bounds=(0, 1.02), shape_bounds=(0.5, 8)
    )
    shape, location, scale = weibull_min.fit(samples)
    assert fit.method == "mle"
    np.testing.assert_allclose(
        [fit.distribution.shape, fit.distribution.scale, fit.distribution.location],
        [shape, scale, location],
        rtol=0.001,
    )


def test_nonregular_mle_falls_back_explicitly():
    """For k<1, (k-1)ln(x_min-gamma) makes log L unbounded as gamma->min.

    This follows directly from log of NIST W density; bounds alone cannot
    turn that boundary-seeking solution into a regular unrestricted MLE.
    """
    samples = 2 + np.random.default_rng(9).weibull(0.6, 5000)
    fit = fit_weibull(
        samples, method="mle", location_bounds=(1.9, 1.999999), shape_bounds=(0.2, 5)
    )
    assert fit.requested_method == "mle"
    assert fit.method == "probability_plot"
    assert fit.distribution.shape < 1
    assert any("non-regular" in note for note in fit.notes)


def test_location_bounds_are_reported():
    """An imposed upper bound below the true threshold is an active constraint."""
    samples = 5 + np.random.default_rng(12).weibull(2, 2000)
    fit = fit_weibull(
        samples, method="mle", location_bounds=(0, 1), shape_bounds=(0.5, 20)
    )
    assert fit.distribution.location <= 1
    assert any("bound" in note for note in fit.notes)


@pytest.mark.parametrize(
    "samples",
    [[], [1, 2], [1, 1, 1], [1, 1, 2], [1, math.nan, 3], [1, math.inf, 3], [[1, 2, 3]]],
)
def test_invalid_samples(samples):
    with pytest.raises(ValueError):
        fit_weibull(
            samples, method="mle", location_bounds=(-2, 0), shape_bounds=(0.5, 5)
        )


@pytest.mark.parametrize("bounds", [(0, 1), (1, 0), (0, math.inf), (0,), (0, 0)])
def test_invalid_location_bounds(bounds):
    with pytest.raises(ValueError):
        fit_weibull(
            [1, 2, 4], method="mle", location_bounds=bounds, shape_bounds=(0.5, 5)
        )


@pytest.mark.parametrize("bounds", [(0, 5), (5, 1), (1, math.inf)])
def test_invalid_shape_bounds(bounds):
    with pytest.raises(ValueError):
        fit_weibull(
            [1, 2, 4], method="mle", location_bounds=(-2, 0), shape_bounds=bounds
        )


def test_unattainable_moments_and_outside_support():
    """A skew outside the bracket and a location inconsistent with data are errors."""
    with pytest.raises(ValueError, match="skew"):
        distribution_from_moments(0, 1, -10, shape_bounds=(0.5, 5))
    with pytest.raises(ValueError, match="location"):
        fit_weibull(
            [1, 2, 3, 4, 8],
            method="moments",
            location_bounds=(-100, -50),
            shape_bounds=(0.2, 10),
        )
    for variance in (0, -1, math.nan):
        with pytest.raises(ValueError):
            distribution_from_moments(0, variance, 2, shape_bounds=(0.5, 5))
    with pytest.raises(ValueError, match="method"):
        fit_weibull(
            [1, 2, 3], method="unknown", location_bounds=(-2, 0), shape_bounds=(0.5, 5)
        )


def test_affine_equivariance():
    """Translation by -7 and scaling by 10 preserve k and transform lambda,gamma."""
    samples = 3 + np.random.default_rng(7).weibull(2, 500)
    kwargs = dict(method="mle", shape_bounds=(0.5, 5))
    a = fit_weibull(samples, location_bounds=(1, 2.9), **kwargs).distribution
    b = fit_weibull(10 * samples - 7, location_bounds=(3, 22), **kwargs).distribution
    assert b.shape == pytest.approx(a.shape, rel=1e-5)
    assert b.scale == pytest.approx(10 * a.scale, rel=1e-5)
    assert b.location == pytest.approx(10 * a.location - 7, rel=1e-5)


def test_three_parameter_law_is_explicit():
    assert WeibullDistribution(2, 3, 1).location == 1
