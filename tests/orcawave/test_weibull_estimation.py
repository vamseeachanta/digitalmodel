# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Estimator controls based on independent moments and probability transforms."""

import math

import numpy as np
import pytest
from scipy.integrate import quad
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


def _integrated_population_moments(shape):
    """Independent Source W substitution t=exp(u), X=k*expm1(u/k).

    The transformed density is exp(u-exp(u)). Integrate centered powers
    directly to avoid subtracting gamma moments. The omitted tails outside
    [-50, 5] contribute below 1e-12 for the constructed shapes tested here;
    quadrature error estimates must also be below 1e-10.
    """

    def integrate(order, center=0.0):
        value, error = quad(
            lambda u: (
                (shape * math.expm1(u / shape) - center) ** order
                * math.exp(u - math.exp(u))
            ),
            -50,
            5,
            epsabs=2e-12,
            epsrel=2e-12,
        )
        assert error < 1e-10
        return value

    mean = integrate(1)
    variance = integrate(2, mean)
    skewness = integrate(3, mean) / variance**1.5
    return mean, variance, skewness


@pytest.mark.parametrize("shape", [0.5, 1.0, 4.0, 15.0])
def test_population_recovery_within_supported_moment_bracket(shape):
    """Independent population integration recovers all three parameters.

    The bracket reaches the supported upper endpoint. The 1e-8 relative
    tolerance covers quadrature and scalar-root arithmetic, not sample error.
    """
    moments = _integrated_population_moments(shape)
    law = distribution_from_moments(*moments, shape_bounds=(0.2, 16))
    np.testing.assert_allclose(
        [law.shape, law.scale, law.location], [shape, shape, -shape], rtol=1e-8
    )


def test_large_shape_population_moment_estimation_is_rejected():
    """Independent large-shape moments must not produce a spurious solution."""
    moments = _integrated_population_moments(10000)
    with pytest.raises(ValueError, match="Moment estimation requires.*16"):
        distribution_from_moments(*moments, shape_bounds=(0.5, 20000))


@pytest.mark.parametrize(
    "bounds", [(0.5, math.nextafter(16, math.inf)), (16, 17), (0.5, 20000)]
)
def test_moment_estimator_rejects_entire_unsupported_bracket(bounds):
    """Reject the supplied bracket rather than silently clipping its endpoint.

    Even exponential population moments inside the supported domain do not
    justify evaluating an unsupported upper endpoint during root finding.
    """
    with pytest.raises(ValueError, match="Moment estimation requires.*16"):
        distribution_from_moments(5, 4, 2, shape_bounds=bounds)


def test_sample_moment_fit_enforces_estimator_shape_domain():
    """The sample-moment entry point propagates the same explicit restriction."""
    with pytest.raises(ValueError, match="Moment estimation requires.*16"):
        fit_weibull(
            [1, 2, 3, 4, 8],
            method="moments",
            location_bounds=(-10, 0.99),
            shape_bounds=(0.5, 20000),
        )
