# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Closed-form controls, derived from NIST e-Handbook 1.3.6.6.8.

All sample parameters below are constructed test inputs, not design criteria.
Floating tolerances bound roundoff of the written arithmetic only.
"""

import math

import numpy as np
import pytest

from digitalmodel.orcawave.weibull_fit import WeibullDistribution


def test_exponential_hand_calculation():
    """k=1, lambda=2, gamma=3: at x=5, z=1, F=1-e^-1, f=e^-1/2.

    E[X]=3+2=5, Var[X]=2^2=4, E[X^2]=25+4=29.
    N=e^2 gives Q(1-1/N)=3+2*2=7. Source: NIST 1.3.6.6.8.
    """
    law = WeibullDistribution(shape=1, scale=2, location=3)
    assert law.pdf(5) == pytest.approx(math.exp(-1) / 2)
    assert law.cdf(5) == pytest.approx(1 - math.exp(-1))
    assert law.quantile(1 - math.exp(-1)) == pytest.approx(5)
    assert law.mean == pytest.approx(5)
    assert law.variance == pytest.approx(4)
    assert law.moment(0) == 1
    assert law.moment(1) == pytest.approx(5)
    assert law.moment(2) == pytest.approx(29)
    assert law.return_level(math.exp(2)) == pytest.approx(7)


def test_rayleigh_hand_calculation():
    """k=2, lambda=sqrt(2)*sigma, gamma=0 gives exp[-x^2/(2 sigma^2)].

    sigma=3: E[X]=sigma sqrt(pi/2), Var[X]=(2-pi/2)*sigma^2.
    Gamma(3/2)=sqrt(pi)/2, Gamma(2)=1 (NIST 1.3.6.6.8).
    """
    sigma = 3
    law = WeibullDistribution(2, math.sqrt(2) * sigma)
    x = math.sqrt(2) * sigma
    assert law.cdf(x) == pytest.approx(1 - math.exp(-1))
    assert law.pdf(x) == pytest.approx(math.sqrt(2) * math.exp(-1) / sigma)
    assert law.mean == pytest.approx(sigma * math.sqrt(math.pi / 2))
    assert law.variance == pytest.approx((2 - math.pi / 2) * sigma**2)


@pytest.mark.parametrize("shape, at_boundary", [(0.5, math.inf), (1, 0.5), (2, 0)])
def test_support_and_endpoint_limits(shape, at_boundary):
    """Limits of k/lambda*z^(k-1)*exp(-z^k) as z approaches zero."""
    law = WeibullDistribution(shape, 2, -3)
    assert law.pdf(-4) == 0
    assert law.pdf(-3) == at_boundary
    assert law.cdf(-4) == law.cdf(-3) == 0
    assert law.quantile(0) == -3
    assert law.quantile(1) == math.inf
    assert law.pdf(math.inf) == 0
    assert law.cdf(math.inf) == 1
    assert law.pdf(-math.inf) == law.cdf(-math.inf) == 0


def test_vectorization_and_extreme_cycles():
    """For k=1, lambda=1, gamma=0, Q_N=ln(N), even beyond 1/eps."""
    law = WeibullDistribution(1, 1)
    np.testing.assert_allclose(law.cdf([0, 1, 2]), 1 - np.exp(-np.arange(3)))
    np.testing.assert_allclose(law.quantile([0, 0.5]), [0, math.log(2)])
    assert law.return_level(1e100) == pytest.approx(100 * math.log(10))
    assert law.return_level(1) == 0


@pytest.mark.parametrize(
    "args",
    [
        (0, 1, 0),
        (-1, 1, 0),
        (1, 0, 0),
        (1, -1, 0),
        (math.nan, 1, 0),
        (1, math.inf, 0),
        (1, 1, math.inf),
    ],
)
def test_invalid_parameters(args):
    with pytest.raises(ValueError):
        WeibullDistribution(*args)


@pytest.mark.parametrize("p", [-0.1, 1.1, math.nan, math.inf])
def test_invalid_probability(p):
    with pytest.raises(ValueError):
        WeibullDistribution(2, 1).quantile(p)


@pytest.mark.parametrize("n", [0, 0.5, math.nan, math.inf])
def test_invalid_cycles(n):
    with pytest.raises(ValueError):
        WeibullDistribution(2, 1).return_level(n)


@pytest.mark.parametrize("order", [-1, 1.5, True])
def test_invalid_moment_order(order):
    with pytest.raises(ValueError):
        WeibullDistribution(2, 1).moment(order)


def test_nan_response_rejected():
    with pytest.raises(ValueError):
        WeibullDistribution(2, 1).pdf(math.nan)
    with pytest.raises(ValueError):
        WeibullDistribution(2, 1).cdf([0, math.nan])


def test_lower_tail_logcdf_does_not_underflow():
    """NIST W: k=2, z=1e-200 gives log F ~ 2 log z (correction < 1e-400).

    The probability underflows float range; its logarithm remains finite.
    """
    law = WeibullDistribution(2, 1)
    assert law.logcdf(1e-200) == pytest.approx(2 * math.log(1e-200))


def test_overflowing_difference_has_finite_standardized_response():
    """NIST W: (x-gamma)/lambda=(1e308+1e308)/1e308=2 by hand scaling."""
    law = WeibullDistribution(1, 1e308, -1e308)
    assert law.cdf(1e308) == pytest.approx(1 - math.exp(-2))
    assert law.pdf(1e308) / (math.exp(-2) / 1e308) == pytest.approx(1)


def test_scale_and_power_overflow_cancel_before_exponentiation():
    """NIST W: k=1/1000, lambda=e^-700, ln N=e.

    lambda*(ln N)^(1/k)=e^-700*(e)^1000=e^300, a finite result,
    although the unscaled power e^1000 is outside floating-point range.
    """
    law = WeibullDistribution(1 / 1000, math.exp(-700))
    assert law.return_level(math.exp(math.e)) == pytest.approx(math.exp(300))
    assert law.quantile(1 - math.exp(-math.e)) == pytest.approx(math.exp(300))
