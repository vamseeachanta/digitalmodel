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


@pytest.mark.parametrize("shape", [0.5, 1, 2])
@pytest.mark.parametrize("location", [-3, 3])
def test_shifted_quantile_endpoints_in_mixed_arrays(shape, location):
    """Source W: Q(0)=gamma, Q(1)=+infinity for either sign of gamma."""
    law = WeibullDistribution(shape, 2, location)
    probabilities = [[0, 0.5], [1, 0]]
    expected = [
        [location, location + 2 * math.log(2) ** (1 / shape)],
        [math.inf, location],
    ]
    np.testing.assert_allclose(law.quantile(probabilities), expected)


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


def test_lower_tail_survives_underflowing_standardized_ratio():
    """NIST W, online edition 2026, 1.3.6.6.8: k=.5, z=10^-400.

    z^k=10^-200; F~10^-200; f=(.5/10^200)*10^200*exp(-10^-200)=.5.
    log F=-200 ln(10), up to a correction smaller than 10^-200.
    """
    law = WeibullDistribution(0.5, 1e200)
    assert law.cdf(1e-200) == pytest.approx(1e-200, rel=1e-12, abs=0)
    assert law.pdf(1e-200) == pytest.approx(0.5, rel=1e-12)
    assert law.logcdf(1e-200) == pytest.approx(-200 * math.log(10))
    assert law.logsf(1e-200) == pytest.approx(-1e-200, rel=1e-12, abs=0)


def test_upper_tail_survives_overflowing_standardized_ratio():
    """NIST W, online edition 2026, 1.3.6.6.8: k=.001, z=10^400.

    The hazard is 10^.4, so log S=-10^.4 and f=k*10^.4*exp(-10^.4)/x.
    Both quantities are representable despite overflow of the ratio z.
    """
    law = WeibullDistribution(0.001, 1e-200)
    hazard = 10**0.4
    density = 0.001 * hazard * math.exp(-hazard) * 1e-200
    assert law.logsf(1e200) == pytest.approx(-hazard, rel=1e-12)
    assert law.pdf(1e200) == pytest.approx(density, rel=1e-12, abs=0)
    assert law.cdf(1e200) == pytest.approx(1 - math.exp(-hazard))
    assert law.logcdf(1e200) == pytest.approx(math.log1p(-math.exp(-hazard)))


@pytest.mark.parametrize("api", ["quantile", "return_level"])
def test_negative_location_cancels_overflowing_quantile_term(api):
    """NIST W, online edition 2026, 1.3.6.6.8, inverse CDF.

    k=1, lambda=10^308, gamma=-10^308 and N=e^2 give
    Q=-10^308+2*10^308=10^308, although the positive term overflows.
    """
    law = WeibullDistribution(1, 1e308, -1e308)
    argument = 1 - math.exp(-2) if api == "quantile" else math.exp(2)
    assert getattr(law, api)(argument) == pytest.approx(1e308, rel=1e-12)


def test_shifted_quantiles_keep_vector_endpoints_and_zero_crossing():
    """NIST W, online edition 2026, 1.3.6.6.8: Q=10^308*(t-1).

    For t=0,.5,1,2,infinity, Q=-10^308,-5*10^307,0,10^308,infinity.
    """
    law = WeibullDistribution(1, 1e308, -1e308)
    p = -np.expm1(-np.array([0, 0.5, 1, 2, math.inf]))
    np.testing.assert_allclose(
        law.quantile(p), [-1e308, -5e307, 0, 1e308, math.inf], rtol=1e-12
    )


@pytest.mark.parametrize("quantity", ["variance", "second_moment"])
def test_tiny_scale_keeps_large_gamma_moments_finite(quantity):
    """NIST W, online edition 2026, 1.3.6.6.8: Gamma(n+1)=n!.

    k=.01, lambda=10^-160: E[X^2]=200!*10^-320=7.886578673647905*10^54.
    Var[X]=(200!-(100!)^2)*10^-320 differs below the asserted tolerance.
    Neither a floating 200! nor lambda^2 is representable separately.
    """
    law = WeibullDistribution(0.01, 1e-160)
    result = law.variance if quantity == "variance" else law.moment(2)
    assert result == pytest.approx(7.886578673647905e54, rel=1e-12)


@pytest.mark.parametrize("location", [-1e154, 1e154])
def test_shifted_moment_combines_signed_overflowing_terms(location):
    """NIST W, online edition 2026, 1.3.6.6.8, shifted gamma moments.

    For k=1, lambda=10^154 and gamma=-10^154, E[X^2]=(1-2+2)*10^308.
    For gamma=10^154, E[X]=(1+1)*10^154; no intermediate power is needed.
    """
    law = WeibullDistribution(1, 1e154, location)
    if location < 0:
        assert law.moment(2) == pytest.approx(1e308, rel=1e-12)
    else:
        assert law.mean == pytest.approx(2e154, rel=1e-12)


def test_odd_shifted_moment_preserves_negative_sign():
    """NIST W, online edition 2026, 1.3.6.6.8: E[X]=gamma+lambda*Gamma(2).

    k=1, gamma=-10^308, lambda=5*10^307 give E[X]=-5*10^307.
    """
    law = WeibullDistribution(1, 5e307, -1e308)
    assert law.mean == pytest.approx(-5e307, rel=1e-12)


def test_variance_combines_large_scale_before_squaring():
    """NIST W, online edition 2026, 1.3.6.6.8: k=2, Var=lambda^2*(1-pi/4).

    lambda=2*10^154 gives (4-pi)*10^308, despite overflow of lambda^2.
    """
    law = WeibullDistribution(2, 2e154)
    assert law.variance == pytest.approx((4 - math.pi) * 1e308, rel=1e-12)


def test_raw_moment_raises_only_for_nonrepresentable_final_result():
    """NIST W: a unit exponential with scale=10^200 has E[X^2]=2*10^400."""
    with pytest.raises(OverflowError):
        WeibullDistribution(1, 1e200).moment(2)


@pytest.mark.parametrize("api", ["quantile", "return_level"])
def test_scaled_quantile_operands_preserve_exact_cancellation(api):
    """NIST W, online edition 2026, 1.3.6.6.8: Q=gamma+lambda*ln(N).

    k=1, lambda=5*10^307, gamma=-10^308 and N=e^2 give
    Q=-10^308+2*(5*10^307)=0, before any separately rounded logarithms.
    """
    law = WeibullDistribution(1, 5e307, -1e308)
    argument = -math.expm1(-2) if api == "quantile" else math.exp(2)
    assert getattr(law, api)(argument) == 0


@pytest.mark.parametrize("api", ["quantile", "return_level"])
@pytest.mark.parametrize("shape, multiplier", [(0.5, 9), (1 / 3, 27)])
@pytest.mark.parametrize("scale", [1, math.ldexp(1, 1000)])
def test_quantile_power_keeps_hand_calculated_zero(api, shape, multiplier, scale):
    """Source W: t=3 gives t^2=9 and t^3=27, exactly cancelling gamma."""
    law = WeibullDistribution(shape, scale, -multiplier * scale)
    argument = -math.expm1(-3) if api == "quantile" else math.exp(3)
    assert getattr(law, api)(argument) == 0


@pytest.mark.parametrize("probability, expected", [(2e-162, 4e-16), (1e-160, 1e-12)])
def test_subnormal_quantile_power_retains_precision_after_scaling(
    probability, expected
):
    """NIST W, online edition 2026, 1.3.6.6.8: k=.5 gives Q=lambda*t^2.

    At these tiny probabilities, t=-ln(1-p)=p+O(p^2), so
    10^308*(2*10^-162)^2=4*10^-16 and 10^308*(10^-160)^2=10^-12.
    Rounding t^2 to a subnormal before scaling corrupts the finite result.
    """
    law = WeibullDistribution(0.5, 1e308)
    assert law.quantile(probability) == pytest.approx(expected, rel=1e-12, abs=0)


def test_subnormal_return_level_power_retains_precision_after_scaling():
    """NIST W, online edition 2026, 1.3.6.6.8: Q_N=lambda*(ln N)^(1/k).

    N=1+2^-52, k=.0485, lambda=10^308. Expanding ln(N)=2^-52-2^-104/2+...
    and raising it to 1/.0485 gives Q_N=1.762747940478373*10^-15.
    The omitted log term changes Q_N by less than the asserted tolerance.
    """
    law = WeibullDistribution(0.0485, 1e308)
    assert law.return_level(math.nextafter(1, 2)) == pytest.approx(
        1.762747940478373e-15, rel=1e-12, abs=0
    )
