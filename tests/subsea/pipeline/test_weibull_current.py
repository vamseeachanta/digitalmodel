# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic quantiles, not measured current data or a random-sample MLE fit.

Source: NIST/SEMATECH e-Handbook of Statistical Methods, section 1.3.6.6.8,
Weibull Distribution, online edition accessed 2026-10-22:
https://www.itl.nist.gov/div898/handbook/eda/section3/eda3668.htm
The module accepts exceedance/speed pairs. CDF values therefore become 1-F.
Shape, scale and all probabilities below are invented test inputs.
"""

import math

import pytest

from digitalmodel.subsea.pipeline.free_span.weibull_current import (
    extrapolate_current,
    fit_weibull_current,
)


@pytest.fixture
def known_weibull_pairs():
    """Fixed uniform variates F=[.1,.3,.7,.95], shape k=2, scale c=1.5 m/s.

    Inverse transform x=1.5*sqrt(-ln(1-F)); the fit receives P=1-F.
    These exact quantile pairs contain no sampling noise. This is not a claim
    that a four-observation statistical estimator recovers population parameters.
    """
    uniform = [0.1, 0.3, 0.7, 0.95]
    speeds = [1.5 * math.sqrt(-math.log1p(-value)) for value in uniform]
    return [1.0 - value for value in uniform], speeds


def test_fit_recovers_both_known_tail_shapes(known_weibull_pairs):
    """ln(-ln P)=2*ln(x)-2*ln(1.5), so each two-point slope is 2.

    Source: NIST section 1.3.6.6.8, survival function, rearranged by hand.
    Relative 1e-12 permits log and inverse-transform round-off only.
    """
    probs, speeds = known_weibull_pairs
    result = fit_weibull_current(probs, speeds)
    assert result.k_upper == pytest.approx(2.0, rel=1e-12)
    assert result.k_lower == pytest.approx(2.0, rel=1e-12)
    assert result.exceedance_probs == probs
    assert result.current_speeds == speeds


@pytest.mark.parametrize("cdf,expected", [(0.01, 0.1503770450), (0.99, 3.2189490394)])
def test_extrapolate_both_tails_hand_quantiles(known_weibull_pairs, cdf, expected):
    """NIST percent-point function: x=c*(-ln(1-F))^(1/k).

    F=.01: -ln(.99)=.010050335854; sqrt=.100251363350; x=.150377045025.
    F=.99: -ln(.01)=4.605170185988; sqrt=2.145966026289; x=3.218949039434.
    The API uses survival P=.99/.01, not the CDF F=.01/.99.
    Absolute 1e-10 covers the displayed hand-value rounding. These are
    probabilities per observation; no unprovided occurrence rate is assumed.
    """
    probs, speeds = known_weibull_pairs
    actual = extrapolate_current(probs, speeds, 1.0 - cdf)
    assert isinstance(actual, float)
    assert actual == pytest.approx(expected, abs=1e-10)


def test_vector_targets_match_two_independent_hand_values(known_weibull_pairs):
    """Same hand quantiles as above, in reverse order to exercise vector dispatch."""
    probs, speeds = known_weibull_pairs
    assert extrapolate_current(probs, speeds, [0.01, 0.99]) == pytest.approx(
        [3.2189490394, 0.1503770450], abs=1e-10
    )


@pytest.mark.parametrize(
    "probs,speeds", [([], []), ([0.5], [1.0]), ([0.5, 0.0], [1.0, 2.0])]
)
def test_fit_requires_two_positive_exceedance_pairs(probs, speeds):
    """Two distinct pairs are the minimum for a two-point logarithmic slope."""
    with pytest.raises(ValueError, match="At least 2"):
        fit_weibull_current(probs, speeds)


def test_fit_refuses_mismatched_lengths():
    with pytest.raises(ValueError, match="Length mismatch"):
        fit_weibull_current([0.9, 0.1], [1.0])


@pytest.mark.xfail(
    strict=True,
    raises=ZeroDivisionError,
    reason="Constant sample divides by zero instead of refusing it",
)
def test_fit_refuses_constant_sample():
    """Equal speeds give ln(x2/x1)=0: no finite Weibull shape is identifiable."""
    with pytest.raises(ValueError):
        fit_weibull_current([0.9, 0.1], [1.0, 1.0])


@pytest.mark.parametrize(
    "field,bad",
    [
        pytest.param(
            "probability",
            math.nan,
            marks=pytest.mark.xfail(
                strict=True, reason="NaN probability is silently filtered"
            ),
        ),
        ("probability", math.inf),
        pytest.param(
            "probability",
            -math.inf,
            marks=pytest.mark.xfail(
                strict=True, reason="Infinite probability is silently filtered"
            ),
        ),
        pytest.param(
            "speed",
            math.nan,
            marks=pytest.mark.xfail(strict=True, reason="NaN speed produces NaN shape"),
        ),
        ("speed", math.inf),
        ("speed", -math.inf),
    ],
)
def test_fit_refuses_nonfinite_pairs(known_weibull_pairs, bad, field):
    """A nonfinite pair cannot define a real-valued distribution, even if filtered."""
    probs, speeds = known_weibull_pairs
    (probs if field == "probability" else speeds)[0] = bad
    with pytest.raises(ValueError):
        fit_weibull_current(probs, speeds)


@pytest.mark.parametrize(
    "bad",
    [
        0.0,
        1.0,
        -0.1,
        1.1,
        pytest.param(
            math.nan,
            marks=pytest.mark.xfail(
                strict=True, reason="NaN target produces NaN current"
            ),
        ),
        math.inf,
        -math.inf,
    ],
)
def test_extrapolate_refuses_invalid_target(known_weibull_pairs, bad):
    """Survival probabilities must be finite and strictly inside (0,1)."""
    probs, speeds = known_weibull_pairs
    with pytest.raises(ValueError):
        extrapolate_current(probs, speeds, bad)
