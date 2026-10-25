# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Hand EDF statistics and independent bootstrap-engine controls."""

import math

import numpy as np
import pytest

from digitalmodel.orcawave import weibull_fit as wf


def test_five_point_hand_statistics():
    """NIST 1.3.5.16 and 1.3.5.14: choose F(x_i)=(.1,.3,.5,.7,.9).

    For x_i=-ln(1-p_i), the unit exponential has these exact probabilities.
    Each EDF left/right difference is .1; hence KS=.1.
    Symmetry 1-p_(6-i)=p_i gives
    AD=-5-(2/5)*(ln .1+3ln .3+5ln .5+7ln .7+9ln .9).
    No fitted-distribution rejection threshold is implied by these identities.
    """
    p = np.array([0.1, 0.3, 0.5, 0.7, 0.9])
    sample = [-math.log(1 - value) for value in p]
    gof = wf.goodness_of_fit(sample, wf.WeibullDistribution(1, 1))
    ad = -5 - 2 / 5 * sum(w * math.log(v) for w, v in zip([1, 3, 5, 7, 9], p))
    assert gof.ks.statistic == pytest.approx(0.1)
    assert gof.ad.statistic == pytest.approx(ad)
    assert gof.ks.status == gof.ad.status == "NOT_EVALUATED"


def test_published_ks_critical_value_known_parameters():
    """NIST 1.3.5.16 worked example: n=1000, alpha=.05, Dcrit=.04301.

    For continuous fully specified distributions this KS threshold is
    distribution free (same source). Midpoints give D=1/(2n)=.0005.
    A deliberately wrong scale makes D exceed this published threshold.
    """
    p = (np.arange(1000) + 0.5) / 1000
    sample = -np.log1p(-p)
    threshold = wf.CriticalValue(
        0.04301, 0.05, 1000, "NIST e-Handbook 1.3.5.16, worked example"
    )
    gof = wf.goodness_of_fit(
        sample, wf.WeibullDistribution(1, 1), ks_critical=threshold
    )
    assert gof.ks.statistic == pytest.approx(0.0005)
    assert gof.ks.status == "NOT_REJECTED"
    wrong = wf.goodness_of_fit(
        sample, wf.WeibullDistribution(1, 10), ks_critical=threshold
    )
    assert wrong.ks.status == "REJECTED"
    fit = wf.WeibullFit(wf.WeibullDistribution(1, 1), "mle", "mle", 0)
    fitted = wf.goodness_of_fit(sample, fit, ks_critical=threshold)
    assert fitted.ks.status == "NOT_EVALUATED"
    assert "estimated" in fitted.ks.reason


def test_no_unread_ad_table_is_applied():
    """NIST 1.3.5.14 does not publish a Weibull table; leave calibration open."""
    fit = wf.fit_weibull(
        [1, 2, 3, 4, 5],
        method="probability_plot",
        location_bounds=(-2, 0),
        shape_bounds=(0.5, 5),
    )
    result = wf.goodness_of_fit([1, 2, 3, 4, 5], fit)
    assert result.ad.status == "NOT_EVALUATED"
    assert result.ad.critical_value is None
    assert result.ks.status == "NOT_EVALUATED"


def test_tail_logs_and_support_mismatch():
    """For unit exponential x=1000, log S=-1000 even though CDF rounds to 1.

    AD is finite for interior observations and infinite at zero probability;
    NIST 1.3.5.14 logarithmic formula. No epsilon clipping of a bad fit.
    """
    law = wf.WeibullDistribution(1, 1)
    assert law.logsf(1000) == pytest.approx(-1000)
    assert np.isfinite(wf.goodness_of_fit([1, 2, 3, 4, 1000], law).ad.statistic)
    assert math.isinf(wf.goodness_of_fit([0, 1, 2, 3, 4], law).ad.statistic)
    assert math.isinf(wf.goodness_of_fit([-1, 1, 2, 3, 4], law).ad.statistic)


def test_bootstrap_percentiles_independent_engine(monkeypatch):
    """Percentile bootstrap: SciPy 1.14.1 bootstrap, Notes/Parameters.

    Replace the estimator by the independently known fixed-location, k=1 MLE:
    lambda=sample mean (differentiate log L=-n ln(lambda)-sum(x)/lambda).
    Every return level is therefore mean(resample)*ln(N). Build the same
    seeded resampling indices independently and hand-interpolate the ordered
    values at (B-1)*.25 and (B-1)*.75, for B=5 and confidence=.5.
    """
    sample = np.arange(1, 6, dtype=float)
    seed, count, cycles = 47, 5, math.exp(1)  # Constructed inputs, not criteria.

    def exponential_fit(samples, **kwargs):
        return wf.WeibullFit(
            wf.WeibullDistribution(1, float(np.mean(samples))), "mle", "mle", 0
        )

    monkeypatch.setattr(wf, "fit_weibull", exponential_fit)
    rng = np.random.default_rng(seed)
    expected = [sum(sample[i] for i in rng.integers(0, 5, 5)) / 5 for _ in range(count)]
    result = wf.bootstrap_return_level(
        sample,
        cycles=cycles,
        confidence=0.5,
        n_resamples=count,
        seed=seed,
        method="mle",
        location_bounds=(-1, 0),
        shape_bounds=(0.5, 5),
    )
    np.testing.assert_allclose(result.return_levels, expected)
    assert result.lower == pytest.approx(sorted(expected)[1])
    assert result.upper == pytest.approx(sorted(expected)[3])
    assert result.status == "EVALUATED"
    assert result.failed == 0


def test_real_bootstrap_reproducible_and_nonzero_width():
    """Synthetic k=2 peaks; check refitting creates finite varying return levels.

    This is a computational reproducibility control, not interval coverage
    validation; percentile intervals need not cover the population parameter.
    """
    samples = 1 + 3 * np.random.default_rng(23).weibull(2, 300)
    args = dict(
        cycles=1000,
        confidence=0.9,
        n_resamples=30,
        seed=71,
        method="mle",
        location_bounds=(0, 1),
        shape_bounds=(0.5, 6),
    )
    a = wf.bootstrap_return_level(samples, **args)
    b = wf.bootstrap_return_level(samples, **args)
    assert a == b
    assert a.lower < a.upper
    assert a.completed == 30
    assert a.status == "EVALUATED"


def test_bootstrap_failures_are_not_silently_dropped():
    """Three-point resamples can be degenerate; a conditional interval is invalid."""
    result = wf.bootstrap_return_level(
        [1, 2, 3],
        cycles=100,
        confidence=0.9,
        n_resamples=20,
        seed=1,
        method="probability_plot",
        location_bounds=(-2, 0),
        shape_bounds=(0.2, 10),
    )
    assert result.status == "NOT_EVALUATED"
    assert result.failed > 0
    assert result.completed + result.failed == 20
    assert result.lower is result.upper is None


@pytest.mark.parametrize(
    "kwargs",
    [
        dict(confidence=1),
        dict(confidence=0),
        dict(n_resamples=1),
        dict(n_resamples=2.5),
        dict(n_resamples=True),
        dict(cycles=0),
    ],
)
def test_bootstrap_invalid_inputs(kwargs):
    args = dict(
        cycles=100,
        confidence=0.9,
        n_resamples=5,
        seed=1,
        method="mle",
        location_bounds=(-2, 0),
        shape_bounds=(0.5, 5),
    )
    args.update(kwargs)
    with pytest.raises(ValueError):
        wf.bootstrap_return_level([1, 2, 3, 4], **args)


@pytest.mark.parametrize(
    "args",
    [
        (0, 0.05, 5, "source"),
        (1, 0, 5, "source"),
        (1, 1, 5, "source"),
        (1, 0.05, 0, "source"),
        (1, 0.05, 5, ""),
    ],
)
def test_invalid_critical_values(args):
    with pytest.raises(ValueError):
        wf.CriticalValue(*args)


def test_critical_value_sample_size_must_match():
    with pytest.raises(ValueError, match="sample size"):
        wf.goodness_of_fit(
            [1, 2, 3, 4, 5],
            wf.WeibullDistribution(1, 1),
            ks_critical=wf.CriticalValue(1, 0.05, 10, "synthetic control"),
        )


def test_ad_retains_finite_interior_observations_with_tiny_ratios():
    """NIST e-Handbook, online edition 2026, 1.3.5.14 and 1.3.6.6.8.

    At x_i=i*10^-200, k=.5 and scale=10^200, log F_i~.5 ln(i)-200 ln(10).
    The log-survival corrections are below 3*10^-200; their contribution
    is below float precision. The raw AD sum therefore remains finite.
    """
    law = wf.WeibullDistribution(0.5, 1e200)
    result = wf.goodness_of_fit(np.arange(1, 6) * 1e-200, law)
    expected = -5 - sum(
        (2 * i - 1) / 5 * (0.5 * math.log(i) - 200 * math.log(10)) for i in range(1, 6)
    )
    assert result.ad.statistic == pytest.approx(expected)
