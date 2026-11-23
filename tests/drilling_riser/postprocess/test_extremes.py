"""Seed-maxima extreme statistics (riser plan section 7.2): Gumbel fit, most probable maximum, 90 % CI."""

from __future__ import annotations

import math

import numpy as np
import pytest

from digitalmodel.drilling_riser.postprocess.extremes import gumbel_fit, gumbel_quantile

SAMPLE = [7.02, 7.41, 6.88, 7.86, 7.15, 7.33, 6.95, 7.58, 7.21, 7.09]


def test_mle_recovers_parameters_of_a_large_sample():
    rng = np.random.default_rng(1)
    x = rng.gumbel(10.0, 2.0, size=20000)
    fit = gumbel_fit(x)
    assert fit.loc == pytest.approx(10.0, rel=0.01)
    assert fit.scale == pytest.approx(2.0, rel=0.02)
    assert fit.mpm == fit.loc


def test_mle_matches_an_independent_implementation():
    stats = pytest.importorskip("scipy.stats")
    loc, scale = stats.gumbel_r.fit(SAMPLE)
    fit = gumbel_fit(SAMPLE)
    assert fit.loc == pytest.approx(loc, rel=1e-6)
    assert fit.scale == pytest.approx(scale, rel=1e-5)


def test_ci90_is_the_asymptotic_normal_interval_of_the_location():
    fit = gumbel_fit(SAMPLE)
    var_factor = 1.0 + 6.0 / math.pi**2 * (1.0 - 0.5772156649015329) ** 2  # 1.1087
    half = 1.6448536269514722 * fit.scale * math.sqrt(var_factor / len(SAMPLE))
    assert fit.ci_low == pytest.approx(fit.mpm - half)
    assert fit.ci_high == pytest.approx(fit.mpm + half)
    assert fit.n == 10
    assert fit.observed_extreme == max(SAMPLE)


def test_quantile_of_a_gumbel_maximum():
    assert gumbel_quantile(1.0, 2.0, 0.9) == pytest.approx(1.0 - 2.0 * math.log(-math.log(0.9)))
    fit = gumbel_fit(SAMPLE)
    assert fit.p90 == pytest.approx(gumbel_quantile(fit.loc, fit.scale, 0.9))


def test_minima_are_fitted_as_maxima_of_the_negated_values():
    lo = gumbel_fit(SAMPLE, kind="min")
    hi = gumbel_fit([-v for v in SAMPLE], kind="max")
    assert lo.mpm == pytest.approx(-hi.mpm)
    assert lo.ci_low == pytest.approx(-hi.ci_high)
    assert lo.ci_high == pytest.approx(-hi.ci_low)
    assert lo.observed_extreme == min(SAMPLE)
    assert lo.p90 == pytest.approx(-hi.p90)


def test_constant_seed_maxima_give_a_degenerate_fit():
    fit = gumbel_fit([5.0] * 10)
    assert fit.scale == 0.0
    assert fit.mpm == fit.ci_low == fit.ci_high == 5.0


def test_fit_diagnostics_ppcc_and_leave_one_out():
    good = gumbel_fit([gumbel_quantile(0.0, 1.0, (i - 0.44) / (10 + 0.12)) for i in range(1, 11)])
    assert good.ppcc == pytest.approx(1.0, abs=1e-9)
    fit = gumbel_fit(SAMPLE)
    assert 0.0 <= fit.loo_max_rel_change < 0.05  # dropping any one seed moves the MPM by < 5 %


def test_too_few_or_non_finite_values_are_rejected():
    with pytest.raises(ValueError):
        gumbel_fit([1.0])
    with pytest.raises(ValueError):
        gumbel_fit([1.0, float("nan"), 2.0])
    with pytest.raises(ValueError):
        gumbel_fit(SAMPLE, kind="mean")
