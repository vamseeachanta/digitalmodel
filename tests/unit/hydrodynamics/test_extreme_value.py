# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent quantiles and synthetic joint-model checks for issue #211."""

from dataclasses import FrozenInstanceError
from importlib import import_module

import numpy as np
import pytest
from scipy.stats import genextreme, gumbel_r, kstest, lognorm, norm, weibull_min


@pytest.fixture
def eva():
    # Import at setup so the red run demonstrates every missing API case.
    return import_module("digitalmodel.hydrodynamics.extreme_value")


def _fit(eva, xi=0.1):
    return eva.GEVFit(loc=5.0, scale=1.0, xi=xi, n=5000, ks_statistic=0.01)


# Entirely synthetic coefficients; no site or benchmark fitted values.
HS_WEIBULL = (1.4, 2.0, 0.5)
TP_LOGNORMAL = ((0.5, 1.2, 0.25), (0.06, 0.25, -0.3))
BETA = 4.498463732930637


def _contour(eva, **kwargs):
    options = dict(beta=BETA, hs_weibull=HS_WEIBULL,
                   tp_lognormal=TP_LOGNORMAL, n_points=72)
    options.update(kwargs)
    return eva.iform_contour_hs_tp(**options)


def test_gev_return_level_heavy_tail_hand_value(eva):
    # mpmath, 30 digits, independent of scipy; second oracle pins shape sign.
    level = eva.gev_return_level(_fit(eva), 100)
    assert level == pytest.approx(10.84097623796323, rel=1e-12)
    assert level == pytest.approx(
        genextreme.ppf(0.99, c=-0.1, loc=5, scale=1), rel=1e-12)


def test_gev_return_level_bounded_tail_hand_value(eva):
    level = eva.gev_return_level(_fit(eva, xi=-0.1), 100)
    assert level == pytest.approx(8.687257748651379, rel=1e-12)
    assert level == pytest.approx(
        genextreme.ppf(0.99, c=0.1, loc=5, scale=1), rel=1e-12)


def test_gev_return_level_gumbel_limit(eva):
    assert eva.gev_return_level(_fit(eva, xi=0), 100) == pytest.approx(
        9.600149226776580, rel=1e-12)
    assert eva.gev_return_level(_fit(eva, xi=1e-9), 100) == pytest.approx(
        9.600149237357266, rel=1e-12)
    # Large T must retain precision after 1 - 1/T would round to one.
    assert eva.gev_return_level(_fit(eva, xi=0), 1e20) == pytest.approx(
        gumbel_r.isf(1e-20, loc=5, scale=1), rel=1e-12)


def test_fit_gev_recovers_parameters(eva):
    data = genextreme.rvs(-0.1, loc=5, scale=1, size=5000,
                         random_state=np.random.default_rng(211))
    fit = eva.fit_gev(data)
    assert abs(fit.xi - 0.1) <= 0.05
    assert abs(fit.loc - 5) <= 0.075
    assert abs(fit.scale - 1) <= 0.065
    assert fit.n == 5000
    assert np.isfinite(fit.ks_statistic)
    expected = kstest(data, "genextreme", args=(-fit.xi, fit.loc, fit.scale))
    assert fit.ks_statistic == pytest.approx(expected.statistic, abs=1e-12)
    with pytest.raises(FrozenInstanceError):
        fit.xi = 0


def test_fit_gev_gumbel_sample_has_near_zero_shape(eva):
    data = gumbel_r.rvs(loc=5, scale=1, size=5000,
                        random_state=np.random.default_rng(211))
    assert abs(eva.fit_gev(data).xi) < 0.05


def test_fit_gev_shape_sign_convention(eva):
    data = genextreme.rvs(-0.3, loc=5, scale=1, size=5000,
                         random_state=np.random.default_rng(211))
    assert eva.fit_gev(data).xi > 0


def test_iform_beta_hand_value_and_monotonic(eva):
    assert eva.iform_beta(100) == pytest.approx(BETA, abs=1e-9)
    assert eva.iform_beta(10) < eva.iform_beta(100) < eva.iform_beta(10000)
    assert eva.iform_beta(1e20) == pytest.approx(
        norm.isf(3 / (1e20 * 365.25 * 24)), rel=1e-12)
    assert eva.iform_beta(100, sea_state_hours=1) > eva.iform_beta(100)


def test_iform_contour_hand_points(eva):
    # Independent 30-digit mpmath evaluations in approved revision 2.
    hs, tp = _contour(eva)
    indices = [0, 9, 18, 36, 54]
    np.testing.assert_allclose(hs[indices], [
        12.70799140604371, 8.706080575591937, 2.039337666598377,
        0.5002494440978627, 2.039337666598377], rtol=1e-9, atol=0)
    np.testing.assert_allclose(tp[indices], [
        15.88999917593648, 16.61827734045888, 16.67455020583108,
        4.523117772579061, 2.869521545287967], rtol=1e-9, atol=0)


def test_iform_contour_inverse_transform(eva):
    hs, tp = _contour(eva)
    theta = np.linspace(0, 2 * np.pi, 73)
    mu = 0.5 + 1.2 * hs**0.25
    sigma = 0.06 + 0.25 * np.exp(-0.3 * hs)
    u1 = norm.ppf(weibull_min.cdf(hs, 1.4, loc=0.5, scale=2))
    u2 = norm.ppf(lognorm.cdf(tp, sigma, scale=np.exp(mu)))
    np.testing.assert_allclose(u1, BETA * np.cos(theta), atol=1e-8, rtol=0)
    np.testing.assert_allclose(u2, BETA * np.sin(theta), atol=1e-8, rtol=0)


def test_iform_contour_geometry(eva):
    hs, tp = _contour(eva)
    assert hs.shape == tp.shape == (73,)
    assert hs[0] == hs[-1] and tp[0] == tp[-1]
    assert np.argmax(hs) == 0
    assert hs[0] == pytest.approx(
        weibull_min.ppf(norm.cdf(BETA), 1.4, loc=0.5, scale=2), rel=1e-12)
    assert np.all(hs >= 0.5)
    assert tp[0] == pytest.approx(np.exp(0.5 + 1.2 * hs[0]**0.25))
    assert _contour(eva, n_points=4)[0].size == 5


@pytest.mark.parametrize("case", [
    "few", "nan_maxima", "inf_maxima", "matrix", "constant",
    "short_period", "zero_period", "nan_period", "inf_period",
    "zero_scale", "negative_scale", "nan_loc", "inf_xi",
    "beta_period", "zero_hours", "nan_hours", "negative_beta", "nan_beta",
    "weibull_shape", "weibull_scale", "weibull_location", "weibull_length",
    "few_points", "fractional_points", "nan_points",
    "nan_coefficients", "coefficient_length", "zero_sigma", "negative_sigma",
    "undefined_mean", "overflow_tp",
])
def test_invalid_inputs(eva, case):
    with pytest.raises(ValueError):
        if case in {"few", "nan_maxima", "inf_maxima", "matrix", "constant"}:
            data = {"few": [1, 2], "nan_maxima": [1, np.nan, 3],
                    "inf_maxima": [1, np.inf, 3], "matrix": [[1, 2, 3]],
                    "constant": [1, 1, 1]}[case]
            eva.fit_gev(data)
        elif case.endswith("period") and case != "beta_period":
            period = {"short_period": 1, "zero_period": 0,
                      "nan_period": np.nan, "inf_period": np.inf}[case]
            eva.gev_return_level(_fit(eva), period)
        elif case in {"zero_scale", "negative_scale", "nan_loc", "inf_xi"}:
            args = dict(loc=5, scale=1, xi=0.1, n=3, ks_statistic=0)
            key, value = {"zero_scale": ("scale", 0),
                          "negative_scale": ("scale", -1),
                          "nan_loc": ("loc", np.nan), "inf_xi": ("xi", np.inf)}[case]
            args[key] = value
            eva.gev_return_level(eva.GEVFit(**args), 100)
        elif case in {"beta_period", "zero_hours", "nan_hours"}:
            period, hours = {"beta_period": (1, 3), "zero_hours": (100, 0),
                             "nan_hours": (100, np.nan)}[case]
            eva.iform_beta(period, hours)
        else:
            options = {
                "negative_beta": dict(beta=-1), "nan_beta": dict(beta=np.nan),
                "weibull_shape": dict(hs_weibull=(0, 2, 0.5)),
                "weibull_scale": dict(hs_weibull=(1.4, -2, 0.5)),
                "weibull_location": dict(hs_weibull=(1.4, 2, np.inf)),
                "weibull_length": dict(hs_weibull=(1.4, 2)),
                "few_points": dict(n_points=3),
                "fractional_points": dict(n_points=4.5),
                "nan_points": dict(n_points=np.nan),
                "nan_coefficients": dict(tp_lognormal=((np.nan, 1, 1), (1, 0, 0))),
                "coefficient_length": dict(tp_lognormal=((1, 1), (1, 0, 0))),
                "zero_sigma": dict(tp_lognormal=((1, 0, 1), (0, 0, 0))),
                "negative_sigma": dict(tp_lognormal=((1, 0, 1), (-0.1, 1, -1))),
                "undefined_mean": dict(hs_weibull=(1.4, 2, -20)),
                "overflow_tp": dict(tp_lognormal=((1000, 0, 1), (1, 0, 0))),
            }[case]
            _contour(eva, **options)
