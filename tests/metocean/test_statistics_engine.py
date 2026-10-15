# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic numerical contracts; install digitalmodel[metocean] to run them."""

import importlib

import numpy as np
import pandas as pd
import pytest
from scipy import stats

MODULE = "digitalmodel.data_systems.data_procurement.metocean.statistics"


@pytest.fixture
def bridge():
    pytest.importorskip("metocean_stats", reason="requires digitalmodel[metocean]")
    return importlib.import_module(MODULE)


@pytest.fixture
def annual():
    # A known annual sample, with a smaller observation in each year.
    maxima = stats.gumbel_r.ppf(np.linspace(0.05, 0.95, 20), loc=3, scale=0.5)
    dates = [pd.Timestamp(year, month, 1) for year in range(2000, 2020) for month in [1, 7]]
    return pd.DataFrame({"hs": np.column_stack([maxima / 2, maxima]).ravel()}, index=dates)


@pytest.fixture
def waves():
    # Conditional log(Tp) mean increases with Hs; each bin has symmetric noise.
    hs = np.repeat(np.linspace(0.5, 5, 20), 20)
    noise = np.tile(stats.norm.ppf(np.linspace(0.025, 0.975, 20)), 20)
    tp = np.exp(1.5 + 0.1 * hs + 0.15 * noise)
    return pd.DataFrame({"hs": hs, "tp": tp}, index=pd.date_range("2000-01-01", periods=len(hs), freq="h"))


def test_annual_extremes_and_intervals_match_engine_and_scipy(bridge, annual):
    from metocean_stats.stats import return_levels_annual_max_uncertainty

    result = bridge.extremes(annual, "hs", [10, 50], seed=42)
    state = np.random.get_state()
    try:
        np.random.seed(42)
        direct = return_levels_annual_max_uncertainty(annual, "hs", "GUM", [10, 50], uncertainty=0.95)
    finally:
        np.random.set_state(state)
    np.testing.assert_allclose(result.to_numpy(), direct.to_numpy())
    maxima = annual.groupby(annual.index.year).hs.max().to_numpy()
    loc, scale = stats.gumbel_r.fit(maxima)
    np.testing.assert_allclose(result.return_levels, stats.gumbel_r.isf(1 / np.array([10, 50]), loc, scale))
    assert (result.ci_lower_rl < result.return_levels).all()
    assert (result.ci_upper_rl > result.return_levels).all()
    assert result.attrs["bootstrap_samples"] == 1000


def test_pot_intervals_and_event_rate_match_engine_and_scipy(bridge):
    from metocean_stats.stats import return_levels_pot_uncertainty

    heights = np.column_stack([np.ones(12), np.linspace(3, 6, 12), np.ones(12)]).ravel()
    data = pd.DataFrame({"hs": heights}, index=pd.date_range("2000-01-01", periods=len(heights), freq="D"))
    result = bridge.extremes(data, "hs", [10, 50], method="POT", distribution="EXP", threshold=2, separation="24h", seed=8)
    state = np.random.get_state()
    try:
        np.random.seed(8)
        direct = return_levels_pot_uncertainty(data, "hs", "EXP", [10, 50], threshold=2, r="24h", uncertainty=0.95)
    finally:
        np.random.set_state(state)
    np.testing.assert_allclose(result.to_numpy(), direct.to_numpy())
    loc, scale = stats.expon.fit(np.linspace(3, 6, 12) - 2)
    np.testing.assert_allclose(result.return_levels, 2 + stats.expon.isf(1 / (12 * np.array([10, 50])), loc, scale))
    assert result.attrs["separation"] == "24h"


def test_seeded_extremes_repeat_without_consuming_caller_rng(bridge, annual):
    before = np.random.get_state()
    first = bridge.extremes(annual, "hs", [10], seed=9)
    after = np.random.get_state()
    assert before[0] == after[0]
    np.testing.assert_array_equal(before[1], after[1])
    assert before[2:] == after[2:]
    pd.testing.assert_frame_equal(first, bridge.extremes(annual, "hs", [10], seed=9))


@pytest.mark.parametrize("kwargs,match", [
    ({"method": "unknown"}, "method"),
    ({"distribution": "unknown"}, "distribution"),
    ({"confidence": 1}, "confidence"),
    ({"method": "POT", "distribution": "GP"}, "threshold"),
    ({"method": "POT", "distribution": "GP", "threshold": 99}, "peaks"),
    ({"method": "POT", "distribution": "GP", "threshold": 1, "separation": "0h"}, "separation"),
])
def test_extremes_reject_invalid_options(bridge, annual, kwargs, match):
    with pytest.raises(ValueError, match=match):
        bridge.extremes(annual, "hs", [10], **kwargs)


@pytest.mark.parametrize("periods", [[1], [], [float("nan")], [10, 5], [10, 10]])
def test_extremes_reject_invalid_periods(bridge, annual, periods):
    with pytest.raises(ValueError, match="periods"):
        bridge.extremes(annual, "hs", periods)


def test_engine_failure_propagates_and_nonfinite_output_is_rejected(bridge, annual, monkeypatch):
    from metocean_stats import stats as engine

    def fail(*args, **kwargs):
        raise ValueError("synthetic fit failed")

    monkeypatch.setattr(engine, "return_levels_annual_max_uncertainty", fail)
    with pytest.raises(ValueError, match="synthetic fit failed"):
        bridge.extremes(annual, "hs", [10])
    monkeypatch.setattr(engine, "return_levels_annual_max_uncertainty", lambda *a, **k: pd.DataFrame({"return_levels": [np.inf], "ci_lower_rl": [1], "ci_upper_rl": [2]}))
    with pytest.raises(ValueError, match="non-finite"):
        bridge.extremes(annual, "hs", [10])


def test_joint_model_fits_known_conditional_periods(bridge, waves):
    model = bridge.joint_hs_tp(waves)
    assert model.n_dim == 2
    # The synthetic log-period mean has exactly this prescribed trend.
    mu = model.distributions[1].parameters["mu"]
    np.testing.assert_allclose(mu(np.array([1, 3, 5])), 1.5 + 0.1 * np.array([1, 3, 5]), atol=0.04)
    assert np.isfinite(model.pdf(waves[["hs", "tp"]].to_numpy())).all()
    pd.testing.assert_frame_equal(model.data, waves)


@pytest.mark.parametrize("method", ["IFORM", "ISORM"])
def test_contours_match_engine_and_normal_space_radius(bridge, waves, method):
    from metocean_stats.CMA.contours import get_contour
    from virocon import calculate_alpha

    model = bridge.joint_hs_tp(waves)
    result = bridge.environmental_contour(model, 10, state_duration_hours=1, method=method, n_points=32)
    direct = get_contour(model, return_period=10, state_duration=1, method=method, n_samples=32)
    np.testing.assert_allclose(result[["hs", "tp"]], direct.coordinates)
    # Independent Rosenblatt transform back to the normal-space circle.
    hs, tp = result.hs.to_numpy(), result.tp.to_numpy()
    u0 = stats.norm.ppf(model.distributions[0].cdf(hs))
    params = {key: dep(hs) for key, dep in model.distributions[1].parameters.items()}
    u1 = stats.norm.ppf(model.distributions[1].distribution.cdf(tp, **params))
    alpha = calculate_alpha(1, 10)
    radius = stats.norm.isf(alpha) if method == "IFORM" else np.sqrt(stats.chi2.isf(alpha, 2))
    np.testing.assert_allclose(np.hypot(u0, u1), radius, rtol=1e-7)
    assert result.attrs["method"] == method


def test_weather_window_waits_match_hand_count_and_engine(bridge):
    from metocean_stats.stats import weather_window_length_MultipleVariables

    data = pd.DataFrame({"hs": [3, 1, 1, 3, 1, 1], "wind": [1] * 6}, index=pd.date_range("2000-01-01", periods=6, freq="h"))
    result = bridge.weather_windows(data, {"hs": 2, "wind": 2}, 2)
    # Qualifying starts: 1 and 4; waits plus operation: 3,2,4,3,2 hours.
    expected = np.array([3, 2, 4, 3, 2]) / 24
    np.testing.assert_allclose(list(result.values()), [expected.mean(), *np.percentile(expected, [10, 50, 90, 95]), expected.max()])
    direct = weather_window_length_MultipleVariables(data, ["hs", "wind"], [2, 2], 2, 1)
    np.testing.assert_allclose(list(result.values()), direct)


@pytest.mark.parametrize("case", ["no_windows", "gap", "duplicate", "nan", "fractional_duration", "too_long"])
def test_weather_windows_reject_unsupported_inputs(bridge, case):
    data = pd.DataFrame({"hs": [1, 1, 3, 1, 1]}, index=pd.date_range("2000-01-01", periods=5, freq="h"))
    duration = 2
    if case == "no_windows":
        data.hs = 3
    elif case == "gap":
        data = data.drop(data.index[2])
    elif case == "duplicate":
        data.index = [data.index[0], *data.index[:4]]
    elif case == "nan":
        data.iloc[0, 0] = np.nan
    elif case == "fractional_duration":
        duration = 1.5
    elif case == "too_long":
        duration = 9
    with pytest.raises(ValueError):
        bridge.weather_windows(data, {"hs": 2}, duration)
