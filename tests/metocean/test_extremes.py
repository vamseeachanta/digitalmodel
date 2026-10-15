# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline failure and numerical contracts for the metocean extreme wrapper."""

import numpy as np
import pytest
from scipy import stats

from digitalmodel.data_systems.data_procurement.metocean import MetoceanClient


@pytest.fixture
def client():
    """Construct the client without retrieving any provider data."""
    return MetoceanClient({"apis": {}})


def records(values):
    """Represent synthetic annual maxima as a single-use stream."""
    return ({"wave_height": value} for value in values)


def test_fit_exception_propagates_instead_of_returning_period(client, monkeypatch):
    failure = ValueError("synthetic fit failure")

    def fail_fit(values):
        raise failure

    monkeypatch.setattr(stats.gumbel_r, "fit", fail_fit)
    with pytest.raises(ValueError) as caught:
        client.calculate_extremes(records([1.0, 2.0, 3.0]), "wave_height", [10])
    assert caught.value is failure


@pytest.mark.parametrize("params", [(np.nan, 1.0), (0.0, np.inf), (0.0, 0.0), (0.0, -1.0)])
def test_invalid_fit_is_explicit_failure(client, monkeypatch, params):
    monkeypatch.setattr(stats.gumbel_r, "fit", lambda values: params)
    with pytest.raises(ValueError, match="fit parameters"):
        client.calculate_extremes(records([1.0, 2.0, 3.0]), "wave_height", [10])


@pytest.mark.parametrize("quantile", [np.nan, np.inf, -np.inf])
def test_nonfinite_return_value_is_explicit_failure(client, monkeypatch, quantile):
    monkeypatch.setattr(stats.gumbel_r, "ppf", lambda *args: quantile)
    with pytest.raises(ValueError, match="return value"):
        client.calculate_extremes(records([1.0, 2.0, 3.0]), "wave_height", [10])


@pytest.mark.parametrize("values", [[], [None], [1.0], [2.0, 2.0], [1.0, np.nan], [1.0, np.inf]])
def test_invalid_sample_is_explicit_failure(client, values):
    with pytest.raises(ValueError, match="finite nonconstant"):
        client.calculate_extremes(records(values), "wave_height", [10])


@pytest.mark.parametrize("periods", [[], [0], [1], [-10], [np.nan], [np.inf]])
def test_invalid_period_is_explicit_failure(client, periods):
    with pytest.raises(ValueError, match="return periods"):
        client.calculate_extremes(records([1.0, 2.0, 3.0]), "wave_height", periods)


def test_success_matches_direct_scipy_fit(client):
    sample = stats.gumbel_r.ppf(np.linspace(0.1, 0.9, 9), loc=2.0, scale=0.5)
    periods = [10, 50, 100]
    params = stats.gumbel_r.fit(sample)
    expected = stats.gumbel_r.ppf(1 - 1 / np.array(periods), *params)
    result = client.calculate_extremes(records([None, *sample]), "wave_height", periods)
    assert list(result) == periods
    np.testing.assert_allclose(list(result.values()), expected)
    assert all(np.diff(list(result.values())) > 0)


def test_default_periods_have_finite_return_values(client):
    result = client.calculate_extremes(records([1.0, 2.0, 3.0]), "wave_height")
    assert list(result) == [10, 50, 100]
    assert np.isfinite(list(result.values())).all()
