from __future__ import annotations

import numpy as np
import pandas as pd
import pytest
from pydantic import ValidationError
from scipy.stats import qmc

from digitalmodel.uncertainty import Prior, sample_lhs, sample_random


def test_lhs_matches_scipy_qmc_for_fixed_seed():
    priors = [
        Prior(name="x", dist="uniform", low=10.0, high=20.0),
        Prior(name="y", dist="normal", mean=5.0, std=2.0),
    ]

    samples = sample_lhs(priors, n_samples=4, seed=123)
    unit = qmc.LatinHypercube(d=2, seed=123).random(4)
    expected = pd.DataFrame(
        {
            "x": priors[0].ppf(unit[:, 0]),
            "y": priors[1].ppf(unit[:, 1]),
        }
    )

    pd.testing.assert_frame_equal(samples, expected)


def test_lhs_uniform_marginal_coverage_has_one_point_per_stratum():
    prior = Prior(name="u", dist="uniform", low=0.0, high=1.0)

    samples = sample_lhs([prior], n_samples=8, seed=7)
    strata = np.floor(samples["u"].to_numpy() * 8).astype(int)

    assert sorted(strata.tolist()) == list(range(8))


def test_random_sampler_is_seeded_and_uses_prior_names():
    prior = Prior(name="load", dist="triangular", low=1.0, mode=2.0, high=5.0)

    first = sample_random([prior], n_samples=5, seed=42)
    second = sample_random([prior], n_samples=5, seed=42)

    pd.testing.assert_frame_equal(first, second)
    assert list(first.columns) == ["load"]
    assert first["load"].between(1.0, 5.0).all()


def test_prior_rejects_incomplete_distribution_params():
    with pytest.raises(ValueError, match="uniform"):
        Prior(name="bad", dist="uniform", low=1.0)


def test_prior_rejects_blank_name():
    with pytest.raises(ValidationError):
        Prior(name="   ", dist="uniform", low=0.0, high=1.0)


@pytest.mark.parametrize(
    "prior_spec",
    [
        {"name": "bad", "dist": "uniform", "low": 0.0, "high": float("nan")},
        {"name": "bad", "dist": "normal", "mean": float("inf"), "std": 1.0},
        {
            "name": "bad",
            "dist": "triangular",
            "low": 0.0,
            "mode": 1.0,
            "high": float("inf"),
        },
        {"name": "bad", "dist": "lognormal", "mean_log": 0.0, "std_log": float("nan")},
    ],
)
def test_prior_rejects_non_finite_numeric_params(prior_spec):
    with pytest.raises(ValidationError):
        Prior.model_validate(prior_spec)


def test_lognormal_rejects_conflicting_mean_log_and_median_front_doors():
    with pytest.raises(ValidationError):
        Prior(
            name="conflict",
            dist="lognormal",
            mean_log=0.0,
            median=999.0,
            std_log=0.2,
        )


@pytest.mark.parametrize(
    "prior_spec",
    [
        {"name": "bad", "dist": "uniform", "low": 0.0, "high": 1.0, "mean": 999.0},
        {"name": "bad", "dist": "normal", "mean": 0.0, "std": 1.0, "low": -1.0},
        {
            "name": "bad",
            "dist": "lognormal",
            "median": 1.0,
            "std_log": 0.2,
            "high": 2.0,
        },
        {
            "name": "bad",
            "dist": "triangular",
            "low": 0.0,
            "mode": 1.0,
            "high": 2.0,
            "std": 0.1,
        },
    ],
)
def test_prior_rejects_distribution_irrelevant_params(prior_spec):
    with pytest.raises(ValidationError, match="irrelevant"):
        Prior.model_validate(prior_spec)
