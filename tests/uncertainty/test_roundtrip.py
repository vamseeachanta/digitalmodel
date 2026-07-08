from __future__ import annotations

import numpy as np

from digitalmodel.uncertainty import Prior, run_matrix, sample_lhs


def test_linear_model_recovers_analytic_mean_and_variance():
    x_value = 4.0
    priors = [
        Prior(name="a", dist="normal", mean=3.0, std=0.5),
        Prior(name="b", dist="uniform", low=1.0, high=2.0),
    ]
    samples = sample_lhs(priors, n_samples=2000, seed=2026)

    result = run_matrix(
        lambda sample: {"y": sample["a"] * x_value + sample["b"]},
        samples,
    )

    expected_mean = 3.0 * x_value + 1.5
    expected_variance = (0.5**2) * (x_value**2) + ((2.0 - 1.0) ** 2) / 12.0
    np.testing.assert_allclose(result["y"].mean(), expected_mean, rtol=0.02)
    np.testing.assert_allclose(result["y"].var(ddof=0), expected_variance, rtol=0.05)
