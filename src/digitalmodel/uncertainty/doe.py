from __future__ import annotations

import pandas as pd
from scipy.stats import qmc

from digitalmodel.uncertainty.distributions import Prior


def _validate_priors(priors: list[Prior]) -> None:
    if not priors:
        raise ValueError("at least one prior is required")
    names = [prior.name for prior in priors]
    if len(names) != len(set(names)):
        raise ValueError("prior names must be unique")


def sample_lhs(priors: list[Prior], n_samples: int, seed: int) -> pd.DataFrame:
    """Generate a Latin Hypercube design mapped through the configured priors."""
    _validate_priors(priors)
    if n_samples <= 0:
        raise ValueError("n_samples must be positive")
    unit_samples = qmc.LatinHypercube(d=len(priors), seed=seed).random(n_samples)
    return pd.DataFrame(
        {
            prior.name: prior.ppf(unit_samples[:, index])
            for index, prior in enumerate(priors)
        }
    )


def sample_random(priors: list[Prior], n_samples: int, seed: int) -> pd.DataFrame:
    """Generate a naive Monte Carlo sample matrix from the configured priors."""
    _validate_priors(priors)
    if n_samples <= 0:
        raise ValueError("n_samples must be positive")
    return pd.DataFrame(
        {
            prior.name: prior.frozen().rvs(size=n_samples, random_state=seed + index)
            for index, prior in enumerate(priors)
        }
    )
