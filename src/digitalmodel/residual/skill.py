"""Forecast-skill metrics for the residual spine (twin B #1374).

Generic float-array metrics shared by the drilling-riser adapter and, later, the
motion-forecast learning loop (#1360): RMSE, correlation, and a skill gain with a
SIGNIFICANCE margin — an improvement counts only when it exceeds the data's own
noise floor, so a within-noise "gain" is not mistaken for a real one.
"""
from __future__ import annotations

import numpy as np


def rmse(a, b) -> float:
    """Root-mean-square error between two equal-length float sequences."""
    a = np.asarray(a, dtype=float)
    b = np.asarray(b, dtype=float)
    if a.shape != b.shape:
        raise ValueError(f"rmse shape mismatch: {a.shape} vs {b.shape}")
    if a.size == 0:
        return float("nan")
    return float(np.sqrt(np.mean((a - b) ** 2)))


def correlation(a, b) -> float:
    """Pearson correlation; ``nan`` when either series is constant."""
    a = np.asarray(a, dtype=float)
    b = np.asarray(b, dtype=float)
    if a.size < 2 or np.std(a) == 0 or np.std(b) == 0:
        return float("nan")
    return float(np.corrcoef(a, b)[0, 1])


def skill_gain(rmse_before: float, rmse_after: float) -> float:
    """RMSE reduction (positive = the correction improved the fit)."""
    return float(rmse_before - rmse_after)


def is_significant(rmse_before: float, rmse_after: float, noise_sigma: float) -> bool:
    """True when the RMSE reduction clears the data's noise floor.

    A correction fit to noisy pairs can show ``rmse_after < rmse_before`` by chance;
    requiring the gain to exceed ``noise_sigma`` guards against reading noise as
    skill (the T2 correctness finding that "skill_gain > 0" is vacuous).
    """
    if not np.isfinite(noise_sigma) or noise_sigma < 0:
        return False
    return skill_gain(rmse_before, rmse_after) > noise_sigma
