"""
Fatigue Reliability Analysis Module
====================================

Probabilistic fatigue assessment using First-Order Reliability Method (FORM)
approximation, sensitivity analysis (Sobol indices), and Monte Carlo simulation.

This module works with the existing S-N curve infrastructure in
``digitalmodel.structural.fatigue.sn_curves`` and uses only numpy/scipy
(no OpenTURNS hard dependency).

Key Functions:
- ``failure_probability`` — FORM-based failure probability for a given S-N
  curve, stress range, and design life with uncertainty in stress and S-N.
- ``reliability_index`` — Convert failure probability to reliability index
  (beta = Phi^-1(1 - Pf)).
- ``sensitivity_analysis`` — First-order Sobol sensitivity indices via
  Saltelli sampling.
- ``monte_carlo_fatigue`` — Monte Carlo simulation of fatigue life with
  full distribution output.

Theory
------
The limit state function is:

    g = ln(N_allowable) - ln(N_design)

where N_allowable = A * S^(-m) from the S-N curve. Uncertainty is modelled
through lognormal random variables on stress range and S-N intercept (A).

For the FORM approximation, in log-space the limit state becomes linear
and the reliability index beta can be computed analytically as:

    beta = mu_g / sigma_g

Author: Digital Model Team
Version: 1.0.0
"""

import numpy as np
from scipy.stats import norm
from typing import Dict, Optional, Union

from .sn_curves import SNCurveBase

__all__ = [
    "failure_probability",
    "reliability_index",
    "sensitivity_analysis",
    "monte_carlo_fatigue",
]


def reliability_index(pf: float) -> float:
    """
    Convert failure probability to reliability index.

    Parameters
    ----------
    pf : float
        Failure probability in [0, 1].

    Returns
    -------
    float
        Reliability index beta = Phi^-1(1 - Pf).

    Raises
    ------
    ValueError
        If pf is not in [0, 1].
    """
    if pf < 0.0 or pf > 1.0:
        raise ValueError(f"Failure probability must be between 0 and 1, got {pf}")

    if pf == 0.0:
        return float("inf")
    if pf == 1.0:
        return float("-inf")

    return float(norm.ppf(1.0 - pf))


def failure_probability(
    sn_curve: SNCurveBase,
    stress_range: float,
    design_life: float,
    cov_stress: float = 0.1,
    cov_sn: float = 0.1,
) -> Dict[str, float]:
    """
    Estimate failure probability using FORM approximation in log-space.

    The limit state function in log-space is:

        g = ln(A) - m * ln(S) - ln(N_design)

    where A and S are treated as lognormal random variables.

    Parameters
    ----------
    sn_curve : SNCurveBase
        S-N curve object (must have attributes ``A`` and ``m``).
    stress_range : float
        Mean (nominal) stress range in MPa.
    design_life : float
        Design life in number of cycles.
    cov_stress : float
        Coefficient of variation for stress range (default 0.1).
    cov_sn : float
        Coefficient of variation for S-N intercept A (default 0.1).

    Returns
    -------
    dict
        Keys: pf, beta, safety_margin_mean, safety_margin_std.
    """
    A = sn_curve.A
    m = sn_curve.m

    # Log-space parameters for lognormal variables
    # For lognormal X with mean mu_X and CoV delta:
    #   sigma_lnX = sqrt(ln(1 + delta^2))
    #   mu_lnX = ln(mu_X) - 0.5 * sigma_lnX^2

    # S-N intercept uncertainty
    if cov_sn > 0:
        sigma_ln_A = np.sqrt(np.log(1.0 + cov_sn**2))
    else:
        sigma_ln_A = 0.0
    mu_ln_A = np.log(A) - 0.5 * sigma_ln_A**2

    # Stress range uncertainty
    if cov_stress > 0:
        sigma_ln_S = np.sqrt(np.log(1.0 + cov_stress**2))
    else:
        sigma_ln_S = 0.0
    mu_ln_S = np.log(stress_range) - 0.5 * sigma_ln_S**2

    # Limit state: g = ln(A) - m * ln(S) - ln(N_design)
    # In terms of the standard normal variables:
    #   E[g] = mu_ln_A - m * mu_ln_S - ln(N_design)
    #   Var[g] = sigma_ln_A^2 + m^2 * sigma_ln_S^2
    mu_g = mu_ln_A - m * mu_ln_S - np.log(design_life)
    sigma_g_sq = sigma_ln_A**2 + m**2 * sigma_ln_S**2

    if sigma_g_sq == 0.0:
        # Deterministic case
        if mu_g > 0:
            pf = 0.0
            beta = float("inf")
        elif mu_g < 0:
            pf = 1.0
            beta = float("-inf")
        else:
            pf = 0.5
            beta = 0.0
    else:
        sigma_g = np.sqrt(sigma_g_sq)
        beta = float(mu_g / sigma_g)
        pf = float(norm.cdf(-beta))

    return {
        "pf": pf,
        "beta": beta,
        "safety_margin_mean": float(mu_g),
        "safety_margin_std": float(np.sqrt(sigma_g_sq)) if sigma_g_sq > 0 else 0.0,
    }


def sensitivity_analysis(
    sn_curve: SNCurveBase,
    stress_range: float,
    design_life: float,
    cov_stress: float = 0.1,
    cov_sn: float = 0.1,
    n_samples: int = 5000,
    seed: Optional[int] = None,
) -> Dict[str, Union[Dict[str, float], int]]:
    """
    Compute first-order Sobol sensitivity indices for the fatigue limit state.

    Uses the Jansen estimator with Saltelli sampling scheme. The two input
    variables are:

    - ``stress``: lognormal stress range
    - ``sn``: lognormal S-N intercept (A)

    Parameters
    ----------
    sn_curve : SNCurveBase
        S-N curve with ``A`` and ``m`` attributes.
    stress_range : float
        Mean stress range (MPa).
    design_life : float
        Design life (cycles).
    cov_stress : float
        Coefficient of variation for stress.
    cov_sn : float
        Coefficient of variation for S-N intercept.
    n_samples : int
        Base sample size (total evaluations = n_samples * (2 + k)).
    seed : int, optional
        Random seed for reproducibility.

    Returns
    -------
    dict
        ``first_order``: dict with keys ``stress`` and ``sn``.
        ``n_evaluations``: total number of model evaluations.
    """
    rng = np.random.default_rng(seed)
    k = 2  # number of input variables

    # Lognormal parameters
    if cov_sn > 0:
        sigma_ln_A = np.sqrt(np.log(1.0 + cov_sn**2))
    else:
        sigma_ln_A = 1e-12  # near-zero to avoid division issues
    mu_ln_A = np.log(sn_curve.A) - 0.5 * sigma_ln_A**2

    if cov_stress > 0:
        sigma_ln_S = np.sqrt(np.log(1.0 + cov_stress**2))
    else:
        sigma_ln_S = 1e-12
    mu_ln_S = np.log(stress_range) - 0.5 * sigma_ln_S**2

    m = sn_curve.m
    ln_Nd = np.log(design_life)

    # Generate two independent sample matrices in unit-normal space
    # Each row is a sample, each column is a variable [stress, sn]
    U_A = rng.standard_normal((n_samples, k))
    U_B = rng.standard_normal((n_samples, k))

    def _evaluate(U: np.ndarray) -> np.ndarray:
        """Evaluate limit state g for unit-normal samples."""
        # Column 0 = stress, Column 1 = sn intercept
        ln_S = mu_ln_S + sigma_ln_S * U[:, 0]
        ln_A = mu_ln_A + sigma_ln_A * U[:, 1]
        return ln_A - m * ln_S - ln_Nd

    f_A = _evaluate(U_A)
    f_B = _evaluate(U_B)

    # Saltelli estimator for first-order indices
    sobol_first = {}
    var_names = ["stress", "sn"]

    f0_sq = np.mean(f_A) * np.mean(f_B)
    var_total = np.var(np.concatenate([f_A, f_B]), ddof=0)

    if var_total < 1e-30:
        # No variance => indices undefined, return equal
        return {
            "first_order": {"stress": 0.5, "sn": 0.5},
            "n_evaluations": n_samples * (2 + k),
        }

    for i, name in enumerate(var_names):
        # Create AB_i: take column i from B, rest from A
        U_AB_i = U_A.copy()
        U_AB_i[:, i] = U_B[:, i]
        f_AB_i = _evaluate(U_AB_i)

        # Jansen estimator: S_i = 1 - Var(f_B - f_AB_i) / (2 * Var)
        Si = 1.0 - np.var(f_B - f_AB_i, ddof=0) / (2.0 * var_total)
        sobol_first[name] = float(max(Si, 0.0))  # clip small negatives

    return {
        "first_order": sobol_first,
        "n_evaluations": n_samples * (2 + k),
    }


def monte_carlo_fatigue(
    sn_curve: SNCurveBase,
    stress_range: float,
    design_life: float,
    cov_stress: float = 0.1,
    cov_sn: float = 0.1,
    n_samples: int = 10000,
    seed: Optional[int] = None,
) -> Dict[str, Union[float, np.ndarray, Dict[str, float]]]:
    """
    Monte Carlo simulation of fatigue life.

    Generates random realizations of stress range and S-N intercept,
    computes fatigue life for each, and estimates failure probability
    as the fraction of samples where life < design_life.

    Parameters
    ----------
    sn_curve : SNCurveBase
        S-N curve with ``A`` and ``m`` attributes.
    stress_range : float
        Mean stress range (MPa).
    design_life : float
        Design life (cycles).
    cov_stress : float
        Coefficient of variation for stress.
    cov_sn : float
        Coefficient of variation for S-N intercept.
    n_samples : int
        Number of Monte Carlo samples.
    seed : int, optional
        Random seed for reproducibility.

    Returns
    -------
    dict
        pf : float — estimated failure probability
        mean_life : float — mean fatigue life across samples
        std_life : float — standard deviation of fatigue life
        samples : np.ndarray — array of fatigue life values (length n_samples)
        percentiles : dict — p5, p25, p50, p75, p95 of fatigue life
    """
    rng = np.random.default_rng(seed)
    A = sn_curve.A
    m = sn_curve.m

    # Lognormal parameters for stress
    if cov_stress > 0:
        sigma_ln_S = np.sqrt(np.log(1.0 + cov_stress**2))
    else:
        sigma_ln_S = 0.0
    mu_ln_S = np.log(stress_range) - 0.5 * sigma_ln_S**2

    # Lognormal parameters for SN intercept
    if cov_sn > 0:
        sigma_ln_A = np.sqrt(np.log(1.0 + cov_sn**2))
    else:
        sigma_ln_A = 0.0
    mu_ln_A = np.log(A) - 0.5 * sigma_ln_A**2

    # Generate lognormal samples
    ln_S = mu_ln_S + sigma_ln_S * rng.standard_normal(n_samples)
    ln_A = mu_ln_A + sigma_ln_A * rng.standard_normal(n_samples)

    # Fatigue life: N = A_sample * S_sample^(-m)
    # In log-space: ln(N) = ln(A) - m * ln(S)
    ln_N = ln_A - m * ln_S
    N_samples = np.exp(ln_N)

    # Failure probability
    n_failures = np.sum(N_samples < design_life)
    pf = float(n_failures / n_samples)

    # Statistics
    mean_life = float(np.mean(N_samples))
    std_life = float(np.std(N_samples, ddof=1))
    percentiles = {
        "p5": float(np.percentile(N_samples, 5)),
        "p25": float(np.percentile(N_samples, 25)),
        "p50": float(np.percentile(N_samples, 50)),
        "p75": float(np.percentile(N_samples, 75)),
        "p95": float(np.percentile(N_samples, 95)),
    }

    return {
        "pf": pf,
        "mean_life": mean_life,
        "std_life": std_life,
        "samples": N_samples,
        "percentiles": percentiles,
    }
