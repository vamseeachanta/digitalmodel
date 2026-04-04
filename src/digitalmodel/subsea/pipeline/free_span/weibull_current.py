"""
DNV RP F105 Section 3.3 — Weibull current profile fitting.

Fits a piecewise Weibull distribution to current speed vs exceedance
probability data, then extrapolates to arbitrary return periods.

The method fits two Weibull tails (upper and lower) from the provided
current–exceedance pairs and uses monotonic piecewise cubic interpolation
(PCHIP) for intermediate probabilities.

Weibull exceedance model:
    P(U > u) = exp(−(u/a)^k)
    → k = ln(ln(P2)/ln(P1)) / ln(U2/U1)
    → U(P) = U_ref × (ln(P) / ln(P_ref))^(1/k)

where P is the exceedance probability and U is the current speed.

Reference: DNV-RP-F105, Free Spanning Pipelines, Section 3.3 (2006/2017).
"""
from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Sequence

import numpy as np
from scipy.interpolate import PchipInterpolator


@dataclass
class WeibullFitResult:
    """Output of ``fit_weibull_current``.

    Attributes
    ----------
    k_upper : float
        Weibull shape parameter fitted to the two highest current speeds.
    k_lower : float
        Weibull shape parameter fitted to the two lowest current speeds.
    current_speeds : list[float]
        Input current speeds [m/s].
    exceedance_probs : list[float]
        Input exceedance probabilities.
    """
    k_upper: float
    k_lower: float
    current_speeds: list[float]
    exceedance_probs: list[float]


def fit_weibull_current(
    exceedance_probs: Sequence[float],
    current_speeds: Sequence[float],
) -> WeibullFitResult:
    """Fit piecewise Weibull to current speed vs exceedance probability.

    Parameters
    ----------
    exceedance_probs
        Exceedance probabilities corresponding to each current speed.
        P(U > u_i) — must be strictly between 0 and 1.
    current_speeds
        Current speeds [m/s] at each probability level.

    Returns
    -------
    WeibullFitResult
        Fitted Weibull parameters for upper and lower tails.

    Raises
    ------
    ValueError
        If fewer than 2 data points or lengths don't match.
    """
    probs = list(exceedance_probs)
    speeds = list(current_speeds)

    if len(probs) != len(speeds):
        raise ValueError(
            f"Length mismatch: {len(probs)} probabilities vs "
            f"{len(speeds)} current speeds."
        )

    # Filter out zero or negative exceedance probabilities (log undefined).
    # The MATLAB code computes exceedance as round(1e8*(1-cumsum(occ)))/1e8,
    # which can produce 0.0 for the highest current.  These are excluded
    # from the Weibull fit — their current speed is already the max observed.
    valid = [(s, p) for s, p in zip(speeds, probs) if p > 0]
    if len(valid) < 2:
        raise ValueError("At least 2 data points with P > 0 required for Weibull fit.")
    speeds = [v[0] for v in valid]
    probs = [v[1] for v in valid]

    # Upper tail: two highest current speeds
    idx_max = int(np.argmax(speeds))
    u_max = speeds[idx_max]
    p_max = probs[idx_max]

    remaining = [(s, p) for i, (s, p) in enumerate(zip(speeds, probs))
                 if i != idx_max]
    u_max2, p_max2 = max(remaining, key=lambda x: x[0])

    k_upper = (
        math.log(math.log(p_max2) / math.log(p_max))
        / math.log(u_max2 / u_max)
    )

    # Lower tail: two lowest current speeds
    idx_min = int(np.argmin(speeds))
    u_min = speeds[idx_min]
    p_min = probs[idx_min]

    remaining2 = [(s, p) for i, (s, p) in enumerate(zip(speeds, probs))
                  if i != idx_min]
    u_min2, p_min2 = min(remaining2, key=lambda x: x[0])

    k_lower = (
        math.log(math.log(p_min2) / math.log(p_min))
        / math.log(u_min2 / u_min)
    )

    return WeibullFitResult(
        k_upper=k_upper,
        k_lower=k_lower,
        current_speeds=speeds,
        exceedance_probs=probs,
    )


def extrapolate_current(
    exceedance_probs: Sequence[float],
    current_speeds: Sequence[float],
    target_probs: float | Sequence[float],
) -> float | list[float]:
    """Extrapolate current speed(s) at target exceedance probability(ies).

    Uses a piecewise Weibull fit:
    - Upper tail (beyond max current): Weibull extrapolation from 2 highest points
    - Lower tail (beyond min current): Weibull extrapolation from 2 lowest points
    - Intermediate: PCHIP monotonic interpolation in log-probability space

    Parameters
    ----------
    exceedance_probs
        Exceedance probabilities at known current speeds.
    current_speeds
        Known current speeds [m/s].
    target_probs
        Exceedance probability(ies) at which to evaluate current speed.
        Single float or array-like.

    Returns
    -------
    float or list[float]
        Extrapolated current speed(s) [m/s].

    Example
    -------
    100-year return period current from hourly occurrence data::

        # Currents with cumulative probabilities (not exceedance)
        cum_probs = [0.80, 0.90, 0.95, 0.99, 1.00]
        speeds = [0.04, 0.20, 0.40, 0.60, 1.00]
        # Exceedance = 1 - cumulative
        exc_probs = [1 - p for p in cum_probs]

        # 100-year exceedance for 24-hr duration:
        p_100yr = 24 / (24 * 365.25 * 100)
        u_100yr = extrapolate_current(exc_probs, speeds, p_100yr)
    """
    probs = list(exceedance_probs)
    speeds = list(current_speeds)

    scalar_input = isinstance(target_probs, (int, float))
    if scalar_input:
        targets = [float(target_probs)]
    else:
        targets = [float(t) for t in target_probs]

    # Validate target probabilities before any computation
    for p_target in targets:
        if p_target <= 0 or p_target >= 1:
            raise ValueError(
                f"Target probability {p_target} must be in (0, 1)."
            )

    # Preserve max speed from zero-exceedance entries before filtering
    max_observed_speed = max(speeds)

    # Filter out zero-exceedance entries (same as fit_weibull_current)
    valid = [(s, p) for s, p in zip(speeds, probs) if p > 0]
    if len(valid) < 2:
        raise ValueError("At least 2 data points with P > 0 required.")
    speeds = [v[0] for v in valid]
    probs = [v[1] for v in valid]

    # Fit Weibull
    fit = fit_weibull_current(probs, speeds)

    # Identify upper and lower reference points
    idx_max = int(np.argmax(speeds))
    u_max = speeds[idx_max]
    p_max = probs[idx_max]

    remaining = [(s, p) for i, (s, p) in enumerate(zip(speeds, probs))
                 if i != idx_max]
    u_max2, p_max2 = max(remaining, key=lambda x: x[0])

    idx_min = int(np.argmin(speeds))
    u_min = speeds[idx_min]
    p_min = probs[idx_min]

    remaining2 = [(s, p) for i, (s, p) in enumerate(zip(speeds, probs))
                  if i != idx_min]
    u_min2, p_min2 = min(remaining2, key=lambda x: x[0])

    # Build PCHIP interpolator for intermediate range
    # Use log(prob) as x-axis for monotonicity
    if len(speeds) > 2:
        interp = PchipInterpolator(
            [math.log(p_max2), math.log(p_min2)],
            [u_max2, u_min2],
        )
    else:
        interp = None

    results = []
    for p_target in targets:
        if p_target <= p_max2:
            # Upper tail: extrapolate using Weibull from highest 2 points
            u = u_max * (math.log(p_target) / math.log(p_max)) ** (
                1.0 / fit.k_upper
            )
        elif p_target >= p_min2:
            # Lower tail: extrapolate using Weibull from lowest 2 points
            u = u_min * (math.log(p_target) / math.log(p_min)) ** (
                1.0 / fit.k_lower
            )
        else:
            # Intermediate: PCHIP interpolation
            if interp is not None:
                u = float(interp(math.log(p_target)))
            else:
                # Only 2 points, linear interp in log space
                u = u_max2 + (u_min2 - u_max2) * (
                    (math.log(p_target) - math.log(p_max2))
                    / (math.log(p_min2) - math.log(p_max2))
                )
        results.append(u)

    if scalar_input:
        return results[0]
    return results
