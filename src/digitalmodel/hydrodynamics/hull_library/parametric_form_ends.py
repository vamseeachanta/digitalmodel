"""Finite-slope, monotone end curves with analytic area and first moments."""

from functools import lru_cache

import numpy as np
from scipy.optimize import brentq
from scipy.special import betainc


def end_parameters(p, run, cp):
    """Mix a finite-slope power end with a beta end; calibrate the mean."""
    middle, transom = p.parallel_midbody_fraction, p.transom_fraction**2
    mean = (cp - middle - run * transom) / (1 - middle - run * transom)
    if not 0 < mean < 1:
        raise ValueError(
            "cb unreachable with parallel_midbody_fraction and transom_fraction"
        )
    lengths = np.array([run, 1 - middle - run]) * p.length_bp
    slopes = (
        lengths
        * np.tan(np.deg2rad([p.run_angle_deg, p.entrance_angle_deg]))
        / (p.beam / 2)
    )
    if transom:
        slopes[0] *= 2 * p.transom_fraction / (1 - transom)
    b = np.maximum(
        np.array([p.stern_fullness, p.bow_fullness]) + 1, 2 * mean / (1 - mean)
    )
    q = np.maximum(b, 4 * slopes / mean)
    weight = slopes / q
    beta_mean = (mean - weight * q / (q + 1)) / (1 - weight)
    a = b * (1 - beta_mean) / beta_mean
    if np.any(a <= 1):
        raise ValueError("cb/end angles cannot produce tangent-continuous ends")
    first = weight * (0.5 - 1 / ((q + 1) * (q + 2)))
    first += (1 - weight) * (1 - a * (a + 1) / ((a + b) * (a + b + 1))) / 2
    return mean, a, b, q, weight, first


def end_curve(s, coefficients, index):
    """C1 at both joins: F(0)=0, F'(0)=slope, F(1)=1, F'(1)=0."""
    _, a, b, q, weight, _ = coefficients
    s = np.clip(s, 0, 1)
    # expm1 avoids cancellation for the endpoint derivative probe.
    with np.errstate(divide="ignore"):
        power = -np.expm1(q[index] * np.log1p(-s))
    return weight[index] * power + (1 - weight[index]) * betainc(a[index], b[index], s)


def _sac_centroid(p, run, cp):
    mean, _, _, _, _, first = end_parameters(p, run, cp)
    middle, transom = p.parallel_midbody_fraction, p.transom_fraction**2
    entrance = 1 - middle - run
    moment = run**2 * (transom / 2 + (1 - transom) * first[0])
    moment += middle * (run + middle / 2) + entrance * mean - entrance**2 * first[1]
    return moment / cp - 0.5


@lru_cache(maxsize=128)
def solve_sac(p, cp):
    """Re-solve the run length using exact moments of the tangent end curves."""
    middle, transom = p.parallel_midbody_fraction, p.transom_fraction**2
    low, high = max(1e-8, 0.5 - middle), min(0.5, 1 - middle - 1e-8)
    if transom:
        high = min(high, (cp - middle) / transom - 1e-8)
    if high < low or cp <= middle or cp >= 1:
        raise ValueError(
            "cb unreachable with parallel_midbody_fraction/transom_fraction"
        )

    def residual(run):
        return _sac_centroid(p, run, cp) - p.lcb_fraction

    if abs(residual(low)) < 1e-12:
        return low
    if high == low or residual(low) * residual(high) > 0:
        raise ValueError(
            "lcb_fraction unreachable with cb, fullness and parallel_midbody_fraction"
        )
    return brentq(residual, low, high, xtol=1e-12)
