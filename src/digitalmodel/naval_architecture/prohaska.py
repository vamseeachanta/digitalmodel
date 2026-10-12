"""Prohaska form-factor regression with transformed-response uncertainty."""
from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Sequence

import numpy as np

PROHASKA_FN_MIN = 0.10
PROHASKA_FN_MAX = 0.20
PROHASKA_MIN_POINTS = 6
PROHASKA_MAX_CONDITION = 1e6

@dataclass(frozen=True)
class ProhaskaFit:
    k: float
    c: float
    residual_rms: float
    k_ci95: tuple
    condition_number: float
    n_points: int
    fn_window: tuple = (PROHASKA_FN_MIN, PROHASKA_FN_MAX)
    error_model: str = (
        "y = C_T/C_F = (1+k) + c Fn^4/C_F + e, e independent, homoscedastic and additive on "
        "y (ordinary least squares); CI from Student's t with n-2 dof"
    )


ProhaskaFit.__module__ = "digitalmodel.naval_architecture.friction_scaling"


def prohaska_form_factor(
    fn: Sequence[float], ct: Sequence[float], cf: Sequence[float]
) -> ProhaskaFit:
    """Least-squares Prohaska fit C_T/C_F = (1+k) + c Fn^4/C_F.

    Admissible data: every point within Fn 0.10-0.20 (points outside are refused, not
    dropped), at least 6 points. Refuses a design matrix whose 2-norm condition number
    exceeds 1e6.

    Error model: the residuals of y = C_T/C_F are independent, homoscedastic and additive
    (ordinary least squares). The parameter covariance is s^2 (R^T R)^-1 from a QR
    factorisation of the design matrix (normal equations are not formed); the 95 %
    confidence interval on k uses Student's t with n-2 dof. Multiplicative (relative) noise
    on C_T is only approximately covered by this model.
    """
    from scipy import stats
    from scipy.linalg import solve_triangular

    fn_a, ct_a, cf_a = _validated_inputs(fn, ct, cf)
    n = fn_a.size
    y = ct_a / cf_a
    x = fn_a**4 / cf_a
    a_mat = np.column_stack([np.ones(n), x])
    cond = float(np.linalg.cond(a_mat))
    if not np.isfinite(cond) or cond > PROHASKA_MAX_CONDITION:
        raise ValueError(
            f"Prohaska design matrix condition number {cond:.3g} exceeds {PROHASKA_MAX_CONDITION:g}"
        )
    q_mat, r_mat = np.linalg.qr(a_mat)
    coef = solve_triangular(r_mat, q_mat.T @ y)
    resid = y - a_mat @ coef
    dof = n - 2
    s2 = float(resid @ resid) / dof
    r_inv = solve_triangular(r_mat, np.eye(2))
    cov = s2 * (r_inv @ r_inv.T)
    se_a = math.sqrt(max(cov[0, 0], 0.0))
    t = float(stats.t.ppf(0.975, dof))
    k = float(coef[0] - 1.0)
    return ProhaskaFit(
        k=k,
        c=float(coef[1]),
        residual_rms=float(math.sqrt(float(resid @ resid) / n)),
        k_ci95=(k - t * se_a, k + t * se_a),
        condition_number=cond,
        n_points=int(n),
    )


def _validated_inputs(fn, ct, cf):
    fn_a = np.asarray(fn, dtype=float)
    ct_a = np.asarray(ct, dtype=float)
    cf_a = np.asarray(cf, dtype=float)
    if not (fn_a.shape == ct_a.shape == cf_a.shape) or fn_a.ndim != 1:
        raise ValueError("fn, ct and cf must be 1-D sequences of equal length")
    n = fn_a.size
    if n < PROHASKA_MIN_POINTS:
        raise ValueError(f"Prohaska fit needs at least {PROHASKA_MIN_POINTS} points, got {n}")
    if not (np.all(np.isfinite(fn_a)) and np.all(np.isfinite(ct_a)) and np.all(np.isfinite(cf_a))):
        raise ValueError("Prohaska inputs must be finite")
    if np.any(ct_a <= 0) or np.any(cf_a <= 0):
        raise ValueError("C_T and C_F must be positive")
    outside = (fn_a < PROHASKA_FN_MIN - 1e-12) | (fn_a > PROHASKA_FN_MAX + 1e-12)
    if np.any(outside):
        raise ValueError(
            f"Prohaska admissible window is Fn {PROHASKA_FN_MIN:.2f}-{PROHASKA_FN_MAX:.2f}; "
            f"points outside it: {fn_a[outside].tolist()}"
        )
    return fn_a, ct_a, cf_a
