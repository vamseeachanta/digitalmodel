# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Annual-maxima GEV fits and conditional Hs/Tp IFORM screening contours.

References (all opened 2026-11-13)
----------------------------------
S1: Gilleland and Katz (2016), extRemes 2.0, JSS 72(8), section 2,
pp. 3-4, equations (2)-(4): GEV CDF, Gumbel limit and return levels.
https://doi.org/10.18637/jss.v072.i08
Paper accessed directly at https://www.jstatsoft.org/article/download/v072i08/1040
S2: SciPy 1.15.3, genextreme Notes: density and opposite shape-sign convention.
https://docs.scipy.org/doc/scipy-1.15.3/reference/generated/scipy.stats.genextreme.html
S3: Haselsteiner et al. (2021), A benchmarking exercise for environmental
contours, preprint 19 January: Table 1 (IFORM radius Phi^-1(1-alpha));
section 2.1 (conditional-model Rosenblatt mapping from normal space).
https://www.lancaster.ac.uk/~jonathan/CntCmp21.pdf
S4: Haselsteiner et al. (2019), OMAE2019-96523, Baseline results: models &
contours, Methods (p. 6), and Table 2 header (p. 8): Weibull marginal and
conditional lognormal with mu(h)=a0+a1*h**a2, sigma(h)=b0+b1*exp(b2*h).
https://ahaselsteiner.github.io/assets/pdf/OMAE2019-96523_ABechmarkingExerciseOnEstimatingExtremeEnvironmentalConditions.pdf
S4 models zero-upcrossing period; this API uses the same form for peak period
with caller-supplied coefficients fitted to that period. No coefficients from
the paper are embedded. Winterstein et al. (1993), ICOSSAR-93, originated IFORM.

GEV shape xi is -scipy's c: positive xi has a heavy upper tail. Annual maxima
must represent independent, stationary, comparable annual blocks. Small-sample
MLEs can be unstable, especially below 15 maxima. The KS statistic is descriptive
against the fitted CDF, not a calibrated significance test. IFORM's alpha is a
per-sea-state marginal exceedance probability, not total probability outside
the contour. These calculations do not qualify a site's design conditions.
"""

import math
from collections.abc import Sequence
from dataclasses import dataclass

import numpy as np
from scipy.stats import genextreme, kstest, norm


def _finite_array(
    values: object, name: str, shape: tuple[int, ...] | None = None,
) -> np.ndarray:
    """Convert numeric inputs and reject non-finite or malformed arrays."""
    try:
        result: np.ndarray = np.asarray(values, dtype=float)
    except (TypeError, ValueError) as exc:
        raise ValueError(f"{name} must contain finite numbers") from exc
    if not np.isfinite(result).all() or (shape is not None and result.shape != shape):
        raise ValueError(f"{name} must contain finite numbers with shape {shape}")
    return result


def _scalar(value: float, name: str, *, positive: bool = False) -> float:
    result = float(_finite_array(value, name, shape=()))
    if positive and result <= 0:
        raise ValueError(f"{name} must be finite and positive")
    return result


def _period(value: float) -> float:
    period = _scalar(value, "return period")
    if period <= 1:
        raise ValueError("return period must be finite and greater than one year")
    return period


@dataclass(frozen=True)
class GEVFit:
    """GEV parameters in input units, sample count and fitted-CDF KS statistic."""

    loc: float
    scale: float
    xi: float
    n: int
    ks_statistic: float

    def __post_init__(self) -> None:
        _scalar(self.loc, "location")
        _scalar(self.scale, "scale", positive=True)
        _scalar(self.xi, "xi")
        n = _scalar(self.n, "sample size")
        if n < 3 or not n.is_integer():
            raise ValueError("sample size must be an integer of at least three")
        ks = _scalar(self.ks_statistic, "KS statistic")
        if not 0 <= ks <= 1:
            raise ValueError("KS statistic must be between zero and one")


def fit_gev(annual_maxima: Sequence[float] | np.ndarray) -> GEVFit:
    """Fit GEV by maximum likelihood to at least three annual maxima (S1/S2)."""
    data = _finite_array(annual_maxima, "annual maxima")
    if data.ndim != 1 or data.size < 3:
        raise ValueError("at least three finite annual maxima are required")
    if np.all(data == data[0]):
        raise ValueError("annual maxima must have nonzero variation")
    c, loc, scale = genextreme.fit(data)
    _scalar(loc, "fitted location")
    _scalar(scale, "fitted scale", positive=True)
    _scalar(c, "fitted shape")
    ks = kstest(data, "genextreme", args=(c, loc, scale)).statistic
    return GEVFit(float(loc), float(scale), float(-c), int(data.size), float(ks))


def gev_return_level(fit: GEVFit, return_period_years: float) -> float:
    """Annual quantile with exceedance probability 1/T; exact Gumbel limit (S1)."""
    period = _period(return_period_years)
    log_y = math.log(-math.log1p(-1 / period))
    if fit.xi == 0:
        return fit.loc - fit.scale * log_y
    try:
        level = fit.loc + fit.scale * math.expm1(-fit.xi * log_y) / fit.xi
    except OverflowError as exc:
        raise ValueError("return level must be finite") from exc
    return _scalar(level, "return level")


def iform_beta(return_period_years: float, sea_state_hours: float = 3.0) -> float:
    """Normal-space radius for T years of independent sea states (S3 Table 1)."""
    period = _period(return_period_years)
    hours = _scalar(sea_state_hours, "sea state hours", positive=True)
    alpha = (hours / (365.25 * 24)) / period
    if not 0 < alpha <= 0.5:
        raise ValueError("sea state duration must imply at least two sea states")
    return float(norm.isf(alpha))


def iform_contour_hs_tp(
    beta: float,
    hs_weibull: tuple[float, float, float],
    tp_lognormal: tuple[tuple[float, float, float], tuple[float, float, float]],
    n_points: int = 72,
) -> tuple[np.ndarray, np.ndarray]:
    """Map a normal circle through the conditional Rosenblatt model (S3/S4).

    Hs parameters are (shape, scale, location). Tp coefficients are
    ((a0, a1, a2), (b0, b1, b2)) for the mean and standard deviation of ln(Tp).
    Hs and Tp retain the caller's units (normally metres and seconds).
    Returns n_points + 1 points, starting at angle zero, with exact closure.
    """
    radius = _scalar(beta, "beta")
    if radius < 0:
        raise ValueError("beta must be finite and nonnegative")
    count = _scalar(n_points, "n_points")
    if count < 4 or not count.is_integer():
        raise ValueError("n_points must be an integer of at least four")
    shape, scale, location = _finite_array(hs_weibull, "Hs Weibull", (3,))
    if shape <= 0 or scale <= 0:
        raise ValueError("Weibull shape and scale must be finite and positive")
    a, b = _finite_array(tp_lognormal, "Tp lognormal coefficients", (2, 3))
    theta = np.linspace(0, 2 * np.pi, int(count) + 1)
    u1, u2 = radius * np.cos(theta), radius * np.sin(theta)
    # logsf evaluates log(Phi(-u1)) without upper-tail cancellation/underflow.
    with np.errstate(over="ignore", invalid="ignore", divide="ignore"):
        hs = location + scale * (-norm.logsf(u1)) ** (1 / shape)
        mu = a[0] + a[1] * hs ** a[2]
        sigma = b[0] + b[1] * np.exp(b[2] * hs)
    _finite_array(hs, "contour Hs")
    _finite_array(mu, "conditional log-period mean")
    if not np.isfinite(sigma).all() or np.any(sigma <= 0):
        raise ValueError("conditional log-period standard deviation must be finite and positive")
    with np.errstate(over="ignore", invalid="ignore", under="ignore"):
        tp = np.exp(mu + sigma * u2)
    if not np.isfinite(tp).all() or np.any(tp <= 0):
        raise ValueError("contour Tp must be finite and positive")
    hs[-1], tp[-1] = hs[0], tp[0]
    return hs, tp
