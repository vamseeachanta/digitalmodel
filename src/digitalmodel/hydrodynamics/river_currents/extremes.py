# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Probability-plot fits and return levels for river speed/discharge series.

References
----------
NIST/SEMATECH e-Handbook, Weibull Distribution, section 1.3.6.6.8:
https://www.itl.nist.gov/div898/handbook/eda/section3/eda3668.htm
and Extreme Value Type I Distribution, section 1.3.6.6.16:
https://www.itl.nist.gov/div898/handbook/eda/section3/eda366g.htm
(both opened 2026-10-14). The CDFs and inverse CDFs define the transforms.

Fits use ordinary least squares on transformed ranks at (rank-0.5)/n, an
explicit plotting-position choice, not maximum likelihood. Weibull is a
two-parameter, zero-location fit. Its annual return level requires a stated
number of independent samples per year: F(x)**n = 1-1/T. Gumbel fits are for
one maximum per year. No declustering, uncertainty interval, nonstationary
trend, negative-flow magnitude conversion or automatic bank correction is
performed. Series units and bank location must be consistent before fitting.
"""

import math
from collections.abc import Sequence
from dataclasses import dataclass

from .bank_effects import _positive


def _series(values: Sequence[float], *, strictly_positive: bool = False) -> list[float]:
    data = [float(x) for x in values]
    if len(data) < 3 or any(not math.isfinite(x) or x < 0 for x in data):
        raise ValueError("at least three finite nonnegative observations are required")
    if strictly_positive and any(x == 0 for x in data):
        raise ValueError("Weibull observations must be strictly positive")
    if min(data) == max(data):
        raise ValueError("observations must not be constant")
    return sorted(data)


def _regression(x: list[float], y: list[float]) -> tuple[float, float]:
    mx, my = math.fsum(x) / len(x), math.fsum(y) / len(y)
    variance = math.fsum((v - mx) ** 2 for v in x)
    slope = math.fsum((v - mx) * (w - my) for v, w in zip(x, y)) / variance
    _positive(slope=slope)
    return slope, my - slope * mx


def _annual_log_probability(period: float, rate: float = 1) -> float:
    _positive(period=period, rate=rate)
    if period <= 1:
        raise ValueError("return period must exceed one year")
    return math.log1p(-1 / period) / rate


@dataclass(frozen=True)
class WeibullFit:
    """Two-parameter Weibull with dimensional scale and dimensionless shape."""

    shape: float
    scale: float

    def __post_init__(self) -> None:
        """Reject invalid fitted parameters."""
        _positive(shape=self.shape, scale=self.scale)

    def return_level(self, period: float, samples_per_year: float = 1) -> float:
        """Return a level exceeded by an annual maximum with probability 1/T."""
        log_p = _annual_log_probability(period, samples_per_year)
        level: float = self.scale * (-math.log(-math.expm1(log_p))) ** (1 / self.shape)
        return level


@dataclass(frozen=True)
class GumbelFit:
    """Maximum-type Gumbel fit; location and scale have the input units."""

    location: float
    scale: float

    def __post_init__(self) -> None:
        """Reject invalid fitted parameters."""
        _positive(scale=self.scale)
        if not math.isfinite(self.location):
            raise ValueError("location must be finite")

    def return_level(self, period: float) -> float:
        """Return an annual-maxima quantile; the fitted support is unbounded."""
        return self.location - self.scale * math.log(-_annual_log_probability(period))


def fit_weibull(values: Sequence[float]) -> WeibullFit:
    """Fit positive speeds or discharges by a linearised Weibull probability plot."""
    data = _series(values, strictly_positive=True)
    reduced = [math.log(-math.log1p(-(i + 0.5) / len(data))) for i in range(len(data))]
    shape, intercept = _regression([math.log(x) for x in data], reduced)
    return WeibullFit(shape, math.exp(-intercept / shape))


def fit_gumbel(annual_maxima: Sequence[float]) -> GumbelFit:
    """Fit one nonnegative maximum per year by a Gumbel probability plot."""
    data = _series(annual_maxima)
    reduced = [-math.log(-math.log((i + 0.5) / len(data))) for i in range(len(data))]
    scale, location = _regression(reduced, data)
    return GumbelFit(location, scale)


def seasonal_summary(values: Sequence[float], seasons: Sequence[str]) -> dict:
    """Return counts, means and maxima by supplied season, without annualising them."""
    if len(values) != len(seasons) or not values:
        raise ValueError("values and seasons must have matching nonzero lengths")
    groups: dict[str, list[float]] = {}
    for value, season in zip(values, seasons):
        if not math.isfinite(value) or value < 0 or not isinstance(season, str) or not season:
            raise ValueError("finite nonnegative values and nonempty season labels required")
        groups.setdefault(season, []).append(float(value))
    return {season: {'count': len(group), 'mean': math.fsum(group) / len(group),
                     'maximum': max(group)} for season, group in sorted(groups.items())}
