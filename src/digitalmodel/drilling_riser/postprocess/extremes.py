"""Extreme statistics over seed maxima (Gumbel, maximum likelihood).

Irregular-sea extremes are taken from N independent seeds of a full-duration sea state. The maximum (or minimum)
of each seed is one sample of the extreme-value distribution of the duration; a Gumbel (type I) distribution is
fitted to the N samples by maximum likelihood.

* ``mpm`` - most probable maximum of the duration = the fitted location (the mode of the Gumbel density).
* ``ci_low``/``ci_high`` - two-sided confidence interval of the location from the asymptotic normal distribution
  of the maximum-likelihood estimate: Var(loc) = scale^2 / n * (1 + 6 / pi^2 * (1 - gamma)^2), gamma = Euler's
  constant (about 1.1087 scale^2 / n).
* ``p90`` - the 90 % non-exceedance quantile of the fitted distribution (information).
* Fit diagnostics: ``ppcc`` - probability-plot correlation coefficient with Gringorten plotting positions;
  ``loo_max_rel_change`` - the largest relative change of the MPM when any one seed is dropped (seed convergence).

Minima are fitted as maxima of the negated values and mapped back.
"""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass
from statistics import NormalDist
from typing import Iterable

EULER_GAMMA = 0.5772156649015329
_VAR_LOC = 1.0 + 6.0 / math.pi**2 * (1.0 - EULER_GAMMA) ** 2


@dataclass(frozen=True)
class GumbelFit:
    kind: str
    n: int
    loc: float
    scale: float
    mpm: float
    ci_low: float
    ci_high: float
    confidence: float
    p90: float
    ppcc: float
    loo_max_rel_change: float
    observed_extreme: float
    method: str = "maximum likelihood (Gumbel, seed extremes)"

    def as_dict(self) -> dict:
        return asdict(self)


def gumbel_quantile(loc: float, scale: float, p: float) -> float:
    """Quantile of a Gumbel maximum distribution at non-exceedance probability ``p``."""
    return loc - scale * math.log(-math.log(p))


def _mle(x: list[float]) -> tuple[float, float]:
    """Maximum-likelihood (loc, scale) of a Gumbel maximum. Scale solves
    scale = mean(x) - sum(x e^(-x/scale)) / sum(e^(-x/scale)); the left minus right side is increasing in scale."""
    n = len(x)
    mean = sum(x) / n
    sd = math.sqrt(sum((v - mean) ** 2 for v in x) / (n - 1))
    if sd == 0.0:
        return x[0], 0.0
    xmin = min(x)  # weights e^(-(x - xmin)/b) <= 1: no overflow

    def g(b: float) -> float:
        w = [math.exp(-(v - xmin) / b) for v in x]
        return b - mean + sum(v * wi for v, wi in zip(x, w)) / sum(w)

    lo, hi = sd * 1e-3, sd * 10.0
    while g(lo) > 0:
        lo /= 10.0
    while g(hi) < 0:
        hi *= 10.0
    for _ in range(200):
        mid = 0.5 * (lo + hi)
        if g(mid) > 0:
            hi = mid
        else:
            lo = mid
        if hi - lo <= 1e-14 * hi:
            break
    b = 0.5 * (lo + hi)
    loc = xmin - b * math.log(sum(math.exp(-(v - xmin) / b) for v in x) / n)
    return loc, b


def _ppcc(x: list[float], loc: float, scale: float) -> float:
    n = len(x)
    xs = sorted(x)
    y = [-math.log(-math.log((i - 0.44) / (n + 0.12))) for i in range(1, n + 1)]
    mx, my = sum(xs) / n, sum(y) / n
    sxy = sum((a - mx) * (b - my) for a, b in zip(xs, y))
    sxx = sum((a - mx) ** 2 for a in xs)
    syy = sum((b - my) ** 2 for b in y)
    return 1.0 if sxx == 0.0 else sxy / math.sqrt(sxx * syy)


def gumbel_fit(values: Iterable[float], *, kind: str = "max", confidence: float = 0.90) -> GumbelFit:
    """Gumbel fit over seed extremes. ``kind`` = ``max`` (seed maxima) or ``min`` (seed minima)."""
    if kind not in ("max", "min"):
        raise ValueError(f"kind must be 'max' or 'min', not {kind!r}")
    raw = [float(v) for v in values]
    if len(raw) < 2:
        raise ValueError("a Gumbel fit needs at least 2 seed extremes")
    if not all(math.isfinite(v) for v in raw):
        raise ValueError("seed extremes must be finite")
    sign = 1.0 if kind == "max" else -1.0
    x = [sign * v for v in raw]
    n = len(x)
    loc, scale = _mle(x)
    z = NormalDist().inv_cdf(0.5 + confidence / 2.0)
    half = z * scale * math.sqrt(_VAR_LOC / n)
    lo, hi = loc - half, loc + half
    p90 = gumbel_quantile(loc, scale, 0.9)
    loo = 0.0
    if 2 < n <= 100 and scale > 0.0:  # seed counts; not computed for long samples
        for i in range(n):
            li, _ = _mle(x[:i] + x[i + 1:])
            loo = max(loo, abs(li - loc) / abs(loc) if loc else abs(li - loc))
    ppcc = _ppcc(x, loc, scale)
    if sign < 0:
        loc, lo, hi, p90 = -loc, -hi, -lo, -p90
    return GumbelFit(kind=kind, n=n, loc=loc, scale=scale, mpm=loc, ci_low=lo, ci_high=hi, confidence=confidence,
                     p90=p90, ppcc=ppcc, loo_max_rel_change=loo,
                     observed_extreme=max(raw) if kind == "max" else min(raw))
