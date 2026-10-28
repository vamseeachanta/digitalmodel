# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Three-parameter Weibull statistics for supplied independent response peaks.

No solver or time-series peak extraction is performed. Notation is shape k,
scale lambda and location gamma; response, scale and location share units.
Source W: NIST/SEMATECH e-Handbook, online edition 2026, 1.3.6.6.8,
Weibull Distribution:
https://www.itl.nist.gov/div898/handbook/eda/section3/eda3668.htm
The general density, standard CDF, inverse and gamma moments on that page
give every distribution equation below by X = gamma + lambda*Z.
"""

from __future__ import annotations

from dataclasses import dataclass
import math
from typing import TYPE_CHECKING, Callable

import numpy as np
from numpy.typing import ArrayLike, NDArray
from scipy.optimize import brentq, minimize_scalar
from scipy.special import gammaln, logsumexp

if TYPE_CHECKING:
    from .motion_statistics import ScatterCell

FloatArray = NDArray[np.float64]
Response = float | FloatArray
Bounds = tuple[float, float]


def _scalar_or_array(value: ArrayLike) -> Response:
    array = np.asarray(value, dtype=float)
    return float(array) if array.ndim == 0 else array


@dataclass(frozen=True)
class WeibullDistribution:
    """Shifted Weibull law, supported on x >= location (Source W)."""

    shape: float
    scale: float
    location: float = 0.0

    def __post_init__(self) -> None:
        if not np.all(np.isfinite([self.shape, self.scale, self.location])):
            raise ValueError("Parameters must be finite")
        if self.shape <= 0 or self.scale <= 0:
            raise ValueError("Shape and scale must be positive")

    def _log_standardize(self, x: ArrayLike) -> tuple[FloatArray, FloatArray]:
        array = np.asarray(x, dtype=float)
        if np.any(np.isnan(array)):
            raise ValueError("Response must not contain NaN")
        with np.errstate(over="ignore", divide="ignore", invalid="ignore"):
            difference = array - self.location
            # Source W: log z=log(x-gamma)-log(lambda). Preserve support
            # before scaling; even a finite distance/scale can underflow.
            log_distance = np.where(
                np.isposinf(difference) & np.isfinite(array),
                np.logaddexp(np.log(np.abs(array)), np.log(abs(self.location))),
                np.log(difference),
            )
            return difference, log_distance - math.log(self.scale)

    def pdf(self, x: ArrayLike) -> Response:
        """Density, including the right-hand limit at location (Source W)."""
        difference, log_z = self._log_standardize(x)
        with np.errstate(over="ignore", divide="ignore", invalid="ignore"):
            log_hazard = self.shape * log_z
            log_density = (
                math.log(self.shape)
                - math.log(self.scale)
                + (self.shape - 1) * log_z
                - np.exp(log_hazard)
            )
            result = np.where(difference > 0, np.exp(log_density), 0.0)
        # At k=1 the removable 0*log(0) term has limit zero.
        boundary = (
            math.inf if self.shape < 1 else 1 / self.scale if self.shape == 1 else 0.0
        )
        result = np.where(difference == 0, boundary, result)
        return _scalar_or_array(np.where(np.isposinf(log_hazard), 0.0, result))

    def logcdf(self, x: ArrayLike) -> Response:
        """Log CDF without subtracting two nearly equal numbers (Source W)."""
        difference, log_z = self._log_standardize(x)
        with np.errstate(over="ignore", divide="ignore", invalid="ignore"):
            log_hazard = self.shape * log_z
            # Source W plus 1-exp(-t)=t+O(t^2). Below machine epsilon,
            # log F=log t with an absolute correction smaller than eps/2;
            # retain log t when t itself underflows.
            log_probability = np.where(
                log_hazard < math.log(np.finfo(float).eps),
                log_hazard,
                np.log(-np.expm1(-np.exp(log_hazard))),
            )
            result = np.where(difference > 0, log_probability, -np.inf)
        return _scalar_or_array(result)

    def logsf(self, x: ArrayLike) -> Response:
        """Log survival, -((x-gamma)/lambda)^k on the support (Source W)."""
        difference, log_z = self._log_standardize(x)
        with np.errstate(over="ignore", invalid="ignore"):
            result = np.where(difference > 0, -np.exp(self.shape * log_z), 0.0)
        return _scalar_or_array(result)

    def cdf(self, x: ArrayLike) -> Response:
        """Cumulative probability; zero below the support (Source W)."""
        return _scalar_or_array(-np.expm1(np.asarray(self.logsf(x))))

    def _shifted_exponential(self, log_power: ArrayLike) -> Response:
        """Combine Source W's positive quantile term and signed location."""
        power = np.asarray(log_power, dtype=float)
        with np.errstate(over="ignore", invalid="ignore"):
            multiplier = np.exp(power)
            reference = max(self.scale, abs(self.location))
            ratio = self.scale / reference
            # Source W, divided by a common finite reference before adding.
            # Subnormal factors/products lose precision before rescaling;
            # use the logarithmic fallback unless all are normal floats.
            positive = ratio * multiplier
            direct = reference * (positive + self.location / reference)
            tiny = np.finfo(float).tiny
            representable = (
                np.isfinite(multiplier)
                & (multiplier >= tiny)
                & (ratio >= tiny)
                & (positive >= tiny)
            )
            if np.all(representable):
                return _scalar_or_array(direct)
            term = math.log(self.scale) + power
            if self.location == 0:
                result = np.exp(term)
            else:
                logs = np.stack(
                    (term, np.full_like(term, math.log(abs(self.location))))
                )
                weights = np.array([1, np.sign(self.location)]).reshape(
                    (2,) + (1,) * term.ndim
                )
                log_result, sign = logsumexp(logs, b=weights, axis=0, return_sign=True)
                result = sign * np.exp(log_result)
            result = np.where(representable, direct, result)
        return _scalar_or_array(np.where(np.isneginf(power), self.location, result))

    def quantile(self, probability: ArrayLike) -> Response:
        """Inverse CDF with Q(0)=location and Q(1)=infinity (Source W)."""
        p = np.asarray(probability, dtype=float)
        if np.any(~np.isfinite(p)) or np.any((p < 0) | (p > 1)):
            raise ValueError("Cumulative probability must be finite in [0, 1]")
        with np.errstate(divide="ignore", over="ignore"):
            return self._shifted_exponential(np.log(-np.log1p(-p)) / self.shape)

    def return_level(self, cycles: float) -> float:
        """Q(1-1/N)=gamma+lambda*(ln N)^(1/k), N >= 1 (Source W).

        This is a one-expected-exceedance threshold over N independent cycles,
        not the expected maximum or a specified quantile of the maximum.
        Using ln(N) avoids cancellation when 1-1/N rounds to one.
        """
        if not np.isfinite(cycles) or cycles < 1:
            raise ValueError("Cycles must be finite and at least one")
        if cycles == 1:
            return self.location
        with np.errstate(over="ignore"):
            return float(
                self._shifted_exponential(math.log(math.log(cycles)) / self.shape)
            )

    def moment(self, order: int) -> float:
        """Raw integer moment via binomial expansion (Source W).

        Substitution t=z^k gives E[Z^j]=Gamma(1+j/k). Thus
        E[X^r]=sum_j binom(r,j)*gamma^(r-j)*lambda^j*Gamma(1+j/k).
        Signed terms are combined in log space; OverflowError is raised
        if the final moment exceeds float range.
        """
        if isinstance(order, bool) or not isinstance(order, int) or order < 0:
            raise ValueError("Moment order must be a nonnegative integer")
        logs, signs = [], []
        for j in range(order + 1):
            power = order - j
            if self.location == 0 and power:
                continue
            log_location = power * math.log(abs(self.location)) if power else 0.0
            logs.append(
                math.log(math.comb(order, j))
                + log_location
                + j * math.log(self.scale)
                + gammaln(1 + j / self.shape)
            )
            signs.append(-1 if self.location < 0 and power % 2 else 1)
        largest = max(logs)
        # Sum bounded, signed terms accurately even when the largest
        # positive and negative terms cancel exactly (Source W expansion).
        signed_sum = math.fsum(
            sign * math.exp(value - largest) for value, sign in zip(logs, signs)
        )
        if signed_sum == 0:
            return 0.0
        return math.copysign(math.exp(largest + math.log(abs(signed_sum))), signed_sum)

    @property
    def mean(self) -> float:
        """Mean response (Source W)."""
        return self.moment(1)

    @property
    def variance(self) -> float:
        """Variance independent of location (Source W, standard deviation)."""
        a = gammaln(1 + 1 / self.shape)
        b = gammaln(1 + 2 / self.shape)
        # Source W: lambda^2 * exp(b) * (1-exp(2*a-b)). No factor
        # is exponentiated until its scale has been included.
        with np.errstate(over="ignore", divide="ignore"):
            log_variance = 2 * math.log(self.scale) + b + np.log(-np.expm1(2 * a - b))
            return float(np.exp(log_variance))


@dataclass(frozen=True)
class WeibullFit:
    """Fit and qualifications; objective units depend on the actual method."""

    distribution: WeibullDistribution
    requested_method: str
    method: str
    objective: float
    notes: tuple[str, ...] = ()


def _bounds(bounds: Bounds, name: str, positive: bool = False) -> Bounds:
    values = np.asarray(bounds, dtype=float)
    if (
        values.shape != (2,)
        or not np.all(np.isfinite(values))
        or values[0] >= values[1]
        or (positive and values[0] <= 0)
    ):
        raise ValueError(f"Require finite ordered {name} bounds")
    return float(values[0]), float(values[1])


def _samples(samples: ArrayLike) -> FloatArray:
    data = np.asarray(samples, dtype=float)
    # Three distinct observations are necessary to identify three parameters;
    # this is an algebraic floor, not a statistical adequacy criterion.
    if (
        data.ndim != 1
        or len(data) < 3
        or not np.all(np.isfinite(data))
        or len(np.unique(data)) < 3
    ):
        raise ValueError("Require a finite vector with at least three distinct samples")
    return np.sort(data)


def _standard_moments(shape: float) -> tuple[float, float, float]:
    """Standard mean, variance, skew from E[Z^r]=Gamma(1+r/k), Source W."""
    a = gammaln(1 + 1 / shape)
    b = np.expm1(gammaln(1 + 2 / shape) - 2 * a)
    c = np.expm1(gammaln(1 + 3 / shape) - 3 * a)
    return float(np.exp(a)), float(np.exp(2 * a) * b), float((c - 3 * b) / b**1.5)


def distribution_from_moments(
    mean: float, variance: float, skewness: float, *, shape_bounds: Bounds
) -> WeibullDistribution:
    """Match three population moments to a shifted Weibull (Source W).

    Skewness=(G3-3G1*G2+2G1^3)/(G2-G1^2)^(3/2), Gj=Gamma(1+j/k).
    Solve this scalar equation; lambda=sqrt(v/(G2-G1^2)), gamma=m-lambda*G1.
    Shape bounds are caller numerical inputs, not empirical validity limits.
    Raises ValueError if the moments cannot be matched within that bracket.
    """
    lo, hi = _bounds(shape_bounds, "shape", positive=True)
    if not np.all(np.isfinite([mean, variance, skewness])) or variance <= 0:
        raise ValueError("Require finite mean, positive variance and finite skewness")

    def residual(k: float) -> float:
        with np.errstate(over="ignore", invalid="ignore", divide="ignore"):
            return _standard_moments(k)[2] - skewness

    ends = np.array([residual(lo), residual(hi)])
    if not np.all(np.isfinite(ends)) or ends[0] * ends[1] > 0:
        raise ValueError("Sample skewness cannot be matched within shape bounds")
    shape = brentq(residual, lo, hi)
    unit_mean, unit_variance, _ = _standard_moments(shape)
    scale = math.sqrt(variance / unit_variance)
    return WeibullDistribution(shape, scale, mean - scale * unit_mean)


def _profile_minimum(function: Callable[[float], float], bounds: Bounds) -> Bounds:
    """Check both endpoints as well as the bounded scalar optimizer result.

    Optimizer tolerances are SciPy defaults; they are numerical stopping rules,
    not fitted-response acceptance criteria. No global optimality is claimed.
    """
    result = minimize_scalar(function, bounds=bounds, method="bounded")
    if not result.success or not np.isfinite(result.fun):
        raise ValueError("Profile optimization did not converge")
    candidates = [(result.x, result.fun)] + [(g, function(g)) for g in bounds]
    point, value = min(candidates, key=lambda item: item[1])
    return float(point), float(value)


def fit_weibull(
    samples: ArrayLike, *, method: str, location_bounds: Bounds, shape_bounds: Bounds
) -> WeibullFit:
    """Fit uncensored independent peaks by MLE, moments or probability plot.

    Both finite search brackets are required inputs. The entire location
    bracket must lie strictly below min(samples), with a caller-chosen gap.
    Units are inherited from samples. Normalization makes the optimization
    invariant to response units. Methods:

    * ``mle``: profile gamma; for each gamma maximize the log of Source W
      density. lambda^k=mean((x-gamma)^k). The shape score is
      1/k+mean(log(x-gamma))-weighted_mean(log(x-gamma))=0, where weights
      are proportional to (x-gamma)^k. Constrain k to the supplied bracket.
      If the solution has k<=1, return a qualified probability-plot fallback:
      for k<1 the likelihood diverges as gamma approaches the sample minimum.
      This non-regularity follows from (k-1)*log(x_min-gamma) in log density.
    * ``moments``: population-style sample moments (divisor n) and the
      gamma-moment equations of ``distribution_from_moments``. A solution
      outside the location bracket is rejected, not clipped.
    * ``probability_plot``: profile gamma, least squares of
      y=log[-log(1-p)] on log(x-gamma), p_i=(i-.3)/(n+.4). The plotting
      positions and transform are NIST e-Handbook 1.3.3.30 (Weibull Plot):
      https://www.itl.nist.gov/div898/handbook/eda/section3/weibplot.htm
      Profiling the shift is an explicit extension of that two-parameter plot.

    For 1<k<=2, the squared location score times the density near the support
    behaves as z^(k-3), whose integral diverges (differentiate Source W).
    Regular asymptotic inference is therefore not assumed. SciPy 1.14.1
    ``anderson`` Notes also document fitting difficulties and cite Table 0:
    https://docs.scipy.org/doc/scipy-1.14.1/reference/generated/scipy.stats.anderson.html
    Bootstrap intervals are therefore descriptive and require sensitivity checks.
    The profile is a numerical local solution; inspect bound warnings and tails.
    """
    data = _samples(samples)
    loc_bounds = _bounds(location_bounds, "location")
    k_lo, k_hi = _bounds(shape_bounds, "shape", positive=True)
    if loc_bounds[1] >= data[0]:
        raise ValueError(
            "Upper location bound must be strictly below the sample minimum"
        )
    if method not in {"mle", "moments", "probability_plot"}:
        raise ValueError("Unknown fitting method")
    origin = float(data[0])
    unit = float(np.std(data))
    if not np.isfinite(unit) or unit <= 0:
        raise ValueError("Samples cannot be normalized in floating arithmetic")
    x = (data - origin) / unit
    bounds = ((loc_bounds[0] - origin) / unit, (loc_bounds[1] - origin) / unit)
    notes: list[str] = []

    def plot_at(g: float) -> tuple[float, float, float]:
        # NIST 1.3.3.30, Definition: median-rank approximation coefficients.
        p = (np.arange(1, len(x) + 1) - 0.3) / (len(x) + 0.4)
        y = np.log(-np.log1p(-p))
        logs = np.log(x - g)
        centered = logs - logs.mean()
        k = np.clip(
            np.dot(centered, y - y.mean()) / np.dot(centered, centered), k_lo, k_hi
        )
        log_scale = logs.mean() - y.mean() / k
        error = y - k * (logs - log_scale)
        return float(np.mean(error**2)), float(k), float(log_scale)

    def mle_at(g: float) -> tuple[float, float, float]:
        logs = np.log(x - g)

        def score(k: float) -> float:
            weights = np.exp(k * logs - logsumexp(k * logs))
            return float(1 / k + logs.mean() - np.dot(weights, logs))

        if score(k_lo) <= 0:
            k = k_lo
        elif score(k_hi) >= 0:
            k = k_hi
        else:
            k = brentq(score, k_lo, k_hi)
        log_scale = (logsumexp(k * logs) - math.log(len(x))) / k
        # At profiled scale, mean(z^k)=1 exactly (Source W density).
        nll = log_scale - math.log(k) - (k - 1) * (logs.mean() - log_scale) + 1
        return float(nll), float(k), float(log_scale)

    if method == "moments":
        mean = float(x.mean())
        variance = float(np.mean((x - mean) ** 2))
        skew = float(np.mean((x - mean) ** 3) / variance**1.5)
        law = distribution_from_moments(mean, variance, skew, shape_bounds=shape_bounds)
        if not bounds[0] <= law.location <= bounds[1]:
            raise ValueError("Moment estimate location is outside the supplied bounds")
        g, k, log_scale = law.location, law.shape, math.log(law.scale)
        objective = 0.0  # Exact moment matching, up to the scalar root tolerance.
    else:
        profile = mle_at if method == "mle" else plot_at
        g, _ = _profile_minimum(lambda g: profile(g)[0], bounds)
        objective, k, log_scale = profile(g)
        if method == "mle" and k <= 1:
            fallback = fit_weibull(
                data,
                method="probability_plot",
                location_bounds=loc_bounds,
                shape_bounds=shape_bounds,
            )
            return WeibullFit(
                fallback.distribution,
                "mle",
                fallback.method,
                fallback.objective,
                ("MLE non-regular at k<=1; probability-plot fallback",)
                + fallback.notes,
            )

    # sqrt(eps) is a floating resolution flag, not a physical acceptance limit.
    tolerance = math.sqrt(np.finfo(float).eps)
    for name, value, limits in (("location", g, bounds), ("shape", k, (k_lo, k_hi))):
        if any(abs(value - end) <= tolerance * max(1, abs(end)) for end in limits):
            notes.append(f"{name} bound active; repeat with justified alternate bounds")
    if k <= 2:
        notes.append("k<=2: regular asymptotic inference is not assumed")
    law = WeibullDistribution(k, unit * math.exp(log_scale), origin + unit * g)
    if law.location >= data[0]:
        raise ValueError(
            "Fitted location is not below the sample minimum after rounding"
        )
    return WeibullFit(law, method, method, objective, tuple(notes))


@dataclass(frozen=True)
class CriticalValue:
    """Caller-supplied raw-statistic threshold for a fully specified law.

    A source (edition, table or worked example) and matching sample size are
    mandatory. The caller must verify the distribution and statistic scaling.
    Fitted-distribution thresholds are deliberately not supported here.
    """

    value: float
    significance: float
    sample_size: int
    source: str

    def __post_init__(self) -> None:
        if (
            not np.all(np.isfinite([self.value, self.significance]))
            or self.value <= 0
            or not 0 < self.significance < 1
            or isinstance(self.sample_size, bool)
            or not isinstance(self.sample_size, int)
            or self.sample_size < 1
            or not isinstance(self.source, str)
            or not self.source.strip()
        ):
            raise ValueError(
                "Require positive threshold, significance in (0,1), n and source"
            )


@dataclass(frozen=True)
class FitStatistic:
    """Unadjusted EDF statistic and explicitly qualified rejection decision."""

    statistic: float
    status: str
    critical_value: float | None
    significance: float | None
    source: str | None
    reason: str


@dataclass(frozen=True)
class GoodnessOfFit:
    """KS and raw Anderson-Darling diagnostics (no automatic fitted-law p-value)."""

    sample_size: int
    ks: FitStatistic
    ad: FitStatistic


def goodness_of_fit(
    samples: ArrayLike,
    model: WeibullDistribution | WeibullFit,
    *,
    ks_critical: CriticalValue | None = None,
    ad_critical: CriticalValue | None = None,
) -> GoodnessOfFit:
    """Compute raw two-sided KS and Anderson-Darling EDF statistics.

    Sources: NIST e-Handbook 1.3.5.16, Definition and Limitations:
    https://www.itl.nist.gov/div898/handbook/eda/section3/eda35g.htm
    D=max_i(F(x_i)-(i-1)/n, i/n-F(x_i)).
    NIST 1.3.5.14, Definition and Critical Region:
    https://www.itl.nist.gov/div898/handbook/eda/section3/eda35e.htm
    A^2=-n-sum_i[(2i-1)/n*(ln F(x_i)+ln S(x_(n+1-i)))].

    A WeibullFit always means parameters estimated from these data: both
    decisions are NOT_EVALUATED because tabulated known-parameter cutoffs
    are invalid. A WeibullDistribution means independently specified
    parameters; provide sourced raw-statistic cutoffs to evaluate a decision.
    No Weibull AD critical table is embedded: the sources above do not
    provide it, and bounded estimation changes the applicable calibration.
    A non-rejection does not establish tail extrapolation accuracy.
    """
    x = np.asarray(samples, dtype=float)
    if x.ndim != 1 or not len(x) or not np.all(np.isfinite(x)):
        raise ValueError("Require a nonempty finite response vector")
    x = np.sort(x)
    fitted = isinstance(model, WeibullFit)
    law = model.distribution if isinstance(model, WeibullFit) else model
    if not isinstance(law, WeibullDistribution):
        raise ValueError("Require a Weibull distribution or fit")
    n = len(x)
    i = np.arange(1, n + 1)
    p = law.cdf(x)
    ks = float(max(np.max(p - (i - 1) / n), np.max(i / n - p)))
    ad = float(-n - np.sum((2 * i - 1) / n * (law.logcdf(x) + law.logsf(x[::-1]))))

    def decide(statistic: float, critical: CriticalValue | None) -> FitStatistic:
        if critical is not None:
            if not isinstance(critical, CriticalValue):
                raise ValueError("Require a CriticalValue with source and sample size")
            if critical.sample_size != n:
                raise ValueError("Critical value sample size must match observations")
        if fitted:
            return FitStatistic(
                statistic,
                "NOT_EVALUATED",
                None,
                None,
                None,
                "Parameters estimated; fitted-law calibration unavailable",
            )
        if critical is None:
            return FitStatistic(
                statistic,
                "NOT_EVALUATED",
                None,
                None,
                None,
                "Applicable published critical value not supplied",
            )
        return FitStatistic(
            statistic,
            "REJECTED" if statistic > critical.value else "NOT_REJECTED",
            critical.value,
            critical.significance,
            critical.source,
            "Fully specified continuous distribution; raw statistic",
        )

    return GoodnessOfFit(n, decide(ks, ks_critical), decide(ad, ad_critical))


@dataclass(frozen=True)
class BootstrapInterval:
    """Nonparametric percentile interval with all failed refits accounted for."""

    estimate: float
    lower: float | None
    upper: float | None
    confidence: float
    status: str
    completed: int
    failed: int
    fallback_count: int
    return_levels: tuple[float, ...]
    reason: str


def bootstrap_return_level(
    samples: ArrayLike,
    *,
    cycles: float,
    confidence: float,
    n_resamples: int,
    seed: int,
    method: str,
    location_bounds: Bounds,
    shape_bounds: Bounds,
) -> BootstrapInterval:
    """Resample independent peaks, refit each sample, take percentile endpoints.

    Source: SciPy 1.14.1 ``bootstrap``, percentile method description:
    https://docs.scipy.org/doc/scipy-1.14.1/reference/generated/scipy.stats.bootstrap.html
    Endpoints are empirical quantiles at (1-confidence)/2 and
    (1+confidence)/2, using linear order-statistic interpolation. Confidence,
    replicate count and seed are required caller inputs, not design limits.
    The same finite bounds are used on every resample; therefore the location
    upper bound must already be below the original minimum. No failures are
    discarded to construct a conditional interval: any failure produces
    NOT_EVALUATED with counts and the successful return levels retained.
    Coverage is not established for non-regular fits or bound-active fits.
    Dependent, nonstationary, censored or mixed-state peaks require other methods.
    """
    if (
        not np.isfinite(confidence)
        or not 0 < confidence < 1
        or isinstance(n_resamples, bool)
        or not isinstance(n_resamples, int)
        or n_resamples < 2
    ):
        raise ValueError("Require confidence in (0,1) and at least two resamples")
    data = _samples(samples)
    fitted = fit_weibull(
        data, method=method, location_bounds=location_bounds, shape_bounds=shape_bounds
    )
    estimate = fitted.distribution.return_level(cycles)
    rng = np.random.default_rng(seed)
    levels: list[float] = []
    failed = fallback = 0
    for _ in range(n_resamples):
        resample = data[rng.integers(0, len(data), len(data))]
        try:
            fit = fit_weibull(
                resample,
                method=method,
                location_bounds=location_bounds,
                shape_bounds=shape_bounds,
            )
            level = fit.distribution.return_level(cycles)
            if not np.isfinite(level):
                raise ValueError("Nonfinite bootstrap return level")
        except (ValueError, OverflowError, FloatingPointError):
            failed += 1
        else:
            fallback += fit.method != method
            levels.append(level)
    if failed:
        return BootstrapInterval(
            estimate,
            None,
            None,
            confidence,
            "NOT_EVALUATED",
            len(levels),
            failed,
            fallback,
            tuple(levels),
            "One or more refits failed; interval not conditioned on success",
        )
    lower, upper = np.quantile(
        levels, [(1 - confidence) / 2, (1 + confidence) / 2], method="linear"
    )
    return BootstrapInterval(
        estimate,
        float(lower),
        float(upper),
        confidence,
        "EVALUATED",
        len(levels),
        0,
        fallback,
        tuple(levels),
        "Percentile interval; coverage and tail model not validated",
    )


@dataclass(frozen=True)
class ExtremeComparison:
    """One-expected-exceedance thresholds for the same elapsed time and cycles."""

    cycles: float
    weibull_return_level: float
    rayleigh_return_level: float
    ratio: float
    difference_percent: float


def compare_rayleigh_extreme(
    model: WeibullDistribution | WeibullFit,
    *,
    scatter: list[ScatterCell],
    rao_amplitude: ArrayLike,
    omega: ArrayLike,
    spectrum_func: Callable[[FloatArray, float, float], FloatArray],
    return_period_years: float,
) -> ExtremeComparison:
    """Compare fitted Q(1-1/N) with ``motion_statistics.long_term_extreme``.

    The unchanged Rayleigh routine supplies Tz_i and its expected-exceedance
    root. Use N=R_seconds*sum_i(p_i/Tz_i), where p_i are time fractions.
    Julian-year conversion is the existing motion_statistics contract; see
    orcawave/docs/engineering-citations.md, R2 (expected exceedance count).
    The Weibull samples must describe the same cycle-weighted peak population,
    not a time-weighted mixture or raw signed response ordinates.

    Location is tail-sensitive: at fixed k and lambda, dQ_N/dgamma=1; when
    refitted, all parameters covary and extrapolation can change more. The
    ratio is a comparison, not a tail-model validation or a maximum quantile.
    """
    from .motion_statistics import long_term_extreme

    law = model.distribution if isinstance(model, WeibullFit) else model
    if not isinstance(law, WeibullDistribution):
        raise ValueError("Require a Weibull distribution or fit")
    reference = long_term_extreme(
        scatter,
        np.asarray(rao_amplitude, dtype=float),
        np.asarray(omega, dtype=float),
        spectrum_func,
        return_period_years=return_period_years,
    )
    seconds = return_period_years * 365.25 * 24 * 3600  # Existing R2 time basis.
    cycles = seconds * sum(
        c["probability"] / c["zero_crossing_period_s"] for c in reference.contributions
    )
    level = law.return_level(cycles)
    ratio = level / reference.extreme_response
    return ExtremeComparison(
        float(cycles), level, reference.extreme_response, ratio, 100 * (ratio - 1)
    )
