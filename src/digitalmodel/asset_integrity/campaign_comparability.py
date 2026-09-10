"""Can two inspection campaigns be compared to each other at all?

Repeat inspection is usually treated as arithmetic: measure a wall, measure it
again later, subtract. That is only valid if both numbers are on the same
scale. When campaigns differ in crew, procedure, instrument or reporting
convention -- which, in practice, they nearly always do -- the difference
between them carries a campaign offset as well as any real metal loss, and the
two are not separable without help.

This module supplies the four calculations that decide whether a differencing
programme can work, before any effort is spent on the differencing itself:

    gauge_from_short_interval   what the measurement scatter actually is,
                                read off repeat readings taken too close
                                together for real degradation
    invariance_family           a demonstration that rate and offset are
                                confounded, and by exactly how much
    reference_set_size          how many stable articles pin the offset to a
                                stated tolerance
    minimum_detectable_rate     the smallest rate a campaign can prove, given
                                that the offset is estimated and not known

The central result is negative and worth stating plainly. For a reading

    y[i][j] = baseline[i] - rate * t[j] + offset[j] + error

the substitution ``rate -> rate + k``, ``offset[j] -> offset[j] + k * t[j]``
leaves every prediction unchanged for any k. Rate and offset are confounded
along a continuous line, so no amount of data at those same campaign dates
identifies the rate. N campaigns supply N-1 independent contrasts against N-1
offsets plus a rate: always one short. The deficiency is structural, and it is
broken only by information from outside -- an offset known to be zero, or
articles known to be stable.

That is why reference articles are not a refinement. They are the thing that
makes the measurement mean anything, and :func:`reference_set_size` prices
them: the count goes as the SQUARE of the measurement scatter, so halving the
scatter quarters the control set. Registering a measurement to a fixed datum,
rather than reporting an extremum found wherever the operator happened to
look, is usually the cheapest way to halve it.

Nothing here is specific to a material, a geometry or a defect mechanism. It
applies to any repeated measurement of the same articles by different parties.
"""

from __future__ import annotations

import math
import statistics as st
from dataclasses import dataclass, field
from typing import Iterable, Mapping, Sequence

__all__ = [
    "GaugeEstimate",
    "InvarianceRow",
    "gauge_from_short_interval",
    "invariance_family",
    "reference_set_size",
    "minimum_detectable_rate",
    "variance_budget",
]

# Two-sided 95% Student t quantiles. Control sets are small by nature -- the
# whole point is that they are cheap -- so the normal quantile understates
# every count derived from them, and the count goes as the square.
_T95: Mapping[int, float] = {
    1: 12.706, 2: 4.303, 3: 3.182, 4: 2.776, 5: 2.571, 6: 2.447, 7: 2.365,
    8: 2.306, 9: 2.262, 10: 2.228, 11: 2.201, 12: 2.179, 13: 2.160, 14: 2.145,
    15: 2.131, 16: 2.120, 18: 2.101, 20: 2.086, 25: 2.060, 30: 2.042,
    40: 2.021, 60: 2.000, 120: 1.980,
}

# Chi-square quantiles for a variance interval, small degrees of freedom.
_CHI2_LO: Mapping[int, float] = {
    1: 0.000982, 2: 0.0506, 3: 0.216, 4: 0.484, 5: 0.831, 6: 1.237, 7: 1.690,
    8: 2.180, 9: 2.700, 10: 3.247, 11: 3.816, 12: 4.404, 15: 6.262, 20: 9.591,
}
_CHI2_HI: Mapping[int, float] = {
    1: 5.024, 2: 7.378, 3: 9.348, 4: 11.143, 5: 12.833, 6: 14.449, 7: 16.013,
    8: 17.535, 9: 19.023, 10: 20.483, 11: 21.920, 12: 23.337, 15: 27.488,
    20: 34.170,
}


def t_quantile_95(df: int) -> float:
    """Two-sided 95% t quantile, rounded up to the next tabulated degree."""
    if df < 1:
        return float("inf")
    for k in sorted(_T95):
        if df <= k:
            return _T95[k]
    return 1.96


def _chi2(table: Mapping[int, float], df: int) -> float | None:
    for k in sorted(table):
        if df <= k:
            return table[k]
    return None


@dataclass(frozen=True)
class GaugeEstimate:
    """Measurement scatter read off repeat readings on the same articles.

    ``difference_sd`` is the scatter of campaign-to-campaign DIFFERENCES, which
    is what a differencing programme actually contends with.
    ``single_measurement_sd`` divides it by root two, which additionally
    assumes the two campaigns' errors are equal in size and independent of one
    another. Shared error -- a common calibration, a common operator habit --
    breaks that assumption and makes the single-measurement figure meaningless
    while leaving the difference figure intact. Prefer the difference figure.
    """

    n: int
    mean_difference: float
    mean_ci95: tuple[float, float]
    difference_sd: float
    difference_sd_ci95: tuple[float, float] | None
    single_measurement_sd: float
    offset_established: bool
    note: str = ""

    @property
    def mean_consistent_with_zero(self) -> bool:
        return self.mean_ci95[0] <= 0.0 <= self.mean_ci95[1]


def gauge_from_short_interval(differences: Sequence[float]) -> GaugeEstimate:
    """Estimate measurement scatter from repeats taken too close to degrade.

    Articles measured twice over an interval short enough that real change is
    negligible give the scatter directly. The mean of those differences
    estimates the campaign offset -- but note that finding a non-zero mean is
    not the same as establishing one, and with the handful of articles these
    studies usually rest on, the interval will often contain zero. In that case
    the result BOUNDS an offset rather than demonstrating it, and
    ``offset_established`` says so.

    A short interval makes metal loss negligible. It does not by itself make
    the articles stable: damage, repair or handling between the two looks will
    appear here as scatter. Stability is an assumption to be justified
    separately, never a conclusion of this function.

    :param differences: first reading minus second, one per article, in
        whatever length unit the caller is working in.
    :raises ValueError: fewer than two articles.
    """
    n = len(differences)
    if n < 2:
        raise ValueError("need at least two articles to estimate scatter")
    mean = st.mean(differences)
    sd = st.stdev(differences)
    df = n - 1
    half = t_quantile_95(df) * sd / math.sqrt(n)
    ci = (mean - half, mean + half)

    lo, hi = _chi2(_CHI2_HI, df), _chi2(_CHI2_LO, df)
    sd_ci = ((sd * math.sqrt(df / lo), sd * math.sqrt(df / hi))
             if lo and hi else None)

    established = not (ci[0] <= 0.0 <= ci[1])
    return GaugeEstimate(
        n=n,
        mean_difference=mean,
        mean_ci95=ci,
        difference_sd=sd,
        difference_sd_ci95=sd_ci,
        single_measurement_sd=sd / math.sqrt(2.0),
        offset_established=established,
        note=("mean differs from zero" if established else
              "mean is consistent with zero: this bounds an offset, it does "
              "not establish one"),
    )


@dataclass(frozen=True)
class InvarianceRow:
    """One point on the confounded line of rate-and-offset solutions."""

    assumed_rate: float
    required_offsets: tuple[float, ...]
    fits_data_exactly: bool = True


def invariance_family(
    contrasts: Mapping[int, float],
    elapsed: Mapping[int, float],
    rates: Iterable[float],
) -> list[InvarianceRow]:
    """Show that any rate fits, given a suitable set of campaign offsets.

    Each contrast is the mean difference between a reference campaign and
    campaign j, measured on articles common to both. For an assumed rate the
    offset campaign j must carry is simply

        offset[j] = contrast[j] - rate * elapsed[j]

    Every row returned reproduces the observed contrasts exactly. That is the
    point: a table where every row fits is a demonstration that the data does
    not choose, not a sensitivity study.

    :param contrasts: campaign index -> mean (reference minus that campaign).
    :param elapsed: campaign index -> time since the reference campaign.
    :raises ValueError: if the two mappings do not cover the same campaigns.
    """
    if set(contrasts) != set(elapsed):
        raise ValueError("contrasts and elapsed must cover the same campaigns")
    keys = sorted(contrasts)
    return [
        InvarianceRow(
            assumed_rate=r,
            required_offsets=tuple(contrasts[k] - r * elapsed[k] for k in keys),
        )
        for r in rates
    ]


def reference_set_size(
    difference_sd: float,
    tolerance: float,
    *,
    sd_is_estimated: bool = True,
    max_articles: int = 100_000,
) -> int:
    """Stable articles needed to pin the campaign offset to ``tolerance``.

    The offset is a mean over the reference articles, so its 95% half-width is
    ``q * sd / sqrt(m)`` and the requirement inverts to ``m >= (q*sd/tol)^2``.
    Quadratic in the scatter: halving it quarters the count.

    When ``sd`` is itself estimated from the reference articles -- the usual
    case, and the honest one -- ``q`` is a t quantile on m-1 degrees of
    freedom, so the requirement is implicit and is solved by iteration. Two
    articles is the floor: one cannot estimate a variance at all.

    Sizing on a point estimate of the scatter under-provisions whenever that
    estimate is itself uncertain. Callers with a small pilot study should size
    on the upper confidence bound of the scatter as well, and expect the two
    answers to differ by several fold.

    This assumes the reference articles are genuinely stable and that their
    errors are independent of one another. Drift biases the estimated offset
    and does not average away; correlation ``rho`` between articles inflates
    the variance to ``sd^2 * (1 + (m-1)*rho) / m``, which for positive rho puts
    a floor under the achievable precision no matter how many are carried.

    :raises ValueError: non-positive scatter or tolerance.
    """
    if difference_sd <= 0.0:
        raise ValueError("difference_sd must be positive")
    if tolerance <= 0.0:
        raise ValueError("tolerance must be positive")
    if not sd_is_estimated:
        return max(1, math.ceil((1.96 * difference_sd / tolerance) ** 2))
    m = 2
    for _ in range(200):
        nxt = max(2, math.ceil((t_quantile_95(m - 1) * difference_sd / tolerance) ** 2))
        if nxt == m or nxt > max_articles:
            return min(nxt, max_articles)
        m = nxt
    return m


def minimum_detectable_rate(
    difference_sd: float,
    n_articles: int,
    interval: float,
    *,
    n_reference: int | None = None,
    power: float = 0.80,
    two_sided: bool = True,
) -> float:
    """Smallest rate a campaign can prove, at the stated power.

    Two corrections to the calculation usually offered, both of which matter
    and both of which flatter the programme when omitted.

    A true effect sitting exactly at the significance threshold is detected
    about half the time. Reliable detection needs ``(z_alpha + z_beta)``
    standard errors, not ``z_alpha`` alone -- at 95% and 80% power that is 2.80
    rather than 1.96, so ignoring power understates the detectable rate by
    about 40%.

    And estimating the offset does not make it known. With the offset taken
    from ``n_reference`` articles the corrected mean carries both variances,

        se = sd * sqrt(1/n + 1/m)

    so a campaign cannot out-run its own control set however many articles it
    pairs. Passing ``n_reference=None`` returns the idealised figure that
    assumes a perfectly known offset; it is useful only as an upper bound on
    what a programme could ever achieve.

    :raises ValueError: non-positive inputs, or a power outside (0, 1).
    """
    if difference_sd <= 0.0 or interval <= 0.0 or n_articles < 1:
        raise ValueError("difference_sd, interval and n_articles must be positive")
    if not 0.0 < power < 1.0:
        raise ValueError("power must lie strictly between 0 and 1")
    z_alpha = 1.96 if two_sided else 1.645
    # Normal quantile for the power term, via a compact rational approximation.
    z_beta = _normal_quantile(power)
    var = 1.0 / n_articles
    if n_reference is not None:
        if n_reference < 1:
            raise ValueError("n_reference must be positive when supplied")
        var += 1.0 / n_reference
    se = difference_sd * math.sqrt(var)
    return (z_alpha + z_beta) * se / interval


def _normal_quantile(p: float) -> float:
    """Inverse standard normal, Acklam's rational approximation."""
    a = [-3.969683028665376e+01, 2.209460984245205e+02, -2.759285104469687e+02,
         1.383577518672690e+02, -3.066479806614716e+01, 2.506628277459239e+00]
    b = [-5.447609879822406e+01, 1.615858368580409e+02, -1.556989798598866e+02,
         6.680131188771972e+01, -1.328068155288572e+01]
    c = [-7.784894002430293e-03, -3.223964580411365e-01, -2.400758277161838e+00,
         -2.549732539343734e+00, 4.374664141464968e+00, 2.938163982698783e+00]
    d = [7.784695709041462e-03, 3.224671290700398e-01, 2.445134137142996e+00,
         3.754408661907416e+00]
    lo, hi = 0.02425, 1.0 - 0.02425
    if p < lo:
        q = math.sqrt(-2.0 * math.log(p))
        return (((((c[0]*q+c[1])*q+c[2])*q+c[3])*q+c[4])*q+c[5]) / \
               ((((d[0]*q+d[1])*q+d[2])*q+d[3])*q+1.0)
    if p > hi:
        q = math.sqrt(-2.0 * math.log(1.0 - p))
        return -(((((c[0]*q+c[1])*q+c[2])*q+c[3])*q+c[4])*q+c[5]) / \
                ((((d[0]*q+d[1])*q+d[2])*q+d[3])*q+1.0)
    q = p - 0.5
    r = q * q
    return (((((a[0]*r+a[1])*r+a[2])*r+a[3])*r+a[4])*r+a[5])*q / \
           (((((b[0]*r+b[1])*r+b[2])*r+b[3])*r+b[4])*r+1.0)


@dataclass(frozen=True)
class VarianceBudget:
    """How much of the observed difference scatter the gauge accounts for."""

    observed_difference_sd: float
    gauge_difference_sd: float
    residual_sd: float
    gauge_fraction_of_variance: float


def variance_budget(observed_sd: float, gauge_sd: float) -> VarianceBudget:
    """Split observed difference scatter into gauge and everything else.

    The residual carries real article-to-article variation in rate together
    with any reproducibility effect -- crew, procedure, instrument. This does
    not separate those two; it says how much room is left for them, which is
    usually enough to settle whether better equipment could help. When the
    residual dominates, it cannot.

    A residual of zero is returned where the gauge exceeds the observed
    scatter, which happens when the gauge estimate is itself noisy and should
    be read as "the gauge accounts for all of it", not as a negative variance.
    """
    if observed_sd < 0.0 or gauge_sd < 0.0:
        raise ValueError("standard deviations must be non-negative")
    resid_var = max(0.0, observed_sd ** 2 - gauge_sd ** 2)
    frac = 1.0 if observed_sd == 0 else min(1.0, gauge_sd ** 2 / observed_sd ** 2)
    return VarianceBudget(
        observed_difference_sd=observed_sd,
        gauge_difference_sd=gauge_sd,
        residual_sd=math.sqrt(resid_var),
        gauge_fraction_of_variance=frac,
    )
