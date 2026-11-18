"""Decline shortfall and bounded, constant-uplift economic screening.

Public equation basis:
* Analysis of Decline Curves (1945); public rate-time derivation in
  Texas A&M PETE 613, Lecture 8 (2023), Appendix B:
  https://blasingame.engr.tamu.edu/z_zCourse_Archive/P613_23A/P613_23A_Lectures/
  P613_23A_Lec_08_%28DCA_Gas_Wells%29_%5B600_dpi%5D_%28v20230228%29_%28pdf%29.pdf
* DOE, Simple Photovoltaic Economic Calculations (31 March 2010), item 1: simple
  payback = investment / net benefit rate, without time value of money:
  https://www1.eere.energy.gov/solar/pdfs/simple_pv_economic_calculations_mi.pdf

The oil cash balance and shortfall rule are explicitly defined screening
identities, not standard acceptance criteria. No standards-derived constants
are embedded. Every uplift, price, persistence, freshness and cost criterion
is supplied by the caller. This module evaluates supplied fits; it does not
fit data or import external production repositories. The existing reservoir
ProductionForecast fits data and assumes 30-day cumulative increments; it is
not used for this date-based supplied-fit boundary.
"""

from __future__ import annotations

from dataclasses import dataclass
from datetime import date
import math
from typing import Sequence

from digitalmodel.well.completion.pump_down._validation import finite, scalar


def _basis(value: str) -> None:
    if not isinstance(value, str) or not value.strip():
        raise ValueError("source/basis must be a non-empty string")


def _date(value: date) -> None:
    # Reject datetime: sub-day timestamps require a separate explicit policy.
    if type(value) is not date:
        raise ValueError("date must be a datetime.date without a time")


@dataclass(frozen=True)
class Band:
    """Nonnegative closed interval, in units named by the receiving field.

    Bounds are scenario limits, not statistical confidence limits. Currency
    scalars are USD; physical quantity scalars are explicitly named field units.
    """

    low: float
    high: float

    def __post_init__(self) -> None:
        lo = scalar(self.low, "dimensionless", "low bound")
        hi = scalar(self.high, "dimensionless", "high bound", minimum=lo)
        object.__setattr__(self, "low", lo)
        object.__setattr__(self, "high", hi)


@dataclass(frozen=True)
class DeclineFit:
    """Supplied Arps fit: qi [bbl/day], nominal Di [1/day], b [0,1].

    Quantity qi/Di inputs are converted with the shared petroleum barrel
    registry (42 US gallons); bare scalars use the stated field units.
    Di is nominal, not an annual effective decline percentage. The public
    Arps derivation cited above gives q=qi*exp(-Di*t) for b=0 and
    q=qi*(1+b*Di*t)**(-1/b) otherwise, including harmonic b=1.
    A fit's physical applicability and quality remain the supplier's scope.
    """

    qi_bopd: float
    di_per_day: float
    b: float
    start_date: date
    basis: str

    def __post_init__(self) -> None:
        object.__setattr__(
            self, "qi_bopd", scalar(self.qi_bopd, "bbl/day", "qi", positive=True)
        )
        object.__setattr__(self, "di_per_day", scalar(self.di_per_day, "1/day", "Di"))
        object.__setattr__(self, "b", finite(self.b, "b", maximum=1))
        _date(self.start_date)
        _basis(self.basis)

    def rate_on(self, when: date) -> float:
        """Evaluate q [bbl/day] on calendar elapsed days; Arps, cited above.

        log1p avoids cancellation as b approaches zero. Dates before the fit
        start are rejected. Zero Di represents a constant reference rate.
        """
        _date(when)
        t = (when - self.start_date).days
        if t < 0:
            raise ValueError("observation precedes fit start date")
        decline = self.di_per_day * t
        exponent = -decline if self.b == 0 else -math.log1p(self.b * decline) / self.b
        return self.qi_bopd * math.exp(exponent)


@dataclass(frozen=True)
class RatePoint:
    """One rate [bbl/day] at a date, with a required source identifier.

    Monthly producing-day rates may be assigned to month midpoints as an
    explicitly approximate observation. Calendar-day and producing-day rates
    shall not be mixed with each other or with a differently based fit.
    Zero is treated as shut-in/no positive-rate evidence, not damage diagnosis.
    """

    date: date
    rate_bopd: float
    source: str

    def __post_init__(self) -> None:
        _date(self.date)
        _basis(self.source)
        object.__setattr__(
            self, "rate_bopd", scalar(self.rate_bopd, "bbl/day", "measured rate")
        )


@dataclass(frozen=True)
class CandidateResult:
    """Latest measured/fit comparison and caller-criterion disposition."""

    candidate: bool
    reason: str
    predicted_bopd: float | None
    measured_bopd: float | None
    shortfall_fraction: float | None
    sources: tuple[str, ...]
    fit_basis: str


def candidate_screen(
    fit: DeclineFit,
    history: Sequence[RatePoint],
    *,
    shortfall_fraction: float,
    consecutive_points: int,
    as_of: date,
    max_age_days: float,
    max_gap_days: float,
) -> CandidateResult:
    """Flag a persistent rate shortfall relative to a supplied fitted decline.

    Definition: shortfall=1-q_measured/q_fit; all last N points must be
    positive and strictly below q_fit*(1-shortfall_fraction). N, threshold,
    maximum age and inter-point gap are caller criteria, not published
    damage thresholds. No causal diagnosis or treatment approval is implied.
    History shall be strictly increasing, homogeneous (one well/completion,
    fluid and rate basis), and no later than as_of. Missing samples do not
    become zeros. All points, including older points, are validated.
    """
    threshold = finite(shortfall_fraction, "shortfall fraction", maximum=1)
    if threshold == 1:
        raise ValueError("shortfall fraction must be less than 1")
    if type(consecutive_points) is not int or consecutive_points < 1:
        raise ValueError("consecutive_points must be a positive integer")
    age = scalar(max_age_days, "day", "maximum age")
    gap = scalar(max_gap_days, "day", "maximum gap", positive=True)
    _date(as_of)
    points = tuple(history)
    predicted = []
    previous = None
    for point in points:
        if point.date > as_of or (previous is not None and point.date <= previous):
            raise ValueError(
                "history dates must increase strictly and not be in the future"
            )
        predicted.append(fit.rate_on(point.date))
        previous = point.date
    sources = tuple(point.source for point in points)
    if not points:
        return CandidateResult(
            False, "no_history", None, None, None, sources, fit.basis
        )
    latest = points[-1]
    expected = predicted[-1]
    shortfall = 1 - latest.rate_bopd / expected if expected > 0 else None
    reason = "below_threshold"
    if len(points) < consecutive_points:
        reason = "insufficient_history"
    elif (as_of - latest.date).days > age:
        reason = "stale_history"
    else:
        tail = points[-consecutive_points:]
        forecasts = predicted[-consecutive_points:]
        if any((b.date - a.date).days > gap for a, b in zip(tail, tail[1:])):
            reason = "history_gap"
        elif any(p.rate_bopd == 0 for p in tail):
            reason = "zero_rate_requires_diagnosis"
        elif any(q <= 0 for q in forecasts):
            reason = "zero_forecast"
        elif all(p.rate_bopd < q * (1 - threshold) for p, q in zip(tail, forecasts)):
            reason = "persistent_shortfall"
    return CandidateResult(
        reason == "persistent_shortfall",
        reason,
        expected,
        latest.rate_bopd,
        shortfall,
        sources,
        fit.basis,
    )


@dataclass(frozen=True)
class PaybackBand:
    """Best/worst payback [days]; None means no finite payback in that case."""

    low: float | None
    high: float | None


def uplift_from_shortfall(candidate: CandidateResult, recovery_fraction: Band) -> Band:
    """Return an assumed recovered share of the positive fitted-rate gap.

    Scenario definition: u=max(q_fit-q_measured,0)*recovery_fraction. The
    fraction is a required caller assumption in [0,1], not a success
    probability or a published treatment coefficient. This caps only this
    gap-recovery scenario at the supplied fit, not all possible treatments.
    A candidate flag does not establish that any gap is physically recoverable.
    """
    if recovery_fraction.high > 1:
        raise ValueError("recovery fraction must be between 0 and 1")
    if candidate.predicted_bopd is None or candidate.measured_bopd is None:
        raise ValueError("a measured and predicted oil rate are required")
    gap = max(candidate.predicted_bopd - candidate.measured_bopd, 0)
    return Band(gap * recovery_fraction.low, gap * recovery_fraction.high)


@dataclass(frozen=True)
class EconomicsResult:
    """Constant-rate, undiscounted scenario bounds; net cash may be negative."""

    deferred_oil_bbl: Band
    incremental_oil_bbl: tuple[float, float]
    net_cash_usd: tuple[float, float]
    payback_after_restart_days: PaybackBand
    payback_from_start_days: PaybackBand
    pays_out_in_all_cases: bool
    basis: str


def screen_economics(
    *,
    baseline_bopd,
    uplift_bopd: Band,
    cost_usd: Band,
    duration_days: Band,
    net_price_usd_per_bbl: float,
    horizon_days,
    basis: str,
) -> EconomicsResult:
    """Bound oil, net cash and simple payback against the no-job baseline.

    Definition, for C direct job cost, q0 no-job rate, d outage, u incremental
    oil after restart, p net price and H elapsed horizon from job start:

        deferred = q0*d
        incremental_oil(H) = u*max(H-d,0) - q0*min(d,H)
        net_cash(H) = p*incremental_oil(H) - C
        payback_after_restart = (C+p*q0*d)/(p*u), p>0 and u>0
        payback_from_start = d + payback_after_restart

    These are oil volume/cash accounting identities with the DOE simple
    payback definition cited in the module. Loss is the baseline oil during
    outage, not total post-job rate. Net price means realized USD/bbl less
    royalties and incremental variable costs, supplied consistently with C.
    Uplift is incremental oil (not total liquid and not a percentage).

    Bounds enumerate independent endpoint scenarios; no probability or
    correlation is inferred. No finite positive-margin recovery is returned
    for p<=0 or u=0. Payback is an extrapolated constant-uplift result;
    pays_out_in_all_cases additionally checks the specified horizon.
    No discount, future decline, success probability, gas revenue, delayed
    recovery of deferred barrels or integrity acceptance is modelled.
    """
    q0 = scalar(baseline_bopd, "bbl/day", "baseline oil rate")
    horizon = scalar(horizon_days, "day", "horizon", positive=True)
    price = float(net_price_usd_per_bbl)
    if not math.isfinite(price):
        raise ValueError("net price must be finite USD/bbl")
    _basis(basis)
    cash, oil, restart, elapsed = [], [], [], []
    for cost in (cost_usd.low, cost_usd.high):
        for duration in (duration_days.low, duration_days.high):
            for uplift in (uplift_bopd.low, uplift_bopd.high):
                barrels = uplift * max(horizon - duration, 0) - q0 * min(
                    duration, horizon
                )
                oil.append(barrels)
                cash.append(price * barrels - cost)
                recovery = (
                    (cost + price * q0 * duration) / (price * uplift)
                    if price > 0 and uplift > 0
                    else None
                )
                restart.append(recovery)
                elapsed.append(duration + recovery if recovery is not None else None)

    def bounds(values) -> PaybackBand:
        valid = [x for x in values if x is not None]
        return PaybackBand(
            min(valid) if valid else None,
            max(valid) if len(valid) == len(values) else None,
        )

    payback = bounds(elapsed)
    return EconomicsResult(
        Band(q0 * duration_days.low, q0 * duration_days.high),
        (min(oil), max(oil)),
        (min(cash), max(cash)),
        bounds(restart),
        payback,
        payback.high is not None and payback.high <= horizon,
        basis,
    )
