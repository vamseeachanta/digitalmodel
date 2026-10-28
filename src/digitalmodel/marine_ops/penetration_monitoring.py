# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Compare a single leg's preload readings with a supplied uncertain curve.

Inputs use kN, metres and elapsed seconds, with penetration positive downward
from one common seabed datum. Loads shall represent the same per-leg reaction
basis in prediction and measurement. Use a separate monitor for each leg of a
generic three-leg independent-leg jack-up; aggregate load is not a leg reading.

The supplied knots follow increasing nominal penetration; loads may rise, fall
or plateau. At each measured load, linear interpolation of the lower and upper
penetration bounds supplies all intersected intervals. Multiple disconnected
intervals are reported, not collapsed across an unpredicted gap. Their inclusion
does not establish which equilibrium is reachable. No extrapolation is made.

Triggers (all limits are required inputs, not standard-derived constants):
* Penetration strictly outside every interpolated interval.
* K = delta load / delta penetration drops by at least the supplied fraction
  between two consecutive positive loading increments, before a supplied-curve
  local peak. Each load increment must exceed the constant-load tolerance.
* Positive penetration rate strictly exceeds its limit while absolute load
  change is at most the supplied tolerance, over one sampling interval.
Missing prediction coverage also requires a hold and reassessment. A curve with
no local peak cannot establish the pre-peak stiffness trigger. Rate is a sampled
average; a short transient between readings can be missed. Sensor uncertainty,
sampling cadence and site-specific trigger qualification remain external inputs.

Alerts mark entry into a condition; active conditions are returned every time.
Reassessment and Plan B actions remain latched after a condition clears. No
reading is certified as a safe fallback stage. Actions are advisory responses
under an independently agreed procedure, not automatic equipment commands.

Clause context only: SNAME T&R 5-5A, Rev 3 (2008), RP 6.2.6 (layered soils)
and 8.3.5 (punch-through); ISO 19905-1:2023, 9.3.2 (leg penetration during
preloading). No clause compliance or numerical trigger requirement is claimed.
Public operational basis: Guidelines for the Selection and Operation of
Jack-ups in the Marine Renewable Energy Industry, Issue 2 (2013), 18.5.7-18.5.10
and 18.6.3: https://iadc.org/wp-content/uploads/2015/04/
ruk13-h_Guidelines-for-the-Selection-and-Operations-of-Jack-ups-in-the-Marine-Renewable-Energy-Industry.pdf
"""

from __future__ import annotations

from collections.abc import Iterable
from dataclasses import dataclass
from enum import Enum
from fractions import Fraction
from math import isfinite
from numbers import Real
from types import MappingProxyType


class Trigger(str, Enum):
    """Conditions requiring an independent review of the preload procedure."""

    OUTSIDE_BAND = 'outside_band'
    STIFFNESS_DROP = 'stiffness_drop'
    RAPID_PENETRATION = 'rapid_penetration'
    PREDICTION_UNAVAILABLE = 'prediction_unavailable'


class PlanBAction(str, Enum):
    """Advisory responses; feasibility and authority are assessed externally."""

    HOLD_LOAD = 'hold load'
    REDUCE_TO_REVIEWED_STAGE = (
        'reduce to the last independently established safe stage under the agreed procedure; '
        'this monitor does not establish that stage'
    )
    HULL_TO_WATER = 'keep or return the hull to the water under the agreed procedure'
    STOP_AND_REASSESS = 'stop and re-assess'
    MOVE_ON_PAPER = "move the rig on paper before mobilisation"


_HOLD_ACTIONS = (
    PlanBAction.HOLD_LOAD, PlanBAction.STOP_AND_REASSESS, PlanBAction.MOVE_ON_PAPER,
)
_INSTABILITY_ACTIONS = (
    PlanBAction.HOLD_LOAD, PlanBAction.REDUCE_TO_REVIEWED_STAGE,
    PlanBAction.HULL_TO_WATER, PlanBAction.STOP_AND_REASSESS, PlanBAction.MOVE_ON_PAPER,
)
PLAN_B_ACTIONS = MappingProxyType({
    Trigger.OUTSIDE_BAND: _HOLD_ACTIONS,
    Trigger.STIFFNESS_DROP: _INSTABILITY_ACTIONS,
    Trigger.RAPID_PENETRATION: _INSTABILITY_ACTIONS,
    Trigger.PREDICTION_UNAVAILABLE: _HOLD_ACTIONS,
})


def _number(name: str, value: float) -> Fraction:
    """Reject invalid nonnegative SI readings; preserve decimal boundaries."""
    if isinstance(value, bool) or not isinstance(value, Real) or not isfinite(value) or value < 0:
        raise ValueError(f'{name} must be a finite nonnegative number')
    return Fraction(str(value))


@dataclass(frozen=True)
class PredictionPoint:
    """Nominal depth/load knot and its lower/upper penetration bounds (m)."""

    penetration_m: float
    load_kn: float
    lower_penetration_m: float
    upper_penetration_m: float


@dataclass(frozen=True)
class Measurement:
    """One timestamped per-leg reading, with a common time and depth datum."""

    elapsed_s: float
    load_kn: float
    penetration_m: float


@dataclass(frozen=True)
class MonitoringCriteria:
    """Explicit project inputs; the module supplies no operational defaults."""

    stiffness_drop_fraction: float
    rapid_penetration_rate_m_s: float
    constant_load_tolerance_kn: float

    def __post_init__(self) -> None:
        """Validate explicitly supplied nonnegative limits and drop fraction."""
        for name in self.__dataclass_fields__:
            _number(name, getattr(self, name))
        if not 0 < self.stiffness_drop_fraction < 1:
            raise ValueError('stiffness drop fraction must be between zero and one')
        if self.rapid_penetration_rate_m_s <= 0:
            raise ValueError('rapid penetration rate must be positive')


@dataclass(frozen=True)
class Alert:
    """Entry into one trigger condition, with its Plan B response."""

    trigger: Trigger
    actions: tuple[PlanBAction, ...]


@dataclass(frozen=True)
class MonitoringResult:
    """Reading, calculated diagnostics, active conditions and latched response."""

    sample: Measurement
    predicted_intervals_m: tuple[tuple[float, float], ...]
    ambiguous_prediction: bool
    stiffness_kn_m: float | None
    stiffness_drop_fraction: float | None
    penetration_rate_m_s: float | None
    next_peak_penetration_m: float | None
    active_triggers: tuple[Trigger, ...]
    alerts: tuple[Alert, ...]
    requires_reassessment: bool
    plan_b_actions: tuple[PlanBAction, ...]


def _validate_values(record: PredictionPoint | Measurement) -> None:
    for name in record.__dataclass_fields__:
        _number(name, getattr(record, name))


def _validated_curve(curve: Iterable[PredictionPoint]) -> tuple[PredictionPoint, ...]:
    points = tuple(curve)
    if len(points) < 2:
        raise ValueError('prediction requires at least two knots')
    previous = Fraction(-1)
    for point in points:
        _validate_values(point)
        depth = _number('penetration', point.penetration_m)
        if depth <= previous:
            raise ValueError('nominal prediction penetrations must be strictly increasing')
        _validate_bounds(point)
        previous = depth
    return points


def _validate_bounds(point: PredictionPoint) -> None:
    if not point.lower_penetration_m <= point.penetration_m <= point.upper_penetration_m:
        raise ValueError('bounds must contain the nominal penetration')


def _segment_interval(left: PredictionPoint, right: PredictionPoint,
                      load: Fraction) -> tuple[Fraction, Fraction] | None:
    start, end = Fraction(str(left.load_kn)), Fraction(str(right.load_kn))
    if not min(start, end) <= load <= max(start, end):
        return None
    if start == end:
        return (Fraction(str(min(left.lower_penetration_m, right.lower_penetration_m))),
                Fraction(str(max(left.upper_penetration_m, right.upper_penetration_m))))
    fraction = (load - start) / (end - start)
    bounds = []
    for name in ('lower_penetration_m', 'upper_penetration_m'):
        a, b = Fraction(str(getattr(left, name))), Fraction(str(getattr(right, name)))
        bounds.append(a + fraction * (b - a))
    return bounds[0], bounds[1]


def _merged_intervals(intervals: Iterable[tuple[Fraction, Fraction]]) -> tuple[tuple[Fraction, Fraction], ...]:
    merged: list[tuple[Fraction, Fraction]] = []
    for low, high in sorted(intervals):
        if merged and low <= merged[-1][1]:
            merged[-1] = (merged[-1][0], max(high, merged[-1][1]))
        else:
            merged.append((low, high))
    return tuple(merged)


def _intervals(curve: tuple[PredictionPoint, ...], load: Fraction) -> tuple[tuple[Fraction, Fraction], ...]:
    """Intersect every piecewise-linear branch and merge overlapping bands."""
    candidates = (_segment_interval(left, right, load) for left, right in zip(curve, curve[1:]))
    return _merged_intervals(item for item in candidates if item is not None)


def _band_trigger(intervals: tuple[tuple[Fraction, Fraction], ...], depth: Fraction) -> Trigger | None:
    if not intervals:
        return Trigger.PREDICTION_UNAVAILABLE
    if not any(low <= depth <= high for low, high in intervals):
        return Trigger.OUTSIDE_BAND
    return None


def _drop(current: Fraction | None, previous: Fraction | None) -> Fraction | None:
    if current is None or previous is None:
        return None
    return max(Fraction(0), 1 - current / previous)


def _stiffness_trigger(drop: Fraction | None, peak: PredictionPoint | None,
                       criteria: MonitoringCriteria) -> Trigger | None:
    if drop is not None and peak is not None and drop >= Fraction(str(criteria.stiffness_drop_fraction)):
        return Trigger.STIFFNESS_DROP
    return None


def _rate_trigger(rate: Fraction | None, sample: Measurement, previous: Measurement | None,
                  criteria: MonitoringCriteria) -> Trigger | None:
    if previous is None:
        return None
    change = abs(Fraction(str(sample.load_kn)) - Fraction(str(previous.load_kn)))
    if (rate is not None and change <= Fraction(str(criteria.constant_load_tolerance_kn))
            and rate > Fraction(str(criteria.rapid_penetration_rate_m_s))):
        return Trigger.RAPID_PENETRATION
    return None


def _new_alerts(active: tuple[Trigger, ...], previous: tuple[Trigger, ...]) -> tuple[Alert, ...]:
    return tuple(Alert(t, PLAN_B_ACTIONS[t]) for t in active if t not in previous)


def _latched_actions(previous: tuple[PlanBAction, ...], active: tuple[Trigger, ...]) -> tuple[PlanBAction, ...]:
    return tuple(dict.fromkeys(previous + tuple(a for t in active for a in PLAN_B_ACTIONS[t])))


def _float_or_none(value: Fraction | None) -> float | None:
    return float(value) if value is not None else None


def _validate_time(sample: Measurement, previous: Measurement | None) -> None:
    if previous is not None and sample.elapsed_s <= previous.elapsed_s:
        raise ValueError('measurement times must be strictly increasing')


class PenetrationMonitor:
    """Bounded-state streaming monitor for one leg; no automatic reset or resume."""

    def __init__(self, predicted_curve: Iterable[PredictionPoint], criteria: MonitoringCriteria):
        """Copy the prediction and initialize an independent single-leg stream."""
        self.curve = _validated_curve(predicted_curve)
        self.criteria = criteria
        self._peaks = tuple(
            point for left, point, right in zip(self.curve, self.curve[1:], self.curve[2:])
            if left.load_kn <= point.load_kn > right.load_kn
        )
        self._previous: Measurement | None = None
        self._previous_stiffness: Fraction | None = None
        self._active: tuple[Trigger, ...] = ()
        self._latched_actions: tuple[PlanBAction, ...] = ()

    def _next_peak(self, sample: Measurement) -> PredictionPoint | None:
        return next((p for p in self._peaks
                     if p.penetration_m > sample.penetration_m and p.load_kn >= sample.load_kn), None)

    def _increments(self, sample: Measurement) -> tuple[Fraction | None, Fraction | None]:
        if self._previous is None:
            return None, None
        previous = self._previous
        load_change = Fraction(str(sample.load_kn)) - Fraction(str(previous.load_kn))
        depth_change = Fraction(str(sample.penetration_m)) - Fraction(str(previous.penetration_m))
        duration = Fraction(str(sample.elapsed_s)) - Fraction(str(previous.elapsed_s))
        stiffness = None
        if load_change > Fraction(str(self.criteria.constant_load_tolerance_kn)) and depth_change > 0:
            stiffness = load_change / depth_change
        return stiffness, depth_change / duration

    def update(self, sample: Measurement) -> MonitoringResult:
        """Validate before state changes, then flag this reading and new entries."""
        _validate_values(sample)
        _validate_time(sample, self._previous)
        intervals = _intervals(self.curve, Fraction(str(sample.load_kn)))
        peak = self._next_peak(sample)
        stiffness, rate = self._increments(sample)
        drop = _drop(stiffness, self._previous_stiffness)
        conditions = (
            _band_trigger(intervals, Fraction(str(sample.penetration_m))),
            _stiffness_trigger(drop, peak, self.criteria),
            _rate_trigger(rate, sample, self._previous, self.criteria),
        )
        active = tuple(t for t in conditions if t is not None)
        alerts = _new_alerts(active, self._active)
        self._latched_actions = _latched_actions(self._latched_actions, active)
        self._previous, self._previous_stiffness, self._active = sample, stiffness, active
        return MonitoringResult(
            sample, tuple((float(a), float(b)) for a, b in intervals), len(intervals) > 1,
            _float_or_none(stiffness), _float_or_none(drop), _float_or_none(rate),
            peak.penetration_m if peak is not None else None,
            active, alerts, bool(self._latched_actions), self._latched_actions,
        )
