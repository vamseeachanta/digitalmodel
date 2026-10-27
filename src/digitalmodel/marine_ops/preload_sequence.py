# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Per-leg preload history screening, without installation acceptance criteria.

All threshold defaults are illustrative assumptions, based on previous
experience. No standard-derived procedure, settlement model or dynamic
punch-through prediction is supplied. Screen every leg independently.
"""

from __future__ import annotations

from collections.abc import Sequence
from dataclasses import dataclass
from math import isclose
from typing import Literal

from digitalmodel.geotechnical.spudcan import PenetrationPoint, finite

StageName = Literal['preparation', 'preload', 'holding', 'discharge']
STAGE_ORDER = ('preparation', 'preload', 'holding', 'discharge')
# Arithmetic equality only, separate from the physical criteria. Compare elapsed
# durations (not absolute timestamps) and rates with no absolute tolerance.
# This covers subtraction roundoff at ordinary history scales, not measurement
# uncertainty or arbitrary loss of precision from very large input offsets.
_BOUNDARY_REL_TOL = 1e-12


@dataclass(frozen=True)
class PreloadSample:
    """Elapsed hours, applied per-leg load and measured or computed depth.

    Depth uses the planned curve's datum: the lowest level of the widest
    spudcan cross-section. Computed readings require a separately supplied
    settlement model; the screen itself does not predict settlement.
    """

    time_h: float
    applied_load_kn: float
    penetration_m: float


@dataclass(frozen=True)
class PreloadStage:
    """One named stage with nonempty chronological per-leg readings."""

    name: StageName
    samples: tuple[PreloadSample, ...]


@dataclass(frozen=True)
class PreloadCriteria:
    """Explicit illustrative controls, assumed based on previous experience.

    The illustrative default holds one hour at or above target load; callers
    may supply any positive minimum duration. Settlement stops when total
    absolute depth movement per hour over the trailing 0.25 h is strictly below
    0.001 m/h. Readings are joined linearly; unobserved motion is unqualified.
    A preload increment up to 0.05 times target is small; positive penetration
    per load increment above 0.01 m/kN raises a punch-through flag. Positive
    penetration at zero load increment during preloading also raises a flag.
    These inputs are sensitivity controls, not qualified physical limits.
    """

    minimum_hold_h: float = 1.0
    settlement_window_h: float = 0.25
    settlement_rate_limit_m_h: float = 0.001
    punch_through_rate_m_kn: float = 0.01
    small_load_increment_fraction: float = 0.05

    def validate(self) -> None:
        """Reject nonfinite or nonpositive controls and invalid fractions."""
        for name in self.__dataclass_fields__:
            finite(name, getattr(self, name), strict=True)
        if self.small_load_increment_fraction > 1:
            raise ValueError('small load increment fraction must not exceed one')


@dataclass(frozen=True)
class PreloadStepCheck:
    """One reading compared with the shallowest planned loading equilibrium."""

    stage: str
    sample: PreloadSample
    planned_penetration_m: float | None
    deviation_m: float | None
    within_planned_depth: bool
    penetration_per_load_m_kn: float | None
    punch_through_reasons: tuple[str, ...]


@dataclass(frozen=True)
class PreloadSequenceScreen:
    """Hold evidence and independent hazard flags, not permission to install.

    Hold start is the start of the final uninterrupted full-load interval within
    holding; hold end is the final holding reading, excluding discharge. The
    criterion is evaluated at that end. None denotes unavailable evidence.
    """

    target_preload_per_leg_kn: float
    criteria: PreloadCriteria
    stage_order: tuple[str, ...]
    hold_start_h: float | None
    hold_end_h: float
    hold_duration_h: float
    settlement_rate_m_h: float | None
    hold_met: bool
    hold_reasons: tuple[str, ...]
    punch_through: bool
    steps: tuple[PreloadStepCheck, ...]


def _validate_history(stages: Sequence[PreloadStage],
                      curve: Sequence[PenetrationPoint]) -> None:
    if tuple(stage.name for stage in stages) != STAGE_ORDER:
        raise ValueError('stages must be preparation, preload, holding, discharge in order')
    previous_time = -1.0
    for stage in stages:
        if not stage.samples:
            raise ValueError('every stage requires at least one sample')
        for sample in stage.samples:
            finite('elapsed time', sample.time_h)
            finite('applied load', sample.applied_load_kn)
            finite('penetration', sample.penetration_m)
            if sample.time_h <= previous_time:
                raise ValueError('sample times must be strictly increasing across all stages')
            previous_time = sample.time_h
    if len(curve) < 2:
        raise ValueError('planned curve needs at least two points')
    previous_depth = -1.0
    for point in curve:
        finite('curve depth', point.depth_m)
        finite('curve load', point.load_kn)
        if point.depth_m <= previous_depth:
            raise ValueError('planned curve depths must be strictly increasing')
        previous_depth = point.depth_m


def _planned_depth(curve: Sequence[PenetrationPoint], load_kn: float) -> float | None:
    # The shallowest intersection is explicit: a falling branch can have other
    # equilibria. Neither unloading hysteresis nor branch switching is modelled.
    for left, right in zip(curve, curve[1:]):
        if min(left.load_kn, right.load_kn) <= load_kn <= max(left.load_kn, right.load_kn):
            if left.load_kn == right.load_kn:
                return left.depth_m
            fraction = (load_kn - left.load_kn) / (right.load_kn - left.load_kn)
            return left.depth_m + fraction * (right.depth_m - left.depth_m)
    return None


def _settlement_rate(samples: Sequence[PreloadSample], window_h: float) -> float | None:
    end = samples[-1].time_h
    available = end - samples[0].time_h
    if available < window_h and not isclose(
            available, window_h, rel_tol=_BOUNDARY_REL_TOL, abs_tol=0.0):
        return None
    # A slightly short window accepted as equal starts at the first reading;
    # never extrapolate beyond the evidence to cover subtraction roundoff.
    start = max(samples[0].time_h, end - window_h)
    travel = 0.0
    for left, right in zip(samples, samples[1:]):
        if right.time_h <= start:
            continue
        fraction = (right.time_h - max(start, left.time_h)) / (right.time_h - left.time_h)
        travel += abs(right.penetration_m - left.penetration_m) * fraction
    return travel / window_h


def _hold_evidence(samples: Sequence[PreloadSample], target: float,
                   criteria: PreloadCriteria) -> tuple[float | None, float, float | None, tuple[str, ...]]:
    # A load dip discards earlier holding credit. No preloading or discharge
    # time contributes. Full load means at or above the supplied per-leg target.
    start_index = 0
    for index, sample in enumerate(samples):
        if sample.applied_load_kn < target:
            start_index = index + 1
    full_load = samples[start_index:]
    if not full_load:
        return None, 0.0, None, ('full_preload_not_maintained',)
    start = full_load[0].time_h
    duration = full_load[-1].time_h - start
    rate = _settlement_rate(full_load, criteria.settlement_window_h)
    reasons = []
    if duration < criteria.minimum_hold_h and not isclose(
            duration, criteria.minimum_hold_h, rel_tol=_BOUNDARY_REL_TOL, abs_tol=0.0):
        reasons.append('hold_shorter_than_minimum')
    if rate is None:
        reasons.append('insufficient_settlement_window')
    elif rate >= criteria.settlement_rate_limit_m_h or isclose(
            rate, criteria.settlement_rate_limit_m_h, rel_tol=_BOUNDARY_REL_TOL, abs_tol=0.0):
        reasons.append('settlement_rate_not_below_limit')
    return start, duration, rate, tuple(reasons)


def screen_preload_sequence(
    stages: Sequence[PreloadStage], *, target_preload_per_leg_kn: float,
    planned_curve: Sequence[PenetrationPoint],
    criteria: PreloadCriteria = PreloadCriteria(),
) -> PreloadSequenceScreen:
    """Walk four stages for one leg and report hold evidence and hazards.

    Every sample gets planned depth and signed actual-minus-planned deviation.
    Intersections use piecewise linear load/depth segments and the shallowest
    root, without extrapolation; None means load outside the planned curve.
    The depth-range indicator is independent of that load-range comparison.
    Discharge comparisons are loading-curve references, not rebound predictions.

    During preloading, compare each sample with the previous sample, including
    the last preparation reading. A sharp positive depth change at a small
    nonnegative load increment raises a flag using the explicit criteria.
    At every stage, any falling curve segment extending below the current tip
    raises a separate capacity-drop flag, even if target load is below the peak.
    No flag is not proof of safety beyond the curve or between readings.
    Holding evidence is independent of these flags and is evaluated at its last
    reading; a settled window does not require the whole hold to be settled.
    """
    finite('target preload per leg', target_preload_per_leg_kn, strict=True)
    criteria.validate()
    _validate_history(stages, planned_curve)
    steps = []
    previous = None
    for stage in stages:
        for sample in stage.samples:
            planned = _planned_depth(planned_curve, sample.applied_load_kn)
            reasons = []
            rate = None
            if stage.name == 'preload' and previous is not None:
                load_change = sample.applied_load_kn - previous.applied_load_kn
                depth_change = sample.penetration_m - previous.penetration_m
                if load_change > 0:
                    rate = depth_change / load_change
                if 0 <= load_change <= criteria.small_load_increment_fraction * target_preload_per_leg_kn:
                    if load_change == 0 and depth_change > 0:
                        reasons.append('penetration_at_constant_preload_step')
                    elif rate is not None and rate > criteria.punch_through_rate_m_kn:
                        reasons.append('sharp_penetration_at_small_load_increment')
            if any(right.load_kn < left.load_kn and right.depth_m > sample.penetration_m
                   for left, right in zip(planned_curve, planned_curve[1:])):
                reasons.append('planned_capacity_drop_below_tip')
            steps.append(PreloadStepCheck(
                stage.name, sample, planned,
                sample.penetration_m - planned if planned is not None else None,
                planned_curve[0].depth_m <= sample.penetration_m <= planned_curve[-1].depth_m,
                rate, tuple(reasons),
            ))
            previous = sample
    holding = stages[2].samples
    start, duration, settlement_rate, hold_reasons = _hold_evidence(
        holding, target_preload_per_leg_kn, criteria)
    return PreloadSequenceScreen(
        target_preload_per_leg_kn, criteria, tuple(stage.name for stage in stages),
        start, holding[-1].time_h, duration, settlement_rate, not hold_reasons,
        hold_reasons, any(step.punch_through_reasons for step in steps), tuple(steps),
    )
