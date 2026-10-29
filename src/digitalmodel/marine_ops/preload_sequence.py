# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Per-leg preload history screening, without installation acceptance criteria.

All threshold defaults are illustrative assumptions, based on previous
experience. No standard-derived procedure, settlement model or dynamic
punch-through prediction is supplied. Screen every leg independently.

Clause context: G 5.3.1-5.3.3; RP 3.1.2, 8.3.1.1-8.3.1.4 and 8.3.5,
first editions. G/RP document identities and OPEN items are recorded in
docs/domains/geotechnical/jackup-screening.md; no clause compliance is claimed.
"""

from __future__ import annotations

from collections.abc import Sequence
from dataclasses import dataclass
from decimal import Decimal
from fractions import Fraction
from typing import Literal

from digitalmodel.geotechnical.spudcan import PenetrationPoint, finite

StageName = Literal['preparation', 'preload', 'holding', 'discharge']
STAGE_ORDER = ('preparation', 'preload', 'holding', 'discharge')


def _decimal_input(value: float) -> Fraction:
    """Interpret str(value) as decimal input, then retain exact rational arithmetic.

    Hold times, depths and limits use Decimal(str(value)); fractions keep window
    interpolation and rate division exact, including recurring decimal results.
    Comparisons use no tolerance: equal durations pass, equal rates fail. Only
    reported results become floats. Precision already lost in inputs is not
    recovered, and this policy supplies no measurement uncertainty allowance.
    """
    return Fraction(Decimal(str(value)))


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

    Hold at least one hour at or above target load. Settlement stops when total
    absolute depth movement per hour over the trailing 0.25 h is strictly below
    0.001 m/h. Readings are joined linearly; unobserved motion is unqualified.
    A preload increment up to 0.05 times target is small; positive penetration
    per load increment above 0.01 m/kN raises a punch-through flag. Positive
    penetration at zero load increment during preloading also raises a flag.
    These inputs are sensitivity controls, not qualified physical limits.
    G 5.3.1-5.3.3; RP 3.1.2, 8.3.1.1-8.3.1.4: unspecified standard hold
    rules remain OPEN; these explicit illustrative controls do not supply them.
    """

    minimum_hold_h: float = 1.0
    settlement_window_h: float = 0.25
    settlement_rate_limit_m_h: float = 0.001
    punch_through_rate_m_kn: float = 0.01
    small_load_increment_fraction: float = 0.05

    def validate(self) -> None:
        """Reject nonfinite controls and a hold minimum below one hour."""
        for name in self.__dataclass_fields__:
            finite(name, getattr(self, name), strict=True)
        if self.minimum_hold_h < 1:
            raise ValueError('minimum hold must be at least one hour')
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
    The mitigation note accompanies every result, independently of hazard flags;
    no hull clearance or buoyancy-arrest calculation is performed.
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
    mitigation_note: str = (
        'During preload, keep the hull at a small air gap, close to the water, '
        'so that buoyancy can be regained if a leg punches through.'
    )


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


def _settlement_rate(samples: Sequence[PreloadSample], window_h: float) -> Fraction | None:
    end = _decimal_input(samples[-1].time_h)
    window = _decimal_input(window_h)
    available = end - _decimal_input(samples[0].time_h)
    if available < window:
        return None
    start = end - window
    travel = Fraction(0)
    for left, right in zip(samples, samples[1:]):
        left_time = _decimal_input(left.time_h)
        right_time = _decimal_input(right.time_h)
        if right_time <= start:
            continue
        fraction = (right_time - max(start, left_time)) / (right_time - left_time)
        travel += abs(_decimal_input(right.penetration_m)
                      - _decimal_input(left.penetration_m)) * fraction
    return travel / window


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
    duration = _decimal_input(full_load[-1].time_h) - _decimal_input(start)
    rate = _settlement_rate(full_load, criteria.settlement_window_h)
    reasons = []
    if duration < _decimal_input(criteria.minimum_hold_h):
        reasons.append('hold_shorter_than_minimum')
    if rate is None:
        reasons.append('insufficient_settlement_window')
    elif rate >= _decimal_input(criteria.settlement_rate_limit_m_h):
        reasons.append('settlement_rate_not_below_limit')
    return start, float(duration), float(rate) if rate is not None else None, tuple(reasons)


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
    reading; a settled window does not require the whole hour to be settled.

    RP 6.2.6.1-6.2.6.4, 8.3.5; C6.2.6.3, C8.3: descending-branch warnings
    and shallowest-root reporting are partial screens. Multiple-root reporting,
    lower-support qualification and supplied margin/plunge criteria remain OPEN.
    G 5.3.1-5.3.3; RP 3.1.2, 8.3.1.1-8.3.1.4: hold checks use illustrative
    criteria only. The result carries the small-air-gap mitigation note.
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
