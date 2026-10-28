# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Preload history and three-leg method screening, without installation acceptance criteria.

All threshold defaults are illustrative assumptions, based on previous
experience. No standard-derived procedure, settlement model or dynamic
punch-through prediction is supplied. Screen every leg independently.

Clause context: G 5.3.1-5.3.3; RP 3.1.2, 8.3.1.1-8.3.1.4 and 8.3.5,
first editions. G/RP document identities and OPEN items are recorded in
docs/domains/geotechnical/jackup-screening.md; no clause compliance is claimed.
"""

from __future__ import annotations

from collections.abc import Callable, Sequence
from dataclasses import dataclass
from decimal import Decimal
from fractions import Fraction
import math
from typing import Literal

from digitalmodel.geotechnical.spudcan import LayeredPenetrationCurve, PenetrationPoint, finite

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


@dataclass(frozen=True)
class HullGeometry:
    """Rectangular waterplane, flat bottom and three noncollinear leg positions.

    Coordinates are metres from the waterplane centre; x is transverse and y
    longitudinal. Length spans y, breadth spans x. The rigid hull follows three
    prescribed support levels. Small-angle vertical geometry retains exact
    arctangent reporting. Horizontal CG motion and structural flexibility are
    excluded. Weight density is kN/m3; hull depth limits deck immersion.
    """

    length_m: float
    breadth_m: float
    depth_m: float
    leg_xy_m: tuple[tuple[float, float], ...]
    water_weight_kn_m3: float

    def validate(self) -> None:
        """Reject degenerate geometry and nonfinite hydrostatic inputs."""
        for name in ('length_m', 'breadth_m', 'depth_m', 'water_weight_kn_m3'):
            finite(name, getattr(self, name), strict=True)
        if len(self.leg_xy_m) != 3:
            raise ValueError('exactly three leg coordinates are required')
        for xy in self.leg_xy_m:
            if len(xy) != 2:
                raise ValueError('each leg needs x and y coordinates')
            for value in xy:
                finite('leg coordinate', value, minimum=-math.inf)
        _support_plane(self.leg_xy_m, (0, 0, 0))


@dataclass(frozen=True)
class MethodStage:
    """One held-weight state, with signed reference draft and a trial loss leg.

    dry_leg_loads_kn encode total weight and both CG moments as equivalent dry
    support reactions. Actual ballast compartment layout is not qualified.
    Positive draft is immersion; negative draft is air gap at zero tide. Tide
    changes water level at fixed jacking position and fixed ballast in a state.
    A null trial leg requests independent loss cases for each of the three legs.
    """

    method: str
    number: int
    dry_leg_loads_kn: tuple[float, float, float]
    reference_draft_m: float
    trial_leg: int | None
    substep: int = 1


@dataclass(frozen=True)
class HullResponse:
    """Required static reactions at imposed support levels, before soil limits.

    Negative reactions expose loss of contact and invalidate the three-support
    equilibrium. Reactions above soil resistance are unresolved demands. The
    result neither caps those demands nor predicts transient load transfer.
    First moments use integral(x*dB) and integral(y*dB), in kN m.
    """

    reactions_kn: tuple[float, ...]
    reaction_changes_kn: tuple[float, ...]
    buoyancy_kn: float
    buoyancy_first_moment_x_kn_m: float
    buoyancy_first_moment_y_kn_m: float
    centre_draft_m: float
    minimum_draft_m: float
    maximum_draft_m: float
    wetted_area_m2: float
    trim_deg: float
    heel_deg: float
    contact_valid: bool


def _support_plane(xy: Sequence[tuple[float, float]], values: Sequence[float]
                   ) -> tuple[float, float, float]:
    """Interpolate a+b*x+c*y through three support levels."""
    (x0, y0), (x1, y1), (x2, y2) = xy
    determinant = (x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)
    if determinant == 0 or not math.isfinite(determinant):
        raise ValueError('leg coordinates must form a finite noncollinear triangle')
    v0, v1, v2 = values
    b = ((v1 - v0) * (y2 - y0) - (v2 - v0) * (y1 - y0)) / determinant
    c = ((x1 - x0) * (v2 - v0) - (x2 - x0) * (v1 - v0)) / determinant
    return v0 - b * x0 - c * y0, b, c


def _wet_polygon(corners: list[tuple[float, float]],
                 draft: Callable[[tuple[float, float]], float]) -> list[tuple[float, float]]:
    """Clip the rectangular bottom at the waterline, retaining partial contact."""
    wet = []
    for left, right in zip(corners, corners[1:] + corners[:1]):
        dl, dr = draft(left), draft(right)
        if dl >= 0:
            wet.append(left)
        if (dl < 0 < dr) or (dr < 0 < dl):
            fraction = dl / (dl - dr)
            wet.append((left[0] + fraction * (right[0] - left[0]),
                        left[1] + fraction * (right[1] - left[1])))
    return wet


def _buoyancy(hull: HullGeometry, plane: tuple[float, float, float]
              ) -> tuple[float, float, float, float, float, float]:
    """Integrate linear draft and its quadratic first moments exactly on triangles."""
    a, b, c = plane
    corners = [(-hull.breadth_m / 2, -hull.length_m / 2),
               (hull.breadth_m / 2, -hull.length_m / 2),
               (hull.breadth_m / 2, hull.length_m / 2),
               (-hull.breadth_m / 2, hull.length_m / 2)]

    def draft(xy: tuple[float, float]) -> float:
        return a + b * xy[0] + c * xy[1]

    depths = [draft(xy) for xy in corners]
    if max(depths) >= hull.depth_m:
        raise ValueError('deck immersion is outside the hull hydrostatic model')
    wet = _wet_polygon(corners, draft)
    volume = moment_x = moment_y = area = 0.0
    for i in range(1, len(wet) - 1):
        triangle = (wet[0], wet[i], wet[i + 1])
        (x0, y0), (x1, y1), (x2, y2) = triangle
        size = abs((x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)) / 2
        ds = [draft(xy) for xy in triangle]
        total = sum(ds)
        volume += size * total / 3
        moment_x += size / 12 * (sum(x for x, _ in triangle) * total
                                 + sum(x * d for (x, _), d in zip(triangle, ds)))
        moment_y += size / 12 * (sum(y for _, y in triangle) * total
                                 + sum(y * d for (_, y), d in zip(triangle, ds)))
        area += size
    weight = hull.water_weight_kn_m3
    return (volume * weight, moment_x * weight, moment_y * weight,
            area, min(depths), max(depths))


def hull_response(hull: HullGeometry, stage: MethodStage, *, tide_m: float = 0,
                  failed_leg: int | None = None, settlement_m: float = 0) -> HullResponse:
    """Balance force and both moments after a prescribed one-leg displacement.

    Positive tide increases immersion. Hull downward displacement interpolates
    the supplied leg settlement; the two other supports are held fixed. Weight
    and dry CG moments are unchanged. Hydrostatic pressure redistributes the
    remaining reactions. Without water contact this model has no load transfer.
    No stiffness, inertia, jacking controller or dynamic arrest is represented.
    """
    hull.validate()
    finite('tide', tide_m, minimum=-math.inf)
    finite('settlement', settlement_m)
    finite('reference draft', stage.reference_draft_m, minimum=-math.inf)
    if len(stage.dry_leg_loads_kn) != 3:
        raise ValueError('three dry equivalent leg loads are required')
    for load in stage.dry_leg_loads_kn:
        finite('dry equivalent load', load)
    if failed_leg is not None and (type(failed_leg) is not int or failed_leg not in range(3)):
        raise ValueError('failed leg must be an integer from zero to two')
    if failed_leg is None and settlement_m != 0:
        raise ValueError('settlement requires a failed leg')
    levels = tuple(settlement_m if i == failed_leg else 0 for i in range(3))
    heave, slope_x, slope_y = _support_plane(hull.leg_xy_m, levels)
    reference = stage.reference_draft_m + tide_m
    buoyancy, mx, my, area, minimum, maximum = _buoyancy(
        hull, (reference + heave, slope_x, slope_y))
    base_b, base_mx, base_my, _, _, _ = _buoyancy(hull, (reference, 0, 0))
    reactions, changes = [], []
    for i, load in enumerate(stage.dry_leg_loads_kn):
        # Barycentric shape function Ni=ai+bi*x+ci*y transfers a distributed
        # waterplane force into three reactions while retaining both moments.
        ai, bi, ci = _support_plane(hull.leg_xy_m, tuple(int(j == i) for j in range(3)))
        share = ai * buoyancy + bi * mx + ci * my
        baseline = ai * base_b + bi * base_mx + ci * base_my
        reactions.append(load - share)
        changes.append(baseline - share)
    return HullResponse(tuple(reactions), tuple(changes), buoyancy, mx, my,
                        reference + heave, minimum, maximum, area,
                        math.degrees(math.atan(slope_y)), math.degrees(math.atan(slope_x)),
                        all(load >= 0 for load in reactions))


def preload_method_stages(*, target_kn: float, reserve_kn: float,
                          maximum_weight_kn: float, wet_draft_m: float,
                          elevated_air_gap_m: float, early_fraction: float = 0.6
                          ) -> tuple[MethodStage, ...]:
    """Build ballast-limited dry-load commands for three assumed methods.

    One-leg-at-a-time has three elevated states. All-legs has one elevated
    state. The staged variant has six stages, each with three sequential
    single-leg substeps; the first four stages are in water and the final two
    elevated. Between states the active load returns to reserve and the ballast
    is transferred. No penetration memory, rebound or elapsed time is implied.
    The public 2021 case paper, Example Rig Move 1 (p. 5), describes a 2006
    six-stage operation with the first four stages in water. The within-stage
    three-leg sweeps, load fractions and final elevation here are assumptions;
    detailed case stages are unpublished. This is not a qualified case replay.
    Each method: SNAME T&RB 5-5A Rev 3 (2008), clause: TO CONFIRM;
    ISO 19905-1 (2012), clause: TO CONFIRM. No compliance is claimed.
    """
    for name, value in (('target', target_kn), ('reserve', reserve_kn),
                        ('maximum weight', maximum_weight_kn), ('wet draft', wet_draft_m),
                        ('elevated air gap', elevated_air_gap_m), ('early fraction', early_fraction)):
        finite(name, value, strict=True)
    if reserve_kn > target_kn or maximum_weight_kn < 3 * reserve_kn:
        raise ValueError('reserve exceeds target or available weight')
    if early_fraction > 1 or early_fraction * target_kn < reserve_kn:
        raise ValueError('early target fraction must lie between reserve/target and one')

    def loads(active: tuple[int, ...], target: float) -> tuple[float, float, float]:
        demand = len(active) * (target - reserve_kn)
        available = maximum_weight_kn - 3 * reserve_kn
        extra = (min(demand, available) / len(active))
        result = [reserve_kn + extra if i in active else reserve_kn for i in range(3)]
        return result[0], result[1], result[2]

    stages = [MethodStage('one_leg_at_a_time', i + 1, loads((i,), target_kn),
                          -elevated_air_gap_m, i) for i in range(3)]
    stages.append(MethodStage('all_legs', 1, loads((0, 1, 2), target_kn),
                              -elevated_air_gap_m, None))
    fractions = (early_fraction, (early_fraction + 1) / 2, 1, 1, 1, 1)
    for number, fraction in enumerate(fractions, 1):
        for leg in range(3):
            stages.append(MethodStage('staged_in_water_then_elevated', number,
                                      loads((leg,), fraction * target_kn),
                                      wet_draft_m if number <= 4 else -elevated_air_gap_m,
                                      leg, substep=leg + 1))
    return tuple(stages)


@dataclass(frozen=True)
class PreloadMethodCheck:
    """Per-leg capacity headroom and unresolved demand, with signed margins.

    Peak headroom is peak minus reaction; post-peak reserve is first trough
    minus reaction. Target margin is reaction minus target. Positive headroom
    does not establish stability, and reaching a peak is flagged at equality.
    Without a descending branch the interval maximum is used and post-peak
    reserve is None. All results describe only the supplied curve interval.
    """

    method: str
    stage: int
    substep: int
    tide_m: float
    reference_draft_m: float
    condition: str
    leg: int
    trial_leg: int | None
    reaction_kn: float
    reaction_change_kn: float
    target_margin_kn: float
    peak_headroom_kn: float
    post_peak_reserve_kn: float | None
    capacity_drop_kn: float
    reaches_descending_peak: bool
    within_evaluated_capacity: bool
    contact_valid: bool
    buoyancy_kn: float
    centre_draft_m: float
    minimum_draft_m: float
    maximum_draft_m: float
    trim_deg: float
    heel_deg: float


def compare_preload_methods(curves: Sequence[LayeredPenetrationCurve], hull: HullGeometry,
                            stages: Sequence[MethodStage], *, tide_levels_m: Sequence[float],
                            target_kn: float, settlement_m: float) -> tuple[PreloadMethodCheck, ...]:
    """Evaluate every state, tide and leg before and after a trial support loss.

    Curves may differ by leg. Settlement is prescribed, never inferred from
    peak/trough depths. Required reactions are not capped at available soil
    capacity. The one-leg history hold checker remains separate: these snapshots
    contain no observed penetration or hold-time evidence.
    """
    finite('target', target_kn, strict=True)
    finite('settlement', settlement_m, strict=True)
    if len(curves) != 3 or not stages or not tide_levels_m:
        raise ValueError('three curves, stages and tide levels are required')
    rows = []
    for stage in stages:
        if stage.trial_leg is not None and (type(stage.trial_leg) is not int
                                           or stage.trial_leg not in range(3)):
            raise ValueError('trial leg must be an integer from zero to two or None')
        trials = range(3) if stage.trial_leg is None else (stage.trial_leg,)
        conditions: list[tuple[str, int | None]] = [('nominal', None)]
        conditions.extend(('prescribed_settlement', i) for i in trials)
        for tide in tide_levels_m:
            for condition, trial in conditions:
                response = hull_response(hull, stage, tide_m=tide, failed_leg=trial,
                                         settlement_m=settlement_m if trial is not None else 0)
                for leg, (curve, reaction) in enumerate(zip(curves, response.reactions_kn)):
                    trough = curve.events[0].trough.load_kn if curve.events else None
                    # A higher earlier capacity shields a later descending peak.
                    reached = any(reaction >= max(p.load_kn for p in curve.points
                                                  if p.depth_m <= event.peak.depth_m)
                                  for event in curve.events)
                    rows.append(PreloadMethodCheck(
                        stage.method, stage.number, stage.substep, tide, stage.reference_draft_m,
                        condition, leg, trial if trial is not None else stage.trial_leg,
                        reaction, response.reaction_changes_kn[leg], reaction - target_kn,
                        curve.peak.load_kn - reaction,
                        None if trough is None else trough - reaction,
                        curve.post_peak_drop_kn, reached,
                        0 <= reaction <= curve.maximum.load_kn, response.contact_valid,
                        response.buoyancy_kn, response.centre_draft_m,
                        response.minimum_draft_m, response.maximum_draft_m,
                        response.trim_deg, response.heel_deg))
    return tuple(rows)
