# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Generic three-leg failure thresholds and conditional punch-through response.

Elementary mechanics and range-labelled assumptions extend jackup_screening's
static loads; neither module supplies qualified structural design capacities.
SNAME T&RB 5-5A Rev 3 (2008), clause: TO CONFIRM; ISO 19905-1:2012,
clause: TO CONFIRM. No standards conformity or event reconstruction is claimed.
"""

from __future__ import annotations

import math
from collections.abc import Sequence
from dataclasses import dataclass, field, fields

from digitalmodel.geotechnical.spudcan import (
    LayeredPenetrationCurve, LayeredProfile, Spudcan, finite, penetration_curve,
)
from digitalmodel.marine_ops.failure_sequence import FailureMode, FailureSequence, rank_failures
from digitalmodel.marine_ops.jackup_screening import environmental_actions, screen_three_leg_unit

ASSUMED = 'assumed, based on previous experience'
STANDARDS = 'SNAME T&RB 5-5A Rev 3 (2008), clause: TO CONFIRM; ISO 19905-1:2012, clause: TO CONFIRM'
# Reference constants adopted by jackup_screening; not measured site properties.
WATER_WEIGHT_KN_M3 = 1.025 * 9.81


def _assumed(value: float, low: float, high: float, unit: str):
    return field(default=value, metadata={'range': (low, high), 'unit': unit, 'basis': ASSUMED})


@dataclass(frozen=True)
class JackupInputs:
    """Independent assumption bounds, not a specification of an actual unit.

    Member ranges are assumed, not extracted from a licensed standard or a
    manufacturer. Effective member lengths include the assumed end restraint.
    Wave heights, tide, draft, ballast and preload pattern are unknown inputs.
    """

    water_depth_m: float = _assumed(60, 30, 90, 'm')
    dry_weight_kn: float = _assumed(36000, 30000, 60000, 'kN')
    leg_plan_radius_m: float = _assumed(20, 15, 30, 'm')
    chord_radius_m: float = _assumed(8, 5, 10, 'm')
    chord_diameter_m: float = _assumed(0.6, 0.5, 0.9, 'm')
    chord_thickness_m: float = _assumed(0.025, 0.02, 0.05, 'm')
    chord_effective_length_m: float = _assumed(12, 6, 18, 'm')
    brace_diameter_m: float = _assumed(0.25, 0.2, 0.4, 'm')
    brace_thickness_m: float = _assumed(0.018, 0.012, 0.03, 'm')
    brace_effective_length_m: float = _assumed(7, 5, 12, 'm')
    brace_angle_deg: float = _assumed(35, 25, 60, 'degrees')
    elastic_modulus_kpa: float = _assumed(210e6, 200e6, 215e6, 'kPa')
    yield_strength_kpa: float = _assumed(355e3, 235e3, 460e3, 'kPa')
    holding_capacity_per_leg_kn: float = _assumed(35000, 25000, 60000, 'kN')
    sliding_coefficient: float = _assumed(0.25, 0.15, 0.4, '1')
    draft_m: float = _assumed(1, 0.5, 2, 'm')
    hull_depth_m: float = _assumed(7, 5, 10, 'm')
    hull_length_m: float = _assumed(60, 50, 80, 'm')
    hull_beam_m: float = _assumed(50, 40, 70, 'm')
    waterplane_area_m2: float = _assumed(2200, 1800, 2800, 'm2')
    hull_vertical_wave_area_m2: float = _assumed(150, 100, 400, 'm2')
    hull_swell_capacity_kn: float = _assumed(15000, 8000, 25000, 'kN')
    ballast_capacity_kn: float = _assumed(120000, 80000, 160000, 'kN')
    preload_increment_per_leg_kn: float = _assumed(18000, 10000, 24000, 'kN')
    storm_hs_m: float = _assumed(6, 0, 9, 'm')
    storm_tp_s: float = _assumed(12, 8, 16, 's')
    storm_wind_m_s: float = _assumed(40, 0, 50, 'm/s')
    storm_current_m_s: float = _assumed(1.2, 0, 2, 'm/s')
    preload_hs_m: float = _assumed(1, 0, 3, 'm')
    preload_tp_s: float = _assumed(10, 8, 16, 's')
    preload_wind_m_s: float = _assumed(10, 0, 20, 'm/s')
    preload_current_m_s: float = _assumed(0.5, 0, 1, 'm/s')
    wind_area_m2: float = _assumed(1800, 1000, 2500, 'm2')
    leg_water_area_m2: float = _assumed(200, 100, 350, 'm2')
    wind_drag_coefficient: float = _assumed(1.2, 1, 1.5, '1')
    water_drag_coefficient: float = _assumed(1, 0.8, 1.4, '1')
    tide_m: float = _assumed(1, 0, 2, 'm')

    def __post_init__(self) -> None:
        for item in fields(self):
            value = getattr(self, item.name)
            finite(item.name, value)
            low, high = item.metadata['range']
            if not low <= value <= high:
                raise ValueError(f'{item.name} is outside the declared assumption range')
        if self.draft_m >= self.hull_depth_m:
            raise ValueError('draft must leave positive buoyancy reserve')
        if WATER_WEIGHT_KN_M3*self.waterplane_area_m2*self.draft_m >= self.dry_weight_kn:
            raise ValueError('minimum-draft buoyancy must leave compressive leg contact')
        if self.waterplane_area_m2 > self.hull_length_m*self.hull_beam_m:
            raise ValueError('waterplane area cannot exceed the rectangular hull footprint')
        for prefix in ('chord', 'brace'):
            if 2*getattr(self, f'{prefix}_thickness_m') >= getattr(self, f'{prefix}_diameter_m'):
                raise ValueError('tube thickness must leave a positive bore')

    def assumptions(self) -> dict:
        """Export every numerical input with its units, range and data basis."""
        return {item.name: {'value': getattr(self, item.name), **dict(item.metadata)}
                for item in fields(self)}


@dataclass(frozen=True)
class MemberCapacity:
    """Elastic buckling/yield minimum; excludes imperfections and local buckling."""

    area_m2: float
    second_moment_m4: float
    axial_capacity_kn: float


def member_capacity(diameter_m: float, thickness_m: float, effective_length_m: float,
                    elastic_modulus_kpa: float, yield_strength_kpa: float) -> MemberCapacity:
    """Calculate A, I and min(A Fy, pi² E I / Le²), without code factors."""
    for name, value in locals().copy().items():
        finite(name, value, strict=True)
    inner = diameter_m - 2*thickness_m
    if inner <= 0:
        raise ValueError('tube thickness must leave a positive bore')
    area = math.pi/4 * (diameter_m**2 - inner**2)
    inertia = math.pi/64 * (diameter_m**4 - inner**4)
    return MemberCapacity(area, inertia, min(area*yield_strength_kpa,
                                           math.pi**2*elastic_modulus_kpa*inertia/effective_length_m**2))


@dataclass(frozen=True)
class PunchConsequence:
    """One-leg kinematic screen at the first capacity loss, not dynamic arrest."""

    travel_m: float
    travel_is_lower_bound: bool
    static_soil_recovery_travel_m: float | None
    peak_to_trough_travel_m: float
    tilt_deg: float
    elastic_leg_moment_knm: float
    elastic_leg_shear_kn: float
    remaining_air_gap_m: float | None
    initial_buoyancy_kn: float
    reserve_buoyancy_kn: float
    buoyancy_replacement_kn: float
    buoyancy_balance_travel_m: float | None
    buoyancy_can_balance_loss: bool
    ballast_to_replace_initial_buoyancy_kn: float


def _check_state(hull_state: str, air_gap_m: float) -> None:
    finite('air gap', air_gap_m)
    if hull_state not in ('in_water', 'elevated'):
        raise ValueError('hull state must be in_water or elevated')
    if ((hull_state == 'in_water' and air_gap_m != 0)
            or (hull_state == 'elevated' and not 0 < air_gap_m <= 30)):
        raise ValueError('in-water gap must be zero; elevated gap must be in (0, 30] m')


def punch_consequence(curve: LayeredPenetrationCurve, p: JackupInputs,
                      hull_state: str, air_gap_m: float) -> PunchConsequence:
    """Screen a failed first leg rotating about the other two supports.

    Failed-leg lever=1.5R; buoyancy at the hull centre has lever=0.5R.
    Therefore replacing a reaction loss dQ needs 3dQ buoyancy. Centre descent
    is leg travel/3, giving leg travel=9dQ/(gamma Awp). Edge descent has lever
    (L+R)/2, so centre sinkage before edge immersion is freeboard*R/(L+R).
    Reserve uses this sinkage with an assumed constant waterplane. Moment
    equilibrium and buoyancy: OpenStax, University Physics Volume 1 (2016),
    sections 12.1 and 14.4. Available reserve and soil recovery cap this
    quasi-static hypothesis. No impact or energy balance is solved. Elevated
    travel uses step-1 peak-to-static-recovery distance; absent recovery,
    peak-to-trough travel is only a lower bound.
    """
    _check_state(hull_state, air_gap_m)
    wet = hull_state == 'in_water'
    initial = WATER_WEIGHT_KN_M3*p.waterplane_area_m2*p.draft_m if wet else 0.0
    centre_sinkage_limit = ((p.hull_depth_m-p.draft_m)*p.leg_plan_radius_m
                            /(p.hull_length_m+p.leg_plan_radius_m))
    reserve = WATER_WEIGHT_KN_M3*p.waterplane_area_m2*centre_sinkage_limit if wet else 0.0
    recovery, trough_travel, loss = None, 0.0, 0.0
    if curve.events:
        event = curve.events[0]
        recovery = (event.recovery_depth_m-event.peak.depth_m
                    if event.recovery_depth_m is not None else None)
        trough_travel, loss = event.trough.depth_m-event.peak.depth_m, event.drop_kn
    balance = 9*loss/(WATER_WEIGHT_KN_M3*p.waterplane_area_m2) if wet and loss > 0 else None
    buoyancy_balances = bool(balance is not None and 3*loss <= reserve
                            and (recovery is None or balance <= recovery))
    travel = balance if buoyancy_balances and balance is not None else (recovery if recovery is not None else trough_travel)
    lower_bound = bool(curve.events and recovery is None and not buoyancy_balances)
    tilt = math.atan(travel/(1.5*p.leg_plan_radius_m))
    chord = member_capacity(p.chord_diameter_m, p.chord_thickness_m,
                            p.chord_effective_length_m, p.elastic_modulus_kpa, p.yield_strength_kpa)
    leg_i = 3*chord.second_moment_m4 + 1.5*chord.area_m2*p.chord_radius_m**2
    length = p.water_depth_m + air_gap_m
    # Cantilever tip translation L*tan(theta), elastic lateral stiffness 3EI/L³.
    moment = 3*p.elastic_modulus_kpa*leg_i*math.tan(tilt)/length
    edge_drop = travel*(p.hull_length_m/2+p.leg_plan_radius_m/2)/(1.5*p.leg_plan_radius_m)
    return PunchConsequence(travel, lower_bound, recovery, trough_travel, math.degrees(tilt),
                            moment, moment/length, air_gap_m-edge_drop if not wet else None,
                            initial, reserve, 3*loss if wet else 0.0, balance,
                            buoyancy_balances, initial)


@dataclass(frozen=True)
class JackupFailureScreen:
    """Intact thresholds and separately labelled conditional consequences."""

    sequence: FailureSequence
    curve: LayeredPenetrationCurve
    consequence: PunchConsequence
    actions: dict
    capacities: dict
    not_applicable: tuple[str, ...]
    reactions_at_one_kn: tuple[float, float, float]
    ballast_at_one_kn: float
    ballast_to_match_dry_preload_at_one_kn: float


def screen_jackup(*, profile: LayeredProfile, spudcan: Spudcan,
                  tip_depths_m: Sequence[float], inputs: JackupInputs,
                  phase: str, hull_state: str, air_gap_m: float,
                  preload_pattern: str = 'all_legs',
                  load_factors: Sequence[float] | None = None) -> JackupFailureScreen:
    """Build the first example of generic callback-based failure ranking.

    Gravity remains at factor zero. Preload adds ballast linearly; storm holds
    gravity fixed. Environmental resultants and wave crest scale linearly with
    factor (a synthetic action path, not a scaled sea-state return period).
    Heading is adverse to the first leg. In water, evaluate both signs of the
    vertical swell proxy for maximum compression and minimum restoring weight.
    All-leg and single-leg preload are independent, explicit assumptions.
    """
    _check_state(hull_state, air_gap_m)
    if phase not in ('preload', 'storm') or preload_pattern not in ('all_legs', 'single_leg'):
        raise ValueError('unknown phase or preload pattern')
    p, wet = inputs, hull_state == 'in_water'
    radius = p.leg_plan_radius_m
    supports = ((radius, 0), (-radius/2, math.sqrt(3)*radius/2),
                (-radius/2, -math.sqrt(3)*radius/2))
    curve = penetration_curve(profile, spudcan, tip_depths_m)
    response = punch_consequence(curve, p, hull_state, air_gap_m)
    hs, tp, wind, current = (getattr(p, f'{phase}_{name}') for name in
                              ('hs_m', 'tp_s', 'wind_m_s', 'current_m_s'))
    action = environmental_actions(
        hs_m=hs, tp_s=tp, wind_m_s=wind, current_m_s=current,
        water_depth_m=p.water_depth_m, air_gap_m=air_gap_m,
        wind_area_m2=p.wind_area_m2, submerged_area_m2=p.leg_water_area_m2,
        wind_drag_coefficient=p.wind_drag_coefficient, water_drag_coefficient=p.water_drag_coefficient)
    hull_horizontal = (0.5*1.025*p.water_drag_coefficient*p.hull_beam_m*p.draft_m
                       *(current+action.surface_orbital_velocity_m_s)**2 if wet else 0.0)
    hull_vertical = WATER_WEIGHT_KN_M3*p.hull_vertical_wave_area_m2*hs/2 if wet else 0.0
    horizontal = action.horizontal_force_kn + hull_horizontal
    moment = action.moment_y_knm + hull_horizontal*p.water_depth_m
    chord = member_capacity(p.chord_diameter_m, p.chord_thickness_m, p.chord_effective_length_m,
                            p.elastic_modulus_kpa, p.yield_strength_kpa)
    brace = member_capacity(p.brace_diameter_m, p.brace_thickness_m, p.brace_effective_length_m,
                            p.elastic_modulus_kpa, p.yield_strength_kpa)
    preload_count = 3 if preload_pattern == 'all_legs' else 1

    def ballast(factor):
        return factor*preload_count*p.preload_increment_per_leg_kn if phase == 'preload' else 0.0

    def path(factor, swell_sign=1):
        weight = p.dry_weight_kn + ballast(factor) - response.initial_buoyancy_kn
        gravity_moment = (ballast(factor)*radius if phase == 'preload'
                          and preload_pattern == 'single_leg' else 0.0)
        vertical = weight + swell_sign*factor*hull_vertical
        finite('effective weight on path', vertical, strict=True)
        my = gravity_moment-factor*moment
        state = screen_three_leg_unit(
            vertical_load_kn=vertical, supports_m=supports, moment_x_knm=0, moment_y_knm=my,
            horizontal_force_kn=factor*horizontal, preload_per_leg_kn=p.holding_capacity_per_leg_kn,
            sliding_resistance_kn=p.sliding_coefficient*vertical)
        return state, vertical, my

    def compression(factor):
        return max(path(factor)[0].reactions_kn)

    def holding(factor):
        return max(abs(r) for sign in (-1, 1) for r in path(factor, sign)[0].reactions_kn)

    def leg_moment(factor):
        # Equal horizontal sharing; cantilever base moment per leg.
        return factor*horizontal/3*(p.water_depth_m+air_gap_m)

    def chord_demand(factor, post=False):
        value = leg_moment(factor)
        if post:
            # Conditional imposed-sway bending plus an illustrative P-delta term.
            value += response.elastic_leg_moment_knm + compression(factor)*(p.water_depth_m+air_gap_m)*math.tan(math.radians(response.tilt_deg))
        return holding(factor)/3 + 2*value/(3*p.chord_radius_m)

    def brace_demand(factor, post=False):
        shear = factor*horizontal/3 + (response.elastic_leg_shear_kn if post else 0)
        return shear/(2*math.sin(math.radians(p.brace_angle_deg)))

    modes = []
    inactive = []
    capacities = {}

    def add(name, demand, capacity, description):
        capacities[name] = description + '; ' + STANDARDS
        modes.append(FailureMode(name, demand, capacity, capacities[name]))

    # Profile exhaustion differs from a first descending peak. No loss mode is
    # invented for the uniform control. Underlying capacity can be higher.
    add('leg_penetration', compression, lambda f: curve.maximum.load_kn,
        'Step-1 maximum resistance within investigated tip interval; no extrapolation')
    if curve.events:
        peak = curve.events[0].peak.load_kn
        add('spudcan_punch_through', compression, lambda f: peak,
            'Step-1 first descending peak; strong-over-weak capacity loss')
    else:
        inactive.append('spudcan_punch_through')
    add('chord_buckling', chord_demand, lambda f: chord.axial_capacity_kn,
        'min(A Fy, pi² E I/Le²); three chords, y=(r,-r/2,-r/2); imperfections and local buckling omitted')
    add('brace_buckling', brace_demand, lambda f: brace.axial_capacity_kn,
        'min(A Fy, pi² E I/Le²); two braces share panel shear; end restraint assumed')
    add('leg_hull_holding', holding, lambda f: p.holding_capacity_per_leg_kn,
        'Assumed per-leg pinion or fixation equivalent holding limit; no equipment qualification')
    add('overturning', lambda f: abs(path(f, -1)[2]),
        lambda f: path(f, -1)[1]*radius*(1 if path(f, -1)[2] >= 0 else 0.5),
        'Rigid support polygon, directional first zero reaction; no soil moment credit')
    add('sliding', lambda f: f*horizontal,
        lambda f: p.sliding_coefficient*path(f, -1)[1],
        'Assumed friction coefficient times minimum effective weight; combined VHM omitted')
    if wet:
        add('hull_swell', lambda f: f*math.hypot(hull_horizontal, hull_vertical),
            lambda f: p.hull_swell_capacity_kn,
            'Assumed hull resultant limit; drag plus gamma Aeffective Hs/2 vertical swell proxy')
        add('buoyancy_reserve', lambda f: (response.buoyancy_replacement_kn
                                          if curve.events and compression(f) >= curve.events[0].peak.load_kn else 0),
            lambda f: response.reserve_buoyancy_kn,
            'Conditional 3 times first reaction loss versus reserve to deck-edge immersion; dynamic arrest unresolved')
        inactive.append('air_gap')
    else:
        add('air_gap', lambda f: p.tide_m+f*hs/2, lambda f: air_gap_m,
            'Assumed Hs/2 crest plus tide versus underside elevation; no extreme-crest qualification')
    add('ballast_reserve', lambda f: ballast(f), lambda f: p.ballast_capacity_kn,
        'Assumed ballast weight capacity; existing ballast not measured')
    if wet and phase == 'preload':
        add('ballast_replacement_reserve', lambda f: ballast(f)+response.initial_buoyancy_kn,
            lambda f: p.ballast_capacity_kn,
            'Ballast reserve to match dry-hull preload reactions, including initial buoyancy; extra ballast not applied to this path')
    if curve.events:
        def triggered(factor):
            return compression(factor) >= curve.events[0].peak.load_kn

        add('post_punch_chord', lambda f: chord_demand(f, True) if triggered(f) else 0,
            lambda f: chord.axial_capacity_kn,
            'Conditional one-leg run: elastic cantilever 3EI delta/L² and P-delta; lower bound if recovery missing')
        add('post_punch_brace', lambda f: brace_demand(f, True) if triggered(f) else 0,
            lambda f: brace.axial_capacity_kn,
            'Conditional imposed-sway shear 3EI delta/L³ in two braces; no transient amplification')
        if not wet:
            if response.remaining_air_gap_m is None:
                raise ValueError('elevated response requires a remaining air gap')
            edge_drop = air_gap_m-response.remaining_air_gap_m
            add('post_punch_air_gap', lambda f: p.tide_m+f*hs/2+(edge_drop if triggered(f) else 0),
                lambda f: air_gap_m,
                'Conditional hull-edge descent from step-1 static run; not an impact calculation')
    else:
        inactive.extend(('post_punch_chord', 'post_punch_brace', 'post_punch_air_gap'))
    if load_factors is None:
        # Stop before zero total effective weight: the compression-only static
        # model cannot represent a freely floating, dynamically restrained hull.
        ballast_slope = preload_count*p.preload_increment_per_leg_kn if phase == 'preload' else 0.0
        upper = 8.0
        if hull_vertical > ballast_slope:
            upper = min(upper, 0.95*(p.dry_weight_kn-response.initial_buoyancy_kn)
                        /(hull_vertical-ballast_slope))
        load_factors = tuple(upper*i/40 for i in range(41))
    result = rank_failures(modes, load_factors)
    return JackupFailureScreen(result, curve, response,
                              {'horizontal_force_kn': horizontal, 'moment_magnitude_knm': moment,
                               'hull_horizontal_swell_kn': hull_horizontal,
                               'hull_vertical_swell_kn': hull_vertical, 'wave_crest_m': hs/2},
                              capacities, tuple(inactive), path(1)[0].reactions_kn, ballast(1),
                              ballast(1)+response.initial_buoyancy_kn if phase == 'preload' else 0.0)
