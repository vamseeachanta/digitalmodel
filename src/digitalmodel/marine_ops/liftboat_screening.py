# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic liftboat screening with explicit criteria and evidence gates.

Units are m, s, kN, kNm, kPa, kg/m³ and degrees unless stated otherwise.
Coordinates are right handed: z upward from seabed, x/y in the support plane.
Results describe supplied cases only and never authorize an operating mode.
"""

from __future__ import annotations

import math
from collections.abc import Mapping, Sequence
from dataclasses import dataclass, fields, is_dataclass, replace
from typing import Any, Literal

from digitalmodel.geotechnical.spudcan import (
    PenetrationPoint, SandOverClay, UniformSand, finite, penetration_curve,
)
from digitalmodel.marine_ops.jackup_screening import screen_three_leg_unit
from digitalmodel.marine_ops.preload_sequence import (
    PreloadCriteria, PreloadStage, screen_preload_sequence,
)

Status = Literal['met', 'not_met', 'unresolved']


@dataclass(frozen=True)
class Basis:
    """Caller records provenance, versions, uncertainty cases and applicability.

    Evidence strings identify supporting records; they are not qualifications
    issued by this program. Uncertainty describes bounds and which runs cover
    them. Exact synthetic points must identify their lack of measured evidence.
    """

    configuration: str | None
    provenance: str | None
    criteria_version: str | None
    uncertainty: str | None
    applicability: str | None


@dataclass(frozen=True)
class Check:
    """One criterion result, retaining unrounded demand and capacity."""

    name: str
    status: Status
    demand: float | None = None
    allowable: float | None = None
    utilization: float | None = None
    margin: float | None = None
    note: str = ''


def combine_checks(checks: Sequence[Check]) -> Status:
    """Retain a known failure even when other checks remain unresolved."""
    if any(check.status == 'not_met' for check in checks):
        return 'not_met'
    if not checks or any(check.status == 'unresolved' for check in checks):
        return 'unresolved'
    return 'met'


@dataclass(frozen=True)
class Screen:
    """Checks and reproducible intermediate quantities for one input case."""

    checks: tuple[Check, ...]
    metrics: Mapping[str, Any]

    @property
    def status(self) -> Status:
        """Return criterion status, without implying engineering approval."""
        return combine_checks(self.checks)

    def by_name(self, name: str) -> Check:
        """Find a check without silently substituting a missing result."""
        return next(check for check in self.checks if check.name == name)


def _missing(value: object) -> tuple[str, ...]:
    if value is None or isinstance(value, str) and not value.strip():
        return ('value',)
    if is_dataclass(value) and not isinstance(value, type):
        missing = []
        for field in fields(value):
            item = getattr(value, field.name)
            if item is None or isinstance(item, str) and not item.strip():
                missing.append(field.name)
            else:
                missing.extend(f'{field.name}.{name}' for name in _missing(item))
        return tuple(missing)
    if isinstance(value, (tuple, list)):
        return tuple(f'[{index}].{name}' for index, item in enumerate(value)
                     for name in _missing(item))
    return ()


def _unresolved(names: Sequence[str]) -> Screen:
    return Screen((Check('inputs', 'unresolved', note='Missing: ' + ', '.join(names)),), {})


def _number(name: str, value: float) -> None:
    if isinstance(value, bool) or not isinstance(value, (int, float)) or not math.isfinite(value):
        raise ValueError(f'{name} must be finite and numerical')


def _limit(name: str, demand: float, allowable: float, *, minimum: bool = False) -> Check:
    _number(name, demand)
    finite(name + ' allowable', allowable, strict=True)
    met = demand >= allowable if minimum else demand <= allowable
    ratio = (allowable / demand if demand > 0 else None) if minimum else demand / allowable
    return Check(name, 'met' if met else 'not_met', demand, allowable, ratio,
                 demand - allowable if minimum else allowable - demand)


@dataclass(frozen=True)
class PreloadInput:
    """One leg's staged history and bounded foundation sensitivity cases.

    Profile cases represent the supplied uncertainty bounds, not a probability
    distribution. Circular flat pads use UniformSand; conical cans may use the
    bounded SandOverClay calibration. Other geometries remain unresolved.
    """

    basis: Basis | None
    leg: str | None
    foundation_type: Literal['pad', 'spudcan'] | None
    preload_method: Literal['ballast', 'jacking', 'combined'] | None
    profiles: tuple[SandOverClay | UniformSand, ...] | None
    depths_m: tuple[float, ...] | None
    stages: tuple[PreloadStage, ...] | None
    target_kn: float | None
    resistance_factor: float | None
    criteria: PreloadCriteria | None
    depth_deviation_allowable_m: float | None
    available_travel_m: float | None
    jacking_allowable_kn: float | None
    governing_load_kn: float | None
    foundation_condition: str | None
    soil_evidence: str | None
    support_evidence: str | None
    monitoring_plan: str | None


def screen_preload(value: PreloadInput) -> Screen:
    """Reuse penetration curves and four-stage per-leg preload checks.

    Loading equilibria use the shallowest sampled crossing without extrapolation.
    Observed and predicted depths both count against travel. A falling branch
    is a failed hazard screen; a missing equilibrium is unresolved.
    """
    if missing := _missing(value):
        return _unresolved(missing)
    if value.basis is None:
        raise ValueError('value.basis is required')
    if value.leg is None:
        raise ValueError('value.leg is required')
    if value.foundation_type is None:
        raise ValueError('value.foundation_type is required')
    if value.preload_method is None:
        raise ValueError('value.preload_method is required')
    if value.profiles is None:
        raise ValueError('value.profiles is required')
    if value.depths_m is None:
        raise ValueError('value.depths_m is required')
    if value.stages is None:
        raise ValueError('value.stages is required')
    if value.target_kn is None:
        raise ValueError('value.target_kn is required')
    if value.resistance_factor is None:
        raise ValueError('value.resistance_factor is required')
    if value.criteria is None:
        raise ValueError('value.criteria is required')
    if value.depth_deviation_allowable_m is None:
        raise ValueError('value.depth_deviation_allowable_m is required')
    if value.available_travel_m is None:
        raise ValueError('value.available_travel_m is required')
    if value.jacking_allowable_kn is None:
        raise ValueError('value.jacking_allowable_kn is required')
    if value.governing_load_kn is None:
        raise ValueError('value.governing_load_kn is required')
    if value.foundation_condition is None:
        raise ValueError('value.foundation_condition is required')
    if value.soil_evidence is None:
        raise ValueError('value.soil_evidence is required')
    if value.support_evidence is None:
        raise ValueError('value.support_evidence is required')
    if value.monitoring_plan is None:
        raise ValueError('value.monitoring_plan is required')
    if value.foundation_type not in ('pad', 'spudcan'):
        raise ValueError('foundation type must be pad or spudcan')
    if value.preload_method not in ('ballast', 'jacking', 'combined'):
        raise ValueError('preload method must be ballast, jacking or combined')
    if not value.profiles or not value.depths_m or not value.stages:
        return _unresolved(('profiles, depths and stages must be nonempty',))
    if any(not stage.samples for stage in value.stages):
        return _unresolved(('nonempty samples in every preload stage',))
    if value.foundation_type == 'pad' and any(isinstance(p, SandOverClay) for p in value.profiles):
        return _unresolved(('conical calibration does not establish flat-pad applicability',))
    for name in ('target_kn', 'depth_deviation_allowable_m', 'available_travel_m',
                 'jacking_allowable_kn', 'governing_load_kn'):
        finite(name, getattr(value, name), strict=True)
    finite('resistance factor', value.resistance_factor, strict=True)
    if value.resistance_factor > 1:
        raise ValueError('resistance factor must not exceed one')
    checks, curves, histories, predicted = [], [], [], []
    observed = max(sample.penetration_m for stage in value.stages for sample in stage.samples)
    maximum_load = max(sample.applied_load_kn for stage in value.stages for sample in stage.samples)
    checks.extend((
        _limit('jacking', maximum_load, value.jacking_allowable_kn),
        _limit('governing_preload', value.governing_load_kn, value.target_kn),
        _limit('observed_travel', observed, value.available_travel_m),
    ))
    for index, profile in enumerate(value.profiles, 1):
        prefix = f'soil_{index}.'
        try:
            curve = penetration_curve(profile, list(value.depths_m))
            if isinstance(profile, UniformSand):
                if value.depths_m[0] != 0 or len(curve) < 2:
                    raise ValueError('pad mobilization needs zero and positive sample depths')
                # The soil expression has finite capacity at zero depth. A
                # separate, explicitly assumed mobilization segment begins
                # at zero load; it is not an elastic settlement prediction.
                curve = [PenetrationPoint(0, 0, 0, 'assumed_pad_mobilization'),
                         *(point for point in curve if point.depth_m > 0)]
            curve = [replace(point, pressure_kpa=point.pressure_kpa * value.resistance_factor,
                             load_kn=point.load_kn * value.resistance_factor) for point in curve]
        except ValueError as error:
            # A valid but inapplicable foundation model must not become a pass.
            checks.append(Check(prefix + 'applicability', 'unresolved', note=str(error)))
            continue
        history = screen_preload_sequence(value.stages, target_preload_per_leg_kn=value.target_kn,
                                          planned_curve=curve, criteria=value.criteria)
        curves.append(tuple(curve))
        histories.append(history)
        hold_status: Status = 'met'
        if history.hold_reasons:
            hold_status = ('not_met' if any(reason != 'insufficient_settlement_window'
                                           for reason in history.hold_reasons) else 'unresolved')
        checks.append(Check(prefix + 'holding', hold_status, note=', '.join(history.hold_reasons)))
        checks.append(Check(prefix + 'penetration_hazard',
                            'not_met' if history.punch_through else 'met',
                            note='Capacity drops and abrupt observed penetration are separate flags'))
        unknown = any(step.planned_penetration_m is None or not step.within_planned_depth
                      for step in history.steps)
        checks.append(Check(prefix + 'history_curve_coverage', 'unresolved' if unknown else 'met'))
        deviations = [abs(step.deviation_m) for step in history.steps
                      if step.deviation_m is not None and step.stage != 'discharge']
        if deviations and not unknown:
            checks.append(_limit(prefix + 'depth_deviation', max(deviations),
                                 value.depth_deviation_allowable_m))
        else:
            checks.append(Check(prefix + 'depth_deviation', 'unresolved'))
        # Include both planned preload and later governing demand, without
        # treating the maximum curve ordinate as a stable recovery capacity.
        for label, load in (('target', value.target_kn), ('governing', value.governing_load_kn)):
            depth = None
            for left, right in zip(curve, curve[1:]):
                if min(left.load_kn, right.load_kn) <= load <= max(left.load_kn, right.load_kn):
                    depth = left.depth_m if left.load_kn == right.load_kn else (
                        left.depth_m + (load - left.load_kn) / (right.load_kn - left.load_kn)
                        * (right.depth_m - left.depth_m))
                    break
            if depth is None:
                checks.append(Check(prefix + label + '_travel', 'unresolved',
                                    note='No sampled equilibrium; no extrapolation'))
            else:
                predicted.append(depth)
                checks.append(_limit(prefix + label + '_travel', depth, value.available_travel_m))
    return Screen(tuple(checks), {
        'leg': value.leg, 'foundation_type': value.foundation_type,
        'preload_method': value.preload_method, 'curves': tuple(curves),
        'histories': tuple(histories),
        'predicted_penetration_range_m': (min(predicted), max(predicted)) if predicted else None,
        'maximum_observed_penetration_m': observed,
        'hold_points': value.monitoring_plan,
    })


@dataclass(frozen=True)
class WaterStrip:
    """Signed simultaneous velocity/acceleration, strip centroid and length.

    Cd and Cm are caller inputs. Cm multiplies displaced-fluid acceleration;
    it includes the inertia convention chosen by the caller. Diameter includes
    growth. Kinematics already account for water level, waves and current.
    """

    z_m: float
    length_m: float
    diameter_m: float
    drag_coefficient: float
    velocity_m_s: float
    inertia_coefficient: float
    acceleration_m_s2: float


@dataclass(frozen=True)
class WindPanel:
    """Projected hull or topside area at a stated load centroid."""

    z_m: float
    area_m2: float
    drag_coefficient: float
    velocity_m_s: float


@dataclass(frozen=True)
class StormInput:
    """One simultaneous collinear load case with explicit support capacities.

    Horizontal shares allocate hull wind load; each leg retains its own water
    load. Combined foundation capacities must be qualified for this load case.
    Vertical reactions use three-point statics after subtracting supplied
    foundation moments from external overturning. The integrated entry point
    derives those moments from the same leg model used for bending.
    """

    basis: Basis | None
    case: str | None
    supports_m: tuple[tuple[float, float], ...] | None
    vertical_load_kn: float | None
    gravity_moments_knm: tuple[float, float] | None
    foundation_moments_knm: tuple[tuple[float, float], ...] | None
    heading_deg: float | None
    water_density_kg_m3: float | None
    air_density_kg_m3: float | None
    strips_by_leg: tuple[tuple[WaterStrip, ...], ...] | None
    wind_panels: tuple[WindPanel, ...] | None
    horizontal_shares: tuple[float, ...] | None
    vertical_allowables_kn: tuple[float, ...] | None
    friction_coefficients: tuple[float, ...] | None
    horizontal_allowables_kn: tuple[float, ...] | None
    foundation_moment_allowables_knm: tuple[float, ...] | None
    preload_proven_kn: tuple[float, ...] | None
    hull_underside_m: float | None
    governing_surface_m: float | None
    minimum_clearance_m: float | None
    load_factor: float | None
    return_basis: str | None
    support_evidence: str | None
    dynamics_evidence: str | None
    morison_evidence: str | None
    foundation_evidence: str | None


def screen_storm(value: StormInput) -> Screen:
    """Integrate prescribed Morison strips, wind and global rigid-body statics.

    Fstrip=(rho Cd D length u|u|/2 + rho Cm pi D² length a/4)/1000.
    No independent maxima are combined or wave kinematics inferred. No torsion
    is represented; the input applicability record must establish its exclusion.
    Positive contact is the overturning screen. Negative reactions fail; they
    are never redistributed over fewer supports or used as friction resistance.
    """
    if missing := _missing(value):
        return _unresolved(missing)
    if value.basis is None:
        raise ValueError('value.basis is required')
    if value.case is None:
        raise ValueError('value.case is required')
    if value.supports_m is None:
        raise ValueError('value.supports_m is required')
    if value.vertical_load_kn is None:
        raise ValueError('value.vertical_load_kn is required')
    if value.gravity_moments_knm is None:
        raise ValueError('value.gravity_moments_knm is required')
    if value.foundation_moments_knm is None:
        raise ValueError('value.foundation_moments_knm is required')
    if value.heading_deg is None:
        raise ValueError('value.heading_deg is required')
    if value.water_density_kg_m3 is None:
        raise ValueError('value.water_density_kg_m3 is required')
    if value.air_density_kg_m3 is None:
        raise ValueError('value.air_density_kg_m3 is required')
    if value.strips_by_leg is None:
        raise ValueError('value.strips_by_leg is required')
    if value.wind_panels is None:
        raise ValueError('value.wind_panels is required')
    if value.horizontal_shares is None:
        raise ValueError('value.horizontal_shares is required')
    if value.vertical_allowables_kn is None:
        raise ValueError('value.vertical_allowables_kn is required')
    if value.friction_coefficients is None:
        raise ValueError('value.friction_coefficients is required')
    if value.horizontal_allowables_kn is None:
        raise ValueError('value.horizontal_allowables_kn is required')
    if value.foundation_moment_allowables_knm is None:
        raise ValueError('value.foundation_moment_allowables_knm is required')
    if value.preload_proven_kn is None:
        raise ValueError('value.preload_proven_kn is required')
    if value.hull_underside_m is None:
        raise ValueError('value.hull_underside_m is required')
    if value.governing_surface_m is None:
        raise ValueError('value.governing_surface_m is required')
    if value.minimum_clearance_m is None:
        raise ValueError('value.minimum_clearance_m is required')
    if value.load_factor is None:
        raise ValueError('value.load_factor is required')
    if value.return_basis is None:
        raise ValueError('value.return_basis is required')
    if value.support_evidence is None:
        raise ValueError('value.support_evidence is required')
    if value.dynamics_evidence is None:
        raise ValueError('value.dynamics_evidence is required')
    if value.morison_evidence is None:
        raise ValueError('value.morison_evidence is required')
    if value.foundation_evidence is None:
        raise ValueError('value.foundation_evidence is required')
    per_leg = ('supports_m', 'strips_by_leg', 'horizontal_shares', 'vertical_allowables_kn',
               'friction_coefficients', 'horizontal_allowables_kn', 'foundation_moment_allowables_knm',
               'preload_proven_kn')
    if any(len(getattr(value, name)) != 3 for name in per_leg):
        raise ValueError('all support arrays require exactly three legs')
    if len(value.foundation_moments_knm) != 3 or any(len(m) != 2 for m in value.foundation_moments_knm):
        raise ValueError('three two-dimensional foundation moments are required')
    for moment in value.foundation_moments_knm:
        for component in moment:
            _number('foundation moment', component)
    if len(value.gravity_moments_knm) != 2:
        raise ValueError('two gravity moments are required')
    for moment in value.gravity_moments_knm:
        _number('gravity moment', moment)
    _number('heading', value.heading_deg)
    _number('surface elevation', value.governing_surface_m)
    for name in ('vertical_load_kn', 'water_density_kg_m3', 'air_density_kg_m3',
                 'minimum_clearance_m', 'load_factor', 'hull_underside_m'):
        finite(name, getattr(value, name), strict=True)
    if value.load_factor < 1:
        raise ValueError('screening load factor must be at least one')
    for name in per_leg[2:]:
        for number in getattr(value, name):
            finite(name, number, strict=name not in ('horizontal_shares', 'friction_coefficients'))
    if not math.isclose(sum(value.horizontal_shares), 1, abs_tol=1e-12, rel_tol=0):
        raise ValueError('horizontal shares must sum to one')
    water, water_moments = [], []
    for strips in value.strips_by_leg:
        if not strips:
            return _unresolved(('nonempty water strips per leg; state zero kinematics explicitly',))
        force, moment = 0.0, 0.0
        for strip in strips:
            for name in ('z_m', 'length_m', 'diameter_m', 'drag_coefficient', 'inertia_coefficient'):
                finite(name, getattr(strip, name), strict=name in ('length_m', 'diameter_m'))
            _number('water velocity', strip.velocity_m_s)
            _number('water acceleration', strip.acceleration_m_s2)
            if strip.z_m > value.hull_underside_m:
                raise ValueError('water-strip centroid must be below hull underside')
            action = value.water_density_kg_m3 * strip.length_m / 1000 * (
                0.5 * strip.drag_coefficient * strip.diameter_m * strip.velocity_m_s
                * abs(strip.velocity_m_s) + strip.inertia_coefficient * math.pi
                * strip.diameter_m**2 / 4 * strip.acceleration_m_s2)
            force += action
            moment += action * strip.z_m
        water.append(force * value.load_factor)
        water_moments.append(moment * value.load_factor)
    if not value.wind_panels:
        return _unresolved(('wind panels; state zero wind explicitly',))
    wind, wind_moment = 0.0, 0.0
    for panel in value.wind_panels:
        for name in ('z_m', 'area_m2', 'drag_coefficient'):
            finite(name, getattr(panel, name))
        _number('wind velocity', panel.velocity_m_s)
        force = (0.5 * value.air_density_kg_m3 * panel.area_m2 * panel.drag_coefficient
                 * panel.velocity_m_s * abs(panel.velocity_m_s) / 1000 * value.load_factor)
        wind += force
        wind_moment += force * panel.z_m
    signed_shear = sum(water) + wind
    environmental_moment = sum(water_moments) + wind_moment
    heading = math.radians(value.heading_deg)
    mx = value.gravity_moments_knm[0] - environmental_moment * math.sin(heading)
    my = value.gravity_moments_knm[1] + environmental_moment * math.cos(heading)
    statics = screen_three_leg_unit(
        vertical_load_kn=value.vertical_load_kn, supports_m=value.supports_m,
        moment_x_knm=mx - sum(m[0] for m in value.foundation_moments_knm),
        moment_y_knm=my - sum(m[1] for m in value.foundation_moments_knm),
        horizontal_force_kn=abs(signed_shear),
        preload_per_leg_kn=max(value.preload_proven_kn),
        sliding_resistance_kn=sum(value.horizontal_allowables_kn))
    gravity_reference = screen_three_leg_unit(
        vertical_load_kn=value.vertical_load_kn, supports_m=value.supports_m,
        moment_x_knm=value.gravity_moments_knm[0], moment_y_knm=value.gravity_moments_knm[1],
        horizontal_force_kn=0, preload_per_leg_kn=1, sliding_resistance_kn=0).reactions_kn
    checks = [_limit('hull_clearance', value.hull_underside_m - value.governing_surface_m,
                     value.minimum_clearance_m, minimum=True)]
    shears = []
    for i, reaction in enumerate(statics.reactions_kn):
        name = f'leg_{i + 1}.'
        shear = abs(water[i] + value.horizontal_shares[i] * wind)
        shears.append(shear)
        checks.append(Check(name + 'contact', 'met' if reaction > 1e-10 * value.vertical_load_kn
                            else 'not_met', reaction, 0, margin=reaction,
                            note='Positive reaction required; no uplift resistance assumed'))
        restoring = gravity_reference[i]
        checks.append(Check(name + 'overturning',
                            'met' if restoring > 0 and reaction > 1e-10 * value.vertical_load_kn else 'not_met',
                            max(0, restoring - reaction), restoring,
                            max(0, restoring - reaction) / restoring if restoring > 0 else None,
                            reaction, note='Unloading relative to gravity-only reaction; full unloading fails'))
        checks.append(_limit(name + 'foundation_moment',
                             math.hypot(*value.foundation_moments_knm[i]),
                             value.foundation_moment_allowables_knm[i]))
        checks.append(_limit(name + 'vertical', max(0, reaction), value.vertical_allowables_kn[i]))
        checks.append(_limit(name + 'preload', max(0, reaction), value.preload_proven_kn[i]))
        resistance = min(value.horizontal_allowables_kn[i],
                         value.friction_coefficients[i] * max(0, reaction))
        checks.append(Check(name + 'sliding', 'met' if reaction > 0 and shear <= resistance else 'not_met',
                            shear, resistance, shear / resistance if resistance > 0 else None,
                            resistance - shear))
    return Screen(tuple(checks), {
        'case': value.case, 'base_shear_kn': abs(signed_shear), 'signed_base_shear_kn': signed_shear,
        'moment_x_knm': mx - sum(m[0] for m in value.foundation_moments_knm),
        'moment_y_knm': my - sum(m[1] for m in value.foundation_moments_knm),
        'external_moment_x_knm': mx, 'external_moment_y_knm': my, 'reactions_kn': statics.reactions_kn,
        'water_force_by_leg_kn': tuple(water), 'water_moment_by_leg_knm': tuple(water_moments),
        'wind_force_kn': wind, 'wind_moment_knm': wind_moment,
        'horizontal_demands_kn': tuple(shears),
        'minimum_overturning_margin_kn': min(statics.reactions_kn),
        'hull_clearance_m': value.hull_underside_m - value.governing_surface_m,
    })


@dataclass(frozen=True)
class BendingInput:
    """Fixed-base prismatic leg with a rotational spring to a level hull.

    Lateral loads are (signed force, height) pairs; top_moment is an applied
    couple at the guide. The spring restrains rotation, not translation. The
    supplied elastic critical load is for the same restraints and section.
    Linear axial-plus-bending interaction uses caller-qualified capacities.
    """

    basis: Basis | None
    leg_type: Literal['tubular', 'truss'] | None
    axial_kn: float | None
    lateral_loads: tuple[tuple[float, float], ...] | None
    guide_height_m: float | None
    top_moment_knm: float | None
    elastic_modulus_kpa: float | None
    second_moment_m4: float | None
    guide_stiffness_knm_rad: float | None
    axial_allowable_kn: float | None
    moment_allowable_knm: float | None
    shear_allowable_kn: float | None
    guide_moment_allowable_knm: float | None
    deflection_allowable_m: float | None
    euler_load_kn: float | None
    condition_evidence: str | None
    restraint_evidence: str | None
    capacity_evidence: str | None
    fatigue_evidence: str | None


def screen_bending(value: BendingInput) -> Screen:
    """Solve spring compatibility and screen beam-column amplification.

    theta_free=(P a²/2 + T L)/EI; Ms=k theta_free/(1+kL/EI).
    M(z)=P max(a-z,0)+T-Ms. Magnification is 1/(1-N/Ncr),
    an explicit screening approximation, not a nonlinear frame solution.
    The axial capacity must already include local/global buckling and condition.
    """
    if missing := _missing(value):
        return _unresolved(missing)
    if value.basis is None:
        raise ValueError('value.basis is required')
    if value.leg_type is None:
        raise ValueError('value.leg_type is required')
    if value.axial_kn is None:
        raise ValueError('value.axial_kn is required')
    if value.lateral_loads is None:
        raise ValueError('value.lateral_loads is required')
    if value.guide_height_m is None:
        raise ValueError('value.guide_height_m is required')
    if value.top_moment_knm is None:
        raise ValueError('value.top_moment_knm is required')
    if value.elastic_modulus_kpa is None:
        raise ValueError('value.elastic_modulus_kpa is required')
    if value.second_moment_m4 is None:
        raise ValueError('value.second_moment_m4 is required')
    if value.guide_stiffness_knm_rad is None:
        raise ValueError('value.guide_stiffness_knm_rad is required')
    if value.axial_allowable_kn is None:
        raise ValueError('value.axial_allowable_kn is required')
    if value.moment_allowable_knm is None:
        raise ValueError('value.moment_allowable_knm is required')
    if value.shear_allowable_kn is None:
        raise ValueError('value.shear_allowable_kn is required')
    if value.guide_moment_allowable_knm is None:
        raise ValueError('value.guide_moment_allowable_knm is required')
    if value.deflection_allowable_m is None:
        raise ValueError('value.deflection_allowable_m is required')
    if value.euler_load_kn is None:
        raise ValueError('value.euler_load_kn is required')
    if value.condition_evidence is None:
        raise ValueError('value.condition_evidence is required')
    if value.restraint_evidence is None:
        raise ValueError('value.restraint_evidence is required')
    if value.capacity_evidence is None:
        raise ValueError('value.capacity_evidence is required')
    if value.fatigue_evidence is None:
        raise ValueError('value.fatigue_evidence is required')
    if value.leg_type not in ('tubular', 'truss'):
        raise ValueError('leg type must be tubular or truss')
    if value.leg_type == 'truss':
        return _unresolved(('truss member, joint and effective stiffness model required',))
    _number('top moment', value.top_moment_knm)
    for name in ('axial_kn', 'guide_height_m', 'elastic_modulus_kpa',
                 'second_moment_m4', 'guide_stiffness_knm_rad', 'axial_allowable_kn',
                 'moment_allowable_knm', 'shear_allowable_kn', 'guide_moment_allowable_knm',
                 'deflection_allowable_m', 'euler_load_kn'):
        finite(name, getattr(value, name),
               strict=name not in ('axial_kn', 'guide_stiffness_knm_rad'))
    if not value.lateral_loads:
        return _unresolved(('lateral loads; state zero actions explicitly',))
    loads = value.lateral_loads
    length = value.guide_height_m
    for force, height in loads:
        _number('lateral force', force)
        finite('load height', height)
        if height > length:
            raise ValueError('load height must not exceed guide height')
    ei = value.elastic_modulus_kpa * value.second_moment_m4
    stiffness = value.guide_stiffness_knm_rad
    theta_free = (sum(p * a**2 / 2 for p, a in loads)
                  + value.top_moment_knm * length) / ei
    rotation = theta_free / (1 + stiffness * length / ei)
    spring_moment = stiffness * rotation
    couple = value.top_moment_knm - spring_moment

    def moment(z: float) -> float:
        return sum(p * max(a - z, 0) for p, a in loads) + couple

    def deflection(z: float) -> float:
        point = sum(p * z**2 * (3 * a - z) if z <= a else p * a**2 * (3 * z - a)
                    for p, a in loads)
        return point / (6 * ei) + couple * z**2 / (2 * ei)

    positions = sorted({0.0, length, *(a for _, a in loads)})
    maximum_moment = max(abs(moment(z)) for z in positions)
    # Each interval's deflection slope is quadratic. Include its real roots
    # so a spring-induced internal deflection maximum is not missed.
    stationary = []
    for lo, hi in zip(positions, positions[1:]):
        mid = (lo + hi) / 2
        aa = -sum(p for p, a in loads if a >= mid) / 2
        bb = sum(p * a for p, a in loads if a >= mid) + couple
        cc = sum(p * a**2 for p, a in loads if a < mid) / 2
        roots: tuple[float, ...]
        if aa == 0:
            roots = (-cc / bb,) if bb != 0 else ()
        else:
            discriminant = bb**2 - 4 * aa * cc
            roots = ((-bb + math.sqrt(discriminant)) / (2 * aa),
                     (-bb - math.sqrt(discriminant)) / (2 * aa)) if discriminant >= 0 else ()
        stationary.extend(z for z in roots if lo < z < hi)
    max_deflection = max(abs(deflection(z)) for z in positions + stationary)
    # Shear changes at every point action, including possible opposing forces.
    max_shear = max(abs(sum(p for p, a in loads if a >= z))
                    for z in positions)
    stable = value.axial_kn < value.euler_load_kn
    checks = [Check('instability', 'met' if stable else 'not_met', value.axial_kn,
                    value.euler_load_kn, value.axial_kn / value.euler_load_kn,
                    value.euler_load_kn - value.axial_kn)]
    amplification = 1 / (1 - value.axial_kn / value.euler_load_kn) if stable else None
    if stable:
        if amplification is None:
            raise ValueError('amplification is required')
        checks.extend((
            _limit('axial_bending', value.axial_kn / value.axial_allowable_kn
                   + maximum_moment * amplification / value.moment_allowable_knm, 1),
            _limit('guide_connection', abs(spring_moment) * amplification,
                   value.guide_moment_allowable_knm),
            _limit('shear', max_shear * amplification, value.shear_allowable_kn),
            _limit('deflection', max_deflection * amplification, value.deflection_allowable_m),
        ))
    else:
        checks.extend(Check(name, 'unresolved', note='Elastic instability invalidates magnification')
                      for name in ('axial_bending', 'guide_connection', 'shear', 'deflection'))
    return Screen(tuple(checks), {
        'guide_moment_knm': spring_moment, 'guide_rotation_rad': rotation,
        'base_moment_knm': moment(0), 'first_order_max_moment_knm': maximum_moment,
        'first_order_guide_deflection_m': deflection(length),
        'first_order_max_deflection_m': max_deflection,
        'axial_kn': value.axial_kn,
        'foundation_shear_kn': sum(p for p, _ in loads),
        'first_order_max_shear_kn': max_shear, 'amplification': amplification,
    })


@dataclass(frozen=True)
class HydrostaticPoint:
    """Solid-weight intact GZ in metres at a heel angle in degrees."""

    heel_deg: float
    gz_m: float


@dataclass(frozen=True)
class StabilityCriteria:
    """Explicit minimum GM, positive range, area and area-ratio criteria.

    Area rows are (start_deg, end_deg, minimum_m_rad). Ratio rows are
    (numerator_start, numerator_end, denominator_start, denominator_end,
    minimum_ratio). Empty tuples explicitly mean no criterion of that family.
    """

    minimum_gm_m: float | None
    minimum_range_deg: float | None
    areas: tuple[tuple[float, float, float], ...] | None
    area_ratios: tuple[tuple[float, float, float, float, float], ...] | None


@dataclass(frozen=True)
class StabilityInput:
    """One raised-leg transit loading state; geometry is not reconstructed.

    GZ and GM are solid-weight values for the supplied configuration. Apply
    free-surface correction to both, exactly once. A heeling assessment record
    must establish whether these intact criteria cover the stated transit.
    """

    basis: Basis | None
    table: tuple[HydrostaticPoint, ...] | None
    gm_solid_m: float | None
    free_surface_correction_m: float | None
    downflooding_deg: float | None
    legs_raised: bool | None
    criteria: StabilityCriteria | None
    loading_state: str | None
    displacement_kn: float | None
    draft_m: float | None
    trim_deg: float | None
    leg_position_evidence: str | None
    geometry_evidence: str | None
    weight_evidence: str | None
    heeling_evidence: str | None
    damage_evidence: str | None


def screen_stability(value: StabilityInput) -> Screen:
    """Integrate piecewise linear corrected GZ without extrapolation.

    Correct GZ by FSC*sin(heel) and GM by FSC. The positive range ends at
    the first positive-to-nonpositive crossing; initial negative GZ gives zero
    range, even if a later lobe is positive. Downflooding truncates usable range
    and area coverage. Tabulated sampling cannot exclude an unseen crossing.
    """
    if missing := _missing(value):
        return _unresolved(missing)
    if value.basis is None:
        raise ValueError('value.basis is required')
    if value.table is None:
        raise ValueError('value.table is required')
    if value.gm_solid_m is None:
        raise ValueError('value.gm_solid_m is required')
    if value.free_surface_correction_m is None:
        raise ValueError('value.free_surface_correction_m is required')
    if value.downflooding_deg is None:
        raise ValueError('value.downflooding_deg is required')
    if value.legs_raised is None:
        raise ValueError('value.legs_raised is required')
    if value.criteria is None:
        raise ValueError('value.criteria is required')
    if value.loading_state is None:
        raise ValueError('value.loading_state is required')
    if value.displacement_kn is None:
        raise ValueError('value.displacement_kn is required')
    if value.draft_m is None:
        raise ValueError('value.draft_m is required')
    if value.trim_deg is None:
        raise ValueError('value.trim_deg is required')
    if value.leg_position_evidence is None:
        raise ValueError('value.leg_position_evidence is required')
    if value.geometry_evidence is None:
        raise ValueError('value.geometry_evidence is required')
    if value.weight_evidence is None:
        raise ValueError('value.weight_evidence is required')
    if value.heeling_evidence is None:
        raise ValueError('value.heeling_evidence is required')
    if value.damage_evidence is None:
        raise ValueError('value.damage_evidence is required')
    criteria = value.criteria
    if missing := _missing(criteria):
        return _unresolved(tuple('criteria.' + name for name in missing))
    if criteria.minimum_gm_m is None:
        raise ValueError('criteria.minimum_gm_m is required')
    if criteria.minimum_range_deg is None:
        raise ValueError('criteria.minimum_range_deg is required')
    if criteria.areas is None:
        raise ValueError('criteria.areas is required')
    if criteria.area_ratios is None:
        raise ValueError('criteria.area_ratios is required')
    if len(value.table) < 2:
        return _unresolved(('at least two hydrostatic points',))
    if not isinstance(value.legs_raised, bool):
        raise ValueError('legs_raised must be a Boolean')
    for name in ('gm_solid_m', 'trim_deg'):
        _number(name, getattr(value, name))
    for name in ('free_surface_correction_m', 'downflooding_deg', 'displacement_kn', 'draft_m'):
        finite(name, getattr(value, name), strict=name != 'free_surface_correction_m')
    for point in value.table:
        finite('heel angle', point.heel_deg)
        _number('GZ', point.gz_m)
        if point.heel_deg > 180:
            raise ValueError('heel angle must not exceed 180 degrees')
    if value.table[0].heel_deg != 0 or value.table[0].gz_m != 0:
        raise ValueError('symmetric intact table must start at zero heel and zero GZ')
    if any(b.heel_deg <= a.heel_deg for a, b in zip(value.table, value.table[1:])):
        raise ValueError('heel angles must be strictly increasing')
    finite('minimum GM', criteria.minimum_gm_m, strict=True)
    finite('minimum range', criteria.minimum_range_deg, strict=True)
    if criteria.minimum_range_deg > 180 or value.downflooding_deg > 180:
        raise ValueError('range and downflooding must not exceed 180 degrees')
    corrected = tuple((p.heel_deg, p.gz_m - value.free_surface_correction_m
                       * math.sin(math.radians(p.heel_deg))) for p in value.table)
    gm = value.gm_solid_m - value.free_surface_correction_m
    checks = [_limit('gm', gm, criteria.minimum_gm_m, minimum=True),
              Check('raised_legs', 'met' if value.legs_raised else 'not_met')]
    positive_end = None
    for left, right in zip(corrected, corrected[1:]):
        if right[1] <= 0:
            positive_end = (left[0] + left[1] / (left[1] - right[1]) * (right[0] - left[0])
                            if left[1] > 0 else left[0])
            break
    # A lower bound through the last positive ordinate can establish a minimum
    # range, but never supplies an extrapolated angle of vanishing stability.
    known_end = min(positive_end if positive_end is not None else corrected[-1][0],
                    value.downflooding_deg)
    range_check = _limit('positive_range', known_end, criteria.minimum_range_deg, minimum=True)
    if positive_end is None and corrected[-1][0] < min(
            criteria.minimum_range_deg, value.downflooding_deg):
        range_check = Check('positive_range', 'unresolved', note='GZ table ends before required range')
    checks.append(range_check)

    def area(start: float, end: float) -> float | None:
        finite('area start', start)
        finite('area end', end, strict=True)
        if start >= end or end > 180:
            raise ValueError('area bounds require 0 <= start < end <= 180')
        if end > min(corrected[-1][0], value.downflooding_deg):
            return None
        total = 0.0
        for (x0, y0), (x1, y1) in zip(corrected, corrected[1:]):
            lo, hi = max(start, x0), min(end, x1)
            if hi <= lo:
                continue
            low = y0 + (y1 - y0) * (lo - x0) / (x1 - x0)
            high = y0 + (y1 - y0) * (hi - x0) / (x1 - x0)
            total += (low + high) / 2 * math.radians(hi - lo)
        return total

    areas, ratios = [], []
    for index, (start, end, minimum) in enumerate(criteria.areas, 1):
        finite('minimum area', minimum, strict=True)
        actual = area(start, end)
        areas.append(actual)
        checks.append(Check(f'area_{index}', 'unresolved', note='Incomplete range or downflooding')
                      if actual is None else _limit(f'area_{index}', actual, minimum, minimum=True))
    for index, (a, b, c, d, minimum) in enumerate(criteria.area_ratios, 1):
        finite('minimum area ratio', minimum, strict=True)
        numerator, denominator = area(a, b), area(c, d)
        ratio = (numerator / denominator if numerator is not None and denominator is not None
                 and denominator > 0 else None)
        ratios.append(ratio)
        checks.append(Check(f'area_ratio_{index}', 'unresolved', note='Uncovered area or nonpositive denominator')
                      if ratio is None else _limit(f'area_ratio_{index}', ratio, minimum, minimum=True))
    return Screen(tuple(checks), {
        'loading_state': value.loading_state, 'displacement_kn': value.displacement_kn,
        'draft_m': value.draft_m, 'trim_deg': value.trim_deg,
        'gm_corrected_m': gm, 'corrected_gz_m': tuple(gz for _, gz in corrected),
        'positive_range_deg': positive_end, 'usable_range_lower_bound_deg': known_end,
        'areas_m_rad': tuple(areas), 'area_ratios': tuple(ratios),
    })


@dataclass(frozen=True)
class PreloadLoadCase:
    """One staged total weight/buoyancy balance at a recorded sample time.

    Total weight includes legs, hull and carried ballast. Centres are x/y in
    the common support frame. Moments include every action other than the
    explicitly supplied vertical weights and buoyancy.
    """

    stage: str | None
    time_h: float | None
    weight_kn: float | None
    buoyancy_kn: float | None
    weight_centre_m: tuple[float, float] | None
    buoyancy_centre_m: tuple[float, float] | None
    external_moments_knm: tuple[float, float] | None


@dataclass(frozen=True)
class PreloadPlan:
    """Staged equilibrium inputs; every per-leg sample needs a matching case."""

    basis: Basis | None
    supports_m: tuple[tuple[float, float], ...] | None
    cases: tuple[PreloadLoadCase, ...] | None
    reaction_tolerance_kn: float | None


def screen_preload_plan(plan: PreloadPlan, legs: Sequence[PreloadInput]) -> Screen:
    """Check stage weights and centres against every leg's recorded load.

    Three-point vertical support equilibrium is determinate. Unequal settlement,
    leg stiffness and foundation moments in a preload procedure need separate
    support evidence. A zero-reaction afloat preparation is permitted here.
    """
    if missing := _missing(plan):
        return _unresolved(missing)
    if plan.basis is None:
        raise ValueError('plan.basis is required')
    if plan.supports_m is None:
        raise ValueError('plan.supports_m is required')
    if plan.cases is None:
        raise ValueError('plan.cases is required')
    if plan.reaction_tolerance_kn is None:
        raise ValueError('plan.reaction_tolerance_kn is required')
    if len(legs) != 3 or len(plan.supports_m) != 3:
        raise ValueError('preload equilibrium requires exactly three legs')
    finite('reaction tolerance', plan.reaction_tolerance_kn, strict=True)
    for x, y in plan.supports_m:
        _number('support x', x)
        _number('support y', y)
    # Validate support geometry through the existing solver, even if all
    # supplied preparation loads happen to be zero.
    screen_three_leg_unit(vertical_load_kn=1, supports_m=plan.supports_m,
                          moment_x_knm=0, moment_y_knm=0, horizontal_force_kn=0,
                          preload_per_leg_kn=1, sliding_resistance_kn=0)
    if len({leg.leg for leg in legs}) != 3 or any(leg.leg is None for leg in legs):
        return _unresolved(('three distinct ordered leg identifiers',))
    if any(leg.stages is None for leg in legs):
        return _unresolved(('per-leg stage histories',))
    if any(leg.basis is None or leg.basis.configuration != plan.basis.configuration for leg in legs):
        return _unresolved(('preload configuration agreement',))
    samples: list[dict[tuple[str, float], float]] = []
    for leg in legs:
        if leg.stages is None:
            raise ValueError('leg.stages is required')
        if tuple(stage.name for stage in leg.stages) != ('preparation', 'preload', 'holding', 'discharge'):
            raise ValueError('preload equilibrium needs four ordered stages')
        previous = -1.0
        for stage in leg.stages:
            if not stage.samples:
                return _unresolved(('nonempty samples in every equilibrium stage',))
            for sample in stage.samples:
                if missing := _missing(sample):
                    return _unresolved(tuple('stage sample.' + name for name in missing))
                finite('stage sample time', sample.time_h)
                finite('stage sample load', sample.applied_load_kn)
                if sample.time_h <= previous:
                    raise ValueError('preload sample times must strictly increase')
                previous = sample.time_h
        samples.append({(stage.name, sample.time_h): sample.applied_load_kn
                        for stage in leg.stages for sample in stage.samples})
    if any(not sample for sample in samples):
        return _unresolved(('nonempty staged loads',))
    required = set().union(*(sample.keys() for sample in samples))
    keys = [(case.stage, case.time_h) for case in plan.cases]
    if len(set(keys)) != len(keys):
        raise ValueError('preload equilibrium cases must be distinct')
    if set(keys) != required or any(set(sample) != required for sample in samples):
        return _unresolved(('one equilibrium case for every common staged sample',))
    checks, reactions = [], []
    for index, case in enumerate(plan.cases, 1):
        if missing := _missing(case):
            checks.append(Check(f'stage_{index}', 'unresolved', note='Missing: ' + ', '.join(missing)))
            continue
        if case.stage is None:
            raise ValueError('case.stage is required')
        if case.time_h is None:
            raise ValueError('case.time_h is required')
        if case.weight_kn is None:
            raise ValueError('case.weight_kn is required')
        if case.buoyancy_kn is None:
            raise ValueError('case.buoyancy_kn is required')
        if case.weight_centre_m is None:
            raise ValueError('case.weight_centre_m is required')
        if case.buoyancy_centre_m is None:
            raise ValueError('case.buoyancy_centre_m is required')
        if case.external_moments_knm is None:
            raise ValueError('case.external_moments_knm is required')
        finite('stage time', case.time_h)
        finite('stage weight', case.weight_kn)
        finite('stage buoyancy', case.buoyancy_kn)
        for pair in (case.weight_centre_m, case.buoyancy_centre_m, case.external_moments_knm):
            if len(pair) != 2:
                raise ValueError('stage centres and moments need two coordinates')
            for number in pair:
                _number('stage coordinate or moment', number)
        vertical = case.weight_kn - case.buoyancy_kn
        mx = (case.external_moments_knm[0] - case.weight_kn * case.weight_centre_m[1]
              + case.buoyancy_kn * case.buoyancy_centre_m[1])
        my = (case.external_moments_knm[1] + case.weight_kn * case.weight_centre_m[0]
              - case.buoyancy_kn * case.buoyancy_centre_m[0])
        actual = tuple(sample[(case.stage, case.time_h)] for sample in samples)
        for force in actual:
            finite('stage leg reaction', force)
        if vertical > 0:
            expected = screen_three_leg_unit(
                vertical_load_kn=vertical, supports_m=plan.supports_m,
                moment_x_knm=mx, moment_y_knm=my, horizontal_force_kn=0,
                preload_per_leg_kn=1, sliding_resistance_kn=0).reactions_kn
            for leg_index, (observed, predicted) in enumerate(zip(actual, expected), 1):
                checks.append(_limit(f'stage_{index}.leg_{leg_index}.equilibrium',
                                     abs(observed - predicted), plan.reaction_tolerance_kn))
        elif vertical == 0 and mx == 0 and my == 0:
            checks.append(_limit(f'stage_{index}.afloat', max(actual), plan.reaction_tolerance_kn))
        else:
            checks.append(Check(f'stage_{index}.equilibrium', 'not_met',
                                note='No compression-only vertical equilibrium'))
        reactions.append((case.stage, case.time_h, actual))
    return Screen(tuple(checks), {'stage_reactions_kn': tuple(reactions)})


def screen_liftboat(
    *, preloads: tuple[PreloadInput, ...], preload_plan: PreloadPlan,
    storm: StormInput, bending_templates: tuple[BendingInput, ...], transit: StabilityInput,
    interface_evidence: str | None, transit_weight_change_kn: float | None,
    interface_weight_tolerance_kn: float | None, adjacent_evidence: Mapping[str, str | None],
) -> Screen:
    """Carry preload history, storm reactions and beam restraints across phases.

    Evaluate once per named directional environmental and transit loading case.
    The caller must supply evidence of complete case and uncertainty coverage.
    Resistance remains NEXT and prevents overall approval. Evidence records
    describe separate assessments; this program does not validate their content.
    """
    if len(preloads) != 3 or len(bending_templates) != 3:
        raise ValueError('integrated screening requires three ordered legs')
    plan_result = screen_preload_plan(preload_plan, preloads)
    initial_preloads = tuple(screen_preload(leg) for leg in preloads)
    transit_result = screen_stability(transit)
    checks = [Check('configuration_link', 'met' if interface_evidence and interface_evidence.strip()
                    else 'unresolved', note='Elevated and transit weight/configuration reconciliation')]
    if (transit_weight_change_kn is None or interface_weight_tolerance_kn is None
            or transit.displacement_kn is None or storm.vertical_load_kn is None):
        checks.append(Check('phase_weight_balance', 'unresolved', note='Missing phase weight reconciliation'))
    else:
        _number('transit weight change', transit_weight_change_kn)
        finite('interface weight tolerance', interface_weight_tolerance_kn, strict=True)
        checks.append(_limit('phase_weight_balance',
                             abs(transit.displacement_kn - storm.vertical_load_kn - transit_weight_change_kn),
                             interface_weight_tolerance_kn))
    configurations = [p.basis.configuration if p.basis else None for p in preloads]
    configurations += [b.basis.configuration if b.basis else None for b in bending_templates]
    config = storm.basis.configuration if storm.basis else None
    consistent = bool(config) and all(name == config for name in configurations)
    checks.append(Check('elevated_configuration', 'met' if consistent else 'unresolved'))
    layout_matches = preload_plan.supports_m == storm.supports_m and storm.supports_m is not None
    checks.append(Check('support_layout', 'met' if layout_matches else 'unresolved'))
    # Only a fully resolved per-leg preload screen supplies a proven target.
    proven: tuple[float, ...] | None = None
    if all(p.status == 'met' for p in initial_preloads) and plan_result.status == 'met':
        targets = []
        for leg in preloads:
            if leg.target_kn is None:
                raise ValueError('leg.target_kn is required')
            targets.append(leg.target_kn)
        proven = tuple(targets)
    preliminary = screen_storm(replace(storm, preload_proven_kn=proven,
                                       foundation_moments_knm=((0, 0),) * 3))
    bending_results: list[Screen] = []
    foundation_moments = None
    final_storm = _unresolved(('coupled foundation/leg response',))
    final_preloads = initial_preloads
    if preliminary.metrics and consistent and layout_matches:
        if storm.strips_by_leg is None:
            raise ValueError('storm.strips_by_leg is required')
        if storm.water_density_kg_m3 is None:
            raise ValueError('storm.water_density_kg_m3 is required')
        if storm.load_factor is None:
            raise ValueError('storm.load_factor is required')
        if storm.horizontal_shares is None:
            raise ValueError('storm.horizontal_shares is required')
        if storm.heading_deg is None:
            raise ValueError('storm.heading_deg is required')
        candidate_templates = []
        for index, template in enumerate(bending_templates):
            if template.guide_height_m is None or template.guide_height_m != storm.hull_underside_m:
                bending_results.append(_unresolved(('guide elevation must match storm hull underside',)))
                continue
            length = template.guide_height_m
            water_loads = []
            for strip in storm.strips_by_leg[index]:
                force = storm.water_density_kg_m3 * strip.length_m / 1000 * storm.load_factor * (
                    0.5 * strip.drag_coefficient * strip.diameter_m * strip.velocity_m_s
                    * abs(strip.velocity_m_s) + strip.inertia_coefficient * math.pi
                    * strip.diameter_m**2 / 4 * strip.acceleration_m_s2)
                water_loads.append((force, strip.z_m))
            wind_force = preliminary.metrics['wind_force_kn'] * storm.horizontal_shares[index]
            wind_moment = preliminary.metrics['wind_moment_knm'] * storm.horizontal_shares[index]
            candidate = replace(template, axial_kn=0,
                                lateral_loads=(*water_loads, (wind_force, length)),
                                top_moment_knm=wind_moment - wind_force * length)
            candidate_templates.append(candidate)
            bending_results.append(screen_bending(candidate))
        if len(candidate_templates) == 3 and all(result.metrics for result in bending_results):
            heading = math.radians(storm.heading_deg)
            foundation_moments = tuple(
                (-result.metrics['base_moment_knm'] * math.sin(heading),
                 result.metrics['base_moment_knm'] * math.cos(heading))
                for result in bending_results)
            final_storm = screen_storm(replace(storm, preload_proven_kn=proven,
                                                foundation_moments_knm=foundation_moments))
            reactions = final_storm.metrics['reactions_kn']
            bending_results = [screen_bending(replace(template, axial_kn=reaction))
                               if reaction > 0 else _unresolved(('support lost contact',))
                               for template, reaction in zip(candidate_templates, reactions)]
            updated_preloads = []
            for leg, reaction in zip(preloads, reactions):
                if leg.governing_load_kn is None:
                    raise ValueError('leg.governing_load_kn is required')
                updated_preloads.append(screen_preload(replace(
                    leg, governing_load_kn=max(leg.governing_load_kn, reaction))))
            final_preloads = tuple(updated_preloads)
    if not bending_results:
        bending_results = [_unresolved(('preload, storm and support interfaces',)) for _ in preloads]
    for name in ('leg_installation_removal', 'jacking_guides', 'risk_assessment',
                 'transit_motion_tow', 'storm_case_coverage', 'transit_loading_coverage',
                 'uncertainty_coverage', 'foundation_support_compatibility', 'preload_lateral_stability'):
        evidence = adjacent_evidence.get(name)
        checks.append(Check(name, 'met' if evidence and evidence.strip() else 'unresolved',
                            note='Separate assessment record required'))
    checks.append(Check('resistance', 'unresolved', note='NEXT: calm-water and added resistance, propulsion/tow margin'))
    screens = {'preload_plan': plan_result, 'storm': final_storm, 'transit': transit_result}
    screens.update({f'preload_{i}': result for i, result in enumerate(final_preloads, 1)})
    screens.update({f'bending_{i}': result for i, result in enumerate(bending_results, 1)})
    for name, screen in screens.items():
        checks.extend(replace(check, name=name + '.' + check.name) for check in screen.checks)
    return Screen(tuple(checks), {
        'preload_plan': plan_result, 'preloads': final_preloads, 'storm': final_storm,
        'bending': tuple(bending_results), 'transit': transit_result,
        'foundation_moments_knm': foundation_moments,
        'governing_check': max((c for c in checks if c.utilization is not None),
                              key=lambda c: c.utilization if c.utilization is not None else -math.inf, default=None),
    })
