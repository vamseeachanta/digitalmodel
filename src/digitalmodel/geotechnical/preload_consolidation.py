# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""One-dimensional primary consolidation during a spudcan preload hold.

MIT 1.361 (2004), Part V-2, sections 1.1-1.2, pages 2-3 supplies the
constant-property diffusion equation, Tv=cv*t/Hd**2 and s=U*s_final:
https://ocw.mit.edu/courses/1-361-advanced-soil-mechanics-fall-2004/resources/part_v_2/
The Fourier coefficients below follow by integrating its uniform initial pore
pressure over a single-drained slab; see the derivation in the companion note.
FHWA NHI-06-088 (2006), sections 5.5.6.1 and 7.5.3, equation 7-8 and
selected checks in Table 7-4 support normalization and NC strength su=S*sigma':
https://www.fhwa.dot.gov/engineering/geotech/pubs/nhi06088.pdf

This equivalent clay slab is a sensitivity model. Constant mv, cv and S,
uniform stress increment and unchanged geometry are assumptions. No radial
drainage, creep, remoulding, stress-history effects or large deformation is
modelled. SNAME 5-5A Rev 3, holding period, clause: TO CONFIRM.
ISO 19905-1:2023, preload holding criterion, clause: TO CONFIRM.
Neither standard supplies a numerical acceptance criterion in this module.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, fields

from digitalmodel.geotechnical.spudcan import clay_resistance_kpa, finite
from digitalmodel.marine_ops.preload_sequence import PreloadCriteria


@dataclass(frozen=True)
class ClayConsolidation:
    """Normally consolidated equivalent slab; metres, hours and kN.

    mv is the constant volume compressibility in inverse kPa (m²/kN).
    Hd equals thickness for single drainage or half thickness for double
    drainage. S=su/sigma' is a supplied normalized strength ratio, not a
    default correlation or a model for overconsolidated material.
    """

    thickness_m: float
    drainage_path_m: float
    cv_m2_h: float
    mv_m2_kn: float
    initial_effective_stress_kpa: float
    normalized_strength_ratio: float

    def validate(self) -> None:
        """Require positive properties and one of the two slab boundaries."""
        for item in fields(self):
            finite(item.name, getattr(self, item.name), strict=True)
        if self.drainage_path_m not in (self.thickness_m, self.thickness_m / 2):
            raise ValueError('drainage path must be thickness or half thickness')


@dataclass(frozen=True)
class ConsolidationPreload:
    """Held per-leg load acting on an equivalent constant-area clay foundation.

    The influence factor maps footing pressure to uniform slab stress. It
    requires independent qualification; no stress-spreading law is implied.
    Initial penetration supplies the existing sequence screen's depth datum;
    consolidation settlement is additional movement from the unloaded datum.
    """

    clay: ClayConsolidation
    area_m2: float
    target_load_kn: float
    stress_influence_factor: float
    initial_penetration_m: float = 0.0

    def validate(self) -> None:
        """Check finite loads and a declared small-strain model domain."""
        self.clay.validate()
        finite('area', self.area_m2, strict=True)
        finite('target load', self.target_load_kn, strict=True)
        finite('stress influence', self.stress_influence_factor, strict=True)
        finite('initial penetration', self.initial_penetration_m)
        if self.stress_influence_factor > 1:
            raise ValueError('stress influence must not exceed one')
        strain = self.clay.mv_m2_kn * self.stress_increment_kpa
        finite('ultimate strain', strain)
        if strain > 0.1:
            raise ValueError('ultimate strain exceeds assumed small-strain limit of 0.1')
        finite('time scale', self.clay.cv_m2_h / self.clay.drainage_path_m**2,
               strict=True)

    @property
    def stress_increment_kpa(self) -> float:
        """Return the assumed mean total vertical stress increment in clay."""
        return self.stress_influence_factor * self.target_load_kn / self.area_m2


@dataclass(frozen=True)
class HoldingPoint:
    """State at full load, t hours after its first attainment.

    Settlement includes consolidation during the loading ramp. Capacity is
    the existing uniform-clay circular reference evaluated with gained su;
    margin_ratio=capacity/target-1. Negative margins invalidate the assumed
    stable held-load state; waiting is not a remedy for unstable loading.
    """

    holding_time_h: float
    degree_of_consolidation: float
    settlement_m: float
    undrained_strength_kpa: float
    capacity_kn: float
    margin_ratio: float


@dataclass(frozen=True)
class HoldingRecommendation:
    """First complete settled window within a finite search horizon.

    The rate criterion is strictly below the supplied limit and the full-load
    floor is inclusive, as in PreloadCriteria. This is a numerical planning
    recommendation, independent of the capacity margin or installation approval.
    """

    holding_time_h: float | None
    settlement_rate_m_h: float | None
    reason: str
    criteria: PreloadCriteria


def degree_of_consolidation(time_factor: float) -> float:
    """Return average U for uniform initial excess pressure in a clay slab.

    U=1-sum[8/(pi²*j²)*exp(-pi²*j²*Tv/4)], odd positive j.
    For Tv<=0.01, use U=2*sqrt(Tv/pi); its image correction is below
    exp(-100). Above this crossover, 64 modes leave less than 1e-180.
    """
    finite('time factor', time_factor)
    if time_factor <= 0.01:
        return 2 * math.sqrt(time_factor / math.pi)
    return 1 - math.fsum(8 / (math.pi**2 * j*j) *
                         math.exp(-math.pi**2 * j*j / 4 * time_factor)
                         for j in range(1, 128, 2))


def _early_ramp_degree(start_tv: float, end_tv: float) -> float:
    # Rationalized difference of cubes avoids loss for very short ramps.
    a, b = math.sqrt(end_tv), math.sqrt(start_tv)
    return 4 / (3 * math.sqrt(math.pi)) * (a*a + a*b + b*b) / (a+b)


def _ramp_degree(hold_tv: float, ramp_tv: float) -> float:
    # Duhamel superposition for a linear rise from zero to target load:
    # Ubar(h,r) = integral[U(v), v=h..h+r]/r. r=0 is the rapid step.
    if ramp_tv == 0:
        return degree_of_consolidation(hold_tv)
    if hold_tv + ramp_tv <= 0.01:
        return _early_ramp_degree(hold_tv, hold_tv + ramp_tv)
    if hold_tv < 0.01:
        # Split a crossing ramp into weighted means. Subtraction of nearly
        # equal antiderivatives would corrupt a tiny ramp about the boundary.
        early_width = 0.01 - hold_tv
        late_width = ramp_tv - early_width
        return (early_width / ramp_tv * _early_ramp_degree(hold_tv, 0.01) +
                late_width / ramp_tv * _ramp_degree(0.01, late_width))
    return 1 - math.fsum(
        8 / (math.pi**2 * j*j) * math.exp(-math.pi**2 * j*j / 4 * hold_tv) *
        (-math.expm1(-math.pi**2 * j*j / 4 * ramp_tv) /
         (math.pi**2 * j*j / 4 * ramp_tv))
        for j in range(1, 128, 2))


def holding_response(model: ConsolidationPreload, holding_time_h: float, *,
                     ramp_duration_h: float = 0.0) -> HoldingPoint:
    """Calculate settlement and gained undrained capacity under held load.

    Zero ramp duration is the ideal rapid, undrained application: at hold
    start, U=0 and capacity is exactly the original undrained capacity. A
    positive ramp duration includes partial drainage during linear loading.
    All cases subsequently use undrained capacity at their current gained su;
    none substitutes a drained bearing-capacity factor.
    """
    model.validate()
    finite('holding time', holding_time_h)
    finite('ramp duration', ramp_duration_h)
    clay = model.clay
    scale = clay.cv_m2_h / clay.drainage_path_m**2
    hold_tv, ramp_tv = holding_time_h * scale, ramp_duration_h * scale
    finite('holding time factor', hold_tv)
    finite('ramp time factor', ramp_tv)
    degree = min(1.0, max(0.0, _ramp_degree(hold_tv, ramp_tv)))
    effective_gain = model.stress_increment_kpa * degree
    settlement = clay.mv_m2_kn * clay.thickness_m * effective_gain
    strength = clay.normalized_strength_ratio * (
        clay.initial_effective_stress_kpa + effective_gain)
    diameter = math.sqrt(4 * model.area_m2 / math.pi)
    capacity = clay_resistance_kpa(strength, 0, diameter) * model.area_m2
    margin = capacity / model.target_load_kn - 1
    for name, value in (('settlement', settlement), ('strength', strength),
                        ('capacity', capacity)):
        finite(name, value)
    if not math.isfinite(margin):
        raise ValueError('capacity margin is nonfinite')
    return HoldingPoint(holding_time_h, degree, settlement, strength, capacity, margin)


def recommend_minimum_hold(
    model: ConsolidationPreload, *, ramp_duration_h: float = 0.0,
    criteria: PreloadCriteria = PreloadCriteria(), max_hold_h: float = 87600.0,
) -> HoldingRecommendation:
    """Find the earliest strict trailing-window rate pass under the hold floor.

    Default 0.001 m/h over 0.25 h and minimum one hour are inherited
    illustrative controls, assumed, based on previous experience. Their source
    is marine_ops.preload_sequence.PreloadCriteria, not a standard requirement.
    SNAME 5-5A Rev 3, holding period, clause: TO CONFIRM.
    ISO 19905-1:2023, preload holding criterion, clause: TO CONFIRM.
    Window rates decrease monotonically for this constant positive load model.
    A finite upper horizon prevents an unbounded search; None means unresolved.
    """
    criteria.validate()
    finite('maximum hold', max_hold_h, strict=True)
    holding_response(model, 0, ramp_duration_h=ramp_duration_h)
    window = criteria.settlement_window_h
    lower = max(criteria.minimum_hold_h, window)

    def rate(time: float) -> float:
        end = holding_response(model, time, ramp_duration_h=ramp_duration_h)
        start = holding_response(model, time - window, ramp_duration_h=ramp_duration_h)
        return (end.settlement_m - start.settlement_m) / window

    if lower > max_hold_h or rate(max_hold_h) >= criteria.settlement_rate_limit_m_h:
        return HoldingRecommendation(None, None, 'criterion_not_reached_within_horizon', criteria)
    if rate(lower) < criteria.settlement_rate_limit_m_h:
        return HoldingRecommendation(lower, rate(lower), 'criterion_reached', criteria)
    upper = max_hold_h
    for _ in range(80):
        middle = (lower + upper) / 2
        if middle == lower or middle == upper:
            break
        if rate(middle) < criteria.settlement_rate_limit_m_h:
            upper = middle
        else:
            lower = middle
    return HoldingRecommendation(upper, rate(upper), 'criterion_reached', criteria)
