"""Pipe bending capacity and parametric out-of-roundness limits in SI units.

Method transcribed from a legacy engineering workbook:
    r = D/t; beta = max((r_limit-r)/r_scale, 0)
    alpha = (1-beta) + beta*fu/fy
    Mp = (D-t)**2 * t * SMYS; Mc = Mp*alpha/gamma_m/gamma_sc

The existing plastic_moment_capacity kernel is reused. SMYS for Mp and fy/fu
for alpha are independent inputs: no material reduction is inferred. These
are capacity components, not a combined-loading or code compliance assessment.

Roundness is parameterized: below the first diameter threshold it is included
in diameter tolerance; in the next interval it is fraction_small*D; in the
next interval it is min(fraction_large*D, cap); at/above the final threshold
agreement is required. Ovality is out-of-roundness/D. No standard table is
embedded. Hydrostatic gauge pressure is rho*g*h, with caller-supplied gravity.

Source references: DNV-OS-F101 (clause not stated in source); API SPEC 5L
44th Ed (2007) Line Pipe and API-5L (clause not stated in source).
No clause or equation number is inferred.

Input gaps: formula-specific clauses; applicability of the bending correction;
beta threshold/divisor; tolerance interval boundaries, fractions and cap;
material strengths, resistance factors, density and gravity. All are required
arguments without engineering defaults. The caller shall qualify coefficients
and the diameter/thickness range for the intended use. Validation enforces
positive finite inputs, a hollow circular section and fu >= fy only; it does
not establish standard applicability. The thin-wall moment approximation is
retained, rather than replaced by an exact annulus section modulus.

Left outside this method: pipe-size tables, pipe registers, model
planning/time estimates, mesh design, material nonlinear behavior, and
interaction checks for tension and pressure. Exact inch/metre and mm/metre
conversions are provided; approximate display conversions are not retained.
"""

from dataclasses import dataclass, fields
from math import isfinite
from typing import Literal

from digitalmodel.code_checks.dnv_os_f101 import plastic_moment_capacity
from digitalmodel.hydrostatics import fluid_column
from digitalmodel.units import Q_


def _positive(name: str, value: float) -> float:
    if not isfinite(value) or value <= 0:
        raise ValueError(f"{name} must be finite and positive")
    return value


def _nonnegative(name: str, value: float) -> float:
    if not isfinite(value) or value < 0:
        raise ValueError(f"{name} must be finite and nonnegative")
    return value


@dataclass(frozen=True)
class BendingInputs:
    """SI geometry/strength inputs and dimensionless resistance factors."""

    outer_diameter_m: float
    wall_thickness_m: float
    specified_yield_strength_pa: float
    yield_strength_pa: float
    tensile_strength_pa: float
    material_factor_fraction: float
    safety_class_factor_fraction: float


@dataclass(frozen=True)
class BendingRule:
    """Required dimensionless coefficients; source applicability is unresolved."""

    beta_threshold_ratio: float
    beta_divisor_ratio: float


@dataclass(frozen=True)
class BendingCapacity:
    """Moment capacities in N.m; ratio, beta and alpha are dimensionless."""

    diameter_thickness_ratio: float
    beta_fraction: float
    alpha_fraction: float
    plastic_moment_nm: float
    design_moment_nm: float


def diameter_thickness_ratio(outer_diameter_m: float, wall_thickness_m: float) -> float:
    """Return D/t for a hollow circular section (D > 2*t > 0)."""
    _positive("outer_diameter_m", outer_diameter_m)
    _positive("wall_thickness_m", wall_thickness_m)
    if wall_thickness_m >= outer_diameter_m / 2:
        raise ValueError("wall thickness must be less than half the diameter")
    return _positive("diameter/thickness", outer_diameter_m / wall_thickness_m)


def hardening_beta(diameter_thickness_ratio: float, rule: BendingRule) -> float:
    """Return max((threshold - D/t)/divisor, 0); clause not stated in source.

    The result is not capped; the caller shall qualify the D/t range.
    """
    _positive("diameter_thickness_ratio", diameter_thickness_ratio)
    for field in fields(rule):
        _positive(field.name, getattr(rule, field.name))
    beta = max((rule.beta_threshold_ratio - diameter_thickness_ratio)
               / rule.beta_divisor_ratio, 0.0)
    return _nonnegative("beta", beta)


def strength_alpha(beta_fraction: float, yield_strength_pa: float,
                   tensile_strength_pa: float) -> float:
    """Return (1-beta)+beta*fu/fy; clause not stated in source."""
    _nonnegative("beta_fraction", beta_fraction)
    _positive("yield_strength_pa", yield_strength_pa)
    _positive("tensile_strength_pa", tensile_strength_pa)
    if tensile_strength_pa < yield_strength_pa:
        raise ValueError("tensile strength must be at least yield strength")
    alpha = (1 - beta_fraction) + beta_fraction * (tensile_strength_pa / yield_strength_pa)
    return _positive("alpha", alpha)


def bending_capacity(inputs: BendingInputs, rule: BendingRule) -> BendingCapacity:
    """Compute Mp and Mc in N.m using supplied factors; clause not stated in source."""
    for field in fields(inputs):
        _positive(field.name, getattr(inputs, field.name))
    ratio = diameter_thickness_ratio(inputs.outer_diameter_m, inputs.wall_thickness_m)
    beta = hardening_beta(ratio, rule)
    alpha = strength_alpha(beta, inputs.yield_strength_pa, inputs.tensile_strength_pa)
    try:
        moment = plastic_moment_capacity(inputs.outer_diameter_m,
                                        inputs.wall_thickness_m,
                                        inputs.specified_yield_strength_pa)
        design = moment * alpha / inputs.material_factor_fraction / inputs.safety_class_factor_fraction
    except OverflowError as exc:
        raise ValueError("derived capacity must be finite") from exc
    _positive("plastic_moment_nm", moment)
    _positive("design_moment_nm", design)
    return BendingCapacity(ratio, beta, alpha, moment, design)


@dataclass(frozen=True)
class RoundnessRule:
    """Three diameter intervals (m), fractional tolerances and absolute cap (m)."""

    included_below_m: float
    large_diameter_start_m: float
    agreement_start_m: float
    small_diameter_fraction: float
    large_diameter_fraction: float
    large_diameter_cap_m: float


@dataclass(frozen=True)
class RoundnessLimit:
    """None means no numerical tolerance has been established, not zero."""

    status: Literal["calculated", "included_in_diameter", "agreement_required"]
    out_of_roundness_m: float | None
    ovality_fraction: float | None


def roundness_limit(outer_diameter_m: float, rule: RoundnessRule) -> RoundnessLimit:
    """Evaluate the piecewise tolerance rule; clause not stated in source."""
    _positive("outer_diameter_m", outer_diameter_m)
    for field in fields(rule):
        _positive(field.name, getattr(rule, field.name))
    if not rule.included_below_m < rule.large_diameter_start_m < rule.agreement_start_m:
        raise ValueError("diameter thresholds must be strictly increasing")
    if outer_diameter_m < rule.included_below_m:
        return RoundnessLimit("included_in_diameter", None, None)
    if outer_diameter_m >= rule.agreement_start_m:
        return RoundnessLimit("agreement_required", None, None)
    if outer_diameter_m < rule.large_diameter_start_m:
        tolerance = rule.small_diameter_fraction * outer_diameter_m
    else:
        tolerance = min(rule.large_diameter_fraction * outer_diameter_m,
                        rule.large_diameter_cap_m)
    _positive("out_of_roundness_m", tolerance)
    ovality = _positive("ovality_fraction", tolerance / outer_diameter_m)
    return RoundnessLimit("calculated", tolerance, ovality)


def hydrostatic_pressure_pa(density_kg_per_m3: float, gravity_m_per_s2: float,
                            depth_m: float) -> float:
    """Return gauge pressure rho*g*h in Pa; no standard clause is stated."""
    _positive("density_kg_per_m3", density_kg_per_m3)
    _positive("gravity_m_per_s2", gravity_m_per_s2)
    _nonnegative("depth_m", depth_m)
    pressure = fluid_column.hydrostatic_pressure(density_kg_per_m3, depth_m,
                                                gravity=gravity_m_per_s2)
    return _nonnegative("pressure_pa", pressure)


def inches_to_metres(length_in: float) -> float:
    """Convert nonnegative length with the exact definition 1 inch = 0.0254 m."""
    _nonnegative("length_in", length_in)
    return _nonnegative("length_m", Q_(length_in, "inch").to("m").magnitude)


def metres_to_inches(length_m: float) -> float:
    """Convert nonnegative metres to inches using the exact inch definition."""
    _nonnegative("length_m", length_m)
    return _nonnegative("length_in", Q_(length_m, "m").to("inch").magnitude)


def millimetres_to_metres(length_mm: float) -> float:
    """Convert nonnegative length with 1 mm = 0.001 m."""
    _nonnegative("length_mm", length_mm)
    return _nonnegative("length_m", Q_(length_mm, "mm").to("m").magnitude)
