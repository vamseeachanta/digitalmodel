"""Cylinder pressure limits and algebraic inverse thickness/stress demands.

Basis: a legacy engineering workbook, API 579, ``Eqn 2C.9`` through
``Eqn 2C.20``. The edition is not stated. API RP 16Q is also named:
clause not stated in source. These are equation calculations, not a complete
fitness-for-service assessment or a service decision.

Inputs and results use metres, pascals and dimensionless ratios. The existing
ASMEThicknessCalc owns rational hoop thickness/capacity. Its mm/MPa boundary
uses digitalmodel.units.ureg. No alternative unit converter is implemented.

Let r = inner radius, t = available wall, u = supplemental thickness,
h = t-u, p = pressure, S = allowable stress, E = joint efficiency, and k =
caller-supplied longitudinal pressure coefficient. The pressure equations are:

* rational hoop: p = S*E*t/(r + c*t), via the existing implementation;
* logarithmic hoop: p = S*E*ln(1+t/r), ``Eqn 2C.13``;
* rational longitudinal: p = 2*S*E*h/(r-k*h), ``Eqn 2C.16``;
* thick longitudinal: p = S*E*((1+h/r)**2-1), ``Eqn 2C.19``.

Thickness and stress demands are algebraic inverses of those equations.
``Eqn 2C.12`` / ``Eqn 2C.18`` label the thick thickness demands;
``Eqn 2C.11``, ``Eqn 2C.14``, ``Eqn 2C.17``, ``Eqn 2C.20`` label
stress demands. In the source, two hoop stress formulas use allowable stress
as the pressure numerator; the rational longitudinal thickness uses allowable
stress instead of pressure. Those inconsistent references are not reproduced.
The well-formed local pressure branches select rational at equality. The
malformed general thickness IF expressions are not used.

Input gaps: edition and governing applicability; qualified material stress,
joint efficiency and supplemental-load thickness; longitudinal coefficient
and branch transition ratios; API RP 16Q clause. All such numerical inputs
are required, without defaults. Governing citations must be qualified and
wired upstream by the caller; no verified standards constants are supplied.

Limits: positive radius/wall/stress, nonnegative pressure/supplement, E in
(0,1], h>0, positive rational denominators. No temperature correction,
external pressure, fatigue, fracture, flaw interaction, section properties
or combined-load acceptance is established. Both equation families are
reported for auditing; branch selection is explicit and caller controlled.
"""

from __future__ import annotations

import math
from dataclasses import dataclass

from digitalmodel.ansys.pressure_vessel import ASMEThicknessCalc
from digitalmodel.units import ureg


def _finite(name: str, value: float, *, positive: bool) -> None:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f'{name} must be a finite number')
    if not math.isfinite(value) or value < 0 or (positive and value == 0):
        raise ValueError(f'{name} must be finite and ' + ('positive' if positive else 'nonnegative'))


@dataclass(frozen=True)
class CylinderPressureInputs:
    """Available wall and actual pressure demand; inputs are SI and explicit."""

    inner_radius_m: float
    wall_thickness_m: float
    pressure_pa: float
    allowable_stress_pa: float
    joint_efficiency_ratio: float
    supplemental_thickness_m: float
    longitudinal_pressure_coefficient_ratio: float

    def __post_init__(self) -> None:
        for name in ('inner_radius_m', 'wall_thickness_m', 'allowable_stress_pa',
                     'joint_efficiency_ratio', 'longitudinal_pressure_coefficient_ratio'):
            _finite(name, getattr(self, name), positive=True)
        for name in ('pressure_pa', 'supplemental_thickness_m'):
            _finite(name, getattr(self, name), positive=False)
        if self.joint_efficiency_ratio > 1:
            raise ValueError('joint_efficiency_ratio must not exceed 1')
        if self.supplemental_thickness_m >= self.wall_thickness_m:
            raise ValueError('supplemental thickness consumes the available wall')
        if self.allowable_stress_pa * self.joint_efficiency_ratio == 0:
            raise ValueError('factored allowable stress underflows to zero')


@dataclass(frozen=True)
class PressureEquationResult:
    """Capacity at available wall and demands at the specified pressure."""

    rational_required_thickness_m: float
    rational_pressure_limit_pa: float
    rational_required_allowable_stress_pa: float
    thick_required_thickness_m: float
    thick_pressure_limit_pa: float
    thick_required_allowable_stress_pa: float

    def __post_init__(self) -> None:
        if any(not math.isfinite(value) or value < 0 for value in self.__dict__.values()):
            raise ValueError('pressure equation result is nonfinite or negative')


@dataclass(frozen=True)
class SelectedPressureLimits:
    """Selected directional capacities; no acceptance decision is supplied."""

    hoop_pressure_limit_pa: float
    longitudinal_pressure_limit_pa: float


def hoop_pressure_limits(inputs: CylinderPressureInputs) -> PressureEquationResult:
    """Calculate the hoop family, API 579 ``Eqn 2C.9``–``Eqn 2C.14``.

    Rational thickness/capacity delegate to ASMEThicknessCalc. The required
    stress scales S by p/capacity because capacity is linear in S. Logarithmic
    thickness is r*expm1(p/(S*E)); stress is p/(E*log1p(t/r)).
    """
    x = inputs
    r_mm = ureg.Quantity(x.inner_radius_m, 'm').to('mm').magnitude
    t_mm = ureg.Quantity(x.wall_thickness_m, 'm').to('mm').magnitude
    p_mpa = ureg.Quantity(x.pressure_pa, 'Pa').to('MPa').magnitude
    s_mpa = ureg.Quantity(x.allowable_stress_pa, 'Pa').to('MPa').magnitude
    calc = ASMEThicknessCalc()
    # The existing API has no denominator guard; check its result before use.
    try:
        thickness_mm = calc.ug27_cylindrical_shell(
            p_mpa, r_mm, s_mpa, x.joint_efficiency_ratio, 0.0,
        )['t_required_mm']
    except ZeroDivisionError as exc:
        raise ValueError('nonpositive rational hoop denominator') from exc
    if thickness_mm < 0 or not math.isfinite(thickness_mm):
        raise ValueError('nonpositive rational hoop denominator')
    capacity_mpa = calc.calculate_mawp(t_mm, r_mm, s_mpa, x.joint_efficiency_ratio, 0.0)
    capacity_pa = ureg.Quantity(capacity_mpa, 'MPa').to('Pa').magnitude
    logarithm = math.log1p(x.wall_thickness_m / x.inner_radius_m)
    factored_stress = x.allowable_stress_pa * x.joint_efficiency_ratio
    return PressureEquationResult(
        rational_required_thickness_m=ureg.Quantity(thickness_mm, 'mm').to('m').magnitude,
        rational_pressure_limit_pa=capacity_pa,
        rational_required_allowable_stress_pa=x.allowable_stress_pa * x.pressure_pa / capacity_pa,
        thick_required_thickness_m=x.inner_radius_m * math.expm1(x.pressure_pa / factored_stress),
        thick_pressure_limit_pa=factored_stress * logarithm,
        thick_required_allowable_stress_pa=x.pressure_pa / (x.joint_efficiency_ratio * logarithm),
    )


def longitudinal_pressure_limits(inputs: CylinderPressureInputs) -> PressureEquationResult:
    """Calculate API 579 ``Eqn 2C.15``–``Eqn 2C.20`` by pressure inversion.

    Eqn 2C.16 is inverted to t = p*r/(2*S*E+k*p)+u. This resolves the
    inconsistent numerator reference in the source thickness expression.
    The thick inverse uses q/(sqrt(1+q)+1) to retain small-pressure accuracy.
    """
    x = inputs
    r = x.inner_radius_m
    h = x.wall_thickness_m - x.supplemental_thickness_m
    k = x.longitudinal_pressure_coefficient_ratio
    denominator = r - k * h
    if denominator <= 0:
        raise ValueError('nonpositive rational longitudinal denominator')
    se = x.allowable_stress_pa * x.joint_efficiency_ratio
    q = x.pressure_pa / se
    thickness_ratio = h / r
    thick_factor = thickness_ratio * (2 + thickness_ratio)
    return PressureEquationResult(
        rational_required_thickness_m=x.pressure_pa * r / (2 * se + k * x.pressure_pa) + x.supplemental_thickness_m,
        rational_pressure_limit_pa=2 * se * h / denominator,
        rational_required_allowable_stress_pa=x.pressure_pa * denominator / (2 * x.joint_efficiency_ratio * h),
        thick_required_thickness_m=r * q / (math.sqrt(1 + q) + 1) + x.supplemental_thickness_m,
        thick_pressure_limit_pa=se * thick_factor,
        thick_required_allowable_stress_pa=x.pressure_pa / (x.joint_efficiency_ratio * thick_factor),
    )


def select_pressure_limits(
    inputs: CylinderPressureInputs, *,
    hoop_transition_ratio: float,
    longitudinal_transition_ratio: float,
) -> SelectedPressureLimits:
    """Select rational at p/(S*E) <= transition, otherwise the thick family.

    Source: a legacy engineering workbook, API 579; clause not stated in
    source for these branch expressions. Transition ratios are required
    caller inputs. Family functions require positive rational denominators
    even when a thick branch is selected, because both families are evaluated.
    """
    for name, value in (('hoop_transition_ratio', hoop_transition_ratio),
                        ('longitudinal_transition_ratio', longitudinal_transition_ratio)):
        _finite(name, value, positive=True)
    hoop = hoop_pressure_limits(inputs)
    longitudinal = longitudinal_pressure_limits(inputs)
    # Compare in pressure units to preserve equality at a supplied threshold.
    se = inputs.allowable_stress_pa * inputs.joint_efficiency_ratio
    return SelectedPressureLimits(
        hoop.rational_pressure_limit_pa if inputs.pressure_pa <= hoop_transition_ratio * se else hoop.thick_pressure_limit_pa,
        longitudinal.rational_pressure_limit_pa if inputs.pressure_pa <= longitudinal_transition_ratio * se else longitudinal.thick_pressure_limit_pa,
    )
