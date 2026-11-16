"""Tabulated lateral soil p-y curve construction from supplied force-displacement tables.

What is computed
----------------
Supplied positive-displacement force curves are interpolated linearly, converted
into pressure by projected area, and interpolated by depth on a common grid.
An odd spring table can then be constructed. Decreasing resistance is retained.
This method constructs tabulated curves; it does not generate them from soil
strength, friction angle, layer type, or an API constitutive law.

Inputs and units
----------------
Inputs store depths, displacements, diameters and tributary lengths in m;
force in N; pressure in Pa; sensitivity as a dimensionless fraction. Pressure
and line resistance (N/m) have distinct functions. Conversion to SI belongs at
the caller boundary; no approximate imperial factors are public defaults.
A force quantum, if required, is supplied in N. No tabulated soil values or
site inputs are included. Tests use independently calculated illustrative examples.

Method and limits
-----------------
Use force_from_pressure or force_from_line_resistance for a declared ordinate
basis, interpolate_force for each desired displacement, optionally round_force_up,
and pressure_from_force for each normalized ordinate. PressureCurve inputs to
interpolate_depth require the same displacement grid. symmetric_spring_points
multiplies pressure by diameter, tributary length and (1+sensitivity_ratio).
No missing origin, out-of-range displacement/depth or endpoint extension is
inferred. All inputs are finite, nonnegative magnitudes except signed spring
points and sensitivity >= -1. Quantization uses the exact binary input ratio;
converting units before quantization can change the selected quantum at a boundary.

Related code
------------
digitalmodel.drilling_riser.global_model.foundation.interpolate_py and
digitalmodel.drilling_riser.global_model.foundation.spring_table_kn operate on
line resistance (N/m), reject decreasing resistance, and hold end curves outside
the tabulated depth range. This module retains softening, raises for out-of-range
depths instead of holding end curves, and uses a pressure basis (Pa).

Standards stated in the source
-----------------------------
Reference images identify API-RP-2A (2000), with the heading API Recommended
Practice 2A-WSD: soft clay clauses 6.8.2 and 6.8.3; stiff clay clause 6.8.5;
sand clauses 6.8.6 and 6.8.7. A source-labelled API-RP-2A (2007) amendment replaces
clause 6.8.3. OTC 20158 (2009) is also named; clause not stated in source.
Those references are dormant theory. No standard or clause is attributed to
the active interpolation, area conversion, rounding or spring-table arithmetic.
No standard-derived numeric constant is introduced by this module.

Input gaps
----------
Callers must declare the physical basis of supplied ordinates and use exact unit
conversions at the input boundary. Soil-law generation would require constitutive
parameters, layer definitions, and confirmed governing standard editions and
clauses; these are not inferred from the supplied tables.
"""

from __future__ import annotations

from bisect import bisect_left
from dataclasses import dataclass
from fractions import Fraction
from math import isfinite
from numbers import Rational, Real


def _finite(value: float, name: str, *, minimum: float = 0.0) -> None:
    if (
        isinstance(value, bool) or not isinstance(value, Real)
        or not isfinite(value) or value < minimum
    ):
        raise ValueError(f"{name} must be finite and >= {minimum}")


def _axis(values: tuple[float, ...]) -> None:
    if len(values) < 2:
        raise ValueError("displacement_m requires at least two points")
    for value in values:
        _finite(value, "displacement_m")
    if any(b <= a for a, b in zip(values, values[1:])):
        raise ValueError("displacement_m must be strictly increasing")


@dataclass(frozen=True)
class ForceCurve:
    """Nonnegative force magnitudes in N on a displacement axis in m, at depth m.

    Decreasing forces are permitted. Tuple conversion prevents mutable aliases.
    A supplied curve need not start at the origin; no missing origin is inferred.
    """

    depth_m: float
    displacement_m: tuple[float, ...]
    force_n: tuple[float, ...]

    def __post_init__(self) -> None:
        object.__setattr__(self, "displacement_m", tuple(self.displacement_m))
        object.__setattr__(self, "force_n", tuple(self.force_n))
        _finite(self.depth_m, "depth_m")
        _axis(self.displacement_m)
        if len(self.displacement_m) != len(self.force_n):
            raise ValueError("force_n and displacement_m lengths must agree")
        for force in self.force_n:
            _finite(force, "force_n")


def _linear(x: float, axis: tuple[float, ...], values: tuple[float, ...]) -> float:
    _finite(x, "interpolation coordinate")
    if x < axis[0] or x > axis[-1]:
        raise ValueError("interpolation coordinate is outside the supplied range")
    high = bisect_left(axis, x)
    if axis[high] == x:
        return values[high]
    low = high - 1
    fraction = (x - axis[low]) / (axis[high] - axis[low])
    return values[low] + fraction * (values[high] - values[low])


def interpolate_force(curve: ForceCurve, displacement_m: float) -> float:
    """F = F0 + (y-y0)*(F1-F0)/(y1-y0), within supplied displacements.

    This is the tabulated interpolation of a legacy calculation;
    no standard or clause is attributed to that interpolation in the source.
    Extrapolation raises ValueError; force reduction is retained.
    """
    return _linear(displacement_m, curve.displacement_m, curve.force_n)


@dataclass(frozen=True)
class SpringGeometry:
    """Projected spring area is diameter_m * tributary_length_m, in m²."""

    diameter_m: float
    tributary_length_m: float

    def __post_init__(self) -> None:
        for name in ("diameter_m", "tributary_length_m"):
            value = getattr(self, name)
            _finite(value, name)
            if value == 0:
                raise ValueError(f"{name} must be positive")


def force_from_pressure(pressure_pa: float, geometry: SpringGeometry) -> float:
    """F = pressure * diameter * tributary length (N), source tabulated method.

    No standard or clause is attributed to this area conversion in the source.
    """
    _finite(pressure_pa, "pressure_pa")
    result = pressure_pa * geometry.diameter_m * geometry.tributary_length_m
    _finite(result, "computed force_n")
    return result


def force_from_line_resistance(
    line_resistance_n_per_m: float, integration_length_m: float
) -> float:
    """F = line resistance * declared integration length (N).

    The source offers integration along length or diameter; the caller supplies
    the appropriate length. No standard or clause is stated for this conversion.
    """
    _finite(line_resistance_n_per_m, "line_resistance_n_per_m")
    _finite(integration_length_m, "integration_length_m")
    if integration_length_m == 0:
        raise ValueError("integration_length_m must be positive")
    result = line_resistance_n_per_m * integration_length_m
    _finite(result, "computed force_n")
    return result


def pressure_from_force(force_n: float, geometry: SpringGeometry) -> float:
    """p = F / diameter / tributary length (Pa), source tabulated method.

    No standard or clause is attributed to this normalization in the source.
    """
    _finite(force_n, "force_n")
    result = force_n / geometry.diameter_m / geometry.tributary_length_m
    _finite(result, "computed pressure_pa")
    return result


def round_force_up(force_n: float, quantum_n: float) -> float:
    """Fq = ceil(F / quantum) * quantum, for nonnegative force magnitudes.

    This generalizes the source's round-up step. The caller declares quantum_n;
    no source-specific precision or force conversion factor is assumed.
    Exact binary input ratios determine which side of a boundary applies.
    No standard or clause is attributed to rounding in the source.
    """
    _finite(force_n, "force_n")
    _finite(quantum_n, "quantum_n")
    if quantum_n == 0:
        raise ValueError("quantum_n must be positive")
    f = Fraction(force_n) if isinstance(force_n, Rational) else Fraction(float(force_n))
    q = Fraction(quantum_n) if isinstance(quantum_n, Rational) else Fraction(float(quantum_n))
    fn, fd = f.numerator, f.denominator
    qn, qd = q.numerator, q.denominator
    numerator, denominator = fn * qd, fd * qn
    steps = -(-numerator // denominator)
    try:
        result = (steps * qn) / qd
    except OverflowError as error:
        raise ValueError("computed rounded force_n is not finite") from error
    _finite(result, "computed rounded force_n")
    return result


@dataclass(frozen=True)
class PressureCurve:
    """Nonnegative lateral pressure in Pa versus displacement m, at depth m.

    Pressure is force divided by diameter and tributary length, not line load.
    """

    depth_m: float
    displacement_m: tuple[float, ...]
    pressure_pa: tuple[float, ...]

    def __post_init__(self) -> None:
        object.__setattr__(self, "displacement_m", tuple(self.displacement_m))
        object.__setattr__(self, "pressure_pa", tuple(self.pressure_pa))
        _finite(self.depth_m, "depth_m")
        _axis(self.displacement_m)
        if len(self.displacement_m) != len(self.pressure_pa):
            raise ValueError("pressure_pa and displacement_m lengths must agree")
        for pressure in self.pressure_pa:
            _finite(pressure, "pressure_pa")


def interpolate_depth(
    curves: tuple[PressureCurve, ...], depth_m: float
) -> PressureCurve:
    """p(z) = p0 + (z-z0)*(p1-p0)/(z1-z0), on a common displacement grid.

    Curves must be ordered by strictly increasing depth. Source tabulated method;
    no standard or clause is attributed to depth interpolation in the source.
    Missing grid points require displacement interpolation before this call.
    Out-of-range depths raise ValueError, including above/below a single curve.
    """
    if not curves:
        raise ValueError("at least one pressure curve is required")
    depths = tuple(curve.depth_m for curve in curves)
    if any(b <= a for a, b in zip(depths, depths[1:])):
        raise ValueError("curve depths must be strictly increasing")
    grid = curves[0].displacement_m
    if any(curve.displacement_m != grid for curve in curves):
        raise ValueError("pressure curves must use a common displacement grid")
    pressures = tuple(
        _linear(depth_m, depths, tuple(curve.pressure_pa[i] for curve in curves))
        for i in range(len(grid))
    )
    return PressureCurve(depth_m, grid, pressures)


@dataclass(frozen=True)
class SpringPoint:
    """Signed displacement m and same-sign resistance magnitude N for a solver.

    The solver applies its own restoring-force sign convention.
    """

    displacement_m: float
    force_n: float

    def __post_init__(self) -> None:
        for value in (self.displacement_m, self.force_n):
            if isinstance(value, bool) or not isinstance(value, Real) or not isfinite(value):
                raise ValueError("spring coordinates must be finite real numbers")


def symmetric_spring_points(
    curve: PressureCurve,
    geometry: SpringGeometry,
    *,
    sensitivity_ratio: float = 0.0,
) -> tuple[SpringPoint, ...]:
    """F(±y) = ±p(y)*diameter*length*(1+s), with one origin point.

    The caller declares dimensionless sensitivity_ratio s (a fraction, not %).
    Source tabulated method; no standard or clause is attributed to symmetry
    or sensitivity. Requires a supplied (0, 0); no extrapolation is appended.
    """
    _finite(sensitivity_ratio, "sensitivity_ratio", minimum=-1.0)
    if curve.displacement_m[0] != 0 or curve.pressure_pa[0] != 0:
        raise ValueError("a symmetric spring requires an explicit (0, 0) origin")
    positive = tuple(
        SpringPoint(y, force_from_pressure(p, geometry) * (1 + sensitivity_ratio))
        for y, p in zip(curve.displacement_m[1:], curve.pressure_pa[1:])
    )
    negative = tuple(SpringPoint(-p.displacement_m, -p.force_n) for p in reversed(positive))
    return negative + (SpringPoint(0.0, 0.0),) + positive
