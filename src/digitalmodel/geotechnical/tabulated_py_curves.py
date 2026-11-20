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

Implementation plan
-------------------
This scoped plan covers the separate pressure-basis module alongside the
existing line-resistance helpers identified above.
1. Validate immutable ForceCurve, PressureCurve, SpringGeometry and SpringPoint
   inputs, preserving supplied softening and requiring explicit units and origin.
2. Provide interpolate_force and interpolate_depth on bounded tables and a common
   displacement grid, retaining query ratios through knot selection and weighting.
3. Convert declared ordinates with force_from_pressure, pressure_from_force and
   force_from_line_resistance; preserve integral pressure products for quantization.
4. Use round_force_up for caller-supplied quanta and symmetric_spring_points for
   odd tables, applying sensitivity before the final force conversion.
5. Validate with independent arithmetic, exact boundary brackets and rejection
   cases in tests/geotechnical/tabulated_py_curves/test_tabulated_curves.py and
   tests/geotechnical/tabulated_py_curves/test_validation.py.
Soil-law generation, empirical calibration, site data and licensed solver
execution are outside this scope. No governing standard coefficient is supplied.

Standards and provenance
------------------------
No standard-derived constant is introduced by this module.

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
from math import ceil, inf, isfinite, nextafter
from numbers import Integral, Rational, Real


def _finite(value: float, name: str, *, minimum: float = 0.0) -> int | float:
    """Validate a scalar and normalize it before fixed-width arithmetic can occur."""
    message = f"{name} must be finite and >= {minimum}"
    if isinstance(value, bool) or not isinstance(value, Real) or value < minimum:
        raise ValueError(message)
    try:
        normalized = int(value) if isinstance(value, Integral) else float(value)
        if not isfinite(normalized):
            raise ValueError(message)
    except OverflowError as error:
        raise ValueError(message) from error
    return normalized


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
        """Validate the displacement axis and its force ordinates."""
        object.__setattr__(self, "displacement_m", tuple(
            _finite(value, "displacement_m") for value in self.displacement_m
        ))
        object.__setattr__(self, "force_n", tuple(
            _finite(value, "force_n") for value in self.force_n
        ))
        object.__setattr__(self, "depth_m", _finite(self.depth_m, "depth_m"))
        _axis(self.displacement_m)
        if len(self.displacement_m) != len(self.force_n):
            raise ValueError("force_n and displacement_m lengths must agree")


def _linear(x: float, axis: tuple[float, ...], values: tuple[float, ...]) -> float:
    _finite(x, "interpolation coordinate")
    # Retain the query ratio through bounds checks, knot selection and weights.
    x = _exact_fraction(x)
    if x < axis[0] or x > axis[-1]:
        raise ValueError("interpolation coordinate is outside the supplied range")
    high = bisect_left(axis, x)
    if axis[high] == x:
        return values[high]
    low = high - 1
    # Measure both distances directly so the smaller weight survives near a knot.
    from_low = x - Fraction(axis[low])
    from_high = Fraction(axis[high]) - x
    weighted_sum = from_high * Fraction(values[low]) + from_low * Fraction(values[high])
    return float(weighted_sum / (from_low + from_high))


def interpolate_force(curve: ForceCurve, displacement_m: float) -> float:
    """F = F0 + (y-y0)*(F1-F0)/(y1-y0), within supplied displacements.

    Extrapolation raises ValueError; force reduction is retained.
    """
    return _linear(displacement_m, curve.displacement_m, curve.force_n)


@dataclass(frozen=True)
class SpringGeometry:
    """Projected spring area is diameter_m * tributary_length_m, in m²."""

    diameter_m: float
    tributary_length_m: float

    def __post_init__(self) -> None:
        """Require finite, positive dimensions for the projected area."""
        for name in ("diameter_m", "tributary_length_m"):
            value = getattr(self, name)
            value = _finite(value, name)
            if value == 0:
                raise ValueError(f"{name} must be positive")
            object.__setattr__(self, name, value)


def _pressure_force_product(pressure_pa: float, geometry: SpringGeometry) -> Fraction:
    pressure_pa = _finite(pressure_pa, "pressure_pa")
    return (Fraction(pressure_pa) * Fraction(geometry.diameter_m)
            * Fraction(geometry.tributary_length_m))


def _float_force(product: Fraction) -> float:
    try:
        return float(product)
    except OverflowError as error:
        raise ValueError("computed force_n is not finite") from error


def force_from_pressure(pressure_pa: float, geometry: SpringGeometry) -> int | float:
    """F = pressure * diameter * tributary length (N), using the exact area.

    Integral products remain exact for subsequent force quantization.
    """
    product = _pressure_force_product(pressure_pa, geometry)
    if product.denominator == 1:
        return _finite(product.numerator, "computed force_n")
    return _float_force(product)


def force_from_line_resistance(
    line_resistance_n_per_m: float, integration_length_m: float
) -> float:
    """F = line resistance * declared integration length (N).

    The caller supplies the appropriate integration length.
    """
    line_resistance_n_per_m = _finite(line_resistance_n_per_m, "line_resistance_n_per_m")
    integration_length_m = _finite(integration_length_m, "integration_length_m")
    if integration_length_m == 0:
        raise ValueError("integration_length_m must be positive")
    result = line_resistance_n_per_m * integration_length_m
    _finite(result, "computed force_n")
    return result


def pressure_from_force(force_n: float, geometry: SpringGeometry) -> float:
    """Divide force by projected area and round the resulting pressure in Pa."""
    force_n = _finite(force_n, "force_n")
    area = Fraction(geometry.diameter_m) * Fraction(geometry.tributary_length_m)
    try:
        return float(Fraction(force_n) / area)
    except OverflowError as error:
        raise ValueError("computed pressure_pa is not finite") from error


def _exact_fraction(value: float) -> Fraction:
    if isinstance(value, Rational):
        # Fixed-width integer numerators must not enter Fraction arithmetic.
        return Fraction(int(value.numerator), int(value.denominator))
    if hasattr(value, "as_integer_ratio"):
        return Fraction(*value.as_integer_ratio())
    return Fraction(float(value))


def round_force_up(force_n: float, quantum_n: float) -> float:
    """Fq = ceil(F / quantum) * quantum, for nonnegative force magnitudes.

    The caller declares quantum_n; no precision or force conversion factor
    is assumed.
    Exact binary input ratios determine which side of a boundary applies.
    The returned float is at least the exact selected multiple; if no finite
    float can represent that upper bound, ValueError is raised.
    With a non-dyadic quantum (for example 0.1 N), that float can exceed the
    selected multiple. Rounding it again can add a quantum: this operation
    is not idempotent. Quantize the original force rather than a stored result.
    """
    _finite(force_n, "force_n")
    _finite(quantum_n, "quantum_n")
    if quantum_n == 0:
        raise ValueError("quantum_n must be positive")
    quantum = _exact_fraction(quantum_n)
    multiple = ceil(_exact_fraction(force_n) / quantum) * quantum
    try:
        result = float(multiple)
        if result < multiple:
            result = nextafter(result, inf)
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
        """Validate the displacement axis and its pressure ordinates."""
        object.__setattr__(self, "displacement_m", tuple(
            _finite(value, "displacement_m") for value in self.displacement_m
        ))
        object.__setattr__(self, "pressure_pa", tuple(
            _finite(value, "pressure_pa") for value in self.pressure_pa
        ))
        object.__setattr__(self, "depth_m", _finite(self.depth_m, "depth_m"))
        _axis(self.displacement_m)
        if len(self.displacement_m) != len(self.pressure_pa):
            raise ValueError("pressure_pa and displacement_m lengths must agree")


def interpolate_depth(
    curves: tuple[PressureCurve, ...], depth_m: float
) -> PressureCurve:
    """p(z) = p0 + (z-z0)*(p1-p0)/(z1-z0), on a common displacement grid.

    Curves must be ordered by strictly increasing depth.
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
        """Validate the signs of both inputs before storing their normalized values."""
        displacement = _finite(self.displacement_m, "displacement_m", minimum=-inf)
        force = _finite(self.force_n, "force_n", minimum=-inf)
        if self.displacement_m < 0 < self.force_n or self.force_n < 0 < self.displacement_m:
            raise ValueError("nonzero displacement and force must have the same sign")
        object.__setattr__(self, "displacement_m", displacement)
        object.__setattr__(self, "force_n", force)


def symmetric_spring_points(
    curve: PressureCurve,
    geometry: SpringGeometry,
    *,
    sensitivity_ratio: float = 0.0,
) -> tuple[SpringPoint, ...]:
    """F(±y) = ±p(y)*diameter*length*(1+s), with one origin point.

    The caller declares dimensionless sensitivity_ratio s (a fraction, not %).
    Requires a supplied (0, 0); no extrapolation is appended.
    Sensitivity scales the exact force product before the final float conversion.
    """
    sensitivity_ratio = _finite(sensitivity_ratio, "sensitivity_ratio", minimum=-1.0)
    if curve.displacement_m[0] != 0 or curve.pressure_pa[0] != 0:
        raise ValueError("a symmetric spring requires an explicit (0, 0) origin")
    factor = 1 + Fraction(sensitivity_ratio)
    positive = tuple(
        SpringPoint(y, _float_force(_pressure_force_product(p, geometry) * factor))
        for y, p in zip(curve.displacement_m[1:], curve.pressure_pa[1:])
    )
    negative = tuple(SpringPoint(-p.displacement_m, -p.force_n) for p in reversed(positive))
    return negative + (SpringPoint(0.0, 0.0),) + positive
