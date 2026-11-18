# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Soft-clay load-transfer backbones and a small lateral pile check solver.

Units: m, kN, kPa, kN/m³, EI in kN m². No empirical soil parameter is selected
automatically. Sources and applicability are recorded in
``docs/domains/geotechnical/soft-clay-pile-analysis.md``.

These are single-direction backbones, not hysteretic constitutive models.
API RP 2GEO, first edition (2011), soft-clay lateral/axial guidance is a
normative cross-reference only: compliance is NOT_EVALUATED.
"""

from __future__ import annotations

from bisect import bisect_right
from dataclasses import dataclass
import math
from typing import Mapping, Sequence

from digitalmodel.geotechnical.database_schema import Quantity


def _number(value: float, name: str, *, positive: bool = False) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a finite number")
    if not math.isfinite(value) or (value <= 0 if positive else value < 0):
        bound = "positive" if positive else "nonnegative"
        raise ValueError(f"{name} must be finite and {bound}")
    return float(value)


def _signed(value: float, name: str) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a finite number")
    if not math.isfinite(value):
        raise ValueError(f"{name} must be finite")
    return float(value)


def degradation_factor(cycles: float, exponent: float) -> float:
    """Return N**(-b), with both N and b explicitly supplied.

    Power-law degradation-index form requested for cyclic clay studies
    (1978 cyclic-clay study; see the calibration gap in the documentation).
    No material calibration or plasticity-index correlation is implied.
    N>=1; b>=0. A numerically exhausted material is rejected, not floored.
    """
    cycles = _number(cycles, "cycles", positive=True)
    exponent = _number(exponent, "exponent")
    if cycles < 1:
        raise ValueError("cycles must be at least one")
    factor = cycles ** (-exponent)
    if factor == 0:
        raise ValueError("degradation factor underflow; material exhausted")
    return factor


@dataclass(frozen=True)
class CyclicDegradation:
    """Independent user-calibrated strength and secant-stiffness exponents."""

    cycles: float
    strength_exponent: float
    stiffness_exponent: float

    def __post_init__(self) -> None:
        degradation_factor(self.cycles, self.strength_exponent)
        degradation_factor(self.cycles, self.stiffness_exponent)

    @property
    def strength_factor(self) -> float:
        """Return the multiplier on undrained strength or axial resistance."""
        return degradation_factor(self.cycles, self.strength_exponent)

    @property
    def stiffness_factor(self) -> float:
        """Return the multiplier on secant stiffness at homologous points."""
        return degradation_factor(self.cycles, self.stiffness_exponent)


@dataclass(frozen=True)
class Curve:
    """Piecewise-linear backbone, with a constant tail after the last point.

    ``symmetric`` gives odd reflection for lateral/shaft springs. Otherwise
    negative displacements carry zero resistance (compression-only toe).
    Axial resistances are stresses, not integrated pile forces.
    """

    displacements_m: tuple[float, ...]
    resistances: tuple[float, ...]
    resistance_unit: str
    symmetric: bool

    def __post_init__(self) -> None:
        # Freeze caller-owned lists as well as the dataclass itself.
        object.__setattr__(self, "displacements_m", tuple(self.displacements_m))
        object.__setattr__(self, "resistances", tuple(self.resistances))
        if len(self.displacements_m) < 2 or len(self.displacements_m) != len(self.resistances):
            raise ValueError("curve needs matching arrays with at least two points")
        for value in self.displacements_m + self.resistances:
            _number(value, "curve value")
        if self.displacements_m[0] != 0 or self.resistances[0] != 0:
            raise ValueError("curve must start at the origin")
        if any(b <= a for a, b in zip(self.displacements_m, self.displacements_m[1:])):
            raise ValueError("displacements must be strictly increasing")
        if self.resistance_unit not in ("kN/m", "kPa"):
            raise ValueError("unsupported resistance unit")
        if not isinstance(self.symmetric, bool):
            raise ValueError("symmetric must be a boolean")

    def resistance_at(self, displacement_m: float) -> float:
        """Interpolate the supplied backbone, preserving the force sign."""
        value = _signed(displacement_m, "displacement_m")
        if value < 0 and not self.symmetric:
            return 0.0
        x = abs(value)
        i = bisect_right(self.displacements_m, x)
        if i == len(self.displacements_m):
            resistance = self.resistances[-1]
        else:
            a, b = self.displacements_m[i - 1:i + 1]
            pa, pb = self.resistances[i - 1:i + 1]
            resistance = pa + (pb - pa) * (x - a) / (b - a)
        return math.copysign(resistance, value)


@dataclass(frozen=True)
class SoftClay:
    """Required submerged soft-clay inputs; J and eps50 have no defaults."""

    su_kpa: float
    effective_unit_weight_kn_m3: float
    eps50: float
    j: float

    def __post_init__(self) -> None:
        _number(self.su_kpa, "su_kpa", positive=True)
        _number(self.effective_unit_weight_kn_m3, "effective_unit_weight_kn_m3")
        _number(self.eps50, "eps50", positive=True)
        _number(self.j, "j", positive=True)


@dataclass(frozen=True)
class SoftClayPY:
    """Lateral curve plus its normalization and transition depth."""

    curve: Curve
    ultimate_kn_m: float
    y50_m: float
    transition_depth_m: float


def soft_clay_py(
    soil: SoftClay,
    *,
    depth_m: float,
    diameter_m: float,
    displacements_m: Sequence[float],
    cyclic: bool = False,
    degradation: CyclicDegradation | None = None,
    effective_stress_kpa: float | None = None,
) -> SoftClayPY:
    """Generate a submerged soft-clay p-y backbone, sampled at caller points.

    OTC 1204 (1970), public reproduction: OpenPile py_curves.matlock_1970
    notes, Ultimate resistance / Strain normalization / Transition zone /
    cyclic formulation. Constants 3, 9, 2.5, 6, 0.5, 8, 3, 12, 15 and 0.72
    below come from those equations. The exact cube root replaces the
    reproduction's rounded exponent 0.33. Capping the initial cyclic branch
    at 0.72 resolves its rounded junction without a force discontinuity.

    For layered soils pass integrated effective stress; the transition-depth
    approximation uses average gamma'=sigma'/z. It is not a layered failure
    analysis. Strength degradation recomputes pu and zr. Scaling y50 by
    (pu_N/pu_1)/stiffness_factor gives the requested secant-stiffness factor
    at homologous static backbone points. Cyclic tail geometry also changes.
    """
    z = _number(depth_m, "depth_m")
    d = _number(diameter_m, "diameter_m", positive=True)
    if not isinstance(cyclic, bool):
        raise ValueError("cyclic must be a boolean")
    stress = _number(soil.effective_unit_weight_kn_m3 * z if effective_stress_kpa is None
                     else effective_stress_kpa, "effective_stress_kpa")
    if z == 0 and stress != 0:
        raise ValueError("effective stress at mudline must be zero")
    strength_factor = degradation.strength_factor if degradation else 1.0
    stiffness_factor = degradation.stiffness_factor if degradation else 1.0
    su = _number(soil.su_kpa * strength_factor, "degraded su", positive=True)

    def ultimate(strength: float) -> float:
        return min((3 * strength + stress) * d + soil.j * strength * z,
                   9 * strength * d)

    pu = _number(ultimate(su), "ultimate resistance", positive=True)
    baseline_pu = _number(ultimate(soil.su_kpa), "baseline ultimate resistance", positive=True)
    yc = _number(2.5 * soil.eps50 * d * (pu / baseline_pu) / stiffness_factor,
                 "degraded y50", positive=True)
    gamma = stress / z if z else soil.effective_unit_weight_kn_m3
    zr = max(6 * d / (gamma * d / su + soil.j), 2.5 * d)
    values = []
    for displacement in displacements_m:
        r = _number(displacement, "displacement_m") / yc
        if not cyclic:
            normalized = min(0.5 * r ** (1 / 3), 1.0)
        elif r <= 3:
            normalized = min(0.5 * r ** (1 / 3), 0.72)
        else:
            depth_ratio = min(z / zr, 1.0)
            fraction = min((r - 3) / 12, 1.0)
            normalized = 0.72 * (1 - (1 - depth_ratio) * fraction)
        values.append(pu * normalized)
    curve = Curve(tuple(displacements_m), tuple(values), "kN/m", True)
    return SoftClayPY(curve, pu, yc, zr)


def _normalized_points(points: Sequence[tuple[float, float]]) -> tuple[tuple[float, ...], tuple[float, ...]]:
    try:
        x, p = zip(*points)
    except (ValueError, TypeError) as error:
        raise ValueError("normalized table needs pairs") from error
    checked = Curve(tuple(x), tuple(p), "kPa", True)
    if p[-1] != 1 or any(b < a for a, b in zip(p, p[1:])):
        raise ValueError("normalized mobilization must increase to unity")
    return checked.displacements_m, checked.resistances


def _axial_scaled_curve(
    x: Sequence[float], p: Sequence[float], symmetric: bool,
    degradation: CyclicDegradation | None,
) -> Curve:
    strength = degradation.strength_factor if degradation else 1.0
    stiffness = degradation.stiffness_factor if degradation else 1.0
    return Curve(tuple(v * strength / stiffness for v in x),
                 tuple(v * strength for v in p), "kPa", symmetric)


def clay_tz(
    *, ultimate_kpa: float, diameter_m: float,
    normalized_points: Sequence[tuple[float, float]], residual_ratio: float,
    residual_displacement_ratio: float,
    degradation: CyclicDegradation | None = None,
) -> Curve:
    """Scale a caller-supplied t-z table and append an explicit residual point.

    Inputs are (z/D, t/tult) pairs increasing from the origin to peak unity.
    API RP 2GEO (2011), axial load-transfer guidance: table conformance is
    NOT_EVALUATED; no licensed table is embedded. Piecewise-linear shape and
    residual extension follow the public OpenPile tz_curves backbone approach.
    The residual location and ratio are required engineering selections.
    """
    ultimate = _number(ultimate_kpa, "ultimate_kpa", positive=True)
    diameter = _number(diameter_m, "diameter_m", positive=True)
    residual = _number(residual_ratio, "residual_ratio")
    end = _number(residual_displacement_ratio, "residual_displacement_ratio", positive=True)
    x, p = _normalized_points(normalized_points)
    if residual > 1 or end <= x[-1]:
        raise ValueError("residual ratio must be <=1 and residual slip beyond peak")
    return _axial_scaled_curve(
        tuple(v * diameter for v in x + (end,)),
        tuple(v * ultimate for v in p + (residual,)), True, degradation,
    )


def clay_qz(
    *, ultimate_kpa: float, diameter_m: float,
    normalized_points: Sequence[tuple[float, float]],
    degradation: CyclicDegradation | None = None,
) -> Curve:
    """Scale a caller-supplied compression-only q-z table, returning kPa.

    Table pairs are (z/D, q/qult). API RP 2GEO (2011) table conformance is
    NOT_EVALUATED. The caller supplies the ultimate stress from a separately
    assessed toe-capacity calculation; no bearing factor is assumed here.
    Multiply the resulting stress by the appropriate toe area outside this API.
    """
    ultimate = _number(ultimate_kpa, "ultimate_kpa", positive=True)
    diameter = _number(diameter_m, "diameter_m", positive=True)
    x, p = _normalized_points(normalized_points)
    return _axial_scaled_curve(tuple(v * diameter for v in x),
                               tuple(v * ultimate for v in p), False, degradation)


def clay_tz_kraft(
    *, ultimate_kpa: float, diameter_m: float, shear_modulus_kpa: float,
    load_ratios: Sequence[float], influence_radius_ratio: float,
    curve_factor: float, residual_ratio: float, residual_slip_m: float,
    degradation: CyclicDegradation | None = None,
) -> Curve:
    """Generate an axial shaft backbone using the public 1981 load-transfer equation.

    Source: OpenPile tz_curves.kraft_modification, public equation expression:
    z = t D/(2 G) ln[(R - Rf t/tult)/(1 - Rf t/tult)].
    R is influence radius / pile radius; R>1 and 0<=Rf<1.
    All curve-fitting and residual parameters are required; none of that
    implementation's empirical defaults or fixed residual slips is adopted.
    The residual slip is an increment beyond peak before degradation scaling.
    """
    ultimate = _number(ultimate_kpa, "ultimate_kpa", positive=True)
    diameter = _number(diameter_m, "diameter_m", positive=True)
    modulus = _number(shear_modulus_kpa, "shear_modulus_kpa", positive=True)
    radius = _number(influence_radius_ratio, "influence_radius_ratio", positive=True)
    fitting = _number(curve_factor, "curve_factor")
    residual = _number(residual_ratio, "residual_ratio")
    slip = _number(residual_slip_m, "residual_slip_m", positive=True)
    if radius <= 1 or fitting >= 1 or residual > 1:
        raise ValueError("require R>1, 0<=Rf<1, and 0<=residual_ratio<=1")
    ratios = tuple(load_ratios)
    if len(ratios) < 2 or ratios[0] != 0 or ratios[-1] != 1:
        raise ValueError("load_ratios must run from zero to unity")
    for ratio in ratios:
        _number(ratio, "load_ratio")
    if any(b <= a for a, b in zip(ratios, ratios[1:])):
        raise ValueError("load_ratios must increase strictly")
    t = tuple(r * ultimate for r in ratios)
    z = tuple(ti * diameter / (2 * modulus) *
              math.log((radius - fitting * r) / (1 - fitting * r))
              for r, ti in zip(ratios, t))
    return _axial_scaled_curve(z + (z[-1] + slip,),
                               t + (residual * ultimate,), True, degradation)


@dataclass(frozen=True)
class ClayLayer:
    """Explicit depth bounds below mudline and optional database row provenance."""

    top_m: float
    bottom_m: float
    soil: SoftClay
    source_row_ids: tuple[str, ...] = ()

    def __post_init__(self) -> None:
        _number(self.top_m, "top_m")
        _number(self.bottom_m, "bottom_m", positive=True)
        if self.bottom_m <= self.top_m:
            raise ValueError("layer bottom must be below its top")
        object.__setattr__(self, "source_row_ids", tuple(self.source_row_ids))


def clay_layer_from_database(
    *, top_m: float, bottom_m: float, quantities: Mapping[str, Quantity],
    selections_si: Mapping[str, float],
) -> ClayLayer:
    """Adapt explicitly selected database quantities without inferring a profile.

    Required keys: su (Pa), effective_unit_weight (N/m³), eps50 and j
    (dimensionless). Every selection is explicit, including a scalar row.
    Ranges require a selection inside the printed bounds; qualifiers and
    unavailable conversions are rejected by Quantity.in_si(). Rows must name
    the same soil unit. The caller assigns physical meaning and depth bounds;
    arbitrary database parameter labels and printed depth text are not parsed.
    """
    names = ("su", "effective_unit_weight", "eps50", "j")
    units = ("Pa", "N/m**3", "dimensionless", "dimensionless")
    if set(quantities) != set(names) or set(selections_si) != set(names):
        raise ValueError("all four quantities and explicit selections are required")
    values, provenance, soil_units = [], [], set()
    for name, unit in zip(names, units):
        quantity = quantities[name]
        if quantity.qualifier or quantity.source.read_status != "read":
            raise ValueError(f"{name}: qualified or unread database input")
        numeric = quantity.in_si()
        selected = _number(selections_si[name], name)
        if numeric.unit != unit:
            raise ValueError(f"{name}: incompatible SI unit")
        available = False
        if numeric.value is not None:
            available = selected == numeric.value
        elif numeric.minimum is not None and numeric.maximum is not None:
            available = numeric.minimum <= selected <= numeric.maximum
        if not available:
            raise ValueError(f"{name}: selection is outside the available database value")
        values.append(selected)
        provenance.append(quantity.parent_row_id)
        soil_units.add(quantity.row.soil_unit)
    if len(soil_units) != 1:
        raise ValueError("database quantities must describe one soil unit")
    # Exact SI prefix conversion (kilo=10³), not an empirical soil coefficient.
    soil = SoftClay(values[0] / 1000, values[1] / 1000, values[2], values[3])
    return ClayLayer(top_m, bottom_m, soil, tuple(provenance))


@dataclass(frozen=True)
class CurveFamily:
    """Depth-major tuples ready for a lateral beam spring discretization."""

    depths_m: tuple[float, ...]
    curves: tuple[Curve, ...]
    layer_indices: tuple[int, ...]
    source_row_ids: tuple[tuple[str, ...], ...]

    @property
    def resistance_arrays(self) -> tuple[tuple[float, ...], ...]:
        """Return depth-major p arrays in kN/m on the common displacement grid."""
        return tuple(curve.resistances for curve in self.curves)


def assemble_py_family(
    *, layers: Sequence[ClayLayer], depths_m: Sequence[float], diameter_m: float,
    displacements_m: Sequence[float], cyclic: bool = False,
    degradation: CyclicDegradation | None = None,
) -> CurveFamily:
    """Integrate effective overburden through an explicit contiguous clay profile.

    Layer interfaces belong to the deeper layer; the final bottom is included.
    This applies local homogeneous-clay backbones at each station, using the
    integrated overburden and average effective unit weight for zr. Layered
    wedge mechanisms, group effects and monopile calibration are NOT_EVALUATED.
    """
    layers = tuple(layers)
    depths = tuple(depths_m)
    if not layers or layers[0].top_m != 0:
        raise ValueError("profile must start at mudline")
    if any(a.bottom_m != b.top_m for a, b in zip(layers, layers[1:])):
        raise ValueError("profile must be contiguous without overlaps")
    if not depths:
        raise ValueError("depths must not be empty")
    for depth in depths:
        _number(depth, "depth_m")
    if any(b <= a for a, b in zip(depths, depths[1:])) or depths[-1] > layers[-1].bottom_m:
        raise ValueError("depths must increase inside the profile")
    tops = tuple(layer.top_m for layer in layers)
    curves, indices, provenance = [], [], []
    grid = tuple(displacements_m)
    for depth in depths:
        index = bisect_right(tops, depth) - 1
        layer = layers[index]
        stress = sum(max(0, min(depth, item.bottom_m) - item.top_m) *
                     item.soil.effective_unit_weight_kn_m3 for item in layers[:index + 1])
        result = soft_clay_py(layer.soil, depth_m=depth, diameter_m=diameter_m,
                             displacements_m=grid, cyclic=cyclic, degradation=degradation,
                             effective_stress_kpa=stress)
        curves.append(result.curve)
        indices.append(index)
        provenance.append(layer.source_row_ids)
    return CurveFamily(depths, tuple(curves), tuple(indices), tuple(provenance))


@dataclass(frozen=True)
class LateralPileResult:
    """Converged displacements, signed soil reactions and equilibrium errors."""

    depths_m: tuple[float, ...]
    displacements_m: tuple[float, ...]
    soil_reaction_kn_m: tuple[float, ...]
    iterations: int
    residual_kn: float
    force_balance_kn: float
    moment_balance_kn_m: float


def _band_solve(diagonal: Sequence[float], first: Sequence[float],
                second: Sequence[float], rhs: Sequence[float]) -> list[float]:
    """Solve a symmetric positive-definite pentadiagonal system by LDLᵀ."""
    n = len(diagonal)
    pivots, l1, l2, forward = [0.0] * n, [0.0] * n, [0.0] * n, list(rhs)
    for i in range(n):
        if i >= 2:
            l2[i] = second[i] / pivots[i - 2]
        if i >= 1:
            cross = l2[i] * pivots[i - 2] * l1[i - 1] if i >= 2 else 0.0
            l1[i] = (first[i] - cross) / pivots[i - 1]
        pivots[i] = diagonal[i]
        if i >= 1:
            pivots[i] -= l1[i] ** 2 * pivots[i - 1]
            forward[i] -= l1[i] * forward[i - 1]
        if i >= 2:
            pivots[i] -= l2[i] ** 2 * pivots[i - 2]
            forward[i] -= l2[i] * forward[i - 2]
        if not math.isfinite(pivots[i]) or pivots[i] <= 0:
            raise ValueError("foundation support is singular or numerically ill-conditioned")
    solution = [f / d for f, d in zip(forward, pivots)]
    for i in range(n - 1, -1, -1):
        if i + 1 < n:
            solution[i] -= l1[i + 1] * solution[i + 1]
        if i + 2 < n:
            solution[i] -= l2[i + 2] * solution[i + 2]
    return solution


def solve_lateral_pile(
    *, depths_m: Sequence[float], ei_kn_m2: float, curves: Sequence[Curve],
    head_shear_kn: float, displacement_tolerance_m: float, force_tolerance_kn: float,
    max_iterations: int, relaxation: float, head_moment_kn_m: float = 0,
) -> LateralPileResult:
    """Solve a free-head, free-toe Euler beam with lateral backbone springs.

    Basis: MIT Structural Mechanics (2013), lecture 5, beam equilibrium;
    Beams on Elastic Foundation (1946), equation 19a is the independent check.
    Discrete bending energy is EI*h/2 * sum((D2 y)**2), with the exact central
    difference stencil D2=(1,-2,1)/h². Soil work uses trapezoidal nodal widths.
    Natural end conditions are free except for applied head shear and moment.
    Moment is conjugate to the forward-difference head rotation (y1-y0)/h.
    All numerical controls are caller supplied; no convergence is presumed.

    Secant iteration with relaxation follows the origin-loading branch.
    A descending spring can have multiple equilibria; continuation, stability,
    unloading, hysteresis, axial coupling and design acceptance are unsupported.
    Displacement increments, nodal residuals and total force balance must
    satisfy their limits; moment balance uses force_tolerance_kn times length.
    """
    depths, curves = tuple(depths_m), tuple(curves)
    n = len(depths)
    ei = _number(ei_kn_m2, "ei_kn_m2", positive=True)
    shear = _signed(head_shear_kn, "head_shear_kn")
    moment = _signed(head_moment_kn_m, "head_moment_kn_m")
    y_tol = _number(displacement_tolerance_m, "displacement_tolerance_m", positive=True)
    f_tol = _number(force_tolerance_kn, "force_tolerance_kn", positive=True)
    relax = _number(relaxation, "relaxation", positive=True)
    if (not isinstance(max_iterations, int) or isinstance(max_iterations, bool)
            or max_iterations < 1 or relax > 1):
        raise ValueError("require integer max_iterations>=1 and 0<relaxation<=1")
    if n < 5 or len(curves) != n or depths[0] != 0:
        raise ValueError("matching depths and curves need at least five nodes from mudline")
    for depth in depths:
        _number(depth, "depth_m")
    h = _number(depths[-1] / (n - 1), "mesh spacing", positive=True)
    # Mesh tolerance is floating-point bookkeeping, not a geotechnical limit.
    if any(not math.isclose(z, i * h, rel_tol=1e-12, abs_tol=1e-12)
           for i, z in enumerate(depths)):
        raise ValueError("solver requires a uniform increasing depth grid")
    if any(c.resistance_unit != "kN/m" or not c.symmetric for c in curves):
        raise ValueError("solver requires symmetric lateral curves in kN/m")
    if sum(c.resistances[1] > 0 for c in curves) < 2:
        raise ValueError("foundation requires positive initial support at two depths")
    weights = [h] * n
    weights[0] = weights[-1] = h / 2
    diagonal, first, second = [0.0] * n, [0.0] * n, [0.0] * n
    c = ei / h**3
    # Outer products of the central curvature stencil preserve force/moment balance.
    for i in range(1, n - 1):
        diagonal[i - 1] += c
        diagonal[i] += 4 * c
        diagonal[i + 1] += c
        first[i] -= 2 * c
        first[i + 1] -= 2 * c
        second[i + 1] += c
    loads = [0.0] * n
    loads[0] = shear - moment / h
    loads[1] = moment / h
    y = [0.0] * n
    for iteration in range(1, max_iterations + 1):
        secants = [curve.resistance_at(yi) / yi if yi else
                   curve.resistances[1] / curve.displacements_m[1]
                   for curve, yi in zip(curves, y)]
        candidate = _band_solve([d + w * k for d, w, k in zip(diagonal, weights, secants)],
                                first, second, loads)
        updated = [old + relax * (new - old) for old, new in zip(y, candidate)]
        increment = max(abs(a - b) for a, b in zip(updated, y))
        y = updated
        reaction = [curve.resistance_at(yi) for curve, yi in zip(curves, y)]
        # Evaluate bending forces through curvature differences to reduce cancellation.
        bending = [0.0] * n
        for i in range(1, n - 1):
            force = c * (y[i - 1] - 2 * y[i] + y[i + 1])
            bending[i - 1] += force
            bending[i] -= 2 * force
            bending[i + 1] += force
        residual = max(abs(b + w * p - f) for b, w, p, f in
                       zip(bending, weights, reaction, loads))
        forces = [w * p for w, p in zip(weights, reaction)]
        force_balance = math.fsum(forces) - shear
        moment_balance = math.fsum(z * f for z, f in zip(depths, forces)) - moment
        if (increment <= y_tol and residual <= f_tol and abs(force_balance) <= f_tol
                and abs(moment_balance) <= f_tol * depths[-1]):
            return LateralPileResult(
                depths, tuple(y), tuple(reaction), iteration, residual,
                force_balance, moment_balance,
            )
    raise RuntimeError(f"lateral pile did not converge after {max_iterations} iterations")
