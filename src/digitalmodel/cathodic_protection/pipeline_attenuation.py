"""Uniform-pipeline ICCP screening using the equations reproduced in #3843.

Equation provenance: issue #3843, citing Peabody (2001), Control of Pipeline
Corrosion, 2nd ed., and von Baeckmann et al. (1997), Handbook of Cathodic
Corrosion Protection, 3rd ed. No standards compliance or protection verdict
is implied. All shifts are positive magnitudes in V; material properties
and their area-specific coating basis must be supplied explicitly.
"""

import math
from dataclasses import dataclass, field

from digitalmodel.cathodic_protection import stray_current


class AttenuationValidationError(ValueError):
    """Invalid screening inputs or an unrepresentable numerical result."""


@dataclass(frozen=True)
class ScreeningResult:
    """Serializable equation basis and assumptions, with no approval claim."""

    status: str = field(init=False, default="screening_only")
    assumptions: tuple[str, ...] = field(init=False, default=(
        "uniform coating", "constant linear leakage", "remote earth"))
    equation_basis: str = field(init=False, default="Issue #3843 transmission-line equations")


@dataclass(frozen=True)
class LineParameters(ScreeningResult):
    """Explicit input properties and unrounded SI transmission-line values."""

    pipeline_od_m: float
    wall_thickness_m: float
    steel_resistivity_ohm_m: float
    coating_resistance_ohm_m2: float
    coating_resistance_basis: str
    alpha_per_m: float
    resistance_ohm_per_m: float
    conductance_S_per_m: float
    characteristic_resistance_ohm: float
    characteristic_length_m: float


@dataclass(frozen=True)
class ProfileResult(ScreeningResult):
    """Shift and longitudinal current on one side of a drain point."""

    line: LineParameters
    shift_V: float
    one_sided_current_A: float


@dataclass(frozen=True)
class SpacingResult(ScreeningResult):
    """Equal interior drains; two-sided current is not a terminal-drain load."""

    line: LineParameters
    spacing_m: float
    interior_drain_current_A: float
    no_overlap_span_m: float


def _validate(value: float, name: str, *, zero: bool = False) -> float:
    if not math.isfinite(value) or value < 0 or (value == 0 and not zero):
        raise AttenuationValidationError(f"{name} must be finite and {'nonnegative' if zero else 'positive'}")
    return value


def _record(kind, line, **values):
    for name, value in values.items():
        _validate(value, name, zero=True)
    return kind(line=line, **values)


def line_parameters(
    pipeline_od_m: float, wall_thickness_m: float,
    steel_resistivity_ohm_m: float, coating_resistance_ohm_m2: float,
    coating_resistance_basis: str, *, experimental: bool = False,
) -> LineParameters:
    """Reuse the public alpha kernel and preserve its experimental opt-in.

    Geometry is in m, steel resistivity in ohm.m and coating resistance in
    ohm.m². Its caller-stated basis is required; no material defaults apply.
    """
    properties = dict(pipeline_od_m=pipeline_od_m, wall_thickness_m=wall_thickness_m,
                      steel_resistivity_ohm_m=steel_resistivity_ohm_m,
                      coating_resistance_ohm_m2=coating_resistance_ohm_m2)
    for name, value in properties.items():
        _validate(value, name)
    if wall_thickness_m >= pipeline_od_m / 2:
        raise AttenuationValidationError("wall_thickness_m must be less than half the OD")
    if not isinstance(coating_resistance_basis, str) or not coating_resistance_basis.strip():
        raise AttenuationValidationError("coating_resistance_basis must be nonblank")
    try:
        alpha = stray_current.pipe_attenuation_constant(
            pipeline_od_m, wall_thickness_m, coating_resistance_ohm_m2,
            steel_resistivity_ohm_m, experimental=experimental)
        g = _validate(math.pi * pipeline_od_m / coating_resistance_ohm_m2, "conductance")
        _validate(alpha, "alpha")
        derived = dict(alpha_per_m=alpha, resistance_ohm_per_m=alpha**2 / g,
                       conductance_S_per_m=g, characteristic_resistance_ohm=alpha / g,
                       characteristic_length_m=1 / alpha)
        for name, value in derived.items():
            _validate(value, name)
    except (OverflowError, ZeroDivisionError) as error:
        raise AttenuationValidationError("line parameters exceed numerical range") from error
    return LineParameters(**properties, coating_resistance_basis=coating_resistance_basis, **derived)


def infinite_profile(line: LineParameters, drain_shift_V: float, x_m: float) -> ProfileResult:
    """Infinite half-line: E=E_A exp(-alpha*x), I=E/Z0, x>=0."""
    _validate(drain_shift_V, "drain_shift_V")
    _validate(x_m, "x_m", zero=True)
    shift = drain_shift_V * math.exp(-line.alpha_per_m * x_m)
    return _record(ProfileResult, line, shift_V=shift,
                   one_sided_current_A=shift / line.characteristic_resistance_ohm)


def insulated_end_profile(
    line: LineParameters, drain_shift_V: float, x_m: float, length_m: float,
) -> ProfileResult:
    """Finite half-line ending at I(L)=0, with 0<=x<=L (including L=0).

    E/E_A=cosh(alpha*(L-x))/cosh(alpha*L); the current uses the corresponding
    sinh ratio. Exponential ratios avoid hyperbolic overflow and expm1
    preserves small-length currents.
    """
    _validate(drain_shift_V, "drain_shift_V")
    _validate(x_m, "x_m", zero=True)
    _validate(length_m, "length_m", zero=True)
    if x_m > length_m:
        raise AttenuationValidationError("x_m must not exceed length_m")
    a = line.alpha_per_m
    scale = math.exp(-a * x_m) / (1 + math.exp(-2 * a * length_m))
    tail = -2 * a * (length_m - x_m)
    return _record(ProfileResult, line, shift_V=drain_shift_V * (scale * (1 + math.exp(tail))),
                   one_sided_current_A=(drain_shift_V * (scale * -math.expm1(tail))
                                        / line.characteristic_resistance_ohm))


def drain_point_spacing(
    line: LineParameters, drain_shift_V: float, minimum_shift_V: float,
) -> SpacingResult:
    """Maximum equal-drain spacing and two-sided interior load, E_A>=E_min>0.

    E_mid=E_A/cosh(alpha*S/2). No-overlap span uses independent infinite
    half-lines; equality gives a degenerate zero span and current.
    """
    _validate(drain_shift_V, "drain_shift_V")
    _validate(minimum_shift_V, "minimum_shift_V")
    if drain_shift_V < minimum_shift_V:
        raise AttenuationValidationError("drain_shift_V must be >= minimum_shift_V")
    ratio = _validate(drain_shift_V / minimum_shift_V, "shift ratio")
    half_length = math.acosh(ratio)
    return _record(SpacingResult, line, spacing_m=2 * half_length / line.alpha_per_m,
                   interior_drain_current_A=(drain_shift_V * math.tanh(half_length)
                                             / line.characteristic_resistance_ohm * 2),
                   no_overlap_span_m=2 * math.log(ratio) / line.alpha_per_m)
