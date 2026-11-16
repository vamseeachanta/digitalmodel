"""Supplied-input vertical motion, stroke, restoring response and deck clearance.

The method is extracted from a legacy engineering workbook. All lengths are m,
forces N, stiffnesses N/m, weight densities N/m^3 and periods s. Ratios, counts
and load factors are dimensionless. Convert boundary units with the existing
``digitalmodel.common.units`` or ``digitalmodel.units`` converters.

Method
------
Rectangular column areas are summed; existing hydrostatic stiffness and period
functions calculate the parallel-spring response. Effective oscillating mass is
inferred from the supplied baseline period and is held constant. Motion scales
linearly with response ratio and supplied wave-height factor. Upward and downward
allowances are summed separately. Deck clearance reuses the existing air-gap
arithmetic with an upward deck translation. Buoyancy load is restoring stiffness
times immersion; existing elastic elongation calculates the extension.

Source references and Input gaps
-------------------------------
The source names API 2Met and API RP 2MET: clause not stated in source. A metocean
table is labelled Table C.31; no standard edition or calculation clause is given.
These references identify environmental input provenance, not qualified design
criteria. No standards-derived constant or table is embedded here.

Required supplied inputs include weight density (force per volume, not mass
density), lumped axial stiffness, baseline period, response ratios, wave-height
ratios, dynamic load factor, motion allowances, elevations, datum, stop position
and clearance criterion. Their qualification is not established by this method.
Rounded source unit coefficients are replaced by exact boundary conversion.

Limits and what is left
----------------------
Linear independent parallel springs, equal restraint load sharing and unchanged
effective mass are assumed. Wave motion uses a scalar amplitude ratio: no phase,
frequency response, damping, coupled dynamics or stress calculation is provided.
Positive upward heave at the crest is a supplied scenario, not a worst-case proof.
Negative clearance and stop margins are retained. Metocean tables, supplied
stress/deflection plot data and the empirical wave-length approximation are left
to the caller; existing dispersion functions remain available separately.
"""

from __future__ import annotations

from dataclasses import dataclass, fields
from math import isfinite, pi

from digitalmodel.asset_integrity.air_gap import (
    AirGapBasis, AirGapResult, Elevation, calculate_air_gap,
)
from digitalmodel.marine_ops.marine_analysis.catenary.utils import calculate_elongation
from digitalmodel.solvers.openfoam.validation.floating_body import (
    GRAVITY, heave_stiffness, hydrostatic_heave_period,
)


def _finite(value: float, name: str) -> None:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a finite number")
    try:
        valid = isfinite(value)
    except OverflowError:
        valid = False
    if not valid:
        raise ValueError(f"{name} must be a finite number")


def _validate(record, *, positive=(), counts=(), signed=()) -> None:
    for field in fields(record):
        name = field.name
        value = getattr(record, name)
        if name == "datum":
            if not isinstance(value, str) or not value.strip():
                raise ValueError("datum must be an explicit nonblank label")
            continue
        _finite(value, name)
        if name in counts and not isinstance(value, int):
            raise ValueError(f"{name} must be an integer")
        if name not in signed and (value < 0 or (name in positive and value == 0)):
            raise ValueError(f"{name} has an invalid magnitude")


def _checked_result(result):
    for field in fields(result):
        value = getattr(result, field.name)
        if value is not None:
            _finite(value, field.name)
    return result


@dataclass(frozen=True)
class RestoringInput:
    """Identical rectangular columns and parallel axial restraints, all SI."""

    column_length_m: float
    column_width_m: float
    column_count: int
    weight_density_n_m3: float
    restraint_stiffness_n_m: float
    restraint_count: int
    baseline_period_s: float

    def __post_init__(self):
        _validate(self, positive=("column_length_m", "column_width_m", "column_count",
                                 "weight_density_n_m3", "restraint_stiffness_n_m",
                                 "baseline_period_s"),
                  counts=("column_count", "restraint_count"))


@dataclass(frozen=True)
class RestoringResult:
    waterplane_area_m2: float
    waterplane_stiffness_n_m: float
    restraint_stiffness_n_m: float
    waterplane_to_restraint_ratio: float | None
    period_s: float


def restoring_response(inputs: RestoringInput) -> RestoringResult:
    """Return k_w, k_r and T = T_baseline / sqrt(1 + k_r/k_w).

    Existing reference functions are used with rho = supplied weight density / g
    and inferred mass = k_w * (T_baseline / (2*pi))^2. This is an input-form
    adapter, without a new density, gravity or added-mass assumption.
    The stiffness ratio is undefined (None) when there are no restraints.
    """
    area = inputs.column_length_m * inputs.column_width_m * inputs.column_count
    _finite(area, "waterplane_area_m2")
    waterplane = heave_stiffness(area, rho=inputs.weight_density_n_m3 / GRAVITY)
    axial = inputs.restraint_stiffness_n_m * inputs.restraint_count
    _finite(waterplane, "waterplane_stiffness_n_m")
    _finite(axial, "restraint_stiffness_n_m")
    try:
        mass = waterplane * (inputs.baseline_period_s / (2 * pi)) ** 2
        _finite(mass, "inferred_mass_kg")
        period = hydrostatic_heave_period(mass, waterplane + axial)
    except (OverflowError, ZeroDivisionError) as exc:
        raise ValueError("restoring response is outside finite arithmetic") from exc
    if period <= 0:
        raise ValueError("derived period must remain positive")
    return _checked_result(RestoringResult(
        area, waterplane, axial, waterplane / axial if axial else None, period,
    ))


@dataclass(frozen=True)
class WaveMotionInput:
    """Wave height and required amplitude response and height multipliers."""

    wave_height_m: float
    response_ratio: float
    height_multiplier: float

    def __post_init__(self):
        _validate(self, positive=("height_multiplier",))


@dataclass(frozen=True)
class WaveMotionResult:
    total_stroke_m: float
    amplitude_m: float


def wave_motion(inputs: WaveMotionInput) -> WaveMotionResult:
    """Return stroke = height * response * height multiplier; amplitude = stroke/2."""
    stroke = inputs.wave_height_m * inputs.response_ratio * inputs.height_multiplier
    return _checked_result(WaveMotionResult(stroke, stroke / 2))


@dataclass(frozen=True)
class MotionScaleInput:
    """Nonnegative excursion with its reference and target response ratios."""

    displacement_m: float
    reference_response_ratio: float
    target_response_ratio: float
    height_ratio: float

    def __post_init__(self):
        _validate(self, positive=("reference_response_ratio", "height_ratio"))


def scale_motion(inputs: MotionScaleInput) -> float:
    """Return displacement * target/reference response / supplied height ratio."""
    result = (inputs.displacement_m * inputs.target_response_ratio
              / inputs.reference_response_ratio / inputs.height_ratio)
    _finite(result, "scaled_displacement_m")
    return result


@dataclass(frozen=True)
class StrokeBudgetInput:
    """Positive motion allowances; upper stop is a signed upward position in m."""

    tide_upstroke_m: float
    wave_upstroke_m: float
    offset_downstroke_m: float
    wave_downstroke_m: float
    current_downstroke_m: float
    surge_downstroke_m: float
    tide_downstroke_m: float
    upper_stop_m: float

    def __post_init__(self):
        _validate(self, signed=("upper_stop_m",))


@dataclass(frozen=True)
class StrokeBudgetResult:
    upstroke_m: float
    downstroke_m: float
    total_stroke_m: float
    net_upstroke_m: float
    stop_margin_m: float


def stroke_budget(inputs: StrokeBudgetInput) -> StrokeBudgetResult:
    """Sum directional allowances; net upstroke subtracts non-wave down allowances.

    Stop margin = upper stop - net upstroke. A negative margin denotes travel
    beyond the supplied stop under this algebraic combination, not a stress check.
    """
    up = inputs.tide_upstroke_m + inputs.wave_upstroke_m
    static_down = (inputs.offset_downstroke_m + inputs.current_downstroke_m
                   + inputs.surge_downstroke_m + inputs.tide_downstroke_m)
    down = static_down + inputs.wave_downstroke_m
    net_up = up - static_down
    return _checked_result(StrokeBudgetResult(
        up, down, up + down, net_up, inputs.upper_stop_m - net_up,
    ))


@dataclass(frozen=True)
class DeckClearanceInput:
    """Elevations sharing the supplied datum; heave is positive upward, in m."""

    deck_elevation_m: float
    water_elevation_m: float
    crest_height_m: float
    upward_heave_m: float
    required_clearance_m: float
    datum: str

    def __post_init__(self):
        _validate(self, signed=("deck_elevation_m", "water_elevation_m"))


def clearance_with_heave(inputs: DeckClearanceInput) -> AirGapResult:
    """Adapt upward deck translation to existing deck-minus-water-minus-crest arithmetic."""
    basis = AirGapBasis(
        Elevation(inputs.deck_elevation_m + inputs.upward_heave_m, inputs.datum),
        Elevation(inputs.water_elevation_m, inputs.datum),
        inputs.crest_height_m, inputs.required_clearance_m,
    )
    return calculate_air_gap(basis, subsidence_m=0.0)


@dataclass(frozen=True)
class BuoyancyStretchInput:
    """Immersed prismatic area and equal linear axial restraints, all SI."""

    waterplane_area_m2: float
    weight_density_n_m3: float
    immersion_m: float
    restraint_stiffness_n_m: float
    restraint_count: int
    dynamic_load_factor: float

    def __post_init__(self):
        _validate(self, positive=("waterplane_area_m2", "weight_density_n_m3",
                                 "restraint_stiffness_n_m", "restraint_count",
                                 "dynamic_load_factor"), counts=("restraint_count",))


@dataclass(frozen=True)
class BuoyancyStretchResult:
    buoyancy_force_n: float
    quasistatic_extension_m: float
    dynamic_extension_m: float
    dynamic_load_per_restraint_n: float


def buoyancy_stretch(inputs: BuoyancyStretchInput) -> BuoyancyStretchResult:
    """Return F = gamma*A*z, extension F/(n*k), dynamic extension and load share.

    The existing TL/EA elongation function accepts EA rather than spring k.
    A unit reference length L = 1 m and EA = k_total * L give exactly F/k_total;
    that reference length cancels and is not an assumed physical riser length.
    """
    stiffness = heave_stiffness(
        inputs.waterplane_area_m2, rho=inputs.weight_density_n_m3 / GRAVITY,
    )
    total_axial = inputs.restraint_stiffness_n_m * inputs.restraint_count
    force = stiffness * inputs.immersion_m
    for name, value in (("waterplane_stiffness_n_m", stiffness),
                        ("total_axial_stiffness_n_m", total_axial), ("force_n", force)):
        _finite(value, name)
    extension = calculate_elongation(force, length=1.0, ea_stiffness=total_axial)
    return _checked_result(BuoyancyStretchResult(
        force, extension, extension * inputs.dynamic_load_factor,
        force * inputs.dynamic_load_factor / inputs.restraint_count,
    ))
