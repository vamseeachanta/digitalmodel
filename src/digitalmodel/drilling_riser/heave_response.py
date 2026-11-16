"""Supplied-data heave, directional stroke and parallel-riser stretch methods.

Inputs and outputs use SI: lengths m, stiffness N/m, force N, density kg/m3,
gravity m/s2 and period s. Counts and ratios are dimensionless. Inputs are frozen
records; validation occurs when a calculation is called. International-foot
boundaries use the existing ``digitalmodel.common.units.length_in_metres``.

Method from a legacy engineering workbook:
* Rectangular column area A=L*B; heave restoring K=rho*g*A*column_count.
  The restoring calculation reuses ``create_hydrostatic_restoring``.
* With unchanged effective mass, T=T0/sqrt(1+Kr/Kw), Kr=n*k. A thin adapter
  converts the supplied baseline period/stiffness to equivalent mass/area for
  the existing ``natural_heave_period`` calculation.
* Wave strokes scale by target/reference RAO and a supplied wave divisor;
  static tide, offset, current and surge strokes are then added by direction.
* Total heave stroke=H*RAO; amplitude is half that stroke. A supplied
  peak/significant ratio scales the wave contribution only.
* An explicitly supplied whole-stroke factor can instead scale an assembled
  budget, including its static terms; it does not infer a wave-height ratio.
* Imposed heave produces F=Kw*z, extension=F/(n*k), dynamic extension=D*F/(n*k),
  and additional dynamic load per identical riser=D*F/n.
* Deck motion shifts the deck elevation before existing air-gap arithmetic.

Standards as stated in the source: API 2Met and API RP 2MET, clause not stated
in source. The supporting environmental table is labelled Table C.31; no
edition or equation citation is supplied. These annotations identify the
environmental inputs, not a qualification of the response formulas above.
No standard-derived constants or environmental tables are embedded here.

Limits: linear uncoupled heave, fixed effective mass and waterplane, identical
parallel axial springs, and supplied scalar RAOs. Directional budgets are
arithmetic envelopes, not a simultaneous time history. Deck/crest phasing must
be supplied consistently. No nonlinear hydrodynamics, structural stress,
fatigue, capacity or operational acceptance is established by these methods.

Input gaps: standard edition and governing clauses; provenance of geometry,
density, gravity, stiffness and baseline period; RAO frequency/heading and phase;
peak/significant ratios, wave divisors and dynamic factors; stroke allowances,
elevation datum and minimum clearance criterion. All numerical inputs and factors
are required without defaults. Rounded legacy length factors are not reproduced.
Environmental fitting, an uncited wavelength approximation, and static stress
plot data remain outside these methods.
"""

from __future__ import annotations

from dataclasses import dataclass, replace
from math import isfinite, pi

from digitalmodel.asset_integrity.air_gap import (
    AirGapBasis, AirGapResult, Elevation, calculate_air_gap,
)
from digitalmodel.naval_architecture.seakeeping import G, RHO_SW, natural_heave_period
from digitalmodel.orcawave.hydro_coefficients import create_hydrostatic_restoring


def _number(value: float, name: str, *, minimum: float | None = None) -> None:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a finite number")
    try:
        finite = isfinite(value)
    except OverflowError as exc:
        raise ValueError(f"{name} must be finite and representable") from exc
    if not finite or (minimum is not None and value < minimum):
        raise ValueError(f"{name} must be finite and >= {minimum}")


def _positive(value: float, name: str) -> None:
    _number(value, name, minimum=0.0)
    if value == 0:
        raise ValueError(f"{name} must be positive")


def _count(value: int, name: str, *, allow_zero: bool = False) -> None:
    if isinstance(value, bool) or not isinstance(value, int):
        raise ValueError(f"{name} must be an integer count")
    _number(value, name, minimum=0 if allow_zero else 1)


@dataclass(frozen=True)
class WaterplaneInput:
    """Identical rectangular columns; geometry m, density kg/m3, gravity m/s2."""

    column_length_m: float
    column_width_m: float
    column_count: int
    water_density_kg_m3: float
    gravity_m_s2: float


@dataclass(frozen=True)
class WaterplaneResult:
    """Per-column and combined waterplane area m2 and stiffness N/m."""

    column_area_m2: float
    total_area_m2: float
    column_stiffness_n_m: float
    total_stiffness_n_m: float


def waterplane_response(inputs: WaterplaneInput) -> WaterplaneResult:
    """Return A=L*B and K=rho*g*A, reusing hydrostatic restoring C33.

    Only C33 is used; zero volume and metacentric heights disable unused axes.
    Density and gravity are supplied explicitly to override existing defaults.
    """
    for name in ("column_length_m", "column_width_m", "water_density_kg_m3", "gravity_m_s2"):
        _positive(getattr(inputs, name), name)
    _count(inputs.column_count, "column_count")
    area = inputs.column_length_m * inputs.column_width_m
    _positive(area, "column_area_m2")
    # Check scalar intermediates before NumPy is entered, including overflow.
    specific_weight = inputs.water_density_kg_m3 * inputs.gravity_m_s2
    _positive(specific_weight, "specific_weight_n_m3")
    _positive(specific_weight * area, "column_stiffness_n_m")
    stiffness = create_hydrostatic_restoring(
        0.0, area, 0.0, 0.0,
        rho=inputs.water_density_kg_m3, g=inputs.gravity_m_s2,
    )[2][2]
    total_area = area * inputs.column_count
    total_stiffness = stiffness * inputs.column_count
    _positive(total_area, "total_area_m2")
    _positive(total_stiffness, "total_stiffness_n_m")
    return WaterplaneResult(area, total_area, stiffness, total_stiffness)


@dataclass(frozen=True)
class PeriodInput:
    """Baseline period s, restoring and per-riser stiffness N/m, riser count."""

    baseline_period_s: float
    waterplane_stiffness_n_m: float
    riser_stiffness_n_m: float
    riser_count: int


@dataclass(frozen=True)
class PeriodResult:
    """Combined riser stiffness N/m, dimensionless Kw/Kr and adjusted period s.

    The ratio is None when total riser stiffness is zero; the period is T0.
    """

    total_riser_stiffness_n_m: float
    waterplane_to_riser_ratio: float | None
    period_s: float


def heave_period_with_risers(inputs: PeriodInput) -> PeriodResult:
    """Adapt T0 and Kw to the existing oscillator for T=T0*sqrt(Kw/(Kw+n*k)).

    Effective mass=(T0/(2*pi))**2*Kw is held fixed. Equivalent area=(Kw+n*k)/
    (rho*g) uses the existing function's own normalization, which cancels; no
    environmental density or gravity is inferred for this adjustment.
    """
    _positive(inputs.baseline_period_s, "baseline_period_s")
    _positive(inputs.waterplane_stiffness_n_m, "waterplane_stiffness_n_m")
    _number(inputs.riser_stiffness_n_m, "riser_stiffness_n_m", minimum=0.0)
    _count(inputs.riser_count, "riser_count", allow_zero=True)
    kr = inputs.riser_stiffness_n_m * inputs.riser_count
    _number(kr, "total_riser_stiffness_n_m", minimum=0.0)
    if kr == 0:
        return PeriodResult(kr, None, inputs.baseline_period_s)
    ratio = inputs.waterplane_stiffness_n_m / kr
    _positive(ratio, "waterplane_to_riser_ratio")
    mass_scale = inputs.baseline_period_s / (2 * pi)
    mass_kg = mass_scale * mass_scale * inputs.waterplane_stiffness_n_m
    area_m2 = (inputs.waterplane_stiffness_n_m + kr) / (RHO_SW * G)
    _positive(mass_kg, "effective_mass_kg")
    _positive(area_m2, "equivalent_area_m2")
    # The existing API accepts tonnes; this is an exact SI-prefix conversion.
    mass_tonnes = mass_kg / 1000.0
    _positive(mass_tonnes, "effective_mass_tonnes")
    period = natural_heave_period(mass_tonnes, area_m2)
    _positive(period, "period_s")
    return PeriodResult(kr, ratio, period)


@dataclass(frozen=True)
class StrokeInput:
    """Nonnegative directional strokes m; RAOs and wave divisor dimensionless.

    Static terms are absolute stroke contributions, not signed elevations.
    The wave divisor converts the supplied wave basis only, leaving statics fixed.
    """

    wave_upstroke_m: float
    wave_downstroke_m: float
    reference_rao_ratio: float
    target_rao_ratio: float
    wave_divisor_ratio: float
    tide_upstroke_m: float
    offset_downstroke_m: float
    current_downstroke_m: float
    surge_downstroke_m: float
    tide_downstroke_m: float


@dataclass(frozen=True)
class StrokeResult:
    """Wave and combined stroke magnitudes m; net upstroke is signed upward."""

    wave_upstroke_m: float
    wave_downstroke_m: float
    upstroke_m: float
    downstroke_m: float
    total_stroke_m: float
    net_upstroke_m: float


def stroke_budget(inputs: StrokeInput) -> StrokeResult:
    """Scale waves by target/reference/divisor; sum fixed strokes by direction.

    Net upstroke equals total upstroke minus the static downward terms; wave
    downstroke belongs to the opposite excursion and is excluded from that net.
    """
    for name in inputs.__dataclass_fields__:
        _number(getattr(inputs, name), name, minimum=0.0)
    _positive(inputs.reference_rao_ratio, "reference_rao_ratio")
    _positive(inputs.wave_divisor_ratio, "wave_divisor_ratio")
    scale = inputs.target_rao_ratio / inputs.reference_rao_ratio / inputs.wave_divisor_ratio
    _number(scale, "wave_scale_ratio", minimum=0.0)
    wave_up = inputs.wave_upstroke_m * scale
    wave_down = inputs.wave_downstroke_m * scale
    static_down = (
        inputs.offset_downstroke_m + inputs.current_downstroke_m
        + inputs.surge_downstroke_m + inputs.tide_downstroke_m
    )
    up = wave_up + inputs.tide_upstroke_m
    down = wave_down + static_down
    total = up + down
    net_up = up - static_down
    for name, value in zip(
        StrokeResult.__dataclass_fields__, (wave_up, wave_down, up, down, total, net_up)
    ):
        _number(value, name)
    return StrokeResult(wave_up, wave_down, up, down, total, net_up)


@dataclass(frozen=True)
class WaveMotionInput:
    """Significant wave height m and supplied RAO/peak ratios, dimensionless."""

    wave_height_m: float
    heave_rao_ratio: float
    peak_to_significant_ratio: float


@dataclass(frozen=True)
class WaveMotionResult:
    """Significant and maximum heave stroke and single amplitude, in metres."""

    significant_total_stroke_m: float
    significant_amplitude_m: float
    maximum_total_stroke_m: float
    maximum_amplitude_m: float


def wave_motion(inputs: WaveMotionInput) -> WaveMotionResult:
    """Return stroke=H*RAO and amplitude=stroke/2 on each supplied wave basis."""
    _number(inputs.wave_height_m, "wave_height_m", minimum=0.0)
    _number(inputs.heave_rao_ratio, "heave_rao_ratio", minimum=0.0)
    _number(inputs.peak_to_significant_ratio, "peak_to_significant_ratio", minimum=1.0)
    significant = inputs.wave_height_m * inputs.heave_rao_ratio
    maximum = significant * inputs.peak_to_significant_ratio
    _number(significant, "significant_total_stroke_m", minimum=0.0)
    _number(maximum, "maximum_total_stroke_m", minimum=0.0)
    return WaveMotionResult(significant, significant / 2, maximum, maximum / 2)


@dataclass(frozen=True)
class StrokeAmplificationInput:
    """Entire supplied stroke magnitude m and explicit dimensionless factor.

    A complete directional budget can include static terms. Choosing to amplify
    those terms is a caller-supplied assumption, not the wave-only method.
    """

    stroke_m: float
    amplification_ratio: float


def amplified_stroke(inputs: StrokeAmplificationInput) -> float:
    """Return stroke_m*amplification_ratio in metres, on the supplied basis.

    The source does not justify the whole-budget multiplier. No factor is
    selected here, and this operation does not establish a peak probability.
    """
    _number(inputs.stroke_m, "stroke_m", minimum=0.0)
    _positive(inputs.amplification_ratio, "amplification_ratio")
    result = inputs.stroke_m * inputs.amplification_ratio
    _number(result, "amplified_stroke_m", minimum=0.0)
    return result


@dataclass(frozen=True)
class StretchInput:
    """Imposed heave m, restoring/axial stiffness N/m, count and dynamic ratio."""

    waterplane_stiffness_n_m: float
    imposed_heave_m: float
    riser_stiffness_n_m: float
    riser_count: int
    dynamic_factor_ratio: float


@dataclass(frozen=True)
class StretchResult:
    """Incremental buoyancy/load N and quasi-static/dynamic extension m."""

    buoyancy_force_n: float
    quasistatic_stretch_m: float
    dynamic_stretch_m: float
    dynamic_load_per_riser_n: float


def buoyancy_stretch(inputs: StretchInput) -> StretchResult:
    """Return Kw*z/(n*k) extension and D*Kw*z/n load for identical risers.

    Kw*z is the supplied imposed-heave forcing, rather than an equilibrium
    solution with combined hull/riser restoring or an inertial response solver.
    The dynamic ratio is supplied explicitly and applies to load and extension.
    """
    for name in ("waterplane_stiffness_n_m", "riser_stiffness_n_m", "dynamic_factor_ratio"):
        _positive(getattr(inputs, name), name)
    _number(inputs.imposed_heave_m, "imposed_heave_m", minimum=0.0)
    _count(inputs.riser_count, "riser_count")
    stiffness = inputs.riser_count * inputs.riser_stiffness_n_m
    _positive(stiffness, "total_riser_stiffness_n_m")
    force = inputs.waterplane_stiffness_n_m * inputs.imposed_heave_m
    _number(force, "buoyancy_force_n", minimum=0.0)
    extension = force / stiffness
    dynamic_extension = extension * inputs.dynamic_factor_ratio
    dynamic_load = force / inputs.riser_count * inputs.dynamic_factor_ratio
    for name, value in zip(
        StretchResult.__dataclass_fields__, (force, extension, dynamic_extension, dynamic_load)
    ):
        _number(value, name, minimum=0.0)
    return StretchResult(force, extension, dynamic_extension, dynamic_load)


def deck_heave_clearance(basis: AirGapBasis, upward_heave_m: float) -> AirGapResult:
    """Shift deck elevation by signed upward heave m; reuse air-gap arithmetic.

    The caller supplies coincident crest/motion, datum and required clearance.
    No motion phase or minimum criterion is selected by this adapter.
    """
    _number(upward_heave_m, "upward_heave_m")
    # Validate the unchanged basis before using any of its values in arithmetic.
    calculate_air_gap(basis, 0.0)
    moved = replace(
        basis,
        deck_underside=Elevation(
            basis.deck_underside.value_m + upward_heave_m, basis.deck_underside.datum
        ),
    )
    return calculate_air_gap(moved, 0.0)
