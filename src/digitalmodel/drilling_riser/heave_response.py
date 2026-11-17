"""Uncoupled heave, stroke demand, deck clearance and elastic spring response.

Inputs and outputs use SI: m, s, N, N/m and fluid *weight* density N/m3.
Ratios and spring counts are dimensionless. Convert boundary quantities with
``digitalmodel.common.units.length_in_metres`` or ``digitalmodel.units.ureg``;
rounded source conversion multipliers are not engineering constants here.

Method:
- wave range = supplied height * height multiplier * heave RAO; amplitude is
  half the range. Height multipliers apply only to the wave term.
- stroke demand adds upward and downward excursions and supplied allowances;
  net upstroke subtracts downward quasi-static allowances from upward motion.
- restoring stiffness = weight density * waterplane area. Parallel axial
  stiffness changes period by T/T0 = sqrt(K0/(K0 + Ka)), at fixed effective mass.
- buoyancy force = stiffness * immersion change; elastic displacement = F/K;
  supplied dynamic amplification scales displacement and load per spring.
- moving-deck clearance adds heave_direction (+1 up, -1 down) * RAO * H/2 to deck
  elevation and delegates clearance arithmetic and its supplied minimum criterion to ``air_gap``.

Existing hydrostatic stiffness/period and catenary elongation functions are
reused. These calls perform algebra only and do not run a simulation program.

Standards stated in the source: API 2MET; API RP 2MET;
clause not stated in source. The environmental table caption mentions
Table C.31, but no equation or clause is associated with the response formulas.
No normative compliance or standard-derived constants are asserted.

Input gaps: RAO and its governing period/heading, wave-height multiplier,
fluid weight density, axial stiffness and participating spring count, dynamic
amplification, wave-period coefficient, stop settings, allowances, datum and
required clearance all require caller qualification; none is supplied by default.

Limits: linear, constant waterplane, identical parallel springs, unchanged
modal mass and caller-selected heave direction at the selected wave condition. Phase,
pitch/roll, inertia changes, damping, resonance, nonlinear hydrodynamics and
axial load limits are not calculated. An air-gap comparison evaluates only the
supplied criterion. Environmental tables, stress-displacement tables, fatigue
and equipment acceptance criteria remain outside this method.
"""

from __future__ import annotations

from dataclasses import dataclass, fields
from math import isfinite, pi

from digitalmodel.asset_integrity.air_gap import (
    AirGapBasis, AirGapResult, Elevation, calculate_air_gap,
)


def _number(value: float, name: str, *, positive: bool = False,
            signed: bool = False) -> float:
    if isinstance(value, bool) or not isinstance(value, (float, int)):
        raise ValueError(f"{name} must be a finite number")
    try:
        numeric = float(value)
    except OverflowError as exc:
        raise ValueError(f"{name} exceeds the numeric range") from exc
    if not isfinite(numeric) or (positive and numeric <= 0) or (
        not signed and numeric < 0
    ):
        raise ValueError(f"{name} has an invalid magnitude")
    return numeric


@dataclass(frozen=True)
class WaveHeaveInput:
    """Wave height in m, displacement RAO in m/m and supplied height multiplier."""

    wave_height_m: float
    heave_rao_ratio: float
    height_multiplier_ratio: float


def wave_heave_range_m(data: WaveHeaveInput) -> float:
    """Return H * height multiplier * RAO, the crest-to-trough response in m."""
    height = _number(data.wave_height_m, "wave_height_m")
    rao = _number(data.heave_rao_ratio, "heave_rao_ratio")
    factor = _number(data.height_multiplier_ratio, "height_multiplier_ratio", positive=True)
    return _number(height * factor * rao, "heave_range_m")


def wave_heave_amplitude_m(data: WaveHeaveInput) -> float:
    """Return half the range for a symmetric wave response, in m."""
    return wave_heave_range_m(data) / 2.0


@dataclass(frozen=True)
class StrokeBudgetInput:
    """Positive excursions in m; wave scaling leaves quasi-static terms unchanged."""

    wave_upstroke_m: float
    wave_downstroke_m: float
    wave_scale_ratio: float
    tide_upstroke_m: float
    offset_downstroke_m: float
    current_downstroke_m: float
    surge_downstroke_m: float
    tide_downstroke_m: float


@dataclass(frozen=True)
class StrokeBudgetResult:
    """Excursion magnitudes and signed net upstroke, all in m."""

    upstroke_m: float
    downstroke_m: float
    total_stroke_m: float
    net_upstroke_m: float


def stroke_budget(data: StrokeBudgetInput) -> StrokeBudgetResult:
    """Add scaled wave excursions and allowances; net upstroke = up - static down."""
    values = {field.name: _number(getattr(data, field.name), field.name)
              for field in fields(data)}
    static_down = (values["offset_downstroke_m"] + values["current_downstroke_m"]
                   + values["surge_downstroke_m"] + values["tide_downstroke_m"])
    up = values["wave_upstroke_m"] * values["wave_scale_ratio"] + values["tide_upstroke_m"]
    down = values["wave_downstroke_m"] * values["wave_scale_ratio"] + static_down
    values = (up, down, up + down, up - static_down)
    for value in values:
        _number(value, "stroke_result_m", signed=True)
    return StrokeBudgetResult(*values)


@dataclass(frozen=True)
class StopDeflectionInput:
    """Nonnegative upward stop and signed net upstroke, both in m."""

    up_stop_m: float
    net_upstroke_m: float


def stop_deflection_m(data: StopDeflectionInput) -> float:
    """Return stop - signed net upstroke, in m; a negative value denotes pushdown."""
    stop = _number(data.up_stop_m, "up_stop_m")
    net = _number(data.net_upstroke_m, "net_upstroke_m", signed=True)
    return _number(stop - net, "stop_deflection_m", signed=True)


@dataclass(frozen=True)
class WaterplaneInput:
    """Total participating area in m2 and supplied weight density in N/m3."""

    waterplane_area_m2: float
    fluid_weight_density_n_m3: float


def waterplane_stiffness_n_m(data: WaterplaneInput) -> float:
    """Adapt existing rho*g*A stiffness to a caller-supplied weight density."""
    from digitalmodel.solvers.openfoam.validation.floating_body import (
        GRAVITY, heave_stiffness,
    )

    area = _number(data.waterplane_area_m2, "waterplane_area_m2", positive=True)
    gamma = _number(data.fluid_weight_density_n_m3, "fluid_weight_density_n_m3", positive=True)
    # Density normalization cancels the existing helper's gravity exactly in
    # the algebra; no fluid density or gravity assumption is added to the basis.
    result = heave_stiffness(area, rho=gamma / GRAVITY)
    return _number(result, "waterplane_stiffness_n_m", positive=True)


@dataclass(frozen=True)
class PeriodShiftInput:
    """Baseline period in s; existing and added parallel stiffnesses in N/m."""

    baseline_period_s: float
    waterplane_stiffness_n_m: float
    added_axial_stiffness_n_m: float


def heave_period_with_axial_springs_s(data: PeriodShiftInput) -> float:
    """Adapt T0 to effective mass K0*(T0/(2*pi))**2, then reuse the period solver."""
    from digitalmodel.solvers.openfoam.validation.floating_body import (
        hydrostatic_heave_period,
    )

    period = _number(data.baseline_period_s, "baseline_period_s", positive=True)
    stiffness = _number(data.waterplane_stiffness_n_m, "waterplane_stiffness_n_m", positive=True)
    added = _number(data.added_axial_stiffness_n_m, "added_axial_stiffness_n_m")
    try:
        mass_kg = stiffness * (period / (2.0 * pi)) ** 2
    except OverflowError as exc:
        raise ValueError("Effective mass exceeds the numeric range") from exc
    _number(mass_kg, "effective_mass_kg", positive=True)
    total = _number(stiffness + added, "total_stiffness_n_m", positive=True)
    return _number(hydrostatic_heave_period(mass_kg, total), "period_s", positive=True)


@dataclass(frozen=True)
class BuoyancyStretchInput:
    """Area m2, weight density N/m3, nonnegative immersion increase m and K N/m."""

    waterplane_area_m2: float
    fluid_weight_density_n_m3: float
    immersion_change_m: float
    spring_stiffness_n_m: float
    spring_count: int
    dynamic_factor_ratio: float


@dataclass(frozen=True)
class BuoyancyStretchResult:
    """Total buoyancy N, displacements m and amplified per-spring force N."""

    buoyancy_force_n: float
    quasi_static_stretch_m: float
    amplified_stretch_m: float
    amplified_load_per_spring_n: float


def buoyancy_stretch(data: BuoyancyStretchInput) -> BuoyancyStretchResult:
    """Compute gamma*A*dz, reuse F*L/EA with L=1 m and EA=K*1 m, then amplify."""
    from digitalmodel.marine_ops.marine_analysis.catenary.utils import calculate_elongation

    if (isinstance(data.spring_count, bool) or not isinstance(data.spring_count, int)
            or data.spring_count <= 0):
        raise ValueError("spring_count must be a positive integer")
    count = _number(data.spring_count, "spring_count", positive=True)
    dz = _number(data.immersion_change_m, "immersion_change_m")
    spring = _number(data.spring_stiffness_n_m, "spring_stiffness_n_m", positive=True)
    factor = _number(data.dynamic_factor_ratio, "dynamic_factor_ratio", positive=True)
    stiffness = waterplane_stiffness_n_m(WaterplaneInput(
        data.waterplane_area_m2, data.fluid_weight_density_n_m3,
    ))
    force = _number(stiffness * dz, "buoyancy_force_n")
    total = _number(spring * count, "total_spring_stiffness_n_m", positive=True)
    # Deliberate adapter: L=1 m and EA=K*1 m map F*L/EA to F/K.
    stretch = calculate_elongation(force, 1.0, total)
    values = (force, stretch, stretch * factor, force * factor / count)
    for value in values:
        _number(value, "buoyancy_response")
    return BuoyancyStretchResult(*values)


@dataclass(frozen=True)
class DeckClearanceInput:
    """Deck and water elevations share datum; all lengths in m; RAO in m/m."""

    deck_elevation_m: float
    still_water_elevation_m: float
    wave_height_m: float
    heave_rao_ratio: float
    crest_height_m: float
    required_clearance_m: float
    datum: str
    heave_direction: int = 1


def moving_deck_clearance(data: DeckClearanceInput) -> AirGapResult:
    """Return deck + direction*RAO*H/2 - still water - crest and its limit margin.

    Minimum clearance requires the caller to choose the governing heave direction.
    """
    if (isinstance(data.heave_direction, bool) or not isinstance(data.heave_direction, int)
            or data.heave_direction not in (-1, 1)):
        raise ValueError("heave_direction must be +1 or -1")
    deck = _number(data.deck_elevation_m, "deck_elevation_m", signed=True)
    water = _number(data.still_water_elevation_m, "still_water_elevation_m", signed=True)
    heave = wave_heave_amplitude_m(WaveHeaveInput(
        data.wave_height_m, data.heave_rao_ratio, 1.0,
    ))
    return calculate_air_gap(AirGapBasis(
        Elevation(deck + data.heave_direction * heave, data.datum), Elevation(water, data.datum),
        data.crest_height_m, data.required_clearance_m,
    ), 0.0)


@dataclass(frozen=True)
class WavelengthInput:
    """Period in s and caller-qualified coefficient in m/s2 for lambda = c*T**2."""

    period_s: float
    coefficient_m_s2: float


def period_squared_wavelength_m(data: WavelengthInput) -> float:
    """Evaluate the supplied period-squared approximation; no depth correction.

    For deep water c = g/(2·pi); prefer
    ``digitalmodel.solvers.openfoam.validation.wave_tank.wavelength(period, depth)``
    when depth is known.
    """
    period = _number(data.period_s, "period_s", positive=True)
    coefficient = _number(data.coefficient_m_s2, "coefficient_m_s2", positive=True)
    try:
        result = coefficient * period ** 2
    except OverflowError as exc:
        raise ValueError("Wavelength exceeds the numeric range") from exc
    return _number(result, "wavelength_m", positive=True)
