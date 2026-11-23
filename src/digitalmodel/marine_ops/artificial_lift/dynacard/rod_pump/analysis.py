# ABOUTME: Public entry point for API RP 11L rod-pump surface-card analysis.
# ABOUTME: Fails closed outside the validity envelope rather than extrapolating.
"""End-to-end API RP 11L analysis of a rod-pumped well.

This ties the rod-string mechanics, kinematics and card metrics together
behind one call, and enforces the validity envelope before returning anything.
The guiding rule is that the analysis refuses rather than extrapolates: a
correlation used outside its range produces a number that looks like every
other number in the report.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import List, Optional, Sequence

from .card_metrics import CardMetrics, analyse_card
from .constants import (
    ENVELOPE_MAX_VALID,
    ENVELOPE_WAVE_DOMINATED,
    SONIC_VELOCITY_STEEL_FT_S,
    SUPPORTED_UNIT_GEOMETRIES,
    UNSUPPORTED_UNIT_GEOMETRIES,
)
from .kinematics import (
    natural_frequency,
    peak_interval,
    taper_adjusted_natural_frequency,
    undulations_per_half_stroke,
)
from .rod_string import (
    RodStringAnalysis,
    analyse_rod_string,
    plunger_stroke,
    pump_displacement,
    volumetric_efficiency,
)


class ValidityError(ValueError):
    """Raised when inputs fall outside the RP 11L correlations' envelope."""


@dataclass
class RodPumpAnalysis:
    """Complete RP 11L analysis of one well."""

    # Kinematics
    natural_frequency_spm: float          # No
    taper_adjusted_frequency_spm: float   # No'
    peak_interval_s: float                # dt = 60/No'
    speed_ratio: float                    # N/No'
    undulations_per_half_stroke: float    # n

    # Rod string and loads
    rod_string: RodStringAnalysis

    # Pump
    plunger_stroke_in: float              # Sp
    rod_stretch_in: float
    pump_displacement_bpd: float          # PD
    volumetric_efficiency: Optional[float]

    # Card
    card: Optional[CardMetrics] = None

    # Everything the analysis could not determine, stated rather than assumed
    warnings: List[str] = field(default_factory=list)
    undetermined: List[str] = field(default_factory=list)


def _check_envelope(
    speed_ratio: float,
    sonic_velocity_ft_s: float,
    taper_factor: Optional[float],
    is_tapered: bool,
    unit_geometry: str,
) -> List[str]:
    """Enforce the validity envelope. Returns advisory notes; raises to refuse."""
    notes: List[str] = []

    geometry = (unit_geometry or "conventional").strip().lower().replace(" ", "_")
    if geometry in UNSUPPORTED_UNIT_GEOMETRIES:
        raise ValidityError(
            f"pumping-unit geometry '{unit_geometry}' is not a Class I "
            "crank-balanced unit. The API RP 11L correlations assume "
            "conventional geometry; Mark II, RotaFlex and hydraulic "
            "long-stroke units have different kinematics and must not be "
            "analysed with them."
        )
    if geometry not in SUPPORTED_UNIT_GEOMETRIES:
        notes.append(
            f"pumping-unit geometry '{unit_geometry}' is unrecognised; "
            "proceeding on the assumption it is Class I crank-balanced"
        )

    if is_tapered and taper_factor is None:
        raise ValidityError(
            "a tapered rod string requires an explicit frequency taper factor "
            "Fc. Fc = 1.000 applies only to a single-diameter string; using it "
            "for a taper misstates the natural frequency and every timing "
            "result that follows."
        )

    if abs(sonic_velocity_ft_s - SONIC_VELOCITY_STEEL_FT_S) > 1.0:
        notes.append(
            f"sonic velocity {sonic_velocity_ft_s} ft/s is not steel "
            f"({SONIC_VELOCITY_STEEL_FT_S}); the RP 11L 245,000 constant "
            "assumes steel and the natural-frequency cross-check will reflect "
            "the difference"
        )

    if speed_ratio > ENVELOPE_MAX_VALID:
        raise ValidityError(
            f"N/No' = {speed_ratio:.3f} exceeds {ENVELOPE_MAX_VALID}. "
            "Resonance effects dominate and the API RP 11L correlations are "
            "invalid here. Refusing to extrapolate — use a wave-equation "
            "solver instead."
        )
    if speed_ratio < ENVELOPE_WAVE_DOMINATED:
        notes.append(
            f"N/No' = {speed_ratio:.3f} is below {ENVELOPE_WAVE_DOMINATED}: the "
            "surface card is wave-dominated and will show pronounced load "
            "undulations. This is expected behaviour, not a pump fault."
        )
    return notes


def analyse(
    rod_diameter_in: float,
    rod_length_ft: float,
    surface_stroke_in: float,
    strokes_per_minute: float,
    plunger_diameter_in: float,
    fluid_level_ft: float,
    specific_gravity: float,
    tubing_pressure_psi: float = 0.0,
    casing_pressure_psi: float = 0.0,
    taper_factor: Optional[float] = None,
    is_tapered: bool = False,
    overtravel_in: float = 0.0,
    sonic_velocity_ft_s: float = SONIC_VELOCITY_STEEL_FT_S,
    unit_geometry: str = "conventional",
    rod_weight_lb_per_ft: Optional[float] = None,
    card_position_in: Optional[Sequence[float]] = None,
    card_load_lb: Optional[Sequence[float]] = None,
    measured_rate_bpd: Optional[float] = None,
    runtime_hours_per_day: Optional[float] = None,
    formation_volume_factor: Optional[float] = None,
) -> RodPumpAnalysis:
    """Run the full RP 11L analysis for one well.

    Args:
        rod_diameter_in: Rod body diameter. Single-diameter strings only;
            supply ``taper_factor`` and set ``is_tapered`` otherwise.
        rod_length_ft: Total rod string length.
        surface_stroke_in: Polished-rod stroke, ``S``.
        strokes_per_minute: Pumping speed, ``N``.
        plunger_diameter_in: Pump bore.
        fluid_level_ft: Fluid column height above the pump.
        specific_gravity: Produced-fluid specific gravity.
        taper_factor: ``Fc``. Required when ``is_tapered``.
        overtravel_in: Plunger overtravel; zero gives the conservative stroke.
        unit_geometry: Pumping-unit class. Non-Class-I geometries are refused.
        card_position_in / card_load_lb: Optional surface card, enabling card
            metrics and the load datum check.
        measured_rate_bpd / runtime_hours_per_day / formation_volume_factor:
            Needed for volumetric efficiency. When runtime or ``Bo`` is
            missing, efficiency is reported as ``None`` and listed under
            ``undetermined`` rather than silently assumed.

    Returns:
        A populated :class:`RodPumpAnalysis`.

    Raises:
        ValidityError: If the case falls outside the correlations' envelope.
    """
    no_ = natural_frequency(rod_length_ft, sonic_velocity_ft_s)
    effective_taper = 1.0 if taper_factor is None else taper_factor
    no_prime = taper_adjusted_natural_frequency(no_, effective_taper)
    speed_ratio = strokes_per_minute / no_prime

    notes = _check_envelope(
        speed_ratio, sonic_velocity_ft_s, taper_factor, is_tapered, unit_geometry
    )

    string = analyse_rod_string(
        diameter_in=rod_diameter_in,
        length_ft=rod_length_ft,
        surface_stroke_in=surface_stroke_in,
        plunger_diameter_in=plunger_diameter_in,
        fluid_level_ft=fluid_level_ft,
        specific_gravity=specific_gravity,
        tubing_pressure_psi=tubing_pressure_psi,
        casing_pressure_psi=casing_pressure_psi,
        weight_lb_per_ft=rod_weight_lb_per_ft,
    )

    stretch = string.fluid_load_lb / string.spring_rate_lb_per_in
    s_p = plunger_stroke(
        surface_stroke_in, string.fluid_load_lb, string.spring_rate_lb_per_in,
        overtravel_in,
    )
    p_d = pump_displacement(plunger_diameter_in, s_p, strokes_per_minute)

    undetermined: List[str] = []
    efficiency = None
    if measured_rate_bpd is not None:
        efficiency = volumetric_efficiency(
            measured_rate_bpd, p_d, runtime_hours_per_day, formation_volume_factor
        )
        if efficiency is None:
            missing = [
                name
                for name, value in (
                    ("runtime_hours_per_day", runtime_hours_per_day),
                    ("formation_volume_factor", formation_volume_factor),
                )
                if value is None
            ]
            undetermined.append(
                "volumetric efficiency: requires "
                f"{' and '.join(missing)}. A unit cycling on a controller has "
                "a duty cycle that mimics low fillage, and surface barrels are "
                "not reservoir barrels — neither is assumed."
            )

    card = None
    if card_position_in is not None and card_load_lb is not None:
        card = analyse_card(
            card_position_in,
            card_load_lb,
            strokes_per_minute,
            rod_weight_in_air_lb=string.weight_in_air_lb,
            buoyant_rod_weight_lb=string.buoyant_weight_lb,
        )
        notes.extend(card.warnings)

    return RodPumpAnalysis(
        natural_frequency_spm=no_,
        taper_adjusted_frequency_spm=no_prime,
        peak_interval_s=peak_interval(no_prime),
        speed_ratio=speed_ratio,
        undulations_per_half_stroke=undulations_per_half_stroke(
            strokes_per_minute, no_prime
        ),
        rod_string=string,
        plunger_stroke_in=s_p,
        rod_stretch_in=stretch,
        pump_displacement_bpd=p_d,
        volumetric_efficiency=efficiency,
        card=card,
        warnings=notes,
        undetermined=undetermined,
    )
