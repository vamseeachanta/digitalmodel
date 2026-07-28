# ABOUTME: Rod-string elastic properties, loads, and the RP 11L dimensionless groups.
# ABOUTME: Plunger stroke is derived from rod stretch -- never assumed equal to surface stroke.
"""Rod-string mechanics for API RP 11L analysis.

The RP 11L correlations are driven by three dimensionless groups — ``Fo/Skr``,
``N/No'`` and ``Wrf/Skr`` — and everything in this module exists to compute
them from field-measurable inputs.

The one trap worth naming: **pump displacement uses plunger stroke, not
surface stroke.** Rod stretch under fluid load shortens the plunger's travel,
by 7.7 in on a 4,200 ft 3/4 in string carrying 2,100 lb of fluid load. Using
the surface stroke overstates displacement and therefore understates
volumetric efficiency.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Optional

from .constants import (
    API_ROD_SIZES,
    BUOYANCY_COEFFICIENT,
    PUMP_DISPLACEMENT_CONSTANT,
    WATER_GRADIENT_PSI_PER_FT,
    YOUNGS_MODULUS_PSI,
)


def rod_area(diameter_in: float) -> float:
    """Cross-sectional area of a rod, square inches."""
    if diameter_in <= 0:
        raise ValueError(f"rod diameter must be positive; got {diameter_in}")
    return math.pi / 4.0 * diameter_in ** 2


def rod_elastic_constant(
    diameter_in: float, modulus_psi: float = YOUNGS_MODULUS_PSI
) -> float:
    """Elastic constant ``Er = 12 / (Ar * E)``, in inches per pound per foot.

    The factor 12 converts the per-foot length basis to inches.
    """
    return 12.0 / (rod_area(diameter_in) * modulus_psi)


def spring_rate(
    diameter_in: float,
    length_ft: float,
    modulus_psi: float = YOUNGS_MODULUS_PSI,
) -> float:
    """Rod-string spring rate ``Kr = 1 / (Er * L)``, pounds per inch."""
    if length_ft <= 0:
        raise ValueError(f"rod length must be positive; got {length_ft}")
    return 1.0 / (rod_elastic_constant(diameter_in, modulus_psi) * length_ft)


def fluid_load(
    plunger_diameter_in: float,
    fluid_level_ft: float,
    specific_gravity: float,
    tubing_pressure_psi: float = 0.0,
    casing_pressure_psi: float = 0.0,
) -> float:
    """Fluid load on the plunger, pounds.

    ``Fo = 0.433 * SG * D * Ap + (Ptbg - Pcsg) * Ap``

    The hydrostatic term dominates; the wellhead differential is usually a
    small fraction of it, so a modest surface pressure swing is not evidence
    of weak pump action.

    Args:
        plunger_diameter_in: Plunger bore, inches.
        fluid_level_ft: Height of the fluid column above the pump, feet.
        specific_gravity: Produced-fluid specific gravity.
        tubing_pressure_psi: Wellhead tubing pressure.
        casing_pressure_psi: Casing pressure.
    """
    area = rod_area(plunger_diameter_in)
    hydrostatic = (
        WATER_GRADIENT_PSI_PER_FT * specific_gravity * fluid_level_ft * area
    )
    differential = (tubing_pressure_psi - casing_pressure_psi) * area
    return hydrostatic + differential


def buoyant_rod_weight(weight_in_air_lb: float, specific_gravity: float) -> float:
    """Buoyed rod weight ``Wrf = Wr * (1 - 0.128 * SG)``, pounds."""
    return weight_in_air_lb * (1.0 - BUOYANCY_COEFFICIENT * specific_gravity)


def plunger_stroke(
    surface_stroke_in: float,
    fluid_load_lb: float,
    spring_rate_lb_per_in: float,
    overtravel_in: float = 0.0,
) -> float:
    """Plunger stroke ``Sp = S - Fo/Kr + overtravel``, inches.

    ``Fo/Kr`` is the rod stretch under fluid load. Overtravel is a dynamic
    effect that partly offsets it and depends on pumping speed; it defaults to
    zero, which yields the conservative (shortest) plunger stroke.

    Raises:
        ValueError: If stretch exceeds the surface stroke, which means the
            plunger never moves and the inputs are inconsistent.
    """
    stretch = fluid_load_lb / spring_rate_lb_per_in
    stroke = surface_stroke_in - stretch + overtravel_in
    if stroke <= 0:
        raise ValueError(
            f"computed plunger stroke {stroke:.2f} in is not positive: rod "
            f"stretch ({stretch:.2f} in) exceeds the surface stroke "
            f"({surface_stroke_in:.2f} in). Check fluid load and rod string."
        )
    return stroke


def pump_displacement(
    plunger_diameter_in: float,
    plunger_stroke_in: float,
    strokes_per_minute: float,
) -> float:
    """Theoretical pump displacement, barrels of fluid per day.

    ``PD = 0.1484 * Ap * Sp * N``. Note the stroke argument is the **plunger**
    stroke; passing the surface stroke overstates displacement.
    """
    return (
        PUMP_DISPLACEMENT_CONSTANT
        * rod_area(plunger_diameter_in)
        * plunger_stroke_in
        * strokes_per_minute
    )


def volumetric_efficiency(
    measured_rate_bpd: float,
    pump_displacement_bpd: float,
    runtime_hours_per_day: Optional[float] = None,
    formation_volume_factor: Optional[float] = None,
) -> Optional[float]:
    """Volumetric efficiency as a fraction, or ``None`` when undetermined.

    Efficiency cannot be stated without knowing how long the unit actually ran
    and what the produced volume was at reservoir conditions. A unit cycling
    on a pump-off controller has a duty cycle that masquerades as low fillage,
    and surface barrels are not reservoir barrels. Rather than silently assume
    24 h and ``Bo = 1.0``, this returns ``None`` when either is unknown, so the
    caller must either supply them or report the gap.

    Args:
        measured_rate_bpd: Measured surface production over the test period.
        pump_displacement_bpd: Theoretical displacement at 100% fillage.
        runtime_hours_per_day: Hours the unit actually pumped.
        formation_volume_factor: ``Bo``, reservoir bbl per surface bbl.

    Returns:
        Efficiency as a fraction, or ``None`` if runtime or ``Bo`` is unknown.
    """
    if runtime_hours_per_day is None or formation_volume_factor is None:
        return None
    if pump_displacement_bpd <= 0 or runtime_hours_per_day <= 0:
        return None
    effective_displacement = pump_displacement_bpd * (runtime_hours_per_day / 24.0)
    return (measured_rate_bpd * formation_volume_factor) / effective_displacement


@dataclass
class RodStringAnalysis:
    """Elastic and load properties of a single-diameter rod string."""

    diameter_in: float
    length_ft: float
    weight_in_air_lb: float
    elastic_constant: float          # in/lb/ft
    spring_rate_lb_per_in: float     # Kr
    stroke_spring_product_lb: float  # Skr = S * Kr
    buoyant_weight_lb: float         # Wrf
    fluid_load_lb: float             # Fo
    fo_over_skr: float
    wrf_over_skr: float


def analyse_rod_string(
    diameter_in: float,
    length_ft: float,
    surface_stroke_in: float,
    plunger_diameter_in: float,
    fluid_level_ft: float,
    specific_gravity: float,
    tubing_pressure_psi: float = 0.0,
    casing_pressure_psi: float = 0.0,
    weight_lb_per_ft: Optional[float] = None,
    modulus_psi: float = YOUNGS_MODULUS_PSI,
) -> RodStringAnalysis:
    """Compute the elastic properties and RP 11L load groups for a string.

    Args:
        weight_lb_per_ft: Rod weight including couplings. Looked up from the
            API table when omitted; required for non-standard sizes.

    Raises:
        ValueError: If the weight is neither supplied nor a standard API size.
    """
    if weight_lb_per_ft is None:
        match = next(
            (
                props.weight_lb_per_ft
                for props in API_ROD_SIZES.values()
                if math.isclose(props.diameter_in, diameter_in, rel_tol=1e-6)
            ),
            None,
        )
        if match is None:
            raise ValueError(
                f"rod diameter {diameter_in} in is not a standard API size "
                f"({sorted(p.diameter_in for p in API_ROD_SIZES.values())}); "
                "supply weight_lb_per_ft explicitly"
            )
        weight_lb_per_ft = match

    elastic = rod_elastic_constant(diameter_in, modulus_psi)
    k_r = spring_rate(diameter_in, length_ft, modulus_psi)
    s_kr = surface_stroke_in * k_r
    weight_air = weight_lb_per_ft * length_ft
    w_rf = buoyant_rod_weight(weight_air, specific_gravity)
    f_o = fluid_load(
        plunger_diameter_in,
        fluid_level_ft,
        specific_gravity,
        tubing_pressure_psi,
        casing_pressure_psi,
    )
    return RodStringAnalysis(
        diameter_in=diameter_in,
        length_ft=length_ft,
        weight_in_air_lb=weight_air,
        elastic_constant=elastic,
        spring_rate_lb_per_in=k_r,
        stroke_spring_product_lb=s_kr,
        buoyant_weight_lb=w_rf,
        fluid_load_lb=f_o,
        fo_over_skr=f_o / s_kr,
        wrf_over_skr=w_rf / s_kr,
    )
