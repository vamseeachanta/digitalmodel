"""DNV-OS-F201 limit-state code checks — LRFD vs WSD (combined loading).

Implements the core DNV-OS-F201 (Dynamic Risers) combined-loading limit-state
check for a tubular member subjected to bending moment, effective tension and
net (internal or external) overpressure, in both the partial-safety-factor
(LRFD) and the working-stress-design (WSD) formats.

Two design formats share the same characteristic resistances:
    - Plastic bending-moment resistance  Mk = fy * alpha_c * (D - t2)^2 * t2
    - Plastic axial-force resistance      Tk = fy * alpha_c * pi * (D - t2) * t2
    - Bursting (containment) resistance   Pb = (2/sqrt(3)) * (2 t2 / (D - t2)) * min(fy, fu/1.15)

with the flow-stress / strain-hardening coefficient:
    alpha_c = (1 - beta) + beta * (fu / fy)
    qh      = ((pi - pe) / Pb) * (2/sqrt(3))   for pi > pe, else 0
    beta    = 0.4 + qh                         for D/t2 < 15
            = (0.4 + qh) * (60 - D/t2) / 45    for 15 <= D/t2 <= 60
            = 0                                for D/t2 > 60

Combined-loading criteria (utilisation, pass when <= limit):

INTERNAL net overpressure (pi > pe):
    LRFD:  gamma_SC * gamma_m * [ (|Md|/Mk) * sqrt(1 - ((pi-pe)/Pb)^2)
                                  + (Td/Tk)^2 ] + ((pi-pe)/Pb)^2  <= 1
    WSD:   same expression without gamma_SC*gamma_m, limit = eta^2

EXTERNAL net overpressure (pe > pi):
    LRFD:  (gamma_SC*gamma_m)^2 * [ ((|Md|/Mk) + (Td/Tk)^2)^2
                                    + ((pe - pmin)/Pc)^2 ]  <= 1
    WSD:   same expression without (gamma_SC*gamma_m)^2, limit = eta^2

For LRFD the load effects (Md, Td) are factored design values
(gamma_F * functional + gamma_E * environmental + gamma_A * accidental);
for WSD they are the unfactored sums. gamma_SC and gamma_m are the safety-class
and material resistance factors; eta is the WSD usage factor.

All functions are unit-consistent: pass resistances and load effects in the
same unit system (e.g. ksi / kips-ft / kips, or MPa / kN.m / kN). The module
performs no internal unit conversion.

References:
    - DNV-OS-F201 (2010), Section 5 (limit-state combined-loading criteria,
      Eqs 5.25-5.26 for Mk/Tk; combined-loading clause D).
    - DNV-OS-F101 for the bursting/collapse resistance and tolerance thicknesses.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from enum import Enum

SQRT3 = math.sqrt(3.0)

# Partial safety factors per limit-state category (DNV-OS-F201 Sec.5).
# (gamma_F functional, gamma_E environmental, gamma_A accidental)
LOAD_FACTORS: dict[str, tuple[float, float, float]] = {
    "ULS": (1.1, 1.3, 0.0),
    "FLS": (1.0, 1.0, 0.0),
    "SLS": (1.0, 1.0, 1.0),
    "ALS": (1.0, 1.0, 1.0),
}

# Safety-class resistance factor gamma_SC.
SAFETY_CLASS_FACTOR: dict[str, float] = {"low": 1.04, "normal": 1.14, "high": 1.26}

# WSD usage factor eta by safety class.
WSD_USAGE_FACTOR: dict[str, float] = {"low": 0.83, "normal": 0.79, "high": 0.75}


class OverpressureMode(str, Enum):
    """Net-overpressure regime governing the combined-loading criterion."""

    INTERNAL = "internal"  # pi > pe (burst-dominated)
    EXTERNAL = "external"  # pe > pi (collapse-dominated)


@dataclass(frozen=True)
class CombinedLoadingResult:
    """Result of a DNV-OS-F201 combined-loading limit-state check."""

    utilisation: float  # left-hand side of the criterion
    limit: float  # right-hand side (1.0 for LRFD, eta^2 for WSD)
    passes: bool  # utilisation <= limit


def yield_strength(smys: float, f_y_temp_derating: float, alpha_u: float) -> float:
    """Characteristic yield strength fy = (SMYS - f_y,temp) * alpha_u.

    Parameters
    ----------
    smys : float
        Specified minimum yield strength [stress].
    f_y_temp_derating : float
        Temperature de-rating on yield stress [stress] (0 below the de-rating
        onset temperature).
    alpha_u : float
        Material strength factor [-] (e.g. 0.96 normal, 1.0 with supplementary
        requirement U).
    """
    return (smys - f_y_temp_derating) * alpha_u


def tensile_strength(smts: float, f_u_temp_derating: float, alpha_u: float) -> float:
    """Characteristic tensile strength fu = (SMTS - f_u,temp) * alpha_u."""
    return (smts - f_u_temp_derating) * alpha_u


def plastic_moment_resistance(
    diameter: float, t2: float, f_y: float, alpha_c: float
) -> float:
    """Plastic bending-moment resistance Mk = fy * alpha_c * (D - t2)^2 * t2.

    Returns a moment in [stress * length^3]. With diameter/thickness in inches
    and fy in ksi this gives inch-kips.
    """
    return f_y * alpha_c * (diameter - t2) ** 2 * t2


def plastic_axial_resistance(
    diameter: float, t2: float, f_y: float, alpha_c: float
) -> float:
    """Plastic axial-force resistance Tk = fy * alpha_c * pi * (D - t2) * t2."""
    return f_y * alpha_c * math.pi * (diameter - t2) * t2


def bursting_resistance(diameter: float, t2: float, f_y: float, f_u: float) -> float:
    """Bursting (containment) resistance Pb.

    Pb = (2/sqrt(3)) * (2 t2 / (D - t2)) * min(fy, fu/1.15).
    """
    return (2.0 / SQRT3) * (2.0 * t2 / (diameter - t2) * min(f_y, f_u / 1.15))


def flow_stress_coefficient(
    diameter: float,
    t2: float,
    f_y: float,
    f_u: float,
    p_internal: float,
    p_external: float,
    bursting_resistance_value: float,
) -> float:
    """Flow-stress / strain-hardening coefficient alpha_c.

    alpha_c = (1 - beta) + beta * (fu / fy), with beta a function of D/t2 and
    the pressure-utilisation term qh.

    Parameters
    ----------
    diameter, t2 : float
        Outer diameter and characteristic (resistance) wall thickness.
    f_y, f_u : float
        Characteristic yield and tensile strengths.
    p_internal, p_external : float
        Internal and external pressures (same unit as bursting resistance).
    bursting_resistance_value : float
        Pre-computed bursting resistance Pb.
    """
    d_over_t = diameter / t2
    if p_internal > p_external:
        qh = ((p_internal - p_external) / bursting_resistance_value) * (2.0 / SQRT3)
    else:
        qh = 0.0

    if d_over_t < 15.0:
        beta = 0.4 + qh
    elif d_over_t <= 60.0:
        beta = (0.4 + qh) * (60.0 - d_over_t) / 45.0
    else:
        beta = 0.0
    return (1.0 - beta) + beta * (f_u / f_y)


def combined_loading_internal_overpressure(
    design_moment: float,
    moment_resistance: float,
    design_tension: float,
    axial_resistance: float,
    p_internal: float,
    p_external: float,
    bursting_resistance_value: float,
    gamma_sc: float = 1.0,
    gamma_m: float = 1.0,
    usage_factor: float | None = None,
) -> CombinedLoadingResult:
    """Combined-loading check for internal net overpressure (pi > pe).

    LRFD (gamma_SC, gamma_m > 1, usage_factor=None): limit = 1.0.
    WSD (gamma_SC = gamma_m = 1, usage_factor=eta): limit = eta^2.

    util = gamma_SC*gamma_m * [ (|Md|/Mk) * sqrt(1 - ((pi-pe)/Pb)^2)
                                + (Td/Tk)^2 ] + ((pi-pe)/Pb)^2
    """
    pressure_ratio = (p_internal - p_external) / bursting_resistance_value
    util = gamma_sc * gamma_m * (
        (abs(design_moment) / moment_resistance) * math.sqrt(1.0 - pressure_ratio**2)
        + (design_tension / axial_resistance) ** 2
    ) + pressure_ratio**2
    limit = 1.0 if usage_factor is None else usage_factor**2
    return CombinedLoadingResult(util, limit, util <= limit)


def combined_loading_external_overpressure(
    design_moment: float,
    moment_resistance: float,
    design_tension: float,
    axial_resistance: float,
    p_external: float,
    p_min_internal: float,
    collapse_resistance: float,
    gamma_sc: float = 1.0,
    gamma_m: float = 1.0,
    usage_factor: float | None = None,
) -> CombinedLoadingResult:
    """Combined-loading check for external net overpressure (pe > pi).

    LRFD (gamma_SC, gamma_m > 1, usage_factor=None): limit = 1.0.
    WSD (gamma_SC = gamma_m = 1, usage_factor=eta): limit = eta^2.

    util = (gamma_SC*gamma_m)^2 * [ ((|Md|/Mk) + (Td/Tk)^2)^2
                                    + ((pe - pmin)/Pc)^2 ]
    """
    pressure_ratio = (p_external - p_min_internal) / collapse_resistance
    util = (gamma_sc * gamma_m) ** 2 * (
        ((abs(design_moment) / moment_resistance) + (design_tension / axial_resistance) ** 2) ** 2
        + pressure_ratio**2
    )
    limit = 1.0 if usage_factor is None else usage_factor**2
    return CombinedLoadingResult(util, limit, util <= limit)


def factored_load_effect(
    functional: float,
    environmental: float,
    accidental: float,
    gamma_f: float,
    gamma_e: float,
    gamma_a: float,
) -> float:
    """LRFD factored design load effect.

    design = gamma_F * functional + gamma_E * environmental + gamma_A * accidental.
    For WSD use gamma_F = gamma_E = gamma_A = 1 (i.e. the unfactored sum).
    """
    return gamma_f * functional + gamma_e * environmental + gamma_a * accidental
