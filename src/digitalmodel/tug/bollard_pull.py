# ABOUTME: Tug bollard pull (ahead/astern) and escort steering/braking force prediction
# ABOUTME: Quasi-static estimates from installed power, propulsion type, and escort hull lift
"""Bollard pull and escort-performance prediction (issue #1194).

Bollard pull is the headline capability number for a tug. Class intact-stability
checks for conventional tugs assume the transverse pull is a fraction of the
maximum continuous bollard pull, so the BP estimate sizes both capability and
the heeling load used in girting analysis.

Escort tugs working in *indirect* mode generate steering and braking forces from
hydrodynamic lift on the hull/skeg that can exceed their direct bollard pull;
this module gives a quasi-static first estimate of that envelope.

References:
    - SNAME / ITS tug performance practice
    - DNV / ABS / LR tug and escort class notations
    - Smoker, escort-tug steering/braking force prediction (CFD), UVic
"""

import math
from dataclasses import dataclass

from digitalmodel.tug.constants import (
    ASTERN_FRACTION,
    BP_FACTOR_T_PER_KW,
    SEAWATER_DENSITY_T_M3,
    TRANSVERSE_PULL_FRACTION,
    Propulsion,
)


@dataclass
class BollardPullResult:
    """Bollard pull estimate for a tug."""

    propulsion: str
    installed_power_kw: float
    bp_ahead_t: float  # ahead bollard pull, tonnes-force
    bp_astern_t: float  # astern bollard pull, tonnes-force
    transverse_pull_t: float  # class heeling pull for stability checks


@dataclass
class EscortForceResult:
    """Escort steering/braking force in indirect mode."""

    speed_kn: float
    steering_force_t: float  # lateral (steering) force, tonnes-force
    braking_force_t: float  # longitudinal (braking) force, tonnes-force
    exceeds_bollard_pull: bool  # indirect force > direct bollard pull


def bollard_pull(
    installed_power_kw: float,
    propulsion: str,
    factor_t_per_kw: float | None = None,
) -> BollardPullResult:
    """Estimate ahead/astern bollard pull from installed power and propulsion.

    BP_ahead = factor * installed_power
    BP_astern = astern_fraction * BP_ahead

    Args:
        installed_power_kw: total installed propulsion power, kW.
        propulsion: one of the Propulsion enum values (e.g. 'asd').
        factor_t_per_kw: override the typical BP-per-power factor (t/kW).

    Returns:
        BollardPullResult with ahead/astern pull and the class transverse pull.
    """
    if installed_power_kw <= 0:
        raise ValueError("installed_power_kw must be positive")
    prop = Propulsion(propulsion.lower())

    factor = (
        factor_t_per_kw if factor_t_per_kw is not None else BP_FACTOR_T_PER_KW[prop]
    )
    bp_ahead = factor * installed_power_kw
    bp_astern = ASTERN_FRACTION[prop] * bp_ahead
    # Heeling pull for girting depends on how much thrust the tug can vector
    # sideways: ~0.6*BP for conventional screws, ~full BP for azimuth/cycloidal.
    transverse = TRANSVERSE_PULL_FRACTION[prop] * bp_ahead

    return BollardPullResult(
        propulsion=prop.value,
        installed_power_kw=installed_power_kw,
        bp_ahead_t=bp_ahead,
        bp_astern_t=bp_astern,
        transverse_pull_t=transverse,
    )


def escort_force(
    speed_kn: float,
    lateral_area_m2: float,
    bp_ahead_t: float,
    lift_coefficient: float = 1.6,
    drag_coefficient: float = 0.9,
) -> EscortForceResult:
    """Quasi-static escort steering/braking force in indirect mode.

    In indirect mode the tug presents its hull/skeg at a yaw angle to the towed
    vessel's wake; the hull acts as a hydrofoil. The lateral (steering) and
    longitudinal (braking) forces scale with dynamic pressure on the lateral
    underwater area:

        F = 0.5 * rho * V^2 * A * C

    with C the lift coefficient (steering) or drag coefficient (braking).

    Args:
        speed_kn: assisted-vessel through-water speed, knots.
        lateral_area_m2: tug lateral underwater (profile) area, m^2.
        bp_ahead_t: ahead bollard pull, tonnes-force, for the comparison flag.
        lift_coefficient: hull/skeg lift coefficient in indirect mode.
        drag_coefficient: hull drag coefficient.

    Returns:
        EscortForceResult with steering/braking force and whether the indirect
        steering force exceeds the tug's direct bollard pull.
    """
    if speed_kn < 0 or lateral_area_m2 <= 0:
        raise ValueError("speed_kn must be >= 0 and lateral_area_m2 > 0")

    v_ms = speed_kn * 0.514444
    rho = SEAWATER_DENSITY_T_M3 * 1000.0  # kg/m^3
    q = 0.5 * rho * v_ms**2  # dynamic pressure, Pa

    steering_n = q * lateral_area_m2 * lift_coefficient
    braking_n = q * lateral_area_m2 * drag_coefficient

    # N -> tonnes-force
    g = 9.80665
    steering_t = steering_n / (1000.0 * g)
    braking_t = braking_n / (1000.0 * g)

    return EscortForceResult(
        speed_kn=speed_kn,
        steering_force_t=steering_t,
        braking_force_t=braking_t,
        exceeds_bollard_pull=steering_t > bp_ahead_t,
    )
