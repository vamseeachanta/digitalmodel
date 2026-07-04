# ABOUTME: Low-speed manoeuvring & station-keeping envelope for a single-screw ship.
# ABOUTME: Composes existing rudder/Nomoto/Soding physics; adds threshold-speed + current-balance.
"""Low-speed manoeuvring and station-keeping envelope.

This module is a thin *composition* layer over the existing digitalmodel
physics — it introduces no new rudder/propeller hydrodynamics. It reuses:

* :mod:`digitalmodel.naval_architecture.maneuverability`
  (Whicker & Fehlner rudder lift, Nomoto steady yaw rate, steady turning
  radius, directional-stability criterion, drift angle).
* :mod:`digitalmodel.hydrodynamics.propeller_rudder`
  (Soding/Brix engine-on rudder side force in the propeller slipstream).

and adds the operational layer the bare physics modules do not provide:

1. Clarke, Gedling & Hine (1983) linear hydrodynamic derivatives and the
   course-stability discriminant, from main dimensions only.
2. **Threshold (minimum steerage) speed** — the lowest speed at which the
   rudder can still overcome a beam wind/current disturbance, engine-off
   (coasting) or engine-on (kick-ahead feeding the rudder race).
3. **Engine-on rudder angle to hold heading** against a hull current yaw
   moment (OCIMF-style), and the **critical current** beyond which no
   rudder angle within the helm limit can balance it.
4. IMO MSC.137(76) turning-circle acceptance check.
5. A :class:`LoadingCondition` so every result can be reported laden vs
   ballast.

References:
    Clarke, Gedling & Hine (1983), Trans. RINA Vol. 125.
    Nomoto et al. (1957), Int. Shipbuilding Progress Vol. 4(35).
    Whicker & Fehlner (1958), DTMB Report 933.
    Soeding, H. (1982), Schiffstechnik Vol. 29; Brix (ed.) (1993).
    Lyster & Knights (1979), Trans. NECIES Vol. 95.
    OCIMF, Prediction of Wind and Current Loads on VLCCs (2nd ed., 1994).
    IMO Res. MSC.137(76) (2002), Standards for Ship Manoeuvrability.
"""
from __future__ import annotations

import math
from dataclasses import dataclass

from digitalmodel.hydrodynamics.propeller_rudder import (
    RudderForces,
    RudderGeometry,
    VesselPropulsion,
    soding_forces,
)
from digitalmodel.naval_architecture.maneuverability import (
    rudder_normal_force,
)

KNOT_TO_M_PER_S = 0.514444
M_PER_S_TO_KNOT = 1.0 / KNOT_TO_M_PER_S
RHO_SEAWATER = 1025.0  # kg/m^3
RHO_AIR = 1.225        # kg/m^3
MAX_RUDDER_ANGLE_DEG = 35.0

# IMO MSC.137(76) turning-ability acceptance limits (in ship lengths L = Lbp).
IMO_ADVANCE_LIMIT_L = 4.5
IMO_TACTICAL_DIAMETER_LIMIT_L = 5.0
IMO_INITIAL_TURNING_LIMIT_L = 2.5
IMO_STOPPING_TRACK_REACH_LIMIT_L = 15.0


# ---------------------------------------------------------------------------
# Loading condition
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class LoadingCondition:
    """A draft-dependent loading state for manoeuvring screening.

    Attributes:
        name: label, e.g. "laden" or "ballast".
        draft_m: mean draft [m].
        rudder_effective_area_m2: immersed/effective rudder area [m²]
            (less than the projected area when the blade is partly emerged).
        lateral_underwater_area_m2: hull lateral underwater area ~ Lbp*T [m²].
        windage_area_m2: lateral above-water (windage) area [m²].
        Cb: block coefficient [-].
        inflow_factor_engine_off: rudder inflow / ship-speed ratio coasting
            (≈ 1 - wake fraction).
        inflow_factor_engine_on: rudder inflow / ship-speed ratio with the
            propeller race over the rudder (kick-ahead), ~1.3-1.6.
    """
    name: str
    draft_m: float
    rudder_effective_area_m2: float
    lateral_underwater_area_m2: float
    windage_area_m2: float
    Cb: float
    inflow_factor_engine_off: float = 1.0
    inflow_factor_engine_on: float = 1.5


# ---------------------------------------------------------------------------
# 1. Clarke (1983) linear hydrodynamic derivatives + course stability
# ---------------------------------------------------------------------------

def clarke_derivatives(lbp_m: float, beam_m: float, draft_m: float, Cb: float) -> dict[str, float]:
    """Clarke, Gedling & Hine (1983) non-dimensional linear derivatives.

    Prime-I non-dimensionalisation with common factor k = pi*(T/L)^2.
    Returns velocity (damping), acceleration (added-mass) and mass terms.
    """
    if min(lbp_m, beam_m, draft_m) <= 0:
        raise ValueError("lbp_m, beam_m, draft_m must be > 0")
    L, B, T = lbp_m, beam_m, draft_m
    BT = B / T
    TL = T / L
    BL = B / L
    k = math.pi * TL**2
    return {
        "k": k,
        # velocity (damping) derivatives
        "Yv": -k * (1.0 + 0.40 * Cb * BT),
        "Yr": -k * (-0.5 + 2.2 * BL - 0.080 * BT),
        "Nv": -k * (0.5 + 2.4 * TL),
        "Nr": -k * (0.25 + 0.039 * BT - 0.56 * BL),
        # acceleration (added-mass) derivatives
        "Yvdot": -k * (1.0 + 0.16 * Cb * BT - 5.1 * BL**2),
        "Yrdot": -k * (0.67 * BL - 0.0033 * BT**2),
        "Nvdot": -k * (1.1 * BL - 0.041 * BT),
        "Nrdot": -k * (1.0 / 12.0 + 0.017 * Cb * BT - 0.33 * BL),
        # mass
        "m": 2.0 * Cb * B * T / L**2,
    }


def course_stability_discriminant(derivs: dict[str, float], xG_prime: float = 0.0) -> float:
    """Linear course-stability discriminant C (stable if C > 0).

    C = Yv*(Nr - m*xG) - Nv*(Yr - m).
    Full-form tankers typically sit near C ≈ 0 (marginally unstable).
    """
    m = derivs["m"]
    return derivs["Yv"] * (derivs["Nr"] - m * xG_prime) - derivs["Nv"] * (derivs["Yr"] - m)


# ---------------------------------------------------------------------------
# 2. Rudder lift slope (Whicker & Fehlner, via the existing module's formula)
# ---------------------------------------------------------------------------

def rudder_lift_slope_per_rad(area_m2: float, span_m: float, behind_hull: bool = True) -> float:
    """Lift-curve slope a = dC_N/dalpha = 6.13*AR_eff/(AR_eff + 2.25) [1/rad].

    AR_eff doubles for a rudder behind the hull (reflection-plane effect) —
    the same convention used in :func:`maneuverability.rudder_lift_coefficient`.
    """
    if area_m2 <= 0 or span_m <= 0:
        raise ValueError("area_m2 and span_m must be > 0")
    ar_geo = span_m**2 / area_m2
    ar_eff = 2.0 * ar_geo if behind_hull else ar_geo
    return 6.13 * ar_eff / (ar_eff + 2.25)


# ---------------------------------------------------------------------------
# 3. Turning circle (steady radius from K', IMO compliance)
# ---------------------------------------------------------------------------

def steady_turning_radius_over_L(K_prime: float, rudder_angle_deg: float) -> float:
    """Steady turning radius in ship lengths: R/L = 1/(K' * delta).

    K' is the non-dimensional Nomoto gain (≈ speed-independent in the linear
    regime), so R/L is set by geometry and rudder angle, NOT by ship speed.
    This is exactly why the turning circle (in lengths) is essentially the
    same from sea speed down to the steerage threshold.
    """
    delta = math.radians(rudder_angle_deg)
    if abs(K_prime * delta) < 1e-12:
        raise ValueError("K_prime * delta must be non-zero (infinite radius)")
    return abs(1.0 / (K_prime * delta))


def tactical_diameter_over_L(K_prime: float, rudder_angle_deg: float, transient_factor: float = 2.0) -> float:
    """Approximate tactical diameter in lengths.

    TD/L ≈ transient_factor * R/L. ``transient_factor`` ≈ 2.0 maps the steady
    radius to the transient 180°-heading transverse distance for a full-form
    single-screw ship (calibrated so a tanker with R/L≈1.6 gives TD/L≈3.2,
    consistent with Lyster & Knights 1979 sea-trial regressions). It is a
    screening factor, not a first-principles result.
    """
    return transient_factor * steady_turning_radius_over_L(K_prime, rudder_angle_deg)


@dataclass(frozen=True)
class IMOTurningVerdict:
    advance_ratio_L: float
    tactical_diameter_ratio_L: float
    advance_pass: bool
    tactical_diameter_pass: bool
    overall_pass: bool


def imo_turning_compliance(advance_m: float, tactical_diameter_m: float, lbp_m: float) -> IMOTurningVerdict:
    """Check turning ability against IMO MSC.137(76): advance ≤ 4.5L, TD ≤ 5L."""
    if lbp_m <= 0:
        raise ValueError("lbp_m must be > 0")
    a_ratio = advance_m / lbp_m
    td_ratio = tactical_diameter_m / lbp_m
    a_pass = a_ratio <= IMO_ADVANCE_LIMIT_L
    td_pass = td_ratio <= IMO_TACTICAL_DIAMETER_LIMIT_L
    return IMOTurningVerdict(a_ratio, td_ratio, a_pass, td_pass, a_pass and td_pass)


# ---------------------------------------------------------------------------
# 4. Threshold (minimum steerage) speed
# ---------------------------------------------------------------------------

def threshold_speed_for_steerage(
    *,
    wind_speed_m_s: float,
    windage_area_m2: float,
    rudder_effective_area_m2: float,
    rudder_span_m: float,
    rudder_angle_deg: float = MAX_RUDDER_ANGLE_DEG,
    wind_force_coeff: float = 0.8,
    inflow_factor: float = 1.0,
    behind_hull: bool = True,
) -> float:
    """Minimum steerage (threshold) speed [m/s] from a rudder-vs-wind balance.

    Equates the rudder side force (at the inflow speed U_R = inflow_factor * U)
    to a beam-wind side force and solves for U:

        0.5*rho_w*U_R^2*A_Re*a*delta = 0.5*rho_air*V_w^2*A_L*C_Y

        U_min = (V_w / inflow_factor) *
                sqrt( rho_air*A_L*C_Y / (rho_w*A_Re*a*sin(delta)) )

    ``inflow_factor`` is ~1 coasting (engine-off) and ~1.3-1.6 with the
    propeller race feeding the rudder (kick-ahead), which is why engine-on
    steerage holds to a much lower speed than coasting.
    """
    if min(windage_area_m2, rudder_effective_area_m2, rudder_span_m) <= 0:
        raise ValueError("areas and span must be > 0")
    if inflow_factor <= 0:
        raise ValueError("inflow_factor must be > 0")
    a = rudder_lift_slope_per_rad(rudder_effective_area_m2, rudder_span_m, behind_hull)
    delta = math.radians(abs(rudder_angle_deg))
    rudder_term = RHO_SEAWATER * rudder_effective_area_m2 * a * math.sin(delta)
    if rudder_term <= 0:
        raise ValueError("rudder_angle_deg must be non-zero")
    ratio = (RHO_AIR * windage_area_m2 * wind_force_coeff) / rudder_term
    return (wind_speed_m_s / inflow_factor) * math.sqrt(ratio)


# ---------------------------------------------------------------------------
# 5. Current yaw moment (OCIMF form) + engine-on rudder balance
# ---------------------------------------------------------------------------

def current_yaw_moment_ocimf(
    *, CXYc: float, current_speed_m_s: float, lbp_m: float, draft_m: float
) -> float:
    """Hull current yaw moment [N·m] (OCIMF): N = CXYc * 0.5*rho*V_c^2 * Lbp^2 * T."""
    q = 0.5 * RHO_SEAWATER * current_speed_m_s**2
    return CXYc * q * lbp_m**2 * draft_m


def current_lateral_force_ocimf(
    *, CYc: float, current_speed_m_s: float, lbp_m: float, draft_m: float
) -> float:
    """Hull current lateral force [N] (OCIMF): Fy = CYc * 0.5*rho*V_c^2 * Lbp * T."""
    q = 0.5 * RHO_SEAWATER * current_speed_m_s**2
    return CYc * q * lbp_m * draft_m


@dataclass(frozen=True)
class HeadingHoldResult:
    """Engine-on rudder balance against a current yaw moment."""
    current_yaw_moment_Nm: float
    rudder_yaw_authority_Nm: float   # max rudder yaw moment at the helm limit
    required_rudder_angle_deg: float | None  # None if beyond authority
    can_hold_heading: bool
    utilisation: float               # |N_current| / authority (>1 ⇒ cannot hold)


def rudder_angle_to_hold_heading(
    *,
    current_yaw_moment_Nm: float,
    ship_speed_m_s: float,
    shaft_speed_rev_s: float,
    vessel: VesselPropulsion,
    rudder: RudderGeometry,
    max_rudder_angle_deg: float = MAX_RUDDER_ANGLE_DEG,
) -> HeadingHoldResult:
    """Rudder angle (engine on) needed to cancel a hull current yaw moment.

    Uses the existing Soding/Brix :func:`soding_forces` engine-on physics.
    Because the Soding rudder yaw moment is proportional to sin(delta), the
    yaw moment at the helm limit fixes the authority, and the required angle
    inverts the sine:

        delta_req = asin( |N_current| / N_authority * sin(delta_max) )

    If |N_current| exceeds the authority at ``max_rudder_angle_deg`` the
    heading cannot be held by rudder alone (tug/thruster assist required).
    """
    forces: RudderForces = soding_forces(
        ship_speed_m_s, shaft_speed_rev_s, max_rudder_angle_deg, vessel, rudder
    )
    authority = abs(forces.F_yaw)  # rudder yaw moment magnitude at the helm limit
    need = abs(current_yaw_moment_Nm)
    if authority <= 0:
        return HeadingHoldResult(current_yaw_moment_Nm, 0.0, None, False, math.inf)
    utilisation = need / authority
    if utilisation > 1.0:
        return HeadingHoldResult(current_yaw_moment_Nm, authority, None, False, utilisation)
    sin_max = math.sin(math.radians(max_rudder_angle_deg))
    required = math.degrees(math.asin(min(1.0, utilisation * sin_max)))
    return HeadingHoldResult(current_yaw_moment_Nm, authority, required, True, utilisation)


def critical_current_speed_for_heading(
    *,
    CXYc: float,
    lbp_m: float,
    draft_m: float,
    ship_speed_m_s: float,
    shaft_speed_rev_s: float,
    vessel: VesselPropulsion,
    rudder: RudderGeometry,
    max_rudder_angle_deg: float = MAX_RUDDER_ANGLE_DEG,
) -> float:
    """Largest current speed [m/s] whose yaw moment the rudder can still hold.

    Since N_current ∝ V_c^2 and the rudder authority is fixed by thrust,
    V_c,crit = sqrt( N_authority / (|CXYc| * 0.5*rho * Lbp^2 * T) ).
    """
    forces = soding_forces(ship_speed_m_s, shaft_speed_rev_s, max_rudder_angle_deg, vessel, rudder)
    authority = abs(forces.F_yaw)
    denom = abs(CXYc) * 0.5 * RHO_SEAWATER * lbp_m**2 * draft_m
    if denom <= 0:
        return math.inf
    return math.sqrt(authority / denom)


# ---------------------------------------------------------------------------
# Convenience: engine-off rudder yaw moment (Whicker-Fehlner, for contrast)
# ---------------------------------------------------------------------------

def engine_off_rudder_yaw_moment(
    *,
    current_speed_m_s: float,
    rudder_area_m2: float,
    rudder_span_m: float,
    rudder_angle_deg: float,
    lever_arm_m: float,
    behind_hull: bool = True,
) -> float:
    """Engine-off rudder yaw moment [N·m] from Whicker-Fehlner normal force.

    F_N from :func:`maneuverability.rudder_normal_force`, times the lever arm.
    Engine-off the rudder only sees the current as inflow, so this is small
    and current-speed-limited — contrast with the thrust-scaled engine-on
    authority from :func:`rudder_angle_to_hold_heading`.
    """
    f_n = rudder_normal_force(
        current_speed_m_s, RHO_SEAWATER, rudder_area_m2, rudder_span_m,
        rudder_angle_deg, behind_hull,
    )
    return f_n * lever_arm_m
