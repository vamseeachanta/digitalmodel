# ABOUTME: Ship maneuverability — rudder forces, turning circle, stability
# ABOUTME: Based on USNA EN400 Chapter 9 and Nomoto 1st-order model
"""Maneuverability calculations for surface ships.

Covers rudder normal force (Whicker & Fehlner lift model), Nomoto
1st-order turning model, directional stability criterion, speed loss
in turn, and drift angle approximation.

Refs: USNA EN400 Ch.9, Nomoto (1957), Whicker & Fehlner (1958)
WRK-1375 | Parent: WRK-1339
"""

import math

RHO_SEAWATER = 1025.0  # kg/m^3
MAX_RUDDER_ANGLE_DEG = 35.0  # degrees


def rudder_lift_coefficient(
    rudder_area_m2: float,
    rudder_span_m: float,
    rudder_angle_deg: float,
    behind_hull: bool = True,
) -> float:
    """Lift coefficient C_N for a rectangular rudder.

    C_N = (6.13 * AR_eff) / (AR_eff + 2.25) * sin(delta)
    Effective AR doubles for rudder behind hull (mirror effect).
    """
    if rudder_area_m2 <= 0:
        raise ValueError(f"rudder_area_m2 must be > 0, got {rudder_area_m2}")
    if rudder_span_m <= 0:
        raise ValueError(f"rudder_span_m must be > 0, got {rudder_span_m}")

    ar_geo = rudder_span_m**2 / rudder_area_m2
    ar_eff = 2.0 * ar_geo if behind_hull else ar_geo
    delta_rad = math.radians(rudder_angle_deg)
    return (6.13 * ar_eff) / (ar_eff + 2.25) * math.sin(delta_rad)


def rudder_normal_force(
    velocity_m_s: float,
    rho_kg_m3: float,
    rudder_area_m2: float,
    rudder_span_m: float,
    rudder_angle_deg: float,
    behind_hull: bool = True,
) -> float:
    """Normal force on rudder (N): F_N = 0.5 * rho * V^2 * A_R * C_N."""
    if velocity_m_s < 0:
        raise ValueError(f"velocity_m_s must be >= 0, got {velocity_m_s}")

    cn = rudder_lift_coefficient(
        rudder_area_m2, rudder_span_m, rudder_angle_deg, behind_hull
    )
    return 0.5 * rho_kg_m3 * velocity_m_s**2 * rudder_area_m2 * cn


def nomoto_steady_yaw_rate(
    K_per_s: float,
    rudder_angle_deg: float,
) -> float:
    """Steady-state yaw rate from Nomoto 1st-order model.

    From T * dr/dt + r = K * delta, at steady state: r_ss = K * delta.
    """
    if K_per_s < 0:
        raise ValueError(f"K_per_s must be >= 0, got {K_per_s}")
    return K_per_s * math.radians(rudder_angle_deg)


def steady_turning_radius(
    velocity_m_s: float,
    yaw_rate_rad_s: float,
) -> float:
    """Steady turning radius R = V / r (m)."""
    if yaw_rate_rad_s == 0.0:
        raise ValueError("yaw_rate_rad_s must be != 0 (infinite radius)")
    return abs(velocity_m_s / yaw_rate_rad_s)


def directional_stability_criterion(
    Yv_prime: float,
    Nr_prime: float,
    Nv_prime: float,
    Yr_prime: float,
) -> float:
    """Linear directional stability criterion.

    C = Yv' * Nr' - Nv' * Yr'. Ship is stable when C > 0.
    """
    return Yv_prime * Nr_prime - Nv_prime * Yr_prime


def is_directionally_stable(
    Yv_prime: float,
    Nr_prime: float,
    Nv_prime: float,
    Yr_prime: float,
) -> bool:
    """Check if ship is directionally stable (C > 0)."""
    return directional_stability_criterion(
        Yv_prime, Nr_prime, Nv_prime, Yr_prime
    ) > 0


def speed_in_turn(
    velocity_m_s: float,
    rudder_angle_deg: float,
    loss_factor: float = 0.4,
) -> float:
    """Speed during steady turn: V_turn = V_0 * (1 - f * (delta/35)^2)."""
    if not 0.0 <= loss_factor <= 1.0:
        raise ValueError(f"loss_factor must be in [0, 1], got {loss_factor}")
    ratio = rudder_angle_deg / MAX_RUDDER_ANGLE_DEG
    return velocity_m_s * (1.0 - loss_factor * ratio**2)


def drift_angle(
    lwl_m: float,
    turning_radius_m: float,
) -> float:
    """Approximate drift angle in steady turn: beta = atan(L/(2R)) deg."""
    if turning_radius_m <= 0:
        raise ValueError(
            f"turning_radius_m must be > 0, got {turning_radius_m}"
        )
    return math.degrees(math.atan(lwl_m / (2.0 * turning_radius_m)))
