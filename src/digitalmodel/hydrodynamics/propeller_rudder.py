"""Propeller-rudder interaction models.

WRK-1149: Implements Söding/Brix (primary) and Actuator-Disk + flat-plate
(fallback) methods for rudder forces in propeller slipstream.
``driveoff_sweep`` exposes their operating regimes, loading and axial inflow.

References:
    - McTaggart (2005), DRDC Atlantic TM 2005-071, Sections 7-9
    - Carlton (2007), Marine Propellers and Propulsion, Ch. 5
    - Molland & Turnock (2007), Marine Rudders and Control Surfaces
"""
from __future__ import annotations

import math
from collections.abc import Iterable
from dataclasses import dataclass


@dataclass(frozen=True)
class VesselPropulsion:
    """Propulsion system parameters."""
    D: float               # propeller diameter [m]
    kt_coeffs: tuple       # K_T polynomial coeffs (a0, a1, a2, ...) in powers of J
    t: float               # thrust deduction fraction [-]
    w: float               # wake fraction [-]
    C_rp: float            # rudder-propeller interaction coefficient [-]
    rho: float = 1025.0    # water density [kg/m³]


@dataclass(frozen=True)
class RudderGeometry:
    """Rudder geometric parameters."""
    area: float            # rudder projected area [m²]
    aspect_ratio: float    # span² / area [-]
    x_R: float             # unsigned CG-to-rudder distance [m, positive]
    gamma_deg: float = 0.0 # dihedral angle [deg], 0 for vertical rudder


@dataclass(frozen=True)
class RudderForces:
    """Rudder interaction forces in ship-fixed axes."""
    F_surge: float   # [N] positive forward
    F_sway: float    # [N] positive to port
    F_yaw: float     # [N·m] positive bow to port
    method: str      # "soding" or "actuator_disk_flat_plate"


def kt_from_poly(J: float, coeffs: tuple) -> float:
    """Evaluate K_T polynomial: K_T = sum(coeffs[i] * J^i)."""
    return sum(c * J**i for i, c in enumerate(coeffs))


# ---------------------------------------------------------------------------
# Primary method: Söding/Brix
# ---------------------------------------------------------------------------

def soding_forces(
    V_s: float,
    n: float,
    delta_deg: float,
    vessel: VesselPropulsion,
    rudder: RudderGeometry,
) -> RudderForces:
    """Söding/Brix propeller-rudder interaction forces.

    Args:
        V_s: ship speed [m/s]
        n: propeller speed [rev/s]
        delta_deg: rudder deflection angle [deg]
        vessel: propulsion parameters
        rudder: rudder geometry

    Returns:
        RudderForces in ship-fixed axes.
    """
    delta = math.radians(delta_deg)
    gamma = math.radians(rudder.gamma_deg)
    V_A = V_s * (1.0 - vessel.w)

    # Guard: engine-off (n ≈ 0) → zero propeller-augmented force
    if abs(n) < 1e-12:
        return RudderForces(0.0, 0.0, 0.0, method="soding")

    J = V_A / (n * vessel.D)
    K_T = kt_from_poly(J, vessel.kt_coeffs)
    T = vessel.rho * n**2 * vessel.D**4 * K_T
    F_prop = (1.0 - vessel.t) * T

    # Thrust loading coefficient
    A_disc = math.pi / 4.0 * vessel.D**2
    V_A_sq = V_A**2 if abs(V_A) > 1e-12 else 1e-12
    C_th = F_prop / (0.5 * vessel.rho * V_A_sq * A_disc)

    # Guard: braking quadrant (C_th < -1) → sqrt undefined
    if 1.0 + C_th < 0.0:
        return RudderForces(0.0, 0.0, 0.0, method="soding")

    interaction = 1.0 + 1.0 / math.sqrt(1.0 + C_th)

    F_lift = vessel.C_rp * F_prop * interaction * math.sin(delta)
    F_drag = -vessel.C_rp * F_prop * interaction * (1.0 - math.cos(delta))

    F_surge = F_drag
    F_sway = -F_lift * math.cos(gamma)
    F_yaw = -F_lift * math.cos(gamma) * rudder.x_R

    return RudderForces(F_surge, F_sway, F_yaw, method="soding")


# ---------------------------------------------------------------------------
# Fallback method: Actuator-Disk slipstream + flat-plate rudder
# ---------------------------------------------------------------------------

def actuator_disk_velocity(
    V_A: float,
    C_T: float,
    x: float,
    R: float,
) -> float:
    """Axial velocity at rudder plane from actuator-disk theory.

    Args:
        V_A: advance velocity [m/s]
        C_T: thrust loading coefficient [-]
        x: axial distance from disc to rudder [m]
        R: propeller radius [m]

    Returns:
        Va_R: axial slipstream velocity at rudder [m/s]
    """
    if C_T < 0.0:
        # No acceleration in braking — return freestream
        return V_A
    a = (math.sqrt(1.0 + C_T) - 1.0) / 2.0
    contraction = 1.0 + x / math.sqrt(x**2 + R**2)
    return V_A * (1.0 + a * contraction)


def flat_plate_rudder_cl(alpha_deg: float, aspect_ratio: float) -> float:
    """Lift coefficient for a flat-plate / thin-airfoil rudder.

    Uses lifting-line correction: dCL/dalpha = 2*pi*AR / (AR + 2).
    Clamps at stall angle (±35°).
    """
    stall = 35.0
    alpha_clamped = max(-stall, min(stall, alpha_deg))
    alpha_rad = math.radians(alpha_clamped)
    dCL_dalpha = 2.0 * math.pi * aspect_ratio / (aspect_ratio + 2.0)
    return dCL_dalpha * alpha_rad


def ad_flat_plate_forces(
    V_s: float,
    n: float,
    delta_deg: float,
    vessel: VesselPropulsion,
    rudder: RudderGeometry,
) -> RudderForces:
    """Actuator-disk + flat-plate rudder forces.

    Computes slipstream velocity via actuator-disk theory, then applies
    flat-plate lift/drag to the rudder in that inflow.

    Args:
        V_s: ship speed [m/s]
        n: propeller speed [rev/s]
        delta_deg: rudder deflection angle [deg]
        vessel: propulsion parameters
        rudder: rudder geometry

    Returns:
        RudderForces in ship-fixed axes.
    """
    gamma = math.radians(rudder.gamma_deg)
    V_A = V_s * (1.0 - vessel.w)
    R = vessel.D / 2.0
    # Rudder located ~0.5D behind propeller disc (typical)
    x_prop_rudder = 0.5 * vessel.D

    if abs(n) < 1e-12 or abs(V_A) < 1e-12:
        # Engine-off: rudder sees ship-speed inflow only
        V_R = max(abs(V_s * (1.0 - vessel.w)), 1e-12)
    else:
        J = V_A / (n * vessel.D)
        K_T = kt_from_poly(J, vessel.kt_coeffs)
        T = vessel.rho * n**2 * vessel.D**4 * K_T

        A_disc = math.pi / 4.0 * vessel.D**2
        V_A_sq = V_A**2 if abs(V_A) > 1e-12 else 1e-12
        C_T = T / (0.5 * vessel.rho * V_A_sq * A_disc)

        V_R = actuator_disk_velocity(V_A, C_T, x_prop_rudder, R)

    C_L = flat_plate_rudder_cl(delta_deg, rudder.aspect_ratio)
    # Small-angle drag: C_D ≈ C_L * sin(alpha) (induced drag)
    alpha_rad = math.radians(min(abs(delta_deg), 35.0))
    C_D = abs(C_L) * math.sin(alpha_rad)

    q = 0.5 * vessel.rho * V_R**2
    F_lift = q * rudder.area * C_L
    F_drag = q * rudder.area * C_D

    F_surge = -F_drag  # drag opposes forward motion
    F_sway = -F_lift * math.cos(gamma)
    F_yaw = -F_lift * math.cos(gamma) * rudder.x_R

    return RudderForces(F_surge, F_sway, F_yaw, method="actuator_disk_flat_plate")


@dataclass(frozen=True)
class DriveOffPoint:
    """Drive-off diagnostics in SI units; coefficients and J are dimensionless.

    Primary forces are the interaction increment; fallback forces are the total
    rudder force. None denotes an undefined coefficient or unevaluated fallback.
    Bollard coefficients are reported as inf, never passed to the force models.
    """
    regime: str
    V_s: float  # ship speed [m/s]
    V_A: float  # wake-reduced advance velocity [m/s]
    n: float  # shaft speed [rev/s]
    J: float | None
    K_T: float | None
    T: float  # thrust [N]
    C_th_primary: float | None
    C_T_fallback: float | None
    primary_clamped: bool
    slipstream_axial: float  # rudder-plane axial velocity [m/s]
    primary: RudderForces
    fallback: RudderForces | None


def _bollard_slipstream_velocity(T: float, rho: float, D: float, x: float) -> float:
    """Finite V_A -> 0 limit of actuator_disk_velocity at fixed thrust [m/s]."""
    area = math.pi * D**2 / 4.0
    contraction = 1.0 + x / math.sqrt(x**2 + (D / 2.0)**2)
    return contraction / 2.0 * math.sqrt(2.0 * max(T, 0.0) / (rho * area))


def _driveoff_point(
    V_s: float, n: float, delta_deg: float,
    vessel: VesselPropulsion, rudder: RudderGeometry,
) -> DriveOffPoint:
    """Compose unchanged force models and their diagnostics for one shaft speed."""
    V_A = V_s * (1.0 - vessel.w)
    J = K_T = C_th_primary = C_T_fallback = None
    T, slipstream, regime = 0.0, V_A, "engine_off"
    if n > 0.0:
        J = V_A / (n * vessel.D)
        K_T = kt_from_poly(J, vessel.kt_coeffs)
        T = vessel.rho * n**2 * vessel.D**4 * K_T
        if abs(V_A) < 1e-12:
            regime = "bollard"
            C_th_primary = C_T_fallback = math.inf
            slipstream = _bollard_slipstream_velocity(
                T, vessel.rho, vessel.D, 0.5 * vessel.D,
            )
        else:
            pressure_area = 0.5 * vessel.rho * V_A**2 * math.pi * vessel.D**2 / 4.0
            C_th_primary = (1.0 - vessel.t) * T / pressure_area
            C_T_fallback = T / pressure_area
            regime = "braking" if K_T <= 0.0 else "ahead"
            slipstream = actuator_disk_velocity(
                V_A, C_T_fallback, 0.5 * vessel.D, vessel.D / 2.0,
            )
    clamped = regime in ("ahead", "braking") and 1.0 + C_th_primary < 0.0
    primary = soding_forces(V_s, n, delta_deg, vessel, rudder)
    fallback = (
        None if regime == "bollard"
        else ad_flat_plate_forces(V_s, n, delta_deg, vessel, rudder)
    )
    return DriveOffPoint(
        regime, V_s, V_A, n, J, K_T, T, C_th_primary, C_T_fallback,
        clamped, slipstream, primary, fallback,
    )


def driveoff_sweep(
    V_s: float, n_values: Iterable[float], delta_deg: float,
    vessel: VesselPropulsion, rudder: RudderGeometry,
) -> list[DriveOffPoint]:
    """Report a shaft-speed sweep at 0.5 D rudder separation, without new forces.

    Astern quadrants are excluded. Engine-off omits propeller drag/windmilling;
    braking reports the primary increment as computed. At bollard the fallback
    force is not evaluated and the axial velocity uses its finite thrust limit.
    Bollard uses abs(V_A) < 1e-12, matching the fallback's inflow guard.
    """
    if not math.isfinite(V_s) or V_s < 0.0:
        raise ValueError("V_s must be finite and nonnegative")
    if not math.isfinite(delta_deg):
        raise ValueError("delta_deg must be finite")
    points = []
    for n in n_values:
        if not math.isfinite(n) or n < 0.0:
            raise ValueError("n must be finite and nonnegative")
        points.append(_driveoff_point(V_s, n, delta_deg, vessel, rudder))
    return points
