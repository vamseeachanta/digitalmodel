"""Splash zone assessment per DNV-RP-H103 Section 4.

Implements hydrodynamic forces during the splash zone phase of subsea
structure installation, including:
- Slamming forces at water entry (§4.3)
- Varying buoyancy forces (§4.4)
- Dynamic hook loads in the splash zone

Reference: DNV-RP-H103 (2011) Modelling and Analysis of Marine Operations.
"""
from __future__ import annotations

import numpy as np
from .models import Structure, InstallationCase, SplashZoneResult


def slamming_force(
    structure: Structure,
    lowering_velocity_m_s: float,
    wave_particle_velocity_m_s: float = 0.0,
    rho_w: float = 1025.0,
) -> float:
    """Slamming force at water entry per DNV-RP-H103 §4.3.4.

    F_slam = 0.5 * rho * C_s * A_p * v^2

    where v is the relative velocity between structure and water surface.

    Parameters
    ----------
    structure : Structure
        Structure with slamming coefficient C_s and projected area.
    lowering_velocity_m_s : float
        Crane tip lowering velocity [m/s].
    wave_particle_velocity_m_s : float
        Wave surface vertical particle velocity [m/s] (upward positive).
    rho_w : float
        Seawater density [kg/m3].

    Returns
    -------
    float
        Slamming force [N].
    """
    v_rel = abs(lowering_velocity_m_s + wave_particle_velocity_m_s)
    return 0.5 * rho_w * structure.C_s * structure.A_projected_m2 * v_rel**2


def varying_buoyancy_force(
    structure: Structure,
    wave_amplitude_m: float,
    rho_w: float = 1025.0,
) -> float:
    """Peak varying buoyancy force in splash zone per DNV-RP-H103 §4.4.

    When the structure is partially submerged, wave surface variations cause
    buoyancy oscillation. The peak varying buoyancy equals:

        F_var = rho * g * A_wp * zeta_a

    where A_wp is the waterplane area and zeta_a is wave amplitude.

    Parameters
    ----------
    structure : Structure
        Structure with length and width for waterplane area.
    wave_amplitude_m : float
        Wave single amplitude [m] (= Hs/2 for significant).
    rho_w : float
        Seawater density [kg/m3].

    Returns
    -------
    float
        Peak varying buoyancy force [N].
    """
    A_wp = structure.length_m * structure.width_m
    return rho_w * 9.80665 * A_wp * wave_amplitude_m


def splash_zone_assessment(
    case: InstallationCase,
    crane_tip_velocity_m_s: float,
    lowering_velocity_m_s: float = 0.25,
) -> SplashZoneResult:
    """Complete splash zone force assessment per DNV-RP-H103 §4.

    Computes all splash zone forces and determines min/max hook loads.

    Parameters
    ----------
    case : InstallationCase
        Installation case with structure, vessel, environment.
    crane_tip_velocity_m_s : float
        Significant crane tip vertical velocity [m/s] from spectral analysis.
    lowering_velocity_m_s : float
        Steady lowering velocity through crane [m/s].

    Returns
    -------
    SplashZoneResult
        Complete splash zone assessment results.
    """
    s = case.structure
    rho = case.rho_w_kg_m3

    # Wave amplitude (significant single amplitude)
    zeta_a = case.wave_hs_m / 2.0

    # Wave particle velocity at surface (linear wave theory approximation)
    omega_p = 2 * np.pi / case.wave_tp_s
    v_wave = omega_p * zeta_a

    # Total relative velocity for slamming
    v_total = lowering_velocity_m_s + crane_tip_velocity_m_s + v_wave

    # Slamming force
    F_slam = slamming_force(s, v_total, rho_w=rho)

    # Varying buoyancy
    F_var = varying_buoyancy_force(s, zeta_a, rho_w=rho)

    # Static weight in air
    W_air = s.weight_air_N

    # Submerged weight (half-submerged approximation for splash zone)
    W_sub_half = W_air - 0.5 * rho * 9.80665 * s.length_m * s.width_m * s.height_m

    # Dynamic hook loads in splash zone
    # Max: static weight + slamming + varying buoyancy (structure pulled down)
    F_max = W_sub_half + F_slam + F_var

    # Min: static weight - varying buoyancy (snap risk — DNV-RP-H103 §4.7)
    F_min = W_sub_half - F_var

    # Dynamic amplification factor
    daf = F_max / W_air if W_air > 0 else float("inf")

    return SplashZoneResult(
        slamming_force_N=F_slam,
        varying_buoyancy_N=F_var,
        total_hydrodynamic_force_N=F_slam + F_var,
        min_crane_load_N=F_min,
        max_crane_load_N=F_max,
        daf=daf,
        details={
            "lowering_velocity_m_s": lowering_velocity_m_s,
            "crane_tip_velocity_m_s": crane_tip_velocity_m_s,
            "wave_particle_velocity_m_s": v_wave,
            "total_relative_velocity_m_s": v_total,
            "wave_amplitude_m": zeta_a,
            "weight_air_N": W_air,
            "weight_submerged_half_N": W_sub_half,
            "snap_risk": F_min < 0,
        },
    )
