"""DNV-RP-F109 -- On-Bottom Stability Design of Submarine Pipelines (2021).

Implements pipeline on-bottom stability checks including:
- Hydrodynamic force calculation (drag + inertia)
- Lift force calculation
- Submerged weight per unit length
- Absolute lateral stability (S4.3.1)
- Generalized lateral stability with passive soil resistance (S4.3.2)

Reference: DNV-RP-F109, Edition October 2021.

Qualification: edition, clauses and coefficient defaults above are legacy
attributions, not independently verified standard requirements. See
docs/validation-claims.md. Use assessment.py with explicit supplied factors for
auditable arithmetic. Coefficients and safety factors are required inputs;
legacy constants remain exported only for compatibility and are unqualified.
The generalized ratio is a static force balance; it does not compute a standard
generalised displacement envelope or a time history.
"""
from __future__ import annotations

import math
from typing import NamedTuple

from ._validation import _geometry, _loads, _stability


class StabilityResult(NamedTuple):
    """Result of a stability check.

    Attributes
    ----------
    utilisation : float
        Demand / capacity ratio. Values <= 1.0 indicate stability.
    is_stable : bool
        True if utilisation <= 1.0.
    details : dict
        Intermediate values for audit trail.
    """

    utilisation: float
    is_stable: bool
    details: dict


# ---------------------------------------------------------------------------
# Hydrodynamic coefficients (DNV-RP-F109 Table 3-3)
# ---------------------------------------------------------------------------
C_D_SMOOTH: float = 0.9    # Drag coefficient, smooth pipe
C_L_SMOOTH: float = 0.9    # Lift coefficient, smooth pipe
C_M_SMOOTH: float = 3.29   # Inertia coefficient, smooth pipe

C_D_ROUGH: float = 1.2     # Drag coefficient, rough pipe (marine growth)
C_L_ROUGH: float = 1.0     # Lift coefficient, rough pipe
C_M_ROUGH: float = 3.29    # Inertia coefficient, rough pipe

# ---------------------------------------------------------------------------
# Safety class factors (DNV-RP-F109 Table 4-1)
# ---------------------------------------------------------------------------
GAMMA_SC_NORMAL: float = 1.1  # Safety class factor, normal safety class


def hydrodynamic_force_per_meter(
    rho_w_kg_m3: float,
    D_outer_m: float,
    U_m_s: float,
    a_m_s2: float,
    C_D: float,
    C_M: float,
) -> float:
    """Inline hydrodynamic force per unit length (DNV-RP-F109 S3.2.1, Eq 3.1).

    F_H = 0.5 * rho * C_D * D * |U| * U  +  rho * C_M * (pi/4) * D^2 * a

    Parameters
    ----------
    rho_w_kg_m3 : float
        Seawater density [kg/m3].
    D_outer_m : float
        Outer pipe diameter including coatings [m].
    U_m_s : float
        Combined current + wave velocity at pipe centreline [m/s].
    a_m_s2 : float
        Combined current + wave acceleration at pipe centreline [m/s2].
    C_D : float
        Required drag coefficient; legacy table attribution is unverified.
    C_M : float
        Required inertia coefficient; legacy table attribution is unverified.

    Returns
    -------
    float
        Horizontal hydrodynamic force per meter [N/m].
    """
    _loads(rho_w_kg_m3, D_outer_m, U_m_s, a_m_s2, C_D, C_M, 0)
    drag = 0.5 * rho_w_kg_m3 * C_D * D_outer_m * abs(U_m_s) * U_m_s
    inertia = rho_w_kg_m3 * C_M * (math.pi / 4) * D_outer_m**2 * a_m_s2
    return drag + inertia


def lift_force_per_meter(
    rho_w_kg_m3: float,
    D_outer_m: float,
    U_m_s: float,
    C_L: float,
) -> float:
    """Lift force per unit length (DNV-RP-F109 S3.2.1, Eq 3.2).

    F_L = 0.5 * rho * C_L * D * U^2

    Parameters
    ----------
    rho_w_kg_m3 : float
        Seawater density [kg/m3].
    D_outer_m : float
        Outer pipe diameter including coatings [m].
    U_m_s : float
        Combined current + wave velocity at pipe centreline [m/s].
    C_L : float
        Required lift coefficient; legacy table attribution is unverified.

    Returns
    -------
    float
        Vertical lift force per meter [N/m]. Always non-negative.
    """
    _loads(rho_w_kg_m3, D_outer_m, U_m_s, 0, 0, 0, C_L)
    return 0.5 * rho_w_kg_m3 * C_L * D_outer_m * U_m_s**2


def submerged_weight_per_meter(
    D_outer_m: float,
    t_wall_m: float,
    rho_steel_kg_m3: float,
    rho_coat_kg_m3: float,
    t_coat_m: float,
    rho_contents_kg_m3: float,
    rho_w_kg_m3: float,
) -> float:
    """Submerged weight per unit length (DNV-RP-F109 S2.2).

    W_s = (m_steel + m_coat + m_contents - m_displaced) * g

    Calculates the net downward force per meter of pipe, accounting for
    steel wall, coating, internal contents, and displaced seawater.

    Parameters
    ----------
    D_outer_m : float
        Outer steel diameter [m].
    t_wall_m : float
        Steel wall thickness [m].
    rho_steel_kg_m3 : float
        Steel density [kg/m3], typically 7850.
    rho_coat_kg_m3 : float
        Coating density [kg/m3]. Use 0 if no coating.
    t_coat_m : float
        Coating thickness [m]. Use 0 if no coating.
    rho_contents_kg_m3 : float
        Internal contents density [kg/m3]. 0 for empty/gas, 1025 for seawater.
    rho_w_kg_m3 : float
        Seawater density [kg/m3].

    Returns
    -------
    float
        Submerged weight per meter [N/m]. Positive = sinks, negative = buoyant.
    """
    _geometry(D_outer_m, t_wall_m, t_coat_m, rho_steel_kg_m3,
              rho_coat_kg_m3, rho_contents_kg_m3, rho_w_kg_m3)
    g = 9.80665  # m/s2

    # Inner diameter
    D_inner_m = D_outer_m - 2.0 * t_wall_m

    # Cross-sectional areas
    A_outer_steel = (math.pi / 4) * D_outer_m**2
    A_inner_steel = (math.pi / 4) * D_inner_m**2
    A_steel = A_outer_steel - A_inner_steel

    # Coating: concentric annulus around the steel pipe
    D_outer_coat = D_outer_m + 2.0 * t_coat_m
    A_outer_coat = (math.pi / 4) * D_outer_coat**2
    A_coat = A_outer_coat - A_outer_steel

    # Contents: fills inside the steel pipe
    A_contents = A_inner_steel

    # Displaced water: based on outer diameter including coating
    A_displaced = A_outer_coat

    # Mass per meter [kg/m]
    m_steel = rho_steel_kg_m3 * A_steel
    m_coat = rho_coat_kg_m3 * A_coat
    m_contents = rho_contents_kg_m3 * A_contents
    m_displaced = rho_w_kg_m3 * A_displaced

    return (m_steel + m_coat + m_contents - m_displaced) * g


def absolute_stability_check(
    W_s_N_m: float,
    F_H_N_m: float,
    F_L_N_m: float,
    mu_soil: float,
    gamma_SC: float,
) -> StabilityResult:
    """Absolute lateral stability check (DNV-RP-F109 S4.3.1, Eq 4.1).

    utilisation = gamma_SC * (F_H + mu * F_L) / (mu * W_s)

    Pipe is stable if utilisation <= 1.0.

    Parameters
    ----------
    W_s_N_m : float
        Submerged weight per meter [N/m].
    F_H_N_m : float
        Horizontal hydrodynamic force per meter [N/m].
    F_L_N_m : float
        Lift force per meter [N/m].
    mu_soil : float
        Lateral soil friction coefficient [-].
    gamma_SC : float
        Required safety factor; safety-class table attribution is unverified.

    Returns
    -------
    StabilityResult
        Utilisation ratio, stability flag, and intermediate values.
    """
    _stability(W_s_N_m, F_H_N_m, F_L_N_m, mu_soil, 0, gamma_SC)
    # Scalar stability uses lateral magnitude in either direction; Morison
    # outputs retain their sign. See validation analysis, force balance.
    F_H_N_m = abs(F_H_N_m)
    details = {
        "W_s_N_m": W_s_N_m,
        "F_H_N_m": F_H_N_m,
        "F_L_N_m": F_L_N_m,
        "mu_soil": mu_soil,
        "gamma_SC": gamma_SC,
    }

    # Contact condition W > FL (analysis A4) also excludes buoyant pipe.
    if W_s_N_m <= F_L_N_m:
        details["capacity"] = 0.0
        details["demand"] = gamma_SC * (F_H_N_m + mu_soil * F_L_N_m)
        return StabilityResult(
            utilisation=float("inf"),
            is_stable=False,
            details=details,
        )

    capacity = mu_soil * W_s_N_m
    demand = gamma_SC * (F_H_N_m + mu_soil * F_L_N_m)

    if capacity <= 0.0:
        utilisation = float("inf")
    else:
        utilisation = demand / capacity

    details["capacity"] = capacity
    details["demand"] = demand

    return StabilityResult(
        utilisation=utilisation,
        is_stable=utilisation <= 1.0,
        details=details,
    )


def generalized_stability_check(
    W_s_N_m: float,
    F_H_N_m: float,
    F_L_N_m: float,
    mu_soil: float,
    F_R_N_m: float,
    gamma_SC: float,
) -> StabilityResult:
    """Generalized lateral stability with soil resistance (DNV-RP-F109 S4.3.2, Eq 4.5).

    utilisation = gamma_SC * F_H / (mu * (W_s - F_L) + F_R)

    Includes passive soil resistance F_R which augments lateral capacity.
    Pipe is stable if utilisation <= 1.0.

    Parameters
    ----------
    W_s_N_m : float
        Submerged weight per meter [N/m].
    F_H_N_m : float
        Horizontal hydrodynamic force per meter [N/m].
    F_L_N_m : float
        Lift force per meter [N/m].
    mu_soil : float
        Lateral soil friction coefficient [-].
    F_R_N_m : float
        Passive soil resistance per meter [N/m].
    gamma_SC : float
        Required safety factor; safety-class table attribution is unverified.

    Returns
    -------
    StabilityResult
        Utilisation ratio, stability flag, and intermediate values.
    """
    _stability(W_s_N_m, F_H_N_m, F_L_N_m, mu_soil, F_R_N_m, gamma_SC)
    F_H_N_m = abs(F_H_N_m)
    details = {
        "W_s_N_m": W_s_N_m,
        "F_H_N_m": F_H_N_m,
        "F_L_N_m": F_L_N_m,
        "mu_soil": mu_soil,
        "F_R_N_m": F_R_N_m,
        "gamma_SC": gamma_SC,
    }

    capacity = mu_soil * (W_s_N_m - F_L_N_m) + F_R_N_m
    demand = gamma_SC * F_H_N_m

    details["capacity"] = capacity
    details["demand"] = demand

    # No unrestrained lateral-soil model is valid after loss of bed contact.
    # See validation analysis, contact condition W > FL.
    if W_s_N_m <= F_L_N_m or capacity <= 0.0:
        return StabilityResult(
            utilisation=float("inf"),
            is_stable=False,
            details=details,
        )

    utilisation = demand / capacity

    return StabilityResult(
        utilisation=utilisation,
        is_stable=utilisation <= 1.0,
        details=details,
    )
