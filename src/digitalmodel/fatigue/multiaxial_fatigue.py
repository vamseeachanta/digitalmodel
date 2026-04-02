"""
Multiaxial Fatigue Criteria
============================

Methods for evaluating fatigue under combined loading (tension, bending,
torsion, shear), where the principal stress directions may rotate.

Implemented approaches:

1. **Von Mises equivalent stress range** — DNV-RP-C203 (2021) Section 4.5.1.
   Suitable when principal stress directions are fixed.
2. **Principal stress range method** — maximum principal stress range,
   suitable for proportional loading.
3. **Critical plane approach** — Findley (1959), Matake (1977).
   Searches for the plane with maximum fatigue damage.
4. **Shear stress correction** — IIW recommendation for weld roots
   under combined normal + shear.

References
----------
- DNV-RP-C203 (2021), Section 4.5 — Simplified fatigue analysis for combined loading
- IIW-2259-15, Section 3.4 — Multiaxial fatigue
- Findley, W.N. (1959), J. Eng. for Industry, Trans. ASME, 81, pp. 301-306
- Socie, D.F. & Marquis, G.B. (2000), Multiaxial Fatigue, SAE International
"""

import math
from typing import List, Optional, Tuple, Union

import numpy as np
from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------

class StressState(BaseModel):
    """Cyclic stress state at a point.

    All values are stress **ranges** (peak-to-peak) in MPa.

    Attributes
    ----------
    sigma_x : float — normal stress range in x-direction
    sigma_y : float — normal stress range in y-direction
    sigma_z : float — normal stress range in z-direction (default 0, plane stress)
    tau_xy : float  — shear stress range in xy-plane
    tau_xz : float  — shear stress range in xz-plane (default 0)
    tau_yz : float  — shear stress range in yz-plane (default 0)
    """
    sigma_x: float = 0.0
    sigma_y: float = 0.0
    sigma_z: float = 0.0
    tau_xy: float = 0.0
    tau_xz: float = 0.0
    tau_yz: float = 0.0


class MultiaxialResult(BaseModel):
    """Result of a multiaxial fatigue evaluation.

    Attributes
    ----------
    equivalent_stress_range : float
        Equivalent uniaxial stress range (MPa).
    method : str
        Name of the method used.
    principal_1 : float
        Maximum principal stress range.
    principal_2 : float
        Minimum principal stress range.
    biaxiality_ratio : float
        σ₂/σ₁ — indicates degree of biaxiality.
    critical_plane_angle : Optional[float]
        Angle (degrees) of the critical plane, if applicable.
    """
    equivalent_stress_range: float
    method: str = ""
    principal_1: float = 0.0
    principal_2: float = 0.0
    biaxiality_ratio: float = 0.0
    critical_plane_angle: Optional[float] = None


# ---------------------------------------------------------------------------
# Principal stress computation (2D)
# ---------------------------------------------------------------------------

def _principal_stresses_2d(
    sx: float, sy: float, txy: float
) -> Tuple[float, float, float]:
    """Return (σ₁, σ₂, θ_principal in degrees) for 2D stress state."""
    avg = (sx + sy) / 2.0
    R = math.sqrt(((sx - sy) / 2.0) ** 2 + txy ** 2)
    s1 = avg + R
    s2 = avg - R
    if abs(sx - sy) < 1e-12:
        theta = 45.0 if txy > 0 else -45.0 if txy < 0 else 0.0
    else:
        theta = math.degrees(0.5 * math.atan2(2.0 * txy, sx - sy))
    return s1, s2, theta


# ---------------------------------------------------------------------------
# Von Mises equivalent stress range
# ---------------------------------------------------------------------------

def von_mises_equivalent(state: StressState) -> MultiaxialResult:
    """Von Mises equivalent stress range.

    DNV-RP-C203 (2021) Section 4.5.1, Eq. 4.8::

        Δσ_eq = √(Δσ_x² + Δσ_y² − Δσ_x·Δσ_y + 3·Δτ_xy²)

    For 3D::

        Δσ_eq = √(0.5·[(σx-σy)² + (σy-σz)² + (σz-σx)²
                      + 6·(τxy² + τyz² + τxz²)])

    Parameters
    ----------
    state : StressState

    Returns
    -------
    MultiaxialResult
    """
    sx, sy, sz = state.sigma_x, state.sigma_y, state.sigma_z
    txy, txz, tyz = state.tau_xy, state.tau_xz, state.tau_yz

    vm = math.sqrt(
        0.5 * ((sx - sy) ** 2 + (sy - sz) ** 2 + (sz - sx) ** 2)
        + 3.0 * (txy ** 2 + txz ** 2 + tyz ** 2)
    )

    s1, s2, theta = _principal_stresses_2d(sx, sy, txy)
    br = s2 / s1 if abs(s1) > 1e-12 else 0.0

    return MultiaxialResult(
        equivalent_stress_range=round(vm, 3),
        method="Von Mises equivalent (DNV-RP-C203 Eq. 4.8)",
        principal_1=round(s1, 3),
        principal_2=round(s2, 3),
        biaxiality_ratio=round(br, 4),
    )


def principal_stress_range(state: StressState) -> MultiaxialResult:
    """Maximum principal stress range method.

    Returns the largest principal stress range as the equivalent value.
    Suitable for proportional loading where principal directions are fixed.

    Per IIW-2259-15 Section 3.4.1.

    Parameters
    ----------
    state : StressState

    Returns
    -------
    MultiaxialResult
    """
    s1, s2, theta = _principal_stresses_2d(
        state.sigma_x, state.sigma_y, state.tau_xy
    )

    # The maximum principal stress range
    eq = max(abs(s1), abs(s2))
    br = s2 / s1 if abs(s1) > 1e-12 else 0.0

    return MultiaxialResult(
        equivalent_stress_range=round(eq, 3),
        method="Maximum principal stress range (IIW-2259-15 Sec 3.4.1)",
        principal_1=round(s1, 3),
        principal_2=round(s2, 3),
        biaxiality_ratio=round(br, 4),
        critical_plane_angle=round(theta, 2),
    )


# ---------------------------------------------------------------------------
# Critical plane approach — Findley criterion
# ---------------------------------------------------------------------------

def findley_critical_plane(
    state: StressState,
    k_findley: float = 0.3,
    n_angles: int = 180,
) -> MultiaxialResult:
    """Findley critical plane criterion.

    Searches for the plane where the combination
    ``τ_a + k · σ_n,max`` is maximised, where:
    - τ_a = shear stress amplitude on the plane
    - σ_n,max = maximum normal stress on the plane
    - k = Findley material constant (typically 0.2–0.4 for steel)

    Reference: Findley (1959).

    Parameters
    ----------
    state : StressState
        Stress ranges (assumed zero-to-max for simplicity).
    k_findley : float
        Material constant. Default 0.3 (typical for structural steel).
    n_angles : int
        Number of candidate plane angles to evaluate. Default 180.

    Returns
    -------
    MultiaxialResult
    """
    sx = state.sigma_x / 2.0  # amplitude = range/2
    sy = state.sigma_y / 2.0
    txy = state.tau_xy / 2.0

    best_damage = -1.0
    best_angle = 0.0

    for i in range(n_angles):
        theta = math.pi * i / n_angles
        cos2 = math.cos(2.0 * theta)
        sin2 = math.sin(2.0 * theta)

        # Normal stress on plane at angle θ
        sigma_n = (sx + sy) / 2.0 + (sx - sy) / 2.0 * cos2 + txy * sin2
        # Shear stress on plane
        tau_plane = -(sx - sy) / 2.0 * sin2 + txy * cos2

        damage_param = abs(tau_plane) + k_findley * sigma_n

        if damage_param > best_damage:
            best_damage = damage_param
            best_angle = math.degrees(theta)

    # The equivalent stress range is 2× the critical damage parameter
    # (converting amplitude back to range)
    eq = 2.0 * best_damage

    s1, s2, _ = _principal_stresses_2d(state.sigma_x, state.sigma_y, state.tau_xy)
    br = s2 / s1 if abs(s1) > 1e-12 else 0.0

    return MultiaxialResult(
        equivalent_stress_range=round(eq, 3),
        method=f"Findley critical plane (k={k_findley})",
        principal_1=round(s1, 3),
        principal_2=round(s2, 3),
        biaxiality_ratio=round(br, 4),
        critical_plane_angle=round(best_angle, 2),
    )


# ---------------------------------------------------------------------------
# Shear stress correction factor — IIW
# ---------------------------------------------------------------------------

def shear_stress_correction(
    delta_sigma: float,
    delta_tau: float,
) -> float:
    """IIW combined normal + shear interaction check.

    IIW-2259-15 Eq. (3.4-1)::

        (Δσ / Δσ_R)^2 + (Δτ / Δτ_R)^2 ≤ 1

    This function returns the equivalent stress range assuming the
    shear S-N curve has the same exponent as the normal stress curve::

        Δσ_eq = √(Δσ² + 3·Δτ²)

    (This is the von Mises simplification for uniaxial + shear.)

    Parameters
    ----------
    delta_sigma : float
        Normal stress range (MPa).
    delta_tau : float
        Shear stress range (MPa).

    Returns
    -------
    float
        Equivalent stress range (MPa).
    """
    return round(math.sqrt(delta_sigma ** 2 + 3.0 * delta_tau ** 2), 3)


def multiaxial_damage_interaction(
    damage_normal: float,
    damage_shear: float,
    exponent: float = 2.0,
) -> float:
    """Multiaxial interaction damage check.

    IIW-2259-15 Eq. (3.4-2)::

        D_v = (D_σ)^e + (D_τ)^e

    where e = 2 for Miner's superposition (typical).

    Parameters
    ----------
    damage_normal : float
        Cumulative damage from normal stress.
    damage_shear : float
        Cumulative damage from shear stress.
    exponent : float
        Interaction exponent (default 2.0 per IIW).

    Returns
    -------
    float
        Combined interaction damage parameter.
        Must be ≤ 1.0 to pass.
    """
    return round(damage_normal ** exponent + damage_shear ** exponent, 6)
