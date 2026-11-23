"""Drilling riser structural damping calculations — damping ratios, Rayleigh model.

Implements structural damping per 50043-PRS-0001-1 (DeepStar test data),
2H-TNE-0050-03 §4.9, and Rayleigh damping theory.
"""

from __future__ import annotations

import math

# ---------------------------------------------------------------------------
# Structural damping ratios from DeepStar test data (50043-PRS-0001-1)
# ---------------------------------------------------------------------------
DAMPING_RATIOS: dict[str, dict[str, float]] = {
    "steel": {
        "bare": 0.003,  # 0.3% — plain pipe, DeepStar §3
        "buoyant": 0.008,  # 0.8% — with buoyancy modules
        "bolted_connector": 0.010,  # 1.0% — bolted flanged connector
    },
    "titanium": {
        "bare": 0.002,  # 0.2% — titanium stress joint
        "buoyant": 0.006,  # 0.6% — with buoyancy
        "bolted_connector": 0.008,
    },
}


def structural_damping_ratio(
    material: str,
    joint_type: str,
) -> float:
    """Look up structural damping ratio from DeepStar test data.

    Per 50043-PRS-0001-1 and 2H-TNE-0050-03 §4.9.

    Parameters
    ----------
    material : str
        Riser material ("steel" or "titanium").
    joint_type : str
        Joint configuration ("bare", "buoyant", "bolted_connector").

    Returns
    -------
    float
        Structural damping ratio [-] (fraction of critical).
    """
    return DAMPING_RATIOS[material][joint_type]


def rayleigh_damping_coefficients(
    omega_1: float,
    omega_2: float,
    zeta: float,
) -> tuple[float, float]:
    """Calculate Rayleigh damping coefficients alpha and beta.

    C = alpha * M + beta * K

    alpha = 2 * zeta * omega_1 * omega_2 / (omega_1 + omega_2)
    beta  = 2 * zeta / (omega_1 + omega_2)

    Parameters
    ----------
    omega_1 : float
        First target circular frequency [rad/s].
    omega_2 : float
        Second target circular frequency [rad/s].
    zeta : float
        Target damping ratio [-].

    Returns
    -------
    tuple[float, float]
        (alpha, beta) — mass-proportional and stiffness-proportional coefficients.
    """
    omega_sum = omega_1 + omega_2
    alpha = 2.0 * zeta * omega_1 * omega_2 / omega_sum
    beta = 2.0 * zeta / omega_sum
    return alpha, beta


def modal_damping_equivalent(
    mass_proportional: float,
    stiffness_proportional: float,
    freq_hz: float,
) -> float:
    """Calculate equivalent modal damping ratio from Rayleigh coefficients.

    zeta_n = alpha / (2 * omega_n) + beta * omega_n / 2

    Parameters
    ----------
    mass_proportional : float
        Alpha coefficient (mass-proportional) [-].
    stiffness_proportional : float
        Beta coefficient (stiffness-proportional) [-].
    freq_hz : float
        Natural frequency [Hz].

    Returns
    -------
    float
        Equivalent modal damping ratio [-].
    """
    omega_n = 2.0 * math.pi * freq_hz
    return mass_proportional / (2.0 * omega_n) + stiffness_proportional * omega_n / 2.0
