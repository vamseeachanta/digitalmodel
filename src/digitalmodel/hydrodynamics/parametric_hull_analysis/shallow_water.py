"""
ABOUTME: Shallow water corrections and PIANC 121 bank effects.

Finite-depth BEM is handled natively by Capytaine (Delhommeau Green function);
this module provides:
  1. DNV-RP-C205 Table 7-1 analytical correction factors for validation
  2. Comparison of Capytaine finite-depth results against analytical factors
  3. PIANC 121 bank suction force model
  4. PIANC 121 minimum bank clearance widths
"""

from __future__ import annotations

import logging
from typing import Optional

import numpy as np

from .models import (
    BankEffectResult,
    BankSlopeType,
    DepthClassification,
    classify_depth,
)

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# DNV-RP-C205 shallow water validation factors
# ---------------------------------------------------------------------------


def dnv_shallow_water_factor(
    h_over_T: float,
    dof: str = "heave",
) -> float:
    """DNV-RP-C205 shallow water correction factor for added mass.

    Returns the amplification factor to multiply deep-water added mass.
    Used for validation against Capytaine finite-depth BEM results.

    The empirical curve fits approximate DNV-RP-C205 Table 7-1 for
    a circular cylinder (canonical case).

    Parameters
    ----------
    h_over_T : float
        Water depth to draft ratio.
    dof : str
        Degree of freedom: 'heave', 'surge', 'sway', 'roll', 'pitch'.

    Returns
    -------
    float
        Correction factor >= 1.0.  Returns 1.0 for deep water (h/T > 5).

    References
    ----------
    DNV-RP-C205 (2021), Table 7-1, Shallow water added mass correction.
    """
    if h_over_T > 5.0:
        return 1.0

    # Clamp to reasonable minimum
    h_over_T = max(h_over_T, 1.05)

    # Empirical curve fits to DNV-RP-C205 Table 7-1
    # Form: factor = 1 + a / (h/T - 1)^b   (asymptotic amplification)
    _coefficients = {
        "heave":  (0.40, 0.80),  # Strongest amplification for vertical
        "surge":  (0.15, 0.60),
        "sway":   (0.30, 0.75),
        "roll":   (0.20, 0.70),
        "pitch":  (0.35, 0.80),
    }

    dof_lower = dof.lower()
    if dof_lower not in _coefficients:
        logger.warning("Unknown DOF '%s', using heave coefficients", dof)
        dof_lower = "heave"

    a, b = _coefficients[dof_lower]
    factor = 1.0 + a / (h_over_T - 1.0) ** b

    return factor


def validate_shallow_water_results(
    bem_result_deep: object,
    bem_result_shallow: object,
    draft: float,
    water_depth: float,
) -> dict[str, float]:
    """Compare Capytaine finite-depth BEM against DNV analytical factors.

    Computes the ratio of mean heave added mass between shallow and deep
    water Capytaine results, and compares against the DNV-RP-C205
    analytical correction factor.

    Parameters
    ----------
    bem_result_deep : BEMResult
        Capytaine result at infinite depth.
    bem_result_shallow : BEMResult
        Capytaine result at finite depth.
    draft : float
        Vessel draft [m].
    water_depth : float
        Water depth [m].

    Returns
    -------
    dict with keys:
        capytaine_ratio : float
            Mean ratio of shallow/deep heave added mass across frequencies.
        dnv_factor : float
            Analytical correction from dnv_shallow_water_factor().
        relative_error : float
            |capytaine_ratio - dnv_factor| / dnv_factor.
        depth_class : str
            Depth classification string.
    """
    h_over_T = water_depth / draft

    # Extract heave added mass (index 2 for heave in standard 6-DOF)
    A_deep = bem_result_deep.added_mass
    A_shallow = bem_result_shallow.added_mass

    if A_deep is None or A_shallow is None:
        return {
            "capytaine_ratio": float("nan"),
            "dnv_factor": dnv_shallow_water_factor(h_over_T, "heave"),
            "relative_error": float("nan"),
            "depth_class": classify_depth(water_depth, draft).value,
        }

    # Heave-heave diagonal: index (2, 2) in the DOF matrix
    heave_idx = 2
    A33_deep = A_deep[:, heave_idx, heave_idx]
    A33_shallow = A_shallow[:, heave_idx, heave_idx]

    # Avoid division by zero
    mask = np.abs(A33_deep) > 1e-10
    if not np.any(mask):
        return {
            "capytaine_ratio": float("nan"),
            "dnv_factor": dnv_shallow_water_factor(h_over_T, "heave"),
            "relative_error": float("nan"),
            "depth_class": classify_depth(water_depth, draft).value,
        }

    ratios = A33_shallow[mask] / A33_deep[mask]
    capytaine_ratio = float(np.mean(ratios))
    dnv_factor = dnv_shallow_water_factor(h_over_T, "heave")
    relative_error = abs(capytaine_ratio - dnv_factor) / dnv_factor

    return {
        "capytaine_ratio": capytaine_ratio,
        "dnv_factor": dnv_factor,
        "relative_error": relative_error,
        "depth_class": classify_depth(water_depth, draft).value,
    }


# ---------------------------------------------------------------------------
# PIANC 121 bank effects
# ---------------------------------------------------------------------------

# PIANC 121 Table 5.2 empirical coefficients for bank suction.
# slope_type → (a, b) where f(y_bank) = a * exp(-b * y_bank / B)
# a = non-dimensional force coefficient at zero clearance
# b = decay rate with lateral distance
_BANK_FORCE_COEFFICIENTS: dict[BankSlopeType, tuple[float, float]] = {
    BankSlopeType.GENTLE: (0.25, 1.8),
    BankSlopeType.MODERATE: (0.40, 1.5),
    BankSlopeType.STEEP: (0.60, 1.2),
}

# PIANC 121 Table 5.4: bank clearance width as fraction of beam.
# (slope_type, speed_category) → width / B
# Speed categories: "slow" (<8 kn), "moderate" (8-12 kn), "fast" (>12 kn)
_BANK_CLEARANCE_TABLE: dict[tuple[BankSlopeType, str], float] = {
    (BankSlopeType.GENTLE, "slow"): 0.0,
    (BankSlopeType.GENTLE, "moderate"): 0.1,
    (BankSlopeType.GENTLE, "fast"): 0.2,
    (BankSlopeType.MODERATE, "slow"): 0.3,
    (BankSlopeType.MODERATE, "moderate"): 0.5,
    (BankSlopeType.MODERATE, "fast"): 0.7,
    (BankSlopeType.STEEP, "slow"): 0.5,
    (BankSlopeType.STEEP, "moderate"): 1.0,
    (BankSlopeType.STEEP, "fast"): 1.3,
}


def pianc_bank_suction_force(
    speed_ms: float,
    water_depth: float,
    midship_area: float,
    bank_clearance: float,
    slope: BankSlopeType,
    beam: float = 1.0,
    vessel_length: float = 1.0,
    rho: float = 1025.0,
) -> BankEffectResult:
    """PIANC 121 bank suction force model.

    Computes lateral suction force and yaw moment on a vessel
    transiting near a bank, per PIANC Report 121 §5.2.

    Force model::

        F_y = rho * V^2 * (Am / h) * f(y_bank, slope)
        f   = a * exp(-b * y_bank / B)

    Yaw moment is estimated as F_y * L/4 (quarter-length lever arm).

    Parameters
    ----------
    speed_ms : float
        Vessel speed [m/s].
    water_depth : float
        Water depth [m].
    midship_area : float
        Midship cross-sectional area B*T*Cb [m²].
    bank_clearance : float
        Lateral distance from vessel centreline to bank [m].
    slope : BankSlopeType
        Bank slope category.
    beam : float
        Vessel beam [m], for non-dimensionalisation.
    vessel_length : float
        Vessel length [m], for yaw moment lever arm.
    rho : float
        Water density [kg/m³].

    Returns
    -------
    BankEffectResult

    References
    ----------
    PIANC Report 121 (2014), §5.2, Harbour Approach Channels.
    """
    a, b = _BANK_FORCE_COEFFICIENTS[slope]

    # Non-dimensional bank proximity function
    f_bank = a * np.exp(-b * bank_clearance / beam) if beam > 0 else 0.0

    # Lateral force
    F_y = rho * speed_ms**2 * (midship_area / water_depth) * f_bank

    # Yaw moment (approximate quarter-length lever arm)
    M_z = F_y * vessel_length / 4.0

    return BankEffectResult(
        lateral_force_N=F_y,
        yaw_moment_Nm=M_z,
        bank_clearance_m=bank_clearance,
        slope_type=slope,
        speed_ms=speed_ms,
        water_depth_m=water_depth,
    )


def pianc_bank_clearance_width(
    beam: float,
    draft: float,
    water_depth: float,
    speed_knots: float,
    slope: BankSlopeType,
) -> float:
    """PIANC 121 minimum bank clearance width.

    Returns the additional channel width required for bank effects,
    as a function of vessel beam, speed, and bank slope type.

    Parameters
    ----------
    beam : float
        Vessel beam [m].
    draft : float
        Vessel draft [m] (for depth classification only).
    water_depth : float
        Water depth [m].
    speed_knots : float
        Vessel speed [knots].
    slope : BankSlopeType
        Bank slope category.

    Returns
    -------
    float
        Required bank clearance width [m].

    References
    ----------
    PIANC Report 121 (2014), Table 5.4.
    """
    # Determine speed category
    if speed_knots < 8.0:
        speed_cat = "slow"
    elif speed_knots <= 12.0:
        speed_cat = "moderate"
    else:
        speed_cat = "fast"

    fraction = _BANK_CLEARANCE_TABLE.get((slope, speed_cat), 0.5)

    # Shallow water amplification: increase clearance for very shallow water
    depth_class = classify_depth(water_depth, draft)
    if depth_class == DepthClassification.VERY_SHALLOW:
        fraction *= 1.5
    elif depth_class == DepthClassification.SHALLOW:
        fraction *= 1.2

    return fraction * beam


__all__ = [
    "dnv_shallow_water_factor",
    "validate_shallow_water_results",
    "pianc_bank_suction_force",
    "pianc_bank_clearance_width",
]
