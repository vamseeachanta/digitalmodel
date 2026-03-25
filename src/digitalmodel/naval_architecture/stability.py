# ABOUTME: Ship stability calculations — GZ curves, free surface effect
# ABOUTME: Validated against USNA EN400 Chapter 4 worked examples
"""
Ship stability calculations.

Covers righting arm (GZ) from cross curves, TCG cosine correction,
and free surface effect from USNA EN400 Chapter 4.
"""

import math
from typing import Tuple

G = 32.17  # ft/s²
LT_TO_LB = 2240.0


def gz_from_cross_curves(
    heel_deg: float, kn_ft: float, kg_ft: float
) -> float:
    """Corrected righting arm from cross curves of stability.

    GZ = KN - KG * sin(heel)

    Args:
        heel_deg: heel angle in degrees
        kn_ft: KN value from cross curves at this displacement/heel
        kg_ft: height of center of gravity above keel
    """
    return kn_ft - kg_ft * math.sin(math.radians(heel_deg))


def gz_with_tcg_correction(
    gz_ft: float, tcg_ft: float, heel_deg: float
) -> float:
    """Correct GZ for transverse CG offset (cosine correction).

    GZ_corrected = GZ - TCG * cos(heel)
    """
    return gz_ft - tcg_ft * math.cos(math.radians(heel_deg))


def free_surface_correction(
    displacement_lt: float,
    kg_ft: float,
    km_ft: float,
    tank_length_ft: float,
    tank_breadth_ft: float,
    rho_fluid: float,
) -> Tuple[float, float]:
    """Calculate free surface correction and effective GM.

    FSC = rho * g * i / delta(lb)
    where i = L * B³ / 12 for rectangular tank

    Args:
        displacement_lt: ship displacement in long tons
        kg_ft: KG in feet
        km_ft: KM (metacentric height above keel) in feet
        tank_length_ft: tank length in feet
        tank_breadth_ft: tank breadth in feet
        rho_fluid: fluid density in slug/ft³

    Returns:
        (free_surface_correction_ft, effective_gm_ft)
    """
    gm_solid = km_ft - kg_ft
    i_tank = tank_length_ft * tank_breadth_ft**3 / 12.0
    fsc = rho_fluid * G * i_tank / (displacement_lt * LT_TO_LB)
    gm_effective = gm_solid - fsc
    return fsc, gm_effective
