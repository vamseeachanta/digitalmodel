# ABOUTME: Composite pile capacity API per API RP 2GEO — alpha method with multi-layer support.
# ABOUTME: Provides alpha_method_capacity() and alpha_method_capacity_multilayer() (#1814).
"""Composite pile capacity API — API RP 2GEO alpha method.

Provides a higher-level interface over piles.py with:
- alpha_method_capacity: single-layer convenience function
- alpha_method_capacity_multilayer: multi-layer support via (thickness, Su, sigma_v) tuples

Both return PileCapacityResult with full breakdown.
"""
from __future__ import annotations

import math
from dataclasses import dataclass

from digitalmodel.geotechnical.piles import (
    skin_friction_clay,
    end_bearing_clay,
    STANDARD,
    DEFAULT_NC,
)


@dataclass
class PileCapacityResult:
    """Full pile axial capacity result for alpha method (API RP 2GEO)."""

    total_capacity_kn: float
    skin_friction_kn: float
    end_bearing_kn: float
    alpha: float
    unit_skin_friction_kpa: float
    standard: str = STANDARD


def alpha_method_capacity(
    D: float,
    L: float,
    Su: float,
    sigma_v: float,
    Nc: float = DEFAULT_NC,
) -> PileCapacityResult:
    """Single-layer alpha method pile capacity per API RP 2GEO Sec 7.3.

    Q = pi * D * L * f + Nc * Su * Ap
    f = alpha * Su
    alpha = 0.5 * (Su/sigma_v')^(-0.5)  for Su/sigma_v' <= 1.0
    alpha = 0.5 * (Su/sigma_v')^(-0.25) for Su/sigma_v' > 1.0

    Args:
        D: Pile outer diameter (m).
        L: Embedded pile length (m).
        Su: Undrained shear strength (kPa).
        sigma_v: Effective vertical overburden stress (kPa).
        Nc: End bearing capacity factor (default 9.0 per API RP 2GEO).

    Returns:
        PileCapacityResult with total capacity and breakdown.
    """
    sf = skin_friction_clay(
        undrained_shear_strength_kpa=Su,
        effective_overburden_kpa=sigma_v,
    )
    eb = end_bearing_clay(
        undrained_shear_strength_kpa=Su,
        nc=Nc,
    )

    perimeter = math.pi * D
    tip_area = math.pi * (D / 2.0) ** 2

    skin_friction_kn = sf.unit_friction_kpa * perimeter * L
    end_bearing_kn = eb.unit_bearing_kpa * tip_area

    return PileCapacityResult(
        total_capacity_kn=skin_friction_kn + end_bearing_kn,
        skin_friction_kn=skin_friction_kn,
        end_bearing_kn=end_bearing_kn,
        alpha=sf.alpha,
        unit_skin_friction_kpa=sf.unit_friction_kpa,
    )


def alpha_method_capacity_multilayer(
    D: float,
    layers: list[tuple[float, float, float]],
    Nc: float = DEFAULT_NC,
) -> PileCapacityResult:
    """Multi-layer alpha method pile capacity per API RP 2GEO Sec 7.3.

    Sums skin friction across all layers. End bearing is computed from
    the last (tip) layer's undrained shear strength.

    Args:
        D: Pile outer diameter (m).
        layers: List of (thickness_m, Su_kpa, sigma_v_kpa) tuples.
                Layers are ordered top-to-bottom; last entry is the tip layer.
        Nc: End bearing capacity factor (default 9.0).

    Returns:
        PileCapacityResult with total capacity, skin friction, end bearing,
        plus alpha and unit_skin_friction from the last (tip) layer.

    Raises:
        ValueError: If layers is empty.
    """
    if not layers:
        raise ValueError("layers must not be empty")

    perimeter = math.pi * D
    tip_area = math.pi * (D / 2.0) ** 2

    total_skin_friction_kn = 0.0

    for thickness, su, sigma_v in layers:
        sf = skin_friction_clay(
            undrained_shear_strength_kpa=su,
            effective_overburden_kpa=sigma_v,
        )
        layer_skin_kn = sf.unit_friction_kpa * perimeter * thickness
        total_skin_friction_kn += layer_skin_kn

    # End bearing from tip (last) layer
    tip_thickness, tip_su, tip_sigma_v = layers[-1]
    sf_tip = skin_friction_clay(
        undrained_shear_strength_kpa=tip_su,
        effective_overburden_kpa=tip_sigma_v,
    )
    eb_tip = end_bearing_clay(
        undrained_shear_strength_kpa=tip_su,
        nc=Nc,
    )
    end_bearing_kn = eb_tip.unit_bearing_kpa * tip_area

    return PileCapacityResult(
        total_capacity_kn=total_skin_friction_kn + end_bearing_kn,
        skin_friction_kn=total_skin_friction_kn,
        end_bearing_kn=end_bearing_kn,
        alpha=sf_tip.alpha,
        unit_skin_friction_kpa=sf_tip.unit_friction_kpa,
    )
