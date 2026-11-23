# ABOUTME: Anchor holding capacity calculations per DNV-RP-E302/E303 and API RP 2SK.
# ABOUTME: Implements drag anchor capacity (empirical efficiency) and suction anchor capacity (skin friction + end bearing).
"""Anchor holding capacity — DNV-RP-E302 / API RP 2SK.

Implements:
- drag_anchor_capacity: empirical holding capacity for drag anchors
- suction_anchor_capacity: skin friction + reverse end bearing for suction caissons
"""
from dataclasses import dataclass
import math


# Empirical efficiency factors: anchor_type -> soil_type -> efficiency
# Based on DNV-RP-E302 Table 3-1 / API RP 2SK simplified methodology.
DRAG_ANCHOR_EFFICIENCY: dict[str, dict[str, float]] = {
    "stevpris": {
        "soft_clay": 30.0,
        "stiff_clay": 20.0,
        "sand": 40.0,
    },
    "bruce": {
        "soft_clay": 28.0,
        "stiff_clay": 18.0,
        "sand": 38.0,
    },
    "vryhof_stevshark": {
        "soft_clay": 32.0,
        "stiff_clay": 22.0,
        "sand": 42.0,
    },
}

VALID_SOIL_TYPES = {"soft_clay", "stiff_clay", "sand"}

# Default wall thickness for suction caissons (m).
DEFAULT_WALL_THICKNESS_M = 0.04


@dataclass
class DragAnchorResult:
    """Result of drag anchor holding capacity calculation."""

    holding_capacity_kn: float
    anchor_weight_kn: float
    efficiency: float
    soil_type: str
    anchor_type: str
    standard: str = "DNV-RP-E302"


@dataclass
class SuctionAnchorResult:
    """Result of suction anchor capacity calculation."""

    total_capacity_kn: float
    skin_friction_kn: float
    reverse_end_bearing_kn: float
    standard: str = "DNV-RP-E303"


def drag_anchor_capacity(
    anchor_weight_kn: float,
    soil_type: str,
    anchor_type: str,
) -> DragAnchorResult:
    """Drag anchor holding capacity per DNV-RP-E302 / API RP 2SK.

    Uses empirical efficiency factors: H = efficiency * W_anchor.

    Args:
        anchor_weight_kn: Anchor weight in air (kN).
        soil_type: One of soft_clay, stiff_clay, sand.
        anchor_type: Anchor model (e.g. stevpris, bruce, vryhof_stevshark).

    Returns:
        DragAnchorResult with holding capacity and efficiency.

    Raises:
        ValueError: If anchor_weight_kn <= 0, soil_type unknown, or anchor_type unknown.
    """
    if anchor_weight_kn <= 0:
        raise ValueError("anchor_weight_kn must be positive")

    if soil_type not in VALID_SOIL_TYPES:
        raise ValueError(
            f"Unknown soil type: {soil_type!r}. "
            f"Valid: {sorted(VALID_SOIL_TYPES)}"
        )

    anchor_efficiencies = DRAG_ANCHOR_EFFICIENCY.get(anchor_type)
    if anchor_efficiencies is None:
        raise ValueError(
            f"Unknown anchor type: {anchor_type!r}. "
            f"Valid: {sorted(DRAG_ANCHOR_EFFICIENCY)}"
        )

    efficiency = anchor_efficiencies[soil_type]
    holding_capacity = efficiency * anchor_weight_kn

    return DragAnchorResult(
        holding_capacity_kn=holding_capacity,
        anchor_weight_kn=anchor_weight_kn,
        efficiency=efficiency,
        soil_type=soil_type,
        anchor_type=anchor_type,
    )


def suction_anchor_capacity(
    diameter_m: float,
    length_m: float,
    su_kpa: float,
    alpha: float = 0.65,
    nc: float = 9.0,
    wall_thickness_m: float = DEFAULT_WALL_THICKNESS_M,
) -> SuctionAnchorResult:
    """Suction anchor (caisson) capacity in clay per DNV-RP-E303.

    Total = skin friction (outer + inner) + reverse end bearing.
    - Outer skin friction: alpha * Su * pi * D * L
    - Inner skin friction: alpha * Su * pi * (D - 2t) * L
    - Reverse end bearing: Nc * Su * pi/4 * D^2

    Args:
        diameter_m: Outer diameter (m).
        length_m: Embedded length (m).
        su_kpa: Undrained shear strength (kPa).
        alpha: Adhesion factor (default 0.65).
        nc: Bearing capacity factor (default 9.0).
        wall_thickness_m: Caisson wall thickness (default 0.04 m).

    Returns:
        SuctionAnchorResult with total, skin friction, and reverse end bearing.

    Raises:
        ValueError: If any geometric parameter is non-positive.
    """
    if diameter_m <= 0:
        raise ValueError("diameter_m must be positive")
    if length_m <= 0:
        raise ValueError("length_m must be positive")
    if su_kpa <= 0:
        raise ValueError("su_kpa must be positive")

    inner_diameter = diameter_m - 2.0 * wall_thickness_m
    if inner_diameter <= 0:
        raise ValueError(
            "Wall thickness too large for given diameter"
        )

    outer_skin = alpha * su_kpa * math.pi * diameter_m * length_m
    inner_skin = alpha * su_kpa * math.pi * inner_diameter * length_m
    skin_friction = outer_skin + inner_skin

    reverse_end_bearing = (
        nc * su_kpa * math.pi / 4.0 * diameter_m ** 2
    )

    return SuctionAnchorResult(
        total_capacity_kn=skin_friction + reverse_end_bearing,
        skin_friction_kn=skin_friction,
        reverse_end_bearing_kn=reverse_end_bearing,
    )
