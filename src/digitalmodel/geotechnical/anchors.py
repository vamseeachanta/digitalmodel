# ABOUTME: Anchor holding capacity calculations per DNV-RP-E302/E303 and API RP 2SK.
# ABOUTME: Implements drag anchor capacity (empirical efficiency) and suction anchor capacity (skin friction + end bearing).
"""Anchor holding capacity — DNV-RP-E302 / API RP 2SK.

Implements:
- drag_anchor_capacity: empirical holding capacity for drag anchors
- suction_anchor_capacity: skin friction + reverse end bearing for suction caissons
- check_anchor_fos: capacity-to-demand comparison with a caller-selected criterion

Ratio checks do not verify standard acceptance factors.
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


@dataclass(frozen=True)
class FoSResult:
    """Capacity-to-demand ratio and outcome for an unverified caller criterion."""

    actual_fos: float
    required_fos: float
    passes: bool


def check_anchor_fos(
    capacity_kn: float,
    demand_kn: float,
    required_fos: float,
) -> FoSResult:
    """Compare capacity / demand with an explicit caller-selected ratio.

    Args:
        capacity_kn: Finite, nonnegative holding capacity (kN).
        demand_kn: Finite, positive load demand (kN).
        required_fos: Finite, positive dimensionless criterion. The caller
            selects this value; it is not a verified standard acceptance factor.

    Returns:
        FoSResult carrying the actual and required ratios and whether the actual
        ratio is greater than or equal to the required ratio. Equality passes;
        zero capacity fails every valid positive criterion.

    Raises:
        ValueError: If an input is nonfinite or outside its stated domain.
    """
    # Zero is the mathematical bound for nonnegative/positive quantities.
    if not math.isfinite(capacity_kn) or capacity_kn < 0:
        raise ValueError("capacity_kn must be finite and nonnegative")
    if not math.isfinite(demand_kn) or demand_kn <= 0:
        raise ValueError("demand_kn must be finite and positive")
    if not math.isfinite(required_fos) or required_fos <= 0:
        raise ValueError("required_fos must be finite and positive")

    actual_fos = capacity_kn / demand_kn
    return FoSResult(
        actual_fos=actual_fos,
        required_fos=required_fos,
        passes=actual_fos >= required_fos,
    )


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
        ValueError: If weight is nonfinite or nonpositive, or a type is unknown.
    """
    if not math.isfinite(anchor_weight_kn) or anchor_weight_kn <= 0:
        raise ValueError("anchor_weight_kn must be finite and positive")

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
        alpha: Finite positive adhesion factor (default 0.65).
        nc: Finite positive bearing capacity factor (default 9.0).
        wall_thickness_m: Finite nonnegative caisson wall thickness (default
            0.04 m), less than half the outer diameter. Zero is accepted as
            a limiting geometry, not a constructible caisson design.

    Returns:
        SuctionAnchorResult with total, skin friction, and reverse end bearing.

    Raises:
        ValueError: If any scalar is nonfinite; diameter, length, strength or
            either factor is nonpositive; thickness is negative; or the inner
            diameter is nonpositive.
    """
    for name, value in (
        ("diameter_m", diameter_m),
        ("length_m", length_m),
        ("su_kpa", su_kpa),
        ("alpha", alpha),
        ("nc", nc),
    ):
        if not math.isfinite(value) or value <= 0:
            raise ValueError(f"{name} must be finite and positive")
    if not math.isfinite(wall_thickness_m) or wall_thickness_m < 0:
        raise ValueError("wall_thickness_m must be finite and nonnegative")

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
