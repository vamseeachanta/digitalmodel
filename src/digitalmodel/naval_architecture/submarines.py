# ABOUTME: Submarine-specific calculations — MBT, density compensation, BG
# ABOUTME: Validated against USNA EN400 Chapter 10 worked examples
"""
Submarine buoyancy and stability calculations.

Covers main ballast tank capacity, water density compensation,
and submerged stability from USNA EN400 Chapter 10.
"""

G = 32.17  # ft/s²
LT_TO_LB = 2240.0


def mbt_capacity(
    surface_disp_lt: float, submerged_disp_lt: float
) -> float:
    """Main ballast tank water required to submerge (LT).

    MBT = submerged_displacement - surface_displacement
    """
    return submerged_disp_lt - surface_disp_lt


def density_compensation(
    submerged_weight_lt: float,
    rho_initial: float,
    rho_final: float,
) -> float:
    """Water to flood/pump when entering different density water (LT).

    Volume stays constant; buoyancy changes with density.
    Positive result = need to flood more (denser water).

    Args:
        submerged_weight_lt: submerged displacement in LT
        rho_initial: initial water density in slug/ft³
        rho_final: final water density in slug/ft³
    """
    weight_lb = submerged_weight_lt * LT_TO_LB
    volume_ft3 = weight_lb / (rho_initial * G)
    new_buoyancy_lb = rho_final * G * volume_ft3
    return (new_buoyancy_lb - weight_lb) / LT_TO_LB


def submerged_bg_after_weight_shift(
    displacement_lt: float,
    kb_ft: float,
    kg_ft: float,
    moved_weight_lt: float,
    old_pos_ft: float,
    new_pos_ft: float,
) -> float:
    """BG (metacentric height for submarine) after vertical weight shift.

    New KG = KG + w*(new_pos - old_pos) / W
    BG = KB - KG (positive = stable, B above G)
    """
    new_kg = kg_ft + moved_weight_lt * (new_pos_ft - old_pos_ft) / displacement_lt
    return kb_ft - new_kg
