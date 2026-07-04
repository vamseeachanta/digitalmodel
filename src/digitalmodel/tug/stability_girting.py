# ABOUTME: Tug girting/capsize analysis — towline heeling arm vs GZ, equilibrium & self-righting
# ABOUTME: Reuses naval_architecture GZ-curve helpers; girting is the signature tug failure mode
"""Tug stability & girting analysis (issue #1195).

Girting (girding) is the signature catastrophic failure mode for tugs: a
transverse towline load applied at the towing point heels the tug, and if the
heeling arm exceeds the righting arm before the deck edge immerses, the tug
capsizes — often too rapidly for the crew to escape (e.g. MAIB *Biter*, 2023).

The towline acts like a wind load: a heeling arm curve competes with the GZ
(righting arm) curve, so this module reuses the GZ-curve helpers already in
`naval_architecture.floating_platform_stability`.

References:
    - West P&I / Britannia P&I loss-prevention bulletins on tug girting
    - MAIB report: girting and capsize of tug *Biter* (2023)
    - SNAME PNA Ch.3; IMO IS Code (intact stability)
"""

import math
from dataclasses import dataclass

from digitalmodel.naval_architecture.floating_platform_stability import (
    compute_area_under_gz,
    compute_gz_curve,
)


@dataclass
class GirtingResult:
    """Girting assessment for a given towline load."""

    towline_force_t: float  # transverse towline load, tonnes-force
    tow_point_lever_m: float  # vertical lever, tow point to lateral-resistance centre
    equilibrium_heel_deg: (
        float | None
    )  # stable heel, or None if no equilibrium (capsize)
    deck_edge_angle_deg: float
    margin_deg: float | None  # deck_edge - equilibrium (>0 safe), None if capsize
    critical_towline_t: float  # max towline load before capsize/down-flooding
    girting_safe: bool


def heeling_arm_curve(
    towline_force_t: float,
    tow_point_lever_m: float,
    displacement_t: float,
    heel_angles_deg: list[float] | None = None,
) -> list[tuple[float, float]]:
    """Towline heeling arm vs heel angle.

    HA(phi) = (F_t / W) * lever * cos(phi)

    The heeling moment from a transverse force reduces with cos(phi) as the
    tug heels; dividing by displacement gives an arm in metres, directly
    comparable to GZ.

    Args:
        towline_force_t: transverse towline load, tonnes-force.
        tow_point_lever_m: vertical distance from the towing point to the
            centre of lateral resistance (the heeling lever).
        displacement_t: tug displacement, tonnes.
        heel_angles_deg: heel angles to evaluate (default 0..90 step 5).

    Returns:
        List of (heel_deg, heeling_arm_m) pairs.
    """
    if displacement_t <= 0:
        raise ValueError("displacement_t must be positive")
    if heel_angles_deg is None:
        heel_angles_deg = list(range(0, 91, 5))

    base = (towline_force_t / displacement_t) * tow_point_lever_m
    return [(h, base * math.cos(math.radians(h))) for h in heel_angles_deg]


def _interp(curve: list[tuple[float, float]], angle: float) -> float:
    """Linear interpolation of a (heel, value) curve at `angle`."""
    if angle <= curve[0][0]:
        return curve[0][1]
    if angle >= curve[-1][0]:
        return curve[-1][1]
    for i in range(1, len(curve)):
        h0, v0 = curve[i - 1]
        h1, v1 = curve[i]
        if h0 <= angle <= h1:
            t = (angle - h0) / (h1 - h0) if h1 != h0 else 0.0
            return v0 + t * (v1 - v0)
    return curve[-1][1]


def _equilibrium_heel(
    gz_curve: list[tuple[float, float]],
    towline_force_t: float,
    tow_point_lever_m: float,
    displacement_t: float,
    deck_edge_angle_deg: float,
) -> float | None:
    """Stable equilibrium heel under a transverse towline load, or None.

    The stable equilibrium is the first heel where the righting arm rises to
    meet the heeling arm (GZ - HA crosses from negative to positive). Returns
    None if that equilibrium lies above the deck-edge / down-flooding angle, or
    never occurs (capsize). Zero towline load gives an upright equilibrium at 0.
    """
    if towline_force_t == 0:
        return 0.0
    angles = [h for h, _ in gz_curve]
    ha = heeling_arm_curve(towline_force_t, tow_point_lever_m, displacement_t, angles)
    diff_prev = gz_curve[0][1] - ha[0][1]
    for i in range(1, len(gz_curve)):
        h0 = gz_curve[i - 1][0]
        h1 = gz_curve[i][0]
        diff = gz_curve[i][1] - ha[i][1]
        if diff_prev < 0 <= diff:
            t = -diff_prev / (diff - diff_prev) if diff != diff_prev else 0.0
            cross = h0 + t * (h1 - h0)
            # the segment may straddle the deck edge; a crossing beyond it is
            # a capsize, not a usable equilibrium
            return cross if cross <= deck_edge_angle_deg else None
        diff_prev = diff
    return None


def critical_towline_force(
    gz_curve: list[tuple[float, float]],
    tow_point_lever_m: float,
    displacement_t: float,
    deck_edge_angle_deg: float,
) -> float:
    """Largest transverse towline force with a stable equilibrium below deck edge.

    Defined operationally as the greatest force for which a stable equilibrium
    still exists at or below the deck-edge / down-flooding angle, so it is
    always consistent with `assess_girting`'s verdict. This is the smaller of
    the down-flooding limit (equilibrium reaches the deck edge) and the
    loss-of-equilibrium / tangency limit (heeling arm overtakes the GZ peak);
    found by bisection over the equilibrium search.

    Args:
        gz_curve: righting-arm curve as (heel_deg, gz_m) pairs.
        tow_point_lever_m: heeling lever, m.
        displacement_t: displacement, tonnes.
        deck_edge_angle_deg: heel at which the deck edge immerses / down-flooding.

    Returns:
        Critical towline force, tonnes-force (0.0 if no stable equilibrium
        exists under any load up to the deck edge).
    """
    if tow_point_lever_m <= 0:
        raise ValueError("tow_point_lever_m must be positive")

    # A force above (W/lever) * max[GZ/cos] over (0, deck_edge] cannot have any
    # crossing below the deck edge; use it as an upper bound for the bisection.
    max_ratio = 0.0
    for heel, gz in gz_curve:
        if heel <= 0 or heel > deck_edge_angle_deg:
            continue
        cos_h = math.cos(math.radians(heel))
        if cos_h > 0:
            max_ratio = max(max_ratio, gz / cos_h)
    if max_ratio <= 0:
        return 0.0

    lo = 0.0
    hi = (displacement_t / tow_point_lever_m) * max_ratio * 1.2
    for _ in range(60):
        mid = 0.5 * (lo + hi)
        safe = (
            _equilibrium_heel(
                gz_curve,
                mid,
                tow_point_lever_m,
                displacement_t,
                deck_edge_angle_deg,
            )
            is not None
        )
        if safe:
            lo = mid
        else:
            hi = mid
    return lo


def assess_girting(
    gz_curve: list[tuple[float, float]],
    towline_force_t: float,
    tow_point_lever_m: float,
    displacement_t: float,
    deck_edge_angle_deg: float,
) -> GirtingResult:
    """Assess girting risk for a tug under a transverse towline load.

    Finds the stable equilibrium heel (first angle where GZ rises to meet the
    heeling arm) and checks it against the deck-edge / down-flooding angle.
    If the heeling arm exceeds GZ everywhere below the deck edge there is no
    stable equilibrium and the tug capsizes.

    Args:
        gz_curve: righting-arm curve (heel_deg, gz_m).
        towline_force_t: transverse towline load, tonnes-force.
        tow_point_lever_m: heeling lever, m.
        displacement_t: displacement, tonnes.
        deck_edge_angle_deg: deck-edge immersion / down-flooding angle.

    Returns:
        GirtingResult with equilibrium heel, margin, critical load, and pass/fail.
    """
    equilibrium = _equilibrium_heel(
        gz_curve,
        towline_force_t,
        tow_point_lever_m,
        displacement_t,
        deck_edge_angle_deg,
    )
    crit = critical_towline_force(
        gz_curve,
        tow_point_lever_m,
        displacement_t,
        deck_edge_angle_deg,
    )

    if equilibrium is None:
        return GirtingResult(
            towline_force_t=towline_force_t,
            tow_point_lever_m=tow_point_lever_m,
            equilibrium_heel_deg=None,
            deck_edge_angle_deg=deck_edge_angle_deg,
            margin_deg=None,
            critical_towline_t=crit,
            girting_safe=False,
        )

    margin = deck_edge_angle_deg - equilibrium
    return GirtingResult(
        towline_force_t=towline_force_t,
        tow_point_lever_m=tow_point_lever_m,
        equilibrium_heel_deg=equilibrium,
        deck_edge_angle_deg=deck_edge_angle_deg,
        margin_deg=margin,
        critical_towline_t=crit,
        girting_safe=margin > 0 and towline_force_t < crit,
    )


def angle_of_vanishing_stability(gz_curve: list[tuple[float, float]]) -> float:
    """Angle at which GZ returns to zero (range of positive stability), degrees.

    Returns the largest heel in the curve if GZ stays positive throughout.
    """
    for i in range(1, len(gz_curve)):
        h0, gz0 = gz_curve[i - 1]
        h1, gz1 = gz_curve[i]
        if gz0 > 0 >= gz1:
            t = gz0 / (gz0 - gz1) if gz0 != gz1 else 0.0
            return h0 + t * (h1 - h0)
    return gz_curve[-1][0]


def is_self_righting(
    gz_curve: list[tuple[float, float]],
    threshold_deg: float = 90.0,
) -> bool:
    """Self-righting if positive stability extends to at least `threshold_deg`."""
    return angle_of_vanishing_stability(gz_curve) >= threshold_deg


__all__ = [
    "GirtingResult",
    "heeling_arm_curve",
    "critical_towline_force",
    "assess_girting",
    "angle_of_vanishing_stability",
    "is_self_righting",
    "compute_gz_curve",
    "compute_area_under_gz",
]
