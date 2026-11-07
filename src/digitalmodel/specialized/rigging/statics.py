# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Pure statics of identical sling legs supporting a centered vertical load.

Free-body diagram of the supported load (positive x right, positive y up)::

       T (left)       T (right)
             \\ theta|theta /
              \\    |    /
                   *
                   |
                   v W

Basis: direct resolution of the forces shown above and static equilibrium.
The two identical legs have equal tension T. The angle theta is measured from
vertical, so each upward component is V = T cos(theta). Vertical equilibrium
gives 2 V - W = 0, hence V = W/2 and T = W/(2 cos(theta)). Horizontal components
are H_left = -T sin(theta) and H_right = +T sin(theta); their sum is zero.
The constants 2 and 0 follow from counting the legs and force equilibrium.
The geometric domain is 0 <= theta < 90 degrees: vertical legs are valid,
whereas horizontal legs cannot support a positive vertical load with finite T.

The applied load is a force supplied by the caller. All returned forces use
that same unit; no mass-to-force conversion, capacity factor, catalogue rating
or design default is applied. These equations do not qualify sling strength
or establish standards compliance. Asymmetric lifts, dynamics and modal
analysis are outside this model. No workbook, solver or file access is used.
"""

from dataclasses import dataclass
import math


@dataclass(frozen=True, slots=True)
class TwoLegSlingForces:
    """Forces on the load, all in the applied vertical load's force unit.

    Attributes:
        tension_per_leg: Equal tensile force magnitude in either leg.
        vertical_force_per_leg: Equal upward force component in either leg.
        left_horizontal_force: Signed left-leg component, negative to the left.
        right_horizontal_force: Signed right-leg component, positive to the right.
    """

    tension_per_leg: float
    vertical_force_per_leg: float
    left_horizontal_force: float
    right_horizontal_force: float


def calculate_two_leg_sling_forces(
    applied_vertical_load: float, angle_from_vertical_deg: float
) -> TwoLegSlingForces:
    """Resolve identical sling legs for a centered, static vertical force.

    Args:
        applied_vertical_load: Finite positive downward force magnitude.
        angle_from_vertical_deg: Each leg's angle from vertical in degrees,
            finite and in the interval [0, 90). The included angle between
            the legs is twice this angle.

    Returns:
        Equal leg tension and vertical components with signed, opposing
        horizontal components, in the input force unit and module axes.

    Raises:
        ValueError: If the load or angle is outside the stated domain, or
            the resulting positive tension cannot be represented as a float.
    """
    if not math.isfinite(applied_vertical_load) or applied_vertical_load <= 0.0:
        raise ValueError("applied_vertical_load must be finite and positive")
    if not math.isfinite(angle_from_vertical_deg) or not (
        0.0 <= angle_from_vertical_deg < 90.0
    ):
        raise ValueError("angle_from_vertical_deg must be finite and in [0, 90)")

    angle = math.radians(angle_from_vertical_deg)
    vertical_force = applied_vertical_load / 2.0
    tension = vertical_force / math.cos(angle)
    if not math.isfinite(tension) or tension <= 0.0:
        raise ValueError("positive leg tension is not representable as a float")
    horizontal_force = tension * math.sin(angle)
    return TwoLegSlingForces(
        tension_per_leg=tension,
        vertical_force_per_leg=vertical_force,
        left_horizontal_force=-horizontal_force,
        right_horizontal_force=horizontal_force,
    )
