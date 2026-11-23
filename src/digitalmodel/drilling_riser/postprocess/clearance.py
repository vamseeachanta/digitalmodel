"""Limiting flex-joint angle for riser clearance through an opening (owner decision W506, CR-21).

A straight member of outside diameter ``d`` rotates by ``theta`` about a pivot on the opening's axis; the opening
(diameter ``D``) lies a distance ``h`` along the axis from the pivot. In the plane of the opening the member reaches
``h tan(theta) + d / (2 cos(theta))`` from the axis, so contact occurs when that equals ``D / 2``.
"""

from __future__ import annotations

import math


def reach(theta_deg: float, h: float, d: float) -> float:
    x = math.radians(theta_deg)
    return h * math.tan(x) + d / (2.0 * math.cos(x))


def limiting_angle_deg(h: float, d: float, opening: float, *, tol: float = 1e-12) -> float:
    """Angle (deg) at which the member first touches the opening; 0 when it does not fit when straight."""
    if d >= opening:
        return 0.0
    lo, hi = 0.0, 89.9
    if reach(hi, h, d) < opening / 2.0:
        return hi
    while hi - lo > tol:
        mid = 0.5 * (lo + hi)
        if reach(mid, h, d) < opening / 2.0:
            lo = mid
        else:
            hi = mid
    return 0.5 * (lo + hi)


__all__ = ["limiting_angle_deg", "reach"]
