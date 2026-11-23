"""
ABOUTME: Parachute drag force calculation for deployment analysis
ABOUTME: F = 0.5 * rho * V^2 * Cd * A * Cx (with opening shock factor)
"""

import math
from dataclasses import dataclass

LBS_TO_NEWTONS = 4.44822


@dataclass
class DragResult:
    """Result of drag force calculation."""
    force_lbs: float
    force_n: float
    speed_mph: float
    speed_fps: float
    cd: float
    cx: float
    rho: float
    chute_area_ft2: float


def mph_to_fps(speed_mph: float) -> float:
    """Convert miles per hour to feet per second."""
    return speed_mph * 5280.0 / 3600.0


def chute_area(diameter_ft: float) -> float:
    """Projected area of circular parachute canopy (ft^2)."""
    if diameter_ft <= 0:
        raise ValueError("Chute diameter must be positive")
    return math.pi * (diameter_ft / 2.0) ** 2


def calculate_drag_force(
    speed_mph: float,
    chute_diameter_ft: float,
    cd: float,
    cx: float,
    rho: float,
) -> DragResult:
    """
    Calculate parachute drag force with opening shock factor.

    Args:
        speed_mph: Vehicle speed at deployment (MPH)
        chute_diameter_ft: Parachute canopy diameter (ft)
        cd: Drag coefficient (dimensionless, typically 1.2-1.6)
        cx: Opening shock factor (dimensionless, 1.2-1.8)
        rho: Air density (slug/ft^3, sea level = 0.002378)

    Returns:
        DragResult with force in lbs and N
    """
    if speed_mph < 0:
        raise ValueError("Speed must be non-negative")

    v_fps = mph_to_fps(speed_mph)
    area = chute_area(chute_diameter_ft) if chute_diameter_ft > 0 else 0.0
    force_lbs = 0.5 * rho * v_fps**2 * cd * area * cx
    force_n = force_lbs * LBS_TO_NEWTONS

    return DragResult(
        force_lbs=force_lbs,
        force_n=force_n,
        speed_mph=speed_mph,
        speed_fps=v_fps,
        cd=cd,
        cx=cx,
        rho=rho,
        chute_area_ft2=area,
    )
