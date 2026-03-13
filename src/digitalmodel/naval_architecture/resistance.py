# ABOUTME: Ship resistance and powering calculations
# ABOUTME: Reynolds number, ITTC 1957 friction line, Froude scaling
"""
Ship resistance and powering calculations.

Covers USNA EN400 Chapter 7: Reynolds number, frictional resistance
(ITTC 1957), Froude scaling, and shaft horsepower.
"""

import math

KNOTS_TO_FPS = 1.6878  # 1 knot = 1.6878 ft/s


def shaft_horsepower(ehp: float, propulsive_coefficient: float) -> float:
    """Calculate shaft horsepower from EHP and propulsive coefficient.

    SHP = EHP / PC
    """
    if propulsive_coefficient <= 0:
        raise ValueError("Propulsive coefficient must be positive")
    return ehp / propulsive_coefficient


def reynolds_number(
    length_ft: float, speed_knots: float, nu: float
) -> float:
    """Calculate Reynolds number.

    Rn = V * L / nu

    Args:
        length_ft: characteristic length in feet
        speed_knots: speed in knots
        nu: kinematic viscosity in ft²/s
    """
    speed_fps = speed_knots * KNOTS_TO_FPS
    return speed_fps * length_ft / nu


def ittc_1957_cf(rn: float) -> float:
    """ITTC 1957 model-ship correlation line for friction coefficient.

    Cf = 0.075 / (log10(Rn) - 2)²
    """
    if rn <= 0:
        raise ValueError("Reynolds number must be positive")
    return 0.075 / (math.log10(rn) - 2) ** 2


def froude_speed_scaling(
    model_speed_fps: float,
    ship_length_ft: float,
    model_length_ft: float,
) -> float:
    """Scale model speed to ship speed via Froude number similarity.

    Vs = Vm * sqrt(lambda), where lambda = Ls/Lm

    Returns:
        Ship speed in knots
    """
    scale_ratio = ship_length_ft / model_length_ft
    ship_speed_fps = model_speed_fps * math.sqrt(scale_ratio)
    return ship_speed_fps / KNOTS_TO_FPS
