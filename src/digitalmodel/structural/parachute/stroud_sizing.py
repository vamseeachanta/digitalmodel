"""
ABOUTME: Stroud parachute sizing chart — single vs dual recommendation
ABOUTME: WRK-1362 — lookup tables from Stroud Safety catalog
"""

from dataclasses import dataclass


@dataclass
class StroudRecommendation:
    """Stroud sizing chart recommendation."""

    vehicle_weight_lbs: float
    speed_mph: float
    config: str  # "single" or "dual"
    model: str  # e.g. "430 Std. 32", "430-28", "450"


# NHRA rule: dual chutes mandatory over 200 MPH (quarter mile)
DUAL_CHUTE_SPEED_THRESHOLD_MPH = 200.0

# Stroud single chute models by vehicle weight range (lbs)
SINGLE_CHUTE_TABLE = [
    (2200, "400"),
    (2800, "410"),
    (3200, "420"),
    (4000, "430 Std. 32"),
]

# Stroud dual chute models by weight + speed range
DUAL_CHUTE_TABLE = [
    (2800, 260, "430-24 Pro Stock"),
    (3200, 280, "430-24 Pro Stock / 430-30 Pro-Mod"),
    (3800, 300, "430-28 / 430-30 Pro-Mod"),
    (4000, 320, "430-26"),
    (4000, 999, "450 / 470"),
]


def recommend_stroud_chute(
    vehicle_weight_lbs: float, speed_mph: float
) -> StroudRecommendation:
    """Recommend single vs dual chute config per Stroud sizing charts."""
    if speed_mph > DUAL_CHUTE_SPEED_THRESHOLD_MPH:
        model = _lookup_dual_model(vehicle_weight_lbs, speed_mph)
        return StroudRecommendation(
            vehicle_weight_lbs=vehicle_weight_lbs,
            speed_mph=speed_mph,
            config="dual",
            model=model,
        )

    model = _lookup_single_model(vehicle_weight_lbs)
    return StroudRecommendation(
        vehicle_weight_lbs=vehicle_weight_lbs,
        speed_mph=speed_mph,
        config="single",
        model=model,
    )


def _lookup_single_model(weight_lbs: float) -> str:
    for max_weight, model in SINGLE_CHUTE_TABLE:
        if weight_lbs <= max_weight:
            return model
    return SINGLE_CHUTE_TABLE[-1][1]


def _lookup_dual_model(weight_lbs: float, speed_mph: float) -> str:
    for max_weight, max_speed, model in DUAL_CHUTE_TABLE:
        if weight_lbs <= max_weight and speed_mph <= max_speed:
            return model
    return DUAL_CHUTE_TABLE[-1][2]
