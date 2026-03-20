"""
ABOUTME: Single and dual chute drag assessment with Stroud sizing logic
ABOUTME: WRK-1362 — load cases, aero lift, tire traction sensitivity
"""

import math
from dataclasses import dataclass

from digitalmodel.structural.parachute.parachute_drag import (
    DragResult,
    LBS_TO_NEWTONS,
    calculate_drag_force,
    chute_area,
    mph_to_fps,
)


@dataclass
class StroudRecommendation:
    """Stroud sizing chart recommendation."""

    vehicle_weight_lbs: float
    speed_mph: float
    config: str  # "single" or "dual"
    model: str  # e.g. "430 Std. 32", "430-28", "450"


@dataclass
class DualChuteResult:
    """Result of dual chute drag calculation."""

    chute1_force_lbs: float
    chute2_force_lbs: float
    total_force_lbs: float
    total_force_n: float
    speed_mph: float


@dataclass
class TractionAssessment:
    """Tire traction assessment at deployment speed."""

    effective_normal_lbs: float
    friction_capacity_lbs: float
    liftoff_risk: bool


@dataclass
class LoadCaseResult:
    """Result for a single load case."""

    case_id: int
    speed_mph: float
    config: str  # "single" or "dual"
    drag_force_lbs: float
    drag_force_n: float
    traction: TractionAssessment
    is_governing: bool = False


# -- Stroud sizing chart data (from WRK-5082) --
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

# Default dual chute diameter (ft) — conservative estimate pending Stroud data
DUAL_CHUTE_DEFAULT_DIAMETER_FT = 10.0

# R35 aero defaults
R35_CL = 0.35
R35_FRONTAL_AREA_FT2 = 25.0
DEFAULT_MU_DRY = 0.8


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


def calculate_dual_chute_drag(
    speed_mph: float,
    chute1_diameter_ft: float,
    chute2_diameter_ft: float,
    cd1: float,
    cd2: float,
    cx: float,
    rho: float,
) -> DualChuteResult:
    """Calculate total drag from two chutes deployed simultaneously."""
    r1 = calculate_drag_force(speed_mph, chute1_diameter_ft, cd1, cx, rho)
    r2 = calculate_drag_force(speed_mph, chute2_diameter_ft, cd2, cx, rho)
    total_lbs = r1.force_lbs + r2.force_lbs
    return DualChuteResult(
        chute1_force_lbs=r1.force_lbs,
        chute2_force_lbs=r2.force_lbs,
        total_force_lbs=total_lbs,
        total_force_n=total_lbs * LBS_TO_NEWTONS,
        speed_mph=speed_mph,
    )


def calculate_aero_lift(
    speed_mph: float,
    cl: float,
    frontal_area_ft2: float,
    rho: float,
) -> float:
    """Aerodynamic lift force (lbs) at given speed."""
    v = mph_to_fps(speed_mph)
    return 0.5 * rho * v**2 * cl * frontal_area_ft2


def calculate_tire_traction(
    vehicle_weight_lbs: float,
    aero_lift_lbs: float,
    downforce_lbs: float,
    mu: float,
) -> TractionAssessment:
    """Effective tire friction capacity accounting for aero lift/downforce."""
    effective_normal = vehicle_weight_lbs - aero_lift_lbs + downforce_lbs
    friction = mu * effective_normal
    liftoff = effective_normal < 0
    return TractionAssessment(
        effective_normal_lbs=effective_normal,
        friction_capacity_lbs=friction,
        liftoff_risk=liftoff,
    )


def assess_all_load_cases(
    vehicle_weight_lbs: float,
    single_chute_diameter_ft: float,
    cd_single: float,
    cx: float,
    rho: float,
    cl: float = R35_CL,
    frontal_area_ft2: float = R35_FRONTAL_AREA_FT2,
    downforce_lbs: float = 0.0,
    mu: float = DEFAULT_MU_DRY,
    dual_chute_diameter_ft: float = DUAL_CHUTE_DEFAULT_DIAMETER_FT,
    cd_dual: float = 1.4,
) -> list[LoadCaseResult]:
    """Assess all WRK-1362 load cases and identify governing case."""
    cases = []

    # Case 1: single chute at 200 MPH
    r1 = calculate_drag_force(200.0, single_chute_diameter_ft, cd_single, cx, rho)
    lift_200 = calculate_aero_lift(200.0, cl, frontal_area_ft2, rho)
    trac_1 = calculate_tire_traction(vehicle_weight_lbs, lift_200, downforce_lbs, mu)
    cases.append(LoadCaseResult(
        case_id=1, speed_mph=200.0, config="single",
        drag_force_lbs=r1.force_lbs, drag_force_n=r1.force_n,
        traction=trac_1,
    ))

    # Case 2: single chute at 250 MPH
    r2 = calculate_drag_force(250.0, single_chute_diameter_ft, cd_single, cx, rho)
    lift_250 = calculate_aero_lift(250.0, cl, frontal_area_ft2, rho)
    trac_2 = calculate_tire_traction(vehicle_weight_lbs, lift_250, downforce_lbs, mu)
    cases.append(LoadCaseResult(
        case_id=2, speed_mph=250.0, config="single",
        drag_force_lbs=r2.force_lbs, drag_force_n=r2.force_n,
        traction=trac_2,
    ))

    # Case 3: dual chute at 250 MPH
    r3 = calculate_dual_chute_drag(
        250.0, dual_chute_diameter_ft, dual_chute_diameter_ft,
        cd_dual, cd_dual, cx, rho,
    )
    trac_3 = calculate_tire_traction(vehicle_weight_lbs, lift_250, downforce_lbs, mu)
    cases.append(LoadCaseResult(
        case_id=3, speed_mph=250.0, config="dual",
        drag_force_lbs=r3.total_force_lbs, drag_force_n=r3.total_force_n,
        traction=trac_3,
    ))

    # Mark governing case (highest drag force)
    max_force = max(c.drag_force_lbs for c in cases)
    for c in cases:
        if c.drag_force_lbs == max_force:
            c.is_governing = True
            break

    return cases
