"""
ABOUTME: Single and dual chute drag assessment with load case orchestrator
ABOUTME: WRK-1362 — aero lift, tire traction, YAML export for downstream WRKs
"""

import math
from dataclasses import asdict, dataclass

import yaml

from digitalmodel.structural.parachute.parachute_drag import (
    LBS_TO_NEWTONS,
    calculate_drag_force,
    mph_to_fps,
)
from digitalmodel.structural.parachute.stroud_sizing import (
    StroudRecommendation,
    recommend_stroud_chute,
)


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


@dataclass
class DeploymentTransientResult:
    """Reduced-order transient deployment response summary."""

    time_s: list[float]
    vehicle_speed_mph: list[float]
    chute_drag_lbs: list[float]
    car_drag_lbs: list[float]
    aero_lift_lbs: list[float]
    total_drag_lbs: list[float]
    deceleration_g: list[float]
    peak_force_lbs: float
    time_to_peak_s: float
    impulse_lbf_s: float
    cfd_recommendation: str


# Default dual chute diameter (ft) — conservative estimate pending Stroud data
DUAL_CHUTE_DEFAULT_DIAMETER_FT = 10.0

# R35 aero defaults
R35_CL = 0.35
R35_FRONTAL_AREA_FT2 = 25.0
DEFAULT_MU_DRY = 0.8


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


def simulate_deployment_transient(
    initial_speed_mph: float,
    vehicle_weight_lbs: float,
    chute_diameter_ft: float,
    cd: float,
    opening_shock_factor: float,
    rho: float,
    frontal_area_ft2: float,
    cl: float,
    duration_s: float,
    dt_s: float,
    *,
    car_cd: float = 0.28,
    inflation_time_constant_s: float = 0.35,
) -> DeploymentTransientResult:
    """Reduced-order transient deployment model.

    Uses a simple explicit time-marching update with chute drag ramped by an
    exponential inflation factor. This is intended as the minimum engineering
    time-history model before any CFD refinement.
    """
    if initial_speed_mph <= 0:
        raise ValueError("initial_speed_mph must be positive")
    if vehicle_weight_lbs <= 0:
        raise ValueError("vehicle_weight_lbs must be positive")
    if duration_s <= 0 or dt_s <= 0:
        raise ValueError("duration_s and dt_s must be positive")

    mass_slugs = vehicle_weight_lbs / 32.174
    steps = max(1, math.ceil(duration_s / dt_s))

    time_s: list[float] = []
    vehicle_speed_mph: list[float] = []
    chute_drag_lbs: list[float] = []
    car_drag_lbs: list[float] = []
    aero_lift_lbs: list[float] = []
    total_drag_lbs: list[float] = []
    deceleration_g: list[float] = []

    speed_mph = initial_speed_mph
    for step in range(steps + 1):
        t = min(round(step * dt_s, 10), duration_s)
        inflation_factor = 1.0
        if inflation_time_constant_s > 0.0:
            inflation_factor = 1.0 - math.exp(-t / inflation_time_constant_s)

        base_chute = calculate_drag_force(
            speed_mph,
            chute_diameter_ft,
            cd,
            opening_shock_factor,
            rho,
        ).force_lbs
        chute_force = base_chute * inflation_factor

        v_fps = mph_to_fps(speed_mph)
        car_force = 0.5 * rho * v_fps**2 * car_cd * frontal_area_ft2
        lift_force = calculate_aero_lift(speed_mph, cl, frontal_area_ft2, rho)
        total_force = chute_force + car_force
        accel_fps2 = total_force / mass_slugs
        accel_g = accel_fps2 / 32.174

        time_s.append(t)
        vehicle_speed_mph.append(speed_mph)
        chute_drag_lbs.append(chute_force)
        car_drag_lbs.append(car_force)
        aero_lift_lbs.append(lift_force)
        total_drag_lbs.append(total_force)
        deceleration_g.append(accel_g)

        step_dt = max(min((step + 1) * dt_s, duration_s) - t, 0.0)
        speed_fps = max(v_fps - accel_fps2 * step_dt, 0.0)
        speed_mph = speed_fps * 3600.0 / 5280.0

    peak_force = max(total_drag_lbs)
    peak_index = total_drag_lbs.index(peak_force)
    impulse = 0.0
    for t0, t1, a, b in zip(time_s, time_s[1:], total_drag_lbs, total_drag_lbs[1:]):
        impulse += 0.5 * (a + b) * (t1 - t0)

    max_lift_ratio = max(aero_lift_lbs) / vehicle_weight_lbs if vehicle_weight_lbs else 0.0
    if max_lift_ratio >= 0.1:
        cfd_recommendation = (
            "Reduced-order transient model provides the first-pass force history, but vehicle aero lift reaches a meaningful fraction of weight; "
            "run vehicle-only CFD next if lift/downforce uncertainty is decision-critical."
        )
    else:
        cfd_recommendation = (
            "Reduced-order transient model is sufficient for the first engineering pass; "
            "use CFD later only if sensitivity to car aero coefficients materially changes peak-load conclusions."
        )

    return DeploymentTransientResult(
        time_s=time_s,
        vehicle_speed_mph=vehicle_speed_mph,
        chute_drag_lbs=chute_drag_lbs,
        car_drag_lbs=car_drag_lbs,
        aero_lift_lbs=aero_lift_lbs,
        total_drag_lbs=total_drag_lbs,
        deceleration_g=deceleration_g,
        peak_force_lbs=peak_force,
        time_to_peak_s=time_s[peak_index],
        impulse_lbf_s=impulse,
        cfd_recommendation=cfd_recommendation,
    )


def export_results_yaml(
    vehicle_weight_lbs: float,
    cases: list[LoadCaseResult],
    wrk_ref: str = "WRK-1362",
) -> str:
    """Export load case results as YAML for downstream WRKs (1363/1364/1365)."""
    governing = next(c for c in cases if c.is_governing)
    load_case_dicts = []
    for c in cases:
        load_case_dicts.append({
            "case_id": c.case_id,
            "speed_mph": c.speed_mph,
            "config": c.config,
            "drag_force_lbs": round(c.drag_force_lbs, 1),
            "drag_force_n": round(c.drag_force_n, 1),
            "is_governing": c.is_governing,
        })

    data = {
        "wrk_ref": wrk_ref,
        "vehicle_weight_lbs": vehicle_weight_lbs,
        "load_cases": load_case_dicts,
        "governing_case_id": governing.case_id,
        "governing_force_lbs": round(governing.drag_force_lbs, 1),
    }
    return yaml.dump(data, default_flow_style=False, sort_keys=False)
