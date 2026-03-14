# ABOUTME: On-bottom stability for subsea pipelines per DNV-RP-F109.
# ABOUTME: Submerged weight, Morison hydrodynamic loads, lateral/vertical stability.
"""On-bottom stability — DNV-RP-F109 simplified method.

Implements:
- submerged_weight: pipe cross-section weight minus buoyancy (per unit length)
- hydrodynamic_loads: Morison drag + inertia + lift on seabed pipe
- drag_force_per_meter, lift_force_per_meter, inertia_force_per_meter: individual components
- lateral_stability_check: utilization = gamma * F_h / (mu * (W_s - F_l))
- check_lateral_stability: kN-based lateral stability wrapper
- check_vertical_stability: vertical stability (weight vs lift)
"""
from dataclasses import dataclass
import math


STANDARD = "DNV-RP-F109"
G = 9.81  # m/s^2
DEFAULT_CL = 0.9  # lift coefficient for pipe on seabed


@dataclass
class SubmergedWeightResult:
    """Submerged weight calculation result per unit length."""

    ws_n_per_m: float
    buoyancy_n_per_m: float
    dry_weight_n_per_m: float


@dataclass
class HydrodynamicLoadResult:
    """Hydrodynamic load calculation result per unit length."""

    drag_n_per_m: float
    inertia_n_per_m: float
    total_horizontal_n_per_m: float
    lift_n_per_m: float


@dataclass
class StabilityResult:
    """Lateral stability check result."""

    is_stable: bool
    utilization: float
    required_weight_n_per_m: float = 0.0
    standard: str = STANDARD


@dataclass
class VerticalStabilityResult:
    """Vertical stability check result."""

    is_stable: bool
    utilization: float
    standard: str = STANDARD


def submerged_weight(
    od_steel_m: float,
    wt_steel_m: float,
    coating_thickness_m: float,
    rho_steel: float,
    rho_coating: float,
    rho_contents: float,
    rho_seawater: float,
) -> SubmergedWeightResult:
    """Submerged weight of coated pipe per unit length.

    Calculates cross-sectional areas of steel wall, coating annulus,
    and internal contents, then subtracts displaced seawater buoyancy.

    Args:
        od_steel_m: Steel outer diameter (m).
        wt_steel_m: Steel wall thickness (m).
        coating_thickness_m: External coating thickness (m).
        rho_steel: Steel density (kg/m^3).
        rho_coating: Coating density (kg/m^3).
        rho_contents: Internal fluid density (kg/m^3).
        rho_seawater: Seawater density (kg/m^3).

    Returns:
        SubmergedWeightResult with weights per unit length (N/m).
    """
    id_steel_m = od_steel_m - 2.0 * wt_steel_m
    od_total_m = od_steel_m + 2.0 * coating_thickness_m

    # Cross-sectional areas (m^2)
    a_steel = math.pi / 4.0 * (od_steel_m**2 - id_steel_m**2)
    a_coating = math.pi / 4.0 * (od_total_m**2 - od_steel_m**2)
    a_contents = math.pi / 4.0 * id_steel_m**2
    a_displaced = math.pi / 4.0 * od_total_m**2

    # Weights per unit length (N/m)
    w_steel = a_steel * rho_steel * G
    w_coating = a_coating * rho_coating * G
    w_contents = a_contents * rho_contents * G
    dry_weight = w_steel + w_coating + w_contents

    buoyancy = a_displaced * rho_seawater * G
    ws = dry_weight - buoyancy

    return SubmergedWeightResult(
        ws_n_per_m=ws,
        buoyancy_n_per_m=buoyancy,
        dry_weight_n_per_m=dry_weight,
    )


def hydrodynamic_loads(
    od_total_m: float,
    water_velocity_m_s: float,
    water_acceleration_m_s2: float,
    rho_seawater: float,
    cd: float,
    cm: float,
    cl: float = DEFAULT_CL,
) -> HydrodynamicLoadResult:
    """Hydrodynamic forces on a seabed pipe per Morison equation.

    Drag: F_d = 0.5 * rho * Cd * D * U * |U|
    Inertia: F_i = rho * Cm * pi/4 * D^2 * a
    Lift: F_l = 0.5 * rho * Cl * D * U^2

    Args:
        od_total_m: Total outer diameter including coating (m).
        water_velocity_m_s: Combined wave + current velocity (m/s).
        water_acceleration_m_s2: Water particle acceleration (m/s^2).
        rho_seawater: Seawater density (kg/m^3).
        cd: Drag coefficient.
        cm: Inertia coefficient.
        cl: Lift coefficient (default 0.9).

    Returns:
        HydrodynamicLoadResult with forces per unit length (N/m).
    """
    u = water_velocity_m_s
    a = water_acceleration_m_s2
    d = od_total_m

    drag = 0.5 * rho_seawater * cd * d * u * abs(u)
    inertia = rho_seawater * cm * (math.pi / 4.0) * d**2 * a
    lift = 0.5 * rho_seawater * cl * d * u**2

    return HydrodynamicLoadResult(
        drag_n_per_m=drag,
        inertia_n_per_m=inertia,
        total_horizontal_n_per_m=drag + inertia,
        lift_n_per_m=lift,
    )


def lateral_stability_check(
    submerged_weight_n_per_m: float,
    horizontal_force_n_per_m: float,
    lift_force_n_per_m: float,
    friction_coefficient: float,
    safety_factor: float,
) -> StabilityResult:
    """Lateral stability check per DNV-RP-F109 simplified method.

    Utilization = gamma * F_h / (mu * (W_s - F_l))
    Pipe is stable when utilization <= 1.0.

    Args:
        submerged_weight_n_per_m: Submerged weight W_s (N/m).
        horizontal_force_n_per_m: Total horizontal force F_h (N/m).
        lift_force_n_per_m: Lift force F_l (N/m).
        friction_coefficient: Pipe-soil friction coefficient mu.
        safety_factor: Safety factor gamma.

    Returns:
        StabilityResult with utilization ratio and stability flag.

    Raises:
        ValueError: If submerged weight is zero or negative.
    """
    if submerged_weight_n_per_m <= 0.0:
        raise ValueError(
            "submerged_weight_n_per_m must be positive; "
            "pipe with non-positive submerged weight cannot be assessed"
        )

    net_downward = submerged_weight_n_per_m - lift_force_n_per_m
    resistance = friction_coefficient * net_downward

    if resistance <= 0.0:
        utilization = float("inf")
    else:
        utilization = safety_factor * horizontal_force_n_per_m / resistance

    # Required weight: gamma * F_h / mu + F_l
    required_ws = (
        safety_factor * horizontal_force_n_per_m / friction_coefficient
        + lift_force_n_per_m
    )

    return StabilityResult(
        is_stable=utilization <= 1.0,
        utilization=utilization,
        required_weight_n_per_m=required_ws,
    )


def drag_force_per_meter(
    current_velocity_ms: float,
    pipe_od_m: float,
    water_density_kg_m3: float,
    drag_coeff: float,
) -> float:
    """Drag force per unit length: F_D = 0.5 * rho * C_D * D * U^2.

    Args:
        current_velocity_ms: Flow velocity (m/s).
        pipe_od_m: Total outer diameter (m).
        water_density_kg_m3: Water density (kg/m^3).
        drag_coeff: Drag coefficient C_D.

    Returns:
        Drag force per metre (N/m).
    """
    return (
        0.5 * water_density_kg_m3 * drag_coeff
        * pipe_od_m * current_velocity_ms**2
    )


def lift_force_per_meter(
    current_velocity_ms: float,
    pipe_od_m: float,
    water_density_kg_m3: float,
    lift_coeff: float,
) -> float:
    """Lift force per unit length: F_L = 0.5 * rho * C_L * D * U^2.

    Args:
        current_velocity_ms: Flow velocity (m/s).
        pipe_od_m: Total outer diameter (m).
        water_density_kg_m3: Water density (kg/m^3).
        lift_coeff: Lift coefficient C_L.

    Returns:
        Lift force per metre (N/m).
    """
    return (
        0.5 * water_density_kg_m3 * lift_coeff
        * pipe_od_m * current_velocity_ms**2
    )


def inertia_force_per_meter(
    acceleration_ms2: float,
    pipe_od_m: float,
    water_density_kg_m3: float,
    inertia_coeff: float,
) -> float:
    """Inertia force per unit length: F_I = rho * C_M * pi/4 * D^2 * a.

    Args:
        acceleration_ms2: Water particle acceleration (m/s^2).
        pipe_od_m: Total outer diameter (m).
        water_density_kg_m3: Water density (kg/m^3).
        inertia_coeff: Inertia coefficient C_M.

    Returns:
        Inertia force per metre (N/m).
    """
    return (
        water_density_kg_m3 * inertia_coeff
        * (math.pi / 4.0) * pipe_od_m**2 * acceleration_ms2
    )


def check_lateral_stability(
    submerged_weight_per_meter_kn: float,
    horizontal_load_per_meter_kn: float,
    lift_load_per_meter_kn: float,
    friction_coeff: float,
    safety_factor: float,
) -> StabilityResult:
    """Lateral stability check with kN/m inputs.

    Convenience wrapper around lateral_stability_check that accepts
    forces in kN/m instead of N/m.

    Args:
        submerged_weight_per_meter_kn: Submerged weight W_s (kN/m).
        horizontal_load_per_meter_kn: Total horizontal load (kN/m).
        lift_load_per_meter_kn: Lift load (kN/m).
        friction_coeff: Pipe-soil friction coefficient.
        safety_factor: Safety factor gamma.

    Returns:
        StabilityResult with utilization ratio and stability flag.
    """
    return lateral_stability_check(
        submerged_weight_n_per_m=submerged_weight_per_meter_kn * 1000.0,
        horizontal_force_n_per_m=horizontal_load_per_meter_kn * 1000.0,
        lift_force_n_per_m=lift_load_per_meter_kn * 1000.0,
        friction_coefficient=friction_coeff,
        safety_factor=safety_factor,
    )


def check_vertical_stability(
    submerged_weight_per_meter_kn: float,
    lift_load_per_meter_kn: float,
    safety_factor: float,
) -> VerticalStabilityResult:
    """Vertical stability check per DNV-RP-F109.

    Pipe is vertically stable when W_s >= gamma * F_l.
    Utilization = gamma * F_l / W_s; stable when <= 1.0.

    Args:
        submerged_weight_per_meter_kn: Submerged weight W_s (kN/m).
        lift_load_per_meter_kn: Lift load F_l (kN/m).
        safety_factor: Safety factor gamma.

    Returns:
        VerticalStabilityResult with stability flag.

    Raises:
        ValueError: If submerged weight is zero or negative.
    """
    if submerged_weight_per_meter_kn <= 0.0:
        raise ValueError(
            "submerged_weight_per_meter_kn must be positive"
        )

    utilization = safety_factor * lift_load_per_meter_kn / submerged_weight_per_meter_kn

    return VerticalStabilityResult(
        is_stable=utilization <= 1.0,
        utilization=utilization,
    )
