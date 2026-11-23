"""Thin workflow runners for pipeline integrity registry entries."""
from __future__ import annotations

from dataclasses import asdict


def run_on_bottom_stability(cfg: dict) -> dict:
    """Run the existing DNV-RP-F109 simplified on-bottom stability surface."""
    from digitalmodel.subsea.on_bottom_stability import (
        check_vertical_stability,
        hydrodynamic_loads,
        lateral_stability_check,
        submerged_weight,
    )

    obs = cfg["on_bottom_stability"]
    pipe = obs["pipe"]
    metocean = obs["metocean"]
    hydrodynamic = obs["hydrodynamic"]
    soil = obs["soil"]
    safety_factor = obs.get("safety_factor", 1.1)

    weight = submerged_weight(
        od_steel_m=pipe["od_steel_m"],
        wt_steel_m=pipe["wt_steel_m"],
        coating_thickness_m=pipe["coating_thickness_m"],
        rho_steel=pipe["rho_steel"],
        rho_coating=pipe["rho_coating"],
        rho_contents=pipe["rho_contents"],
        rho_seawater=pipe["rho_seawater"],
    )
    od_total_m = pipe["od_steel_m"] + 2.0 * pipe["coating_thickness_m"]
    loads = hydrodynamic_loads(
        od_total_m=od_total_m,
        water_velocity_m_s=metocean["water_velocity_m_s"],
        water_acceleration_m_s2=metocean["water_acceleration_m_s2"],
        rho_seawater=pipe["rho_seawater"],
        cd=hydrodynamic["cd"],
        cm=hydrodynamic["cm"],
        cl=hydrodynamic["cl"],
    )
    lateral = lateral_stability_check(
        submerged_weight_n_per_m=weight.ws_n_per_m,
        horizontal_force_n_per_m=loads.total_horizontal_n_per_m,
        lift_force_n_per_m=loads.lift_n_per_m,
        friction_coefficient=soil["friction_coefficient"],
        safety_factor=safety_factor,
    )
    vertical = check_vertical_stability(
        submerged_weight_per_meter_kn=weight.ws_n_per_m / 1000.0,
        lift_load_per_meter_kn=loads.lift_n_per_m / 1000.0,
        safety_factor=safety_factor,
    )

    obs["result"] = {
        "od_total_m": od_total_m,
        "submerged_weight_N_m": weight.ws_n_per_m,
        "buoyancy_N_m": weight.buoyancy_n_per_m,
        "dry_weight_N_m": weight.dry_weight_n_per_m,
        "horizontal_load_N_m": loads.total_horizontal_n_per_m,
        "drag_load_N_m": loads.drag_n_per_m,
        "inertia_load_N_m": loads.inertia_n_per_m,
        "lift_load_N_m": loads.lift_n_per_m,
        "required_submerged_weight_N_m": lateral.required_weight_n_per_m,
        "lateral_utilization": lateral.utilization,
        "vertical_utilization": vertical.utilization,
        "is_laterally_stable": lateral.is_stable,
        "is_vertically_stable": vertical.is_stable,
        "standard": lateral.standard,
    }
    return cfg


def run_free_span(cfg: dict) -> dict:
    """Run the existing DNV-RP-F105 free-span VIV fatigue facade."""
    from digitalmodel.subsea.pipeline.free_span import FreespanVIVFatigue
    from digitalmodel.subsea.pipeline.free_span.models import (
        BoundaryConditionF105,
        EnvironmentType,
        PipeSpanInput,
    )

    free_span = cfg["free_span"]
    pipe_span = dict(free_span["pipe_span"])
    pipe_span["bc"] = BoundaryConditionF105(pipe_span["bc"])
    pipe_span["environment"] = EnvironmentType(pipe_span["environment"])
    inp = PipeSpanInput(**pipe_span)
    result = FreespanVIVFatigue(
        inp,
        submerged_weight_N_m=free_span.get("submerged_weight_N_m", 850.0),
        alpha=free_span.get("alpha", 1.0),
        KC=free_span.get("KC", 30.0),
    ).assess()

    free_span["result"] = asdict(result)
    return cfg


def run_lateral_buckling(cfg: dict) -> dict:
    """Route lateral buckling through the existing pipeline router."""
    from digitalmodel.subsea.pipeline.pipeline import Pipeline

    cfg.setdefault("calculation", {})["name"] = "lateral_buckling"
    return Pipeline().router(cfg)


def run_upheaval_buckling(cfg: dict) -> dict:
    """Route upheaval buckling through the existing pipeline router."""
    from digitalmodel.subsea.pipeline.pipeline import Pipeline

    cfg.setdefault("calculation", {})["name"] = "upheaval_buckling"
    return Pipeline().router(cfg)
