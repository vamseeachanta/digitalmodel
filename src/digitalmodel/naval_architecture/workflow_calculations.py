"""Calculation adapters for engine-routed naval architecture workflows."""

from __future__ import annotations

import math
from dataclasses import asdict
from typing import Any


def run_damage_stability(input_payload: dict[str, Any]) -> dict[str, Any]:
    from digitalmodel.naval_architecture.damage_stability import (
        angle_of_max_gz,
        check_imo_intact_stability,
        flooded_gm,
        gz_area_under_curve,
        interpolate_gz,
        lost_buoyancy_sinkage,
    )

    intact = input_payload["intact_stability"]
    flooding = input_payload["flooding"]
    angles = intact["angles_deg"]
    gz_values = intact["gz_m"]
    return {
        "area_to_30_m_rad": gz_area_under_curve(angles, gz_values, 30.0),
        "area_to_40_m_rad": gz_area_under_curve(angles, gz_values, 40.0),
        "gz_at_30_m": interpolate_gz(angles, gz_values, 30.0),
        "angle_of_max_gz_deg": angle_of_max_gz(angles, gz_values),
        "imo_intact_stability": check_imo_intact_stability(
            angles,
            gz_values,
            intact["gm_m"],
        ),
        "sinkage_m": lost_buoyancy_sinkage(
            flooding["compartment_volume_m3"],
            flooding["permeability"],
            flooding["waterplane_area_m2"],
        ),
        "flooded_gm_m": flooded_gm(
            flooding["intact_gm_m"],
            flooding["lost_waterplane_inertia_m4"],
            flooding["displacement_tonnes"],
        ),
    }


def run_platform_stability(input_payload: dict[str, Any]) -> dict[str, Any]:
    from digitalmodel.naval_architecture.floating_platform_stability import (
        check_intact_stability,
        compute_gm,
        compute_gz_curve,
        compute_wind_heel,
    )

    vessel = input_payload["vessel"]
    wind_input = input_payload["wind"]
    platform_type = input_payload["platform_type"]
    gm_m = compute_gm(
        vessel["displacement_t"],
        vessel["kg_m"],
        vessel["kb_m"],
        vessel["waterplane_area_m2"],
    )
    gz_curve = compute_gz_curve(gm_m, input_payload["gz_curve"]["heel_angles_deg"])
    wind = compute_wind_heel(
        wind_input["wind_pressure_kpa"],
        wind_input["projected_area_m2"],
        wind_input["heeling_arm_m"],
        vessel["displacement_t"],
        gm_m,
    )
    stability = asdict(check_intact_stability(platform_type, gm_m, gz_curve, wind))
    return {
        **stability,
        "platform_type": platform_type,
        "wind_heel_angle_deg": wind.heel_angle_deg,
        "wind_pressure_kpa": wind.wind_pressure_kpa,
        "heeling_arm_m": wind.heeling_arm_m,
        "gz_curve": [{"heel_deg": heel, "gz_m": gz} for heel, gz in gz_curve],
    }


def run_hull_resistance(input_payload: dict[str, Any]) -> dict[str, Any]:
    hull = input_payload["hull"]
    speed_ms = input_payload["operating"]["speed_ms"]
    coefficients = _hull_form_coefficients(hull)
    wetted_surface, resistance_n = _holtrop_resistance(hull, speed_ms, coefficients)

    from digitalmodel.naval_architecture.holtrop_mennen import effective_power
    from digitalmodel.naval_architecture.hull_form import (
        displacement_from_cb,
        froude_number,
    )

    return {
        "coefficients": coefficients,
        "displacement_tonnes": displacement_from_cb(
            coefficients["cb"], hull["lwl_m"], hull["beam_m"], hull["draft_m"]
        ),
        "froude_number": froude_number(speed_ms, hull["lwl_m"]),
        "wetted_surface_m2": wetted_surface,
        "total_resistance_N": resistance_n,
        "effective_power_W": effective_power(resistance_n, speed_ms),
    }


def _hull_form_coefficients(hull: dict[str, Any]) -> dict[str, float]:
    from digitalmodel.naval_architecture.hull_form import (
        block_coefficient,
        midship_coefficient,
        prismatic_coefficient,
        waterplane_coefficient,
    )

    cb = block_coefficient(
        hull["displacement_m3"], hull["lwl_m"], hull["beam_m"], hull["draft_m"]
    )
    cm = midship_coefficient(hull["midship_area_m2"], hull["beam_m"], hull["draft_m"])
    cwp = waterplane_coefficient(
        hull["waterplane_area_m2"], hull["lwl_m"], hull["beam_m"]
    )
    cp_from_cb_cm = prismatic_coefficient(cb, cm)
    return {
        "cb": cb,
        "cm": cm,
        "cwp": cwp,
        "cp": hull.get("cp", cp_from_cb_cm),
        "cp_from_cb_cm": cp_from_cb_cm,
    }


def _holtrop_resistance(
    hull: dict[str, Any],
    speed_ms: float,
    coefficients: dict[str, float],
) -> tuple[float, float]:
    from digitalmodel.naval_architecture.holtrop_mennen import (
        total_resistance,
        wetted_surface_holtrop,
    )

    abt = hull.get("bulbous_bow_area_m2", 0.0)
    at = hull.get("transom_area_m2", 0.0)
    wetted_surface = wetted_surface_holtrop(
        hull["lwl_m"],
        hull["beam_m"],
        hull["draft_m"],
        coefficients["cb"],
        coefficients["cm"],
        coefficients["cwp"],
        abt,
    )
    resistance_n = total_resistance(
        hull["lwl_m"],
        hull["beam_m"],
        hull["draft_m"],
        coefficients["cb"],
        coefficients["cm"],
        coefficients["cwp"],
        coefficients["cp"],
        hull["lcb_pct"],
        hull["cstern"],
        speed_ms,
        abt,
        hull["draft_m"],
        0.0,
        at,
    )
    return wetted_surface, resistance_n


def run_hull_seakeeping(input_payload: dict[str, Any]) -> dict[str, Any]:
    from digitalmodel.naval_architecture.seakeeping import (
        encounter_frequency,
        motion_sickness_incidence,
        natural_heave_period,
        natural_pitch_period,
        natural_roll_period,
        significant_motion,
        simple_heave_rao,
    )

    vessel = input_payload["vessel"]
    wave = input_payload["wave"]
    heave_period_s = natural_heave_period(
        vessel["displacement_tonnes"], vessel["waterplane_area_m2"]
    )
    natural_heave_frequency = 2.0 * math.pi / heave_period_s
    return {
        "natural_roll_period_s": natural_roll_period(vessel["beam_m"], vessel["gm_m"]),
        "natural_heave_period_s": heave_period_s,
        "natural_pitch_period_s": natural_pitch_period(
            vessel["lwl_m"], vessel["gml_m"], vessel["displacement_tonnes"]
        ),
        "encounter_frequency_rad_s": encounter_frequency(
            wave["frequency_rad_s"], vessel["speed_ms"], wave["heading_deg"]
        ),
        "simple_heave_rao": simple_heave_rao(
            wave["frequency_rad_s"], natural_heave_frequency, wave["damping_ratio"]
        ),
        "motion_sickness_incidence_pct": motion_sickness_incidence(
            wave["vertical_accel_rms_g"], wave["exposure_hours"]
        ),
        "significant_motion_m": significant_motion(
            wave["rao_values"], wave["spectrum_values"], wave["frequency_step_rad_s"]
        ),
    }
