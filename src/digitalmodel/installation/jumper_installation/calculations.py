"""Shared rigid-jumper installation calculations used by the demo and workflow."""

from __future__ import annotations

import json
import logging
import math
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

import pandas as pd


logger = logging.getLogger(__name__)
REPO_ROOT = Path(__file__).resolve().parents[4]
DATA_DIR = REPO_ROOT / "examples" / "demos" / "gtm" / "data"

SEAWATER_DENSITY = 1025.0
GRAVITY = 9.80665
WATER_DEPTHS = [500, 1000, 1500, 2000, 2500, 3000]
HS_VALUES = [1.0, 1.5, 2.0, 2.5, 3.0]
REFERENCE_HS = 2.0
DAF_LIFT = 1.10
DAF_SPLASH = 1.30
RIGGING_MASS_KG = 5000.0
CS_PIPE = math.pi
CD_CYLINDER = 1.2
CA_CYLINDER = 1.0
V_LOWERING = 0.5
V_CURRENT = 0.5
SPLASH_SUBMERGED_LENGTH = 10.0
WIRE_ALLOWABLE_FACTOR = 0.85
BENDING_ALLOWABLE = 0.6
TIE_IN_TOLERANCE_MM = 50.0
CABLE_UNIT_WEIGHT_SUB = 50.0
LIFT_SPAN_FRACTION = 0.5
LIFT_SPAN_M = None
LIFT_MOMENT_COEFF = 8.0
TIEIN_UNSUPPORTED_SPAN_FRACTION = 0.28
TIEIN_UNSUPPORTED_SPAN_M = None
TIEIN_INCLUDE_SELF_WEIGHT = False
TIEIN_DEFLECTION_COEFF = 76.8


def _cfg(config: Optional[Any], attr: str, fallback: Any) -> Any:
    if config is None:
        return fallback
    return getattr(config, attr)


def load_vessel_data(path: Optional[Path] = None) -> List[Dict[str, Any]]:
    path = Path(path) if path is not None else DATA_DIR / "csv_hlv_vessels.json"
    with path.open("r", encoding="utf-8") as stream:
        data = json.load(stream)
    vessels = data["vessels"]
    print(f"  Loaded {len(vessels)} vessels from {path.name}")
    return vessels


def load_jumper_data(
    path: Optional[Path] = None,
) -> Tuple[Dict[str, Any], List[Dict[str, Any]]]:
    path = Path(path) if path is not None else DATA_DIR / "rigid_jumpers.json"
    with path.open("r", encoding="utf-8") as stream:
        data = json.load(stream)
    common = data["common_properties"]
    jumpers = data["jumpers"]
    print(f"  Loaded {len(jumpers)} jumper lengths from {path.name}")
    return common, jumpers


def hs_to_tp(hs: float, config: Optional[Any] = None) -> float:
    coeff = _cfg(config, "tp_coefficient", 4.0)
    return coeff * math.sqrt(hs)


def status_from_utils(phase_utils: Dict[str, float], config: Optional[Any] = None) -> str:
    nogo = _cfg(config, "nogo_utilisation", 1.0)
    marginal = _cfg(config, "go_marginal_threshold", 0.85)
    max_util = max(phase_utils.values()) if phase_utils else 0.0
    if max_util > nogo:
        return "NO_GO"
    if max_util >= marginal:
        return "MARGINAL"
    return "GO"


def governing_phase(phase_utils: Dict[str, float]) -> str:
    if not phase_utils:
        return "N/A"
    return max(phase_utils, key=phase_utils.get)  # type: ignore[arg-type]


def calc_phase_1_liftoff(
    jumper: Dict[str, Any],
    common: Dict[str, Any],
    vessel: Dict[str, Any],
    config: Optional[Any] = None,
) -> Dict[str, Any]:
    g = _cfg(config, "gravity_m_s2", GRAVITY)
    daf_lift = _cfg(config, "daf_liftoff", DAF_LIFT)
    rigging_kg = _cfg(config, "rigging_mass_kg", RIGGING_MASS_KG)

    mass_per_m = common["mass_per_meter"]["total_air_kg_per_m"]
    mass_total = mass_per_m * jumper["length_m"] + rigging_kg
    hook_load_te = mass_total * daf_lift / 1000.0
    crane_swl_te = vessel["crane_main"]["swl_max_te"]
    utilisation = hook_load_te / crane_swl_te

    return {
        "phase": "lift_off",
        "mass_total_kg": round(mass_total, 1),
        "hook_load_te": round(hook_load_te, 2),
        "crane_swl_te": crane_swl_te,
        "daf": daf_lift,
        "utilisation": round(utilisation, 4),
        "status": "PASS" if utilisation <= 1.0 else "FAIL",
    }


def calc_phase_2_inair_bending(
    jumper: Dict[str, Any],
    common: Dict[str, Any],
    config: Optional[Any] = None,
) -> Dict[str, Any]:
    w_air = common["weight_per_meter"]["air_n_per_m"]
    od = common["od_m"]
    i_steel = common["i_steel_m4"]
    smys = common["smys_pa"]

    daf_lift = _cfg(config, "daf_liftoff", DAF_LIFT)
    bending_allow = _cfg(config, "bending_allowable", BENDING_ALLOWABLE)
    span_fraction = _cfg(config, "lift_span_fraction", LIFT_SPAN_FRACTION)
    span_override = _cfg(config, "lift_span_m", LIFT_SPAN_M)
    moment_coeff = _cfg(config, "lift_moment_coeff", LIFT_MOMENT_COEFF)

    horizontal_span = jumper["configuration"]["horizontal_span_m"]
    lift_span = (
        float(span_override)
        if span_override is not None and span_override > 0
        else span_fraction * horizontal_span
    )
    w_eff = w_air * daf_lift
    m_max = w_eff * lift_span**2 / moment_coeff
    sigma = m_max * (od / 2.0) / i_steel
    allowable = bending_allow * smys
    utilisation = sigma / allowable

    return {
        "phase": "in_air_bending",
        "weight_per_m_n": round(w_air, 2),
        "lift_daf": daf_lift,
        "lift_span_m": round(lift_span, 3),
        "w_eff_n_per_m": round(w_eff, 2),
        "bending_moment_knm": round(m_max / 1000.0, 2),
        "bending_stress_mpa": round(sigma / 1e6, 2),
        "allowable_stress_mpa": round(allowable / 1e6, 2),
        "utilisation": round(utilisation, 4),
        "status": "PASS" if utilisation <= 1.0 else "FAIL",
    }


def calc_phase_3_splash(
    jumper: Dict[str, Any],
    common: Dict[str, Any],
    vessel: Dict[str, Any],
    hs: float,
    config: Optional[Any] = None,
) -> Dict[str, Any]:
    g = _cfg(config, "gravity_m_s2", GRAVITY)
    rho = _cfg(config, "seawater_density_kg_m3", SEAWATER_DENSITY)
    cs = _cfg(config, "cs_slamming", CS_PIPE)
    cd = _cfg(config, "cd_cylinder", CD_CYLINDER)
    v_low = _cfg(config, "v_lowering", V_LOWERING)
    daf_splash = _cfg(config, "daf_splash", DAF_SPLASH)
    rigging_kg = _cfg(config, "rigging_mass_kg", RIGGING_MASS_KG)
    splash_sub = _cfg(config, "splash_submerged_length_m", SPLASH_SUBMERGED_LENGTH)

    od = common["od_m"]
    length = jumper["length_m"]
    mass_total = common["mass_per_meter"]["total_air_kg_per_m"] * length + rigging_kg
    w_air = mass_total * g
    submerged_len = min(length, splash_sub)
    projected_area = od * submerged_len
    tp = hs_to_tp(hs, config=config)
    v_wave = math.pi * hs / tp
    v_rel = v_low + v_wave
    f_slam = 0.5 * rho * cs * projected_area * v_rel**2
    f_drag = 0.5 * rho * cd * projected_area * v_low**2
    hook_splash_te = (w_air + f_slam + f_drag) * daf_splash / (g * 1000.0)
    crane_swl_te = vessel["crane_main"]["swl_max_te"]
    utilisation = hook_splash_te / crane_swl_te

    return {
        "phase": "splash_zone",
        "slamming_force_kn": round(f_slam / 1000.0, 2),
        "drag_force_kn": round(f_drag / 1000.0, 2),
        "hook_load_splash_te": round(hook_splash_te, 2),
        "crane_swl_te": crane_swl_te,
        "daf": daf_splash,
        "v_wave_ms": round(v_wave, 3),
        "v_rel_ms": round(v_rel, 3),
        "utilisation": round(utilisation, 4),
        "status": "PASS" if utilisation <= 1.0 else "FAIL",
    }


def calc_phase_4_lowering(
    jumper: Dict[str, Any],
    common: Dict[str, Any],
    vessel: Dict[str, Any],
    depth: float,
    hs: float,
    config: Optional[Any] = None,
) -> Dict[str, Any]:
    g = _cfg(config, "gravity_m_s2", GRAVITY)
    rho = _cfg(config, "seawater_density_kg_m3", SEAWATER_DENSITY)
    ca = _cfg(config, "ca_cylinder", CA_CYLINDER)
    cable_w = _cfg(config, "cable_unit_weight_sub_n_per_m", CABLE_UNIT_WEIGHT_SUB)
    wire_factor = _cfg(config, "wire_allowable_factor", WIRE_ALLOWABLE_FACTOR)
    rigging_kg = _cfg(config, "rigging_mass_kg", RIGGING_MASS_KG)

    od = common["od_m"]
    length = jumper["length_m"]
    static_tension = common["weight_per_meter"]["submerged_n_per_m"] * length
    static_tension += cable_w * depth
    mass_total = common["mass_per_meter"]["total_air_kg_per_m"] * length + rigging_kg
    added_mass = rho * math.pi / 4.0 * od**2 * length * ca
    omega = 2.0 * math.pi / hs_to_tp(hs, config=config)
    heave_rao = vessel["motion_characteristics"]["heave_rao"]["peak_amplitude_m_per_m"]
    dynamic_load = (mass_total + added_mass) * omega**2 * heave_rao * (hs / 2.0)
    max_tension_te = (static_tension + dynamic_load) / (g * 1000.0)
    wire_mbl_te = vessel["crane_main"]["main_wire_mbl_te"]
    allowable_te = wire_factor * wire_mbl_te
    utilisation = max_tension_te / allowable_te

    return {
        "phase": "lowering",
        "static_tension_kn": round(static_tension / 1000.0, 2),
        "dynamic_load_kn": round(dynamic_load / 1000.0, 2),
        "max_tension_te": round(max_tension_te, 2),
        "wire_mbl_te": wire_mbl_te,
        "allowable_te": round(allowable_te, 2),
        "utilisation": round(utilisation, 4),
        "status": "PASS" if utilisation <= 1.0 else "FAIL",
    }


def calc_phase_5_tiein(
    jumper: Dict[str, Any],
    common: Dict[str, Any],
    config: Optional[Any] = None,
) -> Dict[str, Any]:
    rho = _cfg(config, "seawater_density_kg_m3", SEAWATER_DENSITY)
    cd = _cfg(config, "cd_cylinder", CD_CYLINDER)
    v_cur = _cfg(config, "v_current", V_CURRENT)
    tolerance_mm = _cfg(config, "tie_in_tolerance_mm", TIE_IN_TOLERANCE_MM)
    span_fraction = _cfg(
        config, "tiein_unsupported_span_fraction", TIEIN_UNSUPPORTED_SPAN_FRACTION
    )
    span_override = _cfg(config, "tiein_unsupported_span_m", TIEIN_UNSUPPORTED_SPAN_M)
    include_sw = _cfg(config, "tiein_include_self_weight", TIEIN_INCLUDE_SELF_WEIGHT)
    defl_coeff = _cfg(config, "tiein_deflection_coeff", TIEIN_DEFLECTION_COEFF)

    od = common["od_m"]
    i_steel = common["i_steel_m4"]
    horizontal_span = jumper["configuration"]["horizontal_span_m"]
    tiein_span = (
        float(span_override)
        if span_override is not None and span_override > 0
        else span_fraction * horizontal_span
    )
    w_current = 0.5 * rho * cd * od * v_cur**2
    w_sub = common["weight_per_meter"]["submerged_n_per_m"] if include_sw else 0.0
    w_res = math.hypot(w_sub, w_current)
    delta_mm = (
        w_res * tiein_span**4 / (defl_coeff * common["youngs_modulus_pa"] * i_steel)
    ) * 1000.0
    utilisation = delta_mm / tolerance_mm

    return {
        "phase": "tie_in",
        "tiein_span_m": round(tiein_span, 3),
        "current_load_n_per_m": round(w_current, 4),
        "self_weight_load_n_per_m": round(w_sub, 2),
        "resultant_load_n_per_m": round(w_res, 2),
        "deflection_mm": round(delta_mm, 4),
        "tolerance_mm": tolerance_mm,
        "utilisation": round(utilisation, 4),
        "status": "PASS" if utilisation <= 1.0 else "FAIL",
    }


def run_single_case(
    vessel: Dict[str, Any],
    jumper: Dict[str, Any],
    common: Dict[str, Any],
    depth: float,
    hs: float,
    config: Optional[Any] = None,
) -> Dict[str, Any]:
    p1 = calc_phase_1_liftoff(jumper, common, vessel, config=config)
    p2 = calc_phase_2_inair_bending(jumper, common, config=config)
    p3 = calc_phase_3_splash(jumper, common, vessel, hs, config=config)
    p4 = calc_phase_4_lowering(jumper, common, vessel, depth, hs, config=config)
    p5 = calc_phase_5_tiein(jumper, common, config=config)
    phase_utils = {
        "lift_off": p1["utilisation"],
        "in_air_bending": p2["utilisation"],
        "splash_zone": p3["utilisation"],
        "lowering": p4["utilisation"],
        "tie_in": p5["utilisation"],
    }
    max_util = max(phase_utils.values())
    return {
        "vessel": vessel["name"],
        "vessel_id": vessel["id"],
        "jumper_id": jumper["id"],
        "jumper_name": jumper["name"],
        "length_m": jumper["length_m"],
        "water_depth_m": depth,
        "hs_m": hs,
        "tp_s": round(hs_to_tp(hs, config=config), 2),
        "phases": {
            "lift_off": p1,
            "in_air_bending": p2,
            "splash_zone": p3,
            "lowering": p4,
            "tie_in": p5,
        },
        "phase_utilisations": phase_utils,
        "overall_status": status_from_utils(phase_utils, config=config),
        "governing_phase": governing_phase(phase_utils),
        "max_utilisation": round(max_util, 4),
    }


def run_parametric_sweep(
    vessels: List[Dict],
    common: Dict[str, Any],
    jumpers: List[Dict],
    config: Optional[Any] = None,
) -> Tuple[List[Dict], pd.DataFrame]:
    if config is not None:
        by_vessel = {v["name"]: v for v in vessels}
        swept_vessels = [by_vessel[name] for name in config.vessels]
        by_length = {j["length_m"]: j for j in jumpers}
        swept_jumpers = [by_length[float(length)] for length in config.lengths_m]
        depths = list(config.depths)
        hs_values = list(config.hs)
    else:
        swept_vessels = list(vessels)
        swept_jumpers = list(jumpers)
        depths = list(WATER_DEPTHS)
        hs_values = list(HS_VALUES)

    total = len(swept_vessels) * len(swept_jumpers) * len(depths) * len(hs_values)
    print(f"PARAMETRIC JUMPER INSTALLATION SWEEP: {total} cases")
    all_results: List[Dict] = []
    case_num = 0
    for vessel in swept_vessels:
        for jumper in swept_jumpers:
            for depth in depths:
                for hs in hs_values:
                    case_num += 1
                    try:
                        result = run_single_case(
                            vessel, jumper, common, depth, hs, config=config
                        )
                        all_results.append(result)
                    except Exception as exc:
                        logger.warning("Case %d failed: %s", case_num, exc)
                        all_results.append(
                            {
                                "vessel": vessel["name"],
                                "jumper_name": jumper["name"],
                                "length_m": jumper["length_m"],
                                "water_depth_m": depth,
                                "hs_m": hs,
                                "overall_status": "ERROR",
                                "max_utilisation": None,
                                "governing_phase": str(exc),
                            }
                        )

    rows = [
        {
            "Vessel": result.get("vessel", ""),
            "Jumper": result.get("jumper_name", ""),
            "Length (m)": result.get("length_m", 0),
            "Depth (m)": result.get("water_depth_m", 0),
            "Hs (m)": result.get("hs_m", 0),
            "Status": result.get("overall_status", "ERROR"),
            "Max Util": result.get("max_utilisation", None),
            "Governing Phase": result.get("governing_phase", "N/A"),
        }
        for result in all_results
    ]
    return all_results, pd.DataFrame(rows)
