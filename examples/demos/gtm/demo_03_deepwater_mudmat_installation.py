#!/usr/bin/env python3
# ABOUTME: GTM Demo 3 — Deepwater Mudmat Installation Analysis
# ABOUTME: Parametric analysis: 2 vessels × 6 depths × 3 mudmats × 5 Hs = 180 cases
"""
GTM Demo 3: Deepwater Mudmat Installation Analysis
====================================================

Runs parametric installation screening across:
  - 2 vessels (Large CSV 5000te, Medium CSV 2500te)
  - 6 water depths (500 – 3000 m)
  - 3 mudmat sizes (50te, 100te, 200te)
  - 5 significant wave heights (1.0 – 3.0 m)
  - 5 installation phases per case (lift-off, in-air, splash, lowering, landing)

Produces:
  - 5 interactive Plotly charts
  - Branded HTML report via GTMReportBuilder
  - JSON results file with --from-cache support

Usage:
    cd digitalmodel
    PYTHONPATH=examples/demos/gtm:src uv run python \\
        examples/demos/gtm/demo_03_deepwater_mudmat_installation.py

    # Re-generate charts from cached results:
    PYTHONPATH=examples/demos/gtm:src uv run python \\
        examples/demos/gtm/demo_03_deepwater_mudmat_installation.py --from-cache
"""

from __future__ import annotations

import argparse
import json
import logging
import math
import sys
import time
import warnings
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

# ---------------------------------------------------------------------------
# Imports — graceful handling
# ---------------------------------------------------------------------------
try:
    import numpy as np
    import pandas as pd
    import plotly.graph_objects as go
    from plotly.subplots import make_subplots
except ImportError as exc:
    print(f"[ERROR] Missing dependency: {exc}")
    print("        Install with: uv pip install numpy pandas plotly")
    sys.exit(1)

try:
    from report_template import GTMReportBuilder, COLORS, CHART_PALETTE
except ImportError:
    try:
        from examples.demos.gtm.report_template import GTMReportBuilder, COLORS, CHART_PALETTE
    except ImportError as exc:
        print(f"[ERROR] Cannot import report template: {exc}")
        print("        Ensure PYTHONPATH includes 'examples/demos/gtm' directory.")
        sys.exit(1)

# ---------------------------------------------------------------------------
# Logging
# ---------------------------------------------------------------------------
logging.basicConfig(
    level=logging.WARNING,
    format="%(levelname)s | %(name)s | %(message)s",
)
logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------
SCRIPT_DIR = Path(__file__).resolve().parent
DATA_DIR = SCRIPT_DIR / "data"
OUTPUT_DIR = SCRIPT_DIR / "output"
RESULTS_DIR = SCRIPT_DIR / "results"

# ---------------------------------------------------------------------------
# Module constants are the LEGACY DEFAULT only (used when no config is supplied, e.g. by
# legacy callers/tests). Per ADR-0005 the committed baseline yaml (inputs/demo_03_mudmat.yml)
# is the COMPLETE source of truth for ALL config; the demo threads a ResolvedDemo03Config
# through every compute/chart/report consumer, so editing the yaml changes results live.
# ---------------------------------------------------------------------------
INPUTS_DIR = SCRIPT_DIR / "inputs"
BASELINE_CONFIG_PATH = INPUTS_DIR / "demo_03_mudmat.yml"

# Canonical run_id for the committed Baseline reference run in the Results Store.
BASELINE_RUN_ID = "baseline"

# Demo identifier — the per-run report/store partition name (matches results_store_demo03.DEMO_ID).
DEMO_ID = "demo_03"


def validate_run_id(run_id: str) -> str:
    """Validate a run_id is a single safe path segment; return it unchanged if valid.

    A run_id becomes a filesystem directory name under the Results Store
    (<base_dir>/parametric/demo_03/<run_id>/) and the per-run report dir, so it must be a
    single safe path segment: no separators, no "." / ".." traversal, no empty string
    (path-traversal guard). Delegates to the store's regex so both stay in lock-step. Raises
    ValueError with a clean message on rejection.
    """
    try:
        import results_store_demo03 as _rs
    except ImportError:  # pragma: no cover — packaged import path fallback.
        from examples.demos.gtm import results_store_demo03 as _rs
    return _rs._validate_run_id(run_id)


SEAWATER_DENSITY = 1025.0  # kg/m3
GRAVITY = 9.80665  # m/s2
STEEL_DENSITY = 7850.0  # kg/m3

# Parameter matrix
WATER_DEPTHS = [500, 1000, 1500, 2000, 2500, 3000]  # metres
HS_VALUES = [1.0, 1.5, 2.0, 2.5, 3.0]  # significant wave height (m)
REFERENCE_HS = 2.0  # m — used for heatmap and vessel comparison charts

# DAFs per DNV-ST-N001 / DNV-RP-H103
DAF_LIFTOFF = 1.10
DAF_SPLASH = 1.30

# Safety / acceptance criteria
# Landing-phase soil bearing-capacity defaults (§4: q_ult = su * Nc * sc * dc, Brinch Hansen
# undrained phi=0). Used only when no config is supplied (legacy default).
SOIL_SU_KPA = 10.0  # undrained shear strength (soft clay)
SOIL_NC = 5.14  # bearing capacity factor Nc for phi=0
SOIL_APPLY_SHAPE_FACTOR = True  # sc = 1 + 0.2*(B/L)
SOIL_APPLY_DEPTH_FACTOR = True  # dc = 1 + 0.4*atan(D/B)
SOIL_FACTOR_OF_SAFETY = 2.0  # q_allow = q_ult / FS
WIRE_MBL_SF = 0.85  # max cable tension / MBL
TILT_LIMIT_DEG = 5.0  # max tilt during in-air transit

# Operating radius for overboard lifts — crane SWL is derated at larger radii.
# Typical subsea lifts use 35-40m radius (boom over the side), not the minimum
# capacity radius. This is the dominant factor in crane utilisation.
OPERATING_RADIUS_M = 40.0

# Go/No-Go acceptance bands (legacy defaults).
GO_MARGINAL_THRESHOLD = 0.85
NOGO_UTILISATION = 1.0

# JONSWAP peak period coefficient: Tp = TP_COEFFICIENT * sqrt(Hs).
TP_COEFFICIENT = 4.0

# Phase names (display). B2: these EXACT display strings (hyphens included) are what the two
# flatten transforms key off — do not retidy.
PHASE_NAMES = ["Lift-off", "In-air", "Splash zone", "Lowering", "Landing"]

# The 11 frozen TOP-LEVEL keys every per-case dict carries BEFORE serialise_results adds
# phases_flat. The drift guard in run_parametric_sweep asserts this set on the pre-serialise
# dict — NOT inside phases{} (heterogeneous per phase; that is a later subissue's concern).
FROZEN_RESULT_KEYS = (
    "vessel_id",
    "vessel_name",
    "structure_id",
    "structure_name",
    "mass_te",
    "water_depth_m",
    "hs_m",
    "overall_status",
    "max_utilisation",
    "governing_phase",
    "phases",
)


def tp_from_hs(hs: float, config: Optional[Any] = None) -> float:
    """JONSWAP peak period approximation: Tp = coeff * sqrt(Hs).

    When *config* is supplied the coefficient comes from it; otherwise the module default.
    """
    coeff = config.tp_coefficient if config is not None else TP_COEFFICIENT
    return coeff * math.sqrt(hs)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def utilisation_color(util: float) -> str:
    """Return hex color for utilisation: green / amber / red."""
    if util < 0.70:
        return COLORS["success"]   # green
    elif util <= 0.90:
        return COLORS["warning"]   # amber
    else:
        return COLORS["danger"]    # red


def go_nogo_label(status: str) -> str:
    """Map status string to display label."""
    mapping = {"GO": "GO", "MARGINAL": "MARGINAL", "NO_GO": "NO-GO"}
    return mapping.get(status, status)


def interpolate_crane_swl(curve: List[Dict], radius_m: float) -> float:
    """Linearly interpolate crane SWL (te) from capacity curve at given radius.

    If the radius is below or above the curve range, extrapolate linearly
    from the two nearest points (clamped to zero minimum).
    """
    radii = [pt["radius_m"] for pt in curve]
    swls = [pt["swl_te"] for pt in curve]

    if radius_m <= radii[0]:
        return swls[0]
    if radius_m >= radii[-1]:
        return swls[-1]

    for i in range(len(radii) - 1):
        if radii[i] <= radius_m <= radii[i + 1]:
            frac = (radius_m - radii[i]) / (radii[i + 1] - radii[i])
            return swls[i] + frac * (swls[i + 1] - swls[i])

    return swls[-1]


# ---------------------------------------------------------------------------
# Data loaders
# ---------------------------------------------------------------------------

def load_vessels() -> List[Dict[str, Any]]:
    """Load vessel data from csv_hlv_vessels.json."""
    path = DATA_DIR / "csv_hlv_vessels.json"
    with open(path, "r") as f:
        data = json.load(f)
    return data["vessels"]


def load_structures() -> List[Dict[str, Any]]:
    """Load mudmat structure data from mudmat_structures.json."""
    path = DATA_DIR / "mudmat_structures.json"
    with open(path, "r") as f:
        data = json.load(f)
    return data["structures"]


# ---------------------------------------------------------------------------
# Phase calculators
# ---------------------------------------------------------------------------

def calc_liftoff(vessel: Dict, structure: Dict, config: Optional[Any] = None) -> Dict[str, Any]:
    """Phase 1: Lift-off check.

    Hook load = mass_air * g * DAF_lift
    Check: hook_load <= crane SWL at operating radius

    When *config* is supplied the gravity, DAF, and operating radius come from it; otherwise
    the module constants are used (legacy default — byte-identical to today).
    """
    gravity = config.gravity_m_s2 if config is not None else GRAVITY
    daf_liftoff = config.daf_liftoff if config is not None else DAF_LIFTOFF
    operating_radius = config.operating_radius_m if config is not None else OPERATING_RADIUS_M

    mass_air_kg = structure["mass_properties"]["mass_air_kg"]
    crane = vessel["crane_main"]

    # Operating radius for overboard lift — SWL derated at larger radii
    crane_swl_te = interpolate_crane_swl(crane["crane_capacity_curve"], operating_radius)
    crane_swl_kn = crane_swl_te * 1000.0 * gravity / 1000.0  # te -> kg -> N -> kN

    hook_load_kn = mass_air_kg * gravity * daf_liftoff / 1000.0  # N -> kN
    utilisation = hook_load_kn / crane_swl_kn if crane_swl_kn > 0 else 999.0

    return {
        "phase": "Lift-off",
        "hook_load_kn": round(hook_load_kn, 1),
        "crane_swl_kn": round(crane_swl_kn, 1),
        "operating_radius_m": operating_radius,
        "daf": daf_liftoff,
        "utilisation": round(utilisation, 4),
        "status": "PASS" if utilisation <= 1.0 else "FAIL",
    }


def calc_in_air(vessel: Dict, structure: Dict, config: Optional[Any] = None) -> Dict[str, Any]:
    """Phase 2: In-air transit — tilt check.

    Simplified: 4-point lift with spreader beam. For properly rigged mudmats
    with symmetric CoG, tilt is negligible.

    When *config* is supplied the tilt limit comes from it; otherwise the module default.
    """
    tilt_limit_deg = config.tilt_limit_deg if config is not None else TILT_LIMIT_DEG

    cog = structure["centre_of_gravity"]
    rigging = structure["rigging"]

    # Tilt = atan(cog_offset / hook_height_above_cog)
    cog_offset = math.sqrt(cog["x_m"] ** 2 + cog["y_m"] ** 2)
    hook_height = rigging["hook_height_above_cog_m"]
    tilt_deg = math.degrees(math.atan2(cog_offset, hook_height)) if hook_height > 0 else 0.0

    utilisation = tilt_deg / tilt_limit_deg if tilt_limit_deg > 0 else 0.0

    return {
        "phase": "In-air",
        "tilt_deg": round(tilt_deg, 2),
        "tilt_limit_deg": tilt_limit_deg,
        "cog_offset_m": round(cog_offset, 3),
        "hook_height_m": hook_height,
        "utilisation": round(utilisation, 4),
        "status": "PASS" if tilt_deg <= tilt_limit_deg else "FAIL",
    }


def calc_splash_zone(
    vessel: Dict, structure: Dict, hs: float, config: Optional[Any] = None
) -> Dict[str, Any]:
    """Phase 3: Splash zone — slamming, varying buoyancy, drag.

    This is typically the CRITICAL phase for mudmat installations.

    Slamming:  F_slam = 0.5 * rho * Cs * Ap * v_rel^2
    Buoyancy:  F_var  = rho * g * A_wp * (Hs/2)
    Drag:      F_drag = 0.5 * rho * Cd * A_side * v_lowering^2
    Hook load: W_air + F_slam + F_var + F_drag (all in kN)

    When *config* is supplied the seawater density, gravity, splash DAF, and operating radius
    come from it; otherwise the module constants are used (legacy default).
    """
    rho = config.seawater_density_kg_m3 if config is not None else SEAWATER_DENSITY
    gravity = config.gravity_m_s2 if config is not None else GRAVITY
    daf_splash = config.daf_splash if config is not None else DAF_SPLASH
    operating_radius = config.operating_radius_m if config is not None else OPERATING_RADIUS_M

    mass_air_kg = structure["mass_properties"]["mass_air_kg"]
    hydro = structure["hydrodynamic_coefficients"]
    areas = structure["projected_areas"]
    crane = vessel["crane_main"]

    # Velocities
    hoist_speed_m_per_s = crane["hoist_speed_m_per_min"] / 60.0  # m/min -> m/s
    tp = tp_from_hs(hs, config=config)
    v_wave = math.pi * hs / tp if tp > 0 else 0.0  # simplified surface particle velocity
    v_rel = hoist_speed_m_per_s + v_wave

    # Forces (all in kN)
    cs = hydro["slamming_coefficient_Cs"]
    ap = areas["bottom_m2"]
    f_slam_kn = 0.5 * rho * cs * ap * v_rel ** 2 / 1000.0

    a_wp = areas["bottom_m2"]  # waterplane area ~ bottom area
    f_var_kn = rho * gravity * a_wp * (hs / 2.0) / 1000.0

    cd = hydro["drag_coefficient_Cd"]
    a_side = areas["side_long_m2"]
    f_drag_kn = 0.5 * rho * cd * a_side * hoist_speed_m_per_s ** 2 / 1000.0

    w_air_kn = mass_air_kg * gravity / 1000.0
    max_hook_splash_kn = w_air_kn + f_slam_kn + f_var_kn + f_drag_kn

    # Apply splash DAF
    design_hook_kn = max_hook_splash_kn * daf_splash

    # Crane capacity at overboard operating radius
    crane_swl_te = interpolate_crane_swl(crane["crane_capacity_curve"], operating_radius)
    crane_swl_kn = crane_swl_te * 1000.0 * gravity / 1000.0

    utilisation = design_hook_kn / crane_swl_kn if crane_swl_kn > 0 else 999.0

    return {
        "phase": "Splash zone",
        "hs_m": hs,
        "tp_s": round(tp, 2),
        "v_lowering_m_s": round(hoist_speed_m_per_s, 3),
        "v_wave_m_s": round(v_wave, 3),
        "v_rel_m_s": round(v_rel, 3),
        "f_slam_kn": round(f_slam_kn, 1),
        "f_var_buoyancy_kn": round(f_var_kn, 1),
        "f_drag_kn": round(f_drag_kn, 1),
        "w_air_kn": round(w_air_kn, 1),
        "max_hook_splash_kn": round(max_hook_splash_kn, 1),
        "daf_splash": daf_splash,
        "design_hook_kn": round(design_hook_kn, 1),
        "crane_swl_kn": round(crane_swl_kn, 1),
        "utilisation": round(utilisation, 4),
        "status": "PASS" if utilisation <= 1.0 else "FAIL",
    }


def calc_lowering(
    vessel: Dict, structure: Dict, depth: float, hs: float, config: Optional[Any] = None
) -> Dict[str, Any]:
    """Phase 4: Lowering through water column — cable tension check.

    Cable tension: T = W_sub + W_cable_sub(depth) + snap_load
    Snap load from dynamic cable response.

    When *config* is supplied the densities, gravity, and wire MBL safety factor come from it;
    otherwise the module constants are used (legacy default).

    N2: the submerged weight is RECOMPUTED here (mass_air - rho*displaced_vol); the catalog's
    precomputed ``submerged_weight_kn`` is NOT used (it disagrees at 0.01).
    """
    rho = config.seawater_density_kg_m3 if config is not None else SEAWATER_DENSITY
    gravity = config.gravity_m_s2 if config is not None else GRAVITY
    steel_density = config.steel_density_kg_m3 if config is not None else STEEL_DENSITY
    wire_mbl_sf = config.wire_mbl_sf if config is not None else WIRE_MBL_SF

    mass_air_kg = structure["mass_properties"]["mass_air_kg"]
    displaced_vol = structure["mass_properties"]["displaced_volume_m3"]
    hydro = structure["hydrodynamic_coefficients"]
    crane = vessel["crane_main"]
    motion = vessel["motion_characteristics"]

    # Submerged weight of structure (RECOMPUTE — N2: not the catalog precomputed value)
    w_sub_kn = (mass_air_kg - rho * displaced_vol) * gravity / 1000.0

    # Cable weight in water
    wire_dia_m = crane["main_wire_diameter_mm"] / 1000.0
    cable_area_m2 = math.pi / 4.0 * wire_dia_m ** 2
    cable_unit_weight_kn_per_m = cable_area_m2 * (steel_density - rho) * gravity / 1000.0
    w_cable_kn = cable_unit_weight_kn_per_m * depth

    # Snap load / dynamic cable tension
    ca = hydro["added_mass_coefficient_Ca"]
    added_mass_kg = rho * displaced_vol * ca
    total_dynamic_mass_kg = mass_air_kg + added_mass_kg

    tp = tp_from_hs(hs, config=config)
    omega = 2.0 * math.pi / tp if tp > 0 else 0.0
    heave_rao_peak = motion["heave_rao"]["peak_amplitude_m_per_m"]
    heave_amp = hs * heave_rao_peak / 2.0  # heave amplitude at crane tip

    snap_load_kn = total_dynamic_mass_kg * omega ** 2 * heave_amp / 1000.0

    # Total cable tension
    max_tension_kn = w_sub_kn + w_cable_kn + snap_load_kn

    # Wire MBL
    wire_mbl_te = crane["main_wire_mbl_te"]
    wire_mbl_kn = wire_mbl_te * 1000.0 * gravity / 1000.0
    allowable_kn = wire_mbl_sf * wire_mbl_kn

    utilisation = max_tension_kn / allowable_kn if allowable_kn > 0 else 999.0

    return {
        "phase": "Lowering",
        "depth_m": depth,
        "w_sub_kn": round(w_sub_kn, 1),
        "w_cable_kn": round(w_cable_kn, 1),
        "snap_load_kn": round(snap_load_kn, 1),
        "max_tension_kn": round(max_tension_kn, 1),
        "wire_mbl_kn": round(wire_mbl_kn, 1),
        "allowable_tension_kn": round(allowable_kn, 1),
        "heave_amp_m": round(heave_amp, 3),
        "omega_rad_s": round(omega, 4),
        "utilisation": round(utilisation, 4),
        "status": "PASS" if utilisation <= 1.0 else "FAIL",
    }


def calc_landing(structure: Dict, config: Optional[Any] = None) -> Dict[str, Any]:
    """Phase 5: Landing — derived undrained bearing CAPACITY check (Brinch Hansen, phi=0).

    Undrained bearing capacity (phi=0):
        q_ult   = su * Nc * sc * dc
        sc      = 1 + 0.2 * (B / L)          shape factor, B=min(L,W), L=max(L,W)
        dc      = 1 + 0.4 * atan(D / B)      depth factor, D = skirt_depth_m (radians)
        q_allow = q_ult / FS
    Applied bearing pressure:
        applied = W_sub / A_base             A_base = length * width
    Utilisation = applied / q_allow.

    phi=0 means there is no gamma'*D surcharge term (Nq = 1). This is a surface-footing q_ult
    with a depth factor; a skirted/embedded mudmat's reverse-end-bearing / suction is NOT
    modelled (screening scope). Uniform-su assumption.

    N2: submerged weight is RECOMPUTED (mass_air - rho*displaced_vol), not the catalog value.
    When *config* is supplied the densities, gravity, and ALL soil parameters come from it;
    otherwise the module constants are used (legacy default).
    """
    rho = config.seawater_density_kg_m3 if config is not None else SEAWATER_DENSITY
    gravity = config.gravity_m_s2 if config is not None else GRAVITY
    su_kpa = config.undrained_shear_strength_su_kpa if config is not None else SOIL_SU_KPA
    nc = config.bearing_capacity_factor_nc if config is not None else SOIL_NC
    apply_shape = config.apply_shape_factor if config is not None else SOIL_APPLY_SHAPE_FACTOR
    apply_depth = config.apply_depth_factor if config is not None else SOIL_APPLY_DEPTH_FACTOR
    fs = config.factor_of_safety if config is not None else SOIL_FACTOR_OF_SAFETY

    mass_air_kg = structure["mass_properties"]["mass_air_kg"]
    displaced_vol = structure["mass_properties"]["displaced_volume_m3"]
    geom = structure["geometry"]

    w_sub_kn = (mass_air_kg - rho * displaced_vol) * gravity / 1000.0
    length_m = geom["length_m"]
    width_m = geom["width_m"]
    a_base_m2 = length_m * width_m  # full bottom plate area

    # Foundation breadth B and length L per geometry (B = shorter side).
    b_eff = min(length_m, width_m)
    l_eff = max(length_m, width_m)
    skirt_depth_m = geom["skirt_depth_m"]

    sc = 1.0 + 0.2 * (b_eff / l_eff) if apply_shape and l_eff > 0 else 1.0
    dc = 1.0 + 0.4 * math.atan(skirt_depth_m / b_eff) if apply_depth and b_eff > 0 else 1.0

    q_ult_kpa = su_kpa * nc * sc * dc
    q_allow_kpa = q_ult_kpa / fs if fs > 0 else 999.0

    applied_kpa = w_sub_kn / a_base_m2 if a_base_m2 > 0 else 999.0
    utilisation = applied_kpa / q_allow_kpa if q_allow_kpa > 0 else 999.0

    return {
        "phase": "Landing",
        "w_sub_kn": round(w_sub_kn, 1),
        "a_base_m2": round(a_base_m2, 1),
        "applied_bearing_pressure_kpa": round(applied_kpa, 2),
        "q_ult_kpa": round(q_ult_kpa, 2),
        "q_allow_kpa": round(q_allow_kpa, 2),
        "su_kpa": su_kpa,
        "nc": nc,
        "sc": round(sc, 4),
        "dc": round(dc, 4),
        "factor_of_safety": fs,
        "utilisation": round(utilisation, 4),
        "status": "PASS" if utilisation <= 1.0 else "FAIL",
    }


# ---------------------------------------------------------------------------
# Single case runner
# ---------------------------------------------------------------------------

def run_single_case(
    vessel: Dict,
    structure: Dict,
    depth: float,
    hs: float,
    config: Optional[Any] = None,
) -> Dict[str, Any]:
    """Run all 5 installation phases for a single case.

    Returns a flat dict with overall go/no-go plus per-phase results. When *config* is
    supplied it is threaded into every phase calculator and the go/no-go bands come from it;
    otherwise the module constants are used (legacy default).
    """
    nogo_util = config.nogo_utilisation if config is not None else NOGO_UTILISATION
    go_marginal = config.go_marginal_threshold if config is not None else GO_MARGINAL_THRESHOLD

    p1 = calc_liftoff(vessel, structure, config=config)
    p2 = calc_in_air(vessel, structure, config=config)
    p3 = calc_splash_zone(vessel, structure, hs, config=config)
    p4 = calc_lowering(vessel, structure, depth, hs, config=config)
    p5 = calc_landing(structure, config=config)

    phases = [p1, p2, p3, p4, p5]
    utilisations = [p["utilisation"] for p in phases]
    max_util = max(utilisations)
    governing_idx = utilisations.index(max_util)
    governing_phase = phases[governing_idx]["phase"]

    all_pass = all(p["status"] == "PASS" for p in phases)

    if not all_pass or max_util > nogo_util:
        overall = "NO_GO"
    elif max_util >= go_marginal:
        overall = "MARGINAL"
    else:
        overall = "GO"

    return {
        "vessel_id": vessel["id"],
        "vessel_name": vessel["name"],
        "structure_id": structure["id"],
        "structure_name": structure["name"],
        "mass_te": structure["mass_properties"]["mass_air_te"],
        "water_depth_m": depth,
        "hs_m": hs,
        "overall_status": overall,
        "max_utilisation": round(max_util, 4),
        "governing_phase": governing_phase,
        "phases": {p["phase"]: p for p in phases},
    }


# ---------------------------------------------------------------------------
# Parametric sweep
# ---------------------------------------------------------------------------

def _resolve_sweep_axes(
    vessels: List[Dict],
    structures: List[Dict],
    config: Optional[Any],
) -> Tuple[List[Dict], List[int], List[Dict], List[float]]:
    """Resolve (vessel_dicts, depths, structure_dicts, hs) for the sweep.

    When *config* is a ``ResolvedDemo03Config`` its axes drive the sweep: the vessel/mudmat
    NAME labels select (and order) the catalog dicts, and the depth/hs lists come from it.
    Otherwise the full catalogs + module axis constants are used (legacy default).

    B1: depths stay ``int`` and hs stay ``float`` — never coerced here.
    """
    if config is None:
        return list(vessels), list(WATER_DEPTHS), list(structures), list(HS_VALUES)

    vessel_by_name = {v["name"]: v for v in vessels}
    vessel_dicts: List[Dict] = []
    for name in config.vessels:
        if name not in vessel_by_name:
            raise ValueError(
                f"sweep config vessel {name!r} not in vessel catalog "
                f"(known: {sorted(vessel_by_name)})"
            )
        vessel_dicts.append(vessel_by_name[name])

    structure_by_name = {s["name"]: s for s in structures}
    structure_dicts: List[Dict] = []
    for name in config.mudmats:
        if name not in structure_by_name:
            raise ValueError(
                f"sweep config mudmat {name!r} not in structure catalog "
                f"(known: {sorted(structure_by_name)})"
            )
        structure_dicts.append(structure_by_name[name])

    return vessel_dicts, list(config.depths), structure_dicts, list(config.hs)


def run_parametric_sweep(
    vessels: List[Dict],
    structures: List[Dict],
    config: Optional[Any] = None,
) -> Tuple[List[Dict], pd.DataFrame]:
    """Run the full parametric sweep across all combinations.

    Iterates the cross-product preserving today's nesting: vessel (outer) -> depth ->
    structure -> hs (inner), so the 180-case order matches the frozen golden exactly. When
    *config* (a ``ResolvedDemo03Config``) is supplied its axes + constants drive the run;
    otherwise the module constants are used.
    """
    sweep_vessels, depths, sweep_structures, hs_values = _resolve_sweep_axes(
        vessels, structures, config
    )
    total = len(sweep_vessels) * len(depths) * len(sweep_structures) * len(hs_values)

    print(f"\n{'='*60}")
    print(f"  PARAMETRIC INSTALLATION SCREENING")
    print(f"  {total} cases: {len(sweep_vessels)} vessels x {len(depths)} depths"
          f" x {len(sweep_structures)} mudmats x {len(hs_values)} sea states")
    print(f"{'='*60}\n")

    all_results: List[Dict] = []
    case_num = 0

    for vessel in sweep_vessels:
        for depth in depths:
            for structure in sweep_structures:
                for hs in hs_values:
                    case_num += 1
                    print(
                        f"  Case {case_num:3d}/{total} | {vessel['name']:<12s} | "
                        f"{depth:5.0f}m | {structure['name']:<18s} | Hs={hs:.1f}m ...",
                        end="",
                    )

                    try:
                        result = run_single_case(vessel, structure, depth, hs, config=config)
                        all_results.append(result)
                        status_tag = result["overall_status"]
                        util = result["max_utilisation"]
                        print(f" util={util:.3f} [{status_tag}]")
                    except Exception as exc:
                        logger.warning("Case failed: %s", exc)
                        print(f" [ERROR: {exc}]")
                        # Normalized ERROR record — SAME 11 FROZEN keys (max_util None,
                        # governing "N/A", phases {}). 0 of the 180 baseline cases hit this.
                        all_results.append({
                            "vessel_id": vessel["id"],
                            "vessel_name": vessel["name"],
                            "structure_id": structure["id"],
                            "structure_name": structure["name"],
                            "mass_te": structure["mass_properties"]["mass_air_te"],
                            "water_depth_m": depth,
                            "hs_m": hs,
                            "overall_status": "ERROR",
                            "max_utilisation": None,
                            "governing_phase": "N/A",
                            "phases": {},
                        })

    # FROZEN_RESULT_KEYS drift guard: every PRE-serialise case must carry exactly the 11
    # top-level keys (NOT asserted inside phases{} — heterogeneous per phase).
    for rec in all_results:
        if set(rec.keys()) != set(FROZEN_RESULT_KEYS):
            raise AssertionError(
                "result record key set drift (FROZEN_RESULT_KEYS): "
                f"{sorted(rec.keys())} != {sorted(FROZEN_RESULT_KEYS)}"
            )

    # Build DataFrame with flat columns for charting
    rows = []
    for r in all_results:
        row = {
            "vessel_id": r["vessel_id"],
            "vessel_name": r["vessel_name"],
            "structure_id": r["structure_id"],
            "structure_name": r["structure_name"],
            "mass_te": r["mass_te"],
            "water_depth_m": r["water_depth_m"],
            "hs_m": r["hs_m"],
            "overall_status": r["overall_status"],
            "max_utilisation": r["max_utilisation"],
            "governing_phase": r["governing_phase"],
        }
        # Flatten phase utilisations
        for pname in PHASE_NAMES:
            phase_data = r.get("phases", {}).get(pname, {})
            row[f"util_{pname.lower().replace(' ', '_').replace('-', '_')}"] = phase_data.get(
                "utilisation"
            )
        rows.append(row)

    df = pd.DataFrame(rows)
    print(f"\n  Sweep complete: {len(all_results)} results collected")
    return all_results, df


# ---------------------------------------------------------------------------
# Chart 1: Go/No-Go Heatmap (HERO)
# ---------------------------------------------------------------------------

def build_chart_1_go_nogo_heatmap(
    results_df: pd.DataFrame, config: Optional[Any] = None
) -> go.Figure:
    """Go/No-Go heatmap at reference Hs — side-by-side for two vessels."""
    print("\n[Chart 1] Building Go/No-Go Heatmap...")
    reference_hs = config.reference_hs if config is not None else REFERENCE_HS

    ref_df = results_df[results_df["hs_m"] == reference_hs].copy()
    vessel_names = sorted(ref_df["vessel_name"].unique())
    structure_names = sorted(ref_df["structure_name"].unique())
    depths = sorted(ref_df["water_depth_m"].unique())

    # Color mapping
    status_to_num = {"GO": 0, "MARGINAL": 1, "NO_GO": 2, "ERROR": 2}
    status_colors = [COLORS["success"], COLORS["warning"], COLORS["danger"]]

    from plotly.colors import make_colorscale
    colorscale = [
        [0.0, COLORS["success"]],
        [0.33, COLORS["success"]],
        [0.34, COLORS["warning"]],
        [0.66, COLORS["warning"]],
        [0.67, COLORS["danger"]],
        [1.0, COLORS["danger"]],
    ]

    fig = make_subplots(
        rows=1,
        cols=len(vessel_names),
        subplot_titles=[f"{v}" for v in vessel_names],
        horizontal_spacing=0.08,
    )

    for col_idx, vname in enumerate(vessel_names, 1):
        vdf = ref_df[ref_df["vessel_name"] == vname]

        z_matrix = []
        annotation_text = []

        for sname in structure_names:
            z_row = []
            ann_row = []
            for d in depths:
                match = vdf[(vdf["structure_name"] == sname) & (vdf["water_depth_m"] == d)]
                if len(match) > 0:
                    row = match.iloc[0]
                    status = row["overall_status"]
                    util = row["max_utilisation"]
                    gov = row["governing_phase"]
                    z_row.append(status_to_num.get(status, 2))
                    if util is not None:
                        ann_row.append(f"{go_nogo_label(status)}<br>{gov}<br>{util:.0%}")
                    else:
                        ann_row.append("ERROR")
                else:
                    z_row.append(2)
                    ann_row.append("N/A")
            z_matrix.append(z_row)
            annotation_text.append(ann_row)

        fig.add_trace(
            go.Heatmap(
                z=z_matrix,
                x=[f"{d}m" for d in depths],
                y=structure_names,
                colorscale=colorscale,
                zmin=0,
                zmax=2,
                showscale=False,
                text=annotation_text,
                texttemplate="%{text}",
                textfont=dict(size=10),
                hovertemplate=(
                    "<b>%{y}</b> at %{x}<br>"
                    "%{text}<extra></extra>"
                ),
            ),
            row=1,
            col=col_idx,
        )

    fig.update_layout(
        title=dict(
            text=f"Installation Go/No-Go Matrix (Hs = {reference_hs} m)",
            font=dict(size=18),
        ),
        height=400,
        width=1000,
    )
    fig.update_xaxes(title_text="Water Depth", row=1, col=1)
    fig.update_xaxes(title_text="Water Depth", row=1, col=2)

    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 2: Crane Utilisation vs Water Depth
# ---------------------------------------------------------------------------

def build_chart_2_crane_utilisation(
    results_df: pd.DataFrame, config: Optional[Any] = None
) -> go.Figure:
    """Crane utilisation vs water depth, parametric by mudmat and vessel."""
    print("\n[Chart 2] Building Crane Utilisation vs Water Depth...")
    reference_hs = config.reference_hs if config is not None else REFERENCE_HS

    ref_df = results_df[results_df["hs_m"] == reference_hs].copy()
    vessel_names = sorted(ref_df["vessel_name"].unique())
    structure_names = sorted(ref_df["structure_name"].unique())

    fig = go.Figure()
    color_idx = 0

    for vname in vessel_names:
        dash = "solid" if "Large" in vname else "dash"
        for sname in structure_names:
            subset = ref_df[
                (ref_df["vessel_name"] == vname) & (ref_df["structure_name"] == sname)
            ].sort_values("water_depth_m")

            fig.add_trace(
                go.Scatter(
                    x=subset["water_depth_m"],
                    y=subset["max_utilisation"] * 100,
                    mode="lines+markers",
                    name=f"{vname} / {sname}",
                    line=dict(
                        color=CHART_PALETTE[color_idx % len(CHART_PALETTE)],
                        dash=dash,
                        width=2,
                    ),
                    marker=dict(size=6),
                    hovertemplate=(
                        f"<b>{vname}</b> — {sname}<br>"
                        "Depth: %{x}m<br>"
                        "Utilisation: %{y:.1f}%<extra></extra>"
                    ),
                )
            )
            color_idx += 1

    # 100% limit line
    fig.add_hline(
        y=100,
        line_dash="dash",
        line_color=COLORS["danger"],
        annotation_text="100% — Capacity Limit",
        annotation_position="top left",
    )

    fig.update_layout(
        title=dict(
            text=f"Crane Utilisation vs Water Depth (Hs = {reference_hs} m)",
            font=dict(size=18),
        ),
        xaxis_title="Water Depth (m)",
        yaxis_title="Max Utilisation (%)",
        height=550,
        width=1000,
        legend=dict(font=dict(size=10)),
    )

    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 3: DAF vs Water Depth
# ---------------------------------------------------------------------------

def build_chart_3_daf_vs_depth(
    results_df: pd.DataFrame, config: Optional[Any] = None
) -> go.Figure:
    """Effective DAF (splash zone + lowering) vs water depth by mudmat size."""
    print("\n[Chart 3] Building DAF vs Water Depth...")
    reference_hs = config.reference_hs if config is not None else REFERENCE_HS

    ref_df = results_df[results_df["hs_m"] == reference_hs].copy()
    structure_names = sorted(ref_df["structure_name"].unique())

    # Use Large CSV for DAF illustration
    vessel_df = ref_df[ref_df["vessel_name"].str.contains("Large")]

    fig = go.Figure()
    color_idx = 0

    for sname in structure_names:
        subset = vessel_df[vessel_df["structure_name"] == sname].sort_values("water_depth_m")

        # Splash zone effective DAF = design_hook / W_air (from phase utilisation)
        splash_utils = subset["util_splash_zone"].values
        lowering_utils = subset["util_lowering"].values
        depths = subset["water_depth_m"].values

        fig.add_trace(
            go.Scatter(
                x=depths,
                y=splash_utils * 100 if splash_utils is not None else [],
                mode="lines+markers",
                name=f"{sname} — Splash zone",
                line=dict(
                    color=CHART_PALETTE[color_idx % len(CHART_PALETTE)],
                    dash="solid",
                    width=2,
                ),
                marker=dict(size=6),
            )
        )
        fig.add_trace(
            go.Scatter(
                x=depths,
                y=lowering_utils * 100 if lowering_utils is not None else [],
                mode="lines+markers",
                name=f"{sname} — Lowering",
                line=dict(
                    color=CHART_PALETTE[color_idx % len(CHART_PALETTE)],
                    dash="dot",
                    width=2,
                ),
                marker=dict(size=6, symbol="diamond"),
            )
        )
        color_idx += 1

    fig.add_hline(
        y=100,
        line_dash="dash",
        line_color=COLORS["danger"],
        annotation_text="100% — Limit",
        annotation_position="top left",
    )

    fig.update_layout(
        title=dict(
            text=f"Phase Utilisation vs Water Depth — Splash & Lowering (Large CSV, Hs = {reference_hs} m)",
            font=dict(size=16),
        ),
        xaxis_title="Water Depth (m)",
        yaxis_title="Phase Utilisation (%)",
        height=550,
        width=1000,
        legend=dict(font=dict(size=10)),
    )

    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 4: Max Hs Limit vs Structure Weight
# ---------------------------------------------------------------------------

def build_chart_4_hs_limit(
    results_df: pd.DataFrame,
    vessels: List[Dict],
    structures: List[Dict],
    config: Optional[Any] = None,
) -> go.Figure:
    """Max allowable Hs for each structure/vessel combination — operability envelope."""
    print("\n[Chart 4] Building Max Hs Limit vs Structure Weight...")
    hs_values = config.hs if config is not None else HS_VALUES

    fig = go.Figure()
    color_idx = 0

    for vessel in vessels:
        vname = vessel["name"]
        masses = []
        max_hs_values = []

        for structure in structures:
            sname = structure["name"]
            mass_te = structure["mass_properties"]["mass_air_te"]

            # Find max Hs where overall_status != NO_GO across all depths
            # Use the most restrictive depth (deepest)
            vdf = results_df[
                (results_df["vessel_name"] == vname)
                & (results_df["structure_name"] == sname)
            ]

            # For each Hs, check if ANY depth gives NO_GO
            max_hs = 0.0
            for hs in sorted(hs_values):
                hs_df = vdf[vdf["hs_m"] == hs]
                # Pass if at least half the depths are GO or MARGINAL
                # But for operability, require ALL depths to pass
                if all(hs_df["overall_status"].isin(["GO", "MARGINAL"])):
                    max_hs = hs
                else:
                    break

            masses.append(mass_te)
            max_hs_values.append(max_hs)

        fig.add_trace(
            go.Scatter(
                x=masses,
                y=max_hs_values,
                mode="lines+markers",
                name=vname,
                line=dict(
                    color=CHART_PALETTE[color_idx % len(CHART_PALETTE)],
                    width=3,
                ),
                marker=dict(size=10),
                hovertemplate=(
                    f"<b>{vname}</b><br>"
                    "Structure weight: %{x} te<br>"
                    "Max Hs (all depths pass): %{y:.1f} m<extra></extra>"
                ),
            )
        )
        color_idx += 1

    fig.update_layout(
        title=dict(
            text="Operability Envelope — Max Allowable Hs vs Structure Weight",
            font=dict(size=18),
        ),
        xaxis_title="Structure Weight in Air (te)",
        yaxis_title="Max Allowable Hs (m)",
        height=500,
        width=900,
        yaxis=dict(range=[0, max(hs_values) + 0.5]),
    )

    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 5: Vessel Head-to-Head Comparison
# ---------------------------------------------------------------------------

def build_chart_5_vessel_comparison(
    results_df: pd.DataFrame, config: Optional[Any] = None
) -> go.Figure:
    """Grouped bar: structures installable per depth per vessel at reference Hs."""
    print("\n[Chart 5] Building Vessel Head-to-Head Comparison...")
    reference_hs = config.reference_hs if config is not None else REFERENCE_HS

    ref_df = results_df[results_df["hs_m"] == reference_hs].copy()
    vessel_names = sorted(ref_df["vessel_name"].unique())
    depths = sorted(ref_df["water_depth_m"].unique())

    fig = go.Figure()
    color_idx = 0

    for vname in vessel_names:
        installable_counts = []
        for d in depths:
            subset = ref_df[
                (ref_df["vessel_name"] == vname) & (ref_df["water_depth_m"] == d)
            ]
            count = len(subset[subset["overall_status"].isin(["GO", "MARGINAL"])])
            installable_counts.append(count)

        fig.add_trace(
            go.Bar(
                x=[f"{d}m" for d in depths],
                y=installable_counts,
                name=vname,
                marker_color=CHART_PALETTE[color_idx % len(CHART_PALETTE)],
                text=installable_counts,
                textposition="auto",
                hovertemplate=(
                    f"<b>{vname}</b><br>"
                    "Depth: %{x}<br>"
                    "Structures installable: %{y}/3<extra></extra>"
                ),
            )
        )
        color_idx += 1

    total_structures = len(results_df["structure_name"].unique())
    fig.update_layout(
        title=dict(
            text=f"Vessel Comparison — Structures Installable per Depth (Hs = {reference_hs} m)",
            font=dict(size=18),
        ),
        xaxis_title="Water Depth",
        yaxis_title=f"Structures Installable (out of {total_structures})",
        yaxis=dict(range=[0, total_structures + 0.5], dtick=1),
        barmode="group",
        height=500,
        width=900,
    )

    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Summary table
# ---------------------------------------------------------------------------

def build_summary_table(
    results_df: pd.DataFrame, config: Optional[Any] = None
) -> pd.DataFrame:
    """Build summary table: vessel x structure x reference Hs at all depths."""
    reference_hs = config.reference_hs if config is not None else REFERENCE_HS
    ref_df = results_df[results_df["hs_m"] == reference_hs].copy()

    rows = []
    for _, row in ref_df.iterrows():
        util = row["max_utilisation"]
        status = row["overall_status"]
        rows.append({
            "Vessel": row["vessel_name"],
            "Structure": row["structure_name"],
            "Depth (m)": int(row["water_depth_m"]),
            "Max Util.": f"{util:.1%}" if util is not None else "N/A",
            "Governing Phase": row["governing_phase"],
            "Status": go_nogo_label(status),
        })

    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# Build HTML report
# ---------------------------------------------------------------------------

def build_report(
    fig1: go.Figure,
    fig2: go.Figure,
    fig3: go.Figure,
    fig4: go.Figure,
    fig5: go.Figure,
    summary_df: pd.DataFrame,
    all_results: List[Dict],
    total_cases: int,
    config: Optional[Any] = None,
    output_dir: Optional[Path] = None,
    output_path: Optional[Path] = None,
) -> str:
    """Build the branded HTML report.

    When *config* is supplied the methodology prose, the assumption strings, the reference-Hs
    chart subtitles, and the go/no-go bands are all driven by it (ADR-0005). Otherwise the
    module constants are used (legacy default — byte-identical to today).
    """
    print("\n[Report] Building HTML report...")

    # Resolve the config-driven values used throughout the report narrative.
    reference_hs = config.reference_hs if config is not None else REFERENCE_HS
    daf_liftoff = config.daf_liftoff if config is not None else DAF_LIFTOFF
    daf_splash = config.daf_splash if config is not None else DAF_SPLASH
    tilt_limit_deg = config.tilt_limit_deg if config is not None else TILT_LIMIT_DEG
    operating_radius_m = config.operating_radius_m if config is not None else OPERATING_RADIUS_M
    wire_mbl_sf = config.wire_mbl_sf if config is not None else WIRE_MBL_SF
    tp_coefficient = config.tp_coefficient if config is not None else TP_COEFFICIENT
    go_marginal = config.go_marginal_threshold if config is not None else GO_MARGINAL_THRESHOLD
    nogo_util = config.nogo_utilisation if config is not None else NOGO_UTILISATION
    seawater_density = config.seawater_density_kg_m3 if config is not None else SEAWATER_DENSITY
    soil_su_kpa = (
        config.undrained_shear_strength_su_kpa if config is not None else SOIL_SU_KPA
    )
    soil_nc = config.bearing_capacity_factor_nc if config is not None else SOIL_NC
    soil_fs = config.factor_of_safety if config is not None else SOIL_FACTOR_OF_SAFETY
    # Axis cardinalities for the subtitle (derived from the produced results).
    n_vessels = len({r["vessel_name"] for r in all_results}) if all_results else 2
    n_structures = len({r["structure_name"] for r in all_results}) if all_results else 3
    n_depths = len({r["water_depth_m"] for r in all_results}) if all_results else 6
    n_hs = len({r["hs_m"] for r in all_results}) if all_results else 5

    report = GTMReportBuilder(
        title="Deepwater Mudmat Installation Analysis",
        subtitle=(
            f"{total_cases} parametric cases: {n_vessels} vessels x {n_depths} depths "
            f"x {n_structures} mudmats x {n_hs} sea states"
        ),
        demo_id="demo_03",
        case_count=total_cases,
        code_refs=[
            "DNV-RP-H103 (2011) — Modelling and Analysis of Marine Operations",
            "DNV-ST-N001 (2021) — Marine Operations and Marine Warranty",
            "DNV-RP-C205 (2010) — Environmental Conditions and Environmental Loads",
        ],
    )

    # B3a: the crane SWL is evaluated at the DERATED operating radius (operating_radius_m m),
    # NOT the maximum-capacity (most favourable) radius — describe the actual derating.
    # B3b (§4): Phase 5 checks the applied bearing pressure against a DERIVED undrained bearing
    # CAPACITY q_ult = su * Nc * sc * dc (Brinch Hansen, phi=0), divided by the FS.
    methodology_html = f"""
    <p>This analysis screens deepwater mudmat installations across a parametric matrix of
    vessels, water depths, structure sizes, and sea states. Each case evaluates five
    installation phases per DNV-RP-H103 and DNV-ST-N001 methodology.</p>

    <h3>Phase 1: Lift-off</h3>
    <p>Hook load = mass<sub>air</sub> &times; g &times; DAF<sub>lift</sub> ({daf_liftoff}).
    Checked against crane SWL at the {operating_radius_m:g} m overboard operating radius
    (a derated radius, not the most favourable maximum-capacity radius), via interpolation
    of the crane capacity curve.</p>

    <h3>Phase 2: In-air Transit</h3>
    <p>Tilt check for 4-point lift with spreader beam. CoG offset / hook height gives
    tilt angle, checked against the {tilt_limit_deg:g}&deg; limit. Symmetric mudmats always pass.</p>

    <h3>Phase 3: Splash Zone (Critical)</h3>
    <p>Slamming force: F<sub>slam</sub> = 0.5 &times; &rho; &times; C<sub>s</sub>
    &times; A<sub>p</sub> &times; v<sub>rel</sub><sup>2</sup>, where
    v<sub>rel</sub> = v<sub>lowering</sub> + v<sub>wave</sub>.
    Additional varying buoyancy and drag forces. DAF<sub>splash</sub> = {daf_splash}.
    This phase typically governs for large mudmats in higher sea states.</p>

    <h3>Phase 4: Lowering through Water Column</h3>
    <p>Cable tension = W<sub>sub</sub> + W<sub>cable</sub>(depth) + snap load.
    Snap load from dynamic cable response using vessel heave RAO. Checked against
    {wire_mbl_sf:.0%} of wire MBL. Becomes critical at extreme depths due to cable self-weight.</p>

    <h3>Phase 5: Landing</h3>
    <p>Applied bearing pressure = W<sub>sub</sub> / A<sub>base</sub>, checked against a derived
    undrained bearing capacity q<sub>ult</sub> = s<sub>u</sub> &times; N<sub>c</sub> &times;
    s<sub>c</sub> &times; d<sub>c</sub> per Brinch Hansen (undrained, &phi; = 0), with
    s<sub>u</sub> = {soil_su_kpa:g} kPa (soft clay), N<sub>c</sub> = {soil_nc:g}, shape factor
    s<sub>c</sub> = 1 + 0.2&middot;(B/L) and depth factor d<sub>c</sub> = 1 + 0.4&middot;atan(D/B)
    (B = shorter base dimension, D = skirt depth). The allowable capacity is q<sub>allow</sub>
    = q<sub>ult</sub> / FS with FS = {soil_fs:g}, and utilisation = applied / q<sub>allow</sub>
    (cites #264). &phi; = 0 gives no &gamma;&prime;&middot;D surcharge term (N<sub>q</sub> = 1).
    <em>Caveat:</em> this assumes a uniform s<sub>u</sub> and is a surface-footing q<sub>ult</sub>
    with a depth factor; a skirted/embedded mudmat's reverse-end-bearing / suction resistance is
    not modelled (screening scope).</p>

    <h3>Go/No-Go Criteria</h3>
    <ul>
        <li><strong>GO:</strong> All phases pass AND max utilisation &lt; {go_marginal:g}</li>
        <li><strong>MARGINAL:</strong> All phases pass AND max utilisation in [{go_marginal:g}, {nogo_util:.2f}]</li>
        <li><strong>NO-GO:</strong> Any phase fails (utilisation &gt; {nogo_util:.2f})</li>
    </ul>
    """
    report.add_methodology(methodology_html)

    report.add_chart(
        "go_nogo",
        fig1,
        title="Chart 1: Go/No-Go Installation Matrix",
        subtitle=f"Side-by-side vessel comparison at Hs = {reference_hs} m reference sea state.",
    )

    report.add_chart(
        "crane_utilisation",
        fig2,
        title="Chart 2: Crane Utilisation vs Water Depth",
        subtitle="Max utilisation across all phases. Solid = Large CSV, dashed = Medium CSV.",
    )

    report.add_chart(
        "daf_vs_depth",
        fig3,
        title="Chart 3: Phase Utilisation vs Water Depth — Splash & Lowering",
        subtitle="Shows how splash zone dominates shallow cases while lowering governs at depth.",
    )

    report.add_chart(
        "hs_limit",
        fig4,
        title="Chart 4: Operability Envelope — Max Hs vs Structure Weight",
        subtitle="Maximum sea state where all depths pass. Defines weather window requirements.",
    )

    report.add_chart(
        "vessel_comparison",
        fig5,
        title="Chart 5: Vessel Head-to-Head — Structures Installable per Depth",
        subtitle=f"Number of mudmat sizes (out of {n_structures}) each vessel can install at Hs = {reference_hs} m.",
    )

    report.add_table(
        "Installation Screening Summary",
        summary_df,
        subtitle=f"All cases at Hs = {reference_hs} m reference sea state",
        status_col="Status",
    )

    report.add_live_mode_teaser(
        analysis_type="the installation screening"
    )

    report.add_assumptions([
        # B3a: HONEST — the crane SWL is taken at the DERATED operating radius, not the
        # most-favourable maximum-capacity radius.
        f"Crane SWL evaluated at the {operating_radius_m:g} m overboard operating radius "
        "(a derated radius, not the most favourable maximum-capacity position)",
        f"DAF lift-off = {daf_liftoff}, DAF splash zone = {daf_splash} per DNV-ST-N001",
        f"JONSWAP peak period approximation: Tp = {tp_coefficient:g} * sqrt(Hs)",
        "Slamming coefficient Cs = 5.0 for flat-bottom structures per DNV-RP-H103 Table 4-1",
        "Added mass coefficient Ca = 1.0, drag coefficient Cd = 2.0 for flat plate geometry",
        f"Wire rope MBL safety factor = {wire_mbl_sf} (max tension / MBL)",
        # B3b (§4): the landing check now uses a DERIVED undrained bearing capacity q_ult.
        f"Landing checks a derived undrained bearing capacity q_ult = su * Nc * sc * dc "
        f"(Brinch Hansen, phi=0; su = {soil_su_kpa:g} kPa soft clay, Nc = {soil_nc:g}), "
        f"with q_allow = q_ult / FS (FS = {soil_fs:g}); utilisation = applied / q_allow (#264)",
        "Landing capacity assumes uniform su and is a surface-footing q_ult with a depth "
        "factor; a skirted/embedded mudmat's reverse-end-bearing / suction is not modelled "
        "(screening scope)",
        "Vessel heave at crane tip uses simplified single-frequency RAO peak",
        "No current loads or wind loads on structure during lowering",
        "Rigging weight excluded from hook load calculations",
        "Seabed assumed flat — no slope corrections for landing",
        f"Seawater density = {seawater_density:g} kg/m3 throughout water column",
    ])

    if output_path is None:
        base_dir = output_dir if output_dir is not None else OUTPUT_DIR
        output_path = base_dir / "demo_03_mudmat_installation_report.html"
    output_path = Path(output_path)
    # demo_03's build_report never mkdir'd output_path's parent (the legacy path relied on
    # main() pre-creating OUTPUT_DIR). A named-run path adds a fresh <run_id>/ dir, so create
    # it here. exist_ok keeps the legacy/baseline call a no-op (byte-identical behaviour).
    output_path.parent.mkdir(parents=True, exist_ok=True)
    html = report.build(output_path)

    print(f"  Report saved to: {output_path}")
    return html


# ---------------------------------------------------------------------------
# Results serialisation
# ---------------------------------------------------------------------------

def build_metadata(
    all_results: List[Dict],
    config: Optional[Any] = None,
) -> Dict[str, Any]:
    """Build the JSON ``metadata`` block from the resolved config (ADR-0005).

    The axes (vessels, structures, depths, hs) and ALL constants are read from *config* so
    editing the yaml changes the saved metadata live. When *config* is None the module
    constants are used (legacy default — byte-identical to today).

    The vessel/structure NAME lists are derived from the produced results in first-seen
    order, so a subset config reports the axes actually swept.
    """
    # First-seen-order axis labels from the produced results.
    vessel_names: List[str] = []
    structure_names: List[str] = []
    for r in all_results:
        if r["vessel_name"] not in vessel_names:
            vessel_names.append(r["vessel_name"])
        if r["structure_name"] not in structure_names:
            structure_names.append(r["structure_name"])

    if config is not None:
        depths = list(config.depths)
        hs_values = list(config.hs)
        reference_hs = config.reference_hs
        constants = {
            "seawater_density_kg_m3": config.seawater_density_kg_m3,
            "gravity_m_s2": config.gravity_m_s2,
            "steel_density_kg_m3": config.steel_density_kg_m3,
            "daf_liftoff": config.daf_liftoff,
            "daf_splash": config.daf_splash,
            "soil_su_kpa": config.undrained_shear_strength_su_kpa,
            "soil_nc": config.bearing_capacity_factor_nc,
            "soil_apply_shape_factor": config.apply_shape_factor,
            "soil_apply_depth_factor": config.apply_depth_factor,
            "soil_factor_of_safety": config.factor_of_safety,
            "wire_mbl_sf": config.wire_mbl_sf,
        }
    else:
        depths = list(WATER_DEPTHS)
        hs_values = list(HS_VALUES)
        reference_hs = REFERENCE_HS
        constants = {
            "seawater_density_kg_m3": SEAWATER_DENSITY,
            "gravity_m_s2": GRAVITY,
            "steel_density_kg_m3": STEEL_DENSITY,
            "daf_liftoff": DAF_LIFTOFF,
            "daf_splash": DAF_SPLASH,
            "soil_su_kpa": SOIL_SU_KPA,
            "soil_nc": SOIL_NC,
            "soil_apply_shape_factor": SOIL_APPLY_SHAPE_FACTOR,
            "soil_apply_depth_factor": SOIL_APPLY_DEPTH_FACTOR,
            "soil_factor_of_safety": SOIL_FACTOR_OF_SAFETY,
            "wire_mbl_sf": WIRE_MBL_SF,
        }

    return {
        "demo": "GTM Demo 3: Deepwater Mudmat Installation Analysis",
        "total_cases": len(all_results),
        "vessels": vessel_names,
        "structures": structure_names,
        "water_depths_m": depths,
        "hs_values_m": hs_values,
        "reference_hs_m": reference_hs,
        "constants": constants,
    }


def serialise_results(all_results: List[Dict]) -> List[Dict]:
    """Prepare results for JSON serialisation — strip non-serialisable types."""
    clean = []
    for r in all_results:
        record = dict(r)
        # Flatten phases dict to avoid nested complexity
        phases_flat = {}
        for pname, pdata in record.get("phases", {}).items():
            for k, v in pdata.items():
                phases_flat[f"{pname.lower().replace(' ', '_')}_{k}"] = v
        record["phases_flat"] = phases_flat
        # Keep phases as-is for the JSON (they are already dicts of primitives)
        clean.append(record)
    return clean


def deserialise_results(json_data: Dict) -> Tuple[List[Dict], pd.DataFrame]:
    """Reconstruct results list and DataFrame from JSON cache."""
    all_results = json_data["cases"]
    rows = []
    for r in all_results:
        row = {
            "vessel_id": r["vessel_id"],
            "vessel_name": r["vessel_name"],
            "structure_id": r["structure_id"],
            "structure_name": r["structure_name"],
            "mass_te": r["mass_te"],
            "water_depth_m": r["water_depth_m"],
            "hs_m": r["hs_m"],
            "overall_status": r["overall_status"],
            "max_utilisation": r["max_utilisation"],
            "governing_phase": r["governing_phase"],
        }
        for pname in PHASE_NAMES:
            phase_data = r.get("phases", {}).get(pname, {})
            row[f"util_{pname.lower().replace(' ', '_').replace('-', '_')}"] = phase_data.get(
                "utilisation"
            )
        rows.append(row)

    df = pd.DataFrame(rows)
    return all_results, df


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Results Store subcommands: `lookup` + `rebuild-db`
#
# Dispatched by an argv-sniff in __main__ so they NEVER touch the existing sweep parser — the
# no-arg / --from-cache / --force / --results-dir / --run-id / --config sweep path stays
# byte-identical. Each subcommand builds its OWN ArgumentParser over sys.argv[2:].
# ---------------------------------------------------------------------------

# Valid overall_status tokens (matches the store's GO/MARGINAL/NO_GO/ERROR taxonomy).
_VALID_STATUSES = ("GO", "MARGINAL", "NO_GO", "ERROR")

# Columns shown by `lookup`, in a clean operator-facing order. The 5 per-phase utilisation
# columns are HYPHEN-prefixed (`lift-off_utilisation`, ...) so they MUST be quoted via rs._q()
# in the SELECT or SQLite would arithmetic-parse `lift-off_utilisation` as `lift - off_...`.
_LOOKUP_DISPLAY_COLS = [
    "vessel_id",
    "structure_id",
    "water_depth_m",
    "hs_m",
    "overall_status",
    "governing_phase",
    "max_utilisation",
    "lift-off_utilisation",
    "in-air_utilisation",
    "splash_zone_utilisation",
    "lowering_utilisation",
    "landing_utilisation",
]


def lookup_main(argv: List[str]) -> int:
    """`lookup` subcommand: read-only filtered SELECT over the demo_03 Results Store cache.

    Returns a process exit code (0 = ok, non-zero = error such as a missing db).
    """
    import sqlite3

    try:
        import results_store_demo03 as rs
    except ImportError:  # pragma: no cover — packaged import path fallback.
        from examples.demos.gtm import results_store_demo03 as rs

    parser = argparse.ArgumentParser(
        prog="demo_03_deepwater_mudmat_installation.py lookup",
        description="Look up cases in the demo_03 Results Store (read-only SQLite cache).",
    )
    parser.add_argument("--run-id", type=str, default=BASELINE_RUN_ID,
                        help=f"Run to query (default '{BASELINE_RUN_ID}').")
    parser.add_argument("--vessel-id", type=str, default=None,
                        help="Vessel SHORT id filter (e.g. CSV-001 / CSV-002).")
    parser.add_argument("--structure-id", type=str, default=None,
                        help="Structure SHORT id filter (e.g. MUD-S / MUD-M / MUD-L).")
    parser.add_argument("--water-depth", type=int, default=None,
                        help="Water depth filter in metres, INTEGER (e.g. 2000).")
    parser.add_argument("--hs", type=float, default=None,
                        help="Significant wave height filter in metres, FLOAT (e.g. 2.0).")
    parser.add_argument("--status", type=str, default=None,
                        help=f"Overall status filter (one of: {', '.join(_VALID_STATUSES)}).")
    parser.add_argument("--base-dir", type=Path, default=RESULTS_DIR,
                        help="Store base dir (default the demo's results/ dir).")
    args = parser.parse_args(argv)

    # Validate run_id before using it (clean error + non-zero exit, no traceback).
    try:
        validate_run_id(args.run_id)
    except ValueError as exc:
        print(f"[ERROR] {exc}")
        return 2

    # Validate --status against the known taxonomy (clear, non-zero, no query attempted).
    if args.status is not None and args.status not in _VALID_STATUSES:
        print(f"unknown status; valid: {', '.join(_VALID_STATUSES)}")
        return 2

    db_path = rs._db_path(args.base_dir)
    if not db_path.exists():
        # Do NOT create the db; clear, actionable message; non-zero exit.
        print(
            f"No results.db at {db_path} — run a sweep first "
            f"(e.g. `... --run-id {BASELINE_RUN_ID}`) or `... rebuild-db`."
        )
        return 1

    # Open strictly read-only via a file: URI so a query never mutates/creates the cache.
    conn = sqlite3.connect(f"file:{db_path}?mode=ro", uri=True)
    try:
        # Distinct message for an unknown run_id vs. a zero-row filter result.
        known_runs = [r[0] for r in conn.execute("SELECT run_id FROM runs ORDER BY run_id")]
        if args.run_id not in known_runs:
            available = ", ".join(known_runs) if known_runs else "(none)"
            print(f"run_id '{args.run_id}' not found; available: {available}")
            return 1

        # Build a parameterized WHERE — each filter binds its value (?) and is added only
        # when provided. --water-depth binds int, --hs binds float (BD-3 column types).
        where = ["run_id = ?"]
        params: List[Any] = [args.run_id]
        if args.vessel_id is not None:
            where.append("vessel_id = ?")
            params.append(str(args.vessel_id))
        if args.structure_id is not None:
            where.append("structure_id = ?")
            params.append(str(args.structure_id))
        if args.water_depth is not None:
            where.append("water_depth_m = ?")
            params.append(int(args.water_depth))
        if args.hs is not None:
            where.append("hs_m = ?")
            params.append(float(args.hs))
        if args.status is not None:
            where.append("overall_status = ?")
            params.append(str(args.status))

        # B1: quote EVERY display column via rs._q() so the hyphen-prefixed per-phase columns
        # are legal identifiers (unquoted they would be arithmetic-parsed → OperationalError).
        cols = ", ".join(rs._q(c) for c in _LOOKUP_DISPLAY_COLS)
        sql = (
            f"SELECT {cols} FROM cases WHERE {' AND '.join(where)} "
            "ORDER BY vessel_id, structure_id, water_depth_m, hs_m"
        )
        rows = conn.execute(sql, params).fetchall()
    finally:
        conn.close()

    if not rows:
        print("0 cases match.")
        return 0

    # Clean fixed-width table.
    widths = [len(c) for c in _LOOKUP_DISPLAY_COLS]
    str_rows = [[("" if v is None else str(v)) for v in row] for row in rows]
    for r in str_rows:
        for i, cell in enumerate(r):
            widths[i] = max(widths[i], len(cell))
    header = "  ".join(c.ljust(widths[i]) for i, c in enumerate(_LOOKUP_DISPLAY_COLS))
    print(header)
    print("  ".join("-" * widths[i] for i in range(len(_LOOKUP_DISPLAY_COLS))))
    for r in str_rows:
        print("  ".join(cell.ljust(widths[i]) for i, cell in enumerate(r)))
    print(f"\n{len(rows)} case(s) matched.")
    return 0


def rebuild_main(argv: List[str]) -> int:
    """`rebuild-db` subcommand: regenerate the SQLite cache + index.csv from the text source."""
    import sqlite3

    try:
        import results_store_demo03 as rs
    except ImportError:  # pragma: no cover — packaged import path fallback.
        from examples.demos.gtm import results_store_demo03 as rs

    parser = argparse.ArgumentParser(
        prog="demo_03_deepwater_mudmat_installation.py rebuild-db",
        description="Rebuild the demo_03 Results Store SQLite cache + index.csv from "
                    "the per-run cases.csv / manifest.json text source of truth.",
    )
    parser.add_argument("--base-dir", type=Path, default=RESULTS_DIR,
                        help="Store base dir (default the demo's results/ dir).")
    args = parser.parse_args(argv)

    db_path = rs.rebuild_db(args.base_dir)
    print(f"Results Store rebuilt: {db_path}")
    conn = sqlite3.connect(f"file:{db_path}?mode=ro", uri=True)
    try:
        n_runs = conn.execute("SELECT COUNT(*) FROM runs").fetchone()[0]
        n_cases = conn.execute("SELECT COUNT(*) FROM cases").fetchone()[0]
        per_run = conn.execute(
            "SELECT run_id, COUNT(*) FROM cases GROUP BY run_id ORDER BY run_id"
        ).fetchall()
    finally:
        conn.close()
    print(f"  runs: {n_runs}   cases: {n_cases}")
    for run_id, n in per_run:
        print(f"    {run_id}: {n} cases")
    return 0


def main():
    """Run the full demo pipeline."""
    parser = argparse.ArgumentParser(
        description="GTM Demo 3: Deepwater Mudmat Installation Analysis",
    )
    parser.add_argument(
        "--from-cache",
        action="store_true",
        help="Load results from cached JSON instead of re-running the sweep",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="Force re-run even if cached results exist",
    )
    parser.add_argument(
        "--results-dir",
        default=None,
        help="Override the results directory (legacy JSON). Defaults to the demo's "
        "results/ dir (or the yaml's results_root).",
    )
    parser.add_argument(
        "--run-id",
        type=str,
        default=None,
        help="Run identifier for the Results Store + per-run report dir "
        f"(default the Baseline Run '{BASELINE_RUN_ID}'). A named run writes its report to "
        "output/parametric/demo_03/<run_id>/report.html and seeds <run_id> in the Store.",
    )
    parser.add_argument(
        "--config",
        type=Path,
        default=BASELINE_CONFIG_PATH,
        help="Path to the Sweep Config yaml driving the run (axes, constants, catalogs, "
        f"artifact paths). Defaults to the committed baseline ({BASELINE_CONFIG_PATH.name}); "
        "point it at your own yaml to run a client config without editing the baseline.",
    )
    args = parser.parse_args()

    # Validate the config path up front (clear error, never a raw traceback later).
    config_path = Path(args.config)
    if not config_path.exists():
        print(f"[ERROR] --config path does not exist: {config_path}")
        sys.exit(2)

    # Validate run_id BEFORE any run dir / report path is built from it (path-traversal /
    # empty / separator guard). Clean error + non-zero exit, never a raw traceback. None means
    # "default" (resolves to BASELINE_RUN_ID at write/report time), so only validate a value.
    if args.run_id is not None:
        try:
            validate_run_id(args.run_id)
        except ValueError as exc:
            print(f"[ERROR] {exc}")
            sys.exit(2)

    # D3: validate the FULL config once, up front — a schema-invalid (but existing) config
    # must surface a clean error here, not a misleading "using committed defaults" path-resolve
    # warning followed by a raw SweepConfigError traceback from the recompute reload.
    try:
        from sweep_config_demo03 import SweepConfigError, load_demo03_config
    except ImportError:  # pragma: no cover — packaged import path fallback.
        from examples.demos.gtm.sweep_config_demo03 import (
            SweepConfigError,
            load_demo03_config,
        )
    try:
        validated_config = load_demo03_config(config_path)
    except SweepConfigError as exc:
        print(f"[ERROR] {exc}")
        sys.exit(2)

    # D1: --from-cache rebuilds the report/charts from a REAL config (never config=None, which
    # would render stale module-global literals or crash when the cached hs axis excludes the
    # reference Hs). The cache on disk was produced by SOME config; the only config we can prove
    # matches it is the demo's committed BASELINE (whose values == the module globals, so the
    # baseline --from-cache stays byte-identical). Refuse --from-cache with a non-default
    # --config: we cannot prove the supplied config produced the cache, and threading a
    # mismatched config would lie (or crash). Refuse-on-mismatch keeps the golden untouched.
    is_default_config = config_path.resolve() == Path(BASELINE_CONFIG_PATH).resolve()
    if args.from_cache and not args.force and not is_default_config:
        print(
            "[ERROR] --from-cache is only supported with the committed baseline config. "
            "A non-default --config cannot be proven to match the cached results "
            f"({config_path}); re-run without --from-cache to recompute from your config, "
            "or drop --config to reload the baseline cache."
        )
        sys.exit(2)

    # N1: --from-cache regenerates the Baseline report only (cached data is always the baseline
    # 180 cases). A named --run-id under --from-cache would write a mislabeled report, so warn
    # AND force the report path back to the legacy Baseline location below (mirror demo_02).
    if args.from_cache and args.run_id is not None and args.run_id != BASELINE_RUN_ID:
        warnings.warn(
            f"--from-cache regenerates the baseline report only; --run-id '{args.run_id}' "
            "will not produce a named Results Store row (run without --from-cache to compute "
            "a named run). The report is written to the baseline path.",
            stacklevel=2,
        )

    start_time = time.time()

    print("=" * 60)
    print("  GTM Demo 3: Deepwater Mudmat Installation Analysis")
    print("=" * 60)

    # Resolve catalog + artifact PATHS from the config yaml (engineering-free, so this also
    # runs on the --from-cache path). On any load failure fall back to the committed module
    # dirs so the demo still runs. The CLI --results-dir always wins.
    vessels_path = DATA_DIR / "csv_hlv_vessels.json"
    mudmats_path = DATA_DIR / "mudmat_structures.json"
    yaml_results_root = RESULTS_DIR
    yaml_output_root = OUTPUT_DIR
    try:
        try:
            from sweep_config_demo03 import load_demo03_paths
        except ImportError:  # pragma: no cover — packaged import path fallback.
            from examples.demos.gtm.sweep_config_demo03 import load_demo03_paths
        paths = load_demo03_paths(config_path)
        vessels_path = paths.vessels_path
        mudmats_path = paths.mudmats_path
        yaml_results_root = paths.results_root
        yaml_output_root = paths.output_root
    except Exception as exc:  # noqa: BLE001 — committed-baseline fallback is intentional.
        logger.warning(
            "Could not resolve paths from %s (%s); using committed defaults.",
            config_path, exc,
        )

    results_dir = Path(args.results_dir) if args.results_dir else yaml_results_root
    results_path = results_dir / "demo_03_mudmat_installation_results.json"

    # ── Step 1: Load data ──────────────────────────────────────────────────
    print("\n[1/7] Loading input data...")
    with open(vessels_path, "r") as f:
        vessels = json.load(f)["vessels"]
    with open(mudmats_path, "r") as f:
        structures = json.load(f)["structures"]
    print(f"  Loaded {len(vessels)} vessels from csv_hlv_vessels.json")
    print(f"  Loaded {len(structures)} structures from mudmat_structures.json")

    # ── Step 2: Run sweep or load cache ────────────────────────────────────
    # D1: config is the resolved Sweep Config, threaded into BOTH paths. On --from-cache we
    # reuse the already-validated config (refused above unless it is the committed baseline),
    # so the chart/summary/report builders run against a REAL config — never config=None, which
    # rendered stale module-global literals or crashed when the cached hs axis excluded the
    # reference Hs. The baseline config's values == the module globals, so baseline --from-cache
    # stays byte-identical to the golden.
    config = validated_config
    if args.from_cache and results_path.exists() and not args.force:
        print("\n[2/7] Loading cached results...")
        with open(results_path, "r") as f:
            cached = json.load(f)
        all_results, results_df = deserialise_results(cached)
        total_cases = len(all_results)
        print(f"  Loaded {total_cases} cached results from {results_path.name}")
    else:
        # Recompute path — ADR-0005: the COMPLETE config source (already validated above).
        print("\n[2/7] Running parametric sweep...")
        all_results, results_df = run_parametric_sweep(vessels, structures, config=config)
        total_cases = len(all_results)

        # Results Store (additive): persist the run alongside the legacy JSON/HTML.
        # RECOMPUTE branch only (config is not None). If --from-cache demoted to
        # recompute (stale/missing cache), force the BASELINE run_id so we never seed
        # a named row the --from-cache contract said wouldn't exist (mirror demo_02).
        try:
            from results_store_demo03 import write_run as _store_write_run
        except ImportError:  # pragma: no cover — packaged import path fallback.
            from examples.demos.gtm.results_store_demo03 import (
                write_run as _store_write_run,
            )
        _store_write_run(
            run_id=(BASELINE_RUN_ID if args.from_cache else (args.run_id or BASELINE_RUN_ID)),
            config=config,
            results=all_results,
            base_dir=results_dir,
        )

    # ── Step 3: Build charts ───────────────────────────────────────────────
    print("\n[3/7] Building charts...")
    fig1 = build_chart_1_go_nogo_heatmap(results_df, config=config)
    fig2 = build_chart_2_crane_utilisation(results_df, config=config)
    fig3 = build_chart_3_daf_vs_depth(results_df, config=config)
    fig4 = build_chart_4_hs_limit(results_df, vessels, structures, config=config)
    fig5 = build_chart_5_vessel_comparison(results_df, config=config)

    # ── Step 4: Build summary table ────────────────────────────────────────
    print("\n[4/7] Building summary table...")
    summary_df = build_summary_table(results_df, config=config)
    print(summary_df.to_string(index=False))

    # ── Step 5: Build HTML report ──────────────────────────────────────────
    print("\n[5/7] Building HTML report...")
    output_dir = yaml_output_root
    output_dir.mkdir(parents=True, exist_ok=True)
    # Baseline Run keeps the legacy output path (byte-identical); a named Run writes a per-run
    # report under output/parametric/demo_03/<run_id>/report.html. Under --from-cache the data
    # is always the baseline 180, so a named run is forced back to the baseline path (N1).
    if args.run_id is None or args.run_id == BASELINE_RUN_ID or args.from_cache:
        report_path: Optional[Path] = None  # build_report defaults to the legacy Baseline path
    else:
        report_path = yaml_output_root / "parametric" / DEMO_ID / args.run_id / "report.html"
    build_report(
        fig1, fig2, fig3, fig4, fig5, summary_df, all_results, total_cases,
        config=config, output_dir=output_dir, output_path=report_path,
    )
    if report_path is not None:
        print(f"  Per-run report written to: {report_path}")

    # ── Step 6: Save JSON results ──────────────────────────────────────────
    if not args.from_cache or args.force:
        print("\n[6/7] Saving JSON results...")
        results_dir.mkdir(parents=True, exist_ok=True)

        json_output = {
            "metadata": build_metadata(all_results, config=config),
            "summary": summary_df.to_dict(orient="records"),
            "cases": serialise_results(all_results),
        }

        with open(results_path, "w") as f:
            json.dump(json_output, f, indent=2, default=str)
        print(f"  Results saved to: {results_path}")
    else:
        print("\n[6/7] Skipping JSON save (loaded from cache)")

    # ── Step 7: Done ───────────────────────────────────────────────────────
    elapsed = time.time() - start_time
    print(f"\n[7/7] Complete!")
    print(f"{'='*60}")
    print(f"  Total cases analysed:  {total_cases}")
    print(f"  HTML report:           output/demo_03_mudmat_installation_report.html")
    print(f"  JSON results:          results/demo_03_mudmat_installation_results.json")
    print(f"  Time elapsed:          {elapsed:.1f} seconds")
    print(f"{'='*60}")


if __name__ == "__main__":
    # Argv-sniff dispatch: the `lookup` / `rebuild-db` subcommands are handled by their OWN
    # parsers over sys.argv[2:] and NEVER reach the sweep parser, so the no-arg / --from-cache
    # / --force / --results-dir / --run-id / --config sweep path is untouched and byte-identical.
    if len(sys.argv) > 1 and sys.argv[1] == "lookup":
        sys.exit(lookup_main(sys.argv[2:]))
    elif len(sys.argv) > 1 and sys.argv[1] == "rebuild-db":
        sys.exit(rebuild_main(sys.argv[2:]))
    else:
        main()
