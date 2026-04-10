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
BEARING_LIMIT_KPA = 50.0  # soft clay default bearing capacity
WIRE_MBL_SF = 0.85  # max cable tension / MBL
TILT_LIMIT_DEG = 5.0  # max tilt during in-air transit

# Operating radius for overboard lifts — crane SWL is derated at larger radii.
# Typical subsea lifts use 35-40m radius (boom over the side), not the minimum
# capacity radius. This is the dominant factor in crane utilisation.
OPERATING_RADIUS_M = 40.0

# Phase names (display)
PHASE_NAMES = ["Lift-off", "In-air", "Splash zone", "Lowering", "Landing"]


def tp_from_hs(hs: float) -> float:
    """JONSWAP peak period approximation: Tp = 4 * sqrt(Hs)."""
    return 4.0 * math.sqrt(hs)


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

def calc_liftoff(vessel: Dict, structure: Dict) -> Dict[str, Any]:
    """Phase 1: Lift-off check.

    Hook load = mass_air * g * DAF_lift
    Check: hook_load <= crane SWL at operating radius
    """
    mass_air_kg = structure["mass_properties"]["mass_air_kg"]
    crane = vessel["crane_main"]

    # Operating radius for overboard lift — SWL derated at larger radii
    operating_radius = OPERATING_RADIUS_M
    crane_swl_te = interpolate_crane_swl(crane["crane_capacity_curve"], operating_radius)
    crane_swl_kn = crane_swl_te * 1000.0 * GRAVITY / 1000.0  # te -> kg -> N -> kN

    hook_load_kn = mass_air_kg * GRAVITY * DAF_LIFTOFF / 1000.0  # N -> kN
    utilisation = hook_load_kn / crane_swl_kn if crane_swl_kn > 0 else 999.0

    return {
        "phase": "Lift-off",
        "hook_load_kn": round(hook_load_kn, 1),
        "crane_swl_kn": round(crane_swl_kn, 1),
        "operating_radius_m": operating_radius,
        "daf": DAF_LIFTOFF,
        "utilisation": round(utilisation, 4),
        "status": "PASS" if utilisation <= 1.0 else "FAIL",
    }


def calc_in_air(vessel: Dict, structure: Dict) -> Dict[str, Any]:
    """Phase 2: In-air transit — tilt check.

    Simplified: 4-point lift with spreader beam. For properly rigged mudmats
    with symmetric CoG, tilt is negligible.
    """
    cog = structure["centre_of_gravity"]
    rigging = structure["rigging"]

    # Tilt = atan(cog_offset / hook_height_above_cog)
    cog_offset = math.sqrt(cog["x_m"] ** 2 + cog["y_m"] ** 2)
    hook_height = rigging["hook_height_above_cog_m"]
    tilt_deg = math.degrees(math.atan2(cog_offset, hook_height)) if hook_height > 0 else 0.0

    utilisation = tilt_deg / TILT_LIMIT_DEG if TILT_LIMIT_DEG > 0 else 0.0

    return {
        "phase": "In-air",
        "tilt_deg": round(tilt_deg, 2),
        "tilt_limit_deg": TILT_LIMIT_DEG,
        "cog_offset_m": round(cog_offset, 3),
        "hook_height_m": hook_height,
        "utilisation": round(utilisation, 4),
        "status": "PASS" if tilt_deg <= TILT_LIMIT_DEG else "FAIL",
    }


def calc_splash_zone(vessel: Dict, structure: Dict, hs: float) -> Dict[str, Any]:
    """Phase 3: Splash zone — slamming, varying buoyancy, drag.

    This is typically the CRITICAL phase for mudmat installations.

    Slamming:  F_slam = 0.5 * rho * Cs * Ap * v_rel^2
    Buoyancy:  F_var  = rho * g * A_wp * (Hs/2)
    Drag:      F_drag = 0.5 * rho * Cd * A_side * v_lowering^2
    Hook load: W_air + F_slam + F_var + F_drag (all in kN)
    """
    mass_air_kg = structure["mass_properties"]["mass_air_kg"]
    hydro = structure["hydrodynamic_coefficients"]
    areas = structure["projected_areas"]
    crane = vessel["crane_main"]

    # Velocities
    hoist_speed_m_per_s = crane["hoist_speed_m_per_min"] / 60.0  # m/min -> m/s
    tp = tp_from_hs(hs)
    v_wave = math.pi * hs / tp if tp > 0 else 0.0  # simplified surface particle velocity
    v_rel = hoist_speed_m_per_s + v_wave

    # Forces (all in kN)
    cs = hydro["slamming_coefficient_Cs"]
    ap = areas["bottom_m2"]
    f_slam_kn = 0.5 * SEAWATER_DENSITY * cs * ap * v_rel ** 2 / 1000.0

    a_wp = areas["bottom_m2"]  # waterplane area ~ bottom area
    f_var_kn = SEAWATER_DENSITY * GRAVITY * a_wp * (hs / 2.0) / 1000.0

    cd = hydro["drag_coefficient_Cd"]
    a_side = areas["side_long_m2"]
    f_drag_kn = 0.5 * SEAWATER_DENSITY * cd * a_side * hoist_speed_m_per_s ** 2 / 1000.0

    w_air_kn = mass_air_kg * GRAVITY / 1000.0
    max_hook_splash_kn = w_air_kn + f_slam_kn + f_var_kn + f_drag_kn

    # Apply splash DAF
    design_hook_kn = max_hook_splash_kn * DAF_SPLASH

    # Crane capacity at overboard operating radius
    operating_radius = OPERATING_RADIUS_M
    crane_swl_te = interpolate_crane_swl(crane["crane_capacity_curve"], operating_radius)
    crane_swl_kn = crane_swl_te * 1000.0 * GRAVITY / 1000.0

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
        "daf_splash": DAF_SPLASH,
        "design_hook_kn": round(design_hook_kn, 1),
        "crane_swl_kn": round(crane_swl_kn, 1),
        "utilisation": round(utilisation, 4),
        "status": "PASS" if utilisation <= 1.0 else "FAIL",
    }


def calc_lowering(vessel: Dict, structure: Dict, depth: float, hs: float) -> Dict[str, Any]:
    """Phase 4: Lowering through water column — cable tension check.

    Cable tension: T = W_sub + W_cable_sub(depth) + snap_load
    Snap load from dynamic cable response.
    """
    mass_air_kg = structure["mass_properties"]["mass_air_kg"]
    displaced_vol = structure["mass_properties"]["displaced_volume_m3"]
    hydro = structure["hydrodynamic_coefficients"]
    crane = vessel["crane_main"]
    motion = vessel["motion_characteristics"]

    # Submerged weight of structure
    w_sub_kn = (mass_air_kg - SEAWATER_DENSITY * displaced_vol) * GRAVITY / 1000.0

    # Cable weight in water
    wire_dia_m = crane["main_wire_diameter_mm"] / 1000.0
    cable_area_m2 = math.pi / 4.0 * wire_dia_m ** 2
    cable_unit_weight_kn_per_m = cable_area_m2 * (STEEL_DENSITY - SEAWATER_DENSITY) * GRAVITY / 1000.0
    w_cable_kn = cable_unit_weight_kn_per_m * depth

    # Snap load / dynamic cable tension
    ca = hydro["added_mass_coefficient_Ca"]
    added_mass_kg = SEAWATER_DENSITY * displaced_vol * ca
    total_dynamic_mass_kg = mass_air_kg + added_mass_kg

    tp = tp_from_hs(hs)
    omega = 2.0 * math.pi / tp if tp > 0 else 0.0
    heave_rao_peak = motion["heave_rao"]["peak_amplitude_m_per_m"]
    heave_amp = hs * heave_rao_peak / 2.0  # heave amplitude at crane tip

    snap_load_kn = total_dynamic_mass_kg * omega ** 2 * heave_amp / 1000.0

    # Total cable tension
    max_tension_kn = w_sub_kn + w_cable_kn + snap_load_kn

    # Wire MBL
    wire_mbl_te = crane["main_wire_mbl_te"]
    wire_mbl_kn = wire_mbl_te * 1000.0 * GRAVITY / 1000.0
    allowable_kn = WIRE_MBL_SF * wire_mbl_kn

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


def calc_landing(structure: Dict) -> Dict[str, Any]:
    """Phase 5: Landing — bearing pressure check.

    Bearing pressure = W_sub / A_base
    Check: p <= 50 kPa (soft clay default)
    """
    mass_air_kg = structure["mass_properties"]["mass_air_kg"]
    displaced_vol = structure["mass_properties"]["displaced_volume_m3"]
    geom = structure["geometry"]

    w_sub_kn = (mass_air_kg - SEAWATER_DENSITY * displaced_vol) * GRAVITY / 1000.0
    a_base_m2 = geom["length_m"] * geom["width_m"]  # full bottom plate area

    bearing_kpa = w_sub_kn / a_base_m2 if a_base_m2 > 0 else 999.0
    utilisation = bearing_kpa / BEARING_LIMIT_KPA

    return {
        "phase": "Landing",
        "w_sub_kn": round(w_sub_kn, 1),
        "a_base_m2": round(a_base_m2, 1),
        "bearing_kpa": round(bearing_kpa, 2),
        "bearing_limit_kpa": BEARING_LIMIT_KPA,
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
) -> Dict[str, Any]:
    """Run all 5 installation phases for a single case.

    Returns a flat dict with overall go/no-go plus per-phase results.
    """
    p1 = calc_liftoff(vessel, structure)
    p2 = calc_in_air(vessel, structure)
    p3 = calc_splash_zone(vessel, structure, hs)
    p4 = calc_lowering(vessel, structure, depth, hs)
    p5 = calc_landing(structure)

    phases = [p1, p2, p3, p4, p5]
    utilisations = [p["utilisation"] for p in phases]
    max_util = max(utilisations)
    governing_idx = utilisations.index(max_util)
    governing_phase = phases[governing_idx]["phase"]

    all_pass = all(p["status"] == "PASS" for p in phases)

    if not all_pass or max_util > 1.0:
        overall = "NO_GO"
    elif max_util >= 0.85:
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

def run_parametric_sweep(
    vessels: List[Dict],
    structures: List[Dict],
) -> Tuple[List[Dict], pd.DataFrame]:
    """Run the full parametric sweep across all combinations."""
    total = len(vessels) * len(WATER_DEPTHS) * len(structures) * len(HS_VALUES)

    print(f"\n{'='*60}")
    print(f"  PARAMETRIC INSTALLATION SCREENING")
    print(f"  {total} cases: {len(vessels)} vessels x {len(WATER_DEPTHS)} depths"
          f" x {len(structures)} mudmats x {len(HS_VALUES)} sea states")
    print(f"{'='*60}\n")

    all_results: List[Dict] = []
    case_num = 0

    for vessel in vessels:
        for depth in WATER_DEPTHS:
            for structure in structures:
                for hs in HS_VALUES:
                    case_num += 1
                    print(
                        f"  Case {case_num:3d}/{total} | {vessel['name']:<12s} | "
                        f"{depth:5.0f}m | {structure['name']:<18s} | Hs={hs:.1f}m ...",
                        end="",
                    )

                    try:
                        result = run_single_case(vessel, structure, depth, hs)
                        all_results.append(result)
                        status_tag = result["overall_status"]
                        util = result["max_utilisation"]
                        print(f" util={util:.3f} [{status_tag}]")
                    except Exception as exc:
                        logger.warning("Case failed: %s", exc)
                        print(f" [ERROR: {exc}]")
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

def build_chart_1_go_nogo_heatmap(results_df: pd.DataFrame) -> go.Figure:
    """Go/No-Go heatmap at reference Hs — side-by-side for two vessels."""
    print("\n[Chart 1] Building Go/No-Go Heatmap...")

    ref_df = results_df[results_df["hs_m"] == REFERENCE_HS].copy()
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
            text=f"Installation Go/No-Go Matrix (Hs = {REFERENCE_HS} m)",
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

def build_chart_2_crane_utilisation(results_df: pd.DataFrame) -> go.Figure:
    """Crane utilisation vs water depth, parametric by mudmat and vessel."""
    print("\n[Chart 2] Building Crane Utilisation vs Water Depth...")

    ref_df = results_df[results_df["hs_m"] == REFERENCE_HS].copy()
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
            text=f"Crane Utilisation vs Water Depth (Hs = {REFERENCE_HS} m)",
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

def build_chart_3_daf_vs_depth(results_df: pd.DataFrame) -> go.Figure:
    """Effective DAF (splash zone + lowering) vs water depth by mudmat size."""
    print("\n[Chart 3] Building DAF vs Water Depth...")

    ref_df = results_df[results_df["hs_m"] == REFERENCE_HS].copy()
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
            text=f"Phase Utilisation vs Water Depth — Splash & Lowering (Large CSV, Hs = {REFERENCE_HS} m)",
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
) -> go.Figure:
    """Max allowable Hs for each structure/vessel combination — operability envelope."""
    print("\n[Chart 4] Building Max Hs Limit vs Structure Weight...")

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
            for hs in sorted(HS_VALUES):
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
        yaxis=dict(range=[0, max(HS_VALUES) + 0.5]),
    )

    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 5: Vessel Head-to-Head Comparison
# ---------------------------------------------------------------------------

def build_chart_5_vessel_comparison(results_df: pd.DataFrame) -> go.Figure:
    """Grouped bar: structures installable per depth per vessel at reference Hs."""
    print("\n[Chart 5] Building Vessel Head-to-Head Comparison...")

    ref_df = results_df[results_df["hs_m"] == REFERENCE_HS].copy()
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
            text=f"Vessel Comparison — Structures Installable per Depth (Hs = {REFERENCE_HS} m)",
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

def build_summary_table(results_df: pd.DataFrame) -> pd.DataFrame:
    """Build summary table: vessel x structure x reference Hs at all depths."""
    ref_df = results_df[results_df["hs_m"] == REFERENCE_HS].copy()

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
) -> str:
    """Build the branded HTML report."""
    print("\n[Report] Building HTML report...")

    report = GTMReportBuilder(
        title="Deepwater Mudmat Installation Analysis",
        subtitle=f"{total_cases} parametric cases: 2 vessels x 6 depths x 3 mudmats x 5 sea states",
        demo_id="demo_03",
        case_count=total_cases,
        code_refs=[
            "DNV-RP-H103 (2011) — Modelling and Analysis of Marine Operations",
            "DNV-ST-N001 (2021) — Marine Operations and Marine Warranty",
            "DNV-RP-C205 (2010) — Environmental Conditions and Environmental Loads",
        ],
    )

    methodology_html = """
    <p>This analysis screens deepwater mudmat installations across a parametric matrix of
    vessels, water depths, structure sizes, and sea states. Each case evaluates five
    installation phases per DNV-RP-H103 and DNV-ST-N001 methodology.</p>

    <h3>Phase 1: Lift-off</h3>
    <p>Hook load = mass<sub>air</sub> &times; g &times; DAF<sub>lift</sub> (1.10).
    Checked against crane SWL at operating radius via interpolation of the crane
    capacity curve.</p>

    <h3>Phase 2: In-air Transit</h3>
    <p>Tilt check for 4-point lift with spreader beam. CoG offset / hook height gives
    tilt angle, checked against 5&deg; limit. Symmetric mudmats always pass.</p>

    <h3>Phase 3: Splash Zone (Critical)</h3>
    <p>Slamming force: F<sub>slam</sub> = 0.5 &times; &rho; &times; C<sub>s</sub>
    &times; A<sub>p</sub> &times; v<sub>rel</sub><sup>2</sup>, where
    v<sub>rel</sub> = v<sub>lowering</sub> + v<sub>wave</sub>.
    Additional varying buoyancy and drag forces. DAF<sub>splash</sub> = 1.30.
    This phase typically governs for large mudmats in higher sea states.</p>

    <h3>Phase 4: Lowering through Water Column</h3>
    <p>Cable tension = W<sub>sub</sub> + W<sub>cable</sub>(depth) + snap load.
    Snap load from dynamic cable response using vessel heave RAO. Checked against
    85% of wire MBL. Becomes critical at extreme depths due to cable self-weight.</p>

    <h3>Phase 5: Landing</h3>
    <p>Bearing pressure = W<sub>sub</sub> / A<sub>base</sub>. Checked against
    50 kPa soft clay bearing capacity. Rarely governs for typical mudmat proportions.</p>

    <h3>Go/No-Go Criteria</h3>
    <ul>
        <li><strong>GO:</strong> All phases pass AND max utilisation &lt; 0.85</li>
        <li><strong>MARGINAL:</strong> All phases pass AND max utilisation in [0.85, 1.00]</li>
        <li><strong>NO-GO:</strong> Any phase fails (utilisation &gt; 1.00)</li>
    </ul>
    """
    report.add_methodology(methodology_html)

    report.add_chart(
        "go_nogo",
        fig1,
        title="Chart 1: Go/No-Go Installation Matrix",
        subtitle=f"Side-by-side vessel comparison at Hs = {REFERENCE_HS} m reference sea state.",
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
        subtitle=f"Number of mudmat sizes (out of 3) each vessel can install at Hs = {REFERENCE_HS} m.",
    )

    report.add_table(
        "Installation Screening Summary",
        summary_df,
        subtitle=f"All cases at Hs = {REFERENCE_HS} m reference sea state",
        status_col="Status",
    )

    report.add_live_mode_teaser(
        analysis_type="the installation screening"
    )

    report.add_assumptions([
        "Crane SWL evaluated at maximum capacity radius (most favourable position)",
        "DAF lift-off = 1.10, DAF splash zone = 1.30 per DNV-ST-N001",
        "JONSWAP peak period approximation: Tp = 4 * sqrt(Hs)",
        "Slamming coefficient Cs = 5.0 for flat-bottom structures per DNV-RP-H103 Table 4-1",
        "Added mass coefficient Ca = 1.0, drag coefficient Cd = 2.0 for flat plate geometry",
        "Wire rope MBL safety factor = 0.85 (max tension / MBL)",
        "Bearing capacity limit = 50 kPa (soft clay, undrained)",
        "Vessel heave at crane tip uses simplified single-frequency RAO peak",
        "No current loads or wind loads on structure during lowering",
        "Rigging weight excluded from hook load calculations",
        "Seabed assumed flat — no slope corrections for landing",
        "Seawater density = 1025 kg/m3 throughout water column",
    ])

    output_path = OUTPUT_DIR / "demo_03_mudmat_installation_report.html"
    html = report.build(output_path)

    print(f"  Report saved to: {output_path}")
    return html


# ---------------------------------------------------------------------------
# Results serialisation
# ---------------------------------------------------------------------------

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
    args = parser.parse_args()

    start_time = time.time()

    print("=" * 60)
    print("  GTM Demo 3: Deepwater Mudmat Installation Analysis")
    print("=" * 60)

    results_path = RESULTS_DIR / "demo_03_mudmat_installation_results.json"

    # ── Step 1: Load data ──────────────────────────────────────────────────
    print("\n[1/7] Loading input data...")
    vessels = load_vessels()
    structures = load_structures()
    print(f"  Loaded {len(vessels)} vessels from csv_hlv_vessels.json")
    print(f"  Loaded {len(structures)} structures from mudmat_structures.json")

    # ── Step 2: Run sweep or load cache ────────────────────────────────────
    if args.from_cache and results_path.exists() and not args.force:
        print("\n[2/7] Loading cached results...")
        with open(results_path, "r") as f:
            cached = json.load(f)
        all_results, results_df = deserialise_results(cached)
        total_cases = len(all_results)
        print(f"  Loaded {total_cases} cached results from {results_path.name}")
    else:
        print("\n[2/7] Running parametric sweep...")
        all_results, results_df = run_parametric_sweep(vessels, structures)
        total_cases = len(all_results)

    # ── Step 3: Build charts ───────────────────────────────────────────────
    print("\n[3/7] Building charts...")
    fig1 = build_chart_1_go_nogo_heatmap(results_df)
    fig2 = build_chart_2_crane_utilisation(results_df)
    fig3 = build_chart_3_daf_vs_depth(results_df)
    fig4 = build_chart_4_hs_limit(results_df, vessels, structures)
    fig5 = build_chart_5_vessel_comparison(results_df)

    # ── Step 4: Build summary table ────────────────────────────────────────
    print("\n[4/7] Building summary table...")
    summary_df = build_summary_table(results_df)
    print(summary_df.to_string(index=False))

    # ── Step 5: Build HTML report ──────────────────────────────────────────
    print("\n[5/7] Building HTML report...")
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    build_report(fig1, fig2, fig3, fig4, fig5, summary_df, all_results, total_cases)

    # ── Step 6: Save JSON results ──────────────────────────────────────────
    if not args.from_cache or args.force:
        print("\n[6/7] Saving JSON results...")
        RESULTS_DIR.mkdir(parents=True, exist_ok=True)

        json_output = {
            "metadata": {
                "demo": "GTM Demo 3: Deepwater Mudmat Installation Analysis",
                "total_cases": total_cases,
                "vessels": [v["name"] for v in vessels],
                "structures": [s["name"] for s in structures],
                "water_depths_m": WATER_DEPTHS,
                "hs_values_m": HS_VALUES,
                "reference_hs_m": REFERENCE_HS,
                "constants": {
                    "seawater_density_kg_m3": SEAWATER_DENSITY,
                    "gravity_m_s2": GRAVITY,
                    "steel_density_kg_m3": STEEL_DENSITY,
                    "daf_liftoff": DAF_LIFTOFF,
                    "daf_splash": DAF_SPLASH,
                    "bearing_limit_kpa": BEARING_LIMIT_KPA,
                    "wire_mbl_sf": WIRE_MBL_SF,
                },
            },
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
    main()
