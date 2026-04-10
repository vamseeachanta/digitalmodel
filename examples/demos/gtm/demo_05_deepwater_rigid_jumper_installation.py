#!/usr/bin/env python3
# ABOUTME: GTM Demo 5 — Deepwater Rigid Jumper Installation Analysis
# ABOUTME: Parametric analysis: 2 vessels × 6 depths × 5 jumper lengths × 5 Hs = 300 cases
"""
GTM Demo 5: Deepwater Rigid Jumper Installation Analysis
=========================================================

Runs parametric installation screening across:
  - 2 vessels (Large CSV 5000te, Medium CSV 2500te)
  - 6 water depths (500–3000 m)
  - 5 rigid jumper lengths (20–100 m, 8" OD X65)
  - 5 significant wave heights (1.0–3.0 m)

Evaluates 5 installation phases per case:
  1. Lift-off (hook load vs crane SWL)
  2. In-air bending (simply-supported beam stress vs 0.6×SMYS)
  3. Splash zone (slamming + drag + DAF vs crane SWL)
  4. Lowering through water column (cable tension + snap vs wire MBL)
  5. Tie-in alignment (current-induced deflection vs 50mm tolerance)

Produces:
  - 5 interactive Plotly charts
  - Branded HTML report via GTMReportBuilder
  - JSON results file with --from-cache support

Usage:
    cd digitalmodel
    PYTHONPATH=examples/demos/gtm:src uv run python \\
        examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py

    # Reuse cached results:
    PYTHONPATH=examples/demos/gtm:src uv run python \\
        examples/demos/gtm/demo_05_deepwater_rigid_jumper_installation.py --from-cache
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

# Physical constants
SEAWATER_DENSITY = 1025.0   # kg/m³
GRAVITY = 9.80665           # m/s²
STEEL_DENSITY = 7850.0      # kg/m³

# Parameter matrix
WATER_DEPTHS = [500, 1000, 1500, 2000, 2500, 3000]  # m
HS_VALUES = [1.0, 1.5, 2.0, 2.5, 3.0]               # m
REFERENCE_HS = 2.0                                     # m — for heatmap

# Phase-specific constants
DAF_LIFT = 1.10               # Dynamic amplification factor for lift-off
DAF_SPLASH = 1.30             # Dynamic amplification factor for splash zone
RIGGING_MASS_KG = 5000.0      # Rigging mass (5 te)
CS_PIPE = math.pi             # Slamming coefficient for circular cross-section
CD_CYLINDER = 1.2             # Drag coefficient for cylinder
CA_CYLINDER = 1.0             # Added mass coefficient for cylinder
V_LOWERING = 0.5              # Lowering velocity (m/s)
V_CURRENT = 0.5               # Typical deepwater current velocity (m/s)
SPLASH_SUBMERGED_LENGTH = 10.0  # Partially submerged length in splash zone (m)
WIRE_ALLOWABLE_FACTOR = 0.85  # Wire MBL utilisation limit
SMYS_X65 = 448e6              # Pa — Specified Minimum Yield Strength
E_STEEL = 207e9               # Pa — Young's modulus
BENDING_ALLOWABLE = 0.6       # Allowable fraction of SMYS for in-air bending
TIE_IN_TOLERANCE_MM = 50.0    # Lateral offset tolerance (mm)

# Cable properties (typical offshore installation wire rope)
CABLE_UNIT_WEIGHT_SUB = 50.0  # N/m submerged weight of wire rope


# ---------------------------------------------------------------------------
# Data loaders
# ---------------------------------------------------------------------------

def load_vessel_data() -> List[Dict[str, Any]]:
    """Load vessel data from csv_hlv_vessels.json."""
    path = DATA_DIR / "csv_hlv_vessels.json"
    with open(path, "r") as f:
        data = json.load(f)
    vessels = data["vessels"]
    print(f"  Loaded {len(vessels)} vessels from csv_hlv_vessels.json")
    return vessels


def load_jumper_data() -> Tuple[Dict[str, Any], List[Dict[str, Any]]]:
    """Load rigid jumper data from rigid_jumpers.json.

    Returns (common_properties, jumper_list).
    """
    path = DATA_DIR / "rigid_jumpers.json"
    with open(path, "r") as f:
        data = json.load(f)
    common = data["common_properties"]
    jumpers = data["jumpers"]
    print(f"  Loaded {len(jumpers)} jumper lengths from rigid_jumpers.json")
    return common, jumpers


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def hs_to_tp(hs: float) -> float:
    """Approximate peak spectral period from Hs using Tp ≈ 4.0 × sqrt(Hs).

    This is a simplified relationship from typical JONSWAP spectra.
    """
    return 4.0 * math.sqrt(hs)


def utilisation_color(util: float) -> str:
    """Return hex color for utilisation: green / amber / red."""
    if util < 0.70:
        return COLORS["success"]
    elif util <= 0.90:
        return COLORS["warning"]
    else:
        return COLORS["danger"]


def status_from_utils(phase_utils: Dict[str, float]) -> str:
    """Determine GO / MARGINAL / NO_GO from phase utilisations."""
    max_util = max(phase_utils.values()) if phase_utils else 0.0
    if max_util > 1.0:
        return "NO_GO"
    elif max_util >= 0.85:
        return "MARGINAL"
    else:
        return "GO"


def governing_phase(phase_utils: Dict[str, float]) -> str:
    """Return the phase name with the highest utilisation."""
    if not phase_utils:
        return "N/A"
    return max(phase_utils, key=phase_utils.get)  # type: ignore[arg-type]


# ---------------------------------------------------------------------------
# Phase calculations
# ---------------------------------------------------------------------------

def calc_phase_1_liftoff(
    jumper: Dict[str, Any],
    common: Dict[str, Any],
    vessel: Dict[str, Any],
) -> Dict[str, Any]:
    """Phase 1: Lift-off — hook load vs crane SWL.

    hook_load = mass_total × g × DAF_lift
    mass_total = mass_per_m × length + rigging_mass
    """
    mass_per_m = common["mass_per_meter"]["total_air_kg_per_m"]
    length = jumper["length_m"]
    mass_total = mass_per_m * length + RIGGING_MASS_KG

    hook_load_n = mass_total * GRAVITY * DAF_LIFT
    hook_load_te = hook_load_n / (GRAVITY * 1000.0)  # convert to tonnes

    crane_swl_te = vessel["crane_main"]["swl_max_te"]
    utilisation = hook_load_te / crane_swl_te

    return {
        "phase": "lift_off",
        "mass_total_kg": round(mass_total, 1),
        "hook_load_te": round(hook_load_te, 2),
        "crane_swl_te": crane_swl_te,
        "daf": DAF_LIFT,
        "utilisation": round(utilisation, 4),
        "status": "PASS" if utilisation <= 1.0 else "FAIL",
    }


def calc_phase_2_inair_bending(
    jumper: Dict[str, Any],
    common: Dict[str, Any],
) -> Dict[str, Any]:
    """Phase 2: In-air bending stress during lift.

    Model jumper as simply-supported beam with uniform self-weight.
    M = w × L² / 8
    σ = M × (OD/2) / I
    Check: σ ≤ 0.6 × SMYS
    """
    w = common["weight_per_meter"]["air_n_per_m"]  # N/m
    length = jumper["length_m"]
    od = common["od_m"]
    id_m = common["id_m"]
    i_steel = common["i_steel_m4"]
    smys = common["smys_pa"]

    # Max bending moment at midspan
    m_max = w * length**2 / 8.0

    # Bending stress
    sigma = m_max * (od / 2.0) / i_steel

    # Allowable
    allowable = BENDING_ALLOWABLE * smys
    utilisation = sigma / allowable

    return {
        "phase": "in_air_bending",
        "weight_per_m_n": round(w, 2),
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
) -> Dict[str, Any]:
    """Phase 3: Splash zone — slamming + drag + DAF.

    Slamming: F_slam = 0.5 × ρ × Cs × Ap × v_rel²
    Drag: F_drag = 0.5 × ρ × Cd × A_side × v_lowering²
    """
    od = common["od_m"]
    length = jumper["length_m"]
    mass_per_m = common["mass_per_meter"]["total_air_kg_per_m"]
    mass_total = mass_per_m * length + RIGGING_MASS_KG
    w_air = mass_total * GRAVITY

    # Splash zone submerged length
    submerged_len = min(length, SPLASH_SUBMERGED_LENGTH)

    # Projected area for slamming
    ap = od * submerged_len

    # Wave velocity (simplified): v_wave = π × Hs / Tp
    tp = hs_to_tp(hs)
    v_wave = math.pi * hs / tp

    # Relative velocity
    v_rel = V_LOWERING + v_wave

    # Slamming force
    f_slam = 0.5 * SEAWATER_DENSITY * CS_PIPE * ap * v_rel**2

    # Drag force on submerged length
    a_side = od * submerged_len
    f_drag = 0.5 * SEAWATER_DENSITY * CD_CYLINDER * a_side * V_LOWERING**2

    # Total hook load in splash
    hook_splash_n = w_air + f_slam + f_drag
    hook_splash_daf_n = hook_splash_n * DAF_SPLASH
    hook_splash_te = hook_splash_daf_n / (GRAVITY * 1000.0)

    crane_swl_te = vessel["crane_main"]["swl_max_te"]
    utilisation = hook_splash_te / crane_swl_te

    return {
        "phase": "splash_zone",
        "slamming_force_kn": round(f_slam / 1000.0, 2),
        "drag_force_kn": round(f_drag / 1000.0, 2),
        "hook_load_splash_te": round(hook_splash_te, 2),
        "crane_swl_te": crane_swl_te,
        "daf": DAF_SPLASH,
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
) -> Dict[str, Any]:
    """Phase 4: Lowering through water column — cable tension + snap load.

    Cable tension: T = W_sub + W_cable_sub(depth)
    Snap load: dynamic = (mass_total + added_mass) × ω² × heave_amp
    Check: max_cable_tension ≤ 0.85 × wire_MBL
    """
    od = common["od_m"]
    length = jumper["length_m"]
    sub_w_per_m = common["weight_per_meter"]["submerged_n_per_m"]
    mass_per_m = common["mass_per_meter"]["total_air_kg_per_m"]

    # Static loads
    w_sub = sub_w_per_m * length
    w_cable_sub = CABLE_UNIT_WEIGHT_SUB * depth
    static_tension = w_sub + w_cable_sub

    # Dynamic loads
    mass_total = mass_per_m * length + RIGGING_MASS_KG
    added_mass = SEAWATER_DENSITY * math.pi / 4.0 * od**2 * length * CA_CYLINDER

    tp = hs_to_tp(hs)
    omega = 2.0 * math.pi / tp

    heave_rao = vessel["motion_characteristics"]["heave_rao"]["peak_amplitude_m_per_m"]
    heave_amp = heave_rao * (hs / 2.0)

    dynamic_load = (mass_total + added_mass) * omega**2 * heave_amp

    max_tension = static_tension + dynamic_load
    max_tension_te = max_tension / (GRAVITY * 1000.0)

    wire_mbl_te = vessel["crane_main"]["main_wire_mbl_te"]
    allowable_te = WIRE_ALLOWABLE_FACTOR * wire_mbl_te
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
) -> Dict[str, Any]:
    """Phase 5: Tie-in alignment — current-induced lateral deflection.

    F_current = 0.5 × ρ × Cd × OD × length × v_current²
    δ = F_current × length³ / (48 × E × I) — fixed-fixed approx
    Check: δ ≤ 50 mm
    """
    od = common["od_m"]
    length = jumper["length_m"]
    i_steel = common["i_steel_m4"]

    f_current = 0.5 * SEAWATER_DENSITY * CD_CYLINDER * od * length * V_CURRENT**2

    # Deflection (fixed-fixed beam with central point load approximation)
    delta = f_current * length**3 / (48.0 * E_STEEL * i_steel)
    delta_mm = delta * 1000.0

    utilisation = delta_mm / TIE_IN_TOLERANCE_MM

    return {
        "phase": "tie_in",
        "current_force_kn": round(f_current / 1000.0, 4),
        "deflection_mm": round(delta_mm, 2),
        "tolerance_mm": TIE_IN_TOLERANCE_MM,
        "utilisation": round(utilisation, 4),
        "status": "PASS" if utilisation <= 1.0 else "FAIL",
    }


# ---------------------------------------------------------------------------
# Single case runner
# ---------------------------------------------------------------------------

def run_single_case(
    vessel: Dict[str, Any],
    jumper: Dict[str, Any],
    common: Dict[str, Any],
    depth: float,
    hs: float,
) -> Dict[str, Any]:
    """Run all 5 installation phases for a single parameter combination."""
    p1 = calc_phase_1_liftoff(jumper, common, vessel)
    p2 = calc_phase_2_inair_bending(jumper, common)
    p3 = calc_phase_3_splash(jumper, common, vessel, hs)
    p4 = calc_phase_4_lowering(jumper, common, vessel, depth, hs)
    p5 = calc_phase_5_tiein(jumper, common)

    phase_utils = {
        "lift_off": p1["utilisation"],
        "in_air_bending": p2["utilisation"],
        "splash_zone": p3["utilisation"],
        "lowering": p4["utilisation"],
        "tie_in": p5["utilisation"],
    }

    overall = status_from_utils(phase_utils)
    gov_phase = governing_phase(phase_utils)
    max_util = max(phase_utils.values())

    return {
        "vessel": vessel["name"],
        "vessel_id": vessel["id"],
        "jumper_id": jumper["id"],
        "jumper_name": jumper["name"],
        "length_m": jumper["length_m"],
        "water_depth_m": depth,
        "hs_m": hs,
        "tp_s": round(hs_to_tp(hs), 2),
        "phases": {
            "lift_off": p1,
            "in_air_bending": p2,
            "splash_zone": p3,
            "lowering": p4,
            "tie_in": p5,
        },
        "phase_utilisations": phase_utils,
        "overall_status": overall,
        "governing_phase": gov_phase,
        "max_utilisation": round(max_util, 4),
    }


# ---------------------------------------------------------------------------
# Parametric sweep
# ---------------------------------------------------------------------------

def run_parametric_sweep(
    vessels: List[Dict],
    common: Dict[str, Any],
    jumpers: List[Dict],
) -> Tuple[List[Dict], pd.DataFrame]:
    """Run the full 300-case parametric sweep."""
    total = len(vessels) * len(WATER_DEPTHS) * len(jumpers) * len(HS_VALUES)

    print(f"\n{'='*60}")
    print(f"  PARAMETRIC JUMPER INSTALLATION SWEEP")
    print(f"  {total} cases: {len(vessels)} vessels x {len(WATER_DEPTHS)} depths"
          f" x {len(jumpers)} jumpers x {len(HS_VALUES)} Hs")
    print(f"{'='*60}\n")

    all_results: List[Dict] = []
    case_num = 0

    for vessel in vessels:
        for jumper in jumpers:
            for depth in WATER_DEPTHS:
                for hs in HS_VALUES:
                    case_num += 1
                    try:
                        result = run_single_case(vessel, jumper, common, depth, hs)
                        all_results.append(result)

                        if case_num % 50 == 0 or case_num == total:
                            print(f"  Case {case_num:3d}/{total} | "
                                  f"{vessel['name']:>12s} | {jumper['name']:>12s} | "
                                  f"d={depth}m Hs={hs}m | "
                                  f"{result['overall_status']:>8s} "
                                  f"(util={result['max_utilisation']:.3f})")
                    except Exception as exc:
                        logger.warning("Case %d failed: %s", case_num, exc)
                        all_results.append({
                            "vessel": vessel["name"],
                            "jumper_name": jumper["name"],
                            "length_m": jumper["length_m"],
                            "water_depth_m": depth,
                            "hs_m": hs,
                            "overall_status": "ERROR",
                            "max_utilisation": None,
                            "governing_phase": str(exc),
                        })

    # Build DataFrame for downstream use
    rows = []
    for r in all_results:
        rows.append({
            "Vessel": r.get("vessel", ""),
            "Jumper": r.get("jumper_name", ""),
            "Length (m)": r.get("length_m", 0),
            "Depth (m)": r.get("water_depth_m", 0),
            "Hs (m)": r.get("hs_m", 0),
            "Status": r.get("overall_status", "ERROR"),
            "Max Util": r.get("max_utilisation", None),
            "Governing Phase": r.get("governing_phase", "N/A"),
        })

    df = pd.DataFrame(rows)
    print(f"\n  Sweep complete: {len(all_results)} results collected")
    return all_results, df


# ---------------------------------------------------------------------------
# Chart 1: Go/No-Go Heatmap (HERO)
# ---------------------------------------------------------------------------

def build_chart_1_heatmap(all_results: List[Dict], vessels: List[Dict]) -> go.Figure:
    """Two-subplot heatmap: Large CSV | Medium CSV.

    X: water depth, Y: jumper length.
    Cell color: GO/MARGINAL/NO_GO at Hs=2.0m reference.
    Annotation: governing phase + utilisation %.
    """
    print("\n[Chart 1] Building Go/No-Go Heatmap...")

    # Filter to reference Hs
    ref_results = [r for r in all_results if abs(r.get("hs_m", 0) - REFERENCE_HS) < 0.01]

    vessel_names = [v["name"] for v in vessels]
    depths = sorted(WATER_DEPTHS)
    lengths = sorted(set(r["length_m"] for r in ref_results))

    fig = make_subplots(
        rows=1, cols=2,
        subplot_titles=[f"{vn}" for vn in vessel_names],
        horizontal_spacing=0.08,
    )

    status_to_val = {"GO": 0, "MARGINAL": 1, "NO_GO": 2, "ERROR": 2}

    for col_idx, vname in enumerate(vessel_names, 1):
        v_results = [r for r in ref_results if r.get("vessel") == vname]

        # Build matrix
        z_matrix = []
        annotations_text = []
        for length in lengths:
            z_row = []
            ann_row = []
            for depth in depths:
                match = [r for r in v_results
                         if r["length_m"] == length and r["water_depth_m"] == depth]
                if match:
                    r = match[0]
                    z_row.append(status_to_val.get(r["overall_status"], 2))
                    gov = r.get("governing_phase", "N/A")
                    gov_short = {
                        "lift_off": "Lift",
                        "in_air_bending": "Bend",
                        "splash_zone": "Splash",
                        "lowering": "Lower",
                        "tie_in": "Tie-in",
                    }.get(gov, gov[:6])
                    util_pct = r.get("max_utilisation", 0) * 100
                    ann_row.append(f"{gov_short}<br>{util_pct:.0f}%")
                else:
                    z_row.append(2)
                    ann_row.append("N/A")
            z_matrix.append(z_row)
            annotations_text.append(ann_row)

        # Custom colorscale: GO=green, MARGINAL=amber, NO_GO=red
        colorscale = [
            [0.0, COLORS["success"]],
            [0.33, COLORS["success"]],
            [0.34, COLORS["warning"]],
            [0.66, COLORS["warning"]],
            [0.67, COLORS["danger"]],
            [1.0, COLORS["danger"]],
        ]

        fig.add_trace(
            go.Heatmap(
                z=z_matrix,
                x=[f"{d}m" for d in depths],
                y=[f"{int(l)}m" for l in lengths],
                colorscale=colorscale,
                zmin=0,
                zmax=2,
                showscale=False,
                text=annotations_text,
                texttemplate="%{text}",
                textfont=dict(size=10, color="white"),
                hovertemplate=(
                    "Depth: %{x}<br>"
                    "Length: %{y}<br>"
                    "%{text}<extra></extra>"
                ),
            ),
            row=1, col=col_idx,
        )

    fig.update_layout(
        title=dict(
            text=f"Go/No-Go Matrix at Hs = {REFERENCE_HS} m",
            font=dict(size=18),
        ),
        height=450,
    )
    fig.update_xaxes(title_text="Water Depth", row=1, col=1)
    fig.update_xaxes(title_text="Water Depth", row=1, col=2)
    fig.update_yaxes(title_text="Jumper Length", row=1, col=1)

    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 2: In-air Bending Stress vs Jumper Length
# ---------------------------------------------------------------------------

def build_chart_2_bending(
    common: Dict[str, Any],
    jumpers: List[Dict],
) -> go.Figure:
    """X: jumper length (m), Y: bending stress (MPa).
    Reference line at 0.6×SMYS allowable.
    """
    print("\n[Chart 2] Building In-air Bending Stress chart...")

    lengths = []
    stresses = []
    for j in jumpers:
        p2 = calc_phase_2_inair_bending(j, common)
        lengths.append(j["length_m"])
        stresses.append(p2["bending_stress_mpa"])

    allowable_mpa = BENDING_ALLOWABLE * SMYS_X65 / 1e6

    fig = go.Figure()

    fig.add_trace(go.Scatter(
        x=lengths,
        y=stresses,
        mode="lines+markers",
        name="Bending Stress",
        line=dict(color=CHART_PALETTE[0], width=3),
        marker=dict(size=10),
        hovertemplate="Length: %{x}m<br>Stress: %{y:.1f} MPa<extra></extra>",
    ))

    fig.add_hline(
        y=allowable_mpa,
        line_dash="dash",
        line_color=COLORS["danger"],
        line_width=2,
        annotation_text=f"0.6 x SMYS = {allowable_mpa:.0f} MPa",
        annotation_position="top left",
    )

    # Shade pass/fail regions
    fig.add_hrect(
        y0=0, y1=allowable_mpa,
        fillcolor=COLORS["success"],
        opacity=0.08,
        line_width=0,
    )
    fig.add_hrect(
        y0=allowable_mpa, y1=max(stresses) * 1.2 if max(stresses) > allowable_mpa else allowable_mpa * 1.3,
        fillcolor=COLORS["danger"],
        opacity=0.08,
        line_width=0,
    )

    fig.update_layout(
        title=dict(text="In-Air Bending Stress vs Jumper Length", font=dict(size=18)),
        xaxis_title="Jumper Length (m)",
        yaxis_title="Max Bending Stress (MPa)",
        height=500,
    )

    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 3: Max Hs Limit vs Jumper Length
# ---------------------------------------------------------------------------

def build_chart_3_max_hs(
    all_results: List[Dict],
    vessels: List[Dict],
    jumpers: List[Dict],
) -> go.Figure:
    """X: jumper length, Y: max allowable Hs. One curve per vessel.

    For each vessel and jumper length, find the max Hs where the case is
    still GO or MARGINAL at the reference depth (1500m).
    """
    print("\n[Chart 3] Building Max Hs Limit chart...")

    ref_depth = 1500  # m — use a mid-range depth

    fig = go.Figure()
    colors = [CHART_PALETTE[0], CHART_PALETTE[1]]

    for v_idx, vessel in enumerate(vessels):
        vname = vessel["name"]
        lengths = []
        max_hs_vals = []

        for jumper in jumpers:
            length = jumper["length_m"]
            # Find max Hs where status is not NO_GO at this depth
            best_hs = 0.0
            for r in all_results:
                if (r.get("vessel") == vname
                        and r.get("length_m") == length
                        and r.get("water_depth_m") == ref_depth
                        and r.get("overall_status") in ("GO", "MARGINAL")
                        and r.get("hs_m", 0) > best_hs):
                    best_hs = r["hs_m"]

            lengths.append(length)
            max_hs_vals.append(best_hs)

        fig.add_trace(go.Scatter(
            x=lengths,
            y=max_hs_vals,
            mode="lines+markers",
            name=vname,
            line=dict(color=colors[v_idx], width=3),
            marker=dict(size=10),
            hovertemplate=f"{vname}<br>Length: %{{x}}m<br>Max Hs: %{{y:.1f}}m<extra></extra>",
        ))

    # Reference line at operational limit
    fig.add_hline(
        y=2.0, line_dash="dot", line_color=COLORS["warning"],
        line_width=1.5,
        annotation_text="Typical Hs=2.0m reference",
    )

    fig.update_layout(
        title=dict(
            text=f"Operability Envelope — Max Allowable Hs at {ref_depth}m Depth",
            font=dict(size=18),
        ),
        xaxis_title="Jumper Length (m)",
        yaxis_title="Max Allowable Hs (m)",
        height=500,
    )

    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 4: Cable Tension vs Water Depth
# ---------------------------------------------------------------------------

def build_chart_4_cable_tension(
    all_results: List[Dict],
    vessels: List[Dict],
    jumpers: List[Dict],
) -> go.Figure:
    """X: depth, Y: cable tension (te). Lines per jumper length.

    Use the Large CSV vessel at reference Hs.
    """
    print("\n[Chart 4] Building Cable Tension chart...")

    large_vessel = vessels[0]  # Large CSV
    vname = large_vessel["name"]
    wire_mbl_te = large_vessel["crane_main"]["main_wire_mbl_te"]
    allowable_te = WIRE_ALLOWABLE_FACTOR * wire_mbl_te

    fig = go.Figure()

    for j_idx, jumper in enumerate(jumpers):
        length = jumper["length_m"]
        depths_plot = []
        tensions = []

        for depth in WATER_DEPTHS:
            match = [r for r in all_results
                     if r.get("vessel") == vname
                     and r.get("length_m") == length
                     and r.get("water_depth_m") == depth
                     and abs(r.get("hs_m", 0) - REFERENCE_HS) < 0.01]
            if match:
                r = match[0]
                t = r.get("phases", {}).get("lowering", {}).get("max_tension_te", 0)
                depths_plot.append(depth)
                tensions.append(t)

        fig.add_trace(go.Scatter(
            x=depths_plot,
            y=tensions,
            mode="lines+markers",
            name=f"{int(length)}m jumper",
            line=dict(color=CHART_PALETTE[j_idx % len(CHART_PALETTE)], width=2),
            marker=dict(size=7),
            hovertemplate=f"{int(length)}m<br>Depth: %{{x}}m<br>Tension: %{{y:.1f}} te<extra></extra>",
        ))

    # Wire MBL reference
    fig.add_hline(
        y=allowable_te,
        line_dash="dash",
        line_color=COLORS["danger"],
        line_width=2,
        annotation_text=f"0.85 x Wire MBL = {allowable_te:.0f} te",
        annotation_position="top left",
    )

    fig.update_layout(
        title=dict(
            text=f"Cable Tension vs Water Depth — {vname} at Hs={REFERENCE_HS}m",
            font=dict(size=18),
        ),
        xaxis_title="Water Depth (m)",
        yaxis_title="Max Cable Tension (te)",
        height=500,
    )

    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 5: Vessel Comparison — Max Installable Length
# ---------------------------------------------------------------------------

def build_chart_5_vessel_comparison(
    all_results: List[Dict],
    vessels: List[Dict],
    jumpers: List[Dict],
) -> go.Figure:
    """X: water depth, Y: max jumper length installable. One bar per vessel.

    At reference Hs=2.0m.
    """
    print("\n[Chart 5] Building Vessel Comparison chart...")

    ref_results = [r for r in all_results if abs(r.get("hs_m", 0) - REFERENCE_HS) < 0.01]
    vessel_names = [v["name"] for v in vessels]
    all_lengths = sorted(set(j["length_m"] for j in jumpers))

    fig = go.Figure()
    colors = [CHART_PALETTE[0], CHART_PALETTE[1]]

    for v_idx, vname in enumerate(vessel_names):
        depths_plot = []
        max_lengths = []

        for depth in WATER_DEPTHS:
            # Find max length where status is GO or MARGINAL
            best_length = 0.0
            for r in ref_results:
                if (r.get("vessel") == vname
                        and r.get("water_depth_m") == depth
                        and r.get("overall_status") in ("GO", "MARGINAL")
                        and r.get("length_m", 0) > best_length):
                    best_length = r["length_m"]

            depths_plot.append(depth)
            max_lengths.append(best_length)

        fig.add_trace(go.Bar(
            x=[f"{d}m" for d in depths_plot],
            y=max_lengths,
            name=vname,
            marker_color=colors[v_idx],
            hovertemplate=f"{vname}<br>Depth: %{{x}}<br>Max Length: %{{y:.0f}}m<extra></extra>",
        ))

    fig.update_layout(
        title=dict(
            text=f"Max Installable Jumper Length by Vessel at Hs={REFERENCE_HS}m",
            font=dict(size=18),
        ),
        xaxis_title="Water Depth",
        yaxis_title="Max Installable Jumper Length (m)",
        barmode="group",
        height=500,
    )

    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Summary table
# ---------------------------------------------------------------------------

def build_summary_table(all_results: List[Dict]) -> pd.DataFrame:
    """Build summary DataFrame: vessel × jumper length → status at reference Hs + 1500m."""
    ref_depth = 1500
    rows = []
    for r in all_results:
        if (abs(r.get("hs_m", 0) - REFERENCE_HS) < 0.01
                and r.get("water_depth_m") == ref_depth):
            gov = r.get("governing_phase", "N/A")
            gov_display = {
                "lift_off": "Lift-off",
                "in_air_bending": "In-air Bending",
                "splash_zone": "Splash Zone",
                "lowering": "Lowering",
                "tie_in": "Tie-in",
            }.get(gov, gov)
            rows.append({
                "Vessel": r.get("vessel", ""),
                "Jumper": r.get("jumper_name", ""),
                "Length (m)": r.get("length_m", 0),
                "Depth (m)": ref_depth,
                "Hs (m)": REFERENCE_HS,
                "Status": r.get("overall_status", "ERROR"),
                "Max Util": round(r.get("max_utilisation", 0), 3),
                "Governing Phase": gov_display,
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
        title="Deepwater Rigid Jumper Installation Analysis",
        subtitle=(
            f"{total_cases} parametric cases across 2 vessels, "
            f"6 water depths, 5 jumper lengths, and 5 sea states"
        ),
        demo_id="demo_05",
        case_count=total_cases,
        code_refs=[
            "DNV-RP-H103 (2011) — Modelling and Analysis of Marine Operations",
            "DNV-ST-N001 (2021) — Marine Operations and Marine Warranty",
            "DNV-OS-F101 (2021) — Submarine Pipeline Systems (pipe stress limits)",
            "API 5L (2018) — Line Pipe (X65 material properties)",
        ],
    )

    # Methodology section
    methodology_html = """
    <p>This analysis screens deepwater rigid jumper installation feasibility
    across a parametric matrix of vessels, water depths, jumper lengths, and
    sea states. Each case evaluates five installation phases:</p>

    <h3>Phase 1: Lift-off</h3>
    <p>Verifies the crane can lift the jumper plus rigging (5 te) with a
    <strong>DAF of 1.10</strong>. Hook load = (mass_per_m &times; length + rigging)
    &times; g &times; DAF. Checked against crane SWL at maximum radius.</p>

    <h3>Phase 2: In-air Bending (Jumper-Specific)</h3>
    <p>Rigid jumpers are long and slender — bending stress during lift is often the
    governing phase. The jumper is modelled as a <strong>simply-supported beam</strong>
    with uniform self-weight: M = wL&sup2;/8, &sigma; = M&times;(OD/2)/I. Stress must
    not exceed <strong>0.6 &times; SMYS</strong> (268.8 MPa for X65).</p>

    <h3>Phase 3: Splash Zone</h3>
    <p>Hydrodynamic loads during passage through the air-water interface.
    Slamming force uses C<sub>s</sub> = &pi; for circular pipe sections.
    Combined with drag and a <strong>DAF of 1.30</strong> for splash zone dynamics.</p>

    <h3>Phase 4: Lowering Through Water Column</h3>
    <p>Cable tension = submerged weight + cable weight + dynamic snap load.
    Dynamic load accounts for heave RAO and added mass (C<sub>a</sub> = 1.0).
    Checked against <strong>85% of wire MBL</strong>.</p>

    <h3>Phase 5: Tie-in Alignment (Jumper-Specific)</h3>
    <p>After landing, the jumper must align to hub connectors within &plusmn;50 mm.
    Current-induced lateral deflection is estimated using fixed-fixed beam theory
    with a deepwater current of 0.5 m/s.</p>

    <h3>Go/No-Go Classification</h3>
    <ul>
        <li><strong>GO:</strong> all phases pass AND max utilisation &lt; 0.85</li>
        <li><strong>MARGINAL:</strong> all phases pass AND max utilisation in [0.85, 1.00]</li>
        <li><strong>NO_GO:</strong> any phase fails (utilisation &gt; 1.00)</li>
    </ul>
    """
    report.add_methodology(methodology_html)

    # Charts
    report.add_chart(
        "gono_heatmap",
        fig1,
        title="Chart 1: Go/No-Go Installation Matrix",
        subtitle=(
            f"At Hs = {REFERENCE_HS} m. Cell shows governing phase and utilisation. "
            "Green = GO, Amber = MARGINAL, Red = NO_GO."
        ),
    )

    report.add_chart(
        "bending_stress",
        fig2,
        title="Chart 2: In-Air Bending Stress vs Jumper Length",
        subtitle="Simply-supported beam model. Red dashed line = 0.6 x SMYS allowable (268.8 MPa).",
    )

    report.add_chart(
        "max_hs_limit",
        fig3,
        title="Chart 3: Operability Envelope — Max Allowable Hs",
        subtitle="Maximum sea state for each jumper length at 1500 m water depth. One curve per vessel.",
    )

    report.add_chart(
        "cable_tension",
        fig4,
        title="Chart 4: Cable Tension vs Water Depth",
        subtitle=f"Large CSV at Hs = {REFERENCE_HS} m. Lines per jumper length. Red = 85% wire MBL.",
    )

    report.add_chart(
        "vessel_comparison",
        fig5,
        title="Chart 5: Vessel Comparison — Max Installable Jumper Length",
        subtitle=f"At Hs = {REFERENCE_HS} m. Grouped by water depth.",
    )

    # Summary table
    report.add_table(
        "Summary: Installation Screening at 1500m Depth, Hs=2.0m",
        summary_df,
        subtitle="Parametric results for each vessel and jumper combination at reference conditions",
        status_col="Status",
    )

    # Live mode teaser
    report.add_live_mode_teaser(
        analysis_type="jumper installation screening"
    )

    # Assumptions
    report.add_assumptions([
        "All jumpers are 8\" OD (219.1 mm) API 5L X65 with 18.26 mm wall thickness and FBE coating",
        "SMYS = 448 MPa, Young's modulus E = 207 GPa",
        "Rigging mass = 5.0 te (slings, shackles, spreader bar)",
        "In-air bending modelled as simply-supported beam — conservative for typical dual-crane lifts",
        "Slamming coefficient Cs = pi for circular cross-section (DNV-RP-H103 Table 8-2)",
        "Drag coefficient Cd = 1.2 for cylinder (DNV-RP-H103)",
        "Added mass coefficient Ca = 1.0 for cylinder",
        "Lowering velocity = 0.5 m/s, typical for heavy-lift CSV operations",
        "Wire rope submerged weight = 50 N/m — representative of 96-128 mm diameter wire",
        "Deepwater current velocity = 0.5 m/s for tie-in deflection check",
        "Tie-in lateral tolerance = +/-50 mm (typical hub connector specification)",
        "Tp estimated from Hs via Tp = 4.0 x sqrt(Hs) — simplified JONSWAP relationship",
        "Vessel RAOs are simplified single-peak values, not full frequency-dependent transfer functions",
        "No wave-current interaction or VIV effects during lowering",
        "No seabed slope or landing zone geotechnical effects considered",
    ])

    # Build and save
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    output_path = OUTPUT_DIR / "demo_05_jumper_installation_report.html"
    html = report.build(output_path)

    print(f"  Report saved to: {output_path}")
    return html


# ---------------------------------------------------------------------------
# JSON cache / save
# ---------------------------------------------------------------------------

def save_json_results(
    all_results: List[Dict],
    summary_df: pd.DataFrame,
    common: Dict[str, Any],
) -> Path:
    """Save results to JSON for --from-cache reuse."""
    RESULTS_DIR.mkdir(parents=True, exist_ok=True)
    results_path = RESULTS_DIR / "demo_05_jumper_installation_results.json"

    json_output = {
        "metadata": {
            "demo": "GTM Demo 5: Deepwater Rigid Jumper Installation Analysis",
            "total_cases": len(all_results),
            "vessels": ["Large CSV (5000te)", "Medium CSV (2500te)"],
            "water_depths_m": WATER_DEPTHS,
            "hs_values_m": HS_VALUES,
            "jumper_lengths_m": [20, 40, 60, 80, 100],
            "jumper_od_mm": common["od_mm"],
            "jumper_wt_mm": common["wt_mm"],
            "grade": common["grade"],
            "smys_mpa": common["smys_mpa"],
            "reference_hs_m": REFERENCE_HS,
            "codes": [
                "DNV-RP-H103 (2011)",
                "DNV-ST-N001 (2021)",
                "DNV-OS-F101 (2021)",
            ],
        },
        "summary": summary_df.to_dict(orient="records"),
        "results": all_results,
    }

    with open(results_path, "w") as f:
        json.dump(json_output, f, indent=2, default=str)

    print(f"  Results saved to: {results_path}")
    return results_path


def load_cached_results(
    common: Dict[str, Any],
) -> Tuple[List[Dict], pd.DataFrame]:
    """Load previously-saved JSON results."""
    results_path = RESULTS_DIR / "demo_05_jumper_installation_results.json"
    if not results_path.exists():
        print(f"  [ERROR] No cached results at {results_path}")
        sys.exit(1)

    with open(results_path, "r") as f:
        data = json.load(f)

    all_results = data["results"]
    summary_df = pd.DataFrame(data["summary"])
    print(f"  Loaded {len(all_results)} cached results from {results_path}")
    return all_results, summary_df


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    """Run the full Demo 5 pipeline."""
    parser = argparse.ArgumentParser(
        description="GTM Demo 5: Deepwater Rigid Jumper Installation Analysis",
    )
    parser.add_argument(
        "--from-cache",
        action="store_true",
        help="Skip calculations and reload results from previous run",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="Force recalculation even if cache exists",
    )
    args = parser.parse_args()

    start_time = time.time()

    print("=" * 60)
    print("  GTM Demo 5: Deepwater Rigid Jumper Installation Analysis")
    print("=" * 60)

    # [1/7] Load data files
    print("\n[1/7] Loading input data...")
    vessels = load_vessel_data()
    common, jumpers = load_jumper_data()

    # [2/7] Run parametric sweep (or load cache)
    if args.from_cache and not args.force:
        print("\n[2/7] Loading cached results...")
        all_results, summary_df = load_cached_results(common)
    else:
        print("\n[2/7] Running parametric sweep...")
        all_results, _ = run_parametric_sweep(vessels, common, jumpers)

        # Build summary after sweep
        summary_df = build_summary_table(all_results)

    total_cases = len(all_results)

    # [3/7] Build charts
    print("\n[3/7] Building charts...")
    fig1 = build_chart_1_heatmap(all_results, vessels)
    fig2 = build_chart_2_bending(common, jumpers)
    fig3 = build_chart_3_max_hs(all_results, vessels, jumpers)
    fig4 = build_chart_4_cable_tension(all_results, vessels, jumpers)
    fig5 = build_chart_5_vessel_comparison(all_results, vessels, jumpers)

    # [4/7] Build summary table
    print("\n[4/7] Summary table...")
    print(summary_df.to_string(index=False))

    # [5/7] Build HTML report
    print("\n[5/7] Building HTML report...")
    build_report(fig1, fig2, fig3, fig4, fig5, summary_df, all_results, total_cases)

    # [6/7] Save JSON results
    print("\n[6/7] Saving JSON results...")
    save_json_results(all_results, summary_df, common)

    # [7/7] Done
    elapsed = time.time() - start_time
    print(f"\n[7/7] Complete!")
    print(f"{'='*60}")
    print(f"  Total cases analysed:  {total_cases}")
    print(f"  HTML report:           output/demo_05_jumper_installation_report.html")
    print(f"  JSON results:          results/demo_05_jumper_installation_results.json")
    print(f"  Time elapsed:          {elapsed:.1f} seconds")
    print(f"{'='*60}")


if __name__ == "__main__":
    main()
