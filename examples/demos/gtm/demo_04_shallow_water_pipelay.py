#!/usr/bin/env python3
# ABOUTME: GTM Demo 4 — Shallow Water Pipeline Installation Analysis
# ABOUTME: S-lay catenary analysis: 2 vessels × 5 pipe sizes × 6 depths = 60 cases
"""
GTM Demo 4: Shallow Water Pipeline Installation Analysis
==========================================================

Runs parametric S-lay installation screening across:
  - 2 vessels (Large PLV 600te, Shallow Water Barge 250te)
  - 5 pipe sizes (8", 12", 16", 20", 24") — all X65
  - 6 water depths (7, 10, 15, 20, 25, 30 m)
  - Total: 2 × 5 × 6 = 60 cases

Self-contained S-lay catenary mechanics — no external pipeline modules.
Evaluates overbend, sagbend, top tension, and stinger departure angle.

Produces:
  - 5 interactive Plotly charts
  - Branded HTML report via GTMReportBuilder
  - JSON results file with --from-cache support

Usage:
    cd digitalmodel
    PYTHONPATH=examples/demos/gtm:src uv run python \\
        examples/demos/gtm/demo_04_shallow_water_pipelay.py

    # Re-generate charts from cached results:
    PYTHONPATH=examples/demos/gtm:src uv run python \\
        examples/demos/gtm/demo_04_shallow_water_pipelay.py --from-cache

    # Force recalculation even if cache exists:
    PYTHONPATH=examples/demos/gtm:src uv run python \\
        examples/demos/gtm/demo_04_shallow_water_pipelay.py --force
"""

from __future__ import annotations

import argparse
import json
import logging
import math
import sys
import time
import warnings
from datetime import datetime, timezone
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
INPUTS_DIR = SCRIPT_DIR / "inputs"

# The committed Baseline Sweep Config — the COMPLETE source of truth (ADR-0002/0005).
BASELINE_CONFIG_PATH = INPUTS_DIR / "demo_04_pipelay.yml"

# Canonical run_id for the committed Baseline reference run in the Results Store
# (mirrors demo_03; matches results_store_demo04.DEMO_ID partition).
BASELINE_RUN_ID = "baseline"

# Demo identifier — the per-run report/store partition name (matches results_store_demo04.DEMO_ID).
DEMO_ID = "demo_04"


def validate_run_id(run_id: str) -> str:
    """Validate a run_id is a single safe path segment; return it unchanged if valid.

    A run_id becomes a filesystem directory name under the Results Store
    (<base_dir>/parametric/demo_04/<run_id>/) and the per-run report dir, so it must be a
    single safe path segment: no separators, no "." / ".." traversal, no empty string
    (path-traversal guard). Delegates to the store's regex so both stay in lock-step. Raises
    ValueError with a clean message on rejection.
    """
    try:
        import results_store_demo04 as _rs
    except ImportError:  # pragma: no cover — packaged import path fallback.
        from examples.demos.gtm import results_store_demo04 as _rs
    return _rs._validate_run_id(run_id)

# ---------------------------------------------------------------------------
# Module-default constants (legacy fallback when config is None — byte-identical
# to the committed baseline yaml values). The yaml is the LIVE source of truth on
# the compute path; these module globals are only the config=None fallback.
# ---------------------------------------------------------------------------

# Physical constants
SEAWATER_DENSITY = 1025.0       # kg/m3
GRAVITY = 9.80665               # m/s2
STEEL_DENSITY = 7850.0          # kg/m3
STEEL_YOUNGS_MODULUS = 207e9    # Pa (207 GPa)
SMYS_X65 = 448e6                # Pa — API 5L X65
SMTS_X65 = 531e6                # Pa — API 5L X65

# DNV-ST-F101 installation stress limit
STRESS_LIMIT_FACTOR = 0.72      # σ_allow = 0.72 × SMYS
TENSION_MARGIN = 1.10           # operational margin above absolute minimum tension
GO_THRESHOLD = 0.85             # max_util >= this (and all pass) => MARGINAL
NOGO_UTILISATION = 1.0          # max_util > this (or any check fails) => NO_GO
SAGBEND_STRESS_BASIS = "combined"  # 'combined' (default) | 'bending_only' (diagnostic)

# Display-only utilisation-colour thresholds (SEPARATE from the go/no-go logic bands).
UTIL_COLOR_GREEN_BELOW = 0.70
UTIL_COLOR_AMBER_BELOW = 0.90

# Parameter matrix
WATER_DEPTHS = [7, 10, 15, 20, 25, 30]  # metres — shallow water focus

# Pipe sizes to analyse: nominal_size -> target wall thickness (mm)
# Using standard/minimum WT for installation screening (worst-case bending stress).
# WT is ON the compute path (drives A_steel/axial and OD/WT/bending) — externalized to the yaml.
PIPE_SELECTION = {
    "8in":  8.18,   # Sch 40
    "12in": 9.53,   # Sch 40
    "16in": 9.53,   # Sch 30
    "20in": 9.53,   # Sch 20
    "24in": 9.53,   # Sch 20
}


# ---------------------------------------------------------------------------
# Config accessors — resolve a scalar from the config, else the module default.
# ---------------------------------------------------------------------------

def _cfg(config: Optional[Any], attr: str, default: Any) -> Any:
    """Return ``config.attr`` when *config* is supplied, else the module default."""
    return getattr(config, attr) if config is not None else default

# Display labels
PIPE_DISPLAY = {
    "8in": '8"',
    "12in": '12"',
    "16in": '16"',
    "20in": '20"',
    "24in": '24"',
}


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def utilisation_color(util: float, config: Optional[Any] = None) -> str:
    """Return hex color for utilisation: green / amber / red.

    These DISPLAY thresholds are deliberately SEPARATE from the go/no-go LOGIC bands
    (``go_threshold`` / ``nogo_utilisation``) — they must not be coupled (reviewer finding).
    """
    green_below = _cfg(config, "util_color_green_below", UTIL_COLOR_GREEN_BELOW)
    amber_below = _cfg(config, "util_color_amber_below", UTIL_COLOR_AMBER_BELOW)
    if util < green_below:
        return COLORS["success"]   # green
    elif util <= amber_below:
        return COLORS["warning"]   # amber
    else:
        return COLORS["danger"]    # red


def go_nogo_label(status: str) -> str:
    """Map status string to display label."""
    mapping = {"GO": "GO", "MARGINAL": "MARGINAL", "NO_GO": "NO-GO"}
    return mapping.get(status, status)


def pipe_label(nominal_size: str) -> str:
    """Return display label for a pipe size, e.g. '8\"'."""
    return PIPE_DISPLAY.get(nominal_size, nominal_size)


# ---------------------------------------------------------------------------
# Data loaders
# ---------------------------------------------------------------------------

def load_vessels(config: Optional[Any] = None) -> List[Dict[str, Any]]:
    """Load vessel data from the pipelay_vessels catalog.

    When *config* is supplied the catalog path comes from it; otherwise the module default.
    """
    path = config.vessels_path if config is not None else DATA_DIR / "pipelay_vessels.json"
    with open(path, "r") as f:
        data = json.load(f)
    return data["vessels"]


def load_pipes(config: Optional[Any] = None) -> List[Dict[str, Any]]:
    """Load pipeline data from the pipelines catalog and select target wall thicknesses.

    The pipe selection map (nominal_size -> target WT mm) is ON the compute path — WT drives
    A_steel/axial and OD/WT/bending. When *config* is supplied both the catalog path and the
    selection map come from it; otherwise the module defaults.
    """
    path = config.pipelines_path if config is not None else DATA_DIR / "pipelines.json"
    pipe_selection = config.pipe_selection if config is not None else PIPE_SELECTION
    with open(path, "r") as f:
        data = json.load(f)

    selected = []
    for pipe in data["pipes"]:
        nom = pipe["nominal_size"]
        if nom not in pipe_selection:
            continue

        target_wt_mm = pipe_selection[nom]

        # Find the wall thickness entry closest to target
        best_wt = None
        best_diff = float("inf")
        for wt_entry in pipe["wall_thicknesses"]:
            diff = abs(wt_entry["wt_mm"] - target_wt_mm)
            if diff < best_diff:
                best_diff = diff
                best_wt = wt_entry

        if best_wt is None:
            logger.warning("No wall thickness found for %s", nom)
            continue

        selected.append({
            "nominal_size": nom,
            "od_mm": pipe["od_mm"],
            "od_m": pipe["od_m"],
            "wt_mm": best_wt["wt_mm"],
            "wt_m": best_wt["wt_m"],
            "schedule": best_wt["schedule_approx"],
            "a_steel_m2": best_wt["a_steel_m2"],
            "submerged_weight_n_per_m": best_wt["submerged_weight_n_per_m"],
            "mass_total_kg_per_m": best_wt["mass_total_kg_per_m"],
            "od_coated_m": best_wt["outer_diameter_incl_coatings_m"],
            "coating": pipe["anti_corrosion_coating"],
            "cwc": pipe["concrete_weight_coating"],
        })

    # Sort by OD
    selected.sort(key=lambda p: p["od_mm"])
    return selected


def extract_vessel_params(vessel: Dict) -> Dict[str, Any]:
    """Extract the key pipelay parameters from the vessel JSON structure."""
    ps = vessel["pipelay_system"]
    stinger = ps["stinger"]
    tensioner = ps["tensioner"]
    depth_range = ps["water_depth_range"]

    return {
        "id": vessel["id"],
        "name": vessel["name"],
        "tensioner_capacity_te": tensioner["capacity_te"],
        "stinger_length_m": stinger["length_m"],
        "stinger_angle_min_deg": stinger["adjustable_angle_range_deg"][0],
        "stinger_angle_max_deg": stinger["adjustable_angle_range_deg"][1],
        "max_pipe_diameter_in": ps["pipe_capacity"]["max_diameter_in"],
        "min_depth_m": depth_range["min_m"],
        "max_depth_m": depth_range["max_m"],
    }


def vessel_band_specs(vessels: List[Dict]) -> List[Dict[str, Any]]:
    """Derive per-vessel display specs (stinger angle band + tensioner capacity) from the
    catalog, so the chart bands / capacity lines / methodology text are SINGLE-SOURCED from the
    vessel data rather than duplicated literals (reviewer finding).

    Returns one dict per vessel: {id, name, angle_min_deg, angle_max_deg, tensioner_capacity_te}.
    """
    specs = []
    for v in vessels:
        vp = extract_vessel_params(v)
        specs.append({
            "id": vp["id"],
            "name": vp["name"],
            "angle_min_deg": vp["stinger_angle_min_deg"],
            "angle_max_deg": vp["stinger_angle_max_deg"],
            "tensioner_capacity_te": vp["tensioner_capacity_te"],
        })
    return specs


# ---------------------------------------------------------------------------
# S-lay Catenary Mechanics (self-contained)
# ---------------------------------------------------------------------------

def calc_submerged_weight(pipe: Dict, config: Optional[Any] = None) -> float:
    """Return submerged weight per unit length in kN/m.

    Uses the pre-calculated value from the pipe catalog (N/m) and converts to kN/m.
    If the catalogued value is near zero or negative (near-neutral buoyancy for
    small uncoated pipes), fall back to a first-principles calculation.

    When *config* is supplied the seawater density + gravity come from it; otherwise defaults.
    """
    seawater_density = _cfg(config, "seawater_density_kg_m3", SEAWATER_DENSITY)
    gravity = _cfg(config, "gravity_m_s2", GRAVITY)

    w_sub_n = pipe["submerged_weight_n_per_m"]

    if w_sub_n > 1.0:
        return w_sub_n / 1000.0  # N/m -> kN/m

    # Fallback: first-principles
    od = pipe["od_m"]
    wt = pipe["wt_m"]
    id_m = od - 2.0 * wt
    a_steel = math.pi / 4.0 * (od**2 - id_m**2)
    od_coat = pipe["od_coated_m"]
    a_displaced = math.pi / 4.0 * od_coat**2

    mass_per_m = pipe["mass_total_kg_per_m"]
    buoyancy_per_m = seawater_density * a_displaced

    w_sub = (mass_per_m - buoyancy_per_m) * gravity  # N/m
    return max(w_sub / 1000.0, 0.01)  # kN/m, floor at 0.01 to avoid division issues


def calc_catenary(
    H: float,
    w_sub: float,
    depth: float,
) -> Dict[str, float]:
    """Solve the catenary geometry for given horizontal tension and water depth.

    Parameters
    ----------
    H : float
        Horizontal tension component (kN).
    w_sub : float
        Submerged weight per unit length (kN/m).
    depth : float
        Water depth (m).

    Returns
    -------
    dict with keys:
        a          : catenary parameter (m) = H / w_sub
        x_dep      : horizontal distance from TDP to departure point (m)
        theta_dep  : departure angle at stinger tip (rad)
        theta_dep_deg : departure angle (degrees)
        s_catenary : suspended pipe arc length from TDP to departure (m)
        R_sag      : sagbend radius of curvature at TDP (m)
        T_top      : tension at stinger departure point (kN)
    """
    if H <= 0 or w_sub <= 0:
        return {
            "a": 0.0,
            "x_dep": 0.0,
            "theta_dep": 0.0,
            "theta_dep_deg": 0.0,
            "s_catenary": 0.0,
            "R_sag": 0.0,
            "T_top": 0.0,
        }

    a = H / w_sub  # catenary parameter (m)

    # At stinger departure: depth = a * (cosh(x_dep/a) - 1)
    # => cosh(x_dep/a) = depth/a + 1
    cosh_val = depth / a + 1.0
    if cosh_val < 1.0:
        cosh_val = 1.0  # safety clamp

    x_dep = a * math.acosh(cosh_val)

    # Departure angle: theta = arctan(sinh(x_dep/a))
    sinh_val = math.sinh(x_dep / a)
    theta_dep = math.atan(sinh_val)

    # Arc length from TDP to departure
    s_catenary = a * sinh_val

    # Sagbend radius (at TDP, curvature = w_sub / H)
    R_sag = a  # = H / w_sub

    # Top tension at departure
    T_top = H * math.cosh(x_dep / a)

    return {
        "a": a,
        "x_dep": x_dep,
        "theta_dep": theta_dep,
        "theta_dep_deg": math.degrees(theta_dep),
        "s_catenary": s_catenary,
        "R_sag": R_sag,
        "T_top": T_top,
    }


def calc_sagbend_stress(
    H: float,
    w_sub: float,
    od_m: float,
    wt_m: float,
    a_steel_m2: float,
    config: Optional[Any] = None,
) -> Dict[str, float]:
    """Calculate sagbend stress near the TDP.

    The sagbend has the tightest curvature in the catenary span (at the TDP).
    Curvature kappa = w_sub / H, so R_sag = H / w_sub = a.

    Stress basis (the §4 fix — ``criteria.sagbend_stress_basis``, default ``combined``):

      - ``combined`` (DEFAULT, user-approved): the report has always CLAIMED a combined
        bending + axial stress (sigma = E*OD/(2R) + H/A_steel, DNV-ST-F101 Sec 5), but the
        legacy code computed BENDING ONLY and silently dropped the axial term. Now
        sigma_combined = sigma_bending + sigma_axial and utilisation = sigma_combined /
        (stress_limit_factor * SMYS). sigma_combined_mpa equals the SUM.
      - ``bending_only``: the legacy diagnostic path — utilisation = sigma_bending / allowable,
        sigma_combined_mpa == sigma_bending_mpa (axial reported but not added).

    Returns
    -------
    dict with keys:
        R_sag_m           : radius of curvature at sagbend (m)
        kappa_sag         : curvature (1/m)
        sigma_bending_mpa : bending stress at sagbend (MPa)
        sigma_axial_mpa   : axial stress from horizontal tension (MPa)
        sigma_combined_mpa: bending + axial (combined basis) or == bending (bending_only basis)
        stress_basis      : the active basis ('combined' | 'bending_only')
        utilisation       : sigma_basis / (stress_limit_factor * SMYS)
        status            : PASS / FAIL
    """
    E = _cfg(config, "youngs_modulus_pa", STEEL_YOUNGS_MODULUS)
    smys = _cfg(config, "smys_pa", SMYS_X65)
    slf = _cfg(config, "stress_limit_factor", STRESS_LIMIT_FACTOR)
    basis = _cfg(config, "sagbend_stress_basis", SAGBEND_STRESS_BASIS)
    allowable = slf * smys  # Pa

    # Curvature at TDP
    w_sub_n = w_sub * 1000.0  # kN/m -> N/m
    H_n = H * 1000.0          # kN -> N

    if H_n <= 0:
        return {
            "R_sag_m": 0.0,
            "kappa_sag": float("inf"),
            "sigma_bending_mpa": float("inf"),
            "sigma_axial_mpa": 0.0,
            "sigma_combined_mpa": float("inf"),
            "stress_basis": basis,
            "utilisation": 999.0,
            "status": "FAIL",
        }

    kappa_sag = w_sub_n / H_n
    R_sag = 1.0 / kappa_sag if kappa_sag > 0 else float("inf")

    # Bending stress: sigma = E * OD / (2 * R)
    sigma_bending = E * od_m / (2.0 * R_sag) if R_sag > 0 else float("inf")

    # Axial stress from horizontal tension at TDP
    sigma_axial = H_n / a_steel_m2 if a_steel_m2 > 0 else 0.0

    # The §4 fix: combined basis adds axial to bending (matching the report's stated method);
    # bending_only is the legacy diagnostic path (axial reported but not added).
    if basis == "bending_only":
        sigma_for_util = sigma_bending
        sigma_combined = sigma_bending
    else:  # 'combined' (default, user-approved)
        sigma_combined = sigma_bending + sigma_axial
        sigma_for_util = sigma_combined

    utilisation = sigma_for_util / allowable if allowable > 0 else 999.0

    return {
        "R_sag_m": round(R_sag, 1),
        "kappa_sag": round(kappa_sag, 8),
        "sigma_bending_mpa": round(sigma_bending / 1e6, 1),
        "sigma_axial_mpa": round(sigma_axial / 1e6, 1),
        "sigma_combined_mpa": round(sigma_combined / 1e6, 1),
        "stress_basis": basis,
        "utilisation": round(utilisation, 4),
        "status": "PASS" if utilisation <= 1.0 else "FAIL",
    }


def calc_overbend_stress(
    stinger_length_m: float,
    theta_dep_rad: float,
    od_m: float,
    config: Optional[Any] = None,
) -> Dict[str, float]:
    """Calculate overbend bending stress at the stinger.

    The pipe curves over the stinger from roughly horizontal (vessel deck) to the
    departure angle.  Approximate stinger radius of curvature:
        R_ob = stinger_length / sin(theta_dep)

    This is a simplified geometric model — real stinger geometry is segmented.

    Returns
    -------
    dict with keys:
        R_overbend_m       : overbend radius of curvature (m)
        sigma_overbend_mpa : overbend bending stress (MPa)
        utilisation        : sigma_overbend / (0.72 * SMYS)
        status             : PASS / FAIL
    """
    E = _cfg(config, "youngs_modulus_pa", STEEL_YOUNGS_MODULUS)
    smys = _cfg(config, "smys_pa", SMYS_X65)
    slf = _cfg(config, "stress_limit_factor", STRESS_LIMIT_FACTOR)
    allowable = slf * smys  # Pa

    sin_theta = math.sin(theta_dep_rad) if theta_dep_rad > 0 else 0.0

    if sin_theta < 0.001:
        # Very flat departure — overbend radius is effectively infinite
        return {
            "R_overbend_m": float("inf"),
            "sigma_overbend_mpa": 0.0,
            "utilisation": 0.0,
            "status": "PASS",
        }

    R_ob = stinger_length_m / sin_theta

    # Overbend bending stress
    sigma_ob = E * od_m / (2.0 * R_ob)

    utilisation = sigma_ob / allowable if allowable > 0 else 999.0

    return {
        "R_overbend_m": round(R_ob, 1),
        "sigma_overbend_mpa": round(sigma_ob / 1e6, 1),
        "utilisation": round(utilisation, 4),
        "status": "PASS" if utilisation <= 1.0 else "FAIL",
    }


def find_required_tension(
    pipe: Dict,
    depth: float,
    vessel_params: Dict,
    config: Optional[Any] = None,
) -> Dict[str, Any]:
    """Find the minimum horizontal tension H that satisfies all installation checks.

    Algorithm
    ---------
    1. Compute H_min from sagbend stress limit:
       kappa_max = 2 * sigma_allow / (E * OD)
       H_min_sagbend = w_sub_N / kappa_max

    2. Iterate: check departure angle against stinger range.
       - If theta_dep > max stinger angle: increase H (flatter catenary)
       - If theta_dep < min stinger angle: decrease H (steeper catenary)
       Use bisection to find H that gives theta_dep within stinger range.

    3. Take H = max(H_sagbend, H_stinger_constraint).

    4. Check top tension against tensioner capacity.

    Returns
    -------
    dict with all check results, H chosen, T_top, and overall status.
    """
    od_m = pipe["od_m"]
    wt_m = pipe["wt_m"]
    a_steel = pipe["a_steel_m2"]
    w_sub = calc_submerged_weight(pipe, config=config)  # kN/m

    E = _cfg(config, "youngs_modulus_pa", STEEL_YOUNGS_MODULUS)
    smys = _cfg(config, "smys_pa", SMYS_X65)
    slf = _cfg(config, "stress_limit_factor", STRESS_LIMIT_FACTOR)
    gravity = _cfg(config, "gravity_m_s2", GRAVITY)
    tension_margin = _cfg(config, "tension_margin", TENSION_MARGIN)
    allowable_stress = slf * smys  # Pa

    stinger_length = vessel_params["stinger_length_m"]
    angle_min_deg = vessel_params["stinger_angle_min_deg"]
    angle_max_deg = vessel_params["stinger_angle_max_deg"]
    tensioner_cap_te = vessel_params["tensioner_capacity_te"]

    # Convert to radians
    angle_min_rad = math.radians(angle_min_deg)
    angle_max_rad = math.radians(angle_max_deg)

    # --- Step 1: Minimum H from sagbend bending stress limit ---
    # sigma_bending = E * OD / (2 * R_sag) where R_sag = H / w_sub
    # => sigma_bending = E * OD * w_sub / (2 * H)
    # At the limit: allowable = E * OD * w_sub_N / (2 * H_N)
    # => H_min_N = E * OD * w_sub_N / (2 * allowable)
    # => kappa_max = 2 * allowable / (E * OD)
    # => H_min = w_sub / kappa_max

    kappa_max = 2.0 * allowable_stress / (E * od_m) if (E * od_m) > 0 else 1e-6
    a_min = 1.0 / kappa_max  # minimum sagbend radius (m)

    if a_min <= 0:
        return _build_infeasible_result(pipe, depth, vessel_params, w_sub, "SAGBEND_INFEASIBLE", config=config)

    H_min_sagbend = a_min * w_sub  # kN (since a = H_kN / w_sub_kN)

    # Apply the operational margin above absolute minimum tension (criteria.tension_margin).
    # In practice, contractors always apply a margin for dynamic effects,
    # lay rate variations, and operational contingency.
    H_target = H_min_sagbend * tension_margin

    # --- Step 2: Check departure angle at H_target ---
    cat = calc_catenary(H_target, w_sub, depth)
    theta_dep_rad = cat["theta_dep"]
    theta_dep_deg = cat["theta_dep_deg"]

    # Adjust H if departure angle is outside stinger range
    H_chosen = H_target
    stinger_governs = False
    stinger_status = "OK"

    if theta_dep_rad > angle_max_rad:
        # Departure angle too steep — need to increase H for flatter catenary
        # Bisection: find H where theta_dep = angle_max
        H_lo = H_target
        H_hi = H_target * 100.0  # generous upper bound

        for _ in range(100):
            H_mid = (H_lo + H_hi) / 2.0
            cat_mid = calc_catenary(H_mid, w_sub, depth)
            if cat_mid["theta_dep"] > angle_max_rad:
                H_lo = H_mid
            else:
                H_hi = H_mid
            if abs(H_hi - H_lo) < 0.01:
                break

        H_chosen = H_hi
        stinger_governs = True
        stinger_status = "ADJUSTED_UP"

    elif theta_dep_rad < angle_min_rad:
        # Departure angle too flat — need to decrease H for steeper catenary
        # But we cannot go below H_min_sagbend (sagbend stress limit).
        # This means the case may be infeasible.
        # Try bisection downward, clamped at a reasonable floor.
        H_lo = w_sub * depth * 0.1  # floor
        H_hi = H_target

        # Check if even at H_lo the angle is still < min
        cat_lo = calc_catenary(H_lo, w_sub, depth)
        if cat_lo["theta_dep"] < angle_min_rad:
            # Still too flat even at very low tension — unusual for shallow water
            # Keep H_target, flag departure angle issue
            stinger_governs = True
            stinger_status = "ANGLE_TOO_FLAT"
        else:
            for _ in range(100):
                H_mid = (H_lo + H_hi) / 2.0
                cat_mid = calc_catenary(H_mid, w_sub, depth)
                if cat_mid["theta_dep"] > angle_min_rad:
                    H_lo = H_mid
                else:
                    H_hi = H_mid
                if abs(H_hi - H_lo) < 0.01:
                    break

            H_candidate = H_lo
            if H_candidate < H_min_sagbend:
                # Can't use this lower H — sagbend would fail.
                # Departure angle at H_target is below minimum stinger angle.
                stinger_governs = True
                stinger_status = "ANGLE_BELOW_MIN"
            else:
                H_chosen = H_candidate
                stinger_governs = True
                stinger_status = "ADJUSTED_DOWN"

    # --- Recompute catenary at chosen H ---
    cat_final = calc_catenary(H_chosen, w_sub, depth)

    # --- Check 1: Stinger departure angle ---
    theta_final_deg = cat_final["theta_dep_deg"]
    theta_final_rad = cat_final["theta_dep"]
    angle_in_range = angle_min_deg <= theta_final_deg <= angle_max_deg
    departure_check = {
        "required_angle_deg": round(theta_final_deg, 2),
        "stinger_min_deg": angle_min_deg,
        "stinger_max_deg": angle_max_deg,
        "in_range": angle_in_range,
        "status": "PASS" if angle_in_range else "FAIL",
    }

    # --- Check 2: Top tension ---
    T_top_kn = cat_final["T_top"]
    # 1 tonne-force = 1000 kg * g (m/s2) = g kN, so T_top_te = T_top_kN / g.
    T_top_te = T_top_kn / gravity

    tension_util = T_top_te / tensioner_cap_te if tensioner_cap_te > 0 else 999.0
    tension_check = {
        "T_top_kn": round(T_top_kn, 1),
        "T_top_te": round(T_top_te, 1),
        "tensioner_capacity_te": tensioner_cap_te,
        "utilisation": round(tension_util, 4),
        "status": "PASS" if tension_util <= 1.0 else "FAIL",
    }

    # --- Check 3: Sagbend stress ---
    sagbend = calc_sagbend_stress(H_chosen, w_sub, od_m, wt_m, a_steel, config=config)

    # --- Check 4: Overbend stress ---
    overbend = calc_overbend_stress(stinger_length, theta_final_rad, od_m, config=config)

    # --- Check 5: Vessel capability (pipe diameter + water depth range) ---
    od_in = pipe["od_mm"] / 25.4
    diameter_ok = od_in <= vessel_params["max_pipe_diameter_in"]
    depth_ok = vessel_params["min_depth_m"] <= depth <= vessel_params["max_depth_m"]
    capability_check = {
        "pipe_od_in": round(od_in, 1),
        "max_vessel_diameter_in": vessel_params["max_pipe_diameter_in"],
        "diameter_ok": diameter_ok,
        "depth_ok": depth_ok,
        "status": "PASS" if (diameter_ok and depth_ok) else "FAIL",
    }

    # --- Collect all utilisations ---
    utils = {
        "sagbend": sagbend["utilisation"],
        "overbend": overbend["utilisation"],
        "tension": tension_util,
        "departure_angle": 0.0 if angle_in_range else 999.0,
        "vessel_capability": 0.0 if capability_check["status"] == "PASS" else 999.0,
    }

    return {
        "H_chosen_kn": round(H_chosen, 1),
        "H_min_sagbend_kn": round(H_min_sagbend, 1),
        "w_sub_kn_per_m": round(w_sub, 4),
        "catenary": {
            "a_m": round(cat_final["a"], 1),
            "x_dep_m": round(cat_final["x_dep"], 1),
            "s_catenary_m": round(cat_final["s_catenary"], 1),
        },
        "stinger_governs": stinger_governs,
        "stinger_status": stinger_status,
        "checks": {
            "departure_angle": departure_check,
            "top_tension": tension_check,
            "sagbend": sagbend,
            "overbend": overbend,
            "vessel_capability": capability_check,
        },
        "utilisations": utils,
    }


def _build_infeasible_result(
    pipe: Dict,
    depth: float,
    vessel_params: Dict,
    w_sub: float,
    reason: str,
    config: Any = None,
) -> Dict[str, Any]:
    """Build a result dict for an infeasible case."""
    # Emit the active sagbend basis even on the infeasible path so checks_flat
    # carries sagbend_stress_basis consistently with the normal path.
    basis = _cfg(config, "sagbend_stress_basis", SAGBEND_STRESS_BASIS)
    return {
        "H_chosen_kn": 0.0,
        "H_min_sagbend_kn": 0.0,
        "w_sub_kn_per_m": round(w_sub, 4),
        "catenary": {"a_m": 0.0, "x_dep_m": 0.0, "s_catenary_m": 0.0},
        "stinger_governs": False,
        "stinger_status": reason,
        "checks": {
            "departure_angle": {"status": "FAIL", "required_angle_deg": 0, "in_range": False,
                                "stinger_min_deg": vessel_params["stinger_angle_min_deg"],
                                "stinger_max_deg": vessel_params["stinger_angle_max_deg"]},
            "top_tension": {"status": "FAIL", "T_top_kn": 0, "T_top_te": 0,
                            "tensioner_capacity_te": vessel_params["tensioner_capacity_te"],
                            "utilisation": 999.0},
            "sagbend": {"status": "FAIL", "utilisation": 999.0, "sigma_combined_mpa": 0,
                        "sigma_bending_mpa": 0, "sigma_axial_mpa": 0, "R_sag_m": 0, "kappa_sag": 0,
                        "stress_basis": basis},
            "overbend": {"status": "FAIL", "utilisation": 999.0, "sigma_overbend_mpa": 0,
                         "R_overbend_m": 0},
            "vessel_capability": {"status": "FAIL", "diameter_ok": False, "depth_ok": False,
                                  "pipe_od_in": pipe["od_mm"] / 25.4,
                                  "max_vessel_diameter_in": vessel_params["max_pipe_diameter_in"]},
        },
        "utilisations": {
            "sagbend": 999.0,
            "overbend": 999.0,
            "tension": 999.0,
            "departure_angle": 999.0,
            "vessel_capability": 999.0,
        },
    }


# ---------------------------------------------------------------------------
# Single case runner
# ---------------------------------------------------------------------------

def run_single_case(
    vessel: Dict,
    pipe: Dict,
    depth: float,
    config: Optional[Any] = None,
) -> Dict[str, Any]:
    """Run all installation checks for a single vessel/pipe/depth combination.

    Returns a flat dict with overall go/no-go plus per-check results.
    """
    go_threshold = _cfg(config, "go_threshold", GO_THRESHOLD)
    nogo_utilisation = _cfg(config, "nogo_utilisation", NOGO_UTILISATION)

    vessel_params = extract_vessel_params(vessel)
    result = find_required_tension(pipe, depth, vessel_params, config=config)

    checks = result["checks"]
    utils = result["utilisations"]

    # Filter out sentinel 999 values (failed checks) for max_util calculation
    real_utils = {k: v for k, v in utils.items() if v < 900}
    failed_checks = {k: v for k, v in utils.items() if v >= 900}

    if failed_checks:
        # At least one check hard-failed
        max_util = max(utils.values())
        # Find governing check from the ones that actually have numeric utils
        if real_utils:
            governing_check = max(real_utils, key=real_utils.get)
            max_real_util = real_utils[governing_check]
        else:
            governing_check = list(failed_checks.keys())[0]
            max_real_util = 999.0
        overall = "NO_GO"
        governing = list(failed_checks.keys())[0]  # report the failed check
    else:
        max_util = max(utils.values())
        governing = max(utils, key=utils.get)
        if max_util > nogo_utilisation:
            overall = "NO_GO"
        elif max_util >= go_threshold:
            overall = "MARGINAL"
        else:
            overall = "GO"

    return {
        "vessel_id": vessel_params["id"],
        "vessel_name": vessel_params["name"],
        "pipe_size": pipe["nominal_size"],
        "pipe_od_mm": pipe["od_mm"],
        "pipe_wt_mm": pipe["wt_mm"],
        "water_depth_m": depth,
        "overall_status": overall,
        "max_utilisation": round(min(max_util, 9.99), 4),  # cap for display
        "governing_check": governing,
        "H_chosen_kn": result["H_chosen_kn"],
        "T_top_te": result["checks"]["top_tension"].get("T_top_te", 0),
        "departure_angle_deg": result["checks"]["departure_angle"].get("required_angle_deg", 0),
        "sagbend_util": result["utilisations"]["sagbend"],
        "overbend_util": result["utilisations"]["overbend"],
        "tension_util": result["utilisations"]["tension"],
        "sagbend_stress_mpa": result["checks"]["sagbend"].get("sigma_combined_mpa", 0),
        "overbend_stress_mpa": result["checks"]["overbend"].get("sigma_overbend_mpa", 0),
        "R_sag_m": result["checks"]["sagbend"].get("R_sag_m", 0),
        "checks": checks,
    }


# ---------------------------------------------------------------------------
# Parametric sweep
# ---------------------------------------------------------------------------

def run_parametric_sweep(
    vessels: List[Dict],
    pipes: List[Dict],
    config: Optional[Any] = None,
) -> Tuple[List[Dict], pd.DataFrame]:
    """Run the full parametric sweep across all combinations.

    The axes come from *config* when supplied (vessels resolved by id in config order, pipes as
    loaded by ``load_pipes`` from the config selection, depths from ``config.water_depths``);
    otherwise the module default depth list and the catalog order are used.
    """
    depths_axis = list(config.water_depths) if config is not None else WATER_DEPTHS

    # Resolve the vessels in config order (by id). The loop is config-driven: a config vessel
    # subset/reorder threads straight through.
    if config is not None:
        by_id = {v["id"]: v for v in vessels}
        missing = [vid for vid in config.vessels if vid not in by_id]
        if missing:
            raise ValueError(
                f"config vessels {missing} not found in the vessels catalog "
                f"(available ids: {sorted(by_id)})."
            )
        sweep_vessels = [by_id[vid] for vid in config.vessels]
    else:
        sweep_vessels = vessels

    total = len(sweep_vessels) * len(pipes) * len(depths_axis)

    print(f"\n{'='*60}")
    print(f"  PARAMETRIC S-LAY INSTALLATION SCREENING")
    print(f"  {total} cases: {len(sweep_vessels)} vessels x {len(pipes)} pipes"
          f" x {len(depths_axis)} depths")
    print(f"{'='*60}\n")

    all_results: List[Dict] = []
    case_num = 0

    for vessel in sweep_vessels:
        vp = extract_vessel_params(vessel)
        for pipe in pipes:
            for depth in depths_axis:
                case_num += 1
                pl = pipe_label(pipe["nominal_size"])
                print(
                    f"  Case {case_num:3d}/{total} | {vp['name']:<22s} | "
                    f"{pl:>4s} | {depth:3.0f}m ...",
                    end="",
                )

                try:
                    result = run_single_case(vessel, pipe, depth, config=config)
                    all_results.append(result)
                    status_tag = result["overall_status"]
                    util = result["max_utilisation"]
                    print(f" util={util:.3f} [{status_tag}]")
                except Exception as exc:
                    logger.warning("Case failed: %s", exc)
                    print(f" [ERROR: {exc}]")
                    all_results.append({
                        "vessel_id": vp["id"],
                        "vessel_name": vp["name"],
                        "pipe_size": pipe["nominal_size"],
                        "pipe_od_mm": pipe["od_mm"],
                        "pipe_wt_mm": pipe["wt_mm"],
                        "water_depth_m": depth,
                        "overall_status": "ERROR",
                        "max_utilisation": None,
                        "governing_check": "N/A",
                        "H_chosen_kn": 0,
                        "T_top_te": 0,
                        "departure_angle_deg": 0,
                        "sagbend_util": None,
                        "overbend_util": None,
                        "tension_util": None,
                        "sagbend_stress_mpa": 0,
                        "overbend_stress_mpa": 0,
                        "R_sag_m": 0,
                        "checks": {},
                    })

    # Build DataFrame
    rows = []
    for r in all_results:
        rows.append({
            "vessel_id": r["vessel_id"],
            "vessel_name": r["vessel_name"],
            "pipe_size": r["pipe_size"],
            "pipe_od_mm": r["pipe_od_mm"],
            "pipe_wt_mm": r["pipe_wt_mm"],
            "water_depth_m": r["water_depth_m"],
            "overall_status": r["overall_status"],
            "max_utilisation": r["max_utilisation"],
            "governing_check": r["governing_check"],
            "H_chosen_kn": r["H_chosen_kn"],
            "T_top_te": r["T_top_te"],
            "departure_angle_deg": r["departure_angle_deg"],
            "sagbend_util": r["sagbend_util"],
            "overbend_util": r["overbend_util"],
            "tension_util": r["tension_util"],
            "sagbend_stress_mpa": r["sagbend_stress_mpa"],
            "overbend_stress_mpa": r["overbend_stress_mpa"],
            "R_sag_m": r["R_sag_m"],
        })

    df = pd.DataFrame(rows)
    print(f"\n  Sweep complete: {len(all_results)} results collected")
    return all_results, df


# ---------------------------------------------------------------------------
# Chart 1: Go/No-Go Matrix (HERO)
# ---------------------------------------------------------------------------

def build_chart_1_go_nogo_matrix(results_df: pd.DataFrame, config: Optional[Any] = None) -> go.Figure:
    """Go/No-Go heatmap — side-by-side for two vessels.

    X: water depth, Y: pipe size
    Cell color: GO / MARGINAL / NO_GO
    Annotation: governing check + utilisation %
    """
    print("\n[Chart 1] Building Go/No-Go Matrix...")

    vessel_names = sorted(results_df["vessel_name"].unique())
    pipe_sizes = sorted(results_df["pipe_size"].unique(), key=lambda s: float(s.replace("in", "")))
    depths = sorted(results_df["water_depth_m"].unique())
    pipe_labels = [pipe_label(ps) for ps in pipe_sizes]

    status_to_num = {"GO": 0, "MARGINAL": 1, "NO_GO": 2, "ERROR": 2}
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
        vdf = results_df[results_df["vessel_name"] == vname]

        z_matrix = []
        annotation_text = []

        for ps in pipe_sizes:
            z_row = []
            ann_row = []
            for d in depths:
                match = vdf[(vdf["pipe_size"] == ps) & (vdf["water_depth_m"] == d)]
                if len(match) > 0:
                    row = match.iloc[0]
                    status = row["overall_status"]
                    util = row["max_utilisation"]
                    gov = row["governing_check"]
                    z_row.append(status_to_num.get(status, 2))
                    if util is not None and util < 900:
                        ann_row.append(
                            f"{go_nogo_label(status)}<br>{gov}<br>{util:.0%}"
                        )
                    else:
                        ann_row.append(f"{go_nogo_label(status)}<br>{gov}")
                else:
                    z_row.append(2)
                    ann_row.append("N/A")
            z_matrix.append(z_row)
            annotation_text.append(ann_row)

        fig.add_trace(
            go.Heatmap(
                z=z_matrix,
                x=[f"{d}m" for d in depths],
                y=pipe_labels,
                colorscale=colorscale,
                zmin=0,
                zmax=2,
                showscale=False,
                text=annotation_text,
                texttemplate="%{text}",
                textfont=dict(size=9),
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
            text="S-lay Installation Go/No-Go Matrix",
            font=dict(size=18),
        ),
        height=450,
        width=1050,
    )
    fig.update_xaxes(title_text="Water Depth", row=1, col=1)
    fig.update_xaxes(title_text="Water Depth", row=1, col=2)

    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 2: Sagbend Stress Utilisation Heatmap
# ---------------------------------------------------------------------------

def build_chart_2_sagbend_heatmap(results_df: pd.DataFrame, config: Optional[Any] = None) -> go.Figure:
    """Sagbend utilisation heatmap — side-by-side per vessel."""
    print("\n[Chart 2] Building Sagbend Stress Utilisation Heatmap...")

    vessel_names = sorted(results_df["vessel_name"].unique())
    pipe_sizes = sorted(results_df["pipe_size"].unique(), key=lambda s: float(s.replace("in", "")))
    depths = sorted(results_df["water_depth_m"].unique())
    pipe_labels = [pipe_label(ps) for ps in pipe_sizes]

    fig = make_subplots(
        rows=1,
        cols=len(vessel_names),
        subplot_titles=[f"{v}" for v in vessel_names],
        horizontal_spacing=0.12,
    )

    for col_idx, vname in enumerate(vessel_names, 1):
        vdf = results_df[results_df["vessel_name"] == vname]

        z_matrix = []
        ann_matrix = []

        for ps in pipe_sizes:
            z_row = []
            ann_row = []
            for d in depths:
                match = vdf[(vdf["pipe_size"] == ps) & (vdf["water_depth_m"] == d)]
                if len(match) > 0:
                    util = match.iloc[0]["sagbend_util"]
                    if util is not None and util < 900:
                        z_row.append(util * 100)
                        ann_row.append(f"{util:.0%}")
                    else:
                        z_row.append(100.0)
                        ann_row.append("N/A")
                else:
                    z_row.append(0.0)
                    ann_row.append("N/A")
            z_matrix.append(z_row)
            ann_matrix.append(ann_row)

        fig.add_trace(
            go.Heatmap(
                z=z_matrix,
                x=[f"{d}m" for d in depths],
                y=pipe_labels,
                colorscale=[
                    [0.0, "#38a169"],    # green — low utilisation
                    [0.5, "#d69e2e"],    # amber — moderate
                    [0.72, "#ed8936"],   # orange — approaching limit
                    [1.0, "#e53e3e"],    # red — at/over limit
                ],
                zmin=0,
                zmax=100,
                showscale=(col_idx == len(vessel_names)),
                colorbar=dict(title="Util %", len=0.8) if col_idx == len(vessel_names) else None,
                text=ann_matrix,
                texttemplate="%{text}",
                textfont=dict(size=10),
                hovertemplate=(
                    "<b>%{y}</b> at %{x}<br>"
                    "Sagbend utilisation: %{z:.0f}%<extra></extra>"
                ),
            ),
            row=1,
            col=col_idx,
        )

    fig.update_layout(
        title=dict(
            text="Sagbend Stress Utilisation (% of 0.72 x SMYS)",
            font=dict(size=18),
        ),
        height=450,
        width=1050,
    )
    fig.update_xaxes(title_text="Water Depth", row=1, col=1)
    fig.update_xaxes(title_text="Water Depth", row=1, col=2)

    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 3: Required Top Tension vs Water Depth
# ---------------------------------------------------------------------------

def build_chart_3_tension_vs_depth(
    results_df: pd.DataFrame,
    vessels: Optional[List[Dict]] = None,
    config: Optional[Any] = None,
) -> go.Figure:
    """Required top tension vs water depth — lines per pipe size,
    with vessel capacity reference lines.

    The capacity reference lines are DERIVED from the vessel catalog (single-sourced), not
    duplicated literals.
    """
    print("\n[Chart 3] Building Required Tension vs Water Depth...")

    pipe_sizes = sorted(results_df["pipe_size"].unique(), key=lambda s: float(s.replace("in", "")))
    depths = sorted(results_df["water_depth_m"].unique())

    # Resolve the per-vessel tensioner capacities from the catalog (single-source). Fall back
    # to the result-frame's own values when no catalog is supplied.
    specs = vessel_band_specs(vessels) if vessels else []

    fig = go.Figure()
    color_idx = 0

    # Plot for Large PLV (solid lines)
    plv_df = results_df[results_df["vessel_name"].str.contains("Large")]
    for ps in pipe_sizes:
        subset = plv_df[plv_df["pipe_size"] == ps].sort_values("water_depth_m")
        pl = pipe_label(ps)

        fig.add_trace(
            go.Scatter(
                x=subset["water_depth_m"],
                y=subset["T_top_te"],
                mode="lines+markers",
                name=f"Large PLV / {pl}",
                line=dict(
                    color=CHART_PALETTE[color_idx % len(CHART_PALETTE)],
                    dash="solid",
                    width=2,
                ),
                marker=dict(size=6),
                hovertemplate=(
                    f"<b>Large PLV — {pl}</b><br>"
                    "Depth: %{x}m<br>"
                    "T_top: %{y:.1f} te<extra></extra>"
                ),
            )
        )
        color_idx += 1

    # Plot for Barge (dashed lines)
    barge_df = results_df[results_df["vessel_name"].str.contains("Barge")]
    color_idx = 0
    for ps in pipe_sizes:
        subset = barge_df[barge_df["pipe_size"] == ps].sort_values("water_depth_m")
        pl = pipe_label(ps)

        fig.add_trace(
            go.Scatter(
                x=subset["water_depth_m"],
                y=subset["T_top_te"],
                mode="lines+markers",
                name=f"Barge / {pl}",
                line=dict(
                    color=CHART_PALETTE[color_idx % len(CHART_PALETTE)],
                    dash="dash",
                    width=2,
                ),
                marker=dict(size=5, symbol="diamond"),
                hovertemplate=(
                    f"<b>Barge — {pl}</b><br>"
                    "Depth: %{x}m<br>"
                    "T_top: %{y:.1f} te<extra></extra>"
                ),
            )
        )
        color_idx += 1

    # Vessel capacity reference lines — DERIVED from the catalog (single-source).
    _line_colors = [COLORS["secondary"], COLORS["danger"]]
    _positions = ["top right", "bottom right"]
    for i, spec in enumerate(specs):
        fig.add_hline(
            y=spec["tensioner_capacity_te"],
            line_dash="dot",
            line_color=_line_colors[i % len(_line_colors)],
            line_width=2,
            annotation_text=f"{spec['name']} capacity ({spec['tensioner_capacity_te']:g} te)",
            annotation_position=_positions[i % len(_positions)],
        )

    fig.update_layout(
        title=dict(
            text="Required Top Tension vs Water Depth",
            font=dict(size=18),
        ),
        xaxis_title="Water Depth (m)",
        yaxis_title="Required Top Tension (te)",
        height=550,
        width=1000,
        legend=dict(font=dict(size=9)),
    )

    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 4: Stinger Departure Angle vs Water Depth
# ---------------------------------------------------------------------------

def build_chart_4_departure_angle(
    results_df: pd.DataFrame,
    vessels: Optional[List[Dict]] = None,
    config: Optional[Any] = None,
) -> go.Figure:
    """Stinger departure angle vs water depth — lines per pipe size,
    with shaded bands for each vessel's stinger range.

    The shaded stinger-range bands are DERIVED from the vessel catalog (single-sourced), not
    duplicated literals.
    """
    print("\n[Chart 4] Building Stinger Departure Angle vs Water Depth...")

    pipe_sizes = sorted(results_df["pipe_size"].unique(), key=lambda s: float(s.replace("in", "")))
    depths = sorted(results_df["water_depth_m"].unique())

    fig = go.Figure()

    # Shaded bands for vessel stinger ranges — DERIVED from the catalog (single-source).
    x_band = [min(depths) - 1, max(depths) + 1]
    specs = vessel_band_specs(vessels) if vessels else []
    _band_fills = ["rgba(44, 82, 130, 0.12)", "rgba(237, 137, 54, 0.12)"]
    for i, spec in enumerate(specs):
        lo = spec["angle_min_deg"]
        hi = spec["angle_max_deg"]
        fig.add_trace(
            go.Scatter(
                x=x_band + x_band[::-1],
                y=[lo, lo, hi, hi],
                fill="toself",
                fillcolor=_band_fills[i % len(_band_fills)],
                line=dict(color="rgba(0,0,0,0)"),
                name=f"{spec['name']} range ({lo:g}-{hi:g} deg)",
                showlegend=True,
                hoverinfo="skip",
            )
        )

    # Plot lines for Large PLV
    plv_df = results_df[results_df["vessel_name"].str.contains("Large")]
    color_idx = 0
    for ps in pipe_sizes:
        subset = plv_df[plv_df["pipe_size"] == ps].sort_values("water_depth_m")
        pl = pipe_label(ps)

        fig.add_trace(
            go.Scatter(
                x=subset["water_depth_m"],
                y=subset["departure_angle_deg"],
                mode="lines+markers",
                name=f"PLV / {pl}",
                line=dict(
                    color=CHART_PALETTE[color_idx % len(CHART_PALETTE)],
                    dash="solid",
                    width=2,
                ),
                marker=dict(size=6),
                hovertemplate=(
                    f"<b>Large PLV — {pl}</b><br>"
                    "Depth: %{x}m<br>"
                    "Departure angle: %{y:.1f} deg<extra></extra>"
                ),
            )
        )
        color_idx += 1

    fig.update_layout(
        title=dict(
            text="Required Stinger Departure Angle vs Water Depth",
            font=dict(size=18),
        ),
        xaxis_title="Water Depth (m)",
        yaxis_title="Departure Angle (degrees)",
        height=550,
        width=1000,
        legend=dict(font=dict(size=9)),
        yaxis=dict(range=[0, 15]),
    )

    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 5: Vessel Head-to-Head — Max Pipe Size at Each Depth
# ---------------------------------------------------------------------------

def build_chart_5_vessel_comparison(results_df: pd.DataFrame, config: Optional[Any] = None) -> go.Figure:
    """Grouped bar chart: max pipe size installable per depth per vessel."""
    print("\n[Chart 5] Building Vessel Head-to-Head Comparison...")

    vessel_names = sorted(results_df["vessel_name"].unique())
    depths = sorted(results_df["water_depth_m"].unique())

    fig = go.Figure()
    color_idx = 0

    for vname in vessel_names:
        max_pipe_sizes = []
        for d in depths:
            subset = results_df[
                (results_df["vessel_name"] == vname)
                & (results_df["water_depth_m"] == d)
                & (results_df["overall_status"].isin(["GO", "MARGINAL"]))
            ]
            if len(subset) > 0:
                # Get max pipe OD in inches
                max_od_in = subset["pipe_od_mm"].max() / 25.4
                max_pipe_sizes.append(round(max_od_in, 0))
            else:
                max_pipe_sizes.append(0)

        fig.add_trace(
            go.Bar(
                x=[f"{d}m" for d in depths],
                y=max_pipe_sizes,
                name=vname,
                marker_color=CHART_PALETTE[color_idx % len(CHART_PALETTE)],
                text=[f'{int(s)}"' if s > 0 else "None" for s in max_pipe_sizes],
                textposition="auto",
                hovertemplate=(
                    f"<b>{vname}</b><br>"
                    "Depth: %{x}<br>"
                    'Max pipe: %{y:.0f}"<extra></extra>'
                ),
            )
        )
        color_idx += 1

    fig.update_layout(
        title=dict(
            text="Vessel Comparison — Max Installable Pipe Size at Each Depth",
            font=dict(size=18),
        ),
        xaxis_title="Water Depth",
        yaxis_title='Max Pipe Size (inches)',
        yaxis=dict(range=[0, 28], dtick=4),
        barmode="group",
        height=500,
        width=900,
    )

    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Summary table
# ---------------------------------------------------------------------------

def build_summary_table(results_df: pd.DataFrame, config: Optional[Any] = None) -> pd.DataFrame:
    """Build summary table for all cases."""
    rows = []
    for _, row in results_df.iterrows():
        util = row["max_utilisation"]
        status = row["overall_status"]
        pl = pipe_label(row["pipe_size"])
        rows.append({
            "Vessel": row["vessel_name"],
            "Pipe": pl,
            "WT (mm)": row["pipe_wt_mm"],
            "Depth (m)": int(row["water_depth_m"]),
            "T_top (te)": round(row["T_top_te"], 1) if row["T_top_te"] else "N/A",
            "Depart. Angle": f"{row['departure_angle_deg']:.1f} deg" if row["departure_angle_deg"] else "N/A",
            "Sagbend Util": f"{row['sagbend_util']:.0%}" if row["sagbend_util"] is not None and row["sagbend_util"] < 900 else "FAIL",
            "Max Util.": f"{util:.1%}" if util is not None and util < 9 else "FAIL",
            "Gov. Check": row["governing_check"],
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
    vessels: Optional[List[Dict]] = None,
    config: Optional[Any] = None,
    output_path: Optional[Path] = None,
) -> str:
    """Build the branded HTML report.

    The methodology + assumptions prose is reconciled to state the ACTIVE config truthfully:
    the stinger-range and tensioner-capacity text is SINGLE-SOURCED from the vessel catalog, and
    the sagbend stress basis prose reflects ``criteria.sagbend_stress_basis`` (no contradiction
    between the stated method and the computed utilisation).
    """
    print("\n[Report] Building HTML report...")

    # Resolve the active config scalars (else module defaults) for the prose.
    slf = _cfg(config, "stress_limit_factor", STRESS_LIMIT_FACTOR)
    go_threshold = _cfg(config, "go_threshold", GO_THRESHOLD)
    nogo = _cfg(config, "nogo_utilisation", NOGO_UTILISATION)
    basis = _cfg(config, "sagbend_stress_basis", SAGBEND_STRESS_BASIS)
    grade = _cfg(config, "grade", "X65")
    smys_pa = _cfg(config, "smys_pa", SMYS_X65)
    youngs_pa = _cfg(config, "youngs_modulus_pa", STEEL_YOUNGS_MODULUS)
    seawater = _cfg(config, "seawater_density_kg_m3", SEAWATER_DENSITY)
    smys_mpa = smys_pa / 1e6
    youngs_gpa = youngs_pa / 1e9

    # Single-source the stinger-range + tensioner-capacity text from the vessel catalog.
    specs = vessel_band_specs(vessels) if vessels else []
    if specs:
        stinger_text = "; ".join(
            f"{s['name']}: {s['angle_min_deg']:g}&deg;-{s['angle_max_deg']:g}&deg;" for s in specs
        )
        capacity_text = " or ".join(
            f"{s['tensioner_capacity_te']:g} te" for s in specs
        )
    else:
        stinger_text = "the vessel's adjustable range"
        capacity_text = "the vessel tensioner capacity"

    report = GTMReportBuilder(
        title="Shallow Water Pipeline Installation Analysis",
        subtitle=f"60 parametric S-lay cases: 2 vessels x 5 pipe sizes x 6 water depths",
        demo_id="demo_04",
        case_count=total_cases,
        code_refs=[
            "DNV-ST-F101 (2021) - Submarine Pipeline Systems",
            "API RP 1111 (2015) - Design, Construction, Operation, and Maintenance of Offshore Hydrocarbon Pipelines",
            "DNV-OS-F101 (2013) - Submarine Pipeline Systems",
        ],
    )

    # The §4 fix: state the ACTIVE sagbend stress basis truthfully (no contradiction with code).
    if basis == "bending_only":
        sagbend_prose = (
            f"At the TDP, curvature &kappa; = w<sub>sub</sub> / H gives the tightest bend "
            f"radius. <strong>Diagnostic basis (bending only):</strong> the sagbend "
            f"utilisation uses bending stress alone, &sigma;<sub>bend</sub> = E &times; OD / "
            f"(2R); the axial stress H / A<sub>steel</sub> is reported but NOT added. Must "
            f"satisfy &sigma; &le; {slf:g} &times; SMYS per DNV-ST-F101 installation allowable."
        )
    else:
        sagbend_prose = (
            f"At the TDP, curvature &kappa; = w<sub>sub</sub> / H gives the tightest bend "
            f"radius. <strong>Combined bending + axial stress</strong> (active basis): "
            f"&sigma; = E &times; OD / (2R) + H / A<sub>steel</sub>. The bending and axial "
            f"components are summed, and the utilisation is &sigma;<sub>combined</sub> / "
            f"({slf:g} &times; SMYS). Must satisfy &sigma; &le; {slf:g} &times; SMYS per "
            f"DNV-ST-F101 installation allowable."
        )

    methodology_html = f"""
    <p>This analysis screens shallow water S-lay pipeline installations across a parametric
    matrix of vessels, pipe sizes, and water depths. Each case solves the catenary mechanics
    to find the minimum horizontal tension and evaluates five installation checks per
    DNV-ST-F101 and API RP 1111 methodology.</p>

    <h3>S-lay Catenary Theory</h3>
    <p>During S-lay, the pipe forms a catenary curve from the vessel stinger tip down to the
    seabed touchdown point (TDP). The catenary is characterised by parameter
    <em>a = H / w<sub>sub</sub></em>, where H is horizontal tension and w<sub>sub</sub> is
    submerged weight per unit length.</p>
    <ul>
        <li>Arc length: s = a &times; sinh(x/a)</li>
        <li>Height above TDP: y = a &times; (cosh(x/a) - 1)</li>
        <li>Pipe angle: &theta; = arctan(sinh(x/a))</li>
    </ul>

    <h3>Check 1: Stinger Departure Angle</h3>
    <p>The required departure angle at the stinger tip must fall within the vessel's
    adjustable range. {stinger_text}.</p>

    <h3>Check 2: Top Tension</h3>
    <p>Tension at stinger departure: T<sub>top</sub> = H &times; cosh(x<sub>dep</sub>/a).
    Must not exceed vessel tensioner capacity ({capacity_text}).</p>

    <h3>Check 3: Sagbend Stress (Critical for Shallow Water)</h3>
    <p>{sagbend_prose}</p>

    <h3>Check 4: Overbend Stress</h3>
    <p>Pipe bending over the stinger: R<sub>ob</sub> = L<sub>stinger</sub> / sin(&theta;<sub>dep</sub>).
    Overbend stress must also satisfy the {slf:g} &times; SMYS limit.</p>

    <h3>Check 5: Vessel Capability</h3>
    <p>Pipe diameter must be within vessel's handling range and water depth within
    operational envelope.</p>

    <h3>Go/No-Go Criteria</h3>
    <ul>
        <li><strong>GO:</strong> All checks pass AND max utilisation &lt; {go_threshold:g}</li>
        <li><strong>MARGINAL:</strong> All checks pass AND max utilisation in [{go_threshold:g}, {nogo:g}]</li>
        <li><strong>NO-GO:</strong> Any check fails (utilisation &gt; {nogo:g})</li>
    </ul>
    """
    report.add_methodology(methodology_html)

    report.add_chart(
        "go_nogo",
        fig1,
        title="Chart 1: Go/No-Go Installation Matrix",
        subtitle="Side-by-side vessel comparison across all pipe sizes and water depths.",
    )

    report.add_chart(
        "sagbend_heatmap",
        fig2,
        title="Chart 2: Sagbend Stress Utilisation Heatmap",
        subtitle=(
            f"Sagbend utilisation as percentage of {slf:g} x SMYS "
            f"({grade} = {smys_mpa:g} MPa). Green = low, red = critical."
        ),
    )

    report.add_chart(
        "tension_vs_depth",
        fig3,
        title="Chart 3: Required Top Tension vs Water Depth",
        subtitle="Solid = Large PLV, dashed = Barge. Horizontal lines show tensioner capacity limits.",
    )

    report.add_chart(
        "departure_angle",
        fig4,
        title="Chart 4: Stinger Departure Angle vs Water Depth",
        subtitle="Required departure angle with vessel stinger range bands. Shows where stinger geometry governs.",
    )

    report.add_chart(
        "vessel_comparison",
        fig5,
        title="Chart 5: Vessel Head-to-Head — Max Pipe Size at Each Depth",
        subtitle="Maximum installable pipe diameter for each vessel at each water depth.",
    )

    report.add_table(
        "Installation Screening Summary",
        summary_df,
        subtitle="All 60 parametric cases — 2 vessels x 5 pipe sizes x 6 water depths",
        status_col="Status",
    )

    report.add_live_mode_teaser(
        analysis_type="the pipeline installation screening"
    )

    # The §4 fix: the sagbend-basis assumption now states the ACTIVE basis truthfully, matching
    # the computed utilisation (no contradiction with the methodology or the code).
    if basis == "bending_only":
        sagbend_assumption = (
            "Sagbend utilisation uses bending stress ONLY (diagnostic basis); the axial stress "
            "H / A_steel is reported but not added"
        )
    else:
        sagbend_assumption = (
            "Sagbend utilisation uses the COMBINED additive stress (bending + axial), "
            "sigma = E*OD/(2R) + H/A_steel, per DNV-ST-F101 Sec 5"
        )

    report.add_assumptions([
        f"All pipes are {grade} grade (SMYS = {smys_mpa:g} MPa, E = {youngs_gpa:g} GPa)",
        "Catenary solution assumes no current loading on the suspended span",
        "Sagbend and overbend stresses use simplified elastic beam theory",
        sagbend_assumption,
        "Stinger radius approximated as R_ob = L_stinger / sin(theta_dep)",
        "No dynamic amplification applied — quasi-static installation assumed",
        "Pipe submerged weight includes anti-corrosion coating and CWC where applicable",
        "Seabed assumed flat at each water depth — no slope corrections",
        f"Seawater density = {seawater:g} kg/m3 throughout water column",
        f"Installation stress limit factor = {slf:g} per DNV-ST-F101 (2021)",
    ])

    out_path = output_path if output_path is not None else OUTPUT_DIR / "demo_04_shallow_pipelay_report.html"
    # A named-run path adds a fresh <run_id>/ dir; create the parent here. exist_ok keeps the
    # legacy/baseline call a no-op (byte-identical behaviour — main() pre-creates OUTPUT_DIR).
    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    html = report.build(out_path)
    output_path = out_path

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
        # Flatten checks dict for JSON
        checks_flat = {}
        for check_name, check_data in record.get("checks", {}).items():
            if isinstance(check_data, dict):
                for k, v in check_data.items():
                    checks_flat[f"{check_name}_{k}"] = v
        record["checks_flat"] = checks_flat
        clean.append(record)
    return clean


def deserialise_results(json_data: Dict) -> Tuple[List[Dict], pd.DataFrame]:
    """Reconstruct results list and DataFrame from JSON cache."""
    all_results = json_data["cases"]
    rows = []
    for r in all_results:
        rows.append({
            "vessel_id": r.get("vessel_id", ""),
            "vessel_name": r.get("vessel_name", ""),
            "pipe_size": r.get("pipe_size", ""),
            "pipe_od_mm": r.get("pipe_od_mm", 0),
            "pipe_wt_mm": r.get("pipe_wt_mm", 0),
            "water_depth_m": r.get("water_depth_m", 0),
            "overall_status": r.get("overall_status", "ERROR"),
            "max_utilisation": r.get("max_utilisation"),
            "governing_check": r.get("governing_check", "N/A"),
            "H_chosen_kn": r.get("H_chosen_kn", 0),
            "T_top_te": r.get("T_top_te", 0),
            "departure_angle_deg": r.get("departure_angle_deg", 0),
            "sagbend_util": r.get("sagbend_util"),
            "overbend_util": r.get("overbend_util"),
            "tension_util": r.get("tension_util"),
            "sagbend_stress_mpa": r.get("sagbend_stress_mpa", 0),
            "overbend_stress_mpa": r.get("overbend_stress_mpa", 0),
            "R_sag_m": r.get("R_sag_m", 0),
        })

    df = pd.DataFrame(rows)
    return all_results, df


# ---------------------------------------------------------------------------
# Results Store subcommands: `lookup` + `rebuild-db`
#
# Dispatched by an argv-sniff in __main__ so they NEVER touch the existing sweep parser — the
# no-arg / --from-cache / --force / --results-dir / --run-id / --config sweep path stays
# byte-identical. Each subcommand builds its OWN ArgumentParser over sys.argv[2:].
# ---------------------------------------------------------------------------

# Valid overall_status tokens (matches the store's GO/MARGINAL/NO_GO/ERROR taxonomy).
_VALID_STATUSES = ("GO", "MARGINAL", "NO_GO", "ERROR")

# Columns shown by `lookup`, in a clean operator-facing order. demo_04 has no hyphen-prefixed
# columns (the per-check utilisations are flat top-level `*_util` columns), but every identifier
# is still quoted via rs._q() so the idiom stays consistent with the demo_03 store.
_LOOKUP_DISPLAY_COLS = [
    "vessel_id",
    "pipe_size",
    "water_depth_m",
    "overall_status",
    "governing_check",
    "max_utilisation",
    "sagbend_util",
    "overbend_util",
    "tension_util",
]


def lookup_main(argv: List[str]) -> int:
    """`lookup` subcommand: read-only filtered SELECT over the demo_04 Results Store cache.

    Returns a process exit code (0 = ok, non-zero = error such as a missing db).
    """
    import sqlite3

    try:
        import results_store_demo04 as rs
    except ImportError:  # pragma: no cover — packaged import path fallback.
        from examples.demos.gtm import results_store_demo04 as rs

    parser = argparse.ArgumentParser(
        prog="demo_04_shallow_water_pipelay.py lookup",
        description="Look up cases in the demo_04 Results Store (read-only SQLite cache).",
    )
    parser.add_argument("--run-id", type=str, default=BASELINE_RUN_ID,
                        help=f"Run to query (default '{BASELINE_RUN_ID}').")
    parser.add_argument("--vessel-id", type=str, default=None,
                        help="Vessel SHORT id filter (e.g. PLV-001 / PLV-002).")
    parser.add_argument("--pipe-size", type=str, default=None,
                        help="Pipe nominal size filter (e.g. 8in / 12in / 16in / 20in / 24in).")
    parser.add_argument("--water-depth", type=int, default=None,
                        help="Water depth filter in metres, INTEGER (e.g. 30).")
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
        # when provided. --water-depth binds int (the column's INTEGER type, BD-3).
        where = ["run_id = ?"]
        params: List[Any] = [args.run_id]
        if args.vessel_id is not None:
            where.append("vessel_id = ?")
            params.append(str(args.vessel_id))
        if args.pipe_size is not None:
            where.append("pipe_size = ?")
            params.append(str(args.pipe_size))
        if args.water_depth is not None:
            where.append("water_depth_m = ?")
            params.append(int(args.water_depth))
        if args.status is not None:
            where.append("overall_status = ?")
            params.append(str(args.status))

        # Quote EVERY display column via rs._q() (consistent with the demo_03 store idiom).
        cols = ", ".join(rs._q(c) for c in _LOOKUP_DISPLAY_COLS)
        sql = (
            f"SELECT {cols} FROM cases WHERE {' AND '.join(where)} "
            "ORDER BY vessel_id, pipe_size, water_depth_m"
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
        import results_store_demo04 as rs
    except ImportError:  # pragma: no cover — packaged import path fallback.
        from examples.demos.gtm import results_store_demo04 as rs

    parser = argparse.ArgumentParser(
        prog="demo_04_shallow_water_pipelay.py rebuild-db",
        description="Rebuild the demo_04 Results Store SQLite cache + index.csv from "
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


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    """Run the full demo pipeline."""
    parser = argparse.ArgumentParser(
        description="GTM Demo 4: Shallow Water Pipeline Installation Analysis",
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
        help="Override the results directory (legacy JSON + Results Store base). Defaults to "
        "the demo's results/ dir (or the yaml's results_root).",
    )
    parser.add_argument(
        "--run-id",
        type=str,
        default=None,
        help="Run identifier for the Results Store + per-run report dir "
        f"(default the Baseline Run '{BASELINE_RUN_ID}'). A named run writes its report to "
        "output/parametric/demo_04/<run_id>/report.html and seeds <run_id> in the Store.",
    )
    parser.add_argument(
        "--config",
        type=Path,
        default=BASELINE_CONFIG_PATH,
        help="Path to the Sweep Config yaml driving the run (axes, material grade, constants, "
        "criteria, catalogs, artifact paths). Defaults to the committed baseline "
        f"({BASELINE_CONFIG_PATH.name}); point it at your own yaml to run a client config "
        "without editing the baseline.",
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

    # Validate the FULL config once, up front — a schema-invalid (but existing) config must
    # surface a clean error here, not a misleading default-fallback warning followed by a raw
    # SweepConfigError traceback from the recompute reload.
    try:
        from sweep_config_demo04 import (
            SweepConfigError,
            load_demo04_config,
            load_demo04_paths,
        )
    except ImportError:  # pragma: no cover — packaged import path fallback.
        from examples.demos.gtm.sweep_config_demo04 import (
            SweepConfigError,
            load_demo04_config,
            load_demo04_paths,
        )
    try:
        config = load_demo04_config(config_path)
    except SweepConfigError as exc:
        print(f"[ERROR] {exc}")
        sys.exit(2)

    # --from-cache rebuilds the report/charts from a REAL config. The cache on disk was produced
    # by SOME config; the only config we can prove matches it is the committed BASELINE (whose
    # values == the module globals). Refuse --from-cache with a non-default --config.
    is_default_config = config_path.resolve() == Path(BASELINE_CONFIG_PATH).resolve()
    if args.from_cache and not args.force and not is_default_config:
        print(
            "[ERROR] --from-cache is only supported with the committed baseline config. "
            "A non-default --config cannot be proven to match the cached results "
            f"({config_path}); re-run without --from-cache to recompute from your config, "
            "or drop --config to reload the baseline cache."
        )
        sys.exit(2)

    # --from-cache regenerates the Baseline report only (cached data is always the baseline
    # 60 cases). A named --run-id under --from-cache would write a mislabeled report, so warn
    # AND force the report path back to the legacy Baseline location below (mirror demo_03).
    if args.from_cache and args.run_id is not None and args.run_id != BASELINE_RUN_ID:
        warnings.warn(
            f"--from-cache regenerates the baseline report only; --run-id '{args.run_id}' "
            "will not produce a named Results Store row (run without --from-cache to compute "
            "a named run). The report is written to the baseline path.",
            stacklevel=2,
        )

    start_time = time.time()

    print("=" * 60)
    print("  GTM Demo 4: Shallow Water Pipeline Installation Analysis")
    print("=" * 60)

    # Resolve catalog + artifact PATHS from the config yaml (engineering-free). On any load
    # failure fall back to the committed module dirs so the demo still runs.
    results_root = RESULTS_DIR
    output_root = OUTPUT_DIR
    try:
        paths = load_demo04_paths(config_path)
        results_root = paths.results_root
        output_root = paths.output_root
    except Exception as exc:  # noqa: BLE001 — committed-baseline fallback is intentional.
        logger.warning(
            "Could not resolve paths from %s (%s); using committed defaults.",
            config_path, exc,
        )

    # The CLI --results-dir always wins (legacy JSON + Results Store base).
    if args.results_dir:
        results_root = Path(args.results_dir)

    results_path = results_root / "demo_04_shallow_pipelay_results.json"

    # ── Step 1: Load data ──────────────────────────────────────────────────
    print("\n[1/7] Loading input data...")
    vessels = load_vessels(config=config)
    pipes = load_pipes(config=config)
    print(f"  Loaded {len(vessels)} vessels from {config.vessels_path.name}")
    print(f"  Loaded {len(pipes)} pipe sizes from {config.pipelines_path.name}")
    print(f"  Material grade {config.grade}: SMYS={config.smys_pa/1e6:g} MPa, "
          f"SMTS={config.smts_pa/1e6:g} MPa")
    print(f"  Sagbend stress basis: {config.sagbend_stress_basis}")
    for p in pipes:
        pl = pipe_label(p["nominal_size"])
        print(f"    {pl}: OD={p['od_mm']}mm, WT={p['wt_mm']}mm ({p['schedule']}), "
              f"w_sub={p['submerged_weight_n_per_m']:.1f} N/m")

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
        all_results, results_df = run_parametric_sweep(vessels, pipes, config=config)
        total_cases = len(all_results)

        # Results Store (additive): persist the run alongside the legacy JSON/HTML.
        # RECOMPUTE branch only (config is not None). Mirrors demo_03 — a default sweep seeds
        # the committed BASELINE_RUN_ID; a named --run-id seeds that partition. If --from-cache
        # demoted to recompute (stale/missing cache), force BASELINE so we never seed a named
        # row the --from-cache contract said wouldn't exist. Additive: it does NOT touch the
        # JSON/HTML the golden pins.
        try:
            from results_store_demo04 import write_run as _store_write_run
        except ImportError:  # pragma: no cover — packaged import path fallback.
            from examples.demos.gtm.results_store_demo04 import (
                write_run as _store_write_run,
            )
        _store_write_run(
            run_id=(BASELINE_RUN_ID if args.from_cache else (args.run_id or BASELINE_RUN_ID)),
            config=config,
            results=all_results,
            base_dir=results_root,
        )

    # ── Step 3: Build charts ───────────────────────────────────────────────
    print("\n[3/7] Building charts...")
    fig1 = build_chart_1_go_nogo_matrix(results_df, config=config)
    fig2 = build_chart_2_sagbend_heatmap(results_df, config=config)
    fig3 = build_chart_3_tension_vs_depth(results_df, vessels=vessels, config=config)
    fig4 = build_chart_4_departure_angle(results_df, vessels=vessels, config=config)
    fig5 = build_chart_5_vessel_comparison(results_df, config=config)

    # ── Step 4: Build summary table ────────────────────────────────────────
    print("\n[4/7] Building summary table...")
    summary_df = build_summary_table(results_df, config=config)

    # ── Step 5: Build HTML report ──────────────────────────────────────────
    print("\n[5/7] Building HTML report...")
    output_root.mkdir(parents=True, exist_ok=True)
    # Baseline Run keeps the legacy output path (byte-identical); a named Run writes a per-run
    # report under output/parametric/demo_04/<run_id>/report.html. Under --from-cache the data
    # is always the baseline 60, so a named run is forced back to the baseline path.
    if args.run_id is None or args.run_id == BASELINE_RUN_ID or args.from_cache:
        report_path = output_root / "demo_04_shallow_pipelay_report.html"
    else:
        report_path = output_root / "parametric" / DEMO_ID / args.run_id / "report.html"
    build_report(
        fig1, fig2, fig3, fig4, fig5, summary_df, all_results, total_cases,
        vessels=vessels, config=config, output_path=report_path,
    )
    if args.run_id is not None and args.run_id != BASELINE_RUN_ID and not args.from_cache:
        print(f"  Per-run report written to: {report_path}")

    # ── Step 6: Save JSON results ──────────────────────────────────────────
    if not args.from_cache or args.force:
        print("\n[6/7] Saving JSON results...")
        results_root.mkdir(parents=True, exist_ok=True)

        json_output = {
            "metadata": {
                "demo": "GTM Demo 4: Shallow Water Pipeline Installation Analysis",
                "total_cases": total_cases,
                "vessels": [v["name"] for v in vessels],
                "pipe_sizes": [p["nominal_size"] for p in pipes],
                "water_depths_m": list(config.water_depths),
                "material_grade": config.grade,
                "sagbend_stress_basis": config.sagbend_stress_basis,
                "constants": {
                    "seawater_density_kg_m3": config.seawater_density_kg_m3,
                    "gravity_m_s2": config.gravity_m_s2,
                    "steel_density_kg_m3": config.steel_density_kg_m3,
                    "youngs_modulus_pa": config.youngs_modulus_pa,
                    "smys_pa": config.smys_pa,
                    "smts_pa": config.smts_pa,
                    "stress_limit_factor": config.stress_limit_factor,
                    "tension_margin": config.tension_margin,
                    "go_threshold": config.go_threshold,
                    "nogo_utilisation": config.nogo_utilisation,
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
    print(f"  HTML report:           output/demo_04_shallow_pipelay_report.html")
    print(f"  JSON results:          results/demo_04_shallow_pipelay_results.json")
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
