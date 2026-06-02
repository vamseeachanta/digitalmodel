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
# ADR-0005: ALL compute-path config lives in inputs/demo_05_jumper.yml, resolved by
# sweep_config_demo05.load_demo05_config into a ResolvedDemo05Config that is threaded into the
# phases, charts, summary and report. The module-level literals below are the FALLBACK BASELINE
# used only when a caller passes config=None (e.g. a bare unit test); they MUST stay numerically
# identical to inputs/demo_05_jumper.yml so a config=None run reproduces the baseline. The
# baseline yaml is the single source of truth.
SCRIPT_DIR = Path(__file__).resolve().parent
DATA_DIR = SCRIPT_DIR / "data"
OUTPUT_DIR = SCRIPT_DIR / "output"
RESULTS_DIR = SCRIPT_DIR / "results"
BASELINE_CONFIG_PATH = SCRIPT_DIR / "inputs" / "demo_05_jumper.yml"

# The Baseline Run id — the committed reference run a default sweep seeds
# (mirrors demo_04; matches results_store_demo05.DEMO_ID partition).
BASELINE_RUN_ID = "baseline"

# Demo identifier — the per-run report/store partition name (matches results_store_demo05.DEMO_ID).
DEMO_ID = "demo_05"


def validate_run_id(run_id: str) -> str:
    """Validate a run_id is a single safe path segment; return it unchanged if valid.

    A run_id becomes a filesystem directory name under the Results Store
    (<base_dir>/parametric/demo_05/<run_id>/) and the per-run report dir, so it must be a
    single safe path segment: no separators, no "." / ".." traversal, no empty string
    (path-traversal guard). Delegates to the store's regex so both stay in lock-step. Raises
    ValueError with a clean message on rejection.
    """
    try:
        import results_store_demo05 as _rs
    except ImportError:  # pragma: no cover — packaged import path fallback.
        from examples.demos.gtm import results_store_demo05 as _rs
    return _rs._validate_run_id(run_id)

# Physical constants
SEAWATER_DENSITY = 1025.0   # kg/m³
GRAVITY = 9.80665           # m/s²

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
BENDING_ALLOWABLE = 0.6       # Allowable fraction of SMYS for in-air bending
TIE_IN_TOLERANCE_MM = 50.0    # Lateral offset tolerance (mm)

# Cable properties (typical offshore installation wire rope)
CABLE_UNIT_WEIGHT_SUB = 50.0  # N/m submerged weight of wire rope

# Phase-2 lift-span model defaults (the fix). See inputs/demo_05_jumper.yml lift_span_model.
# The lift span is an explicit length-scaling fraction of horizontal_span_m (absolute override
# wins when set). 0.5 = single mid-pick (one unsupported bay = half the run). Conservative.
LIFT_SPAN_FRACTION = 0.5
LIFT_SPAN_M = None            # absolute override (m); None => use the fraction
LIFT_MOMENT_COEFF = 8.0       # simply-supported midspan basis M = w s^2 / 8 (conservative)

# Phase-5 tie-in span model defaults (the fix). See inputs/demo_05_jumper.yml tiein_span_model.
# The tie-in free span is an explicit length-scaling fraction of horizontal_span_m (absolute
# override wins when set). 0.28 = one unsupported leg adjacent to the connector. Conservative.
TIEIN_UNSUPPORTED_SPAN_FRACTION = 0.28
TIEIN_UNSUPPORTED_SPAN_M = None  # absolute override (m); None => use the fraction
TIEIN_INCLUDE_SELF_WEIGHT = False  # RATIFIED: lateral tie-in check is current-driven; the
                                   # vertical self-weight sag is reacted by the lowering rigging
                                   # (different DOF). true = deliberate conservative-magnitude stance.
TIEIN_DEFLECTION_COEFF = 76.8   # simply-supported UDL basis delta = w s^4 / (76.8 E I) (conserv.)


# ---------------------------------------------------------------------------
# Config accessor — resolve a value from the threaded ResolvedDemo05Config, falling back to the
# module-level baseline literal when config is None (config=None reproduces the baseline).
# ---------------------------------------------------------------------------

def _cfg(config: Optional[Any], attr: str, fallback: Any) -> Any:
    """Return ``getattr(config, attr)`` when a config is threaded, else the baseline ``fallback``."""
    if config is None:
        return fallback
    return getattr(config, attr)


# ---------------------------------------------------------------------------
# Data loaders
# ---------------------------------------------------------------------------

def load_vessel_data(path: Optional[Path] = None) -> List[Dict[str, Any]]:
    """Load vessel data from csv_hlv_vessels.json (or the config-resolved path)."""
    path = Path(path) if path is not None else DATA_DIR / "csv_hlv_vessels.json"
    with open(path, "r") as f:
        data = json.load(f)
    vessels = data["vessels"]
    print(f"  Loaded {len(vessels)} vessels from csv_hlv_vessels.json")
    return vessels


def load_jumper_data(path: Optional[Path] = None) -> Tuple[Dict[str, Any], List[Dict[str, Any]]]:
    """Load rigid jumper data from rigid_jumpers.json (or the config-resolved path).

    Returns (common_properties, jumper_list).
    """
    path = Path(path) if path is not None else DATA_DIR / "rigid_jumpers.json"
    with open(path, "r") as f:
        data = json.load(f)
    common = data["common_properties"]
    jumpers = data["jumpers"]
    print(f"  Loaded {len(jumpers)} jumper lengths from rigid_jumpers.json")
    return common, jumpers


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def hs_to_tp(hs: float, config: Optional[Any] = None) -> float:
    """Approximate peak spectral period from Hs using Tp ≈ coeff × sqrt(Hs).

    This is a simplified relationship from typical JONSWAP spectra. The coefficient is the
    threaded config's ``tp_coefficient`` (baseline 4.0).
    """
    coeff = _cfg(config, "tp_coefficient", 4.0)
    return coeff * math.sqrt(hs)


def utilisation_color(util: float) -> str:
    """Return hex color for utilisation: green / amber / red."""
    if util < 0.70:
        return COLORS["success"]
    elif util <= 0.90:
        return COLORS["warning"]
    else:
        return COLORS["danger"]


def status_from_utils(phase_utils: Dict[str, float], config: Optional[Any] = None) -> str:
    """Determine GO / MARGINAL / NO_GO from phase utilisations.

    Bands come from the threaded config (``nogo_utilisation`` / ``go_marginal_threshold``;
    baselines 1.0 / 0.85).
    """
    nogo = _cfg(config, "nogo_utilisation", 1.0)
    marginal = _cfg(config, "go_marginal_threshold", 0.85)
    max_util = max(phase_utils.values()) if phase_utils else 0.0
    if max_util > nogo:
        return "NO_GO"
    elif max_util >= marginal:
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
    config: Optional[Any] = None,
) -> Dict[str, Any]:
    """Phase 1: Lift-off — hook load vs crane SWL.

    hook_load = mass_total × g × DAF_lift
    mass_total = mass_per_m × length + rigging_mass
    """
    g = _cfg(config, "gravity_m_s2", GRAVITY)
    daf_lift = _cfg(config, "daf_liftoff", DAF_LIFT)
    rigging_kg = _cfg(config, "rigging_mass_kg", RIGGING_MASS_KG)

    mass_per_m = common["mass_per_meter"]["total_air_kg_per_m"]
    length = jumper["length_m"]
    mass_total = mass_per_m * length + rigging_kg

    hook_load_n = mass_total * g * daf_lift
    hook_load_te = hook_load_n / (g * 1000.0)  # convert to tonnes

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
    """Phase 2: In-air lift bending stress over an explicit, length-scaling lift span (FIX).

    DEFECT 1 (original): the jumper was modelled as ONE simply-supported straight beam of the
    FULL ``length_m`` with M = w·length²/8 → σ ≈ 2120 MPa (~4.7× SMYS) at L=100 m, FAILED for
    L ≥ 40 m. A rigid jumper is rigged on a spreader bar with MULTIPLE sling picks; the governing
    bending span is the longest UNSUPPORTED run between picks, a fraction of the jumper.

    DEFECT 2 (first fix, non-discriminating): deriving lift_span = horizontal_span_m / (n_picks−1)
    with n_picks tied to number_of_bends gave a NEARLY CONSTANT span (15 m for every L≥40, because
    bends grow in lock-step with horizontal_span), so utilisation barely moved with jumper length.

    MODEL (USER-RATIFIED fix — EXPLICIT length-scaling span, conservative BC):
      lift_span = lift_span_m              IF set (> 0)  [absolute override wins],
                = lift_span_fraction · horizontal_span_m   otherwise (scales with the jumper).
      w_eff     = air_n_per_m · daf_liftoff                (self-weight, amplified by lift DAF).
      M         = w_eff · lift_span² / moment_coeff        (moment_coeff = 8 simply-supported =
                                                            CONSERVATIVE default; 12 fixed-fixed).
      σ         = M · (OD/2) / I.
      Check: σ ≤ bending_allowable · SMYS  (baseline 0.6 × 448 MPa = 268.8 MPa).

    lift_span_fraction, the absolute override, moment_coeff and the lift DAF are all YAML params
    so the model is reviewable/editable without code changes. lift_span_fraction = 0.5 is a
    CONSERVATIVE screening JUDGMENT (single mid-pick → one unsupported bay = half the run);
    flagged for domain re-review.

    HAND-CHECK (L=100 m, baseline): horizontal_span_m = 75, lift_span_fraction = 0.5 →
      lift_span = 0.5 · 75 = 37.5 m
      w_eff = 906.49 · 1.10 = 997.14 N/m ; M = 997.14 · 37.5² / 8 = 175.25 kN·m
      σ = 175.25e3 · (0.2191/2) / 5.857e-5 = 327.8 MPa → util = 327.8 / 268.8 = 1.220.
      In-air bending NO_GOes the 100 m jumper and GOVERNS all cases: with the ratified current-only
      tie-in basis the lateral tie-in deflection collapses (util ~0.14 at L=100 m) and never governs.
      For the 20 m jumper (horizontal_span 15): lift_span = 7.5 m, σ ≈ 13.1 MPa, util 0.049 → GO.
    """
    w_air = common["weight_per_meter"]["air_n_per_m"]  # N/m
    od = common["od_m"]
    i_steel = common["i_steel_m4"]
    smys = common["smys_pa"]

    daf_lift = _cfg(config, "daf_liftoff", DAF_LIFT)
    bending_allow = _cfg(config, "bending_allowable", BENDING_ALLOWABLE)
    span_fraction = _cfg(config, "lift_span_fraction", LIFT_SPAN_FRACTION)
    span_override = _cfg(config, "lift_span_m", LIFT_SPAN_M)
    moment_coeff = _cfg(config, "lift_moment_coeff", LIFT_MOMENT_COEFF)

    # Explicit, length-scaling lift span: absolute override wins, else fraction of the run.
    cfgn = jumper["configuration"]
    horizontal_span = cfgn["horizontal_span_m"]
    if span_override is not None and span_override > 0:
        lift_span = float(span_override)
    else:
        lift_span = span_fraction * horizontal_span

    # Effective uniform lift load = self-weight × lift DAF (the dominant load, not current).
    w_eff = w_air * daf_lift

    # Governing bending moment over the unsupported span (boundary condition via moment_coeff).
    m_max = w_eff * lift_span**2 / moment_coeff

    # Bending stress at the extreme fibre.
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
    """Phase 3: Splash zone — slamming + drag + DAF.

    Slamming: F_slam = 0.5 × ρ × Cs × Ap × v_rel²
    Drag: F_drag = 0.5 × ρ × Cd × A_side × v_lowering²
    """
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
    mass_per_m = common["mass_per_meter"]["total_air_kg_per_m"]
    mass_total = mass_per_m * length + rigging_kg
    w_air = mass_total * g

    # Splash zone submerged length
    submerged_len = min(length, splash_sub)

    # Projected area for slamming
    ap = od * submerged_len

    # Wave velocity (simplified): v_wave = π × Hs / Tp
    tp = hs_to_tp(hs, config=config)
    v_wave = math.pi * hs / tp

    # Relative velocity
    v_rel = v_low + v_wave

    # Slamming force
    f_slam = 0.5 * rho * cs * ap * v_rel**2

    # Drag force on submerged length
    a_side = od * submerged_len
    f_drag = 0.5 * rho * cd * a_side * v_low**2

    # Total hook load in splash
    hook_splash_n = w_air + f_slam + f_drag
    hook_splash_daf_n = hook_splash_n * daf_splash
    hook_splash_te = hook_splash_daf_n / (g * 1000.0)

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
    """Phase 4: Lowering through water column — cable tension + snap load.

    Cable tension: T = W_sub + W_cable_sub(depth)
    Snap load: dynamic = (mass_total + added_mass) × ω² × heave_amp
    Check: max_cable_tension ≤ 0.85 × wire_MBL
    """
    g = _cfg(config, "gravity_m_s2", GRAVITY)
    rho = _cfg(config, "seawater_density_kg_m3", SEAWATER_DENSITY)
    ca = _cfg(config, "ca_cylinder", CA_CYLINDER)
    cable_w = _cfg(config, "cable_unit_weight_sub_n_per_m", CABLE_UNIT_WEIGHT_SUB)
    wire_factor = _cfg(config, "wire_allowable_factor", WIRE_ALLOWABLE_FACTOR)
    rigging_kg = _cfg(config, "rigging_mass_kg", RIGGING_MASS_KG)

    od = common["od_m"]
    length = jumper["length_m"]
    sub_w_per_m = common["weight_per_meter"]["submerged_n_per_m"]
    mass_per_m = common["mass_per_meter"]["total_air_kg_per_m"]

    # Static loads
    w_sub = sub_w_per_m * length
    w_cable_sub = cable_w * depth
    static_tension = w_sub + w_cable_sub

    # Dynamic loads
    mass_total = mass_per_m * length + rigging_kg
    added_mass = rho * math.pi / 4.0 * od**2 * length * ca

    tp = hs_to_tp(hs, config=config)
    omega = 2.0 * math.pi / tp

    heave_rao = vessel["motion_characteristics"]["heave_rao"]["peak_amplitude_m_per_m"]
    heave_amp = heave_rao * (hs / 2.0)

    dynamic_load = (mass_total + added_mass) * omega**2 * heave_amp

    max_tension = static_tension + dynamic_load
    max_tension_te = max_tension / (g * 1000.0)

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
    """Phase 5: Tie-in connector mating — free-span deflection vs connector tolerance (FIX).

    DEFECT 1 (original): the TOTAL distributed current force W = w·length over the FULL jumper was
    fed into a central POINT-LOAD deflection δ = W·length³/(48EI), giving 5789 mm / util 116 at
    L=100 m (misapplied point-load formula AND whole-jumper free span). The dominant transverse
    load (submerged self-weight ≈ 506 N/m, vs current ≈ 34 N/m) was never computed.

    DEFECT 2 (first fix, non-discriminating): deriving tiein_span = horizontal_span_m /
    number_of_bends pinned the span at ~7.5 m for every L≥40 (bends grow with horizontal_span),
    so the deflection utilisation stayed < 0.01 for all cases — non-discriminating.

    MODEL (USER-RATIFIED fix — EXPLICIT length-scaling free span, conservative BC, API 17R):
    The connector-mating tolerance applies to the offset at a hub, governed by the deflection of
    ONE unsupported leg adjacent to the connector during stab-in — NOT the whole jumper sag. The
    free span SCALES with the jumper:
      tiein_span = tiein_unsupported_span_m              IF set (> 0)  [absolute override wins],
                 = tiein_unsupported_span_fraction · horizontal_span_m   otherwise (scales).
      w_current  = 0.5 · ρ · Cd · OD · v_current²        (lateral, weather/current-driven).
      w_sub      = submerged_n_per_m                      (vertical self-weight; toggled).
      w_res      = w_current alone (RATIFIED DEFAULT — lateral, current-driven) else
                   sqrt(w_sub² + w_current²)  if include_self_weight (conservative-magnitude).
      δ          = w_res · tiein_span⁴ / (deflection_coeff · E · I)
                   deflection_coeff = 76.8 simply-supported UDL = CONSERVATIVE default;
                   384 fixed-fixed UDL selectable.
      Check: δ ≤ tie_in_tolerance_mm  (connector-mating tolerance, API 17R subsea connectors).

    All YAML params: tiein_unsupported_span_fraction (0.28 CONSERVATIVE JUDGMENT — one
    unsupported leg adjacent to the connector before touchdown; flagged for re-review), the
    absolute override, the self-weight toggle, deflection_coeff, current velocity, tolerance.

    SELF-WEIGHT TOGGLE (RATIFIED): include_self_weight defaults FALSE = CURRENT-DRIVEN LATERAL.
    The ±50 mm tie-in tolerance is a LATERAL connector-mating tolerance, so the check must be
    driven by the LATERAL current load alone (~34 N/m). The vertical submerged self-weight sag
    (~506 N/m) is reacted by the lowering/support rigging — a DIFFERENT degree of freedom; folding
    it into this lateral check was a DOF category error. The toggle remains engineer-tunable: set
    it true for a deliberate conservative-MAGNITUDE stance (folds the vertical self-weight into the
    resultant => larger δ), but that mixes DOFs and is no longer the screening default.

    HAND-CHECK (L=100 m, baseline, current-only RATIFIED default): horizontal_span_m=75,
    tiein_unsupported_span_fraction=0.28 →
      tiein_span = 0.28 · 75 = 21.0 m
      w_current = 0.5·1025·1.2·0.2191·0.5² = 33.69 N/m ; w_res = w_current = 33.69 N/m
      δ = 33.69 · 21.0⁴ / (76.8 · 207e9 · 5.857e-5) = 0.00704 m = 7.04 mm → util = 7.04/50 ≈ 0.14
      → GO. The lateral tie-in deflection collapses and NEVER governs; in-air lift bending
      (util 1.22) is the NO_GO driver for the 100 m jumper. For the 20 m jumper (horizontal_span 15,
      span 4.2 m): δ ≈ 0.011 mm, util ~0.0002 → GO.
      (If include_self_weight=true: w_res = sqrt(506.41² + 33.69²) = 507.53 N/m, δ = 106.0 mm,
      util 2.120 → tie-in would then govern — the conservative-magnitude alternative.)
    """
    rho = _cfg(config, "seawater_density_kg_m3", SEAWATER_DENSITY)
    cd = _cfg(config, "cd_cylinder", CD_CYLINDER)
    v_cur = _cfg(config, "v_current", V_CURRENT)
    e_steel = common["youngs_modulus_pa"]
    tolerance_mm = _cfg(config, "tie_in_tolerance_mm", TIE_IN_TOLERANCE_MM)
    span_fraction = _cfg(config, "tiein_unsupported_span_fraction", TIEIN_UNSUPPORTED_SPAN_FRACTION)
    span_override = _cfg(config, "tiein_unsupported_span_m", TIEIN_UNSUPPORTED_SPAN_M)
    include_sw = _cfg(config, "tiein_include_self_weight", TIEIN_INCLUDE_SELF_WEIGHT)
    defl_coeff = _cfg(config, "tiein_deflection_coeff", TIEIN_DEFLECTION_COEFF)

    od = common["od_m"]
    i_steel = common["i_steel_m4"]
    sub_w_per_m = common["weight_per_meter"]["submerged_n_per_m"]

    # Explicit, length-scaling free span: absolute override wins, else fraction of the run.
    cfgn = jumper["configuration"]
    horizontal_span = cfgn["horizontal_span_m"]
    if span_override is not None and span_override > 0:
        tiein_span = float(span_override)
    else:
        tiein_span = span_fraction * horizontal_span

    # Distributed transverse loads on the free span.
    w_current = 0.5 * rho * cd * od * v_cur**2            # N/m, lateral
    w_sub = sub_w_per_m if include_sw else 0.0            # N/m, vertical self-weight
    w_res = math.hypot(w_sub, w_current)                 # resultant transverse UDL

    # Distributed-load beam deflection over the free span (boundary condition via defl_coeff).
    delta = w_res * tiein_span**4 / (defl_coeff * e_steel * i_steel)
    delta_mm = delta * 1000.0

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


# ---------------------------------------------------------------------------
# Single case runner
# ---------------------------------------------------------------------------

def run_single_case(
    vessel: Dict[str, Any],
    jumper: Dict[str, Any],
    common: Dict[str, Any],
    depth: float,
    hs: float,
    config: Optional[Any] = None,
) -> Dict[str, Any]:
    """Run all 5 installation phases for a single parameter combination."""
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

    overall = status_from_utils(phase_utils, config=config)
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
        "tp_s": round(hs_to_tp(hs, config=config), 2),
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
    config: Optional[Any] = None,
) -> Tuple[List[Dict], pd.DataFrame]:
    """Run the full parametric sweep.

    The sweep axes come from the threaded ``config`` (ADR-0005). The cross-product nesting is
    FROZEN: vessel (outer) -> jumper length -> depth -> hs (inner), matching the 300-case golden.
    When ``config`` is None the module-level baseline axes are used (config=None reproduces the
    baseline). The catalog ``vessels`` / ``jumpers`` lists are filtered to the config's selected
    names/lengths, preserving the config axis ORDER.
    """
    # Resolve the swept axes from the config, falling back to module baselines (config=None).
    if config is not None:
        # Select catalog entries by the config axis order (vessel names, jumper lengths).
        by_vessel = {v["name"]: v for v in vessels}
        swept_vessels = [by_vessel[name] for name in config.vessels]
        by_length = {j["length_m"]: j for j in jumpers}
        swept_jumpers = [by_length[float(L)] for L in config.lengths_m]
        depths = list(config.depths)
        hs_values = list(config.hs)
    else:
        swept_vessels = list(vessels)
        swept_jumpers = list(jumpers)
        depths = list(WATER_DEPTHS)
        hs_values = list(HS_VALUES)

    total = len(swept_vessels) * len(swept_jumpers) * len(depths) * len(hs_values)

    print(f"\n{'='*60}")
    print(f"  PARAMETRIC JUMPER INSTALLATION SWEEP")
    print(f"  {total} cases: {len(swept_vessels)} vessels x {len(depths)} depths"
          f" x {len(swept_jumpers)} jumpers x {len(hs_values)} Hs")
    print(f"{'='*60}\n")

    all_results: List[Dict] = []
    case_num = 0

    for vessel in swept_vessels:
        for jumper in swept_jumpers:
            for depth in depths:
                for hs in hs_values:
                    case_num += 1
                    try:
                        result = run_single_case(vessel, jumper, common, depth, hs, config=config)
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

def build_chart_1_heatmap(
    all_results: List[Dict], vessels: List[Dict], config: Optional[Any] = None
) -> go.Figure:
    """Two-subplot heatmap: Large CSV | Medium CSV.

    X: water depth, Y: jumper length.
    Cell color: GO/MARGINAL/NO_GO at the reference Hs.
    Annotation: governing phase + utilisation %.
    """
    print("\n[Chart 1] Building Go/No-Go Heatmap...")

    ref_hs = _cfg(config, "reference_hs", REFERENCE_HS)
    cfg_depths = list(config.depths) if config is not None else list(WATER_DEPTHS)

    # Filter to reference Hs
    ref_results = [r for r in all_results if abs(r.get("hs_m", 0) - ref_hs) < 0.01]

    vessel_names = [v["name"] for v in vessels]
    depths = sorted(cfg_depths)
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
            text=f"Go/No-Go Matrix at Hs = {ref_hs} m",
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
    config: Optional[Any] = None,
) -> go.Figure:
    """X: jumper length (m), Y: bending stress (MPa).
    Reference line at 0.6×SMYS allowable.
    """
    print("\n[Chart 2] Building In-air Bending Stress chart...")

    bending_allow = _cfg(config, "bending_allowable", BENDING_ALLOWABLE)

    lengths = []
    stresses = []
    for j in jumpers:
        p2 = calc_phase_2_inair_bending(j, common, config=config)
        lengths.append(j["length_m"])
        stresses.append(p2["bending_stress_mpa"])

    allowable_mpa = bending_allow * common["smys_pa"] / 1e6

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
        annotation_text=f"{bending_allow:g} x SMYS = {allowable_mpa:.0f} MPa",
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
    config: Optional[Any] = None,
) -> go.Figure:
    """X: jumper length, Y: max allowable Hs. One curve per vessel.

    For each vessel and jumper length, find the max Hs where the case is
    still GO or MARGINAL at the reference depth (mid-range swept depth).
    """
    print("\n[Chart 3] Building Max Hs Limit chart...")

    cfg_depths = sorted(config.depths) if config is not None else sorted(WATER_DEPTHS)
    # Mid-range reference depth: keep 1500 m when it is in the swept set (baseline), else the
    # middle swept depth.
    ref_depth = 1500 if 1500 in cfg_depths else cfg_depths[len(cfg_depths) // 2]

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
    ref_hs = _cfg(config, "reference_hs", REFERENCE_HS)
    fig.add_hline(
        y=ref_hs, line_dash="dot", line_color=COLORS["warning"],
        line_width=1.5,
        annotation_text=f"Typical Hs={ref_hs}m reference",
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
    config: Optional[Any] = None,
) -> go.Figure:
    """X: depth, Y: cable tension (te). Lines per jumper length.

    Use the Large CSV vessel at reference Hs.
    """
    print("\n[Chart 4] Building Cable Tension chart...")

    wire_factor = _cfg(config, "wire_allowable_factor", WIRE_ALLOWABLE_FACTOR)
    ref_hs = _cfg(config, "reference_hs", REFERENCE_HS)
    cfg_depths = list(config.depths) if config is not None else list(WATER_DEPTHS)

    large_vessel = vessels[0]  # Large CSV
    vname = large_vessel["name"]
    wire_mbl_te = large_vessel["crane_main"]["main_wire_mbl_te"]
    allowable_te = wire_factor * wire_mbl_te

    fig = go.Figure()

    for j_idx, jumper in enumerate(jumpers):
        length = jumper["length_m"]
        depths_plot = []
        tensions = []

        for depth in cfg_depths:
            match = [r for r in all_results
                     if r.get("vessel") == vname
                     and r.get("length_m") == length
                     and r.get("water_depth_m") == depth
                     and abs(r.get("hs_m", 0) - ref_hs) < 0.01]
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
        annotation_text=f"{wire_factor:g} x Wire MBL = {allowable_te:.0f} te",
        annotation_position="top left",
    )

    fig.update_layout(
        title=dict(
            text=f"Cable Tension vs Water Depth — {vname} at Hs={ref_hs}m",
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
    config: Optional[Any] = None,
) -> go.Figure:
    """X: water depth, Y: max jumper length installable. One bar per vessel.

    At the reference Hs.
    """
    print("\n[Chart 5] Building Vessel Comparison chart...")

    ref_hs = _cfg(config, "reference_hs", REFERENCE_HS)
    cfg_depths = list(config.depths) if config is not None else list(WATER_DEPTHS)

    ref_results = [r for r in all_results if abs(r.get("hs_m", 0) - ref_hs) < 0.01]
    vessel_names = [v["name"] for v in vessels]
    all_lengths = sorted(set(j["length_m"] for j in jumpers))

    fig = go.Figure()
    colors = [CHART_PALETTE[0], CHART_PALETTE[1]]

    for v_idx, vname in enumerate(vessel_names):
        depths_plot = []
        max_lengths = []

        for depth in cfg_depths:
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
            text=f"Max Installable Jumper Length by Vessel at Hs={ref_hs}m",
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

def build_summary_table(all_results: List[Dict], config: Optional[Any] = None) -> pd.DataFrame:
    """Build summary DataFrame: vessel × jumper length → status at reference Hs + 1500m."""
    ref_hs = _cfg(config, "reference_hs", REFERENCE_HS)
    cfg_depths = sorted(config.depths) if config is not None else sorted(WATER_DEPTHS)
    ref_depth = 1500 if 1500 in cfg_depths else cfg_depths[len(cfg_depths) // 2]
    rows = []
    for r in all_results:
        if (abs(r.get("hs_m", 0) - ref_hs) < 0.01
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
                "Hs (m)": ref_hs,
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
    config: Optional[Any] = None,
    output_path: Optional[Path] = None,
    common: Optional[Dict[str, Any]] = None,
) -> str:
    """Build the branded HTML report. Narrative reflects the threaded config + catalog."""
    print("\n[Report] Building HTML report...")

    ref_hs = _cfg(config, "reference_hs", REFERENCE_HS)
    daf_lift = _cfg(config, "daf_liftoff", DAF_LIFT)
    daf_splash = _cfg(config, "daf_splash", DAF_SPLASH)
    bending_allow = _cfg(config, "bending_allowable", BENDING_ALLOWABLE)
    wire_factor = _cfg(config, "wire_allowable_factor", WIRE_ALLOWABLE_FACTOR)
    v_cur = _cfg(config, "v_current", V_CURRENT)
    tol_mm = _cfg(config, "tie_in_tolerance_mm", TIE_IN_TOLERANCE_MM)
    moment_coeff = _cfg(config, "lift_moment_coeff", LIFT_MOMENT_COEFF)
    defl_coeff = _cfg(config, "tiein_deflection_coeff", TIEIN_DEFLECTION_COEFF)
    marginal = _cfg(config, "go_marginal_threshold", 0.85)
    nogo = _cfg(config, "nogo_utilisation", 1.0)
    span_frac = _cfg(config, "lift_span_fraction", LIFT_SPAN_FRACTION)
    tiein_frac = _cfg(config, "tiein_unsupported_span_fraction", TIEIN_UNSUPPORTED_SPAN_FRACTION)
    include_sw = _cfg(config, "tiein_include_self_weight", TIEIN_INCLUDE_SELF_WEIGHT)
    # Material props come from the catalog (the single source of truth), not hardcoded literals,
    # so the report labels never desync from a changed catalog.
    smys_mpa = common["smys_mpa"] if common is not None else 448.0
    e_gpa = (common["youngs_modulus_pa"] / 1e9) if common is not None else 207.0
    grade = common["grade"] if common is not None else "X65"
    allow_mpa = bending_allow * smys_mpa  # bending_allowable x SMYS
    sw_clause = (
        "the resultant of the <strong>submerged self-weight (~506 N/m)</strong> and"
        if include_sw
        else "<strong>the lateral current load alone</strong> (current-driven; vertical "
        "self-weight sag is reacted by the lowering rigging, a different DOF), namely"
    )
    # Phase-5 governing-behaviour clause: with the ratified current-only lateral basis the tie-in
    # deflection collapses and lift bending governs; with self-weight included it grows as length^4
    # and governs the longest jumpers (the deliberate conservative-magnitude stance).
    tiein_gov_clause = (
        f"Because deflection grows as length&sup4;, this phase <strong>governs the longest "
        f"jumpers</strong>: at L=100 m the {tiein_frac:g}&times;75 m = {tiein_frac*75:.0f} m free "
        f"span exceeds the {tol_mm:.0f} mm tolerance."
        if include_sw
        else f"With the ratified current-only lateral basis the tie-in deflection is small "
        f"(at L=100 m the {tiein_frac:g}&times;75 m = {tiein_frac*75:.0f} m free span gives "
        f"&delta; &asymp; 7 mm, util &asymp; 0.14), so this phase <strong>never governs</strong>; "
        f"in-air lift bending governs throughout. (Selecting include_self_weight would fold in the "
        f"vertical self-weight and let this length&sup4;-scaling phase govern the longest jumpers.)"
    )

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
            "API 17R (2015) — Subsea Production System Connectors (tie-in tolerance basis)",
            "API 5L (2018) — Line Pipe (X65 material properties)",
        ],
    )

    # Methodology section — Phase 2 and Phase 5 describe the M/W-geometry span models (the fix).
    methodology_html = f"""
    <p>This analysis screens deepwater rigid jumper installation feasibility
    across a parametric matrix of vessels, water depths, jumper lengths, and
    sea states. Each case evaluates five installation phases:</p>

    <h3>Phase 1: Lift-off</h3>
    <p>Verifies the crane can lift the jumper plus rigging (5 te) with a
    <strong>DAF of {daf_lift}</strong>. Hook load = (mass_per_m &times; length + rigging)
    &times; g &times; DAF. Checked against crane SWL.</p>

    <h3>Phase 2: In-air Lift Bending (Spreader-Bar Span Model)</h3>
    <p>A rigid jumper is lifted on a <strong>spreader bar with sling picks</strong>, not as one
    full-length beam. The governing bending span is the longest <strong>unsupported run between
    picks</strong>, taken as an explicit, length-scaling fraction of the horizontal run:
    lift_span = <strong>{span_frac:g} &times; horizontal_span</strong> (an absolute override is
    also available). The effective uniform load is the self-weight amplified by the lift DAF,
    w<sub>eff</sub> = w<sub>air</sub> &times; {daf_lift}. The governing moment is
    M = w<sub>eff</sub> &times; lift_span&sup2; &divide; {moment_coeff:.0f} (simply-supported
    midspan basis, conservative), &sigma; = M &times; (OD/2) / I. Stress must not exceed
    <strong>{bending_allow} &times; SMYS</strong> ({allow_mpa:.1f} MPa for {grade}). Because the
    span scales with the jumper, bending utilisation grows as length&sup2; &mdash; this phase
    governs the short/medium jumpers.</p>

    <h3>Phase 3: Splash Zone</h3>
    <p>Hydrodynamic loads during passage through the air-water interface.
    Slamming force uses C<sub>s</sub> = &pi; for circular pipe sections.
    Combined with drag and a <strong>DAF of {daf_splash}</strong> for splash zone dynamics.</p>

    <h3>Phase 4: Lowering Through Water Column</h3>
    <p>Cable tension = submerged weight + cable weight + dynamic snap load.
    Dynamic load accounts for heave RAO and added mass (C<sub>a</sub> = 1.0).
    Checked against <strong>{wire_factor:.0%} of wire MBL</strong>.</p>

    <h3>Phase 5: Tie-in Connector Mating (Free-Span Deflection Model)</h3>
    <p>The connector-mating tolerance (&plusmn;{tol_mm:.0f} mm, API 17R basis) applies to the
    offset at the hub, governed by the deflection of one <strong>unsupported leg</strong>
    adjacent to the connector during stab-in &mdash; NOT whole-jumper sag. The free span is an
    explicit, length-scaling fraction of the run:
    tiein_span = <strong>{tiein_frac:g} &times; horizontal_span</strong> (an absolute override is
    also available). The transverse distributed load is {sw_clause} the lateral current load
    (0.5 &times; &rho; &times; C<sub>d</sub> &times; OD &times; {v_cur}&sup2; &asymp; 34 N/m):
    &delta; = w<sub>res</sub> &times; tiein_span&sup4; &divide; ({defl_coeff:.1f} &times; E &times;
    I), simply-supported UDL (conservative). {tiein_gov_clause}</p>
    <p class="note"><em>Ratified basis:</em> this &plusmn;{tol_mm:.0f} mm tie-in tolerance is a
    <strong>LATERAL</strong> connector-mating tolerance, so the screening default checks the
    <strong>current-driven lateral deflection</strong>; the vertical self-weight sag is reacted by
    the lowering rigging (a different degree of freedom) and is therefore <strong>not</strong> in
    this lateral check. The include_self_weight toggle (default off) remains available to fold the
    vertical self-weight into the resultant for a deliberate conservative-magnitude stance. The
    span fractions ({span_frac:g} lift, {tiein_frac:g} tie-in) are conservative screening choices
    flagged for domain re-review.</p>

    <h3>Go/No-Go Classification</h3>
    <ul>
        <li><strong>GO:</strong> all phases pass AND max utilisation &lt; {marginal}</li>
        <li><strong>MARGINAL:</strong> all phases pass AND max utilisation in [{marginal}, {nogo}]</li>
        <li><strong>NO_GO:</strong> any phase fails (utilisation &gt; {nogo})</li>
    </ul>
    """
    report.add_methodology(methodology_html)

    # Charts
    report.add_chart(
        "gono_heatmap",
        fig1,
        title="Chart 1: Go/No-Go Installation Matrix",
        subtitle=(
            f"At Hs = {ref_hs} m. Cell shows governing phase and utilisation. "
            "Green = GO, Amber = MARGINAL, Red = NO_GO."
        ),
    )

    report.add_chart(
        "bending_stress",
        fig2,
        title="Chart 2: In-Air Lift Bending Stress vs Jumper Length",
        subtitle=(
            f"Spreader-bar span model (longest unsupported run between picks). "
            f"Red dashed line = {bending_allow} x SMYS allowable ({allow_mpa:.1f} MPa)."
        ),
    )

    report.add_chart(
        "max_hs_limit",
        fig3,
        title="Chart 3: Operability Envelope — Max Allowable Hs",
        subtitle="Maximum sea state for each jumper length at the mid-range water depth. One curve per vessel.",
    )

    report.add_chart(
        "cable_tension",
        fig4,
        title="Chart 4: Cable Tension vs Water Depth",
        subtitle=f"Large CSV at Hs = {ref_hs} m. Lines per jumper length. Red = {wire_factor:.0%} wire MBL.",
    )

    report.add_chart(
        "vessel_comparison",
        fig5,
        title="Chart 5: Vessel Comparison — Max Installable Jumper Length",
        subtitle=f"At Hs = {ref_hs} m. Grouped by water depth.",
    )

    # Summary table
    report.add_table(
        f"Summary: Installation Screening at Reference Depth, Hs={ref_hs}m",
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
        f"All jumpers are 8\" OD (219.1 mm) API 5L {grade} with 18.26 mm wall thickness and FBE coating",
        f"SMYS = {smys_mpa:g} MPa, Young's modulus E = {e_gpa:g} GPa (from the jumper catalog)",
        f"Rigging mass = {_cfg(config, 'rigging_mass_kg', RIGGING_MASS_KG) / 1000:g} te "
        "(slings, shackles, spreader bar)",
        f"Phase 2 lift bending: governing span is an EXPLICIT, length-scaling fraction of the "
        f"horizontal run, lift_span = {span_frac:g} x horizontal_span (absolute override available); "
        "longer jumpers => longer unsupported bay. NOT the full jumper length",
        f"Phase 2 effective load = self-weight x lift DAF (dominant); simply-supported midspan "
        f"moment M = w_eff x span^2 / {moment_coeff:.0f} (conservative pin-pin boundary condition, "
        "editable in yaml)",
        f"Phase 5 tie-in: connector misalignment governed by the LATERAL deflection of ONE "
        f"unsupported leg adjacent to the connector, an EXPLICIT length-scaling free span "
        f"tiein_span = {tiein_frac:g} x horizontal_span (absolute override available); "
        + ("transverse load = resultant of submerged self-weight (~506 N/m) and current "
           "(~34 N/m) -- conservative-MAGNITUDE stance (include_self_weight=true)"
           if include_sw else "transverse load = the LATERAL current load alone (~34 N/m) -- "
           "RATIFIED: the +/-50mm tie-in tolerance is a lateral connector-mating tolerance, so it "
           "is current-driven; the vertical self-weight sag is reacted by the lowering rigging "
           "(a different DOF) and is excluded")
        + f"; simply-supported UDL delta = w_res x span^4 / ({defl_coeff:.1f} EI), checked vs "
        f"+/-{tol_mm:.0f} mm (API 17R connector)",
        "Phase-5 self-weight inclusion is an engineer-tunable toggle (include_self_weight, "
        "RATIFIED default off = current-only lateral); true selects a deliberate "
        "conservative-MAGNITUDE stance. The Phase-2 / Phase-5 span fractions are conservative "
        "screening choices flagged for domain re-review, not first-principles derivations",
        "Screening note: depth and Hs drive only the splash/lowering phases, which stay well below "
        "0.01 utilisation here (cable self-weight is small vs wire MBL in deepwater CSV lifts); "
        "jumper LENGTH is the dominant discriminator in this matrix",
        "Slamming coefficient Cs = pi for circular cross-section (DNV-RP-H103 Table 8-2)",
        "Drag coefficient Cd = 1.2 for cylinder (DNV-RP-H103)",
        "Added mass coefficient Ca = 1.0 for cylinder",
        "Lowering velocity = 0.5 m/s, typical for heavy-lift CSV operations",
        "Wire rope submerged weight = 50 N/m — representative of 96-128 mm diameter wire",
        f"Deepwater current velocity = {v_cur:g} m/s for tie-in deflection check",
        f"Tie-in connector-mating tolerance = +/-{tol_mm:.0f} mm (API 17R subsea connector basis)",
        "Tp estimated from Hs via Tp = 4.0 x sqrt(Hs) — simplified JONSWAP relationship",
        "Vessel RAOs are simplified single-peak values, not full frequency-dependent transfer functions",
        "No wave-current interaction or VIV effects during lowering",
        "No seabed slope or landing zone geotechnical effects considered",
    ])

    # Build and save. output_path defaults to the legacy demo output location.
    if output_path is None:
        OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
        output_path = OUTPUT_DIR / "demo_05_jumper_installation_report.html"
    else:
        output_path = Path(output_path)
        output_path.parent.mkdir(parents=True, exist_ok=True)
    html = report.build(output_path)

    print(f"  Report saved to: {output_path}")
    return html


# ---------------------------------------------------------------------------
# JSON cache / save
# ---------------------------------------------------------------------------

def build_metadata(
    all_results: List[Dict],
    common: Dict[str, Any],
    config: Optional[Any] = None,
) -> Dict[str, Any]:
    """Build the results-JSON metadata block from the threaded config (ADR-0005).

    The swept axes and the engineering constants come from the config so the cache metadata
    reflects the run's ACTUAL config (a config=None run reproduces the baseline literals). The
    ``constants`` sub-dict is the reviewable record of every value that drove the compute.
    """
    depths = list(config.depths) if config is not None else list(WATER_DEPTHS)
    hs_values = list(config.hs) if config is not None else list(HS_VALUES)
    lengths = (
        [int(L) if float(L).is_integer() else L for L in config.lengths_m]
        if config is not None else [20, 40, 60, 80, 100]
    )
    ref_hs = _cfg(config, "reference_hs", REFERENCE_HS)
    # Vessel list derived from the config's selected vessels (a vessel-subset config emits the
    # correct metadata), falling back to the baseline pair when config is None.
    vessels_meta = (
        list(config.vessels) if config is not None
        else ["Large CSV (5000te)", "Medium CSV (2500te)"]
    )
    return {
        "demo": "GTM Demo 5: Deepwater Rigid Jumper Installation Analysis",
        "total_cases": len(all_results),
        "vessels": vessels_meta,
        "water_depths_m": depths,
        "hs_values_m": hs_values,
        "jumper_lengths_m": lengths,
        "jumper_od_mm": common["od_mm"],
        "jumper_wt_mm": common["wt_mm"],
        "grade": common["grade"],
        "smys_mpa": common["smys_mpa"],
        "reference_hs_m": ref_hs,
        "constants": {
            "daf_liftoff": _cfg(config, "daf_liftoff", DAF_LIFT),
            "daf_splash": _cfg(config, "daf_splash", DAF_SPLASH),
            "rigging_mass_kg": _cfg(config, "rigging_mass_kg", RIGGING_MASS_KG),
            "cs_slamming": _cfg(config, "cs_slamming", CS_PIPE),
            "cd_cylinder": _cfg(config, "cd_cylinder", CD_CYLINDER),
            "ca_cylinder": _cfg(config, "ca_cylinder", CA_CYLINDER),
            "v_lowering": _cfg(config, "v_lowering", V_LOWERING),
            "v_current": _cfg(config, "v_current", V_CURRENT),
            "splash_submerged_length_m": _cfg(config, "splash_submerged_length_m", SPLASH_SUBMERGED_LENGTH),
            "wire_allowable_factor": _cfg(config, "wire_allowable_factor", WIRE_ALLOWABLE_FACTOR),
            "bending_allowable": _cfg(config, "bending_allowable", BENDING_ALLOWABLE),
            "cable_unit_weight_sub_n_per_m": _cfg(config, "cable_unit_weight_sub_n_per_m", CABLE_UNIT_WEIGHT_SUB),
            "tie_in_tolerance_mm": _cfg(config, "tie_in_tolerance_mm", TIE_IN_TOLERANCE_MM),
            "tp_coefficient": _cfg(config, "tp_coefficient", 4.0),
            "go_marginal_threshold": _cfg(config, "go_marginal_threshold", 0.85),
            "nogo_utilisation": _cfg(config, "nogo_utilisation", 1.0),
            "seawater_density_kg_m3": _cfg(config, "seawater_density_kg_m3", SEAWATER_DENSITY),
            "gravity_m_s2": _cfg(config, "gravity_m_s2", GRAVITY),
            "lift_span_fraction": _cfg(config, "lift_span_fraction", LIFT_SPAN_FRACTION),
            "lift_span_m": _cfg(config, "lift_span_m", LIFT_SPAN_M),
            "lift_moment_coeff": _cfg(config, "lift_moment_coeff", LIFT_MOMENT_COEFF),
            "tiein_unsupported_span_fraction": _cfg(config, "tiein_unsupported_span_fraction", TIEIN_UNSUPPORTED_SPAN_FRACTION),
            "tiein_unsupported_span_m": _cfg(config, "tiein_unsupported_span_m", TIEIN_UNSUPPORTED_SPAN_M),
            "tiein_include_self_weight": _cfg(config, "tiein_include_self_weight", TIEIN_INCLUDE_SELF_WEIGHT),
            "tiein_deflection_coeff": _cfg(config, "tiein_deflection_coeff", TIEIN_DEFLECTION_COEFF),
        },
        "codes": [
            "DNV-RP-H103 (2011)",
            "DNV-ST-N001 (2021)",
            "DNV-OS-F101 (2021)",
            "API 17R (2015)",
        ],
    }


def save_json_results(
    all_results: List[Dict],
    summary_df: pd.DataFrame,
    common: Dict[str, Any],
    config: Optional[Any] = None,
    results_path: Optional[Path] = None,
) -> Path:
    """Save results to JSON for --from-cache reuse."""
    if results_path is None:
        RESULTS_DIR.mkdir(parents=True, exist_ok=True)
        results_path = RESULTS_DIR / "demo_05_jumper_installation_results.json"
    else:
        results_path = Path(results_path)
        results_path.parent.mkdir(parents=True, exist_ok=True)

    json_output = {
        "metadata": build_metadata(all_results, common, config=config),
        "summary": summary_df.to_dict(orient="records"),
        "results": all_results,
    }

    with open(results_path, "w") as f:
        json.dump(json_output, f, indent=2, default=str)

    print(f"  Results saved to: {results_path}")
    return results_path


# ---------------------------------------------------------------------------
# Results Store subcommands: `lookup` + `rebuild-db`
#
# Dispatched by an argv-sniff in __main__ so they NEVER touch the existing sweep parser — the
# no-arg / --from-cache / --force / --results-dir / --run-id / --config sweep path stays
# byte-identical. Each subcommand builds its OWN ArgumentParser over sys.argv[2:].
# ---------------------------------------------------------------------------

# Valid overall_status tokens (matches the store's GO/MARGINAL/NO_GO/ERROR taxonomy).
_VALID_STATUSES = ("GO", "MARGINAL", "NO_GO", "ERROR")

# Columns shown by `lookup`, in a clean operator-facing order. demo_05 has no hyphen-prefixed
# columns, but every identifier is still quoted via rs._q() so the idiom stays consistent with
# the demo_04 store.
_LOOKUP_DISPLAY_COLS = [
    "vessel_id",
    "jumper_id",
    "length_m",
    "water_depth_m",
    "hs_m",
    "overall_status",
    "governing_phase",
    "max_utilisation",
]


def lookup_main(argv: List[str]) -> int:
    """`lookup` subcommand: read-only filtered SELECT over the demo_05 Results Store cache.

    Returns a process exit code (0 = ok, non-zero = error such as a missing db).
    """
    import sqlite3

    try:
        import results_store_demo05 as rs
    except ImportError:  # pragma: no cover — packaged import path fallback.
        from examples.demos.gtm import results_store_demo05 as rs

    parser = argparse.ArgumentParser(
        prog="demo_05_deepwater_rigid_jumper_installation.py lookup",
        description="Look up cases in the demo_05 Results Store (read-only SQLite cache).",
    )
    parser.add_argument("--run-id", type=str, default=BASELINE_RUN_ID,
                        help=f"Run to query (default '{BASELINE_RUN_ID}').")
    parser.add_argument("--vessel-id", type=str, default=None,
                        help="Vessel SHORT id filter (e.g. CSV-001 / CSV-002).")
    parser.add_argument("--jumper-id", type=str, default=None,
                        help="Jumper id filter (e.g. JMP-20 / JMP-40 / JMP-60 / JMP-80 / JMP-100).")
    parser.add_argument("--water-depth", type=int, default=None,
                        help="Water depth filter in metres, INTEGER (e.g. 500).")
    parser.add_argument("--hs", type=float, default=None,
                        help="Significant wave height filter in metres, REAL (e.g. 2.0).")
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
        # when provided. --water-depth binds int (the column's INTEGER type, BD-3); --hs binds
        # float (the column's REAL type, BD-3).
        where = ["run_id = ?"]
        params: List[Any] = [args.run_id]
        if args.vessel_id is not None:
            where.append("vessel_id = ?")
            params.append(str(args.vessel_id))
        if args.jumper_id is not None:
            where.append("jumper_id = ?")
            params.append(str(args.jumper_id))
        if args.water_depth is not None:
            where.append("water_depth_m = ?")
            params.append(int(args.water_depth))
        if args.hs is not None:
            where.append("hs_m = ?")
            params.append(float(args.hs))
        if args.status is not None:
            where.append("overall_status = ?")
            params.append(str(args.status))

        # Quote EVERY display column via rs._q() (consistent with the demo_04 store idiom).
        cols = ", ".join(rs._q(c) for c in _LOOKUP_DISPLAY_COLS)
        sql = (
            f"SELECT {cols} FROM cases WHERE {' AND '.join(where)} "
            "ORDER BY vessel_id, jumper_id, water_depth_m, hs_m"
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
        import results_store_demo05 as rs
    except ImportError:  # pragma: no cover — packaged import path fallback.
        from examples.demos.gtm import results_store_demo05 as rs

    parser = argparse.ArgumentParser(
        prog="demo_05_deepwater_rigid_jumper_installation.py rebuild-db",
        description="Rebuild the demo_05 Results Store SQLite cache + index.csv from "
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
        "output/parametric/demo_05/<run_id>/report.html and seeds <run_id> in the Store.",
    )
    parser.add_argument(
        "--config",
        type=Path,
        default=BASELINE_CONFIG_PATH,
        help="Path to the Sweep Config yaml driving the run (axes, constants, span models, "
        "catalogs, artifact paths). Defaults to the committed baseline "
        f"({BASELINE_CONFIG_PATH.name}); point it at your own yaml to run a client config "
        "without editing the baseline.",
    )
    args = parser.parse_args()

    # Lazy import of the loader (engineering-free; usable on both paths).
    try:
        from sweep_config_demo05 import (
            SweepConfigError,
            load_demo05_config,
            load_demo05_paths,
        )
    except ImportError:  # pragma: no cover — packaged import path fallback.
        from examples.demos.gtm.sweep_config_demo05 import (
            SweepConfigError,
            load_demo05_config,
            load_demo05_paths,
        )

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

    # Validate + resolve the FULL config once, up front (clean error on schema failure).
    try:
        config = load_demo05_config(config_path)
    except SweepConfigError as exc:
        print(f"[ERROR] {exc}")
        sys.exit(2)

    # --from-cache rebuilds the report/charts from a REAL config; the cache on disk was produced
    # by SOME config, and the only config we can prove matches it is the committed BASELINE.
    # Refuse --from-cache with a non-default --config (mirror demo_03 D1).
    is_default_config = config_path.resolve() == Path(BASELINE_CONFIG_PATH).resolve()
    if args.from_cache and not args.force and not is_default_config:
        print(
            "[ERROR] --from-cache is only supported with the committed baseline config. "
            f"A non-default --config cannot be proven to match the cached results ({config_path}); "
            "re-run without --from-cache to recompute from your config, or drop --config."
        )
        sys.exit(2)

    # --from-cache regenerates the Baseline report only (cached data is always the baseline
    # 300 cases). A named --run-id under --from-cache would write a mislabeled report, so warn
    # AND force the report path back to the legacy Baseline location below (mirror demo_04).
    if args.from_cache and args.run_id is not None and args.run_id != BASELINE_RUN_ID:
        warnings.warn(
            f"--from-cache regenerates the baseline report only; --run-id '{args.run_id}' "
            "will not produce a named Results Store row (run without --from-cache to compute "
            "a named run). The report is written to the baseline path.",
            stacklevel=2,
        )

    start_time = time.time()

    print("=" * 60)
    print("  GTM Demo 5: Deepwater Rigid Jumper Installation Analysis")
    print("=" * 60)

    # Resolve catalog + artifact PATHS from the config yaml (engineering-free).
    paths = load_demo05_paths(config_path)

    # The CLI --results-dir always wins (legacy JSON + Results Store base). Mirrors demo_04 —
    # it overrides the results root only; the report still lands under the yaml's output_root.
    results_root = Path(args.results_dir) if args.results_dir else paths.results_root
    results_path = results_root / "demo_05_jumper_installation_results.json"

    # [1/7] Load data files (from the config-resolved catalog paths).
    print("\n[1/7] Loading input data...")
    vessels = load_vessel_data(paths.vessels_path)
    common, jumpers = load_jumper_data(paths.jumpers_path)

    # [2/7] Run parametric sweep (or load cache). Config is threaded into both paths.
    if args.from_cache and not args.force:
        print("\n[2/7] Loading cached results...")
        with open(results_path, "r") as f:
            data = json.load(f)
        all_results = data["results"]
        summary_df = pd.DataFrame(data["summary"])
        print(f"  Loaded {len(all_results)} cached results from {results_path}")
    else:
        print("\n[2/7] Running parametric sweep...")
        all_results, _ = run_parametric_sweep(vessels, common, jumpers, config=config)

        # Build summary after sweep
        summary_df = build_summary_table(all_results, config=config)

        # Results Store (§2, additive): persist the run alongside the legacy JSON/HTML.
        # RECOMPUTE branch only. Mirrors demo_04 — a default sweep seeds the committed
        # BASELINE_RUN_ID; a named --run-id seeds that partition. If --from-cache demoted to
        # recompute (stale/missing cache), force BASELINE so we never seed a named row the
        # --from-cache contract said wouldn't exist. cases.csv + manifest.json are the text
        # source of truth; results.db + index.csv are derived/gitignored. Additive: it does NOT
        # touch the JSON/HTML the §1 golden pins, so the default sweep stays byte-identical.
        try:
            from results_store_demo05 import write_run as _store_write_run
        except ImportError:  # pragma: no cover — packaged import path fallback.
            from examples.demos.gtm.results_store_demo05 import (
                write_run as _store_write_run,
            )
        _store_write_run(
            run_id=(BASELINE_RUN_ID if args.from_cache else (args.run_id or BASELINE_RUN_ID)),
            config=config,
            results=all_results,
            base_dir=results_root,
        )

    total_cases = len(all_results)

    # [3/7] Build charts
    print("\n[3/7] Building charts...")
    fig1 = build_chart_1_heatmap(all_results, vessels, config=config)
    fig2 = build_chart_2_bending(common, jumpers, config=config)
    fig3 = build_chart_3_max_hs(all_results, vessels, jumpers, config=config)
    fig4 = build_chart_4_cable_tension(all_results, vessels, jumpers, config=config)
    fig5 = build_chart_5_vessel_comparison(all_results, vessels, jumpers, config=config)

    # [4/7] Build summary table
    print("\n[4/7] Summary table...")
    print(summary_df.to_string(index=False))

    # [5/7] Build HTML report
    print("\n[5/7] Building HTML report...")
    # Baseline Run keeps the legacy output path (byte-identical); a named Run writes a per-run
    # report under output/parametric/demo_05/<run_id>/report.html. Under --from-cache the data
    # is always the baseline 300, so a named run is forced back to the baseline path.
    if args.run_id is None or args.run_id == BASELINE_RUN_ID or args.from_cache:
        report_path = paths.output_root / "demo_05_jumper_installation_report.html"
    else:
        report_path = paths.output_root / "parametric" / DEMO_ID / args.run_id / "report.html"
    build_report(
        fig1, fig2, fig3, fig4, fig5, summary_df, all_results, total_cases,
        config=config, output_path=report_path, common=common,
    )
    if args.run_id is not None and args.run_id != BASELINE_RUN_ID and not args.from_cache:
        print(f"  Per-run report written to: {report_path}")

    # [6/7] Save JSON results
    if not args.from_cache or args.force:
        print("\n[6/7] Saving JSON results...")
        save_json_results(all_results, summary_df, common, config=config, results_path=results_path)
    else:
        print("\n[6/7] Skipping JSON save (loaded from cache)")

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
    # Argv-sniff dispatch: the `lookup` / `rebuild-db` subcommands are handled by their OWN
    # parsers over sys.argv[2:] and NEVER reach the sweep parser, so the no-arg / --from-cache
    # / --force / --results-dir / --run-id / --config sweep path is untouched and byte-identical.
    if len(sys.argv) > 1 and sys.argv[1] == "lookup":
        sys.exit(lookup_main(sys.argv[2:]))
    elif len(sys.argv) > 1 and sys.argv[1] == "rebuild-db":
        sys.exit(rebuild_main(sys.argv[2:]))
    else:
        main()
