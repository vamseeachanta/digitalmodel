#!/usr/bin/env python3
# ABOUTME: GTM Demo 2 — Pipeline Wall Thickness Multi-Code Comparison
# ABOUTME: Parametric analysis across 6 pipe sizes, 3 codes, 4 pressures with 5 Plotly charts
"""
GTM Demo 2: Pipeline Wall Thickness — Multi-Code Comparison
=============================================================

Runs parametric wall thickness design checks across:
  - 6 pipe sizes (6" to 20")
  - 3 design codes (DNV-ST-F101, API RP 1111, PD 8010-2)
  - 4 internal pressures (10, 15, 20, 25 MPa)
  - 5 lifecycle phases per pipe

Produces:
  - 5 interactive Plotly charts
  - Branded HTML report via GTMReportBuilder
  - JSON results file with --from-cache support

Usage:
    cd digitalmodel
    PYTHONPATH=examples/demos/gtm:src uv run python \\
        examples/demos/gtm/demo_02_wall_thickness_multicode.py

    # Re-generate charts from cached results:
    PYTHONPATH=examples/demos/gtm:src uv run python \\
        examples/demos/gtm/demo_02_wall_thickness_multicode.py --from-cache
"""

from __future__ import annotations

import argparse
import json
import logging
import math
import sys
import time
from collections import defaultdict
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

# Engineering modules — deferred when running from cache
_ENGINEERING_MODULES_LOADED = False

def _load_engineering_modules():
    """Import digitalmodel engineering modules on demand."""
    global _ENGINEERING_MODULES_LOADED
    if _ENGINEERING_MODULES_LOADED:
        return
    # These are injected into module globals so existing functions can use them
    global DesignCode, DesignFactors, DesignLoads, FabricationType
    global PipeGeometry, PipeMaterial, SafetyClass, WallThicknessAnalyzer
    global API_5L_GRADES
    global PhaseAnalysisRunner, PipeDefinition, PipelinePhase
    global create_standard_phases, compute_hydrostatic_pressure
    try:
        from digitalmodel.structural.analysis.wall_thickness import (
            DesignCode as _DesignCode,
            DesignFactors as _DesignFactors,
            DesignLoads as _DesignLoads,
            FabricationType as _FabricationType,
            PipeGeometry as _PipeGeometry,
            PipeMaterial as _PipeMaterial,
            SafetyClass as _SafetyClass,
            WallThicknessAnalyzer as _WallThicknessAnalyzer,
        )
        from digitalmodel.structural.analysis.wall_thickness_parametric import (
            API_5L_GRADES as _API_5L_GRADES,
        )
        from digitalmodel.structural.analysis.wall_thickness_phases import (
            PhaseAnalysisRunner as _PhaseAnalysisRunner,
            PipeDefinition as _PipeDefinition,
            PipelinePhase as _PipelinePhase,
            create_standard_phases as _create_standard_phases,
            compute_hydrostatic_pressure as _compute_hydrostatic_pressure,
        )
        DesignCode = _DesignCode
        DesignFactors = _DesignFactors
        DesignLoads = _DesignLoads
        FabricationType = _FabricationType
        PipeGeometry = _PipeGeometry
        PipeMaterial = _PipeMaterial
        SafetyClass = _SafetyClass
        WallThicknessAnalyzer = _WallThicknessAnalyzer
        API_5L_GRADES = _API_5L_GRADES
        PhaseAnalysisRunner = _PhaseAnalysisRunner
        PipeDefinition = _PipeDefinition
        PipelinePhase = _PipelinePhase
        create_standard_phases = _create_standard_phases
        compute_hydrostatic_pressure = _compute_hydrostatic_pressure
        _ENGINEERING_MODULES_LOADED = True
    except ImportError as exc:
        print(f"[ERROR] Cannot import digitalmodel modules: {exc}")
        print("        Ensure PYTHONPATH includes 'src' directory.")
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

SEAWATER_DENSITY = 1025.0  # kg/m³
GRAVITY = 9.80665  # m/s²
STEEL_DENSITY = 7850.0  # kg/m³

# Parameter matrix
PIPE_SIZES = [
    {"name": '6"',  "od_mm": 168.3, "od_m": 0.1683},
    {"name": '8"',  "od_mm": 219.1, "od_m": 0.2191},
    {"name": '10"', "od_mm": 273.1, "od_m": 0.2731},
    {"name": '12"', "od_mm": 323.9, "od_m": 0.3239},
    {"name": '16"', "od_mm": 406.4, "od_m": 0.4064},
    {"name": '20"', "od_mm": 508.0, "od_m": 0.5080},
]

# CODES and CODE_NAMES are initialised after engineering modules are loaded
# (see _init_code_constants).  In --from-cache mode they are plain strings.
CODES: list = []
CODE_NAMES: dict = {}
CODE_NAME_STRINGS = ["DNV-ST-F101", "API RP 1111", "PD 8010-2"]


def _init_code_constants():
    """Populate CODES / CODE_NAMES from loaded DesignCode enum."""
    global CODES, CODE_NAMES
    CODES = [DesignCode.DNV_ST_F101, DesignCode.API_RP_1111, DesignCode.PD_8010_2]
    CODE_NAMES = {
        DesignCode.DNV_ST_F101: "DNV-ST-F101",
        DesignCode.API_RP_1111: "API RP 1111",
        DesignCode.PD_8010_2: "PD 8010-2",
    }

INTERNAL_PRESSURES_MPA = [10, 15, 20, 25]
WATER_DEPTH = 500.0  # m
GRADE = "X65"
SMYS = 448e6  # Pa
SMTS = 531e6  # Pa
CORROSION_ALLOWANCE = 0.001  # m (1 mm)

# Lifecycle phase names for Chart 1
PHASE_DISPLAY_NAMES = [
    "Installation",
    "Hydrotest",
    "Operation",
    "Shutdown",
]

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def external_pressure_pa(depth: float = WATER_DEPTH) -> float:
    """Hydrostatic pressure at given depth."""
    return SEAWATER_DENSITY * GRAVITY * depth


def get_min_wt_from_catalog(od_mm: float, pipe_catalog: list) -> Optional[float]:
    """Get minimum standard WT (in metres) for a given OD from the pipe catalog."""
    for pipe in pipe_catalog:
        if abs(pipe["od_mm"] - od_mm) < 0.5:
            wts = [wt["wt_m"] for wt in pipe["wall_thicknesses"]]
            return min(wts) if wts else None
    return None


def utilisation_color(util: float) -> str:
    """Return hex color for utilisation: green / amber / red."""
    if util < 0.7:
        return COLORS["success"]   # green
    elif util <= 0.9:
        return COLORS["warning"]   # amber
    else:
        return COLORS["danger"]    # red


def run_single_analysis(
    od_m: float,
    wt_m: float,
    pi_pa: float,
    pe_pa: float,
    code: DesignCode,
) -> Optional[Dict[str, Any]]:
    """Run wall thickness analysis for a single case. Returns result dict or None."""
    try:
        geom = PipeGeometry(
            outer_diameter=od_m,
            wall_thickness=wt_m,
            corrosion_allowance=CORROSION_ALLOWANCE,
        )
        mat = PipeMaterial(grade=GRADE, smys=SMYS, smts=SMTS)
        loads = DesignLoads(internal_pressure=pi_pa, external_pressure=pe_pa)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, code)
        result = analyzer.perform_analysis()
        return {
            "is_safe": result.is_safe,
            "governing_check": result.governing_check,
            "max_utilisation": result.max_utilisation,
            "checks": dict(result.checks),
        }
    except Exception as exc:
        logger.warning("Analysis failed for OD=%.4f WT=%.4f code=%s: %s",
                       od_m, wt_m, code.value, exc)
        return None


def find_min_wall_thickness(
    od_m: float,
    pi_pa: float,
    pe_pa: float,
    code: DesignCode,
    wt_min: float = 0.003,
    wt_max: float = 0.060,
    tol: float = 0.0001,
) -> Tuple[float, Optional[str]]:
    """Binary search for minimum WT that satisfies all checks (util <= 1.0).
    
    Returns (min_wt_m, governing_check).
    """
    lo, hi = wt_min, wt_max

    # Check if even max WT fails
    result_max = run_single_analysis(od_m, hi, pi_pa, pe_pa, code)
    if result_max is None or not result_max["is_safe"]:
        return hi, result_max.get("governing_check") if result_max else "N/A"

    # Check if min WT already passes
    result_min = run_single_analysis(od_m, lo, pi_pa, pe_pa, code)
    if result_min is not None and result_min["is_safe"]:
        return lo, result_min.get("governing_check")

    # Binary search
    governing = "N/A"
    for _ in range(100):
        mid = (lo + hi) / 2
        if hi - lo < tol:
            break
        result = run_single_analysis(od_m, mid, pi_pa, pe_pa, code)
        if result is None or not result["is_safe"]:
            lo = mid
            if result:
                governing = result.get("governing_check", "N/A")
        else:
            hi = mid
            governing = result.get("governing_check", "N/A")

    return hi, governing


# ---------------------------------------------------------------------------
# Chart 1: Lifecycle Utilisation (Hero Chart)
# ---------------------------------------------------------------------------

def build_chart_1_lifecycle(
    pipe_catalog: Optional[list] = None,
    cached_lifecycle: Optional[Dict] = None,
) -> Tuple[go.Figure, Dict]:
    """Build lifecycle utilisation grouped bar chart with pipe size dropdown.

    Returns (figure, lifecycle_data) so intermediate results can be cached.
    When *cached_lifecycle* is provided, skip engineering calculations.
    """
    print("\n[Chart 1] Building Lifecycle Utilisation chart...")

    if cached_lifecycle is not None:
        # Reconstruct all_pipe_data from cache
        all_pipe_data = {}
        for pipe_name, code_map in cached_lifecycle.items():
            all_pipe_data[pipe_name] = {
                "phase_data": code_map,
                "phases": PHASE_DISPLAY_NAMES,
            }
    else:
        pe = external_pressure_pa()
        design_pressure = 20e6  # Fixed for lifecycle phases

        # Pre-compute data for all pipe sizes
        all_pipe_data = {}
        for ps in PIPE_SIZES:
            od_m = ps["od_m"]
            pipe_name = ps["name"]

            # Get a reasonable WT for this pipe (mid-range from catalog)
            wt_m = select_analysis_wt(ps, pipe_catalog)

            pipe_def = PipeDefinition(
                outer_diameter=od_m,
                wall_thickness=wt_m,
                grade=GRADE,
                smys=SMYS,
                smts=SMTS,
                corrosion_allowance=CORROSION_ALLOWANCE,
            )

            phases = create_standard_phases(
                water_depth=WATER_DEPTH,
                design_pressure=design_pressure,
            )

            # Run phase analysis for each code
            phase_data = {}  # {code_name: {phase_name: max_util}}
            for code in CODES:
                code_name = CODE_NAMES[code]
                try:
                    runner = PhaseAnalysisRunner(
                        pipe=pipe_def,
                        phases=phases,
                        codes=[code],
                        safety_class=SafetyClass.MEDIUM,
                    )
                    comparison = runner.run()
                    summary = comparison.summary_dataframe()

                    phase_utils = {}
                    for _, row in summary.iterrows():
                        phase_utils[row["phase"]] = row["max_utilisation"]
                    phase_data[code_name] = phase_utils
                except Exception as exc:
                    logger.warning("Phase analysis failed for %s %s: %s", pipe_name, code_name, exc)
                    phase_data[code_name] = {}

            all_pipe_data[pipe_name] = {
                "phase_data": phase_data,
                "phases": [p.name for p in phases],
            }

    # Build serialisable lifecycle data for caching
    lifecycle_data = {}
    for pipe_name, pdata in all_pipe_data.items():
        lifecycle_data[pipe_name] = pdata["phase_data"]

    # Build figure with dropdown
    fig = go.Figure()

    # Get phase names from first pipe
    first_pipe = list(all_pipe_data.values())[0]
    phase_names = first_pipe["phases"]
    code_names_list = CODE_NAME_STRINGS
    code_colors = {
        code_names_list[0]: CHART_PALETTE[0],
        code_names_list[1]: CHART_PALETTE[1],
        code_names_list[2]: CHART_PALETTE[2],
    }

    # Add traces for ALL pipe sizes (hide non-default ones)
    traces_per_pipe = len(code_names_list)
    default_pipe = PIPE_SIZES[2]["name"]  # default to 10"

    for ps in PIPE_SIZES:
        pipe_name = ps["name"]
        pipe_data = all_pipe_data.get(pipe_name, {})
        pd_data = pipe_data.get("phase_data", {})
        visible = (pipe_name == default_pipe)

        for code_name in code_names_list:
            utils = pd_data.get(code_name, {})
            y_vals = [utils.get(pn, 0.0) for pn in phase_names]

            # Color each bar by utilisation
            bar_colors = [utilisation_color(u) for u in y_vals]

            fig.add_trace(go.Bar(
                x=phase_names,
                y=y_vals,
                name=code_name,
                marker_color=code_colors.get(code_name, CHART_PALETTE[0]),
                visible=visible,
                legendgroup=code_name,
                showlegend=visible,
                hovertemplate=(
                    f"<b>{code_name}</b><br>"
                    "Phase: %{x}<br>"
                    "Utilisation: %{y:.3f}<extra></extra>"
                ),
            ))

    # Add unity line
    fig.add_hline(y=1.0, line_dash="dash", line_color="red", line_width=2,
                  annotation_text="Unity (1.0)")

    # Build dropdown buttons
    buttons = []
    for i, ps in enumerate(PIPE_SIZES):
        pipe_name = ps["name"]
        visibility = []
        for j, ps2 in enumerate(PIPE_SIZES):
            for _ in code_names_list:
                visibility.append(ps2["name"] == pipe_name)

        # Update showlegend too
        showlegend = []
        for j, ps2 in enumerate(PIPE_SIZES):
            for _ in code_names_list:
                showlegend.append(ps2["name"] == pipe_name)

        buttons.append(dict(
            label=f"{pipe_name} ({ps['od_mm']}mm)",
            method="update",
            args=[
                {"visible": visibility, "showlegend": showlegend},
            ],
        ))

    fig.update_layout(
        title=dict(
            text="Lifecycle Utilisation by Design Code",
            font=dict(size=18),
        ),
        xaxis_title="Lifecycle Phase",
        yaxis_title="Max Utilisation Ratio",
        barmode="group",
        height=550,
        updatemenus=[
            dict(
                type="dropdown",
                direction="down",
                x=0.98,
                xanchor="right",
                y=1.15,
                yanchor="top",
                buttons=buttons,
                showactive=True,
                active=2,  # default to 10"
            ),
        ],
        annotations=[
            dict(
                text="Pipe Size:",
                x=0.82, y=1.15,
                xref="paper", yref="paper",
                showarrow=False,
                font=dict(size=13),
            ),
        ],
    )

    print("  ✓ Chart 1 complete")
    return fig, lifecycle_data


# ---------------------------------------------------------------------------
# Chart 2: Min Wall Thickness vs Pipe Size
# ---------------------------------------------------------------------------

def build_chart_2_min_wt(
    cached_min_wt: Optional[Dict] = None,
) -> Tuple[go.Figure, Dict]:
    """Build min WT vs pipe size line chart.

    Returns (figure, min_wt_data) so intermediate results can be cached.
    When *cached_min_wt* is provided, skip engineering calculations.
    """
    print("\n[Chart 2] Building Min Wall Thickness vs Pipe Size...")

    od_values = [ps["od_mm"] for ps in PIPE_SIZES]

    if cached_min_wt is not None:
        # Reconstruct code_results from cache  {pipe_name: {code: wt_mm}}
        code_results = {}
        for code_name in CODE_NAME_STRINGS:
            wt_values = []
            for ps in PIPE_SIZES:
                wt_mm = cached_min_wt.get(ps["name"], {}).get(code_name, 0.0)
                wt_values.append(wt_mm)
            code_results[code_name] = wt_values
    else:
        pe = external_pressure_pa()
        pi = 20e6  # Fixed 20 MPa

        code_results = {}
        for code_idx, code in enumerate(CODES):
            code_name = CODE_NAMES[code]
            wt_values = []
            for ps in PIPE_SIZES:
                min_wt, _ = find_min_wall_thickness(ps["od_m"], pi, pe, code)
                wt_values.append(min_wt * 1000)  # Convert to mm
                print(f"  {ps['name']} | {code_name}: min WT = {min_wt*1000:.2f} mm")
            code_results[code_name] = wt_values

    # Build serialisable min_wt data for caching  {pipe_name: {code: wt_mm}}
    min_wt_data: Dict[str, Dict[str, float]] = {}
    for i, ps in enumerate(PIPE_SIZES):
        pipe_wts = {}
        for code_name, wt_list in code_results.items():
            pipe_wts[code_name] = round(wt_list[i], 2)
        min_wt_data[ps["name"]] = pipe_wts

    fig = go.Figure()

    # Add lines for each code
    code_names_list = list(code_results.keys())
    for idx, code_name in enumerate(code_names_list):
        fig.add_trace(go.Scatter(
            x=od_values,
            y=code_results[code_name],
            mode="lines+markers",
            name=code_name,
            line=dict(color=CHART_PALETTE[idx], width=3),
            marker=dict(size=8),
        ))

    # Fill between most and least conservative
    all_wts = [code_results[cn] for cn in code_names_list]
    max_wt = [max(vals) for vals in zip(*all_wts)]
    min_wt = [min(vals) for vals in zip(*all_wts)]

    fig.add_trace(go.Scatter(
        x=od_values + od_values[::-1],
        y=max_wt + min_wt[::-1],
        fill="toself",
        fillcolor="rgba(237,137,54,0.15)",
        line=dict(color="rgba(0,0,0,0)"),
        name="Code Penalty Band",
        showlegend=True,
        hoverinfo="skip",
    ))

    fig.update_layout(
        title=dict(
            text="Minimum Required Wall Thickness vs Pipe Size",
            font=dict(size=18),
        ),
        xaxis_title="Pipe OD (mm)",
        yaxis_title="Minimum Wall Thickness (mm)",
        height=500,
        legend=dict(x=0.02, y=0.98),
    )

    print("  ✓ Chart 2 complete")
    return fig, min_wt_data


# ---------------------------------------------------------------------------
# Chart 3: Utilisation Heatmap
# ---------------------------------------------------------------------------

def build_chart_3_heatmap(
    pipe_catalog: Optional[list] = None,
    cached_results: Optional[List[Dict]] = None,
) -> go.Figure:
    """Build 3 side-by-side utilisation heatmaps (one per code).

    When *cached_results* (the parametric sweep list) is provided, extract
    utilisation values from the sweep instead of re-running analyses.
    """
    print("\n[Chart 3] Building Utilisation Heatmaps...")

    pipe_labels = [ps["name"] for ps in PIPE_SIZES]

    # Custom colorscale: green -> amber -> red
    colorscale = [
        [0.0, "#38a169"],   # green
        [0.35, "#38a169"],
        [0.5, "#d69e2e"],   # amber
        [0.7, "#d69e2e"],
        [0.85, "#e53e3e"],  # red
        [1.0, "#e53e3e"],
    ]

    code_names_list = CODE_NAME_STRINGS
    fig = make_subplots(
        rows=1, cols=3,
        subplot_titles=code_names_list,
        horizontal_spacing=0.08,
    )

    # Build a lookup from cached results if available
    _cache_lookup: Dict[tuple, Dict] = {}
    if cached_results is not None:
        for r in cached_results:
            key = (r.get("pipe_size"), r.get("code"), r.get("internal_pressure_mpa"))
            _cache_lookup[key] = r

    for col_idx, code_name in enumerate(code_names_list, start=1):
        z_matrix = []
        hover_text = []

        for pi_mpa in INTERNAL_PRESSURES_MPA:
            row_z = []
            row_hover = []
            for ps in PIPE_SIZES:
                if cached_results is not None:
                    r = _cache_lookup.get((ps["name"], code_name, pi_mpa))
                    if r and r.get("max_utilisation") not in (None, "N/A"):
                        util = float(r["max_utilisation"])
                        gov = r.get("governing_check") or "N/A"
                        wt_mm = r.get("wt_mm", 0)
                    else:
                        util = float("nan")
                        gov = "N/A"
                        wt_mm = 0
                else:
                    wt_m = select_analysis_wt(ps, pipe_catalog)
                    wt_mm = wt_m * 1000
                    pe = external_pressure_pa()
                    result = run_single_analysis(
                        ps["od_m"], wt_m, pi_mpa * 1e6, pe, CODES[col_idx - 1],
                    )
                    if result:
                        util = result["max_utilisation"]
                        gov = result["governing_check"] or "N/A"
                    else:
                        util = float("nan")
                        gov = "N/A"

                row_z.append(util)
                row_hover.append(
                    f"Pipe: {ps['name']}<br>"
                    f"P_int: {pi_mpa} MPa<br>"
                    f"WT: {wt_mm:.1f} mm<br>"
                    f"Util: {util:.3f}<br>"
                    f"Gov: {gov}"
                )
            z_matrix.append(row_z)
            hover_text.append(row_hover)

        fig.add_trace(
            go.Heatmap(
                x=pipe_labels,
                y=[f"{p} MPa" for p in INTERNAL_PRESSURES_MPA],
                z=z_matrix,
                text=hover_text,
                hoverinfo="text",
                colorscale=colorscale,
                zmin=0,
                zmax=2.0,
                showscale=(col_idx == 3),
                colorbar=dict(title="Utilisation") if col_idx == 3 else None,
            ),
            row=1, col=col_idx,
        )

    fig.update_layout(
        title=dict(
            text="Utilisation Heatmap: Pipe Size × Internal Pressure",
            font=dict(size=18),
        ),
        height=450,
        width=1100,
    )

    for col_idx in range(1, 4):
        fig.update_xaxes(title_text="Pipe Size", row=1, col=col_idx)
    fig.update_yaxes(title_text="Internal Pressure", row=1, col=1)

    print("  ✓ Chart 3 complete")
    return fig


# ---------------------------------------------------------------------------
# Chart 4: Weight Penalty Bar Chart
# ---------------------------------------------------------------------------

def build_chart_4_weight_penalty(
    cached_weight_penalty: Optional[Dict] = None,
    cached_min_wt: Optional[Dict] = None,
) -> Tuple[go.Figure, Dict]:
    """Build dual-axis weight penalty bar chart.

    When *cached_weight_penalty* is provided, use pre-computed values.
    When *cached_min_wt* is provided (but not weight_penalty), derive from
    min WT data without running engineering calculations.
    """
    print("\n[Chart 4] Building Weight Penalty chart...")

    pipe_labels = [ps["name"] for ps in PIPE_SIZES]
    extra_kg_m = []
    extra_pct = []
    most_conservative_codes = []
    least_conservative_codes = []

    if cached_weight_penalty is not None:
        # Use pre-computed weight penalty data
        for ps in PIPE_SIZES:
            pname = ps["name"]
            wp = cached_weight_penalty.get(pname, {})
            extra_kg_m.append(wp.get("delta_kg_m", 0.0))
            extra_pct.append(wp.get("delta_pct", 0.0))
            most_conservative_codes.append(wp.get("most_conservative", "N/A"))
            least_conservative_codes.append(wp.get("least_conservative", "N/A"))
    elif cached_min_wt is not None:
        # Derive from cached min WT values
        for ps in PIPE_SIZES:
            pname = ps["name"]
            wt_by_code = cached_min_wt.get(pname, {})
            # Convert mm -> m
            wt_m_by_code = {k: v / 1000.0 for k, v in wt_by_code.items()}
            wt_values = list(wt_m_by_code.values())
            max_wt = max(wt_values) if wt_values else 0
            min_wt_val = min(wt_values) if wt_values else 0
            delta_wt = max_wt - min_wt_val

            most_code = max(wt_m_by_code, key=wt_m_by_code.get) if wt_m_by_code else "N/A"
            least_code = min(wt_m_by_code, key=wt_m_by_code.get) if wt_m_by_code else "N/A"
            most_conservative_codes.append(most_code)
            least_conservative_codes.append(least_code)

            od = ps["od_m"]
            a_thick = math.pi / 4 * (od**2 - (od - 2 * max_wt)**2)
            a_thin = math.pi / 4 * (od**2 - (od - 2 * min_wt_val)**2)
            delta_mass = (a_thick - a_thin) * STEEL_DENSITY
            pct = (delta_wt / min_wt_val * 100) if min_wt_val > 0 else 0
            extra_kg_m.append(delta_mass)
            extra_pct.append(pct)
    else:
        pe = external_pressure_pa()
        pi = 20e6

        for ps in PIPE_SIZES:
            wt_by_code = {}
            for code in CODES:
                min_wt, _ = find_min_wall_thickness(ps["od_m"], pi, pe, code)
                wt_by_code[CODE_NAMES[code]] = min_wt

            wt_values = list(wt_by_code.values())
            max_wt = max(wt_values)
            min_wt = min(wt_values)
            delta_wt = max_wt - min_wt

            # Find which codes are most/least conservative
            most_code = max(wt_by_code, key=wt_by_code.get)
            least_code = min(wt_by_code, key=wt_by_code.get)
            most_conservative_codes.append(most_code)
            least_conservative_codes.append(least_code)

            # Extra steel weight per metre: rho_steel * pi * (OD - t) * t_delta (approx)
            # More precisely: delta in cross-sectional area
            od = ps["od_m"]
            a_thick = math.pi / 4 * (od**2 - (od - 2 * max_wt)**2)
            a_thin = math.pi / 4 * (od**2 - (od - 2 * min_wt)**2)
            delta_mass = (a_thick - a_thin) * STEEL_DENSITY  # kg/m

            pct = (delta_wt / min_wt * 100) if min_wt > 0 else 0

            extra_kg_m.append(delta_mass)
            extra_pct.append(pct)

            print(f"  {ps['name']}: ΔWT={delta_wt*1000:.2f}mm, Δmass={delta_mass:.1f} kg/m, {pct:.1f}%")

    # Find typical annotation data (use 12" pipe)
    idx_12 = 3  # 12" is index 3
    pipeline_length_km = 50
    extra_tonnes = extra_kg_m[idx_12] * pipeline_length_km * 1000 / 1000  # tonnes

    fig = make_subplots(specs=[[{"secondary_y": True}]])

    fig.add_trace(
        go.Bar(
            x=pipe_labels,
            y=extra_kg_m,
            name="Extra Steel (kg/m)",
            marker_color=CHART_PALETTE[0],
            hovertemplate="<b>%{x}</b><br>Extra: %{y:.1f} kg/m<extra></extra>",
        ),
        secondary_y=False,
    )

    fig.add_trace(
        go.Scatter(
            x=pipe_labels,
            y=extra_pct,
            mode="lines+markers",
            name="WT Increase (%)",
            line=dict(color=CHART_PALETTE[1], width=3),
            marker=dict(size=8),
            hovertemplate="<b>%{x}</b><br>WT increase: %{y:.1f}%%<extra></extra>",
        ),
        secondary_y=True,
    )

    # Annotation
    most = most_conservative_codes[idx_12]
    least = least_conservative_codes[idx_12]
    fig.add_annotation(
        text=(
            f"On a {pipeline_length_km}km pipeline, choosing {most} over {least}<br>"
            f"costs an extra <b>{extra_tonnes:.0f} tonnes</b> of steel"
        ),
        xref="paper", yref="paper",
        x=0.5, y=-0.22,
        showarrow=False,
        font=dict(size=13, color=COLORS["text"]),
        align="center",
        bgcolor="rgba(237,137,54,0.1)",
        bordercolor=COLORS["accent"],
        borderwidth=1,
        borderpad=8,
    )

    fig.update_layout(
        title=dict(
            text="Weight Penalty: Most vs Least Conservative Code",
            font=dict(size=18),
        ),
        height=500,
        margin=dict(b=120),
    )
    fig.update_yaxes(title_text="Extra Steel Weight (kg/m)", secondary_y=False)
    fig.update_yaxes(title_text="Wall Thickness Increase (%)", secondary_y=True)

    penalty_info = {
        "pipeline_length_km": pipeline_length_km,
        "extra_tonnes_12in": extra_tonnes,
        "most_conservative": most,
        "least_conservative": least,
    }

    # Build serialisable weight penalty data for caching
    weight_penalty_data: Dict[str, Dict] = {}
    for i, ps in enumerate(PIPE_SIZES):
        weight_penalty_data[ps["name"]] = {
            "delta_kg_m": round(extra_kg_m[i], 2),
            "delta_pct": round(extra_pct[i], 1),
            "most_conservative": most_conservative_codes[i],
            "least_conservative": least_conservative_codes[i],
        }

    print("  ✓ Chart 4 complete")
    return fig, penalty_info, weight_penalty_data


# ---------------------------------------------------------------------------
# Chart 5: Governing Limit State Sunburst
# ---------------------------------------------------------------------------

def build_chart_5_sunburst(all_results: List[Dict]) -> go.Figure:
    """Build sunburst chart of governing limit states."""
    print("\n[Chart 5] Building Governing Limit State Sunburst...")

    # Normalize check names for display
    check_display = {
        "pressure_containment": "Pressure Containment",
        "collapse": "Collapse",
        "propagation_buckling": "Propagation Buckling",
        "combined_loading": "Combined Loading",
        "burst": "Pressure Containment",
        "hoop_stress": "Pressure Containment",
        "propagation": "Propagation Buckling",
    }

    # Count governing checks by code
    counts = defaultdict(lambda: defaultdict(int))
    for r in all_results:
        if r.get("governing_check"):
            code_name = r["code"]
            raw_check = r["governing_check"]
            check = check_display.get(raw_check, raw_check.replace("_", " ").title())
            counts[check][code_name] += 1

    # Build sunburst data
    labels = ["All Cases"]
    parents = [""]
    values = [0]
    colors = ["#f7fafc"]

    limit_state_colors = {
        "Pressure Containment": CHART_PALETTE[0],
        "Collapse": CHART_PALETTE[1],
        "Propagation Buckling": CHART_PALETTE[2],
        "Combined Loading": CHART_PALETTE[3],
    }

    total = sum(sum(v.values()) for v in counts.values())

    for check, code_counts in sorted(counts.items()):
        check_total = sum(code_counts.values())
        labels.append(check)
        parents.append("All Cases")
        values.append(check_total)
        colors.append(limit_state_colors.get(check, "#999"))

        for code_name, count in sorted(code_counts.items()):
            labels.append(f"{code_name}")
            parents.append(check)
            values.append(count)
            base_color = limit_state_colors.get(check, "#999")
            colors.append(base_color)

    values[0] = total

    fig = go.Figure(go.Sunburst(
        labels=labels,
        parents=parents,
        values=values,
        marker=dict(colors=colors),
        branchvalues="total",
        hovertemplate="<b>%{label}</b><br>Count: %{value}<br>%{percentRoot:.1%} of total<extra></extra>",
        textinfo="label+percent parent",
        insidetextorientation="radial",
    ))

    fig.update_layout(
        title=dict(
            text="Governing Limit State Distribution",
            font=dict(size=18),
        ),
        height=550,
        width=700,
    )

    print("  ✓ Chart 5 complete")
    return fig


# ---------------------------------------------------------------------------
# Main sweep
# ---------------------------------------------------------------------------

def select_analysis_wt(ps: Dict, pipe_catalog: list) -> float:
    """Select a sensible wall thickness for analysis.
    
    Uses the mid-range standard WT from the catalog — thick enough to be
    relevant for deepwater but not so thick that everything trivially passes.
    """
    for pipe in pipe_catalog:
        if abs(pipe["od_mm"] - ps["od_mm"]) < 0.5:
            wts = sorted([wt["wt_m"] for wt in pipe["wall_thicknesses"]])
            if len(wts) >= 3:
                return wts[len(wts) // 2]  # median WT
            elif wts:
                return wts[-1]  # use thickest if few options
    return ps["od_m"] * 0.05  # fallback


def run_parametric_sweep(pipe_catalog: list) -> Tuple[List[Dict], pd.DataFrame]:
    """Run the full parametric sweep across all cases."""
    pe = external_pressure_pa()
    total_cases = len(PIPE_SIZES) * len(CODES) * len(INTERNAL_PRESSURES_MPA)

    print(f"\n{'='*60}")
    print(f"  PARAMETRIC WALL THICKNESS SWEEP")
    print(f"  {total_cases} cases: {len(PIPE_SIZES)} pipes × {len(CODES)} codes × {len(INTERNAL_PRESSURES_MPA)} pressures")
    print(f"{'='*60}\n")

    all_results = []
    case_num = 0

    for ps in PIPE_SIZES:
        # Use mid-range WT for more interesting results
        wt_m = select_analysis_wt(ps, pipe_catalog)

        for code in CODES:
            for pi_mpa in INTERNAL_PRESSURES_MPA:
                case_num += 1
                code_name = CODE_NAMES[code]
                pi_pa = pi_mpa * 1e6

                print(f"  Case {case_num:3d}/{total_cases} | {ps['name']:>4s} X65 WT={wt_m*1000:.1f}mm | {code_name:<14s} | {pi_mpa} MPa...",
                      end="")

                try:
                    result = run_single_analysis(ps["od_m"], wt_m, pi_pa, pe, code)
                    if result:
                        record = {
                            "pipe_size": ps["name"],
                            "od_mm": ps["od_mm"],
                            "od_m": ps["od_m"],
                            "wt_mm": round(wt_m * 1000, 2),
                            "wt_m": wt_m,
                            "grade": GRADE,
                            "code": code_name,
                            "internal_pressure_mpa": pi_mpa,
                            "external_pressure_mpa": round(pe / 1e6, 3),
                            "water_depth_m": WATER_DEPTH,
                            "is_safe": result["is_safe"],
                            "governing_check": result["governing_check"],
                            "max_utilisation": round(result["max_utilisation"], 4),
                            "checks": {k: round(v, 4) for k, v in result["checks"].items()},
                        }
                        all_results.append(record)
                        status = "PASS" if result["is_safe"] else "FAIL"
                        print(f" util={result['max_utilisation']:.3f} [{status}]")
                    else:
                        all_results.append({
                            "pipe_size": ps["name"],
                            "od_mm": ps["od_mm"],
                            "code": code_name,
                            "internal_pressure_mpa": pi_mpa,
                            "is_safe": "N/A",
                            "governing_check": "N/A",
                            "max_utilisation": "N/A",
                        })
                        print(" [N/A]")
                except Exception as exc:
                    print(f" [ERROR: {exc}]")
                    all_results.append({
                        "pipe_size": ps["name"],
                        "od_mm": ps["od_mm"],
                        "code": code_name,
                        "internal_pressure_mpa": pi_mpa,
                        "is_safe": "N/A",
                        "governing_check": str(exc),
                        "max_utilisation": "N/A",
                    })

    df = pd.DataFrame(all_results)
    print(f"\n  ✓ Sweep complete: {len(all_results)} results collected")
    return all_results, df


# ---------------------------------------------------------------------------
# Summary table
# ---------------------------------------------------------------------------

def build_summary_table(cached_summary: Optional[List[Dict]] = None) -> pd.DataFrame:
    """Build summary: pipe size x code -> min WT, governing check.

    When *cached_summary* is provided (list of row dicts from JSON), rebuild
    the DataFrame directly without running engineering calculations.
    """
    if cached_summary is not None:
        return pd.DataFrame(cached_summary)

    pe = external_pressure_pa()
    pi = 20e6

    rows = []
    for ps in PIPE_SIZES:
        for code in CODES:
            min_wt, gov = find_min_wall_thickness(ps["od_m"], pi, pe, code)
            rows.append({
                "Pipe Size": ps["name"],
                "OD (mm)": ps["od_mm"],
                "Code": CODE_NAMES[code],
                "Min WT (mm)": round(min_wt * 1000, 2),
                "Governing Check": (gov or "N/A").replace("_", " ").title(),
                "Status": "PASS" if min_wt < ps["od_m"] / 2 * 0.9 else "MARGINAL",
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
    penalty_info: Dict,
    total_cases: int,
) -> str:
    """Build the branded HTML report."""
    print("\n[Report] Building HTML report...")

    report = GTMReportBuilder(
        title="Pipeline Wall Thickness — Multi-Code Comparison",
        subtitle=f"{total_cases} parametric cases across 6 pipe sizes, 3 design codes, and 4 operating pressures",
        demo_id="demo_02",
        case_count=total_cases,
        code_refs=[
            "DNV-ST-F101 (2021) — Submarine Pipeline Systems",
            "API RP 1111 (2015) — Offshore Hydrocarbon Pipelines (Limit State Design)",
            "PD 8010-2 (2015) — Code of Practice for Pipelines – Part 2: Subsea Pipelines",
            "API 5L (2018) — Line Pipe (material grades)",
        ],
    )

    # Methodology section
    methodology_html = """
    <p>This analysis compares minimum wall thickness requirements across three widely-used
    subsea pipeline design codes. Each code uses a different philosophy:</p>
    
    <h3>DNV-ST-F101 (2021)</h3>
    <p>Uses <strong>Load and Resistance Factor Design (LRFD)</strong> with partial safety factors
    on both loads and resistance. The most granular approach with separate factors for safety class
    (Low/Medium/High), material resistance (γ<sub>m</sub>=1.15), and condition
    (γ<sub>SC</sub> varies by safety class). Performs four limit state checks: pressure containment,
    collapse, propagation buckling, and combined loading.</p>
    
    <h3>API RP 1111 (2015)</h3>
    <p>Uses <strong>Working Stress Design (WSD)</strong> with single design factors applied to
    material strength. The burst check uses a Barlow-based approach with f<sub>d</sub>=0.72.
    Collapse uses an elastic-plastic transition formula. Generally the least conservative
    of the three codes for typical offshore conditions.</p>
    
    <h3>PD 8010-2 (2015)</h3>
    <p>Also uses <strong>Working Stress Design (WSD)</strong> based on British Standards tradition.
    Similar design factor approach (f<sub>d</sub>=0.72 for hoop stress) with an additional
    Von Mises equivalent stress check (σ<sub>e</sub> ≤ 0.9 × SMYS). Includes collapse and
    propagation checks similar to API.</p>
    
    <h3>Common Parameters</h3>
    <ul>
        <li>Grade: API 5L X65 (SMYS = 448 MPa, SMTS = 531 MPa)</li>
        <li>Water depth: 500 m (external pressure ≈ 5.02 MPa)</li>
        <li>Internal pressures: 10, 15, 20, 25 MPa</li>
        <li>Corrosion allowance: 1 mm</li>
        <li>Safety Class: Medium (DNV), standard factors (API, PD 8010-2)</li>
    </ul>
    """
    report.add_methodology(methodology_html)

    # Charts
    report.add_chart(
        "lifecycle_utilisation",
        fig1,
        title="Chart 1: Lifecycle Utilisation by Design Code",
        subtitle="Max utilisation across limit states for each lifecycle phase. Use dropdown to select pipe size.",
    )

    report.add_chart(
        "min_wt_vs_pipe_size",
        fig2,
        title="Chart 2: Minimum Required Wall Thickness vs Pipe Size",
        subtitle="At 20 MPa internal pressure, 500m water depth, X65 grade. Shaded band shows code penalty.",
    )

    report.add_chart(
        "utilisation_heatmap",
        fig3,
        title="Chart 3: Utilisation Heatmap — Pipe Size × Internal Pressure",
        subtitle="Each cell uses the minimum standard WT from the pipe catalog. Green = safe, Red = overstressed.",
    )

    report.add_chart(
        "weight_penalty",
        fig4,
        title="Chart 4: Steel Weight Penalty — Code Conservatism Cost",
        subtitle="Difference in required WT between most and least conservative code, at 20 MPa.",
    )

    report.add_chart(
        "governing_limit_state",
        fig5,
        title="Chart 5: Governing Limit State Distribution",
        subtitle="Which limit state governs across all parametric cases, broken down by code.",
    )

    # Summary table
    report.add_table(
        "Summary: Minimum Wall Thickness by Pipe Size and Code",
        summary_df,
        subtitle="At 20 MPa internal pressure, 500m water depth, X65 grade, Safety Class Medium",
        status_col="Status",
    )

    # Live mode teaser
    report.add_live_mode_teaser(
        analysis_type="wall thickness verification"
    )

    # Assumptions
    report.add_assumptions([
        "All analyses use API 5L X65 grade (SMYS = 448 MPa, SMTS = 531 MPa)",
        "Water depth fixed at 500 m for all cases (P_ext ≈ 5.02 MPa)",
        "Corrosion allowance = 1.0 mm internal, 0.0 mm external (intact coating assumed)",
        "Fabrication tolerance = 12.5% (DNV default for seamless pipe)",
        "Ovality = 0.5% (Dmax-Dmin)/Dnom — conservative for good-quality pipe",
        "Seamless pipe fabrication (α_fab = 1.0 for DNV, no UOE derating)",
        "Temperature derating not applied (operating temperature < 50°C assumed)",
        "Safety Class Medium used for DNV (typical for production pipelines)",
        "Lifecycle phases use simplified bending moments and tensions",
        "No dynamic loading or fatigue effects considered",
        "Wall thicknesses from standard pipe schedules per ASME B36.10M",
    ])

    # Build and save
    output_path = OUTPUT_DIR / "demo_02_wall_thickness_report.html"
    html = report.build(output_path)

    print(f"  ✓ Report saved to: {output_path}")
    return html


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    """Run the full demo pipeline."""
    parser = argparse.ArgumentParser(
        description="GTM Demo 2: Pipeline Wall Thickness — Multi-Code Comparison",
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
    print("  GTM Demo 2: Pipeline Wall Thickness — Multi-Code Comparison")
    print("=" * 60)

    results_path = RESULTS_DIR / "demo_02_wall_thickness_results.json"

    # 1. Load data files
    print("\n[1/7] Loading input data...")
    with open(DATA_DIR / "pipelines.json", "r") as f:
        pipe_data = json.load(f)
    pipe_catalog = pipe_data["pipes"]
    print(f"  ✓ Loaded {len(pipe_catalog)} pipe sizes from pipelines.json")

    with open(DATA_DIR / "design_codes.json", "r") as f:
        code_data = json.load(f)
    print(f"  ✓ Loaded {len(code_data['codes'])} design codes from design_codes.json")

    # ── Step 2: Run sweep or load cache ────────────────────────────────────
    if args.from_cache and results_path.exists() and not args.force:
        print("\n[2/7] Loading cached results...")
        with open(results_path, "r") as f:
            cached = json.load(f)

        all_results = cached.get("results", [])
        results_df = pd.DataFrame(all_results)
        total_cases = len(all_results)
        cached_lifecycle = cached.get("lifecycle_phases")
        cached_min_wt = cached.get("min_wall_thickness")
        cached_weight_penalty = cached.get("weight_penalty")
        cached_summary = cached.get("summary")
        print(f"  Loaded {total_cases} cached results from {results_path.name}")
    else:
        # Load engineering modules and run full calculations
        _load_engineering_modules()
        _init_code_constants()

        print("\n[2/7] Running parametric sweep...")
        all_results, results_df = run_parametric_sweep(pipe_catalog)
        total_cases = len(all_results)
        cached_lifecycle = None
        cached_min_wt = None
        cached_weight_penalty = None
        cached_summary = None

    # 3. Build charts
    print("\n[3/7] Building charts...")
    fig1, lifecycle_data = build_chart_1_lifecycle(
        pipe_catalog=pipe_catalog,
        cached_lifecycle=cached_lifecycle,
    )
    fig2, min_wt_data = build_chart_2_min_wt(
        cached_min_wt=cached_min_wt,
    )
    fig3 = build_chart_3_heatmap(
        pipe_catalog=pipe_catalog,
        cached_results=all_results if (args.from_cache and not args.force) else None,
    )
    fig4, penalty_info, weight_penalty_data = build_chart_4_weight_penalty(
        cached_weight_penalty=cached_weight_penalty,
        cached_min_wt=cached_min_wt,
    )
    fig5 = build_chart_5_sunburst(all_results)

    # 4. Build summary table
    print("\n[4/7] Building summary table...")
    summary_df = build_summary_table(cached_summary=cached_summary)
    print(summary_df.to_string(index=False))

    # 5. Build HTML report
    print("\n[5/7] Building HTML report...")
    build_report(fig1, fig2, fig3, fig4, fig5, summary_df, all_results, penalty_info, total_cases)

    # 6. Save JSON results
    if not args.from_cache or args.force:
        print("\n[6/7] Saving JSON results...")
        RESULTS_DIR.mkdir(parents=True, exist_ok=True)

        json_output = {
            "metadata": {
                "demo": "GTM Demo 2: Wall Thickness Multi-Code Comparison",
                "total_cases": total_cases,
                "pipe_sizes": [ps["name"] for ps in PIPE_SIZES],
                "codes": CODE_NAME_STRINGS,
                "internal_pressures_mpa": INTERNAL_PRESSURES_MPA,
                "water_depth_m": WATER_DEPTH,
                "grade": GRADE,
                "smys_mpa": SMYS / 1e6,
                "smts_mpa": SMTS / 1e6,
                "corrosion_allowance_mm": CORROSION_ALLOWANCE * 1000,
            },
            "summary": summary_df.to_dict(orient="records"),
            "results": all_results,
            "lifecycle_phases": lifecycle_data,
            "min_wall_thickness": min_wt_data,
            "weight_penalty": weight_penalty_data,
        }

        with open(results_path, "w") as f:
            json.dump(json_output, f, indent=2, default=str)
        print(f"  ✓ Results saved to: {results_path}")
    else:
        print("\n[6/7] Skipping JSON save (loaded from cache)")

    # 7. Done
    elapsed = time.time() - start_time
    print(f"\n[7/7] Complete!")
    print(f"{'='*60}")
    print(f"  Total cases analysed:  {total_cases}")
    print(f"  HTML report:           output/demo_02_wall_thickness_report.html")
    print(f"  JSON results:          results/demo_02_wall_thickness_results.json")
    print(f"  Time elapsed:          {elapsed:.1f} seconds")
    print(f"{'='*60}")


if __name__ == "__main__":
    main()
