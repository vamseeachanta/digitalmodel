#!/usr/bin/env python3
# ABOUTME: GTM Demo 8 — Impressed Current Cathodic Protection (ICCP) System Design
# ABOUTME: Parametric sweep over structure area x current density x anode bed/material x cable run
"""
GTM Demo 8: ICCP Rectifier & Ground-Bed Sizing
==============================================

Parametric ICCP design screening for fixed platforms and vessel hulls. The
demo is a PRESENTATION / SWEEP layer over the existing
``digitalmodel.cathodic_protection.iccp_design`` module — it reuses that
module's physics (rectifier sizing, anode-bed design, cable sizing) and adds
a parametric cross-product, a results table, and a branded HTML report.

Per case the demo chains the three iccp_design routines:

  1. ``anode_bed_design``  -> number of anodes + ground-bed resistance + life
  2. ``cable_sizing``      -> cable cross-section + cable resistance (V-drop)
  3. ``rectifier_sizing``  -> DC output V/A, power, recommended TR rating

Parametric axes (the cross-product):
  - 4 structures (small jacket -> large FPSO hull) — surface area
  - 3 coating-breakdown current-density demands (final/mean profiles, A/m2)
  - 3 anode materials x their representative bed type
  - 4 cable-run lengths (voltage-drop sizing)

= 4 x 3 x 3 x 4 = 144 cases.

Outputs:
  - results table (pandas)
  - 4 interactive Plotly charts (falls back to static HTML note if plotly absent)
  - branded HTML report via GTMReportBuilder
  - JSON results cache with --from-cache support

Usage:
    cd digitalmodel
    PYTHONPATH=examples/demos/gtm:src python \\
        examples/demos/gtm/demo_08_iccp_design.py

    # Reuse cached results:
    PYTHONPATH=examples/demos/gtm:src python \\
        examples/demos/gtm/demo_08_iccp_design.py --from-cache
"""

from __future__ import annotations

import argparse
import json
import sys
import time
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

# ---------------------------------------------------------------------------
# Backing module — the ICCP physics (REUSED, not reimplemented).
# ---------------------------------------------------------------------------
from digitalmodel.cathodic_protection.iccp_design import (
    AnodeBedType,
    AnodeMaterial,
    RectifierSizingInput,
    anode_bed_design,
    cable_sizing,
    rectifier_sizing,
)

# ---------------------------------------------------------------------------
# Optional deps — pandas is required for the table; plotly drives the charts.
# When plotly is missing the report falls back to a static HTML note.
# ---------------------------------------------------------------------------
try:
    import pandas as pd
except ImportError as exc:  # pragma: no cover - environment guard
    print(f"[ERROR] Missing dependency: {exc}")
    print("        Install with: pip install pandas")
    sys.exit(1)

try:
    import plotly.graph_objects as go

    HAVE_PLOTLY = True
except ImportError:  # pragma: no cover - exercised only on plotly-less hosts
    go = None  # type: ignore[assignment]
    HAVE_PLOTLY = False

try:
    from report_template import COLORS, CHART_PALETTE, GTMReportBuilder
except ImportError:  # pragma: no cover - packaged import path fallback
    from examples.demos.gtm.report_template import (  # type: ignore[no-redef]
        COLORS,
        CHART_PALETTE,
        GTMReportBuilder,
    )

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
SCRIPT_DIR = Path(__file__).resolve().parent
OUTPUT_DIR = SCRIPT_DIR / "output"
RESULTS_DIR = SCRIPT_DIR / "results"
DEMO_ID = "demo_08"
REPORT_NAME = "demo_08_iccp_design_report.html"
RESULTS_NAME = "demo_08_iccp_design_results.json"

# ---------------------------------------------------------------------------
# Parametric space
# ---------------------------------------------------------------------------
# Structures — bare/coated steel surface area to be protected [m2].
STRUCTURES: List[Dict[str, Any]] = [
    {"id": "STR-1", "name": "Small jacket (shallow)", "surface_area_m2": 1500.0,
     "environment": "seawater", "resistivity_ohm_m": 0.30},
    {"id": "STR-2", "name": "Medium jacket", "surface_area_m2": 6000.0,
     "environment": "seawater", "resistivity_ohm_m": 0.30},
    {"id": "STR-3", "name": "Large platform", "surface_area_m2": 15000.0,
     "environment": "seawater", "resistivity_ohm_m": 0.25},
    {"id": "STR-4", "name": "FPSO hull", "surface_area_m2": 28000.0,
     "environment": "seawater", "resistivity_ohm_m": 0.25},
]

# Design current-density demand profiles [A/m2]. These represent the coating
# breakdown / bare-steel demand the rectifier must satisfy (DNV-RP-B401 /
# NACE SP0169 style mean-to-final ranges for marine immersion).
CURRENT_DENSITIES: List[Dict[str, Any]] = [
    {"id": "CD-low", "label": "Well-coated (mean)", "j_a_per_m2": 0.020},
    {"id": "CD-mid", "label": "Aged coating (final)", "j_a_per_m2": 0.060},
    {"id": "CD-high", "label": "Bare / damaged", "j_a_per_m2": 0.110},
]

# Anode material -> representative anode bed type and physical anode geometry.
ANODE_OPTIONS: List[Dict[str, Any]] = [
    {"id": "AN-mmo", "material": AnodeMaterial.MIXED_METAL_OXIDE,
     "bed_type": AnodeBedType.DISTRIBUTED, "label": "MMO (distributed)",
     "anode_length_m": 1.0, "anode_diameter_m": 0.025},
    {"id": "AN-fesi", "material": AnodeMaterial.HIGH_SILICON_CAST_IRON,
     "bed_type": AnodeBedType.DEEP_WELL, "label": "Si-Fe (deep well)",
     "anode_length_m": 1.5, "anode_diameter_m": 0.075},
    {"id": "AN-pt", "material": AnodeMaterial.PLATINIZED_TITANIUM,
     "bed_type": AnodeBedType.SHALLOW_HORIZONTAL, "label": "Pt-Ti (shallow)",
     "anode_length_m": 1.2, "anode_diameter_m": 0.025},
]

# Cable-run lengths (one-way, anode bed -> rectifier) [m].
CABLE_LENGTHS_M: List[float] = [50.0, 150.0, 300.0, 500.0]

# Design constants.
DESIGN_LIFE_YEARS = 25.0
MAX_VOLTAGE_DROP_V = 2.0
STRUCTURE_COATING_RESISTANCE_OHM = 0.5  # structure/return path resistance [ohm]
SAFETY_FACTOR = 1.25

# A "healthy" ICCP design wants the recommended rectifier to comfortably cover
# the demand. We flag utilisation = required / recommended for each axis.
RATING_MARGINAL = 0.85  # >= this = MARGINAL (little headroom)


# ---------------------------------------------------------------------------
# Single-case runner — chains the three iccp_design routines.
# ---------------------------------------------------------------------------

def run_single_case(
    structure: Dict[str, Any],
    current_density: Dict[str, Any],
    anode: Dict[str, Any],
    cable_length_m: float,
) -> Dict[str, Any]:
    """Size an ICCP system for one parameter combination via iccp_design.

    total_current = surface_area x design_current_density
    Then: anode bed -> ground-bed resistance, cable -> cable resistance,
    rectifier -> DC V/A + standard rating.
    """
    total_current_A = structure["surface_area_m2"] * current_density["j_a_per_m2"]

    # 1) Anode bed design (number of anodes, bed resistance, life).
    bed = anode_bed_design(
        total_current_A=total_current_A,
        soil_resistivity_ohm_m=structure["resistivity_ohm_m"],
        design_life_years=DESIGN_LIFE_YEARS,
        bed_type=anode["bed_type"],
        anode_material=anode["material"],
        anode_length_m=anode["anode_length_m"],
        anode_diameter_m=anode["anode_diameter_m"],
    )

    # 2) Cable sizing (cross-section + cable resistance for V-drop).
    cable = cable_sizing(
        current_A=total_current_A,
        cable_length_m=cable_length_m,
        max_voltage_drop_V=MAX_VOLTAGE_DROP_V,
    )

    # 3) Rectifier sizing (DC output V/A, power, recommended TR rating).
    rect = rectifier_sizing(
        RectifierSizingInput(
            total_current_A=total_current_A,
            ground_bed_resistance_ohm=bed.bed_resistance_ohm,
            structure_coating_resistance_ohm=STRUCTURE_COATING_RESISTANCE_OHM,
            cable_resistance_ohm=cable["cable_resistance_ohm"],
            safety_factor=SAFETY_FACTOR,
        )
    )

    # Rating utilisations: required vs the recommended standard rating.
    v_util = rect.dc_voltage_V / rect.recommended_rating_V if rect.recommended_rating_V else 0.0
    a_util = total_current_A / rect.recommended_rating_A if rect.recommended_rating_A else 0.0
    max_util = max(v_util, a_util)

    # Status: NO_FIT if demand exceeds the largest standard rating (util > 1),
    # MARGINAL if close to the chosen standard rating, else OK.
    if max_util > 1.0:
        status = "NO_FIT"
    elif max_util >= RATING_MARGINAL:
        status = "MARGINAL"
    else:
        status = "OK"

    life_ok = bed.estimated_life_years >= DESIGN_LIFE_YEARS

    return {
        "structure_id": structure["id"],
        "structure": structure["name"],
        "surface_area_m2": structure["surface_area_m2"],
        "current_density_id": current_density["id"],
        "current_density_label": current_density["label"],
        "j_a_per_m2": current_density["j_a_per_m2"],
        "anode_id": anode["id"],
        "anode_label": anode["label"],
        "anode_material": anode["material"].value,
        "bed_type": anode["bed_type"].value,
        "cable_length_m": cable_length_m,
        "total_current_A": round(total_current_A, 2),
        # Anode bed
        "number_of_anodes": bed.number_of_anodes,
        "bed_resistance_ohm": bed.bed_resistance_ohm,
        "anode_life_years": bed.estimated_life_years,
        "life_ok": life_ok,
        # Cable
        "cable_area_mm2": cable["selected_area_mm2"],
        "cable_resistance_ohm": cable["cable_resistance_ohm"],
        "cable_voltage_drop_V": cable["voltage_drop_V"],
        # Rectifier
        "rect_dc_voltage_V": rect.dc_voltage_V,
        "rect_dc_current_A": rect.dc_current_A,
        "rect_power_W": rect.power_W,
        "rect_rating_V": rect.recommended_rating_V,
        "rect_rating_A": rect.recommended_rating_A,
        "rating_utilisation": round(max_util, 4),
        "status": status,
    }


# ---------------------------------------------------------------------------
# Parametric sweep
# ---------------------------------------------------------------------------

def run_parametric_sweep() -> List[Dict[str, Any]]:
    """Run the full cross-product sweep over the parametric axes."""
    total = (
        len(STRUCTURES) * len(CURRENT_DENSITIES)
        * len(ANODE_OPTIONS) * len(CABLE_LENGTHS_M)
    )
    print(f"\n{'='*60}")
    print("  PARAMETRIC ICCP DESIGN SWEEP")
    print(f"  {total} cases: {len(STRUCTURES)} structures x "
          f"{len(CURRENT_DENSITIES)} current densities x "
          f"{len(ANODE_OPTIONS)} anode types x {len(CABLE_LENGTHS_M)} cable runs")
    print(f"{'='*60}\n")

    results: List[Dict[str, Any]] = []
    case_num = 0
    for structure in STRUCTURES:
        for cd in CURRENT_DENSITIES:
            for anode in ANODE_OPTIONS:
                for cable_len in CABLE_LENGTHS_M:
                    case_num += 1
                    r = run_single_case(structure, cd, anode, cable_len)
                    results.append(r)
                    if case_num % 36 == 0 or case_num == total:
                        print(f"  Case {case_num:3d}/{total} | "
                              f"{r['structure']:>18s} | {r['anode_label']:>16s} | "
                              f"I={r['total_current_A']:>7.1f}A | "
                              f"TR {r['rect_rating_V']:.0f}V/{r['rect_rating_A']:.0f}A | "
                              f"{r['status']}")
    print(f"\n  Sweep complete: {len(results)} cases")
    return results


# ---------------------------------------------------------------------------
# Results table
# ---------------------------------------------------------------------------

def build_summary_df(results: List[Dict[str, Any]]) -> pd.DataFrame:
    """Full per-case results table."""
    rows = []
    for r in results:
        rows.append({
            "Structure": r["structure"],
            "Area (m2)": r["surface_area_m2"],
            "Demand": r["current_density_label"],
            "Anode": r["anode_label"],
            "Cable (m)": r["cable_length_m"],
            "I (A)": r["total_current_A"],
            "Anodes": r["number_of_anodes"],
            "Bed R (ohm)": r["bed_resistance_ohm"],
            "Life (yr)": r["anode_life_years"],
            "Cable (mm2)": r["cable_area_mm2"],
            "DC V": r["rect_dc_voltage_V"],
            "TR Rating": f"{r['rect_rating_V']:.0f}V / {r['rect_rating_A']:.0f}A",
            "Power (W)": r["rect_power_W"],
            "Status": r["status"],
        })
    return pd.DataFrame(rows)


def build_headline_df(results: List[Dict[str, Any]]) -> pd.DataFrame:
    """Compact headline: per structure x demand at the reference anode/cable."""
    ref_anode = "AN-mmo"
    ref_cable = 150.0
    rows = []
    for r in results:
        if r["anode_id"] == ref_anode and r["cable_length_m"] == ref_cable:
            rows.append({
                "Structure": r["structure"],
                "Area (m2)": r["surface_area_m2"],
                "Demand": r["current_density_label"],
                "I (A)": r["total_current_A"],
                "Anodes": r["number_of_anodes"],
                "Bed R (ohm)": r["bed_resistance_ohm"],
                "DC V": r["rect_dc_voltage_V"],
                "TR Rating": f"{r['rect_rating_V']:.0f}V / {r['rect_rating_A']:.0f}A",
                "Power (W)": r["rect_power_W"],
                "Status": r["status"],
            })
    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# Charts (plotly). Each returns a go.Figure or None when plotly is unavailable.
# ---------------------------------------------------------------------------

def _ref(results, anode_id=None, cable_len=None, cd_id=None):
    """Filter helper for chart slices."""
    out = results
    if anode_id is not None:
        out = [r for r in out if r["anode_id"] == anode_id]
    if cable_len is not None:
        out = [r for r in out if r["cable_length_m"] == cable_len]
    if cd_id is not None:
        out = [r for r in out if r["current_density_id"] == cd_id]
    return out


def build_chart_rectifier_vs_area(results) -> Optional["go.Figure"]:
    """Recommended rectifier rating (V & A) vs structure surface area.

    One curve per current-density demand, at the reference anode + cable run.
    """
    if not HAVE_PLOTLY:
        return None
    sub = _ref(results, anode_id="AN-mmo", cable_len=150.0)
    fig = go.Figure()
    for i, cd in enumerate(CURRENT_DENSITIES):
        pts = [r for r in sub if r["current_density_id"] == cd["id"]]
        pts.sort(key=lambda r: r["surface_area_m2"])
        fig.add_trace(go.Scatter(
            x=[r["surface_area_m2"] for r in pts],
            y=[r["rect_rating_A"] for r in pts],
            mode="lines+markers", name=cd["label"],
            line=dict(color=CHART_PALETTE[i % len(CHART_PALETTE)], width=3),
            marker=dict(size=9),
            hovertemplate="Area: %{x:.0f} m2<br>TR current: %{y:.0f} A<extra></extra>",
        ))
    fig.update_layout(
        title=dict(text="Rectifier Current Rating vs Structure Surface Area",
                   font=dict(size=18)),
        xaxis_title="Surface Area (m2)",
        yaxis_title="Recommended TR Current Rating (A)",
        height=500,
    )
    return fig


def build_chart_bed_resistance_by_material(results) -> Optional["go.Figure"]:
    """Ground-bed resistance by anode material/bed type, per structure.

    Reference current-density (aged/final) and cable run.
    """
    if not HAVE_PLOTLY:
        return None
    sub = _ref(results, cable_len=150.0, cd_id="CD-mid")
    fig = go.Figure()
    structures = [s["name"] for s in STRUCTURES]
    for i, anode in enumerate(ANODE_OPTIONS):
        ys = []
        for s in STRUCTURES:
            match = [r for r in sub if r["anode_id"] == anode["id"]
                     and r["structure_id"] == s["id"]]
            ys.append(match[0]["bed_resistance_ohm"] if match else None)
        fig.add_trace(go.Bar(
            x=structures, y=ys, name=anode["label"],
            marker_color=CHART_PALETTE[i % len(CHART_PALETTE)],
            hovertemplate="%{x}<br>Bed R: %{y:.3f} ohm<extra></extra>",
        ))
    fig.update_layout(
        title=dict(text="Ground-Bed Resistance by Anode Material",
                   font=dict(size=18)),
        xaxis_title="Structure", yaxis_title="Anode Bed Resistance (ohm)",
        barmode="group", height=500,
    )
    return fig


def build_chart_power_envelope(results) -> Optional["go.Figure"]:
    """System power demand envelope: power vs total current, colored by status."""
    if not HAVE_PLOTLY:
        return None
    sub = _ref(results, cable_len=150.0)
    status_color = {"OK": COLORS["success"], "MARGINAL": COLORS["warning"],
                    "NO_FIT": COLORS["danger"]}
    fig = go.Figure()
    for status in ("OK", "MARGINAL", "NO_FIT"):
        pts = [r for r in sub if r["status"] == status]
        if not pts:
            continue
        fig.add_trace(go.Scatter(
            x=[r["total_current_A"] for r in pts],
            y=[r["rect_power_W"] for r in pts],
            mode="markers", name=status,
            marker=dict(size=11, color=status_color[status],
                        line=dict(width=1, color="white")),
            text=[f"{r['structure']} | {r['anode_label']}" for r in pts],
            hovertemplate="%{text}<br>I: %{x:.0f} A<br>Power: %{y:.0f} W<extra></extra>",
        ))
    fig.update_layout(
        title=dict(text="ICCP System Power-Demand Envelope", font=dict(size=18)),
        xaxis_title="Total Protection Current (A)",
        yaxis_title="Rectifier Power (W)", height=500,
    )
    return fig


def build_chart_cable_voltage_drop(results) -> Optional["go.Figure"]:
    """Cable voltage drop vs cable-run length, per current-density demand.

    Reference structure (large platform) + anode. Includes the V-drop limit.
    """
    if not HAVE_PLOTLY:
        return None
    sub = [r for r in results if r["structure_id"] == "STR-3"
           and r["anode_id"] == "AN-mmo"]
    fig = go.Figure()
    for i, cd in enumerate(CURRENT_DENSITIES):
        pts = [r for r in sub if r["current_density_id"] == cd["id"]]
        pts.sort(key=lambda r: r["cable_length_m"])
        fig.add_trace(go.Scatter(
            x=[r["cable_length_m"] for r in pts],
            y=[r["cable_voltage_drop_V"] for r in pts],
            mode="lines+markers", name=cd["label"],
            line=dict(color=CHART_PALETTE[i % len(CHART_PALETTE)], width=3),
            marker=dict(size=9),
            hovertemplate="Run: %{x:.0f} m<br>V-drop: %{y:.3f} V<extra></extra>",
        ))
    fig.add_hline(
        y=MAX_VOLTAGE_DROP_V, line_dash="dash", line_color=COLORS["danger"],
        line_width=2,
        annotation_text=f"V-drop limit = {MAX_VOLTAGE_DROP_V:g} V",
        annotation_position="top left",
    )
    fig.update_layout(
        title=dict(text="Cable Voltage Drop vs Run Length (Large Platform)",
                   font=dict(size=18)),
        xaxis_title="Cable Run Length, one-way (m)",
        yaxis_title="Voltage Drop (V)", height=500,
    )
    return fig


# ---------------------------------------------------------------------------
# HTML report
# ---------------------------------------------------------------------------

def build_report(
    results: List[Dict[str, Any]],
    output_path: Optional[Path] = None,
) -> str:
    """Build the branded HTML report mirroring the repo demo report style."""
    print("\n[Report] Building HTML report...")
    total_cases = len(results)
    headline_df = build_headline_df(results)
    summary_df = build_summary_df(results)

    report = GTMReportBuilder(
        title="ICCP Rectifier & Ground-Bed Sizing",
        subtitle=(
            f"{total_cases} parametric cases across 4 structures, "
            "3 current-density demands, 3 anode materials, and 4 cable runs"
        ),
        demo_id=DEMO_ID,
        case_count=total_cases,
        code_refs=[
            "NACE SP0169 (2013) — Control of External Corrosion on Submerged Metallic Piping",
            "API RP 1632 (1996) §7 — Impressed Current Systems",
            "DNV-RP-B401 (2021) — Cathodic Protection Design (current-density demand)",
            "NACE TM0497 — Measurement Techniques for CP Criteria",
        ],
    )

    methodology_html = f"""
    <p>This analysis screens <strong>impressed current cathodic protection
    (ICCP)</strong> system sizing across a parametric matrix of structures,
    coating-breakdown current demands, anode materials, and cable runs. Each
    case chains the three <code>digitalmodel.cathodic_protection.iccp_design</code>
    routines (the demo reuses the module physics, it does not reimplement it):</p>

    <h3>Step 1: Total Current Demand</h3>
    <p>Total protection current = structure surface area &times; design current
    density. The current-density demand spans a well-coated mean
    ({CURRENT_DENSITIES[0]['j_a_per_m2']:g} A/m&sup2;) through aged-coating final
    ({CURRENT_DENSITIES[1]['j_a_per_m2']:g} A/m&sup2;) to bare/damaged steel
    ({CURRENT_DENSITIES[2]['j_a_per_m2']:g} A/m&sup2;), a DNV-RP-B401 style range.</p>

    <h3>Step 2: Anode Ground-Bed Design (<code>anode_bed_design</code>)</h3>
    <p>Sizes the number of anodes from the per-anode current capacity and the
    consumption/life constraint, then computes the bed resistance via the Dwight
    single-anode equation with a parallel/interference adjustment (deep-well beds
    get a depth resistance reduction). Anode life is checked against the
    {DESIGN_LIFE_YEARS:g}-year design life.</p>

    <h3>Step 3: Cable Sizing (<code>cable_sizing</code>)</h3>
    <p>Selects the minimum standard copper cross-section so the round-trip
    voltage drop stays within {MAX_VOLTAGE_DROP_V:g} V over the one-way cable run,
    returning the resulting cable resistance fed into the rectifier circuit.</p>

    <h3>Step 4: Rectifier Sizing (<code>rectifier_sizing</code>)</h3>
    <p>V<sub>dc</sub> = I &times; (R<sub>bed</sub> + R<sub>structure</sub> +
    R<sub>cable</sub>) + V<sub>back-emf</sub>, with a {SAFETY_FACTOR:g} safety
    factor, rounded up to the nearest standard transformer-rectifier (TR) rating.
    The structure/return resistance is taken as
    {STRUCTURE_COATING_RESISTANCE_OHM:g} &ohm;.</p>

    <h3>Status Classification</h3>
    <ul>
        <li><strong>OK:</strong> demand fits a standard TR rating with headroom
        (rating utilisation &lt; {RATING_MARGINAL:g})</li>
        <li><strong>MARGINAL:</strong> demand fits but with little headroom
        (utilisation in [{RATING_MARGINAL:g}, 1.0])</li>
        <li><strong>NO_FIT:</strong> demand exceeds the largest standard TR
        rating (utilisation &gt; 1.0) &mdash; split into multiple units</li>
    </ul>
    """
    report.add_methodology(methodology_html)

    # Charts (or a static note when plotly is unavailable).
    if HAVE_PLOTLY:
        fig1 = build_chart_rectifier_vs_area(results)
        fig2 = build_chart_bed_resistance_by_material(results)
        fig3 = build_chart_power_envelope(results)
        fig4 = build_chart_cable_voltage_drop(results)
        report.add_chart(
            "rect_vs_area", fig1,
            title="Chart 1: Rectifier Current Rating vs Surface Area",
            subtitle="MMO anode, 150 m cable run. One curve per current-density demand.",
        )
        report.add_chart(
            "bed_resistance", fig2,
            title="Chart 2: Ground-Bed Resistance by Anode Material",
            subtitle="Aged-coating demand, 150 m cable run. Grouped by structure.",
        )
        report.add_chart(
            "power_envelope", fig3,
            title="Chart 3: System Power-Demand Envelope",
            subtitle="All cases at 150 m cable run, colored by sizing status.",
        )
        report.add_chart(
            "cable_vdrop", fig4,
            title="Chart 4: Cable Voltage Drop vs Run Length",
            subtitle=f"Large platform, MMO anode. Red dashed = {MAX_VOLTAGE_DROP_V:g} V limit.",
        )
    else:  # pragma: no cover - plotly-less host
        report.add_section(
            "Charts",
            "<p class='note'><em>plotly is not installed in this environment, so "
            "the interactive charts were omitted. The full numeric results are in "
            "the tables below; install plotly to regenerate the charts.</em></p>",
        )

    report.add_table(
        "Headline: Sizing at Reference Anode (MMO) and 150 m Cable Run",
        headline_df,
        subtitle="One row per structure x current-density demand at reference anode/cable.",
        status_col="Status",
    )
    report.add_table(
        "Full Parametric Results",
        summary_df,
        subtitle=f"All {total_cases} ICCP design cases.",
        status_col="Status",
    )

    report.add_live_mode_teaser(analysis_type="ICCP rectifier and ground-bed sizing")

    report.add_assumptions([
        "ICCP physics reused verbatim from digitalmodel.cathodic_protection.iccp_design "
        "(rectifier_sizing, anode_bed_design, cable_sizing) — the demo is a sweep/report layer",
        "Total current demand = surface area x design current density; demand profiles "
        "(0.02 / 0.06 / 0.11 A/m2) bracket well-coated mean to bare-steel final per DNV-RP-B401",
        f"Anode design life = {DESIGN_LIFE_YEARS:g} years; bed resistance via the Dwight equation "
        "with a parallel/interference factor (deep-well beds get a depth reduction)",
        f"Cable sized for a max round-trip voltage drop of {MAX_VOLTAGE_DROP_V:g} V over the "
        "one-way run length, copper resistivity at 20 C",
        f"Rectifier driving voltage uses a {SAFETY_FACTOR:g} safety factor and a "
        f"{STRUCTURE_COATING_RESISTANCE_OHM:g} ohm structure/return resistance plus the module's "
        "default 2 V back-EMF; rounded up to standard TR voltage/current ratings",
        "Seawater resistivity 0.25-0.30 ohm-m (warm marine immersion); soil_resistivity argument "
        "of anode_bed_design carries the seawater resistivity for the immersed beds modelled here",
        "Standard TR ratings capped at 120 V / 200 A (module's standard-size list); NO_FIT cases "
        "indicate the demand should be split across multiple rectifier units",
        "Screening-level sizing: no detailed anode placement, current distribution / attenuation "
        "modelling, reference-electrode layout, or AC-supply / transformer thermal design",
    ])

    if output_path is None:
        OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
        output_path = OUTPUT_DIR / REPORT_NAME
    else:
        output_path = Path(output_path)
        output_path.parent.mkdir(parents=True, exist_ok=True)
    html = report.build(output_path)
    print(f"  Report saved to: {output_path}")
    return html


# ---------------------------------------------------------------------------
# JSON cache
# ---------------------------------------------------------------------------

def build_metadata(results: List[Dict[str, Any]]) -> Dict[str, Any]:
    """Reviewable metadata block for the JSON cache."""
    return {
        "demo": "GTM Demo 8: ICCP Rectifier & Ground-Bed Sizing",
        "total_cases": len(results),
        "structures": [{"id": s["id"], "name": s["name"],
                        "surface_area_m2": s["surface_area_m2"]} for s in STRUCTURES],
        "current_densities_a_per_m2": [cd["j_a_per_m2"] for cd in CURRENT_DENSITIES],
        "anode_options": [{"id": a["id"], "material": a["material"].value,
                           "bed_type": a["bed_type"].value} for a in ANODE_OPTIONS],
        "cable_lengths_m": CABLE_LENGTHS_M,
        "constants": {
            "design_life_years": DESIGN_LIFE_YEARS,
            "max_voltage_drop_V": MAX_VOLTAGE_DROP_V,
            "structure_coating_resistance_ohm": STRUCTURE_COATING_RESISTANCE_OHM,
            "safety_factor": SAFETY_FACTOR,
            "rating_marginal": RATING_MARGINAL,
        },
        "codes": ["NACE SP0169 (2013)", "API RP 1632 (1996)", "DNV-RP-B401 (2021)"],
        "backing_module": "digitalmodel.cathodic_protection.iccp_design",
    }


def save_json_results(
    results: List[Dict[str, Any]],
    results_path: Optional[Path] = None,
) -> Path:
    """Save results to JSON for --from-cache reuse."""
    if results_path is None:
        RESULTS_DIR.mkdir(parents=True, exist_ok=True)
        results_path = RESULTS_DIR / RESULTS_NAME
    else:
        results_path = Path(results_path)
        results_path.parent.mkdir(parents=True, exist_ok=True)
    payload = {"metadata": build_metadata(results), "results": results}
    with open(results_path, "w") as f:
        json.dump(payload, f, indent=2, default=str)
    print(f"  Results saved to: {results_path}")
    return results_path


def load_cached_results(results_path: Optional[Path] = None) -> List[Dict[str, Any]]:
    """Load cached results for --from-cache."""
    if results_path is None:
        results_path = RESULTS_DIR / RESULTS_NAME
    with open(results_path, "r") as f:
        data = json.load(f)
    return data["results"]


# ---------------------------------------------------------------------------
# Pipeline entry — importable by tests.
# ---------------------------------------------------------------------------

def run_demo(
    from_cache: bool = False,
    output_path: Optional[Path] = None,
    results_path: Optional[Path] = None,
) -> Tuple[List[Dict[str, Any]], Path]:
    """Run the demo end to end. Returns (results, report_path)."""
    if from_cache:
        print("\n[1/3] Loading cached results...")
        results = load_cached_results(results_path)
        print(f"  Loaded {len(results)} cached cases")
    else:
        print("\n[1/3] Running parametric sweep...")
        results = run_parametric_sweep()

    print("\n[2/3] Building HTML report...")
    if output_path is None:
        OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
        report_path = OUTPUT_DIR / REPORT_NAME
    else:
        report_path = Path(output_path)
    build_report(results, output_path=report_path)

    if not from_cache:
        print("\n[3/3] Saving JSON results...")
        save_json_results(results, results_path=results_path)
    else:
        print("\n[3/3] Skipping JSON save (loaded from cache)")
    return results, report_path


def main() -> None:
    parser = argparse.ArgumentParser(
        description="GTM Demo 8: ICCP Rectifier & Ground-Bed Sizing",
    )
    parser.add_argument("--from-cache", action="store_true",
                        help="Skip the sweep and reload results from the previous run")
    args = parser.parse_args()

    start = time.time()
    print("=" * 60)
    print("  GTM Demo 8: ICCP Rectifier & Ground-Bed Sizing")
    print("=" * 60)

    results, report_path = run_demo(from_cache=args.from_cache)

    # Quick status tally + headline print.
    tally: Dict[str, int] = {}
    for r in results:
        tally[r["status"]] = tally.get(r["status"], 0) + 1
    print("\n" + build_headline_df(results).to_string(index=False))

    elapsed = time.time() - start
    print(f"\n{'='*60}")
    print(f"  Total cases:   {len(results)}")
    print(f"  Status tally:  {tally}")
    print(f"  HTML report:   {report_path}")
    print(f"  Time elapsed:  {elapsed:.2f} s")
    print(f"{'='*60}")


if __name__ == "__main__":
    main()
