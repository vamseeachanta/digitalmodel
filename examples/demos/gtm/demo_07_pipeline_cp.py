#!/usr/bin/env python3
# ABOUTME: GTM Demo 7 — Subsea Pipeline Cathodic Protection (bracelet-anode design)
# ABOUTME: Parametric sweep over diameter x coating x environment x seawater resistivity
"""
GTM Demo 7: Subsea Pipeline CP — Bracelet-Anode Design
======================================================

Parametric galvanic (bracelet-anode) CP design screening for subsea
pipelines. The demo is a PRESENTATION / SWEEP layer over the existing
``digitalmodel.cathodic_protection.iso_15589_2`` module (offshore pipeline
CP per ISO 15589-2 / DNV-RP-F103) plus the DNV-RP-B401 anode-count and
protected-length helpers — it reuses that module's physics and adds a
parametric cross-product, a results table, and a branded HTML report.

Per case the demo chains the iso_15589_2 routines:

  1. ``initial_current_density``  -> temperature-dependent design current density
  2. ``coating_breakdown_factor`` -> mean (t = T/2) and final (t = T) breakdown
  3. ``pipeline_current_demand``  -> mean & final protection current demand [A]
  4. ``anode_resistance`` / ``anode_output_current`` -> per-bracelet output [A]
  5. ``anode_mass_requirement``   -> total Al-Zn-In anode mass [kg]
  6. DNV-RP-B401 ``number_of_anodes`` + bracelet spacing along the line
  7. DNV-RP-F103 ``protected_length`` envelope + ``check_protection_potential``

Parametric axes (the cross-product):
  - 6 diameters (6" -> 20" NPS) — pipeline OD
  - 3 coating systems (FBE / 3LPP / CWC) — initial/final breakdown factors
  - 3 environments (exposed seawater / partially-buried / buried) — temperature
    and design current density
  - 3 seawater/sediment resistivities

= 6 x 3 x 3 x 3 = 162 cases.

Outputs:
  - results table (pandas)
  - 4 interactive Plotly charts (falls back to static HTML note if plotly absent)
  - branded HTML report via GTMReportBuilder
  - JSON results cache with --from-cache support

Usage:
    cd digitalmodel
    PYTHONPATH=examples/demos/gtm:src python \\
        examples/demos/gtm/demo_07_pipeline_cp.py

    # Reuse cached results:
    PYTHONPATH=examples/demos/gtm:src python \\
        examples/demos/gtm/demo_07_pipeline_cp.py --from-cache
"""

from __future__ import annotations

import argparse
import json
import sys
import time
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

# ---------------------------------------------------------------------------
# Backing module — the pipeline CP physics (REUSED, not reimplemented).
# ---------------------------------------------------------------------------
from digitalmodel.cathodic_protection.iso_15589_2 import (
    ANODE_CAPACITY_ALZNI,
    ANODE_UTILIZATION_FACTOR,
    anode_mass_requirement,
    anode_output_current,
    anode_resistance,
    check_protection_potential,
    coating_breakdown_factor,
    initial_current_density,
    pipeline_current_demand,
)
from digitalmodel.cathodic_protection.dnv_rp_b401 import (
    number_of_anodes,
    protected_length,
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
DEMO_ID = "demo_07"
REPORT_NAME = "demo_07_pipeline_cp_report.html"
RESULTS_NAME = "demo_07_pipeline_cp_results.json"

# ---------------------------------------------------------------------------
# Parametric space
# ---------------------------------------------------------------------------
# Pipeline diameters — NPS label and outer diameter [m].
DIAMETERS: List[Dict[str, Any]] = [
    {"id": "D6", "nps": '6"', "od_m": 0.1683, "wt_m": 0.0091},
    {"id": "D8", "nps": '8"', "od_m": 0.2191, "wt_m": 0.0103},
    {"id": "D10", "nps": '10"', "od_m": 0.2731, "wt_m": 0.0119},
    {"id": "D12", "nps": '12"', "od_m": 0.3239, "wt_m": 0.0127},
    {"id": "D16", "nps": '16"', "od_m": 0.4064, "wt_m": 0.0143},
    {"id": "D20", "nps": '20"', "od_m": 0.5080, "wt_m": 0.0159},
]

# Coating systems — initial/final coating breakdown factors (ISO 15589-2 §8.2 /
# DNV-RP-F103 Table 4-1 style values for offshore line pipe coatings).
COATINGS: List[Dict[str, Any]] = [
    {"id": "FBE", "label": "FBE (single-layer)", "fc_i": 0.030, "fc_f": 0.150},
    {"id": "3LPP", "label": "3-layer PP", "fc_i": 0.010, "fc_f": 0.060},
    {"id": "CWC", "label": "Concrete-weight coat", "fc_i": 0.050, "fc_f": 0.200},
]

# Environments — drive seawater temperature and (via initial_current_density)
# the design current density. Partially-/fully-buried lines see warmer, lower-
# oxygen sediment so a reduced effective current density is applied.
ENVIRONMENTS: List[Dict[str, Any]] = [
    {"id": "EXP", "label": "Exposed seawater", "T_C": 8.0, "buried_factor": 1.00},
    {"id": "PART", "label": "Partially-buried", "T_C": 12.0, "buried_factor": 0.75},
    {"id": "BUR", "label": "Buried (trenched)", "T_C": 20.0, "buried_factor": 0.50},
]

# Seawater / sediment resistivity [ohm-m] — cold-deep to warm-shallow marine.
RESISTIVITIES_OHM_M: List[float] = [0.20, 0.30, 1.00]

# Design constants.
DESIGN_LIFE_YEARS = 30.0
PIPELINE_LENGTH_M = 10000.0  # 10 km design section
# Bracelet (half-shell) anode geometry — Al-Zn-In, representative subsea size.
BRACELET_LENGTH_M = 0.30  # axial length of one bracelet [m]
BRACELET_RADIUS_M = 0.12  # equivalent radius (pipe + standoff) [m]
BRACELET_NET_MASS_KG = 90.0  # net deliverable mass of one bracelet anode [kg]
# DNV-RP-F103 protected-length inputs.
STEEL_RESISTIVITY_OHM_M = 2.0e-7  # carbon-steel line pipe resistivity [ohm-m]
DELTA_E_ME_V = 0.15  # metallic voltage drop [V] (DNV-RP-F103 §5.6.3)
# Assumed in-service protection potential [V vs Ag/AgCl] for the criteria check.
ASSUMED_POTENTIAL_V = -0.950

# Utilisation flag: anode-mass margin (delivered vs required).
MASS_MARGINAL = 0.85  # delivered/required at/below this with rounding = flag


# ---------------------------------------------------------------------------
# Single-case runner — chains the iso_15589_2 / dnv_rp_b401 routines.
# ---------------------------------------------------------------------------

def run_single_case(
    diameter: Dict[str, Any],
    coating: Dict[str, Any],
    environment: Dict[str, Any],
    resistivity_ohm_m: float,
) -> Dict[str, Any]:
    """Design a bracelet-anode CP system for one parameter combination.

    All physics is reused from iso_15589_2 / dnv_rp_b401; this function only
    orchestrates the calls and classifies the outcome.
    """
    D = diameter["od_m"]
    WT = diameter["wt_m"]
    L = PIPELINE_LENGTH_M
    bf = environment["buried_factor"]

    # 1) Design current density [mA/m2], temperature-dependent, reduced for
    #    buried/partially-buried sections (lower oxygen availability).
    ic_mA_m2 = initial_current_density(environment["T_C"]) * bf

    # 2) Coating breakdown factors: mean (t = T/2) and final (t = T).
    fc_mean = coating_breakdown_factor(
        coating["fc_i"], coating["fc_f"], DESIGN_LIFE_YEARS / 2.0, DESIGN_LIFE_YEARS
    )
    fc_final = coating_breakdown_factor(
        coating["fc_i"], coating["fc_f"], DESIGN_LIFE_YEARS, DESIGN_LIFE_YEARS
    )

    # 3) Pipeline current demand [A] — mean (sizes anode mass) and final
    #    (sizes anode output / number of anodes).
    i_mean_A = pipeline_current_demand(D, L, fc_mean, ic_mA_m2)
    i_final_A = pipeline_current_demand(D, L, fc_final, ic_mA_m2)

    # 4) Bracelet anode resistance + output current.
    R_a = anode_resistance(resistivity_ohm_m, BRACELET_LENGTH_M, BRACELET_RADIUS_M)
    i_anode_A = anode_output_current(R_a)

    # 5) Total anode mass requirement [kg] (mean current over design life).
    mass_kg = anode_mass_requirement(
        i_mean_A, DESIGN_LIFE_YEARS, ANODE_CAPACITY_ALZNI, ANODE_UTILIZATION_FACTOR
    )

    # 6) Number of bracelet anodes — governed by BOTH the mass requirement and
    #    the final-current/output requirement; take the larger.
    n_by_mass = number_of_anodes(mass_kg, BRACELET_NET_MASS_KG)
    n_by_current = max(1, -(-i_final_A // i_anode_A))  # ceil division
    n_anodes = int(max(n_by_mass, n_by_current))
    spacing_m = L / n_anodes if n_anodes else L

    # 7) DNV-RP-F103 protected length per single bracelet (attenuation envelope)
    #    and the protection-potential criteria check.
    i_cm_A_m2 = (ic_mA_m2 / 1000.0) * fc_final  # mean design current density [A/m2]
    pl_m = protected_length(
        DELTA_E_ME_V, WT, D, STEEL_RESISTIVITY_OHM_M, fc_final, i_cm_A_m2
    )
    crit = check_protection_potential(ASSUMED_POTENTIAL_V)

    # Mass utilisation = required / delivered (delivered = n * net mass).
    delivered_kg = n_anodes * BRACELET_NET_MASS_KG
    mass_util = mass_kg / delivered_kg if delivered_kg else 0.0

    # Status. NO_FIT if a single bracelet cannot cover the inter-anode span
    # (protected length < spacing -> CP gaps); MARGINAL if mass margin is thin
    # or potential check fails; else OK.
    if pl_m < spacing_m or not crit["pass"]:
        status = "NO_FIT"
    elif mass_util >= MASS_MARGINAL:
        status = "MARGINAL"
    else:
        status = "OK"

    return {
        "diameter_id": diameter["id"],
        "nps": diameter["nps"],
        "od_m": D,
        "wt_m": WT,
        "coating_id": coating["id"],
        "coating_label": coating["label"],
        "fc_mean": round(fc_mean, 4),
        "fc_final": round(fc_final, 4),
        "environment_id": environment["id"],
        "environment_label": environment["label"],
        "temperature_C": environment["T_C"],
        "resistivity_ohm_m": resistivity_ohm_m,
        "current_density_mA_m2": round(ic_mA_m2, 2),
        # Current demand
        "i_mean_A": round(i_mean_A, 2),
        "i_final_A": round(i_final_A, 2),
        # Anode electricals
        "anode_resistance_ohm": round(R_a, 4),
        "anode_output_A": round(i_anode_A, 3),
        # Sizing
        "anode_mass_kg": round(mass_kg, 1),
        "n_by_mass": n_by_mass,
        "n_by_current": int(n_by_current),
        "number_of_anodes": n_anodes,
        "anode_spacing_m": round(spacing_m, 1),
        "protected_length_m": round(pl_m, 1),
        "mass_utilisation": round(mass_util, 4),
        "potential_pass": crit["pass"],
        "status": status,
    }


# ---------------------------------------------------------------------------
# Parametric sweep
# ---------------------------------------------------------------------------

def run_parametric_sweep() -> List[Dict[str, Any]]:
    """Run the full cross-product sweep over the parametric axes."""
    total = (
        len(DIAMETERS) * len(COATINGS)
        * len(ENVIRONMENTS) * len(RESISTIVITIES_OHM_M)
    )
    print(f"\n{'='*60}")
    print("  PARAMETRIC SUBSEA PIPELINE CP SWEEP (bracelet anodes)")
    print(f"  {total} cases: {len(DIAMETERS)} diameters x "
          f"{len(COATINGS)} coatings x "
          f"{len(ENVIRONMENTS)} environments x {len(RESISTIVITIES_OHM_M)} resistivities")
    print(f"{'='*60}\n")

    results: List[Dict[str, Any]] = []
    case_num = 0
    for diameter in DIAMETERS:
        for coating in COATINGS:
            for env in ENVIRONMENTS:
                for rho in RESISTIVITIES_OHM_M:
                    case_num += 1
                    r = run_single_case(diameter, coating, env, rho)
                    results.append(r)
                    if case_num % 27 == 0 or case_num == total:
                        print(f"  Case {case_num:3d}/{total} | "
                              f"{r['nps']:>4s} {r['coating_id']:>5s} | "
                              f"{r['environment_label']:>16s} | "
                              f"M={r['anode_mass_kg']:>7.0f}kg | "
                              f"n={r['number_of_anodes']:>3d} @ {r['anode_spacing_m']:.0f}m | "
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
            "NPS": r["nps"],
            "OD (m)": r["od_m"],
            "Coating": r["coating_label"],
            "Environment": r["environment_label"],
            "rho (ohm-m)": r["resistivity_ohm_m"],
            "ic (mA/m2)": r["current_density_mA_m2"],
            "I mean (A)": r["i_mean_A"],
            "I final (A)": r["i_final_A"],
            "Anode mass (kg)": r["anode_mass_kg"],
            "Anodes": r["number_of_anodes"],
            "Spacing (m)": r["anode_spacing_m"],
            "Prot. len (m)": r["protected_length_m"],
            "Status": r["status"],
        })
    return pd.DataFrame(rows)


def build_headline_df(results: List[Dict[str, Any]]) -> pd.DataFrame:
    """Compact headline: per diameter x coating at the reference env/resistivity."""
    ref_env = "EXP"
    ref_rho = 0.30
    rows = []
    for r in results:
        if r["environment_id"] == ref_env and r["resistivity_ohm_m"] == ref_rho:
            rows.append({
                "NPS": r["nps"],
                "Coating": r["coating_label"],
                "ic (mA/m2)": r["current_density_mA_m2"],
                "I mean (A)": r["i_mean_A"],
                "Anode mass (kg)": r["anode_mass_kg"],
                "Anodes": r["number_of_anodes"],
                "Spacing (m)": r["anode_spacing_m"],
                "Prot. len (m)": r["protected_length_m"],
                "Status": r["status"],
            })
    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# Charts (plotly). Each returns a go.Figure or None when plotly is unavailable.
# ---------------------------------------------------------------------------

def _ref(results, coating_id=None, env_id=None, rho=None):
    """Filter helper for chart slices."""
    out = results
    if coating_id is not None:
        out = [r for r in out if r["coating_id"] == coating_id]
    if env_id is not None:
        out = [r for r in out if r["environment_id"] == env_id]
    if rho is not None:
        out = [r for r in out if r["resistivity_ohm_m"] == rho]
    return out


def build_chart_spacing_vs_diameter(results) -> Optional["go.Figure"]:
    """Bracelet-anode spacing vs pipeline diameter, one curve per coating.

    Reference environment (exposed seawater) + resistivity.
    """
    if not HAVE_PLOTLY:
        return None
    sub = _ref(results, env_id="EXP", rho=0.30)
    fig = go.Figure()
    for i, coating in enumerate(COATINGS):
        pts = [r for r in sub if r["coating_id"] == coating["id"]]
        pts.sort(key=lambda r: r["od_m"])
        fig.add_trace(go.Scatter(
            x=[r["od_m"] for r in pts],
            y=[r["anode_spacing_m"] for r in pts],
            mode="lines+markers", name=coating["label"],
            line=dict(color=CHART_PALETTE[i % len(CHART_PALETTE)], width=3),
            marker=dict(size=9),
            hovertemplate="OD: %{x:.3f} m<br>Spacing: %{y:.0f} m<extra></extra>",
        ))
    fig.update_layout(
        title=dict(text="Bracelet-Anode Spacing vs Pipeline Diameter",
                   font=dict(size=18)),
        xaxis_title="Pipeline Outer Diameter (m)",
        yaxis_title="Anode Spacing (m)",
        height=500,
    )
    return fig


def build_chart_mass_by_coating(results) -> Optional["go.Figure"]:
    """Total anode mass by coating system, per diameter.

    Reference environment (exposed seawater) + resistivity.
    """
    if not HAVE_PLOTLY:
        return None
    sub = _ref(results, env_id="EXP", rho=0.30)
    fig = go.Figure()
    diameters = [d["nps"] for d in DIAMETERS]
    for i, coating in enumerate(COATINGS):
        ys = []
        for d in DIAMETERS:
            match = [r for r in sub if r["coating_id"] == coating["id"]
                     and r["diameter_id"] == d["id"]]
            ys.append(match[0]["anode_mass_kg"] if match else None)
        fig.add_trace(go.Bar(
            x=diameters, y=ys, name=coating["label"],
            marker_color=CHART_PALETTE[i % len(CHART_PALETTE)],
            hovertemplate="%{x}<br>Anode mass: %{y:.0f} kg<extra></extra>",
        ))
    fig.update_layout(
        title=dict(text="Total Bracelet-Anode Mass by Coating System",
                   font=dict(size=18)),
        xaxis_title="Pipeline NPS", yaxis_title="Total Anode Mass (kg)",
        barmode="group", height=500,
    )
    return fig


def build_chart_protected_length_envelope(results) -> Optional["go.Figure"]:
    """Protected-length envelope: protected length vs anode spacing, by status.

    Reference resistivity; all diameters/coatings/environments. Points above
    the y = x line are protected (PL >= spacing).
    """
    if not HAVE_PLOTLY:
        return None
    sub = _ref(results, rho=0.30)
    status_color = {"OK": COLORS["success"], "MARGINAL": COLORS["warning"],
                    "NO_FIT": COLORS["danger"]}
    fig = go.Figure()
    for status in ("OK", "MARGINAL", "NO_FIT"):
        pts = [r for r in sub if r["status"] == status]
        if not pts:
            continue
        fig.add_trace(go.Scatter(
            x=[r["anode_spacing_m"] for r in pts],
            y=[r["protected_length_m"] for r in pts],
            mode="markers", name=status,
            marker=dict(size=11, color=status_color[status],
                        line=dict(width=1, color="white")),
            text=[f"{r['nps']} {r['coating_id']} | {r['environment_label']}"
                  for r in pts],
            hovertemplate="%{text}<br>Spacing: %{x:.0f} m<br>"
                          "Prot. len: %{y:.0f} m<extra></extra>",
        ))
    # y = x reference (protected length must exceed spacing).
    spans = [r["anode_spacing_m"] for r in sub]
    if spans:
        lo, hi = min(spans), max(spans)
        fig.add_trace(go.Scatter(
            x=[lo, hi], y=[lo, hi], mode="lines",
            name="PL = spacing", line=dict(color="gray", dash="dash", width=2),
            hoverinfo="skip",
        ))
    fig.update_layout(
        title=dict(text="Protected-Length Envelope vs Anode Spacing",
                   font=dict(size=18)),
        xaxis_title="Anode Spacing (m)",
        yaxis_title="Protected Length per Bracelet (m)", height=500,
    )
    return fig


def build_chart_current_demand_sensitivity(results) -> Optional["go.Figure"]:
    """Mean current demand vs diameter, per environment (temperature/burial).

    Reference coating (FBE) + resistivity. Shows the temperature/burial
    coating-breakdown sensitivity.
    """
    if not HAVE_PLOTLY:
        return None
    sub = _ref(results, coating_id="FBE", rho=0.30)
    fig = go.Figure()
    for i, env in enumerate(ENVIRONMENTS):
        pts = [r for r in sub if r["environment_id"] == env["id"]]
        pts.sort(key=lambda r: r["od_m"])
        fig.add_trace(go.Scatter(
            x=[r["od_m"] for r in pts],
            y=[r["i_mean_A"] for r in pts],
            mode="lines+markers",
            name=f"{env['label']} ({env['T_C']:.0f} C)",
            line=dict(color=CHART_PALETTE[i % len(CHART_PALETTE)], width=3),
            marker=dict(size=9),
            hovertemplate="OD: %{x:.3f} m<br>I mean: %{y:.1f} A<extra></extra>",
        ))
    fig.update_layout(
        title=dict(text="Mean Current Demand vs Diameter (Coating/Temperature)",
                   font=dict(size=18)),
        xaxis_title="Pipeline Outer Diameter (m)",
        yaxis_title="Mean Current Demand (A)", height=500,
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
        title="Subsea Pipeline CP — Bracelet-Anode Design",
        subtitle=(
            f"{total_cases} parametric cases across 6 diameters, "
            "3 coating systems, 3 environments, and 3 seawater resistivities"
        ),
        demo_id=DEMO_ID,
        case_count=total_cases,
        code_refs=[
            "ISO 15589-2 (2004) — CP of Pipeline Systems, Part 2: Offshore Pipelines",
            "DNV-RP-F103 (2010) — CP of Submarine Pipelines by Galvanic Anodes",
            "DNV-RP-B401 (2021) — Cathodic Protection Design (anode count, protected length)",
        ],
    )

    methodology_html = f"""
    <p>This analysis screens <strong>galvanic (bracelet-anode) cathodic
    protection</strong> for subsea pipelines across a parametric matrix of
    diameters, coating systems, environments, and seawater resistivities. Each
    case chains the
    <code>digitalmodel.cathodic_protection.iso_15589_2</code> routines plus the
    DNV-RP-B401 anode-count / protected-length helpers (the demo reuses the
    module physics, it does not reimplement it):</p>

    <h3>Step 1: Design Current Density (<code>initial_current_density</code>)</h3>
    <p>Temperature-interpolated initial current density per ISO 15589-2 Table 1
    ({150:g} mA/m&sup2; warm to {200:g} mA/m&sup2; cold), reduced by a burial
    factor for partially-/fully-buried sections (lower oxygen availability).</p>

    <h3>Step 2: Coating Breakdown (<code>coating_breakdown_factor</code>)</h3>
    <p>Linear breakdown f<sub>c</sub>(t) = f<sub>ci</sub> +
    (f<sub>cf</sub> &minus; f<sub>ci</sub>)&middot;(t / T<sub>design</sub>),
    evaluated at the mean (t = T/2) and final (t = T) over a
    {DESIGN_LIFE_YEARS:g}-year design life. FBE / 3LPP / CWC carry distinct
    initial/final factors.</p>

    <h3>Step 3: Current Demand (<code>pipeline_current_demand</code>)</h3>
    <p>I<sub>c</sub> = &pi; &middot; D &middot; L &middot; f<sub>c</sub> &middot;
    i<sub>c</sub> over a {PIPELINE_LENGTH_M/1000:g} km design section. The mean
    demand sizes the anode mass; the final demand sizes the anode output and
    count.</p>

    <h3>Step 4: Bracelet Output &amp; Mass
    (<code>anode_resistance</code>, <code>anode_output_current</code>,
    <code>anode_mass_requirement</code>)</h3>
    <p>Slender-anode resistance feeds the per-bracelet output current
    (driving potential |E<sub>anode</sub> &minus; E<sub>struct</sub>|). Total
    Al-Zn-In anode mass M<sub>a</sub> = I<sub>mean</sub> &middot; T &middot; 8760
    / (&epsilon; &middot; u<sub>f</sub>), with capacity &epsilon; =
    {ANODE_CAPACITY_ALZNI:g} A&middot;h/kg and utilisation u<sub>f</sub> =
    {ANODE_UTILIZATION_FACTOR:g}.</p>

    <h3>Step 5: Anode Count &amp; Protected Length
    (<code>number_of_anodes</code>, <code>protected_length</code>)</h3>
    <p>The number of {BRACELET_NET_MASS_KG:g} kg bracelets is the larger of the
    mass-governed and final-current/output-governed counts; spacing = L / n.
    DNV-RP-F103 Eq. 14 gives the protected length per bracelet (attenuation
    envelope), and the in-service potential is checked against the
    &minus;0.80 / &minus;1.10 V (Ag/AgCl) window.</p>

    <h3>Status Classification</h3>
    <ul>
        <li><strong>OK:</strong> protected length &ge; anode spacing and the
        potential criterion passes, with anode-mass headroom (utilisation
        &lt; {MASS_MARGINAL:g})</li>
        <li><strong>MARGINAL:</strong> protected and criterion passes but
        anode-mass margin is thin (utilisation &ge; {MASS_MARGINAL:g})</li>
        <li><strong>NO_FIT:</strong> protected length &lt; spacing (CP gaps) or
        the potential criterion fails &mdash; tighten spacing / re-size</li>
    </ul>
    """
    report.add_methodology(methodology_html)

    # Charts (or a static note when plotly is unavailable).
    if HAVE_PLOTLY:
        fig1 = build_chart_spacing_vs_diameter(results)
        fig2 = build_chart_mass_by_coating(results)
        fig3 = build_chart_protected_length_envelope(results)
        fig4 = build_chart_current_demand_sensitivity(results)
        report.add_chart(
            "spacing_vs_diameter", fig1,
            title="Chart 1: Bracelet-Anode Spacing vs Pipeline Diameter",
            subtitle="Exposed seawater, 0.30 ohm-m. One curve per coating system.",
        )
        report.add_chart(
            "mass_by_coating", fig2,
            title="Chart 2: Total Anode Mass by Coating System",
            subtitle="Exposed seawater, 0.30 ohm-m. Grouped by pipeline diameter.",
        )
        report.add_chart(
            "protected_length", fig3,
            title="Chart 3: Protected-Length Envelope vs Anode Spacing",
            subtitle="0.30 ohm-m. Points above the dashed y=x line are protected.",
        )
        report.add_chart(
            "current_sensitivity", fig4,
            title="Chart 4: Mean Current Demand (Coating/Temperature Sensitivity)",
            subtitle="FBE coating, 0.30 ohm-m. One curve per environment/temperature.",
        )
    else:  # pragma: no cover - plotly-less host
        report.add_section(
            "Charts",
            "<p class='note'><em>plotly is not installed in this environment, so "
            "the interactive charts were omitted. The full numeric results are in "
            "the tables below; install plotly to regenerate the charts.</em></p>",
        )

    report.add_table(
        "Headline: Sizing at Exposed Seawater (0.30 ohm-m)",
        headline_df,
        subtitle="One row per diameter x coating at the reference environment/resistivity.",
        status_col="Status",
    )
    report.add_table(
        "Full Parametric Results",
        summary_df,
        subtitle=f"All {total_cases} pipeline CP design cases.",
        status_col="Status",
    )

    report.add_live_mode_teaser(analysis_type="subsea pipeline bracelet-anode CP design")

    report.add_assumptions([
        "Pipeline CP physics reused verbatim from "
        "digitalmodel.cathodic_protection.iso_15589_2 (current density, coating "
        "breakdown, current demand, anode resistance/output, anode mass) plus "
        "dnv_rp_b401 (number_of_anodes, protected_length) — the demo is a sweep/report layer",
        f"Galvanic Al-Zn-In bracelet anodes: capacity {ANODE_CAPACITY_ALZNI:g} A-h/kg, "
        f"utilisation {ANODE_UTILIZATION_FACTOR:g}, net mass {BRACELET_NET_MASS_KG:g} kg each "
        f"(length {BRACELET_LENGTH_M:g} m, equivalent radius {BRACELET_RADIUS_M:g} m)",
        f"Design life {DESIGN_LIFE_YEARS:g} years over a {PIPELINE_LENGTH_M/1000:g} km design "
        "section; mean demand (t = T/2) sizes mass, final demand (t = T) sizes output/count",
        "Current density temperature-interpolated per ISO 15589-2 Table 1 (150 mA/m2 warm to "
        "200 mA/m2 cold); buried/partially-buried sections apply a 0.50 / 0.75 reduction factor "
        "for reduced oxygen availability",
        "Coating breakdown factors (FBE 0.03->0.15, 3LPP 0.01->0.06, CWC 0.05->0.20) are "
        "representative ISO 15589-2 / DNV-RP-F103 initial->final values, not project-specific",
        f"Protected length per DNV-RP-F103 Eq. 14 with steel resistivity "
        f"{STEEL_RESISTIVITY_OHM_M:g} ohm-m and metallic voltage drop {DELTA_E_ME_V:g} V; "
        "anode count is the larger of mass- and final-current-governed counts",
        f"In-service potential assumed {ASSUMED_POTENTIAL_V:g} V vs Ag/AgCl for the criteria "
        "check (-0.80 to -1.10 V protection window); NO_FIT marks CP gaps or a failed criterion",
        "Screening-level sizing: no detailed bracelet placement, current attenuation along the "
        "full line, field-joint / CWC current-drain detail, or interference/stray-current modelling",
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
        "demo": "GTM Demo 7: Subsea Pipeline CP — Bracelet-Anode Design",
        "total_cases": len(results),
        "diameters": [{"id": d["id"], "nps": d["nps"], "od_m": d["od_m"]}
                      for d in DIAMETERS],
        "coatings": [{"id": c["id"], "fc_i": c["fc_i"], "fc_f": c["fc_f"]}
                     for c in COATINGS],
        "environments": [{"id": e["id"], "label": e["label"], "T_C": e["T_C"],
                          "buried_factor": e["buried_factor"]} for e in ENVIRONMENTS],
        "resistivities_ohm_m": RESISTIVITIES_OHM_M,
        "constants": {
            "design_life_years": DESIGN_LIFE_YEARS,
            "pipeline_length_m": PIPELINE_LENGTH_M,
            "bracelet_net_mass_kg": BRACELET_NET_MASS_KG,
            "bracelet_length_m": BRACELET_LENGTH_M,
            "bracelet_radius_m": BRACELET_RADIUS_M,
            "anode_capacity_Ah_per_kg": ANODE_CAPACITY_ALZNI,
            "anode_utilisation_factor": ANODE_UTILIZATION_FACTOR,
            "mass_marginal": MASS_MARGINAL,
        },
        "codes": ["ISO 15589-2 (2004)", "DNV-RP-F103 (2010)", "DNV-RP-B401 (2021)"],
        "backing_module": "digitalmodel.cathodic_protection.iso_15589_2",
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
        description="GTM Demo 7: Subsea Pipeline CP — Bracelet-Anode Design",
    )
    parser.add_argument("--from-cache", action="store_true",
                        help="Skip the sweep and reload results from the previous run")
    args = parser.parse_args()

    start = time.time()
    print("=" * 60)
    print("  GTM Demo 7: Subsea Pipeline CP — Bracelet-Anode Design")
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
