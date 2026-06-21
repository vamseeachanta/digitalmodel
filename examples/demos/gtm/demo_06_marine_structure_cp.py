#!/usr/bin/env python3
# ABOUTME: GTM Demo 6 — Sacrificial-Anode Cathodic Protection for Offshore Structures
# ABOUTME: Parametric sweep: 3 structures x 4 climates x 3 design lives x 4 coatings = 144 cases
"""
GTM Demo 6: Sacrificial-Anode Cathodic Protection (Jackets & Monopiles)
=======================================================================

Runs a parametric CP design sweep over offshore structures using the existing
``digitalmodel.cathodic_protection.marine_structure_cp`` module (DNV-RP-B401).
This is a PRESENTATION / SCREENING layer over that backing module — it does NOT
reimplement any DNV-RP-B401 math; every per-zone current demand, total anode
mass, anode count and per-zone anode distribution comes straight from
``marine_structure_current_demand`` and ``anode_distribution``.

Parametric matrix (144 cases)::

  - 3 structure types : 4-leg jacket, monopile, subsea PLET frame
                        (each defines its own exposure-zone area set)
  - 4 climate regions : tropical, subtropical, temperate, arctic
  - 3 design lives    : 15, 25, 40 years
  - 4 coating profiles: bare, good-coating, aged-coating, premium-coating
                        (zone coating-breakdown factors)

For each case the backing module returns:
  - per-zone current demand (initial / mean / final) [A]
  - total anode mass [kg]
  - anode count
  - per-zone anode distribution

Produces:
  - 4 interactive Plotly charts (per-zone current heatmap, anode-mass vs design
    life, climate-region sensitivity, coating-breakdown impact)
  - Branded self-contained HTML report via GTMReportBuilder
  - JSON results file with --from-cache support

Usage::

    cd digitalmodel
    PYTHONPATH=examples/demos/gtm:src PYTHONUNBUFFERED=1 \\
        .venv/bin/python examples/demos/gtm/demo_06_marine_structure_cp.py

    # Reuse cached results (skip the sweep, just rebuild charts + report):
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python examples/demos/gtm/demo_06_marine_structure_cp.py --from-cache
"""

from __future__ import annotations

import argparse
import json
import logging
import sys
import time
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

# ---------------------------------------------------------------------------
# Imports — graceful handling
# ---------------------------------------------------------------------------
try:
    import pandas as pd
    import plotly.graph_objects as go
    from plotly.subplots import make_subplots
except ImportError as exc:  # pragma: no cover — dependency guard
    print(f"[ERROR] Missing dependency: {exc}")
    print("        Install with: uv pip install pandas plotly")
    sys.exit(1)

# Backing engineering module — DNV-RP-B401 CP design. NOT reimplemented here.
from digitalmodel.cathodic_protection.marine_structure_cp import (
    ClimateRegion,
    ExposureZone,
    StructuralZone,
    anode_distribution,
    marine_structure_current_demand,
)

try:
    from report_template import COLORS, CHART_PALETTE, GTMReportBuilder
except ImportError:  # pragma: no cover — packaged import path fallback
    try:
        from examples.demos.gtm.report_template import (
            COLORS,
            CHART_PALETTE,
            GTMReportBuilder,
        )
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
# Paths
# ---------------------------------------------------------------------------
SCRIPT_DIR = Path(__file__).resolve().parent
OUTPUT_DIR = SCRIPT_DIR / "output"
RESULTS_DIR = SCRIPT_DIR / "results"
DEMO_ID = "demo_06"
REPORT_NAME = "demo_06_marine_structure_cp_report.html"
RESULTS_NAME = "demo_06_marine_structure_cp_results.json"

# ---------------------------------------------------------------------------
# Anode physical constants (single Al-Zn-In bracelet/stand-off anode, DNV-RP-B401)
# ---------------------------------------------------------------------------
ANODE_NET_MASS_KG = 200.0       # net mass of one anode [kg]
ANODE_CAPACITY_AH_KG = 2000.0   # Al-Zn-In electrochemical capacity [A-h/kg]
UTILIZATION_FACTOR = 0.90       # anode utilisation factor (stand-off geometry)

# ---------------------------------------------------------------------------
# Parametric axes
# ---------------------------------------------------------------------------
DESIGN_LIVES = [15.0, 25.0, 40.0]  # years

CLIMATE_REGIONS = [
    ClimateRegion.TROPICAL,
    ClimateRegion.SUBTROPICAL,
    ClimateRegion.TEMPERATE,
    ClimateRegion.ARCTIC,
]

# Structure types — each gives its own exposure-zone area set [m^2].
# Areas are representative screening values for the named exposure zones
# (splash zone is not CP-protected per Table 10-1, so it carries 0 demand).
STRUCTURES: Dict[str, Dict[str, Any]] = {
    "4-leg jacket": {
        "id": "JKT-4L",
        "label": "4-Leg Steel Jacket (shallow/medium water)",
        "zones": [
            ("splash", ExposureZone.SPLASH, 220.0),
            ("tidal", ExposureZone.TIDAL, 180.0),
            ("submerged", ExposureZone.SUBMERGED, 2400.0),
            ("buried_mudline", ExposureZone.BURIED_MUDLINE, 320.0),
        ],
    },
    "monopile": {
        "id": "MP-XL",
        "label": "XL Monopile (offshore wind foundation)",
        "zones": [
            ("splash", ExposureZone.SPLASH, 120.0),
            ("tidal", ExposureZone.TIDAL, 95.0),
            ("submerged", ExposureZone.SUBMERGED, 760.0),
            ("buried_mudline", ExposureZone.BURIED_MUDLINE, 540.0),
        ],
    },
    "subsea PLET frame": {
        "id": "PLET",
        "label": "Subsea PLET / Manifold Frame",
        "zones": [
            ("submerged", ExposureZone.SUBMERGED, 310.0),
            ("buried_mudline", ExposureZone.BURIED_MUDLINE, 60.0),
        ],
    },
}

# Coating breakdown profiles — coating_breakdown_factor per exposure zone.
# 0 = fully coated (module treats 0 as bare, fc=1.0, so we never pass exactly 0
# for a "coated" zone); 1.0 = fully bare. Submerged/buried zones of an offshore
# structure are frequently left bare and protected by CP alone, so their factor
# stays high even on a coated structure.
COATING_PROFILES: Dict[str, Dict[ExposureZone, float]] = {
    "bare": {
        ExposureZone.SPLASH: 1.0,
        ExposureZone.TIDAL: 1.0,
        ExposureZone.SUBMERGED: 1.0,
        ExposureZone.BURIED_MUDLINE: 1.0,
    },
    "good-coating": {
        ExposureZone.SPLASH: 1.0,
        ExposureZone.TIDAL: 0.40,
        ExposureZone.SUBMERGED: 0.90,
        ExposureZone.BURIED_MUDLINE: 1.0,
    },
    "aged-coating": {
        ExposureZone.SPLASH: 1.0,
        ExposureZone.TIDAL: 0.70,
        ExposureZone.SUBMERGED: 0.95,
        ExposureZone.BURIED_MUDLINE: 1.0,
    },
    "premium-coating": {
        ExposureZone.SPLASH: 1.0,
        ExposureZone.TIDAL: 0.15,
        ExposureZone.SUBMERGED: 0.60,
        ExposureZone.BURIED_MUDLINE: 1.0,
    },
}

# A reference slice used for the heatmap (one structure/climate/life so the
# per-zone current grid is well-defined).
REFERENCE_DESIGN_LIFE = 25.0
REFERENCE_COATING = "good-coating"


# ---------------------------------------------------------------------------
# Build the backing-module zone list for one case
# ---------------------------------------------------------------------------

def build_zones(
    structure_key: str,
    coating_profile: str,
) -> List[StructuralZone]:
    """Build the list of ``StructuralZone`` for one structure + coating profile.

    The coating-breakdown factor per zone comes from the named coating profile.
    These ``StructuralZone`` objects are fed straight into the backing module.
    """
    spec = STRUCTURES[structure_key]
    profile = COATING_PROFILES[coating_profile]
    zones: List[StructuralZone] = []
    for zone_name, exposure, area in spec["zones"]:
        zones.append(
            StructuralZone(
                zone_name=zone_name,
                exposure_zone=exposure,
                surface_area_m2=area,
                coating_breakdown_factor=profile[exposure],
            )
        )
    return zones


# ---------------------------------------------------------------------------
# Single case runner — delegates ALL engineering to marine_structure_cp
# ---------------------------------------------------------------------------

def run_single_case(
    structure_key: str,
    climate: ClimateRegion,
    design_life: float,
    coating_profile: str,
) -> Dict[str, Any]:
    """Run one CP design case via the backing module and shape the result dict."""
    spec = STRUCTURES[structure_key]
    zones = build_zones(structure_key, coating_profile)

    # Engineering call #1: current demand + anode mass + anode count (DNV-RP-B401).
    result = marine_structure_current_demand(
        zones=zones,
        climate_region=climate,
        design_life_years=design_life,
        anode_net_mass_kg=ANODE_NET_MASS_KG,
        anode_capacity_Ah_kg=ANODE_CAPACITY_AH_KG,
        utilization_factor=UTILIZATION_FACTOR,
    )

    # Engineering call #2: per-zone anode distribution (proportional to demand).
    distribution = anode_distribution(
        zones=zones,
        total_anodes=result.number_of_anodes,
        climate_region=climate,
    )

    return {
        "structure": structure_key,
        "structure_id": spec["id"],
        "structure_label": spec["label"],
        "climate_region": climate.value,
        "design_life_years": design_life,
        "coating_profile": coating_profile,
        "total_initial_current_A": result.total_initial_current_A,
        "total_mean_current_A": result.total_mean_current_A,
        "total_final_current_A": result.total_final_current_A,
        "total_anode_mass_kg": result.total_anode_mass_kg,
        "number_of_anodes": result.number_of_anodes,
        "edition_used": str(result.edition_used),
        "standard": result.standard,
        "zone_details": result.zone_details,
        "anode_distribution": distribution,
    }


# ---------------------------------------------------------------------------
# Parametric sweep
# ---------------------------------------------------------------------------

def run_parametric_sweep() -> List[Dict[str, Any]]:
    """Run the full sweep: structures x climates x design lives x coatings."""
    total = (
        len(STRUCTURES) * len(CLIMATE_REGIONS) * len(DESIGN_LIVES) * len(COATING_PROFILES)
    )
    print(f"\n{'='*60}")
    print("  PARAMETRIC SACRIFICIAL-ANODE CP SWEEP (DNV-RP-B401)")
    print(
        f"  {total} cases: {len(STRUCTURES)} structures x {len(CLIMATE_REGIONS)} climates"
        f" x {len(DESIGN_LIVES)} lives x {len(COATING_PROFILES)} coatings"
    )
    print(f"{'='*60}\n")

    all_results: List[Dict[str, Any]] = []
    case_num = 0
    for structure_key in STRUCTURES:
        for climate in CLIMATE_REGIONS:
            for design_life in DESIGN_LIVES:
                for coating in COATING_PROFILES:
                    case_num += 1
                    try:
                        res = run_single_case(structure_key, climate, design_life, coating)
                        all_results.append(res)
                        if case_num % 24 == 0 or case_num == total:
                            print(
                                f"  Case {case_num:3d}/{total} | "
                                f"{structure_key:>17s} | {climate.value:>11s} | "
                                f"life={design_life:.0f}yr {coating:>15s} | "
                                f"mass={res['total_anode_mass_kg']:>9.1f} kg "
                                f"n={res['number_of_anodes']:>3d}"
                            )
                    except Exception as exc:  # pragma: no cover — defensive
                        logger.warning("Case %d failed: %s", case_num, exc)
                        all_results.append({
                            "structure": structure_key,
                            "climate_region": climate.value,
                            "design_life_years": design_life,
                            "coating_profile": coating,
                            "error": str(exc),
                        })

    print(f"\n  Sweep complete: {len(all_results)} results collected")
    return all_results


# ---------------------------------------------------------------------------
# Summary DataFrame
# ---------------------------------------------------------------------------

def build_summary_table(all_results: List[Dict[str, Any]]) -> pd.DataFrame:
    """Summary at the reference design life + coating, one row per structure x climate."""
    rows = []
    for r in all_results:
        if "error" in r:
            continue
        if (
            r["design_life_years"] == REFERENCE_DESIGN_LIFE
            and r["coating_profile"] == REFERENCE_COATING
        ):
            rows.append({
                "Structure": r["structure"],
                "Climate": r["climate_region"],
                "Life (yr)": int(r["design_life_years"]),
                "Coating": r["coating_profile"],
                "Mean Current (A)": round(r["total_mean_current_A"], 2),
                "Anode Mass (kg)": round(r["total_anode_mass_kg"], 1),
                "Anodes": r["number_of_anodes"],
            })
    df = pd.DataFrame(rows)
    return df


# ---------------------------------------------------------------------------
# Chart 1 (HERO): per-zone current-demand heatmap
# ---------------------------------------------------------------------------

def build_chart_1_zone_heatmap(all_results: List[Dict[str, Any]]) -> go.Figure:
    """Heatmap: zones (Y) x climate region (X), cell = final current demand [A].

    Uses the reference structure (4-leg jacket) at reference life + coating so
    the per-zone grid is well-defined.
    """
    print("\n[Chart 1] Building per-zone current-demand heatmap...")
    ref_structure = "4-leg jacket"
    climates = [c.value for c in CLIMATE_REGIONS]

    # Collect zone names in a fixed order from the structure spec.
    zone_order = [z[0] for z in STRUCTURES[ref_structure]["zones"]]

    z_matrix: List[List[float]] = []
    text_matrix: List[List[str]] = []
    for zone_name in zone_order:
        z_row: List[float] = []
        t_row: List[str] = []
        for climate in climates:
            match = [
                r for r in all_results
                if "error" not in r
                and r["structure"] == ref_structure
                and r["climate_region"] == climate
                and r["design_life_years"] == REFERENCE_DESIGN_LIFE
                and r["coating_profile"] == REFERENCE_COATING
            ]
            val = 0.0
            if match:
                for zd in match[0]["zone_details"]:
                    if zd["zone_name"] == zone_name:
                        val = zd["final_current_A"]
                        break
            z_row.append(round(val, 3))
            t_row.append(f"{val:.2f} A")
        z_matrix.append(z_row)
        text_matrix.append(t_row)

    fig = go.Figure(
        go.Heatmap(
            z=z_matrix,
            x=[c.capitalize() for c in climates],
            y=[z.replace("_", " ").title() for z in zone_order],
            text=text_matrix,
            texttemplate="%{text}",
            textfont=dict(size=12),
            colorscale="YlOrRd",
            colorbar=dict(title="Final<br>Current (A)"),
            hovertemplate="Zone: %{y}<br>Climate: %{x}<br>Final current: %{z:.2f} A<extra></extra>",
        )
    )
    fig.update_layout(
        title=dict(
            text=f"Per-Zone Final Current Demand — {ref_structure} "
            f"({REFERENCE_COATING}, {int(REFERENCE_DESIGN_LIFE)} yr)",
            font=dict(size=18),
        ),
        xaxis_title="Climate Region",
        yaxis_title="Exposure Zone",
        height=450,
    )
    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 2: anode mass vs design life (one line per structure)
# ---------------------------------------------------------------------------

def build_chart_2_mass_vs_life(all_results: List[Dict[str, Any]]) -> go.Figure:
    """X: design life (yr), Y: total anode mass (kg). One line per structure type.

    Reference coating + temperate climate.
    """
    print("\n[Chart 2] Building anode mass vs design life...")
    ref_climate = ClimateRegion.TEMPERATE.value

    fig = go.Figure()
    for idx, structure_key in enumerate(STRUCTURES):
        lives = []
        masses = []
        for life in DESIGN_LIVES:
            match = [
                r for r in all_results
                if "error" not in r
                and r["structure"] == structure_key
                and r["climate_region"] == ref_climate
                and r["design_life_years"] == life
                and r["coating_profile"] == REFERENCE_COATING
            ]
            if match:
                lives.append(life)
                masses.append(match[0]["total_anode_mass_kg"])
        fig.add_trace(go.Scatter(
            x=lives,
            y=masses,
            mode="lines+markers",
            name=structure_key,
            line=dict(color=CHART_PALETTE[idx % len(CHART_PALETTE)], width=3),
            marker=dict(size=10),
            hovertemplate=f"{structure_key}<br>Life: %{{x}} yr<br>Mass: %{{y:.0f}} kg<extra></extra>",
        ))

    fig.update_layout(
        title=dict(
            text=f"Total Anode Mass vs Design Life ({ref_climate}, {REFERENCE_COATING})",
            font=dict(size=18),
        ),
        xaxis_title="Design Life (years)",
        yaxis_title="Total Anode Mass (kg)",
        height=500,
    )
    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 3: climate-region sensitivity (grouped bars, mean current)
# ---------------------------------------------------------------------------

def build_chart_3_climate_sensitivity(all_results: List[Dict[str, Any]]) -> go.Figure:
    """X: climate, Y: total mean current demand (A). Grouped bars per structure.

    Reference life + coating.
    """
    print("\n[Chart 3] Building climate-region sensitivity...")
    climates = [c.value for c in CLIMATE_REGIONS]

    fig = go.Figure()
    for idx, structure_key in enumerate(STRUCTURES):
        currents = []
        for climate in climates:
            match = [
                r for r in all_results
                if "error" not in r
                and r["structure"] == structure_key
                and r["climate_region"] == climate
                and r["design_life_years"] == REFERENCE_DESIGN_LIFE
                and r["coating_profile"] == REFERENCE_COATING
            ]
            currents.append(match[0]["total_mean_current_A"] if match else 0.0)
        fig.add_trace(go.Bar(
            x=[c.capitalize() for c in climates],
            y=currents,
            name=structure_key,
            marker_color=CHART_PALETTE[idx % len(CHART_PALETTE)],
            hovertemplate=f"{structure_key}<br>%{{x}}<br>Mean current: %{{y:.1f}} A<extra></extra>",
        ))

    fig.update_layout(
        title=dict(
            text=f"Climate-Region Sensitivity — Mean Current Demand "
            f"({REFERENCE_COATING}, {int(REFERENCE_DESIGN_LIFE)} yr)",
            font=dict(size=18),
        ),
        xaxis_title="Climate Region",
        yaxis_title="Total Mean Current Demand (A)",
        barmode="group",
        height=500,
    )
    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 4: coating-breakdown impact (grouped bars, anode mass)
# ---------------------------------------------------------------------------

def build_chart_4_coating_impact(all_results: List[Dict[str, Any]]) -> go.Figure:
    """X: coating profile, Y: total anode mass (kg). Grouped bars per structure.

    Reference life + temperate climate.
    """
    print("\n[Chart 4] Building coating-breakdown impact...")
    ref_climate = ClimateRegion.TEMPERATE.value
    coatings = list(COATING_PROFILES.keys())

    fig = go.Figure()
    for idx, structure_key in enumerate(STRUCTURES):
        masses = []
        for coating in coatings:
            match = [
                r for r in all_results
                if "error" not in r
                and r["structure"] == structure_key
                and r["climate_region"] == ref_climate
                and r["design_life_years"] == REFERENCE_DESIGN_LIFE
                and r["coating_profile"] == coating
            ]
            masses.append(match[0]["total_anode_mass_kg"] if match else 0.0)
        fig.add_trace(go.Bar(
            x=[c.replace("-", " ").title() for c in coatings],
            y=masses,
            name=structure_key,
            marker_color=CHART_PALETTE[idx % len(CHART_PALETTE)],
            hovertemplate=f"{structure_key}<br>%{{x}}<br>Anode mass: %{{y:.0f}} kg<extra></extra>",
        ))

    fig.update_layout(
        title=dict(
            text=f"Coating-Breakdown Impact on Anode Mass "
            f"({ref_climate}, {int(REFERENCE_DESIGN_LIFE)} yr)",
            font=dict(size=18),
        ),
        xaxis_title="Coating Profile",
        yaxis_title="Total Anode Mass (kg)",
        barmode="group",
        height=500,
    )
    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# HTML report
# ---------------------------------------------------------------------------

def build_report(
    fig1: go.Figure,
    fig2: go.Figure,
    fig3: go.Figure,
    fig4: go.Figure,
    summary_df: pd.DataFrame,
    all_results: List[Dict[str, Any]],
    total_cases: int,
    output_path: Optional[Path] = None,
) -> str:
    """Build the branded HTML report mirroring the GTM demo style."""
    print("\n[Report] Building HTML report...")

    standard = next(
        (r["standard"] for r in all_results if "error" not in r),
        "DNV-RP-B401 (2017)",
    )

    report = GTMReportBuilder(
        title="Sacrificial-Anode Cathodic Protection — Offshore Structures",
        subtitle=(
            f"{total_cases} parametric CP-design cases across 3 structure types, "
            "4 climate regions, 3 design lives, and 4 coating profiles"
        ),
        demo_id=DEMO_ID,
        case_count=total_cases,
        code_refs=[
            "DNV-RP-B401 (2017) — Cathodic Protection Design (current densities Table 10-1, anode mass Eq. 10.x)",
            "NACE SP0176 — Corrosion Control of Submerged Areas of Offshore Steel Structures",
            "ISO 12473 (2006) — General Principles of Cathodic Protection in Seawater",
        ],
    )

    methodology_html = f"""
    <p>This analysis screens sacrificial-anode cathodic-protection (CP) designs
    for offshore structures across a parametric matrix of structure types,
    climate regions, design lives, and coating conditions. Every engineering
    quantity is computed by the <strong>digitalmodel
    <code>marine_structure_cp</code></strong> module implementing
    <strong>DNV-RP-B401</strong> — this report is the screening / presentation
    layer over that module.</p>

    <h3>Zone Current Demand</h3>
    <p>Each structure is decomposed into exposure zones (splash, tidal,
    submerged, buried-mudline). Per-zone current demand uses the DNV-RP-B401
    Table 10-1 design current densities (initial / mean / final) for the
    selected climate region, scaled by each zone's surface area and
    coating-breakdown factor:
    I<sub>zone</sub> = area &times; f<sub>c</sub> &times; i<sub>c</sub>.
    The splash zone is not CP-protected (Table 10-1 density = 0).</p>

    <h3>Anode Mass &amp; Count</h3>
    <p>Total anode mass is sized from the mean current demand over the design
    life (DNV-RP-B401):
    M = (I<sub>mean</sub> &times; life<sub>yr</sub> &times; 8760) &divide;
    (capacity &times; utilisation), with capacity = {ANODE_CAPACITY_AH_KG:.0f}
    A&middot;h/kg (Al-Zn-In), utilisation = {UTILIZATION_FACTOR:.2f}. The anode
    count is M rounded up to whole {ANODE_NET_MASS_KG:.0f} kg anodes.</p>

    <h3>Anode Distribution</h3>
    <p>Anodes are distributed across the zones in proportion to each zone's
    fraction of the total final (design) current demand.</p>
    """
    report.add_methodology(methodology_html)

    report.add_chart(
        "zone_heatmap",
        fig1,
        title="Chart 1: Per-Zone Final Current Demand",
        subtitle=(
            "4-leg jacket at the reference coating and design life. Cells show "
            "the DNV-RP-B401 final current demand per exposure zone by climate region."
        ),
    )
    report.add_chart(
        "mass_vs_life",
        fig2,
        title="Chart 2: Anode Mass vs Design Life",
        subtitle="Total sacrificial-anode mass grows ~linearly with design life. One line per structure type.",
    )
    report.add_chart(
        "climate_sensitivity",
        fig3,
        title="Chart 3: Climate-Region Sensitivity",
        subtitle="Mean current demand rises from tropical to arctic (colder, more aggressive seawater).",
    )
    report.add_chart(
        "coating_impact",
        fig4,
        title="Chart 4: Coating-Breakdown Impact",
        subtitle="Better coatings lower the protected bare-area fraction and so the required anode mass.",
    )

    report.add_table(
        f"Summary: CP Design at {int(REFERENCE_DESIGN_LIFE)}-yr Life, {REFERENCE_COATING}",
        summary_df,
        subtitle="Mean current demand, total anode mass, and anode count per structure and climate region.",
    )

    report.add_live_mode_teaser(analysis_type="this CP design screening")

    report.add_assumptions([
        f"All engineering computed by digitalmodel.cathodic_protection.marine_structure_cp "
        f"({standard}); this demo does not reimplement DNV-RP-B401 math",
        "Per-zone design current densities from DNV-RP-B401 Table 10-1 (initial/mean/final), "
        "selected by exposure zone and climate region",
        "Splash zone is not CP-protected (Table 10-1 density = 0); corrosion there is managed by "
        "coatings + a corrosion allowance, outside the anode sizing",
        f"Single anode net mass = {ANODE_NET_MASS_KG:.0f} kg, electrochemical capacity = "
        f"{ANODE_CAPACITY_AH_KG:.0f} A-h/kg (Al-Zn-In), utilisation factor = {UTILIZATION_FACTOR:.2f}",
        "Structure exposure-zone surface areas are representative screening values for the named "
        "structure types (4-leg jacket, XL monopile, subsea PLET frame), not project-specific takeoffs",
        "Coating-breakdown factors per zone are representative profiles (bare / good / aged / premium); "
        "submerged & buried zones are commonly left bare and protected by CP alone",
        "Anode count = total mass rounded up to whole anodes; anode distribution is proportional to "
        "each zone's share of the final (design) current demand",
        "Anode resistance / individual anode current output (Dwight's equation) and electrolyte "
        "resistivity-vs-temperature effects are not included in this screening sizing",
        "All outputs are preliminary screening estimates requiring review by a qualified CP engineer",
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

def build_metadata(all_results: List[Dict[str, Any]]) -> Dict[str, Any]:
    """Reviewable metadata block for the results JSON."""
    standard = next(
        (r["standard"] for r in all_results if "error" not in r),
        "DNV-RP-B401 (2017)",
    )
    return {
        "demo": "GTM Demo 6: Sacrificial-Anode Cathodic Protection for Offshore Structures",
        "total_cases": len(all_results),
        "structures": list(STRUCTURES.keys()),
        "climate_regions": [c.value for c in CLIMATE_REGIONS],
        "design_lives_years": DESIGN_LIVES,
        "coating_profiles": list(COATING_PROFILES.keys()),
        "reference_design_life_years": REFERENCE_DESIGN_LIFE,
        "reference_coating": REFERENCE_COATING,
        "anode_constants": {
            "anode_net_mass_kg": ANODE_NET_MASS_KG,
            "anode_capacity_Ah_kg": ANODE_CAPACITY_AH_KG,
            "utilization_factor": UTILIZATION_FACTOR,
        },
        "standard": standard,
        "backing_module": "digitalmodel.cathodic_protection.marine_structure_cp",
        "codes": ["DNV-RP-B401 (2017)", "NACE SP0176", "ISO 12473 (2006)"],
    }


def save_json_results(
    all_results: List[Dict[str, Any]],
    summary_df: pd.DataFrame,
    results_path: Optional[Path] = None,
) -> Path:
    """Save results JSON for --from-cache reuse."""
    if results_path is None:
        RESULTS_DIR.mkdir(parents=True, exist_ok=True)
        results_path = RESULTS_DIR / RESULTS_NAME
    else:
        results_path = Path(results_path)
        results_path.parent.mkdir(parents=True, exist_ok=True)

    json_output = {
        "metadata": build_metadata(all_results),
        "summary": summary_df.to_dict(orient="records"),
        "results": all_results,
    }
    with open(results_path, "w") as f:
        json.dump(json_output, f, indent=2, default=str)
    print(f"  Results saved to: {results_path}")
    return results_path


# ---------------------------------------------------------------------------
# Pipeline (importable for the smoke test)
# ---------------------------------------------------------------------------

def run_pipeline(
    from_cache: bool = False,
    output_path: Optional[Path] = None,
    results_path: Optional[Path] = None,
) -> Tuple[List[Dict[str, Any]], Path, Path]:
    """Run the full demo pipeline. Returns (all_results, report_path, results_path).

    When ``from_cache`` is True the sweep is skipped and results are reloaded
    from ``results_path`` (defaults to the committed results JSON).
    """
    if results_path is None:
        RESULTS_DIR.mkdir(parents=True, exist_ok=True)
        results_path = RESULTS_DIR / RESULTS_NAME
    else:
        results_path = Path(results_path)

    if from_cache:
        print("\n[2/5] Loading cached results...")
        with open(results_path, "r") as f:
            data = json.load(f)
        all_results = data["results"]
        summary_df = pd.DataFrame(data["summary"])
        print(f"  Loaded {len(all_results)} cached results from {results_path}")
    else:
        print("\n[2/5] Running parametric sweep...")
        all_results = run_parametric_sweep()
        summary_df = build_summary_table(all_results)

    total_cases = len(all_results)

    print("\n[3/5] Building charts...")
    fig1 = build_chart_1_zone_heatmap(all_results)
    fig2 = build_chart_2_mass_vs_life(all_results)
    fig3 = build_chart_3_climate_sensitivity(all_results)
    fig4 = build_chart_4_coating_impact(all_results)

    print("\n[4/5] Building HTML report...")
    report_path = (
        Path(output_path) if output_path is not None else OUTPUT_DIR / REPORT_NAME
    )
    build_report(
        fig1, fig2, fig3, fig4, summary_df, all_results, total_cases,
        output_path=report_path,
    )

    if not from_cache:
        print("\n[5/5] Saving JSON results...")
        save_json_results(all_results, summary_df, results_path=results_path)
    else:
        print("\n[5/5] Skipping JSON save (loaded from cache)")

    return all_results, report_path, results_path


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(
        description="GTM Demo 6: Sacrificial-Anode Cathodic Protection for Offshore Structures",
    )
    parser.add_argument(
        "--from-cache",
        action="store_true",
        help="Skip the sweep and reload results from the cached JSON",
    )
    args = parser.parse_args()

    start_time = time.time()
    print("=" * 60)
    print("  GTM Demo 6: Sacrificial-Anode Cathodic Protection")
    print("=" * 60)
    print("\n[1/5] Configuring sweep...")

    all_results, report_path, results_path = run_pipeline(from_cache=args.from_cache)

    elapsed = time.time() - start_time
    print("\nComplete!")
    print("=" * 60)
    print(f"  Total cases analysed:  {len(all_results)}")
    print(f"  HTML report:           {report_path}")
    print(f"  JSON results:          {results_path}")
    print(f"  Time elapsed:          {elapsed:.1f} seconds")
    print("=" * 60)


if __name__ == "__main__":
    main()
