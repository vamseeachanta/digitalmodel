#!/usr/bin/env python3
# ABOUTME: GTM Demo 9 — CP Retrofit & Remaining-Life Assessment for Aging Offshore Structures
# ABOUTME: Parametric sweep: 3 structures x 4 ages x 4 potentials x 3 coating-breakdown = 144 cases
"""
GTM Demo 9: CP Retrofit & Remaining-Life Assessment (Aging Structures)
======================================================================

In-service decision support for aging offshore structures: given an aged
structure (years in service, original installed anode mass, measured
protection potential, present coating breakdown), this demo computes the
remaining sacrificial-anode life and the retrofit anode mass required to
extend protection to a target design life.

This is a PRESENTATION / SCREENING layer over the existing
``digitalmodel.cathodic_protection`` modules — it does NOT reimplement any
DNV-RP-B401 math. Every engineering quantity comes from:

  - ``marine_structure_cp.marine_structure_current_demand`` — present mean
    current demand of the (aged-coating) structure (DNV-RP-B401 Table 10-1).
  - ``marine_structure_cp.retrofit_assessment`` — remaining anode life,
    additional anodes/mass for the remaining design life, protection-potential
    check, retrofit recommendation.
  - ``anode_depletion.calculate_remaining_life`` — depletion %% and remaining
    life from the present anode status.
  - ``anode_depletion.generate_depletion_profile`` — year-by-year remaining
    mass / depletion projection.
  - ``anode_depletion.recommend_inspection_interval`` — inspection interval,
    type and urgency from the depletion state.

The "remaining life" is consumed-vs-capacity arithmetic over the backing
module's mean-current output: mass consumed = I_mean * t * 8760 /
(capacity * u_f); remaining usable mass drives remaining life and the
retrofit mass needed to reach the target design life.

Parametric matrix (144 cases)::

  - 3 structure types    : 4-leg jacket, monopile, subsea PLET frame
  - 4 elapsed ages       : 10, 18, 25, 32 years in service
  - 4 measured potentials: -1.05, -0.95, -0.85, -0.78 V vs Ag/AgCl
  - 3 coating-breakdown  : sound, moderate, degraded (present condition)

Produces:
  - 4 interactive Plotly charts (remaining-life vs age; depletion profile;
    retrofit-trigger map potential x age; inspection-interval heatmap)
  - Branded self-contained HTML report via GTMReportBuilder
  - JSON results file with --from-cache support

Usage::

    cd digitalmodel
    PYTHONPATH=examples/demos/gtm:src PYTHONUNBUFFERED=1 \\
        .venv/bin/python examples/demos/gtm/demo_09_cp_retrofit_remaining_life.py

    # Reuse cached results (skip the sweep, just rebuild charts + report):
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python examples/demos/gtm/demo_09_cp_retrofit_remaining_life.py --from-cache
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
except ImportError as exc:  # pragma: no cover — dependency guard
    print(f"[ERROR] Missing dependency: {exc}")
    print("        Install with: uv pip install pandas plotly")
    sys.exit(1)

# Backing engineering modules — DNV-RP-B401 CP design + depletion. NOT reimplemented here.
from digitalmodel.cathodic_protection.marine_structure_cp import (
    ClimateRegion,
    ExposureZone,
    StructuralZone,
    marine_structure_current_demand,
    retrofit_assessment,
)
from digitalmodel.cathodic_protection.anode_depletion import (
    AnodeStatus,
    calculate_remaining_life,
    generate_depletion_profile,
    recommend_inspection_interval,
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
DEMO_ID = "demo_09"
REPORT_NAME = "demo_09_cp_retrofit_remaining_life_report.html"
RESULTS_NAME = "demo_09_cp_retrofit_remaining_life_results.json"

# ---------------------------------------------------------------------------
# Anode physical constants (single Al-Zn-In stand-off anode, DNV-RP-B401)
# ---------------------------------------------------------------------------
ANODE_NET_MASS_KG = 200.0       # net mass of one (retrofit) anode [kg]
ANODE_CAPACITY_AH_KG = 2000.0   # Al-Zn-In electrochemical capacity [A-h/kg]
UTILIZATION_FACTOR = 0.90       # anode utilisation factor (stand-off geometry)
PROTECTION_THRESHOLD_V = -0.800  # DNV-RP-B401 protection criterion [V vs Ag/AgCl]

# In-service CP design context: the structures were designed for this life and
# we want to extend protection to the TARGET design life.
ORIGINAL_DESIGN_LIFE_YEARS = 30.0
TARGET_DESIGN_LIFE_YEARS = 40.0
CLIMATE = ClimateRegion.TEMPERATE

# ---------------------------------------------------------------------------
# Parametric axes
# ---------------------------------------------------------------------------
ELAPSED_AGES = [10.0, 18.0, 25.0, 32.0]  # years in service

# Measured protection potentials [V vs Ag/AgCl]. More negative = better
# protected; -0.78 V is UNDER-protected (above the -0.80 V threshold).
MEASURED_POTENTIALS = [-1.05, -0.95, -0.85, -0.78]

# Present coating-breakdown profiles for the aged structure (submerged/buried
# bare-area fractions grow as coatings degrade in service).
COATING_BREAKDOWN: Dict[str, Dict[ExposureZone, float]] = {
    "sound": {
        ExposureZone.SPLASH: 1.0,
        ExposureZone.TIDAL: 0.30,
        ExposureZone.SUBMERGED: 0.55,
        ExposureZone.BURIED_MUDLINE: 1.0,
    },
    "moderate": {
        ExposureZone.SPLASH: 1.0,
        ExposureZone.TIDAL: 0.55,
        ExposureZone.SUBMERGED: 0.80,
        ExposureZone.BURIED_MUDLINE: 1.0,
    },
    "degraded": {
        ExposureZone.SPLASH: 1.0,
        ExposureZone.TIDAL: 0.85,
        ExposureZone.SUBMERGED: 0.98,
        ExposureZone.BURIED_MUDLINE: 1.0,
    },
}

# Structure types — exposure-zone area sets [m^2] plus the ORIGINAL installed
# anode mass [kg] for the aging in-service system.
STRUCTURES: Dict[str, Dict[str, Any]] = {
    "4-leg jacket": {
        "id": "JKT-4L",
        "label": "4-Leg Steel Jacket (shallow/medium water)",
        # Sized for the original 30-yr design at the moderate-coating mean demand
        # (~30 t at I_mean ~ 206 A), so the in-service depletion story is realistic.
        "original_anode_mass_kg": 30000.0,
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
        "original_anode_mass_kg": 11000.0,
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
        "original_anode_mass_kg": 3800.0,
        "zones": [
            ("submerged", ExposureZone.SUBMERGED, 310.0),
            ("buried_mudline", ExposureZone.BURIED_MUDLINE, 60.0),
        ],
    },
}

# Reference slice used for the depletion-profile chart (one well-defined case).
REFERENCE_STRUCTURE = "4-leg jacket"
REFERENCE_AGE = 18.0
REFERENCE_POTENTIAL = -0.95
REFERENCE_COATING = "moderate"


# ---------------------------------------------------------------------------
# Build the backing-module zone list for one case
# ---------------------------------------------------------------------------

def build_zones(structure_key: str, coating_key: str) -> List[StructuralZone]:
    """Build the ``StructuralZone`` list for one structure + present coating."""
    spec = STRUCTURES[structure_key]
    profile = COATING_BREAKDOWN[coating_key]
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
# Single case runner — delegates ALL engineering to the CP modules
# ---------------------------------------------------------------------------

def run_single_case(
    structure_key: str,
    elapsed_years: float,
    measured_potential_V: float,
    coating_key: str,
) -> Dict[str, Any]:
    """Run one retrofit/remaining-life case via the backing modules."""
    spec = STRUCTURES[structure_key]
    original_mass = spec["original_anode_mass_kg"]
    zones = build_zones(structure_key, coating_key)

    # Engineering call #1: present mean current demand of the aged structure
    # (DNV-RP-B401 Table 10-1 via marine_structure_cp).
    cp = marine_structure_current_demand(
        zones=zones,
        climate_region=CLIMATE,
        design_life_years=TARGET_DESIGN_LIFE_YEARS,
        anode_net_mass_kg=ANODE_NET_MASS_KG,
        anode_capacity_Ah_kg=ANODE_CAPACITY_AH_KG,
        utilization_factor=UTILIZATION_FACTOR,
    )
    mean_current = cp.total_mean_current_A

    # Engineering call #2: retrofit assessment vs the TARGET design life
    # (remaining life, additional anodes/mass, protection-potential check).
    retro = retrofit_assessment(
        original_anode_mass_kg=original_mass,
        elapsed_years=elapsed_years,
        design_life_years=TARGET_DESIGN_LIFE_YEARS,
        mean_current_A=mean_current,
        measured_potential_V=measured_potential_V,
        anode_capacity_Ah_kg=ANODE_CAPACITY_AH_KG,
        utilization_factor=UTILIZATION_FACTOR,
        anode_net_mass_kg=ANODE_NET_MASS_KG,
        protection_threshold_V=PROTECTION_THRESHOLD_V,
    )

    # Engineering call #3: depletion % / remaining life from the present anode
    # status. current_mass is left at original so the consumption is driven by
    # the mean-current draw over the elapsed years (module takes max of the two).
    status = AnodeStatus(
        anode_id=spec["id"],
        original_mass_kg=original_mass,
        current_mass_kg=original_mass,
        elapsed_years=elapsed_years,
        mean_current_A=mean_current,
        anode_capacity_Ah_kg=ANODE_CAPACITY_AH_KG,
        utilization_factor=UTILIZATION_FACTOR,
    )
    depl = calculate_remaining_life(status)

    # Engineering call #4: year-by-year depletion projection over the target life.
    profile = generate_depletion_profile(
        original_mass_kg=original_mass,
        mean_current_A=mean_current,
        design_life_years=TARGET_DESIGN_LIFE_YEARS,
        anode_capacity_Ah_kg=ANODE_CAPACITY_AH_KG,
        utilization_factor=UTILIZATION_FACTOR,
        time_step_years=2.0,
    )

    # Engineering call #5: inspection-interval recommendation.
    insp = recommend_inspection_interval(
        depletion_result=depl,
        design_life_years=TARGET_DESIGN_LIFE_YEARS,
        elapsed_years=elapsed_years,
    )

    return {
        "structure": structure_key,
        "structure_id": spec["id"],
        "structure_label": spec["label"],
        "original_anode_mass_kg": original_mass,
        "elapsed_years": elapsed_years,
        "measured_potential_V": measured_potential_V,
        "coating_breakdown": coating_key,
        "mean_current_A": round(mean_current, 4),
        # retrofit_assessment outputs
        "remaining_life_years": retro.remaining_anode_life_years,
        "additional_anodes_needed": retro.additional_anodes_needed,
        "additional_mass_kg": retro.additional_mass_kg,
        "is_retrofit_needed": retro.is_retrofit_needed,
        "recommendation": retro.recommendation,
        # depletion outputs
        "depletion_percentage": depl.depletion_percentage,
        "mass_consumed_kg": depl.mass_consumed_kg,
        "remaining_mass_kg": depl.remaining_mass_kg,
        "is_depleted": depl.is_depleted,
        # inspection outputs
        "next_inspection_years": insp.next_inspection_years,
        "inspection_type": insp.inspection_type,
        "inspection_urgency": insp.urgency,
        # depletion profile (kept for the reference-case chart)
        "profile_years": profile.years,
        "profile_remaining_mass_kg": profile.remaining_mass_kg,
        "profile_depletion_pct": profile.depletion_percentage,
        "profile_end_of_life_year": profile.end_of_life_year,
        "standard": cp.standard,
        "target_design_life_years": TARGET_DESIGN_LIFE_YEARS,
    }


# ---------------------------------------------------------------------------
# Parametric sweep
# ---------------------------------------------------------------------------

def run_parametric_sweep() -> List[Dict[str, Any]]:
    """Run the full sweep: structures x ages x potentials x coating-breakdown."""
    total = (
        len(STRUCTURES) * len(ELAPSED_AGES) * len(MEASURED_POTENTIALS) * len(COATING_BREAKDOWN)
    )
    print(f"\n{'='*60}")
    print("  PARAMETRIC CP RETROFIT / REMAINING-LIFE SWEEP (DNV-RP-B401)")
    print(
        f"  {total} cases: {len(STRUCTURES)} structures x {len(ELAPSED_AGES)} ages"
        f" x {len(MEASURED_POTENTIALS)} potentials x {len(COATING_BREAKDOWN)} coatings"
    )
    print(f"{'='*60}\n")

    all_results: List[Dict[str, Any]] = []
    case_num = 0
    for structure_key in STRUCTURES:
        for age in ELAPSED_AGES:
            for potential in MEASURED_POTENTIALS:
                for coating in COATING_BREAKDOWN:
                    case_num += 1
                    try:
                        res = run_single_case(structure_key, age, potential, coating)
                        all_results.append(res)
                        if case_num % 24 == 0 or case_num == total:
                            print(
                                f"  Case {case_num:3d}/{total} | "
                                f"{structure_key:>17s} | age={age:>4.0f}yr | "
                                f"E={potential:+.2f}V {coating:>9s} | "
                                f"rem_life={res['remaining_life_years']:>6.1f}yr "
                                f"add_anodes={res['additional_anodes_needed']:>3d}"
                            )
                    except Exception as exc:  # pragma: no cover — defensive
                        logger.warning("Case %d failed: %s", case_num, exc)
                        all_results.append({
                            "structure": structure_key,
                            "elapsed_years": age,
                            "measured_potential_V": potential,
                            "coating_breakdown": coating,
                            "error": str(exc),
                        })

    print(f"\n  Sweep complete: {len(all_results)} results collected")
    return all_results


# ---------------------------------------------------------------------------
# Summary DataFrame
# ---------------------------------------------------------------------------

def build_summary_table(all_results: List[Dict[str, Any]]) -> pd.DataFrame:
    """Summary at the reference potential + coating, one row per structure x age."""
    rows = []
    for r in all_results:
        if "error" in r:
            continue
        if (
            r["measured_potential_V"] == REFERENCE_POTENTIAL
            and r["coating_breakdown"] == REFERENCE_COATING
        ):
            rows.append({
                "Structure": r["structure"],
                "Age (yr)": int(r["elapsed_years"]),
                "Coating": r["coating_breakdown"],
                "Potential (V)": r["measured_potential_V"],
                "Depletion (%)": round(r["depletion_percentage"], 1),
                "Remaining Life (yr)": round(r["remaining_life_years"], 1),
                "Retrofit Anodes": r["additional_anodes_needed"],
                "Retrofit Mass (kg)": round(r["additional_mass_kg"], 0),
                "Action": "RETROFIT" if r["is_retrofit_needed"] else "monitor",
            })
    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# Chart 1 (HERO): remaining anode life vs age (one line per structure)
# ---------------------------------------------------------------------------

def build_chart_1_remaining_life_vs_age(all_results: List[Dict[str, Any]]) -> go.Figure:
    """X: elapsed age (yr), Y: remaining anode life (yr). One line per structure.

    Reference potential + coating. A dashed line marks the remaining design
    life needed to reach the target (target - age).
    """
    print("\n[Chart 1] Building remaining-life vs age...")
    fig = go.Figure()
    for idx, structure_key in enumerate(STRUCTURES):
        ages, lives = [], []
        for age in ELAPSED_AGES:
            match = [
                r for r in all_results
                if "error" not in r
                and r["structure"] == structure_key
                and r["elapsed_years"] == age
                and r["measured_potential_V"] == REFERENCE_POTENTIAL
                and r["coating_breakdown"] == REFERENCE_COATING
            ]
            if match:
                ages.append(age)
                lives.append(match[0]["remaining_life_years"])
        fig.add_trace(go.Scatter(
            x=ages, y=lives, mode="lines+markers", name=structure_key,
            line=dict(color=CHART_PALETTE[idx % len(CHART_PALETTE)], width=3),
            marker=dict(size=10),
            hovertemplate=f"{structure_key}<br>Age: %{{x}} yr<br>Remaining life: %{{y:.1f}} yr<extra></extra>",
        ))

    # Remaining design-life-to-target reference line.
    fig.add_trace(go.Scatter(
        x=ELAPSED_AGES,
        y=[TARGET_DESIGN_LIFE_YEARS - a for a in ELAPSED_AGES],
        mode="lines", name=f"Remaining to {int(TARGET_DESIGN_LIFE_YEARS)}-yr target",
        line=dict(color=COLORS["text_muted"], width=2, dash="dash"),
        hovertemplate="Age: %{x} yr<br>Needed: %{y:.0f} yr<extra></extra>",
    ))

    fig.update_layout(
        title=dict(
            text=f"Remaining Anode Life vs Age in Service "
            f"({REFERENCE_COATING} coating, {REFERENCE_POTENTIAL:+.2f} V)",
            font=dict(size=18),
        ),
        xaxis_title="Years in Service",
        yaxis_title="Remaining Anode Life (years)",
        height=500,
    )
    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 2: depletion profile over time (reference case)
# ---------------------------------------------------------------------------

def build_chart_2_depletion_profile(all_results: List[Dict[str, Any]]) -> go.Figure:
    """Year-by-year remaining mass + depletion %% for the reference case."""
    print("\n[Chart 2] Building depletion profile...")
    match = [
        r for r in all_results
        if "error" not in r
        and r["structure"] == REFERENCE_STRUCTURE
        and r["elapsed_years"] == REFERENCE_AGE
        and r["measured_potential_V"] == REFERENCE_POTENTIAL
        and r["coating_breakdown"] == REFERENCE_COATING
    ]
    fig = go.Figure()
    if match:
        r = match[0]
        years = r["profile_years"]
        fig.add_trace(go.Scatter(
            x=years, y=r["profile_remaining_mass_kg"], mode="lines+markers",
            name="Remaining mass (kg)",
            line=dict(color=CHART_PALETTE[0], width=3), marker=dict(size=7),
            hovertemplate="Year %{x}<br>Remaining: %{y:.0f} kg<extra></extra>",
        ))
        fig.add_trace(go.Scatter(
            x=years, y=r["profile_depletion_pct"], mode="lines",
            name="Depletion (%)", yaxis="y2",
            line=dict(color=CHART_PALETTE[1], width=2, dash="dot"),
            hovertemplate="Year %{x}<br>Depletion: %{y:.0f}%<extra></extra>",
        ))
        # Mark the present age and end-of-life.
        fig.add_vline(
            x=REFERENCE_AGE, line=dict(color=COLORS["text_muted"], dash="dash"),
            annotation_text=f"now ({int(REFERENCE_AGE)} yr)",
        )
        fig.add_vline(
            x=r["profile_end_of_life_year"], line=dict(color="#c0392b", dash="dash"),
            annotation_text=f"EoL ({r['profile_end_of_life_year']:.0f} yr)",
        )
        fig.update_layout(
            yaxis2=dict(title="Depletion (%)", overlaying="y", side="right", range=[0, 100]),
        )

    fig.update_layout(
        title=dict(
            text=f"Anode Depletion Profile — {REFERENCE_STRUCTURE} "
            f"({REFERENCE_COATING}, {REFERENCE_POTENTIAL:+.2f} V)",
            font=dict(size=18),
        ),
        xaxis_title="Years in Service",
        yaxis_title="Remaining Anode Mass (kg)",
        height=500,
    )
    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 3: retrofit-trigger map (potential x age), color = additional mass
# ---------------------------------------------------------------------------

def build_chart_3_retrofit_trigger_map(all_results: List[Dict[str, Any]]) -> go.Figure:
    """Heatmap: measured potential (Y) x age (X), cell = retrofit mass [kg].

    Reference structure + coating; surfaces the potential/age combos that trip
    the retrofit trigger.
    """
    print("\n[Chart 3] Building retrofit-trigger map...")
    z_matrix, text_matrix = [], []
    for potential in MEASURED_POTENTIALS:
        z_row, t_row = [], []
        for age in ELAPSED_AGES:
            match = [
                r for r in all_results
                if "error" not in r
                and r["structure"] == REFERENCE_STRUCTURE
                and r["elapsed_years"] == age
                and r["measured_potential_V"] == potential
                and r["coating_breakdown"] == REFERENCE_COATING
            ]
            mass = match[0]["additional_mass_kg"] if match else 0.0
            flag = "RETROFIT" if (match and match[0]["is_retrofit_needed"]) else "ok"
            z_row.append(round(mass, 1))
            t_row.append(f"{mass:.0f} kg<br>{flag}")
        z_matrix.append(z_row)
        text_matrix.append(t_row)

    fig = go.Figure(go.Heatmap(
        z=z_matrix,
        x=[f"{int(a)} yr" for a in ELAPSED_AGES],
        y=[f"{p:+.2f} V" for p in MEASURED_POTENTIALS],
        text=text_matrix, texttemplate="%{text}", textfont=dict(size=11),
        colorscale="OrRd",
        colorbar=dict(title="Retrofit<br>mass (kg)"),
        hovertemplate="Age: %{x}<br>Potential: %{y}<br>Retrofit mass: %{z:.0f} kg<extra></extra>",
    ))
    fig.update_layout(
        title=dict(
            text=f"Retrofit-Trigger Map — {REFERENCE_STRUCTURE} ({REFERENCE_COATING} coating)",
            font=dict(size=18),
        ),
        xaxis_title="Years in Service",
        yaxis_title="Measured Potential (V vs Ag/AgCl)",
        height=460,
    )
    print("  Done")
    return fig


# ---------------------------------------------------------------------------
# Chart 4: inspection-interval heatmap (structure x age)
# ---------------------------------------------------------------------------

def build_chart_4_inspection_heatmap(all_results: List[Dict[str, Any]]) -> go.Figure:
    """Heatmap: structure (Y) x age (X), cell = next-inspection interval [yr].

    Reference potential + coating; annotated with the urgency level.
    """
    print("\n[Chart 4] Building inspection-interval heatmap...")
    structures = list(STRUCTURES.keys())
    z_matrix, text_matrix = [], []
    for structure_key in structures:
        z_row, t_row = [], []
        for age in ELAPSED_AGES:
            match = [
                r for r in all_results
                if "error" not in r
                and r["structure"] == structure_key
                and r["elapsed_years"] == age
                and r["measured_potential_V"] == REFERENCE_POTENTIAL
                and r["coating_breakdown"] == REFERENCE_COATING
            ]
            interval = match[0]["next_inspection_years"] if match else 0.0
            urgency = match[0]["inspection_urgency"] if match else "n/a"
            z_row.append(round(interval, 1))
            t_row.append(f"{interval:.1f} yr<br>{urgency}")
        z_matrix.append(z_row)
        text_matrix.append(t_row)

    fig = go.Figure(go.Heatmap(
        z=z_matrix,
        x=[f"{int(a)} yr" for a in ELAPSED_AGES],
        y=structures,
        text=text_matrix, texttemplate="%{text}", textfont=dict(size=11),
        # Reverse so short interval (urgent) is hot.
        colorscale="RdYlGn",
        colorbar=dict(title="Next<br>inspection (yr)"),
        hovertemplate="Structure: %{y}<br>Age: %{x}<br>Next inspection: %{z:.1f} yr<extra></extra>",
    ))
    fig.update_layout(
        title=dict(
            text=f"Recommended Inspection Interval "
            f"({REFERENCE_COATING} coating, {REFERENCE_POTENTIAL:+.2f} V)",
            font=dict(size=18),
        ),
        xaxis_title="Years in Service",
        yaxis_title="Structure",
        height=440,
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
        title="CP Retrofit & Remaining-Life Assessment — Aging Offshore Structures",
        subtitle=(
            f"{total_cases} parametric in-service cases across 3 structure types, "
            "4 ages, 4 measured potentials, and 3 coating-breakdown conditions"
        ),
        demo_id=DEMO_ID,
        case_count=total_cases,
        code_refs=[
            "DNV-RP-B401 (2017) — Cathodic Protection Design (current densities Table 10-1; anode sizing/retrofit §7.7, §10.8)",
            "ISO 15589-2 (2004) — Cathodic Protection of Pipeline Systems, anode mass requirements §8.5",
            "NACE SP0176 — Corrosion Control of Submerged Areas of Offshore Steel Structures",
        ],
    )

    methodology_html = f"""
    <p>This analysis provides <strong>in-service decision support</strong> for
    aging offshore structures: given years in service, original installed anode
    mass, the most recent measured protection potential, and present
    coating-breakdown condition, it estimates the <strong>remaining anode
    life</strong> and the <strong>retrofit anode mass</strong> required to extend
    protection to a {int(TARGET_DESIGN_LIFE_YEARS)}-year target design life.
    Every engineering quantity is computed by the <strong>digitalmodel
    <code>cathodic_protection</code></strong> modules implementing
    <strong>DNV-RP-B401</strong> — this report is the screening / presentation
    layer over those modules.</p>

    <h3>Present Current Demand</h3>
    <p>The aged structure's present mean current demand is computed by
    <code>marine_structure_cp.marine_structure_current_demand</code> from the
    DNV-RP-B401 Table 10-1 design current densities, scaled by each exposure
    zone's surface area and its <em>present</em> coating-breakdown factor.</p>

    <h3>Remaining Life (consumed-vs-capacity)</h3>
    <p>Mass consumed to date is
    M<sub>consumed</sub> = I<sub>mean</sub> &times; t &times; 8760 &divide;
    (capacity &times; utilisation); the remaining usable mass drives the
    remaining anode life,
    life<sub>rem</sub> = M<sub>rem</sub> &times; capacity &times; utilisation
    &divide; (I<sub>mean</sub> &times; 8760), with capacity =
    {ANODE_CAPACITY_AH_KG:.0f} A&middot;h/kg (Al-Zn-In), utilisation =
    {UTILIZATION_FACTOR:.2f}. This is computed by
    <code>anode_depletion.calculate_remaining_life</code> and
    <code>marine_structure_cp.retrofit_assessment</code>.</p>

    <h3>Retrofit Sizing &amp; Triggers</h3>
    <p><code>retrofit_assessment</code> compares remaining anode life against
    the remaining design life to the target and sizes the additional anode mass
    for any shortfall, and flags an <em>urgent</em> retrofit when the measured
    potential is above the protection threshold
    ({PROTECTION_THRESHOLD_V:+.3f} V vs Ag/AgCl).</p>

    <h3>Depletion Profile &amp; Inspection</h3>
    <p><code>anode_depletion.generate_depletion_profile</code> projects remaining
    mass year-by-year to end-of-life, and
    <code>recommend_inspection_interval</code> sets the inspection interval, type
    and urgency from the depletion percentage.</p>
    """
    report.add_methodology(methodology_html)

    report.add_chart(
        "remaining_life_vs_age",
        fig1,
        title="Chart 1: Remaining Anode Life vs Age in Service",
        subtitle=(
            "Remaining anode life falls as the structure ages; where it drops "
            f"below the dashed remaining-to-{int(TARGET_DESIGN_LIFE_YEARS)}-yr line, a retrofit is needed."
        ),
    )
    report.add_chart(
        "depletion_profile",
        fig2,
        title="Chart 2: Anode Depletion Profile",
        subtitle="Year-by-year remaining mass and depletion percentage for the reference aged jacket, with present age and end-of-life marked.",
    )
    report.add_chart(
        "retrofit_trigger_map",
        fig3,
        title="Chart 3: Retrofit-Trigger Map (Potential x Age)",
        subtitle="Retrofit anode mass by measured potential and age. Under-protected potentials (above the threshold) trip an urgent retrofit at any age.",
    )
    report.add_chart(
        "inspection_heatmap",
        fig4,
        title="Chart 4: Recommended Inspection Interval",
        subtitle="Inspection interval and urgency tighten as depletion advances with age.",
    )

    report.add_table(
        f"Summary: Retrofit Assessment at {REFERENCE_POTENTIAL:+.2f} V, {REFERENCE_COATING} coating",
        summary_df,
        subtitle="Depletion, remaining life, retrofit anodes/mass, and the recommended action per structure and age.",
    )

    report.add_live_mode_teaser(analysis_type="this CP retrofit & remaining-life screening")

    report.add_assumptions([
        f"All engineering computed by digitalmodel.cathodic_protection ({standard}); this demo "
        f"does not reimplement DNV-RP-B401 math",
        "Present mean current demand from marine_structure_cp.marine_structure_current_demand "
        "(DNV-RP-B401 Table 10-1 densities x area x present coating-breakdown factor)",
        f"Remaining anode life and retrofit sizing from retrofit_assessment + "
        f"anode_depletion.calculate_remaining_life (consumed-vs-capacity over the "
        f"{int(TARGET_DESIGN_LIFE_YEARS)}-yr target design life)",
        f"Original installed anode mass per structure is a representative screening value "
        f"(jacket {STRUCTURES['4-leg jacket']['original_anode_mass_kg']:.0f} kg, "
        f"monopile {STRUCTURES['monopile']['original_anode_mass_kg']:.0f} kg, "
        f"PLET {STRUCTURES['subsea PLET frame']['original_anode_mass_kg']:.0f} kg), not a project takeoff",
        f"Anode electrochemical capacity = {ANODE_CAPACITY_AH_KG:.0f} A-h/kg (Al-Zn-In), "
        f"utilisation factor = {UTILIZATION_FACTOR:.2f}, retrofit anode net mass = {ANODE_NET_MASS_KG:.0f} kg",
        f"Protection criterion = {PROTECTION_THRESHOLD_V:+.3f} V vs Ag/AgCl; a measured potential above "
        f"this is treated as under-protected and trips an urgent retrofit",
        "Consumption is driven by the mean-current draw over the elapsed years assuming constant demand; "
        "measured remaining anode mass (where available from inspection) would override this",
        "Inspection intervals follow general industry depletion-band guidance (routine / priority / urgent / critical), "
        "not an operator-specific RBI scheme",
        "All outputs are preliminary screening estimates requiring review by a qualified CP / integrity engineer",
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
        "demo": "GTM Demo 9: CP Retrofit & Remaining-Life Assessment for Aging Structures",
        "total_cases": len(all_results),
        "structures": list(STRUCTURES.keys()),
        "climate_region": CLIMATE.value,
        "elapsed_ages_years": ELAPSED_AGES,
        "measured_potentials_V": MEASURED_POTENTIALS,
        "coating_breakdown_profiles": list(COATING_BREAKDOWN.keys()),
        "original_design_life_years": ORIGINAL_DESIGN_LIFE_YEARS,
        "target_design_life_years": TARGET_DESIGN_LIFE_YEARS,
        "reference_case": {
            "structure": REFERENCE_STRUCTURE,
            "age_years": REFERENCE_AGE,
            "potential_V": REFERENCE_POTENTIAL,
            "coating_breakdown": REFERENCE_COATING,
        },
        "anode_constants": {
            "anode_net_mass_kg": ANODE_NET_MASS_KG,
            "anode_capacity_Ah_kg": ANODE_CAPACITY_AH_KG,
            "utilization_factor": UTILIZATION_FACTOR,
            "protection_threshold_V": PROTECTION_THRESHOLD_V,
        },
        "standard": standard,
        "backing_module": "digitalmodel.cathodic_protection.marine_structure_cp+anode_depletion",
        "codes": ["DNV-RP-B401 (2017)", "ISO 15589-2 (2004)", "NACE SP0176"],
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
    """Run the full demo pipeline. Returns (all_results, report_path, results_path)."""
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
    fig1 = build_chart_1_remaining_life_vs_age(all_results)
    fig2 = build_chart_2_depletion_profile(all_results)
    fig3 = build_chart_3_retrofit_trigger_map(all_results)
    fig4 = build_chart_4_inspection_heatmap(all_results)

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
        description="GTM Demo 9: CP Retrofit & Remaining-Life Assessment for Aging Structures",
    )
    parser.add_argument(
        "--from-cache",
        action="store_true",
        help="Skip the sweep and reload results from the cached JSON",
    )
    args = parser.parse_args()

    start_time = time.time()
    print("=" * 60)
    print("  GTM Demo 9: CP Retrofit & Remaining-Life Assessment")
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
