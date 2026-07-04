#!/usr/bin/env python3
# ABOUTME: GTM Demo 10 — CAD geometry-verification (plate-buckling capacity + render, packaged)
# ABOUTME: Parametric sweep: plate geometry (a x b x t) x boundary k-factor x material; sigma_cr, usage, pass/fail
"""
GTM Demo 10: CAD Geometry-Verification — Plate-Buckling Capacity + Render
========================================================================

Story for a CAD-firm audience: *geometry -> structural check -> report*.
A parametric set of rectangular plate geometries (the kind a CAD package
exports as STEP/IGES midsurface panels) is screened for elastic plate
buckling, then rendered and reported. This packages the existing digitalmodel
plate-buckling capability (``digitalmodel.infrastructure.calculations.
plate_buckling.ElasticBucklingCalculator``, DNV-RP-C201 / classical plate
theory) behind a clean demo — it does NOT reimplement the buckling math.

Buckling capacity (classical elastic plate buckling, also the DNV-RP-C201
elastic reference stress)::

    sigma_cr = k * pi^2 * E / (12 * (1 - nu^2)) * (t / b)^2
    usage    = sigma_applied / sigma_cr        (pass if usage <= 1.0)

where ``k`` is the boundary-condition buckling coefficient (4.0 simply
supported, 7.0 clamped for uniaxial longitudinal compression), ``E`` Young's
modulus, ``nu`` Poisson's ratio, ``t`` plate thickness and ``b`` the loaded
(short) edge breadth. The base factor ``pi^2 E / (12(1-nu^2)) * (t/b)^2`` is
computed by ``ElasticBucklingCalculator.calculate_base_factor`` and the
coefficient applied by ``calculate_longitudinal_buckling_stress`` — both
straight from the backing module.

Geometry-hygiene angle (cheap, no CAD kernel required): each geometry is
sanity-checked for analysis readiness — positive dimensions, aspect ratio
``a/b`` in a sane panel range, and plate slenderness ``beta = (b/t)
sqrt(sigma_y/E)`` flagged as stocky / normal / slender.

Parametric matrix::

  - geometries  : a set of (a x b) panel footprints       (mm -> m)
  - thicknesses : representative plate gauges             (mm -> m)
  - boundaries  : simply-supported (k=4.0) / clamped (k=7.0)
  - materials   : mild steel S235, higher-strength S355, 5083 aluminium

For each case the backing module returns sigma_cr; the demo computes the
applied longitudinal stress from a per-material design utilisation target,
the usage factor, and a pass/fail verdict.

Produces:
  - 3 interactive Plotly charts (usage vs slenderness, sigma_cr vs t/b,
    pass/fail vs aspect ratio) + 1 matplotlib geometry render (PNG)
  - Branded self-contained HTML report via GTMReportBuilder
  - JSON results file with --from-cache support

Usage::

    cd digitalmodel
    PYTHONPATH=examples/demos/gtm:src PYTHONUNBUFFERED=1 \\
        .venv/bin/python examples/demos/gtm/demo_10_cad_plate_buckling_verification.py

    # Reuse cached results (skip the sweep, just rebuild charts + report):
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python examples/demos/gtm/demo_10_cad_plate_buckling_verification.py --from-cache
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
    import pandas as pd
    import plotly.graph_objects as go
except ImportError as exc:  # pragma: no cover — dependency guard
    print(f"[ERROR] Missing dependency: {exc}")
    print("        Install with: uv pip install pandas plotly")
    sys.exit(1)

# matplotlib is used only for the geometry render (PNG). Optional — the demo
# still produces charts + report without it.
try:
    import matplotlib

    matplotlib.use("Agg")  # headless render
    import matplotlib.pyplot as plt
    from mpl_toolkits.mplot3d.art3d import Poly3DCollection

    _HAVE_MPL = True
except ImportError:  # pragma: no cover — render is optional
    _HAVE_MPL = False

# Backing engineering module — classical / DNV-RP-C201 elastic plate buckling.
# NOT reimplemented here.
from digitalmodel.infrastructure.calculations.plate_buckling import (
    ElasticBucklingCalculator,
    PlateEdgeCondition,
)

try:
    from report_template import GTMReportBuilder
except ImportError:  # pragma: no cover — packaged import path fallback
    try:
        from examples.demos.gtm.report_template import GTMReportBuilder
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
DEMO_ID = "demo_10"
REPORT_NAME = "demo_10_cad_plate_buckling_verification_report.html"
RESULTS_NAME = "demo_10_cad_plate_buckling_verification_results.json"
RENDER_NAME = "demo_10_geometry_render.png"

STANDARD = "DNV-RP-C201 (2010) / classical elastic plate buckling"
BACKING_MODULE = "digitalmodel.infrastructure.calculations.plate_buckling"

# ---------------------------------------------------------------------------
# Parametric axes
# ---------------------------------------------------------------------------
# Panel footprints (a = stiffener-direction length, b = loaded short edge) [mm].
GEOMETRIES_MM: List[Tuple[str, float, float]] = [
    ("small panel",   800.0,  400.0),
    ("deck panel",   2400.0, 1200.0),
    ("bulkhead bay", 3000.0, 1000.0),
    ("wide panel",   2000.0, 1600.0),
]

# Plate gauges [mm].
THICKNESSES_MM: List[float] = [8.0, 12.0, 18.0, 25.0]

# Boundary conditions -> buckling coefficient k for uniaxial longitudinal compression.
BOUNDARIES: List[Tuple[str, PlateEdgeCondition]] = [
    ("simply-supported", PlateEdgeCondition.SIMPLY_SUPPORTED),  # k = 4.0
    ("clamped", PlateEdgeCondition.CLAMPED),                    # k = 7.0
]

# Materials: (name, E [Pa], nu, yield [Pa], design utilisation target of yield).
# The applied longitudinal stress is set to `design_target * yield` so the
# screening exercises a realistic demand independent of the buckling capacity.
MATERIALS: List[Dict[str, Any]] = [
    {"name": "Steel S235", "E": 2.10e11, "nu": 0.30, "fy": 235e6, "design_target": 0.60},
    {"name": "Steel S355", "E": 2.10e11, "nu": 0.30, "fy": 355e6, "design_target": 0.60},
    {"name": "Al 5083-H116", "E": 7.00e10, "nu": 0.33, "fy": 145e6, "design_target": 0.50},
]

# Geometry-hygiene thresholds.
ASPECT_RATIO_MAX = 6.0   # a/b above this flags an odd panel for analysis
BETA_STOCKY = 1.0        # plate slenderness beta below -> stocky (capacity-rich)
BETA_SLENDER = 2.0       # beta above -> slender (buckling-prone)


# ---------------------------------------------------------------------------
# Single-case engineering
# ---------------------------------------------------------------------------
def classify_slenderness(beta: float) -> str:
    """Classify plate slenderness beta = (b/t) sqrt(fy/E)."""
    if beta < BETA_STOCKY:
        return "stocky"
    if beta > BETA_SLENDER:
        return "slender"
    return "normal"


def geometry_hygiene(a_m: float, b_m: float, t_m: float, beta: float) -> Dict[str, Any]:
    """Cheap analysis-readiness check (no CAD kernel needed)."""
    flags: List[str] = []
    ok = True
    if not (a_m > 0 and b_m > 0 and t_m > 0):
        flags.append("non-positive dimension")
        ok = False
    aspect = a_m / b_m if b_m > 0 else float("inf")
    if aspect < 1.0:
        flags.append("a < b (axes swapped?)")
    if aspect > ASPECT_RATIO_MAX:
        flags.append(f"aspect ratio {aspect:.1f} > {ASPECT_RATIO_MAX:.0f}")
    if t_m > 0 and b_m / t_m > 200:
        flags.append("very thin plate (b/t > 200)")
    return {
        "watertight_ok": ok,            # representative: positive closed footprint
        "aspect_ratio": aspect,
        "slenderness_beta": beta,
        "slenderness_class": classify_slenderness(beta),
        "flags": flags,
    }


def run_single_case(
    geometry_name: str,
    a_mm: float,
    b_mm: float,
    t_mm: float,
    boundary_name: str,
    edge_condition: PlateEdgeCondition,
    material: Dict[str, Any],
) -> Dict[str, Any]:
    """Verify one plate geometry for elastic buckling. Returns a result dict.

    sigma_cr is computed by the backing ElasticBucklingCalculator
    (DNV-RP-C201 / classical plate theory). The applied stress is
    design_target * fy. Usage = sigma_applied / sigma_cr.
    """
    a_m = a_mm / 1000.0
    b_m = b_mm / 1000.0
    t_m = t_mm / 1000.0

    calc = ElasticBucklingCalculator()
    # sigma_cr = k * pi^2 E / (12(1-nu^2)) * (t/b)^2  — from the backing module.
    sigma_cr = calc.calculate_longitudinal_buckling_stress(
        youngs_modulus=material["E"],
        poisson_ratio=material["nu"],
        thickness=t_m,
        breadth=b_m,
        length=a_m,
        edge_condition=edge_condition,
    )
    k_factor = (
        calc.coefficients.k_xx_clamped
        if edge_condition == PlateEdgeCondition.CLAMPED
        else calc.coefficients.k_xx_simple
    )

    sigma_applied = material["design_target"] * material["fy"]
    usage = sigma_applied / sigma_cr if sigma_cr > 0 else float("inf")
    passed = usage <= 1.0

    beta = (b_m / t_m) * math.sqrt(material["fy"] / material["E"])
    hygiene = geometry_hygiene(a_m, b_m, t_m, beta)

    return {
        "case": f"{geometry_name} | t={t_mm:.0f}mm | {boundary_name} | {material['name']}",
        "geometry": geometry_name,
        "a_mm": a_mm,
        "b_mm": b_mm,
        "t_mm": t_mm,
        "aspect_ratio": hygiene["aspect_ratio"],
        "t_over_b": t_m / b_m,
        "boundary": boundary_name,
        "k_factor": k_factor,
        "material": material["name"],
        "E_Pa": material["E"],
        "nu": material["nu"],
        "fy_Pa": material["fy"],
        "sigma_applied_Pa": sigma_applied,
        "sigma_applied_MPa": sigma_applied / 1e6,
        "sigma_cr_Pa": sigma_cr,
        "sigma_cr_MPa": sigma_cr / 1e6,
        "usage": usage,
        "verdict": "PASS" if passed else "FAIL",
        "slenderness_beta": beta,
        "slenderness_class": hygiene["slenderness_class"],
        "hygiene_flags": hygiene["flags"],
        "standard": STANDARD,
    }


def run_parametric_sweep() -> List[Dict[str, Any]]:
    """Run the full parametric plate-geometry verification sweep."""
    results: List[Dict[str, Any]] = []
    for geometry_name, a_mm, b_mm in GEOMETRIES_MM:
        for t_mm in THICKNESSES_MM:
            for boundary_name, edge_condition in BOUNDARIES:
                for material in MATERIALS:
                    results.append(
                        run_single_case(
                            geometry_name,
                            a_mm,
                            b_mm,
                            t_mm,
                            boundary_name,
                            edge_condition,
                            material,
                        )
                    )
    return results


def build_summary_table(all_results: List[Dict[str, Any]]) -> pd.DataFrame:
    """Build a compact summary table (one representative material/boundary)."""
    rows = [
        {
            "Geometry": r["geometry"],
            "a x b (mm)": f"{r['a_mm']:.0f} x {r['b_mm']:.0f}",
            "t (mm)": f"{r['t_mm']:.0f}",
            "Boundary": r["boundary"],
            "Material": r["material"],
            "sigma_cr (MPa)": round(r["sigma_cr_MPa"], 1),
            "sigma_applied (MPa)": round(r["sigma_applied_MPa"], 1),
            "Usage": round(r["usage"], 3),
            "Class": r["slenderness_class"],
            "Verdict": r["verdict"],
        }
        for r in all_results
        if r["boundary"] == "simply-supported" and r["material"] == "Steel S355"
    ]
    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# Charts
# ---------------------------------------------------------------------------
def build_chart_1_usage_vs_slenderness(all_results: List[Dict[str, Any]]) -> go.Figure:
    """Usage factor vs plate slenderness beta, coloured by verdict."""
    fig = go.Figure()
    for verdict, colour in (("PASS", "#1a9850"), ("FAIL", "#d73027")):
        pts = [r for r in all_results if r["verdict"] == verdict]
        fig.add_trace(
            go.Scatter(
                x=[r["slenderness_beta"] for r in pts],
                y=[r["usage"] for r in pts],
                mode="markers",
                name=verdict,
                marker=dict(color=colour, size=7, opacity=0.75),
                text=[r["case"] for r in pts],
                hovertemplate="%{text}<br>beta=%{x:.2f}<br>usage=%{y:.2f}<extra></extra>",
            )
        )
    fig.add_hline(y=1.0, line_dash="dash", line_color="#555",
                  annotation_text="usage = 1.0 (capacity limit)")
    fig.update_layout(
        title="Usage Factor vs Plate Slenderness",
        xaxis_title="Plate slenderness  beta = (b/t) sqrt(fy/E)",
        yaxis_title="Usage = sigma_applied / sigma_cr",
        template="plotly_white",
        height=480,
    )
    return fig


def build_chart_2_sigmacr_vs_tb(all_results: List[Dict[str, Any]]) -> go.Figure:
    """sigma_cr vs thickness ratio t/b, one trace per boundary condition."""
    fig = go.Figure()
    for boundary_name, _ in BOUNDARIES:
        pts = sorted(
            (r for r in all_results
             if r["boundary"] == boundary_name and r["material"] == "Steel S355"),
            key=lambda r: r["t_over_b"],
        )
        fig.add_trace(
            go.Scatter(
                x=[r["t_over_b"] for r in pts],
                y=[r["sigma_cr_MPa"] for r in pts],
                mode="markers",
                name=f"{boundary_name} (k={pts[0]['k_factor']:.0f})" if pts else boundary_name,
                marker=dict(size=7),
                text=[r["case"] for r in pts],
                hovertemplate="%{text}<br>t/b=%{x:.4f}<br>sigma_cr=%{y:.0f} MPa<extra></extra>",
            )
        )
    fig.update_layout(
        title="Critical Buckling Stress vs Thickness Ratio (Steel S355)",
        xaxis_title="t / b",
        yaxis_title="sigma_cr (MPa)",
        template="plotly_white",
        height=480,
    )
    return fig


def build_chart_3_passfail_by_geometry(all_results: List[Dict[str, Any]]) -> go.Figure:
    """Pass/fail counts per geometry footprint."""
    geometries = [g[0] for g in GEOMETRIES_MM]
    passes = [sum(1 for r in all_results if r["geometry"] == g and r["verdict"] == "PASS")
              for g in geometries]
    fails = [sum(1 for r in all_results if r["geometry"] == g and r["verdict"] == "FAIL")
             for g in geometries]
    fig = go.Figure()
    fig.add_trace(go.Bar(x=geometries, y=passes, name="PASS", marker_color="#1a9850"))
    fig.add_trace(go.Bar(x=geometries, y=fails, name="FAIL", marker_color="#d73027"))
    fig.update_layout(
        title="Verification Outcomes by Panel Footprint",
        xaxis_title="Geometry",
        yaxis_title="Number of cases",
        barmode="stack",
        template="plotly_white",
        height=480,
    )
    return fig


def render_geometry_png(
    all_results: List[Dict[str, Any]], output_path: Path
) -> Optional[Path]:
    """Render the distinct plate footprints as a 3D geometry figure (PNG).

    This is the lightweight CAD-render stand-in (matplotlib, headless Agg) for
    the heavier Blender baseline — it visualises the imported geometry that
    feeds the structural check.
    """
    if not _HAVE_MPL:  # pragma: no cover — render optional
        logger.warning("matplotlib not available; skipping geometry render PNG")
        return None

    # One representative plate per footprint (mid thickness).
    seen: Dict[str, Dict[str, Any]] = {}
    for r in all_results:
        if r["geometry"] not in seen and abs(r["t_mm"] - 18.0) < 1e-6:
            seen[r["geometry"]] = r
    plates = list(seen.values()) or all_results[: len(GEOMETRIES_MM)]

    n = len(plates)
    fig = plt.figure(figsize=(4 * n, 4))
    for i, r in enumerate(plates):
        ax = fig.add_subplot(1, n, i + 1, projection="3d")
        a = r["a_mm"] / 1000.0
        b = r["b_mm"] / 1000.0
        t = r["t_mm"] / 1000.0
        # 8 corners of the plate box (a along x, b along y, t along z).
        verts = [
            [0, 0, 0], [a, 0, 0], [a, b, 0], [0, b, 0],
            [0, 0, t], [a, 0, t], [a, b, t], [0, b, t],
        ]
        faces = [
            [verts[0], verts[1], verts[2], verts[3]],
            [verts[4], verts[5], verts[6], verts[7]],
            [verts[0], verts[1], verts[5], verts[4]],
            [verts[2], verts[3], verts[7], verts[6]],
            [verts[1], verts[2], verts[6], verts[5]],
            [verts[0], verts[3], verts[7], verts[4]],
        ]
        colour = "#1a9850" if r["verdict"] == "PASS" else "#d73027"
        ax.add_collection3d(
            Poly3DCollection(faces, facecolors=colour, edgecolors="#222",
                             linewidths=0.6, alpha=0.55)
        )
        ax.set_xlim(0, max(a, b))
        ax.set_ylim(0, max(a, b))
        ax.set_zlim(0, max(a, b))
        ax.set_box_aspect((a, b, max(a, b) * 0.15))
        ax.set_title(
            f"{r['geometry']}\n{r['a_mm']:.0f}x{r['b_mm']:.0f}x{r['t_mm']:.0f}mm "
            f"({r['slenderness_class']}, {r['verdict']})",
            fontsize=9,
        )
        ax.set_xlabel("a (m)", fontsize=8)
        ax.set_ylabel("b (m)", fontsize=8)
        ax.tick_params(labelsize=7)
    fig.suptitle(
        "CAD geometry-verification — imported plate footprints (t=18mm, S355 SS)",
        fontsize=11,
    )
    output_path.parent.mkdir(parents=True, exist_ok=True)
    fig.tight_layout(rect=(0, 0, 1, 0.95))
    fig.savefig(output_path, dpi=110)
    plt.close(fig)
    return output_path


# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------
def build_report(
    fig1: go.Figure,
    fig2: go.Figure,
    fig3: go.Figure,
    summary_df: pd.DataFrame,
    all_results: List[Dict[str, Any]],
    total_cases: int,
    render_path: Optional[Path],
    output_path: Optional[Path] = None,
) -> str:
    """Build the branded HTML report mirroring the GTM demo style."""
    print("\n[Report] Building HTML report...")

    n_pass = sum(1 for r in all_results if r["verdict"] == "PASS")
    n_fail = total_cases - n_pass

    report = GTMReportBuilder(
        title="CAD Geometry-Verification — Plate-Buckling Capacity",
        subtitle=(
            f"{total_cases} parametric plate-geometry cases verified for elastic "
            "buckling across panel footprints, thicknesses, boundary conditions, "
            "and materials"
        ),
        demo_id=DEMO_ID,
        case_count=total_cases,
        code_refs=[
            "DNV-RP-C201 (2010) — Buckling Strength of Plated Structures (elastic reference stress)",
            "Classical elastic plate buckling — Timoshenko & Gere, Theory of Elastic Stability",
            "Buckling coefficient k: 4.0 (simply supported), 7.0 (clamped) for uniaxial longitudinal compression",
        ],
    )

    render_html = ""
    if render_path is not None and render_path.is_file():
        render_html = (
            f'<h3>Imported Geometry Render</h3>'
            f'<p>Lightweight 3D render of the distinct plate footprints that feed '
            f'the buckling check (matplotlib, headless). Green = PASS, red = FAIL.</p>'
            f'<img src="{render_path.name}" alt="plate geometry render" '
            f'style="max-width:100%;border:1px solid #ddd;border-radius:4px;" />'
        )

    methodology_html = f"""
    <p>This demo packages the digitalmodel plate-buckling capability behind a
    CAD-firm story: <strong>geometry &rarr; structural check &rarr; report</strong>.
    A parametric set of rectangular plate panels (the midsurface geometry a CAD
    package exports as STEP/IGES) is screened for elastic plate buckling. Every
    capacity number is computed by the <strong><code>{BACKING_MODULE}</code></strong>
    module (<code>ElasticBucklingCalculator</code>) — this report is the
    verification / presentation layer and does not reimplement the buckling math.</p>

    <h3>Buckling Capacity</h3>
    <p>The critical (elastic) buckling stress for a rectangular plate under
    uniaxial longitudinal compression is
    sigma_cr = k &middot; pi&sup2; E / (12 (1 - nu&sup2;)) &middot; (t/b)&sup2;,
    where k is the boundary-condition coefficient (4.0 simply supported, 7.0
    clamped), E Young's modulus, nu Poisson's ratio, t thickness and b the
    loaded short edge. Usage = sigma_applied / sigma_cr; a panel passes when
    usage &le; 1.0.</p>

    <h3>Geometry Hygiene</h3>
    <p>Each panel is sanity-checked for analysis readiness without a CAD
    kernel: positive closed footprint, aspect ratio a/b within range, and plate
    slenderness beta = (b/t) sqrt(fy/E) classified stocky / normal / slender.</p>

    <h3>Outcome</h3>
    <p><strong>{n_pass}/{total_cases}</strong> cases PASS, <strong>{n_fail}</strong>
    FAIL — thin, slender, simply-supported panels in higher-strength steel are
    the buckling-critical ones.</p>

    {render_html}
    """
    report.add_methodology(methodology_html)

    report.add_chart(
        "usage_vs_slenderness",
        fig1,
        title="Chart 1: Usage Factor vs Plate Slenderness",
        subtitle="Usage rises with slenderness; points above the dashed line (usage>1) fail the buckling check.",
    )
    report.add_chart(
        "sigmacr_vs_tb",
        fig2,
        title="Chart 2: Critical Buckling Stress vs Thickness Ratio",
        subtitle="sigma_cr scales with (t/b)^2; clamped edges (k=7) carry ~1.75x the simply-supported (k=4) capacity.",
    )
    report.add_chart(
        "passfail_by_geometry",
        fig3,
        title="Chart 3: Verification Outcomes by Panel Footprint",
        subtitle="Pass/fail tally per panel footprint across all thicknesses, boundaries, and materials.",
    )

    report.add_table(
        "Summary: Plate Verification (Steel S355, simply-supported)",
        summary_df,
        subtitle="Critical stress, applied stress, usage, slenderness class, and verdict per geometry and thickness.",
        status_col="Verdict",
    )

    report.add_live_mode_teaser(analysis_type="this plate-buckling verification")

    report.add_assumptions([
        f"All buckling capacity computed by {BACKING_MODULE}.ElasticBucklingCalculator "
        f"({STANDARD}); this demo does not reimplement the buckling math",
        "sigma_cr = k pi^2 E / (12(1-nu^2)) (t/b)^2 for uniaxial longitudinal compression; "
        "k = 4.0 (simply supported), 7.0 (clamped) per classical plate theory",
        "Applied longitudinal stress = design-utilisation target x yield strength "
        "(0.60 fy for steel, 0.50 fy for aluminium) — a representative demand, not a project load case",
        "Elastic buckling only — no post-buckling reserve, plasticity reduction, residual stress, "
        "initial imperfection knock-down, or combined biaxial/shear interaction (those add to the full "
        "DNV-RP-C201 usage and would lower allowable stress)",
        "Geometry hygiene (watertight / aspect ratio / slenderness) is a cheap analysis-readiness screen, "
        "not a CAD-kernel STEP/IGES topology validation",
        "Plate footprints, gauges, and materials are representative screening values, not project takeoffs",
        "All outputs are preliminary screening estimates requiring review by a qualified structural engineer",
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
    n_pass = sum(1 for r in all_results if r["verdict"] == "PASS")
    return {
        "demo_id": DEMO_ID,
        "total_cases": len(all_results),
        "pass_count": n_pass,
        "fail_count": len(all_results) - n_pass,
        "standard": STANDARD,
        "backing_module": BACKING_MODULE,
        "axes": {
            "geometries": [g[0] for g in GEOMETRIES_MM],
            "thicknesses_mm": THICKNESSES_MM,
            "boundaries": [b[0] for b in BOUNDARIES],
            "materials": [m["name"] for m in MATERIALS],
        },
    }


def save_json_results(
    all_results: List[Dict[str, Any]],
    summary_df: pd.DataFrame,
    results_path: Path,
) -> Path:
    results_path.parent.mkdir(parents=True, exist_ok=True)
    json_output = {
        "metadata": build_metadata(all_results),
        "summary": summary_df.to_dict(orient="records"),
        "results": all_results,
    }
    with open(results_path, "w") as f:
        json.dump(json_output, f, indent=2, default=str)
    print(f"  JSON results saved to: {results_path}")
    return results_path


# ---------------------------------------------------------------------------
# Pipeline
# ---------------------------------------------------------------------------
def run_pipeline(
    from_cache: bool = False,
    output_path: Optional[Path] = None,
    results_path: Optional[Path] = None,
    render_path: Optional[Path] = None,
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

    report_path = (
        Path(output_path) if output_path is not None else OUTPUT_DIR / REPORT_NAME
    )
    if render_path is None:
        render_path = report_path.parent / RENDER_NAME
    else:
        render_path = Path(render_path)

    print("\n[3/5] Building charts + geometry render...")
    fig1 = build_chart_1_usage_vs_slenderness(all_results)
    fig2 = build_chart_2_sigmacr_vs_tb(all_results)
    fig3 = build_chart_3_passfail_by_geometry(all_results)
    rendered = render_geometry_png(all_results, render_path)

    print("\n[4/5] Building HTML report...")
    build_report(
        fig1, fig2, fig3, summary_df, all_results, total_cases,
        render_path=rendered, output_path=report_path,
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
        description="GTM Demo 10: CAD Geometry-Verification — Plate-Buckling Capacity + Render",
    )
    parser.add_argument(
        "--from-cache",
        action="store_true",
        help="Skip the sweep and reload results from the cached JSON",
    )
    args = parser.parse_args()

    start_time = time.time()
    print("=" * 60)
    print("  GTM Demo 10: CAD Geometry-Verification (Plate Buckling)")
    print("=" * 60)
    print("\n[1/5] Configuring sweep...")

    all_results, report_path, results_path = run_pipeline(from_cache=args.from_cache)

    n_pass = sum(1 for r in all_results if r["verdict"] == "PASS")
    elapsed = time.time() - start_time
    print("\nComplete!")
    print("=" * 60)
    print(f"  Total cases verified:  {len(all_results)}  ({n_pass} PASS / {len(all_results) - n_pass} FAIL)")
    print(f"  HTML report:           {report_path}")
    print(f"  JSON results:          {results_path}")
    print(f"  Time elapsed:          {elapsed:.1f} seconds")
    print("=" * 60)


if __name__ == "__main__":
    main()
