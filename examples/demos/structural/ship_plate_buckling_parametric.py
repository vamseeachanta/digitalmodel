#!/usr/bin/env python3
# ABOUTME: Headline demo driver: parametric ship-plate buckling sweep -> interactive HTML utility curves + JSON.
# ABOUTME: Imports the buckling sweep module once, runs DEFAULT_SHIP_PLATE_SWEEP, builds a GTM-branded report.
"""
Ship Hull Plate Buckling -- Parametric Utility Curves
=====================================================

Self-contained driver (demo_02 pattern). Runs the full-factorial plate-buckling
sweep over marine hull grades, writes API outputs (cases.csv, results.json), and
builds an interactive Plotly management report with utility curves.

Run::

    cd /mnt/local-analysis/digitalmodel
    timeout 300 .venv/bin/python examples/demos/structural/ship_plate_buckling_parametric.py
"""

from __future__ import annotations

import sys
from datetime import datetime, timezone
from pathlib import Path

import plotly.graph_objects as go

# --- Repo / report-builder imports -----------------------------------------
HERE = Path(__file__).resolve().parent
OUTPUT_DIR = HERE / "output"
sys.path.insert(0, "/mnt/local-analysis/digitalmodel/examples/demos/gtm")
from report_template import GTMReportBuilder  # noqa: E402

from digitalmodel.structural.buckling_parametric import (  # noqa: E402
    DEFAULT_SHIP_PLATE_SWEEP,
    REFERENCE_WIDTH,
    REFERENCE_LENGTH,
    REFERENCE_LOAD,
    run_sweep,
    utility_curves,
    write_outputs,
)
from digitalmodel.structural.structural_analysis.models import MARINE_GRADES  # noqa: E402
from digitalmodel.structural.structural_analysis.buckling import (  # noqa: E402
    PlateBucklingAnalyzer,
)
from digitalmodel.structural.structural_analysis.models import PlateGeometry  # noqa: E402

# Grade -> brand colour for consistent chart series.
GRADE_COLORS = {
    "Grade A": "#2c5282",  # blue
    "AH36": "#ed8936",     # orange
    "EH40": "#38a169",     # green
}
GREEN, AMBER, RED = "#38a169", "#d69e2e", "#e53e3e"


def crossing_thickness(thk, util, threshold=1.0):
    """Linearly interpolate the thickness where util crosses `threshold`
    (descending util as thickness rises). Returns None if never crossed."""
    for i in range(1, len(thk)):
        u0, u1 = util[i - 1], util[i]
        if (u0 - threshold) * (u1 - threshold) <= 0 and u0 != u1:
            frac = (u0 - threshold) / (u0 - u1)
            return thk[i - 1] + frac * (thk[i] - thk[i - 1])
    return None


def chart_util_vs_thickness(curves) -> go.Figure:
    """(1) Headline: buckling utilisation vs thickness, one line per grade."""
    fig = go.Figure()
    ref = curves["reference"]
    for grade, s in curves["grades"].items():
        fig.add_trace(
            go.Scatter(
                x=s["thickness_mm"],
                y=s["utilization"],
                mode="lines+markers",
                name=f"{grade} (fy={s['fy']:.0f})",
                line=dict(color=GRADE_COLORS.get(grade), width=3),
                marker=dict(size=6),
                hovertemplate=f"{grade}<br>t=%{{x}} mm<br>util=%{{y:.3f}}<extra></extra>",
            )
        )
    # Safe region shading (util <= 1.0) + unity line.
    fig.add_hrect(y0=0, y1=1.0, fillcolor=GREEN, opacity=0.06, line_width=0)
    fig.add_hline(
        y=1.0, line=dict(color=RED, width=2, dash="dash"),
        annotation_text="Unity (util = 1.0) — capacity limit",
        annotation_position="top right",
    )
    fig.add_annotation(
        x=curves["grades"][list(curves["grades"])[0]]["thickness_mm"][-1],
        y=0.45, text="SAFE REGION", showarrow=False,
        font=dict(color=GREEN, size=13), opacity=0.8,
    )
    fig.update_layout(
        xaxis_title="Plate thickness t (mm)",
        yaxis_title="Buckling utilisation (—)",
        legend=dict(orientation="h", yanchor="bottom", y=1.02, x=0),
    )
    fig.update_yaxes(rangemode="tozero")
    return fig


def chart_crit_vs_thickness(curves) -> go.Figure:
    """(2) Critical buckling stress vs thickness, one line per grade."""
    fig = go.Figure()
    for grade, s in curves["grades"].items():
        fig.add_trace(
            go.Scatter(
                x=s["thickness_mm"],
                y=s["critical_stress_mpa"],
                mode="lines+markers",
                name=grade,
                line=dict(color=GRADE_COLORS.get(grade), width=3),
                marker=dict(size=6),
                hovertemplate=f"{grade}<br>t=%{{x}} mm<br>sigma_cr=%{{y:.0f}} MPa<extra></extra>",
            )
        )
    fig.update_layout(
        xaxis_title="Plate thickness t (mm)",
        yaxis_title="Critical buckling stress sigma_cr (MPa)",
        legend=dict(orientation="h", yanchor="bottom", y=1.02, x=0),
    )
    return fig


def chart_util_vs_beta(curves) -> go.Figure:
    """(3) Utilisation vs plate slenderness beta, coloured by grade."""
    fig = go.Figure()
    for grade, s in curves["grades"].items():
        # marker colour by util zone
        zone = [
            GREEN if u <= 0.7 else (AMBER if u <= 0.9 else RED)
            for u in s["utilization"]
        ]
        fig.add_trace(
            go.Scatter(
                x=s["slenderness_beta"],
                y=s["utilization"],
                mode="lines+markers",
                name=grade,
                line=dict(color=GRADE_COLORS.get(grade), width=2),
                marker=dict(size=9, color=zone, line=dict(width=1, color="white")),
                hovertemplate=f"{grade}<br>beta=%{{x:.2f}}<br>util=%{{y:.3f}}<extra></extra>",
            )
        )
    fig.add_hline(y=1.0, line=dict(color=RED, width=2, dash="dash"))
    fig.add_hline(y=0.7, line=dict(color=AMBER, width=1, dash="dot"))
    fig.update_layout(
        xaxis_title="Plate slenderness beta = (b/t)·sqrt(fy/E)",
        yaxis_title="Buckling utilisation (—)",
        legend=dict(orientation="h", yanchor="bottom", y=1.02, x=0),
    )
    fig.update_yaxes(rangemode="tozero")
    return fig


def chart_interaction(grade="AH36", width=REFERENCE_WIDTH, length=REFERENCE_LENGTH,
                      thickness=12.0, gamma_m=1.15) -> go.Figure:
    """(4) Combined load interaction: utilisation as sigma_x and tau vary."""
    analyzer = PlateBucklingAnalyzer(MARINE_GRADES[grade])
    plate = PlateGeometry(length=length, width=width, thickness=thickness)
    sx_vals = list(range(0, 221, 20))   # MPa
    tau_vals = list(range(0, 121, 10))  # MPa
    z = []
    for tau in tau_vals:
        row = []
        for sx in sx_vals:
            res = analyzer.check_plate_buckling(
                plate, sigma_x=sx, sigma_y=0.0, tau=tau, gamma_m=gamma_m
            )
            row.append(round(res.utilization, 3))
        z.append(row)
    fig = go.Figure(
        go.Contour(
            x=sx_vals, y=tau_vals, z=z,
            colorscale=[[0, GREEN], [0.55, GREEN], [0.7, AMBER], [0.9, RED], [1, RED]],
            contours=dict(start=0, end=2.0, size=0.1, showlabels=True),
            colorbar=dict(title="util"),
            hovertemplate="sigma_x=%{x} MPa<br>tau=%{y} MPa<br>util=%{z:.2f}<extra></extra>",
        )
    )
    # Highlight the unity contour explicitly.
    fig.add_trace(
        go.Contour(
            x=sx_vals, y=tau_vals, z=z,
            contours=dict(start=1.0, end=1.0, size=0.1, coloring="lines"),
            line=dict(color="black", width=3), showscale=False,
            contours_showlabels=True, hoverinfo="skip", name="util = 1.0",
        )
    )
    fig.update_layout(
        xaxis_title="Axial stress sigma_x (MPa)",
        yaxis_title="Shear stress tau (MPa)",
    )
    return fig


def main() -> None:
    cfg = DEFAULT_SHIP_PLATE_SWEEP
    ts = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")

    rows = run_sweep(cfg)
    curves = utility_curves(cfg)
    paths = write_outputs(rows, curves, OUTPUT_DIR, gamma_m=cfg.gamma_m, timestamp=ts)

    n_cases = len(rows)
    sx, sy, tau = REFERENCE_LOAD

    # --- Management takeaway: AH36 minimum acceptable thickness --------------
    ah36 = curves["grades"]["AH36"]
    t_min = crossing_thickness(ah36["thickness_mm"], ah36["utilization"], 1.0)

    # --- Report -------------------------------------------------------------
    report = GTMReportBuilder(
        title="Ship Hull Plate Buckling — Parametric Utility Curves",
        subtitle=(
            f"Grade A / AH36 / EH40 unstiffened plate fields — {n_cases:,} cases, "
            "DNV-RP-C201"
        ),
        demo_id="ship_plate_buckling",
        case_count=n_cases,
        code_refs=["DNV-RP-C201 (2010)"],
    )

    report.add_methodology(
        """
        <p>Each case checks an unstiffened rectangular plate field for in-plane
        buckling under combined axial, transverse and shear stress, following
        <strong>DNV-RP-C201</strong> (Buckling Strength of Plated Structures).</p>
        <ul>
          <li><strong>Elastic buckling</strong>: classical plate theory,
              &sigma;<sub>E</sub> = k·&pi;²E/[12(1−&nu;²)]·(t/b)², with the
              buckling coefficient k for a simply-supported field under uniform
              compression (and the shear coefficient k<sub>&tau;</sub> for the
              shear term).</li>
          <li><strong>Johnson–Ostenfeld correction</strong>: where the elastic
              stress exceeds half the yield, the critical stress is reduced
              inelastically, &sigma;<sub>cr</sub> = f<sub>y</sub>(1 − f<sub>y</sub>/4&sigma;<sub>E</sub>).</li>
          <li><strong>Interaction check</strong>: utilisation =
              &sigma;<sub>x</sub>/(&sigma;<sub>cr</sub>/&gamma;<sub>M</sub>) +
              [&tau;/(&tau;<sub>cr</sub>/&gamma;<sub>M</sub>)]², compared against
              unity. A material factor <strong>&gamma;<sub>M</sub> = 1.15</strong>
              is applied to all capacities.</li>
          <li><strong>Materials</strong>: IACS marine grades — Grade A
              (f<sub>y</sub> 235 MPa), AH36 (355 MPa) and EH40 (390 MPa), all with
              E = 206 GPa. Toughness suffix does not change buckling capacity.</li>
        </ul>
        """,
        code_refs=["DNV-RP-C201 (2010)", "IACS UR W11 (hull steel grades)"],
    )

    report.add_assumptions(
        [
            "Simply-supported plate field on four edges (no rotational restraint credited).",
            "Uniform in-plane compression across the loaded edge; no lateral pressure.",
            "Marine grades Grade A / AH36 / EH40 with E = 206 GPa, Poisson's ratio 0.3.",
            "Effective width of attached plating equals the stiffener spacing — the "
            "stiffener itself is NOT modelled; this is unstiffened plate-field buckling only.",
            "Material factor gamma_M = 1.15; results are preliminary screening estimates "
            "and require review by a qualified engineer.",
            f"Charts reference a {REFERENCE_WIDTH:.0f} mm breadth x {REFERENCE_LENGTH:.0f} mm "
            f"span plate under sigma_x={sx:.0f}, sigma_y={sy:.0f}, tau={tau:.0f} MPa.",
        ]
    )

    # Charts
    report.add_chart(
        "util_vs_thickness", chart_util_vs_thickness(curves),
        title="Buckling utilisation vs plate thickness",
        subtitle=(
            f"Reference: {REFERENCE_WIDTH:.0f} mm breadth, axial sigma_x={sx:.0f} MPa. "
            "Below the dashed unity line the plate field is acceptable."
        ),
    )
    report.add_chart(
        "crit_vs_thickness", chart_crit_vs_thickness(curves),
        title="Critical buckling stress vs plate thickness",
        subtitle=f"Per-grade sigma_cr at {REFERENCE_WIDTH:.0f} mm breadth (DNV-RP-C201 + Johnson–Ostenfeld).",
    )
    report.add_chart(
        "util_vs_beta", chart_util_vs_beta(curves),
        title="Utilisation vs plate slenderness beta",
        subtitle="Markers coloured by zone: green util≤0.7, amber 0.7–0.9, red >0.9.",
    )
    report.add_chart(
        "load_interaction", chart_interaction(),
        title="Combined load interaction (AH36, 12 mm plate)",
        subtitle="Utilisation as axial sigma_x and shear tau vary; black line is the util=1.0 capacity boundary.",
    )

    # PASS/FAIL summary table for a representative thickness range (AH36, ref).
    import pandas as pd
    ref_rows = [
        r for r in rows
        if r["grade"] == "AH36"
        and r["width_mm"] == REFERENCE_WIDTH
        and (r["sigma_x"], r["sigma_y"], r["tau"]) == (sx, sy, tau)
    ]
    ref_rows.sort(key=lambda r: r["thickness_mm"])
    tbl = pd.DataFrame(
        [
            {
                "Thickness (mm)": r["thickness_mm"],
                "Slenderness beta": r["slenderness_beta"],
                "sigma_cr (MPa)": r["critical_stress_mpa"],
                "Utilisation": r["utilization"],
                "Status": r["status"],
            }
            for r in ref_rows
        ]
    )
    report.add_table(
        "AH36 plate-field check — representative thickness range",
        tbl,
        subtitle=(
            f"AH36 at {REFERENCE_WIDTH:.0f} mm breadth under axial sigma_x={sx:.0f} MPa. "
            f"Minimum acceptable thickness ≈ {t_min:.1f} mm (utilisation crosses unity)."
            if t_min else "AH36 reference series."
        ),
        status_col="Status",
    )

    report.add_section(
        "From parametric screening to live measured loads",
        """
        <p>This study screens a fixed grid of design stresses overnight. The same
        DNV-RP-C201 plate-buckling model can instead be driven by
        <em>measured</em> in-service loads — strain-gauge or hull-monitoring
        derived stress histories — to report utilisation against the identical
        capacity envelope shown above, turning the static utility curves into a
        continuous structural-margin readout.</p>
        """,
    )

    out_html = OUTPUT_DIR / "ship_plate_buckling_report.html"
    report.build(out_html)

    # --- Stdout summary -----------------------------------------------------
    print("=" * 70)
    print("Ship Hull Plate Buckling — parametric sweep complete")
    print("=" * 70)
    print(f"n_cases               : {n_cases}")
    print(f"grades                : {', '.join(curves['grades'].keys())}")
    print(f"cases.csv             : {paths['cases_csv']}")
    print(f"results.json          : {paths['results_json']}")
    print(f"report HTML           : {out_html}")
    print("-" * 70)
    print(
        f"MANAGEMENT TAKEAWAY — AH36 at {REFERENCE_WIDTH:.0f} mm breadth, "
        f"axial sigma_x={sx:.0f} MPa:"
    )
    if t_min is not None:
        print(
            f"  minimum acceptable plate thickness ≈ {t_min:.1f} mm "
            "(utilisation crosses 1.0)."
        )
    else:
        print("  utilisation stays below 1.0 across the full thickness range.")
    print("=" * 70)


if __name__ == "__main__":
    main()
