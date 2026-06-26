#!/usr/bin/env python3
# ABOUTME: Headline demo driver: parametric ship stiffened-panel buckling sweep -> interactive HTML + JSON.
# ABOUTME: Imports the panel sweep module once, runs DEFAULT_PANEL_SWEEP, builds a GTM-branded report.
"""
Ship Hull Stiffened-Panel Buckling -- Parametric Utility Curves
===============================================================

Self-contained driver. Runs the full-factorial stiffened-panel buckling sweep
(plate field + single longitudinal tee stiffener) over marine hull grades,
writes API outputs (panel_cases.csv, panel_results.json), and builds an
interactive Plotly management report with utility curves and a governing-mode
map.

Run::

    cd /mnt/local-analysis/digitalmodel
    timeout 300 .venv/bin/python examples/demos/structural/ship_panel_buckling_parametric.py
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

from digitalmodel.structural.panel_buckling_parametric import (  # noqa: E402
    DEFAULT_PANEL_SWEEP,
    REFERENCE_SPACING,
    REFERENCE_PROFILE,
    REFERENCE_LOAD,
    run_sweep,
    utility_curves,
    write_outputs,
)
from digitalmodel.structural.structural_analysis.models import MARINE_GRADES  # noqa: E402
from digitalmodel.structural.structural_analysis.panel_buckling import (  # noqa: E402
    StiffenerGeometry,
    StiffenedPanelGeometry,
    StiffenedPanelBucklingAnalyzer,
)

# Grade -> brand colour for consistent chart series.
GRADE_COLORS = {
    "Grade A": "#2c5282",  # blue
    "AH36": "#ed8936",     # orange
    "EH40": "#38a169",     # green
}
GREEN, AMBER, RED = "#38a169", "#d69e2e", "#e53e3e"

# Governing-mode -> colour for the mode map.
MODE_COLORS = {
    "plate_induced": "#2c5282",  # blue
    "column": "#805ad5",         # purple
    "torsional": "#e53e3e",      # red
}
MODE_INDEX = {"plate_induced": 0, "column": 1, "torsional": 2}
MODE_LABEL = {
    "plate_induced": "Plate-field",
    "column": "Column",
    "torsional": "Torsional (tripping)",
}


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
    """(1) Headline: panel utilisation vs plate thickness, one line per grade."""
    fig = go.Figure()
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
        yaxis_title="Panel buckling utilisation (—)",
        legend=dict(orientation="h", yanchor="bottom", y=1.02, x=0),
    )
    fig.update_yaxes(rangemode="tozero")
    return fig


def chart_governing_mode_map(cfg, grade="AH36") -> go.Figure:
    """(2) Governing-mode map: which mode governs over (thickness x spacing).

    Coloured scatter for a reference grade + profile + load. This is the
    panel-specific insight chart (plate-field vs column vs torsional tripping).
    """
    profile = next(p for p in cfg.profiles if p["name"] == REFERENCE_PROFILE)
    sigma_x, tau = REFERENCE_LOAD
    analyzer = StiffenedPanelBucklingAnalyzer(MARINE_GRADES[grade])

    fig = go.Figure()
    seen = set()
    for spacing in cfg.stiffener_spacings:
        for t in cfg.plate_thicknesses:
            stiff = StiffenerGeometry(
                web_height=profile["web_height"],
                web_thickness=profile["web_thickness"],
                flange_width=profile["flange_width"],
                flange_thickness=profile["flange_thickness"],
                spacing=spacing, section_type="tee",
            )
            panel = StiffenedPanelGeometry(
                plate_length=cfg.plate_length, plate_thickness=t, stiffener=stiff,
            )
            res = analyzer.check_panel(
                panel, sigma_x=sigma_x, sigma_y=0.0, tau=tau, gamma_m=cfg.gamma_m
            )
            mode = res.governing_mode
            fig.add_trace(
                go.Scatter(
                    x=[t], y=[spacing],
                    mode="markers",
                    marker=dict(
                        size=26, color=MODE_COLORS[mode], symbol="square",
                        line=dict(width=1, color="white"),
                    ),
                    name=MODE_LABEL[mode],
                    legendgroup=mode,
                    showlegend=mode not in seen,
                    hovertemplate=(
                        f"t=%{{x}} mm<br>spacing=%{{y}} mm<br>"
                        f"governing: {MODE_LABEL[mode]}<br>util={res.utilization:.3f}<extra></extra>"
                    ),
                )
            )
            seen.add(mode)
    fig.update_layout(
        xaxis_title="Plate thickness t (mm)",
        yaxis_title="Stiffener spacing s (mm)",
        legend=dict(orientation="h", yanchor="bottom", y=1.02, x=0, title="Governing mode"),
    )
    return fig


def chart_component_bars(cfg, grade="AH36") -> go.Figure:
    """(3) Plate / column / torsional component utilisations as grouped bars.

    A handful of representative panels at the reference spacing + load, varying
    the stiffener profile and plate thickness to show the tripping vs
    plate-field tradeoff.
    """
    sigma_x, tau = REFERENCE_LOAD
    analyzer = StiffenedPanelBucklingAnalyzer(MARINE_GRADES[grade])
    reps = [
        ("T250x90", 10.0), ("T250x90", 16.0),
        ("T400x120", 12.0), ("T400x120", 18.0),
        ("T600x200", 12.0), ("T600x200", 20.0),
    ]
    labels, plate_u, col_u, tors_u = [], [], [], []
    for pname, t in reps:
        profile = next(p for p in cfg.profiles if p["name"] == pname)
        stiff = StiffenerGeometry(
            web_height=profile["web_height"], web_thickness=profile["web_thickness"],
            flange_width=profile["flange_width"], flange_thickness=profile["flange_thickness"],
            spacing=REFERENCE_SPACING, section_type="tee",
        )
        panel = StiffenedPanelGeometry(
            plate_length=cfg.plate_length, plate_thickness=t, stiffener=stiff,
        )
        res = analyzer.check_panel(panel, sigma_x=sigma_x, sigma_y=0.0, tau=tau, gamma_m=cfg.gamma_m)
        labels.append(f"{pname}<br>t={t:.0f}mm")
        plate_u.append(round(res.plate_utilization, 3))
        col_u.append(round(res.column_utilization, 3))
        tors_u.append(round(res.stiffener_utilization, 3))

    fig = go.Figure()
    fig.add_trace(go.Bar(name="Plate-field", x=labels, y=plate_u, marker_color="#2c5282"))
    fig.add_trace(go.Bar(name="Column", x=labels, y=col_u, marker_color="#805ad5"))
    fig.add_trace(go.Bar(name="Torsional (tripping)", x=labels, y=tors_u, marker_color="#e53e3e"))
    fig.add_hline(y=1.0, line=dict(color=RED, width=2, dash="dash"),
                  annotation_text="Unity", annotation_position="top right")
    fig.update_layout(
        barmode="group",
        xaxis_title=f"Representative {grade} panels (spacing {REFERENCE_SPACING:.0f} mm)",
        yaxis_title="Component utilisation (—)",
        legend=dict(orientation="h", yanchor="bottom", y=1.02, x=0),
    )
    return fig


def chart_util_vs_spacing(cfg, grade_subset=None) -> go.Figure:
    """(4) Panel utilisation vs stiffener spacing, per grade.

    Reference profile + load + plate thickness; sweeps the stiffener spacing.
    """
    sigma_x, tau = REFERENCE_LOAD
    profile = next(p for p in cfg.profiles if p["name"] == REFERENCE_PROFILE)
    ref_t = 12.0
    grades = grade_subset or list(cfg.grades)
    fig = go.Figure()
    for grade in grades:
        analyzer = StiffenedPanelBucklingAnalyzer(MARINE_GRADES[grade])
        xs, ys = [], []
        for spacing in cfg.stiffener_spacings:
            stiff = StiffenerGeometry(
                web_height=profile["web_height"], web_thickness=profile["web_thickness"],
                flange_width=profile["flange_width"], flange_thickness=profile["flange_thickness"],
                spacing=spacing, section_type="tee",
            )
            panel = StiffenedPanelGeometry(
                plate_length=cfg.plate_length, plate_thickness=ref_t, stiffener=stiff,
            )
            res = analyzer.check_panel(panel, sigma_x=sigma_x, sigma_y=0.0, tau=tau, gamma_m=cfg.gamma_m)
            xs.append(spacing)
            ys.append(round(res.utilization, 4))
        fig.add_trace(
            go.Scatter(
                x=xs, y=ys, mode="lines+markers", name=grade,
                line=dict(color=GRADE_COLORS.get(grade), width=3), marker=dict(size=8),
                hovertemplate=f"{grade}<br>spacing=%{{x}} mm<br>util=%{{y:.3f}}<extra></extra>",
            )
        )
    fig.add_hrect(y0=0, y1=1.0, fillcolor=GREEN, opacity=0.06, line_width=0)
    fig.add_hline(y=1.0, line=dict(color=RED, width=2, dash="dash"))
    fig.update_layout(
        xaxis_title="Stiffener spacing s (mm)",
        yaxis_title=f"Panel utilisation (—) at t={ref_t:.0f} mm, {REFERENCE_PROFILE}",
        legend=dict(orientation="h", yanchor="bottom", y=1.02, x=0),
    )
    fig.update_yaxes(rangemode="tozero")
    return fig


def main() -> None:
    cfg = DEFAULT_PANEL_SWEEP
    ts = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")

    rows = run_sweep(cfg)
    curves = utility_curves(cfg)
    paths = write_outputs(rows, curves, OUTPUT_DIR, gamma_m=cfg.gamma_m, timestamp=ts)

    n_cases = len(rows)
    sx, tau = REFERENCE_LOAD

    # --- Management takeaway: AH36 minimum acceptable plate thickness --------
    ah36 = curves["grades"]["AH36"]
    t_min = crossing_thickness(ah36["thickness_mm"], ah36["utilization"], 1.0)
    # Governing mode at (or just above) that thickness.
    gov_at_min = None
    if t_min is not None:
        for t, m in zip(ah36["thickness_mm"], ah36["governing_mode"]):
            if t >= t_min:
                gov_at_min = m
                break
    if gov_at_min is None:
        gov_at_min = ah36["governing_mode"][-1]

    # --- Report -------------------------------------------------------------
    report = GTMReportBuilder(
        title="Ship Hull Stiffened-Panel Buckling — Parametric Utility Curves",
        subtitle=(
            f"Grade A / AH36 / EH40 plate + tee-stiffener panels — {n_cases:,} cases, "
            "DNV-RP-C201 (plate-field / column / torsional tripping)"
        ),
        demo_id="ship_panel_buckling",
        case_count=n_cases,
        code_refs=["DNV-RP-C201 (2010)"],
    )

    report.add_methodology(
        """
        <p>Each case screens a <strong>stiffened panel</strong> — a plate field
        reinforced by a single longitudinal tee stiffener — for buckling under
        combined axial and shear stress, following <strong>DNV-RP-C201</strong>
        (Buckling Strength of Plated Structures). Three failure modes are checked
        and the governing (most-utilised) one is reported:</p>
        <ul>
          <li><strong>Plate-field buckling</strong>: the unstiffened plate panel
              spanning <em>between</em> longitudinal stiffeners (width = stiffener
              spacing), via classical plate theory + Johnson–Ostenfeld inelastic
              correction.</li>
          <li><strong>Overall column buckling</strong>: the plate + stiffener
              acting as a built-up column over the transverse-frame span (Euler /
              flexural, EC3 buckling curve "c").</li>
          <li><strong>Stiffener torsional (tripping) buckling</strong>: lateral-
              torsional instability of the stiffener per
              <strong>DNV-RP-C201 Sec. 7.5.2</strong>, including the plate
              rotational-restraint coefficient &beta;.</li>
          <li><strong>Interaction / utilisation</strong>: each mode reports
              utilisation against unity with material factor
              <strong>&gamma;<sub>M</sub> = 1.15</strong>; the panel utilisation
              is the maximum of the three.</li>
          <li><strong>Materials</strong>: IACS marine grades — Grade A
              (f<sub>y</sub> 235 MPa), AH36 (355 MPa), EH40 (390 MPa), E = 206 GPa.</li>
        </ul>
        <p><strong>VALIDATED:</strong> the underlying solver is validated against
        the 0119-015 stiffened-panel worked example (effective area, neutral
        axis, second moment, plate stress, column slenderness, and torsional
        f<sub>ET</sub>/&lambda;<sub>T</sub>/f<sub>T</sub> all within 0.2%); see
        <code>docs/domains/plate-buckling/panel-buckling-validation-2026-06-26.md</code>.</p>
        """,
        code_refs=["DNV-RP-C201 (2010)", "DNV-RP-C201 Sec. 7.5.2 (stiffener tripping)",
                   "IACS UR W11 (hull steel grades)"],
    )

    report.add_assumptions(
        [
            "Single longitudinal stiffener with its associated plate flange modelled; "
            "girder-level (cross-stiffener) interaction is out of scope.",
            "Effective plate width taken as the full stiffener spacing (no effective-width "
            "reduction for very slender plate fields).",
            "Pure in-plane stress screening — no lateral pressure / continuous-beam bending "
            "of the stiffener.",
            "Tee stiffeners; torsional restraint spacing L_T defaults to the transverse-frame span.",
            "Marine grades Grade A / AH36 / EH40 with E = 206 GPa, Poisson's ratio 0.3.",
            "Material factor gamma_M = 1.15; results are preliminary screening estimates "
            "and require review by a qualified engineer.",
            f"Charts reference stiffener spacing {REFERENCE_SPACING:.0f} mm, profile "
            f"{REFERENCE_PROFILE}, plate span {cfg.plate_length:.0f} mm under "
            f"sigma_x={sx:.0f}, tau={tau:.0f} MPa.",
        ]
    )

    # Charts
    report.add_chart(
        "util_vs_thickness", chart_util_vs_thickness(curves),
        title="Panel utilisation vs plate thickness",
        subtitle=(
            f"Reference: spacing {REFERENCE_SPACING:.0f} mm, profile {REFERENCE_PROFILE}, "
            f"axial sigma_x={sx:.0f} MPa. Below the dashed unity line the panel is acceptable."
        ),
    )
    report.add_chart(
        "governing_mode_map", chart_governing_mode_map(cfg, grade="AH36"),
        title="Governing failure mode map (AH36)",
        subtitle=(
            "Which of the three DNV-RP-C201 modes governs over plate thickness × stiffener "
            "spacing — the panel-specific insight: plate-field (blue) vs column (purple) vs "
            "torsional tripping (red)."
        ),
    )
    report.add_chart(
        "component_bars", chart_component_bars(cfg, grade="AH36"),
        title="Component utilisations: plate-field / column / torsional",
        subtitle=(
            f"Representative AH36 panels at {REFERENCE_SPACING:.0f} mm spacing. Shows the "
            "tripping vs plate-field tradeoff as stiffener profile and plate thickness change."
        ),
    )
    report.add_chart(
        "util_vs_spacing", chart_util_vs_spacing(cfg),
        title="Panel utilisation vs stiffener spacing",
        subtitle=f"Per grade at t=12 mm, profile {REFERENCE_PROFILE}; closer stiffeners reduce utilisation.",
    )

    # PASS/FAIL summary table (AH36, reference spacing/profile/load, vs thickness).
    import pandas as pd
    ref_rows = [
        r for r in rows
        if r["grade"] == "AH36"
        and r["spacing_mm"] == REFERENCE_SPACING
        and r["profile"] == REFERENCE_PROFILE
        and (r["sigma_x"], r["tau"]) == (sx, tau)
    ]
    ref_rows.sort(key=lambda r: r["thickness_mm"])
    tbl = pd.DataFrame(
        [
            {
                "Thickness (mm)": r["thickness_mm"],
                "Plate util": r["plate_util"],
                "Column util": r["column_util"],
                "Torsional util": r["torsional_util"],
                "Panel util": r["utilization"],
                "Governing": MODE_LABEL.get(r["governing_mode"], r["governing_mode"]),
                "Status": r["status"],
            }
            for r in ref_rows
        ]
    )
    report.add_table(
        "AH36 stiffened-panel check — representative thickness range",
        tbl,
        subtitle=(
            f"AH36 at {REFERENCE_SPACING:.0f} mm spacing, {REFERENCE_PROFILE} under axial "
            f"sigma_x={sx:.0f} MPa. "
            + (
                f"Minimum acceptable plate thickness ≈ {t_min:.1f} mm "
                f"(governing mode there: {MODE_LABEL.get(gov_at_min, gov_at_min)})."
                if t_min else "Utilisation stays below unity across the thickness range."
            )
        ),
        status_col="Status",
    )

    report.add_section(
        "From parametric screening to live measured loads",
        """
        <p>This study screens a fixed grid of design stresses overnight. The same
        validated DNV-RP-C201 stiffened-panel model can instead be driven by
        <em>measured</em> in-service loads — strain-gauge or hull-monitoring
        derived stress histories — to report which of the three buckling modes
        governs against the identical capacity envelope shown above, turning the
        static utility curves into a continuous structural-margin readout.</p>
        """,
    )

    out_html = OUTPUT_DIR / "ship_panel_buckling_report.html"
    report.build(out_html)

    # --- Stdout summary -----------------------------------------------------
    print("=" * 70)
    print("Ship Hull Stiffened-Panel Buckling — parametric sweep complete")
    print("=" * 70)
    print(f"n_cases               : {n_cases}")
    print(f"grades                : {', '.join(curves['grades'].keys())}")
    print(f"governing-mode split  : {curves['mode_split']}")
    print(f"panel_cases.csv       : {paths['cases_csv']}")
    print(f"panel_results.json    : {paths['results_json']}")
    print(f"report HTML           : {out_html}")
    print("-" * 70)
    print(
        f"MANAGEMENT TAKEAWAY — AH36 at {REFERENCE_SPACING:.0f} mm spacing, "
        f"profile {REFERENCE_PROFILE}, axial sigma_x={sx:.0f} MPa:"
    )
    if t_min is not None:
        print(
            f"  minimum acceptable plate thickness ≈ {t_min:.1f} mm "
            f"(utilisation crosses 1.0); governing mode there: "
            f"{MODE_LABEL.get(gov_at_min, gov_at_min)}."
        )
    else:
        print("  utilisation stays below 1.0 across the full thickness range; "
              f"governing mode: {MODE_LABEL.get(gov_at_min, gov_at_min)}.")
    print("=" * 70)


if __name__ == "__main__":
    main()
