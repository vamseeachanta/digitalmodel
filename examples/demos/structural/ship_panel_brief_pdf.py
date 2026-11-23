# ABOUTME: Generates a one-page branded PDF management brief for the ship hull
# ABOUTME: stiffened-panel buckling study (DNV-RP-C201) for offline circulation.
"""Build a single-page, branded management brief PDF for the ship hull
stiffened-panel buckling study.

Data comes from the validated DNV-RP-C201 solver via
``digitalmodel.structural.panel_design_explorer.build_design_grids`` (the same
``StiffenedPanelBucklingAnalyzer`` that passes the golden tests). No physics is
re-implemented here -- this module only reads the solver grids and renders a
clean one-page PDF with matplotlib.

Run:
    .venv/bin/python examples/demos/structural/ship_panel_brief_pdf.py
"""

from __future__ import annotations

import os
import textwrap

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib.patches import FancyBboxPatch

from digitalmodel.structural.panel_design_explorer import (
    ExplorerConfig,
    build_design_grids,
    combo_key,
)

# ---------------------------------------------------------------------------
# Branding palette (matches the interactive dashboards)
# ---------------------------------------------------------------------------
NAVY = "#1a365d"
ORANGE = "#ed8936"
LIGHT_NAVY = "#2c5282"
SAFE = "#2f855a"
UNSAFE = "#c53030"
GREY = "#4a5568"

# ---------------------------------------------------------------------------
# Reference design point for the headline brief
# ---------------------------------------------------------------------------
REF_PROFILE = {
    "name": "T400x120",
    "web_height": 400,
    "web_thickness": 12,
    "flange_width": 120,
    "flange_thickness": 16,
}
REF_SPACING = 700.0       # mm
REF_SPAN = 2400.0         # mm
REF_TAU = 0.0             # MPa shear
REF_LOAD = 150.0          # MPa applied longitudinal stress
GRADES = ["Grade A", "AH36", "EH40"]
GRADE_FY = {"Grade A": 235, "AH36": 355, "EH40": 390}
GRADE_COLOR = {"Grade A": GREY, "AH36": NAVY, "EH40": ORANGE}
SPACINGS = [600.0, 700.0, 800.0]
TABLE_THICK = [10.0, 12.0, 14.0, 16.0, 18.0]

OUT_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "output")
OUT_PDF = os.path.join(OUT_DIR, "ship_panel_buckling_brief.pdf")


def build_data():
    """Run the validated solver over only the combos this brief needs."""
    cfg = ExplorerConfig(
        grades=GRADES,
        profiles=[REF_PROFILE],
        spacings=SPACINGS,
        spans=[REF_SPAN],
        tau_levels=[REF_TAU],
    )
    return build_design_grids(cfg)


def _nearest_index(values, target):
    return min(range(len(values)), key=lambda i: abs(values[i] - target))


def compute_findings(data):
    """Derive the headline numbers from the solver grids."""
    thick = data["thickness"]
    loads = data["load"]
    li = _nearest_index(loads, REF_LOAD)

    findings = {"thickness": thick, "loads": loads, "load_index": li}

    # Utilisation vs thickness at REF_LOAD, per grade (reference spacing/span/tau)
    util_by_grade = {}
    min_thick = {}
    for grade in GRADES:
        g = data["grids"][combo_key(grade, REF_PROFILE["name"], REF_SPACING,
                                    REF_SPAN, REF_TAU)]
        row = g["util"][li]
        util_by_grade[grade] = row
        # first thickness where utilisation <= 1.0
        mt = None
        for t, u in zip(thick, row):
            if u <= 1.0:
                mt = t
                break
        min_thick[grade] = mt
    findings["util_by_grade"] = util_by_grade
    findings["min_thick"] = min_thick

    # AH36 allowable axial load vs thickness across spacings
    allow_by_spacing = {}
    for sp in SPACINGS:
        g = data["grids"][combo_key("AH36", REF_PROFILE["name"], sp,
                                    REF_SPAN, REF_TAU)]
        allow_by_spacing[sp] = g["allow"]
    findings["allow_by_spacing"] = allow_by_spacing

    # Table: AH36 allowable at reference spacing for selected thicknesses
    g_ref = data["grids"][combo_key("AH36", REF_PROFILE["name"], REF_SPACING,
                                    REF_SPAN, REF_TAU)]
    table = []
    for t in TABLE_THICK:
        ti = _nearest_index(thick, t)
        table.append((thick[ti], g_ref["allow"][ti]))
    findings["table"] = table

    # Governing modes present at the reference design point (over thickness)
    modes_present = set()
    for grade in GRADES:
        g = data["grids"][combo_key(grade, REF_PROFILE["name"], REF_SPACING,
                                    REF_SPAN, REF_TAU)]
        modes_present.update(g["mode"][li])
    findings["modes_present"] = modes_present

    return findings


def render(data, f):
    """Render the one-page brief to OUT_PDF."""
    os.makedirs(OUT_DIR, exist_ok=True)

    fig = plt.figure(figsize=(8.27, 11.69))  # A4 portrait, inches
    fig.patch.set_facecolor("white")

    # ----- Header band -----------------------------------------------------
    head = fig.add_axes([0.0, 0.90, 1.0, 0.10])
    head.set_axis_off()
    head.add_patch(FancyBboxPatch(
        (0, 0), 1, 1, boxstyle="square,pad=0",
        facecolor=NAVY, edgecolor="none", transform=head.transAxes))
    head.add_patch(FancyBboxPatch(
        (0, 0), 1, 0.10, boxstyle="square,pad=0",
        facecolor=ORANGE, edgecolor="none", transform=head.transAxes))
    head.text(0.035, 0.74,
              "Ship Hull Stiffened-Panel Buckling — Design Brief",
              color="white", fontsize=15.5, fontweight="bold",
              va="center", ha="left", transform=head.transAxes)
    head.text(0.035, 0.46,
              "DNV-RP-C201 buckling assessment  ·  marine grades: "
              "Grade A (235), AH36 (355), EH40 (390 MPa)",
              color="#cbd5e0", fontsize=9.3, va="center", ha="left",
              transform=head.transAxes)
    head.text(0.035, 0.24,
              "Validated solver — StiffenedPanelBucklingAnalyzer "
              "(golden-tested against DNV-RP-C201)",
              color=ORANGE, fontsize=8.2, fontweight="bold",
              va="center", ha="left", transform=head.transAxes)
    head.set_xlim(0, 1)
    head.set_ylim(0, 1)

    thick = f["thickness"]

    # ----- Chart A: headline utilisation vs thickness ----------------------
    axA = fig.add_axes([0.085, 0.555, 0.40, 0.295])
    axA.axhspan(0, 1.0, color=SAFE, alpha=0.08)
    axA.axhspan(1.0, 3.0, color=UNSAFE, alpha=0.08)
    axA.axhline(1.0, color=UNSAFE, lw=1.4, ls="--")
    axA.text(thick[-1], 1.03, "unity (util = 1.0)", color=UNSAFE,
             fontsize=7, ha="right", va="bottom")
    for grade in GRADES:
        axA.plot(thick, f["util_by_grade"][grade], color=GRADE_COLOR[grade],
                 lw=2.0, label=f"{grade} (f_y={GRADE_FY[grade]})")
    axA.set_ylim(0, 2.6)
    axA.set_xlim(thick[0], thick[-1])
    axA.set_xlabel("Plate thickness (mm)", fontsize=8.5)
    axA.set_ylabel("Buckling utilisation", fontsize=8.5)
    axA.set_title(
        f"Utilisation vs thickness  ·  σx = {REF_LOAD:g} MPa\n"
        f"{REF_PROFILE['name']} · {REF_SPACING:g} mm spacing · "
        f"{REF_SPAN:g} mm span · zero shear",
        fontsize=8.5, color=NAVY, fontweight="bold")
    axA.legend(fontsize=7, loc="upper right", framealpha=0.9)
    axA.tick_params(labelsize=7.5)
    axA.grid(True, alpha=0.25)

    # ----- Chart B: AH36 allowable axial load vs thickness -----------------
    axB = fig.add_axes([0.585, 0.555, 0.355, 0.295])
    for sp in SPACINGS:
        allow = f["allow_by_spacing"][sp]
        xs = [t for t, a in zip(thick, allow) if a is not None]
        ys = [a for a in allow if a is not None]
        ls = "-" if sp == REF_SPACING else "--"
        lw = 2.2 if sp == REF_SPACING else 1.3
        col = NAVY if sp == REF_SPACING else (ORANGE if sp < REF_SPACING else GREY)
        axB.plot(xs, ys, color=col, lw=lw, ls=ls,
                 label=f"{sp:g} mm spacing")
    axB.set_xlim(thick[0], thick[-1])
    axB.set_xlabel("Plate thickness (mm)", fontsize=8.5)
    axB.set_ylabel("Max allowable σx (MPa)", fontsize=8.5)
    axB.set_title("AH36 design margin\nallowable axial load vs thickness",
                  fontsize=8.5, color=NAVY, fontweight="bold")
    axB.legend(fontsize=7, loc="upper left", framealpha=0.9)
    axB.tick_params(labelsize=7.5)
    axB.grid(True, alpha=0.25)

    # ----- Key findings box ------------------------------------------------
    mt = f["min_thick"]["AH36"]
    mtA = f["min_thick"]["Grade A"]
    mtE = f["min_thick"]["EH40"]
    mode_names = {0: "plate-field", 1: "overall column", 2: "stiffener tripping"}
    present = sorted(f["modes_present"])
    gov_txt = " / ".join(mode_names[m] for m in present if m in mode_names)
    column_governs = 1 in f["modes_present"]

    bullets = [
        ("Reference AH36 panel (700 mm spacing, T400x120, 2400 mm span, zero "
         f"shear): minimum acceptable plate thickness for σx = "
         f"{REF_LOAD:g} MPa is {mt:g} mm "
         "(first thickness with utilisation ≤ 1.0)."),
        (f"Grade sensitivity at σx = {REF_LOAD:g} MPa: Grade A needs "
         f"≈{mtA:g} mm, AH36 ≈{mt:g} mm, EH40 ≈{mtE:g} mm — "
         "higher grade lifts capacity and trims required plate."),
        (f"Governing failure modes at the reference point: {gov_txt}. "
         + ("Overall column buckling never governs at ship frame spacings."
            if not column_governs else
            "Overall column buckling can govern for these geometries.")),
        ("Longer stiffener span and added in-plane shear both reduce the "
         "allowable axial load — see the AH36 margin curve for the "
         "spacing trade-off."),
    ]

    axK = fig.add_axes([0.085, 0.265, 0.555, 0.255])
    axK.set_axis_off()
    axK.add_patch(FancyBboxPatch(
        (0, 0), 1, 1, boxstyle="round,pad=0.02,rounding_size=0.03",
        facecolor="#f7fafc", edgecolor=NAVY, lw=1.2,
        transform=axK.transAxes))
    axK.text(0.04, 0.93, "KEY FINDINGS", color=NAVY, fontsize=11,
             fontweight="bold", va="top", ha="left", transform=axK.transAxes)
    y = 0.78
    line_h = 0.052
    for b in bullets:
        wrapped = textwrap.fill(b, width=70)
        nlines = wrapped.count("\n") + 1
        axK.text(0.045, y, "▪", color=ORANGE, fontsize=8,
                 va="top", ha="left", transform=axK.transAxes)
        axK.text(0.085, y, wrapped, color="#1a202c", fontsize=7.9, va="top",
                 ha="left", linespacing=1.25, transform=axK.transAxes)
        y -= line_h * nlines + 0.028

    # ----- Acceptable-load table -------------------------------------------
    axT = fig.add_axes([0.665, 0.265, 0.275, 0.255])
    axT.set_axis_off()
    axT.text(0.5, 1.0,
             "AH36 max allowable σx (MPa)\n700 mm spacing · "
             "T400x120 · 2400 mm · τ=0",
             color=NAVY, fontsize=8.0, fontweight="bold", va="top",
             ha="center", transform=axT.transAxes)
    rows = [["t (mm)", "Allowable σx"]]
    for t, a in f["table"]:
        rows.append([f"{t:g}", "—" if a is None else f"{a:g}"])
    tbl = axT.table(cellText=rows[1:], colLabels=rows[0],
                    cellLoc="center", colLoc="center",
                    bbox=[0.0, 0.10, 1.0, 0.72])
    tbl.auto_set_font_size(False)
    tbl.set_fontsize(8.5)
    for (r, c), cell in tbl.get_celld().items():
        cell.set_edgecolor("#cbd5e0")
        if r == 0:
            cell.set_facecolor(NAVY)
            cell.set_text_props(color="white", fontweight="bold")
        elif r % 2 == 0:
            cell.set_facecolor("#edf2f7")

    # ----- Footer ----------------------------------------------------------
    foot = fig.add_axes([0.0, 0.0, 1.0, 0.05])
    foot.set_axis_off()
    foot.add_patch(FancyBboxPatch(
        (0, 0), 1, 1, boxstyle="square,pad=0", facecolor=NAVY,
        edgecolor="none", transform=foot.transAxes))
    foot.text(0.035, 0.55,
              "Generated by digitalmodel · validated "
              "StiffenedPanelBucklingAnalyzer (DNV-RP-C201) · γ_M = 1.15",
              color="white", fontsize=6.8, va="center", ha="left",
              transform=foot.transAxes)
    foot.text(0.965, 0.55,
              "Full interactive explorer available separately.",
              color="#cbd5e0", fontsize=6.8, va="center", ha="right",
              transform=foot.transAxes)
    foot.set_xlim(0, 1)
    foot.set_ylim(0, 1)

    with PdfPages(OUT_PDF) as pdf:
        pdf.savefig(fig, facecolor="white")
    plt.close(fig)


def main():
    print("Running validated DNV-RP-C201 solver (build_design_grids)...")
    data = build_data()
    findings = compute_findings(data)
    print("Reference AH36 min acceptable thickness at "
          f"sigma_x={REF_LOAD:g} MPa: {findings['min_thick']['AH36']:g} mm")
    render(data, findings)
    size = os.path.getsize(OUT_PDF)
    print(f"Wrote {OUT_PDF} ({size/1024:.1f} KB)")
    return findings


if __name__ == "__main__":
    main()
