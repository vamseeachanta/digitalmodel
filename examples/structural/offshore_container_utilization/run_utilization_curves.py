"""Generate DNV 2.7-1 offshore-container utilization curves.

Sweeps container rating, sling angle, footprint aspect ratio and the (L, W)
plan envelope using the DNVGL-ST-E271-cited design model
(``digitalmodel.structural.offshore_container``). Writes PNG charts + a markdown
report next to this script.

Run::

    uv run python examples/structural/offshore_container_utilization/run_utilization_curves.py

Reduced-order screening only. Final design must be verified by FEA + prototype
tests per DNVGL-ST-E271 (§4.2, §4.6).
"""

from __future__ import annotations

import math
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

from digitalmodel.structural.offshore_container.dnv_2_7_1 import (
    SECTION_CATALOGUE,
    DesignFactors,
    factor_citations,
    utilization,
)

OUT = Path(__file__).parent
FACTORS = DesignFactors()
FY = 355e6  # S355 (Re)

# Representative offshore basket; lift height set so the sling angle is the
# DNV default of 45 deg to the vertical (lift_height = plan half-diagonal).
BASE_L, BASE_W = 6.0, 2.5
BASE_LIFT = 0.5 * math.hypot(BASE_L, BASE_W)  # -> v = 45 deg
BASE_R = 25_000.0
BASE_SECTION = SECTION_CATALOGUE["SHS250x12"]


def _finish(ax, title, xlabel, ylabel):
    ax.axhline(1.0, color="k", ls="--", lw=1, label="util = 1.0 (limit)")
    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.grid(True, alpha=0.3)
    ax.legend(fontsize=8)


def curve_vs_rating() -> Path:
    """Governing utilization vs rating R for several frame sections."""
    ratings = np.linspace(5_000, 50_000, 60)
    fig, ax = plt.subplots(figsize=(7, 4.5))
    for name, sec in SECTION_CATALOGUE.items():
        u = [utilization(BASE_L, BASE_W, BASE_LIFT, r, sec, FY, FACTORS).governing
             for r in ratings]
        ax.plot(ratings / 1000.0, u, label=name)
    _finish(ax, "Utilization vs rating R\n(L=6.0 m, W=2.5 m, v=45 deg, S355, DNV 2.7-1)",
            "Rating R [tonne]", "Governing utilization")
    p = OUT / "util_vs_rating.png"
    fig.tight_layout(); fig.savefig(p, dpi=130); plt.close(fig)
    return p


def curve_vs_sling_angle() -> Path:
    """Component utilizations vs sling angle to the vertical."""
    vees = np.linspace(15, 65, 60)  # deg from vertical
    half_diag = 0.5 * math.hypot(BASE_L, BASE_W)
    fig, ax = plt.subplots(figsize=(7, 4.5))
    top, pad, floor = [], [], []
    for vd in vees:
        lift = half_diag / math.tan(math.radians(vd))
        u = utilization(BASE_L, BASE_W, lift, BASE_R, BASE_SECTION, FY, FACTORS)
        top.append(u.top_rail); pad.append(u.pad_eye); floor.append(u.floor)
    ax.plot(vees, top, color="C0", label="top rail (axial)")
    ax.plot(vees, pad, color="C1", label="pad eye (RSL)")
    ax.plot(vees, floor, color="C2", label="floor (bending, angle-independent)")
    ax.axvline(45, color="grey", ls=":", lw=1, label="DNV default v=45 deg")
    _finish(ax, f"Utilization vs sling angle to vertical\n(SHS250x12, R={BASE_R/1000:.0f} t)",
            "Sling angle to vertical v [deg]", "Utilization")
    p = OUT / "util_vs_sling_angle.png"
    fig.tight_layout(); fig.savefig(p, dpi=130); plt.close(fig)
    return p


def curve_vs_aspect_ratio() -> Path:
    """Governing utilization vs footprint aspect ratio L/W at constant plan area."""
    area = BASE_L * BASE_W
    ratios = np.linspace(0.5, 4.0, 60)
    fig, ax = plt.subplots(figsize=(7, 4.5))
    for name in ("SHS200x10", "SHS200x12", "SHS250x12"):
        sec = SECTION_CATALOGUE[name]
        u = []
        for ratio in ratios:
            w = math.sqrt(area / ratio)
            length = ratio * w
            lift = 0.5 * math.hypot(length, w)  # keep v = 45 deg
            u.append(utilization(length, w, lift, BASE_R, sec, FY, FACTORS).governing)
        ax.plot(ratios, u, label=name)
    _finish(ax, f"Utilization vs aspect ratio L/W\n(plan area={area:.1f} m^2, R={BASE_R/1000:.0f} t, v=45 deg)",
            "Aspect ratio L/W", "Governing utilization")
    p = OUT / "util_vs_aspect_ratio.png"
    fig.tight_layout(); fig.savefig(p, dpi=130); plt.close(fig)
    return p


def envelope_heatmap() -> Path:
    """Utilization contour over the (L, W) plan envelope; mark util=1.0."""
    lengths = np.linspace(2.0, 12.0, 80)
    widths = np.linspace(2.0, 3.5, 60)
    LL, WW = np.meshgrid(lengths, widths)
    U = np.zeros_like(LL)
    for i in range(LL.shape[0]):
        for j in range(LL.shape[1]):
            lift = 0.5 * math.hypot(LL[i, j], WW[i, j])
            U[i, j] = utilization(LL[i, j], WW[i, j], lift, BASE_R,
                                  BASE_SECTION, FY, FACTORS).governing
    fig, ax = plt.subplots(figsize=(7.5, 4.5))
    cf = ax.contourf(LL, WW, U, levels=np.linspace(0, 2.0, 21), cmap="RdYlGn_r", extend="max")
    ax.contour(LL, WW, U, levels=[1.0], colors="k", linewidths=2)
    cs = ax.contour(LL, WW, U, levels=[0.5, 1.5], colors="k", linewidths=0.6, alpha=0.5)
    ax.clabel(cs, fmt="%.1f", fontsize=8)
    fig.colorbar(cf, ax=ax, label="Governing utilization")
    ax.set_title(f"Plan-envelope utilization (SHS250x12, R={BASE_R/1000:.0f} t, v=45 deg)\n"
                 "bold line = util 1.0 feasibility boundary (DNV 2.7-1)")
    ax.set_xlabel("Length L [m]"); ax.set_ylabel("Width W [m]")
    p = OUT / "util_envelope_LW.png"
    fig.tight_layout(); fig.savefig(p, dpi=130); plt.close(fig)
    return p


def _citation_block() -> str:
    cites = factor_citations()
    if not cites:
        return ("> Calc citations: **standalone mode** — the `dnvgl-st-e271` wiki "
                "page was not resolvable from this clone, so citation sidecars were "
                "not emitted (set `LLM_WIKI_PATH` to enable). Factor values are still "
                "the DNV-ST-E271 clause values listed below.")
    rows = "\n".join(
        f"| {k} | {cv.value} | {cv.citation.code_id} {cv.citation.section} |"
        for k, cv in cites.items())
    return ("Calc citations resolved against `code_id: dnvgl-st-e271`:\n\n"
            "| Factor | Value | Citation |\n|---|---|---|\n" + rows)


def write_report(paths: list[Path]) -> Path:
    b = utilization(BASE_L, BASE_W, BASE_LIFT, BASE_R, BASE_SECTION, FY, FACTORS)
    rows = []
    for name, sec in SECTION_CATALOGUE.items():
        u = utilization(BASE_L, BASE_W, BASE_LIFT, BASE_R, sec, FY, FACTORS)
        rows.append(f"| {name} | {sec.area*1e4:.1f} | {sec.modulus*1e6:.1f} | "
                    f"{u.top_rail:.2f} | {u.floor:.2f} | {u.pad_eye:.2f} | **{u.governing:.2f}** |")
    table = "\n".join(rows)
    imgs = "\n\n".join(f"### {p.stem}\n\n![{p.stem}]({p.name})" for p in paths)
    report = f"""# DNV 2.7-1 offshore-container utilization curves

Reduced-order structural **screening** of the governing offshore-lift load case
for an offshore container (CCU) per **DNVGL-ST-E271 (2.7-1), Edition August 2017**.
Generated by `run_utilization_curves.py`.

> Screening tool only. Final design must be verified by FEA + prototype tests
> per DNVGL-ST-E271 (§4.2, §4.6).

## Design basis (clause-traced)

- Primary structure design load **FL = {FACTORS.structure_load_factor} · R · g** (§4.2.3.1);
  internal/floor load Fi = ({FACTORS.structure_load_factor}·R − T)·g.
- Pad eyes **Fp = {FACTORS.padeye_load_factor} · R · g** over (n−1) pad eyes, n∈[2,4]
  (single pad eye {FACTORS.single_padeye_load_factor}·R·g); resolved at sling angle v to vertical
  (default {FACTORS.default_sling_angle_to_vertical_deg:.0f}°).
- Allowable stress **σe ≤ {FACTORS.usage_factor} · Re** (§4.2.1); S355 → allowable
  {FY*FACTORS.usage_factor/1e6:.0f} MPa.
- Tare fraction T/R = {FACTORS.tare_fraction}.

{_citation_block()}

## Baseline unit

L = {BASE_L} m, W = {BASE_W} m, lift height = {BASE_LIFT:.2f} m (v = {b.sling_angle_to_vertical_deg:.0f}°),
R = {BASE_R/1000:.0f} t, frame = {BASE_SECTION}.
Primary-structure design load FL = {b.FL_kN:.0f} kN; sling load per pad eye RSL = {b.rsl_kN:.0f} kN.
Governing utilization = **{b.governing:.2f}** (top rail {b.top_rail:.2f}, floor {b.floor:.2f}, pad eye {b.pad_eye:.2f}).

## Section comparison at baseline (R = {BASE_R/1000:.0f} t)

| Section | Area [cm^2] | Z [cm^3] | Top rail | Floor | Pad eye | Governing |
|---|---|---|---|---|---|---|
{table}

## Curves

{imgs}
"""
    p = OUT / "README.md"
    p.write_text(report)
    return p


def main() -> None:
    paths = [curve_vs_rating(), curve_vs_sling_angle(),
             curve_vs_aspect_ratio(), envelope_heatmap()]
    report = write_report(paths)
    print("Wrote:")
    for p in paths + [report]:
        print(f"  {p}")


if __name__ == "__main__":
    main()
