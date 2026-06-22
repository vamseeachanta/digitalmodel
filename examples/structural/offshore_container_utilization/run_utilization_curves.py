"""Generate DNV 2.7-1 offshore-container utilization curves.

Sweeps container rating, sling angle, footprint aspect ratio and the (L, W)
plan envelope, and writes PNG charts + a markdown report next to this script.

Run::

    uv run python examples/structural/offshore_container_utilization/run_utilization_curves.py

Reduced-order screening only -- see
``digitalmodel.structural.offshore_container.dnv_2_7_1`` for the model and its
assumptions. Final design must be verified by FEA per DNV-ST-E271.
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
    SHS,
    DesignFactors,
    sling_angle,
    utilization,
)

OUT = Path(__file__).parent
FACTORS = DesignFactors()
FY = 355e6  # S355

# A representative baseline CCU (a "half-height" offshore basket-ish unit).
BASE_L, BASE_W, BASE_H = 6.0, 2.5, 2.5
BASE_LIFT = 2.5          # master link height above top frame [m]
BASE_R = 25_000.0        # gross rating [kg]
BASE_SECTION = SECTION_CATALOGUE["SHS150x10"]


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
    _finish(ax, "Utilization vs rating R\n(L=6.0 m, W=2.5 m, lift=2.5 m, S355)",
            "Rating R [tonne]", "Governing utilization")
    p = OUT / "util_vs_rating.png"
    fig.tight_layout(); fig.savefig(p, dpi=130); plt.close(fig)
    return p


def curve_vs_sling_angle() -> Path:
    """Utilization vs sling angle (top-rail vs floor split) for two ratings."""
    betas = np.linspace(25, 80, 60)  # degrees
    half_diag = 0.5 * math.hypot(BASE_L, BASE_W)
    fig, ax = plt.subplots(figsize=(7, 4.5))
    for r, style in ((20_000.0, "-"), (35_000.0, "--")):
        gov, top, bot = [], [], []
        for bd in betas:
            lift = half_diag * math.tan(math.radians(bd))
            u = utilization(BASE_L, BASE_W, lift, r, BASE_SECTION, FY, FACTORS)
            gov.append(u.governing); top.append(u.top_rail); bot.append(u.bottom_rail)
        ax.plot(betas, gov, style, color="C3", label=f"governing, R={r/1000:.0f} t")
        ax.plot(betas, top, style, color="C0", alpha=0.6, label=f"top rail (axial), R={r/1000:.0f} t")
        ax.plot(betas, bot, style, color="C2", alpha=0.6, label=f"floor (bending), R={r/1000:.0f} t")
    _finish(ax, "Utilization vs sling angle\n(SHS150x10, L=6.0 m, W=2.5 m)",
            "Sling angle from horizontal [deg]", "Utilization")
    p = OUT / "util_vs_sling_angle.png"
    fig.tight_layout(); fig.savefig(p, dpi=130); plt.close(fig)
    return p


def curve_vs_aspect_ratio() -> Path:
    """Utilization vs footprint aspect ratio L/W at constant plan area."""
    area = BASE_L * BASE_W
    ratios = np.linspace(0.5, 4.0, 60)
    fig, ax = plt.subplots(figsize=(7, 4.5))
    for name in ("SHS120x8", "SHS150x10", "SHS200x10"):
        sec = SECTION_CATALOGUE[name]
        u = []
        for ratio in ratios:
            w = math.sqrt(area / ratio)
            length = ratio * w
            u.append(utilization(length, w, BASE_LIFT, BASE_R, sec, FY, FACTORS).governing)
        ax.plot(ratios, u, label=name)
    _finish(ax, f"Utilization vs aspect ratio L/W\n(plan area={area:.1f} m^2, R={BASE_R/1000:.0f} t)",
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
            U[i, j] = utilization(LL[i, j], WW[i, j], BASE_LIFT, BASE_R,
                                  BASE_SECTION, FY, FACTORS).governing
    fig, ax = plt.subplots(figsize=(7.5, 4.5))
    cf = ax.contourf(LL, WW, U, levels=np.linspace(0, 2.0, 21), cmap="RdYlGn_r", extend="max")
    ax.contour(LL, WW, U, levels=[1.0], colors="k", linewidths=2)
    cs = ax.contour(LL, WW, U, levels=[0.5, 1.5], colors="k", linewidths=0.6, alpha=0.5)
    ax.clabel(cs, fmt="%.1f", fontsize=8)
    fig.colorbar(cf, ax=ax, label="Governing utilization")
    ax.set_title(f"Plan-envelope utilization (SHS150x10, R={BASE_R/1000:.0f} t, lift={BASE_LIFT} m)\n"
                 "bold line = util 1.0 feasibility boundary")
    ax.set_xlabel("Length L [m]"); ax.set_ylabel("Width W [m]")
    p = OUT / "util_envelope_LW.png"
    fig.tight_layout(); fig.savefig(p, dpi=130); plt.close(fig)
    return p


def write_report(paths: list[Path]) -> Path:
    b = utilization(BASE_L, BASE_W, BASE_LIFT, BASE_R, BASE_SECTION, FY, FACTORS)
    rows = []
    for name, sec in SECTION_CATALOGUE.items():
        u = utilization(BASE_L, BASE_W, BASE_LIFT, BASE_R, sec, FY, FACTORS)
        rows.append(f"| {name} | {sec.area*1e4:.1f} | {sec.modulus*1e6:.1f} | "
                    f"{u.top_rail:.2f} | {u.bottom_rail:.2f} | **{u.governing:.2f}** |")
    table = "\n".join(rows)
    imgs = "\n\n".join(f"### {p.stem}\n\n![{p.stem}]({p.name})" for p in paths)
    report = f"""# DNV 2.7-1 offshore-container utilization curves

Reduced-order structural **screening** of the governing offshore-lift load case
for an offshore container (CCU) per DNV 2.7-1 / DNV-ST-E271. Generated by
`run_utilization_curves.py`.

> Screening tool only. Final design must be verified by FEA per DNV-ST-E271.

## Model & assumptions

- 4-point top lift to a master link; **skew case** (two diagonally opposite
  slings carry the full rating R) taken as governing, per DNV 2.7-1.
- Dynamic amplification factor DAF = {FACTORS.daf}; material factor
  gamma_m = {FACTORS.gamma_m}; usage factor eta = {FACTORS.eta};
  tare fraction = {FACTORS.tare_fraction} (payload = (1-tare) * R).
- Material S355 (fy = 355 MPa); allowable = fy * eta / gamma_m
  = {FY*FACTORS.eta/FACTORS.gamma_m/1e6:.0f} MPa.
- Top side rail checked in axial (inward horizontal sling component); bottom
  floor beam checked in bending (payload UDL, simply supported over L).

## Baseline unit

L = {BASE_L} m, W = {BASE_W} m, H = {BASE_H} m, lift height = {BASE_LIFT} m,
R = {BASE_R/1000:.0f} t, frame = {BASE_SECTION}.
Sling angle = {b.beta_deg:.1f} deg. Governing utilization = **{b.governing:.2f}**
(top rail {b.top_rail:.2f}, floor {b.bottom_rail:.2f}).

## Section comparison at baseline (R = {BASE_R/1000:.0f} t)

| Section | Area [cm^2] | Z [cm^3] | Top rail | Floor | Governing |
|---|---|---|---|---|---|
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
