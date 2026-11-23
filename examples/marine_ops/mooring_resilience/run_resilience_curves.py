"""Generate mooring-system resilience screening curves.

Composes the existing pre-computed atlases (``fpso_mooring_full``,
``anchor_capacity``) into a resilience view and sweeps the two dominant drivers:
sea state (Hs -> fatigue / extreme weather) and water depth (-> peak tension /
foundation). Writes PNG charts + a markdown report next to this script.

Run::

    uv run python examples/marine_ops/mooring_resilience/run_resilience_curves.py

Screening only -- a full mooring analysis (OrcaFlex) remains the document of
record. See ``digitalmodel.mooring_resilience`` for the model and assumptions.
"""

from __future__ import annotations

from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import ListedColormap, BoundaryNorm

from digitalmodel.mooring_resilience import Metocean, MooringConfig, ResilienceFactors, assess

OUT = Path(__file__).parent
F = ResilienceFactors()

# Baseline: 120 mm studless chain, 12-line spread, 1000 m, soft clay.
BASE = MooringConfig(n_lines=12, mbl_kN=15000, line_area_mm2=22600,
                     anchor_diameter_m=5.0, anchor_length_m=20.0,
                     soil_su_kpa=60.0, water_depth_m=1000.0)
BASE_TP = 11.0
LIGHT_VAL = {"GREEN": 0, "AMBER": 1, "RED": 2, "ESCALATE": 3}


def _bands(ax):
    ax.axhspan(0.0, 0.8, color="tab:green", alpha=0.08)
    ax.axhspan(0.8, 1.0, color="tab:orange", alpha=0.10)
    ax.axhspan(1.0, 2.0, color="tab:red", alpha=0.08)
    ax.axhline(1.0, color="k", ls="--", lw=1)


def curve_vs_Hs() -> Path:
    """Fatigue margin + governing utilization vs sea state (extreme weather)."""
    hs = np.linspace(2.0, 5.0, 40)
    res = [assess(BASE, Metocean(h, BASE_TP), F) for h in hs]
    fig, ax1 = plt.subplots(figsize=(7, 4.5))
    ax1.plot(hs, [r.governing_util for r in res], color="C3", label="governing utilization")
    _bands(ax1)
    ax1.set_xlabel("Significant wave height Hs [m]")
    ax1.set_ylabel("Governing utilization")
    ax1.set_ylim(0, 1.6)
    ax2 = ax1.twinx()
    ax2.plot(hs, [r.fatigue_margin for r in res], color="C0", ls="-.", label="fatigue margin (life/design)")
    ax2.axhline(1.0, color="C0", ls=":", lw=1)
    ax2.set_ylabel("Fatigue margin [-]", color="C0")
    ax2.set_ylim(0, 8)
    ax1.set_title("Resilience vs sea state (1000 m, 120 mm chain)\nbands: green<0.8, amber 0.8-1.0, red>1.0")
    l1, lb1 = ax1.get_legend_handles_labels(); l2, lb2 = ax2.get_legend_handles_labels()
    ax1.legend(l1 + l2, lb1 + lb2, fontsize=8, loc="upper left")
    ax1.grid(True, alpha=0.3)
    p = OUT / "resilience_vs_Hs.png"
    fig.tight_layout(); fig.savefig(p, dpi=130); plt.close(fig)
    return p


def curve_intact_vs_damaged() -> Path:
    """Intact vs one-line-damaged tension utilization vs water depth."""
    depths = np.linspace(500, 1500, 40)
    res = [assess(MooringConfig(BASE.n_lines, BASE.mbl_kN, BASE.line_area_mm2,
                                BASE.anchor_diameter_m, BASE.anchor_length_m,
                                BASE.soil_su_kpa, d), Metocean(3.0, BASE_TP), F)
           for d in depths]
    fig, ax = plt.subplots(figsize=(7, 4.5))
    ax.plot(depths, [r.util_intact for r in res], color="C0", label="intact (FoS 1.67)")
    ax.plot(depths, [r.util_damaged for r in res], color="C1", label="one-line damaged (FoS 1.25)")
    ax.plot(depths, [r.util_foundation for r in res], color="C2", label="foundation (FoS 1.5)")
    _bands(ax)
    ax.set_title("Tension & foundation utilization vs water depth\n(Hs=3 m, 120 mm chain, 5 m suction anchor)")
    ax.set_xlabel("Water depth [m]"); ax.set_ylabel("Utilization")
    ax.set_ylim(0, 1.3); ax.grid(True, alpha=0.3); ax.legend(fontsize=8, loc="upper left")
    p = OUT / "util_intact_vs_damaged.png"
    fig.tight_layout(); fig.savefig(p, dpi=130); plt.close(fig)
    return p


def curve_foundation_vs_soil() -> Path:
    """Foundation utilization vs soil strength for several anchor diameters."""
    su = np.linspace(20, 100, 40)
    fig, ax = plt.subplots(figsize=(7, 4.5))
    for dia in (3.5, 5.0, 6.5):
        u = []
        for s in su:
            c = MooringConfig(BASE.n_lines, BASE.mbl_kN, BASE.line_area_mm2,
                              dia, BASE.anchor_length_m, s, BASE.water_depth_m)
            u.append(assess(c, Metocean(3.0, BASE_TP), F).util_foundation)
        ax.plot(su, u, label=f"anchor D={dia} m")
    _bands(ax)
    ax.set_title("Foundation utilization vs soil shear strength\n(1000 m, Hs=3 m, L=20 m anchor)")
    ax.set_xlabel("Undrained shear strength su [kPa]"); ax.set_ylabel("Foundation utilization")
    ax.set_ylim(0, 1.3); ax.grid(True, alpha=0.3); ax.legend(fontsize=8)
    p = OUT / "foundation_vs_soil.png"
    fig.tight_layout(); fig.savefig(p, dpi=130); plt.close(fig)
    return p


def resilience_map() -> Path:
    """Traffic-light map over (Hs, water depth)."""
    hs = np.linspace(2.0, 5.0, 50)
    depths = np.linspace(500, 1500, 50)
    HH, DD = np.meshgrid(hs, depths)
    Z = np.zeros_like(HH)
    for i in range(HH.shape[0]):
        for j in range(HH.shape[1]):
            c = MooringConfig(BASE.n_lines, BASE.mbl_kN, BASE.line_area_mm2,
                              BASE.anchor_diameter_m, BASE.anchor_length_m,
                              BASE.soil_su_kpa, DD[i, j])
            Z[i, j] = LIGHT_VAL[assess(c, Metocean(HH[i, j], BASE_TP), F).light]
    cmap = ListedColormap(["#2ca02c", "#ff7f0e", "#d62728", "#7f7f7f"])
    norm = BoundaryNorm([-0.5, 0.5, 1.5, 2.5, 3.5], cmap.N)
    fig, ax = plt.subplots(figsize=(7.5, 4.5))
    pc = ax.pcolormesh(HH, DD, Z, cmap=cmap, norm=norm, shading="auto")
    cb = fig.colorbar(pc, ax=ax, ticks=[0, 1, 2, 3])
    cb.ax.set_yticklabels(["GREEN", "AMBER", "RED", "ESCALATE"])
    ax.set_title("Mooring resilience traffic-light (120 mm chain, 12 lines)\nfn(sea state, water depth)")
    ax.set_xlabel("Significant wave height Hs [m]"); ax.set_ylabel("Water depth [m]")
    p = OUT / "resilience_map_Hs_depth.png"
    fig.tight_layout(); fig.savefig(p, dpi=130); plt.close(fig)
    return p


def write_report(paths: list[Path]) -> Path:
    b = assess(BASE, Metocean(3.0, BASE_TP), F)
    imgs = "\n\n".join(f"### {p.stem}\n\n![{p.stem}]({p.name})" for p in paths)
    report = f"""# Mooring-system resilience screening curves

Reduced-order resilience screening for a spread-moored unit, composing the
existing **fpso_mooring_full** (peak line tension) and **anchor_capacity**
(holding capacity) atlases with a closed-form DNV chain fatigue check.
Generated by `run_resilience_curves.py` (digitalmodel #974).

> Screening tool only. A full mooring analysis (OrcaFlex) remains the document
> of record. Out-of-range atlas queries are escalated, never extrapolated.

## Checks & safety factors

| Check | Source | Safety factor | Basis |
|---|---|---|---|
| Intact tension | fpso_mooring_full atlas | {F.fos_intact} | API RP 2SK ASD intact |
| One-line damaged | intact x {F.redistribution} redistribution | {F.fos_damaged} | API RP 2SK ASD damaged |
| Foundation | anchor_capacity atlas | {F.fos_foundation} | DNV-OS-E301 geotechnical |
| Fatigue | DNV studless chain T-N (m={F.sn_m}) | margin vs {F.design_life_years:.0f} y | dynamic range ~ {F.dyn_tension_per_Hs_kN} kN/m Hs |

Traffic-light: **GREEN** all util < 0.8 and fatigue margin > 2; **AMBER** util
0.8-1.0 or margin 1-2; **RED** util > 1.0 or margin < 1; **ESCALATE** outside
atlas coverage.

## Baseline unit

12-line spread, 120 mm studless chain (MBL {BASE.mbl_kN:.0f} kN, area
{BASE.line_area_mm2:.0f} mm^2), {BASE.water_depth_m:.0f} m water depth, 5 m x 20 m
suction anchor in su={BASE.soil_su_kpa:.0f} kPa clay, Hs=3 m / Tp={BASE_TP:.0f} s.

Result: intact util **{b.util_intact:.2f}**, damaged util **{b.util_damaged:.2f}**,
foundation util **{b.util_foundation:.2f}**, fatigue life **{b.fatigue_life_years:.0f} y**
(margin {b.fatigue_margin:.1f}) -> **{b.light}**.

## Curves

{imgs}

## Note on atlas sensitivity

In the current `fpso_mooring_full` atlas, peak line tension is dominated by
**water depth** (~5.5 -> 8.7 MN over 500-1500 m) and is only weakly sensitive to
Hs. Extreme-weather severity therefore drives **fatigue** here (via the dynamic
tension range), while depth drives the **strength/foundation** utilizations. A
higher-fidelity tension atlas would sharpen the Hs dependence of the ULS checks.
"""
    p = OUT / "README.md"
    p.write_text(report)
    return p


def main() -> None:
    paths = [curve_vs_Hs(), curve_intact_vs_damaged(),
             curve_foundation_vs_soil(), resilience_map()]
    report = write_report(paths)
    print("Wrote:")
    for p in paths + [report]:
        print(f"  {p}")


if __name__ == "__main__":
    main()
