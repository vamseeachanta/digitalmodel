"""NACA0012 polar: lift-curve slope vs thin-airfoil theory + drag polar."""
import json, sys
from pathlib import Path
import numpy as np

CASE = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).parent
OUT = CASE / "naca_results"; OUT.mkdir(exist_ok=True)

d = np.genfromtxt(CASE / "polar.csv", delimiter=",", names=True)
alpha, cl, cd = d["alpha"], d["Cl"], d["Cd"]

# lift-curve slope from a linear least-squares fit over the linear regime (<= 6 deg,
# below stall onset). Slope is independent of the mesh's built-in incidence offset.
lin = alpha <= 6.0
p = np.polyfit(alpha[lin], cl[lin], 1)      # p[0] = dCl/dalpha per degree
slope_deg = float(p[0]); slope_rad = slope_deg * 180 / np.pi
intercept = float(p[1])
alpha0 = -intercept / slope_deg             # effective zero-lift angle (built-in incidence)

THEORY_DEG = 2 * np.pi * (np.pi / 180)       # 2*pi/rad -> per deg = 0.10966
EXP_DEG = 0.106                              # experimental NACA0012 (Abbott & von Doenhoff)

res = {
    "alpha": alpha.tolist(), "cl": cl.tolist(), "cd": cd.tolist(),
    "slope_per_deg": slope_deg, "slope_per_rad": slope_rad,
    "theory_per_deg": THEORY_DEG, "theory_per_rad": 2*np.pi,
    "exp_per_deg": EXP_DEG,
    "slope_err_vs_theory_pct": 100*(slope_deg-THEORY_DEG)/THEORY_DEG,
    "slope_err_vs_exp_pct": 100*(slope_deg-EXP_DEG)/EXP_DEG,
    "built_in_incidence_deg": float(alpha0),
    "re": 26.003*35.05/1e-5, "chord": 35.05, "n_cells": 10720,
}
(OUT / "results.json").write_text(json.dumps(res, indent=2))

import matplotlib; matplotlib.use("Agg"); import matplotlib.pyplot as plt
# Cl vs alpha
fig, ax = plt.subplots(figsize=(7, 4.5))
ax.plot(alpha, cl, "o", ms=5, color="#c0392b", label="OpenFOAM (Spalart-Allmaras)")
af = np.linspace(alpha.min(), alpha.max(), 50)
ax.plot(af, np.polyval(p, af), "-", color="#0b6e99", lw=1.6,
        label=f"linear fit: {slope_deg:.4f}/deg")
ax.plot(af, THEORY_DEG*(af-alpha0), "k--", lw=1.3,
        label=f"thin-airfoil 2$\\pi$/rad = {THEORY_DEG:.4f}/deg")
ax.axvspan(6, alpha.max(), color="#b06f00", alpha=0.07, label="stall onset (excluded)")
ax.set_xlabel(r"angle of attack $\alpha$ (deg)"); ax.set_ylabel(r"lift coefficient $C_l$")
ax.set_title("NACA0012 lift curve vs thin-airfoil theory")
ax.legend(fontsize=8); ax.grid(alpha=0.3)
fig.tight_layout(); fig.savefig(OUT / "lift_curve.png", dpi=130); plt.close(fig)
# drag polar
fig, ax = plt.subplots(figsize=(6, 4.5))
ax.plot(cd, cl, "o-", ms=4, color="#1a7a3f")
for a, x, y in zip(alpha, cd, cl):
    if a % 2 == 0: ax.annotate(f"{a:.0f}°", (x, y), fontsize=7, xytext=(4,-2), textcoords="offset points")
ax.set_xlabel(r"drag coefficient $C_d$"); ax.set_ylabel(r"lift coefficient $C_l$")
ax.set_title("NACA0012 drag polar"); ax.grid(alpha=0.3)
fig.tight_layout(); fig.savefig(OUT / "drag_polar.png", dpi=130); plt.close(fig)

print(f"slope = {slope_deg:.4f}/deg ({slope_rad:.3f}/rad)")
print(f"  vs thin-airfoil 2pi/rad ({THEORY_DEG:.4f}/deg): {res['slope_err_vs_theory_pct']:+.1f}%")
print(f"  vs experiment (~0.106/deg): {res['slope_err_vs_exp_pct']:+.1f}%")
print(f"  built-in incidence (zero-lift angle): {alpha0:.2f} deg;  Re={res['re']:.2e}")
for a, l, c in zip(alpha, cl, cd):
    print(f"  a={a:.0f}  Cl={l:.4f}  Cd={c:.5f}")
