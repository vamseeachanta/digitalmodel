"""Velocity-magnitude field around the NACA0012 at the last (8 deg) solution."""
from pathlib import Path
import numpy as np, pyvista as pv
import matplotlib; matplotlib.use("Agg"); import matplotlib.pyplot as plt

CASE = Path("/tmp/claude-1000/-mnt-local-analysis/86727272-c4c1-41a9-aab1-739eec70e02d/scratchpad/naca")
OUT = CASE / "naca_results"; OUT.mkdir(exist_ok=True)
UINF = 26.003

(CASE / "n.foam").write_text("")
r = pv.OpenFOAMReader(str(CASE / "n.foam")); r.set_active_time_value(r.time_values[-1])
r.enable_all_patch_arrays(); mesh = r.read(); internal = mesh["internalMesh"]
cc = internal.cell_centers().points
U = internal.cell_data["U"]; umag = np.linalg.norm(U, axis=1) / UINF
x, y = cc[:, 0], cc[:, 1]
sel = (x > -25) & (x < 35) & (np.abs(y) < 22)
xs, ys, us = x[sel], y[sel], umag[sel]

# airfoil outline from the walls patch
wall = mesh["boundary"]["walls"]; wp = wall.points

fig, ax = plt.subplots(figsize=(8.2, 4.6))
lv = np.linspace(0, 1.6, 24)
tcf = ax.tricontourf(xs, ys, np.clip(us, 0, 1.6), levels=lv, cmap="turbo", extend="max")
ax.plot(wp[:, 0], wp[:, 1], ".", ms=0.6, color="k")
ax.set_aspect("equal"); ax.set_xlim(-25, 35); ax.set_ylim(-22, 22)
ax.set_xlabel("x"); ax.set_ylabel("y")
ax.set_title(r"NACA0012 at $\alpha$=8°: velocity magnitude $|U|/U_\infty$ (Re≈9×10⁷)")
cb = fig.colorbar(tcf, ax=ax, shrink=0.85, pad=0.02); cb.set_label(r"$|U|/U_\infty$")
fig.tight_layout(); fig.savefig(OUT / "airfoil_field.png", dpi=130); plt.close(fig)
print("wrote", OUT / "airfoil_field.png", "cells:", int(sel.sum()))
