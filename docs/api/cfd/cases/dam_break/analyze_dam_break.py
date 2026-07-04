#!/usr/bin/env python3
"""Post-process the dam-break case and compare to Martin & Moyce (1952).

Computes the dimensionless surge-front history Z(T) and residual column
height, checks mass conservation from the waterVolume functionObject, and
writes results.json + verification PNGs.

Usage: uv run python analyze_dam_break.py <case_dir> [<out_dir>]

Reference: Martin, J.C. & Moyce, W.J. (1952), Phil. Trans. R. Soc. Lond. A
244(882) 312-324, doi:10.1098/rsta.1952.0006 (n^2=2 column). Digitized
surge-front data + the standard +0.175 gate-release time shift follow the
Lethe project (chaos-polymtl/lethe, examples/multiphysics/dam-break).
"""
import json
import math
import sys
from pathlib import Path

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import pyvista as pv

CASE = Path(sys.argv[1])
OUT = Path(sys.argv[2]) if len(sys.argv) > 2 else CASE
OUT.mkdir(parents=True, exist_ok=True)

A = 0.1461                          # column base width a (m)
G = 9.81
TSCALE = math.sqrt(2.0 * G / A)     # T = t * sqrt(2g/a)
GATE_SHIFT = 0.175                  # experimental gate-release correction

# Martin & Moyce (1952) n^2=2 surge front, digitized (Lethe project).
T_EXP = [0.41, 0.84, 1.19, 1.43, 1.63, 1.82, 1.97, 2.2, 2.32, 2.5, 2.64, 2.82, 2.96]
Z_EXP = [1.11, 1.23, 1.44, 1.67, 1.89, 2.11, 2.33, 2.56, 2.78, 3.0, 3.22, 3.44, 3.67]

# --- read the solved fields ------------------------------------------------
(CASE / "case.foam").write_text("")
reader = pv.OpenFOAMReader(str(CASE / "case.foam"))
mesh0 = reader.read()["internalMesh"]
n_cells = mesh0.n_cells
# Tank is 8a x 4a with a uniform mesh, so nx = 2*ny and nx*ny = n_cells.
ny = int(round(math.sqrt(n_cells / 2)))
nx = 2 * ny
DX = 8 * A / nx

T_cfd, Z_cfd, H_cfd = [], [], []
for t in reader.time_values:
    reader.set_active_time_value(t)
    cc = reader.read()["internalMesh"].cell_centers()
    alpha = np.asarray(cc.cell_data["alpha.water"])
    pts = cc.points
    wet = alpha >= 0.5
    sel = wet & (pts[:, 1] < DX)          # bottom row -> surge front
    x_front = pts[sel, 0].max() + DX / 2 if sel.any() else 0.0
    sel_h = wet & (pts[:, 0] < DX)        # left column -> residual height
    h_col = pts[sel_h, 1].max() + DX / 2 if sel_h.any() else 0.0
    T_cfd.append(t * TSCALE)
    Z_cfd.append(x_front / A)
    H_cfd.append(h_col / (2 * A))
T_cfd, Z_cfd, H_cfd = map(np.asarray, (T_cfd, Z_cfd, H_cfd))

# --- deviation vs experiment ------------------------------------------------
def deviation(shift):
    devs = [abs(np.interp(te, T_cfd + shift, Z_cfd) - ze) / ze
            for te, ze in zip(T_EXP, Z_EXP)]
    return float(np.mean(devs)), float(np.max(devs))

mean_raw, max_raw = deviation(0.0)
mean_cor, max_cor = deviation(GATE_SHIFT)

# --- mass conservation --------------------------------------------------------
mass = np.loadtxt(CASE / "postProcessing/waterVolume/0/volFieldValue.dat",
                  comments="#")
v0 = mass[0, 1]
drift = float(np.max(np.abs(mass[:, 1] - v0)) / v0)

results = {
    "column_width_m": A, "mesh_cells": int(n_cells),
    "front_dev_mean_gate_corrected": mean_cor,
    "front_dev_max_gate_corrected": max_cor,
    "front_dev_mean_raw": mean_raw, "front_dev_max_raw": max_raw,
    "mass_drift_rel": drift, "water_volume_m3": float(v0),
    "gate_release_shift": GATE_SHIFT,
    "gates": {"front_mean_corrected<=0.06": mean_cor <= 0.06,
              "front_max_corrected<=0.10": max_cor <= 0.10,
              "mass_drift<=0.01": drift <= 0.01},
}
(OUT / "results.json").write_text(json.dumps(results, indent=2) + "\n")
print(json.dumps(results["gates"], indent=2))
print(f"front dev corrected: mean {mean_cor:.1%} max {max_cor:.1%}; "
      f"raw mean {mean_raw:.1%}; mass drift {drift:.2e}")

# --- plots -------------------------------------------------------------------
plt.rcParams.update({"font.size": 11, "axes.grid": True, "grid.alpha": 0.3})

fig, ax = plt.subplots(figsize=(7.2, 5))
ax.plot(T_cfd, Z_cfd, "-", color="#1f6feb", lw=2, label="interFoam (raw)")
ax.plot(T_cfd + GATE_SHIFT, Z_cfd, "--", color="#1f6feb", lw=1.6, alpha=0.75,
        label=f"interFoam (+{GATE_SHIFT} gate-release shift)")
ax.plot(T_EXP, Z_EXP, "o", color="#d1242f", ms=7, mfc="none", mew=2,
        label="Martin & Moyce (1952), $n^2$=2")
ax.set_xlabel(r"$T = t\,\sqrt{2g/a}$"); ax.set_ylabel(r"$Z = x_{front}/a$")
ax.set_xlim(0, 3.4); ax.set_ylim(0.9, 4.2); ax.legend(loc="upper left")
ax.set_title("Dam-break surge front vs Martin & Moyce (1952)")
fig.tight_layout(); fig.savefig(OUT / "front.png", dpi=140)

fig, ax = plt.subplots(figsize=(7.2, 4.2))
ax.plot(T_cfd, H_cfd, "-", color="#1a7f37", lw=2)
ax.set_xlabel(r"$T$"); ax.set_ylabel(r"$H = h/(2a)$")
ax.set_xlim(0, float(T_cfd.max())); ax.set_ylim(0, 1.05)
ax.set_title("Residual column height at the left wall")
fig.tight_layout(); fig.savefig(OUT / "height.png", dpi=140)

fig, ax = plt.subplots(figsize=(7.2, 3.4))
ax.plot(mass[:, 0] * TSCALE, 100 * (mass[:, 1] - v0) / v0, "-",
        color="#9a6700", lw=1.5)
ax.set_xlabel(r"$T$"); ax.set_ylabel("water volume error (%)")
ax.set_title("Mass conservation (volIntegrate alpha.water)")
fig.tight_layout(); fig.savefig(OUT / "mass.png", dpi=140)

snap_times = [t for t in (0.05, 0.15, 0.25, 0.4) if t <= reader.time_values[-1]]
fig, axes = plt.subplots(len(snap_times), 1, figsize=(8.2, 2.3 * len(snap_times)))
for ax, t in zip(np.atleast_1d(axes), snap_times):
    reader.set_active_time_value(t)
    cc = reader.read()["internalMesh"].cell_centers()
    alpha = np.asarray(cc.cell_data["alpha.water"]).reshape(ny, nx)
    ax.imshow(alpha, origin="lower", extent=(0, 8 * A, 0, 4 * A),
              cmap="Blues", vmin=0, vmax=1, aspect="equal")
    ax.set_title(f"t = {t:.2f} s   (T = {t * TSCALE:.2f})", fontsize=10)
    ax.set_xticks([]); ax.set_yticks([]); ax.grid(False)
fig.suptitle("alpha.water — column collapse and surge", y=0.995)
fig.tight_layout(); fig.savefig(OUT / "snapshots.png", dpi=130)
print(f"wrote plots to {OUT}")
