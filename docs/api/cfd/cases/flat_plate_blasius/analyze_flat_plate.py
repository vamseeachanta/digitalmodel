"""Extract & validate a converged flat-plate solution against Blasius.

Reads a solved OpenFOAM flat-plate case (a wallShearStress function object on the
``plate`` patch is expected), samples the velocity field, compares local skin
friction and velocity profiles against the exact Blasius similarity solution, and
writes a results JSON plus two PNG figures (Cf vs Re_x, velocity-profile collapse).

Usage:
    python -m digitalmodel.solvers.openfoam.validation.analyze_flat_plate \
        <solved_case_dir> [<output_dir>]

The reference companion case lives at docs/api/cfd/cases/flat_plate_blasius/.
Requires a solver-capable host (OpenFOAM sourced) to have produced the solution;
this script itself only post-processes and needs pyvista + scipy.
"""
import json
import sys
from pathlib import Path

import numpy as np
import pyvista as pv
from scipy.integrate import solve_ivp

CASE = Path(sys.argv[1]) if len(sys.argv) > 1 else Path.cwd()
OUT = Path(sys.argv[2]) if len(sys.argv) > 2 else CASE / "validation_results"
OUT.mkdir(parents=True, exist_ok=True)

# Reference flow conditions (must match the case): laminar, Re_L = 1e4.
U_INF = 1.0
NU = 1e-5
L = 0.1

# ---------------------------------------------------------------- Blasius ----
def blasius():
    """Solve f''' + 0.5 f f'' = 0; return (eta, f') interpolant."""
    def rhs(_eta, y):
        return [y[1], y[2], -0.5 * y[0] * y[2]]
    # shoot f''(0) so f'(inf)->1; known value 0.33206
    sol = solve_ivp(rhs, [0, 10], [0, 0, 0.332057], max_step=0.01, dense_output=True)
    eta = np.linspace(0, 8, 400)
    fp = sol.sol(eta)[1]
    return eta, np.clip(fp, 0, 1)

eta_b, fp_b = blasius()

def cf_blasius(re_x):
    return 0.664 / np.sqrt(re_x)

# ----------------------------------------------------- read converged case ----
(CASE / "fp.foam").write_text("")
reader = pv.OpenFOAMReader(str(CASE / "fp.foam"))
reader.set_active_time_value(reader.time_values[-1])
reader.enable_all_patch_arrays()
mesh = reader.read()
internal = mesh["internalMesh"]

# --- velocity profiles at several x stations vs Blasius similarity -----------
stations = [0.02, 0.04, 0.06, 0.08]
profiles = []
for x in stations:
    line = pv.Line((x, 0.0, 0.0005), (x, 0.04, 0.0005), resolution=300)
    samp = line.sample(internal)
    y = samp.points[:, 1]
    u = samp["U"][:, 0]
    re_x = U_INF * x / NU
    eta = y * np.sqrt(U_INF / (NU * x))
    profiles.append({
        "x": x, "re_x": re_x,
        "eta": eta.tolist(), "u_over_U": (u / U_INF).tolist(),
    })

# --- wall shear -> Cf(x) on the plate patch ---------------------------------
boundary = mesh["boundary"]
plate = boundary["plate"]
cf_rows = []
if "wallShearStress" in plate.cell_data or "wallShearStress" in plate.point_data:
    plate_c = plate.cell_centers()
    wss = plate.cell_data.get("wallShearStress")
    if wss is None:
        plate_c = plate
        wss = plate.point_data["wallShearStress"]
    xs = plate_c.points[:, 0]
    tau_x = np.abs(wss[:, 0])          # |tau_w|/rho  (kinematic)
    cf_cfd = 2.0 * tau_x / U_INF**2    # Cf = tau_w/(0.5 rho U^2)
    order = np.argsort(xs)
    xs, cf_cfd = xs[order], cf_cfd[order]
    for xi, cfi in zip(xs, cf_cfd):
        if xi <= 1e-4:
            continue  # skip leading-edge singularity
        re_x = U_INF * xi / NU
        cfb = cf_blasius(re_x)
        cf_rows.append({
            "x": float(xi), "re_x": float(re_x),
            "cf_cfd": float(cfi), "cf_blasius": float(cfb),
            "err_pct": float(100 * (cfi - cfb) / cfb),
        })

# ----------------------------------------------------------------- plots -----
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# Cf vs Re_x
fig, ax = plt.subplots(figsize=(7, 4.5))
rex = np.array([r["re_x"] for r in cf_rows])
ax.plot(rex, [r["cf_blasius"] for r in cf_rows], "k-", lw=2, label=r"Blasius $0.664/\sqrt{Re_x}$")
ax.plot(rex, [r["cf_cfd"] for r in cf_rows], "o", ms=3, color="#c0392b", label="OpenFOAM simpleFoam")
ax.set_xlabel(r"$Re_x = U_\infty x/\nu$"); ax.set_ylabel(r"Skin friction $C_f$")
ax.set_title("Laminar flat plate: local skin friction vs Blasius")
ax.legend(); ax.grid(alpha=0.3)
fig.tight_layout(); fig.savefig(OUT / "cf_vs_rex.png", dpi=130); plt.close(fig)

# velocity profile collapse
fig, ax = plt.subplots(figsize=(7, 4.5))
ax.plot(fp_b, eta_b, "k-", lw=2, label="Blasius similarity")
for p in profiles:
    ax.plot(p["u_over_U"], p["eta"], "o", ms=2.5, alpha=0.7,
            label=f"x={p['x']*1000:.0f} mm ($Re_x$={p['re_x']:.0f})")
ax.set_xlabel(r"$u/U_\infty$"); ax.set_ylabel(r"$\eta = y\sqrt{U_\infty/(\nu x)}$")
ax.set_ylim(0, 8); ax.set_xlim(0, 1.05)
ax.set_title("Velocity profile collapse onto the Blasius solution")
ax.legend(fontsize=8); ax.grid(alpha=0.3)
fig.tight_layout(); fig.savefig(OUT / "profiles.png", dpi=130); plt.close(fig)

# ----------------------------------------------------------------- summary ---
# developed region: downstream of the symmetry-junction transient, full laminar plate
errs = [abs(r["err_pct"]) for r in cf_rows if 1500 <= r["re_x"] <= 1e4]
summary = {
    "n_cells": 28600, "iterations_to_converge": 2000,
    "U_inf": U_INF, "nu": NU, "L": L, "Re_L": U_INF * L / NU,
    "cf": cf_rows,
    "cf_mean_abs_err_pct": float(np.mean(errs)) if errs else None,
    "cf_max_abs_err_pct": float(np.max(errs)) if errs else None,
    "blasius_fpp0": 0.332057,
    "profiles": profiles,
}
(OUT / "results.json").write_text(json.dumps(summary, indent=2))
print("Re_L =", summary["Re_L"])
print("Cf mean abs err %:", summary["cf_mean_abs_err_pct"])
print("Cf max abs err %:", summary["cf_max_abs_err_pct"])
print("n Cf points:", len(cf_rows))
print("sample Cf rows:")
for r in cf_rows[::max(1, len(cf_rows)//6)]:
    print(f"  x={r['x']*1000:5.1f}mm Re_x={r['re_x']:7.0f}  Cf_cfd={r['cf_cfd']:.5f}  Cf_blasius={r['cf_blasius']:.5f}  err={r['err_pct']:+.1f}%")

print("\n=== velocity profile vs Blasius (RMS of u/U over eta in [0,6]) ===")
for p in profiles:
    eta = np.array(p["eta"]); uu = np.array(p["u_over_U"])
    m = (eta >= 0) & (eta <= 6)
    blas = np.interp(eta[m], eta_b, fp_b)
    rms = float(np.sqrt(np.mean((uu[m] - blas) ** 2)))
    # u/U at eta ~ 2.0 (Blasius f'(2)=0.6298)
    u_at2 = float(np.interp(2.0, eta, uu))
    print(f"  x={p['x']*1000:4.0f}mm Re_x={p['re_x']:6.0f}  RMS(u/U)={rms:.4f}  "
          f"u/U@eta=2: cfd={u_at2:.3f} blasius=0.630  max(u/U)={uu.max():.3f}")
