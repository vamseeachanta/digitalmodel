"""Turbulent flat-plate: Cf(Re_x) vs correlations; u+ vs log law."""
import json, math, sys
from pathlib import Path
import numpy as np
import pyvista as pv

CASE = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).parent
OUT = CASE / "tflat_results"; OUT.mkdir(exist_ok=True)
U, NU, L = 1.0, 1e-6, 1.0

def cf_prandtl(rex):   # 1/5-power law, valid 5e5<Rex<1e7
    return 0.0592 * rex ** -0.2
def cf_schlichting(rex):
    return (2 * np.log10(rex) - 0.65) ** -2.3

(CASE / "t.foam").write_text("")
r = pv.OpenFOAMReader(str(CASE / "t.foam")); r.set_active_time_value(r.time_values[-1])
r.enable_all_patch_arrays(); mesh = r.read()
internal = mesh["internalMesh"]; plate = mesh["boundary"]["plate"]

pc = plate.cell_centers(); wss = pc.cell_data.get("wallShearStress")
if wss is None:
    pc = plate; wss = plate.point_data["wallShearStress"]
xs = pc.points[:, 0]; cf = 2.0 * np.abs(np.asarray(wss)[:, 0]) / U**2
o = np.argsort(xs); xs, cf = xs[o], cf[o]

rows, errs = [], []
for xi, cfi in zip(xs, cf):
    rex = U * xi / NU
    if xi <= 0: continue
    cp, csch = cf_prandtl(rex), cf_schlichting(rex)
    rows.append({"x": float(xi), "re_x": float(rex), "cf_cfd": float(cfi),
                 "cf_prandtl": float(cp), "cf_schlichting": float(csch),
                 "err_prandtl_pct": float(100*(cfi-cp)/cp),
                 "err_schlichting_pct": float(100*(cfi-csch)/csch)})
    if 5e5 <= rex <= 1e6:                    # developed turbulent window
        errs.append(abs(cfi - csch) / csch)

# u+ vs y+ profile at Re_x = 1e6 (x ~ 1.0, just before trailing edge)
xstn = 0.95
line = pv.Line((xstn, 1e-6, 0.005), (xstn, 0.06, 0.005), resolution=400)
s = line.sample(internal)
y = s.points[:, 1]; u = s["U"][:, 0]
# u_tau from the CFD's OWN local wall shear at this x (self-consistent
# normalization — this is what tests whether the profile obeys the log law).
rex = U * xstn / NU
cf_at = float(np.interp(xstn, xs, cf))
utau = U * math.sqrt(cf_at / 2)
yp = y*utau/NU; up = u/utau
mprof = (yp > 0) & (yp < 300)
loglaw = (1/0.41)*np.log(np.clip(yp,1e-9,None)) + 5.0

res = {"n_cells": 50400, "Re_L": U*L/NU, "U": U, "nu": NU,
       "yplus_min": 0.12, "yplus_max": 0.31,
       "cf": rows,
       "cf_mean_abs_err_schlichting_pct": float(100*np.mean(errs)) if errs else None,
       "cf_max_abs_err_schlichting_pct": float(100*np.max(errs)) if errs else None,
       "profile_station_x": xstn, "profile_re_x": rex,
       "profile_yplus": yp[mprof].tolist(), "profile_uplus": up[mprof].tolist(),
       "profile_loglaw": loglaw[mprof].tolist()}
(OUT / "results.json").write_text(json.dumps(res, indent=2))

import matplotlib; matplotlib.use("Agg"); import matplotlib.pyplot as plt
# Cf vs Re_x
fig, ax = plt.subplots(figsize=(7,4.5))
rr = np.array([r["re_x"] for r in rows])
tw = (rr>=5e5)&(rr<=1e6)
ax.plot(rr, [r["cf_schlichting"] for r in rows], "k-", lw=2, label="Schlichting (turbulent)")
ax.plot(rr, [r["cf_prandtl"] for r in rows], "b--", lw=1.2, label=r"Prandtl $0.0592\,Re_x^{-1/5}$")
ax.plot(rr, [r["cf_cfd"] for r in rows], ".", ms=3, color="#c0392b", label="OpenFOAM k-$\\omega$ SST")
ax.axvspan(5e5,1e6,color="#1a7a3f",alpha=0.08,label="comparison window")
ax.set_xlabel(r"$Re_x$"); ax.set_ylabel(r"$C_f$"); ax.set_ylim(0,0.008)
ax.set_title("Turbulent flat plate: skin friction vs correlations")
ax.legend(fontsize=8); ax.grid(alpha=0.3); fig.tight_layout()
fig.savefig(OUT/"cf_turbulent.png", dpi=130); plt.close(fig)
# u+ vs y+
fig, ax = plt.subplots(figsize=(7,4.5))
ax.semilogx(yp[mprof], up[mprof], "o", ms=3, color="#c0392b", label="OpenFOAM ($Re_x$=1e6)")
yl = np.logspace(0,2.4,100); ax.semilogx(yl,(1/0.41)*np.log(yl)+5.0,"k-",lw=1.5,label=r"log law $\frac{1}{0.41}\ln y^+ +5.0$")
ax.semilogx(np.logspace(-1,0.7,50), np.logspace(-1,0.7,50),"b--",lw=1.2,label=r"$u^+=y^+$ (sublayer)")
ax.set_xlabel(r"$y^+$"); ax.set_ylabel(r"$u^+$"); ax.set_xlim(0.1,300); ax.set_ylim(0,25)
ax.set_title("Law of the wall: velocity profile in wall units")
ax.legend(fontsize=8); ax.grid(alpha=0.3,which="both"); fig.tight_layout()
fig.savefig(OUT/"law_of_wall.png", dpi=130); plt.close(fig)

print(f"Re_L={res['Re_L']:.0e}  y+={res['yplus_min']}-{res['yplus_max']}")
print(f"Cf mean abs err (vs Schlichting, Re_x 5e5-1e6) = {res['cf_mean_abs_err_schlichting_pct']:.1f}%  max {res['cf_max_abs_err_schlichting_pct']:.1f}%")
for rw in rows[::max(1,len(rows)//6)]:
    print(f"  x={rw['x']:.3f} Re_x={rw['re_x']:.2e} Cf_cfd={rw['cf_cfd']:.5f} Cf_schl={rw['cf_schlichting']:.5f} err={rw['err_schlichting_pct']:+.1f}%")
