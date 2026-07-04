#!/usr/bin/env python3
"""Floating-body heave free decay (#1169): equilibrium draft + heave period.

Body: cuboid 0.3 x 0.2 x 0.5, rho=500 -> m=15 kg, Awp=0.06 m^2, draft 0.25 m.
Closed 1x1 m tank: water level after release settles by volume conservation;
effective heave stiffness k_eff = rho g Awp / (1 - Awp/Atank).
Gates: equilibrium draft vs Archimedes within 5%; damped decay envelope;
measured period vs hydrostatic period documents the implied added mass.
"""
import json
import math
import re
import sys
from pathlib import Path

import numpy as np

CASE = Path(sys.argv[1])
OUT = Path(sys.argv[2]) if len(sys.argv) > 2 else CASE
OUT.mkdir(parents=True, exist_ok=True)

RHO, G = 998.2, 9.81            # water density per transportProperties (check)
LX, LY, LZ, MASS = 0.3, 0.2, 0.5, 15.0
AWP, ATANK = LX * LY, 1.0 * 1.0
Z0_BOTTOM = 0.233333            # initial body bottom
H0 = 0.5368                     # initial fill level (setFields)

# ---- body state: FO file, else parse the solver log --------------------------
state = sorted(CASE.glob("postProcessing/bodyState/0/*.dat"))
if state:
    raw = np.loadtxt(state[0], comments="#", dtype=str)
    t = raw[:, 0].astype(float)
    # columns: time (cx cy cz) ... -> strip parentheses
    cz = np.array([float(str(v).strip("()")) for v in raw[:, 3]])
else:
    t, cz = [], []
    log = (CASE / "log.interFoam").read_text().splitlines()
    tc = None
    for line in log:
        if line.startswith("Time = "):
            tc = float(line.split()[-1].rstrip("s"))
        mm = re.search(r"Centre of mass: \(([\d.eE+-]+) ([\d.eE+-]+) ([\d.eE+-]+)\)", line)
        if mm and tc is not None:
            if t and t[-1] == tc:
                cz[-1] = float(mm.group(3))   # keep last outer-corrector value
            else:
                t.append(tc); cz.append(float(mm.group(3)))
    t, cz = np.array(t), np.array(cz)

# ---- Archimedes equilibrium in the closed tank --------------------------------
draft_eq = MASS / (RHO * AWP)                       # 0.2505 m
# initial water volume: fill to H0 minus the submerged part of the hole
v_w = ATANK * H0 - AWP * (H0 - Z0_BOTTOM)
# at equilibrium: v_w = h*Atank - Awp*draft
h_eq = (v_w + AWP * draft_eq) / ATANK
z_cm_eq_th = h_eq - draft_eq + LZ / 2               # expected CoM height

# ---- measured equilibrium + decay ---------------------------------------------
tail = t >= t[-1] - 2.0
z_eq_meas = float(np.mean(cz[tail]))
draft_meas = float(h_eq - (z_eq_meas - LZ / 2))     # using conserved water level
draft_err = (draft_meas - draft_eq) / draft_eq

z = cz - z_eq_meas
# peaks of |z| for the decay envelope; period from zero-upcrossings
zc_idx = np.where((z[:-1] < 0) & (z[1:] >= 0))[0]
zc = np.array([t[i] - z[i] * (t[i+1] - t[i]) / (z[i+1] - z[i]) for i in zc_idx])
zc = zc[zc > 0.3]                                   # skip release transient
periods = np.diff(zc)
T_meas = float(np.mean(periods)) if len(periods) else float("nan")

peaks = []
for a, b in zip(zc[:-1], zc[1:]):
    seg = z[(t >= a) & (t < b)]
    if len(seg) > 3:
        peaks.append(float(np.max(np.abs(seg))))
monotone = all(p2 <= p1 * 1.05 for p1, p2 in zip(peaks[:-1], peaks[1:]))
log_dec = (math.log(peaks[0] / peaks[-1]) / (len(peaks) - 1)) if len(peaks) > 2 else None

# ---- hydrostatic period + implied added mass ----------------------------------
k_eff = RHO * G * AWP / (1 - AWP / ATANK)
T_hyd = 2 * math.pi * math.sqrt(MASS / k_eff)
m_total = k_eff * (T_meas / (2 * math.pi)) ** 2
ca = (m_total - MASS) / (RHO * AWP * draft_eq)      # added mass / displaced mass

res = {
    "mass": MASS, "awp": AWP, "k_eff": k_eff,
    "draft_theory": draft_eq, "draft_measured": draft_meas,
    "draft_err": draft_err,
    "z_cm_eq_theory": z_cm_eq_th, "z_cm_eq_measured": z_eq_meas,
    "T_hydrostatic": T_hyd, "T_measured": T_meas,
    "T_ratio": T_meas / T_hyd,
    "implied_added_mass_coeff": ca,
    "n_cycles": int(len(periods)),
    "peaks": peaks, "log_decrement": log_dec, "monotone_decay": bool(monotone),
    "decay": {"t": t[::4].tolist(), "z_cm": cz[::4].tolist()},
    "gates": {"draft_within_5pct": abs(draft_err) <= 0.05,
              "monotone_damped_decay": bool(monotone),
              "period_in_added_mass_band": 1.0 <= T_meas / T_hyd <= 1.6,
              "implied_Ca_physical": 0.1 <= ca <= 1.5},
}
(OUT / "results.json").write_text(json.dumps(res, indent=2))
print(f"draft: {draft_meas:.4f} vs {draft_eq:.4f} ({100*draft_err:+.1f}%)")
print(f"z_cm eq: {z_eq_meas:.4f} vs {z_cm_eq_th:.4f}")
print(f"T: {T_meas:.3f}s vs hydrostatic {T_hyd:.3f}s (ratio {T_meas/T_hyd:.2f}, Ca~{ca:.2f}) over {len(periods)} cycles")
print(f"peaks: {[round(p,4) for p in peaks]}")
print(json.dumps(res["gates"], indent=2))
