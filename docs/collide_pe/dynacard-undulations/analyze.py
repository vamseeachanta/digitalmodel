"""Rod-pump surface dynacard analysis for the Collide 'Dynamometer Discussions' post.

Inputs (from the post):
  stroke S      = 41 in        pump depth L = 4300 ft
  speed  N      = 6.4 SPM      plunger d    = 1.25 in (top-hold-down insert)
  prime mover   = C-66         tubing head  = builds to 150 psi (Baird valve)
  casing        = 25 psi       production   = 23 bopd, 0 bwpd

Card geometry is digitized from the posted screenshot (dynacard_digitized.csv).
"""
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# ------------------------------------------------------------------ inputs
S, L, N, D_PL = 41.0, 4300.0, 6.4, 1.25
Q_OIL, P_TBG, P_CSG = 23.0, 150.0, 25.0

d = np.genfromtxt("dynacard_digitized.csv", delimiter=",", names=True)
x, up, dn = d["position_in"], d["upstroke_load_lb"], d["downstroke_load_lb"]

# ------------------------------------------------------- 1. pump displacement
A_pl = np.pi / 4 * D_PL**2                       # in^2
PD = 0.1484 * A_pl * S * N                       # bbl/day, API pump constant
eff = Q_OIL / PD * 100
print(f"plunger area          = {A_pl:.4f} in^2")
print(f"pump displacement PD  = {PD:.1f} bfpd  (0.1484 * Ap * S * N)")
print(f"volumetric efficiency = {eff:.0f} %   (23 bopd actual)")

# --------------------------------------------- 2. rod-string natural frequency
C_STEEL = 16_300.0                               # ft/s, sonic velocity in steel rods
T_n = 4 * L / C_STEEL                            # s, fixed-free fundamental period
N0_API = 245_000 / L                             # SPM, API RP 11L natural frequency
T_n_API = 60 / N0_API
NN0 = N / N0_API
print(f"\nrod-string period T_n = {T_n:.3f} s   (4L/c, c={C_STEEL:,.0f} ft/s)")
print(f"      API N0          = {N0_API:.1f} SPM -> T_n = {T_n_API:.3f} s")
print(f"      N/N0            = {NN0:.3f}")
print(f"predicted undulations per half-stroke = 0.5/(N/N0) = {0.5/NN0:.2f}")

# --------------------------------------- 3. map card position -> time (crank SHM)
w = 2 * np.pi * N / 60                           # rad/s
T_cyc = 60 / N
t_up = np.arccos(np.clip(1 - 2 * x / S, -1, 1)) / w              # 0 -> T/2
t_dn = T_cyc - t_up                                              # T/2 -> T
print(f"\ncycle time            = {T_cyc:.3f} s  (upstroke {T_cyc/2:.3f} s)")
print(f"max polished-rod vel  = {S/2*w:.2f} in/s at mid-stroke")
print(f"distance covered in one T_n at mid-stroke = {S/2*w*T_n:.1f} in")


def peaks(t, y, kind="max", prom=60.0):
    """Local extrema with a minimum prominence, returned in scan order."""
    out = []
    for i in range(2, len(y) - 2):
        seg = y[max(0, i - 3):i + 4]
        if kind == "max" and y[i] >= seg.max() and y[i] - seg.min() > prom:
            out.append(i)
        if kind == "min" and y[i] <= seg.min() and seg.max() - y[i] > prom:
            out.append(i)
    ded = [out[0]] if out else []
    for i in out[1:]:
        if t[i] - t[ded[-1]] > 0.35:
            ded.append(i)
        elif (kind == "max" and y[i] > y[ded[-1]]) or (kind == "min" and y[i] < y[ded[-1]]):
            ded[-1] = i
    return ded


iu = peaks(t_up, up, "max")
print("\nupstroke load peaks   position(in)   time(s)   load(lb)")
for i in iu:
    print(f"                         {x[i]:5.1f}      {t_up[i]:5.2f}    {up[i]:8.0f}")
dt = np.diff(t_up[iu])
print(f"peak-to-peak spacing in TIME  : {np.round(dt,3)}  mean {dt.mean():.3f} s")
print(f"peak-to-peak spacing in STROKE: {np.round(np.diff(x[iu]),1)} in  <-- shrinks near top")

idn = peaks(t_dn, dn, "min")
print("\ndownstroke load valleys position(in)  time(s)   load(lb)")
for i in idn:
    print(f"                         {x[i]:5.1f}      {t_dn[i]:5.2f}    {dn[i]:8.0f}")

# ------------------------------------------------------------- 4. card metrics
PPRL, MPRL = up.max(), dn.min()
area = np.trapezoid(up - dn, x)                  # lb-in per stroke
hp_pr = area / 12 * N / 33_000
print(f"\nPPRL = {PPRL:,.0f} lb @ {x[up.argmax()]:.1f} in")
print(f"MPRL = {MPRL:,.0f} lb @ {x[dn.argmin()]:.1f} in")
print(f"load range  = {PPRL-MPRL:,.0f} lb")
print(f"card area   = {area:,.0f} lb-in = {area/12:,.0f} ft-lb/stroke")
print(f"polished-rod horsepower = {hp_pr:.2f} hp")
# counterbalance / gearbox torque proxy
print(f"C-66 unit: 57,000 in-lb gearbox rating; peak torque proxy "
      f"(PPRL-MPRL)/2 * S/2 = {(PPRL-MPRL)/2*S/2:,.0f} in-lb")

# fluid load on the plunger, for reference
Fo = A_pl * (P_TBG - P_CSG)
print(f"\nsurface-pressure fluid load Fo = Ap*(Ptbg-Pcsg) = {Fo:,.0f} lb "
      f"(net lift load is dominated by fluid column, not wellhead dP)")

# ================================================================== plots
plt.rcParams.update({"font.size": 9, "axes.grid": True,
                     "grid.alpha": .3, "figure.dpi": 150})
C_UP, C_DN, C_ACC = "#2A6FDB", "#D9822B", "#B5484B"

# ---- Fig 1: digitized card ------------------------------------------------
fig, ax = plt.subplots(figsize=(7.2, 4.4))
ax.plot(x, up, color=C_UP, lw=1.8, label="upstroke")
ax.plot(x, dn, color=C_DN, lw=1.8, label="downstroke")
ax.fill_between(x, dn, up, color=C_UP, alpha=.07)
ax.scatter(x[iu], up[iu], s=28, color=C_ACC, zorder=5)
for n, i in enumerate(iu, 1):
    ax.annotate(f"P{n}\n{t_up[i]:.2f}s", (x[i], up[i]), textcoords="offset points",
                xytext=(0, 9), ha="center", fontsize=7.5, color=C_ACC)
ax.axhline(PPRL, ls=":", lw=.9, color="#666")
ax.axhline(MPRL, ls=":", lw=.9, color="#666")
ax.text(41, PPRL, f" PPRL {PPRL:,.0f}", va="bottom", ha="right", fontsize=8, color="#444")
ax.text(41, MPRL, f" MPRL {MPRL:,.0f}", va="top", ha="right", fontsize=8, color="#444")
ax.set_xlabel("polished-rod position (in)")
ax.set_ylabel("polished-rod load (lb)")
ax.set_title("Fig 1 — Surface dynacard, digitized from posted screenshot\n"
             f"41 in stroke · 6.4 SPM · 4300 ft · 1-1/4 in insert pump", fontsize=10)
ax.legend(loc="lower left", framealpha=.9)
fig.tight_layout()
fig.savefig("fig1_card.png")

# ---- Fig 2: load vs TIME --------------------------------------------------
t_all = np.concatenate([t_up, t_dn[::-1]])
f_all = np.concatenate([up, dn[::-1]])
fig, ax = plt.subplots(figsize=(7.2, 4.0))
ax.plot(t_all, f_all, color="#333", lw=1.5)
for i in iu:
    ax.axvline(t_up[i], color=C_ACC, ls="--", lw=.9)
for k, i in enumerate(iu[:-1]):
    ax.annotate("", (t_up[iu[k]], 12550), (t_up[iu[k+1]], 12550),
                arrowprops=dict(arrowstyle="<->", color=C_ACC, lw=1))
    ax.text((t_up[iu[k]] + t_up[iu[k+1]]) / 2, 12610,
            f"{t_up[iu[k+1]]-t_up[iu[k]]:.2f}s", ha="center", fontsize=7.5, color=C_ACC)
ax.axvline(T_cyc / 2, color=C_UP, lw=1, alpha=.6)
ax.text(T_cyc / 2, 9350, " top of stroke", fontsize=8, color=C_UP)
ax.set_xlabel("time in pump cycle (s)")
ax.set_ylabel("polished-rod load (lb)")
ax.set_title("Fig 2 — Same card replotted against TIME\n"
             f"peaks are evenly spaced (~{dt.mean():.2f} s) — rod-string period "
             f"4L/c = {T_n:.2f} s", fontsize=10)
fig.tight_layout()
fig.savefig("fig2_load_vs_time.png")

# ---- Fig 3: why peaks crowd at the top of stroke --------------------------
fig, (a1, a2) = plt.subplots(2, 1, figsize=(7.2, 5.2), sharex=True,
                             gridspec_kw={"height_ratios": [2, 1]})
tt = np.linspace(0, T_cyc / 2, 400)
xx = S / 2 * (1 - np.cos(w * tt))
vv = S / 2 * w * np.sin(w * tt)
a1.plot(tt, xx, color=C_UP, lw=1.8)
for k in range(1, int(T_cyc / 2 / T_n) + 1):
    a1.axvline(k * T_n, color=C_ACC, ls="--", lw=.9)
    a1.plot([k * T_n], [S / 2 * (1 - np.cos(w * k * T_n))], "o", color=C_ACC, ms=4)
for k in range(1, int(T_cyc / 2 / T_n)):
    x1 = S / 2 * (1 - np.cos(w * k * T_n))
    x2 = S / 2 * (1 - np.cos(w * (k + 1) * T_n))
    a1.annotate("", (0.15, x1), (0.15, x2),
                arrowprops=dict(arrowstyle="<->", color=C_ACC, lw=1))
    a1.text(0.30, (x1 + x2) / 2, f"{x2-x1:.1f} in", fontsize=7.5, color=C_ACC, va="center")
a1.set_ylabel("polished-rod position (in)")
a1.set_title("Fig 3 — Undulations are periodic in time; the card plots position.\n"
             f"Equal {T_n:.2f} s intervals map to shrinking position steps as the unit "
             "slows at the top of stroke", fontsize=10)
a2.plot(tt, vv, color=C_DN, lw=1.8)
for k in range(1, int(T_cyc / 2 / T_n) + 1):
    a2.axvline(k * T_n, color=C_ACC, ls="--", lw=.9)
a2.set_ylabel("rod velocity (in/s)")
a2.set_xlabel("time from bottom of stroke (s)")
fig.tight_layout()
fig.savefig("fig3_time_vs_position.png")
print("\nwrote fig1_card.png, fig2_load_vs_time.png, fig3_time_vs_position.png")
