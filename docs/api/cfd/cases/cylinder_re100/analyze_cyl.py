"""Extract mean Cd and Strouhal number from the cylinder forceCoeffs history."""
import json
import sys
from pathlib import Path

import numpy as np

CASE = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).parent
OUT = CASE / "cyl_results"
OUT.mkdir(exist_ok=True)
D, U = 1.0, 1.0

cf = sorted(CASE.glob("postProcessing/forceCoeffs/*/coefficient*.dat"))
if not cf:
    cf = sorted(CASE.glob("postProcessing/forceCoeffs/*/forceCoeffs*.dat"))
path = cf[0]

# Parse header to locate columns (last comment line lists column names).
header = []
for raw in path.read_text().splitlines():
    if raw.startswith("#"):
        header = raw.lstrip("#").split()
    elif raw.strip():
        break
rows = np.loadtxt(path, comments="#")
# Column index: header may or may not include 'Time' as col 0. Build name->idx.
names = header
def col(name):
    for i, n in enumerate(names):
        if n.lower() == name.lower():
            return i
    return None

t = rows[:, 0]
ci_cd, ci_cl = col("Cd"), col("Cl")
if ci_cd is None:  # fallback to classic forceCoeffs.dat order: t Cm Cd Cl
    cd, cl = rows[:, 2], rows[:, 3]
else:
    cd, cl = rows[:, ci_cd], rows[:, ci_cl]

# Developed window: the saturated limit cycle (shedding saturates ~t=120 here).
# Use a fixed start past saturation, falling back to last 25% for short records.
t0 = max(115.0, t[0] + 0.75 * (t[-1] - t[0]))
m = t >= t0
tw, cdw, clw = t[m], cd[m], cl[m]

mean_cd = float(np.mean(cdw))
cd_amp = float((cdw.max() - cdw.min()) / 2)
cl_amp = float((clw.max() - clw.min()) / 2)

# Strouhal: shedding period from Cl up-crossings (high resolution for a clean
# limit cycle) — the FFT bin width over a short window is too coarse. FFT kept
# as a cross-check.
clc = clw - clw.mean()
up = np.where((clc[:-1] < 0) & (clc[1:] >= 0))[0]
# linear-interpolate each zero-crossing time
tcross = np.array(
    [tw[i] - clc[i] * (tw[i + 1] - tw[i]) / (clc[i + 1] - clc[i]) for i in up]
)
period = float(np.mean(np.diff(tcross))) if len(tcross) >= 2 else float("nan")
fpeak = 1.0 / period
strouhal = fpeak * D / U
n_periods = len(tcross) - 1

# FFT cross-check
tu = np.linspace(tw[0], tw[-1], 8192)
clu = np.interp(tu, tw, clc)
freq = np.fft.rfftfreq(len(tu), d=(tu[1] - tu[0]))
amp = np.abs(np.fft.rfft(clu * np.hanning(len(clu))))
fft_st = float(freq[1 + np.argmax(amp[1:])]) * D / U

res = {
    "t_start": float(t[0]), "t_end": float(t[-1]), "n_samples": int(len(t)),
    "window_start": float(t0),
    "mean_cd": mean_cd, "cd_amp": cd_amp, "cl_amp": cl_amp,
    "strouhal": strouhal, "shedding_freq": fpeak, "period": period,
    "n_periods_sampled": int(n_periods), "fft_strouhal": fft_st,
    "ref_cd": 1.35, "ref_cd_range": [1.33, 1.37],
    "ref_strouhal": 0.164, "ref_strouhal_range": [0.160, 0.167],
    "cd_err_pct": 100 * (mean_cd - 1.35) / 1.35,
    "st_err_pct": 100 * (strouhal - 0.164) / 0.164,
}
(OUT / "results.json").write_text(json.dumps(res, indent=2))

# time-history figure
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

fig, ax = plt.subplots(2, 1, figsize=(8, 5.2), sharex=True)
ax[0].plot(t, cd, lw=0.8, color="#c0392b"); ax[0].axhline(1.35, ls="--", color="k", lw=1, label="Williamson Cd=1.35")
ax[0].axvspan(t0, t[-1], color="#0b6e99", alpha=0.08, label="sampling window")
ax[0].set_ylabel("Cd"); ax[0].legend(fontsize=8); ax[0].grid(alpha=0.3)
ax[1].plot(t, cl, lw=0.8, color="#0b6e99")
ax[1].axvspan(t0, t[-1], color="#0b6e99", alpha=0.08)
ax[1].set_ylabel("Cl"); ax[1].set_xlabel("time"); ax[1].grid(alpha=0.3)
ax[0].set_title(f"Cylinder Re=100: mean Cd={mean_cd:.3f}, St={strouhal:.3f}")
fig.tight_layout(); fig.savefig(OUT / "force_history.png", dpi=130); plt.close(fig)

print(f"mean_cd={mean_cd:.3f} (ref 1.33-1.37, err {res['cd_err_pct']:+.1f}%)")
print(f"strouhal={strouhal:.4f} (zero-cross, {n_periods} periods) (ref 0.164, err {res['st_err_pct']:+.1f}%)")
print(f"  FFT cross-check St={fft_st:.4f}  period={period:.3f}")
print(f"cl_amp={cl_amp:.3f}  cd_amp={cd_amp:.4f}  window t={t0:.1f}-{t[-1]:.1f}  n={len(t)}")
