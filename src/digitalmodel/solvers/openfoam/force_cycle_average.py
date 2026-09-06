"""Cycle-averaged hull force and damped-oscillation extrapolation for LTS resistance runs.

Why: on a bare hull at low Froude number the net pressure force wobbles about a small mean for
thousands of pseudo-iterations while friction settles early. A 400-iteration window rides the
wobble. Averaging the total force over a full wobble cycle, and extrapolating the decaying
extrema of the pressure force, gives the delivered-power level long before the instantaneous
force stops moving.

Method (all from postProcessing/forces_hull/<seg>/force.dat, columns: time, total xyz, pressure
xyz, viscous xyz; x is the flow direction):
  1. light smoothing (running mean over `--smooth` rows) of the pressure force x;
  2. extrema of the smoothed pressure force after `--start` (alternating peaks / troughs);
  3. cycle averages: the mean total, pressure and viscous force over the last full cycle
     (extremum k-2 -> k) and the cycle before it; the cycle-to-cycle change of the total is the
     CYCLE POWER criterion (1 %);
  4. Aitken (geometric) extrapolation of the last three extrema of the pressure force to its
     asymptote, and a damped-cosine least-squares fit p(t) = m + A exp(-(t-t0)/tau) cos(w (t-t0) + phi)
     for m with a standard error; predicted settled total = last-window viscous + m;
  5. the iteration at which the fitted wobble amplitude falls below `--amp-pct` % of the total.

CLI: python -m digitalmodel.solvers.openfoam.force_cycle_average <case-or-force.dat> [--start 500]
     [--smooth 25] [--gate-pct 1.0] [--amp-pct 1.0] [--json out.json]
"""
from __future__ import annotations

import argparse
import json
import math
import sys
from pathlib import Path

import numpy as np


def load_force(path: Path):
    p = Path(path)
    if p.is_dir():
        segs = sorted((p / "postProcessing" / "forces_hull").glob("*/force.dat"), key=lambda q: float(q.parent.name))
        if not segs:
            raise FileNotFoundError(f"no forces_hull/*/force.dat under {p}")
        p = segs[-1]
    rows = [l.split() for l in p.read_text().splitlines() if l.strip() and not l.startswith("#")]
    a = np.array([[float(v) for v in r[:10]] for r in rows])
    return p, a[:, 0], a[:, 1], a[:, 4], a[:, 7]   # t, total_x, pressure_x, viscous_x


def extrema(t, y, start, smooth):
    k = max(1, int(smooth))
    ys = np.convolve(y, np.ones(k) / k, mode="same") if k > 1 else y
    sel = t >= start
    ti, yi = t[sel], ys[sel]
    ex = []
    for i in range(k, len(yi) - k):
        w = yi[i - k:i + k + 1]
        if yi[i] == w.max() and yi[i] > yi[i - 1] and (not ex or ex[-1][2] != "peak"):
            ex.append((ti[i], yi[i], "peak"))
        elif yi[i] == w.min() and yi[i] < yi[i - 1] and (not ex or ex[-1][2] != "trough"):
            ex.append((ti[i], yi[i], "trough"))
    # prune ripples: merge extrema closer than min_sep, keep the more extreme of a same-type pair,
    # and re-enforce alternation
    pruned = []
    for e in ex:
        if pruned and e[0] - pruned[-1][0] < MIN_SEP:
            if e[2] == pruned[-1][2]:
                if (e[2] == "peak" and e[1] > pruned[-1][1]) or (e[2] == "trough" and e[1] < pruned[-1][1]):
                    pruned[-1] = e
            else:
                # opposite type within min_sep: a ripple, drop the smaller excursion
                if abs(e[1]) > abs(pruned[-1][1]) and len(pruned) > 1 and pruned[-2][2] == e[2]:
                    pruned.pop()
                    if (e[2] == "peak" and e[1] > pruned[-1][1]) or (e[2] == "trough" and e[1] < pruned[-1][1]):
                        pruned[-1] = e
                continue
        elif pruned and e[2] == pruned[-1][2]:
            if (e[2] == "peak" and e[1] > pruned[-1][1]) or (e[2] == "trough" and e[1] < pruned[-1][1]):
                pruned[-1] = e
        else:
            pruned.append(e)
    return ys, pruned


MIN_SEP = 300.0


def aitken(x1, x2, x3):
    d = x1 + x3 - 2 * x2
    return None if abs(d) < 1e-12 else (x1 * x3 - x2 * x2) / d


def damped_fit(t, y, t0):
    try:
        from scipy.optimize import curve_fit
    except Exception:
        return None
    sel = t >= t0
    tt, yy = t[sel] - t0, y[sel]
    if len(tt) < 200:
        return None
    m0 = yy[-len(yy) // 3:].mean(); A0 = (yy.max() - yy.min()) / 2
    # period guess from zero crossings of (y - m0)
    z = np.where(np.diff(np.sign(yy - m0)) != 0)[0]
    T0 = 2 * np.median(np.diff(tt[z])) if len(z) > 2 else max(tt[-1] / 2, 100)
    f = lambda x, m, A, tau, w, phi: m + A * np.exp(-np.clip(x / max(tau, 1e-6), -50, 50)) * np.cos(w * x + phi)
    best = None
    for phi0 in (0.0, 1.5, 3.0, 4.5):
        try:
            popt, pcov = curve_fit(f, tt, yy, p0=[m0, A0, tt[-1], 2 * math.pi / T0, phi0], maxfev=20000)
            r = float(np.sum((f(tt, *popt) - yy) ** 2))
            if best is None or r < best[2]:
                best = (popt, pcov, r)
        except Exception:
            continue
    if best is None:
        return None
    popt, pcov, r = best
    se = float(np.sqrt(max(pcov[0, 0], 0.0)))
    return {"mean": float(popt[0]), "mean_se": se, "amplitude": float(abs(popt[1])), "tau": float(popt[2]),
            "period": float(2 * math.pi / abs(popt[3])), "rms_residual": float(math.sqrt(r / len(tt))), "t0": float(t0)}


def analyse(path, start=500.0, smooth=25, gate_pct=1.0, amp_pct=1.0):
    p, t, tot, pr, vi = load_force(path)
    ys, ex = extrema(t, pr, start, smooth)
    out = {"file": str(p), "rows": int(len(t)), "last_iteration": float(t[-1]), "extrema": [(float(a), float(b), c) for a, b, c in ex]}
    vis_last = float(vi[-400:].mean())
    out["viscous_last400"] = vis_last
    out["cycles"] = []
    if len(ex) >= 3:
        for k in range(len(ex) - 1, 1, -1):
            a, b = ex[k - 2][0], ex[k][0]
            sel = (t >= a) & (t <= b)
            out["cycles"].append({"from": float(a), "to": float(b), "n": int(sel.sum()), "total": float(tot[sel].mean()),
                                  "pressure": float(pr[sel].mean()), "viscous": float(vi[sel].mean())})
            if len(out["cycles"]) == 3:
                break
        c = out["cycles"]
        if len(c) >= 2:
            out["cycle_change_pct"] = float(abs(c[0]["total"] - c[1]["total"]) / abs(c[1]["total"]) * 100)
            out["cycle_power_gate"] = bool(out["cycle_change_pct"] < gate_pct)
        e = [v for _, v, _ in ex[-3:]]
        m = aitken(*e)
        if m is not None:
            out["aitken_pressure"] = float(m); out["aitken_total"] = float(vis_last + m)
    if len(ex) >= 2:
        out["half_period_last"] = float(ex[-1][0] - ex[-2][0])
    fit = damped_fit(t, pr, ex[0][0]) if len(ex) >= 3 else None
    if len(ex) < 3:
        out["note"] = f"{len(ex)} extremum/extrema only: no cycle average or asymptote yet (need 3)"
    if fit:
        out["fit"] = fit; out["fit_total"] = float(vis_last + fit["mean"])
        tot_abs = abs(out["fit_total"])
        if fit["amplitude"] > 0 and fit["tau"] > 0:
            # amplitude A exp(-(t-t0)/tau) < amp_pct % of total
            need = fit["tau"] * math.log(fit["amplitude"] / (amp_pct / 100 * tot_abs)) if fit["amplitude"] > amp_pct / 100 * tot_abs else 0.0
            out["iteration_amp_below_pct"] = float(fit["t0"] + max(need, 0.0))
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("case"); ap.add_argument("--start", type=float, default=500); ap.add_argument("--smooth", type=int, default=25)
    ap.add_argument("--gate-pct", type=float, default=1.0); ap.add_argument("--amp-pct", type=float, default=1.0); ap.add_argument("--min-sep", type=float, default=300); ap.add_argument("--json")
    a = ap.parse_args(argv)
    global MIN_SEP; MIN_SEP = a.min_sep
    r = analyse(a.case, a.start, a.smooth, a.gate_pct, a.amp_pct)
    kN = lambda v: f"{v / 1000:+.1f}"
    print(f"{r['file']}: {r['rows']} rows to {r['last_iteration']:.0f}; viscous last 400 {kN(r['viscous_last400'])} kN")
    print("pressure-force extrema (it, kN): " + ", ".join(f"{a:.0f}:{kN(b)} {c[0]}" for a, b, c in r["extrema"]))
    if "half_period_last" in r:
        print(f"last half period   : {r['half_period_last']:.0f} iterations")
    if "note" in r:
        print("note               : " + r["note"])
    for i, c in enumerate(r["cycles"]):
        print(f"cycle {'latest' if i == 0 else 'previous' if i == 1 else 'earlier':8s} {c['from']:.0f}-{c['to']:.0f} ({c['n']} it): total {kN(c['total'])}  pressure {kN(c['pressure'])}  viscous {kN(c['viscous'])} kN")
    if "cycle_change_pct" in r:
        print(f"CYCLE POWER GATE   : {'PASS' if r['cycle_power_gate'] else 'FAIL'}  (cycle-to-cycle change of the total {r['cycle_change_pct']:.2f} %, gate {a.gate_pct} %)")
    if "aitken_total" in r:
        print(f"Aitken asymptote   : pressure {kN(r['aitken_pressure'])} kN -> settled total {kN(r['aitken_total'])} kN")
    if "fit" in r:
        f = r["fit"]
        print(f"damped-cosine fit  : mean {kN(f['mean'])} ± {f['mean_se'] / 1000:.1f} kN, amplitude {f['amplitude'] / 1000:.0f} kN, tau {f['tau']:.0f} it, period {f['period']:.0f} it, rms resid {f['rms_residual'] / 1000:.1f} kN -> settled total {kN(r['fit_total'])} kN")
        if "iteration_amp_below_pct" in r:
            print(f"wobble < {a.amp_pct} % of total at ~{r['iteration_amp_below_pct']:.0f} iterations (fit)")
    if a.json:
        Path(a.json).write_text(json.dumps(r, indent=1))
    return 0


if __name__ == "__main__":
    sys.exit(main())
