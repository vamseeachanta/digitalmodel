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
        segs = sorted(list((p / "postProcessing" / "forces_hull").glob("*/force.dat")) + list((p / "postProcessing" / "forces").glob("*/force.dat")), key=lambda q: float(q.parent.name))
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


def central_value(iterations, series, period, n_periods=4):
    """Return a block-based mean and standard error over the newest full periods."""
    t = np.asarray(iterations, dtype=float)
    y = np.asarray(series, dtype=float)
    if len(t) != len(y):
        raise ValueError("iterations and series must have the same length")
    if len(t) < 2 or not np.all(np.isfinite(t)) or np.any(np.diff(t) <= 0):
        raise ValueError("iterations must contain at least two increasing finite values")
    if (
        not np.isfinite(period)
        or period <= 0
        or not isinstance(n_periods, int)
        or n_periods < 1
    ):
        raise ValueError("period must be positive and n_periods a positive integer")

    half_period = float(period) / 2.0
    requested_blocks = 2 * int(n_periods)
    available_blocks = int(math.floor((t[-1] - t[0]) / half_period))
    available_blocks -= available_blocks % 2
    blocks = min(requested_blocks, available_blocks)
    window_end = float(t[-1])
    window_start = window_end - blocks * half_period
    selected = (t >= window_start) & (t <= window_end)
    mean = float(np.mean(y[selected]))
    result = {
        "window_start": float(window_start),
        "window_end": window_end,
        "blocks": blocks,
        "periods": blocks // 2,
        "mean": mean,
        "standard_error": None,
        "relative_standard_error_pct": None,
        "note": None,
    }
    if blocks < 4:
        result["note"] = "fewer than 4 half-period blocks; standard error unavailable"
        return result

    block_means = []
    for index in range(blocks):
        lower = window_start + index * half_period
        upper = lower + half_period
        block_selected = (t >= lower) & (
            (t <= upper) if index == blocks - 1 else (t < upper)
        )
        if np.any(block_selected):
            block_means.append(float(np.mean(y[block_selected])))
    if len(block_means) < 4:
        result["blocks"] = len(block_means)
        result["periods"] = len(block_means) // 2
        result["note"] = "fewer than 4 populated half-period blocks; standard error unavailable"
        return result
    standard_error = float(np.std(block_means, ddof=1) / math.sqrt(len(block_means)))
    result["blocks"] = len(block_means)
    result["periods"] = len(block_means) // 2
    result["standard_error"] = standard_error
    if mean == 0:
        result["note"] = "zero central value; relative standard error unavailable"
    else:
        result["relative_standard_error_pct"] = float(
            standard_error / abs(mean) * 100.0
        )
    return result


def envelope_trend(iterations, series, period, n_windows=3):
    """Measure period-scaled peak-to-peak envelopes at the end of a history.

    Returned windows are chronological (the newest is last).  Each spans two of
    this history's own wobble periods.
    """
    t = np.asarray(iterations, dtype=float)
    y = np.asarray(series, dtype=float)
    if len(t) != len(y):
        raise ValueError("iterations and series must have the same length")
    if len(t) < 2 or not np.isfinite(period) or period <= 0 or n_windows < 1:
        return [], "indeterminate (history shorter than four wobble periods)"
    steps = np.diff(t)
    sample_step = float(np.median(steps))
    if not np.all(np.isfinite(steps)) or np.any(steps <= 0):
        raise ValueError("iterations must be strictly increasing")
    width = max(11, int(round(period / (8.0 * sample_step))))
    if width % 2 == 0:
        width += 1
    pad = width // 2
    smoothed = np.convolve(
        np.pad(y, pad, mode="reflect"), np.ones(width) / width, mode="valid"
    )

    newest_end = float(t[-1])
    window_width = 2.0 * float(period)
    windows = []
    for offset in range(int(n_windows) - 1, -1, -1):
        end = newest_end - offset * window_width
        start = end - window_width
        selected = (t >= start) & (t <= end)
        if int(selected.sum()) < 200:
            continue
        envelope = float(np.ptp(smoothed[selected]))
        windows.append((float(start), float(end), envelope))
    if len(windows) < 2:
        return windows, "indeterminate (history shorter than four wobble periods)"
    ratio = (
        windows[-1][2] / windows[-2][2]
        if windows[-2][2] != 0
        else (1.0 if windows[-1][2] == 0 else math.inf)
    )
    verdict = "decaying" if ratio < 0.8 else "rising" if ratio > 1.25 else "flat"
    return windows, verdict


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
        period = float(2.0 * np.median(np.diff([point[0] for point in ex])))
        windows, verdict = envelope_trend(t, pr, period)
        out["envelope_windows"] = windows
        out["envelope_newest"] = windows[-1][2] if windows else None
        out["envelope_verdict"] = verdict
        out["envelope_period"] = period
        out["central_value"] = {
            "n_periods": 4,
            "total": central_value(t, tot, period),
            "pressure": central_value(t, pr, period),
            "viscous": central_value(t, vi, period),
        }
    else:
        out["envelope_windows"] = []
        out["envelope_newest"] = None
        out["envelope_verdict"] = "indeterminate (history shorter than four wobble periods)"
        out["envelope_period"] = None
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
    # A damped-cosine fit is only usable as an estimator when it actually describes a
    # damped oscillation: a positive decay time, a period comparable with the measured
    # wobble period, a finite standard error on the mean, and residuals small against the
    # oscillation it claims to fit. Unconstrained least squares can otherwise return a
    # formally "converged" fit with tau < 0 or a period of a few iterations, which is
    # numerical noise; letting such a fit veto two agreeing estimators wrongly marks a
    # settled run as unsettled.
    fit_usable = False
    if fit:
        measured_period = out.get("envelope_period")
        period_ok = True
        if measured_period:
            period_ok = 0.3 <= fit["period"] / measured_period <= 3.0
        se = fit.get("mean_se")
        fit_usable = (
            fit["tau"] > 0
            and period_ok
            and se is not None and math.isfinite(se)
            and fit["rms_residual"] <= max(abs(fit["amplitude"]), 1e-9)
        )
        out["fit_usable"] = fit_usable
        if not fit_usable:
            reasons = []
            if fit["tau"] <= 0: reasons.append("non-decaying tau")
            if not period_ok: reasons.append(f"period {fit['period']:.0f} it vs measured {measured_period:.0f} it")
            if se is None or not math.isfinite(se): reasons.append("infinite standard error")
            if fit["rms_residual"] > max(abs(fit["amplitude"]), 1e-9): reasons.append("residual exceeds amplitude")
            out["fit_reject_reason"] = "; ".join(reasons)
    agreement = None
    agreement_basis = None
    if "aitken_total" in out and "fit_total" in out and fit_usable:
        agreement = float(abs(out["aitken_total"] - out["fit_total"]) / max(abs(out["fit_total"]), 1e-9) * 100.0)
        agreement_basis = "Aitken vs damped-cosine fit"
    elif "aitken_total" in out and out.get("cycles"):
        # Fall back to the two estimators that remain trustworthy.
        latest_cycle = out["cycles"][0]["total"]
        agreement = float(abs(out["aitken_total"] - latest_cycle) / max(abs(latest_cycle), 1e-9) * 100.0)
        agreement_basis = "Aitken vs latest cycle average (fit rejected)"
    out["estimator_agreement_pct"] = agreement
    out["estimator_agreement_basis"] = agreement_basis
    envelope_ok = out["envelope_verdict"] == "decaying"
    out["settling_verdict"] = "settled" if envelope_ok and (
        out.get("cycle_power_gate", False) or (agreement is not None and agreement <= 2.0)
    ) else "not settled"
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
    if "central_value" in r:
        central = r["central_value"]
        total = central["total"]
        pressure = central["pressure"]
        viscous = central["viscous"]
        def uncertainty(value):
            return "n/a" if value["standard_error"] is None else f"{value['standard_error'] / 1000:.1f}"
        relative = total["relative_standard_error_pct"]
        relative_text = "n/a" if relative is None else f"{relative:.1f} %"
        print(
            f"central value ({total['periods']} periods): total {kN(total['mean'])} +/- {uncertainty(total)} kN "
            f"({relative_text})  pressure {kN(pressure['mean'])} +/- {uncertainty(pressure)}  "
            f"viscous {kN(viscous['mean'])} +/- {uncertainty(viscous)}  "
            f"[window {total['window_start']:.0f}-{total['window_end']:.0f}, {total['blocks']} blocks]"
        )
    if "cycle_change_pct" in r:
        print(f"CYCLE POWER GATE   : {'PASS' if r['cycle_power_gate'] else 'FAIL'}  (cycle-to-cycle change of the total {r['cycle_change_pct']:.2f} %, gate {a.gate_pct} %)")
    if r["envelope_windows"]:
        values = " -> ".join(f"{window[2] / 1000:.0f}" for window in r["envelope_windows"])
        print(f"envelope (2 periods)   : {values} kN  {r['envelope_verdict'].upper()}   (period {r['envelope_period']:.0f} it)")
    else:
        print(f"envelope (2 periods)   : unavailable  {r['envelope_verdict'].upper()}")
    if "aitken_total" in r:
        print(f"Aitken asymptote   : pressure {kN(r['aitken_pressure'])} kN -> settled total {kN(r['aitken_total'])} kN")
    if "fit" in r:
        f = r["fit"]
        print(f"damped-cosine fit  : mean {kN(f['mean'])} ± {f['mean_se'] / 1000:.1f} kN, amplitude {f['amplitude'] / 1000:.0f} kN, tau {f['tau']:.0f} it, period {f['period']:.0f} it, rms resid {f['rms_residual'] / 1000:.1f} kN -> settled total {kN(r['fit_total'])} kN")
        if "iteration_amp_below_pct" in r:
            print(f"wobble < {a.amp_pct} % of total at ~{r['iteration_amp_below_pct']:.0f} iterations (fit)")
    agreement = "n/a" if r["estimator_agreement_pct"] is None else f"{r['estimator_agreement_pct']:.2f}"
    cycle_gate = "pass" if r.get("cycle_power_gate", False) else "fail"
    basis = r.get("estimator_agreement_basis")
    basis_txt = f", {basis}" if basis else ""
    print(f"SETTLING VERDICT    : {r['settling_verdict']}  (cycle gate {cycle_gate}, estimator agreement {agreement} %{basis_txt}, envelope {r['envelope_verdict']})")
    if "central_value" in r:
        total = r["central_value"]["total"]
        relative = total["relative_standard_error_pct"]
        uncertainty = "n/a" if relative is None else f"{relative:.1f} %"
        print(
            f"REPORTABLE AS      : {kN(total['mean'])} kN +/- {uncertainty} | settled: "
            f"{'yes' if r['settling_verdict'] == 'settled' else 'no'}  "
            "(a settled verdict does not imply a precise mean, and a precise mean does not imply a steady state)"
        )
    if r.get("fit_reject_reason"):
        print(f"  fit rejected as an estimator: {r['fit_reject_reason']}")
    if a.json:
        Path(a.json).write_text(json.dumps(r, indent=1))
    return 0


if __name__ == "__main__":
    sys.exit(main())
