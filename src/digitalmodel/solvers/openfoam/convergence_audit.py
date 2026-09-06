"""Convergence audit across many LTS resistance runs: when could each run have been stopped?

For every force history given (case directories or force.dat files, optionally labelled
`label=path`), runs force_cycle_average.analyse and reports one row: rows, pressure-force
extrema count, last half period, latest cycle mean of the total, cycle-to-cycle change,
Aitken and damped-fit asymptotes of the settled total, the iteration at which the fitted
wobble falls below `--amp-pct` % of the total, and a verdict:
  settled     : cycle change < gate and fit amplitude already below amp_pct
  extrapolable: >= 3 extrema, asymptote available (Aitken and fit within 2 % of each other)
  transient   : fewer than 3 extrema (half period known if 2)
  short       : fewer than 800 rows
Output: a markdown table (stdout) and optional JSON.

CLI: python -m digitalmodel.solvers.openfoam.convergence_audit [label=]path ... [--start 400]
     [--gate-pct 1] [--amp-pct 1] [--json out.json] [--md out.md]
"""
from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

from .force_cycle_average import analyse


def audit_one(label, path, start, gate, amp):
    try:
        r = analyse(path, start, 25, gate, amp)
    except Exception as e:  # noqa: BLE001
        return {"label": label, "error": str(e)}
    row = {"label": label, "rows": r["rows"], "last": r["last_iteration"], "n_extrema": len(r["extrema"]),
           "half_period": r.get("half_period_last"), "viscous": r["viscous_last400"]}
    if r["cycles"]:
        row["cycle_total"] = r["cycles"][0]["total"]; row["cycle_span"] = (r["cycles"][0]["from"], r["cycles"][0]["to"])
    row["cycle_change_pct"] = r.get("cycle_change_pct")
    row["aitken_total"] = r.get("aitken_total"); row["fit_total"] = r.get("fit_total")
    row["fit_period"] = r["fit"]["period"] if "fit" in r else None
    row["fit_tau"] = r["fit"]["tau"] if "fit" in r else None
    row["amp_ok_at"] = r.get("iteration_amp_below_pct")
    tot_ref = abs(row.get("cycle_total") or r["viscous_last400"] or 1.0)
    fit = r.get("fit")
    fit_sane = bool(fit and fit["period"] > 200 and fit["tau"] > 0 and fit["amplitude"] > 0.002 * tot_ref)
    flat = bool(fit and fit["amplitude"] <= 0.002 * tot_ref)   # no wobble to speak of
    if not fit_sane:
        row["fit_total"] = None; row["fit_period"] = None; row["fit_tau"] = None; row["amp_ok_at"] = None
    if r["rows"] < 800:
        v = "short"
    elif len(r["extrema"]) < 3:
        v = "transient"
    else:
        agree = (row["aitken_total"] is not None and row["fit_total"] is not None and
                 abs(row["aitken_total"] - row["fit_total"]) / max(abs(row["fit_total"]), 1e-9) < 0.02)
        amp_ok = row["amp_ok_at"] is not None and row["amp_ok_at"] <= r["last_iteration"]
        cyc_ok = row["cycle_change_pct"] is not None and row["cycle_change_pct"] < gate
        if cyc_ok and (amp_ok or flat):
            v = "settled"
        elif agree:
            v = "extrapolable"
        elif flat and cyc_ok:
            v = "settled"
        else:
            v = "oscillating"
    row["verdict"] = v
    return row


def md_table(rows, amp):
    scale = 1000.0 if any(abs(r.get("viscous") or 0) >= 1000 for r in rows if "error" not in r) else 1.0
    unit = "kN" if scale == 1000.0 else "N"
    kN = lambda v: "—" if v is None else (f"{v / 1000:+.1f}" if abs(v) >= 1000 else f"{v:+.2f} N")
    it = lambda v: "—" if v is None else f"{v:.0f}"
    pc = lambda v: "—" if v is None else f"{v:.2f}"
    o = ["| run | rows | extrema | half period | viscous kN | cycle total kN | cycle change % | Aitken total kN | fit total kN | fit period / tau | wobble < " + f"{amp:g} % at | verdict |",
         "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|"]
    for r in rows:
        if "error" in r:
            o.append(f"| {r['label']} | error: {r['error'][:60]} |||||||||||"); continue
        fp = "—" if r["fit_period"] is None else f"{r['fit_period']:.0f} / {r['fit_tau']:.0f}"
        o.append(f"| {r['label']} | {r['rows']} | {r['n_extrema']} | {it(r['half_period'])} | {kN(r['viscous'])} | {kN(r.get('cycle_total'))} | {pc(r['cycle_change_pct'])} | {kN(r['aitken_total'])} | {kN(r['fit_total'])} | {fp} | {it(r['amp_ok_at'])} | {r['verdict']} |")
    return "\n".join(o)


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("runs", nargs="+"); ap.add_argument("--start", type=float, default=400)
    ap.add_argument("--gate-pct", type=float, default=1.0); ap.add_argument("--amp-pct", type=float, default=1.0)
    ap.add_argument("--json"); ap.add_argument("--md")
    a = ap.parse_args(argv)
    rows = []
    for spec in a.runs:
        label, _, path = spec.partition("=") if "=" in spec else (Path(spec).stem, "", spec)
        rows.append(audit_one(label, path, a.start, a.gate_pct, a.amp_pct))
    md = md_table(rows, a.amp_pct)
    print(md)
    if a.md:
        Path(a.md).write_text(md + "\n")
    if a.json:
        Path(a.json).write_text(json.dumps(rows, indent=1))
    return 0


if __name__ == "__main__":
    sys.exit(main())
