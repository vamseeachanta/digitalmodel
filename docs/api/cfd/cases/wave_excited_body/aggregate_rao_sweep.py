#!/usr/bin/env python3
"""Aggregate per-period RAO-sweep results into one sweep_results.json (#1324).

Usage:
    uv run python aggregate_rao_sweep.py <results_root> <out_json>

``results_root`` holds one sub-directory per solved point, each with a
``results.json`` from ``analyze_rao_sweep_point.py``. The verified #1302
long-wave point (heave RAO 1.017 at T = 3 s) is folded in as a reused CFD
point — no recompute.

Resolution study
----------------
Short waves are numerically dissipated over the ~9.5 m fetch to the body, so
at the verified fast resolution (18 cells/wavelength) the incident wave — and
hence the measured RAO — is capped below the potential-flow reference near
resonance. Points at the same period but different cells-per-wavelength are
therefore folded into a **grid-convergence** view:

- ``summary.points`` — the gated curve: the *highest-resolution* point at each
  period (best available estimate of the true RAO).
- ``convergence`` — for every period solved at more than one resolution, the
  RAO at each cells-per-wavelength, ascending, showing it climb toward the
  reference as the grid is refined.
"""
from __future__ import annotations

import json
import sys
from pathlib import Path

from digitalmodel.solvers.openfoam.validation.wave_excited_body_rao import (
    RAO_SWEEP_BAND,
    RESONANCE_GUARD,
    _band_for,
    load_reference_curve,
    reference_peak,
    reference_rao_at,
    summarize_sweep,
)

# The verified #1302 CFD long-wave point, reused (not recomputed).
REUSED_1302 = {"period_s": 3.0, "rao_cfd": 1.017, "source": "#1302 (verified)"}


def _reused_point(curve) -> dict:
    T = REUSED_1302["period_s"]
    rao_cfd = REUSED_1302["rao_cfd"]
    rao_ref = reference_rao_at(T, curve)
    band = _band_for(T, curve)
    rel = abs(rao_cfd - rao_ref) / rao_ref
    return {
        "period_s": T, "rao_cfd": rao_cfd, "rao_reference": rao_ref,
        "rao_rel_error": rel, "band": band, "within_band": bool(rel <= band),
        "cells_per_wavelength": None, "reused_from": REUSED_1302["source"],
        "heave": None,
    }


def main() -> None:
    root = Path(sys.argv[1])
    out_json = Path(sys.argv[2])
    curve = load_reference_curve()

    # Collect every solved point (may include several resolutions per period).
    solved, heaves = [], {}
    for rj in sorted(root.glob("*/results.json")):
        data = json.loads(rj.read_text())
        pt = data["point"]
        solved.append(pt)
        # heave history keyed by period+resolution so refined runs don't clobber
        cpw = pt.get("cells_per_wavelength")
        heaves[f"{pt['period_s']:g}@{cpw:.0f}" if cpw else f"{pt['period_s']:g}"] = \
            data.get("heave")

    reused = _reused_point(curve)

    # Group by period (2-dp key). Primary curve point = highest resolution.
    by_period: dict[float, list[dict]] = {}
    for pt in solved:
        by_period.setdefault(round(pt["period_s"], 2), []).append(pt)

    primary, convergence = [], []
    for T, pts in sorted(by_period.items()):
        pts_sorted = sorted(pts, key=lambda p: p.get("cells_per_wavelength") or 0)
        primary.append(pts_sorted[-1])
        if len(pts_sorted) > 1:
            convergence.append({
                "period_s": T,
                "reference_rao": reference_rao_at(T, curve),
                "resolutions": [
                    {"cells_per_wavelength": p.get("cells_per_wavelength"),
                     "nx": p.get("nx"),
                     "incident_amplitude": p.get("incident_amplitude"),
                     "rao_cfd": p.get("rao_cfd"),
                     "rao_rel_error": p.get("rao_rel_error"),
                     "within_band": p.get("within_band")}
                    for p in pts_sorted
                ],
            })
    primary.append(reused)

    summary = summarize_sweep(primary, curve)
    peak_T, peak_rao = reference_peak(curve)
    out = {
        "issue": "#1324",
        "reference": {
            "method": "capytaine linear BEM (Froude-Krylov + diffraction), "
                      "finite depth 0.4 m, long box ~ 2D slab",
            "peak_period_s": peak_T, "peak_rao": peak_rao,
            "curve": {
                "period_s": [float(v) for v in curve["period_s"]],
                "rao_heave": [float(v) for v in curve["rao_heave"]],
            },
        },
        "band": {"off_resonance": RAO_SWEEP_BAND,
                 "resonance_guard": RESONANCE_GUARD},
        "summary": summary,
        "convergence": convergence,
        "heave": heaves,
    }
    out_json.write_text(json.dumps(out, indent=2) + "\n")

    print(f"{'T(s)':>6}{'CPW':>5}{'RAO_cfd':>9}{'RAO_ref':>9}{'err':>8}{'band':>7}  gate")
    for p in summary["points"]:
        cpw = p.get("cells_per_wavelength")
        print(f"{p['period_s']:6.2f}{(cpw or 0):5.0f}{p['rao_cfd']:9.3f}"
              f"{p['rao_reference']:9.3f}{p['rao_rel_error']*100:+7.0f}%"
              f"{p['band']*100:6.0f}%  {'PASS' if p['within_band'] else 'OUT'}")
    for c in convergence:
        chain = " -> ".join(
            f"CPW{r['cells_per_wavelength']:.0f}:{r['rao_cfd']:.3f}"
            for r in c["resolutions"])
        print(f"  convergence T={c['period_s']:.2f}s (ref {c['reference_rao']:.3f}): {chain}")
    print(f"reference peak RAO {peak_rao:.2f} at T={peak_T:.2f}s; "
          f"all_within_band={summary['all_within_band']}")
    print(f"wrote {out_json}")


if __name__ == "__main__":
    main()
