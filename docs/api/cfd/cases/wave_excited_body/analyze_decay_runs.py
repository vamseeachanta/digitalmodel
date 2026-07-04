#!/usr/bin/env python3
"""Analyze the free-decay heave runs (#1332) -> decay_results.json.

Usage:
    uv run python analyze_decay_runs.py <runs_root> <out_json>

``runs_root`` holds one sub-directory per release amplitude, each with a solved
``validation_wave_excited_body_decay_*/background`` (sixDoF ``report on`` log).
Fits each ring-down (:func:`analyze_ringdown`), then attributes the #1324
near-resonance RAO reduction against the potential-flow reference: the damped
natural period vs the reference resonance frequency (body dynamics check) and
the damping ratio vs the reference radiation damping (predicts the resonance
peak the body would actually reach).
"""
from __future__ import annotations

import glob
import json
import sys
from pathlib import Path

import numpy as np

from digitalmodel.solvers.openfoam.validation.wave_excited_body import (
    extract_heave_from_log,
)
from digitalmodel.solvers.openfoam.validation.wave_excited_body_decay import (
    analyze_ringdown,
    attribution,
    predict_peak_rao,
    reference_damping_ratio,
)

# The #1324 forced-sweep RAO nearest the reference resonance peak (T = 0.9 s).
FORCED_RAO_NEAR_RESONANCE = 0.744


def main() -> None:
    root = Path(sys.argv[1])
    out_json = Path(sys.argv[2])

    points = []
    for run in sorted(root.glob("*/validation_wave_excited_body_decay_*")):
        bg = run / "background"
        if not bg.is_dir():
            continue
        t, z = extract_heave_from_log(bg)
        t, z = np.asarray(t), np.asarray(z)
        fit = analyze_ringdown(t, z, settle_start=0.05)
        # release amplitude from the run directory tag (o0p03 -> 0.03)
        tag = run.name.split("_o")[-1].replace("p", ".")
        offset = float(tag) if tag else fit["amp0"]
        att = attribution(fit, FORCED_RAO_NEAR_RESONANCE)
        points.append({"offset_m": offset, "fit": fit, "attribution": att,
                       "heave": {"t": [round(float(v), 4) for v in t],
                                 "z_cm": [round(float(v), 6) for v in z]}})

    ref = reference_damping_ratio()
    zetas = [p["fit"]["zeta"] for p in points]
    wns = [p["fit"]["omega_n"] for p in points]
    amplitude_independent = (max(zetas) - min(zetas) < 0.03) if len(zetas) > 1 else None
    summary = {
        "issue": "#1332",
        "resolves": "#1324 near-resonance open finding",
        "reference": ref,
        "forced_rao_near_resonance": FORCED_RAO_NEAR_RESONANCE,
        "zeta_mean": float(np.mean(zetas)),
        "zeta_amplitude_independent": amplitude_independent,
        "omega_n_mean": float(np.mean(wns)),
        "omega_n_rel_error_vs_reference": float(np.mean(wns) / ref["omega_n"] - 1.0),
        "predicted_peak_rao": predict_peak_rao(float(np.mean(zetas))),
        "inviscid_peak_rao": predict_peak_rao(ref["zeta_radiation"]),
    }
    out = {"summary": summary, "points": points}
    out_json.write_text(json.dumps(out, indent=2) + "\n")

    print(f"{'offset(mm)':>11}{'zeta':>8}{'wn':>8}{'Td':>8}{'pred_peakRAO':>14}")
    for p in points:
        f = p["fit"]
        print(f"{p['offset_m']*1000:11.0f}{f['zeta']:8.3f}{f['omega_n']:8.3f}"
              f"{f['period_d']:8.3f}{p['attribution']['predicted_peak_rao']:14.3f}")
    print(f"\nzeta_mean={summary['zeta_mean']:.3f} "
          f"(radiation {ref['zeta_radiation']:.3f}); "
          f"amplitude-independent={amplitude_independent}")
    print(f"omega_n {summary['omega_n_mean']:.3f} vs reference "
          f"{ref['omega_n']:.3f} ({summary['omega_n_rel_error_vs_reference']:+.1%})")
    print(f"predicted peak RAO {summary['predicted_peak_rao']:.2f} "
          f"(inviscid {summary['inviscid_peak_rao']:.2f}); "
          f"forced-sweep near-resonance RAO {FORCED_RAO_NEAR_RESONANCE:.2f}")
    print(f"wrote {out_json}")


if __name__ == "__main__":
    main()
