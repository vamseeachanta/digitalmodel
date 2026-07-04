#!/usr/bin/env python3
"""Analyze one solved RAO-sweep case (#1324) → per-period results.json.

Usage:
    uv run python analyze_rao_sweep_point.py <period> <case_root> <out_dir>

``case_root`` contains the solved ``background/`` (fields reconstructed).
Writes ``<out_dir>/results.json``: the #1302-style incident split + heave
response PLUS the potential-flow reference RAO at this period and the sweep
band gate (:mod:`wave_excited_body_rao`). The per-period config is
regenerated from the period (must match what built the case).
"""
from __future__ import annotations

import json
import sys
from pathlib import Path

from digitalmodel.solvers.openfoam.validation.wave_excited_body import (
    extract_heave_from_log,
)
from digitalmodel.solvers.openfoam.validation.wave_excited_body_rao import (
    analyze_sweep_point,
    build_sweep_config,
    load_reference_curve,
)


def main() -> None:
    period = float(sys.argv[1])
    case_root = Path(sys.argv[2])
    out_dir = Path(sys.argv[3])
    # Optional resolution override so the reported nx / cells-per-wavelength
    # match a refined-grid case (short-wave resolution study of #1324). Only
    # nx changes; the wave-gauge array and analysis window are period-driven,
    # so the analysis itself is identical to the default-resolution point.
    cpw = float(sys.argv[4]) if len(sys.argv) > 4 else 18.0
    out_dir.mkdir(parents=True, exist_ok=True)

    cfg = build_sweep_config(period, cells_per_wavelength=cpw)
    curve = load_reference_curve()
    point = analyze_sweep_point(case_root, cfg, curve)

    # Heave history (small) so the report can show the settled response
    # without keeping the heavy CFD field data.
    t, z = extract_heave_from_log(case_root / "background")

    out = {
        "config": {
            "wave": {"H": cfg.wave_height, "T": cfg.wave_period,
                     "depth": cfg.depth, "wavelength": cfg.wavelength,
                     "lambda_over_beam": cfg.lambda_over_beam},
            "body": {"beam": cfg.body_beam, "height": cfg.body_height,
                     "density": cfg.body_density, "mass": cfg.mass,
                     "draft": cfg.draft, "x": cfg.body_x,
                     "natural_period_hydrostatic": cfg.heave_natural_period},
            "mesh": {"background": [cfg.nx, 1, cfg.nz],
                     "component": [cfg.comp_nx, 1, cfg.comp_nz]},
            "steady_window": list(cfg.steady_window),
        },
        "point": point,
        "heave": {"t": list(map(float, t)), "z_cm": list(map(float, z))},
    }
    (out_dir / "results.json").write_text(json.dumps(out, indent=2) + "\n")
    p = point
    print(f"T={period:g}s  RAO_cfd={p['rao_cfd']:.3f}  "
          f"RAO_ref={p['rao_reference']:.3f}  "
          f"rel_err={p['rao_rel_error']:+.1%}  band={p['band']:.0%}  "
          f"{'PASS' if p['within_band'] else 'OUT'}  "
          f"(Kr={p['reflection_kr']:.2f}, CPW={p['cells_per_wavelength']:.0f})")
    print(f"wrote {out_dir / 'results.json'}")


if __name__ == "__main__":
    main()
