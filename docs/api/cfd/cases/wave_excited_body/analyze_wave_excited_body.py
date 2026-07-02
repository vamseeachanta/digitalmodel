#!/usr/bin/env python3
"""Analyze a solved wave-excited floating body case (#1302).

Usage:
    uv run python analyze_wave_excited_body.py <case_root> <out_dir> [nx nz cnx cnz]

``case_root`` is the directory containing ``background/`` (solved) and
``body/``. Writes ``<out_dir>/results.json`` with the incident-wave split,
heave response, RAO and the #1302 gates. Run from a repo checkout (needs
``digitalmodel`` importable via ``uv run``).
"""
from __future__ import annotations

import json
import sys
from pathlib import Path

from digitalmodel.solvers.openfoam.validation import (
    DRAFT_TOLERANCE,
    PERIOD_TOLERANCE,
    RAO_TOLERANCE,
    WaveExcitedBodyConfig,
    analyze_excited_heave,
    extract_heave_from_log,
    incident_wave_split,
)


def main() -> None:
    case_root = Path(sys.argv[1])
    out_dir = Path(sys.argv[2])
    out_dir.mkdir(parents=True, exist_ok=True)
    if len(sys.argv) > 6:
        nx, nz, cnx, cnz = (int(v) for v in sys.argv[3:7])
        cfg = WaveExcitedBodyConfig(nx=nx, nz=nz, comp_nx=cnx, comp_nz=cnz)
    else:
        # reference mesh (the archived case): background-refined, component
        # at the fast resolution (see WaveExcitedBodyConfig docstring)
        cfg = WaveExcitedBodyConfig(nx=375, nz=49)

    bg = case_root / "background"

    # incident amplitude from the broken-gauge-safe upstream split
    quality = incident_wave_split(bg, cfg)
    a_i = quality["incident_amplitude"]

    t, z = extract_heave_from_log(bg)
    res = analyze_excited_heave(t, z, a_i, cfg)

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
        "wave_quality": quality,
        "response": res,
        "heave": {"t": list(map(float, t)), "z_cm": list(map(float, z))},
        "tolerances": {"rao": RAO_TOLERANCE, "draft": DRAFT_TOLERANCE,
                       "period": PERIOD_TOLERANCE},
        "all_gates_pass": all(res["gates"].values()),
    }
    (out_dir / "results.json").write_text(json.dumps(out, indent=2) + "\n")
    print(f"RAO = {res['rao']:.3f} (gate 1.0 +/- {RAO_TOLERANCE:.0%}) "
          f"draft err {res['draft_error']:+.1%} period {res['period_measured']:.3f} s "
          f"-> gates {res['gates']}")
    print(f"wrote {out_dir / 'results.json'}")


if __name__ == "__main__":
    main()
