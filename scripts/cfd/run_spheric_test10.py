#!/usr/bin/env python
"""Delorme / SPHERIC Test 10 shallow-fill forced-roll validation (#1433).

External validation against published EXPERIMENTAL data (not our own analytical):
the canonical SPHERIC Test 10 lateral-impact case (Delorme et al. 2009; benchmark
values Botia-Vera et al. 2010). A shallow-filled rectangular tank in forced roll
produces an overturning wave that slams the side wall at the still-water line; the
first-impact pressure at Sensor 1 is the validated quantity.

Verified benchmark spec (Botia-Vera et al. 2010, read from the PDF; adversarially
cross-checked):
  tank L=0.900 m, H=0.508 m, slab 0.062 m; fill h=0.093 m (~18%); roll 4 deg,
  sinusoidal; rotation axis at the tank-floor mid-length (0.45, 0, 0);
  drive period T = 0.85*T0 = 1.6295 s (T0 = 1.9171 s, low-fill first mode);
  Sensor 1 = left vertical wall, y = 0.093 m (the still-water line).
Published first-peak pressure (Sensor 1, 1X thickness, WATER; mean +/- SD over 100
experimental runs): 37.10 +/- 7.32 mBar = 3710 +/- 732 Pa (CoV ~19.7%).

This runs a single 2D interFoam realisation with a moving-wall (patchProbes) pressure
tap at the Sensor-1 height on BOTH walls (impacts alternate), and compares the CFD
first-impact peak to the experimental mean +/- SD band. A single deterministic CFD
run is one draw from a strongly stochastic (CoV ~20%) distribution, so agreement is
"within the experimental scatter", not an exact number. 2D cannot capture the
experiment's out-of-plane-thickness dependence or air entrapment.

    source /usr/lib/openfoam/openfoam2312/etc/bashrc
    uv run python scripts/cfd/run_spheric_test10.py --work-dir /mnt/local-analysis/sloshing_cfd_work
"""
from __future__ import annotations

import argparse
import json
import math
from pathlib import Path
from typing import Any, Dict, List

from digitalmodel.solvers.openfoam.runner import OpenFOAMRunConfig, OpenFOAMRunner
from digitalmodel.solvers.openfoam.validation.sloshing_2d import (
    _CASE_DEPTH,
    SloshingForcedRollConfig,
    build_forced_roll_case,
)
from digitalmodel.solvers.openfoam.pressure_taps import (
    PressureTap,
    read_tap_statistics,
    render_patch_probes_entry,
)

_REPO = Path(__file__).resolve().parents[2]
_OUT = _REPO / "docs" / "api" / "cfd" / "sloshing-spheric.json"

# Verified benchmark (Botia-Vera et al. 2010, Table II; SPHERIC Test 10 lateral water).
SENSOR1_HEIGHT_M = 0.093
PUBLISHED_MEAN_PA = 3710.0
PUBLISHED_SD_PA = 732.0
PUBLISHED_SRC = "Botia-Vera, Souto-Iglesias, Bulian & Lobovsky (2010), Table II (100 runs); Delorme et al. (2009), Ocean Eng. 36(2)"
N_CYCLES = 6.0
CPB = 90


def _inject_pressure_taps(case_dir: Path, breadth: float) -> None:
    """Add moving-wall (patchProbes) pressure taps at the Sensor-1 height on both walls."""
    z = 0.5 * _CASE_DEPTH
    left = render_patch_probes_entry(
        [PressureTap(name="S1_left", location=(0.0, SENSOR1_HEIGHT_M, z), patch="leftWall")],
        "leftWall", object_name="pTaps_leftWall")
    right = render_patch_probes_entry(
        [PressureTap(name="S1_right", location=(breadth, SENSOR1_HEIGHT_M, z), patch="rightWall")],
        "rightWall", object_name="pTaps_rightWall")
    cd = case_dir / "system" / "controlDict"
    txt = cd.read_text()
    i = txt.rfind("}")           # close of functions{} (last brace in the file)
    cd.write_text(txt[:i] + left + right + txt[i:])


def _peak_pa(case_dir: Path, fo: str, tap: str) -> Dict[str, float] | None:
    base = case_dir / "postProcessing" / fo
    dats = sorted(base.glob("*/p"))
    if not dats:
        return None
    try:
        stats = read_tap_statistics(dats[0], [tap], field_name="p", design_percentile=99.0)
    except Exception:  # noqa: BLE001
        return None
    s = stats.get(tap) or next(iter(stats.values()), None)
    if s is None:
        return None
    return {"peak": round(s.max, 1), "design_p99": round(s.design_equivalent, 1),
            "mean": round(s.mean, 1)}


def main(argv: List[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="SPHERIC Test 10 validation (#1433)")
    ap.add_argument("--work-dir", type=Path, default=Path("/mnt/local-analysis/sloshing_cfd_work"))
    args = ap.parse_args(argv)
    args.work_dir.mkdir(parents=True, exist_ok=True)

    cfg = SloshingForcedRollConfig(cells_per_breadth=CPB, n_cycles=N_CYCLES,
                                   name="spheric_test10_validation")
    case = build_forced_roll_case(cfg, args.work_dir, with_moment=False)
    _inject_pressure_taps(case, cfg.breadth)
    print(f"drive T={cfg.drive_period:.4f}s (0.85*T1; T1={cfg.first_mode_period:.4f}s), "
          f"fill h={cfg.fill_depth:.3f} m, roll {cfg.roll_amplitude_deg} deg, {cfg.end_time:.1f}s")
    res = OpenFOAMRunner(OpenFOAMRunConfig(run_set_fields=True, to_vtk=False)).run(case)
    print(f"solver: {res.status.value} ({res.duration_seconds:.0f}s)")

    left = _peak_pa(case, "pTaps_leftWall", "S1_left")
    right = _peak_pa(case, "pTaps_rightWall", "S1_right")
    peaks = [x["peak"] for x in (left, right) if x]
    cfd_peak = max(peaks) if peaks else None
    within = None
    if cfd_peak is not None:
        # within 2-sigma of the experimental first-peak distribution?
        within = abs(cfd_peak - PUBLISHED_MEAN_PA) <= 2.0 * PUBLISHED_SD_PA

    manifest = {
        "meta": {"generated_by": "scripts/cfd/run_spheric_test10.py", "epic": "#1429", "issue": "#1433",
                 "solver": "interFoam (VOF), OpenFOAM ESI v2312",
                 "benchmark": "SPHERIC Test 10 / Delorme et al. 2009 (lateral impact, water)",
                 "note": "Single 2D realisation vs the experimental first-peak distribution (strongly stochastic, CoV ~20%); 2D omits out-of-plane-thickness dependence and air entrapment."},
        "case": {"tank_L_m": cfg.breadth, "tank_H_m": cfg.tank_height, "fill_h_m": round(cfg.fill_depth, 4),
                 "fill_fraction": round(cfg.fill_depth / cfg.tank_height, 3),
                 "roll_amplitude_deg": cfg.roll_amplitude_deg,
                 "drive_period_s": round(cfg.drive_period, 4), "T0_first_mode_s": round(cfg.first_mode_period, 4),
                 "sensor1_height_m": SENSOR1_HEIGHT_M, "cells": [cfg.nx, cfg.ny, 1],
                 "n_cycles": N_CYCLES, "solver_status": res.status.value, "runtime_s": round(res.duration_seconds, 1)},
        "cfd_pressure_pa": {"left_wall": left, "right_wall": right, "first_peak_pa": cfd_peak},
        "published_pressure_pa": {"quantity": "Sensor 1, first-peak, water, 1X thickness; mean +/- SD (100 runs)",
                                  "mean": PUBLISHED_MEAN_PA, "sd": PUBLISHED_SD_PA, "cov_pct": 19.7,
                                  "source": PUBLISHED_SRC},
        "comparison": {"cfd_first_peak_pa": cfd_peak, "exp_mean_pa": PUBLISHED_MEAN_PA,
                       "exp_2sigma_band_pa": [PUBLISHED_MEAN_PA - 2 * PUBLISHED_SD_PA, PUBLISHED_MEAN_PA + 2 * PUBLISHED_SD_PA],
                       "within_experimental_scatter": within,
                       "ratio_cfd_over_mean": round(cfd_peak / PUBLISHED_MEAN_PA, 3) if cfd_peak else None},
    }
    _OUT.parent.mkdir(parents=True, exist_ok=True)
    _OUT.write_text(json.dumps(manifest, indent=2) + "\n")
    print(f"wrote {_OUT.relative_to(_REPO)}")
    print(f"  CFD first-peak (max wall): {cfd_peak} Pa vs exp {PUBLISHED_MEAN_PA}+/-{PUBLISHED_SD_PA} Pa "
          f"-> within 2-sigma: {within}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
