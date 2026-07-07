#!/usr/bin/env python
"""Shallow-fill (h/L=0.15) refinement pack — does the amplitude-dependent
hardening trend survive a finer ratio grid and longer records? (issue #1433,
epic #1429, backlog item 0.)

The shipped shallow sweep (PR #1452, ``sloshing-shallow.json``, 8 cycles,
ratio grid {0.80..1.15} step 0.05) reported that the fill below the ~0.34
critical depth HARDENS, with a hardening detuning that SHRINKS as roll
amplitude grows (6.78% @2 deg -> 2.91% @8 deg). But every grid argmax of the
quadrature coefficient sat at r=0.95, so that whole amplitude trend lived in
the 3-point sub-grid parabola — an interpolation artefact until refined. The
verify workflow flagged two things to harden it:

  * a FINER ratio grid so the argmax moves between amplitudes on the grid
    itself (not only in the sub-grid fit), and
  * 16-CYCLE records — 8-cycle tails are transient-contaminated near the
    resonance (beat ~= 20 drive cycles), amplitude-dependently.

Matrix: h/L=0.15 x roll amplitude {1, 2, 8} deg x ratio {0.875..1.00} step
0.025 (6 points) = 18 cases. Endpoints {2, 8} deg re-run on the fine grid at
16 cycles; the 1 deg case is a low-amplitude anchor to reconcile the +7% @2 deg
forced offset against the 0.28% free-decay natural-frequency anchor (does the
hardening turn over toward the anchor as amplitude -> 0, or persist?).

This writes a SEPARATE manifest (``sloshing-shallow-refine.json``) and uses
DISTINCT 16-cycle case names (``shr_...c16``) so it never collides with — or
overwrites — the shipped 8-cycle sweep. Same tank/framework/reductions as the
shipped sweep: resonance located by the QUADRATURE (damping) coefficient peak
(phase-based, Bauerlein & Avila 2021); bore indicators from the moving-wall
probe tail.

    source /usr/lib/openfoam/openfoam2312/etc/bashrc
    uv run python scripts/cfd/run_sloshing_shallow_refine.py --work-dir /mnt/local-analysis/sloshing_cfd_work --workers 14
"""
from __future__ import annotations

import argparse
import json
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from typing import Any, Dict, List, Optional

from digitalmodel.solvers.openfoam.runner import OpenFOAMRunConfig, OpenFOAMRunner
from digitalmodel.solvers.openfoam.validation.sloshing_2d import (
    _CASE_DEPTH,
    SloshingForcedRollConfig,
    build_forced_roll_case,
    parse_interface_height,
    parse_roll_moment,
)
from digitalmodel.solvers.openfoam.validation.sloshing_sweep import reduce_roll_moment

G = 9.80665
_REPO = Path(__file__).resolve().parents[2]
_MANIFEST = _REPO / "docs" / "api" / "cfd" / "sloshing-shallow-refine.json"

BREADTH = 0.9
TANK_HEIGHT = 0.9
CRITICAL_DEPTH = 0.34
FILL = 0.15                        # h = 0.135 m — 9 cells exactly at cpb=60
# Per-amplitude ratio grid (Delta 0.025). Batch 1 ran {1,2,8} deg on {0.875..1.00}
# and found the hardening peak COLLAPSES with amplitude far faster than the
# shipped 8-cycle sweep implied: 1-2 deg peak ~0.93 (hardening ~+7.5%), but 8 deg
# was edge-clamped at the TOP of that grid (quad still rising at r=1.00) — the
# peak had moved to r>=1.0, i.e. the hardening is gone by 8 deg. Batch 2 extends
# 8 deg upward to bracket that peak and adds 4/6 deg at 16 cycles so the
# corrected amplitude curve is a complete, single-method (16-cycle) set. Each
# grid is centred to keep its quadrature argmax interior; already-run cases are
# cached, so re-running only executes the new ratios.
GRID = {
    1.0: (0.875, 0.900, 0.925, 0.950, 0.975, 1.000),
    # 2 deg is refined to a HALF-STEP (Delta_r = 0.0125) grid around its peak as
    # a grid-convergence check (adversarial-verify ask): if r* stays ~0.935 on
    # the finer grid the hardening is grid-converged, not a sub-grid parabola
    # artifact. The extra ratios 0.8875/0.9125/0.9375/0.9625 interleave the
    # 0.025-step nodes across 0.875..0.9625, bracketing the argmax at half step.
    2.0: (0.875, 0.8875, 0.900, 0.9125, 0.925, 0.9375, 0.950, 0.9625, 0.975, 1.000),
    4.0: (0.925, 0.950, 0.975, 1.000, 1.025, 1.050),
    6.0: (0.950, 0.975, 1.000, 1.025, 1.050, 1.075),
    8.0: (0.875, 0.900, 0.925, 0.950, 0.975, 1.000, 1.025, 1.050, 1.075, 1.100),
}
ROLL_DEGS = tuple(sorted(GRID))
CPB = 60
N_CYCLES = 16.0                    # 8-cycle tails are transient-contaminated

_g = "{:.6g}".format


def _t1() -> float:
    return SloshingForcedRollConfig(
        breadth=BREADTH, tank_height=TANK_HEIGHT, fill_depth=FILL * BREADTH).first_mode_period


def _patch_wall_probe(case_dir: Path) -> None:
    cd = case_dir / "system" / "controlDict"
    txt = cd.read_text()
    center = f"({_g(0.5 * BREADTH)} 0 {_g(0.5 * _CASE_DEPTH)})"
    wall = f"({_g(0.5 * BREADTH / CPB)} 0 {_g(0.5 * _CASE_DEPTH)})"
    if center in txt:
        cd.write_text(txt.replace(center, wall))


def _case_name(deg: float, ratio: float) -> str:
    # ratio*1000 avoids the round-half-to-even collision that ratio*100 would
    # cause for the 0.025-spaced grid (e.g. 0.925 -> 92 vs 0.975 -> 98).
    return f"shr_hl15_a{round(deg*10):03d}_r{round(ratio*1000):04d}_c16"


def _tail(elev: List[float]) -> List[float]:
    return elev[int(0.4 * len(elev)):] or elev


def _run_case(work_dir: Path, deg: float, ratio: float) -> Dict[str, Any]:
    name = _case_name(deg, ratio)
    case = work_dir / name
    done = case / "_result.json"
    if done.exists():
        prev = json.loads(done.read_text())
        if prev.get("status") == "completed":
            return prev
    h = FILL * BREADTH
    drive = round(ratio * _t1(), 5)
    cfg = SloshingForcedRollConfig(
        breadth=BREADTH, tank_height=TANK_HEIGHT, fill_depth=h,
        roll_amplitude_deg=deg, roll_period=drive, cells_per_breadth=CPB,
        n_cycles=N_CYCLES, name=name)
    build_forced_roll_case(cfg, work_dir, with_moment=True)
    _patch_wall_probe(case)
    res = OpenFOAMRunner(OpenFOAMRunConfig(run_set_fields=True, to_vtk=False)).run(case)
    row: Dict[str, Any] = {"roll_deg": deg, "ratio": ratio,
                           "drive_period_s": drive, "status": res.status.value,
                           "runtime_s": round(res.duration_seconds, 1)}
    if res.status.value == "completed":
        try:
            _, elev = parse_interface_height(case, expected_height=h)
            tail = _tail(elev)
            crest = max(tail) - h
            trough = h - min(tail)
            runup = 0.5 * (max(tail) - min(tail))
            row["runup_amp_m"] = round(runup, 6)
            row["crest_m"] = round(crest, 6)
            row["trough_m"] = round(trough, 6)
            row["asymmetry"] = round(crest / trough, 4) if trough > 1e-6 else None
            row["runup_over_h"] = round(runup / h, 4)
        except (FileNotFoundError, RuntimeError):
            row["runup_amp_m"] = None
        try:
            mt, mz = parse_roll_moment(case)
            red = reduce_roll_moment(mt, mz, drive, fill_level=h / TANK_HEIGHT,
                                     roll_amplitude_deg=deg)
            row["quad_coeff"] = round(red["quad_coeff"], 5)
        except (FileNotFoundError, RuntimeError, ValueError):
            row["quad_coeff"] = None
    else:
        row["error"] = res.error_message
    done.write_text(json.dumps(row, indent=2) + "\n")
    return row


def _parabolic_peak(xs: List[float], ys: List[float]) -> Optional[float]:
    """Sub-grid resonant ratio from a 3-point parabola around the max."""
    if len(xs) < 3:
        return xs[ys.index(max(ys))] if ys else None
    k = ys.index(max(ys))
    if k == 0 or k == len(ys) - 1:
        return xs[k]  # peak at a grid edge — report the edge
    x0, x1, x2 = xs[k - 1], xs[k], xs[k + 1]
    y0, y1, y2 = ys[k - 1], ys[k], ys[k + 1]
    denom = (y0 - 2 * y1 + y2)
    if denom == 0:
        return x1
    # Vertex of the parabola through the three points (Numerical Recipes 10.2).
    # Leading MINUS reflects the vertex correctly about x1 (the +sign bug that
    # shipped in PR #1438 was caught and fixed with the #1452 shallow sweep).
    return x1 - 0.5 * ((x1 - x0) ** 2 * (y1 - y2) - (x1 - x2) ** 2 * (y1 - y0)) / (
        (x1 - x0) * (y1 - y2) - (x1 - x2) * (y1 - y0))


def collect(work_dir: Path, rows: List[Dict[str, Any]]) -> Dict[str, Any]:
    t1 = _t1()
    by: Dict[float, List[Dict[str, Any]]] = {}
    for r in rows:
        by.setdefault(r["roll_deg"], []).append(r)
    amps = []
    for deg in ROLL_DEGS:
        pts = sorted(by.get(deg, []), key=lambda r: r["ratio"])
        comp = [p for p in pts if p["status"] == "completed"]
        qx = [p["ratio"] for p in comp if p.get("quad_coeff") is not None]
        qy = [p["quad_coeff"] for p in comp if p.get("quad_coeff") is not None]
        r_res = _parabolic_peak(qx, qy) if qy else None
        argmax_ratio = qx[qy.index(max(qy))] if qy else None
        at_edge = bool(qy) and argmax_ratio in (qx[0], qx[-1])
        runups = [p["runup_amp_m"] for p in comp if p.get("runup_amp_m")]
        asyms = [p["asymmetry"] for p in comp if p.get("asymmetry")]
        rohs = [p["runup_over_h"] for p in comp if p.get("runup_over_h")]
        amps.append({
            "roll_deg": deg,
            "resonant_ratio": round(r_res, 4) if r_res else None,
            "freq_ratio": round(1.0 / r_res, 4) if r_res else None,
            "detuning_pct": round((1.0 / r_res - 1.0) * 100, 2) if r_res else None,
            "argmax_ratio": argmax_ratio,          # grid argmax (not sub-grid)
            "peak_quad_at_grid_edge": at_edge,
            "peak_runup_m": round(max(runups), 5) if runups else None,
            "peak_runup_over_h": round(max(rohs), 4) if rohs else None,
            "peak_asymmetry": round(max(asyms), 4) if asyms else None,
            "points": [{"ratio": p["ratio"], "drive_period_s": p["drive_period_s"],
                        "runup_amp_m": p.get("runup_amp_m"), "quad_coeff": p.get("quad_coeff"),
                        "asymmetry": p.get("asymmetry"), "runup_over_h": p.get("runup_over_h"),
                        "status": p["status"]} for p in pts],
        })
    manifest = {
        "meta": {"generated_by": "scripts/cfd/run_sloshing_shallow_refine.py",
                 "solver": "interFoam (VOF), OpenFOAM ESI v2312",
                 "epic": "#1429", "issue": "#1433", "g": G,
                 "purpose": "refinement of the shipped h/L=0.15 shallow sweep (sloshing-shallow.json): finer ratio grid + 16-cycle records to test whether the amplitude-dependent hardening trend survives sub-grid interpolation",
                 "critical_depth_h_over_L": CRITICAL_DEPTH,
                 "resonance_locator": "quadrature (damping) coefficient peak — phase-based, per Bäuerlein & Avila (2021)",
                 "free_decay_anchor": "h/L=0.15 free-decay: measured f1 within 0.28% of linear tanh at small amplitude (sloshing-cfd-benchmark.json)",
                 "note": "argmax_ratio is the on-grid quadrature-coefficient maximum; resonant_ratio adds the 3-point sub-grid parabola. Bore indicators from the wall-probe tail (last 60%): asymmetry = crest/trough; runup_over_h = runup / still depth."},
        "tank": {"shape": "rectangular", "breadth_m": BREADTH, "tank_height_m": TANK_HEIGHT,
                 "fill_h_over_L": FILL, "fill_depth_m": FILL * BREADTH,
                 "T1_analytical_s": round(t1, 5),
                 "cells_per_breadth": CPB, "n_cycles": N_CYCLES},
        "amplitudes": amps,
    }
    _MANIFEST.parent.mkdir(parents=True, exist_ok=True)
    _MANIFEST.write_text(json.dumps(manifest, indent=2) + "\n")
    return manifest


def main(argv: List[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Shallow-fill refinement pack (#1433)")
    ap.add_argument("--work-dir", type=Path, default=Path("/mnt/local-analysis/sloshing_cfd_work"))
    ap.add_argument("--workers", type=int, default=14)
    args = ap.parse_args(argv)
    args.work_dir.mkdir(parents=True, exist_ok=True)

    matrix = [(deg, r) for deg in ROLL_DEGS for r in GRID[deg]]
    print(f"running {len(matrix)} cases with {args.workers} workers")
    rows: List[Dict[str, Any]] = []
    with ThreadPoolExecutor(max_workers=args.workers) as ex:
        futs = {ex.submit(_run_case, args.work_dir, deg, r): (deg, r) for deg, r in matrix}
        for fut, key in futs.items():
            try:
                row = fut.result()
            except Exception as exc:  # noqa: BLE001 - record and continue
                deg, r = key
                row = {"roll_deg": deg, "ratio": r, "status": "error", "error": str(exc)}
            rows.append(row)
            print(f"[{_case_name(*key)}] {row['status']} runup={row.get('runup_amp_m','-')} q={row.get('quad_coeff','-')}")
    collect(args.work_dir, rows)
    ok = sum(1 for r in rows if r["status"] == "completed")
    print(f"done: {ok}/{len(matrix)} completed -> {_MANIFEST.relative_to(_REPO)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
