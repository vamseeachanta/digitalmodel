#!/usr/bin/env python
"""Roll-amplitude backbone sweep — how the sloshing resonance detunes off the
linear natural period as roll amplitude grows (issue #1433, epic #1429).

The forced-response study (#1433 round 1) found the resonant period sits on the
linear T1 at 4 deg roll. The literature (Bäuerlein & Avila 2024; Faltinsen &
Timokha 2009) predicts that ABOVE the critical filling depth h/L ~= 0.34 the
response is a SOFT-spring: the resonant peak shifts to LOWER frequency (LONGER
period, r = T_drive/T1 > 1) as amplitude grows; BELOW the critical depth it is a
HARD-spring and shifts the other way. This sweep maps that amplitude-dependent
backbone at two fills to test the claim with our own CFD and turn "peak at T1"
into an amplitude-annotated statement.

Matrix: fills {0.30 (below critical), 0.70 (above critical)} x roll amplitude
{2,4,6,8} deg x drive-period ratio {0.90..1.15}*T1. Per (fill, amplitude) the
wall run-up vs ratio is fit with a 3-point parabola to locate the resonant ratio
(the backbone point) to sub-grid resolution.

Runs the 48 cases with an in-process thread pool (each thread blocks on its own
interFoam subprocess — true parallelism, and NO per-case timeout, unlike the
590 s agent cap that killed violent near-resonance cases before). Reuses the
validated framework; the only local logic is the matrix, the wall-probe patch,
and the parabolic backbone fit.

    source /usr/lib/openfoam/openfoam2312/etc/bashrc
    uv run python scripts/cfd/run_sloshing_backbone.py --work-dir /mnt/local-analysis/sloshing_cfd_work --workers 14
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
_MANIFEST = _REPO / "docs" / "api" / "structural" / "sloshing-backbone.json"

BREADTH = 0.9
TANK_HEIGHT = 0.9
CRITICAL_DEPTH = 0.34
FILLS = (0.30, 0.70)               # below / above the critical depth
ROLL_DEGS = (2.0, 4.0, 6.0, 8.0)
RATIOS = (0.90, 0.95, 1.00, 1.05, 1.10, 1.15)   # x T1(fill)
CPB = 60
N_CYCLES = 6.0

_g = "{:.6g}".format


def _t1(hl: float) -> float:
    return SloshingForcedRollConfig(
        breadth=BREADTH, tank_height=TANK_HEIGHT, fill_depth=hl * BREADTH).first_mode_period


def _patch_wall_probe(case_dir: Path) -> None:
    cd = case_dir / "system" / "controlDict"
    txt = cd.read_text()
    center = f"({_g(0.5 * BREADTH)} 0 {_g(0.5 * _CASE_DEPTH)})"
    wall = f"({_g(0.5 * BREADTH / CPB)} 0 {_g(0.5 * _CASE_DEPTH)})"
    if center in txt:
        cd.write_text(txt.replace(center, wall))


def _case_name(hl: float, deg: float, ratio: float) -> str:
    return f"bb_hl{round(hl*100):02d}_a{round(deg*10):03d}_r{round(ratio*100):03d}"


def _runup(elev: List[float]) -> float:
    if not elev:
        return 0.0
    tail = elev[int(0.4 * len(elev)):] or elev
    return 0.5 * (max(tail) - min(tail))


def _run_case(work_dir: Path, hl: float, deg: float, ratio: float) -> Dict[str, Any]:
    name = _case_name(hl, deg, ratio)
    case = work_dir / name
    done = case / "_result.json"
    if done.exists():
        prev = json.loads(done.read_text())
        if prev.get("status") == "completed":
            return prev
    drive = round(ratio * _t1(hl), 5)
    cfg = SloshingForcedRollConfig(
        breadth=BREADTH, tank_height=TANK_HEIGHT, fill_depth=hl * BREADTH,
        roll_amplitude_deg=deg, roll_period=drive, cells_per_breadth=CPB,
        n_cycles=N_CYCLES, name=name)
    build_forced_roll_case(cfg, work_dir, with_moment=True)
    _patch_wall_probe(case)
    res = OpenFOAMRunner(OpenFOAMRunConfig(run_set_fields=True, to_vtk=False)).run(case)
    row: Dict[str, Any] = {"h_over_L": hl, "roll_deg": deg, "ratio": ratio,
                           "drive_period_s": drive, "status": res.status.value,
                           "runtime_s": round(res.duration_seconds, 1)}
    if res.status.value == "completed":
        try:
            _, elev = parse_interface_height(case, expected_height=hl * BREADTH)
            row["runup_amp_m"] = round(_runup(elev), 6)
        except (FileNotFoundError, RuntimeError):
            row["runup_amp_m"] = None
        try:
            mt, mz = parse_roll_moment(case)
            red = reduce_roll_moment(mt, mz, drive, fill_level=hl * BREADTH / TANK_HEIGHT,
                                     roll_amplitude_deg=deg)
            row["quad_coeff"] = round(red["quad_coeff"], 5)
        except (FileNotFoundError, RuntimeError, ValueError):
            row["quad_coeff"] = None
    else:
        row["error"] = res.error_message
    done.write_text(json.dumps(row, indent=2) + "\n")
    return row


def _parabolic_peak(xs: List[float], ys: List[float]) -> Optional[float]:
    """Sub-grid resonant ratio from a 3-point parabola around the max run-up."""
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
    # NOTE the leading minus — the version shipped in PR #1438 had "+", which
    # REFLECTS the vertex about the grid maximum x1; peak locations were correct
    # only to the grid spacing. Corrected with the #1433 shallow-fill sweep.
    return x1 - 0.5 * ((x1 - x0) ** 2 * (y1 - y2) - (x1 - x2) ** 2 * (y1 - y0)) / (
        (x1 - x0) * (y1 - y2) - (x1 - x2) * (y1 - y0))


def collect(work_dir: Path, rows: List[Dict[str, Any]]) -> Dict[str, Any]:
    by = {}
    for r in rows:
        by.setdefault((r["h_over_L"], r["roll_deg"]), []).append(r)
    fills_out = []
    for hl in FILLS:
        t1 = _t1(hl)
        amps = []
        for deg in ROLL_DEGS:
            pts = sorted(by.get((hl, deg), []), key=lambda r: r["ratio"])
            comp = [p for p in pts if p["status"] == "completed"]
            # Resonance located by the QUADRATURE (damping) coefficient peak — the
            # phase-based locator (Bäuerlein & Avila 2021); the run-up amplitude
            # saturates near resonance and is NOT a reliable locator.
            qx = [p["ratio"] for p in comp if p.get("quad_coeff") is not None]
            qy = [p["quad_coeff"] for p in comp if p.get("quad_coeff") is not None]
            r_res = _parabolic_peak(qx, qy) if qy else None
            at_edge = bool(qy) and qx[qy.index(max(qy))] in (qx[0], qx[-1])
            runups = [p["runup_amp_m"] for p in comp if p.get("runup_amp_m")]
            amps.append({
                "roll_deg": deg,
                "resonant_ratio": round(r_res, 4) if r_res else None,   # T_res / T1
                "freq_ratio": round(1.0 / r_res, 4) if r_res else None,  # f_res / f1
                "detuning_pct": round((1.0 / r_res - 1.0) * 100, 2) if r_res else None,
                "peak_quad_at_grid_edge": at_edge,
                "peak_runup_m": round(max(runups), 5) if runups else None,
                "points": [{"ratio": p["ratio"], "drive_period_s": p["drive_period_s"],
                            "runup_amp_m": p.get("runup_amp_m"), "quad_coeff": p.get("quad_coeff"),
                            "status": p["status"]} for p in pts],
            })
        fills_out.append({
            "h_over_L": hl, "T1_analytical_s": round(t1, 5),
            "regime": "above critical depth" if hl > CRITICAL_DEPTH else "near / below critical depth",
            "amplitudes": amps,
        })
    manifest = {
        "meta": {"generated_by": "scripts/cfd/run_sloshing_backbone.py",
                 "solver": "interFoam (VOF), OpenFOAM ESI v2312",
                 "epic": "#1429", "issue": "#1433", "g": G,
                 "critical_depth_h_over_L": CRITICAL_DEPTH,
                 "resonance_locator": "quadrature (damping) coefficient peak — phase-based, per Bäuerlein & Avila (2021)",
                 "note": "Resonant ratio r=T_drive/T1 located by 3-point parabolic fit of the quadrature coefficient vs ratio; run-up amplitude saturates near resonance and is reported separately."},
        "tank": {"shape": "rectangular", "breadth_m": BREADTH, "tank_height_m": TANK_HEIGHT,
                 "cells_per_breadth": CPB, "n_cycles": N_CYCLES},
        "fills": fills_out,
    }
    _MANIFEST.parent.mkdir(parents=True, exist_ok=True)
    _MANIFEST.write_text(json.dumps(manifest, indent=2) + "\n")
    return manifest


def main(argv: List[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Roll-amplitude backbone sweep (#1433)")
    ap.add_argument("--work-dir", type=Path, default=Path("/mnt/local-analysis/sloshing_cfd_work"))
    ap.add_argument("--workers", type=int, default=14)
    args = ap.parse_args(argv)
    args.work_dir.mkdir(parents=True, exist_ok=True)

    matrix = [(hl, deg, r) for hl in FILLS for deg in ROLL_DEGS for r in RATIOS]
    print(f"running {len(matrix)} cases with {args.workers} workers")
    rows: List[Dict[str, Any]] = []
    with ThreadPoolExecutor(max_workers=args.workers) as ex:
        futs = {ex.submit(_run_case, args.work_dir, hl, deg, r): (hl, deg, r) for hl, deg, r in matrix}
        for fut, key in futs.items():
            try:
                row = fut.result()
            except Exception as exc:  # noqa: BLE001 - record and continue
                hl, deg, r = key
                row = {"h_over_L": hl, "roll_deg": deg, "ratio": r, "status": "error", "error": str(exc)}
            rows.append(row)
            print(f"[{_case_name(*key)}] {row['status']} runup={row.get('runup_amp_m','-')}")
    m = collect(args.work_dir, rows)
    ok = sum(1 for r in rows if r["status"] == "completed")
    print(f"done: {ok}/{len(matrix)} completed -> {_MANIFEST.relative_to(_REPO)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
