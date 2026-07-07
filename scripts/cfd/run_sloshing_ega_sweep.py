#!/usr/bin/env python
"""Combined sway+roll (Effective Gravity Angle) excitation sweep — does adding
the roll-induced lateral acceleration shift the resonance and amplify the
response versus roll-only? (issue #1433, epic #1429, backlog item 1.)

Carette (2023): a real vessel's roll axis sits BELOW the ballast tank, so a
rolling tank also feels a lateral acceleration; the true forcing is an
Effective Gravity Angle that combines the angular tilt with an in-phase lateral
translation. Rolling the tank about its own floor (the earlier forced-roll and
backbone sweeps) captures only the tilt term and is therefore a PARTIAL,
conservative drive. This sweep adds the lateral term via OpenFOAM ``multiMotion``
(oscillatingRotatingMotion roll about the floor + an in-phase oscillatingLinear
SURGE) and measures the difference.

A roll about an axis a distance d below the floor is, to leading order in the
roll angle, roll-about-the-floor + a lateral surge of amplitude d*theta_0 (rad)
in anti-phase with the roll (phaseShift = T/2 -> -sin). Using multiMotion keeps
the reaction-moment reference at the floor centre so the quadrature coefficient
stays comparable to the roll-only baseline (rolling about the lowered axis would
put a large rigid lever arm into the moment). The lateral acceleration amplitude
is d*theta_0*omega^2, so the effective-angle amplification 1 + d*omega^2/g grows
with frequency — the EGA term matters most near and above resonance.

Matrix: fill h/L=0.70 (deep, clean soft-spring) x lever d in {0 (roll-only),
0.5L, 1.0L} x drive-period ratio {0.95..1.20}*T1 = 18 cases. Roll amplitude 4
deg, 12 drive cycles. Resonance located by the quadrature (damping) coefficient
peak (phase-based, Bäuerlein & Avila 2021); wall run-up reported for amplitude.

    source /usr/lib/openfoam/openfoam2312/etc/bashrc
    uv run python scripts/cfd/run_sloshing_ega_sweep.py --work-dir /mnt/local-analysis/sloshing_cfd_work --workers 14
"""
from __future__ import annotations

import argparse
import json
import math
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
_MANIFEST = _REPO / "docs" / "api" / "structural" / "sloshing-ega.json"

BREADTH = 0.9
TANK_HEIGHT = 0.9
FILL = 0.70
ROLL_DEG = 4.0
LEVERS = (0.0, 0.45, 0.90)         # roll-axis depth below floor: 0, 0.5L, 1.0L
RATIOS = (0.95, 1.00, 1.05, 1.10, 1.15, 1.20)   # x T1
# The EGA levers carry a much larger effective forcing amplitude (2-4x), so the
# 0.70 fill soft-springs further and its resonance shifts to LONGER period (r>1);
# the base grid clamps both EGA peaks at the r=1.20 high edge. Extend the grid
# there so the shifted quadrature peak is bracketed interior. collect() is
# data-driven, so these fold in automatically; roll-only (d=0) peaks at r~1.10
# and needs no extension.
EXTRA = [(d, r) for d in (0.45, 0.90) for r in (1.25, 1.30, 1.35)]
CPB = 60
N_CYCLES = 12.0                    # settled deep-fill records (> backbone's 6)

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


def _case_name(lever: float, ratio: float) -> str:
    return f"ega_hl70_d{round(lever*100):03d}_r{round(ratio*100):03d}"


def _runup(elev: List[float]) -> float:
    if not elev:
        return 0.0
    tail = elev[int(0.4 * len(elev)):] or elev
    return 0.5 * (max(tail) - min(tail))


def _run_case(work_dir: Path, lever: float, ratio: float) -> Dict[str, Any]:
    name = _case_name(lever, ratio)
    case = work_dir / name
    done = case / "_result.json"
    if done.exists():
        prev = json.loads(done.read_text())
        if prev.get("status") == "completed":
            return prev
    drive = round(ratio * _t1(), 5)
    theta = math.radians(ROLL_DEG)
    # multiMotion surge of amplitude d*theta (rad) in anti-phase (phaseShift=T/2)
    # reproduces roll about an axis d below the floor to leading order; lever 0 is
    # the roll-only single-DOF baseline.
    sway = lever * theta
    cfg = SloshingForcedRollConfig(
        breadth=BREADTH, tank_height=TANK_HEIGHT, fill_depth=FILL * BREADTH,
        roll_amplitude_deg=ROLL_DEG, roll_period=drive, cells_per_breadth=CPB,
        n_cycles=N_CYCLES, name=name,
        sway_amplitude_m=sway, sway_phase_shift_s=(drive / 2 if sway else 0.0))
    build_forced_roll_case(cfg, work_dir, with_moment=True)
    _patch_wall_probe(case)
    res = OpenFOAMRunner(OpenFOAMRunConfig(run_set_fields=True, to_vtk=False)).run(case)
    omega = 2.0 * math.pi / drive
    row: Dict[str, Any] = {"lever_m": lever, "lever_over_L": round(lever / BREADTH, 3),
                           "ratio": ratio, "drive_period_s": drive,
                           "ega_amplification": round(1.0 + lever * omega * omega / G, 4),
                           "status": res.status.value, "runtime_s": round(res.duration_seconds, 1)}
    if res.status.value == "completed":
        try:
            _, elev = parse_interface_height(case, expected_height=FILL * BREADTH)
            row["runup_amp_m"] = round(_runup(elev), 6)
        except (FileNotFoundError, RuntimeError):
            row["runup_amp_m"] = None
        try:
            mt, mz = parse_roll_moment(case)
            red = reduce_roll_moment(mt, mz, drive, fill_level=FILL * BREADTH / TANK_HEIGHT,
                                     roll_amplitude_deg=ROLL_DEG)
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
        return xs[k]
    x0, x1, x2 = xs[k - 1], xs[k], xs[k + 1]
    y0, y1, y2 = ys[k - 1], ys[k], ys[k + 1]
    if (y0 - 2 * y1 + y2) == 0:
        return x1
    return x1 - 0.5 * ((x1 - x0) ** 2 * (y1 - y2) - (x1 - x2) ** 2 * (y1 - y0)) / (
        (x1 - x0) * (y1 - y2) - (x1 - x2) * (y1 - y0))


def collect(work_dir: Path, rows: List[Dict[str, Any]]) -> Dict[str, Any]:
    t1 = _t1()
    by: Dict[float, List[Dict[str, Any]]] = {}
    for r in rows:
        by.setdefault(r["lever_m"], []).append(r)
    levers_out = []
    for lever in LEVERS:
        pts = sorted(by.get(lever, []), key=lambda r: r["ratio"])
        comp = [p for p in pts if p["status"] == "completed"]
        qx = [p["ratio"] for p in comp if p.get("quad_coeff") is not None]
        qy = [p["quad_coeff"] for p in comp if p.get("quad_coeff") is not None]
        r_res = _parabolic_peak(qx, qy) if qy else None
        at_edge = bool(qy) and qx[qy.index(max(qy))] in (qx[0], qx[-1])
        # Forcing-normalised resonance: the quad coefficient is normalised to the
        # ROLL angle, but the EGA effective-forcing magnification (1 + d*omega^2/g)
        # is frequency-dependent, so the raw peak is a BIASED (lower-bound) estimate
        # of the soft-spring shift. Dividing by the magnification isolates the
        # transfer function; if the shift survives it is genuine, not an artifact.
        qyn = [p["quad_coeff"] / (p.get("ega_amplification") or 1.0)
               for p in comp if p.get("quad_coeff") is not None]
        r_res_fn = _parabolic_peak(qx, qyn) if qyn else None
        runups = [p["runup_amp_m"] for p in comp if p.get("runup_amp_m")]
        levers_out.append({
            "lever_m": lever,
            "lever_over_L": round(lever / BREADTH, 3),
            "label": "roll-only" if lever == 0 else f"EGA d={lever/BREADTH:.2g}L",
            "resonant_ratio": round(r_res, 4) if r_res else None,
            "freq_ratio": round(1.0 / r_res, 4) if r_res else None,
            "detuning_pct": round((1.0 / r_res - 1.0) * 100, 2) if r_res else None,
            "forcing_norm_resonant_ratio": round(r_res_fn, 4) if r_res_fn else None,
            "forcing_norm_detuning_pct": round((1.0 / r_res_fn - 1.0) * 100, 2) if r_res_fn else None,
            "peak_quad_at_grid_edge": at_edge,
            "peak_runup_m": round(max(runups), 5) if runups else None,
            "points": [{"ratio": p["ratio"], "drive_period_s": p["drive_period_s"],
                        "ega_amplification": p.get("ega_amplification"),
                        "runup_amp_m": p.get("runup_amp_m"), "quad_coeff": p.get("quad_coeff"),
                        "status": p["status"]} for p in pts],
        })
    # Amplification of peak run-up vs the roll-only baseline (same ratio grid).
    base = next((l for l in levers_out if l["lever_m"] == 0), None)
    for l in levers_out:
        if base and base["peak_runup_m"] and l["peak_runup_m"]:
            l["runup_amplification_vs_rollonly"] = round(l["peak_runup_m"] / base["peak_runup_m"], 3)
    manifest = {
        "meta": {"generated_by": "scripts/cfd/run_sloshing_ega_sweep.py",
                 "solver": "interFoam (VOF), OpenFOAM ESI v2312",
                 "epic": "#1429", "issue": "#1433", "g": G,
                 "excitation": "combined sway+roll (Effective Gravity Angle) via multiMotion — roll about the tank floor + an in-phase lateral SURGE representing the roll-induced lateral acceleration for a roll axis a distance d below the floor (Carette 2023)",
                 "resonance_locator": "quadrature (damping) coefficient peak — phase-based, per Bäuerlein & Avila (2021); moment referenced to the floor centre so it is comparable across levers",
                 "note": "lever d is the roll-axis depth below the floor; the EGA lateral surge amplitude is d*theta0 (rad), phaseShift T/2. ega_amplification = 1 + d*omega^2/g is the effective-gravity-angle magnification at the drive frequency."},
        "tank": {"shape": "rectangular", "breadth_m": BREADTH, "tank_height_m": TANK_HEIGHT,
                 "fill_h_over_L": FILL, "fill_depth_m": FILL * BREADTH,
                 "roll_amplitude_deg": ROLL_DEG, "T1_analytical_s": round(t1, 5),
                 "cells_per_breadth": CPB, "n_cycles": N_CYCLES},
        "levers": levers_out,
    }
    _MANIFEST.parent.mkdir(parents=True, exist_ok=True)
    _MANIFEST.write_text(json.dumps(manifest, indent=2) + "\n")
    return manifest


def main(argv: List[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Combined sway+roll (EGA) sweep (#1433)")
    ap.add_argument("--work-dir", type=Path, default=Path("/mnt/local-analysis/sloshing_cfd_work"))
    ap.add_argument("--workers", type=int, default=14)
    args = ap.parse_args(argv)
    args.work_dir.mkdir(parents=True, exist_ok=True)

    matrix = [(d, r) for d in LEVERS for r in RATIOS]
    matrix += [t for t in EXTRA if t not in matrix]
    print(f"running {len(matrix)} cases with {args.workers} workers")
    rows: List[Dict[str, Any]] = []
    with ThreadPoolExecutor(max_workers=args.workers) as ex:
        futs = {ex.submit(_run_case, args.work_dir, d, r): (d, r) for d, r in matrix}
        for fut, key in futs.items():
            try:
                row = fut.result()
            except Exception as exc:  # noqa: BLE001
                d, r = key
                row = {"lever_m": d, "ratio": r, "status": "error", "error": str(exc)}
            rows.append(row)
            print(f"[{_case_name(*key)}] {row['status']} runup={row.get('runup_amp_m','-')} q={row.get('quad_coeff','-')}")
    collect(args.work_dir, rows)
    ok = sum(1 for r in rows if r["status"] == "completed")
    print(f"done: {ok}/{len(matrix)} completed -> {_MANIFEST.relative_to(_REPO)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
