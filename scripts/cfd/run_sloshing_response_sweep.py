#!/usr/bin/env python
"""Forced-roll frequency-response sweep — how roll excitation reveals the sloshing
resonance (issue #1433, epic #1429).

For one rectangular tank (L = H = 0.9 m) at three fills, this quantifies the
"with vs without forced-roll" story that gives confidence in the CFD methodology:

* WITHOUT forced-roll (per fill): a free-decay case -> the tank's intrinsic
  first-mode natural period T1_cfd (vs the analytical tanh value).
* WITH forced-roll (per fill): drive the tank at a range of roll periods
  bracketing T1; the steady wall run-up (and quadrature/damping coefficient)
  trace a resonance curve that PEAKS at the natural period. The drive period at
  the peak is the CFD forced-roll resonant period.

Confidence check: at every fill the forced-roll resonant period coincides with
the free-decay / analytical natural period.

CLI (generate / run-one / collect) so the 18 cases fan out across cores via a
workflow (one agent per prepared case):

    generate  : build all case dirs (+ per-case _spec.json), no solver
    run-one   : run+reduce ONE prepared case dir, write _result.json, print JSON
    collect   : assemble docs/api/structural/sloshing-forced-response.json
    all       : generate -> run every case in-process -> collect (serial)

Reuses the validated framework (no new solver code):
  sloshing_2d (build/analyze) + sloshing_sweep.reduce_roll_moment.
The center probe build_forced_roll_case writes sits on the first-mode NODE; for
forced cases this driver re-points it to a near-wall ANTINODE (a non-invasive
controlDict patch) so the run-up response is clean.
"""
from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any, Dict, List

from digitalmodel.solvers.openfoam.runner import OpenFOAMRunConfig, OpenFOAMRunner
from digitalmodel.solvers.openfoam.validation.sloshing_2d import (
    _CASE_DEPTH,
    SloshingForcedRollConfig,
    SloshingFreeDecayConfig,
    analyze_free_decay,
    build_forced_roll_case,
    build_free_decay_case,
    parse_interface_height,
    parse_roll_moment,
)
from digitalmodel.solvers.openfoam.validation.sloshing_sweep import reduce_roll_moment

G = 9.80665
_REPO = Path(__file__).resolve().parents[2]
_MANIFEST = _REPO / "docs" / "api" / "structural" / "sloshing-forced-response.json"

BREADTH = 0.9
TANK_HEIGHT = 0.9              # square section: ample freeboard even at h/L = 0.70
FILLS = (0.30, 0.50, 0.70)    # h/L
PERIOD_RATIOS = (0.70, 0.85, 1.00, 1.15, 1.30)   # x T1(fill)
ROLL_DEG = 4.0
FREEDECAY_CPB = 80            # accurate natural freq; case stays < ~5 min
FREEDECAY_ENDTIME = 12.0      # ~10-12 first-mode periods, enough for the FFT
FORCED_CPB = 60              # peak LOCATION is robust to mesh; keeps the sweep cheap
N_CYCLES = 6.0

_g = "{:.6g}".format


def _t1(hl: float) -> float:
    """Analytical first-mode period (s) at fill h/L for the L=0.9 tank."""
    return SloshingForcedRollConfig(
        breadth=BREADTH, tank_height=TANK_HEIGHT, fill_depth=hl * BREADTH,
    ).first_mode_period


def _specs() -> List[Dict[str, Any]]:
    """The 18 case specs: one free-decay + five forced per fill."""
    out: List[Dict[str, Any]] = []
    for hl in FILLS:
        out.append({"kind": "free-decay", "hl": hl,
                    "name": f"resp_fd_hl{round(hl * 100):02d}"})
        t1 = _t1(hl)
        for r in PERIOD_RATIOS:
            out.append({"kind": "forced", "hl": hl, "ratio": r,
                        "drive_period": round(r * t1, 5), "t1_analytical": round(t1, 5),
                        "name": f"resp_fr_hl{round(hl * 100):02d}_r{round(r * 100):03d}"})
    return out


def _patch_wall_probe(case_dir: Path) -> None:
    """Re-point interfaceHeight from tank centre (first-mode node) to a near-wall
    antinode. Non-invasive: edits only the generated system/controlDict."""
    cd = case_dir / "system" / "controlDict"
    txt = cd.read_text()
    center = f"({_g(0.5 * BREADTH)} 0 {_g(0.5 * _CASE_DEPTH)})"
    wall = f"({_g(0.5 * BREADTH / FORCED_CPB)} 0 {_g(0.5 * _CASE_DEPTH)})"
    if center in txt:
        cd.write_text(txt.replace(center, wall))


def _build(work_dir: Path, spec: Dict[str, Any]) -> Path:
    if spec["kind"] == "free-decay":
        cfg = SloshingFreeDecayConfig(
            breadth=BREADTH, tank_height=TANK_HEIGHT, fill_level=spec["hl"],
            cells_per_breadth=FREEDECAY_CPB, end_time=FREEDECAY_ENDTIME, name=spec["name"],
        )
        case = build_free_decay_case(cfg, work_dir)
    else:
        cfg = SloshingForcedRollConfig(
            breadth=BREADTH, tank_height=TANK_HEIGHT, fill_depth=spec["hl"] * BREADTH,
            roll_amplitude_deg=ROLL_DEG, roll_period=spec["drive_period"],
            cells_per_breadth=FORCED_CPB, n_cycles=N_CYCLES, name=spec["name"],
        )
        case = build_forced_roll_case(cfg, work_dir, with_moment=True)
        _patch_wall_probe(case)
    (case / "_spec.json").write_text(json.dumps(spec, indent=2) + "\n")
    return case


def _runup_amplitude(elev: List[float]) -> float:
    if not elev:
        return 0.0
    tail = elev[int(0.4 * len(elev)):] or elev
    return 0.5 * (max(tail) - min(tail))


def _run_one(case_dir: Path) -> Dict[str, Any]:
    spec = json.loads((case_dir / "_spec.json").read_text())
    # Idempotent: skip a case already solved (so a parallel fan-out is re-runnable).
    done = case_dir / "_result.json"
    if done.exists():
        prev = json.loads(done.read_text())
        if prev.get("status") == "completed":
            return prev
    runner = OpenFOAMRunner(OpenFOAMRunConfig(run_set_fields=True, to_vtk=False))
    res = runner.run(case_dir)
    row: Dict[str, Any] = {**spec, "status": res.status.value,
                           "runtime_s": round(res.duration_seconds, 1)}
    if res.status.value == "completed":
        if spec["kind"] == "free-decay":
            cfg = SloshingFreeDecayConfig(
                breadth=BREADTH, tank_height=TANK_HEIGHT, fill_level=spec["hl"],
                cells_per_breadth=FREEDECAY_CPB, end_time=FREEDECAY_ENDTIME, name=spec["name"])
            a = analyze_free_decay(case_dir, cfg)
            row.update(
                natural_freq_cfd_hz=round(a["measured_frequency"], 6),
                natural_period_cfd_s=round(1.0 / a["measured_frequency"], 5),
                natural_period_analytical_s=round(1.0 / a["analytical_frequency"], 5),
                rel_error=round(a["relative_error"], 5))
        else:
            fill_depth = spec["hl"] * BREADTH
            try:
                _, elev = parse_interface_height(case_dir, expected_height=fill_depth)
                row["runup_amp_m"] = round(_runup_amplitude(elev), 6)
            except (FileNotFoundError, RuntimeError):
                row["runup_amp_m"] = None
            try:
                mt, mz = parse_roll_moment(case_dir)
                red = reduce_roll_moment(mt, mz, spec["drive_period"],
                                         fill_level=fill_depth / TANK_HEIGHT,
                                         roll_amplitude_deg=ROLL_DEG)
                row.update(quad_coeff=round(red["quad_coeff"], 5),
                           moment_amplitude_nm=round(red["moment_amplitude"], 5))
            except (FileNotFoundError, RuntimeError, ValueError):
                row["quad_coeff"] = None
    else:
        row["error"] = res.error_message
    (case_dir / "_result.json").write_text(json.dumps(row, indent=2) + "\n")
    return row


def _collect(work_dir: Path) -> Dict[str, Any]:
    results: List[Dict[str, Any]] = []
    for spec in _specs():
        rp = work_dir / spec["name"] / "_result.json"
        if rp.exists():
            results.append(json.loads(rp.read_text()))
    fills: List[Dict[str, Any]] = []
    for hl in FILLS:
        fd = next((r for r in results if r["kind"] == "free-decay" and r["hl"] == hl), {})
        forced = [r for r in results if r["kind"] == "forced" and r["hl"] == hl]
        forced.sort(key=lambda r: r.get("ratio", 0))
        done = [p for p in forced if p.get("runup_amp_m")]
        resonant = (max(done, key=lambda p: p["runup_amp_m"]).get("drive_period")
                    if done else None)
        fills.append({
            "h_over_L": hl,
            "T1_analytical_s": round(_t1(hl), 5),
            "natural_period_cfd_s": fd.get("natural_period_cfd_s"),
            "natural_period_analytical_s": fd.get("natural_period_analytical_s"),
            "freedecay_rel_error": fd.get("rel_error"),
            "resonant_period_cfd_s": resonant,
            "resonance_metric": "wall run-up amplitude (runup_amp_m)",
            "forced": [{
                "period_ratio": p.get("ratio"),
                "drive_period_s": p.get("drive_period"),
                "drive_freq_hz": round(1.0 / p["drive_period"], 5) if p.get("drive_period") else None,
                "runup_amp_m": p.get("runup_amp_m"),
                "quad_coeff": p.get("quad_coeff"),
                "moment_amplitude_nm": p.get("moment_amplitude_nm"),
                "status": p.get("status"),
            } for p in forced],
        })
    manifest = {
        "meta": {"generated_by": "scripts/cfd/run_sloshing_response_sweep.py",
                 "solver": "interFoam (VOF), OpenFOAM ESI v2312",
                 "epic": "#1429", "issue": "#1433", "g": G, "roll_amplitude_deg": ROLL_DEG},
        "tank": {"shape": "rectangular", "breadth_m": BREADTH, "tank_height_m": TANK_HEIGHT,
                 "freedecay_cells_per_breadth": FREEDECAY_CPB,
                 "forced_cells_per_breadth": FORCED_CPB, "n_cycles": N_CYCLES},
        "fills": fills,
    }
    _MANIFEST.parent.mkdir(parents=True, exist_ok=True)
    _MANIFEST.write_text(json.dumps(manifest, indent=2) + "\n")
    return manifest


def main(argv: List[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Forced-roll sloshing frequency-response sweep (#1433)")
    ap.add_argument("command", choices=("generate", "run-one", "collect", "all"))
    ap.add_argument("--work-dir", type=Path, default=Path("/mnt/local-analysis/sloshing_cfd_work"))
    ap.add_argument("--case-dir", type=Path, help="case directory for run-one")
    args = ap.parse_args(argv)

    if args.command == "run-one":
        if not args.case_dir:
            ap.error("run-one requires --case-dir")
        row = _run_one(args.case_dir)
        print(json.dumps(row))
        return 0 if row["status"] == "completed" else 1

    args.work_dir.mkdir(parents=True, exist_ok=True)
    if args.command == "generate":
        specs = _specs()
        dirs = [_build(args.work_dir, s) for s in specs]
        (args.work_dir / "response_plan.json").write_text(
            json.dumps({"n": len(dirs), "cases": [{"name": s["name"], "kind": s["kind"],
                        "dir": str(args.work_dir / s["name"])} for s in specs]}, indent=2) + "\n")
        print(f"generated {len(dirs)} cases under {args.work_dir}")
        return 0
    if args.command == "collect":
        m = _collect(args.work_dir)
        print(f"wrote {_MANIFEST.relative_to(_REPO)} ({len(m['fills'])} fills)")
        return 0
    # all (serial)
    for s in _specs():
        case = _build(args.work_dir, s)
        r = _run_one(case)
        print(f"[{s['name']}] {r['status']}")
    m = _collect(args.work_dir)
    print(f"wrote {_MANIFEST.relative_to(_REPO)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
