#!/usr/bin/env python
"""Aggregate CFD runtime / mesh / throughput into a committed compute manifest (#1439).

Compute optimization: turn the sloshing CFD case tree into a reusable
characterization of how long a 2D interFoam case takes on this machine and why —
so future compute cost (finer grids, 3D, larger sweeps) can be predicted.

Scans the (uncommitted) case tree in ``--work-dir`` for measured wall-runtimes,
reads mesh size (blockMeshDict) and sim time / step count (controlDict + solver
log), classifies by case-class, and writes the small derived manifest
``docs/api/cfd/sloshing-compute.json`` (the committed, reproducible artifact).

    uv run python scripts/cfd/collect_compute_stats.py --work-dir /mnt/local-analysis/sloshing_cfd_work
"""
from __future__ import annotations

import argparse
import glob
import json
import os
import re
import statistics as st
from pathlib import Path
from typing import Any, Dict, List, Optional

_REPO = Path(__file__).resolve().parents[2]
_OUT = _REPO / "docs" / "api" / "cfd" / "sloshing-compute.json"

# Machine this study ran on (a-l-2 / dev-secondary).
MACHINE = {"host": "dev-secondary (ace-linux-2)", "cores": 32, "ram_gb": 31,
           "solver": "interFoam (VOF), OpenFOAM ESI v2312 — single-threaded per case"}

# Per-case uncontended baseline wall time (s) for the forced 60x60 case, measured
# single (the smoke run) — used to separate contention from raw cost.
FORCED_UNCONTENDED_S = 309.0


def _classify(name: str) -> Optional[str]:
    if name.startswith("bb_"):
        return "backbone (forced roll, 60x60)"
    if name.startswith("resp_fr_"):
        return "forced roll (60x60)"
    if name.startswith("resp_fd_"):
        return "free-decay (80x80)"
    return None


# concurrency the class was actually run at (fan-out width)
CONCURRENCY = {"backbone (forced roll, 60x60)": 14,
               "forced roll (60x60)": 16, "free-decay (80x80)": 16}


def _cells(case: Path) -> Optional[int]:
    bm = case / "system" / "blockMeshDict"
    if not bm.exists():
        return None
    m = re.search(r"hex\s*\([^)]*\)\s*\((\d+)\s+(\d+)\s+(\d+)\)", bm.read_text())
    return int(m.group(1)) * int(m.group(2)) * int(m.group(3)) if m else None


def _control(case: Path):
    cd = case / "system" / "controlDict"
    txt = cd.read_text() if cd.exists() else ""
    end = re.search(r"endTime\s+([\d.eE+-]+)", txt)
    dt = re.search(r"deltaT\s+([\d.eE+-]+)", txt)
    adj = re.search(r"adjustTimeStep\s+(\w+)", txt)
    return (float(end.group(1)) if end else None,
            float(dt.group(1)) if dt else None,
            (adj.group(1) if adj else "no"))


def _timesteps(case: Path) -> Optional[int]:
    log = case / "log.interFoam"
    if not log.exists():
        return None
    n = 0
    for line in log.read_text(errors="replace").splitlines():
        if line.startswith("Time = "):
            n += 1
    return n or None


def _stats(v: List[float]) -> Dict[str, float]:
    return {"min": round(min(v)), "median": round(st.median(v)),
            "max": round(max(v)), "mean": round(st.mean(v))}


def main(argv: List[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Aggregate CFD compute stats (#1439)")
    ap.add_argument("--work-dir", type=Path, default=Path("/mnt/local-analysis/sloshing_cfd_work"))
    args = ap.parse_args(argv)

    buckets: Dict[str, List[Dict[str, Any]]] = {}
    for rp in glob.glob(str(args.work_dir / "*" / "_result.json")):
        case = Path(rp).parent
        cls = _classify(case.name)
        if cls is None:
            continue
        d = json.loads(Path(rp).read_text())
        if d.get("status") != "completed" or d.get("runtime_s") is None:
            continue
        end, dt, adj = _control(case)
        buckets.setdefault(cls, []).append({
            "runtime_s": d["runtime_s"], "cells": _cells(case),
            "sim_time_s": end, "dt": dt, "adjust": adj,
            "timesteps": _timesteps(case),
            "ctime": os.path.getctime(case), "mtime": os.path.getmtime(rp),
        })

    classes = []
    for cls, rows in sorted(buckets.items()):
        rt = [r["runtime_s"] for r in rows]
        cells = st.median([r["cells"] for r in rows if r["cells"]])
        sim = st.median([r["sim_time_s"] for r in rows if r["sim_time_s"]])
        steps = [r["timesteps"] for r in rows if r["timesteps"]]
        med_steps = st.median(steps) if steps else None
        med_rt = st.median(rt)
        s_per_sim = med_rt / sim if sim else None
        # mesh-normalised cost: micro-seconds of wall per (cell x timestep)
        us_cell_step = (med_rt / (cells * med_steps) * 1e6) if (cells and med_steps) else None
        wall = max(r["mtime"] for r in rows) - min(r["ctime"] for r in rows)
        serial = sum(rt)
        classes.append({
            "class": cls,
            "n_cases": len(rows),
            "cells": int(cells),
            "sim_time_s": round(sim, 2) if sim else None,
            "dt_mode": ("fixed dt=%.3g" % rows[0]["dt"]) if rows[0]["adjust"] == "no" else "adaptive (maxCo)",
            "median_timesteps": int(med_steps) if med_steps else None,
            "runtime_s": _stats(rt),
            "throughput_s_per_sim_s": round(s_per_sim, 1) if s_per_sim else None,
            "cost_us_per_cell_timestep": round(us_cell_step, 3) if us_cell_step else None,
            "concurrency": CONCURRENCY.get(cls),
            "batch_wall_min": round(wall / 60, 1),
            "sum_runtime_min": round(serial / 60, 1),
        })

    # Parallel-efficiency headline from the backbone batch (largest, cleanest).
    bb = next((c for c in classes if c["class"].startswith("backbone")), None)
    parallel = None
    if bb:
        n = bb["n_cases"]
        # serial-uncontended estimate: n x uncontended single-case time scaled by sim ratio
        sim_ratio = (bb["sim_time_s"] or 1) / 5.26  # smoke case sim time
        serial_uncontended_min = n * FORCED_UNCONTENDED_S * sim_ratio / 60
        parallel = {
            "batch": bb["class"], "n_cases": n, "concurrency": bb["concurrency"],
            "actual_wall_min": bb["batch_wall_min"],
            "serial_uncontended_est_min": round(serial_uncontended_min, 1),
            "wallclock_speedup_x": round(serial_uncontended_min / bb["batch_wall_min"], 1),
            "per_case_contention_x": round(bb["runtime_s"]["median"] / FORCED_UNCONTENDED_S, 2),
            "note": ("14-wide fan-out finished the batch ~%.0fx faster in wall-clock than "
                     "one-at-a-time, despite each case running ~%.1fx slower under memory-bandwidth "
                     "contention — net throughput win." % (
                         round(serial_uncontended_min / bb["batch_wall_min"]),
                         round(bb["runtime_s"]["median"] / FORCED_UNCONTENDED_S, 2))),
        }

    manifest = {
        "meta": {"generated_by": "scripts/cfd/collect_compute_stats.py", "epic": "#1429",
                 "issue": "#1439",
                 "note": "Derived compute characterization; case trees are not committed."},
        "machine": MACHINE,
        "classes": classes,
        "parallel_efficiency": parallel,
        "runtime_model": {
            "form": "t_wall ≈ C · N_cells · N_timesteps · f(concurrency)",
            "explanation": ("Wall time scales with the product of mesh cells and solver timesteps; "
                            "the mesh-normalised cost (µs per cell·timestep) is the machine invariant, "
                            "and f(concurrency) is the contention factor for parallel fan-out."),
        },
    }
    _OUT.parent.mkdir(parents=True, exist_ok=True)
    _OUT.write_text(json.dumps(manifest, indent=2) + "\n")
    print(f"wrote {_OUT.relative_to(_REPO)} ({len(classes)} classes)")
    for c in classes:
        print(f"  {c['class']}: n={c['n_cases']} cells={c['cells']} "
              f"sim={c['sim_time_s']}s steps={c['median_timesteps']} "
              f"rt_med={c['runtime_s']['median']}s thr={c['throughput_s_per_sim_s']}s/s "
              f"cost={c['cost_us_per_cell_timestep']}us/cell-step")
    if parallel:
        print(f"  parallel: {parallel['note']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
