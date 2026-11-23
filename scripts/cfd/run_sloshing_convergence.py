#!/usr/bin/env python
"""Grid & timestep convergence study for the rectangular free-decay case (#1433).

Our literature review (adversarially verified) flagged that the "≤0.30%" free-decay
match must be EARNED by a Courant/mesh-convergence study, not asserted. This runs
that study on the exact case with the known ~0.30% error (L=1.0 m tank, h/L=0.30)
and answers: is the measured first-mode frequency grid- and timestep-converged, and
does the residual error shrink toward the analytical value (discretization) or sit
at a fixed offset (physical finite-amplitude nonlinearity)?

* MESH sweep: cells-per-breadth {40,60,80,100,120} at fixed Δt (h=0.30 lands exactly
  on a cell face for every mesh, so the analytical target is identical — a clean study).
* TIMESTEP sweep: Δt {0.004,0.002,0.001} at fixed 80×80 mesh.

Per case: measured FFT natural frequency vs analytical. On the mesh sweep the three
finest grids give a Richardson-extrapolated grid-converged frequency, observed order
of accuracy, and a GCI. Also records the run-time-estimator prediction vs actual
(validating scripts/capabilities/build_cfd_runtime_estimator.py).

Runs the cases with an in-process thread pool (uncapped per case). Reuses the
validated free-decay framework in sloshing_2d.py.

    source /usr/lib/openfoam/openfoam2312/etc/bashrc
    uv run python scripts/cfd/run_sloshing_convergence.py --work-dir /mnt/local-analysis/sloshing_cfd_work
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
    SloshingFreeDecayConfig,
    analyze_free_decay,
    build_free_decay_case,
)

_REPO = Path(__file__).resolve().parents[2]
_MANIFEST = _REPO / "docs" / "api" / "cfd" / "sloshing-convergence.json"

BREADTH = 1.0
TANK_HEIGHT = 1.0
FILL_LEVEL = 0.30          # h/L = 0.30 — exact cell-face fit for every mesh below
END_TIME = 12.0
STATIC_RATE_US = 12.25     # measured µs/cell·timestep for static free-decay (the estimator rate)

MESH_CPB = (40, 60, 80, 120, 160)   # {40,80,160} = constant ratio-2 triple for Richardson/GCI
RICH_TRIPLE = (40, 80, 160)
MESH_DT = 0.002
DT_SWEEP = (0.004, 0.002, 0.001)
DT_CPB = 80


def _case(work_dir: Path, cpb: int, dt: float, tag: str) -> Dict[str, Any]:
    name = f"conv_{tag}_cpb{cpb}_dt{str(dt).replace('.', 'p')}"
    cfg = SloshingFreeDecayConfig(
        breadth=BREADTH, tank_height=TANK_HEIGHT, fill_level=FILL_LEVEL,
        cells_per_breadth=cpb, delta_t=dt, end_time=END_TIME, name=name)
    case = work_dir / name
    done = case / "_result.json"
    if done.exists():
        prev = json.loads(done.read_text())
        if prev.get("status") == "completed":
            return prev
    build_free_decay_case(cfg, work_dir)
    res = OpenFOAMRunner(OpenFOAMRunConfig(run_set_fields=True, to_vtk=False)).run(case)
    cells = cpb * cfg.ny
    timesteps = round(END_TIME / dt)
    row: Dict[str, Any] = {
        "tag": tag, "cells_per_breadth": cpb, "cells": cells, "dt": dt,
        "timesteps": timesteps, "cell_size": round(BREADTH / cpb, 6),
        "status": res.status.value, "runtime_s": round(res.duration_seconds, 1),
        "predicted_runtime_s": round(STATIC_RATE_US * 1e-6 * cells * timesteps, 1),
    }
    if res.status.value == "completed":
        a = analyze_free_decay(case, cfg)
        row.update(
            freq_meas_hz=round(a["measured_frequency"], 6),
            freq_analytical_hz=round(a["analytical_frequency"], 6),
            rel_error_pct=round(a["relative_error"] * 100, 4))
    else:
        row["error"] = res.error_message
    done.write_text(json.dumps(row, indent=2) + "\n")
    return row


def _richardson(cpbs: List[int], freqs: List[float]) -> Optional[Dict[str, float]]:
    """Richardson extrapolation + GCI from the three finest (uniform-refinement) grids.

    Uses cell size h ∝ 1/cpb. Returns observed order p, extrapolated f0, and the
    fine-grid GCI (Roache, Fs=1.25). Needs a monotone triple with a non-degenerate ratio.
    """
    if len(cpbs) < 3:
        return None
    # three finest
    idx = sorted(range(len(cpbs)), key=lambda i: cpbs[i])[-3:]
    c1, c2, c3 = (cpbs[i] for i in idx)          # coarse->fine by cpb ascending
    f1, f2, f3 = (freqs[i] for i in idx)
    h1, h2, h3 = 1.0 / c1, 1.0 / c2, 1.0 / c3    # coarse->fine cell size (h1>h2>h3)
    r21 = h1 / h2
    r32 = h2 / h3
    e21 = f2 - f1
    e32 = f3 - f2
    if e32 == 0 or e21 == 0 or (e32 / e21) <= 0:
        return None
    # constant-ratio Richardson (approx if ratios differ slightly)
    r = 0.5 * (r21 + r32)
    try:
        p = math.log(abs(e21 / e32)) / math.log(r)
    except (ValueError, ZeroDivisionError):
        return None
    f0 = f3 + (f3 - f2) / (r ** p - 1.0)         # extrapolated (h->0)
    gci_fine = 1.25 * abs((f3 - f2) / f3) / (r ** p - 1.0)
    return {"observed_order_p": round(p, 3),
            "extrapolated_freq_hz": round(f0, 6),
            "gci_fine_pct": round(gci_fine * 100, 4)}


def main(argv: List[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Sloshing grid/timestep convergence (#1433)")
    ap.add_argument("--work-dir", type=Path, default=Path("/mnt/local-analysis/sloshing_cfd_work"))
    ap.add_argument("--workers", type=int, default=8)
    args = ap.parse_args(argv)
    args.work_dir.mkdir(parents=True, exist_ok=True)

    jobs = [("mesh", cpb, MESH_DT) for cpb in MESH_CPB]
    jobs += [("dt", DT_CPB, dt) for dt in DT_SWEEP if not (dt == MESH_DT)]  # 0.002@80 already in mesh
    print(f"running {len(jobs)} convergence cases with {args.workers} workers")
    rows: List[Dict[str, Any]] = []
    with ThreadPoolExecutor(max_workers=args.workers) as ex:
        futs = {ex.submit(_case, args.work_dir, cpb, dt, tag): (tag, cpb, dt) for tag, cpb, dt in jobs}
        for fut, key in futs.items():
            try:
                rows.append(fut.result())
            except Exception as exc:  # noqa: BLE001
                rows.append({"tag": key[0], "cells_per_breadth": key[1], "dt": key[2],
                             "status": "error", "error": str(exc)})
            print(f"  [{key}] {rows[-1]['status']} "
                  f"err={rows[-1].get('rel_error_pct','-')}% rt={rows[-1].get('runtime_s','-')}s")

    mesh = sorted([r for r in rows if r["tag"] == "mesh" and r["status"] == "completed"],
                  key=lambda r: r["cells_per_breadth"])
    # the fixed-dt 80-mesh row also serves the dt sweep
    dt_rows = sorted([r for r in rows if (r["tag"] == "dt" or (r["tag"] == "mesh" and r["cells_per_breadth"] == DT_CPB))
                      and r["status"] == "completed"], key=lambda r: -r["dt"])
    tri = [r for r in mesh if r["cells_per_breadth"] in RICH_TRIPLE]
    rich = _richardson([r["cells_per_breadth"] for r in tri],
                       [r["freq_meas_hz"] for r in tri]) if len(tri) >= 3 else None

    manifest = {
        "meta": {"generated_by": "scripts/cfd/run_sloshing_convergence.py",
                 "epic": "#1429", "issue": "#1433",
                 "case": f"rectangular free-decay, L={BREADTH} m, h/L={FILL_LEVEL}",
                 "solver": "interFoam (VOF), OpenFOAM ESI v2312",
                 "note": "h/L=0.30 lands exactly on a cell face for every mesh, so the analytical target is identical."},
        "analytical_freq_hz": mesh[0]["freq_analytical_hz"] if mesh else None,
        "mesh_sweep": [{k: r[k] for k in ("cells_per_breadth", "cells", "dt", "freq_meas_hz",
                        "rel_error_pct", "runtime_s", "predicted_runtime_s")} for r in mesh],
        "dt_sweep": [{k: r[k] for k in ("dt", "cells_per_breadth", "timesteps", "freq_meas_hz",
                      "rel_error_pct", "runtime_s")} for r in dt_rows],
        "richardson": rich,
    }
    _MANIFEST.parent.mkdir(parents=True, exist_ok=True)
    _MANIFEST.write_text(json.dumps(manifest, indent=2) + "\n")
    print(f"wrote {_MANIFEST.relative_to(_REPO)}")
    if rich:
        print(f"  observed order p={rich['observed_order_p']}, extrapolated "
              f"f0={rich['extrapolated_freq_hz']} Hz, fine-grid GCI={rich['gci_fine_pct']}%")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
