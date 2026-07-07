#!/usr/bin/env python
"""3D sloshing MPI strong-scaling benchmark (issue #1433, epic #1429, item 3
prerequisite).

The 3D L-tank production run is gated on an UNMEASURED prerequisite: how well
does interFoam strong-scale across MPI ranks on this box (a-l-2, 32 cores)? The
run-time estimator only *predicts* ~1-2.5 h/case at 16-core assuming decent
scaling. This benchmark measures it: it builds ONE representative 3D forced-roll
case (a near-square plan-form, which is where 3D swirl matters — Faltinsen,
Rognebakke & Timokha 2003) and times a fixed physics window at a ladder of core
counts, reporting speed-up, parallel efficiency, and cells/rank so the machine
decision (this box vs a heavier node) is made from data.

It is a benchmark, not a validated production case: the geometry is a methodology
placeholder (real B_db/D_db proportions come from #640) and the timed window is
short. It reuses the validated prescribed-motion engine for the roll; the rest of
the case is standard interFoam (damBreak-derived, same dicts as the 2D suite).

    source /usr/lib/openfoam/openfoam2312/etc/bashrc
    uv run python scripts/cfd/run_sloshing_3d_benchmark.py \
        --work-dir /mnt/local-analysis/sloshing_cfd_work --cpb 60 --end-time 0.30 \
        --ranks 1 2 4 8 16 24 32
"""
from __future__ import annotations

import argparse
import json
import math
import re
import shutil
import subprocess
import time
from pathlib import Path
from typing import Any, Dict, List, Optional

from digitalmodel.solvers.openfoam.motion import (
    MotionType, PrescribedMotion, render_dynamic_mesh_dict_body)

G = 9.80665
_REPO = Path(__file__).resolve().parents[2]
_MANIFEST = _REPO / "docs" / "api" / "cfd" / "sloshing-3d-benchmark.json"

# Near-square plan-form (breadth x length) so the planar mode can go 3D (swirl);
# height gives freeboard. Methodology geometry — NOT the real B1546 tank.
BREADTH = 0.9          # x
HEIGHT = 0.9           # y
LENGTH = 0.9           # z (span / longitudinal)
FILL = 0.50            # h/L -> h = 0.45 m
ROLL_DEG = 4.0
_H = "FoamFile {{ version 2.0; format ascii; class {cls}; object {obj}; }}\n"


def _first_mode_period() -> float:
    # transverse first mode: omega1^2 = (pi g / L) tanh(pi h / L)
    h = FILL * BREADTH
    w2 = (math.pi * G / BREADTH) * math.tanh(math.pi * h / BREADTH)
    return 2.0 * math.pi / math.sqrt(w2)


def _blockmesh(nx: int, ny: int, nz: int) -> str:
    f = "{:.6g}".format
    return _H.format(cls="dictionary", obj="blockMeshDict") + f"""
scale 1;
vertices
(
    (0 0 0) ({f(BREADTH)} 0 0) ({f(BREADTH)} {f(HEIGHT)} 0) (0 {f(HEIGHT)} 0)
    (0 0 {f(LENGTH)}) ({f(BREADTH)} 0 {f(LENGTH)}) ({f(BREADTH)} {f(HEIGHT)} {f(LENGTH)}) (0 {f(HEIGHT)} {f(LENGTH)})
);
blocks ( hex (0 1 2 3 4 5 6 7) ({nx} {ny} {nz}) simpleGrading (1 1 1) );
edges ();
boundary
(
    leftWall  {{ type wall; faces ( (0 4 7 3) ); }}
    rightWall {{ type wall; faces ( (1 2 6 5) ); }}
    lowerWall {{ type wall; faces ( (0 1 5 4) ); }}
    frontWall {{ type wall; faces ( (0 3 2 1) ); }}
    backWall  {{ type wall; faces ( (4 5 6 7) ); }}
    atmosphere {{ type patch; faces ( (3 7 6 2) ); }}
);
mergePatchPairs ();
"""


_WALLS = ("leftWall", "rightWall", "lowerWall", "frontWall", "backWall")


def _field_U() -> str:
    walls = "\n".join(f"    {w} {{ type movingWallVelocity; value uniform (0 0 0); }}" for w in _WALLS)
    return _H.format(cls="volVectorField", obj="U") + f"""
dimensions [0 1 -1 0 0 0 0];
internalField uniform (0 0 0);
boundaryField
{{
{walls}
    atmosphere {{ type pressureInletOutletVelocity; value uniform (0 0 0); }}
}}
"""


def _field_prgh() -> str:
    walls = "\n".join(f"    {w} {{ type fixedFluxPressure; value uniform 0; }}" for w in _WALLS)
    return _H.format(cls="volScalarField", obj="p_rgh") + f"""
dimensions [1 -1 -2 0 0 0 0];
internalField uniform 0;
boundaryField
{{
{walls}
    atmosphere {{ type totalPressure; p0 uniform 0; }}
}}
"""


def _field_alpha() -> str:
    walls = "\n".join(f"    {w} {{ type zeroGradient; }}" for w in _WALLS)
    return _H.format(cls="volScalarField", obj="alpha.water") + f"""
dimensions [0 0 0 0 0 0 0];
internalField uniform 0;
boundaryField
{{
{walls}
    atmosphere {{ type inletOutlet; inletValue uniform 0; value uniform 0; }}
}}
"""


def _control(end_time: float, dt: float) -> str:
    # Write only at the end (minimise IO so the timing reflects the solver).
    return _H.format(cls="dictionary", obj="controlDict") + f"""
application interFoam;
startFrom startTime; startTime 0; stopAt endTime;
endTime {end_time:.6g}; deltaT {dt:.6g};
writeControl adjustableRunTime; writeInterval {end_time:.6g}; purgeWrite 1;
writeFormat ascii; writePrecision 8; writeCompression off;
timeFormat general; timePrecision 8; runTimeModifiable no;
adjustTimeStep yes; maxCo 0.9; maxAlphaCo 0.9; maxDeltaT 0.01;
"""


_FVSCHEMES = """
ddtSchemes { default Euler; }
gradSchemes { default Gauss linear; }
divSchemes
{
    div(rhoPhi,U) Gauss linearUpwind grad(U);
    div(phi,alpha) Gauss vanLeer;
    div(phirb,alpha) Gauss linear;
    div(((rho*nuEff)*dev2(T(grad(U))))) Gauss linear;
}
laplacianSchemes { default Gauss linear corrected; }
interpolationSchemes { default linear; }
snGradSchemes { default corrected; }
"""

_FVSOLUTION = """
solvers
{
    "alpha.water.*"
    {
        nAlphaCorr 2; nAlphaSubCycles 1; cAlpha 1;
        MULESCorr yes; nLimiterIter 5;
        solver smoothSolver; smoother symGaussSeidel; tolerance 1e-8; relTol 0;
    }
    "pcorr.*" { solver PCG; preconditioner DIC; tolerance 1e-5; relTol 0; }
    p_rgh { solver PCG; preconditioner DIC; tolerance 1e-07; relTol 0.05; }
    p_rghFinal { $p_rgh; relTol 0; }
    U { solver smoothSolver; smoother symGaussSeidel; tolerance 1e-06; relTol 0; }
    cellDisplacement { solver PCG; preconditioner DIC; tolerance 1e-06; relTol 0; }
}
PIMPLE { momentumPredictor no; nOuterCorrectors 2; nCorrectors 3; nNonOrthogonalCorrectors 0; }
relaxationFactors { equations { ".*" 1; } }
"""

_TRANSPORT = """
phases (water air);
water { transportModel Newtonian; nu 1e-06; rho 1000; }
air   { transportModel Newtonian; nu 1.48e-05; rho 1; }
sigma 0.07;
"""


def _setfields() -> str:
    h = FILL * BREADTH
    f = "{:.6g}".format
    return _H.format(cls="dictionary", obj="setFieldsDict") + f"""
defaultFieldValues ( volScalarFieldValue alpha.water 0 );
regions
(
    boxToCell
    {{
        box (0 0 0) ({f(BREADTH)} {f(h)} {f(LENGTH)});
        fieldValues ( volScalarFieldValue alpha.water 1 );
    }}
);
"""


def _decompose(n: int) -> str:
    return _H.format(cls="dictionary", obj="decomposeParDict") + f"""
numberOfSubdomains {n};
method scotch;
"""


def build_case(case: Path, cpb: int, end_time: float, dt: float) -> int:
    """Write a complete 3D interFoam forced-roll case; return the cell count."""
    if case.exists():
        shutil.rmtree(case)
    (case / "system").mkdir(parents=True)
    (case / "constant").mkdir()
    (case / "0").mkdir()
    nx = cpb
    ny = max(1, round(HEIGHT / BREADTH * cpb))
    nz = max(1, round(LENGTH / BREADTH * cpb))
    (case / "system" / "blockMeshDict").write_text(_blockmesh(nx, ny, nz))
    (case / "system" / "controlDict").write_text(_control(end_time, dt))
    (case / "system" / "fvSchemes").write_text(_H.format(cls="dictionary", obj="fvSchemes") + _FVSCHEMES)
    (case / "system" / "fvSolution").write_text(_H.format(cls="dictionary", obj="fvSolution") + _FVSOLUTION)
    (case / "system" / "setFieldsDict").write_text(_setfields())
    (case / "constant" / "transportProperties").write_text(
        _H.format(cls="dictionary", obj="transportProperties") + _TRANSPORT)
    (case / "constant" / "g").write_text(
        _H.format(cls="uniformDimensionedVectorField", obj="g")
        + "dimensions [0 1 -2 0 0 0 0];\nvalue (0 -9.81 0);\n")
    (case / "constant" / "turbulenceProperties").write_text(
        _H.format(cls="dictionary", obj="turbulenceProperties") + "simulationType laminar;\n")
    # Roll about the longitudinal (z) axis through the floor centre — engine YAW.
    roll = PrescribedMotion(MotionType.YAW, amplitude=ROLL_DEG,
                            period=_first_mode_period(),
                            origin=(0.5 * BREADTH, 0.0, 0.5 * LENGTH))
    (case / "constant" / "dynamicMeshDict").write_text(
        _H.format(cls="dictionary", obj="dynamicMeshDict") + "\n"
        + render_dynamic_mesh_dict_body(roll) + "\n")
    (case / "0" / "U").write_text(_field_U())
    (case / "0" / "p_rgh").write_text(_field_prgh())
    (case / "0" / "alpha.water").write_text(_field_alpha())
    return nx * ny * nz


def _sh(argv: List[str], cwd: Path, log: Path, timeout: int = 7200) -> int:
    with log.open("w") as fh:
        p = subprocess.run(argv, cwd=str(cwd), stdout=fh, stderr=subprocess.STDOUT,
                           timeout=timeout, check=False)
    return p.returncode


def _steps_from_log(log: Path) -> Optional[int]:
    try:
        return len(re.findall(r"^Time = ", log.read_text(errors="replace"), re.M))
    except OSError:
        return None


def run_at_ranks(work_dir: Path, cpb: int, end_time: float, dt: float,
                 n: int) -> Dict[str, Any]:
    """Fresh meshed case, then TIME only the solver stage at n ranks."""
    case = work_dir / f"s3d_bench_cpb{cpb}_np{n:02d}"
    ncells = build_case(case, cpb, end_time, dt)
    # setup (untimed): mesh + init + (decompose)
    if _sh(["blockMesh"], case, case / "log.blockMesh") != 0:
        return {"ranks": n, "cells": ncells, "status": "blockMesh_failed"}
    if _sh(["setFields"], case, case / "log.setFields") != 0:
        return {"ranks": n, "cells": ncells, "status": "setFields_failed"}
    parallel = n > 1
    if parallel:
        (case / "system" / "decomposeParDict").write_text(_decompose(n))
        if _sh(["decomposePar", "-force"], case, case / "log.decomposePar") != 0:
            return {"ranks": n, "cells": ncells, "status": "decomposePar_failed"}
    # timed: solver only
    solver_log = case / "log.interFoam"
    argv = (["mpirun", "-np", str(n), "--oversubscribe", "interFoam", "-parallel"]
            if parallel else ["interFoam"])
    t0 = time.monotonic()
    rc = _sh(argv, case, solver_log)
    wall = time.monotonic() - t0
    steps = _steps_from_log(solver_log)
    row: Dict[str, Any] = {
        "ranks": n, "cells": ncells,
        "cells_per_rank": round(ncells / n),
        "status": "completed" if rc == 0 else f"solver_rc_{rc}",
        "wall_s": round(wall, 2), "steps": steps,
        "s_per_step": round(wall / steps, 4) if steps else None,
    }
    # keep disk sane — drop processor dirs + written time dirs
    for pd in case.glob("processor*"):
        shutil.rmtree(pd, ignore_errors=True)
    return row


def collect(cpb: int, end_time: float, rows: List[Dict[str, Any]]) -> Dict[str, Any]:
    done = [r for r in rows if r["status"] == "completed" and r.get("s_per_step")]
    base = next((r for r in done if r["ranks"] == 1), None) or (
        min(done, key=lambda r: r["ranks"]) if done else None)
    for r in done:
        if base:
            sp = base["s_per_step"] * base["ranks"] / r["s_per_step"]  # normalise to 1-rank equiv
            r["speedup_vs_1rank"] = round(base["s_per_step"] / r["s_per_step"], 3)
            r["parallel_efficiency"] = round(
                (base["s_per_step"] / r["s_per_step"]) / (r["ranks"] / base["ranks"]), 3)
    ncells = done[0]["cells"] if done else None
    # extrapolate a production estimate at the best-efficiency rank
    best = max((r for r in done if r["ranks"] > 1),
               key=lambda r: r.get("parallel_efficiency", 0), default=None)
    manifest = {
        "meta": {"generated_by": "scripts/cfd/run_sloshing_3d_benchmark.py",
                 "solver": "interFoam (VOF), OpenFOAM ESI v2312, Open MPI 4.1.6",
                 "box": "a-l-2 / dev-secondary (32 cores)",
                 "epic": "#1429", "issue": "#1433",
                 "purpose": "MPI strong-scaling of a representative 3D forced-roll sloshing case to size the 3D L-tank run (this box vs a heavier node)",
                 "geometry": "methodology near-square plan-form (NOT the real B1546 tank; real B_db/D_db from #640)",
                 "note": "Timed stage = solver only (blockMesh/setFields/decomposePar are untimed setup). s_per_step normalises out any adaptive-dt step-count drift across ranks.",
                 "decomposition": "scotch", "timing": "solver wall-clock / timesteps"},
        "case": {"breadth_m": BREADTH, "height_m": HEIGHT, "length_m": LENGTH,
                 "fill_h_over_L": FILL, "roll_amplitude_deg": ROLL_DEG,
                 "cells_per_breadth": cpb, "cells": ncells, "end_time_s": end_time},
        "scaling": sorted(rows, key=lambda r: r["ranks"]),
        "best_efficiency_rank": best,
    }
    _MANIFEST.parent.mkdir(parents=True, exist_ok=True)
    _MANIFEST.write_text(json.dumps(manifest, indent=2) + "\n")
    return manifest


def main(argv: List[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="3D sloshing MPI scaling benchmark (#1433)")
    ap.add_argument("--work-dir", type=Path, default=Path("/mnt/local-analysis/sloshing_cfd_work"))
    ap.add_argument("--cpb", type=int, default=60)
    ap.add_argument("--end-time", type=float, default=0.30)
    ap.add_argument("--dt", type=float, default=0.001)
    ap.add_argument("--ranks", type=int, nargs="+", default=[1, 2, 4, 8, 16, 24, 32])
    args = ap.parse_args(argv)
    args.work_dir.mkdir(parents=True, exist_ok=True)
    print(f"3D benchmark: cpb={args.cpb}, end={args.end_time}s, ranks={args.ranks}")
    rows: List[Dict[str, Any]] = []
    for n in args.ranks:
        row = run_at_ranks(args.work_dir, args.cpb, args.end_time, args.dt, n)
        rows.append(row)
        print(f"[np={n:>2}] {row['status']:<16} cells={row.get('cells','-')} "
              f"wall={row.get('wall_s','-')}s steps={row.get('steps','-')} "
              f"s/step={row.get('s_per_step','-')}")
    m = collect(args.cpb, args.end_time, rows)
    print("\n=== strong scaling ===")
    for r in m["scaling"]:
        if r["status"] == "completed":
            print(f"  np={r['ranks']:>2}  s/step={r.get('s_per_step')}  "
                  f"speedup={r.get('speedup_vs_1rank','-')}x  "
                  f"eff={r.get('parallel_efficiency','-')}  "
                  f"cells/rank={r.get('cells_per_rank')}")
    print(f"-> {_MANIFEST.relative_to(_REPO)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
