# Session handoff — CFD suite 8→9: wave-excited floating body via 2D overset (#1302)

**Date:** 2026-07-02 · **Epic:** #1161 · **Host:** ace-linux-2 (OpenFOAM v2312, license-free)

## What shipped (PR #1321, closes #1302)

The suite's first **wave-body interaction** case, unparked from the morphing-mesh
collapse by switching to overset (`overInterDyMFoam`). Verified on real v2312:

| Gate | Reference (bg 375×49) | Fast (250×35) | Band |
|------|----------------------|---------------|------|
| Heave RAO vs long-wave wave-follower limit | **1.017** (+1.7%) | 1.006 | 1.0 ± 15% |
| Mean draft vs Archimedes | +2.1% | +2.0% | ± 5% |
| Response period vs T = 3 s | 3.001 s | 3.002 s | ± 5% |

Design: verified #1170 NWT background (top raised 0.55 → 0.7 m so the overset fringe
stays interior) + rigid component mesh around a 0.2 m half-density box at x = 13 m
(λ/B ≈ 29), hole by topoSet/subsetMesh, heave-only sixDoF (#1169 constraints), zoneID
recipe from the v2312 `floatingBody` overset tutorial (2D-slab overset legitimacy per
`twoSquaresOutDomain`). Body starts *at* its Archimedes draft — no decay transient.

Delivered: `validation/wave_excited_body.py` (+ registry), runner `merge_meshes_source`
+ `run_solver=False` stages, 18 always-on tests + `DIGITALMODEL_RUN_LONG_CFD=1` solve
test, archived case + analyzer + README, interactive report (wave-riding animation,
heave/wave-follower/gauge charts), suite index 9/9, capabilities cards.

## Live links
- Report: https://vamseeachanta.github.io/digitalmodel/cfd/wave-excited-body-verification.html
- Suite index (9/9): https://vamseeachanta.github.io/digitalmodel/cfd/
- Capabilities CFD section: https://vamseeachanta.github.io/digitalmodel/capabilities/#cfd

## Engineering lessons (hard-won, recorded in the report + case README)

1. **Overset retires the morphing collapse structurally** — the deforming-mesh run dies
   at first wave arrival (waterline corner cell sheared to zero volume, deltaCoeffs FPE);
   the rigid overset component never deforms a cell. This is the machinery for all future
   large-motion cases (RAO sweeps, installation lowering, float-over).
2. **`sixDoFRigidBodyState` FATALs under `dynamicOversetFvMesh`** (refCasts the mesh to
   `dynamicMotionSolverFvMesh`) — parse the sixDoF `report on` log lines with per-timestep
   dedupe instead.
3. **Parallel overset decomposition must not cut the component region** — a hierarchical
   x-slab boundary straddling the overset block measured ~6× slower per cell
   (cross-processor donor search every step). 10-way 2 m slabs keep [12.5, 13.5] inside one.
4. **Refine the background, not the component** — component refinement below dx 0.02/dz 0.01
   pins the adjustable time step on spurious ~2 m/s air-side velocities in one near-body
   cell (max Co pinned while mean Co is O(1e-3)), tripling cost for no gate-relevant gain.
5. **`interfaceHeight` gauges must not sit bit-exactly on a cell face** — the x = 8.8 m
   gauge landed on-face at *both* resolutions (110×0.08 and 165×(20/375) both reproduce
   8.8 in floats) and double-counted cell columns, reporting mean levels of 0.78/0.47 m in
   a 0.4 m tank. `incident_wave_split()` guards on mean level and excludes such gauges in
   the open (unit-tested); the naive split had read RAO 0.888/0.985 from the corrupt gauge.

## Next steps (backlog under #1161)
1. **CFD RAO curve** — this case at shorter wavelengths (RAO departs from 1) → spot-checks
   vs the OrcaWave/AQWA diffraction pipeline (docs/api/hydro/). The overset machinery and
   the incident-split are now verified pieces.
2. **#1171 wave force on a vertical cylinder** (MacCamy–Fuchs) — 3D wave tank + cylinder.
3. **#1173 Wigley hull resistance** · **#1175–#1177 aero** · **#1261 cylinder-builder fix**
   · **#1179 remote-SSH compute** · **#155 CAD→CFD**.

## Where things live
- Module: `src/digitalmodel/solvers/openfoam/validation/wave_excited_body.py`
- Case + analyzer: `docs/api/cfd/cases/wave_excited_body/` (background/ + body/ layout)
- Report generator: `docs/api/cfd/report_build/build_wave_excited_body.py`
- Tests: `tests/solvers/openfoam/validation/test_wave_excited_body.py` (+ runner stage
  tests in `tests/solvers/openfoam/test_runner.py`)
- Auto-memory: `cfd-capability-initiative.md` (gotchas mirrored there)
