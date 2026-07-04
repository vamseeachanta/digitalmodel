# Session handoff — CFD suite: 5→8 verified cases, interactive capability pages, marine milestone

**Dates:** 2026-07-01 → 2026-07-02 · **Epic:** #1161 · **Host:** ace-linux-2 (OpenFOAM v2312, license-free)

## What shipped (all MERGED, all verified on real OpenFOAM v2312)

| PR | Closes | Delivered |
|----|--------|-----------|
| #1260 | #1165 | **Dam break (Martin & Moyce 1952)** — first VOF case: surge front mean 3.0%/max 8.1% (with the literature-standard +0.175 gate-release shift), mass drift 6e-7; runner grew `run_set_fields`; 10 s solve-gated test |
| #1264 | — | Side-fix: main's Quality Gates red after #1252 (abs-path violations) — `# abs-path-allowed` exemptions |
| #1278 | #1276 | **Interactive upgrade of all 5 reports** (user review: "why not interactive / where's the VIV animation") — Plotly charts, scrubbable Kármán vortex-street (50 frames) + dam-break (51 frames) animations, live VIV lock-in calculator (DNV 4≤Ur≤8 band), per-case "what this gates in real work" panels; every number re-solved fresh and reproduced |
| #1291 | #1170, #1169 | **Marine pair** — numerical wave tank (dispersion +0.2%, H ≤4.7%, Kr=0.010 via 7-gauge Goda–Suzuki split) + floating-body heave decay (draft +1.1% vs Archimedes, 11 cycles, implied Ca≈0.39); runner grew `topoSet`/`subsetMesh`; both have 2.5–3 min solve-gated tests |
| #1304 | #1172 | **Kleefsman/MARIN green-water impact** — official SPHERIC Test 2 data (geometry + traces, nothing digitized): P2 peak **+1.5%**, arrival +3.5%, H2/H4 MAE 4.3%/2.1% on half the Kleefsman grid; density-aware probe placement regression-tested; heavy solves opt-in via `DIGITALMODEL_RUN_LONG_CFD=1` |

**Suite: 8/8** — laminar BL, bluff body, turbulent RANS, airfoil lift, free-surface VOF, wave generation, 6-DOF floating body, green-water impact.

## Live links
- Suite index (8/8, interactive): https://vamseeachanta.github.io/digitalmodel/cfd/
- New this session: [dam break](https://vamseeachanta.github.io/digitalmodel/cfd/dam-break-verification.html) · [wave tank](https://vamseeachanta.github.io/digitalmodel/cfd/wave-tank-verification.html) · [floating body](https://vamseeachanta.github.io/digitalmodel/cfd/floating-body-decay-verification.html) · [Kleefsman impact](https://vamseeachanta.github.io/digitalmodel/cfd/kleefsman-impact-verification.html)
- Capabilities page CFD section (8 cards, interactivity noted): https://vamseeachanta.github.io/digitalmodel/capabilities/#cfd

## Issues filed / updated
- **#1261 (OPEN)**: pre-existing cylinder-builder solve failure found while landing #1165 — generic `fvSchemes` template missing the laminar momentum-diffusion div entry + the #1189 box case can't shed (needs the #1219 O-grid + tilt).
- **#1302 (OPEN, parked with diagnosis)**: wave-excited floating body — builds and floats correctly, but the 2D-slab (`empty`) mesh morphing collapses a cell at first wave arrival (`deltaCoeffs` FPE in `dynamicMotionSolverFvMesh`; reproduced across decompositions/CFL/blend distances; restart dt→1e-14). 3D floatingObject works, so the 2D slab is the differentiator. **Recommended: overset (`overInterDyMFoam`)** or a 3D thin tank. Builder NOT shipped unverified (the #1261 lesson).

## Engineering gotchas recorded (also in auto-memory `cfd-capability-initiative.md`)
- Golden numbers must be fetched + cited, never recalled: M&M data verified via chaos-polymtl/lethe; Kleefsman via the official SPHERIC zip (`test_case_2_exp_data.xls`).
- VOF meshes: size `dx = a/N` so initialization boxes land on cell faces (else `boxToCell` mis-initializes).
- Wave tanks: exclude gauges <1λ from the wavemaker (evanescent near-field, +8% H); the naive fixed-point dispersion iteration diverges in shallow water — use Newton.
- Floating bodies: the period excess over the waterplane-stiffness value *is* the added mass — gate as a physical band; closed tanks need the `k/(1−Awp/Atank)` stiffness correction.
- Impact probes: offset half a *local* cell off faces — fixed offsets land inside the carved obstacle at other densities (coarse run read P2=0).
- Parallel runs need `reconstructPar` before pyvista; `sixDoFRigidBodyState` FO exists only for the sixDoF solver; FOs added mid-run (runTimeModifiable) register late — parse the `report on` log lines with per-timestep dedupe.
- Shared-clone/worktree discipline: work in `/tmp/claude-1000/wt-*` worktrees (scratchpad-path worktrees get auto-cleaned mid-session); `ln -s /mnt/local-analysis/assetutilities /tmp/claude-1000/` for worktree `uv`; drop `uv.lock` before committing.

## Next steps (backlog under #1161)
1. **#1302 overset floating body in waves** — scaffolding written (in the parked scratchpad + issue notes); the path to CFD RAO spot-checks vs the OrcaWave/AQWA pipeline.
2. **#1171 wave force on a vertical cylinder** (MacCamy–Fuchs) — 3D wave tank + cylinder; heavier compute, geometry now trivial with the NWT builder.
3. **#1173 hull resistance** (Wigley) · **#1175–#1177 aero** · **#1261 cylinder-builder fix**.
4. Phase 3 CAD→CFD pipeline (#155); Phase 4 provenance run-index; remote-SSH compute (#1179) would remove the single-host wall-clock ceiling that shaped this session's fast-variant strategy.

## Where things live
- Modules: `src/digitalmodel/solvers/openfoam/validation/{dam_break,wave_tank,floating_body,kleefsman}.py` (+ registry in `__init__.py`)
- Cases: `docs/api/cfd/cases/{dam_break,wave_tank,floating_body_decay,kleefsman_impact}/` (each: dicts + README + analyzer + provenance; Kleefsman also carries the official experimental CSV)
- Report generators: `docs/api/cfd/report_build/` (house style + flipbook animation component + per-case builders, regeneration README)
- Tests: `tests/solvers/openfoam/validation/` — 61 pass without OpenFOAM; solve-gated tests run on solver hosts (dam break 10 s, plates ~2 min, NWT ~2.5 min, floating body ~3 min; Kleefsman opt-in via `DIGITALMODEL_RUN_LONG_CFD=1`)
