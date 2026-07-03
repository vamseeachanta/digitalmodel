# Wave-excited floating body — overset heave-RAO verification case

Reference OpenFOAM case backing the verification report
[`../../wave-excited-body-verification.html`](../../wave-excited-body-verification.html)
(digitalmodel issue #1302, epic #1161).

The suite's first **wave-body interaction** case: a 0.2 × 0.2 m half-density box floats
at its Archimedes draft at x = 13 m in the verified #1170 regular wave (StokesII,
H = 0.05 m, T = 3.0 s, d = 0.4 m, λ = 5.77 m), constrained to pure heave (#1169
pattern) on a **rigid overset component mesh** (`overInterDyMFoam`). In the
long-wavelength limit (λ/B ≈ 29, T ≫ heave natural period) the body is a wave
follower: **heave RAO → 1** (Newman 1977, ch. 6).

## Why overset

The deforming-mesh attempt (`dynamicMotionSolverFvMesh`, #1169 machinery unchanged)
collapses a waterline corner cell at first wave arrival — `deltaCoeffs` FPE,
reproducible across decompositions/CFL/blend distances (diagnosis on #1302). The
overset component moves rigidly, so no cell ever deforms; the failure is eliminated
structurally, not tuned away. Note `sixDoFRigidBodyState` refCasts the mesh to
`dynamicMotionSolverFvMesh` and FATALs under `dynamicOversetFvMesh` — heave is parsed
from the sixDoF `report on` log lines instead.

## Result (background 375×49, component 50×42, 34 s)

| Metric | Result | Gate |
|--------|--------|------|
| Heave RAO (2A_z / H_incident) | **1.017** (+1.7%) | 1.0 ± 15% |
| Mean draft vs Archimedes (0.10 m) | **+2.1%** | ± 5% |
| Response period vs wave period (3 s) | **3.001 s** (+0.03%) | ± 5% |
| Incident split at the upstream array | 2a_i = 0.0472 m, Kr = 0.098 | consistency |

The incident amplitude comes from a Goda–Suzuki least-squares split over the gauges at
x = 8–12 m (≥ 1λ from the wavemaker, upstream of the component region), so the body's
scattered field cannot pollute the RAO denominator — minus the x = 8.8 m gauge, whose
sample ray lands bit-exactly on a cell face at *both* resolutions and double-counts cell
columns (mean level 0.47/0.78 m vs the true 0.40 m); `incident_wave_split` detects this
by a mean-level guard and excludes it in the open. The fast regression variant
(background 250×35, ~75 min at 8-way / ~2.5 h serial) reproduces: RAO 1.006,
draft +2.0%, period 3.002 s — mesh-consistent to ~1%.

## Layout

Two sub-cases (the overset recipe from the v2312 `floatingBody` tutorial):

- `body/` — component mesh: block around the box, hole carved by
  `blockMesh → topoSet → subsetMesh -overwrite c0 -patch floatingObject`
- `background/` — the verified #1170 NWT (top raised to 0.7 m) that runs the solver:
  `blockMesh → mergeMeshes . ../body -overwrite → topoSet → setFields → overInterDyMFoam`

## Reproduce

```bash
# regenerate from the in-repo builder (this archive = reference mesh):
uv run python -c "
from digitalmodel.solvers.openfoam.validation import WaveExcitedBodyConfig, build_wave_excited_body_case
build_wave_excited_body_case(WaveExcitedBodyConfig(nx=375, nz=49, write_interval=0.25,
                                                   name='wave_excited_body'), '.')"
cd wave_excited_body/body && blockMesh && topoSet && subsetMesh -overwrite c0 -patch floatingObject
cd ../background && blockMesh && mergeMeshes . ../body -overwrite && topoSet && setFields
# serial (slow) or parallel — keep the overset region inside one x-slab:
decomposePar && mpirun -np 10 overInterDyMFoam -parallel && reconstructPar -newTimes
cd ../.. && uv run python wave_excited_body/analyze_wave_excited_body.py wave_excited_body ./results 375 49 50 42
```

Or drive it through the runner (`OpenFOAMRunConfig(run_solver=False)` mesh-prep on
`body/`, then `OpenFOAMRunConfig(merge_meshes_source="../body", run_topo_set=True,
run_set_fields=True)` on `background/`) — the exact sequence the regression test uses.

## Engineering notes (hard-won)

- **Parallel decomposition**: hierarchical x-slabs must NOT cut the overset component
  region — a straddling slab boundary makes every step pay cross-processor donor
  search (measured 6× slower per cell than the slab-aligned run).
- **Component resolution**: refining the component mesh below dx = 0.02/dz = 0.01
  halves the adjustable time step via spurious air-side velocities near the body
  (max Courant pinned in one cell while the mean is O(1e-3)) for no gate-relevant
  gain; refine the *background* (wave fidelity) instead.
- **Face alignment**: still-water level, body edges and component extents all land on
  cell faces of their respective meshes (the #1165 VOF lesson; `nz` a multiple of 7
  keeps depth 0.4 on a face of the 0.7 m-tall background).

## Citations

- `$FOAM_TUTORIALS/multiphase/overInterDyMFoam/floatingBody` (ESI v2312) — overset
  mesh-merge/zoneID recipe and settings; `twoSquaresOutDomain` — 2D overset precedent.
- `$FOAM_TUTORIALS/multiphase/interFoam/laminar/waves/stokesII` (ESI v2312) — wave
  generation/absorption, verified in #1170.
- Newman, J.N. (1977). *Marine Hydrodynamics*, MIT Press — quasi-static long-wave response.
- Goda, Y. & Suzuki, Y. (1976) — incident/reflected wave resolution from gauge arrays.

## Heave-RAO sweep (#1324)

The same case swept across wave period turns the single long-wave point into a
**heave-RAO curve** — from the wave-follower limit (RAO → 1) toward heave
**resonance** (reference peak ≈ 2.2 near T ≈ 0.87 s).
Report: [`../../wave-excited-body-rao-verification.html`](../../wave-excited-body-rao-verification.html).

**Result (CFD points T = 3.0 reused, 1.5, 1.0, 0.9):**

| T (s) | CFD RAO | ref RAO | region | verdict |
|------:|--------:|--------:|--------|---------|
| 3.0 | 1.017 | 1.014 | off-resonance | PASS (reused #1302) |
| 1.5 | 1.056 | 1.087 | off-resonance | PASS (+3 %) |
| 1.0 | 0.992 | 1.524 | resonance | reduced — open finding |
| 0.9 | 0.744 | 2.053 | resonance | reduced — open finding |

Off resonance the CFD matches the inviscid reference to a few percent. Near
resonance the CFD stays close to a wave-follower (RAO ≈ 0.7–1.0), far below the
inviscid peak. A two-resolution study (18 vs 36 cells/wavelength) shows this
reduction is **grid-independent** — refining recovers incident amplitude but the
heave scales with it, so the RAO barely moves — i.e. the reduction is in the
body's dynamic response, not the wave field.

Each CFD point is spot-checked against a frozen **potential-flow diffraction
reference** — the same physics the OrcaWave/AQWA pipeline implements, computed
2D-consistently for the exact section:

- `rao_reference_capytaine.csv` — the frozen reference curve (capytaine linear BEM,
  Froude-Krylov + diffraction, finite depth 0.4 m, long box ≈ 2D slab, irregular
  frequencies removed by an interior lid). Reproduces the #1302 CFD long-wave point
  (RAO 1.017 at T = 3 s) from an independent method. Regenerate with
  `generate_rao_reference.py` (needs `capytaine`; **not** a digitalmodel runtime
  dependency — the CSV is committed like the Kleefsman MARIN data).

Per-period drivers (module `digitalmodel.solvers.openfoam.validation.wave_excited_body_rao`):

- `build_sweep_config(T)` adapts the verified geometry per period — background `nx`
  (≥ 18 cells/wavelength), gauge array (spacing ≈ 0.2 λ so the split does not alias),
  solve duration / analysis window (from the group velocity). `nz = 49` keeps the
  0.4 m waterline on a cell face at every period.
- `analyze_rao_sweep_point.py <T> <case_root> <out>` → per-period `results.json`
  (incident split + heave RAO + reference RAO + band gate).
- `aggregate_rao_sweep.py <results_root> <out.json>` folds the points (plus the
  reused #1302 T = 3 s point) into one `sweep_results.json` for the report.

Run one sweep point (parallel overset, 10× 2 m x-slabs keep the component in one slab):

```
# build via build_sweep_config, then the #1302 mesh+solve sequence per period;
# see the module + analyze_rao_sweep_point.py. The long multi-solve regression is
# opt-in: DIGITALMODEL_RUN_LONG_CFD=1 on a solver host.
```

**Gate.** The band gate applies **off resonance** (±20 %), where the inviscid
reference is accurate ground truth — and there the CFD passes. In the resonance
region the inviscid reference is only an upper bound (no viscous damping), so
those points are reported as a measured, grid-independent discrepancy — an **open
finding**, not graded pass/fail against a reference that is not ground truth
there. The reduction is consistent with the separation damping of a sharp-cornered
bluff square that the BEM omits, but the present forced-wave setup cannot separate
that from a residual gauge-to-body incident-wave decay or overset numerical
damping. **Recommended follow-up:** a free-decay test (displace in still water,
fit the decaying oscillation) measures the damped natural period and damping ratio
directly — attributing the reduction cleanly and confirming the body's natural
frequency.

- Newman/Faltinsen seakeeping texts — heave frequency response of a floating section.
- capytaine (linear BEM) — the in-repo potential-flow cross-check tier of #1161.
