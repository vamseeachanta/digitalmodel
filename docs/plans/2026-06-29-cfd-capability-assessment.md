# CFD Capability Assessment — digitalmodel

**Date:** 2026-06-29
**Scope:** 2D and 3D geometry CFD for marine/offshore hydrodynamics + external aerodynamics
**Integration target:** Python-automated, config-driven workflows (the `digitalmodel` / Deckhand pattern)
**Author:** capability scoping pass (pre-implementation, for review — not self-approved)

---

## 0. TL;DR

This is **not greenfield**. digitalmodel already contains a ~85%-complete OpenFOAM
case-generation module (`solvers/openfoam`, 110 passing tests), a working gmsh meshing
module (`solvers/gmsh_meshing`, registered CLI), and adjacent CAD/FEA tooling. There is
also a **~20-issue open backlog** spanning the CFD pipeline. What is missing is not case
*authoring* — it is **execution, environment, wiring, validation, and reporting**.

The recommended initiative therefore is to **activate and consolidate**, not rebuild:
a phased epic that (a) makes the existing case-generation actually *run* end-to-end on a
reproducible environment, (b) adds known-answer validation gates, and (c) wraps it in the
same provenance-gated report backbone used by `installation_pamphlet` and the model-gen
pipeline. The scattered existing issues become children of this epic rather than being
duplicated.

---

## 1. Open-source toolchain (the answer to "what packages do we have")

Both target domains — marine free-surface hydrodynamics **and** external aerodynamics — are
covered by a single solver, which is the right consolidation for standing this up.

| Role | Tool | Status in repo |
|------|------|----------------|
| **Solver** | **OpenFOAM (ESI / openfoam.com)** — FV, 2D & 3D, VOF free-surface (`interFoam`), RANS/LES, 6-DOF | code targets it; **binaries not installed** |
| Solver (aero alt) | SU2 — compressible aero, adjoint optimization | not used; OpenFOAM `simpleFoam` covers our aero needs |
| **Geometry/CAD** | FreeCAD (+ pythonOCC / OpenCASCADE) | `solvers/blender_automation` + FreeCAD issues exist; FreeCAD not yet wired |
| **Meshing** | **gmsh** (2D + parametric 3D); snappyHexMesh / cfMesh (3D from STL) | `solvers/gmsh_meshing` **working**; gmsh in optional deps |
| **Wave gen (marine)** | olaFlow / waves2Foam (wave BCs + absorption) | wave BCs hand-built in `wave_models.py`; olaFlow not integrated |
| **Post / viz** | **PyVista** (headless, report-embeddable) + ParaView (interactive) | PyVista + meshio in **core deps** |
| **Automation** | PyFoam + in-repo config→case templating | in-repo templating exists; PyFoam not used |

**Recommended stack (unchanged from discussion):**
`FreeCAD + gmsh` (geometry/mesh) → `OpenFOAM ESI + olaFlow` (solve) → `PyVista/ParaView`
(post) → `Python orchestration` (run + report). Entirely free, Linux-native, no license
gate — which removes the Windows-licensed-host bottleneck that constrains the OrcaFlex/
OrcaWave lanes.

---

## 2. What already exists in the repo (inventory)

### 2.1 `src/digitalmodel/solvers/openfoam/` — case generation ~85% (per issue #139)
- `models.py` — `OpenFOAMCase`, `BoundaryCondition`, `SolverConfig`, `TurbulenceModel`,
  `DomainConfig`, `CaseType`
- `case_builder.py` — writes the standard OpenFOAM tree (`0/`, `constant/`, `system/`)
- `domain_builder.py` — parametric domain sizing + refinement zones
- `initial_fields.py` — U, p, k, omega, alpha.water field files
- `marine_solvers.py` — pre-configured `WaveLoadingSetup` (interFoam VOF),
  `CurrentLoadingSetup` (simpleFoam/pimpleFoam), `GreenWaterSetup`, `SloshingSetup`, `VIVSetup`
- `wave_models.py` — Stokes 2nd/5th, JONSWAP/PM irregular waves, current profiles
- `post_processing.py` + `results_models.py` — force/moment extraction, probes, free-surface elevation
- `parametric.py` — YAML-spec parameter-matrix case generation
- `spectral_analysis.py` — FFT/Welch spectra, natural-frequency extraction
- `cli.py` — Click CLI (`setup / mesh / run / postprocess`) **— not registered as an entry point**
- `templates/` — Jinja2 OpenFOAM dict templates
- **110 tests** across 6 files, all green

### 2.2 `src/digitalmodel/solvers/gmsh_meshing/` — working
- `mesh_generator.py`, `quality_analyzer.py`, `models.py`, `cli.py`
- Registered console script: `gmsh-meshing`; `gmsh` in `[solvers]` optional deps
- Tests under `tests/solvers/gmsh_meshing/`

### 2.3 Adjacent / supporting
- `solvers/blender_automation/`, `solvers/calculix/`, `solvers/fea_model/`
- `hydrodynamics/` — capytaine, bemrosetta, aqwa, diffraction (**potential-flow BEM = the
  validation targets** for CFD cross-checks)
- Core deps already include `meshio`, `pyvista`

---

## 3. The real gaps (this is the initiative)

| # | Gap | Evidence | Mirrors |
|---|-----|----------|---------|
| G1 | **No execution layer** — case is written but nothing invokes `blockMesh`/`snappyHexMesh`/solver as a subprocess | no `subprocess` run in `solvers/openfoam`; `cli.py run` not wired | `ansys/runner.py` (subprocess MAPDL, fail-closed) |
| G2 | **Environment not reproducible** — OpenFOAM binaries absent; no install spec | `command -v simpleFoam` → empty | item #3 of this initiative |
| G3 | **Not wired into `dm` engine** — no engine routing, no `usecase_registry` entry, openfoam CLI not in `project.scripts` | grep of `engine.py`, `registry.yaml` → no hits | `hydrodynamics`, `gmsh-meshing` entry points |
| G4 | **No known-answer validation** — no regression-gated benchmark cases | issue #155 asks for them; none exist | — |
| G5 | **External-aero path not framed/validated** — `simpleFoam` exists but no aero use-case (wind on topsides/vessel, Cd/Cl) or aero benchmark | `CurrentLoadingSetup` is single-phase RANS already | — |
| G6 | **No provenance-gated report** — CFD runs have no T1–T8 ledger + HTML report | other workflows have it | `installation_pamphlet`, model-gen pipeline |
| G7 | **Output storage unimplemented** — field data must live outside git (`/mnt/ace/cfd-output/`) | noted in #139, not built | — |

---

## 4. Existing issue topology (consolidate, do not duplicate)

| Group | Issues |
|-------|--------|
| Foundation / capability | **#139** OpenFOAM cap (85%), #63 engg-debt openFOAM, #64 engg-debt CAD, #92 engg-debt FreeCAD |
| Deep workflows | #153 gmsh deep, #154 OpenFOAM deep, #277 FreeCAD deep (plan-approved) |
| **Capstone pipeline** | **#155** FreeCAD→gmsh→OpenFOAM→ParaView, YAML-driven, known-answer validation |
| Marine applications | #470 seakeeping CFD (plan-approved), #637/#638/#639/#641/#642/#643 ballast-sloshing cluster, #619 sloshing structural, #643 CFD→roll coupling |
| CFD post | #660 FFT nat-freq, #661 wall pressure taps, #662 gmsh→polyMesh 3D bridge |
| Aero / business | **#630** Suzuka-aero feasibility (domain:cfd), #575 FOWT aero-hydro |

These already express most of the work. The missing piece is a **coordinating epic** that
sequences them and adds the cross-cutting gaps (G1, G2, G6, G7) that none of them own.

---

## 5. Recommended phased epic

> Sequencing rationale: nothing downstream is trustworthy until the existing case-gen can
> actually run on a reproducible box and pass a known answer. So environment + execution +
> one validated case come first; breadth (domains, pipeline, reporting) follows.

**Phase 0 — Environment & reproducibility** *(initiative item #3)*
Install OpenFOAM ESI + gmsh + olaFlow + PyVista on the Linux hosts; `uv`-managed Python
side; a `cfd doctor` command that verifies the toolchain. Closes G2. *Maps to: new env spec.*

**Phase 1 — Activate execution** *(initiative item #1, the scaffold)*
Subprocess runner mirroring `ansys/runner.py` (fail-closed, log-capturing); register the
openfoam CLI entry point; engine routing + `usecase_registry` entry; make one 2D case run
end-to-end (`blockMesh` → solver → `foamToVTK` → PyVista). Closes G1, G3.

**Phase 2 — Known-answer validation harness**
Cylinder drag Cd ≈ 1.0 @ Re=100 (2D laminar); flat-plate Cf vs Blasius (within 5%);
box-barge wave loading vs Morison/AQWA. Regression-gated in CI. Closes G4. *Adopts #155
validation core.*

**Phase 3 — CAD→CFD pipeline orchestrator**
YAML config → FreeCAD STEP → gmsh volume mesh (boundary layers) → OpenFOAM → PyVista/
ParaView state, with stage-gate validation (mesh quality, field-init, convergence).
*Adopts #155, #153, #277, #662.*

**Phase 4 — Provenance-gated reporting + output storage**
T1–T8 assumption/provenance ledger + HTML report via `build_pages`; field outputs to
`/mnt/ace/cfd-output/` with a run index. Closes G6, G7.

**Phase 5 — Domain applications (two parallel lanes)**
- *Marine:* seakeeping (#470), ballast-sloshing cluster (#637–#643), CFD→roll coupling (#643)
- *External aero:* new aero use-case (wind on topsides/vessel, Cd/Cl) + aero benchmark
  (e.g. flat-plate / known bluff-body Cd); assess #630 dataset against the now-real path

---

## 6. Validation philosophy (non-negotiable)

Every domain workflow ships with at least one **known-answer regression** before it is
considered "real." CFD without validation is decoration. The cross-check hierarchy:

1. Analytical / empirical (Blasius Cf, Morison, published Cd)
2. Potential-flow BEM already in-repo (capytaine / AQWA / OrcaWave) for marine loads
3. Published experimental data where available

This is the same provenance discipline the model-gen pipeline already enforces — CFD just
adds viscous fidelity where potential flow can't reach (separation, green water, sloshing,
slamming, wind).

---

## 7. What this assessment is asking for

Approval to (a) file the coordinating epic that adopts the existing issues into the phased
structure above, and then proceed with **item #1 (Phase 0–1 scaffold)** and **item #3
(environment spec)**. No code or environment changes have been made yet — this is scoping
only.
