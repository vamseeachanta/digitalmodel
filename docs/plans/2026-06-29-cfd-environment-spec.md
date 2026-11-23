# CFD Environment & Reproducibility Spec — digitalmodel

**Date:** 2026-06-29
**Epic:** #1161 — Phase 0
**Goal:** A reproducible, documented OpenFOAM CFD toolchain on the Linux hosts so the
existing `solvers/openfoam` case-generation (and the new fail-closed runner) can actually
execute, end-to-end, with a single verification command.
**Scope:** Linux (Ubuntu). The whole point of choosing OpenFOAM is that CFD compute runs
**license-free on Linux** — it does not need the Windows licensed host that gates OrcaFlex/
OrcaWave/ANSYS.

---

## 0. Layered design

| Layer | Provided by | Managed how |
|-------|-------------|-------------|
| Solver + utilities (`blockMesh`, `interFoam`, `simpleFoam`, `snappyHexMesh`, `foamToVTK`) | **OpenFOAM ESI (openfoam.com)** | system package (apt) — *outside* uv |
| Wave generation/absorption (marine) | **olaFlow** | source build against OpenFOAM |
| Meshing | **gmsh** | uv (`[solvers]` optional dep, already declared) |
| Geometry/CAD | **FreeCAD** (+ pythonOCC) | system package (apt) |
| Post / I/O | **PyVista**, **meshio** | uv (already in core deps) |
| Orchestration | `digitalmodel.solvers.openfoam` | this repo |

Rationale: OpenFOAM is a large C++ toolchain with its own build/runtime environment
(`source /usr/lib/openfoam/.../etc/bashrc`); it is **not** a pip package and must not be
forced under uv. uv owns only the Python-side libraries. This split is why the runner
(`runner.py`) shells out to the utilities rather than importing a Python binding.

---

## 1. OpenFOAM ESI install (Ubuntu)

Use the official openfoam.com apt repository (ESI fork — chosen for its marine/olaFlow
ecosystem and `waveFoam` lineage).

```bash
# 1. Add the ESI OpenFOAM apt repo (one-time)
curl https://dl.openfoam.com/add-debian-repo.sh | sudo bash

# 2. Install a pinned version (pin for reproducibility — do NOT float 'latest')
sudo apt-get update
sudo apt-get install -y openfoam2406-default     # OpenFOAM v2406

# 3. Source the environment (per shell; add to the CFD host's profile)
source /usr/lib/openfoam/openfoam2406/etc/bashrc

# 4. Verify the utilities the runner invokes are now on PATH
command -v blockMesh interFoam simpleFoam snappyHexMesh foamToVTK
```

**Pin policy:** record the exact package (`openfoam2406`) in this spec and in the run
provenance ledger (Phase 4). A version bump is a deliberate, reviewed change — CFD results
are not comparable across solver versions without re-validation.

**Sourcing note:** the runner detects availability via `shutil.which("blockMesh")`. If the
utilities are not found, the runner **fail-closes to DRY_RUN** rather than producing a false
result — so an un-sourced shell is safe (it degrades, it doesn't lie). For automated/lane
runs, ensure the OpenFOAM `etc/bashrc` is sourced in the environment that invokes
`openfoam run` (e.g. a wrapper or the host's non-interactive profile).

---

## 2. olaFlow (marine wave generation) — optional, Phase 5 marine lane

Only needed for the free-surface marine workflows (wave loading, green water). The current
`wave_models.py` hand-builds wave BCs; olaFlow adds active wave generation + absorption.

```bash
source /usr/lib/openfoam/openfoam2406/etc/bashrc
git clone https://github.com/phicau/olaFlow.git
cd olaFlow && ./allMake
```

Defer until Phase 5; not required for the 2D validation cases (Phase 2) or aero.

---

## 3. FreeCAD (geometry/CAD) — Phase 3

```bash
sudo apt-get install -y freecad        # or the AppImage for a pinned version
```

Used headless via its Python console for STEP export in the CAD→CFD pipeline (#155, #277).
Pin via AppImage if the apt version lags.

---

## 4. Python side (uv-managed — already declared)

Already in `pyproject.toml`:
- core deps: `pyvista`, `meshio`
- `[solvers]` optional: `gmsh`

```bash
uv sync --extra solvers        # pulls gmsh; pyvista/meshio are core
```

No new Python deps are required for Phase 0–2. (`PyFoam` is optional convenience for case
manipulation; the runner does not depend on it.)

---

## 5. `cfd doctor` — verification command (to build in Phase 1)

A single command that reports toolchain readiness, mirroring `orcawave doctor`. Proposed
checks:

| Check | Pass condition |
|-------|----------------|
| OpenFOAM utilities | `blockMesh`, `<solvers>`, `foamToVTK` resolvable on PATH |
| OpenFOAM version | matches the pinned version in this spec |
| gmsh | `import gmsh` succeeds |
| PyVista / meshio | importable |
| Output root | `/mnt/ace/cfd-output/` exists and is writable |
| Smoke | `openfoam run --dry-run` on a generated case returns `DRY_RUN`/`COMPLETED` cleanly |

Until `cfd doctor` exists, the equivalent manual check is:

```bash
source /usr/lib/openfoam/openfoam2406/etc/bashrc
uv run openfoam setup --type current_loading --name doctor_case -o /tmp
uv run openfoam run /tmp/doctor_case          # COMPLETED if OpenFOAM present; exits 2 if not
```

---

## 6. Output storage (gap G7)

Per #139: case *inputs* are git-tracked in-repo; *outputs* (field data, VTK, postProcessing/)
are large and live **outside git**.

- Output root: `/mnt/ace/cfd-output/` (shared across machines), fallback `/mnt/dde/cfd-output/`
- Layout: `<output_root>/<project>/<case_name>/` containing the OpenFOAM tree
- Run index: register each run (case id, solver, version, runtime, paths) in the doc index
  (`source: cfd_output`) — built in Phase 4
- Cleanup: archive completed runs to compressed tar; drop intermediate time directories
- Small/reproducible artifacts (ParaView `.pvsm` state, the YAML case spec) stay in-repo

This spec does **not** create these directories; it documents the contract that Phase 1's
runner config and Phase 4's reporting will honor.

---

## 7. Reproducibility checklist

- [ ] OpenFOAM version pinned (`openfoam2406`) and recorded here + in run provenance
- [ ] `etc/bashrc` sourced in the lane/automation environment (non-interactive profile)
- [ ] `uv sync --extra solvers` run on the CFD host
- [ ] `/mnt/ace/cfd-output/` exists + writable
- [ ] `cfd doctor` green (once built) — or the manual smoke in §5
- [ ] Known-answer validation (Phase 2) green on this host before any production run

---

## 8. What this spec is / isn't

**Is:** the documented, pinned recipe to make CFD compute reproducible on a Linux host, and
the contract the runner + reporting bind to.

**Isn't:** an automated installer. Installing OpenFOAM/FreeCAD is a privileged, host-level,
one-time action best done by the operator (`! sudo ...` in-session, or by hand). Phase 1's
`cfd doctor` will *verify* the result; it will not perform the privileged install.
