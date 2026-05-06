# Plan: digitalmodel #470 — Seakeeping analysis using OpenFOAM (CFD-based ship motion simulation)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/470
**Status:** plan-approved
**Tier:** T3 (multi-phase capability addition)
**Related:** #154, #139, #155, #464
**Machine assignment (2026-05-06 user directive):** **`ace-linux-2`** — CFD computational constraints make this unsuitable for `dev-primary`. All Phase 2+ runs (`interDyMFoam`/`overInterDyMFoam` + wave library) execute on ace-linux-2. Phase 1 literature survey + tooling map can run anywhere; Phase 2 spike onward is ace-linux-2-only.

## Context

The hydrodynamics module already supports potential-flow BEM (Capytaine #464, OrcaWave wrapper, AQWA legacy) at `src/digitalmodel/hydrodynamics/`. The gap is viscous/nonlinear effects: slamming, green water, large-amplitude motions, added resistance — all of which require CFD. OpenFOAM's `interDyMFoam` / `overInterDyMFoam` plus a wave library (`olaFlow` preferred per actively-maintained status; `waves2Foam` legacy) is the canonical path.

This issue is intentionally multi-phase (Phases 1-4 in the body). Plan-now scope: deliver Phase 1 (literature/method survey + tooling map) and a Phase 2 spike against the Wigley hull, with Phases 3-4 broken out into follow-on issues. This avoids a months-long single-PR and gives the team a real validated benchmark before adding scope.

No OpenFOAM scaffold exists in `src/digitalmodel/`. There is a `solvers/openfoam/` adjacent area per #154 — verify before assuming a clean slate.

## Plan

1. **Confirm OpenFOAM tooling state.** Run `find . -path "*/solvers/openfoam*" -name "*.py" | head` and `grep -r "interDyMFoam\|olaFlow\|waves2Foam" src/ docs/ --include="*.md" --include="*.py"`. Catalog whatever exists, especially anything from #154 (deep-solver workflows). The plan diverges from here based on findings: clean slate → write Phase 1 doc + create skeleton; existing scaffold → extend it.

2. **Phase 1 deliverable: `docs/domains/openfoam/seakeeping/README.md`.** New 200-300 line document with:
   - Tutorial inventory: `interDyMFoam` motion of floating bodies in waves (`$FOAM_TUTORIALS/multiphase/interFoam/RAS/floatingObject` etc.)
   - Mesh-motion strategy comparison table (morphing | overset/chimera | sliding-mesh) with mooring-vs-seakeeping guidance
   - Wave-library decision: `olaFlow` for new work; document `waves2Foam` only as legacy
   - Benchmark catalog: Wigley hull (most-cited), DTMB 5415 (US Navy frigate), KCS (KRISO container ship) with experimental-data citation links
   - Tool-chain map: snappyHexMesh / cfMesh, ParaView post, PyFoam scripting

3. **Phase 2 spike: Wigley hull setup.** New directory `src/digitalmodel/hydrodynamics/openfoam_seakeeping/`:
   - `__init__.py` — empty for v0
   - `case_template.py` — emits an OpenFOAM case directory from a `spec.yml` (mesh, boundary conditions, wave dictionary, control dict)
   - `runner.py` — wraps `Allmesh` / `Allrun` shell scripts; logs to `logs/<case>/`
   - `rao_extractor.py` — reads `postProcessing/sixDoFRigidBodyState/0/sixDoFRigidBodyState.dat` and computes heave/pitch RAO via FFT in head seas
   - First spec: `docs/domains/openfoam/seakeeping/wigley_hull/spec.yml` driving regular wave heave/pitch RAO at three wave periods
   No actual OpenFOAM execution required for this issue's PR — the runner is exercised in dry-run mode (writes the case dir, asserts file structure).

4. **YAML schema lock.** New `src/digitalmodel/hydrodynamics/openfoam_seakeeping/schema.py` defining the spec.yml shape:
   ```python
   @dataclass
   class SeakeepingSpec:
       hull: HullConfig          # mesh path, displacement, GM, inertia
       environment: EnvConfig    # water depth, density, gravity
       waves: WaveConfig         # type (regular/JONSWAP), Hs, Tp, direction
       mesh: MeshConfig          # snappy/cfMesh + overset opts
       solver: SolverConfig      # interDyMFoam vs overInterDyMFoam, time settings
   ```
   Mirrors the `hydrodynamics/capytaine/models.py` patterns.

5. **Tests (no OpenFOAM dep at CI time).**
   - `tests/hydrodynamics/openfoam_seakeeping/test_case_template.py` — case generates expected file tree from spec
   - `test_schema.py` — schema rejects malformed inputs
   - `test_rao_extractor.py` — fixture `sixDoFRigidBodyState.dat` produces heave/pitch RAO within 5% of analytic reference

6. **Follow-up issue creation.** File child issues for Phase 3 (irregular seas, slamming) and Phase 4 (BEM-vs-CFD validation against #464). Link in #470 body. This issue closes when Phases 1-2 land.

7. **Smoke check.** `uv run pytest tests/hydrodynamics/openfoam_seakeeping/ -v` green; `cat docs/domains/openfoam/seakeeping/README.md | wc -l` ≥ 150.

## Acceptance Criteria

- [ ] `docs/domains/openfoam/seakeeping/README.md` covers tutorial inventory, mesh-motion strategies, wave-library decision, and benchmark catalog
- [ ] `src/digitalmodel/hydrodynamics/openfoam_seakeeping/` skeleton lands with case_template, runner (dry-run), rao_extractor, schema
- [ ] First spec.yml (Wigley hull regular waves) drives case-template generation
- [ ] Tests pass without requiring OpenFOAM installation at CI
- [ ] Follow-up issues filed for Phases 3-4

## Open Questions

- Where does OpenFOAM live in target environments? `dev-secondary` (NVIDIA T400) is mentioned in #464 — confirm OpenFOAM 11/12 install path before the runner module assumes a binary location. Default: read `OPENFOAM_BIN` env var, fail with actionable message if absent.
- Capytaine BEM vs CFD validation (Phase 4) is contingent on hull mesh parity — the OC4 semi-sub or Wigley hull mesh must be re-usable across solvers. Coordinate with #465's hull-benchmark pick.
