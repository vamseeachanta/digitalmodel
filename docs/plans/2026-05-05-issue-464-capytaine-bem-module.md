# Plan: digitalmodel #464 — Add hydrodynamic BEM analysis module (Capytaine)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/464
**Status:** plan-review
**Tier:** T3 (large new module — partially landed)

## Context

The Capytaine module is **already substantially implemented** at `src/digitalmodel/hydrodynamics/capytaine/`:

| File | Purpose | State |
|------|---------|-------|
| `__init__.py` | Public API exports | landed |
| `models.py` | `BEMResult`, `BodyConfig`, `DOF`, `MeshConfig`, `MeshFormat`, `RAOResult`, `SolverConfig`, `WaveConditions` | landed |
| `mesh_adapter.py` | `create_floating_body` (meshio/STL/GDF) | landed |
| `solver.py` | `CapytaineSolver`, `run_bem_analysis` (246 lines) | landed |
| `rao.py` | `compute_rao`, `compute_rao_manual` | landed |
| `results.py` | `added_mass_table`, `excitation_force_table`, `export_netcdf`, `plot_added_mass_damping`, `plot_excitation_force`, `plot_rao` | landed |
| `manifest.yaml` | DNV-RP-C205 traceability | landed |

What remains under #464's umbrella: (a) fully-validated benchmark suite — that's #465 (split issue); (b) the production-grade extras in the issue body that aren't yet present.

Gap analysis vs the issue body's requirements:
- digitalmodel adapter: ✅ landed (`mesh_adapter.py`)
- Standard calculations: diffraction, radiation, RAO ✅
- Validation against benchmarks: PARTIAL (sphere/Hulme done per #465 context; OC3/OC4/semi-sub/barge → #465)
- Standards traceability via manifest: ✅
- Result export (xarray, plots, summary tables): ✅
- Hierarchical Toeplitz matrices for large meshes: NOT YET — issue notes this as "consider"
- OpenMP + GPU parallelism on dev-secondary: NOT YET configured

Practical scope for this PR: **close the loop on #464** by (1) confirming all listed deliverables present, (2) adding the missing performance hooks (Hierarchical Toeplitz), (3) adding the spec.yml→Capytaine driver, and (4) referring benchmark expansion to #465 to scope-protect.

## Plan

1. **Audit each acceptance bullet from the issue.** Run module-level inspection: confirm exports match the issue's "Standard calculations" list. Confirm `manifest.yaml` lists all DNV-RP-C205 clauses for landed functions. Output: a checklist comment posted to issue #464 with ✅/✗ per bullet — gates whether this issue can close on documentation alone or needs more code.

2. **Add Hierarchical Toeplitz hook.** `src/digitalmodel/hydrodynamics/capytaine/solver.py` — add a `use_hierarchical_toeplitz: bool = False` flag on `SolverConfig`. When `True`, the `CapytaineSolver` constructs the `HierarchicalToeplitzMatrices`-backed engine per Capytaine 2.x API. Document the speedup band (issue claims relevant for "large meshes").

3. **GPU/OpenMP env note.** New section in `src/digitalmodel/hydrodynamics/capytaine/__init__.py` docstring (or sidecar `README.md`):
   - On Linux with OpenMP: set `CAPYTAINE_USE_OPENMP=1` before run
   - dev-secondary GPU (NVIDIA T400): Capytaine 2.x doesn't expose direct CUDA — OpenMP path is the realistic option
   - Document benchmark numbers from the executor's local run (panels-per-second)

4. **spec.yml → Capytaine driver.** New file `src/digitalmodel/hydrodynamics/capytaine/runner.py`:
   - `run_from_spec(spec_path: Path) -> BEMResult` — load spec.yml, build `BodyConfig` + `WaveConditions` + `SolverConfig`, run `run_bem_analysis`
   - First reference spec at `docs/domains/capytaine/examples/sphere_diffraction/spec.yml` (Hulme analytical benchmark, already implicit in tests)
   This makes Capytaine consistent with the digitalmodel YAML-driven workflow other modules use.

5. **Citations on output.** Audit `solver.run_bem_analysis` and `rao.compute_rao` — confirm each emits a DNV-RP-C205 §7.1.2 / §7.2.1 Citation per `.claude/rules/calc-citation-contract.md`. Wire if absent.

6. **Tests.**
   - `tests/hydrodynamics/capytaine/test_runner.py` — `test_run_from_spec_sphere_matches_hulme` (Hulme benchmark via spec.yml)
   - `tests/hydrodynamics/capytaine/test_solver.py::test_hierarchical_toeplitz_matches_dense` — same problem with both engines, results within tolerance
   - `tests/hydrodynamics/capytaine/test_manifest.py` — manifest.yaml exposed clauses cover all functions in `__all__`

7. **Issue-level decision.** This issue closes when steps 1's audit shows all bullets ✅ and the new code in steps 2-5 lands. Benchmark expansion (OC3/OC4/multi-body validation) tracks in #465 and may stay open after #464 closes.

8. **Smoke check.** `uv run python -m digitalmodel.hydrodynamics.capytaine.runner docs/domains/capytaine/examples/sphere_diffraction/spec.yml` — produces BEMResult, RAO, plots in default output dir.

## Acceptance Criteria

- [ ] Per-bullet audit posted to #464 with each issue-body item resolved
- [ ] `SolverConfig.use_hierarchical_toeplitz` flag wired through to Capytaine engine
- [ ] OpenMP / parallelism documented in module docstring or README
- [ ] `runner.py` provides spec.yml → BEMResult driver, with one example spec on disk
- [ ] All public Capytaine outputs emit DNV-RP-C205 Citations per the citation contract
- [ ] All new tests pass; existing tests unaffected

## Open Questions

- Capytaine's Hierarchical Toeplitz API surface in version 2.3.1 — confirm exact import path; spike during step 2 to derisk.
- Dependency boundary: does this issue cross into `pyproject.toml` declaring `capytaine` as a runtime extra? Currently the env is at `/mnt/local-analysis/capytaine-env` (separate). Default: keep as optional extra `digitalmodel[hydrodynamics-bem]`, document install steps in README.
