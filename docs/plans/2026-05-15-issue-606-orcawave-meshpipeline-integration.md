# Plan for #606: OrcaWave: integrate MeshPipeline into spec conversion and runner

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/606
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-606-claude.md | scripts/review/results/2026-05-15-plan-606-codex.md

---

## Scope

Mesh format preparation only. This issue should build on the #605 package layout and #608 quality gates, but it does not define the given-mesh CLI (#607). It depends on the shared resolver from #605, expected at `src/digitalmodel/hydrodynamics/diffraction/orcawave_asset_resolver.py` after #500/#605 reconciliation, so generated meshes are not later overwritten or skipped by the legacy runner copy path.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#606` - OPEN - `OrcaWave: integrate MeshPipeline into spec conversion and runner`; label: `enhancement`.
- `#500` - OPEN - `OrcaWave: mesh file pre-flight validation + auto-copy in runner`; label includes `status:plan-approved`.

### Sources consulted

- `AGENTS.md` - digitalmodel declares `PYTHONPATH=src uv run python -m pytest` as the repository test command and points source ownership at `src/digitalmodel/`.
- `docs/plans/` - repo has standalone plan files but no `docs/plans/README.md` index/template; issue #596 explicitly recorded "no `docs/plans/README.md` in this issue", so these plans follow the existing standalone-file convention.
- `src/digitalmodel/hydrodynamics/diffraction/cli.py` - current Click surface includes `convert-aqwa`, `convert-orcawave`, `compare`, `batch`, `convert-spec`, `validate-spec`, `run-orcawave`, `run-aqwa`, `batch-aqwa`, `batch-orcawave`, `plot-raos`, `mesh-build`, and benchmark commands. #606 does not add CLI commands; it changes the conversion/run internals consumed by existing commands.
- `src/digitalmodel/hydrodynamics/diffraction/spec_converter.py` - `SpecConverter.convert()` delegates directly to backends and `validate()` checks non-empty mesh strings, frequencies, headings, and positive mass only.
- `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` - runner can generate OrcaWave input, copy existing mesh files, prefer OrcFxAPI, and fall back to dry-run when no API/executable is available.
- `src/digitalmodel/hydrodynamics/diffraction/mesh_pipeline.py` - existing pipeline can load/validate/prepare meshes and maps OrcaWave target format to GDF, but it is not integrated into `SpecConverter` or `OrcaWaveRunner`.
- Related issue `#500` - open plan-approved issue for mesh pre-flight validation and runner auto-copy; these future issues must not duplicate its direct scope.

### Issue-specific code findings

- `mesh_pipeline.py` maps OrcaWave target format to GDF and supports `.gdf`, `.dat`, and `.stl` through BEMRosetta handlers.
- `SpecConverter` and `OrcaWaveRunner` do not instantiate `MeshPipeline` today.
- `OrcaWaveBackend` writes mesh filenames directly from the spec geometry path.
- Existing tests in `tests/hydrodynamics/diffraction/test_mesh_pipeline.py` cover `prepare_for_solver("orcawave")` behavior independently.

### Gaps identified

- No integration chooses passthrough versus conversion during OrcaWave package/run preparation, and no taxonomy currently distinguishes body/control/damping-lid/free-surface asset classes.
- No metadata records original mesh path, converted path, source format, target format, or conversion warnings.
- Unsupported source format errors are not centralized for the OrcaWave workflow.

### Evidence

Commands and inspections used while drafting:

```text
sed -n '1,220p' AGENTS.md
ls -la docs/plans
rg -n "Plan Index|Adversarial Review Summary|Review artifacts" docs/plans -S
sed -n '400,590p' src/digitalmodel/hydrodynamics/diffraction/cli.py
sed -n '1,180p' src/digitalmodel/hydrodynamics/diffraction/spec_converter.py
sed -n '220,560p' src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py
sed -n '1,280p' src/digitalmodel/hydrodynamics/diffraction/mesh_pipeline.py
```

Reproduction proofs: N/A - this is a future-work enhancement issue, not an alleged runtime regression or failing-test report. Implementation must still start with the TDD tests below.

## Deliverable

The OrcaWave conversion/run path uses `MeshPipeline` to prepare supported source meshes into solver-ready assets while preserving passthrough for already-ready meshes.

## Proposed Tasks

1. Define the OrcaWave asset taxonomy before code: body mesh, body control surface mesh, damping lid mesh, and free-surface panelled-zone mesh. Body/control/damping-lid use GDF passthrough or supported conversion to GDF; free-surface panelled-zone `.fdf` is valid passthrough and must not be rejected by body-mesh conversion rules.
2. Add `OrcaWaveMeshPreparer` as a layer on top of the #605 `OrcaWaveAssetResolver`. The preparer first identifies the asset type from the schema path, then bypasses `MeshPipeline` for valid free-surface panelled-zone `.fdf` assets, short-circuits ready `.gdf` assets, and calls `MeshPipeline.prepare_for_solver()` only for supported body/control/damping-lid conversion classes.
3. Return an `OrcaWavePreparedAsset` manifest with at least: `asset_id`, `asset_type`, `source_path`, `output_path`, `yaml_filename`, `source_format`, `target_format`, `was_passthrough`, `was_converted`, and conversion warnings.
4. Hoist path resolution before backend YAML generation, then prepare meshes and generate YAML from an explicit prepared-filename map keyed by asset id/type; do not mutate or clone `self.spec` because `convert_all()` reuses `self.spec` across AQWA and OrcaWave.
5. Update all backend mesh filename write sites through the non-mutating prepared-filename map: `BodyMeshFileName`, `BodyControlSurfaceMeshFileName`, `DampingLidMeshFileName`, and `FreeSurfacePanelledZoneMeshFileName`.
6. Refactor runner copy/validation so generated prepared assets are not resolved back against `spec_dir` and cannot overwrite converted output with an older source file of the same basename.
7. Preserve units/symmetry/source provenance in run metadata and add tests for passthrough, conversion, unsupported format, damping lid, FSZ `.fdf`, runner overwrite protection, and generated YAML filename updates.

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-606-orcawave-meshpipeline-integration.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-606-claude.md` |
| Plan review - Codex (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-606-codex.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-606-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/hydrodynamics/diffraction/orcawave_mesh_preparer.py` | asset taxonomy and `MeshPipeline` integration layer |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/spec_converter.py` | invoke preparation during OrcaWave conversion |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` | consume prepared-asset manifest and remove/wrap legacy copy path to avoid overwrites |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py` | accept prepared mesh filenames via explicit map, without mutating shared converter state |
| Modify/create | `tests/hydrodynamics/diffraction/test_orcawave_mesh_preparer.py` | TDD coverage |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_orcawave_mesh_prepare_gdf_passthrough_manifest` | no churn for ready mesh | `.gdf` body/control/damping-lid mesh | manifest records `was_passthrough=True`, `was_converted=False`, and YAML basename without relying on an internal `MeshPipeline.write()` spy |
| `test_orcawave_mesh_prepare_dat_to_gdf` | supported conversion path | `.dat` mesh using existing BEMRosetta DAT coverage or a minimal fixture under `tests/hydrodynamics/diffraction/fixtures/meshes/` | `.gdf` output referenced by YAML |
| `test_orcawave_mesh_prepare_unsupported_format_fails` | clear error surface | `.obj` body mesh | `OrcaWaveMeshPreparationError` message names asset type, extension, and supported formats |
| `test_orcawave_free_surface_fdf_passthrough` | valid FDF auxiliary mesh not rejected | QTF spec with free-surface `.fdf` mesh | packaged passthrough and `FreeSurfacePanelledZoneMeshFileName` emitted |
| `test_orcawave_free_surface_non_fdf_fails_or_delegates_by_policy` | FSZ policy is explicit | QTF spec with non-`.fdf` free-surface mesh | either clear unsupported-FSZ error or documented conversion path; no accidental body-mesh conversion rules |
| `test_orcawave_mesh_prepare_convert_all_no_cross_solver_leak` | OrcaWave prep does not mutate shared spec | `SpecConverter.convert_all()` with AQWA+OrcaWave | AQWA keeps its expected mesh target and OrcaWave uses prepared file |
| `test_orcawave_damping_lid_mesh_policy` | damping lid participates in taxonomy | spec with damping lid mesh | converted/passthrough per policy and YAML references prepared asset |
| `test_orcawave_prepared_asset_not_overwritten_by_source` | runner does not recopy source over converted output | converted mesh shares basename with source | prepared output remains intact and `RunResult.mesh_files` records it |
| `test_orcawave_mesh_metadata_records_source_and_target` | provenance available | converted mesh | metadata includes source/target paths and formats |

## Acceptance Criteria

- [ ] Supported non-GDF meshes can be converted into OrcaWave-ready package assets.
- [ ] Already-ready GDF input is not rewritten unnecessarily.
- [ ] Generated YAML points at prepared asset filenames for all mesh write sites, not only body meshes.
- [ ] Unsupported body/control/damping-lid formats fail with a domain-specific clear supported-format list, while `.fdf` free-surface meshes pass through when valid for OrcaWave.
- [ ] Tests cover passthrough, conversion, unsupported format, damping lid behavior, `.fdf` free-surface passthrough with QTF solve type, runner overwrite protection, and metadata.
- [ ] Completed review artifacts under `/mnt/local-analysis/workspace-hub/digitalmodel/scripts/review/results/` exist for at least two providers and each non-empty artifact contains a `## Verdict` section; 0-byte in-progress files do not satisfy this gate.
- [ ] Any provider `MAJOR` finding requires a plan revision and re-review; the issue is commented with this plan and moved to `status:plan-review` only after no unresolved `MAJOR` findings remain.
- [ ] The implementation plan has one shared path-resolution/copy path through `orcawave_asset_resolver.py` from #605/#500; no parallel resolver remains in both runner and preparer.

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | PENDING | Awaiting review artifact |
| Codex | PENDING | Awaiting review artifact |

**Overall result:** PENDING - do not label `status:plan-review` until artifacts exist and no unresolved `MAJOR` findings remain.

## Risks and Open Questions

- **Risk:** #500/#605 must establish the shared resolver first. #606 must extend that resolver/preparer chain instead of introducing a competing path-resolution/copy implementation.
- **Risk:** Existing command surfaces are broad; #606 changes internals only and must not add or rename CLI commands unless covered by a separate issue.
- **Risk:** Licensed OrcaWave/OrcFxAPI behavior cannot be fully verified on Linux; tests requiring a license must skip cleanly and be proven on the licensed host where applicable.
- **Open:** Gemini was unavailable in this environment; use Claude + Codex as the required two-provider review set for plan-review.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
