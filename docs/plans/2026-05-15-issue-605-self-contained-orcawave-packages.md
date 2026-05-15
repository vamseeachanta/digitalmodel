# Plan for #605: OrcaWave: produce self-contained solver packages from spec conversion

> **Status:** plan-review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/605
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-605-claude.md | scripts/review/results/2026-05-15-plan-605-codex.md | scripts/review/results/2026-05-15-plan-605-gemini.md

---

## Scope

Package-generation scope only. This issue does not implement mesh format conversion (#606), mesh quality metrics (#608), body-level control-surface normalization (#609), or the given-mesh UX (#607). #605 copies only mesh references already emitted or already copied by the current OrcaWave backend/runner; #609 owns any new auxiliary-mesh semantics, precedence, or additional emitted references. Because #500 is labeled `status:plan-approved` but its local plan artifact still says draft and has a MINOR review, #605 implementation must begin by reading the local #500 plan/review artifacts, fetching the current #500 issue state/comments, and writing a reconciliation note that resolves stale/local discrepancies against current HEAD. #605 implementation is blocked until #500 lands `src/digitalmodel/hydrodynamics/diffraction/mesh_preflight.py` with concrete APIs equivalent to the #500 plan's mesh surface iteration, path resolution/existence validation, and basename collision detection. If #500 lands warning strings/exceptions instead of structured finding/severity objects, #605 must extend `mesh_preflight.py` or add a thin adapter in the same module during the #605 PR; structured findings are a #605 package-reporting requirement, not a hard claim about the approved #500 API. #605 must not create a second resolver/copy path independent of that #500 preflight surface.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#605` - OPEN - `OrcaWave: produce self-contained solver packages from spec conversion`; label: `enhancement`.
- `#500` - OPEN - `OrcaWave: mesh file pre-flight validation + auto-copy in runner`; label includes `status:plan-approved`.

### Sources consulted

- `AGENTS.md` - digitalmodel declares `PYTHONPATH=src uv run python -m pytest` as the repository test command and points source ownership at `src/digitalmodel/`.
- `docs/plans/` - repo has standalone plan files but no `docs/plans/README.md` index/template; issue #596 explicitly recorded "no `docs/plans/README.md` in this issue", so these plans follow the existing standalone-file convention.
- `src/digitalmodel/hydrodynamics/diffraction/cli.py` - current Click surface includes `convert-aqwa`, `convert-orcawave`, `compare`, `batch`, `convert-spec`, `validate-spec`, `run-orcawave`, `run-aqwa`, `batch-aqwa`, `batch-orcawave`, `plot-raos`, `mesh-build`, and benchmark commands; there is no given-mesh or doctor command yet.
- `src/digitalmodel/hydrodynamics/diffraction/spec_converter.py` - `SpecConverter.convert()` delegates directly to backends and `validate()` checks non-empty mesh strings, frequencies, headings, and positive mass only.
- `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` - runner can generate OrcaWave input, copy existing mesh files, prefer OrcFxAPI, and fall back to dry-run when no API/executable is available.
- `src/digitalmodel/hydrodynamics/diffraction/mesh_pipeline.py` - existing pipeline can load/validate/prepare meshes and maps OrcaWave target format to GDF, but it is not integrated into `SpecConverter` or `OrcaWaveRunner`.
- Related issue `#500` - open plan-approved issue for mesh pre-flight validation and runner auto-copy; these future issues must not duplicate its direct scope.
- `/mnt/local-analysis/workspace-hub/docs/plans/2026-04-24-issue-500-orcawave-mesh-preflight-auto-copy.md` - approved #500 plan; requires strict, fail-fast runner preflight by default, a shared `mesh_preflight.py`, and coverage for body, control surface, damping lid, and free-surface zone references.
- `/mnt/local-analysis/workspace-hub/scripts/review/results/2026-04-24-plan-500-adversarial.md` - #500 adversarial review; final verdict `MINOR`, with no open blocker that invalidates the strict-preflight direction.

### Issue-specific code findings

- `SpecConverter.convert()` currently calls `backend.generate_single()` / `generate_modular()` directly and has no packaging step.
- `OrcaWaveBackend` writes `BodyMeshFileName` as `Path(geom.mesh_file).name`, so generated YAML assumes files are colocated with the generated input.
- `OrcaWaveRunner.prepare()` sets `spec_dir = Path(spec_path).parent if spec_path else None` and passes it to `_copy_mesh_files()`, so the existing runner convention is already "relative paths resolve from the source `spec.yml` directory when `spec_path` is provided."
- `_copy_mesh_files()` already copies body mesh, damping lid, per-body `VesselSpec.control_surface`, and free-surface mesh; #605 should extract/share that existing convention for converter packaging rather than redesign it. Per-body `VesselSpec.control_surface` is in #605 because the backend emits `BodyControlSurfaceMeshFileName` from `body.vessel.control_surface` today. Per-body `BodySpec.control_surface` remains #609 scope; at HEAD the backend only reads it to set `QuadraticLoadControlSurface` and does not emit/copy its mesh filename.

### Gaps identified

- No converter-side packaging function exists for OrcaWave assets.
- `convert-spec --solver orcawave` currently produces YAML only; it does not attempt to copy assets or fail on absent referenced mesh files, so the YAML can look solver-ready while its basename mesh references do not resolve from the output directory.
- No tests prove that converter output can be run from a clean output directory, or that duplicate basenames from different source directories are rejected before OrcaWave sees ambiguous YAML.

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

`diffraction convert-spec --solver orcawave` emits a package directory where every generated OrcaWave YAML mesh reference resolves locally, and where the package manifest may also record runner-compatible auxiliary assets copied for parity with current runner behavior.

## Proposed Tasks

1. Create a shared `OrcaWaveAssetResolver` class as a package-layer wrapper over the #500 `mesh_preflight.py` helpers after the #500 reconciliation comment and after import/API gates pass on `main`: `from digitalmodel.hydrodynamics.diffraction import mesh_preflight`, plus exact landed equivalents for mesh-reference iteration, spec-relative path resolution, required-file existence validation, and basename collision detection. Expected #500 names from the local plan include `MESH_SURFACE_ITER`, `validate_mesh_exists`, and `detect_basename_collisions`; if #500 lands different names or partial behavior, revise/re-review #605 or extend `mesh_preflight.py` in the #605 PR instead of duplicating those algorithms in the resolver. If #500 lands warning strings/exceptions rather than typed findings, #605 adds the typed package finding adapter/extension in `mesh_preflight.py` and tests it there. Define `OrcaWaveAssetResolver.resolve_assets(spec, spec_path, output_dir, strict=True, stage_dir=None) -> OrcaWaveAssetPackage` with source path, destination path, YAML filename, asset type, copied/generated status, and warning/error findings. `stage_dir=None` is validate/discover-only and must not copy files; passing `stage_dir` enables staged passthrough copies used by the package publisher. Do not add a second `copy` flag, preparer hook, or second path-resolution/collision implementation in #605. If #500 has not landed when implementation begins, stop with a dependency error rather than implementing a private replacement.
2. Keep the #500 runner contract explicit: `SpecConverter`/CLI preflight must call the resolver in strict mode and fail on missing required package assets, and `OrcaWaveRunner.prepare()` must keep #500's approved strict-preflight default once #500 has landed. #605 may preserve any explicit #500 warn/advisory mode, but it must not reintroduce HEAD's silent skip or warning-soft body-mesh behavior as the default. Audit all runner callers before replacing existing helper internals.
3. Split preflight into validate-only and staged-package phases. Strict converter preflight validates all required OrcaWave asset references and collision rules before any OrcaWave YAML or final-output OrcaWave asset is written. The package phase copies assets and writes YAML into a temporary staging directory first, then publishes the completed stage through the package-directory semantics in task 4.
4. Define package-directory replacement semantics before coding. `output_dir` is treated as owned by the converter only when it is absent or contains a valid `orcawave_package_manifest.json` from a prior #605 package; an existing empty directory is also allowed but only through the same exclusive parent-directory lock. Every publish path that can create, rename, backup, or replace `output_dir` must acquire the same parent lock first, including absent-output publish, empty-dir publish, and valid-prior-manifest replacement. The plan does not claim protection against external concurrent writers beyond that lock. The lock path is `output_dir.parent / f".{output_dir.name}.orcawave-package.lock"` and is acquired with exclusive create semantics (`os.open(..., O_CREAT|O_EXCL|O_WRONLY)` or the cross-platform equivalent), writing pid/timestamp for diagnostics; if it already exists, fail with a lock-held message rather than removing it automatically. Release is a `finally` block that closes and unlinks the lock only if this process acquired it. The manifest schema is `{"schema_version":"1.0","package_root":".","created_by":"digitalmodel.orcawave_package","input_spec":<path-or-null>,"files":[{"path":<relative-posix-path>,"asset_type":<string>,"source_path":<absolute-or-null>,"sha256":<hex>,"size":<int>}],"warnings":[...]}`. `files[].path` and `package_root` must be relative package paths; `files[].source_path` may be absolute because it records provenance only. `orcawave_package_manifest.json` is reserved package metadata and is not listed in `files[]` to avoid self-referential hashing; manifest validation checks that file's schema/content separately, then applies the unlisted-file rule to every package file except that reserved manifest path. Manifest validation must reject missing/unknown `schema_version`, absolute package file paths, `..` traversal, paths outside `package_root`, duplicate file paths, missing listed files, listed files whose size/hash does not match, and any non-manifest file in the prior output directory not listed in the prior manifest. Do not allow an unlisted prior file merely because the new staged package would write the same relative path. If `output_dir` is absent, publish by atomic rename of the stage directory after acquiring the lock. If `output_dir` exists and is empty, acquire the parent lock, rename the empty directory to a temporary backup path, rename the staged package into `output_dir`, and restore the empty backup on failure; direct POSIX replacement of one existing directory with another is not assumed. If `output_dir` contains a valid prior manifest, acquire the parent lock, replace the directory by renaming the previous package to a backup, moving the stage into place, and restoring the backup on failure. If `output_dir` is non-empty without a valid manifest, has a corrupt/forged manifest, or contains unrelated/unlisted files outside the manifest, fail before writing unless a separate future force flag is approved. A failed first run leaves no final OrcaWave YAML/assets; a failed rerun leaves the previous valid package intact.
5. Copy body meshes, damping lid mesh, emitted per-body `VesselSpec.control_surface` meshes, and `free_surface_zone.mesh_file` references into the output directory using the #500/#605 resolver. The package guarantee has two parts: every mesh filename emitted in generated YAML resolves from the package directory, and the package manifest may also list runner-copied auxiliary assets such as non-QTF FSZ files that are not emitted in YAML but are preserved for runner compatibility. Relative paths resolve from `spec_path.parent`; absolute paths are accepted and copied by basename unless a basename or stale-destination conflict is detected.
6. Define collision behavior: two distinct current source assets that would package to the same basename fail before YAML is written, regardless of whether their file contents are identical, and the error lists both source paths and asset types. Re-running the same logical asset from the same source/destination pair, or with a destination that is `samefile()`/content-identical to that same source, is idempotent and not a collision only inside an output directory already proven manifest-owned by #605. In an unmanifested or unrelated destination, any existing basename is treated as a stale/unowned destination and fails instead of being adopted or overwritten, even if the bytes are identical.
7. Preserve current `BodySpec.control_surface` solver semantics without turning #605 into the #609 auxiliary-precedence issue. At HEAD, a non-null per-body `BodySpec.control_surface` affects the solver-level `QuadraticLoadControlSurface` flag but does not emit a `BodyControlSurfaceMeshFileName`; #605 must not reject that object merely because it exists. If a per-body `BodySpec.control_surface.mesh_file` is present before #609 lands, #605 records a package warning that the body-level control-surface mesh is not emitted or packaged by #605 and is deferred to #609; it still verifies every mesh filename actually emitted in YAML resolves locally. If #609 has landed first and changed body-level control surfaces into emitted mesh references, #605 must revise/re-review to consume the selected auxiliary accessor instead of silently ignoring those newly emitted assets.
8. Verify generated YAML continues to reference packaged basenames already emitted by `OrcaWaveBackend`; #605 should not rewrite YAML except for future prepared filenames supplied by #606.
9. Add clear `FileNotFoundError`/Click error messages for missing required source files, and ensure known missing OrcaWave source assets fail during validate-only preflight before any staged or final OrcaWave `.yml` is written. If the backend later emits a `*MeshFileName` key that was not represented by the resolver asset set, detect it during staged YAML verification, fail before final publish, and leave no final package; that verification is a guard against backend/resolver drift rather than the primary missing-file preflight.
10. Refactor `SpecConverter.convert_all()` into an OrcaWave-aware two-pass flow for missing-asset atomicity only: because current `convert_all()` always includes every registered backend and writes AQWA before OrcaWave, run strict OrcaWave validate-only preflight before any backend writes. If validate-only preflight fails, no AQWA or OrcaWave output is written. After preflight succeeds, AQWA may use its existing output behavior; #605 does not define AQWA manifest ownership or claim rollback of already-written AQWA files if a later OrcaWave staged copy/write/publish failure occurs. User-facing messages and acceptance must distinguish "preflight failures are all-solver atomic" from "post-preflight OrcaWave publish failures may leave already-written non-OrcaWave solver output."
11. Cover both `generate_single()` and `generate_modular()` paths. For current HEAD, `generate_modular()` writes body/control references into `04_bodies.yml` and writes damping-lid/QTF/free-surface references only into merged `master.yml`; it does not create separate numbered damping/QTF section files. #605 must package every asset referenced by the YAML files that actually exist. If implementation introduces numbered auxiliary section files later, add tests for those files in the same change; otherwise modular tests must distinguish `master.yml` auxiliary references from numbered section files.

## Pseudocode

```text
resolver.resolve_assets(spec, spec_path, output_dir, strict, stage_dir=None):
  call/extend #500 mesh_preflight helpers for path resolution, existence, and basename collision checks
  collect OrcaWave asset references from body, per-body VesselSpec.control_surface, damping lid, and any spec.free_surface_zone.mesh_file
  preserve BodySpec.control_surface solver flag semantics; before #609, warn but do not package body-level control-surface mesh_file because it is not emitted in YAML
  resolve relative paths from spec_path.parent and absolute paths as-is
  validate missing files, duplicate basenames, stale destination conflicts
  if stage_dir: copy assets into stage_dir and record staged paths
  return OrcaWaveAssetPackage(assets, warnings, errors)

convert_orcawave_package(spec, output_dir):
  package = resolve_assets(..., strict=True, stage_dir=None)
  stage_dir = create temp dir next to output_dir
  resolver.resolve_assets(..., strict=True, stage_dir=stage_dir)
  backend.generate_single_or_modular(spec, stage_dir)
  assert every emitted MeshFileName resolves under stage_dir and was represented by a resolver asset or an explicit non-packageable warning
  write orcawave_package_manifest.json in stage_dir
  publish stage using absent/empty/prior-manifest output_dir rules
```

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-605-self-contained-orcawave-packages.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-605-claude.md` |
| Plan review - Codex (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-605-codex.md` |
| Plan review - Gemini (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-605-gemini.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-605-disagreement.md` |
| #500 reconciliation note (implementation artifact required before code changes) | `docs/plans/2026-05-15-issue-605-500-reconciliation.md` |
| #500 local plan source (read-only input; reconcile because local status text may lag GitHub label) | `/mnt/local-analysis/workspace-hub/docs/plans/2026-04-24-issue-500-orcawave-mesh-preflight-auto-copy.md` |
| #500 review source (read-only input) | `/mnt/local-analysis/workspace-hub/scripts/review/results/2026-04-24-plan-500-adversarial.md` |
| #500 provider review sources named by GitHub comments (read-only inputs) | `/mnt/local-analysis/workspace-hub/scripts/review/results/2026-04-24-plan-500-claude.md`, `/mnt/local-analysis/workspace-hub/scripts/review/results/2026-04-24-plan-500-codex.md`, `/mnt/local-analysis/workspace-hub/scripts/review/results/2026-04-24-plan-500-gemini.md`, and `/mnt/local-analysis/workspace-hub/scripts/review/results/2026-04-24-plan-500-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `src/digitalmodel/hydrodynamics/diffraction/spec_converter.py` | add OrcaWave packaging/preflight call |
| Create | `src/digitalmodel/hydrodynamics/diffraction/orcawave_asset_resolver.py` | package model and #605 staging wrapper around #500 `mesh_preflight.py`; not mesh format conversion |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/mesh_preflight.py` | extend landed #500 helpers if required for destination conflict/stale-file checks or structured findings; keep path/collision algorithms single-sourced |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` | replace existing `_copy_mesh_files()` / `_validate_mesh_references()` internals with #500/#605 shared resolver calls while preserving #500 strict defaults |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py` | only if modular output must emit currently omitted damping-lid/QTF sections |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/cli.py` | surface clear conversion errors |
| Create/modify | `tests/hydrodynamics/diffraction/test_orcawave_packaging.py` | TDD coverage |
| Create | `docs/plans/2026-05-15-issue-605-500-reconciliation.md` | explicit #500 evidence/reconciliation note before implementation starts |
| Modify | `docs/domains/orcawave/README.md` | path-resolution/package docs only; stale canonical quickstart cleanup remains #614 scope and #605 must not add a contradictory quickstart |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_convert_spec_orcawave_copies_body_mesh` | body mesh is packaged from a non-output source tree | source spec under `source/`, output under isolated `out/` | output contains `.yml` and copied `body.gdf` that was absent before conversion |
| `test_convert_spec_orcawave_missing_mesh_api_fails` | module API failure contract | spec references absent mesh | `SpecConverter.convert(..., solver="orcawave")` raises `FileNotFoundError` naming the missing path |
| `test_convert_spec_orcawave_missing_mesh_api_leaves_no_output` | single-solver API preflight is atomic | absent mesh with `solver="orcawave"` and empty output dir | no `.yml`, copied mesh, manifest, or partial package remains |
| `test_convert_spec_orcawave_auxiliary_meshes_packaged` | emitted auxiliary assets copied | QTF-enabled spec with damping lid, per-body `VesselSpec.control_surface`, and free-surface fields | all backend-emitted mesh references exist in output; `BodySpec.control_surface` remains #609 scope |
| `test_convert_spec_orcawave_non_qtf_fsz_mesh_still_packaged` | runner-compatible FSZ copy behavior is preserved | non-QTF spec with `free_surface_zone.mesh_file` | packaged output includes the FSZ mesh even if no `FreeSurfacePanelledZoneMeshFileName` is emitted |
| `test_convert_spec_orcawave_body_control_surface_warns_until_609` | body-level control-surface packaging gap is not silent while preserving current solver semantics | spec with non-null `BodySpec.control_surface` and no #609 accessor | package succeeds for emitted YAML refs, `QuadraticLoadControlSurface` behavior is preserved, and manifest warning names #609 for un-emitted body-level control-surface mesh packaging |
| `test_generated_yaml_references_packaged_names` | YAML references runnable local assets | packaged output | mesh filename values match output basenames |
| `test_packaged_orcawave_yaml_references_resolve_from_clean_output_dir` | package is Linux-checkable without a solver | generated YAML parsed from isolated output after source path is not used | every `*MeshFileName` value exists under output dir and no assertion depends on original source adjacency |
| `test_convert_spec_orcawave_missing_mesh_cli_exit_code` | Click wrapper failure contract | missing mesh through Click runner | exit code 1 and message contains `Missing OrcaWave mesh asset` plus the missing path |
| `test_convert_spec_orcawave_missing_mesh_cli_leaves_no_output` | single-solver CLI preflight is atomic | `diffraction convert-spec --solver orcawave` with absent mesh | nonzero exit and output dir contains no YAML/assets/manifest |
| `test_convert_spec_solver_all_documents_orcawave_only_package` | default CLI behavior is not misleading | `--solver all` conversion | OrcaWave sub-output has packaged refs; AQWA behavior unchanged and CLI/doc message states scope |
| `test_convert_spec_solver_all_missing_orcawave_asset_is_preflight_atomic` | default path avoids partial success on known-missing assets | `--solver all` spec with missing OrcaWave mesh | command fails before AQWA/OrcaWave output trees are written |
| `test_orcawave_asset_resolver_rejects_duplicate_basenames` | basename collision policy | two distinct meshes named `body.gdf` | preflight error lists both sources and no YAML is written |
| `test_orcawave_asset_resolver_rejects_duplicate_basenames_across_asset_types` | auxiliary assets share the same collision policy | body mesh and damping/control/FSZ mesh share a basename from different sources | preflight error lists both sources and asset types |
| `test_orcawave_asset_resolver_rejects_stale_destination` | existing package file is not silently overwritten | unmanifested output already contains `body.gdf`, even with same bytes | preflight error names stale/unowned destination; no adoption or overwrite |
| `test_orcawave_asset_resolver_samefile_idempotent_manifest_owned_rerun` | re-running a manifest-owned package does not fail as a false collision | valid prior manifest owns the same source/destination file or identical packaged asset | no collision and package remains valid |
| `test_orcawave_asset_resolver_absolute_path_copies_by_basename` | absolute mesh path policy | spec references absolute mesh path outside spec dir | output contains copied basename and YAML resolves locally |
| `test_orcawave_asset_resolver_no_optional_fsz_ok` | optional free-surface zone absent is allowed | spec without free-surface panelled-zone mesh | resolver succeeds and no free-surface filename is emitted |
| `test_runner_prepare_missing_mesh_uses_500_strict_default` | resolver extraction preserves the approved #500 runner gate | `OrcaWaveRunner.prepare()` with missing mesh and default config after #500 lands | run preparation fails fast before solve/copy side effects |
| `test_runner_prepare_missing_auxiliary_meshes_use_500_strict_default` | aux validation follows #500 across all surfaces | `OrcaWaveRunner.prepare()` with missing damping lid/control/FSZ refs and default config after #500 lands | run preparation fails fast according to #500 severity rules |
| `test_runner_prepare_explicit_500_warn_mode_remains_available` | any approved #500 advisory mode is not broken | `OrcaWaveRunner.prepare()` with missing mesh and explicit non-strict/advisory config | result records #500 warning/finding semantics without silent skip |
| `test_convert_spec_orcawave_stage_copy_failure_rolls_back_first_run` | atomic package behavior covers first-run copy/write failures | simulated copy failure after validation into absent output dir | no final YAML/assets remain in output dir |
| `test_convert_spec_orcawave_existing_empty_output_requires_lock` | empty existing output dir has defined non-concurrent semantics | pre-created empty `out/` | package publish proceeds only after parent lock acquisition and empty-dir backup/rename; simulated publish failure restores the empty dir and leaves no partial files |
| `test_orcawave_package_lock_existing_lock_fails_without_removal` | lock semantics are deterministic and non-destructive | pre-existing `.out.orcawave-package.lock` next to `out/` | package publish fails naming the lock and leaves the lock/output untouched |
| `test_orcawave_package_lock_acquired_for_prior_manifest_replacement` | concurrent reruns use the same lock as first publish | valid prior manifest-owned `out/` plus new staged package | replacement attempts acquire the parent lock before backup/rename and fail without touching `out/` if the lock exists |
| `test_orcawave_package_lock_removed_after_success_and_failure` | acquired locks cannot brick later writes | successful publish and simulated failure after lock acquisition | lock file is closed/unlinked in both paths only when this process acquired it |
| `test_convert_spec_orcawave_stage_copy_failure_preserves_previous_package` | failed rerun does not destroy a valid package | existing manifest-owned package plus simulated copy failure | previous package is restored and still self-contained |
| `test_convert_spec_orcawave_nonmanifest_output_refuses_merge` | unrelated destination files are not deleted by package replace | non-empty output dir without manifest | preflight fails before stage publish and leaves unrelated files untouched |
| `test_orcawave_package_manifest_success_lists_owned_files` | success manifest is complete, not just rejectable | successful package with generated YAML, body mesh, auxiliary mesh, and warning | manifest lists each generated YAML/copied asset with relative path, asset_type, source_path, sha256, size, and warnings; reserved manifest path is not self-listed |
| `test_orcawave_package_manifest_rejects_corrupt_or_forged_ownership` | destructive replacement is gated by a trustworthy manifest | prior output with malformed schema, absolute path, `..`, stale hash, missing listed file, duplicate path, unlisted non-manifest file, or manifest path incorrectly listed in `files[]` | package publish refuses replacement and leaves directory untouched |
| `test_convert_spec_orcawave_modular_packages_assets` | modular output path is self-contained too | OrcaWave conversion using `format="modular"` | `04_bodies.yml` body/control refs and `master.yml` auxiliary refs resolve from output dir; no assertion expects numbered damping/QTF section files unless implementation adds them |

## Acceptance Criteria

- [ ] `diffraction convert-spec analysis.yml --solver orcawave -o out/` creates a Linux-checkable self-contained OrcaWave package whose mesh references resolve from `out/`.
- [ ] Missing required source mesh assets fail during OrcaWave validate-only preflight before OrcaWave YAML or partial `--solver all` outputs are written; staged YAML drift checks can still fail before final publish if the backend emits an uncollected mesh key.
- [ ] Duplicate source basenames and stale destination basenames fail before YAML/assets are written; same-source/content idempotence is allowed only for valid manifest-owned reruns.
- [ ] Package publish semantics are manifest-owned and lock-protected for absent-output publish, existing-empty-dir publish, and valid-prior-manifest replacement: unrelated output files are not deleted, a failed first run leaves no package, a failed pre-existing-empty-dir publish restores the empty directory, a failed rerun preserves the previous valid package, and acquired locks are removed on both success and failure.
- [ ] Manifest-owned replacement validates schema version, relative paths, package root, file hashes/sizes, duplicate paths, stale listed files, path traversal, and unlisted non-manifest files before any destructive replacement; the manifest file itself is reserved metadata and not self-listed in `files[]`, and successful packages have tests proving generated YAML/assets/hashes/sizes/warnings are listed.
- [ ] Body and auxiliary mesh references in generated YAML resolve from a clean package directory without relying on original source paths.
- [ ] Tests cover the currently emitted backend mesh keys: `BodyMeshFileName`, per-body `VesselSpec.control_surface` emitted as `BodyControlSurfaceMeshFileName`, `DampingLidMeshFileName`, and QTF-mode `FreeSurfacePanelledZoneMeshFileName`; tests also prove the resolver copies `free_surface_zone.mesh_file` when present outside QTF to preserve runner behavior; per-body `BodySpec.control_surface` preserves existing solver-flag semantics and warns/defer-packages un-emitted mesh references to #609.
- [ ] #605 does not change auxiliary precedence/semantics owned by #609; it only packages auxiliary references already emitted by the backend and warns when a schema-valid auxiliary object is semantically present but not emitted as a packageable mesh reference at HEAD.
- [ ] `--solver all` messaging and tests distinguish preflight-missing-asset atomicity from post-preflight OrcaWave publish failures, which may leave AQWA output because #605 does not implement cross-solver rollback.
- [ ] The #500 reconciliation note exists, is linked from #500/#605 comments, records that the local #500 plan/review artifacts and current GitHub issue state/comments were read, resolves the local draft/MINOR artifact status against the `status:plan-approved` issue label, and states exactly how #605 wraps or extends #500 `mesh_preflight.py` rather than duplicating it.
- [ ] Pre-implementation dependency gate fails if #500 has not landed `mesh_preflight.py` and the required mesh-reference iteration, path resolution/existence validation, basename collision, and structured finding/severity APIs on `main`.
- [ ] If #609 has landed first, #605 consumes the selected auxiliary accessor or is revised/re-reviewed before implementation; it does not reject body-level control surfaces that #609 has made valid.
- [ ] Documentation states relative paths resolve from the source `spec.yml` directory, absolute mesh paths are copied by basename, duplicate distinct basenames fail, same-source reruns are idempotent, and `--solver all` packages only OrcaWave output in this issue.
## Plan Review Gating

- [ ] Completed review artifacts under `/mnt/local-analysis/workspace-hub/digitalmodel/scripts/review/results/` exist for at least two providers and each non-empty artifact contains a `## Verdict` section; 0-byte in-progress files do not satisfy this gate.
- [ ] Any provider `MAJOR` finding requires a plan revision and re-review; the issue is commented with this plan and moved to `status:plan-review` only after no unresolved `MAJOR` findings remain.

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | UNAVAILABLE | Review command failed; completed artifact retained |
| Codex | UNAVAILABLE | Codex CLI quota/usage unavailable on final rerun after plan patch |
| Gemini | MINOR | Notes #500 dependency, conservative stale-destination behavior, lock-location risk, and return-type documentation risk; no unresolved MAJOR |

**Overall result:** READY FOR USER REVIEW - completed artifacts exist, prior MAJOR findings were addressed by plan revisions, and no fresh unresolved MAJOR remains. Implementation remains gated by upstream dependencies named in the plan.

## Risks and Open Questions

- **Risk:** #500 is plan-approved but may be partly stale against HEAD; #605 implementation must create the explicit reconciliation note and resolve discrepancies before code changes rather than proceeding from memory or current HEAD alone.
- **Risk:** The converter needs fail-loud package preflight and #500 approves a strict runner default, while current HEAD is warning-soft/silent in places. The shared resolver must expose findings/severity so #605 preserves #500's approved strict default and any explicit #500 advisory mode without creating a third policy.
- **Risk:** `FreeSurfacePanelledZoneMeshFileName` is emitted only for diagonal/full QTF solve types, current runner copy behavior collects `free_surface_zone.mesh_file` whenever present, and current modular output puts damping/QTF auxiliary references in `master.yml` rather than numbered section files. Tests must separate "asset copied for runner/package completeness" from "YAML key emitted for QTF" and must assert the actual modular YAML artifact that contains the reference.
- **Risk:** Licensed OrcaWave/OrcFxAPI behavior cannot be fully verified on Linux; tests requiring a license must skip cleanly and be proven on the licensed host where applicable.
- **Open:** Default review policy uses three-provider review; Claude, Codex, and Gemini are the selected review set for this plan.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
