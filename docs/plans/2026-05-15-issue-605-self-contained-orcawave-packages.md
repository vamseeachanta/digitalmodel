# Plan for #605: OrcaWave: produce self-contained solver packages from spec conversion

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/605
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-605-claude.md | scripts/review/results/2026-05-15-plan-605-gemini.md

---

## Scope

Package-generation scope only. This issue does not implement mesh format conversion (#606), mesh quality metrics (#608), body-level control-surface normalization (#609), or the given-mesh UX (#607). Because #500 is already `status:plan-approved` and touches the same runner/converter path-resolution surface, #605 implementation must start by reconciling with #500: either implement after #500 is merged/closed, or make the first approved implementation step an extraction of the current #500 runner helpers into the shared resolver used by both issues. Do not maintain parallel resolver/copy paths.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#605` - OPEN - `OrcaWave: produce self-contained solver packages from spec conversion`; label: `enhancement`.
- `#500` - OPEN - `OrcaWave: mesh file pre-flight validation + auto-copy in runner`; label includes `status:plan-approved`.

### Sources consulted

- `AGENTS.md` - digitalmodel declares `PYTHONPATH=src uv run python -m pytest` as the repository test command and points source ownership at `src/digitalmodel/`.
- `docs/plans/` - repo has standalone plan files but no `docs/plans/README.md` index/template; issue #596 explicitly recorded "no `docs/plans/README.md` in this issue", so these plans follow the existing standalone-file convention.
- `src/digitalmodel/hydrodynamics/diffraction/cli.py` - current Click surface includes `convert-spec`, `validate-spec`, `run-orcawave`, and `batch-orcawave`; there is no given-mesh or doctor command yet.
- `src/digitalmodel/hydrodynamics/diffraction/spec_converter.py` - `SpecConverter.convert()` delegates directly to backends and `validate()` checks non-empty mesh strings, frequencies, headings, and positive mass only.
- `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` - runner can generate OrcaWave input, copy existing mesh files, prefer OrcFxAPI, and fall back to dry-run when no API/executable is available.
- `src/digitalmodel/hydrodynamics/diffraction/mesh_pipeline.py` - existing pipeline can load/validate/prepare meshes and maps OrcaWave target format to GDF, but it is not integrated into `SpecConverter` or `OrcaWaveRunner`.
- Related issue `#500` - open plan-approved issue for mesh pre-flight validation and runner auto-copy; these future issues must not duplicate its direct scope.

### Issue-specific code findings

- `SpecConverter.convert()` currently calls `backend.generate_single()` / `generate_modular()` directly and has no packaging step.
- `OrcaWaveBackend` writes `BodyMeshFileName` as `Path(geom.mesh_file).name`, so generated YAML assumes files are colocated with the generated input.
- `OrcaWaveRunner.prepare()` sets `spec_dir = Path(spec_path).parent if spec_path else None` and passes it to `_copy_mesh_files()`, so the existing runner convention is already "relative paths resolve from the source `spec.yml` directory when `spec_path` is provided."
- `_copy_mesh_files()` already copies body mesh, damping lid, vessel-level control surface, and free-surface mesh; #605 should extract/share that existing convention for converter packaging rather than redesign it. Body-level `BodySpec.control_surface` remains #609 scope because backend/runner do not consistently emit/copy it today.

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

`diffraction convert-spec --solver orcawave` emits a self-contained OrcaWave package directory whose YAML references packaged mesh assets and fails early on missing required files.

## Proposed Tasks

1. Create a shared `OrcaWaveAssetResolver` interface, extracted from the existing runner helpers: `resolve_assets(spec, spec_path, output_dir) -> OrcaWaveAssetPackage` with source path, destination path, YAML filename, asset type, and copied/generated status.
2. Make `OrcaWaveRunner._copy_mesh_files()` and `_validate_mesh_references()` thin wrappers around the resolver or remove them after replacing their callers; the runner and converter must share exactly one path-resolution/copy implementation.
3. Preflight all OrcaWave asset references before backend YAML generation. Copy body meshes, damping lid mesh, emitted vessel-level control surface meshes, and free-surface panelled-zone meshes into the output directory. Relative paths resolve from `spec_path.parent`; absolute paths are accepted and copied by basename unless a documented safety conflict is detected.
4. Define basename collision behavior: if two distinct source assets would package to the same basename, fail before writing YAML with both source paths in the error. #605 does not rename assets; future filename rewriting belongs to #606 prepared-asset mapping.
5. Verify generated YAML continues to reference packaged basenames already emitted by `OrcaWaveBackend`; #605 should not rewrite YAML except for future prepared filenames supplied by #606.
6. Add clear `FileNotFoundError`/Click error messages for missing required files, and ensure missing OrcaWave assets fail before any OrcaWave `.yml` is written.
7. Add explicit `--solver all` behavior: preflight OrcaWave assets before writing any solver output; if OrcaWave preflight fails, do not create a partial AQWA success tree. When preflight passes, OrcaWave output is packaged, AQWA remains current behavior, and CLI/docs state that only the OrcaWave output is self-contained in this issue.

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-605-self-contained-orcawave-packages.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-605-claude.md` |
| Plan review - Gemini (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-605-gemini.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-605-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `src/digitalmodel/hydrodynamics/diffraction/spec_converter.py` | add OrcaWave packaging/preflight call |
| Create | `src/digitalmodel/hydrodynamics/diffraction/orcawave_asset_resolver.py` | shared resolver/package model extracted from runner copy/validate logic; not mesh format conversion |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` | replace existing `_copy_mesh_files()` / `_validate_mesh_references()` internals with shared resolver calls |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/cli.py` | surface clear conversion errors |
| Create/modify | `tests/hydrodynamics/diffraction/test_orcawave_packaging.py` | TDD coverage |
| Modify | `docs/domains/orcawave/README.md` | path-resolution docs |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_convert_spec_orcawave_copies_body_mesh` | body mesh is packaged from a non-output source tree | source spec under `source/`, output under isolated `out/` | output contains `.yml` and copied `body.gdf` that was absent before conversion |
| `test_convert_spec_orcawave_missing_mesh_api_fails` | module API failure contract | spec references absent mesh | `SpecConverter.convert(..., solver="orcawave")` raises `FileNotFoundError` naming the missing path |
| `test_convert_spec_orcawave_auxiliary_meshes_packaged` | emitted auxiliary assets copied | spec with damping lid, vessel-level control surface, and free-surface fields | all backend-emitted mesh references exist in output; body-level control surface remains #609 scope |
| `test_generated_yaml_references_packaged_names` | YAML references runnable local assets | packaged output | mesh filename values match output basenames |
| `test_packaged_orcawave_yaml_references_resolve_from_clean_output_dir` | package is Linux-checkable without a solver | generated YAML parsed from isolated output after source path is not used | every `*MeshFileName` value exists under output dir and no assertion depends on original source adjacency |
| `test_convert_spec_orcawave_missing_mesh_cli_exit_code` | Click wrapper failure contract | missing mesh through Click runner | exit code 1 and missing path in message |
| `test_convert_spec_solver_all_documents_orcawave_only_package` | default CLI behavior is not misleading | `--solver all` conversion | OrcaWave sub-output has packaged refs; AQWA behavior unchanged and CLI/doc message states scope |
| `test_convert_spec_solver_all_missing_orcawave_asset_is_preflight_atomic` | default path avoids partial success | `--solver all` spec with missing OrcaWave mesh | command fails before AQWA/OrcaWave output trees are written |
| `test_orcawave_asset_resolver_rejects_duplicate_basenames` | basename collision policy | two distinct meshes named `body.gdf` | preflight error lists both sources and no YAML is written |
| `test_orcawave_asset_resolver_absolute_path_copies_by_basename` | absolute mesh path policy | spec references absolute mesh path outside spec dir | output contains copied basename and YAML resolves locally |

## Acceptance Criteria

- [ ] `diffraction convert-spec analysis.yml --solver orcawave -o out/` creates a Linux-checkable self-contained OrcaWave package whose mesh references resolve from `out/`.
- [ ] Missing required mesh assets and duplicate basename collisions fail before OrcaWave YAML or partial `--solver all` outputs are written.
- [ ] Body and auxiliary mesh references in generated YAML resolve from a clean package directory without relying on original source paths.
- [ ] Tests cover the currently emitted backend mesh keys: `BodyMeshFileName`, vessel-level `BodyControlSurfaceMeshFileName`, `DampingLidMeshFileName`, and `FreeSurfacePanelledZoneMeshFileName`; body-level control-surface behavior is explicitly deferred to #609.
- [ ] Documentation states relative paths resolve from the source `spec.yml` directory, absolute mesh paths are copied by basename, and `--solver all` packages only OrcaWave output in this issue.
## Plan Review Gating

- [ ] Completed review artifacts under `/mnt/local-analysis/workspace-hub/digitalmodel/scripts/review/results/` exist for at least two providers and each non-empty artifact contains a `## Verdict` section; 0-byte in-progress files do not satisfy this gate.
- [ ] Issue is commented with this plan and moved to `status:plan-review` only after review artifacts exist.

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | PENDING | Awaiting review artifact |
| Gemini | PENDING | Awaiting review artifact |

**Overall result:** PENDING - do not label `status:plan-review` until artifacts exist and MAJOR findings, if any, are handled by a revised plan and re-review.

## Risks and Open Questions

- **Risk:** #500 is already plan-approved and runner-side `_copy_mesh_files()` / `_validate_mesh_references()` exist at HEAD; implementation must reuse or refactor that code instead of creating divergent path-resolution/copy logic.
- **Risk:** Licensed OrcaWave/OrcFxAPI behavior cannot be fully verified on Linux; tests requiring a license must skip cleanly and be proven on the licensed host where applicable.
- **Open:** Whether to include Codex as a third review provider is optional; plan-review requires at least two completed provider artifacts with verdicts.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
