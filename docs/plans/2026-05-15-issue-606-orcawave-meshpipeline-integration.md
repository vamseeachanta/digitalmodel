# Plan for #606: OrcaWave: integrate MeshPipeline into spec conversion and runner

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/606
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-606-claude.md | scripts/review/results/2026-05-15-plan-606-codex.md | scripts/review/results/2026-05-15-plan-606-gemini.md

---

## Scope

Mesh format preparation only. This issue depends on the #605 package layout and shared resolver, but it does not define the given-mesh CLI (#607) or mesh quality policy (#608). Implementation is blocked until the #605 PR is merged, `src/digitalmodel/hydrodynamics/diffraction/orcawave_asset_resolver.py` exists at HEAD, and the overlapping #500 runner-copy scope has been reconciled. #606 extends the landed #605 resolver/package module with a format-preparation and owned-generated-file registration surface; it must not assume #605 already shipped a preparer hook, create a fallback resolver, or duplicate runner copy logic. The plan intentionally covers every backend mesh filename write site, not only `BodyMeshFileName`, because preparing body meshes alone would leave auxiliary OrcaWave inputs inconsistent. If #609 lands first, #606 must consume #609's auxiliary accessor for selected `BodySpec.control_surface`/`VesselSpec.control_surface` assets instead of rejecting body-level control surfaces.

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
- `MeshFormatType` and `OrcaWaveBackend` contain mappings beyond GDF, but repository OrcaWave examples source-back only `Wamit gdf` and `Aqwa dat` for body meshes at drafting time. #606 must not bless `OBJ`, `Gmsh`, or `STL` as native passthrough unless an OrcaWave-exported/sample reference proves the exact YAML value and asset type.
- `_parse_fdf_panels` exists in `src/digitalmodel/hydrodynamics/diffraction/benchmark_helpers.py`, but it skips header lines and cannot validate declared panel counts. #606 needs a stronger minimal `.fdf` validator for free-surface assets rather than relying on that helper alone.

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

1. Define the OrcaWave asset taxonomy before code: body mesh, selected control-surface mesh, damping lid mesh, and free-surface panelled-zone mesh. Before resolving assets, scan for per-body `BodySpec.control_surface` so #606 cannot miss it merely because the #605 resolver only packages emitted assets. Until #609 lands, preserve current `BodySpec.control_surface` solver-flag semantics and emit a defer-to-#609 warning for any body-level control-surface mesh file rather than treating the object as a hard error; after #609 lands, use its auxiliary accessor so the selected body-level or vessel-level control-surface asset is prepared consistently.
2. Extend the landed #605 `OrcaWaveAssetResolver`/package module with a tested preparation ownership surface in this issue, then add `OrcaWaveMeshPreparer` behind that #606-owned extension rather than adding an independent copy/publish path. #605 is expected to provide validate/discover/staged-copy behavior only; #606 adds any API needed to register generated converted assets and the mesh-preparation manifest into #605's package manifest writer. The preparer first identifies the asset type from the schema path, then uses an explicit source-backed OrcaWave-native extension table for default passthrough (`.gdf -> Wamit gdf`, `.dat -> Aqwa dat` for body/damping assets, `.csf` only where #609-selected control-surface semantics apply, and `.fdf` only for free-surface panelled zones), bypasses `MeshPipeline` for native passthrough assets before any `MeshPipeline.load()` call, and calls `MeshPipeline.prepare_for_solver()` for MeshPipeline-supported non-native sources such as `.stl`. `.obj` and `.msh` are fail-closed until MeshPipeline handlers and source-backed OrcaWave mappings are added in this or a later reviewed issue; #606 must not default-pass them through merely because the enum/backend map contains names.
3. Define format policy explicitly: source-backed OrcaWave-native `.gdf` and `.dat` body/damping assets are solver-ready passthroughs with matching YAML mesh format (`Wamit gdf` and `Aqwa dat`), even when the source spec used `mesh_format: auto`. MeshPipeline-supported `.stl` body/damping assets are converted to GDF by default unless a later source-backed native-STL OrcaWave reference is added and the plan is re-reviewed. `.obj` and `.msh` fail with `OrcaWaveMeshPreparationError` unless implementation also adds tested MeshPipeline handlers and a reviewed conversion/passthrough policy. Control-surface `.csf` is native for selected control-surface assets only. Free-surface panelled-zone assets are conservative: `.fdf` is structurally validated and passed through; non-FDF FSZ formats are allowed only when the current backend mapping and an OrcaWave-exported/sample reference prove the exact YAML value for that FSZ asset type. Unknown formats such as `.iges`, and unsourced FSZ `.csf`, fail with `OrcaWaveMeshPreparationError` unless a later conversion handler/reference is explicitly added. `MeshPipeline.prepare_for_solver()` is still extended with explicit output naming because conversions need collision-safe writes.
4. Define a non-mutating backend override contract before touching backend YAML generation. `OrcaWavePreparedAsset` must carry `asset_id: str`, `yaml_filename: str`, `yaml_mesh_format: str | None`, `yaml_length_units: str | None`, and `yaml_symmetry: str | None` where applicable. `yaml_mesh_format` is the exact source-backed OrcaWave YAML value (`Wamit gdf`, `Aqwa dat`, converted `Wamit gdf`, `Wamit csf`, `Wamit fdf`, etc.); `source_format` and `target_format` retain schema-like normalized values such as `gdf`, `dat`, `stl`, `fdf`, or `csf`. `OrcaWaveBackend` must accept an explicit prepared-metadata map keyed by deterministic `asset_id` and use that map for filename/format/units/symmetry fields instead of mutating `self.spec` or relying on the original `mesh_format: auto` values. If current backend code still maps `dat -> Wamit dat`, #606 must update or override that path to `Aqwa dat` with tests because repository examples use `Aqwa dat`.
5. Keep `ControlSurfaceSpec` schema changes guarded. Prefer representing control-surface native passthrough through the prepared-metadata map and existing geometry units. If implementation must add optional `mesh_format` or `length_units` fields to the shared `ControlSurfaceSpec`, the preparer/backend may consume those fields only for the selected control-surface source. Until #609 lands, body-level control-surface mesh preparation remains deferred with a warning while current solver-flag behavior is preserved; after #609 lands, #606 consumes the #609 selected accessor result.
6. Validate `.fdf` free-surface files structurally before passthrough with a purpose-built minimal validator for the WAMIT free-surface panel file syntax used by OrcaWave fixtures. The supported form for #606 fixtures is: after stripping blank/comment/title lines, the first parseable declaration line contains the integer panel count as its first token, followed by exactly that many numeric panel rows with the expected free-surface row shape. Unknown header forms fail closed with "unsupported FDF header" rather than being accepted. `_parse_fdf_panels` may be reused only as a row parser inside that stricter validator; empty, malformed, or header-count-mismatched `.fdf` files fail before solve time. Non-FDF free-surface assets fail closed unless #606 can cite and test an exact backend-supported OrcaWave YAML mapping for that FSZ asset type; do not infer `.csf` control-surface semantics for free-surface zones.
7. Return and persist an `OrcaWaveMeshPreparationManifest` at `orcawave_mesh_manifest.json` with `prepared_assets`. Each `OrcaWavePreparedAsset` contains `asset_id`, `asset_type`, `source_path`, `output_path`, `yaml_filename`, `yaml_mesh_format`, `yaml_length_units`, `yaml_symmetry`, `source_format`, `target_format`, `was_passthrough`, `was_converted`, and conversion warnings. The mesh manifest is a child artifact of the #605 package: it must be written inside the #605 staging directory and listed in `orcawave_package_manifest.json` alongside prepared mesh files through the #606-owned extension to #605's manifest writer so package replacement/cleanup treats converted assets as owned files.
8. Use collision-safe prepared filenames for converted assets: include `asset_type` and a stable `asset_id` prefix (for example `body_0__hull.gdf`, `body_0_control_surface__hull.gdf`) so same-stem body/control/damping assets cannot overwrite each other. `asset_id` is deterministic and index-based, not name-based: `body_{index}`, `body_{index}_control_surface`, `damping_lid`, and `free_surface_zone`. #606 must first extend `MeshPipeline.prepare_for_solver()` with an explicit `output_path`/`output_name` parameter and backward-compatibility tests for callers that omit it, then call that API only for converted assets; using the current fixed `stem + target_ext` behavior is not allowed. Argument precedence is explicit: callers may pass either `output_path` or `output_dir`/`output_name`; passing both `output_path` and `output_name` or both `output_path` and an incompatible `output_dir` fails with `ValueError`, and existing callers using only `output_dir` keep current behavior. Passthrough basename collision policy remains owned by #605.
9. Hoist path resolution before backend YAML generation, then prepare meshes and generate YAML from the explicit prepared-metadata map keyed by `asset_id`: `{"body_0": PreparedAsset(...), "body_0_control_surface": PreparedAsset(...), "damping_lid": PreparedAsset(...), "free_surface_zone": PreparedAsset(...)}`. Pass this map as an explicit backend argument or context object; do not mutate or clone `self.spec` because `convert_all()` reuses `self.spec` across AQWA and OrcaWave.
10. Update all backend mesh filename write sites through the non-mutating prepared-metadata map, including modular output behavior from #605: `BodyMeshFileName`, per-body `VesselSpec.control_surface` emitted as `BodyControlSurfaceMeshFileName`, `DampingLidMeshFileName`, and QTF-mode `FreeSurfacePanelledZoneMeshFileName`. For current HEAD modular output, tests must cover `04_bodies.yml` for body/control references and merged `master.yml` for damping-lid/QTF free-surface references; if #605 introduced numbered auxiliary section files, cover those exact files too.
11. Refactor runner copy/validation only through the landed #605 resolver plus the #606 preparation extension so generated prepared assets are not resolved back against `spec_dir` and cannot overwrite converted output with an older source file of the same basename. #606 owns the runner manifest handoff fields unless a landed #611/#605 contract already provides equivalent names: add or consume `RunConfig(package_manifest_path: Path | None = None, mesh_manifest_path: Path | None = None)` with tests, and revise/re-review if #611 lands a different artifact field contract first. When no manifest exists, direct `run-orcawave` must create an in-process #605 staged package, run `OrcaWaveMeshPreparer` through the #606 extension before backend YAML generation, then pass the prepared metadata map to the backend in the same run; it must not generate YAML first and try to copy/convert after. When a valid prepared manifest is present, it overrides legacy `copy_mesh_files=True` for those prepared assets, records manifest output paths in `RunResult.mesh_files`, and fails if package and mesh manifests disagree about owned prepared files. All pass-through copies and converted outputs must be produced or adopted by the #605 resolver's staged package and #606 owned-generated-file registration; no code in #606 may copy mesh files directly into the final package directory.
12. Preserve units/symmetry/source provenance in `orcawave_mesh_manifest.json`. `RunResult.mesh_files` remains a `list[Path]` compatibility surface that records prepared output paths only; #611 may later promote manifest fields into the general run manifest, but #606 must not depend on #608 or #611. #606 records unit mismatches but does not normalize units across body/damping/FSZ assets; unit quality policy is handed to #608.

## Pseudocode

```text
prepare_orcawave_meshes(spec, spec_path, output_dir):
  require OrcaWaveAssetResolver from #605
  extend/import #605 package manifest writer with #606 owned-generated-file registration
  package_stage_dir = #605-provided staging directory for the package publish
  assets = resolver.resolve_assets(..., strict=True, stage_dir=None)
  prepared_metadata_map = {}
  for asset in assets:
    if BodySpec.control_surface and #609 accessor unavailable: preserve solver-flag semantics, record defer-to-#609 warning for mesh preparation, and do not silently miss the field
    if #609 accessor available: use selected control-surface asset from accessor
    output_name = collision_safe_name(asset.asset_type, asset.asset_id, asset.source_path)
    if asset is FSZ and asset.type == "auto": record no mesh asset and continue
    elif asset is FSZ .fdf: validate_fdf_header_and_panels_then_passthrough
    elif asset is FSZ and ext has source-backed backend FSZ mapping: passthrough with native mesh format override
    elif asset ext in source_backed_native_orcawave_extensions_for_asset(asset): passthrough with native mesh format override
    elif asset ext in explicitly_supported_conversion_extensions: convert via MeshPipeline.prepare_for_solver(output_path=package_stage_dir/output_name)
    else: raise OrcaWaveMeshPreparationError
    prepared_metadata_map[asset.asset_id] = PreparedAsset(...)
  resolver.resolve_assets(..., strict=True, stage_dir=package_stage_dir)
  register converted outputs and mesh manifest with the #606 extension to #605 package ownership
  # The #605 resolver performs staged passthrough copies; #606 registers converted/generated files through the package writer extension.
  write orcawave_mesh_manifest.json in package_stage_dir
  add mesh manifest and prepared outputs to #605 orcawave_package_manifest.json
  return prepared_metadata_map keyed by asset_id
```

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-606-orcawave-meshpipeline-integration.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-606-claude.md` |
| Plan review - Codex (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-606-codex.md` |
| Plan review - Gemini (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-606-gemini.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-606-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/hydrodynamics/diffraction/orcawave_mesh_preparer.py` | asset taxonomy and `MeshPipeline` integration layer |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_asset_resolver.py` | extend landed #605 resolver/package module with preparation and owned-generated-file registration APIs; #606 must not assume #605 already provides this seam |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/mesh_pipeline.py` | add explicit converted `output_path`/`output_name` support to `prepare_for_solver()` |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` | optional only if implementation chooses schema metadata; guarded so new shared fields are ignored for deferred `BodySpec.control_surface` |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/spec_converter.py` | invoke preparation during OrcaWave conversion |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` | consume prepared-asset manifest and remove/wrap legacy copy path to avoid overwrites |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py` | accept prepared mesh filenames via explicit map, without mutating shared converter state |
| Modify/create | `tests/hydrodynamics/diffraction/test_orcawave_mesh_preparer.py` | TDD coverage |
| Create | `tests/hydrodynamics/diffraction/fixtures/meshes/` | minimal `.dat`, `.gdf`, `.obj`, `.msh`, `.fdf`, and unsupported-format fixtures for deterministic tests |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_orcawave_mesh_prepare_gdf_passthrough_manifest` | no churn for ready mesh | `.gdf` body/control/damping-lid mesh | manifest records `was_passthrough=True`, `was_converted=False`, and YAML basename without invoking `MeshPipeline.prepare_for_solver()` or handler writes |
| `test_orcawave_mesh_prepare_dat_passthrough_uses_aqwa_dat` | DAT passthrough follows repository OrcaWave examples, not the stale backend map | `.dat` body/damping mesh | manifest records passthrough and backend metadata emits `Aqwa dat` |
| `test_orcawave_mesh_prepare_stl_converts_to_gdf_by_default` | unsupported native-STL claims are avoided | `.stl` body mesh supported by MeshPipeline | converted GDF output is registered through #605 ownership and YAML emits `Wamit gdf` |
| `test_mesh_pipeline_prepare_for_solver_default_output_contract_unchanged` | existing MeshPipeline callers remain compatible | caller invokes `prepare_for_solver("orcawave")` without `output_path`/`output_name` | output path and filename follow the pre-#606 default stem/extension behavior |
| `test_mesh_pipeline_prepare_for_solver_accepts_explicit_output_path` | new writer contract is concrete before OrcaWave integration | caller passes `output_path=tmp_path / "body_0__hull.gdf"` | prepared mesh is written exactly at that path and return metadata points at it |
| `test_mesh_pipeline_prepare_for_solver_accepts_explicit_output_name` | output-name API is deterministic when only the package dir is supplied | caller passes `output_dir=stage`, `output_name="body_0__hull.gdf"` | prepared mesh is written under `stage/body_0__hull.gdf` with no fixed-stem fallback |
| `test_mesh_pipeline_prepare_for_solver_rejects_conflicting_output_arguments` | output writer precedence cannot be guessed | caller passes `output_path` plus conflicting `output_dir` or `output_name` | `ValueError` names the incompatible arguments |
| `test_orcawave_mesh_prepare_obj_msh_fail_closed_without_handlers_or_sources` | enum/backend names are not treated as proof | `.obj` and `.msh` body meshes with spec `mesh_format: auto` and no MeshPipeline handler/source-backed policy | `OrcaWaveMeshPreparationError` names missing handler/source-backed policy; no native passthrough |
| `test_orcawave_control_surface_native_format_uses_guarded_metadata` | control-surface native formats are representable without crossing #609 scope | `.obj` `VesselSpec.control_surface` | prepared metadata emits correct filename/format/units, or clear deferral/error before YAML if schema support is not chosen |
| `test_orcawave_mesh_prepare_force_gdf_uses_real_mesh_pipeline_conversion` | explicit force-convert path is real | MeshPipeline-supported `.dat` fixture with `force_gdf=True`/conversion policy enabled | converted GDF output uses explicit `output_path`/`output_name` and is registered through #605 ownership |
| `test_orcawave_mesh_prepare_unsupported_format_fails` | clear error surface | `.iges` body mesh | `OrcaWaveMeshPreparationError` message names asset type, extension, and supported formats |
| `test_orcawave_free_surface_fdf_passthrough` | valid FDF auxiliary mesh not rejected | QTF spec with parseable free-surface `.fdf` mesh | packaged passthrough and `FreeSurfacePanelledZoneMeshFileName` emitted |
| `test_orcawave_free_surface_invalid_fdf_fails` | invalid FSZ input fails before solve | empty/unparseable `.fdf` | `OrcaWaveMeshPreparationError` naming the FSZ path |
| `test_orcawave_free_surface_corrupt_fdf_header_fails` | structural FDF validation is not just non-empty | `.fdf` with mismatched panel count or malformed rows | `OrcaWaveMeshPreparationError` with structural reason |
| `test_orcawave_free_surface_non_fdf_requires_source_backed_mapping` | FSZ policy does not borrow control-surface semantics | QTF spec with `.csf` or other non-`.fdf` free-surface mesh without source-backed FSZ mapping | clear unsupported-FSZ error; no accidental body/control conversion rules |
| `test_orcawave_free_surface_auto_has_no_mesh_asset` | FSZ auto mode does not spuriously validate files | QTF spec with `FreeSurfaceZoneSpec(type="auto")` and no mesh file | preparer records no FSZ mesh asset and does not raise missing/non-FDF error |
| `test_orcawave_mesh_prepare_convert_all_no_cross_solver_leak` | OrcaWave prep does not mutate shared spec | `SpecConverter.convert_all()` with AQWA+OrcaWave | AQWA keeps its expected mesh target and OrcaWave uses prepared file |
| `test_orcawave_mesh_prepare_body_control_surface_warns_until_609` | known body-level field edge is explicit without breaking current solver-flag semantics | `BodySpec.control_surface.mesh_file` plus any new control-surface metadata fields | solver flag semantics are preserved, warning names #609 deferral, and no silent miss occurs |
| `test_orcawave_mesh_prepare_uses_609_selected_control_surface_when_available` | #606 does not regress #609 semantics | #609 accessor available and body-level control-surface mesh selected | prepared asset uses selected body-level mesh instead of rejecting it |
| `test_orcawave_damping_lid_mesh_policy` | damping lid participates in taxonomy | spec with damping lid mesh | converted/passthrough per policy and YAML references prepared asset |
| `test_orcawave_prepared_asset_filename_disambiguates_converted_same_stems` | converted outputs cannot collide while passthrough collision stays #605-owned | two force-converted MeshPipeline-supported sources with the same basename from different asset types | converted output filenames include asset ids and differ; native passthrough same-stem collision remains a #605 resolver failure |
| `test_orcawave_prepared_asset_not_overwritten_by_source` | runner does not recopy source over converted output | converted mesh shares basename with source | prepared output remains intact and `RunResult.mesh_files` records it |
| `test_orcawave_runner_consumes_mesh_manifest_without_legacy_copy` | runner/preparer contact surface is concrete | output dir containing `orcawave_mesh_manifest.json` and prepared files | runner records manifest output paths and does not resolve original spec mesh paths back through `spec_dir` |
| `test_orcawave_runner_manifest_paths_override_copy_mesh_files` | runner API is named and deterministic | `RunConfig(package_manifest_path=..., mesh_manifest_path=..., copy_mesh_files=True)` | prepared manifest assets are not recopied from source paths |
| `test_orcawave_runner_direct_run_prepares_before_yaml_generation` | direct `run-orcawave` path gets the same preparation as conversion | spec with native and converted assets and no existing manifest | runner creates/uses in-process staged package before backend YAML generation and generated YAML references prepared metadata |
| `test_orcawave_package_manifest_owns_mesh_manifest_and_prepared_assets` | #605/#606 manifests do not conflict | package containing converted assets | `orcawave_package_manifest.json` lists `orcawave_mesh_manifest.json` and all prepared mesh outputs through the #606-owned registration extension, so rerun cleanup treats them as owned |
| `test_orcawave_mesh_preparer_uses_resolver_staged_copy_and_owned_registration_only` | #606 does not create a second package copy path | instrumented #605 resolver plus passthrough and converted assets | passthrough writes are observed through #605 resolver staging and converted/manifest writes are observed through #606 package-manifest registration, with no direct final-output mesh copy |
| `test_orcawave_mesh_metadata_records_source_and_target` | provenance available | converted mesh | metadata includes source/target paths and formats |
| `test_orcawave_mesh_manifest_records_units_symmetry_provenance` | provenance not squeezed into `RunResult.mesh_files` | converted mesh with units/symmetry metadata | manifest includes units/symmetry/source, `RunResult.mesh_files` remains paths |
| `test_orcawave_backend_prepared_metadata_overrides_auto_format` | backend consumes prepared format/units/symmetry map | converted `.stl` and native `.dat` where source spec says `auto` | YAML emits prepared filename and source-backed format rather than `Auto` or stale backend defaults |
| `test_orcawave_mesh_prepare_modular_filename_map` | modular output uses prepared names too | modular OrcaWave generation with prepared body and auxiliary meshes | `04_bodies.yml` and `master.yml` use prepared filenames/formats according to #605 modular policy |

## Acceptance Criteria

- [ ] Supported non-GDF meshes can be converted into OrcaWave-ready package assets.
- [ ] Already-ready GDF input is not rewritten unnecessarily.
- [ ] Generated YAML points at prepared asset filenames for all mesh write sites, not only body meshes.
- [ ] Generated YAML also points at prepared mesh format/units/symmetry metadata, so converted/non-native inputs do not emit stale `Auto` values.
- [ ] Source-backed OrcaWave-native `.gdf` and `.dat` body/damping assets pass through with correct mesh-format metadata (`Wamit gdf`, `Aqwa dat`); control-surface native passthrough is either supported by explicit schema/#609 metadata or deferred clearly; supported conversion formats such as `.stl` produce GDF; unknown or unsourced formats fail with a domain-specific supported-format list.
- [ ] `.obj` and `.msh` are fail-closed unless this issue or a later reviewed issue adds MeshPipeline handlers plus source-backed conversion/passthrough policy; they are not native passthrough by default.
- [ ] Shared `ControlSurfaceSpec` changes, if any, are guarded so body-level mesh preparation remains #609 scope while current `BodySpec.control_surface` solver-flag behavior is preserved and warning-visible before new metadata fields are interpreted.
- [ ] If #609 lands first, #606 consumes #609's selected control-surface accessor and does not reject valid body-level control-surface meshes.
- [ ] Valid `.fdf` free-surface meshes pass through with the documented fixture header grammar; non-FDF FSZ formats require a source-backed backend mapping and otherwise fail with an explicit unsupported-format message.
- [ ] Tests cover passthrough, conversion, unsupported format, damping lid behavior, `.fdf` free-surface passthrough with QTF solve type, runner overwrite protection, and metadata.
- [ ] #605 has landed the shared `orcawave_asset_resolver.py` first; #606 extends it with tested preparation/owned-file registration APIs and does not introduce a parallel path-resolution/copy implementation.
- [ ] Pre-implementation import gate passes against `main`: `from digitalmodel.hydrodynamics.diffraction.orcawave_asset_resolver import OrcaWaveAssetResolver`.
- [ ] `orcawave_mesh_manifest.json` carries source/target format, units, symmetry, and warning provenance; `RunResult.mesh_files` remains a path list for compatibility.
- [ ] `OrcaWaveRunner` consumes `orcawave_mesh_manifest.json` or an in-process manifest handoff to avoid legacy source-copy overwrites of prepared assets.
- [ ] #606 adds or consumes concrete `RunConfig.package_manifest_path` / `mesh_manifest_path` handoff fields; runner manifest tests do not depend on an unnamed future issue.
- [ ] `orcawave_mesh_manifest.json` and prepared mesh outputs are written under the #605 staging/publish flow and listed as owned files in `orcawave_package_manifest.json` through APIs added or extended by #606.
- [ ] Pass-through copies are produced through #605 resolver staging and converted outputs are registered through #606's package-manifest extension; #606 does not introduce a second final-output copy path.
- [ ] `MeshPipeline.prepare_for_solver()` has direct tests for explicit `output_path`/`output_name`, conflicting output arguments, and the existing default naming behavior.
- [ ] OrcaWave preparation does not mutate or clone shared `SpecConverter.spec`; AQWA output remains unchanged in `convert_all()` while OrcaWave uses prepared metadata.
- [ ] Backend helper functions that emit mesh filename/format fields accept the prepared metadata map explicitly, including `_build_body_dict`, `_build_damping_lid_section`, and `_build_qtf_section`; tests cover those helper boundaries.

## Plan Review Gating

- [ ] Completed review artifacts under `/mnt/local-analysis/workspace-hub/digitalmodel/scripts/review/results/` exist for at least two providers and each non-empty artifact contains a `## Verdict` section; 0-byte in-progress files do not satisfy this gate.
- [ ] Any provider `MAJOR` finding requires a plan revision and re-review; the issue is commented with this plan and moved to `status:plan-review` only after no unresolved `MAJOR` findings remain.

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | PENDING | Awaiting review artifact |
| Codex | PENDING | Awaiting review artifact |
| Gemini | PENDING | Awaiting review artifact |

**Overall result:** PENDING - do not label `status:plan-review` until artifacts exist and no unresolved `MAJOR` findings remain.

## Risks and Open Questions

- **Risk:** #500/#605 must establish the shared resolver first. #606 is blocked until that resolver lands and must rebase onto it instead of introducing a competing path-resolution/copy implementation.
- **Risk:** The preparer naturally wants to copy passthrough assets and converted outputs, but that would duplicate #605 package ownership and rollback logic. #606 must extend the landed #605 package module with explicit preparation and owned-generated-file registration APIs, then use those APIs so package publish, manifest ownership, and rollback stay single-sourced.
- **Risk:** The current backend derives YAML mesh format from the source spec, so the prepared-metadata map is required for native passthrough; filename-only overrides are insufficient.
- **Risk:** `ControlSurfaceSpec` is shared by vessel-level and body-level control-surface fields. Any schema extension must be guarded so #606 does not accidentally implement #609's deferred body-level semantics.
- **Risk:** Existing command surfaces are broad; #606 changes internals only and must not add or rename CLI commands unless covered by a separate issue.
- **Risk:** #608 quality gates are deliberately not a prerequisite for #606. #606 should emit manifest/provenance hooks that #608 can consume later, avoiding a circular dependency.
- **Risk:** Licensed OrcaWave/OrcFxAPI behavior cannot be fully verified on Linux; tests requiring a license must skip cleanly and be proven on the licensed host where applicable.
- **Open:** Default review policy uses three-provider review; Claude, Codex, and Gemini are the selected review set for this plan.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
