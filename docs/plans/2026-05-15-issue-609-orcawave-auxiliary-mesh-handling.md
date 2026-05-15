# Plan for #609: OrcaWave: normalize auxiliary mesh handling for control surfaces and QTF inputs

> **Status:** plan-review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/609
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-609-claude.md | scripts/review/results/2026-05-15-plan-609-codex.md | scripts/review/results/2026-05-15-plan-609-gemini.md

---

## Scope

Auxiliary mesh consistency scope. Body mesh packaging remains #605; conversion remains #606; runner path-resolution and copy mechanics remain #500/#605. This issue defines the schema/backend/accessor contract for auxiliary assets and updates existing runner auxiliary selection, validation, and copy inputs to call the same accessor-selected assets, using the runner's current path semantics rather than adding new path-resolution or copy-policy behavior. Preparer integration is blocked until #606 provides `orcawave_mesh_preparer.py`.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#609` - OPEN - `OrcaWave: normalize auxiliary mesh handling for control surfaces and QTF inputs`; label: `enhancement`.
- `#500` - OPEN - `OrcaWave: mesh file pre-flight validation + auto-copy in runner`; label includes `status:plan-approved`.

### Sources consulted

- `AGENTS.md` - digitalmodel declares `PYTHONPATH=src uv run python -m pytest` as the repository test command and points source ownership at `src/digitalmodel/`.
- `docs/plans/` - repo has standalone plan files but no `docs/plans/README.md` index/template; issue #596 explicitly recorded "no `docs/plans/README.md` in this issue", so these plans follow the existing standalone-file convention.
- `src/digitalmodel/hydrodynamics/diffraction/cli.py` - current Click surface includes `convert-aqwa`, `convert-orcawave`, `compare`, `batch`, `convert-spec`, `validate-spec`, `run-orcawave`, `run-aqwa`, `batch-aqwa`, `batch-orcawave`, `plot-raos`, `mesh-build`, and benchmark commands; there is no given-mesh or doctor command yet.
- `src/digitalmodel/hydrodynamics/diffraction/spec_converter.py` - `SpecConverter.convert()` delegates directly to backends and `validate()` checks non-empty mesh strings, frequencies, headings, and positive mass only.
- `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` - runner can generate OrcaWave input, copy existing mesh files, prefer OrcFxAPI, and fall back to dry-run when no API/executable is available.
- `src/digitalmodel/hydrodynamics/diffraction/mesh_pipeline.py` - existing pipeline can load/validate/prepare meshes and maps OrcaWave target format to GDF, but it is not integrated into `SpecConverter` or `OrcaWaveRunner`.
- Related issue `#500` - open plan-approved issue for mesh pre-flight validation and runner auto-copy; these future issues must not duplicate its direct scope.

### Issue-specific code findings

- `input_schemas.py` defines both `VesselSpec.control_surface` and `BodySpec.control_surface`.
- `orcawave_backend.py` currently reads `getattr(vessel, "control_surface", None)` when generating body control-surface YAML.
- `orcawave_runner.py` copies `getattr(body.vessel, "control_surface", None)` and does not inspect `BodySpec.control_surface`.
- `orcawave_backend.py` also generates damping lid and free-surface zone filename fields when present.
- Valid field sites are two-tier for each body: `BodySpec.control_surface` and `body.vessel.control_surface`. In single-body specs, top-level `spec.vessel.control_surface` is the same object exposed through `spec.get_bodies()[0].vessel.control_surface`; valid multi-body specs cannot also set top-level `spec.vessel`.
- Existing tests under `tests/hydrodynamics/diffraction/` have no current `control_surface` coverage (`rg -n "control_surface" tests/hydrodynamics/diffraction tests` returns no matches), so #609 must add the first backend/runner tests for this surface.

### Gaps identified

- Body-level control-surface mesh filenames can be silently ignored in multi-body specs: the backend reads `BodySpec.control_surface` only for the quadratic-load control-surface flag but emits `BodyControlSurfaceMeshFileName` from `body.vessel.control_surface`.
- The same selected control-surface source must drive both `BodyControlSurfaceMeshFileName` and the general `QuadraticLoadControlSurface` flag; fixing only the filename would leave internally inconsistent YAML.
- There is no documented precedence rule if both vessel-level and body-level control surfaces are set.
- Auxiliary meshes do not share a single resolver with body meshes.

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

Captured reproduction proof on 2026-05-15: an inline `PYTHONPATH=src uv run python` script constructed a multi-body `DiffractionSpec` with `body.control_surface.mesh_file = "body_cs.gdf"` and `body.vessel.control_surface.mesh_file = "vessel_cs.gdf"`, then called `OrcaWaveBackend().generate_single(spec, tmpdir)`. The generated YAML contained `BodyControlSurfaceType: Defined by mesh file`, `BodyControlSurfaceMeshFileName: vessel_cs.gdf`, `BodyControlSurfaceMeshFormat: Wamit csf`, and top-level `QuadraticLoadControlSurface: Yes`; `body_cs.gdf` was absent from the emitted body control-surface filename. The first TDD test below must lock this observed failure before fixing it.

## Deliverable

Control surface, damping lid, and free-surface panelled-zone mesh references use one documented resolution/precedence rule across schema, backend, validation, and runner packaging.

## Proposed Tasks

1. Make the real precedence rule explicit before implementation: for each body, `BodySpec.control_surface` overrides `body.vessel.control_surface`. There is no separate top-level fallback in valid multi-body specs; single-body top-level `spec.vessel.control_surface` is handled as `body.vessel.control_surface` through `get_bodies()`. If higher-priority and lower-priority fields differ, emit an `AuxiliaryMeshResolutionWarning` naming the selected source, shadowed source, selected mode/path, shadowed mode/path, body id/name, and field sources.
2. Define #609-owned helper models in a new module such as `src/digitalmodel/hydrodynamics/diffraction/orcawave_auxiliary_assets.py`: `AuxiliaryMeshAsset(asset_id, asset_type, source_field, mesh_file, mesh_format, length_units, metadata)`, `AuxiliaryMeshFinding(severity: Literal["INFO","WARN","ERROR"], code, body_id, selected_source, shadowed_source, selected_path, shadowed_path, message)`, `AuxiliaryMeshResolution(asset_id, asset_type, body_id, source_field, mode, asset, findings)`, and `AuxiliaryResolutionReport(resolutions, findings)`. `body_id` is required on every resolution, using `None` only for spec-level damping-lid/free-surface assets. `metadata` preserves non-control fields such as damping-lid damping factor, free-surface-zone inner radius, and available body symmetry/units. Add optional `mesh_format` metadata to `ControlSurfaceSpec` if no landed equivalent exists; for control-surface meshes, derive OrcaWave format from explicit schema metadata first, otherwise from the mesh extension with a source-backed map: `.gdf -> Wamit gdf` from the repository OrcaWave examples and `.csf -> Wamit csf` from current backend behavior. Unknown control-surface mesh extensions without explicit schema format are helper-level `ERROR` findings; do not silently default to CSF. `length_units` is derived from the owning body's `vessel.geometry.length_units`. Helper APIs return typed findings; `SpecConverter.validate()` appends only `ERROR` findings to its existing `list[str]` failure surface, while `WARN`/`INFO` findings are exposed through `SpecConverter.auxiliary_resolution_report()` and `validate-spec --auxiliary-warnings` / `--auxiliary-report-json` for CLI/preflight/docs consumers without making the spec invalid. Serialization into #605/#611 manifests is follow-up integration that must use the landed manifest schema versions; #609 does not invent those manifest contracts.
3. Add helper accessors for control-surface, damping-lid, and free-surface auxiliary meshes and use them from backend, validation, current runner auxiliary validation, and runner auxiliary copy selection. Control surfaces are per-body and return `mode="mesh"`, `mode="auto"`, or `mode="none"`. Damping lid and free-surface zone are spec-level assets (`spec.damping_lid`, `spec.free_surface_zone`), not body-keyed assets; their helper records use `body_id=None` and asset ids `damping_lid` / `free_surface_zone`. #609 closes its independently landable scope by making these selected auxiliary assets the only source consumed by the current backend and runner auxiliary validation/copy loop. The runner contract is explicit because current `OrcaWaveRunner._validate_mesh_references()` validates only body meshes: add `selected_auxiliary_assets(spec) -> AuxiliaryResolutionReport` and `validate_selected_auxiliary_assets(spec, spec_path: Path | None, output_dir: Path) -> AuxiliaryResolutionReport` (or equivalently named APIs with the same fields), then extend `_copy_mesh_files()` and `_validate_mesh_references()` to consume only those selected assets for control/damping/FSZ path checks. #500/#605 may later replace severity/reporting with their shared resolver/preflight machinery, but #609 must at least pass damping-lid and free-surface mesh references into the existing runner-visible preflight/copy path so they are not silently skipped.
4. Define `type="auto"` and invalid-control-surface semantics using repository-cited OrcaWave YAML keys rather than guessed names. A higher-priority `BodySpec.control_surface(type="auto")` is a selected outcome and shadows a lower-priority mesh; it records existing schema metadata `panel_size` and `separation` and may add optional `include_free_surface` to `ControlSurfaceSpec` because repository OrcaWave examples include `BodyControlSurfaceIncludeFreeSurface`. Backend generation preserves solver-level control-surface enablement for non-QTF output, emits no `BodyControlSurfaceMeshFileName`, and emits source-backed auto keys: `BodyControlSurfaceType: Automatically generated`, `BodyControlSurfacePanelSize`, `BodyControlSurfaceSeparationFromBody`, and `BodyControlSurfaceIncludeFreeSurface` when the corresponding schema value is present. If a required auto metadata value is absent, emit only the supported subset plus an INFO finding naming the omitted optional key; do not fall through to a lower-priority mesh. `type="mesh"` without `mesh_file`, and unknown `type` values, are helper-level validation errors and must not silently shadow or fall through to a valid lower-priority mesh; #609 should not tighten the Pydantic field to `Literal` unless a compatibility note confirms existing specs with typos are meant to fail at schema-load time.
5. Use the same accessor result for both `BodyControlSurfaceMeshFileName` / auto-control-surface body fields and solver-section `QuadraticLoadControlSurface` enablement so backend YAML cannot disagree about which control-surface source is active. Preserve existing solver/QTF behavior: if generating a full/diagonal QTF section (`is_qtf` path), keep `QuadraticLoadControlSurface: No` as current backend/tests require; otherwise set it from `solver.qtf_calculation or any(body resolution mode in {"mesh","auto"})`. Per-body mesh fields still come only from each body's selected `mode="mesh"` resolution; selected `mode="auto"` emits source-backed auto keys and no mesh-file key.
6. Include damping lid and free-surface panelled-zone meshes in shared auxiliary discovery metadata without changing #606 format-conversion policy. Existing runner auxiliary validation and copy should use the accessor-selected assets for consistency; this changes only which source references are handed to the current runner validation/copy loop, not how paths are resolved, copied, or diagnosed. Missing-file/path-resolution severity and copy mechanics remain owned by #500/#605, but #609's runner tests must prove body/control/damping/FSZ selected assets are all visible to `_copy_mesh_files()` and `_validate_mesh_references()` through the selected-asset helper. #609 also passes enough contract fields for future #500/#605 consumers: `asset_id`, `asset_type`, `body_id`, `source_field`, `mesh_file`, `mesh_format`, `length_units`, and `metadata`.
7. Define invalid and automatic free-surface-zone semantics with source-backed OrcaWave keys. `FreeSurfaceZoneSpec(type="mesh", mesh_file=None)` is a validation error; `type="auto"` with no mesh is allowed and returns no mesh asset, but backend generation should emit source-backed auto keys when QTF/free-surface output is generated: `FreeSurfacePanelledZoneType: Automatically generated`, `FreeSurfacePanelledZonePanelSize`, and `FreeSurfacePanelledZoneInnerRadius` when schema values exist. If current `FreeSurfaceZoneSpec` lacks a needed panel-size or inner-radius field, add the optional schema field in #609 or explicitly defer only that missing field with an INFO finding; do not silently omit the whole auto mode. Unknown `type` values are validation errors. Existing body `VesselGeometry.symmetry` is recorded in the informational finding, but it is insufficient to prove free-surface-zone global-coordinate/symmetry conventions because FSZ-specific coordinate-system metadata is absent. The finding should say which body symmetry values were available and name the follow-up schema need.
8. Add control-surface coordinate/symmetry metadata checks as informational findings, not blocking validation: current `ControlSurfaceSpec` lacks coordinate-system and control-surface symmetry metadata, while OrcaWave references require body-coordinate assumptions and sometimes different control-surface symmetry. The accessor should preserve available body geometry symmetry/units and emit a `metadata_unavailable` info finding naming the missing control-surface coordinate/symmetry fields.
9. Document schema behavior, conflict warning behavior, and migration guidance from vessel-level defaults to body-level overrides for multi-body specs, including recommended new multi-body layout, when legacy vessel-level fields remain valid, warning examples, and no automatic rewrite in #609. The docs must explicitly state that single-body top-level specs cannot express a separate `BodySpec.control_surface` override because `get_bodies()` wraps `spec.vessel` with `body.control_surface=None`; users needing body-level override semantics should migrate to an explicit `bodies:` list.
10. Add multi-body tests proving per-body auxiliary meshes are not ignored.

## Pseudocode

```text
resolve_control_surface(spec, body, body_index):
  candidates = [
    ("body.control_surface", body.control_surface),
    ("body.vessel.control_surface", body.vessel.control_surface),
  ]
  selected = first candidate whose control_surface is not None
  if selected is None: return AuxiliaryMeshResolution(body_id=body.id/name/index, mode="none", control_surface=None, findings=[])
  if selected.type not in {"mesh", "auto"}: return validation error allowed types are mesh|auto
  if selected.type == "mesh" and not selected.mesh_file: return validation error without falling through
  if selected.type == "auto": mode = "auto"; mesh_file = None; metadata = panel_size/separation/include_free_surface; auto YAML keys are source-backed
  elif selected.mesh_file: mode = "mesh"; mesh_file = selected.mesh_file; mesh_format = explicit ControlSurfaceSpec.mesh_format if present else extension-derived .gdf->Wamit gdf/.csf->Wamit csf; length_units = body.vessel.geometry.length_units
  else: return validation error for ambiguous non-null control_surface
  findings = shadowed candidate WARN findings where lower-priority candidate has different mode/path
  return AuxiliaryMeshResolution(asset_id=f"body_{i}_control_surface", body_id=body.id/name/index, mode=mode, asset=asset_or_none, findings=findings)

backend_body_yaml(body):
  cs = resolve_control_surface(spec, body, i)
  if cs has ERROR findings: SpecConverter.validate() returns errors; backend generation raises ValueError if called with unresolved errors
  set per-body control-surface fields from cs.mode
  set solver-level QuadraticLoadControlSurface = "No" for QTF output; otherwise solver.qtf_calculation OR any body cs.mode in {"mesh","auto"}
  if cs.mode == "mesh": emit BodyControlSurfaceMeshFileName = basename(cs.asset.mesh_file) and format/length metadata from cs.asset
  if cs.mode == "auto": emit BodyControlSurfaceType = "Automatically generated", panel-size/separation/include-free-surface fields when present, and no BodyControlSurfaceMeshFileName
  serialize warnings to validation/preflight manifest surfaces
```

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-609-orcawave-auxiliary-mesh-handling.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-609-claude.md` |
| Plan review - Codex (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-609-codex.md` |
| Plan review - Gemini (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-609-gemini.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-609-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` | document precedence and add optional control-surface `mesh_format` metadata if no landed equivalent exists; helper-level validation owns `type` errors unless compatibility review approves stricter schema enums |
| Create | `src/digitalmodel/hydrodynamics/diffraction/orcawave_auxiliary_assets.py` | #609-owned accessor, resolution, and warning dataclasses |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py` | generate YAML from canonical auxiliary accessor and raise a clear `ValueError` if called with unresolved auxiliary validation errors |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` | use canonical accessor for current auxiliary copy selection without changing #500 path/preflight policy |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/spec_converter.py` | render accessor conflict/invalid-control-surface warnings or errors through existing `list[str]` validation surface |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/cli.py` | expose auxiliary WARN/INFO findings via `validate-spec --auxiliary-warnings` and optional JSON report output |
| Modify later (blocked follow-up) | `src/digitalmodel/hydrodynamics/diffraction/orcawave_asset_resolver.py` | consume #609 auxiliary accessor only after #605 creates this file |
| Modify later (blocked) | `src/digitalmodel/hydrodynamics/diffraction/orcawave_mesh_preparer.py` | preparer integration only after #606 creates this file |
| Create/modify | `tests/hydrodynamics/diffraction/test_orcawave_auxiliary_meshes.py` | multi-body and QTF tests |
| Modify | `docs/domains/orcawave/README.md` | schema precedence and migration guidance |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_body_level_control_surface_generates_yaml_value` | body-level field honored | multi-body spec with body and vessel control surfaces | `BodyControlSurfaceMeshFileName` equals basename of `body.control_surface.mesh_file` |
| `test_reproduction_body_level_control_surface_currently_ignored_pre_fix_red` | captured failure is used only as the first TDD red step | current HEAD backend with body mesh `body_cs.gdf` and vessel mesh `vessel_cs.gdf` | temporary red/proof test demonstrates current `vessel_cs.gdf` behavior, then is replaced or inverted before the final passing suite |
| `test_vessel_level_control_surface_applies_when_body_missing` | legacy/default behavior preserved | body without override, nested vessel-level control surface | emitted filename equals `body.vessel.control_surface.mesh_file` basename |
| `test_single_vessel_control_surface_uses_get_bodies_path` | top-level single-body behavior is not a separate tier | single-vessel spec with `spec.vessel.control_surface` | emitted filename equals `body.vessel.control_surface.mesh_file` after `get_bodies()` wrapping |
| `test_control_surface_precedence_conflict_warns` | conflict not silent | body + nested vessel control surfaces with different files | body-level file emitted and serialized warning lists selected and shadowed paths |
| `test_control_surface_auto_shadows_lower_mesh` | `type="auto"` participates in precedence | body auto control surface plus vessel mesh control surface | accessor selects `mode="auto"` with no mesh asset and lower mesh not selected |
| `test_control_surface_auto_emits_source_backed_yaml_keys` | #609 uses cited OrcaWave auto keys, not guessed names | body auto control surface with panel size/separation/include-free-surface and lower mesh | backend emits `BodyControlSurfaceType: Automatically generated`, panel-size/separation/include-free-surface fields, no `BodyControlSurfaceMeshFileName`, and lower mesh is not emitted |
| `test_control_surface_mesh_type_without_mesh_file_errors` | invalid higher-priority control surface is not silent | body `ControlSurfaceSpec(type="mesh", mesh_file=None)` plus vessel mesh | validation error names body field; lower mesh is not silently selected |
| `test_control_surface_unknown_type_errors_without_fallthrough` | arbitrary type strings are rejected | body `ControlSurfaceSpec(type="banana", mesh_file="x.csf")` plus vessel mesh | validation error names allowed types and selected asset is not the lower-priority vessel mesh |
| `test_control_surface_selected_mesh_source_drives_quadratic_flag` | backend YAML is internally consistent | multi-body spec with selected body-level mesh surface | `QuadraticLoadControlSurface` and body control-surface filename/type fields follow the same selected mesh source |
| `test_control_surface_mesh_derives_gdf_format_and_length_units` | repository example format is not overwritten by the old CSF default | selected `.gdf` mesh control surface on body with `length_units="m"` | YAML emits `BodyControlSurfaceMeshFormat: Wamit gdf` and `BodyControlSurfaceMeshLengthUnits: m` |
| `test_control_surface_mesh_derives_csf_format_from_extension` | current backend CSF behavior is retained only for CSF inputs | selected `.csf` mesh control surface on body with `length_units="m"` | YAML emits `BodyControlSurfaceMeshFormat: Wamit csf` |
| `test_control_surface_unknown_extension_requires_explicit_format` | helper cannot silently default to CSF | selected control-surface mesh with unknown suffix and no `mesh_format` | validation error names unsupported control-surface mesh format and asks for explicit schema metadata |
| `test_no_control_surface_returns_empty_resolution` | no-candidate outcome explicit | body with no control-surface fields | accessor returns `mode="none"` and backend omits control-surface mesh fields |
| `test_solver_quadratic_load_control_surface_preserves_qtf_branches` | solver-level flag follows existing QTF semantics | qtf solve type, `solver.qtf_calculation=True`, and selected control-surface combinations | full/diagonal QTF output keeps `QuadraticLoadControlSurface=False`; non-QTF output uses `solver.qtf_calculation or any selected mesh/auto` |
| `test_runner_reproduction_body_control_surface_copy_uses_body_override` | runner-copy parallel bug is locked before fix | body and vessel control-surface meshes differ | runner auxiliary copy/preflight selection uses body-level selected asset, not vessel-level shadowed asset |
| `test_runner_uses_accessor_for_auxiliary_copy_selection` | runner cannot diverge from backend selection | body-level and vessel-level control-surface meshes differ | runner copies/preflights the selected body-level asset through existing copy policy |
| `test_runner_validate_mesh_references_uses_selected_auxiliary_assets` | runner validation is extended beyond body meshes | control-surface, damping-lid, and FSZ selected assets missing from output directory | `_validate_mesh_references()` reports selected auxiliary missing references using the helper result rather than silently passing |
| `test_backend_invalid_control_surface_errors_before_yaml` | direct backend path cannot bypass helper errors | invalid body control surface passed to `generate_single()`/`generate_modular()` | backend raises `ValueError` and writes no inconsistent YAML |
| `test_runner_invalid_auxiliary_resolution_errors_before_prepare_output` | runner path cannot bypass helper errors | invalid control-surface/free-surface spec through `_generate_input_files()`/dry run | runner reports failure before solve/package output rather than generating inconsistent YAML |
| `test_auxiliary_asset_metadata_preserves_damping_and_fsz_fields` | helper model fits all aux assets | damping lid and free-surface zone specs | asset records include mesh format, length units, damping factor/inner radius metadata as applicable |
| `test_control_surface_asset_derives_format_and_units` | #609 asset model has concrete provenance before #606 | selected `.gdf` or `.csf` control-surface mesh without schema format/unit fields | asset has extension-derived `mesh_format` and `length_units` from owning body geometry |
| `test_free_surface_mesh_type_without_mesh_file_errors` | FSZ mesh mode is not silent no-op | `FreeSurfaceZoneSpec(type="mesh", mesh_file=None)` | validation error names free-surface zone mesh_file |
| `test_free_surface_auto_has_no_mesh_asset` | automatic FSZ does not invent a mesh | `FreeSurfaceZoneSpec(type="auto", mesh_file=None)` | helper returns no mesh asset and no missing-file error |
| `test_free_surface_auto_emits_source_backed_yaml_keys` | automatic FSZ YAML behavior is explicit | `FreeSurfaceZoneSpec(type="auto")` with panel-size/inner-radius metadata in a QTF/free-surface output | backend emits `FreeSurfacePanelledZoneType: Automatically generated` plus supported auto metadata keys and no mesh filename |
| `test_free_surface_unknown_type_errors` | arbitrary FSZ modes rejected | `FreeSurfaceZoneSpec(type="banana")` | validation error names allowed types |
| `test_auxiliary_assets_expose_resolver_contract_fields` | future #500/#605 seam has concrete fields without importing missing module | body/control/damping/FSZ selected assets | helper output contains asset_id, asset_type, body_id, source_field, mesh_file, mesh_format, length_units, metadata |
| `test_free_surface_qtf_metadata_absent_reports_info` | QTF assumptions not guessed | free-surface mesh spec plus body symmetry values | informational metadata-unavailable finding includes available body symmetry and names missing FSZ coordinate/symmetry metadata |
| `test_control_surface_coordinate_symmetry_metadata_absent_reports_info` | control-surface coordinate assumptions are not guessed | selected control-surface mesh with body geometry symmetry | informational finding names missing control-surface coordinate-system/symmetry metadata |
| `test_validate_spec_auxiliary_warnings_visible_without_failing` | WARN/INFO findings have a public CLI surface | control-surface precedence conflict plus `validate-spec --auxiliary-warnings` | command exits 0, prints selected/shadowed auxiliary warning, and does not include the warning in `SpecConverter.validate()` errors |
| `test_validate_spec_auxiliary_report_json_contains_warn_info` | helper report is machine-readable | same conflict plus `--auxiliary-report-json report.json` | JSON report contains WARN/INFO findings with selected/shadowed source fields |
| `test_runner_validates_and_copies_damping_and_fsz_selected_assets` | #609 issue acceptance covers damping lid and FSZ, not only control surfaces | spec with body/control/damping/free-surface mesh references | current runner auxiliary validation/copy seams receive all selected auxiliary assets; none are silently skipped |

## Acceptance Criteria

- [ ] When `BodySpec.control_surface.mesh_file` is set, emitted `BodyControlSurfaceMeshFileName` equals that basename and does not silently use `body.vessel.control_surface`.
- [ ] Vessel/body control-surface precedence is documented and tested.
- [ ] Backend YAML, validation, current runner auxiliary validation, and current runner auxiliary copy selection use the same accessor rules; #500/#605 still own shared path-resolution/preflight policy and may replace severity/reporting later.
- [ ] #609 selected auxiliary assets expose a concrete field contract for future #500/#605 resolver/preflight integration, but #609 can land independently without importing nonexistent resolver/preparer modules.
- [ ] `type="auto"` shadows lower-priority meshes and preserves `panel_size`/`separation`/`include_free_surface` metadata; backend emits the source-backed automatic control-surface YAML keys and no mesh filename.
- [ ] Invalid `type="mesh"` without `mesh_file` and unknown `type` values surface validation errors and do not silently select lower-priority assets.
- [ ] Damping lid and free-surface mesh references are discoverable through auxiliary metadata that preserves their format, units, body_id/null scope, and domain-specific fields, and they are passed into current runner validation/copy seams.
- [ ] Control-surface mesh output derives `BodyControlSurfaceMeshFormat` from explicit schema metadata or known suffixes (`.gdf -> Wamit gdf`, `.csf -> Wamit csf`) and preserves `BodyControlSurfaceMeshLengthUnits` behavior.
- [ ] Multi-body tests cover per-body auxiliary behavior.
- [ ] Solver-level `QuadraticLoadControlSurface` preserves existing QTF/full-QTF behavior and uses `solver.qtf_calculation or selected control-surface modes` only for non-QTF output.
- [ ] Free-surface-zone `type="mesh"` without `mesh_file`, `type="auto"`, and unknown `type` values have explicit validation behavior; source-backed automatic FSZ YAML keys are emitted when metadata is present.
- [ ] Backend and runner direct paths raise/report errors for invalid auxiliary resolution before generating inconsistent YAML.
- [ ] Auxiliary WARN/INFO findings are visible through `SpecConverter.auxiliary_resolution_report()` and the `validate-spec` CLI without making the spec invalid.
- [ ] Shared resolver/preparer integration is explicitly gated as a follow-up on #500/#605/#606; backend/schema/accessor/current-runner-validation/current-runner-copy behavior can land independently.
## Plan Review Gating

- [ ] Completed review artifacts under `/mnt/local-analysis/workspace-hub/digitalmodel/scripts/review/results/` exist for at least two providers and each non-empty artifact contains a `## Verdict` section; 0-byte in-progress files do not satisfy this gate.
- [ ] Any provider `MAJOR` finding requires a plan revision and re-review; the issue is commented with this plan and moved to `status:plan-review` only after no unresolved `MAJOR` findings remain.

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | UNAVAILABLE | Review command failed; completed artifact retained |
| Codex | UNAVAILABLE | Codex CLI quota/usage unavailable on final rerun after plan patch |
| Gemini | APPROVE | Notes non-blocking body-id, generic validation, unknown-extension, and schema-field cleanup risks; no blockers |

**Overall result:** READY FOR USER REVIEW - completed artifacts exist, prior MAJOR findings were addressed by plan revisions, and no fresh unresolved MAJOR remains.

## Risks and Open Questions

- **Risk:** Auxiliary asset precedence must be applied before backend YAML and before package/run path resolution. If backend and runner call different accessors, multi-body cases can still diverge.
- **Risk:** #605/#606 own resolver/preparer mechanics and #500 owns existing runner preflight/copy behavior. #609 should add asset discovery/precedence first, then delegate packaging/conversion after those dependencies land.
- **Risk:** This plan's tests are schema/backend/runner-copy tests and should not require a licensed OrcaWave solve; any future licensed behavior belongs in #610.
- **Open:** Default review policy uses three-provider review; Claude, Codex, and Gemini are the selected review set for this plan.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
