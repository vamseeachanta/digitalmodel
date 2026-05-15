# Plan for #607: OrcaWave: add given-mesh CLI workflow for diffraction setup

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/607
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-607-claude.md | scripts/review/results/2026-05-15-plan-607-codex.md | scripts/review/results/2026-05-15-plan-607-gemini.md

---

## Scope

CLI/spec authoring scope. It must not infer mass properties that require engineering input unless an explicit template supplies them. #607 can implement `mesh-to-spec` and dry-run with already-ready mesh formats through the existing `run-orcawave --dry-run` path at HEAD. #605/#606 are dependencies only for future self-contained packaging and non-ready mesh preparation; #607 must not introduce a second mesh resolver or second converter. #607 may check that the user-supplied mesh path exists before authoring a spec, but all package/path-resolution policy remains #500/#605. The issue-body `run-orcawave-from-mesh` shape is allowed only as a thin dry-run orchestration command that calls the same `mesh-to-spec` builder and existing `OrcaWaveRunner` dry-run path; licensed solves still use `run-orcawave <generated.yml>` or a future wrapper flag.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#607` - OPEN - `OrcaWave: add given-mesh CLI workflow for diffraction setup`; label: `enhancement`.
- `#500` - OPEN - `OrcaWave: mesh file pre-flight validation + auto-copy in runner`; label includes `status:plan-approved`.

### Sources consulted

- `AGENTS.md` - digitalmodel declares `PYTHONPATH=src uv run python -m pytest` as the repository test command and points source ownership at `src/digitalmodel/`.
- `/mnt/local-analysis/workspace-hub/docs/plans/_template-issue-plan.md` - T2/T3 plans require pseudocode/design checkpoint, verification commands, and explicit test/no-regression acceptance.
- GitHub issue `#607` body - command design is intentionally flexible but includes `run-orcawave-from-mesh` as a possible shape; #607 should either implement it as a thin orchestration command or record maintainer approval before omitting it.
- `docs/plans/` - repo has standalone plan files but no `docs/plans/README.md` index/template; issue #596 explicitly recorded "no `docs/plans/README.md` in this issue", so these plans follow the existing standalone-file convention.
- `src/digitalmodel/hydrodynamics/diffraction/cli.py` - current Click surface includes `convert-aqwa`, `convert-orcawave`, `compare`, `batch`, `convert-spec`, `validate-spec`, `run-orcawave`, `run-aqwa`, `batch-aqwa`, `batch-orcawave`, `plot-raos`, `mesh-build`, and benchmark commands; there is no given-mesh or doctor command yet.
- `src/digitalmodel/hydrodynamics/diffraction/spec_converter.py` - `SpecConverter.convert()` delegates directly to backends and `validate()` checks non-empty mesh strings, frequencies, headings, and positive mass only.
- `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` - runner can generate OrcaWave input, copy existing mesh files, prefer OrcFxAPI, and fall back to dry-run when no API/executable is available.
- `src/digitalmodel/hydrodynamics/diffraction/mesh_pipeline.py` - existing pipeline can load/validate/prepare meshes and maps OrcaWave target format to GDF, but it is not integrated into `SpecConverter` or `OrcaWaveRunner`.
- Related issue `#500` - open plan-approved issue for mesh pre-flight validation and runner auto-copy; these future issues must not duplicate its direct scope.

### Issue-specific code findings

- `cli.py` has a broader Click surface than the initial four-command summary: existing commands include `convert-spec`, `validate-spec`, `run-orcawave`, `batch-orcawave`, and the adjacent `mesh-build` geometry-to-mesh command. #607 must follow existing Click conventions and keep the semantic split clear: `mesh-build` is geometry spec -> panel mesh; `mesh-to-spec` is existing mesh -> diffraction spec.
- `diffraction_cli.py` is a legacy/parallel argparse entry point. #607 is scoped to the canonical Click CLI in `cli.py`; docs must say the new command is Click-only unless a separate deprecation/parity issue is approved.
- `input_schemas.py` defines required `DiffractionSpec`, `VesselGeometry`, `VesselInertia`, frequency, heading, and environment structures. `mesh_pipeline.py` can load/validate meshes but does not create a `DiffractionSpec`.
- Current `run-orcawave --dry-run` can generate input through `OrcaWaveRunner(RunConfig(dry_run=True)).run(spec, spec_path=...)`; #607 should use that for ready-mesh dry-runs. Because runner mesh copy resolves relative mesh paths against `spec_path.parent`, #607 writes generated specs with paths relative to the generated spec directory, matching #500's spec-relative convention. #605/#606 remain dependencies for self-contained packaging and mesh conversion.
- `DiffractionSpec.to_yaml(path)` already exists in `input_schemas.py`; #607 should reuse it rather than adding a speculative serializer.

### Gaps identified

- No ergonomic starting point exists for users who have a mesh but no canonical spec.
- CLI help does not explain which physical inputs are mandatory and cannot be inferred from geometry alone.
- No tests prove generated specs round-trip through `DiffractionSpec.from_yaml()` and `validate-spec`.

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
rg -n "mesh-to-spec|run-orcawave-from-mesh|orcawave-doctor" src/digitalmodel/hydrodynamics/diffraction/cli.py
ls src/digitalmodel/hydrodynamics/diffraction/mesh_to_spec.py
```

Present-state evidence: `rg` finds no `mesh-to-spec` or `run-orcawave-from-mesh` command in the Click CLI at HEAD, and `ls src/digitalmodel/hydrodynamics/diffraction/mesh_to_spec.py` fails because the builder module does not exist yet. The implementation must still start with the TDD tests below.

Verification outputs captured while drafting: GitHub issue #607 is open with label `enhancement`; `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` defines `VesselInertia.centre_of_gravity`, `VesselInertia.radii_of_gyration`, `VesselInertia.inertia_tensor`, `VesselGeometry.length_units`, top-level `DiffractionSpec.vessel`, top-level `DiffractionSpec.bodies`, `DiffractionSpec.frequencies`, `DiffractionSpec.wave_headings`, and `DiffractionSpec.solver_options`; `DiffractionSpec` rejects setting both `vessel` and `bodies`. These exact schema names are part of the implementation contract below.

## Deliverable

A user-facing CLI workflow can create and optionally dry-run an OrcaWave diffraction spec/package starting from a mesh file plus required physical inputs.

## Proposed Tasks

1. Design command shape consistent with existing Click CLI conventions, especially `mesh-build`: implement `mesh-to-spec` for existing mesh -> `DiffractionSpec` YAML. Also implement `run-orcawave-from-mesh` as a thin convenience orchestration command for the issue-body UX: it must call the same builder, write the generated spec, and then invoke the existing `OrcaWaveRunner(RunConfig(dry_run=True))` path. The wrapper is dry-run-only in #607; help text must say licensed solves are performed by running `run-orcawave <generated-spec>` after inspection or by a later explicit non-dry-run wrapper enhancement. Keep `diffraction_cli.py` unchanged in #607 and document the Click CLI as canonical.
2. Define concrete `mesh-to-spec` CLI flags: `--name` optional with default `mesh.stem`, `--mass`, `--cog x,y,z`, exactly one of `--rog x,y,z` or `--inertia ixx,iyy,izz,ixy,ixz,iyz` after template merge, `--water-depth`, `--frequencies f1,f2,...` or `--frequency-range min,max,count`, `--frequency-input-type frequency|period` defaulting to `frequency` and documented as rad/s for `frequency` and seconds for `period`, `--frequency-distribution linear|logarithmic` defaulting to `linear`, `--headings h1,h2,...` or `--heading-range min,max,increment`, `--mesh-format auto|gdf|dat|stl|msh|obj`, `--mesh-units` optional with effective default `m` only when no template value exists, `--symmetry none|xz|yz|xz+yz` with `both` accepted only as a CLI alias translated to `xz+yz`, `--template`, `--body-index` for selecting a body in complete multi-body templates, `--keep-invalid`, and `--output` as the generated spec path. Internally distinguish explicit CLI values from effective defaults: explicit `--mesh-units` overrides template `length_units`, but an omitted option preserves a template's non-`m` units and only fills `m` when the merged geometry has no units. Build and Pydantic-validate the merged `DiffractionSpec` before writing canonical YAML; schema construction failures exit nonzero without creating an output file, even when `--keep-invalid` is present. `--keep-invalid` is an explicit #607 contract for files that have already been serialized successfully: when post-write converter validation fails, the command exits nonzero and keeps the generated file only if this flag is present; otherwise it removes the generated file.
3. Define `run-orcawave-from-mesh` as a wrapper over the `mesh-to-spec` builder using the same mesh, physical-input, template, body-targeting, validation, and `--keep-invalid` flags except `--output`. The wrapper does not expose or alias `--output` in #607; it exposes `--spec-output` for the generated spec path plus required `--output-dir` for OrcaWave dry-run files. If `--spec-output` is omitted, write `<output-dir>/<mesh.stem>.diffraction.yml` so the runner's spec-relative mesh copy convention is deterministic. It must not introduce separate mesh handling; internally it calls the builder and then the existing runner.
4. Define mesh-format inference explicitly: with `--mesh-format auto`, suffix matching is case-insensitive and maps `.gdf -> gdf`, `.dat -> dat`, `.stl -> stl`, `.msh -> msh`, `.obj -> obj`; unknown suffixes fail unless an explicit supported `--mesh-format` is provided. This suffix table is schema-value inference for authoring a `DiffractionSpec`, not a path resolver and not a `MeshPipeline.detect_format()` replacement. #607 only writes schema/YAML values and can dry-run/copy the referenced mesh through the current runner; it does not prove licensed OrcaWave acceptance for `.stl`, `.msh`, or `.obj`. #606 owns preparation/conversion policy and #610 owns licensed acceptance.
5. Define template format as YAML loaded to a raw dictionary first, then merged with CLI flags and validated as a complete `DiffractionSpec` after merge. Complete single-vessel `DiffractionSpec` templates are valid. Complete multi-body templates with `bodies` are valid only when the generated mesh target is unambiguous: if `len(bodies) == 1`, target `bodies[0].vessel`; if `len(bodies) > 1`, require `--body-index` and apply mesh/name/inertia overrides only to `bodies[body_index].vessel` before `DiffractionSpec.model_validate()`. Never add top-level `vessel` to a template that already has `bodies`, because `DiffractionSpec` rejects both. Partial templates are valid only as raw fragments because validation occurs after merge. Merge precedence is: explicit CLI flags override template values; template values override command defaults; effective defaults fill only missing fields after template/explicit CLI merge. Mesh path and inferred mesh format always override template geometry mesh fields because they are the point of the command; mesh units and symmetry override template fields only when explicitly supplied, otherwise template values are preserved and defaults fill missing values. The flat flag to schema map is fixed for single-vessel/no-template output: `--name -> vessel.name`, `--mass -> vessel.inertia.mass`, `--cog -> vessel.inertia.centre_of_gravity`, `--rog -> vessel.inertia.radii_of_gyration`, `--inertia -> vessel.inertia.inertia_tensor` with dict keys `Ixx`, `Iyy`, `Izz`, `Ixy`, `Ixz`, `Iyz` in that CLI order, `--water-depth -> environment.water_depth`, `--frequencies -> frequencies.values`, `--frequency-range -> frequencies.range.{start,end,count}`, `--frequency-input-type -> frequencies.input_type`, `--frequency-distribution -> frequencies.range.distribution`, `--headings -> wave_headings.values`, `--heading-range -> wave_headings.range.{start,end,increment}`, explicit `--mesh-format -> vessel.geometry.mesh_format`, explicit `--mesh-units -> vessel.geometry.length_units`, explicit `--symmetry -> vessel.geometry.symmetry`, and the positional mesh path -> `vessel.geometry.mesh_file`. In multi-body templates, the vessel-scoped paths are projected under `bodies[body_index].vessel` before validation instead. Merge is a recursive dict merge after projecting flat flags into this nested skeleton; list/vector flags replace the target field rather than merging elementwise. CLI `--frequencies` clears any template `frequencies.range`; CLI `--frequency-range` clears any template `frequencies.values`; heading list/range flags apply the same mutual-exclusion clearing before `DiffractionSpec.model_validate()`.
6. Generate canonical `DiffractionSpec` YAML using the existing `DiffractionSpec.to_yaml(path)` helper; no ad hoc string assembly. Store `VesselGeometry.mesh_file` as a path relative to the generated spec file's parent whenever possible, using #500's spec-relative mesh path convention; if the user supplies an absolute path, convert it to `os.path.relpath(mesh_path.resolve(), output_spec.parent)` before writing unless the path is on a different Windows drive, in which case fail with a clear message requiring the mesh to be under a reachable path or use a future packaging workflow. This keeps generated specs portable and still lets `OrcaWaveRunner.run(..., spec_path=generated_spec_path)` resolve/copy the mesh from the correct location. Use `length_units=mesh_units`, not a non-existent `units` field. `--mesh-format auto` is resolved to a concrete `MeshFormatType` before writing; the generated spec must not preserve `auto` for the given mesh.
7. Validate authoring inputs before writing and validate the generated YAML after writing. Before schema construction, require the supplied mesh path to exist and be a file; this is a command input check only and must not grow into package resolution/copy policy. Reject nonpositive explicit frequency/period values, nonpositive frequency-range endpoints/count, invalid logarithmic ranges, and nonpositive heading increments before YAML is written. Construct `DiffractionSpec` from the merged raw dictionary before serialization; Pydantic/schema construction failures are pre-write failures, print actionable messages, and leave no generated YAML regardless of `--keep-invalid`. After writing canonical YAML, instantiate `SpecConverter(generated_path)` inside a try/except and then run `.validate()`; unexpected post-write schema-load exceptions or returned validation issues both make the command fail, print actionable messages, and leave the generated file only when `--keep-invalid` is present. Otherwise remove it. `validate-spec` passing remains schema validation, not a physical-quality guarantee.
8. For dry-run, call the existing Python API equivalent of `run-orcawave <generated-spec> --dry-run`: load the generated spec, construct `OrcaWaveRunner(RunConfig(output_dir=..., dry_run=True))`, and call `runner.run(spec, spec_path=generated_spec_path)`. #607's dry-run output must include OrcaWave YAML and, when `copy_mesh_files=True`, the runner-visible mesh copied into `output_dir`; the test must assert the copied mesh exists rather than only observing `RunStatus.DRY_RUN`. This is still not the #605 self-contained package contract because it lacks the package manifest, collision policy, strict auxiliary preflight, and deterministic packaged YAML rewrite. #607 only writes schema-supported mesh format values; #606/#610 own conversion and licensed acceptance for non-GDF/non-DAT formats.
9. Add help text that states mesh-only inputs are insufficient for physics-ready diffraction, names the non-inferable inputs, and states that CLI frequency flags default to rad/s unless `--frequency-input-type period` is set. The help must explicitly say Hz values are not accepted directly in #607 and must be converted to rad/s before use.
10. Update docs only after #614 has landed or in the same PR by consuming #614's stale-doc cleanup and docs guard. #607 must not add a domain given-mesh quickstart beside stale current/recommended OrcaWave workflow docs. If #614 is not landed and not consumed, limit #607 documentation to command help/docstrings and leave the domain quickstart to #614/follow-up. When domain docs are updated, fix or remove the broken `docs/domains/orcawave/diffraction/QUICK_START.md` link in `docs/domains/orcawave/README.md` and ensure stale current workflow references listed in #614 are absent or labeled historical/reference.
11. Add tests with `CliRunner` and schema reload validation in a standalone `tests/hydrodynamics/diffraction/test_mesh_to_spec_cli.py` because this workflow is multi-step and would make the existing integration test file harder to scan.

## Pseudocode

```text
mesh_to_spec(mesh_path, options, template_path):
  if not mesh_path.exists() or not mesh_path.is_file(): fail before writing YAML
  template_dict = yaml.safe_load(template_path) if provided else {}
  flag_dict = project flat CLI flags to nested DiffractionSpec schema paths
  validate positive frequencies/periods, positive frequency ranges, positive heading increments, and logarithmic range constraints
  if CLI frequency values supplied: remove any template frequencies.range before merge
  if CLI frequency range supplied: remove any template frequencies.values before merge
  if CLI heading values supplied: remove any template wave_headings.range before merge
  if CLI heading range supplied: remove any template wave_headings.values before merge
  target body if template has bodies; project vessel-scoped CLI overrides under bodies[body_index].vessel before validation
  physical_inputs = recursive merge(template_dict, explicit flag_dict, then fill missing defaults) with explicit flags highest precedence
  require mass, cog, inertia/rog, water_depth, frequencies, headings, mesh_units, symmetry
  resolve mesh_path to a spec-relative path before writing the generated spec
  infer mesh_format from explicit flag or suffix map
  translate symmetry "both" alias to schema value "xz+yz"
  if --frequencies supplied: build FrequencySpec(input_type=frequency_input_type, values=[...])
  else: build FrequencySpec(input_type=frequency_input_type, range=FrequencyRangeSpec(start,end,count,distribution=linear by default))
  build HeadingRangeSpec(start,end,increment) for heading ranges
  geometry = build VesselGeometry(mesh_file=spec_relative_mesh_path, mesh_format=mesh_format, length_units=mesh_units, symmetry=symmetry)
  inertia = build VesselInertia(mass=mass, centre_of_gravity=cog, radii_of_gyration=rog) or VesselInertia(..., inertia_tensor={"Ixx": ixx, "Iyy": iyy, "Izz": izz, "Ixy": ixy, "Ixz": ixz, "Iyz": iyz})
  try: spec = DiffractionSpec.model_validate(physical_inputs)
  except ValidationError as exc: fail command before writing YAML; --keep-invalid does not create a file
  write canonical YAML via DiffractionSpec.to_yaml(path)
  try: issues = SpecConverter(generated_path).validate()
  except Exception as exc: fail command with unexpected post-write schema-load error and remove generated path unless --keep-invalid
  if issues: fail command and remove invalid generated path unless --keep-invalid
  return generated path

dry_run_package(generated_spec_path, output_dir):
  spec = DiffractionSpec.from_yaml(generated_spec_path)
  runner = OrcaWaveRunner(RunConfig(output_dir=output_dir, dry_run=True))
  runner.run(spec, spec_path=generated_spec_path)
  assert result.status == DRY_RUN and package files exist on disk

run_orcawave_from_mesh(mesh_path, options):
  generated_spec_path = mesh_to_spec(mesh_path, options, template_path)
  return dry_run_package(generated_spec_path, output_dir)
```

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-607-orcawave-given-mesh-cli.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-607-claude.md` |
| Plan review - Codex (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-607-codex.md` |
| Plan review - Gemini (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-607-gemini.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-607-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `src/digitalmodel/hydrodynamics/diffraction/cli.py` | add commands/help |
| Create | `src/digitalmodel/hydrodynamics/diffraction/mesh_to_spec.py` | spec construction and template merge logic |
| Create | `tests/hydrodynamics/diffraction/test_mesh_to_spec_cli.py` | CLI/schema/dry-run tests using Click `CliRunner` |
| Modify conditionally | `docs/domains/orcawave/README.md` | only if #614 is landed or consumed in the same PR; remove/fix stale canonical-doc issues before adding "Given Mesh Workflow" quickstart |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_mesh_to_spec_writes_valid_diffraction_spec` | generated YAML loads | mesh + required numeric options, omitting vessel name | `DiffractionSpec.from_yaml()` succeeds and vessel name defaults to `mesh.stem` |
| `test_mesh_to_spec_missing_mass_fails` | no fake inferred mass | mesh only | Click error naming required inputs |
| `test_mesh_to_spec_missing_mesh_file_fails_before_write` | command does not author specs for absent meshes | nonexistent mesh path plus otherwise valid flags | Click error naming the missing mesh and no generated spec |
| `test_mesh_to_spec_uses_raw_partial_template_and_flag_precedence` | template merge works | mesh + partial raw template + overriding heading flag | output preserves template frequencies and flag headings after post-merge schema validation |
| `test_mesh_to_spec_flag_shapes_parse_vectors_and_grids` | concrete CLI schema works | COG/ROG vectors, frequency range with default linear distribution, and heading range with increment | generated spec has expected `FrequencyRangeSpec` and `HeadingRangeSpec` values |
| `test_mesh_to_spec_inertia_tensor_projects_to_schema_keys` | six CLI inertia values map deterministically | `--inertia 1,2,3,4,5,6` | generated spec has `inertia_tensor={"Ixx":1,"Iyy":2,"Izz":3,"Ixy":4,"Ixz":5,"Iyz":6}` |
| `test_mesh_to_spec_frequency_input_type_period` | frequency units are explicit | `--frequency-range` with `--frequency-input-type period` | generated spec stores `input_type=period`; default test stores `input_type=frequency` |
| `test_mesh_to_spec_rejects_nonpositive_frequency_values` | CLI does not rely on weak schema validation | `--frequencies -1,0` and period variant | Click error before YAML is written |
| `test_mesh_to_spec_explicit_frequencies_use_values_branch` | list and range branches are exclusive | `--frequencies 0.4,0.8` | generated spec has `frequencies.values` set and no `frequencies.range` |
| `test_mesh_to_spec_cli_frequencies_clear_template_range` | template/CLI mutual exclusion is deterministic | template with `frequencies.range` plus CLI `--frequencies 0.4,0.8` | output has `frequencies.values` and no `frequencies.range`; inverse range-over-values case is also covered |
| `test_mesh_to_spec_logarithmic_distribution_projection` | non-default distribution is not lost | `--frequency-range 0.4,1.0,5 --frequency-distribution logarithmic` | generated spec stores `FrequencyDistribution.LOGARITHMIC` |
| `test_mesh_to_spec_template_deep_merge_flag_path_map` | flat flags project to nested schema deterministically | complete template plus overriding mass, water depth, and heading flags | output applies only mapped overrides and preserves unrelated nested template values |
| `test_mesh_to_spec_template_inertia_conflict_after_merge_errors` | post-merge inertia rule is enforced across template/CLI | template contains `radii_of_gyration` and CLI provides `--inertia`, plus inverse case with template `inertia_tensor` and CLI `--rog` | Click error before YAML is written naming the conflicting sources |
| `test_mesh_to_spec_multibody_template_requires_body_index` | valid multi-body templates are not made invalid by adding top-level vessel | complete template with two `bodies` and no `--body-index` | Click error before YAML is written naming `--body-index` |
| `test_mesh_to_spec_multibody_template_overrides_selected_body_only` | body targeting is deterministic | complete template with two `bodies` and `--body-index 1` | output keeps top-level `vessel` unset and overrides only `bodies[1].vessel.geometry.mesh_file` and vessel-scoped flags |
| `test_mesh_to_spec_rejects_inertia_and_rog_together` | physical inertia inputs are unambiguous | CLI provides both `--rog` and `--inertia` | Click error before YAML is written |
| `test_mesh_to_spec_rejects_missing_inertia_source` | physical inertia inputs are required after template merge | no `--rog`, no `--inertia`, and no template inertia | Click error naming the missing inertia source |
| `test_mesh_to_spec_maps_mesh_format_and_symmetry_aliases` | CLI values match schema enums | `.GDF` mesh, `--mesh-format auto`, `--symmetry both`, omit `--mesh-units` | generated spec uses `mesh_format=gdf`, `symmetry=xz+yz`, and default `length_units=m` |
| `test_mesh_to_spec_template_units_preserved_when_mesh_units_omitted` | command defaults do not overwrite template units | complete template with `vessel.geometry.length_units="ft"` and no `--mesh-units` | generated spec preserves `length_units=ft`; explicit `--mesh-units m` overrides it |
| `test_mesh_to_spec_relative_mesh_path_dry_run_resolves_and_copies` | dry-run path resolution is deterministic | relative mesh path and spec output in a different directory | generated spec stores a path relative to the generated spec, runner dry-run finds the source mesh through `spec_path.parent`, and copied mesh exists under the dry-run `output_dir` |
| `test_mesh_to_spec_absolute_path_normalized_to_relative` | absolute paths are projected without machine-specific YAML | absolute mesh path reachable from generated spec parent | output stores a relative mesh path and dry-run copy still finds the source |
| `test_mesh_to_spec_generated_file_passes_validate_spec_cli` | user-facing validation command is part of the workflow | generated spec path | `CliRunner.invoke(cli, ["validate-spec", generated.yml])` exits 0; installed `diffraction validate-spec generated.yml` coverage remains a subprocess integration-test pattern, not a `CliRunner` argument pattern |
| `test_mesh_to_spec_obj_msh_are_schema_dry_run_only` | non-GDF/non-DAT support is not overstated | `.obj` or `.msh` mesh with `--mesh-format auto` | spec/YAML dry-run is allowed and help/output states licensed acceptance is owned by #606/#610 |
| `test_mesh_to_spec_validate_issues_fail_command` | `SpecConverter.validate()` return value is honored | generated spec that reloads but validation returns issues | nonzero exit, issues printed, invalid generated file removed unless keep-invalid is explicitly enabled |
| `test_mesh_to_spec_schema_validation_error_writes_no_file` | invalid raw merge cannot be serialized with `DiffractionSpec.to_yaml()` | template/flag merge that causes Pydantic validation to raise | nonzero exit before YAML is written; no generated file exists even with `--keep-invalid` |
| `test_mesh_to_spec_post_write_converter_error_removes_or_keeps_generated_file` | `--keep-invalid` applies only after canonical YAML exists | monkeypatched `SpecConverter(generated_path).validate()` or constructor error after `DiffractionSpec.to_yaml()` succeeds | nonzero exit; generated file removed by default and retained with `--keep-invalid` |
| `test_mesh_to_spec_help_names_physical_limits_and_units` | help text requirements are testable | `mesh-to-spec --help` and `run-orcawave-from-mesh --help` | help names non-inferable inputs, validation-vs-physics warning, and frequency units |
| `test_mesh_to_spec_then_existing_run_orcawave_dry_run` | workflow creates dry-run input, not just method call | minimal valid generated spec with ready mesh | dry-run output dir contains OrcaWave YAML and `RunStatus.DRY_RUN`; generated spec exists at `--output`; self-contained package contract remains #605 |
| `test_run_orcawave_from_mesh_thin_wrapper` | issue-body command shape does not duplicate logic | ready mesh plus physical flags, `--output-dir`, and optional `--spec-output` | wrapper writes the generated spec, invokes the same runner path, and produces `RunStatus.DRY_RUN`; `--output` is not accepted on the wrapper |

## Acceptance Criteria

- [ ] CLI can generate a valid `DiffractionSpec` from mesh plus required physical inputs, with `--name` optional and defaulting to the mesh stem.
- [ ] Frequency and heading grids are supported from both direct flags and raw partial or complete templates; frequency ranges default to linear distribution and heading ranges use schema `increment`.
- [ ] Frequency CLI flags have explicit `frequency|period` input-type semantics; default is documented as rad/s and period inputs require `--frequency-input-type period`.
- [ ] Explicit frequency/period values and ranges are validated as positive before YAML is written; Hz is not accepted directly and help text says to convert Hz to rad/s.
- [ ] Template format is raw YAML that may be a partial fragment or a complete `DiffractionSpec`; it is validated only after CLI/template merge, with documented flag-over-template precedence.
- [ ] Defaulted CLI values fill missing fields only; omitted `--mesh-units` preserves template `length_units`, while explicit `--mesh-units` overrides it.
- [ ] CLI list/range overrides clear mutually exclusive template branches for `frequencies` and `wave_headings` before schema validation.
- [ ] Complete multi-body templates are supported only with explicit body targeting when ambiguous; generated YAML never sets both top-level `vessel` and `bodies`.
- [ ] Multi-body template overrides are projected under the selected `bodies[body_index].vessel` before `DiffractionSpec.model_validate()`.
- [ ] CLI flag projection uses actual schema field names: `centre_of_gravity`, `radii_of_gyration`, `inertia_tensor` keys `Ixx/Iyy/Izz/Ixy/Ixz/Iyz`, `frequencies`, `wave_headings`, and `solver_options`.
- [ ] Generated spec passes `validate-spec`; non-empty `SpecConverter.validate()` issues fail the command, and help text states this is schema validation rather than a physical-quality guarantee.
- [ ] Missing mesh paths and Pydantic/schema construction errors fail before spec writing; no file is created for those failures, even with `--keep-invalid`.
- [ ] Post-write converter validation or unexpected schema-load errors remove the generated YAML by default and retain it only when `--keep-invalid` is passed.
- [ ] Workflow can create dry-run OrcaWave input without a licensed host through the existing `run-orcawave --dry-run` path using `OrcaWaveRunner(RunConfig(dry_run=True)).run(spec, spec_path=generated_spec_path)`.
- [ ] Relative user mesh paths are normalized in generated specs so dry-run mesh copy resolves correctly when spec output and dry-run output are in different directories.
- [ ] Generated specs follow #500's spec-relative path convention rather than baking machine-specific absolute paths.
- [ ] `run-orcawave-from-mesh` is included as a dry-run-only thin wrapper around `mesh-to-spec` plus the existing runner, exposes `--spec-output` instead of `--output`, uses a deterministic `<output-dir>/<mesh.stem>.diffraction.yml` default when `--spec-output` is omitted, and introduces no separate mesh resolver/converter.
- [ ] Help text names non-inferable inputs clearly.
- [ ] `--mesh-units` defaults to `m`, `--keep-invalid` is part of the documented CLI surface with defined retention behavior, and the dry-run test asserts the runner-copied mesh exists in `output_dir`.
- [ ] Post-merge inertia conflicts are tested for both template-plus-CLI directions, and the generated spec is validated through the public `validate-spec` Click command.
- [ ] `.stl`, `.msh`, and `.obj` are documented as schema/YAML dry-run inputs only in #607; preparation and licensed acceptance remain #606/#610 scope.
- [ ] Domain documentation changes either consume landed #614 cleanup/docs guards or are deferred to #614/follow-up; #607 does not add a given-mesh quickstart alongside stale current OrcaWave workflow docs.
- [ ] Targeted tests pass with `PYTHONPATH=src uv run python -m pytest tests/hydrodynamics/diffraction/test_mesh_to_spec_cli.py`, and no related CLI regressions are introduced in `tests/hydrodynamics/diffraction/test_cli_integration.py`.
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

- **Risk:** #605/#606 must provide self-contained packaging and mesh-loading/preparation policy beyond ready-mesh dry runs. #607 should clearly document that full packaging/conversion remains planned until those dependencies land.
- **Risk:** #607 dry-run can create OrcaWave input and runner-copied ready meshes, but it is not the #605 package contract: no manifest-owned replacement, basename collision policy, auxiliary strict preflight, or prepared filename rewrite is promised here.
- **Risk:** All physical inputs are user-supplied free parameters or template values; #607 must not inject standards-derived defaults, so no calc-citation metadata is required unless a future template source adds standards-derived constants.
- **Risk:** Template provenance for standards-derived values is not solved by #607. If templates become curated engineering sources rather than user-supplied fragments, a follow-up provenance/calc-citation issue is required.
- **Risk:** The legacy `diffraction_cli.py` argparse entry point will not receive this command in #607; docs must explicitly steer users to the Click CLI or a follow-up parity/deprecation issue.
- **Open:** This T2 plan would require at least two providers under the minimum review policy; this batch intentionally uses the stricter three-provider set. Claude, Codex, and Gemini are the selected review providers.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
