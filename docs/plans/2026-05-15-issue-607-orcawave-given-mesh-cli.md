# Plan for #607: OrcaWave: add given-mesh CLI workflow for diffraction setup

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/607
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-607-claude.md | scripts/review/results/2026-05-15-plan-607-codex.md | scripts/review/results/2026-05-15-plan-607-gemini.md

---

## Scope

CLI/spec authoring scope. It must not infer mass properties that require engineering input unless an explicit template supplies them. #607 can implement `mesh-to-spec` and dry-run with already-ready mesh formats through the existing `run-orcawave --dry-run` path at HEAD. #605/#606 are dependencies only for future self-contained packaging and non-ready mesh preparation; #607 must not introduce a redundant `run-orcawave-from-mesh` command, second mesh resolver, or second converter.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#607` - OPEN - `OrcaWave: add given-mesh CLI workflow for diffraction setup`; label: `enhancement`.
- `#500` - OPEN - `OrcaWave: mesh file pre-flight validation + auto-copy in runner`; label includes `status:plan-approved`.

### Sources consulted

- `AGENTS.md` - digitalmodel declares `PYTHONPATH=src uv run python -m pytest` as the repository test command and points source ownership at `src/digitalmodel/`.
- `/mnt/local-analysis/workspace-hub/docs/plans/_template-issue-plan.md` - T2/T3 plans require pseudocode/design checkpoint, verification commands, and explicit test/no-regression acceptance.
- GitHub issue `#607` body - command design is intentionally flexible; it requires a given-mesh workflow and dry-run package outcome, not a mandatory `run-orcawave-from-mesh` command name.
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
- Current `run-orcawave --dry-run` can generate input through `OrcaWaveRunner(RunConfig(dry_run=True)).run(spec, spec_path=...)`; #607 should use that for ready-mesh dry-runs. #605/#606 remain dependencies for self-contained packaging and mesh conversion.
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
```

Reproduction proofs: N/A - this is a future-work enhancement issue, not an alleged runtime regression or failing-test report. Implementation must still start with the TDD tests below.

## Deliverable

A user-facing CLI workflow can create and optionally dry-run an OrcaWave diffraction spec/package starting from a mesh file plus required physical inputs.

## Proposed Tasks

1. Design command shape consistent with existing Click CLI conventions, especially `mesh-build`: implement `mesh-to-spec` for existing mesh -> `DiffractionSpec` YAML. Keep `diffraction_cli.py` unchanged in #607 and document Click CLI as canonical. The issue body lists `run-orcawave-from-mesh` as a possible shape, but this plan rejects it as redundant because `run-orcawave --dry-run` already exists; reopen that decision only with maintainer direction.
2. Define concrete CLI flags: `--name`, `--mass`, `--cog x,y,z`, `--rog x,y,z` or `--inertia ixx,iyy,izz,ixy,ixz,iyz`, `--water-depth`, `--frequencies f1,f2,...` or `--frequency-range min,max,count`, `--headings h1,h2,...` or `--heading-range min,max,count`, `--mesh-units`, `--symmetry none|xz|yz|both`, `--template`, and `--output`.
3. Define template format as YAML loaded to a raw dictionary first, then merged with CLI flags and validated as a complete `DiffractionSpec` after merge. Complete `DiffractionSpec` templates are valid; partial templates are allowed only because validation occurs after merge. Merge precedence is: explicit CLI flags override template values; template values override command defaults; mesh path/mesh format/mesh units from the given mesh override any template geometry mesh path unless an explicit `--allow-template-mesh` flag is approved.
4. Generate canonical `DiffractionSpec` YAML using the existing `DiffractionSpec.to_yaml(path)` helper; no ad hoc string assembly. `validate-spec` passing is necessary but not sufficient for physical quality, so command help must identify non-physical placeholder risk.
5. For dry-run, call the existing Python API equivalent of `run-orcawave <generated-spec> --dry-run`: load the generated spec, construct `OrcaWaveRunner(RunConfig(output_dir=..., dry_run=True))`, and call `runner.run(spec, spec_path=generated_spec_path)`. Do not add a new wrapper command name unless a later issue approves it.
6. Add help text that states mesh-only inputs are insufficient for physics-ready diffraction.
7. Add tests with `CliRunner` and schema reload validation in a standalone `tests/hydrodynamics/diffraction/test_mesh_to_spec_cli.py` because this workflow is multi-step and would make the existing integration test file harder to scan.

## Pseudocode

```text
mesh_to_spec(mesh_path, options, template_path):
  template_dict = yaml.safe_load(template_path) if provided else {}
  physical_inputs = merge_raw_template_with_flags(template_dict, CLI flags)
  require mass, cog, inertia/rog, water_depth, frequencies, headings, mesh_units, symmetry
  geometry = build VesselGeometry(mesh_file=mesh_path, mesh_format=extension, units=mesh_units)
  spec = build DiffractionSpec(vessel/body/environment/frequency/heading/solver)
  write canonical YAML via DiffractionSpec.to_yaml(path)
  run SpecConverter(generated_path).validate() and return generated path

dry_run_package(generated_spec_path, output_dir):
  invoke existing run-orcawave dry-run Python path, not a new wrapper command
  runner.run(spec, spec_path=generated_spec_path)
  assert result.status == DRY_RUN and package files exist on disk
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
| Create/modify | `src/digitalmodel/hydrodynamics/diffraction/mesh_to_spec.py` | spec construction and template merge logic |
| Create | `tests/hydrodynamics/diffraction/test_mesh_to_spec_cli.py` | CLI/schema/dry-run tests using Click `CliRunner` |
| Modify | `docs/domains/orcawave/README.md` | add "Given Mesh Workflow" quickstart with `mesh-to-spec` then `run-orcawave --dry-run` |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_mesh_to_spec_writes_valid_diffraction_spec` | generated YAML loads | mesh + required numeric options including vessel name | `DiffractionSpec.from_yaml()` succeeds |
| `test_mesh_to_spec_missing_mass_fails` | no fake inferred mass | mesh only | Click error naming required inputs |
| `test_mesh_to_spec_uses_template_grid_and_flag_precedence` | template merge works | mesh + partial `DiffractionSpec` template + overriding heading flag | output preserves template frequencies and flag headings |
| `test_mesh_to_spec_flag_shapes_parse_vectors_and_grids` | concrete CLI schema works | COG/ROG vectors and frequency/heading ranges | generated spec has expected numeric arrays |
| `test_mesh_to_spec_then_existing_run_orcawave_dry_run` | workflow creates dry-run input, not just method call | minimal valid generated spec with ready mesh | dry-run output dir contains generated spec, OrcaWave YAML, and `RunStatus.DRY_RUN`; self-contained packaging remains #605 |

## Acceptance Criteria

- [ ] CLI can generate a valid `DiffractionSpec` from mesh plus required physical inputs, including vessel name or documented mesh-stem default.
- [ ] Frequency and heading grids are supported from both direct flags and a schema-loaded template.
- [ ] Template format is a schema-loaded `DiffractionSpec` YAML with documented flag-over-template precedence.
- [ ] Generated spec passes `validate-spec`, and help text states this is schema validation rather than a physical-quality guarantee.
- [ ] Workflow can create dry-run OrcaWave input without a licensed host through the existing `run-orcawave --dry-run` path using `OrcaWaveRunner(RunConfig(dry_run=True)).run(spec, spec_path=generated_spec_path)`.
- [ ] Help text names non-inferable inputs clearly.
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
- **Risk:** All physical inputs are user-supplied free parameters or template values; #607 must not inject standards-derived defaults, so no calc-citation metadata is required unless a future template source adds standards-derived constants.
- **Risk:** The legacy `diffraction_cli.py` argparse entry point will not receive this command in #607; docs must explicitly steer users to the Click CLI or a follow-up parity/deprecation issue.
- **Open:** Default review policy uses three-provider review; Claude, Codex, and Gemini are the selected review set for this plan.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
