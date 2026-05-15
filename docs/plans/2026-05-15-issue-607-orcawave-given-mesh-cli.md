# Plan for #607: OrcaWave: add given-mesh CLI workflow for diffraction setup

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/607
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-607-claude.md | scripts/review/results/2026-05-15-plan-607-codex.md

---

## Scope

CLI/spec authoring scope. It must not infer mass properties that require engineering input unless an explicit template supplies them. The `mesh-to-spec` command may land as a schema-writing workflow, but `run-orcawave-from-mesh` must depend on the #605 package path and #606 mesh preparation policy; #607 must not introduce a second mesh resolver or converter.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#607` - OPEN - `OrcaWave: add given-mesh CLI workflow for diffraction setup`; label: `enhancement`.
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

- `cli.py` has a broader Click surface than the initial four-command summary: existing commands include `convert-spec`, `validate-spec`, `run-orcawave`, `batch-orcawave`, and the adjacent `mesh-build` geometry-to-mesh command. #607 must follow existing Click conventions and keep the semantic split clear: `mesh-build` is geometry spec -> panel mesh; `mesh-to-spec` is existing mesh -> diffraction spec.
- `diffraction_cli.py` is a legacy/parallel argparse entry point. #607 is scoped to the canonical Click CLI in `cli.py`; docs must say the new command is Click-only unless a separate deprecation/parity issue is approved.
- `input_schemas.py` defines required `DiffractionSpec`, `VesselGeometry`, `VesselInertia`, frequency, heading, and environment structures. `mesh_pipeline.py` can load/validate meshes but does not create a `DiffractionSpec`.
- #606 is a sequencing dependency for mesh loading/preparation policy. #607 may implement only a thin spec-authoring layer if #606 has not landed; any mesh loading must be delegated to the #606 preparer once available rather than creating a second mesh pipeline.

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

1. Design command shape consistent with existing Click CLI conventions, especially `mesh-build`: implement `mesh-to-spec` and a thin `run-orcawave-from-mesh` dry-run wrapper because the acceptance test requires the wrapper path. Keep `diffraction_cli.py` unchanged in #607 and document Click CLI as canonical.
2. Require or template vessel name, mass, COG, ROG/inertia, water depth, frequency grid, heading grid, mesh units, and symmetry; default vessel name may be the mesh stem but must be explicit in generated YAML.
3. Generate canonical `DiffractionSpec` YAML using existing schema classes rather than ad hoc string assembly; `validate-spec` passing is necessary but not sufficient for physical quality, so command help must identify non-physical placeholder risk.
4. Add wrapper behavior that calls the configured `OrcaWaveRunner.run(...)` path in dry-run mode using the generated spec path, with licensed solve still requiring explicit `run-orcawave` or an approved flag policy. If #605/#606 have not landed, the wrapper must be blocked/deferred rather than implementing duplicate packaging or mesh-preparation logic.
5. Add help text that states mesh-only inputs are insufficient for physics-ready diffraction.
6. Add tests with `CliRunner` and schema reload validation, extending existing CLI test patterns in `tests/hydrodynamics/diffraction/test_cli_integration.py` or documenting why a standalone test file is cleaner.

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-607-orcawave-given-mesh-cli.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-607-claude.md` |
| Plan review - Codex (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-607-codex.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-607-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `src/digitalmodel/hydrodynamics/diffraction/cli.py` | add commands/help |
| Create/modify | `src/digitalmodel/hydrodynamics/diffraction/mesh_to_spec.py` | spec construction logic |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` | only if helper serialization gaps appear |
| Create/modify | `tests/hydrodynamics/diffraction/test_mesh_to_spec_cli.py` or `tests/hydrodynamics/diffraction/test_cli_integration.py` | CLI/schema tests using existing Click test conventions |
| Modify | `docs/domains/orcawave/README.md` | quickstart once command exists |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_mesh_to_spec_writes_valid_diffraction_spec` | generated YAML loads | mesh + required numeric options including vessel name | `DiffractionSpec.from_yaml()` succeeds |
| `test_mesh_to_spec_missing_mass_fails` | no fake inferred mass | mesh only | Click error naming required inputs |
| `test_mesh_to_spec_uses_template_grid` | template merge works | mesh + template | output preserves frequency/headings |
| `test_run_orcawave_from_mesh_dry_run_packages` | wrapper calls `OrcaWaveRunner.run(..., dry_run=True)` path | minimal valid inputs | dry-run output dir contains generated spec, OrcaWave YAML, and packaged mesh refs from the runner/package path |

## Acceptance Criteria

- [ ] CLI can generate a valid `DiffractionSpec` from mesh plus required physical inputs, including vessel name or documented mesh-stem default.
- [ ] Frequency and heading grids can come from flags or a template.
- [ ] Generated spec passes `validate-spec`, and help text states this is schema validation rather than a physical-quality guarantee.
- [ ] Workflow can create a dry-run package without a licensed host through `OrcaWaveRunner.run(..., dry_run=True)` or the equivalent configured runner object.
- [ ] Help text names non-inferable inputs clearly.
## Plan Review Gating

- [ ] Completed review artifacts under `/mnt/local-analysis/workspace-hub/digitalmodel/scripts/review/results/` exist for at least two providers and each non-empty artifact contains a `## Verdict` section; 0-byte in-progress files do not satisfy this gate.
- [ ] Any provider `MAJOR` finding requires a plan revision and re-review; the issue is commented with this plan and moved to `status:plan-review` only after no unresolved `MAJOR` findings remain.

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | PENDING | Awaiting review artifact |
| Codex | PENDING | Awaiting review artifact |

**Overall result:** PENDING - do not label `status:plan-review` until artifacts exist and no unresolved `MAJOR` findings remain.

## Risks and Open Questions

- **Risk:** #605/#606 must provide packaging and mesh-loading/preparation policy for the dry-run wrapper. #607 should not create a competing mesh pipeline; the wrapper is blocked until those dependencies are available or explicitly approved for sequencing in the issue thread.
- **Risk:** The legacy `diffraction_cli.py` argparse entry point will not receive this command in #607; docs must explicitly steer users to the Click CLI or a follow-up parity/deprecation issue.
- **Open:** Gemini was unavailable in this environment; use Claude + Codex as the required two-provider review set for plan-review.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
