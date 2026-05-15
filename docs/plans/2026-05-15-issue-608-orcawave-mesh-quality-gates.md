# Plan for #608: OrcaWave: add mesh quality gates before diffraction solve

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/608
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-608-claude.md | scripts/review/results/2026-05-15-plan-608-codex.md

---

## Scope

Quality gate scope only. Mesh conversion policy belongs to #606; package layout belongs to #605. #608 should consume resolved/prepared asset paths from the #605/#606 flow instead of doing independent path resolution.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#608` - OPEN - `OrcaWave: add mesh quality gates before diffraction solve`; label: `enhancement`.
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

- `MeshPipeline.validate()` delegates to the BEMRosetta handler for a `MeshQualityReport`.
- `MeshQualityReport` already exposes `n_vertices`, `n_panels`, panel-area stats, `aspect_ratio_max`, `aspect_ratio_mean`, `skewness_max`, degenerate-panel count/flag, normal consistency flags, `quality_score`, warnings, and errors.
- `SpecConverter.validate()` does not load mesh files or inspect mesh quality.
- `OrcaWaveRunner._validate_mesh_references()` only warns about missing packaged mesh files in output, not geometry quality.
- BEMRosetta mesh handlers live under `src/digitalmodel/hydrodynamics/bemrosetta/mesh/`.

### Gaps identified

- No OrcaWave-specific severity model maps mesh metrics to blocking errors versus warnings.
- No CLI report exists for mesh QA during `validate-spec` or preflight.
- No fixtures cover poor-quality meshes in the OrcaWave workflow.

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

OrcaWave validation/preflight produces blocking mesh errors and non-blocking quality warnings before conversion or solve.

## Proposed Tasks

1. Inventory available `MeshQualityReport` fields and BEMRosetta validation behavior, then define an `OrcaWaveMeshQAPolicy` with configurable thresholds rather than hardcoding engineering cutoffs inside the runner.
2. Define severity mapping for missing files, unsupported format, zero panels, degenerate panels, extreme aspect ratio/skew, inconsistent normals, units/symmetry metadata, and any waterline checks that can be defended from available mesh/spec fields. Do not invent waterline physics checks if the needed datum is absent.
3. Add a QA runner that consumes resolved/prepared asset paths from #605/#606 when available, loads meshes with `MeshPipeline`, and emits structured findings with `asset_type`, source path, metric, severity, threshold, and recommendation.
4. Add a dedicated `diffraction orcawave-preflight` command and an opt-in `validate-spec --mesh-qa` integration; keep hard-fail policy explicit for package/solve preflight.
5. Emit machine-readable JSON metadata and concise CLI output.
6. Add valid and invalid mesh fixtures/tests, including a fixture that exercises BEMRosetta error handling without needing a licensed OrcaWave host.

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-608-orcawave-mesh-quality-gates.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-608-claude.md` |
| Plan review - Codex (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-608-codex.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-608-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/hydrodynamics/diffraction/orcawave_mesh_quality.py` | QA policy and report model |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/spec_converter.py` | call QA from validation/preflight |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/cli.py` | expose output |
| Create/modify | `tests/hydrodynamics/diffraction/test_orcawave_mesh_quality.py` | TDD coverage |
| Create/modify | `tests/hydrodynamics/diffraction/fixtures/meshes/` | valid/bad fixtures |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_orcawave_mesh_qa_valid_mesh_passes` | nominal mesh accepted | known valid GDF | no blocking findings |
| `test_orcawave_mesh_qa_missing_file_blocks` | missing asset hard fails | absent mesh path | blocking finding |
| `test_orcawave_mesh_qa_degenerate_panels_warn_or_block` | geometry metric mapped | bad fixture | severity per policy |
| `test_orcawave_mesh_qa_policy_overrides_thresholds` | thresholds configurable | strict/relaxed policy | same report metrics map to expected severity |
| `test_validate_spec_mesh_qa_reports_findings` | CLI integration | spec + bad mesh and `--mesh-qa` | human-readable issue list and nonzero only for blocking findings |
| `test_orcawave_preflight_writes_json_report` | machine-readable output | spec + valid mesh | JSON report contains asset type, metric, severity, and path |

## Acceptance Criteria

- [ ] Mesh QA runs before solve/package generation through a documented command path.
- [ ] Blocking geometry/file errors prevent misleading success output.
- [ ] Non-blocking warnings are reported distinctly from hard failures.
- [ ] QA report is available in machine-readable and human-readable forms.
- [ ] Tests include valid and invalid/poor-quality meshes.
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

- **Risk:** QA must run on the same resolved/prepared asset paths that package/solve will use. Duplicating path resolution outside #605/#606 would let QA pass a different file than OrcaWave receives.
- **Risk:** Licensed OrcaWave/OrcFxAPI behavior cannot be fully verified on Linux; tests requiring a license must skip cleanly and be proven on the licensed host where applicable.
- **Open:** Gemini was unavailable in this environment; use Claude + Codex as the required two-provider review set for plan-review.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
