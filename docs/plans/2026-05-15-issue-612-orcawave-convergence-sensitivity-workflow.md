# Plan for #612: OrcaWave: add batch mesh convergence and grid sensitivity workflow

> **Status:** draft - awaiting adversarial review
> **Complexity:** T3
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/612
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-612-claude.md | scripts/review/results/2026-05-15-plan-612-gemini.md

---

## Scope

Study orchestration and reporting scope. It should depend on the result contract in #611 for stable comparison inputs.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#612` - OPEN - `OrcaWave: add batch mesh convergence and grid sensitivity workflow`; label: `enhancement`.
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

- `orcawave_batch_runner.py` exists for batch OrcaWave execution.
- `benchmark_runner.py` already compares/plots multi-solver hydrodynamic outputs and looks for `.owr` paths in solver metadata.
- `docs/domains/orcawave/notes/run_time.md` documents runtime/memory considerations including thread count, mesh size, and QTF method choices.
- No dedicated convergence-study schema was found in the diffraction module.

### Gaps identified

- No YAML schema defines mesh refinement variants plus frequency/heading grids as a study.
- Batch output directories are not tied to a convergence/sensitivity report contract.
- No comparison metric set is defined for added mass, damping, excitation, RAOs, hydrostatics, and warnings.

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

A batch study workflow can define OrcaWave mesh/grid variants, prepare/run or dry-run cases, and compare selected hydrodynamic outputs for convergence and sensitivity.

## Proposed Tasks

1. Define a convergence-study YAML schema for mesh variants, grid variants, solver settings, and comparison metrics.
2. Extend or wrap `OrcaWaveBatchRunner` to prepare one run directory per case and support dry-run planning.
3. Consume standardized result metadata from #611 for comparisons.
4. Implement comparison tables/plots for selected outputs and warnings.
5. Add runtime/memory estimates using documented OrcaWave notes and case dimensions.
6. Add tests for schema parsing, dry-run case planning, mocked result comparison, and report generation.

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-612-orcawave-convergence-sensitivity-workflow.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-612-claude.md` |
| Plan review - Gemini (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-612-gemini.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-612-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/hydrodynamics/diffraction/orcawave_convergence.py` | study schema/orchestration/reporting |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_batch_runner.py` | integrate case generation if needed |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/benchmark_runner.py` | reuse comparison helpers if practical |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/cli.py` | add command if approved |
| Create | `tests/hydrodynamics/diffraction/test_orcawave_convergence.py` | TDD coverage |
| Modify | `docs/domains/orcawave/notes/run_time.md` | link study guidance if needed |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_convergence_study_config_parses_cases` | YAML schema expands cases | mesh/grid study | expected case matrix |
| `test_convergence_study_dry_run_creates_case_dirs` | planning works without license | dry-run study | per-case directories/manifests |
| `test_convergence_study_compares_mock_results` | metrics computed | mocked artifacts | convergence table |
| `test_convergence_study_report_flags_outliers` | report highlights risk | divergent mock values | outlier warning |

## Acceptance Criteria

- [ ] User can define a convergence study in YAML.
- [ ] Batch runner creates one package/run directory per case.
- [ ] Comparison report identifies convergence metrics and outliers.
- [ ] Workflow supports dry-run planning on non-licensed hosts.
- [ ] Tests cover batch planning and mocked result comparison.
## Plan Review Gating

- [ ] Completed review artifacts under `/mnt/local-analysis/workspace-hub/digitalmodel/scripts/review/results/` exist for at least two providers and each non-empty artifact contains a `## Verdict` section; 0-byte in-progress files do not satisfy this gate.
- [ ] Issue is commented with this plan and moved to `status:plan-review` only after review artifacts exist.

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | PENDING | Awaiting review artifact |
| Gemini | PENDING | Awaiting review artifact |

**Overall result:** PENDING - do not label `status:plan-review` until artifacts exist and MAJOR findings, if any, are handled or explicitly carried as blockers.

## Risks and Open Questions

- **Risk:** #500 is already plan-approved and runner-side `_copy_mesh_files()` / `_validate_mesh_references()` exist at HEAD; implementation must reuse or refactor that code instead of creating divergent path-resolution/copy logic.
- **Risk:** Licensed OrcaWave/OrcFxAPI behavior cannot be fully verified on Linux; tests requiring a license must skip cleanly and be proven on the licensed host where applicable.
- **Open:** Whether to include Codex as a third review provider is optional; plan-review requires at least two completed provider artifacts with verdicts.

## Complexity: T3

T3 justification: T3 cross-module orchestration and reporting workflow with multiple dependent surfaces.
