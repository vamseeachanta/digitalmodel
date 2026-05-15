# Plan for #612: OrcaWave: add batch mesh convergence and grid sensitivity workflow

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/612
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-612-claude.md | scripts/review/results/2026-05-15-plan-612-codex.md | scripts/review/results/2026-05-15-plan-612-gemini.md

---

## Scope

Study orchestration and reporting scope, bounded to schema expansion, dry-run case planning, and comparison against #611-shaped result fixtures. Implementation is blocked until #605/#606 package/preparation APIs exist for case directory generation and #611 result metadata exists for comparison mode. It should not implement new numerical solvers, live licensed execution, or duplicate result extraction formulas.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#612` - OPEN - `OrcaWave: add batch mesh convergence and grid sensitivity workflow`; label: `enhancement`.
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

- `orcawave_batch_runner.py` exists for batch OrcaWave execution.
- `benchmark_runner.py` already compares/plots multi-solver hydrodynamic outputs and looks for `.owr` paths in solver metadata.
- `multi_solver_comparator.py` already contains comparison/report structures for hydrodynamic outputs and should be evaluated before adding new metric code.
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

1. Define a convergence-study YAML schema for mesh variants, grid variants, solver settings, comparison metrics, tolerances, and baseline/reference selection.
2. Add `diffraction orcawave-convergence` for dry-run case planning and comparison-report generation from existing #611 manifests. Licensed execution is out of scope for #612 and should remain a follow-up.
3. Extend or wrap `OrcaWaveBatchRunner` to prepare one run directory per case only after #605/#606 APIs are present. If those APIs are absent, the command fails/skips with a dependency message rather than generating duplicate package logic.
4. Consume standardized result metadata from #611 for comparisons; if #611 is absent, comparison mode is unavailable and reports the dependency.
5. Implement comparison tables/plots only for fields present in `DiffractionResults` / #611 contracts: added mass, radiation damping, RAOs, hydrostatics, and missing-artifact status. Excitation arrays and solver warnings are out of scope until #611 defines those fields.
6. Add tests for schema parsing, dry-run case planning, #605/#606/#611 dependency errors, #611-shaped fixture comparison, tolerance/outlier behavior, and report generation.

## Pseudocode

```text
load_study_config(path):
  parse mesh variants, grid variants, solver settings, metric list, tolerances
  expand deterministic case matrix and baseline selection

plan_cases(config):
  require #605/#606 package/preparer APIs
  for case in matrix: create case dir and manifest in dry-run mode

compare_cases(config, result_manifests):
  require #611 manifest schema
  load #611-shaped fixture/manifests
  compute selected metrics from added_mass/damping/raos/hydrostatics
  flag outliers by configured tolerances and write report
```

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-612-orcawave-convergence-sensitivity-workflow.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-612-claude.md` |
| Plan review - Codex (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-612-codex.md` |
| Plan review - Gemini (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-612-gemini.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-612-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/hydrodynamics/diffraction/orcawave_convergence.py` | study schema/orchestration/reporting |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_batch_runner.py` | integrate case generation if needed |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/benchmark_runner.py` | reuse comparison helpers if practical |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/multi_solver_comparator.py` | reuse/extend existing comparison models if it is the better fit than benchmark runner |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/cli.py` | add `orcawave-convergence` command |
| Create | `tests/hydrodynamics/diffraction/test_orcawave_convergence.py` | TDD coverage |
| Modify | `docs/domains/orcawave/notes/run_time.md` | link study guidance if needed |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_convergence_study_config_parses_cases` | YAML schema expands cases | mesh/grid study | expected case matrix |
| `test_convergence_study_dry_run_creates_case_dirs` | planning works without license | dry-run study | per-case directories/manifests |
| `test_convergence_study_compares_mock_results` | metrics computed | mocked artifacts | convergence table |
| `test_convergence_study_compares_611_shaped_fixture` | comparison honors real contract shape | in-tree #611 manifest fixture | convergence table from actual schema fields |
| `test_convergence_study_requires_result_contract_for_compare` | dependency is explicit | compare requested without #611 metadata | clear dependency error, no guessed sidecar paths |
| `test_convergence_study_requires_packaging_apis_for_planning` | #605/#606 dependency explicit | package/preparer APIs absent | clear dependency error, no duplicate package logic |
| `test_convergence_study_report_flags_outliers` | report highlights risk | divergent mock values | outlier warning |

## Acceptance Criteria

- [ ] User can define a convergence study in YAML.
- [ ] Study schema expands deterministic mesh/grid case matrices.
- [ ] Dry-run planning creates one package/run directory per case only through #605/#606 APIs.
- [ ] Comparison report identifies convergence metrics and outliers only when #611-shaped result manifests are available.
- [ ] Workflow supports dry-run planning on non-licensed hosts.
- [ ] Tests cover batch planning and mocked result comparison.
- [ ] Standard Linux tests pass without OrcFxAPI installed; licensed execution remains out of scope.
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

- **Risk:** #605/#606/#611 are hard dependencies. Without them, #612 must report dependency errors rather than infer package or result paths.
- **Risk:** Batch convergence workflows can be expensive. This plan excludes live licensed execution and keeps normal tests/CI on dry-run and fixture-based comparison only.
- **Risk:** Licensed OrcaWave/OrcFxAPI behavior cannot be fully verified on Linux; tests requiring a license must skip cleanly and be proven on the licensed host where applicable.
- **Open:** Default review policy uses three-provider review; Claude, Codex, and Gemini are the selected review set for this plan.

## Complexity: T2

T2 justification: this plan is deliberately bounded to schema/dry-run planning and fixture-based comparison, with live licensed batch execution deferred until the upstream package/result contracts have landed.
