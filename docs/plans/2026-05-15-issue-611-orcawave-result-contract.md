# Plan for #611: OrcaWave: define result export and postprocess contract for diffraction runs

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/611
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-611-claude.md | scripts/review/results/2026-05-15-plan-611-codex.md

---

## Scope

Output contract scope. It should avoid changing numerical extraction formulas unless tests show a contract gap. This issue defines metadata and artifact paths for completed/dry-run runs; it does not change package generation (#605/#606) or licensed acceptance coverage (#610).

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#611` - OPEN - `OrcaWave: define result export and postprocess contract for diffraction runs`; label: `enhancement`.
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

- `RunResult` currently stores input, modular files, mesh files, log file, stdout/stderr, return code, duration, error message, and spec name.
- `_execute_via_api()` saves `.owr` with `SaveResults()` and a data file with `SaveData()`, then stores the `.owr` path in `log_file`.
- `orcawave_to_orcaflex.py` consumes `.xlsx` sidecars for OrcaFlex handoff without OrcFxAPI.
- `solver/report_extractors.py` consumes `.owr` through OrcFxAPI for reports.

### Gaps identified

- Result file paths are not first-class fields in `RunResult`.
- `.xlsx` export policy is not defined for normal runner execution.
- CLI output does not print a structured result manifest for downstream tools.
- No test covers mocked successful API execution metadata including result artifacts.

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

OrcaWave run results expose explicit structured paths and metadata for `.owr`, saved data, optional `.xlsx`, reports, warnings, and downstream handoff.

## Proposed Tasks

1. Extend `RunResult` or add a nested result-artifact dataclass/model for `.owr`, saved data, optional `.xlsx`, report, and metadata manifest paths. Keep `log_file` backward-compatible as a real log path only; do not silently use it as the `.owr` result path.
2. Make `.xlsx` export opt-in through a runner config/CLI flag because it can be large and is only needed for selected handoff flows; document the rationale and default.
3. Update OrcFxAPI execution to populate explicit result fields and preserve a compatibility alias or migration note for callers that previously inspected `log_file`.
4. Emit a run manifest with solver version when available, thread count, timings, input file, mesh files, warnings, result files, and dry-run/completed status.
5. Update CLI output and downstream handoff/report utilities to use the contract without requiring callers to guess sidecar names.
6. Add dry-run and mocked API tests; licensed extraction behavior remains covered by #610.

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-611-orcawave-result-contract.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-611-claude.md` |
| Plan review - Codex (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-611-codex.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-611-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` | result contract fields and API population |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/cli.py` | print artifacts/flags |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_to_orcaflex.py` | consume standard paths if applicable |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/solver/report_extractors.py` | integrate with manifest if applicable |
| Create/modify | `tests/hydrodynamics/diffraction/test_orcawave_runner.py` | metadata/export tests |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_run_result_records_api_artifacts` | explicit artifact fields | mocked OrcFxAPI success | `.owr` and data paths populated |
| `test_runner_optional_xlsx_export_records_path` | Excel policy works | export flag enabled | `.xlsx` path populated |
| `test_runner_default_does_not_export_xlsx` | default avoids large sidecar | mocked OrcFxAPI success with default config | `.xlsx` path absent and manifest explains disabled export |
| `test_log_file_no_longer_overloaded_with_owr` | compatibility contract clear | mocked OrcFxAPI success | `.owr` field populated; `log_file` is absent or points to log, not result file |
| `test_cli_prints_result_artifacts` | user-visible paths | mocked completed run | paths in CLI output |
| `test_dry_run_manifest_has_inputs_no_results` | dry-run contract | dry run | generated inputs, no fake results |

## Acceptance Criteria

- [ ] Successful runs expose `.owr`, saved data, optional `.xlsx`, and report/manifest paths in structured metadata.
- [ ] CLI output prints the same paths clearly.
- [ ] Downstream `.xlsx -> OrcaFlex` and `.owr -> report` paths do not require manual guessing.
- [ ] Tests cover dry-run metadata and mocked successful API execution metadata.
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

- **Risk:** Existing callers may treat `RunResult.log_file` as the `.owr` result path. Implementation needs an explicit compatibility/migration check before changing that behavior.
- **Risk:** Excel export can be large and slow; making it opt-in avoids surprising normal runs while still supporting OrcaFlex handoff.
- **Risk:** Licensed OrcaWave/OrcFxAPI behavior cannot be fully verified on Linux; tests requiring a license must skip cleanly and be proven on the licensed host where applicable.
- **Open:** Gemini was unavailable in this environment; use Claude + Codex as the required two-provider review set for plan-review.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
