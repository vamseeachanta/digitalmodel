# Plan for #611: OrcaWave: define result export and postprocess contract for diffraction runs

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/611
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-611-claude.md | scripts/review/results/2026-05-15-plan-611-codex.md | scripts/review/results/2026-05-15-plan-611-gemini.md

---

## Scope

Output contract scope. It should avoid changing numerical extraction formulas unless tests show a contract gap. This issue defines metadata and artifact paths for API, subprocess, and dry-run OrcaWave runs; it does not change package generation (#605/#606) or licensed acceptance coverage (#610).

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
- `tests/solver/smoke_test.py` uses `diff.SaveResultsSpreadsheet(...)` for Excel export on licensed hosts; #611 should use that API for opt-in `.xlsx` export.
- `output_schemas.py`, `output_validator.py`, `result_extractor.py`, `orcawave_batch_runner.py`, and `report_generator.py` are existing result/report contract surfaces that must be considered to avoid a parallel manifest.

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

1. Add `OrcaWaveResultArtifacts` to `RunResult` or as a nested field with explicit names: `results_file` (`.owr`, optional until completed), `saved_data_file` (`.owd`/saved data path), `excel_file` (`.xlsx`, optional and opt-in), `report_file` (optional generated report), and `manifest_file` (`orcawave_run_manifest.json`). Paths are stored as `Path` objects in Python and serialized relative to `output_dir` in the manifest.
2. Define `orcawave_run_manifest.json` schema with `schema_version`, `status`, `input_file`, `output_dir`, `mesh_files`, `artifacts`, `solver_version`, `thread_count`, `duration_seconds`, `warnings`, `return_code`, `execution_mode` (`api|subprocess|dry_run`), and `error_message`.
3. Make `.xlsx` export opt-in through a runner config/CLI flag such as `export_xlsx`; implement licensed API export via `OrcFxAPI.Diffraction.SaveResultsSpreadsheet(...)`. Dry-run never fabricates `.xlsx`; subprocess mode records discovered `.xlsx` only if produced by OrcaWave/external workflow.
4. Update API execution to populate actual `.owr`, saved-data, optional `.xlsx`, and manifest fields. Update subprocess completed execution to discover expected result artifacts in `output_dir` after process exit and record missing artifacts as warnings rather than guessing success paths. Dry-run records generated inputs and package assets with no fake result files.
5. Stop overloading `log_file` for `.owr`: set `log_file` only to captured text logs if present and migrate tests/callers to `result.artifacts.results_file`.
6. Update CLI output, `result_extractor.py`, `orcawave_batch_runner.py`, `benchmark_runner.py`, `solver/report_extractors.py`, and `report_generator.py` integration points so downstream `.xlsx -> OrcaFlex` and `.owr -> report` paths use the manifest/artifact fields rather than guessed names.
7. Add dry-run, mocked API, mocked subprocess, manifest-schema, and downstream-integration tests; licensed extraction behavior remains covered by #610.

## Pseudocode

```text
execute_via_api(result, config):
  diffraction.Calculate()
  result.artifacts.results_file = output/name.owr after SaveResults()
  result.artifacts.saved_data_file = output/name.owd after SaveData()
  if config.export_xlsx: result.artifacts.excel_file = output/name.xlsx after SaveResultsSpreadsheet()
  result.artifacts.manifest_file = write_manifest(result)

execute_via_subprocess(result, config):
  run executable and capture return_code/stdout/stderr/log_file
  if completed: discover .owr/.xlsx/report files in output_dir by configured names
  missing expected artifacts become warnings, not fabricated paths
  write_manifest(result)
```

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-611-orcawave-result-contract.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-611-claude.md` |
| Plan review - Codex (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-611-codex.md` |
| Plan review - Gemini (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-611-gemini.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-611-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` | result contract fields and API population |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/cli.py` | print artifacts/flags |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_to_orcaflex.py` | consume standard Excel artifact path from the result contract |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/solver/report_extractors.py` | consume explicit `.owr` artifact path |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/report_generator.py` | record generated report path in manifest/result artifacts |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/result_extractor.py` | consume OrcaWave artifact fields instead of scanning only `.sim`/`.dat` |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_batch_runner.py` | pass structured artifacts to extraction/report steps |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/benchmark_runner.py` | migrate `.owr` metadata lookup to result manifest/artifact contract or document adapter |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/output_schemas.py` | reuse/extend existing output schema definitions for manifest validation |
| Create/modify | `tests/hydrodynamics/diffraction/test_orcawave_runner.py` | metadata/export tests |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_run_result_records_api_artifacts` | explicit artifact fields | mocked OrcFxAPI success | `.owr` and data paths populated |
| `test_runner_optional_xlsx_export_records_path` | Excel policy works | export flag enabled | `.xlsx` path populated |
| `test_runner_default_does_not_export_xlsx` | default avoids large sidecar | mocked OrcFxAPI success with default config | `.xlsx` path absent and manifest explains disabled export |
| `test_log_file_no_longer_overloaded_with_owr` | compatibility contract clear | mocked OrcFxAPI success | `.owr` field populated; `log_file is None` unless a real text log is captured |
| `test_subprocess_completed_discovers_result_artifacts` | subprocess path contract | mocked executable success and fake `.owr` in output | `results_file` populated and manifest written |
| `test_subprocess_completed_missing_artifact_warns` | no guessed result paths | mocked executable success with no `.owr` | warning recorded, no fake `.owr` path |
| `test_cli_prints_result_artifacts` | user-visible paths | mocked completed run | paths in CLI output |
| `test_dry_run_manifest_has_inputs_no_results` | dry-run contract | dry run | generated inputs, no fake results |
| `test_manifest_schema_records_required_metadata` | issue-required metadata covered | mocked API result | OrcFxAPI version, thread count, timing, warnings, and artifact paths serialized |
| `test_batch_and_report_paths_use_artifact_contract` | downstream paths stop guessing | mocked `RunResult` with artifacts | batch/report helpers consume `results_file`/manifest |

## Acceptance Criteria

- [ ] Successful runs expose `.owr`, saved data, optional `.xlsx`, and report/manifest paths in structured metadata.
- [ ] CLI output prints the same paths clearly.
- [ ] Downstream `.xlsx -> OrcaFlex` and `.owr -> report` paths do not require manual guessing.
- [ ] Tests cover dry-run metadata, mocked successful API execution metadata, mocked subprocess metadata, manifest schema, and downstream batch/report integration.
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

- **Risk:** Existing callers may treat `RunResult.log_file` as the `.owr` result path. Implementation needs an explicit compatibility/migration check before changing that behavior.
- **Risk:** Excel export can be large and slow; making it opt-in avoids surprising normal runs while still supporting OrcaFlex handoff.
- **Risk:** Licensed OrcaWave/OrcFxAPI behavior cannot be fully verified on Linux; tests requiring a license must skip cleanly and be proven on the licensed host where applicable.
- **Open:** Default review policy uses three-provider review; Claude, Codex, and Gemini are the selected review set for this plan.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
