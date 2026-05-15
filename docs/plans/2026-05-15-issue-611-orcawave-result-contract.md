# Plan for #611: OrcaWave: define result export and postprocess contract for diffraction runs

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/611
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-611-claude.md | scripts/review/results/2026-05-15-plan-611-codex.md | scripts/review/results/2026-05-15-plan-611-gemini.md

---

## Scope

Output contract scope. It should avoid changing numerical extraction formulas unless tests show a contract gap. This issue defines metadata and artifact paths for API, subprocess, and dry-run OrcaWave runs; it does not change package generation (#605/#606) or licensed acceptance coverage (#610). CLI changes target the canonical Click CLI in `src/digitalmodel/hydrodynamics/diffraction/cli.py`; the legacy argparse `diffraction_cli.py` compatibility entry point is out of scope for #611 and must either receive a later parity issue or be documented as deprecated for OrcaWave artifact-contract features.

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
- Related completed issue `#468` covered the L01 `.yml -> OrcFxAPI -> .owr/.xlsx` path. `docs/domains/orcawave/L01_aqwa_benchmark/run_orcawave_api.py` strips unsupported OrcFxAPI YAML keys such as `PanelAngleWarningLevel` before loading into `OrcFxAPI.Diffraction`; #611 must preserve that compatibility requirement in the runner API path.
- `output_schemas.py` / `output_validator.py` define hydrodynamic coefficient/result-array contracts, not run-time solver telemetry. The #611 run manifest belongs in a new run-artifact schema module so OrcaFlex export and output validation do not inherit execution metadata.
- `result_extractor.py`, `orcawave_batch_runner.py`, `benchmark_runner.py`, `solver/report_extractors.py`, and `report_generator.py` are existing result/report contract surfaces that must be considered to avoid a parallel manifest.
- `rg -n "log_file|results_file|SaveData|SaveResults|export_xlsx|RunConfig\\(" src/digitalmodel/hydrodynamics/diffraction tests/hydrodynamics tests/solver -S` shows `orcawave_runner.py` is the current `.owr -> log_file` producer; no in-tree consumer should continue using `RunResult.log_file` as the `.owr` path after #611.

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

1. Add `OrcaWaveResultArtifacts` in the new module `src/digitalmodel/hydrodynamics/diffraction/orcawave_run_artifacts.py` and import it into `RunResult` to avoid embedding manifest classes in `orcawave_runner.py`. The module must not import `orcawave_runner.py`, preventing circular imports. Fields are explicit names: `results_file` (`.owr`, optional until completed), `saved_data_file` (current `SaveData()` `_data.dat` path; do not rename to `.owd` in #611), `excel_file` (`.xlsx`, optional and opt-in), `report_file` (optional generated report), and `manifest_file` (`orcawave_run_manifest.json`). Paths are stored as `Path` objects in Python and serialized relative to `output_dir` in the manifest.
2. Add explicit `warnings: list[str]` and `execution_mode: Literal["api","subprocess","dry_run","fallback_dry_run"]` to `RunResult` so prep warnings, subprocess missing-artifact notes, and fallback diagnostics have an in-memory source before manifest serialization.
3. Define a new `src/digitalmodel/hydrodynamics/diffraction/orcawave_run_artifacts.py` schema for `orcawave_run_manifest.json` with initial `schema_version="1.0"`, `status`, `input_file`, `api_input_file`, `output_dir`, `mesh_files`, `artifacts`, `solver_version`, `configured_thread_count`, `solver_thread_count`, `duration_seconds`, `warnings`, `return_code`, `execution_mode`, `error_message`, `export_xlsx_requested`, `export_xlsx_effective`, and `export_xlsx_reason`. `status` and `execution_mode` are serialized/validated as strings or `Literal[...]` values in this module, not as imports of `RunStatus` from `orcawave_runner.py`, so the no-circular-import rule is enforceable. `solver_version` and `solver_thread_count` are nullable in dry-run/fallback/subprocess modes when no observed solver/API value exists; `configured_thread_count` records `RunConfig.thread_count` whenever available. Add a round-trip load/validate API; do not place this run telemetry schema in `output_schemas.py`.
4. Make `.xlsx` export policy explicit through `RunConfig.export_xlsx: bool | None = None`, CLI flags `run-orcawave --export-xlsx/--no-export-xlsx`, the module-level `run_orcawave(..., export_xlsx: bool | None = None, expected_artifacts: list[str] | None = None, result_file_name: str | None = None, saved_data_file_name: str | None = None, report_file_name: str | None = None)`, batch config plumbing, and `DiffractionSpec.outputs.formats`. Precedence: explicit `RunConfig.export_xlsx=True/False` wins; when it is `None`, export if normalized `spec.outputs.formats` contains `xlsx`; otherwise do not export. Normalize output formats by accepting `OutputFormat` enum members, strings, and case variants via `getattr(format_item, "value", format_item).lower()`. Resolve this policy while `spec` is still in scope in `run()`/`prepare()` and store it in the run context/result fields `export_xlsx_requested`, `export_xlsx_effective`, and `export_xlsx_reason` before `execute()` runs, because `execute()` currently has no spec argument. Implement licensed API export via `OrcFxAPI.Diffraction.SaveResultsSpreadsheet(...)` only when `export_xlsx_effective` is true. Dry-run never fabricates `.xlsx`; subprocess mode does not claim `.xlsx` support unless a later external workflow explicitly writes and registers one.
5. Update API execution to populate actual `.owr`, current `_data.dat` saved-data, optional `.xlsx`, warnings, execution mode, and manifest fields. Carry forward #468's input compatibility behavior by filtering/normalizing known OrcFxAPI-unsupported YAML keys before `OrcFxAPI.Diffraction(...)` loads the generated input, with tests covering `PanelAngleWarningLevel`: write a sibling API-compatible YAML copy such as `<stem>_orcfxapi_compatible.yml`, load that file into OrcFxAPI, preserve the original generated input as `input_file`, record the sibling as `api_input_file`, and add a warning naming stripped/normalized keys. Refactor the current inline subprocess branch in `execute()` into `_execute_via_subprocess()` as part of #611. Add subprocess artifact configuration fields `RunConfig.result_file_name: str | None = None`, `RunConfig.saved_data_file_name: str | None = None`, `RunConfig.report_file_name: str | None = None`, and `RunConfig.expected_artifacts: list[Literal["owr","report","xlsx","saved_data"]] = Field(default_factory=list)`. Subprocess mode discovers `.owr`, OrcaWave saved-data files, and report files when present, preferring configured names when provided and otherwise using bounded globs that compare filesystem `mtime` to a `time.time()` wall-clock run start, not `time.monotonic()`. Saved-data fallback glob is limited to current OrcaWave `*_data.dat` files created after the run start so it cannot be confused with OrcaFlex `.dat` model files. Discovery is best-effort and does not fabricate paths. Missing-artifact warnings are emitted only for `expected_artifacts` entries requested by config; default subprocess runs do not warn forever just because the current executable invocation may not save `.owr` or `_data.dat`. Dry-run and fallback-dry-run paths must also write a manifest recording generated inputs and package assets with no fake result files.
6. Stop overloading `log_file` for `.owr`: set `log_file` only to captured text logs if present and migrate tests/callers to `result.artifacts.results_file`. Acceptance requires a grep/audit proving no in-tree consumer reads `RunResult.log_file` for `.owr` semantics.
7. Update CLI output, `result_extractor.py`, `orcawave_batch_runner.py`, `benchmark_runner.py`, `solver/report_extractors.py`, and `report_generator.py` integration points so downstream artifact flows are explicit: `.xlsx` goes to `orcawave_to_orcaflex.convert_orcawave_xlsx_to_orcaflex`, `.owr` goes only to `solver.report_extractors.extract_report_data_from_owr`, current `_data.dat` is recorded as an OrcaWave SaveData/project artifact but is not fed to the existing OrcaFlex-oriented `ResultExtractor`/`OrcaWaveConverter` unless a future OrcaWave SaveData parser is added, and `.sim` remains OrcaFlex/AQWA-specific. Preserve `ResultExtractor` for `.sim`/OrcaFlex `.dat` paths and add a guard so generic `.dat` scanning does not mistake OrcaWave `_data.dat` for an OrcaFlex model. For report generation, add an explicit runner-side helper such as `OrcaWaveRunner._generate_report_artifact(result, config)` that calls `generate_report_from_owr(result.artifacts.results_file, output_path, ...)` when `config.report_file_name` is set or `"report"` is requested in `expected_artifacts`, assigns the returned/existing path to `RunResult.artifacts.report_file`, and rewrites/validates the run manifest. For `benchmark_runner.py`, add an adapter that flattens `RunResult.artifacts.results_file` / `orcawave_run_manifest.json` into the existing `solver_metadata["owr_path"]`/`["results_file"]` keys until the benchmark metadata schema is migrated. For `orcawave_batch_runner.py`, define artifact-option precedence explicitly: global `OrcaWaveBatchConfig.run_config` supplies defaults, per-job artifact fields override only when non-null/non-empty, and the merged result constructs one `RunConfig` per job. Batch YAML/config can pass top-level defaults and per-job overrides for `export_xlsx`, `expected_artifacts`, `result_file_name`, `saved_data_file_name`, and `report_file_name` into per-run `RunConfig`.
8. Add dry-run, fallback-dry-run, mocked API, mocked subprocess, manifest-schema round-trip, and downstream-integration tests; licensed extraction behavior remains covered by #610.

## Pseudocode

```text
execute_via_api(result, config):
  use resolved result.export_xlsx_effective/result.export_xlsx_reason computed while spec was in scope
  write #468-compatible api_input_file sibling and load that file into OrcFxAPI
  diffraction.Calculate()
  result.artifacts.results_file = output/name.owr after SaveResults()
  result.artifacts.saved_data_file = output/name_data.dat after SaveData()
  if result.export_xlsx_effective: result.artifacts.excel_file = output/name.xlsx after SaveResultsSpreadsheet()
  result.execution_mode = "api"
  result.artifacts.manifest_file = write_manifest(result)

execute_via_subprocess(result, config):
  run executable and capture return_code/stdout/stderr/log_file
  result.execution_mode = "subprocess"
  if completed: discover actual .owr/saved-data/report files in output_dir by config.result_file_name/config.saved_data_file_name/config.report_file_name first, then bounded post-time.time-start globs
  missing config.expected_artifacts become warnings, not fabricated paths
  do not discover .xlsx unless an explicit external exporter registers it
  write_manifest(result)

record_dry_run_manifest(result, fallback=False):
  result.execution_mode = "fallback_dry_run" if fallback else "dry_run"
  result.artifacts.manifest_file = write_manifest(result)
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
| Create | `src/digitalmodel/hydrodynamics/diffraction/orcawave_run_artifacts.py` | run manifest schema and round-trip validation; keep separate from hydrodynamic `output_schemas.py` |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/cli.py` | print artifacts/flags |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/__init__.py` | export public artifact contract types alongside `RunConfig`/`RunResult` |
| Out of scope | `src/digitalmodel/hydrodynamics/diffraction/diffraction_cli.py` | legacy argparse compatibility CLI is not updated in #611; document/follow up separately |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_to_orcaflex.py` | consume standard Excel artifact path from the result contract |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/solver/report_extractors.py` | consume explicit `.owr` artifact path |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/report_generator.py` | record generated report path in manifest/result artifacts |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/result_extractor.py` | consume OrcaWave artifact fields instead of scanning only `.sim`/`.dat` |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_batch_runner.py` | pass structured artifacts to extraction/report steps and plumb `export_xlsx` through batch run config |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/benchmark_runner.py` | migrate `.owr` metadata lookup to result manifest/artifact contract or document adapter |
| Create/modify | `tests/hydrodynamics/diffraction/test_orcawave_runner.py` | metadata/export tests |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_run_result_records_api_artifacts` | explicit artifact fields | mocked OrcFxAPI success | `.owr` and current `_data.dat` paths populated under `result.artifacts` |
| `test_runner_optional_xlsx_export_records_path` | Excel policy works | export flag enabled | `.xlsx` path populated |
| `test_runner_spec_outputs_xlsx_requests_export_when_config_none` | spec output intent is honored | `RunConfig.export_xlsx=None` and spec outputs include `xlsx` | `.xlsx` export path populated |
| `test_runner_spec_outputs_xlsx_normalizes_enum_and_string_formats` | output-format policy matches parsed schemas | `spec.outputs.formats` containing `OutputFormat.XLSX`, `"xlsx"`, and `"XLSX"` variants | export decision treats all as `xlsx` when config is `None` |
| `test_runner_config_no_export_overrides_spec_xlsx` | precedence explicit | `RunConfig.export_xlsx=False` and spec outputs include `xlsx` | no `.xlsx` export and manifest explains disabled by config |
| `test_manifest_records_xlsx_export_decision_fields` | disabled/export reasons are schema-backed | config true/false/None with and without spec xlsx | manifest records `export_xlsx_requested`, `export_xlsx_effective`, and `export_xlsx_reason` |
| `test_runner_filters_468_unsupported_orcfxapi_keys` | completed #468 path preserved | generated input includes `PanelAngleWarningLevel` | API load receives filtered/compatible YAML |
| `test_runner_default_does_not_export_xlsx` | default avoids large sidecar | mocked OrcFxAPI success with default config | `.xlsx` path absent and manifest explains disabled export |
| `test_log_file_no_longer_overloaded_with_owr` | compatibility contract clear | mocked OrcFxAPI success | `.owr` field populated; `log_file is None` unless a real text log is captured |
| `test_subprocess_completed_discovers_result_artifacts_when_present` | subprocess path contract | mocked executable success and fake post-`time.time()`-start `.owr` in output | `results_file` populated and manifest written |
| `test_subprocess_completed_discovers_saved_data_when_present` | subprocess saved-data contract is not API-only | mocked executable success and fake post-`time.time()`-start `name_data.dat` in output | `saved_data_file` populated and manifest written |
| `test_subprocess_completed_missing_requested_artifact_warns` | no guessed result paths | mocked executable success with expected `.owr` requested but absent | warning recorded in `RunResult.warnings`, no fake `.owr` path |
| `test_subprocess_completed_without_expected_artifacts_does_not_warn` | warning channel avoids noise | mocked executable success with `expected_artifacts=[]` and no artifacts present | no missing-artifact warning and no fake result path |
| `test_subprocess_expected_artifacts_config_drives_warnings_not_discovery` | requested-artifact state is concrete | `RunConfig.expected_artifacts=["report","saved_data"]` and output contains `.owr` but no report/saved-data | `.owr` is still discovered; missing report and saved-data warnings are recorded |
| `test_subprocess_configured_result_names_precede_globs` | configured names avoid stale file guesses | output dir has stale `.owr` plus configured fresh result filename | `results_file` uses configured filename and ignores stale file |
| `test_subprocess_configured_saved_data_name_precedes_globs` | configured saved-data name avoids generic `.dat` confusion | output dir has OrcaFlex-like `.dat`, stale `_data.dat`, and configured saved-data filename | `saved_data_file` uses configured filename and ignores unrelated `.dat` files |
| `test_run_orcawave_wrapper_plumbs_artifact_options` | convenience API not stranded | call `run_orcawave(..., export_xlsx=True, expected_artifacts=["owr"])` | constructed `RunConfig` carries artifact options |
| `test_cli_export_xlsx_flags_set_run_config` | user-facing export policy works | `run-orcawave --export-xlsx` and `--no-export-xlsx` | CLI passes True/False into `RunConfig.export_xlsx` |
| `test_cli_prints_result_artifacts` | user-visible paths | mocked completed run | paths in CLI output |
| `test_dry_run_manifest_has_inputs_no_results` | dry-run contract | explicit and fallback dry run | generated inputs, execution mode, warnings, no fake results, manifest written |
| `test_manifest_schema_records_required_metadata` | issue-required metadata covered | mocked API result | schema_version `1.0`, OrcFxAPI version, thread count, timing, warnings, and artifact paths serialized |
| `test_manifest_records_configured_and_solver_thread_counts` | manifest distinguishes requested config from observed solver metadata | dry-run, API, and subprocess-style results | `configured_thread_count` records `RunConfig.thread_count`; `solver_thread_count` is nullable unless observed |
| `test_manifest_schema_round_trips_from_disk` | manifest is not write-only | written manifest JSON | `OrcaWaveRunManifest.from_json()` validates and returns equivalent data |
| `test_batch_and_report_paths_use_artifact_contract` | downstream paths stop guessing | mocked `RunResult` with artifacts | batch/report helpers consume `results_file`/manifest |
| `test_batch_artifact_option_precedence` | per-job overrides do not silently inherit wrong artifact settings | batch config with global run_config defaults plus per-job artifact overrides | merged per-job `RunConfig` uses non-null job overrides and otherwise falls back to global defaults |
| `test_artifact_flow_xlsx_uses_orcaflex_converter` | Excel handoff explicit | mocked `RunResult.artifacts.excel_file` | `orcawave_to_orcaflex` receives the xlsx path |
| `test_artifact_flow_owr_uses_report_extractor` | OWR handoff explicit | mocked `RunResult.artifacts.results_file` | `extract_report_data_from_owr` receives the owr path |
| `test_artifact_flow_saved_data_guarded_from_orcaflex_dat_scan` | SaveData is not misrouted | directory with `name_data.dat` and no `.sim` plus mocked `RunResult.artifacts.saved_data_file` | `ResultExtractor.extract(run_result)` returns a clear "OrcaWave SaveData, not an OrcaFlex model" outcome and never constructs `OrcaWaveConverter` |
| `test_runner_report_artifact_helper_updates_manifest` | report path does not leave manifest stale | runner helper generates report path from `.owr` | `_generate_report_artifact` records `report_file` and rewrites/validates manifest |
| `test_benchmark_metadata_adapter_flattens_artifact_contract` | benchmark runner stays connected during migration | `RunResult.artifacts.results_file` and manifest path | adapter populates existing `solver_metadata["owr_path"]`/`["results_file"]` keys from structured artifacts |
| `test_no_log_file_owr_consumers_remain` | migration coverage is auditable | source grep/static check | no in-tree consumer reads `RunResult.log_file` for `.owr` semantics |
| `test_artifact_contract_types_are_public_exports` | downstream imports are stable | `from digitalmodel.hydrodynamics.diffraction import OrcaWaveResultArtifacts, OrcaWaveRunManifest` | imports succeed without circular import |

## Acceptance Criteria

- [ ] Successful runs expose `.owr`, saved data, optional `.xlsx`, and report/manifest paths in structured metadata.
- [ ] Completed subprocess runs discover actual `.owr`, OrcaWave `_data.dat` saved-data, and report artifacts when present; configured result/saved-data/report names take precedence over bounded globs; `expected_artifacts` only controls warnings for missing expected files and defaults to `Field(default_factory=list)`.
- [ ] CLI output prints the same paths clearly.
- [ ] Canonical Click CLI is updated; legacy `diffraction_cli.py` is documented as out of scope for this artifact contract or tracked by a separate parity/deprecation issue.
- [ ] `SaveData()` keeps the current `_data.dat` artifact path unless a later migration issue explicitly changes it.
- [ ] Excel export precedence is deterministic: explicit `RunConfig.export_xlsx` overrides `spec.outputs.formats`; config `None` follows `spec.outputs.formats`.
- [ ] Manifest records Excel export requested/effective/reason fields and configured-vs-observed thread-count fields so disabled-export and dry-run/subprocess tests do not rely on prose.
- [ ] #468 API compatibility is preserved by filtering/normalizing known OrcFxAPI-unsupported input keys before API load.
- [ ] Downstream `.xlsx -> OrcaFlex` and `.owr -> report` paths are tied to concrete checks: `orcawave_to_orcaflex`, `solver/report_extractors`, `orcawave_batch_runner`, `benchmark_runner` metadata adapter, and `report_generator` accept or receive `RunResult.artifacts` / `orcawave_run_manifest.json` and no longer rely on output-directory globs for these artifact paths.
- [ ] OrcaWave `_data.dat` is recorded as SaveData/project data but guarded from the current OrcaFlex `.dat` model extractor until a dedicated parser exists.
- [ ] `OrcaWaveResultArtifacts` and `OrcaWaveRunManifest` live in `orcawave_run_artifacts.py`, are exported from `digitalmodel.hydrodynamics.diffraction`, and that module has no import dependency on `orcawave_runner.py`.
- [ ] `OrcaWaveRunManifest` serializes runner status/execution mode without importing `RunStatus` from `orcawave_runner.py`; dry-run/fallback manifests allow nullable solver version/thread count.
- [ ] Batch config supports top-level defaults and per-job non-null overrides for artifact-related run config fields, including `saved_data_file_name`, with explicit merge precedence.
- [ ] Tests cover dry-run metadata, fallback-dry-run manifest writing, mocked successful API execution metadata, mocked subprocess metadata, manifest schema round-trip, and downstream batch/report integration.
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
