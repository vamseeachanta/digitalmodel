# Plan for #613: OrcaWave: add environment and license doctor command

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/613
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-613-claude.md | scripts/review/results/2026-05-15-plan-613-codex.md | scripts/review/results/2026-05-15-plan-613-gemini.md

---

## Scope

Diagnostics only, plus a user-visible warning when an attempted solve falls back to dry-run because no API/executable is available. This issue should not change solver execution outcomes: the doctor may apply stricter readiness validation than the current runner, but runner execution detection must preserve existing behavior unless a separate compatibility decision changes it.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#613` - OPEN - `OrcaWave: add environment and license doctor command`; label: `enhancement`.
- `#500` - OPEN - `OrcaWave: mesh file pre-flight validation + auto-copy in runner`; label includes `status:plan-approved`.
- `#611` - OPEN - `OrcaWave: result export and postprocess contract`; current label at drafting: `enhancement` only, not yet landed, so #613 must not add partial `RunResult.warnings`.

### Sources consulted

- `AGENTS.md` - digitalmodel declares `PYTHONPATH=src uv run python -m pytest` as the repository test command and points source ownership at `src/digitalmodel/`.
- `docs/plans/` - repo has standalone plan files but no `docs/plans/README.md` index/template; issue #596 explicitly recorded "no `docs/plans/README.md` in this issue", so these plans follow the existing standalone-file convention.
- `src/digitalmodel/hydrodynamics/diffraction/cli.py` - current Click surface includes `convert-aqwa`, `convert-orcawave`, `compare`, `batch`, `convert-spec`, `validate-spec`, `run-orcawave`, `run-aqwa`, `batch-aqwa`, `batch-orcawave`, `plot-raos`, `mesh-build`, and benchmark commands; there is no given-mesh or doctor command yet.
- `src/digitalmodel/hydrodynamics/diffraction/spec_converter.py` - `SpecConverter.convert()` delegates directly to backends and `validate()` checks non-empty mesh strings, frequencies, headings, and positive mass only.
- `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` - runner can generate OrcaWave input, copy existing mesh files, prefer OrcFxAPI, and fall back to dry-run when no API/executable is available.
- `src/digitalmodel/hydrodynamics/diffraction/mesh_pipeline.py` - existing pipeline can load/validate/prepare meshes and maps OrcaWave target format to GDF, but it is not integrated into `SpecConverter` or `OrcaWaveRunner`.
- `docs/domains/orcawave/notes/run_time.md` - existing thread/memory/runtime guidance source for doctor output.
- Related issue `#500` - open plan-approved issue for mesh pre-flight validation and runner auto-copy; these future issues must not duplicate its direct scope.

### Issue-specific code findings

- `RunConfig.use_api` defaults true and runner checks OrcFxAPI import availability before execution.
- `_detect_executable()` checks explicit path, `ORCAWAVE_PATH`, standard install paths, and `shutil.which("orcawave")`.
- `run()` silently returns `DRY_RUN` when neither API nor executable is available and `dry_run` was not requested.
- `tests/solver/smoke_test.py` prints OrcFxAPI DLL version when available but is not a general doctor command.
- `_detect_executable()` currently collapses unset and set-but-missing candidates into `None`, but it treats any existing explicit/`ORCAWAVE_PATH` path as usable even if it is a directory or non-executable file; the doctor must inspect each source independently so diagnostics can name invalid candidates without changing runner execution behavior in #613.
- Current runner code checks OrcFxAPI importability but does not call `DLLVersion()`; `tests/solver/smoke_test.py` demonstrates `DLLVersion()` use. No OrcFxAPI license-check API has been verified, so API license readiness remains `UNKNOWN` in #613. A repo-local batch script uses the executable probe `orcawave.exe -check-license`; #613 should support that as an executable-license diagnostic when a usable executable candidate is available, without treating DLL import alone as license proof.

### Gaps identified

- No user-facing command summarizes OrcFxAPI, executable path, license readiness, output writability, or dry-run-only status.
- No mocked tests cover diagnostics for no-OrcFxAPI Linux hosts.
- Exit-code policy for warnings versus hard failures is undefined.

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

A diagnostic CLI command reports whether the host can run OrcaWave diffraction or is limited to dry-run/package preparation.

## Proposed Tasks

1. Add a diagnostics service that checks OrcFxAPI import, DLL/version when available, API license status (`UNKNOWN` unless a verified OrcFxAPI check exists), executable `-check-license` status when a usable executable candidate exists, executable candidates, `ORCAWAVE_PATH`, configured `--executable`, output writability/creatability, platform, thread count, and basic memory/runtime guidance. Current runner code checks importability but does not call `DLLVersion()`; the version check is new doctor behavior and must not be described as existing runner behavior. The executable license probe is source-backed by the existing repo batch script; run it with a short timeout, capture stdout/stderr/return code, report PASS only on exit 0, WARN by default on nonzero/timeout, and FAIL only when `--require-license` is set.
2. Inspect each executable candidate source independently: explicit `--executable`, `ORCAWAVE_PATH`, standard install paths, and `PATH`. Report set-but-invalid paths distinctly from "not configured"; an executable candidate must exist, be a file, and be executable on POSIX (or have an accepted Windows executable suffix on Windows). Existing directories and non-executable files are invalid candidates.
3. Add `diffraction orcawave-doctor` as a Click command with human-readable output and optional JSON output. Options are: `--json`, `--executable PATH`, `--output-dir PATH` defaulting to the runner's effective fallback directory `output` when `RunConfig.output_dir is None`, `--thread-count INT` optional and defaulting to `RunConfig.thread_count` (`4` at current HEAD), `--require-api`, `--require-executable`, `--require-license`, and `--require-output-writable`. `--thread-count` uses Click/Pydantic-equivalent validation (`INT >= 1`) so zero/negative values fail before diagnostics run.
4. Define requirement flags that drive nonzero exit codes: `--require-api`, `--require-executable`, `--require-license`, and `--require-output-writable`. Default Linux/no-OrcFxAPI/no-executable behavior is WARN/dry-run-only and exits 0 unless a requirement flag is unmet.
5. Define JSON schema: `{schema_version, overall_status, checks:[{name,status,message,details}], execution_mode, exit_code}` where status is `PASS|WARN|FAIL|UNKNOWN`. `execution_mode` values are `api_available`, `executable_available`, `dry_run_only`, and `unknown`; priority follows runner intent: API available wins when `use_api` is true, else usable executable, else dry-run-only.
6. Define output-directory policy without mutation: if the output directory exists and is a directory, check writability; if it exists and is a file, report status `FAIL` even without `--require-output-writable` because `OrcaWaveRunner.prepare()` will fail when `mkdir(parents=True, exist_ok=True)` targets a file; if it does not exist, walk parents with `Path.parent`/`exists`/`is_dir` checks only, never `mkdir`, up to the nearest existing ancestor, report `WARN`/`FAIL` if any missing path component is blocked by an existing non-directory, and report "creatable" only when the nearest existing ancestor is writable. This is a non-mutating approximation of runner behavior because current `OrcaWaveRunner.prepare()` mutates with `mkdir(parents=True, exist_ok=True)`. `--require-output-writable` passes for an existing writable dir or a nonexistent dir with a writable ancestor and no non-directory path component, and fails for an unwritable/missing parent or non-directory component.
7. Reuse runner detection helpers only after making them pure/idempotent; no doctor check may mutate run state or create solver output. Keep doctor executable validation separate from runner execution detection in #613: doctor reports directories/non-executable files as invalid readiness findings, while the runner keeps its current execution outcome behavior unless a separate issue tightens it. Do not add `RunResult.warnings` in #613 while #611 owns the result contract. For user-visible fallback warnings before #611 lands, update only the `run-orcawave` Click command to emit a concise stderr/stdout warning when `result.status == DRY_RUN`, `dry_run` was false, and neither API nor executable was available; do not add user-facing warning side effects to the library runner API. If #611 has landed, the CLI may also surface `RunResult.warnings`.
8. Add tests for no-OrcFxAPI, mocked OrcFxAPI, explicit executable, env executable, invalid env path, existing non-executable file, directory-as-executable, JSON output, license UNKNOWN, thread-count/memory guidance sourced from `run_time.md`, output directory policy, and requirement-flag exit policy.

## Pseudocode

```text
orcawave_doctor(options):
  checks = [
    check_orcfxapi_import_and_version(),
    check_api_license_unknown_and_executable_check_license_if_available(),
    check_executable_candidate("--executable", options.executable),
    check_executable_candidate("ORCAWAVE_PATH", env),
    check_standard_install_paths(),
    check_path_lookup("orcawave"),
    check_output_dir_writable_or_creatable_without_creating(options.output_dir),
    check_thread_count_guidance(options.thread_count),
  ]
  overall = FAIL only if requested requirement check failed else worst(PASS/WARN/UNKNOWN)
  render human or JSON schema_version=1
  exit 1 only when requirement failed
```

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-613-orcawave-doctor-command.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-613-claude.md` |
| Plan review - Codex (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-613-codex.md` |
| Plan review - Gemini (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-613-gemini.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-613-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `src/digitalmodel/hydrodynamics/diffraction/cli.py` | add doctor command |
| Create | `src/digitalmodel/hydrodynamics/diffraction/orcawave_doctor.py` | diagnostics logic |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` | expose pure detection helpers only; no user-facing fallback-warning side effects in library runner API |
| Create | `tests/hydrodynamics/diffraction/test_orcawave_doctor.py` | diagnostic tests |
| Modify | `docs/domains/orcawave/README.md` | document readiness check |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_orcawave_doctor_no_orcfxapi_reports_dry_run_only` | Linux fallback clear | import unavailable | WARN text, exit 0 by default, no stack trace |
| `test_orcawave_doctor_mock_orcfxapi_reports_version` | API version shown | mocked DLLVersion | PASS line |
| `test_orcawave_doctor_detects_env_executable` | env path included | `ORCAWAVE_PATH` | path in report |
| `test_orcawave_doctor_detects_explicit_executable` | explicit path included | `--executable` | path in report |
| `test_orcawave_doctor_invalid_env_path_reports_source` | set-but-invalid path distinct | bad `ORCAWAVE_PATH` | WARN/FAIL check names env var and bad path |
| `test_orcawave_doctor_existing_non_executable_file_reports_invalid` | existence alone is not enough | `ORCAWAVE_PATH` points at existing non-executable file | invalid executable finding names path and reason |
| `test_orcawave_doctor_directory_executable_candidate_reports_invalid` | directories are not executables | `--executable` points at directory | invalid executable finding names path and reason |
| `test_orcawave_doctor_standard_install_paths_report_when_present` | standard path checks covered without platform noise | mocked standard path candidate | check reports the candidate source and validity |
| `test_orcawave_doctor_json_output_is_structured` | machine output stable | `--json` | checks contain name, status, message, details |
| `test_orcawave_doctor_license_unknown_without_verified_api` | no false license claim | OrcFxAPI version available but no license API | license check status `UNKNOWN`/WARN |
| `test_orcawave_doctor_executable_check_license_passes_when_supported` | repo-local executable license probe is used when available | usable executable mock returns 0 for `-check-license` | executable license check status PASS with captured version/source details |
| `test_orcawave_doctor_executable_check_license_nonzero_warns_or_fails_when_required` | license failures are explicit but not default-fatal | usable executable mock returns nonzero/timeout for `-check-license` | WARN by default, nonzero exit with `--require-license` |
| `test_orcawave_doctor_require_api_exit_policy` | hard requirement applied | `--require-api` and no OrcFxAPI | nonzero exit |
| `test_orcawave_doctor_invalid_thread_count_rejected` | CLI mirrors `RunConfig.thread_count > 0` | `--thread-count 0` or negative | Click validation error before diagnostic checks |
| `test_orcawave_doctor_output_dir_policy_matches_runner_creation` | non-mutating output policy | nonexistent output dir with writable parent | no directory created; check reports creatable and passes `--require-output-writable` |
| `test_orcawave_doctor_output_dir_existing_file_fails` | non-directory output target matches runner hard failure | `--output-dir` path that exists as a file or where a component is a file | check fails by default and no filesystem mutation occurs |
| `test_orcawave_doctor_unwritable_output_policy` | exit policy applied | output dir or parent unwritable with/without `--require-output-writable` | nonzero only when required |
| `test_orcawave_doctor_thread_memory_guidance` | issue guidance covered | thread count option/config | report includes thread count and memory/runtime guidance source |
| `test_run_orcawave_cli_warns_on_implicit_fallback_dry_run` | attempted solve gets a visible warning without changing result schema | CLI run with no API/no executable and no `--dry-run` | exit remains successful/dry-run behavior, output contains dry-run-only warning, no partial `RunResult.warnings` field added pre-#611 |
| `test_orcawave_runner_fallback_status_not_changed_by_doctor` | runner behavior not regressed and #611 remains owner of warning schema | no API/no executable run | status remains `DRY_RUN`; #613 does not add partial `RunResult` fields unless #611 has landed |

## Acceptance Criteria

- [ ] CLI reports PASS/WARN/FAIL diagnostics for OrcaWave execution readiness.
- [ ] Missing OrcFxAPI produces a clear dry-run-only message, not a stack trace.
- [ ] Explicit executable path and `ORCAWAVE_PATH` are reported.
- [ ] Set-but-invalid executable candidates are reported with their source.
- [ ] Existing directories and non-executable files are not reported as usable executables.
- [ ] JSON output follows the documented schema.
- [ ] OrcFxAPI license status is `UNKNOWN` unless a verified API check exists; the doctor must not imply a license is available from DLL import alone.
- [ ] Executable license status is checked with a bounded `-check-license` probe when a usable executable candidate exists, with WARN by default and FAIL only under `--require-license`.
- [ ] Thread-count and basic memory/runtime guidance are reported.
- [ ] Output-directory checks are non-mutating and match runner semantics for creatable directories and existing-file hard failures.
- [ ] Doctor options are documented and tested: `--json`, `--executable`, `--output-dir`, `--thread-count`, `--require-api`, `--require-executable`, `--require-license`, and `--require-output-writable`.
- [ ] #613 does not implement partial #611 result-contract fields; `RunResult.warnings` is consumed only if #611 has already landed.
- [ ] `run-orcawave` CLI prints a user-visible dry-run-only warning on implicit fallback without changing runner execution status.
- [ ] The implicit-fallback warning is CLI-owned; the library runner API does not gain user-facing warning output or partial #611 fields in #613.
- [ ] Doctor stricter executable validation is documented as readiness-only and does not change `_detect_executable()` execution semantics in #613.
- [ ] Exit-code policy is documented and tested.
- [ ] Tests cover Linux/no-OrcFxAPI and mocked OrcFxAPI behavior.
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

- **Risk:** The doctor must not imply a full license is available unless OrcFxAPI/executable exposes a reliable check. Unknown license state should remain WARN/UNKNOWN.
- **Risk:** Sharing detection helpers with `OrcaWaveRunner` must preserve current run behavior; diagnostics should not change solve/dry-run semantics.
- **Risk:** Licensed OrcaWave/OrcFxAPI behavior cannot be fully verified on Linux; tests requiring a license must skip cleanly and be proven on the licensed host where applicable.
- **Open:** Default review policy uses three-provider review; Claude, Codex, and Gemini are the selected review set for this plan.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
