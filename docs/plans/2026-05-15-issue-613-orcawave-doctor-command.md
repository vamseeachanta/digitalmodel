# Plan for #613: OrcaWave: add environment and license doctor command

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/613
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-613-claude.md | scripts/review/results/2026-05-15-plan-613-gemini.md

---

## Scope

Diagnostics only. This issue should not change solver execution behavior except to share detection helpers safely.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#613` - OPEN - `OrcaWave: add environment and license doctor command`; label: `enhancement`.
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

- `RunConfig.use_api` defaults true and runner checks OrcFxAPI import availability before execution.
- `_detect_executable()` checks explicit path, `ORCAWAVE_PATH`, standard install paths, and `shutil.which("orcawave")`.
- `run()` silently returns `DRY_RUN` when neither API nor executable is available and `dry_run` was not requested.
- `tests/solver/smoke_test.py` prints OrcFxAPI DLL version when available but is not a general doctor command.

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

1. Add a diagnostics service that checks OrcFxAPI import, DLL/version when available, executable detection, environment variables, output writability, and optional thread count.
2. Investigate whether OrcFxAPI exposes a license check; if not, report capability as unknown rather than guessing.
3. Add `diffraction orcawave-doctor` or equivalent Click command.
4. Define PASS/WARN/FAIL severity and exit-code policy.
5. Reuse runner detection helpers without mutating run state.
6. Add tests for no-OrcFxAPI, mocked OrcFxAPI, explicit executable, and output directory failure.

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-613-orcawave-doctor-command.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-613-claude.md` |
| Plan review - Gemini (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-613-gemini.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-613-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `src/digitalmodel/hydrodynamics/diffraction/cli.py` | add doctor command |
| Create | `src/digitalmodel/hydrodynamics/diffraction/orcawave_doctor.py` | diagnostics logic |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` | expose pure detection helpers if needed |
| Create | `tests/hydrodynamics/diffraction/test_orcawave_doctor.py` | diagnostic tests |
| Modify | `docs/domains/orcawave/README.md` | document readiness check |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_orcawave_doctor_no_orcfxapi_reports_dry_run_only` | Linux fallback clear | import unavailable | WARN/FAIL text, no stack trace |
| `test_orcawave_doctor_mock_orcfxapi_reports_version` | API version shown | mocked DLLVersion | PASS line |
| `test_orcawave_doctor_detects_env_executable` | env path included | `ORCAWAVE_PATH` | path in report |
| `test_orcawave_doctor_unwritable_output_policy` | exit policy applied | bad output dir | nonzero only per policy |

## Acceptance Criteria

- [ ] CLI reports PASS/WARN/FAIL diagnostics for OrcaWave execution readiness.
- [ ] Missing OrcFxAPI produces a clear dry-run-only message, not a stack trace.
- [ ] Explicit executable path and `ORCAWAVE_PATH` are reported.
- [ ] Exit-code policy is documented and tested.
- [ ] Tests cover Linux/no-OrcFxAPI and mocked OrcFxAPI behavior.
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

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
