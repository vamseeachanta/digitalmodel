# Plan for #613: OrcaWave: add environment and license doctor command

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/613
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-613-claude.md | scripts/review/results/2026-05-15-plan-613-codex.md

---

## Scope

Diagnostics only. This issue should not change solver execution behavior except to share pure detection helpers safely. It should make the current "dry-run-only fallback" visible to users before they attempt a solve.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#613` - OPEN - `OrcaWave: add environment and license doctor command`; label: `enhancement`.
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

1. Add a diagnostics service that checks OrcFxAPI import, DLL/version when available, executable detection, `ORCAWAVE_PATH`, configured executable path, output writability, platform, and optional thread count.
2. Investigate whether OrcFxAPI exposes a license check; if not, report license capability as `UNKNOWN` rather than guessing or failing.
3. Add `diffraction orcawave-doctor` as a Click command with human-readable output and optional JSON output.
4. Define PASS/WARN/FAIL severity and exit-code policy: FAIL only for explicit requested requirements that are unmet, WARN for dry-run-only/unknown license status, PASS for available capability.
5. Reuse runner detection helpers only after making them pure/idempotent; no doctor check may mutate run state or create solver output.
6. Add tests for no-OrcFxAPI, mocked OrcFxAPI, explicit executable, env executable, JSON output, and output directory failure.

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-613-orcawave-doctor-command.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-613-claude.md` |
| Plan review - Codex (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-613-codex.md` |
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
| `test_orcawave_doctor_json_output_is_structured` | machine output stable | `--json` | checks contain name, status, message, details |
| `test_orcawave_doctor_unwritable_output_policy` | exit policy applied | bad output dir | nonzero only per policy |

## Acceptance Criteria

- [ ] CLI reports PASS/WARN/FAIL diagnostics for OrcaWave execution readiness.
- [ ] Missing OrcFxAPI produces a clear dry-run-only message, not a stack trace.
- [ ] Explicit executable path and `ORCAWAVE_PATH` are reported.
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

**Overall result:** PENDING - do not label `status:plan-review` until artifacts exist and no unresolved `MAJOR` findings remain.

## Risks and Open Questions

- **Risk:** The doctor must not imply a full license is available unless OrcFxAPI/executable exposes a reliable check. Unknown license state should remain WARN/UNKNOWN.
- **Risk:** Sharing detection helpers with `OrcaWaveRunner` must preserve current run behavior; diagnostics should not change solve/dry-run semantics.
- **Risk:** Licensed OrcaWave/OrcFxAPI behavior cannot be fully verified on Linux; tests requiring a license must skip cleanly and be proven on the licensed host where applicable.
- **Open:** Gemini was unavailable in this environment; use Claude + Codex as the required two-provider review set for plan-review.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
