# Plan for #610: OrcaWave: add licensed end-to-end acceptance test for arbitrary mesh workflow

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/610
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-610-claude.md | scripts/review/results/2026-05-15-plan-610-codex.md

---

## Scope

Acceptance-test scope. This does not add the underlying package/conversion workflow; it should run after #605/#606/#611 surfaces exist or skip with clear dependency notes until they do.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#610` - OPEN - `OrcaWave: add licensed end-to-end acceptance test for arbitrary mesh workflow`; label: `enhancement`.
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

- `tests/solver/smoke_test.py` has L00/L01 smoke tests for known OrcaWave files and imports `OrcFxAPI` only at runtime.
- Issue #468 is closed and covered the known `.yml -> OrcFxAPI.Calculate() -> .owr/.xlsx` path.
- `orcawave_runner.py` can execute via OrcFxAPI and save `.owr` plus data.
- `solver/report_extractors.py` can load `.owr` through OrcFxAPI and extract report data.

### Gaps identified

- No acceptance test starts from a DigitalModel-managed arbitrary mesh/spec fixture.
- Current Linux/CI behavior for this exact acceptance path is not defined beyond general OrcFxAPI absence handling.
- No single test asserts generated input, mesh asset, `.owr`, `.xlsx`, and extracted result arrays together.

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

A licensed-host acceptance test proves a repo-managed arbitrary mesh/spec can generate OrcaWave input, calculate through OrcFxAPI, save results, export Excel, and extract usable result data.

## Proposed Tasks

1. Choose or create a small mesh/spec fixture suitable for licensed OrcaWave execution.
2. Add a licensed-host test entrypoint separate from fast unit tests or marked to skip cleanly without OrcFxAPI.
3. Drive the package/run path, preferably through `OrcaWaveRunner` rather than ad hoc scripts.
4. Save `.owr`, exported `.xlsx`, and metadata into a predictable fixture/output location.
5. Verify non-empty frequencies, headings, hydrostatics, and at least one RAO/result array.
6. Document how to run on the licensed Windows host.

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-610-orcawave-arbitrary-mesh-acceptance-test.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-610-claude.md` |
| Plan review - Codex (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-610-codex.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-610-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify/create | `tests/solver/smoke_test.py` | add arbitrary-mesh L02 or separate entrypoint |
| Create | `tests/fixtures/solver/arbitrary_mesh/` | mesh/spec fixture |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` | only if output contract gaps block test |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/solver/report_extractors.py` | only if extraction gaps block assertion |
| Modify | `docs/domains/orcawave/README.md` | licensed acceptance instructions |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_arbitrary_mesh_orcawave_e2e_skips_without_orcfxapi` | non-licensed hosts skip cleanly | Linux/no OrcFxAPI | pytest skip, no failure |
| `test_arbitrary_mesh_orcawave_e2e_generates_outputs` | licensed run creates artifacts | mesh/spec fixture | `.yml`, mesh, `.owr`, `.xlsx` |
| `test_arbitrary_mesh_orcawave_e2e_extracts_results` | outputs usable | generated `.owr` | non-empty grids and result arrays |

## Acceptance Criteria

- [ ] Test exits 0 on licensed OrcFxAPI host.
- [ ] Test starts from repo-managed mesh/spec fixture.
- [ ] Generated `.yml`, mesh asset, `.owr`, and `.xlsx` paths are recorded.
- [ ] Test verifies non-empty frequencies, headings, hydrostatic data, and one result array.
- [ ] Linux/CI skips cleanly when OrcFxAPI is unavailable.
## Plan Review Gating

- [ ] Completed review artifacts under `/mnt/local-analysis/workspace-hub/digitalmodel/scripts/review/results/` exist for at least two providers and each non-empty artifact contains a `## Verdict` section; 0-byte in-progress files do not satisfy this gate.
- [ ] Issue is commented with this plan and moved to `status:plan-review` only after review artifacts exist.

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | PENDING | Awaiting review artifact |
| Codex | PENDING | Awaiting review artifact |

**Overall result:** PENDING - do not label `status:plan-review` until artifacts exist and MAJOR findings, if any, are handled or explicitly carried as blockers.

## Risks and Open Questions

- **Risk:** #500 is already plan-approved and runner-side `_copy_mesh_files()` / `_validate_mesh_references()` exist at HEAD; implementation must reuse or refactor that code instead of creating divergent path-resolution/copy logic.
- **Risk:** Licensed OrcaWave/OrcFxAPI behavior cannot be fully verified on Linux; tests requiring a license must skip cleanly and be proven on the licensed host where applicable.
- **Open:** Gemini was unavailable in this environment; use Claude + Codex as the required two-provider review set for plan-review.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
