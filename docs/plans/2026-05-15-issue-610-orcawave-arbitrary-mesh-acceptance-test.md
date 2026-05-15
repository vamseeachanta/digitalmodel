# Plan for #610: OrcaWave: add licensed end-to-end acceptance test for arbitrary mesh workflow

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/610
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-610-claude.md | scripts/review/results/2026-05-15-plan-610-codex.md

---

## Scope

Acceptance-test scope. This does not add the underlying package/conversion workflow. Implementation is blocked until #605 packaging, #606 mesh preparation, and #611 result contract have landed. The test must be opt-in on licensed hosts and must not make normal Linux CI depend on OrcFxAPI or an OrcaWave license.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#610` - OPEN - `OrcaWave: add licensed end-to-end acceptance test for arbitrary mesh workflow`; label: `enhancement`.
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

- `tests/solver/smoke_test.py` has L00/L01 smoke tests for known OrcaWave files and imports `OrcFxAPI` only at runtime.
- Issue #468 is closed and covered the known `.yml -> OrcFxAPI.Calculate() -> .owr/.xlsx` path.
- `orcawave_runner.py` can execute via OrcFxAPI and save `.owr` plus data.
- `solver/report_extractors.py` can load `.owr` through OrcFxAPI and extract report data.

### Gaps identified

- No acceptance test starts from a DigitalModel-managed arbitrary mesh/spec fixture.
- Current Linux/CI behavior for this exact acceptance path is not defined beyond general OrcFxAPI absence handling.
- No single test asserts generated input, mesh asset, `.owr`, `.xlsx`, and extracted result arrays together.
- Existing `tests/solver/smoke_test.py` collects L00/L01 tests and writes licensed artifacts into `tests/fixtures/solver`; #610 must use a new collection-safe test file and temp output directory instead of extending that write pattern.

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
PYTHONPATH=src uv run python -m pytest tests/solver/smoke_test.py --collect-only -q
```

Baseline proof: `PYTHONPATH=src uv run python -m pytest tests/solver/smoke_test.py --collect-only -q` collected only `test_l00_smoke` and `test_l01_smoke` on Linux on 2026-05-15; there is no arbitrary-mesh acceptance entry today.

## Deliverable

A licensed-host acceptance test proves a repo-managed arbitrary mesh/spec can generate OrcaWave input, calculate through OrcFxAPI, save results, export Excel, and extract usable result data.

## Proposed Tasks

1. Choose or create a small mesh/spec fixture suitable for licensed OrcaWave execution, with explicit mesh units, mass/inertia, water depth, frequency grid, heading grid, and expected runtime budget of 10 minutes or less on the licensed Windows host.
2. Add a licensed-host pytest entrypoint separate from fast unit tests, marked and gated by `DIGITALMODEL_RUN_LICENSED_ORCAWAVE=1`; without OrcFxAPI/license, without the env var, or without the #605/#606/#611 APIs, it must skip cleanly with a message naming the missing precondition. The env var is intentional so licensed hosts do not run solver jobs accidentally during normal test sweeps.
3. Drive the package/run path through `OrcaWaveRunner` and the #605/#606 workflow rather than ad hoc scripts.
4. Require #611 to expose concrete fields before #610 implementation starts: `.owr` results path, saved-data path, optional Excel path, manifest path, and an `export_xlsx=True` runner/CLI option. #610 must enable Excel export for the licensed test.
5. Write `.owr`, exported `.xlsx`, and metadata into a temp/output directory recorded by the test, not into committed fixture directories.
6. Verify non-empty frequencies, headings, hydrostatics, and at least one RAO/result array using #611 structured result fields.
7. Document how to run on the licensed Windows host, including env var, expected duration, output location, cleanup expectations, and proof-of-run issue comment requirements.

## Pseudocode

```text
test_fixture_validates_without_license:
  spec = DiffractionSpec.from_yaml(fixture/spec.yml)
  assert mesh fixture exists and MeshPipeline can load/validate it

test_licensed_arbitrary_mesh_e2e:
  skip unless DIGITALMODEL_RUN_LICENSED_ORCAWAVE=1 and OrcFxAPI import succeeds
  skip unless #605/#606/#611 public APIs are importable
  result = OrcaWaveRunner(RunConfig(output_dir=tmp, export_xlsx=True)).run(spec, spec_path)
  assert result.status == COMPLETED
  assert result.results_file, result.saved_data_file, result.excel_file exist
  extract report data from .owr and assert non-empty grids/results
```

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
| Create | `tests/solver/test_orcawave_arbitrary_mesh_e2e.py` | add arbitrary-mesh acceptance tests behind explicit licensed marker/env gate |
| Create | `tests/fixtures/solver/arbitrary_mesh/` | mesh/spec fixture |
| Modify | `docs/domains/orcawave/README.md` | licensed acceptance instructions |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_arbitrary_mesh_orcawave_e2e_skips_without_orcfxapi` | non-licensed hosts skip cleanly | Linux/no OrcFxAPI | pytest skip, no failure |
| `test_arbitrary_mesh_orcawave_e2e_skips_without_opt_in_env` | licensed tests are not accidental | OrcFxAPI may be present but env var absent | pytest skip, no solver invocation |
| `test_arbitrary_mesh_fixture_validates_without_license` | fixture not broken behind skip | mesh/spec fixture on Linux | schema loads and mesh validates/preflights without OrcFxAPI |
| `test_arbitrary_mesh_orcawave_e2e_skips_without_dependencies` | dependency surface explicit | #605/#606/#611 APIs absent | pytest skip naming missing dependency |
| `test_arbitrary_mesh_orcawave_e2e_generates_outputs` | licensed run creates artifacts | mesh/spec fixture | `.yml`, mesh, `.owr`, `.xlsx` |
| `test_arbitrary_mesh_orcawave_e2e_extracts_results` | outputs usable | generated `.owr` | non-empty grids and result arrays |

## Acceptance Criteria

- [ ] Test exits 0 on licensed OrcFxAPI host.
- [ ] Test starts from repo-managed mesh/spec fixture.
- [ ] Generated `.yml`, mesh asset, `.owr`, and `.xlsx` paths are recorded.
- [ ] Test verifies non-empty frequencies, headings, hydrostatic data, and one result array.
- [ ] Linux/CI skips cleanly when OrcFxAPI is unavailable.
- [ ] Licensed test is opt-in via marker/env var and writes outputs outside committed fixtures.
- [ ] Fixture validity is checked on unlicensed Linux, so broken fixture/spec data cannot hide behind licensed skips.
- [ ] Before closing #610, a licensed-host proof-of-run is attached or commented with run date, host, command, duration, output directory, and hashes/paths for `.owr` and `.xlsx`.
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

- **Risk:** #605/#606/#611 are hard dependencies. If they are absent, #610 implementation must not duplicate package/result-contract implementation; it may only add clearly skipping scaffolding if the maintainer explicitly requests that split.
- **Risk:** The test can be expensive or unavailable on ordinary hosts; marker/env gating and temp output isolation are required to keep CI stable.
- **Risk:** Licensed OrcaWave/OrcFxAPI behavior cannot be fully verified on Linux; tests requiring a license must skip cleanly and be proven on the licensed host where applicable.
- **Open:** T2 complexity requires two-provider review; Claude + Codex are the selected review pair for this plan.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
