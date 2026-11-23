# Plan for #610: OrcaWave: add licensed end-to-end acceptance test for arbitrary mesh workflow

> **Status:** plan-review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/610
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-610-claude.md | scripts/review/results/2026-05-15-plan-610-codex.md | scripts/review/results/2026-05-15-plan-610-gemini.md

---

## Scope

Acceptance-test scope. This does not add the underlying package/conversion workflow. Implementation is blocked until #500 preflight/runner copy, #605 packaging, #606 mesh preparation, and #611 result contract have landed. The test must be opt-in on licensed hosts and must not make normal Linux CI depend on OrcFxAPI or an OrcaWave license.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#610` - OPEN - `OrcaWave: add licensed end-to-end acceptance test for arbitrary mesh workflow`; label: `enhancement`.
- `#500` - OPEN - `OrcaWave: mesh file pre-flight validation + auto-copy in runner`; label includes `status:plan-approved`.

### Sources consulted

- `AGENTS.md` - digitalmodel declares `PYTHONPATH=src uv run python -m pytest` as the repository test command and points source ownership at `src/digitalmodel/`.
- `docs/plans/` - repo has standalone plan files but no `docs/plans/README.md` index/template; issue #596 explicitly recorded "no `docs/plans/README.md` in this issue", so these plans follow the existing standalone-file convention.
- `pytest.ini` and `pyproject.toml` - strict marker checking is enabled; #610 must register any new licensed OrcaWave marker before adding marked tests.
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

1. Choose or create a small supported non-GDF source mesh fixture, with `.stl` as the default fixture unless #606 lands another reviewed supported-source policy. Do not use `.obj` or `.msh` unless #606 has landed handlers and source-backed conversion/passthrough policy for them and #610 is re-reviewed. The fixture spec must be suitable for licensed OrcaWave execution, with explicit mesh units, mass/inertia, water depth, frequency grid, heading grid, expected runtime target of 8 minutes or less on the licensed Windows host, and parent timeout of 900 seconds to leave teardown/artifact margin. The licensed E2E starts from this non-GDF source mesh and must exercise the #606 prepare/convert path into OrcaWave-ready GDF before calculation. For unlicensed Linux validation, include a fast fixture test that runs the landed #606 preparer against the same non-GDF fixture and asserts the prepared GDF/package metadata exists without invoking OrcFxAPI.
2. Add a licensed-host pytest entrypoint separate from fast unit tests, using the existing `solver` marker applied automatically under `tests/solver/` and additionally gated by `DIGITALMODEL_RUN_LICENSED_ORCAWAVE=1`; check preconditions in this order: env var, dependency symbol predicates, `OrcFxAPI` import/version, then the bounded subprocess run. The env var is intentional even with the `solver` marker so solver-marker sweeps on licensed hosts do not run this longer acceptance case accidentally. Do not import future #605/#606/#611 modules at test-module import time; every dependency predicate must be evaluated through a function-scoped/importlib-based helper after the env-var gate so pytest collection passes on hosts where those blocked dependencies are not landed. Do not claim license readiness from object construction alone; until a verified OrcFxAPI license-check API exists, license readiness remains `UNKNOWN` before the bounded calculation starts.
3. Do not add a new marker unless the existing `solver` marker proves insufficient. Keep unlicensed fixture-validation, dependency-gate, child-protocol, timeout-classification, and proof-marker schema tests outside `tests/solver/` so `tests/solver/conftest.py` does not auto-mark and deselect them under `-m "not solver"`. Only the real licensed OrcaWave calculation test belongs under `tests/solver/`.
4. Drive the package/run path through `OrcaWaveRunner` and the #500/#605/#606 workflow rather than ad hoc scripts.
5. Align with the landed #611 artifact contract at implementation time, not the current draft text. Before #610 implementation starts, verify on `main` that the final #611 API exposes the equivalent of `RunConfig.export_xlsx`, `RunResult.artifacts`, `OrcaWaveResultArtifacts`, `OrcaWaveRunManifest`, `result.artifacts.results_file`, `result.artifacts.saved_data_file`, `result.artifacts.excel_file`, and `result.artifacts.manifest_file`; if #611 lands different names or locations, revise/re-review #610 instead of matching this draft plan by memory. #610 must enable Excel export through the landed #611 mechanism for the licensed test. A report artifact is optional in #610 unless the landed #611 contract exposes and documents a mandatory report-generation helper; otherwise the proof marker records `report_file: null` and verifies result arrays directly from `.owr`.
6. Define dependency symbol predicates before the licensed test body runs through a collection-safe helper such as `collect_orcawave_arbitrary_mesh_dependencies()`: use `importlib.import_module("digitalmodel.hydrodynamics.diffraction.orcawave_asset_resolver")` and require `OrcaWaveAssetResolver`; import `digitalmodel.hydrodynamics.diffraction.orcawave_mesh_preparer` and require `OrcaWaveMeshPreparer`; import `digitalmodel.hydrodynamics.diffraction.orcawave_run_artifacts` and require `OrcaWaveRunManifest` / `OrcaWaveResultArtifacts`; then check `"export_xlsx" in RunConfig.model_fields`, and `"artifacts" in RunResult.__annotations__` or the equivalent `dataclasses.fields(RunResult)` check. Do not use module-level imports for these future dependency modules in either `tests/solver/test_orcawave_arbitrary_mesh_e2e.py` or the unlicensed harness tests. Do not use `hasattr()` for Pydantic/dataclass field predicates because it returns false for existing model fields. These module/class names are expected integration seams, not permanent skip excuses: if #605 or #606 lands equivalent resolver/preparer APIs under different names or locations, revise/re-review #610 instead of leaving a predicate that can never pass. #500 is treated as satisfied only through the #605 resolver reconciliation. Because #610 implementation is blocked until #500/#605/#606/#611 land, missing dependency symbols are an import/API gate failure when `DIGITALMODEL_RUN_LICENSED_ORCAWAVE=1`; the licensed proof test must fail with a dependency diagnostic rather than skip. Only env-var absence is a valid skip. Missing `OrcFxAPI` or `DLLVersion()` failure after env opt-in is an infrastructure failure, not a skip.
7. Define the OrcFxAPI availability probe concretely but narrowly: after `OrcFxAPI` imports, call `OrcFxAPI.DLLVersion()` and record it. Do not instantiate `OrcFxAPI.Diffraction()` as a license probe unless #613 later documents that as a verified license-consuming check. The first operation that can prove the solve license is the bounded calculation itself. The child process writes structured JSON to an explicit temp-file path supplied by the parent; stdout/stderr may contain ordinary logs and are not parsed as the protocol. Normal child outcomes are `completed`, `license_denied`, and `failed`, with `error_type`, `error_message`, and artifact paths. If the child crashes before writing JSON, the parent records `failed` with captured return code/stdout/stderr. A timeout is parent-classified: when the 900-second wall-clock budget expires, the parent terminates the child, writes/classifies the `timeout` outcome with elapsed time and any partial artifact paths it can observe, and fails the test with that diagnostic. Classify `license_denied` only for known OrcFxAPI license exception classes/codes or conservative denial phrases such as `not licensed`, `no valid license`, `license denied`, `license checkout failed`, `OrcaWave license unavailable`, or `OrcFxAPI license unavailable` before accepted artifacts exist. Do not use bare substring `license` as a denial predicate because informational/warning text may mention available licenses. Because `DIGITALMODEL_RUN_LICENSED_ORCAWAVE=1` is an explicit proof-run opt-in, `license_denied` fails the licensed test with a clear infrastructure diagnostic and does not count as a passing skip or produce `non_skipped=true` proof. Unexpected errors are `failed` and fail the test. Unlicensed harness tests for this parent/child protocol must use mock child processes or static JSON result fixtures; they must not invoke OrcFxAPI, start a real OrcaWave calculation, or require #605/#606/#611 modules to exist at collection time.
8. Write `.owr`, exported `.xlsx`, manifest, and metadata into a pytest temp/output directory recorded by the test, never into committed fixture directories. `tests/fixtures/solver/arbitrary_mesh/` contains read-only fixture files only.
9. Execute the licensed runner inside a child process created with the Windows-compatible `multiprocessing` `spawn` method or an equivalent subprocess wrapper. The parent test enforces the 900-second budget by terminating the child process on timeout and failing with a timeout diagnostic; it does not rely on `RunConfig.timeout_seconds`, `pytest-timeout`, or in-process cancellation of `OrcFxAPI.Diffraction.Calculate()`, which is synchronous and not safely cancellable from the same process.
10. Verify non-empty frequencies, headings, hydrostatics, and at least one RAO/result array by passing `result.artifacts.results_file` to `digitalmodel.hydrodynamics.diffraction.solver.report_extractors.extract_report_data_from_owr()` or the landed #611-approved extractor route; #611 provides artifact paths, not hydrodynamic result arrays.
11. Parse the generated OrcaWave `.yml` and assert the body mesh filename points at the prepared OrcaWave-ready GDF/package path, not the original non-GDF source fixture. This is required in both the fast fixture validation and the licensed E2E so the test proves the actual solver input consumes the prepared mesh.
12. Define proof marker file `orcawave_arbitrary_mesh_proof.json` in the test output directory with fields: `schema_version="1.0"`, `non_skipped=true`, `source_mesh_file` for the non-GDF fixture, `prepared_mesh_file` for the #606 OrcaWave-ready GDF, `license_readiness="unknown_before_calculation"`, `run_date_utc`, `host` sourced from `platform.node()`, `command` sourced from the parent test invocation (`sys.argv` plus the resolved pytest node id or equivalent explicit command string), `duration_seconds`, `output_dir`, `input_file` for the generated OrcaWave `.yml`, `artifacts` mapping logical names to paths with optional `report_file`, and `sha256` for generated `.yml`, `.owr`, `.xlsx`, prepared mesh, and manifest artifacts. These hashes are identity/provenance fingerprints for the concrete proof run, not deterministic regression baselines across machines or repeated OrcaWave executions. The issue proof-of-run comment must include this JSON path or its contents.
13. Document how to run on the licensed Windows host, including env var, marker, expected duration, output location, cleanup expectations, and proof-of-run issue comment requirements.

## Pseudocode

```text
test_fixture_validates_without_license:
  spec = DiffractionSpec.from_yaml(fixture/spec.yml)
  assert non-GDF source mesh fixture exists
  prepared = landed #606 OrcaWaveMeshPreparer prepares source mesh to OrcaWave-ready GDF without OrcFxAPI
  assert prepared mesh and package/preparer metadata exist

test_licensed_arbitrary_mesh_e2e:
  skip unless DIGITALMODEL_RUN_LICENSED_ORCAWAVE=1
  evaluate #500/#605/#606/#611 dependency symbol predicates through a function-scoped importlib helper after env opt-in; fail if missing
  fail with infrastructure diagnostic if OrcFxAPI import or DLLVersion fails after env opt-in; license state is UNKNOWN before calculation
  result = run child process that starts from non-GDF fixture/spec and calls #605/#606 package/prepare path plus OrcaWaveRunner(RunConfig(output_dir=tmp, export_xlsx=True)).run(spec, spec_path)
  parent terminates child, writes/classifies timeout outcome, and fails if wall-clock duration >900 seconds
  if child outcome == license_denied: fail proof run with diagnostic; do not skip
  assert result.status == COMPLETED
  assert result.artifacts.results_file, result.artifacts.saved_data_file, result.artifacts.excel_file, result.artifacts.manifest_file exist
  assert generated OrcaWave .yml references the prepared GDF basename/path, not the original non-GDF source mesh
  assert orcawave_arbitrary_mesh_proof.json records non_skipped=True, source_mesh_file, prepared_mesh_file, license_readiness, host, command, duration, input_file, output_dir, and sha256 hashes including generated `.yml`
  extract report data from result.artifacts.results_file with digitalmodel.hydrodynamics.diffraction.solver.report_extractors.extract_report_data_from_owr or the landed #611-approved .owr extractor route and assert non-empty grids/results
```

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-610-orcawave-arbitrary-mesh-acceptance-test.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-610-claude.md` |
| Plan review - Codex (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-610-codex.md` |
| Plan review - Gemini (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-610-gemini.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-610-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `tests/solver/test_orcawave_arbitrary_mesh_e2e.py` | add arbitrary-mesh acceptance tests behind existing `solver` marker/env gate |
| Create | `tests/hydrodynamics/diffraction/test_orcawave_arbitrary_mesh_fixture.py` | unlicensed Linux fixture/schema validation outside auto-marked `tests/solver/` |
| Create | `tests/hydrodynamics/diffraction/test_orcawave_arbitrary_mesh_harness.py` | unlicensed dependency-gate, child JSON protocol, timeout classification, and proof-marker schema tests outside auto-marked `tests/solver/` |
| Create | `tests/fixtures/solver/arbitrary_mesh/` | non-GDF source mesh/spec fixture plus read-only expected metadata seeds if required by #606 tests |
| Modify | `docs/domains/orcawave/README.md` | licensed acceptance instructions |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_arbitrary_mesh_orcawave_e2e_fails_without_orcfxapi_after_opt_in` | proof runs cannot pass by missing licensed dependency | env var set, dependencies present/mocked, no OrcFxAPI | test fails with infrastructure diagnostic and no proof marker with `non_skipped=true` |
| `test_arbitrary_mesh_orcawave_e2e_skips_without_opt_in_env` | licensed tests are not accidental | OrcFxAPI may be present but env var absent | pytest skip, no solver invocation |
| `test_arbitrary_mesh_orcawave_e2e_records_unknown_license_before_run` | unverified object construction is not treated as license proof | env var set, dependencies present, OrcFxAPI importable, `DLLVersion()` succeeds | child calculation is the first license-consuming operation; pre-run status records license readiness `UNKNOWN` |
| `test_arbitrary_mesh_orcawave_e2e_fails_on_known_license_denied_after_opt_in` | proof runs cannot pass by skip after explicit licensed opt-in | env var set, dependencies present, child runner returns `license_denied` by class or conservative message predicate before accepted artifacts | test fails with license diagnostic and no proof marker with `non_skipped=true` |
| `test_arbitrary_non_gdf_fixture_prepares_without_license` | arbitrary source mesh fixture not broken behind licensed skip | non-GDF source mesh/spec fixture on Linux plus landed #606 preparer | schema loads, source mesh prepares to OrcaWave-ready GDF/package metadata, and no OrcFxAPI is invoked |
| `test_arbitrary_mesh_orcawave_e2e_fails_without_landed_dependencies_after_opt_in` | dependency surface cannot pass by skip after implementation prerequisites are expected to exist, while collection remains safe before dependencies land | env var set and one required symbol predicate absent through monkeypatched `importlib.import_module` or temporary module fixture | test module collects, then test fails naming exact missing dependency before OrcFxAPI/license checks |
| `test_arbitrary_mesh_orcawave_e2e_generated_input_uses_prepared_gdf` | solver input actually consumes converted mesh | supported non-GDF fixture prepared by #606 and generated OrcaWave `.yml` | parsed `.yml` body mesh filename points at prepared GDF/package asset, not original `.stl` source |
| `test_arbitrary_mesh_orcawave_e2e_generates_outputs` | licensed run creates artifacts and cannot pass by skip | non-GDF mesh/spec fixture with env var, dependencies, OrcFxAPI, license | test is collected, not skipped, prepares/copies OrcaWave-ready mesh, writes proof marker, `.yml`, `.owr`, `.xlsx`, and manifest |
| `test_arbitrary_mesh_orcawave_e2e_extracts_results` | outputs usable | generated `.owr` | non-empty grids and result arrays |
| `test_arbitrary_mesh_orcawave_timeout_is_bounded` | runtime budget enforced with teardown margin | licensed test child process hangs or exceeds budget | parent terminates child at 900 seconds, creates parent-owned timeout outcome JSON/diagnostic, fails with timeout diagnostic, and no pytest-timeout plugin is required |
| `test_arbitrary_mesh_child_result_protocol_uses_json_file` | logs cannot corrupt child result parsing without a licensed run | mocked child process writes logs to stdout/stderr and static result JSON to parent-supplied path | parent reads structured JSON from file, and classifies missing JSON as failed with captured logs |
| `test_arbitrary_mesh_proof_marker_schema` | proof artifact deterministic without a licensed run | static completed child-result JSON and temp files representing `.yml`, `.owr`, `.xlsx`, prepared mesh, and manifest | JSON has schema_version, non_skipped, source_mesh_file, prepared_mesh_file, host from `platform.node()`, command from explicit parent invocation data, output_dir, paths, optional report_file, and sha256 fields including generated `.yml` |

## Acceptance Criteria

- [ ] On a licensed OrcFxAPI host with `DIGITALMODEL_RUN_LICENSED_ORCAWAVE=1`, the `solver`-marked E2E test is collected, not skipped, targets an 8-minute-or-less solve, completes under the 900-second parent-enforced child-process timeout, and writes `orcawave_arbitrary_mesh_proof.json` with artifact hashes/paths.
- [ ] Test starts from a repo-managed supported non-GDF source mesh/spec fixture and proves #606 prepares it into an OrcaWave-ready GDF before licensed calculation; the generated OrcaWave `.yml` is parsed and verified to reference the prepared GDF rather than the source `.stl`/`.obj`.
- [ ] Generated OrcaWave `.yml` is recorded as #611 manifest `input_file` and proof-marker `input_file`, while result/data/excel/manifest outputs are recorded through `result.artifacts`; report output is asserted only if the landed #611 contract makes it mandatory, otherwise proof records `report_file: null`.
- [ ] Test verifies non-empty frequencies, headings, hydrostatic data, and one result array.
- [ ] Linux/CI skips cleanly only when the opt-in env var is absent; when `DIGITALMODEL_RUN_LICENSED_ORCAWAVE=1` is set, missing OrcFxAPI or DLLVersion failure is a proof-run infrastructure failure, not a skip.
- [ ] Licensed test is opt-in via existing registered `solver` marker plus env var and writes outputs outside committed fixtures.
- [ ] Fixture validity and non-GDF preparation are checked on unlicensed Linux, so broken fixture/spec/preparer data cannot hide behind licensed skips.
- [ ] Dependency predicates use function-scoped/importlib-based checks, Pydantic `model_fields`, and dataclass annotations/fields, not module-level future imports or `hasattr()`; #605/#606/#611 public API name mismatches require plan revision/re-review rather than indefinite skips; after env opt-in, missing landed dependency symbols fail the test instead of skipping, while pytest collection remains safe when dependencies are absent.
- [ ] OrcFxAPI license readiness is not claimed by `OrcFxAPI.Diffraction()` construction; known license-denied calculation failures fail the explicit proof run without producing `non_skipped=true`, hangs are killed by the parent timeout, and unexpected failures fail.
- [ ] Unlicensed harness tests use mock child processes/static JSON fixtures for dependency-gate, child protocol, timeout classification, and proof-marker schema coverage; they do not require OrcFxAPI, a license, or future #605/#606/#611 modules at collection time.
- [ ] Proof marker provenance defines `host` and `command` sources and includes the generated OrcaWave `.yml` in the SHA-256 artifact list.
- [ ] Result-array verification uses the explicit `.owr` extractor route from `result.artifacts.results_file`; #611 artifact metadata is not treated as hydrodynamic array data.
- [ ] Before closing #610, a licensed-host proof-of-run is attached or commented with run date, host, command, duration, output directory, and hashes/paths for `.owr` and `.xlsx`.
## Plan Review Gating

- [ ] Completed review artifacts under `/mnt/local-analysis/workspace-hub/digitalmodel/scripts/review/results/` exist for at least two providers and each non-empty artifact contains a `## Verdict` section; 0-byte in-progress files do not satisfy this gate.
- [ ] Any provider `MAJOR` finding requires a plan revision and re-review; the issue is commented with this plan and moved to `status:plan-review` only after no unresolved `MAJOR` findings remain.

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | UNAVAILABLE | Review command failed; completed artifact retained |
| Codex | UNAVAILABLE | Codex CLI quota/usage unavailable on final rerun after plan patch |
| Gemini | UNAVAILABLE | Final rerun hit Gemini quota after the previous MAJOR was patched; artifact records provider failure |

**Overall result:** READY FOR USER REVIEW (best-effort) - completed artifacts exist, the fresh MAJOR findings on collection-safe imports and unlicensed harness mocks were addressed by plan revision, and the final rerun was provider-unavailable due quota. Implementation remains gated by upstream dependencies named in the plan.

## Risks and Open Questions

- **Risk:** #605/#606/#611 are hard dependencies. If they are absent, #610 implementation must not duplicate package/result-contract implementation; it may only add clearly skipping scaffolding if the maintainer explicitly requests that split.
- **Risk:** The test can be expensive or unavailable on ordinary hosts; marker/env gating and temp output isolation are required to keep CI stable.
- **Risk:** Licensed OrcaWave/OrcFxAPI behavior cannot be fully verified on Linux; tests requiring a license must skip cleanly and be proven on the licensed host where applicable.
- **Open:** Default review policy uses three-provider review; Claude, Codex, and Gemini are the selected review set for this plan.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
