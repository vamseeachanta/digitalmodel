# Plan for #612: OrcaWave: add batch mesh convergence and grid sensitivity workflow

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/612
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-612-claude.md | scripts/review/results/2026-05-15-plan-612-codex.md | scripts/review/results/2026-05-15-plan-612-gemini.md

---

## Scope

Study orchestration and reporting scope, bounded to schema expansion, dry-run case planning, runtime/memory estimation, and comparison against #611-pinned run manifests paired with hydrodynamic `DiffractionResults` fixtures or extracted result files listed in #612's fixture/config metadata. Implementation is blocked until #500 is landed and #605/#606/#611 APIs are importable on `main`; `status:plan-approved` is not enough for #612 code because it depends on concrete classes/functions. As of 2026-05-15, #605/#606/#611 are still open with only `enhancement` labels, so #612 implementation must fail fast before imports until those dependencies progress and land. It should not implement new numerical solvers, live licensed execution, runner preflight, or duplicate result extraction formulas.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#612` - OPEN - `OrcaWave: add batch mesh convergence and grid sensitivity workflow`; label: `enhancement`.
- `#500` - OPEN - `OrcaWave: mesh file pre-flight validation + auto-copy in runner`; label includes `status:plan-approved`.
- `#605` - OPEN - `OrcaWave: produce self-contained solver packages from spec conversion`; current label at drafting: `enhancement` only, not yet `status:plan-approved`.
- `#606` - OPEN - `OrcaWave: integrate MeshPipeline into spec conversion and runner`; current label at drafting: `enhancement` only, not yet `status:plan-approved`.
- `#611` - OPEN - `OrcaWave: result export and postprocess contract`; current label at drafting: `enhancement` only, not yet `status:plan-approved`.

### Sources consulted

- `AGENTS.md` - digitalmodel declares `PYTHONPATH=src uv run python -m pytest` as the repository test command and points source ownership at `src/digitalmodel/`.
- `docs/plans/` - repo has standalone plan files but no `docs/plans/README.md` index/template; issue #596 explicitly recorded "no `docs/plans/README.md` in this issue", so these plans follow the existing standalone-file convention.
- `src/digitalmodel/hydrodynamics/diffraction/cli.py` - current Click surface includes `convert-aqwa`, `convert-orcawave`, `compare`, `batch`, `convert-spec`, `validate-spec`, `run-orcawave`, `run-aqwa`, `batch-aqwa`, `batch-orcawave`, `plot-raos`, `mesh-build`, and a registered benchmark command; there is no `orcawave-convergence`, given-mesh, or doctor command yet.
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
rg -n "@cli.command" src/digitalmodel/hydrodynamics/diffraction/cli.py
sed -n '1,180p' src/digitalmodel/hydrodynamics/diffraction/spec_converter.py
sed -n '220,560p' src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py
sed -n '1,280p' src/digitalmodel/hydrodynamics/diffraction/mesh_pipeline.py
```

Present-state reproduction to capture before implementation: `diffraction orcawave-convergence --help` should fail or report an unknown command at HEAD, proving no dedicated convergence-study command exists yet. The first TDD commit should lock this missing command before adding the new Click surface.

## Deliverable

A batch study workflow can define OrcaWave mesh/grid variants, prepare/run or dry-run cases, and compare selected hydrodynamic outputs for convergence and sensitivity.

## Proposed Tasks

1. Define a convergence-study YAML schema for mesh variants, grid variants, solver settings, comparison metrics, tolerances, baseline/reference selection, and cardinality limits. The schema must include `max_cases` defaulting to 50; expansion above `max_cases` fails unless an explicit `--allow-large-study` or config flag is set.
2. Add `diffraction orcawave-convergence` for dry-run case planning and comparison-report generation from existing #611 manifests. Licensed execution is out of scope for #612 and should remain a follow-up.
3. Emit deterministic case directories/configs compatible with `OrcaWaveBatchRunner`, but do not call live solve or change `orcawave_batch_runner.py` in #612. The dry-run planner creates #605/#606 packages/manifests and writes batch YAML that is verified two ways: it round-trips through the current `OrcaWaveBatchConfig.from_yaml()` / `BatchJobConfig` models, and a raw YAML key whitelist check proves every emitted top-level/job key is present in the current `OrcaWaveBatchConfig.model_fields` / `BatchJobConfig.model_fields` or an explicitly documented existing accepted alias. Do not rely on Pydantic ignoring extras as proof of compatibility. Each job must include a `spec_path` pointing at the case spec/package input and any existing fields allowed by `BatchJobConfig`; no extra schema fields are added to the batch YAML unless `OrcaWaveBatchConfig` already accepts them. #612 may write a separate `orcawave_convergence_manifest.json` with its own `schema_version` for study metadata. #612 itself stops at dry-run planning and fixture-based comparison.
4. Consume #611 run manifests for artifact paths, status, execution mode, duration, warnings, and missing-artifact warnings only. Hydrodynamic comparison metrics must load `DiffractionResults` JSON/YAML fixtures or extractor outputs explicitly paired to run manifests in the #612 study config or `fixture_manifest.yml`; do not assume #611 manifests contain added-mass/damping/RAO arrays or a postprocessed result path.
5. Add a small typed loader in `src/digitalmodel/hydrodynamics/diffraction/diffraction_results_io.py` before comparison. Current `output_schemas.py` primarily has `to_dict()` methods, so the loader validates the exact `DiffractionResults.to_dict()` shape consumed by #612 without changing the hydrodynamic result schema classes in this issue. Fixture comparison tests must use that loader rather than directory globs.
6. Pin #611-shaped fixtures under `tests/hydrodynamics/diffraction/fixtures/orcawave_convergence/` with integrity metadata in `fixture_manifest.yml`: fixture source commit/PR, landed `orcawave_run_manifest.schema_version` from #611 (not a pre-merge draft literal), consumed result class names and consumed-field-set hashes for axis carriers `FrequencyData` and `HeadingData`, `RAOComponent`, `RAOSet`, `HydrodynamicMatrix`, `AddedMassSet`, `DampingSet`, `HydrostaticResults`, `DiffractionResults`, and the #612-local excitation sidecar schema, artifact paths, result-fixture paths, phase convention, unit system, heading unwrap policy, and regeneration command. Field-set hash algorithm is deterministic: inspect only fields consumed by the #612 loader/comparator; resolve annotations with `typing.get_type_hints(..., include_extras=True)` when possible; serialize annotation as `str(resolved_hint)`, required status as a boolean, default as `"MISSING"` / `"FACTORY:<qualname>"` / canonical JSON scalar where applicable; emit canonical JSON with sorted keys and source-order field order, then `sha256` that UTF-8 payload. Do not pin the whole `output_schemas.py` module hash because unrelated helper edits should not force fixture rebaselines.
7. Implement a dedicated `OrcaWaveCaseComparator` in `orcawave_convergence.py` for case variants. Before writing new metric helpers, inventory `multi_solver_comparator.py` and either import a named public helper that has no solver-pair assumptions or document that no reusable public helper exists and reimplement the small calculation locally with tests. Do not modify `multi_solver_comparator.py` in #612 unless a separate extraction plan is approved.
8. Implement comparison tables/plots for added mass, radiation damping, excitation, RAOs, hydrostatics, and warnings. Because `DiffractionResults` currently has no `excitation` field, #612 defines a local typed excitation fixture/extractor-output sidecar in `diffraction_results_io.py` rather than editing `output_schemas.py`: `ExcitationResults(frequencies, headings, dof_components, magnitude, phase, phase_convention, unit_system)`. Study config/fixture metadata must explicitly pair this sidecar with the #611 run manifest and `DiffractionResults` fixture when `excitation` is requested. If a study requests `excitation` without the sidecar, raise `OrcaWaveConvergenceDependencyError` naming the missing excitation result fixture, not the run manifest.
9. Define grid alignment policy by result type: first require compatible `DiffractionResults.phase_convention`/excitation `phase_convention` and `unit_system` across baseline/cases. #612 does not normalize between `orcina_lag`/`iso_lead`/`unknown` phase conventions or `SI`/`orcaflex` unit systems; mismatches raise `OrcaWaveConvergenceDataError` before numeric comparison. RAO and excitation magnitudes have frequency and heading axes and use linear interpolation onto the baseline frequency/heading grid when in bounds; RAO/excitation phase unwraps along the frequency axis independently for each heading/DOF before frequency interpolation, then applies heading interpolation only when no 0/360 seam crossing is required, and re-wraps to `[-180, 180]`; added mass and radiation damping have frequency axes only and use linear frequency interpolation with no heading interpolation; hydrostatics has no frequency/heading axis and is compared directly as scalar/vector/matrix values; warnings are compared as sets/counts. Extrapolation does not raise; it produces an `UNAVAILABLE` metric cell naming the baseline coordinate outside the case grid. Heading coordinates are normalized to `[0, 360)`; a heading interpolation segment is considered a seam crossing when the absolute normalized endpoint difference is greater than 180 degrees, or when interpolation would require wrapping across 0/360. Seam interpolation is forbidden unless `fixture_manifest.yml` sets `heading_unwrap: true` for that source grid.
10. Add runtime/memory estimates and large QTF/grid warnings using `docs/domains/orcawave/notes/run_time.md` without misrepresenting the notes as a standards model. Compute `case_count`, `frequency_count`, `heading_count`, `linear_grid_size=frequency_count*heading_count`, QTF period-pair count, and QTF direction-pair factor deterministically. Period-pair count is `qtf_period_count ** 2`, where `qtf_period_count` is the number of study periods/frequencies inside an optional QTF period/frequency min/max window after converting to the requested QTF period basis; if no QTF window is configured, use the full `frequency_count`. Direction-pair factor is `1` when `max_qtf_crossing_angle == 0`; when `max_qtf_crossing_angle` is unset, use `heading_count ** 2` as a conservative full ordered-pair count; otherwise normalize headings to `[0, 360)`, compute circular absolute difference `min(abs(a-b), 360-abs(a-b))`, and count ordered heading pairs `(a,b)` including self-pairs where the difference is `<= max_qtf_crossing_angle`. `qtf_combination_count = qtf_period_pair_count * qtf_direction_pair_factor`. `case_count`, `linear_grid_size`, `qtf_combination_count`, and `threads` thresholds are schema-configurable defaults (`warn_case_count`, `warn_linear_grid_size`, `warn_qtf_combination_count`, `warn_threads`) initialized to 20, 500, 2000, and 8 and labeled as #612 internal heuristics. Memory guidance must preserve the notes' strict ordered version/options gates: if OrcaWave version is unknown or older than the notes' stated/latest version, do not emit a numeric safe memory-per-thread estimate; report `UNKNOWN_VERSION_MEMORY` and require operator-supplied override or latest-version confirmation before recommending thread counts. If the latest-version gate is explicit, estimate `13 GB/thread` for defaults; estimate `4.3 GB/thread` only when latest-version and divide-non-planar-panels-off are both explicit; estimate `2.1 GB/thread` only when those gates plus XZ symmetry are explicit; estimate `1.6 GB/thread` only when the preceding gates plus direct-only QTF are explicit. If options are unknown, report the highest applicable gated warning and cite `run_time.md` in the message.
11. Add tests for schema parsing, cardinality cap, dry-run case planning, #500/#605/#606/#611 dependency errors, #611-pinned fixture comparison with explicit result fixture paths, grid interpolation/unavailable behavior by result type, tolerance/outlier behavior, warning/excitation reporting, runtime/memory warnings, and report generation.

## Pseudocode

```text
load_study_config(path):
  parse mesh variants, grid variants, solver settings, metric list, tolerances, max_cases
  expand deterministic case matrix and baseline selection
  fail if case_count > max_cases without explicit large-study override

plan_cases(config):
  require concrete upstream package/preparer APIs from the landed #605/#606 contracts (placeholder names in this plan: OrcaWaveAssetResolver and OrcaWaveMeshPreparer) plus #500 strict preflight reconciliation
  for case in matrix: create case dir and manifest in dry-run mode
  estimate runtime/memory from case count, mesh size, frequency/heading grid, QTF settings, and thread count

compare_cases(config, result_manifests):
  require #611 manifest schema and explicit #612 result fixture/extractor-output paths
  load run metadata/warnings from #611 manifests
  load hydrodynamic arrays from DiffractionResults files through the #612 loader, not the run manifest
  align grids to baseline by result type: RAO freq/heading, added/damping frequency-only, hydrostatics direct
  compute selected metrics from added_mass/damping/excitation/raos/hydrostatics/warnings
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
| Create | `src/digitalmodel/hydrodynamics/diffraction/diffraction_results_io.py` | add typed loader for the consumed `DiffractionResults.to_dict()` fixture shape |
| No change | `src/digitalmodel/hydrodynamics/diffraction/orcawave_batch_runner.py` | live/batch execution handoff is deferred; #612 dry-run planning does not alter existing batch runner behavior |
| No change | `src/digitalmodel/hydrodynamics/diffraction/benchmark_runner.py` | not the #612 comparator home |
| No change | `src/digitalmodel/hydrodynamics/diffraction/multi_solver_comparator.py` | solver-pair semantics do not fit single-solver case variants |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/cli.py` | add `orcawave-convergence` command |
| Create | `tests/hydrodynamics/diffraction/test_orcawave_convergence.py` | TDD coverage |
| Create | `tests/hydrodynamics/diffraction/fixtures/orcawave_convergence/fixture_manifest.yml` | #611/result fixture pin metadata and regeneration command |
| Create | `tests/hydrodynamics/diffraction/fixtures/orcawave_convergence/*.json` | pinned run manifests and `DiffractionResults` fixture data |
| Modify | `docs/domains/orcawave/notes/run_time.md` | link study guidance if needed |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_convergence_study_config_parses_cases` | YAML schema expands cases | mesh/grid study | expected case matrix |
| `test_convergence_study_case_cap_blocks_large_matrix` | cardinality bounded | study expands beyond `max_cases` | clear error unless explicit large-study override is set |
| `test_convergence_study_dry_run_creates_case_dirs` | planning works without license | dry-run study | per-case directories/manifests |
| `test_convergence_study_batch_config_round_trips_with_runner_model` | later execution handoff is loadable | emitted batch YAML from dry-run planner | `OrcaWaveBatchConfig.from_yaml()` loads every `BatchJobConfig` without modifying `orcawave_batch_runner.py` |
| `test_convergence_study_batch_yaml_emits_only_supported_keys` | batch handoff compatibility is not hidden by ignored extra fields | emitted batch YAML plus current `OrcaWaveBatchConfig.model_fields` and `BatchJobConfig.model_fields` | raw YAML top-level/job keys are a subset of supported fields or explicit accepted aliases |
| `test_convergence_study_compares_611_pinned_fixture` | comparison honors real contract shape | pinned #611 manifest plus `DiffractionResults` fixture | convergence table from actual schema fields and fixture pin validated |
| `test_convergence_fixture_pin_detects_schema_drift` | fixture integrity contract works without whole-module churn | fixture metadata with mismatched manifest schema or consumed-field-set hash for axis carriers, top-level, or nested consumed classes | clear rebaseline-required error |
| `test_diffraction_results_fixture_loader_round_trips_consumed_fields` | comparison has a typed result input | JSON/YAML produced from `DiffractionResults.to_dict()` | loader reconstructs/validates RAO, added mass, damping, and hydrostatics fields used by comparator |
| `test_convergence_study_requires_result_contract_for_compare` | dependency is explicit | compare requested without #611 metadata | clear dependency error, no guessed sidecar paths |
| `test_convergence_study_requires_packaging_apis_for_planning` | #500/#605/#606 dependency explicit | importlib/import resolver monkeypatched so a landed package/preparer API import raises `ImportError` | clear dependency error, no duplicate package logic |
| `test_convergence_study_interpolates_rao_to_baseline_grid` | RAO grid sensitivity comparison defined | unequal but overlapping frequency/heading grids | RAO metrics computed on baseline grid via linear interpolation |
| `test_convergence_study_interpolates_added_damping_frequency_only` | non-heading data is not given fake heading axes | unequal frequency grids for added mass/damping | frequency-only interpolation succeeds without heading interpolation |
| `test_convergence_study_compares_hydrostatics_directly` | hydrostatics grid policy is correct | hydrostatics-only fixtures | scalar/vector/matrix differences computed directly |
| `test_convergence_study_unwraps_rao_phase_along_frequency_axis` | circular phase handled correctly | phase values crossing +/-180 degrees by frequency for a fixed heading/DOF | interpolated phase follows frequency-axis unwrap/rewrap policy |
| `test_convergence_study_rejects_phase_convention_mismatch` | RAO phases are not silently compared across conventions | baseline `orcina_lag`, case `iso_lead` or `unknown` | `OrcaWaveConvergenceDataError` naming phase convention mismatch |
| `test_convergence_study_rejects_unit_system_mismatch` | mass/damping/hydrostatic units are not silently mixed | baseline `SI`, case `orcaflex` | `OrcaWaveConvergenceDataError` naming unit system mismatch |
| `test_convergence_study_rejects_heading_seam_interpolation_without_unwrap` | heading periodicity is explicit | heading grid crossing 0/360 without fixture unwrap metadata | `UNAVAILABLE` metric naming seam policy, not a numeric comparison |
| `test_convergence_study_marks_extrapolation_unavailable` | no silent invalid comparison | baseline grid outside case grid | `UNAVAILABLE` metric naming grid bounds |
| `test_convergence_study_compares_excitation_sidecar_when_requested` | issue-required excitation dimension represented | manifest plus `DiffractionResults` fixture plus #612 excitation sidecar | excitation magnitude/phase metrics computed on aligned grid |
| `test_convergence_study_reports_warnings_and_excitation_dependency` | warning and missing-excitation behavior is explicit | manifest warnings and fixture without excitation sidecar | warning deltas reported; requested excitation metric fails with dependency/schema error |
| `test_convergence_study_runtime_memory_warning` | large-study guidance covered | large QTF/grid config with unknown OrcaWave version/options | report warns with `UNKNOWN_VERSION_MEMORY`, no numeric safe estimate, and labels case/grid thresholds as internal heuristics |
| `test_convergence_study_qtf_combination_warning_uses_period_pairs_and_crossing_angle` | QTF warning follows a deterministic count | full-QTF configs with QTF period min/max and `max_qtf_crossing_angle` variants | QTF combination metric equals period-pair count times ordered heading-pair count; `0` crossing angle gives factor 1, unset angle gives `heading_count ** 2`, finite angle counts circular-difference pairs |
| `test_convergence_study_memory_tiers_follow_run_time_notes` | notes constants are not collapsed into full-QTF/other | latest-version configs for defaults, divide-non-planar off, XZ symmetry, and direct QTF | estimates use 13, 4.3, 2.1, and 1.6 GB/thread tiers only when every preceding gate is explicit |
| `test_convergence_fixture_field_hash_is_reproducible` | fixture pin can be regenerated deterministically | consumed result classes and #612 excitation sidecar | sha256 over canonical consumed-field JSON matches fixture manifest across Python versions |
| `test_convergence_study_report_flags_outliers` | report highlights risk | divergent mock values | outlier warning |

## Acceptance Criteria

- [ ] User can define a convergence study in YAML.
- [ ] Study schema expands deterministic mesh/grid case matrices with an enforced `max_cases` cap.
- [ ] Dry-run planning creates one package/run directory per case only through landed/importable #500/#605/#606 APIs, and emits batch YAML that both round-trips through `OrcaWaveBatchConfig.from_yaml()` and passes a raw-key whitelist check against current batch model fields.
- [ ] Comparison report identifies convergence metrics and outliers only when #611 run manifests and explicit #612-pinned `DiffractionResults` plus optional excitation sidecar fixture/extractor-output paths are available; #611 manifests are not treated as hydrodynamic array containers.
- [ ] Added mass, damping, excitation, RAOs, hydrostatics, and warnings are represented in the report; excitation requires the #612 excitation sidecar until the core `DiffractionResults` schema grows a native field.
- [ ] Unequal grids follow the documented result-type policy: RAO/excitation frequency/heading interpolation with frequency-axis phase unwrap, added/damping frequency-only interpolation, hydrostatics direct comparison, heading seam/extrapolation unavailable metric behavior.
- [ ] Phase convention and unit-system mismatches are rejected before numeric comparison; #612 does not silently normalize or compare mixed conventions/units.
- [ ] Runtime/memory estimates or warnings are emitted for large mesh/grid/QTF studies using internal case/grid heuristics, deterministic QTF period-pair and ordered heading-pair combination warnings, and the correct gated `run_time.md` memory tiers without numeric estimates for unknown/older versions.
- [ ] Fixture metadata records the landed #611 manifest schema version and deterministic consumed-field hashes for axis carriers, top-level, and nested consumed result schema classes.
- [ ] Workflow supports dry-run planning on non-licensed hosts.
- [ ] Tests cover batch planning and #611-pinned fixture comparison.
- [ ] Standard Linux tests pass without OrcFxAPI installed; licensed execution remains out of scope.
## Plan Review Gating

- [ ] Completed review artifacts under `/mnt/local-analysis/workspace-hub/digitalmodel/scripts/review/results/` exist for Claude, Codex, and Gemini; each non-empty artifact contains a `## Verdict` section; 0-byte in-progress files do not satisfy this gate.
- [ ] Any provider `MAJOR` finding requires a plan revision and re-review; the issue is commented with this plan and moved to `status:plan-review` only after no unresolved `MAJOR` findings remain.

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | PENDING | Awaiting review artifact |
| Codex | PENDING | Awaiting review artifact |
| Gemini | PENDING | Awaiting review artifact |

**Overall result:** PENDING - do not label `status:plan-review` until artifacts exist and no unresolved `MAJOR` findings remain.

## Risks and Open Questions

- **Risk:** #500/#605/#606/#611 are hard dependencies. Without them, #612 must report dependency errors rather than infer package or result paths.
- **Risk:** #611 run manifests provide artifact metadata, not hydrodynamic arrays. #612 must keep the result fixture/extractor-output path explicit in study/fixture metadata until a later result-index contract exists.
- **Risk:** `run_time.md` constants come from a specific OrcaWave memory note. #612 should surface them as guidance tiers and avoid presenting internal thresholds as vendor limits.
- **Risk:** Batch convergence workflows can be expensive. This plan excludes live licensed execution and keeps normal tests/CI on dry-run and fixture-based comparison only.
- **Risk:** Licensed OrcaWave/OrcFxAPI behavior cannot be fully verified on Linux; tests requiring a license must skip cleanly and be proven on the licensed host where applicable.
- **Open:** Default review policy uses three-provider review; Claude, Codex, and Gemini are the selected review set for this plan.

## Complexity: T2

T2 justification: this plan is deliberately bounded to schema/dry-run planning and fixture-based comparison, with live licensed batch execution deferred until the upstream package/result contracts have landed.
