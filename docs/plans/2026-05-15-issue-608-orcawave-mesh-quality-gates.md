# Plan for #608: OrcaWave: add mesh quality gates before diffraction solve

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/608
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-608-claude.md | scripts/review/results/2026-05-15-plan-608-codex.md | scripts/review/results/2026-05-15-plan-608-gemini.md

---

## Scope

Quality gate scope only. Mesh conversion policy belongs to #606; package layout belongs to #605; missing-file/path-resolution preflight and runner auto-copy belong to #500/#605. #608 implementation is blocked until #500/#605/#606 expose resolved/prepared asset surfaces, because the required gate must run on the same assets that package/solve will use. #608 must not create a second path resolver or duplicate runner preflight behavior; `orcawave-preflight`, `validate-spec --mesh-qa`, `convert-spec --mesh-qa`, `run-orcawave --mesh-qa`, and `batch-orcawave` all call one shared QA engine after resolver/preparer asset collection.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#608` - OPEN - `OrcaWave: add mesh quality gates before diffraction solve`; label: `enhancement`.
- `#500` - OPEN - `OrcaWave: mesh file pre-flight validation + auto-copy in runner`; labels include `enhancement` and `status:plan-approved`.

### Sources consulted

- `AGENTS.md` - digitalmodel declares `PYTHONPATH=src uv run python -m pytest` as the repository test command and points source ownership at `src/digitalmodel/`.
- `docs/plans/` - repo has standalone plan files but no `docs/plans/README.md` index/template; issue #596 explicitly recorded "no `docs/plans/README.md` in this issue", so these plans follow the existing standalone-file convention.
- `src/digitalmodel/hydrodynamics/diffraction/cli.py` - current Click surface includes `convert-aqwa`, `convert-orcawave`, `compare`, `batch`, `convert-spec`, `validate-spec`, `run-orcawave`, `run-aqwa`, `batch-aqwa`, `batch-orcawave`, `plot-raos`, `mesh-build`, and benchmark commands; there is no given-mesh or doctor command yet.
- `src/digitalmodel/hydrodynamics/diffraction/spec_converter.py` - `SpecConverter.convert()` delegates directly to backends and `validate()` checks non-empty mesh strings, frequencies, headings, and positive mass only.
- `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` - runner can generate OrcaWave input, copy existing mesh files, prefer OrcFxAPI, and fall back to dry-run when no API/executable is available.
- `src/digitalmodel/hydrodynamics/diffraction/mesh_pipeline.py` - existing pipeline can load/validate/prepare meshes and maps OrcaWave target format to GDF, but it is not integrated into `SpecConverter` or `OrcaWaveRunner`.
- `src/digitalmodel/hydrodynamics/diffraction/geometry_quality.py` - existing `GeometryQualityChecker` and `GeometryQualityReport` already cover watertightness, normals, panel count, aspect ratios, element sizes, JSON report generation, and public re-export from the diffraction package.
- Related issue `#500` - open plan-approved issue for mesh pre-flight validation and runner auto-copy; these future issues must not duplicate its direct scope.

### Issue-specific code findings

- `MeshPipeline.load()` returns a BEMRosetta `PanelMesh` with `vertices`, `panels`, and `bounding_box`; `MeshPipeline.validate()` delegates to the BEMRosetta handler for a `MeshQualityReport`.
- `MeshQualityReport` dataclass has many fields, but the current `BaseMeshHandler.validate_mesh()` population path reliably fills only `n_panels`, `n_vertices`, panel-area stats, `aspect_ratio_max`, `n_degenerate_panels`, duplicate-vertex count, `has_consistent_normals`, `quality_score`, and warnings. #608 must not rely on default-only fields such as `skewness_max` unless it adds the computation.
- `SpecConverter.validate()` does not load mesh files or inspect mesh quality.
- `OrcaWaveRunner._validate_mesh_references()` only warns about missing packaged mesh files in output, not geometry quality.
- BEMRosetta mesh handlers live under `src/digitalmodel/hydrodynamics/bemrosetta/mesh/`.

### Gaps identified

- No OrcaWave-specific severity model maps existing `GeometryQualityChecker` metrics to blocking errors versus warnings.
- #605, #606, and #609 are open future-work issues and do not yet carry `status:plan-approved`; #608 implementation cannot start until those issues land the required resolver/preparer/selected-auxiliary surfaces.
- No CLI report exists for mesh QA during `validate-spec` or preflight.
- No fixtures cover poor-quality meshes in the OrcaWave workflow.

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

Present-state reproduction to capture before implementation: create a minimal bad-mesh `DiffractionSpec` fixture, then run `diffraction validate-spec bad.yml` and `diffraction run-orcawave bad.yml --dry-run` at HEAD. Expected current behavior is schema/dry-run success or ordinary mesh-reference handling without any geometry-quality finding, because `SpecConverter.validate()` does not load mesh geometry and `OrcaWaveRunner._validate_mesh_references()` only checks packaged reference existence. The first TDD commit must include this failing regression fixture before adding the QA adapter.

## Deliverable

OrcaWave validation/preflight produces blocking mesh errors and non-blocking quality warnings before conversion or solve.

## Proposed Tasks

1. Extend/reuse `GeometryQualityChecker` rather than creating a competing QA implementation. First fix/adapt its quad handling, empty mesh behavior, and noisy output: every panel-indexing path (`check_watertightness()`, `check_aspect_ratios()`, `check_element_sizes()`, edge-length helpers, nonplanarity helpers, and wavelength edge metrics) must drop `-1` sentinel padding before indexing vertices; 3-node and 4-node panels must be handled without indexing past the valid nodes; zero-vertex or zero-valid-panel meshes must short-circuit before any `np.min`/`np.max` calls; and `generate_report(..., verbose=False)` or an explicit quiet `run_checks()` API must support CLI/JSON output with no unsolicited stdout. `orcawave_mesh_quality.py` may use `MeshPipeline.load()` only to obtain a `PanelMesh`; `GeometryQualityReport` must keep existing names `num_nodes`, `num_panels`, `num_edges`, and existing watertight field `is_watertight`, and add only missing metric fields needed by #608 (`degenerate_panel_count`, `max_aspect_ratio`, `element_size_mean`, `element_size_cv`, `p95_panel_edge`, `max_panel_edge`, `quad_nonplanarity_ratio`, `bbox_characteristic_length`) plus a non-numeric `normal_consistency_method` metadata label so policy mapping does not parse formatted issue strings. The OrcaWave JSON finding metric can be named `watertightness` while reading from `is_watertight`; do not add a second `watertight` boolean to the report model. `MeshQualityReport` from `MeshPipeline.validate()` may be included only as supplemental loader/handler context for fields known to be populated.
2. Define `OrcaWaveMeshQAReport` before implementation. Required JSON/report shape:
   - top level: `schema_version`, `spec_path`, `policy`, `status` (`PASS|WARN|FAIL`), `blocking_count`, `warning_count`, `info_count`, `assets`, `findings`, and `source_dependencies`.
   - each asset: `asset_id`, `asset_type`, `source_field`, `path`, `format`, `body_id`, `resolver_status`, and optional `geometry_metrics`.
   - each finding: `asset_id`, `asset_type`, `metric`, `severity` (`INFO|WARN|BLOCK|UNAVAILABLE`), `blocking` bool after policy mapping, `value`, `threshold`, `message`, `recommendation`, and `source` (`geometry_quality|resolver|fdf_validator|policy`).
   - each asset `geometry_metrics` includes `status` (`OK|UNAVAILABLE|STRUCTURAL_ONLY`) separately from finding severity.
   - `UNAVAILABLE` means geometry QA could not run for that asset; under `warn` it is non-blocking with exit code 0 unless the finding came from an upstream resolver/preparer hard error, and under `strict` it is blocking for `orcawave-preflight`, `validate-spec --mesh-qa=strict`, `convert-spec`, and `run-orcawave`.
   Overall `status` is deterministic and shared by every entry point: `FAIL` if any finding has `blocking=true`; otherwise `WARN` if `warning_count > 0`; otherwise `PASS`, including INFO-only reports. All entry points must serialize this exact schema and use the same exit-code policy: `off` returns a PASS report with empty assets/findings and performs no mesh loading; `warn` blocks only `BLOCK` findings and upstream resolver/preparer hard errors while leaving `WARN`, `INFO`, and ordinary QA-loader `UNAVAILABLE` findings non-blocking; `strict` exits nonzero for any finding whose mapped `blocking` is true, including strict-mode `UNAVAILABLE`.
3. Define a concrete OrcaWave severity mapping table. These thresholds are #608 internal engineering heuristics, not standards-derived Orcina limits; implementation must document them as configurable defaults and should not cite them as code/standard requirements without a separate sourced calibration issue:
   - zero vertices or zero panels: `BLOCK` in warn and strict policies.
   - any degenerate/zero-area panel: `BLOCK` in warn and strict policies because structurally unusable panels must stop default package/solve paths.
   - watertightness failure applies only to body meshes and other selected auxiliary meshes whose metadata or asset type declares a closed-surface expectation. Closed-surface failures are `WARN` in warn policy and `BLOCK` in strict policy. Damping lids, free-surface zones, and other explicitly open surfaces skip the watertightness gate or report a non-blocking `INFO`/`WARN` that the check is not applicable; they never block solely for open boundaries.
   - inconsistent normals from the existing mean-normal heuristic: `INFO`/`WARN` only and never `BLOCK`; strict blocking for normals is deferred until a robust closed-surface/adjacency-aware normal consistency method is implemented and tested.
   - panel count >50,000: `WARN`; >100,000: `BLOCK` in strict policy.
   - max aspect ratio >10: `WARN`; >25: `BLOCK` in strict policy.
   - element-size coefficient of variation >0.5: `WARN`; it does not block by default because graded meshes can be legitimate. A configurable `block_on_element_size_cv` policy may make CV >1.0 blocking only when the study/operator opts in.
   - quad non-planarity ratio, computed as the maximum out-of-plane distance divided by maximum quad edge length after dropping `-1` sentinels: >1e-4 `WARN`; >1e-2 `BLOCK` in strict policy. Triangular panels report the metric as not applicable rather than failing.
   - missing units/symmetry metadata: `INFO` unless #606/#609 marks the field required. Known length units are recorded from #606 prepared metadata when present; #608 does not convert mesh coordinates.
   - length-scale sanity: compute `bbox_characteristic_length = max(x_span, y_span, z_span)` on non-empty body/control/damping meshes. If declared length units are missing or unknown, emit `INFO` naming that the scale check is advisory only. If units are known and `bbox_characteristic_length < 1e-3` or `> 1e4` in the declared unit, emit `WARN` as a probable unit/scale error; these configurable defaults never block by default, including under `strict`, because model scale depends on the vessel and study. A future sourced calibration issue may add optional blocking thresholds.
   - waterline/submergence: warning-only in #608.
   - panels-per-wavelength ratio based on `lambda_min / p95_panel_edge` <6: `WARN`; <4: `BLOCK` in strict policy. Also report the single longest edge as a separate outlier metric, but do not use it as the count-threshold denominator.
Missing-file, unsupported-format, and path-resolution errors are not owned by #608; they are consumed from #500/#605/#606 findings. Unsupported QA loaders become per-asset `UNAVAILABLE` findings rather than process crashes.
4. Define panels-per-wavelength guidance: compute the shortest wavelength in the frequency grid. Prefer the existing `FrequencySpec.to_frequencies_rad_s()` path if available; otherwise interpret `FrequencySpec.input_type="frequency"` as angular frequency in rad/s and `"period"` as seconds with `omega = 2*pi/T`. Normalize `EnvironmentSpec.water_depth`: numeric values use the finite-depth branch, and the string `"infinite"` uses the deep-water branch. For finite water depth, solve `omega^2 = g*k*tanh(k*h)` with a bracketing method such as Brent's method over `k > 0`, tolerance `1e-8`, and failure reported as an `INFO` finding naming the frequency/depth that could not be solved. For infinite/deep water use `lambda = 2*pi*g/omega^2`. Estimate panel length from p95 valid panel-edge length after dropping `-1` sentinels and warn when `lambda_min / p95_panel_edge < 6`; this is an internal heuristic and is intentionally not treated as a standards citation. If frequency, water depth, or mesh edge data is unavailable, emit an `INFO` finding naming the missing input.
5. Add waterline/submergence as warning-only in #608 using `VesselGeometry.waterline_z` and `PanelMesh.bounding_box`: with tolerance `max(1e-6, 1e-4 * max(z_span, 1.0))`, warn if `waterline_z < z_min - tol` (mesh entirely above the declared waterline) or `waterline_z > z_max + tol` (mesh entirely below the declared waterline). If `waterline_z` lies inside `[z_min - tol, z_max + tol]`, emit no waterline finding. Do not make this blocking until a later policy issue approves stronger hydrostatic/submergence thresholds.
6. Add a QA runner that operates from #500/#605/#606 resolved/prepared asset manifests and #609 selected-auxiliary metadata. Implementation is dependency-blocked until #500/#605/#606/#609 are merged to `main` and their APIs are importable; their current issue/plan-review state is not enough to start code. Before coding, add failing import/API gate tests for `mesh_preflight` from #500, `OrcaWaveAssetResolver`, `OrcaWaveMeshPreparer`, and `orcawave_auxiliary_assets` selected asset helpers. These are CI/pre-implementation gates, not runtime branches: if any import/API gate fails, stop implementation and revise/re-review #608 rather than shipping fallback behavior. `OrcaWaveMeshQAReport` is the locked public report schema for #608; upstream resolver/preparer fields are adapted into normalized `asset_id`, `source_field`, and `resolver_status` fields. If landed upstream APIs cannot supply those normalized fields without adding a parallel resolver/preparer, revise/re-review #608 before implementation. The compatibility helper for direct `spec.yml` may resolve paths only by delegating to the shared resolver with `spec_path.parent` semantics; it must not implement a parallel resolver or create its own missing-file finding. It emits structured findings with `asset_type`, source path, metric, severity, threshold, and recommendation. Asset collection must include every mesh reference the current backend can emit/copy: body mesh, selected control surface from #609, damping lid, and free-surface panelled-zone mesh.
7. Handle symmetry explicitly. Record raw `num_panels` and an `effective_panel_count` for guidance using multiplier 2 for `xz` or `yz` symmetry and 4 for `xz+yz`; panel-count warnings report both values and make clear which threshold was applied. Symmetry does not change panel edge lengths. Waterline z-bounds are not mirrored by xz/yz symmetry planes, so the waterline check uses raw mesh z-bounds and records the symmetry setting for context.
8. For FDF free-surface assets, consume #606's structural FDF validator result if available. If the validator API is unavailable, return an `UNAVAILABLE` finding for that asset in strict mode rather than implementing an independent FDF parser in #608. Geometric panel metrics use `geometry_metrics.status="STRUCTURAL_ONLY"` for FDF assets because they do not load through the same `PanelMesh` path.
9. Define the shared function signature: `run_orcawave_mesh_qa(spec_path: Path, policy: Literal["off","warn","strict"], output_json: Path | None = None, threshold_overrides: OrcaWaveMeshQAThresholds | None = None, source: Literal["preflight","validate-spec","convert-spec","run-orcawave","batch-orcawave"] = "preflight") -> OrcaWaveMeshQAReport`. All entry points pass through this signature so schema, `off` handling, and threshold configuration cannot diverge silently.
10. Add a dedicated top-level Click command `diffraction orcawave-preflight`, plus `validate-spec --mesh-qa=off|warn|strict`, `convert-spec --mesh-qa=off|warn|strict`, `run-orcawave --mesh-qa=off|warn|strict`, and `batch-orcawave` integration after dependencies land. The runner-level contract is a `RunConfig.mesh_qa` field (policy plus optional JSON report path/threshold overrides) consumed inside `OrcaWaveRunner.prepare()` before solve/package output; `run-orcawave` CLI and `OrcaWaveBatchRunner` both populate the same `RunConfig` field so batch jobs cannot bypass QA by calling `OrcaWaveRunner(config).run(...)` directly. Batch configuration supports a global `run_config.mesh_qa` default plus per-job non-null overrides using the same precedence pattern as other run settings.
11. All entry points call `run_orcawave_mesh_qa(...)` and emit the same JSON schema when a report is requested. Defaults: `validate-spec` remains `off` because it is schema validation; `orcawave-preflight` defaults to `strict`; `convert-spec` and `run-orcawave` default to `warn` so ordinary OrcaWave package/solve paths run pre-solve QA and block structurally unusable geometry by default while keeping stricter heuristic blocking opt-in; `batch-orcawave` defaults from `RunConfig.mesh_qa`, with absent config equivalent to `warn` for OrcaWave jobs. Explicit `--mesh-qa=off` is the documented bypass for compatibility. `convert-spec --solver aqwa` skips OrcaWave mesh QA because it does not emit OrcaWave inputs. `convert-spec --solver orcawave` runs OrcaWave QA before writing OrcaWave output. `convert-spec --solver all` runs OrcaWave QA once before writing any solver output; if the OrcaWave QA result is blocking under the selected policy, no solver output is written, avoiding a partial success report for a multi-solver conversion. `run-orcawave` and `batch-orcawave` QA tests must use dry-run or mocked runner paths and must not require OrcFxAPI on Linux. When `validate-spec` runs with default `off`, emit one concise note only in verbose/help contexts documenting `--mesh-qa=warn|strict` rather than silently implying physical QA ran.
12. Emit machine-readable JSON metadata and concise CLI output. INFO-only reports have overall status `PASS` but must display `info_count` and INFO finding labels in both JSON and human summary; warning-only reports have overall status `WARN` and exit zero unless their mapped `blocking` flag is true.
13. Add valid and invalid mesh fixtures/tests with an explicit fixture matrix: valid triangular GDF, valid quad GDF, degenerate-panel GDF, non-planar quad GDF, high-aspect-ratio GDF, high element-size-CV GDF, implausibly tiny/huge scale GDF plus spec/prepared-asset metadata that declares units outside the GDF file, empty/zero-panel mesh fixture, unsupported/corrupt mesh fixture for loader `UNAVAILABLE`, valid FDF free-surface fixture, invalid FDF panel-count fixture, and a spec fixture with body/control/damping/FSZ assets to prove auxiliary coverage.

## Pseudocode

```text
orcawave_preflight(spec_path, policy, source="standalone"):
  spec = DiffractionSpec.from_yaml(spec_path)
  assets = #500/#605/#606 manifest plus #609 selected-auxiliary metadata; if unavailable, fail the pre-implementation gate
  for asset in assets:
    if missing/path error from resolver -> carry resolver finding; do not duplicate #500/#605 blocking semantics
    if asset is FDF free-surface zone -> run #606 structural FDF validator if available, else UNAVAILABLE; mark geometric metrics STRUCTURAL_ONLY
    try: mesh = MeshPipeline.load(asset.path)
    except (ValueError, MeshError) as exc: record UNAVAILABLE finding for this asset and continue
    if mesh has zero vertices or zero valid panels after dropping -1 sentinels: record BLOCK without calling GeometryQualityChecker.generate_report(); continue
    base_report = GeometryQualityChecker(...).generate_report(str(asset.path), mesh.vertices, mesh.panels, verbose=False)
    findings += map_report_to_orcawave_policy(base_report, policy, asset)
    findings += waterline_submergence_warning_if_applicable(mesh.bounding_box, asset.owning_geometry.waterline_z)
    findings += panels_per_wavelength_guidance(mesh.vertices, mesh.panels, spec.frequencies, spec.environment.water_depth)
    findings += length_scale_sanity(mesh.bounding_box, asset.yaml_length_units or asset.source_units, thresholds)
  write json report and render human summary
  return FAIL if any blocking finding else WARN if warning_count > 0 else PASS
```

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-608-orcawave-mesh-quality-gates.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-608-claude.md` |
| Plan review - Codex (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-608-codex.md` |
| Plan review - Gemini (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-608-gemini.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-608-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `src/digitalmodel/hydrodynamics/diffraction/geometry_quality.py` | reuse/extend existing quality checker and report model |
| Create/modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_mesh_quality.py` | thin OrcaWave policy adapter only; no duplicate geometry calculations |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/spec_converter.py` | call shared QA report from `convert-spec --mesh-qa`; keep `SpecConverter.validate()` returning `list[str]` |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/cli.py` | add top-level `orcawave-preflight` command and `validate-spec`/`convert-spec`/`run-orcawave`/`batch-orcawave` mesh-QA options |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` | add `RunConfig.mesh_qa` and call shared QA report before solve/package output when enabled |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_batch_runner.py` | propagate global/per-job `RunConfig.mesh_qa` so batch uses the same QA gate as single runs |
| Create/modify | `tests/hydrodynamics/diffraction/test_orcawave_mesh_quality.py` | TDD coverage |
| Create/modify | `tests/hydrodynamics/diffraction/fixtures/meshes/` | valid/bad fixtures |
| Modify | `docs/domains/orcawave/README.md` | document preflight command and strict/warn policy |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_orcawave_mesh_qa_valid_mesh_passes` | nominal mesh accepted | known valid GDF | no blocking findings |
| `test_geometry_quality_checker_handles_quad_panels_quietly` | adapter does not crash/noise on common meshes | one quad-panel `PanelMesh` | no `IndexError`; quiet mode produces no unsolicited stdout |
| `test_geometry_quality_checker_element_sizes_handles_quad_panels` | CV metric is not left broken after aspect-ratio fix | one quad-panel `PanelMesh` | `check_element_sizes()` computes metrics without `IndexError` |
| `test_orcawave_mesh_qa_missing_file_uses_resolver_finding` | #608 does not duplicate #500 path preflight | absent mesh path returned by shared resolver | report carries resolver-owned finding; #608 does not create a separate missing-file check |
| `test_orcawave_mesh_qa_degenerate_panels_strict_blocks` | strict policy blocks bad geometry | bad fixture + strict policy | blocking finding |
| `test_orcawave_mesh_qa_degenerate_panels_warn_policy_blocks` | default warn policy blocks structurally unusable panels | bad fixture + warn policy | `BLOCK` finding and nonzero/blocking exit mapping |
| `test_orcawave_mesh_qa_nonplanar_quad_strict_blocks_at_high_threshold` | issue-required non-planar panel QA is planned | non-planar quad fixture + strict policy | `metric=quad_nonplanarity_ratio` finding blocks only past the strict threshold |
| `test_orcawave_mesh_qa_length_scale_sanity_warns_for_implausible_units` | issue-required units/scale sanity is planned | tiny or huge mesh with declared `m` units | `metric=length_scale_sanity` warning names characteristic length and configured threshold |
| `test_orcawave_mesh_qa_missing_units_reports_info_for_scale_check` | units metadata gap is visible without false blocking | mesh with no prepared unit metadata | `metric=length_units` INFO finding and scale sanity does not claim calibrated units |
| `test_orcawave_mesh_qa_policy_overrides_thresholds` | heuristic thresholds configurable | strict/warn policy with custom threshold config | same report metrics map to expected severity |
| `test_orcawave_mesh_qa_off_policy_returns_empty_pass_report` | `off` is part of the shared API rather than a divergent CLI bypass | any valid spec with `policy="off"` | report has `policy=off`, `status=PASS`, empty assets/findings, and no mesh loader call |
| `test_orcawave_mesh_qa_unavailable_blocks_only_in_strict` | `UNAVAILABLE` severity has deterministic exit policy | corrupt/unsupported mesh fixture | warn policy non-blocking, strict policy blocking |
| `test_validate_spec_default_off_reports_no_physical_qa_in_verbose_help_only` | schema validation does not imply physical QA | `validate-spec` with default options plus verbose/help coverage | default command does not run mesh QA; verbose/help text names `--mesh-qa=warn|strict` |
| `test_orcawave_mesh_qa_empty_mesh_short_circuits_before_geometry_checker` | zero-size geometry does not crash | empty/zero-panel fixture | `BLOCK` finding and no `np.min`/`np.max` crash |
| `test_orcawave_mesh_qa_ignores_negative_one_panel_padding` | quad/triangle padding does not use `vertices[-1]` | triangle rows padded with `-1` | wavelength and edge metrics ignore sentinel entries |
| `test_orcawave_mesh_qa_normals_heuristic_never_blocks` | closed-body normal heuristic cannot false-block strict QA | closed or symmetric fixture with mean-normal ambiguity | normal finding is INFO/WARN, not blocking |
| `test_orcawave_mesh_qa_symmetry_effective_panel_count_reported` | symmetry is visible in panel guidance | spec with `xz` or `xz+yz` symmetry | report includes raw and effective panel count with documented multiplier |
| `test_validate_spec_mesh_qa_reports_findings` | CLI integration | spec + bad mesh and `--mesh-qa` | human-readable issue list and nonzero only for blocking findings |
| `test_orcawave_preflight_writes_json_report` | machine-readable output | spec + valid mesh | JSON report contains asset type, metric, severity, and path |
| `test_orcawave_preflight_warning_only_status_is_warn` | top-level status is not ambiguous | mesh with a non-blocking warning only | report status is `WARN`, `warning_count > 0`, and exit code remains zero |
| `test_orcawave_preflight_human_summary_shape` | human-readable output stable | spec + mixed findings | summary contains PASS/WARN/FAIL counts and severity labels |
| `test_orcawave_preflight_covers_auxiliary_mesh_assets` | aux assets cannot bypass QA | resolved assets for body, selected control-surface, damping-lid, and QTF FSZ references | JSON report contains one asset entry per emitted/copyable mesh reference; FDF FSZ is structural-only, not silently skipped |
| `test_orcawave_mesh_qa_waterline_above_or_below_mesh_warns` | concrete datum is used without over-blocking | geometry with `waterline_z` outside mesh z-bounds by tolerance | warning finding; not blocking |
| `test_orcawave_mesh_qa_panels_per_wavelength_guidance` | issue guidance covered | frequency grid + water depth + mesh dimensions | finding uses `lambda_min / p95_panel_edge < 6` warning threshold or INFO naming unavailable input; longest edge is reported separately as an outlier metric |
| `test_orcawave_mesh_qa_frequency_units_period_and_rad_per_second` | wavelength guidance uses declared frequency input type | one frequency case and one period case with known depth | computed wavelength guidance matches rad/s and period conversions |
| `test_orcawave_mesh_qa_infinite_water_depth_uses_deep_water_branch` | string water-depth branch does not crash | `EnvironmentSpec.water_depth="infinite"` | wavelength guidance uses `lambda = 2*pi*g/omega^2` without finite-depth solver |
| `test_convert_spec_blocks_on_mesh_qa_failure` | conversion path cannot bypass enabled QA | `convert-spec --solver orcawave --mesh-qa=strict` with bad resolved mesh | no package/YAML output and blocking finding shown |
| `test_convert_spec_solver_aqwa_skips_orcawave_mesh_qa` | AQWA conversion remains scoped to AQWA | `convert-spec --solver aqwa --mesh-qa=strict` with an OrcaWave-only bad mesh condition | OrcaWave QA is not called and AQWA behavior is unchanged |
| `test_convert_spec_solver_all_blocks_before_any_output_on_orcawave_qa_failure` | all-solver conversion does not produce partial output after OrcaWave BLOCK | `convert-spec --solver all --mesh-qa=warn` with zero-panel OrcaWave mesh | no solver output is written and the QA finding is reported once |
| `test_run_orcawave_blocks_on_mesh_qa_failure` | solve path cannot bypass enabled QA | `run-orcawave --mesh-qa=strict` with bad resolved mesh | runner stops before solve output |
| `test_runconfig_mesh_qa_drives_runner_without_cli` | library callers cannot bypass QA accidentally | `OrcaWaveRunner(RunConfig(mesh_qa=warn)).run(...)` with bad resolved mesh | QA runs before package/solve output and blocks structural blockers |
| `test_batch_orcawave_uses_runconfig_mesh_qa` | batch path cannot bypass single-run QA gate | batch config with global `run_config.mesh_qa=warn` and bad mesh job | batch report marks the job failed/blocked before solve output |
| `test_batch_orcawave_job_override_mesh_qa_precedence` | batch per-job overrides are deterministic | global strict with one job override `off`, plus another defaulted job | first job skips QA, second job runs strict QA |
| `test_convert_spec_and_run_orcawave_default_warn_blocks_structural_blockers` | issue-required solve/package gates run without explicit strict flag | ordinary `convert-spec --solver orcawave` and `run-orcawave` with zero-panel mesh | default `warn` QA runs and stops before output because zero-panel geometry maps to `BLOCK` |
| `test_orcawave_mesh_qa_invalid_fdf_panel_count_warn_and_strict_policy` | FDF fixture matrix is exercised without ambiguous outcomes | invalid FDF panel-count fixture through #606 validator seam, parametrized over warn/strict | report carries validator finding; warn is non-blocking unless #606 marks it a hard resolver/preparer error, strict is blocking |
| `test_qa_entrypoints_share_report_schema` | CLI behavior cannot diverge | same bad mesh through `orcawave-preflight`, `validate-spec --mesh-qa=strict`, `convert-spec --mesh-qa=strict`, and `run-orcawave --mesh-qa=strict` | same JSON finding schema and matching severity/exit policy across all four entry points |

## Acceptance Criteria

- [ ] Mesh QA runs through `diffraction orcawave-preflight`, `validate-spec --mesh-qa=warn|strict`, `convert-spec`, `run-orcawave`, and `batch-orcawave` using the same shared report schema; `policy=off` returns an empty PASS report through the same API.
- [ ] Blocking geometry-quality errors prevent misleading preflight/conversion/solve success output by default for `convert-spec`, `run-orcawave`, and `batch-orcawave` under `warn` when findings map to `BLOCK`, and under `strict` for stricter heuristic blockers; file/path/package blocking remains delegated to #500/#605/#606.
- [ ] Non-blocking warnings are reported distinctly from hard failures.
- [ ] Given a bad fixture, JSON findings expose machine fields such as `metric=max_aspect_ratio`, numeric `value`, and `threshold`; policy code does not parse formatted issue strings.
- [ ] Empty meshes, `-1` padded panels, `"infinite"` water depth, symmetry multipliers, INFO-only reports, and invalid FDF validator output are covered by observable tests.
- [ ] `UNAVAILABLE` findings are non-blocking under `warn` unless they carry an upstream resolver/preparer hard-error origin, and blocking under `strict`; all entry points share that exit-code mapping.
- [ ] Non-planar quad findings and units/length-scale sanity findings are computed as first-class metrics, exposed in JSON, and covered by tests rather than treated as prose-only warnings.
- [ ] Tests include the explicit fixture matrix from task 13, including valid, invalid/poor-quality, non-planar, implausible-scale, unsupported/corrupt, FDF, and auxiliary-asset meshes.
- [ ] Package/solve integration uses #500/#605/#606 resolved/prepared asset APIs and #609 selected-auxiliary metadata; #608 does not introduce a circular dependency or a parallel resolver/preparer.
- [ ] `RunConfig.mesh_qa`, single-run CLI flags, and batch global/per-job config feed the same runner-level QA gate, so `OrcaWaveRunner(config).run(...)` cannot bypass enabled QA.
- [ ] `convert-spec --solver aqwa`, `orcawave`, and `all` have explicit tested mesh-QA scoping; `all` writes no partial solver outputs when OrcaWave QA blocks.
- [ ] `OrcaWaveMeshQAReport` remains the locked #608 public report schema; upstream asset shapes are normalized into it or the plan is revised/re-reviewed before implementation.
- [ ] Pre-implementation import/API gates pass against `main` for #500/#605/#606/#609 surfaces before code changes begin, or this plan is revised to match the landed APIs.
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

- **Risk:** QA must run on the same resolved/prepared asset paths that package/solve will use. #608 implementation is blocked until #500/#605/#606 make those paths available and #609 selects auxiliary assets; duplicating path resolution outside those issues would let QA pass a different file than OrcaWave receives.
- **Risk:** The quality thresholds in this plan are internal defaults. They should be configurable and documented as heuristics until a separate sourced calibration/standards issue replaces them.
- **Risk:** Licensed OrcaWave/OrcFxAPI behavior cannot be fully verified on Linux; tests requiring a license must skip cleanly and be proven on the licensed host where applicable.
- **Open:** Default review policy uses three-provider review; Claude, Codex, and Gemini are the selected review set for this plan.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
