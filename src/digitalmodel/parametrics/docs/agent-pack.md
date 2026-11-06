# Parametrics knowledge pack

| Header | Value |
|---|---|
| Module | parametrics |
| Repository | digitalmodel |
| Package | `src/digitalmodel/parametrics` |
| Pack revision | 0 |
| Code revision | `09089a121a16069c4b657d1a320a630a23ab1dd5` |
| Analysis family | Engineering calculations (EC); AQG r0, section 10, Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

`run_sweep` expands a supplied study through `generate_case_configs`, calls a runner for each case, and retains exceptions as failed results instead of aborting the sweep. [src/digitalmodel/parametrics/sweep.py:L24-L43]
The package re-exports the existing `ParameterSweep`, `ParametricStudy`, `CaseResult` and `ParametricResultsSummary` types; its routing maturity is development. [src/digitalmodel/parametrics/__init__.py:L18-L34] [docs/registry/module-routing.yaml:L696-L701]
The three pilots prepare padeye APDL inputs, prepare diffraction specifications, and delegate a watch-circle calculation to an analytical workflow; preparation status is distinct from completed calculation status. [src/digitalmodel/parametrics/pilots.py:L33-L70] [src/digitalmodel/parametrics/pilots.py:L76-L122] [src/digitalmodel/parametrics/pilots.py:L128-L180]
`atlas_from_summary` adapts completed sweep results to the separate `parametric.Atlas` implementation; interpolation and query serving remain outside this package. [src/digitalmodel/parametrics/atlas_bridge.py:L15-L19] [src/digitalmodel/parametrics/atlas_bridge.py:L39-L80] [tests/parametrics/test_atlas_serving.py:L12-L15]
The delegated watch-circle calculation is a preliminary feasibility screen requiring subsequent dynamic-cable analysis; generated padeye text does not establish solved stress or mesh convergence. [src/digitalmodel/orcaflex/fowt_mooring_workflow.py:L1-L8] [src/digitalmodel/ansys/padeye.py:L86-L90]

## 2. Inputs and outputs

Input ranges below describe explicit example grids, not qualified physical envelopes; generic sweep values are explicit lists or start/stop/step sequences. [tests/parametrics/test_pilots.py:L16-L51] [src/digitalmodel/orcaflex/batch_parametric.py:L28-L51]

| Input | Unit | Range or constraint | Default | Source |
|---|---|---|---|---|
| `study`, `case_runner` | mixed; runner-defined | Callable receives expanded configuration including case ID | Required | [src/digitalmodel/parametrics/sweep.py:L19-L34] |
| `thicknesses_mm`, `hole_diameters_mm` | mm | No local bounds; geometry validation delegated | Thickness 30, 40, 50; diameter 70, 90 | [src/digitalmodel/parametrics/pilots.py:L33-L59] [src/digitalmodel/ansys/padeye.py:L56-L77] |
| `water_depths_m`, `repo_root` | m; directory | Base specification and relative mesh must exist; root defaults to current directory | Depth 100, 250, 500 | [src/digitalmodel/parametrics/pilots.py:L76-L111] |
| `watch_circle_radii_m`, `mbr_limits_m` | m | Passed to analytical workflow | Radius 15, 25, 35; MBR 4.5 | [src/digitalmodel/parametrics/pilots.py:L128-L164] |
| `cable` | m for geometric fields | False-valued mapping uses embedded cable; supplied mapping is otherwise used directly | Suspended length 320; elevation 90; horizontal span 260; MBR 4.5; hybrid mooring; semi floater | [src/digitalmodel/parametrics/pilots.py:L139-L164] |
| `response`, `basename`, `atlas_id` | field name; identifiers | Response must be a result column; at least one parameter required | Required | [src/digitalmodel/parametrics/atlas_bridge.py:L22-L48] |
| `physics`, `scales` | dimensionless transform labels | Sorted grids; missing scale labels use linear | `linear`, `None` | [src/digitalmodel/parametrics/atlas_bridge.py:L29-L30] [src/digitalmodel/parametrics/atlas_bridge.py:L50-L58] |

Output conventions are those of the delegated calculation: the padeye generator states mm/N/MPa, a vertical sling angle toward positive X, and a fixed bottom edge at y=0; this package does not transform coordinates. [src/digitalmodel/ansys/padeye.py:L28-L44] [src/digitalmodel/ansys/padeye.py:L81-L94] [src/digitalmodel/ansys/padeye.py:L118-L133] [src/digitalmodel/parametrics/pilots.py:L54-L59]
The diffraction pilot copies the source mesh and replaces only the environment water depth before writing YAML; frame, phase and geometry conventions must therefore be taken from that specification and its consumer. [src/digitalmodel/parametrics/pilots.py:L85-L119]

| Output | Unit or format | Meaning and default | Source |
|---|---|---|---|
| Sweep summary | `ParametricResultsSummary` | One appended result per expanded case; failed results retain exception type and text | [src/digitalmodel/parametrics/sweep.py:L28-L43] |
| Padeye input | APDL `.inp`; mm/N/MPa | Per-case file; `prepared`, path in notes | [src/digitalmodel/parametrics/pilots.py:L54-L68] [src/digitalmodel/ansys/padeye.py:L86-L94] |
| Diffraction input | YAML plus mesh; depth m | Per-case `spec.yml`; `prepared` only when converter issues are empty | [src/digitalmodel/parametrics/pilots.py:L100-L120] |
| `min_clearance_m` in watch-circle results | m | Cable bend-radius margin rounded to 3 places; this field is not seabed clearance in this pilot | [src/digitalmodel/parametrics/pilots.py:L164-L177] |
| `max_utilisation` in watch-circle results | 1 | MBR limit / governing bend radius, rounded to 4 places; `None` for zero governing radius | [src/digitalmodel/parametrics/pilots.py:L166-L177] |
| Atlas | In-memory object | Completed response grid, axes, identity and study provenance; validation error is set to zero without a holdout calculation | [src/digitalmodel/parametrics/atlas_bridge.py:L39-L80] |

## 3. Method and governing standards

| Method | Implementation and basis | Applicability |
|---|---|---|
| Full-factorial expansion | `run_sweep` delegates to `ParametricStudy.generate_case_configs`, whose matrix uses `itertools.product` | Parameter combinations, not a physical response equation. [src/digitalmodel/parametrics/sweep.py:L28-L34] [src/digitalmodel/orcaflex/batch_parametric.py:L78-L111] |
| Fail-soft collection | Exceptions become `CaseResult(status="failed")`, then append to the summary | Consumers must inspect statuses. [src/digitalmodel/parametrics/sweep.py:L33-L43] |
| Input generation | `ansys_padeye_sweep.runner` calls `write_padeye_inp`; `orcawave_depth_sweep.runner` copies mesh, updates depth and invokes `SpecConverter.validate` | Text preparation only. [src/digitalmodel/parametrics/pilots.py:L54-L70] [src/digitalmodel/parametrics/pilots.py:L100-L122] |
| Watch-circle response | `orcaflex_fowt_watch_circle_sweep.runner` delegates to `FOWTMooringWorkflow.router`; only the utilisation quotient and rounding are local | Not an independent hand-check reference. [src/digitalmodel/parametrics/pilots.py:L157-L180] [src/digitalmodel/orcaflex/fowt_mooring_workflow.py:L28-L42] |
| Atlas adaptation | Filter completed cases, sort each parameter grid, require expected row count and nonmissing response, then construct `Atlas` | Count equality is not unique Cartesian membership; see #2118. [src/digitalmodel/parametrics/atlas_bridge.py:L39-L80] |

The delegated cable calculation emits a reference to DNV-RP-0360, *Subsea power cables in shallow water*, revision 2016, clause title “Dynamic cable minimum bend radius (MBR)”; clause text not checked against the licensed copy. [src/digitalmodel/orcaflex/mooring_design_fowt.py:L61-L69] [src/digitalmodel/orcaflex/mooring_design_fowt.py:L277-L285]
Its `Citation` carries `source_sibling="generic"` and is validated before return when a wiki is configured; a missing page raises `CitationResolutionError`. [src/digitalmodel/orcaflex/mooring_design_fowt.py:L277-L304]
No standards-derived coefficient is introduced by the harness: its local arithmetic comprises factorial counts, a response quotient and rounding, while numerical physics is delegated. [src/digitalmodel/parametrics/sweep.py:L24-L43] [src/digitalmodel/parametrics/atlas_bridge.py:L50-L80] [src/digitalmodel/parametrics/pilots.py:L164-L177]

## 4. Validated worked examples

Validated physical examples: 0; all 12 executable examples below are regression tests or synthetic harness checks, with no independently sourced physical response oracle. [tests/parametrics/test_sweep.py:L23-L54] [tests/parametrics/test_pilots.py:L16-L51] [tests/parametrics/test_atlas_bridge.py:L37-L82] [tests/parametrics/test_atlas_serving.py:L56-L77]

Offline command: `PYTHONPATH=src uv run --no-sources python -m pytest tests/parametrics/ -q -rA -p no:cacheprovider --basetemp=<scratch>`, at the code pin on the uncommitted tree; BASE: 12 run, 4 PASS, 8 FAIL, 0 SKIP, 0 NOT_RUN. Results after documentation changes are recorded in section 10.

The FOWT runner records `CitationResolutionError` for `DNV-RP-0360`, reason `page_missing`; the bridge then reports `ValueError: response 'max_utilisation' not in result columns` because no completed response survives filtering. This is the configured missing-page path, not a solver requirement. [src/digitalmodel/orcaflex/mooring_design_fowt.py:L277-L304] [src/digitalmodel/parametrics/sweep.py:L33-L43] [src/digitalmodel/parametrics/atlas_bridge.py:L39-L48]

| Case / test node ID | Input and oracle; tolerance basis | Kind | Offline result BASE / AFTER |
|---|---|---|---|
| `tests/parametrics/test_atlas_bridge.py::test_atlas_builds_with_two_axes` | Radius × MBR, 3×3 grid; 2 named axes, 9 rows; exact counts. [tests/parametrics/test_atlas_bridge.py:L37-L41] | Regression | FAIL (citation dependency; downstream fixture failure where applicable) / FAIL (same citation dependency) |
| `tests/parametrics/test_atlas_bridge.py::test_predict_at_grid_node_matches_swept_value` | Radius 25 m, MBR 5 m; prediction equals swept utilisation with relative tolerance 1e-6 (unit 1), fixture tolerance. [tests/parametrics/test_atlas_bridge.py:L44-L50] | Regression | FAIL (citation dependency; downstream fixture failure where applicable) / FAIL (same citation dependency) |
| `tests/parametrics/test_atlas_bridge.py::test_predict_interpolates_interior_point` | Radius 20 m, MBR 4.5 m; prediction strictly between MBR 4 and 6 m responses (unit 1). [tests/parametrics/test_atlas_bridge.py:L53-L60] | Regression | FAIL (citation dependency; downstream fixture failure where applicable) / FAIL (same citation dependency) |
| `tests/parametrics/test_atlas_bridge.py::test_predict_out_of_range_is_gated` | Radius 80 m outside grid; in_range=False and reason names axis; 0 accepted out-of-range points. [tests/parametrics/test_atlas_bridge.py:L63-L67] | Regression | FAIL (citation dependency; downstream fixture failure where applicable) / FAIL (same citation dependency) |
| `tests/parametrics/test_atlas_bridge.py::test_atlas_from_summary_rejects_unknown_response` | Unknown result field; 1 ValueError with matching text expected. [tests/parametrics/test_atlas_bridge.py:L70-L82] | Regression | FAIL (citation dependency; downstream fixture failure where applicable) / FAIL (same citation dependency) |
| `tests/parametrics/test_atlas_serving.py::test_in_range_query_serves_utilisation` | Radius 20 m, MBR 4.5 m; in_range=True, utilisation response, pass status and positive value (unit 1). [tests/parametrics/test_atlas_serving.py:L56-L66] | Regression | FAIL (citation dependency; downstream fixture failure where applicable) / FAIL (same citation dependency) |
| `tests/parametrics/test_atlas_serving.py::test_out_of_range_query_escalates` | Radius 80 m, MBR 5 m; in_range=False, reason identifies radius; 0 accepted points. [tests/parametrics/test_atlas_serving.py:L69-L77] | Regression | FAIL (citation dependency; downstream fixture failure where applicable) / FAIL (same citation dependency) |
| `tests/parametrics/test_pilots.py::test_ansys_padeye_sweep_prepares_inp_per_case` | Thickness {30,40,50} mm × diameter {70,90} mm; 6 prepared inputs containing PLANE182; exact count/content. [tests/parametrics/test_pilots.py:L16-L25] | Regression | PASS / PASS |
| `tests/parametrics/test_pilots.py::test_orcawave_depth_sweep_prepares_validated_specs` | Depth {100,250,500} m; 3 prepared spec.yml files; exact count/status/name. [tests/parametrics/test_pilots.py:L28-L38] | Regression | PASS / PASS |
| `tests/parametrics/test_pilots.py::test_orcaflex_fowt_sweep_returns_real_results` | Radius {15,25,35} m; 3 completed results, increasing utilisation and critical radius 35 m; exact counts and ordering. [tests/parametrics/test_pilots.py:L41-L51] | Regression | FAIL (citation dependency; downstream fixture failure where applicable) / FAIL (same citation dependency) |
| `tests/parametrics/test_sweep.py::test_run_sweep_covers_full_factorial_matrix` | Synthetic axes a={1,2}, b={10,20,30}; 6 cases, largest a*b=60 (unit 1); exact fixture assertions. [tests/parametrics/test_sweep.py:L23-L39] | Regression | PASS / PASS |
| `tests/parametrics/test_sweep.py::test_run_sweep_is_fail_soft_per_case` | Same grid; 1 forced failure, 5 completed; exception text retained; exact counts (cases). [tests/parametrics/test_sweep.py:L42-L54] | Regression | PASS / PASS |

## 5. QA gates

AQG r0, section 10, Python calculations supplies G0 inputs/units/citations, G1 imports/tests and G2 hand/published checks; G3 and G4 are NOT_APPLICABLE for this family, and section 9 requires G5 evidence.
Checks below prescribe reproducible acceptance criteria; a failed baseline response check is retained as FAIL, never treated as a physical validation pass. [tests/parametrics/test_pilots.py:L41-L51]

| Gate | Applicability | Checks |
|---|---|---|
| G0 | Applicable: sweep cardinality, source input and status checks | C01, C03, C04. [tests/parametrics/test_sweep.py:L23-L39] [tests/parametrics/test_pilots.py:L16-L38] |
| G1 | Applicable: import and offline execution | C02, C05, C06. The padeye route writes text only, the diffraction route uses `SpecConverter.validate`, and the FOWT route calls an analytical workflow; no target-program load or solver-version qualification is established. [src/digitalmodel/parametrics/pilots.py:L54-L70] [src/digitalmodel/parametrics/pilots.py:L100-L122] [src/digitalmodel/parametrics/pilots.py:L157-L180] |
| G2 | Applicable: synthetic hand arithmetic and grid reproduction | C07, C08. [tests/parametrics/test_sweep.py:L23-L39] [tests/parametrics/test_atlas_bridge.py:L44-L50] |
| G3 | NOT_APPLICABLE: Python calculations family; no dynamic solve is performed by these pilot runners | AQG r0, section 10; [src/digitalmodel/parametrics/pilots.py:L54-L70] [src/digitalmodel/parametrics/pilots.py:L100-L122] [src/digitalmodel/parametrics/pilots.py:L157-L180] |
| G4 | NOT_APPLICABLE: Python calculations family; spec preparation is not a diffraction solution | AQG r0, section 10; [src/digitalmodel/parametrics/pilots.py:L100-L122] |
| G5 | Applicable: evidence and provenance accounting | C09; AQG r0, section 9. [src/digitalmodel/parametrics/atlas_bridge.py:L70-L80] |

| Check | Executable route | Numeric pass criterion and unit | Evidence / baseline result | Gate |
|---|---|---|---|---|
| C01 | Section-4 full-factorial matrix node | 6 results, 0 noncompleted cases (cases) | Test log / PASS. [tests/parametrics/test_sweep.py:L23-L39] | G0 |
| C02 | Section-4 fail-soft node | 1 failed case, 5 completed cases; 1 retained exception substring (cases, occurrences) | Test log / PASS. [tests/parametrics/test_sweep.py:L42-L54] | G1 |
| C03 | Section-4 padeye node | 6 prepared files, each with at least 1 PLANE182 token (files, tokens) | Test log and temporary APDL / PASS. [tests/parametrics/test_pilots.py:L16-L25] | G0 |
| C04 | Section-4 diffraction node | 3 prepared specification files, 0 failed statuses (files, cases) | Test log and temporary YAML / PASS. [tests/parametrics/test_pilots.py:L28-L38] | G0 |
| C05 | Section-4 FOWT node | 3 completed cases, 0 missing utilisations, critical radius=35 m; first utilisation < last (unit 1) | Test log / FAIL: citation page_missing. [tests/parametrics/test_pilots.py:L41-L51] | G1 |
| C06 | Run all 12 section-4 nodes | 12 passed tests, 0 failed tests (tests) | Test log / FAIL: 4 passed, 8 failed. [tests/parametrics/test_sweep.py:L23-L54] [tests/parametrics/test_pilots.py:L16-L51] [tests/parametrics/test_atlas_bridge.py:L37-L82] [tests/parametrics/test_atlas_serving.py:L56-L77] | G1 |
| C07 | Hand-check the synthetic runner oracle: 2×3=6 cases; max({1,2}×{10,20,30})=60; compare section-4 full-factorial node | Absolute differences=0 cases and 0 (unit 1) | Test log / PASS; harness arithmetic only. [tests/parametrics/test_sweep.py:L13-L39] | G2 |
| C08 | Section-4 grid-node prediction test | Relative error <=1e-6 (unit 1) at radius 25 m, MBR 5 m | Test log / FAIL before prediction: fixture cannot build. [tests/parametrics/test_atlas_bridge.py:L44-L50] | G2 |
| C09 | Inspect every returned atlas validation/provenance record and require holdout basis before interpreting `max_rel_error` as measured | 0 unsupported error assertions; 1 source and 1 study key per atlas (assertions, keys) | Static inspection / FAIL: literal zero error with no holdout computation; #2118. [src/digitalmodel/parametrics/atlas_bridge.py:L70-L80] | G5 |

## 6. Known failure modes and pitfalls

sweep: 21 found

The count denotes reviewed sites, including benign defaults and unit conventions, rather than raw matching lines; the sweep covers all four package Python files and excludes tests from the site count. [src/digitalmodel/parametrics/__init__.py:L1-L34] [src/digitalmodel/parametrics/sweep.py:L1-L43] [src/digitalmodel/parametrics/pilots.py:L1-L180] [src/digitalmodel/parametrics/atlas_bridge.py:L1-L80]

Reproduction commands (text inspection only):

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05' src/digitalmodel/parametrics --glob '*.py'
rg -n 'float\(|int\(|unit|scale|round\(|or \{|or \[|validation|status|write|cache' src/digitalmodel/parametrics --glob '*.py'
```

| Site | Symptom / cause | Detection and disposition | Evidence at code pin |
|---|---|---|---|
| S01 | Broad exception capture permits callers to overlook failed cases | C02 confirms retention; C06 rejects incomplete success; intentional fail-soft behavior | [src/digitalmodel/parametrics/sweep.py:L33-L43] |
| S02 | Failed/noncompleted rows are filtered from atlas input | C06 exposes the failed FOWT fixture; no dedicated all-failed-grid detection check exists; issue pending (1) | [src/digitalmodel/parametrics/atlas_bridge.py:L39-L48] |
| S03 | Expected-grid accumulator starts at 1 (dimensionless product identity), not a missing-data fallback | C01/C07 check product size; benign initializer | [src/digitalmodel/parametrics/atlas_bridge.py:L50-L60] |
| S04 | Absent or empty `scales` uses an empty mapping | no detection check exists for unintended transform selection; issue pending (1) | [src/digitalmodel/parametrics/atlas_bridge.py:L52-L57] |
| S05 | Missing per-axis scale silently selects linear | no detection check exists for omitted logarithmic intent; issue pending (1) | [src/digitalmodel/parametrics/atlas_bridge.py:L54-L58] |
| S06 | `physics` defaults to linear | no detection check exists for unsuitable response transform; issue pending (1) | [src/digitalmodel/parametrics/atlas_bridge.py:L22-L31] |
| S07 | `max_rel_error` is always set to 0.0, not measured | C09 detects unsupported evidence; #2118 | [src/digitalmodel/parametrics/atlas_bridge.py:L70-L80] |
| S08 | Omitted root uses current directory, so template discovery is working-directory dependent | C04 checks explicit-root success; no detection check exists for wrong implicit root; issue pending (1) | [src/digitalmodel/parametrics/pilots.py:L85-L89] |
| S09 | Padeye axes are labelled mm without local conversion | C03 checks file generation only; no detection check exists for caller unit mistakes; issue pending (1) | [src/digitalmodel/parametrics/pilots.py:L43-L59] |
| S10 | Depth axis is labelled m without local conversion | C04 checks file generation only; no detection check exists for caller unit mistakes; issue pending (1) | [src/digitalmodel/parametrics/pilots.py:L92-L109] |
| S11 | Radius and MBR axes are labelled m without local conversion | C05 checks trend only; no detection check exists for unit mismatch; issue pending (1) | [src/digitalmodel/parametrics/pilots.py:L147-L164] |
| S12 | `cable or {...}` replaces an empty caller mapping with embedded example geometry | no detection check exists; issue pending (1) | [src/digitalmodel/parametrics/pilots.py:L139-L146] |
| S13 | Governing radius converted using `float()` | Internal workflow result, not locale-aware external-text parsing; no detection check exists for nonfinite values; issue pending (1) | [src/digitalmodel/parametrics/pilots.py:L164-L166] |
| S14 | MBR limit converted using `float()` | Internal workflow result; no detection check exists for nonfinite values; issue pending (1) | [src/digitalmodel/parametrics/pilots.py:L164-L167] |
| S15 | Margin converted using `float()` | Internal workflow result; no detection check exists for nonfinite values; issue pending (1) | [src/digitalmodel/parametrics/pilots.py:L164-L175] |
| S16 | Margin rounded to 3 decimal places in m | no detection check exists for loss near a decision boundary; issue pending (1) | [src/digitalmodel/parametrics/pilots.py:L174-L177] |
| S17 | Utilisation quotient rounded to 4 decimal places (unit 1) | no detection check exists for threshold effects; issue pending (1) | [src/digitalmodel/parametrics/pilots.py:L166-L177] |
| S18 | Zero governing radius gives `None` utilisation while status remains completed | no detection check exists; issue pending (1) | [src/digitalmodel/parametrics/pilots.py:L174-L177] |
| S19 | Bend margin is stored in a field normally documented as seabed clearance | no detection check exists for semantic misuse; issue pending (1) | [src/digitalmodel/parametrics/pilots.py:L174-L177] [src/digitalmodel/orcaflex/batch_parametric.py:L161-L165] |
| S20 | Equal row count can mask duplicate/missing Cartesian coordinates | no detection check exists in this package; #2118 | [src/digitalmodel/parametrics/atlas_bridge.py:L50-L70] |
| S21 | NaN guard does not explicitly reject infinite responses | no detection check exists in this package; #2118 | [src/digitalmodel/parametrics/atlas_bridge.py:L65-L70] |

No executable `continue`, explicit `return None`, empty error-list/dict return, epoch default, percentage fallback or cache write occurs in this package; the three float conversions consume workflow result fields rather than raw numeric text. [src/digitalmodel/parametrics/sweep.py:L24-L43] [src/digitalmodel/parametrics/atlas_bridge.py:L22-L80] [src/digitalmodel/parametrics/pilots.py:L33-L180]

| Additional limitation | Detection check | Issue and evidence |
|---|---|---|
| Configured citation page is absent: FOWT cases become failed and seven downstream atlas tests cannot reach their assertions | C05/C06; observed 8 failed tests before any edits | #574; [src/digitalmodel/orcaflex/mooring_design_fowt.py:L277-L304] [src/digitalmodel/parametrics/sweep.py:L33-L43] [tests/parametrics/test_atlas_bridge.py:L11-L34] [tests/parametrics/test_atlas_serving.py:L21-L39] |
| Pilot docstring calls the calculation completed, but success depends on citation content | C05/C06; no bypass of citation validation | #574; [src/digitalmodel/parametrics/__init__.py:L9-L15] [src/digitalmodel/orcaflex/mooring_design_fowt.py:L250-L304] |
| Plan describes different first pilot choices from the implemented three pilots | no detection check exists | #943; [docs/plans/parametrics-preparedness.md:L71-L82] [src/digitalmodel/parametrics/__init__.py:L9-L15] |

Issues #968, #943, #971 and #975 describe implementation scope rather than confirmed incidents; the reproducible defects above use #574 and #2118 or numbered drafts. [src/digitalmodel/parametrics/__init__.py:L1-L15] [src/digitalmodel/parametrics/atlas_bridge.py:L1-L8] [tests/parametrics/test_atlas_serving.py:L1-L5]

## 7. Lessons log

This draft records inspection lessons; no independent quality clearance or earlier cleared lesson is asserted.

| Date | Change reference | Lesson | Evidence | Pack change | Shared-log entry |
|---|---|---|---|---|---|
| 2026-10-30 | Initial pack | A failed runner result remains in a returned summary; inspect status before interpreting an atlas fixture failure | [src/digitalmodel/parametrics/sweep.py:L33-L43] [src/digitalmodel/parametrics/atlas_bridge.py:L39-L48] | Sections 4–6 distinguish baseline failure from solved physics | Not assigned; issue pending (1) |
| 2026-10-30 | Initial pack | An assigned zero interpolation error is not measured validation | [src/digitalmodel/parametrics/atlas_bridge.py:L70-L80] | C09 and S07 retain #2118 | Not assigned; #2118 |

## 8. Ownership

Proposed review assignments for this draft:

| Responsibility | Role |
|---|---|
| Family quality review | Engineering calculations quality manager |
| Deputy | Engineering calculations deputy quality manager; individual assignment pending |
| Production | Engineering calculations contributors |
| Engineering escalation | Family quality manager for reference, tolerance or classification disputes; maintainer for scope and publication decisions |

Individual review and deputy assignments remain open under issue pending (3); these are proposed responsibilities, not evidence of a completed review.

## 9. Dependencies

### Runtime and data dependencies

| Dependency | Used for | Evidence |
|---|---|---|
| `orcaflex.batch_parametric` | Study expansion, result records and summaries; separate package | [src/digitalmodel/parametrics/__init__.py:L18-L23] [src/digitalmodel/parametrics/sweep.py:L13-L17] |
| `ansys.padeye` | Padeye geometry and text writer | [src/digitalmodel/parametrics/pilots.py:L40-L59] |
| `hydrodynamics.diffraction.spec_converter` | Input specification validation | [src/digitalmodel/parametrics/pilots.py:L83-L119] |
| `orcaflex.fowt_mooring_workflow` | Analytical cable-screening router | [src/digitalmodel/parametrics/pilots.py:L137-L177] |
| `parametric.atlas` | Atlas axes and object; distinct from this package | [src/digitalmodel/parametrics/atlas_bridge.py:L15-L19] [src/digitalmodel/parametrics/atlas_bridge.py:L70-L80] |
| YAML and filesystem | Source spec, mesh copy, per-case output files | [src/digitalmodel/parametrics/pilots.py:L12-L15] [src/digitalmodel/parametrics/pilots.py:L85-L119] |
| Downstream `parametric.query` | Test integration saves atlas then queries its response | [tests/parametrics/test_atlas_serving.py:L21-L53] |

UNVERIFIED: target solver versions and dependency inventory-row identifiers are not established by these pilots; no licensed solver execution or version qualification was performed (issue pending (3)).

Every line naming OrcaFlex, OrcaWave or ANSYS in the package is indexed below, including docstrings, comments and import paths; these are text dependencies, not solver execution evidence.

| Source | Names occurring on that line | Citation |
|---|---|---|
| `src/digitalmodel/parametrics/__init__.py` | orcaflex | [src/digitalmodel/parametrics/__init__.py:L4-L4] |
| `src/digitalmodel/parametrics/__init__.py` | ansys | [src/digitalmodel/parametrics/__init__.py:L10-L10] |
| `src/digitalmodel/parametrics/__init__.py` | orcawave | [src/digitalmodel/parametrics/__init__.py:L12-L12] |
| `src/digitalmodel/parametrics/__init__.py` | orcawave | [src/digitalmodel/parametrics/__init__.py:L13-L13] |
| `src/digitalmodel/parametrics/__init__.py` | orcaflex | [src/digitalmodel/parametrics/__init__.py:L14-L14] |
| `src/digitalmodel/parametrics/__init__.py` | orcaflex | [src/digitalmodel/parametrics/__init__.py:L18-L18] |
| `src/digitalmodel/parametrics/atlas_bridge.py` | orcaflex | [src/digitalmodel/parametrics/atlas_bridge.py:L15-L15] |
| `src/digitalmodel/parametrics/pilots.py` | ansys, orcawave | [src/digitalmodel/parametrics/pilots.py:L5-L5] |
| `src/digitalmodel/parametrics/pilots.py` | orcaflex | [src/digitalmodel/parametrics/pilots.py:L6-L6] |
| `src/digitalmodel/parametrics/pilots.py` | orcaflex | [src/digitalmodel/parametrics/pilots.py:L17-L17] |
| `src/digitalmodel/parametrics/pilots.py` | orcawave | [src/digitalmodel/parametrics/pilots.py:L25-L25] |
| `src/digitalmodel/parametrics/pilots.py` | orcawave | [src/digitalmodel/parametrics/pilots.py:L26-L26] |
| `src/digitalmodel/parametrics/pilots.py` | orcawave | [src/digitalmodel/parametrics/pilots.py:L27-L27] |
| `src/digitalmodel/parametrics/pilots.py` | ansys | [src/digitalmodel/parametrics/pilots.py:L30-L30] |
| `src/digitalmodel/parametrics/pilots.py` | ansys | [src/digitalmodel/parametrics/pilots.py:L33-L33] |
| `src/digitalmodel/parametrics/pilots.py` | ansys | [src/digitalmodel/parametrics/pilots.py:L40-L40] |
| `src/digitalmodel/parametrics/pilots.py` | ansys | [src/digitalmodel/parametrics/pilots.py:L44-L44] |
| `src/digitalmodel/parametrics/pilots.py` | orcawave | [src/digitalmodel/parametrics/pilots.py:L73-L73] |
| `src/digitalmodel/parametrics/pilots.py` | orcawave | [src/digitalmodel/parametrics/pilots.py:L76-L76] |
| `src/digitalmodel/parametrics/pilots.py` | orcawave | [src/digitalmodel/parametrics/pilots.py:L82-L82] |
| `src/digitalmodel/parametrics/pilots.py` | orcawave | [src/digitalmodel/parametrics/pilots.py:L86-L86] |
| `src/digitalmodel/parametrics/pilots.py` | orcawave | [src/digitalmodel/parametrics/pilots.py:L93-L93] |
| `src/digitalmodel/parametrics/pilots.py` | orcaflex | [src/digitalmodel/parametrics/pilots.py:L125-L125] |
| `src/digitalmodel/parametrics/pilots.py` | orcaflex | [src/digitalmodel/parametrics/pilots.py:L128-L128] |
| `src/digitalmodel/parametrics/pilots.py` | orcaflex | [src/digitalmodel/parametrics/pilots.py:L137-L137] |
| `src/digitalmodel/parametrics/pilots.py` | orcaflex | [src/digitalmodel/parametrics/pilots.py:L148-L148] |
| `src/digitalmodel/parametrics/sweep.py` | orcaflex | [src/digitalmodel/parametrics/sweep.py:L13-L13] |

### Implementation-map inventory and counted-family search

Map reference: `origin/codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; map data revision `270c8759c374b67a7f032953e7fcdb14e0224461`. Citations to `docs/registry/implementation-map.yaml` in this pack resolve at that map commit, while all other repository citations resolve at the header code revision. [docs/registry/implementation-map.yaml:L1-L4]
Exactly one family has an in-package row: `orcaflex-text-model-writers`, with 1 in-package row and 440 total implementation rows; it is therefore the sole counted family. [docs/registry/implementation-map.yaml:L4933-L4938] [docs/registry/implementation-map.yaml:L6815-L6824]
The inventory below retains all 440 row identities in map order, including helpers, tests and external rows; its contents describe map classifications, not independently established equivalence. Identifying discovery paths and symbols use neutral row identifiers, with their exact map-entry citations retained; codenaming is tracked in issue pending (2).

| Map row | Implementation path or discovery identifier | Symbol | Map-entry evidence |
|---|---|---|---|
| 1 | `docs/domains/examples/orcaflex/run_universal_example.py` | `main` | [docs/registry/implementation-map.yaml:L4940-L4949] |
| 2 | `discovery-002 (identifying path withheld)` | `withheld` | [docs/registry/implementation-map.yaml:L4950-L4958] |
| 3 | `docs/domains/orcaflex/examples/modular/C10/C10 Multiple statics/run_orcaflex.py` | `main` | [docs/registry/implementation-map.yaml:L4959-L4968] |
| 4 | `docs/domains/orcaflex/examples/modular/E08/PipelayConfig/run_orcaflex.py` | `main` | [docs/registry/implementation-map.yaml:L4969-L4978] |
| 5 | `docs/domains/orcaflex/risers/drilling/wd9500ft/Preprocessing code.py` | `strengthInputFilePreperation` | [docs/registry/implementation-map.yaml:L4979-L4988] |
| 6 | `docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_diffraction.py` | `run_orcawave_diffraction` | [docs/registry/implementation-map.yaml:L4989-L4998] |
| 7 | `docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_diffraction_improved.py` | `run_diffraction_with_monitoring` | [docs/registry/implementation-map.yaml:L4999-L5008] |
| 8 | `docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_diffraction_improved.py` | `run_batch_execution` | [docs/registry/implementation-map.yaml:L5009-L5020] |
| 9 | `docs/domains/orcawave/examples/L02 OC4 Semi-sub/run_orcawave.py` | `_load_data_compat` | [docs/registry/implementation-map.yaml:L5021-L5030] |
| 10 | `docs/domains/orcawave/examples/L03 Semi-sub multibody analysis/run_orcawave.py` | `_load_data_compat` | [docs/registry/implementation-map.yaml:L5031-L5040] |
| 11 | `docs/domains/orcawave/examples/L04 Sectional bodies/run_orcawave.py` | `_load_data_compat` | [docs/registry/implementation-map.yaml:L5041-L5050] |
| 12 | `docs/domains/orcawave/examples/L05 Panel pressures/run_orcawave.py` | `_load_data_compat` | [docs/registry/implementation-map.yaml:L5051-L5060] |
| 13 | `examples/domains/workflows/utils.py` | `OrcaFlexExporter.create_mooring_yml` | [docs/registry/implementation-map.yaml:L5061-L5070] |
| 14 | `scripts/audit_spec_library.py` | `write_audit_yaml` | [docs/registry/implementation-map.yaml:L5071-L5080] |
| 15 | `scripts/benchmark/generate_owd_ymls.py` | `generate_yml` | [docs/registry/implementation-map.yaml:L5081-L5090] |
| 16 | `scripts/benchmark/run_spar_benchmark.py` | `main` | [docs/registry/implementation-map.yaml:L5091-L5100] |
| 17 | `scripts/benchmark/solve_owd_extract.py` | `solve_and_extract` | [docs/registry/implementation-map.yaml:L5101-L5110] |
| 18 | `scripts/benchmark/validate_owd_vs_spec.py` | `solve_owd` | [docs/registry/implementation-map.yaml:L5111-L5120] |
| 19 | `scripts/benchmark/validate_owd_vs_spec.py` | `solve_spec` | [docs/registry/implementation-map.yaml:L5121-L5132] |
| 20 | `scripts/benchmark/validate_owd_vs_spec.py` | `_solve_spec_via_orcfxapi` | [docs/registry/implementation-map.yaml:L5133-L5142] |
| 21 | `scripts/benchmark_model_library.py` | `(module)` | [docs/registry/implementation-map.yaml:L5143-L5151] |
| 22 | `scripts/benchmark_model_library.py` | `load_and_run` | [docs/registry/implementation-map.yaml:L5152-L5161] |
| 23 | `scripts/benchmark_model_library.py` | `run_spec_driven` | [docs/registry/implementation-map.yaml:L5162-L5171] |
| 24 | `scripts/benchmark_model_library.py` | `load_json` | [docs/registry/implementation-map.yaml:L5172-L5181] |
| 25 | `scripts/benchmark_model_library.py` | `_trace` | [docs/registry/implementation-map.yaml:L5182-L5191] |
| 26 | `scripts/benchmark_riser_library.py` | `apply_mesh_preset` | [docs/registry/implementation-map.yaml:L5192-L5201] |
| 27 | `scripts/benchmark_riser_library.py` | `run_mesh_sensitivity` | [docs/registry/implementation-map.yaml:L5202-L5211] |
| 28 | `scripts/benchmark_riser_library.py` | `benchmark_model` | [docs/registry/implementation-map.yaml:L5212-L5223] |
| 29 | `scripts/build_sme_report.py` | `make_chart_js` | [docs/registry/implementation-map.yaml:L5224-L5233] |
| 30 | `scripts/build_sme_report.py` | `make_profile_js` | [docs/registry/implementation-map.yaml:L5234-L5243] |
| 31 | `scripts/ci/check_include_targets.py` | `extract_references` | [docs/registry/implementation-map.yaml:L5244-L5253] |
| 32 | `scripts/consolidate_panels.py` | `_build_manual_entries` | [docs/registry/implementation-map.yaml:L5254-L5263] |
| 33 | `scripts/convert_orcaflex_to_yml.py` | `OrcaFlexConverter.convert_file` | [docs/registry/implementation-map.yaml:L5264-L5273] |
| 34 | `scripts/create_flattened_models.py` | `create_flattened_model_dir` | [docs/registry/implementation-map.yaml:L5274-L5283] |
| 35 | `scripts/extract_library_validation.py` | `process_model_dir` | [docs/registry/implementation-map.yaml:L5284-L5293] |
| 36 | `scripts/extract_property_inventory.py` | `_field_to_section_name` | [docs/registry/implementation-map.yaml:L5294-L5303] |
| 37 | `scripts/extract_property_inventory.py` | `write_yaml` | [docs/registry/implementation-map.yaml:L5304-L5313] |
| 38 | `scripts/extract_riser_validation.py` | `main` | [docs/registry/implementation-map.yaml:L5314-L5323] |
| 39 | `scripts/extract_s7_specs.py` | `save_spec` | [docs/registry/implementation-map.yaml:L5324-L5333] |
| 40 | `scripts/fix_orcaflex_structure.py` | `create_env_section_wrappers` | [docs/registry/implementation-map.yaml:L5334-L5343] |
| 41 | `scripts/fix_orcaflex_structure.py` | `create_fixed_analysis_model` | [docs/registry/implementation-map.yaml:L5344-L5353] |
| 42 | `scripts/fix_orcaflex_structure.py` | `fix_sections_for_discretised` | [docs/registry/implementation-map.yaml:L5354-L5363] |
| 43 | `scripts/generate_all_specs.py` | `(module)` | [docs/registry/implementation-map.yaml:L5364-L5372] |
| 44 | `scripts/generate_all_specs.py` | `extract_spec` | [docs/registry/implementation-map.yaml:L5373-L5382] |
| 45 | `scripts/generate_all_specs.py` | `save_spec_yml` | [docs/registry/implementation-map.yaml:L5383-L5392] |
| 46 | `scripts/generate_analysis_files.py` | `create_analysis_file` | [docs/registry/implementation-map.yaml:L5393-L5402] |
| 47 | `scripts/generate_calm_buoy_project.py` | `CALMBuoyProjectGenerator.generate_orcaflex_modules` | [docs/registry/implementation-map.yaml:L5403-L5415] |
| 48 | `scripts/generate_calm_buoy_project.py` | `CALMBuoyProjectGenerator._populate_environment_module` | [docs/registry/implementation-map.yaml:L5416-L5425] |
| 49 | `scripts/generate_calm_buoy_project.py` | `CALMBuoyProjectGenerator._apply_module_customizations` | [docs/registry/implementation-map.yaml:L5426-L5437] |
| 50 | `scripts/generate_calm_buoy_project.py` | `CALMBuoyProjectGenerator._update_base_file_paths` | [docs/registry/implementation-map.yaml:L5438-L5447] |
| 51 | `scripts/generate_calm_buoy_project.py` | `CALMBuoyProjectGenerator.save_updated_config` | [docs/registry/implementation-map.yaml:L5448-L5457] |
| 52 | `scripts/generate_env_files.py` | `create_wave_file` | [docs/registry/implementation-map.yaml:L5458-L5467] |
| 53 | `scripts/generate_env_files.py` | `create_current_file` | [docs/registry/implementation-map.yaml:L5468-L5477] |
| 54 | `scripts/generate_env_files.py` | `create_wind_file` | [docs/registry/implementation-map.yaml:L5478-L5487] |
| 55 | `scripts/initialize_skill_metadata.py` | `create_version_metadata_block` | [docs/registry/implementation-map.yaml:L5488-L5497] |
| 56 | `scripts/inventory/orcaflex_legacy_inventory.py` | `_report_type` | [docs/registry/implementation-map.yaml:L5498-L5507] |
| 57 | `scripts/inventory/orcaflex_legacy_inventory.py` | `classify` | [docs/registry/implementation-map.yaml:L5508-L5519] |
| 58 | `scripts/inventory/orcaflex_legacy_inventory.py` | `main` | [docs/registry/implementation-map.yaml:L5520-L5531] |
| 59 | `scripts/maintenance/archive_candidates.py` | `(module)` | [docs/registry/implementation-map.yaml:L5532-L5540] |
| 60 | `scripts/maintenance/archive_candidates.py` | `main` | [docs/registry/implementation-map.yaml:L5541-L5550] |
| 61 | `scripts/mesh_sensitivity_riser.py` | `apply_mesh_preset` | [docs/registry/implementation-map.yaml:L5551-L5560] |
| 62 | `scripts/orcaflex/check_generic_scr_slwr.py` | `scr_reference` | [docs/registry/implementation-map.yaml:L5561-L5570] |
| 63 | `scripts/orcaflex/check_generic_scr_slwr.py` | `slwr_reference` | [docs/registry/implementation-map.yaml:L5571-L5580] |
| 64 | `scripts/orcaflex/check_generic_scr_slwr.py` | `calculate` | [docs/registry/implementation-map.yaml:L5581-L5593] |
| 65 | `scripts/orcaflex/prepare_slay_campaign.py` | `case_spec` | [docs/registry/implementation-map.yaml:L5594-L5603] |
| 66 | `scripts/orcaflex/prepare_slay_campaign.py` | `prepare_case` | [docs/registry/implementation-map.yaml:L5604-L5615] |
| 67 | `scripts/orcaflex/prepare_slay_campaign.py` | `prepare` | [docs/registry/implementation-map.yaml:L5616-L5628] |
| 68 | `scripts/prepare_mooring_qualification.py` | `_generated_sections` | [docs/registry/implementation-map.yaml:L5629-L5638] |
| 69 | `scripts/python/digitalmodel/doc_tools/check_structure_creation.py` | `ContentAnalyzer._categorize_folder` | [docs/registry/implementation-map.yaml:L5639-L5648] |
| 70 | `scripts/python/digitalmodel/doc_tools/frontmatter_generator.py` | `FrontmatterGenerator.frontmatter_to_yaml` | [docs/registry/implementation-map.yaml:L5649-L5658] |
| 71 | `scripts/python/digitalmodel/legacy/51_-_SerializationYAMLPart1.py` | `(module)` | [docs/registry/implementation-map.yaml:L5659-L5667] |
| 72 | `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | `make_comparison_loaded_vs_ballast_cxc` | [docs/registry/implementation-map.yaml:L5668-L5677] |
| 73 | `discovery-073 (identifying path withheld)` | `withheld` | [docs/registry/implementation-map.yaml:L5678-L5687] |
| 74 | `discovery-074 (identifying path withheld)` | `withheld` | [docs/registry/implementation-map.yaml:L5688-L5697] |
| 75 | `scripts/python/digitalmodel/orcawave/create_gdf_consistent.py` | `create_config_with_gdf` | [docs/registry/implementation-map.yaml:L5698-L5707] |
| 76 | `scripts/python/digitalmodel/orcawave/create_working_examples.py` | `create_simple_vessel_config` | [docs/registry/implementation-map.yaml:L5708-L5717] |
| 77 | `scripts/python/digitalmodel/orcawave/create_working_examples.py` | `create_oc4_semisub_config` | [docs/registry/implementation-map.yaml:L5718-L5727] |
| 78 | `scripts/python/digitalmodel/orcawave/create_working_examples.py` | `create_minimal_test_config` | [docs/registry/implementation-map.yaml:L5728-L5737] |
| 79 | `scripts/python/digitalmodel/orcawave/final_working_config.py` | `create_final_working_config` | [docs/registry/implementation-map.yaml:L5738-L5749] |
| 80 | `scripts/python/digitalmodel/orcawave/final_working_config.py` | `create_extended_version` | [docs/registry/implementation-map.yaml:L5750-L5759] |
| 81 | `scripts/python/digitalmodel/orcawave/fix_orcawave_yaml.py` | `withheld (identifying symbol)` | [docs/registry/implementation-map.yaml:L5760-L5769] |
| 82 | `discovery-082 (identifying path withheld)` | `withheld` | [docs/registry/implementation-map.yaml:L5770-L5781] |
| 83 | `discovery-083 (identifying path withheld)` | `withheld` | [docs/registry/implementation-map.yaml:L5782-L5791] |
| 84 | `scripts/python/digitalmodel/orcawave/fix_working_model.py` | `fix_working_model` | [docs/registry/implementation-map.yaml:L5792-L5801] |
| 85 | `scripts/python/digitalmodel/orcawave/fix_working_model.py` | `create_minimal_with_test_box` | [docs/registry/implementation-map.yaml:L5802-L5811] |
| 86 | `scripts/python/digitalmodel/orcawave/validate_orcawave_configs.py` | `OrcaWaveValidator.run_validation` | [docs/registry/implementation-map.yaml:L5812-L5823] |
| 87 | `scripts/python/digitalmodel/tools/add_orcaflex_test_configs.py` | `create_batch_config` | [docs/registry/implementation-map.yaml:L5824-L5833] |
| 88 | `scripts/python/digitalmodel/tools/standardize_orcaflex_tests.py` | `OrcaFlexTestStandardizer.create_standard_config` | [docs/registry/implementation-map.yaml:L5834-L5843] |
| 89 | `scripts/run_riser_analysis.py` | `run_library` | [docs/registry/implementation-map.yaml:L5844-L5853] |
| 90 | `scripts/run_semisub_quality_check.py` | `main` | [docs/registry/implementation-map.yaml:L5854-L5863] |
| 91 | `scripts/sanitize_s7_models.py` | `(module)` | [docs/registry/implementation-map.yaml:L5864-L5872] |
| 92 | `scripts/sanitize_s7_models.py` | `process_dat_file` | [docs/registry/implementation-map.yaml:L5873-L5882] |
| 93 | `scripts/semantic_validate.py` | `(module)` | [docs/registry/implementation-map.yaml:L5883-L5891] |
| 94 | `scripts/sync_monolithic_yml.py` | `convert_dat_to_yml` | [docs/registry/implementation-map.yaml:L5892-L5901] |
| 95 | `discovery-095 (identifying path withheld)` | `withheld` | [docs/registry/implementation-map.yaml:L5902-L5911] |
| 96 | `discovery-096 (identifying path withheld)` | `withheld` | [docs/registry/implementation-map.yaml:L5912-L5921] |
| 97 | `scripts/update_skill_versions.py` | `update_skill_file` | [docs/registry/implementation-map.yaml:L5922-L5933] |
| 98 | `scripts/validate_orcaflex_model.py` | `ModelValidator.extract_model_info` | [docs/registry/implementation-map.yaml:L5934-L5943] |
| 99 | `scripts/verification/verify_orcaflex_campaign.py` | `_save_body` | [docs/registry/implementation-map.yaml:L5944-L5953] |
| 100 | `scripts/verification/verify_orcaflex_campaign.py` | `main` | [docs/registry/implementation-map.yaml:L5954-L5965] |
| 101 | `src/digitalmodel/asset_integrity/common/data.py` | `SaveData.saveDataYaml` | [docs/registry/implementation-map.yaml:L5966-L5975] |
| 102 | `src/digitalmodel/asset_integrity/common/data.py` | `SaveData.WriteOrcaFlexYMLFile` | [docs/registry/implementation-map.yaml:L5976-L5985] |
| 103 | `src/digitalmodel/asset_integrity/common/data.py` | `(module)` | [docs/registry/implementation-map.yaml:L5986-L5994] |
| 104 | `src/digitalmodel/data_systems/data_procurement/common/stream_handler.py` | `StreamHandler._format_orcaflex_yml` | [docs/registry/implementation-map.yaml:L5995-L6004] |
| 105 | `src/digitalmodel/data_systems/data_procurement/mooring/client.py` | `MooringClient.to_orcaflex_mooring` | [docs/registry/implementation-map.yaml:L6005-L6014] |
| 106 | `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/chain_db_client.py` | `ChainDatabaseClient.to_orcaflex_line_type` | [docs/registry/implementation-map.yaml:L6015-L6024] |
| 107 | `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/synthetic_rope_db_client.py` | `SyntheticRopeDatabaseClient.to_orcaflex_line_type` | [docs/registry/implementation-map.yaml:L6025-L6034] |
| 108 | `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/wire_rope_db_client.py` | `WireRopeDatabaseClient.to_orcaflex_line_type` | [docs/registry/implementation-map.yaml:L6035-L6044] |
| 109 | `src/digitalmodel/data_systems/data_procurement/riser/database_clients/pipe_db_client.py` | `PipeSpecificationClient.to_orcaflex_line_type` | [docs/registry/implementation-map.yaml:L6045-L6054] |
| 110 | `src/digitalmodel/data_systems/data_procurement/vessel/client.py` | `VesselClient.to_orcaflex_vessel` | [docs/registry/implementation-map.yaml:L6055-L6064] |
| 111 | `src/digitalmodel/drilling_riser/campaign.py` | `case_spec` | [docs/registry/implementation-map.yaml:L6065-L6074] |
| 112 | `src/digitalmodel/drilling_riser/campaign.py` | `_probe` | [docs/registry/implementation-map.yaml:L6075-L6084] |
| 113 | `src/digitalmodel/drilling_riser/campaign.py` | `_capped_probe` | [docs/registry/implementation-map.yaml:L6085-L6096] |
| 114 | `src/digitalmodel/drilling_riser/campaign.py` | `_statics_path` | [docs/registry/implementation-map.yaml:L6097-L6109] |
| 115 | `src/digitalmodel/drilling_riser/campaign.py` | `physical_state_checks` | [docs/registry/implementation-map.yaml:L6110-L6119] |
| 116 | `src/digitalmodel/drilling_riser/global_model/build.py` | `_line_type` | [docs/registry/implementation-map.yaml:L6120-L6129] |
| 117 | `src/digitalmodel/drilling_riser/global_model/build.py` | `build_generic_spec` | [docs/registry/implementation-map.yaml:L6130-L6141] |
| 118 | `src/digitalmodel/drilling_riser/global_model/build.py` | `write_model` | [docs/registry/implementation-map.yaml:L6142-L6153] |
| 119 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `reference_periods` | [docs/registry/implementation-map.yaml:L6154-L6163] |
| 120 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `build_open_water_generic_spec` | [docs/registry/implementation-map.yaml:L6164-L6173] |
| 121 | `src/digitalmodel/drilling_riser/postprocess/stress_range.py` | `extract` | [docs/registry/implementation-map.yaml:L6174-L6183] |
| 122 | `src/digitalmodel/drilling_riser/postprocess/stress_range.py` | `merge_stress_range` | [docs/registry/implementation-map.yaml:L6184-L6193] |
| 123 | `src/digitalmodel/drilling_riser/postprocess/stress_range.py` | `extract_significant` | [docs/registry/implementation-map.yaml:L6194-L6203] |
| 124 | `src/digitalmodel/floating_wind/solver_tier.py` | `prepare_variant` | [docs/registry/implementation-map.yaml:L6204-L6213] |
| 125 | `src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py` | `OrcaFlexConverter.convert_to_vessel_yaml` | [docs/registry/implementation-map.yaml:L6214-L6223] |
| 126 | `src/digitalmodel/hydrodynamics/diffraction/benchmark_mesh_schematic.py` | `_add_fdf_surface_traces` | [docs/registry/implementation-map.yaml:L6224-L6233] |
| 127 | `src/digitalmodel/hydrodynamics/diffraction/diffraction_cli.py` | `run_aqwa` | [docs/registry/implementation-map.yaml:L6234-L6243] |
| 128 | `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` | `DiffractionSpec.to_yaml` | [docs/registry/implementation-map.yaml:L6244-L6253] |
| 129 | `src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py` | `OrcaFlexExporter.export_vessel_type` | [docs/registry/implementation-map.yaml:L6254-L6263] |
| 130 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py` | `_write_yml` | [docs/registry/implementation-map.yaml:L6264-L6273] |
| 131 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` | `OrcaWaveRunner._execute_via_api` | [docs/registry/implementation-map.yaml:L6274-L6283] |
| 132 | `src/digitalmodel/hydrodynamics/hull_library/panel_catalog.py` | `PanelCatalog.to_yaml` | [docs/registry/implementation-map.yaml:L6284-L6293] |
| 133 | `src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py` | `RAOAnalysis.write_filtered_orcaflex_seastate_rao_file` | [docs/registry/implementation-map.yaml:L6294-L6303] |
| 134 | `src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py` | `RAOAnalysis.get_shape_data` | [docs/registry/implementation-map.yaml:L6304-L6313] |
| 135 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_viv_modes_excited` | [docs/registry/implementation-map.yaml:L6314-L6323] |
| 136 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.tabulate_viv_results` | [docs/registry/implementation-map.yaml:L6324-L6333] |
| 137 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.tabulate_overall_viv_results` | [docs/registry/implementation-map.yaml:L6334-L6343] |
| 138 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.save_viv_histograms` | [docs/registry/implementation-map.yaml:L6344-L6353] |
| 139 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.save_combined_histograms` | [docs/registry/implementation-map.yaml:L6354-L6363] |
| 140 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.save_fatigue_summary_result` | [docs/registry/implementation-map.yaml:L6364-L6373] |
| 141 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | [docs/registry/implementation-map.yaml:L6374-L6383] |
| 142 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py` | `DNVRPH103_hydrodynamics_circular.save_model` | [docs/registry/implementation-map.yaml:L6384-L6393] |
| 143 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py` | `DNVRPH103_hydrodynamics_circular.update_6d_buoy` | [docs/registry/implementation-map.yaml:L6394-L6403] |
| 144 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py` | `DNVRPH103_hydrodynamics_rectangular.save_model` | [docs/registry/implementation-map.yaml:L6404-L6413] |
| 145 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py` | `DNVRPH103_hydrodynamics_rectangular.update_6d_buoy` | [docs/registry/implementation-map.yaml:L6414-L6423] |
| 146 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` | [docs/registry/implementation-map.yaml:L6424-L6436] |
| 147 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.riser_joints_to_line_types` | [docs/registry/implementation-map.yaml:L6437-L6448] |
| 148 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.GetGeneral` | [docs/registry/implementation-map.yaml:L6449-L6458] |
| 149 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.lineTypes` | [docs/registry/implementation-map.yaml:L6459-L6468] |
| 150 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.WriteOrcaflexModel` | [docs/registry/implementation-map.yaml:L6469-L6478] |
| 151 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.build_model` | [docs/registry/implementation-map.yaml:L6479-L6490] |
| 152 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` | `FEAComponents.AssignGeneral` | [docs/registry/implementation-map.yaml:L6491-L6500] |
| 153 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` | `FEAComponents.AssignBuoyProperties` | [docs/registry/implementation-map.yaml:L6501-L6510] |
| 154 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` | `FEAComponents.AssignLineProperties` | [docs/registry/implementation-map.yaml:L6511-L6520] |
| 155 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` | `FEAComponents.AssignLineTypeProperties` | [docs/registry/implementation-map.yaml:L6521-L6530] |
| 156 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` | `FEAComponents.AssignVesselProperties` | [docs/registry/implementation-map.yaml:L6531-L6540] |
| 157 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` | `FEAComponents.save_output` | [docs/registry/implementation-map.yaml:L6541-L6550] |
| 158 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` | `FEAComponents.load_variations` | [docs/registry/implementation-map.yaml:L6551-L6560] |
| 159 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_viv_modes_excited` | [docs/registry/implementation-map.yaml:L6561-L6570] |
| 160 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.tabulate_viv_results` | [docs/registry/implementation-map.yaml:L6571-L6580] |
| 161 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.tabulate_overall_viv_results` | [docs/registry/implementation-map.yaml:L6581-L6590] |
| 162 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.save_viv_histograms` | [docs/registry/implementation-map.yaml:L6591-L6600] |
| 163 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.save_combined_histograms` | [docs/registry/implementation-map.yaml:L6601-L6610] |
| 164 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.save_fatigue_summary_result` | [docs/registry/implementation-map.yaml:L6611-L6620] |
| 165 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | [docs/registry/implementation-map.yaml:L6621-L6630] |
| 166 | `src/digitalmodel/infrastructure/utils/data.py` | `SaveData.saveDataYaml` | [docs/registry/implementation-map.yaml:L6631-L6640] |
| 167 | `src/digitalmodel/infrastructure/utils/data.py` | `SaveData.WriteOrcaFlexYMLFile` | [docs/registry/implementation-map.yaml:L6641-L6650] |
| 168 | `src/digitalmodel/infrastructure/utils/data.py` | `(module)` | [docs/registry/implementation-map.yaml:L6651-L6659] |
| 169 | `src/digitalmodel/marine_ops/installation/jumper_input.py` | `generate_input` | [docs/registry/implementation-map.yaml:L6660-L6672] |
| 170 | `src/digitalmodel/marine_ops/installation/jumper_installation_schema.py` | `JumperInstallationSpec.to_yaml` | [docs/registry/implementation-map.yaml:L6673-L6682] |
| 171 | `src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py` | `build_line_types` | [docs/registry/implementation-map.yaml:L6683-L6692] |
| 172 | `src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py` | `build_modular_spec` | [docs/registry/implementation-map.yaml:L6693-L6704] |
| 173 | `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` | `UnifiedRAOReader._detect_format` | [docs/registry/implementation-map.yaml:L6705-L6714] |
| 174 | `src/digitalmodel/orcaflex/model_builder.py` | `VesselConfig.to_orcaflex_dict` | [docs/registry/implementation-map.yaml:L6715-L6724] |
| 175 | `src/digitalmodel/orcaflex/model_builder.py` | `Buoy6DConfig.to_orcaflex_dict` | [docs/registry/implementation-map.yaml:L6725-L6734] |
| 176 | `src/digitalmodel/orcaflex/model_builder.py` | `LineConfig.to_orcaflex_dict` | [docs/registry/implementation-map.yaml:L6735-L6744] |
| 177 | `src/digitalmodel/orcaflex/model_builder.py` | `build_scr_model` | [docs/registry/implementation-map.yaml:L6745-L6754] |
| 178 | `src/digitalmodel/orcaflex/model_builder.py` | `build_lazy_wave_model` | [docs/registry/implementation-map.yaml:L6755-L6764] |
| 179 | `src/digitalmodel/orcaflex/model_builder.py` | `build_mooring_line` | [docs/registry/implementation-map.yaml:L6765-L6774] |
| 180 | `src/digitalmodel/orcaflex/reporting/sections/model_summary.py` | `build_model_summary` | [docs/registry/implementation-map.yaml:L6775-L6784] |
| 181 | `src/digitalmodel/orcaflex/reporting/sections/range_graphs.py` | `build_range_graphs` | [docs/registry/implementation-map.yaml:L6785-L6794] |
| 182 | `src/digitalmodel/orcawave/reporting/sections/rao_plots.py` | `build_rao_plots` | [docs/registry/implementation-map.yaml:L6795-L6804] |
| 183 | `src/digitalmodel/parametric/query.py` | `_write_result` | [docs/registry/implementation-map.yaml:L6805-L6814] |
| 184 | `src/digitalmodel/parametrics/pilots.py` | `orcawave_depth_sweep.runner` | [docs/registry/implementation-map.yaml:L6815-L6824] |
| 185 | `src/digitalmodel/solvers/benchmark/solvers.py` | `_write_diffraction_inputs` | [docs/registry/implementation-map.yaml:L6825-L6834] |
| 186 | `src/digitalmodel/solvers/fea_model/LineType_components.py` | `LineType.get_general` | [docs/registry/implementation-map.yaml:L6835-L6844] |
| 187 | `src/digitalmodel/solvers/fea_model/general_components.py` | `General.getGeneralOrcaFlexSetUp` | [docs/registry/implementation-map.yaml:L6845-L6854] |
| 188 | `src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py` | `load_csv` | [docs/registry/implementation-map.yaml:L6855-L6864] |
| 189 | `src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py` | `get_data` | [docs/registry/implementation-map.yaml:L6865-L6874] |
| 190 | `src/digitalmodel/solvers/orcaflex/core/configuration.py` | `OrcaFlexConfig.to_yaml` | [docs/registry/implementation-map.yaml:L6875-L6884] |
| 191 | `src/digitalmodel/solvers/orcaflex/core/model_interface.py` | `OrcaFlexModelWrapper.save_simulation` | [docs/registry/implementation-map.yaml:L6885-L6894] |
| 192 | `src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py` | `FeatureAnalyzer.analyze_file` | [docs/registry/implementation-map.yaml:L6895-L6904] |
| 193 | `src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py` | `FeatureAnalyzer.generate_summary` | [docs/registry/implementation-map.yaml:L6905-L6914] |
| 194 | `src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py` | `OrcaFlexBatchConverter._real_convert` | [docs/registry/implementation-map.yaml:L6915-L6927] |
| 195 | `src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py` | `OrcaFlexBatchConverter._mock_convert` | [docs/registry/implementation-map.yaml:L6928-L6937] |
| 196 | `src/digitalmodel/solvers/orcaflex/examples_integration/converter.py` | `YamlConverter.convert_batch` | [docs/registry/implementation-map.yaml:L6938-L6947] |
| 197 | `src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py` | `DownloadedExamplesIntegrator._infer_components` | [docs/registry/implementation-map.yaml:L6948-L6957] |
| 198 | `src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py` | `DownloadedExamplesIntegrator._extract_common_configs` | [docs/registry/implementation-map.yaml:L6958-L6967] |
| 199 | `src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py` | `KnowledgeIntegrator._extract_patterns` | [docs/registry/implementation-map.yaml:L6968-L6977] |
| 200 | `src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py` | `KnowledgeIntegrator.build_searchable_index` | [docs/registry/implementation-map.yaml:L6978-L6987] |
| 201 | `src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py` | `OrcFxAPIConverter._initialize_api` | [docs/registry/implementation-map.yaml:L6988-L6997] |
| 202 | `src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py` | `OrcFxAPIConverter.convert_file` | [docs/registry/implementation-map.yaml:L6998-L7007] |
| 203 | `src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py` | `detect_format` | [docs/registry/implementation-map.yaml:L7008-L7017] |
| 204 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` | `ModularToSingleConverter.convert` | [docs/registry/implementation-map.yaml:L7018-L7031] |
| 205 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` | `ModularToSingleConverter._merge_master` | [docs/registry/implementation-map.yaml:L7032-L7041] |
| 206 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` | `ModularToSingleConverter._extract_includefiles.traverse` | [docs/registry/implementation-map.yaml:L7042-L7051] |
| 207 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py` | `ModularToSpecConverter.convert` | [docs/registry/implementation-map.yaml:L7052-L7061] |
| 208 | `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` | `SingleToModularConverter._write_master_file` | [docs/registry/implementation-map.yaml:L7062-L7071] |
| 209 | `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` | `SingleToModularConverter._write_parameters_file` | [docs/registry/implementation-map.yaml:L7072-L7081] |
| 210 | `src/digitalmodel/solvers/orcaflex/inverse_resolver.py` | `_resolve_mooring` | [docs/registry/implementation-map.yaml:L7082-L7091] |
| 211 | `src/digitalmodel/solvers/orcaflex/inverse_resolver.py` | `_resolve_riser` | [docs/registry/implementation-map.yaml:L7092-L7101] |
| 212 | `src/digitalmodel/solvers/orcaflex/library_generator.py` | `LibraryGenerator.generate_component` | [docs/registry/implementation-map.yaml:L7102-L7111] |
| 213 | `src/digitalmodel/solvers/orcaflex/library_generator.py` | `LibraryGenerator.write_index` | [docs/registry/implementation-map.yaml:L7112-L7121] |
| 214 | `src/digitalmodel/solvers/orcaflex/model_catalogue_render.py` | `object_counts` | [docs/registry/implementation-map.yaml:L7122-L7131] |
| 215 | `src/digitalmodel/solvers/orcaflex/model_catalogue_render.py` | `geometry` | [docs/registry/implementation-map.yaml:L7132-L7141] |
| 216 | `src/digitalmodel/solvers/orcaflex/model_generator/__init__.py` | `OrcaFlexModelGenerator.generate_from_template` | [docs/registry/implementation-map.yaml:L7142-L7153] |
| 217 | `src/digitalmodel/solvers/orcaflex/model_generator/__init__.py` | `OrcaFlexModelGenerator._assemble_model` | [docs/registry/implementation-map.yaml:L7154-L7163] |
| 218 | `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py` | `ModularModelGenerator.generate` | [docs/registry/implementation-map.yaml:L7164-L7176] |
| 219 | `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py` | `ModularModelGenerator._write_parameters` | [docs/registry/implementation-map.yaml:L7177-L7186] |
| 220 | `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py` | `ModularModelGenerator._write_master` | [docs/registry/implementation-map.yaml:L7187-L7196] |
| 221 | `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py` | `ModularModelGenerator.generate_with_overrides` | [docs/registry/implementation-map.yaml:L7197-L7207] |
| 222 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/general_builder.py` | `GeneralBuilder.build` | [docs/registry/implementation-map.yaml:L7208-L7217] |
| 223 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/generic_builder.py` | `GenericModelBuilder.build` | [docs/registry/implementation-map.yaml:L7218-L7227] |
| 224 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/lines_builder.py` | `LinesBuilder.build` | [docs/registry/implementation-map.yaml:L7228-L7237] |
| 225 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py` | `LineTypeBuilder.build` | [docs/registry/implementation-map.yaml:L7238-L7247] |
| 226 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_lines_builder.py` | `RiserLinesBuilder.build` | [docs/registry/implementation-map.yaml:L7248-L7257] |
| 227 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_linetype_builder.py` | `RiserLineTypeBuilder.build` | [docs/registry/implementation-map.yaml:L7258-L7269] |
| 228 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_linetype_builder.py` | `RiserLineTypeBuilder._build_riser_line_type` | [docs/registry/implementation-map.yaml:L7270-L7279] |
| 229 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_vessel_builder.py` | `RiserVesselBuilder.build` | [docs/registry/implementation-map.yaml:L7280-L7289] |
| 230 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_builder.py` | `VesselBuilder.build` | [docs/registry/implementation-map.yaml:L7290-L7299] |
| 231 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` | `WinchBuilder.build` | [docs/registry/implementation-map.yaml:L7300-L7309] |
| 232 | `src/digitalmodel/solvers/orcaflex/modular_generator/cli.py` | `cmd_generate` | [docs/registry/implementation-map.yaml:L7310-L7319] |
| 233 | `src/digitalmodel/solvers/orcaflex/modular_generator/extractor.py` | `MonolithicExtractor._extract_simulation` | [docs/registry/implementation-map.yaml:L7320-L7329] |
| 234 | `src/digitalmodel/solvers/orcaflex/modular_generator/extractor.py` | `MonolithicExtractor._extract_general_properties` | [docs/registry/implementation-map.yaml:L7330-L7339] |
| 235 | `src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py` | `PostGenerationValidator._check_line_type_references` | [docs/registry/implementation-map.yaml:L7340-L7349] |
| 236 | `src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py` | `PostGenerationValidator._check_vessel_references` | [docs/registry/implementation-map.yaml:L7350-L7359] |
| 237 | `src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py` | `PostGenerationValidator._check_winch_references` | [docs/registry/implementation-map.yaml:L7360-L7369] |
| 238 | `src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py` | `_iter_includefile_refs.traverse` | [docs/registry/implementation-map.yaml:L7370-L7379] |
| 239 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py` | `MooringRouter.route` | [docs/registry/implementation-map.yaml:L7380-L7389] |
| 240 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py` | `MooringRouter._chain_linetype` | [docs/registry/implementation-map.yaml:L7390-L7399] |
| 241 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py` | `MooringRouter._wire_linetype` | [docs/registry/implementation-map.yaml:L7400-L7409] |
| 242 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py` | `MooringRouter._polyester_linetype` | [docs/registry/implementation-map.yaml:L7410-L7419] |
| 243 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/vessel_router.py` | `VesselRouter.route` | [docs/registry/implementation-map.yaml:L7420-L7429] |
| 244 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/vessel_router.py` | `VesselRouter.from_hull_catalog` | [docs/registry/implementation-map.yaml:L7430-L7439] |
| 245 | `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` | `CampaignGenerator.generate` | [docs/registry/implementation-map.yaml:L7440-L7449] |
| 246 | `src/digitalmodel/solvers/orcaflex/modular_generator/writers/basefile.py` | `write_variation_model` | [docs/registry/implementation-map.yaml:L7450-L7459] |
| 247 | `src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py` | `extract_includefiles.traverse` | [docs/registry/implementation-map.yaml:L7460-L7469] |
| 248 | `src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py` | `ModularInputValidator._collect_issues_from_level_1` | [docs/registry/implementation-map.yaml:L7470-L7479] |
| 249 | `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.prepare_includefile_for_all_lines` | [docs/registry/implementation-map.yaml:L7480-L7489] |
| 250 | `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.prepare_includefile_for_fender_compression` | [docs/registry/implementation-map.yaml:L7490-L7499] |
| 251 | `src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py` | `MockOrcaFlexAPI.load_model` | [docs/registry/implementation-map.yaml:L7500-L7509] |
| 252 | `src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py` | `OrcaFlexAPI.save_model` | [docs/registry/implementation-map.yaml:L7510-L7519] |
| 253 | `src/digitalmodel/solvers/orcaflex/operability_analysis.py` | `OperabilityAnalyzer.generate_operability_envelope` | [docs/registry/implementation-map.yaml:L7520-L7529] |
| 254 | `src/digitalmodel/solvers/orcaflex/opp_visualization.py` | `OPPVisualization.combine_models` | [docs/registry/implementation-map.yaml:L7530-L7539] |
| 255 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.save_data` | [docs/registry/implementation-map.yaml:L7540-L7551] |
| 256 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.saveSummaryToExcel` | [docs/registry/implementation-map.yaml:L7552-L7561] |
| 257 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.save_histograms` | [docs/registry/implementation-map.yaml:L7562-L7571] |
| 258 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.save_cfg_files_from_multiple_files` | [docs/registry/implementation-map.yaml:L7572-L7581] |
| 259 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.save_RAOs` | [docs/registry/implementation-map.yaml:L7582-L7591] |
| 260 | `src/digitalmodel/solvers/orcaflex/orcaflex_converter_enhanced.py` | `OrcaFlexConverterEnhanced._real_convert` | [docs/registry/implementation-map.yaml:L7592-L7603] |
| 261 | `src/digitalmodel/solvers/orcaflex/orcaflex_converter_enhanced.py` | `OrcaFlexConverterEnhanced._mock_convert` | [docs/registry/implementation-map.yaml:L7604-L7613] |
| 262 | `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` | `OrcaFlexCustomAnalysis.process_fea` | [docs/registry/implementation-map.yaml:L7614-L7626] |
| 263 | `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` | `OrcaFlexCustomAnalysis.clean_model` | [docs/registry/implementation-map.yaml:L7627-L7636] |
| 264 | `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` | `OrcaFlexCustomAnalysis.save_model_with_calculated_positions` | [docs/registry/implementation-map.yaml:L7637-L7646] |
| 265 | `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` | `OrcaFlexCustomAnalysis.iterate_to_target_value` | [docs/registry/implementation-map.yaml:L7647-L7656] |
| 266 | `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` | `OrcaFlexCustomAnalysis.save_data` | [docs/registry/implementation-map.yaml:L7657-L7666] |
| 267 | `src/digitalmodel/solvers/orcaflex/orcaflex_installation.py` | `OrcInstallation.create_model_for_water_depth` | [docs/registry/implementation-map.yaml:L7667-L7676] |
| 268 | `src/digitalmodel/solvers/orcaflex/orcaflex_installation.py` | `OrcInstallation.create_installation_depth_model` | [docs/registry/implementation-map.yaml:L7677-L7686] |
| 269 | `src/digitalmodel/solvers/orcaflex/orcaflex_installation.py` | `OrcInstallation.create_installation_depth_model2` | [docs/registry/implementation-map.yaml:L7687-L7696] |
| 270 | `src/digitalmodel/solvers/orcaflex/orcaflex_iterative_runs.py` | `OrcaflexIterativeRuns.save_iterative_file` | [docs/registry/implementation-map.yaml:L7697-L7706] |
| 271 | `src/digitalmodel/solvers/orcaflex/orcaflex_iterative_runs.py` | `OrcaflexIterativeRuns.run_iterative_simulations` | [docs/registry/implementation-map.yaml:L7707-L7716] |
| 272 | `src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py` | `OrcaflexLineTypes.get_umbilical_properties` | [docs/registry/implementation-map.yaml:L7717-L7729] |
| 273 | `src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py` | `OrcaflexLineTypes.assign_key_umbilical_properties` | [docs/registry/implementation-map.yaml:L7730-L7739] |
| 274 | `src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py` | `OrcaflexLineTypes.save_model` | [docs/registry/implementation-map.yaml:L7740-L7749] |
| 275 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` | [docs/registry/implementation-map.yaml:L7750-L7762] |
| 276 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.riser_joints_to_line_types` | [docs/registry/implementation-map.yaml:L7763-L7774] |
| 277 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.GetGeneral` | [docs/registry/implementation-map.yaml:L7775-L7784] |
| 278 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.lineTypes` | [docs/registry/implementation-map.yaml:L7785-L7794] |
| 279 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.WriteOrcaflexModel` | [docs/registry/implementation-map.yaml:L7795-L7804] |
| 280 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.build_model` | [docs/registry/implementation-map.yaml:L7805-L7816] |
| 281 | `src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel.py` | `OrcaFlexOptimizedParallelAnalysis.process_single_file` | [docs/registry/implementation-map.yaml:L7817-L7826] |
| 282 | `src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel_v2.py` | `process_single_file_worker` | [docs/registry/implementation-map.yaml:L7827-L7836] |
| 283 | `src/digitalmodel/solvers/orcaflex/orcaflex_parallel_analysis.py` | `OrcaFlexParallelAnalysis.process_single_file` | [docs/registry/implementation-map.yaml:L7837-L7846] |
| 284 | `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` | `OrcaflexUtilities.update_model` | [docs/registry/implementation-map.yaml:L7847-L7856] |
| 285 | `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` | `OrcaflexUtilities.update_input_file` | [docs/registry/implementation-map.yaml:L7857-L7866] |
| 286 | `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` | `OrcaflexUtilities.get_full_yaml_file_and_save` | [docs/registry/implementation-map.yaml:L7867-L7878] |
| 287 | `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` | `OrcaflexUtilities.get_general_yml` | [docs/registry/implementation-map.yaml:L7879-L7888] |
| 288 | `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` | `OrcaflexUtilities.save_cfg_files_from_multiple_files` | [docs/registry/implementation-map.yaml:L7889-L7898] |
| 289 | `src/digitalmodel/solvers/orcaflex/orcaflex_yml_converter.py` | `convert_to_yml` | [docs/registry/implementation-map.yaml:L7899-L7908] |
| 290 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` | `PipelineSchematicGenerator._merge_yaml` | [docs/registry/implementation-map.yaml:L7909-L7921] |
| 291 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` | `PipelineSchematicGenerator._merge_lines` | [docs/registry/implementation-map.yaml:L7922-L7931] |
| 292 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` | `PipelineSchematicGenerator._merge_6dbuoys` | [docs/registry/implementation-map.yaml:L7932-L7941] |
| 293 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` | `PipelineSchematicGenerator._parse_pipeline_path` | [docs/registry/implementation-map.yaml:L7942-L7951] |
| 294 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` | `PipelineSchematicGenerator._parse_boundary_conditions` | [docs/registry/implementation-map.yaml:L7952-L7961] |
| 295 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` | `PipelineSchematicGenerator.create_plan_view` | [docs/registry/implementation-map.yaml:L7962-L7971] |
| 296 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` | `PipelineSchematicGenerator.create_elevation_view` | [docs/registry/implementation-map.yaml:L7972-L7981] |
| 297 | `src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py` | `LoadVessel.aqwa_group_item` | [docs/registry/implementation-map.yaml:L7982-L7991] |
| 298 | `src/digitalmodel/solvers/orcaflex/reporting/extractors/aggregator.py` | `_extract_dynamic_results_all` | [docs/registry/implementation-map.yaml:L7992-L8001] |
| 299 | `src/digitalmodel/solvers/orcaflex/reporting/renderers/riser.py` | `RiserRenderer._build_riser_dynamic_results` | [docs/registry/implementation-map.yaml:L8002-L8011] |
| 300 | `src/digitalmodel/solvers/orcaflex/run_to_sim.py` | `find_model_files` | [docs/registry/implementation-map.yaml:L8012-L8021] |
| 301 | `src/digitalmodel/solvers/orcaflex/spec_author.py` | `AuthoredSpec.to_spec_yaml` | [docs/registry/implementation-map.yaml:L8022-L8031] |
| 302 | `src/digitalmodel/solvers/orcaflex/template_generator.py` | `TemplateGenerator.generate` | [docs/registry/implementation-map.yaml:L8032-L8043] |
| 303 | `src/digitalmodel/solvers/orcaflex/template_generator.py` | `TemplateGenerator._generate_reference` | [docs/registry/implementation-map.yaml:L8044-L8053] |
| 304 | `src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py` | `InstallationVtoHost.installation_step` | [docs/registry/implementation-map.yaml:L8054-L8063] |
| 305 | `src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py` | `InstallationVtoHost.get_target_line_length` | [docs/registry/implementation-map.yaml:L8064-L8073] |
| 306 | `src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py` | `InstallationVtoHost.get_target_reference_distance` | [docs/registry/implementation-map.yaml:L8074-L8083] |
| 307 | `src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py` | `Lower2ndEnd.installation_step` | [docs/registry/implementation-map.yaml:L8084-L8093] |
| 308 | `src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py` | `Lower2ndEnd.get_target_line_length` | [docs/registry/implementation-map.yaml:L8094-L8103] |
| 309 | `src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py` | `Lower2ndEnd.get_target_reference_distance` | [docs/registry/implementation-map.yaml:L8104-L8113] |
| 310 | `src/digitalmodel/solvers/orcaflex/universal/universal_runner.py` | `UniversalOrcaFlexRunner._process_single_model` | [docs/registry/implementation-map.yaml:L8114-L8123] |
| 311 | `src/digitalmodel/solvers/orcaflex/universal_cli.py` | `create_config` | [docs/registry/implementation-map.yaml:L8124-L8133] |
| 312 | `src/digitalmodel/solvers/orcaflex/yaml_timestep_validation.py` | `_mapping_issues` | [docs/registry/implementation-map.yaml:L8134-L8143] |
| 313 | `src/digitalmodel/solvers/orcaflex/yaml_timestep_validation.py` | `inspect_source` | [docs/registry/implementation-map.yaml:L8144-L8155] |
| 314 | `src/digitalmodel/solvers/orcaflex/yaml_utils.py` | `orcaflex_dump` | [docs/registry/implementation-map.yaml:L8156-L8165] |
| 315 | `src/digitalmodel/solvers/orcaflex/yaml_utils.py` | `orcaflex_load` | [docs/registry/implementation-map.yaml:L8166-L8175] |
| 316 | `src/digitalmodel/solvers/orcaflex/yaml_validator.py` | `OrcaFlexYAMLValidator._detect_format` | [docs/registry/implementation-map.yaml:L8176-L8185] |
| 317 | `src/digitalmodel/solvers/orcaflex/yaml_validator.py` | `OrcaFlexYAMLValidator._check_singular_section_keys` | [docs/registry/implementation-map.yaml:L8186-L8195] |
| 318 | `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | `OrcaWaveOrchestrator._create_vessel_config` | [docs/registry/implementation-map.yaml:L8196-L8205] |
| 319 | `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` | `OrcaWaveOrchestrator.list_available_vessels` | [docs/registry/implementation-map.yaml:L8206-L8215] |
| 320 | `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | `OrcaFlexConverter.save_orcaflex_file` | [docs/registry/implementation-map.yaml:L8216-L8227] |
| 321 | `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` | `OrcaFlexConverter._save_compact_format` | [docs/registry/implementation-map.yaml:L8228-L8237] |
| 322 | `src/digitalmodel/solvers/smoke/model_manifest.py` | `_tree` | [docs/registry/implementation-map.yaml:L8238-L8247] |
| 323 | `src/digitalmodel/solvers/smoke/model_probe.py` | `check_settings` | [docs/registry/implementation-map.yaml:L8248-L8257] |
| 324 | `src/digitalmodel/solvers/smoke/probes.py` | `_saved_proof` | [docs/registry/implementation-map.yaml:L8258-L8267] |
| 325 | `src/digitalmodel/subsea/catenary_riser/legacy/getFatigueLoading.py` | `prepareFatiguePostProcessing` | [docs/registry/implementation-map.yaml:L8268-L8279] |
| 326 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `orcaflexModel` | [docs/registry/implementation-map.yaml:L8280-L8290] |
| 327 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `lineTypes` | [docs/registry/implementation-map.yaml:L8291-L8300] |
| 328 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `WriteOrcaflexModel` | [docs/registry/implementation-map.yaml:L8301-L8310] |
| 329 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `build_model` | [docs/registry/implementation-map.yaml:L8311-L8323] |
| 330 | `src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py` | `OrcaFlexModelGenerator.generate_model_yml` | [docs/registry/implementation-map.yaml:L8324-L8333] |
| 331 | `src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py` | `OrcaFlexModelGenerator.generate_model_dict` | [docs/registry/implementation-map.yaml:L8334-L8343] |
| 332 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.get_fea_properties` | [docs/registry/implementation-map.yaml:L8344-L8353] |
| 333 | `src/digitalmodel/viv_screening/workflow.py` | `save_echo` | [docs/registry/implementation-map.yaml:L8354-L8363] |
| 334 | `src/digitalmodel/web/digitaltwinfeed/BuoySALM/BuoySALM.py` | `OrcaFlex.get` | [docs/registry/implementation-map.yaml:L8364-L8373] |
| 335 | `src/digitalmodel/web/digitaltwinfeed/FFS/FFS.py` | `OrcaFlex.get` | [docs/registry/implementation-map.yaml:L8374-L8383] |
| 336 | `src/digitalmodel/web/digitaltwinfeed/OffshoreRiser/OffshoreRiser.py` | `OrcaFlex.get` | [docs/registry/implementation-map.yaml:L8384-L8393] |
| 337 | `src/digitalmodel/workflows/automation/go_by_folder/metadata/analysis.py` | `AnalysisMetadataGenerator._detect_workflow_type` | [docs/registry/implementation-map.yaml:L8394-L8403] |
| 338 | `src/digitalmodel/workflows/installation_added_mass_bracket.py` | `integration_basis` | [docs/registry/implementation-map.yaml:L8404-L8413] |
| 339 | `src/digitalmodel/workflows/installation_added_mass_bracket.py` | `named_body` | [docs/registry/implementation-map.yaml:L8414-L8423] |
| 340 | `src/digitalmodel/workflows/installation_added_mass_bracket.py` | `expected_change` | [docs/registry/implementation-map.yaml:L8424-L8435] |
| 341 | `src/digitalmodel/workflows/installation_added_mass_evidence.py` | `read_cases` | [docs/registry/implementation-map.yaml:L8436-L8445] |
| 342 | `src/digitalmodel/workflows/installation_added_mass_materialize.py` | `_write_request` | [docs/registry/implementation-map.yaml:L8446-L8457] |
| 343 | `src/digitalmodel/workflows/installation_added_mass_materialize.py` | `materialize_bracket_case` | [docs/registry/implementation-map.yaml:L8458-L8470] |
| 344 | `src/digitalmodel/workflows/installation_hydrodynamic_sensitivity.py` | `_source` | [docs/registry/implementation-map.yaml:L8471-L8480] |
| 345 | `src/digitalmodel/workflows/installation_hydrodynamic_sensitivity.py` | `_case_files` | [docs/registry/implementation-map.yaml:L8481-L8492] |
| 346 | `src/digitalmodel/workflows/installation_numerical_compare.py` | `_apply_change` | [docs/registry/implementation-map.yaml:L8493-L8505] |
| 347 | `src/digitalmodel/workflows/installation_numerical_compare.py` | `_canonical_tables` | [docs/registry/implementation-map.yaml:L8506-L8517] |
| 348 | `src/digitalmodel/workflows/installation_numerical_compare.py` | `_runtime` | [docs/registry/implementation-map.yaml:L8518-L8527] |
| 349 | `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_field` | [docs/registry/implementation-map.yaml:L8528-L8537] |
| 350 | `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_line` | [docs/registry/implementation-map.yaml:L8538-L8547] |
| 351 | `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_value` | [docs/registry/implementation-map.yaml:L8548-L8560] |
| 352 | `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_anchors` | [docs/registry/implementation-map.yaml:L8561-L8572] |
| 353 | `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_timing` | [docs/registry/implementation-map.yaml:L8573-L8582] |
| 354 | `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_ordered` | [docs/registry/implementation-map.yaml:L8583-L8596] |
| 355 | `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_apply_native` | [docs/registry/implementation-map.yaml:L8597-L8608] |
| 356 | `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_counts` | [docs/registry/implementation-map.yaml:L8609-L8619] |
| 357 | `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_variation` | [docs/registry/implementation-map.yaml:L8620-L8632] |
| 358 | `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_native_case` | [docs/registry/implementation-map.yaml:L8633-L8647] |
| 359 | `src/digitalmodel/workflows/installation_numerical_prepare.py` | `_request` | [docs/registry/implementation-map.yaml:L8648-L8657] |
| 360 | `src/digitalmodel/workflows/installation_seastates.py` | `_snapshot` | [docs/registry/implementation-map.yaml:L8658-L8667] |
| 361 | `src/digitalmodel/workflows/installation_seastates.py` | `_structure` | [docs/registry/implementation-map.yaml:L8668-L8677] |
| 362 | `src/digitalmodel/workflows/installation_seastates.py` | `_request` | [docs/registry/implementation-map.yaml:L8678-L8687] |
| 363 | `src/digitalmodel/workflows/installation_seastates.py` | `generate_case` | [docs/registry/implementation-map.yaml:L8688-L8701] |
| 364 | `src/digitalmodel/workflows/installation_seastates.py` | `prepare_matrix` | [docs/registry/implementation-map.yaml:L8702-L8714] |
| 365 | `src/digitalmodel/workflows/installation_seastates.py` | `_change_payload` | [docs/registry/implementation-map.yaml:L8715-L8724] |
| 366 | `src/digitalmodel/workflows/installation_seastates.py` | `materialize_case` | [docs/registry/implementation-map.yaml:L8725-L8739] |
| 367 | `src/digitalmodel/workflows/installation_variant_study.py` | `_verify_master` | [docs/registry/implementation-map.yaml:L8740-L8749] |
| 368 | `src/digitalmodel/workflows/installation_variant_study.py` | `_verify_settings` | [docs/registry/implementation-map.yaml:L8750-L8759] |
| 369 | `src/digitalmodel/workflows/installation_variant_study.py` | `prepare_variant_study` | [docs/registry/implementation-map.yaml:L8760-L8773] |
| 370 | `src/digitalmodel/workflows/mcp_server/orcawave/core/mcp_server.py` | `OrcaWaveMCPServer._register_tools.export_to_orcaflex` | [docs/registry/implementation-map.yaml:L8774-L8783] |
| 371 | `src/digitalmodel/workflows/orcaflex_reproduce.py` | `_prepare` | [docs/registry/implementation-map.yaml:L8784-L8793] |
| 372 | `src/digitalmodel/workflows/orcaflex_reproduce_results.py` | `extract_results` | [docs/registry/implementation-map.yaml:L8794-L8803] |
| 373 | `src/digitalmodel/workflows/parametric_run.py` | `_write_yaml` | [docs/registry/implementation-map.yaml:L8804-L8813] |
| 374 | `src/digitalmodel/workflows/skills/skill_resolver.py` | `SkillResolver._parse_skill_file` | [docs/registry/implementation-map.yaml:L8814-L8823] |
| 375 | `src/digitalmodel/workflows/structure_hydrodynamic_candidate.py` | `_model` | [docs/registry/implementation-map.yaml:L8824-L8833] |
| 376 | `src/digitalmodel/workflows/structure_hydrodynamic_candidate.py` | `_apply` | [docs/registry/implementation-map.yaml:L8834-L8843] |
| 377 | `src/digitalmodel/workflows/structure_hydrodynamic_candidate.py` | `build_candidate` | [docs/registry/implementation-map.yaml:L8844-L8856] |
| 378 | `src/digitalmodel/workflows/structure_installation_prepare.py` | `_dependencies` | [docs/registry/implementation-map.yaml:L8857-L8866] |
| 379 | `tests/drilling_riser/global_model/test_batch_params.py` | `base_spec` | [docs/registry/implementation-map.yaml:L8867-L8876] |
| 380 | `tests/drilling_riser/global_model/test_campaign.py` | `base_spec` | [docs/registry/implementation-map.yaml:L8877-L8886] |
| 381 | `tests/drilling_riser/global_model/test_event_models.py` | `_case` | [docs/registry/implementation-map.yaml:L8887-L8896] |
| 382 | `tests/drilling_riser/global_model/test_hang_off.py` | `_generic` | [docs/registry/implementation-map.yaml:L8897-L8908] |
| 383 | `tests/drilling_riser/global_model/test_hang_off.py` | `_case` | [docs/registry/implementation-map.yaml:L8909-L8918] |
| 384 | `tests/drilling_riser/global_model/test_open_water.py` | `_ow_case` | [docs/registry/implementation-map.yaml:L8919-L8928] |
| 385 | `tests/drilling_riser/global_model/test_statics_path.py` | `base_spec` | [docs/registry/implementation-map.yaml:L8929-L8938] |
| 386 | `tests/drilling_riser/global_model/test_tensioner_representation.py` | `_line` | [docs/registry/implementation-map.yaml:L8939-L8948] |
| 387 | `tests/drilling_riser/global_model/test_w5_channels.py` | `base_spec` | [docs/registry/implementation-map.yaml:L8949-L8958] |
| 388 | `tests/drilling_riser/postprocess/test_w510_significant_range.py` | `_doc` | [docs/registry/implementation-map.yaml:L8959-L8970] |
| 389 | `tests/legal/test_sanitize_s7_audit_private.py` | `setup` | [docs/registry/implementation-map.yaml:L8971-L8980] |
| 390 | `tests/maintenance/test_archive_safety.py` | `test_deep_reference_and_registry_paths_are_excluded` | [docs/registry/implementation-map.yaml:L8981-L8990] |
| 391 | `tests/maintenance/test_archive_safety.py` | `test_generator_partitions_move_keep_hold_and_excluded` | [docs/registry/implementation-map.yaml:L8991-L9000] |
| 392 | `tests/orcaflex/test_generic_scr_slwr.py` | `test_specs_match_basis_and_static_geometry` | [docs/registry/implementation-map.yaml:L9001-L9010] |
| 393 | `tests/orcaflex/test_generic_scr_slwr.py` | `test_analytical_reference_reproduces_from_basis` | [docs/registry/implementation-map.yaml:L9011-L9020] |
| 394 | `tests/solvers/orcaflex/format_converter/test_spec_conversions.py` | `TestModularToSpecBoundaryReporting.test_expected_gaps_classified` | [docs/registry/implementation-map.yaml:L9021-L9030] |
| 395 | `tests/solvers/orcaflex/format_converter/test_spec_conversions.py` | `TestModularToSpecBoundaryReporting.test_actionable_gaps_classified` | [docs/registry/implementation-map.yaml:L9031-L9040] |
| 396 | `tests/solvers/orcaflex/format_converter/test_spec_conversions.py` | `TestModularToSpecBoundaryReporting.test_all_unmapped_are_categorized` | [docs/registry/implementation-map.yaml:L9041-L9050] |
| 397 | `tests/solvers/orcaflex/modular_generator/builders/test_general_builder.py` | `_build` | [docs/registry/implementation-map.yaml:L9051-L9060] |
| 398 | `tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py` | `_build` | [docs/registry/implementation-map.yaml:L9061-L9070] |
| 399 | `tests/solvers/orcaflex/modular_generator/builders/test_linetype_builder.py` | `_build` | [docs/registry/implementation-map.yaml:L9071-L9080] |
| 400 | `tests/solvers/orcaflex/modular_generator/goldens/basefile/capture.py` | `main` | [docs/registry/implementation-map.yaml:L9081-L9090] |
| 401 | `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_floating_campaign` | [docs/registry/implementation-map.yaml:L9091-L9100] |
| 402 | `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_slay_campaign` | [docs/registry/implementation-map.yaml:L9101-L9110] |
| 403 | `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py` | `_write_campaign_yaml` | [docs/registry/implementation-map.yaml:L9111-L9120] |
| 404 | `tests/solvers/orcaflex/modular_generator/test_collection_collision.py` | `_write_model` | [docs/registry/implementation-map.yaml:L9121-L9130] |
| 405 | `discovery-405 (identifying path withheld)` | `withheld` | [docs/registry/implementation-map.yaml:L9131-L9140] |
| 406 | `discovery-406 (identifying path withheld)` | `withheld` | [docs/registry/implementation-map.yaml:L9141-L9152] |
| 407 | `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py` | `TestJumperLineTypes.test_three_jumper_variants_emitted` | [docs/registry/implementation-map.yaml:L9153-L9162] |
| 408 | `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py` | `TestJumperLineReferences.test_jumper_line_uses_all_three_variants` | [docs/registry/implementation-map.yaml:L9163-L9172] |
| 409 | `tests/solvers/orcaflex/modular_generator/test_riser_variant_semantic_proof.py` | `_assert_line_type_references_resolve` | [docs/registry/implementation-map.yaml:L9173-L9182] |
| 410 | `tests/solvers/orcaflex/modular_generator/test_semantic_roundtrip.py` | `_write_mono_yaml` | [docs/registry/implementation-map.yaml:L9183-L9192] |
| 411 | `discovery-411 (identifying path withheld)` | `withheld` | [docs/registry/implementation-map.yaml:L9193-L9202] |
| 412 | `tests/solvers/orcaflex/reporting/fixture_helpers.py` | `_build_boundary_conditions` | [docs/registry/implementation-map.yaml:L9203-L9212] |
| 413 | `tests/solvers/orcaflex/single_file_parallel_test.py` | `SingleFileParallelStrategy._get_line_results` | [docs/registry/implementation-map.yaml:L9213-L9222] |
| 414 | `tests/solvers/orcaflex/single_file_parallel_test.py` | `SingleFileParallelStrategy._get_vessel_results` | [docs/registry/implementation-map.yaml:L9223-L9232] |
| 415 | `tests/solvers/orcaflex/test_model_catalogue.py` | `test_structural_owners_and_path_references` | [docs/registry/implementation-map.yaml:L9233-L9242] |
| 416 | `tests/solvers/orcaflex/test_model_catalogue_references.py` | `test_same_named_unrelated_folders_are_not_references` | [docs/registry/implementation-map.yaml:L9243-L9252] |
| 417 | `tests/solvers/orcaflex/test_model_header_guard.py` | `header_repository_findings` | [docs/registry/implementation-map.yaml:L9253-L9262] |
| 418 | `tests/solvers/orcaflex/test_modular_input_validation_fixes.py` | `master_with_missing_include` | [docs/registry/implementation-map.yaml:L9263-L9272] |
| 419 | `tests/solvers/orcaflex/test_slay_layout.py` | `test_support_types_are_defined_before_vessel_references` | [docs/registry/implementation-map.yaml:L9273-L9282] |
| 420 | `tests/solvers/orcaflex/test_slay_load_form.py` | `_resolve_case.read` | [docs/registry/implementation-map.yaml:L9283-L9292] |
| 421 | `tests/solvers/orcaflex/test_slay_load_form.py` | `_pipe_seed_points` | [docs/registry/implementation-map.yaml:L9293-L9302] |
| 422 | `tests/solvers/orcaflex/test_template_generator.py` | `sample_base_template` | [docs/registry/implementation-map.yaml:L9303-L9312] |
| 423 | `tests/solvers/orcaflex/test_template_generator.py` | `sample_variation` | [docs/registry/implementation-map.yaml:L9313-L9322] |
| 424 | `tests/solvers/orcaflex/test_template_generator.py` | `sample_case_template` | [docs/registry/implementation-map.yaml:L9323-L9332] |
| 425 | `tests/solvers/orcaflex/test_template_generator.py` | `TestTemplateGenerator.test_generate_with_basefile_reference` | [docs/registry/implementation-map.yaml:L9333-L9342] |
| 426 | `tests/solvers/orcaflex/test_template_generator.py` | `TestTemplateGenerator.test_reference_paths_resolve_from_arbitrary_output_dir` | [docs/registry/implementation-map.yaml:L9343-L9352] |
| 427 | `tests/solvers/orcaflex/test_template_generator.py` | `TestTemplateGenerator.test_reference_paths_in_cases_dir_stay_relative` | [docs/registry/implementation-map.yaml:L9353-L9362] |
| 428 | `tests/solvers/smoke/test_model_manifest.py` | `manifest` | [docs/registry/implementation-map.yaml:L9363-L9372] |
| 429 | `tests/test_engine.py` | `temp_config_file` | [docs/registry/implementation-map.yaml:L9373-L9382] |
| 430 | `tests/test_main_exit_status.py` | `_solve_input` | [docs/registry/implementation-map.yaml:L9383-L9392] |
| 431 | `tests/test_skill_resolver.py` | `_write_skill` | [docs/registry/implementation-map.yaml:L9393-L9402] |
| 432 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | [docs/registry/implementation-map.yaml:L9403-L9412] |
| 433 | `tests/workflows/test_durable_workflows.py` | `test_synthetic_rope_tn_curve_matches_dnv_os_e301_table_f3` | [docs/registry/implementation-map.yaml:L9413-L9422] |
| 434 | `tests/workflows/test_durable_workflows.py` | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | [docs/registry/implementation-map.yaml:L9423-L9432] |
| 435 | `tests/workflows/test_durable_workflows.py` | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | [docs/registry/implementation-map.yaml:L9433-L9442] |
| 436 | `tests/workflows/test_durable_workflows.py` | `test_vessel_seakeeping_significant_amplitude_matches_library` | [docs/registry/implementation-map.yaml:L9443-L9452] |
| 437 | `tests/workflows/test_installation_instability_diagnostic.py` | `fake` | [docs/registry/implementation-map.yaml:L9453-L9462] |
| 438 | `tests/workflows/test_orcaflex_batch_contract.py` | `test_duration_render_preserves_native_default_spelling_and_source` | [docs/registry/implementation-map.yaml:L9463-L9472] |
| 439 | `tests/workflows/test_orcaflex_reproduce.py` | `resume_case` | [docs/registry/implementation-map.yaml:L9473-L9482] |
| 440 | `tests/workflows/test_structure_hydrodynamic_candidate.py` | `case` | [docs/registry/implementation-map.yaml:L9483-L9493] |

7 discovery files with identifying names are represented by neutral row identifiers; no identifying file names are transcribed (issue pending (2)).

Exact case-sensitive Python `re` signals used, with OR matching across signals; the case-insensitive package-path exclusion removes 0 signals because none matches `parametrics`:

```text
(?:yaml\.(?:dump|safe_dump)|write_text|open\(|\.write\(|save\(|SaveData|dump\().*|to_orcaflex_dict|ObjectType|EndAConnection|LineConfig|VesselConfig|WaveTrains|CurrentRamp|WindSpeed|\.write\(|\.ya?ml|yaml\.safe_dump|shutil\.copy|\.write\(|write_model|ModularModelGenerator|ProjectInputSpec|yaml\.safe_dump|\.write_text
orcaflex|OrcaFlex|includefile|modular_generator
[\"\'](?:Lines|LineTypes|Vessels|6DBuoys|General|IncludeFile|includefile)[\"\']|%YAML
```

[docs/registry/implementation-map.yaml:L4933-L4938]

At the code pin, 15 matching lines belong to 6 enclosing definitions (module-level statements count together); all are listed below. Zero further second implementations remain after the listed writer and delegation/docstring exclusions.

| Definition | Matching lines | Classification | Evidence |
|---|---|---|---|
| `__init__` module | 4, 14, 18 | Re-export and docstrings | [src/digitalmodel/parametrics/__init__.py:L1-L23] |
| `atlas_bridge` module | 15 | Imported data types | [src/digitalmodel/parametrics/atlas_bridge.py:L15-L19] |
| `pilots` module | 6, 17, 26, 125 | Docstring, imports, source-spec constant and comment | [src/digitalmodel/parametrics/pilots.py:L1-L27] [src/digitalmodel/parametrics/pilots.py:L125-L125] |
| `orcawave_depth_sweep.runner` | 104, 110, 111 | Existing inventory row 184; diffraction-spec writer, not an additional OrcaFlex implementation | [src/digitalmodel/parametrics/pilots.py:L100-L122] |
| `orcaflex_fowt_watch_circle_sweep` | 128, 137, 148 | Delegated analytical runner, already described in section 3; no model writer or launch | [src/digitalmodel/parametrics/pilots.py:L128-L180] |
| `sweep` module | 13 | Imported data types | [src/digitalmodel/parametrics/sweep.py:L13-L17] |

The in-package map row is a vocabulary-driven classification candidate: it emits OrcaWave diffraction specifications rather than OrcaFlex model text; retain its inventory membership for this map revision and request family correction in issue pending (2). [src/digitalmodel/parametrics/pilots.py:L76-L122]

Repository-wide discovery, including scripts, used:

```bash
git ls-files '*.py'
rg -n 'ParametricStudy|itertools\.product|def (run_sweep|atlas_from_summary)|RegularGridInterpolator' --glob '*.py' src scripts tests docs examples
```

The tracked-file Python scan applied that expression to every tracked `.py` file, including root files, and found 190 matching lines in 61 files. These are discovery counts, not 61 independent implementations.

| Related implementation | Relation / map follow-up | Evidence |
|---|---|---|
| `ParametricStudy.generate_case_matrix` | Canonical delegated Cartesian expansion; no local second expander | [src/digitalmodel/orcaflex/batch_parametric.py:L78-L111] |
| `BatchRunner.generate_run_matrix` | Separate ANSYS Cartesian/zip expansion; uncounted sweep-family gap | [src/digitalmodel/ansys/batch_runner.py:L149-L172] |
| OpenFOAM `ParametricStudy.generate_cases` | Separate Cartesian case expansion; uncounted sweep-family gap | [src/digitalmodel/solvers/openfoam/parametric.py:L99-L130] |
| `parametric.generate._grid_points`, `generate_atlas` | Separate grid generation/holdout build, not the local summary adapter | [src/digitalmodel/parametric/generate.py:L587-L612] [src/digitalmodel/parametric/generate.py:L623-L711] |
| `Atlas._interpolator` | Separate response interpolation; local code constructs its data | [src/digitalmodel/parametric/atlas.py:L139-L156] [src/digitalmodel/parametrics/atlas_bridge.py:L70-L80] |

Map follow-up: add full-factorial sweep orchestration, atlas-grid adaptation and pilot response-quotient families with in-package rows before counting them; review the external expansion implementations and the misclassified diffraction writer (issue pending (2)). Existing physical solvers are dependencies rather than local reimplementations. [src/digitalmodel/parametrics/sweep.py:L24-L43] [src/digitalmodel/parametrics/atlas_bridge.py:L22-L80] [src/digitalmodel/parametrics/pilots.py:L157-L180]

## 10. Coverage and open items

All four package Python files and all four test modules were read; there are no nested source packages in the reviewed scope. [src/digitalmodel/parametrics/__init__.py:L1-L34] [src/digitalmodel/parametrics/sweep.py:L1-L43] [src/digitalmodel/parametrics/pilots.py:L1-L180] [src/digitalmodel/parametrics/atlas_bridge.py:L1-L80] [tests/parametrics/test_sweep.py:L1-L54] [tests/parametrics/test_pilots.py:L1-L51] [tests/parametrics/test_atlas_bridge.py:L1-L82] [tests/parametrics/test_atlas_serving.py:L1-L77]
Section 4 lists every collected node; no package-local tests folder was found. No example-script count is adopted from the inventory lead; the executable evidence comprises these tests and the source-template path used by the diffraction pilot. [tests/parametrics/test_sweep.py:L23-L54] [tests/parametrics/test_pilots.py:L16-L51] [tests/parametrics/test_atlas_bridge.py:L37-L82] [tests/parametrics/test_atlas_serving.py:L56-L77] [src/digitalmodel/parametrics/pilots.py:L24-L27]

Documentation discovery/read inventory:

| Document | Coverage |
|---|---|
| `docs/plans/parametrics-preparedness.md` | Sweep plan, reuse table, phasing and open dispatch dependency; planned pilot choices differ from code. [docs/plans/parametrics-preparedness.md:L1-L98] |
| `docs/maps/digitalmodel-operator-map.md` | Module route and dependencies. [docs/maps/digitalmodel-operator-map.md:L95-L95] |
| `docs/registry/module-routing.yaml` | Existing development entry and test directory. [docs/registry/module-routing.yaml:L696-L701] |
| `docs/registry/implementation-map.yaml`, `docs/registry/implementation-map.md` | Family inventory read at the explicit map pin; summary also found at code pin. [docs/registry/implementation-map.yaml:L1-L4] [docs/registry/implementation-map.md:L20-L20] |
| `docs/plans/parametric-refresh/adding-an-atlas.md` | Adjacent analytical-atlas authoring guide, including holdout and nonextrapolation requirements. [docs/plans/parametric-refresh/adding-an-atlas.md:L1-L23] [docs/plans/parametric-refresh/adding-an-atlas.md:L85-L111] |
| `docs/plans/2026-09-13-issue-2117-padeye-parametric.html` | Summary-adapter reuse proposal, not evidence of execution. [docs/plans/2026-09-13-issue-2117-padeye-parametric.html:L8-L8] |
| `docs/migration/legacy-batch-workflows.md` | Adjacent reuse and proposed workflow wrapper; read for method discovery. [docs/migration/legacy-batch-workflows.md:L89-L96] [docs/migration/legacy-batch-workflows.md:L134-L137] |
| `docs/domains/marine-engineering/marine_3d_models_catalog.md` | Generic word match about mesh format, not this package. [docs/domains/marine-engineering/marine_3d_models_catalog.md:L192-L192] |

The calculation-citation contract was readable; the standard-dependent cable reference is delegated, and no new physics coefficient or citation schema is introduced here. [src/digitalmodel/parametrics/pilots.py:L137-L177] [src/digitalmodel/orcaflex/mooring_design_fowt.py:L277-L304]
No private data was used; no private-sidecar entry is required for the synthetic grids and public source statements included here. [tests/parametrics/test_sweep.py:L13-L39] [tests/parametrics/test_pilots.py:L16-L51]

| Open item | Responsibility | Tracking |
|---|---|---|
| Configured missing standards page prevents FOWT and atlas tests from passing; restore reference content then rerun all 12 nodes | Calculation/citation maintainers | #574; section 6 |
| False zero validation, duplicate/missing Cartesian membership and finite-response coverage | Atlas maintainers | #2118; S07, S20, S21 |
| Boundary, default, units, rounding and field-semantics detection gaps; shared lesson registration | Calculation maintainers | issue pending (1); S02, S04–S06, S08–S19 and section 7 |
| Map family coverage, diffraction-writer classification and codenaming of discovery paths | Registry maintainers | issue pending (2); section 9 |
| Unverified claim from section 9: target solver versions and dependency inventory-row identifiers; add independent physical worked examples, appoint deputy/reviewer and record clearance | Calculation maintainers | issue pending (3) |
| Plan/implemented pilot mismatch and the documented lack of a licensed dispatch/result-return route | Documentation maintainers | #943; [docs/plans/parametrics-preparedness.md:L71-L91] |

AFTER results: 12 run, 4 PASS, 8 FAIL, 0 SKIP, 0 NOT_RUN; exactly the same node outcomes as BASE at the header code revision, using the same `uv run --no-sources python -m pytest` form on the uncommitted tree. The 8 failed nodes are all listed individually in section 4. These unchanged failures are baseline evidence; they do not establish successful physical validation. Physical qualification remains open; neither prepared APDL nor prepared diffraction specifications establish a solved benchmark. [src/digitalmodel/ansys/padeye.py:L86-L90] [src/digitalmodel/parametrics/pilots.py:L113-L120]
