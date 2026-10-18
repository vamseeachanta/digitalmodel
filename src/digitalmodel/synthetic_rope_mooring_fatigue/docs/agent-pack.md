# Synthetic rope mooring fatigue knowledge pack

| Header | Value |
|---|---|
| Module | synthetic_rope_mooring_fatigue |
| Repository | digitalmodel |
| Package | src/digitalmodel/synthetic_rope_mooring_fatigue |
| Pack revision | 0 |
| Code revision | 7aad603be1c6136106dbb2381ce5096b050654f8 |
| Analysis family | Engineering calculations (EC) |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

`router` screens each synthetic rope for tension-range fatigue, creep rupture and minimum-tension compression, and requires all three margins to reach unity; the registry assigns development maturity. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L98-L133] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L195-L243] [docs/registry/module-routing.yaml:L502-L507]

Metallic materials are rejected; polyester and generic synthetic materials are admitted, nylon requires its own T-N model, and HMPE requires both T-N and creep models. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L246-L271]

This is an algebraic screening calculation using prescribed tension bins, rather than a geometry, load-history or solver calculation; temperature is carried to outputs but does not enter the fatigue or creep expressions. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L136-L192] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L195-L243]

## 2. Inputs and outputs

Inputs reside in `cfg["synthetic_rope_mooring_fatigue"]`; absent numeric model parameters raise errors rather than acquiring standard coefficients. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L17-L35] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L300-L361]

| Input | Unit | Range / default | Source and evidence |
|---|---|---|---|
| design_life_years | years | Required, positive | Configuration; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L19-L23] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L342-L349] |
| dff; creep_safety_factor | dimensionless | Required, positive, separate limit states | Configuration; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L24-L32] |
| tn_curve.intercept; slope | cycles; dimensionless | Required, positive | Global model, overridden per line; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L108-L109] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L300-L309] |
| mean_load_knockdown | dimensionless | Required, nonnegative | T-N model; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L300-L309] |
| creep.reference_life_years | years | Required, positive | Creep model; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L312-L329] |
| creep.load_ratio_ref; decades_per_load_ratio | dimensionless | Required, positive | Creep model; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L312-L329] |
| min_tension.min_ratio_allow | dimensionless | Required, positive | Global minimum-tension criterion; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L332-L339] |
| lines | records | Nonempty list | Configuration; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L274-L280] |
| id / name; material | text | ID falls back to name then line; material required | Line record; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L139-L141] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L246-L257] |
| MBL_kN; mean_tension_kN | kN | Required, positive | Line record; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L142-L147] |
| min_tension_kN | kN | Required, nonnegative | Line record; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L148-L152] |
| temperature_C | degrees C | Optional, absent becomes None; no finite-range validation | Line record; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L153-L156] |
| tension_range_bins | records | Nonempty list; range in kN and n_cycles in cycles both positive | Line record; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L283-L297] |
| output_dir | directory | Default results, relative to configuration directory | Configuration; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L364-L388] |

There is no coordinate frame, origin or phase input in the calculation interface; force ratios use the same kN basis and life ratios use years, without unit conversion. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L136-L175] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L195-L227]

| Output | Unit / format | Content and evidence |
|---|---|---|
| Per-bin CSV | kN, cycles, dimensionless damage | Line/material/temperature, bin, range, normalised range, count, allowable cycles, damage; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L168-L188] |
| Per-line summary CSV | years and dimensionless ratios | Fatigue life/margin, creep life/margin, minimum-tension ratio, compression margin, governing mechanism, passes; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L208-L243] |
| Returned configuration | mapping | Replaces module settings with summary, governing line, paths and screening status; also sets top-level screening_status; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L52-L95] |
| CSV paths | text | Input stem plus module suffix; headers from first row, text-mode CSV, encoding unspecified; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L364-L394] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L397-L412] |

## 3. Method and governing standards

`_fatigue_rows` calculates mean-load ratio from mean tension / MBL, effective intercept as intercept times exp(-knockdown times mean-load ratio), allowable cycles from effective intercept times normalised range to negative slope, and sums bin count / allowable cycles. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L142-L156] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L168-L192]

`_line_summary` calculates fatigue life as design life / total damage, fatigue margin as fatigue life / (design life times dff), creep life from a base-ten load-ratio law, and compression margin as minimum tension / MBL / minimum allowable ratio; the smallest margin governs. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L208-L243]

The example README names DNV-OS-E301, *Position mooring*, October 2010, Table F3 (polyester T-N design curve) and F702 (polyester fatigue safety factor); clause text not checked against the licensed copy. The implementation comment names F702 without an edition, and the test names October 2010 Table F3; no conflicting edition is stated in these locations. [examples/workflows/synthetic-rope-mooring-fatigue/README.md:L4-L4] [src/digitalmodel/data_systems/data_procurement/README.md:L682-L682] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L24-L27] [tests/workflows/test_durable_workflows.py:L1822-L1827]

UNVERIFIED: independent qualification of the mean-load knockdown, creep law and minimum-tension criterion is not established by a cited publication in the implementation; standards-derived results also lack a Citation sidecar in the returned summary. Qualification and provenance remain open in issue pending (2). [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L168-L175] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L208-L243] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L73-L95]

## 4. Validated worked examples

No independently checked standard text or benchmark dataset is retained in this pack; validated examples: 0. The standard-curve test is retained below as regression evidence pending source qualification, rather than treating its docstring as an independently checked standard. [tests/workflows/test_durable_workflows.py:L1822-L1884]

| Case / classification | Source, input and expectation | Tolerance | Existing test node ID | Offline result |
|---|---|---|---|---|
| Two-line workflow / regression | Example YAML; two lines, creep governs failing line, positive bin quantities | Exact statuses; damage > 0 dimensionless | `tests/workflows/test_durable_workflows.py::test_workflow_registry[synthetic-rope-mooring-fatigue]` | PASS at code pin, temporary copy; [tests/workflows/test_durable_workflows.py:L333-L356] |
| Standard-curve wiring / regression pending qualification | Test-generated YAML; expected cycles from the test's named DNV-OS-E301 October 2010 Table F3 expression; licensed coefficients omitted here | Relative error <= 1e-9 dimensionless | `tests/workflows/test_durable_workflows.py::test_synthetic_rope_tn_curve_matches_dnv_os_e301_table_f3` | PASS at code pin; [tests/workflows/test_durable_workflows.py:L1822-L1884] |
| Atlas interpolation / related regression | Generated grid compared with duplicated fatigue response; not an independent module check | Absolute zero-error tolerance 1e-9 dimensionless; prediction relative tolerance 1e-9 | `tests/parametric/test_atlas.py::test_synthetic_rope_fatigue_atlas_is_log_log_exact` | PASS at code pin; [tests/parametric/test_atlas.py:L202-L224] |
| Atlas bin handler / related regression | Registry atlas; requires in-range fatigue-life result > 0 years | Positive life and exact response/status | `tests/parametric/test_atlas.py::test_fatigue_bins_handler_shared_by_both_ropes` | PASS at code pin; [tests/parametric/test_atlas.py:L227-L244] |

BASE and AFTER results are each 4 passed, 0 failed, 0 skipped, 0 NOT_RUN after completing the temporary test copy's helper imports; the first copy failed collection with `ModuleNotFoundError: No module named 'tests.workflows.field_dev_production_assertions'`, before the unchanged helper was copied. Test expectations are the assertions cited above. [tests/workflows/test_durable_workflows.py:L16-L19] [tests/workflows/test_durable_workflows.py:L333-L356] [tests/workflows/test_durable_workflows.py:L1879-L1884] [tests/parametric/test_atlas.py:L202-L244]

The command form is `PYTHONPATH=src uv run --no-sources python -m pytest <node IDs> -q -rA -p no:cacheprovider --basetemp=<scratch>`; workflow nodes run from an unchanged temporary copy because their registry case writes example results. [tests/workflows/test_durable_workflows.py:L57-L92] [docs/registry/workflows.yaml:L128-L137]

## 5. QA gates

Checks below are executable criteria for AQG r0, Python calculations; they define screening QA rather than a claim of engineering clearance. Function boundaries and quantities are pinned to the cited implementation. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L17-L70] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L168-L243]

| Gate | Applicability | Check ID / executable check | Numeric pass criterion and unit | Evidence / current disposition |
|---|---|---|---|---|
| G0 | Applicable: inputs, units, citations | C0: `_positive_float` and `_nonnegative_float` reject missing and out-of-domain inputs; inspect model provenance | 0 accepted invalid inputs, count; 0 unresolved model sources, count | Numeric domain guards exist; finite-value and provenance gaps remain, issue pending (1), issue pending (2); [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L342-L361] |
| G1 | Applicable: imports and tests | C1: run both workflow nodes in section 4 through the engine's module route | 2 passed nodes, 0 failed nodes, count | BASE PASS; engine route and test assertions; [src/digitalmodel/engine.py:L333-L338] [tests/workflows/test_durable_workflows.py:L333-L356] |
| G2 | Applicable: hand or published check | C2: compare `_fatigue_rows` with count / (intercept * exp(-knockdown * mean/MBL) * (range/MBL)**(-slope)); compare `_line_summary` with direct scalar ratios | Relative error <= 1e-12 dimensionless for damage and margins, absolute error <= 1e-12 years for synthetic life check | Algebra-only hand check; publication qualification remains open, issue pending (2); [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L168-L175] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L208-L227] |
| G3 | NOT_APPLICABLE | No dynamic integration in this screening route | NOT_APPLICABLE | Prescribed bins evaluated directly; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L98-L133] |
| G4 | NOT_APPLICABLE | No diffraction model or mesh | NOT_APPLICABLE | Route reads line settings and writes CSV; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L17-L70] |
| G5 | Applicable: evidence appendix | C5: compare `_workflow_summary` paths, governing line and CSV rows against per-line margins; retain code pin, input digest and gate outcomes | 0 inconsistent governing/status fields, count; 0 missing required evidence fields, count | Functional fields checked by workflow regression; fingerprint, signed gate record and Citation appendix not emitted, issue pending (2); [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L73-L95] [tests/workflows/test_durable_workflows.py:L333-L356] |

## 6. Known failure modes and pitfalls

sweep: 14 found

The count is 14 reviewed groups of fallback, conversion, unit or error-handling sites, including benign guards; it is not a count of confirmed defects. Every identified group is recorded below. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L17-L412]

Exact sweep command (run over every package Python file):

```sh
rg -n 'except|continue|return None|return \[\]|return \{|= 0(\.0)?\b|1970|float\(|int\(|0\.05|cache|_kN|_years|temperature' src/digitalmodel/synthetic_rope_mooring_fatigue
```

| Group | Symptom / classification at pin | Detection / disposition |
|---|---|---|
| 1 | Missing settings or global model mappings become empty dictionaries, then required numeric fields raise; not a swallowed error. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L18-L35] | C0, count of accepted missing values = 0. |
| 2 | Empty per-line T-N or creep mapping silently selects global model. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L108-L109] | no detection check exists; issue pending (1). |
| 3 | Missing line ID falls back to name then a common default, without uniqueness enforcement. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L139-L141] | no detection check exists; issue pending (1). |
| 4 | Optional temperature becomes None or is converted with float, then only echoed; no thermal correction or finite check. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L153-L156] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L168-L188] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L208-L243] | no detection check exists; issue pending (1), issue pending (2). |
| 5 | Empty row/summary accumulators and zero damage accumulator are initial state; empty line/bin inputs and nonpositive damage raise. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L37-L50] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L168-L192] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L274-L297] | C1 verifies positive bin damage; no detection check exists for every invalid bin boundary; issue pending (1). |
| 6 | Fatigue assumes all force quantities share kN, normalises by MBL and treats supplied counts as design-life exposure; no conversion occurs. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L142-L175] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L208-L210] | C2 checks scalar arithmetic; no detection check exists for mixed units or exposure basis; issue pending (1). |
| 7 | Creep assumes years and dimensionless load ratios; compression assumes a consistent tension ratio; user factors determine acceptance. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L208-L227] | C2 checks scalar arithmetic; no detection check exists for model applicability; issue pending (2). |
| 8 | `_positive_float` uses float and a sign comparison only; nonfinite values are not rejected, locale-formatted numbers raise rather than normalise. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L342-L349] | C0 exposes nonfinite acceptance; no detection check exists in the module tests; issue pending (1). |
| 9 | `_nonnegative_float` has the same conversion/finite limitation; zero knockdown and minimum tension are admitted. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L300-L309] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L352-L361] | C0 exposes nonfinite acceptance; no detection check exists in the module tests; issue pending (1). |
| 10 | Output directory defaults to results and accepts absolute paths; this is an output-location fallback. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L376-L380] | no detection check exists; issue pending (1). |
| 11 | Missing configuration paths fall back to the current directory, so output location depends on caller state. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L383-L388] | no detection check exists; issue pending (1). |
| 12 | Missing configuration file stem falls back to basename or a fixed stem, so repeated calls can target the same files. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L391-L394] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L364-L373] | no detection check exists; issue pending (1). |
| 13 | `_display_path` catches ValueError and returns an absolute path outside the repository; output metadata may expose local locations. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L407-L412] | no detection check exists; issue pending (1). |
| 14 | Empty CSV rows raise explicitly; the two CSV writes are sequential and not transactional. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L53-L56] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L397-L404] | C1 checks successful files; no detection check exists for second-write failure; issue pending (1). |

No continue, return None, empty-list/dictionary error return, epoch default or cache write occurs in the package; the sole exception handler is the path-display fallback above. Ordinary result dictionaries are not error fallbacks. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L17-L412]

UNVERIFIED: no independent material or temperature applicability envelope is established for these supplied models; the code admits generic synthetic material while requiring explicit nylon/HMPE overrides. Model qualification remains open in issue pending (2). [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L246-L271]

## 7. Lessons log

| Date | Record | Lesson | Evidence | Pack change | Shared-log entry |
|---|---|---|---|---|---|
| 2026-10-17 | Initial evidence inventory | Atlas agreement is a regression check because its response independently repeats the same expression; it does not qualify creep or compression | [src/digitalmodel/parametric/generate.py:L48-L62] [tests/parametric/test_atlas.py:L202-L244] | Separate validated and regression evidence in sections 4 and 9 | Not assigned; issue pending (2) |

## 8. Ownership

Proposed ownership: engineering calculations quality manager; deputy: engineering calculations deputy quality manager; producing discipline: engineering calculations. Model qualification and disputed implementation classifications will be referred to the family quality manager; publication remains subject to maintainer review. These are proposed responsibilities, not completed review or approval records.

## 9. Dependencies and implementation inventory

The engine dispatches the basename to `router`; the package imports only csv, math, pathlib and typing. The durable workflow registry marks the workflow offline and lists its YAML input and CSV outputs. [src/digitalmodel/engine.py:L333-L338] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L3-L8] [docs/registry/workflows.yaml:L128-L137]

The parametric route calculates fatigue damage only; it explicitly leaves creep and minimum-tension checks to the full workflow. No solver version is an input to the algebraic package route. [src/digitalmodel/parametric/generate.py:L48-L62] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L17-L70]

Implementation-map snapshot: `docs/registry/implementation-map.yaml` at ref `origin/codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, declared data revision `270c8759c374b67a7f032953e7fcdb14e0224461`. Inventory: 0 accepted implementation rows in the package across 13 families and 2348 accepted rows; the rejected candidate `_validate_material_models` is not an accepted implementation. A8(a): empty inventory; A8(c): NOT_APPLICABLE because no map family has an in-package implementation row. [docs/registry/implementation-map.yaml:L1-L4] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L260-L271]

Counted families: 0; additional counted-family implementations: 0 (no counted-family grep applies). Map follow-up: add tension-range fatigue with mean-load knockdown, creep rupture and minimum-tension compression, then classify the separate parametric fatigue expression; issue pending (3). [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L159-L243] [src/digitalmodel/parametric/generate.py:L48-L62]

Repository-wide discovery included scripts using the following command; the separate parametric response computes its own fatigue result and is a map gap outside this package, rather than an A8 miss. [src/digitalmodel/parametric/generate.py:L48-L62]

```sh
git grep -n -E 'mean_load_knockdown|decades_per_load_ratio|synthetic_rope_mooring_fatigue' HEAD -- '*.py' '*.md' '*.yaml' '*.yml'
```

## 10. Coverage and open items

Coverage read: both package files (`__init__.py` is empty and `workflow.py` contains the route); the worked-example README and YAML; the durable workflow registry and relevant test branches; the two related atlas tests and duplicated parametric expression. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L1-L412] [src/digitalmodel/synthetic_rope_mooring_fatigue/__init__.py:L1-L1] [examples/workflows/synthetic-rope-mooring-fatigue/README.md:L1-L6] [examples/workflows/synthetic-rope-mooring-fatigue/input.yml:L1-L48] [docs/registry/workflows.yaml:L128-L150] [tests/workflows/test_durable_workflows.py:L333-L356] [tests/workflows/test_durable_workflows.py:L1822-L1884] [tests/parametric/test_atlas.py:L202-L244] [src/digitalmodel/parametric/generate.py:L48-L62]

Documentation discovery also found the operator-map row, routing entry, capability index, atlas guide and offering-plan inventory. Their maturity/count claims are not substituted for test results or engineering qualification. [docs/maps/digitalmodel-operator-map.md:L101-L101] [docs/registry/module-routing.yaml:L502-L507] [docs/capability-map/capabilities-added.yml:L66-L66] [docs/plans/parametric-refresh/adding-an-atlas.md:L109-L119] [docs/plans/2026-09-25-issue-1057-ffs-offering-program.md:L21-L21]

| Open item | Disposition / responsible discipline |
|---|---|
| Nonfinite inputs, silent model fallback, duplicate IDs, units/exposure, temperature metadata and output boundaries from section 6 | issue pending (1); calculation maintainers; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L108-L156] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L342-L412] |
| UNVERIFIED model qualification from sections 3 and 6: mean-load knockdown, creep, compression, material/temperature envelope, independently checked standard text | issue pending (2); engineering calculations quality manager; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L168-L243] [tests/workflows/test_durable_workflows.py:L1822-L1884] |
| Missing independent examples for all three mechanisms and provenance / signed gate appendix / shared-log reference | issue pending (2); calculation maintainers; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L73-L95] [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L195-L243] |
| Method-family map coverage and separate parametric fatigue computation | issue pending (3); map maintainers; [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L159-L243] [src/digitalmodel/parametric/generate.py:L48-L62] |

The draft remains unqualified for engineering acceptance until the independent evidence and gate-record items above are resolved; a passing regression suite alone does not establish the model envelope. [src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py:L73-L95] [tests/workflows/test_durable_workflows.py:L1822-L1884]
