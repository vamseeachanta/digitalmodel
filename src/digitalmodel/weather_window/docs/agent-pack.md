# Weather-window module knowledge pack

| Header | Value |
| --- | --- |
| Module / repository | `weather_window` / `digitalmodel` |
| Package / scope | `src/digitalmodel/weather_window`; both Python files, with the imported persistence implementation examined as a dependency |
| Pack revision / date | 0 / 2026-10-18 |
| Code pin | `f1d58a2ac0db65e68b5b690f0a609ef4746d114e` |
| Analysis family | Engineering calculations (`EC`); AQG r0, Python calculations |
| Quality role / status | Engineering calculations quality manager / `DRAFT` |

## 1. Purpose and scope

`assess_weather_window` screens a nonempty Hs record for workability, persistence statistics and continuous-window fit, then requires both window fit and any supplied workability threshold for a passing status. [src/digitalmodel/weather_window/workflow.py:L48-L99]

`router` reads the `weather_window` configuration, adds results and a summary path, and copies the screening status to the top-level configuration; the engine dispatches `basename: weather_window` to this router. [src/digitalmodel/weather_window/workflow.py:L102-L129] [src/digitalmodel/engine.py:L381-L384]

The registry maturity is `development`; its broad `tests/` routing entry does not itself establish calculation coverage. [docs/registry/module-routing.yaml:L692-L697]

The interface has scalar wave heights, a fixed sample interval and operation thresholds; it supplies no timestamp, direction, coordinate frame, spatial origin, phase, wind or current input. [src/digitalmodel/weather_window/workflow.py:L48-L54] [src/digitalmodel/weather_window/workflow.py:L102-L117]

## 2. Inputs and outputs

Input contract (ranges below describe implemented checks, rather than recommended physical limits): [src/digitalmodel/weather_window/workflow.py:L48-L80]

| Input | Unit | Implemented range / default | Route and evidence |
| --- | --- | --- | --- |
| `metocean.hs_timeseries_m` | m | Nonempty list; no per-sample range or finite-value guard | Required; each element is passed through `float`. [src/digitalmodel/weather_window/workflow.py:L60-L61] [src/digitalmodel/weather_window/workflow.py:L102-L110] |
| `operation.hs_limit_m` | m | Positive; required | `float` conversion then positive check. [src/digitalmodel/weather_window/workflow.py:L62-L63] [src/digitalmodel/weather_window/workflow.py:L108-L110] |
| `operation.required_duration_hours` | h | Positive; required | `float` conversion then positive check. [src/digitalmodel/weather_window/workflow.py:L64-L65] [src/digitalmodel/weather_window/workflow.py:L109-L110] |
| `metocean.time_step_hours` | h/sample | Positive; default 3.0 in both routes | Fixed interval multiplied by sample counts. [src/digitalmodel/weather_window/workflow.py:L30-L45] [src/digitalmodel/weather_window/workflow.py:L52-L67] [src/digitalmodel/weather_window/workflow.py:L111-L111] |
| `operation.required_workability_pct` | % | Default `None`; no range guard | Missing or null disables the workability threshold; supplied values pass through `float`. [src/digitalmodel/weather_window/workflow.py:L53-L54] [src/digitalmodel/weather_window/workflow.py:L78-L80] [src/digitalmodel/weather_window/workflow.py:L112-L116] |
| `outputs.directory`, `outputs.summary_json` | path / filename | Defaults `results/weather_window`, `weather_window_summary.json` | Relative directory resolves against `_config_dir_path`, otherwise the current directory. [src/digitalmodel/weather_window/workflow.py:L124-L126] [src/digitalmodel/weather_window/workflow.py:L132-L141] |

| Output | Unit / representation | Contract and evidence |
| --- | --- | --- |
| `record_hours`, `workability_pct` | h, % | Record length is sample count times interval; workability uses the helper's total below-limit hours divided by record hours. [src/digitalmodel/weather_window/workflow.py:L69-L89] |
| `longest_window_hours`, `mean_window_hours`, `p10_window_hours`, `p90_window_hours` | h | Copied from `analyse_persistence`, whose nonempty statistics round to one decimal place. [src/digitalmodel/weather_window/workflow.py:L91-L94] [src/digitalmodel/orcaflex/weather_window.py:L224-L234] |
| `num_windows`, `num_viable_windows` | count | Total from persistence; viable windows counted from unrounded local durations. [src/digitalmodel/weather_window/workflow.py:L72-L73] [src/digitalmodel/weather_window/workflow.py:L95-L96] |
| `operation_fits_window`, `screening_status` | boolean, `pass` / `fail` | Fit compares the helper's rounded maximum with required duration; final status also applies workability. [src/digitalmodel/weather_window/workflow.py:L77-L81] [src/digitalmodel/weather_window/workflow.py:L97-L98] |
| `standard`, input echoes | text, m, h, % | Result includes a descriptive reference string and threshold echoes, without a `Citation` sidecar. [src/digitalmodel/weather_window/workflow.py:L83-L99] |
| Summary JSON / returned configuration | UTF-8 JSON / dictionary | Payload is written before `summary_json` is inserted; JSON contains settings, result and status, while returned settings also contain the path. [src/digitalmodel/weather_window/workflow.py:L119-L141] |

## 3. Method and governing standards

`window_lengths_hours` counts consecutive samples satisfying `Hs <= hs_limit`, multiplies each count by the fixed interval, flushes a final run, and returns an empty list when no sample qualifies. [src/digitalmodel/weather_window/workflow.py:L30-L45]

`assess_weather_window` delegates statistics to `analyse_persistence`, separately extracts durations for viable-window counting, computes workability as `100 * total_hours_below / record_hours`, and uses a logical conjunction for the verdict. [src/digitalmodel/weather_window/workflow.py:L69-L81]

The dependency also uses inclusive comparison, returns zero statistics when there are no windows, and rounds nonempty statistics including maximum and total below-limit hours before returning them. [src/digitalmodel/orcaflex/weather_window.py:L177-L234]

| Reference as named in the source | Edition / clause evidence | Implementation relationship |
| --- | --- | --- |
| DNV-OS-H101; Marine Operations, General | No edition; dependency names Section 3 | Persistence reference, not a checked standard-derived criterion. [src/digitalmodel/weather_window/workflow.py:L19-L20] [src/digitalmodel/orcaflex/weather_window.py:L9-L13] [src/digitalmodel/orcaflex/weather_window.py:L182-L195] |
| DNV-ST-N001; Marine Operations, General | No edition or clause | Docstring reference only in this package. [src/digitalmodel/weather_window/workflow.py:L19-L20] [src/digitalmodel/orcaflex/weather_window.py:L9-L13] |
| DNV-RP-H103; Modelling and Analysis of Marine Operations | No edition or clause | Dependency describes weather criteria; no clause attached to the wrapper calculation. [src/digitalmodel/weather_window/workflow.py:L19-L20] [src/digitalmodel/orcaflex/weather_window.py:L9-L13] |
| API RP 2MET; Metocean Design and Operating Conditions | No edition or clause in this package; dependency's seasonal class names Section 5 | Seasonal class is outside this package's computation path. [src/digitalmodel/weather_window/workflow.py:L19-L20] [src/digitalmodel/orcaflex/weather_window.py:L9-L13] [src/digitalmodel/orcaflex/weather_window.py:L241-L245] [src/digitalmodel/weather_window/workflow.py:L58-L70] |
| Noble Denton 0027/ND; General Guidelines for Marine Operations | No edition; persistence dependency names no numbered clause | Appears in docstring and result label. [src/digitalmodel/weather_window/workflow.py:L19-L20] [src/digitalmodel/weather_window/workflow.py:L83-L84] [src/digitalmodel/orcaflex/weather_window.py:L9-L13] [src/digitalmodel/orcaflex/weather_window.py:L194-L195] |

UNVERIFIED: editions, clause titles and compliance with the named standards; clause text not checked against the licensed copy; issue pending (3).

The executable reference label names only DNV-OS-H101 and Noble Denton, while the docstring names additional references; neither supplies an edition, so no edition mismatch can be established from this package. [src/digitalmodel/weather_window/workflow.py:L19-L20] [src/digitalmodel/weather_window/workflow.py:L83-L99]

The result construction emits no `Citation` object or citation sidecar; provenance qualification remains issue pending (3). [src/digitalmodel/weather_window/workflow.py:L83-L99] [src/digitalmodel/weather_window/workflow.py:L119-L141]

## 4. Validated worked examples and regression examples

Case status is an offline test result at the code pin; the validated case's expectation is direct counting independent of the persistence implementation, while comparison to that helper is regression evidence only. [tests/workflows/test_durable_workflows.py:L2310-L2349]

| Case / classification | Inputs and independent expectation | Tolerance / test node / result |
| --- | --- | --- |
| V1: validated, closed-form run counting | Series `[1, 1, 1, 3, 1.5, 1.5, 3]` m, limit 2.0 m, interval 3.0 h: runs 9.0 and 6.0 h; workability `100 * 5 / 7` % (71.4 % displayed); one viable window for 9.0 h, none for 12.0 h | Run list and counts exact; other numbers use the existing `pytest.approx` assertion. `tests/workflows/test_durable_workflows.py::test_weather_window_persistence_and_fit_closed_form` — PASS before/after. [tests/workflows/test_durable_workflows.py:L2322-L2349] |
| R1: regression, durable workflow fixture | `examples/workflows/weather-window/input.yml`: illustrative 168.0 h record, 41 of 56 qualifying samples, longest 36.0 h, four windows; 48.0 h operation fails despite sufficient workability | Counts/status exact; numeric values use existing `pytest.approx`. `tests/workflows/test_durable_workflows.py::test_workflow_registry[weather-window]` — PASS before/after in a temporary copy because the test writes results. [examples/workflows/weather-window/input.yml:L10-L32] [tests/workflows/test_durable_workflows.py:L57-L92] [tests/workflows/test_durable_workflows.py:L1554-L1568] |

The upstream regression file defines 17 test methods covering scatter, operability, persistence, seasonal statistics and waiting time; these are dependency coverage rather than additional validated examples of this package. [tests/orcaflex/test_weather_window.py:L17-L147]

No independent published hindcast benchmark or standards-annex expected value is attached to either selected test; benchmark qualification remains issue pending (3). [tests/workflows/test_durable_workflows.py:L1554-L1568] [tests/workflows/test_durable_workflows.py:L2310-L2349]

## 5. QA gates and executable checks

Gate assignment: AQG r0 section 10, Python calculations, and section 9 for G5; the following criteria are checks to apply, with current limitations recorded separately.

| Gate | Applicability / check IDs | Numeric criterion and unit | Evidence / current result |
| --- | --- | --- | --- |
| G0 | Applicable: WW0 positive guards; WW1 finite values, sampling and units | WW0: 4 of 4 invalid calls rejected (case count). WW1: 0 invalid samples/scalars accepted (case count); Hs nonnegative m, limits/duration/interval positive m or h, optional workability in 0 to 100 % | WW0 passes; WW1 detects missing guards, issue pending (1); conversion and fixed-interval assumptions are in section 6. [src/digitalmodel/weather_window/workflow.py:L48-L80] [src/digitalmodel/weather_window/workflow.py:L102-L116] |
| G1 | Applicable: WW2 Python imports and existing tests | 19 of 19 calculation/dependency/workflow tests pass; 0 failures, skips or NOT_RUN cases (test counts) | V1, R1 and all 17 dependency tests; load route is Python import plus offline engine dispatch, with 0 solver launches. [tests/workflows/test_durable_workflows.py:L57-L92] [tests/workflows/test_durable_workflows.py:L2310-L2349] [tests/orcaflex/test_weather_window.py:L1-L147] [src/digitalmodel/engine.py:L381-L384] |
| G2 | Applicable: WW3 closed-form comparison; WW4 rounding boundary consistency | WW3: durations differ by 0.0 h and workability by at most 0.1 percentage point from direct counting, counts exact. WW4: 0 inconsistent fit/count verdicts; workability error at most 0.1 percentage point | WW3 passes; WW4 exposes a rounded-duration and rounded-total defect, issue pending (2); the helper is not an independent hand-check reference. [tests/workflows/test_durable_workflows.py:L2322-L2349] [src/digitalmodel/weather_window/workflow.py:L69-L81] [src/digitalmodel/orcaflex/weather_window.py:L224-L234] |
| G3 | `NOT_APPLICABLE` | No dynamics integration or simulation-time output in this screening function | Scalar persistence calculation only. [src/digitalmodel/weather_window/workflow.py:L48-L99] |
| G4 | `NOT_APPLICABLE` | No mesh, diffraction body, buoyancy or RAO output in this screening function | Scalar persistence calculation only. [src/digitalmodel/weather_window/workflow.py:L48-L99] |
| G5 | Applicable: WW5 summary trace; WW6 evidence completeness | WW5: 0 mismatches in result/status between returned config and JSON (field count). WW6: 1 reproducible input fingerprint, 0 unresolved reference items and 0 unmatched example results (counts) | WW5 passes for result/status; WW6 finds missing fingerprint and reference evidence, issue pending (3); this pack does not clear production QA. [src/digitalmodel/weather_window/workflow.py:L119-L141] [src/digitalmodel/weather_window/workflow.py:L83-L99] |

Executable WW0, WW1, WW3 and WW4 probes (run with the project Python environment; assertions on defect reproductions intentionally describe the pinned behaviour): [src/digitalmodel/weather_window/workflow.py:L30-L99]

```python
from math import nan, inf
from digitalmodel.weather_window.workflow import assess_weather_window, window_lengths_hours

# WW0: four implemented guards, case count = 4.
for args in [([], 2, 1, 1), ([1], 0, 1, 1), ([1], 2, 0, 1), ([1], 2, 1, 0)]:
    try:
        assess_weather_window(*args)
    except ValueError:
        pass
    else:
        raise AssertionError("missing positive/empty guard")

# WW1: each accepted call is a detected validation gap, not a QA pass.
for args in [([-1], 2, 1, 1), ([nan, 1], 2, 1, 1),
             ([1, inf], 2, 1, 1), ([1], nan, 1, 1),
             ([1], inf, 1, 1), ([1], 2, nan, 1), ([1], 2, 1, nan)]:
    assess_weather_window(*args)
for threshold in [-1, 101, nan]:
    assess_weather_window([1], 2, 1, 1, threshold)

# WW3: independent run-count arithmetic.
series = [1, 1, 1, 3, 1.5, 1.5, 3]
assert window_lengths_hours(series, 2, 3) == [9, 6]
assert window_lengths_hours([3], 2, 3) == []
assert window_lengths_hours([1], 2, 3) == [3]
assert window_lengths_hours([2], 2, 3) == [3]
r = assess_weather_window(series, 2, 9, 3)
assert abs(r["workability_pct"] - 100 * 5 / 7) <= 0.1
assert r["longest_window_hours"] == 9 and r["num_viable_windows"] == 1

# WW4: contradictory verdict and lost sub-hour exposure at the pin.
r = assess_weather_window([1], 2, 1, 0.96)
assert r["operation_fits_window"] and r["num_viable_windows"] == 0
assert assess_weather_window([1], 2, 0.04, 0.04)["workability_pct"] == 0
```

WW2 reproducer: `python -m pytest tests/orcaflex/test_weather_window.py tests/workflows/test_durable_workflows.py::test_weather_window_persistence_and_fit_closed_form -q -rA -p no:cacheprovider`, then the R1 node in a temporary repository copy with its unchanged test, configuration, registry and fixture; use `uv run --no-sources`, `PYTHONPATH=src`, a temporary basetemp and the same addopts override for every run. [tests/orcaflex/test_weather_window.py:L1-L147] [tests/workflows/test_durable_workflows.py:L57-L92] [tests/workflows/test_durable_workflows.py:L2310-L2349]

WW5 reproducer: call `router` with a scratch output directory, load its `summary_json` with `json.loads`, and require equality of `result` and `screening_status`; the saved payload omits the later-added path, so compare those fields explicitly. [src/digitalmodel/weather_window/workflow.py:L119-L141]

WW6 reproducer: inspect that same JSON for input fingerprints and citation evidence and compare every section-4 test result with its offline record; the current writer emits no such fingerprint or citation fields, so completeness remains open. [src/digitalmodel/weather_window/workflow.py:L83-L99] [src/digitalmodel/weather_window/workflow.py:L119-L141]

## 6. Known failure modes and pitfalls

sweep: 20 found

The sweep classifies 20 sites below, including intentional initializers, filters and defaults; no `except`, `continue`, `return None`, literal empty-list/dict error return or cache write occurs in the two Python files. [src/digitalmodel/weather_window/__init__.py:L1-L6] [src/digitalmodel/weather_window/workflow.py:L1-L141]

Exact searches, followed by line-by-line review of both complete Python files:

```bash
rg -n --glob '*.py' 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|5 ?%' src/digitalmodel/weather_window
rg -n --glob '*.py' 'float\(|int\(|dtype=float|or \{\}|get\(|None|hours|_m|write|cache|\[.*for .*if' src/digitalmodel/weather_window
```

| Site / pinned location | Symptom / classification | Detection / follow-up |
| --- | --- | --- |
| S01: empty accumulator, possible empty return | No qualifying sample yields `[]`; this is a valid zero-window result rather than an error fallback. [src/digitalmodel/weather_window/workflow.py:L34-L45] | WW3 plus `tests/orcaflex/test_weather_window.py::TestPersistence::test_all_above_threshold`; intentional. [tests/orcaflex/test_weather_window.py:L82-L87] |
| S02: initial zero run counter | Counter starts at zero; no inferred qualifying samples. [src/digitalmodel/weather_window/workflow.py:L35-L38] | WW3 exact 9.0/6.0 h; intentional. [tests/workflows/test_durable_workflows.py:L2322-L2327] |
| S03: reset counter to zero | Above-limit sample terminates a run. [src/digitalmodel/weather_window/workflow.py:L39-L44] | WW3 exact separation; intentional. [tests/workflows/test_durable_workflows.py:L2324-L2327] |
| S04: direct interval default | Omission silently assumes 3.0 h/sample. [src/digitalmodel/weather_window/workflow.py:L52-L52] | No detection check exists for an unintended sampling interval; issue pending (1). |
| S05: optional workability default | `None` disables the percentage requirement. [src/digitalmodel/weather_window/workflow.py:L53-L54] | WW3 uses omitted threshold; intentional. [tests/workflows/test_durable_workflows.py:L2329-L2337] |
| S06: array conversion | `dtype=float` does not reject negative or non-finite wave heights. [src/digitalmodel/weather_window/workflow.py:L60-L70] | WW1; issue pending (1). |
| S07: filtered viable windows | Durations shorter than the requirement are excluded, using unrounded values. [src/digitalmodel/weather_window/workflow.py:L72-L73] | WW3; WW4 detects disagreement with rounded fit; issue pending (2). |
| S08: optional threshold bypass | Missing threshold is intentional; supplied non-finite/out-of-range thresholds are unchecked. [src/digitalmodel/weather_window/workflow.py:L78-L80] | WW1; issue pending (1). |
| S09: settings fallback | Missing/falsy `weather_window` becomes `{}`; required `operation` and `metocean` then raise `KeyError`, rather than return a silent pass. [src/digitalmodel/weather_window/workflow.py:L103-L105] | No detection check exists for a structured schema diagnostic; issue pending (1). |
| S10: external Hs text conversion | `float(v)` uses the Python numeric convention; separators are not normalized, and non-finite text is accepted. [src/digitalmodel/weather_window/workflow.py:L108-L108] | WW1 covers numeric consequences; no detection check exists for locale-formatted input; issue pending (1). |
| S11: external limit conversion | Same conversion convention; NaN passes the positive-only guard. [src/digitalmodel/weather_window/workflow.py:L62-L63] [src/digitalmodel/weather_window/workflow.py:L109-L109] | WW1; issue pending (1). |
| S12: external duration conversion | Same conversion convention; NaN passes the positive-only guard. [src/digitalmodel/weather_window/workflow.py:L64-L65] [src/digitalmodel/weather_window/workflow.py:L110-L110] | WW1; issue pending (1). |
| S13: external interval conversion/default | `float` conversion plus a second 3.0 h/sample default; no timestamp or gap test. [src/digitalmodel/weather_window/workflow.py:L66-L70] [src/digitalmodel/weather_window/workflow.py:L111-L111] | WW1 detects non-finite acceptance; no detection check exists for gaps; issue pending (1). |
| S14: external optional percentage conversion | Missing/null disables it, supplied text goes through `float` with no finite/range check. [src/digitalmodel/weather_window/workflow.py:L112-L116] | WW1; issue pending (1). |
| S15: empty output settings fallback | Missing outputs uses `{}` and downstream defaults rather than suppressing output. [src/digitalmodel/weather_window/workflow.py:L124-L126] [src/digitalmodel/weather_window/workflow.py:L132-L141] | WW5 exercises default writer settings; intentional. |
| S16: default result directory | Missing directory uses `results/weather_window`. [src/digitalmodel/weather_window/workflow.py:L133-L133] | WW5; intentional. |
| S17: directory-origin fallback | Missing `_config_dir_path` resolves against the current directory, so location depends on invocation. [src/digitalmodel/weather_window/workflow.py:L134-L135] | No detection check exists for output-location ambiguity; issue pending (3). |
| S18: default summary filename | Missing summary name uses `weather_window_summary.json`. [src/digitalmodel/weather_window/workflow.py:L137-L139] | WW5; intentional. |
| S19: unversioned JSON write | Writes directly to a fixed summary path without a fingerprint; a later run replaces its contents, and accepted non-finite input is serialized as non-finite JSON constants. [src/digitalmodel/weather_window/workflow.py:L137-L140] | WW6 detects provenance gap; WW5 with a non-finite sample and strict JSON decoding detects serialization gap; issue pending (3), issue pending (1). |
| S20: scalar unit/fixed-step assumption | Samples are treated as m and each qualifying sample as a full interval, including the final run; no timestamp or unit conversion is provided. [src/digitalmodel/weather_window/workflow.py:L30-L45] [src/digitalmodel/weather_window/workflow.py:L108-L111] | WW3 checks fixed-step arithmetic; no detection check exists for mismatched source units; issue pending (1). |

| Additional pitfall / incident | Cause and detection | Follow-up |
| --- | --- | --- |
| Rounded fit and exposure | Rounded helper maximum can indicate fit with zero viable windows; rounded total below-limit hours can erase a short record's workability. WW4 detects both; do not use the helper as an independent decision-boundary reference. [src/digitalmodel/weather_window/workflow.py:L72-L81] [src/digitalmodel/orcaflex/weather_window.py:L224-L234] | Issue pending (2). |
| Standards/provenance are descriptive | WW6 detects absent editions, clause evidence and citation sidecar in results. [src/digitalmodel/weather_window/workflow.py:L19-L20] [src/digitalmodel/weather_window/workflow.py:L83-L99] | Issue pending (3). |
| #2614: upstream wind/current fields ignored | Confirmed in `OperabilityInput`: fields are declared, but calculation uses only Hs/Tp; this wrapper does not call that class. No detection check exists for wind/current rejection in this package. [src/digitalmodel/orcaflex/weather_window.py:L108-L132] [src/digitalmodel/weather_window/workflow.py:L58-L70] | #2614; use this wrapper only for its explicit Hs screening contract. |
| #924: feature lead, not a defect incident | Closed-form and workflow assertions cover the feature's run counting and fit; WW2/WW3 are detection checks for regression. [tests/workflows/test_durable_workflows.py:L1554-L1568] [tests/workflows/test_durable_workflows.py:L2310-L2349] | No incident attributed to this issue. |
| #1661: integration lead, not an established incident | Current package accepts caller-provided Hs values and contains no network fetch. [src/digitalmodel/weather_window/workflow.py:L23-L29] [src/digitalmodel/weather_window/workflow.py:L102-L141] | #1661; no detection check exists for live-data integration here. |

## 7. Lessons log

Append-only review record; no clearance or shared-log receipt is asserted by this initial draft.

| Date | Record | Lesson to retain | Evidence | Pack change / shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-18 | Initial pack | Check window-fit and viable-count agreement using unrounded durations before accepting rounded persistence summaries. | [src/digitalmodel/weather_window/workflow.py:L72-L81] [src/digitalmodel/orcaflex/weather_window.py:L224-L234] | Sections 5/6 define WW4; shared-log entry unassigned, issue pending (2). |

## 8. Ownership

Review assignments for this pack: engineering calculations quality manager; deputy engineering calculations quality manager; producing engineering calculations maintainers; escalation to the family quality manager for ambiguous criteria and to the maintainers for cross-family scope (AQG r0 section 11, independent sign-off).

Clearance requires a provider independent of the producer; this document's status remains `DRAFT` pending that assessment (AQG r0 section 11).

## 9. Dependencies and alternative implementations

| Dependency / relationship | Scope, version evidence and inventory |
| --- | --- |
| Engine → router | Upstream `engine` dispatches `weather_window` to the package; the registry inventory row marks the package `development`. [src/digitalmodel/engine.py:L381-L384] [docs/registry/module-routing.yaml:L692-L697] |
| `digitalmodel.orcaflex.weather_window.analyse_persistence` | Upstream pure Python statistic provider; this package imports it locally; its module says no OrcFxAPI is required. No solver version is selected. [src/digitalmodel/weather_window/workflow.py:L58-L70] [src/digitalmodel/orcaflex/weather_window.py:L1-L21] |
| NumPy | Local import converts the Hs series before persistence analysis; no version is constrained by this function. [src/digitalmodel/weather_window/workflow.py:L55-L70] |
| JSON / pathlib | Standard-library output construction; no external data download or solver launch appears in the writer. [src/digitalmodel/weather_window/workflow.py:L25-L27] [src/digitalmodel/weather_window/workflow.py:L132-L141] |
| Offline durable workflow | Inventory `weather-window` declares its input, outputs, test node and offline runtime. [docs/registry/workflows.yaml:L1207-L1215] |
| Parametric atlas | Separate inventory extension and response; it estimates a planned-operability percentage using fixed synthetic Hs data and exponential window persistence, rather than calling this package's router or fit function. [docs/registry/workflows.yaml:L1216-L1225] [src/digitalmodel/parametric/generate.py:L435-L475] |

OrcaFlex occurrence audit: case-sensitive `rg -n 'OrcaFlex' src/digitalmodel/weather_window` has 0 hits at the pin; the two lowercase `orcaflex` occurrences are the docstring and import of the pure Python helper, both listed above, and neither is a licensed interface invocation. [src/digitalmodel/weather_window/__init__.py:L1-L6] [src/digitalmodel/weather_window/workflow.py:L1-L141]

Implementation-map inventory: map ref `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461`, `docs/registry/implementation-map.yaml` on `codex/implmap-work`; 0 families have an implementation path inside `src/digitalmodel/weather_window/`, so the required inventory is empty, counted families = 0, and the counted-family search is `NOT_APPLICABLE` for that reason. The map pin is separate from this pack's code pin.

Map follow-up: register continuous below-limit run extraction, persistence statistics, workability fraction and continuous-duration fit from section 3; issue pending (4). [src/digitalmodel/weather_window/workflow.py:L30-L99] [src/digitalmodel/orcaflex/weather_window.py:L177-L234]

Repository-wide discovery (including `scripts/`) used `rg -n --glob '*.py' 'analyse_persistence|window_lengths_hours|assess_weather_window|weather_window_operability|consecutive.*window|persistence' src scripts tests`; the algorithm-bearing alternatives and helpers found are classified below, independently of the empty counted-family inventory. [src/digitalmodel/weather_window/workflow.py:L30-L99] [src/digitalmodel/orcaflex/weather_window.py:L177-L234] [src/digitalmodel/parametric/generate.py:L451-L475] [src/digitalmodel/marine_ops/installation/operability.py:L190-L235]

| Definition | Classification / distinction |
| --- | --- |
| `window_lengths_hours` and `assess_weather_window` | In-package direct run extraction and workability/fit calculation, both already in section 3. [src/digitalmodel/weather_window/workflow.py:L30-L99] |
| `analyse_persistence` | External direct implementation of the same run counting plus aggregate statistics; the wrapper delegates statistics but repeats extraction for viable counts. [src/digitalmodel/orcaflex/weather_window.py:L177-L234] [src/digitalmodel/weather_window/workflow.py:L69-L77] |
| `_weather_window_operability_pct` | External direct planned-operability formula over helper statistics, with a fixed synthetic record and exponential persistence approximation; map follow-up, not a counted-family miss. [src/digitalmodel/parametric/generate.py:L435-L475] |
| `weather_window_operability` | External scatter-probability integration against interpolated Hs limits by period, distinct from time-series persistence; map follow-up. [src/digitalmodel/marine_ops/installation/operability.py:L190-L235] |
| `router` / `_write_summary` | Delegate, assemble and serialize; no further persistence algorithm. [src/digitalmodel/weather_window/workflow.py:L102-L141] |
| Selected workflow tests | Assertions and helper comparisons, rather than an independent production implementation. [tests/workflows/test_durable_workflows.py:L1554-L1568] [tests/workflows/test_durable_workflows.py:L2310-L2349] |

No repository-wide uniqueness claim follows from this search; future map rows and other discovered method families belong in the map follow-up, issue pending (4).

## 10. Coverage and open items

Code coverage read set: both package files, the complete persistence dependency, the engine dispatch, existing routing/workflow inventories, both selected test files and the parametric/installation alternatives. [src/digitalmodel/weather_window/__init__.py:L1-L6] [src/digitalmodel/weather_window/workflow.py:L1-L141] [src/digitalmodel/orcaflex/weather_window.py:L1-L311] [src/digitalmodel/engine.py:L381-L384] [docs/registry/module-routing.yaml:L692-L697] [docs/registry/workflows.yaml:L1207-L1225] [tests/orcaflex/test_weather_window.py:L1-L147] [tests/workflows/test_durable_workflows.py:L2310-L2349] [src/digitalmodel/parametric/generate.py:L435-L475] [src/digitalmodel/marine_ops/installation/operability.py:L190-L235]

Documentation discovery/read set (no README exists inside the package at the pin; its only files are the two Python sources): [src/digitalmodel/weather_window/__init__.py:L1-L6] [src/digitalmodel/weather_window/workflow.py:L1-L141]

| Document | Relevance / boundary |
| --- | --- |
| `docs/maps/digitalmodel-operator-map.md` | Module routing row. [docs/maps/digitalmodel-operator-map.md:L108-L108] |
| `docs/riser_database.md` | Reuses the dependency's scatter interface, rather than this package's workflow. [docs/riser_database.md:L239-L243] |
| `docs/plans/parametric-refresh/adding-an-atlas.md` | Separate atlas conventions; not independent validation of this wrapper. [docs/plans/parametric-refresh/adding-an-atlas.md:L9-L26] |
| `docs/plans/2026-05-05-issue-491-rov-intervention-modeling.md` | Separate task-sequencing weather-window proposal. [docs/plans/2026-05-05-issue-491-rov-intervention-modeling.md:L20-L32] |
| `docs/plans/2026-05-05-issue-578-w2w-gangway-operability.md` | Related operability layer using the upstream helper; no edition supplied for this package's references. [docs/plans/2026-05-05-issue-578-w2w-gangway-operability.md:L8-L16] |
| `docs/plans/2026-05-05-issue-467-subsea-installation-vessel-feedback.md` | Related installation scatter operability. [docs/plans/2026-05-05-issue-467-subsea-installation-vessel-feedback.md:L9-L14] |
| `docs/capability-map/capabilities-ia-spec-1444.md` | Broad installation capability wording, not calculation qualification. [docs/capability-map/capabilities-ia-spec-1444.md:L53-L57] |
| `docs/domains/metocean/wait_on_weather.md` | Monte Carlo proposal, not this module's persistence method. [docs/domains/metocean/wait_on_weather.md:L1-L11] |

Offline evidence at the code pin, baseline and after this documentation change: 18/18 selected calculation/dependency tests; 1/1 workflow fixture in a temporary copy; 497/497 contract/routing tests; 0 failures, skipped tests or NOT_RUN cases in each set. The generic test form was `PYTHONPATH=src uv run --no-sources python -m pytest <paths> -q -rA -p no:cacheprovider --override-ini addopts= --basetemp=<scratch>`; the same override was used for every run. These are measured execution records, not a claim that every physical or provenance requirement passes.

| Open item | Disposition / responsibility / tracking |
| --- | --- |
| C1: finite values, ranges, fixed interval, units and locale conventions; missing negative-input tests | Maintainers to define the input contract and add tests; WW1 detects the numeric gap. Issue pending (1). [src/digitalmodel/weather_window/workflow.py:L48-L80] [src/digitalmodel/weather_window/workflow.py:L102-L116] |
| C2: fit/count disagreement and rounded exposure | Maintainers to use unrounded decision quantities and add WW4 boundary tests. Issue pending (2). [src/digitalmodel/weather_window/workflow.py:L72-L81] [src/digitalmodel/orcaflex/weather_window.py:L224-L234] |
| C3: standards qualification from section 3, independent published benchmark, Citation sidecar, input fingerprint and output provenance | Family quality manager to confirm editions/clause titles; maintainers to add evidence and test coverage. Issue pending (3). [src/digitalmodel/weather_window/workflow.py:L19-L20] [src/digitalmodel/weather_window/workflow.py:L83-L99] [src/digitalmodel/weather_window/workflow.py:L132-L141] |
| C4: section-3 method families absent from map inventory | Maintainers to register families and re-run inventory/search at the first map revision with in-package rows. Issue pending (4). [src/digitalmodel/weather_window/workflow.py:L30-L99] |
| C5: upstream wind/current limits are ignored | Existing defect #2614; separate from this Hs-only wrapper, and no detection check exists here. [src/digitalmodel/orcaflex/weather_window.py:L108-L132] [src/digitalmodel/weather_window/workflow.py:L48-L54] |
| C6: live metocean integration | Existing integration lead #1661; current router expects provided series, and no live-data detection check exists here. [src/digitalmodel/weather_window/workflow.py:L102-L117] |

Existing issue #924 is a feature reference; the confirmed upstream incident is #2614, while new input, rounding, provenance and map gaps are grouped into four issue drafts rather than altering calculation code. [src/digitalmodel/weather_window/workflow.py:L48-L141] [src/digitalmodel/orcaflex/weather_window.py:L108-L132] [src/digitalmodel/orcaflex/weather_window.py:L224-L234]
