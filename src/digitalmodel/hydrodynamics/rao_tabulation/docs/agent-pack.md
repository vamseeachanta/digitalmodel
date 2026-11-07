# RAO tabulation knowledge pack

| Header | Value |
| --- | --- |
| Module | hydrodynamics/rao_tabulation |
| Repository | digitalmodel |
| Package | src/digitalmodel/hydrodynamics/rao_tabulation |
| Pack revision | 0 |
| Code revision | `b47e42fbe7208c69b28dd436aa91fb129bc385f7` |
| Analysis family | diffraction (DF) |
| Quality role | diffraction quality manager |
| Status | DRAFT |

Code citations resolve at the code revision above; section 9 explicitly identifies the separate implementation-map revision used for its inventory.

## 1. Purpose and scope

`router` reads an in-memory RAO grid, interpolates six degrees of freedom, writes a CSV and replaces the input settings with a summary including the peak queried heave amplitude; it does not search for the continuous response maximum. [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L25-L46]

`annotate_rows` and the separate CSV command annotate native samples with a dataset-specific frequency policy while preserving original values and earlier reliability findings; the tabulation router does not call this screen. [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L53-L105] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L25-L46]

The routing registry labels the hydrodynamics domain `production`; this is a domain designation rather than independent physical validation of this package. [docs/registry/module-routing.yaml:L181-L187]

The workflow calls a shared numerical interpolator and writes tabular data; neither path creates a solver deck, mesh or numerical diffraction solution. [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L203-L219] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L275-L281] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L138-L146]

## 2. Inputs and outputs

| Input | Unit | Accepted form / range | Default / source | Evidence |
| --- | --- | --- | --- | --- |
| Grid periods or angular frequencies | s or rad/s | Exactly one non-empty finite positive vector; aliases supported | Required grid in `rao_database`, `database`, or settings | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L69-L94] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L284-L298] |
| Grid headings | degree | Non-empty finite unique vector, sorted with the data | Required; four aliases | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L49-L62] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L97-L103] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L284-L298] |
| Amplitudes and phases | Amplitude normalization undeclared; phase degree | Finite arrays shaped frequency count × heading count × 6 | Required; `phases` aliases `phases_deg` | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L106-L124] |
| Queries | s, rad/s, degree | Non-empty list; period/frequency positive; both must agree at relative tolerance `1e-9`; heading finite and within grid bounds | `query_points` or `queries` | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L127-L191] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L222-L234] |
| Method | dimensionless selector | `linear` or `nearest` | `linear`; whitespace stripped, lowercased | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L194-L200] |
| Result location | filesystem path | Output directory resolved against config directory; existing CSV replaced | `results`; config file stem, else basename, else `rao_tabulation` | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L253-L281] |
| Native screening rows | Frequency rad/s; other columns opaque | Positive finite frequencies; bool rejected; names containing hz/period rejected case-insensitively | `frequency_rad_s` column | [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L35-L43] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L53-L80] |
| Screening policy | rad/s and citation text | Positive finite lower bound and non-empty basis | No bound; absent policy yields `NOT_EVALUATED` | [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L22-L32] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L80-L103] |
| Screening CSV | UTF-8 text | Unique non-empty headers and complete non-empty rows; output must not exist | Explicit input and output paths | [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L108-L146] |

| Output | Unit / structure | Evidence |
| --- | --- | --- |
| Tabulation CSV | `period_s`, `heading_deg`, surge/sway/heave/roll/pitch/yaw amplitude, then six `_phase_deg` columns | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L20-L22] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L237-L250] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L275-L281] |
| Summary dictionary | Query count, DOF names, CSV path, method and maximum queried heave with period and heading | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L34-L46] |
| Annotated rows / new CSV | Original fields plus reliability, retained bases, current screening status/basis and minimum frequency in rad/s | [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L88-L105] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L138-L146] |

Period conversion uses `omega_rad_s = 2*pi/period_s`; input axes and corresponding amplitude/phase slices are sorted together. [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L49-L66] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L76-L94]

UNVERIFIED: amplitude normalization, rotational amplitude units, coordinate frame, origin, heading sign and phase reference are not established by the untagged array contract; engineering use requires an explicit source-data convention record (section 10, issue pending (1)). [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L106-L124] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L237-L250]

The screening tool performs no Hz conversion and does not infer units from custom column names beyond rejecting hz/period; callers establish angular-frequency units upstream. [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L68-L73]

## 3. Method and governing standards

| Method | Implementation and applicability | Evidence |
| --- | --- | --- |
| Input normalization | Choose grid aliases, convert numeric vectors, require finite arrays and unique sorted axes | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L69-L124] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L284-L305] |
| Query conversion and consistency | Convert period/frequency reciprocally using `2*pi`; compare dual inputs with relative tolerance `1e-9`; finite query-frequency validation has a gap listed in section 6 | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L143-L175] |
| Interpolation | `_interpolated_rows` delegates to `CoefficientsInterpolator.interpolate_all_dofs`; shared `interpolate_rao_2d` applies SciPy `RegularGridInterpolator` separately to amplitudes and scalar phases | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L203-L219] [src/digitalmodel/hydrodynamics/interpolator.py:L134-L163] [src/digitalmodel/hydrodynamics/interpolator.py:L185-L207] |
| Boundaries and output | Reject out-of-grid queries; reject non-finite interpolated outputs; serialize one result row per query | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L222-L250] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L275-L281] |
| Native frequency screen | Strictly below supplied bound: `low_freq_artifact`; equality or above: `screening_range`; absent policy: `NOT_EVALUATED`; no universal threshold | [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L59-L87] |
| Label/provenance retention | Append distinct semicolon-delimited findings; in-range screening adds no independent reliability qualification | [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L46-L50] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L88-L103] |

The implemented interpolation and screening paths name no governing licensed standard or standards-derived coefficient; the screening documentation instead cites #1548 as the dataset-specific policy source, not evidence that other datasets share its bound. [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L76-L94] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L203-L219] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L59-L69] [docs/domains/hydrodynamics/rao-frequency-screening.md:L25-L36]

No calculation-citation sidecar is emitted by these paths; physical response validation and a justified source-data policy remain separate evidence requirements. [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L34-L46] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L97-L105] [docs/domains/hydrodynamics/rao-frequency-screening.md:L89-L101]

## 4. Worked examples and offline results

Validated physical examples: 0; the following 3 examples use synthetic fixtures and fixed assertions and are classified conservatively as regression examples rather than independent hydrodynamic benchmarks. [tests/hydrodynamics/test_rao_tabulation_workflow.py:L29-L63] [tests/hydrodynamics/rao_tabulation/test_reliability.py:L24-L49] [examples/workflows/rao-tabulation/input.yml:L3-L35]

| Case | Source / input | Expected value and tolerance basis | Existing test node ID | BASE / after | Evidence |
| --- | --- | --- | --- | --- | --- |
| R1: six-DOF interpolation | Synthetic `_base_cfg` grid | 2 rows and 14 columns; first heave `0.755` in input amplitude units, peak `1.0` at 16 s / 0 degree; test uses `pytest.approx` defaults, no physical tolerance established | `tests/hydrodynamics/test_rao_tabulation_workflow.py::test_rao_tabulation_workflow_writes_interpolated_6dof_table` | PASS / PASS | [tests/hydrodynamics/test_rao_tabulation_workflow.py:L29-L91] |
| R2: durable tabulation | `examples/workflows/rao-tabulation/input.yml` | 3 rows; first heave `0.575`, peak `1.02` in input amplitude units at 18 s / 0 degree; `pytest.approx` defaults are regression tolerances | `tests/workflows/test_durable_workflows.py::test_workflow_registry[rao-tabulation]` | PASS / PASS | [tests/workflows/test_durable_workflows.py:L601-L620] |
| R3: boundary labels and preservation | Synthetic frequencies / headings | 9 rows retained; 3 below-bound flags, 6 in-range statuses; 0 changed original values (count); exact comparisons across 3 headings | `tests/hydrodynamics/rao_tabulation/test_reliability.py::test_boundary_and_all_headings_preserve_values` | PASS / PASS | [tests/hydrodynamics/rao_tabulation/test_reliability.py:L24-L49] |

Native-frequency screening is separate from the durable tabulation example; applying the screen only to an interpolated query would not propagate findings from all contributing native samples. [docs/domains/hydrodynamics/rao-frequency-screening.md:L89-L101]

Offline execution record at `b47e42fbe7208c69b28dd436aa91fb129bc385f7`: direct module tests plus routing contracts, 61 passed; related workflow/atlas/catalog tests, 10 passed; repository contract tests, 576 passed. Final results cover 647 required nodes at BASE and after the documentation/registry edits: 647 passed, 0 failed, 0 skipped, 0 NOT_RUN. The first archive-copy contract attempt had 585 passed and 1 environment failure (`git ls-files`: not a Git repository); the complete contract domain subsequently passed in the worktree. [tests/hydrodynamics/test_rao_tabulation_workflow.py:L72-L130] [tests/hydrodynamics/rao_tabulation/test_reliability.py:L24-L255] [tests/contracts/test_vba_credit.py:L98-L107]

The test form is `PYTHONPATH=src uv run --no-sources python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>` with repository addopts retained; the durable example runs in a BASE archive copy using `--project <worktree>` so its result files remain outside the working tree. [tests/workflows/test_durable_workflows.py:L57-L98]

## 5. QA gates

The gate allocation below applies AQG r0, sections 3, 10 and 13, to this package's CSV/dictionary outputs; source solver qualification remains a separate prerequisite, not established by interpolation or a frequency flag. [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L25-L46] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L59-L70]

| Gate | Applicability and program route | Check / evidence |
| --- | --- | --- |
| G0 inputs | Applicable: numerical grids, query units and native CSV structure | C0.1, C0.2, C0.3; offline assertions and CSV readback |
| G1 load | NOT_APPLICABLE: neither path writes OrcaWave data/mesh or AQWA decks; CSV parse is checked by C0.3. If qualifying upstream solver inputs, OrcaWave opens data/mesh and requires 0 validation errors (count); AQWA requires a short licensed run to hydrostatics, 0 load errors (count); neither upstream route ran here | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L275-L281] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L126-L146]; AQG r0 section 10, OrcaWave and AQWA rows |
| G2 statics | NOT_APPLICABLE: no equilibrium solver or statics model is produced | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L203-L219] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L80-L105]; AQG r0 section 10 |
| G3 dynamics | NOT_APPLICABLE: frequency-grid interpolation and annotation produce no time simulation | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L203-L219] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L80-L105]; AQG r0 section 10 |
| G4 diffraction | NOT_APPLICABLE to these tabular outputs: no mesh, hydrostatics or diffraction solve is generated; upstream physical validation remains open and must accompany source datasets | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L25-L46] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L7-L9] [docs/domains/hydrodynamics/rao-frequency-screening.md:L97-L101]; AQG r0 section 10 |
| G5 evidence appendix | Applicable: retain the input pin, test commands/results, case classifications and failure-mode register with any downstream report | C5.1; offline result ledger; this pack is not a solver QA certificate |

| Check | Executable procedure | Numeric pass criterion and unit | Evidence / result |
| --- | --- | --- | --- |
| C0.1 (G0) | Run `tests/hydrodynamics/test_rao_tabulation_workflow.py::test_rao_tabulation_workflow_fails_closed_on_malformed_inputs` for shape mismatch, empty queries, inconsistent period/frequency and out-of-grid frequency | 4 of 4 cases raise `ValueError`; 0 accepted malformed cases (count) | PASS at BASE and after; [tests/hydrodynamics/test_rao_tabulation_workflow.py:L94-L130] |
| C0.2 (G0) | Run invalid-frequency, invalid-cutoff, missing-basis, incompatible-column, malformed-CSV and paired-policy tests in `tests/hydrodynamics/rao_tabulation/test_reliability.py`; run `test_missing_policy_is_not_evaluated` and `test_cli_without_policy_exposes_missing_criterion` | 0 accepted enumerated invalid inputs (count); missing policy produces exactly 1 `NOT_EVALUATED` current status per row (count) | PASS at BASE and after; [tests/hydrodynamics/rao_tabulation/test_reliability.py:L52-L56] [tests/hydrodynamics/rao_tabulation/test_reliability.py:L94-L147] [tests/hydrodynamics/rao_tabulation/test_reliability.py:L194-L200] [tests/hydrodynamics/rao_tabulation/test_reliability.py:L219-L255] |
| C0.3 (G0) | Run R1–R3 and `tests/hydrodynamics/rao_tabulation/test_reliability.py::test_cli_retains_every_row_and_source_bytes`, `test_cli_refuses_existing_output`, and flag-retention tests in the same file | R1 2 rows / 14 columns, R2 3 rows, R3 9 rows; 0 modified source values or source bytes (count); 0 overwritten annotation files (count) | PASS at BASE and after; [tests/hydrodynamics/test_rao_tabulation_workflow.py:L72-L91] [tests/hydrodynamics/rao_tabulation/test_reliability.py:L24-L49] [tests/hydrodynamics/rao_tabulation/test_reliability.py:L59-L91] [tests/hydrodynamics/rao_tabulation/test_reliability.py:L150-L191] [tests/hydrodynamics/rao_tabulation/test_reliability.py:L203-L216] |
| C5.1 (G5) | Match the 3 section-4 nodes to the offline logs, verify the code pin and retain results in the evidence ledger before reuse in a report | 0 missing node results (count), 0 pin mismatches (count), 3 regression classifications (count) | PASS at BASE and after; case nodes and assertions: [tests/hydrodynamics/test_rao_tabulation_workflow.py:L72-L91] [tests/workflows/test_durable_workflows.py:L601-L620] [tests/hydrodynamics/rao_tabulation/test_reliability.py:L24-L49] |

These are 4 defined checks: G0 = 3, G5 = 1; no existing check asserts source-data frame/normalization, physical RAO validity, continuous peak capture or flag propagation through interpolation. [tests/hydrodynamics/test_rao_tabulation_workflow.py:L72-L130] [tests/hydrodynamics/rao_tabulation/test_reliability.py:L24-L255] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L34-L44] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L203-L219]

## 6. Known failure modes and pitfalls

Observation date: 2026-11-07.

sweep: 24 found

The count denotes the 24 reviewed sites or grouped conversion/default paths below, including guarded paths and intentional defaults; it is not a count of 24 confirmed defects. Source coverage is the entire package (3 Python files); no cache-write path or record-skipping `continue` occurs in those implementations. [src/digitalmodel/hydrodynamics/rao_tabulation/__init__.py:L1-L2] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L1-L313] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L1-L151]

Exact sweep commands (run before adding this Markdown file):

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|\b0(\.0)?\b|1970|0\.05|float\(|int\(|dtype=float|\.get\(' src/digitalmodel/hydrodynamics/rao_tabulation
rg -n 'except|continue|return|default|cache|unit|rad/s|deg|period|frequency' src/digitalmodel/hydrodynamics/rao_tabulation --glob '*.py'
```

| Site | Symptom / interpretation | Detection / tracking | Evidence at code pin |
| --- | --- | --- | --- |
| S01: empty settings fallback | Missing or false-valued settings become an empty mapping; required grid later rejects missing axes | C0.1; guarded path | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L25-L30] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L76-L84] |
| S02: name fallback | Missing source name becomes a generic label, which is not provenance | no detection check exists; issue pending (1) | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L63-L65] |
| S03: alias precedence | Present primary database/query key wins over aliases even if unusable; ambiguity is not reconciled | no detection check exists; issue pending (1) | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L69-L73] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L127-L130] |
| S04: grid units / conversion | Period aliases mean seconds; frequency aliases mean rad/s; positive tests and reciprocal conversion apply without external unit metadata | no detection check exists for a wrongly labelled numeric grid; issue pending (1) | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L76-L103] |
| S05: array coercion | External amplitudes/phases converted with `dtype=float`; comma-formatted text raises, finite signed values are accepted without normalization metadata | no detection check exists for normalization or locale semantics; issue pending (1) | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L106-L124] |
| S06: query period conversion | `float(period)` and positivity test do not explicitly reject NaN, infinity or booleans before reciprocal conversion | no detection check exists; issue pending (1) | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L154-L160] |
| S07: query frequency conversion | `float(frequency)` also lacks an explicit finite/bool check; NaN can evade bound comparisons; rejection depends on the downstream output guard | no detection check exists; issue pending (1) | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L161-L175] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L222-L241] |
| S08: query heading conversion | `float(heading)` rejects non-finite results but accepts booleans as numbers; no locale normalization | no detection check exists for boolean/locale inputs; issue pending (1) | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L178-L191] |
| S09: default interpolation | Missing method selects linear; invalid selectors raise rather than silently selecting another algorithm | no detection check exists for all selector branches; issue pending (1) | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L194-L200] |
| S10: summary casts | Peak key and summary fields are numeric casts of already checked result rows; peak is only over requested rows | C0.3 for sample summary; no detection check exists for missed continuous maxima; issue pending (2) | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L34-L44] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L237-L250] |
| S11: interpolator query casts | Internal query scalars are converted again to float arrays without another finite check | no detection check exists for non-finite query ingress; issue pending (1) | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L211-L217] |
| S12: result indexing and casts | Read the first interpolated frequency/heading and serialize 6 amplitudes / 6 phases; non-finite output raises | C0.3 for row/column counts; no detection check exists for every non-finite output branch; issue pending (1) | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L237-L250] |
| S13: output directory default | Missing output directory means `results`; tabulation opens the target with `w`, replacing earlier content | no detection check exists for tabulation replacement; issue pending (1) | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L253-L258] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L275-L281] |
| S14: path/stem fallbacks | Missing config metadata uses current directory and basename; output location can depend on invocation context | no detection check exists; issue pending (1) | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L261-L272] |
| S15: vector conversion | Shared `dtype=float` conversion accepts numeric text and bool values, rejects non-finite/empty/nonvector inputs; thousands/decimal separators are not normalized | no detection check exists for every coercion branch; issue pending (1) | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L284-L298] |
| S16: None sentinel | `_first_value` returns None for absent aliases; it is a missing-key sentinel, not a swallowed exception | C0.1 covers missing data indirectly; required-field raises | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L301-L305] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L81-L84] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L101-L102] |
| S17: path exception fallback | A relative-path conversion error returns an absolute path; local filesystem locations may escape in summary metadata | no detection check exists; issue pending (1) | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L308-L313] |
| S18: native frequency conversion | `float(value)` catches conversion errors and re-raises; boolean/non-finite/non-positive values reject, without separator normalization | C0.2; guarded conversion, locale coverage absent under issue pending (1) | [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L35-L43] |
| S19: label coercion | `str(previous or "")` coalesces false-valued labels and strips empty pieces; numeric labels can be lost or stringified | no detection check exists for malformed label types; issue pending (1) | [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L46-L50] |
| S20: empty iterable and missing policy | `annotate_rows([])` returns an empty list; API policy defaults to None and uses empty basis/bound fields; CSV command separately rejects empty input | C0.2 for missing policy and malformed CSV; no detection check exists for empty API input; issue pending (1) | [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L53-L58] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L74-L87] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L104-L105] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L135-L137] |
| S21: missing existing labels/bases | Missing reliability fields default to empty text; in-range screening leaves them empty rather than declaring physical validity | C0.3, retention and boundary tests | [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L88-L103] [tests/hydrodynamics/rao_tabulation/test_reliability.py:L24-L49] [tests/hydrodynamics/rao_tabulation/test_reliability.py:L79-L91] |
| S22: CLI cutoff parser | `type=float` parses the lower bound before positive-finite validation; policy and basis must be supplied together | C0.2 for paired options and invalid cutoff; no detection check exists for locale parser cases; issue pending (1) | [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L118-L125] |
| S23: CLI policy default | With no options, policy remains None and every row is explicitly unevaluated | C0.2; intentional unqualified result | [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L121-L125] [tests/hydrodynamics/rao_tabulation/test_reliability.py:L247-L255] |
| S24: zero exit code | Success returns integer 0 after rows are written; conversion exceptions are not swallowed and output is opened exclusively | C0.3; intentional exit status | [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L138-L147] [tests/hydrodynamics/rao_tabulation/test_reliability.py:L150-L191] |

| Additional pitfall / incident | Detection / disposition | Evidence |
| --- | --- | --- |
| #1548: low-frequency artifacts motivate dataset-specific flags; a range screen does not repair the source solution or qualify in-range data | C0.2/C0.3 detect label and preservation behavior; no detection check exists for physical repair or downstream flag propagation; retain #1548 | [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L1-L9] [docs/domains/hydrodynamics/rao-frequency-screening.md:L89-L101] |
| Scalar phase interpolation does not unwrap angles or encode phase references | no detection check exists for wrap-boundary/reference handling; issue pending (2) | [src/digitalmodel/hydrodynamics/interpolator.py:L134-L163] |
| Native annotation is not integrated into the router; interpolation cannot carry flags not represented in its arrays | no detection check exists for flag propagation; #1548 | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L49-L66] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L203-L219] [docs/domains/hydrodynamics/rao-frequency-screening.md:L89-L95] |
| Implementation-map rows attach #2107 as `not_checked`; this is not a confirmed tabulation defect or an independent hand-check reference | no detection check exists here for upstream parser grid truncation; map follow-up issue pending (3) | [docs/registry/implementation-map.yaml:L11799-L11812] |

## 7. Lessons log

| Date | Record | Lesson | Evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- | --- |
| 2026-11-07 | Initial pack | Treat frequency screening, numerical regression and physical qualification as separate evidence; preserve native findings across any later interpolation | [docs/domains/hydrodynamics/rao-frequency-screening.md:L89-L101] | Separate scope, regression cases and failure modes | Not assigned |

## 8. Ownership

Required review roles: diffraction quality manager and deputy diffraction quality manager; production responsibility: hydrodynamics maintainers. Assign the reviewing individuals outside this public pack before clearance. Escalate method classification and tolerance disputes to the diffraction quality manager; unresolved scope or evidence decisions go to the maintainer.

Clearance remains pending a review by a different provider. This draft records no review approval.

## 9. Dependencies and implementation inventory

The workflow imports NumPy, `CoefficientsInterpolator`, `MatrixDOF` and `RAOData`; the shared two-dimensional implementation uses SciPy `RegularGridInterpolator`. [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L14-L22] [src/digitalmodel/hydrodynamics/interpolator.py:L138-L163]

The engine dispatches the `rao_tabulation` basename to this workflow, and the durable workflow registry supplies the offline input, expected CSV and regression node. [src/digitalmodel/engine.py:L277-L283] [docs/registry/workflows.yaml:L364-L380]

The downstream parametric route `_rao_interpolator` loads the YAML database itself and `_rao_heave` delegates to the same shared interpolator; its numerical agreement is not an independent physical reference. [src/digitalmodel/parametric/generate.py:L100-L138] [tests/parametric/test_atlas.py:L129-L151]

No solver-version claim is made for the CSV workflow or native screen; source solver versions and qualification evidence must be supplied with any engineering dataset. [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L25-L46] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L7-L9]

### Implementation-map basis and counted family

All implementation-map citations in this pack resolve at map commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` on `codex/implmap-work`, whose data revision is `270c8759c374b67a7f032953e7fcdb14e0224461`; all other citations resolve at the code pin. [docs/registry/implementation-map.yaml:L1-L4]

The only map family with rows in this package is `rao-readers-and-orcaflex-converters`: 5 in-package rows, 295 total implementation rows; it is therefore the only counted family. All 295 rows, including helpers, tests and external paths, are inventoried below by their one-based ordinal in that family's `implementations` sequence and exact map source range. The source range is the canonical locator for path, symbol, relation and recorded status; external discovery identifiers are not repeated here. [docs/registry/implementation-map.yaml:L11799-L11861] [docs/registry/implementation-map.md:L21-L21]

| In-package map symbol | Recorded relation | Actual role at code pin | Evidence |
| --- | --- | --- | --- |
| `router` | caller | orchestration and summary | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L25-L46] |
| `_database` | independent | mapping selection / type validation, no interpolation formula | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L69-L73] |
| `_rao_array` | independent | array conversion / shape / finiteness validation | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L106-L124] |
| `_interpolated_rows` | caller | delegates interpolation to shared implementation | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L203-L219] |
| `_check_query_bounds` | independent | explicit grid-range criterion | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L222-L234] |

### Counted-family grep and classification

Search procedure: read every package `.py` file at the code pin, apply each map `search_signals.pattern` verbatim using case-sensitive Python `re.search`, and assign hits to the smallest enclosing AST function/class or one module-level definition. The vocabulary signal is excluded because its exact pattern matches `hydrod` in the package-relative path under the case-insensitive path comparison; the algorithm and constants/key signals remain case-sensitive and unchanged. [docs/registry/implementation-map.yaml:L9512-L9514]

Verbatim signals used for this reconciliation:

```text
algorithm: \.lis\b|\.hyd\b|\.owr\b|read_rao|parse_rao|read_hyd|read_wamit|read_nemoh|parse_lis|rao_data|rao_table|RAOAmplitude|RAOPhase|DisplacementRAOs|read_csv|read_excel|loadtxt|open_dataset|netCDF4|read_hydrodynamic|parse_aqwa|parse_wamit|parse_nemoh|convert_rao|RAOInterpolator|self\.content|_table_span|parse_aqwa_lis_file|AQWALISParser
vocabulary (path-excluded): \bRAO\b|\braos?\b|AQWA|WAMIT|NEMOH|HydroD|BEMRosetta|Capytaine
constants_or_keys: ADDED MASS|RADIATION DAMPING|displacement_rao|displacementRAO|DisplacementRAOs|RAOPeriodOrFrequency|RAOAmplitude|RAOPhase|RAODirection
```

| Hit definition | Classification | Evidence |
| --- | --- | --- |
| `router` | Listed in the inventory | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L25-L46] |
| `_rao_data` | Data holder / conversion wrapper delegating array preparation; helper, not a further implementation | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L49-L66] |
| `_database` | Listed in the inventory | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L69-L73] |
| `_rao_array` | Listed in the inventory | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L106-L124] |
| `_interpolated_rows` | Listed in the inventory | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L203-L219] |
| `_check_query_bounds` | Listed in the inventory | [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L222-L234] |

There are 6 hit definitions: 5 already listed plus the `_rao_data` holder/wrapper; further independent implementations = 0, summed over the 1 counted family, so the inventory/grep self-check passes. The wrapper selects arrays, applies shared sort orders and constructs `RAOData`; it does not implement the interpolation algorithm. [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L49-L66] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L203-L219]

### Complete map inventory

| Family row | Scope | Map relation | Canonical implementation record |
| --- | --- | --- | --- |
| 1 | external | `independent` | [docs/registry/implementation-map.yaml:L9516-L9527] |
| 2 | external | `independent` | [docs/registry/implementation-map.yaml:L9528-L9539] |
| 3 | external | `independent` | [docs/registry/implementation-map.yaml:L9540-L9551] |
| 4 | external | `independent` | [docs/registry/implementation-map.yaml:L9552-L9563] |
| 5 | external | `independent` | [docs/registry/implementation-map.yaml:L9564-L9578] |
| 6 | external | `independent` | [docs/registry/implementation-map.yaml:L9579-L9589] |
| 7 | external | `independent` | [docs/registry/implementation-map.yaml:L9590-L9601] |
| 8 | external | `independent` | [docs/registry/implementation-map.yaml:L9602-L9613] |
| 9 | external | `independent` | [docs/registry/implementation-map.yaml:L9614-L9625] |
| 10 | external | `independent` | [docs/registry/implementation-map.yaml:L9626-L9637] |
| 11 | external | `independent` | [docs/registry/implementation-map.yaml:L9638-L9652] |
| 12 | external | `independent` | [docs/registry/implementation-map.yaml:L9653-L9664] |
| 13 | external | `independent` | [docs/registry/implementation-map.yaml:L9665-L9678] |
| 14 | external | `independent` | [docs/registry/implementation-map.yaml:L9679-L9690] |
| 15 | external | `independent` | [docs/registry/implementation-map.yaml:L9691-L9704] |
| 16 | external | `independent` | [docs/registry/implementation-map.yaml:L9705-L9715] |
| 17 | external | `independent` | [docs/registry/implementation-map.yaml:L9716-L9727] |
| 18 | external | `independent` | [docs/registry/implementation-map.yaml:L9728-L9739] |
| 19 | external | `independent` | [docs/registry/implementation-map.yaml:L9740-L9753] |
| 20 | external | `independent` | [docs/registry/implementation-map.yaml:L9754-L9765] |
| 21 | external | `independent` | [docs/registry/implementation-map.yaml:L9766-L9777] |
| 22 | external | `independent` | [docs/registry/implementation-map.yaml:L9778-L9788] |
| 23 | external | `independent` | [docs/registry/implementation-map.yaml:L9789-L9800] |
| 24 | external | `independent` | [docs/registry/implementation-map.yaml:L9801-L9812] |
| 25 | external | `example` | [docs/registry/implementation-map.yaml:L9813-L9826] |
| 26 | external | `example` | [docs/registry/implementation-map.yaml:L9827-L9838] |
| 27 | external | `independent` | [docs/registry/implementation-map.yaml:L9839-L9849] |
| 28 | external | `independent` | [docs/registry/implementation-map.yaml:L9850-L9861] |
| 29 | external | `independent` | [docs/registry/implementation-map.yaml:L9862-L9873] |
| 30 | external | `independent` | [docs/registry/implementation-map.yaml:L9874-L9885] |
| 31 | external | `independent` | [docs/registry/implementation-map.yaml:L9886-L9897] |
| 32 | external | `independent` | [docs/registry/implementation-map.yaml:L9898-L9909] |
| 33 | external | `independent` | [docs/registry/implementation-map.yaml:L9910-L9921] |
| 34 | external | `independent` | [docs/registry/implementation-map.yaml:L9922-L9932] |
| 35 | external | `independent` | [docs/registry/implementation-map.yaml:L9933-L9943] |
| 36 | external | `independent` | [docs/registry/implementation-map.yaml:L9944-L9954] |
| 37 | external | `independent` | [docs/registry/implementation-map.yaml:L9955-L9965] |
| 38 | external | `independent` | [docs/registry/implementation-map.yaml:L9966-L9977] |
| 39 | external | `independent` | [docs/registry/implementation-map.yaml:L9978-L9989] |
| 40 | external | `independent` | [docs/registry/implementation-map.yaml:L9990-L10004] |
| 41 | external | `independent` | [docs/registry/implementation-map.yaml:L10005-L10017] |
| 42 | external | `independent` | [docs/registry/implementation-map.yaml:L10018-L10032] |
| 43 | external | `independent` | [docs/registry/implementation-map.yaml:L10033-L10046] |
| 44 | external | `independent` | [docs/registry/implementation-map.yaml:L10047-L10060] |
| 45 | external | `independent` | [docs/registry/implementation-map.yaml:L10061-L10072] |
| 46 | external | `independent` | [docs/registry/implementation-map.yaml:L10073-L10087] |
| 47 | external | `independent` | [docs/registry/implementation-map.yaml:L10088-L10099] |
| 48 | external | `independent` | [docs/registry/implementation-map.yaml:L10100-L10111] |
| 49 | external | `independent` | [docs/registry/implementation-map.yaml:L10112-L10122] |
| 50 | external | `independent` | [docs/registry/implementation-map.yaml:L10123-L10134] |
| 51 | external | `independent` | [docs/registry/implementation-map.yaml:L10135-L10146] |
| 52 | external | `independent` | [docs/registry/implementation-map.yaml:L10147-L10158] |
| 53 | external | `independent` | [docs/registry/implementation-map.yaml:L10159-L10169] |
| 54 | external | `independent` | [docs/registry/implementation-map.yaml:L10170-L10181] |
| 55 | external | `independent` | [docs/registry/implementation-map.yaml:L10182-L10193] |
| 56 | external | `independent` | [docs/registry/implementation-map.yaml:L10194-L10207] |
| 57 | external | `independent` | [docs/registry/implementation-map.yaml:L10208-L10222] |
| 58 | external | `independent` | [docs/registry/implementation-map.yaml:L10223-L10235] |
| 59 | external | `independent` | [docs/registry/implementation-map.yaml:L10236-L10246] |
| 60 | external | `independent` | [docs/registry/implementation-map.yaml:L10247-L10260] |
| 61 | external | `independent` | [docs/registry/implementation-map.yaml:L10261-L10272] |
| 62 | external | `independent` | [docs/registry/implementation-map.yaml:L10273-L10284] |
| 63 | external | `independent` | [docs/registry/implementation-map.yaml:L10285-L10296] |
| 64 | external | `independent` | [docs/registry/implementation-map.yaml:L10297-L10307] |
| 65 | external | `independent` | [docs/registry/implementation-map.yaml:L10308-L10319] |
| 66 | external | `independent` | [docs/registry/implementation-map.yaml:L10320-L10333] |
| 67 | external | `independent` | [docs/registry/implementation-map.yaml:L10334-L10345] |
| 68 | external | `independent` | [docs/registry/implementation-map.yaml:L10346-L10356] |
| 69 | external | `independent` | [docs/registry/implementation-map.yaml:L10357-L10368] |
| 70 | external | `independent` | [docs/registry/implementation-map.yaml:L10369-L10380] |
| 71 | external | `independent` | [docs/registry/implementation-map.yaml:L10381-L10391] |
| 72 | external | `independent` | [docs/registry/implementation-map.yaml:L10392-L10403] |
| 73 | external | `independent` | [docs/registry/implementation-map.yaml:L10404-L10415] |
| 74 | external | `independent` | [docs/registry/implementation-map.yaml:L10416-L10427] |
| 75 | external | `independent` | [docs/registry/implementation-map.yaml:L10428-L10439] |
| 76 | external | `independent` | [docs/registry/implementation-map.yaml:L10440-L10451] |
| 77 | external | `independent` | [docs/registry/implementation-map.yaml:L10452-L10465] |
| 78 | external | `independent` | [docs/registry/implementation-map.yaml:L10466-L10477] |
| 79 | external | `independent` | [docs/registry/implementation-map.yaml:L10478-L10489] |
| 80 | external | `independent` | [docs/registry/implementation-map.yaml:L10490-L10501] |
| 81 | external | `independent` | [docs/registry/implementation-map.yaml:L10502-L10513] |
| 82 | external | `caller` | [docs/registry/implementation-map.yaml:L10514-L10527] |
| 83 | external | `independent` | [docs/registry/implementation-map.yaml:L10528-L10539] |
| 84 | external | `independent` | [docs/registry/implementation-map.yaml:L10540-L10551] |
| 85 | external | `independent` | [docs/registry/implementation-map.yaml:L10552-L10563] |
| 86 | external | `independent` | [docs/registry/implementation-map.yaml:L10564-L10575] |
| 87 | external | `independent` | [docs/registry/implementation-map.yaml:L10576-L10587] |
| 88 | external | `independent` | [docs/registry/implementation-map.yaml:L10588-L10599] |
| 89 | external | `independent` | [docs/registry/implementation-map.yaml:L10600-L10611] |
| 90 | external | `independent` | [docs/registry/implementation-map.yaml:L10612-L10623] |
| 91 | external | `independent` | [docs/registry/implementation-map.yaml:L10624-L10635] |
| 92 | external | `independent` | [docs/registry/implementation-map.yaml:L10636-L10647] |
| 93 | external | `independent` | [docs/registry/implementation-map.yaml:L10648-L10659] |
| 94 | external | `independent` | [docs/registry/implementation-map.yaml:L10660-L10671] |
| 95 | external | `independent` | [docs/registry/implementation-map.yaml:L10672-L10683] |
| 96 | external | `independent` | [docs/registry/implementation-map.yaml:L10684-L10699] |
| 97 | external | `independent` | [docs/registry/implementation-map.yaml:L10700-L10711] |
| 98 | external | `independent` | [docs/registry/implementation-map.yaml:L10712-L10725] |
| 99 | external | `independent` | [docs/registry/implementation-map.yaml:L10726-L10737] |
| 100 | external | `independent` | [docs/registry/implementation-map.yaml:L10738-L10749] |
| 101 | external | `independent` | [docs/registry/implementation-map.yaml:L10750-L10761] |
| 102 | external | `independent` | [docs/registry/implementation-map.yaml:L10762-L10775] |
| 103 | external | `independent` | [docs/registry/implementation-map.yaml:L10776-L10789] |
| 104 | external | `independent` | [docs/registry/implementation-map.yaml:L10790-L10801] |
| 105 | external | `caller` | [docs/registry/implementation-map.yaml:L10802-L10815] |
| 106 | external | `caller` | [docs/registry/implementation-map.yaml:L10816-L10828] |
| 107 | external | `caller` | [docs/registry/implementation-map.yaml:L10829-L10841] |
| 108 | external | `independent` | [docs/registry/implementation-map.yaml:L10842-L10853] |
| 109 | external | `independent` | [docs/registry/implementation-map.yaml:L10854-L10865] |
| 110 | external | `independent` | [docs/registry/implementation-map.yaml:L10866-L10877] |
| 111 | external | `independent` | [docs/registry/implementation-map.yaml:L10878-L10891] |
| 112 | external | `independent` | [docs/registry/implementation-map.yaml:L10892-L10905] |
| 113 | external | `independent` | [docs/registry/implementation-map.yaml:L10906-L10917] |
| 114 | external | `caller` | [docs/registry/implementation-map.yaml:L10918-L10933] |
| 115 | external | `caller` | [docs/registry/implementation-map.yaml:L10934-L10948] |
| 116 | external | `independent` | [docs/registry/implementation-map.yaml:L10949-L10962] |
| 117 | external | `wrapper` | [docs/registry/implementation-map.yaml:L10963-L10979] |
| 118 | external | `independent` | [docs/registry/implementation-map.yaml:L10980-L10991] |
| 119 | external | `independent` | [docs/registry/implementation-map.yaml:L10992-L11003] |
| 120 | external | `independent` | [docs/registry/implementation-map.yaml:L11004-L11015] |
| 121 | external | `independent` | [docs/registry/implementation-map.yaml:L11016-L11027] |
| 122 | external | `independent` | [docs/registry/implementation-map.yaml:L11028-L11039] |
| 123 | external | `independent` | [docs/registry/implementation-map.yaml:L11040-L11051] |
| 124 | external | `independent` | [docs/registry/implementation-map.yaml:L11052-L11063] |
| 125 | external | `independent` | [docs/registry/implementation-map.yaml:L11064-L11075] |
| 126 | external | `independent` | [docs/registry/implementation-map.yaml:L11076-L11089] |
| 127 | external | `independent` | [docs/registry/implementation-map.yaml:L11090-L11101] |
| 128 | external | `independent` | [docs/registry/implementation-map.yaml:L11102-L11113] |
| 129 | external | `independent` | [docs/registry/implementation-map.yaml:L11114-L11125] |
| 130 | external | `independent` | [docs/registry/implementation-map.yaml:L11126-L11137] |
| 131 | external | `independent` | [docs/registry/implementation-map.yaml:L11138-L11149] |
| 132 | external | `independent` | [docs/registry/implementation-map.yaml:L11150-L11163] |
| 133 | external | `independent` | [docs/registry/implementation-map.yaml:L11164-L11175] |
| 134 | external | `independent` | [docs/registry/implementation-map.yaml:L11176-L11187] |
| 135 | external | `independent` | [docs/registry/implementation-map.yaml:L11188-L11202] |
| 136 | external | `independent` | [docs/registry/implementation-map.yaml:L11203-L11213] |
| 137 | external | `independent` | [docs/registry/implementation-map.yaml:L11214-L11225] |
| 138 | external | `independent` | [docs/registry/implementation-map.yaml:L11226-L11237] |
| 139 | external | `independent` | [docs/registry/implementation-map.yaml:L11238-L11248] |
| 140 | external | `independent` | [docs/registry/implementation-map.yaml:L11249-L11260] |
| 141 | external | `caller` | [docs/registry/implementation-map.yaml:L11261-L11274] |
| 142 | external | `independent` | [docs/registry/implementation-map.yaml:L11275-L11286] |
| 143 | external | `independent` | [docs/registry/implementation-map.yaml:L11287-L11298] |
| 144 | external | `independent` | [docs/registry/implementation-map.yaml:L11299-L11310] |
| 145 | external | `independent` | [docs/registry/implementation-map.yaml:L11311-L11322] |
| 146 | external | `independent` | [docs/registry/implementation-map.yaml:L11323-L11334] |
| 147 | external | `independent` | [docs/registry/implementation-map.yaml:L11335-L11346] |
| 148 | external | `independent` | [docs/registry/implementation-map.yaml:L11347-L11360] |
| 149 | external | `independent` | [docs/registry/implementation-map.yaml:L11361-L11374] |
| 150 | external | `independent` | [docs/registry/implementation-map.yaml:L11375-L11388] |
| 151 | external | `independent` | [docs/registry/implementation-map.yaml:L11389-L11400] |
| 152 | external | `caller` | [docs/registry/implementation-map.yaml:L11401-L11414] |
| 153 | external | `independent` | [docs/registry/implementation-map.yaml:L11415-L11426] |
| 154 | external | `independent` | [docs/registry/implementation-map.yaml:L11427-L11438] |
| 155 | external | `independent` | [docs/registry/implementation-map.yaml:L11439-L11450] |
| 156 | external | `independent` | [docs/registry/implementation-map.yaml:L11451-L11462] |
| 157 | external | `independent` | [docs/registry/implementation-map.yaml:L11463-L11474] |
| 158 | external | `independent` | [docs/registry/implementation-map.yaml:L11475-L11486] |
| 159 | external | `independent` | [docs/registry/implementation-map.yaml:L11487-L11498] |
| 160 | external | `independent` | [docs/registry/implementation-map.yaml:L11499-L11510] |
| 161 | external | `independent` | [docs/registry/implementation-map.yaml:L11511-L11524] |
| 162 | external | `independent` | [docs/registry/implementation-map.yaml:L11525-L11536] |
| 163 | external | `independent` | [docs/registry/implementation-map.yaml:L11537-L11548] |
| 164 | external | `independent` | [docs/registry/implementation-map.yaml:L11549-L11560] |
| 165 | external | `caller` | [docs/registry/implementation-map.yaml:L11561-L11574] |
| 166 | external | `independent` | [docs/registry/implementation-map.yaml:L11575-L11590] |
| 167 | external | `caller` | [docs/registry/implementation-map.yaml:L11591-L11603] |
| 168 | external | `independent` | [docs/registry/implementation-map.yaml:L11604-L11619] |
| 169 | external | `caller` | [docs/registry/implementation-map.yaml:L11620-L11633] |
| 170 | external | `independent` | [docs/registry/implementation-map.yaml:L11634-L11648] |
| 171 | external | `caller` | [docs/registry/implementation-map.yaml:L11649-L11662] |
| 172 | external | `independent` | [docs/registry/implementation-map.yaml:L11663-L11674] |
| 173 | external | `independent` | [docs/registry/implementation-map.yaml:L11675-L11686] |
| 174 | external | `independent` | [docs/registry/implementation-map.yaml:L11687-L11698] |
| 175 | external | `independent` | [docs/registry/implementation-map.yaml:L11699-L11710] |
| 176 | external | `caller` | [docs/registry/implementation-map.yaml:L11711-L11724] |
| 177 | external | `independent` | [docs/registry/implementation-map.yaml:L11725-L11736] |
| 178 | external | `independent` | [docs/registry/implementation-map.yaml:L11737-L11748] |
| 179 | external | `independent` | [docs/registry/implementation-map.yaml:L11749-L11762] |
| 180 | external | `independent` | [docs/registry/implementation-map.yaml:L11763-L11774] |
| 181 | external | `independent` | [docs/registry/implementation-map.yaml:L11775-L11786] |
| 182 | external | `independent` | [docs/registry/implementation-map.yaml:L11787-L11798] |
| 183 | package: `router` | `caller` | [docs/registry/implementation-map.yaml:L11799-L11812] |
| 184 | package: `_database` | `independent` | [docs/registry/implementation-map.yaml:L11813-L11824] |
| 185 | package: `_rao_array` | `independent` | [docs/registry/implementation-map.yaml:L11825-L11836] |
| 186 | package: `_interpolated_rows` | `caller` | [docs/registry/implementation-map.yaml:L11837-L11850] |
| 187 | package: `_check_query_bounds` | `independent` | [docs/registry/implementation-map.yaml:L11851-L11862] |
| 188 | external | `independent` | [docs/registry/implementation-map.yaml:L11863-L11874] |
| 189 | external | `independent` | [docs/registry/implementation-map.yaml:L11875-L11888] |
| 190 | external | `independent` | [docs/registry/implementation-map.yaml:L11889-L11900] |
| 191 | external | `independent` | [docs/registry/implementation-map.yaml:L11901-L11912] |
| 192 | external | `caller` | [docs/registry/implementation-map.yaml:L11913-L11926] |
| 193 | external | `independent` | [docs/registry/implementation-map.yaml:L11927-L11938] |
| 194 | external | `independent` | [docs/registry/implementation-map.yaml:L11939-L11950] |
| 195 | external | `caller` | [docs/registry/implementation-map.yaml:L11951-L11964] |
| 196 | external | `independent` | [docs/registry/implementation-map.yaml:L11965-L11976] |
| 197 | external | `caller` | [docs/registry/implementation-map.yaml:L11977-L11990] |
| 198 | external | `independent` | [docs/registry/implementation-map.yaml:L11991-L12002] |
| 199 | external | `independent` | [docs/registry/implementation-map.yaml:L12003-L12014] |
| 200 | external | `independent` | [docs/registry/implementation-map.yaml:L12015-L12026] |
| 201 | external | `independent` | [docs/registry/implementation-map.yaml:L12027-L12038] |
| 202 | external | `caller` | [docs/registry/implementation-map.yaml:L12039-L12052] |
| 203 | external | `independent` | [docs/registry/implementation-map.yaml:L12053-L12064] |
| 204 | external | `caller` | [docs/registry/implementation-map.yaml:L12065-L12078] |
| 205 | external | `wrapper` | [docs/registry/implementation-map.yaml:L12079-L12095] |
| 206 | external | `independent` | [docs/registry/implementation-map.yaml:L12096-L12107] |
| 207 | external | `independent` | [docs/registry/implementation-map.yaml:L12108-L12119] |
| 208 | external | `independent` | [docs/registry/implementation-map.yaml:L12120-L12131] |
| 209 | external | `independent` | [docs/registry/implementation-map.yaml:L12132-L12143] |
| 210 | external | `caller` | [docs/registry/implementation-map.yaml:L12144-L12159] |
| 211 | external | `caller` | [docs/registry/implementation-map.yaml:L12160-L12173] |
| 212 | external | `caller` | [docs/registry/implementation-map.yaml:L12174-L12186] |
| 213 | external | `caller` | [docs/registry/implementation-map.yaml:L12187-L12199] |
| 214 | external | `independent` | [docs/registry/implementation-map.yaml:L12200-L12211] |
| 215 | external | `caller` | [docs/registry/implementation-map.yaml:L12212-L12226] |
| 216 | external | `independent` | [docs/registry/implementation-map.yaml:L12227-L12238] |
| 217 | external | `caller` | [docs/registry/implementation-map.yaml:L12239-L12254] |
| 218 | external | `copy` | [docs/registry/implementation-map.yaml:L12255-L12266] |
| 219 | external | `independent` | [docs/registry/implementation-map.yaml:L12267-L12278] |
| 220 | external | `independent` | [docs/registry/implementation-map.yaml:L12279-L12290] |
| 221 | external | `independent` | [docs/registry/implementation-map.yaml:L12291-L12302] |
| 222 | external | `independent` | [docs/registry/implementation-map.yaml:L12303-L12314] |
| 223 | external | `independent` | [docs/registry/implementation-map.yaml:L12315-L12326] |
| 224 | external | `independent` | [docs/registry/implementation-map.yaml:L12327-L12338] |
| 225 | external | `independent` | [docs/registry/implementation-map.yaml:L12339-L12350] |
| 226 | external | `independent` | [docs/registry/implementation-map.yaml:L12351-L12362] |
| 227 | external | `caller` | [docs/registry/implementation-map.yaml:L12363-L12376] |
| 228 | external | `independent` | [docs/registry/implementation-map.yaml:L12377-L12388] |
| 229 | external | `independent` | [docs/registry/implementation-map.yaml:L12389-L12400] |
| 230 | external | `independent` | [docs/registry/implementation-map.yaml:L12401-L12412] |
| 231 | external | `caller` | [docs/registry/implementation-map.yaml:L12413-L12427] |
| 232 | external | `wrapper` | [docs/registry/implementation-map.yaml:L12428-L12441] |
| 233 | external | `independent` | [docs/registry/implementation-map.yaml:L12442-L12453] |
| 234 | external | `independent` | [docs/registry/implementation-map.yaml:L12454-L12465] |
| 235 | external | `independent` | [docs/registry/implementation-map.yaml:L12466-L12477] |
| 236 | external | `independent` | [docs/registry/implementation-map.yaml:L12478-L12489] |
| 237 | external | `independent` | [docs/registry/implementation-map.yaml:L12490-L12503] |
| 238 | external | `independent` | [docs/registry/implementation-map.yaml:L12504-L12515] |
| 239 | external | `independent` | [docs/registry/implementation-map.yaml:L12516-L12529] |
| 240 | external | `independent` | [docs/registry/implementation-map.yaml:L12530-L12541] |
| 241 | external | `independent` | [docs/registry/implementation-map.yaml:L12542-L12555] |
| 242 | external | `independent` | [docs/registry/implementation-map.yaml:L12556-L12567] |
| 243 | external | `independent` | [docs/registry/implementation-map.yaml:L12568-L12582] |
| 244 | external | `independent` | [docs/registry/implementation-map.yaml:L12583-L12594] |
| 245 | external | `independent` | [docs/registry/implementation-map.yaml:L12595-L12606] |
| 246 | external | `independent` | [docs/registry/implementation-map.yaml:L12607-L12620] |
| 247 | external | `independent` | [docs/registry/implementation-map.yaml:L12621-L12632] |
| 248 | external | `independent` | [docs/registry/implementation-map.yaml:L12633-L12646] |
| 249 | external | `independent` | [docs/registry/implementation-map.yaml:L12647-L12658] |
| 250 | external | `independent` | [docs/registry/implementation-map.yaml:L12659-L12670] |
| 251 | external | `independent` | [docs/registry/implementation-map.yaml:L12671-L12682] |
| 252 | external | `independent` | [docs/registry/implementation-map.yaml:L12683-L12696] |
| 253 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12697-L12710] |
| 254 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12711-L12721] |
| 255 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12722-L12733] |
| 256 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12734-L12745] |
| 257 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12746-L12759] |
| 258 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12760-L12773] |
| 259 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12774-L12787] |
| 260 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12788-L12799] |
| 261 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12800-L12813] |
| 262 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12814-L12827] |
| 263 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12828-L12839] |
| 264 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12840-L12850] |
| 265 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12851-L12862] |
| 266 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12863-L12874] |
| 267 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12875-L12886] |
| 268 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12887-L12898] |
| 269 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12899-L12910] |
| 270 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12911-L12922] |
| 271 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12923-L12934] |
| 272 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12935-L12946] |
| 273 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12947-L12958] |
| 274 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12959-L12973] |
| 275 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12974-L12986] |
| 276 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12987-L12998] |
| 277 | external | `test_helper` | [docs/registry/implementation-map.yaml:L12999-L13010] |
| 278 | external | `test_helper` | [docs/registry/implementation-map.yaml:L13011-L13022] |
| 279 | external | `test_helper` | [docs/registry/implementation-map.yaml:L13023-L13034] |
| 280 | external | `test_helper` | [docs/registry/implementation-map.yaml:L13035-L13048] |
| 281 | external | `test_helper` | [docs/registry/implementation-map.yaml:L13049-L13062] |
| 282 | external | `test_helper` | [docs/registry/implementation-map.yaml:L13063-L13074] |
| 283 | external | `test_helper` | [docs/registry/implementation-map.yaml:L13075-L13086] |
| 284 | external | `test_helper` | [docs/registry/implementation-map.yaml:L13087-L13101] |
| 285 | external | `test_helper` | [docs/registry/implementation-map.yaml:L13102-L13113] |
| 286 | external | `test_helper` | [docs/registry/implementation-map.yaml:L13114-L13125] |
| 287 | external | `test_helper` | [docs/registry/implementation-map.yaml:L13126-L13139] |
| 288 | external | `test_helper` | [docs/registry/implementation-map.yaml:L13140-L13153] |
| 289 | external | `test_helper` | [docs/registry/implementation-map.yaml:L13154-L13167] |
| 290 | external | `test_helper` | [docs/registry/implementation-map.yaml:L13168-L13181] |
| 291 | external | `test_helper` | [docs/registry/implementation-map.yaml:L13182-L13193] |
| 292 | external | `test_helper` | [docs/registry/implementation-map.yaml:L13194-L13204] |
| 293 | external | `test_helper` | [docs/registry/implementation-map.yaml:L13205-L13216] |
| 294 | external | `test_helper` | [docs/registry/implementation-map.yaml:L13217-L13228] |
| 295 | external | `test_helper` | [docs/registry/implementation-map.yaml:L13229-L13240] |

### Whole-repository discovery and map follow-up

The whole-repository search includes tracked Python files in `scripts/`, using every family signal verbatim at the code pin; external results are map-discovery findings and do not add in-package misses. The canonical inventory above retains those external rows without asserting physical validity from a name match. [docs/registry/implementation-map.yaml:L9512-L9514]

Native frequency screening has no implementation row in this family's in-package inventory; add a family/rows before counting it in a later map revision. Other method families documented in section 3 for follow-up are angular-frequency conversion, numerical interpolation delegation, grid bounds/shape/finiteness checks, scalar phase treatment and CSV output/provenance handling. Reconcile broad #2107 attachments by affected path rather than treating `not_checked` as a confirmed defect. Tracking: issue pending (3), with physical flag-propagation work retained under #1548. [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L22-L105] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L76-L124] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L222-L250] [docs/registry/implementation-map.yaml:L11799-L11861]

## 10. Coverage and open items

All 3 package Python files were read: `__init__.py`, `workflow.py`, `reliability.py`; the package has no other source subpackage at the code pin. [src/digitalmodel/hydrodynamics/rao_tabulation/__init__.py:L1-L2] [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L1-L313] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L1-L151]

Related documentation discovered and read: [docs/domains/hydrodynamics/rao-frequency-screening.md:L1-L101] [docs/plans/parametric-refresh/adding-an-atlas.md:L115-L122] [docs/plans/parametric-refresh/historical-provenance.md:L77-L97] [docs/registry/workflows.yaml:L364-L380] [docs/registry/implementation-map.md:L21-L21] [docs/registry/agent-pack-audit/production-nde-functions-1.json:L8370-L8380] [docs/audits/workflow-failure-evidence-coverage.html:L88-L88]

Tests read: complete dedicated workflow and reliability files (5 and 42 expanded cases respectively); the durable registry case; related RAO atlas/adaptive, refresh/staleness and catalog tests; routing contracts and repository contract domain. No new tests or module-code changes are included. [tests/hydrodynamics/test_rao_tabulation_workflow.py:L1-L130] [tests/hydrodynamics/rao_tabulation/test_reliability.py:L1-L255] [tests/workflows/test_durable_workflows.py:L601-L620] [tests/parametric/test_atlas.py:L129-L151] [tests/parametric/test_atlas.py:L323-L338] [tests/parametric/test_refresh.py:L21-L86] [tests/parametric/test_catalog.py:L1-L40]

| Open item | Responsible role | Tracking / completion evidence |
| --- | --- | --- |
| Unestablished conventions from section 2: normalization, rotational units, frame, origin, heading sign and phase reference; strengthen numeric ingress, locale/boolean coverage, aliases, empty API input, output replacement and path provenance | hydrodynamics maintainer | issue pending (1); convention specification plus defensive tests for section-6 gaps; no client-derived data included |
| Independent physical worked example, phase-wrap/reference validation and continuous-peak applicability; current examples remain regression-only | diffraction quality manager | issue pending (2); independent benchmark and explicit expected units/tolerances |
| Native-screen implementation-map coverage and affected-path reconciliation for #2107; external discovery identifiers remain in canonical map records | hydrodynamics maintainer | issue pending (3); revised map rows and exact-signal reconciliation |
| Low-frequency numerical repair, source mesh/grid evidence, native-flag propagation and qualification of engineering datasets | diffraction quality manager | #1548; qualified source results and downstream flag-propagation checks; no detection check exists for physical repair in this module |

The calculation-output citation contract was readable during preparation; this pack introduces no standards-derived calculation constant or sidecar payload. [src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py:L76-L94] [src/digitalmodel/hydrodynamics/rao_tabulation/reliability.py:L22-L32]
