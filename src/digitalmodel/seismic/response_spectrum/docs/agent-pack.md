# Response-spectrum preprocessing knowledge pack

| Header | Value |
| --- | --- |
| Module | seismic/response_spectrum |
| Repository | digitalmodel |
| Package | src/digitalmodel/seismic/response_spectrum |
| Pack revision | 0 |
| Code revision | 21942e8cfc7dfb985c5bfa693245df7d197cf76c |
| Analysis family | Engineering calculations (EC); AQG r0, section 10, Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

Citations below resolve at the code revision unless section 9 explicitly identifies the historical implementation-map revision.

## 1. Purpose and scope

`compute_kinematics` corrects an accelerogram baseline, optionally filters acceleration, and integrates acceleration to velocity and then displacement; it does not solve the SDOF equation of motion. [src/digitalmodel/seismic/response_spectrum/calculations.py:L1-L5] [src/digitalmodel/seismic/response_spectrum/calculations.py:L35-L64]

`router` reads a record, writes a timeseries CSV and HTML report, then replaces the configuration's response-spectrum block with a result payload; the engine selects this route for `basename: response_spectrum`. [src/digitalmodel/seismic/response_spectrum/workflow.py:L16-L47] [src/digitalmodel/engine.py:L732-L737]

The seismic domain has development maturity; its operator map describes strong-motion preprocessing and workflow scaffolding. [docs/registry/module-routing.yaml:L428-L437] [docs/maps/digitalmodel-operator-map.md:L65-L65]

Scope is the four Python files in the header package; the sibling reader, baseline, SDOF and elastic-layer implementations are dependencies or adjacent methods, rather than additional package coverage. [src/digitalmodel/seismic/response_spectrum/__init__.py:L1-L5] [src/digitalmodel/seismic/response_spectrum/calculations.py:L17-L21] [src/digitalmodel/seismic/response_spectrum/workflow.py:L8-L11]

## 2. Inputs and outputs

### Inputs

| Input | Unit | Range or behaviour | Default | Evidence |
| --- | --- | --- | --- | --- |
| `record.file` | path | Required mapping key; relative paths use configuration directory | No router default | [src/digitalmodel/seismic/response_spectrum/workflow.py:L19-L26] [src/digitalmodel/seismic/response_spectrum/workflow.py:L90-L102] |
| `record.units`, `record.dt_s` | g, cm/s² or m/s²; s | Reader accepts two-column time/acceleration or one-column acceleration with time step; uniform increasing times required | Router passes None; reader chooses explicit units, metadata, then m/s² | [src/digitalmodel/seismic/response_spectrum/workflow.py:L22-L26] [src/digitalmodel/seismic/readers/ascii.py:L104-L126] [src/digitalmodel/seismic/readers/ascii.py:L157-L205] |
| `baseline.method` | dimensionless choice | `none`, `pre_event_mean`, `polynomial` or `poly`; other strings raise ValueError | `pre_event_mean` | [src/digitalmodel/seismic/response_spectrum/calculations.py:L67-L96] |
| `baseline.samples` | samples | Converted with int; upstream requires positive count no greater than record length | min(10, npts) in calculation; base template specifies 20 | [src/digitalmodel/seismic/response_spectrum/calculations.py:L74-L82] [src/digitalmodel/seismic/baseline.py:L15-L21] [src/digitalmodel/base_configs/modules/response_spectrum/response_spectrum.yml:L8-L10] |
| `baseline.order` | polynomial degree | Converted with int; nonnegative and less than sample count in helper | 1 | [src/digitalmodel/seismic/response_spectrum/calculations.py:L83-L95] [src/digitalmodel/seismic/baseline.py:L32-L42] |
| `filter.enabled`, `filter.band_hz`, `filter.order` | Boolean; Hz; dimensionless order | Explicit False or missing/empty band bypasses filter; two numeric band edges; upper edge clipped to 0.99 of Nyquist | Empty filter mapping bypasses; enabled template uses order 4 | [src/digitalmodel/seismic/response_spectrum/calculations.py:L99-L122] [src/digitalmodel/base_configs/modules/response_spectrum/response_spectrum.yml:L11-L14] |
| `output_dir`, `generated_label` | path; text | Relative output directory uses configuration directory; falsey label uses module label | `results`; generated workflow label | [src/digitalmodel/seismic/response_spectrum/workflow.py:L32-L42] [src/digitalmodel/seismic/response_spectrum/workflow.py:L83-L87] [src/digitalmodel/seismic/response_spectrum/render.py:L26-L39] |

Time is in seconds and acceleration is converted to SI by the reader; integration starts with zero velocity and displacement and retains the supplied sample order. [src/digitalmodel/seismic/readers/ascii.py:L118-L126] [src/digitalmodel/seismic/readers/ascii.py:L199-L205] [src/digitalmodel/seismic/response_spectrum/calculations.py:L45-L56]

UNVERIFIED: physical axis, sensor orientation, spatial origin and acceleration polarity are not qualified for an engineering use; require explicit input metadata before interpreting integrated motion (section 10, issue pending (1)).

### Outputs

| Output | Unit or representation | Content | Evidence |
| --- | --- | --- | --- |
| `KinematicResult.timeseries` | s, m/s², m/s, m | time, raw/corrected/filtered acceleration, velocity, displacement | [src/digitalmodel/seismic/response_spectrum/calculations.py:L49-L64] |
| `summary` | samples, s, m/s², g | npts, dt, duration and raw-record PGA; not filtered peaks | [src/digitalmodel/seismic/response_spectrum/calculations.py:L125-L132] [src/digitalmodel/seismic/readers/ascii.py:L81-L101] |
| `processing_chain` | list of mappings | baseline parameters and filter requested/effective band | [src/digitalmodel/seismic/response_spectrum/calculations.py:L63-L64] [src/digitalmodel/seismic/response_spectrum/calculations.py:L67-L122] |
| `<stem>_timeseries.csv` | Text, SI columns | No row index; numeric format %.10g | [src/digitalmodel/seismic/response_spectrum/workflow.py:L32-L36] [src/digitalmodel/seismic/response_spectrum/workflow.py:L78-L80] |
| `<stem>_report.html` | HTML | Summary, timeseries and processing, with provenance and assumption ledger | [src/digitalmodel/seismic/response_spectrum/render.py:L29-L53] |
| Returned configuration | mappings and paths | Unconditional `screening_status: pass` after successful output; result method/summary/chain/paths and outputs mapping | [src/digitalmodel/seismic/response_spectrum/workflow.py:L43-L58] |

External source identifiers retain a basename and digest, whereas external output paths are returned as absolute paths; those identifiers must not be published without inspection. [src/digitalmodel/seismic/response_spectrum/workflow.py:L70-L75] [src/digitalmodel/seismic/response_spectrum/workflow.py:L111-L116]

## 3. Method and governing standards

| Method family | Implementing definition | Calculation or delegation | Reference basis |
| --- | --- | --- | --- |
| Baseline correction | `_apply_baseline` | None copies input; pre-event mean subtracts first-window mean; polynomial subtracts fitted polynomial | [src/digitalmodel/seismic/response_spectrum/calculations.py:L67-L96] [src/digitalmodel/seismic/baseline.py:L8-L42] |
| Bandpass filtering | `_apply_filter` | Sampling rate 1/dt, Nyquist half sampling rate, effective upper edge min(requested, 0.99 Nyquist), default order 4; delegates to FrequencyFilter | [src/digitalmodel/seismic/response_spectrum/calculations.py:L99-L122] |
| Filter design/application | `FrequencyFilter.bandpass_filter` upstream | Butterworth coefficients, then forward/backward filtfilt by default | [src/digitalmodel/signal_processing/signal_analysis/filters/frequency.py:L60-L70] [src/digitalmodel/signal_processing/signal_analysis/filters/frequency.py:L96-L125] [src/digitalmodel/signal_processing/signal_analysis/filters/frequency.py:L219-L250] |
| Kinematic integration | `compute_kinematics` | Two cumulative trapezoidal integrations on supplied time coordinates, each initial=0.0 | [src/digitalmodel/seismic/response_spectrum/calculations.py:L43-L64] |
| Record summary | `_summary` | Raw-record count, interval, duration and peak absolute acceleration | [src/digitalmodel/seismic/response_spectrum/calculations.py:L125-L132] [src/digitalmodel/seismic/readers/ascii.py:L81-L101] |
| Report/provenance | `render_report`, `_ledger`, `_data_source` | Report backbone with source digest; processing methods recorded as assumed defaults at high confidence | [src/digitalmodel/seismic/response_spectrum/render.py:L29-L53] [src/digitalmodel/seismic/response_spectrum/render.py:L132-L142] [src/digitalmodel/seismic/response_spectrum/workflow.py:L61-L67] |

The integration implementation names SciPy cumulative_trapezoid rather than a governing design standard, and explicitly excludes Newmark EoM integration. [src/digitalmodel/seismic/response_spectrum/calculations.py:L1-L5] [src/digitalmodel/seismic/response_spectrum/calculations.py:L43-L48]

No standards-derived coefficient is introduced by this pack; use the implementation references above for existing numerical conventions. Do not interpret kinematic outputs as elastic or inelastic spectra. [src/digitalmodel/seismic/response_spectrum/calculations.py:L1-L5]

UNVERIFIED: an externally published engineering benchmark and any governing standard edition for this preprocessing route remain unqualified; this pack asserts no standards compliance (section 10, #1182).

## 4. Validated worked examples

Validated examples: 0; the existing expected outputs below are treated as regressions because the tests do not cite an independent published benchmark for this route. [tests/seismic/test_response_spectrum_workflow.py:L15-L41] [tests/workflows/test_durable_workflows.py:L579-L604]

Results recorded on the uncommitted documentation tree against the pinned code; the BASE results were obtained before edits. Commands use `PYTHONPATH=src uv run --no-sources python -m pytest <nodes> -q -rA -p no:cacheprovider --basetemp=<scratch>`, without overriding pytest addopts. The durable case runs in temporary copies to contain generated outputs.

| Regression case | Input and expected result | Tolerance/basis | Existing test node ID | BASE / after |
| --- | --- | --- | --- | --- |
| Kinematic quadrature | Synthetic t=[0,1,2] s, a=[0,1,1] m/s²; baseline none, filter disabled; v=[0,0.5,1.5] m/s, d=[0,0.25,1.25] m | Test uses NumPy assert_allclose; C2 also checks discrete hand arithmetic with exact binary-representable targets | `tests/seismic/test_response_spectrum_workflow.py::test_compute_kinematics_keeps_eom_solver_out_of_pr1_scope` | PASS / PASS [tests/seismic/test_response_spectrum_workflow.py:L15-L41] |
| Provenance rejection | Synthetic record, no DataSource; render raises ProvenanceError | Exactly 1 expected exception | `tests/seismic/test_response_spectrum_workflow.py::test_report_requires_declared_data_source` | PASS / PASS [tests/seismic/test_response_spectrum_workflow.py:L44-L52] |
| Router round trip | Synthetic ASCII; CSV velocity above; HTML provenance, inline script, no external plot CDN or temporary directory | pytest.approx for CSV; exact string presence/absence | `tests/seismic/test_response_spectrum_workflow.py::test_router_writes_timeseries_and_provenance_report` | PASS / PASS [tests/seismic/test_response_spectrum_workflow.py:L55-L92] |
| Engine dispatch | Synthetic relative input record; raw PGA=1 m/s²; CSV and report exist | pytest.approx and exact file existence | `tests/seismic/test_response_spectrum_workflow.py::test_engine_dispatches_registered_response_spectrum_workflow` | PASS / PASS [tests/seismic/test_response_spectrum_workflow.py:L95-L127] |
| Registered durable example | Registry input; 1560 samples, dt=0.02 s, raw PGA=0.31882 g; velocity/displacement columns and provenance HTML | PGA relative tolerance 1e-4; remaining asserts as shipped; golden regression, not independent validation | `tests/workflows/test_durable_workflows.py::test_workflow_registry[response-spectrum]` | PASS / PASS [docs/registry/workflows.yaml:L354-L363] [tests/workflows/test_durable_workflows.py:L579-L604] |

The discrete hand check uses v1=(0+1)/2=0.5 m/s, v2=0.5+(1+1)/2=1.5 m/s, d1=(0+0.5)/2=0.25 m and d2=0.25+(0.5+1.5)/2=1.25 m for unit sample intervals; these are the test's discrete quadrature targets, not a continuous-time displacement benchmark. [tests/seismic/test_response_spectrum_workflow.py:L15-L41]

## 5. QA gates

Gate applicability follows AQG r0, section 10, Python calculations; G5 follows section 9.

| Gate | Applicability | Module route and check IDs |
| --- | --- | --- |
| G0 | Applicable | ASCII-to-SI input qualification and explicit processing choices: C0 |
| G1 | Applicable | Import through the Python calculation, router and engine; direct tests C1 and registered workflow C4; no external program deck is written [tests/seismic/test_response_spectrum_workflow.py:L7-L12] [src/digitalmodel/seismic/response_spectrum/workflow.py:L16-L47] |
| G2 | Applicable | Discrete hand quadrature C2 and raw-record summary C3; these do not qualify seismic spectra [src/digitalmodel/seismic/response_spectrum/calculations.py:L43-L64] [src/digitalmodel/seismic/response_spectrum/calculations.py:L125-L132] |
| G3 | NOT_APPLICABLE | This route integrates a supplied record without solving oscillator dynamics [src/digitalmodel/seismic/response_spectrum/calculations.py:L1-L5] |
| G4 | NOT_APPLICABLE | Outputs are accelerogram CSV and HTML, with no diffraction mesh or hydrodynamic solve [src/digitalmodel/seismic/response_spectrum/workflow.py:L16-L47] |
| G5 | Applicable | Generated HTML provenance/content C5 and evidence completeness C6; quality signature remains a separate clearance action (AQG r0, sections 9 and 11) |

Checks below are reproducible self-check specifications; C0, C2, C3, C5 and C6 use independent inspection of the pinned fixture and retained test outputs, and C1/C4 run the shipped test nodes. Their records remain outside the repository.

| Check | Gate | Function, quantity and numeric pass criterion | Evidence / result |
| --- | --- | --- | --- |
| C0 | G0 | Qualify `_record` synthetic input: 3 finite samples; maximum interval error from 1 s =0 s; matching array lengths; declared units m_s2; exactly 0 invalid samples | Input qualification record; PASS [tests/seismic/test_response_spectrum_workflow.py:L15-L22] |
| C1 | G1 | All 4 direct response-spectrum workflow tests pass; 0 failed cases and 0 skipped cases (unit: tests) | BASE 4 PASS; after 4 PASS [tests/seismic/test_response_spectrum_workflow.py:L25-L127] |
| C2 | G2 | `compute_kinematics`, baseline none/filter disabled: maximum absolute error vs section-4 discrete hand velocities =0 m/s and displacements =0 m | Independent arithmetic record; PASS [tests/seismic/test_response_spectrum_workflow.py:L15-L41] |
| C3 | G2 | `_summary`: npts=3 samples, dt=1 s, duration=2 s and raw PGA=1 m/s²; errors =0 in the respective units; 0 PGV/PGD keys | Summary inspection record; PASS [src/digitalmodel/seismic/response_spectrum/calculations.py:L125-L132] [tests/seismic/test_response_spectrum_workflow.py:L32-L36] |
| C4 | G1 | Durable registered workflow: 1 passed test, 0 failed/skipped tests; 1560 CSV rows; dt error =0 s; PGA relative error <=1e-4 (dimensionless) | BASE 1 PASS in temporary copy; after 1 PASS [tests/workflows/test_durable_workflows.py:L579-L604] |
| C5 | G5 | `render_report`: 1 ProvenanceError for missing source; generated router HTML contains provenance and source basename, at least 1 inline script, 0 CDN references and 0 temporary-directory occurrences (unit: occurrences) | Shipped rejection/round-trip tests plus report inspection; PASS [tests/seismic/test_response_spectrum_workflow.py:L44-L92] |
| C6 | G5 | Evidence bundle: 6 SI CSV columns, 2 processing steps, 1 exact file-byte SHA-256 identity represented in HTML, 5 section-4 node results, and 0 unresolved pack citations (units: columns, steps, identities, cases, citations) | Evidence inventory; PASS [src/digitalmodel/seismic/response_spectrum/calculations.py:L49-L64] [src/digitalmodel/seismic/response_spectrum/workflow.py:L61-L67] [src/digitalmodel/seismic/readers/ascii.py:L112-L126] |

## 6. Known failure modes and pitfalls

sweep: 41 found

The sweep records 33 lexical candidate lines and 8 additional default/output sites from manual inspection, including benign conventions; it does not mean 41 confirmed defects. Each candidate below retains its disposition and detection gap. The package has no record-skipping `continue`, explicit `return None`, error-return empty container or cache-write implementation. [src/digitalmodel/seismic/response_spectrum/calculations.py:L24-L132] [src/digitalmodel/seismic/response_spectrum/render.py:L26-L154] [src/digitalmodel/seismic/response_spectrum/workflow.py:L13-L116]

Exact lexical command (case-sensitive); manual inspection additionally covers conditional empty provenance, falsey label fallback, assumed-default/high-confidence ledger entries, current-directory fallback, unconditional pass fields and CSV precision:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|5 ?%|int\(|float\(|\.get\(|\.setdefault\(|min\(|cache|units|initial=|or \{' src/digitalmodel/seismic/response_spectrum --glob '*.py'
```

All rows describe the pinned implementation; observations were recorded 2026-11-07. Draft 1 groups configuration qualification, draft 2 groups report/result provenance, and draft 3 groups output-boundary qualification.

| Site | Symptom or disposition | Detection / tracking | Evidence |
| --- | --- | --- | --- |
| S01 calculations:43 | Empty baseline mapping silently selects pre-event mean | no detection check exists; issue pending (1) | [src/digitalmodel/seismic/response_spectrum/calculations.py:L43-L45] |
| S02 calculations:44 | Missing/falsey filter becomes empty mapping, so filtering is bypassed | no detection check exists; issue pending (1) | [src/digitalmodel/seismic/response_spectrum/calculations.py:L44-L46] |
| S03 calculations:47 | Initial velocity fixed to zero; not a measured initial condition | C2 for discrete convention; other initial conditions #1182 | [src/digitalmodel/seismic/response_spectrum/calculations.py:L47-L47] |
| S04 calculations:48 | Initial displacement fixed to zero; drift is not an oscillator solution | C2 for discrete convention; engineering applicability #1182 | [src/digitalmodel/seismic/response_spectrum/calculations.py:L48-L48] |
| S05 calculations:71 | Absent method becomes pre-event mean; null stringifies to none and disables baseline without schema rejection | no detection check exists; issue pending (1) | [src/digitalmodel/seismic/response_spectrum/calculations.py:L71-L76] |
| S06 calculations:75 | int conversion can truncate numeric fractional sample counts; default window min(10,npts) differs from template's 20 | no detection check exists; issue pending (1) | [src/digitalmodel/seismic/response_spectrum/calculations.py:L75-L76] [src/digitalmodel/base_configs/modules/response_spectrum/response_spectrum.yml:L8-L10] |
| S07 calculations:84 | Polynomial order defaults to 1; int conversion can truncate fractional order | no detection check exists; issue pending (1) | [src/digitalmodel/seismic/response_spectrum/calculations.py:L83-L89] |
| S08 calculations:104 | Missing/empty band bypasses even when enabled; only literal False disables explicitly | no detection check exists for incomplete/string-Boolean config; issue pending (1) | [src/digitalmodel/seismic/response_spectrum/calculations.py:L104-L105] |
| S09 calculations:106 | Float conversion of band text has no locale-separator handling; malformed text raises, without a dedicated negative test | no detection check exists; issue pending (1) | [src/digitalmodel/seismic/response_spectrum/calculations.py:L106-L115] |
| S10 calculations:108 | Assumes dt is seconds, defines Nyquist as half sampling rate | C0 for synthetic SI contract; arbitrary direct-record qualification issue pending (1) | [src/digitalmodel/seismic/response_spectrum/calculations.py:L107-L108] |
| S11 calculations:109 | Requested upper cutoff silently clipped; both requested/effective bands retained | no detection check exists for clipping; issue pending (1) | [src/digitalmodel/seismic/response_spectrum/calculations.py:L109-L120] |
| S12 calculations:110 | Filter order int conversion/default 4 precedes upstream filtering | no detection check exists for fractional/order-boundary config; issue pending (1) | [src/digitalmodel/seismic/response_spectrum/calculations.py:L110-L115] |
| S13 calculations:121 | Repeats order int conversion in metadata; same truncation risk | no detection check exists; issue pending (1) | [src/digitalmodel/seismic/response_spectrum/calculations.py:L116-L122] |
| S14 calculations:129 | Float conversion is internal duration arithmetic, not external text parsing | C3; no parsing defect inferred | [src/digitalmodel/seismic/response_spectrum/calculations.py:L125-L132] |
| S15 workflow:19 | Falsey response-spectrum settings become empty mapping; required file lookup later raises | no detection check exists for schema rejection; issue pending (1) | [src/digitalmodel/seismic/response_spectrum/workflow.py:L19-L21] |
| S16 workflow:20 | Falsey record settings become empty mapping; no router schema guard | no detection check exists; issue pending (1) | [src/digitalmodel/seismic/response_spectrum/workflow.py:L20-L26] |
| S17 workflow:24 | Missing units delegate to reader metadata/default m/s²; record-label units and internal SI values differ for g input | no detection check exists for missing-unit ambiguity; issue pending (1) | [src/digitalmodel/seismic/response_spectrum/workflow.py:L24-L24] [src/digitalmodel/seismic/readers/ascii.py:L115-L126] |
| S18 workflow:25 | Missing dt delegates to reader; single-column records require dt metadata or argument | no detection check exists in package tests for this failure; issue pending (1) | [src/digitalmodel/seismic/response_spectrum/workflow.py:L25-L25] [src/digitalmodel/seismic/readers/ascii.py:L161-L180] |
| S19 workflow:29 | Missing baseline delegates to calculation fallback rather than requiring explicit processing choice | no detection check exists; issue pending (1) | [src/digitalmodel/seismic/response_spectrum/workflow.py:L27-L31] |
| S20 workflow:30 | Missing filter delegates to bypass fallback | no detection check exists; issue pending (1) | [src/digitalmodel/seismic/response_spectrum/workflow.py:L27-L31] |
| S21 workflow:41 | Missing generated label delegates to renderer fallback | no detection check exists for fallback label; issue pending (2) | [src/digitalmodel/seismic/response_spectrum/workflow.py:L37-L42] |
| S22 workflow:45 | Missing outputs mapping initialised; existing unsuitable value has no schema validation | C1 for normal missing mapping; no detection check exists for malformed value; issue pending (3) | [src/digitalmodel/seismic/response_spectrum/workflow.py:L43-L47] |
| S23 workflow:66 | Source description retains original unit label; numeric CSV uses SI conventions | C6 for identity and SI column labels; no detection check exists for unit-description consistency; issue pending (2) | [src/digitalmodel/seismic/response_spectrum/workflow.py:L61-L67] [src/digitalmodel/seismic/response_spectrum/calculations.py:L49-L57] |
| S24 workflow:74 | Catches relative-path ValueError and emits external basename/digest; no error swallowed from file reading | C5 for absence of temporary directory; no detection check exists for identifying basename; issue pending (3) | [src/digitalmodel/seismic/response_spectrum/workflow.py:L70-L75] |
| S25 workflow:84 | Output directory defaults to results; explicit null stringifies to None; directories are created and existing CSV may be overwritten | C1 for normal outputs; no detection check exists for collision policy; issue pending (3) | [src/digitalmodel/seismic/response_spectrum/workflow.py:L78-L87] |
| S26 workflow:98 | First available config-directory metadata wins | C1 tests configured file path; no detection check exists for competing metadata; issue pending (3) | [src/digitalmodel/seismic/response_spectrum/workflow.py:L97-L102] |
| S27 workflow:100 | Falls back from config directory to parent of config-file path | C1 for engine/file route; no schema qualification; issue pending (3) | [src/digitalmodel/seismic/response_spectrum/workflow.py:L97-L102] |
| S28 workflow:106 | Config-file stem overrides basename for output names | C1 for input stem; collision qualification issue pending (3) | [src/digitalmodel/seismic/response_spectrum/workflow.py:L105-L108] |
| S29 workflow:108 | Missing basename defaults output stem to response_spectrum | no detection check exists for default stem; issue pending (3) | [src/digitalmodel/seismic/response_spectrum/workflow.py:L105-L108] |
| S30 workflow:115 | Catches relative-path ValueError and returns absolute output path | no detection check exists for publication boundary; issue pending (3) | [src/digitalmodel/seismic/response_spectrum/workflow.py:L111-L116] |
| S31 render:82 | Missing processing step name renders empty cell | no detection check exists; issue pending (2) | [src/digitalmodel/seismic/response_spectrum/render.py:L79-L85] |
| S32 render:83 | Missing method name renders empty cell | no detection check exists; issue pending (2) | [src/digitalmodel/seismic/response_spectrum/render.py:L79-L85] |
| S33 render:137 | Ledger method lookup may yield None for malformed processing data | no detection check exists; issue pending (2) | [src/digitalmodel/seismic/response_spectrum/render.py:L132-L142] |
| S34 render:38 | Missing source builds empty list; report rejects it rather than silently inventing provenance | C5, expected ProvenanceError | [src/digitalmodel/seismic/response_spectrum/render.py:L38-L53] [tests/seismic/test_response_spectrum_workflow.py:L44-L52] |
| S35 render:39 | Empty/None label replaced with generated workflow label | no detection check exists; issue pending (2) | [src/digitalmodel/seismic/response_spectrum/render.py:L26-L39] |
| S36 render:138 | Explicit caller processing still tagged ASSUMED_DEFAULT | no detection check exists for provenance origin; issue pending (2) | [src/digitalmodel/seismic/response_spectrum/render.py:L132-L142] |
| S37 render:140 | Ledger confidence always HIGH independent of validation | no detection check exists; issue pending (2) | [src/digitalmodel/seismic/response_spectrum/render.py:L132-L142] |
| S38 workflow:102 | Missing config metadata resolves paths against current working directory | no detection check exists; issue pending (3) | [src/digitalmodel/seismic/response_spectrum/workflow.py:L97-L102] |
| S39 workflow:43 | Output success unconditionally labelled pass; not engineering acceptance | C1 checks current contract; engineering criterion absent, #1182 | [src/digitalmodel/seismic/response_spectrum/workflow.py:L36-L47] |
| S40 workflow:52 | Payload duplicates unconditional pass field | C1 for top-level field only; no detection check exists for payload acceptance, #1182 | [src/digitalmodel/seismic/response_spectrum/workflow.py:L50-L58] |
| S41 workflow:80 | CSV uses 10 significant digits, so serialization is not exact binary preservation | C1 round-trip fixture; no detection check exists for large-dynamic-range precision; issue pending (3) | [src/digitalmodel/seismic/response_spectrum/workflow.py:L78-L80] |

| Additional pitfall | Cause and consequence | Detection / tracking | Evidence |
| --- | --- | --- | --- |
| Summary and processed motion differ | Summary reads raw record, including PGA, while plotted acceleration is filtered | C3 only verifies raw convention; no processed-PGA check exists; #1182 | [src/digitalmodel/seismic/response_spectrum/calculations.py:L125-L132] [src/digitalmodel/seismic/response_spectrum/render.py:L97-L105] |
| Nested result remains mutable | Frozen result contains DataFrame/dicts/lists; audit leaves consistency unevaluated | no detection check exists; #2313 | [src/digitalmodel/seismic/response_spectrum/calculations.py:L24-L32] [docs/domains/provenance-array-audit.md:L48-L52] |
| Workflow name overstates delivered analysis if read literally | Kinematic integration is explicitly separate from Newmark/spectra; #1182 is capability backlog, not evidence of an implemented spectrum | C1 scope assertions; #1182 | [src/digitalmodel/seismic/response_spectrum/calculations.py:L1-L5] [tests/seismic/test_response_spectrum_workflow.py:L32-L41] |

The issue search identified #1182 as the capability reference and #2313 as the provenance-container audit reference; neither is asserted to be a confirmed production incident in this package. Detection gaps above are retained explicitly rather than reported as passed tests. [src/digitalmodel/seismic/response_spectrum/calculations.py:L1-L5] [docs/domains/provenance-array-audit.md:L3-L10]

## 7. Lessons log

Append future evidence-backed lessons without rewriting earlier rows.

| Date | Change reference | Lesson | Evidence | Pack change | Shared-log reference |
| --- | --- | --- | --- | --- | --- |
| 2026-11-07 | Initial pack revision 0 | Separate preprocessing success from oscillator/spectrum qualification; retain the absence of PGV/PGD scope claims | [tests/seismic/test_response_spectrum_workflow.py:L32-L41] | Sections 1, 4, 5 and 6 | Proposed; issue pending (6) |

## 8. Ownership

Proposed review responsibilities: engineering calculations quality manager; deputy: engineering calculations deputy quality manager; producer: calculation documentation contributor. Route ambiguous input conventions, tolerances, provenance and method qualification to that quality role, with unresolved cross-module policy questions referred to the maintainers (AQG r0, section 11).

UNVERIFIED: a named assignment of the quality and deputy roles and a shared-log entry have not been established by repository evidence; issue pending (6).

## 9. Dependencies and implementation inventory

| Dependency or consumer | Role | Evidence |
| --- | --- | --- |
| ASCII reader / AccelerogramRecord | Input parsing, SI conversion, source SHA-256, raw PGA | [src/digitalmodel/seismic/readers/ascii.py:L104-L126] [src/digitalmodel/seismic/readers/ascii.py:L183-L205] |
| seismic baseline helpers | Window-mean and polynomial subtraction | [src/digitalmodel/seismic/baseline.py:L8-L42] |
| NumPy, pandas, SciPy | Array/dataframe assembly and cumulative trapezoidal integration | [src/digitalmodel/seismic/response_spectrum/calculations.py:L35-L64] |
| FrequencyFilter | Butterworth bandpass applied with zero phase by default | [src/digitalmodel/signal_processing/signal_analysis/filters/frequency.py:L60-L70] [src/digitalmodel/signal_processing/signal_analysis/filters/frequency.py:L96-L125] [src/digitalmodel/signal_processing/signal_analysis/filters/frequency.py:L219-L250] |
| Reporting and assumption ledger; Plotly | HTML with declared source and processing methods; inline plots | [src/digitalmodel/seismic/response_spectrum/render.py:L29-L53] [src/digitalmodel/seismic/response_spectrum/render.py:L97-L142] |
| Engine / durable workflow | Dispatch and registered input/output contract; runtime offline | [src/digitalmodel/engine.py:L732-L737] [docs/registry/workflows.yaml:L354-L363] |

### Historical map inventory and acceptance

Map reference: `origin/codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; map data revision `270c8759c374b67a7f032953e7fcdb14e0224461`. The same data revision is recorded in the code pin's map header. [docs/registry/implementation-map.yaml:L1-L3]

Inventory result from the historical map: 0 families have an implementation path inside the header package, so the full required inventory is empty and the counted-family set is empty. A8 inventory PASS; counted-family grep NOT_APPLICABLE because no map family has an in-package row. This is a map-coverage observation, not proof that the methods have no other implementations.

Reproduce inventory by loading that map revision with YAML, then selecting each family's `implementations` rows whose `path` starts with `src/digitalmodel/seismic/response_spectrum/`. Rank qualifying families by descending in-package row count, preserving map order for ties; take at most 3. No family qualifies here, so no search signal is applied or excluded. The recorded inventory and map pins are self-check evidence; the map itself remains unchanged.

Map follow-up: add baseline correction, bandpass preprocessing, kinematic double integration, raw-record peak summary and report-provenance composition from section 3 before counting those families; issue pending (4). [src/digitalmodel/seismic/response_spectrum/calculations.py:L35-L132] [src/digitalmodel/seismic/response_spectrum/render.py:L29-L53]

### Whole-repository method discovery

The additional search includes every tracked Python file, including `scripts/`; its results are map follow-up evidence and do not change A8.

```sh
git grep -n -E 'cumulative_trapezoid|cumtrapz|def pre_event_mean|def polynomial_detrend|def bandpass_filter|kinematic_cumtrapz' <code-pin> -- '*.py'
```

| Located definition(s) | Classification and disposition | Evidence |
| --- | --- | --- |
| `compute_kinematics`, `_apply_baseline`, `_apply_filter`, `_summary` | Package route, adapters and summary already listed in section 3 | [src/digitalmodel/seismic/response_spectrum/calculations.py:L35-L132] |
| `pre_event_mean`, `polynomial_detrend`, `FrequencyFilter.bandpass_filter` | Listed upstream implementations delegated to by package | [src/digitalmodel/seismic/baseline.py:L8-L42] [src/digitalmodel/signal_processing/signal_analysis/filters/frequency.py:L219-L250] |
| signal-processing `integration_for_df` | Separate single/double trapezoidal time-series route; potential map gap, not a qualified reference for this package; issue pending (4) | [src/digitalmodel/signal_processing/time_series/time_series_components.py:L718-L738] |
| infrastructure `integration_for_df` | Separate legacy single/double trapezoidal route; potential map gap; issue pending (4) | [src/digitalmodel/infrastructure/utils/time_series_components.py:L409-L423] |
| signal-processing alternate `integration_for_df` | Separate single/double trapezoidal route; potential map gap; issue pending (4) | [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L494-L513] |
| Hull-girder `_cumtrapz`, OpenFOAM time-history `_cumulative_trapezoid` | Explicit quadrature implementations for different physical quantities; map follow-up candidates, not seismic validation references; issue pending (4) | [src/digitalmodel/naval_architecture/hull_girder_screening.py:L361-L368] [src/digitalmodel/solvers/openfoam/time_history.py:L602-L623] |
| BEM causality-check integrals | Frequency-domain integrations, different result from accelerogram integration; retain as search false positives | [src/digitalmodel/hydrodynamics/bemrosetta/validators/causality_checker.py:L143-L159] [src/digitalmodel/hydrodynamics/bemrosetta/cli.py:L405-L423] |

## 10. Coverage and open items

### Read coverage

| File or discovery set | Coverage / disposition | Evidence |
| --- | --- | --- |
| Package `__init__.py` | Complete; exported router | [src/digitalmodel/seismic/response_spectrum/__init__.py:L1-L5] |
| Package `calculations.py` | Complete; input choices, integration and summary | [src/digitalmodel/seismic/response_spectrum/calculations.py:L1-L132] |
| Package `workflow.py` | Complete; IO, path and result boundaries | [src/digitalmodel/seismic/response_spectrum/workflow.py:L1-L116] |
| Package `render.py` | Complete; report, plot, processing and ledger | [src/digitalmodel/seismic/response_spectrum/render.py:L1-L154] |
| `tests/seismic/test_response_spectrum_workflow.py` | All 4 cases in section 4; no package-local test subtree | [tests/seismic/test_response_spectrum_workflow.py:L1-L127] |
| Durable workflow test | Registry loader, output assertions and response-spectrum branch read; selected existing node in section 4 | [tests/workflows/test_durable_workflows.py:L23-L29] [tests/workflows/test_durable_workflows.py:L50-L105] [tests/workflows/test_durable_workflows.py:L579-L604] |
| Base configuration and example input | Baseline/filter defaults and example route; 1 discovery file with an identifying name is omitted from this inventory; issue pending (5) | [src/digitalmodel/base_configs/modules/response_spectrum/response_spectrum.yml:L1-L22] [examples/workflows/response-spectrum/input.yml:L1-L21] |
| Example data README | Fixture statistics and deferred validation read; source claims remain unqualified by this pack | [examples/workflows/response-spectrum/data/README.md:L19-L30] |
| Module routing / operator map / workflow registry | Existing seismic entry, scope and offline registry row read | [docs/registry/module-routing.yaml:L428-L437] [docs/maps/digitalmodel-operator-map.md:L65-L65] [docs/registry/workflows.yaml:L354-L363] |
| Provenance audit | Result mutability marked unevaluated | [docs/domains/provenance-array-audit.md:L48-L52] |
| Workflow failure-evidence audit | Names registered response-spectrum row; does not replace package negative tests | [docs/audits/workflow-failure-evidence-coverage.html:L87-L87] |
| Implementation map and two agent-pack function-discovery JSON files | Historical map used in section 9; lexical `compute_response_spectrum` matches in the JSON files concern other domains, not this package | [docs/registry/implementation-map.yaml:L14344-L14351] [docs/registry/agent-pack-audit/production-nde-functions-1.json:L6432-L6438] [docs/registry/agent-pack-audit/production-nde-functions-1.json:L8416-L8422] [docs/registry/agent-pack-audit/production-nde-functions-2.json:L3856-L3862] [docs/registry/agent-pack-audit/production-nde-functions-2.json:L3922-L3928] |
| Motion-forecast plan | Lexical response-spectrum matches describe phase-blind marine RAO statistics, not this accelerogram workflow | [docs/plans/2026-07-03-issue-1358-motion-forecast-keystone.md:L12-L23] |

A module-name search covered Markdown, HTML, YAML and Python references; no package README or package documentation existed at the pin. The four source files and the references above define the bounded coverage, rather than the inventory's approximate test count. [src/digitalmodel/seismic/response_spectrum/__init__.py:L1-L5] [tests/seismic/test_response_spectrum_workflow.py:L25-L127]

### Open items

Each item is proposed to the engineering calculations quality role for disposition; issue numbers are retained when already present, otherwise draft references await issue creation before final clearance.

| Item | Gap or qualification limit | Tracking |
| --- | --- | --- |
| O1 | Section-2 UNVERIFIED axis, origin and polarity; strict settings schema, numeric conversion, baseline defaults, filter enable/band/order/clipping and SI preflight negative tests; sweep S01–S20 | issue pending (1) |
| O2 | Section-3 UNVERIFIED external engineering benchmark/standard qualification; 0 validated examples; this route supplies no SDOF spectra, intensity measures or engineering acceptance criterion | #1182 |
| O3 | Report ledger origin/confidence, missing processing metadata and result mutation consistency | #2313; issue pending (2) |
| O4 | Output-directory/stem/collision/schema qualification, external basename disclosure, absolute returned paths and CSV precision; sweep boundary gaps | issue pending (3) |
| O5 | Historical map has no in-package rows; add section-3 families and evaluate section-9 alternative integration implementations | issue pending (4) |
| O6 | Codename the 1 identifying discovery filename before citing it in the public pack | issue pending (5) |
| O8 | Section-8 UNVERIFIED quality/deputy assignment and shared lesson evidence | issue pending (6) |
| O7 | UNVERIFIED: the fixture README's external source and redistribution statements were not independently checked; regression status retained | #1182 |

No private sidecar entry is introduced: the pack uses the synthetic tests and public repository fixture statistics, without client-derived engineering values. [tests/seismic/test_response_spectrum_workflow.py:L15-L22] [examples/workflows/response-spectrum/data/README.md:L19-L30]
