# Results analysis knowledge pack

| Header | Value |
| --- | --- |
| Module | `solvers/orcaflex/results_analysis` |
| Repository | `digitalmodel` |
| Package path | `src/digitalmodel/solvers/orcaflex/results_analysis` |
| Pack revision | 0, 2026-10-15 |
| Code revision | `d96aad6b05f0da8b01be3cc490277bf8080bf663` |
| Analysis family | OrcaFlex riser and mooring; QA family `MR` |
| Family quality manager | OrcaFlex riser/mooring quality manager |
| Status | DRAFT; independent clearance pending |

## 1. Purpose and scope

The public library parses CSV exports, classifies columns, decodes loading conditions, validates DataFrames and provides statistical, comparative, sensitivity and anomaly analyses; file monitoring is a separate asynchronous service. [src/digitalmodel/solvers/orcaflex/results_analysis/__init__.py:L1-L30] [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L143-L160]

The main path covered here is CSV → column metadata/polar values → classification and validation → descriptive statistics; the analytical services also accept DataFrames directly. [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L281-L347] [src/digitalmodel/solvers/orcaflex/results_analysis/component_classifier.py:L332-L355] [src/digitalmodel/solvers/orcaflex/results_analysis/data_validator.py:L147-L174] [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L66-L81]

The routing registry marks the enclosing `solvers` domain beta; this is a maturity label, not an engineering validation result. [docs/registry/module-routing.yaml:L279-L288]

File recognition uses filename and header heuristics, not a vendor schema or a solver model; the parser returns exported data rather than calculating statics or dynamics. [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L96-L144] [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L300-L347]

UNVERIFIED: supported solver versions, geometry/water-depth/load envelopes and suitability for design acceptance have not been established; see U1 in section 10.

## 2. Inputs and outputs

### Inputs

| Input | Unit and range | Default | Source and evidence |
| --- | --- | --- | --- |
| `CSVParser.parse_file(file_path, **kwargs)` | CSV text; existing file required; column labels retained | UTF-8, Latin-1 fallback; blank lines skipped; pandas options overridable | Caller-supplied export; NA tokens are configured explicitly. [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L281-L319] |
| `CSVParser(chunk_size)` / `parse_chunked` | Rows per chunk; constructor does not enforce a positive range | 10000 rows | Chunked iterator has its own reader and metadata. [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L86-L94] [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L353-L412] |
| `PolarData` | Heading degrees; extraction admits `0 <= heading < 360`; at least 12 usable headings per parameter | Validation reference is 24 headings, 0 through 345 degrees in 15 degree increments | Last non-NaN value in each heading column, sorted by heading. [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L44-L49] [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L64-L65] [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L214-L272] |
| `DataValidator.validate_analysis_data` | DataFrame; optional classified components; score dimensionless | `STANDARD`; `ValidationResult.is_valid` threshold 0.7 | Caller data, component consistency only at strict level. [src/digitalmodel/solvers/orcaflex/results_analysis/data_validator.py:L57-L71] [src/digitalmodel/solvers/orcaflex/results_analysis/data_validator.py:L130-L149] [src/digitalmodel/solvers/orcaflex/results_analysis/data_validator.py:L170-L225] |
| `LoadingDecoder` | Filename, parent directory and optional DataFrame; unknown categories allowed | Environmental fields `None`; confidence 0; data-only phase assumes connected | Wave height m, period s, wind/current m/s, directions degrees. [src/digitalmodel/solvers/orcaflex/results_analysis/loading_decoder.py:L53-L78] [src/digitalmodel/solvers/orcaflex/results_analysis/loading_decoder.py:L169-L187] [src/digitalmodel/solvers/orcaflex/results_analysis/loading_decoder.py:L312-L328] |
| Statistical inputs | Numeric DataFrame/arrays; regression requires equal lengths and at least 3 entries before NaN filtering | Correlation Pearson; confidence level 0.95; linear trend | No dimensional conversion is performed in these calculations. [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L66-L115] [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L131-L172] [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L278-L323] |
| Comparison and sensitivity inputs | Case-keyed DataFrames and environmental dictionaries; parameter ranges carry optional unit strings | Comparison `auto`, confidence 0.95; sensitivity `correlation`, 1000 samples | Callers supply response functions for surfaces/Monte Carlo. [src/digitalmodel/solvers/orcaflex/results_analysis/comparative_analysis.py:L74-L118] [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L41-L79] [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L253-L260] [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L360-L366] |
| Monitor configuration | Directory path, patterns, async callback; file sizes bytes; time seconds/hours | Minimum 1024 bytes; stable time 2.0 s; polling 1.0 s; age 24 h; maximum tracked 10000; checksums enabled | Optional watchdog observer or polling branch. [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L80-L107] [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L157-L191] [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L222-L243] |

### Outputs

| Output | Units / representation | Destination and evidence |
| --- | --- | --- |
| Parser dictionary | Original DataFrame, enum file type, metadata, shape; memory in MiB despite key `memory_usage_mb`; optional polar data and counts | Returned to caller; no unit conversion in the return construction. [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L316-L347] |
| `ParsedColumn`, `PolarData` | Unit strings are metadata; polar values retain export magnitudes | Metadata recognition checks regex groups in order. [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L33-L49] [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L156-L188] [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L251-L272] |
| `ComponentInfo` list | Component/parameter enums and dimensionless confidence | Grouped by component type; unknown groups below 3 columns omitted; sorted by confidence. [src/digitalmodel/solvers/orcaflex/results_analysis/component_classifier.py:L332-L407] |
| `ValidationResult` | Dimensionless score; category scores, issues, checks, timing seconds and missing-data percentage | Weighted penalties follow raw passed/total checks; any error prevents `is_valid`. [src/digitalmodel/solvers/orcaflex/results_analysis/data_validator.py:L57-L71] [src/digitalmodel/solvers/orcaflex/results_analysis/data_validator.py:L220-L260] |
| Time-series statistics | Mean/min/max/RMS/range in input units; standard deviation in input units; time-weighted RMS uses the supplied time coordinate | Numeric non-time columns only; average time spacing is computed but not resampled. [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L490-L545] |
| Statistical results | Slope response/input; intercept/residuals in response units; dimensionless correlation, p-value and R-squared | Arrays/dataclasses/dictionaries returned directly. [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L35-L57] [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L159-L214] |
| Anomaly results | Value in input units except multivariate value is a score; index, severity, threshold and recommendations | Returns data containers; exact column names select engineering limits. [src/digitalmodel/solvers/orcaflex/results_analysis/anomaly_detection.py:L47-L68] [src/digitalmodel/solvers/orcaflex/results_analysis/anomaly_detection.py:L295-L371] [src/digitalmodel/solvers/orcaflex/results_analysis/anomaly_detection.py:L436-L466] |
| Monitor events | Timestamp, path, optional byte size and checksum; status/error counters | Callback and in-memory records; checksum implementation is MD5, not the SHA-256 input fingerprint required by AQG. [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L53-L78] [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L387-L418] [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L458-L472] |

UNVERIFIED: export coordinate frame, origin, positive directions, phase reference and an enforced file unit system are not established by this package; treat headers as metadata and obtain these conventions upstream; see U1. [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L146-L200] [src/digitalmodel/solvers/orcaflex/results_analysis/loading_decoder.py:L293-L305]

## 3. Method and governing standards

The entries below describe implementation rather than approval against an engineering standard; no licensed clause text was consulted.

| Method | Definition and implementing route | Reference basis / limit |
| --- | --- | --- |
| CSV recognition | `identify_file_type` checks `dm_`, inputs and trace/time names before header inspection; structural polar recognition needs at least 20 matching columns | Repository algorithm, not a schema guarantee. [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L96-L144] |
| Metadata and polar extraction | `parse_column_metadata` takes first matching unit/component/parameter regex; `extract_polar_data` sorts headings and keeps last valid row per heading | Repository algorithm; 12-point extraction threshold differs from full 24-point validation. [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L156-L200] [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L214-L279] |
| Polar validation | `validate_polar_data`: completeness = expected/actual heading-set intersection divided by expected count, actual headings rounded; missing headings and nonfinite values invalidate | DataValidator instead warns below 0.8 completeness and checks interior gaps above 45 degrees. [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L430-L478] [src/digitalmodel/solvers/orcaflex/results_analysis/data_validator.py:L558-L596] |
| Descriptive statistics | `extract_time_series_stats`: RMS = sqrt(mean(value squared)); `time_rms` = sqrt(trapezoidal integral(value squared)/duration); population standard deviation | Closed-form constant and sine references are separated from regression cases in section 4. [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L512-L545] |
| Classification | `classify_column` combines regex/unit/data heuristics; overall confidence is the minimum of component and parameter confidences | Confidence is a heuristic score, not a calibrated probability. [src/digitalmodel/solvers/orcaflex/results_analysis/component_classifier.py:L220-L235] [src/digitalmodel/solvers/orcaflex/results_analysis/component_classifier.py:L237-L330] |
| Data quality | `validate_analysis_data` selects checks by level, combines passed/total count and subtracts penalties for errors/warnings; `is_valid` also rejects any error | Hard-coded limits and name/unit heuristics are screening rules, not certified allowable loads. [src/digitalmodel/solvers/orcaflex/results_analysis/data_validator.py:L170-L225] [src/digitalmodel/solvers/orcaflex/results_analysis/data_validator.py:L67-L71] [src/digitalmodel/solvers/orcaflex/results_analysis/data_validator.py:L356-L438] |
| Loading decoding | `decode_from_filename`, `decode_from_data`, `_combine_conditions` merge pattern-derived categories and inferred environmental values | Directions assigned by encounter order; filename values use truthiness in combination, including zero. [src/digitalmodel/solvers/orcaflex/results_analysis/loading_decoder.py:L169-L258] [src/digitalmodel/solvers/orcaflex/results_analysis/loading_decoder.py:L293-L305] [src/digitalmodel/solvers/orcaflex/results_analysis/loading_decoder.py:L468-L509] |
| Correlation/regression | `calculate_correlation_matrix` calls Pearson or Spearman; `perform_linear_regression` fits y = slope*x + intercept, uses mean squared residual and Student-t intervals | Zero residual variance is a known failing path; do not use its confidence outputs as an independent reference. [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L100-L125] [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L159-L214] |
| Mean interval, trend, distribution | `calculate_confidence_interval` uses sample standard deviation with normal critical value above 30 observations and Student-t otherwise; trend implements linear/moving-average branches; distribution calls Shapiro, fitted-normal KS and Jarque–Bera | Polynomial is named in the trend docstring but rejected by the implemented selection branches; normality recommendations use a vote. [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L244-L271] [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L278-L369] [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L387-L473] |
| Comparative analysis | Pairwise t-test or Mann–Whitney, group ANOVA or Kruskal; `auto` group choice is nonparametric; effect sizes and benchmark percentage differences computed locally | Constant identical groups yield NaN statistics; warnings do not turn that into equivalence evidence. [src/digitalmodel/solvers/orcaflex/results_analysis/comparative_analysis.py:L364-L482] [src/digitalmodel/solvers/orcaflex/results_analysis/comparative_analysis.py:L520-L546] [src/digitalmodel/solvers/orcaflex/results_analysis/comparative_analysis.py:L320-L350] |
| Sensitivity | Correlation, standardized regression, random-forest feature importance and normalized response-range branches; grid surfaces evaluate caller function; Monte Carlo evaluates callbacks on four threads | Enum names Sobol/Morris are not accepted by the parameter-analysis selection; `_calculate_sobol_index` is a median-split approximation; random-forest/OAT intervals and p-values include placeholders. [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L31-L38] [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L118-L135] [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L253-L358] [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L391-L408] [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L604-L813] [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L863-L901] |
| Anomaly detection | IQR fences; population Z-score; modified Z-score from median absolute deviation; scaled isolation forest/elliptic envelope; centered rolling Z-score; exact-column engineering bounds | Comprehensive scan catches multivariate/time-series errors into result dictionaries; a returned summary need not mean all methods succeeded. [src/digitalmodel/solvers/orcaflex/results_analysis/anomaly_detection.py:L175-L238] [src/digitalmodel/solvers/orcaflex/results_analysis/anomaly_detection.py:L295-L371] [src/digitalmodel/solvers/orcaflex/results_analysis/anomaly_detection.py:L399-L466] [src/digitalmodel/solvers/orcaflex/results_analysis/anomaly_detection.py:L494-L552] [src/digitalmodel/solvers/orcaflex/results_analysis/anomaly_detection.py:L787-L805] |
| Monitoring | `start` launches consumer, scans existing files, starts observer/poller then sets running flag; files require size stability and optional checksum change before callback | Event-loop/thread defects are tracked in section 6; MD5 change detection is not input provenance. [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L203-L251] [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L333-L418] [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L437-L472] |

UNVERIFIED: provenance and engineering applicability of the hard-coded screening bounds, inferred environmental ranges and response-change coefficients are not established by a named standard or independent dataset; do not treat them as design allowables; see U2. [src/digitalmodel/solvers/orcaflex/results_analysis/data_validator.py:L88-L124] [src/digitalmodel/solvers/orcaflex/results_analysis/loading_decoder.py:L379-L433] [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L940-L963]

For future standards-derived calculations, require a calculation-citation sidecar; do not attach a standard attribution to the hard-coded rules without checking its source. The evidence gap belongs to U2 in section 10.

## 4. Validated worked examples

Offline evidence at the header pin: `PYTHONPATH=src uv run --no-sources python -m pytest tests/solvers/orcaflex/results_analysis -q -rA -p no:cacheprovider --basetemp=<scratch>`; no configuration override, test deselection or solver invocation; BASE collected 689 nodes, 688 PASS and 1 FAIL, 0 skipped, 0 NOT_RUN; AFTER is identical at the same pin on the uncommitted documentation tree. Results below refer to the individual existing test nodes, rather than the suite's exit code.

Use `tests/solvers/orcaflex/results_analysis/` as the prefix for every node ID below; all inputs are inline synthetic fixtures, with no external input file. [tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L775-L786] [tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L393-L400] [tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L635-L645] [tests/solvers/orcaflex/results_analysis/test_statistical_analysis.py:L222-L229] [tests/solvers/orcaflex/results_analysis/test_statistical_analysis.py:L295-L301]

| Case | Kind and independent basis | Inputs / expected value / tolerance basis | Existing test node ID | BASE / AFTER |
| --- | --- | --- | --- | --- |
| V1 constant RMS | Validated against closed form: RMS(c) = abs(c) | Three samples of 3.0, dimensionless; expected 3.0; test uses `pytest.approx(3.0)`; additional audit bound: absolute error <= 1e-8 (unit 1) | `test_csv_parser.py::TestExtractTimeSeriesStats::test_rms_calculation` | PASS / PASS. [tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L775-L779] |
| V2 sine RMS | Validated against closed form: integral of sin(t)^2 over one period divided by period = 1/2 | 1000 samples over 0 to 2*pi; expected 1/sqrt(2), dimensionless; absolute tolerance 0.02 accommodates discrete sampling | `test_csv_parser.py::TestExtractTimeSeriesStats::test_rms_sine_wave` | PASS / PASS. [tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L781-L786] |
| V3 exact straight line | Validated against closed form y = 3*x + 2, independent of the fit implementation | x = 1 through 5; expected slope 3.0, intercept 2.0, R-squared 1.0, dimensionless synthetic units; absolute errors < 1e-8, as asserted | `test_statistical_analysis.py::TestLinearRegression::test_perfect_fit` | PASS / PASS; validates fitted coefficients, not uncertainty estimates. [tests/solvers/orcaflex/results_analysis/test_statistical_analysis.py:L222-L229] |
| R1 final polar row | Regression of selection semantics, not solver validation | 24 columns, rows 10.0/20.0/30.0; output exactly 30.0 in each column, dimensionless synthetic units; 0 unequal elements | `test_csv_parser.py::TestExtractPolarData::test_values_are_last_row` | PASS / PASS. [tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L393-L400] |
| R2 incomplete polar coverage | Regression of configured heading grid | 12 of 24 headings; invalid, completeness exactly 0.5 dimensionless; 0 incorrect flags | `test_csv_parser.py::TestValidatePolarData::test_missing_headings_flagged` | PASS / PASS. [tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L664-L672] |
| R3 chunked encoding failure | Regression preserving an observed limitation | Synthetic Latin-1 bytes; expects `UnicodeDecodeError`, exactly 1 caught exception; no engineering value | `test_csv_parser.py::TestParseChunked::test_latin1_chunked_raises_unicode_error` | PASS / PASS; passing means the defect is reproduced. [tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L635-L645] |
| R4 NaN regression | Regression of row filtering; currently fails before the length assertion | Eight pairs with one NaN; expected 7 predictions, tolerance 0 rows | `test_statistical_analysis.py::TestLinearRegression::test_nan_values_filtered` | FAIL / FAIL; `RuntimeWarning: divide by zero encountered in scalar divide` at `statistical_analysis.py:184`; issue pending (1). [tests/solvers/orcaflex/results_analysis/test_statistical_analysis.py:L295-L301] [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L175-L185] |

The existing time-series test class contains a conditional NumPy compatibility fixture; the production dependency requires NumPy below 2.0, and this run used the installed dependency set without modifying tests. [tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L741-L760] [pyproject.toml:L57-L61]

## 5. QA gates

Acceptance criteria below are proposed executable checks, not claims of solver clearance. AQG r0, sections 3–10 and 13, defines G0–G5 and the riser/mooring family set. Synthetic CSV tests cannot establish the corresponding source-model gate records.

| ID / gate | Module-specific check and numeric pass criterion with unit | Evidence and present result |
| --- | --- | --- |
| C01 / G0 applicable | Run R2 and inspect `CSVParser.validate_polar_data`; for the half-grid case require completeness = 0.5 (unit 1), invalid = true and 0 missing-heading detection errors | R2 PASS; checker recognizes the missing headings. [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L442-L453] [tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L664-L672] |
| C02 / G0 applicable | Hand-check `CSVParser.parse_column_metadata("Velocity m/s")`; require 0 unit-token mismatches against the literal `m/s` (count), without interpreting `m` as velocity | FAIL by source and existing regression: returns `m`; issue pending (2). The shipped test passes because it encodes this limitation. [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L68-L74] [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L156-L162] [tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L262-L267] |
| C03 / G0 applicable | Inspect `FileMonitor.start` / `_process_events`: require 0 consumer launches before the running flag becomes true and 0 observer-thread `create_task` calls without captured-loop handoff (count) | FAIL by source inspection; #1925; no executable monitor detection test exists. [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L222-L243] [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L280-L311] |
| C04 / G1 applicable | Upstream OrcaFlex source-model load: an authorized solver operator uses `Model.LoadData` in the target version, records 0 load warnings/errors (count), and compares `ComponentClassifier.validate_classification` against the export manifest with coverage = 1.0 (unit 1), 0 missing expected components (count) | NOT_RUN: requires licensed source model, target version and manifest; none supplied. This library writes no solver input deck on the documented parser path. AQG r0 §5, G1.1–G1.3; [src/digitalmodel/solvers/orcaflex/results_analysis/component_classifier.py:L450-L485] [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L321-L347] |
| C05 / G2 applicable | Run V1–V3; `extract_time_series_stats` constant RMS absolute error <= 1e-8, sine RMS absolute error <= 0.02 (both unit 1); `perform_linear_regression` slope/intercept/R-squared absolute errors < 1e-8 (unit 1) | PASS for the synthetic references only; not a static line equilibrium check. [tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L775-L786] [tests/solvers/orcaflex/results_analysis/test_statistical_analysis.py:L222-L229] |
| C06 / G2 applicable | For every upstream static line response exported through `CSVParser.parse_file`, compare top tension against an independent catenary reference: abs(Texport-Tref)/abs(Tref)*100 <= 5.0 %, 0 unmatched component rows (count); use absolute tolerance from the design basis if Tref = 0 | NOT_RUN: source statics and independent reference absent; do not substitute this package's regression or heuristic sensitivity for catenary. AQG r0 §6, G2.3–G2.6; [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L321-L347] [src/digitalmodel/solvers/orcaflex/results_analysis/component_classifier.py:L450-L485] |
| C07 / G3 applicable | After the upstream 1.0 s short run, require 0 nonfinite exported values (count); pass `EngineeringLimits` for each tension channel to `detect_engineering_limit_violations` with bounds 0.75*Tstatic to 1.25*Tstatic (same force unit), require 0 violations and 0 warnings/errors in the upstream log (count) | NOT_RUN: requires a licensed dynamic run and static basis. Exact column-name matching is required; `limits_checked` alone does not prove each column was checked. AQG r0 §7; [src/digitalmodel/solvers/orcaflex/results_analysis/anomaly_detection.py:L295-L337] [src/digitalmodel/solvers/orcaflex/results_analysis/anomaly_detection.py:L362-L371] |
| C08 / G4 conditional | Where vessel data come from new diffraction, require 1 current upstream G4 record per imported body (count) and 0 fingerprint/version mismatches against the data feeding `LoadingDecoder` / parsed response set (count) | NOT_APPLICABLE to these synthetic fixtures, which contain no new diffraction result; no hydrodynamic validation is inferred from decoded headings. For real imported data, a cited upstream record is required. AQG r0 §8 and §10; [src/digitalmodel/solvers/orcaflex/results_analysis/loading_decoder.py:L435-L464] |
| C09 / G5 applicable | Compare the dictionary returned by `generate_statistical_report` with an external QA appendix manifest: 0 unlinked result quantities (count), 1 fingerprint per source set (count), 0 unsigned applicable gates (count) | NOT_RUN: report dictionary contains analysis details, not a generated AQG evidence appendix; adapter/records missing, issue pending (4). AQG r0 §9 and §13; [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L490-L520] |
| C10 / G2 applicable | Run R4; require 7 predictions and 0 uncaught numerical warnings (count) from `perform_linear_regression` | FAIL at BASE, issue pending (1); failed node must stay visible. [tests/solvers/orcaflex/results_analysis/test_statistical_analysis.py:L295-L301] [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L175-L185] |

Defined checks by gate: G0 3; G1 1; G2 3; G3 1; G4 1 conditional; G5 1. The unavailable solver/evidence checks are specified for later execution, not run or signed here. AQG r0, sections 3–10 and 13.

## 6. Known failure modes and pitfalls

| Symptom (silent paths first) | Cause and detection | Incident / disposition |
| --- | --- | --- |
| Monitor reports running after consumer has exited; observer events fail off-loop | `start` schedules consumer while `is_running` is false, yields during initial scan, then sets true; callback uses `asyncio.create_task` directly; C03 detects ordering, no executable detection check exists | #1925, open, read 2026-10-15; confirmed structurally, no runtime reproduction made. [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L222-L243] [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L280-L311] [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L474-L485] |
| Monte Carlo sensitivities may pair responses with different parameters | Successful responses append in `as_completed` order, while parameters use the original prefix; failed callbacks are removed without retaining indices; no detection check exists in the current suite for paired ordering | issue pending (3); proposed check: y=x with deliberately reversed completion and partial failure, require 0 pair mismatches and correlation 1.0 within 1e-12 (unit 1). Existing tests check structure/rank presence, not pair identity. [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L391-L435] [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L845-L861] [tests/solvers/orcaflex/results_analysis/test_sensitivity_analysis.py:L923-L976] |
| Velocity/acceleration units become displacement metadata; classifier can override semantic matches | Parser displacement regex precedes velocity/acceleration; classifier uses substring units and broad data heuristics; C02 | issue pending (2); these are metadata limitations, not conversion. [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L68-L74] [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L156-L162] [src/digitalmodel/solvers/orcaflex/results_analysis/component_classifier.py:L280-L325] [tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L262-L267] [tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L361-L365] |
| Zero filename direction/speed is replaced by data-derived value | `_combine_conditions` uses `or` instead of explicit missing-value selection; no detection check exists for zero-preservation | issue pending (6); proposed check: filename wave direction 0 degrees and data direction 90 degrees must produce 0 degrees, absolute error 0 degrees. [src/digitalmodel/solvers/orcaflex/results_analysis/loading_decoder.py:L487-L500] |
| Missing/insufficient metrics disappear from summaries; errors may be nested rather than raised | Comparison skips absent metrics; comprehensive anomaly scan catches selected method failures; recommendations are not gate signatures | Treat 0 unchecked expected metrics as a requirement; coverage follow-up issue pending (4). [src/digitalmodel/solvers/orcaflex/results_analysis/comparative_analysis.py:L100-L115] [src/digitalmodel/solvers/orcaflex/results_analysis/anomaly_detection.py:L787-L805] |
| Summary polar values are mistaken for envelope maxima | Extraction keeps the last non-NaN sample per heading, not max/min over the time history; R1 detects the selection | Regression limitation, coverage follow-up issue pending (4). [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L251-L258] [tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L393-L400] |
| Latin-1 fallback works for whole-file parse but fails during chunk iteration | The chunked try/except covers reader creation, not decode errors raised later by the iterator; R3 reproduces the limitation | issue pending (2). [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L308-L314] [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L380-L412] [tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L635-L645] |
| Perfect-fit regression raises a numerical warning | Residual standard error becomes zero; `t_stat = slope / slope_se` has no degeneracy branch; C10/R4 | issue pending (1); BASE failure, not introduced by this pack; warning-as-error remains enabled. [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L175-L185] [tests/solvers/orcaflex/results_analysis/test_statistical_analysis.py:L295-L301] [pytest.ini:L68-L71] |
| Hard-coded rules are used as independent references | Load-case response change uses a coefficient table; random-forest/OAT intervals and p-values include placeholders; generic bounds lack a cited design basis | U2, issue pending (4); independent calibration required before engineering acceptance. [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L715-L724] [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L781-L799] [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L940-L963] [src/digitalmodel/solvers/orcaflex/results_analysis/data_validator.py:L88-L124] |

## 7. Lessons log

| Date | Change reference | Lesson | Evidence | Pack change | Shared-log reference |
| --- | --- | --- | --- | --- | --- |
| 2026-10-15 | Initial pack revision | A passing regression can encode a known defect: chunked Latin-1 parsing deliberately expects an exception | [tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L635-L645] | Separate R3 from independent validation and retain encoding limitation in section 6 | Pending maintainer registration; issue pending (4) |

## 8. Ownership

Assigned review role: OrcaFlex riser/mooring quality manager. Proposed deputy: another independent riser/mooring quality manager, subject to maintainer confirmation. Producing function: engineering documentation. Escalate method classification or acceptance ambiguity to the family quality manager; escalate unresolved coverage ownership to the maintainers. Role assignment and shared-log registration remain open under issue pending (4).

## 7. Lessons log

No clearance or shared-log entry is asserted in this draft.

## 8. Ownership

Assigned review role: OrcaFlex riser/mooring quality manager. Proposed deputy: another independent riser/mooring quality manager, subject to maintainer confirmation. Producing function: engineering documentation. Escalate method classification or acceptance ambiguity to the family quality manager; escalate unresolved coverage ownership to the maintainers.

## 9. Dependencies

### Runtime and solver boundary

| Dependency | Use / inventory relationship | Evidence |
| --- | --- | --- |
| NumPy and pandas | Arrays, DataFrames and CSV input; external libraries, outside the module inventory | [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L8-L16] |
| SciPy and scikit-learn | Statistics, regression, anomaly models and sensitivity; external libraries | [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L12-L18] [src/digitalmodel/solvers/orcaflex/results_analysis/anomaly_detection.py:L12-L17] [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L12-L19] |
| watchdog (optional) | Filesystem observer; missing import selects polling | [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L18-L31] [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L228-L241] |
| Internal analysis helpers | `quick_classify` calls parser then classifier; `validate_file_data` reads CSV then validates | [src/digitalmodel/solvers/orcaflex/results_analysis/component_classifier.py:L503-L511] [src/digitalmodel/solvers/orcaflex/results_analysis/data_validator.py:L860-L865] |
| Upstream OrcaFlex CSV exporter | File-format association; source extraction model and solver version must be supplied separately | [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L1-L5] [src/digitalmodel/solvers/orcaflex/results_analysis/loading_decoder.py:L53-L61] |
| Downstream caller | Returns dictionaries/dataclasses and invokes supplied monitor callback; no named downstream application is qualified here | [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L321-L347] [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L202-L214] [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L404-L418] |

No solver interface is imported by these ten files; source names below are textual format/service references, not proof of a licensed runtime dependency. The only conditional external import in monitoring is watchdog. [src/digitalmodel/solvers/orcaflex/results_analysis/__init__.py:L1-L30] [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L8-L31]

### Complete source index of OrcaFlex names

Case-insensitive text scan of every package `.py` file at the code pin, including identifiers, comments, docstrings and literal import paths; each occurrence line is retained below as a dependency mention, not as an executed call.

| Source file | Every matching line |
| --- | --- |
| `__init__.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/__init__.py:L1-L1]; [src/digitalmodel/solvers/orcaflex/results_analysis/__init__.py:L3-L3]; [src/digitalmodel/solvers/orcaflex/results_analysis/__init__.py:L6-L6]; [src/digitalmodel/solvers/orcaflex/results_analysis/__init__.py:L15-L15] |
| `anomaly_detection.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/anomaly_detection.py:L2-L2]; [src/digitalmodel/solvers/orcaflex/results_analysis/anomaly_detection.py:L73-L73] |
| `comparative_analysis.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/comparative_analysis.py:L2-L2]; [src/digitalmodel/solvers/orcaflex/results_analysis/comparative_analysis.py:L69-L69] |
| `component_classifier.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/component_classifier.py:L4-L4]; [src/digitalmodel/solvers/orcaflex/results_analysis/component_classifier.py:L21-L21]; [src/digitalmodel/solvers/orcaflex/results_analysis/component_classifier.py:L78-L78] |
| `csv_parser.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L2-L2]; [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L4-L4]; [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L26-L26]; [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L54-L54]; [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L64-L64]; [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L98-L98]; [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L283-L283] |
| `data_validator.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/data_validator.py:L4-L4]; [src/digitalmodel/solvers/orcaflex/results_analysis/data_validator.py:L76-L76]; [src/digitalmodel/solvers/orcaflex/results_analysis/data_validator.py:L127-L127] |
| `file_monitor.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L4-L4]; [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L110-L110]; [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L111-L111]; [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L145-L145]; [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L230-L230]; [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L626-L626]; [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L630-L630]; [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L633-L633] |
| `loading_decoder.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/loading_decoder.py:L4-L4]; [src/digitalmodel/solvers/orcaflex/results_analysis/loading_decoder.py:L83-L83] |
| `sensitivity_analysis.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L2-L2]; [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L67-L67] |
| `statistical_analysis.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L2-L2]; [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L61-L61] |

### Implementation inventory and further-implementation search

Implementation-map ref: `origin/codex/implmap-work`; map-file revision `b5acbe5f61ed4c755c1f7ea4fafa4611bd14e407`; declared data revision `947fa50ff9a9e210de7e8f90e01b37add85c5954`. Map citations explicitly use the map-file revision after `@`; all other citations use the code pin. [docs/registry/implementation-map.yaml@b5acbe5f61ed4c755c1f7ea4fafa4611bd14e407:L1-L4]

Inventory result: **0 map families, 0 in-package implementation rows**. All 13 families were inspected for implementation paths inside this header package; no subpackage is excluded. The complete required family inventory is therefore empty. Counted-family grep: **NOT_APPLICABLE**, because no map family has an implementation row in this package. No counted-family misses are claimed; the numeric pass condition has no counted families. [docs/registry/implementation-map.yaml@b5acbe5f61ed4c755c1f7ea4fafa4611bd14e407:L50-L23673]

The map does mention `file_monitor.py` and `sensitivity_analysis.py` as rejected candidates, not implementation rows; these do not make a counted family. [docs/registry/implementation-map.yaml@b5acbe5f61ed4c755c1f7ea4fafa4611bd14e407:L28464-L28471]

Repository search included `src/`, `scripts/`, tests, examples and documentation, using exact service/method names and formula/call signals (`sqrt(mean`, `trapz`, `pearsonr`, `mannwhitneyu`, `IsolationForest`, `hashlib.md5`). An independent descriptive/RMS path exists in signal analysis, and a script computes residual RMS for chart annotation; these are map follow-up findings outside this package and do not alter the counted-family result. [src/digitalmodel/signal_processing/signal_analysis/core/timeseries.py:L280-L313] [scripts/python/digitalmodel/legacy/generate_integration_charts.py:L752-L771]

Map follow-up: add the section-3 method families before counting them: CSV recognition/metadata and polar extraction; descriptive/time-weighted statistics; component/loading inference; data-quality scoring; correlation/regression/interval/trend/distribution tests; pairwise/group comparison; response-surface and sensitivity analysis; anomaly detection; asynchronous file-event processing. Inventory other implementations and decide canonical routes with the relevant family quality manager; issue pending (5). [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L96-L144] [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L27-L32] [src/digitalmodel/solvers/orcaflex/results_analysis/comparative_analysis.py:L364-L482] [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L118-L135] [src/digitalmodel/solvers/orcaflex/results_analysis/anomaly_detection.py:L37-L43] [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L309-L331]

## 10. Coverage and open items

### Files and test evidence

Read coverage includes all ten package Python files, their algorithm branches and docstrings; the source/test inventories below identify the exact pinned files. Advanced numerical adequacy beyond the main CSV path remains an open item rather than an inferred validation claim.

| Package file read | Pinned coverage |
| --- | --- |
| `__init__.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/__init__.py:L1-L30] |
| `anomaly_detection.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/anomaly_detection.py:L1-L868] |
| `comparative_analysis.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/comparative_analysis.py:L1-L796] |
| `component_classifier.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/component_classifier.py:L1-L544] |
| `csv_parser.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/csv_parser.py:L1-L546] |
| `data_validator.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/data_validator.py:L1-L866] |
| `file_monitor.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/file_monitor.py:L1-L650] |
| `loading_decoder.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/loading_decoder.py:L1-L640] |
| `sensitivity_analysis.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/sensitivity_analysis.py:L1-L1114] |
| `statistical_analysis.py` | [src/digitalmodel/solvers/orcaflex/results_analysis/statistical_analysis.py:L1-L524] |

| Test file | Collected test nodes | BASE passed / failed / skipped / NOT_RUN | AFTER passed / failed / skipped / NOT_RUN | Pinned source |
| --- | --- | --- | --- | --- |
| `test_anomaly_detection.py` | 107 | 107 / 0 / 0 / 0 | 107 / 0 / 0 / 0 | [tests/solvers/orcaflex/results_analysis/test_anomaly_detection.py:L1-L922] |
| `test_comparative_analysis.py` | 100 | 100 / 0 / 0 / 0 | 100 / 0 / 0 / 0 | [tests/solvers/orcaflex/results_analysis/test_comparative_analysis.py:L1-L1026] |
| `test_component_classifier.py` | 82 | 82 / 0 / 0 / 0 | 82 / 0 / 0 / 0 | [tests/solvers/orcaflex/results_analysis/test_component_classifier.py:L1-L593] |
| `test_csv_parser.py` | 110 | 110 / 0 / 0 / 0 | 110 / 0 / 0 / 0 | [tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L1-L914] |
| `test_data_validator.py` | 48 | 48 / 0 / 0 / 0 | 48 / 0 / 0 / 0 | [tests/solvers/orcaflex/results_analysis/test_data_validator.py:L1-L505] |
| `test_loading_decoder.py` | 45 | 45 / 0 / 0 / 0 | 45 / 0 / 0 / 0 | [tests/solvers/orcaflex/results_analysis/test_loading_decoder.py:L1-L410] |
| `test_sensitivity_analysis.py` | 109 | 109 / 0 / 0 / 0 | 109 / 0 / 0 / 0 | [tests/solvers/orcaflex/results_analysis/test_sensitivity_analysis.py:L1-L1128] |
| `test_statistical_analysis.py` | 88 | 87 / 1 / 0 / 0 | 87 / 1 / 0 / 0 | [tests/solvers/orcaflex/results_analysis/test_statistical_analysis.py:L1-L659] |

Recount: 654 test-function definitions expand to 689 collected nodes because of parametrization; the header docstring's “689 passing tests” is a repository claim, not the observed result (688 passed, 1 failed). Test evidence above replaces the earlier inventory estimate of 654 tests. [src/digitalmodel/solvers/orcaflex/results_analysis/__init__.py:L5-L10] [tests/solvers/orcaflex/results_analysis/test_component_classifier.py:L214-L241] [tests/solvers/orcaflex/results_analysis/test_sensitivity_analysis.py:L170-L183]

Documentation search record: package tree inspected recursively, plus `docs/` searched for `results_analysis`, `results analysis` and `results-analysis`; no pre-existing package README/docs or matching Markdown/reStructuredText under `docs/` was found. The package docstring and the runtime dependency contract describe the extraction; the routing registry supplies enclosing-domain maturity. [src/digitalmodel/solvers/orcaflex/results_analysis/__init__.py:L1-L30] [tests/contracts/test_runtime_dependencies.py:L22-L28] [docs/registry/module-routing.yaml:L279-L288]

The calculation-citation contract was readable; no inaccessible licensed standard or private dataset was consulted. Existing source rules remain implementation descriptions, with their missing independent engineering basis recorded below.

### Open register

All items are assigned to the maintainers for issue registration and to the family quality manager for technical disposition; draft numbers refer to the accompanying issue drafts, not opened issues. Clearance remains pending replacement of draft references with issue numbers.

| Item | Remaining work and limit | Issue |
| --- | --- | --- |
| U1 | UNVERIFIED claims in sections 1 and 2: supported solver versions and engineering applicability; export coordinate frame/origin/sign/phase/unit conventions. Obtain a public synthetic export with declared conventions and version; do not infer them from unit labels | issue pending (4) |
| U2 | UNVERIFIED claim in section 3: independent provenance and applicability of hard-coded bounds, inference ranges and coefficients; independently assess placeholder intervals/p-values and median-split sensitivity before engineering use | issue pending (4) |
| D1 | Resolve perfect-fit warning and uncertainty degeneracy; retain visible failing R4/C10 | issue pending (1) |
| D2 | Correct parser/classifier unit-token recognition and chunked encoding fallback; distinguish defect-characterization tests from corrected-behavior tests | issue pending (2) |
| D3 | Preserve parameter/response pair identity through asynchronous completion and partial callback failure | issue pending (3) |
| D4 | Correct monitor lifecycle and thread-safe handoff; add missing monitor tests; no executable monitor detection check exists | #1925 |
| D5 | Preserve valid zero environmental values when combining conditions | issue pending (6) |
| Q1 | Obtain upstream source-model G1/G2/G3 records; C04/C06/C07 NOT_RUN for absent licensed run and fixtures; conditional G4 record when new diffraction supplies vessel data; implement/verify C09 appendix linkage | issue pending (4) |
| Q2 | Main-path validation only: 3 closed-form examples and 4 regression examples. Add independent public numerical references for comparisons, confidence coverage, anomaly models, response surfaces and global sensitivity; do not reuse the module's outputs as references | issue pending (4) |
| Q3 | Confirm deputy/coverage assignments and shared-log registration; complete independent review; add detailed consumer/exporter inventory rows and solver-version qualification | issue pending (4) |
| M1 | Map lacks this package's section-3 method families; add them and repository alternatives before any counted-family acceptance grep | issue pending (5) |

No client-derived values are included; this draft establishes no private sidecar entry. Add a sidecar entry only if a later restricted lesson requires it.
