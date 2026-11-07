# Time-series module knowledge pack

| Header | Value |
|---|---|
| Module | signal_processing/time_series |
| Repository | digitalmodel |
| Path | src/digitalmodel/signal_processing/time_series |
| Pack revision | 0 |
| Code revision | 21942e8cfc7dfb985c5bfa693245df7d197cf76c |
| Analysis family | Engineering calculations (EC); AQG r0, section 10, Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

`TimeSeriesAnalysis.router` dispatches sample FFT, sample window averaging, CSV window averaging and rainflow branches; `TimeSeriesComponents` emits a deprecation warning directing migration to `signal_analysis`. [src/digitalmodel/signal_processing/time_series/time_series_analysis.py:L20-L41] [src/digitalmodel/signal_processing/time_series/time_series_components.py:L1-L24]

The domain maturity is beta; this does not establish numerical qualification of the deprecated package. [docs/registry/module-routing.yaml:L438-L447]

The scope includes all four Python files in this package, including the alternate component class and standalone configuration driver; the replacement package has its own pack and is a dependency comparison only. [src/digitalmodel/signal_processing/time_series/time_series.py:L19-L81] [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L3-L50] [docs/registry/module-routing.yaml:L438-L443]

The checked numerical scope is synthetic FFT bins, energy, reconstruction, peak location and legacy averaging; the three original wrapper tests replace the engine with a mock and provide no numerical acceptance. [tests/signal_processing/time_series/test_component_values.py:L47-L149] [tests/signal_processing/time_series/test_sample_fft.py:L13-L38] [tests/signal_processing/time_series/test_window_fft.py:L13-L38] [tests/signal_processing/time_series/test_csv_window_fft.py:L13-L38]

## 2. Inputs and outputs

Input contract (A denotes the arbitrary signal-amplitude unit; it is not a specified physical quantity). [tests/signal_processing/time_series/test_component_values.py:L47-L58]

| Input | Unit | Range or requirement | Default | Source |
|---|---|---|---|---|
| `signal`, `time_step` | A, seconds when frequency is Hz | Array/list and matching sampling interval; no positive-interval or finite-value guard here | `cfg.fft.time_step` | [src/digitalmodel/signal_processing/time_series/time_series_components.py:L364-L395] |
| Alternate `signal`, `time_step` | A, same time convention | Nested `default.analysis.fft` rather than top-level `fft` | Configuration interval | [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L285-L328] |
| CSV groups | User time and signal units | File, selected columns, filters and transformations; spacing taken from first two time values only | No default file | [src/digitalmodel/signal_processing/time_series/time_series_components.py:L58-L92] |
| Window size | samples | Whole nonoverlapping windows; main API rejects length below 1.5 times size; trailing remainder unused | `fft.window.size` | [src/digitalmodel/signal_processing/time_series/time_series_components.py:L275-L306] |
| Band-pass bounds | reciprocal input-time unit | Strict interior of minimum and maximum; both cutoffs removed | Disabled when flag false | [src/digitalmodel/signal_processing/time_series/time_series_components.py:L408-L435] |
| Synthetic signal configuration | seconds, dimensionless amplitude | Seed, interval, period and time range; no range validation | 1234, 0.02 s, 5 s, [0,20) s; noise factor 0.5 | [src/digitalmodel/signal_processing/time_series/time_series_components.py:L600-L626] |
| Rainflow samples | A | Passed to `rainflow.extract_cycles`; no local validation | None | [src/digitalmodel/signal_processing/time_series/time_series_components.py:L644-L657] |
| Legacy dataframe calculations | Caller units | First column treated as time/index; angle passed directly to `math.tan`; integration assumes datetime differences | Integration initial value 0 | [src/digitalmodel/signal_processing/time_series/time_series_components.py:L498-L519] [src/digitalmodel/signal_processing/time_series/time_series_components.py:L681-L738] |

| Output | Unit and convention | Destination | Source |
|---|---|---|---|
| Main FFT dictionaries | `fft` complex A, `power=abs(fft)**2` in A², `fft_freq` in reciprocal time; retained raw bins nonnegative | `time_trace` plus `fft` dataframe; filtered dictionary | [src/digitalmodel/signal_processing/time_series/time_series_components.py:L376-L395] |
| Alternate FFT pair | Raw power and signed frequency bins when unfiltered; real inverse-FFT trace | Dataframe and array, no complex coefficient column | [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L294-L328] |
| CSV spectrum paths | CSV retains numeric column values without unit conversion; index omitted | `cfg[basename].csv.signal_fft` and optional filtered path | [src/digitalmodel/signal_processing/time_series/time_series_components.py:L121-L167] |
| Cycle table and dictionary | Range/mean in A; count in cycles; start/end in sample indices | Dataframe columns `range, mean, count, i_start, i_end` and `to_dict()` | [src/digitalmodel/signal_processing/time_series/time_series_components.py:L651-L657] |
| Response ratio | Amplitude ratio; phase radians by default, degrees with `phase.deg`; frequency inherited | Raw and filtered ratio frames; currently broken call signature, see section 6 | [src/digitalmodel/signal_processing/time_series/time_series_components.py:L439-L488] |

The FFT has no spatial coordinate frame; custom displacement computes forward/starboard components from caller-selected angles and length without defining an origin or converting angle units. [src/digitalmodel/signal_processing/time_series/time_series_components.py:L376-L395] [src/digitalmodel/signal_processing/time_series/time_series_components.py:L681-L707]

## 3. Method and governing standards

| Method | Implementation and reference | Qualification boundary |
|---|---|---|
| Raw discrete Fourier transform | SciPy FFT, squared coefficient magnitude and `fftfreq`; NumPy reference manual 1.26, Discrete Fourier Transform, as cited by the existing hand-value tests | Unnormalized raw power, not PSD; interior single-tone amplitude and Parseval derivation in the test docstring. [src/digitalmodel/signal_processing/time_series/time_series_components.py:L376-L395] [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L294-L328] [tests/signal_processing/time_series/test_component_values.py:L3-L10] [tests/signal_processing/time_series/test_component_values.py:L47-L84] |
| Window averaging | Whole windows, arithmetic sum followed by division by window count plus one in both classes | Known divisor defect; never use this path as a hand-check reference. [src/digitalmodel/signal_processing/time_series/time_series_components.py:L289-L317] [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L200-L234] |
| Peak extraction | Wavelet, local extrema or `find_peaks`; height and minimum distance from configured percentages | Unsupported selection silently returns an empty list. [src/digitalmodel/signal_processing/time_series/time_series_components.py:L332-L362] [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L256-L283] |
| Frequency filtering | Coefficient mask and inverse FFT; exclusive band bounds | Abrupt cutoff; main inverse errors become `None`. [src/digitalmodel/signal_processing/time_series/time_series_components.py:L397-L437] [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L301-L328] |
| Rainflow | Delegates to the external `rainflow.extract_cycles`; docstring names ASTM E1049-85 | ASTM E1049, Standard Practices for Cycle Counting in Fatigue Analysis, edition 1985; no clause title specified in code; clause text not checked against the licensed copy; no compliance claim or citation sidecar emitted by this wrapper. [src/digitalmodel/signal_processing/time_series/time_series_components.py:L644-L657] |
| Auxiliary calculations | Rolling mean, sigma clipping, statistics, two-component Euclidean resultant, tangent displacement and cumulative trapezoid integration | No governing standard specified in these implementations; legacy API defects are recorded in section 6. [src/digitalmodel/signal_processing/time_series/time_series_components.py:L498-L591] [src/digitalmodel/signal_processing/time_series/time_series_components.py:L659-L738] [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L338-L513] |

The synthetic hand values are independent finite-sum answers, not snapshots; the existing documentation derives two nonzero coefficients, amplitude 4 A and mean-square 8 A², with absolute tolerance 1e-9 and relative tolerance zero. [docs/domains/signal_processing/synthetic-value-checks.md:L55-L77] [tests/signal_processing/time_series/test_component_values.py:L47-L84]

## 4. Validated worked examples

Offline record: BASE and AFTER use code revision in the header; commands are `PYTHONPATH=src uv run --no-sources python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`, without an addopts override; the workflow node runs in a temporary repository copy because it writes results. The component command collects 10 nodes: 8 PASS, 2 strict XFAIL, 0 unexpected failures, 0 skipped, 0 NOT_RUN; the workflow command collects 1 node, PASS. These are execution records, not additional source claims.

Independent answers use the finite geometric-sum and Parseval derivations in the existing test, with absolute tolerance 1e-9 and relative tolerance 0; A is an arbitrary amplitude unit, so raw coefficient power has units A² with unnormalized sample-count scaling. [tests/signal_processing/time_series/test_component_values.py:L3-L10] [tests/signal_processing/time_series/test_component_values.py:L47-L84]

| Case | Source/input and expected result | Test node ID | BASE / AFTER |
|---|---|---|---|
| V1 main raw FFT | Synthetic 256 samples, 256 Hz, amplitude 4 A at 16 Hz; complex coefficient -512j A, raw power 262144 A², spacing 1 Hz, mean-square 8 A²; tolerance above | `tests/signal_processing/time_series/test_component_values.py::test_raw_amplitude_resolution_parseval_and_peaks[components]` | PASS / PASS; [tests/signal_processing/time_series/test_component_values.py:L47-L84] |
| V2 alternate raw FFT | Same finite-sum answer and reconstructed samples within 1e-9 A | `tests/signal_processing/time_series/test_component_values.py::test_raw_amplitude_resolution_parseval_and_peaks[variant]` | PASS / PASS; [tests/signal_processing/time_series/test_component_values.py:L47-L84] |
| V3 both APIs | Both raw APIs independently compared with 262144 A², absolute 1e-9 | `tests/signal_processing/time_series/test_component_values.py::test_legacy_apis_agree_with_hand_power` | PASS / PASS; [tests/signal_processing/time_series/test_component_values.py:L87-L102] |
| V4 main window peak | Two identical 256-sample windows; exactly one positive peak flag at 16 Hz, 0 Hz error; no amplitude qualification | `tests/signal_processing/time_series/test_component_values.py::test_window_peak_frequency_and_flags[components]` | PASS / PASS; [tests/signal_processing/time_series/test_component_values.py:L105-L121] |
| V5 alternate window peak | Same peak; current chained-assignment warning explicitly required | `tests/signal_processing/time_series/test_component_values.py::test_window_peak_frequency_and_flags[variant]` | PASS / PASS; [tests/signal_processing/time_series/test_component_values.py:L105-L121] |
| F1 main average, not validated | Arithmetic mean of two identical spectra should retain 262144 A², amplitude 4 A, mean-square 8 A²; abs 1e-9, rel 0 | `tests/signal_processing/time_series/test_component_values.py::test_window_average_preserves_amplitude_and_parseval[components]` | FAIL (strict XFAIL) / same; [tests/signal_processing/time_series/test_component_values.py:L124-L149] |
| F2 alternate average, not validated | Same independent expected answer; defective divisor is not adopted as reference | `tests/signal_processing/time_series/test_component_values.py::test_window_average_preserves_amplitude_and_parseval[variant]` | FAIL (strict XFAIL) / same; [tests/signal_processing/time_series/test_component_values.py:L124-L149] |

Regression examples (four; no independent amplitude acceptance for the noisy workflow, no numerical acceptance for mocked wrappers). [tests/workflows/test_durable_workflows.py:L462-L467] [tests/signal_processing/time_series/test_sample_fft.py:L13-L38]

| Case | Input and expected result | Test node ID | BASE / AFTER |
|---|---|---|---|
| R1 wrapper | `sample_fft.yml`; mocked completed status only; no numerical tolerance | `tests/signal_processing/time_series/test_sample_fft.py::test_run_process` | PASS / PASS; [tests/signal_processing/time_series/test_sample_fft.py:L13-L38] |
| R2 wrapper | `sample_window_fft.yml`; same mock limitation | `tests/signal_processing/time_series/test_window_fft.py::test_run_process` | PASS / PASS; [tests/signal_processing/time_series/test_window_fft.py:L13-L38] |
| R3 wrapper | `csv_window_fft.yml`; same mock limitation | `tests/signal_processing/time_series/test_csv_window_fft.py::test_run_process` | PASS / PASS; [tests/signal_processing/time_series/test_csv_window_fft.py:L13-L38] |
| R4 durable workflow | `examples/workflows/time-series/input.yml`; dominant frequency 0.20 Hz (pytest approximate default), raw power greater than 250000 A²; stochastic power threshold is regression evidence | `tests/workflows/test_durable_workflows.py::test_workflow_registry[time-series]` | PASS / PASS; [docs/registry/workflows.yaml:L231-L239] [tests/workflows/test_durable_workflows.py:L462-L467] |

## 5. QA gates

Applicability follows AQG r0, section 10, Python calculations; G5 follows AQG r0, section 9.

| Gate | Applicability | Module check and numeric pass criterion | Evidence |
|---|---|---|---|
| G0 | Applicable | C0: check synthetic input rate times `time_step` differs from 1 by at most 1e-12 dimensionless; C7: resolve every pack citation, 0 missing files or line ranges | Synthetic fixture and source resolver; [tests/signal_processing/time_series/test_component_values.py:L25-L44] [tests/signal_processing/time_series/test_component_values.py:L47-L58] |
| G1 | Applicable, Python imports and tests | C1: component collection 10 nodes and workflow collection 1 node; 0 unexpected test failures and 0 unexplained skips; preserve and report the two strict expected failures separately | Offline pytest summaries; [tests/signal_processing/time_series/test_component_values.py:L17-L22] [tests/signal_processing/time_series/test_component_values.py:L124-L149] [docs/registry/workflows.yaml:L231-L239] |
| G2 | Applicable, hand values | C2: raw `fft_analysis` amplitude 4 A, spacing 1 Hz, mean-square 8 A², power 262144 A², abs error at most 1e-9 in each stated unit, rel 0 (V1/V2); C3: both APIs give that power within 1e-9 A² (V3); C4: `identify_signal_peaks` and window flags yield exactly [16] Hz, 0 Hz error (V4/V5); C6: averaging retains the same hand power within 1e-9 A² (F1/F2), currently FAIL | Test assertions; [tests/signal_processing/time_series/test_component_values.py:L47-L149] |
| G3 | NOT_APPLICABLE | Short solver dynamics is outside the Python calculation gate set | AQG r0, section 10, Python calculations |
| G4 | NOT_APPLICABLE | No diffraction mesh or hydrostatics in the reviewed FFT calculation route | AQG r0, section 10, Python calculations; [src/digitalmodel/signal_processing/time_series/time_series_analysis.py:L20-L41] |
| G5 | Applicable | C5: evidence manifest contains 11 example-node results, 6 gate applicability rows and both test-run records; 0 missing entries, unit records | This pack sections 4–5 and scratch self-check JSON; AQG r0, section 9 |

Checks defined: eight; G0 two, G1 one, G2 four, G5 one. C0 and C7 are reproducible arithmetic/source checks; C1–C4 and C6 use existing test nodes; C5 checks record completeness. These criteria define this draft's review procedure, not approval of the failed average.

## 6. Known failure modes and pitfalls

Silent-failure sweep sites follow first; intentional defaults and masking are included so that their semantics are visible rather than treated as validated error recovery. Each row identifies one site or a contiguous related operation, with every repeated implementation cited separately.

sweep: 57 found

Reproduction commands (read-only; inspect enclosing definitions after matching):

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|fillna|sys.exit|\.get\(|\.microseconds|time_step|np.angle|math.tan|to_csv' src/digitalmodel/signal_processing/time_series --glob '*.py'
rg -n 'default|seed|random|rolling|floor|window|range|initial|empty|cache|read_csv|degrees|radian|time/index|cfg_variations' src/digitalmodel/signal_processing/time_series --glob '*.py'
```

| Site | Symptom and cause | Detection | Tracking and evidence |
|---|---|---|---|
| S01 | Missing CSV logs errors and continues without a result | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L64-L75] |
| S02 | Sampling interval uses only first two time values; regularity and time units are unchecked | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L82-L85] |
| S03 | Absent plotting flag defaults to True; optional visualization is invoked | no detection check exists | issue pending (3); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L170-L171] |
| S04 | Integer window count drops trailing samples without recording discarded count | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L289-L293] |
| S05 | Window count plus one reduces averaged power and coefficients | C6 | #3350; [src/digitalmodel/signal_processing/time_series/time_series_components.py:L304-L306] |
| S06 | Rolling startup NaNs and any other missing spectrum values become zero | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L308-L317] |
| S07 | Unknown peak solver returns the initialized empty list | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L340-L362] |
| S08 | Wavelet indices shifted down by one can become negative | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L341-L346] |
| S09 | Configured sampling interval accepted without positive/finite validation | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L373-L381] |
| S10 | Band cutoff coefficients set to zero, including both boundary bins | no detection check exists | #60; [src/digitalmodel/signal_processing/time_series/time_series_components.py:L415-L418] |
| S11 | Filtered spectrum missing values replaced by zero | no detection check exists | #60; [src/digitalmodel/signal_processing/time_series/time_series_components.py:L423-L424] |
| S12 | Bare exception hides inverse-FFT failure as a None trace | no detection check exists | #60; [src/digitalmodel/signal_processing/time_series/time_series_components.py:L425-L432] |
| S13 | Real and imaginary columns initially zero, then overwritten; initialization is intentional | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L450-L458] |
| S14 | Phase uses radians unless degree flag is true; no phase convention supplied upstream | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L461-L465] |
| S15 | Low-excitation response ratio becomes zero in all result columns; raw division can be nonfinite | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L470-L486] |
| S16 | Moving-average path returns an empty frame if by_data_points is absent | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L508-L519] |
| S17 | Moving average assumes first column is time/index and leaves it unchanged | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L511-L516] |
| S18 | Lower sigma threshold replaces low values with NaN | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L543-L549] |
| S19 | Upper clipping checks the first range item too; negative-sigma string also matches sigma | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L550-L561] |
| S20 | No sample configuration silently chooses fixed seed, time interval, period and range | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L605-L615] |
| S21 | Missing individual sample keys silently use fixed seed, interval, period and range | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L617-L620] |
| S22 | Sample generator resets global random state and uses fixed noise scale 0.5 | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L622-L626] |
| S23 | Custom resultant ignores a dataset not matching the expected dataframe selector | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L666-L672] |
| S24 | Displacement takes tangent of caller angles directly and assumes compatible length units | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L686-L707] |
| S25 | Integration treats first column as datetime and uses minimum microsecond component, losing whole seconds | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L724-L727] |
| S26 | Unsupported integration selector leaves the frame without integration columns | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L728-L738] |
| S27 | Single/double integration sets initial integrals to zero without caller initial conditions | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L729-L735] |
| S28 | Database connection exception printed and loop continues | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L18-L25] |
| S29 | Input acquisition exception printed and loop continues | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L26-L33] |
| S30 | Missing data source exits with default successful exit status | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L35-L38] |
| S31 | Database analysis exception printed without propagated failure | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L41-L50] |
| S32 | Missing or unsupported input source prints and implicitly returns None | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L96-L104] |
| S33 | Connection setup exception returns False; caller ignores the return | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L113-L119] |
| S34 | Whole-window truncation silently drops trailing samples | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L202-L210] |
| S35 | Alternate average divides by window count plus one | C6 | #3350; [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L219-L222] |
| S36 | Rolling startup and missing values become zero | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L224-L234] |
| S37 | Short record prints and exits with default successful exit status | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L245-L249] |
| S38 | Explicit time_window or missing window config prints and returns empty frame | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L251-L254] |
| S39 | Unsupported peak solver returns initialized empty list | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L261-L283] |
| S40 | Alternate wavelet index shift can produce negative indices | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L262-L267] |
| S41 | Configured sampling interval accepted without unit or regularity validation | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L291-L299] |
| S42 | Alternate band mask zeros coefficients at and outside cutoffs | no detection check exists | #60; [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L308-L311] |
| S43 | Alternate raw/filtered spectrum replaces missing values with zero | no detection check exists | #60; [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L324-L324] |
| S44 | Missing by_data_points setting returns empty moving-average frame | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L347-L358] |
| S45 | Alternate moving average assumes first column is time/index | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L350-L355] |
| S46 | Alternate lower sigma clipping substitutes NaN | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L381-L387] |
| S47 | Alternate upper clipping tests first range item and substitutes NaN | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L388-L399] |
| S48 | Alternate resultant silently ignores unmatched dataframe selector | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L443-L449] |
| S49 | Alternate tangent displacement assumes radian angles and compatible lengths | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L462-L483] |
| S50 | Alternate integration loses whole seconds from datetime step | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L499-L502] |
| S51 | Unsupported alternate integration selector silently omits output columns | no detection check exists | issue pending (1); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L503-L513] |
| S52 | Alternate single/double integration assumes zero initial integral | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L504-L510] |
| S53 | Alternate sample fixes seed, interval, period, range and noise, resets global RNG | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L519-L526] |
| S54 | Example discards the boolean result of allclose instead of asserting | no detection check exists | issue pending (3); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L546-L548] |
| S55 | Example high-frequency coefficients zeroed at selected peak | no detection check exists | #60; [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L558-L560] |
| S56 | No configured variation returns an empty result list; intentional absence, not a computed result | no detection check exists | issue pending (3); [src/digitalmodel/signal_processing/time_series/time_series.py:L28-L36] |
| S57 | Elapsed datetime.seconds omits fractional seconds and days | no detection check exists | issue pending (2); [src/digitalmodel/signal_processing/time_series/time_series.py:L79-L80] |

No actual `continue` statement, external-text `int()`/`float()` conversion, 1970 default, or empty-result cache write was found in the four reviewed source files; CSV parsing is delegated to pandas rather than validating locale or schema here. [src/digitalmodel/signal_processing/time_series/time_series.py:L1-L87] [src/digitalmodel/signal_processing/time_series/time_series_analysis.py:L1-L53] [src/digitalmodel/signal_processing/time_series/time_series_components.py:L1-L738] [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L1-L574]

| Additional pitfall | Detection | Tracking and evidence |
|---|---|---|
| Rainflow router uses undefined time_series rather than configured samples | no detection check exists | issue pending (3); [src/digitalmodel/signal_processing/time_series/time_series_analysis.py:L34-L37] |
| Standalone driver imports an obsolete common path and calls undefined TimeSeriesComponentsAdapter | no detection check exists | issue pending (3); [src/digitalmodel/signal_processing/time_series/time_series.py:L39-L48] |
| Main dataframe FFT wrapper calls fft_analysis and fft_window_analysis without required arguments | no detection check exists | issue pending (3); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L199-L213] |
| Response-ratio helper calls fft_window_analysis with an incompatible signature | no detection check exists | issue pending (3); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L439-L445] |
| Statistics mean/stdev rows include one extra None and can have the wrong row length | no detection check exists | issue pending (3); [src/digitalmodel/signal_processing/time_series/time_series_components.py:L575-L589] |
| Alternate statistics repeats the row-length defect | no detection check exists | issue pending (3); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L413-L426] |
| Alternate run_example unpacks three return values from a two-value FFT API | no detection check exists | issue pending (3); [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L533-L539] |
| Spectrum filtering uses min/max variables even when filter flag is absent or false | no detection check exists | #60; [src/digitalmodel/signal_processing/time_series/time_series_components.py:L263-L273] |
| Alternate peak assignment uses chained indexing; warning is explicitly tested, future pandas behavior unqualified | C4 | #3350; [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L241-L243] |

Issue references are leads rather than evidence of an incident: #60 requests filtering and inverse traces; #3350 requests synthetic value coverage; #711 concerns broader durable-workflow onboarding, not a confirmed defect in this registered workflow. The numerical and source evidence for these boundaries is the cited code and tests. [src/digitalmodel/signal_processing/time_series/time_series_components.py:L397-L437] [tests/signal_processing/time_series/test_component_values.py:L47-L149] [docs/registry/workflows.yaml:L231-L239]

## 7. Lessons log

| Date | Entry | Lesson | Evidence | Pack change | Shared log |
|---|---|---|---|---|---|
| 2026-11-06 | Initial draft | A mocked engine success is not a numeric calculation check; keep hand values and defective averages separate | [tests/signal_processing/time_series/test_sample_fft.py:L13-L38] [tests/signal_processing/time_series/test_component_values.py:L47-L149] | Sections 4–6 distinguish passing hand checks, expected failures and regression wrappers | Not assigned; issue pending (4) |

## 8. Ownership

Proposed review allocation: engineering calculations quality manager; deputy role: engineering calculations deputy quality manager; producing role: module documentation maintainer; technical classification and numerical-criterion questions go to the family quality manager, with cross-family matters referred to the maintainers (AQG r0, section 11, Sign-off and independence).

UNVERIFIED: named appointments, deputy availability and shared-log registration are not evidenced in the repository sources inspected; issue pending (4).

## 9. Dependencies and alternative implementations

The main component imports NumPy, pandas, rainflow, SciPy, assetutilities and colorama; CSV preparation and plotting delegate to assetutilities; the router uses `update_deep_dictionary`. [src/digitalmodel/signal_processing/time_series/time_series_components.py:L26-L43] [src/digitalmodel/signal_processing/time_series/time_series_components.py:L77-L92] [src/digitalmodel/signal_processing/time_series/time_series_components.py:L169-L188] [src/digitalmodel/signal_processing/time_series/time_series_analysis.py:L5-L13]

The durable time-series workflow uses the package's engine route and local synthetic input; its registry declares offline runtime and two outputs. No solver version is required by that route. [docs/registry/workflows.yaml:L231-L239] [examples/workflows/time-series/input.yml:L1-L33] [src/digitalmodel/signal_processing/time_series/time_series_analysis.py:L20-L41]

The alternate database/visualization routes use obsolete `common.*` imports and a database adapter; their operation is outside the passing numerical examples. [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L52-L55] [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L109-L119] [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L430-L434]

Implementation-map source pin: `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; data revision named by that map: `270c8759c374b67a7f032953e7fcdb14e0224461`. Every citation to `docs/registry/implementation-map.yaml` below resolves at that source pin; all other repository citations resolve at the code pin in the header. [docs/registry/implementation-map.yaml:L1-L3]

Counted family: `sn-curves-and-miner-damage`, with two in-package implementation rows; it is the only family with such rows, hence the first and only counted family. Its package rows are `TimeSeriesAnalysis.router` and `TimeSeriesComponents.count_cycles`. [docs/registry/implementation-map.yaml:L18780-L18799]

The exact case-sensitive Python `re` patterns are shown below; none matches the package path under `src/digitalmodel/` case-insensitively, so none is excluded. [docs/registry/implementation-map.yaml:L16807-L16816]

```text
rainflow|miner|cycles_to_failure|log_a|log10.*(?:cycles|stress)|(?:stress|stress_range|delta_sigma).*\*\*.*\bm\b|damage_sum|thickness_correction|endurance_limit|sn_curve|SNcurve|sn_data|stress_life
S.N.curve|fatigue.damage|C203|BS.?7608|RP.?2A|SNcurve|SN.?Curve
thickness_correction|endurance_limit|n_cycles|sn_curve|stress_ranges
```

At the code pin the grep hits module-level deprecation/import lines, `count_cycles`, and the router. Module-level text/imports compute no result; the two definitions are already inventoried, and `count_cycles` delegates to the external package. The alternate component file and standalone driver have zero hits. Further second implementations: 0; A8 passes the threshold of at most 1. [src/digitalmodel/signal_processing/time_series/time_series_components.py:L1-L29] [src/digitalmodel/signal_processing/time_series/time_series_components.py:L644-L657] [src/digitalmodel/signal_processing/time_series/time_series_analysis.py:L20-L41] [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L1-L574] [src/digitalmodel/signal_processing/time_series/time_series.py:L1-L87]

Whole-repository discovery includes tracked Python files under `scripts/`, using the same expressions and definition assignment. External hits are map follow-up candidates, not package misses; listed rows retain the map's classification without independently qualifying every external routine. The complete family inventory follows in map order, including helpers, examples and test definitions that the map itself lists. [docs/registry/implementation-map.yaml:L16807-L20963]

Map follow-up: FFT, window averaging, peak selection, filtering, response ratio, moving average, clipping/statistics, resultant, tangent displacement and integration have no in-package family rows in this map revision; they are not counted until mapped, issue pending (4). Their implementations are listed in section 3. [src/digitalmodel/signal_processing/time_series/time_series_components.py:L263-L591] [src/digitalmodel/signal_processing/time_series/time_series_components.py:L659-L738] [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L193-L513]

### Full map-family inventory

The following table reproduces every one of the map family's 392 implementation rows; row numbers are local inventory ordinals, and source lines are map lines rather than code-pin implementation lines. [docs/registry/implementation-map.yaml:L16807-L20963]

| Row | Implementation path | Symbol | Map evidence |
|---|---|---|---|
| 1 | `docs/domains/examples/orcaflex_signal_analysis_example.py` | `example_6_custom_workflow` | [docs/registry/implementation-map.yaml:L16814-L16823] |
| 2 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_1_basic_rainflow_counting` | [docs/registry/implementation-map.yaml:L16824-L16833] |
| 3 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_4_fatigue_analysis` | [docs/registry/implementation-map.yaml:L16834-L16843] |
| 4 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_6_backward_compatibility` | [docs/registry/implementation-map.yaml:L16844-L16853] |
| 5 | `docs/domains/examples/signal_analysis_usage_example.py` | `main` | [docs/registry/implementation-map.yaml:L16854-L16867] |
| 6 | `examples/demos/demo_sn_library.py` | `main` | [docs/registry/implementation-map.yaml:L16868-L16879] |
| 7 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_sn_curves` | [docs/registry/implementation-map.yaml:L16880-L16889] |
| 8 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_rainflow_counting` | [docs/registry/implementation-map.yaml:L16890-L16899] |
| 9 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_damage_accumulation` | [docs/registry/implementation-map.yaml:L16900-L16909] |
| 10 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_frequency_domain_analysis` | [docs/registry/implementation-map.yaml:L16910-L16919] |
| 11 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_quick_analysis` | [docs/registry/implementation-map.yaml:L16920-L16929] |
| 12 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `main` | [docs/registry/implementation-map.yaml:L16930-L16945] |
| 13 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_cli.py` | `list_curves` | [docs/registry/implementation-map.yaml:L16946-L16955] |
| 14 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_cli.py` | `compare_curves` | [docs/registry/implementation-map.yaml:L16956-L16965] |
| 15 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_2_linear_log` | [docs/registry/implementation-map.yaml:L16966-L16975] |
| 16 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_3_with_scf` | [docs/registry/implementation-map.yaml:L16976-L16985] |
| 17 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_4_without_fatigue_limit` | [docs/registry/implementation-map.yaml:L16986-L16995] |
| 18 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_5_dnv_comparison` | [docs/registry/implementation-map.yaml:L16996-L17005] |
| 19 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_6_abs_comparison` | [docs/registry/implementation-map.yaml:L17006-L17015] |
| 20 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_7_reference_comparison` | [docs/registry/implementation-map.yaml:L17016-L17025] |
| 21 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_8_joint_type_comparison` | [docs/registry/implementation-map.yaml:L17026-L17035] |
| 22 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_1_basic_sn_curves` | [docs/registry/implementation-map.yaml:L17036-L17045] |
| 23 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_2_multislope_curves` | [docs/registry/implementation-map.yaml:L17046-L17055] |
| 24 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_3_time_domain_analysis` | [docs/registry/implementation-map.yaml:L17056-L17065] |
| 25 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_4_frequency_domain_analysis` | [docs/registry/implementation-map.yaml:L17066-L17075] |
| 26 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_5_high_stress_range_analysis` | [docs/registry/implementation-map.yaml:L17076-L17085] |
| 27 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_6_shear_data_analysis` | [docs/registry/implementation-map.yaml:L17086-L17095] |
| 28 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `run_all_examples` | [docs/registry/implementation-map.yaml:L17096-L17112] |
| 29 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `example_well_log_analysis` | [docs/registry/implementation-map.yaml:L17113-L17122] |
| 30 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `example_stratigraphic_correlation` | [docs/registry/implementation-map.yaml:L17123-L17132] |
| 31 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `main` | [docs/registry/implementation-map.yaml:L17133-L17145] |
| 32 | `scripts/capabilities/build_onepagers.py` | `<module>` | [docs/registry/implementation-map.yaml:L17146-L17154] |
| 33 | `scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py` | `<module>` | [docs/registry/implementation-map.yaml:L17155-L17163] |
| 34 | `scripts/python/digitalmodel/tools/check_process_fatigue_single.py` | `process_single_test_file` | [docs/registry/implementation-map.yaml:L17164-L17173] |
| 35 | `scripts/python/digitalmodel/tools/migrate_yaml_configs.py` | `YAMLConfigMigrator._migrate_analysis` | [docs/registry/implementation-map.yaml:L17174-L17183] |
| 36 | `scripts/python/digitalmodel/tools/migrate_yaml_configs.py` | `YAMLConfigMigrator._migrate_time_series_components` | [docs/registry/implementation-map.yaml:L17184-L17193] |
| 37 | `scripts/python/digitalmodel/tools/process_fatigue_data.py` | `process_fatigue_files` | [docs/registry/implementation-map.yaml:L17194-L17203] |
| 38 | `scripts/python/digitalmodel/tools/process_fatigue_final.py` | `process_single_test_file` | [docs/registry/implementation-map.yaml:L17204-L17213] |
| 39 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `process_fatigue_file` | [docs/registry/implementation-map.yaml:L17214-L17223] |
| 40 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `process_sample_file` | [docs/registry/implementation-map.yaml:L17224-L17235] |
| 41 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `batch_process_all_files` | [docs/registry/implementation-map.yaml:L17236-L17246] |
| 42 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `rainflow_counting` | [docs/registry/implementation-map.yaml:L17247-L17256] |
| 43 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `process_fatigue_file` | [docs/registry/implementation-map.yaml:L17257-L17268] |
| 44 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `batch_process` | [docs/registry/implementation-map.yaml:L17269-L17280] |
| 45 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v2.py` | `process_single_test_file` | [docs/registry/implementation-map.yaml:L17281-L17290] |
| 46 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v3.py` | `process_single_test_file` | [docs/registry/implementation-map.yaml:L17291-L17300] |
| 47 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v4.py` | `process_single_test_file` | [docs/registry/implementation-map.yaml:L17301-L17310] |
| 48 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v5.py` | `process_single_test_file` | [docs/registry/implementation-map.yaml:L17311-L17320] |
| 49 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v6.py` | `process_single_test_file` | [docs/registry/implementation-map.yaml:L17321-L17330] |
| 50 | `scripts/riser_database/build_tables.py` | `<module>` | [docs/registry/implementation-map.yaml:L17331-L17339] |
| 51 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `SNcurve.from_dnv` | [docs/registry/implementation-map.yaml:L17340-L17349] |
| 52 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.calculate_cycles_to_failure` | [docs/registry/implementation-map.yaml:L17350-L17359] |
| 53 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.calculate_fatigue_damage` | [docs/registry/implementation-map.yaml:L17360-L17371] |
| 54 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.generate_sn_curve_apdl` | [docs/registry/implementation-map.yaml:L17372-L17381] |
| 55 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.create_damage_summary_df` | [docs/registry/implementation-map.yaml:L17382-L17391] |
| 56 | `src/digitalmodel/benchmarks/inventory.py` | `build_model_inventory` | [docs/registry/implementation-map.yaml:L17392-L17401] |
| 57 | `src/digitalmodel/drilling_riser/global_model/fatigue_channels.py` | `half_cycles` | [docs/registry/implementation-map.yaml:L17402-L17411] |
| 58 | `src/digitalmodel/drilling_riser/global_model/fatigue_channels.py` | `extract` | [docs/registry/implementation-map.yaml:L17412-L17423] |
| 59 | `src/digitalmodel/engine.py` | `engine` | [docs/registry/implementation-map.yaml:L17424-L17441] |
| 60 | `src/digitalmodel/fatg_spectral_fatigue/workflow.py` | `router` | [docs/registry/implementation-map.yaml:L17442-L17454] |
| 61 | `src/digitalmodel/fatg_spectral_fatigue/workflow.py` | `_sn_curve` | [docs/registry/implementation-map.yaml:L17455-L17464] |
| 62 | `src/digitalmodel/fatigue/assessment.py` | `_case_histogram` | [docs/registry/implementation-map.yaml:L17465-L17474] |
| 63 | `src/digitalmodel/fatigue/assessment.py` | `assess_fatigue` | [docs/registry/implementation-map.yaml:L17475-L17488] |
| 64 | `src/digitalmodel/fatigue/counting_contract.py` | `<module>` | [docs/registry/implementation-map.yaml:L17489-L17497] |
| 65 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_pylife_4pt` | [docs/registry/implementation-map.yaml:L17498-L17507] |
| 66 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_pypi_rainflow` | [docs/registry/implementation-map.yaml:L17508-L17517] |
| 67 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_sigproc_astm` | [docs/registry/implementation-map.yaml:L17518-L17527] |
| 68 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_calm_buoy` | [docs/registry/implementation-map.yaml:L17528-L17537] |
| 69 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_fapps_counting` | [docs/registry/implementation-map.yaml:L17538-L17547] |
| 70 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_fapps_counter` | [docs/registry/implementation-map.yaml:L17548-L17557] |
| 71 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_struct_fatigue` | [docs/registry/implementation-map.yaml:L17558-L17567] |
| 72 | `src/digitalmodel/fatigue/counting_contract.py` | `_is_counting_name` | [docs/registry/implementation-map.yaml:L17568-L17577] |
| 73 | `src/digitalmodel/fatigue/damage.py` | `miner_damage` | [docs/registry/implementation-map.yaml:L17578-L17587] |
| 74 | `src/digitalmodel/fatigue/damage.py` | `design_life_check` | [docs/registry/implementation-map.yaml:L17588-L17599] |
| 75 | `src/digitalmodel/fatigue/environmental_correction.py` | `environment_correction` | [docs/registry/implementation-map.yaml:L17600-L17609] |
| 76 | `src/digitalmodel/fatigue/environmental_correction.py` | `apply_environment_to_sn` | [docs/registry/implementation-map.yaml:L17610-L17621] |
| 77 | `src/digitalmodel/fatigue/fatg.py` | `fatg_sea_state_damage` | [docs/registry/implementation-map.yaml:L17622-L17631] |
| 78 | `src/digitalmodel/fatigue/fatg.py` | `fatg_annual_damage` | [docs/registry/implementation-map.yaml:L17632-L17643] |
| 79 | `src/digitalmodel/fatigue/fatigue_reporting.py` | `_sec_fatigue_locations` | [docs/registry/implementation-map.yaml:L17644-L17653] |
| 80 | `src/digitalmodel/fatigue/rainflow.py` | `fatigue_life` | [docs/registry/implementation-map.yaml:L17654-L17666] |
| 81 | `src/digitalmodel/fatigue/sn_curves.py` | `_compute_sd` | [docs/registry/implementation-map.yaml:L17667-L17676] |
| 82 | `src/digitalmodel/fatigue/sn_curves.py` | `_build_curve_dict` | [docs/registry/implementation-map.yaml:L17677-L17688] |
| 83 | `src/digitalmodel/fatigue/sn_curves.py` | `get_sn_curve` | [docs/registry/implementation-map.yaml:L17689-L17699] |
| 84 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.knee_stress` | [docs/registry/implementation-map.yaml:L17700-L17711] |
| 85 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.switch_stress` | [docs/registry/implementation-map.yaml:L17712-L17723] |
| 86 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.cycles` | [docs/registry/implementation-map.yaml:L17724-L17735] |
| 87 | `src/digitalmodel/fatigue/sn_library.py` | `_knee_stress` | [docs/registry/implementation-map.yaml:L17736-L17745] |
| 88 | `src/digitalmodel/fatigue/sn_library.py` | `_switch_stress` | [docs/registry/implementation-map.yaml:L17746-L17756] |
| 89 | `src/digitalmodel/fatigue/sn_library.py` | `_bilinear_cycles` | [docs/registry/implementation-map.yaml:L17757-L17766] |
| 90 | `src/digitalmodel/fatigue/sn_library.py` | `_sd` | [docs/registry/implementation-map.yaml:L17767-L17776] |
| 91 | `src/digitalmodel/fatigue/sn_library.py` | `_build_dnv_curves` | [docs/registry/implementation-map.yaml:L17777-L17786] |
| 92 | `src/digitalmodel/fatigue/sn_library.py` | `_build_api_curves` | [docs/registry/implementation-map.yaml:L17787-L17796] |
| 93 | `src/digitalmodel/fatigue/sn_library.py` | `_build_bs7608_curves` | [docs/registry/implementation-map.yaml:L17797-L17808] |
| 94 | `src/digitalmodel/fatigue/sn_library.py` | `_build_iiw_curves` | [docs/registry/implementation-map.yaml:L17809-L17819] |
| 95 | `src/digitalmodel/fatigue/sn_library.py` | `_build_eurocode3_curves` | [docs/registry/implementation-map.yaml:L17820-L17830] |
| 96 | `src/digitalmodel/fatigue/sn_library.py` | `_build_norsok_curves` | [docs/registry/implementation-map.yaml:L17831-L17841] |
| 97 | `src/digitalmodel/fatigue/sn_library.py` | `_build_aws_curves` | [docs/registry/implementation-map.yaml:L17842-L17851] |
| 98 | `src/digitalmodel/fatigue/sn_library.py` | `_build_asme_curves` | [docs/registry/implementation-map.yaml:L17852-L17861] |
| 99 | `src/digitalmodel/fatigue/sn_library.py` | `_build_en13445_curves` | [docs/registry/implementation-map.yaml:L17862-L17872] |
| 100 | `src/digitalmodel/fatigue/sn_library.py` | `_build_pd5500_curves` | [docs/registry/implementation-map.yaml:L17873-L17883] |
| 101 | `src/digitalmodel/fatigue/sn_library.py` | `_build_abs_curves` | [docs/registry/implementation-map.yaml:L17884-L17893] |
| 102 | `src/digitalmodel/fatigue/sn_library.py` | `_build_iso19902_curves` | [docs/registry/implementation-map.yaml:L17894-L17904] |
| 103 | `src/digitalmodel/fatigue/sn_library.py` | `_build_csa_curves` | [docs/registry/implementation-map.yaml:L17905-L17914] |
| 104 | `src/digitalmodel/fatigue/sn_library.py` | `_build_hse_curves` | [docs/registry/implementation-map.yaml:L17915-L17925] |
| 105 | `src/digitalmodel/fatigue/sn_library.py` | `_build_doe_curves` | [docs/registry/implementation-map.yaml:L17926-L17936] |
| 106 | `src/digitalmodel/fatigue/sn_library.py` | `_build_gl_curves` | [docs/registry/implementation-map.yaml:L17937-L17946] |
| 107 | `src/digitalmodel/fatigue/sn_library.py` | `_build_jis_curves` | [docs/registry/implementation-map.yaml:L17947-L17956] |
| 108 | `src/digitalmodel/fatigue/sn_library.py` | `_ensure_loaded` | [docs/registry/implementation-map.yaml:L17957-L17966] |
| 109 | `src/digitalmodel/fatigue/sn_library.py` | `summary_table` | [docs/registry/implementation-map.yaml:L17967-L17978] |
| 110 | `src/digitalmodel/fatigue/sn_library_api.py` | `_record_to_curve_info` | [docs/registry/implementation-map.yaml:L17979-L17988] |
| 111 | `src/digitalmodel/fatigue/sn_library_api.py` | `_record_to_sncurve` | [docs/registry/implementation-map.yaml:L17989-L17998] |
| 112 | `src/digitalmodel/fatigue/sn_library_api.py` | `get_curve` | [docs/registry/implementation-map.yaml:L17999-L18010] |
| 113 | `src/digitalmodel/fatigue/sn_library_api.py` | `calculate_endurance` | [docs/registry/implementation-map.yaml:L18011-L18023] |
| 114 | `src/digitalmodel/fatigue/spectral_fatigue.py` | `wirsching_light_damage` | [docs/registry/implementation-map.yaml:L18024-L18033] |
| 115 | `src/digitalmodel/fatigue/spectral_fatigue.py` | `benasciutti_tovo_damage` | [docs/registry/implementation-map.yaml:L18034-L18043] |
| 116 | `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py` | `weld_fatigue_quickcheck` | [docs/registry/implementation-map.yaml:L18044-L18054] |
| 117 | `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py` | `main` | [docs/registry/implementation-map.yaml:L18055-L18064] |
| 118 | `src/digitalmodel/fatigue/woehler_fitting.py` | `generate_test_data` | [docs/registry/implementation-map.yaml:L18065-L18074] |
| 119 | `src/digitalmodel/fatigue/woehler_fitting.py` | `_fallback_log_regression` | [docs/registry/implementation-map.yaml:L18075-L18084] |
| 120 | `src/digitalmodel/fatigue/workflow.py` | `router` | [docs/registry/implementation-map.yaml:L18085-L18102] |
| 121 | `src/digitalmodel/fatigue/workflow.py` | `_curve_ids` | [docs/registry/implementation-map.yaml:L18103-L18112] |
| 122 | `src/digitalmodel/fatigue/workflow.py` | `_stress_ranges` | [docs/registry/implementation-map.yaml:L18113-L18122] |
| 123 | `src/digitalmodel/fatigue/workflow.py` | `_curve_summary` | [docs/registry/implementation-map.yaml:L18123-L18132] |
| 124 | `src/digitalmodel/fatigue/workflow.py` | `_curve_csv_path` | [docs/registry/implementation-map.yaml:L18133-L18144] |
| 125 | `src/digitalmodel/fatigue/workflow.py` | `_input_stem` | [docs/registry/implementation-map.yaml:L18145-L18154] |
| 126 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.router` | [docs/registry/implementation-map.yaml:L18155-L18167] |
| 127 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_sn_data` | [docs/registry/implementation-map.yaml:L18168-L18177] |
| 128 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_rainflow_cycles` | [docs/registry/implementation-map.yaml:L18178-L18187] |
| 129 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_timetrace` | [docs/registry/implementation-map.yaml:L18188-L18200] |
| 130 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.get_rainflow_from_timetrace` | [docs/registry/implementation-map.yaml:L18201-L18210] |
| 131 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | [docs/registry/implementation-map.yaml:L18211-L18220] |
| 132 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.router` | [docs/registry/implementation-map.yaml:L18221-L18232] |
| 133 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_lcf_timetrace_rainflow_and_fatigue_damage` | [docs/registry/implementation-map.yaml:L18233-L18244] |
| 134 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_fatigue_damage_output_df_for_timetrace` | [docs/registry/implementation-map.yaml:L18245-L18256] |
| 135 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_fatigue_df_definition_for_timetrace` | [docs/registry/implementation-map.yaml:L18257-L18266] |
| 136 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | [docs/registry/implementation-map.yaml:L18267-L18276] |
| 137 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `SNcurve.cycles_to_failure` | [docs/registry/implementation-map.yaml:L18277-L18286] |
| 138 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `RainflowFatigue.count_cycles` | [docs/registry/implementation-map.yaml:L18287-L18296] |
| 139 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `MinersRuleDamage.compute` | [docs/registry/implementation-map.yaml:L18297-L18306] |
| 140 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `ScatterDiagramFatigue.compute` | [docs/registry/implementation-map.yaml:L18307-L18316] |
| 141 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `compute_fatigue_life` | [docs/registry/implementation-map.yaml:L18317-L18326] |
| 142 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.add_well_analysis` | [docs/registry/implementation-map.yaml:L18327-L18336] |
| 143 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.set_well_order` | [docs/registry/implementation-map.yaml:L18337-L18346] |
| 144 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.create_flattened_section` | [docs/registry/implementation-map.yaml:L18347-L18356] |
| 145 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `WellCorrelation.add_well_data` | [docs/registry/implementation-map.yaml:L18357-L18366] |
| 146 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `CoreAnalysis.correlate_core_to_logs` | [docs/registry/implementation-map.yaml:L18367-L18376] |
| 147 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `ReservoirAnalysis.add_well_analysis` | [docs/registry/implementation-map.yaml:L18377-L18386] |
| 148 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `ReservoirAnalysis.run_comprehensive_analysis` | [docs/registry/implementation-map.yaml:L18387-L18396] |
| 149 | `src/digitalmodel/mooring_fatigue/workflow.py` | `router` | [docs/registry/implementation-map.yaml:L18397-L18410] |
| 150 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_evaluate_line` | [docs/registry/implementation-map.yaml:L18411-L18421] |
| 151 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_history_bins` | [docs/registry/implementation-map.yaml:L18422-L18431] |
| 152 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_curve_settings` | [docs/registry/implementation-map.yaml:L18432-L18441] |
| 153 | `src/digitalmodel/parametric/generate.py` | `_mooring_fatigue_damage` | [docs/registry/implementation-map.yaml:L18442-L18452] |
| 154 | `src/digitalmodel/parametric/generate.py` | `_free_span_utilisation` | [docs/registry/implementation-map.yaml:L18453-L18462] |
| 155 | `src/digitalmodel/parametric/generate.py` | `_span_rectification_utilisation` | [docs/registry/implementation-map.yaml:L18463-L18472] |
| 156 | `src/digitalmodel/parametric/refresh.py` | `<module>` | [docs/registry/implementation-map.yaml:L18473-L18481] |
| 157 | `src/digitalmodel/rao_spectral_fatigue/workflow.py` | `router` | [docs/registry/implementation-map.yaml:L18482-L18493] |
| 158 | `src/digitalmodel/riser_fatigue/touchdown.py` | `assess_touchdown_fatigue` | [docs/registry/implementation-map.yaml:L18494-L18503] |
| 159 | `src/digitalmodel/riser_fatigue/workflow.py` | `router` | [docs/registry/implementation-map.yaml:L18504-L18515] |
| 160 | `src/digitalmodel/riser_fatigue/workflow.py` | `_viv_damage` | [docs/registry/implementation-map.yaml:L18516-L18526] |
| 161 | `src/digitalmodel/riser_fatigue/workflow.py` | `_curve_settings` | [docs/registry/implementation-map.yaml:L18527-L18536] |
| 162 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `TimeSeriesComponentsAdapter.get_rainflow_count_from_time_series` | [docs/registry/implementation-map.yaml:L18537-L18546] |
| 163 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `FatigueAnalysisAdapter.get_rainflow_from_timetrace` | [docs/registry/implementation-map.yaml:L18547-L18556] |
| 164 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `FatigueAnalysisAdapter.damage_from_rainflow_cycles` | [docs/registry/implementation-map.yaml:L18557-L18566] |
| 165 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `OrcaFlexAdapter.RainflowHalfCycles` | [docs/registry/implementation-map.yaml:L18567-L18576] |
| 166 | `src/digitalmodel/signal_processing/signal_analysis/cli.py` | `rainflow_cmd` | [docs/registry/implementation-map.yaml:L18577-L18586] |
| 167 | `src/digitalmodel/signal_processing/signal_analysis/core/rainflow.py` | `RainflowCounter.count_cycles` | [docs/registry/implementation-map.yaml:L18587-L18596] |
| 168 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/curves.py` | `SNCurve._bilinear` | [docs/registry/implementation-map.yaml:L18597-L18606] |
| 169 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/curves.py` | `SNCurve.from_test_data` | [docs/registry/implementation-map.yaml:L18607-L18616] |
| 170 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator.calculate_damage` | [docs/registry/implementation-map.yaml:L18617-L18632] |
| 171 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._miners_rule` | [docs/registry/implementation-map.yaml:L18633-L18644] |
| 172 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._modified_miners_rule` | [docs/registry/implementation-map.yaml:L18645-L18655] |
| 173 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._nonlinear_damage` | [docs/registry/implementation-map.yaml:L18656-L18666] |
| 174 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._calculate_allowable_cycles` | [docs/registry/implementation-map.yaml:L18667-L18676] |
| 175 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._apply_mean_stress_correction` | [docs/registry/implementation-map.yaml:L18677-L18686] |
| 176 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._estimate_cycles_to_failure` | [docs/registry/implementation-map.yaml:L18687-L18696] |
| 177 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator.calculate_equivalent_stress` | [docs/registry/implementation-map.yaml:L18697-L18706] |
| 178 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `create_parser` | [docs/registry/implementation-map.yaml:L18707-L18716] |
| 179 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `build_config_from_args` | [docs/registry/implementation-map.yaml:L18717-L18726] |
| 180 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `main` | [docs/registry/implementation-map.yaml:L18727-L18739] |
| 181 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._analyze_column` | [docs/registry/implementation-map.yaml:L18740-L18749] |
| 182 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._generate_outputs` | [docs/registry/implementation-map.yaml:L18750-L18759] |
| 183 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._generate_summary` | [docs/registry/implementation-map.yaml:L18760-L18769] |
| 184 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/error_handling.py` | `SafeAnalyzer.safe_rainflow_analysis` | [docs/registry/implementation-map.yaml:L18770-L18779] |
| 185 | `src/digitalmodel/signal_processing/time_series/time_series_analysis.py` | `TimeSeriesAnalysis.router` | [docs/registry/implementation-map.yaml:L18780-L18789] |
| 186 | `src/digitalmodel/signal_processing/time_series/time_series_components.py` | `TimeSeriesComponents.count_cycles` | [docs/registry/implementation-map.yaml:L18790-L18799] |
| 187 | `src/digitalmodel/solvers/orcaflex/opp_time_series.py` | `OPPTimeSeries.get_histogram_object_from_orcaflex_run` | [docs/registry/implementation-map.yaml:L18800-L18809] |
| 188 | `src/digitalmodel/solvers/orcaflex/opp_time_series.py` | `OPPTimeSeries.get_detailed_histogram_df_columns` | [docs/registry/implementation-map.yaml:L18810-L18819] |
| 189 | `src/digitalmodel/solvers/orcaflex/opp_time_series_v2.py` | `OPPTimeSeriesV2.process_histograms` | [docs/registry/implementation-map.yaml:L18820-L18829] |
| 190 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.get_histogram_object_from_orcaflex_run` | [docs/registry/implementation-map.yaml:L18830-L18839] |
| 191 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.get_detailed_histogram_df_columns` | [docs/registry/implementation-map.yaml:L18840-L18849] |
| 192 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._perform_fatigue_analysis` | [docs/registry/implementation-map.yaml:L18850-L18861] |
| 193 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._parallel_fatigue_analysis` | [docs/registry/implementation-map.yaml:L18862-L18871] |
| 194 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._analyze_single_trace` | [docs/registry/implementation-map.yaml:L18872-L18881] |
| 195 | `src/digitalmodel/spectral_fatigue/workflow.py` | `router` | [docs/registry/implementation-map.yaml:L18882-L18893] |
| 196 | `src/digitalmodel/spectral_fatigue/workflow.py` | `_sn_curve` | [docs/registry/implementation-map.yaml:L18894-L18903] |
| 197 | `src/digitalmodel/structural/fatigue/__init__.py` | `quick_fatigue_analysis` | [docs/registry/implementation-map.yaml:L18904-L18917] |
| 198 | `src/digitalmodel/structural/fatigue/__init__.py` | `get_module_info` | [docs/registry/implementation-map.yaml:L18918-L18927] |
| 199 | `src/digitalmodel/structural/fatigue/analysis.py` | `<module>` | [docs/registry/implementation-map.yaml:L18928-L18936] |
| 200 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_sn_curve` | [docs/registry/implementation-map.yaml:L18937-L18947] |
| 201 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_damage_calculator` | [docs/registry/implementation-map.yaml:L18948-L18957] |
| 202 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_rainflow_counter` | [docs/registry/implementation-map.yaml:L18958-L18967] |
| 203 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_time_series` | [docs/registry/implementation-map.yaml:L18968-L18979] |
| 204 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_psd` | [docs/registry/implementation-map.yaml:L18980-L18989] |
| 205 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.calculate_high_stress_range_life` | [docs/registry/implementation-map.yaml:L18990-L18999] |
| 206 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_shear_data` | [docs/registry/implementation-map.yaml:L19000-L19009] |
| 207 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._perform_engineering_validation` | [docs/registry/implementation-map.yaml:L19010-L19019] |
| 208 | `src/digitalmodel/structural/fatigue/analysis.py` | `quick_time_domain_analysis` | [docs/registry/implementation-map.yaml:L19020-L19029] |
| 209 | `src/digitalmodel/structural/fatigue/analysis.py` | `quick_frequency_domain_analysis` | [docs/registry/implementation-map.yaml:L19030-L19039] |
| 210 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `<module>` | [docs/registry/implementation-map.yaml:L19040-L19048] |
| 211 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `LinearDamageAccumulation.calculate_damage` | [docs/registry/implementation-map.yaml:L19049-L19060] |
| 212 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `LinearDamageAccumulation._apply_mean_stress_correction` | [docs/registry/implementation-map.yaml:L19061-L19070] |
| 213 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `ModifiedMinersRule.calculate_damage` | [docs/registry/implementation-map.yaml:L19071-L19080] |
| 214 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation.calculate_damage` | [docs/registry/implementation-map.yaml:L19081-L19094] |
| 215 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._marco_starkey_damage` | [docs/registry/implementation-map.yaml:L19095-L19104] |
| 216 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._corten_dolan_damage` | [docs/registry/implementation-map.yaml:L19105-L19114] |
| 217 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._double_linear_damage` | [docs/registry/implementation-map.yaml:L19115-L19124] |
| 218 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `compare_damage_methods` | [docs/registry/implementation-map.yaml:L19125-L19134] |
| 219 | `src/digitalmodel/structural/fatigue/design_code_report.py` | `_build_curve` | [docs/registry/implementation-map.yaml:L19135-L19144] |
| 220 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `failure_probability` | [docs/registry/implementation-map.yaml:L19145-L19154] |
| 221 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `sensitivity_analysis` | [docs/registry/implementation-map.yaml:L19155-L19164] |
| 222 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `monte_carlo_fatigue` | [docs/registry/implementation-map.yaml:L19165-L19174] |
| 223 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `<module>` | [docs/registry/implementation-map.yaml:L19175-L19183] |
| 224 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `NarrowBandMethod.calculate_damage_rate` | [docs/registry/implementation-map.yaml:L19184-L19193] |
| 225 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `DirlikMethod.calculate_damage_rate` | [docs/registry/implementation-map.yaml:L19194-L19203] |
| 226 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `TovoBenasciuttiMethod.calculate_damage_rate` | [docs/registry/implementation-map.yaml:L19204-L19213] |
| 227 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `SingleMomentMethod.calculate_damage_rate` | [docs/registry/implementation-map.yaml:L19214-L19223] |
| 228 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `compare_frequency_methods` | [docs/registry/implementation-map.yaml:L19224-L19233] |
| 229 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `run_sweep` | [docs/registry/implementation-map.yaml:L19234-L19243] |
| 230 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `compute_sensitivity._single_damage` | [docs/registry/implementation-map.yaml:L19244-L19253] |
| 231 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `_build_scatter_json` | [docs/registry/implementation-map.yaml:L19254-L19263] |
| 232 | `src/digitalmodel/structural/fatigue/rainflow.py` | `<module>` | [docs/registry/implementation-map.yaml:L19264-L19272] |
| 233 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter.count_cycles` | [docs/registry/implementation-map.yaml:L19273-L19284] |
| 234 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter._rainflow_count` | [docs/registry/implementation-map.yaml:L19285-L19294] |
| 235 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter.extract_cycles_with_means` | [docs/registry/implementation-map.yaml:L19295-L19306] |
| 236 | `src/digitalmodel/structural/fatigue/rainflow.py` | `rainflow_count` | [docs/registry/implementation-map.yaml:L19307-L19316] |
| 237 | `src/digitalmodel/structural/fatigue/rainflow.py` | `rainflow_with_means` | [docs/registry/implementation-map.yaml:L19317-L19326] |
| 238 | `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `scatter_fatigue_damage` | [docs/registry/implementation-map.yaml:L19327-L19336] |
| 239 | `src/digitalmodel/structural/fatigue/skill.py` | `fatigue_assessment` | [docs/registry/implementation-map.yaml:L19337-L19348] |
| 240 | `src/digitalmodel/structural/fatigue/sn_curve_plotter.py` | `main` | [docs/registry/implementation-map.yaml:L19349-L19358] |
| 241 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `<module>` | [docs/registry/implementation-map.yaml:L19359-L19367] |
| 242 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves._ensure_loaded` | [docs/registry/implementation-map.yaml:L19368-L19377] |
| 243 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.register_custom_curve` | [docs/registry/implementation-map.yaml:L19378-L19387] |
| 244 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_dnv_c203_curve` | [docs/registry/implementation-map.yaml:L19388-L19397] |
| 245 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_curve` | [docs/registry/implementation-map.yaml:L19398-L19407] |
| 246 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_multislope_curve` | [docs/registry/implementation-map.yaml:L19408-L19417] |
| 247 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `ThicknessCorrection.apply_thickness_effect` | [docs/registry/implementation-map.yaml:L19418-L19427] |
| 248 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `SNDataFitting.fit_power_law` | [docs/registry/implementation-map.yaml:L19428-L19437] |
| 249 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_dnv_curve` | [docs/registry/implementation-map.yaml:L19438-L19447] |
| 250 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_api_curve` | [docs/registry/implementation-map.yaml:L19448-L19457] |
| 251 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_bs_curve` | [docs/registry/implementation-map.yaml:L19458-L19467] |
| 252 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `_dnv_curves_from_c203` | [docs/registry/implementation-map.yaml:L19468-L19477] |
| 253 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_dnv_seawater_cp` | [docs/registry/implementation-map.yaml:L19478-L19487] |
| 254 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_dnv_seawater_free` | [docs/registry/implementation-map.yaml:L19488-L19497] |
| 255 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_chain_seawater_cp` | [docs/registry/implementation-map.yaml:L19498-L19507] |
| 256 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `pipeline_girth_weld` | [docs/registry/implementation-map.yaml:L19508-L19520] |
| 257 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `scr_touchdown` | [docs/registry/implementation-map.yaml:L19521-L19533] |
| 258 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `mooring_chain` | [docs/registry/implementation-map.yaml:L19534-L19546] |
| 259 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_compute_example` | [docs/registry/implementation-map.yaml:L19547-L19556] |
| 260 | `src/digitalmodel/structural/fatigue_apps/__main__.py` | `run_analysis` | [docs/registry/implementation-map.yaml:L19557-L19568] |
| 261 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `SNCurve._load_sn_parameters` | [docs/registry/implementation-map.yaml:L19569-L19578] |
| 262 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `SNCurve.calculate_cycles_to_failure` | [docs/registry/implementation-map.yaml:L19579-L19588] |
| 263 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `FatigueDamageCalculator.calculate_damage` | [docs/registry/implementation-map.yaml:L19589-L19598] |
| 264 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `main` | [docs/registry/implementation-map.yaml:L19599-L19608] |
| 265 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `<module>` | [docs/registry/implementation-map.yaml:L19609-L19617] |
| 266 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `SNCurveParameters.a1` | [docs/registry/implementation-map.yaml:L19618-L19627] |
| 267 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `SNCurveParameters.a2` | [docs/registry/implementation-map.yaml:L19628-L19637] |
| 268 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.cycles_to_failure` | [docs/registry/implementation-map.yaml:L19638-L19647] |
| 269 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.calculate_damage` | [docs/registry/implementation-map.yaml:L19648-L19659] |
| 270 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.export_results` | [docs/registry/implementation-map.yaml:L19660-L19669] |
| 271 | `src/digitalmodel/structural/fatigue_apps/file_namer.py` | `FatigueFileNamer.get_rainflow_filename` | [docs/registry/implementation-map.yaml:L19670-L19679] |
| 272 | `src/digitalmodel/structural/fatigue_apps/file_namer.py` | `<module>` | [docs/registry/implementation-map.yaml:L19680-L19688] |
| 273 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `IntegratedFatigueProcessor.process_fatigue_condition` | [docs/registry/implementation-map.yaml:L19689-L19698] |
| 274 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `main` | [docs/registry/implementation-map.yaml:L19699-L19708] |
| 275 | `src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py` | `IntegratedFatigueProcessorWithNaming._process_condition_base` | [docs/registry/implementation-map.yaml:L19709-L19718] |
| 276 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `<module>` | [docs/registry/implementation-map.yaml:L19719-L19727] |
| 277 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter.count_cycles` | [docs/registry/implementation-map.yaml:L19728-L19739] |
| 278 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter._rainflow_algorithm` | [docs/registry/implementation-map.yaml:L19740-L19749] |
| 279 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter.export_results` | [docs/registry/implementation-map.yaml:L19750-L19759] |
| 280 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `rainflow_count` | [docs/registry/implementation-map.yaml:L19760-L19769] |
| 281 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.process_time_series` | [docs/registry/implementation-map.yaml:L19770-L19779] |
| 282 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.process_batch` | [docs/registry/implementation-map.yaml:L19780-L19791] |
| 283 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.save_cycle_data` | [docs/registry/implementation-map.yaml:L19792-L19801] |
| 284 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `main` | [docs/registry/implementation-map.yaml:L19802-L19811] |
| 285 | `src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py` | `ReferenceSeaStateProcessor.process_fatigue_condition` | [docs/registry/implementation-map.yaml:L19812-L19821] |
| 286 | `src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py` | `TensionStressConverter.process_batch` | [docs/registry/implementation-map.yaml:L19822-L19831] |
| 287 | `src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py` | `main` | [docs/registry/implementation-map.yaml:L19832-L19841] |
| 288 | `src/digitalmodel/structural/fatigue_apps/visualizer.py` | `generate_all_visualizations` | [docs/registry/implementation-map.yaml:L19842-L19851] |
| 289 | `src/digitalmodel/structural/spectral_fatigue.py` | `narrow_band_damage` | [docs/registry/implementation-map.yaml:L19852-L19861] |
| 290 | `src/digitalmodel/subsea/connectors/fatigue.py` | `assess_jumper_fatigue` | [docs/registry/implementation-map.yaml:L19862-L19873] |
| 291 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `_dnv_params` | [docs/registry/implementation-map.yaml:L19874-L19883] |
| 292 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `get_sn_curve` | [docs/registry/implementation-map.yaml:L19884-L19895] |
| 293 | `src/digitalmodel/subsea/pipeline/free_span/span_fatigue_damage.py` | `SpanFatigueDamage._load_sn_curve` | [docs/registry/implementation-map.yaml:L19896-L19907] |
| 294 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.calculate_damage` | [docs/registry/implementation-map.yaml:L19908-L19917] |
| 295 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.calculate_fatigue_life` | [docs/registry/implementation-map.yaml:L19918-L19927] |
| 296 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.analyze_viv_fatigue` | [docs/registry/implementation-map.yaml:L19928-L19940] |
| 297 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.cumulative_damage` | [docs/registry/implementation-map.yaml:L19941-L19952] |
| 298 | `src/digitalmodel/viv_screening/inputs.py` | `parse_inputs` | [docs/registry/implementation-map.yaml:L19953-L19962] |
| 299 | `src/digitalmodel/viv_screening/workflow.py` | `router` | [docs/registry/implementation-map.yaml:L19963-L19976] |
| 300 | `src/digitalmodel/viv_screening/workflow.py` | `_damage_rate` | [docs/registry/implementation-map.yaml:L19977-L19987] |
| 301 | `src/digitalmodel/viv_screening/workflow.py` | `_provenance` | [docs/registry/implementation-map.yaml:L19988-L19997] |
| 302 | `src/digitalmodel/web/digitaltwinfeed/app.py` | `page` | [docs/registry/implementation-map.yaml:L19998-L20007] |
| 303 | `tests/ansys/padeye_pressure_bundle.py` | `_check_receipt` | [docs/registry/implementation-map.yaml:L20008-L20017] |
| 304 | `tests/ansys/padeye_pressure_bundle.py` | `verify_pressure_bundle` | [docs/registry/implementation-map.yaml:L20018-L20029] |
| 305 | `tests/ansys/test_fatigue_postprocessor.py` | `TestThicknessCorrection.test_known_correction` | [docs/registry/implementation-map.yaml:L20030-L20039] |
| 306 | `tests/ansys/test_padeye_pressure_bundle.py` | `bundle` | [docs/registry/implementation-map.yaml:L20040-L20049] |
| 307 | `tests/fatigue/test_assessment.py` | `histogram_input` | [docs/registry/implementation-map.yaml:L20050-L20061] |
| 308 | `tests/fatigue/test_c203_consumers.py` | `_span_input` | [docs/registry/implementation-map.yaml:L20062-L20071] |
| 309 | `tests/fatigue/test_c203_sn_values.py` | `_non_dnv_bilinear` | [docs/registry/implementation-map.yaml:L20072-L20081] |
| 310 | `tests/fatigue/test_c203_sn_values.py` | `test_non_dnv_bilinear_switch_stays_at_endurance_limit` | [docs/registry/implementation-map.yaml:L20082-L20095] |
| 311 | `tests/fatigue/test_c203_variable_amplitude.py` | `_signal_e` | [docs/registry/implementation-map.yaml:L20096-L20105] |
| 312 | `tests/fatigue/test_c203_variable_amplitude.py` | `_user_curve` | [docs/registry/implementation-map.yaml:L20106-L20115] |
| 313 | `tests/fatigue/test_c203_variable_amplitude.py` | `_span_input` | [docs/registry/implementation-map.yaml:L20116-L20125] |
| 314 | `tests/fatigue/test_c203_variable_amplitude.py` | `_fresh_standard_curves` | [docs/registry/implementation-map.yaml:L20126-L20135] |
| 315 | `tests/fatigue/test_c203_variable_amplitude.py` | `_data_dir` | [docs/registry/implementation-map.yaml:L20136-L20145] |
| 316 | `tests/fatigue/test_damage.py` | `f_curve` | [docs/registry/implementation-map.yaml:L20146-L20156] |
| 317 | `tests/fatigue/test_damage.py` | `TestThicknessCorrection.test_correction_above_ref` | [docs/registry/implementation-map.yaml:L20157-L20166] |
| 318 | `tests/fatigue/test_damage.py` | `TestThicknessCorrection.test_correction_formula` | [docs/registry/implementation-map.yaml:L20167-L20176] |
| 319 | `tests/fatigue/test_fatg.py` | `TestAgainstTransliterationSweep.test_all_presets_and_curves` | [docs/registry/implementation-map.yaml:L20177-L20188] |
| 320 | `tests/fatigue/test_fatg_workflow.py` | `_cfg` | [docs/registry/implementation-map.yaml:L20189-L20198] |
| 321 | `tests/fatigue/test_fatg_workflow.py` | `_library_reference` | [docs/registry/implementation-map.yaml:L20199-L20209] |
| 322 | `tests/fatigue/test_fatigue_reporting.py` | `_sample_locations` | [docs/registry/implementation-map.yaml:L20210-L20219] |
| 323 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls` | [docs/registry/implementation-map.yaml:L20220-L20232] |
| 324 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls.calm_scatter` | [docs/registry/implementation-map.yaml:L20233-L20242] |
| 325 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls.fapps_batch` | [docs/registry/implementation-map.yaml:L20243-L20252] |
| 326 | `tests/fatigue/test_sn_curves.py` | `TestGetSNCurve.test_f_curve_at_100mpa` | [docs/registry/implementation-map.yaml:L20253-L20263] |
| 327 | `tests/fatigue/test_sn_curves.py` | `TestGetSNCurve.test_d_curve_at_100mpa` | [docs/registry/implementation-map.yaml:L20264-L20274] |
| 328 | `tests/fatigue/test_weld_fatigue_quickcheck.py` | `_hand_N` | [docs/registry/implementation-map.yaml:L20275-L20284] |
| 329 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_cycles_to_failure_chain_known_value` | [docs/registry/implementation-map.yaml:L20285-L20294] |
| 330 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_cycles_to_failure_wire_rope_known_value` | [docs/registry/implementation-map.yaml:L20295-L20304] |
| 331 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_custom_sn_curve` | [docs/registry/implementation-map.yaml:L20305-L20314] |
| 332 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestMinersRuleDamage.test_single_range_known_damage` | [docs/registry/implementation-map.yaml:L20315-L20324] |
| 333 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestMinersRuleDamage.test_damage_additivity` | [docs/registry/implementation-map.yaml:L20325-L20334] |
| 334 | `tests/parametric/test_atlas.py` | `_small_atlas` | [docs/registry/implementation-map.yaml:L20335-L20344] |
| 335 | `tests/signal_processing/signal_analysis/fatigue_analysis_reference/process_fatigue_final.py` | `process_single_test_file` | [docs/registry/implementation-map.yaml:L20345-L20354] |
| 336 | `tests/signal_processing/signal_analysis/reference_fatigue_processor.py` | `process_single_test_file` | [docs/registry/implementation-map.yaml:L20355-L20364] |
| 337 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `<module>` | [docs/registry/implementation-map.yaml:L20365-L20373] |
| 338 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `analyze_signal` | [docs/registry/implementation-map.yaml:L20374-L20383] |
| 339 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `main` | [docs/registry/implementation-map.yaml:L20384-L20395] |
| 340 | `tests/signal_processing/signal_analysis/test_orcaflex_tension_analysis.py` | `OrcaFlexTensionAnalyzer.analyze_tension` | [docs/registry/implementation-map.yaml:L20396-L20405] |
| 341 | `tests/signal_processing/signal_analysis/test_orcaflex_tension_analysis.py` | `OrcaFlexTensionAnalyzer.export_results` | [docs/registry/implementation-map.yaml:L20406-L20415] |
| 342 | `tests/signal_processing/signal_analysis/test_rainflow_timeseries1.py` | `run_rainflow_for_timeseries` | [docs/registry/implementation-map.yaml:L20416-L20425] |
| 343 | `tests/signal_processing/signal_analysis/test_rainflow_timeseries1.py` | `test_rainflow_for_timeseries` | [docs/registry/implementation-map.yaml:L20426-L20437] |
| 344 | `tests/signal_processing/signal_analysis/validate_module.py` | `main` | [docs/registry/implementation-map.yaml:L20438-L20452] |
| 345 | `tests/solvers/test_integration_solvers.py` | `mock_solver_factory.MockSolverFactory.create_fatigue_solver` | [docs/registry/implementation-map.yaml:L20453-L20462] |
| 346 | `tests/structural/fatigue/conftest.py` | `simple_sn_curve` | [docs/registry/implementation-map.yaml:L20463-L20472] |
| 347 | `tests/structural/fatigue/test_damage_accumulation.py` | `dnv_d_curve` | [docs/registry/implementation-map.yaml:L20473-L20482] |
| 348 | `tests/structural/fatigue/test_damage_accumulation.py` | `simple_curve` | [docs/registry/implementation-map.yaml:L20483-L20492] |
| 349 | `tests/structural/fatigue/test_damage_accumulation.py` | `curve_with_material` | [docs/registry/implementation-map.yaml:L20493-L20502] |
| 350 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestSNCurveMigration.test_multislope_curve_implementation` | [docs/registry/implementation-map.yaml:L20503-L20512] |
| 351 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestSNCurveMigration.test_curve_continuity` | [docs/registry/implementation-map.yaml:L20513-L20522] |
| 352 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestFrequencyDomainMigration.test_basic_frequency_analysis` | [docs/registry/implementation-map.yaml:L20523-L20532] |
| 353 | `tests/structural/fatigue/test_fatigue_reliability.py` | `simple_sn` | [docs/registry/implementation-map.yaml:L20533-L20542] |
| 354 | `tests/structural/fatigue/test_fatigue_reliability.py` | `mild_sn` | [docs/registry/implementation-map.yaml:L20543-L20552] |
| 355 | `tests/structural/fatigue/test_fatigue_skill.py` | `_simple_input` | [docs/registry/implementation-map.yaml:L20553-L20562] |
| 356 | `tests/structural/fatigue/test_fatigue_skill.py` | `TestOffshoreScenario.test_high_cycle_low_stress_result` | [docs/registry/implementation-map.yaml:L20563-L20574] |
| 357 | `tests/structural/fatigue/test_frequency_domain.py` | `simple_sn_curve` | [docs/registry/implementation-map.yaml:L20575-L20584] |
| 358 | `tests/structural/fatigue/test_rainflow.py` | `default_counter` | [docs/registry/implementation-map.yaml:L20585-L20594] |
| 359 | `tests/structural/fatigue/test_scatter_fatigue.py` | `simple_sn` | [docs/registry/implementation-map.yaml:L20595-L20604] |
| 360 | `tests/structural/fatigue/test_scatter_fatigue.py` | `TestFatigueLife.test_fatigue_life_inverse_of_damage` | [docs/registry/implementation-map.yaml:L20605-L20616] |
| 361 | `tests/structural/fatigue/test_sn_curves.py` | `simple_power_law` | [docs/registry/implementation-map.yaml:L20617-L20626] |
| 362 | `tests/structural/fatigue/test_sn_curves.py` | `power_law_with_limit` | [docs/registry/implementation-map.yaml:L20627-L20636] |
| 363 | `tests/structural/fatigue/test_sn_curves.py` | `bilinear_curve` | [docs/registry/implementation-map.yaml:L20637-L20646] |
| 364 | `tests/structural/fatigue/test_sn_curves.py` | `dnv_d_curve` | [docs/registry/implementation-map.yaml:L20647-L20656] |
| 365 | `tests/structural/fatigue/test_sn_curves.py` | `TestBS7608RevisedCurveParameters.test_bs7608_revised_d_known_n_at_50mpa` | [docs/registry/implementation-map.yaml:L20657-L20666] |
| 366 | `tests/structural/fatigue/test_sn_curves.py` | `TestISO19902CurveParameters.test_iso19902_f_air_known_n_at_100mpa` | [docs/registry/implementation-map.yaml:L20667-L20676] |
| 367 | `tests/structural/fatigue/test_sn_curves.py` | `TestISO19902CurveParameters.test_iso19902_f2_sw_cp_known_n_at_50mpa` | [docs/registry/implementation-map.yaml:L20677-L20686] |
| 368 | `tests/structural/fatigue/test_sn_curves_yaml.py` | `_all_single_slope_cases` | [docs/registry/implementation-map.yaml:L20687-L20696] |
| 369 | `tests/structural/fatigue/test_sn_curves_yaml.py` | `_all_multislope_cases` | [docs/registry/implementation-map.yaml:L20697-L20706] |
| 370 | `tests/structural/fatigue_analysis/conftest.py` | `sn_curve_data` | [docs/registry/implementation-map.yaml:L20707-L20716] |
| 371 | `tests/structural/fatigue_analysis/test_fatigue_analysis_sn.py` | `run_fatigue_analysis` | [docs/registry/implementation-map.yaml:L20717-L20727] |
| 372 | `tests/structural/fatigue_analysis/test_fatigue_analysis_timetrace.py` | `run_fatigue_analysis` | [docs/registry/implementation-map.yaml:L20728-L20738] |
| 373 | `tests/subsea/pipeline/test_free_span_f105.py` | `ref_input` | [docs/registry/implementation-map.yaml:L20739-L20748] |
| 374 | `tests/subsea/viv_analysis/test_viv_analysis_unit.py` | `TestVIVFatigueCalculator.test_cumulative_damage` | [docs/registry/implementation-map.yaml:L20749-L20758] |
| 375 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_orcaflex_post_process_mock` | [docs/registry/implementation-map.yaml:L20759-L20768] |
| 376 | `tests/test_automation/scripts/comprehensive_fix_campaign.py` | `ComprehensiveTestFixCampaign.generate_mock_data` | [docs/registry/implementation-map.yaml:L20769-L20778] |
| 377 | `tests/test_complete_fatigue_pipeline.py` | `process_configuration` | [docs/registry/implementation-map.yaml:L20779-L20788] |
| 378 | `tests/test_complete_fatigue_pipeline.py` | `main` | [docs/registry/implementation-map.yaml:L20789-L20800] |
| 379 | `tests/test_fatigue_basic.py` | `run_all_tests` | [docs/registry/implementation-map.yaml:L20801-L20810] |
| 380 | `tests/test_rainflow_with_effective_tension.py` | `process_effective_tension_file` | [docs/registry/implementation-map.yaml:L20811-L20820] |
| 381 | `tests/test_rainflow_with_effective_tension.py` | `main` | [docs/registry/implementation-map.yaml:L20821-L20831] |
| 382 | `tests/test_reservoir_framework.py` | `run_performance_tests` | [docs/registry/implementation-map.yaml:L20832-L20841] |
| 383 | `tests/test_spectral_fatigue_validation.py` | `TestSNCurveDNVReference.test_dnv_d_curve_parameters` | [docs/registry/implementation-map.yaml:L20842-L20853] |
| 384 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | [docs/registry/implementation-map.yaml:L20854-L20864] |
| 385 | `tests/workflows/test_durable_workflows.py` | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | [docs/registry/implementation-map.yaml:L20865-L20875] |
| 386 | `tests/workflows/test_durable_workflows.py` | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | [docs/registry/implementation-map.yaml:L20876-L20886] |
| 387 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `PowerLawSNCurve.get_allowable_cycles` | [docs/registry/implementation-map.yaml:L20887-L20897] |
| 388 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `PowerLawSNCurve.get_stress_range` | [docs/registry/implementation-map.yaml:L20898-L20907] |
| 389 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `BilinearSNCurve.get_allowable_cycles` | [docs/registry/implementation-map.yaml:L20908-L20917] |
| 390 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `BilinearSNCurve.get_stress_range` | [docs/registry/implementation-map.yaml:L20918-L20928] |
| 391 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `BilinearSNCurve.get_allowable_cycles` | [docs/registry/implementation-map.yaml:L20929-L20938] |
| 392 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `BilinearSNCurve.get_stress_range` | [docs/registry/implementation-map.yaml:L20939-L20948] |

## 10. Coverage and open items

Coverage records below identify the source inspected, not a claim that roadmap promises were implemented.

| Read scope | Evidence at code pin | Disposition |
|---|---|---|
| All package source: configuration driver, router, main component and alternate component; no subordinate Python package | [src/digitalmodel/signal_processing/time_series/time_series.py:L1-L87] [src/digitalmodel/signal_processing/time_series/time_series_analysis.py:L1-L53] [src/digitalmodel/signal_processing/time_series/time_series_components.py:L1-L738] [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L1-L574] | Section 6 records sweep sites and additional pitfalls; issue pending (1), issue pending (2), issue pending (3) |
| Test discovery: package's own test folder has no tests; dedicated repository folder has four test files, seven component parameterizations and three mock-wrapper nodes; fixtures are definitions, not extra tests | [tests/signal_processing/time_series/test_component_values.py:L25-L149] [tests/signal_processing/time_series/test_sample_fft.py:L13-L41] [tests/signal_processing/time_series/test_window_fft.py:L13-L41] [tests/signal_processing/time_series/test_csv_window_fft.py:L13-L41] [tests/signal_processing/time_series/conftest.py:L1-L39] | Recounted 10 nodes; baseline average defects retained under #3350 |
| Durable workflow registry, synthetic input and executing test | [docs/registry/workflows.yaml:L231-L239] [examples/workflows/time-series/input.yml:L1-L33] [tests/workflows/test_durable_workflows.py:L55-L100] [tests/workflows/test_durable_workflows.py:L462-L467] | One additional real execution; #711 is an onboarding lead, no confirmed defect for this node |
| Domain registry and operator map | [docs/registry/module-routing.yaml:L438-L447] [docs/maps/digitalmodel-operator-map.md:L66-L66] | Add time_series as a sibling of signal_analysis, family EC |
| Module README under docs | [docs/domains/signal_processing/time_series/README.md:L1-L17] | Placeholder API and usage, not method evidence; issue pending (3) |
| Synthetic value explanation and executable worked example | [docs/domains/signal_processing/synthetic-value-checks.md:L49-L95] [docs/domains/signal_processing/synthetic-value-checks.md:L149-L185] [docs/domains/examples/signal_value_example.py:L17-L39] | The executable example uses the replacement analyzer; legacy qualification comes from the direct component tests; #3350 |
| Migration plan | [docs/domains/migration/signal_analysis_migration_plan.md:L89-L109] | Old example calls an API absent from this class; roadmap figures are not adopted; issue pending (3) |
| Solver post-processing migration guide, inspected as text only | [docs/domains/migration/orcaflex_migration_guide.md:L19-L35] | Same stale old-API call; no solver/API import or execution; issue pending (3) |
| Engine testing plan | [docs/domains/testing/engine_test_implementation_plan.md:L607-L627] | Mock routing recipe, not independent numerical evidence; #3350 |
| Implementation-map documentation | [docs/registry/implementation-map.md:L25-L25] | Family discovery lead; section 9 separately pins the actual map data; issue pending (4) |
| Legacy engineering-flow catalog | [docs/migration/legacy-engineering-flow-catalog.md:L59-L62] | Legacy path lead, not proof of present functionality; issue pending (3) |
| Domain index | [docs/domains/domains_index.md:L288-L291] | Historical time-series folder spelling; verify navigation in documentation follow-up; issue pending (3) |
| Identifying documentation discovery | One discovery file with an identifying name is omitted from the public path list | Codenaming and public discovery follow-up; issue pending (4) |

Discovery used `rg -l 'signal_processing[./]time_series|time_series_analysis|time-series' docs --glob '*.md'`; generic time-series mentions unrelated to this module were excluded after inspecting their matched paragraphs. Identifying discovery paths are not reproduced here; issue pending (4).

| Open item | Responsible role | Tracking | Evidence or missing qualification |
|---|---|---|---|
| Silent error/default sites | Module maintainer | issue pending (1), #60 | Add explicit failure/status and empty-output tests for section-6 sites; [src/digitalmodel/signal_processing/time_series/time_series_components.py:L425-L432] [src/digitalmodel/signal_processing/time_series/time_series_components_bop.py:L18-L50] |
| Units and numerical edge contracts | Module maintainer | issue pending (2) | Complete seconds extraction, interval/angle contracts, masking and discarded-window evidence; [src/digitalmodel/signal_processing/time_series/time_series_components.py:L681-L738] |
| Entry points, statistics and navigation | Module maintainer | issue pending (3) | Correct undefined names, incompatible signatures and extra row values; stale migration-guide target named by deprecation text was not found at the code pin; [src/digitalmodel/signal_processing/time_series/time_series_analysis.py:L34-L37] [src/digitalmodel/signal_processing/time_series/time_series_components.py:L9-L9] [src/digitalmodel/signal_processing/time_series/time_series_components.py:L575-L589] |
| Average defect and warning boundary | Module maintainer | #3350 | F1/F2 fail independently derived values; V5 depends on current pandas warning behavior; [tests/signal_processing/time_series/test_component_values.py:L105-L149] |
| Map expansion, public discovery and evidence registration | Module maintainer and family quality manager | issue pending (4) | Add section-3 families to map before counting; record shared-log and role appointments; [src/digitalmodel/signal_processing/time_series/time_series_components.py:L263-L591] [src/digitalmodel/signal_processing/time_series/time_series_components.py:L659-L738] |

UNVERIFIED: numerical accuracy of the default external rainflow counter and compliance with ASTM E1049 are not established by these examples; an independent counter example and citation-sidecar decision remain open, issue pending (4). [src/digitalmodel/signal_processing/time_series/time_series_components.py:L644-L657] [tests/signal_processing/time_series/test_component_values.py:L47-L149]

UNVERIFIED: physical-unit conventions, origin/sign conventions of custom displacement and acceptance on measured signals are not established by the synthetic FFT evidence; issue pending (2). [src/digitalmodel/signal_processing/time_series/time_series_components.py:L681-L707] [tests/signal_processing/time_series/test_component_values.py:L47-L149]

The appointment/shared-log uncertainty from section 8 is tracked by issue pending (4). The calculation-output citation contract was readable; this change introduces no standards-derived constants, no module code and no private values or sidecar. This pack remains a draft pending independent review; AQG clearance and gate signatures are not claimed.
