# Mooring analysis knowledge pack

| Header | Value |
| --- | --- |
| Module | solvers/orcaflex/mooring_analysis |
| Repository | digitalmodel |
| Package path | `src/digitalmodel/solvers/orcaflex/mooring_analysis` |
| Pack revision | 0 |
| Code revision | `c099a5e1317e6bed63d876252d87cbb12c774372` |
| Analysis family | OrcaFlex riser and mooring; registry family MR |
| Quality role | OrcaFlex riser/mooring quality manager |
| Status | DRAFT |

## 1. Purpose and scope

`NaturalPeriodAnalyzer` calculates periods from a supplied stiffness matrix and mass properties; its convenience function estimates inertias rather than reading a solver model. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L137-L185] [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L331-L373]

The comprehensive subpackage parses CSV pretension, stiffness and fender data, while its directory/file orchestration returns empty results and its comparison orchestration returns empty comparison fields. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L34-L93] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L32-L92] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L29-L79] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/analyzer.py:L42-L96]

The existing solvers domain has maturity `beta`; this is a domain classification, not evidence of completed engineering validation. [docs/registry/module-routing.yaml:L354-L359]

UNVERIFIED: U1 — supported solver version, geometry, water-depth and environmental applicability envelopes are not established; do not treat this draft as a solver gate clearance (issue pending (2)).

## 2. Inputs and outputs

### Inputs

| Name and route | Unit | Range / default | Source and evidence |
| --- | --- | --- | --- |
| `VesselMassProperties.mass`, inertias, centre coordinates | tonnes; tonne m²; m | Required mass/inertias; centre coordinates default zero; added-mass multiplier has a fixed default | Dataclass; no physical range guard in setter. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L14-L24] [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L113-L122] |
| `set_stiffness_matrix(stiffness)` | UNVERIFIED U2: caller must declare N/m versus kN/m, rotation units and origin | Exactly 6 × 6 entries; only shape checked | Caller array. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L124-L135] |
| Pretension CSV | kN tension/force; m length; percent deviation; kN EA | Required columns listed in parser; optional EA is None when absent; no finite-value or separator contract | `pandas.read_csv`, scalar conversions, list parser. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L46-L104] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/models.py:L15-L28] |
| Stiffness CSV | kN/m translations and cross terms; kN forces; dimensionless cosines | `ObjectName`, `k_x`, `k_y`, `k_z` required; remaining numeric fields default zero | CSV parser and data model. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L44-L92] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/models.py:L89-L101] |
| Fender CSV | kN force; m compression; UNVERIFIED U3: coordinate unit and contact duration basis | Identifier required; missing coordinates, statistics and duration default zero; compression defaults None | CSV parser; duration model says seconds or percentage. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L41-L79] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/models.py:L162-L172] |
| `AnalysisConfig` / YAML | Paths and dimensionless options; mass tonnes; capacity kN | CSV glob and recursion defaults; convergence and fender thresholds are configurable; no edition selection | Configuration dataclasses and YAML conversion. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/config.py:L9-L95] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/config.py:L97-L172] |
| Filename context | Model documents depth in m and return period in years | Unmatched or malformed numeric tokens become None; depth parser converts digits directly | Regex extraction and parsers. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/context_extractor.py:L23-L89] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/models.py:L262-L273] |

### Outputs

| Name and route | Unit | Range / default | Evidence |
| --- | --- | --- | --- |
| `NaturalPeriods` | s | Six DOFs; nonpositive diagonal mass/stiffness or unassigned modes yield infinity | [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L27-L46] [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L187-L203] [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L244-L280] |
| `calculate_frequency_response` | Inverse stiffness under unit excitation; UNVERIFIED U2 for rotational normalisation | Frequency grid in Hz; singular matrix gives a fixed large response | [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L282-L328] |
| `PretensionMetrics`, `ConvergenceStatus` | Percent, kN, dimensionless measures; adjustments % | CSV deviation drives convergence; adjustment and iteration estimates are placeholders | [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L106-L148] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L170-L210] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/models.py:L58-L82] |
| `compute_stiffness_matrix`, `estimate_natural_periods` | kN/m translations; s periods | Translation/cross-term sums; optional rotational diagonals; rotational periods are zero placeholders | [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L94-L130] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L194-L225] |
| `FenderMetrics` | Percent utilisation/margin/sharing; kN statistics | Unknown or nonpositive capacity yields zero utilisation; contact duration copied into percentages | [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L81-L151] |
| Summaries, reports and figures | Text and image files | Compliance flags and overall success wording are unconditional; report bodies and plots are placeholders | [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/summarizer.py:L59-L79] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/summarizer.py:L122-L136] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/report_generator.py:L24-L61] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/visualizer.py:L22-L57] |

The pretension model labels force components Gx/Gy/Gz; natural-period ordering is surge, sway, heave, roll, pitch, yaw. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/models.py:L19-L27] [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L187-L198]

UNVERIFIED: U2 — stiffness unit, rotational convention, frame origin, sign, phase and inter-module conversion contract require confirmation; the two period routes use different stiffness scaling (issue pending (2)).

UNVERIFIED: U3 — fender location unit and seconds-to-percent contact conversion require a declared input schema (issue pending (1)).

## 3. Method and governing standards

| Method | Implementing definition and limits |
| --- | --- |
| Diagonal periods | `_calculate_uncoupled_periods`: angular frequency from square root of diagonal stiffness / mass; period from full-circle angle / frequency; no damping or hydrostatic terms added here. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L187-L203] |
| Coupled periods | `_calculate_coupled_periods`: eigenproblem of inverse mass times stiffness; absolute eigenvalues precede square root, optional Hz filtering, then greedy eigenvector-dominance assignment; linear-algebra failure falls back to uncoupled periods. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L205-L280] |
| Frequency response | `calculate_frequency_response`: diagonal magnitudes of inverse dynamic stiffness under unit excitation; singularity substitutes a fixed large value. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L298-L328] |
| Convenience inertia estimate | `calculate_natural_periods_from_stiffness`: type-dependent empirical inertia and length estimates with a fallback type; no independent benchmark cited in the implementation. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L349-L373] |
| Pretension | `parse_pretension_csv` and `calculate_metrics`: compare supplied absolute deviation percent to configured tolerance; descriptive statistics do not recalculate equilibrium; `assess_convergence` uses categorical cutoffs and placeholder adjustments. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L64-L82] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L115-L148] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L179-L210] |
| Stiffness | `compute_stiffness_matrix` sums columns and inserts symmetric translational couplings; `analyze_characteristics` uses translational eigenpairs, whole-matrix conditioning and ratios; `estimate_natural_periods` computes translations separately and returns placeholder rotations. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L103-L192] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L194-L225] |
| Fender utilisation | `calculate_utilization` divides stored maximum force by capacity, reports load shares and compares configured thresholds; statistics are taken from supplied maximum forces. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L100-L151] |
| Group/context/report helpers | Grouping uses filename regexes; comparison metrics, trends and reporting remain placeholders; filename context may inflate confidence without additional extraction. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/group_comparator.py:L29-L117] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/context_extractor.py:L91-L106] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/report_generator.py:L24-L61] |
| DNV-OS-E301 | Named by `IndustryStandards.standard`, `ContextExtractor.apply_standards`, and two unconditional summary compliance fields; edition and clause/table identifiers are absent at these uses; clause text not checked against the licensed copy. Numeric criteria are not reproduced here. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/config.py:L80-L94] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/context_extractor.py:L108-L137] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/summarizer.py:L78-L79] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/summarizer.py:L125-L135] |
| Additional context standard | `apply_standards` also names SIGTTO Guidelines without edition or clause metadata; no numerical criterion is adopted here. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/context_extractor.py:L123-L126] |
| API RP 2SK | Named by the compliance configuration flag and the context-based secondary-standard selection; edition and clause/table identifiers are absent; clause text not checked against the licensed copy. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/config.py:L80-L94] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/context_extractor.py:L123-L129] |

UNVERIFIED: U4 — applicability, editions, clause identities and provenance of the named standard criteria remain unresolved; no licensed numeric table or compliance conclusion is adopted (#1630).

The standards-related implementations above construct dictionaries and boolean flags without emitting a `Citation` sidecar; summary compliance cannot be used as a hand-check reference (#1630). [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/context_extractor.py:L117-L137] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/summarizer.py:L67-L79] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/summarizer.py:L125-L136]

## 4. Validated worked examples

Validated engineering examples: **0**; the dedicated tests assert configuration/data-model behaviour and the validation methods at the end are assertion-free placeholders, so none supplies an independent physical benchmark. [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_config.py:L24-L370] [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_models.py:L35-L365]

The following are **regression** examples, with synthetic inputs constructed in the named tests; tolerances below describe their exact assertions, not engineering acceptance tolerances. [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_models.py:L57-L83] [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_models.py:L146-L169] [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_config.py:L329-L347]

| Case | Source / input | Expected value, unit and tolerance basis | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| R1 convergence-rate property | Test-constructed line objects, no input file | 5 lines, 3 converged, 60.0%; exact equality (0.0 percentage-point tolerance) | `tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_models.py::TestPretensionModels::test_pretension_data_properties` | BASE PASS; AFTER PASS. [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_models.py:L57-L83] |
| R2 stiffness container | Test-constructed matrix, no input file | 6 × 6 entries; positive eigenvalue predicate true; principal values present; exact shape/predicate assertions | `tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_models.py::TestStiffnessModels::test_stiffness_matrix_properties` | BASE PASS; AFTER PASS. [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_models.py:L146-L169] |
| R3 invalid configuration warnings | Test-constructed configuration, no input file | At least 3 warnings (count), including tolerance, utilisation and memory; no numerical analysis asserted | `tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_config.py::TestAnalysisConfig::test_config_validation` | BASE PASS; AFTER PASS; historical incident #531 is not reproduced. [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_config.py:L329-L347] |

Offline evidence at code revision `c099a5e1317e6bed63d876252d87cbb12c774372`: dedicated command `PYTHONPATH=src uv run --no-sources python -m pytest tests/solvers/orcaflex/mooring_analysis -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-<run>`; BASE 40 run / 40 passed / 0 failed / 0 skipped / 0 NOT_RUN; AFTER 40 run / 40 passed / 0 failed / 0 skipped / 0 NOT_RUN. No addopts override was used. The case expectations are defined by the test assertions above. [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_config.py:L24-L370] [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_models.py:L35-L365]

## 5. QA gates

The following checklist applies **AQG r0**, sections 4–10 and 13, to this package; it defines evidence requirements, not a claim that a model or calculation has cleared a gate. (AQG r0, sections 4–10, 13.)

| Gate | Applicability and route |
| --- | --- |
| G0 | Applicable: declared CSV/config schema, units and evidence fingerprint before analysing data. (AQG r0, section 4.) |
| G1 | Applicable to the upstream OrcaFlex model: load in the recorded target version with `LoadData` through the licensed Python interface, or a matching-version demo where demonstrated; retain messages and object count. NOT_RUN here: no model/version supplied and licensed interface prohibited. This package itself only reads CSVs. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L46-L55] (AQG r0, section 5.) |
| G2 | Applicable: statics convergence and independent equilibrium check precede stiffness/period interpretation. Solver records NOT_RUN here: no model, licence or statics record supplied. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L94-L130] (AQG r0, section 6.) |
| G3 | Applicable to source dynamics: a short-run record must accompany derived fender/contact data. NOT_RUN here: no simulation/model supplied and solver execution prohibited. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L55-L67] (AQG r0, section 7.) |
| G4 | Conditional: cite diffraction QA when vessel data come from a new diffraction run. NOT_APPLICABLE to this code-only review: no new diffraction data are introduced; C4 becomes required with such data. (AQG r0, sections 8 and 10.) |
| G5 | Applicable to every report; template success wording is not a QA appendix. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/report_generator.py:L33-L61] (AQG r0, section 9.) |

Checks below are executable test or evidence-inspection procedures. A source inspection reproducing a baseline defect is a finding, not a solver PASS. (AQG r0, sections 2–3.)

| ID | Gate | Module-specific procedure | Numeric pass criterion and unit | Evidence / current status |
| --- | --- | --- | --- | --- |
| C0a | G0 | Run R3 against `AnalysisConfig.validate` | ≥3 warnings (count) for the test's invalid configuration; 0 missing expected warning categories | R3 BASE PASS; logging rather than rejection remains a limitation. [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_config.py:L329-L347] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/analyzer.py:L36-L40] |
| C0b | G0 | For each CSV consumed by `parse_pretension_csv`, `parse_stiffness_csv`, `parse_fender_csv`, audit required and optional engineering fields before conversion | 0 missing engineering fields; 0 non-finite values; 0 ambiguous unit/separator declarations (counts) | Input-schema audit; NOT_RUN without an input dataset; defaults listed in section 6. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L46-L104] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L44-L92] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L41-L79] |
| C0c | G0 | Recompute SHA-256 over sorted input/config file digests used for each result | 0 mismatching fingerprints (count) | Manifest comparison; NOT_RUN: no analysis manifest supplied. (AQG r0, sections 4 and 13.) |
| C1 | G1 | Load the originating OrcaFlex model by the route above; compare model objects with CSV object identifiers and the manifest | 0 load errors, 0 warnings, 0 missing objects (counts); exact version match | Versioned load log and object reconciliation; NOT_RUN. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L50-L55] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L44-L56] (AQG r0, section 5.) |
| C2a | G2 | For `compute_stiffness_matrix`, compare each translational diagonal and coupling entry with its CSV column sum, then run R2 on the container | 0.0 kN/m difference in sums; 6 × 6 entries; 0 negative eigenvalues (count) for a claimed stable case | Column-sum table and eigenspectrum; R2 BASE PASS only for container assertions; engineering comparison NOT_RUN. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L103-L130] [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_models.py:L146-L169] |
| C2b | G2 | Reconcile `NaturalPeriodAnalyzer` and `estimate_natural_periods` to the same declared SI stiffness, mass and independent diagonal oscillator solution | 0 unexplained unit conversions (count); 0.0 s period difference for exactly representable diagonal benchmark results | Proposed benchmark criterion, not a published validation result; NOT_RUN: no existing benchmark test (issue pending (2)). [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L148-L156] [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L192-L201] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L206-L223] |
| C2c | G2 | Compare CSV `current_tension`/end forces with a signed upstream statics and independent equilibrium record, then inspect R1's rate property | 0 unconverged configurations (count); top tension within 5.0%, angle within 1.0° and touchdown distance within 5.0% of independent solution | R1 BASE PASS only for the stored boolean count; statics and independent comparisons NOT_RUN. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L69-L79] [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_models.py:L57-L83] (AQG r0, sections 6.1–6.4.) |
| C3 | G3 | Compare fender `Force`, `Max_Force` and contact evidence with the same originating model's short dynamics record | Stop time ≥1.0 s; 0 non-finite outputs, 0 solver warnings (counts); tension change ≤25.0% from statics | Simulation log/image and response reconciliation; NOT_RUN. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L62-L67] (AQG r0, section 7.) |
| C4 | G4 | For new diffraction-derived mass data passed to `VesselMassProperties`, reconcile mass/buoyancy and centres with the signed diffraction record | 0 missing G4 records (count); weight/buoyancy discrepancy ≤1.0% | Conditional record audit; NOT_APPLICABLE without new diffraction inputs. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L14-L24] (AQG r0, sections 8 and 10.) |
| C5a | G5 | Inspect summary/report code for unconditional standard-compliance success | 0 unconditional compliance-success assignments (count) | Source inspection FAIL at BASE: 2 literal success dictionaries in summarizer; #1630. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/summarizer.py:L78-L79] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/summarizer.py:L125-L136] |
| C5b | G5 | Inspect generated report against `ComprehensiveResults` and AQG records; compare input identifiers, status, version, fingerprint and evidence links | 0 missing required evidence fields; 0 unsupported success statements (counts) | Source inspection FAIL at BASE: Markdown/HTML omit the required evidence; #1630. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/report_generator.py:L24-L61] (AQG r0, sections 9 and 13.) |

## 6. Known failure modes and pitfalls

sweep: 82 found

The count is the number of separately listed fallback, conversion, unit-assumption or placeholder sites below; each row is an inspected source site, not a count of failed tests. Three outer CSV exception handlers re-raise `DataParsingError`; structural zero-initialisation of matrices, accumulators and empty containers is excluded unless returned as a result without computation. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L92-L104] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L91-L106] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L78-L98]

Exact sweep commands, executed over every Python file in the package:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|\b0(\.0)?\b|1970|0\.05|int\(|float\(|unit|cache' src/digitalmodel/solvers/orcaflex/mooring_analysis --glob '*.py'
rg -n 'get\(|else|Placeholder|placeholder|threshold|confidence|percent|tonne|kg|kN|met(er|re)|inf|nan' src/digitalmodel/solvers/orcaflex/mooring_analysis --glob '*.py'
```

| Site | Symptom / cause at the code pin | Existing detection / tracking |
| --- | --- | --- |
| S01 | Default CoG x is zero; mass assembly does not use the centre offset. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L21-L21] | no detection check exists; issue pending (2). |
| S02 | Default CoG y is zero; mass assembly does not use the centre offset. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L22-L22] | no detection check exists; issue pending (2). |
| S03 | Default CoG z is zero; mass assembly does not use the centre offset. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L23-L23] | no detection check exists; issue pending (2). |
| S04 | Added mass uses a fixed default multiplier without input-specific evidence. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L24-L24] | no detection check exists; issue pending (2). |
| S05 | Range screening uses fixed type ranges and silently substitutes a fallback type for unknown input. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L48-L93] | no detection check exists; issue pending (2). |
| S06 | Mass and inertias are scaled to kg; caller stiffness unit is not specified in the setter. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L148-L156] | C2b defined; benchmark NOT_RUN; issue pending (2). |
| S07 | Nonpositive diagonal stiffness or mass becomes an infinite period rather than a rejected physical state. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L196-L201] | no detection check exists; issue pending (2). |
| S08 | Absolute eigenvalues can hide an unstable negative eigenvalue. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L218-L219] | no detection check exists; issue pending (2). |
| S09 | Linear-algebra failure logs a warning and returns uncoupled periods. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L238-L240] | no detection check exists; issue pending (2). |
| S10 | Filtered, zero-frequency or unassigned modes become infinity; greedy assignment can consume another DOF’s dominant mode. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L261-L278] | no detection check exists; issue pending (2). |
| S11 | Singular response matrix is replaced by a fixed finite large response. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L323-L326] | no detection check exists; issue pending (2). |
| S12 | Convenience inertias use empirical dimensions and fallback type factors without a benchmark. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L349-L366] | no detection check exists; issue pending (2). |
| S13 | Default convergence and warning thresholds are fixed; warning threshold is stored without affecting the deviation comparison. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L24-L32] | no detection check exists; issue pending (1). |
| S14 | CSV float conversions and supplied percent deviation have no finite-value, locale or recomputed-deviation guard; EA may be absent. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L69-L81] | C0b defined; dataset NOT_RUN; issue pending (1). |
| S15 | Bare exception switches list parsing to scalar conversion; falsy values become zero; nonnumeric text can still raise. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L99-L104] | no detection check exists; issue pending (1). |
| S16 | Empty or NaN deviations/statistics are not guarded; empty denominators and skipped NaNs can misstate convergence. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L118-L145] | no detection check exists; issue pending (1). |
| S17 | Recommendation uses a fixed coefficient of variation criterion. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L161-L166] | no detection check exists; issue pending (1). |
| S18 | Empty problem list implies convergence; quality labels use fixed cutoffs. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L179-L190] | no detection check exists; #1630. |
| S19 | Adjustment for each problem line is a fixed placeholder percentage. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L193-L196] | no detection check exists; #1630. |
| S20 | Iteration estimate for each problem line is a fixed placeholder count. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L198-L201] | no detection check exists; #1630. |
| S21 | Only the first five critical lines are returned and confidence is offset and capped without an uncertainty model. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L203-L209] | no detection check exists; #1630. |
| S22 | Missing axial stiffness becomes zero; no finite-value or separator policy. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L64-L64] | C0b defined; dataset NOT_RUN; issue pending (1). |
| S23 | Required directional stiffness values use direct float conversions; no finite-value or separator policy. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L65-L67] | C0b defined; dataset NOT_RUN; issue pending (1). |
| S24 | Missing coupling values each become zero; no finite-value or separator policy. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L68-L70] | C0b defined; dataset NOT_RUN; issue pending (1). |
| S25 | Missing direction cosines each become zero; no finite-value or separator policy. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L72-L74] | C0b defined; dataset NOT_RUN; issue pending (1). |
| S26 | Missing force components each become zero; no finite-value or separator policy. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L77-L79] | C0b defined; dataset NOT_RUN; issue pending (1). |
| S27 | Missing directional/rotational columns become zero matrix entries; summation can skip NaNs and absent rotations look numerical. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L105-L128] | C2a defined; dataset NOT_RUN; issue pending (1). |
| S28 | Eigenanalysis exception is logged; default dominant direction is returned without a successful eigensystem. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L148-L162] | no detection check exists; issue pending (2). |
| S29 | Nonpositive ratio denominators become infinity; all-infinite ratios yield an isotropic fallback. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L169-L178] | no detection check exists; issue pending (2). |
| S30 | Critical-line list remains empty and natural periods remain None in returned metrics. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L180-L186] | no detection check exists; #1630. |
| S31 | Translational estimate assumes input kN/m and tonnes; nonpositive stiffness yields infinity. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L206-L216] | C2b defined; benchmark NOT_RUN; issue pending (2). |
| S32 | Roll period is returned as zero without inertia calculation. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L221-L221] | no detection check exists; #1630. |
| S33 | Pitch period is returned as zero without inertia calculation. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L222-L222] | no detection check exists; #1630. |
| S34 | Yaw period is returned as zero without inertia calculation. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L223-L223] | no detection check exists; #1630. |
| S35 | Missing fender location x becomes zero; direct float conversion lacks units/locale validation. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L58-L58] | C0b defined; dataset NOT_RUN; issue pending (1). |
| S36 | Missing fender location y becomes zero; direct float conversion lacks units/locale validation. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L59-L59] | C0b defined; dataset NOT_RUN; issue pending (1). |
| S37 | Missing fender location z becomes zero; direct float conversion lacks units/locale validation. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L60-L60] | C0b defined; dataset NOT_RUN; issue pending (1). |
| S38 | Missing Force series becomes an empty list; other statistics are not derived from it. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L62-L62] | no detection check exists; issue pending (1). |
| S39 | Missing maximum force becomes zero; first-row scalar conversion has no finite-value policy. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L63-L63] | C0b defined; dataset NOT_RUN; issue pending (1). |
| S40 | Missing mean force becomes zero; first-row scalar conversion has no finite-value policy. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L64-L64] | C0b defined; dataset NOT_RUN; issue pending (1). |
| S41 | Missing contact event count becomes zero; first-row scalar conversion has no finite-value policy. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L65-L65] | C0b defined; dataset NOT_RUN; issue pending (1). |
| S42 | Missing contact duration becomes zero; first-row scalar conversion has no finite-value policy. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L66-L66] | C0b defined; dataset NOT_RUN; issue pending (1). |
| S43 | Missing compression becomes None; first-row float conversion is unvalidated. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L67-L67] | no detection check exists; issue pending (1). |
| S44 | Repeated fender identifiers retain first-row statistics; later rows only contribute to Force. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L52-L67] | no detection check exists; issue pending (1). |
| S45 | Absent capacity map or nonpositive capacity reports zero utilisation and full margin. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L91-L112] | no detection check exists; issue pending (1). |
| S46 | Contact duration is copied directly into contact_percentages with no seconds-to-percent conversion. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L103-L103] | no detection check exists; issue pending (1). |
| S47 | Nonpositive total force yields zero load shares; empty maximum-force arrays reach undefined min/max reductions. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L125-L139] | no detection check exists; issue pending (1). |
| S48 | Default warning and critical utilisation thresholds have no input-specific provenance. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L19-L27] | no detection check exists; issue pending (1). |
| S49 | Recommendation cutoff and load-imbalance multiplier are fixed independently of configured thresholds. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L154-L168] | no detection check exists; issue pending (1). |
| S50 | Depth regex reads digits before a km token; return-period regex can capture unrelated digits. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/context_extractor.py:L23-L29] | no detection check exists; issue pending (3). |
| S51 | Missing matches leave None fields and zero confidence; fixed increments are not measured uncertainty. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/context_extractor.py:L41-L62] | no detection check exists; issue pending (3). |
| S52 | Missing/invalid depth returns None; float conversion performs no unit or separator scaling. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/context_extractor.py:L71-L78] | no detection check exists; issue pending (3). |
| S53 | Missing/invalid return period returns None; integer conversion has no explicit range check. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/context_extractor.py:L80-L89] | no detection check exists; issue pending (3). |
| S54 | LLM placeholder raises confidence without reading additional evidence. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/context_extractor.py:L101-L106] | no detection check exists; issue pending (3). |
| S55 | Standards and criteria are selected from fixed defaults and filename-derived context without clauses. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/context_extractor.py:L117-L137] | no detection check exists; #1630. |
| S56 | Convergence and stiffness configuration thresholds/defaults are not physical validation; mass may remain None. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/config.py:L9-L25] | C0a checks invalid tolerance; other defaults have no detection check; #1630. |
| S57 | Fender capacity defaults empty and warning/contact cutoffs are fixed. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/config.py:L29-L34] | no detection check exists; issue pending (1). |
| S58 | Caching is a configuration flag/path only; no cache writer is implemented by the orchestrator. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/config.py:L68-L77] | no detection check exists; #1630. |
| S59 | Standard-related factors and compliance flag are defaults without editions/clauses or computation. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/config.py:L80-L94] | no detection check exists; #1630. |
| S60 | Validation returns warnings only; orchestrator logs them and continues. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/config.py:L204-L227] | C0a detects warning categories; rejection has no detection check; #1630. |
| S61 | Empty pretension data returns zero convergence rather than an unavailable status. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/models.py:L49-L54] | no detection check exists; issue pending (1). |
| S62 | Absent eigenvalues make positive-definite false and principal stiffness None without a reason field. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/models.py:L129-L141] | no detection check exists; issue pending (2). |
| S63 | Duration can mean seconds or percent; coordinate frame/unit is not declared. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/models.py:L162-L172] | no detection check exists; issue pending (1). |
| S64 | Context fields default None and confidence zero, with no required provenance. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/models.py:L262-L273] | no detection check exists; issue pending (3). |
| S65 | Empty comprehensive results return zero success; presence of three metric objects defines success. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/models.py:L330-L338] | no detection check exists; #1630. |
| S66 | Directory analysis returns empty individual results for any supplied directory. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/analyzer.py:L42-L57] | no detection check exists; #1630. |
| S67 | File analysis returns empty individual results for any supplied list. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/analyzer.py:L59-L74] | no detection check exists; #1630. |
| S68 | Group comparison returns empty dictionaries/lists and an empty best configuration. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/analyzer.py:L76-L96] | no detection check exists; #1630. |
| S69 | Unmatched group key silently becomes the default group. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/group_comparator.py:L50-L61] | no detection check exists; issue pending (3). |
| S70 | Group metrics return zero average convergence and maximum stiffness placeholders. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/group_comparator.py:L72-L83] | no detection check exists; #1630. |
| S71 | Configuration rankings return three empty lists. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/group_comparator.py:L94-L101] | no detection check exists; #1630. |
| S72 | Trend identification returns a placeholder string. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/group_comparator.py:L112-L117] | no detection check exists; #1630. |
| S73 | Missing metrics leave an empty pass/fail map and all(empty) produces PASS; fixed summary thresholds replace case-specific criteria. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/summarizer.py:L38-L65] | no detection check exists; #1630. |
| S74 | Individual standard compliance is unconditionally true. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/summarizer.py:L78-L79] | C5a source inspection FAIL at BASE; #1630. |
| S75 | Group statistics/trends are placeholders; first/last runs become best/worst and rankings receive zero scores. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/summarizer.py:L91-L110] | no detection check exists; #1630. |
| S76 | Overall success, low risk and compliance are unconditional; matrix/actions remain empty. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/summarizer.py:L125-L136] | C5a source inspection FAIL at BASE; #1630. |
| S77 | Recommendations use fixed total-run threshold and default low-risk path can omit action. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/summarizer.py:L147-L155] | no detection check exists; #1630. |
| S78 | Markdown writes unconditional success without consulting results or QA evidence. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/report_generator.py:L33-L43] | C5b source inspection FAIL at BASE; #1630. |
| S79 | HTML report contains a heading only and no evidence table. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/report_generator.py:L54-L61] | C5b source inspection FAIL at BASE; #1630. |
| S80 | Pretension plot creates axes but never plots supplied analysis data. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/visualizer.py:L22-L33] | no detection check exists; #1630. |
| S81 | Stiffness plot creates axes but never plots supplied analysis data. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/visualizer.py:L35-L44] | no detection check exists; #1630. |
| S82 | Fender plot creates axes but never plots supplied analysis data. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/visualizer.py:L46-L57] | no detection check exists; #1630. |

Additional incident evidence: #531 reported configuration-validation drift; R3 now passes at the pin, while the three model-validation test bodies still contain no assertions (#1630). [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_config.py:L329-L347] [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_models.py:L350-L365] [docs/plans/2026-05-05-issue-531-9-pre-existing-test-failures.md:L9-L18]

The inspected package contains no record-skipping `continue`, epoch fallback or implemented cache-write operation; cache options alone do not establish caching behaviour. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/config.py:L68-L77] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/analyzer.py:L42-L96]

## 7. Lessons log

| Date | Change reference | Lesson | Evidence | Shared-log reference |
| --- | --- | --- | --- | --- |
| 2026-10-18 | Initial pack revision 0 | Configuration/container PASS results do not establish physical validation; assertion-free validation nodes cannot detect defects | [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_models.py:L350-L365] | Entry to be assigned; issue pending (5) |
| 2026-10-18 | Initial pack revision 0 | Standard compliance flags must not be interpreted as evaluated compliance | [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/summarizer.py:L78-L79] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/summarizer.py:L125-L136] | #1630; entry to be assigned; issue pending (5) |

Append future lessons with evidence and a shared-log reference; do not rewrite these rows.

## 8. Ownership

Requested primary review: OrcaFlex riser/mooring quality manager; requested deputy review: installation quality manager. These are review assignments for this draft, not recorded approvals.

Maintainer actions: open the numbered drafts, replace pending references, assign the deputy/shared-log references and request independent second-provider review. Escalate disputed method classification to the family quality manager; cross-family changes to the maintainers. Status remains DRAFT until that review clears the pack.

## 9. Dependencies and implementation inventory

### Inventory at the declared map revision

| Inventory parameter | Recorded value |
| --- | --- |
| Map file | `docs/registry/implementation-map.yaml` |
| Map ref | `origin/codex/implmap-work` |
| Map ref commit | `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` |
| Map data revision | `270c8759c374b67a7f032953e7fcdb14e0224461` |
| Scope | Complete header package, including comprehensive_analysis; no subpackage excluded |
| Confirmed families with an in-package implementation row | 0 |
| Implementation rows to inventory for those families | 0 (empty inventory) |
| Counted families after ranking | 0 |
| Counted-family grep | NOT_APPLICABLE: no map family has an implementation row in this package |
| Further-second-implementation count for counted families | 0; no counted-family search required |

The inventory above was computed by parsing `families[].implementations` and matching `path` against the header package; rejected candidates were not counted as implementations, consistent with the map's distinction between implementations and unresolved matches. [docs/registry/implementation-map.md:L3-L13]

Reproduction of the empty inventory, with the map loaded as text from the declared ref:

```python
import yaml
package = 'src/digitalmodel/solvers/orcaflex/mooring_analysis/'
map_data = yaml.safe_load(map_text)
families = [(i, family, [row for row in family['implementations']
                       if row['path'].startswith(package)])
            for i, family in enumerate(map_data['families'])]
ranked = sorted((item for item in families if item[2]),
                key=lambda item: (-len(item[2]), item[0]))
counted = ranked[:3]
```

For any future counted family, use its recorded `search_signals` exactly as Python `re`, case-sensitive, omit signals matching the package suffix below `src/digitalmodel/` case-insensitively, and assign hits to enclosing AST definitions, with module-level code as one definition.

Map follow-up: add or reconcile the section-3 method families (diagonal/coupled periods and response, pretension convergence, stiffness aggregation, fender utilisation, context criteria and summary criteria) before counting them; empty inventory does not assert that these algorithms are unique (issue pending (4)). [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L187-L328] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L106-L210] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L94-L225] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L81-L170] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/context_extractor.py:L108-L137] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/summarizer.py:L24-L136]

### Repository-wide search and related definitions

The repository-wide search included `scripts/`, using the following command; candidates below are related computations or helpers outside the header package, so they are map follow-up findings rather than counted-family misses. [src/digitalmodel/solvers/orcaflex/mooring.py:L684-L707] [scripts/benchmark/solver_metadata.py:L51-L70]

```sh
git grep -n -i -E 'natural.?period|pretension|stiffness.?matrix|fender' HEAD -- '*.py'
```

| Related definition | Classification and relationship |
| --- | --- |
| `Mooring.calculate_mooring_stiffness_3d` | External calculation: segment axial stiffness, force-derived cosines and directional/coupling contributions; not the package's CSV-column summation. [src/digitalmodel/solvers/orcaflex/mooring.py:L684-L790] |
| `Mooring.fender_force_analysis` | External orchestration/iteration and CSV writer; not a direct call to the package utilisation method. [src/digitalmodel/solvers/orcaflex/mooring.py:L364-L413] |
| `MooringComparativeAnalysis.analyze_pretension` | External CSV statistics and convergence criterion; separately implemented. [src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L85-L123] |
| `MooringComparativeAnalysis.analyze_stiffness` / `analyze_fender_forces` | Reads stored summary values, including periods and force statistics; helpers rather than a replacement eigenproblem. [src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L125-L164] [src/digitalmodel/solvers/orcaflex/analysis/comparative.py:L215-L250] |
| `natural_periods` in floating-system response | External frequency-dependent eigenbranch/root solution with modal assignment; a related independent period algorithm. [src/digitalmodel/marine_ops/fowt_quasi_static/response.py:L72-L108] |
| `_matrix_nonzero_summary` | Script helper formats supplied matrix entries; does not compute a stiffness response. [scripts/benchmark/solver_metadata.py:L51-L70] |
| 1 discovery file with an identifying name | Codenaming required before including its path or case details; keep out of public evidence until resolved (issue pending (6)). |

### Upstream, downstream and solver text inventory

| Dependency | Interface / evidence |
| --- | --- |
| NumPy | Matrix eigenanalysis, inversion and period/response calculations. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L192-L219] [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L312-L326] |
| pandas and CSV producers | Three readers consume CSVs; no enforced producer/version or manifest identity is carried through these parse returns. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L46-L93] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L44-L92] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L41-L79] |
| PyYAML | `AnalysisConfig.from_yaml`/`to_yaml` serialise configuration. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/config.py:L124-L172] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/config.py:L174-L202] |
| Logging and dataclasses | Mass-property assignment logs the input mass; dataclasses hold typed result fields without runtime physical validation. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L113-L122] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/models.py:L15-L28] |
| Matplotlib | Visualizer allocates figures/axes and optionally saves files; supplied results are not plotted. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/visualizer.py:L22-L57] |
| Downstream summaries/reports | Consume metric/result containers but currently substitute placeholder output. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/summarizer.py:L24-L136] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/report_generator.py:L24-L61] |
| OrcaFlex textual dependency | Exactly one source occurrence of the program name, in the package docstring; the same file exports the period API. No program version is named there. Read as text only. [src/digitalmodel/solvers/orcaflex/mooring_analysis/__init__.py:L1-L24] |
| Licensed interface | No import or solver launch in the inspected package; the implemented routes use CSV readers and in-memory arrays. Program execution and upstream solver data remain external dependencies, not offline-test results. [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L8-L11] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L46-L93] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L44-L92] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L41-L79] |

Text inventory command: `rg -n -i 'orcaflex|orcfxapi' src/digitalmodel/solvers/orcaflex/mooring_analysis --glob '*.py'`; result: the one docstring occurrence cited above. [src/digitalmodel/solvers/orcaflex/mooring_analysis/__init__.py:L1-L10]

## 10. Coverage and open items

### Files read at the code pin

| Coverage group | Files / evidence |
| --- | --- |
| Package root | `__init__.py`, `natural_periods.py`. [src/digitalmodel/solvers/orcaflex/mooring_analysis/__init__.py:L1-L24] [src/digitalmodel/solvers/orcaflex/mooring_analysis/natural_periods.py:L1-L373] |
| Comprehensive public surface and orchestration | `__init__.py`, `analyzer.py`, `config.py`, `exceptions.py`. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/__init__.py:L1-L29] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/analyzer.py:L1-L96] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/config.py:L1-L245] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/exceptions.py:L1-L144] |
| Numerical/CSV paths | `pretension.py`, `stiffness.py`, `fender_forces.py`, `models.py`. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/pretension.py:L1-L210] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/stiffness.py:L1-L225] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/fender_forces.py:L1-L170] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/models.py:L1-L338] |
| Context and presentation paths | `context_extractor.py`, `group_comparator.py`, `summarizer.py`, `report_generator.py`, `visualizer.py`. [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/context_extractor.py:L1-L137] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/group_comparator.py:L1-L117] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/summarizer.py:L1-L155] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/report_generator.py:L1-L61] [src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/visualizer.py:L1-L57] |
| Dedicated tests and fixtures | Both test modules and conftest, including unavailable legacy fixture references; identifying tokens are not reproduced. [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_config.py:L1-L370] [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_models.py:L1-L365] [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/conftest.py:L1-L171] |
| Direct documentation hit | `docs/plans/2026-05-05-issue-531-9-pre-existing-test-failures.md`; its module test reference was read as historical evidence, not proof of current failure. [docs/plans/2026-05-05-issue-531-9-pre-existing-test-failures.md:L1-L44] |
| Routing | Existing solvers domain; new nested `orcaflex/mooring_analysis` pack registration uses MR. [docs/registry/module-routing.yaml:L354-L359] |

Discovery searched the package and `tests/` for the package import path and public period symbols, and README/docs for the exact module path, `comprehensive_analysis` and `NaturalPeriodAnalyzer`; no package-local README/docs existed at the code pin. The dedicated suite is two test modules containing 40 test definitions, including 3 assertion-free nodes; no dedicated period/analyzer/CSV calculation benchmark node was found. [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_config.py:L1-L370] [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_models.py:L1-L365]

No private data repository, client-derived numerical values or licensed standard text was used. No private-sidecar entry ID is claimed; identifying discovery content remains excluded pending codenaming (issue pending (6)).

### Open items

| Item | Required follow-up | Responsible role | Tracking |
| --- | --- | --- | --- |
| U1 | Establish supported version and engineering applicability; add independent period and stiffness benchmarks | Riser/mooring quality manager | issue pending (2) |
| U2 | Resolve stiffness scaling, frame/sign/phase/origin and rotational conventions; test negative eigenvalues, singular and filtered modes | Riser/mooring quality manager | issue pending (2) |
| U3 | Declare fender coordinate/contact units and CSV missing/locale/finite-value policy; prevent zero defaults from implying evidence | Riser/mooring quality manager | issue pending (1) |
| U4 | Resolve editions/clauses and calculation provenance; remove unsupported compliance conclusions | Riser/mooring quality manager | #1630 |
| F1 | Replace or reject orchestration, comparison, visualisation and reporting placeholders; test all retained paths and assertion-free nodes | Maintainer | #1630 |
| F2 | Correct filename depth, unknown contexts and unsupported confidence increases | Maintainer | issue pending (3) |
| F3 | Add method-family map rows and reconcile related external methods | Maintainer | issue pending (4) |
| F5 | Assign deputy and shared-log references for the initial lessons | Maintainer | issue pending (5) |
| F6 | Codename the 1 identifying discovery file and identifying fixture tokens without publishing their values | Maintainer | issue pending (6) |
| F4 | Obtain upstream G1–G3 records and conditional G4 evidence; build a traceable G5 appendix before engineering use | Riser/mooring quality manager | #1630 |

The historical configuration-validation incident #531 has an existing detection test and currently passes; it is not used to infer that other paths are tested. [tests/solvers/orcaflex/mooring_analysis/comprehensive_analysis/test_config.py:L329-L347] [docs/plans/2026-05-05-issue-531-9-pre-existing-test-failures.md:L9-L18]
