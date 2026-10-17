# AQWA module knowledge pack

| Header | Value |
| --- | --- |
| Module | hydrodynamics/aqwa |
| Repository | digitalmodel |
| Package path | `src/digitalmodel/hydrodynamics/aqwa` |
| Pack revision | 0, 2026-10-16 |
| Code revision | `6cf35f466e5c1b730951af045d8cfe0065af073f` |
| Analysis family | diffraction; registry family DF |
| Quality role | diffraction quality manager |
| Status | DRAFT; second-provider clearance pending |
| Implementation-map revision | `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` |
| Map embedded data revision | `270c8759c374b67a7f032953e7fcdb14e0224461` |

Code citations resolve at the code revision; `docs/registry/implementation-map.yaml` citations resolve at the separate implementation-map revision, whose inventory describes its embedded data revision.

## 1. Purpose and scope

The main router dispatches preprocessing, analysis, message-file inspection and postprocessing from configuration switches; a viscous-damping configuration selects a separate orchestration path. [src/digitalmodel/hydrodynamics/aqwa/aqwa_router.py:L33-L100]

`AqwaAnalysis.analysis_router` selects RAO preparation, damping or external-force callbacks and then labels the configuration completed; that label alone does not establish a successful native calculation. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis.py:L12-L39]

The hydrodynamics domain is recorded as production in the routing registry; that domain label is not independent qualification of these readers or calculations. [docs/registry/module-routing.yaml:L144-L150]

The pack covers every Python source file inside the package, including the external-force protocol and validation subpackages; neighbouring diffraction, marine-analysis and mesh-library packages retain separate implementation ownership. [src/digitalmodel/hydrodynamics/aqwa/__init__.py:L1-L17][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L17-L20]

`run_raos` is a manual-run placeholder, and `AQWAValidator.validate_all` returns NOT_IMPLEMENTED; neither is evidence of an automated solver or validation pass. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L228-L231][src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/validator.py:L32-L51]

## 2. Inputs and outputs

### Inputs

| Name / consuming function | Unit / range | Default | Source / interpretation |
| --- | --- | --- | --- |
| `type.preprocess`, `type.analysis`, `type.results`; `Aqwa.router` | Boolean switches; required keys | No switch default | Caller configuration. [src/digitalmodel/hydrodynamics/aqwa/aqwa_router.py:L33-L100] |
| `analysis_settings.method`; `AqwaAnalysis.analysis_router` | Enumerated string, aliases selected in code | Missing or unsupported method raises ValueError | Caller configuration. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis.py:L12-L33] |
| `inputs[].data.raw`; DAT deck writers | Coordinates X/Y/Z; mass; inertia; period; coefficient units not enforced | Six omitted inertia components become zero | Configuration, not a verified engineering basis. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L145-L231] |
| DAT inspection | Deck numbers, numeric text, options, CoG element | Missing decks become empty sequences; mass defaults zero | UTF-8 DAT, undecodable bytes ignored. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L515-L539][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L564-L581] |
| LIS extraction | Configured start/end keywords, occurrence, header/data offsets | No numeric-locale policy | `file_management.input_files.LIS`. [src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L41-L95][src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L110-L150] |
| PLT reader | Four integer selectors; equilibrium, frequency or time-response text | Fourth selector expands to six components | Installed reader/workbench output; native execution required. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L114-L164][src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L225-L288] |
| MES inspection | Message counts, unit 1 | No input directory default suitable for a portable run | Directory containing uppercase `.MES`; exact warning/error patterns. [src/digitalmodel/hydrodynamics/aqwa/mes_files.py:L16-L52][src/digitalmodel/hydrodynamics/aqwa/mes_files.py:L162-L171] |
| Viscous workflow | `context.analysis_source`, cases, DAT/LIS file patterns | Empty cases/files allowed; fluid-density default present | YAML or dictionary; paired files copied into output inputs. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L42-L80][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L82-L141][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L187-L190] |
| Drag members; `derive_viscous_damping_statistical` | cd, area, lever arm; no dimensional validation | Missing quantities become zero | Caller/member maps. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L71-L155] |
| External-force callback state | Pos/Vel vectors, times, forces, added mass; callback-specific units | Several fixed coefficients and matrices | AQWA socket protocol and configuration; application-derived values are excluded here. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L118-L163][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L442-L519] |

### Outputs

| Name / producing function | Unit / range | Default / destination |
| --- | --- | --- |
| `AqwaDATFiles.write_dc_data` | Fixed-width deck text; inherited input units | Configured output directory, deck category and extension. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L104-L144] |
| `DATInspectionResult` | Frequency, reciprocal period, optional drag/CoG, mass/inertia/restoring maps | Missing-data sentinels include zero, None and empty maps. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L40-L75][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L595-L731] |
| `AqwaLISFiles.router` | Concatenated dataframe, optional CSV and workbook replacement | Result folder; save CSV defaults True. [src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L24-L39][src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L218-L251] |
| `AqwaReader` | Dataframe, statistics, CSV, optional workbook replacement | Native stdout parsed into time/frequency and response arrays. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L290-L371][src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L373-L416] |
| `MesFiles.router` | Warning/error frequencies and status matrix, unit 1 | Four CSV products in the input directory. [src/digitalmodel/hydrodynamics/aqwa/mes_files.py:L173-L226] |
| Viscous workflow | B33/B44/B55, component percentages, critical ratios, natural frequency rad/s and period s | Per-record CSV tables, HTML figures and nonempty master summary. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L202-L260][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L399-L485][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L621-L641] |

UNVERIFIED: U1 — consistent dimensional units, global origin, heading sense, phase sign and rotational RAO convention across all DAT/LIS/PLT and callback paths are not established; in particular the DAT reciprocal-period convention and angular-frequency plot label require reconciliation (section 10).

## 3. Method and governing standards

These are descriptions of implemented algorithms, not independent validation references; no numerical value from a licensed publication is reproduced.

| Method | Implementing definition / governing basis |
| --- | --- |
| Deck construction | `get_dc_data` combines library header, dynamically selected body and footer; node coordinates use three decimal places. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L93-L102][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L145-L163] |
| Deck metadata parsing | `inspect_dat_file` groups decks and parses options, frequency, drag, CoG, mass, inertia and restoring; frequency parsing uses reciprocal period. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L515-L539][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L564-L615] |
| LIS block parsing | `get_data`, `get_refine_data_using_cfg`, `get_data_for_refine_data_line_number` find configured keyword spans and coerce numeric rows, including continuation-row metadata. [src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L41-L150] |
| Response transforms | `transform_by_data_key` multiplies displacement by frequency or frequency squared and rounds the result; an angular-frequency input is needed for the velocity/acceleration interpretation. [src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L189-L216] |
| Equilibrium parsing, supplemental inventory | `AqwaReader.process_equilibrium_result` separates stdout on comma/colon, extracts labels and float-converts a fixed row slice. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L290-L313] |
| Frequency parsing, supplemental inventory | `AqwaReader.process_frequency_result` independently extracts labels and frequency/value arrays from a fixed row slice. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L315-L341] |
| Time-response parsing, supplemental inventory | `AqwaReader.process_timeresponse_result` rejects an empty dataframe and independently parses time/value arrays. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L343-L371] |
| Legacy peak/damping selection | `get_natural_periods` averages angular-frequency columns; `get_additional_damping_by_cfg` clamps peaks to tabulated frequency ranges, interpolates and scales additional damping. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L499-L526][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L581-L653] |
| Heading aggregation | `compute_heading_rms` uses square root of the NaN-aware mean of squared amplitudes. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L61-L69] |
| Viscous linearization | `derive_viscous_damping_statistical` and `_aggregate_viscous_force` combine density, member drag, area, frequency, heading RMS and rotational lever arm. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L71-L155] |
| Damping combination | `combine_damping` sums radiation, viscous and external arrays; zero totals yield zero component percentages. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L157-L200] |
| Critical ratio | `compute_critical_damping_ratios` uses inertia plus added mass, square-root stiffness/mass frequency and damping divided by twice mass times natural frequency; nonpositive denominators yield zero ratio. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L202-L260] |
| Hydrostatic precedence | `_process_case` prefers LIS restoring, then DAT restoring, then configured properties; frequency alignment is checked only for nonempty comparator grids. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L196-L267][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L318-L322] |
| Force rotation and moment | `RotationMatrix.Apply` rotates vectors; `ForceTorque` computes the offset cross force; the default rotation constructor has an unresolved failure listed in section 6. [src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L244-L304] |
| External-force coupling | `ef_server_router` starts native AQWA and exchanges callback state; callback functions produce force/added-mass arrays and save histories. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L48-L116][src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L390-L473] |

UNVERIFIED: U2 — the numerical kernels do not establish a governing standard edition or emitted calculation Citation sidecar; no licensed clause text has been checked, and the implemented viscous model is not an independent hand-check reference (section 10).

## 4. Validated worked examples

Validated examples: **0**; no independent expected hydrodynamic benchmark is established by the two synthetic precedence tests below, which replace both parsers. [tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L17-L99]

Regression examples: **2**; expected values are synthetic test literals, with no established physical coefficient unit; these check source precedence only, not hydrodynamic accuracy. [tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L102-L151]

| Case / source | Input and expected value | Tolerance / test node | Offline result |
| --- | --- | --- | --- |
| Restoring source precedence; synthetic revision at code pin | Fake DAT/LIS objects; LIS C33/C44/C55 = 1111/2222/3333 synthetic coefficient units | `pytest.approx` defaults; `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py::test_restoring_prioritizes_lis_values` | PASS at baseline and AFTER. [tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L102-L138] |
| Whole-missing LIS restoring; synthetic revision at code pin | Fake DAT/LIS objects; DAT C33/C44/C55 = 101/202/303 synthetic coefficient units | `pytest.approx` defaults; `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py::test_restoring_falls_back_to_dat_when_lis_missing` | PASS at baseline and AFTER. [tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L141-L151] |

No physical meaning is assigned to the synthetic coefficient numbers; U1 covers the missing dimensional contract.

## 5. QA gates

The required family gates are G0, G1, G4 and G5; G2 and G3 are NOT_APPLICABLE for diffraction preparation under AQG r0, section 10, AQWA row. The outputs here are AQWA decks and reader/report products; the OrcaWave load route is NOT_APPLICABLE because this package has no OrcaWave output writer. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L104-L144][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis.py:L12-L39]

Checks below define executable acceptance procedures; a definition does not claim that its gate has passed. Numerical gate tolerances are from AQG r0, sections 4–10, rather than a licensed standard. Native checks remain NOT_RUN; no solver or licensed interface was started for this pack.

| Gate / check | Module-specific procedure | Numeric pass criterion and unit | Evidence / current disposition |
| --- | --- | --- | --- |
| G0 / AQ-01 | Run `validate_ldrg_option` and `validate_cog_element` on each damping DAT, compare selected CoG element with configured expectation | 0 missing LDRG flags; 0 mismatching element IDs, count | Exceptions and DAT inspection metadata; NOT_RUN without qualified decks. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L541-L558] |
| G0 / AQ-02 | Call `_validate_frequency_alignment` on extracted LIS RAO and radiation/added-mass grids, first require both nonempty | 0 missing frequencies, count; equal shapes and all differences ≤ `1e-8 + 1e-5*abs(other)` rad/s after unit reconciliation | Grid arrays and mismatch exceptions; NOT_RUN; empty-grid blind spot in section 6. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L200-L212][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L318-L322] |
| G0 / AQ-03 | Run the two section-4 restoring tests against `_process_case` | 0 incorrect coefficient selections, count; test `pytest.approx` criterion | Test-node outcomes in section 4; precedence only. [tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L127-L151] |
| G0 / AQ-12 | Run `tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py::TestAqwaLISFilesEdgeCases::test_csv_save_disabled` on `AqwaLISFiles.save_to_csv` | 0 non-None returned filenames when save is disabled, count | PASS at baseline; regression of an option, not an error-path test. [tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py:L309-L343] |
| G0 / AQ-13 | Run the LIS parser velocity and acceleration transformation tests on `transform_by_data_key` | Absolute errors < 0.001 in the synthetic output units; 0 failed assertions, count | PASS at baseline; frequency-unit correctness remains unqualified. [tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py:L178-L248] |
| G1 / AQ-04 | For every deck written by `write_dc_data`, use the target AQWA executable with the deck, run only through hydrostatics, then read native LIS and MES; compare body count with manifest | Exit status 0, 0 native errors, 0 load warnings, 0 count differences, count | Native version, stage, message text, input fingerprint and output digests; NOT_RUN: licensed AQWA required. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L104-L112][src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L59-L97][src/digitalmodel/hydrodynamics/aqwa/mes_files.py:L16-L52] AQG r0, sections 5 and 10. |
| G2 | NOT_APPLICABLE | No statics QA gate for this diffraction family | AQG r0, section 10, AQWA row. |
| G3 | NOT_APPLICABLE | No short-dynamics QA gate for this diffraction family | AQG r0, section 10, AQWA row. |
| G4 / AQ-05 | Compare `inspect_dat_file.mass` and independent mesh displaced volume against native hydrostatics, with one declared density/gravity and any declared preload | Weight residual ≤ 1.0% of weight; volume residual ≤ 0.5%, % | Balance and independent integration tables; NOT_RUN: independent mesh/hydrostatics not in hand. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L518-L539] AQG r0, section 8, G4.1–G4.2. |
| G4 / AQ-06 | Compare independent CoB, mass-model CoG and GM with the deck/hydrostatics for each body; the parser's fixed CoG ID alone is insufficient | CoB horizontal residual ≤ 0.5% length, vertical ≤ 1.0% draft; GM > 0 m and difference ≤ 2.0% | Centres/GM table; NOT_RUN: hydrostatics required. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L550-L558][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L704-L731] AQG r0, section 8, G4.3. |
| G4 / AQ-07 | Independently inspect mesh used with the generated deck; compare panel count with target-version limit and shortest-wave panel size | 0 panels above version limit, count; aspect ratio ≤ 3, unit 1; maximum dimension ≤ shortest wavelength/6, m | Mesh metrics, memory estimate and version limit source; NOT_RUN: no mesh qualification. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L93-L112] AQG r0, section 8, G4.4. |
| G4 / AQ-08 | Independently inspect waterline, draft, normals, signed volume, gaps and symmetry declarations of that mesh | Waterline z residual ≤ 1 mm; draft residual ≤ 0.01 m; 0 reversed normals, 0 zero-area panels, 0 duplicate symmetry declarations; signed volume > 0 m³ | Mesh images and closure metrics; NOT_RUN. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L145-L163] AQG r0, section 8, G4.5–G4.6. |
| G4 / AQ-09 | Compare LIS analysed frequencies with an independently estimated first irregular frequency, record interior lid where needed | If maximum analysed frequency ≥ 0.9 times estimated irregular frequency, 0 missing lids, count | Frequency/lid record; NOT_RUN: geometry and qualified frequency convention required. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L200-L212] AQG r0, section 8, G4.7. |
| G4 / AQ-10 | Review extracted heave RAO, radiation damping, added-mass matrices and mesh views for each body | Long-period heave within 2.0% of 1 m/m; diagonal damping ≥ 0 in declared damping units; added-mass asymmetry ≤ 2.0%; 0 unexplained spikes in ±5.0% irregular-frequency interval | Extracted arrays and independent review; NOT_RUN: qualified native result needed. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L200-L215] AQG r0, section 8, G4.8. |
| G5 / AQ-11 | Compare `_generate_case_reports` CSV/HTML output with gate records and recomputed input SHA-256 manifest | 0 missing applicable gate records, 0 fingerprint mismatches and 0 unsigned decisions, count | QA appendix, versions, digests, dispositions and quality signature; NOT_RUN: current report writer lacks this appendix. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L399-L426][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L600-L641] AQG r0, section 9. |

## 6. Known failure modes and pitfalls

sweep: 82 found

The count denotes classified candidate groups, including benign initializers and propagated failures; it is not a count of 82 proven numerical defects. Each row supplies exact occurrence lines, symptom, and existing detection or its absence; proposed gate procedures in section 5 are not represented as shipped regression coverage.

Search commands, applied to every package Python file at the code pin:

```sh
rg -n 'except|\bcontinue\b|return None|return \[\]|return \{\}|= (0(?:\.0)?|1970)\b|\b(?:int|float)\(|[Uu]nit|0\.05|cache' src/digitalmodel/hydrodynamics/aqwa -g '*.py'
rg -n 'get\([^\n]*,\s*(?:0(?:\.0)?|\[\]|\{\})\)|\bor\s+(?:0(?:\.0)?|\[\]|\{\})|zeros|errors=|\bpass\b|\b(Hz|rad|deg|kg|tonne|density|1025|9\.81|percent|unit)\b|cache' src/digitalmodel/hydrodynamics/aqwa -g '*.py'
```

The source sweep also examined all handlers and numerical/container defaults in their enclosing definitions; no cache-write path or 1970 default was identified. Mechanical occurrence totals are 38 exception handlers, 15 continues, 43 float conversions, 3 integer conversions, 6 literal None/empty-map returns and 2 tuple-of-None returns.

| Candidate | Classification / symptom and exact source occurrences | Existing detection / follow-up |
| --- | --- | --- |
| 5 | Optional file-management import failure stores False/None; same unconditional use at 57. Occurrences 17-19. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L17-L19] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 6 | Utilities import failure stores False/None; router later uses au at 86. Occurrences 28-30. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L28-L30] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 7 | Server import failure stores False; router refers to missing class at 71. Occurrences 35-36. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L35-L36] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 8 | Optional NumPy import is suppressed; array wrapper raises explicitly at 46–47, but parsers return None/empty at 619/677. Occurrences 22-23. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L22-L23] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 9 | Bare except suppresses every OrcFxAPI import error; missing interface is only logged at debug. Occurrences 13-14. [src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L13-L14] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 10 | Bare except rewrites all licence/model errors to generic exception; return False after raise is unreachable. Occurrences 32-35. [src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L32-L35] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 13 | All CSV errors become empty DataFrame and False; logged fallback rather than raised failure. Occurrences 280-285. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L280-L285] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 14 | All CSV errors become empty DataFrame while is_file_valid remains True, initialized at 318. Occurrences 318,330-334. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L318-L334] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 15 | All absolute-damping CSV errors become empty DataFrame without validity state. Occurrences 352-356. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L352-L356] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 16 | RAO CSV error becomes empty DataFrame; failed CSV filename still enters published result mapping. Occurrences 712-716. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L712-L716] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 17 | Empty damping source logs and returns empty additional-damping dictionary; no explicit invalid status. Occurrences 453-472. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L453-L472] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 18 | Zero roll/pitch/heave angular frequency is replaced by fixed 0.04, concealing zero input. Occurrences 504-509. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L504-L509] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 19 | Arithmetic assumes natural-period input columns are angular frequency; conversion to Hz and seconds performed after rounding. Occurrences 500-502,511-523. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L500-L502][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L511-L523] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 20 | Requested peak outside sample range is clamped twice; zero critical damping substitutes absolute damping rather than applying target ratio. Occurrences 596-613,625-626. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L596-L613][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L625-L626] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 21 | Per-file exception logs and drops whole input, returning partial/empty master dataset. Occurrences 92-95. [src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L92-L95] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 22 | External tokens float-converted without locale/thousands normalization; short-row ValueError is swallowed. Occurrences 124,131,138,140-142. [src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L124-L124][src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L131-L131][src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L138-L138][src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L140-L142] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 23 | Blank rows skipped for the special position table; benign separator handling but no row-count check. Occurrences 143-148. [src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L143-L148] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 24 | Missing short-row metadata is copied from preceding row, assuming continuation layout. Occurrences 125-140. [src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L125-L140] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 25 | Velocity/acceleration multiply by frequency and frequency squared, implicitly angular frequency; float coercion propagates except outer get_data catch occurs earlier. Occurrences 197-214. [src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L197-L214] | AQ-13 covers numeric multiplication only; no detection check exists for a wrong frequency unit; issue pending (1), observed 2026-10-16. |
| 26 | transform_by_data_key returns implicitly None when its per-output flag is False; caller unconditionally assigns result. Occurrences 173-216. [src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L173-L216] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 28 | No MES files and directory-read errors both return three None values; router then iterates them at 170–171. Occurrences 19-25. [src/digitalmodel/hydrodynamics/aqwa/mes_files.py:L19-L25] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 31 | Empty input list yields None output filename and no explicit invalid status. Occurrences 48-100. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L48-L100] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 32 | Reader subprocess return codes ignored; primary stderr not validated and CSV-export output discarded. Occurrences 123-158. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L123-L158] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 33 | Invalid change reference prints and returns None; optional disabled transform also returns None by design. Occurrences 167-223. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L167-L223] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 34 | External text converted directly to float after fixed row slicing; separators/locale unvalidated and final row always excluded. Occurrences 308-309,337-338,367-368. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L308-L309][src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L337-L338][src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L367-L368] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 35 | AQWA subprocess outputs discarded; waited process return code untested; detached launch logs COMPLETE before execution completes. Occurrences 83-97. [src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L83-L97] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 36 | Case exception prints and continues, then saves current/partial/stale result DataFrames with no invalid marker. Occurrences 80-96,106-116. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L80-L96][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L106-L116] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 37 | Zero control coefficient triggers hardcoded fallback force coefficient; expected units not declared. Occurrences 184-189. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L184-L189] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 40 | Two times rounded to 3 decimals suppress a result row when equal; distinct close times can collide. Occurrences 390-394. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L390-L394] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 41 | Missing configured added-mass matrix substitutes fixed diagonal magnitudes; physical units not declared locally. Occurrences 473-480. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L473-L480] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 42 | Hardcoded piecewise stiffness/damping, displacement/velocity breakpoints and fractional power; engineering provenance and units not stated locally; constants must not be copied into public pack. Occurrences 489-519. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L489-L519] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 43 | DATInspectionResult mass defaults 0.0, conflating absent mass with true zero. Occurrences 73. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L73-L73] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 44 | Mass writer comments explicitly question mass units; no conversion/validation. Occurrences 177-185. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L177-L185] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 45 | Six inertia terms each default zero when omitted, without missing-data validation. Occurrences 202,204,206,208,210,212. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L202-L202][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L204-L204][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L206-L206][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L208-L208][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L210-L210][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L212-L212] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 46 | Deck writer/parser assumes frequency equals reciprocal period; not angular frequency, unlike later workflow plot label. Occurrences 227-230,601-613. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L227-L230][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L601-L613] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 47 | Explicit zero damping entries, initial velocities and custom rotation suppression are model assumptions; the latter is application-specific. Occurrences 239-245,445-447,477,488-490. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L239-L245][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L445-L447][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L477-L477][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L488-L490] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 48 | Missing decks 6,7,1,3,4,5 are each passed as empty lists; downstream empties/zeros may represent absent source data. Occurrences 520-527. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L520-L527] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 49 | errors='ignore' silently discards invalid UTF-8 bytes. Occurrences 569. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L569-L569] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 51 | Empty Deck6 rows ignored and last-token float failure skipped; malformed frequency record disappears. Occurrences 600,603-605. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L600-L600][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L603-L605] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 52 | Non-positive frequency becomes infinite period without input error. Occurrences 607-610. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L607-L610] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 53 | Missing NumPy/short rows/non-integer row IDs/non-float cells yield None or skipped rows; absent rows stay zero and out-of-range IDs are ignored. Occurrences 619,622,628,631-637,638-643. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L619-L619][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L622-L622][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L628-L628][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L631-L637][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L638-L643] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 54 | Mass starts zero, regex extracts every number and selects last; nonnumeric/failed rows skip; empty deck returns zero. Occurrences 648,650-658. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L648-L648][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L650-L658] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 55 | Three inertia terms start zero; short/failed rows skip; last six numeric substrings become matrix terms. Occurrences 661,663-673. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L661-L661][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L663-L673] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 56 | Missing NumPy/short restoring matrix returns {}; regex/intended float check ignores invalid tokens, first 36 extracted numbers used. Occurrences 677,682-688,699-702. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L677-L677][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L682-L688][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L699-L702] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 57 | Missing CoG remains None; no-integer rows skip, ID int regex-constrained, failed coordinate conversion yields None; first matching fixed ID wins. Occurrences 706-724. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L706-L724] | AQ-01 checks element ID only; no detection check exists for missing coordinates; issue pending (1), observed 2026-10-16. |
| 58 | Bind error logs then sys.exit() without nonzero status; also indexes exception object as legacy tuple. Occurrences 112-116. [src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L112-L116] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 60 | TypeError treated as scalar fallback; unsupported types silently produce no packet bytes. Occurrences 211-221. [src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L211-L221] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 61 | Send socket error closes and marks closed but does not re-raise or return explicit failure. Occurrences 225-228. [src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L225-L228] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 65 | Empty amplitudes/frequency arrays return empty unchanged; benign propagation but no required-data check. Occurrences 65-69,139-142. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L65-L69][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L139-L142] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 66 | Assumed fluid density and empty/missing drag/RAOs become zero contributions. Occurrences 76,91,96,104,113,125-126. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L76-L76][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L91-L91][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L96-L96][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L104-L104][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L113-L113][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L125-L126] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 67 | Missing cd, area and lever_arm each default 0.0; missing rotational lever eliminates damping. Occurrences 145-147. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L145-L147] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 68 | Missing radiation, viscous and external sources become zero arrays. Occurrences 167,173-175. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L167-L167][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L173-L175] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 69 | Zero total gives three 0% contributions while floating-point warnings suppressed. Occurrences 180-183. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L180-L183] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 70 | Missing damping/inertia/restoring/added mass become zeros; nonpositive denominator gives zero natural frequency/ratio and infinite period, concealing invalid basis. Occurrences 224-244. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L224-L244] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 72 | Missing cases/files or zero glob matches produce empty case/manifest and no analysis error; non-DAT/LIS extension skip is intentional. Occurrences 56,93,124-128. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L56-L56][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L93-L93][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L124-L128] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 73 | Missing hydrostatic maps/added-mass diagonal use empties; density defaults fixed value. Occurrences 187-190,216. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L187-L190][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L216-L216] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 74 | Optional missing hydrostatic property returns {}; required missing property raises explicitly. Occurrences 302-307. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L302-L307] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 75 | Empty comparator frequency grid skips alignment check; nonempty mismatch explicitly raises. Occurrences 319-322. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L319-L322] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 76 | Missing matrix section returns three empty arrays; no invalid marker. Occurrences 330-331. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L330-L331] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 77 | Missing CoG/centroid defaults origin, missing area/cd defaults zero and heave lever explicitly zero; rotational geometry can be fabricated. Occurrences 344,347-349,353. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L344-L344][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L347-L349][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L353-L353] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 78 | Missing/empty external damping becomes zero array. Occurrences 364,369-370. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L364-L364][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L369-L370] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 79 | Missing restoring diagonal returns {}; invalid numeric value skips each coefficient, leaving partial map. Occurrences 378-392. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L378-L392] | AQ-03 covers whole-missing restoring only; no detection check exists for partially invalid maps; issue pending (2), observed 2026-10-16. |
| 80 | Twelve missing report fields each become zero arrays, masking missing components/totals. Occurrences 436-447. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L436-L447] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 81 | Plot missing radiation, viscous or total each becomes zero series. Occurrences 501,509,517. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L501-L501][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L509-L509][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L517-L517] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 82 | Percentage nanmean may emit NaN; empty critical ratio reports numeric zero. Occurrences 537,598. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L537-L537][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L598-L598] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 1 | Benign compatibility import retry; the same import is retried and propagates if unavailable. Occurrences 20-21. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L20-L21] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 2 | Benign compatibility import retry; same import retried, failure propagates. Occurrences 13-14. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L13-L14] | no detection check exists; issue pending (2), observed 2026-10-16. |
| 3 | Benign compatibility import retry; same import retried, failure propagates. Occurrences 85-86. [src/digitalmodel/hydrodynamics/aqwa/aqwa_router.py:L85-L86] | no detection check exists; issue pending (4), observed 2026-10-16. |
| 4 | Benign optional data-exploration import failure stores False/None; de is initialized conditionally at 41 and not used elsewhere in this file. Occurrences 10-12. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L10-L12] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 11 | Benign failure reporting: configuration exception prints and exits 1. Occurrences 135-137. [src/digitalmodel/hydrodynamics/aqwa/aqwa_cli.py:L135-L137] | no detection check exists; issue pending (4), observed 2026-10-16. |
| 12 | Benign failure reporting: both ValueError and general error print and exit 1. Occurrences 223-231. [src/digitalmodel/hydrodynamics/aqwa/aqwa_cli.py:L223-L231] | no detection check exists; issue pending (4), observed 2026-10-16. |
| 27 | Benign explicit save-disabled None filename; no write requested. Occurrences 220-228. [src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L220-L228] | AQ-12 covers save-disabled None; benign option behavior; issue pending (1), observed 2026-10-16. |
| 29 | Benign zero message counter initializes count; files with unrecognized warning/error formats can appear clear because regex matches only fixed format. Occurrences 34-50. [src/digitalmodel/hydrodynamics/aqwa/mes_files.py:L34-L50] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 30 | Benign known scalar-DataFrame fallback; unrelated exceptions re-raised. Occurrences 59-63. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L59-L63] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 38 | Benign protocol success-code initializers; UF1/UF2 separately set error on wrong file, other callbacks leave success. Occurrences 134,171,231,247,346,399,456. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L134-L134][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L171-L171][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L231-L231][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L247-L247][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L346-L346][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L399-L399][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L456-L456] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 39 | Benign no-contact zero forces, ramp time origin, accumulator and heartbeat counter; no missing-data substitution. Occurrences 305-307,334,522,597-601. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L305-L307][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L334-L334][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L522-L522][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L597-L601] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 50 | Benign deck index int conversion constrained by digit regex, deck-header/pre-header skipping and option-keyword skipping; header reset can replace earlier same-number deck. Occurrences 574,576,578,591. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L574-L574][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L576-L576][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L578-L578][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L591-L591] | no detection check exists; issue pending (1), observed 2026-10-16. |
| 59 | Benign receive errors close connection and re-raise; short reads are not retried before unpack. Occurrences 157-161,171-175,191-195. [src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L157-L161][src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L171-L175][src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L191-L195] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 62 | Benign destructor socket-error suppression; other cleanup errors propagate. Occurrences 237-238. [src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L237-L238] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 63 | Benign size accumulator, identity off-diagonals, unpopulated state, protocol version, zero force/mass containers and sample success flag; default rotation path separately fails on None at 259. Occurrences 152,248-255,310-316,391,551-556,584,607. [src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L152-L152][src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L248-L255][src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L310-L316][src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L391-L391][src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L551-L556][src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L584-L584][src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L607-L607] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 64 | Benign numeric-to-float serialization/coefficient coercion; not external text parsing. Occurrences 530,562,588. [src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L530-L530][src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L562-L562][src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L588-L588] | no detection check exists; issue pending (3), observed 2026-10-16. |
| 71 | Benign default lower bounds/time counter; range dimensions are only partly labelled and validator does not apply them. Occurrences 33,35,37,38-40,76. [src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/models.py:L33-L33][src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/models.py:L35-L35][src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/models.py:L37-L37][src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/models.py:L38-L40][src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/models.py:L76-L76] | no detection check exists; issue pending (4), observed 2026-10-16. |

Additional confirmed defects and incident leads:

| Symptom / scope | Detection / incident |
| --- | --- |
| Preprocessing master-summary branch calls an unbound merge helper; postprocess visualizations calls an unbound visualization helper. [src/digitalmodel/hydrodynamics/aqwa/aqwa_pre_process.py:L24-L37][src/digitalmodel/hydrodynamics/aqwa/aqwa_post_process.py:L28-L30] | no detection check exists; issue pending (4). |
| Dynamic deck-15 handler output is overwritten by a specialized position serializer. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L403-L413] | no detection check exists; issue pending (1). |
| Default `RotationMatrix()` indexes None after initializing identity; `Inv` invokes that default constructor. [src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L244-L297] | no detection check exists; issue pending (3). |
| `transform_by_data_key` can replace the dataframe with None for a disabled velocity or acceleration output. [src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L173-L216] | no detection check exists; issue pending (1). |
| CLI tests use a blanket solver skip; the ten functions include help/list/error handling, and several reference an older script location. [tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py:L19-L22][tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py:L27-L50][tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py:L89-L130] | no detection check exists; issue pending (4); existing skip markers are unchanged. |
| Real-data extraction test converts any extraction exception into a skip. [tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py:L96-L116] | no detection check exists; issue pending (4). |
| CoG/CoB, mass/displacement and external-force input/output checks requested by #51 are not implemented by the placeholder validator. [src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/validator.py:L42-L51] | AQ-05/AQ-06 define manual criteria; no detection check exists in that validator; #51, issue lead, not a proven native incident. |
| External-force history recording addressed by #55: closely spaced timestamps can collapse after rounding. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L382-L394] | no detection check exists; #55. |
| Deck-10/11 coefficient work in #52 is an existing feature lead; six force/moment channels are serialized separately. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L313-L371] | no defect is inferred from the issue title; feature lead #52. |
| Executable/license diagnostics in #1521 concern the neighbouring diffraction runner, rather than qualification of this package's unchecked launch status. [src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L59-L97] | AQ-04 defines native readback; no current native check was performed; #1521, external lead. |

## 7. Lessons log

| Date | Entry | Lesson / evidence | Pack change / shared entry |
| --- | --- | --- | --- |
| 2026-10-16 | Initial pack | A completion label is separate from native readback; errors can be suppressed before result publication. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis.py:L35-L39][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L80-L96] | Section 6 records each sweep candidate; shared-log entry not created, issue pending (6). |
| 2026-10-16 | Source precedence | Synthetic tests confirm intended restoring precedence only, without a physical independent reference. [tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L127-L151] | Section 4 calls these regression examples; benchmark gap issue pending (6). |

## 8. Ownership

| Responsibility | Role / route |
| --- | --- |
| Family quality review | Diffraction quality manager, using AQG r0 sections 8 and 11 |
| Deputy quality review | UNVERIFIED: U3 — deputy appointment and second-provider clearance have not been established; issue pending (6) |
| Production and maintenance | Hydrodynamics maintainer; proposed documentation responsibility |
| Classification disagreement | Family quality manager decides whether a definition is a further implementation |
| Missing basis, standards or tolerances | Refer to the family quality manager; maintainer resolves cross-family scope |

These roles describe the intended review route; the pack status remains DRAFT until an independent decision is recorded.

## 9. Dependencies and implementation inventory

### Runtime and solver boundaries

| Dependency | Concrete call route / inventory relation |
| --- | --- |
| assetutilities | File management, ASCII/YAML/CSV handling, dictionary updates and plotting services; no dependency version is qualified by these imports. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L91-L144][src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L41-L95][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L60-L81] |
| marine analysis enhanced parser | `ViscousDampingDetermination._process_case` consumes RAOs, radiation, added mass, restoring and drag extracts from `AQWAEnhancedParser`. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L32-L36][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L196-L219] |
| AQWA executable | `get_aqwa_exe` resolves the configured installation, `run_aqwa_analysis_as_subprocess` launches it, and the external-force router requests a detached run. [src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L59-L97][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L80-L96] |
| AQWA reader/workbench | Installed reader and workbench are resolved before PLT extraction; stdout and optional CSV use subprocesses. [src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L37-L57][src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L29-L33][src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L114-L164] |
| OrcaFlex / OrcFxAPI | A guarded module-level import and a callable licence probe remain in utilities; neither is treated as a qualified output route. [src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L10-L35] |
| External-force protocol | `AqwaServerMgr` binds a socket and serializes native packets; `AqwaUserForceServer.Run` supplies callback forces and added mass. [src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L101-L130][src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L197-L228][src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L390-L473] |
| Versions / datasets | No installed solver version or client-derived dataset is qualified here; test inputs in section 4 are synthetic. [tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L17-L99] |

### Every solver-name occurrence, text only

The following roster assigns the `OrcaFlex|OrcFxAPI|OrcaWave|AQWA` case-insensitive substring matches to source definitions; package-local imports and class names are text references, not native execution. It records 223 source lines in 74 definition/file groups, with no OrcaWave occurrence.

| File / definition | Matching line numbers and citation |
| --- | --- |
| `__init__.py` / `module-level statements` | 1, 3, 4, 12. [src/digitalmodel/hydrodynamics/aqwa/__init__.py:L1-L12] |
| `aqwa_analysis.py` / `module-level statements` | 3, 4, 5. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis.py:L3-L5] |
| `aqwa_analysis.py` / `AqwaAnalysis` | 8. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis.py:L8-L8] |
| `aqwa_analysis.py` / `analysis_router` | 17, 22, 24, 31, 33. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis.py:L17-L33] |
| `aqwa_analysis_damping.py` / `module-level statements` | 12, 14, 16. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L12-L16] |
| `aqwa_analysis_damping.py` / `AqwaDamping` | 19. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L19-L19] |
| `aqwa_analysis_ef_server.py` / `module-level statements` | 26, 27, 29, 30, 33, 34, 36, 40. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L26-L40] |
| `aqwa_analysis_ef_server.py` / `AqwaEFServer` | 44. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L44-L44] |
| `aqwa_analysis_ef_server.py` / `ef_server_router` | 54, 55, 67, 68, 70, 71, 86. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L54-L86] |
| `aqwa_analysis_ef_server.py` / `save_result_df` | 113, 115. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L113-L115] |
| `aqwa_analysis_ef_server.py` / `UF1` | 144. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L144-L144] |
| `aqwa_analysis_ef_server.py` / `UF2` | 181, 189. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L181-L189] |
| `aqwa_analysis_ef_server.py` / `application-specific callback` | 424, 426. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L424-L426] |
| `aqwa_analysis_raos.py` / `module-level statements` | 19, 21, 29. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L19-L29] |
| `aqwa_analysis_raos.py` / `AqwaRAOs` | 33. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L33-L33] |
| `aqwa_analysis_raos.py` / `run_raos` | 229, 230. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L229-L230] |
| `aqwa_cli.py` / `module-level statements` | 3, 5, 13. [src/digitalmodel/hydrodynamics/aqwa/aqwa_cli.py:L3-L13] |
| `aqwa_cli.py` / `create_parser` | 20, 24, 25, 28, 31, 34. [src/digitalmodel/hydrodynamics/aqwa/aqwa_cli.py:L20-L34] |
| `aqwa_cli.py` / `list_methods` | 90, 91, 96, 100, 101, 105, 117, 118, 119. [src/digitalmodel/hydrodynamics/aqwa/aqwa_cli.py:L90-L119] |
| `aqwa_cli.py` / `run_analysis` | 172, 176, 198. [src/digitalmodel/hydrodynamics/aqwa/aqwa_cli.py:L172-L198] |
| `aqwa_dat_files.py` / `module-level statements` | 18, 29. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L18-L29] |
| `aqwa_dat_files.py` / `AqwaDATFiles` | 78. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L78-L78] |
| `aqwa_dat_files.py` / `get_dc_header` | 119. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L119-L119] |
| `aqwa_dat_files.py` / `get_dc_footer` | 134. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L134-L134] |
| `aqwa_dat_files.py` / `inspect_dat_file` | 516. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L516-L516] |
| `aqwa_dat_files.py` / `validate_ldrg_option` | 547. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L547-L547] |
| `aqwa_lis_files.py` / `module-level statements` | 12, 15. [src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L12-L15] |
| `aqwa_lis_files.py` / `AqwaLISFiles` | 20. [src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L20-L20] |
| `aqwa_post_process.py` / `module-level statements` | 4, 5, 7, 8. [src/digitalmodel/hydrodynamics/aqwa/aqwa_post_process.py:L4-L8] |
| `aqwa_post_process.py` / `AqwaPostProcess` | 11. [src/digitalmodel/hydrodynamics/aqwa/aqwa_post_process.py:L11-L11] |
| `aqwa_post_process.py` / `post_process_router` | 20. [src/digitalmodel/hydrodynamics/aqwa/aqwa_post_process.py:L20-L20] |
| `aqwa_post_process.py` / `get_visualizations` | 29. [src/digitalmodel/hydrodynamics/aqwa/aqwa_post_process.py:L29-L29] |
| `aqwa_pre_process.py` / `module-level statements` | 5, 8. [src/digitalmodel/hydrodynamics/aqwa/aqwa_pre_process.py:L5-L8] |
| `aqwa_pre_process.py` / `AqwaPreProcess` | 11. [src/digitalmodel/hydrodynamics/aqwa/aqwa_pre_process.py:L11-L11] |
| `aqwa_reader.py` / `module-level statements` | 17, 22. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L17-L22] |
| `aqwa_reader.py` / `AqwaReader` | 25. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L25-L25] |
| `aqwa_reader.py` / `router` | 30. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L30-L30] |
| `aqwa_reader.py` / `get_result_groups` | 106, 108. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L106-L108] |
| `aqwa_reader.py` / `get_result_group` | 114, 116, 118, 143, 150, 151, 162. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L114-L162] |
| `aqwa_reader.py` / `get_change_of_data_result_group` | 166, 169. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L166-L169] |
| `aqwa_reader.py` / `get_args_for_data` | 236. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L236-L236] |
| `aqwa_reader.py` / `get_property_args_array` | 272, 274, 275, 276, 277, 278. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L272-L278] |
| `aqwa_router.py` / `module-level statements` | 10, 11, 12, 17, 18. [src/digitalmodel/hydrodynamics/aqwa/aqwa_router.py:L10-L18] |
| `aqwa_router.py` / `Aqwa` | 22. [src/digitalmodel/hydrodynamics/aqwa/aqwa_router.py:L22-L22] |
| `aqwa_router.py` / `_resolve_attr` | 28. [src/digitalmodel/hydrodynamics/aqwa/aqwa_router.py:L28-L28] |
| `aqwa_router.py` / `router` | 53, 59, 84, 86, 88. [src/digitalmodel/hydrodynamics/aqwa/aqwa_router.py:L53-L88] |
| `aqwa_utilities.py` / `module-level statements` | 12, 14. [src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L12-L14] |
| `aqwa_utilities.py` / `AqwaUtilities` | 22. [src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L22-L22] |
| `aqwa_utilities.py` / `is_license_available` | 29, 30, 33, 34. [src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L29-L34] |
| `aqwa_utilities.py` / `get_aqwareader_exe` | 37, 39, 40, 42, 43, 45, 47. [src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L37-L47] |
| `aqwa_utilities.py` / `get_aqwa_exe` | 59, 61, 62, 63, 65, 67. [src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L59-L67] |
| `aqwa_utilities.py` / `run_aqwa_analysis_as_process` | 69, 71, 72. [src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L69-L72] |
| `aqwa_utilities.py` / `run_aqwa_analysis_as_subprocess` | 74, 75, 78, 79, 80, 81, 85, 92, 95, 97. [src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L74-L97] |
| `aqwa_validation/__init__.py` / `module-level statements` | 2, 3, 5, 8, 12, 14, 15, 18, 19, 23, 24, 25. [src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/__init__.py:L2-L25] |
| `aqwa_validation/models.py` / `module-level statements` | 2, 3. [src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/models.py:L2-L3] |
| `aqwa_validation/models.py` / `AQWAFileType` | 13, 14. [src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/models.py:L13-L14] |
| `aqwa_validation/models.py` / `AQWAAnalysisType` | 22, 23. [src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/models.py:L22-L23] |
| `aqwa_validation/models.py` / `AQWAValidationConfig` | 44, 45, 47, 50. [src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/models.py:L44-L50] |
| `aqwa_validation/models.py` / `AQWAValidationResult` | 55, 56, 58, 59, 62, 69, 70. [src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/models.py:L55-L70] |
| `aqwa_validation/models.py` / `to_dict` | 97, 98. [src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/models.py:L97-L98] |
| `aqwa_validation/validator.py` / `module-level statements` | 2, 3, 10. [src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/validator.py:L2-L10] |
| `aqwa_validation/validator.py` / `AQWAValidator` | 13, 15, 19. [src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/validator.py:L13-L19] |
| `aqwa_validation/validator.py` / `__init__` | 23, 25, 30. [src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/validator.py:L23-L30] |
| `aqwa_validation/validator.py` / `validate_all` | 32, 37, 44, 46. [src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/validator.py:L32-L46] |
| `ef_server/AqwaServerMgr.py` / `module-level statements` | 86, 385, 624. [src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L86-L624] |
| `ef_server/AqwaServerMgr.py` / `AqwaMsg` | 91. [src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L91-L91] |
| `ef_server/AqwaServerMgr.py` / `AqwaServerMgr` | 101. [src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L101-L101] |
| `ef_server/AqwaServerMgr.py` / `__init__` | 124, 125, 127, 128, 388. [src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L124-L388] |
| `ef_server/AqwaServerMgr.py` / `AqwaUserForceServer` | 386. [src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L386-L386] |
| `ef_server/AqwaServerMgr.py` / `Start` | 402, 426. [src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L402-L426] |
| `mes_files.py` / `router` | 166. [src/digitalmodel/hydrodynamics/aqwa/mes_files.py:L166-L166] |
| `viscous_damping_determination.py` / `module-level statements` | 1, 17, 18, 19, 20, 25. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L1-L25] |
| `viscous_damping_determination.py` / `ViscousDampingDetermination` | 29, 30. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L29-L30] |
| `viscous_damping_determination.py` / `__init__` | 34, 35, 36. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L34-L36] |

### Counted map families and exact signal search

Family rank uses in-package row count, retaining map order for ties: RAO readers/converters has 9 in-package rows of 295 total; external solver stacks has 2 in-package rows of 137 total. Both families are counted; their complete 432-row inventory follows. [docs/registry/implementation-map.yaml:L9509-L13255][docs/registry/implementation-map.yaml:L24504-L25895]

Search procedure: for each family, use its exact Python `re` search signals, case-sensitive, on all package `.py` files at the code revision; omit any signal matching `hydrodynamics/aqwa` case-insensitively. Assign each matching source line to its innermost function/class, otherwise module-level statements. Exclude no subpackage from this package's search.

Family `rao-readers-and-orcaflex-converters`; retained signals, verbatim:

```text
\.lis\b|\.hyd\b|\.owr\b|read_rao|parse_rao|read_hyd|read_wamit|read_nemoh|parse_lis|rao_data|rao_table|RAOAmplitude|RAOPhase|DisplacementRAOs|read_csv|read_excel|loadtxt|open_dataset|netCDF4|read_hydrodynamic|parse_aqwa|parse_wamit|parse_nemoh|convert_rao|RAOInterpolator|self\.content|_table_span|parse_aqwa_lis_file|AQWALISParser
ADDED MASS|RADIATION DAMPING|displacement_rao|displacementRAO|DisplacementRAOs|RAOPeriodOrFrequency|RAOAmplitude|RAOPhase|RAODirection
```

Omitted path-matching signals, verbatim:

```text
\bRAO\b|\braos?\b|AQWA|WAMIT|NEMOH|HydroD|BEMRosetta|Capytaine
```

Source: [docs/registry/implementation-map.yaml:L9509-L9515].

Family `external-solver-batch-run-stacks`; retained signals, verbatim:

```text
subprocess\.(?:run|Popen|call)|ProcessPoolExecutor|ThreadPoolExecutor|CalculateStatics|RunSimulation|run_batch|batch_run|wait_for_license|license_wait|execute_solver|run_solver|collect_results
CalculateStatics|RunSimulation|Solve|wait_for_license|license_wait|max_retries|collect_results
```

Omitted path-matching signals, verbatim:

```text
OrcFxAPI|orcaflex|aqwa|OpenFOAM|ANSYS|solver|workflows
```

Source: [docs/registry/implementation-map.yaml:L24504-L24510].

The exact grep finds 15 definitions: 7 mapped, 5 delegation/serialization/data helpers, and 3 independent stdout parsers additionally listed in section 3. Further implementations absent from both sections 3 and 9: **0**, below the permitted maximum of **1**; this is the producer's A8 result, pending independent review.

| Grep definition | Classification / evidence |
| --- | --- |
| `aqwa_analysis_raos.py` / `AqwaRAOs.get_natural_period_from_lis_file` | listed implementation. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L254-L285] |
| `aqwa_analysis_raos.py` / `AqwaRAOs.get_percent_critical_damping` | listed implementation. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L301-L334] |
| `aqwa_analysis_raos.py` / `AqwaRAOs.get_absolute_damping` | listed implementation. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L336-L356] |
| `aqwa_analysis_raos.py` / `AqwaRAOs.get_damping_values` | helper: delegation, serialization, or data holder; not another reader/converter implementation. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L528-L579] |
| `aqwa_analysis_raos.py` / `AqwaRAOs.get_raos_by_cfg` | listed implementation. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L675-L722] |
| `aqwa_dat_files.py` / `AqwaDATFiles.get_dc_10_body` | helper: delegation, serialization, or data holder; not another reader/converter implementation. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L253-L298] |
| `aqwa_dat_files.py` / `AqwaDATFiles.get_dc_15_body_generic` | helper: delegation, serialization, or data holder; not another reader/converter implementation. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L415-L450] |
| `aqwa_dat_files.py` / `specialized position serializer` | helper: delegation, serialization, or data holder; not another reader/converter implementation. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L452-L493] |
| `aqwa_reader.py` / `AqwaReader.process_equilibrium_result` | Independent parsing algorithm; explicitly inventoried in section 3 and here; map addition pending. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L290-L313] |
| `aqwa_reader.py` / `AqwaReader.process_frequency_result` | Independent parsing algorithm; explicitly inventoried in section 3 and here; map addition pending. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L315-L341] |
| `aqwa_reader.py` / `AqwaReader.process_timeresponse_result` | Independent parsing algorithm; explicitly inventoried in section 3 and here; map addition pending. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L343-L371] |
| `aqwa_validation/models.py` / `AQWAValidationConfig` | helper: delegation, serialization, or data holder; not another reader/converter implementation. [src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/models.py:L44-L51] |
| `viscous_damping_determination.py` / `ViscousDampingDetermination._process_case` | listed implementation. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L176-L294] |
| `aqwa_reader.py` / `AqwaReader.get_result_group` | listed implementation. [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L114-L164] |
| `aqwa_utilities.py` / `AqwaUtilities.run_aqwa_analysis_as_subprocess` | listed implementation. [src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L74-L97] |

### Complete map inventory

Every row below identifies one implementation row at the map revision, preserving map order and its recorded relation. Map inventory is not a correctness endorsement, nor a claim that all paths exist unchanged at the newer code pin. Two identifying discovery paths are withheld and retain their exact map-row locators; codenaming is issue pending (5).

#### rao-readers-and-orcaflex-converters

| Row | Implementation path and symbol at map data revision | Relation / map locator |
| --- | --- | --- |
| 1 | `docs/domains/diffraction/examples/orcawave_conversion_example.py` / `main` | independent; [docs/registry/implementation-map.yaml:L9516-L9527] |
| 2 | `docs/domains/orcawave/L01_aqwa_benchmark/compare_model_inputs.py` / `main` | independent; [docs/registry/implementation-map.yaml:L9528-L9539] |
| 3 | `docs/domains/orcawave/L01_aqwa_benchmark/create_interactive_rao_comparison.py` / `extract_aqwa_raos` | independent; [docs/registry/implementation-map.yaml:L9540-L9551] |
| 4 | `docs/domains/orcawave/L01_aqwa_benchmark/create_interactive_rao_comparison.py` / `extract_orcawave_raos` | independent; [docs/registry/implementation-map.yaml:L9552-L9563] |
| 5 | `docs/domains/orcawave/L01_aqwa_benchmark/create_interactive_rao_comparison.py` / `main` | independent; [docs/registry/implementation-map.yaml:L9564-L9578] |
| 6 | `docs/domains/orcawave/L01_aqwa_benchmark/extract_orcawave_raos.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L9579-L9589] |
| 7 | `docs/domains/orcawave/L01_aqwa_benchmark/extract_orcawave_raos.py` / `extract_orcawave_raos` | independent; [docs/registry/implementation-map.yaml:L9590-L9601] |
| 8 | `docs/domains/orcawave/L01_aqwa_benchmark/monitor_orcawave_run.py` / `monitor_progress` | independent; [docs/registry/implementation-map.yaml:L9602-L9613] |
| 9 | `docs/domains/orcawave/L01_aqwa_benchmark/run_comparison.py` / `extract_aqwa_data` | independent; [docs/registry/implementation-map.yaml:L9614-L9625] |
| 10 | `docs/domains/orcawave/L01_aqwa_benchmark/run_comparison.py` / `extract_orcawave_data` | independent; [docs/registry/implementation-map.yaml:L9626-L9637] |
| 11 | `docs/domains/orcawave/L01_aqwa_benchmark/run_comparison.py` / `main` | independent; [docs/registry/implementation-map.yaml:L9638-L9652] |
| 12 | `docs/domains/orcawave/L01_aqwa_benchmark/run_comparison_with_interpolation.py` / `extract_orcawave_raos` | independent; [docs/registry/implementation-map.yaml:L9653-L9664] |
| 13 | `docs/domains/orcawave/L01_aqwa_benchmark/run_comparison_with_interpolation.py` / `main` | independent; [docs/registry/implementation-map.yaml:L9665-L9678] |
| 14 | `docs/domains/orcawave/L01_aqwa_benchmark/run_heading_specific_comparison.py` / `extract_orcawave_raos` | independent; [docs/registry/implementation-map.yaml:L9679-L9690] |
| 15 | `docs/domains/orcawave/L01_aqwa_benchmark/run_heading_specific_comparison.py` / `main` | independent; [docs/registry/implementation-map.yaml:L9691-L9704] |
| 16 | `docs/domains/orcawave/L01_aqwa_benchmark/run_orcawave_api.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L9705-L9715] |
| 17 | `docs/domains/orcawave/L01_aqwa_benchmark/run_orcawave_matched_api.py` / `main` | independent; [docs/registry/implementation-map.yaml:L9716-L9727] |
| 18 | `docs/domains/orcawave/L01_aqwa_benchmark/run_proper_comparison.py` / `extract_orcawave_motion_raos` | independent; [docs/registry/implementation-map.yaml:L9728-L9739] |
| 19 | `docs/domains/orcawave/L01_aqwa_benchmark/run_proper_comparison.py` / `main` | independent; [docs/registry/implementation-map.yaml:L9740-L9753] |
| 20 | `docs/domains/orcawave/L01_aqwa_benchmark/validate_matched_model.py` / `main` | independent; [docs/registry/implementation-map.yaml:L9754-L9765] |
| 21 | `docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_diffraction.py` / `run_orcawave_diffraction` | independent; [docs/registry/implementation-map.yaml:L9766-L9777] |
| 22 | `docs/domains/orcawave/examples/qa/orcawave_example_qa.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L9778-L9788] |
| 23 | `docs/domains/orcawave/examples/qa/orcawave_example_qa.py` / `_check_raos_finite` | independent; [docs/registry/implementation-map.yaml:L9789-L9800] |
| 24 | `docs/domains/orcawave/examples/qa/orcawave_example_qa.py` / `_check_heave_rao_long_period` | independent; [docs/registry/implementation-map.yaml:L9801-L9812] |
| 25 | `examples/domains/core/cache_example.py` / `example_realistic_usage.load_hydrodynamic_database` | example; [docs/registry/implementation-map.yaml:L9813-L9826] |
| 26 | `examples/domains/validation/validation_pipeline_example.py` / `example_rao_polar_data_validation` | example; [docs/registry/implementation-map.yaml:L9827-L9838] |
| 27 | `scripts/benchmark/compare_hydro_properties.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L9839-L9849] |
| 28 | `scripts/benchmark/compare_hydro_properties.py` / `extract_aqwa_damping` | independent; [docs/registry/implementation-map.yaml:L9850-L9861] |
| 29 | `scripts/benchmark/compare_hydro_properties.py` / `extract_aqwa_added_mass` | independent; [docs/registry/implementation-map.yaml:L9862-L9873] |
| 30 | `scripts/benchmark/compare_hydro_properties.py` / `parse_args` | independent; [docs/registry/implementation-map.yaml:L9874-L9885] |
| 31 | `scripts/benchmark/digitize_validation_plots.py` / `parse_args` | independent; [docs/registry/implementation-map.yaml:L9886-L9897] |
| 32 | `scripts/benchmark/probe_owd_content.py` / `_extract_core_data` | independent; [docs/registry/implementation-map.yaml:L9898-L9909] |
| 33 | `scripts/benchmark/probe_owd_content.py` / `probe_body_details` | independent; [docs/registry/implementation-map.yaml:L9910-L9921] |
| 34 | `scripts/benchmark/qtf_postprocessing.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L9922-L9932] |
| 35 | `scripts/benchmark/regenerate_barge_benchmark.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L9933-L9943] |
| 36 | `scripts/benchmark/regenerate_ship_benchmark.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L9944-L9954] |
| 37 | `scripts/benchmark/rerun_benchmarks_with_fidp.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L9955-L9965] |
| 38 | `scripts/benchmark/rerun_benchmarks_with_fidp.py` / `_extract_correlations_from_json` | independent; [docs/registry/implementation-map.yaml:L9966-L9977] |
| 39 | `scripts/benchmark/rerun_benchmarks_with_fidp.py` / `_extract_correlations_from_runner` | independent; [docs/registry/implementation-map.yaml:L9978-L9989] |
| 40 | `scripts/benchmark/rerun_benchmarks_with_fidp.py` / `_run_ship_benchmark` | independent; [docs/registry/implementation-map.yaml:L9990-L10004] |
| 41 | `scripts/benchmark/rerun_benchmarks_with_fidp.py` / `_run_spar_benchmark` | independent; [docs/registry/implementation-map.yaml:L10005-L10017] |
| 42 | `scripts/benchmark/rerun_benchmarks_with_fidp.py` / `main` | independent; [docs/registry/implementation-map.yaml:L10018-L10032] |
| 43 | `scripts/benchmark/run_3way_benchmark.py` / `run_orcawave` | independent; [docs/registry/implementation-map.yaml:L10033-L10046] |
| 44 | `scripts/benchmark/run_3way_benchmark.py` / `run_aqwa` | independent; [docs/registry/implementation-map.yaml:L10047-L10060] |
| 45 | `scripts/benchmark/run_3way_benchmark.py` / `_extract_from_owr` | independent; [docs/registry/implementation-map.yaml:L10061-L10072] |
| 46 | `scripts/benchmark/run_3way_benchmark.py` / `_extract_from_aqwa_lis` | independent; [docs/registry/implementation-map.yaml:L10073-L10087] |
| 47 | `scripts/benchmark/run_3way_benchmark.py` / `_parse_aqwa_raos` | independent; [docs/registry/implementation-map.yaml:L10088-L10099] |
| 48 | `scripts/benchmark/run_3way_benchmark.py` / `_parse_aqwa_matrix_section` | independent; [docs/registry/implementation-map.yaml:L10100-L10111] |
| 49 | `scripts/benchmark/run_spar_benchmark.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L10112-L10122] |
| 50 | `scripts/benchmark/run_spar_benchmark.py` / `main` | independent; [docs/registry/implementation-map.yaml:L10123-L10134] |
| 51 | `scripts/benchmark/run_validation_benchmark.py` / `_run_orcawave_for_case` | independent; [docs/registry/implementation-map.yaml:L10135-L10146] |
| 52 | `scripts/benchmark/run_validation_benchmark.py` / `_load_wamit_reference` | independent; [docs/registry/implementation-map.yaml:L10147-L10158] |
| 53 | `scripts/benchmark/solve_owd_extract.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L10159-L10169] |
| 54 | `scripts/benchmark/solve_owd_extract.py` / `solve_and_extract` | independent; [docs/registry/implementation-map.yaml:L10170-L10181] |
| 55 | `scripts/benchmark/validate_owd_vs_spec.py` / `_extract_from_diffraction` | independent; [docs/registry/implementation-map.yaml:L10182-L10193] |
| 56 | `scripts/benchmark/validate_owd_vs_spec.py` / `solve_owd` | independent; [docs/registry/implementation-map.yaml:L10194-L10207] |
| 57 | `scripts/benchmark/validate_owd_vs_spec.py` / `solve_spec` | independent; [docs/registry/implementation-map.yaml:L10208-L10222] |
| 58 | `scripts/benchmark/validate_owd_vs_spec.py` / `_solve_spec_via_orcfxapi` | independent; [docs/registry/implementation-map.yaml:L10223-L10235] |
| 59 | `scripts/build_results_workbook.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L10236-L10246] |
| 60 | `scripts/populate_rao_database.py` / `main` | independent; [docs/registry/implementation-map.yaml:L10247-L10260] |
| 61 | `scripts/python/digitalmodel/legacy/aqwa_data_helper.py` / `main` | independent; [docs/registry/implementation-map.yaml:L10261-L10272] |
| 62 | `scripts/python/digitalmodel/legacy/check_aqwa_heading_fix.py` / `test_actual_file_counts` | independent; [docs/registry/implementation-map.yaml:L10273-L10284] |
| 63 | `scripts/python/digitalmodel/legacy/check_aqwa_parser_fix.py` / `main` | independent; [docs/registry/implementation-map.yaml:L10285-L10296] |
| 64 | `scripts/python/digitalmodel/legacy/diagnose_rao_parsing.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L10297-L10307] |
| 65 | `scripts/python/digitalmodel/legacy/direct_rao_plot.py` / `parse_rao_section_simple` | independent; [docs/registry/implementation-map.yaml:L10308-L10319] |
| 66 | `scripts/python/digitalmodel/legacy/direct_rao_plot.py` / `main` | independent; [docs/registry/implementation-map.yaml:L10320-L10333] |
| 67 | `scripts/python/digitalmodel/legacy/fix_rao_plots.py` / `main` | independent; [docs/registry/implementation-map.yaml:L10334-L10345] |
| 68 | `scripts/python/digitalmodel/legacy/fix_rao_plots_standalone.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L10346-L10356] |
| 69 | `scripts/python/digitalmodel/legacy/generate_rao_qa_report.py` / `main` | independent; [docs/registry/implementation-map.yaml:L10357-L10368] |
| 70 | `scripts/python/digitalmodel/legacy/regenerate_rao_plots.py` / `main` | independent; [docs/registry/implementation-map.yaml:L10369-L10380] |
| 71 | `scripts/python/digitalmodel/legacy/simple_rao_plot.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L10381-L10391] |
| 72 | `scripts/python/digitalmodel/legacy/verify_parser_extraction.py` / `test_rao_section` | independent; [docs/registry/implementation-map.yaml:L10392-L10403] |
| 73 | `scripts/python/digitalmodel/legacy/verify_parser_extraction.py` / `main` | independent; [docs/registry/implementation-map.yaml:L10404-L10415] |
| 74 | `scripts/rao_quality_check.py` / `parse_args` | independent; [docs/registry/implementation-map.yaml:L10416-L10427] |
| 75 | `scripts/rao_quality_check.py` / `process_file` | independent; [docs/registry/implementation-map.yaml:L10428-L10439] |
| 76 | `scripts/run_benchmark_ship_raos.py` / `create_diffraction_results` | independent; [docs/registry/implementation-map.yaml:L10440-L10451] |
| 77 | `scripts/run_benchmark_ship_raos.py` / `main` | independent; [docs/registry/implementation-map.yaml:L10452-L10465] |
| 78 | `scripts/run_semisub_quality_check.py` / `main` | independent; [docs/registry/implementation-map.yaml:L10466-L10477] |
| 79 | `src/digitalmodel/data_systems/data_procurement/vessel/api_clients/generic_rao_client.py` / `GenericRAOClient._generate_generic_raos` | independent; [docs/registry/implementation-map.yaml:L10478-L10489] |
| 80 | `src/digitalmodel/data_systems/data_procurement/vessel/api_clients/generic_rao_client.py` / `GenericRAOClient.interpolate_raos` | independent; [docs/registry/implementation-map.yaml:L10490-L10501] |
| 81 | `src/digitalmodel/data_systems/data_procurement/vessel/api_clients/generic_rao_client.py` / `GenericRAOClient.stream_raos_orcaflex` | independent; [docs/registry/implementation-map.yaml:L10502-L10513] |
| 82 | `src/digitalmodel/data_systems/data_procurement/vessel/api_clients/generic_rao_client.py` / `GenericRAOClient.query_by_date` | caller; [docs/registry/implementation-map.yaml:L10514-L10527] |
| 83 | `src/digitalmodel/data_systems/data_procurement/vessel/client.py` / `VesselClient._initialize_clients` | independent; [docs/registry/implementation-map.yaml:L10528-L10539] |
| 84 | `src/digitalmodel/drilling_riser/global_model/build.py` / `_vessel_type` | independent; [docs/registry/implementation-map.yaml:L10540-L10551] |
| 85 | `src/digitalmodel/drilling_riser/global_model/raos.py` / `parse_direction_grouped_rao_text` | independent; [docs/registry/implementation-map.yaml:L10552-L10563] |
| 86 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py` / `AqwaRAOs.get_natural_period_from_lis_file` | independent; [docs/registry/implementation-map.yaml:L10564-L10575] |
| 87 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py` / `AqwaRAOs.get_percent_critical_damping` | independent; [docs/registry/implementation-map.yaml:L10576-L10587] |
| 88 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py` / `AqwaRAOs.get_absolute_damping` | independent; [docs/registry/implementation-map.yaml:L10588-L10599] |
| 89 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py` / `AqwaRAOs.get_raos_by_cfg` | independent; [docs/registry/implementation-map.yaml:L10600-L10611] |
| 90 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_cli.py` / `create_parser` | independent; [docs/registry/implementation-map.yaml:L10612-L10623] |
| 91 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_cli.py` / `list_methods` | independent; [docs/registry/implementation-map.yaml:L10624-L10635] |
| 92 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py` / `AqwaReader.get_change_of_data_result_group` | independent; [docs/registry/implementation-map.yaml:L10636-L10647] |
| 93 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py` / `AqwaReader.get_property_args_array` | independent; [docs/registry/implementation-map.yaml:L10648-L10659] |
| 94 | `src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py` / `ViscousDampingDetermination._process_case` | independent; [docs/registry/implementation-map.yaml:L10660-L10671] |
| 95 | `src/digitalmodel/hydrodynamics/bemrosetta/__init__.py` / `get_module_info` | independent; [docs/registry/implementation-map.yaml:L10672-L10683] |
| 96 | `src/digitalmodel/hydrodynamics/bemrosetta/cli.py` / `convert` | independent; [docs/registry/implementation-map.yaml:L10684-L10699] |
| 97 | `src/digitalmodel/hydrodynamics/bemrosetta/cli.py` / `_export_raos_csv` | independent; [docs/registry/implementation-map.yaml:L10700-L10711] |
| 98 | `src/digitalmodel/hydrodynamics/bemrosetta/cli.py` / `status` | independent; [docs/registry/implementation-map.yaml:L10712-L10725] |
| 99 | `src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py` / `OrcaFlexConverter._call_orcaflex_exporter` | independent; [docs/registry/implementation-map.yaml:L10726-L10737] |
| 100 | `src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py` / `OrcaFlexConverter._build_vessel_yaml` | independent; [docs/registry/implementation-map.yaml:L10738-L10749] |
| 101 | `src/digitalmodel/hydrodynamics/bemrosetta/mesh/dat_handler.py` / `DATHandler._parse_aqwa_deck` | independent; [docs/registry/implementation-map.yaml:L10750-L10761] |
| 102 | `src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py` / `AQWAParser.supported_extensions` | independent; [docs/registry/implementation-map.yaml:L10762-L10775] |
| 103 | `src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py` / `AQWAParser.parse` | independent; [docs/registry/implementation-map.yaml:L10776-L10789] |
| 104 | `src/digitalmodel/hydrodynamics/bemrosetta/parsers/aqwa_parser.py` / `AQWAParser._extract_raos` | independent; [docs/registry/implementation-map.yaml:L10790-L10801] |
| 105 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` / `AQWAConverter._extract_rao_data` | caller; [docs/registry/implementation-map.yaml:L10802-L10815] |
| 106 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` / `AQWAConverter._extract_added_mass_data` | caller; [docs/registry/implementation-map.yaml:L10816-L10828] |
| 107 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` / `AQWAConverter._extract_damping_data` | caller; [docs/registry/implementation-map.yaml:L10829-L10841] |
| 108 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` / `AQWAConverter._build_rao_set` | independent; [docs/registry/implementation-map.yaml:L10842-L10853] |
| 109 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` / `AQWAConverter._build_added_mass_set` | independent; [docs/registry/implementation-map.yaml:L10854-L10865] |
| 110 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_converter.py` / `AQWAConverter._build_damping_set` | independent; [docs/registry/implementation-map.yaml:L10866-L10877] |
| 111 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` / `AQWALISParser.extract_frequencies_and_periods` | independent; [docs/registry/implementation-map.yaml:L10878-L10891] |
| 112 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` / `AQWALISParser.extract_headings` | independent; [docs/registry/implementation-map.yaml:L10892-L10905] |
| 113 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` / `AQWALISParser._table_span` | independent; [docs/registry/implementation-map.yaml:L10906-L10917] |
| 114 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` / `AQWALISParser.parse_added_mass_table` | caller; [docs/registry/implementation-map.yaml:L10918-L10933] |
| 115 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` / `AQWALISParser.parse_damping_table` | caller; [docs/registry/implementation-map.yaml:L10934-L10948] |
| 116 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` / `AQWALISParser.parse_rao_table` | independent; [docs/registry/implementation-map.yaml:L10949-L10962] |
| 117 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` / `AQWALISParser.parse_all` | wrapper; [docs/registry/implementation-map.yaml:L10963-L10979] |
| 118 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` / `parse_aqwa_lis_file` | independent; [docs/registry/implementation-map.yaml:L10980-L10991] |
| 119 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` / `extract_aqwa_frequencies` | independent; [docs/registry/implementation-map.yaml:L10992-L11003] |
| 120 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` / `extract_aqwa_headings` | independent; [docs/registry/implementation-map.yaml:L11004-L11015] |
| 121 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` / `extract_aqwa_added_mass` | independent; [docs/registry/implementation-map.yaml:L11016-L11027] |
| 122 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` / `extract_aqwa_damping` | independent; [docs/registry/implementation-map.yaml:L11028-L11039] |
| 123 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_lis_parser.py` / `extract_aqwa_raos` | independent; [docs/registry/implementation-map.yaml:L11040-L11051] |
| 124 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_result_extractor.py` / `AQWAResultExtractor.extract` | independent; [docs/registry/implementation-map.yaml:L11052-L11063] |
| 125 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_runner.py` / `AQWARunner._capture_lis_file` | independent; [docs/registry/implementation-map.yaml:L11064-L11075] |
| 126 | `src/digitalmodel/hydrodynamics/diffraction/cli.py` / `convert_aqwa` | independent; [docs/registry/implementation-map.yaml:L11076-L11089] |
| 127 | `src/digitalmodel/hydrodynamics/diffraction/diffraction_cli.py` / `create_parser` | independent; [docs/registry/implementation-map.yaml:L11090-L11101] |
| 128 | `src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py` / `OrcaFlexExporter.export_raos_csv` | independent; [docs/registry/implementation-map.yaml:L11102-L11113] |
| 129 | `src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py` / `OrcaFlexExporter._write_rao_sheet` | independent; [docs/registry/implementation-map.yaml:L11114-L11125] |
| 130 | `src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py` / `OrcaFlexExporter._write_discretization_sheet` | independent; [docs/registry/implementation-map.yaml:L11126-L11137] |
| 131 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` / `OrcaWaveRunner._execute_via_api` | independent; [docs/registry/implementation-map.yaml:L11138-L11149] |
| 132 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` / `OrcaWaveRunner._read_live_attrs` | independent; [docs/registry/implementation-map.yaml:L11150-L11163] |
| 133 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` / `OrcaWaveRunner._build_results_from_object` | independent; [docs/registry/implementation-map.yaml:L11164-L11175] |
| 134 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_test_utilities.py` / `MockDataGenerator.create_mock_vessel` | independent; [docs/registry/implementation-map.yaml:L11176-L11187] |
| 135 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_to_orcaflex.py` / `convert_orcawave_xlsx_to_orcaflex` | independent; [docs/registry/implementation-map.yaml:L11188-L11202] |
| 136 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_to_orcaflex.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L11203-L11213] |
| 137 | `src/digitalmodel/hydrodynamics/diffraction/polars_exporter.py` / `PolarsExporter.export_all_csv` | independent; [docs/registry/implementation-map.yaml:L11214-L11225] |
| 138 | `src/digitalmodel/hydrodynamics/diffraction/polars_exporter.py` / `PolarsExporter._build_rao_records` | independent; [docs/registry/implementation-map.yaml:L11226-L11237] |
| 139 | `src/digitalmodel/hydrodynamics/diffraction/report_generator.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L11238-L11248] |
| 140 | `src/digitalmodel/hydrodynamics/diffraction/reverse_parsers.py` / `OrcaWaveInputParser.parse` | independent; [docs/registry/implementation-map.yaml:L11249-L11260] |
| 141 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py` / `OrcaWaveConverter._extract_rao_data` | caller; [docs/registry/implementation-map.yaml:L11261-L11274] |
| 142 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py` / `OrcaWaveConverter._build_rao_set` | independent; [docs/registry/implementation-map.yaml:L11275-L11286] |
| 143 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py` / `OrcaWaveConverter._build_added_mass_set` | independent; [docs/registry/implementation-map.yaml:L11287-L11298] |
| 144 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_converter.py` / `OrcaWaveConverter._build_damping_set` | independent; [docs/registry/implementation-map.yaml:L11299-L11310] |
| 145 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_data_extraction.py` / `OrcaWaveDataExtractor.extract_rao_frequencies` | independent; [docs/registry/implementation-map.yaml:L11311-L11322] |
| 146 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_data_extraction.py` / `OrcaWaveDataExtractor.extract_rao_headings` | independent; [docs/registry/implementation-map.yaml:L11323-L11334] |
| 147 | `src/digitalmodel/hydrodynamics/diffraction/solver/orcawave_data_extraction.py` / `extract_all_rao_data` | independent; [docs/registry/implementation-map.yaml:L11335-L11346] |
| 148 | `src/digitalmodel/hydrodynamics/diffraction/wamit_reference_loader.py` / `WamitReferenceLoader.from_owd` | independent; [docs/registry/implementation-map.yaml:L11347-L11360] |
| 149 | `src/digitalmodel/hydrodynamics/diffraction/wamit_reference_loader.py` / `WamitReferenceLoader.from_yaml` | independent; [docs/registry/implementation-map.yaml:L11361-L11374] |
| 150 | `src/digitalmodel/hydrodynamics/diffraction/wamit_reference_loader.py` / `WamitReferenceLoader._build_results` | independent; [docs/registry/implementation-map.yaml:L11375-L11388] |
| 151 | `src/digitalmodel/hydrodynamics/hull_library/analysis_setup.py` / `_attempt_rao_lookup` | independent; [docs/registry/implementation-map.yaml:L11389-L11400] |
| 152 | `src/digitalmodel/hydrodynamics/hull_library/analysis_setup.py` / `setup_hull_analysis` | caller; [docs/registry/implementation-map.yaml:L11401-L11414] |
| 153 | `src/digitalmodel/hydrodynamics/hull_library/catalog.py` / `HullCatalog.compute_motions` | independent; [docs/registry/implementation-map.yaml:L11415-L11426] |
| 154 | `src/digitalmodel/hydrodynamics/hull_library/catalog.py` / `HullCatalog.compute_accelerations` | independent; [docs/registry/implementation-map.yaml:L11427-L11438] |
| 155 | `src/digitalmodel/hydrodynamics/hull_library/rao_database.py` / `RAODatabase.save_to_disk` | independent; [docs/registry/implementation-map.yaml:L11439-L11450] |
| 156 | `src/digitalmodel/hydrodynamics/hull_library/rao_database.py` / `RAODatabase.load_from_disk` | independent; [docs/registry/implementation-map.yaml:L11451-L11462] |
| 157 | `src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py` / `PipelineXlsxReader.read_rao_data` | independent; [docs/registry/implementation-map.yaml:L11463-L11474] |
| 158 | `src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py` / `NativeXlsxReader.read_rao_data` | independent; [docs/registry/implementation-map.yaml:L11475-L11486] |
| 159 | `src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py` / `xlsx_to_rao_data` | independent; [docs/registry/implementation-map.yaml:L11487-L11498] |
| 160 | `src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py` / `xlsx_to_hydro_coefficients` | independent; [docs/registry/implementation-map.yaml:L11499-L11510] |
| 161 | `src/digitalmodel/hydrodynamics/hull_library/rao_extractor.py` / `populate_database_from_xlsx` | independent; [docs/registry/implementation-map.yaml:L11511-L11524] |
| 162 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_comparison.py` / `compare_box_exports` | independent; [docs/registry/implementation-map.yaml:L11525-L11536] |
| 163 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py` / `_read_units` | independent; [docs/registry/implementation-map.yaml:L11537-L11548] |
| 164 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py` / `_same_content` | independent; [docs/registry/implementation-map.yaml:L11549-L11560] |
| 165 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py` / `store_fixture_entry` | caller; [docs/registry/implementation-map.yaml:L11561-L11574] |
| 166 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py` / `populate_fixture_database` | independent; [docs/registry/implementation-map.yaml:L11575-L11590] |
| 167 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py` / `_verify_readback` | caller; [docs/registry/implementation-map.yaml:L11591-L11603] |
| 168 | `src/digitalmodel/hydrodynamics/hull_library/rao_fixture_population.py` / `regenerate_fixture_database` | independent; [docs/registry/implementation-map.yaml:L11604-L11619] |
| 169 | `src/digitalmodel/hydrodynamics/hull_library/rao_registry.py` / `RaoRegistry.register_rao` | caller; [docs/registry/implementation-map.yaml:L11620-L11633] |
| 170 | `src/digitalmodel/hydrodynamics/hull_library/rao_registry.py` / `RaoRegistry._store_content` | independent; [docs/registry/implementation-map.yaml:L11634-L11648] |
| 171 | `src/digitalmodel/hydrodynamics/hull_library/rao_registry.py` / `RaoRegistry.load_rao_data` | caller; [docs/registry/implementation-map.yaml:L11649-L11662] |
| 172 | `src/digitalmodel/hydrodynamics/hull_library/rao_registry.py` / `_read_payload` | independent; [docs/registry/implementation-map.yaml:L11663-L11674] |
| 173 | `src/digitalmodel/hydrodynamics/interpolator.py` / `CoefficientsInterpolator.load_raos` | independent; [docs/registry/implementation-map.yaml:L11675-L11686] |
| 174 | `src/digitalmodel/hydrodynamics/interpolator.py` / `CoefficientsInterpolator.interpolate_rao_1d` | independent; [docs/registry/implementation-map.yaml:L11687-L11698] |
| 175 | `src/digitalmodel/hydrodynamics/interpolator.py` / `CoefficientsInterpolator.interpolate_rao_2d` | independent; [docs/registry/implementation-map.yaml:L11699-L11710] |
| 176 | `src/digitalmodel/hydrodynamics/interpolator.py` / `CoefficientsInterpolator.interpolate_all_dofs` | caller; [docs/registry/implementation-map.yaml:L11711-L11724] |
| 177 | `src/digitalmodel/hydrodynamics/interpolator.py` / `CoefficientsInterpolator.extract_rao_at_frequency` | independent; [docs/registry/implementation-map.yaml:L11725-L11736] |
| 178 | `src/digitalmodel/hydrodynamics/interpolator.py` / `CoefficientsInterpolator.extract_rao_at_direction` | independent; [docs/registry/implementation-map.yaml:L11737-L11748] |
| 179 | `src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py` / `run_parametric_sweep` | independent; [docs/registry/implementation-map.yaml:L11749-L11762] |
| 180 | `src/digitalmodel/hydrodynamics/parametric_hull_analysis/sweep.py` / `_to_rao_db_entry` | independent; [docs/registry/implementation-map.yaml:L11763-L11774] |
| 181 | `src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py` / `RAOAnalysis.read_orcaflex_displacement_raos` | independent; [docs/registry/implementation-map.yaml:L11775-L11786] |
| 182 | `src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py` / `RAOAnalysis.get_rao_data_for_vessel` | independent; [docs/registry/implementation-map.yaml:L11787-L11798] |
| 183 | `src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py` / `router` | caller; [docs/registry/implementation-map.yaml:L11799-L11812] |
| 184 | `src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py` / `_database` | independent; [docs/registry/implementation-map.yaml:L11813-L11824] |
| 185 | `src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py` / `_rao_array` | independent; [docs/registry/implementation-map.yaml:L11825-L11836] |
| 186 | `src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py` / `_interpolated_rows` | caller; [docs/registry/implementation-map.yaml:L11837-L11850] |
| 187 | `src/digitalmodel/hydrodynamics/rao_tabulation/workflow.py` / `_check_query_bounds` | independent; [docs/registry/implementation-map.yaml:L11851-L11862] |
| 188 | `src/digitalmodel/installation/installation_pamphlet/calculations.py` / `_extract_rao_dict` | independent; [docs/registry/implementation-map.yaml:L11863-L11874] |
| 189 | `src/digitalmodel/installation/installation_pamphlet/calculations.py` / `load_rao_set_from_artifact` | independent; [docs/registry/implementation-map.yaml:L11875-L11888] |
| 190 | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` / `crane_tip_raos` | independent; [docs/registry/implementation-map.yaml:L11889-L11900] |
| 191 | `src/digitalmodel/marine_ops/installation/models.py` / `Vessel.has_complete_raos` | independent; [docs/registry/implementation-map.yaml:L11901-L11912] |
| 192 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py` / `AQWAEnhancedParser.parse_lis_file` | caller; [docs/registry/implementation-map.yaml:L11913-L11926] |
| 193 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py` / `AQWAEnhancedParser._parse_rao_section_enhanced` | independent; [docs/registry/implementation-map.yaml:L11927-L11938] |
| 194 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py` / `AQWAEnhancedParser._extract_section_lines` | independent; [docs/registry/implementation-map.yaml:L11939-L11950] |
| 195 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py` / `AQWAEnhancedParser.extract_drag_table` | caller; [docs/registry/implementation-map.yaml:L11951-L11964] |
| 196 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py` / `AQWAEnhancedParser.export_to_csv` | independent; [docs/registry/implementation-map.yaml:L11965-L11976] |
| 197 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py` / `AQWAReader.parse_lis_file` | caller; [docs/registry/implementation-map.yaml:L11977-L11990] |
| 198 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py` / `AQWAReader._parse_rao_section` | independent; [docs/registry/implementation-map.yaml:L11991-L12002] |
| 199 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py` / `AQWAReader._extract_last_rao_section` | independent; [docs/registry/implementation-map.yaml:L12003-L12014] |
| 200 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py` / `AQWAReader._parse_frequencies` | independent; [docs/registry/implementation-map.yaml:L12015-L12026] |
| 201 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader.py` / `AQWAReader._parse_headings` | independent; [docs/registry/implementation-map.yaml:L12027-L12038] |
| 202 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader_fixed.py` / `AQWAReaderFixed.parse_lis_file` | caller; [docs/registry/implementation-map.yaml:L12039-L12052] |
| 203 | `src/digitalmodel/marine_ops/marine_analysis/aqwa_reader_fixed.py` / `AQWAReaderFixed._parse_rao_section_fixed` | independent; [docs/registry/implementation-map.yaml:L12053-L12064] |
| 204 | `src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py` / `OrcaFlexReader.parse_yml_file` | caller; [docs/registry/implementation-map.yaml:L12065-L12078] |
| 205 | `src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py` / `OrcaFlexReader._parse_orcaflex_rao_data` | wrapper; [docs/registry/implementation-map.yaml:L12079-L12095] |
| 206 | `src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py` / `OrcaFlexReader._extract_headings` | independent; [docs/registry/implementation-map.yaml:L12096-L12107] |
| 207 | `src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py` / `OrcaFlexReader._extract_frequencies` | independent; [docs/registry/implementation-map.yaml:L12108-L12119] |
| 208 | `src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py` / `OrcaFlexReader._fill_rao_values` | independent; [docs/registry/implementation-map.yaml:L12120-L12131] |
| 209 | `src/digitalmodel/marine_ops/marine_analysis/orcaflex_reader.py` / `OrcaFlexReader._parse_table_format` | independent; [docs/registry/implementation-map.yaml:L12132-L12143] |
| 210 | `src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py` / `AQWALISParser.parse_lis_file` | caller; [docs/registry/implementation-map.yaml:L12144-L12159] |
| 211 | `src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py` / `AQWALISParser._extract_displacement_raos` | caller; [docs/registry/implementation-map.yaml:L12160-L12173] |
| 212 | `src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py` / `AQWALISParser._extract_velocity_raos` | caller; [docs/registry/implementation-map.yaml:L12174-L12186] |
| 213 | `src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py` / `AQWALISParser._extract_acceleration_raos` | caller; [docs/registry/implementation-map.yaml:L12187-L12199] |
| 214 | `src/digitalmodel/marine_ops/marine_analysis/parsers/aqwa_lis_parser.py` / `AQWALISParser._parse_rao_section` | independent; [docs/registry/implementation-map.yaml:L12200-L12211] |
| 215 | `src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py` / `OrcaFlexYMLParser.parse_yml_file` | caller; [docs/registry/implementation-map.yaml:L12212-L12226] |
| 216 | `src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py` / `OrcaFlexYMLParser._navigate_to_draught` | independent; [docs/registry/implementation-map.yaml:L12227-L12238] |
| 217 | `src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py` / `OrcaFlexYMLParser._parse_orcaflex_rao_structure` | caller; [docs/registry/implementation-map.yaml:L12239-L12254] |
| 218 | `src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py` / `OrcaFlexYMLParser._extract_headings` | copy; [docs/registry/implementation-map.yaml:L12255-L12266] |
| 219 | `src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py` / `OrcaFlexYMLParser._extract_frequencies` | independent; [docs/registry/implementation-map.yaml:L12267-L12278] |
| 220 | `src/digitalmodel/marine_ops/marine_analysis/parsers/orcaflex_yml_parser.py` / `OrcaFlexYMLParser._fill_rao_values` | independent; [docs/registry/implementation-map.yaml:L12279-L12290] |
| 221 | `src/digitalmodel/marine_ops/marine_analysis/rao_interpolator.py` / `RAOInterpolator.interpolate_2d` | independent; [docs/registry/implementation-map.yaml:L12291-L12302] |
| 222 | `src/digitalmodel/marine_ops/marine_analysis/rao_interpolator.py` / `RAOInterpolator.interpolate_1d_frequency` | independent; [docs/registry/implementation-map.yaml:L12303-L12314] |
| 223 | `src/digitalmodel/marine_ops/marine_analysis/rao_processor.py` / `RAODataProcessor.import_aqwa_lis_file` | independent; [docs/registry/implementation-map.yaml:L12315-L12326] |
| 224 | `src/digitalmodel/marine_ops/marine_analysis/rao_processor.py` / `RAODataProcessor.import_orcaflex_yml_file` | independent; [docs/registry/implementation-map.yaml:L12327-L12338] |
| 225 | `src/digitalmodel/marine_ops/marine_analysis/rao_processor.py` / `RAODataProcessor.get_rao_dataframes` | independent; [docs/registry/implementation-map.yaml:L12339-L12350] |
| 226 | `src/digitalmodel/marine_ops/marine_analysis/rao_processor.py` / `RAODataProcessor.interpolate_rao_data` | independent; [docs/registry/implementation-map.yaml:L12351-L12362] |
| 227 | `src/digitalmodel/marine_ops/marine_analysis/rao_validators.py` / `RAODataValidators._detect_phase_convention` | caller; [docs/registry/implementation-map.yaml:L12363-L12376] |
| 228 | `src/digitalmodel/marine_ops/marine_analysis/rao_validators.py` / `RAODataValidators._find_phase_convention_in_data` | independent; [docs/registry/implementation-map.yaml:L12377-L12388] |
| 229 | `src/digitalmodel/marine_ops/marine_analysis/rao_validators.py` / `RAODataValidators._extract_orcaflex_raos` | independent; [docs/registry/implementation-map.yaml:L12389-L12400] |
| 230 | `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` / `RAOReaderError.get_user_message` | independent; [docs/registry/implementation-map.yaml:L12401-L12412] |
| 231 | `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` / `UnifiedRAOReader.read` | caller; [docs/registry/implementation-map.yaml:L12413-L12427] |
| 232 | `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` / `UnifiedRAOReader.read_aqwa_lis` | wrapper; [docs/registry/implementation-map.yaml:L12428-L12441] |
| 233 | `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` / `UnifiedRAOReader._detect_format` | independent; [docs/registry/implementation-map.yaml:L12442-L12453] |
| 234 | `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` / `UnifiedRAOReader._read_aqwa_lis` | independent; [docs/registry/implementation-map.yaml:L12454-L12465] |
| 235 | `src/digitalmodel/marine_ops/marine_analysis/visualization/rao_plotter.py` / `main` | independent; [docs/registry/implementation-map.yaml:L12466-L12477] |
| 236 | `src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py` / `read_rao_workbook` | independent; [docs/registry/implementation-map.yaml:L12478-L12489] |
| 237 | `src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py` / `real_rao_datasets` | independent; [docs/registry/implementation-map.yaml:L12490-L12503] |
| 238 | `src/digitalmodel/marine_ops/vessel_db/promote.py` / `promote` | independent; [docs/registry/implementation-map.yaml:L12504-L12515] |
| 239 | `src/digitalmodel/motion_forecast/rao_adapter.py` / `GridRAO.from_file` | independent; [docs/registry/implementation-map.yaml:L12516-L12529] |
| 240 | `src/digitalmodel/orcawave/reporting/sections/qa_summary.py` / `_run_checks` | independent; [docs/registry/implementation-map.yaml:L12530-L12541] |
| 241 | `src/digitalmodel/parametric/generate.py` / `_rao_interpolator` | independent; [docs/registry/implementation-map.yaml:L12542-L12555] |
| 242 | `src/digitalmodel/solvers/benchmark/solvers.py` / `run_orcawave` | independent; [docs/registry/implementation-map.yaml:L12556-L12567] |
| 243 | `src/digitalmodel/solvers/benchmark/solvers.py` / `run_aqwa` | independent; [docs/registry/implementation-map.yaml:L12568-L12582] |
| 244 | `src/digitalmodel/solvers/orcaflex/library_generator.py` / `csv_row_to_vessel_type` | independent; [docs/registry/implementation-map.yaml:L12583-L12594] |
| 245 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_type_builder.py` / `VesselTypeBuilder.build` | independent; [docs/registry/implementation-map.yaml:L12595-L12606] |
| 246 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/vessel_router.py` / `VesselRouter.from_hull_catalog` | independent; [docs/registry/implementation-map.yaml:L12607-L12620] |
| 247 | `src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py` / `LoadVessel.aqwa_group_item` | independent; [docs/registry/implementation-map.yaml:L12621-L12632] |
| 248 | `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` / `OrcaFlexConverter.convert_to_orcaflex` | independent; [docs/registry/implementation-map.yaml:L12633-L12646] |
| 249 | `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` / `OrcaFlexConverter._format_load_raos` | independent; [docs/registry/implementation-map.yaml:L12647-L12658] |
| 250 | `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` / `OrcaFlexConverter._save_compact_format` | independent; [docs/registry/implementation-map.yaml:L12659-L12670] |
| 251 | `src/digitalmodel/solvers/smoke/probes.py` / `check_aqwa` | independent; [docs/registry/implementation-map.yaml:L12671-L12682] |
| 252 | `src/digitalmodel/workflows/mcp_server/orcawave/core/mcp_server.py` / `OrcaWaveMCPServer._register_tools.export_to_orcaflex` | independent; [docs/registry/implementation-map.yaml:L12683-L12696] |
| 253 | `tests/drilling_riser/global_model/test_nonlinear_vessel_foundation.py` / `_vessel_motion` | test_helper; [docs/registry/implementation-map.yaml:L12697-L12710] |
| 254 | `tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py` / `module-level statements` | test_helper; [docs/registry/implementation-map.yaml:L12711-L12721] |
| 255 | `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py` / `_FakeLISParser.parse_lis_file` | test_helper; [docs/registry/implementation-map.yaml:L12722-L12733] |
| 256 | `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py` / `_run_case` | test_helper; [docs/registry/implementation-map.yaml:L12734-L12745] |
| 257 | `tests/hydrodynamics/bemrosetta/conftest.py` / `sample_aqwa_file` | test_helper; [docs/registry/implementation-map.yaml:L12746-L12759] |
| 258 | `tests/hydrodynamics/bemrosetta/test_cli.py` / `temp_lis_file` | test_helper; [docs/registry/implementation-map.yaml:L12760-L12773] |
| 259 | `tests/hydrodynamics/bemrosetta/test_parsers.py` / `temp_lis_file` | test_helper; [docs/registry/implementation-map.yaml:L12774-L12787] |
| 260 | `tests/hydrodynamics/capytaine/test_cylinder_benchmark.py` / `_load_reference_data` | test_helper; [docs/registry/implementation-map.yaml:L12788-L12799] |
| 261 | `tests/hydrodynamics/diffraction/conftest.py` / `l00_owr_path` | test_helper; [docs/registry/implementation-map.yaml:L12800-L12813] |
| 262 | `tests/hydrodynamics/diffraction/conftest.py` / `l01_owr_path` | test_helper; [docs/registry/implementation-map.yaml:L12814-L12827] |
| 263 | `tests/hydrodynamics/diffraction/test_aqwa_lis_coefficient_grid.py` / `_synthetic` | test_helper; [docs/registry/implementation-map.yaml:L12828-L12839] |
| 264 | `tests/hydrodynamics/diffraction/test_aqwa_parser.py` / `module-level statements` | test_helper; [docs/registry/implementation-map.yaml:L12840-L12850] |
| 265 | `tests/hydrodynamics/diffraction/test_aqwa_runner.py` / `_patch_extract` | test_helper; [docs/registry/implementation-map.yaml:L12851-L12862] |
| 266 | `tests/hydrodynamics/diffraction/test_matrix_unit_completeness.py` / `_fake_diffraction` | test_helper; [docs/registry/implementation-map.yaml:L12863-L12874] |
| 267 | `tests/hydrodynamics/diffraction/test_orcawave_runner.py` / `_make_fake_diffraction` | test_helper; [docs/registry/implementation-map.yaml:L12875-L12886] |
| 268 | `tests/hydrodynamics/diffraction/test_orcawave_runner.py` / `_make_fake_diffraction._install` | test_helper; [docs/registry/implementation-map.yaml:L12887-L12898] |
| 269 | `tests/hydrodynamics/diffraction/test_orcawave_runner_units_boundary.py` / `_fake_diffraction` | test_helper; [docs/registry/implementation-map.yaml:L12899-L12910] |
| 270 | `tests/hydrodynamics/diffraction/test_report_generator_golden.py` / `_full_basic` | test_helper; [docs/registry/implementation-map.yaml:L12911-L12922] |
| 271 | `tests/hydrodynamics/diffraction/test_report_generator_golden.py` / `_benchmark` | test_helper; [docs/registry/implementation-map.yaml:L12923-L12934] |
| 272 | `tests/hydrodynamics/diffraction/test_solver_orcawave_data_extraction.py` / `TestValidateDataConsistency.test_mismatched_frequencies_logged` | test_helper; [docs/registry/implementation-map.yaml:L12935-L12946] |
| 273 | `tests/hydrodynamics/diffraction/test_solver_report_extractors.py` / `_mock_diffraction_object` | test_helper; [docs/registry/implementation-map.yaml:L12947-L12958] |
| 274 | `tests/hydrodynamics/hull_library/test_rao_fixture_metadata.py` / `repository_pair` | test_helper; [docs/registry/implementation-map.yaml:L12959-L12973] |
| 275 | `tests/hydrodynamics/hull_library/test_rao_fixture_population.py` / `test_ship_periods_convert_and_responses_stay_aligned` | test_helper; [docs/registry/implementation-map.yaml:L12974-L12986] |
| 276 | `tests/hydrodynamics/hull_library/test_rao_lookup_plots.py` / `_make_entry` | test_helper; [docs/registry/implementation-map.yaml:L12987-L12998] |
| 277 | `tests/hydrodynamics/hull_library/test_rao_registry.py` / `TestPanelCatalogRaoRoundTrip.test_csv_export_with_raos` | test_helper; [docs/registry/implementation-map.yaml:L12999-L13010] |
| 278 | `tests/hydrodynamics/rao_analysis/test_rao_analysis.py` / `run_rao_analysis` | test_helper; [docs/registry/implementation-map.yaml:L13011-L13022] |
| 279 | `tests/hydrodynamics/test_rao_tabulation_workflow.py` / `_base_cfg` | test_helper; [docs/registry/implementation-map.yaml:L13023-L13034] |
| 280 | `tests/marine_ops/installation/conftest.py` / `vessel` | test_helper; [docs/registry/implementation-map.yaml:L13035-L13048] |
| 281 | `tests/marine_ops/installation/test_crane_tip_motion.py` / `TestCraneTipRAOs.test_heave_transfer_formula` | test_helper; [docs/registry/implementation-map.yaml:L13049-L13062] |
| 282 | `tests/marine_ops/marine_analysis/test_rao_quality_checks.py` / `create_synthetic_orcaflex_rao_data` | test_helper; [docs/registry/implementation-map.yaml:L13063-L13074] |
| 283 | `tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py` / `RAOVerification._find_test_files` | test_helper; [docs/registry/implementation-map.yaml:L13075-L13086] |
| 284 | `tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py` / `RAOVerification.verify_file` | test_helper; [docs/registry/implementation-map.yaml:L13087-L13101] |
| 285 | `tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py` / `RAOVerification._extract_unified_data` | test_helper; [docs/registry/implementation-map.yaml:L13102-L13113] |
| 286 | `tests/marine_ops/marine_engineering/test_rao_verification_all_routes.py` / `RAOVerification._extract_legacy_data` | test_helper; [docs/registry/implementation-map.yaml:L13114-L13125] |
| 287 | `tests/orcawave/conftest.py` / `mock_diff_single_body` | test_helper; [docs/registry/implementation-map.yaml:L13126-L13139] |
| 288 | `tests/orcawave/conftest.py` / `mock_diff_two_body` | test_helper; [docs/registry/implementation-map.yaml:L13140-L13153] |
| 289 | `tests/orcawave/test_builder_orchestration.py` / `owr_path` | test_helper; [docs/registry/implementation-map.yaml:L13154-L13167] |
| 290 | `tests/orcawave/test_report_builder.py` / `builder` | test_helper; [docs/registry/implementation-map.yaml:L13168-L13181] |
| 291 | `tests/postprocess/test_build_results_workbook.py` / `_orcawave_dir` | test_helper; [docs/registry/implementation-map.yaml:L13182-L13193] |
| 292 | `tests/solver/smoke_test.py` / `module-level statements` | test_helper; [docs/registry/implementation-map.yaml:L13194-L13204] |
| 293 | `tests/solver/smoke_test.py` / `promote_committed_artifacts` | test_helper; [docs/registry/implementation-map.yaml:L13205-L13216] |
| 294 | `tests/solver/smoke_test.py` / `run_l00_smoke_test` | test_helper; [docs/registry/implementation-map.yaml:L13217-L13228] |
| 295 | `tests/solver/smoke_test.py` / `run_l01_smoke_test` | test_helper; [docs/registry/implementation-map.yaml:L13229-L13240] |

#### external-solver-batch-run-stacks

| Row | Implementation path and symbol at map data revision | Relation / map locator |
| --- | --- | --- |
| 1 | `docs/domains/orcaflex/examples/modular/A01/A01 Catenary riser/run_orcaflex.py` / `main` | independent; [docs/registry/implementation-map.yaml:L24511-L24520] |
| 2 | `docs/domains/orcaflex/examples/modular/A01/A01 Lazy wave riser/run_orcaflex.py` / `main` | independent; [docs/registry/implementation-map.yaml:L24521-L24530] |
| 3 | `docs/domains/orcaflex/examples/modular/C10/C10 Multiple statics/run_orcaflex.py` / `main` | independent; [docs/registry/implementation-map.yaml:L24531-L24540] |
| 4 | `docs/domains/orcaflex/examples/modular/E08/PipelayConfig/run_orcaflex.py` / `main` | independent; [docs/registry/implementation-map.yaml:L24541-L24550] |
| 5 | `docs/domains/orcaflex/examples/modular/K01/K01 5MW spar FOWT/run_orcaflex.py` / `main` | independent; [docs/registry/implementation-map.yaml:L24551-L24560] |
| 6 | `docs/domains/orcaflex/examples/modular/L01/L01 Default vessel/run_orcaflex.py` / `main` | independent; [docs/registry/implementation-map.yaml:L24561-L24570] |
| 7 | `docs/domains/orcawave/L01_aqwa_benchmark/run_orcawave_benchmark.py` / `run_comparison` | independent; [docs/registry/implementation-map.yaml:L24571-L24580] |
| 8 | `docs/domains/orcawave/L01_aqwa_benchmark/run_orcawave_matched_api.py` / `run_orcawave_analysis` | independent; [docs/registry/implementation-map.yaml:L24581-L24590] |
| 9 | `docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_api.py` / `run_orcawave_diffraction` | independent; [docs/registry/implementation-map.yaml:L24591-L24600] |
| 10 | `docs/plans/evidence/issue-2082-reproduce.py` / `reproduce` | independent; [docs/registry/implementation-map.yaml:L24601-L24610] |
| 11 | `scripts/benchmark/run_3way_benchmark.py` / `run_bemrosetta` | independent; [docs/registry/implementation-map.yaml:L24611-L24620] |
| 12 | `scripts/benchmark_model_library.py` / `_calc_statics` | independent; [docs/registry/implementation-map.yaml:L24621-L24630] |
| 13 | `scripts/benchmark_riser_library.py` / `run_model` | independent; [docs/registry/implementation-map.yaml:L24631-L24640] |
| 14 | `scripts/benchmark_riser_library.py` / `run_mesh_variant` | independent; [docs/registry/implementation-map.yaml:L24641-L24650] |
| 15 | `scripts/cfd/run_sloshing_3d_check.py` / `_launch` | independent; [docs/registry/implementation-map.yaml:L24651-L24660] |
| 16 | `scripts/cfd/run_sloshing_convergence.py` / `main` | independent; [docs/registry/implementation-map.yaml:L24661-L24670] |
| 17 | `scripts/extract_library_validation.py` / `process_model_dir` | independent; [docs/registry/implementation-map.yaml:L24671-L24680] |
| 18 | `scripts/extract_riser_validation.py` / `main` | independent; [docs/registry/implementation-map.yaml:L24681-L24690] |
| 19 | `scripts/generate_24in_statics_sim.py` / `main` | independent; [docs/registry/implementation-map.yaml:L24691-L24700] |
| 20 | `scripts/generate_calm_buoy_project.py` / `CALMBuoyProjectGenerator.generate_orcaflex_modules` | independent; [docs/registry/implementation-map.yaml:L24701-L24712] |
| 21 | `scripts/generate_calm_buoy_project.py` / `CALMBuoyProjectGenerator._generate_batch_script` | independent; [docs/registry/implementation-map.yaml:L24713-L24722] |
| 22 | `scripts/mesh_sensitivity_riser.py` / `run_mesh_variant` | independent; [docs/registry/implementation-map.yaml:L24723-L24732] |
| 23 | `scripts/python/digitalmodel/analysis/FinalRollerClearance.py` / `calcClearance` | independent; [docs/registry/implementation-map.yaml:L24733-L24742] |
| 24 | `scripts/python/digitalmodel/legacy/15_-_Analyses.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24743-L24751] |
| 25 | `scripts/python/digitalmodel/legacy/16_-_TimeHistoryResultsPeriod.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24752-L24760] |
| 26 | `scripts/python/digitalmodel/legacy/17_-_SampleTimes.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24761-L24769] |
| 27 | `scripts/python/digitalmodel/legacy/18_-_TimeHistoryMultipleVariables.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24770-L24778] |
| 28 | `scripts/python/digitalmodel/legacy/19_-_DistanceBetweenTwoVessels.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24779-L24787] |
| 29 | `scripts/python/digitalmodel/legacy/1_-_FirstProgram.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24788-L24796] |
| 30 | `scripts/python/digitalmodel/legacy/20_-_ObjectExtra.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24797-L24805] |
| 31 | `scripts/python/digitalmodel/legacy/21_-_StaticResult.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24806-L24814] |
| 32 | `scripts/python/digitalmodel/legacy/22_-_RangeGraphDynamics.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24815-L24823] |
| 33 | `scripts/python/digitalmodel/legacy/26_-_LinkedStatistics.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24824-L24832] |
| 34 | `scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24833-L24841] |
| 35 | `scripts/python/digitalmodel/legacy/29_-_ExtremeStatisticsRayleigh.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24842-L24850] |
| 36 | `scripts/python/digitalmodel/legacy/30_-_ExtremeStatisticsLikelihood.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24851-L24859] |
| 37 | `scripts/python/digitalmodel/legacy/31_-_ExtremeStatisticsAggregation.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24860-L24868] |
| 38 | `scripts/python/digitalmodel/legacy/32_-_FrequencyDomainResults.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24869-L24877] |
| 39 | `scripts/python/digitalmodel/legacy/33_-_FrequencyDomainMPM.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24878-L24886] |
| 40 | `scripts/python/digitalmodel/legacy/34_-_FrequencyDomainRangeGraph.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24887-L24895] |
| 41 | `scripts/python/digitalmodel/legacy/35_-_FrequencyDomainSpectralDensitySpectralResponse.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24896-L24904] |
| 42 | `scripts/python/digitalmodel/legacy/36_-_FrequencyDomainTimeHistorySynthesis.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24905-L24913] |
| 43 | `scripts/python/digitalmodel/legacy/51_-_SerializationYAMLPart1.py` / `module-level statements` | independent; [docs/registry/implementation-map.yaml:L24914-L24922] |
| 44 | `scripts/run_benchmarks.py` / `main` | independent; [docs/registry/implementation-map.yaml:L24923-L24932] |
| 45 | `scripts/run_riser_analysis.py` / `run_statics` | independent; [docs/registry/implementation-map.yaml:L24933-L24942] |
| 46 | `scripts/run_riser_analysis.py` / `run_dynamics` | independent; [docs/registry/implementation-map.yaml:L24943-L24952] |
| 47 | `scripts/studies/surface_crack_post.py` / `solve` | independent; [docs/registry/implementation-map.yaml:L24953-L24962] |
| 48 | `src/digitalmodel/ansys/runner.py` / `ANSYSRunner.execute` | independent; [docs/registry/implementation-map.yaml:L24963-L24974] |
| 49 | `src/digitalmodel/ansys/runner.py` / `ANSYSRunner._execute_solver` | independent; [docs/registry/implementation-map.yaml:L24975-L24984] |
| 50 | `src/digitalmodel/engine.py` / `engine` | caller; [docs/registry/implementation-map.yaml:L24985-L24997] |
| 51 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py` / `AqwaReader.get_result_group` | independent; [docs/registry/implementation-map.yaml:L24998-L25007] |
| 52 | `src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py` / `AqwaUtilities.run_aqwa_analysis_as_subprocess` | independent; [docs/registry/implementation-map.yaml:L25008-L25017] |
| 53 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_batch_runner.py` / `AQWABatchRunner._run_parallel` | independent; [docs/registry/implementation-map.yaml:L25018-L25027] |
| 54 | `src/digitalmodel/hydrodynamics/diffraction/aqwa_batch_runner.py` / `AQWABatchRunner._process_single_job` | independent; [docs/registry/implementation-map.yaml:L25028-L25037] |
| 55 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_batch_runner.py` / `OrcaWaveBatchRunner._process_single_job` | independent; [docs/registry/implementation-map.yaml:L25038-L25047] |
| 56 | `src/digitalmodel/hydrodynamics/diffraction/workflow.py` / `DiffractionWorkflow.router` | independent; [docs/registry/implementation-map.yaml:L25048-L25057] |
| 57 | `src/digitalmodel/solvers/benchmark/solvers.py` / `run_orcaflex` | independent; [docs/registry/implementation-map.yaml:L25058-L25067] |
| 58 | `src/digitalmodel/solvers/openfoam/runner.py` / `OpenFOAMRunner._stage_plan` | caller; [docs/registry/implementation-map.yaml:L25068-L25079] |
| 59 | `src/digitalmodel/solvers/openfoam/runner.py` / `OpenFOAMRunner._dtchull_stage_plan` | independent; [docs/registry/implementation-map.yaml:L25080-L25089] |
| 60 | `src/digitalmodel/solvers/openfoam/runner.py` / `OpenFOAMRunner._required_executable` | independent; [docs/registry/implementation-map.yaml:L25090-L25099] |
| 61 | `src/digitalmodel/solvers/orcaflex/core/model_interface.py` / `OrcaFlexModelWrapper.run_static_analysis` | independent; [docs/registry/implementation-map.yaml:L25100-L25109] |
| 62 | `src/digitalmodel/solvers/orcaflex/core/model_interface.py` / `OrcaFlexModelWrapper.run_dynamic_simulation` | independent; [docs/registry/implementation-map.yaml:L25110-L25119] |
| 63 | `src/digitalmodel/solvers/orcaflex/modular_generator/slay_execution.py` / `write_queue` | independent; [docs/registry/implementation-map.yaml:L25120-L25129] |
| 64 | `src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py` / `Level2OrcaFlexValidator._get_model_warnings` | independent; [docs/registry/implementation-map.yaml:L25130-L25139] |
| 65 | `src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py` / `OrcaFlexAPI.run_static_analysis` | independent; [docs/registry/implementation-map.yaml:L25140-L25149] |
| 66 | `src/digitalmodel/solvers/orcaflex/opp_visualization.py` / `OPPVisualization.save_views_for_files` | independent; [docs/registry/implementation-map.yaml:L25150-L25159] |
| 67 | `src/digitalmodel/solvers/orcaflex/opp_visualization.py` / `OPPVisualization._process_single_file` | independent; [docs/registry/implementation-map.yaml:L25160-L25169] |
| 68 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` / `OrcaFlexAnalysis.process_fea` | independent; [docs/registry/implementation-map.yaml:L25170-L25179] |
| 69 | `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` / `OrcaFlexCustomAnalysis.process_fea` | independent; [docs/registry/implementation-map.yaml:L25180-L25189] |
| 70 | `src/digitalmodel/solvers/orcaflex/orcaflex_iterative_runs.py` / `OrcaflexIterativeRuns.run_iterative_simulations` | independent; [docs/registry/implementation-map.yaml:L25190-L25199] |
| 71 | `src/digitalmodel/solvers/orcaflex/orcaflex_modal_analysis.py` / `OrcModalAnalysis.run_by_file` | independent; [docs/registry/implementation-map.yaml:L25200-L25209] |
| 72 | `src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel.py` / `OrcaFlexOptimizedParallelAnalysis.process_single_file` | independent; [docs/registry/implementation-map.yaml:L25210-L25219] |
| 73 | `src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel_v2.py` / `process_single_file_worker` | independent; [docs/registry/implementation-map.yaml:L25220-L25229] |
| 74 | `src/digitalmodel/solvers/orcaflex/orcaflex_parallel_analysis.py` / `OrcaFlexParallelAnalysis.process_single_file` | independent; [docs/registry/implementation-map.yaml:L25230-L25239] |
| 75 | `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` / `OrcaflexUtilities.file_run_and_save` | independent; [docs/registry/implementation-map.yaml:L25240-L25249] |
| 76 | `src/digitalmodel/solvers/orcaflex/parallel_runner.py` / `run_one` | independent; [docs/registry/implementation-map.yaml:L25250-L25259] |
| 77 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` / `OrcaFlexViewCapture.load_model` | independent; [docs/registry/implementation-map.yaml:L25260-L25269] |
| 78 | `src/digitalmodel/solvers/orcaflex/run_to_sim.py` / `OrcaFlexModelRunner.run_single_model` | independent; [docs/registry/implementation-map.yaml:L25270-L25279] |
| 79 | `src/digitalmodel/solvers/orcaflex/run_to_sim.py` / `run_models` | independent; [docs/registry/implementation-map.yaml:L25280-L25289] |
| 80 | `src/digitalmodel/solvers/orcaflex/schematic_capture.py` / `save_orcaflex_views` | independent; [docs/registry/implementation-map.yaml:L25290-L25299] |
| 81 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` / `OrcaFlexTimeTraceProcessor._load_from_orcaflex` | independent; [docs/registry/implementation-map.yaml:L25300-L25309] |
| 82 | `src/digitalmodel/solvers/orcaflex/universal/batch_processor.py` / `BatchProcessor._default_processor` | independent; [docs/registry/implementation-map.yaml:L25310-L25319] |
| 83 | `src/digitalmodel/solvers/orcaflex/universal/universal_runner.py` / `UniversalOrcaFlexRunner._process_single_model` | independent; [docs/registry/implementation-map.yaml:L25320-L25329] |
| 84 | `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` / `OrcaWaveOrchestrator._convert_to_orcaflex` | independent; [docs/registry/implementation-map.yaml:L25330-L25339] |
| 85 | `src/digitalmodel/workflows/installation_added_mass_evidence.py` / `_run_inputs` | independent; [docs/registry/implementation-map.yaml:L25340-L25349] |
| 86 | `src/digitalmodel/workflows/installation_campaign.py` / `_verify_run` | independent; [docs/registry/implementation-map.yaml:L25350-L25359] |
| 87 | `src/digitalmodel/workflows/installation_instability_diagnostic.py` / `run_case` | independent; [docs/registry/implementation-map.yaml:L25360-L25369] |
| 88 | `src/digitalmodel/workflows/installation_trace_extract.py` / `extract` | independent; [docs/registry/implementation-map.yaml:L25370-L25379] |
| 89 | `src/digitalmodel/workflows/openfoam_batch_config.py` / `resolve_case_matrix` | independent; [docs/registry/implementation-map.yaml:L25380-L25389] |
| 90 | `src/digitalmodel/workflows/openfoam_batch_config.py` / `_base_name` | independent; [docs/registry/implementation-map.yaml:L25390-L25399] |
| 91 | `src/digitalmodel/workflows/openfoam_batch_execution.py` / `run_cases` | independent; [docs/registry/implementation-map.yaml:L25400-L25409] |
| 92 | `src/digitalmodel/workflows/openfoam_batch_execution.py` / `mpi_command_plan` | independent; [docs/registry/implementation-map.yaml:L25410-L25419] |
| 93 | `src/digitalmodel/workflows/openfoam_batch_execution.py` / `run_command` | independent; [docs/registry/implementation-map.yaml:L25420-L25429] |
| 94 | `src/digitalmodel/workflows/openfoam_batch_results.py` / `write_results` | independent; [docs/registry/implementation-map.yaml:L25430-L25439] |
| 95 | `src/digitalmodel/workflows/openfoam_run_batch.py` / `router` | caller; [docs/registry/implementation-map.yaml:L25440-L25455] |
| 96 | `src/digitalmodel/workflows/openfoam_run_batch.py` / `_settings` | independent; [docs/registry/implementation-map.yaml:L25456-L25465] |
| 97 | `src/digitalmodel/workflows/orcaflex_reproduce.py` / `batch_config` | independent; [docs/registry/implementation-map.yaml:L25466-L25475] |
| 98 | `src/digitalmodel/workflows/orcaflex_reproduce.py` / `_code_identity` | independent; [docs/registry/implementation-map.yaml:L25476-L25485] |
| 99 | `src/digitalmodel/workflows/orcaflex_reproduce.py` / `_solve` | independent; [docs/registry/implementation-map.yaml:L25486-L25495] |
| 100 | `src/digitalmodel/workflows/orcaflex_reproduce.py` / `_readback` | independent; [docs/registry/implementation-map.yaml:L25496-L25505] |
| 101 | `src/digitalmodel/workflows/orcaflex_run_batch.py` / `router` | independent; [docs/registry/implementation-map.yaml:L25506-L25518] |
| 102 | `src/digitalmodel/workflows/orcaflex_run_batch.py` / `_resolve_models` | independent; [docs/registry/implementation-map.yaml:L25519-L25528] |
| 103 | `src/digitalmodel/workflows/orcaflex_run_batch.py` / `_write_summary` | independent; [docs/registry/implementation-map.yaml:L25529-L25538] |
| 104 | `tests/ansys/test_runner_evidence_failures.py` / `_setup` | test_helper; [docs/registry/implementation-map.yaml:L25539-L25548] |
| 105 | `tests/hydrodynamics/diffraction/test_aqwa_runner.py` / `_run_solver_success` | test_helper; [docs/registry/implementation-map.yaml:L25549-L25558] |
| 106 | `tests/orcaflex/test_generic_scr_slwr.py` / `test_models_validate_and_reproduce` | test_helper; [docs/registry/implementation-map.yaml:L25559-L25568] |
| 107 | `tests/solvers/openfoam/test_runner_prebuilt.py` / `_patch_execution` | test_helper; [docs/registry/implementation-map.yaml:L25569-L25578] |
| 108 | `tests/solvers/openfoam/test_warm_start.py` / `test_change_dictionary_v2312_applies_literals_and_preserves_warm_internal` | test_helper; [docs/registry/implementation-map.yaml:L25579-L25588] |
| 109 | `tests/solvers/openfoam/test_workflow_case_definition.py` / `_canonical_batch_cfg` | test_helper; [docs/registry/implementation-map.yaml:L25589-L25598] |
| 110 | `tests/solvers/openfoam/validation/test_wave_excited_body_rao.py` / `test_sweep_point_solves_within_band` | test_helper; [docs/registry/implementation-map.yaml:L25599-L25608] |
| 111 | `tests/solvers/orcaflex/batch_processing/scripts/run_analysis.py` / `main` | test_helper; [docs/registry/implementation-map.yaml:L25609-L25618] |
| 112 | `tests/solvers/orcaflex/modular_generator/goldens/basefile/capture.py` / `main` | test_helper; [docs/registry/implementation-map.yaml:L25619-L25628] |
| 113 | `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` / `qa_model` | test_helper; [docs/registry/implementation-map.yaml:L25629-L25638] |
| 114 | `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` / `a01_riser_model` | test_helper; [docs/registry/implementation-map.yaml:L25639-L25648] |
| 115 | `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` / `lazy_wave_model` | test_helper; [docs/registry/implementation-map.yaml:L25649-L25658] |
| 116 | `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` / `fender_model` | test_helper; [docs/registry/implementation-map.yaml:L25659-L25668] |
| 117 | `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` / `pull_in_model` | test_helper; [docs/registry/implementation-map.yaml:L25669-L25678] |
| 118 | `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` / `modular_model` | test_helper; [docs/registry/implementation-map.yaml:L25679-L25688] |
| 119 | `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` / `modular_noload_model` | test_helper; [docs/registry/implementation-map.yaml:L25689-L25698] |
| 120 | `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` / `monolithic_noload_model` | test_helper; [docs/registry/implementation-map.yaml:L25699-L25708] |
| 121 | Identifying discovery path withheld | test_helper; [docs/registry/implementation-map.yaml:L25709-L25718] |
| 122 | Identifying discovery path withheld | test_helper; [docs/registry/implementation-map.yaml:L25719-L25728] |
| 123 | `tests/solvers/orcaflex/run_tests.py` / `run_tests` | test_helper; [docs/registry/implementation-map.yaml:L25729-L25738] |
| 124 | `tests/solvers/orcaflex/single_file_parallel_test.py` / `SingleFileParallelStrategy.process_file_sequential_load_parallel_analysis` | test_helper; [docs/registry/implementation-map.yaml:L25739-L25748] |
| 125 | `tests/solvers/orcaflex/single_file_parallel_test.py` / `SingleFileParallelStrategy._run_static_analysis` | test_helper; [docs/registry/implementation-map.yaml:L25749-L25758] |
| 126 | `tests/solvers/orcaflex/single_file_parallel_test.py` / `SingleFileParallelStrategy.compare_strategies` | test_helper; [docs/registry/implementation-map.yaml:L25759-L25770] |
| 127 | `tests/solvers/orcaflex/test_slay_execution.py` / `prepared_queue_inputs.DenyNative.find_spec` | test_helper; [docs/registry/implementation-map.yaml:L25771-L25780] |
| 128 | `tests/solvers/smoke/test_native_harness.py` / `fake_repo` | test_helper; [docs/registry/implementation-map.yaml:L25781-L25790] |
| 129 | `tests/solvers/smoke/test_native_model_harness.py` / `fake_repo` | test_helper; [docs/registry/implementation-map.yaml:L25791-L25800] |
| 130 | `tests/workflows/test_installation_added_mass_comparison.py` / `synthetic_study` | test_helper; [docs/registry/implementation-map.yaml:L25801-L25810] |
| 131 | `tests/workflows/test_installation_campaign.py` / `study.reproduce` | test_helper; [docs/registry/implementation-map.yaml:L25811-L25820] |
| 132 | `tests/workflows/test_openfoam_batch_identity.py` / `test_identity_is_stable_and_changes_with_input_tool_and_capacity` | test_helper; [docs/registry/implementation-map.yaml:L25821-L25830] |
| 133 | `tests/workflows/test_orcaflex_batch_contract.py` / `_single_model` | test_helper; [docs/registry/implementation-map.yaml:L25831-L25840] |
| 134 | `tests/workflows/test_orcaflex_batch_contract.py` / `_read_summary` | test_helper; [docs/registry/implementation-map.yaml:L25841-L25850] |
| 135 | `tests/workflows/test_orcaflex_batch_contract.py` / `_assert_verdict` | test_helper; [docs/registry/implementation-map.yaml:L25851-L25860] |
| 136 | `tests/workflows/test_orcaflex_reproduce.py` / `resume_case` | test_helper; [docs/registry/implementation-map.yaml:L25861-L25870] |
| 137 | `tests/workflows/test_orcaflex_reproduce.py` / `test_batch_child_configures_api_before_importing_router.intercept` | test_helper; [docs/registry/implementation-map.yaml:L25871-L25880] |

### Whole-repository follow-up, including scripts

The supplemental text search covers tracked `.py` files throughout the repository, including `scripts/`; 321 files mention AQWA, with 142 parse/read/RAO/damping/launch named-definition candidates, 106 absent as exact rows from the two counted families. Candidates include tests and helpers and are not 106 asserted second implementations.

Confirmed external method gaps include `_parse_force_rao` in the AH1 reader, which independently fills excitation magnitude/phase, and `critical_damping`, `percent_critical_to_damping`, `damping_to_percent_critical`, which own damping calculations. These are outside this package and do not alter the A8 result. [src/digitalmodel/hydrodynamics/diffraction/aqwa_ah1_parser.py:L452-L485][src/digitalmodel/hydrodynamics/diffraction/roll_damping.py:L103-L141]

Map follow-up: add the three in-package stdout parsers; assess the external force-RAO parser gap; add explicit families/rows for DAT deck generation and inspection, viscous/critical damping, MES diagnostics, force rotation and callback algorithms, validation and report qualification. Families with no in-package map rows are not counted until such rows are added; issue pending (7). [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L93-L112][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L515-L539][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L61-L260][src/digitalmodel/hydrodynamics/aqwa/mes_files.py:L16-L52][src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L244-L304][src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/validator.py:L32-L51]

## 10. Coverage and open items

### Open-item register

Every item below is assigned to the hydrodynamics maintainer for issue filing and disposition; draft references are provisional and require real issue numbers before independent clearance.

| Item | Evidence / limitation | Tracking |
| --- | --- | --- |
| U1 dimensional and convention contract | Units, origin, heading and phase convention are not consistently qualified; DAT mass comments question the unit; reciprocal-period and angular-frequency paths differ. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L177-L185][src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L595-L615][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L523-L526] | issue pending (2) |
| U2 engineering basis | Published derivation, standard edition and Citation sidecar are not established for viscous/critical damping kernels; no standard clause text was checked. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L71-L155][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L202-L260] | issue pending (2) |
| U3 review responsibility | Deputy appointment and independent clearance remain unverified; DRAFT is retained | issue pending (6) |
| Parsing completeness and missing data | Sweep candidates in DAT/LIS/PLT/MES handling need fail-closed diagnostics and malformed/locale input tests. [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L564-L731][src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L41-L150][src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L290-L371][src/digitalmodel/hydrodynamics/aqwa/mes_files.py:L16-L52] | issue pending (1) |
| Damping defaults and interpolation | Sweep candidates need explicit required-data validation and independent dimensional benchmarks. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L499-L653][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L61-L260] | issue pending (2) |
| Native execution/protocol | Return codes, failed-case persistence and rotation default require regression tests; native G1/G4 remain NOT_RUN. [src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L74-L97][src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L80-L96][src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L244-L297] | issue pending (3); #55 for history recording; #1521 for neighbouring runner diagnostics |
| Validation and offline-test visibility | Placeholder validator, blanket CLI skips, extraction-exception skip and unbound helpers remain. [src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/validator.py:L42-L51][src/digitalmodel/hydrodynamics/aqwa/aqwa_pre_process.py:L24-L37][src/digitalmodel/hydrodynamics/aqwa/aqwa_post_process.py:L28-L30] [tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py:L19-L22][tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py:L96-L116] | issue pending (4) |
| Codenaming / restricted interpretation | Identifying discovery names and specialized callback provenance are not copied; sidecar entry: to be created; no private repository was read. [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L473-L519] | issue pending (5) |
| Benchmark and evidence closure | No independent worked example or signed QA appendix; report writer outputs tables and figures without gate evidence. [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L399-L426][src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L600-L641] | issue pending (6); #51 for physics checks |
| Map and documentation breadth | Supplementary methods and uncounted families require map updates; wider documentation is discovery-only beyond the core readings named below. [src/digitalmodel/hydrodynamics/diffraction/aqwa_ah1_parser.py:L452-L485] | issue pending (7) |
| Offline execution limits | Baseline: 60 passed, 12 skipped, 0 failed; one of the skips is a module-collection NOT_RUN for unavailable OrcFxAPI. Ten CLI skips are existing blanket markers; one extraction assertion is converted into a skip. Post-edit execution is identical: 60 passed, 12 skipped and 0 failed. | issue pending (4) |

### Source and test coverage

All 19 package Python files were examined as text, including both subfolders; no licensed binary, interface or licence probe was run. Each source-file extent below is at the code pin.

| Source file | Extent |
| --- | --- |
| `__init__.py` | [src/digitalmodel/hydrodynamics/aqwa/__init__.py:L1-L17] |
| `aqwa_analysis.py` | [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis.py:L1-L39] |
| `aqwa_analysis_damping.py` | [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L1-L260] |
| `aqwa_analysis_ef_server.py` | [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_ef_server.py:L1-L601] |
| `aqwa_analysis_raos.py` | [src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_raos.py:L1-L780] |
| `aqwa_cli.py` | [src/digitalmodel/hydrodynamics/aqwa/aqwa_cli.py:L1-L262] |
| `aqwa_dat_files.py` | [src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L1-L731] |
| `aqwa_lis_files.py` | [src/digitalmodel/hydrodynamics/aqwa/aqwa_lis_files.py:L1-L251] |
| `aqwa_post_process.py` | [src/digitalmodel/hydrodynamics/aqwa/aqwa_post_process.py:L1-L30] |
| `aqwa_pre_process.py` | [src/digitalmodel/hydrodynamics/aqwa/aqwa_pre_process.py:L1-L37] |
| `aqwa_reader.py` | [src/digitalmodel/hydrodynamics/aqwa/aqwa_reader.py:L1-L438] |
| `aqwa_router.py` | [src/digitalmodel/hydrodynamics/aqwa/aqwa_router.py:L1-L126] |
| `aqwa_utilities.py` | [src/digitalmodel/hydrodynamics/aqwa/aqwa_utilities.py:L1-L97] |
| `aqwa_validation/__init__.py` | [src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/__init__.py:L1-L26] |
| `aqwa_validation/models.py` | [src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/models.py:L1-L103] |
| `aqwa_validation/validator.py` | [src/digitalmodel/hydrodynamics/aqwa/aqwa_validation/validator.py:L1-L51] |
| `ef_server/AqwaServerMgr.py` | [src/digitalmodel/hydrodynamics/aqwa/ef_server/AqwaServerMgr.py:L1-L626] |
| `mes_files.py` | [src/digitalmodel/hydrodynamics/aqwa/mes_files.py:L1-L230] |
| `viscous_damping_determination.py` | [src/digitalmodel/hydrodynamics/aqwa/viscous_damping_determination.py:L1-L641] |

The module test-directory search plus `tests/test_aqwa.py` identifies 65 test functions, including 10 CLI functions, 10 LIS parser functions, 2 restoring-precedence functions, 11 other test-directory smoke functions and 32 router functions; these are source counts, not executed-pass counts.

| Test node at code pin | Definition citation / offline record |
| --- | --- |
| `tests/hydrodynamics/aqwa/test_aqwa_ef_server.py::test_run_process` | [tests/hydrodynamics/aqwa/test_aqwa_ef_server.py:L22-L32]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_aqr_raos.py::test_run_process` | [tests/hydrodynamics/aqwa/test_aqwa_aqr_raos.py:L22-L32]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_anl_raos.py::test_run_process` | [tests/hydrodynamics/aqwa/test_aqwa_anl_raos.py:L24-L34]; BASE NOT_RUN: guarded OrcFxAPI collection import unavailable, no licence probe; AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_preproces_deck_10.py::test_run_process` | [tests/hydrodynamics/aqwa/test_preproces_deck_10.py:L23-L32]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_damping.py::test_run_process` | [tests/hydrodynamics/aqwa/test_damping.py:L22-L32]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_aqr.py::test_run_process` | [tests/hydrodynamics/aqwa/test_aqwa_aqr.py:L23-L35]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_mes_files.py::test_run_process` | [tests/hydrodynamics/aqwa/test_aqwa_mes_files.py:L22-L33]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_lis.py::test_run_process` | [tests/hydrodynamics/aqwa/test_aqwa_lis.py:L22-L33]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_no_mes_files.py::test_run_process` | [tests/hydrodynamics/aqwa/test_aqwa_no_mes_files.py:L22-L33]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py::TestAQWACLIBasic::test_cli_help` | [tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py:L28-L37]; BASE SKIP: existing blanket AQWA solver marker; AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py::TestAQWACLIBasic::test_list_methods` | [tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py:L39-L49]; BASE SKIP: existing blanket AQWA solver marker; AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py::TestAQWACLIRAOExtraction::test_rao_method_with_folder` | [tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py:L59-L81]; BASE SKIP: existing blanket AQWA solver marker; AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py::TestAQWACLIErrorHandling::test_missing_method` | [tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py:L87-L99]; BASE SKIP: existing blanket AQWA solver marker; AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py::TestAQWACLIErrorHandling::test_invalid_method` | [tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py:L101-L114]; BASE SKIP: existing blanket AQWA solver marker; AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py::TestAQWACLIErrorHandling::test_missing_required_folder` | [tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py:L116-L127]; BASE SKIP: existing blanket AQWA solver marker; AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py::TestAQWACLIConfigFile::test_config_file_loading` | [tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py:L152-L166]; BASE SKIP: existing blanket AQWA solver marker; AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py::TestAQWACLIConfigFile::test_missing_config_file` | [tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py:L168-L180]; BASE SKIP: existing blanket AQWA solver marker; AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py::TestAQWACLIDampingAnalysis::test_damping_method` | [tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py:L190-L205]; BASE SKIP: existing blanket AQWA solver marker; AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py::TestAQWACLIVerboseMode::test_verbose_flag` | [tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py:L215-L233]; BASE SKIP: existing blanket AQWA solver marker; AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_preproces_deck.py::test_run_process` | [tests/hydrodynamics/aqwa/test_aqwa_preproces_deck.py:L23-L32]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py::test_restoring_prioritizes_lis_values` | [tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L132-L140]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py::test_restoring_falls_back_to_dat_when_lis_missing` | [tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L143-L151]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py::TestAqwaLISFilesRealData::test_parser_initialization` | [tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py:L86-L89]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py::TestAqwaLISFilesRealData::test_get_data_from_lis_file` | [tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py:L95-L116]; BASE SKIP: empty dataframe assertion caught by the test (0 rows); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py::TestAqwaLISFilesRealData::test_header_column_extraction` | [tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py:L122-L142]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py::TestAqwaLISFilesRealData::test_dataframe_sorting` | [tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py:L148-L172]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py::TestAqwaLISFilesRealData::test_velocity_transformation` | [tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py:L178-L210]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py::TestAqwaLISFilesRealData::test_acceleration_transformation` | [tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py:L216-L248]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py::TestAqwaLISFilesRealData::test_csv_save` | [tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py:L254-L283]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py::TestAqwaLISFilesEdgeCases::test_sorting_without_sort_config` | [tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py:L293-L305]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py::TestAqwaLISFilesEdgeCases::test_header_columns_from_config` | [tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py:L307-L319]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py::TestAqwaLISFilesEdgeCases::test_csv_save_disabled` | [tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py:L321-L343]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/hydrodynamics/aqwa/test_aqwa_dat.py::test_run_process` | [tests/hydrodynamics/aqwa/test_aqwa_dat.py:L23-L32]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaClass::test_aqwa_initialization` | [tests/test_aqwa.py:L26-L29]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaClass::test_aqwa_multiple_instances` | [tests/test_aqwa.py:L31-L37]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaRouter::test_router_no_processing` | [tests/test_aqwa.py:L62-L74]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaRouter::test_router_preprocess_only` | [tests/test_aqwa.py:L79-L94]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaRouter::test_router_analysis_only` | [tests/test_aqwa.py:L99-L125]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaRouter::test_router_analysis_viscous_damping_enabled` | [tests/test_aqwa.py:L131-L162]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaRouter::test_router_analysis_viscous_damping_disabled_fallback` | [tests/test_aqwa.py:L168-L199]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaRouter::test_router_mes_flag_true` | [tests/test_aqwa.py:L204-L219]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaRouter::test_router_mes_flag_false` | [tests/test_aqwa.py:L224-L235]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaRouter::test_router_no_mes_key` | [tests/test_aqwa.py:L240-L251]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaRouter::test_router_results_only` | [tests/test_aqwa.py:L256-L278]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaRouter::test_router_all_flags_enabled` | [tests/test_aqwa.py:L283-L319]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaRouter::test_router_parametrized_configurations` | [tests/test_aqwa.py:L334-L385]; BASE PASS (8 expanded cases); AFTER identical to BASE |
| `tests/test_aqwa.py::TestGetCfgWithMasterData::test_get_cfg_with_master_data_no_master_data` | [tests/test_aqwa.py:L396-L400]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestGetCfgWithMasterData::test_get_cfg_with_master_data_summary_settings_master` | [tests/test_aqwa.py:L402-L432]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestGetCfgWithMasterData::test_get_cfg_with_master_data_settings_master` | [tests/test_aqwa.py:L434-L460]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestGetCfgWithMasterData::test_get_cfg_with_master_data_both_masters` | [tests/test_aqwa.py:L462-L488]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestGetCfgWithMasterData::test_get_cfg_with_master_data_empty_groups` | [tests/test_aqwa.py:L490-L504]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestGetCfgWithMasterData::test_get_cfg_with_master_data_multiple_groups` | [tests/test_aqwa.py:L506-L528]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestGetCfgWithMasterData::test_get_cfg_with_master_data_update_deep_dictionary_called_correctly` | [tests/test_aqwa.py:L531-L550]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaEdgeCases::test_router_with_none_cfg` | [tests/test_aqwa.py:L561-L565]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaEdgeCases::test_router_with_missing_type_key` | [tests/test_aqwa.py:L567-L572]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaEdgeCases::test_router_with_malformed_type_structure` | [tests/test_aqwa.py:L574-L579]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaEdgeCases::test_router_partial_type_keys` | [tests/test_aqwa.py:L582-L593]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaEdgeCases::test_get_cfg_with_master_data_malformed_summary_settings` | [tests/test_aqwa.py:L595-L605]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaEdgeCases::test_get_cfg_with_master_data_missing_keychain` | [tests/test_aqwa.py:L607-L617]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaEdgeCases::test_get_cfg_with_master_data_invalid_keychain_reference` | [tests/test_aqwa.py:L619-L629]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaEdgeCases::test_router_import_error_simulation` | [tests/test_aqwa.py:L631-L648]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaIntegration::test_complete_workflow_integration` | [tests/test_aqwa.py:L696-L729]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaIntegration::test_workflow_state_preservation` | [tests/test_aqwa.py:L731-L757]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaPerformance::test_large_configuration_processing` | [tests/test_aqwa.py:L769-L795]; BASE PASS (1 expanded case); AFTER identical to BASE |
| `tests/test_aqwa.py::TestAqwaPerformance::test_deep_nested_configuration` | [tests/test_aqwa.py:L797-L818]; BASE PASS (1 expanded case); AFTER identical to BASE |

### Documentation discovery

Core documentation inspected: the AQWA README navigation, quick-start workflow descriptions, testing summary and model-check/lessons material; claims of completeness and counts in these documents are not adopted as validation evidence. [docs/domains/aqwa/README.md:L9-L54][docs/domains/aqwa/QUICK_START.md:L7-L14][docs/domains/aqwa/AQWA_TESTING_SUMMARY.md:L19-L29][docs/domains/aqwa/workflows/model-checks.md:L7-L29][docs/domains/aqwa/workflows/lessons-learned.md:L1-L28]

The following `docs/` discovery list comes from `rg -l -i aqwa docs -g '*.md' -g '*.rst'`; wider articles, plans and examples are listed as discovery-only, not independently reconciled with this package. Existing source prose with identifying details is not reproduced; documentation reconciliation and codenaming are issue pending (7) and issue pending (5).

| Discovery | AQWA mention locator at code pin |
| --- | --- |
| `docs/capability-map/capability-report-2026-02-14.md` | [docs/capability-map/capability-report-2026-02-14.md:L35-L35]; discovery-only except core readings above. |
| `docs/maps/digitalmodel-operator-map.md` | [docs/maps/digitalmodel-operator-map.md:L45-L45]; discovery-only except core readings above. |
| `docs/reports/2026-10-09-compute-program/README.md` | [docs/reports/2026-10-09-compute-program/README.md:L10-L10]; discovery-only except core readings above. |
| Discovery 4 | Discovery path withheld for neutral wording; issue pending (7). |
| `docs/registry/implementation-map.md` | [docs/registry/implementation-map.md:L20-L20]; discovery-only except core readings above. |
| `docs/RELOCATED-DATA.md` | [docs/RELOCATED-DATA.md:L47-L47]; discovery-only except core readings above. |
| `docs/research/hydrodynamics-industry-practice.md` | [docs/research/hydrodynamics-industry-practice.md:L19-L19]; discovery-only except core readings above. |
| `docs/archive/ARCHIVE-MOVE-PLAN-2026-10-08.md` | [docs/archive/ARCHIVE-MOVE-PLAN-2026-10-08.md:L167-L167]; discovery-only except core readings above. |
| `docs/plans/evidence/issue-2082-claude-plan-r2.md` | [docs/plans/evidence/issue-2082-claude-plan-r2.md:L101-L101]; discovery-only except core readings above. |
| `docs/plans/2026-07-03-issue-1358-motion-forecast-keystone.md` | [docs/plans/2026-07-03-issue-1358-motion-forecast-keystone.md:L6-L6]; discovery-only except core readings above. |
| `docs/README.md` | [docs/README.md:L17-L17]; discovery-only except core readings above. |
| `docs/api/cfd/cases/wave_excited_body/README.md` | [docs/api/cfd/cases/wave_excited_body/README.md:L116-L116]; discovery-only except core readings above. |
| `docs/plans/2026-05-05-issue-566-marine-ops-r1-r5-r7-r8-batched.md` | [docs/plans/2026-05-05-issue-566-marine-ops-r1-r5-r7-r8-batched.md:L10-L10]; discovery-only except core readings above. |
| `docs/plans/parametrics-preparedness.md` | [docs/plans/parametrics-preparedness.md:L4-L4]; discovery-only except core readings above. |
| `docs/plans/2026-05-15-issue-613-orcawave-doctor-command.md` | [docs/plans/2026-05-15-issue-613-orcawave-doctor-command.md:L29-L29]; discovery-only except core readings above. |
| `docs/plans/2026-05-05-issue-552-aqwa-crlf-lf-alignment.md` | [docs/plans/2026-05-05-issue-552-aqwa-crlf-lf-alignment.md:L1-L1]; discovery-only except core readings above. |
| `docs/research/mooring-industry-practice.md` | [docs/research/mooring-industry-practice.md:L66-L66]; discovery-only except core readings above. |
| `docs/plans/2026-05-15-issue-612-orcawave-convergence-sensitivity-workflow.md` | [docs/plans/2026-05-15-issue-612-orcawave-convergence-sensitivity-workflow.md:L31-L31]; discovery-only except core readings above. |
| `docs/plans/2026-05-05-issue-470-openfoam-seakeeping.md` | [docs/plans/2026-05-05-issue-470-openfoam-seakeeping.md:L11-L11]; discovery-only except core readings above. |
| `docs/plans/evidence/2026-09-12-issue-2094-claude-r2.md` | [docs/plans/evidence/2026-09-12-issue-2094-claude-r2.md:L33-L33]; discovery-only except core readings above. |
| `docs/domains/testing/CLI_TESTING_FRAMEWORK_COMPLETE.md` | [docs/domains/testing/CLI_TESTING_FRAMEWORK_COMPLETE.md:L395-L395]; discovery-only except core readings above. |
| `docs/plans/2026-09-11-issue-2094-ansys-example-deck-validation.md` | [docs/plans/2026-09-11-issue-2094-ansys-example-deck-validation.md:L58-L58]; discovery-only except core readings above. |
| `docs/plans/2026-06-29-cfd-capability-assessment.md` | [docs/plans/2026-06-29-cfd-capability-assessment.md:L75-L75]; discovery-only except core readings above. |
| `docs/plans/2026-05-15-issue-606-orcawave-meshpipeline-integration.md` | [docs/plans/2026-05-15-issue-606-orcawave-meshpipeline-integration.md:L28-L28]; discovery-only except core readings above. |
| `docs/plans/2026-05-05-issue-575-fowt-coupled-aero-hydro-facade.md` | [docs/plans/2026-05-05-issue-575-fowt-coupled-aero-hydro-facade.md:L9-L9]; discovery-only except core readings above. |
| `docs/plans/2026-05-15-issue-609-orcawave-auxiliary-mesh-handling.md` | [docs/plans/2026-05-15-issue-609-orcawave-auxiliary-mesh-handling.md:L28-L28]; discovery-only except core readings above. |
| `docs/plans/evidence/2026-08-03-plan-1633-r2-codex-testdesign.md` | [docs/plans/evidence/2026-08-03-plan-1633-r2-codex-testdesign.md:L10-L10]; discovery-only except core readings above. |
| `docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md` | [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L14-L14]; discovery-only except core readings above. |
| `docs/plans/parametric-refresh/adding-an-atlas.md` | [docs/plans/parametric-refresh/adding-an-atlas.md:L18-L18]; discovery-only except core readings above. |
| `docs/plans/2026-05-15-issue-608-orcawave-mesh-quality-gates.md` | [docs/plans/2026-05-15-issue-608-orcawave-mesh-quality-gates.md:L28-L28]; discovery-only except core readings above. |
| `docs/plans/2026-05-15-issue-614-orcawave-docs-canonical-workflow.md` | [docs/plans/2026-05-15-issue-614-orcawave-docs-canonical-workflow.md:L28-L28]; discovery-only except core readings above. |
| `docs/plans/evidence/issue-2082-codex-plan-r1.md` | [docs/plans/evidence/issue-2082-codex-plan-r1.md:L18-L18]; discovery-only except core readings above. |
| `docs/domains/testing/coverage_analysis_report.md` | [docs/domains/testing/coverage_analysis_report.md:L37-L37]; discovery-only except core readings above. |
| `docs/plans/2026-07-13-issue-1571-aqwa-triangles-frequency-v261.md` | [docs/plans/2026-07-13-issue-1571-aqwa-triangles-frequency-v261.md:L1-L1]; discovery-only except core readings above. |
| Discovery 35 | Identifying discovery path withheld; issue pending (5). |
| `docs/plans/2026-05-15-issue-607-orcawave-given-mesh-cli.md` | [docs/plans/2026-05-15-issue-607-orcawave-given-mesh-cli.md:L30-L30]; discovery-only except core readings above. |
| `docs/plans/2026-09-11-issue-2089-ghs-canary.md` | [docs/plans/2026-09-11-issue-2089-ghs-canary.md:L17-L17]; discovery-only except core readings above. |
| `docs/plans/2026-05-15-issue-605-self-contained-orcawave-packages.md` | [docs/plans/2026-05-15-issue-605-self-contained-orcawave-packages.md:L28-L28]; discovery-only except core readings above. |
| `docs/plans/evidence/2026-09-12-issue-1633-claude-continuation.md` | [docs/plans/evidence/2026-09-12-issue-1633-claude-continuation.md:L32-L32]; discovery-only except core readings above. |
| `docs/plans/2026-08-03-issue-1633-ship-benchmark-verdict.md` | [docs/plans/2026-08-03-issue-1633-ship-benchmark-verdict.md:L22-L22]; discovery-only except core readings above. |
| `docs/plans/parametric-refresh/atlas-spec.md` | [docs/plans/parametric-refresh/atlas-spec.md:L34-L34]; discovery-only except core readings above. |
| `docs/plans/2026-05-05-issue-283-passing-ship-mooring-response.md` | [docs/plans/2026-05-05-issue-283-passing-ship-mooring-response.md:L5-L5]; discovery-only except core readings above. |
| Discovery 43 | Identifying discovery path withheld; issue pending (5). |
| `docs/plans/evidence/2026-08-03-plan-1633-r2-codex-physics.md` | [docs/plans/evidence/2026-08-03-plan-1633-r2-codex-physics.md:L6-L6]; discovery-only except core readings above. |
| `docs/domains/testing/CLI_TESTING_NEXT_STEPS.md` | [docs/domains/testing/CLI_TESTING_NEXT_STEPS.md:L67-L67]; discovery-only except core readings above. |
| `docs/domains/examples/propagation_setup_example.md` | [docs/domains/examples/propagation_setup_example.md:L33-L33]; discovery-only except core readings above. |
| `docs/domains/wec/intro.md` | [docs/domains/wec/intro.md:L5-L5]; discovery-only except core readings above. |
| `docs/domains/_index.md` | [docs/domains/_index.md:L12-L12]; discovery-only except core readings above. |
| `docs/plans/evidence/2026-08-03-plan-1633-r2-codex-evidence.md` | [docs/plans/evidence/2026-08-03-plan-1633-r2-codex-evidence.md:L7-L7]; discovery-only except core readings above. |
| `docs/plans/parametric-refresh/licensed-libraries.md` | [docs/plans/parametric-refresh/licensed-libraries.md:L8-L8]; discovery-only except core readings above. |
| `docs/plans/2026-05-15-issue-611-orcawave-result-contract.md` | [docs/plans/2026-05-15-issue-611-orcawave-result-contract.md:L28-L28]; discovery-only except core readings above. |
| `docs/plans/evidence/2026-09-12-issue-2094-claude-r1.md` | [docs/plans/evidence/2026-09-12-issue-2094-claude-r1.md:L13-L13]; discovery-only except core readings above. |
| `docs/domains/testing/VALIDATION_FRAMEWORK_BEST_PRACTICES.md` | [docs/domains/testing/VALIDATION_FRAMEWORK_BEST_PRACTICES.md:L517-L517]; discovery-only except core readings above. |
| `docs/domains/infrastructure/SKILL_VERSIONING.md` | [docs/domains/infrastructure/SKILL_VERSIONING.md:L28-L28]; discovery-only except core readings above. |
| `docs/domains/infrastructure/SKILL_VERSIONING_QUICK_REF.md` | [docs/domains/infrastructure/SKILL_VERSIONING_QUICK_REF.md:L11-L11]; discovery-only except core readings above. |
| `docs/plans/evidence/2026-09-13-analysis-workflows-review.md` | [docs/plans/evidence/2026-09-13-analysis-workflows-review.md:L3-L3]; discovery-only except core readings above. |
| `docs/domains/aqwa/README.md` | [docs/domains/aqwa/README.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/ansys/ansys_gpt.md` | [docs/domains/ansys/ansys_gpt.md:L3-L3]; discovery-only except core readings above. |
| `docs/plans/2026-05-05-issue-280-orcaflex-format-converter-license-validation.md` | [docs/plans/2026-05-05-issue-280-orcaflex-format-converter-license-validation.md:L59-L59]; discovery-only except core readings above. |
| `docs/domains/infrastructure/SKILL_VERSIONING_IMPLEMENTATION.md` | [docs/domains/infrastructure/SKILL_VERSIONING_IMPLEMENTATION.md:L194-L194]; discovery-only except core readings above. |
| `docs/domains/infrastructure/cache_implementation_summary.md` | [docs/domains/infrastructure/cache_implementation_summary.md:L138-L138]; discovery-only except core readings above. |
| `docs/plans/evidence/2026-08-10-issue-1983-triage.md` | [docs/plans/evidence/2026-08-10-issue-1983-triage.md:L115-L115]; discovery-only except core readings above. |
| `docs/domains/testing/engine_test_strategy.md` | [docs/domains/testing/engine_test_strategy.md:L85-L85]; discovery-only except core readings above. |
| `docs/domains/infrastructure/cache_layer_implementation.md` | [docs/domains/infrastructure/cache_layer_implementation.md:L200-L200]; discovery-only except core readings above. |
| `docs/domains/wind/wind_summary.md` | [docs/domains/wind/wind_summary.md:L32-L32]; discovery-only except core readings above. |
| `docs/plans/evidence/issue-2082-claude-plan-haiku.md` | [docs/plans/evidence/issue-2082-claude-plan-haiku.md:L192-L192]; discovery-only except core readings above. |
| `docs/domains/diffraction/CLI_GUIDE.md` | [docs/domains/diffraction/CLI_GUIDE.md:L8-L8]; discovery-only except core readings above. |
| `docs/domains/aqwa/AQWA_TESTING_SUMMARY.md` | [docs/domains/aqwa/AQWA_TESTING_SUMMARY.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/ansys/workflows/aqwa-to-structural-submodel.md` | [docs/domains/ansys/workflows/aqwa-to-structural-submodel.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/diffraction/AQWA_PARSER.md` | [docs/domains/diffraction/AQWA_PARSER.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/diffraction/PHASE_2_COMPLETION.md` | [docs/domains/diffraction/PHASE_2_COMPLETION.md:L95-L95]; discovery-only except core readings above. |
| `docs/domains/diffraction/ORCAWAVE_CONVERTER_GUIDE.md` | [docs/domains/diffraction/ORCAWAVE_CONVERTER_GUIDE.md:L16-L16]; discovery-only except core readings above. |
| `docs/plans/evidence/2026-08-03-plan-1633-r1-claude.md` | [docs/plans/evidence/2026-08-03-plan-1633-r1-claude.md:L15-L15]; discovery-only except core readings above. |
| `docs/domains/testing/engine_test_strategy_summary.md` | [docs/domains/testing/engine_test_strategy_summary.md:L64-L64]; discovery-only except core readings above. |
| `docs/domains/diffraction/README.md` | [docs/domains/diffraction/README.md:L3-L3]; discovery-only except core readings above. |
| `docs/domains/diffraction/tutorials/README.md` | [docs/domains/diffraction/tutorials/README.md:L10-L10]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/lessons-learned.md` | [docs/domains/aqwa/workflows/lessons-learned.md:L3-L3]; discovery-only except core readings above. |
| `docs/domains/diffraction/examples/README.md` | [docs/domains/diffraction/examples/README.md:L9-L9]; discovery-only except core readings above. |
| `docs/domains/diffraction/tutorials/01_getting_started.md` | [docs/domains/diffraction/tutorials/01_getting_started.md:L8-L8]; discovery-only except core readings above. |
| `docs/domains/diffraction/tutorials/02_aqwa_conversion_example.md` | [docs/domains/diffraction/tutorials/02_aqwa_conversion_example.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/aqwa/aqwa_parser_fix_complete.md` | [docs/domains/aqwa/aqwa_parser_fix_complete.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/diffraction/examples/ORCAWAVE_TEST_RESULTS.md` | [docs/domains/diffraction/examples/ORCAWAVE_TEST_RESULTS.md:L225-L225]; discovery-only except core readings above. |
| `docs/domains/diffraction/tutorials/03_batch_processing.md` | [docs/domains/diffraction/tutorials/03_batch_processing.md:L26-L26]; discovery-only except core readings above. |
| `docs/domains/testing/CROSS_REPO_TESTING_EXAMPLES.md` | [docs/domains/testing/CROSS_REPO_TESTING_EXAMPLES.md:L134-L134]; discovery-only except core readings above. |
| `docs/domains/ansys/ansys.md` | [docs/domains/ansys/ansys.md:L40-L40]; discovery-only except core readings above. |
| `docs/domains/aqwa/examples/001_ship_raos.md` | [docs/domains/aqwa/examples/001_ship_raos.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/model-checks.md` | [docs/domains/aqwa/workflows/model-checks.md:L3-L3]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/diffraction-analysis.md` | [docs/domains/aqwa/workflows/diffraction-analysis.md:L14-L14]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/scripting.md` | [docs/domains/aqwa/workflows/scripting.md:L3-L3]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/aqwa-gs.md` | [docs/domains/aqwa/workflows/aqwa-gs.md:L3-L3]; discovery-only except core readings above. |
| `docs/domains/structural/STRUCTURAL_ANALYSIS_IMPLEMENTATION.md` | [docs/domains/structural/STRUCTURAL_ANALYSIS_IMPLEMENTATION.md:L12-L12]; discovery-only except core readings above. |
| `docs/domains/aqwa/QUICK_START.md` | [docs/domains/aqwa/QUICK_START.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/aqwa/aqwa_parser_fix_summary.md` | [docs/domains/aqwa/aqwa_parser_fix_summary.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/error-handling.md` | [docs/domains/aqwa/workflows/error-handling.md:L3-L3]; discovery-only except core readings above. |
| `docs/domains/testing/engine_test_implementation_plan.md` | [docs/domains/testing/engine_test_implementation_plan.md:L371-L371]; discovery-only except core readings above. |
| `docs/domains/aqwa/getting-started/dat-and-workbench.md` | [docs/domains/aqwa/getting-started/dat-and-workbench.md:L43-L43]; discovery-only except core readings above. |
| `docs/domains/aqwa/examples/002_ship_with_piers.md` | [docs/domains/aqwa/examples/002_ship_with_piers.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/convergence-analysis.md` | [docs/domains/aqwa/workflows/convergence-analysis.md:L3-L3]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/scripting-guide.md` | [docs/domains/aqwa/workflows/scripting-guide.md:L13-L13]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/queries.md` | [docs/domains/aqwa/workflows/queries.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/rao-analysis.md` | [docs/domains/aqwa/workflows/rao-analysis.md:L4-L4]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/general-instructions.md` | [docs/domains/aqwa/workflows/general-instructions.md:L4-L4]; discovery-only except core readings above. |
| `docs/domains/orcfxapi/reference/python-ref-object.md` | [docs/domains/orcfxapi/reference/python-ref-object.md:L263-L263]; discovery-only except core readings above. |
| `docs/domains/orcaflex/support/orcawave/examples.md` | [docs/domains/orcaflex/support/orcawave/examples.md:L12-L12]; discovery-only except core readings above. |
| `docs/domains/agents/CLAUDE_FLOW_INTEGRATION.md` | [docs/domains/agents/CLAUDE_FLOW_INTEGRATION.md:L12-L12]; discovery-only except core readings above. |
| `docs/domains/test_automation/README.md` | [docs/domains/test_automation/README.md:L34-L34]; discovery-only except core readings above. |
| `docs/domains/aqwa/getting-started/dat-to-workbench.md` | [docs/domains/aqwa/getting-started/dat-to-workbench.md:L2-L2]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/external-forces/external_force_util_example.md` | [docs/domains/aqwa/workflows/external-forces/external_force_util_example.md:L16-L16]; discovery-only except core readings above. |
| `docs/domains/aqwa/examples/005_Truss_Spar.md` | [docs/domains/aqwa/examples/005_Truss_Spar.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/aqwa-reader.md` | [docs/domains/aqwa/workflows/aqwa-reader.md:L3-L3]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/warning-handling.md` | [docs/domains/aqwa/workflows/warning-handling.md:L3-L3]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/post-processing.md` | [docs/domains/aqwa/workflows/post-processing.md:L3-L3]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/advanced-techniques.md` | [docs/domains/aqwa/workflows/advanced-techniques.md:L3-L3]; discovery-only except core readings above. |
| `docs/domains/aqwa/examples/003_FPSO_Turret.md` | [docs/domains/aqwa/examples/003_FPSO_Turret.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/test_automation/pipeline_mock_patterns.md` | [docs/domains/test_automation/pipeline_mock_patterns.md:L5-L5]; discovery-only except core readings above. |
| `docs/domains/test_automation/getting-started.md` | [docs/domains/test_automation/getting-started.md:L109-L109]; discovery-only except core readings above. |
| `docs/domains/aqwa/getting-started/aqwa-overview.md` | [docs/domains/aqwa/getting-started/aqwa-overview.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/orcawave/L01_aqwa_benchmark/AGENT_PROMPT.md` | [docs/domains/orcawave/L01_aqwa_benchmark/AGENT_PROMPT.md:L12-L12]; discovery-only except core readings above. |
| `docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md` | [docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md:L230-L230]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/external-forces/external_force_process.md` | [docs/domains/aqwa/workflows/external-forces/external_force_process.md:L31-L31]; discovery-only except core readings above. |
| `docs/domains/aqwa/examples/004_multi_body_interaction.md` | [docs/domains/aqwa/examples/004_multi_body_interaction.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/aqwa/getting-started/file-preparation.md` | [docs/domains/aqwa/getting-started/file-preparation.md:L12-L12]; discovery-only except core readings above. |
| `docs/domains/aqwa/getting-started/geometry-basics.md` | [docs/domains/aqwa/getting-started/geometry-basics.md:L9-L9]; discovery-only except core readings above. |
| `docs/domains/aqwa/getting-started/introduction.md` | [docs/domains/aqwa/getting-started/introduction.md:L5-L5]; discovery-only except core readings above. |
| `docs/domains/test_automation/fatigue_analysis_mock_patterns.md` | [docs/domains/test_automation/fatigue_analysis_mock_patterns.md:L150-L150]; discovery-only except core readings above. |
| `docs/domains/test_automation/aqwa_mock_patterns.md` | [docs/domains/test_automation/aqwa_mock_patterns.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/orcaflex/ORCAFLEX_MODEL_PREPARATION_GUIDE.md` | [docs/domains/orcaflex/ORCAFLEX_MODEL_PREPARATION_GUIDE.md:L18-L18]; discovery-only except core readings above. |
| `docs/domains/orcaflex/aqwa/to_orcawave/intro.md` | [docs/domains/orcaflex/aqwa/to_orcawave/intro.md:L3-L3]; discovery-only except core readings above. |
| `docs/domains/orcawave/L01_aqwa_benchmark/way_forward.md` | [docs/domains/orcawave/L01_aqwa_benchmark/way_forward.md:L16-L16]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/external-forces/aqwa_external_force.md` | [docs/domains/aqwa/workflows/external-forces/aqwa_external_force.md:L6-L6]; discovery-only except core readings above. |
| `docs/domains/README.md` | [docs/domains/README.md:L10-L10]; discovery-only except core readings above. |
| `docs/domains/orcaflex/property_routing/routing_map.md` | [docs/domains/orcaflex/property_routing/routing_map.md:L106-L106]; discovery-only except core readings above. |
| `docs/domains/orcaflex/RAO_PARSER_FIX_SUMMARY.md` | [docs/domains/orcaflex/RAO_PARSER_FIX_SUMMARY.md:L9-L9]; discovery-only except core readings above. |
| `docs/domains/orcaflex/aqwa/to_orcaflex/intro.md` | [docs/domains/orcaflex/aqwa/to_orcaflex/intro.md:L3-L3]; discovery-only except core readings above. |
| `docs/domains/orcaflex/aqwa/to_orcaflex/from_aqwa_to_orcaflex.md` | [docs/domains/orcaflex/aqwa/to_orcaflex/from_aqwa_to_orcaflex.md:L7-L7]; discovery-only except core readings above. |
| `docs/domains/orcaflex/aqwa/to_orcawave/from_aqwa_to_orcawave.md` | [docs/domains/orcaflex/aqwa/to_orcawave/from_aqwa_to_orcawave.md:L2-L2]; discovery-only except core readings above. |
| `docs/domains/hull_library/hullprod-curvature-screening-evaluation.md` | [docs/domains/hull_library/hullprod-curvature-screening-evaluation.md:L105-L105]; discovery-only except core readings above. |
| `docs/domains/charts/phase2/validation/README.md` | [docs/domains/charts/phase2/validation/README.md:L77-L77]; discovery-only except core readings above. |
| `docs/domains/marine-engineering/marine_excel_analysis_summary.md` | [docs/domains/marine-engineering/marine_excel_analysis_summary.md:L28-L28]; discovery-only except core readings above. |
| `docs/domains/orcawave/L01_aqwa_benchmark/SESSION_SUMMARY.md` | [docs/domains/orcawave/L01_aqwa_benchmark/SESSION_SUMMARY.md:L5-L5]; discovery-only except core readings above. |
| `docs/domains/development/web-contextualization-deployment-prompt.md` | [docs/domains/development/web-contextualization-deployment-prompt.md:L443-L443]; discovery-only except core readings above. |
| `docs/domains/aqwa/workflows/external-forces/external_force.md` | [docs/domains/aqwa/workflows/external-forces/external_force.md:L20-L20]; discovery-only except core readings above. |
| `docs/domains/marine-engineering/marine_excel_analysis_report.md` | [docs/domains/marine-engineering/marine_excel_analysis_report.md:L5-L5]; discovery-only except core readings above. |
| `docs/domains/charts/phase2/README.md` | [docs/domains/charts/phase2/README.md:L280-L280]; discovery-only except core readings above. |
| `docs/domains/orcawave/PHASE_1_COMPLETION.md` | [docs/domains/orcawave/PHASE_1_COMPLETION.md:L19-L19]; discovery-only except core readings above. |
| `docs/domains/orcawave/DIFFRACTION_CAPABILITIES_EXPANSION_PLAN.md` | [docs/domains/orcawave/DIFFRACTION_CAPABILITIES_EXPANSION_PLAN.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/hull_library/curvature-signature-table.md` | [docs/domains/hull_library/curvature-signature-table.md:L87-L87]; discovery-only except core readings above. |
| `docs/domains/standards/CROSS_REPO_ENFORCEMENT_GUIDE.md` | [docs/domains/standards/CROSS_REPO_ENFORCEMENT_GUIDE.md:L83-L83]; discovery-only except core readings above. |
| `docs/domains/marine-engineering/IMPLEMENTATION_SUMMARY.md` | [docs/domains/marine-engineering/IMPLEMENTATION_SUMMARY.md:L32-L32]; discovery-only except core readings above. |
| `docs/domains/marine-engineering/unified_rao_reader_guide.md` | [docs/domains/marine-engineering/unified_rao_reader_guide.md:L9-L9]; discovery-only except core readings above. |
| `docs/domains/orcawave/L01_aqwa_benchmark/INTERPOLATION_COMPARISON_SUMMARY.md` | [docs/domains/orcawave/L01_aqwa_benchmark/INTERPOLATION_COMPARISON_SUMMARY.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/development/test_summary_with_validations.md` | [docs/domains/development/test_summary_with_validations.md:L56-L56]; discovery-only except core readings above. |
| `docs/domains/development/verification_commands.md` | [docs/domains/development/verification_commands.md:L5-L5]; discovery-only except core readings above. |
| `docs/domains/marine-engineering/marine_3d_models_catalog.md` | [docs/domains/marine-engineering/marine_3d_models_catalog.md:L393-L393]; discovery-only except core readings above. |
| `docs/domains/standards/STANDARDS_INDEX.md` | [docs/domains/standards/STANDARDS_INDEX.md:L258-L258]; discovery-only except core readings above. |
| `docs/domains/marine-engineering/RAO_VERIFICATION_SUMMARY.md` | [docs/domains/marine-engineering/RAO_VERIFICATION_SUMMARY.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/standards/RAO_PHASE_CONVENTIONS.md` | [docs/domains/standards/RAO_PHASE_CONVENTIONS.md:L15-L15]; discovery-only except core readings above. |
| `docs/domains/orcawave/intended-api/README.md` | [docs/domains/orcawave/intended-api/README.md:L15-L15]; discovery-only except core readings above. |
| `docs/domains/marine-engineering/python-code-passing-ship/integration_guide.md` | [docs/domains/marine-engineering/python-code-passing-ship/integration_guide.md:L3-L3]; discovery-only except core readings above. |
| `docs/domains/marine-engineering/MARINE_ENGINEERING_PHASE_1-3_SUMMARY.md` | [docs/domains/marine-engineering/MARINE_ENGINEERING_PHASE_1-3_SUMMARY.md:L104-L104]; discovery-only except core readings above. |
| `docs/domains/marine-engineering/python-code-passing-ship/api_reference.md` | [docs/domains/marine-engineering/python-code-passing-ship/api_reference.md:L441-L441]; discovery-only except core readings above. |
| `docs/domains/marine-engineering/RAO_EXTRACTION_VERIFICATION.md` | [docs/domains/marine-engineering/RAO_EXTRACTION_VERIFICATION.md:L9-L9]; discovery-only except core readings above. |
| `docs/domains/orcawave/L01_aqwa_benchmark/API_INVESTIGATION_SUMMARY.md` | [docs/domains/orcawave/L01_aqwa_benchmark/API_INVESTIGATION_SUMMARY.md:L17-L17]; discovery-only except core readings above. |
| `docs/domains/development/pre_development_testing_checklist.md` | [docs/domains/development/pre_development_testing_checklist.md:L29-L29]; discovery-only except core readings above. |
| `docs/domains/marine-engineering/MARINE_ENGINEERING_DOCUMENTATION_INDEX.md` | [docs/domains/marine-engineering/MARINE_ENGINEERING_DOCUMENTATION_INDEX.md:L224-L224]; discovery-only except core readings above. |
| `docs/domains/marine-engineering/marine_implementation_metrics.md` | [docs/domains/marine-engineering/marine_implementation_metrics.md:L107-L107]; discovery-only except core readings above. |
| `docs/domains/marine-engineering/python-code-passing-ship/README.md` | [docs/domains/marine-engineering/python-code-passing-ship/README.md:L257-L257]; discovery-only except core readings above. |
| `docs/domains/guides/user_stories/cross_platform_bash_scripts_with_uv.md` | [docs/domains/guides/user_stories/cross_platform_bash_scripts_with_uv.md:L34-L34]; discovery-only except core readings above. |
| `docs/domains/orcawave/intended-api/aqwa-benchmark.md` | [docs/domains/orcawave/intended-api/aqwa-benchmark.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/marine-engineering/marine-engineering-excel-mapping.md` | [docs/domains/marine-engineering/marine-engineering-excel-mapping.md:L23-L23]; discovery-only except core readings above. |
| `docs/domains/orcawave/examples/L01_default_vessel/API_EXECUTION_SUMMARY.md` | [docs/domains/orcawave/examples/L01_default_vessel/API_EXECUTION_SUMMARY.md:L156-L156]; discovery-only except core readings above. |
| `docs/domains/marine-engineering/marine-engineering-spec-creation-summary.md` | [docs/domains/marine-engineering/marine-engineering-spec-creation-summary.md:L139-L139]; discovery-only except core readings above. |
| `docs/domains/orcawave/notes/anl_typical_checks/typical_checks.md` | [docs/domains/orcawave/notes/anl_typical_checks/typical_checks.md:L22-L22]; discovery-only except core readings above. |
| `docs/domains/orcawave/L01_aqwa_benchmark/QUICK_REFERENCE_API_FIXES.md` | [docs/domains/orcawave/L01_aqwa_benchmark/QUICK_REFERENCE_API_FIXES.md:L22-L22]; discovery-only except core readings above. |
| `docs/domains/orcawave/L01_aqwa_benchmark/ROOT_CAUSE_ANALYSIS.md` | [docs/domains/orcawave/L01_aqwa_benchmark/ROOT_CAUSE_ANALYSIS.md:L6-L6]; discovery-only except core readings above. |
| `docs/domains/development/DEPLOYMENT-PROMPT-WEB-CONTEXTUALIZATION.md` | [docs/domains/development/DEPLOYMENT-PROMPT-WEB-CONTEXTUALIZATION.md:L292-L292]; discovery-only except core readings above. |
| `docs/domains/orcawave/L01_aqwa_benchmark/README_ORCAWAVE_EXECUTION.md` | [docs/domains/orcawave/L01_aqwa_benchmark/README_ORCAWAVE_EXECUTION.md:L33-L33]; discovery-only except core readings above. |
| `docs/domains/orcawave/examples/L01_default_vessel/LICENSE_TEST_RESULTS.md` | [docs/domains/orcawave/examples/L01_default_vessel/LICENSE_TEST_RESULTS.md:L344-L344]; discovery-only except core readings above. |
| `docs/domains/project-docs/IMPLEMENTATION_PHASE1_SUMMARY.md` | [docs/domains/project-docs/IMPLEMENTATION_PHASE1_SUMMARY.md:L13-L13]; discovery-only except core readings above. |
| `docs/domains/hydrodynamics/morison-elements-assessment.md` | [docs/domains/hydrodynamics/morison-elements-assessment.md:L173-L173]; discovery-only except core readings above. |
| `docs/domains/orcawave/L01_aqwa_benchmark/SESSION_STATUS.md` | [docs/domains/orcawave/L01_aqwa_benchmark/SESSION_STATUS.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/orcawave/README.md` | [docs/domains/orcawave/README.md:L42-L42]; discovery-only except core readings above. |
| `docs/domains/orcawave/L01_aqwa_benchmark/MODEL_UPDATE_SUMMARY.md` | [docs/domains/orcawave/L01_aqwa_benchmark/MODEL_UPDATE_SUMMARY.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/orcawave/L01_aqwa_benchmark/COMPARISON_SUMMARY.md` | [docs/domains/orcawave/L01_aqwa_benchmark/COMPARISON_SUMMARY.md:L1-L1]; discovery-only except core readings above. |
| `docs/domains/orcawave/L01_aqwa_benchmark/ORCAWAVE_GUI_EXECUTION_RESULTS.md` | [docs/domains/orcawave/L01_aqwa_benchmark/ORCAWAVE_GUI_EXECUTION_RESULTS.md:L27-L27]; discovery-only except core readings above. |
| `docs/domains/orcawave/L01_aqwa_benchmark/THREAD_USAGE_SUMMARY.md` | [docs/domains/orcawave/L01_aqwa_benchmark/THREAD_USAGE_SUMMARY.md:L5-L5]; discovery-only except core readings above. |
| `docs/domains/orcawave/L01_aqwa_benchmark/EXECUTION_TEST_RESULTS.md` | [docs/domains/orcawave/L01_aqwa_benchmark/EXECUTION_TEST_RESULTS.md:L4-L4]; discovery-only except core readings above. |
| `docs/domains/development/stability_check_results.md` | [docs/domains/development/stability_check_results.md:L87-L87]; discovery-only except core readings above. |
| `docs/domains/project-docs/phase2_implementation_plan.md` | [docs/domains/project-docs/phase2_implementation_plan.md:L42-L42]; discovery-only except core readings above. |
| `docs/domains/project-docs/FULL_CLEANUP_REPORT.md` | [docs/domains/project-docs/FULL_CLEANUP_REPORT.md:L50-L50]; discovery-only except core readings above. |
| `docs/domains/project-docs/TEST_BASELINE_SUMMARY.md` | [docs/domains/project-docs/TEST_BASELINE_SUMMARY.md:L33-L33]; discovery-only except core readings above. |
| `docs/domains/project-docs/IMPLEMENTATION_COMPLETE_SUMMARY.md` | [docs/domains/project-docs/IMPLEMENTATION_COMPLETE_SUMMARY.md:L97-L97]; discovery-only except core readings above. |
| `docs/domains/project-docs/phase2_validation_summary.md` | [docs/domains/project-docs/phase2_validation_summary.md:L24-L24]; discovery-only except core readings above. |
| `docs/domains/project-docs/phase1-implementation-report.md` | [docs/domains/project-docs/phase1-implementation-report.md:L33-L33]; discovery-only except core readings above. |
| `docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE2.md` | [docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE2.md:L30-L30]; discovery-only except core readings above. |
| `docs/domains/project-docs/MODULE_SURVEY.md` | [docs/domains/project-docs/MODULE_SURVEY.md:L17-L17]; discovery-only except core readings above. |
| `docs/domains/project-docs/MIGRATION_GUIDE.md` | [docs/domains/project-docs/MIGRATION_GUIDE.md:L228-L228]; discovery-only except core readings above. |
| `docs/domains/project-docs/UPDATED_BASELINE_AFTER_ASSETUTILITIES_FIX.md` | [docs/domains/project-docs/UPDATED_BASELINE_AFTER_ASSETUTILITIES_FIX.md:L52-L52]; discovery-only except core readings above. |
| `docs/domains/project-docs/IMPLEMENTATION_SUMMARY_CROSS_REPO_TESTING.md` | [docs/domains/project-docs/IMPLEMENTATION_SUMMARY_CROSS_REPO_TESTING.md:L435-L435]; discovery-only except core readings above. |
| `docs/domains/project-docs/CLEANUP_SUMMARY.md` | [docs/domains/project-docs/CLEANUP_SUMMARY.md:L66-L66]; discovery-only except core readings above. |
| `docs/domains/project-docs/phase2_data_extraction_plan.md` | [docs/domains/project-docs/phase2_data_extraction_plan.md:L30-L30]; discovery-only except core readings above. |
| `docs/domains/project-docs/FINAL_TEST_BASELINE.md` | [docs/domains/project-docs/FINAL_TEST_BASELINE.md:L42-L42]; discovery-only except core readings above. |
| `docs/domains/project-docs/phase1-api-reference.md` | [docs/domains/project-docs/phase1-api-reference.md:L362-L362]; discovery-only except core readings above. |
| `docs/domains/project-docs/IMPLEMENTATION_SUMMARY_PROPAGATION.md` | [docs/domains/project-docs/IMPLEMENTATION_SUMMARY_PROPAGATION.md:L44-L44]; discovery-only except core readings above. |
| `docs/domains/project-docs/MODULE_IMPLEMENTATION_PHASE4.md` | [docs/domains/project-docs/MODULE_IMPLEMENTATION_PHASE4.md:L18-L18]; discovery-only except core readings above. |
| `docs/domains/project-docs/MODULE_PORTFOLIO_2026.md` | [docs/domains/project-docs/MODULE_PORTFOLIO_2026.md:L32-L32]; discovery-only except core readings above. |
| `docs/domains/project-docs/IMPLEMENTATION_PHASE2_SUMMARY.md` | [docs/domains/project-docs/IMPLEMENTATION_PHASE2_SUMMARY.md:L168-L168]; discovery-only except core readings above. |
| `docs/domains/project-docs/PRODUCTION_READINESS_CHECKLIST.md` | [docs/domains/project-docs/PRODUCTION_READINESS_CHECKLIST.md:L241-L241]; discovery-only except core readings above. |
| `docs/domains/project-docs/MODULE_IMPLEMENTATION_RECOMMENDATIONS.md` | [docs/domains/project-docs/MODULE_IMPLEMENTATION_RECOMMENDATIONS.md:L29-L29]; discovery-only except core readings above. |
| `docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE3.md` | [docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE3.md:L42-L42]; discovery-only except core readings above. |
| `docs/domains/project-docs/README.md` | [docs/domains/project-docs/README.md:L9-L9]; discovery-only except core readings above. |

Discovery paths withheld for identifying-name review: 2; issue pending (5).

### Offline execution record

At code revision `6cf35f466e5c1b730951af045d8cfe0065af073f`, the baseline module command reported 60 passed, 12 skipped and 0 failed; NOT_RUN is 1 guarded module import, included in the skipped total rather than added to it. The source count is 65 functions; parameterization and module-collection reporting account for the different outcome count. [tests/test_aqwa.py:L1-L20][tests/hydrodynamics/aqwa/test_aqwa_anl_raos.py:L6-L11][tests/hydrodynamics/aqwa/test_aqwa_cli_integration.py:L19-L22][tests/hydrodynamics/aqwa/test_aqwa_lis_parser_real_data.py:L96-L116]

Generic commands, run sequentially with the supplied full executable path and isolated environment:

```sh
PYTHONPATH=src uv run --no-sources --extra test python -m pytest tests/hydrodynamics/aqwa tests/test_aqwa.py -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-base
PYTHONPATH=src uv run --no-sources --extra test python -m pytest tests/contracts tests/docs/test_digitalmodel_routing_contract.py -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-contract-base
```

The baseline contract/routing command reported 494 passed, 0 failed and 0 skipped. Post-edit commands use the same form and separate scratch basetemp names; contract/routing result: 494 passed, 0 failed and 0 skipped, identical to baseline. The test extra is enabled; no pytest addopts override or existing skip-marker modification is used. These documentation checks run on an uncommitted tree whose source-code pin remains the revision above.
