# OrcaFlex format converter knowledge pack

| Header | Value |
| --- | --- |
| Module | solvers/orcaflex/format_converter |
| Repository | digitalmodel |
| Package | src/digitalmodel/solvers/orcaflex/format_converter |
| Pack revision | 4, 2026-10-25 |
| Code revision | 7aad603be1c6136106dbb2381ce5096b050654f8 |
| Analysis family | OrcaFlex riser and mooring; text conversion, routing family MR |
| Quality role | OrcaFlex riser/mooring quality manager |
| Status | DRAFT; second-provider review pending |

The code revision is the evidence pin for repository citations, including the implementation map in section 9.

## 1. Purpose and scope

The package exposes six directed conversions between specification YAML, modular YAML and single model YAML, plus format detection and CLI dispatch. [src/digitalmodel/solvers/orcaflex/format_converter/__init__.py:L1-L29] [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L184-L243]

Single-to-modular splits sections into include files and writes a master and extracted parameters; modular-to-single merges master content and includes into one YAML document. [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L88-L157] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L67-L90]

Specification-to-modular delegates to the modular generator; the specification-to-single and single-to-specification routes chain converters through a temporary directory. [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L42-L64] [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L42-L83] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py:L42-L88]

Reverse extraction maps selected Environment and General fields, takes only the first line name for pipeline metadata, and reports omitted sections as expected or actionable gaps; its confidence is field coverage, not a solver or model-equivalence result. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L20-L46] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L164-L221]

The routing registry assigns beta maturity to the enclosing solvers domain, without a separate converter maturity field. [docs/registry/module-routing.yaml:L303-L327]

UNVERIFIED: Supported target program versions, geometric applicability and physical-load fidelity are not established by the offline evidence; native acceptance remains open in #280.

Reverse extraction is best-effort, with expected and actionable gaps reported separately; section 10 item O1 retains the native-qualification boundary. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L48-L85] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L172-L222]

### Revision history

Revisions 0–3 are reconstructed from the recorded pack headers and changes; revision 0 was first committed on 2026-10-17 after the 2026-10-16 execution record.

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-17 | Added the converter knowledge pack to document implementation evidence and qualification gaps. |
| 1 | 2026-10-17 | Clarified gate applicability and fixture drift, corrected the missing-include citation and reconciled implementation coverage. |
| 2 | 2026-10-18 | Classified stale-fixture outcomes as SKIP to distinguish executed pytest cases from unexecuted qualification procedures. |
| 3 | 2026-10-18 | Retained original-against-converted statics parity for both conversion routes and separated downstream model qualification. |
| 4 | 2026-10-25 | Linked the G2 procedure to check-11a, recorded revision provenance and identified the unavailable gate source so acceptance evidence can be traced. |

## 2. Inputs and outputs

### Inputs

| Input | Unit and convention | Range / default | Source and evidence |
| --- | --- | --- | --- |
| Single model YAML | Values retain their supplied units; no unit conversion in grouping | Nonempty mapping; source path required | `SingleToModularConverter.convert`, `_group_by_filename` [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L88-L122] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L191-L215] |
| Modular master | Include paths relative to master directory, or absolute | List or mapping; default constructor source is absent | `_merge_master`, `_resolve_include_path` [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L26-L49] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L107-L165] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L206-L212] |
| Specification YAML | Upstream schema determines dimensions and ranges | No local defaults for engineering quantities; upstream validation | `ModularModelGenerator` construction and generation [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L35-L55] |
| CLI source / output | Filesystem paths, no physical unit | `source` positional; output defaults to None | `add_conversion_args` [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L169-L182] |
| Reverse extraction | Copies depth, density, wave/current/wind direction and simulation values without converting units or coordinates | Absent fields omitted; missing wave type becomes Airy | `EXTRACTION_MAP`, `_populate_wave_trains` [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L20-L43] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L249-L278] |

UNVERIFIED: A universal coordinate frame, origin, direction sign, phase convention and unit system for arbitrary input files have not been established; the converter copies values rather than validating those conventions.

### Upstream specification dimensions

These constraints belong to the delegated schema; they do not qualify arbitrary single-file input values. [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L42-L55]

| Quantity | Unit / convention | Range and default from schema | Evidence |
| --- | --- | --- | --- |
| Water depth | m | Greater than 0; required | [src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L10-L25] |
| Water density | te/m3 | Greater than 0; default 1.025 | [src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L10-L25] |
| Wave height / period | m / s; deterministic or spectral meaning depends on type | Height at least 0, default 0; period greater than 0, default 8 | [src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L74-L100] |
| Wave direction / phase | Degrees from North, going to; phase in degrees | Direction defaults 0 and is normalized modulo 360; phase defaults 0 and rejects nonzero phase | [src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L74-L126] |
| Current speed / direction / profile depth | m/s; degrees from North, going to; m positive downward for profile depth | Speed at least 0, default 0; direction default 0; profile default pairs begin at depth 0 with factor 1.0 | [src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L196-L225] |
| Time step / stages / North | s / s / degrees | Time step greater than 0 and at most 1, default 0.1; at least 1 stage, default durations 8 and 16; North at least 0 and less than 360, default 0 | [src/digitalmodel/solvers/orcaflex/modular_generator/schema/simulation.py:L8-L36] |

### Outputs

| Output | Unit / range | Default destination / meaning | Evidence |
| --- | --- | --- | --- |
| Modular directory | YAML, section values unchanged | Source parent / source stem; master.yml, includes, inputs/parameters.yml when parameters exist | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L61-L67] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L129-L148] |
| Single model YAML | YAML, no physical conversion | Master directory / directory-name.yml; spec source stem + `_model.yml` for spec-to-single | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L61-L81] [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L35-L40] |
| Extracted specification | Partial YAML, review required | Source parent / extracted_spec.yml for modular route; source stem + `_spec.yml` for single route | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L105-L109] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L194-L221] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py:L35-L40] |
| ConversionReport | success Boolean; confidence dimensionless, default 1.0; lists default empty | Paths, formats, warnings, unmapped sections, best-effort flag and categorized gaps | [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L15-L44] |
| CLI exit code | integer status code | 0 on successful report; 1 on unsuccessful report; warnings alone do not fail | [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L34-L69] |

No coordinate, origin, sign or phase transformation is performed by the extraction map: selected values are assigned directly. This does not establish the convention or unit validity of an arbitrary source; section 10 item O2 retains the design-basis qualification gap. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L20-L43] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L143-L150]

## 3. Method and governing standards

No engineering formula or licensed-standard criterion is used in the conversion algorithms below; this pack makes no engineering-design validation claim from text conversion. [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L88-L215] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L107-L238] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L137-L221]

| Definition / algorithm | Implemented rule and boundary | Evidence |
| --- | --- | --- |
| `detect_format` | Directory structure first; YAML list with any includefile means modular; metadata plus pipeline or environment means spec; recognized section keys mean single | [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L30-L91] |
| `SingleToModularConverter.convert`, `_group_by_filename` | Group sections using canonical and extra mappings; unknown sections go to 99_unmapped.yml; sorted filenames determine includes | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L114-L139] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L191-L215] |
| `SingleToModularConverter._extract_input_parameters` | Copy configured keys, omit None / tilde; only the first WaveTrains entry feeds parameter comments and parameters.yml | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L159-L189] [src/digitalmodel/solvers/orcaflex/format_converter/section_mapping.py:L44-L61] |
| `SingleToModularConverter._write_include_file`, `_write_master_file`, `_write_parameters_file` | Serialize grouped data, source headers and include directives; include and parameter comments contain the current time | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L217-L277] |
| `ModularToSingleConverter.convert`, `merge_to_dict`, `_merge_master` | Preserve mapping master's inline data, resolve top-level and section-level includes; merge_to_dict discards accumulated warnings | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L67-L105] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L107-L165] |
| `ModularToSingleConverter._load_include`, `_extract_includefiles.traverse`, `_resolve_include_path` | Load one referenced mapping; scan include references recursively, but merging does not recursively load includes within fragments | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L167-L212] |
| `ModularToSingleConverter._deep_merge` | Recurse on dict pairs, concatenate list pairs, otherwise let the later value win; this is a Python merge policy, not native program equivalence | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L214-L238] [src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L208-L212] |
| `ModularToSpecConverter.convert`, `_set_nested` | Map 16 configured fields into dotted spec paths; raw field count / map length forms confidence, rounded to two decimals in the report | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L20-L46] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L137-L150] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L189-L221] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L299-L305] |
| `ModularToSpecConverter._populate_wave_trains` | Reverse-map wave types; recover height/period from regular or spectral keys; a single train becomes flat fields, multiple trains a list | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L227-L297] |
| Spec generation and chained routes | Delegate to the modular generator and the listed converters rather than implement physical analyses | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L42-L55] [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L42-L74] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py:L42-L88] |

AQG r0, sections 4–9 and 10, supplies the gate contract in section 5; it is not a solver manual or a licensed engineering standard. (Analysis QA gates, revision 0, sections 4–10.)

UNVERIFIED: A retrievable source for AQG r0 has not been established. The title, revision and section references identify the intended source; its gate thresholds remain unconfirmed independently.

The documented reverse-extraction boundary warns against treating extracted specifications as lossless model definitions. [docs/domains/orcaflex/QUICKSTART_FILE_CONVERSION.md:L295-L350]
UNVERIFIED: no governing engineering-standard edition, clause or calculation Citation sidecar was established for these transformations; section 10 item O2 records the qualification boundary.

## 4. Validated worked examples and regression examples

Validated examples: 0; independent physical-reference values are not supplied by the examples below, which compare synthetic inputs or serialization contracts. The cases are regression examples rather than physical validation. [tests/solvers/orcaflex/format_converter/test_round_trip.py:L19-L54] [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L261-L310]

Execution record, 2026-10-16: the baseline suite at the code pin collected 131 cases: 96 PASS, 0 FAIL and 35 pytest skips recorded here as SKIP because the fixture roots still point to docs/modules rather than docs/domains; the reference data is present at the code pin. Of these, the converter test directory contributes 116 cases (81 PASS, 35 SKIP), and the related generator fidelity file contributes 15 PASS. Section 10 enumerates the node IDs and results; these are observations of this pack's offline run, not results inferred from historical counts.

| Case / kind | Source and input | Expected result / tolerance basis | Existing test node ID | Offline result at pin |
| --- | --- | --- | --- | --- |
| regression-1 | Synthetic input dictionary, no independent reference | 0 changed original section values; exact equality, dimensionless field count | `tests/solvers/orcaflex/format_converter/test_round_trip.py::TestRoundTrip::test_roundtrip_synthetic_data` | PASS [tests/solvers/orcaflex/format_converter/test_round_trip.py:L19-L54] |
| regression-2 | Synthetic dictionary master with section-scoped fragment | WaterDepth 100 m, RefCurrentSpeed 0.5 m/s; 0 misplaced top-level fields; exact authored inputs | `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_dict_master_preserves_content_and_section_scope` | PASS [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L228-L269] |
| regression-3 | Synthetic single spectral train | Height 3.0 m, period 8.0 s, direction 180 degrees and type jonswap; exact authored inputs | `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_extract_single_spectral_train_keeps_hs_tz` | PASS [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L261-L310] |
| regression-4 | Synthetic unknown section | 99_unmapped.yml is last; 0 unknown fields in General; at least 1 warning | `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_unknown_sections_quarantined_last` | PASS [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L187-L225] |
| regression-5 | Synthetic out-of-scope object sections | 3 expected gap names; 0 misclassified VesselTypes entries in actionable gaps | `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecBoundaryReporting::test_expected_gaps_classified` | PASS [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L393-L409] |
| regression-6 | Synthetic None scalar | 1 literal `key: ~` representation; exact token comparison | `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexDumper::test_none_represented_as_tilde` | PASS [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L18-L21] |
| regression-7 | Dataset ID A01; external fixture requested by existing fixture | General and Environment equality; LineTypes equality when present; no physical-reference assertion | `tests/solvers/orcaflex/format_converter/test_round_trip.py::TestRoundTrip::test_roundtrip_real_a01` | FAIL with relocated fixture: General blank scalar differs (empty text versus None); #2008 [tests/solvers/orcaflex/format_converter/test_round_trip.py:L56-L90] |
| regression-8 | Specification fixture requested by existing fixture | 1 master.yml and 1 includes directory; exact presence checks | `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_convert_creates_modular_structure` | PASS with relocated fixture; #2008 [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L25-L33] |
| regression-9 | Shared buoy sections; synthetic input | three section keys preserved; 0 differing values (count). [tests/solvers/orcaflex/format_converter/test_round_trip.py:L92-L126] | `tests/solvers/orcaflex/format_converter/test_round_trip.py::TestRoundTrip::test_roundtrip_with_multiple_buoy_sections` | PASS |
| regression-10 | Nested round trip; synthetic input | whole dictionary equal; 0 differing values (count). [tests/solvers/orcaflex/format_converter/test_round_trip.py:L128-L164] | `tests/solvers/orcaflex/format_converter/test_round_trip.py::TestRoundTrip::test_roundtrip_preserves_nested_data` | PASS |
| regression-11 | Boundary report; synthetic input | best-effort flag true: 1 true flag (count). [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L377-L383] | `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecBoundaryReporting::test_is_best_effort_true` | PASS |

Relocated-data execution, 2026-10-17: a scratch copy of all 116 converter tests changed only the fixture root to the owning checkout and docs/modules to docs/domains. Result: 115 PASS, 1 FAIL, 0 skips. Regression-7 fails at test_round_trip.py:L86 because General.RestartStateRecordingTest is empty text after the native-aware load but None through the test's yaml.safe_load reference; regression-8 passes. This exposes a comparison-basis limitation rather than qualifying semantic equality; the shipped fixture roots and production behavior remain unchanged. #2008 [tests/solvers/orcaflex/format_converter/test_round_trip.py:L56-L90] [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L17-L34]

## 5. QA gates

The gate set is AQG r0, section 10, “OrcaFlex riser and mooring”; G0 and G1 apply to conversion outputs; G2 applies to original-against-converted statics parity in check-11a, which remains NOT_RUN under #280; the converter itself performs no structural solution. G3 is NOT_APPLICABLE to the converter itself. C4 and C5 remain downstream model qualification checks, G4 is conditional on new diffraction data, and G5 applies to every issued report (AQG r0, sections 4–11).
These procedures define acceptance checks; offline representation checks do not sign an engineering model's solver gates. [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L15-L44]

| Gate | Applicability | Check and executable procedure | Numeric pass criterion and unit | Evidence / current status |
| --- | --- | --- | --- | --- |
| G0 | Applicable | C0: exercise `SingleToModularConverter.convert`, `ModularToSingleConverter.convert` and `ModularToSpecConverter.convert` through the six section-4 nodes and `TestSingleToModularConverter.test_unknown_sections_quarantined_last` in the module test directory. | 0 assertion failures (count); exact source-value equality. [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L187-L225] | pytest node outcomes: PASS |
| G0 | Applicable | C1: execute `TestModularToSingleConverter.test_missing_include_warns`; inspect `report.warnings`, never accept `success` alone. | At least 1 missing-include warning (count) for the negative fixture; 0 warnings (count) required for a production conversion. [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L99-L112] | Negative regression PASS |
| G0 | Applicable | C2: strict-parse emitted master and all includes with duplicate keys rejected; compare source UnitsSystem and every transferred numeric value; verify every referenced include exists. | 0 duplicate keys, 0 unresolved files, 0 changed source values (counts); 0 unit-system differences (count). [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L121-L147] | Proposed acceptance inspection; no detection check exists in the shipped tests for all these conditions; issue pending (1) |
| G1 | Applicable | C3: load the emitted `master.yml` or merged single YAML in the target OrcaFlex version, through the version-matched demo route or the licensed LoadData route; compare object inventory with source. | 0 load warnings, 0 load errors, 0 object-count differences (counts); exact version match. | NOT_RUN: licensed interface and solver execution excluded; #280 (AQG r0, section 5) |
| G2 | Applicable to conversion parity in check-11a; C4 is downstream | Conversion parity: execute check-11a in the additional checks table below for both conversion routes, including its separate parity criterion and zero-source-tension handling. C4 (downstream model check): calculate statics for the converted model; compare line top tension and angle with an independent hand calculation; record every object and offset sweep. | 0 unconverged models (count); tension difference at most 5 %, angle difference at most 1.0 degree; at least 6 offsets (count); 0 unchecked objects (count). | Converter performs no statics solution; C4 does not establish conversion fidelity. Downstream C4 NOT_RUN: licensed solver required; #280 (AQG r0, section 6) |
| G3 | NOT_APPLICABLE to converter | C5 (downstream model check): run converted model from converged statics with production settings and retain the final image and warnings. | Reach 1 s simulated time; 0 errors, warnings or nonnumeric results (counts); tension change at most 25 % relative to statics. | Converter performs no dynamics solution; C5 does not establish conversion fidelity. Downstream C5 NOT_RUN: licensed solver required; #280 (AQG r0, section 7) |
| G4 | Conditional | C6: for a new diffraction-derived VesselTypes input, require its diffraction record; compare balance and independent displacement against the producing run. | 0 missing records (count); weight/buoyancy mismatch at most 1.0 %; displacement mismatch at most 0.5 %. | NOT_APPLICABLE to synthetic conversion cases; no new diffraction run supplies them. [tests/solvers/orcaflex/format_converter/conftest.py:L59-L89] (AQG r0, section 8) |
| G5 | Applicable to issued report | C7: reconcile ConversionReport warnings, expected_gaps and actionable_gaps with the report appendix and link the solver-gate records by input digest. | 0 omitted warnings or gaps (counts); 0 missing required gate records (count). [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L19-L44] | NOT_RUN: no engineering report or solver record issued; #280 (AQG r0, section 9) |
| G0 | Applicable | C8: execute all `TestModularToSpecBoundaryReporting` nodes and `TestCLIReporting.test_print_report_surfaces_expected_and_actionable_gaps`. | 0 uncategorised unmapped sections (count); 1 best-effort flag (count); both expected and actionable categories shown (2 categories). [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L377-L459] [tests/solvers/orcaflex/format_converter/test_cli.py:L126-L149] | PASS |
| G0 | Applicable | C9: execute `TestSpecToModularConverter.test_convert_invalid_spec_returns_failure` and `TestSpecToSingleConverter.test_invalid_spec_propagates_failure`. | 0 successful reports (count) for invalid specification; at least 1 warning (count) at generator wrapper. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L44-L56] [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L130-L141] | PASS |

### Additional executable checks and downstream procedures

The check-01–check-14 identifiers below supplement C0–C9 above. Check-11a tests converter-route statics parity and is required before a converted model is used. Check-11b and check-12 qualify a downstream engineering model; those downstream procedures alone do not establish conversion fidelity.

| Gate / status | Check | Module-specific pass criterion, quantity and unit | Execution / evidence |
| --- | --- | --- | --- |
| G0 applicable; offline subset PASS | check-01 | SingleToModularConverter then ModularToSingleConverter: 0 changed synthetic section values, count of fields | regression-1; dictionary comparison [tests/solvers/orcaflex/format_converter/test_round_trip.py:L19-L54] |
| G0 applicable; defect characterization PASS; production criterion NOT_RUN | check-02 | Missing include fixture: 1 successful conversion report and at least 1 missing-include warning; production use additionally requires 0 unresolved include paths, count of paths | No executable check exists for the production criterion; it remains NOT_RUN. The existing test characterizes fail-open warning behavior rather than qualifying partial output: `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_missing_include_warns` [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L99-L112] |
| G0 applicable; offline subset PASS | check-03 | ModularToSpecConverter: spectral height 3.0 m, period 8.0 s, direction 180 degrees, gamma 2.0 dimensionless and mapped type jonswap; exact synthetic inputs | regression-3; extracted YAML comparison [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L261-L310] |
| G0 applicable; offline subset PASS | check-04 | SingleToModularConverter: 0 include-order inversions for line types / lines before friction and browser groups; unknown include last, count of inversions | regression-4 and `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_real_sections_ordered_after_referenced_objects` [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L227-L278] |
| G0 applicable; offline subset PASS | check-05 | ModularToSpecConverter: 0 uncategorized unmapped section names; report.is_best_effort equals True (1 Boolean assertion) | Existing tests: `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecBoundaryReporting::test_all_unmapped_are_categorized` and `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecBoundaryReporting::test_is_best_effort_true` [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L377-L450] |
| G0 applicable; offline subset PASS | check-06 | OrcaFlexDumper: 1 expected tilde scalar for 1 None input and 0 alternate null tokens in that scalar; count of tokens | regression-6; exact serialization assertion [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L18-L21] |
| G0 applicable; offline subset PASS | check-07 | SpecToModularConverter: invalid spec produces 0 successful reports and at least 1 warning; count of reports / warnings | Existing invalid-spec test: `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_convert_invalid_spec_returns_failure` [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L44-L54] |
| G0 applicable; evidence incomplete | check-08 | For each input/output pair, compare all declared units, coordinate origin, direction and phase conventions; 0 unrecorded changes, count of conventions | Hand audit of the input UnitsSystem and fields against the extraction map; no automatic convention validator exists; #520 [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L20-L43] |
| G0 applicable; defect reproduced | check-09 | Negative include probes: missing, empty or wrong-type fragments and malformed master entries each leave 0 unreported omissions; count of omitted records | Reproduce using one valid Environment fragment plus one absent fragment; compare convert warnings against merge_to_dict/reverse report; observed reverse warnings 0 despite 1 missing fragment, issue pending (1) [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L95-L105] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L124-L181] |
| G1 applicable; NOT_RUN | check-10 | For each converter output, OrcaFlex load: 0 errors, 0 load warnings and 0 object-count mismatches against the source manifest; count of messages / objects | Open converted master.yml or single output in the matching-version demo; if that route is unavailable use LoadData through the licensed interface, recording exact version and messages; #280 [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L129-L156] [docs/plans/2026-05-05-issue-280-orcaflex-format-converter-license-validation.md:L15-L34] |
| G2 applicable; NOT_RUN (#280) | check-11a | For SingleToModularConverter.convert, compare source single YAML against emitted master.yml; for ModularToSingleConverter.convert, compare source master against emitted single YAML. Each original/converted pair: 0 unconverged statics and touchdown tension relative change at most 0.1 percent, calculated from unrounded tensions in the same declared force unit. | Licensed statics with matching program version, inputs and solver settings; retain source/output fingerprints and both tensions with units. Relative change uses the absolute source tension as denominator; a zero source tension requires a separately agreed absolute criterion before acceptance. No parity result is established; #280 [docs/plans/2026-05-05-issue-280-orcaflex-format-converter-license-validation.md:L36-L40] |
| G2 downstream; NOT_RUN | check-11b | Independent hand catenary comparison for the converted model; all component rows covered, 0 unchecked objects; at least 6 offset steps per AQG r0 | Licensed statics, images, per-line hand reference and component/offset records; no converter function is a physical hand reference; #280 [docs/plans/2026-05-05-issue-280-orcaflex-format-converter-license-validation.md:L36-L40] (AQG r0, section 6.) |
| G3 NOT_APPLICABLE to converter; downstream NOT_RUN | check-12 | Converted model uses production settings for 1.0 s dynamics; 0 errors / warnings / nonnumeric values; line tension within 25.0 percent of static and 0 newly slack or compressed lines | Native short run from converged statics; record source and output fingerprints with final image; #280 (AQG r0, section 7.) [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L63-L83] |
| G4 conditional; NOT_APPLICABLE to synthetic text cases | check-13 | When converted VesselTypes come from a new diffraction run: 1 matching accepted G4 record per imported dataset and 0 unmatched datasets; count of records / datasets | Read source provenance and link the existing diffraction record; synthetic text fixtures contain no new diffraction result; native campaign provenance not checked; #280 (AQG r0, sections 8 and 10.) [src/digitalmodel/solvers/orcaflex/format_converter/section_mapping.py:L11-L35] [tests/solvers/orcaflex/format_converter/test_round_trip.py:L131-L164] |
| G5 applicable; evidence incomplete | check-14 | For every released converted model, 1 evidence entry keyed by input fingerprint, 0 missing required gate records, 0 unrecorded gaps; count of records / gaps | Hand inspection of appendix manifest linking ConversionReport, gate records, input digests, images and dispositions; no automatic appendix adapter exists, issue pending (5) [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L15-L30] (AQG r0, section 9.) |

For check-11b, the independent hand comparison also requires top tension within 5.0 percent, top angle within 1.0 degree and touchdown distance within 5.0 percent; the offset sweep and complete component table remain separate required evidence, rather than consequences of round-trip equality. (AQG r0, sections 6.3–6.6.)

## 6. Known failure modes and pitfalls

sweep: 35 found

The count below is 35 reviewed source behaviors, including explicit error reports and benign initializers, rather than 35 confirmed defects. Each row records whether a detection check exists; an existing regression that merely preserves a failure mode is identified as such.

Exact discovery commands, applied to the package's pinned Python files, followed by reading every definition and its callers:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|5%|float\(|int\(|units?|density|direction|cache|\.get\(' src/digitalmodel/solvers/orcaflex/format_converter --glob '*.py'
rg -n 'return|default|setdefault|WaveTrains|UnitsSystem|confidence|warnings|write' src/digitalmodel/solvers/orcaflex/format_converter --glob '*.py'
```

| Row | Symptom / cause at the pin | Detection and disposition |
| --- | --- | --- |
| sweep-01 | Scalar records in list masters are skipped with continue. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L124-L127] | check-09; issue pending (1) |
| sweep-02 | Missing include returns None with a warning; convert still returns success. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L167-L174] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L79-L90] | check-02 / check-09; issue pending (1) |
| sweep-03 | Empty or nonmapping include returns None without any warning. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L176-L181] | check-09; issue pending (1) |
| sweep-04 | merge_to_dict passes a new empty warning list and returns only the merged data, losing missing-include warnings before reverse extraction. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L95-L105] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L111-L117] | check-09; issue pending (1) |
| sweep-05 | Unsupported / empty master data leaves the empty merge accumulator and can write an empty successful output. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L122-L165] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L79-L90] | check-09; issue pending (1) |
| sweep-06 | No include directives emits only a warning and still succeeds; inline-only masters can also be intentional. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L70-L90] | check-02; issue pending (1) |
| sweep-07 | Parameter extraction silently skips nonmapping General or Environment sections. [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L164-L169] | no detection check exists; #520 |
| sweep-08 | Parameter extraction silently omits None and literal tilde values. [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L170-L175] | no detection check exists; #520 |
| sweep-09 | Only the first wave train supplies parameters.yml wave values; subsequent trains are absent from this summary. [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L177-L189] | no detection check exists; #520 |
| sweep-10 | Absent Environment defaults to an empty dict and first-train setup is bypassed on unsupported structure. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L129-L135] | no detection check exists; #520 |
| sweep-11 | Missing fields start at None and are omitted; no field-level omissions enter warnings. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L143-L150] | no detection check exists; #520 |
| sweep-12 | Lines defaults to an empty list, only its first dict Name is copied, and a non-dict first entry becomes an empty dict. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L164-L170] | no detection check exists; #520 |
| sweep-13 | Metadata is filled with extracted_model / unknown placeholders rather than recovered provenance. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L155-L162] | no detection check exists; #520 |
| sweep-14 | General, Environment and VariableData are treated as known sections even when most fields are not extracted; gap lists are section-level only. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L172-L187] | check-05 only covers section categories; no detection check exists for omitted fields; #520 |
| sweep-15 | extracted_count starts at 0; zero denominator fallback is 0.0; confidence is raw mapped-field coverage, not completeness after wave recovery. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L137-L150] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L189-L192] | no detection check exists for score consistency; #520 |
| sweep-16 | Wave population defaults Environment to an empty dict and returns on nonmapping data. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L249-L251] | no detection check exists; #520 |
| sweep-17 | WaveTrains defaults to an empty list and empty/nonlist data causes an early return. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L252-L254] | no detection check exists; #520 |
| sweep-18 | Nonmapping wave train entries are skipped with continue. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L256-L259] | no detection check exists; #520 |
| sweep-19 | A missing WaveType becomes Airy without a warning. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L260-L264] | no detection check exists; #520 |
| sweep-20 | An unknown wave type passes through unchanged. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L263-L264] | no detection check exists; #520 |
| sweep-21 | Multiple trains replace the flat waves fields; default empty dict lookups and pop(..., None) suppress absent-field errors. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L280-L286] | no detection check exists for malformed mixed input; #520 |
| sweep-22 | Report unmapped_sections, warnings, expected_gaps and actionable_gaps default to separate empty lists; this stores absence of findings rather than proving no omissions. [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L24-L30] | check-05 covers categories after conversion; #520 |
| sweep-23 | Report confidence defaults to 1.0 even outside measured reverse extraction. [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L19-L25] | no detection check exists for interpreting default confidence; #520 |
| sweep-24 | Report is_best_effort defaults False; SingleToSpecConverter explicitly propagates reverse boundary fields. [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L27-L30] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py:L75-L88] | check-05 and existing single-to-spec boundary test; #520 |
| sweep-25 | Spec generation catches every Exception and returns failure plus the error string; failures are explicit, not raised. [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L42-L55] | check-07; explicit failure handling, #280 |
| sweep-26 | cmd_detect catches ValueError, prints an error and returns 1; YAML parser errors are not caught there. [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L104-L112] | existing detect-nonexistent test; explicit failure handling, #280 |
| sweep-27 | cmd_auto catches detection ValueError and returns 1; unsupported same-format conversion also returns 1. [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L119-L123] [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L148-L153] | no detection check exists for malformed YAML through auto; #280 |
| sweep-28 | Falsy loaded single input returns an unsuccessful report after output directories have been created. [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L88-L109] | existing empty-file regression; explicit failure handling, #280 |
| sweep-29 | Reverse extraction copies numeric values but omits UnitsSystem, coordinate origin and wave phase/time-origin conventions. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L20-L43] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L260-L278] | check-08; #520 |
| sweep-30 | Include fragments are loaded once; recursive discovery does not imply recursive merging of includefiles inside loaded fragments. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L167-L204] | no detection check exists; issue pending (1) |
| sweep-31 | List concatenation can differ from native list collection replacement; Python equality alone cannot prove native model parity. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L214-L238] [src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L208-L212] | check-10 / check-11a NOT_RUN; #515 / #280 |
| sweep-32 | Includes use yaml.safe_load rather than the native blank-scalar loader used for the master; blank text can become None in a split/merge path. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L67-L68] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L176-L181] [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L17-L34] | no detection check exists for this cross-loader case; issue pending (1) |
| sweep-33 | CLI parses dry-run and batch but command handlers execute conversion without consulting either flag; a dry run can write files. [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L53-L101] [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L169-L182] | no shipped detection check exists; offline dry-run probe confirmed 1 master written, issue pending (2) |
| sweep-34 | WaterDepth alone is extracted; the generator can emit SeabedOriginDepth, so round-trip extraction can omit depth without a warning. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L20-L26] [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L366-L420] | no detection check exists that requires depth preservation for the alias; #520 |
| sweep-35 | Physical source values are copied without unit conversion or numeric-text parsing; non-SI values and decimal-separator text are not qualified by the synthetic suite. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L143-L150] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L170-L187] | no detection check exists; issue pending (7) |

The sweep found no int() / float() conversion of external text, no 1970 sentinel, no five-percent fallback constant and no cache writer inside the package; the numeric zero is the confidence accumulator / fallback, while empty report lists are result containers. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L137-L150] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L189-L192] [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L15-L30]

### Confirmed incidents and limitations

| Incident / limitation | Detection check and state | Evidence |
| --- | --- | --- |
| #2008: stale fixture roots silently skip real-file coverage | Data is present at the pin under docs/domains/orcaflex/examples/raw/A01/, docs/domains/orcaflex/examples/modular/A01/ and docs/domains/orcaflex/pipeline/installation/floating/30in_pipeline/spec.yml. No shipped check fails on the stale roots; the unmodified suite records 35 SKIP. Scratch relocation runs all 116 converter cases: 115 PASS, 1 FAIL on the blank-scalar comparison in regression-7 | [tests/solvers/orcaflex/format_converter/conftest.py:L9-L55] |
| #515: historical dictionary-master content / scope loss | regression-2 / check-01; fixed behavior covered by a passing regression | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L228-L269] |
| #515: historical loss of single spectral height, period and type | regression-3 / check-03; fixed behavior covered by a passing regression | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L261-L310] |
| #520: reverse extraction is partial, even with high confidence | check-05; field-level coverage gaps remain as detailed above | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L172-L221] |
| #280: native load, statics and binary round-trip evidence incomplete | check-10 / check-11a / check-11b / check-12 NOT_RUN; no licensed solver invoked | [docs/plans/2026-05-05-issue-280-orcaflex-format-converter-license-validation.md:L15-L49] |
| #518 / #2014: wider model-library fidelity is outside this synthetic evidence | no detection check exists within this package that qualifies the full library | [tests/solvers/orcaflex/format_converter/test_round_trip.py:L19-L92] |
| Quick-start text gives a 14-field confidence denominator; current map has 16 fields | Hand-count EXTRACTION_MAP; documentation count is stale, #520 | [docs/domains/orcaflex/QUICKSTART_FILE_CONVERSION.md:L329-L334] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L20-L46] |

## 7. Lessons log

| Date | Change reference | Lesson | Evidence | Pack change / shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-16 | Pack revision 0 | Separate successful text conversion from native acceptance; reverse-extraction confidence counts fields | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L189-L221] | Sections 1, 3 and 5; shared-log entry not assigned |
| 2026-10-16 | Pack revision 0 | Inspect warning propagation through merge_to_dict before relying on extracted output | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L95-L105] | Sections 5 and 6; issue pending (1); shared-log entry not assigned |
| 2026-10-16 | Pack revision 0 | Stale fixture roots hide available reference data and lose coverage even when pytest exits successfully | [tests/solvers/orcaflex/format_converter/conftest.py:L9-L55] | Sections 4, 6 and 10; #2008; shared-log entry not assigned |
| 2026-10-17 | Initial pack | Distinguish representation regressions from model qualification; do not use confidence as whole-model fidelity. | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L189-L222] |Sections 4–6 define this boundary. Shared-log entry: Not created; issue pending (7) |
| 2026-10-25 | Pack revision 4 | Check a gate procedure cell against its applicability cell; downstream hand checks cannot establish conversion parity. | Section 5, G2 and check-11a | G2 now links directly to the separate parity procedure; source retrieval remains open. |

## 8. Review responsibilities and escalation

Proposed review responsibilities: riser and mooring quality manager; deputy: installation quality manager; producing discipline: engineering software; ambiguous method classifications will be referred to the family quality manager, with cross-family questions referred to the maintainer (AQG r0, section 11).
UNVERIFIED: named role assignments and deputy availability are not established; section 10 item O3 records the assignment gap.

Escalate disputed implementation classifications, tolerances or interpretation to the family quality manager; cross-family evidence questions go to the maintainers. Keep native gate acceptance separate from the producing agent's text checks. (AQG r0, section 11.)

## 9. Dependencies and implementation inventory

### Dependencies and solver boundary

| Dependency / consumer | Boundary | Evidence |
| --- | --- | --- |
| PyYAML | Detect and load mappings, serialize extracted specs | [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L65-L86] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L176-L181] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L194-L208] |
| Parent YAML utilities | Native null / Boolean spellings and master headers; fragment loading uses a different loader | [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L17-L59] [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L88-L115] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L167-L181] |
| Modular generator and ProjectInputSpec | Forward spec validation / writing delegated upstream; upstream subpackage has a separate pack | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L42-L55] [src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L163-L178] [docs/registry/module-routing.yaml:L317-L319] |
| CLI and legacy script wrapper | Main entry dispatches conversions; legacy wrapper delegates and returns only target_path | [src/digitalmodel/solvers/orcaflex/format_converter/__main__.py:L1-L7] [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L248-L257] [scripts/conversion/yaml_to_include.py:L27-L43] |
| OrcaFlex | Target text format; native load / statics proof remains separate, with no native execution in this pack | [docs/plans/2026-05-05-issue-280-orcaflex-format-converter-license-validation.md:L15-L49] |

Inventory linkage: the upstream generator is the separately routed orcaflex/modular_generator submodule; the converter is an engineering-data conversion route (MR). The external inventory's historical test count is not used as execution evidence. [docs/registry/module-routing.yaml:L317-L319] [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L42-L55]

### Every source mention of OrcaFlex

The following ledger includes every case-sensitive OrcaFlex source hit, including docstrings, comments, display text and serializer names. Each entry is a text-format dependency mention, not evidence that a solver was loaded; source files are read as text.

| Python file | Mention line citations at code pin |
| --- | --- |
| `src/digitalmodel/solvers/orcaflex/format_converter/__init__.py` | [src/digitalmodel/solvers/orcaflex/format_converter/__init__.py:L1-L1] [src/digitalmodel/solvers/orcaflex/format_converter/__init__.py:L6-L6] |
| `src/digitalmodel/solvers/orcaflex/format_converter/cli.py` | [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L1-L1] [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L165-L165] |
| `src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py` | [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L1-L1] [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L12-L12] [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L19-L19] [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L31-L31] [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L36-L36] [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L84-L84] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L1-L1] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L20-L20] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L118-L118] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py` | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L1-L1] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L3-L3] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L18-L18] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L48-L48] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L77-L77] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L89-L89] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L91-L91] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L159-L159] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L172-L172] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L196-L196] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L227-L227] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L241-L241] |
| `src/digitalmodel/solvers/orcaflex/format_converter/protocols.py` | [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L5-L5] |
| `src/digitalmodel/solvers/orcaflex/format_converter/section_mapping.py` | [src/digitalmodel/solvers/orcaflex/format_converter/section_mapping.py:L1-L1] [src/digitalmodel/solvers/orcaflex/format_converter/section_mapping.py:L4-L4] [src/digitalmodel/solvers/orcaflex/format_converter/section_mapping.py:L9-L9] [src/digitalmodel/solvers/orcaflex/format_converter/section_mapping.py:L44-L44] |
| `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L1-L1] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L4-L4] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L15-L15] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L21-L21] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L48-L48] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L192-L192] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L194-L194] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L273-L273] |
| `src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py` | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py:L1-L1] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py:L17-L17] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py:L32-L32] |
| `src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py` | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L1-L1] [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L14-L14] |
| `src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py` | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L1-L1] [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L17-L17] |

Mention count: 45 lines in 11 Python files, obtained by `rg -n 'OrcaFlex' src/digitalmodel/solvers/orcaflex/format_converter --glob '*.py'`; this ledger records the pin's text hits.

### Counted family, search procedure and result

Implementation-map source: `docs/registry/implementation-map.yaml` at data commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, whose declared code revision is `270c8759c374b67a7f032953e7fcdb14e0224461`; the map blob is byte-identical at the header code revision, so every citation also resolves at that single code revision. [docs/registry/implementation-map.yaml:L1-L4]

The only map family with an implementation path inside this package is `orcaflex-text-model-writers`: 7 in-package rows and 440 rows overall; it is therefore the only counted family. No nested converter subpackage is excluded. [docs/registry/implementation-map.yaml:L4933-L4943] [docs/registry/implementation-map.yaml:L7008-L7074]

Search uses all three map search_signals (algorithm, vocabulary and constants_or_keys) exactly as written with case-sensitive Python re, including their escaping. The reproducible procedure is: load this map revision with PyYAML; retain every signal; for every pinned package Python line run `re.search(pattern, line)`; assign the hit to the smallest enclosing AST function/class, otherwise its module-level definition. [docs/registry/implementation-map.yaml:L4933-L4943]

The full-signal search produced 39 hit definitions. The three vocabulary-only additions to the ledger below are cli.create_parser, format_detector.FormatType and the protocols module; they are parser/container definitions, not additional writers. All computing definitions are either listed by the map or named in section 3; other hits are containers, docstrings, serializers or delegates. Further second implementations not listed by either inventory: 0, within the acceptance limit of 1. The classification ledger follows, with code citations for inspection.

| Hit definition | Classification | Evidence at code pin |
| --- | --- | --- |
| `src/digitalmodel/solvers/orcaflex/format_converter/__init__.py` — `<module>` | Module/class container, imports or docstrings; helper | [src/digitalmodel/solvers/orcaflex/format_converter/__init__.py:L1-L29] |
| `src/digitalmodel/solvers/orcaflex/format_converter/cli.py` — `<module>` | Module/class container, imports or docstrings; helper | [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L1-L257] |
| `src/digitalmodel/solvers/orcaflex/format_converter/cli.py` — `cmd_spec2modular` | Delegate / wrapper; helper | [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L72-L77] |
| `src/digitalmodel/solvers/orcaflex/format_converter/cli.py` — `cmd_spec2single` | Delegate / wrapper; helper | [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L80-L85] |
| `src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py` — `<module>` | Module/class container, imports or docstrings; helper | [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L1-L91] |
| `src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py` — `detect_format` | Map-listed definition | [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L30-L91] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `<module>` | Module/class container, imports or docstrings; helper | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L1-L238] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter` | Module/class container, imports or docstrings; helper | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L19-L238] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter.convert` | Map-listed definition | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L29-L90] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter.merge_to_dict` | Section-3-listed method or its serializer / loader helper | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L95-L105] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter._merge_master` | Map-listed definition | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L107-L165] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter._load_include` | Section-3-listed method or its serializer / loader helper | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L167-L181] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter._extract_includefiles` | Section-3-listed method or its serializer / loader helper | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L184-L204] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter._extract_includefiles.traverse` | Map-listed definition | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L191-L200] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py` — `<module>` | Module/class container, imports or docstrings; helper | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L1-L305] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py` — `ModularToSpecConverter` | Module/class container, imports or docstrings; helper | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L88-L305] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py` — `ModularToSpecConverter.convert` | Map-listed definition | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L96-L222] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py` — `ModularToSpecConverter._populate_wave_trains` | Section-3-listed method or its serializer / loader helper | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L238-L297] |
| `src/digitalmodel/solvers/orcaflex/format_converter/section_mapping.py` — `<module>` | Module/class container, imports or docstrings; helper | [src/digitalmodel/solvers/orcaflex/format_converter/section_mapping.py:L1-L61] |
| `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` — `<module>` | Module/class container, imports or docstrings; helper | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L1-L277] |
| `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` — `SingleToModularConverter` | Module/class container, imports or docstrings; helper | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L47-L277] |
| `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` — `SingleToModularConverter.convert` | Section-3-listed method or its serializer / loader helper | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L69-L156] |
| `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` — `SingleToModularConverter._extract_input_parameters` | Section-3-listed method or its serializer / loader helper | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L161-L189] |
| `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` — `SingleToModularConverter._group_by_filename` | Section-3-listed method or its serializer / loader helper | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L191-L215] |
| `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` — `SingleToModularConverter._write_include_file` | Section-3-listed method or its serializer / loader helper | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L217-L228] |
| `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` — `SingleToModularConverter._write_master_file` | Map-listed definition | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L230-L260] |
| `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` — `SingleToModularConverter._write_parameters_file` | Map-listed definition | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L262-L277] |
| `src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py` — `<module>` | Module/class container, imports or docstrings; helper | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py:L1-L92] |
| `src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py` — `SingleToSpecConverter` | Module/class container, imports or docstrings; helper | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py:L16-L92] |
| `src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py` — `SingleToSpecConverter.convert` | Delegate / wrapper; helper | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py:L26-L89] |
| `src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py` — `<module>` | Module/class container, imports or docstrings; helper | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L1-L67] |
| `src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py` — `SpecToModularConverter` | Module/class container, imports or docstrings; helper | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L13-L67] |
| `src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py` — `SpecToModularConverter.convert` | Delegate / wrapper; helper | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L25-L64] |
| `src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py` — `<module>` | Module/class container, imports or docstrings; helper | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L1-L87] |
| `src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py` — `SpecToSingleConverter` | Module/class container, imports or docstrings; helper | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L16-L87] |
| `src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py` — `SpecToSingleConverter.convert` | Delegate / wrapper; helper | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L26-L84] |

### Complete map-family inventory

Every map implementation row is listed below in original family order, including rows outside the package. An ordinal plus a map citation identifies each original record without repeating external identifying paths; package rows also name their path and symbol. Relation and kind are the map's classifications, not independently attested equivalence. External naming reconciliation is issue pending (4); classification reconciliation is issue pending (3).

| Family row | Implementation identity | Map relation / kind | Map record at map pin |
| --- | --- | --- | --- |
| 1 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L4940-L4949] |
| 2 | External definition; identity in cited map record | independent / script | [docs/registry/implementation-map.yaml:L4950-L4958] |
| 3 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L4959-L4968] |
| 4 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L4969-L4978] |
| 5 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L4979-L4988] |
| 6 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L4989-L4998] |
| 7 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L4999-L5008] |
| 8 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5009-L5020] |
| 9 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5021-L5030] |
| 10 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L5031-L5040] |
| 11 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L5041-L5050] |
| 12 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L5051-L5060] |
| 13 | External definition; identity in cited map record | example / writer | [docs/registry/implementation-map.yaml:L5061-L5070] |
| 14 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5071-L5080] |
| 15 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5081-L5090] |
| 16 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5091-L5100] |
| 17 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5101-L5110] |
| 18 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5111-L5120] |
| 19 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5121-L5132] |
| 20 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5133-L5142] |
| 21 | External definition; identity in cited map record | independent / script | [docs/registry/implementation-map.yaml:L5143-L5151] |
| 22 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5152-L5161] |
| 23 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5162-L5171] |
| 24 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5172-L5181] |
| 25 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5182-L5191] |
| 26 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5192-L5201] |
| 27 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5202-L5211] |
| 28 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5212-L5223] |
| 29 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5224-L5233] |
| 30 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5234-L5243] |
| 31 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5244-L5253] |
| 32 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5254-L5263] |
| 33 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5264-L5273] |
| 34 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5274-L5283] |
| 35 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5284-L5293] |
| 36 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5294-L5303] |
| 37 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5304-L5313] |
| 38 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5314-L5323] |
| 39 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5324-L5333] |
| 40 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5334-L5343] |
| 41 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5344-L5353] |
| 42 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5354-L5363] |
| 43 | External definition; identity in cited map record | independent / script | [docs/registry/implementation-map.yaml:L5364-L5372] |
| 44 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5373-L5382] |
| 45 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5383-L5392] |
| 46 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5393-L5402] |
| 47 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5403-L5415] |
| 48 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5416-L5425] |
| 49 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5426-L5437] |
| 50 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5438-L5447] |
| 51 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5448-L5457] |
| 52 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5458-L5467] |
| 53 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5468-L5477] |
| 54 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5478-L5487] |
| 55 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5488-L5497] |
| 56 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5498-L5507] |
| 57 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5508-L5519] |
| 58 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5520-L5531] |
| 59 | External definition; identity in cited map record | independent / script | [docs/registry/implementation-map.yaml:L5532-L5540] |
| 60 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5541-L5550] |
| 61 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5551-L5560] |
| 62 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5561-L5570] |
| 63 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5571-L5580] |
| 64 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5581-L5593] |
| 65 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5594-L5603] |
| 66 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5604-L5615] |
| 67 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5616-L5628] |
| 68 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5629-L5638] |
| 69 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5639-L5648] |
| 70 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5649-L5658] |
| 71 | External definition; identity in cited map record | independent / script | [docs/registry/implementation-map.yaml:L5659-L5667] |
| 72 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5668-L5677] |
| 73 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5678-L5687] |
| 74 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5688-L5697] |
| 75 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5698-L5707] |
| 76 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5708-L5717] |
| 77 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5718-L5727] |
| 78 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5728-L5737] |
| 79 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5738-L5749] |
| 80 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5750-L5759] |
| 81 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5760-L5769] |
| 82 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5770-L5781] |
| 83 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5782-L5791] |
| 84 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5792-L5801] |
| 85 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5802-L5811] |
| 86 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5812-L5823] |
| 87 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5824-L5833] |
| 88 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5834-L5843] |
| 89 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5844-L5853] |
| 90 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5854-L5863] |
| 91 | External definition; identity in cited map record | independent / script | [docs/registry/implementation-map.yaml:L5864-L5872] |
| 92 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5873-L5882] |
| 93 | External definition; identity in cited map record | independent / script | [docs/registry/implementation-map.yaml:L5883-L5891] |
| 94 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5892-L5901] |
| 95 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5902-L5911] |
| 96 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5912-L5921] |
| 97 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5922-L5933] |
| 98 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5934-L5943] |
| 99 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5944-L5953] |
| 100 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5954-L5965] |
| 101 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5966-L5975] |
| 102 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5976-L5985] |
| 103 | External definition; identity in cited map record | independent / script | [docs/registry/implementation-map.yaml:L5986-L5994] |
| 104 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5995-L6004] |
| 105 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6005-L6014] |
| 106 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6015-L6024] |
| 107 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6025-L6034] |
| 108 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6035-L6044] |
| 109 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6045-L6054] |
| 110 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6055-L6064] |
| 111 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6065-L6074] |
| 112 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6075-L6084] |
| 113 | External definition; identity in cited map record | caller / writer | [docs/registry/implementation-map.yaml:L6085-L6096] |
| 114 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6097-L6109] |
| 115 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6110-L6119] |
| 116 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6120-L6129] |
| 117 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6130-L6141] |
| 118 | External definition; identity in cited map record | caller / writer | [docs/registry/implementation-map.yaml:L6142-L6153] |
| 119 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6154-L6163] |
| 120 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6164-L6173] |
| 121 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6174-L6183] |
| 122 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6184-L6193] |
| 123 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6194-L6203] |
| 124 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6204-L6213] |
| 125 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6214-L6223] |
| 126 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6224-L6233] |
| 127 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6234-L6243] |
| 128 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6244-L6253] |
| 129 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6254-L6263] |
| 130 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6264-L6273] |
| 131 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6274-L6283] |
| 132 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6284-L6293] |
| 133 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6294-L6303] |
| 134 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6304-L6313] |
| 135 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6314-L6323] |
| 136 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6324-L6333] |
| 137 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6334-L6343] |
| 138 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6344-L6353] |
| 139 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6354-L6363] |
| 140 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6364-L6373] |
| 141 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6374-L6383] |
| 142 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6384-L6393] |
| 143 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6394-L6403] |
| 144 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L6404-L6413] |
| 145 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6414-L6423] |
| 146 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6424-L6436] |
| 147 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6437-L6448] |
| 148 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6449-L6458] |
| 149 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6459-L6468] |
| 150 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6469-L6478] |
| 151 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6479-L6490] |
| 152 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6491-L6500] |
| 153 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6501-L6510] |
| 154 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6511-L6520] |
| 155 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6521-L6530] |
| 156 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6531-L6540] |
| 157 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6541-L6550] |
| 158 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6551-L6560] |
| 159 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6561-L6570] |
| 160 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6571-L6580] |
| 161 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6581-L6590] |
| 162 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L6591-L6600] |
| 163 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L6601-L6610] |
| 164 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6611-L6620] |
| 165 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6621-L6630] |
| 166 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L6631-L6640] |
| 167 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L6641-L6650] |
| 168 | External definition; identity in cited map record | independent / script | [docs/registry/implementation-map.yaml:L6651-L6659] |
| 169 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6660-L6672] |
| 170 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6673-L6682] |
| 171 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6683-L6692] |
| 172 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6693-L6704] |
| 173 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6705-L6714] |
| 174 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6715-L6724] |
| 175 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6725-L6734] |
| 176 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6735-L6744] |
| 177 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6745-L6754] |
| 178 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6755-L6764] |
| 179 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6765-L6774] |
| 180 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6775-L6784] |
| 181 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6785-L6794] |
| 182 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6795-L6804] |
| 183 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6805-L6814] |
| 184 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6815-L6824] |
| 185 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6825-L6834] |
| 186 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6835-L6844] |
| 187 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6845-L6854] |
| 188 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6855-L6864] |
| 189 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6865-L6874] |
| 190 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6875-L6884] |
| 191 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6885-L6894] |
| 192 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6895-L6904] |
| 193 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6905-L6914] |
| 194 | External definition; identity in cited map record | wrapper / writer | [docs/registry/implementation-map.yaml:L6915-L6927] |
| 195 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6928-L6937] |
| 196 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6938-L6947] |
| 197 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6948-L6957] |
| 198 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6958-L6967] |
| 199 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6968-L6977] |
| 200 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6978-L6987] |
| 201 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6988-L6997] |
| 202 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6998-L7007] |
| 203 | `src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py` — `detect_format` | independent / writer | [docs/registry/implementation-map.yaml:L7008-L7017] |
| 204 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter.convert` | independent / writer | [docs/registry/implementation-map.yaml:L7018-L7031] |
| 205 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter._merge_master` | independent / writer | [docs/registry/implementation-map.yaml:L7032-L7041] |
| 206 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter._extract_includefiles.traverse` | independent / writer | [docs/registry/implementation-map.yaml:L7042-L7051] |
| 207 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py` — `ModularToSpecConverter.convert` | independent / writer | [docs/registry/implementation-map.yaml:L7052-L7061] |
| 208 | `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` — `SingleToModularConverter._write_master_file` | independent / writer | [docs/registry/implementation-map.yaml:L7062-L7071] |
| 209 | `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` — `SingleToModularConverter._write_parameters_file` | independent / writer | [docs/registry/implementation-map.yaml:L7072-L7081] |
| 210 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7082-L7091] |
| 211 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7092-L7101] |
| 212 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7102-L7111] |
| 213 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7112-L7121] |
| 214 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7122-L7131] |
| 215 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7132-L7141] |
| 216 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7142-L7153] |
| 217 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7154-L7163] |
| 218 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7164-L7176] |
| 219 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7177-L7186] |
| 220 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7187-L7196] |
| 221 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7197-L7207] |
| 222 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7208-L7217] |
| 223 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7218-L7227] |
| 224 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7228-L7237] |
| 225 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7238-L7247] |
| 226 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7248-L7257] |
| 227 | External definition; identity in cited map record | caller / writer | [docs/registry/implementation-map.yaml:L7258-L7269] |
| 228 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7270-L7279] |
| 229 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7280-L7289] |
| 230 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7290-L7299] |
| 231 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7300-L7309] |
| 232 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7310-L7319] |
| 233 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7320-L7329] |
| 234 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7330-L7339] |
| 235 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7340-L7349] |
| 236 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7350-L7359] |
| 237 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7360-L7369] |
| 238 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7370-L7379] |
| 239 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7380-L7389] |
| 240 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7390-L7399] |
| 241 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7400-L7409] |
| 242 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7410-L7419] |
| 243 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7420-L7429] |
| 244 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7430-L7439] |
| 245 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7440-L7449] |
| 246 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7450-L7459] |
| 247 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7460-L7469] |
| 248 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7470-L7479] |
| 249 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7480-L7489] |
| 250 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7490-L7499] |
| 251 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7500-L7509] |
| 252 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7510-L7519] |
| 253 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7520-L7529] |
| 254 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7530-L7539] |
| 255 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7540-L7551] |
| 256 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7552-L7561] |
| 257 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7562-L7571] |
| 258 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7572-L7581] |
| 259 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7582-L7591] |
| 260 | External definition; identity in cited map record | wrapper / writer | [docs/registry/implementation-map.yaml:L7592-L7603] |
| 261 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7604-L7613] |
| 262 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7614-L7626] |
| 263 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7627-L7636] |
| 264 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7637-L7646] |
| 265 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7647-L7656] |
| 266 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7657-L7666] |
| 267 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7667-L7676] |
| 268 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7677-L7686] |
| 269 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7687-L7696] |
| 270 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7697-L7706] |
| 271 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7707-L7716] |
| 272 | External definition; identity in cited map record | wrapper / writer | [docs/registry/implementation-map.yaml:L7717-L7729] |
| 273 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7730-L7739] |
| 274 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7740-L7749] |
| 275 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7750-L7762] |
| 276 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L7763-L7774] |
| 277 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L7775-L7784] |
| 278 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L7785-L7794] |
| 279 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7795-L7804] |
| 280 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7805-L7816] |
| 281 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7817-L7826] |
| 282 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7827-L7836] |
| 283 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7837-L7846] |
| 284 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7847-L7856] |
| 285 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7857-L7866] |
| 286 | External definition; identity in cited map record | caller / writer | [docs/registry/implementation-map.yaml:L7867-L7878] |
| 287 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7879-L7888] |
| 288 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7889-L7898] |
| 289 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7899-L7908] |
| 290 | External definition; identity in cited map record | wrapper / writer | [docs/registry/implementation-map.yaml:L7909-L7921] |
| 291 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7922-L7931] |
| 292 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7932-L7941] |
| 293 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7942-L7951] |
| 294 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7952-L7961] |
| 295 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7962-L7971] |
| 296 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7972-L7981] |
| 297 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7982-L7991] |
| 298 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7992-L8001] |
| 299 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8002-L8011] |
| 300 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8012-L8021] |
| 301 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8022-L8031] |
| 302 | External definition; identity in cited map record | caller / writer | [docs/registry/implementation-map.yaml:L8032-L8043] |
| 303 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8044-L8053] |
| 304 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8054-L8063] |
| 305 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8064-L8073] |
| 306 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8074-L8083] |
| 307 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8084-L8093] |
| 308 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L8094-L8103] |
| 309 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8104-L8113] |
| 310 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8114-L8123] |
| 311 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8124-L8133] |
| 312 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8134-L8143] |
| 313 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8144-L8155] |
| 314 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8156-L8165] |
| 315 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8166-L8175] |
| 316 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8176-L8185] |
| 317 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8186-L8195] |
| 318 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8196-L8205] |
| 319 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8206-L8215] |
| 320 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8216-L8227] |
| 321 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8228-L8237] |
| 322 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8238-L8247] |
| 323 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8248-L8257] |
| 324 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8258-L8267] |
| 325 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8268-L8279] |
| 326 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8280-L8290] |
| 327 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8291-L8300] |
| 328 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8301-L8310] |
| 329 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8311-L8323] |
| 330 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8324-L8333] |
| 331 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8334-L8343] |
| 332 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8344-L8353] |
| 333 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8354-L8363] |
| 334 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8364-L8373] |
| 335 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L8374-L8383] |
| 336 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L8384-L8393] |
| 337 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8394-L8403] |
| 338 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8404-L8413] |
| 339 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8414-L8423] |
| 340 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8424-L8435] |
| 341 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8436-L8445] |
| 342 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8446-L8457] |
| 343 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8458-L8470] |
| 344 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8471-L8480] |
| 345 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8481-L8492] |
| 346 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8493-L8505] |
| 347 | External definition; identity in cited map record | wrapper / writer | [docs/registry/implementation-map.yaml:L8506-L8517] |
| 348 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8518-L8527] |
| 349 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8528-L8537] |
| 350 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8538-L8547] |
| 351 | External definition; identity in cited map record | caller / writer | [docs/registry/implementation-map.yaml:L8548-L8560] |
| 352 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8561-L8572] |
| 353 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8573-L8582] |
| 354 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8583-L8596] |
| 355 | External definition; identity in cited map record | wrapper / writer | [docs/registry/implementation-map.yaml:L8597-L8608] |
| 356 | External definition; identity in cited map record | wrapper / writer | [docs/registry/implementation-map.yaml:L8609-L8619] |
| 357 | External definition; identity in cited map record | caller / writer | [docs/registry/implementation-map.yaml:L8620-L8632] |
| 358 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8633-L8647] |
| 359 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8648-L8657] |
| 360 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8658-L8667] |
| 361 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8668-L8677] |
| 362 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8678-L8687] |
| 363 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8688-L8701] |
| 364 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8702-L8714] |
| 365 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8715-L8724] |
| 366 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8725-L8739] |
| 367 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8740-L8749] |
| 368 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8750-L8759] |
| 369 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8760-L8773] |
| 370 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8774-L8783] |
| 371 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8784-L8793] |
| 372 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8794-L8803] |
| 373 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8804-L8813] |
| 374 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8814-L8823] |
| 375 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8824-L8833] |
| 376 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8834-L8843] |
| 377 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8844-L8856] |
| 378 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8857-L8866] |
| 379 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8867-L8876] |
| 380 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8877-L8886] |
| 381 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8887-L8896] |
| 382 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8897-L8908] |
| 383 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8909-L8918] |
| 384 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8919-L8928] |
| 385 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8929-L8938] |
| 386 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8939-L8948] |
| 387 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8949-L8958] |
| 388 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8959-L8970] |
| 389 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8971-L8980] |
| 390 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8981-L8990] |
| 391 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8991-L9000] |
| 392 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9001-L9010] |
| 393 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9011-L9020] |
| 394 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9021-L9030] |
| 395 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9031-L9040] |
| 396 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9041-L9050] |
| 397 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9051-L9060] |
| 398 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9061-L9070] |
| 399 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9071-L9080] |
| 400 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9081-L9090] |
| 401 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9091-L9100] |
| 402 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9101-L9110] |
| 403 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9111-L9120] |
| 404 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9121-L9130] |
| 405 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9131-L9140] |
| 406 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9141-L9152] |
| 407 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9153-L9162] |
| 408 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9163-L9172] |
| 409 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9173-L9182] |
| 410 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9183-L9192] |
| 411 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9193-L9202] |
| 412 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9203-L9212] |
| 413 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9213-L9222] |
| 414 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9223-L9232] |
| 415 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9233-L9242] |
| 416 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9243-L9252] |
| 417 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9253-L9262] |
| 418 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9263-L9272] |
| 419 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9273-L9282] |
| 420 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9283-L9292] |
| 421 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9293-L9302] |
| 422 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9303-L9312] |
| 423 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9313-L9322] |
| 424 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9323-L9332] |
| 425 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9333-L9342] |
| 426 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9343-L9352] |
| 427 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9353-L9362] |
| 428 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9363-L9372] |
| 429 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9373-L9382] |
| 430 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9383-L9392] |
| 431 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9393-L9402] |
| 432 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9403-L9412] |
| 433 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9413-L9422] |
| 434 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9423-L9432] |
| 435 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9433-L9442] |
| 436 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9443-L9452] |
| 437 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9453-L9462] |
| 438 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9463-L9472] |
| 439 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9473-L9482] |
| 440 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9483-L9493] |

### Repository-wide variants and map follow-up

The repository-wide search included scripts and source, using `rg -l 'class .*Converter|def.*(convert|merge)|EXTRACTION_MAP|SECTION_MAPPING' scripts src/digitalmodel --glob '*.py'`, then `rg -n 'SingleToModularConverter|ModularToSingleConverter|ModularToSpecConverter|SECTION_MAPPING' scripts --glob '*.py'`; matches were inspected as text rather than imported.

| Other route | Classification / difference | Evidence |
| --- | --- | --- |
| Legacy conversion script | Wrapper around the listed SingleToModularConverter; not another algorithm | [scripts/conversion/yaml_to_include.py:L27-L43] |
| Physical validator merge | Own include loading and deep merge, including list concatenation; outside this package | [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L192-L265] |
| Validation utility merge | Own dictionary recursion and override behavior; outside this package | [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L201-L220] |
| MonolithicExtractor | Own native-YAML-to-spec algorithm, with generic model output and multidocument loading; outside this package | [src/digitalmodel/solvers/orcaflex/modular_generator/extractor.py:L109-L165] |

Map follow-up: reconcile writer/detector/helper classifications and cross-package merge/extraction variants, issue pending (3). No additional unmapped method family is asserted from keyword similarity alone; the package's own section-3 methods are explicitly named above. A further repository search finding is a map follow-up, not an extra in-package miss.

### Supplemental audit with two retained signals

Separately from the full three-signal audit above, the branch audit used Python `re` case-sensitively on every package `.py` file at the code pin, using the map's exact `search_signals`; the vocabulary pattern was excluded because it matches the package path case-insensitively. The two retained patterns were not unescaped or normalised. [docs/registry/implementation-map.yaml:L4936-L4938]
The audit produced 150 line/signal hits assigned to 36 enclosing definitions or module-level groups; the classifications below account for every group, with the transformation algorithms also enumerated in section 3.

| Definition group | Classification / rationale | Code evidence |
| --- | --- | --- |
| `__init__.py: <module>` | Module documentation, imports or data tables; no further implementation. | [src/digitalmodel/solvers/orcaflex/format_converter/__init__.py:L4-L6] |
| `cli.py: <module>` | Module documentation, imports or data tables; no further implementation. | [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L6-L14] |
| `cli.py: cmd_spec2modular` | Wrapper, class container, delegate or serialisation helper of a listed route; section 3 identifies the route. | [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L73-L73] |
| `cli.py: cmd_spec2single` | Wrapper, class container, delegate or serialisation helper of a listed route; section 3 identifies the route. | [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L81-L81] |
| `format_detector.py: <module>` | Module documentation, imports or data tables; no further implementation. | [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L21-L22] |
| `format_detector.py: detect_format` | Listed in map inventory; method or serialiser also recorded in section 3. | [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L34-L72] |
| `modular_to_single.py: <module>` | Module documentation, imports or data tables; no further implementation. | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L15-L15] |
| `modular_to_single.py: ModularToSingleConverter` | Wrapper, class container, delegate or serialisation helper of a listed route; section 3 identifies the route. | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L20-L22] |
| `modular_to_single.py: _extract_includefiles` | Transformation helper explicitly inventoried in section 3. | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L187-L187] |
| `modular_to_single.py: _load_include` | Transformation helper explicitly inventoried in section 3. | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L176-L176] |
| `modular_to_single.py: _merge_master` | Listed in map inventory; method or serialiser also recorded in section 3. | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L113-L152] |
| `modular_to_single.py: convert` | Listed in map inventory; method or serialiser also recorded in section 3. | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L35-L81] |
| `modular_to_single.py: merge_to_dict` | Wrapper, class container, delegate or serialisation helper of a listed route; section 3 identifies the route. | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L99-L99] |
| `modular_to_single.py: traverse` | Listed in map inventory; method or serialiser also recorded in section 3. | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L194-L194] |
| `modular_to_spec.py: <module>` | Module documentation, imports or data tables; no further implementation. | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L1-L83] |
| `modular_to_spec.py: ModularToSpecConverter` | Wrapper, class container, delegate or serialisation helper of a listed route; section 3 identifies the route. | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L89-L89] |
| `modular_to_spec.py: _populate_wave_trains` | Transformation helper explicitly inventoried in section 3. | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L241-L252] |
| `modular_to_spec.py: convert` | Listed in map inventory; method or serialiser also recorded in section 3. | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L102-L208] |
| `section_mapping.py: <module>` | Module documentation, imports or data tables; no further implementation. | [src/digitalmodel/solvers/orcaflex/format_converter/section_mapping.py:L12-L57] |
| `single_to_modular.py: <module>` | Module documentation, imports or data tables; no further implementation. | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L15-L44] |
| `single_to_modular.py: SingleToModularConverter` | Wrapper, class container, delegate or serialisation helper of a listed route; section 3 identifies the route. | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L52-L58] |
| `single_to_modular.py: _extract_input_parameters` | Transformation helper explicitly inventoried in section 3. | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L176-L180] |
| `single_to_modular.py: _group_by_filename` | Transformation helper explicitly inventoried in section 3. | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L195-L207] |
| `single_to_modular.py: _write_include_file` | Wrapper, class container, delegate or serialisation helper of a listed route; section 3 identifies the route. | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L228-L228] |
| `single_to_modular.py: _write_master_file` | Listed in map inventory; method or serialiser also recorded in section 3. | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L237-L260] |
| `single_to_modular.py: _write_parameters_file` | Listed in map inventory; method or serialiser also recorded in section 3. | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L265-L277] |
| `single_to_modular.py: convert` | Wrapper, class container, delegate or serialisation helper of a listed route; section 3 identifies the route. | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L133-L144] |
| `single_to_spec.py: <module>` | Module documentation, imports or data tables; no further implementation. | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py:L1-L1] |
| `single_to_spec.py: SingleToSpecConverter` | Wrapper, class container, delegate or serialisation helper of a listed route; section 3 identifies the route. | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py:L17-L17] |
| `single_to_spec.py: convert` | Wrapper, class container, delegate or serialisation helper of a listed route; section 3 identifies the route. | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py:L33-L63] |
| `spec_to_modular.py: <module>` | Module documentation, imports or data tables; no further implementation. | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L1-L3] |
| `spec_to_modular.py: SpecToModularConverter` | Wrapper, class container, delegate or serialisation helper of a listed route; section 3 identifies the route. | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L14-L19] |
| `spec_to_modular.py: convert` | Wrapper, class container, delegate or serialisation helper of a listed route; section 3 identifies the route. | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L31-L45] |
| `spec_to_single.py: <module>` | Module documentation, imports or data tables; no further implementation. | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L1-L1] |
| `spec_to_single.py: SpecToSingleConverter` | Wrapper, class container, delegate or serialisation helper of a listed route; section 3 identifies the route. | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L17-L17] |
| `spec_to_single.py: convert` | Wrapper, class container, delegate or serialisation helper of a listed route; section 3 identifies the route. | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L32-L63] |

Further unlisted in-package implementations after sections 3 and 9: 0, against the A8 limit of at most 1; map labels are retained as inventory claims, not independently endorsed classifications. The detector row, for example, recognises input format rather than writing a model. [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L30-L91]
The exact retained-signal repository-wide search inspected 5,650 tracked Python files and matched 13,902 lines in 2,279 files, including 412 files under `scripts/`; these are discovery hits, not qualified independent implementations. The repository-wide discovery search includes `scripts/`: related code includes the legacy wrapper, shared YAML loader/dumper and modular-input validation merge logic. These are map follow-up candidates outside this package and do not increase its A8 miss count. [scripts/conversion/yaml_to_include.py:L1-L45] [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L63-L115] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L1-L6]
Map follow-up: classify format detection, include merging, parameter projection and reverse specification extraction more precisely than the broad writer family; issue pending (3).

### Supplemental solver-name and interface locations (text only)

| Package file | Lines naming the solver or its interface | Evidence |
| --- | --- | --- |
| `__init__.py` | 1, 6 | [src/digitalmodel/solvers/orcaflex/format_converter/__init__.py:L1-L6] |
| `cli.py` | 1, 14, 15, 164, 165 | [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L1-L165] |
| `format_detector.py` | 1, 12, 19, 20, 31, 36, 84, 85 | [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L1-L85] |
| `modular_to_single.py` | 1, 15, 20, 68, 81, 104, 118 | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L1-L118] |
| `modular_to_spec.py` | 1, 3, 18, 19, 48, 77, 89, 91, 159, 172, 196, 227, 241 | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L1-L241] |
| `protocols.py` | 5 | [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L5-L5] |
| `section_mapping.py` | 1, 4, 9, 44 | [src/digitalmodel/solvers/orcaflex/format_converter/section_mapping.py:L1-L44] |
| `single_to_modular.py` | 1, 4, 15, 21, 48, 94, 192, 194, 228, 273 | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L1-L273] |
| `single_to_spec.py` | 1, 17, 32 | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py:L1-L32] |
| `spec_to_modular.py` | 1, 14 | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L1-L14] |
| `spec_to_single.py` | 1, 17 | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L1-L17] |

No target solver version is fixed by the converter protocol; preserving a source header does not establish loadability. The legacy wrapper's importability is not established by the converter tests. [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L15-L30] [scripts/conversion/yaml_to_include.py:L1-L45]

## 10. Coverage and open items

### Files read

Every original package Python file was read at the code pin; no nested source package or local README/doc file was found in the package tree before this pack was added. The following table records those files and their full pinned extents.

| Package source read | Pinned lines |
| --- | --- |
| `src/digitalmodel/solvers/orcaflex/format_converter/__init__.py` | [src/digitalmodel/solvers/orcaflex/format_converter/__init__.py:L1-L29] |
| `src/digitalmodel/solvers/orcaflex/format_converter/__main__.py` | [src/digitalmodel/solvers/orcaflex/format_converter/__main__.py:L1-L7] |
| `src/digitalmodel/solvers/orcaflex/format_converter/cli.py` | [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L1-L257] |
| `src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py` | [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L1-L91] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L1-L238] |
| `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py` | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L1-L305] |
| `src/digitalmodel/solvers/orcaflex/format_converter/protocols.py` | [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L1-L61] |
| `src/digitalmodel/solvers/orcaflex/format_converter/section_mapping.py` | [src/digitalmodel/solvers/orcaflex/format_converter/section_mapping.py:L1-L61] |
| `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L1-L277] |
| `src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py` | [src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py:L1-L92] |
| `src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py` | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L1-L67] |
| `src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py` | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L1-L87] |

The documentation search used `rg -l -i 'format[_ -]converter|three.way.*convert' docs --glob '*.md'`; seven matching discovery files were read. One discovery file with identifying names is represented by count only; naming follow-up is issue pending (4).

| Public discovery document | Relevant pinned evidence |
| --- | --- |
| Conversion quick start | [docs/domains/orcaflex/QUICKSTART_FILE_CONVERSION.md:L286-L344] |
| Native converter validation plan | [docs/plans/2026-05-05-issue-280-orcaflex-format-converter-license-validation.md:L7-L11] |
| Semantic-equivalence boundary | [docs/domains/orcaflex/SEMANTIC_EQUIVALENCE_CLAIM_BOUNDARY.md:L13-L30] |
| Semantic-diff taxonomy | [docs/domains/orcaflex/SEMANTIC_DIFF_TAXONOMY.md:L9-L35] |
| CAD automation surface | [docs/cad-inventory/ecosystem-and-automation-surface.md:L77-L80] |
| Blender automation summary; incidental file-format-converter phrase | [docs/domains/blender/BLENDER_AUTOMATION_MODULE.md:L24-L31] |

Additional readings: the parent YAML utilities, generator delegation and schema fields, legacy script wrapper, related generator fidelity tests, routing registry and existing parent/generator packs were inspected for boundaries; neither private source data nor licensed-standard text was read. [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L17-L115] [src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L10-L25] [src/digitalmodel/solvers/orcaflex/modular_generator/schema/environment.py:L74-L100] [src/digitalmodel/solvers/orcaflex/modular_generator/schema/simulation.py:L8-L36]

### Open items and limitations

| Item | Responsible review role | Issue / draft | Evidence / required action |
| --- | --- | --- | --- |
| Supported versions and physical applicability, the UNVERIFIED statement in section 1 | Riser/mooring quality manager | #280 | Native load/statics/binary proof; [docs/plans/2026-05-05-issue-280-orcaflex-format-converter-license-validation.md:L15-L49] |
| Universal units / frames / signs / phase, the UNVERIFIED statement in section 2 | Converter maintainers | #520 | Preserve input convention provenance or reject unsupported conventions; [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L20-L43] |
| No independent validated example; broader library fidelity not established | Riser/mooring quality manager | #518 / #2014 | Add independent physical reference and restore real-file coverage; [tests/solvers/orcaflex/format_converter/test_round_trip.py:L19-L92] |
| Stale fixture roots; 35 cases SKIP in the unmodified suite | Test maintainers | #2008 | Repoint docs/modules roots to docs/domains; data is present under docs/domains/orcaflex/examples/raw/A01/, docs/domains/orcaflex/examples/modular/A01/ and docs/domains/orcaflex/pipeline/installation/floating/30in_pipeline/spec.yml; [tests/solvers/orcaflex/format_converter/conftest.py:L9-L55] |
| Partial reverse mapping, confidence denominator docs and absent depth alias / field-level warnings | Converter maintainers | #520 | Section-6 rows identify individual gaps; [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L20-L46] |
| Missing/empty/wrong-type/nested include handling and scalar-loader differences | Converter maintainers | issue pending (1) | Add negative fixtures and warning propagation; [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L95-L181] |
| Unsupported dry-run / batch flags | Converter maintainers | issue pending (2) | Reject or implement before conversion; [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L169-L182] |
| Implementation-map follow-up | Map maintainers | issue pending (3) | Reconcile section-3 definitions and cross-package variants; section-9 inventory and audit |
| Identifying discovery references | Documentation maintainers | issue pending (4) | Codename discovery references without copying identities into the pack |
| Complete evidence appendix and provenance adapter absent | Riser/mooring quality manager | issue pending (5) | Implement check-14 evidence linkage; [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L15-L30] |
| Full mapped contracts-domain invocation not run because other packs collect licensed-binding imports | Test maintainers | #280 | Run the full domain where native import is permitted; [tests/docs/test_agent_pack_node_ids.py:L18-L38] [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L25-L36] |
| Duplicate submodules keys from the earlier registry | Registry maintainers | Resolved during merge | All routes retained in one mapping in docs/registry/module-routing.yaml |

The calculation-citation contract was readable; no standards-derived coefficient was introduced. Pack-native formulas in section 3 are field counts or serialization algorithms, not licensed-standard calculations. No private sidecar entry is required for this public synthetic evidence. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L189-L192]

### Retained qualification items

| Item | Remaining qualification / responsible discipline | Tracking |
| --- | --- | --- |
| O1 | UNVERIFIED target versions, loadability, statics and dynamics; riser and mooring quality manager; execute C3, converter-route statics parity check-11a and downstream C4–C5 / check-11b with an independently qualified model; C4–C5 alone do not establish conversion fidelity. | #280; regression corpus #2014 and #518 |
| O2 | UNVERIFIED units, coordinate basis, independent benchmark and governing standard / Citation applicability; engineering software and family quality manager; validated examples remain 0. | issue pending (7) |
| O3 | UNVERIFIED assigned deputy and availability; maintainers; approve the proposed review roles and shared-log destination. | issue pending (7) |
| O4 | Stale fixture path after the documentation move and all resulting SKIP examples; engineering software; correct the path and run the 35 nodes below (no solver required). | #2008 |
| O5 | Partial merges, hidden omissions and incomplete negative detection tests sweep-01–sweep-06, sweep-30 and sweep-32; engineering software; implement and run negative tests, without changing this pack's code pin. | issue pending (1) |
| O6 | Reverse extraction boundaries sweep-07–sweep-24; engineering software; qualify missing fields and multiple trains before any equivalence claim. | #520 |
| O7 | Accepted but unused CLI dry-run/batch flags; engineering software; define their behaviour and test absence of writes for dry-run. | issue pending (2) |
| O8 | Broad writer inventory classifications, repository-wide gaps and identifying discovery names; map maintainers; resolve codenames before expanding source inventories. | issue pending (3) |
| O9 | Registry mapping consolidation: resolved during the merge; all routes from both branches are retained in one submodules mapping. | Resolved in docs/registry/module-routing.yaml |

### Complete test-node ledger

The command form is `PYTHONPATH=src uv run --no-sources python -m pytest tests/solvers/orcaflex/format_converter tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py -q -rA -p no:cacheprovider --basetemp=<scratch>`. The configured uv executable and external scratch location were used; no addopts override or worker option was added. The documentation fix leaves the pinned converter code unchanged. The table records results with the shipped stale fixture roots; the separate relocated-data run in section 4 does not overwrite those baseline observations.

| Existing test node ID | BASE result | Definition at code pin |
| --- | --- | --- |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_convert_creates_output_file` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L16-L38] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_merged_output_contains_all_sections` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L40-L69] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_deep_merge_dicts` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L71-L76] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_deep_merge_lists_concatenate` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L78-L83] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_deep_merge_override_non_dict` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L85-L90] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_deep_merge_adds_new_keys` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L92-L97] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_missing_include_warns` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L99-L112] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_convert_real_a01_modular` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L114-L129] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_convert_from_directory` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L131-L147] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_merge_to_dict` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L149-L164] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_report_format` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L166-L183] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_header_preserved` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L185-L209] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_missing_master_returns_failure` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L211-L220] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_no_source_raises_error` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L222-L226] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_dict_master_preserves_content_and_section_scope` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L228-L269] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_dict_master_section_include_wrapped_fragment` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L271-L289] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_dict_master_missing_section_include_warns` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L291-L314] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_modular_directory` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L17-L19] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_modular_master_file` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L21-L23] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_single_file` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L25-L27] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_spec_file` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L29-L31] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_single_from_dict` | PASS | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L33-L39] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_spec_from_dict` | PASS | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L41-L51] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_modular_from_list` | PASS | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L53-L62] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_modular_directory_structure` | PASS | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L64-L69] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_error_on_nonexistent_path` | PASS | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L71-L73] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_error_on_empty_file` | PASS | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L75-L79] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_error_on_unknown_dict_format` | PASS | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L81-L87] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_error_on_directory_without_master` | PASS | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L89-L92] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_format_type_values` | PASS | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L94-L98] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIDetect::test_detect_single_file` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L17-L20] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIDetect::test_detect_modular_dir` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L22-L25] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIDetect::test_detect_spec_file` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L27-L30] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIDetect::test_detect_nonexistent` | PASS | [tests/solvers/orcaflex/format_converter/test_cli.py:L32-L35] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLISingle2Modular::test_convert` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L41-L46] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLISingle2Modular::test_no_output_uses_default` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L48-L55] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIModular2Single::test_convert` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L61-L66] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLISpec2Modular::test_convert` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L72-L77] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLISingle2Spec::test_convert` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L83-L88] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIAuto::test_auto_single_to_modular` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L94-L100] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIAuto::test_auto_modular_to_single` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L102-L108] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIParser::test_parser_creates_subcommands` | PASS | [tests/solvers/orcaflex/format_converter/test_cli.py:L114-L118] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIParser::test_no_command_returns_1` | PASS | [tests/solvers/orcaflex/format_converter/test_cli.py:L120-L123] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIReporting::test_print_report_surfaces_expected_and_actionable_gaps` | PASS | [tests/solvers/orcaflex/format_converter/test_cli.py:L129-L149] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_convert_creates_directory_structure` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L16-L31] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_convert_splits_sections_to_files` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L33-L48] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_include_file_contains_section_data` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L50-L66] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_master_file_has_include_directives` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L68-L82] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_parameters_extracted` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L84-L101] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_convert_real_a01_file` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L103-L112] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_report_format` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L114-L127] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_empty_file_returns_failure` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L129-L138] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_header_preserved_in_master` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L140-L158] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_multiple_sections_same_file` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L160-L185] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_unknown_sections_quarantined_last` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L187-L225] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_real_sections_ordered_after_referenced_objects` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L227-L278] |
| `tests/solvers/orcaflex/format_converter/test_round_trip.py::TestRoundTrip::test_roundtrip_synthetic_data` | PASS | [tests/solvers/orcaflex/format_converter/test_round_trip.py:L19-L54] |
| `tests/solvers/orcaflex/format_converter/test_round_trip.py::TestRoundTrip::test_roundtrip_real_a01` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_round_trip.py:L56-L90] |
| `tests/solvers/orcaflex/format_converter/test_round_trip.py::TestRoundTrip::test_roundtrip_with_multiple_buoy_sections` | PASS | [tests/solvers/orcaflex/format_converter/test_round_trip.py:L92-L126] |
| `tests/solvers/orcaflex/format_converter/test_round_trip.py::TestRoundTrip::test_roundtrip_preserves_nested_data` | PASS | [tests/solvers/orcaflex/format_converter/test_round_trip.py:L128-L164] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexDumper::test_none_represented_as_tilde` | PASS | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L18-L21] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexDumper::test_true_represented_as_yes` | PASS | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L23-L26] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexDumper::test_false_represented_as_no` | PASS | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L28-L31] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexDumper::test_strings_preserved` | PASS | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L33-L36] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexDumper::test_numbers_preserved` | PASS | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L38-L41] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexDump::test_creates_file` | PASS | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L47-L53] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexDump::test_creates_parent_directories` | PASS | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L55-L59] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexDump::test_with_header` | PASS | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L61-L69] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexDump::test_header_with_separator` | PASS | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L71-L78] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexDump::test_none_values_as_tilde` | PASS | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L80-L85] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexDump::test_bool_values_as_yes_no` | PASS | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L87-L93] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexDump::test_sort_keys_false` | PASS | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L95-L103] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexLoad::test_load_simple_file` | PASS | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L109-L115] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexLoad::test_load_with_yaml_header` | PASS | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L117-L125] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexLoad::test_load_preserves_header_comments` | PASS | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L127-L140] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexLoad::test_load_empty_file` | PASS | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L142-L146] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexLoad::test_load_real_a01_file` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L148-L154] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexLoad::test_roundtrip_dump_load` | PASS | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L156-L173] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_convert_creates_modular_structure` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L25-L33] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_convert_generates_includes` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L35-L42] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_convert_invalid_spec_returns_failure` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L44-L54] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_report_metadata` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L56-L63] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_no_source_raises` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L65-L69] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_init_with_spec_path` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L71-L77] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_supported_formats` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L79-L82] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToSingleConverter::test_convert_creates_single_file` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L88-L95] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToSingleConverter::test_output_has_orcaflex_sections` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L97-L108] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToSingleConverter::test_report_metadata` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L110-L117] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToSingleConverter::test_no_source_raises` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L119-L123] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToSingleConverter::test_supported_formats` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L125-L128] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToSingleConverter::test_invalid_spec_propagates_failure` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L130-L141] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_extract_environment_fields` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L147-L161] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_extract_simulation_fields` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L163-L173] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_confidence_score` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L175-L182] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_unmapped_sections_reported` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L184-L191] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_metadata_placeholder` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L193-L203] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_confidence_header_in_file` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L205-L213] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_from_directory` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L215-L221] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_extract_wave_fields` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L223-L259] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_extract_single_spectral_train_keeps_hs_tz` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L261-L310] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_no_source_raises` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L312-L316] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_supported_formats` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L318-L321] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_empty_modular_returns_failure` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L323-L334] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecBoundaryReporting::test_is_best_effort_true` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L377-L383] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecBoundaryReporting::test_summary_includes_best_effort_label` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L385-L391] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecBoundaryReporting::test_expected_gaps_classified` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L393-L409] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecBoundaryReporting::test_actionable_gaps_classified` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L411-L425] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecBoundaryReporting::test_unknown_sections_classified_as_actionable` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L427-L434] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecBoundaryReporting::test_all_unmapped_are_categorized` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L436-L450] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecBoundaryReporting::test_file_header_not_lossless_warning` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L452-L458] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSingleToSpecBoundaryReporting::test_is_best_effort_true` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L464-L470] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSingleToSpecConverter::test_convert_real_a01` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L476-L484] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSingleToSpecConverter::test_extracted_spec_has_environment` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L486-L498] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSingleToSpecConverter::test_report_metadata` | SKIP: stale fixture root; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L500-L508] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSingleToSpecConverter::test_no_source_raises` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L510-L514] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSingleToSpecConverter::test_supported_formats` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L516-L519] |
| `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestShadowedPropertiesWarning::test_warns_on_shadowed_category` | PASS | [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L112-L132] |
| `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestShadowedPropertiesWarning::test_warns_on_shadowed_od` | PASS | [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L134-L149] |
| `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestShadowedPropertiesWarning::test_no_warning_when_no_conflict` | PASS | [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L151-L167] |
| `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesLineTypes::test_line_type_names_preserved` | PASS | [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L182-L194] |
| `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesLineTypes::test_line_type_od_values_match` | PASS | [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L196-L210] |
| `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesLineTypes::test_line_type_ea_values_match` | PASS | [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L212-L226] |
| `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesLineTypes::test_line_type_mass_values_match` | PASS | [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L228-L243] |
| `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesLineTypes::test_properties_bag_values_emitted` | PASS | [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L245-L264] |
| `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesEnvironment::test_water_depth_matches` | PASS | [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L274-L286] |
| `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesEnvironment::test_wave_type_matches` | PASS | [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L288-L304] |
| `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesEnvironment::test_wave_height_matches` | PASS | [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L306-L318] |
| `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesVessels::test_vessel_names_preserved` | PASS | [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L328-L343] |
| `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripPreservesVessels::test_vessel_type_names_preserved` | PASS | [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L345-L356] |
| `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripExtraction::test_extracted_spec_water_depth_matches` | PASS | [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L366-L418] |
| `tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py::TestRoundtripExtraction::test_extracted_spec_wave_height_matches` | PASS | [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L420-L442] |

Execution comparison repeated on 2026-10-17: BASE and AFTER each collected 131 cases, ran and passed 96, failed 0 and skipped 35; the same 35 stale-root cases are SKIP in the ledger, not an additional 35 cases. All 96 passing node IDs matched between runs. [tests/solvers/orcaflex/format_converter/conftest.py:L9-L55]

The current combined converter/fidelity and contract/routing invocation uses `PYTHONPATH=src uv run --no-sources python -m pytest tests/solvers/orcaflex/format_converter tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py tests/contracts tests/docs/test_digitalmodel_routing_contract.py -q -rA -p no:cacheprovider --basetemp=<scratch>`. On 2026-10-18, BASE and AFTER each collected 628 cases: 593 passed, 35 skipped and 0 failed, with identical passing node IDs. The contract/routing subset contributed 497 passes; converter/fidelity contributed 96 passes and 35 skips. The earlier 625-case total is superseded by the current test inventory; the converter source remains at the code pin. [tests/docs/test_digitalmodel_routing_contract.py:L94-L110]

Three contract cases were added after the code pin: `tests/contracts/test_force_units_boundary_contract.py::test_suffix_vocabulary_accepts_each_unit_class[dimensionless-most_loaded_share]`, `tests/contracts/test_force_units_boundary_contract.py::test_scanner_accepts_imperial_and_dimensionless_names[most_loaded_share]` and `tests/contracts/test_runtime_dependencies.py::test_known_undeclared_list_excludes_declared_dependencies`. These current-checkout nodes explain the difference from the earlier total.

The full contracts domain also includes pack-node collection that can import licensed bindings through other packs' native test files; that broader invocation was NOT_RUN under the solver prohibition. The contract directory and explicit routing file above were run without excluding tests from either path. [tests/DOMAINS.md:L13-L13] [tests/docs/test_agent_pack_node_ids.py:L18-L38] [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L25-L36]
