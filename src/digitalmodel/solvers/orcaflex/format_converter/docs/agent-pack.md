# Format converter knowledge pack

| Header | Value |
| --- | --- |
| Module | solvers/orcaflex/format_converter |
| Repository | digitalmodel |
| Package | src/digitalmodel/solvers/orcaflex/format_converter |
| Pack revision | 0, 2026-10-16 |
| Code revision | 7aad603be1c6136106dbb2381ce5096b050654f8 |
| Analysis family | OrcaFlex riser and mooring; text conversion, routing family EC |
| Quality role | OrcaFlex riser/mooring quality manager |
| Status | DRAFT; second-provider review pending |

The code revision is the evidence pin for repository citations; section 9 identifies the separate implementation-map pin.

## 1. Purpose and scope

The package exposes six directed conversions between specification YAML, modular YAML and single model YAML, plus format detection and CLI dispatch. [src/digitalmodel/solvers/orcaflex/format_converter/__init__.py:L1-L29] [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L184-L243]

Single-to-modular splits sections into include files and writes a master and extracted parameters; modular-to-single merges master content and includes into one YAML document. [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L88-L157] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L67-L90]

Specification-to-modular delegates to the modular generator; the specification-to-single and single-to-specification routes chain converters through a temporary directory. [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L42-L64] [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L42-L83] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py:L42-L88]

Reverse extraction maps selected Environment and General fields, takes only the first line name for pipeline metadata, and reports omitted sections as expected or actionable gaps; its confidence is field coverage, not a solver or model-equivalence result. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L20-L46] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L164-L221]

The routing registry assigns beta maturity to the enclosing solvers domain, without a separate converter maturity field. [docs/registry/module-routing.yaml:L303-L327]

UNVERIFIED: Supported target program versions, geometric applicability and physical-load fidelity are not established by the offline evidence; native acceptance remains open in #280.

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
| `ModularToSpecConverter.convert`, `_set_nested` | Map 17 configured fields into dotted spec paths; raw field count / map length forms confidence, rounded to two decimals in the report | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L20-L46] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L137-L150] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L189-L221] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L299-L305] |
| `ModularToSpecConverter._populate_wave_trains` | Reverse-map wave types; recover height/period from regular or spectral keys; a single train becomes flat fields, multiple trains a list | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L227-L297] |
| Spec generation and chained routes | Delegate to the modular generator and the listed converters rather than implement physical analyses | [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L42-L55] [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L42-L74] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py:L42-L88] |

AQG r0, sections 4–9 and 10, supplies the gate contract in section 5; it is not a solver manual or a licensed engineering standard. (Analysis QA gates, revision 0, sections 4–10.)

## 4. Validated worked examples and regression examples

Validated examples: 0; independent physical-reference values are not supplied by the examples below, which compare synthetic inputs or serialization contracts. The cases are regression examples rather than physical validation. [tests/solvers/orcaflex/format_converter/test_round_trip.py:L19-L54] [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L261-L310]

Execution record, 2026-10-16: the baseline suite at the code pin collected 131 cases: 96 PASS, 0 FAIL and 35 pytest skips recorded here as NOT_RUN because the referenced fixture paths are absent. Of these, the converter test directory contributes 116 cases (81 PASS, 35 NOT_RUN), and the related generator fidelity file contributes 15 PASS. Section 10 enumerates the node IDs and results; these are observations of this pack's offline run, not results inferred from historical counts.

| Case / kind | Source and input | Expected result / tolerance basis | Existing test node ID | Offline result at pin |
| --- | --- | --- | --- | --- |
| regression-1 | Synthetic input dictionary, no independent reference | 0 changed original section values; exact equality, dimensionless field count | `tests/solvers/orcaflex/format_converter/test_round_trip.py::TestRoundTrip::test_roundtrip_synthetic_data` | PASS [tests/solvers/orcaflex/format_converter/test_round_trip.py:L19-L54] |
| regression-2 | Synthetic dictionary master with section-scoped fragment | WaterDepth 100 m, RefCurrentSpeed 0.5 m/s; 0 misplaced top-level fields; exact authored inputs | `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_dict_master_preserves_content_and_section_scope` | PASS [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L228-L269] |
| regression-3 | Synthetic single spectral train | Height 3.0 m, period 8.0 s, direction 180 degrees and type jonswap; exact authored inputs | `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_extract_single_spectral_train_keeps_hs_tz` | PASS [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L261-L310] |
| regression-4 | Synthetic unknown section | 99_unmapped.yml is last; 0 unknown fields in General; at least 1 warning | `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_unknown_sections_quarantined_last` | PASS [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L187-L225] |
| regression-5 | Synthetic out-of-scope object sections | 3 expected gap names; 0 misclassified VesselTypes entries in actionable gaps | `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecBoundaryReporting::test_expected_gaps_classified` | PASS [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L393-L409] |
| regression-6 | Synthetic None scalar | 1 literal `key: ~` representation; exact token comparison | `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexDumper::test_none_represented_as_tilde` | PASS [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L18-L21] |
| regression-7 | Dataset ID A01; external fixture requested by existing fixture | General and Environment equality; LineTypes equality when present; no physical-reference assertion | `tests/solvers/orcaflex/format_converter/test_round_trip.py::TestRoundTrip::test_roundtrip_real_a01` | NOT_RUN: missing fixture; #2008 [tests/solvers/orcaflex/format_converter/test_round_trip.py:L56-L90] |
| regression-8 | Specification fixture requested by existing fixture | 1 master.yml and 1 includes directory; exact presence checks | `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_convert_creates_modular_structure` | NOT_RUN: missing fixture; #2008 [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L28-L36] |

## 5. QA gates and executable checks

Gate applicability follows Analysis QA gates, revision 0, section 10, OrcaFlex riser and mooring: G0–G3 apply, G4 is conditional on newly imported diffraction data, and G5 applies to evidence issued with analysis results. Text regression results satisfy only their named text checks; they do not clear native gates. (AQG r0, sections 3–10.)

The numbered checks below are procedures for the next conversion assessment. Existing tests are executable checks; the specified native procedures are NOT_RUN here because a licensed solver is required. A gate cannot become PASS by treating successful conversion as a load or solve result. [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L15-L30] (AQG r0, sections 5–7.)

| Gate / status | Check | Module-specific pass criterion, quantity and unit | Execution / evidence |
| --- | --- | --- | --- |
| G0 applicable; offline subset PASS | check-01 | SingleToModularConverter then ModularToSingleConverter: 0 changed synthetic section values, count of fields | regression-1; dictionary comparison [tests/solvers/orcaflex/format_converter/test_round_trip.py:L19-L54] |
| G0 applicable; offline subset PASS | check-02 | Missing include fixture: 1 successful conversion report and at least 1 missing-include warning; production use additionally requires 0 unresolved include paths, count of paths | Existing test characterizes the warning behavior rather than qualifying partial output: `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_missing_include_warns` [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L99-L112] |
| G0 applicable; offline subset PASS | check-03 | ModularToSpecConverter: spectral height 3.0 m, period 8.0 s, direction 180 degrees, gamma 2.0 dimensionless and mapped type jonswap; exact synthetic inputs | regression-3; extracted YAML comparison [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L261-L310] |
| G0 applicable; offline subset PASS | check-04 | SingleToModularConverter: 0 include-order inversions for line types / lines before friction and browser groups; unknown include last, count of inversions | regression-4 and `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_real_sections_ordered_after_referenced_objects` [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L227-L278] |
| G0 applicable; offline subset PASS | check-05 | ModularToSpecConverter: 0 uncategorized unmapped section names; report.is_best_effort equals True (1 Boolean assertion) | Existing tests: `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecBoundaryReporting::test_all_unmapped_are_categorized` and `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecBoundaryReporting::test_is_best_effort_true` [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L377-L450] |
| G0 applicable; offline subset PASS | check-06 | OrcaFlexDumper: 1 expected tilde scalar for 1 None input and 0 alternate null tokens in that scalar; count of tokens | regression-6; exact serialization assertion [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L18-L21] |
| G0 applicable; offline subset PASS | check-07 | SpecToModularConverter: invalid spec produces 0 successful reports and at least 1 warning; count of reports / warnings | Existing invalid-spec test: `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_convert_invalid_spec_returns_failure` [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L48-L59] |
| G0 applicable; evidence incomplete | check-08 | For each input/output pair, compare all declared units, coordinate origin, direction and phase conventions; 0 unrecorded changes, count of conventions | Hand audit of the input UnitsSystem and fields against the extraction map; no automatic convention validator exists; #520 [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L20-L43] |
| G0 applicable; defect reproduced | check-09 | Negative include probes: missing, empty or wrong-type fragments and malformed master entries each leave 0 unreported omissions; count of omitted records | Reproduce using one valid Environment fragment plus one absent fragment; compare convert warnings against merge_to_dict/reverse report; observed reverse warnings 0 despite 1 missing fragment, issue pending (1) [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L95-L105] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L124-L181] |
| G1 applicable; NOT_RUN | check-10 | For each converter output, OrcaFlex load: 0 errors, 0 load warnings and 0 object-count mismatches against the source manifest; count of messages / objects | Open converted master.yml or single output in the matching-version demo; if that route is unavailable use LoadData through the licensed interface, recording exact version and messages; #280 [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L129-L156] [docs/plans/2026-05-05-issue-280-orcaflex-format-converter-license-validation.md:L15-L34] |
| G2 applicable; NOT_RUN | check-11 | Original and converted models: 0 unconverged statics, touchdown tension relative change at most 0.1 percent; all component rows covered, 0 unchecked objects; independent hand catenary and at least 6 offset steps per AQG r0 | Licensed statics on original / converted model, images, per-line hand reference and component/offset records; no converter function is a physical hand reference; #280 [docs/plans/2026-05-05-issue-280-orcaflex-format-converter-license-validation.md:L36-L40] (AQG r0, section 6.) |
| G3 applicable; NOT_RUN | check-12 | Converted model uses production settings for 1.0 s dynamics; 0 errors / warnings / nonnumeric values; line tension within 25.0 percent of static and 0 newly slack or compressed lines | Native short run from converged statics; record source and output fingerprints with final image; #280 (AQG r0, section 7.) [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L63-L83] |
| G4 conditional; NOT_APPLICABLE to synthetic text cases | check-13 | When converted VesselTypes come from a new diffraction run: 1 matching accepted G4 record per imported dataset and 0 unmatched datasets; count of records / datasets | Read source provenance and link the existing diffraction record; synthetic text fixtures contain no new diffraction result; native campaign provenance not checked; #280 (AQG r0, sections 8 and 10.) [src/digitalmodel/solvers/orcaflex/format_converter/section_mapping.py:L11-L35] [tests/solvers/orcaflex/format_converter/test_round_trip.py:L131-L164] |
| G5 applicable; evidence incomplete | check-14 | For every released converted model, 1 evidence entry keyed by input fingerprint, 0 missing required gate records, 0 unrecorded gaps; count of records / gaps | Hand inspection of appendix manifest linking ConversionReport, gate records, input digests, images and dispositions; no automatic appendix adapter exists, issue pending (5) [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L15-L30] (AQG r0, section 9.) |

For check-11, the independent hand comparison also requires top tension within 5.0 percent, top angle within 1.0 degree and touchdown distance within 5.0 percent; the offset sweep and complete component table remain separate required evidence, rather than consequences of round-trip equality. (AQG r0, sections 6.3–6.6.)

## 6. Known failure modes and pitfalls

sweep: 34 found

The count below is 34 reviewed source behaviors, including explicit error reports and benign initializers, rather than 34 confirmed defects. Each row records whether a detection check exists; an existing regression that merely preserves a failure mode is identified as such.

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
| sweep-31 | List concatenation can differ from native list collection replacement; Python equality alone cannot prove native model parity. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L214-L238] [src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py:L208-L212] | check-10 / check-11 NOT_RUN; #515 / #280 |
| sweep-32 | Includes use yaml.safe_load rather than the native blank-scalar loader used for the master; blank text can become None in a split/merge path. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L67-L68] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L176-L181] [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L17-L34] | no detection check exists for this cross-loader case; issue pending (1) |
| sweep-33 | CLI parses dry-run and batch but command handlers execute conversion without consulting either flag; a dry run can write files. [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L53-L101] [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L169-L182] | no shipped detection check exists; offline dry-run probe confirmed 1 master written, issue pending (2) |
| sweep-34 | WaterDepth alone is extracted; the generator can emit SeabedOriginDepth, so round-trip extraction can omit depth without a warning. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L20-L26] [tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py:L366-L420] | no detection check exists that requires depth preservation for the alias; #520 |

The sweep found no int() / float() conversion of external text, no 1970 sentinel, no five-percent fallback constant and no cache writer inside the package; the numeric zero is the confidence accumulator / fallback, while empty report lists are result containers. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L137-L150] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L189-L192] [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L15-L30]

### Confirmed incidents and limitations

| Incident / limitation | Detection check and state | Evidence |
| --- | --- | --- |
| #2008: old fixture roots silently skip real-file coverage | No shipped detection check exists that fails on missing required fixtures; offline run recorded 35 NOT_RUN | [tests/solvers/orcaflex/format_converter/conftest.py:L9-L55] |
| #515: historical dictionary-master content / scope loss | regression-2 / check-01; fixed behavior covered by a passing regression | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L228-L269] |
| #515: historical loss of single spectral height, period and type | regression-3 / check-03; fixed behavior covered by a passing regression | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L261-L310] |
| #520: reverse extraction is partial, even with high confidence | check-05; field-level coverage gaps remain as detailed above | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L172-L221] |
| #280: native load, statics and binary round-trip evidence incomplete | check-10 / check-11 / check-12 NOT_RUN; no licensed solver invoked | [docs/plans/2026-05-05-issue-280-orcaflex-format-converter-license-validation.md:L15-L49] |
| #518 / #2014: wider model-library fidelity is outside this synthetic evidence | no detection check exists within this package that qualifies the full library | [tests/solvers/orcaflex/format_converter/test_round_trip.py:L19-L92] |
| Quick-start text gives a 14-field confidence denominator; current map has 17 fields | Hand-count EXTRACTION_MAP; documentation count is stale, #520 | [docs/domains/orcaflex/QUICKSTART_FILE_CONVERSION.md:L329-L334] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L20-L46] |

## 7. Lessons log

| Date | Change reference | Lesson | Evidence | Pack change / shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-16 | Pack revision 0 | Separate successful text conversion from native acceptance; reverse-extraction confidence counts fields | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L189-L221] | Sections 1, 3 and 5; shared-log entry not assigned |
| 2026-10-16 | Pack revision 0 | Inspect warning propagation through merge_to_dict before relying on extracted output | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L95-L105] | Sections 5 and 6; issue pending (1); shared-log entry not assigned |
| 2026-10-16 | Pack revision 0 | Missing fixture paths are coverage loss, even when pytest exits successfully | [tests/solvers/orcaflex/format_converter/conftest.py:L9-L55] | Sections 4, 6 and 10; #2008; shared-log entry not assigned |

## 8. Ownership and escalation

Proposed review responsibility: OrcaFlex riser/mooring quality manager; deputy: riser/mooring deputy quality manager. Producing responsibility: converter documentation maintainers. These are review assignments for this draft, not claims of completed review.

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

Inventory linkage: the upstream generator is the separately routed orcaflex/modular_generator submodule; the converter is an engineering-data conversion route (EC). The external inventory's historical test count is not used as execution evidence. [docs/registry/module-routing.yaml:L317-L319] [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L42-L55]

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

Implementation map read from `origin/codex/implmap-work`, ref `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; citations to `docs/registry/implementation-map.yaml` in this section resolve at that ref, rather than the code pin. [docs/registry/implementation-map.yaml:L1-L4]

The only map family with an implementation path inside this package is `orcaflex-text-model-writers`: 7 in-package rows and 440 rows overall; it is therefore the only counted family. No nested converter subpackage is excluded. [docs/registry/implementation-map.yaml:L4933-L4943] [docs/registry/implementation-map.yaml:L7008-L7074]

Search uses the map's search_signals exactly as written with case-sensitive Python re. Before scanning, a case-insensitive path comparison excludes the vocabulary signal because it matches `solvers/orcaflex/format_converter`; the algorithm and constants_or_keys signals remain unchanged, including their escaping. The reproducible procedure is: load this map revision with PyYAML; retain signals for which `re.search(pattern, package_below_src, re.I)` is false; for every pinned package Python line run `re.search(pattern, line)`; assign the hit to the smallest enclosing AST function/class, otherwise its module-level definition. [docs/registry/implementation-map.yaml:L4933-L4943]

The search produced 36 hit definitions. All computing definitions are either listed by the map or named in section 3; other hits are containers, docstrings, serializers or delegates. Further second implementations not listed by either inventory: 0, within the acceptance limit of 1. The classification ledger follows, with code citations for inspection.

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
| 1 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L4940-L4942] |
| 2 | External definition; identity in cited map record | independent / script | [docs/registry/implementation-map.yaml:L4950-L4952] |
| 3 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L4959-L4961] |
| 4 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L4969-L4971] |
| 5 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L4979-L4981] |
| 6 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L4989-L4991] |
| 7 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L4999-L5001] |
| 8 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5009-L5011] |
| 9 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5021-L5023] |
| 10 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L5031-L5033] |
| 11 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L5041-L5043] |
| 12 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L5051-L5053] |
| 13 | External definition; identity in cited map record | example / writer | [docs/registry/implementation-map.yaml:L5061-L5063] |
| 14 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5071-L5073] |
| 15 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5081-L5083] |
| 16 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5091-L5093] |
| 17 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5101-L5103] |
| 18 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5111-L5113] |
| 19 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5121-L5123] |
| 20 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5133-L5135] |
| 21 | External definition; identity in cited map record | independent / script | [docs/registry/implementation-map.yaml:L5143-L5145] |
| 22 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5152-L5154] |
| 23 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5162-L5164] |
| 24 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5172-L5174] |
| 25 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5182-L5184] |
| 26 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5192-L5194] |
| 27 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5202-L5204] |
| 28 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5212-L5214] |
| 29 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5224-L5226] |
| 30 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5234-L5236] |
| 31 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5244-L5246] |
| 32 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5254-L5256] |
| 33 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5264-L5266] |
| 34 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5274-L5276] |
| 35 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5284-L5286] |
| 36 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5294-L5296] |
| 37 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5304-L5306] |
| 38 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5314-L5316] |
| 39 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5324-L5326] |
| 40 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5334-L5336] |
| 41 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5344-L5346] |
| 42 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5354-L5356] |
| 43 | External definition; identity in cited map record | independent / script | [docs/registry/implementation-map.yaml:L5364-L5366] |
| 44 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5373-L5375] |
| 45 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5383-L5385] |
| 46 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5393-L5395] |
| 47 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5403-L5405] |
| 48 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5416-L5418] |
| 49 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5426-L5428] |
| 50 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5438-L5440] |
| 51 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5448-L5450] |
| 52 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5458-L5460] |
| 53 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5468-L5470] |
| 54 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5478-L5480] |
| 55 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5488-L5490] |
| 56 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5498-L5500] |
| 57 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5508-L5510] |
| 58 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5520-L5522] |
| 59 | External definition; identity in cited map record | independent / script | [docs/registry/implementation-map.yaml:L5532-L5534] |
| 60 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5541-L5543] |
| 61 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5551-L5553] |
| 62 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5561-L5563] |
| 63 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5571-L5573] |
| 64 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5581-L5583] |
| 65 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5594-L5596] |
| 66 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5604-L5606] |
| 67 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5616-L5618] |
| 68 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5629-L5631] |
| 69 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5639-L5641] |
| 70 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5649-L5651] |
| 71 | External definition; identity in cited map record | independent / script | [docs/registry/implementation-map.yaml:L5659-L5661] |
| 72 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5668-L5670] |
| 73 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5678-L5680] |
| 74 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5688-L5690] |
| 75 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5698-L5700] |
| 76 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5708-L5710] |
| 77 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5718-L5720] |
| 78 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5728-L5730] |
| 79 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5738-L5740] |
| 80 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5750-L5752] |
| 81 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5760-L5762] |
| 82 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5770-L5772] |
| 83 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5782-L5784] |
| 84 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5792-L5794] |
| 85 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5802-L5804] |
| 86 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5812-L5814] |
| 87 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5824-L5826] |
| 88 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5834-L5836] |
| 89 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5844-L5846] |
| 90 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5854-L5856] |
| 91 | External definition; identity in cited map record | independent / script | [docs/registry/implementation-map.yaml:L5864-L5866] |
| 92 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5873-L5875] |
| 93 | External definition; identity in cited map record | independent / script | [docs/registry/implementation-map.yaml:L5883-L5885] |
| 94 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5892-L5894] |
| 95 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5902-L5904] |
| 96 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5912-L5914] |
| 97 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5922-L5924] |
| 98 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5934-L5936] |
| 99 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5944-L5946] |
| 100 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5954-L5956] |
| 101 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5966-L5968] |
| 102 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5976-L5978] |
| 103 | External definition; identity in cited map record | independent / script | [docs/registry/implementation-map.yaml:L5986-L5988] |
| 104 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L5995-L5997] |
| 105 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6005-L6007] |
| 106 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6015-L6017] |
| 107 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6025-L6027] |
| 108 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6035-L6037] |
| 109 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6045-L6047] |
| 110 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6055-L6057] |
| 111 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6065-L6067] |
| 112 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6075-L6077] |
| 113 | External definition; identity in cited map record | caller / writer | [docs/registry/implementation-map.yaml:L6085-L6087] |
| 114 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6097-L6099] |
| 115 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6110-L6112] |
| 116 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6120-L6122] |
| 117 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6130-L6132] |
| 118 | External definition; identity in cited map record | caller / writer | [docs/registry/implementation-map.yaml:L6142-L6144] |
| 119 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6154-L6156] |
| 120 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6164-L6166] |
| 121 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6174-L6176] |
| 122 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6184-L6186] |
| 123 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6194-L6196] |
| 124 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6204-L6206] |
| 125 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6214-L6216] |
| 126 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6224-L6226] |
| 127 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6234-L6236] |
| 128 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6244-L6246] |
| 129 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6254-L6256] |
| 130 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6264-L6266] |
| 131 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6274-L6276] |
| 132 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6284-L6286] |
| 133 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6294-L6296] |
| 134 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6304-L6306] |
| 135 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6314-L6316] |
| 136 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6324-L6326] |
| 137 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6334-L6336] |
| 138 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6344-L6346] |
| 139 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6354-L6356] |
| 140 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6364-L6366] |
| 141 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6374-L6376] |
| 142 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6384-L6386] |
| 143 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6394-L6396] |
| 144 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L6404-L6406] |
| 145 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6414-L6416] |
| 146 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6424-L6426] |
| 147 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6437-L6439] |
| 148 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6449-L6451] |
| 149 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6459-L6461] |
| 150 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6469-L6471] |
| 151 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6479-L6481] |
| 152 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6491-L6493] |
| 153 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6501-L6503] |
| 154 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6511-L6513] |
| 155 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6521-L6523] |
| 156 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6531-L6533] |
| 157 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6541-L6543] |
| 158 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6551-L6553] |
| 159 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6561-L6563] |
| 160 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6571-L6573] |
| 161 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6581-L6583] |
| 162 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L6591-L6593] |
| 163 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L6601-L6603] |
| 164 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6611-L6613] |
| 165 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6621-L6623] |
| 166 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L6631-L6633] |
| 167 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L6641-L6643] |
| 168 | External definition; identity in cited map record | independent / script | [docs/registry/implementation-map.yaml:L6651-L6653] |
| 169 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6660-L6662] |
| 170 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6673-L6675] |
| 171 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6683-L6685] |
| 172 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6693-L6695] |
| 173 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6705-L6707] |
| 174 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6715-L6717] |
| 175 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6725-L6727] |
| 176 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6735-L6737] |
| 177 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6745-L6747] |
| 178 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6755-L6757] |
| 179 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6765-L6767] |
| 180 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6775-L6777] |
| 181 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6785-L6787] |
| 182 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6795-L6797] |
| 183 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6805-L6807] |
| 184 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6815-L6817] |
| 185 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6825-L6827] |
| 186 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6835-L6837] |
| 187 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6845-L6847] |
| 188 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6855-L6857] |
| 189 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6865-L6867] |
| 190 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6875-L6877] |
| 191 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6885-L6887] |
| 192 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6895-L6897] |
| 193 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6905-L6907] |
| 194 | External definition; identity in cited map record | wrapper / writer | [docs/registry/implementation-map.yaml:L6915-L6917] |
| 195 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6928-L6930] |
| 196 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6938-L6940] |
| 197 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6948-L6950] |
| 198 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6958-L6960] |
| 199 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6968-L6970] |
| 200 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6978-L6980] |
| 201 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6988-L6990] |
| 202 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L6998-L7000] |
| 203 | `src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py` — `detect_format` | independent / writer | [docs/registry/implementation-map.yaml:L7008-L7010] |
| 204 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter.convert` | independent / writer | [docs/registry/implementation-map.yaml:L7018-L7020] |
| 205 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter._merge_master` | independent / writer | [docs/registry/implementation-map.yaml:L7032-L7034] |
| 206 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter._extract_includefiles.traverse` | independent / writer | [docs/registry/implementation-map.yaml:L7042-L7044] |
| 207 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py` — `ModularToSpecConverter.convert` | independent / writer | [docs/registry/implementation-map.yaml:L7052-L7054] |
| 208 | `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` — `SingleToModularConverter._write_master_file` | independent / writer | [docs/registry/implementation-map.yaml:L7062-L7064] |
| 209 | `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` — `SingleToModularConverter._write_parameters_file` | independent / writer | [docs/registry/implementation-map.yaml:L7072-L7074] |
| 210 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7082-L7084] |
| 211 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7092-L7094] |
| 212 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7102-L7104] |
| 213 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7112-L7114] |
| 214 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7122-L7124] |
| 215 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7132-L7134] |
| 216 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7142-L7144] |
| 217 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7154-L7156] |
| 218 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7164-L7166] |
| 219 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7177-L7179] |
| 220 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7187-L7189] |
| 221 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7197-L7199] |
| 222 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7208-L7210] |
| 223 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7218-L7220] |
| 224 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7228-L7230] |
| 225 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7238-L7240] |
| 226 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7248-L7250] |
| 227 | External definition; identity in cited map record | caller / writer | [docs/registry/implementation-map.yaml:L7258-L7260] |
| 228 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7270-L7272] |
| 229 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7280-L7282] |
| 230 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7290-L7292] |
| 231 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7300-L7302] |
| 232 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7310-L7312] |
| 233 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7320-L7322] |
| 234 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7330-L7332] |
| 235 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7340-L7342] |
| 236 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7350-L7352] |
| 237 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7360-L7362] |
| 238 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7370-L7372] |
| 239 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7380-L7382] |
| 240 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7390-L7392] |
| 241 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7400-L7402] |
| 242 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7410-L7412] |
| 243 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7420-L7422] |
| 244 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7430-L7432] |
| 245 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7440-L7442] |
| 246 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7450-L7452] |
| 247 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7460-L7462] |
| 248 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7470-L7472] |
| 249 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7480-L7482] |
| 250 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7490-L7492] |
| 251 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7500-L7502] |
| 252 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7510-L7512] |
| 253 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7520-L7522] |
| 254 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7530-L7532] |
| 255 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7540-L7542] |
| 256 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7552-L7554] |
| 257 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7562-L7564] |
| 258 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7572-L7574] |
| 259 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7582-L7584] |
| 260 | External definition; identity in cited map record | wrapper / writer | [docs/registry/implementation-map.yaml:L7592-L7594] |
| 261 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7604-L7606] |
| 262 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7614-L7616] |
| 263 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7627-L7629] |
| 264 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7637-L7639] |
| 265 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7647-L7649] |
| 266 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7657-L7659] |
| 267 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7667-L7669] |
| 268 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7677-L7679] |
| 269 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7687-L7689] |
| 270 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7697-L7699] |
| 271 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7707-L7709] |
| 272 | External definition; identity in cited map record | wrapper / writer | [docs/registry/implementation-map.yaml:L7717-L7719] |
| 273 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7730-L7732] |
| 274 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7740-L7742] |
| 275 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7750-L7752] |
| 276 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L7763-L7765] |
| 277 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L7775-L7777] |
| 278 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L7785-L7787] |
| 279 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7795-L7797] |
| 280 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7805-L7807] |
| 281 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7817-L7819] |
| 282 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7827-L7829] |
| 283 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7837-L7839] |
| 284 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7847-L7849] |
| 285 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7857-L7859] |
| 286 | External definition; identity in cited map record | caller / writer | [docs/registry/implementation-map.yaml:L7867-L7869] |
| 287 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7879-L7881] |
| 288 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7889-L7891] |
| 289 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7899-L7901] |
| 290 | External definition; identity in cited map record | wrapper / writer | [docs/registry/implementation-map.yaml:L7909-L7911] |
| 291 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7922-L7924] |
| 292 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7932-L7934] |
| 293 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7942-L7944] |
| 294 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7952-L7954] |
| 295 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7962-L7964] |
| 296 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7972-L7974] |
| 297 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7982-L7984] |
| 298 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L7992-L7994] |
| 299 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8002-L8004] |
| 300 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8012-L8014] |
| 301 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8022-L8024] |
| 302 | External definition; identity in cited map record | caller / writer | [docs/registry/implementation-map.yaml:L8032-L8034] |
| 303 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8044-L8046] |
| 304 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8054-L8056] |
| 305 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8064-L8066] |
| 306 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8074-L8076] |
| 307 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8084-L8086] |
| 308 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L8094-L8096] |
| 309 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8104-L8106] |
| 310 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8114-L8116] |
| 311 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8124-L8126] |
| 312 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8134-L8136] |
| 313 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8144-L8146] |
| 314 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8156-L8158] |
| 315 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8166-L8168] |
| 316 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8176-L8178] |
| 317 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8186-L8188] |
| 318 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8196-L8198] |
| 319 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8206-L8208] |
| 320 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8216-L8218] |
| 321 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8228-L8230] |
| 322 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8238-L8240] |
| 323 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8248-L8250] |
| 324 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8258-L8260] |
| 325 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8268-L8270] |
| 326 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8280-L8282] |
| 327 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8291-L8293] |
| 328 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8301-L8303] |
| 329 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8311-L8313] |
| 330 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8324-L8326] |
| 331 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8334-L8336] |
| 332 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8344-L8346] |
| 333 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8354-L8356] |
| 334 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8364-L8366] |
| 335 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L8374-L8376] |
| 336 | External definition; identity in cited map record | copy / writer | [docs/registry/implementation-map.yaml:L8384-L8386] |
| 337 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8394-L8396] |
| 338 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8404-L8406] |
| 339 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8414-L8416] |
| 340 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8424-L8426] |
| 341 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8436-L8438] |
| 342 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8446-L8448] |
| 343 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8458-L8460] |
| 344 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8471-L8473] |
| 345 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8481-L8483] |
| 346 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8493-L8495] |
| 347 | External definition; identity in cited map record | wrapper / writer | [docs/registry/implementation-map.yaml:L8506-L8508] |
| 348 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8518-L8520] |
| 349 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8528-L8530] |
| 350 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8538-L8540] |
| 351 | External definition; identity in cited map record | caller / writer | [docs/registry/implementation-map.yaml:L8548-L8550] |
| 352 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8561-L8563] |
| 353 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8573-L8575] |
| 354 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8583-L8585] |
| 355 | External definition; identity in cited map record | wrapper / writer | [docs/registry/implementation-map.yaml:L8597-L8599] |
| 356 | External definition; identity in cited map record | wrapper / writer | [docs/registry/implementation-map.yaml:L8609-L8611] |
| 357 | External definition; identity in cited map record | caller / writer | [docs/registry/implementation-map.yaml:L8620-L8622] |
| 358 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8633-L8635] |
| 359 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8648-L8650] |
| 360 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8658-L8660] |
| 361 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8668-L8670] |
| 362 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8678-L8680] |
| 363 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8688-L8690] |
| 364 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8702-L8704] |
| 365 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8715-L8717] |
| 366 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8725-L8727] |
| 367 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8740-L8742] |
| 368 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8750-L8752] |
| 369 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8760-L8762] |
| 370 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8774-L8776] |
| 371 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8784-L8786] |
| 372 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8794-L8796] |
| 373 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8804-L8806] |
| 374 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8814-L8816] |
| 375 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8824-L8826] |
| 376 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8834-L8836] |
| 377 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8844-L8846] |
| 378 | External definition; identity in cited map record | independent / writer | [docs/registry/implementation-map.yaml:L8857-L8859] |
| 379 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8867-L8869] |
| 380 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8877-L8879] |
| 381 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8887-L8889] |
| 382 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8897-L8899] |
| 383 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8909-L8911] |
| 384 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8919-L8921] |
| 385 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8929-L8931] |
| 386 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8939-L8941] |
| 387 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8949-L8951] |
| 388 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8959-L8961] |
| 389 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8971-L8973] |
| 390 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8981-L8983] |
| 391 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L8991-L8993] |
| 392 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9001-L9003] |
| 393 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9011-L9013] |
| 394 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9021-L9023] |
| 395 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9031-L9033] |
| 396 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9041-L9043] |
| 397 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9051-L9053] |
| 398 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9061-L9063] |
| 399 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9071-L9073] |
| 400 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9081-L9083] |
| 401 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9091-L9093] |
| 402 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9101-L9103] |
| 403 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9111-L9113] |
| 404 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9121-L9123] |
| 405 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9131-L9133] |
| 406 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9141-L9143] |
| 407 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9153-L9155] |
| 408 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9163-L9165] |
| 409 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9173-L9175] |
| 410 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9183-L9185] |
| 411 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9193-L9195] |
| 412 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9203-L9205] |
| 413 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9213-L9215] |
| 414 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9223-L9225] |
| 415 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9233-L9235] |
| 416 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9243-L9245] |
| 417 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9253-L9255] |
| 418 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9263-L9265] |
| 419 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9273-L9275] |
| 420 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9283-L9285] |
| 421 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9293-L9295] |
| 422 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9303-L9305] |
| 423 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9313-L9315] |
| 424 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9323-L9325] |
| 425 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9333-L9335] |
| 426 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9343-L9345] |
| 427 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9353-L9355] |
| 428 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9363-L9365] |
| 429 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9373-L9375] |
| 430 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9383-L9385] |
| 431 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9393-L9395] |
| 432 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9403-L9405] |
| 433 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9413-L9415] |
| 434 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9423-L9425] |
| 435 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9433-L9435] |
| 436 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9443-L9445] |
| 437 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9453-L9455] |
| 438 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9463-L9465] |
| 439 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9473-L9475] |
| 440 | External definition; identity in cited map record | test_helper / writer | [docs/registry/implementation-map.yaml:L9483-L9485] |

### Repository-wide variants and map follow-up

The repository-wide search included scripts and source, using `rg -l 'class .*Converter|def.*(convert|merge)|EXTRACTION_MAP|SECTION_MAPPING' scripts src/digitalmodel --glob '*.py'`, then `rg -n 'SingleToModularConverter|ModularToSingleConverter|ModularToSpecConverter|SECTION_MAPPING' scripts --glob '*.py'`; matches were inspected as text rather than imported.

| Other route | Classification / difference | Evidence |
| --- | --- | --- |
| Legacy conversion script | Wrapper around the listed SingleToModularConverter; not another algorithm | [scripts/conversion/yaml_to_include.py:L27-L43] |
| Physical validator merge | Own include loading and deep merge, including list concatenation; outside this package | [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L192-L265] |
| Validation utility merge | Own dictionary recursion and override behavior; outside this package | [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L201-L220] |
| MonolithicExtractor | Own native-YAML-to-spec algorithm, with generic model output and multidocument loading; outside this package | [src/digitalmodel/solvers/orcaflex/modular_generator/extractor.py:L109-L165] |

Map follow-up: reconcile writer/detector/helper classifications and cross-package merge/extraction variants, issue pending (3). No additional unmapped method family is asserted from keyword similarity alone; the package's own section-3 methods are explicitly named above. A further repository search finding is a map follow-up, not an extra in-package miss.

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
| Missing old-tree fixtures; 35 cases NOT_RUN | Test maintainers | #2008 | Repoint or explicitly qualify reference data; [tests/solvers/orcaflex/format_converter/conftest.py:L9-L55] |
| Partial reverse mapping, confidence denominator docs and absent depth alias / field-level warnings | Converter maintainers | #520 | Section-6 rows identify individual gaps; [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L20-L46] |
| Missing/empty/wrong-type/nested include handling and scalar-loader differences | Converter maintainers | issue pending (1) | Add negative fixtures and warning propagation; [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L95-L181] |
| Unsupported dry-run / batch flags | Converter maintainers | issue pending (2) | Reject or implement before conversion; [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L169-L182] |
| Implementation-map follow-up | Map maintainers | issue pending (3) | Reconcile section-3 definitions and cross-package variants; section-9 inventory and audit |
| Identifying discovery references | Documentation maintainers | issue pending (4) | Codename discovery references without copying identities into the pack |
| Complete evidence appendix and provenance adapter absent | Riser/mooring quality manager | issue pending (5) | Implement check-14 evidence linkage; [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L15-L30] |
| Full mapped contracts-domain invocation not run because other packs collect licensed-binding imports | Test maintainers | #280 | Run the full domain where native import is permitted; [tests/docs/test_agent_pack_node_ids.py:L18-L38] [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L25-L36] |
| Existing duplicate submodules keys in the solvers routing entry | Registry maintainers | issue pending (6) | Reconcile existing mappings in a separate registry change; [docs/registry/module-routing.yaml:L303-L325] |

The calculation-citation contract was readable; no standards-derived coefficient was introduced. Pack-native formulas in section 3 are field counts or serialization algorithms, not licensed-standard calculations. No private sidecar entry is required for this public synthetic evidence. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L189-L192]

### Complete test-node ledger

The command form is `PYTHONPATH=src uv run --no-sources python -m pytest tests/solvers/orcaflex/format_converter tests/solvers/orcaflex/modular_generator/test_roundtrip_fidelity.py -q -rA -p no:cacheprovider --basetemp=<scratch>`. The configured uv executable and external scratch location were used; no addopts override or worker option was added. BASE and AFTER run against the uncommitted tree's same pinned code revision. The table records baseline results; the AFTER comparison below completed with the same per-node outcomes.

| Existing test node ID | BASE result | Definition at code pin |
| --- | --- | --- |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_convert_creates_output_file` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L16-L38] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_merged_output_contains_all_sections` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L40-L69] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_deep_merge_dicts` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L71-L76] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_deep_merge_lists_concatenate` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L78-L83] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_deep_merge_override_non_dict` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L85-L90] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_deep_merge_adds_new_keys` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L92-L97] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_missing_include_warns` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L99-L112] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_convert_real_a01_modular` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L114-L129] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_convert_from_directory` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L131-L147] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_merge_to_dict` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L149-L164] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_report_format` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L166-L183] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_header_preserved` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L185-L209] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_missing_master_returns_failure` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L211-L220] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_no_source_raises_error` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L222-L226] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_dict_master_preserves_content_and_section_scope` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L228-L269] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_dict_master_section_include_wrapped_fragment` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L271-L289] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_dict_master_missing_section_include_warns` | PASS | [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L291-L314] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_modular_directory` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L17-L19] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_modular_master_file` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L21-L23] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_single_file` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L25-L27] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_spec_file` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L29-L31] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_single_from_dict` | PASS | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L33-L39] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_spec_from_dict` | PASS | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L41-L51] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_modular_from_list` | PASS | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L53-L62] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_modular_directory_structure` | PASS | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L64-L69] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_error_on_nonexistent_path` | PASS | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L71-L73] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_error_on_empty_file` | PASS | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L75-L79] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_error_on_unknown_dict_format` | PASS | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L81-L87] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_error_on_directory_without_master` | PASS | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L89-L92] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_format_type_values` | PASS | [tests/solvers/orcaflex/format_converter/test_format_detector.py:L94-L98] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIDetect::test_detect_single_file` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L17-L20] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIDetect::test_detect_modular_dir` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L22-L25] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIDetect::test_detect_spec_file` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L27-L30] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIDetect::test_detect_nonexistent` | PASS | [tests/solvers/orcaflex/format_converter/test_cli.py:L32-L35] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLISingle2Modular::test_convert` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L41-L46] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLISingle2Modular::test_no_output_uses_default` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L48-L55] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIModular2Single::test_convert` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L61-L66] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLISpec2Modular::test_convert` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L72-L77] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLISingle2Spec::test_convert` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L83-L88] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIAuto::test_auto_single_to_modular` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L94-L100] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIAuto::test_auto_modular_to_single` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_cli.py:L102-L108] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIParser::test_parser_creates_subcommands` | PASS | [tests/solvers/orcaflex/format_converter/test_cli.py:L114-L118] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIParser::test_no_command_returns_1` | PASS | [tests/solvers/orcaflex/format_converter/test_cli.py:L120-L123] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIReporting::test_print_report_surfaces_expected_and_actionable_gaps` | PASS | [tests/solvers/orcaflex/format_converter/test_cli.py:L129-L149] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_convert_creates_directory_structure` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L16-L31] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_convert_splits_sections_to_files` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L33-L48] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_include_file_contains_section_data` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L50-L66] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_master_file_has_include_directives` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L68-L82] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_parameters_extracted` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L84-L101] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_convert_real_a01_file` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L103-L112] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_report_format` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L114-L127] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_empty_file_returns_failure` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L129-L138] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_header_preserved_in_master` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L140-L158] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_multiple_sections_same_file` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L160-L185] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_unknown_sections_quarantined_last` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L187-L225] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_real_sections_ordered_after_referenced_objects` | PASS | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L227-L278] |
| `tests/solvers/orcaflex/format_converter/test_round_trip.py::TestRoundTrip::test_roundtrip_synthetic_data` | PASS | [tests/solvers/orcaflex/format_converter/test_round_trip.py:L19-L54] |
| `tests/solvers/orcaflex/format_converter/test_round_trip.py::TestRoundTrip::test_roundtrip_real_a01` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_round_trip.py:L56-L90] |
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
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexLoad::test_load_real_a01_file` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L148-L154] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexLoad::test_roundtrip_dump_load` | PASS | [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L156-L173] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_convert_creates_modular_structure` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L25-L33] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_convert_generates_includes` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L35-L42] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_convert_invalid_spec_returns_failure` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L44-L54] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_report_metadata` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L56-L63] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_no_source_raises` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L65-L69] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_init_with_spec_path` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L71-L77] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_supported_formats` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L79-L82] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToSingleConverter::test_convert_creates_single_file` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L88-L95] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToSingleConverter::test_output_has_orcaflex_sections` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L97-L108] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToSingleConverter::test_report_metadata` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L110-L117] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToSingleConverter::test_no_source_raises` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L119-L123] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToSingleConverter::test_supported_formats` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L125-L128] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToSingleConverter::test_invalid_spec_propagates_failure` | PASS | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L130-L141] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_extract_environment_fields` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L147-L161] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_extract_simulation_fields` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L163-L173] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_confidence_score` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L175-L182] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_unmapped_sections_reported` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L184-L191] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_metadata_placeholder` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L193-L203] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_confidence_header_in_file` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L205-L213] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_from_directory` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L215-L221] |
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
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSingleToSpecConverter::test_convert_real_a01` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L476-L484] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSingleToSpecConverter::test_extracted_spec_has_environment` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L486-L498] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSingleToSpecConverter::test_report_metadata` | NOT_RUN: missing fixture; #2008 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L500-L508] |
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

Execution comparison at the code pin: BASE and AFTER each collected 131 cases, ran and passed 96, failed 0 and skipped 35; the same 35 missing-fixture cases are NOT_RUN in the ledger, not an additional 35 cases. All 96 passing node IDs matched between runs. [tests/solvers/orcaflex/format_converter/conftest.py:L9-L55]

Contract-directory and routing checks used `PYTHONPATH=src uv run --no-sources python -m pytest tests/contracts tests/docs/test_digitalmodel_routing_contract.py -q -rA -p no:cacheprovider --basetemp=<scratch>`: 494 run, 494 passed, 0 failed, 0 skipped at BASE and AFTER. These checks ran on the uncommitted tree; the converter source remained at the code pin. [tests/docs/test_digitalmodel_routing_contract.py:L94-L110]

The full contracts domain also includes pack-node collection that can import licensed bindings through other packs' native test files; that broader invocation was NOT_RUN under the solver prohibition. The contract directory and explicit routing file above were run without excluding tests from either path. [tests/DOMAINS.md:L13-L13] [tests/docs/test_agent_pack_node_ids.py:L18-L38] [tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py:L25-L36]
