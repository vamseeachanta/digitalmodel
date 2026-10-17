# OrcaFlex format converter knowledge pack

| Header | Value |
| --- | --- |
| Module | OrcaFlex format converter |
| Repository | digitalmodel |
| Package | `src/digitalmodel/solvers/orcaflex/format_converter` |
| Pack revision | 1 |
| Code revision | `7aad603be1c6136106dbb2381ce5096b050654f8` |
| Analysis family | OrcaFlex riser and mooring (MR) |
| Quality role | Riser and mooring quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The public interface exports six converters between specification, modular and single YAML representations, plus format detection and conversion reports. [src/digitalmodel/solvers/orcaflex/format_converter/__init__.py:L1-L29]
Reverse extraction is best-effort: component details remain unmapped and the report distinguishes expected from actionable gaps. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L48-L85] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L172-L222]
The routing registry classifies the containing solvers domain as beta; it does not establish a separate converter maturity. [docs/registry/module-routing.yaml:L303-L331]
Specification generation delegates to `ModularModelGenerator`; the single-output route chains generation and merging through a temporary directory. [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L42-L64] [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L43-L84]
UNVERIFIED: supported target OrcaFlex versions and physical equivalence after conversion are not established by the offline tests; see section 10, item O1.

## 2. Inputs and outputs

### Inputs

| Input | Unit / range | Default | Interpretation and source |
| --- | --- | --- | --- |
| source and target | filesystem paths; source required | converter-specific output beside source | Missing source raises `ValueError`; modular source may be a directory or master file. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L41-L63] |
| single YAML | declared source units; no range conversion | no numeric default | Loaded with `orcaflex_load`; empty input returns failure. [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L93-L106] |
| modular YAML | source units unchanged | master.yml inside a directory | Include content is merged in master order. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L124-L165] |
| specification | schema-defined input | output directory based on source stem | Generator owns validation and include production. [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L34-L55] |

### Outputs

| Output | Unit / range | Default | Meaning |
| --- | --- | --- | --- |
| master.yml and includes | YAML data, inherited units | section filenames sorted lexically | Unknown sections go last, with a warning. [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L111-L139] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L191-L215] |
| inputs/parameters.yml | source field values, no conversion | omitted when no parameters extracted | Environment and General keys plus first-train wave values are extracted. [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L141-L147] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L161-L189] |
| merged single YAML | source units | master-parent name plus .yml | Header preserved; include dictionaries merged. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L61-L90] |
| extracted specification | source values; no unit normalisation | extracted_spec.yml | Field projection with placeholder metadata; only first line name extracted. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L143-L170] |
| ConversionReport | confidence dimensionless, default 1.0 | empty warning and gap lists | Success, paths, formats, confidence and boundary flags. [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L15-L44] |

No coordinate, origin, sign or phase transformation is performed by the extraction map: selected values are assigned directly; this does not establish the convention or unit validity of an arbitrary source. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L20-L43] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L143-L150]
UNVERIFIED: complete source-format unit and coordinate conventions are not established here; section 10 item O2 requires explicit design-basis checks before engineering use.

## 3. Method and governing standards

| Method | Implementing definition | Basis / boundary |
| --- | --- | --- |
| Detection | `detect_format` | Directory structure, includefile list, metadata keys, then recognised model sections; raises on unknown structure. [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L48-L91] |
| Section partition | `SingleToModularConverter.convert`, `_group_by_filename` | Canonical plus extra mappings; preserve shared-file sections and quarantine unknowns. [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L108-L156] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L191-L215] |
| Parameter projection | `SingleToModularConverter._extract_input_parameters` | Named fields and first wave train only. [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L161-L189] |
| Include traversal | `ModularToSingleConverter._extract_includefiles` and nested `traverse` | Recursively collects references; collection alone does not recursively load included files. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L183-L204] |
| Include load and path resolution | `ModularToSingleConverter._load_include`, `_resolve_include_path` | Missing files warn; empty or non-dictionary contents return None; paths resolved relative to master unless absolute. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L167-L181] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L206-L212] |
| Merge | `ModularToSingleConverter._merge_master`, `_deep_merge`, `merge_to_dict` | Section-scoped fragments retained; dictionaries recurse, lists concatenate, other values overwrite. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L95-L165] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L214-L238] |
| Reverse projection | `ModularToSpecConverter.convert`, `_set_nested` | Field lookup into nested specification; confidence is extracted field count divided by map length, rounded to two decimals. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L137-L150] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L189-L222] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L299-L305] |
| Wave projection | `ModularToSpecConverter._populate_wave_trains` | Reverse type mapping; regular or spectral height/period; multiple trains replace flat fields. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L227-L297] |
| File serialisation | `SingleToModularConverter._write_include_file`, `_write_master_file`, `_write_parameters_file` | Shared dumper for includes, preserved or default YAML header for master, extracted parameter YAML. [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L217-L277] |
| Delegating routes | `SpecToModularConverter.convert`, `SpecToSingleConverter.convert`, `SingleToSpecConverter.convert` | Generator wrapper and temporary-directory chains; failure reports propagate. [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L34-L64] [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_single.py:L43-L84] [src/digitalmodel/solvers/orcaflex/format_converter/single_to_spec.py:L43-L89] |

The package implements representation changes and field extraction rather than a structural-response equation; confidence measures field coverage rather than physical equivalence. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L189-L222]
The documented reverse-extraction boundary warns against treating extracted specifications as lossless model definitions. [docs/domains/orcaflex/QUICKSTART_FILE_CONVERSION.md:L295-L350]
UNVERIFIED: no governing engineering-standard edition, clause or calculation Citation sidecar was established for these transformations; section 10 item O2 records the qualification boundary.

## 4. Validated worked examples

Validated examples: 0; the following cases use authored test dictionaries or representation expectations, rather than an independently qualified engineering benchmark. [tests/solvers/orcaflex/format_converter/test_round_trip.py:L19-L54]

| Regression case | Input / expected result / tolerance | Existing node ID | Offline result at code revision |
| --- | --- | --- | --- |
| Synthetic round trip | sample_single_data; all original values equal, 0 differing values (count). [tests/solvers/orcaflex/format_converter/test_round_trip.py:L19-L54] | `tests/solvers/orcaflex/format_converter/test_round_trip.py::TestRoundTrip::test_roundtrip_synthetic_data` | PASS |
| Shared buoy sections | three section keys preserved; 0 differing values (count). [tests/solvers/orcaflex/format_converter/test_round_trip.py:L92-L126] | `tests/solvers/orcaflex/format_converter/test_round_trip.py::TestRoundTrip::test_roundtrip_with_multiple_buoy_sections` | PASS |
| Nested round trip | whole dictionary equal; 0 differing values (count). [tests/solvers/orcaflex/format_converter/test_round_trip.py:L128-L164] | `tests/solvers/orcaflex/format_converter/test_round_trip.py::TestRoundTrip::test_roundtrip_preserves_nested_data` | PASS |
| Section-scoped master | depth 100 and current 0.5 in source units; 0 misplaced properties (count). [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L228-L269] | `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_dict_master_preserves_content_and_section_scope` | PASS |
| Spectral extraction | height 3.0, period 8.0, direction 180, gamma 2.0 in source units; exact assertions, no independent tolerance. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L261-L310] | `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_extract_single_spectral_train_keeps_hs_tz` | PASS |
| Boundary report | best-effort flag true: 1 true flag (count). [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L377-L383] | `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecBoundaryReporting::test_is_best_effort_true` | PASS |

The offline command collected 116 tests: 81 PASS, 0 FAIL and 35 SKIP; the 35 cases skipped because of a stale fixture path are classified NOT_RUN in section 10 rather than evidence of conversion fidelity. The fixtures exist under `docs/domains/orcaflex/examples/` and `docs/domains/orcaflex/pipeline/`, but conftest still points to the deleted `docs/modules/orcaflex/` tree (#2008). These tests need no solver; the fixture skip mechanism is explicit. [tests/solvers/orcaflex/format_converter/conftest.py:L9-L55]

## 5. QA gates

The gate set is AQG r0, section 10, “OrcaFlex riser and mooring”; G0 and G1 apply to conversion outputs; G2 and G3 are NOT_APPLICABLE to the converter because representation changes do not solve structural response. C4 and C5 remain downstream model qualification checks, G4 is conditional on new diffraction data, and G5 applies to every issued report (AQG r0, sections 4–11).
These procedures define acceptance checks; offline representation checks do not sign an engineering model's solver gates. [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L15-L44]

| Gate | Applicability | Check and executable procedure | Numeric pass criterion and unit | Evidence / current status |
| --- | --- | --- | --- | --- |
| G0 | Applicable | C0: exercise `SingleToModularConverter.convert`, `ModularToSingleConverter.convert` and `ModularToSpecConverter.convert` through the six section-4 nodes and `TestSingleToModularConverter.test_unknown_sections_quarantined_last` in the module test directory. | 0 assertion failures (count); exact source-value equality. [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L187-L225] | pytest node outcomes: PASS |
| G0 | Applicable | C1: execute `TestModularToSingleConverter.test_missing_include_warns`; inspect `report.warnings`, never accept `success` alone. | At least 1 missing-include warning (count) for the negative fixture; 0 warnings (count) required for a production conversion. [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L99-L112] | Negative regression PASS |
| G0 | Applicable | C2: strict-parse emitted master and all includes with duplicate keys rejected; compare source UnitsSystem and every transferred numeric value; verify every referenced include exists. | 0 duplicate keys, 0 unresolved files, 0 changed source values (counts); 0 unit-system differences (count). [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L121-L147] | Proposed acceptance inspection; no detection check exists in the shipped tests for all these conditions; issue pending (1) |
| G1 | Applicable | C3: load the emitted `master.yml` or merged single YAML in the target OrcaFlex version, through the version-matched demo route or the licensed LoadData route; compare object inventory with source. | 0 load warnings, 0 load errors, 0 object-count differences (counts); exact version match. | NOT_RUN: licensed interface and solver execution excluded; #280 (AQG r0, section 5) |
| G2 | NOT_APPLICABLE to converter | C4 (downstream model check): calculate statics for the converted model; compare line top tension and angle with an independent hand calculation; record every object and offset sweep. | 0 unconverged models (count); tension difference at most 5 %, angle difference at most 1.0 degree; at least 6 offsets (count); 0 unchecked objects (count). | Converter performs no statics solution; C4 does not establish conversion fidelity. Downstream C4 NOT_RUN: licensed solver required; #280 (AQG r0, section 6) |
| G3 | NOT_APPLICABLE to converter | C5 (downstream model check): run converted model from converged statics with production settings and retain the final image and warnings. | Reach 1 s simulated time; 0 errors, warnings or nonnumeric results (counts); tension change at most 25 % relative to statics. | Converter performs no dynamics solution; C5 does not establish conversion fidelity. Downstream C5 NOT_RUN: licensed solver required; #280 (AQG r0, section 7) |
| G4 | Conditional | C6: for a new diffraction-derived VesselTypes input, require its diffraction record; compare balance and independent displacement against the producing run. | 0 missing records (count); weight/buoyancy mismatch at most 1.0 %; displacement mismatch at most 0.5 %. | NOT_APPLICABLE to synthetic conversion cases; no new diffraction run supplies them. [tests/solvers/orcaflex/format_converter/conftest.py:L59-L89] (AQG r0, section 8) |
| G5 | Applicable to issued report | C7: reconcile ConversionReport warnings, expected_gaps and actionable_gaps with the report appendix and link the solver-gate records by input digest. | 0 omitted warnings or gaps (counts); 0 missing required gate records (count). [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L19-L44] | NOT_RUN: no engineering report or solver record issued; #280 (AQG r0, section 9) |
| G0 | Applicable | C8: execute all `TestModularToSpecBoundaryReporting` nodes and `TestCLIReporting.test_print_report_surfaces_expected_and_actionable_gaps`. | 0 uncategorised unmapped sections (count); 1 best-effort flag (count); both expected and actionable categories shown (2 categories). [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L377-L459] [tests/solvers/orcaflex/format_converter/test_cli.py:L126-L149] | PASS |
| G0 | Applicable | C9: execute `TestSpecToModularConverter.test_convert_invalid_spec_returns_failure` and `TestSpecToSingleConverter.test_invalid_spec_propagates_failure`. | 0 successful reports (count) for invalid specification; at least 1 warning (count) at generator wrapper. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L44-L56] [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L130-L141] | PASS |

## 6. Known failure modes and pitfalls

sweep: 26 found

The sweep classifications below distinguish reported failures from silent omissions; a listed counter initialisation is not itself a defect. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L137-L150]
The exact searches, run over all package Python files, were:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?|1970|float\(|int\(|unit|cache|0\.05' src/digitalmodel/solvers/orcaflex/format_converter
rg -n '\.get\(|setdefault|return$|confidence|safe_load|warnings|dry.run|batch' src/digitalmodel/solvers/orcaflex/format_converter
```

| ID | Symptom / cause at pin | Detection / incident or tracking |
| --- | --- | --- |
| S01 | Non-dictionary master list records silently skipped. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L124-L127] | no detection check exists; issue pending (1) |
| S02 | Missing include returns None after warning, yet convert returns success. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L167-L174] | C1; issue pending (1) |
| S03 | Empty, scalar or list include returns None without warning. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L176-L181] | no detection check exists; issue pending (1) |
| S04 | merge_to_dict discards the accumulated warning list. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L95-L105] | no detection check exists; issue pending (1) |
| S05 | Unsupported master root falls through to empty merged dictionary. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L122-L165] | no detection check exists; issue pending (1) |
| S06 | Recursive reference discovery returns an empty list for falsey input; convert warns on no directives. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L183-L204][src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L70-L81] | no detection check exists; issue pending (1) |
| S07 | Malformed parameter section skipped. [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L165-L169] | no detection check exists; #520 |
| S08 | Missing or null/tilde parameter fields omitted. [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L170-L174] | no detection check exists; #520 |
| S09 | Parameter summary reads only the first wave train; absent fields omitted. [src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L176-L187] | no detection check exists; #520 |
| S10 | Generator exceptions become failure reports; callers must inspect success and warnings. [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L42-L55] | C9; #280 |
| S11 | No merged data returns a failure report. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L119-L127] | TestModularToSpecConverter.test_empty_modular_returns_failure in the tests directory; #520 |
| S12 | Missing Environment, WaveTrains or first-train dictionary omits flat confidence fields. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L130-L135] | no detection check exists; #520 |
| S13 | extracted_count starts at zero; missing mapped values are omitted, not inferred. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L137-L150] | no detection check exists for every absent-field branch; #520 |
| S14 | Placeholder metadata uses extracted_model and unknown structure/operation. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L155-L162] | no detection check exists in an offline passing case; #520 |
| S15 | Only first Lines name retained; missing or malformed first row becomes empty dictionary. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L164-L170] | C8 exposes actionable section loss; #520 |
| S16 | VariableData counted as known although not projected into spec. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L172-L177] | no detection check exists; #520 |
| S17 | Unknown unmapped sections default to actionable, preventing an unclassified category. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L179-L187] | C8; #520 |
| S18 | Confidence falls back to 0.0 for empty extraction map and is rounded; not a physics metric. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L189-L198][src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L210-L222] | no detection check exists for empty-map branch; #520 |
| S19 | Wave population silently returns for missing/malformed Environment or WaveTrains. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L249-L254] | no detection check exists; #520 |
| S20 | Non-dictionary wave-train rows silently skipped. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L256-L259] | no detection check exists; #520 |
| S21 | Missing WaveType defaults to Airy; unknown types pass through unchanged. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L263-L264] | no detection check exists; #520 |
| S22 | Absent wave keys omitted; multi-train projection removes flat fields. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L265-L297] | C0 spectral regression covers present keys only; #520 |
| S23 | Report defaults to confidence 1.0 with empty warning/gap lists, even before specialised completeness assessment. [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L19-L30] | C8 only covers specialised boundary reports; #520 |
| S24 | Physical source values copied with no unit conversion or numeric text parsing. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L143-L150][src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py:L170-L187] | no detection check exists for non-SI or decimal-separator text; issue pending (2) |
| S25 | Nested include directives inside loaded fragments are not expanded by `_load_include`; recursive reference collection does not recursively load them. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L167-L204] | no detection check exists; issue pending (1) |
| S26 | Include `safe_load` collapses native blank scalars to None, while the shared native loader distinguishes blank text; round-trip tests use `safe_load` for comparison and can hide that distinction. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L176-L181] [src/digitalmodel/solvers/orcaflex/yaml_utils.py:L17-L34] [tests/solvers/orcaflex/format_converter/test_round_trip.py:L41-L54] | no detection check exists; issue pending (1) |

Additional observed pitfalls and incident evidence:

| Pitfall | Detection / tracking | Evidence |
| --- | --- | --- |
| CLI accepts dry-run and batch switches but conversion handlers do not inspect them. | no detection check exists; issue pending (3) | [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L53-L101][src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L169-L182] |
| Confidence documentation says 14 fields but the extraction map contains 16 rows. | no detection check exists for documentation consistency; #520 | [docs/domains/orcaflex/QUICKSTART_FILE_CONVERSION.md:L330-L334] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L20-L46] |
| A stale fixture path after the documentation move turns engineering example tests into skips despite the fixtures being present. | no fixture-presence failure check exists; #2008 | [tests/solvers/orcaflex/format_converter/conftest.py:L9-L55] |
| Whole-model reverse equivalence and library round-trip coverage remain separate from field projection. | C8 reports the boundary; #520 and #2014 | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L172-L222] |
| Unknown section declaration order previously put references ahead of referenced objects. | C0 unknown-section regression; historical regression comments, no incident number established | [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L187-L225] |
| Single spectral train could lose spectral height and period before regeneration. | C0 spectral regression; historical regression comments, #520 | [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L261-L310] |
| CLI detection catches ValueError, prints the error and returns 1; it does not silently claim success. | TestCLIDetect.test_detect_nonexistent in the tests directory | [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L104-L123][tests/solvers/orcaflex/format_converter/test_cli.py:L32-L35] |

No explicit external-text int/float conversion, epoch default, five-percent fallback or cache write was found by the stated package searches; the transformations assign loaded YAML values. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L143-L150]

## 7. Lessons log

| Date | Record | Lesson | Evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- | --- |
| 2026-10-17 | Initial pack | Distinguish representation regressions from model qualification; do not use confidence as whole-model fidelity. | [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py:L189-L222] | Sections 4–6 define this boundary. | Not created; issue pending (2) |

## 8. Ownership

Proposed review responsibilities: riser and mooring quality manager; deputy: installation quality manager; producing discipline: engineering software; ambiguous method classifications will be referred to the family quality manager, with cross-family questions referred to the maintainer (AQG r0, section 11).
UNVERIFIED: named role assignments and deputy availability are not established; section 10 item O3 records the assignment gap.

## 9. Dependencies and implementation inventory

The CLI names OrcaFlex in its parser description and documents all six conversion routes; the detector and package docstrings name OrcaFlex as the text format, not as a solver result produced here. [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L1-L16] [src/digitalmodel/solvers/orcaflex/format_converter/cli.py:L161-L166] [src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py:L19-L37] [src/digitalmodel/solvers/orcaflex/format_converter/__init__.py:L1-L7]
The package uses PyYAML, pathlib, the shared `yaml_utils` and the neighbouring modular generator; the specification routes inherit the generator's schema and builder behaviour. [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L8-L16] [src/digitalmodel/solvers/orcaflex/format_converter/spec_to_modular.py:L42-L55]
No target solver version is fixed by the converter protocol; source headers can be preserved but preservation is not a loadability test. [src/digitalmodel/solvers/orcaflex/format_converter/protocols.py:L15-L30] [src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py:L79-L90]
The legacy conversion script declares delegation to a compatibility import path; its importability is not established by the converter tests. [scripts/conversion/yaml_to_include.py:L1-L45]

### Solver-name dependency locations (text only)

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

### Map revision and counted family

Implementation-map source: `docs/registry/implementation-map.yaml` at data commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, whose declared code revision is `270c8759c374b67a7f032953e7fcdb14e0224461`; the map blob is byte-identical at the header code revision, so every citation also resolves at that single code revision. [docs/registry/implementation-map.yaml:L1-L4]
The sole family with in-package rows is `orcaflex-text-model-writers`: 7 in-package rows, therefore ranked first and counted; the full family inventory contains 440 rows, including out-of-package implementations and test helpers. [docs/registry/implementation-map.yaml:L7008-L7078]
The inventory below identifies every map implementation by its one-based ordinal within this family's implementations sequence and a citation to its complete mapping record; these identifiers resolve to the map's exact path, symbol and classification without repeating identifying discovery names.

| Family row | Complete implementation record at map data revision |
| --- | --- |
| 1 | [docs/registry/implementation-map.yaml:L4940-L4949] |
| 2 | [docs/registry/implementation-map.yaml:L4950-L4958] |
| 3 | [docs/registry/implementation-map.yaml:L4959-L4968] |
| 4 | [docs/registry/implementation-map.yaml:L4969-L4978] |
| 5 | [docs/registry/implementation-map.yaml:L4979-L4988] |
| 6 | [docs/registry/implementation-map.yaml:L4989-L4998] |
| 7 | [docs/registry/implementation-map.yaml:L4999-L5008] |
| 8 | [docs/registry/implementation-map.yaml:L5009-L5020] |
| 9 | [docs/registry/implementation-map.yaml:L5021-L5030] |
| 10 | [docs/registry/implementation-map.yaml:L5031-L5040] |
| 11 | [docs/registry/implementation-map.yaml:L5041-L5050] |
| 12 | [docs/registry/implementation-map.yaml:L5051-L5060] |
| 13 | [docs/registry/implementation-map.yaml:L5061-L5070] |
| 14 | [docs/registry/implementation-map.yaml:L5071-L5080] |
| 15 | [docs/registry/implementation-map.yaml:L5081-L5090] |
| 16 | [docs/registry/implementation-map.yaml:L5091-L5100] |
| 17 | [docs/registry/implementation-map.yaml:L5101-L5110] |
| 18 | [docs/registry/implementation-map.yaml:L5111-L5120] |
| 19 | [docs/registry/implementation-map.yaml:L5121-L5132] |
| 20 | [docs/registry/implementation-map.yaml:L5133-L5142] |
| 21 | [docs/registry/implementation-map.yaml:L5143-L5151] |
| 22 | [docs/registry/implementation-map.yaml:L5152-L5161] |
| 23 | [docs/registry/implementation-map.yaml:L5162-L5171] |
| 24 | [docs/registry/implementation-map.yaml:L5172-L5181] |
| 25 | [docs/registry/implementation-map.yaml:L5182-L5191] |
| 26 | [docs/registry/implementation-map.yaml:L5192-L5201] |
| 27 | [docs/registry/implementation-map.yaml:L5202-L5211] |
| 28 | [docs/registry/implementation-map.yaml:L5212-L5223] |
| 29 | [docs/registry/implementation-map.yaml:L5224-L5233] |
| 30 | [docs/registry/implementation-map.yaml:L5234-L5243] |
| 31 | [docs/registry/implementation-map.yaml:L5244-L5253] |
| 32 | [docs/registry/implementation-map.yaml:L5254-L5263] |
| 33 | [docs/registry/implementation-map.yaml:L5264-L5273] |
| 34 | [docs/registry/implementation-map.yaml:L5274-L5283] |
| 35 | [docs/registry/implementation-map.yaml:L5284-L5293] |
| 36 | [docs/registry/implementation-map.yaml:L5294-L5303] |
| 37 | [docs/registry/implementation-map.yaml:L5304-L5313] |
| 38 | [docs/registry/implementation-map.yaml:L5314-L5323] |
| 39 | [docs/registry/implementation-map.yaml:L5324-L5333] |
| 40 | [docs/registry/implementation-map.yaml:L5334-L5343] |
| 41 | [docs/registry/implementation-map.yaml:L5344-L5353] |
| 42 | [docs/registry/implementation-map.yaml:L5354-L5363] |
| 43 | [docs/registry/implementation-map.yaml:L5364-L5372] |
| 44 | [docs/registry/implementation-map.yaml:L5373-L5382] |
| 45 | [docs/registry/implementation-map.yaml:L5383-L5392] |
| 46 | [docs/registry/implementation-map.yaml:L5393-L5402] |
| 47 | [docs/registry/implementation-map.yaml:L5403-L5415] |
| 48 | [docs/registry/implementation-map.yaml:L5416-L5425] |
| 49 | [docs/registry/implementation-map.yaml:L5426-L5437] |
| 50 | [docs/registry/implementation-map.yaml:L5438-L5447] |
| 51 | [docs/registry/implementation-map.yaml:L5448-L5457] |
| 52 | [docs/registry/implementation-map.yaml:L5458-L5467] |
| 53 | [docs/registry/implementation-map.yaml:L5468-L5477] |
| 54 | [docs/registry/implementation-map.yaml:L5478-L5487] |
| 55 | [docs/registry/implementation-map.yaml:L5488-L5497] |
| 56 | [docs/registry/implementation-map.yaml:L5498-L5507] |
| 57 | [docs/registry/implementation-map.yaml:L5508-L5519] |
| 58 | [docs/registry/implementation-map.yaml:L5520-L5531] |
| 59 | [docs/registry/implementation-map.yaml:L5532-L5540] |
| 60 | [docs/registry/implementation-map.yaml:L5541-L5550] |
| 61 | [docs/registry/implementation-map.yaml:L5551-L5560] |
| 62 | [docs/registry/implementation-map.yaml:L5561-L5570] |
| 63 | [docs/registry/implementation-map.yaml:L5571-L5580] |
| 64 | [docs/registry/implementation-map.yaml:L5581-L5593] |
| 65 | [docs/registry/implementation-map.yaml:L5594-L5603] |
| 66 | [docs/registry/implementation-map.yaml:L5604-L5615] |
| 67 | [docs/registry/implementation-map.yaml:L5616-L5628] |
| 68 | [docs/registry/implementation-map.yaml:L5629-L5638] |
| 69 | [docs/registry/implementation-map.yaml:L5639-L5648] |
| 70 | [docs/registry/implementation-map.yaml:L5649-L5658] |
| 71 | [docs/registry/implementation-map.yaml:L5659-L5667] |
| 72 | [docs/registry/implementation-map.yaml:L5668-L5677] |
| 73 | [docs/registry/implementation-map.yaml:L5678-L5687] |
| 74 | [docs/registry/implementation-map.yaml:L5688-L5697] |
| 75 | [docs/registry/implementation-map.yaml:L5698-L5707] |
| 76 | [docs/registry/implementation-map.yaml:L5708-L5717] |
| 77 | [docs/registry/implementation-map.yaml:L5718-L5727] |
| 78 | [docs/registry/implementation-map.yaml:L5728-L5737] |
| 79 | [docs/registry/implementation-map.yaml:L5738-L5749] |
| 80 | [docs/registry/implementation-map.yaml:L5750-L5759] |
| 81 | [docs/registry/implementation-map.yaml:L5760-L5769] |
| 82 | [docs/registry/implementation-map.yaml:L5770-L5781] |
| 83 | [docs/registry/implementation-map.yaml:L5782-L5791] |
| 84 | [docs/registry/implementation-map.yaml:L5792-L5801] |
| 85 | [docs/registry/implementation-map.yaml:L5802-L5811] |
| 86 | [docs/registry/implementation-map.yaml:L5812-L5823] |
| 87 | [docs/registry/implementation-map.yaml:L5824-L5833] |
| 88 | [docs/registry/implementation-map.yaml:L5834-L5843] |
| 89 | [docs/registry/implementation-map.yaml:L5844-L5853] |
| 90 | [docs/registry/implementation-map.yaml:L5854-L5863] |
| 91 | [docs/registry/implementation-map.yaml:L5864-L5872] |
| 92 | [docs/registry/implementation-map.yaml:L5873-L5882] |
| 93 | [docs/registry/implementation-map.yaml:L5883-L5891] |
| 94 | [docs/registry/implementation-map.yaml:L5892-L5901] |
| 95 | [docs/registry/implementation-map.yaml:L5902-L5911] |
| 96 | [docs/registry/implementation-map.yaml:L5912-L5921] |
| 97 | [docs/registry/implementation-map.yaml:L5922-L5933] |
| 98 | [docs/registry/implementation-map.yaml:L5934-L5943] |
| 99 | [docs/registry/implementation-map.yaml:L5944-L5953] |
| 100 | [docs/registry/implementation-map.yaml:L5954-L5965] |
| 101 | [docs/registry/implementation-map.yaml:L5966-L5975] |
| 102 | [docs/registry/implementation-map.yaml:L5976-L5985] |
| 103 | [docs/registry/implementation-map.yaml:L5986-L5994] |
| 104 | [docs/registry/implementation-map.yaml:L5995-L6004] |
| 105 | [docs/registry/implementation-map.yaml:L6005-L6014] |
| 106 | [docs/registry/implementation-map.yaml:L6015-L6024] |
| 107 | [docs/registry/implementation-map.yaml:L6025-L6034] |
| 108 | [docs/registry/implementation-map.yaml:L6035-L6044] |
| 109 | [docs/registry/implementation-map.yaml:L6045-L6054] |
| 110 | [docs/registry/implementation-map.yaml:L6055-L6064] |
| 111 | [docs/registry/implementation-map.yaml:L6065-L6074] |
| 112 | [docs/registry/implementation-map.yaml:L6075-L6084] |
| 113 | [docs/registry/implementation-map.yaml:L6085-L6096] |
| 114 | [docs/registry/implementation-map.yaml:L6097-L6109] |
| 115 | [docs/registry/implementation-map.yaml:L6110-L6119] |
| 116 | [docs/registry/implementation-map.yaml:L6120-L6129] |
| 117 | [docs/registry/implementation-map.yaml:L6130-L6141] |
| 118 | [docs/registry/implementation-map.yaml:L6142-L6153] |
| 119 | [docs/registry/implementation-map.yaml:L6154-L6163] |
| 120 | [docs/registry/implementation-map.yaml:L6164-L6173] |
| 121 | [docs/registry/implementation-map.yaml:L6174-L6183] |
| 122 | [docs/registry/implementation-map.yaml:L6184-L6193] |
| 123 | [docs/registry/implementation-map.yaml:L6194-L6203] |
| 124 | [docs/registry/implementation-map.yaml:L6204-L6213] |
| 125 | [docs/registry/implementation-map.yaml:L6214-L6223] |
| 126 | [docs/registry/implementation-map.yaml:L6224-L6233] |
| 127 | [docs/registry/implementation-map.yaml:L6234-L6243] |
| 128 | [docs/registry/implementation-map.yaml:L6244-L6253] |
| 129 | [docs/registry/implementation-map.yaml:L6254-L6263] |
| 130 | [docs/registry/implementation-map.yaml:L6264-L6273] |
| 131 | [docs/registry/implementation-map.yaml:L6274-L6283] |
| 132 | [docs/registry/implementation-map.yaml:L6284-L6293] |
| 133 | [docs/registry/implementation-map.yaml:L6294-L6303] |
| 134 | [docs/registry/implementation-map.yaml:L6304-L6313] |
| 135 | [docs/registry/implementation-map.yaml:L6314-L6323] |
| 136 | [docs/registry/implementation-map.yaml:L6324-L6333] |
| 137 | [docs/registry/implementation-map.yaml:L6334-L6343] |
| 138 | [docs/registry/implementation-map.yaml:L6344-L6353] |
| 139 | [docs/registry/implementation-map.yaml:L6354-L6363] |
| 140 | [docs/registry/implementation-map.yaml:L6364-L6373] |
| 141 | [docs/registry/implementation-map.yaml:L6374-L6383] |
| 142 | [docs/registry/implementation-map.yaml:L6384-L6393] |
| 143 | [docs/registry/implementation-map.yaml:L6394-L6403] |
| 144 | [docs/registry/implementation-map.yaml:L6404-L6413] |
| 145 | [docs/registry/implementation-map.yaml:L6414-L6423] |
| 146 | [docs/registry/implementation-map.yaml:L6424-L6436] |
| 147 | [docs/registry/implementation-map.yaml:L6437-L6448] |
| 148 | [docs/registry/implementation-map.yaml:L6449-L6458] |
| 149 | [docs/registry/implementation-map.yaml:L6459-L6468] |
| 150 | [docs/registry/implementation-map.yaml:L6469-L6478] |
| 151 | [docs/registry/implementation-map.yaml:L6479-L6490] |
| 152 | [docs/registry/implementation-map.yaml:L6491-L6500] |
| 153 | [docs/registry/implementation-map.yaml:L6501-L6510] |
| 154 | [docs/registry/implementation-map.yaml:L6511-L6520] |
| 155 | [docs/registry/implementation-map.yaml:L6521-L6530] |
| 156 | [docs/registry/implementation-map.yaml:L6531-L6540] |
| 157 | [docs/registry/implementation-map.yaml:L6541-L6550] |
| 158 | [docs/registry/implementation-map.yaml:L6551-L6560] |
| 159 | [docs/registry/implementation-map.yaml:L6561-L6570] |
| 160 | [docs/registry/implementation-map.yaml:L6571-L6580] |
| 161 | [docs/registry/implementation-map.yaml:L6581-L6590] |
| 162 | [docs/registry/implementation-map.yaml:L6591-L6600] |
| 163 | [docs/registry/implementation-map.yaml:L6601-L6610] |
| 164 | [docs/registry/implementation-map.yaml:L6611-L6620] |
| 165 | [docs/registry/implementation-map.yaml:L6621-L6630] |
| 166 | [docs/registry/implementation-map.yaml:L6631-L6640] |
| 167 | [docs/registry/implementation-map.yaml:L6641-L6650] |
| 168 | [docs/registry/implementation-map.yaml:L6651-L6659] |
| 169 | [docs/registry/implementation-map.yaml:L6660-L6672] |
| 170 | [docs/registry/implementation-map.yaml:L6673-L6682] |
| 171 | [docs/registry/implementation-map.yaml:L6683-L6692] |
| 172 | [docs/registry/implementation-map.yaml:L6693-L6704] |
| 173 | [docs/registry/implementation-map.yaml:L6705-L6714] |
| 174 | [docs/registry/implementation-map.yaml:L6715-L6724] |
| 175 | [docs/registry/implementation-map.yaml:L6725-L6734] |
| 176 | [docs/registry/implementation-map.yaml:L6735-L6744] |
| 177 | [docs/registry/implementation-map.yaml:L6745-L6754] |
| 178 | [docs/registry/implementation-map.yaml:L6755-L6764] |
| 179 | [docs/registry/implementation-map.yaml:L6765-L6774] |
| 180 | [docs/registry/implementation-map.yaml:L6775-L6784] |
| 181 | [docs/registry/implementation-map.yaml:L6785-L6794] |
| 182 | [docs/registry/implementation-map.yaml:L6795-L6804] |
| 183 | [docs/registry/implementation-map.yaml:L6805-L6814] |
| 184 | [docs/registry/implementation-map.yaml:L6815-L6824] |
| 185 | [docs/registry/implementation-map.yaml:L6825-L6834] |
| 186 | [docs/registry/implementation-map.yaml:L6835-L6844] |
| 187 | [docs/registry/implementation-map.yaml:L6845-L6854] |
| 188 | [docs/registry/implementation-map.yaml:L6855-L6864] |
| 189 | [docs/registry/implementation-map.yaml:L6865-L6874] |
| 190 | [docs/registry/implementation-map.yaml:L6875-L6884] |
| 191 | [docs/registry/implementation-map.yaml:L6885-L6894] |
| 192 | [docs/registry/implementation-map.yaml:L6895-L6904] |
| 193 | [docs/registry/implementation-map.yaml:L6905-L6914] |
| 194 | [docs/registry/implementation-map.yaml:L6915-L6927] |
| 195 | [docs/registry/implementation-map.yaml:L6928-L6937] |
| 196 | [docs/registry/implementation-map.yaml:L6938-L6947] |
| 197 | [docs/registry/implementation-map.yaml:L6948-L6957] |
| 198 | [docs/registry/implementation-map.yaml:L6958-L6967] |
| 199 | [docs/registry/implementation-map.yaml:L6968-L6977] |
| 200 | [docs/registry/implementation-map.yaml:L6978-L6987] |
| 201 | [docs/registry/implementation-map.yaml:L6988-L6997] |
| 202 | [docs/registry/implementation-map.yaml:L6998-L7007] |
| 203 | [docs/registry/implementation-map.yaml:L7008-L7017] |
| 204 | [docs/registry/implementation-map.yaml:L7018-L7031] |
| 205 | [docs/registry/implementation-map.yaml:L7032-L7041] |
| 206 | [docs/registry/implementation-map.yaml:L7042-L7051] |
| 207 | [docs/registry/implementation-map.yaml:L7052-L7061] |
| 208 | [docs/registry/implementation-map.yaml:L7062-L7071] |
| 209 | [docs/registry/implementation-map.yaml:L7072-L7081] |
| 210 | [docs/registry/implementation-map.yaml:L7082-L7091] |
| 211 | [docs/registry/implementation-map.yaml:L7092-L7101] |
| 212 | [docs/registry/implementation-map.yaml:L7102-L7111] |
| 213 | [docs/registry/implementation-map.yaml:L7112-L7121] |
| 214 | [docs/registry/implementation-map.yaml:L7122-L7131] |
| 215 | [docs/registry/implementation-map.yaml:L7132-L7141] |
| 216 | [docs/registry/implementation-map.yaml:L7142-L7153] |
| 217 | [docs/registry/implementation-map.yaml:L7154-L7163] |
| 218 | [docs/registry/implementation-map.yaml:L7164-L7176] |
| 219 | [docs/registry/implementation-map.yaml:L7177-L7186] |
| 220 | [docs/registry/implementation-map.yaml:L7187-L7196] |
| 221 | [docs/registry/implementation-map.yaml:L7197-L7207] |
| 222 | [docs/registry/implementation-map.yaml:L7208-L7217] |
| 223 | [docs/registry/implementation-map.yaml:L7218-L7227] |
| 224 | [docs/registry/implementation-map.yaml:L7228-L7237] |
| 225 | [docs/registry/implementation-map.yaml:L7238-L7247] |
| 226 | [docs/registry/implementation-map.yaml:L7248-L7257] |
| 227 | [docs/registry/implementation-map.yaml:L7258-L7269] |
| 228 | [docs/registry/implementation-map.yaml:L7270-L7279] |
| 229 | [docs/registry/implementation-map.yaml:L7280-L7289] |
| 230 | [docs/registry/implementation-map.yaml:L7290-L7299] |
| 231 | [docs/registry/implementation-map.yaml:L7300-L7309] |
| 232 | [docs/registry/implementation-map.yaml:L7310-L7319] |
| 233 | [docs/registry/implementation-map.yaml:L7320-L7329] |
| 234 | [docs/registry/implementation-map.yaml:L7330-L7339] |
| 235 | [docs/registry/implementation-map.yaml:L7340-L7349] |
| 236 | [docs/registry/implementation-map.yaml:L7350-L7359] |
| 237 | [docs/registry/implementation-map.yaml:L7360-L7369] |
| 238 | [docs/registry/implementation-map.yaml:L7370-L7379] |
| 239 | [docs/registry/implementation-map.yaml:L7380-L7389] |
| 240 | [docs/registry/implementation-map.yaml:L7390-L7399] |
| 241 | [docs/registry/implementation-map.yaml:L7400-L7409] |
| 242 | [docs/registry/implementation-map.yaml:L7410-L7419] |
| 243 | [docs/registry/implementation-map.yaml:L7420-L7429] |
| 244 | [docs/registry/implementation-map.yaml:L7430-L7439] |
| 245 | [docs/registry/implementation-map.yaml:L7440-L7449] |
| 246 | [docs/registry/implementation-map.yaml:L7450-L7459] |
| 247 | [docs/registry/implementation-map.yaml:L7460-L7469] |
| 248 | [docs/registry/implementation-map.yaml:L7470-L7479] |
| 249 | [docs/registry/implementation-map.yaml:L7480-L7489] |
| 250 | [docs/registry/implementation-map.yaml:L7490-L7499] |
| 251 | [docs/registry/implementation-map.yaml:L7500-L7509] |
| 252 | [docs/registry/implementation-map.yaml:L7510-L7519] |
| 253 | [docs/registry/implementation-map.yaml:L7520-L7529] |
| 254 | [docs/registry/implementation-map.yaml:L7530-L7539] |
| 255 | [docs/registry/implementation-map.yaml:L7540-L7551] |
| 256 | [docs/registry/implementation-map.yaml:L7552-L7561] |
| 257 | [docs/registry/implementation-map.yaml:L7562-L7571] |
| 258 | [docs/registry/implementation-map.yaml:L7572-L7581] |
| 259 | [docs/registry/implementation-map.yaml:L7582-L7591] |
| 260 | [docs/registry/implementation-map.yaml:L7592-L7603] |
| 261 | [docs/registry/implementation-map.yaml:L7604-L7613] |
| 262 | [docs/registry/implementation-map.yaml:L7614-L7626] |
| 263 | [docs/registry/implementation-map.yaml:L7627-L7636] |
| 264 | [docs/registry/implementation-map.yaml:L7637-L7646] |
| 265 | [docs/registry/implementation-map.yaml:L7647-L7656] |
| 266 | [docs/registry/implementation-map.yaml:L7657-L7666] |
| 267 | [docs/registry/implementation-map.yaml:L7667-L7676] |
| 268 | [docs/registry/implementation-map.yaml:L7677-L7686] |
| 269 | [docs/registry/implementation-map.yaml:L7687-L7696] |
| 270 | [docs/registry/implementation-map.yaml:L7697-L7706] |
| 271 | [docs/registry/implementation-map.yaml:L7707-L7716] |
| 272 | [docs/registry/implementation-map.yaml:L7717-L7729] |
| 273 | [docs/registry/implementation-map.yaml:L7730-L7739] |
| 274 | [docs/registry/implementation-map.yaml:L7740-L7749] |
| 275 | [docs/registry/implementation-map.yaml:L7750-L7762] |
| 276 | [docs/registry/implementation-map.yaml:L7763-L7774] |
| 277 | [docs/registry/implementation-map.yaml:L7775-L7784] |
| 278 | [docs/registry/implementation-map.yaml:L7785-L7794] |
| 279 | [docs/registry/implementation-map.yaml:L7795-L7804] |
| 280 | [docs/registry/implementation-map.yaml:L7805-L7816] |
| 281 | [docs/registry/implementation-map.yaml:L7817-L7826] |
| 282 | [docs/registry/implementation-map.yaml:L7827-L7836] |
| 283 | [docs/registry/implementation-map.yaml:L7837-L7846] |
| 284 | [docs/registry/implementation-map.yaml:L7847-L7856] |
| 285 | [docs/registry/implementation-map.yaml:L7857-L7866] |
| 286 | [docs/registry/implementation-map.yaml:L7867-L7878] |
| 287 | [docs/registry/implementation-map.yaml:L7879-L7888] |
| 288 | [docs/registry/implementation-map.yaml:L7889-L7898] |
| 289 | [docs/registry/implementation-map.yaml:L7899-L7908] |
| 290 | [docs/registry/implementation-map.yaml:L7909-L7921] |
| 291 | [docs/registry/implementation-map.yaml:L7922-L7931] |
| 292 | [docs/registry/implementation-map.yaml:L7932-L7941] |
| 293 | [docs/registry/implementation-map.yaml:L7942-L7951] |
| 294 | [docs/registry/implementation-map.yaml:L7952-L7961] |
| 295 | [docs/registry/implementation-map.yaml:L7962-L7971] |
| 296 | [docs/registry/implementation-map.yaml:L7972-L7981] |
| 297 | [docs/registry/implementation-map.yaml:L7982-L7991] |
| 298 | [docs/registry/implementation-map.yaml:L7992-L8001] |
| 299 | [docs/registry/implementation-map.yaml:L8002-L8011] |
| 300 | [docs/registry/implementation-map.yaml:L8012-L8021] |
| 301 | [docs/registry/implementation-map.yaml:L8022-L8031] |
| 302 | [docs/registry/implementation-map.yaml:L8032-L8043] |
| 303 | [docs/registry/implementation-map.yaml:L8044-L8053] |
| 304 | [docs/registry/implementation-map.yaml:L8054-L8063] |
| 305 | [docs/registry/implementation-map.yaml:L8064-L8073] |
| 306 | [docs/registry/implementation-map.yaml:L8074-L8083] |
| 307 | [docs/registry/implementation-map.yaml:L8084-L8093] |
| 308 | [docs/registry/implementation-map.yaml:L8094-L8103] |
| 309 | [docs/registry/implementation-map.yaml:L8104-L8113] |
| 310 | [docs/registry/implementation-map.yaml:L8114-L8123] |
| 311 | [docs/registry/implementation-map.yaml:L8124-L8133] |
| 312 | [docs/registry/implementation-map.yaml:L8134-L8143] |
| 313 | [docs/registry/implementation-map.yaml:L8144-L8155] |
| 314 | [docs/registry/implementation-map.yaml:L8156-L8165] |
| 315 | [docs/registry/implementation-map.yaml:L8166-L8175] |
| 316 | [docs/registry/implementation-map.yaml:L8176-L8185] |
| 317 | [docs/registry/implementation-map.yaml:L8186-L8195] |
| 318 | [docs/registry/implementation-map.yaml:L8196-L8205] |
| 319 | [docs/registry/implementation-map.yaml:L8206-L8215] |
| 320 | [docs/registry/implementation-map.yaml:L8216-L8227] |
| 321 | [docs/registry/implementation-map.yaml:L8228-L8237] |
| 322 | [docs/registry/implementation-map.yaml:L8238-L8247] |
| 323 | [docs/registry/implementation-map.yaml:L8248-L8257] |
| 324 | [docs/registry/implementation-map.yaml:L8258-L8267] |
| 325 | [docs/registry/implementation-map.yaml:L8268-L8279] |
| 326 | [docs/registry/implementation-map.yaml:L8280-L8290] |
| 327 | [docs/registry/implementation-map.yaml:L8291-L8300] |
| 328 | [docs/registry/implementation-map.yaml:L8301-L8310] |
| 329 | [docs/registry/implementation-map.yaml:L8311-L8323] |
| 330 | [docs/registry/implementation-map.yaml:L8324-L8333] |
| 331 | [docs/registry/implementation-map.yaml:L8334-L8343] |
| 332 | [docs/registry/implementation-map.yaml:L8344-L8353] |
| 333 | [docs/registry/implementation-map.yaml:L8354-L8363] |
| 334 | [docs/registry/implementation-map.yaml:L8364-L8373] |
| 335 | [docs/registry/implementation-map.yaml:L8374-L8383] |
| 336 | [docs/registry/implementation-map.yaml:L8384-L8393] |
| 337 | [docs/registry/implementation-map.yaml:L8394-L8403] |
| 338 | [docs/registry/implementation-map.yaml:L8404-L8413] |
| 339 | [docs/registry/implementation-map.yaml:L8414-L8423] |
| 340 | [docs/registry/implementation-map.yaml:L8424-L8435] |
| 341 | [docs/registry/implementation-map.yaml:L8436-L8445] |
| 342 | [docs/registry/implementation-map.yaml:L8446-L8457] |
| 343 | [docs/registry/implementation-map.yaml:L8458-L8470] |
| 344 | [docs/registry/implementation-map.yaml:L8471-L8480] |
| 345 | [docs/registry/implementation-map.yaml:L8481-L8492] |
| 346 | [docs/registry/implementation-map.yaml:L8493-L8505] |
| 347 | [docs/registry/implementation-map.yaml:L8506-L8517] |
| 348 | [docs/registry/implementation-map.yaml:L8518-L8527] |
| 349 | [docs/registry/implementation-map.yaml:L8528-L8537] |
| 350 | [docs/registry/implementation-map.yaml:L8538-L8547] |
| 351 | [docs/registry/implementation-map.yaml:L8548-L8560] |
| 352 | [docs/registry/implementation-map.yaml:L8561-L8572] |
| 353 | [docs/registry/implementation-map.yaml:L8573-L8582] |
| 354 | [docs/registry/implementation-map.yaml:L8583-L8596] |
| 355 | [docs/registry/implementation-map.yaml:L8597-L8608] |
| 356 | [docs/registry/implementation-map.yaml:L8609-L8619] |
| 357 | [docs/registry/implementation-map.yaml:L8620-L8632] |
| 358 | [docs/registry/implementation-map.yaml:L8633-L8647] |
| 359 | [docs/registry/implementation-map.yaml:L8648-L8657] |
| 360 | [docs/registry/implementation-map.yaml:L8658-L8667] |
| 361 | [docs/registry/implementation-map.yaml:L8668-L8677] |
| 362 | [docs/registry/implementation-map.yaml:L8678-L8687] |
| 363 | [docs/registry/implementation-map.yaml:L8688-L8701] |
| 364 | [docs/registry/implementation-map.yaml:L8702-L8714] |
| 365 | [docs/registry/implementation-map.yaml:L8715-L8724] |
| 366 | [docs/registry/implementation-map.yaml:L8725-L8739] |
| 367 | [docs/registry/implementation-map.yaml:L8740-L8749] |
| 368 | [docs/registry/implementation-map.yaml:L8750-L8759] |
| 369 | [docs/registry/implementation-map.yaml:L8760-L8773] |
| 370 | [docs/registry/implementation-map.yaml:L8774-L8783] |
| 371 | [docs/registry/implementation-map.yaml:L8784-L8793] |
| 372 | [docs/registry/implementation-map.yaml:L8794-L8803] |
| 373 | [docs/registry/implementation-map.yaml:L8804-L8813] |
| 374 | [docs/registry/implementation-map.yaml:L8814-L8823] |
| 375 | [docs/registry/implementation-map.yaml:L8824-L8833] |
| 376 | [docs/registry/implementation-map.yaml:L8834-L8843] |
| 377 | [docs/registry/implementation-map.yaml:L8844-L8856] |
| 378 | [docs/registry/implementation-map.yaml:L8857-L8866] |
| 379 | [docs/registry/implementation-map.yaml:L8867-L8876] |
| 380 | [docs/registry/implementation-map.yaml:L8877-L8886] |
| 381 | [docs/registry/implementation-map.yaml:L8887-L8896] |
| 382 | [docs/registry/implementation-map.yaml:L8897-L8908] |
| 383 | [docs/registry/implementation-map.yaml:L8909-L8918] |
| 384 | [docs/registry/implementation-map.yaml:L8919-L8928] |
| 385 | [docs/registry/implementation-map.yaml:L8929-L8938] |
| 386 | [docs/registry/implementation-map.yaml:L8939-L8948] |
| 387 | [docs/registry/implementation-map.yaml:L8949-L8958] |
| 388 | [docs/registry/implementation-map.yaml:L8959-L8970] |
| 389 | [docs/registry/implementation-map.yaml:L8971-L8980] |
| 390 | [docs/registry/implementation-map.yaml:L8981-L8990] |
| 391 | [docs/registry/implementation-map.yaml:L8991-L9000] |
| 392 | [docs/registry/implementation-map.yaml:L9001-L9010] |
| 393 | [docs/registry/implementation-map.yaml:L9011-L9020] |
| 394 | [docs/registry/implementation-map.yaml:L9021-L9030] |
| 395 | [docs/registry/implementation-map.yaml:L9031-L9040] |
| 396 | [docs/registry/implementation-map.yaml:L9041-L9050] |
| 397 | [docs/registry/implementation-map.yaml:L9051-L9060] |
| 398 | [docs/registry/implementation-map.yaml:L9061-L9070] |
| 399 | [docs/registry/implementation-map.yaml:L9071-L9080] |
| 400 | [docs/registry/implementation-map.yaml:L9081-L9090] |
| 401 | [docs/registry/implementation-map.yaml:L9091-L9100] |
| 402 | [docs/registry/implementation-map.yaml:L9101-L9110] |
| 403 | [docs/registry/implementation-map.yaml:L9111-L9120] |
| 404 | [docs/registry/implementation-map.yaml:L9121-L9130] |
| 405 | [docs/registry/implementation-map.yaml:L9131-L9140] |
| 406 | [docs/registry/implementation-map.yaml:L9141-L9152] |
| 407 | [docs/registry/implementation-map.yaml:L9153-L9162] |
| 408 | [docs/registry/implementation-map.yaml:L9163-L9172] |
| 409 | [docs/registry/implementation-map.yaml:L9173-L9182] |
| 410 | [docs/registry/implementation-map.yaml:L9183-L9192] |
| 411 | [docs/registry/implementation-map.yaml:L9193-L9202] |
| 412 | [docs/registry/implementation-map.yaml:L9203-L9212] |
| 413 | [docs/registry/implementation-map.yaml:L9213-L9222] |
| 414 | [docs/registry/implementation-map.yaml:L9223-L9232] |
| 415 | [docs/registry/implementation-map.yaml:L9233-L9242] |
| 416 | [docs/registry/implementation-map.yaml:L9243-L9252] |
| 417 | [docs/registry/implementation-map.yaml:L9253-L9262] |
| 418 | [docs/registry/implementation-map.yaml:L9263-L9272] |
| 419 | [docs/registry/implementation-map.yaml:L9273-L9282] |
| 420 | [docs/registry/implementation-map.yaml:L9283-L9292] |
| 421 | [docs/registry/implementation-map.yaml:L9293-L9302] |
| 422 | [docs/registry/implementation-map.yaml:L9303-L9312] |
| 423 | [docs/registry/implementation-map.yaml:L9313-L9322] |
| 424 | [docs/registry/implementation-map.yaml:L9323-L9332] |
| 425 | [docs/registry/implementation-map.yaml:L9333-L9342] |
| 426 | [docs/registry/implementation-map.yaml:L9343-L9352] |
| 427 | [docs/registry/implementation-map.yaml:L9353-L9362] |
| 428 | [docs/registry/implementation-map.yaml:L9363-L9372] |
| 429 | [docs/registry/implementation-map.yaml:L9373-L9382] |
| 430 | [docs/registry/implementation-map.yaml:L9383-L9392] |
| 431 | [docs/registry/implementation-map.yaml:L9393-L9402] |
| 432 | [docs/registry/implementation-map.yaml:L9403-L9412] |
| 433 | [docs/registry/implementation-map.yaml:L9413-L9422] |
| 434 | [docs/registry/implementation-map.yaml:L9423-L9432] |
| 435 | [docs/registry/implementation-map.yaml:L9433-L9442] |
| 436 | [docs/registry/implementation-map.yaml:L9443-L9452] |
| 437 | [docs/registry/implementation-map.yaml:L9453-L9462] |
| 438 | [docs/registry/implementation-map.yaml:L9463-L9472] |
| 439 | [docs/registry/implementation-map.yaml:L9473-L9482] |
| 440 | [docs/registry/implementation-map.yaml:L9483-L9493] |

### Exact-signal audit

The audit used Python `re` case-sensitively on every package `.py` file at the code pin, using the map's exact `search_signals`; the vocabulary pattern was excluded because it matches the package path case-insensitively. The two retained patterns were not unescaped or normalised. [docs/registry/implementation-map.yaml:L4936-L4938]
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
Map follow-up: classify format detection, include merging, parameter projection and reverse specification extraction more precisely than the broad writer family; issue pending (4).

## 10. Coverage and open items

Read scope: all twelve Python files in the package; all nine Python files in its test directory, including fixtures and the empty test initializer. Source methods and test assertions are cited in sections 1–6. [src/digitalmodel/solvers/orcaflex/format_converter/__init__.py:L9-L29] [tests/solvers/orcaflex/format_converter/conftest.py:L1-L115]
Additional documentation read: `docs/domains/orcaflex/QUICKSTART_FILE_CONVERSION.md`, `SEMANTIC_DIFF_TAXONOMY.md`, `SEMANTIC_EQUIVALENCE_CLAIM_BOUNDARY.md` in that domain directory, `docs/cad-inventory/ecosystem-and-automation-surface.md`, and the public conversion validation plan; one further discovery file has an identifying name and are not reproduced here (codenaming: issue pending (4)). [docs/domains/orcaflex/QUICKSTART_FILE_CONVERSION.md:L295-L350] [docs/domains/orcaflex/SEMANTIC_DIFF_TAXONOMY.md:L1-L20] [docs/domains/orcaflex/SEMANTIC_EQUIVALENCE_CLAIM_BOUNDARY.md:L1-L25] [docs/cad-inventory/ecosystem-and-automation-surface.md:L78-L83]
The calculation-citation contract was read as guidance; this pack introduces no standards-derived calculation constant and no private sidecar values.

| Item | Remaining qualification / responsible discipline | Tracking |
| --- | --- | --- |
| O1 | UNVERIFIED target versions, loadability, statics and dynamics; riser and mooring quality manager; execute C3 and downstream C4–C5 with an independently qualified model; C4–C5 do not establish conversion fidelity. | #280; regression corpus #2014 and #518 |
| O2 | UNVERIFIED units, coordinate basis, independent benchmark and governing standard / Citation applicability; engineering software and family quality manager; validated examples remain 0. | issue pending (2) |
| O3 | UNVERIFIED assigned deputy and availability; maintainers; approve the proposed review roles and shared-log destination. | issue pending (2) |
| O4 | Stale fixture path after the documentation move and all resulting NOT_RUN examples; engineering software; correct the path and run the 35 nodes below (no solver required). | #2008 |
| O5 | Partial merges, hidden omissions and incomplete negative detection tests S01–S06 and S25–S26; engineering software; implement and run negative tests, without changing this pack's code pin. | issue pending (1) |
| O6 | Reverse extraction boundaries S07–S09 and S11–S23; engineering software; qualify missing fields and multiple trains before any equivalence claim. | #520 |
| O7 | Accepted but unused CLI dry-run/batch flags; engineering software; define their behaviour and test absence of writes for dry-run. | issue pending (3) |
| O8 | Broad writer inventory classifications, repository-wide gaps and identifying discovery names; map maintainers; resolve codenames before expanding source inventories. | issue pending (4) |
| O9 | Existing duplicate submodules key in solvers registry; maintainers; consolidate independently so all parsed entries survive strict YAML loading. | issue pending (5) |

### Tests not executed beyond fixture setup

Each node below was collected and skipped because the pinned fixture guards use a stale fixture path under the deleted `docs/modules/orcaflex/` tree. The fixture files exist under `docs/domains/orcaflex/examples/` and `docs/domains/orcaflex/pipeline/`; these are NOT_RUN for the conversion assertions because of path drift, not missing reference data, solver or dependency skips. [tests/solvers/orcaflex/format_converter/conftest.py:L9-L55]

| Node ID | Result / tracking |
| --- | --- |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIDetect::test_detect_single_file` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_cli.py:L17-L17] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIDetect::test_detect_modular_dir` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_cli.py:L22-L22] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIDetect::test_detect_spec_file` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_cli.py:L27-L27] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLISingle2Modular::test_convert` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_cli.py:L41-L41] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLISingle2Modular::test_no_output_uses_default` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_cli.py:L48-L48] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIModular2Single::test_convert` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_cli.py:L61-L61] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLISpec2Modular::test_convert` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_cli.py:L72-L72] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLISingle2Spec::test_convert` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_cli.py:L83-L83] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIAuto::test_auto_single_to_modular` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_cli.py:L94-L94] |
| `tests/solvers/orcaflex/format_converter/test_cli.py::TestCLIAuto::test_auto_modular_to_single` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_cli.py:L102-L102] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_modular_directory` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_format_detector.py:L17-L17] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_modular_master_file` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_format_detector.py:L21-L21] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_single_file` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_format_detector.py:L25-L25] |
| `tests/solvers/orcaflex/format_converter/test_format_detector.py::TestFormatDetector::test_detect_spec_file` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_format_detector.py:L29-L29] |
| `tests/solvers/orcaflex/format_converter/test_modular_to_single.py::TestModularToSingleConverter::test_convert_real_a01_modular` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_modular_to_single.py:L114-L114] |
| `tests/solvers/orcaflex/format_converter/test_round_trip.py::TestRoundTrip::test_roundtrip_real_a01` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_round_trip.py:L56-L56] |
| `tests/solvers/orcaflex/format_converter/test_single_to_modular.py::TestSingleToModularConverter::test_convert_real_a01_file` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_single_to_modular.py:L103-L103] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_convert_creates_modular_structure` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L25-L25] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_convert_generates_includes` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L35-L35] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_report_metadata` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L56-L56] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToModularConverter::test_init_with_spec_path` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L71-L71] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToSingleConverter::test_convert_creates_single_file` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L88-L88] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToSingleConverter::test_output_has_orcaflex_sections` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L97-L97] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSpecToSingleConverter::test_report_metadata` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L110-L110] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_extract_environment_fields` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L147-L147] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_extract_simulation_fields` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L163-L163] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_confidence_score` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L175-L175] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_unmapped_sections_reported` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L184-L184] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_metadata_placeholder` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L193-L193] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_confidence_header_in_file` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L205-L205] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestModularToSpecConverter::test_from_directory` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L215-L215] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSingleToSpecConverter::test_convert_real_a01` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L476-L476] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSingleToSpecConverter::test_extracted_spec_has_environment` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L486-L486] |
| `tests/solvers/orcaflex/format_converter/test_spec_conversions.py::TestSingleToSpecConverter::test_report_metadata` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_spec_conversions.py:L500-L500] |
| `tests/solvers/orcaflex/format_converter/test_yaml_utils.py::TestOrcaFlexLoad::test_load_real_a01_file` | NOT_RUN: stale fixture path; #2008. [tests/solvers/orcaflex/format_converter/test_yaml_utils.py:L148-L148] |
