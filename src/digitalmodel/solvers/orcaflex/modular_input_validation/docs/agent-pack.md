# Modular input validation knowledge pack

| Header | Value |
| --- | --- |
| Module | solvers/orcaflex/modular_input_validation |
| Repository | digitalmodel |
| Path | src/digitalmodel/solvers/orcaflex/modular_input_validation |
| Pack revision | 1, 2026-10-15 |
| Code revision | d96aad6b05f0da8b01be3cc490277bf8080bf663 |
| Analysis family | OrcaFlex riser and mooring; MR |
| Quality manager role | OrcaFlex riser and mooring quality manager |
| Status | DRAFT |

Source citations resolve at the code revision above; implementation-map metadata uses the separate revision specified in section 9.

## 1. Purpose and scope

`ModularInputValidator.validate_file` coordinates YAML checks, optional API validation and physical comparisons, returning early on a Level 1 failure. [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L45-L85]

Level 1 parses YAML and checks the existence of directly extracted `includefile` references; the dependency-graph and cycle methods are separate helpers, absent from this validation path. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L28-L87] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L145-L222]

Level 2 constructs a model, calls `LoadData`, and collects warnings by calling `CalculateStatics`; it is a solver execution path, not an offline schema check. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L142-L176] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L230-L267]

Level 3 compares selected buoy, environment and synthetic line fields with CSV ranges and supplied reference values; absent paths or ranges can produce no check. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L265-L348] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L350-L511] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L544-L681]

The containing `solvers` domain is beta; the registry has no separate maturity declaration for this package at the code pin. [docs/registry/module-routing.yaml:L278-L288]

The scope includes all Python files and `reporters/`; adjacent generators, native analysis and diffraction qualification have separate responsibilities. [src/digitalmodel/solvers/orcaflex/modular_input_validation/__init__.py:L18-L29] [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L11-L16]

## 2. Inputs and outputs

### Inputs

| Input | Unit | Allowed range / default | Source and consumer |
| --- | --- | --- | --- |
| File, directory or list of paths | filesystem path | Existing file/directory; default directory pattern `*.yml`, nonrecursive `glob` | `run_validation`, `validate_directory`; [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L105-L134] [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L182-L217] |
| YAML / `includefile` | text; UTF-8 | PyYAML `safe_load`; exact lowercase string key; relative paths resolve against the containing file; absolute paths retained | `load_yaml_file`, `extract_includefiles`, `resolve_include_path`; [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L11-L59] [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L114-L132] |
| `tolerance_percent` | % | Greater than 0, at most 100; default 10.0 | `ValidationConfig`; [src/digitalmodel/solvers/orcaflex/modular_input_validation/config.py:L28-L30] [src/digitalmodel/solvers/orcaflex/modular_input_validation/config.py:L58-L60] |
| Level selection | count / Boolean | `skip_levels` accepts 1, 2, 3; default empty; `should_run_level` tests membership only | `enable_orcaflex` and `run_all_levels` do not enter that predicate; [src/digitalmodel/solvers/orcaflex/modular_input_validation/config.py:L31-L42] [src/digitalmodel/solvers/orcaflex/modular_input_validation/config.py:L62-L85] |
| Report selection | format / path | Default console, CSV, Markdown, HTML; reports and results directory defaults are relative | `ValidationConfig`; [src/digitalmodel/solvers/orcaflex/modular_input_validation/config.py:L34-L45] [src/digitalmodel/solvers/orcaflex/modular_input_validation/config.py:L69-L73] |
| Hull ranges CSV | same unit as input scalar; no conversion | `parameter`, optional `min_value`/`max_value`, `reference_basis`, `notes`; missing file returns empty ranges | `load_hull_geometry_ranges`, `_load_ranges_csv`; [src/digitalmodel/solvers/orcaflex/modular_input_validation/data_loader.py:L65-L74] [src/digitalmodel/solvers/orcaflex/modular_input_validation/data_loader.py:L225-L258] |
| Metocean ranges CSV | input scalar units | `condition`, paired `hs`, `tp`, `surface_current`, `wind_speed` minimum/maximum columns | Keys become condition + parameter; [src/digitalmodel/solvers/orcaflex/modular_input_validation/data_loader.py:L76-L113] |
| Mooring ranges CSV | input scalar units; safety factor dimensionless | `component`, capacity and safety-factor bounds; keys become component + category | `load_mooring_capacity_ranges`; [src/digitalmodel/solvers/orcaflex/modular_input_validation/data_loader.py:L115-L156] |
| Environmental / line CSV | input scalar units | Optional numeric fields become floats or `None`; records keyed by sea-state ID or line/segment ID | `load_environmental_conditions`, `load_mooring_line_properties`; [src/digitalmodel/solvers/orcaflex/modular_input_validation/data_loader.py:L158-L223] |
| YAML physical fields | no unit conversion | Hull: first buoy/cylinder diameter, mass and flat draft/displacement; environment: first wave train and flat fallbacks; capacities: flat `Line`/`Connector` fields | Extraction tables; [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L282-L295] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L367-L389] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L454-L463] |

UNVERIFIED: U1 — supported native versions, an enforced input unit system, coordinate origin/frame, sign and phase conventions are not established by these scalar readers; callers must supply a consistent basis. See section 10, issue pending (5).

The usage guide labels wave height and line dimensions in m, period in s, velocity in m/s and line breaking load in kN; these documentation conventions do not enforce YAML `UnitsSystem`. [docs/domains/orcaflex/DATA_LOADER_USAGE.md:L82-L124] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L398-L415]

### Outputs

| Output | Unit / domain | Producer and limitation |
| --- | --- | --- |
| `ValidationResult` | statuses: pass, warn, fail, unknown, skipped; elapsed s | Per-level results and issues; [src/digitalmodel/solvers/orcaflex/modular_input_validation/models.py:L20-L26] [src/digitalmodel/solvers/orcaflex/modular_input_validation/models.py:L147-L165] |
| Level 1 result | Boolean flags, module count, section/error lists | `Level1YAMLValidator.validate`; [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L38-L87] |
| Level 2 result | availability, version, object count, load/statics flags and warnings | `static_converged` defaults false while the validator assigns `static_analysis_passed`; [src/digitalmodel/solvers/orcaflex/modular_input_validation/models.py:L86-L107] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L109-L138] |
| Level 3 result | check count, critical/warning count, formatted scalar values | The validator updates `parameters_validated` and category lists; [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L163-L188] |
| Console / CSV / Markdown / HTML | text; duration s, issue counts | Timestamped report filenames; UTF-8 file writers; [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L136-L180] [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/csv_reporter.py:L44-L84] [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/markdown_reporter.py:L34-L46] [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/html_reporter.py:L35-L47] |
| `to_dict()` | dictionary with seconds and counts | Exports `checks_performed`, `checks_passed`, `static_converged`, not the alternate counters updated by validation; do not use it as complete gate evidence | [src/digitalmodel/solvers/orcaflex/modular_input_validation/models.py:L200-L233] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L163-L188] |

## 3. Method and governing standards

| Method | Implementing definition | Reference / algorithm and limits |
| --- | --- | --- |
| YAML parsing | `load_yaml_file` | UTF-8 plus `yaml.safe_load`; errors converted to a success/content/error tuple; no duplicate-key override or selector-order table in this function. [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L11-L30] |
| Direct include validation | `Level1YAMLValidator.validate` | Extract string include values recursively from the parsed object, resolve paths and require existence; missing includes yield FAIL. [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L33-L59] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L64-L85] |
| Dependency helpers | `build_dependency_graph`, `detect_circular_dependencies` | Visited-file traversal stops past depth 10; DFS uses a path stack for cycles; graph keys are file paths but edge strings remain raw include values. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L145-L222] |
| Physical content merge | `_load_and_merge_yaml`, `_deep_merge_dicts` | Loads direct includes, ignores unavailable/invalid includes, recursively merges dictionaries and concatenates two lists; non-dictionary overrides replace. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L192-L263] |
| Alternate dictionary merge | `merge_dicts_deep` | Recurses only for two dictionary values; lists are replaced, unlike the Level 3 merge. [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L201-L220] |
| Path probes | `_extract_path`, `_extract_first` | Dot-key traversal supports nonnegative list indices; first non-None fallback wins. Only index 0 is used in the physical mapping tables. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L45-L81] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L287-L295] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L374-L389] |
| Range test | `is_within_range` | Inclusive optional minimum/maximum; unbounded ends use `None`; comparison has no explicit finite-number rejection. [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L95-L111] |
| Percentage difference | `calculate_percentage_difference` | `(actual - expected) / abs(expected) * 100`; zero expected returns 100.0 for nonzero actual and 0.0 for zero actual. [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L62-L76] |
| Tolerance test | `is_within_tolerance` | Absolute percentage difference at most configured tolerance; this is code-defined behavior, not an independently qualified physical criterion. [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L79-L92] |
| Physical severity | `_validate_hull_geometry`, `_validate_metocean`, `_validate_mooring_capacity` | Out-of-range values generally warn; an out-of-range synthetic safety-factor field is critical. Matching capacity ranges uses any word of the parameter name and takes the first match. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L329-L346] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L408-L433] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L465-L509] |
| Reference comparisons | `_compare_environmental_conditions`, `_compare_line_properties` | Environment emits two checks only when both quantities match a reference within tolerance; line comparison emits only matching MBL records, reads but does not compare length. `WaveTz` can be compared with reference `tp`. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L561-L623] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L640-L681] |
| Native load and warnings | `_load_with_orcaflex`, `_get_model_warnings`, `_run_static_analysis` | `Model.LoadData` followed by warning collection that runs statics; static exceptions become warning strings; the final heuristic is not a hand-calculation reference. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L142-L176] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L230-L327] |
| Overall decision | `_determine_overall_status` | Level 1/2 FAIL or physical critical issues fail; warnings warn; otherwise any passed level can yield PASS despite another unknown/skipped level. [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L311-L362] |

UNVERIFIED: U2 — independent provenance, editions and clause support for the CSV reference bounds are not established; `reference_basis` is loaded as text without standards resolution. No reference bound is reproduced here. See section 10, issue pending (5). [src/digitalmodel/solvers/orcaflex/modular_input_validation/data_loader.py:L245-L250]

The reference-data documentation lists standards without supplying a complete edition/clause specification; clause text not checked against the licensed copy. No licensed standard value is adopted as a reference in this pack. [docs/domains/orcaflex/DATA_LOADER_VERIFICATION.md:L133-L139]

The percentage and range rules above use code-defined arithmetic; the reference-data loader stores provenance strings rather than emitting a `Citation` sidecar. [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L62-L111] [src/digitalmodel/solvers/orcaflex/modular_input_validation/data_loader.py:L13-L30]

## 4. Validated worked examples

Validated engineering examples: 0; regression examples: 4. Expectations below are test literals, not independent engineering references. [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L54-L85] [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L158-L165] [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L199-L212] [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L274-L282]

All results below were recorded offline on 2026-10-15 at BASE `c2fd516dec828ac9788731a408fc671643c5bad2` and on its uncommitted documentation update; the four test files and package Python source are unchanged from the header code pin.

| Case / class | Input and expected value | Tolerance / basis | Existing test node ID | BASE / after |
| --- | --- | --- | --- | --- |
| R01 missing include; regression | Fixture master references one absent include; status FAIL, 1 missing reference, includes unresolved | Exact status and count; test assertions | `tests/solvers/orcaflex/test_modular_input_validation_fixes.py::TestLevel1MissingInclude::test_missing_include_is_fail_not_warn` | PASS / PASS; [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L46-L65] |
| R02 indexed lookup; regression | First wave height 25.0 and first cylinder diameter 11, in input scalar units | Exact equality, 0 scalar-unit error; test literals | `tests/solvers/orcaflex/test_modular_input_validation_fixes.py::TestExtractPath::test_list_index_traversal` | PASS / PASS; [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L131-L165] |
| R03 range rejection; regression | Synthetic first wave height exceeds the injected bound; exactly 1 `operating_hs` check with `within_range=False` | Exact count and Boolean; injected reference tagged test | `tests/solvers/orcaflex/test_modular_input_validation_fixes.py::TestLevel3RealModelPaths::test_metocean_hs_out_of_range_flagged` | PASS / PASS; [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L199-L212] |
| R04 selection flag; regression | `--no-orcaflex` produces disabled flag and Level 2 in skip list | Exactly 1 required skip-list membership and false `should_run_level(2)` | `tests/solvers/orcaflex/test_modular_input_validation_fixes.py::TestNoOrcaflexFlag::test_no_orcaflex_skips_level_2` | PASS / PASS; configuration only, not constructor execution; [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L274-L282] |

The two orchestrator nodes skip Level 2 and run offline: the installed-distribution metadata confirms that the native interface package is absent, so the constructor's optional import takes its guarded `ImportError` path. No native interface was imported or queried. The unconditional constructor probe remains a defect under C04, issue pending (1). [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L88-L125] [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L37-L43] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L24-L55]

| Test node ID | BASE / after | Reason |
| --- | --- | --- |
| `tests/solvers/orcaflex/test_modular_input_validation_fixes.py::TestLevel1MissingInclude::test_validator_reports_issue_once_and_fails_overall` | PASS / PASS | Levels 2 and 3 skipped; absent native package handled by the constructor; [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L88-L106] |
| `tests/solvers/orcaflex/test_modular_input_validation_fixes.py::TestLevel1MissingInclude::test_cli_exits_nonzero_for_missing_include` | PASS / PASS | CLI skips native execution; absent native package handled by the constructor; [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L108-L125] [src/digitalmodel/solvers/orcaflex/modular_input_validation/cli.py:L243-L247] |

The legacy Level 1 test returns a Boolean when its fixture is missing, rather than asserting; its pytest PASS covers import and the early-return path only. The fixture is not exercised: `specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml` is absent at the pin (tree listing). The loader smoke test also returns false for zero loaded rows without asserting. Neither is a validated example. [tests/test_level1_minimal.py:L24-L43] [tests/test_level1_minimal.py:L107-L113] [tests/orcaflex/test_calm_data_loader.py:L141-L153]

Offline execution inventory: 10 utility nodes, 1 Level 1 smoke node, 2 loader smoke nodes and all 15 regression nodes passed at BASE and after: 28 run, 28 passed, 0 failed, 0 skipped, 0 NOT_RUN tests. [tests/test_validation_utils.py:L34-L428] [tests/test_level1_minimal.py:L24-L113] [tests/orcaflex/test_calm_data_loader.py:L19-L177] [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L54-L289]

Reproduction form: `PYTHONPATH=src uv run --no-sources python -m pytest <offline-node-IDs> -q -rA -p no:cacheprovider --basetemp=<scratch-directory>`; default repository addopts retained, no addopts override. Commands: the utility, Level 1 and regression files together (26 passed), then `tests/orcaflex/test_calm_data_loader.py` separately (2 passed); no node was excluded or patched.

## 5. QA gates

Gate applicability follows AQG r0, sections 3–10; criteria below are proposed module acceptance checks, not a claim that the module implements all gates. Checks with native or downstream evidence remain NOT_RUN until the appropriate evidence exists. (AQG r0, sections 4–10.)

| Gate | Applicability and current evidence |
| --- | --- |
| G0 | Applicable; partial offline evidence only. Parsing, direct includes and scalar range checks do not establish the complete schema, units, order or fingerprint gate. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L28-L87] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L107-L188] |
| G1 | Applicable; NOT_RUN. The module's OrcaFlex route is `Model()` then `LoadData(file_path)`; use a same-target-version demo where independently qualified, otherwise the licensed API route. This module's route also runs statics while gathering warnings. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L155-L170] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L240-L265] (AQG r0, section 5.) |
| G2 | Applicable downstream; NOT_RUN. Module warning collection calls statics, but does not provide the complete equilibrium, image, hand calculation, offset or component record. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L230-L327] (AQG r0, section 6.) |
| G3 | Applicable downstream; NOT_RUN. This package's native validation path stops at load/static findings. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L97-L140] (AQG r0, section 7.) |
| G4 | Conditional: record cited when input body data come from a new diffraction run; NOT_APPLICABLE to the synthetic scalar/unit fixtures here because they carry no new diffraction result. [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L131-L153] (AQG r0, sections 8 and 10.) |
| G5 | Applicable; report writers expose summaries but do not establish a signed QA appendix or input fingerprints. [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L136-L180] [src/digitalmodel/solvers/orcaflex/modular_input_validation/models.py:L200-L233] (AQG r0, section 9.) |

Every count below has unit count (dimensionless); Boolean/status comparisons have 0 allowed mismatches. Native evidence audits are manual checks of supplied records, executable without importing any native interface.

| ID / gate | Module-specific check and numeric pass criterion | Evidence and current disposition |
| --- | --- | --- |
| C01 / G0 | `Level1YAMLValidator.validate` on R01: 1 missing include, 0 syntax duplicates, FAIL and false include resolution; 0 mismatched assertions | Existing missing-include and nonduplication nodes; PASS. [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L58-L74] |
| C02 / G0 | `_extract_path` on R02: 0 scalar-unit error at both indexed paths; 0 incorrect missing-index defaults | Existing indexed and out-of-range nodes; PASS. [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L158-L175] |
| C03 / G0 | `_validate_metocean` on R03: 1 rejected height check; injected current case: 1 accepted current check; 0 count/Boolean mismatches | Existing metocean tests; PASS. [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L199-L227] |
| C04 / G0 | `create_config_from_args` on R04: Level 2 excluded, 0 selection mismatches; separately require 0 native-import attempts during an offline constructor | Selection test PASS; constructor criterion not met by source inspection, issue pending (1). [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L274-L282] [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L40-L43] |
| C05 / G0 | Reconcile expected scalar fields with `parameters_validated`: 0 missing expected checks; 0 nonfinite inputs accepted; 0 unreported unmatched reference records | Manual coverage audit of mapping and skip branches; incomplete controls, issue pending (2). [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L297-L305] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L391-L409] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L573-L623] [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L107-L111] |
| C06 / G0 | Audit direct and transitive inputs: 0 unresolved references/cycles, 0 duplicate keys, 0 forbidden nulls or selector-order inversions; recomputed fingerprint: 0 digest mismatches and 0 missing immutable revisions | Separate dependency/schema/fingerprint evidence required; issues #495 and #2515. `validate` does not invoke the graph helper. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L28-L87] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L145-L222] [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L21-L30] |
| C07 / G1 | For each file passed to `validate_file`, inspect the native `LoadData` record: 0 errors, 0 unique load warnings, 0 duplicated warning entries, 0 version mismatches, 0 difference between `object_count` and the input manifest count | Same-version demo or licensed API evidence with route, version and input fingerprint; NOT_RUN. Module field locations: [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L97-L114] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L155-L170] (AQG r0, section 5.) |
| C08 / G2 | Audit `Level2Result` against a completed static-state record: 0 convergence-state mismatches; 0 unchecked objects; top tension difference at most 5.0 %, angle error at most 1.0 degree; offset evidence at least 6 steps | Native static record, images, independent line reference and component table; NOT_RUN. Do not substitute `static_analysis_passed` for actual state. [src/digitalmodel/solvers/orcaflex/modular_input_validation/models.py:L86-L107] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L240-L265] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L297-L321] (AQG r0, section 6.) |
| C09 / G3 | Match downstream dynamics evidence to `validate_file` output: 0 records with a different input fingerprint; 0 runs started for overall FAIL, or Level 2 SKIPPED/UNKNOWN (count) | Fingerprint-linked downstream run record and final image; NOT_RUN. This module's result has no dynamics result field. [src/digitalmodel/solvers/orcaflex/modular_input_validation/models.py:L147-L165] (AQG r0, section 7.) |
| C10 / G4 | If a new diffraction source supplies body data, require 1 matching G4 record per source body and 0 missing source/fingerprint links; otherwise require 1 explicit inapplicability reason | NOT_APPLICABLE to this pack's synthetic inputs; new body data require a separate diffraction record. [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L131-L153] (AQG r0, sections 8 and 10.) |
| C11 / G5 | Reconcile `to_dict()` and all report formats with category lists: 0 lost checks, 0 false convergence flags, 0 count differences, 0 duplicated native warnings/issues, 0 FAIL files with 0 critical issues; appendix has 0 missing applicable gate records/signatures and 0 fingerprint mismatches | Serializer/report evidence audit; incomplete fields, issue pending (2); warning duplication and missing-include severity: issue pending (7); full appendix NOT_RUN. [src/digitalmodel/solvers/orcaflex/modular_input_validation/models.py:L200-L233] [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/csv_reporter.py:L68-L84] [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/markdown_reporter.py:L181-L208] [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L240-L277] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L109-L129] (AQG r0, section 9.) |

UNVERIFIED: U3 — no native load, statics, dynamics, diffraction or signed appendix acceptance result is established for this module by the offline runs; the downstream checks require external evidence. See section 10, issue pending (4).

## 6. Known failure modes and pitfalls

These findings are source-observed limits, not failed engineering qualification measurements; proposed detection checks are distinguished from existing executable tests.

| Symptom / cause | Detection and incident record | Date |
| --- | --- | --- |
| Non-finite scalar accepted as within range: `float()` accepts NaN, and both bound comparisons are false, so height, diameter or safety-factor NaN receives INFO | C05: require 0 non-finite inputs accepted; no existing detection check exists; issue pending (2). [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L95-L111] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L307-L334] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L403-L419] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L476-L494] | 2026-10-15 |
| Duplicate YAML keys accepted: `yaml.safe_load` keeps the last mapping value without a duplicate-key check | C06: require 0 duplicate keys accepted, using a repeated-key input; no detection check exists in this path; #495. [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L21-L30] | 2026-10-15 |
| Missing CSVs, missing fields, nonnumeric values or nonmatching environmental/line targets can yield empty checks; a passed level can mask UNKNOWN/SKIPPED coverage | C05: explicitly reconcile expected and actual checks; no existing negative-coverage detection check exists. Issue pending (2). [src/digitalmodel/solvers/orcaflex/modular_input_validation/data_loader.py:L107-L113] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L180-L188] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L398-L406] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L573-L623] [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L350-L362] | 2026-10-15 |
| Reference `tp` can receive a `WaveTz` value, first buoy/wave only is probed, and capacity lookup takes the first partial-word match | C05: require period definition and exact field/range identity, audit all expected objects; no existing detection check exists for these ambiguities. Issue pending (2). [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L287-L295] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L374-L409] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L465-L481] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L563-L592] | 2026-10-15 |
| Serialized physical counts and convergence may retain defaults while validation updates alternate fields; Markdown physical tables omit the comparison category | C11: compare category-list lengths, counters and native state before accepting output; no existing reconciliation check exists. Issue pending (2). [src/digitalmodel/solvers/orcaflex/modular_input_validation/models.py:L86-L107] [src/digitalmodel/solvers/orcaflex/modular_input_validation/models.py:L220-L224] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L163-L188] [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/markdown_reporter.py:L193-L208] | 2026-10-15 |
| Boolean-return smoke tests can pass with missing fixtures or zero reference rows | Existing incident #1983 records the loader test's assertion gap; C05 requires positive coverage, rather than trusting its pytest status. Fixture/assertion follow-up: issue pending (3). [docs/plans/evidence/2026-08-10-issue-1983-triage.md:L145-L146] [tests/test_level1_minimal.py:L35-L43] [tests/orcaflex/test_calm_data_loader.py:L141-L153] | 2026-10-15 |
| `--no-orcaflex` selects no Level 2 execution, but the orchestrator still constructs the API-probing validator; direct `enable_orcaflex=False` and `run_all_levels` are absent from level selection | C04; selection regression exists, constructor-isolation detection check does not exist. Issue pending (1). [src/digitalmodel/solvers/orcaflex/modular_input_validation/cli.py:L198-L217] [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L37-L43] [src/digitalmodel/solvers/orcaflex/modular_input_validation/config.py:L31-L42] [src/digitalmodel/solvers/orcaflex/modular_input_validation/config.py:L75-L85] | 2026-10-15 |
| Level 1 does not validate transitive include syntax or invoke cycle detection; cycle graph edges and nodes use different path representations | C06; direct missing-include regression exists; complete transitive/key-order/schema detection is not supplied by this path. Issue #495. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L64-L87] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L163-L180] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L199-L220] | 2026-10-15 |
| Native warnings duplicated in result and issues: `validate` assigns the native list, `_run_static_analysis` copies it, and `validate` extends it with the copy | C07/C11: compare unique warning count with native input and require 0 duplicates in results/issues; no detection check exists. Issue pending (7). [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L109-L129] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L315-L317] [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L271-L277] | 2026-10-15 |
| Missing include makes the file FAIL but is recorded as WARNING; the summary can show 0 critical issues for a failed file | C01/C11: require 0 FAIL files with 0 critical issues; the existing overall-status test does not assert severity. Issue pending (7). [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L80-L85] [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L240-L247] [src/digitalmodel/solvers/orcaflex/modular_input_validation/models.py:L227-L231] [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L88-L106] | 2026-10-15 |
| Static exceptions/nonconvergence become warnings; `_run_static_analysis` begins passed and does not derive a completed static state | C08/C11: independent completed-state evidence and consistent exported flags; no offline native-state qualification was run. Issue pending (4). [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L240-L265] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L297-L327] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L125-L138] | 2026-10-15 |
| Quick-start documentation calls a missing include a warning, while code returns FAIL; `enable_orcaflex=False` is shown as sufficient to skip the API | C01/C04 versus documented expectations; issue pending (3). [docs/domains/orcaflex/VALIDATION_QUICK_START.md:L180-L203] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L80-L85] [src/digitalmodel/solvers/orcaflex/modular_input_validation/config.py:L31-L42] [src/digitalmodel/solvers/orcaflex/modular_input_validation/config.py:L75-L85] | 2026-10-15 |

The input manifest and revision-binding gap has existing tracking #2515; this package's result serialization contains no such fields, so C06 and C11 require a separate evidence record. [src/digitalmodel/solvers/orcaflex/modular_input_validation/models.py:L200-L233]

## 7. Lessons log

| Entry | Date | Lesson / evidence | Pack change | Shared-log reference |
| --- | --- | --- | --- | --- |
| 1 | 2026-10-15 | Skip-list configuration does not prevent constructor probing; selection-only tests do not prove import isolation. [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L37-L43] [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L274-L282] | Separate C04 selection evidence from native initialization evidence | Not assigned; issue pending (4) |
| 2 | 2026-10-15 | A printed failure can accompany a pytest PASS when the test returns a Boolean; fixture and check coverage need explicit reconciliation. [tests/test_level1_minimal.py:L35-L43] [tests/orcaflex/test_calm_data_loader.py:L141-L153] | Exclude smoke results from validated examples | Not assigned; issue pending (3) |

## 8. Ownership

Proposed review responsibility: OrcaFlex riser and mooring quality manager; module maintenance: module maintainer; engineering gate questions go to the family quality manager, cross-family diffraction questions to the diffraction quality manager. (AQG r0, sections 8 and 11.)

UNVERIFIED: U4 — a named deputy-role assignment, dependency inventory-row reconciliation and shared-log registrations are not established by repository metadata; maintainer confirmation is pending. See section 10, issue pending (4).

## 9. Dependencies

### Runtime and caller relationships

| Relationship | Evidence |
| --- | --- |
| PyYAML parses YAML; loguru writes console output | Direct imports and executed calls; [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L8-L30] [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/console.py:L34-L45] |
| Configuration supplies paths, range data and report selection | The orchestrator constructs all validators and dispatches reporters; [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L30-L43] [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L148-L180] |
| Reference CSV data supply physical comparisons | Five loader categories feed validation; source strings are data, not standards qualification; [src/digitalmodel/solvers/orcaflex/modular_input_validation/data_loader.py:L50-L63] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L131-L161] |
| Native API and state helper | Availability detection imports the binding and queries `Version`; model load calls `Model`/`LoadData`; warning collection calls `CalculateStatics` and imports `run_state`; all read as text only | [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L33-L59] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L155-L170] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L240-L265] |
| Downstream script wrappers | `scripts/run_validation.py` and `scripts/validate_calm_buoy_files.py` construct this validator and delegate validation/reporting; [scripts/run_validation.py:L24-L55] [scripts/validate_calm_buoy_files.py:L24-L55] |
| Generation consumer | The generation script constructs the validator, validates its generated file list and produces reports; this is a caller, not another validation implementation | [scripts/generate_calm_buoy_project.py:L716-L744] |

### Every native-product name occurrence in package Python source

The following occurrence list includes docstrings, identifiers, messages and imports matching case-insensitive `orcaflex|orcfxapi`; source paths are read as text and no availability check is executed. Executable native behavior is confined to the Level 2 definitions described in section 3; callers and report/data references do not establish a successful load. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L24-L59] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L142-L176] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L230-L267]

| Source file | Exact occurrence line numbers | Dependency interpretation |
| --- | --- | --- |
| `src/digitalmodel/solvers/orcaflex/modular_input_validation/__init__.py` | 2, 3, 5, 8, 12 | Public exports and usage documentation; [src/digitalmodel/solvers/orcaflex/modular_input_validation/__init__.py:L2-L12] |
| `src/digitalmodel/solvers/orcaflex/modular_input_validation/cli.py` | 3, 26, 27, 32, 35, 38, 41, 44, 79, 102, 104, 198, 199, 202, 211, 250, 254 | Selection flags, CLI text and printed status; [src/digitalmodel/solvers/orcaflex/modular_input_validation/cli.py:L3-L254] |
| `src/digitalmodel/solvers/orcaflex/modular_input_validation/config.py` | 18, 32, 131 | Configuration and flag descriptions; [src/digitalmodel/solvers/orcaflex/modular_input_validation/config.py:L18-L131] |
| `src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py` | 81 | Explains missing-include failure; [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L81-L81] |
| `src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py` | 2, 3, 9, 13, 15, 18, 19, 26, 27, 28, 30, 31, 33, 35, 38, 39, 40, 43, 44, 45, 49, 51, 54, 55, 57, 58, 59, 63, 74, 75, 80, 85, 86, 87, 88, 89, 93, 94, 95, 97, 98, 101, 107, 117, 126, 133, 138, 142, 144, 152, 153, 157, 160, 175, 183, 191, 193, 195, 197, 199, 211, 220, 235, 249, 258, 274, 315, 331, 337, 338 | Native interface detection, load, statics and metadata path; text inspection only; [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L2-L338] |
| `src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py` | 54, 55, 284, 367, 370, 454, 456 | YAML layout documentation and physical probes; [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L54-L456] |
| `src/digitalmodel/solvers/orcaflex/modular_input_validation/models.py` | 16, 36, 37, 88, 89, 90, 91, 101, 214, 215, 216 | Availability, result fields and statuses; [src/digitalmodel/solvers/orcaflex/modular_input_validation/models.py:L16-L216] |
| `src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/console.py` | 83, 119, 121, 122, 123, 125 | Printed Level 2 status; [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/console.py:L83-L125] |
| `src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/csv_reporter.py` | 58, 77, 104, 105, 109 | Serialized availability/load labels; [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/csv_reporter.py:L58-L109] |
| `src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/markdown_reporter.py` | 111, 153, 157, 158, 159 | Formatted Level 2 section; [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/markdown_reporter.py:L111-L159] |
| `src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py` | 14, 25, 42, 71, 264, 273 | Constructs/delegates to Level 2; text inspection only; [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L14-L273] |

### Map basis and counted families

Implementation-map commit `b5acbe5f61ed4c755c1f7ea4fafa4611bd14e407`; data revision declared in that map: `947fa50ff9a9e210de7e8f90e01b37add85c5954`. The code pin remains the header revision. The map's two qualifying families have 2 and 1 in-package rows respectively; both are counted. Their complete inventories are cross-indexed below, including every outside-package row. The referenced inventory is written against `fb8a214bdb622ed04f0b6473a9311695c9a5a314`, an earlier code revision; the correction table below overrides its stale definition lines and absent-symbol labels at this pack's pin. [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L881-L1324] [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1950-L2090]

| Rank / family | In-package rows | Mapped package implementations and source readback |
| --- | --- | --- |
| 1 / `orcaflex-text-model-writers` | 2 | `extract_includefiles.traverse` extracts references; `ModularInputValidator._collect_issues_from_level_1` collects findings. They do not write model text despite the map classification. [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L43-L59] [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L219-L247] |
| 2 / `external-solver-batch-run-stacks` | 1 | `Level2OrcaFlexValidator._get_model_warnings` launches statics and collects state/warning findings. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L230-L267] |

### Exact package search and classification

The search uses each map signal as written with case-sensitive Python `re`, over all 15 package Python files at the code pin, assigning hits to the innermost enclosing function/class or one module-level definition. A signal is excluded when it matches the package path below `src/digitalmodel/`, case-insensitively. No regular expression is repaired or broadened. The full source inventory is recorded in section 10. [src/digitalmodel/solvers/orcaflex/modular_input_validation/__init__.py:L1-L29] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L1-L340]

| Family | Used signals | Excluded signals / reason | Matched definitions | Further second implementations |
| --- | --- | --- | --- | --- |
| `orcaflex-text-model-writers` | algorithm; constants_or_keys | vocabulary matches `orcaflex` in package path | 25 | 0; classification below |
| `external-solver-batch-run-stacks` | algorithm | vocabulary matches `solvers`/`orcaflex`; constants_or_keys matches `Solve` within `solvers`, case-insensitively | 2 | 0; statics launcher already listed in sections 3 and 9 |

The following table accounts for every matched definition; a reader, record serializer, CLI formatter or report writer does not compute model text or launch another solver. The CLI configuration hit for the external-solver family is a comment; the other external-solver hit is the mapped native warning collector. [src/digitalmodel/solvers/orcaflex/modular_input_validation/cli.py:L198-L203] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L240-L265]

| Family | Matched definition | Classification / source |
| --- | --- | --- |
| `orcaflex-text-model-writers` | `<module>` | module docstring/exports; [src/digitalmodel/solvers/orcaflex/modular_input_validation/__init__.py:L1-L29] |
| `orcaflex-text-model-writers` | `create_parser` | CLI help and argument definitions; [src/digitalmodel/solvers/orcaflex/modular_input_validation/cli.py:L18-L142] |
| `orcaflex-text-model-writers` | `CALMBuoyDataLoader.load_metocean_ranges` | input reader, extraction/range helper or documentation; no model output; [src/digitalmodel/solvers/orcaflex/modular_input_validation/data_loader.py:L77-L113] |
| `orcaflex-text-model-writers` | `CALMBuoyDataLoader.load_mooring_capacity_ranges` | input reader, extraction/range helper or documentation; no model output; [src/digitalmodel/solvers/orcaflex/modular_input_validation/data_loader.py:L116-L156] |
| `orcaflex-text-model-writers` | `CALMBuoyDataLoader.load_environmental_conditions` | input reader, extraction/range helper or documentation; no model output; [src/digitalmodel/solvers/orcaflex/modular_input_validation/data_loader.py:L158-L189] |
| `orcaflex-text-model-writers` | `CALMBuoyDataLoader.load_mooring_line_properties` | input reader, extraction/range helper or documentation; no model output; [src/digitalmodel/solvers/orcaflex/modular_input_validation/data_loader.py:L191-L223] |
| `orcaflex-text-model-writers` | `CALMBuoyDataLoader._load_ranges_csv` | input reader, extraction/range helper or documentation; no model output; [src/digitalmodel/solvers/orcaflex/modular_input_validation/data_loader.py:L225-L258] |
| `orcaflex-text-model-writers` | `Level1YAMLValidator.validate` | input reader, extraction/range helper or documentation; no model output; [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L28-L87] |
| `orcaflex-text-model-writers` | `Level1YAMLValidator.validate_module` | input reader, extraction/range helper or documentation; no model output; [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L89-L143] |
| `orcaflex-text-model-writers` | `Level2OrcaFlexValidator._load_with_orcaflex` | comment in the listed native load method (section 3); [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L142-L176] |
| `orcaflex-text-model-writers` | `_extract_path` | input reader, extraction/range helper or documentation; no model output; [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L48-L72] |
| `orcaflex-text-model-writers` | `Level3PhysicalValidator._validate_hull_geometry` | input reader, extraction/range helper or documentation; no model output; [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L265-L348] |
| `orcaflex-text-model-writers` | `Level3PhysicalValidator._validate_metocean` | input reader, extraction/range helper or documentation; no model output; [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L350-L435] |
| `orcaflex-text-model-writers` | `Level3PhysicalValidator._validate_mooring_capacity` | input reader, extraction/range helper or documentation; no model output; [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L437-L511] |
| `orcaflex-text-model-writers` | `Level3PhysicalValidator._compare_environmental_conditions` | input reader, extraction/range helper or documentation; no model output; [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L544-L623] |
| `orcaflex-text-model-writers` | `ValidationResult.to_dict` | result data serialization; [src/digitalmodel/solvers/orcaflex/modular_input_validation/models.py:L200-L233] |
| `orcaflex-text-model-writers` | `ConsoleReporter._report_level_1` | validation report serialization or formatting; [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/console.py:L103-L115] |
| `orcaflex-text-model-writers` | `CSVReporter.report` | validation report serialization or formatting; [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/csv_reporter.py:L37-L86] |
| `orcaflex-text-model-writers` | `HTMLReporter.report` | validation report serialization or formatting; [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/html_reporter.py:L35-L47] |
| `orcaflex-text-model-writers` | `MarkdownReporter.report` | validation report serialization or formatting; [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/markdown_reporter.py:L34-L46] |
| `orcaflex-text-model-writers` | `MarkdownReporter._generate_level_1_section` | validation report serialization or formatting; [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/markdown_reporter.py:L127-L149] |
| `orcaflex-text-model-writers` | `load_yaml_file` | input reader, extraction/range helper or documentation; no model output; [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L11-L30] |
| `orcaflex-text-model-writers` | `ModularInputValidator.validate_directory` | listed issue collector or file-discovery wrapper; [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L105-L134] |
| `orcaflex-text-model-writers` | `extract_includefiles.traverse` | input reader, extraction/range helper or documentation; no model output; [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L45-L54] |
| `orcaflex-text-model-writers` | `ModularInputValidator._collect_issues_from_level_1` | listed issue collector or file-discovery wrapper; [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L219-L247] |
| `external-solver-batch-run-stacks` | `create_config_from_args` | comment only; configuration helper; [src/digitalmodel/solvers/orcaflex/modular_input_validation/cli.py:L175-L220] |
| `external-solver-batch-run-stacks` | `Level2OrcaFlexValidator._get_model_warnings` | listed statics launcher; [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L230-L267] |

Inventory/search result: complete mapped inventory, 0 further second implementations summed across the counted families; the duplicate-implementation search passes its at-most-1 criterion. This is a discovery result, not native engineering qualification. [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L33-L59] [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L219-L247] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L230-L267]

Repository-wide comparison, including `scripts/`: the exact used writer signals hit 2,230 Python files (405 in `scripts/`), and external-solver signals hit 452 files (111 in `scripts/`). These are text-hit counts, not implementation counts. Separate validation is implemented by `OrcaFlexYAMLValidator.validate_file`, which performs source inspection and format-specific dispatch; it is outside this package. The three script consumers above delegate rather than implement the validator. [src/digitalmodel/solvers/orcaflex/yaml_validator.py:L158-L231] [scripts/run_validation.py:L36-L55] [scripts/validate_calm_buoy_files.py:L36-L55] [scripts/generate_calm_buoy_project.py:L734-L744]

Map follow-up: add a validation-specific family for YAML/include dependency validation, scalar range/tolerance checks and physical-reference matching; the current writer rows are extraction/report helpers rather than model writers. The two dictionary-merge algorithms in section 3 also need semantic distinction (list concatenation versus replacement). `OrcaFlexYAMLValidator` is an outside-package comparison route; inventory reconciliation is issue pending (6). These uncounted validation methods do not change the counted-family result. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L28-L87] [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L62-L111] [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L201-L220] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L226-L263] [src/digitalmodel/solvers/orcaflex/yaml_validator.py:L158-L231]

### Complete inventory cross-index

Each numbered row below identifies exactly one implementation row, in map-file order within its family. Its citation resolves path and symbol in the repository inventory, whose own code revision is `fb8a214bdb622ed04f0b6473a9311695c9a5a314`; the correction table supplies this pack's pinned definition lines where that inventory is stale. This cross-index retains all 440 writer rows and all 137 external-solver rows without duplicating unrelated example identities. Outside-package rows are discovery inventory only; no equivalence or gate pass is assigned. [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L881-L1324] [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1950-L2090]


#### Pinned definition corrections

These entries override the numbered cross-index rows below; all other path/symbol entries retain their referenced definition location. Module-level rows are discovery records rather than function definitions. The six previously absent symbols exist at this pack's pin. [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L881-L1324] [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1950-L2090]

| Family / map row | Symbol at this pack's code pin | Definition citation |
| --- | --- | --- |
| `orcaflex-text-model-writers` / 62 | `scr_reference` | [scripts/orcaflex/check_generic_scr_slwr.py:L78-L78] |
| `orcaflex-text-model-writers` / 63 | `slwr_reference` | [scripts/orcaflex/check_generic_scr_slwr.py:L373-L373] |
| `orcaflex-text-model-writers` / 64 | `calculate` | [scripts/orcaflex/check_generic_scr_slwr.py:L418-L418] |
| `orcaflex-text-model-writers` / 65 | `case_spec` | [scripts/orcaflex/prepare_slay_campaign.py:L69-L69] |
| `orcaflex-text-model-writers` / 66 | `prepare_case` | [scripts/orcaflex/prepare_slay_campaign.py:L90-L90] |
| `orcaflex-text-model-writers` / 67 | `prepare` | [scripts/orcaflex/prepare_slay_campaign.py:L116-L116] |
| `orcaflex-text-model-writers` / 224 | `LinesBuilder.build` | [src/digitalmodel/solvers/orcaflex/modular_generator/builders/lines_builder.py:L46-L46] |
| `orcaflex-text-model-writers` / 230 | `VesselBuilder.build` | [src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_builder.py:L37-L37] |
| `orcaflex-text-model-writers` / 231 | `WinchBuilder.build` | [src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py:L40-L40] |
| `orcaflex-text-model-writers` / 392 | `test_specs_match_basis_and_static_geometry` | [tests/orcaflex/test_generic_scr_slwr.py:L59-L59] |
| `orcaflex-text-model-writers` / 393 | `test_analytical_reference_reproduces_from_basis` | [tests/orcaflex/test_generic_scr_slwr.py:L196-L196] |
| `orcaflex-text-model-writers` / 398 | `_build` | [tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py:L70-L70] |
| `orcaflex-text-model-writers` / 419 | `test_support_types_are_defined_before_vessel_references` | [tests/solvers/orcaflex/test_slay_layout.py:L90-L90] |
| `orcaflex-text-model-writers` / 420 | `_resolve_case.read` | [tests/solvers/orcaflex/test_slay_load_form.py:L38-L38] |
| `orcaflex-text-model-writers` / 421 | `_pipe_seed_points` | [tests/solvers/orcaflex/test_slay_load_form.py:L117-L117] |
| `orcaflex-text-model-writers` / 433 | `test_synthetic_rope_tn_curve_matches_dnv_os_e301_table_f3` | [tests/workflows/test_durable_workflows.py:L1790-L1790] |
| `orcaflex-text-model-writers` / 434 | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | [tests/workflows/test_durable_workflows.py:L1855-L1855] |
| `orcaflex-text-model-writers` / 435 | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | [tests/workflows/test_durable_workflows.py:L1914-L1914] |
| `orcaflex-text-model-writers` / 436 | `test_vessel_seakeeping_significant_amplitude_matches_library` | [tests/workflows/test_durable_workflows.py:L1984-L1984] |
| `external-solver-batch-run-stacks` / 15 | `_launch` | [scripts/cfd/run_sloshing_3d_check.py:L203-L203] |
| `external-solver-batch-run-stacks` / 50 | `engine` | [src/digitalmodel/engine.py:L90-L90] |
| `external-solver-batch-run-stacks` / 63 | `write_queue` | [src/digitalmodel/solvers/orcaflex/modular_generator/slay_execution.py:L86-L86] |
| `external-solver-batch-run-stacks` / 106 | `test_models_validate_and_reproduce` | [tests/orcaflex/test_generic_scr_slwr.py:L111-L111] |
| `external-solver-batch-run-stacks` / 127 | `prepared_queue_inputs.DenyNative.find_spec` | [tests/solvers/orcaflex/test_slay_execution.py:L45-L45] |

#### orcaflex-text-model-writers — complete map inventory

| Map row | Inventory locator for path and symbol | Package relation |
| --- | --- | --- |
| 1 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L885-L885] | outside package; inventory only |
| 2 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L886-L886] | outside package; inventory only |
| 3 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L887-L887] | outside package; inventory only |
| 4 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L888-L888] | outside package; inventory only |
| 5 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L889-L889] | outside package; inventory only |
| 6 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L890-L890] | outside package; inventory only |
| 7 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L891-L891] | outside package; inventory only |
| 8 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L892-L892] | outside package; inventory only |
| 9 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L893-L893] | outside package; inventory only |
| 10 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L894-L894] | outside package; inventory only |
| 11 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L895-L895] | outside package; inventory only |
| 12 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L896-L896] | outside package; inventory only |
| 13 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L897-L897] | outside package; inventory only |
| 14 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L898-L898] | outside package; inventory only |
| 15 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L899-L899] | outside package; inventory only |
| 16 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L900-L900] | outside package; inventory only |
| 17 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L901-L901] | outside package; inventory only |
| 18 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L902-L902] | outside package; inventory only |
| 19 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L903-L903] | outside package; inventory only |
| 20 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L904-L904] | outside package; inventory only |
| 21 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L905-L905] | outside package; inventory only |
| 22 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L906-L906] | outside package; inventory only |
| 23 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L907-L907] | outside package; inventory only |
| 24 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L908-L908] | outside package; inventory only |
| 25 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L909-L909] | outside package; inventory only |
| 26 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L910-L910] | outside package; inventory only |
| 27 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L911-L911] | outside package; inventory only |
| 28 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L912-L912] | outside package; inventory only |
| 29 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L913-L913] | outside package; inventory only |
| 30 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L914-L914] | outside package; inventory only |
| 31 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L915-L915] | outside package; inventory only |
| 32 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L916-L916] | outside package; inventory only |
| 33 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L917-L917] | outside package; inventory only |
| 34 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L918-L918] | outside package; inventory only |
| 35 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L919-L919] | outside package; inventory only |
| 36 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L920-L920] | outside package; inventory only |
| 37 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L921-L921] | outside package; inventory only |
| 38 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L922-L922] | outside package; inventory only |
| 39 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L923-L923] | outside package; inventory only |
| 40 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L924-L924] | outside package; inventory only |
| 41 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L925-L925] | outside package; inventory only |
| 42 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L926-L926] | outside package; inventory only |
| 43 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L927-L927] | outside package; inventory only |
| 44 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L928-L928] | outside package; inventory only |
| 45 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L929-L929] | outside package; inventory only |
| 46 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L930-L930] | outside package; inventory only |
| 47 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L931-L931] | outside package; inventory only |
| 48 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L932-L932] | outside package; inventory only |
| 49 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L933-L933] | outside package; inventory only |
| 50 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L934-L934] | outside package; inventory only |
| 51 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L935-L935] | outside package; inventory only |
| 52 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L936-L936] | outside package; inventory only |
| 53 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L937-L937] | outside package; inventory only |
| 54 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L938-L938] | outside package; inventory only |
| 55 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L939-L939] | outside package; inventory only |
| 56 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L940-L940] | outside package; inventory only |
| 57 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L941-L941] | outside package; inventory only |
| 58 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L942-L942] | outside package; inventory only |
| 59 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L943-L943] | outside package; inventory only |
| 60 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L944-L944] | outside package; inventory only |
| 61 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L945-L945] | outside package; inventory only |
| 62 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L946-L946] | outside package; inventory only |
| 63 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L947-L947] | outside package; inventory only |
| 64 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L948-L948] | outside package; inventory only |
| 65 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L949-L949] | outside package; inventory only |
| 66 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L950-L950] | outside package; inventory only |
| 67 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L951-L951] | outside package; inventory only |
| 68 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L952-L952] | outside package; inventory only |
| 69 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L953-L953] | outside package; inventory only |
| 70 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L954-L954] | outside package; inventory only |
| 71 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L955-L955] | outside package; inventory only |
| 72 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L956-L956] | outside package; inventory only |
| 73 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L957-L957] | outside package; inventory only |
| 74 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L958-L958] | outside package; inventory only |
| 75 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L959-L959] | outside package; inventory only |
| 76 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L960-L960] | outside package; inventory only |
| 77 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L961-L961] | outside package; inventory only |
| 78 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L962-L962] | outside package; inventory only |
| 79 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L963-L963] | outside package; inventory only |
| 80 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L964-L964] | outside package; inventory only |
| 81 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L965-L965] | outside package; inventory only |
| 82 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L966-L966] | outside package; inventory only |
| 83 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L967-L967] | outside package; inventory only |
| 84 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L968-L968] | outside package; inventory only |
| 85 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L969-L969] | outside package; inventory only |
| 86 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L970-L970] | outside package; inventory only |
| 87 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L971-L971] | outside package; inventory only |
| 88 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L972-L972] | outside package; inventory only |
| 89 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L973-L973] | outside package; inventory only |
| 90 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L974-L974] | outside package; inventory only |
| 91 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L975-L975] | outside package; inventory only |
| 92 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L976-L976] | outside package; inventory only |
| 93 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L977-L977] | outside package; inventory only |
| 94 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L978-L978] | outside package; inventory only |
| 95 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L979-L979] | outside package; inventory only |
| 96 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L980-L980] | outside package; inventory only |
| 97 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L981-L981] | outside package; inventory only |
| 98 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L982-L982] | outside package; inventory only |
| 99 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L983-L983] | outside package; inventory only |
| 100 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L984-L984] | outside package; inventory only |
| 101 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L985-L985] | outside package; inventory only |
| 102 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L986-L986] | outside package; inventory only |
| 103 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L987-L987] | outside package; inventory only |
| 104 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L988-L988] | outside package; inventory only |
| 105 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L989-L989] | outside package; inventory only |
| 106 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L990-L990] | outside package; inventory only |
| 107 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L991-L991] | outside package; inventory only |
| 108 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L992-L992] | outside package; inventory only |
| 109 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L993-L993] | outside package; inventory only |
| 110 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L994-L994] | outside package; inventory only |
| 111 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L995-L995] | outside package; inventory only |
| 112 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L996-L996] | outside package; inventory only |
| 113 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L997-L997] | outside package; inventory only |
| 114 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L998-L998] | outside package; inventory only |
| 115 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L999-L999] | outside package; inventory only |
| 116 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1000-L1000] | outside package; inventory only |
| 117 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1001-L1001] | outside package; inventory only |
| 118 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1002-L1002] | outside package; inventory only |
| 119 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1003-L1003] | outside package; inventory only |
| 120 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1004-L1004] | outside package; inventory only |
| 121 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1005-L1005] | outside package; inventory only |
| 122 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1006-L1006] | outside package; inventory only |
| 123 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1007-L1007] | outside package; inventory only |
| 124 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1008-L1008] | outside package; inventory only |
| 125 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1009-L1009] | outside package; inventory only |
| 126 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1010-L1010] | outside package; inventory only |
| 127 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1011-L1011] | outside package; inventory only |
| 128 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1012-L1012] | outside package; inventory only |
| 129 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1013-L1013] | outside package; inventory only |
| 130 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1014-L1014] | outside package; inventory only |
| 131 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1015-L1015] | outside package; inventory only |
| 132 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1016-L1016] | outside package; inventory only |
| 133 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1017-L1017] | outside package; inventory only |
| 134 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1018-L1018] | outside package; inventory only |
| 135 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1019-L1019] | outside package; inventory only |
| 136 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1020-L1020] | outside package; inventory only |
| 137 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1021-L1021] | outside package; inventory only |
| 138 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1022-L1022] | outside package; inventory only |
| 139 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1023-L1023] | outside package; inventory only |
| 140 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1024-L1024] | outside package; inventory only |
| 141 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1025-L1025] | outside package; inventory only |
| 142 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1026-L1026] | outside package; inventory only |
| 143 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1027-L1027] | outside package; inventory only |
| 144 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1028-L1028] | outside package; inventory only |
| 145 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1029-L1029] | outside package; inventory only |
| 146 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1030-L1030] | outside package; inventory only |
| 147 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1031-L1031] | outside package; inventory only |
| 148 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1032-L1032] | outside package; inventory only |
| 149 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1033-L1033] | outside package; inventory only |
| 150 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1034-L1034] | outside package; inventory only |
| 151 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1035-L1035] | outside package; inventory only |
| 152 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1036-L1036] | outside package; inventory only |
| 153 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1037-L1037] | outside package; inventory only |
| 154 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1038-L1038] | outside package; inventory only |
| 155 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1039-L1039] | outside package; inventory only |
| 156 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1040-L1040] | outside package; inventory only |
| 157 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1041-L1041] | outside package; inventory only |
| 158 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1042-L1042] | outside package; inventory only |
| 159 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1043-L1043] | outside package; inventory only |
| 160 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1044-L1044] | outside package; inventory only |
| 161 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1045-L1045] | outside package; inventory only |
| 162 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1046-L1046] | outside package; inventory only |
| 163 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1047-L1047] | outside package; inventory only |
| 164 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1048-L1048] | outside package; inventory only |
| 165 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1049-L1049] | outside package; inventory only |
| 166 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1050-L1050] | outside package; inventory only |
| 167 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1051-L1051] | outside package; inventory only |
| 168 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1052-L1052] | outside package; inventory only |
| 169 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1053-L1053] | outside package; inventory only |
| 170 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1054-L1054] | outside package; inventory only |
| 171 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1055-L1055] | outside package; inventory only |
| 172 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1056-L1056] | outside package; inventory only |
| 173 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1057-L1057] | outside package; inventory only |
| 174 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1058-L1058] | outside package; inventory only |
| 175 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1059-L1059] | outside package; inventory only |
| 176 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1060-L1060] | outside package; inventory only |
| 177 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1061-L1061] | outside package; inventory only |
| 178 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1062-L1062] | outside package; inventory only |
| 179 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1063-L1063] | outside package; inventory only |
| 180 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1064-L1064] | outside package; inventory only |
| 181 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1065-L1065] | outside package; inventory only |
| 182 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1066-L1066] | outside package; inventory only |
| 183 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1067-L1067] | outside package; inventory only |
| 184 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1068-L1068] | outside package; inventory only |
| 185 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1069-L1069] | outside package; inventory only |
| 186 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1070-L1070] | outside package; inventory only |
| 187 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1071-L1071] | outside package; inventory only |
| 188 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1072-L1072] | outside package; inventory only |
| 189 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1073-L1073] | outside package; inventory only |
| 190 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1074-L1074] | outside package; inventory only |
| 191 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1075-L1075] | outside package; inventory only |
| 192 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1076-L1076] | outside package; inventory only |
| 193 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1077-L1077] | outside package; inventory only |
| 194 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1078-L1078] | outside package; inventory only |
| 195 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1079-L1079] | outside package; inventory only |
| 196 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1080-L1080] | outside package; inventory only |
| 197 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1081-L1081] | outside package; inventory only |
| 198 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1082-L1082] | outside package; inventory only |
| 199 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1083-L1083] | outside package; inventory only |
| 200 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1084-L1084] | outside package; inventory only |
| 201 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1085-L1085] | outside package; inventory only |
| 202 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1086-L1086] | outside package; inventory only |
| 203 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1087-L1087] | outside package; inventory only |
| 204 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1088-L1088] | outside package; inventory only |
| 205 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1089-L1089] | outside package; inventory only |
| 206 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1090-L1090] | outside package; inventory only |
| 207 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1091-L1091] | outside package; inventory only |
| 208 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1092-L1092] | outside package; inventory only |
| 209 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1093-L1093] | outside package; inventory only |
| 210 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1094-L1094] | outside package; inventory only |
| 211 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1095-L1095] | outside package; inventory only |
| 212 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1096-L1096] | outside package; inventory only |
| 213 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1097-L1097] | outside package; inventory only |
| 214 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1098-L1098] | outside package; inventory only |
| 215 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1099-L1099] | outside package; inventory only |
| 216 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1100-L1100] | outside package; inventory only |
| 217 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1101-L1101] | outside package; inventory only |
| 218 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1102-L1102] | outside package; inventory only |
| 219 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1103-L1103] | outside package; inventory only |
| 220 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1104-L1104] | outside package; inventory only |
| 221 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1105-L1105] | outside package; inventory only |
| 222 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1106-L1106] | outside package; inventory only |
| 223 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1107-L1107] | outside package; inventory only |
| 224 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1108-L1108] | outside package; inventory only |
| 225 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1109-L1109] | outside package; inventory only |
| 226 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1110-L1110] | outside package; inventory only |
| 227 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1111-L1111] | outside package; inventory only |
| 228 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1112-L1112] | outside package; inventory only |
| 229 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1113-L1113] | outside package; inventory only |
| 230 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1114-L1114] | outside package; inventory only |
| 231 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1115-L1115] | outside package; inventory only |
| 232 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1116-L1116] | outside package; inventory only |
| 233 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1117-L1117] | outside package; inventory only |
| 234 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1118-L1118] | outside package; inventory only |
| 235 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1119-L1119] | outside package; inventory only |
| 236 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1120-L1120] | outside package; inventory only |
| 237 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1121-L1121] | outside package; inventory only |
| 238 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1122-L1122] | outside package; inventory only |
| 239 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1123-L1123] | outside package; inventory only |
| 240 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1124-L1124] | outside package; inventory only |
| 241 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1125-L1125] | outside package; inventory only |
| 242 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1126-L1126] | outside package; inventory only |
| 243 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1127-L1127] | outside package; inventory only |
| 244 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1128-L1128] | outside package; inventory only |
| 245 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1129-L1129] | outside package; inventory only |
| 246 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1130-L1130] | outside package; inventory only |
| 247 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1131-L1131] | in package; detailed above |
| 248 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1132-L1132] | in package; detailed above |
| 249 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1133-L1133] | outside package; inventory only |
| 250 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1134-L1134] | outside package; inventory only |
| 251 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1135-L1135] | outside package; inventory only |
| 252 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1136-L1136] | outside package; inventory only |
| 253 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1137-L1137] | outside package; inventory only |
| 254 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1138-L1138] | outside package; inventory only |
| 255 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1139-L1139] | outside package; inventory only |
| 256 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1140-L1140] | outside package; inventory only |
| 257 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1141-L1141] | outside package; inventory only |
| 258 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1142-L1142] | outside package; inventory only |
| 259 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1143-L1143] | outside package; inventory only |
| 260 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1144-L1144] | outside package; inventory only |
| 261 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1145-L1145] | outside package; inventory only |
| 262 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1146-L1146] | outside package; inventory only |
| 263 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1147-L1147] | outside package; inventory only |
| 264 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1148-L1148] | outside package; inventory only |
| 265 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1149-L1149] | outside package; inventory only |
| 266 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1150-L1150] | outside package; inventory only |
| 267 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1151-L1151] | outside package; inventory only |
| 268 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1152-L1152] | outside package; inventory only |
| 269 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1153-L1153] | outside package; inventory only |
| 270 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1154-L1154] | outside package; inventory only |
| 271 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1155-L1155] | outside package; inventory only |
| 272 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1156-L1156] | outside package; inventory only |
| 273 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1157-L1157] | outside package; inventory only |
| 274 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1158-L1158] | outside package; inventory only |
| 275 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1159-L1159] | outside package; inventory only |
| 276 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1160-L1160] | outside package; inventory only |
| 277 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1161-L1161] | outside package; inventory only |
| 278 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1162-L1162] | outside package; inventory only |
| 279 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1163-L1163] | outside package; inventory only |
| 280 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1164-L1164] | outside package; inventory only |
| 281 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1165-L1165] | outside package; inventory only |
| 282 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1166-L1166] | outside package; inventory only |
| 283 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1167-L1167] | outside package; inventory only |
| 284 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1168-L1168] | outside package; inventory only |
| 285 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1169-L1169] | outside package; inventory only |
| 286 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1170-L1170] | outside package; inventory only |
| 287 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1171-L1171] | outside package; inventory only |
| 288 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1172-L1172] | outside package; inventory only |
| 289 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1173-L1173] | outside package; inventory only |
| 290 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1174-L1174] | outside package; inventory only |
| 291 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1175-L1175] | outside package; inventory only |
| 292 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1176-L1176] | outside package; inventory only |
| 293 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1177-L1177] | outside package; inventory only |
| 294 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1178-L1178] | outside package; inventory only |
| 295 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1179-L1179] | outside package; inventory only |
| 296 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1180-L1180] | outside package; inventory only |
| 297 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1181-L1181] | outside package; inventory only |
| 298 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1182-L1182] | outside package; inventory only |
| 299 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1183-L1183] | outside package; inventory only |
| 300 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1184-L1184] | outside package; inventory only |
| 301 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1185-L1185] | outside package; inventory only |
| 302 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1186-L1186] | outside package; inventory only |
| 303 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1187-L1187] | outside package; inventory only |
| 304 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1188-L1188] | outside package; inventory only |
| 305 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1189-L1189] | outside package; inventory only |
| 306 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1190-L1190] | outside package; inventory only |
| 307 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1191-L1191] | outside package; inventory only |
| 308 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1192-L1192] | outside package; inventory only |
| 309 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1193-L1193] | outside package; inventory only |
| 310 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1194-L1194] | outside package; inventory only |
| 311 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1195-L1195] | outside package; inventory only |
| 312 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1196-L1196] | outside package; inventory only |
| 313 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1197-L1197] | outside package; inventory only |
| 314 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1198-L1198] | outside package; inventory only |
| 315 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1199-L1199] | outside package; inventory only |
| 316 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1200-L1200] | outside package; inventory only |
| 317 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1201-L1201] | outside package; inventory only |
| 318 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1202-L1202] | outside package; inventory only |
| 319 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1203-L1203] | outside package; inventory only |
| 320 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1204-L1204] | outside package; inventory only |
| 321 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1205-L1205] | outside package; inventory only |
| 322 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1206-L1206] | outside package; inventory only |
| 323 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1207-L1207] | outside package; inventory only |
| 324 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1208-L1208] | outside package; inventory only |
| 325 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1209-L1209] | outside package; inventory only |
| 326 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1210-L1210] | outside package; inventory only |
| 327 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1211-L1211] | outside package; inventory only |
| 328 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1212-L1212] | outside package; inventory only |
| 329 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1213-L1213] | outside package; inventory only |
| 330 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1214-L1214] | outside package; inventory only |
| 331 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1215-L1215] | outside package; inventory only |
| 332 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1216-L1216] | outside package; inventory only |
| 333 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1217-L1217] | outside package; inventory only |
| 334 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1218-L1218] | outside package; inventory only |
| 335 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1219-L1219] | outside package; inventory only |
| 336 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1220-L1220] | outside package; inventory only |
| 337 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1221-L1221] | outside package; inventory only |
| 338 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1222-L1222] | outside package; inventory only |
| 339 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1223-L1223] | outside package; inventory only |
| 340 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1224-L1224] | outside package; inventory only |
| 341 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1225-L1225] | outside package; inventory only |
| 342 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1226-L1226] | outside package; inventory only |
| 343 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1227-L1227] | outside package; inventory only |
| 344 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1228-L1228] | outside package; inventory only |
| 345 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1229-L1229] | outside package; inventory only |
| 346 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1230-L1230] | outside package; inventory only |
| 347 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1231-L1231] | outside package; inventory only |
| 348 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1232-L1232] | outside package; inventory only |
| 349 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1233-L1233] | outside package; inventory only |
| 350 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1234-L1234] | outside package; inventory only |
| 351 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1235-L1235] | outside package; inventory only |
| 352 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1236-L1236] | outside package; inventory only |
| 353 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1237-L1237] | outside package; inventory only |
| 354 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1238-L1238] | outside package; inventory only |
| 355 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1239-L1239] | outside package; inventory only |
| 356 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1240-L1240] | outside package; inventory only |
| 357 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1241-L1241] | outside package; inventory only |
| 358 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1242-L1242] | outside package; inventory only |
| 359 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1243-L1243] | outside package; inventory only |
| 360 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1244-L1244] | outside package; inventory only |
| 361 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1245-L1245] | outside package; inventory only |
| 362 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1246-L1246] | outside package; inventory only |
| 363 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1247-L1247] | outside package; inventory only |
| 364 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1248-L1248] | outside package; inventory only |
| 365 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1249-L1249] | outside package; inventory only |
| 366 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1250-L1250] | outside package; inventory only |
| 367 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1251-L1251] | outside package; inventory only |
| 368 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1252-L1252] | outside package; inventory only |
| 369 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1253-L1253] | outside package; inventory only |
| 370 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1254-L1254] | outside package; inventory only |
| 371 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1255-L1255] | outside package; inventory only |
| 372 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1256-L1256] | outside package; inventory only |
| 373 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1257-L1257] | outside package; inventory only |
| 374 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1258-L1258] | outside package; inventory only |
| 375 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1259-L1259] | outside package; inventory only |
| 376 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1260-L1260] | outside package; inventory only |
| 377 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1261-L1261] | outside package; inventory only |
| 378 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1262-L1262] | outside package; inventory only |
| 379 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1263-L1263] | outside package; inventory only |
| 380 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1264-L1264] | outside package; inventory only |
| 381 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1265-L1265] | outside package; inventory only |
| 382 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1266-L1266] | outside package; inventory only |
| 383 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1267-L1267] | outside package; inventory only |
| 384 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1268-L1268] | outside package; inventory only |
| 385 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1269-L1269] | outside package; inventory only |
| 386 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1270-L1270] | outside package; inventory only |
| 387 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1271-L1271] | outside package; inventory only |
| 388 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1272-L1272] | outside package; inventory only |
| 389 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1273-L1273] | outside package; inventory only |
| 390 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1274-L1274] | outside package; inventory only |
| 391 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1275-L1275] | outside package; inventory only |
| 392 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1276-L1276] | outside package; inventory only |
| 393 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1277-L1277] | outside package; inventory only |
| 394 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1278-L1278] | outside package; inventory only |
| 395 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1279-L1279] | outside package; inventory only |
| 396 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1280-L1280] | outside package; inventory only |
| 397 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1281-L1281] | outside package; inventory only |
| 398 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1282-L1282] | outside package; inventory only |
| 399 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1283-L1283] | outside package; inventory only |
| 400 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1284-L1284] | outside package; inventory only |
| 401 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1285-L1285] | outside package; inventory only |
| 402 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1286-L1286] | outside package; inventory only |
| 403 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1287-L1287] | outside package; inventory only |
| 404 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1288-L1288] | outside package; inventory only |
| 405 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1289-L1289] | outside package; inventory only |
| 406 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1290-L1290] | outside package; inventory only |
| 407 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1291-L1291] | outside package; inventory only |
| 408 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1292-L1292] | outside package; inventory only |
| 409 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1293-L1293] | outside package; inventory only |
| 410 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1294-L1294] | outside package; inventory only |
| 411 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1295-L1295] | outside package; inventory only |
| 412 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1296-L1296] | outside package; inventory only |
| 413 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1297-L1297] | outside package; inventory only |
| 414 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1298-L1298] | outside package; inventory only |
| 415 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1299-L1299] | outside package; inventory only |
| 416 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1300-L1300] | outside package; inventory only |
| 417 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1301-L1301] | outside package; inventory only |
| 418 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1302-L1302] | outside package; inventory only |
| 419 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1303-L1303] | outside package; inventory only |
| 420 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1304-L1304] | outside package; inventory only |
| 421 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1305-L1305] | outside package; inventory only |
| 422 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1306-L1306] | outside package; inventory only |
| 423 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1307-L1307] | outside package; inventory only |
| 424 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1308-L1308] | outside package; inventory only |
| 425 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1309-L1309] | outside package; inventory only |
| 426 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1310-L1310] | outside package; inventory only |
| 427 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1311-L1311] | outside package; inventory only |
| 428 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1312-L1312] | outside package; inventory only |
| 429 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1313-L1313] | outside package; inventory only |
| 430 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1314-L1314] | outside package; inventory only |
| 431 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1315-L1315] | outside package; inventory only |
| 432 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1316-L1316] | outside package; inventory only |
| 433 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1317-L1317] | outside package; inventory only |
| 434 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1318-L1318] | outside package; inventory only |
| 435 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1319-L1319] | outside package; inventory only |
| 436 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1320-L1320] | outside package; inventory only |
| 437 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1321-L1321] | outside package; inventory only |
| 438 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1322-L1322] | outside package; inventory only |
| 439 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1323-L1323] | outside package; inventory only |
| 440 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1324-L1324] | outside package; inventory only |

#### external-solver-batch-run-stacks — complete map inventory

| Map row | Inventory locator for path and symbol | Package relation |
| --- | --- | --- |
| 1 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1954-L1954] | outside package; inventory only |
| 2 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1955-L1955] | outside package; inventory only |
| 3 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1956-L1956] | outside package; inventory only |
| 4 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1957-L1957] | outside package; inventory only |
| 5 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1958-L1958] | outside package; inventory only |
| 6 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1959-L1959] | outside package; inventory only |
| 7 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1960-L1960] | outside package; inventory only |
| 8 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1961-L1961] | outside package; inventory only |
| 9 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1962-L1962] | outside package; inventory only |
| 10 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1963-L1963] | outside package; inventory only |
| 11 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1964-L1964] | outside package; inventory only |
| 12 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1965-L1965] | outside package; inventory only |
| 13 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1966-L1966] | outside package; inventory only |
| 14 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1967-L1967] | outside package; inventory only |
| 15 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1968-L1968] | outside package; inventory only |
| 16 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1969-L1969] | outside package; inventory only |
| 17 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1970-L1970] | outside package; inventory only |
| 18 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1971-L1971] | outside package; inventory only |
| 19 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1972-L1972] | outside package; inventory only |
| 20 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1973-L1973] | outside package; inventory only |
| 21 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1974-L1974] | outside package; inventory only |
| 22 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1975-L1975] | outside package; inventory only |
| 23 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1976-L1976] | outside package; inventory only |
| 24 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1977-L1977] | outside package; inventory only |
| 25 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1978-L1978] | outside package; inventory only |
| 26 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1979-L1979] | outside package; inventory only |
| 27 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1980-L1980] | outside package; inventory only |
| 28 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1981-L1981] | outside package; inventory only |
| 29 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1982-L1982] | outside package; inventory only |
| 30 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1983-L1983] | outside package; inventory only |
| 31 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1984-L1984] | outside package; inventory only |
| 32 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1985-L1985] | outside package; inventory only |
| 33 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1986-L1986] | outside package; inventory only |
| 34 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1987-L1987] | outside package; inventory only |
| 35 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1988-L1988] | outside package; inventory only |
| 36 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1989-L1989] | outside package; inventory only |
| 37 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1990-L1990] | outside package; inventory only |
| 38 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1991-L1991] | outside package; inventory only |
| 39 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1992-L1992] | outside package; inventory only |
| 40 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1993-L1993] | outside package; inventory only |
| 41 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1994-L1994] | outside package; inventory only |
| 42 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1995-L1995] | outside package; inventory only |
| 43 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1996-L1996] | outside package; inventory only |
| 44 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1997-L1997] | outside package; inventory only |
| 45 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1998-L1998] | outside package; inventory only |
| 46 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L1999-L1999] | outside package; inventory only |
| 47 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2000-L2000] | outside package; inventory only |
| 48 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2001-L2001] | outside package; inventory only |
| 49 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2002-L2002] | outside package; inventory only |
| 50 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2003-L2003] | outside package; inventory only |
| 51 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2004-L2004] | outside package; inventory only |
| 52 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2005-L2005] | outside package; inventory only |
| 53 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2006-L2006] | outside package; inventory only |
| 54 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2007-L2007] | outside package; inventory only |
| 55 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2008-L2008] | outside package; inventory only |
| 56 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2009-L2009] | outside package; inventory only |
| 57 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2010-L2010] | outside package; inventory only |
| 58 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2011-L2011] | outside package; inventory only |
| 59 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2012-L2012] | outside package; inventory only |
| 60 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2013-L2013] | outside package; inventory only |
| 61 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2014-L2014] | outside package; inventory only |
| 62 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2015-L2015] | outside package; inventory only |
| 63 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2016-L2016] | outside package; inventory only |
| 64 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2017-L2017] | in package; detailed above |
| 65 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2018-L2018] | outside package; inventory only |
| 66 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2019-L2019] | outside package; inventory only |
| 67 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2020-L2020] | outside package; inventory only |
| 68 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2021-L2021] | outside package; inventory only |
| 69 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2022-L2022] | outside package; inventory only |
| 70 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2023-L2023] | outside package; inventory only |
| 71 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2024-L2024] | outside package; inventory only |
| 72 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2025-L2025] | outside package; inventory only |
| 73 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2026-L2026] | outside package; inventory only |
| 74 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2027-L2027] | outside package; inventory only |
| 75 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2028-L2028] | outside package; inventory only |
| 76 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2029-L2029] | outside package; inventory only |
| 77 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2030-L2030] | outside package; inventory only |
| 78 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2031-L2031] | outside package; inventory only |
| 79 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2032-L2032] | outside package; inventory only |
| 80 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2033-L2033] | outside package; inventory only |
| 81 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2034-L2034] | outside package; inventory only |
| 82 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2035-L2035] | outside package; inventory only |
| 83 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2036-L2036] | outside package; inventory only |
| 84 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2037-L2037] | outside package; inventory only |
| 85 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2038-L2038] | outside package; inventory only |
| 86 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2039-L2039] | outside package; inventory only |
| 87 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2040-L2040] | outside package; inventory only |
| 88 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2041-L2041] | outside package; inventory only |
| 89 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2042-L2042] | outside package; inventory only |
| 90 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2043-L2043] | outside package; inventory only |
| 91 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2044-L2044] | outside package; inventory only |
| 92 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2045-L2045] | outside package; inventory only |
| 93 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2046-L2046] | outside package; inventory only |
| 94 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2047-L2047] | outside package; inventory only |
| 95 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2048-L2048] | outside package; inventory only |
| 96 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2049-L2049] | outside package; inventory only |
| 97 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2050-L2050] | outside package; inventory only |
| 98 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2051-L2051] | outside package; inventory only |
| 99 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2052-L2052] | outside package; inventory only |
| 100 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2053-L2053] | outside package; inventory only |
| 101 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2054-L2054] | outside package; inventory only |
| 102 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2055-L2055] | outside package; inventory only |
| 103 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2056-L2056] | outside package; inventory only |
| 104 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2057-L2057] | outside package; inventory only |
| 105 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2058-L2058] | outside package; inventory only |
| 106 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2059-L2059] | outside package; inventory only |
| 107 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2060-L2060] | outside package; inventory only |
| 108 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2061-L2061] | outside package; inventory only |
| 109 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2062-L2062] | outside package; inventory only |
| 110 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2063-L2063] | outside package; inventory only |
| 111 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2064-L2064] | outside package; inventory only |
| 112 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2065-L2065] | outside package; inventory only |
| 113 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2066-L2066] | outside package; inventory only |
| 114 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2067-L2067] | outside package; inventory only |
| 115 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2068-L2068] | outside package; inventory only |
| 116 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2069-L2069] | outside package; inventory only |
| 117 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2070-L2070] | outside package; inventory only |
| 118 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2071-L2071] | outside package; inventory only |
| 119 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2072-L2072] | outside package; inventory only |
| 120 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2073-L2073] | outside package; inventory only |
| 121 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2074-L2074] | outside package; inventory only |
| 122 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2075-L2075] | outside package; inventory only |
| 123 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2076-L2076] | outside package; inventory only |
| 124 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2077-L2077] | outside package; inventory only |
| 125 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2078-L2078] | outside package; inventory only |
| 126 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2079-L2079] | outside package; inventory only |
| 127 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2080-L2080] | outside package; inventory only |
| 128 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2081-L2081] | outside package; inventory only |
| 129 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2082-L2082] | outside package; inventory only |
| 130 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2083-L2083] | outside package; inventory only |
| 131 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2084-L2084] | outside package; inventory only |
| 132 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2085-L2085] | outside package; inventory only |
| 133 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2086-L2086] | outside package; inventory only |
| 134 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2087-L2087] | outside package; inventory only |
| 135 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2088-L2088] | outside package; inventory only |
| 136 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2089-L2089] | outside package; inventory only |
| 137 | [src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md:L2090-L2090] | outside package; inventory only |


## 10. Coverage and open items

### Files read

All package Python files and its README were read; the table gives the read extent, not a claim of independent validation for every branch.

| Package file | Read extent |
| --- | --- |
| `README.md` | [src/digitalmodel/solvers/orcaflex/modular_input_validation/README.md:L1-L212] |
| `__init__.py` | [src/digitalmodel/solvers/orcaflex/modular_input_validation/__init__.py:L1-L29] |
| `cli.py` | [src/digitalmodel/solvers/orcaflex/modular_input_validation/cli.py:L1-L319] |
| `config.py` | [src/digitalmodel/solvers/orcaflex/modular_input_validation/config.py:L1-L141] |
| `data_loader.py` | [src/digitalmodel/solvers/orcaflex/modular_input_validation/data_loader.py:L1-L279] |
| `level_1_yaml.py` | [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L1-L222] |
| `level_2_orcaflex.py` | [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L1-L340] |
| `level_3_physical.py` | [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L1-L681] |
| `models.py` | [src/digitalmodel/solvers/orcaflex/modular_input_validation/models.py:L1-L233] |
| `reporters/__init__.py` | [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/__init__.py:L1-L16] |
| `reporters/console.py` | [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/console.py:L1-L217] |
| `reporters/csv_reporter.py` | [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/csv_reporter.py:L1-L115] |
| `reporters/html_reporter.py` | [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/html_reporter.py:L1-L384] |
| `reporters/markdown_reporter.py` | [src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/markdown_reporter.py:L1-L242] |
| `utils.py` | [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L1-L220] |
| `validator.py` | [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L1-L400] |

| Additional documentation found and read | Extent / use |
| --- | --- |
| Validation quick start | CLI guidance and known mismatch; [docs/domains/orcaflex/VALIDATION_QUICK_START.md:L1-L259] |
| Loader usage | Format conventions and examples, not qualified numeric references; [docs/domains/orcaflex/DATA_LOADER_USAGE.md:L1-L353] |
| Loader verification | Historical count/qualification claims are documentation claims, unverified for current data; [docs/domains/orcaflex/DATA_LOADER_VERIFICATION.md:L1-L172] |
| Utility review | Historical assertions checked against actual tests rather than inherited as qualification; [docs/domains/orcaflex/UTILITY_FUNCTIONS_REVIEW.md:L1-L346] |
| Usage example | Script workflows, not independent expected results; read only because constructors probe the API; [docs/domains/orcaflex/modular_input_validation_example.py:L1-L188] |
| Validation framework guidance | Prescriptive design guidance; the early-failure behavior follows implementation rather than the guidance to run all levels; [docs/domains/testing/VALIDATION_FRAMEWORK_BEST_PRACTICES.md:L1-L796] [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L60-L69] |
| Legacy inventory | Lists the usage example; historical counts not adopted; [docs/domains/orcaflex/LEGACY_SCRIPT_INVENTORY.md:L46-L59] |
| Test triage evidence | Existing loader smoke-test limitation; [docs/plans/evidence/2026-08-10-issue-1983-triage.md:L145-L146] |

| Test file inventory | Recount / disposition |
| --- | --- |
| `tests/test_validation_utils.py` | 10 test functions, all PASS; [tests/test_validation_utils.py:L34-L428] |
| `tests/test_level1_minimal.py` | 1 test function, pytest PASS; fixture not exercised because it is absent at the pin (tree listing); PASS covers import and early return only; [tests/test_level1_minimal.py:L24-L113] |
| `tests/orcaflex/test_calm_data_loader.py` | 2 test functions, pytest PASS; assertion-free smoke only, zero reference rows observed; [tests/orcaflex/test_calm_data_loader.py:L19-L177] |
| `tests/solvers/orcaflex/test_modular_input_validation_fixes.py` | 15 test methods: all PASS, including both orchestrator nodes in section 4; [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L54-L289] |

Recount: 28 test definitions across four files, not the inventory lead's approximate count; the public test files and exact node IDs above are the basis. All 28 nodes passed at BASE and on the uncommitted update; both orchestrator nodes handled the absent optional native package and skipped native execution. No node was disabled or patched. [tests/test_validation_utils.py:L34-L428] [tests/test_level1_minimal.py:L24-L113] [tests/orcaflex/test_calm_data_loader.py:L19-L177] [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L54-L289]

No independent engineering example is qualified here; reference-data documentation examples remain unqualified until provenance and units are established. [docs/domains/orcaflex/DATA_LOADER_VERIFICATION.md:L132-L143]

| Open item | Responsibility | Tracking / completion criterion |
| --- | --- | --- |
| Constructor import isolation and ignored direct configuration flags | Module maintainer | issue pending (1); C04 requires 0 native-import attempts for offline construction. [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L37-L43] [src/digitalmodel/solvers/orcaflex/modular_input_validation/config.py:L75-L85] |
| Physical coverage, period identity, range selection, finite numbers and serialized evidence | Module maintainer | issue pending (2); C05/C11 require 0 omitted expected checks and 0 mismatched counters/quantities. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L374-L409] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L465-L481] [src/digitalmodel/solvers/orcaflex/modular_input_validation/models.py:L200-L233] |
| Smoke assertions, missing fixture and stale quick-start guidance | Module maintainer | issue pending (3), related #1983; explicit assertions and a positive fixture are required. [tests/test_level1_minimal.py:L35-L43] [docs/plans/evidence/2026-08-10-issue-1983-triage.md:L145-L146] [docs/domains/orcaflex/VALIDATION_QUICK_START.md:L180-L203] |
| Full schema, transitive references, duplicate keys and selector semantics | Module maintainer | #495; complete C06 independently of direct-include success. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_1_yaml.py:L64-L87] [src/digitalmodel/solvers/orcaflex/yaml_validator.py:L190-L231] |
| Fingerprints and immutable revision binding | Module maintainer | #2515; C06/C11 require 0 missing digests/revisions. [src/digitalmodel/solvers/orcaflex/modular_input_validation/models.py:L200-L233] |
| Native warning duplication and failed-file severity consistency | Module maintainer | issue pending (7); C07/C11 require 0 duplicated warnings/issues and 0 FAIL files with 0 critical issues. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L109-L129] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L315-L317] [src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py:L240-L277] |
| U3 native/downstream qualification; U4 deputy, shared-log and dependency-row assignments | Family quality manager and module maintainer | issue pending (4); establish native state, independent worked example and gate records, assign review deputy and evidence registrations. [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_2_orcaflex.py:L230-L327] |
| U1 units/conventions/version support; U2 range provenance and licensed edition/clause qualification | Module maintainer | issue pending (5); qualify the reference basis without publishing licensed values. [src/digitalmodel/solvers/orcaflex/modular_input_validation/data_loader.py:L13-L30] [docs/domains/orcaflex/DATA_LOADER_VERIFICATION.md:L132-L139] |
| Map classification and uncounted validation families | Module maintainer | issue pending (6); add correct validation rows and distinguish the two merge semantics before counting these families. [src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py:L201-L220] [src/digitalmodel/solvers/orcaflex/modular_input_validation/level_3_physical.py:L226-L263] |

No client-derived reference value or private sidecar payload is included; this pack's numerical regression expectations come from the public synthetic tests cited in section 4. No private-sidecar entry is needed for those test literals. [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L131-L153] [tests/solvers/orcaflex/test_modular_input_validation_fixes.py:L199-L227]
