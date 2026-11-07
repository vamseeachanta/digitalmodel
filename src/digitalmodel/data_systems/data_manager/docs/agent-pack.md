# Data manager knowledge pack

| Header | Value |
|---|---|
| Module | data_systems/data_manager |
| Repository | digitalmodel |
| Package | src/digitalmodel/data_systems/data_manager |
| Pack revision | 0 |
| Code pin | aebd7e4bc5c3dda5f7433e20e021dd58ac17d0b3 |
| Analysis family | EC; data pipeline; Python calculations gate set |
| Quality role | Calculations quality manager; provisional coverage of data pipelines |
| Status | DRAFT |

## 1. Purpose and scope

`ConfigurationManager` loads and recursively overlays YAML mappings, derives a missing pipe diameter, formats plot metadata, prepares analysis inputs, extracts YAML fields into a pandas table, validates selected fields, resets defaults and saves YAML. [src/digitalmodel/data_systems/data_manager/configuration.py:L37-L102] [src/digitalmodel/data_systems/data_manager/configuration.py:L113-L233] [src/digitalmodel/data_systems/data_manager/configuration.py:L235-L376]

The package exports `ConfigurationManager`, `ymlInput`, `customUpdate` and `loadConfiguration`; the latter three delegate to the manager, with `customUpdate` operating on the supplied mapping. [src/digitalmodel/data_systems/data_manager/__init__.py:L8-L20] [src/digitalmodel/data_systems/data_manager/configuration.py:L379-L423]

The routing registry assigns beta maturity to the containing data_systems domain; this is a domain designation, not evidence of module validation. [docs/registry/module-routing.yaml:L113-L122]

Scope includes both package Python files; pressure and stress analysis belong to the separate analyzer, which consumes the manager and invokes a separate calculator. [src/digitalmodel/data_systems/data_manager/__init__.py:L1-L20] [src/digitalmodel/data_systems/data_manager/configuration.py:L1-L423] [src/digitalmodel/specialized/api_analysis/apistd2rd.py:L19-L51]

## 2. Inputs and outputs

Input contract, as implemented:

| Input | Unit or convention | Range and default | Source |
|---|---|---|---|
| `default_file`, optional `update_file` | UTF-8 YAML; native `Path` semantics | Default file must exist; update absent or invalid YAML leaves defaults with a log message | [src/digitalmodel/data_systems/data_manager/configuration.py:L56-L86] |
| Constructor `default_config_file` | Native path | Default `None`; constructor stores the path but does not load it | [src/digitalmodel/data_systems/data_manager/configuration.py:L26-L35] |
| `geometry.NominalOD`, `NominalID`, `DesignWT` | Caller must use one consistent length unit; no conversion | No positivity/range checks; OD and WT can derive ID, or ID and WT can derive OD | [src/digitalmodel/data_systems/data_manager/configuration.py:L119-L132] [src/digitalmodel/data_systems/data_manager/configuration.py:L334-L343] |
| `material.E`, `SMYS`, `SMUS` | Caller supplies compatible stress units | Validation warns for absent or nonpositive values; input preparation checks only presence | [src/digitalmodel/data_systems/data_manager/configuration.py:L205-L227] [src/digitalmodel/data_systems/data_manager/configuration.py:L345-L351] |
| `Poissionsratio`, `alphafab`, `k`, `designFactors` | Values passed through without conversion | Optional material keys become `None`; designFactors section required for input preparation | [src/digitalmodel/data_systems/data_manager/configuration.py:L192-L227] |
| `nominalWTAPISTD2RDMethod1.data`, `method_index` | Sequence; zero-based index | Index defaults to 0; absent method data becomes an empty mapping; index bounds not checked | [src/digitalmodel/data_systems/data_manager/configuration.py:L179-L203] |
| Method pressures, corrosion allowance, limit state | Caller-supplied pressure and length units | Missing pressures and allowance become 0; limit state becomes `design` | [src/digitalmodel/data_systems/data_manager/configuration.py:L215-L219] |
| `plotSettings.variableWTBurst` | Python format strings | Filename positional arguments: diameter type, diameter, allowance; title: diameter, allowance, type; missing allowance becomes 0 | [src/digitalmodel/data_systems/data_manager/configuration.py:L138-L177] |
| `file_list`, extraction `columns` and `data` | UTF-8 YAML files; column labels; L1/L2/L3 keys | Both extraction keys required; traversal stops at first absent level; no schema or unit conversion | [src/digitalmodel/data_systems/data_manager/configuration.py:L254-L290] [src/digitalmodel/data_systems/data_manager/configuration.py:L294-L316] |

Output contract:

| Output | Unit or convention | Contents and limits | Source |
|---|---|---|---|
| Loaded/current mapping | Unconverted input values | Mutable internal mapping returned directly; default snapshot is a deep copy before overlays | [src/digitalmodel/data_systems/data_manager/configuration.py:L61-L86] [src/digitalmodel/data_systems/data_manager/configuration.py:L104-L111] |
| Analysis input mapping | Input units retained | OD, ID, t, E, Poissionsratio, alphafab, k, S, U, CorrosionAllowance, designFactors, externalPressure, internalPressure, LimitState | [src/digitalmodel/data_systems/data_manager/configuration.py:L205-L229] |
| Extraction DataFrame | Values retained; filename is first column | Missing files and YAML errors skip rows; missing nested keys insert `None` | [src/digitalmodel/data_systems/data_manager/configuration.py:L260-L292] |
| Validation warning list | Count of messages | Required sections, diameter/thickness consistency and positive material properties only | [src/digitalmodel/data_systems/data_manager/configuration.py:L325-L353] |
| Saved YAML | UTF-8, block style, indent 2 | Creates parent directories and serializes current mapping; save errors re-raised | [src/digitalmodel/data_systems/data_manager/configuration.py:L360-L376] |

No coordinate, origin, sign or phase transform is implemented in the configuration or extraction routes; these routes copy values or perform scalar diameter arithmetic. [src/digitalmodel/data_systems/data_manager/configuration.py:L88-L102] [src/digitalmodel/data_systems/data_manager/configuration.py:L113-L132] [src/digitalmodel/data_systems/data_manager/configuration.py:L205-L220] [src/digitalmodel/data_systems/data_manager/configuration.py:L294-L316]

## 3. Method and governing standards

| Method | Implementation and reference | Limits |
|---|---|---|
| YAML loading and overlay | `load_configuration` uses `yaml.safe_load`; nested `deep_merge` recurses only when both values are dictionaries, otherwise replaces the value | Empty or false YAML becomes `{}`; no duplicate-key rejection or top-level mapping validation is added here. [src/digitalmodel/data_systems/data_manager/configuration.py:L61-L102] |
| Diameter completion | `update_geometry_properties`: ID = OD − 2 × WT; OD = ID + 2 × WT; elementary concentric-pipe geometry | Does not derive WT from OD and ID, despite the broader docstring; supplied diameters are not overwritten. [src/digitalmodel/data_systems/data_manager/configuration.py:L113-L132] |
| Geometry consistency | `validate_configuration`: expected WT = (OD − ID) / 2; warning when absolute error exceeds `1e-6` in the input length unit | Absolute tolerance has no declared physical unit; no warning if one geometry field is absent. [src/digitalmodel/data_systems/data_manager/configuration.py:L334-L343] |
| Extraction | `extract_data_from_files` and `_extract_nested_value` walk up to three mapping levels and assemble one row per successfully parsed file | Failed rows and values are treated differently; see section 6. [src/digitalmodel/data_systems/data_manager/configuration.py:L263-L316] |
| Plot metadata and persistence | `update_plot_settings`, `reset_to_defaults`, `save_configuration` format strings, restore a deep copy and serialize YAML | Filename formatting replaces every period with `p`, including an extension separator. [src/digitalmodel/data_systems/data_manager/configuration.py:L159-L177] [src/digitalmodel/data_systems/data_manager/configuration.py:L355-L376] |
| API STD 2RD association | Module and manager docstrings associate the configuration route with API STD 2RD analysis | No edition, clause, table or standards-derived calculation is identified in this package; no `Citation` sidecar is emitted by its return/save paths. [src/digitalmodel/data_systems/data_manager/configuration.py:L1-L8] [src/digitalmodel/data_systems/data_manager/configuration.py:L21-L24] [src/digitalmodel/data_systems/data_manager/configuration.py:L205-L233] [src/digitalmodel/data_systems/data_manager/configuration.py:L360-L376] |

UNVERIFIED: API STD 2RD edition and clause applicability remain unspecified; clause text not checked against the licensed copy; issue pending (4). [src/digitalmodel/data_systems/data_manager/configuration.py:L1-L8] [docs/domains/structural/api_std_2rd_migration_summary.md:L1-L17]

The migration summary names the same standard without an edition; an edition disagreement cannot be established from these two documents. [src/digitalmodel/data_systems/data_manager/configuration.py:L1-L8] [docs/domains/structural/api_std_2rd_migration_summary.md:L1-L17]

## 4. Validated worked examples

Validated examples: 0; regression examples with executable test node IDs: 0; the dedicated example is a TODO, and the migration test file skips at module level before defining its tests. [examples/data_systems/data_manager/basic_usage.py:L1-L2] [tests/test_apistd2rd_migration.py:L19-L31]

The configuration demonstration is a workflow lead, not a validated or regression case: it loads a generated mapping, derives a diameter and prints validation and input preparation results without an independent expected value or test node ID. [examples/domains/api_standards/apistd2rd_demo.py:L161-L206]

The dormant configuration and YAML-security assertions are tracked in section 10 rather than cited as runnable examples; the offline file run reported SKIP at collection, not PASS for either assertion; issue pending (3). [tests/test_apistd2rd_migration.py:L23-L26] [tests/test_apistd2rd_migration.py:L94-L138] [tests/test_apistd2rd_migration.py:L182-L203]

## 5. QA gates

Gate assignment: AQG r0, section 10, Python calculations; G5 applies under section 9.

| Gate | Applicability | Module-specific checks and reason |
|---|---|---|
| G0 | Applicable | C01–C05 check mapping inputs, fallbacks, defaults, geometry and extraction completeness. [src/digitalmodel/data_systems/data_manager/configuration.py:L37-L102] [src/digitalmodel/data_systems/data_manager/configuration.py:L179-L353] |
| G1 | Applicable | C06–C07 use Python imports and offline pytest; the module writes generic YAML, not a program-specific solver deck. [src/digitalmodel/data_systems/data_manager/configuration.py:L360-L376] [tests/test_apistd2rd_migration.py:L19-L31] |
| G2 | Applicable | C08 compares diameter completion with elementary annulus geometry, independently rearranged by hand. [src/digitalmodel/data_systems/data_manager/configuration.py:L113-L132] |
| G3 | NOT_APPLICABLE | No time integration is implemented by the input adapter; AQG r0 section 10 excludes Python calculations. [src/digitalmodel/data_systems/data_manager/configuration.py:L179-L233] |
| G4 | NOT_APPLICABLE | No diffraction body or mesh is produced by the input adapter; AQG r0 section 10 excludes Python calculations. [src/digitalmodel/data_systems/data_manager/configuration.py:L179-L233] |
| G5 | Applicable | C09–C10 require roundtrip/reset evidence and a cited record of inputs, outputs, omissions and checks. [src/digitalmodel/data_systems/data_manager/configuration.py:L355-L376] |

Reproduction basis for C01–C09: use an isolated temporary directory and the public `digitalmodel.data_systems.data_manager.configuration` import; synthetic base mapping `geometry={NominalOD:12, DesignWT:1}`, `material={E:20, SMYS:2, SMUS:3}`, `designFactors={}`; quantities use arbitrary consistent length/stress units, not design data or standard values; use `yaml.safe_dump` for fixtures and call the functions named below. [src/digitalmodel/data_systems/data_manager/configuration.py:L37-L86] [src/digitalmodel/data_systems/data_manager/configuration.py:L113-L132] [src/digitalmodel/data_systems/data_manager/configuration.py:L192-L233]

These are reproducible audit checks, not new repository tests; checks that reproduce a fallback identify exposure rather than approve that fallback for engineering use. [src/digitalmodel/data_systems/data_manager/configuration.py:L80-L84] [src/digitalmodel/data_systems/data_manager/configuration.py:L263-L290]

| ID | Gate | Procedure and numeric pass criterion, with unit | Evidence and offline result |
|---|---|---|---|
| C01 | G0 | Overlay `geometry={NominalID:10}` and `extra={key:1}`; `load_configuration` must retain all base material keys and exactly 3 geometry keys, with 0 unequal values; missing default and unsupported Python YAML tag each raise 1 exception (count). | PASS: recursive merge and 2 expected exceptions; no tag payload executed. [src/digitalmodel/data_systems/data_manager/configuration.py:L56-L102] |
| C02 | G0 | Reproduce exactly 10 fallback scenarios (count): empty/false default YAML; empty/false/malformed/missing overlay; filename/title `{missing}` format errors; absent geometry/plot sections; duplicate `value` YAML keys. Match the source outcomes with 0 mismatches (count). Duplicate-key probe uses `value:1` then `value:2` on separate YAML lines. | PASS: defaults normalize to `{}`, overlays retain base, templates remain unchanged, absent sections do nothing, duplicate key retains 2. This detects permissive behaviour, not strict input validity. [src/digitalmodel/data_systems/data_manager/configuration.py:L61-L84] [src/digitalmodel/data_systems/data_manager/configuration.py:L119-L177] |
| C03 | G0 | After diameter completion, omit method data: `get_analysis_input_data` returns exactly 3 `None` material values, 3 zero allowance/pressure values and 1 `design` string (7 values, count). With two rows containing internal pressure 1 and 2 in synthetic pressure units, default index selects 1 and index −1 selects 2, with 0 mismatches (count). | PASS: all 7 defaults and both index selections reproduced. [src/digitalmodel/data_systems/data_manager/configuration.py:L179-L227] |
| C04 | G0 | Set geometry to OD alone; separately set complete geometry WT and material E to NaN: `validate_configuration` returns 0 warnings for each (count). Change WT to 2 with OD 12 and ID 10: exactly 1 warning (count). | PASS: exposes missing completeness/finiteness enforcement; source tolerance is `1e-6` caller length units, not an engineering acceptance unit. [src/digitalmodel/data_systems/data_manager/configuration.py:L334-L351] |
| C05 | G0 | `extract_data_from_files`: request 5 files (count): valid nested value 7, missing file, malformed YAML, missing nested key, empty YAML. Columns `[filename,value]`, path L1=`root`, L2=`value`. Require 3 returned rows and 2 missing values (counts); empty file list returns 0 rows; L1=`root`, L3=`value` with L2 absent returns the root subtree, 0 mapping differences (count). | PASS: 2 skipped files, 2 missing values and truncated traversal reproduced. [src/digitalmodel/data_systems/data_manager/configuration.py:L254-L316] |
| C06 | G1 | Import canonical and legacy configuration routes in a fresh Python process. Require 4 callable exports through each of 2 routes and 0 licensed interface imports (counts). | PASS: both routes expose the manager and 3 wrappers; no solver required. [src/digitalmodel/data_systems/data_manager/__init__.py:L8-L20] [src/digitalmodel/_compat.py:L129-L129] [src/digitalmodel/data_systems/data_manager/configuration.py:L379-L423] |
| C07 | G1 | Run the offline command below before and after documentation edits. Require 0 newly failing tests and 0 change in result counts (tests); record existing skips separately, never as passes. | BASE: 590 passed, 0 failed, 1 module-level skip; AFTER: 590 passed, 0 failed, 1 module-level skip; no change in counts. Migration skip is in the pinned test source. [tests/test_apistd2rd_migration.py:L19-L26] |
| C08 | G2 | Hand annulus check: for OD/WT pairs `(12,1)`, `(8,2)`, `(30,3)`, subtract WT twice for ID, then add WT twice for recovered OD. Require 6 results with 0 error in caller length units. With OD and ID only, require 0 new WT keys (count), documenting the docstring limitation. | PASS: all 6 diameter values exact; missing WT remains missing. These are algebra checks, not validated section-4 cases. [src/digitalmodel/data_systems/data_manager/configuration.py:L113-L132] |
| C09 | G5 | Overlay then `reset_to_defaults`; mutate restored geometry and reset again; save/read YAML; call all 3 wrappers. Require 0 mapping differences (count), including retained default snapshot and in-place `customUpdate` diameter. | PASS: reset isolation, roundtrip and wrappers agree with their source contracts. [src/digitalmodel/data_systems/data_manager/configuration.py:L61-L65] [src/digitalmodel/data_systems/data_manager/configuration.py:L355-L423] |
| C10 | G5 | Audit the record: every repo citation resolves at its declared pin (0 missing files or out-of-range lines); all 30 section-6 rows name a detection check or an explicit missing check (0 omissions); every open item has an issue/draft (0 untracked items); every example has a collectable node/result (0 unsupported examples). Units: counts. | PASS: all citation ranges resolve; branch audit and draft linkage are complete; name scan and pack lint are clean; no section-4 example is promoted from the skipped tests. [tests/test_apistd2rd_migration.py:L19-L26] [examples/data_systems/data_manager/basic_usage.py:L1-L2] |

Offline test command, with `uv` invoked by its installed executable and cache/temp locations supplied by the environment:

```bash
PYTHONPATH=src uv run --no-sources python -m pytest \
  tests/test_apistd2rd_migration.py tests/contracts \
  tests/docs/test_digitalmodel_routing_contract.py \
  -q -rA -p no:cacheprovider --basetemp="$TMPDIR/pytest-run"
```

Test pin: `aebd7e4bc5c3dda5f7433e20e021dd58ac17d0b3`; documentation-only AFTER checks run on the uncommitted tree with module/test sources still at that pin. No pytest configuration overrides or test deselections are used. [tests/test_apistd2rd_migration.py:L19-L26]

## 6. Known failure modes and pitfalls

sweep: 30 found

Scope: every package `.py` file, followed by branch-by-branch inspection; the count includes intentional defaults and no-op paths so they are visible alongside error suppression. [src/digitalmodel/data_systems/data_manager/configuration.py:L26-L376] [src/digitalmodel/data_systems/data_manager/__init__.py:L1-L20]

Exact candidate search commands:

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|\bor \{\}|= 0(\.0)?\b|1970|0\.05|5\s*%|int\(|float\(|\.get\(|unit|cache' src/digitalmodel/data_systems/data_manager --glob '*.py'
rg -n '^\s*(return|break)|safe_load|warnings|Nominal|DesignWT' src/digitalmodel/data_systems/data_manager --glob '*.py'
```

| ID | Symptom and cause at the code pin | Detection check; tracking |
|---|---|---|
| S01 | Empty or false default YAML becomes `{}`, losing the distinction from an empty mapping. [src/digitalmodel/data_systems/data_manager/configuration.py:L61-L65] | C02; issue pending (1) |
| S02 | Empty or false overlay becomes `{}`, so no change is distinguishable in the returned mapping. [src/digitalmodel/data_systems/data_manager/configuration.py:L75-L79] | C02; issue pending (1) |
| S03 | Overlay YAML error is logged and defaults returned. [src/digitalmodel/data_systems/data_manager/configuration.py:L80-L82] | C02; issue pending (1) |
| S04 | Missing requested overlay logs a warning and defaults remain. [src/digitalmodel/data_systems/data_manager/configuration.py:L83-L86] | C02; issue pending (1) |
| S05 | Missing or underdetermined geometry makes diameter completion a no-op, including OD/ID without WT despite the docstring. [src/digitalmodel/data_systems/data_manager/configuration.py:L113-L132] | C02 covers absent geometry; C08 covers absent WT; issue pending (2) |
| S06 | Missing geometry or plotSettings makes plot update a no-op. [src/digitalmodel/data_systems/data_manager/configuration.py:L138-L139] | C02; issue pending (1) |
| S07 | Missing variableWTBurst defaults to empty dictionaries; empty settings return without an update. [src/digitalmodel/data_systems/data_manager/configuration.py:L141-L145] | no detection check exists for both nested empty-settings branches; issue pending (1) |
| S08 | Plot configuration without either diameter returns unchanged. [src/digitalmodel/data_systems/data_manager/configuration.py:L148-L155] | no detection check exists; issue pending (1) |
| S09 | Missing geometry corrosion allowance formats as 0. [src/digitalmodel/data_systems/data_manager/configuration.py:L157-L165] | no detection check exists for successful zero-allowance plot formatting; issue pending (2) |
| S10 | Filename KeyError/ValueError is suppressed and leaves the template. [src/digitalmodel/data_systems/data_manager/configuration.py:L160-L167] | C02 covers KeyError; no detection check exists for ValueError; issue pending (1) |
| S11 | Title KeyError/ValueError is suppressed and leaves the template. [src/digitalmodel/data_systems/data_manager/configuration.py:L169-L177] | C02 covers KeyError; no detection check exists for ValueError; issue pending (1) |
| S12 | Method index defaults to 0; negative indices select from the end without range validation. [src/digitalmodel/data_systems/data_manager/configuration.py:L179-L201] | C03; issue pending (2) |
| S13 | Missing method section or data key becomes `{}` and activates input defaults. [src/digitalmodel/data_systems/data_manager/configuration.py:L199-L203] | C03; issue pending (2) |
| S14 | Absent Poissionsratio becomes `None` outside the required-field list. [src/digitalmodel/data_systems/data_manager/configuration.py:L210-L224] | C03; issue pending (2) |
| S15 | Absent alphafab becomes `None` outside the required-field list. [src/digitalmodel/data_systems/data_manager/configuration.py:L211-L224] | C03; issue pending (2) |
| S16 | Absent k becomes `None` outside the required-field list. [src/digitalmodel/data_systems/data_manager/configuration.py:L212-L224] | C03; issue pending (2) |
| S17 | Missing method corrosion allowance becomes 0. [src/digitalmodel/data_systems/data_manager/configuration.py:L215-L215] | C03; issue pending (2) |
| S18 | Missing external pressure becomes 0. [src/digitalmodel/data_systems/data_manager/configuration.py:L217-L217] | C03; issue pending (2) |
| S19 | Missing internal pressure becomes 0. [src/digitalmodel/data_systems/data_manager/configuration.py:L218-L218] | C03; issue pending (2) |
| S20 | Missing limit state becomes `design`. [src/digitalmodel/data_systems/data_manager/configuration.py:L219-L219] | C03; issue pending (2) |
| S21 | Missing extraction file is skipped after a warning. [src/digitalmodel/data_systems/data_manager/configuration.py:L266-L268] | C05; issue pending (1) |
| S22 | Empty or false extraction YAML becomes `{}`. [src/digitalmodel/data_systems/data_manager/configuration.py:L270-L272] | C05 covers empty YAML; no detection check exists for false extraction YAML; issue pending (1) |
| S23 | Missing extracted key appends `None` and retains a partial row. [src/digitalmodel/data_systems/data_manager/configuration.py:L277-L283] | C05; issue pending (1) |
| S24 | YAML extraction error skips the file after logging. [src/digitalmodel/data_systems/data_manager/configuration.py:L288-L290] | C05; issue pending (1) |
| S25 | Empty file list or all skipped files returns an empty table without a completeness failure. [src/digitalmodel/data_systems/data_manager/configuration.py:L260-L292] | C05 covers empty file list; no detection check exists for all-files-invalid input; issue pending (1) |
| S26 | First absent L1/L2/L3 key breaks traversal and returns a higher subtree. [src/digitalmodel/data_systems/data_manager/configuration.py:L308-L316] | C05; issue pending (1) |
| S27 | Geometry arithmetic assumes compatible length units, while material and pressure inputs pass through without conversion or unit metadata; the WT tolerance is absolute. [src/digitalmodel/data_systems/data_manager/configuration.py:L125-L131] [src/digitalmodel/data_systems/data_manager/configuration.py:L340-L343] [src/digitalmodel/data_systems/data_manager/configuration.py:L205-L220] | C08 checks compatible-unit algebra only; no detection check exists for mixed units; issue pending (2) |
| S28 | Incomplete geometry and NaN thickness/material values can yield an empty warning list, because comparisons are conditional or false for NaN. [src/digitalmodel/data_systems/data_manager/configuration.py:L334-L353] | C04; issue pending (2) |
| S29 | Loader adds no duplicate-key rejection; repeated keys can overwrite earlier values without a warning from this method. [src/digitalmodel/data_systems/data_manager/configuration.py:L61-L69] | C02 duplicate-key probe; issue pending (1) |
| S30 | Successful filename formatting replaces every period, including the extension separator, with `p`. [src/digitalmodel/data_systems/data_manager/configuration.py:L160-L165] | no detection check exists for extension preservation; issue pending (1) |

The other exception handlers re-raise default YAML, missing input-key and save failures; they do not suppress those errors. [src/digitalmodel/data_systems/data_manager/configuration.py:L67-L69] [src/digitalmodel/data_systems/data_manager/configuration.py:L231-L233] [src/digitalmodel/data_systems/data_manager/configuration.py:L374-L376]

No external-text `int()`/`float()` conversion, epoch fallback, percentage fallback or cache write is implemented in either package file; the initialized empty config/snapshot and warning list are ordinary state, and filename `str()` conversion labels extraction rows rather than parsing numbers. [src/digitalmodel/data_systems/data_manager/__init__.py:L1-L20] [src/digitalmodel/data_systems/data_manager/configuration.py:L26-L35] [src/digitalmodel/data_systems/data_manager/configuration.py:L263-L353]

Additional pitfalls: geometry completion does not infer WT; plot filenames replace all periods; extraction column count must match filename plus requested values; input preparation validates presence of six fields but not positivity or numeric type. [src/digitalmodel/data_systems/data_manager/configuration.py:L113-L132] [src/digitalmodel/data_systems/data_manager/configuration.py:L159-L167] [src/digitalmodel/data_systems/data_manager/configuration.py:L222-L227] [src/digitalmodel/data_systems/data_manager/configuration.py:L275-L286]

No incident issue was supplied for this module; the dormant migration skip and placeholder example provide the local evidence for the coverage gap, with no passing detection test for those module methods; issue pending (3). [tests/test_apistd2rd_migration.py:L19-L31] [examples/data_systems/data_manager/basic_usage.py:L1-L2]

## 7. Lessons log

| Date | Change reference | Lesson and evidence | Pack change | Shared-log entry |
|---|---|---|---|---|
| 2026-11-07 | Initial documentation | A migration test file can contain configuration assertions while skipping before their definition. [tests/test_apistd2rd_migration.py:L19-L31] [tests/test_apistd2rd_migration.py:L94-L138] | Sections 4–6 distinguish absent validation from passing surrounding contract tests | Not created; tracked with example/test coverage, issue pending (3) |
| 2026-11-07 | Initial documentation | Extraction logs do not encode attempted/rejected file counts in the returned table. [src/digitalmodel/data_systems/data_manager/configuration.py:L263-L292] | C05 and S21–S25 require an explicit completeness audit | Not created; tracked with observable fallbacks, issue pending (1) |

## 8. Ownership

Review assignment for this draft: calculations family quality manager, provisionally covering data pipelines under AQG r0 section 10; deputy role: calculations deputy quality manager, appointment unrecorded; producing role: module documentation contributor; engineering classification disputes go to the family quality manager; acceptance and unresolved scope go to the maintainer. This is a review assignment, not a record of clearance.

Appointment and shared-log recording remain administrative follow-up with the coverage work, issue pending (3).

## 9. Dependencies and parallel implementations

| Dependency | Relationship and limit | Evidence |
|---|---|---|
| YAML input files | Default mapping, optional recursive overlay, UTF-8; no dataset revision or unit metadata required by loader | [src/digitalmodel/data_systems/data_manager/configuration.py:L56-L102] |
| PyYAML | Safe loading and dumping of mapping values; strict duplicate-key checks are not added by the module | [src/digitalmodel/data_systems/data_manager/configuration.py:L61-L84] [src/digitalmodel/data_systems/data_manager/configuration.py:L369-L376] |
| pandas | Extraction table assembled with one filename column and configured values | [src/digitalmodel/data_systems/data_manager/configuration.py:L254-L292] |
| Python path, logging and deep-copy facilities | Path existence, log-only fallback messages, independent default snapshots and restoration | [src/digitalmodel/data_systems/data_manager/configuration.py:L56-L84] [src/digitalmodel/data_systems/data_manager/configuration.py:L355-L376] |
| Compatibility redirect | Legacy data_manager name maps to data_systems; package re-export uses that legacy route | [src/digitalmodel/_compat.py:L129-L129] [src/digitalmodel/data_systems/data_manager/__init__.py:L8-L20] |
| Separate API analysis adapter | Constructs/accepts this manager, loads configuration, derives geometry/plot metadata and logs validation warnings | [src/digitalmodel/specialized/api_analysis/apistd2rd.py:L41-L73] |

Implementation-map pins: map commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, declared data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; only implementation-map citations below use the map commit rather than the code pin. [docs/registry/implementation-map.yaml:L1-L3]

A8 inventory: empty. No family implementation row lies in this package at the map pin; the one package entry is explicitly a rejected candidate, `ConfigurationManager.validate_configuration`, under `irregular-frequency-treatment`, with membership unestablished and excluded from implementation counts. It does not establish that this adapter implements irregular-frequency treatment. [docs/registry/implementation-map.yaml:L40621-L40627]

A8 counted families: 0; counted-family grep: `NOT_APPLICABLE` because the map has no in-package implementation family; no further-second-implementation total is asserted for an uncounted family. The inventory passes with zero counted families; the missing-method follow-up below is required before these families become counted. [docs/registry/implementation-map.yaml:L40621-L40627]

Whole-repository discovery, including scripts, used the following searches; symbols were inspected at the code pin rather than classified by name alone:

```bash
rg -n 'ConfigurationManager|digitalmodel\.(data_systems\.)?data_manager|def (ymlInput|customUpdate|loadConfiguration)' src scripts tests examples --glob '*.py'
rg -n 'def (update_deep|deep_merge)|NominalOD.*DesignWT|NominalID.*DesignWT|def _extract_nested_value' . --glob '*.py'
```

Selected concrete alternatives and helpers, outside this package:

| Path and definition | Classification and method relationship |
|---|---|
| `src/digitalmodel/infrastructure/utils/ymlInput.py`, `ymlInput`, `update_deep` | Separate YAML loader and recursive mapping merge, including a broad overlay exception fallback. [src/digitalmodel/infrastructure/utils/ymlInput.py:L19-L61] |
| `src/digitalmodel/asset_integrity/common/yml_utilities.py`, `ymlInput`, `update_deep` | Separate loader and recursive merge with stream fallback and overlay warning. [src/digitalmodel/asset_integrity/common/yml_utilities.py:L25-L63] |
| `src/digitalmodel/asset_integrity/common/ymlInput.py`, `ymlInput`, `update_deep` | Separate loader/recursive merge with printed overlay failure. [src/digitalmodel/asset_integrity/common/ymlInput.py:L8-L40] |
| `src/digitalmodel/hydrodynamics/rao_analysis/legacy/dataManager/ymlInput.py`, `ymlInput`, `update_deep` | Separate legacy loader and recursive merge. [src/digitalmodel/hydrodynamics/rao_analysis/legacy/dataManager/ymlInput.py:L7-L36] |
| `src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/dataManager/ymlInput.py`, `ymlInput` | Separate YAML loader with shallow update, not the package's recursive merge. [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/dataManager/ymlInput.py:L6-L23] |
| `src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/dataManager/customUpdate.py`, `customUpdate` | Separate plot formatter; a different settings nesting is used. [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/dataManager/customUpdate.py:L13-L47] |
| `scripts/python/digitalmodel/analysis/ymlInput.py`, `ymlInput` | Separate legacy YAML loader using shallow update and an unqualified loader. [scripts/python/digitalmodel/analysis/ymlInput.py:L3-L18] |
| `scripts/python/digitalmodel/analysis/customUpdate.py`, `customUpdate` | Separate plot formatter with the variableWTBurst nesting. [scripts/python/digitalmodel/analysis/customUpdate.py:L9-L20] |
| `scripts/python/digitalmodel/analysis/ConfigurationManager.py`, `ConfigurationManager` | Different INI/configparser route with numeric text coercion; no equivalence to this YAML adapter is asserted. [scripts/python/digitalmodel/analysis/ConfigurationManager.py:L11-L61] |
| `scripts/python/digitalmodel/analysis/loadConfiguration.py`, `loadConfiguration`; corresponding structural adapter | Delegates to a different manager, not another local formula. [scripts/python/digitalmodel/analysis/loadConfiguration.py:L1-L6] [src/digitalmodel/structural/pipe_capacity/custom/API_STD_2RD/BurstPressure/dataManager/loadConfiguration.py:L3-L9] |
| `src/digitalmodel/signal_processing/signal_analysis/orcaflex/config.py`, `ConfigurationManager`; `src/digitalmodel/solvers/orcaflex/core/configuration.py`, `ConfigurationManager` | Different configured applications, not established duplicate engineering algorithms. [src/digitalmodel/signal_processing/signal_analysis/orcaflex/config.py:L23-L36] [src/digitalmodel/signal_processing/signal_analysis/orcaflex/config.py:L142-L155] [src/digitalmodel/solvers/orcaflex/core/configuration.py:L447-L484] |

Map follow-up, issue pending (5): classify recursive YAML overlay, diameter completion and consistency, nested field extraction, plot metadata formatting, default restoration and YAML persistence before assigning family search signals; these are the methods inventoried in section 3 and its persistence row. External alternatives above are map gaps, not A8 misses inside this package. [src/digitalmodel/data_systems/data_manager/configuration.py:L88-L102] [src/digitalmodel/data_systems/data_manager/configuration.py:L113-L177] [src/digitalmodel/data_systems/data_manager/configuration.py:L294-L376]

## 10. Coverage and open items

Package coverage: both `__init__.py` and `configuration.py` read in full; the initializer exports four symbols and the configuration implementation contains the methods covered in sections 1–9. [src/digitalmodel/data_systems/data_manager/__init__.py:L1-L20] [src/digitalmodel/data_systems/data_manager/configuration.py:L1-L423]

Documentation discovery was inspected for module references and distinguished from similarly named legacy data directories or unrelated helper functions:

| Discovered document | Relevance and evidence |
|---|---|
| `docs/domains/data_systems/data_manager/README.md` | Direct module README, currently TODO description/API. [docs/domains/data_systems/data_manager/README.md:L1-L17] |
| `docs/domains/structural/api_std_2rd_migration_summary.md` | Direct migration/configuration claims; its coverage, cross-platform and caching claims are not adopted as validation evidence. [docs/domains/structural/api_std_2rd_migration_summary.md:L9-L29] [docs/domains/structural/api_std_2rd_migration_summary.md:L112-L128] [docs/domains/structural/api_std_2rd_migration_summary.md:L183-L207] |
| `docs/migration/legacy-batch-workflows.md` | Refers to legacy data-directory workflows, not proof of this manager's runtime behaviour. [docs/migration/legacy-batch-workflows.md:L30-L49] |
| `docs/migration/legacy-engineering-flow-catalog.md` | Refers to legacy data assets; no private or legacy dataset is used by this pack. [docs/migration/legacy-engineering-flow-catalog.md:L87-L102] |
| `docs/domains/asset-integrity/b31g-validation-2026-06-27.md` | Refers to a legacy external data directory and another module; its tabulated values are not reproduced. [docs/domains/asset-integrity/b31g-validation-2026-06-27.md:L25-L31] |
| `docs/api-contracts.md` | Search hit is an upstream YAML helper contract, not this package's API. [docs/api-contracts.md:L11-L25] |
| `docs/domains/test_automation/fatigue_analysis_mock_patterns.md` | Search hit uses a generic YAML helper in a different test workflow. [docs/domains/test_automation/fatigue_analysis_mock_patterns.md:L192-L200] |
| `docs/domains/testing/engine_test_implementation_plan.md` | Search hits concern engine mocks, not the manager. [docs/domains/testing/engine_test_implementation_plan.md:L46-L51] |
| `docs/domains/project-docs/TEST_BASELINE_SUMMARY.md` | Search hits list upstream utility stubs, not module-specific validation. [docs/domains/project-docs/TEST_BASELINE_SUMMARY.md:L40-L56] |

Test discovery: the explicit manager references occur in `tests/test_apistd2rd_migration.py`; its configuration and YAML-security function definitions remain behind the existing module-level skip; no passing node is asserted for them. [tests/test_apistd2rd_migration.py:L19-L31] [tests/test_apistd2rd_migration.py:L94-L138] [tests/test_apistd2rd_migration.py:L182-L203]

Dormant source definitions, not collected example node IDs: `tests/test_apistd2rd_migration.py::TestAPISTD2RDMigration::test_configuration_manager` and `tests/test_apistd2rd_migration.py::TestAPISTD2RDMigration::test_yaml_security`; both suppressed by the same file-level SKIP, not independently executed. [tests/test_apistd2rd_migration.py:L23-L26] [tests/test_apistd2rd_migration.py:L94-L138] [tests/test_apistd2rd_migration.py:L182-L203]

The complete tests-contracts aggregate was not invoked: its compatibility imports include diffraction, whose package initializer imports the solver subpackage; the focused contracts and routing tests above avoid that interface path. This aggregate coverage limit is tracked with the test work, issue pending (3). [tests/compat/test_group_compat.py:L85-L112] [src/digitalmodel/hydrodynamics/diffraction/__init__.py:L58-L68]

| Open item | Maintainer action and tracking | Evidence |
|---|---|---|
| Observable YAML/plot/extraction outcomes | Resolve S01–S04, S06–S08, S10–S11, S21–S26 and S29–S30; add missing detection checks; issue pending (1) | [src/digitalmodel/data_systems/data_manager/configuration.py:L61-L84] [src/digitalmodel/data_systems/data_manager/configuration.py:L138-L177] [src/digitalmodel/data_systems/data_manager/configuration.py:L263-L316] |
| Types, units, geometry and explicit defaults | Resolve S05, S09, S12–S20 and S27–S28, including absent WT derivation, finite values and caller-unit tolerance; issue pending (2) | [src/digitalmodel/data_systems/data_manager/configuration.py:L113-L132] [src/digitalmodel/data_systems/data_manager/configuration.py:L157-L219] [src/digitalmodel/data_systems/data_manager/configuration.py:L334-L351] |
| Examples, tests and documentation | Add collectable module tests, a synthetic example with independent expectations, and evidence for or correction of migration claims; record deputy/shared-log assignment; issue pending (3) | [examples/data_systems/data_manager/basic_usage.py:L1-L2] [tests/test_apistd2rd_migration.py:L19-L31] [docs/domains/data_systems/data_manager/README.md:L5-L17] [docs/domains/structural/api_std_2rd_migration_summary.md:L183-L207] |
| Standards provenance | Resolve the single UNVERIFIED claim in section 3: API STD 2RD edition/clause applicability and any required citation sidecar; issue pending (4) | [src/digitalmodel/data_systems/data_manager/configuration.py:L1-L8] [src/digitalmodel/data_systems/data_manager/configuration.py:L205-L233] |
| Implementation-map coverage | Add/classify section-3 method families and outside-package alternatives from section 9; issue pending (5) | [docs/registry/implementation-map.yaml:L40621-L40627] [src/digitalmodel/data_systems/data_manager/configuration.py:L88-L102] [src/digitalmodel/data_systems/data_manager/configuration.py:L113-L177] [src/digitalmodel/data_systems/data_manager/configuration.py:L294-L376] |

The calculation citation contract was readable during preparation; no standards-derived constant was introduced by this documentation change, and no licensed clause text, standard table or private-data value is included. The module's explicit numeric arithmetic is diameter completion and consistency, while design factors are passed through. [src/digitalmodel/data_systems/data_manager/configuration.py:L125-L131] [src/digitalmodel/data_systems/data_manager/configuration.py:L205-L220] [src/digitalmodel/data_systems/data_manager/configuration.py:L340-L343]
