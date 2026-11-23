# Model generator knowledge pack

| Header | Value |
| --- | --- |
| Module | solvers/orcaflex/model_generator |
| Repository | digitalmodel |
| Package | src/digitalmodel/solvers/orcaflex/model_generator |
| Pack revision | 0, 2026-10-18 |
| Code revision | 29a87f1aba14aa22067794846aa5c2ec8bf407c1 |
| Analysis family | OrcaFlex riser and mooring; MR |
| Quality role | Riser and mooring quality manager |
| Status | DRAFT; independent clearance outstanding |

Source citations resolve at the code revision; execution results below describe the offline inspection of that revision.

## 1. Purpose and scope

`OrcaFlexModelGenerator` reads component CSV tables, merges selected properties and overrides into a template dictionary, adds metadata, and optionally writes YAML. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L192-L266] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L268-L335]
The package scope is the single implementation file and its README; the solver domain has maturity `beta`, without a separate model-generator maturity field at this pin. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L1-L4] [docs/registry/module-routing.yaml:L374-L410]
`generate_model()` creates a default generator and delegates to `generate_from_template()`; it returns a dictionary, including when an output path is provided. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L463-L476]
Generation has no solver call or validation call; `validate()` is a separate operation with structural errors and advisory property warnings. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L228-L266] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L354-L400]
UNVERIFIED: Supported target-program versions and engineering applicability limits are not established by the generation or validation routines; issue pending (1). [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L228-L266] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L354-L400]

## 2. Inputs and outputs

Input contract:

| Input | Unit | Allowed range and default | Source and behavior |
| --- | --- | --- | --- |
| `components_dir`, `templates_dir` | filesystem path | Optional; defaults to the repository template tree | Constructor accepts custom paths; no existence check during construction. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L56-L84] |
| `category`, `component_id` | string, no physical unit | Directory category or two-part CSV category; no ID normalization | First CSV column supplies IDs; a directory search returns the first matching row. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L100-L131] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L152-L190] |
| `template` | relative path string | Required; missing `model_template.yml` raises `FileNotFoundError` | Read by `yaml.safe_load`. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L235-L246] |
| `config` | dictionary or YAML path | Required argument; an empty dictionary is accepted | YAML is loaded without a schema; only recognized assembly branches are used. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L228-L246] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L276-L335] |
| `vessel`, `riser`, `environment` | property-specific; no conversion | Processed only with `lookup`; remaining keys overwrite selected properties | Lookup categories are fixed in the assembly routine. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L276-L322] |
| `model`, `analysis` | property-specific | Optional; model keys replace whole top-level values, analysis updates its mapping | No allowed-value ranges are enforced here. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L324-L335] |
| `output` | filesystem path | Optional, default `None` | Parent directories created; destination opened in write mode. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L256-L266] |
| `add_component` properties | property-specific | Required dictionary; category must exist | Writes a CSV then invalidates matching cache keys. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L426-L459] |

Output contract:

| Output | Unit and format | Range/default | Source |
| --- | --- | --- | --- |
| Component list / mapping | IDs / raw CSV properties | Missing listing category returns empty list; missing lookup raises | No common unit conversion is performed. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L100-L190] |
| Model dictionary / optional file | Mapping / YAML; physical units inherited from input | Metadata replaces `_metadata`; timestamp varies between calls | Writer uses `yaml.dump(..., sort_keys=False)`, without an explicit encoding. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L248-L266] |
| Validation record | Boolean, error/warning/check-name lists | Starts valid with empty lists; requires a `Vessel` or `Line` key | Missing recommended properties only add warnings. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L354-L400] |

The shipped template contains initial positions, orientations, end coordinates and wave/current directions; the generator copies template mappings without coordinate transformations. [docs/domains/orcaflex/templates/risers/scr_catenary/model_template.yml:L9-L101] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L274-L335]
UNVERIFIED: A complete coordinate origin, handedness, sign and phase contract, and a unit schema for arbitrary CSV/configuration properties, are not supplied by the assembly code; issue pending (3). [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L268-L335]

## 3. Method and governing standards

| Method | Implementing definition | Reference and limit |
| --- | --- | --- |
| Component discovery and lookup | `list_components`, `get_component`, `_load_component_csv` | CSV first-column identity, first matching row, instance cache; no engineering equation. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L100-L204] |
| Template assembly | `_assemble_model` | Shallow dictionary copy; component merge, explicit overrides, top-level replacement, analysis update, in that order. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L274-L335] |
| Generation and export | `generate_from_template`, `generate_model` | Read YAML, assemble, record metadata, optional YAML dump; wrapper delegates. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L228-L266] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L463-L476] |
| Structural advice | `validate` | Missing top-level content is an error; missing vessel/line properties and high wave height are warnings. This is not an independent mechanics reference. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L354-L400] |
| Library mutation | `add_component` | First discovered CSV or a new custom table; appends raw properties, then clears cache entries with the category prefix. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L426-L459] |

| Standard named by local documentation | Edition, clause/table, implementation |
| --- | --- |
| DNV-OS-F201, Dynamic Risers | Edition and clause/table absent; wall-thickness claim has no corresponding check in `validate`. Clause text not checked against the licensed copy. [src/digitalmodel/solvers/orcaflex/model_generator/README.md:L409-L419] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L354-L400] |
| API RP 2RD, Design of Risers | Edition and clause/table absent; no standard-specific calculation in `validate`. Clause text not checked against the licensed copy. [src/digitalmodel/solvers/orcaflex/model_generator/README.md:L415-L419] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L354-L400] |
| API RP 2SK, Stationkeeping Systems | Edition and clause/table absent; no stationkeeping calculation in `validate`. Clause text not checked against the licensed copy. [src/digitalmodel/solvers/orcaflex/model_generator/README.md:L415-L419] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L354-L400] |
| DNV-RP-F105, Free Spanning Pipelines | Edition and clause/table absent; no free-span calculation in `validate`. Clause text not checked against the licensed copy. [src/digitalmodel/solvers/orcaflex/model_generator/README.md:L415-L419] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L354-L400] |
| ISO 13628, Subsea Production Systems | Part, edition and clause/table absent; no corresponding calculation in `validate`. Clause text not checked against the licensed copy. [src/digitalmodel/solvers/orcaflex/model_generator/README.md:L415-L419] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L354-L400] |

UNVERIFIED: README performance, standards-compliance and pre-validation claims are not demonstrated by these checks or the regression suite; no conflicting edition can be reconciled because the local standard list gives no editions; issue pending (1). [src/digitalmodel/solvers/orcaflex/model_generator/README.md:L398-L429] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L354-L400] [tests/solvers/orcaflex/test_model_generator.py:L1-L148]
No `Citation` sidecar is emitted by generation or validation; the generation metadata contains a generator string, template string, time and version string. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L248-L266] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L354-L400]
For any future standards-derived calculation, require the calculation-output citation contract and an independently checked edition/clause before treating its output as a QA reference.

## 4. Validated worked examples and regression examples

Validated examples: 0; the discovered tests use synthetic fixture assertions or repository template/library expectations, without an independent engineering benchmark. [tests/solvers/orcaflex/test_model_generator.py:L23-L148]
Regression examples: 10; PASS at BASE and PASS after documentation edits, against the header code revision; no solver, skipped case or NOT_RUN case in this suite. [tests/solvers/orcaflex/test_model_generator.py:L29-L148]

| Case | Input/source | Expected value and tolerance basis | Existing test node ID | Offline result, BASE / after |
| --- | --- | --- | --- | --- |
| R1: Default directories | Repository fixture / template; [tests/solvers/orcaflex/test_model_generator.py:L32-L42] | 0 mismatched paths (count); exact assertions | `tests/solvers/orcaflex/test_model_generator.py::TestDefaultPaths::test_default_dirs_exist` | PASS / PASS |
| R2: Default property lookup | Repository fixture / template; [tests/solvers/orcaflex/test_model_generator.py:L44-L51] | 0 mismatched asserted CSV properties (count); repository expectations, not independently qualified; exact assertions | `tests/solvers/orcaflex/test_model_generator.py::TestDefaultPaths::test_default_get_component_vessels` | PASS / PASS |
| R3: Default ID listing | Repository fixture / template; [tests/solvers/orcaflex/test_model_generator.py:L53-L58] | 1 required ID present (count); exact assertions | `tests/solvers/orcaflex/test_model_generator.py::TestDefaultPaths::test_default_list_components_vessels` | PASS / PASS |
| R4: Nested directory listing | Repository fixture / template; [tests/solvers/orcaflex/test_model_generator.py:L64-L81] | 2 IDs in fixture order (count); exact assertions | `tests/solvers/orcaflex/test_model_generator.py::TestSlashCategoryHandling::test_list_components_directory_category_with_slash` | PASS / PASS |
| R5: Synthetic numeric lookup | Repository fixture / template; [tests/solvers/orcaflex/test_model_generator.py:L83-L88] | Value = 2.0, tolerance 0 (dimensionless); exact assertions | `tests/solvers/orcaflex/test_model_generator.py::TestSlashCategoryHandling::test_get_component_directory_category_with_slash` | PASS / PASS |
| R6: Missing component | Repository fixture / template; [tests/solvers/orcaflex/test_model_generator.py:L90-L94] | 1 ComponentNotFoundError (count); exact assertions | `tests/solvers/orcaflex/test_model_generator.py::TestSlashCategoryHandling::test_get_component_missing_raises` | PASS / PASS |
| R7: Synthetic append and lookup | Repository fixture / template; [tests/solvers/orcaflex/test_model_generator.py:L96-L106] | Value = 3.0, tolerance 0 (dimensionless); exact assertions | `tests/solvers/orcaflex/test_model_generator.py::TestSlashCategoryHandling::test_add_component_directory_category_with_slash` | PASS / PASS |
| R8: Two-part CSV category | Repository fixture / template; [tests/solvers/orcaflex/test_model_generator.py:L108-L114] | 1 required ID present (count); exact assertions | `tests/solvers/orcaflex/test_model_generator.py::TestSlashCategoryHandling::test_lines_risers_csv_category` | PASS / PASS |
| R9: Template export round trip | Repository fixture / template; [tests/solvers/orcaflex/test_model_generator.py:L120-L140] | 0 mismatched asserted depth/metadata values (count); template expectation, not independent physics; exact assertions | `tests/solvers/orcaflex/test_model_generator.py::TestGenerateFromTemplate::test_generate_from_template_slash_name` | PASS / PASS |
| R10: Missing template | Repository fixture / template; [tests/solvers/orcaflex/test_model_generator.py:L142-L148] | 1 FileNotFoundError (count); exact assertions | `tests/solvers/orcaflex/test_model_generator.py::TestGenerateFromTemplate::test_generate_from_template_missing_raises` | PASS / PASS |

Run form: `PYTHONPATH=src uv run --no-sources --extra test python -m pytest tests/solvers/orcaflex/test_model_generator.py -q -rA -p no:cacheprovider --basetemp=<scratch-case-directory>`; declared test extras installed, original addopts retained, serial execution, 10 run / 10 passed / 0 failed / 0 skipped / 0 NOT_RUN in each run. [tests/solvers/orcaflex/test_model_generator.py:L29-L148]
The append test writes only to its temporary component fixture; export writes only to its temporary model destination. [tests/solvers/orcaflex/test_model_generator.py:L64-L106] [tests/solvers/orcaflex/test_model_generator.py:L120-L140]

## 5. QA gates

The following are acceptance checks for generated deliverables, not an assertion that the package has passed the engineering gates (AQG r0, sections 3–10).

| Gate | Applicability | Module-specific acceptance check and evidence | Recorded state |
| --- | --- | --- | --- |
| G0 inputs | Applicable | C0.1–C0.5 below; component selection, schema, units and complete provenance (AQG r0, section 4) | Regression path PASS; qualification gaps remain |
| G1 load | Applicable, OrcaFlex only | C1: load the exact `generate_from_template` YAML in the target OrcaFlex version through a matching-version demonstration program where available, otherwise licensed `Model.LoadData`; 0 load errors, 0 warnings, 0 object-count mismatches (count), retain version/messages and input digest (AQG r0, section 5) | NOT_RUN: requires target program; no licensed interface imported |
| G2 statics | Applicable | C2: for every generated configuration, converge statics; compare independently calculated top tension within 5 %, angle within 1 degree and touchdown distance within 5 %; offset sweep at least 6 steps, 0 unconverged steps and 0 unchecked components (count); retain tables/images (AQG r0, section 6) | NOT_RUN: requires licensed solver; this package supplies no hand-calculation reference |
| G3 short dynamics | Applicable | C3: simulate the generated model for 1 s at production settings; 0 warnings, 0 nonnumeric responses (count), tension within 25 % of static; retain final image and time (AQG r0, section 7) | NOT_RUN: requires licensed solver |
| G4 diffraction | Conditional; NOT_APPLICABLE for regression fixtures with no new diffraction run | C4: when supplying newly diffracted vessel data to `_assemble_model`, require 1 matching diffraction record per vessel type (count); displacement within 0.5 % and weight/buoyancy within 1.0 %; retain source record and input digest (AQG r0, sections 8 and 10) | NOT_APPLICABLE for these fixtures; new diffraction data require a separate record |
| G5 evidence appendix | Applicable to any report using generated output | C5: tie every generated model and dependency to its gate records; 0 missing digests, 0 missing applicable gate dispositions, 0 unsigned records (count); generate appendix from records (AQG r0, section 9) | NOT_RUN: no analysis report or engineering gate record supplied |

| Check | Executable procedure | Numeric pass criterion and unit | Evidence / gate |
| --- | --- | --- | --- |
| C0.1 paths and lookup | Run all section-4 nodes against pinned CSV/template inputs | 10 passing nodes; 0 failed/skipped nodes (count) | Pytest node results / G0 |
| C0.2 identity and cache | In a scratch library, list a missing category, a header-only CSV and a directory without CSVs; try duplicate IDs, a leading-zero ID and a numeric field containing a thousands separator; edit a cached CSV, then re-read and compare all returned values | 0 silently missing IDs; 0 duplicates selected; 0 unintended type/value changes; 0 stale results (count); reject ambiguous/empty inputs explicitly | Compare disk CSV to list/get results; reproduces section-6 gaps / G0 |
| C0.3 assembly fidelity | Deep-copy a template as reference; call `_assemble_model` with independent vessel, riser and environment overrides with and without lookup; compare input and output recursively; also exercise top-level replacement | 0 changes to source template; 0 silently ignored overrides; 0 unacknowledged replaced sections (count) | Before/after mappings; reproduces section-6 gaps / G0 |
| C0.4 schema, validation and units | Call `validate` on empty sections, missing fields, negative dimensions and boundary wave heights; serialize a `None` value and duplicate-key input; independently check exported property names, selector order, expected units and target schema | 0 missing required fields; 0 invalid dimensions; 0 duplicate keys; 0 forbidden null tokens; 0 unit/schema discrepancies (count); wave height is compared in m | Validation record, YAML and independent schema/unit checklist; current key checks do not establish this pass / G0 |
| C0.5 provenance | Generate a model; enumerate all input, template, component and referenced files, recompute sorted relative-path SHA-256 records and compare with manifest; repeat after changing one referenced input | 0 missing/mismatched digests (count); exactly 3 resolvable immutable generator/template/design-basis revisions (count) | Manifest comparison; metadata alone is insufficient / G0 |

Checks defined: G0 = 5; G1 = 1; G2 = 1; G3 = 1; G4 = 1 conditional; G5 = 1; total = 10 (AQG r0, sections 4–10).
C0.2–C0.5 are reproducible inspection procedures for deficiencies, not shipped automated tests; the engineering gates remain outstanding. An offline scratch inspection confirmed 11 deficiency observations covering absent/header-only listings, duplicate selection, stale cache reads, source-template mutation, ignored overrides, permissive validation, duplicate-key loading, null emission and missing fingerprints; these observations do not constitute engineering-gate passes. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L100-L204] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L228-L400]

## 6. Known failure modes and pitfalls

sweep: 18 found

The sweep covers the complete package Python source; the rows group individual behavior sites, including benign defaults that need explicit interpretation. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L1-L483]
Commands used (read as text only):

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|unit|cache' src/digitalmodel/solvers/orcaflex/model_generator --glob '*.py'
rg -n 'None|\[\]|\{\}|get\(|read_csv|safe_load|copy\(|update\(|iloc|columns\[0\]|dump|Hs|lookup' src/digitalmodel/solvers/orcaflex/model_generator --glob '*.py'
```

| Site | Symptom / cause at the pin | Detection check / disposition |
| --- | --- | --- |
| S01 | Missing listing category logs a warning and returns `[]`, which can be mistaken for a valid empty library. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L119-L121] | C0.2; issue pending (2) |
| S02 | Existing directory with no CSV files or header-only tables returns an empty list; the initial empty list and column guards do not distinguish unavailable content. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L100-L131] | C0.2; issue pending (2) |
| S03 | Listing assumes the first CSV column contains IDs; values are returned without identity normalization. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L109-L129] | C0.2; issue pending (2) |
| S04 | Lookup assumes the first column contains IDs and returns the first duplicate match; duplicate rows/tables are not rejected. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L159-L186] | C0.2; issue pending (2) |
| S05 | CSV text is passed directly to `pd.read_csv`; there is no explicit identifier dtype or locale/separator contract. No explicit external-text `int()` or `float()` conversion occurs. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L192-L204] | C0.2; issue pending (2) |
| S06 | A header-only DataFrame is cached without checking row count; missing files do raise. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L196-L204] | C0.2; issue pending (2) |
| S07 | The initially empty instance cache is benign, but loaded values have no input digest/mtime invalidation on an external CSV edit. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L79-L80] | C0.2; issue pending (2) |
| S08 | Default paths are constructed without existence checks; listing can mask missing installation data through S01. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L56-L84] | C0.1 and C0.2; issue pending (2) |
| S09 | Configuration and template use non-strict `yaml.safe_load`; no schema or duplicate-key rejection is requested. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L228-L243] | C0.4; issue pending (3) |
| S10 | Shallow template copy aliases nested mappings; vessel updates can mutate the caller template. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L274-L285] | C0.3; issue pending (3) |
| S11 | Riser and environment nested updates share the same shallow-copy hazard. Their empty-mapping defaults only apply when a section is absent. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L292-L322] | C0.3; issue pending (3) |
| S12 | Component override sections without `lookup` are ignored, including environment overrides. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L276-L322] | C0.3; issue pending (3) |
| S13 | Model-level assignment replaces complete template sections; analysis defaults to an empty mapping then updates without allowed-value checks. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L324-L333] | C0.3 and C0.4; issue pending (3) |
| S14 | Default `is_valid=True` and empty error/warning/check lists are not standards compliance: empty component mappings and missing properties produce warnings without invalidation. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L354-L389] | C0.4; issue pending (1) |
| S15 | The high-wave-height comparison assumes m and only warns above its code threshold; no lower bound, finite-value or comprehensive unit checks are present. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L391-L400] | C0.4; issue pending (1) |
| S16 | Output uses ordinary YAML dumping and write mode, with no explicit encoding, vendor-null spelling, target-version schema or output-path overwrite guard. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L256-L262] | C0.4; issue pending (3) |
| S17 | `add_component` chooses the first globbed CSV, or creates `custom.csv` at a zero-file count; properties supply the stored ID independently of the `component_id` argument, and duplicate checks cover only the selected existing CSV. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L433-L453] | C0.2; issue pending (2) |
| S18 | Cache invalidation uses string prefix matching; neighboring category prefixes may also be invalidated; caller edits outside this method remain stale. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L456-L459] | C0.2; issue pending (2) |

No swallowed exception, record-skipping `continue`, `return None`, explicit numeric-zero fallback, epoch default or error-path empty-dictionary return occurs in this source; the empty mappings, lists and zero-file branch are accounted for above. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L1-L483]
Additional confirmed pitfalls:

| Symptom | Evidence | Detection / tracking |
| --- | --- | --- |
| Generation is described as automatically standards-validated, although generation never invokes validation | README narrative versus generation and validation bodies. [src/digitalmodel/solvers/orcaflex/model_generator/README.md:L398-L419] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L228-L266] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L354-L400] | C0.4; issue pending (1); observed 2026-10-18 |
| Metadata does not bind inputs to immutable design/template revisions | Generation records names, time and a literal version string. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L248-L254] | C0.5; related #2515 tracks the neighboring generator's provenance gap; issue pending (3) extends this package's acceptance |
| Selector-sensitive controls can pass through model/analysis overrides without filtering | Assembly applies raw dictionaries. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L324-L333] | C0.4 and C1; #2132 is a related generator audit, not evidence of a reproduced native load failure here |
| Historical examples use a different import path, and a dictionary is described as a model file passed to a runner | Example imports and workflow variables conflict with this package's dictionary return. [docs/domains/orcaflex/QUICKSTART_MODEL_GENERATOR.md:L223-L259] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L256-L266] | no detection check exists for historical documentation execution; issue pending (4) |

The discovered issue references are leads or related work, not confirmed incidents in this package; no incident issue was named in the module brief.

## 7. Lessons log

| Date | Record | Lesson | Evidence | Pack change | Shared entry |
| --- | --- | --- | --- | --- | --- |
| 2026-10-18 | Initial knowledge pack | Dictionary generation and a Boolean validation result do not establish native loadability or engineering qualification | [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L228-L266] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L354-L400] | Separate regression examples from G1–G5 evidence | Not assigned; issue pending (1) |

## 8. Ownership

Proposed review assignment: riser and mooring quality manager; deputy: installation quality manager; producing function: engineering documentation; unresolved engineering criteria go to the family quality manager, and cross-family matters to the maintainer.
UNVERIFIED: Named appointments and an enduring shared-log entry are not established by repository evidence; issue pending (4).

## 9. Dependencies and alternative implementations

Upstream: pandas CSV reading, PyYAML loading, template/component files under the documentation tree, and the Python standard library; downstream: returned dictionaries and optional YAML output. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L6-L11] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L66-L77] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L192-L266]
The local README demonstrates converter, runner and post-processor usage; these are documentation dependencies, without a native solver call in this package. [src/digitalmodel/solvers/orcaflex/model_generator/README.md:L370-L394] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L228-L266]
UNVERIFIED: Compatible solver versions, independent component provenance and inventory rows for every external library are not supplied by these interfaces; issue pending (1). [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L192-L266]

### Every source occurrence naming OrcaFlex

These references were read as text only; identifiers and examples are not evidence of a solver invocation.

| Occurrence | Dependency meaning | Citation |
| --- | --- | --- |
| Module docstring, lines 2–3 | Declared generated-model target | [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L1-L4] |
| Class name/docstring, lines 26 and 28 | Generator target and public class | [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L26-L39] |
| Class example, line 42 | Example constructor | [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L41-L53] |
| Constructor directories, lines 70 and 75 | Default OrcaFlex template/component dependencies | [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L66-L77] |
| Constructor log, line 82 | Initialization message | [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L79-L84] |
| Generation docstring, line 211 | Output description | [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L206-L219] |
| Metadata, line 250 | Generator identifier | [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L248-L254] |
| Wrapper import example/body, lines 472 and 475 | Documented package dependency and default generator construction | [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L465-L476] |
| Export, line 480 | Public class export | [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L479-L483] |

### Map inventory and counted-family audit

The map branch was read at `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, whose recorded data revision is `270c8759c374b67a7f032953e7fcdb14e0224461`; its selected family is structurally identical to the copy at the code pin, so the inventory citations below resolve at the code pin. [docs/registry/implementation-map.yaml:L1-L4]
One map family has in-package rows: `orcaflex-text-model-writers` (2 rows); it is therefore the only counted family, including all 440 implementations in that family across the repository, rather than only the 2 package entries. [docs/registry/implementation-map.yaml:L4921-L9496]
The table retains the map order; W001–W440 are one-based row identities in that family, not claims that all entries are equivalent or independent algorithms. Each row is identified by its map citation; identifying discovery paths are withheld in favor of that stable identity.

| Map row | Implementation (map spelling; module-level when no symbol) | Source in map |
| --- | --- | --- |
| W001 | `docs/domains/examples/orcaflex/run_universal_example.py` — `main` | [docs/registry/implementation-map.yaml:L4928-L4937] |
| W002 | Identifying discovery file; refer to this map row; issue pending (4) | [docs/registry/implementation-map.yaml:L4938-L4946] |
| W003 | `docs/domains/orcaflex/examples/modular/C10/C10 Multiple statics/run_orcaflex.py` — `main` | [docs/registry/implementation-map.yaml:L4947-L4956] |
| W004 | `docs/domains/orcaflex/examples/modular/E08/PipelayConfig/run_orcaflex.py` — `main` | [docs/registry/implementation-map.yaml:L4957-L4966] |
| W005 | `docs/domains/orcaflex/risers/drilling/wd9500ft/Preprocessing code.py` — `strengthInputFilePreperation` | [docs/registry/implementation-map.yaml:L4967-L4976] |
| W006 | `docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_diffraction.py` — `run_orcawave_diffraction` | [docs/registry/implementation-map.yaml:L4977-L4986] |
| W007 | `docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_diffraction_improved.py` — `run_diffraction_with_monitoring` | [docs/registry/implementation-map.yaml:L4987-L4996] |
| W008 | `docs/domains/orcawave/examples/L01_default_vessel/run_orcawave_diffraction_improved.py` — `run_batch_execution` | [docs/registry/implementation-map.yaml:L4997-L5008] |
| W009 | `docs/domains/orcawave/examples/L02 OC4 Semi-sub/run_orcawave.py` — `_load_data_compat` | [docs/registry/implementation-map.yaml:L5009-L5018] |
| W010 | `docs/domains/orcawave/examples/L03 Semi-sub multibody analysis/run_orcawave.py` — `_load_data_compat` | [docs/registry/implementation-map.yaml:L5019-L5028] |
| W011 | `docs/domains/orcawave/examples/L04 Sectional bodies/run_orcawave.py` — `_load_data_compat` | [docs/registry/implementation-map.yaml:L5029-L5038] |
| W012 | `docs/domains/orcawave/examples/L05 Panel pressures/run_orcawave.py` — `_load_data_compat` | [docs/registry/implementation-map.yaml:L5039-L5048] |
| W013 | `examples/domains/workflows/utils.py` — `OrcaFlexExporter.create_mooring_yml` | [docs/registry/implementation-map.yaml:L5049-L5058] |
| W014 | `scripts/audit_spec_library.py` — `write_audit_yaml` | [docs/registry/implementation-map.yaml:L5059-L5068] |
| W015 | `scripts/benchmark/generate_owd_ymls.py` — `generate_yml` | [docs/registry/implementation-map.yaml:L5069-L5078] |
| W016 | `scripts/benchmark/run_spar_benchmark.py` — `main` | [docs/registry/implementation-map.yaml:L5079-L5088] |
| W017 | `scripts/benchmark/solve_owd_extract.py` — `solve_and_extract` | [docs/registry/implementation-map.yaml:L5089-L5098] |
| W018 | `scripts/benchmark/validate_owd_vs_spec.py` — `solve_owd` | [docs/registry/implementation-map.yaml:L5099-L5108] |
| W019 | `scripts/benchmark/validate_owd_vs_spec.py` — `solve_spec` | [docs/registry/implementation-map.yaml:L5109-L5120] |
| W020 | `scripts/benchmark/validate_owd_vs_spec.py` — `_solve_spec_via_orcfxapi` | [docs/registry/implementation-map.yaml:L5121-L5130] |
| W021 | `scripts/benchmark_model_library.py` — `<module>` | [docs/registry/implementation-map.yaml:L5131-L5139] |
| W022 | `scripts/benchmark_model_library.py` — `load_and_run` | [docs/registry/implementation-map.yaml:L5140-L5149] |
| W023 | `scripts/benchmark_model_library.py` — `run_spec_driven` | [docs/registry/implementation-map.yaml:L5150-L5159] |
| W024 | `scripts/benchmark_model_library.py` — `load_json` | [docs/registry/implementation-map.yaml:L5160-L5169] |
| W025 | `scripts/benchmark_model_library.py` — `_trace` | [docs/registry/implementation-map.yaml:L5170-L5179] |
| W026 | `scripts/benchmark_riser_library.py` — `apply_mesh_preset` | [docs/registry/implementation-map.yaml:L5180-L5189] |
| W027 | `scripts/benchmark_riser_library.py` — `run_mesh_sensitivity` | [docs/registry/implementation-map.yaml:L5190-L5199] |
| W028 | `scripts/benchmark_riser_library.py` — `benchmark_model` | [docs/registry/implementation-map.yaml:L5200-L5211] |
| W029 | `scripts/build_sme_report.py` — `make_chart_js` | [docs/registry/implementation-map.yaml:L5212-L5221] |
| W030 | `scripts/build_sme_report.py` — `make_profile_js` | [docs/registry/implementation-map.yaml:L5222-L5231] |
| W031 | `scripts/ci/check_include_targets.py` — `extract_references` | [docs/registry/implementation-map.yaml:L5232-L5241] |
| W032 | `scripts/consolidate_panels.py` — `_build_manual_entries` | [docs/registry/implementation-map.yaml:L5242-L5251] |
| W033 | `scripts/convert_orcaflex_to_yml.py` — `OrcaFlexConverter.convert_file` | [docs/registry/implementation-map.yaml:L5252-L5261] |
| W034 | `scripts/create_flattened_models.py` — `create_flattened_model_dir` | [docs/registry/implementation-map.yaml:L5262-L5271] |
| W035 | `scripts/extract_library_validation.py` — `process_model_dir` | [docs/registry/implementation-map.yaml:L5272-L5281] |
| W036 | `scripts/extract_property_inventory.py` — `_field_to_section_name` | [docs/registry/implementation-map.yaml:L5282-L5291] |
| W037 | `scripts/extract_property_inventory.py` — `write_yaml` | [docs/registry/implementation-map.yaml:L5292-L5301] |
| W038 | `scripts/extract_riser_validation.py` — `main` | [docs/registry/implementation-map.yaml:L5302-L5311] |
| W039 | `scripts/extract_s7_specs.py` — `save_spec` | [docs/registry/implementation-map.yaml:L5312-L5321] |
| W040 | `scripts/fix_orcaflex_structure.py` — `create_env_section_wrappers` | [docs/registry/implementation-map.yaml:L5322-L5331] |
| W041 | `scripts/fix_orcaflex_structure.py` — `create_fixed_analysis_model` | [docs/registry/implementation-map.yaml:L5332-L5341] |
| W042 | `scripts/fix_orcaflex_structure.py` — `fix_sections_for_discretised` | [docs/registry/implementation-map.yaml:L5342-L5351] |
| W043 | `scripts/generate_all_specs.py` — `<module>` | [docs/registry/implementation-map.yaml:L5352-L5360] |
| W044 | `scripts/generate_all_specs.py` — `extract_spec` | [docs/registry/implementation-map.yaml:L5361-L5370] |
| W045 | `scripts/generate_all_specs.py` — `save_spec_yml` | [docs/registry/implementation-map.yaml:L5371-L5380] |
| W046 | `scripts/generate_analysis_files.py` — `create_analysis_file` | [docs/registry/implementation-map.yaml:L5381-L5390] |
| W047 | `scripts/generate_calm_buoy_project.py` — `CALMBuoyProjectGenerator.generate_orcaflex_modules` | [docs/registry/implementation-map.yaml:L5391-L5403] |
| W048 | `scripts/generate_calm_buoy_project.py` — `CALMBuoyProjectGenerator._populate_environment_module` | [docs/registry/implementation-map.yaml:L5404-L5413] |
| W049 | `scripts/generate_calm_buoy_project.py` — `CALMBuoyProjectGenerator._apply_module_customizations` | [docs/registry/implementation-map.yaml:L5414-L5425] |
| W050 | `scripts/generate_calm_buoy_project.py` — `CALMBuoyProjectGenerator._update_base_file_paths` | [docs/registry/implementation-map.yaml:L5426-L5435] |
| W051 | `scripts/generate_calm_buoy_project.py` — `CALMBuoyProjectGenerator.save_updated_config` | [docs/registry/implementation-map.yaml:L5436-L5445] |
| W052 | `scripts/generate_env_files.py` — `create_wave_file` | [docs/registry/implementation-map.yaml:L5446-L5455] |
| W053 | `scripts/generate_env_files.py` — `create_current_file` | [docs/registry/implementation-map.yaml:L5456-L5465] |
| W054 | `scripts/generate_env_files.py` — `create_wind_file` | [docs/registry/implementation-map.yaml:L5466-L5475] |
| W055 | `scripts/initialize_skill_metadata.py` — `create_version_metadata_block` | [docs/registry/implementation-map.yaml:L5476-L5485] |
| W056 | `scripts/inventory/orcaflex_legacy_inventory.py` — `_report_type` | [docs/registry/implementation-map.yaml:L5486-L5495] |
| W057 | `scripts/inventory/orcaflex_legacy_inventory.py` — `classify` | [docs/registry/implementation-map.yaml:L5496-L5507] |
| W058 | `scripts/inventory/orcaflex_legacy_inventory.py` — `main` | [docs/registry/implementation-map.yaml:L5508-L5519] |
| W059 | `scripts/maintenance/archive_candidates.py` — `<module>` | [docs/registry/implementation-map.yaml:L5520-L5528] |
| W060 | `scripts/maintenance/archive_candidates.py` — `main` | [docs/registry/implementation-map.yaml:L5529-L5538] |
| W061 | `scripts/mesh_sensitivity_riser.py` — `apply_mesh_preset` | [docs/registry/implementation-map.yaml:L5539-L5548] |
| W062 | `scripts/orcaflex/check_generic_scr_slwr.py` — `scr_reference` | [docs/registry/implementation-map.yaml:L5549-L5558] |
| W063 | `scripts/orcaflex/check_generic_scr_slwr.py` — `slwr_reference` | [docs/registry/implementation-map.yaml:L5559-L5568] |
| W064 | `scripts/orcaflex/check_generic_scr_slwr.py` — `calculate` | [docs/registry/implementation-map.yaml:L5569-L5581] |
| W065 | `scripts/orcaflex/prepare_slay_campaign.py` — `case_spec` | [docs/registry/implementation-map.yaml:L5582-L5591] |
| W066 | `scripts/orcaflex/prepare_slay_campaign.py` — `prepare_case` | [docs/registry/implementation-map.yaml:L5592-L5603] |
| W067 | `scripts/orcaflex/prepare_slay_campaign.py` — `prepare` | [docs/registry/implementation-map.yaml:L5604-L5616] |
| W068 | `scripts/prepare_mooring_qualification.py` — `_generated_sections` | [docs/registry/implementation-map.yaml:L5617-L5626] |
| W069 | `scripts/python/digitalmodel/doc_tools/check_structure_creation.py` — `ContentAnalyzer._categorize_folder` | [docs/registry/implementation-map.yaml:L5627-L5636] |
| W070 | `scripts/python/digitalmodel/doc_tools/frontmatter_generator.py` — `FrontmatterGenerator.frontmatter_to_yaml` | [docs/registry/implementation-map.yaml:L5637-L5646] |
| W071 | `scripts/python/digitalmodel/legacy/51_-_SerializationYAMLPart1.py` — `<module>` | [docs/registry/implementation-map.yaml:L5647-L5655] |
| W072 | `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` — `make_comparison_loaded_vs_ballast_cxc` | [docs/registry/implementation-map.yaml:L5656-L5665] |
| W073 | `scripts/python/digitalmodel/orcawave/analyze_tug_30m_1.py` — `load_config` | [docs/registry/implementation-map.yaml:L5666-L5675] |
| W074 | `scripts/python/digitalmodel/orcawave/analyze_tug_30m_1.py` — `generate_optimized_config` | [docs/registry/implementation-map.yaml:L5676-L5685] |
| W075 | `scripts/python/digitalmodel/orcawave/create_gdf_consistent.py` — `create_config_with_gdf` | [docs/registry/implementation-map.yaml:L5686-L5695] |
| W076 | `scripts/python/digitalmodel/orcawave/create_working_examples.py` — `create_simple_vessel_config` | [docs/registry/implementation-map.yaml:L5696-L5705] |
| W077 | `scripts/python/digitalmodel/orcawave/create_working_examples.py` — `create_oc4_semisub_config` | [docs/registry/implementation-map.yaml:L5706-L5715] |
| W078 | `scripts/python/digitalmodel/orcawave/create_working_examples.py` — `create_minimal_test_config` | [docs/registry/implementation-map.yaml:L5716-L5725] |
| W079 | `scripts/python/digitalmodel/orcawave/final_working_config.py` — `create_final_working_config` | [docs/registry/implementation-map.yaml:L5726-L5737] |
| W080 | `scripts/python/digitalmodel/orcawave/final_working_config.py` — `create_extended_version` | [docs/registry/implementation-map.yaml:L5738-L5747] |
| W081 | `scripts/python/digitalmodel/orcawave/fix_orcawave_yaml.py` — `create_tug_30m_1_working` | [docs/registry/implementation-map.yaml:L5748-L5757] |
| W082 | `scripts/python/digitalmodel/orcawave/fix_tug_30m_1_errors.py` — `fix_configuration` | [docs/registry/implementation-map.yaml:L5758-L5769] |
| W083 | `scripts/python/digitalmodel/orcawave/fix_tug_30m_1_errors.py` — `create_minimal_test_config` | [docs/registry/implementation-map.yaml:L5770-L5779] |
| W084 | `scripts/python/digitalmodel/orcawave/fix_working_model.py` — `fix_working_model` | [docs/registry/implementation-map.yaml:L5780-L5789] |
| W085 | `scripts/python/digitalmodel/orcawave/fix_working_model.py` — `create_minimal_with_test_box` | [docs/registry/implementation-map.yaml:L5790-L5799] |
| W086 | `scripts/python/digitalmodel/orcawave/validate_orcawave_configs.py` — `OrcaWaveValidator.run_validation` | [docs/registry/implementation-map.yaml:L5800-L5811] |
| W087 | `scripts/python/digitalmodel/tools/add_orcaflex_test_configs.py` — `create_batch_config` | [docs/registry/implementation-map.yaml:L5812-L5821] |
| W088 | `scripts/python/digitalmodel/tools/standardize_orcaflex_tests.py` — `OrcaFlexTestStandardizer.create_standard_config` | [docs/registry/implementation-map.yaml:L5822-L5831] |
| W089 | `scripts/run_riser_analysis.py` — `run_library` | [docs/registry/implementation-map.yaml:L5832-L5841] |
| W090 | `scripts/run_semisub_quality_check.py` — `main` | [docs/registry/implementation-map.yaml:L5842-L5851] |
| W091 | `scripts/sanitize_s7_models.py` — `<module>` | [docs/registry/implementation-map.yaml:L5852-L5860] |
| W092 | `scripts/sanitize_s7_models.py` — `process_dat_file` | [docs/registry/implementation-map.yaml:L5861-L5870] |
| W093 | `scripts/semantic_validate.py` — `<module>` | [docs/registry/implementation-map.yaml:L5871-L5879] |
| W094 | `scripts/sync_monolithic_yml.py` — `convert_dat_to_yml` | [docs/registry/implementation-map.yaml:L5880-L5889] |
| W095 | Identifying discovery file; refer to this map row; issue pending (4) | [docs/registry/implementation-map.yaml:L5890-L5899] |
| W096 | Identifying discovery file; refer to this map row; issue pending (4) | [docs/registry/implementation-map.yaml:L5900-L5909] |
| W097 | `scripts/update_skill_versions.py` — `update_skill_file` | [docs/registry/implementation-map.yaml:L5910-L5921] |
| W098 | `scripts/validate_orcaflex_model.py` — `ModelValidator.extract_model_info` | [docs/registry/implementation-map.yaml:L5922-L5931] |
| W099 | `scripts/verification/verify_orcaflex_campaign.py` — `_save_body` | [docs/registry/implementation-map.yaml:L5932-L5941] |
| W100 | `scripts/verification/verify_orcaflex_campaign.py` — `main` | [docs/registry/implementation-map.yaml:L5942-L5953] |
| W101 | `src/digitalmodel/asset_integrity/common/data.py` — `SaveData.saveDataYaml` | [docs/registry/implementation-map.yaml:L5954-L5963] |
| W102 | `src/digitalmodel/asset_integrity/common/data.py` — `SaveData.WriteOrcaFlexYMLFile` | [docs/registry/implementation-map.yaml:L5964-L5973] |
| W103 | `src/digitalmodel/asset_integrity/common/data.py` — `<module>` | [docs/registry/implementation-map.yaml:L5974-L5982] |
| W104 | `src/digitalmodel/data_systems/data_procurement/common/stream_handler.py` — `StreamHandler._format_orcaflex_yml` | [docs/registry/implementation-map.yaml:L5983-L5992] |
| W105 | `src/digitalmodel/data_systems/data_procurement/mooring/client.py` — `MooringClient.to_orcaflex_mooring` | [docs/registry/implementation-map.yaml:L5993-L6002] |
| W106 | `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/chain_db_client.py` — `ChainDatabaseClient.to_orcaflex_line_type` | [docs/registry/implementation-map.yaml:L6003-L6012] |
| W107 | `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/synthetic_rope_db_client.py` — `SyntheticRopeDatabaseClient.to_orcaflex_line_type` | [docs/registry/implementation-map.yaml:L6013-L6022] |
| W108 | `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/wire_rope_db_client.py` — `WireRopeDatabaseClient.to_orcaflex_line_type` | [docs/registry/implementation-map.yaml:L6023-L6032] |
| W109 | `src/digitalmodel/data_systems/data_procurement/riser/database_clients/pipe_db_client.py` — `PipeSpecificationClient.to_orcaflex_line_type` | [docs/registry/implementation-map.yaml:L6033-L6042] |
| W110 | `src/digitalmodel/data_systems/data_procurement/vessel/client.py` — `VesselClient.to_orcaflex_vessel` | [docs/registry/implementation-map.yaml:L6043-L6052] |
| W111 | `src/digitalmodel/drilling_riser/campaign.py` — `case_spec` | [docs/registry/implementation-map.yaml:L6053-L6062] |
| W112 | `src/digitalmodel/drilling_riser/campaign.py` — `_probe` | [docs/registry/implementation-map.yaml:L6063-L6072] |
| W113 | `src/digitalmodel/drilling_riser/campaign.py` — `_capped_probe` | [docs/registry/implementation-map.yaml:L6073-L6084] |
| W114 | `src/digitalmodel/drilling_riser/campaign.py` — `_statics_path` | [docs/registry/implementation-map.yaml:L6085-L6097] |
| W115 | `src/digitalmodel/drilling_riser/campaign.py` — `physical_state_checks` | [docs/registry/implementation-map.yaml:L6098-L6107] |
| W116 | `src/digitalmodel/drilling_riser/global_model/build.py` — `_line_type` | [docs/registry/implementation-map.yaml:L6108-L6117] |
| W117 | `src/digitalmodel/drilling_riser/global_model/build.py` — `build_generic_spec` | [docs/registry/implementation-map.yaml:L6118-L6129] |
| W118 | `src/digitalmodel/drilling_riser/global_model/build.py` — `write_model` | [docs/registry/implementation-map.yaml:L6130-L6141] |
| W119 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` — `reference_periods` | [docs/registry/implementation-map.yaml:L6142-L6151] |
| W120 | `src/digitalmodel/drilling_riser/global_model/open_water.py` — `build_open_water_generic_spec` | [docs/registry/implementation-map.yaml:L6152-L6161] |
| W121 | `src/digitalmodel/drilling_riser/postprocess/stress_range.py` — `extract` | [docs/registry/implementation-map.yaml:L6162-L6171] |
| W122 | `src/digitalmodel/drilling_riser/postprocess/stress_range.py` — `merge_stress_range` | [docs/registry/implementation-map.yaml:L6172-L6181] |
| W123 | `src/digitalmodel/drilling_riser/postprocess/stress_range.py` — `extract_significant` | [docs/registry/implementation-map.yaml:L6182-L6191] |
| W124 | `src/digitalmodel/floating_wind/solver_tier.py` — `prepare_variant` | [docs/registry/implementation-map.yaml:L6192-L6201] |
| W125 | `src/digitalmodel/hydrodynamics/bemrosetta/converters/to_orcaflex.py` — `OrcaFlexConverter.convert_to_vessel_yaml` | [docs/registry/implementation-map.yaml:L6202-L6211] |
| W126 | `src/digitalmodel/hydrodynamics/diffraction/benchmark_mesh_schematic.py` — `_add_fdf_surface_traces` | [docs/registry/implementation-map.yaml:L6212-L6221] |
| W127 | `src/digitalmodel/hydrodynamics/diffraction/diffraction_cli.py` — `run_aqwa` | [docs/registry/implementation-map.yaml:L6222-L6231] |
| W128 | `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` — `DiffractionSpec.to_yaml` | [docs/registry/implementation-map.yaml:L6232-L6241] |
| W129 | `src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py` — `OrcaFlexExporter.export_vessel_type` | [docs/registry/implementation-map.yaml:L6242-L6251] |
| W130 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py` — `_write_yml` | [docs/registry/implementation-map.yaml:L6252-L6261] |
| W131 | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` — `OrcaWaveRunner._execute_via_api` | [docs/registry/implementation-map.yaml:L6262-L6271] |
| W132 | `src/digitalmodel/hydrodynamics/hull_library/panel_catalog.py` — `PanelCatalog.to_yaml` | [docs/registry/implementation-map.yaml:L6272-L6281] |
| W133 | `src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py` — `RAOAnalysis.write_filtered_orcaflex_seastate_rao_file` | [docs/registry/implementation-map.yaml:L6282-L6291] |
| W134 | `src/digitalmodel/hydrodynamics/rao_analysis/rao_analysis.py` — `RAOAnalysis.get_shape_data` | [docs/registry/implementation-map.yaml:L6292-L6301] |
| W135 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.get_viv_modes_excited` | [docs/registry/implementation-map.yaml:L6302-L6311] |
| W136 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.tabulate_viv_results` | [docs/registry/implementation-map.yaml:L6312-L6321] |
| W137 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.tabulate_overall_viv_results` | [docs/registry/implementation-map.yaml:L6322-L6331] |
| W138 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.save_viv_histograms` | [docs/registry/implementation-map.yaml:L6332-L6341] |
| W139 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.save_combined_histograms` | [docs/registry/implementation-map.yaml:L6342-L6351] |
| W140 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.save_fatigue_summary_result` | [docs/registry/implementation-map.yaml:L6352-L6361] |
| W141 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | [docs/registry/implementation-map.yaml:L6362-L6371] |
| W142 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py` — `DNVRPH103_hydrodynamics_circular.save_model` | [docs/registry/implementation-map.yaml:L6372-L6381] |
| W143 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_circular.py` — `DNVRPH103_hydrodynamics_circular.update_6d_buoy` | [docs/registry/implementation-map.yaml:L6382-L6391] |
| W144 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py` — `DNVRPH103_hydrodynamics_rectangular.save_model` | [docs/registry/implementation-map.yaml:L6392-L6401] |
| W145 | `src/digitalmodel/infrastructure/base_solvers/hydrodynamics/code_dnvrph103_hydrodynamics_rectangular.py` — `DNVRPH103_hydrodynamics_rectangular.update_6d_buoy` | [docs/registry/implementation-map.yaml:L6402-L6411] |
| W146 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.orcaflex_model` | [docs/registry/implementation-map.yaml:L6412-L6424] |
| W147 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.riser_joints_to_line_types` | [docs/registry/implementation-map.yaml:L6425-L6436] |
| W148 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.GetGeneral` | [docs/registry/implementation-map.yaml:L6437-L6446] |
| W149 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.lineTypes` | [docs/registry/implementation-map.yaml:L6447-L6456] |
| W150 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.WriteOrcaflexModel` | [docs/registry/implementation-map.yaml:L6457-L6466] |
| W151 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.build_model` | [docs/registry/implementation-map.yaml:L6467-L6478] |
| W152 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` — `FEAComponents.AssignGeneral` | [docs/registry/implementation-map.yaml:L6479-L6488] |
| W153 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` — `FEAComponents.AssignBuoyProperties` | [docs/registry/implementation-map.yaml:L6489-L6498] |
| W154 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` — `FEAComponents.AssignLineProperties` | [docs/registry/implementation-map.yaml:L6499-L6508] |
| W155 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` — `FEAComponents.AssignLineTypeProperties` | [docs/registry/implementation-map.yaml:L6509-L6518] |
| W156 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` — `FEAComponents.AssignVesselProperties` | [docs/registry/implementation-map.yaml:L6519-L6528] |
| W157 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` — `FEAComponents.save_output` | [docs/registry/implementation-map.yaml:L6529-L6538] |
| W158 | `src/digitalmodel/infrastructure/base_solvers/structural/fea/FEAComponents.py` — `FEAComponents.load_variations` | [docs/registry/implementation-map.yaml:L6539-L6548] |
| W159 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.get_viv_modes_excited` | [docs/registry/implementation-map.yaml:L6549-L6558] |
| W160 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.tabulate_viv_results` | [docs/registry/implementation-map.yaml:L6559-L6568] |
| W161 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.tabulate_overall_viv_results` | [docs/registry/implementation-map.yaml:L6569-L6578] |
| W162 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.save_viv_histograms` | [docs/registry/implementation-map.yaml:L6579-L6588] |
| W163 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.save_combined_histograms` | [docs/registry/implementation-map.yaml:L6589-L6598] |
| W164 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.save_fatigue_summary_result` | [docs/registry/implementation-map.yaml:L6599-L6608] |
| W165 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | [docs/registry/implementation-map.yaml:L6609-L6618] |
| W166 | `src/digitalmodel/infrastructure/utils/data.py` — `SaveData.saveDataYaml` | [docs/registry/implementation-map.yaml:L6619-L6628] |
| W167 | `src/digitalmodel/infrastructure/utils/data.py` — `SaveData.WriteOrcaFlexYMLFile` | [docs/registry/implementation-map.yaml:L6629-L6638] |
| W168 | `src/digitalmodel/infrastructure/utils/data.py` — `<module>` | [docs/registry/implementation-map.yaml:L6639-L6647] |
| W169 | `src/digitalmodel/marine_ops/installation/jumper_input.py` — `generate_input` | [docs/registry/implementation-map.yaml:L6648-L6660] |
| W170 | `src/digitalmodel/marine_ops/installation/jumper_installation_schema.py` — `JumperInstallationSpec.to_yaml` | [docs/registry/implementation-map.yaml:L6661-L6670] |
| W171 | `src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py` — `build_line_types` | [docs/registry/implementation-map.yaml:L6671-L6680] |
| W172 | `src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py` — `build_modular_spec` | [docs/registry/implementation-map.yaml:L6681-L6692] |
| W173 | `src/digitalmodel/marine_ops/marine_analysis/unified_rao_reader.py` — `UnifiedRAOReader._detect_format` | [docs/registry/implementation-map.yaml:L6693-L6702] |
| W174 | `src/digitalmodel/orcaflex/model_builder.py` — `VesselConfig.to_orcaflex_dict` | [docs/registry/implementation-map.yaml:L6703-L6712] |
| W175 | `src/digitalmodel/orcaflex/model_builder.py` — `Buoy6DConfig.to_orcaflex_dict` | [docs/registry/implementation-map.yaml:L6713-L6722] |
| W176 | `src/digitalmodel/orcaflex/model_builder.py` — `LineConfig.to_orcaflex_dict` | [docs/registry/implementation-map.yaml:L6723-L6732] |
| W177 | `src/digitalmodel/orcaflex/model_builder.py` — `build_scr_model` | [docs/registry/implementation-map.yaml:L6733-L6742] |
| W178 | `src/digitalmodel/orcaflex/model_builder.py` — `build_lazy_wave_model` | [docs/registry/implementation-map.yaml:L6743-L6752] |
| W179 | `src/digitalmodel/orcaflex/model_builder.py` — `build_mooring_line` | [docs/registry/implementation-map.yaml:L6753-L6762] |
| W180 | `src/digitalmodel/orcaflex/reporting/sections/model_summary.py` — `build_model_summary` | [docs/registry/implementation-map.yaml:L6763-L6772] |
| W181 | `src/digitalmodel/orcaflex/reporting/sections/range_graphs.py` — `build_range_graphs` | [docs/registry/implementation-map.yaml:L6773-L6782] |
| W182 | `src/digitalmodel/orcawave/reporting/sections/rao_plots.py` — `build_rao_plots` | [docs/registry/implementation-map.yaml:L6783-L6792] |
| W183 | `src/digitalmodel/parametric/query.py` — `_write_result` | [docs/registry/implementation-map.yaml:L6793-L6802] |
| W184 | `src/digitalmodel/parametrics/pilots.py` — `orcawave_depth_sweep.runner` | [docs/registry/implementation-map.yaml:L6803-L6812] |
| W185 | `src/digitalmodel/solvers/benchmark/solvers.py` — `_write_diffraction_inputs` | [docs/registry/implementation-map.yaml:L6813-L6822] |
| W186 | `src/digitalmodel/solvers/fea_model/LineType_components.py` — `LineType.get_general` | [docs/registry/implementation-map.yaml:L6823-L6832] |
| W187 | `src/digitalmodel/solvers/fea_model/general_components.py` — `General.getGeneralOrcaFlexSetUp` | [docs/registry/implementation-map.yaml:L6833-L6842] |
| W188 | `src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py` — `load_csv` | [docs/registry/implementation-map.yaml:L6843-L6852] |
| W189 | `src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py` — `get_data` | [docs/registry/implementation-map.yaml:L6853-L6862] |
| W190 | `src/digitalmodel/solvers/orcaflex/core/configuration.py` — `OrcaFlexConfig.to_yaml` | [docs/registry/implementation-map.yaml:L6863-L6872] |
| W191 | `src/digitalmodel/solvers/orcaflex/core/model_interface.py` — `OrcaFlexModelWrapper.save_simulation` | [docs/registry/implementation-map.yaml:L6873-L6882] |
| W192 | `src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py` — `FeatureAnalyzer.analyze_file` | [docs/registry/implementation-map.yaml:L6883-L6892] |
| W193 | `src/digitalmodel/solvers/orcaflex/examples_integration/analyzer.py` — `FeatureAnalyzer.generate_summary` | [docs/registry/implementation-map.yaml:L6893-L6902] |
| W194 | `src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py` — `OrcaFlexBatchConverter._real_convert` | [docs/registry/implementation-map.yaml:L6903-L6915] |
| W195 | `src/digitalmodel/solvers/orcaflex/examples_integration/batch_converter.py` — `OrcaFlexBatchConverter._mock_convert` | [docs/registry/implementation-map.yaml:L6916-L6925] |
| W196 | `src/digitalmodel/solvers/orcaflex/examples_integration/converter.py` — `YamlConverter.convert_batch` | [docs/registry/implementation-map.yaml:L6926-L6935] |
| W197 | `src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py` — `DownloadedExamplesIntegrator._infer_components` | [docs/registry/implementation-map.yaml:L6936-L6945] |
| W198 | `src/digitalmodel/solvers/orcaflex/examples_integration/integrate_downloaded_examples.py` — `DownloadedExamplesIntegrator._extract_common_configs` | [docs/registry/implementation-map.yaml:L6946-L6955] |
| W199 | `src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py` — `KnowledgeIntegrator._extract_patterns` | [docs/registry/implementation-map.yaml:L6956-L6965] |
| W200 | `src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py` — `KnowledgeIntegrator.build_searchable_index` | [docs/registry/implementation-map.yaml:L6966-L6975] |
| W201 | `src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py` — `OrcFxAPIConverter._initialize_api` | [docs/registry/implementation-map.yaml:L6976-L6985] |
| W202 | `src/digitalmodel/solvers/orcaflex/examples_integration/orcfxapi_converter.py` — `OrcFxAPIConverter.convert_file` | [docs/registry/implementation-map.yaml:L6986-L6995] |
| W203 | `src/digitalmodel/solvers/orcaflex/format_converter/format_detector.py` — `detect_format` | [docs/registry/implementation-map.yaml:L6996-L7005] |
| W204 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter.convert` | [docs/registry/implementation-map.yaml:L7006-L7019] |
| W205 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter._merge_master` | [docs/registry/implementation-map.yaml:L7020-L7029] |
| W206 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_single.py` — `ModularToSingleConverter._extract_includefiles.traverse` | [docs/registry/implementation-map.yaml:L7030-L7039] |
| W207 | `src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py` — `ModularToSpecConverter.convert` | [docs/registry/implementation-map.yaml:L7040-L7049] |
| W208 | `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` — `SingleToModularConverter._write_master_file` | [docs/registry/implementation-map.yaml:L7050-L7059] |
| W209 | `src/digitalmodel/solvers/orcaflex/format_converter/single_to_modular.py` — `SingleToModularConverter._write_parameters_file` | [docs/registry/implementation-map.yaml:L7060-L7069] |
| W210 | `src/digitalmodel/solvers/orcaflex/inverse_resolver.py` — `_resolve_mooring` | [docs/registry/implementation-map.yaml:L7070-L7079] |
| W211 | `src/digitalmodel/solvers/orcaflex/inverse_resolver.py` — `_resolve_riser` | [docs/registry/implementation-map.yaml:L7080-L7089] |
| W212 | `src/digitalmodel/solvers/orcaflex/library_generator.py` — `LibraryGenerator.generate_component` | [docs/registry/implementation-map.yaml:L7090-L7099] |
| W213 | `src/digitalmodel/solvers/orcaflex/library_generator.py` — `LibraryGenerator.write_index` | [docs/registry/implementation-map.yaml:L7100-L7109] |
| W214 | `src/digitalmodel/solvers/orcaflex/model_catalogue_render.py` — `object_counts` | [docs/registry/implementation-map.yaml:L7110-L7119] |
| W215 | `src/digitalmodel/solvers/orcaflex/model_catalogue_render.py` — `geometry` | [docs/registry/implementation-map.yaml:L7120-L7129] |
| W216 | `src/digitalmodel/solvers/orcaflex/model_generator/__init__.py` — `OrcaFlexModelGenerator.generate_from_template` | [docs/registry/implementation-map.yaml:L7130-L7141] |
| W217 | `src/digitalmodel/solvers/orcaflex/model_generator/__init__.py` — `OrcaFlexModelGenerator._assemble_model` | [docs/registry/implementation-map.yaml:L7142-L7151] |
| W218 | `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py` — `ModularModelGenerator.generate` | [docs/registry/implementation-map.yaml:L7152-L7164] |
| W219 | `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py` — `ModularModelGenerator._write_parameters` | [docs/registry/implementation-map.yaml:L7165-L7174] |
| W220 | `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py` — `ModularModelGenerator._write_master` | [docs/registry/implementation-map.yaml:L7175-L7184] |
| W221 | `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py` — `ModularModelGenerator.generate_with_overrides` | [docs/registry/implementation-map.yaml:L7185-L7195] |
| W222 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/general_builder.py` — `GeneralBuilder.build` | [docs/registry/implementation-map.yaml:L7196-L7205] |
| W223 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/generic_builder.py` — `GenericModelBuilder.build` | [docs/registry/implementation-map.yaml:L7206-L7215] |
| W224 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/lines_builder.py` — `LinesBuilder.build` | [docs/registry/implementation-map.yaml:L7216-L7225] |
| W225 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py` — `LineTypeBuilder.build` | [docs/registry/implementation-map.yaml:L7226-L7235] |
| W226 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_lines_builder.py` — `RiserLinesBuilder.build` | [docs/registry/implementation-map.yaml:L7236-L7245] |
| W227 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_linetype_builder.py` — `RiserLineTypeBuilder.build` | [docs/registry/implementation-map.yaml:L7246-L7257] |
| W228 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_linetype_builder.py` — `RiserLineTypeBuilder._build_riser_line_type` | [docs/registry/implementation-map.yaml:L7258-L7267] |
| W229 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_vessel_builder.py` — `RiserVesselBuilder.build` | [docs/registry/implementation-map.yaml:L7268-L7277] |
| W230 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/vessel_builder.py` — `VesselBuilder.build` | [docs/registry/implementation-map.yaml:L7278-L7287] |
| W231 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` — `WinchBuilder.build` | [docs/registry/implementation-map.yaml:L7288-L7297] |
| W232 | `src/digitalmodel/solvers/orcaflex/modular_generator/cli.py` — `cmd_generate` | [docs/registry/implementation-map.yaml:L7298-L7307] |
| W233 | `src/digitalmodel/solvers/orcaflex/modular_generator/extractor.py` — `MonolithicExtractor._extract_simulation` | [docs/registry/implementation-map.yaml:L7308-L7317] |
| W234 | `src/digitalmodel/solvers/orcaflex/modular_generator/extractor.py` — `MonolithicExtractor._extract_general_properties` | [docs/registry/implementation-map.yaml:L7318-L7327] |
| W235 | `src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py` — `PostGenerationValidator._check_line_type_references` | [docs/registry/implementation-map.yaml:L7328-L7337] |
| W236 | `src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py` — `PostGenerationValidator._check_vessel_references` | [docs/registry/implementation-map.yaml:L7338-L7347] |
| W237 | `src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py` — `PostGenerationValidator._check_winch_references` | [docs/registry/implementation-map.yaml:L7348-L7357] |
| W238 | `src/digitalmodel/solvers/orcaflex/modular_generator/post_validator.py` — `_iter_includefile_refs.traverse` | [docs/registry/implementation-map.yaml:L7358-L7367] |
| W239 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py` — `MooringRouter.route` | [docs/registry/implementation-map.yaml:L7368-L7377] |
| W240 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py` — `MooringRouter._chain_linetype` | [docs/registry/implementation-map.yaml:L7378-L7387] |
| W241 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py` — `MooringRouter._wire_linetype` | [docs/registry/implementation-map.yaml:L7388-L7397] |
| W242 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py` — `MooringRouter._polyester_linetype` | [docs/registry/implementation-map.yaml:L7398-L7407] |
| W243 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/vessel_router.py` — `VesselRouter.route` | [docs/registry/implementation-map.yaml:L7408-L7417] |
| W244 | `src/digitalmodel/solvers/orcaflex/modular_generator/routers/vessel_router.py` — `VesselRouter.from_hull_catalog` | [docs/registry/implementation-map.yaml:L7418-L7427] |
| W245 | `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` — `CampaignGenerator.generate` | [docs/registry/implementation-map.yaml:L7428-L7437] |
| W246 | `src/digitalmodel/solvers/orcaflex/modular_generator/writers/basefile.py` — `write_variation_model` | [docs/registry/implementation-map.yaml:L7438-L7447] |
| W247 | `src/digitalmodel/solvers/orcaflex/modular_input_validation/utils.py` — `extract_includefiles.traverse` | [docs/registry/implementation-map.yaml:L7448-L7457] |
| W248 | `src/digitalmodel/solvers/orcaflex/modular_input_validation/validator.py` — `ModularInputValidator._collect_issues_from_level_1` | [docs/registry/implementation-map.yaml:L7458-L7467] |
| W249 | `src/digitalmodel/solvers/orcaflex/mooring.py` — `Mooring.prepare_includefile_for_all_lines` | [docs/registry/implementation-map.yaml:L7468-L7477] |
| W250 | `src/digitalmodel/solvers/orcaflex/mooring.py` — `Mooring.prepare_includefile_for_fender_compression` | [docs/registry/implementation-map.yaml:L7478-L7487] |
| W251 | `src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py` — `MockOrcaFlexAPI.load_model` | [docs/registry/implementation-map.yaml:L7488-L7497] |
| W252 | `src/digitalmodel/solvers/orcaflex/mooring_tension_iteration/orcaflex_interface.py` — `OrcaFlexAPI.save_model` | [docs/registry/implementation-map.yaml:L7498-L7507] |
| W253 | `src/digitalmodel/solvers/orcaflex/operability_analysis.py` — `OperabilityAnalyzer.generate_operability_envelope` | [docs/registry/implementation-map.yaml:L7508-L7517] |
| W254 | `src/digitalmodel/solvers/orcaflex/opp_visualization.py` — `OPPVisualization.combine_models` | [docs/registry/implementation-map.yaml:L7518-L7527] |
| W255 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` — `OrcaFlexAnalysis.save_data` | [docs/registry/implementation-map.yaml:L7528-L7539] |
| W256 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` — `OrcaFlexAnalysis.saveSummaryToExcel` | [docs/registry/implementation-map.yaml:L7540-L7549] |
| W257 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` — `OrcaFlexAnalysis.save_histograms` | [docs/registry/implementation-map.yaml:L7550-L7559] |
| W258 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` — `OrcaFlexAnalysis.save_cfg_files_from_multiple_files` | [docs/registry/implementation-map.yaml:L7560-L7569] |
| W259 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` — `OrcaFlexAnalysis.save_RAOs` | [docs/registry/implementation-map.yaml:L7570-L7579] |
| W260 | `src/digitalmodel/solvers/orcaflex/orcaflex_converter_enhanced.py` — `OrcaFlexConverterEnhanced._real_convert` | [docs/registry/implementation-map.yaml:L7580-L7591] |
| W261 | `src/digitalmodel/solvers/orcaflex/orcaflex_converter_enhanced.py` — `OrcaFlexConverterEnhanced._mock_convert` | [docs/registry/implementation-map.yaml:L7592-L7601] |
| W262 | `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` — `OrcaFlexCustomAnalysis.process_fea` | [docs/registry/implementation-map.yaml:L7602-L7614] |
| W263 | `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` — `OrcaFlexCustomAnalysis.clean_model` | [docs/registry/implementation-map.yaml:L7615-L7624] |
| W264 | `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` — `OrcaFlexCustomAnalysis.save_model_with_calculated_positions` | [docs/registry/implementation-map.yaml:L7625-L7634] |
| W265 | `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` — `OrcaFlexCustomAnalysis.iterate_to_target_value` | [docs/registry/implementation-map.yaml:L7635-L7644] |
| W266 | `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` — `OrcaFlexCustomAnalysis.save_data` | [docs/registry/implementation-map.yaml:L7645-L7654] |
| W267 | `src/digitalmodel/solvers/orcaflex/orcaflex_installation.py` — `OrcInstallation.create_model_for_water_depth` | [docs/registry/implementation-map.yaml:L7655-L7664] |
| W268 | `src/digitalmodel/solvers/orcaflex/orcaflex_installation.py` — `OrcInstallation.create_installation_depth_model` | [docs/registry/implementation-map.yaml:L7665-L7674] |
| W269 | `src/digitalmodel/solvers/orcaflex/orcaflex_installation.py` — `OrcInstallation.create_installation_depth_model2` | [docs/registry/implementation-map.yaml:L7675-L7684] |
| W270 | `src/digitalmodel/solvers/orcaflex/orcaflex_iterative_runs.py` — `OrcaflexIterativeRuns.save_iterative_file` | [docs/registry/implementation-map.yaml:L7685-L7694] |
| W271 | `src/digitalmodel/solvers/orcaflex/orcaflex_iterative_runs.py` — `OrcaflexIterativeRuns.run_iterative_simulations` | [docs/registry/implementation-map.yaml:L7695-L7704] |
| W272 | `src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py` — `OrcaflexLineTypes.get_umbilical_properties` | [docs/registry/implementation-map.yaml:L7705-L7717] |
| W273 | `src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py` — `OrcaflexLineTypes.assign_key_umbilical_properties` | [docs/registry/implementation-map.yaml:L7718-L7727] |
| W274 | `src/digitalmodel/solvers/orcaflex/orcaflex_linetypes.py` — `OrcaflexLineTypes.save_model` | [docs/registry/implementation-map.yaml:L7728-L7737] |
| W275 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.orcaflex_model` | [docs/registry/implementation-map.yaml:L7738-L7750] |
| W276 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.riser_joints_to_line_types` | [docs/registry/implementation-map.yaml:L7751-L7762] |
| W277 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.GetGeneral` | [docs/registry/implementation-map.yaml:L7763-L7772] |
| W278 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.lineTypes` | [docs/registry/implementation-map.yaml:L7773-L7782] |
| W279 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.WriteOrcaflexModel` | [docs/registry/implementation-map.yaml:L7783-L7792] |
| W280 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.build_model` | [docs/registry/implementation-map.yaml:L7793-L7804] |
| W281 | `src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel.py` — `OrcaFlexOptimizedParallelAnalysis.process_single_file` | [docs/registry/implementation-map.yaml:L7805-L7814] |
| W282 | `src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel_v2.py` — `process_single_file_worker` | [docs/registry/implementation-map.yaml:L7815-L7824] |
| W283 | `src/digitalmodel/solvers/orcaflex/orcaflex_parallel_analysis.py` — `OrcaFlexParallelAnalysis.process_single_file` | [docs/registry/implementation-map.yaml:L7825-L7834] |
| W284 | `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` — `OrcaflexUtilities.update_model` | [docs/registry/implementation-map.yaml:L7835-L7844] |
| W285 | `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` — `OrcaflexUtilities.update_input_file` | [docs/registry/implementation-map.yaml:L7845-L7854] |
| W286 | `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` — `OrcaflexUtilities.get_full_yaml_file_and_save` | [docs/registry/implementation-map.yaml:L7855-L7866] |
| W287 | `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` — `OrcaflexUtilities.get_general_yml` | [docs/registry/implementation-map.yaml:L7867-L7876] |
| W288 | `src/digitalmodel/solvers/orcaflex/orcaflex_utilities.py` — `OrcaflexUtilities.save_cfg_files_from_multiple_files` | [docs/registry/implementation-map.yaml:L7877-L7886] |
| W289 | `src/digitalmodel/solvers/orcaflex/orcaflex_yml_converter.py` — `convert_to_yml` | [docs/registry/implementation-map.yaml:L7887-L7896] |
| W290 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` — `PipelineSchematicGenerator._merge_yaml` | [docs/registry/implementation-map.yaml:L7897-L7909] |
| W291 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` — `PipelineSchematicGenerator._merge_lines` | [docs/registry/implementation-map.yaml:L7910-L7919] |
| W292 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` — `PipelineSchematicGenerator._merge_6dbuoys` | [docs/registry/implementation-map.yaml:L7920-L7929] |
| W293 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` — `PipelineSchematicGenerator._parse_pipeline_path` | [docs/registry/implementation-map.yaml:L7930-L7939] |
| W294 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` — `PipelineSchematicGenerator._parse_boundary_conditions` | [docs/registry/implementation-map.yaml:L7940-L7949] |
| W295 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` — `PipelineSchematicGenerator.create_plan_view` | [docs/registry/implementation-map.yaml:L7950-L7959] |
| W296 | `src/digitalmodel/solvers/orcaflex/pipeline_schematic.py` — `PipelineSchematicGenerator.create_elevation_view` | [docs/registry/implementation-map.yaml:L7960-L7969] |
| W297 | `src/digitalmodel/solvers/orcaflex/preprocess/load_vessel.py` — `LoadVessel.aqwa_group_item` | [docs/registry/implementation-map.yaml:L7970-L7979] |
| W298 | `src/digitalmodel/solvers/orcaflex/reporting/extractors/aggregator.py` — `_extract_dynamic_results_all` | [docs/registry/implementation-map.yaml:L7980-L7989] |
| W299 | `src/digitalmodel/solvers/orcaflex/reporting/renderers/riser.py` — `RiserRenderer._build_riser_dynamic_results` | [docs/registry/implementation-map.yaml:L7990-L7999] |
| W300 | `src/digitalmodel/solvers/orcaflex/run_to_sim.py` — `find_model_files` | [docs/registry/implementation-map.yaml:L8000-L8009] |
| W301 | `src/digitalmodel/solvers/orcaflex/spec_author.py` — `AuthoredSpec.to_spec_yaml` | [docs/registry/implementation-map.yaml:L8010-L8019] |
| W302 | `src/digitalmodel/solvers/orcaflex/template_generator.py` — `TemplateGenerator.generate` | [docs/registry/implementation-map.yaml:L8020-L8031] |
| W303 | `src/digitalmodel/solvers/orcaflex/template_generator.py` — `TemplateGenerator._generate_reference` | [docs/registry/implementation-map.yaml:L8032-L8041] |
| W304 | `src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py` — `InstallationVtoHost.installation_step` | [docs/registry/implementation-map.yaml:L8042-L8051] |
| W305 | `src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py` — `InstallationVtoHost.get_target_line_length` | [docs/registry/implementation-map.yaml:L8052-L8061] |
| W306 | `src/digitalmodel/solvers/orcaflex/umbilical_installation_IV_to_host.py` — `InstallationVtoHost.get_target_reference_distance` | [docs/registry/implementation-map.yaml:L8062-L8071] |
| W307 | `src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py` — `Lower2ndEnd.installation_step` | [docs/registry/implementation-map.yaml:L8072-L8081] |
| W308 | `src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py` — `Lower2ndEnd.get_target_line_length` | [docs/registry/implementation-map.yaml:L8082-L8091] |
| W309 | `src/digitalmodel/solvers/orcaflex/umbilical_installation_lower_2nd_end.py` — `Lower2ndEnd.get_target_reference_distance` | [docs/registry/implementation-map.yaml:L8092-L8101] |
| W310 | `src/digitalmodel/solvers/orcaflex/universal/universal_runner.py` — `UniversalOrcaFlexRunner._process_single_model` | [docs/registry/implementation-map.yaml:L8102-L8111] |
| W311 | `src/digitalmodel/solvers/orcaflex/universal_cli.py` — `create_config` | [docs/registry/implementation-map.yaml:L8112-L8121] |
| W312 | `src/digitalmodel/solvers/orcaflex/yaml_timestep_validation.py` — `_mapping_issues` | [docs/registry/implementation-map.yaml:L8122-L8131] |
| W313 | `src/digitalmodel/solvers/orcaflex/yaml_timestep_validation.py` — `inspect_source` | [docs/registry/implementation-map.yaml:L8132-L8143] |
| W314 | `src/digitalmodel/solvers/orcaflex/yaml_utils.py` — `orcaflex_dump` | [docs/registry/implementation-map.yaml:L8144-L8153] |
| W315 | `src/digitalmodel/solvers/orcaflex/yaml_utils.py` — `orcaflex_load` | [docs/registry/implementation-map.yaml:L8154-L8163] |
| W316 | `src/digitalmodel/solvers/orcaflex/yaml_validator.py` — `OrcaFlexYAMLValidator._detect_format` | [docs/registry/implementation-map.yaml:L8164-L8173] |
| W317 | `src/digitalmodel/solvers/orcaflex/yaml_validator.py` — `OrcaFlexYAMLValidator._check_singular_section_keys` | [docs/registry/implementation-map.yaml:L8174-L8183] |
| W318 | `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` — `OrcaWaveOrchestrator._create_vessel_config` | [docs/registry/implementation-map.yaml:L8184-L8193] |
| W319 | `src/digitalmodel/solvers/orcawave/diffraction/orchestrator.py` — `OrcaWaveOrchestrator.list_available_vessels` | [docs/registry/implementation-map.yaml:L8194-L8203] |
| W320 | `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` — `OrcaFlexConverter.save_orcaflex_file` | [docs/registry/implementation-map.yaml:L8204-L8215] |
| W321 | `src/digitalmodel/solvers/orcawave/diffraction/scripts/convert_to_orcaflex.py` — `OrcaFlexConverter._save_compact_format` | [docs/registry/implementation-map.yaml:L8216-L8225] |
| W322 | `src/digitalmodel/solvers/smoke/model_manifest.py` — `_tree` | [docs/registry/implementation-map.yaml:L8226-L8235] |
| W323 | `src/digitalmodel/solvers/smoke/model_probe.py` — `check_settings` | [docs/registry/implementation-map.yaml:L8236-L8245] |
| W324 | `src/digitalmodel/solvers/smoke/probes.py` — `_saved_proof` | [docs/registry/implementation-map.yaml:L8246-L8255] |
| W325 | `src/digitalmodel/subsea/catenary_riser/legacy/getFatigueLoading.py` — `prepareFatiguePostProcessing` | [docs/registry/implementation-map.yaml:L8256-L8267] |
| W326 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` — `orcaflexModel` | [docs/registry/implementation-map.yaml:L8268-L8278] |
| W327 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` — `lineTypes` | [docs/registry/implementation-map.yaml:L8279-L8288] |
| W328 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` — `WriteOrcaflexModel` | [docs/registry/implementation-map.yaml:L8289-L8298] |
| W329 | `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` — `build_model` | [docs/registry/implementation-map.yaml:L8299-L8311] |
| W330 | `src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py` — `OrcaFlexModelGenerator.generate_model_yml` | [docs/registry/implementation-map.yaml:L8312-L8321] |
| W331 | `src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py` — `OrcaFlexModelGenerator.generate_model_dict` | [docs/registry/implementation-map.yaml:L8322-L8331] |
| W332 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` — `VerticalRiser.get_fea_properties` | [docs/registry/implementation-map.yaml:L8332-L8341] |
| W333 | `src/digitalmodel/viv_screening/workflow.py` — `save_echo` | [docs/registry/implementation-map.yaml:L8342-L8351] |
| W334 | `src/digitalmodel/web/digitaltwinfeed/BuoySALM/BuoySALM.py` — `OrcaFlex.get` | [docs/registry/implementation-map.yaml:L8352-L8361] |
| W335 | `src/digitalmodel/web/digitaltwinfeed/FFS/FFS.py` — `OrcaFlex.get` | [docs/registry/implementation-map.yaml:L8362-L8371] |
| W336 | `src/digitalmodel/web/digitaltwinfeed/OffshoreRiser/OffshoreRiser.py` — `OrcaFlex.get` | [docs/registry/implementation-map.yaml:L8372-L8381] |
| W337 | `src/digitalmodel/workflows/automation/go_by_folder/metadata/analysis.py` — `AnalysisMetadataGenerator._detect_workflow_type` | [docs/registry/implementation-map.yaml:L8382-L8391] |
| W338 | `src/digitalmodel/workflows/installation_added_mass_bracket.py` — `integration_basis` | [docs/registry/implementation-map.yaml:L8392-L8401] |
| W339 | `src/digitalmodel/workflows/installation_added_mass_bracket.py` — `named_body` | [docs/registry/implementation-map.yaml:L8402-L8411] |
| W340 | `src/digitalmodel/workflows/installation_added_mass_bracket.py` — `expected_change` | [docs/registry/implementation-map.yaml:L8412-L8423] |
| W341 | `src/digitalmodel/workflows/installation_added_mass_evidence.py` — `read_cases` | [docs/registry/implementation-map.yaml:L8424-L8433] |
| W342 | `src/digitalmodel/workflows/installation_added_mass_materialize.py` — `_write_request` | [docs/registry/implementation-map.yaml:L8434-L8445] |
| W343 | `src/digitalmodel/workflows/installation_added_mass_materialize.py` — `materialize_bracket_case` | [docs/registry/implementation-map.yaml:L8446-L8458] |
| W344 | `src/digitalmodel/workflows/installation_hydrodynamic_sensitivity.py` — `_source` | [docs/registry/implementation-map.yaml:L8459-L8468] |
| W345 | `src/digitalmodel/workflows/installation_hydrodynamic_sensitivity.py` — `_case_files` | [docs/registry/implementation-map.yaml:L8469-L8480] |
| W346 | `src/digitalmodel/workflows/installation_numerical_compare.py` — `_apply_change` | [docs/registry/implementation-map.yaml:L8481-L8493] |
| W347 | `src/digitalmodel/workflows/installation_numerical_compare.py` — `_canonical_tables` | [docs/registry/implementation-map.yaml:L8494-L8505] |
| W348 | `src/digitalmodel/workflows/installation_numerical_compare.py` — `_runtime` | [docs/registry/implementation-map.yaml:L8506-L8515] |
| W349 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_field` | [docs/registry/implementation-map.yaml:L8516-L8525] |
| W350 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_line` | [docs/registry/implementation-map.yaml:L8526-L8535] |
| W351 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_value` | [docs/registry/implementation-map.yaml:L8536-L8548] |
| W352 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_anchors` | [docs/registry/implementation-map.yaml:L8549-L8560] |
| W353 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_timing` | [docs/registry/implementation-map.yaml:L8561-L8570] |
| W354 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_ordered` | [docs/registry/implementation-map.yaml:L8571-L8584] |
| W355 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_apply_native` | [docs/registry/implementation-map.yaml:L8585-L8596] |
| W356 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_counts` | [docs/registry/implementation-map.yaml:L8597-L8607] |
| W357 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_variation` | [docs/registry/implementation-map.yaml:L8608-L8620] |
| W358 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_native_case` | [docs/registry/implementation-map.yaml:L8621-L8635] |
| W359 | `src/digitalmodel/workflows/installation_numerical_prepare.py` — `_request` | [docs/registry/implementation-map.yaml:L8636-L8645] |
| W360 | `src/digitalmodel/workflows/installation_seastates.py` — `_snapshot` | [docs/registry/implementation-map.yaml:L8646-L8655] |
| W361 | `src/digitalmodel/workflows/installation_seastates.py` — `_structure` | [docs/registry/implementation-map.yaml:L8656-L8665] |
| W362 | `src/digitalmodel/workflows/installation_seastates.py` — `_request` | [docs/registry/implementation-map.yaml:L8666-L8675] |
| W363 | `src/digitalmodel/workflows/installation_seastates.py` — `generate_case` | [docs/registry/implementation-map.yaml:L8676-L8689] |
| W364 | `src/digitalmodel/workflows/installation_seastates.py` — `prepare_matrix` | [docs/registry/implementation-map.yaml:L8690-L8702] |
| W365 | `src/digitalmodel/workflows/installation_seastates.py` — `_change_payload` | [docs/registry/implementation-map.yaml:L8703-L8712] |
| W366 | `src/digitalmodel/workflows/installation_seastates.py` — `materialize_case` | [docs/registry/implementation-map.yaml:L8713-L8727] |
| W367 | `src/digitalmodel/workflows/installation_variant_study.py` — `_verify_master` | [docs/registry/implementation-map.yaml:L8728-L8737] |
| W368 | `src/digitalmodel/workflows/installation_variant_study.py` — `_verify_settings` | [docs/registry/implementation-map.yaml:L8738-L8747] |
| W369 | `src/digitalmodel/workflows/installation_variant_study.py` — `prepare_variant_study` | [docs/registry/implementation-map.yaml:L8748-L8761] |
| W370 | `src/digitalmodel/workflows/mcp_server/orcawave/core/mcp_server.py` — `OrcaWaveMCPServer._register_tools.export_to_orcaflex` | [docs/registry/implementation-map.yaml:L8762-L8771] |
| W371 | `src/digitalmodel/workflows/orcaflex_reproduce.py` — `_prepare` | [docs/registry/implementation-map.yaml:L8772-L8781] |
| W372 | `src/digitalmodel/workflows/orcaflex_reproduce_results.py` — `extract_results` | [docs/registry/implementation-map.yaml:L8782-L8791] |
| W373 | `src/digitalmodel/workflows/parametric_run.py` — `_write_yaml` | [docs/registry/implementation-map.yaml:L8792-L8801] |
| W374 | `src/digitalmodel/workflows/skills/skill_resolver.py` — `SkillResolver._parse_skill_file` | [docs/registry/implementation-map.yaml:L8802-L8811] |
| W375 | `src/digitalmodel/workflows/structure_hydrodynamic_candidate.py` — `_model` | [docs/registry/implementation-map.yaml:L8812-L8821] |
| W376 | `src/digitalmodel/workflows/structure_hydrodynamic_candidate.py` — `_apply` | [docs/registry/implementation-map.yaml:L8822-L8831] |
| W377 | `src/digitalmodel/workflows/structure_hydrodynamic_candidate.py` — `build_candidate` | [docs/registry/implementation-map.yaml:L8832-L8844] |
| W378 | `src/digitalmodel/workflows/structure_installation_prepare.py` — `_dependencies` | [docs/registry/implementation-map.yaml:L8845-L8854] |
| W379 | `tests/drilling_riser/global_model/test_batch_params.py` — `base_spec` | [docs/registry/implementation-map.yaml:L8855-L8864] |
| W380 | `tests/drilling_riser/global_model/test_campaign.py` — `base_spec` | [docs/registry/implementation-map.yaml:L8865-L8874] |
| W381 | `tests/drilling_riser/global_model/test_event_models.py` — `_case` | [docs/registry/implementation-map.yaml:L8875-L8884] |
| W382 | `tests/drilling_riser/global_model/test_hang_off.py` — `_generic` | [docs/registry/implementation-map.yaml:L8885-L8896] |
| W383 | `tests/drilling_riser/global_model/test_hang_off.py` — `_case` | [docs/registry/implementation-map.yaml:L8897-L8906] |
| W384 | `tests/drilling_riser/global_model/test_open_water.py` — `_ow_case` | [docs/registry/implementation-map.yaml:L8907-L8916] |
| W385 | `tests/drilling_riser/global_model/test_statics_path.py` — `base_spec` | [docs/registry/implementation-map.yaml:L8917-L8926] |
| W386 | `tests/drilling_riser/global_model/test_tensioner_representation.py` — `_line` | [docs/registry/implementation-map.yaml:L8927-L8936] |
| W387 | `tests/drilling_riser/global_model/test_w5_channels.py` — `base_spec` | [docs/registry/implementation-map.yaml:L8937-L8946] |
| W388 | `tests/drilling_riser/postprocess/test_w510_significant_range.py` — `_doc` | [docs/registry/implementation-map.yaml:L8947-L8958] |
| W389 | `tests/legal/test_sanitize_s7_audit_private.py` — `setup` | [docs/registry/implementation-map.yaml:L8959-L8968] |
| W390 | `tests/maintenance/test_archive_safety.py` — `test_deep_reference_and_registry_paths_are_excluded` | [docs/registry/implementation-map.yaml:L8969-L8978] |
| W391 | `tests/maintenance/test_archive_safety.py` — `test_generator_partitions_move_keep_hold_and_excluded` | [docs/registry/implementation-map.yaml:L8979-L8988] |
| W392 | `tests/orcaflex/test_generic_scr_slwr.py` — `test_specs_match_basis_and_static_geometry` | [docs/registry/implementation-map.yaml:L8989-L8998] |
| W393 | `tests/orcaflex/test_generic_scr_slwr.py` — `test_analytical_reference_reproduces_from_basis` | [docs/registry/implementation-map.yaml:L8999-L9008] |
| W394 | `tests/solvers/orcaflex/format_converter/test_spec_conversions.py` — `TestModularToSpecBoundaryReporting.test_expected_gaps_classified` | [docs/registry/implementation-map.yaml:L9009-L9018] |
| W395 | `tests/solvers/orcaflex/format_converter/test_spec_conversions.py` — `TestModularToSpecBoundaryReporting.test_actionable_gaps_classified` | [docs/registry/implementation-map.yaml:L9019-L9028] |
| W396 | `tests/solvers/orcaflex/format_converter/test_spec_conversions.py` — `TestModularToSpecBoundaryReporting.test_all_unmapped_are_categorized` | [docs/registry/implementation-map.yaml:L9029-L9038] |
| W397 | `tests/solvers/orcaflex/modular_generator/builders/test_general_builder.py` — `_build` | [docs/registry/implementation-map.yaml:L9039-L9048] |
| W398 | `tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py` — `_build` | [docs/registry/implementation-map.yaml:L9049-L9058] |
| W399 | `tests/solvers/orcaflex/modular_generator/builders/test_linetype_builder.py` — `_build` | [docs/registry/implementation-map.yaml:L9059-L9068] |
| W400 | `tests/solvers/orcaflex/modular_generator/goldens/basefile/capture.py` — `main` | [docs/registry/implementation-map.yaml:L9069-L9078] |
| W401 | `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` — `_write_floating_campaign` | [docs/registry/implementation-map.yaml:L9079-L9088] |
| W402 | `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` — `_write_slay_campaign` | [docs/registry/implementation-map.yaml:L9089-L9098] |
| W403 | `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py` — `_write_campaign_yaml` | [docs/registry/implementation-map.yaml:L9099-L9108] |
| W404 | `tests/solvers/orcaflex/modular_generator/test_collection_collision.py` — `_write_model` | [docs/registry/implementation-map.yaml:L9109-L9118] |
| W405 | Identifying discovery file; refer to this map row; issue pending (4) | [docs/registry/implementation-map.yaml:L9119-L9128] |
| W406 | Identifying discovery file; refer to this map row; issue pending (4) | [docs/registry/implementation-map.yaml:L9129-L9140] |
| W407 | `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py` — `TestJumperLineTypes.test_three_jumper_variants_emitted` | [docs/registry/implementation-map.yaml:L9141-L9150] |
| W408 | `tests/solvers/orcaflex/modular_generator/test_jumper_plet_to_plem_semantic.py` — `TestJumperLineReferences.test_jumper_line_uses_all_three_variants` | [docs/registry/implementation-map.yaml:L9151-L9160] |
| W409 | `tests/solvers/orcaflex/modular_generator/test_riser_variant_semantic_proof.py` — `_assert_line_type_references_resolve` | [docs/registry/implementation-map.yaml:L9161-L9170] |
| W410 | `tests/solvers/orcaflex/modular_generator/test_semantic_roundtrip.py` — `_write_mono_yaml` | [docs/registry/implementation-map.yaml:L9171-L9180] |
| W411 | Identifying discovery file; refer to this map row; issue pending (4) | [docs/registry/implementation-map.yaml:L9181-L9190] |
| W412 | `tests/solvers/orcaflex/reporting/fixture_helpers.py` — `_build_boundary_conditions` | [docs/registry/implementation-map.yaml:L9191-L9200] |
| W413 | `tests/solvers/orcaflex/single_file_parallel_test.py` — `SingleFileParallelStrategy._get_line_results` | [docs/registry/implementation-map.yaml:L9201-L9210] |
| W414 | `tests/solvers/orcaflex/single_file_parallel_test.py` — `SingleFileParallelStrategy._get_vessel_results` | [docs/registry/implementation-map.yaml:L9211-L9220] |
| W415 | `tests/solvers/orcaflex/test_model_catalogue.py` — `test_structural_owners_and_path_references` | [docs/registry/implementation-map.yaml:L9221-L9230] |
| W416 | `tests/solvers/orcaflex/test_model_catalogue_references.py` — `test_same_named_unrelated_folders_are_not_references` | [docs/registry/implementation-map.yaml:L9231-L9240] |
| W417 | `tests/solvers/orcaflex/test_model_header_guard.py` — `header_repository_findings` | [docs/registry/implementation-map.yaml:L9241-L9250] |
| W418 | `tests/solvers/orcaflex/test_modular_input_validation_fixes.py` — `master_with_missing_include` | [docs/registry/implementation-map.yaml:L9251-L9260] |
| W419 | `tests/solvers/orcaflex/test_slay_layout.py` — `test_support_types_are_defined_before_vessel_references` | [docs/registry/implementation-map.yaml:L9261-L9270] |
| W420 | `tests/solvers/orcaflex/test_slay_load_form.py` — `_resolve_case.read` | [docs/registry/implementation-map.yaml:L9271-L9280] |
| W421 | `tests/solvers/orcaflex/test_slay_load_form.py` — `_pipe_seed_points` | [docs/registry/implementation-map.yaml:L9281-L9290] |
| W422 | `tests/solvers/orcaflex/test_template_generator.py` — `sample_base_template` | [docs/registry/implementation-map.yaml:L9291-L9300] |
| W423 | `tests/solvers/orcaflex/test_template_generator.py` — `sample_variation` | [docs/registry/implementation-map.yaml:L9301-L9310] |
| W424 | `tests/solvers/orcaflex/test_template_generator.py` — `sample_case_template` | [docs/registry/implementation-map.yaml:L9311-L9320] |
| W425 | `tests/solvers/orcaflex/test_template_generator.py` — `TestTemplateGenerator.test_generate_with_basefile_reference` | [docs/registry/implementation-map.yaml:L9321-L9330] |
| W426 | `tests/solvers/orcaflex/test_template_generator.py` — `TestTemplateGenerator.test_reference_paths_resolve_from_arbitrary_output_dir` | [docs/registry/implementation-map.yaml:L9331-L9340] |
| W427 | `tests/solvers/orcaflex/test_template_generator.py` — `TestTemplateGenerator.test_reference_paths_in_cases_dir_stay_relative` | [docs/registry/implementation-map.yaml:L9341-L9350] |
| W428 | `tests/solvers/smoke/test_model_manifest.py` — `manifest` | [docs/registry/implementation-map.yaml:L9351-L9360] |
| W429 | `tests/test_engine.py` — `temp_config_file` | [docs/registry/implementation-map.yaml:L9361-L9370] |
| W430 | `tests/test_main_exit_status.py` — `_solve_input` | [docs/registry/implementation-map.yaml:L9371-L9380] |
| W431 | `tests/test_skill_resolver.py` — `_write_skill` | [docs/registry/implementation-map.yaml:L9381-L9390] |
| W432 | `tests/workflows/test_durable_workflows.py` — `test_workflow_registry` | [docs/registry/implementation-map.yaml:L9391-L9400] |
| W433 | `tests/workflows/test_durable_workflows.py` — `test_synthetic_rope_tn_curve_matches_dnv_os_e301_table_f3` | [docs/registry/implementation-map.yaml:L9401-L9410] |
| W434 | `tests/workflows/test_durable_workflows.py` — `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | [docs/registry/implementation-map.yaml:L9411-L9420] |
| W435 | `tests/workflows/test_durable_workflows.py` — `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | [docs/registry/implementation-map.yaml:L9421-L9430] |
| W436 | `tests/workflows/test_durable_workflows.py` — `test_vessel_seakeeping_significant_amplitude_matches_library` | [docs/registry/implementation-map.yaml:L9431-L9440] |
| W437 | `tests/workflows/test_installation_instability_diagnostic.py` — `fake` | [docs/registry/implementation-map.yaml:L9441-L9450] |
| W438 | `tests/workflows/test_orcaflex_batch_contract.py` — `test_duration_render_preserves_native_default_spelling_and_source` | [docs/registry/implementation-map.yaml:L9451-L9460] |
| W439 | `tests/workflows/test_orcaflex_reproduce.py` — `resume_case` | [docs/registry/implementation-map.yaml:L9461-L9470] |
| W440 | `tests/workflows/test_structure_hydrodynamic_candidate.py` — `case` | [docs/registry/implementation-map.yaml:L9471-L9481] |

Discovery paths withheld: 5 files, 6 implementation rows; map row identities retain full inventory coverage; codenaming follow-up: issue pending (4). [docs/registry/implementation-map.yaml:L4921-L9496]

Counted-family grep uses the selected map revision’s `search_signals` exactly as Python `re` expressions, case-sensitive, on every package `.py` file. Signals matching the package path below `src/digitalmodel/`, case-insensitively, are omitted. The `vocabulary` expression `orcaflex|OrcaFlex|includefile|modular_generator` matches that path and is omitted; `algorithm` and `constants_or_keys` remain.

| Signal | Exact Python regular expression |
| --- | --- |
| algorithm | `(?:yaml\.(?:dump\|safe_dump)\|write_text\|open\(\|\.write\(\|save\(\|SaveData\|dump\().*\|to_orcaflex_dict\|ObjectType\|EndAConnection\|LineConfig\|VesselConfig\|WaveTrains\|CurrentRamp\|WindSpeed\|\.write\(\|\.ya?ml\|yaml\.safe_dump\|shutil\.copy\|\.write\(\|write_model\|ModularModelGenerator\|ProjectInputSpec\|yaml\.safe_dump\|\.write_text` |
| constants_or_keys | `[\"\'](?:Lines\|LineTypes\|Vessels\|6DBuoys\|General\|IncludeFile\|includefile)[\"\']\|%YAML` |

Expressions above use Markdown pipe escapes for rendering; execute the original YAML strings, without rewriting or adding flags. [docs/registry/implementation-map.yaml:L4921-L4936]

| Matched enclosing definition | Hit lines | Classification |
| --- | --- | --- |
| `OrcaFlexModelGenerator` | 51, 52 | Class-docstring examples only; no further computation. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L26-L461] |
| `OrcaFlexModelGenerator.generate_from_template` | 224, 225, 230, 237, 242, 261, 262 | Listed writer (W216). [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L206-L266] |
| `generate_model` | 473 | Delegating wrapper listed in section 3; docstring hit only. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L465-L476] |

A8 result: PASS; inventory 440/440 map rows, 1 counted family, 3 matched enclosing definitions, 0 further second implementations. `_assemble_model` is W217 and is also described in section 3, regardless of whether the literal map patterns hit its body. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L268-L335]
Whole-repository alternative search included `scripts/`: `rg -n 'OrcaFlexModelGenerator|model_generator' --glob '*.py' --glob '*.md' --glob '!agent-pack.md' --glob '!uv.lock'`; historical smoke scripts delegate to a generator import rather than defining this package's assembly algorithm. [scripts/testing/check_model_generator_basic.py:L6-L19] [scripts/testing/check_complete_workflow.py:L6-L26]
A separate class of the same name builds mooring output outside this package; this is an alternative implementation, not an in-package A8 miss. [src/digitalmodel/subsea/mooring_analysis/orcaflex_generator.py:L22-L63]
Map follow-up: component CSV identity/cache/mutation and advisory structural-validation methods have no separate in-package family rows; add those families and rows before counting them under A8. The text-model-writer family contains the package's assembly and generation methods; no map edit is made here; issue pending (4). [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L86-L204] [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L337-L459]

## 10. Coverage and open items

Read coverage: every package Python file and subfolder at the code pin, the module README, the module test file, the routing entry and the map family. [src/digitalmodel/solvers/orcaflex/model_generator/__init__.py:L1-L483] [src/digitalmodel/solvers/orcaflex/model_generator/README.md:L1-L571] [tests/solvers/orcaflex/test_model_generator.py:L1-L148] [docs/registry/module-routing.yaml:L374-L410]

Documentation discovery used `rg -l 'model_generator|OrcaFlexModelGenerator' docs --glob '*.md' --glob '*.rst' --glob '*.py'`; the list below separates readable source documentation from identifying discovery files without reproducing their names.

| Documentation read / discovered | Evidence | Coverage limit and tracking |
| --- | --- | --- |
| `docs/domains/orcaflex/QUICKSTART_MODEL_GENERATOR.md` | Historical imports and workflow. [docs/domains/orcaflex/QUICKSTART_MODEL_GENERATOR.md:L223-L259] | Import/return-contract refresh; issue pending (4) |
| `docs/domains/project-docs/MODEL_GENERATOR_IMPLEMENTATION_SUMMARY.md` | Claimed implementation and standards. [docs/domains/project-docs/MODEL_GENERATOR_IMPLEMENTATION_SUMMARY.md:L40-L66] [docs/domains/project-docs/MODEL_GENERATOR_IMPLEMENTATION_SUMMARY.md:L307-L325] | Historical counts and validation claims need qualification; issue pending (1) |
| `docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE2.md` and `MODULE_RECOMMENDATIONS_PHASE3.md` | Distinct generator discussion. [docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE2.md:L230-L240] [docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE3.md:L213-L224] | Discovery mentions, not this package's implementation reference; issue pending (4) |
| `docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md` | Module inventory mention. [docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md:L163-L178] | Broad inventory claims not adopted; issue pending (4) |
| `docs/domains/orcaflex/LEGACY_SCRIPT_INVENTORY.md` | Example-script discovery. [docs/domains/orcaflex/LEGACY_SCRIPT_INVENTORY.md:L50-L59] | Historical script status, not a test result; issue pending (4) |
| `docs/domains/orcaflex/examples/model_generator_examples.py` | Generator example imports. [docs/domains/orcaflex/examples/model_generator_examples.py:L1-L22] | Read as text; documentation execution not attested; issue pending (4) |
| `docs/domains/orcaflex/templates/risers/scr_catenary/README.md` | Template generator workflow. [docs/domains/orcaflex/templates/risers/scr_catenary/README.md:L41-L70] | Standards/units claims not independently qualified; issue pending (1) |
| `docs/domains/mooring/MOORING_ANALYSIS_IMPLEMENTATION.md` | Separate mooring-generator tests. [docs/domains/mooring/MOORING_ANALYSIS_IMPLEMENTATION.md:L274-L285] | Different package, excluded from this package's test count; issue pending (4) |
| `docs/plans/evidence/2026-08-10-issue-1983-triage.md` | Historical smoke-script classification. [docs/plans/evidence/2026-08-10-issue-1983-triage.md:L113-L121] | Scripts are not section-4 pytest nodes; issue pending (4) |
| 2 discovery files with identifying names | Discovery only; paths withheld for public naming compliance | Codenaming and provenance review; issue pending (4) |

| Open item | Responsible function | Issue |
| --- | --- | --- |
| Five standards lack editions, clauses/tables and demonstrated calculations; README performance/pre-validation claims have not been established; no independently validated worked example | Family quality manager | issue pending (1) |
| UNVERIFIED scope/version compatibility, independent component provenance and external dependency inventory | Family quality manager | issue pending (1) |
| CSV identity, empty-result, numeric-text, duplicate-selection and cache/mutation hazards S01–S08, S17–S18 | Package maintainer | issue pending (2) |
| UNVERIFIED complete units/frame/sign/phase contract; configuration, shallow-copy and serializer hazards S09–S13, S16; package-specific immutable fingerprints and revisions | Package maintainer | issue pending (3); related #2515 |
| Standards/validation hazards S14–S15 and outstanding native G1, statics G2, dynamics G3, conditional diffraction G4 and appendix G5 evidence | Family quality manager | issue pending (1); related #2132 |
| Historical imports/return assumptions; identifying discovery filenames; method-map follow-up; UNVERIFIED named appointments/shared-log entry | Documentation maintainer | issue pending (4) |

No private values are reproduced; no private sidecar entry was supplied or used. The public regression examples remain software expectations, not qualified design data. [tests/solvers/orcaflex/test_model_generator.py:L29-L148]
