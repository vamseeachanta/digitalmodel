# Power protocol models knowledge pack

| Header | Value |
| --- | --- |
| Module | power/protocols |
| Repository | digitalmodel |
| Path | src/digitalmodel/power/protocols |
| Pack revision | 1 |
| Code revision | `f4032a2ecaac110244919db23c5656d1c0d1cd5a` |
| Analysis family | Engineering calculations (EC); AQG r0 Python calculations |
| Quality manager role | Engineering calculations quality manager |
| Deputy role | Engineering calculations deputy quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The package exposes an IEC 61850 GOOSE model containing enums, dataclasses, dataset and binding validators, and an SCL-like dictionary generator. [src/digitalmodel/power/protocols/__init__.py:L1-L5] [src/digitalmodel/power/protocols/iec61850_goose.py:L1-L11]

The implementation performs in-memory construction and dictionary generation; `generate_scl_structure` returns a nested dictionary rather than transmitting messages or writing an XML document. [src/digitalmodel/power/protocols/iec61850_goose.py:L282-L330]

The parent `power` domain is registered as beta; no separate protocols maturity is recorded in that domain entry. [docs/registry/module-routing.yaml:L319-L334]

Scope includes both package Python files and the module test file; adjacent power subpackages are outside this pack. [src/digitalmodel/power/protocols/__init__.py:L1-L5] [src/digitalmodel/power/protocols/iec61850_goose.py:L1-L11] [tests/power/protocols/test_iec61850_goose.py:L1-L29]

## 2. Inputs and outputs

### Inputs

| Name | Unit | Allowed range or constraint implemented | Default | Source and citation |
| --- | --- | --- | --- | --- |
| `GOOSEDataAttribute.name`, `da_type`, `value` | Name and enum: dimensionless; payload unit unspecified | Annotations only; no runtime type or payload-unit validation | Required | Caller; [src/digitalmodel/power/protocols/iec61850_goose.py:L77-L93] |
| `GOOSEDataset.dataset_ref`, `attributes` | Reference and collection: dimensionless | Construction rejects an empty collection; `validate_dataset` flags duplicate names | Empty list factory, rejected on construction | Caller; [src/digitalmodel/power/protocols/iec61850_goose.py:L96-L115] [src/digitalmodel/power/protocols/iec61850_goose.py:L232-L241] |
| `GOOSEControlBlock.cb_ref`, `dataset_ref` | Dimensionless strings | No reference-syntax check | Required | Caller; [src/digitalmodel/power/protocols/iec61850_goose.py:L138-L150] |
| `GOOSEControlBlock.app_id` | Dimensionless identifier | For integer input, inclusive range 0x0000–0x3FFF (0–16383); out-of-range integers raise `ValueError`; no integer-type enforcement | Required | Caller; [src/digitalmodel/power/protocols/iec61850_goose.py:L145-L150] |
| `GOOSEControlBlock.conf_rev` | Dimensionless revision | No revision or integer-type check | Required | Caller; [src/digitalmodel/power/protocols/iec61850_goose.py:L138-L150] |
| `max_time_ms`, `min_time_ms` | ms, by docstring convention | No sign, integer-type or ordering check | Required | Caller; [src/digitalmodel/power/protocols/iec61850_goose.py:L132-L150] |
| `GOOSEPublisher` identity and object references | Dimensionless strings and model objects | No cross-check between dataset reference and control-block reference | Required | Caller; [src/digitalmodel/power/protocols/iec61850_goose.py:L153-L175] |
| `GOOSESubscriber` identity, subscribed references, `inputs` | Dimensionless identifiers and name list | Binding validator compares application and dataset references; it does not check the mapped input list | Empty list factory for `inputs`; others required | Caller; [src/digitalmodel/power/protocols/iec61850_goose.py:L178-L197] [src/digitalmodel/power/protocols/iec61850_goose.py:L261-L279] |
| `LogicalNode.ln_class`, `ln_inst`, `description` | Dimensionless enum and strings | Annotations only | Required | Caller; [src/digitalmodel/power/protocols/iec61850_goose.py:L200-L216] |

### Outputs

| Name | Unit | Range, default and format | Consumer and citation |
| --- | --- | --- | --- |
| `validate_dataset` | Boolean and diagnostic count: dimensionless | Dictionary with `pass` and `issues`; one diagnostic per repeated name after its first appearance | Caller; [src/digitalmodel/power/protocols/iec61850_goose.py:L232-L241] |
| `validate_binding` | Boolean and diagnostic count: dimensionless | Dictionary with `pass` and `issues`; reference-mismatch diagnostics | Caller; [src/digitalmodel/power/protocols/iec61850_goose.py:L261-L279] |
| `generate_scl_structure` | Identifiers: dimensionless; time fields retain caller convention in ms | Nested `IED/AccessPoint/Server/LDevice/LN0` dictionary; `GOOSEControl` and `DataSet/FCDA`; hex string for application ID; attribute names and enum values, without payload values | Caller; [src/digitalmodel/power/protocols/iec61850_goose.py:L296-L330] |

No coordinate frame, origin, geometric sign or phase convention participates in these functions; they compare identifiers and assemble dictionaries. [src/digitalmodel/power/protocols/iec61850_goose.py:L219-L330]

UNVERIFIED: U1 payload engineering units are not established; issue pending (1). [src/digitalmodel/power/protocols/iec61850_goose.py:L77-L93]

UNVERIFIED: U3 interoperability with an external SCL schema or network peer is not established; dictionary key names, nesting and the meaning of `appID` are not checked against the SCL part of the standard; issue pending (3). [src/digitalmodel/power/protocols/iec61850_goose.py:L282-L330]

## 3. Method and governing standards

| Method | Implementation and coverage | Reference and provenance limit |
| --- | --- | --- |
| Model vocabulary | `LogicalNodeType` and `DAType` enumerate the supported symbols; dataclasses retain caller data. [src/digitalmodel/power/protocols/iec61850_goose.py:L20-L216] | IEC 61850 parts 7-2 and 8-1 are named; no edition or clause is supplied. [src/digitalmodel/power/protocols/iec61850_goose.py:L1-L11] |
| Dataset acceptance | Constructor rejects an initially empty attribute list; validator accumulates repeated names using a set. [src/digitalmodel/power/protocols/iec61850_goose.py:L108-L115] [src/digitalmodel/power/protocols/iec61850_goose.py:L232-L241] | IEC 61850-7-2, Abstract communication service interface (ACSI), is named in the module reference list without edition or numbered clause. [src/digitalmodel/power/protocols/iec61850_goose.py:L7-L11] |
| Control-block acceptance | For integer input, constructor checks application-ID bounds and raises `ValueError` outside them; it does not enforce integer type. [src/digitalmodel/power/protocols/iec61850_goose.py:L145-L150] | IEC 61850-8-1, Specific communication service mapping (SCSM), is named beside the application-ID description; no edition or numbered clause is provided. [src/digitalmodel/power/protocols/iec61850_goose.py:L126-L135] |
| Binding acceptance | Two equality comparisons: subscribed application ID and dataset reference against the control block. [src/digitalmodel/power/protocols/iec61850_goose.py:L261-L279] | No function-level clause or equation citation is provided. [src/digitalmodel/power/protocols/iec61850_goose.py:L244-L279] |
| SCL-like representation | Builds dictionaries and maps attribute enums to their values; does not emit a `Citation` sidecar. [src/digitalmodel/power/protocols/iec61850_goose.py:L296-L330] | IEC 61850-8-1 refers to mappings to MMS and ISO/IEC 8802-3; the latter is a reference-list mention, without a separate implementation, edition or clause. [src/digitalmodel/power/protocols/iec61850_goose.py:L7-L11] |

The code names parts 7-2 and 8-1 rather than just the broad parts 7 and 8; the test docstring names 8-1 and supplies no edition either, so there is no recorded pair of conflicting editions to reconcile. [src/digitalmodel/power/protocols/iec61850_goose.py:L7-L11] [tests/power/protocols/test_iec61850_goose.py:L7-L10]

UNVERIFIED: U2 standard editions, clause identifiers, standards conformance and standards-derived constant provenance remain unestablished, including the docstring attribution of the application-ID range to IEC 61850-8-1; SCL dictionary key names, nesting and the meaning of `appID` are not checked against the SCL part of the standard; clause text not checked against the licensed copy; issue pending (2). [src/digitalmodel/power/protocols/iec61850_goose.py:L7-L11] [src/digitalmodel/power/protocols/iec61850_goose.py:L126-L150]

The application-ID validator returns no calculation-output citation sidecar; the only returned dictionaries belong to the validators and structure generator. [src/digitalmodel/power/protocols/iec61850_goose.py:L145-L150] [src/digitalmodel/power/protocols/iec61850_goose.py:L219-L330]

## 4. Validated worked examples

Validated examples: 0; the existing tests use local fixtures and implementation-level assertions, without a revision-pinned independent reference dataset or standards annex; retain these as regression evidence, issue pending (3). [tests/power/protocols/test_iec61850_goose.py:L35-L70] [tests/power/protocols/test_iec61850_goose.py:L238-L328]

The following five regression examples use the inline fixtures of the test file; tolerances are exact Boolean, string or count equality, with no numerical approximation. [tests/power/protocols/test_iec61850_goose.py:L238-L328]

| Case | Source and input | Expected result and tolerance | Existing test node ID | Offline result at code pin |
| --- | --- | --- | --- | --- |
| R1 duplicate names | Local two-attribute fixture | `pass=False`; diagnostic contains duplicate; exact Boolean and substring, dimensionless. [tests/power/protocols/test_iec61850_goose.py:L244-L254] | `tests/power/protocols/test_iec61850_goose.py::TestValidateDataset::test_duplicate_attribute_names_flagged` | PASS, baseline and after |
| R2 matching binding | Local publisher and matching subscriber | `pass=True`; exact Boolean, dimensionless. [tests/power/protocols/test_iec61850_goose.py:L261-L271] | `tests/power/protocols/test_iec61850_goose.py::TestValidateBinding::test_matching_binding_passes` | PASS, baseline and after |
| R3 application mismatch | Local publisher and differing subscribed identifier | `pass=False`; application-ID diagnostic; exact Boolean and substring, dimensionless. [tests/power/protocols/test_iec61850_goose.py:L273-L284] | `tests/power/protocols/test_iec61850_goose.py::TestValidateBinding::test_mismatched_app_id_fails` | PASS, baseline and after |
| R4 reference mismatch | Local publisher and differing subscribed reference | `pass=False`; dataset-reference diagnostic; exact Boolean and substring, dimensionless. [tests/power/protocols/test_iec61850_goose.py:L286-L295] | `tests/power/protocols/test_iec61850_goose.py::TestValidateBinding::test_mismatched_dataset_ref_fails` | PASS, baseline and after |
| R5 dataset representation | Local publisher dataset | Dataset name unchanged; exactly 1 FCDA entry, zero count difference. [tests/power/protocols/test_iec61850_goose.py:L323-L328] | `tests/power/protocols/test_iec61850_goose.py::TestGenerateSCLStructure::test_scl_contains_dataset` | PASS, baseline and after |

Execution record, 2026-10-18: baseline and after each ran 25 module tests, with 25 passed, 0 failed, 0 skipped and 0 NOT_RUN, against the code revision in the header; all five examples are members of that suite. [tests/power/protocols/test_iec61850_goose.py:L76-L328]

Generic invocation: `PYTHONPATH=src uv run --no-sources python -m pytest tests/power/protocols/test_iec61850_goose.py -q -rA -p no:cacheprovider --basetemp=<scratch>`, using the configured `pytest.ini` options without overrides. [pytest.ini:L1-L3] [pytest.ini:L48-L52]

## 5. QA gates

Gate selection: AQG r0, section 10, Python calculations, with G5 from section 9; the checks below establish the model's implemented behavior and evidence traceability, without claiming standards conformance.

| Gate | Applicability | Module-specific checks |
| --- | --- | --- |
| G0 inputs, units, citations | Applicable; model construction, reference consistency and provenance boundary | C01, C02, C03 |
| G1 imports and tests | Applicable; Python import and offline test execution, with no external program writer | C04 |
| G2 hand or published check | Applicable; set-cardinality and equality reasoning for the implemented predicates; no independent validation claimed | C05 |
| G3 short dynamics | NOT_APPLICABLE; no dynamic solver or time integration is implemented. [src/digitalmodel/power/protocols/iec61850_goose.py:L219-L330] | None |
| G4 diffraction | NOT_APPLICABLE; outputs are diagnostic and protocol-structure dictionaries. [src/digitalmodel/power/protocols/iec61850_goose.py:L219-L330] | None |
| G5 evidence appendix | Applicable; pin, test results, checks and known gaps accompany model claims | C06 |

| Check | Function, output and executable route | Numeric pass criterion and unit | Evidence and gate |
| --- | --- | --- | --- |
| C01 | Run `tests/power/protocols/test_iec61850_goose.py::TestGOOSEDataset::test_empty_attributes_raises` and the R1 node for initial-emptiness rejection and duplicate flagging. [tests/power/protocols/test_iec61850_goose.py:L127-L132] [tests/power/protocols/test_iec61850_goose.py:L244-L254] | 2 passed tests; empty construction raises `ValueError` and duplicate validation returns `pass=False` with a duplicate diagnostic, exact outcomes | Test results; G0 |
| C02 | Run R2, R3 and R4 for `validate_binding` reference predicates. [tests/power/protocols/test_iec61850_goose.py:L261-L295] | 3 passed tests; 0 incorrect Boolean results, count | Test results; G0 |
| C03 | Inspect `GOOSEControlBlock` timing units and constructor bounds; retain missing edition, clause and citation metadata in section 10 rather than asserting conformance. [src/digitalmodel/power/protocols/iec61850_goose.py:L126-L150] | 2 time fields documented as ms; 0 untracked provenance gaps, count | Input table and issue pending (2); G0; documentation check only |
| C04 | Run the complete module test file, including its import and structure checks. [tests/power/protocols/test_iec61850_goose.py:L17-L29] [tests/power/protocols/test_iec61850_goose.py:L76-L328] | 25 passed tests; 0 failures, skips or NOT_RUN, count | Baseline and after results; G1 |
| C05 | Reasoning: the two equal R1 names imply 1 repeat; the cited test asserts `pass=False` and a diagnostic containing duplicate, without asserting diagnostic count. The matching R2 references imply 0 unequal pairs; the cited test asserts `pass=True`. [tests/power/protocols/test_iec61850_goose.py:L244-L271] | 0 contradictions with the cited Boolean and substring assertions; diagnostic cardinality is not tested | Reasoning recorded in this row; G2; no independent reference check |
| C06 | Resolve all ranged citations at the declared code or explicit map pin; require the five R1–R5 nodes in both module run logs, and retain every section-10 item with a draft reference | 0 unresolved citations; 0 missing example results; 0 untracked open items, count | Citation resolver, offline logs and draft list; G5 |

Recorded results: C01–C06 PASS for their stated criteria; C03 records absent provenance and C05 records reasoning consistent with the cited assertions; neither establishes IEC conformance. [src/digitalmodel/power/protocols/iec61850_goose.py:L126-L150] [tests/power/protocols/test_iec61850_goose.py:L244-L271]

## 6. Known failure modes and pitfalls

sweep: 6 found

Sweep definition: six candidate source sites, including documented time-unit assumptions and empty-list defaults; zero exception-swallowing, record-skipping, external-text numeric conversion or cache-write sites were found in the package Python files. [src/digitalmodel/power/protocols/__init__.py:L1-L5] [src/digitalmodel/power/protocols/iec61850_goose.py:L1-L330]

Exact command used: `rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|default|\bms\b|unit|cache' src/digitalmodel/power/protocols -g '*.py'`; every match is assigned a row below, with manual inspection of both complete Python files. [src/digitalmodel/power/protocols/__init__.py:L1-L5] [src/digitalmodel/power/protocols/iec61850_goose.py:L1-L330]

No incident issue was specified, and read-only searches for GOOSE and IEC 61850 found no existing incident to cite; the newly identified gaps use numbered drafts below.

| Site | Symptom and cause | Detection that exists | Reference and date |
| --- | --- | --- | --- |
| S1 empty attribute default | The empty default is rejected initially, but the dataclass remains mutable. [src/digitalmodel/power/protocols/iec61850_goose.py:L108-L115] | C01 detects initial emptiness; no detection check exists for mutation after construction | issue pending (1); 2026-10-18 |
| S2 maximum time unit | `max_time_ms` assumes ms; type, sign and ordering are not checked. [src/digitalmodel/power/protocols/iec61850_goose.py:L132-L150] | C03 records the unit; no detection check exists for invalid maximum times | issue pending (1); 2026-10-18 |
| S3 minimum time unit | `min_time_ms` assumes ms; type, sign and ordering are not checked. [src/digitalmodel/power/protocols/iec61850_goose.py:L134-L150] | C03 records the unit; no detection check exists for invalid minimum times | issue pending (1); 2026-10-18 |
| S4 empty input default | Subscriber inputs default to an empty list; binding acceptance ignores the list and whether its names occur in the dataset. [src/digitalmodel/power/protocols/iec61850_goose.py:L194-L197] [src/digitalmodel/power/protocols/iec61850_goose.py:L261-L279] | no detection check exists | issue pending (1); 2026-10-18 |
| S5 dataset success predicate | `len(issues)==0` tests only repeated names; a dataset cleared after construction passes; types, payload values and reference syntax are unchecked. [src/digitalmodel/power/protocols/iec61850_goose.py:L232-L241] | C01/C05 cover duplicates only; no detection check exists for the other accepted states | issue pending (1); 2026-10-18 |
| S6 binding success predicate | `len(issues)==0` reflects only two control-block comparisons; actual publisher dataset consistency is unchecked. [src/digitalmodel/power/protocols/iec61850_goose.py:L261-L279] [src/digitalmodel/power/protocols/iec61850_goose.py:L296-L323] | C02/C05 cover the compared references only; no detection check exists for publisher inconsistency | issue pending (1); 2026-10-18 |
| Additional type boundary | Application-ID comparison does not enforce integer type: NaN, `True` and `1.5` are accepted at construction; NaN and `1.5` later raise `TypeError` in `generate_scl_structure` at `hex(cb.app_id)`, while `True` serialises as `0x1`. Out-of-range float `1e5` raises `TypeError` while formatting the constructor error, rather than `ValueError`. Data-attribute annotations do not validate payload type. [src/digitalmodel/power/protocols/iec61850_goose.py:L91-L93] [src/digitalmodel/power/protocols/iec61850_goose.py:L145-L150] [src/digitalmodel/power/protocols/iec61850_goose.py:L313-L319] | no detection check exists | issue pending (1); 2026-10-18 |
| Standards provenance | References do not supply editions or clauses; constructor does not produce citation metadata. [src/digitalmodel/power/protocols/iec61850_goose.py:L7-L11] [src/digitalmodel/power/protocols/iec61850_goose.py:L145-L150] | C03/C06 track the gap; no conformance detection check exists | issue pending (2); 2026-10-18 |
| Representation boundary | Local dictionary assertions do not establish XML/schema or peer interoperability; dictionary key names, nesting and the meaning of `appID` are not checked against the SCL part of the standard. [src/digitalmodel/power/protocols/iec61850_goose.py:L282-L330] [tests/power/protocols/test_iec61850_goose.py:L301-L328] | C04 exercises dictionaries; no interoperability detection check exists | issue pending (3); 2026-10-18 |

## 7. Lessons log

Append future entries with date, change reference, lesson, evidence, pack change and shared-log reference; preserve earlier entries.

| Date | Change reference | Lesson | Evidence | Pack change | Shared-log reference |
| --- | --- | --- | --- | --- | --- |
| 2026-10-18 | Initial pack | Treat SCL-like dictionary tests as regression evidence and disclose the conformance boundary | [src/digitalmodel/power/protocols/iec61850_goose.py:L282-L330] [tests/power/protocols/test_iec61850_goose.py:L301-L328] | Sections 3–6 distinguish representation checks from independent validation | No external entry asserted |

## 8. Ownership

Review assignment: engineering calculations quality manager, with the engineering calculations deputy quality manager as the deputy function; producing function: module documentation and calculation maintainers.

Escalation route for ambiguous standards, units or acceptance criteria: the family quality manager, then the maintainer for cross-family scope or authorization; no named appointment or clearance is asserted.

## 9. Dependencies and implementation inventory

The module imports only standard-library `dataclasses` and `enum`; the test file imports pytest and the protocol module. [src/digitalmodel/power/protocols/iec61850_goose.py:L14-L17] [tests/power/protocols/test_iec61850_goose.py:L13-L29]

No sibling engineering module, solver version or external dataset is consumed by the module's definitions; consumers supply dataclass inputs and receive in-memory diagnostics or dictionaries. [src/digitalmodel/power/protocols/iec61850_goose.py:L14-L330]

Implementation-map snapshot: `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; explicit `@revision` citations below refer to that snapshot rather than the code pin. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L1-L6]

Inventory computed from every `families[*].implementations[*].path` in that snapshot: 0 in-package implementation rows and 0 module map families; the complete section-9 inventory is empty. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L345-L26502]

The two package hits in `rejected_candidates` are `validate_dataset` and `validate_binding`, both marked `needs_review` for irregular-frequency treatment and explicitly excluded from implementation counts; neither computes that family's result. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L42693-L42706] [src/digitalmodel/power/protocols/iec61850_goose.py:L219-L279]

Counted-family ranking: empty; counted-family grep: NOT_APPLICABLE because no map family has an implementation row in this package; further second implementations counted: 0; A8 inventory and search condition pass on that basis. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L345-L26502]

Map follow-up: inventory dataset non-emptiness/uniqueness, application-ID acceptance, subscriber-reference binding and SCL-like structure generation before counting these method families; issue pending (4). [src/digitalmodel/power/protocols/iec61850_goose.py:L108-L150] [src/digitalmodel/power/protocols/iec61850_goose.py:L219-L330]

Additional whole-repository discovery, including `scripts/`: `rg -n -i 'goose|iec.?61850|power[/\.]protocols' scripts src tests -g '*.py' -g '*.md'`; protocol hits were the package and its tests; incidental substring matches were excluded by inspecting their definitions. [src/digitalmodel/power/protocols/iec61850_goose.py:L1-L11] [tests/power/protocols/test_iec61850_goose.py:L1-L29]

## 10. Coverage and open items

Coverage manifest: read `src/digitalmodel/power/protocols/__init__.py`, `src/digitalmodel/power/protocols/iec61850_goose.py`, the complete `tests/power/protocols/test_iec61850_goose.py`, the `power` routing entry, both implementation-map snapshots, `pytest.ini`, and the repository hook and test-domain declarations; no module README or additional module documentation was found by the package traversal and documentation search.

Documentation discovery: `rg -l -i 'iec.?61850|power/protocols|power\.protocols|ISO/IEC 8802' docs -g '*.md' -g '*.yaml'`; the only matching documentation file at the code pin was `docs/registry/implementation-map.yaml`, with excluded validator candidates. [docs/registry/implementation-map.yaml:L42639-L42652]

The module's tests comprise 25 nodes across enum, data-attribute, dataset, control-block, publisher, subscriber, logical-node, dataset-validator, binding-validator and structure-generator classes; the complete file was run, not just the five examples. [tests/power/protocols/test_iec61850_goose.py:L76-L328]

| Open item | Responsible function | Reference |
| --- | --- | --- |
| U1 payload units; constructor and validator type, timing, mutability, mapped-input and publisher-consistency boundaries | Protocol maintainers | issue pending (1); [src/digitalmodel/power/protocols/iec61850_goose.py:L77-L197] [src/digitalmodel/power/protocols/iec61850_goose.py:L219-L279] |
| U2 governing editions, clauses, conformance and standards-derived constant citation metadata, including the application-ID range attribution; clause text not checked against the licensed copy | Protocol maintainers and engineering calculations quality manager | issue pending (2); [src/digitalmodel/power/protocols/iec61850_goose.py:L7-L11] [src/digitalmodel/power/protocols/iec61850_goose.py:L126-L150] |
| U3 external schema/peer interoperability; SCL dictionary key names, nesting and `appID` meaning unchecked; independent worked example missing; current examples remain regressions | Protocol maintainers | issue pending (3); [tests/power/protocols/test_iec61850_goose.py:L301-L328] |
| Protocol method families absent from the accepted implementation inventory; review excluded candidates and add appropriate rows first | Implementation-map maintainers | issue pending (4); [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L42693-L42706] |

No private-sidecar content or client-derived values are included; all examples refer to the local test fixtures. [tests/power/protocols/test_iec61850_goose.py:L35-L70]
