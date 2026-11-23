# Data module knowledge pack

| Header | Value |
| --- | --- |
| Module / repository | data / digitalmodel |
| Package | `src/digitalmodel/data` |
| Pack revision / status | 0 / DRAFT |
| Code revision | `334521c97a21c16950046f52b61586e8e8465787` |
| Analysis family | EC; data pipeline, assessed with AQG r0 Python calculations gates |
| Quality role | Calculations quality manager; deputy: maintainer-designated independent calculations reviewer |
| Implementation-map ref | `origin/codex/implmap-work` at `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` |
| Map data revision | `270c8759c374b67a7f032953e7fcdb14e0224461` |

## 1. Purpose and scope

The module validates curated chain, wire and synthetic-line CSV schemas and their provenance, while retaining an explicitly unverified route for historical directories without a sidecar. [src/digitalmodel/data/mooring_component_schema.py:L1-L4] [src/digitalmodel/data/mooring_component_schema.py:L127-L148]

`ComponentDatabase` outside this package calls `load_tables` and exposes `find_provenance` through `get_provenance`; historical property-formula helpers are separate from the curated seed. [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L1-L6] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L305-L317]

The seed is for offline coefficient examples, with component qualification and valid size ranges left open; it is not a certified selection database. [src/digitalmodel/data/marine_engineering/mooring_components/README.html:L5-L6] [src/digitalmodel/data/marine_engineering/mooring_components/README.html:L28-L38]

UNVERIFIED: registry maturity is unassigned for this namespace package; there is no existing domain entry under which to attach its routing fields; follow-up is issue pending (4).

## 2. Inputs and outputs

| Input | Unit | Range / default | Source and evidence |
| --- | --- | --- | --- |
| `data_dir` | filesystem directory | caller-supplied; default resolved by `ComponentDatabase` | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L288-L304] |
| `chain`, `wire`, `line` tables | field-dependent | exact column sets; identities respectively diameter/grade/link_type, diameter/construction/grade, diameter/material | [src/digitalmodel/data/mooring_component_schema.py:L14-L26] |
| `diameter` | mm | finite and strictly positive; no numerical default | [src/digitalmodel/data/mooring_component_schema.py:L27-L28] [src/digitalmodel/data/mooring_component_schema.py:L50-L60] |
| `mbl`, `stiffness` | kN | finite and strictly positive; stiffness denotes EA | [src/digitalmodel/data/mooring_component_schema.py:L27-L28] [src/digitalmodel/data/marine_engineering/mooring_components/README.html:L14-L15] |
| `weight_air`, `weight_water` | kg/m | finite and strictly positive; apparent mass rather than force | [src/digitalmodel/data/mooring_component_schema.py:L27-L28] [src/digitalmodel/data/marine_engineering/mooring_components/README.html:L16-L17] |
| `bend_radius` | m | chain only, finite and strictly positive | [src/digitalmodel/data/mooring_component_schema.py:L14-L20] [src/digitalmodel/data/mooring_component_schema.py:L27-L28] [src/digitalmodel/data/mooring_component_schema.py:L50-L60] |
| Other CSV fields | dimensionless text | nonempty except notes; missing-value coercion caveat S04 | [src/digitalmodel/data/mooring_component_schema.py:L50-L61] |
| `provenance.json` | dimensionless metadata | optional for legacy loading; schema version 1, exact units, source records, table and excerpt hashes, per-row identities | [src/digitalmodel/data/mooring_component_schema.py:L92-L124] [src/digitalmodel/data/mooring_component_schema.py:L127-L148] |
| Source metadata | strings / UTC time | nonempty fields, lowercase hexadecimal commit and digest; zero UTC offset | [src/digitalmodel/data/mooring_component_schema.py:L29-L30] [src/digitalmodel/data/mooring_component_schema.py:L73-L89] |

| Output | Unit | Contract | Evidence |
| --- | --- | --- | --- |
| `load_tables(data_dir)` | tables retain declared field units | `(tables, provenance)`; dictionary keyed by chain/wire/line; may contain empty tables on legacy route | [src/digitalmodel/data/mooring_component_schema.py:L127-L148] |
| `validate_table(kind, table)` | same units | mutates converted columns in place; raises `ValueError` for schema, positivity, duplicate identity or monotonicity defects | [src/digitalmodel/data/mooring_component_schema.py:L42-L70] |
| `sha256(path)` | dimensionless hexadecimal string | hashes exact file bytes | [src/digitalmodel/data/mooring_component_schema.py:L33-L34] |
| `find_provenance(...)` | metadata | exactly one matching row; deep copies of row, source and diameter source; absent/ambiguous evidence raises `ValueError` | [src/digitalmodel/data/mooring_component_schema.py:L151-L163] |

These are scalar component properties, with no coordinate-frame, origin, sign or phase fields; positivity excludes floating components with negative apparent submerged mass. [src/digitalmodel/data/mooring_component_schema.py:L14-L28] [src/digitalmodel/data/mooring_component_schema.py:L50-L60] [src/digitalmodel/data/marine_engineering/mooring_components/README.html:L33-L33]

## 3. Method and governing standards

`validate_table` checks exact column membership, converts numeric fields, rejects nonfinite/nonpositive numbers, checks identity duplication and requires increasing breaking load and air mass within sorted diameter families. [src/digitalmodel/data/mooring_component_schema.py:L42-L70]

`validate_provenance` checks source-field presence, commit/digest syntax, UTC retrieval time, excerpt/CSV byte hashes, row counts, ordered identities and computed-row derivation presence; it does not recalculate the derivation (S17). [src/digitalmodel/data/mooring_component_schema.py:L73-L124]

The companion generator, outside the package, evaluates diameter-normalized polynomial breaking load, EA, air mass and buoyancy-corrected submerged mass from retained MoorPy coefficients; it writes CSVs and a manifest with per-row derivations and table hashes. [scripts/data/generate_mooring_components.py:L17-L42] [scripts/data/generate_mooring_components.py:L65-L89]

The retained source names the MoorPy coefficient/helper revision `1fb29f8eca2618543b2df7056adfdfce0265737b`, with file locators and digests, and distinguishes fixture sampling from qualified manufacturer ranges. [src/digitalmodel/data/marine_engineering/mooring_components/sources/moorpy_coefficients.json:L2-L16] [src/digitalmodel/data/marine_engineering/mooring_components/README.html:L23-L26]

| Standard lead | Located use and disposition |
| --- | --- |
| OCIMF | Broader marine documentation associates OCIMF MEG4 with environmental forces; this is not an implemented schema criterion or a component-property clause citation. [docs/domains/marine-engineering/marine_implementation_metrics.md:L328-L335] [src/digitalmodel/data/mooring_component_schema.py:L42-L70] |
| API 2C | UNVERIFIED: no governing edition, clause or table for this validator was established; issue pending (4). |
| DNV-RP-C203 | UNVERIFIED: no governing edition, clause or table for this validator was established; issue pending (4). |

UNVERIFIED: the inventory's three standard leads do not establish a standards-compliance basis for this validator; no edition reconciliation or licensed-clause verification is claimed; issue pending (4).

For each standard lead, clause text not checked against the licensed copy; no licensed tabulated value is reproduced here. The current component documentation excludes paid-standard chain coefficients and lists anchor capacity, safety factors and tension-life curves outside its scope. [src/digitalmodel/data/marine_engineering/mooring_components/README.html:L23-L25] [src/digitalmodel/data/marine_engineering/mooring_components/README.html:L30-L37]

The provenance sidecar here binds data and source records rather than emitting a calculation `Citation` instance; standards-derived engineering calculations require separate qualification before reuse. [src/digitalmodel/data/mooring_component_schema.py:L92-L124] [src/digitalmodel/data/marine_engineering/mooring_components/README.html:L37-L38]

## 4. Validated worked examples and regression examples

| Case / class | Independent basis or regression scope | Input / expected result / tolerance | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| V1 / validated coefficient reproduction | MoorPy coefficient file and helper at revision `1fb29f8eca2618543b2df7056adfdfce0265737b`; this checks coefficient reproduction, not certified strength | Committed wire/line CSVs and retained excerpt; first wire diameter 20 mm gives breaking load 408.8 kN; every numeric column follows source equations; NumPy default relative tolerance 1e-7, absolute tolerance 0 in each field's units | `tests/marine_ops/marine_engineering/test_component_database_public.py::test_default_load_and_cited_pins` | PASS at BASE and after; offline test recomputes expectations from the source excerpt. [tests/marine_ops/marine_engineering/test_component_database_public.py:L41-L67] [src/digitalmodel/data/marine_engineering/mooring_components/sources/moorpy_coefficients.json:L21-L30] [src/digitalmodel/data/marine_engineering/mooring_components/sources/moorpy_coefficients.json:L52-L60] |
| R1 / regression | Module round trip; no independent physical validation | Copy of committed tables and manifest; 0 dataframe-equivalence assertion failures (pandas defaults; declared field units) and unchanged row provenance | `tests/marine_ops/marine_engineering/test_component_database_public.py::test_csv_round_trip` | PASS at BASE and after. [tests/marine_ops/marine_engineering/test_component_database_public.py:L91-L103] |
| R2 / regression | Generator snapshot; no independent physical validation | Generate in temporary directory; 0 differing bytes across three CSVs and manifest | `tests/marine_ops/marine_engineering/test_component_database_public.py::test_generator_reproduces_all_outputs` | PASS at BASE and after. [tests/marine_ops/marine_engineering/test_component_database_public.py:L158-L165] |
| R3 / regression | Generated-directory loader compatibility | Generate in temporary directory; 2 nonempty wire/line tables | `tests/marine_ops/marine_engineering/test_component_database_public.py::test_generated_directory_is_loadable` | PASS at BASE and after. [tests/marine_ops/marine_engineering/test_component_database_public.py:L168-L175] |

The two older suites implement their own property/database classes, so their results are regression evidence for those fixtures rather than independent qualification of this package. [tests/marine_ops/marine_engineering/test_component_database.py:L29-L66] [tests/marine_ops/marine_engineering/test_component_database.py:L192-L227] [tests/marine_ops/marine_engineering/legacy/test_component_database.py:L29-L66] [tests/marine_ops/marine_engineering/legacy/test_component_database.py:L192-L227]

Execution record: `PYTHONPATH=src uv run --no-sources python -m pytest` with the three component test files, `tests/contracts` and `tests/docs/test_digitalmodel_routing_contract.py`, followed by `-q -rA -p no:cacheprovider` and a scratch `--basetemp`; no addopts override and no solver imports. BASE pin above: 555 run, 555 passed, 0 failed, 0 skipped, 0 NOT_RUN; after: 555 run, 555 passed, 0 failed, 0 skipped, 0 NOT_RUN. Of these, 58 are component tests (22 production and 18 per fixture copy), 487 contracts and 10 routing tests. These are measured execution records, not README counts.

## 5. QA gates and executable checks

AQG r0, section 10, Python calculations, supplies G0, G1 and G2; G3 and G4 are NOT_APPLICABLE; G5 applies through evidence traceability. (AQG r0, sections 9–10.)

| Gate / check | Executable check | Numeric pass criterion and unit | Evidence / status |
| --- | --- | --- | --- |
| G0 / C01 | Run `test_malformed_numeric_row_rejected` for all parameters | 4 invalid inputs rejected with `ValueError`; 0 accepted invalid mbl values, kN | Production test, PASS. [tests/marine_ops/marine_engineering/test_component_database_public.py:L106-L113] |
| G0 / C02 | Run `test_missing_column_rejected` | 1 missing stiffness column rejected; 0 accepted missing EA columns, kN | Production test, PASS. [tests/marine_ops/marine_engineering/test_component_database_public.py:L116-L121] |
| G0 / C03 | Run `test_invalid_provenance_rejected` for all parameters | 5 altered sidecars rejected; 0 accepted row/terms/unit/digest/sampling defects, count | Production test, PASS. [tests/marine_ops/marine_engineering/test_component_database_public.py:L124-L140] |
| G0 / C04 | Run `test_wrong_manifest_row_count_rejected` and `test_malformed_sidecar_has_clear_error` | 2 invalid manifests rejected, count | Production tests, PASS. [tests/marine_ops/marine_engineering/test_component_database_public.py:L186-L198] |
| G0 / C05 | Run `test_complete_provenance_and_monotonic_families` and `test_monotonicity_is_enforced` | 0 nonpositive/nonfinite properties in declared units; 0 non-increasing mbl [kN] or air mass [kg/m] transitions; 1 altered monotonicity case rejected | Production tests, PASS. [tests/marine_ops/marine_engineering/test_component_database_public.py:L70-L88] [tests/marine_ops/marine_engineering/test_component_database_public.py:L201-L207] |
| G0 / C06 | Run `test_missing_sidecar_is_explicitly_unverified` and `test_provenance_lookup_does_not_guess` | 2 unsupported provenance requests rejected, count; no provenance inferred | Production tests, PASS. [tests/marine_ops/marine_engineering/test_component_database_public.py:L143-L155] |
| G1 / C07 | Execute the section-4 offline command; production test directly loads `ComponentDatabase`, which calls the schema module | 555 collected tests pass; 0 failures and 0 skipped/NOT_RUN cases, count | BASE and after PASS. [tests/marine_ops/marine_engineering/test_component_database_public.py:L14-L24] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L305-L317] |
| G2 / C08 | Run V1 against the pinned public coefficient/helper source and compare retained-source byte hashes | 0 mismatched source digests, count; numeric relative error at most 1e-7, dimensionless, absolute tolerance 0 kN or kg/m | Source opened successfully; test PASS; this is coefficient reproduction only. [tests/marine_ops/marine_engineering/test_component_database_public.py:L46-L65] [src/digitalmodel/data/marine_engineering/mooring_components/sources/moorpy_coefficients.json:L5-L15] |
| G3 | NOT_APPLICABLE: no time-domain dynamics model is implemented in the schema functions | 0 applicable dynamics checks, count | [src/digitalmodel/data/mooring_component_schema.py:L33-L163] |
| G4 | NOT_APPLICABLE: no diffraction mesh or hydrostatic body is implemented in the schema functions | 0 applicable diffraction checks, count | [src/digitalmodel/data/mooring_component_schema.py:L33-L163] |
| G5 / C09 | Resolve every pack citation against the code pin, verify each listed case appears as PASS in both logs, and match every open item to an issue/draft | 0 unresolved citations, 0 missing case results, 0 untracked open items, count | Citation resolver, test logs and draft cross-check; PASS. |

C01–C06 refer to functions in `tests/marine_ops/marine_engineering/test_component_database_public.py`; their complete node IDs are formed by that path followed by `::` and the named test function. [tests/marine_ops/marine_engineering/test_component_database_public.py:L106-L155] [tests/marine_ops/marine_engineering/test_component_database_public.py:L186-L207]

## 6. Known failure modes and pitfalls

sweep: 17 found

The following 17 reviewed sites include guarded errors and intentional compatibility behavior as well as confirmed gaps; they are not 17 confirmed defects. Each location is in `src/digitalmodel/data/mooring_component_schema.py` at the code pin. [src/digitalmodel/data/mooring_component_schema.py:L27-L163]

Sweep commands (whole package; Python files only):

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|5 ?%|int\(|float\(|to_numeric|astype|UNITS|\.get\(|DataFrame\(|provenance.*None' src/digitalmodel/data --glob '*.py'
rg -n 'cache|write|dump|save|open\(' src/digitalmodel/data --glob '*.py'
```

| Site | Symptom / cause / disposition | Detection and follow-up |
| --- | --- | --- |
| S01 units, lines 27–28 | Field units are prescribed labels, not automatic unit inference; apparent mass must not be interpreted as force. [src/digitalmodel/data/mooring_component_schema.py:L27-L28] [src/digitalmodel/data/marine_engineering/mooring_components/README.html:L16-L17] | C03 wrong-unit rejection; no defect asserted. |
| S02 diameter conversion, lines 37–39 | `float` converts identity values without locale normalization; failures propagate. [src/digitalmodel/data/mooring_component_schema.py:L37-L39] | No detection check exists for locale text; issue pending (1). |
| S03 numeric coercion, lines 51–53 | Invalid strings become NaN and are then rejected along with zero, negatives and infinity; no silent zero default. [src/digitalmodel/data/mooring_component_schema.py:L50-L60] | C01; locale/thousands strings are not normalized, issue pending (1). |
| S04 string coercion, lines 55–56 | Missing object values become nonempty text before validation; notes checks missingness only after string conversion. [src/digitalmodel/data/mooring_component_schema.py:L55-L61] | No detection check exists; scratch probes confirmed missing grade accepted; issue pending (1). |
| S05 monotonicity, lines 65–70 | Empty/single-row groups provide no increasing-diameter comparison; empty chain table is an acknowledged seed gap. [src/digitalmodel/data/mooring_component_schema.py:L65-L70] [src/digitalmodel/data/marine_engineering/mooring_components/README.html:L30-L30] | C05 for populated groups; engineering coverage remains #1200. |
| S06 source `.get`, lines 78–80 | Missing/non-string source fields raise rather than silently default. [src/digitalmodel/data/mooring_component_schema.py:L78-L82] | C03; no defect asserted. |
| S07 UTC, lines 87–89 | Missing timezone or nonzero UTC offset raises; no epoch fallback. [src/digitalmodel/data/mooring_component_schema.py:L87-L89] | No detection check exists for timezone variants; issue pending (1). |
| S08 units, lines 95–96 | Exact schema/unit equality is required; no conversion is attempted. [src/digitalmodel/data/mooring_component_schema.py:L95-L96] | C03; no defect asserted. |
| S09 exceptions, lines 123–124 | Key/type/attribute/file errors are rethrown as `ValueError`, not swallowed. [src/digitalmodel/data/mooring_component_schema.py:L123-L124] | C03; no defect asserted. |
| S10 optional sidecar, lines 129–133 | JSON syntax errors raise, but a present JSON null becomes the legacy sentinel. [src/digitalmodel/data/mooring_component_schema.py:L129-L133] | C04 covers syntax errors; no detection check exists for null; issue pending (2). |
| S11 CSV route, lines 138–140 | Missing provenance changes NA parsing and bypasses schema validation, including the null case. [src/digitalmodel/data/mooring_component_schema.py:L138-L140] | C06 covers absent sidecar; null remains issue pending (2). |
| S12 missing CSV, lines 141–145 | Curated missing CSV raises; legacy missing CSV becomes an empty dataframe. [src/digitalmodel/data/mooring_component_schema.py:L141-L145] | `test_legacy_chain_query_interface` confirms empty wire/line compatibility; #1200 for readiness. [tests/marine_ops/marine_engineering/test_component_database_public.py:L210-L220] |
| S13 validation route, lines 146–148 | Provenance validation runs only for a non-None decoded sidecar. [src/digitalmodel/data/mooring_component_schema.py:L146-L148] | No detection check exists for present-null bypass; issue pending (2). |
| S14 no evidence, lines 152–153 | Absent provenance raises explicitly instead of returning unknown evidence silently. [src/digitalmodel/data/mooring_component_schema.py:L151-L153] | C06; no defect asserted. |
| S15 lookup defaults, lines 154–159 | Unknown kind defaults to no candidates and raises; an unknown specification key with None can compare equal to a missing identity value. [src/digitalmodel/data/mooring_component_schema.py:L154-L159] | C06 covers no matches; no detection check exists for unknown keys; issue pending (1). |
| S16 sampling `.get`, line 162 | Direct lookup can return a None diameter source, although normal loading requires sampling metadata first. [src/digitalmodel/data/mooring_component_schema.py:L97-L100] [src/digitalmodel/data/mooring_component_schema.py:L160-L163] | C03 tests missing sampling source at load time; direct-call coverage issue pending (1). |
| S17 derivation presence, lines 120–122 | Nonempty unrelated formulas/coefficients satisfy the presence check without recomputation. [src/digitalmodel/data/mooring_component_schema.py:L120-L122] | No detection check exists; scratch probe confirmed acceptance; issue pending (3). |

The schema file has no cache write, `continue`, error-path literal empty-list/dictionary return, epoch fallback or percentage default; its exception handlers raise errors. [src/digitalmodel/data/mooring_component_schema.py:L33-L163]

Documentation pitfall: historical marine summaries describe certified coverage, while the current seed documentation states partial computed coverage and unqualified ranges; no certification should be inferred from those summaries. [docs/domains/marine-engineering/MARINE_ENGINEERING_PHASE_1-3_SUMMARY.md:L42-L48] [src/digitalmodel/data/marine_engineering/mooring_components/README.html:L28-L38] Follow-up: issue pending (4).

No incident was named for this package; #1200 is the discovered data-establishment issue referenced by the seed documentation, with the coverage limitation detected by C05 and the empty-chain assertion in V1. [src/digitalmodel/data/marine_engineering/mooring_components/README.html:L5-L5] [tests/marine_ops/marine_engineering/test_component_database_public.py:L41-L45]

## 7. Lessons log

| Date | Change reference | Lesson | Evidence | Pack change / shared entry |
| --- | --- | --- | --- | --- |
| 2026-10-18 | Initial pack | Distinguish no sidecar from invalid present metadata; legacy compatibility is not curated readiness. | [src/digitalmodel/data/mooring_component_schema.py:L129-L148] | S10–S13; issue pending (2); shared entry not assigned. |
| 2026-10-18 | Initial pack | Byte/source metadata checks do not establish truth of a nonempty derivation. | [src/digitalmodel/data/mooring_component_schema.py:L101-L122] | S17; issue pending (3); shared entry not assigned. |

## 8. Ownership

Review assignment for this draft: calculations quality manager, with an independent calculations reviewer as deputy role; appointment of the deputy and any future data-specific quality role remains a maintainer action, issue pending (4). Producing discipline: data pipelines and calculations. Escalation policy: ambiguous provenance, units, editions or qualification criteria go to the calculations quality manager; cross-family questions go to the maintainers. These are proposed pack responsibilities, not recorded approvals. (AQG r0, section 11.)

## 9. Dependencies and implementation inventory

| Dependency | Direction / constraint | Evidence |
| --- | --- | --- |
| NumPy / pandas | Runtime numeric/table validation | [src/digitalmodel/data/mooring_component_schema.py:L50-L70] |
| Python hashlib / JSON / datetime | Byte digests, sidecar decoding and UTC validation | [src/digitalmodel/data/mooring_component_schema.py:L33-L34] [src/digitalmodel/data/mooring_component_schema.py:L83-L89] [src/digitalmodel/data/mooring_component_schema.py:L129-L133] |
| `scripts/data/generate_mooring_components.py` | Upstream offline generator uses schema constants and identity/hash helpers; polynomial calculation is outside this package | [scripts/data/generate_mooring_components.py:L11-L24] [scripts/data/generate_mooring_components.py:L28-L42] |
| CSV tables / JSON sidecar / retained source excerpt | Upstream component dataset; raw-byte and identity binding, not solver results | [src/digitalmodel/data/mooring_component_schema.py:L101-L122] |
| `ComponentDatabase` | Downstream caller of the schema functions; no solver interface in this route | [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L305-L317] |

A8 audit record: map ref and data revision are recorded in the header; parsing all 13 map families found 0 families with rows inside `src/digitalmodel/data/`, so the complete in-package inventory is empty. Counted families: 0; counted-family grep: NOT_APPLICABLE because no map family has an in-package row. The map declares its data revision independently of this pack's code pin. [docs/registry/implementation-map.yaml:L1-L4]

| A8 inventory | Implementations to list | Further second implementations |
| --- | --- | --- |
| Empty: no in-package map family | 0 | NOT_APPLICABLE; no counted-family grep |

The supplemental whole-repository Python search, including scripts and both test copies, used `rg -n 'mooring_component_schema|def (validate_table|validate_provenance|find_provenance)|TABLE_COLUMNS|IDENTITY_FIELDS' . --glob '*.py'`. The generator uses the schema helpers, and the production loader delegates to this module; the two older tests instead define their own component databases. [scripts/data/generate_mooring_components.py:L11-L13] [src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py:L305-L317] [tests/marine_ops/marine_engineering/test_component_database.py:L192-L227] [tests/marine_ops/marine_engineering/legacy/test_component_database.py:L192-L227]

Map follow-up: add CSV schema validation, byte-bound provenance and component identity lookup method families, including their in-package definitions and repository callers, before they become counted families; issue pending (4). These methods are implemented here rather than physical mooring analysis. [src/digitalmodel/data/mooring_component_schema.py:L42-L70] [src/digitalmodel/data/mooring_component_schema.py:L92-L163]

## 10. Coverage and open items

Read scope: the complete package Python file and all package subdirectories were inventoried; CSVs, sidecar, retained coefficient JSON and accompanying HTML were examined for the validator path; four discovery spreadsheets with identifying names were inventoried without evaluating their engineering contents, issue pending (4). [src/digitalmodel/data/mooring_component_schema.py:L1-L163] [src/digitalmodel/data/marine_engineering/mooring_components/README.html:L5-L38]

| Files read / discovered | Disposition and follow-up |
| --- | --- |
| `src/digitalmodel/data/mooring_component_schema.py` | Complete Python coverage; S01–S17; issue pending (1), issue pending (2), issue pending (3). |
| Package `marine_engineering/mooring_components/` | README.html, three property CSVs, provenance.json, sources/moorpy_coefficients.json and sources/MoorPy-LICENSE.txt; component qualification gaps remain #1200. |
| `scripts/data/generate_mooring_components.py` and downstream `component_database.py` | Supporting source/loader route; historical formulas are not qualified hand-check references; #1200. |
| Three `tests/marine_ops/marine_engineering/` component files | `test_component_database_public.py`, `test_component_database.py`, `legacy/test_component_database.py`; run all existing nodes, with regression-only status for fixture implementations; #1200 for physical qualification. |
| `docs/domains/data/DATA_REORGANIZATION_PLAN.md` | Historical empty-directory claim; compare current seed rather than accepting the plan's counts; issue pending (4). [docs/domains/data/DATA_REORGANIZATION_PLAN.md:L24-L30] |
| `docs/domains/data/DATA_ARCHITECTURE_IMPROVEMENTS_COMPLETE.md`, `DATA_CATALOG_IMPLEMENTATION_SUMMARY.md`, `MIGRATION_SUMMARY.md` | Old catalogue/migration paths and counts; not evidence of the live validator's coverage; issue pending (4). [docs/domains/data/DATA_ARCHITECTURE_IMPROVEMENTS_COMPLETE.md:L102-L120] [docs/domains/data/DATA_CATALOG_IMPLEMENTATION_SUMMARY.md:L9-L11] [docs/domains/data/MIGRATION_SUMMARY.md:L9-L23] |
| `docs/domains/integration/MIGRATION_GUIDE.md`, `INTEGRATION_COMPLETE.md` | Historical catalogue/migration integration references; issue pending (4). [docs/domains/integration/MIGRATION_GUIDE.md:L170-L172] [docs/domains/integration/INTEGRATION_COMPLETE.md:L292-L295] |
| `docs/domains/marine-engineering/marine_implementation_metrics.md`, `MARINE_ENGINEERING_DOCUMENTATION_INDEX.md`, `MARINE_ENGINEERING_PHASE_1-3_SUMMARY.md` | Historical component counts and qualification claims are not adopted; issue pending (4). [docs/domains/marine-engineering/marine_implementation_metrics.md:L298-L311] [docs/domains/marine-engineering/MARINE_ENGINEERING_DOCUMENTATION_INDEX.md:L447-L453] [docs/domains/marine-engineering/MARINE_ENGINEERING_PHASE_1-3_SUMMARY.md:L354-L360] |
| `docs/domains/project-docs/MIGRATION_GUIDE.md`, `REORGANIZATION_SUMMARY.md`, `IMPLEMENTATION_PHASE4_SUMMARY.md` | Extraction/migration and broader integration references; no additional validator qualification established; issue pending (4). [docs/domains/project-docs/MIGRATION_GUIDE.md:L32-L35] [docs/domains/project-docs/REORGANIZATION_SUMMARY.md:L29-L32] [docs/domains/project-docs/IMPLEMENTATION_PHASE4_SUMMARY.md:L395-L398] |
| Broad prefix search false matches | `docs/README.md`, `docs/maps/digitalmodel-operator-map.md`, `docs/data/OCIMF_CORPUS_README.md`, `docs/integrations/steel-shapes.html`, `docs/domains/structural/api_std_2rd_migration_summary.md`, `docs/domains/project-docs/IMPLEMENTATION_PHASE1_SUMMARY.md`, `IMPLEMENTATION_PHASE2_SUMMARY.md`, `OPTIMIZATION_ANALYSIS.md`, `RISER_PHASE1_SUMMARY.md`, and two synthetic-rope/artifact-generation plan documents were inspected as search context; their other namespaces are not this validator's method basis; issue pending (4) tracks documentary routing. |
| `docs/registry/module-routing.yaml`, `tests/docs/test_digitalmodel_routing_contract.py` | No existing data entry; no registry edit is made because domain discovery requires an initializer; issue pending (4). [tests/docs/test_digitalmodel_routing_contract.py:L16-L22] [tests/docs/test_digitalmodel_routing_contract.py:L97-L113] |

| Open item | Evidence / required next action | Tracking |
| --- | --- | --- |
| U1 registry maturity and route | UNVERIFIED: no assigned registry maturity or supported namespace route established; resolve routing without adding a conflicting domain. | issue pending (4) |
| U2 API 2C | UNVERIFIED: governing edition/clause/table not established. | issue pending (4) |
| U3 DNV-RP-C203 | UNVERIFIED: governing edition/clause/table not established. | issue pending (4) |
| U4 inventory standard basis | UNVERIFIED: OCIMF/API 2C/DNV-RP-C203 leads do not establish validator compliance; reconcile inventory with code and docs. | issue pending (4) |
| Input-contract test gaps | Reject missing strings/unknown identity keys; cover locale numeric text, timezone errors and direct sampling lookup; S02–S04, S07, S15–S16. | issue pending (1) |
| Invalid-present sidecar | Reject JSON null before compatibility routing; S10–S13. | issue pending (2) |
| Derivation truth | Bind computed formulas/coefficients to source evidence and row values; S17. | issue pending (3) |
| Map gaps, stale docs and discovery names | Add section-3 method families to map; reconcile stale paths/counts; introduce neutral discovery identifiers; appoint deputy. | issue pending (4) |
| Engineering qualification / missing physical worked example | Establish eligible complete chain data, wire/fibre product ranges, buoyancy-sign support and independent component qualifications; V1 qualifies reproduction only. [src/digitalmodel/data/marine_engineering/mooring_components/README.html:L30-L38] | #1200 |

No private values or private sidecar entries are used; the public seed documentation excludes private validation from this slice. [src/digitalmodel/data/marine_engineering/mooring_components/README.html:L38-L38]
