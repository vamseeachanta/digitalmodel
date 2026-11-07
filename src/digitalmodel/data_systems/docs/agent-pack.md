# Data systems knowledge pack

| Header | Value |
| --- | --- |
| Module | data_systems, top level |
| Repository | digitalmodel |
| Package | `src/digitalmodel/data_systems/`; directly contained Python files only |
| Pack revision | 1 |
| Code revision | `aebd7e4bc5c3dda5f7433e20e021dd58ac17d0b3` |
| Analysis family | EC; data pipelines, using the Python calculations gates of AQG r0 section 10 |
| Quality role | Calculations quality manager; deputy to be designated by the maintainer |
| Status | DRAFT |

## 1. Purpose and scope

The top-level interfaces validate vessel reference records, load explicitly supplied engineering catalogues, count metocean threshold exposure and serialise solver-reference evidence. [src/digitalmodel/data_systems/vessel_specs.py:L142-L175] [src/digitalmodel/data_systems/engineering_databases.py:L149-L165] [src/digitalmodel/data_systems/metocean_statistics.py:L34-L62] [src/digitalmodel/data_systems/solver_reference_export.py:L147-L154]

The routing registry records beta maturity and directs this domain to `tests/data_systems/`. [docs/registry/module-routing.yaml:L113-L122]

Catalogue validation establishes consistency rather than source authenticity, publication rights or engineering suitability; reference export always records `NOT_EVALUATED`. [src/digitalmodel/data_systems/engineering_databases.py:L4-L9] [src/digitalmodel/data_systems/solver_reference_export.py:L132-L144]

The exposure adapter counts observations rather than contiguous weather windows or annual extrapolations; naive timestamps mean UTC and the final observation represents a full interval. [src/digitalmodel/data_systems/metocean_statistics.py:L40-L55]

Scope excludes child packages; their directory inventory and map boundaries are recorded in section 9, with remaining coverage assigned in section 10.

### Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-11-07 | Initial top-level method, QA and risk inventory. |
| 1 | 2026-11-07 | Correct install-dependent test status, source preconditions, citations, scope and follow-up routes. |

Table 0. Pack revision history.

## 2. Inputs and outputs

| Input | Unit and convention | Allowed range and default | Source |
| --- | --- | --- | --- |
| Vessel rows, schema, provenance manifest | Units encoded in field names; technical unknowns are null | Schema required; numeric specifications finite and positive; depth bounds ordered; no inferred values | `validate_rows`, `_check_ranges`. [src/digitalmodel/data_systems/vessel_specs.py:L124-L164] |
| Source records | HTTPS URL; UTC RFC3339 timestamp when jsonschema's optional date-time checker is registered; digest hexadecimal; size bytes | Retrieved sources require a digest, positive byte count and recorded terms; row dates must agree; every non-null fact needs a retrieved source; future-date rejection depends on the current UTC clock, including clock skew | `_source_index`, `_check_provenance`; S05 records checker and clock preconditions. [src/digitalmodel/data_systems/vessel_specs.py:L69-L121] |
| Catalogue directory and identifier | Caller-supplied `schema.json`, `data.json`, `sources.yml`; exact schema unit labels | Four identifiers; `allow_synthetic=False`; no conversion or numeric defaults | `load_database`, catalogue contract. [src/digitalmodel/data_systems/engineering_databases.py:L149-L188] [docs/plans/engineering-databases-plan-lite.md:L40-L55] |
| Catalogue geometry and strength | Line pipe: m, MPa; flexible pipe: m, N, N*m^2, kg/m, Pa, degC | Relationship checks reject incompatible geometry and strength; conditions accompany flexible properties | Contract and `_engineering`. [docs/plans/engineering-databases-plan-lite.md:L121-L155] [src/digitalmodel/data_systems/engineering_databases.py:L77-L95] |
| Section and soil properties | Section area m², density kg/m³, dry mass kg/m; displacement m, resistance N/m, stiffness N/m² | Mass equals area times density; soil starts at the origin, increases in displacement and matches first-segment secant stiffness | `_engineering`, `_soil_relationships`. [src/digitalmodel/data_systems/engineering_databases.py:L86-L115] |
| Metocean records, parameter, thresholds | Caller-defined matching physical units; datetime timestamps converted to UTC | At least two regularly increasing observations; distinct nonempty finite thresholds; no text-number coercion or unit inference | `hours_below_threshold`. [src/digitalmodel/data_systems/metocean_statistics.py:L21-L31] [src/digitalmodel/data_systems/metocean_statistics.py:L64-L95] |
| Reference rows and unit allowlist | Exact quantity-specific strings; no unit aliases; no coordinate, sign or phase transformation | Nonblank condition, quantity and supported source unit; warnings absent, null or list of strings; state defaults to unknown | `_prepare`; caller supplies units, no conversions. [src/digitalmodel/data_systems/solver_reference_export.py:L110-L144] [src/digitalmodel/data_systems/solver_reference_export.py:L10-L14] |
| Reference JSON / CSV | JSON number syntax; CSV contains JSON in each present cell | Blank CSV cell is absent; null is explicit null; duplicate keys, malformed widths and inconsistent annotations rejected | Strict readers. [src/digitalmodel/data_systems/solver_reference_export.py:L195-L252] |

Table 1. Input contracts; caller-defined units require an upstream source contract.

| Output | Unit and meaning | Source |
| --- | --- | --- |
| Vessel validation | Returns None on success; raises on structural, range or provenance failure; CLI prints a valid marker per dataset | `validate_rows`, `validate_dataset`, `main`. [src/digitalmodel/data_systems/vessel_specs.py:L142-L185] |
| Catalogue rows | New dictionaries in file order with resolved citation mapping; stored units retained | Loader and join. [src/digitalmodel/data_systems/engineering_databases.py:L159-L164] [src/digitalmodel/data_systems/engineering_databases.py:L202-L235] |
| Threshold exposure | Nested threshold → UTC sample year → observed hours | Backend call and return mapping. [src/digitalmodel/data_systems/metocean_statistics.py:L108-L116] |
| Reference interchange | JSON text / JSON-cell CSV; original fields retained; explicit evidence state and NOT_EVALUATED | Snapshot, annotation and writers. [src/digitalmodel/data_systems/solver_reference_export.py:L94-L107] [src/digitalmodel/data_systems/solver_reference_export.py:L132-L144] [src/digitalmodel/data_systems/solver_reference_export.py:L254-L270] |

Table 2. Outputs describe data consistency and evidence, without conferring engineering acceptance.

## 3. Method and governing standards

| Method | Implementation and reference | Qualification limit |
| --- | --- | --- |
| Vessel contract | `parse_json` rejects duplicate members and nonstandard numeric constants; `validate_rows` applies JSON Schema 2020-12, then ranges and provenance joins | Offline validation cannot establish factual source transcription. [src/digitalmodel/data_systems/vessel_specs.py:L19-L41] [src/digitalmodel/data_systems/vessel_specs.py:L142-L164] |
| Catalogue contract | Duplicate-aware JSON/YAML loaders, finite recursion, schema validation, citation joins and synthetic opt-in | Reviewed schema is supplied alongside data; authenticity and rights need separate review. [src/digitalmodel/data_systems/engineering_databases.py:L40-L74] [src/digitalmodel/data_systems/engineering_databases.py:L169-L199] [src/digitalmodel/data_systems/engineering_databases.py:L211-L235] |
| Section mass | Dry mass per length = area × density; `math.isclose` relative tolerance `1e-6`, absolute tolerance `0.0 kg/m`; comparison basis is PEP 485, A Function for testing approximate equality (2015), ["Proposed Implementation"](https://peps.python.org/pep-0485/#proposed-implementation) | Precision policy concerns decimal rounding, not design acceptance; PEP 485 does not prescribe this catalogue tolerance. [src/digitalmodel/data_systems/engineering_databases.py:L28-L33] [src/digitalmodel/data_systems/engineering_databases.py:L86-L93] |
| Soil initial secant | The second curve point (first segment): `resistance[1] / displacement[1]`; relative tolerance `1e-6`, absolute tolerance `0.0 N/m²`, using the same PEP 485 comparison policy | Equal depth bounds permitted; a zero second-point resistance is rejected, without searching later points. [src/digitalmodel/data_systems/engineering_databases.py:L98-L115] [tests/data_systems/test_engineering_databases.py:L377-L381] |
| Exposure | `hours_below_threshold` delegates interval-hours × count(value < threshold) to `nb_hours_below_threshold`; code names NIST/SEMATECH e-Handbook (2012), section 1.3.3.14, and BIPM SI Brochure, ninth edition (2019), table 8; backend source revision `3f99e50bf08cbf0ec47370c0a8eee349b0226b85` is pinned in its docstring | Same-unit thresholds; UTC sample-year assignment; no duration-qualified operability. [src/digitalmodel/data_systems/metocean_statistics.py:L4-L12] [src/digitalmodel/data_systems/metocean_statistics.py:L40-L55] [src/digitalmodel/data_systems/metocean_statistics.py:L108-L116] |
| Citation URI syntax | RFC 3986, Uniform Resource Identifier (URI): Generic Syntax (2005), ["Characters"](https://www.rfc-editor.org/rfc/rfc3986.html#section-2) and ["Syntax Components"](https://www.rfc-editor.org/rfc/rfc3986.html#section-3), supplies the stated basis for escape and component checks | Syntax checks do not establish source rights or factual accuracy. [src/digitalmodel/data_systems/engineering_databases.py:L122-L146] |
| Positioning-class vocabulary | IMO equipment-class vocabulary restricts `dp_class`; the code and schema do not identify an edition or clause | Vocabulary membership does not establish current classification; source-basis completion remains under O6. [src/digitalmodel/data_systems/vessel_specs.py:L142-L150] [assets/data/vessel_specs_hlv.schema.json:L45-L50] |
| Reference evidence | Duplicate-aware JSON, nonfinite rejection and nonzero-underflow refusal; code cites RFC 8259 (2017), sections 4 and 6 | Python float rounding remains; decoded caller values have no source token to check. [src/digitalmodel/data_systems/solver_reference_export.py:L23-L33] [src/digitalmodel/data_systems/solver_reference_export.py:L58-L91] |
| Citation structure | Catalogue rows resolve `citation_id` to citation metadata; reference rows retain caller evidence | This mapping is not an emitted calculation `Citation` sidecar; no standards-derived design coefficient is introduced by these paths. [src/digitalmodel/data_systems/engineering_databases.py:L211-L235] [src/digitalmodel/data_systems/solver_reference_export.py:L10-L18] |

Table 3. Implemented algorithms and their stated reference bases.

No licensed engineering standard supplies a design criterion in these top-level methods; catalogue source policy excludes standards as value sources. [docs/plans/engineering-databases-plan-lite.md:L71-L80] [src/digitalmodel/data_systems/engineering_databases.py:L77-L115] [src/digitalmodel/data_systems/metocean_statistics.py:L40-L55] [src/digitalmodel/data_systems/solver_reference_export.py:L16-L24]

## 4. Validated worked examples

All inputs below are synthetic protocol or arithmetic fixtures; the catalogue fixtures explicitly identify invented test data. [tests/data_systems/test_engineering_databases.py:L39-L88] [tests/data_systems/test_solver_reference_export.py:L549-L555] [tests/data_systems/test_metocean_statistics.py:L4-L8]

Results are execution observations at the code pin, using the commands in section 10; PASS does not establish fitness of a production dataset.

| Case / class | Independent basis or regression input | Expected value, tolerance and basis | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| V1 section mass; validated arithmetic | Hand product of fixture area 0.01 m² and density 1000.0 kg/m³ | 10.0 kg/m; exact stored return, consistency tolerance `1e-6` relative | `tests/data_systems/test_engineering_databases.py::test_mass_consistency_precision_boundary[0.01-10.0-True-structure_properties]` | PASS; hand basis and assertion. [tests/data_systems/test_engineering_databases.py:L297-L315] |
| V2 soil secant; validated arithmetic | Hand quotient of fixture resistance 10.0 N/m by displacement 0.01 m | 1000.0 N/m²; exact stored return, consistency tolerance `1e-6` relative | `tests/data_systems/test_engineering_databases.py::test_secant_consistency_precision_boundary[1000.0-True-soil_stiffness]` | PASS; hand basis and assertion. [tests/data_systems/test_engineering_databases.py:L318-L331] |
| R1 null gaps; regression | Hand-written vessel row / manifest; fixture values are not repeated here | 0 validation exceptions, count; null gaps allowed by fixture schema | `tests/data_systems/test_vessel_specs.py::test_accepts_known_values_and_null_gaps` | PASS. [tests/data_systems/test_vessel_specs.py:L128-L129] |
| R2 synthetic opt-in; regression | Marked line-pipe fixture copied to an external temporary directory | 1 synthetic row returned with its exact citation after explicit opt-in; default rejects | `tests/data_systems/test_engineering_databases.py::test_synthetic_requires_explicit_opt_in[line_pipe]` | PASS. [tests/data_systems/test_engineering_databases.py:L76-L115] |
| R3 reference roundtrip; regression | `examples/domains/data_systems/solver_reference_roundtrip.py`; literal state tokens | 6 ordered states: computed, uncomputed, cached_unqualified, absent, invalid, unknown; 0 altered source-unit strings | `tests/data_systems/test_solver_reference_export.py::test_documented_worked_example_has_the_enumerated_answer` | PASS; identity/state regression, not independent physical validation. [tests/data_systems/test_solver_reference_export.py:L549-L582] |
| R4 offline retrieval; out-of-package script regression | Injected synthetic transport exercises `scripts/fetch_vessel_specs.py`; no website contacted | 11 document files; saved manifest equals returned manifest; 0 unretrieved statuses | `tests/data_systems/test_fetch_vessel_specs.py::test_emitted_manifest_contract` | PASS; supporting script coverage is outside the package inventory. [tests/data_systems/test_fetch_vessel_specs.py:L26-L29] [tests/data_systems/test_fetch_vessel_specs.py:L57-L76] [scripts/fetch_vessel_specs.py:L4-L25] |
| N1 threshold count; validated arithmetic | Four half-hour samples split across UTC years; values [1, 2, 3, 1] | Threshold 2: 0.5 h each year; threshold 3: 1.0 h then 0.5 h; exact arithmetic equality | `tests/data_systems/test_metocean_statistics.py::test_hand_count_across_year_boundary_and_strict_threshold` | PASS with precompiled bytecode; NOT_RUN in the source-import record; section 10 separates both modes. [tests/data_systems/test_metocean_statistics.py:L28-L40] |

Table 4. Three validated arithmetic examples and four regressions, including one out-of-package script regression.

## 5. QA gates

The gate assignment uses AQG r0 section 10, Python calculations, and section 9, G5; checks below are pack requirements with executable evidence, rather than declarations of gate clearance.

| Gate | Applicability | Module route and evidence |
| --- | --- | --- |
| G0 | Applies | Schema, source joins, exact units and metocean input validation: C01–C03, C11 |
| G1 | Applies | Python imports and offline pytest; C04 core route, C05 optional backend route; neither exports nor loads a solver model |
| G2 | Applies | Closed-form section mass and soil secant, C06–C07; exposure count C08 passes with precompiled bytecode |
| G3 | NOT_APPLICABLE | These functions validate, count or serialise records without a dynamics time integration. [src/digitalmodel/data_systems/vessel_specs.py:L150-L175] [src/digitalmodel/data_systems/engineering_databases.py:L169-L188] [src/digitalmodel/data_systems/metocean_statistics.py:L108-L116] [src/digitalmodel/data_systems/solver_reference_export.py:L254-L270] |
| G4 | NOT_APPLICABLE | No diffraction mesh, hydrostatics solve or response solver is implemented by these routes. [src/digitalmodel/data_systems/vessel_specs.py:L150-L175] [src/digitalmodel/data_systems/engineering_databases.py:L169-L188] [src/digitalmodel/data_systems/metocean_statistics.py:L108-L116] [src/digitalmodel/data_systems/solver_reference_export.py:L254-L270] |
| G5 | Applies | Evidence and state retention C09–C10, traceability C12, numeric-token retention C13; retain limitations with every exported record |

Table 5. Gate applicability for the bounded top-level interfaces.

| Check | Gate | Function/output and numeric pass criterion, with unit | Executable evidence and observed result |
| --- | --- | --- | --- |
| C01 | G0 | `validate_rows`: 0 accepted invalid field mutations, count | `tests/data_systems/test_vessel_specs.py::test_rejects_invalid_fields`, `tests/data_systems/test_vessel_specs.py::test_rejects_reversed_operating_depths`, `tests/data_systems/test_vessel_specs.py::test_rejects_capacity_without_basis`; PASS. [tests/data_systems/test_vessel_specs.py:L80-L98] [tests/data_systems/test_vessel_specs.py:L141-L177] |
| C02 | G0 | `_check_provenance`: 0 accepted missing, unknown, refused or mismatched-date joins, count | `tests/data_systems/test_vessel_specs.py::test_rejects_untraceable_fields`, `tests/data_systems/test_vessel_specs.py::test_rejects_incomplete_manifest`; PASS. [tests/data_systems/test_vessel_specs.py:L114-L125] [tests/data_systems/test_vessel_specs.py:L180-L186] |
| C03 | G0 | `load_database`: 0 admitted wrong-unit / unresolved-citation / duplicate-key mutations, count | `tests/data_systems/test_engineering_databases.py::test_units_are_exact`, `tests/data_systems/test_engineering_databases.py::test_row_citation_join_is_required`, `tests/data_systems/test_engineering_databases.py::test_duplicate_mapping_keys_are_rejected`; PASS. [tests/data_systems/test_engineering_databases.py:L141-L154] [tests/data_systems/test_engineering_databases.py:L224-L235] [tests/data_systems/test_engineering_databases.py:L411-L424] |
| C04 | G1 | Core functions and contract imports: 0 failed tests, count | Core partition in section 10; current BASE and after each have 1179 PASS and one metadata-dependent SKIP; historical 1180 PASS is a separate record; routing checks exercise unique YAML mappings. [tests/docs/test_digitalmodel_routing_contract.py:L18-L40] |
| C05 | G1 | `hours_below_threshold` backend import and tests: 0 collection errors, count | `tests/data_systems/test_metocean_statistics.py`; 33 PASS with precompiled bytecode; source-import collection fails under the full S19 precondition. [tests/data_systems/test_metocean_statistics.py:L14-L18] [pyproject.toml:L119-L122] |
| C06 | G2 | `_engineering`: mass product 10.0 kg/m, relative tolerance `1e-6`, absolute tolerance 0.0 kg/m; boundary mutations classified exactly | `tests/data_systems/test_engineering_databases.py::test_mass_consistency_precision_boundary`; PASS all 7 cases. [tests/data_systems/test_engineering_databases.py:L297-L315] |
| C07 | G2 | `_soil_relationships`: 1000.0 N/m² secant, relative tolerance `1e-6`, absolute tolerance 0.0 N/m²; boundary mutations classified exactly | `tests/data_systems/test_engineering_databases.py::test_secant_consistency_precision_boundary`; PASS all 5 cases. [tests/data_systems/test_engineering_databases.py:L318-L331] |
| C08 | G2 | `hours_below_threshold`: threshold 2 returns 0.5 h in each UTC year, absolute error 0.0 h | N1 test node in section 4; PASS with precompiled bytecode. [tests/data_systems/test_metocean_statistics.py:L28-L40] |
| C09 | G5 | `SolverReferenceExporter`: 0 changed source evidence fields and 0 qualified rows, count | `tests/data_systems/test_solver_reference_export.py::test_documented_worked_example_has_the_enumerated_answer`, `tests/data_systems/test_solver_reference_export.py::test_successful_helper_without_output_remains_absent`; PASS. [tests/data_systems/test_solver_reference_export.py:L189-L209] [tests/data_systems/test_solver_reference_export.py:L549-L582] |
| C10 | G5 | Empty exports and missing/null/empty warnings: 0 invented values, count | `tests/data_systems/test_solver_reference_export.py::test_empty_exports_are_well_defined`, `tests/data_systems/test_solver_reference_export.py::test_csv_distinguishes_missing_null_and_empty_warning_evidence`; PASS. [tests/data_systems/test_solver_reference_export.py:L183-L186] [tests/data_systems/test_solver_reference_export.py:L212-L225] |
| C11 | G0 | `hours_below_threshold`: 0 accepted missing / nonnumeric / irregular / mixed-time observations, count | Statistics invalid-input tests; PASS with precompiled bytecode. [tests/data_systems/test_metocean_statistics.py:L79-L145] |
| C12 | G5 | Pack traceability: 0 unresolved file/line citations and 0 missing section-4 node definitions, count | Resolve each citation with `git show` at the code pin; compare test IDs with PASS records; bounds resolution alone does not establish that a range supports its claim |
| C13 | G5 | Reference readers: 0 admitted nonzero-underflow numeric tokens, count; preserve such tokens as invalid source text | `tests/data_systems/test_solver_reference_export.py::test_readers_reject_nonzero_numeric_underflow`, `tests/data_systems/test_solver_reference_export.py::test_readers_retain_underflow_tokens_supplied_as_source_text`; PASS. [tests/data_systems/test_solver_reference_export.py:L402-L432] [tests/data_systems/test_solver_reference_export.py:L468-L476] |

Table 6. Thirteen checks: G0 four, G1 two, G2 three, G5 four; observed failures or unavailable execution remain explicit.

C03 also requires zero accepted URI, numeric-type and malformed-document mutations, count: `tests/data_systems/test_engineering_databases.py::test_malformed_citation_authority_without_optional_checker`, `tests/data_systems/test_engineering_databases.py::test_numeric_values_are_finite_and_typed`, `tests/data_systems/test_engineering_databases.py::test_malformed_documents`; all PASS. [tests/data_systems/test_engineering_databases.py:L168-L188] [tests/data_systems/test_engineering_databases.py:L251-L260] [tests/data_systems/test_engineering_databases.py:L401-L409]

C07 additionally requires zero admitted contradictory geometry/curve mutations, count: `tests/data_systems/test_engineering_databases.py::test_engineering_relationships`; PASS. [tests/data_systems/test_engineering_databases.py:L273-L294]

C06/C07 additionally require zero accepted overflowing arithmetic mutations, count: `tests/data_systems/test_engineering_databases.py::test_unrepresentable_derived_arithmetic_is_a_validation_error`; PASS. [tests/data_systems/test_engineering_databases.py:L357-L364]

C09 additionally requires zero coerced nested evidence fields, zero admitted unsupported quantity-unit pairs and zero admitted malformed CSV tables, count: `tests/data_systems/test_solver_reference_export.py::test_nested_source_evidence_cannot_be_silently_coerced`, `tests/data_systems/test_solver_reference_export.py::test_unit_allowlist_is_quantity_specific`, `tests/data_systems/test_solver_reference_export.py::test_csv_reader_rejects_ambiguous_or_malformed_tables`; PASS. [tests/data_systems/test_solver_reference_export.py:L111-L114] [tests/data_systems/test_solver_reference_export.py:L236-L241] [tests/data_systems/test_solver_reference_export.py:L477-L491]

C10 additionally requires zero rows from empty readback, count: `tests/data_systems/test_solver_reference_export.py::test_empty_export_readback`; PASS. [tests/data_systems/test_solver_reference_export.py:L503-L508]


## 6. Known failure modes and pitfalls

sweep: 30 found

The count covers reviewed risk sites, including guards and intentional missing-data conventions; it does not mean 30 confirmed defects.

Exact sweep command at the code pin:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|unit|cache|\.get\(' src/digitalmodel/data_systems/*.py
```

The inspection also read every directly contained Python file to distinguish guards, successful empty results and error fallbacks; the scoped files contain no cache writer or record-skipping `continue`. [src/digitalmodel/data_systems/__init__.py:L1-L1] [src/digitalmodel/data_systems/vessel_specs.py:L19-L189] [src/digitalmodel/data_systems/engineering_databases.py:L40-L263] [src/digitalmodel/data_systems/metocean_statistics.py:L21-L116] [src/digitalmodel/data_systems/solver_reference_export.py:L58-L270]

| Site | Symptom / cause | Detection and disposition |
| --- | --- | --- |
| S01 catalogue numeric decoding | JSON `1e-999` can become zero before finite/range validation; independent review reproduced acceptance as 0.0 Pa for synthetic flexible-pipe pressure | No detection check exists for this loader; the observation is reported evidence, without a shipped reproduction; issue pending (4). [src/digitalmodel/data_systems/engineering_databases.py:L169-L188] |
| S02 vessel numeric decoding | `parse_json` also converts the nonzero token `1e-999` to 0.0; the range guard rejects zero specifications later, but parser evidence is lost | No detection check exists for token retention here; issue pending (4). [src/digitalmodel/data_systems/vessel_specs.py:L32-L41] [src/digitalmodel/data_systems/vessel_specs.py:L124-L129] |
| S03 backend output pairing | Nested `zip` ignores unmatched threshold/year entries; int/float conversion of backend outputs has no shape or finite-output assertion | No detection check exists for malformed backend shape; issue pending (5). [src/digitalmodel/data_systems/metocean_statistics.py:L111-L116] |
| S04 URL parsing | Empty hostname fallback / parsing exception rejects URL rather than substituting a source | C01; guarded input path, #1773. [src/digitalmodel/data_systems/vessel_specs.py:L44-L66] |
| S05 retrieval time parsing | RFC3339 strictness requires jsonschema's registered date-time checker, supplied transitively by optional `rfc3339-validator`; without it, `conforms` accepts unchecked syntax. Future-date rejection uses `datetime.now(timezone.utc)`, so acceptance varies with time and clock skew | C01 covers the installed-checker path; no detection check exists for a missing date-time checker or a controlled clock; O6 routes both gaps to #1773. [src/digitalmodel/data_systems/vessel_specs.py:L69-L80] |
| S06 missing provenance metadata | Digest and terms default to empty strings, size to None; retrieved sources then fail validation | C02 includes incomplete-metadata cases, PASS; #1773. [src/digitalmodel/data_systems/vessel_specs.py:L83-L100] [tests/data_systems/test_vessel_specs.py:L180-L186] |
| S07 null fact omission | Null fields are excluded from provenance coverage; treating null as known would invent evidence | C02; #1773. [src/digitalmodel/data_systems/vessel_specs.py:L103-L121] |
| S08 optional range / basis lookup | Missing bounds remain None; capacity/basis and operating-depth/lay-method consistency are checked | C01 covers field typing; paired relationships are included in C01; #1773. [src/digitalmodel/data_systems/vessel_specs.py:L130-L139] [tests/data_systems/test_vessel_specs.py:L161-L203] |
| S09 schema exception translation | Schema error is re-raised as ValueError; no success or empty-list fallback | C01; #1773. [src/digitalmodel/data_systems/vessel_specs.py:L150-L155] |
| S10 catalogue URI exception | ValueError sets valid=False, followed by explicit refusal | C03 includes authority tests, PASS; no silent fallback. [src/digitalmodel/data_systems/engineering_databases.py:L122-L146] [tests/data_systems/test_engineering_databases.py:L174-L214] |
| S11 finite traversal | Nonfinite floats rejected recursively; numeric strings are left to the schema | C03 schema contract; existing numeric-type tests; no coercion. [src/digitalmodel/data_systems/engineering_databases.py:L66-L74] [tests/data_systems/test_engineering_databases.py:L251-L260] |
| S12 mass tolerance | Absolute tolerance is zero, not a fallback mass; relative precision policy can be mistaken for a design margin | C06, PASS. [src/digitalmodel/data_systems/engineering_databases.py:L86-L93] |
| S13 soil zero and secant convention | Origin must be zero; equal-length curves and increasing displacement required; zero absolute tolerance is deliberate | C07, PASS; geometry mutations also tested. [src/digitalmodel/data_systems/engineering_databases.py:L98-L115] [tests/data_systems/test_engineering_databases.py:L273-L294] |
| S14 explicit catalogue failure | CatalogueValidationError propagates unchanged | C03; not an empty-result fallback. [src/digitalmodel/data_systems/engineering_databases.py:L189-L190] |
| S15 catalogue arithmetic overflow | OverflowError is converted to CatalogueValidationError, not a numeric default | C06/C07 check ordinary arithmetic; dedicated overflow test exists. [src/digitalmodel/data_systems/engineering_databases.py:L226-L231] [tests/data_systems/test_engineering_databases.py:L357-L364] |
| S16 metocean float coercion | Real values only; bool/text rejected before float conversion; overflow / nonfinite raises | C11, PASS with precompiled bytecode. [src/digitalmodel/data_systems/metocean_statistics.py:L21-L31] |
| S17 exposure units and sampling | Caller-defined units, naive UTC, full final interval and whole UTC sample-year assignment can be mistaken for continuous operability | C08, PASS with precompiled bytecode; real-observation and duration qualification remains issue pending (6). [src/digitalmodel/data_systems/metocean_statistics.py:L40-L55] |
| S18 missing observations and interval | `.get` returns None; missing or irregular observations are rejected rather than dropped | C11, PASS with precompiled bytecode. [src/digitalmodel/data_systems/metocean_statistics.py:L64-L95] |
| S19 backend exception | Only missing backend is translated to explanatory ImportError; transitive dependencies and import-time SyntaxError propagate | C05, PASS with precompiled bytecode; source-import collection has the preconditions below; issue pending (1). [src/digitalmodel/data_systems/metocean_statistics.py:L99-L106] [tests/data_systems/test_metocean_statistics.py:L145-L182] |
| S20 strict source float | External numeric text uses Python float; nonzero underflow explicitly raises; no decimal-separator inference | C13, PASS. [src/digitalmodel/data_systems/solver_reference_export.py:L75-L91] |
| S21 snapshot conversion | JSON errors raise; post-copy inequality rejects coercion of keys or arrays | C09 and existing nested-evidence tests, PASS. [src/digitalmodel/data_systems/solver_reference_export.py:L94-L107] [tests/data_systems/test_solver_reference_export.py:L237-L241] |
| S22 unit default | Missing quantity allowlist becomes an empty tuple, causing unsupported-unit refusal | C09 includes the quantity-specific unit test, PASS. [src/digitalmodel/data_systems/solver_reference_export.py:L110-L125] [tests/data_systems/test_solver_reference_export.py:L105-L114] |
| S23 warning lookup | Missing warnings are inspected as None but the missing key remains absent | C10, PASS; #2226. [src/digitalmodel/data_systems/solver_reference_export.py:L126-L131] |
| S24 state default | Missing / opaque numeric state maps to unknown; missing or null value maps to absent; nonnumeric to invalid | C09, PASS; #2226. [src/digitalmodel/data_systems/solver_reference_export.py:L132-L144] |
| S25 empty CSV input | Empty text constructs zero rows without treating that as solver success | C10, PASS; #2226. [src/digitalmodel/data_systems/solver_reference_export.py:L228-L231] [tests/data_systems/test_solver_reference_export.py:L503-L509] |
| S26 empty CSV cell | Comprehension excludes empty cells to retain absent keys; null and empty warnings remain distinct | C10, PASS; #2226. [src/digitalmodel/data_systems/solver_reference_export.py:L240-L249] |
| S27 CSV error translation | csv.Error raises ValueError rather than silently accepting a partial table | C09 readback and malformed-table tests, PASS. [src/digitalmodel/data_systems/solver_reference_export.py:L234-L252] [tests/data_systems/test_solver_reference_export.py:L480-L500] |
| S28 empty CSV output | Zero rows return empty text; this carries no execution evidence | C10, PASS; #2226. [src/digitalmodel/data_systems/solver_reference_export.py:L258-L261] |
| S29 catalogue schema failure | ValidationError becomes CatalogueValidationError with validator and location, without input values | C03; no success fallback. [src/digitalmodel/data_systems/engineering_databases.py:L191-L194] |
| S30 catalogue read / arithmetic / parse failure | Other named exceptions become CatalogueValidationError with restricted detail | C03; malformed documents refuse rather than return empty results. [src/digitalmodel/data_systems/engineering_databases.py:L195-L199] [tests/data_systems/test_engineering_databases.py:L395-L409] |

Table 7. Complete top-level risk-site classification; detection limits are explicit.

Existing incident #2226 concerns loss of warnings, unknown states and uncomputed zeros; C09/C10/C13 protect the generic interchange path, while other adapter changes remain outside this package. [tests/data_systems/test_solver_reference_export.py:L189-L209] [docs/domains/data_systems/solver-reference-exports.md:L102-L123]

The statistics collection failure requires source compiled at import, Python >= 3.12 and `filterwarnings = error`. Invalid escapes in metocean-stats 1.2.0 `tables/dir.py:36` and `tables/dir.py:59`, and in its transitive dependency `virocon/contours.py:869`, are compilation warnings promoted to SyntaxError. Independent review observed the virocon failure after compiling only metocean-stats. With precompiled bytecode for the installed dependencies, all 33 statistics cases pass, including N1/C05/C08/C11, with warning filters unchanged. A default uv installation without bytecode and an installation that compiles bytecode therefore have different collection outcomes; O1 concerns reproducible test-environment preparation, rather than a demonstrated backend calculation defect. [tests/data_systems/test_metocean_statistics.py:L14-L18] [pyproject.toml:L119-L122] [pytest.ini:L69-L72]

## 7. Lessons log

| Date | Entry | Lesson | Evidence | Pack change | Shared log |
| --- | --- | --- | --- | --- | --- |
| 2026-11-07 | reference-evidence | A successful helper exit, numeric zero and empty warning list do not qualify a reference result | #2226; regression protects absent output. [tests/data_systems/test_solver_reference_export.py:L189-L209] | Added C09/C10 and state-preservation pitfalls | Pending, #2226 |
| 2026-11-07 | numeric-token-loss | Underflow protection in the exporter is not shared by the catalogue and vessel JSON parsers | Separate float parsing policies. [src/digitalmodel/data_systems/solver_reference_export.py:L75-L91] [src/digitalmodel/data_systems/engineering_databases.py:L169-L180] [src/digitalmodel/data_systems/vessel_specs.py:L32-L36] | Added S01/S02 | issue pending (4) |
| 2026-11-07 | import-compilation | A collection failure can depend on installation bytecode and warning policy rather than runtime arithmetic | Source-import collection fails; the same 33 statistics cases pass with precompiled dependencies and unchanged warnings | Corrected S19 and O1; separated execution modes in section 10 | issue pending (1) |

Table 8. Initial lessons; append future entries without rewriting these rows.

## 8. Ownership

For this draft, the calculations quality manager reviews the Python calculations gates; the maintainer designates a deputy and resolves cross-package scope, issue pending (3).

The producing role prepares cited code claims and execution evidence; independent review determines clearance. Escalate ambiguous tolerances, source rights and unavailable checks to the calculations quality manager; unresolved cross-family matters go to the maintainer. These are pack controls under AQG r0 section 11, not recorded approvals.

## 9. Dependencies and implementation inventory

| Dependency / boundary | Role and limit |
| --- | --- |
| jsonschema and PyYAML | JSON Schema / format validation and duplicate-aware YAML catalogue loading. [src/digitalmodel/data_systems/engineering_databases.py:L21-L23] [src/digitalmodel/data_systems/engineering_databases.py:L118-L119] [src/digitalmodel/data_systems/vessel_specs.py:L150-L155] |
| pandas and metocean-stats 1.2.0 | Imported at analysis time; exposure delegated to the named backend; version declared in the metocean extra. [src/digitalmodel/data_systems/metocean_statistics.py:L97-L116] [pyproject.toml:L119-L122] |
| Vessel JSON/schema/provenance triplets | `validate_dataset` reads dataset, schema and shared manifest from its caller directory; source readback remains separate. [src/digitalmodel/data_systems/vessel_specs.py:L142-L175] |
| Four engineering catalogues | Explicit directories for line pipe, flexible pipe, section properties and soil stiffness; tests copy fixtures to temporary directories. [tests/data_systems/test_engineering_databases.py:L26-L38] [tests/data_systems/test_engineering_databases.py:L76-L98] |
| Export consumers | Caller-owned unit contract and source evidence; readers recompute annotations; no solver version is selected or executed. [src/digitalmodel/data_systems/solver_reference_export.py:L147-L172] [src/digitalmodel/data_systems/solver_reference_export.py:L174-L213] |
| `data/` child package | Migration and catalogue exports; excluded from this top-level pack. [src/digitalmodel/data_systems/data/__init__.py:L1-L30] |
| `data_manager/` child package | Configuration exports; excluded from this top-level pack. [src/digitalmodel/data_systems/data_manager/__init__.py:L1-L20] |
| `data_procurement/` child package | API/stream/cache/configuration and asset clients; separate pack routing already present. [src/digitalmodel/data_systems/data_procurement/__init__.py:L28-L49] [docs/registry/module-routing.yaml:L113-L117] |
| `data_quality/` child package | Completeness guardrail exports; excluded from this top-level pack. [src/digitalmodel/data_systems/data_quality/__init__.py:L1-L19] |
| `data_scraping/` child package | Scraper and validator exports; excluded from this top-level pack. [src/digitalmodel/data_systems/data_scraping/__init__.py:L15-L23] |
| `pyintegrity/` child package | Deprecation shim redirects to asset_integrity; excluded from this top-level pack. [src/digitalmodel/data_systems/pyintegrity/__init__.py:L1-L14] |

Table 9. Dependencies and all child-package boundaries; no private sidecar was read or produced.

### Implementation-map acceptance

Map reference: `origin/codex/implmap-work`, ref commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461`, file `docs/registry/implementation-map.yaml` at that reference. This is a separate map pin, not the code pin in the header. It is not an ancestor of the inspected main reference; future retrieval depends on branch retention or another retained reference to that commit.

Inventory result: 0 families and 0 implementation rows inside the directly contained Python files. Rows inside excluded child packages do not make a family part of this pack. The counted-family grep is `NOT_APPLICABLE`: there is no in-package family to rank or search. This is the empty-inventory acceptance result, not a claim that the repository has no related implementations.

| Map entry outside the family inventory | Disposition and follow-up |
| --- | --- |
| `rejected_candidates`: `vessel_specs.py::validate_rows` at line 142 and `validate_dataset` at line 167, tagged `irregular-frequency-treatment` | Classified as name-pattern false positives: schema, ranges and provenance are checked; neither function computes irregular-frequency treatment. The map still records `needs_review`; O2 will carry the disposition into the map without counting the candidates as implementations. [src/digitalmodel/data_systems/vessel_specs.py:L142-L175] |

Table 9a. Two rejected candidates remain outside implementation-family membership at the map pin.

Map follow-up, issue pending (2): add vessel schema/range/provenance validation, explicit-unit catalogue validation including mass and secant consistency, threshold-exposure counting, and evidence-preserving JSON/CSV reference interchange from section 3 before counting these families in a later map revision.

The additional whole-repository search, including scripts, used:

```sh
rg -n 'nb_hours_below_threshold|def (hours_below_threshold|load_database|validate_rows|_unique_object|_source_float)|class SolverReferenceExporter|CONSISTENCY_REL_TOL' src scripts tests docs --glob '*.py'
```

| Other implementation / helper | Classification and map follow-up |
| --- | --- |
| `_unique_object` in vessel and reference modules; `_unique_pairs` in catalogues | Independent duplicate-key rejection helpers already covered in section 3; generic JSON preservation, not a second exposure algorithm. [src/digitalmodel/data_systems/vessel_specs.py:L19-L25] [src/digitalmodel/data_systems/solver_reference_export.py:L58-L66] [src/digitalmodel/data_systems/engineering_databases.py:L40-L46] |
| `scripts/reporting/capability_report_inventory.py` | Another duplicate-key reader; include in future evidence-reader inventory, issue pending (2). [scripts/reporting/capability_report_inventory.py:L122-L137] |
| `pipelay/evidence.py`, `riser_bom/loader.py`, `asset_integrity/assessment/fe_crack_screen.py` | Other independently coded duplicate-key helpers outside scope; map gaps for a future structural-reader family, issue pending (2). [src/digitalmodel/pipelay/evidence.py:L58-L64] [src/digitalmodel/riser_bom/loader.py:L16-L22] [src/digitalmodel/asset_integrity/assessment/fe_crack_screen.py:L359-L365] |
| `tests/ansys/golden_acceptance.py` | Test-support duplicate-key helper, outside implementation acceptance counts. [tests/ansys/golden_acceptance.py:L31-L37] |
| `geotechnical/database.py`, `materials/database.py`, `bha_database/loader.py` | Distinct domain catalogue loaders, not wrappers of this module's `load_database`; compare contracts before any consolidation, issue pending (2). [src/digitalmodel/geotechnical/database.py:L154-L171] [src/digitalmodel/materials/database.py:L123-L143] [src/digitalmodel/bha_database/loader.py:L72-L90] |

Table 10. Additional search findings do not alter the empty-inventory acceptance result.

## 10. Coverage and open items

### Read coverage

All directly contained Python files were read: `__init__.py`, `vessel_specs.py`, `engineering_databases.py`, `metocean_statistics.py`, `solver_reference_export.py`; child-package boundaries are limited to the initialisers in section 9. [src/digitalmodel/data_systems/__init__.py:L1-L1] [src/digitalmodel/data_systems/vessel_specs.py:L1-L189] [src/digitalmodel/data_systems/engineering_databases.py:L1-L263] [src/digitalmodel/data_systems/metocean_statistics.py:L1-L116] [src/digitalmodel/data_systems/solver_reference_export.py:L1-L270]

The related test inventory is 623 parameter cases: 614 in-package cases (vessel validation 30, catalogue validation 332, metocean statistics 33, reference export 219) and 9 out-of-package script cases for offline retrieval. All 623 pass with precompiled dependencies; the original source-import record passed 590 and left 33 NOT_RUN. The old inventory lead of about 19 tests is not used as evidence. Counts derive from execution records, with modes separated below. [tests/data_systems/test_vessel_specs.py:L1-L205] [tests/data_systems/test_fetch_vessel_specs.py:L1-L157] [tests/data_systems/test_engineering_databases.py:L1-L424] [tests/data_systems/test_metocean_statistics.py:L1-L188] [tests/data_systems/test_solver_reference_export.py:L1-L582] [scripts/fetch_vessel_specs.py:L4-L25]

Package documentation read: `src/digitalmodel/data_systems/README.md`, `data/README.md`, `data_procurement/README.md`, `data_scraping/README.md`; the directly relevant reference contract is `assets/data/vessel_specs_README.md`. Sub-package documentation supplies boundaries, not top-level runtime evidence. [src/digitalmodel/data_systems/README.md:L1-L26] [src/digitalmodel/data_systems/data/README.md:L1-L24] [src/digitalmodel/data_systems/data_procurement/README.md:L1-L24] [src/digitalmodel/data_systems/data_scraping/README.md:L1-L26] [assets/data/vessel_specs_README.md:L123-L137]

The documentation-name search `rg -l 'data_systems|engineering_databases|solver_reference_export|metocean_statistics|vessel_specs' docs --glob '*.md'` found these 19 files; relevant passages were read, and sub-package work remains outside scope, issue pending (3):

- `docs/riser_database.md`, `docs/README.md`, `docs/registry/implementation-map.md`, `docs/maps/digitalmodel-operator-map.md`.
- `docs/plans/gebco-elevation-response-admission.md`, `docs/data/OCIMF_CORPUS_README.md`, `docs/domains/_index.md`.
- `docs/plans/2026-08-05-issue-1970-artifact-generation-commit.md`, `docs/plans/2026-07-09-issue-701-oracle-dependency-isolation.md`, `docs/plans/2026-05-05-issue-584-api-rp-2sm-synthetic-rope-mooring.md`, `docs/plans/engineering-databases-plan-lite.md`.
- `docs/domains/data_systems/data_procurement/README.md`, `docs/domains/data_systems/solver-reference-exports.md`, `docs/domains/data_systems/data_manager/README.md`, `docs/domains/data_systems/data_scraping/README.md`, `docs/domains/metocean/statistics.md`.
- `docs/domains/materials/line-pipe-validation-2026-06-28.md`, `docs/domains/grade-table-migration-2026-06-28.md`, `docs/domains/domains_index.md`.

### Offline execution record

The original source-import test form did not request bytecode compilation:

```sh
PYTHONPATH=src uv run --no-sources --extra metocean python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<external-test-directory>
```

Its collection failure requires source compiled at import, Python >= 3.12 and `filterwarnings = error`; it is not invariant across installation modes. Installation-time `uv run --compile-bytecode` or `UV_COMPILE_BYTECODE=1` prepares bytecode before pytest applies its filters. Explicit compilation of the installed dependencies also permits the same tests to run. Repository addopts and warning filters remain unchanged in both modes.

The current runs use Python 3.12.3, pytest 8.4.2, pytest-asyncio 0.21.1, metocean-stats 1.2.0, virocon 2.4.0 and assetutilities 0.1.0; `PYTHONUTF8` is unset and UTF-8 mode is off. Their test form is:

This pytest form assumes dependency bytecode has already been generated. For a fresh installation, add `--compile-bytecode` to `uv run`; the measured runs used explicit compilation instead of that installation switch.

```sh
PYTHONPATH=src uv run --no-project --python python3.12 \
  --with 'pytest==8.4.2' --with 'pytest-asyncio==0.21.1' \
  --with 'assetutilities>=0.0.7,<1.0.0' \
  --with 'jsonschema[format-nongpl]>=4.20,<5' --with pyyaml \
  --with 'pydantic>=2.7,<3' --with 'metocean-stats==1.2.0' \
  python -m pytest <paths> -q -rA -p no:cacheprovider \
  --basetemp=<external-test-directory>
```

The core selection names the four non-statistics top-level test files, `tests/contracts` and `tests/docs/test_digitalmodel_routing_contract.py`. The statistics selection names `tests/data_systems/test_metocean_statistics.py`. The full selection combines both; no test marker or node is deselected. BASE is `458684eab815650bed2e46b2cd466308f11b9034`; runtime source at BASE matches the header's code pin.

| Selection | Mode / tree | Selected | Passed | Failed nodes | Skipped | NOT_RUN | Collection errors |
| --- | --- | --- | ---: | ---: | ---: | --- | ---: |
| Statistics BASE | Source compiled at import | 0 | 0 | 0 | 0 | 33 static cases | 1 |
| Core BASE | Before edits | 1180 | 1179 | 0 | 1 | 0 | 0 |
| Statistics BASE | Precompiled dependencies, before edits | 33 | 33 | 0 | 0 | 0 | 0 |
| Full after | Precompiled dependencies, documentation edits | 1213 | 1212 | 0 | 1 | 0 | 0 |

Table 11. Collection failure and passing execution are different installation modes; after checks use the uncommitted documentation tree.

The core selection comprises 581 in-package cases, 9 out-of-package retrieval-script cases, 576 contracts-domain cases and 14 routing cases. The contracts metadata test skips in an uninstalled checkout; the current core partition is 1179 PASS / 1 SKIP, rather than the original pack's recorded 1180 PASS / 0 SKIP. Statistics has 33 executed PASS cases with bytecode, rather than only a static case count. The full after selection has 1212 PASS / 1 SKIP; its PASS-node set equals the union of the two passing BASE records. Historical node lists are not shipped; the new PASS-node digests below identify current records and do not authenticate unavailable historical logs.

PASS-node digest convention: sorted unique node IDs, UTF-8, one LF per node, SHA-256. The BASE core and statistics lists together contain 1212 passing nodes.

| Record | Passing nodes | SHA-256 |
| --- | ---: | --- |
| BASE core | 1179 | `773a3f1ea9725bb31fc393cd55d6bf8495c13876967b0db0ebd7813d4e3fe61d` |
| BASE statistics, precompiled | 33 | `f2d1b5047ae4d5ce4ad23891095542e7c34e6cda863a25a86893ca275eb9df9a` |
| BASE union | 1212 | `43d8ce8e2ee9086baf03a31de9687bbe59cd632fddaf6f1f320079fb004e04ed` |
| After full | 1212 | `43d8ce8e2ee9086baf03a31de9687bbe59cd632fddaf6f1f320079fb004e04ed` |

Table 11a. Execution-record digests; full logs remain external and are not reproduced by these digests.

The following bounds resolver is included so C12 can be repeated. Claim-to-range agreement requires source inspection in addition to this check; all section-4 node IDs are also compared with the passing-node records.

```sh
python - <<'PY'
from pathlib import Path
import re
import subprocess

text = Path('src/digitalmodel/data_systems/docs/agent-pack.md').read_text(encoding='utf-8')
pin = re.search(r'\| Code revision \| `([0-9a-f]{40})`', text).group(1)
citations = re.findall(r'\[([^\]\n]+):L(\d+)-L(\d+)\]', text)
for path, first, last in citations:
    source = subprocess.check_output(['git', 'show', f'{pin}:{path}'], encoding='utf-8')
    assert 1 <= int(first) <= int(last) <= len(source.splitlines()), (path, first, last)
print(f'{len(citations)} citations resolve; {len(set(citations))} distinct ranges')
PY
```

### Open-item register

| Item | Coverage / limit and next action | Responsible role / issue |
| --- | --- | --- |
| O1 | Reproducible test-environment preparation: retain dependency versions and compile bytecode before pytest under Python >= 3.12; source-import collection still fails with warnings-as-errors, while all 33 statistics cases pass with precompiled dependencies | Maintainer; issue pending (1). [tests/data_systems/test_metocean_statistics.py:L14-L18] [pytest.ini:L69-L72] |
| O2 | Empty implementation-map inventory; register all four section-3 method groups and the external structural-reader candidates | Maintainer; issue pending (2) |
| O3 | Child-package packs, deputy designation and full discovery coverage are separate work; this pack checks only top-level interfaces and boundary initialisers | Maintainer; issue pending (3) |
| O4 | Preserve or reject nonzero JSON tokens before catalogue / vessel float decoding; no detection check exists in those readers | Maintainer; issue pending (4). [src/digitalmodel/data_systems/engineering_databases.py:L169-L188] [src/digitalmodel/data_systems/vessel_specs.py:L32-L36] |
| O5 | Establish a backend result-shape contract before nested zip; no detection check exists for malformed dimensions | Maintainer; issue pending (5). [src/digitalmodel/data_systems/metocean_statistics.py:L111-L116] |
| O6 | Vessel source readback, unknown fields and reference completeness require separate review; require an available date-time checker, a controlled clock test and an explicit IMO vocabulary edition/clause; no client-derived values are included here | Maintainer; #1773. [src/digitalmodel/data_systems/vessel_specs.py:L69-L80] [assets/data/vessel_specs_README.md:L102-L119] |
| O7 | Production catalogue source population and independent engineering-use qualification remain separate from invented contract fixtures | Maintainer; #1841 and #2671. [src/digitalmodel/data_systems/README.md:L7-L9] [src/digitalmodel/data_systems/engineering_databases.py:L4-L9] |
| O8 | Other solver-export adapters still need warning/state retention; the generic exporter does not repair downstream adapters | Maintainer; #2226. [docs/domains/data_systems/solver-reference-exports.md:L102-L123] |
| O9 | Exposure is not an operational weather window; real-observation validation and broader statistics need an open work item rather than the closed integration issue | Maintainer; issue pending (6). [docs/domains/metocean/statistics.md:L89-L100] |

Table 12. Issue numbers or numbered drafts retain every open item for independent review.
