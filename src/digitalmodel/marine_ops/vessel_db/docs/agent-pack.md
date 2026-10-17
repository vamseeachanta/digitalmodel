# Vessel database knowledge pack

| Header | Value |
| --- | --- |
| Module | `marine_ops/vessel_db` |
| Repository | `digitalmodel` |
| Package path | `src/digitalmodel/marine_ops/vessel_db` |
| Pack revision | 1 |
| Code revision | `6cf35f466e5c1b730951af045d8cfe0065af073f` |
| Analysis family | diffraction data preparation; registry family EC |
| Quality role | diffraction quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The package loads curated JSON records, normalises dimension aliases, builds installation crane curves, estimates gyradii, and converts particulars into mass properties for diffraction preparation. [src/digitalmodel/marine_ops/vessel_db/__init__.py:L1-L6] [src/digitalmodel/marine_ops/vessel_db/loader.py:L27-L45] [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L69-L123]
The containing `marine_ops` domain has beta maturity; this is a domain classification, not a solver validation claim. [docs/registry/module-routing.yaml:L162-L167]
`MassProperties.orcawave_body_block` emits a body mass dictionary; meshes, complete decks and licensed runs belong to downstream consumers. [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L37-L55] [src/digitalmodel/hydrodynamics/diffraction/vessel_deck_builder.py:L304-L353]
No geometry, depth, load or target-version qualification follows from a mass dictionary: its only represented tensor is diagonal about the centre of mass. [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L37-L55]
The worldenergydata adapter reads CSV files; the RAO adapter reads separately configured workbooks rather than shipping a reference library. [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L105-L140] [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L38-L72]

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-16 | Initial knowledge pack with pinned source inventory and qualification limits |
| 1 | 2026-10-17 | Align EC gates, qualify standards attribution, pin the map reference and state the portability detection gap |

## 2. Inputs and outputs

| Input | Unit / allowed range | Default / missing behaviour | Source |
| --- | --- | --- | --- |
| Curated records, scope/layer | JSON; numeric fields accept aliases, without physical range enforcement | Missing record arrays become empty; absent fields get empty collections or strings | [src/digitalmodel/marine_ops/vessel_db/loader.py:L162-L200] |
| LOA/LBP, beam, draft, depth | m by schema; estimates require positive beam/length | LBP falls back to LOA for inertia estimation | [data/vessels/SCHEMA.yaml:L16-L39] [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L82-L84] [src/digitalmodel/marine_ops/vessel_db/gyradii.py:L106-L122] |
| Displacement | tonnes; mass must be positive | `from_record` returns None otherwise | [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L76-L80] |
| LCG/TCG/VCG, gyradii | m; input datum must be checked | CoG components default to zero; VCG enters gaps | [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L28-L32] [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L100-L113] |
| record collection CSVs | dimensional aliases follow the curated schema; text units are not converted | Environment precedence then local discovery; missing datasets return empty lists | [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L58-L102] [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L110-L158] [src/digitalmodel/marine_ops/vessel_db/loader.py:L75-L106] |
| RAO heading-block workbook | period s; six amplitude and six phase columns | First recognised sheet; unrecognised layouts return None | [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L52-L72] [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L92-L152] |

| Output | Unit / convention | Default / limitation | Source |
| --- | --- | --- | --- |
| `Record.canonical_dimensions()` | schema m, tonnes, dimensionless block coefficient | Only numeric markers survive; first matching alias wins | [src/digitalmodel/marine_ops/vessel_db/loader.py:L143-L161] |
| `MassProperties` | mass te; CoG m; inertia te·m²; x forward, y port, z up | Datum must be supplied; principal axes assumed | [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L25-L55] |
| `orcawave_body_block()` | mass and CoG rounded to three decimals; tensor to one decimal | No provenance or gap fields in emitted dictionary | [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L37-L55] |
| Crane curves | radius m; capacity te | No reach yields a flat headline point | [src/digitalmodel/marine_ops/vessel_db/loader.py:L333-L357] [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L187-L205] |
| `RAOData` | omega rad/s; sorted heading values; array shape frequency × heading × six DOFs | Longest period block is the reference grid; numeric phases copied/interpolated | [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L127-L152] |
| Processed tables / provenance report | particulars, crane summary, RAO index, metocean CSV plus Markdown | Empty table writes are suppressed; metadata is summarised separately | [src/digitalmodel/marine_ops/vessel_db/promote.py:L167-L205] |
| Confidence / capability / audit | confidence dimensionless; capability score points; audit counts records/fields | Confidence scoring and conflict comparison are code heuristics | [src/digitalmodel/marine_ops/vessel_db/confidence.py:L111-L125] [src/digitalmodel/marine_ops/vessel_db/confidence.py:L141-L181] [src/digitalmodel/marine_ops/vessel_db/audit.py:L33-L39] |

The mass record defaults describe amidships/centreline and a keel VCG placeholder; the downstream deck builder shifts x to the aft-perpendicular mesh origin and z to the waterline. [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L69-L113] [src/digitalmodel/hydrodynamics/diffraction/vessel_deck_builder.py:L201-L214]
The RAO adapter does not convert or validate phase units, rotational amplitude units, heading sense or phase sign; those conventions need an explicit source contract before reuse (issue pending (3)). [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L102-L152]

## 3. Method and governing standards

| Method / implementation | Basis and limits | Evidence |
| --- | --- | --- |
| `parse_value`, `Record.dimension`, `validate_provenance` | Whole-string numeric recognition, comma removal and suffix allowlist; citation coverage uses quantity-stem inclusion, not source verification | [src/digitalmodel/marine_ops/vessel_db/loader.py:L75-L106] [src/digitalmodel/marine_ops/vessel_db/loader.py:L143-L161] [src/digitalmodel/marine_ops/vessel_db/loader.py:L231-L285] |
| `estimate_gyradii` | Empirical ship-form relations defined in source; non-ship forms receive a caveat; missing dimensions omit axes | [src/digitalmodel/marine_ops/vessel_db/gyradii.py:L1-L24] [src/digitalmodel/marine_ops/vessel_db/gyradii.py:L60-L122] |
| `from_record` | Diagonal rigid-body identity `inertia_axis = mass * radius_axis²`; numeric input is labelled cited without checking its citations | [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L58-L98] |
| `parse_heading_block_rao` | Header recognition, per-heading arrays, longest-block period grid, angular-frequency conversion and component-wise linear interpolation | [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L79-L152] |
| `capability_score`, `record_confidence` | Capped margin × tier score, weighted mean in points; weakest field governs confidence; missing fields do not enter record mean | [src/digitalmodel/marine_ops/vessel_db/confidence.py:L111-L125] [src/digitalmodel/marine_ops/vessel_db/confidence.py:L141-L181] |
| `audit_cross_source_conflicts` | Normalised-name intersection; exact identifier comparison and relative numeric differences; missing values cannot conflict | [src/digitalmodel/marine_ops/vessel_db/audit.py:L86-L128] |
| `promote`, `_write_csv` | Record-table serialisation and provenance summary, not another RAO numerical parser | [src/digitalmodel/marine_ops/vessel_db/promote.py:L130-L152] [src/digitalmodel/marine_ops/vessel_db/promote.py:L167-L205] |

UNVERIFIED: DNV-RP-H103 edition, clause, table and publication title supporting the gyradii attribution are absent from the cited source; clause text not checked against the licensed copy (issue pending (1)). [src/digitalmodel/marine_ops/vessel_db/gyradii.py:L10-L14]
UNVERIFIED: The installation demonstration attributes static and dynamic crane criteria to DNV-RP-H103 without a verified publication title, edition or clause title; the attribution requires qualification (issue pending (1)); no tabulated limit is reproduced here. [examples/demos/installation/go_no_go_crane_curves.py:L82-L91]
UNVERIFIED: OCIMF publication, edition and clause/table are unspecified: package references describe adapter precedence, while the schema calls its coefficient source an annex reference (issue pending (1)). [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L3-L10] [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L11-L21] [data/vessels/SCHEMA.yaml:L68-L72]
No inconsistent edition pair can be resolved from these unspecified editions; standards attribution remains an open item rather than an inferred standard check. [src/digitalmodel/marine_ops/vessel_db/gyradii.py:L10-L14] [examples/demos/installation/go_no_go_crane_curves.py:L82-L91]
The estimate carries a basis string, but the estimate and mass-property return types have no calculation `Citation` sidecar field (issue pending (1)). [src/digitalmodel/marine_ops/vessel_db/gyradii.py:L49-L59] [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L25-L35]

## 4. Validated worked examples

Classification: 0 independently validated examples; the cases below are regression examples with repository-defined expectations, not independent hydrodynamic validation. [tests/marine_ops/vessel_db/test_mass_properties.py:L20-L72] [tests/marine_ops/vessel_db/test_hulls_adapter.py:L23-L56]

| Case | Source and inputs | Expected result / tolerance | Existing test node ID | Offline result |
| --- | --- | --- | --- | --- |
| R1 rigid-body inertia | Synthetic record in test; expected diagonal identity, not an external benchmark | Three axis results match `mass * radius²`; pytest default approximation, relative 1e-6 or absolute 1e-12 te·m² | `tests/marine_ops/vessel_db/test_mass_properties.py::test_inertia_is_mass_times_k_squared` | PASS at BASE and AFTER [tests/marine_ops/vessel_db/test_mass_properties.py:L20-L29] |
| R2 heading-block parse | Synthetic two-heading fixture in test | Array shape (3, 2, 6); longest-period heave 1.0 and sway 0.0, absolute 1e-6 dimensionless | `tests/marine_ops/vessel_db/test_hulls_adapter.py::test_parse_synthetic_heading_block` | PASS at BASE and AFTER [tests/marine_ops/vessel_db/test_hulls_adapter.py:L23-L48] |
| R3 angular-frequency conversion | Synthetic periods from the same fixture | Minimum omega equals `2*pi/20` rad/s, relative 1e-6 | `tests/marine_ops/vessel_db/test_hulls_adapter.py::test_frequencies_are_rad_per_s` | PASS at BASE and AFTER [tests/marine_ops/vessel_db/test_hulls_adapter.py:L49-L52] |
| R4 diffraction setup demonstration | Existing curated-record workflow; no individual values reproduced | At least 1 setup and every emitted mass greater than 0 te; smoke criterion only | `tests/marine_ops/vessel_db/test_mass_properties.py::test_demo_generates_setups_from_real_db` | PASS at BASE and AFTER [tests/marine_ops/vessel_db/test_mass_properties.py:L78-L96] |
| R5 isolated promotion | Test copies raw JSON to an isolated temporary directory | 5 named output files present; every output category has more than 0 rows | `tests/marine_ops/vessel_db/test_promote.py::test_promote_writes_all_outputs` | PASS at BASE and AFTER [tests/marine_ops/vessel_db/test_promote.py:L14-L37] |
| R6 capability demonstration | Existing record collection smoke workflow; no individual record copied | 0 ordering disagreements; at least 1 defensible and 1 non-defensible row | `tests/marine_ops/vessel_db/test_capability_demo.py::test_score_fleet_ranks_and_flags` | PASS at BASE and AFTER [tests/marine_ops/vessel_db/test_capability_demo.py:L28-L42] |
| R7 crane screening demonstration | Existing record collection smoke workflow; no independent standard check claimed | 0 decisions outside the three permitted states; every capacity greater than 0 te | `tests/marine_ops/vessel_db/test_go_no_go_demo.py::test_decisions_are_valid_states` | PASS at BASE and AFTER [tests/marine_ops/vessel_db/test_go_no_go_demo.py:L64-L67] |

## 5. QA gates

Gate applicability uses the EC Python-calculations row: G0, G1 imports/tests, G2 hand or published check, and G5 apply; G3 and G4 do not apply to this data-preparation package. Downstream diffraction qualification remains separate. (AQG r0, revision 0, Gate summary; Family mapping.)

| Gate | Applicability and route | Module-specific checks / evidence |
| --- | --- | --- |
| G0 | Applicable to curated data and mass inputs | C01–C04 and C17–C19 check parsing, provenance, discovery and missing inputs [src/digitalmodel/marine_ops/vessel_db/loader.py:L75-L106] [src/digitalmodel/marine_ops/vessel_db/loader.py:L255-L285] |
| G1 | Applicable through module imports and solver-free tests | C05 and C08–C10 check export shape, coordinate defaults and RAO ingestion; section 10 records the executable module test path [tests/marine_ops/vessel_db/test_mass_properties.py:L42-L96] [tests/marine_ops/vessel_db/test_hulls_adapter.py:L23-L59] |
| G2 | Applicable through the closed-form rigid-body check | C07 checks all three diagonal inertias against independently expressed `mass * radius²` expectations; regression identity only, not a hydrodynamic benchmark [tests/marine_ops/vessel_db/test_mass_properties.py:L20-L29] |
| G3 | NOT_APPLICABLE to the EC Python-calculations row | No time-domain solver or short-dynamics run is implemented in this package [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L37-L55] |
| G4 | NOT_APPLICABLE to the EC Python-calculations row | No mesh or diffraction solve is implemented here; C06 and C11 retain downstream qualification requirements without claiming an EC gate result [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L37-L55] |
| G5 | Applicable to every result used in a report | C12–C15 provide table/basis, confidence and audit evidence; C16 requires a complete signed fingerprint-linked record, NOT_RUN because this package emits no such record [src/digitalmodel/marine_ops/vessel_db/promote.py:L179-L237] [src/digitalmodel/marine_ops/vessel_db/audit.py:L153-L200] |

The checks below are execution recipes using existing tests unless explicitly identified as downstream qualification; counts and physical units are stated for each criterion. [tests/marine_ops/vessel_db/test_loader.py:L20-L101] [tests/marine_ops/vessel_db/test_mass_properties.py:L20-L96]

| ID | Gate | Function / output checked | Numeric pass criterion and unit | Executable evidence |
| --- | --- | --- | --- | --- |
| C01 | G0 | `parse_value` marker and numeric parsing | 0 marker mismatches across 13 input cases, count; numeric values within pytest default approximation | `tests/marine_ops/vessel_db/test_loader.py::test_parse_value_strict` [tests/marine_ops/vessel_db/test_loader.py:L20-L45] |
| C02 | G0 | `validate_provenance()` | 0 uncited numeric fields, count; coverage heuristic only | `tests/marine_ops/vessel_db/test_loader.py::test_provenance_is_clean` [tests/marine_ops/vessel_db/test_loader.py:L70-L76] |
| C03 | G0 | `datasets`, `iter_records` | More than 0 records per discovered dataset; 0 empty/placeholder names, count | `tests/marine_ops/vessel_db/test_loader.py::test_every_dataset_loads` [tests/marine_ops/vessel_db/test_loader.py:L79-L84] |
| C04 | G0 | `resolve_wed_curated_dir` | 0 precedence mismatches across fixture layouts; missing configuration emits at least 1 warning, count | `tests/marine_ops/vessel_db/test_wed_adapter.py::test_resolution_order_env_then_legacy_then_package`; `tests/marine_ops/vessel_db/test_wed_adapter.py::test_missing_curated_dir_warns_but_returns_none` [tests/marine_ops/vessel_db/test_wed_adapter.py:L62-L110] |
| C05 | G1 | `orcawave_body_block` | 0 missing required keys, count; tested off-diagonal entries exactly 0.0 te·m² | `tests/marine_ops/vessel_db/test_mass_properties.py::test_orcawave_body_block_shape` [tests/marine_ops/vessel_db/test_mass_properties.py:L64-L75] |
| C06 | Downstream G1 | Complete downstream model containing exported body | Target version load errors 0 and load warnings 0, count; record version and mesh fingerprint | NOT_RUN: licensed program required; future execution through the version-matched OrcaWave licensed load-and-validate route for the assembled deck (AQG r0, revision 0, G1 load; Family mapping.) |
| C07 | G2 | `from_record` diagonal inertia | 3 axis equalities; relative tolerance 1e-6 or absolute 1e-12 te·m² | R1 node, section 4 [tests/marine_ops/vessel_db/test_mass_properties.py:L20-L29] |
| C08 | G1 | `from_record` missing mass, CoG defaults and explicit CoG | 0 accepted missing/zero masses, count; 0.0 m error for default coordinates; explicit coordinates match pytest approximation | `tests/marine_ops/vessel_db/test_mass_properties.py::test_no_mass_returns_none`; `tests/marine_ops/vessel_db/test_mass_properties.py::test_cog_defaults_flagged_when_missing`; `tests/marine_ops/vessel_db/test_mass_properties.py::test_cog_uses_cited_vcg` [tests/marine_ops/vessel_db/test_mass_properties.py:L42-L61] |
| C09 | G1 | `parse_heading_block_rao` synthetic amplitudes / layout | 1e-6 absolute dimensionless amplitude error; 0 accepted unrecognised layouts, count | R2 node; `tests/marine_ops/vessel_db/test_hulls_adapter.py::test_unrecognised_layout_returns_none` [tests/marine_ops/vessel_db/test_hulls_adapter.py:L38-L59] |
| C10 | G1 | RAO angular-frequency conversion | Relative error at most 1e-6, dimensionless; output rad/s | R3 node [tests/marine_ops/vessel_db/test_hulls_adapter.py:L49-L52] |
| C11 | Downstream G4 | Downstream body/mesh physical qualification | Weight/buoyancy error at most 1.0 %, independent volume difference at most 0.5 %, waterline error at most 1 mm; full centres, normals, panel-size and irregular-frequency evidence required | NOT_RUN: no qualified body/mesh dataset or licensed hydrostatics; mesh-test dependency build also failed (AQG r0, revision 0, G4 diffraction QA.) |
| C12 | G5 | `promote` outputs and basis strings | 5 expected files, 0 provenance violations and 0 populated gyradii without a basis, count | R5 node; `tests/marine_ops/vessel_db/test_promote.py::test_promote_provenance_clean`; `tests/marine_ops/vessel_db/test_promote.py::test_no_bare_gyradius_without_basis` [tests/marine_ops/vessel_db/test_promote.py:L27-L59] |
| C13 | G5 | `capability_score`, confidence gaps | 0 defensible outputs for the gap/shortfall fixture, count; gap score exactly 0.0 dimensionless | `tests/marine_ops/vessel_db/test_confidence.py::test_capability_score_not_defensible_on_gap_or_shortfall`; `tests/marine_ops/vessel_db/test_confidence.py::test_tier_score_order` [tests/marine_ops/vessel_db/test_confidence.py:L23-L29] [tests/marine_ops/vessel_db/test_confidence.py:L98-L106] |
| C14 | G5 | `_conflict` missing values / tolerance | 0 conflicts with an absent side; 1 conflict in the beyond-tolerance fixture, count | `tests/marine_ops/vessel_db/test_audit.py::test_conflict_numeric_tolerance`; `tests/marine_ops/vessel_db/test_audit.py::test_conflict_ignores_missing_values` [tests/marine_ops/vessel_db/test_audit.py:L14-L29] |
| C15 | G5 | `run_audit` / Markdown report | 0 uncited fields, count; all 4 report heading strings present, count | `tests/marine_ops/vessel_db/test_audit.py::test_curated_has_no_uncited_numbers`; `tests/marine_ops/vessel_db/test_audit.py::test_report_renders_sections` [tests/marine_ops/vessel_db/test_audit.py:L38-L49] |
| C16 | G5 | Downstream QA appendix | 0 missing applicable gate records, 0 fingerprint mismatches, 0 missing signatures, count | NOT_RUN: no generated AQG evidence record/appendix in this package; maintainers must attach one before qualification (AQG r0, revision 0, G5 QA appendix.) |
| C17 | G0 | `estimate_gyradii` missing/nonpositive inputs | 0 axes for unusable dimensions; 2 axes when only length is supplied, count | `tests/marine_ops/vessel_db/test_gyradii.py::test_missing_inputs_omit_axes`; `tests/marine_ops/vessel_db/test_gyradii.py::test_zero_or_negative_dimensions_ignored` [tests/marine_ops/vessel_db/test_gyradii.py:L29-L36] |
| C18 | G0 | record collection consumers when resolution misses | 0 construction rows, 0 rig rows, 0 crane rows, count | `tests/marine_ops/vessel_db/test_wed_adapter.py::test_adapter_degrades_without_wed` [tests/marine_ops/vessel_db/test_wed_adapter.py:L128-L136] |
| C19 | G0 | Unset RAO resolver and datasets | 0 datasets, count; exactly 1 expected required-resolution exception, count | `tests/marine_ops/vessel_db/test_hulls_adapter.py::test_resolver_none_when_unset`; `tests/marine_ops/vessel_db/test_hulls_adapter.py::test_datasets_empty_when_unset` [tests/marine_ops/vessel_db/test_hulls_adapter.py:L60-L69] |

## 6. Known failure modes and pitfalls

The sweep covers all package Python files, including error suppression, skipped records, empty returns, numeric conversions, unit assumptions and fallbacks; rows below include documented sentinel behaviour, not only defects. [src/digitalmodel/marine_ops/vessel_db/loader.py:L75-L106] [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L92-L182]

Commands used, followed by full-file inspection and AST definition assignment:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?|1970' src/digitalmodel/marine_ops/vessel_db --glob '*.py'
rg -n 'int\(|float\(|zeros|interp|default|unit|\.get\(|or 1\.0|0\.05|0\.10|write|cache' src/digitalmodel/marine_ops/vessel_db --glob '*.py'
```

sweep: 101 found

| Site | Symptom / classification | Detection and follow-up |
| --- | --- | --- |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L85-L86] | Boolean input becomes text, not a number | C01; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L87-L88] | Numeric input is converted without finite-value validation | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L89-L90] | Nonstrings become text/None | C01; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L92-L93] | Gap sentinel returns no number | C01; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L94-L95] | Estimated strings carry no numeric value | C01; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L98-L103] | Comma removal and suffix stripping assume compatible units; no scaling or signed-string support | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L104-L106] | Conversion errors and unrecognised text become None/text | C01 covers text, not conversion exceptions; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L143-L150] | First dimension alias wins; missing aliases return None | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L171-L191] | Absent record list/fields/citations/gaps silently use empty defaults | C03 detects empty datasets, not partial fields; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L241-L250] | Empty citation stems are skipped; decoration-only quantity names pass | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L251-L253] | Either subset stem is accepted as citation coverage | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L273-L274] | Estimated/gap/text numeric candidates bypass provenance checks | C02 records current coverage only; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L275-L276] | Count/person and named fields bypass provenance checks | C02 records current exemptions only; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L277-L278] | Covered fields skip violation collection | C02 records current coverage only; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L308-L308] | No capacity starts as zero; missing-data sentinel later drives omission | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L318-L320] | Radius text is converted to float, interpreted as metres without conversion | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L326-L329] | Capacity/radius strings are converted with comma removal and tonne/metre assumptions | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L347-L348] | Nonpositive or absent capacity omits the record | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L349-L351] | No radius becomes a flat headline capacity at zero radius | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L353-L356] | Float arrays and tonne capacities are assembled without source-unit conversion | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L394-L395] | Curated records without a curve are skipped | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L398-L404] | Missing or nonnumeric optional deck/tandem fields become None | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L428-L429] | Any record collection adapter exception is swallowed; curated-only output appears successful | no detection check exists; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/gyradii.py:L33-L35] | Empirical coefficient defaults lack an edition/clause-qualified reference | no detection check exists; issue pending (1) |
| [src/digitalmodel/marine_ops/vessel_db/gyradii.py:L63-L64] | Absent hull type defaults to ship-form applicability | no detection check exists; issue pending (1) |
| [src/digitalmodel/marine_ops/vessel_db/gyradii.py:L95-L104] | Non-ship caveat is attached but calculation proceeds | no detection check exists; issue pending (4) |
| [src/digitalmodel/marine_ops/vessel_db/gyradii.py:L105-L110] | Absent/nonpositive beam omits roll estimate | C17; issue pending (4) |
| [src/digitalmodel/marine_ops/vessel_db/gyradii.py:L111-L122] | Absent/nonpositive length omits pitch/yaw; no dimensions leaves empty estimates | C17; issue pending (4) |
| [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L60-L67] | Numeric radius is called cited without provenance validation; otherwise estimate or None/gap | no detection check exists; issue pending (4) |
| [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L78-L80] | Absent/nonpositive displacement returns None | C08; issue pending (4) |
| [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L82-L84] | LBP falls back to LOA without a separate source-field marker | no detection check exists; issue pending (4) |
| [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L93-L95] | Missing radius skips the inertia axis; partial object still returned | no detection check exists; issue pending (4) |
| [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L104-L107] | Missing LCG uses zero amidships | C08; issue pending (4) |
| [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L105-L108] | Missing TCG uses zero centreline | C08 records defaults; no separate TCG assertion; issue pending (4) |
| [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L106-L113] | Missing VCG uses zero keel coordinate and a gap flag | C08; issue pending (4) |
| [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L37-L55] | Export assumes principal axes, rounds values, and omits gaps/provenance | C05 records tensor shape only; issue pending (4) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L38-L49] | Unset or invalid library directory returns None unless required | C19; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L56-L63] | No recognised XLS sheet returns empty rows | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L64-L71] | No recognised XLSX sheet returns empty rows | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L72-L72] | Unsupported suffix returns empty rows | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L83-L84] | Short header candidates are skipped | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L96-L98] | No recognised headers returns None | C09; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L102-L102] | Numeric heading converted to float without heading-sense validation | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L107-L108] | Empty/nonnumeric-period rows are silently skipped | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L109-L110] | Rows shorter than thirteen columns are skipped | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L113-L114] | Nonnumeric amplitude rows are skipped | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L115-L115] | Numeric periods converted to float without positivity/finite checks | C10 covers positive fixture only; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L116-L116] | Amplitude conversion assumes workbook units without a dimensional contract | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L117-L117] | Nonnumeric phase becomes zero; phase units/sign not validated | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L118-L123] | Empty heading blocks disappear; repeated headings overwrite earlier blocks | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L124-L125] | No accepted blocks returns None | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L129-L133] | Longest block supplies reference grid; period-to-omega assumes seconds | C10 covers conversion, not invalid periods; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L136-L137] | Zero-filled amplitude/phase allocation; every accepted DOF is overwritten before return | C09 covers normal shape; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L143-L143] | Amplitude interpolation extends endpoint values outside shorter period coverage | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L144-L144] | Phase interpolation uses raw numbers without angular unwrapping | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L158-L159] | Unrecognised workbook rows become None | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L163-L165] | Parser exceptions are logged and replaced by None | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L175-L176] | Absent library returns an empty dataset mapping | C19; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L180-L182] | Failed and single-frequency workbooks are omitted; equal dataset keys overwrite | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L187-L191] | Unset library summary reports zero; configured but all-rejected library also counts zero | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L44-L56] | Fixed local discovery fallback remains environment-dependent | no detection check exists for fallback removal; #1002; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L95-L102] | Missing record collection resolution warns then returns None | C04; #1297; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L111-L132] | Missing source metadata is blanket-cited; empty CSV cells are omitted | no detection check exists; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L137-L140] | Absent construction dataset returns empty list | C18; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L148-L150] | Absent drilling dataset returns empty list | C18; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L153-L154] | Missing/unrecognised offshore flag excludes rig | no detection check exists; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L155-L156] | Dimensioned filter checks presence, not parseability | no detection check exists; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L176-L181] | Missing/nonnumeric crane metadata returns None | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L183-L185] | Missing/nonpositive main crane capacity omits vessel | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L191-L196] | Missing reaches produce a flat headline point; auxiliary/main anchors are combined | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L198-L205] | Capacity/reach arrays assume tonnes/metres; hook load converts to float | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L220-L228] | Unavailable record collection summary reports zero counts; missing rig file also zero | no detection check exists; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/confidence.py:L49-L56] | Confidence tier weights are heuristic constants, not calibrated probabilities | C13 tests tier order only; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/confidence.py:L72-L99] | Text markers can reach source-confidence logic; unmatched citation access defaults to cited | no detection check exists; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/confidence.py:L114-L114] | Empty requested fields silently selects default fields | no detection check exists; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/confidence.py:L118-L119] | Missing fields are excluded from record confidence mean | no detection check exists; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/confidence.py:L121-L125] | No fields produces zero/GAP; tied dominant tiers use set iteration | no detection check exists; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/confidence.py:L157-L158] | No margins produces zero score/GAP without an exception | no detection check exists; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/confidence.py:L159-L160] | Missing weights use unity; zero total weight becomes unity | no detection check exists; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/confidence.py:L166-L181] | Missing confidence becomes GAP; negative/nonfinite margins/weights are not rejected | C13 tests gap/shortfall only; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/audit.py:L33-L39] | Relative numeric conflict thresholds are fixed heuristics; identifier comparison exact | C14 covers one tolerance fixture; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/audit.py:L77-L83] | Any adapter exception becomes an empty index; audit cannot distinguish unavailable store | no detection check exists; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/audit.py:L87-L88] | Missing comparison value returns no conflict | C14 explicitly records this behaviour; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/audit.py:L94-L94] | Near-zero denominator is clamped to a small positive value | no detection check exists; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/audit.py:L120-L128] | Missing identifiers become None; only selected fields searched | no detection check exists; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/audit.py:L140-L141] | Absent canonical fields are not counted as confidence gaps | no detection check exists; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/audit.py:L176-L178] | Only first twenty uncited examples are printed; total count retained | C15 does not assert truncation; issue pending (5) |
| [src/digitalmodel/marine_ops/vessel_db/promote.py:L46-L53] | Numeric radii labelled cited; estimate fallback else None/gap | no detection check exists; issue pending (4) |
| [src/digitalmodel/marine_ops/vessel_db/promote.py:L62-L63] | Non-particulars datasets are intentionally excluded from particulars table | C12 covers nonempty outputs only; issue pending (6) |
| [src/digitalmodel/marine_ops/vessel_db/promote.py:L65-L101] | LOA fallback; estimate basis retained but non-ship caveat discarded | no detection check exists; issue pending (6) |
| [src/digitalmodel/marine_ops/vessel_db/promote.py:L108-L114] | Missing/nonnumeric crane summary quantities become None | no detection check exists; issue pending (6) |
| [src/digitalmodel/marine_ops/vessel_db/promote.py:L119-L119] | Zero maximum crane capacity serialises as missing | no detection check exists; issue pending (6) |
| [src/digitalmodel/marine_ops/vessel_db/promote.py:L133-L134] | Non-RAO datasets are intentionally excluded from RAO index | C12 covers nonempty outputs only; issue pending (6) |
| [src/digitalmodel/marine_ops/vessel_db/promote.py:L138-L149] | First citation URL selected; missing RAO metadata becomes empty strings | no detection check exists; issue pending (6) |
| [src/digitalmodel/marine_ops/vessel_db/promote.py:L167-L169] | Empty rows suppress file write; old output can survive | no detection check exists; issue pending (6) |
| [src/digitalmodel/marine_ops/vessel_db/promote.py:L173-L177] | Unlisted row columns silently discarded by CSV writer | no detection check exists; issue pending (6) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L152-L159] | Nonnumeric dimensions disappear from canonical output | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L191-L198] | Missing raw glob matches produce an empty dataset inventory | C03 detects nonempty discovered data only; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/loader.py:L309-L320] | Descriptive primary capacity strings fail the numeric gate before embedded radius extraction | no detection check exists; issue pending (2) |
| [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L79-L88] | Rounded numeric DOF header labels can accept nonintegral labels | no detection check exists; issue pending (3) |
| [src/digitalmodel/marine_ops/vessel_db/audit.py:L61-L63] | Nonnumeric dimension becomes None and is excluded from comparison | C14 records missing-value exclusion only; issue pending (5) |

All exception handlers, skip-record branches and empty error/sentinel returns are included above; no year-epoch fallback or empty-result cache writer was found in the package inspection. The zero-valued dataclass counts and accumulators initialise bookkeeping rather than replacing failed engineering quantities. [src/digitalmodel/marine_ops/vessel_db/audit.py:L51-L58] [src/digitalmodel/marine_ops/vessel_db/confidence.py:L103-L125] [src/digitalmodel/marine_ops/vessel_db/confidence.py:L157-L181]
Issue #1297 describes absent record collection discovery; the current resolver includes both layouts and warning tests C04, while exception swallowing remains separate. [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L35-L102] [tests/marine_ops/vessel_db/test_wed_adapter.py:L62-L110]
Issue #1002 concerns a fixed local discovery fallback; the source still has that fallback with an explicit allowlist comment, so this pack does not claim the underlying portability concern is removed; no detection check exists for removal of the fixed fallback (issue pending (5)). [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L44-L56]
The map attaches #2107 as not checked to these package rows; this parser handles workbook rows, not fixed-byte LIS windows, so no package occurrence of that exact truncation mechanism was identified. [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L52-L72] [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L92-L152] [docs/registry/implementation-map.yaml:L12478-L12515]

## 7. Lessons log

| Date | Change reference | Lesson | Evidence | Pack change / shared record |
| --- | --- | --- | --- | --- |
| 2026-10-16 | Initial pack | Keep smoke/regression outputs separate from independent diffraction qualification | [tests/marine_ops/vessel_db/test_mass_properties.py:L90-L96] [tests/marine_ops/vessel_db/test_hulls_adapter.py:L38-L54] | Sections 4–6; no shared record assigned |

## 8. Ownership

Review roles: diffraction family quality manager; deputy: diffraction deputy quality manager; producing discipline: marine data preparation. Escalation for reference, tolerance or datum ambiguity: family quality manager, then the maintainer. These are review assignments for this draft, not a completed approval.

## 9. Dependencies and implementation inventory

### Runtime and solver dependencies

| Dependency / mention | Role and boundary | Evidence |
| --- | --- | --- |
| Curated JSON / schema | Upstream raw records and engineering units | [src/digitalmodel/marine_ops/vessel_db/loader.py:L162-L191] [data/vessels/SCHEMA.yaml:L16-L39] |
| worldenergydata vessel_fleet | Upstream CSV collection; absent checkout is optional | [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L58-L107] |
| NumPy, openpyxl, xlrd, `RAOData` | Array interpolation and workbook reading; no numerical solver launch in the adapter | [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L28-L32] [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L52-L72] [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L92-L152] |
| Installation `CraneCurve` | Downstream crane interpolation object | [src/digitalmodel/marine_ops/vessel_db/loader.py:L341-L357] [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L168-L205] |
| OrcaWave / AQWA mention 1 | Re-export comment names the intended diffraction consumer, not a version-qualified interface | [src/digitalmodel/marine_ops/vessel_db/__init__.py:L98-L101] |
| OrcaWave / AQWA mention 2 | Mass-property module docstring describes intended consumers | [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L2-L10] |
| OrcaWave mention 3 | `orcawave_body_block` docstring and emitted dictionary | [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L37-L55] |
| Downstream diffraction builder | Full native OrcaWave deck and mesh assembly; separate package | [src/digitalmodel/hydrodynamics/diffraction/vessel_deck_builder.py:L324-L374] |

These are all case-sensitive OrcaWave/AQWA source mentions in the package; no solver version is pinned in those mentions. [src/digitalmodel/marine_ops/vessel_db/__init__.py:L98-L101] [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L2-L10] [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L37-L55]

### Counted map family and exact search

Map artifact commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, declared data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; the map artifact is byte-identical at the code pin. [docs/registry/implementation-map.yaml:L1-L3]
One family has package rows: `rao-readers-and-orcaflex-converters`, with 3 in-package rows and 295 total rows; it is the only counted family. [docs/registry/implementation-map.md:L20-L20] [docs/registry/implementation-map.yaml:L12478-L12515]
The in-package mapped rows are `hulls_adapter.read_rao_workbook`, `hulls_adapter.real_rao_datasets` and `promote.promote`; the inventory below includes all outside-package entries as well. [docs/registry/implementation-map.yaml:L12478-L12515]

Search recipe: load that map YAML, select families having a path beginning with the header package, rank by descending in-package row count with original family order for ties, and take the first three; search every package `.py` file at the code pin using Python `re.finditer(signal['pattern'], source)` without flags or escape rewriting. Drop only signals for which `re.search(signal['pattern'], package_suffix, re.I)` matches; assign each hit to the narrowest enclosing AST function/class, otherwise the module definition. No signal was dropped for this package. [docs/registry/implementation-map.yaml:L9511-L9517]

| Hit definition | Classification and inventory disposition | Code evidence |
| --- | --- | --- |
| `__init__.py` / `<module>` | Module-level imports, exports, comments or docstrings; no further family algorithm | [src/digitalmodel/marine_ops/vessel_db/__init__.py:L1-L101] |
| `hulls_adapter.py` / `read_rao_workbook` | Mapped wrapper; delegates to the section-3 parser | [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L155-L165] |
| `hulls_adapter.py` / `real_rao_datasets` | Mapped collection wrapper | [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L168-L182] |
| `hulls_adapter.py` / `library_summary` | Count helper | [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L185-L191] |
| `hulls_adapter.py` / `<module>` | Module-level imports, exports, comments or docstrings; no further family algorithm | [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L1-L196] |
| `hulls_adapter.py` / `resolve_rao_library_dir` | Path resolver helper | [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L38-L49] |
| `hulls_adapter.py` / `parse_heading_block_rao` | Numerical parser; explicitly listed in section 3; map addition pending (8), not an unlisted second implementation | [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L92-L152] |
| `mass_properties.py` / `<module>` | Module-level imports, exports, comments or docstrings; no further family algorithm | [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L1-L123] |
| `promote.py` / `<module>` | Module-level imports, exports, comments or docstrings; no further family algorithm | [src/digitalmodel/marine_ops/vessel_db/promote.py:L1-L247] |
| `promote.py` / `promote` | Mapped CSV/index serialiser; not a numerical RAO computation | [src/digitalmodel/marine_ops/vessel_db/promote.py:L179-L205] |
| `promote.py` / `build_rao_index` | Index metadata serialiser, not a numerical RAO computation | [src/digitalmodel/marine_ops/vessel_db/promote.py:L130-L152] |
| `promote.py` / `_write_provenance_report` | Report serialiser | [src/digitalmodel/marine_ops/vessel_db/promote.py:L208-L237] |
| `wed_adapter.py` / `_read_csv` | CSV helper, not a numerical RAO computation | [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L105-L107] |
| `wed_adapter.py` / `construction_vessels` | Record adapter, not a numerical RAO computation | [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L134-L140] |
| `wed_adapter.py` / `drilling_rigs` | Record adapter, not a numerical RAO computation | [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L143-L158] |
| `wed_adapter.py` / `fleet_summary` | Count helper | [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L218-L228] |

Further second implementations absent from both sections 3 and 9: 0; the counted-family search therefore meets the at-most-one criterion. The numerical parser is already inventoried in section 3; adding it to the map remains a follow-up, not an omitted algorithm. [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L92-L165]

### Full family inventory

Each row below identifies one implementation by its one-based order within the family and the exact map record containing its source path and qualified symbol; YAML aliases retain their map meaning. This locator form retains every entry without copying identifying discovery paths. [docs/registry/implementation-map.md:L47-L47]

| Family row | Path / symbol record in pinned map |
| --- | --- |
| 1 | [docs/registry/implementation-map.yaml:L9516-L9527] |
| 2 | [docs/registry/implementation-map.yaml:L9528-L9539] |
| 3 | [docs/registry/implementation-map.yaml:L9540-L9551] |
| 4 | [docs/registry/implementation-map.yaml:L9552-L9563] |
| 5 | [docs/registry/implementation-map.yaml:L9564-L9578] |
| 6 | [docs/registry/implementation-map.yaml:L9579-L9589] |
| 7 | [docs/registry/implementation-map.yaml:L9590-L9601] |
| 8 | [docs/registry/implementation-map.yaml:L9602-L9613] |
| 9 | [docs/registry/implementation-map.yaml:L9614-L9625] |
| 10 | [docs/registry/implementation-map.yaml:L9626-L9637] |
| 11 | [docs/registry/implementation-map.yaml:L9638-L9652] |
| 12 | [docs/registry/implementation-map.yaml:L9653-L9664] |
| 13 | [docs/registry/implementation-map.yaml:L9665-L9678] |
| 14 | [docs/registry/implementation-map.yaml:L9679-L9690] |
| 15 | [docs/registry/implementation-map.yaml:L9691-L9704] |
| 16 | [docs/registry/implementation-map.yaml:L9705-L9715] |
| 17 | [docs/registry/implementation-map.yaml:L9716-L9727] |
| 18 | [docs/registry/implementation-map.yaml:L9728-L9739] |
| 19 | [docs/registry/implementation-map.yaml:L9740-L9753] |
| 20 | [docs/registry/implementation-map.yaml:L9754-L9765] |
| 21 | [docs/registry/implementation-map.yaml:L9766-L9777] |
| 22 | [docs/registry/implementation-map.yaml:L9778-L9788] |
| 23 | [docs/registry/implementation-map.yaml:L9789-L9800] |
| 24 | [docs/registry/implementation-map.yaml:L9801-L9812] |
| 25 | [docs/registry/implementation-map.yaml:L9813-L9826] |
| 26 | [docs/registry/implementation-map.yaml:L9827-L9838] |
| 27 | [docs/registry/implementation-map.yaml:L9839-L9849] |
| 28 | [docs/registry/implementation-map.yaml:L9850-L9861] |
| 29 | [docs/registry/implementation-map.yaml:L9862-L9873] |
| 30 | [docs/registry/implementation-map.yaml:L9874-L9885] |
| 31 | [docs/registry/implementation-map.yaml:L9886-L9897] |
| 32 | [docs/registry/implementation-map.yaml:L9898-L9909] |
| 33 | [docs/registry/implementation-map.yaml:L9910-L9921] |
| 34 | [docs/registry/implementation-map.yaml:L9922-L9932] |
| 35 | [docs/registry/implementation-map.yaml:L9933-L9943] |
| 36 | [docs/registry/implementation-map.yaml:L9944-L9954] |
| 37 | [docs/registry/implementation-map.yaml:L9955-L9965] |
| 38 | [docs/registry/implementation-map.yaml:L9966-L9977] |
| 39 | [docs/registry/implementation-map.yaml:L9978-L9989] |
| 40 | [docs/registry/implementation-map.yaml:L9990-L10004] |
| 41 | [docs/registry/implementation-map.yaml:L10005-L10017] |
| 42 | [docs/registry/implementation-map.yaml:L10018-L10032] |
| 43 | [docs/registry/implementation-map.yaml:L10033-L10046] |
| 44 | [docs/registry/implementation-map.yaml:L10047-L10060] |
| 45 | [docs/registry/implementation-map.yaml:L10061-L10072] |
| 46 | [docs/registry/implementation-map.yaml:L10073-L10087] |
| 47 | [docs/registry/implementation-map.yaml:L10088-L10099] |
| 48 | [docs/registry/implementation-map.yaml:L10100-L10111] |
| 49 | [docs/registry/implementation-map.yaml:L10112-L10122] |
| 50 | [docs/registry/implementation-map.yaml:L10123-L10134] |
| 51 | [docs/registry/implementation-map.yaml:L10135-L10146] |
| 52 | [docs/registry/implementation-map.yaml:L10147-L10158] |
| 53 | [docs/registry/implementation-map.yaml:L10159-L10169] |
| 54 | [docs/registry/implementation-map.yaml:L10170-L10181] |
| 55 | [docs/registry/implementation-map.yaml:L10182-L10193] |
| 56 | [docs/registry/implementation-map.yaml:L10194-L10207] |
| 57 | [docs/registry/implementation-map.yaml:L10208-L10222] |
| 58 | [docs/registry/implementation-map.yaml:L10223-L10235] |
| 59 | [docs/registry/implementation-map.yaml:L10236-L10246] |
| 60 | [docs/registry/implementation-map.yaml:L10247-L10260] |
| 61 | [docs/registry/implementation-map.yaml:L10261-L10272] |
| 62 | [docs/registry/implementation-map.yaml:L10273-L10284] |
| 63 | [docs/registry/implementation-map.yaml:L10285-L10296] |
| 64 | [docs/registry/implementation-map.yaml:L10297-L10307] |
| 65 | [docs/registry/implementation-map.yaml:L10308-L10319] |
| 66 | [docs/registry/implementation-map.yaml:L10320-L10333] |
| 67 | [docs/registry/implementation-map.yaml:L10334-L10345] |
| 68 | [docs/registry/implementation-map.yaml:L10346-L10356] |
| 69 | [docs/registry/implementation-map.yaml:L10357-L10368] |
| 70 | [docs/registry/implementation-map.yaml:L10369-L10380] |
| 71 | [docs/registry/implementation-map.yaml:L10381-L10391] |
| 72 | [docs/registry/implementation-map.yaml:L10392-L10403] |
| 73 | [docs/registry/implementation-map.yaml:L10404-L10415] |
| 74 | [docs/registry/implementation-map.yaml:L10416-L10427] |
| 75 | [docs/registry/implementation-map.yaml:L10428-L10439] |
| 76 | [docs/registry/implementation-map.yaml:L10440-L10451] |
| 77 | [docs/registry/implementation-map.yaml:L10452-L10465] |
| 78 | [docs/registry/implementation-map.yaml:L10466-L10477] |
| 79 | [docs/registry/implementation-map.yaml:L10478-L10489] |
| 80 | [docs/registry/implementation-map.yaml:L10490-L10501] |
| 81 | [docs/registry/implementation-map.yaml:L10502-L10513] |
| 82 | [docs/registry/implementation-map.yaml:L10514-L10527] |
| 83 | [docs/registry/implementation-map.yaml:L10528-L10539] |
| 84 | [docs/registry/implementation-map.yaml:L10540-L10551] |
| 85 | [docs/registry/implementation-map.yaml:L10552-L10563] |
| 86 | [docs/registry/implementation-map.yaml:L10564-L10575] |
| 87 | [docs/registry/implementation-map.yaml:L10576-L10587] |
| 88 | [docs/registry/implementation-map.yaml:L10588-L10599] |
| 89 | [docs/registry/implementation-map.yaml:L10600-L10611] |
| 90 | [docs/registry/implementation-map.yaml:L10612-L10623] |
| 91 | [docs/registry/implementation-map.yaml:L10624-L10635] |
| 92 | [docs/registry/implementation-map.yaml:L10636-L10647] |
| 93 | [docs/registry/implementation-map.yaml:L10648-L10659] |
| 94 | [docs/registry/implementation-map.yaml:L10660-L10671] |
| 95 | [docs/registry/implementation-map.yaml:L10672-L10683] |
| 96 | [docs/registry/implementation-map.yaml:L10684-L10699] |
| 97 | [docs/registry/implementation-map.yaml:L10700-L10711] |
| 98 | [docs/registry/implementation-map.yaml:L10712-L10725] |
| 99 | [docs/registry/implementation-map.yaml:L10726-L10737] |
| 100 | [docs/registry/implementation-map.yaml:L10738-L10749] |
| 101 | [docs/registry/implementation-map.yaml:L10750-L10761] |
| 102 | [docs/registry/implementation-map.yaml:L10762-L10775] |
| 103 | [docs/registry/implementation-map.yaml:L10776-L10789] |
| 104 | [docs/registry/implementation-map.yaml:L10790-L10801] |
| 105 | [docs/registry/implementation-map.yaml:L10802-L10815] |
| 106 | [docs/registry/implementation-map.yaml:L10816-L10828] |
| 107 | [docs/registry/implementation-map.yaml:L10829-L10841] |
| 108 | [docs/registry/implementation-map.yaml:L10842-L10853] |
| 109 | [docs/registry/implementation-map.yaml:L10854-L10865] |
| 110 | [docs/registry/implementation-map.yaml:L10866-L10877] |
| 111 | [docs/registry/implementation-map.yaml:L10878-L10891] |
| 112 | [docs/registry/implementation-map.yaml:L10892-L10905] |
| 113 | [docs/registry/implementation-map.yaml:L10906-L10917] |
| 114 | [docs/registry/implementation-map.yaml:L10918-L10933] |
| 115 | [docs/registry/implementation-map.yaml:L10934-L10948] |
| 116 | [docs/registry/implementation-map.yaml:L10949-L10962] |
| 117 | [docs/registry/implementation-map.yaml:L10963-L10979] |
| 118 | [docs/registry/implementation-map.yaml:L10980-L10991] |
| 119 | [docs/registry/implementation-map.yaml:L10992-L11003] |
| 120 | [docs/registry/implementation-map.yaml:L11004-L11015] |
| 121 | [docs/registry/implementation-map.yaml:L11016-L11027] |
| 122 | [docs/registry/implementation-map.yaml:L11028-L11039] |
| 123 | [docs/registry/implementation-map.yaml:L11040-L11051] |
| 124 | [docs/registry/implementation-map.yaml:L11052-L11063] |
| 125 | [docs/registry/implementation-map.yaml:L11064-L11075] |
| 126 | [docs/registry/implementation-map.yaml:L11076-L11089] |
| 127 | [docs/registry/implementation-map.yaml:L11090-L11101] |
| 128 | [docs/registry/implementation-map.yaml:L11102-L11113] |
| 129 | [docs/registry/implementation-map.yaml:L11114-L11125] |
| 130 | [docs/registry/implementation-map.yaml:L11126-L11137] |
| 131 | [docs/registry/implementation-map.yaml:L11138-L11149] |
| 132 | [docs/registry/implementation-map.yaml:L11150-L11163] |
| 133 | [docs/registry/implementation-map.yaml:L11164-L11175] |
| 134 | [docs/registry/implementation-map.yaml:L11176-L11187] |
| 135 | [docs/registry/implementation-map.yaml:L11188-L11202] |
| 136 | [docs/registry/implementation-map.yaml:L11203-L11213] |
| 137 | [docs/registry/implementation-map.yaml:L11214-L11225] |
| 138 | [docs/registry/implementation-map.yaml:L11226-L11237] |
| 139 | [docs/registry/implementation-map.yaml:L11238-L11248] |
| 140 | [docs/registry/implementation-map.yaml:L11249-L11260] |
| 141 | [docs/registry/implementation-map.yaml:L11261-L11274] |
| 142 | [docs/registry/implementation-map.yaml:L11275-L11286] |
| 143 | [docs/registry/implementation-map.yaml:L11287-L11298] |
| 144 | [docs/registry/implementation-map.yaml:L11299-L11310] |
| 145 | [docs/registry/implementation-map.yaml:L11311-L11322] |
| 146 | [docs/registry/implementation-map.yaml:L11323-L11334] |
| 147 | [docs/registry/implementation-map.yaml:L11335-L11346] |
| 148 | [docs/registry/implementation-map.yaml:L11347-L11360] |
| 149 | [docs/registry/implementation-map.yaml:L11361-L11374] |
| 150 | [docs/registry/implementation-map.yaml:L11375-L11388] |
| 151 | [docs/registry/implementation-map.yaml:L11389-L11400] |
| 152 | [docs/registry/implementation-map.yaml:L11401-L11414] |
| 153 | [docs/registry/implementation-map.yaml:L11415-L11426] |
| 154 | [docs/registry/implementation-map.yaml:L11427-L11438] |
| 155 | [docs/registry/implementation-map.yaml:L11439-L11450] |
| 156 | [docs/registry/implementation-map.yaml:L11451-L11462] |
| 157 | [docs/registry/implementation-map.yaml:L11463-L11474] |
| 158 | [docs/registry/implementation-map.yaml:L11475-L11486] |
| 159 | [docs/registry/implementation-map.yaml:L11487-L11498] |
| 160 | [docs/registry/implementation-map.yaml:L11499-L11510] |
| 161 | [docs/registry/implementation-map.yaml:L11511-L11524] |
| 162 | [docs/registry/implementation-map.yaml:L11525-L11536] |
| 163 | [docs/registry/implementation-map.yaml:L11537-L11548] |
| 164 | [docs/registry/implementation-map.yaml:L11549-L11560] |
| 165 | [docs/registry/implementation-map.yaml:L11561-L11574] |
| 166 | [docs/registry/implementation-map.yaml:L11575-L11590] |
| 167 | [docs/registry/implementation-map.yaml:L11591-L11603] |
| 168 | [docs/registry/implementation-map.yaml:L11604-L11619] |
| 169 | [docs/registry/implementation-map.yaml:L11620-L11633] |
| 170 | [docs/registry/implementation-map.yaml:L11634-L11648] |
| 171 | [docs/registry/implementation-map.yaml:L11649-L11662] |
| 172 | [docs/registry/implementation-map.yaml:L11663-L11674] |
| 173 | [docs/registry/implementation-map.yaml:L11675-L11686] |
| 174 | [docs/registry/implementation-map.yaml:L11687-L11698] |
| 175 | [docs/registry/implementation-map.yaml:L11699-L11710] |
| 176 | [docs/registry/implementation-map.yaml:L11711-L11724] |
| 177 | [docs/registry/implementation-map.yaml:L11725-L11736] |
| 178 | [docs/registry/implementation-map.yaml:L11737-L11748] |
| 179 | [docs/registry/implementation-map.yaml:L11749-L11762] |
| 180 | [docs/registry/implementation-map.yaml:L11763-L11774] |
| 181 | [docs/registry/implementation-map.yaml:L11775-L11786] |
| 182 | [docs/registry/implementation-map.yaml:L11787-L11798] |
| 183 | [docs/registry/implementation-map.yaml:L11799-L11812] |
| 184 | [docs/registry/implementation-map.yaml:L11813-L11824] |
| 185 | [docs/registry/implementation-map.yaml:L11825-L11836] |
| 186 | [docs/registry/implementation-map.yaml:L11837-L11850] |
| 187 | [docs/registry/implementation-map.yaml:L11851-L11862] |
| 188 | [docs/registry/implementation-map.yaml:L11863-L11874] |
| 189 | [docs/registry/implementation-map.yaml:L11875-L11888] |
| 190 | [docs/registry/implementation-map.yaml:L11889-L11900] |
| 191 | [docs/registry/implementation-map.yaml:L11901-L11912] |
| 192 | [docs/registry/implementation-map.yaml:L11913-L11926] |
| 193 | [docs/registry/implementation-map.yaml:L11927-L11938] |
| 194 | [docs/registry/implementation-map.yaml:L11939-L11950] |
| 195 | [docs/registry/implementation-map.yaml:L11951-L11964] |
| 196 | [docs/registry/implementation-map.yaml:L11965-L11976] |
| 197 | [docs/registry/implementation-map.yaml:L11977-L11990] |
| 198 | [docs/registry/implementation-map.yaml:L11991-L12002] |
| 199 | [docs/registry/implementation-map.yaml:L12003-L12014] |
| 200 | [docs/registry/implementation-map.yaml:L12015-L12026] |
| 201 | [docs/registry/implementation-map.yaml:L12027-L12038] |
| 202 | [docs/registry/implementation-map.yaml:L12039-L12052] |
| 203 | [docs/registry/implementation-map.yaml:L12053-L12064] |
| 204 | [docs/registry/implementation-map.yaml:L12065-L12078] |
| 205 | [docs/registry/implementation-map.yaml:L12079-L12095] |
| 206 | [docs/registry/implementation-map.yaml:L12096-L12107] |
| 207 | [docs/registry/implementation-map.yaml:L12108-L12119] |
| 208 | [docs/registry/implementation-map.yaml:L12120-L12131] |
| 209 | [docs/registry/implementation-map.yaml:L12132-L12143] |
| 210 | [docs/registry/implementation-map.yaml:L12144-L12159] |
| 211 | [docs/registry/implementation-map.yaml:L12160-L12173] |
| 212 | [docs/registry/implementation-map.yaml:L12174-L12186] |
| 213 | [docs/registry/implementation-map.yaml:L12187-L12199] |
| 214 | [docs/registry/implementation-map.yaml:L12200-L12211] |
| 215 | [docs/registry/implementation-map.yaml:L12212-L12226] |
| 216 | [docs/registry/implementation-map.yaml:L12227-L12238] |
| 217 | [docs/registry/implementation-map.yaml:L12239-L12254] |
| 218 | [docs/registry/implementation-map.yaml:L12255-L12266] |
| 219 | [docs/registry/implementation-map.yaml:L12267-L12278] |
| 220 | [docs/registry/implementation-map.yaml:L12279-L12290] |
| 221 | [docs/registry/implementation-map.yaml:L12291-L12302] |
| 222 | [docs/registry/implementation-map.yaml:L12303-L12314] |
| 223 | [docs/registry/implementation-map.yaml:L12315-L12326] |
| 224 | [docs/registry/implementation-map.yaml:L12327-L12338] |
| 225 | [docs/registry/implementation-map.yaml:L12339-L12350] |
| 226 | [docs/registry/implementation-map.yaml:L12351-L12362] |
| 227 | [docs/registry/implementation-map.yaml:L12363-L12376] |
| 228 | [docs/registry/implementation-map.yaml:L12377-L12388] |
| 229 | [docs/registry/implementation-map.yaml:L12389-L12400] |
| 230 | [docs/registry/implementation-map.yaml:L12401-L12412] |
| 231 | [docs/registry/implementation-map.yaml:L12413-L12427] |
| 232 | [docs/registry/implementation-map.yaml:L12428-L12441] |
| 233 | [docs/registry/implementation-map.yaml:L12442-L12453] |
| 234 | [docs/registry/implementation-map.yaml:L12454-L12465] |
| 235 | [docs/registry/implementation-map.yaml:L12466-L12477] |
| 236 | [docs/registry/implementation-map.yaml:L12478-L12489] |
| 237 | [docs/registry/implementation-map.yaml:L12490-L12503] |
| 238 | [docs/registry/implementation-map.yaml:L12504-L12515] |
| 239 | [docs/registry/implementation-map.yaml:L12516-L12529] |
| 240 | [docs/registry/implementation-map.yaml:L12530-L12541] |
| 241 | [docs/registry/implementation-map.yaml:L12542-L12555] |
| 242 | [docs/registry/implementation-map.yaml:L12556-L12567] |
| 243 | [docs/registry/implementation-map.yaml:L12568-L12582] |
| 244 | [docs/registry/implementation-map.yaml:L12583-L12594] |
| 245 | [docs/registry/implementation-map.yaml:L12595-L12606] |
| 246 | [docs/registry/implementation-map.yaml:L12607-L12620] |
| 247 | [docs/registry/implementation-map.yaml:L12621-L12632] |
| 248 | [docs/registry/implementation-map.yaml:L12633-L12646] |
| 249 | [docs/registry/implementation-map.yaml:L12647-L12658] |
| 250 | [docs/registry/implementation-map.yaml:L12659-L12670] |
| 251 | [docs/registry/implementation-map.yaml:L12671-L12682] |
| 252 | [docs/registry/implementation-map.yaml:L12683-L12696] |
| 253 | [docs/registry/implementation-map.yaml:L12697-L12710] |
| 254 | [docs/registry/implementation-map.yaml:L12711-L12721] |
| 255 | [docs/registry/implementation-map.yaml:L12722-L12733] |
| 256 | [docs/registry/implementation-map.yaml:L12734-L12745] |
| 257 | [docs/registry/implementation-map.yaml:L12746-L12759] |
| 258 | [docs/registry/implementation-map.yaml:L12760-L12773] |
| 259 | [docs/registry/implementation-map.yaml:L12774-L12787] |
| 260 | [docs/registry/implementation-map.yaml:L12788-L12799] |
| 261 | [docs/registry/implementation-map.yaml:L12800-L12813] |
| 262 | [docs/registry/implementation-map.yaml:L12814-L12827] |
| 263 | [docs/registry/implementation-map.yaml:L12828-L12839] |
| 264 | [docs/registry/implementation-map.yaml:L12840-L12850] |
| 265 | [docs/registry/implementation-map.yaml:L12851-L12862] |
| 266 | [docs/registry/implementation-map.yaml:L12863-L12874] |
| 267 | [docs/registry/implementation-map.yaml:L12875-L12886] |
| 268 | [docs/registry/implementation-map.yaml:L12887-L12898] |
| 269 | [docs/registry/implementation-map.yaml:L12899-L12910] |
| 270 | [docs/registry/implementation-map.yaml:L12911-L12922] |
| 271 | [docs/registry/implementation-map.yaml:L12923-L12934] |
| 272 | [docs/registry/implementation-map.yaml:L12935-L12946] |
| 273 | [docs/registry/implementation-map.yaml:L12947-L12958] |
| 274 | [docs/registry/implementation-map.yaml:L12959-L12973] |
| 275 | [docs/registry/implementation-map.yaml:L12974-L12986] |
| 276 | [docs/registry/implementation-map.yaml:L12987-L12998] |
| 277 | [docs/registry/implementation-map.yaml:L12999-L13010] |
| 278 | [docs/registry/implementation-map.yaml:L13011-L13022] |
| 279 | [docs/registry/implementation-map.yaml:L13023-L13034] |
| 280 | [docs/registry/implementation-map.yaml:L13035-L13048] |
| 281 | [docs/registry/implementation-map.yaml:L13049-L13062] |
| 282 | [docs/registry/implementation-map.yaml:L13063-L13074] |
| 283 | [docs/registry/implementation-map.yaml:L13075-L13086] |
| 284 | [docs/registry/implementation-map.yaml:L13087-L13101] |
| 285 | [docs/registry/implementation-map.yaml:L13102-L13113] |
| 286 | [docs/registry/implementation-map.yaml:L13114-L13125] |
| 287 | [docs/registry/implementation-map.yaml:L13126-L13139] |
| 288 | [docs/registry/implementation-map.yaml:L13140-L13153] |
| 289 | [docs/registry/implementation-map.yaml:L13154-L13167] |
| 290 | [docs/registry/implementation-map.yaml:L13168-L13181] |
| 291 | [docs/registry/implementation-map.yaml:L13182-L13193] |
| 292 | [docs/registry/implementation-map.yaml:L13194-L13204] |
| 293 | [docs/registry/implementation-map.yaml:L13205-L13216] |
| 294 | [docs/registry/implementation-map.yaml:L13217-L13228] |
| 295 | [docs/registry/implementation-map.yaml:L13229-L13240] |

### Repository-wide follow-up

The family map covers source, examples, tests and scripts outside this package; full inventory entries above retain these candidates without narrowing the search to `src/`. [docs/registry/implementation-map.md:L13-L20]
Additional repository-wide text searches used `rg -l 'estimate_gyradii|parse_heading_block_rao|mass.*k.*\*|_extract_anchor_points|capability_score' src scripts examples tests --glob '*.py'`; outside-package hits are map follow-up candidates, not extra counted-package misses. [src/digitalmodel/marine_ops/vessel_db/gyradii.py:L68-L122] [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L92-L152]
Map follow-up: add/classify the numerical workbook parser, mass/inertia conversion, empirical gyradii, crane anchors, confidence/capability, cross-store audit and promotion method families before counting them in a later map revision (issue pending (8)). [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L69-L123] [src/digitalmodel/marine_ops/vessel_db/gyradii.py:L68-L122] [src/digitalmodel/marine_ops/vessel_db/loader.py:L296-L357] [src/digitalmodel/marine_ops/vessel_db/confidence.py:L141-L181] [src/digitalmodel/marine_ops/vessel_db/audit.py:L97-L117] [src/digitalmodel/marine_ops/vessel_db/promote.py:L179-L205]

## 10. Coverage and open items

Coverage: all nine package Python files, all eleven nonempty module test files, the curated-data README/schema, and the consumer examples listed below were read; historical document discovery is listed separately rather than adopted as an implemented behaviour claim. [src/digitalmodel/marine_ops/vessel_db/__init__.py:L1-L101]

| Package file read | Source coverage |
| --- | --- |
| `__init__.py` | [src/digitalmodel/marine_ops/vessel_db/__init__.py:L1-L101] |
| `audit.py` | [src/digitalmodel/marine_ops/vessel_db/audit.py:L1-L204] |
| `confidence.py` | [src/digitalmodel/marine_ops/vessel_db/confidence.py:L1-L181] |
| `gyradii.py` | [src/digitalmodel/marine_ops/vessel_db/gyradii.py:L1-L122] |
| `hulls_adapter.py` | [src/digitalmodel/marine_ops/vessel_db/hulls_adapter.py:L1-L196] |
| `loader.py` | [src/digitalmodel/marine_ops/vessel_db/loader.py:L1-L431] |
| `mass_properties.py` | [src/digitalmodel/marine_ops/vessel_db/mass_properties.py:L1-L123] |
| `promote.py` | [src/digitalmodel/marine_ops/vessel_db/promote.py:L1-L247] |
| `wed_adapter.py` | [src/digitalmodel/marine_ops/vessel_db/wed_adapter.py:L1-L233] |

| Module tests read | Source coverage |
| --- | --- |
| `test_audit.py` | [tests/marine_ops/vessel_db/test_audit.py:L1-L58] |
| `test_capability_demo.py` | [tests/marine_ops/vessel_db/test_capability_demo.py:L1-L42] |
| `test_class_level_record.py` | [tests/marine_ops/vessel_db/test_class_level_record.py:L1-L50] |
| `test_confidence.py` | [tests/marine_ops/vessel_db/test_confidence.py:L1-L126] |
| `test_go_no_go_demo.py` | [tests/marine_ops/vessel_db/test_go_no_go_demo.py:L1-L67] |
| `test_gyradii.py` | [tests/marine_ops/vessel_db/test_gyradii.py:L1-L44] |
| `test_hulls_adapter.py` | [tests/marine_ops/vessel_db/test_hulls_adapter.py:L1-L78] |
| `test_loader.py` | [tests/marine_ops/vessel_db/test_loader.py:L1-L101] |
| `test_mass_properties.py` | [tests/marine_ops/vessel_db/test_mass_properties.py:L1-L96] |
| `test_promote.py` | [tests/marine_ops/vessel_db/test_promote.py:L1-L59] |
| `test_wed_adapter.py` | [tests/marine_ops/vessel_db/test_wed_adapter.py:L1-L176] |

| Other reading | Source |
| --- | --- |
| `data/vessels/README.md` | [data/vessels/README.md:L1-L131] |
| `data/vessels/SCHEMA.yaml` | [data/vessels/SCHEMA.yaml:L1-L90] |
| `examples/demos/diffraction/diffraction_setup.py` | [examples/demos/diffraction/diffraction_setup.py:L1-L78] |
| `examples/demos/diffraction/prepare_vessel_decks.py` | [examples/demos/diffraction/prepare_vessel_decks.py:L1-L138] |
| `examples/demos/diffraction/validate_decks.py` | [examples/demos/diffraction/validate_decks.py:L1-L96] |
| `examples/demos/installation/go_no_go_crane_curves.py` | [examples/demos/installation/go_no_go_crane_curves.py:L1-L170] |
| `examples/demos/installation/vessel_capability_score.py` | [examples/demos/installation/vessel_capability_score.py:L1-L80] |

The exact module-name search in `docs/` found the implementation-map summary; a broader vessel-database phrase search also found the historical documents below. Matching passages were read for discovery; their completion and maturity claims were not adopted as this package's results. [docs/registry/implementation-map.md:L20-L20]

| Discovery document | Matching source lines |
| --- | --- |
| `docs/plans/2026-05-20-issue-616-ocimf-polar-vessel-force-overlay.md` | [docs/plans/2026-05-20-issue-616-ocimf-polar-vessel-force-overlay.md:L28-L28] |
| `docs/registry/implementation-map.md` | [docs/registry/implementation-map.md:L20-L20] |
| `docs/plans/2026-05-05-issue-467-subsea-installation-vessel-feedback.md` | [docs/plans/2026-05-05-issue-467-subsea-installation-vessel-feedback.md:L20-L20], [docs/plans/2026-05-05-issue-467-subsea-installation-vessel-feedback.md:L23-L23], [docs/plans/2026-05-05-issue-467-subsea-installation-vessel-feedback.md:L38-L38] |
| `docs/domains/mooring/PHASE5_CALM_BUOY_OPERATIONAL_ANALYSIS.md` | [docs/domains/mooring/PHASE5_CALM_BUOY_OPERATIONAL_ANALYSIS.md:L38-L38], [docs/domains/mooring/PHASE5_CALM_BUOY_OPERATIONAL_ANALYSIS.md:L444-L444], [docs/domains/mooring/PHASE5_CALM_BUOY_OPERATIONAL_ANALYSIS.md:L506-L506] |
| `docs/domains/fatigue/PHASE4_FATIGUE_FINDINGS.md` | [docs/domains/fatigue/PHASE4_FATIGUE_FINDINGS.md:L267-L267] |
| `docs/domains/data/DATA_PROCUREMENT_STRATEGY.md` | [docs/domains/data/DATA_PROCUREMENT_STRATEGY.md:L26-L26], [docs/domains/data/DATA_PROCUREMENT_STRATEGY.md:L65-L65], [docs/domains/data/DATA_PROCUREMENT_STRATEGY.md:L343-L343], [docs/domains/data/DATA_PROCUREMENT_STRATEGY.md:L412-L412] |
| `docs/domains/data/DATA_REORGANIZATION_PLAN.md` | [docs/domains/data/DATA_REORGANIZATION_PLAN.md:L93-L93] |
| `docs/domains/data/DATA_PROCUREMENT_COMPLETION_SUMMARY.md` | [docs/domains/data/DATA_PROCUREMENT_COMPLETION_SUMMARY.md:L15-L15], [docs/domains/data/DATA_PROCUREMENT_COMPLETION_SUMMARY.md:L27-L27], [docs/domains/data/DATA_PROCUREMENT_COMPLETION_SUMMARY.md:L33-L33], [docs/domains/data/DATA_PROCUREMENT_COMPLETION_SUMMARY.md:L39-L39], [docs/domains/data/DATA_PROCUREMENT_COMPLETION_SUMMARY.md:L46-L46], [docs/domains/data/DATA_PROCUREMENT_COMPLETION_SUMMARY.md:L390-L390], [docs/domains/data/DATA_PROCUREMENT_COMPLETION_SUMMARY.md:L455-L455], [docs/domains/data/DATA_PROCUREMENT_COMPLETION_SUMMARY.md:L644-L644], [docs/domains/data/DATA_PROCUREMENT_COMPLETION_SUMMARY.md:L720-L720], [docs/domains/data/DATA_PROCUREMENT_COMPLETION_SUMMARY.md:L777-L777] |
| `docs/domains/automation/WEB_SCRAPING_FINDINGS.md` | [docs/domains/automation/WEB_SCRAPING_FINDINGS.md:L4-L4], [docs/domains/automation/WEB_SCRAPING_FINDINGS.md:L11-L11], [docs/domains/automation/WEB_SCRAPING_FINDINGS.md:L341-L341] |
| `docs/domains/project-docs/IMPLEMENTATION_PHASE2_SUMMARY.md` | [docs/domains/project-docs/IMPLEMENTATION_PHASE2_SUMMARY.md:L12-L12], [docs/domains/project-docs/IMPLEMENTATION_PHASE2_SUMMARY.md:L37-L37], [docs/domains/project-docs/IMPLEMENTATION_PHASE2_SUMMARY.md:L42-L42], [docs/domains/project-docs/IMPLEMENTATION_PHASE2_SUMMARY.md:L53-L53], [docs/domains/project-docs/IMPLEMENTATION_PHASE2_SUMMARY.md:L55-L55], [docs/domains/project-docs/IMPLEMENTATION_PHASE2_SUMMARY.md:L114-L114], [docs/domains/project-docs/IMPLEMENTATION_PHASE2_SUMMARY.md:L136-L136], [docs/domains/project-docs/IMPLEMENTATION_PHASE2_SUMMARY.md:L428-L428], [docs/domains/project-docs/IMPLEMENTATION_PHASE2_SUMMARY.md:L442-L442] |
| `docs/domains/automation/SELENIUM_FINAL_RESULTS.md` | [docs/domains/automation/SELENIUM_FINAL_RESULTS.md:L10-L10], [docs/domains/automation/SELENIUM_FINAL_RESULTS.md:L129-L129], [docs/domains/automation/SELENIUM_FINAL_RESULTS.md:L161-L161], [docs/domains/automation/SELENIUM_FINAL_RESULTS.md:L279-L279], [docs/domains/automation/SELENIUM_FINAL_RESULTS.md:L314-L314] |
| `docs/domains/automation/SELENIUM_SETUP.md` | [docs/domains/automation/SELENIUM_SETUP.md:L299-L299] |
| `docs/domains/marine-engineering/MARINE_ENGINEERING_PHASE_1-3_SUMMARY.md` | [docs/domains/marine-engineering/MARINE_ENGINEERING_PHASE_1-3_SUMMARY.md:L137-L137] |
| `docs/domains/project-docs/phase2_implementation_plan.md` | [docs/domains/project-docs/phase2_implementation_plan.md:L271-L271], [docs/domains/project-docs/phase2_implementation_plan.md:L561-L561] |
| `docs/domains/project-docs/PHASE3_OCIMF_FINDINGS.md` | [docs/domains/project-docs/PHASE3_OCIMF_FINDINGS.md:L308-L308] |
| `docs/domains/project-docs/PHASE2_INITIAL_FINDINGS.md` | [docs/domains/project-docs/PHASE2_INITIAL_FINDINGS.md:L109-L109], [docs/domains/project-docs/PHASE2_INITIAL_FINDINGS.md:L276-L276] |
| `docs/domains/project-docs/PHASE2_EQUIPMENT_STRATEGY.md` | [docs/domains/project-docs/PHASE2_EQUIPMENT_STRATEGY.md:L5-L5], [docs/domains/project-docs/PHASE2_EQUIPMENT_STRATEGY.md:L11-L11], [docs/domains/project-docs/PHASE2_EQUIPMENT_STRATEGY.md:L13-L13], [docs/domains/project-docs/PHASE2_EQUIPMENT_STRATEGY.md:L278-L278] |
| `docs/domains/project-docs/IMPLEMENTATION_PHASE4_SUMMARY.md` | [docs/domains/project-docs/IMPLEMENTATION_PHASE4_SUMMARY.md:L758-L758] |
| `docs/domains/charts/phase2/ocimf/PROMPT_TEMPLATE.md` | [docs/domains/charts/phase2/ocimf/PROMPT_TEMPLATE.md:L148-L148] |

| Open item / responsible role | Tracking and completion criterion |
| --- | --- |
| UNVERIFIED standards metadata from section 3; maintainers | issue pending (1): qualify both named reference families and sidecar contract; no licensed copy read |
| Curated parsing, units, aliases and crane omission coverage; maintainers | issue pending (2): add explicit boundary fixtures and omission evidence |
| RAO phase/heading/rotation conventions and row losses; maintainers | issue pending (3): dimensioned fixture and rejection accounting; library acquisition also #852 |
| Mass provenance, missing tensor axes and datum/export guard; maintainers | issue pending (4): complete finite-positive export contract and independent reference |
| Confidence, audit and adapter empty-result ambiguity; maintainers | issue pending (5): explicit availability/completeness record; original interfaces #857 and #888; resolver incident #1297; portability concern #1002 |
| Promotion stale output, extra columns and caveat propagation; maintainers | issue pending (6): isolated empty/stale-output regression |
| External references and test coverage; maintainers | issue pending (7): supply authorised data and independent expected values; obtain downstream G1/G4/G5 evidence; review this draft and replace draft markers with issue numbers |
| Map classification and uncovered method families; maintainers | issue pending (8): update family definitions and the parser/helper inventory before later counting |
| Existing duplicate solver routing mapping; maintainers | issue pending (9): combine the two baseline mappings; strict duplicate-key validation fails at the pin [docs/registry/module-routing.yaml:L303-L316] |

The `Citation` contract was readable; no private data repository was read and no client-derived value or private sidecar content is included. There is no supplied private lesson identifier requiring a sidecar entry in this draft.

### Offline execution record

Revision 1 focused comparison: the BASE archive and the edited tree each collected 86 module cases, with 80 passed, 0 failed and 6 external-data skips; all 86 status entries agree. The archive retains the source and test bytes at the PR base. The command uses `uv run --no-project` with pytest, NumPy, pandas, PyYAML, openpyxl, xlrd, SciPy, pytest-asyncio and pydantic, then `python -m pytest tests/marine_ops/vessel_db -q -rA -p no:cacheprovider --basetemp=<scratch>`, with `PYTHONPATH=src`. This focused comparison does not repeat the historical extended runs below.

Initial module-only run at the code pin: 86 collected, 80 passed, 0 failed, 6 skipped; no solver was imported. The six skipped cases are recorded as data-dependent NOT_RUN qualifications below, while retaining pytest's raw SKIP status.

| Node ID | Raw status / qualification reason |
| --- | --- |
| `tests/marine_ops/vessel_db/test_hulls_adapter.py::test_real_library_parses_when_configured` | SKIP / NOT_RUN: external RAO library not configured; #852 and issue pending (7) |
| `tests/marine_ops/vessel_db/test_wed_adapter.py::test_construction_vessels_real_named` | SKIP / NOT_RUN: external record collection checkout absent; issue pending (7) |
| `tests/marine_ops/vessel_db/test_wed_adapter.py::test_drilling_rigs_large_fleet` | SKIP / NOT_RUN: external record collection checkout absent; issue pending (7) |
| `tests/marine_ops/vessel_db/test_wed_adapter.py::test_crane_vessels_have_curves_with_reach` | SKIP / NOT_RUN: external record collection checkout absent; issue pending (7) |
| `tests/marine_ops/vessel_db/test_wed_adapter.py::test_wed_wins_on_collision` | SKIP / NOT_RUN: external record collection checkout absent; issue pending (7) |
| `tests/marine_ops/vessel_db/test_wed_adapter.py::test_fleet_summary_counts` | SKIP / NOT_RUN: external record collection checkout absent; issue pending (7) |
| `tests/hydrodynamics/diffraction/test_deck_validation.py::test_parametric_fpso_mesh_is_physical` | NOT_RUN: declared fowt extra attempted; Capytaine 3.0.0 build failed, `/usr/bin/ld: cannot find -lquadmath: No such file or directory`; issue pending (7) |

Extended BASE and AFTER each collected 655 cases: 648 passed, 0 failed, 7 skipped; all 648 passing node IDs agree. Module-only coverage remains 86 cases (80 passed, 6 external-data skips), replacing an unverified historical count. The seven extended skips also represent seven NOT_RUN qualifications; these are overlapping descriptions, not additional tests. The standard command form is `PYTHONPATH=src uv run --no-sources --extra test python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`; no addopts override, deselection or test alteration is used. The failed dependency attempt added `--extra fowt` to the same form. Checks run against the uncommitted tree because the documentation change does not alter the pinned module code.


| Run | Paths and command form | Result at pinned code |
| --- | --- | --- |
| Initial BASE | `uv run --no-sources python -m pytest tests/marine_ops/vessel_db -q -rA -p no:cacheprovider --basetemp=<scratch>` with `PYTHONPATH=src` | 86 collected; 80 passed; 0 failed; 6 skipped / 6 data qualifications NOT_RUN |
| Declared-extra attempt | Extended path set below, same command with `--extra test --extra fowt` | Failed before collection: Capytaine 3.0.0 linker error; 0 tests executed; per-test counts unavailable |
| Extended BASE | Same command with `--extra test` and extended path set | 655 collected; 648 passed; 0 failed; 7 skipped / 7 qualifications NOT_RUN |
| Extended AFTER | Exactly the extended BASE form on the uncommitted documentation tree | 655 collected; 648 passed; 0 failed; 7 skipped / 7 qualifications NOT_RUN |

Extended path set: `tests/marine_ops/vessel_db`, `tests/marine_ops/installation/test_vessel_suitability.py`, `tests/hydrodynamics/diffraction/test_vessel_deck_builder.py`, `tests/hydrodynamics/diffraction/test_deck_validation.py`, `tests/hydrodynamics/diffraction/test_column_pontoon_deck.py`, `tests/hydrodynamics/diffraction/test_fleet_decks.py`, `tests/installation/test_installation_pamphlet.py`, `tests/contracts` and `tests/docs/test_digitalmodel_routing_contract.py`.

All 295 mapped implementation paths were checked for existence at the code pin; none was absent. [docs/registry/implementation-map.md:L20-L20]
The only strict registry parse finding is the pre-existing repeated `submodules` key under `solvers`; ordinary YAML parsing succeeds and parsed content outside the new `marine_ops.submodules.vessel_db` entry is unchanged. [docs/registry/module-routing.yaml:L303-L316]
