# Cross-section module knowledge pack

| Header | Value |
|---|---|
| Module | subsea/cross_sections |
| Repository | digitalmodel |
| Package | src/digitalmodel/subsea/cross_sections |
| Pack revision | 0 |
| Code revision | 309d343e08a2f83aebc6ea5344e586407b80c1f8 |
| Analysis family | Engineering calculations (EC); AQG r0 Python calculations |
| Quality manager role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The package validates cable, umbilical and rigid-pipeline cross-section definitions, loads and dumps fixtures, and renders deterministic Markdown, HTML and SVG reports. [src/digitalmodel/subsea/cross_sections/__init__.py:L1-L42]
The separate calculation interface computes homogeneous annular area and dry mass per axial length, and elementary equal-strain axial rigidity of bonded concentric layers. [src/digitalmodel/subsea/cross_sections/calculations.py:L19-L43] [src/digitalmodel/subsea/cross_sections/calculations.py:L63-L138]
The containing subsea domain is marked stable; this is a domain-level registry value, not an independent qualification of these calculations. [docs/registry/module-routing.yaml:L568-L573]
The annular calculation excludes strength, buoyancy, packed-component mechanics and design-code acceptance; the bonded calculation excludes unbonded flexible pipes, anisotropy, bond strength, Poisson constraints, bending and failure. [src/digitalmodel/subsea/cross_sections/calculations.py:L4-L8] [src/digitalmodel/subsea/cross_sections/calculations.py:L91-L110]

## 2. Inputs and outputs

### Inputs

| Input | Unit; allowed range | Default | Source and interpretation |
|---|---|---|---|
| UnitValue | Controlled unit string; finite scalar | Neither value nor unit defaults | Schema; field-specific positivity is separate. [src/digitalmodel/subsea/cross_sections/schema.py:L11-L72] |
| RadialLayer geometry | mm, m or inch; at least two of inner diameter, outer diameter, thickness; inner nonnegative, outer/thickness positive | Each geometry field may be absent | Three supplied dimensions must use the same unit and satisfy OD = ID + 2t within 1e-6 in that unit. Two-term derivation has the limitations in section 6. [src/digitalmodel/subsea/cross_sections/schema.py:L101-L182] |
| Layer density | kg/m³, positive when supplied | None | Required by calculate_annular_layer; no material lookup. [src/digitalmodel/subsea/cross_sections/schema.py:L150-L154] [src/digitalmodel/subsea/cross_sections/calculations.py:L26-L43] |
| PackedComponent | count at least 1; optional positive diameter/thickness in length units, pressure in bar/MPa, voltage in kV/V | Optional dimensions and ratings: None | No packing solution or tube-wall/diameter relationship is enforced by this validator. [src/digitalmodel/subsea/cross_sections/schema.py:L185-L224] |
| DesignMetadata | Nonnegative water depth in length units; temperature in degC; positive pressure in bar/MPa and voltage in kV/V | All optional values None; extra metadata empty | Metadata validation checks units and signs. [src/digitalmodel/subsea/cross_sections/schema.py:L227-L256] |
| CrossSectionDefinition | Nonblank id/name; controlled family and duty; declared provenance; radial or packed content required by family | Empty layer/component lists; default metadata | Source IDs on components/layers must be declared; radial neighbours must be contiguous within 1e-6 in their common length unit. [src/digitalmodel/subsea/cross_sections/schema.py:L28-L40] [src/digitalmodel/subsea/cross_sections/schema.py:L281-L350] |
| Provenance | Nonblank source_id; controlled source_type; calculated sources require derived_from | Optional citation, URL/path and note None; derived_from empty | A source-ID relationship is checked, not the truth of the cited source. [src/digitalmodel/subsea/cross_sections/schema.py:L23-L29] [src/digitalmodel/subsea/cross_sections/schema.py:L75-L98] [src/digitalmodel/subsea/cross_sections/schema.py:L303-L316] |
| BondedAxialLayer | All supplied geometry in m; finite positive modulus in Pa; independent modulus provenance | No modulus, unit or provenance defaults | Complete geometry/modulus pairs are sorted; interfaces must match exactly, unlike schema contiguity tolerance. [src/digitalmodel/subsea/cross_sections/calculations.py:L46-L77] [src/digitalmodel/subsea/cross_sections/calculations.py:L91-L138] |
| Fixture files / report CLI | JSON by suffix, otherwise YAML mapping; --format md/html/all | CLI format all; output directory docs/subsea/cross_sections | The loader delegates to schema validation; the CLI writes requested reports. [src/digitalmodel/subsea/cross_sections/io.py:L14-L50] [src/digitalmodel/subsea/cross_sections/cli.py:L12-L26] |

### Outputs

| Output | Unit / shape | Contract |
|---|---|---|
| AnnularLayerProperties | area_m2: m²; mass_per_length_kg_m: kg/m | Finite positive outputs, dry mass only. [src/digitalmodel/subsea/cross_sections/calculations.py:L19-L43] |
| BondedAxialProperties | axial_stiffness_n: N; area_m2: m²; equivalent_modulus_pa: Pa | EA is force per strain, not EA/L; area excludes the bore. [src/digitalmodel/subsea/cross_sections/calculations.py:L80-L88] |
| ValidationReport | is_valid, errors, warnings, summary | Errors include code/path/message/severity; caught validation failures return a false report. [src/digitalmodel/subsea/cross_sections/schema.py:L259-L278] [src/digitalmodel/subsea/cross_sections/validation.py:L16-L52] |
| Fixture serialization | JSON or YAML, explicit unit fields retained | None fields excluded; JSON sorted and newline terminated. [src/digitalmodel/subsea/cross_sections/schema.py:L349-L350] [src/digitalmodel/subsea/cross_sections/io.py:L31-L50] |
| Report / SVG | Markdown, HTML, SVG strings | Radial circles follow layer order; packed arrangements are deterministic schematics, not optimized manufacturing layouts. [src/digitalmodel/subsea/cross_sections/reporting.py:L24-L59] [src/digitalmodel/subsea/cross_sections/visualization.py:L23-L32] |

UNVERIFIED: a physical coordinate origin, signed orientation and phase convention are not established for interchange; the SVG coordinates describe a display schematic only. See section 10, issue pending (1). [src/digitalmodel/subsea/cross_sections/visualization.py:L35-L105]

## 3. Method and governing standards

| Method | Implementation and basis | Limits |
|---|---|---|
| Annular material area | _annular_geometry uses A = π(OD − ID)(OD + ID)/4; code cites OpenStax, Prealgebra 2e (2020), section 9.5, circle area and diameter relation. [src/digitalmodel/subsea/cross_sections/calculations.py:L46-L61] | Every supplied dimension must already be in m; require 0 ≤ ID < OD and finite positive area. [src/digitalmodel/subsea/cross_sections/calculations.py:L46-L61] |
| Dry mass per length | calculate_annular_layer uses m/L = ρA; code cites OpenStax, University Physics Volume 1 (2016), section 14.1, equation 14.1. [src/digitalmodel/subsea/cross_sections/calculations.py:L26-L43] | Caller supplies density and its provenance; no buoyancy or strength acceptance. [src/digitalmodel/subsea/cross_sections/calculations.py:L4-L8] [src/digitalmodel/subsea/cross_sections/calculations.py:L26-L43] |
| Fully bonded axial rigidity | bonded_axial_stiffness sums EᵢAᵢ; equivalent modulus is ΣEᵢAᵢ/ΣAᵢ; code derives the parallel-load relation from OpenStax, University Physics Volume 1 (2016), section 12.3. [src/digitalmodel/subsea/cross_sections/calculations.py:L80-L138] | Homogeneous, isotropic, linear elastic layers share uniform strain; no DNV-F202 qualification is claimed. [src/digitalmodel/subsea/cross_sections/calculations.py:L91-L110] |
| Schema validation | Pydantic models forbid extra fields, check units/provenance and enforce required radial or packed content; derived diameters use OD = ID + 2t. [src/digitalmodel/subsea/cross_sections/schema.py:L101-L182] [src/digitalmodel/subsea/cross_sections/schema.py:L281-L350] | Schema acceptance alone does not establish a valid calculation geometry; see section 6. [src/digitalmodel/subsea/cross_sections/calculations.py:L46-L61] |

The pipeline fixture names DNV-ST-F101, Submarine Pipeline Systems, through a registry provenance record; edition and clause title are absent in that record, and no clause acceptance calculation is implemented here. Clause text not checked against the licensed copy. [src/digitalmodel/subsea/cross_sections/fixtures/concrete_coated_pipeline.yml:L18-L21] [src/digitalmodel/subsea/cross_sections/calculations.py:L4-L8]
UNVERIFIED: the fixture's standards bibliographic locator and underlying vendor/wiki values have not been independently established; source-ID resolution tests are not source-content checks. See section 10, #1792. [tests/subsea/cross_sections/test_fixtures.py:L76-L85]
The calculation outputs carry numeric dataclasses rather than a Citation sidecar; geometry and modulus provenance remain input records. These elementary calculations do not introduce a standards-derived acceptance constant. [src/digitalmodel/subsea/cross_sections/calculations.py:L19-L43] [src/digitalmodel/subsea/cross_sections/calculations.py:L63-L88] [src/digitalmodel/subsea/cross_sections/calculations.py:L91-L110]

## 4. Validated worked examples and regression examples

The following independent references are elementary closed forms using synthetic inputs, not material specifications or design-code acceptance limits. [tests/subsea/cross_sections/test_calculations.py:L1-L10] [tests/subsea/cross_sections/test_bonded_layer_axial_stiffness.py:L4-L10]

| Case / classification | Independent source; input | Expected value and tolerance basis | Existing test node ID | Offline result at code pin |
|---|---|---|---|---|
| V1 annulus / validated | Circle subtraction and density relation; ID 2 m, OD 4 m, density 10 kg/m³; executable documentation setup | 3π m² and 30π kg/m; relative 1e-12 for floating-point rounding. [tests/subsea/cross_sections/test_calculations.py:L124-L165] | `tests/subsea/cross_sections/test_calculations.py::test_documented_radial_layer_example[annulus]` | BASE PASS; AFTER PASS |
| V2 solid core / validated | Circle area and density relation; ID 0 m, OD 2 m, density 10 kg/m³ | π m² and 10π kg/m; relative 1e-12 for rounding. [tests/subsea/cross_sections/test_calculations.py:L124-L165] | `tests/subsea/cross_sections/test_calculations.py::test_documented_radial_layer_example[solid-core]` | BASE PASS; AFTER PASS |
| V3 bonded pair / validated | Equal-strain parallel-load closed form; diameters 2/4/6 m; moduli 10/20 Pa | Area 8π m², EA 130π N, equivalent modulus 130/8 Pa; relative 1e-12. [tests/subsea/cross_sections/test_bonded_layer_axial_stiffness.py:L48-L57] | `tests/subsea/cross_sections/test_bonded_layer_axial_stiffness.py::test_bonded_layers_hand_stiffness` | BASE PASS; AFTER PASS |
| V4 identical modulus / validated | Partitioned solid disk, radius 3 m, modulus 10 Pa | Area 9π m², EA 90π N, equivalent modulus 10 Pa; relative 1e-12. [tests/subsea/cross_sections/test_bonded_layer_axial_stiffness.py:L60-L65] | `tests/subsea/cross_sections/test_bonded_layer_axial_stiffness.py::test_identical_modulus_limit` | BASE PASS; AFTER PASS |
| V5 single layer / validated | Radius 1 m solid disk, modulus 10 Pa | Area π m², EA 10π N, equivalent modulus 10 Pa; relative 1e-12. [tests/subsea/cross_sections/test_bonded_layer_axial_stiffness.py:L68-L73] | `tests/subsea/cross_sections/test_bonded_layer_axial_stiffness.py::test_single_layer_limit` | BASE PASS; AFTER PASS |
| R1 absent density / regression | Executable documentation with density omitted | Exact refusal message; 0 successful mass results. [tests/subsea/cross_sections/test_calculations.py:L124-L155] | `tests/subsea/cross_sections/test_calculations.py::test_documented_radial_layer_example[missing-density]` | BASE PASS; AFTER PASS |
| R2 fixture round trip / regression | Bundled YAML fixture, written to temporary YAML | 0 differences in family, first outer-diameter unit and provenance source IDs. [tests/subsea/cross_sections/test_fixtures.py:L88-L94] | `tests/subsea/cross_sections/test_fixtures.py::test_fixture_roundtrip_preserves_family_units_and_provenance` | BASE PASS; AFTER PASS |
| R3 report contract / regression | Bundled fixtures; Markdown and HTML renderers | 0 missing required headings/fixture IDs, deterministic repeated Markdown digest; schematic caveats present. [tests/subsea/cross_sections/test_reporting.py:L62-L87] | `tests/subsea/cross_sections/test_reporting.py::test_markdown_and_html_share_required_contract_without_machine_paths` | BASE PASS; AFTER PASS |

Recorded command: `PYTHONPATH=src uv run --no-sources python -m pytest tests/subsea/cross_sections -q -rA -p no:cacheprovider --basetemp=<scratch>`; BASE and AFTER use the header code revision, the uncommitted documentation tree for AFTER, and no addopts override. Results: each run 96 collected, 96 passed, 0 failed, 0 skipped, 0 NOT_RUN; the six test files define the calculation, fixture, reporting, schema and validation cases. [tests/subsea/cross_sections/test_calculations.py:L38-L168] [tests/subsea/cross_sections/test_bonded_layer_axial_stiffness.py:L48-L220] [tests/subsea/cross_sections/test_fixtures.py:L39-L99] [tests/subsea/cross_sections/test_reporting.py:L37-L140] [tests/subsea/cross_sections/test_schema.py:L23-L106] [tests/subsea/cross_sections/test_validation.py:L85-L185]

## 5. QA gates

Gate applicability follows AQG r0, section 10, Python calculations; G5 follows AQG r0, section 9. These are review requirements, not solver qualification.

| Gate | Applicability / route | Module-specific checks |
|---|---|---|
| G0 | Applicable: explicit inputs, units and provenance | C01, C02, C03, C04 |
| G1 | Applicable: Python imports and offline tests; the package writes reports and fixture files, not a solver deck | C05, C06 |
| G2 | Applicable: independent circle/density and equal-strain checks | C07, C08 |
| G3 | NOT_APPLICABLE: this interface has no time-domain solve. [src/digitalmodel/subsea/cross_sections/calculations.py:L19-L138] | No dynamics check |
| G4 | NOT_APPLICABLE: this interface has no diffraction solve or panel mesh output. [src/digitalmodel/subsea/cross_sections/calculations.py:L19-L138] [src/digitalmodel/subsea/cross_sections/visualization.py:L23-L32] | No diffraction check |
| G5 | Applicable: trace calculation results and preserve report limits; signed evidence export remains an open item | C09, C10 |

| Check ID | Executable check and named output | Numeric pass criterion and unit | Evidence / result |
|---|---|---|---|
| C01 / G0 | test_schema.py tests for missing/unknown units, nonfinite scalars and semantic density, ratings and metadata units | 0 accepted invalid inputs, count | 9 schema test cases PASS. [tests/subsea/cross_sections/test_schema.py:L23-L106] |
| C02 / G0 | test_validation.py geometry, family, count and provenance cases through RadialLayer and CrossSectionDefinition | 0 unexpectedly accepted invalid inputs; schema gap/overlap tolerance 1e-6 in common length unit | 11 validation cases PASS; this does not cover two-term schema defects. [tests/subsea/cross_sections/test_validation.py:L85-L185] [src/digitalmodel/subsea/cross_sections/schema.py:L332-L347] |
| C03 / G0 | `tests/subsea/cross_sections/test_fixtures.py::test_every_fixture_layer_provenance_resolves_to_known_source_page` | 0 undeclared layer/component source IDs, count | PASS; checks IDs only. [tests/subsea/cross_sections/test_fixtures.py:L76-L85] |
| C04 / G0 | test_bonded_layer_axial_stiffness.py parameter, missing-input, unit, provenance and mutated-input refusal tests | 0 accepted invalid inputs, count; modulus must be positive Pa; interfaces exact in m | PASS; snapshot revalidation is tested. [tests/subsea/cross_sections/test_bonded_layer_axial_stiffness.py:L85-L220] |
| C05 / G1 | Import through all six cross-section test files, then run the section-4 command | 96 tests passed; 0 failures, count | BASE/AFTER PASS; no solver interface imported. [tests/subsea/cross_sections/test_reporting.py:L9-L14] [tests/subsea/cross_sections/test_calculations.py:L18-L19] |
| C06 / G1 | `tests/subsea/cross_sections/test_reporting.py::test_cli_generates_requested_formats_atomically` and test_cli_format_filter variants | CLI exit 0; 2 output files for all, 1 file for a single format; 0 residual temporary files, count | PASS; load route is the package CLI and its schema parser. OrcaFlex export is excluded by the report generator. [tests/subsea/cross_sections/test_reporting.py:L90-L140] [src/digitalmodel/subsea/cross_sections/reporting.py:L118-L119] |
| C07 / G2 | V1/V2 plus annular arithmetic, missing density, non-SI geometry, negative bore and unrepresentable-result tests | Area m² and dry mass kg/m match closed forms to relative 1e-12; 0 invalid results accepted, count | 14 calculation cases PASS. [tests/subsea/cross_sections/test_calculations.py:L38-L168] |
| C08 / G2 | V3/V4/V5 plus bonded order-invariance and interface refusal tests | EA N, material area m² and equivalent modulus Pa match closed forms to relative 1e-12; 0 unequal interfaces accepted, count | 49 bonded cases PASS. [tests/subsea/cross_sections/test_bonded_layer_axial_stiffness.py:L48-L220] |
| C09 / G5 | R3 and `tests/subsea/cross_sections/test_reporting.py::test_report_model_loads_all_fixtures_in_deterministic_order` | 5 fixture IDs; 0 missing required headings or caveats; repeated Markdown has 0 digest differences, count | PASS; does not supply an AQG signature or input fingerprint. [tests/subsea/cross_sections/test_reporting.py:L37-L87] |
| C10 / G5 | R2: reload the temporary dump and compare explicit unit and provenance fields | 0 differences in the asserted fields, count | PASS for YAML; no comprehensive signed evidence or JSON round-trip test exists, issue pending (7). [tests/subsea/cross_sections/test_fixtures.py:L88-L94] |

## 6. Known failure modes and pitfalls

sweep: 43 found

The audit below includes guarded paths and benign defaults as well as defects; 43 is the count of enumerated review sites, not a count of swallowed exceptions. Detection references apply only to the behaviour actually asserted by the named tests. [src/digitalmodel/subsea/cross_sections/validation.py:L24-L52] [tests/subsea/cross_sections/test_validation.py:L173-L180]

Reproduce the textual sweep at the code pin; inspect every match in its enclosing definition and read the entire package for implicit fallbacks:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|default|unit|cache' src/digitalmodel/subsea/cross_sections --glob '*.py'
rg -n ' or |not specified|not applicable|default_factory|return 0|return 1' src/digitalmodel/subsea/cross_sections --glob '*.py'
```

| Site | Symptom / cause at code pin | Existing detection / disposition |
|---|---|---|
| S01 | Mixed two-term units can be arithmetically relabelled; equality of units is required only for a complete triplet. [src/digitalmodel/subsea/cross_sections/schema.py:L139-L148] | C07/C08 reject non-SI calculations; no detection check exists for schema acceptance, issue pending (2). |
| S02 | Deriving ID from OD and thickness can produce a negative bore accepted by the schema. [src/digitalmodel/subsea/cross_sections/schema.py:L163-L170] | C07 catches calculation refusal; no detection check exists for schema acceptance, issue pending (2). |
| S03 | Existing CrossSectionDefinition bypasses parsing in validate_cross_section; mutation can leave is_valid true. [src/digitalmodel/subsea/cross_sections/validation.py:L27-L30] | No detection check exists, issue pending (3). |
| S04 | dump_cross_section_fixture trusts an existing model before serialization. [src/digitalmodel/subsea/cross_sections/io.py:L36-L41] | C10 covers unmutated round trip only; no detection check exists for mutation, issue pending (3). |
| S05 | Annular mass reads density directly; a mutated unit can bypass construction checks. [src/digitalmodel/subsea/cross_sections/calculations.py:L34-L42] | C01 checks initial density units only; no detection check exists for this mutation, issue pending (3). |
| S06 | An explicit empty report path list silently selects bundled fixtures through `paths or fixture_paths()`. [src/digitalmodel/subsea/cross_sections/reporting.py:L45-L49] | C09 covers supplied bundled paths only; no detection check exists for empty input, issue pending (4). |
| S07 | Component maximum compares raw diameter values without converting allowed units. [src/digitalmodel/subsea/cross_sections/reporting.py:L277-L280] | No detection check exists for mixed-unit reports, issue pending (4). |
| S08 | Missing optional geometry takes continue; the independent two-dimension requirement still applies. [src/digitalmodel/subsea/cross_sections/schema.py:L124-L138] | C02 tests incomplete geometry refusal. |
| S09 | derived_inner_diameter returns None if derivation is impossible. [src/digitalmodel/subsea/cross_sections/schema.py:L163-L171] | C02 incomplete geometry; full definition rejects unmaterialized dimensions. [src/digitalmodel/subsea/cross_sections/schema.py:L334-L340] |
| S10 | derived_outer_diameter returns None if derivation is impossible. [src/digitalmodel/subsea/cross_sections/schema.py:L174-L182] | C02 incomplete geometry; guarded by definition materialization. [src/digitalmodel/subsea/cross_sections/schema.py:L334-L340] |
| S11 | Unit-set construction filters absent values before comparing units. [src/digitalmodel/subsea/cross_sections/schema.py:L156-L160] | C02 mixed radial units; two-term gap remains S01. |
| S12 | Global geometric tolerance is absolute in the supplied length unit. [src/digitalmodel/subsea/cross_sections/schema.py:L48-L48] [src/digitalmodel/subsea/cross_sections/schema.py:L143-L145] [src/digitalmodel/subsea/cross_sections/schema.py:L344-L346] | C02 covers finite gap/overlap; no detection check exists at the tolerance boundary, issue pending (2). |
| S13 | Provenance defaults derived_from to empty, but calculated sources must supply dependencies. [src/digitalmodel/subsea/cross_sections/schema.py:L85-L97] | C01 calculated-provenance refusal. |
| S14 | Extra design metadata defaults to empty dict; absence is permitted. [src/digitalmodel/subsea/cross_sections/schema.py:L237-L237] | C01 semantic metadata checks; no detection check exists for completeness, issue pending (7). |
| S15 | ValidationReport errors default to empty list; model construction does not couple it to is_valid. [src/digitalmodel/subsea/cross_sections/schema.py:L269-L278] | C02 checks the validator's failure report; no detection check exists for directly inconsistent reports, issue pending (7). |
| S16 | ValidationReport warnings default to empty list. [src/digitalmodel/subsea/cross_sections/schema.py:L277-L277] | No detection check exists for missing warnings, issue pending (7). |
| S17 | Default DesignMetadata permits absent basis quantities. [src/digitalmodel/subsea/cross_sections/schema.py:L291-L291] | No detection check exists for design-basis completeness, issue pending (7). |
| S18 | radial_layers defaults empty; applicable families subsequently require layers. [src/digitalmodel/subsea/cross_sections/schema.py:L292-L292] [src/digitalmodel/subsea/cross_sections/schema.py:L317-L323] | C02 unsupported-family/geometry coverage; no detection check exists for each omitted family-content combination, issue pending (7). |
| S19 | packed_components defaults empty; umbilical families subsequently require components. [src/digitalmodel/subsea/cross_sections/schema.py:L293-L293] [src/digitalmodel/subsea/cross_sections/schema.py:L324-L329] | No detection check exists for every omitted family-content combination, issue pending (7). |
| S20 | Error location/type/message use fallback strings when fields are absent. [src/digitalmodel/subsea/cross_sections/validation.py:L16-L21] | C02 report shape; no detection check exists for missing error-dictionary fields, issue pending (7). |
| S21 | ValidationError becomes a false report with errors and empty warnings; it is not swallowed as success. [src/digitalmodel/subsea/cross_sections/validation.py:L32-L39] | C02 stable failure report. |
| S22 | ValueError becomes a false report with one error and empty warnings. [src/digitalmodel/subsea/cross_sections/validation.py:L40-L46] | No detection check exists for this separate exception branch, issue pending (7). |
| S23 | Successful validation returns empty errors and warnings. [src/digitalmodel/subsea/cross_sections/validation.py:L47-L52] | C02 does not test this success-report branch directly; no detection check exists, issue pending (7). |
| S24 | Bonded summation catches OverflowError and raises ValueError; no zero fallback. [src/digitalmodel/subsea/cross_sections/calculations.py:L127-L137] | C08 unrepresentable total stiffness test. |
| S25 | CLI catches Exception, prints failure and returns 1. [src/digitalmodel/subsea/cross_sections/cli.py:L19-L23] | C06 covers successful exit only; no detection check exists for failure exit, issue pending (7). |
| S26 | CLI defaults output directory and format; successful completion returns 0. [src/digitalmodel/subsea/cross_sections/cli.py:L14-L26] | C06 format selection; no detection check exists for default-directory isolation, issue pending (7). |
| S27 | Fixture discovery ignores every non-.yml name. [src/digitalmodel/subsea/cross_sections/reporting.py:L38-L42] | C09 enumerates 5 bundled YAML fixtures; no detection check exists for externally supplied JSON discovery, issue pending (7). |
| S28 | Unknown family sorting falls back to rank 99. [src/digitalmodel/subsea/cross_sections/reporting.py:L199-L209] | Schema limits families; no detection check exists for this internal fallback, issue pending (7). |
| S29 | Provenance deduplication replaces absent text with empty strings for its key. [src/digitalmodel/subsea/cross_sections/reporting.py:L224-L237] | No detection check exists for None/empty equivalence, issue pending (7). |
| S30 | Missing radial outer envelope displays not specified. [src/digitalmodel/subsea/cross_sections/reporting.py:L273-L276] | Guarded by schema; no detection check exists for mutated input, issue pending (3). |
| S31 | No component diameter displays not specified. [src/digitalmodel/subsea/cross_sections/reporting.py:L277-L281] | No detection check exists for this branch, issue pending (7). |
| S32 | Absent ratings display not specified. [src/digitalmodel/subsea/cross_sections/reporting.py:L284-L299] | No detection check exists for all absent ratings, issue pending (7). |
| S33 | None UnitValue displays not specified; numbers round to three decimals and trim zeros. [src/digitalmodel/subsea/cross_sections/reporting.py:L302-L306] | No detection check exists for values rounded to zero, issue pending (7). |
| S34 | Markdown provenance replaces missing citation/path/note with not specified and empty derivation with not applicable. [src/digitalmodel/subsea/cross_sections/reporting.py:L309-L317] | No detection check exists for each fallback, issue pending (7). |
| S35 | HTML provenance has the same missing-data fallbacks. [src/digitalmodel/subsea/cross_sections/reporting.py:L320-L329] | No detection check exists for each fallback, issue pending (7). |
| S36 | Radial SVG maximum defaults to 1 and skips absent derived diameters. [src/digitalmodel/subsea/cross_sections/visualization.py:L35-L40] | Normal schema guards this path; no detection check exists for bypassed/mutated geometry, issue pending (4). |
| S37 | Radial SVG substitutes max_outer for absent outer diameter and clips small display radii to 2. [src/digitalmodel/subsea/cross_sections/visualization.py:L43-L45] | Schematic only; no detection check exists for clipping, issue pending (4). |
| S38 | Packed SVG uses max(1, component-list length) to avoid zero division. [src/digitalmodel/subsea/cross_sections/visualization.py:L60-L76] | No detection check exists for an empty schematic, issue pending (4). |
| S39 | Missing component diameter yields radius 11; supplied raw diameter is divided by 4 and clipped to 7–20 display units. [src/digitalmodel/subsea/cross_sections/visualization.py:L92-L95] | Schematic only; no detection check exists for unit conversion or clipping, issue pending (4). |
| S40 | Calculations explicitly require m, kg/m³ and Pa; no implicit length or modulus conversion is performed. [src/digitalmodel/subsea/cross_sections/schema.py:L150-L154] [src/digitalmodel/subsea/cross_sections/calculations.py:L46-L77] | C01/C04/C07 cover constructed-input unit refusals; S05 remains open. |
| S41 | PackedComponent checks individual positive dimensions but not tube wall versus diameter or consistent length units. [src/digitalmodel/subsea/cross_sections/schema.py:L208-L224] | No detection check exists for physical tube geometry, issue pending (2). |
| S42 | write_report_files returns an empty generated list for an empty or unsupported format set without an input error. [src/digitalmodel/subsea/cross_sections/reporting.py:L177-L185] | C06 covers CLI-controlled valid formats only; no detection check exists for direct invalid formats, issue pending (4). |
| S43 | _atomic_write removes a surviving temporary path in finally; filesystem errors still propagate. [src/digitalmodel/subsea/cross_sections/reporting.py:L188-L196] | C06 checks successful cleanup only; no detection check exists for write/replace/cleanup failures, issue pending (7). |

No explicit external-text int()/float() conversion, empty-result cache write, epoch fallback or percentage default was found by the recorded sweep and full-file read; numeric text coercion is delegated to Pydantic and file parsers. [src/digitalmodel/subsea/cross_sections/schema.py:L51-L72] [src/digitalmodel/subsea/cross_sections/io.py:L14-L28]

Additional pitfalls: the report text describes #2516 as deferred flexible-pipe mechanics, and fixture provenance describes #2514 as representative assumptions; those issue bodies currently concern other generator defects, so these links are not reliable engineering evidence, issue pending (6). No detection check exists. Current issue bodies: digitalmodel, #2516, “docs(orcaflex): resolve generator standards citation metadata”, and #2514, “feat(orcaflex): validate starting shapes against their line geometry”, inspected 2026-11-07. [src/digitalmodel/subsea/cross_sections/reporting.py:L111-L119] [src/digitalmodel/subsea/cross_sections/reporting.py:L165-L171] [src/digitalmodel/subsea/cross_sections/fixtures/66kv_inter_array_cable.yml:L16-L19]
The issues found through module-name searches are catalogue and audit leads, not proof of an incident in this package; #1792 covers source cataloguing, #1793 covers flexible-pipe follow-up, and #1817 covers the wider audit. The documented arithmetic examples explicitly describe their limited contribution to #1817. [docs/examples/radial-layer-area-and-mass.md:L36-L40]

## 7. Lessons log

| Date | Record | Lesson / pack change | Evidence | Shared-log entry |
|---|---|---|---|---|
| 2026-11-07 | Initial knowledge pack | Keep schema acceptance, calculation guards and schematic reporting limits distinct; record independently derived examples separately from regression contracts | [src/digitalmodel/subsea/cross_sections/schema.py:L122-L182] [src/digitalmodel/subsea/cross_sections/calculations.py:L46-L61] [tests/subsea/cross_sections/test_reporting.py:L62-L87] | Not assigned; issue pending (7) |

## 8. Ownership

Review routing designated by this pack: engineering calculations quality manager; deputy: engineering calculations deputy quality manager. Producing role: calculation and documentation contributors. Escalate unresolved method classification, tolerances, standards locators and missing basis data to the family quality manager; the maintainer handles cross-package routing. This is a review requirement, not a claim of completed approval.

## 9. Dependencies and other implementations

### Dependency inventory

| Dependency / consumer | Relationship and version boundary |
|---|---|
| Pydantic | BaseModel, ConfigDict, Field and validators implement the schema; declared runtime range is >=2.7.0,<3.0.0. [src/digitalmodel/subsea/cross_sections/schema.py:L5-L8] [pyproject.toml:L61-L61] |
| ruamel.yaml and JSON | YAML fixture parsing/dumping and JSON serialization; declared YAML runtime range >=0.18.0,<1.0.0. [src/digitalmodel/subsea/cross_sections/io.py:L5-L28] [src/digitalmodel/subsea/cross_sections/io.py:L31-L50] [pyproject.toml:L72-L72] |
| Bundled public fixtures | Five fixture filenames are enumerated by tests; representative data support schema/report regression, not independent design acceptance. [tests/subsea/cross_sections/test_fixtures.py:L12-L18] [src/digitalmodel/subsea/cross_sections/fixtures/66kv_inter_array_cable.yml:L10-L19] |
| Reporting and CLI | build_report_model loads fixture files, validates schema and creates SVG; write_report_files renders Markdown/HTML. [src/digitalmodel/subsea/cross_sections/reporting.py:L45-L59] [src/digitalmodel/subsea/cross_sections/reporting.py:L177-L196] |
| OrcaFlex, source mention 1 | reporting.py line 119 explicitly excludes export; this is a prospective downstream dependency, not a solver call. [src/digitalmodel/subsea/cross_sections/reporting.py:L118-L119] |
| OrcaFlex, source mention 2 | reporting.py line 171 repeats the exclusion in HTML output; no solver version is specified there. [src/digitalmodel/subsea/cross_sections/reporting.py:L170-L171] |

The exact case-sensitive source search `rg -n 'OrcaFlex' src/digitalmodel/subsea/cross_sections --glob '*.py'` finds the two reporting strings listed above; no solver interface is required by the six offline test files. [src/digitalmodel/subsea/cross_sections/reporting.py:L119-L119] [src/digitalmodel/subsea/cross_sections/reporting.py:L171-L171] [tests/subsea/cross_sections/test_calculations.py:L18-L19] [tests/subsea/cross_sections/test_reporting.py:L9-L14]

### Implementation-map inventory and counted-family audit

Map reference: `origin/codex/implmap-work`; map document commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; map data revision `270c8759c374b67a7f032953e7fcdb14e0224461`. Map citations below explicitly identify that separate document commit; every other repository citation uses the header code pin. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L1-L3]

Audit procedure: parse the map's `families` sequence without sorting; select each family's `implementations` rows whose path begins `src/digitalmodel/subsea/cross_sections/`; rank by descending selected-row count with original family order for ties. The computed inventory is empty: 0 module map families and 0 in-package implementation rows. Consequently there are 0 counted families; the exact-signal grep is `NOT_APPLICABLE` because no map family has an implementation row in the package. No signal exclusion or enclosing-definition assignment is needed for an empty family set. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L345-L26502]

Eight schema/validation names occur under `rejected_candidates`, with disposition `needs_review` and an explicit exclusion from implementation counts; they are name matches for irregular-frequency vocabulary, not implementation rows or irregular-frequency calculations. [docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L44583-L44639] [src/digitalmodel/subsea/cross_sections/schema.py:L122-L182] [src/digitalmodel/subsea/cross_sections/validation.py:L24-L52]

Map follow-up: add annular area/dry-mass, bonded equal-strain axial rigidity and radial-schema geometry families first, issue pending (5); they are not counted at this map revision. Section 3 lists the in-package methods and their helpers. [src/digitalmodel/subsea/cross_sections/calculations.py:L26-L61] [src/digitalmodel/subsea/cross_sections/calculations.py:L93-L138] [src/digitalmodel/subsea/cross_sections/schema.py:L122-L182]

### Repository-wide supplementary search

```sh
git grep -n -E 'annular|bonded_axial_stiffness|equivalent_modulus|pi[[:space:]]*\*.*(outer|diameter)|EA[[:space:]]*=.*sum' 309d343e08a2f83aebc6ea5344e586407b80c1f8 -- '*.py'
```

This search includes scripts and all tracked Python files, beyond the package-only counted-family procedure; the independently computing examples below are map gaps, not counted-family misses. [src/digitalmodel/subsea/jumper_tools/jumper_section_properties.py:L72-L105] [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L140-L160]

| Alternative / helper | Classification and reason |
|---|---|
| jumper_tools.calculate_section_properties | Independent annular areas, dry mass and axial rigidity, with a broader coated/filled/submerged model. [src/digitalmodel/subsea/jumper_tools/jumper_section_properties.py:L54-L105] |
| flexible_pipes layer annular_area, mass_per_length and axial_stiffness | Independent formulas; axial stiffness includes a lay-angle factor and is outside the bonded model scope. [src/digitalmodel/subsea/flexible_pipes/cross_section.py:L140-L160] |
| on_bottom_stability.e305_simplified | Independent annular area helper. [src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py:L76-L82] |
| In-package _annular_geometry | Shared computation for both calculate_annular_layer and bonded_axial_stiffness; already listed in section 3. [src/digitalmodel/subsea/cross_sections/calculations.py:L34-L36] [src/digitalmodel/subsea/cross_sections/calculations.py:L46-L61] [src/digitalmodel/subsea/cross_sections/calculations.py:L114-L119] |
| In-package output dataclasses, report renderers and serializers | Data holders or presentation helpers, not further independent implementations of the area/mass or bonded stiffness result. [src/digitalmodel/subsea/cross_sections/calculations.py:L19-L24] [src/digitalmodel/subsea/cross_sections/calculations.py:L80-L90] [src/digitalmodel/subsea/cross_sections/reporting.py:L240-L306] [src/digitalmodel/subsea/cross_sections/io.py:L31-L50] |

The supplementary search is a discovery record; it does not claim an exhaustive classification of every repository-wide geometric formula, issue pending (5). [src/digitalmodel/subsea/jumper_tools/jumper_section_properties.py:L54-L105]

## 10. Coverage and open items

### Reading and test coverage

| Material read | Coverage at code pin |
|---|---|
| Package Python files | __init__.py, schema.py, validation.py, io.py, calculations.py, visualization.py, reporting.py and cli.py; full-file reading, including all subfolders; fixtures contain YAML rather than additional Python. [src/digitalmodel/subsea/cross_sections/__init__.py:L1-L42] [src/digitalmodel/subsea/cross_sections/schema.py:L1-L350] [src/digitalmodel/subsea/cross_sections/validation.py:L1-L52] [src/digitalmodel/subsea/cross_sections/io.py:L1-L50] [src/digitalmodel/subsea/cross_sections/calculations.py:L1-L138] [src/digitalmodel/subsea/cross_sections/visualization.py:L1-L105] [src/digitalmodel/subsea/cross_sections/reporting.py:L1-L329] [src/digitalmodel/subsea/cross_sections/cli.py:L1-L30] |
| Bundled fixtures | 220kv_hvac_export_cable.yml, 66kv_inter_array_cable.yml, steel_tube_electro_hydraulic_umbilical.yml, power_optical_hybrid_umbilical.yml and concrete_coated_pipeline.yml; all read as representative schema data. [tests/subsea/cross_sections/test_fixtures.py:L12-L18] |
| Existing module tests | tests/subsea/cross_sections/{test_bonded_layer_axial_stiffness,test_calculations,test_fixtures,test_reporting,test_schema,test_validation}.py; 96 cases in the recorded run; no package-local test folder was found. [tests/subsea/cross_sections/test_bonded_layer_axial_stiffness.py:L48-L220] [tests/subsea/cross_sections/test_calculations.py:L38-L168] [tests/subsea/cross_sections/test_fixtures.py:L39-L99] [tests/subsea/cross_sections/test_reporting.py:L37-L140] [tests/subsea/cross_sections/test_schema.py:L23-L106] [tests/subsea/cross_sections/test_validation.py:L85-L185] |
| Direct documentation | docs/subsea/cross_sections/offshore_cross_section_report.md and .html; docs/api/subsea/offshore-cross-section-report.html; docs/subsea/cross_sections/bonded_axial_stiffness.md; docs/examples/radial-layer-area-and-mass.md. [docs/subsea/cross_sections/offshore_cross_section_report.md:L1-L8] [docs/subsea/cross_sections/offshore_cross_section_report.html:L1-L12] [docs/api/subsea/offshore-cross-section-report.html:L1-L12] [docs/subsea/cross_sections/bonded_axial_stiffness.md:L1-L5] [docs/examples/radial-layer-area-and-mass.md:L1-L6] |
| Other module-name documentation matches | docs/plans/2026-05-05-issue-488-umbilicals-control-systems.md; docs/plans/2026-05-05-issue-485-subsea-manifold-aggregation.md; docs/plans/2026-05-05-issue-494-flexible-pipe-modeling.md; docs/domains/umbilical/sizing-and-lay-analysis.md. These describe adjacent work, not implemented cross-section functions. [docs/plans/2026-05-05-issue-488-umbilicals-control-systems.md:L10-L14] [docs/plans/2026-05-05-issue-485-subsea-manifold-aggregation.md:L10-L12] [docs/plans/2026-05-05-issue-494-flexible-pipe-modeling.md:L10-L14] [docs/domains/umbilical/sizing-and-lay-analysis.md:L32-L35] |

UNVERIFIED: 1 discovery file with an identifying name is withheld from the public reading-path list; codenaming remains open, issue pending (1).

### Open-item register

Every row below is assigned to the engineering calculations maintainers; draft references denote issue text prepared for creation, not an existing issue number.

| Item | Evidence / bounded limitation | Tracking |
|---|---|---|
| O1 conventions and disclosure | UNVERIFIED convention statement in section 2 and withheld discovery path in this section; define physical interchange conventions if such an interface is added. [src/digitalmodel/subsea/cross_sections/visualization.py:L35-L105] | issue pending (1) |
| O2 radial and tube schema | Two-term units, negative derived bore, tolerance-boundary coverage and tube-wall consistency; S01/S02/S12/S41. [src/digitalmodel/subsea/cross_sections/schema.py:L122-L182] [src/digitalmodel/subsea/cross_sections/schema.py:L208-L224] | issue pending (2) |
| O3 mutable objects | Validation, dump and annular-calculation boundary revalidation; S03/S04/S05/S30. [src/digitalmodel/subsea/cross_sections/validation.py:L27-L30] [src/digitalmodel/subsea/cross_sections/io.py:L36-L41] [src/digitalmodel/subsea/cross_sections/calculations.py:L34-L42] | issue pending (3) |
| O4 report fallbacks | Empty selection, mixed-unit comparison, display defaults and radius clipping; S06/S07/S36–S39. [src/digitalmodel/subsea/cross_sections/reporting.py:L45-L49] [src/digitalmodel/subsea/cross_sections/reporting.py:L277-L280] [src/digitalmodel/subsea/cross_sections/visualization.py:L35-L95] | issue pending (4) |
| O5 map coverage | Add the section-3 method families and classify supplementary discoveries before counting them. [src/digitalmodel/subsea/cross_sections/calculations.py:L26-L138] | issue pending (5) |
| O6 historical issue references | Source text uses links that do not substantiate their descriptions; do not use them as method authority. [src/digitalmodel/subsea/cross_sections/reporting.py:L111-L119] [src/digitalmodel/subsea/cross_sections/fixtures/66kv_inter_array_cable.yml:L16-L19] | issue pending (6) |
| O7 QA evidence and branch coverage | G5 signature/fingerprint record, missing-data formatting, exception paths, JSON round-trip and log linkage remain unimplemented or untested; section 6 marks the individual detection gaps. [src/digitalmodel/subsea/cross_sections/reporting.py:L24-L35] [tests/subsea/cross_sections/test_fixtures.py:L88-L94] | issue pending (7) |
| O8 source basis | UNVERIFIED source-content and standards bibliographic locator statement in section 3; do not qualify representative fixtures as independently validated. [src/digitalmodel/subsea/cross_sections/fixtures/concrete_coated_pipeline.yml:L18-L21] [tests/subsea/cross_sections/test_fixtures.py:L76-L85] | #1792 |
| O9 wider mechanics / audit | Closed-form examples establish this elementary scope only; broader flexible-pipe mechanics and wider coverage remain separate. [src/digitalmodel/subsea/cross_sections/calculations.py:L108-L110] [docs/examples/radial-layer-area-and-mass.md:L36-L40] | #1793; #1817 |

No private sidecar is read or produced for this pack; the examples use synthetic arithmetic inputs and the public fixture caveats remain explicit. [tests/subsea/cross_sections/test_calculations.py:L8-L10] [tests/subsea/cross_sections/test_bonded_layer_axial_stiffness.py:L4-L10] [src/digitalmodel/subsea/cross_sections/reporting.py:L111-L119]
