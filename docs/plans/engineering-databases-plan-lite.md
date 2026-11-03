# Engineering databases plan-lite

## Summary

Four catalogues will separate reusable engineering inputs from calculation code:
line pipe, flexible pipe, structural section properties and pipe-soil stiffness.
The initial delivery will contain schemas, empty production files, citation
contracts and validated loaders. Invented test fixtures will exercise the
contracts; engineering values will require a later source and suitability review.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-26 | Define catalogue boundaries, units and citation checks before population. |
| 2 | 2026-10-26 | Specify citation URI validation, the numerical precision basis, soil interpretation limits and validation-error handling. |

Table 1. Plan issue history.

## Existing components and boundaries

- `src/digitalmodel/materials/line_pipe.py` and `materials/database.py` will
  remain the existing geometry and grade interfaces. Their reference tables will
  not be imported into this clean-source catalogue without independent review.
- `src/digitalmodel/subsea/flexible_pipes/` will remain calculation code. A
  separate product-property catalogue will supply future reviewed inputs.
- `data/riser_database/` will retain its current configuration, stackup and
  reference tables. A `structure_properties/` child will extend that database
  with reusable bare-section properties beyond drilling risers. Existing table
  manifests will retain their current scope; the child will have its own schema.
- `src/digitalmodel/geotechnical/database.py` will remain the general soil
  parameter interface. The new soil catalogue will address pipe resistance curves
  and stiffness, including the context absent from isolated seabed model inputs.
  Existing parameters and embedded seabed values will require fresh source review
  before reuse; they will not be promoted automatically.

No calculation consumer will switch inputs in this delivery. Future adapters will
select records explicitly and preserve their citations, conditions and units.

## Shared file and loader contract

Each catalogue directory will contain `schema.json` (JSON Schema 2020-12),
`data.json` and `sources.yml`. The JSON envelope will require `schema_version: 1`,
the database identifier, an exact `units` mapping and a `records` array.
Unknown fields and missing fields will be rejected. Empty arrays will be valid.
All numeric fields will require finite JSON numbers; strings and booleans will
not substitute for numbers. Unit labels will be exact; conversion will occur
explicitly during future population, with its basis recorded in the citation.

Every row will require `id`, `status` (`cited` or `synthetic`) and `citation_id`.
IDs will be unique within a catalogue. `sources.yml` will declare
`citation_schema: "schema.json#/$defs/citation"` and hold a `citations` mapping.
Each reference will resolve; unused citations and duplicate JSON or YAML keys
will be rejected. A separate citation entry will identify each distinct value
locator. Several rows from the same table may share a citation.

The citation schema will require all of these keys:

| Field | Meaning and validation |
| --- | --- |
| `title`, `publisher` | Nonblank public bibliographic text. |
| `url`, `catalogue_number` | At least one nonblank locator; a URL will use HTTP or HTTPS and include a valid host and optional numeric port. URI syntax checks will run without optional format packages. User information, scoped IP addresses and future IP-literal syntax will be excluded; ordinary IPv6 literals will be validated explicitly. The unused field may be null. |
| `revision` | Nonblank edition or revision; `not stated` will be explicit when absent in the source. |
| `date` | Valid ISO calendar date of the source, quoted in YAML. An unavailable source date will defer admission. |
| `page`, `table` | At least one nonblank exact value locator; the unused field may be null. |
| `source_class` | One of the permitted classes below, or `synthetic_fixture` for tests only. |
| `reuse_terms` | Nonblank record of the terms and permission basis for publishing the extracted values. |

Table 2. Mandatory citation metadata per referenced row.

Permitted engineering source classes will be `mill_catalogue`,
`manufacturer_catalogue`, `public_paper`, `public_thesis`,
`public_agency_report` and `own_design_basis`. Standards will never be sources.
A catalogue or paper that merely reproduces a standard table will not establish
an independent clean source for that table. Public access alone will not establish
redistribution permission. An internal design basis will be admitted only after
the maintainer confirms that its generic content and citation can be public;
private identifiers and private source documents will remain outside these files.
Synthetic metadata will identify invented test data and will make no claim of
external publication.

The API will be provided by
`digitalmodel.data_systems.engineering_databases`:

```python
from digitalmodel.data_systems.engineering_databases import (
    load_database,
    load_line_pipe,
    load_flexible_pipe,
    load_structure_properties,
    load_soil_stiffness,
)

rows = load_line_pipe("data/line_pipe")
rows = load_database("soil_stiffness", "data/soil_stiffness")
```

All five functions will accept an explicit catalogue directory. The four named
wrappers will select the matching schema. The directory will supply the reviewed
schema together with its data; schema changes will require review alongside data
changes. Successful reads will return a list of row dictionaries with a resolved
`citation` mapping, preserving input order and numeric units. Errors will raise
`CatalogueValidationError`; no values will be guessed or defaulted. Repository
data will not be bundled into the installed wheel in this delivery; installed
callers will supply a separately retained catalogue directory.

`allow_synthetic=True` will permit invented fixtures only when explicitly passed.
By default, synthetic rows will be rejected. Row status and citation source class
will agree even when this option is enabled. Schema and consistency checks will
not authenticate a source, enforce publication rights or qualify design inputs;
those decisions will remain part of population review.

## Line pipe

Scope will be nominal bare-pipe geometry and specified product strength for a
cited grade and product entry. Coating, tolerances, corrosion allowance, measured
strength, temperature derating and design factors will be outside this schema.

Directory: `data/line_pipe`; API: `load_line_pipe(directory)`.

| Fields | Units | Validation |
| --- | --- | --- |
| `od_m`, `wt_m` | m | Positive; twice wall thickness will be less than OD. |
| `grade` | text | Nonblank source designation; designation alone will not establish qualification. |
| `smys_mpa`, `smts_mpa` | MPa | Positive; SMTS will be at least SMYS. Values will be specified minima, not measured coupon results. |

Table 3. Line-pipe row fields in addition to shared metadata.

Clean source classes will be mill or manufacturer catalogues, public papers or
theses, public agency reports and publishable internal design bases. The first
population batch will target one independently published mill product catalogue,
with 20 nominal size/wall/grade entries and their stated minima. Source selection,
revision, exact pages and reuse permission will be confirmed before extraction;
no particular catalogue is yet qualified. Estimated effort will be 2 engineering
days including dual entry comparison and review. Missing strength provenance will
defer a row rather than trigger lookup in a standard.

## Flexible pipe

Scope will be equivalent whole-product properties under cited conditions, rather
than layer designs or a reconstruction of proprietary construction details.
Operating envelopes will not be inferred from geometry.

Directory: `data/flexible_pipe`; API: `load_flexible_pipe(directory)`.

| Fields | Units | Validation |
| --- | --- | --- |
| `id_m`, `od_m` | m | Positive; ID will be less than OD. |
| `axial_stiffness_n` | N | Positive equivalent axial rigidity EA. |
| `bending_stiffness_n_m2`, `torsional_stiffness_n_m2` | N*m^2 | Positive equivalent EI and GJ under stated conditions. |
| `dry_mass_kg_per_m` | kg/m | Positive dry product mass per unit length. |
| `minimum_bend_radius_m` | m | Positive; the operating or installation condition will be identified in `test_conditions`. |
| `design_pressure_pa` | Pa | Nonnegative cited internal differential design pressure. |
| `temperature_deg_c` | degC | Finite and above absolute zero; condition for the cited properties. |
| `stiffness_basis` | text | `initial_tangent`, `secant_tension` or `linearised`. |
| `test_conditions` | text | Nonblank description of pressure, temperature, tension, bend regime and applicability, as available from the source. |

Table 4. Flexible-pipe row fields in addition to shared metadata.

Clean source classes will be manufacturer or mill catalogues, public papers or
theses, public agency reports and publishable internal design bases. The first
population batch will seek one open full-product experimental paper or thesis
with 3 fully specified condition records. Manufacturer literature will be an
alternative only if it publishes every required property and condition. Source
selection and permissions will precede entry; incomplete brochures will remain
out of the catalogue. Estimated effort will be 3 engineering days. A single row
will not be treated as a load-independent stiffness law or used beyond its cited
conditions. Coupled stiffness matrices and layer properties will need a later
schema revision.

## Structure properties

Scope will extend the riser database with generic uniform bare cross-sections:
tubular, rectangular hollow, I-shaped and explicitly described custom sections.
The fields will describe section properties rather than complete assemblies,
buoyancy modules, joints, coatings or fatigue design inputs.

Directory: `data/riser_database/structure_properties`;
API: `load_structure_properties(directory)`.

| Fields | Units | Validation |
| --- | --- | --- |
| `section_type` | text | `circular_tube`, `rectangular_hollow`, `i_section` or `custom`. |
| `section_label` | text | Nonblank public section designation; local y and z axes will be identified by the cited table. |
| `area_m2` | m^2 | Positive material cross-sectional area. |
| `iy_m4`, `iz_m4` | m^4 | Positive centroidal second moments about the cited local axes. |
| `torsion_constant_m4` | m^4 | Positive Saint-Venant torsion constant; a polar moment will not substitute for open sections. |
| `elastic_modulus_pa` | Pa | Positive elastic modulus for the cited material and conditions. |
| `density_kg_per_m3` | kg/m^3 | Positive bare material density. |
| `dry_mass_kg_per_m` | kg/m | Positive and equal to area times density within relative tolerance `1e-6`. |

Table 5. Structural-section row fields in addition to shared metadata.

Clean source classes will be mill or manufacturer catalogues, public papers or
theses, public agency reports and publishable internal design bases. The first
population batch will target a publishable internal generic section design basis
with 12 analytically generated tubular and rectangular sections, independent
formula checks and a public calculation locator. The basis will be issued and
reviewed before any rows are admitted. Estimated effort will be 2 engineering
days. Rounded manufacturer tabulations that cannot satisfy the mass consistency
rule will require an explicit future precision policy, rather than silent changes
to stated values or weakened checks.

## Soil stiffness and pipe-soil interaction

Scope will be positive resistance-magnitude curves per unit pipe length versus
displacement for one direction and one set of soil, geometry and loading
conditions. Cyclic records will be envelopes, not full hysteresis loops. These
records will not be universal soil constants or automatic replacements for model
seabed inputs.

Directory: `data/soil_stiffness`; API: `load_soil_stiffness(directory)`.

| Fields | Units | Validation |
| --- | --- | --- |
| `soil_type` | text | `clay`, `sand`, `silt` or `mixed`. |
| `drainage` | text | `drained`, `undrained` or `partial`. |
| `loading` | text | `monotonic` or `cyclic_envelope`. |
| `direction` | text | `axial`, `lateral` or `vertical`. |
| `pipe_od_m`, `embedment_m` | m | OD positive; embedment nonnegative, measured below the original seabed to the pipe invert. |
| `depth_top_m`, `depth_bottom_m` | m | Nonnegative and ordered, relative to the original seabed; equal bounds will represent a single sampling depth. |
| `displacement_m` | m | At least two nonnegative, strictly increasing displacement magnitudes, starting at zero. |
| `resistance_n_per_m` | N/m | Same length as displacement; nonnegative magnitudes starting at zero. Post-peak softening will be permitted. |
| `initial_secant_stiffness_n_per_m2` | N/m^2 | Positive; resistance at the first nonzero displacement divided by that displacement, within relative tolerance `1e-6`. A zero-resistance first segment will be rejected. |
| `test_conditions` | text | Nonblank test method, stress and strength state, drainage, rate or cycle history, and applicability limits from the source. |

Table 6. Pipe-soil row fields in addition to shared metadata.

Clean source classes will be public papers or theses, public agency reports,
mill or manufacturer test catalogues and publishable internal design bases.
The first population batch will target one open pipe-soil laboratory paper or
thesis with 6 curves, retaining geometry, embedment, soil state and test conditions.
Source selection and exact table or figure locators will precede extraction;
figure digitisation will record the method and uncertainty in `test_conditions`.
Estimated effort will be 4 engineering days including independent digitisation
comparison. Nodal spring stiffness will require multiplication by a documented
tributary length in a future adapter. N/m and N/m^2 will not be interchanged.
Site-specific application, signed unloading, suction and hysteresis will need
additional qualification and potentially a schema revision.

## Numerical validation basis

The two consistency checks will apply one shared catalogue precision policy,
rather than a material, manufacturing or design tolerance. The relative limit
will be one part per million, with no absolute allowance. The comparison will
follow [PEP 485 (2015), Proposed Implementation](https://peps.python.org/pep-0485/#proposed-implementation):
the absolute difference will not exceed `1e-6` times the larger magnitude.
Strictly positive schema fields will make an absolute allowance unnecessary;
small values will retain the same relative check.

Derived values will be computed from the operands as stored in the row, including
analytically generated section areas; only the derived result will then be rounded
to at least seven significant decimal digits. Operand source precision will be
retained, but operand rounding before storage will not be charged to this internal
consistency budget. For `x = c * 10^e`, with `1 <= c < 10`, the last retained place
will be `10^(e-6)` and half a place will be `0.5 * 10^(e-6)`. Dividing by
`x >= 10^e` will bound that rounding contribution by `5e-7`. The `1e-6` policy
will provide an equal remaining margin. This will be a declared catalogue
precision budget, not a value prescribed by PEP 485 or a claim about source
accuracy. Independently rounded operands, measured uncertainty or coarser tables
will require a separately reviewed precision basis before population; passing
this numerical check alone will not establish engineering suitability.

For the synthetic section, `0.01 m^2 * 1000 kg/m^3` will give `10 kg/m`.
The acceptance tests will bracket the limit using deviations of `0.000009`
and `0.000011 kg/m`, on both sides, and will repeat the scale check near
`1e-11 kg/m`. For the synthetic soil segment, `10 N/m / 0.01 m` will give
`1000 N/m^2`; deviations of `0.0009` and `0.0011 N/m^2` will bracket the limit.
These will be invented numerical controls, not qualified engineering data.
The regression brackets will detect a tolerance drift beyond 0.9 to 1.1 times
the declared limit; they will not resolve changes inside that interval.

The temperature bound will follow the freely available BIPM
[The International System of Units (SI), 9th edition, v4.01 (2026)](https://doi.org/10.59161/AUEZ1291),
section 2.3.1, "The kelvin", page 129. The Celsius relation will give
`t/degC = T/K - 273.15`. This catalogue will require strictly positive
thermodynamic temperature; therefore `temperature_deg_c > -273.15` will apply.
Tests will cover below, at and above this bound. This unit definition will
support a validation limit only; standards-derived product values will remain
excluded from the catalogue, and service-temperature qualification will remain
outside this check.

## Verification and acceptance

Tests will establish that each schema is valid and each empty catalogue loads.
Synthetic fixtures will exercise required citation fields, exact units, citation
joins, source classes, dates, duplicate keys and IDs, numeric type and finiteness,
synthetic opt-in, and the engineering relationships listed above. Positive tests
will cover every permitted source class using invented metadata, without claiming
that those fixtures are engineering evidence.

File-level checks will cover the repository structure, engineering writing
register, JSON and YAML parsing, whitespace, conflict markers, source compilation,
licence headers, file size, LFS attributes and publication text restrictions.
The production files will retain zero rows at acceptance. Source selection,
redistribution review, population, consumer adapters and engineering qualification
will remain explicit follow-up work; effort and row counts above will be planning
estimates, not measured source availability.
