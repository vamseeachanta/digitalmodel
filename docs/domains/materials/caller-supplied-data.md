# Material and line-pipe data hygiene analysis

The embedded dimensions, material properties and legacy strength snapshots are
removed. No engineering row is re-added or re-sourced. Numerical inputs require
caller-owned data with an adjacent `sources.yml`; test inputs are synthetic and
are rejected unless test use is explicitly enabled.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 1 | 2026-10-26 | Removed standards-derived tables, required sourced caller inputs and replaced reference-table tests with synthetic fixtures. |

## Removed input inventory

This inventory identifies every removed row and its former stated source.
Numerical table contents are deliberately not reproduced. The previous source
is retained in Git history; this inventory does not qualify it for reuse.

### Dimensions and weight

`line_pipe.py` attributed its outside diameters and schedule walls to ASME
B36.10M, *Welded and Seamless Wrought Steel Pipe*, with no edition stated.
The product identifier was API 5L / ISO 3183. All 44 OD entries and 229 schedule
entries are removed, including aliases that formerly duplicated a wall entry.
Each NPS identifier below enumerates all removed schedule entries for that row;
"OD only" identifies rows with no schedule entry.

| NPS identifier | Removed schedule identifiers | Former source |
|---|---|---|
| 0.125 | SCH 40, STD, SCH 80, XS | ASME B36.10M; edition unstated |
| 0.25 | SCH 40, STD, SCH 80, XS | ASME B36.10M; edition unstated |
| 0.375 | SCH 40, STD, SCH 80, XS | ASME B36.10M; edition unstated |
| 0.5 | SCH 40, STD, SCH 80, XS, SCH 160, XXS | ASME B36.10M; edition unstated |
| 0.75 | SCH 40, STD, SCH 80, XS, SCH 160, XXS | ASME B36.10M; edition unstated |
| 1 | SCH 40, STD, SCH 80, XS, SCH 160, XXS | ASME B36.10M; edition unstated |
| 1.25 | SCH 40, STD, SCH 80, XS, SCH 160, XXS | ASME B36.10M; edition unstated |
| 1.5 | SCH 40, STD, SCH 80, XS, SCH 160, XXS | ASME B36.10M; edition unstated |
| 2 | SCH 40, STD, SCH 80, XS, SCH 160, XXS | ASME B36.10M; edition unstated |
| 2.5 | SCH 40, STD, SCH 80, XS, SCH 160, XXS | ASME B36.10M; edition unstated |
| 3 | SCH 40, STD, SCH 80, XS, SCH 160, XXS | ASME B36.10M; edition unstated |
| 3.5 | SCH 40, STD, SCH 80, XS, XXS | ASME B36.10M; edition unstated |
| 4 | SCH 40, STD, SCH 80, XS, SCH 120, SCH 160, XXS | ASME B36.10M; edition unstated |
| 5 | SCH 40, STD, SCH 80, XS, SCH 120, SCH 160, XXS | ASME B36.10M; edition unstated |
| 6 | SCH 40, STD, SCH 80, XS, SCH 120, SCH 160, XXS | ASME B36.10M; edition unstated |
| 8 | SCH 20, SCH 30, SCH 40, STD, SCH 60, SCH 80, XS, SCH 100, SCH 120, SCH 140, XXS, SCH 160 | ASME B36.10M; edition unstated |
| 10 | SCH 20, SCH 30, SCH 40, STD, SCH 60, XS, SCH 80, SCH 100, SCH 120, SCH 140, XXS, SCH 160 | ASME B36.10M; edition unstated |
| 12 | SCH 20, SCH 30, STD, SCH 40, XS, SCH 60, SCH 80, SCH 100, SCH 120, XXS, SCH 140, SCH 160 | ASME B36.10M; edition unstated |
| 14 | SCH 10, SCH 20, SCH 30, STD, SCH 40, XS, SCH 60, SCH 80, SCH 100, SCH 120, SCH 140, SCH 160 | ASME B36.10M; edition unstated |
| 16 | SCH 10, SCH 20, SCH 30, STD, SCH 40, XS, SCH 60, SCH 80, SCH 100, SCH 120, SCH 140, SCH 160 | ASME B36.10M; edition unstated |
| 18 | SCH 10, SCH 20, STD, SCH 30, XS, SCH 40, SCH 60, SCH 80, SCH 100, SCH 120, SCH 140, SCH 160 | ASME B36.10M; edition unstated |
| 20 | SCH 10, SCH 20, STD, SCH 30, XS, SCH 40, SCH 60, SCH 80, SCH 100, SCH 120, SCH 140, SCH 160 | ASME B36.10M; edition unstated |
| 22 | SCH 10, SCH 20, STD, SCH 30, XS, SCH 60, SCH 80, SCH 100, SCH 120, SCH 140, SCH 160 | ASME B36.10M; edition unstated |
| 24 | SCH 10, SCH 20, STD, XS, SCH 30, SCH 40, SCH 60, SCH 80, SCH 100, SCH 120, SCH 140, SCH 160 | ASME B36.10M; edition unstated |
| 26 | SCH 10, STD, SCH 20, XS | ASME B36.10M; edition unstated |
| 28 | SCH 10, STD, SCH 20, XS, SCH 30 | ASME B36.10M; edition unstated |
| 30 | SCH 10, STD, SCH 20, XS, SCH 30 | ASME B36.10M; edition unstated |
| 32 | SCH 10, STD, SCH 20, XS, SCH 30, SCH 40 | ASME B36.10M; edition unstated |
| 34 | SCH 10, STD, SCH 20, XS, SCH 30, SCH 40 | ASME B36.10M; edition unstated |
| 36 | SCH 10, STD, SCH 20, XS, SCH 30, SCH 40 | ASME B36.10M; edition unstated |
| 38 | OD only | ASME B36.10M; edition unstated |
| 40 | OD only | ASME B36.10M; edition unstated |
| 42 | STD, XS | ASME B36.10M; edition unstated |
| 44 | OD only | ASME B36.10M; edition unstated |
| 46 | OD only | ASME B36.10M; edition unstated |
| 48 | STD, XS | ASME B36.10M; edition unstated |
| 52 | OD only | ASME B36.10M; edition unstated |
| 56 | OD only | ASME B36.10M; edition unstated |
| 60 | OD only | ASME B36.10M; edition unstated |
| 64 | OD only | ASME B36.10M; edition unstated |
| 68 | OD only | ASME B36.10M; edition unstated |
| 72 | OD only | ASME B36.10M; edition unstated |
| 76 | OD only | ASME B36.10M; edition unstated |
| 80 | OD only | ASME B36.10M; edition unstated |

The fixed density, the rounded weight coefficient and the published-weight
fixtures are also removed. Density is now supplied per material row. The
reference-weight fixtures had the same dimensional-standard attribution.
Unit conversions remain numerical constants; they are not product data.

### Material rows

Every row below loses its yield/tensile strengths, optional product-level
strength, equivalent designation data, strength-designation number, elastic
modulus, Poisson ratio, density and toughness-temperature default. Grade names
remain usable only as caller-supplied identifiers, not as strength formulas.
The named standards were the only source stated by the removed registry.

| Removed registry group | Every removed row identifier | Former source |
|---|---|---|
| Line pipe | A25, A, B, X42, X46, X52, X56, X60, X65, X70, X80, X90, X100, X120 | API 5L, *Line Pipe*, 46th edition, tensile-test requirements; ISO 3183 equivalences |
| Hull normal strength | Grade A, Grade B, Grade D, Grade E | IACS UR W11, *Normal & Higher-Strength Hull Structural Steels*; revision unstated |
| Hull higher strength | AH32, DH32, EH32, FH32, AH36, DH36, EH36, FH36, AH40, DH40, EH40, FH40 | IACS UR W11; revision unstated |
| Structural | S275, S355, S420 | EN 10025-2, *Hot-rolled structural steel*; edition unstated |
| Austenitic pipe | TP304, TP304L, TP316, TP316L, TP321 | ASTM A312/A312M, austenitic stainless steel pipe; edition unstated |
| Duplex pipe | S31803, S32205 | ASTM A790/A790M, ferritic/austenitic stainless steel pipe; edition unstated |

The per-family modulus constants, common Poisson ratio, common density and
stainless density overrides had no independent citation. They are removed.
The hull lookup also stops inferring a test temperature from a toughness letter;
that temperature is a separate caller input.

| Removed compatibility snapshot | Every removed row identifier | Former source |
|---|---|---|
| `_LEGACY_SMYS_TABLE_MPA` (yield) | X52, X60, X65, X70, X80, X100 | Historical consumer copies attributed to API 5L; no independent citation |
| `_LEGACY_PIPE_DB_MPA` (yield_strength/tensile_strength) | X42, X52, X60, X65, X70, X80 | Historical consumer copies attributed to API 5L; no independent citation |
| `_LEGACY_RISER_MPA` (smys_mpa/smts_mpa) | X52, X60, X65, X70, X80 | Historical consumer copies attributed to API 5L; no independent citation |
| `_LEGACY_PIPELINE_PRESSURE_MPA` (smys/smts) | X52, X60, X65, X70 | Historical consumer copies attributed to API 5L; no independent citation |

The legacy psi adapters also lose their named-grade selection table and their
strength-from-designation and rounding conventions. All adapters now expose the
same supplied strengths, converting only units and output key names.

The pipe database caller had a second schedule-wall table attributed to the
same product standard. All of its rows are removed rather than used as a
fallback. Each removed row is identified below by nominal size and schedule.

| Nominal-size identifier | Every removed schedule identifier | Former source |
|---|---|---|
| 2 | SCH 40, SCH 80, STD, XS, XXS | API 5L; edition unstated |
| 3 | SCH 40, SCH 80, STD, XS, XXS | API 5L; edition unstated |
| 4 | SCH 40, SCH 80, STD, XS, XXS | API 5L; edition unstated |
| 6 | SCH 40, SCH 80, STD, XS, XXS | API 5L; edition unstated |
| 8 | SCH 20, SCH 40, SCH 60, SCH 80, SCH 100, SCH 120, SCH 140, SCH 160, STD, XS, XXS | API 5L; edition unstated |
| 10 | SCH 20, SCH 40, SCH 60, SCH 80, SCH 100, SCH 120, SCH 140, SCH 160, STD, XS, XXS | API 5L; edition unstated |
| 12 | SCH 20, SCH 40, SCH 60, SCH 80, SCH 100, SCH 120, SCH 140, SCH 160, STD, XS, XXS | API 5L; edition unstated |
| 14 | SCH 20, SCH 40, SCH 60, SCH 80, SCH 100, SCH 120, SCH 140, SCH 160, STD, XS | API 5L; edition unstated |
| 16 | SCH 20, SCH 40, SCH 60, SCH 80, SCH 100, SCH 120, SCH 140, SCH 160, STD, XS | API 5L; edition unstated |
| 18 | SCH 20, SCH 40, SCH 60, SCH 80, SCH 100, SCH 120, SCH 140, SCH 160, STD, XS | API 5L; edition unstated |
| 20 | SCH 20, SCH 40, SCH 60, SCH 80, SCH 100, SCH 120, SCH 140, SCH 160, STD, XS | API 5L; edition unstated |
| 24 | SCH 20, SCH 40, SCH 60, SCH 80, SCH 100, SCH 120, SCH 140, SCH 160, STD, XS | API 5L; edition unstated |
| 30 | SCH 20, SCH 40, SCH 60, SCH 80, STD, XS | API 5L; edition unstated |
| 36 | SCH 20, SCH 40, SCH 60, SCH 80, STD, XS | API 5L; edition unstated |

Its fixed elastic, shear, Poisson and density values are also removed. The
supplied material provides E, nu and density; shear modulus is computed as
E / (2 * (1 + nu)). The removed family default identifiers in `grades.py` are
`_E_LINE_PIPE`, `_E_MARINE`, `_E_STRUCTURAL`, `_E_AUSTENITIC`, `_E_DUPLEX`,
`_NU`, `_RHO`, `_RHO_AUSTENITIC` and `_RHO_DUPLEX`. None had an independent
source citation. No numerical default is retained.

## Caller input contract

`load_catalog(data_path)` reads JSON data and the adjacent `sources.yml`.
The sidecar uses JSON syntax, a YAML subset. Duplicate keys are errors. The
root data keys are `grades` and `dimensions`, both arrays. Each data row has a
unique `row_id`; the sidecar has exactly one matching key for each row, with no
orphan keys. The complete executable schema example is in
`tests/materials/fixtures/catalog.json` and its adjacent `sources.yml`. Every
row there is synthetic software-test data, not an engineering basis.

Each source entry requires `title`, `publisher`, `revision`, `date` in ISO
calendar form, `kind`, and a public HTTP(S) `url` or `catalogue_number`.
Allowed source kinds are `manufacturer`, `public_paper`, `design_basis` and
`synthetic`. Standards-attributed source titles and standard publishers are
rejected. A manufacturer catalogue or public paper may mention a product
specification in its exact title when its publisher is independent of the
standard publisher. Specification designations belong in the data's `standard` field.
A source entry describes provenance; it does not authenticate rights or qualify
engineering suitability. A clean manufacturer publication or public paper must
be checked before an engineering row is introduced.

Grade rows require `name`, `standard`, `smys_mpa`, `smts_mpa`, `E_mpa`, `nu`,
`rho_kg_m3` and source linkage through `row_id`. `smts_mpa` may be null only
when `smts_psl1_mpa` is supplied. Optional aliases, material class and Charpy
test temperature are explicit row inputs. Positive engineering inputs reject
booleans, strings, nonfinite numbers and zero. Tensile strength cannot be below
yield. Poisson ratio must be between minus one and one half. Aliases cannot
collide with another name or alias. An unavailable product level raises an error.

Dimension rows require `nps`, `schedule`, `od_in`, `wt_in` and `row_id`.
Each NPS/schedule pair is unique; OD must agree across rows for the same NPS.
Explicit geometry also requires a validated `geometry_source` record.
No engineering property defaults are installed.

```python
from digitalmodel.materials import load_catalog, use_catalog, line_pipe

catalog = load_catalog("inputs/catalog.json")
with use_catalog(catalog):
    pipe = line_pipe("specified-grade", nps=8, schedule="specified-schedule")
```

The context must encompass the calculation, model generation or request. Nested
contexts restore the previous inputs even after an exception. Context variables
isolate concurrent requests; callers must explicitly propagate a context to
new threads when required. `allow_synthetic=True` is reserved for tests.

## Callers and resulting behavior

| Caller | Supplied input now used |
|---|---|
| Material query and unit-view API | Exact caller name or alias, strengths and product levels |
| Material database | Snapshot of supplied rows; digest covers data bytes, a null separator and sidecar bytes; source revision and date come from each row |
| FFS material library | Supplied strength, modulus, density, Poisson ratio, class and optional Charpy temperature |
| Corroded pipe, corrosion assessment and dependent FFS lookup, acceptance-curve and riser-joint utilities | Lazy sourced yield/tensile mappings in the existing units |
| Pipeline integrity skill | Supplied yield strength, without a historical rounded snapshot |
| Pipeline pressure library | Lazy material objects from supplied strengths |
| Riser configuration | Supplied elastic modulus and density |
| Wall-thickness parameter sweep | Supplied yield and tensile strengths in Pa |
| Pipe specification database | Supplied dimensions, strengths and material properties; shear modulus computed geometrically from elastic inputs |
| Line-type and line generator | Supplied density, modulus, Poisson ratio and yield; unknown grades raise rather than select another grade |
| Flowline sweep and its field-development API callers | Supplied OD and wall per candidate NPS/schedule |

Without an active validated catalogue, these lookup paths fail with an explicit
caller-input error. Importing them does not create or capture a material table.
Callers using fully explicit strength inputs retain their calculation interfaces.

## Geometry and unit basis

For outside diameter D and wall t, ID = D - 2t, metal area = pi*(D squared -
ID squared)/4, and bore area = pi*ID squared/4. Mass per length is metal area
in square metres times supplied density. Internal volume per length equals bore
area in square metres. D and t must be finite and positive, with t less than D/2.
Nominal-size labels do not imply an outside diameter.

## Verification and limits

The starting materials suite passed 256 tests. Synthetic tests cover the source
schema, rejection of standards-attributed source labels, missing inputs, scope
restoration, aliases, product levels, unit adapters, geometry, immutable database
snapshots and source digests. Final counts are recorded in the pull request.
No standard was opened, downloaded or quoted. No replacement engineering row
was sourced. Studies require independently qualified caller data before use.
