# Riser database knowledge pack

| Header | Value |
|---|---|
| Module / repository | `riser_database` / `digitalmodel` |
| Package | `src/digitalmodel/riser_database` |
| Pack revision | 0, 2026-10-16 |
| Code revision | `1dba289c5dc1bc8dc82d72e4bfe02f8dcfc22c23` |
| Analysis family | Reference-data calculations supporting OrcaFlex riser and mooring; routing family `EC` |
| Quality role | Riser and mooring quality manager; independent provider required |
| Status | DRAFT; second-provider clearance pending |
| Implementation-map ref | `codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; map data revision `270c8759c374b67a7f032953e7fcdb14e0224461` |

## 1. Purpose and scope

`RiserDatabase` loads five keyed reference tables, checks their fingerprints and column sets, and exposes typed rows and exact-key lookups; citation getters return scalar factors with a `Citation` rather than performing a riser response analysis. [src/digitalmodel/riser_database/loader.py:L162-L168] [src/digitalmodel/riser_database/loader.py:L183-L214] [src/digitalmodel/riser_database/loader.py:L235-L289] [src/digitalmodel/riser_database/getters.py:L122-L141]

The package scope includes all three Python files and no excluded subpackage; its public exports include the loader, five row classes, seven getters and the bundled citation helper. [src/digitalmodel/riser_database/__init__.py:L8-L48]

Registry maturity is `development`; the initial documentation's three-table and two-getter description is historical rather than a current inventory. [docs/registry/module-routing.yaml:L484-L489] [docs/riser_database.md:L3-L6] [src/digitalmodel/riser_database/loader.py:L162-L168] [src/digitalmodel/riser_database/__init__.py:L40-L47]

Configuration envelopes, standards locators, material references, stack-up handles and rig-interface records are the supported data products; exact joint-by-joint source values remain outside the public stack-up row schema. [src/digitalmodel/riser_database/loader.py:L60-L141]

No geometry applicability envelope, water-depth applicability envelope, solver version or engineering acceptance verdict is enforced by this loader: numeric cells are converted with `float`, and unknown keys raise instead of selecting a nearby record. [src/digitalmodel/riser_database/loader.py:L49-L57] [src/digitalmodel/riser_database/loader.py:L183-L214] [src/digitalmodel/riser_database/loader.py:L282-L289]

## 2. Inputs and outputs

### Inputs

| Input | Unit / convention | Allowed range / default | Source and contract |
|---|---|---|---|
| `RiserDatabase.load(root)` | directory; UTF-8 CSV and YAML | Default `data/riser_database`; exactly five named tables; per-file cap 5,242,880 bytes | [src/digitalmodel/riser_database/loader.py:L33-L37] [src/digitalmodel/riser_database/loader.py:L171-L208] [src/digitalmodel/riser_database/loader.py:L235-L256] |
| Manifest | SHA-256 hexadecimal digests; row counts are metadata | Table set must match; declared `rows` is not checked at load | [data/riser_database/manifest.yaml:L1-L25] [src/digitalmodel/riser_database/loader.py:L242-L256] |
| Configuration numeric cells | `water_depth_m` in m; `od_mm`, `wt_mm` in mm | Empty cell becomes `None`; no positivity or finiteness constraint | [src/digitalmodel/riser_database/loader.py:L49-L70] [src/digitalmodel/riser_database/loader.py:L200-L209] |
| Stack-up identifiers and envelope | `rsu_id`, topology, operation, coarse depth band; `riser_od_in` in inches | No guessed depth; rig reference may be empty; typed numeric cell may be `None` | [src/digitalmodel/riser_database/loader.py:L97-L111] [tests/riser_database/test_stackup_rig.py:L143-L155] |
| Rig capability cells | counts; capacity in kips; strokes, moonpool and floor dimensions in ft; moment in kN·m; angle in degrees | Empty cell becomes `None`; rated moment/angle documented as initially empty | [src/digitalmodel/riser_database/loader.py:L114-L141] [src/digitalmodel/riser_database/loader.py:L144-L159] |
| Getter `override`, `repo_root` | scalar in getter output units; optional wiki root | `override=None` selects named code constant; supplied override is converted by `float`, still citation-validated | [src/digitalmodel/riser_database/getters.py:L122-L141] [src/digitalmodel/riser_database/getters.py:L246-L276] |
| Flex-joint `kind` | categorical | `mean` or `max`; default `max`; other strings raise `ValueError` | [src/digitalmodel/riser_database/getters.py:L246-L276] |
| Envelope `mode` and wiki criteria | category key; UTF-8 page with fenced YAML | Required mode and three value fields; no numeric-domain validation after conversion | [src/digitalmodel/riser_database/getters.py:L294-L359] |

### Outputs

| Output | Unit / convention | Shape / default | Source |
|---|---|---|---|
| `config`, `crosswalk`, `material`, `stackup`, `rig` | Units above; references are identifiers | Frozen dataclass rows; unknown key raises `RiserDatabaseError` | [src/digitalmodel/riser_database/loader.py:L60-L141] [src/digitalmodel/riser_database/loader.py:L267-L289] |
| Collection properties | records | Tuples of rows from the loaded mappings | [src/digitalmodel/riser_database/loader.py:L291-L309] |
| `iter_public_rows()` | text for inspection, not a numeric interchange format | `(table, key, flattened text)`; missing numerics flatten to empty text | [src/digitalmodel/riser_database/loader.py:L311-L328] |
| DFF, SCF, weight, buoyancy and equivalent-stress design factors | dimensionless | `CitedValue(value, citation, units)` | [src/digitalmodel/riser_database/getters.py:L122-L243] |
| Flex-joint limits | degrees | One `CitedValue` per selected kind | [src/digitalmodel/riser_database/getters.py:L246-L276] |
| Envelope criteria | dimensionless design factor; angles in degrees | Frozen object with criteria set, category and three cited quantities | [src/digitalmodel/riser_database/getters.py:L113-L119] [src/digitalmodel/riser_database/getters.py:L344-L359] |
| `riser_citations()` | keyed cited values | Five keys on success; empty mapping on any caught resolution failure, with first warning only | [src/digitalmodel/riser_database/getters.py:L377-L395] |

Vector axes and RAO phase are NOT_APPLICABLE to the scalar and identifier outputs; no vector, motion or complex response is returned. [src/digitalmodel/riser_database/loader.py:L60-L141] [src/digitalmodel/riser_database/getters.py:L113-L119]

UNVERIFIED: the datum and positive direction for `drill_floor_elev_ft` are not established by the row schema; issue pending (1). [src/digitalmodel/riser_database/loader.py:L130-L132]

## 3. Method and governing standards

Loading is implemented as bounded file read → SHA-256 equality → exact CSV column order → non-ragged rows → optional-float conversion → unique keys; manifest parsing uses `yaml.safe_load`, not a duplicate-key-rejecting parser. [src/digitalmodel/riser_database/loader.py:L171-L214] [src/digitalmodel/riser_database/loader.py:L242-L256]

The table generator, outside the package, extracts geometry from the unified examples and from pipe configurations, using explicit conversion rules and filename tokens; a missing depth token produces an unknown cell rather than a physical estimate. [scripts/riser_database/build_tables.py:L98-L145] [scripts/riser_database/sources.yml:L10-L17] [scripts/riser_database/sources.yml:L41-L51]

| Method / reference named in source | Edition and locator as implemented | Implementation and citation sidecar |
|---|---|---|
| Riser design fatigue factor; allowable damage described as reciprocal DFF | DNV-OS-F201, Dynamic Risers, 2010, design fatigue factors | `get_riser_dff`, `CitedValue.citation`; clause text not checked against the licensed copy. [src/digitalmodel/riser_database/getters.py:L45-L50] [src/digitalmodel/riser_database/getters.py:L122-L141] |
| Stress concentration methodology; neutral default is a code convention | DNV-RP-C203, Fatigue Design of Offshore Steel Structures, 2021, stress concentration factors | `get_riser_scf`; clause text not checked against the licensed copy. [src/digitalmodel/riser_database/getters.py:L52-L59] [src/digitalmodel/riser_database/getters.py:L144-L169] [tests/citations/fixtures/knowledge/wikis/engineering-standards/wiki/standards/dnv-rp-c203.md:L1-L18] |
| Weight and buoyancy tension factors | API RP 16Q, Drilling Riser Systems (fixture title), first edition 1993, tension factors | `get_tension_weight_factor`, `get_buoyancy_tension_factor`; clause text not checked against the licensed copy. [src/digitalmodel/riser_database/getters.py:L61-L69] [src/digitalmodel/riser_database/getters.py:L172-L219] [tests/citations/fixtures/knowledge/wikis/engineering-standards/wiki/standards/api-rp-16q.md:L1-L18] |
| Equivalent-stress design factor; allowable described as factor × SMYS | API STD 2RD, Dynamic Risers (fixture title), second edition 2013, dynamic riser design criteria | `get_von_mises_design_factor`; clause text not checked against the licensed copy. [src/digitalmodel/riser_database/getters.py:L71-L78] [src/digitalmodel/riser_database/getters.py:L222-L243] [tests/citations/fixtures/knowledge/wikis/engineering-standards/wiki/standards/api-std-2rd.md:L1-L18] |
| Flex-joint operating angle selection | API RP 16Q, first edition 1993, operating limits; numeric clause locator not given | `get_flexjoint_angle_limit`; clause text not checked against the licensed copy. [src/digitalmodel/riser_database/getters.py:L246-L276] |
| Legacy API RP 2RD name | Recorded as former designation, not a second implemented edition in this crosswalk | `api-std-2rd` row and getter template; no RP-specific getter. [data/riser_database/standards_crosswalk.csv:L4-L4] [src/digitalmodel/riser_database/getters.py:L71-L78] |
| DNV-OS-E301 | Named only as the mooring-getter pattern, not as this package's factor basis | No package E301 getter or crosswalk row. [src/digitalmodel/riser_database/getters.py:L1-L17] [data/riser_database/standards_crosswalk.csv:L1-L6] |
| Additional envelope-criteria reference | AMJIG drilling-riser integrity management guideline, revision 2 (2000), criteria by operating category | `get_amjig_envelope_criteria` reads wiki-side values; clause text not checked against the licensed copy. [src/digitalmodel/riser_database/getters.py:L80-L89] [src/digitalmodel/riser_database/getters.py:L310-L359] |

The API RP 16Q crosswalk still carries `public-metadata-required-before-citation-use`, whereas the getter and documentation select 1993; this is a metadata defect, not an inferred edition correction, tracked as issue pending (2). [data/riser_database/standards_crosswalk.csv:L3-L3] [src/digitalmodel/riser_database/getters.py:L61-L69] [docs/riser_database.md:L70-L79]

`validate_citation` checks page existence and `code_id`, `publisher`, `revision` metadata, but explicitly does not validate clause locators; a successful fixture-based getter test therefore establishes metadata behavior rather than independent standards correctness. [src/digitalmodel/citations/schema.py:L141-L180] [tests/riser_database/test_citations.py:L27-L30]

UNVERIFIED: standards correctness and complete edition/title/locator provenance beyond the code's own labels remain unestablished without the licensed sources; issue pending (2).

## 4. Validated worked examples

Validated examples: 0; the selected cases below assert repository fixtures, catalogue expectations or defensive behavior, and supply no independent engineering benchmark. [tests/riser_database/test_loader.py:L61-L91] [tests/riser_database/test_citations.py:L256-L281]

| Case / class | Source and input | Expected result, unit and tolerance basis | Existing test node ID | Last offline result |
|---|---|---|---|---|
| Catalogue counts / regression | Pinned public tables and manifest | 27 configurations, 5 crosswalk rows, at least 2 material rows; exact counts (records), repository assertion | `tests/riser_database/test_loader.py::test_row_counts_pinned` | PASS, BASE and AFTER. [tests/riser_database/test_loader.py:L61-L66] |
| Unified example / regression | `examples/riser_input/riser_scr.yml` → `data/riser_database/config_catalog.csv` | Water depth 1500.0 m; OD 273.100 mm; depth exact and OD `pytest.approx` defaults, test convention | `tests/riser_database/test_loader.py::test_config_row_values_scr` | PASS, BASE and AFTER. [tests/riser_database/test_loader.py:L69-L75] |
| Pipe conversion / regression | `config/pipe/pipe_py_21OD_0875in_5000ft_upper_riser.yml` → catalogue | Depth 1524.0 m; OD 533.400 mm; wall 22.225 mm; `pytest.approx` defaults, test convention | `tests/riser_database/test_loader.py::test_config_row_pipe_filename_derivation` | PASS, BASE and AFTER. [tests/riser_database/test_loader.py:L78-L85] |
| Missing depth / regression | `config/pipe/pipe_py_21OD_0875in_Drilling_Riser.yml` → catalogue; no depth token | Exactly 1 unknown depth (`None`), not zero m | `tests/riser_database/test_loader.py::test_config_row_drilling_variant_has_no_depth` | PASS, BASE and AFTER. [tests/riser_database/test_loader.py:L88-L91] |
| Handle-table counts / regression | `data/riser_database/riser_stackup.csv` and `rig_riser_interface.csv` | Exactly 84 stack-up records and 10 rig records | `tests/riser_database/test_stackup_rig.py::test_row_counts_pinned` | PASS, BASE and AFTER. [tests/riser_database/test_stackup_rig.py:L88-L93] |
| Missing-wiki wrapper / regression | Temporary no-wiki fixture | Empty mapping both calls; exactly 1 warning; first call <0.5 s | `tests/riser_database/test_citations.py::test_pip_no_wiki_helper_warns_once_and_degrades` | PASS, BASE and AFTER. [tests/riser_database/test_citations.py:L261-L271] |

The installed pytest 8.4.2 tolerance calculation gives the section-4 approximate assertions a relative tolerance of 1e-6 with an absolute floor of 1e-12; this is a test convention, not an independently established engineering accuracy (pytest, `pytest.approx`, version 8.4.2, tolerances; calculated during the offline check).

The documentation's worked loader and getter snippets map to the catalogue and citation regression paths, not a solver result or a published benchmark. [docs/riser_database.md:L30-L59] [tests/riser_database/test_loader.py:L69-L75] [tests/riser_database/test_citations.py:L274-L281]

## 5. QA gates

AQG r0, sections 4–10, supplies the family gate requirements; `EC` records the package's reference-data calculation role, while the downstream OrcaFlex riser and mooring gate obligations remain explicit below.

| Gate | Applicability / observed status | Check IDs and boundary |
|---|---|---|
| G0 inputs | Applicable; package checks offline | RDB-01–RDB-07: table integrity, lookup coverage, conversion conventions, citation failure behavior and publication boundaries |
| G1 load | Applicable to consuming models; NOT_RUN, no target model/version supplied | RDB-08: target-version OrcaFlex `LoadData` route on an authorized installation; Python import alone is not a load result; issue pending (4) |
| G2 statics | Applicable to consuming models; NOT_RUN, solver output absent | RDB-09: consumer's zero-load reference and offset sweep; issue pending (4) |
| G3 short dynamics | Applicable to consuming models; NOT_RUN, solver output absent | RDB-10: consumer's short-run response evidence; issue pending (4) |
| G4 diffraction | NOT_APPLICABLE to catalogue-only outputs; conditional when a consumer introduces newly computed vessel data | No vessel/mesh/RAO fields in these row types. [src/digitalmodel/riser_database/loader.py:L60-L141] |
| G5 evidence appendix | Applicable; offline pack evidence only, downstream signed appendix absent | RDB-11: examples and failures must retain their outcomes; issue pending (4) for analysis appendix |

| Check | Module-specific executable route | Numeric pass criterion and unit | Evidence / status / gate |
|---|---|---|---|
| RDB-01 | `tests/riser_database/test_loader.py`: load, absent root/table, modified hash, columns, duplicate key and cap cases | 0 unexpected successful loads in negative cases; 5 known tables; 0 missing row classes (counts) | Pytest outcomes; G0. [tests/riser_database/test_loader.py:L54-L58] [tests/riser_database/test_loader.py:L121-L185] |
| RDB-02 | `test_schema.py`: exact headers, relative wiki locators, locator/free-text shapes; `test_stackup_rig.py`: vocabulary and joins | 0 schema mismatches; 0 unresolved nonempty rig joins (counts) | Pytest outcomes; G0. [tests/riser_database/test_schema.py:L62-L109] [tests/riser_database/test_stackup_rig.py:L118-L148] |
| RDB-03 | Four section-4 loader fixtures and `test_every_seed_file_has_a_row` | Missing seeds 0 records; relative conversion error ≤1e-6 (dimensionless) with absolute floor 1e-12 m or mm under `pytest.approx`; missing depth remains 1 `None` | Pytest outcomes; G0. [tests/riser_database/test_loader.py:L69-L103] |
| RDB-04 | `test_citations.py`: missing-page, no-wiki, valid override, bundle-key, angle-kind and selection regressions | Direct missing-page exceptions 1 per call; bundle keys on success 5; standalone warnings 1; resolution <0.5 s; valid-override mismatches 0 (counts) | Pytest outcomes; does not test configured-wiki wrapper rejection; G0. [tests/riser_database/test_citations.py:L156-L195] [tests/riser_database/test_citations.py:L211-L222] [tests/riser_database/test_citations.py:L256-L281] |
| RDB-05 | `test_code_editions.py` and `test_material_refs.py` | Edition-label discrepancies 0 in tested surfaces; unresolved curve/getter references 0 records | Pytest outcomes; no claim of licensed-text validation; G0. [tests/riser_database/test_code_editions.py:L48-L76] [tests/riser_database/test_code_editions.py:L99-L136] [tests/riser_database/test_material_refs.py:L22-L38] |
| RDB-06 | `test_layer_a_public_rows_structurally_clean`, schema no-value-column test, stack-up de-identification tests | Structural findings 0; bare numeric cells in standards-reference tables 0; exact unit-bearing stack-up disclosures 0 (counts) | Shipped pytest checks; G0. [tests/riser_database/test_leak_gate.py:L48-L60] [tests/riser_database/test_schema.py:L112-L132] [tests/riser_database/test_stackup_rig.py:L179-L216] |
| RDB-07 | Private registry reconciliation and provenance tests, when authorized reference data are present | Unmatched handle sets 0 records; provenance matches 0 findings | Availability-dependent; missing private data gives NOT_RUN, never PASS; G0. [tests/riser_database/test_stackup_rig.py:L237-L260] [tests/riser_database/test_provenance_tripwire.py:L241-L305] |
| RDB-08 | Proposed consuming-model check: retain chosen `RiserDatabase.config` key and table digests; load the resulting model in its declared target OrcaFlex version through `LoadData` | Load errors 0, load warnings 0, referenced database keys absent 0 (counts) | NOT_RUN; no package model writer or existing detection check; issue pending (4); G1. [src/digitalmodel/riser_database/loader.py:L267-L289] |
| RDB-09 | Proposed consumer hand calculation: compare low-load top tension and angle to the consuming model, then check offsets | Tension difference ≤5.0 %; angle difference ≤1.0 degree; ≥6 offset steps; unconverged cases 0 | NOT_RUN; no package equilibrium solver or existing detection check; issue pending (4); G2. AQG r0, section 6, statics QA. |
| RDB-10 | Proposed consumer transient check with table-root digest, selected keys and getter criteria recorded | Stop time 1.0 s; nonfinite outputs 0; warnings 0; tension departure from statics ≤25.0 % | NOT_RUN; no package dynamics runner or existing detection check; issue pending (4); G3. AQG r0, section 7, short dynamics. |
| RDB-11 | Hand audit: match section-4 node IDs and statuses to the offline log and retain named exclusions | Missing case outcomes 0; missing citations 0; unlabelled skipped cases 0 (counts) | Pack self-check; downstream appendix/signatures NOT_RUN; G5. AQG r0, section 9, QA appendix. |

RDB-08–RDB-10 are proposed integration acceptance procedures, not shipped detection checks; package-only test success cannot clear a consuming analysis under AQG r0, sections 5–9.

## 6. Known failure modes and pitfalls

sweep: 37 found

The sweep counts distinct source sites and individual unit-schema assumptions, including documented defaults; a hit is not automatically a defect. The complete package was inspected with these exact commands:

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|or \{\}|= 0(\.0)?\b|1970|0\.05' src/digitalmodel/riser_database --glob '*.py'
rg -n 'float\(|int\(|DEFAULT|FACTOR|LIMIT|\.get\(|units=|_m\b|_mm\b|_in\b|_ft\b|_kips\b|_deg\b|_kn_m\b|write|cache' src/digitalmodel/riser_database --glob '*.py'
```

| Site | Symptom / classification | Existing detection / gap | Tracking |
|---|---|---|---|
| S01 `getters.py:386–395` | Every `CitationResolutionError` becomes `{}`; later calls lose even the warning, including a configured but stale wiki | RDB-04 detects standalone degradation only; no detection check exists for configured-wiki rejection | #2166; [src/digitalmodel/riser_database/getters.py:L377-L395] |
| S25 `loader.py:243` | Manifest parsing uses `yaml.safe_load`, whose duplicate mapping keys can overwrite earlier values; strict duplicate-key rejection is absent | no detection check exists for duplicate manifest keys | issue pending (1); [src/digitalmodel/riser_database/loader.py:L242-L256] |
| S02 `loader.py:50–51` | Empty numeric field becomes `None`; unknown is distinct from true zero | RDB-03 missing-depth fixture | [src/digitalmodel/riser_database/loader.py:L49-L57] [tests/riser_database/test_loader.py:L88-L91] |
| S03 `loader.py:53` | `float` of external CSV text accepts nonfinite strings; comma/group/decimal conventions are not normalized; malformed text raises | no detection check exists for nonfinite or locale input | issue pending (1); [src/digitalmodel/riser_database/loader.py:L49-L57] |
| S04 `loader.py:200–214,246–256` | Empty table mapping can load a header-only file if its hash is authorized; declared row counts are ignored | RDB-01/RDB-02 cover committed table shapes, not tampered completeness with repaired hash; no detection check exists | issue pending (1); [src/digitalmodel/riser_database/loader.py:L193-L214] [src/digitalmodel/riser_database/loader.py:L246-L256] |
| S05 `getters.py:93` | Named DFF default selected when no override is supplied; numerical default not reproduced here | RDB-05 parity only; no independent adequacy check exists | issue pending (2); [src/digitalmodel/riser_database/getters.py:L91-L99] [tests/riser_database/test_material_refs.py:L41-L47] |
| S06 `getters.py:96` | Neutral SCF default is a convention, not a resolved structural concentration | RDB-05 parity only | [src/digitalmodel/riser_database/getters.py:L144-L169] [tests/riser_database/test_material_refs.py:L41-L47] |
| S07 `getters.py:99` | Named weight-factor default enters a getter when override is absent | RDB-05/RDB-04 citation and parity, not independent standards correctness | issue pending (2); [src/digitalmodel/riser_database/getters.py:L172-L193] [tests/riser_database/test_citations.py:L139-L150] |
| S08 `getters.py:102` | Named buoyancy-factor default is distinct from a rig tensioner reduction factor | RDB-05 parity only | issue pending (2); [src/digitalmodel/riser_database/getters.py:L196-L219] [tests/riser_database/test_citations.py:L139-L150] |
| S09 `getters.py:106` | Named von-Mises design-factor default has a fixed operating basis | RDB-05 engine-default parity only | issue pending (2); [src/digitalmodel/riser_database/getters.py:L222-L243] [tests/riser_database/test_citations.py:L245-L250] |
| S10 `getters.py:109` | Named mean-angle default is a fixed operating limit, not hardware capability | RDB-04 getter selection regression only | issue pending (2); [src/digitalmodel/riser_database/getters.py:L246-L276] [tests/riser_database/test_citations.py:L211-L217] |
| S11 `getters.py:110` | Named maximum-angle default and default selector can be mistaken for a case-specific rating | RDB-04 selection and invalid-kind tests only | issue pending (2); [src/digitalmodel/riser_database/getters.py:L246-L276] [tests/riser_database/test_citations.py:L211-L222] |
| S12 `getters.py:133` | DFF override converted by `float`, without positivity/finiteness validation | RDB-04 valid-override regression; no detection check exists for invalid-domain override | issue pending (1); [src/digitalmodel/riser_database/getters.py:L130-L141] [tests/riser_database/test_citations.py:L168-L172] |
| S13 `getters.py:161` | SCF override has the same numeric-domain gap | no detection check exists for invalid-domain override | issue pending (1); [src/digitalmodel/riser_database/getters.py:L155-L169] |
| S14 `getters.py:185` | Weight-factor override has the same numeric-domain gap | no detection check exists for invalid-domain override | issue pending (1); [src/digitalmodel/riser_database/getters.py:L182-L193] |
| S15 `getters.py:211` | Buoyancy-factor override has the same numeric-domain gap | no detection check exists for invalid-domain override | issue pending (1); [src/digitalmodel/riser_database/getters.py:L208-L219] |
| S16 `getters.py:235` | Design-factor override has the same numeric-domain gap | no detection check exists for invalid-domain override | issue pending (1); [src/digitalmodel/riser_database/getters.py:L232-L243] |
| S17 `getters.py:268` | Angle override has the same numeric-domain gap; no degrees/radians inference is performed | no detection check exists for invalid-domain override | issue pending (1); [src/digitalmodel/riser_database/getters.py:L265-L276] |
| S18 `getters.py:298` | Empty YAML fence becomes `{}`; wrong payload shape continues searching before explicit missing-block failure | no detection check exists for empty/nonmapping fences or duplicate criteria keys | issue pending (1); [src/digitalmodel/riser_database/getters.py:L294-L307] |
| S19 `getters.py:351` | External criteria value converted by `float`; nonfinite accepted, decimal/thousands locale unspecified | no detection check exists for malformed/nonfinite criteria values | issue pending (1); [src/digitalmodel/riser_database/getters.py:L344-L358] |
| S20 `getters.py:355` | Missing `category` silently becomes the mode key | no detection check exists for missing-category fallback | issue pending (1); [src/digitalmodel/riser_database/getters.py:L353-L359] |
| S21 `loader.py:325` | Flattening converts missing values to empty text; loses the typed `None` distinction | RDB-01 table coverage only; no detection check exists for unknown/empty text distinction | issue pending (1); [src/digitalmodel/riser_database/loader.py:L311-L328] |
| S22 `loader.py:65` | `water_depth_m` assumes metres; no unit tag or physical range is validated at load | RDB-03 fixed-depth fixture only | issue pending (1); [src/digitalmodel/riser_database/loader.py:L60-L70] |
| S23 `loader.py:107` | `riser_od_in` assumes inches; no physical range or unit metadata is validated | no detection check exists for unit/domain mismatch | issue pending (1); [src/digitalmodel/riser_database/loader.py:L103-L111] |
| S24 `loader.py:124` | `n_tensioners` assumes a count but accepts an optional float without integrality checks | no detection check exists for invalid count domain | issue pending (1); [src/digitalmodel/riser_database/loader.py:L122-L132] |
| S26 `loader.py:66` | `od_mm` assumes millimetres; its schema does not validate physical units or ranges | RDB-03 checks a fixed conversion fixture only | issue pending (1); [src/digitalmodel/riser_database/loader.py:L66-L66] |
| S27 `loader.py:67` | `wt_mm` assumes millimetres; its schema does not validate physical units or ranges | RDB-03 checks a fixed conversion fixture only | issue pending (1); [src/digitalmodel/riser_database/loader.py:L67-L67] |
| S28 `loader.py:108` | `water_depth_band` assumes a coarse metre band, not an exact depth; its schema does not validate physical units or ranges | RDB-02 restricts the depth-band vocabulary; no exact-depth inference is permitted | [tests/riser_database/test_stackup_rig.py:L118-L123] [src/digitalmodel/riser_database/loader.py:L108-L108] |
| S29 `loader.py:126` | `tensioner_capacity_kips` assumes kips; its schema does not validate physical units or ranges | no detection check exists for unit/domain mismatch | issue pending (1); [src/digitalmodel/riser_database/loader.py:L126-L126] |
| S30 `loader.py:127` | `tensioner_stroke_ft` assumes feet; its schema does not validate physical units or ranges | no detection check exists for unit/domain mismatch | issue pending (1); [src/digitalmodel/riser_database/loader.py:L127-L127] |
| S31 `loader.py:128` | `tj_stroke_ft` assumes feet; its schema does not validate physical units or ranges | no detection check exists for unit/domain mismatch | issue pending (1); [src/digitalmodel/riser_database/loader.py:L128-L128] |
| S32 `loader.py:129` | `n_tension_wires` assumes an equipment count; its schema does not validate physical units or ranges | no detection check exists for unit/domain mismatch | issue pending (1); [src/digitalmodel/riser_database/loader.py:L129-L129] |
| S33 `loader.py:130` | `moonpool_length_ft` assumes feet; its schema does not validate physical units or ranges | no detection check exists for unit/domain mismatch | issue pending (1); [src/digitalmodel/riser_database/loader.py:L130-L130] |
| S34 `loader.py:131` | `moonpool_width_ft` assumes feet; its schema does not validate physical units or ranges | no detection check exists for unit/domain mismatch | issue pending (1); [src/digitalmodel/riser_database/loader.py:L131-L131] |
| S35 `loader.py:132` | `drill_floor_elev_ft` assumes feet; physical range, elevation datum and sign convention are not validated | no detection check exists for unit/domain mismatch | issue pending (1); [src/digitalmodel/riser_database/loader.py:L132-L132] |
| S36 `loader.py:137` | `conductor_moment_capacity_kn_m` assumes kN·m; its schema does not validate physical units or ranges | no detection check exists for unit/domain mismatch | issue pending (1); [src/digitalmodel/riser_database/loader.py:L137-L137] |
| S37 `loader.py:138` | `flexjoint_angle_rating_deg` assumes degrees; its schema does not validate physical units or ranges | no detection check exists for unit/domain mismatch | issue pending (1); [src/digitalmodel/riser_database/loader.py:L138-L138] |

No package `continue`, `int()` conversion, zero/epoch/percentage fallback or persistent empty-result cache write is present in the inspected Python files; the only caught conversion error re-raises with table and column context rather than swallowing the record. [src/digitalmodel/riser_database/loader.py:L49-L57] [src/digitalmodel/riser_database/loader.py:L183-L214] [src/digitalmodel/riser_database/getters.py:L294-L307] [src/digitalmodel/riser_database/getters.py:L365-L395]

Additional confirmed pitfalls: API RP 16Q metadata differs between the crosswalk and getter (issue pending (2)); the brief historical three-table documentation omits later tables; and citation fixtures cannot verify clause text. [data/riser_database/standards_crosswalk.csv:L3-L3] [src/digitalmodel/riser_database/getters.py:L61-L69] [docs/riser_database.md:L3-L6] [src/digitalmodel/citations/schema.py:L141-L154]

## 7. Lessons log

Append-only record; no earlier entries are asserted.

| Date | Entry | Lesson and evidence | Pack change | Shared-log reference |
|---|---|---|---|---|
| 2026-10-16 | Initial pack | Treat standalone degradation and configured-wiki errors separately; the current helper catches both. [src/digitalmodel/riser_database/getters.py:L377-L395] | S01 and RDB-04 limit the fail-closed claim | #2166 |
| 2026-10-16 | Initial pack | An optional numeric cell is unknown, not zero, and a table hash does not validate physical input domains. [src/digitalmodel/riser_database/loader.py:L49-L57] [src/digitalmodel/riser_database/loader.py:L183-L214] | S02–S04, S12–S24 | issue pending (1) |

## 8. Ownership

Required producing role: module contributor. Required family review: riser and mooring quality manager on a different provider; calculation/data checks feed that review. Proposed deputy: calculation quality manager. Classification disagreements about further implementations are resolved by the family quality manager; merge clearance remains with the maintainer. Escalate a check unresolved after two attempts, ambiguous clauses, missing or contradictory inputs, and proposed tolerance/edition changes to the family quality manager; cross-family conflicts and rights, budget or outward-contact decisions go to the maintainer (Module agent lifecycle, revision 0, section 4, Escalation).

UNVERIFIED: a named deputy assignment and a prior clearance record are not established by the module's registry entry; issue pending (4). [docs/registry/module-routing.yaml:L484-L489]

## 9. Dependencies

| Dependency / consumer | Relationship, text-only inspection |
|---|---|
| `data/riser_database` | Five CSV tables and manifest digests; 129 total records from 27 + 5 + 3 + 84 + 10. [data/riser_database/manifest.yaml:L5-L25] |
| `scripts/riser_database` | Externalized source specification, deterministic CSV generation and manifest writer. [scripts/riser_database/build_tables.py:L162-L212] |
| `citations.schema`, `citations.resolver`, generic standards wiki | Metadata validation and wiki resolution; fixture pages test the metadata contract. [src/digitalmodel/riser_database/getters.py:L135-L141] [src/digitalmodel/riser_database/getters.py:L279-L307] [tests/riser_database/test_citations.py:L27-L30] |
| `fatigue.sn_curves`, `riser_fatigue` | Material-row vocabulary is reconciled against the actual curve loader; getter-default parity is tested. [tests/riser_database/test_material_refs.py:L22-L55] |
| `drilling_riser.stackup`, `drilling_riser.envelope` | Weight/buoyancy getter consumers and envelope-criteria consumers named by the getters and docs. [src/digitalmodel/riser_database/getters.py:L172-L205] [src/digitalmodel/riser_database/getters.py:L222-L230] [docs/riser_database.md:L202-L216] |
| `orcaflex.code_check_engine` | Both package mentions are default-parity references, at getter lines 105 and 229, not solver-interface imports. [src/digitalmodel/riser_database/getters.py:L103-L106] [src/digitalmodel/riser_database/getters.py:L225-L243] |
| OrcaFlex references in module documentation | Citation-surface follow-up; downstream code-check and weather-window references; synthetic dynamic-tier stub and future licensed replacement. [docs/riser_database.md:L105-L110] [docs/riser_database.md:L202-L216] [docs/riser_database.md:L237-L268] |
| Private stack-up registry | Handle reconciliation and provenance tests need external reference data; the private data repository was not read for this pack. [tests/riser_database/test_stackup_rig.py:L237-L260] [tests/riser_database/test_provenance_tripwire.py:L241-L284] |

### Implementation-map inventory and follow-up

The named implementation-map branch and BASE have byte-identical map blobs; the map's data revision is the one recorded in the header. [docs/registry/implementation-map.yaml:L1-L6]

In-package implementation inventory: **empty**. No family has an `implementations` row under `src/digitalmodel/riser_database/`; `loader.py` instead appears in rejected candidates and `get_buoyancy_tension_factor` appears in needs-review candidates, neither of which is an implementation row. [docs/registry/implementation-map.yaml:L30483-L30486] [docs/registry/implementation-map.yaml:L52325-L52331]

Counted families: 0; counted-family grep: `NOT_APPLICABLE`, because there are no in-package implementation rows to rank. The inventory criterion passes with an empty inventory; this does not establish map completeness. The family-selection audit uses `yaml.safe_load`, iterates `families` in file order, counts each `implementations[].path` with the package prefix, and sorts by descending count with map order as the tie-breaker.

The whole-tracked-repository follow-up search, including hidden tracked files and `scripts/`, uses `git grep -n -E 'class RiserDatabase|def _opt_float|def get_riser_dff|def get_riser_scf|def get_tension_weight_factor|def get_buoyancy_tension_factor|def get_von_mises_design_factor|def get_flexjoint_angle_limit|def get_amjig_envelope_criteria|def riser_citations|def _riser_input_row|def _pipe_row' 1dba289c5dc1bc8dc82d72e4bfe02f8dcfc22c23 -- '*.py'`; the loader/getters and external table generator are the corresponding source definitions, the additional `_opt_float` definitions in `foam_system`, `vessel_stability_screening` and `field_development` are generic numeric adapters, not independent riser factor implementations. [src/digitalmodel/foam_system/workflow.py:L429-L431] [src/digitalmodel/vessel_stability_screening/workflow.py:L989-L991] [src/digitalmodel/field_development/benchmarks.py:L572-L579] [src/digitalmodel/riser_database/loader.py:L49-L57] [src/digitalmodel/riser_database/loader.py:L217-L263] [src/digitalmodel/riser_database/getters.py:L122-L276] [src/digitalmodel/riser_database/getters.py:L310-L395] [scripts/riser_database/build_tables.py:L98-L145]

Map follow-up, issue pending (3): add or classify bounded table integrity/loading, unit-aware catalogue extraction, citation-emitting factor lookup, and wiki-side category criteria lookup; these are the section-3 method families absent as in-package implementations. The external generator is already listed under the map's `effective-tension-riser-stack-up` and `sn-curves-and-miner-damage` families, with no package row, so it is not a counted family. [docs/registry/implementation-map.yaml:L2587-L2595] [docs/registry/implementation-map.yaml:L17331-L17339] [src/digitalmodel/riser_database/loader.py:L171-L214] [src/digitalmodel/riser_database/getters.py:L294-L359]

## 10. Coverage and open items

Read scope: `__init__.py`, `loader.py`, `getters.py`; all nine `tests/riser_database/test_*.py` files; generator, seed specification, five public CSVs and manifest; matching consumer-test references and routing documents. [src/digitalmodel/riser_database/__init__.py:L8-L48] [src/digitalmodel/riser_database/loader.py:L162-L168] [data/riser_database/manifest.yaml:L1-L25]

Documentation discovery (`rg -l 'riser_database|riser.database' docs`, before this pack): `docs/riser_database.md`; `docs/maps/digitalmodel-operator-map.md`; `docs/registry/module-routing.yaml`; `docs/registry/implementation-map.yaml`; `docs/plans/2026-07-08-issue-1470-registry-batch-coverage.md`; `docs/plans/2026-08-04-issue-1961-protected-identifier-scanner.md`; `docs/plans/2026-08-05-issue-1970-artifact-generation-commit.md`; `docs/domains/data/DATA_REORGANIZATION_PLAN.md`; `docs/domains/project-docs/RISER_PHASE1_SUMMARY.md`. Matching passages are discovery context, not validation evidence; documentation reconciliation is issue pending (2).

Tests listed: `test_loader.py`, `test_schema.py`, `test_crosswalk.py`, `test_material_refs.py`, `test_citations.py`, `test_leak_gate.py`, `test_provenance_tripwire.py`, `test_stackup_rig.py`, `test_code_editions.py`, all under `tests/riser_database/`; results are recorded below and section 4 selects six regression cases. [tests/riser_database/test_loader.py:L54-L185] [tests/riser_database/test_schema.py:L62-L132] [tests/riser_database/test_citations.py:L98-L281]

At the code pin, the offline module suite records **83 run, 83 passed, 0 failed, 0 skipped, 0 NOT_RUN**. The combined module, consumer and contracts-domain command records **1,176 passed, 0 failed, 61 skipped** (1,236 collected items plus one module-level skip). The AFTER suite records **1,177 passed, 0 failed, 61 skipped**; all 1,176 BASE passing node IDs are retained, and the only added pass checks this pack's section-4 node IDs. The module subset remains **83 run, 83 passed, 0 failed, 0 skipped, 0 NOT_RUN**. [tests/riser_database/test_loader.py:L54-L185] [tests/riser_database/test_citations.py:L98-L281]

The command form is `PYTHONPATH=src uv run --no-sources --extra test python -m pytest <module, consumer and contracts-domain paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`; the prescribed absolute executable was used, with unchanged repository addopts, one worker and the same project environment. The initial provisioning launch returned exit 141 after installing dependencies and supplied no pytest session; the completed BASE command above supplies the recorded results.

The 61 baseline skips comprise one unimplemented migration module, one unavailable catenary compatibility import and 59 obsolete layout/compatibility tests associated with #949; they are outside this package. [tests/test_apistd2rd_migration.py:L18-L26] [tests/compat/test_group_compat.py:L108-L119] [tests/test_module_reorganization.py:L1-L20] [tests/test_restructure_compat.py:L15-L30]

| Open item | Disposition / responsible review | Issue |
|---|---|---|
| S01 configured-wiki fail-open | Maintainer correction; current code confirms the existing incident | #2166 |
| Numeric completeness, finite/physical domains, mixed units, unverified elevation datum/sign, YAML criteria/category shape and flattening loss, S03–S04/S12–S27/S29–S37 | Maintainer validation work; no module code changed here | issue pending (1) |
| API RP 16Q placeholder crosswalk, historical docs and standards-basis uncertainty | Maintainer metadata/documentation reconciliation; licensed clause text remains unchecked | issue pending (2) |
| Empty in-package map inventory | Maintainer map update/classification for the section-3 methods | issue pending (3) |
| No independent validated engineering example, no target model/version or G1–G3/G5 signed analysis evidence | Family quality review; proposed checks remain NOT_RUN | issue pending (4) |
| Deputy assignment and prior clearance (section 8) | Maintainer assignment and second-provider clearance | issue pending (4) |
| Private-reference reproducibility | Tests discover current private files without requiring a reference revision or digest; retain those fingerprints for reproducible qualification. [tests/riser_database/test_crosswalk.py:L23-L37] [tests/riser_database/test_stackup_rig.py:L237-L250] | issue pending (4) |
| New OrcaFlex riser citation surface remains outside this package | Existing extension issue; not treated as an implemented getter | #1247 |

No client-derived value, private sidecar or solver result is introduced by this pack; the public-only table and reference boundary follows the loader schemas and the reference-table no-value test. [src/digitalmodel/riser_database/loader.py:L60-L141] [tests/riser_database/test_schema.py:L112-L132]
