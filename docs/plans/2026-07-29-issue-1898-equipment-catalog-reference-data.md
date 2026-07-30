# Plan for #1898: Catalogued artificial-lift equipment reference data

> **Status:** adversarial-reviewed — user approval checkpoint pending
> **Complexity:** T3
> **Date:** 2026-07-29
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1898
> **Client:** N/A
> **Lane:** lane:codex
> **Execution mode:** parallel-readonly planning; single-lane implementation
> **Review artifacts:** `scripts/review/results/2026-07-29-plan-1898-claude.md` | `scripts/review/results/2026-07-29-plan-1898-codex.md` | `scripts/review/results/2026-07-29-plan-1898-agy.md`
---
## Resource Intelligence Summary

### Existing repo code

- `src/digitalmodel/marine_ops/artificial_lift/` will have no packaged
  equipment-reference catalog or strict lookup API before this work.
- `src/digitalmodel/well/tubulars/{casing,sucker_rod}.py` will remain separate
  standards-oriented product models, not previous-project catalog preservation.
- Existing dynacard rod constants/models will remain for a later wiring issue.
- `src/digitalmodel/marine_ops/artificial_lift/dynacard/everitt_jennings/adapter.py`
  will remain the owner of `DEFAULT_TUBING_ID_IN`. The current safe models will
  not provide the nominal tubing OD-and-weight key needed for an unambiguous
  lookup.
- `pyproject.toml` will require an explicit package-data glob so versioned CSV
  and YAML resources ship in distributions.

### Source catalog preflight

Only catalog sheets will be read. Mixed operational/well sheets will be
excluded. Workbook-relative paths will be resolved below a caller-supplied
source root; no machine-specific absolute source path will enter tracked files.

| Source | Sheet | Exact worksheet dimensions | Candidate catalog rows | Planned use |
|---|---|---:|---:|---|
| `data/2018/Sucker Rod Pump Database 02_23_18.xlsx` | `Surface Unit Catalog` | 3,594 x 34 | 3,593 | surface-unit geometry |
| same workbook | `Rods Catalog` | 942 x 8 | 940 after metadata row | lossless safe preservation |
| same workbook | `Rods Guide Catalog` | 113 x 3 | 112 | preserved guide catalog |
| `REF/Rod Detail Table.xlsx` | `Sheet1` | 943 x 6 | 942 | canonical rod properties |
| `REF/Rod Coupling/nexus_catalog_couplings.xlsx` | `Sheet1` | 117 x 8 | 116 | coupling properties |
| `data/Rodpump Pumping Unit (1).xlsx` | `Sheet1` | 3,581 x 40 | 3,580 | lossless safe preservation |
| `data/2018/UniqueRodODData.xlsx` | `Rod ODs` | 34 x 2 | 33 | rod/connection preservation |
| same workbook | `Look-up` | 8 x 4 | 3 visible mappings | coupling-map cross-check only |
| `data/2018/Rod Parts Data.xlsx` | `DATA_2` | 296 x 10 | 295 | excluded: aggregate installation data, not a clean catalog |
| requested tubing workbook | `Tubing Stretch Table` | unavailable | 0 | no extraction or API data |

The tubing workbook will be treated as unavailable because the specified file
will not exist below the supplied source tree at planning time and a bounded
filename search will find no replacement. The extractor will report this
source as unavailable and will never synthesize tubing records.

Candidate counts describe occupied ranges, not final outputs. Extraction will
record source, blank, duplicate, conflict, quarantine, and emitted counts.

### Catalog semantics and key risks

- `Rod Detail Table` will express modulus in million psi and rounded sonic
  velocity in thousands of ft/s. Its `Unit Weight` will be lb/ft, not material
  density. Raw values and explicitly unit-normalized values will both survive.
- Rod grade/diameter labels will require whitespace-and-hyphen normalization.
  Identical duplicates such as `97  - 0.750` and `97-0.750` will collapse;
  conflicting duplicates will fail extraction.
- Coupling rod diameter alone will be non-unique across manufacturer, size, and
  type. A lookup will either return all matches or require enough filters to
  identify one record; it will never select the first row.
- Surface sources will retain source-qualified identifiers. No heuristic
  crosswalk will equate compact manufacturer codes, numeric IDs, geometry codes,
  or differently formatted descriptions. Exact typed duplicates may collapse;
  conflicts will remain distinct and require source qualification.
- `manufacturer_key`/`model_key` will come from `P.Unit Manf.` plus
  `Pumping Unit Description / Information` in the first source and
  `PumpingUnitManufacturer` plus `Description` in the second.
- Rod-guide blank manufacturer/model cells will be explicit fill-down values
  during extraction, with the transformation recorded in the manifest.
- Rod-connection values will retain raw text plus nullable parsed values. A
  strict fraction/decimal parser and coupling-catalog cross-check will quarantine
  suspect rows rather than interpreting them as valid engineering sizes.
- Numeric cells will parse as `Decimal(str(cell))`; deduplication will require
  exact numeric equality (`1 == 1.0`) with no rounding or tolerance for any
  area, weight, modulus, velocity, tensile, dimension, rating, angle, or
  coefficient. Key text will strip/collapse whitespace and normalize case and
  spacing around hyphens; descriptive text will only strip outer whitespace.
  Lineage will retain every source row. Any other disagreement will fail.

### House style and packaging

Source-specific CSVs will carry comment headers for provenance, extraction date,
schema, and units. A versioned YAML manifest will carry digests, transformations,
availability, and counts. `importlib.resources.files()` will load them, following
existing motion-forecast and naval-architecture packaged-data loaders.

### Evidence and reproduction

**Issue state** (verified 2026-07-29): OPEN; labels `priority:high`,
`cat:data`; plan, approval marker, and review artifacts absent. Implementation
will remain blocked pending adversarial review and explicit user approval.

**Parallel work** (verified 2026-07-29): `codex/1896-cardcompare` owns card
comparison/parity; `codex/1893-buckling-datum` owns rod buckling; this worktree
is clean. The planned file set will avoid every reserved file and test.

**Physical reproduction proof** (catalog row `97 - 0.875`):
```text
literal source velocity = 16.0 thousand ft/s (rounded)
area = 0.601 in^2
unit weight = 2.22 lbf/ft
E = 30.5e6 psi
g_c = 386.0886 lbm*in/(lbf*s^2)
weight-derived density = 2.22 / (0.601 * 12) = 0.307822 lbm/in^3
c = sqrt(E * g_c / density) / 12 = 16,299.8 ft/s
independent physical target = 16,300 ft/s
relative difference ~= 0.001%
```
This will become an independent unit-handling test with a required difference
below 1% from the physical target. The extractor will preserve the rounded
source value without rewriting it. It will report the full residual distribution
for each rod source; it will not apply a blanket 1% rejection threshold unless
the extraction evidence supports one.

### Gaps identified

- No deterministic extractor, versioned catalog package, strict lookup API, or
  manifest will exist before this work.
- The requested tubing workbook will be unavailable; tubing data and tubing
  wiring will remain explicitly incomplete.
---
## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-07-29-issue-1898-equipment-catalog-reference-data.md` |
| Extraction script | `scripts/artificial_lift/extract_reference_catalogs.py` |
| Versioned manifest | `src/digitalmodel/marine_ops/artificial_lift/reference_data/v1/manifest.yml` |
| Versioned catalog CSVs | `src/digitalmodel/marine_ops/artificial_lift/reference_data/v1/*.csv` |
| Lookup API | `src/digitalmodel/marine_ops/artificial_lift/reference_catalog.py` |
| Lookup tests | `tests/marine_ops/artificial_lift/test_reference_catalog.py` |
| Extractor tests | `tests/scripts/artificial_lift/test_extract_reference_catalogs.py` |
| Review artifacts | `scripts/review/results/2026-07-29-plan-1898-*.md` |
---
## Deliverable

A deterministic extractor will preserve catalog-only artificial-lift equipment
data as versioned CSV/YAML resources, and a strict packaged API will expose rod,
coupling, and surface-unit lookups with physical and provenance validation.
---
## Proposed Design and Pseudocode

### 1. Deterministic, allowlisted extraction

```text
extract_catalogs(source_root, output_dir, extraction_date):
    resolve only fixed relative workbook paths below source_root
    reject path traversal and missing required rod/coupling/surface sources
    report the optional tubing source as unavailable when absent
    read only named catalog sheets in read-only/data-only mode
    select only explicit allowlisted columns
    normalize grade, diameter, manufacturer, model, and numeric units
    collapse only exact typed numeric equals after representation-only Decimal
        normalization; apply no rounding or tolerance
    retain source-row lineage and quarantine unresolvable connection mappings
    reject conflicting duplicates and malformed required values
    calculate source/rejected/deduplicated/emitted counts
    calculate SHA-256 for every source workbook and emitted data file
    write the complete version tree to a temporary sibling
    validate staged bytes, hashes, counts, schemas, and confidentiality
    atomically rename staging to an absent target; never overwrite v1 in place
    on existing target, --check will extract to temporary storage and compare
```
The script will require a source-root argument, accept a test output directory,
and default only the output to the repo-relative versioned data destination.

### 2. Versioned data contract

```text
manifest:
    schema_version: "1.0"
    catalog_version: "v1"
    provenance: "previous project reference"
    extraction_date: "2026-07-29"
    sources:
        relative workbook, sheet, sha256, availability, exact counts
    outputs:
        relative CSV, sha256, columns, units, emitted count
    transformations:
        normalization, fill-down, deduplication, exclusions
```
The extractor will emit lossless safe, source-specific `rod_details.csv`,
`rods_catalog.csv`, `surface_unit_catalog.csv`, `rodpump_units.csv`,
`couplings.csv`, `rod_guides.csv`, and `rod_connections.csv`. Every source row
will receive an emitted, duplicate-with-lineage, rejected, or quarantined
disposition in the manifest/lineage data. No `tubing.csv` will be emitted while
the requested source is unavailable.

Public/output schemas will be explicit:
```text
RodProperties: area_in2, unit_weight_lbf_ft, modulus_psi,
               catalog_sonic_velocity_ft_s, weight_derived_velocity_ft_s,
               tensile_strength_psi, raw_sonic_velocity_kft_s
CouplingProperties: rod_diameter_in, manufacturer, size, coupling_type,
                    coupling_diameter_in, coupling_length_in,
                    tensile_strength_psi, friction_coefficient
SurfaceUnitGeometry: source_catalog, manufacturer_key, model_key, geometry_code,
                     gearbox_rating_raw, beam_rating_raw, max_stroke_length_raw,
                     dimensional_a/c/i/k/p_raw, stroke_length_pin_1..8_raw,
                     radius_pin_1..8_raw, structural_imbalance_raw,
                     phase_angle_raw, counterbalance_effect_raw,
                     air_balance_raw, air_balance_dimensional_d/f/h_raw
```
Surface fields will remain `raw_*` until a source data dictionary verifies
units; the manifest will mark their unit as `unverified_source_unit`. The API
will not imply inches, force, or torque units that the workbook does not state.

### 3. Strict immutable lookup API

```text
load_catalog(version="v1"):
    load and validate manifest through importlib.resources
    verify every packaged CSV digest and declared row count
    parse comment-headed CSVs into frozen dataclasses
    build immutable indexes
    cache only after all validation succeeds

rod_properties(grade, diameter_in):
    NFKC-normalize/strip/uppercase grade; parse diameter as finite Decimal
    reject bool, NaN, infinity, malformed composites, and unsupported precision
    return RodProperties(area_in2, unit_weight_lbf_ft, modulus_psi,
                         catalog_sonic_velocity_ft_s,
                         weight_derived_velocity_ft_s, tensile_strength_psi,
                         raw_sonic_velocity_kft_s)
    raise KeyError with the normalized key when unknown

find_couplings(rod_diameter_in, manufacturer=None, size=None, type=None):
    apply the shared strict diameter/text-key grammar
    return an immutable tuple of every matching CouplingProperties record
    return an empty tuple when no match exists

coupling_properties(rod_diameter_in, manufacturer=None, size=None, type=None):
    call find_couplings
    raise KeyError when empty
    raise AmbiguousCatalogKeyError when more than one record remains
    return the sole explicitly unit-suffixed CouplingProperties record

find_surface_units(manufacturer, model, source_catalog=None):
    match source-qualified manufacturer code/ID plus exact normalized description
    return every immutable geometry/rating match without cross-source heuristics

surface_unit_geometry(manufacturer, model, source_catalog=None):
    call find_surface_units
    raise KeyError when unknown
    raise AmbiguousCatalogKeyError unless exactly one record remains
    return the sole source-qualified SurfaceUnitGeometry raw-value record
```
Every diameter API will accept finite positive `str|int|float|Decimal` except
`bool`, preserve at most three fractional digits without quantization, and
reject malformed/non-finite values. Every text key will require nonblank
`str`, apply Unicode NFKC, strip/collapse whitespace, and casefold; `None` will
mean “no filter” only for optional filters. `source_catalog` will be a closed
enum. Source composite rod labels will split only at the final diameter suffix,
so internal grade hyphens survive.
The module will not expose an always-failing tubing facade or fallback table.

### 4. Physical validation

```text
report_rod_physics(record):
    density_lbm_in3 = weight_lbf_ft / (area_in2 * 12)
    computed_ft_s = sqrt(modulus_psi * g_c / density_lbm_in3) / 12
    record residual against preserved rounded catalog velocity
```
The extraction will report this check for every complete rod record. The focused
hard test will independently hand-check the 0.875-inch `97` record against the
16,300-ft/s physical target and will not call a production helper for expected data.
---
## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `scripts/artificial_lift/extract_reference_catalogs.py` | one-shot, deterministic extraction |
| Create | `src/digitalmodel/marine_ops/artificial_lift/reference_data/__init__.py` | packaged-resource marker |
| Create | `src/digitalmodel/marine_ops/artificial_lift/reference_data/v1/manifest.yml` | version/provenance/count contract |
| Create | `src/digitalmodel/marine_ops/artificial_lift/reference_data/v1/*.csv` | preserved human-readable catalogs |
| Create | `src/digitalmodel/marine_ops/artificial_lift/reference_catalog.py` | strict lookup API |
| Create | `tests/marine_ops/artificial_lift/test_reference_catalog.py` | lookup/data/physics tests |
| Create | `tests/scripts/artificial_lift/test_extract_reference_catalogs.py` | extractor behavior and confidentiality tests |
| Modify | `pyproject.toml` | include versioned YAML/CSV package data |
| Modify | `docs/plans/README.md` | index this plan and status |

No reserved artificial-lift implementation or test file will change. No tubing
default will be wired.
---
## TDD Test List

Each production behavior will be introduced by a failing test and observed RED
before its minimal implementation.

| Test name | Break it catches | Expected behavior |
|---|---|---|
| `test_extracts_only_allowlisted_catalog_columns` | operational/private columns leak into output | emitted headers equal frozen catalog allowlists |
| `test_scans_allowlisted_free_text_for_prohibited_content` | an identifier hides in a description | extraction rejects the synthetic prohibited value |
| `test_extractor_requires_source_root_argument` | machine path becomes an implicit default | missing argument exits with a clear parser error |
| `test_extractor_rejects_relative_path_escape` | source resolution escapes the supplied root | traversal raises `ValueError` |
| `test_extractor_collapses_typed_equivalent_rods_with_lineage` | formatting duplicates lose provenance | one normalized row plus every source-row reference |
| `test_near_engineering_values_do_not_deduplicate` | rounding merges distinct parts | near rod/coupling/geometry values remain distinct |
| `test_extractor_rejects_conflicting_normalized_rods` | conflicting properties silently overwrite | extraction raises a contextual conflict error |
| `test_extractor_records_missing_tubing_source` | absent workbook is hidden or fabricated | manifest marks unavailable and no tubing CSV exists |
| `test_each_safe_source_row_has_a_disposition` | cross-check-only data silently disappears | emitted/duplicate/rejected/quarantined totals reconcile |
| `test_suspect_connection_mapping_is_quarantined` | ambiguous numeric OD becomes engineering data | raw value survives; parsed value remains null |
| `test_extractor_is_byte_deterministic` | reruns create noisy or unstable data | two frozen-date runs produce identical bytes |
| `test_failed_extraction_preserves_prior_tree` | partial output replaces valid data | injected failure leaves prior bytes unchanged |
| `test_manifest_counts_and_hashes_match_outputs` | report counts/digests drift from emitted data | every declared count and SHA-256 verifies |
| `test_rod_properties_spot_checks_catalog_values` | wrong columns or unit scaling enter API | hand-selected grade/diameter rows match literals |
| `test_rod_sonic_velocity_matches_independent_physics` | MOE, weight, area, or velocity units are wrong | computed 16,299.8 ft/s is within 1% of 16,300 |
| `test_unknown_rod_key_raises_without_default` | unknown rods silently receive defaults | clear `KeyError` includes normalized key |
| `test_all_lookup_key_grammars_reject_invalid_inputs` | malformed keys alias records | parameterized rod/coupling/surface rejection |
| `test_coupling_lookup_requires_disambiguation` | diameter-only lookup chooses arbitrary coupling | multi-match query raises ambiguity error |
| `test_coupling_filters_return_catalog_od` | filters/index fields are crossed | exact filtered record returns literal OD/length |
| `test_coupling_schema_has_explicit_units` | OD, length, tensile, or friction fields swap | exact unit-suffixed schema |
| `test_unknown_coupling_key_raises_without_default` | missing coupling silently defaults | clear `KeyError` |
| `test_surface_unit_geometry_spot_check` | geometry columns shift or model key is wrong | source-qualified model returns literal A/C/I/K/P and ratings |
| `test_surface_unit_cross_source_ambiguity_raises` | incompatible IDs are heuristically merged | unqualified multi-match raises ambiguity |
| `test_surface_unknown_units_remain_raw` | unverified units gain false engineering meaning | raw fields plus manifest unit marker |
| `test_unknown_surface_unit_raises_without_default` | missing unit silently defaults | clear `KeyError` |
| `test_wheel_contains_and_loads_catalog_resources` | source checkout hides package-data omission | wheel resources load and hash-verify outside repo |

Generated-data tests will use hand-derived literals. Extractor tests will build
small synthetic workbooks and will not depend on the off-repo source.
---
## Acceptance Criteria

- [ ] The implementation will touch only the new catalog script/data/API/tests,
      `pyproject.toml`, and lifecycle artifacts listed above.
- [ ] The extractor will read every available named catalog source and will
      report actual source, rejected, duplicate, and emitted counts.
- [ ] Every safe row from both rod and both surface sources will have an auditable
      emitted, duplicate-with-lineage, rejected, or quarantined disposition.
- [ ] Every emitted CSV will carry the required provenance and extraction-date
      comment header.
- [ ] The YAML manifest will verify source/output SHA-256 values, units,
      transformations, availability, and actual counts.
- [ ] No machine-specific absolute path, well identifier, well name, production
      rate, or other operational field will enter tracked artifacts.
- [ ] Rod lookup will return explicitly unit-suffixed area, coupling-inclusive
      weight per foot, modulus, raw/normalized sonic velocity, and tensile strength.
- [ ] Coupling lookup will never silently choose among diameter matches.
- [ ] Surface-unit lookup will return geometry by source-qualified manufacturer
      and model/description; unqualified ambiguity will raise clearly.
- [ ] Unknown exact keys will raise clear exceptions; collection queries will
      return empty immutable tuples rather than defaults.
- [ ] The independent 0.875-inch rod calculation will reproduce the 16,300-ft/s
      physical target within 1% (expected approximately 16,299.8 ft/s), while
      preserving the literal rounded source velocity.
- [ ] The unavailable tubing workbook will be reported honestly; no tubing
      catalog or wiring will be fabricated.
- [ ] Focused tests will pass with
      `PATH=.venv/bin:$PATH pytest tests/marine_ops/artificial_lift/test_reference_catalog.py tests/scripts/artificial_lift/test_extract_reference_catalogs.py -q`.
- [ ] Exact regression tests will pass:
      `PATH=.venv/bin:$PATH pytest tests/well/tubulars/test_casing.py tests/well/tubulars/test_sucker_rod.py -q`.
- [ ] Regeneration will pass:
      `tmp_dir="$(mktemp -d)"; trap 'rm -rf "$tmp_dir"' EXIT; .venv/bin/python scripts/artificial_lift/extract_reference_catalogs.py --source-root "$CATALOG_SOURCE_ROOT" --output-dir "$tmp_dir/v1" --extraction-date 2026-07-29; diff -ru src/digitalmodel/marine_ops/artificial_lift/reference_data/v1 "$tmp_dir/v1"`.
- [ ] After staging the exact intended set, `git diff --quiet` and
      `test -z "$(git ls-files --others --exclude-standard)"` will prove no
      unstaged/untracked artifact can evade scanning; the cached file list will
      be nonempty and reconciled to the artifact map.
- [ ] Legal/path gates will pass:
      `repo_rel="$(realpath --relative-to="$WORKSPACE_HUB_ROOT" "$(git rev-parse --show-toplevel)")"; (cd "$WORKSPACE_HUB_ROOT" && bash scripts/legal/legal-sanity-scan.sh --repo="$repo_rel" --diff-only); mapfile -t code_paths < <(git diff --cached --name-only --diff-filter=ACMR -- '*.py' '*.sh'); bash "$WORKSPACE_HUB_ROOT/scripts/enforcement/check-no-abs-paths.sh" "${code_paths[@]}"`.
- [ ] Wheel packaging will pass:
      `wheel_tmp="$(mktemp -d)"; trap 'rm -rf "$wheel_tmp"' EXIT; .venv/bin/python -m pip wheel . --no-build-isolation --no-deps -w "$wheel_tmp/wheels"; .venv/bin/python -m pip install --no-deps --target "$wheel_tmp/site" "$wheel_tmp"/wheels/*.whl; (cd "$wheel_tmp" && PYTHONPATH="$wheel_tmp/site" .venv/bin/python -c 'from digitalmodel.marine_ops.artificial_lift.reference_catalog import load_catalog; load_catalog()')`.
- [ ] Adversarial code/artifact review will complete before issue closeout.
- [ ] The implementing agent will post a summary comment on issue #1898, but
      will not push, open a PR, or close the issue unless separately authorized.
---
## Adversarial Review Summary

| Reviewer | Verdict | Key findings |
|---|---|---|
| Claude | UNAVAILABLE | checkout trust gate; two attempts, final timeout 124 |
| Codex | APPROVE | two independent lanes; final rounds found no defects |
| Agy | UNAVAILABLE | authentication required; exit 1 |

**Overall result:** PASS under documented provider degradation; user approval pending.

All three provider verdicts will be required. A genuine quota/service/auth
outage will produce an `UNAVAILABLE` artifact with command/exit evidence and an
explicit T3-to-T2 degradation. `INVALID_OUTPUT` or parse failure will block.
---
## Risks and Open Questions

- **BLOCKER — approval:** Issue #1898 will have no approved-plan evidence until
  the user explicitly approves this reviewed plan.
- **Missing source:** The absent tubing `.xls` will block tubing data/API/wiring.
- **Rounded source velocity:** The literal 16.0 kft/s and independently derived
  16,299.8 ft/s will remain separate API fields; extraction will not “correct”
  the catalog. The user-supplied 16,300-ft/s target will validate unit handling.
- **Ambiguity:** Couplings will require filters; surface sources and unverified
  units will stay separate/raw.
---
## Complexity: T3

Multiple workbook schemas, confidentiality, units, ambiguous keys, packaging,
and physical validation make this T3; implementation will remain single-lane.
