# Riser results exchange v1

`ris-exchange/1` defines portable strength and fatigue records, their validation,
the frozen twelve-case matrix and the output token registry. It performs no
response calculation, damage calculation, aggregation, plotting or engineering
qualification. Inputs and fixtures are synthetic unless their producer establishes
another basis.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-26 | Reconcile three export readers and publish shared matrix, registry and fixtures to prevent incompatible post-processing inputs. |
| 2 | 2026-10-26 | Check supplied station summaries, retain run provenance, constrain comparison cases and document source limits and complete import migration. |

## API and layout

```python
from pathlib import Path
from digitalmodel.riser_study.exchange import (
    VERSION, CASE_IDS, SEA_STATE_IDS, StrengthResult, FatigueResult,
    load_result, validate_result, load_campaign,
    load_case_matrix, validate_matrix, load_token_registry, assert_token_ids,
    load_json, validate_campaign_run,
)

strength = load_result(Path("results/R08-W1-SCR/strength.json"), "strength")
campaign = load_campaign(Path("results"))
matrix = load_case_matrix()
registry = load_token_registry()
```

`records.py` supplies typed records. `strength.schema.json` and
`fatigue.schema.json` are standalone Draft 2020-12 schemas with local references.
`exchange_schema(kind)` returns a fresh schema; the installed package includes
both schemas and the catalog data. `validate_result` applies structure, format,
finiteness and cross-field checks and returns the original mapping.
`load_result` also rejects duplicate JSON keys and nonfinite literals, including
exponent overflow. Reading never changes a field or invents a missing source.
Schema-only callers must enable format checking; `validate_result` checks dates
and timezone components independently of optional date-time dependencies.

Single-record validation retains the generic sea-state ids used by legacy
examples; campaign loading requires F03/F04/F05.

`load_campaign` requires twelve case directories, each containing exactly
`strength.json`, `fatigue-F03.json`, `fatigue-F04.json` and `fatigue-F05.json`.
It rejects extras, omissions and path/record identity disagreement. It returns
`{case_id: {"strength": record, "fatigue": {sea_state_id: record}}}`. Use the
single-record reader for additional strength load cases; campaign completeness
is separate from strength load-history completeness. No completeness helper
establishes qualification or authorizes a resolved ranking.

`data/case-matrix-v1.tsv` preserves the committed matrix bytes from PR #3463,
revision `e0d04824822c4a1f2aeb2f20841671b58d8fcd82`.
Its SHA-256 is
`f95a98735eb3882a354857fc56975a82d1b67095d6a57cb8cf565229e9ddf2c1`.
The source revision and digest are recorded in `data/manifest-v1.json`.
No cells, precision or line endings were changed. The loader preserves cells as
strings, requires the exact twelve ids and F03/F04/F05, and checks raw bytes.
`validate_matrix(rows)` compares all cells to that frozen matrix, ignoring row
order. A supplied alternative file must have the same bytes and digest.

`data/token-registry-v1.json` retains all 58 supplied analysis-list ids,
kinds and captions. The source is `maintainer-supplied analysis list, draft 3`,
revision `d3`, with source-file SHA-256
`f84d243ba2561496815ef5d537d19c00f147713f4cbe469f2b087df183996f66`.
The source is not public; no public URL is available. These supplied provenance
fields identify the private source bytes, which are unavailable for an independent
id/kind/caption/producer comparison here. Internal digest and inventory tests
establish registry stability, not independent source fidelity.
`producing_module` contains only the named module, including
non-Python producers. Both the code and manifest pin its raw-byte digest.
`assert_token_ids(module, ids)` rejects missing, unexpected and duplicate ids.
The complete inventories are 30 for `riser_strength_runs.py`, one for
`riser_configuration_plot.py`, 13 for `riser_fatigue_runs.py`, ten for
`decision_chart.py`, and one each for `wall_basis_table.py`,
`rao_disposition.md`, `review_record.md` and `case_matrix.yml`.
The fatigue inventory includes `TAB-PHB-FAT-01`, the duration-check table;
12 Phase A outputs alone do not constitute that module's complete inventory.
LF attributes preserve all digest-bound catalog and fixture files.

## Physical meaning and validation boundary

Arc length is measured in metres from hang-off zero and increases strictly.
Strength arrays share the arc grid. The maximum identifies a sampled maximum
within the consistency tolerance below; touchdown lies within the grid,
agrees with a coincident sample and cannot exceed that maximum. Intermediate
touchdown values are not interpolated. Dynamic angle and tension extrema cannot
be reversed. Optional screening records retain their fibre, envelope, independent
static comparison and combined-loading inputs; lengths, applicability and
hang-off consistency are checked. No criterion or allowable is calculated.

Fatigue stress range is nominal before SCF and thickness correction.
`cycles_per_year` is annual cycles at continuous exposure to that sea state;
histogram bin counts sum to the station total. `occurrence` is the exposure
fraction, applied exactly once by a calculating consumer. Root and station
`damage_per_year` values already include that exposure fraction. Root damage is
the maximum named-station annual damage for that state, rather than a sum over
sea states. No supplied named-station damage may exceed root damage outside the
consistency tolerance. When every named station supplies damage, root damage must
equal their maximum within that tolerance; partial coverage imposes only the
lower bound. Unnamed `arc_distribution` points do not define this named-station
summary. The contract does not recalculate or enforce agreement between the
recorded damage and the curve. Summing states at the same physical station and
then finding the maximum remains consumer work; summing state maxima is invalid.

A numeric `thickness_correction` is a positive stress multiplier. An object
records the multiplier `(thickness_mm / reference_thickness_mm) ** exponent`,
without clipping. Null or absence records no correction. No material parameters,
curve constants, reference values or endurance limits are supplied by default.
Legacy `life_years` is optional; when supplied it is the reciprocal of positive
annual damage, and null exactly when damage is zero. These are input semantics,
not a computed service-life output.

Run provenance permits a date or an RFC3339 timestamp with seconds and timezone.
The input digest has exactly 64 hexadecimal characters. `result_file` is a
relative POSIX locator with no traversal or control characters; it is never
opened by this package. Explicit qualification requires completed status and a
run id and phase. Generic records without status do not acquire qualification.
`validate_campaign_run` requires id, phase, status and qualified and accepts the
same optional `input_sha256`, `result_file`, `date` and `data_basis` fields as an
export run, with the same validation. Unknown fields remain rejected. It accepts
both minimal ledger entries and full run mappings passed by decision selection;
`validate_result(record, "g4_checks")` validates the fatigue comparison-evidence
shape only. Passing either structural check is not engineering acceptance.

### Consistency tolerance basis

`ROUND_OFF = 1e-9` is an explicit contract policy for comparisons of supplied
values. It is not derived from a solver precision requirement or a measured
error bound. With `math.isclose`, comparisons use
`abs(a-b) <= max(1e-9 * max(abs(a), abs(b)), 1e-9)` in the recorded units.
Independently rounding two values to nine decimal places can differ by at most
`1e-9`, motivating the absolute allowance near unity; larger magnitudes use the
relative allowance. This policy does not establish that any producer actually
uses that precision. Arc identity uses only the absolute allowance; reciprocal
life and histogram cycle totals use only the relative allowance. Tests place
values at 0.99 and 1.01 times the relevant boundary near unity, at larger
magnitudes and on both sides of the station summary. Numeric inputs must be representable as finite binary64
values before arithmetic checks, including JSON integers. No engineering
acceptance criterion follows from these consistency checks.

## Field provenance

Paths below are relative to `src/digitalmodel/`, except where `tests/` is shown.
These references quote the inspected PR heads; they do not imply those changes
are present on the default branch.

| PR and revision | File and lines | Supported fields |
| --- | --- | --- |
| #3517 `cd6e9edc` | `subsea/riser_study/results_exchange.py:167` and `:177` | Strength arrays, `method`, object `allowable`, optional `screening`. |
| #3517 `cd6e9edc` | `subsea/riser_study/results_exchange.py:152` and `:291` | Screening condition, qualification, both stress fibres, envelopes, excursion inputs, static comparison, combined-loading checks. |
| #3517 `cd6e9edc` | `tests/subsea/riser_study/test_riser_study.py:145` and `:162` | Dynamic, functional static and extreme static screening fixture forms. |
| #3517 `cd6e9edc` | `subsea/riser_study/results_exchange.py:215` and `:225` | Curve `source`, numeric correction, scalar or histogram ranges, reciprocal or null life. |
| #3518 `c399d80e` | `riser_study/results_exchange.py:153` and `:163` | `utilisation_method` and numeric `utilisation_allowable`, general load metadata objects. |
| #3518 `c399d80e` | `riser_study/results_exchange.py:29` and `:93` | Run `data_basis`, optional life and `arc_distribution`. |
| #3518 `c399d80e` | `riser_study/results_exchange.py:48` and `:57` | Curve `source_label`; object, null or absent thickness correction; reference cycles/range and endurance limit. |
| #3518 `c399d80e` | `riser_study/results_exchange.schema.json:515` | Fatigue comparison cases, mesh-step and half-hour evidence. |
| #3519 `87d262da` | `riser_study/results_exchange.py:34` and `:43` | Run ledger, optional id/phase/status/qualified, date or timestamp. |
| #3519 `87d262da` | `riser_study/results_exchange.py:71` and `:111` | Optional allowable `basis_label`, optional `utilisation_basis`. |
| #3519 `87d262da` | `riser_study/results_exchange.py:148` and `:170` | Station `damage_per_year`, source-labelled numeric correction and optional legacy life. |
| #3519 `87d262da` | `tests/riser_study/test_decision_chart.py:62` and `:80` | Empty load metadata and recorded station damages. |

## Shared fixtures and hand checks

`tests/subsea/riser_exchange/fixtures/exports-v1.json` is a path-keyed bundle of 48
clearly labelled synthetic exports: twelve strength and 36 fatigue records.
This avoids adding many individual result files. The `campaign` fixture in
`tests/subsea/riser_exchange/test_exchange.py` writes those paths into its temporary
folder to exercise the real reader. Consumers can use the same bundle and
materialization pattern; it contains no licensed outputs. `manifest-v1.json`
pins raw fixture bytes and each valid record's canonical JSON digest.
Canonical records use UTF-8, sorted keys, compact separators, finite numbers and
no terminal newline. `malformed-v1.json` contains an impossible touchdown maximum
and inconsistent reciprocal life, with expected error fragments. Its raw bytes
are also pinned. `peer-examples-v1.json` additionally retains five unmodified
example records from the three inspected PR test files, with revision and
file:line references and a raw-byte digest in the fixture manifest.

For `R08-W1-SCR`, strength arc samples are 0, 50 and 100 m; utilisations are
0.4, 0.6 and 0.5. The sampled maximum is 0.6 at 50 m; touchdown is 0.5 at 100 m.
For F03, use the explicitly synthetic power law `N = 10**12 / range_MPa**3`,
SCF 1, thickness multiplier 1, one million continuous-exposure cycles per year
and occurrence 0.2. Ranges 100 and 50 MPa give annual damages 0.2 and 0.025.
The recorded state maximum is 0.2 and legacy reciprocal life is 5 years.
The same data repeat across the other cases to isolate contract behavior.
`R12-W2-SLWR/F05` instead has true zero range and damage, with null life.
The tests calculate these hand values independently; production validation
performs no damage calculation.

Run the focused suite with an external temporary directory:

```bash
PYTHONPATH=src uv run --no-project --with pytest --with pytest-asyncio --with jsonschema python -m pytest tests/subsea/riser_exchange -q -rA -p no:cacheprovider --basetemp="$CHECK_TMP"
```

## Consumer migration

These instructions were checked against the diffs for #3517 at `cd6e9edc`,
#3518 at `c399d80e` and #3519 at `87d262da`. Paths are repository-relative.
The consumer branches are unchanged. Delete each redundant module and schema
only after switching every reference listed below. Keep the shared package-data
entries `riser_study/exchange/*.schema.json`, `riser_study/exchange/data/*.json`
and `riser_study/exchange/data/*.tsv` in `pyproject.toml`.

### #3517: strength

- In `src/digitalmodel/subsea/riser_study/riser_strength_runs.py`, replace
  `from .results_exchange import load_result` with imports of `load_result`,
  `CASE_IDS`, `load_case_matrix` and `assert_token_ids` from
  `digitalmodel.riser_study.exchange`. Replace the local `CASES` construction
  with `set(CASE_IDS)` and compare governing rows with the loaded frozen matrix.
  Retain load-history coverage checks; `load_campaign` represents a single
  strength summary per case and does not replace them. Assert all 30 registered
  strength ids after production.
- In `src/digitalmodel/subsea/riser_study/riser_configuration_plot.py`, replace
  `from .results_exchange import portable_path` with
  `from digitalmodel.riser_study.exchange import portable_path, assert_token_ids`.
  Assert its separate `SCH-01` inventory. The `portable_path` switch is required
  before deleting the local module.
- In `tests/subsea/riser_study/test_riser_study.py`, switch the `exchange()`
  helper from `digitalmodel.subsea.riser_study.results_exchange` to
  `digitalmodel.riser_study.exchange`. Replace the adjacent schema-file read in
  `test_schema_accepts_both_contracts` with `exchange_schema(kind)` for each
  record kind, imported from the shared package. Keep validation, geometry,
  plotting, CLI and calculation assertions. Shared fixture bundles may replace
  local interchange builders without removing consumer-specific assertions.
- Delete `src/digitalmodel/subsea/riser_study/results_exchange.py` in full,
  including its local record classes, reader, schema selection and validation;
  delete `src/digitalmodel/subsea/riser_study/results_exchange.schema.json`.
  Remove the obsolete `subsea/riser_study/*.schema.json` package-data entry.
  In `src/digitalmodel/subsea/riser_study/README.md`, replace both local contract
  references with the common package and this contract's field-form guidance.
- After removal, import `riser_strength_runs` and `riser_configuration_plot`
  and run `tests/subsea/riser_study/test_riser_study.py`. Preserve screening
  extensions; adapt calculation readers to the alternate strength basis pair
  when accepting generic records.

### #3518: fatigue

- In `src/digitalmodel/riser_study/riser_fatigue_runs.py`, replace
  `from .results_exchange import validate_result` with imports of
  `validate_result`, `load_json`, `CASE_IDS`, `SEA_STATE_IDS`, `load_case_matrix`
  and `assert_token_ids` from `digitalmodel.riser_study.exchange`. Use strict
  `load_json` for export reads; use the shared case and sea-state sets and frozen
  matrix for production coverage. Retain local fatigue calculation.
- In `tests/subsea/riser_study/test_riser_fatigue_runs.py`, change `schema()`
  to import `digitalmodel.riser_study.exchange`. Change the no-argument
  `schema().exchange_schema()` in
  `test_shared_schema_is_valid_and_rejects_wrong_kind` to
  `schema().exchange_schema("fatigue")`. Other schema selections must explicitly
  pass `strength` or `g4_checks` as appropriate; existing `validate_result`
  calls keep their explicit kinds.
- Delete `src/digitalmodel/riser_study/results_exchange.py` in full, including
  its local record classes, cached schema loader and validation; delete
  `src/digitalmodel/riser_study/results_exchange.schema.json`. Remove the
  obsolete `riser_study/*.schema.json` package-data entry. In
  `src/digitalmodel/riser_study/README.md`, replace the two local contract
  references with the common package and these explicit schema-kind calls.
- Adapt curve reading to the sole supplied `source` or `source_label`.
  A numeric thickness correction is a stress multiplier; an object uses its
  recorded power law; absent/null means no correction. Apply occurrence exactly
  once to continuous-exposure cycles; supplied damage already includes it.
  Preserve histogram, arc-distribution, G4 and zero-damage assertions.
- Assert the complete 13-id inventory, including `TAB-PHB-FAT-01`; the current
  twelve Phase A ids alone are incomplete. After removal, import
  `riser_fatigue_runs` and run
  `tests/subsea/riser_study/test_riser_fatigue_runs.py`. Calculation underflow
  and saved-byte artifact hashing remain separate consumer concerns.

### #3519: decision

- In `src/digitalmodel/riser_study/decision_chart.py`, switch the absolute
  `digitalmodel.riser_study.results_exchange` import to
  `digitalmodel.riser_study.exchange`, retaining `load_json`,
  `validate_campaign_run` and `validate_result`. Also import `CASE_IDS`,
  `SEA_STATE_IDS`, `load_case_matrix`, `validate_matrix` and `assert_token_ids`.
  `_eligible` may continue passing the entire export run, including provenance;
  no four-field projection is required. Retain ledger linkage and qualification.
- In `tests/riser_study/test_decision_chart.py`, switch the `load_json` and
  `validate_result` import to the common package. Keep all selection,
  qualification, station aggregation and unresolved-output assertions.
- Delete `src/digitalmodel/riser_study/results_exchange.py` in full, including
  its local record classes, JSON reader and schema validation; delete
  `src/digitalmodel/riser_study/results.schema.json`. Remove the obsolete
  `riser_study/*.json` package-data entry if no other retained data requires it.
  In `src/digitalmodel/riser_study/README.md`, replace both local contract
  references with the common package and shared provenance rules.
- Require exact shared case and sea-state sets before selection. Use
  `validate_matrix` for full matrix rows; an id-only manifest must agree with
  `load_case_matrix`. Retain same-station aggregation and upstream resolution
  checks. Assert the ten decision ids. After removal, import `decision_chart`
  and run `tests/riser_study/test_decision_chart.py`. Legacy life may be null
  only at zero damage and otherwise must be reciprocal.

For all three integrations, materialize shared exports using the `campaign`
fixture pattern in `tests/subsea/riser_exchange/test_exchange.py`. Resolve the
shared parent-package metadata together. No forwarding modules or schema copies
are required. Search source, tests, documentation and package metadata for each
deleted module/schema name before completing integration. Import smoke checks
cover import compatibility only; consumer calculations require their own suites.

## CHANGELOG

### ris-exchange/1

- Accept either `method` with object `allowable`, or `utilisation_method` with
  numeric `utilisation_allowable`. Both complete pairs describe a stress
  denominator in MPa. Reject mixed or simultaneous pairs, even if their values
  agree; ambiguous precedence is not permitted. The legacy numeric form carries
  no source label, and reading does not invent one.
- Accept either curve `source` or `source_label`. Reject both together with a
  source-form error. Preserve the supplied name rather than silently aliasing it.
- Retain numeric, object, null and absent thickness forms with the explicit
  stress-multiplier meanings above. Reject nonpositive numeric correction and
  incomplete object correction. They are distinct representations, not aliases
  to merge or defaults to infer.
- Retain optional life and accept zero damage with null or absent life; reject
  zero damage with numeric life and positive damage with null or a nonreciprocal
  life. The decision schema's positive-only life interpretation is retired.
- Retain optional `basis_label`, `utilisation_basis`, `arc_distribution`, station
  damage, run status and fatigue comparison evidence. Unknown fields outside
  the explicitly open wave/current/offset metadata objects are rejected.
- Permit a single hang-off sample for legacy strength records and empty load
  metadata objects. Screening fibre/envelope extensions still require their
  recorded multi-station arrays and aligned lengths.
- Tighten all readers to exact matrix case ids, strict JSON,
  finite numbers, portable provenance, bounded timezone offsets and consistent
  touchdown maxima. Rejected alternatives now raise `ValueError` without
  echoing supplied values.
- `exchange_version` may be absent in legacy records. If present, it must be
  exactly `ris-exchange/1`. New writers should include it. Catalogs and fixture
  manifests always carry the version.

Consumer integration remains in #3517, #3518 and #3519. Replacing their imports
alone does not adapt calculation code that assumes one field form. The migration
note accompanying this contract specifies those required changes. Their branches
and calculation behavior are outside this change.
