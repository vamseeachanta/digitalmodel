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

`data/token-registry-v1.json` transcribes all 58 supplied analysis-list ids,
kinds and captions. `producing_module` contains only the named module, including
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
within a serialization tolerance of `1e-9`; touchdown lies within the grid,
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
sea states. The contract does not recalculate or enforce agreement between the
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
`validate_campaign_run` validates a ledger entry;
`validate_result(record, "g4_checks")` validates the fatigue comparison-evidence
shape only. Passing either structural check is not engineering acceptance.

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
