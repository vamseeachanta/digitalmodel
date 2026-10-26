# Riser strength and fatigue decision outputs

`decision_chart.py` joins qualified strength exports to annual benchmark damage
at matching stations. The chart uses the maximum qualified Phase A U1 on x and
the maximum of the station sums over F03, F04 and F05 on y. A station sum is
formed before selecting the maximum; summing the separate sea-state maxima
would combine different physical locations. Annual damages are already
weighted by the exporter and are added without further occurrence weighting.

U1 at or below one meets the strength screen. The chart has one screening line
at U1 = 1, colours for nominal sizes, markers for configurations, and links
between W1 and W2 within each size and configuration. Positive damage uses a
logarithmic axis. True zero is labelled in a separate strip at the axis edge;
it is never replaced with a positive damage floor. Damage is an air-equivalent
benchmark, not service life. It has no qualified service acceptance line.

## Inputs

Provide a case manifest, a strength-screen criteria file, an upstream fatigue
token file, and a results directory. The case manifest declares `matrix_version: 1` and contains exactly twelve
cases: the Cartesian product of sizes 08/10/12, walls W1/W2 and SCR/SLWR.
It also contains the complete Phase A run ledger. Subsets are rejected even
when the manifest and exports omit the same competitor. No partial ranking
mode is supported. Every case requires at least one qualified Phase A strength export and all
three fatigue exports. Missing, duplicate, mismatched and unexpected exports
are rejected before an output directory is created.

Manifest structure for synthetic inputs (the run ledger shown is an excerpt;
all twelve cases require their own strength and fatigue run entries):

```json
{
  "matrix_version": 1,
  "basis_label": "synthetic",
  "fatigue_sea_state_ids": ["F03", "F04", "F05"],
  "cases": [
    {"case_id": "R08-W1-SCR"},
    {"case_id": "R08-W1-SLWR"},
    {"case_id": "R08-W2-SCR"},
    {"case_id": "R08-W2-SLWR"},
    {"case_id": "R10-W1-SCR"},
    {"case_id": "R10-W1-SLWR"},
    {"case_id": "R10-W2-SCR"},
    {"case_id": "R10-W2-SLWR"},
    {"case_id": "R12-W1-SCR"},
    {"case_id": "R12-W1-SLWR"},
    {"case_id": "R12-W2-SCR"},
    {"case_id": "R12-W2-SLWR"}
  ],
  "runs": [
    {"id": "strength-example", "phase": "A", "status": "completed", "qualified": true},
    {"id": "fatigue-f03", "phase": "A", "status": "completed", "qualified": true},
    {"id": "fatigue-f04", "phase": "A", "status": "completed", "qualified": true},
    {"id": "fatigue-f05", "phase": "A", "status": "completed", "qualified": true}
  ]
}
```

Each ledger entry records a unique `id`, `phase` (`A` or `B`), `status`
(`attempted`, `completed` or `failed`) and boolean `qualified`. Qualification
requires completion. The attempted count includes all Phase A ledger entries;
completed and qualified counts use their explicit status. Counts describe
runs, including attempts without exports, rather than the number of cases.
Phase B entries are retained as evidence and excluded from Phase A counts.
Ledger completeness and qualification require upstream review; they cannot be
established by the presence of a result file.

The criteria file contains only `strength`, with `criterion`, `value` (one),
`unit` (`-`), `basis_label` and `source`. The upstream token file must contain
`V-FAT-RESOLVED.value` equal to `resolved` or `not resolved`. Its bytes are
included in input provenance. The decision reader does not independently
repeat the fatigue mesh, time-step or split-history checks.

## Shared exchange contract

The reader imports validation from `results_exchange.py` and uses the single
`results.schema.json` contract, identified by `urn:digitalmodel:riser-results:v1`.
`exchange_schema()` exposes this contract to consumers; the decision reader
imports its validator and matrix constants rather than defining another schema.
No solver or standards document is loaded.
Existing strength arrays and provenance remain required. The following fields
make the decision inputs explicit:

- Strength files are `results/<case_id>/strength.json` or
  `results/<case_id>/strength-<export_id>.json`. Eligible exports must declare
  `utilisation_basis: "U1"`. The existing `von_mises_utilisation` array and
  `max_utilisation` carry that declared U1 basis. The maximum must identify an
  actual sample. The largest qualified Phase A export supplies the chart x.
- Fatigue files are `results/<case_id>/fatigue-F03.json`, `fatigue-F04.json`
  and `fatigue-F05.json`. Each `hot_spots` entry must include an annual
  `damage_per_year`. The three exports must share the same station grid and
  location names; duplicate stations are rejected. A case-level scalar damage
  cannot substitute for station evidence. Legacy `life_years` is optional in
  the shared schema and is never read by the decision calculation.
- Each result's `run` retains `input_sha256`, `result_file` and `date`, and
  adds `id`, `phase`, `status` and `qualified`, matching a unique ledger entry.
  Only completed, qualified Phase A exports enter the comparison. Every
  fatigue export must be eligible. Excluded strength exports remain in input
  provenance. Duplicate run identifiers across exports are rejected.

Version 1 preserves the following recorded metadata forms:

- `allowable` requires positive `value`, `unit` and `source`; `basis_label` is
  optional. No basis label is invented when it is absent.
- `sn_curve` requires a source in `source` or `source_label`. Each supplied
  source must be nonblank. `thickness_correction` is either a positive exported
  stress-range multiplier, null (no correction applied), or an object with
  positive `thickness_mm` and `reference_thickness_mm`, nonnegative `exponent`
  and nonblank `source_label`. These forms are retained without conversion.
  This reader sums recorded damage and applies no thickness correction.

`tests/riser_study/fixtures/peer-records.json` contains strength and fatigue
records extracted from peer test functions. Each entry pins the source commit,
repository path, function name and source SHA-256. Tests validate those records
unchanged, including a missing allowable basis and null correction, then check
structured corrections separately. These establish structural compatibility
with those fixtures only. The peer consumers are absent from this branch;
import migration on their branches remains unverified. The separate exporter
contract using `utilisation_method`, stress envelopes and multiple fibres also
requires a versioned adapter before it can feed this reader; its compatibility
is not claimed.

Missing required metadata is rejected rather than inferred. This reader also
requires the explicit qualification and station-damage fields above. Dates accept calendar dates or timestamps with a timezone;
offset hours must be 00–23 and minutes 00–59. Leap-second timestamps are unsupported. Non-finite numbers, invalid dates,
negative damage and inconsistent strength arrays are rejected. The validator
checks structure and sampled consistency; it cannot establish that an exported
U1 method, annualisation, allowable or qualification record is correct.

## Run

With `src` on the Python import path and `matplotlib` and `jsonschema` available:

```sh
python -m digitalmodel.riser_study.decision_chart \
  --results results --manifest cases.json --criteria criteria.json \
  --fatigue-tokens fatigue-tokens.json --output decision
```

The output directory must be empty or absent. A directory containing older
artifacts is rejected so that removed outputs cannot survive a regeneration.

## Outputs

| Token | Artifact or value | Data source |
|---|---|---|
| FIG-DEC-01 | `FIG-DEC-01.svg` | `FIG-DEC-01.csv` |
| TAB-RES-01 | `TAB-RES-01.md` | `TAB-RES-01.csv` |
| V-DEC-COUNT-STR | Cases with U1 at or below one | `manifest.json` rows |
| V-ANS-OD1 | NPS 8 configuration and wall, or unresolved | `manifest.json` rows and resolution |
| V-ANS-OD2 | NPS 10 configuration and wall, or unresolved | `manifest.json` rows and resolution |
| V-ANS-OD3 | NPS 12 configuration and wall, or unresolved | `manifest.json` rows and resolution |
| V-RUN-ATTEMPTED | Phase A attempted runs | `manifest.json` ledger |
| V-RUN-COMPLETED | Phase A completed runs | `manifest.json` ledger |
| V-RUN-QUALIFIED | Phase A qualified runs | `manifest.json` ledger |
| V-PHB-STATUS | `not yet done` | Phase B is outside this reader's assessment |

For each size, selection is restricted to strength-passing cases and requires
upstream resolved fatigue. The unique minimum annual benchmark damage in that
set supplies the configuration and wall. Ties, no strength-passing candidate,
or unresolved fatigue produce `ranking unresolved`. Strength flags and counts
remain available independently of the ranking.

`tokens.json` contains exactly the ten decision IDs above. Every
entry includes a `source_file`; figure and table entries link their artifacts
and CSV data. No criteria table is emitted. `manifest.json` retains unrounded
rows, the governing strength run and damage station, the resolution status,
run ledger and counts, exchange-contract identity, matrix version, input SHA-256 digests, and output SHA-256 digests. It
does not hash itself. Both CSV files contain the same unrounded rows. Display
values use one decimal place, with scientific notation for damage.

The token list follows the supplied analysis map. The underlying ruling
is absent from this module's starting commit message and README. Engineering
results and Phase B completion are not established by the synthetic tests.
The tests use hand-enumerated strength counts and damage sums. Their static
reference tension is copied from case C1 in
`docs/benchmarks/catenary_closed_form/benchmark.json`; all other result numbers
are assumed synthetic arithmetic examples.
