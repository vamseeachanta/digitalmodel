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
token file, and a results directory. The case manifest lists completed cases
ready for the decision comparison and the complete Phase A run ledger. Every
listed case requires at least one qualified Phase A strength export and all
three fatigue exports. Missing, duplicate, mismatched and unexpected exports
are rejected before an output directory is created.

Example manifest structure for synthetic inputs:

```json
{
  "basis_label": "synthetic",
  "fatigue_sea_state_ids": ["F03", "F04", "F05"],
  "cases": [{"case_id": "R08-W1-SCR"}],
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
`results.schema.json` contract. No solver or standards document is loaded.
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

Missing metadata is rejected rather than inferred. The shared schema retains
compatibility with older consumers, while this reader requires the explicit
fields above. Dates accept calendar dates or timestamps with a timezone;
leap-second timestamps are unsupported. Non-finite numbers, invalid dates,
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

`tokens.json` contains exactly the ten registered decision IDs above. Every
entry includes a `source_file`; figure and table entries link their artifacts
and CSV data. No criteria table is emitted. `manifest.json` retains unrounded
rows, the governing strength run and damage station, the resolution status,
run ledger and counts, input SHA-256 digests, and output SHA-256 digests. It
does not hash itself. Both CSV files contain the same unrounded rows. Display
values use one decimal place, with scientific notation for damage.

The token list follows the supplied draft-3 analysis map. The underlying ruling
is absent from this module's starting commit message and README. Engineering
results and Phase B completion are not established by the synthetic tests.
The tests use hand-enumerated strength counts and damage sums. Their static
reference tension is copied from case C1 in
`docs/benchmarks/catenary_closed_form/benchmark.json`; all other result numbers
are assumed synthetic arithmetic examples.
