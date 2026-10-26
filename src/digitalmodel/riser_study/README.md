# Riser strength and fatigue decision outputs

`decision_chart.py` reads exported results and produces a fatigue against
strength chart. It uses the maximum exported utilisation and the shortest
exported sea-state life for each case. Strength passes when utilisation is
less than or equal to its criterion; fatigue passes when life is greater than
or equal to its target. Comparisons use unrounded values.

This is a screening sensitivity, not a full fatigue analysis. Occurrences are
retained in the exchange records; this package neither reweights lives nor
sums damage. Exporters must document their annualisation and fatigue factors.
The minimum life is a sensitivity envelope, not a combined scatter-diagram
life. No solver or standards document is loaded.

## Inputs

Provide an explicit case manifest, a criteria file, and a results directory.
The manifest defines completeness; every listed case requires strength and
every listed fatigue sea state. Missing, duplicate, mismatched and unexpected
exports are rejected. Case identifiers carry the configuration suffix.

Example manifest for synthetic inputs:

```json
{
  "basis_label": "synthetic",
  "fatigue_sea_state_ids": ["S1", "S2", "S3"],
  "cases": [
    {"case_id": "R08-W1-SCR"},
    {"case_id": "R08-W1-SLWR"}
  ]
}
```

The example uses a subset only to show the shape. List every study case in
the actual manifest; no case count or sea-state count is embedded in the code.

Criteria have `strength` and `fatigue` objects, each containing `criterion`,
`value`, `unit`, `basis_label` and `source`. Units are `-` and `years`,
respectively. Use the reviewed matrix criteria record as the source, including
its revision. There are no default numeric criteria. Until the reviewed record
is available, the fixture criteria are **ASSUMED**, as their basis labels state.
Those synthetic numbers are arithmetic examples, not design acceptance limits.

The supplied fixture is
`tests/riser_study/fixtures/criteria.json` from the repository root. Tests define
their synthetic result values and hand-counted expectations in
`tests/riser_study/test_decision_chart.py`. Its static top tension is copied
from case C1 in `docs/benchmarks/catenary_closed_form/benchmark.json`, whose
independent calculation is `catenary_hand_values.py` in the same directory.
Every other fixture number is an ASSUMED synthetic schema or arithmetic example.
Presentation settings in the chart are not engineering parameters.

## Exchange contract

`results_exchange.py` defines `StrengthResult` and `FatigueResult` TypedDicts.
`results.schema.json` is the structural contract, with JSON Schema draft
2020-12 definitions for both records. `validate_result(data, kind)` checks
the schema, real dates, finite numbers and strength-array consistency.
Duplicate JSON keys, non-finite numbers and nonpositive fatigue lives are
rejected. Positive finite life is required for the logarithmic plot; exporters
must resolve zero-damage or undefined-life cases before chart generation.
Dates may be calendar dates or timestamps with seconds and a timezone;
leap-second timestamps are unsupported.

Licensed runs must export:

- `results/<case_id>/strength.json`: `case_id`; `software` with `name` and
  `version`; `load_case` with `id`, `wave`, `current`, `offset`; aligned arrays
  `arc_length_m`, `effective_tension_kN`, `bending_moment_kNm`,
  `von_mises_utilisation`; top-level `method` and `allowable`; `max_utilisation`
  with `value`, `arc_length_m`, `location`; `touchdown` with `arc_length_m`,
  `utilisation`, `stress_range_MPa`; `top_angle_deg` with `static`, `dynamic_min`,
  `dynamic_max`; `hang_off_tension_kN` with `static`, `dynamic_max`; and `run`.
- `results/<case_id>/fatigue-<sea_state_id>.json`: `case_id`; `sea_state` with
  `id`, `Hs_m`, `Tp_s`, `direction_deg`, fractional `occurrence`; `hot_spots`
  with `name` (`touchdown` or `hang_off`), `arc_length_m`, `cycles_per_year`,
  `scf`, and either `stress_range_MPa` or `range_histogram` bins containing
  `stress_range_MPa` and `cycles_per_year`; `sn_curve` with `name`, `source`,
  `m`, `log_a`, `thickness_correction`; `damage_per_year`; `life_years`; and `run`.

`run` contains `input_sha256`, `result_file`, and `date`. `allowable` contains
`value`, `unit`, `basis_label`, and `source`. Exporter metadata objects
`wave`, `current`, and `offset` preserve their own units and loading basis.
Additional metadata fields are allowed. The exported maximum must identify an
actual sample of the utilisation array, using the same precision. All tied
maximum locations are valid. Tied shortest lives select the first sea-state
identifier in lexical order for reproducibility.

The validator checks structure and sample consistency; it cannot establish
whether a stress method, allowable, S-N curve or source is qualified. The
input digest is recorded as supplied, not verified against an unavailable
solver input. No engineering criterion or S-N constant is inferred.

## Run

With `src` on the Python import path and the repository's `matplotlib` and
`jsonschema` dependencies available:

```sh
python -m digitalmodel.riser_study.decision_chart \
  --results results --manifest cases.json --criteria criteria.json --output decision
```

Use a new output directory for each run. Existing decision artifacts are never
overwritten. The chart uses circles for SCR and triangles for SLWR, labels every
case, and shades utilisation at or below the strength criterion and life at or
above the fatigue target. The legend includes the criteria basis labels.

## Outputs

| Token | Artifact | Data |
|---|---|---|
| FIG-DEC-01 | `decision.svg` | `results.csv` |
| TAB-RES-01 | `results.md` | `results.csv` |
| V-DEC-COUNT-BOTH | `tokens.json` value | `manifest.json` rows |
| TAB-DEC-CRITERIA-01 | `criteria.md` | `criteria.csv` |

The criteria-table identifier is supplied for the additional criteria token;
the consuming document can map it to its registered identifier.
`tokens.json` links each artifact. `manifest.json` stores criteria, rows,
completeness identifiers, the count, limitations, input file SHA-256 digests
and output artifact SHA-256 digests. Input locators are relative to the case
manifest when possible, otherwise basenames; roles distinguish input kinds.
The manifest does not hash itself.

CSV and JSON retain input numeric precision to permit point-to-row checks.
Display tables and criterion legends use one decimal place; rounding never
changes pass flags. Generated tables are data artifacts for document assembly,
not a study report. No study conclusions are supplied by synthetic fixtures.
