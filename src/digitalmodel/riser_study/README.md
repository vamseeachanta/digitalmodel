# Riser fatigue export post-processing

This package reads exported stress ranges and annual cycle counts and produces
**air-equivalent benchmark damage, not service life**. The outputs provide a
screening sensitivity over three sea states, not a full fatigue analysis.
Synthetic fixtures establish arithmetic and artifact consistency. No solver is
started or queried.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| A | 2026-10-25 | Add shared export validation and fatigue artifacts so recorded inputs can be traced to screening results. |
| B | 2026-10-26 | Replace reciprocal results with benchmark damage, per-size range panels and damage heatmaps to align the analysis tokens with the specified outputs. |
| C | 2026-10-26 | Write portable artifact bytes, reject positive stress underflow and accept the common exchange field forms with a tested fixture. |

## Run

From the repository root, with the declared `jsonschema` dependency available:

```bash
PYTHONPATH=src python -m digitalmodel.riser_study.riser_fatigue_runs results output
```

Input files follow `results/<case_id>/fatigue-<sea_state_id>.json`. Output must be
an empty directory outside the results tree. Earlier outputs are retained
separately; a nonempty directory is rejected so obsolete figures cannot remain
beside the new tokens. No existing output data is deleted.

All twelve combinations of nominal size 08/10/12, wall W1/W2 and configuration
SCR/SLWR are required. Every case must carry the same three sea-state definitions,
including occurrence, and both named stations (`touchdown`, `hang_off`) at
unchanged arc coordinates across states. Occurrence fractions are retained
without normalization and their sum must not exceed one. Missing cases, stations,
recorded curve parameters or inconsistent exports fail before outputs are written.

## Shared results exchange

`results_exchange.py` loads the adjacent `results_exchange.schema.json`, the
validation authority for this package's fatigue and strength exports and G4
comparison evidence. Runtime validation also checks finite numbers, increasing
arc coordinates, aligned strength arrays, unique station names and histogram
cycle totals. Unknown fields fail validation; booleans are not numeric inputs.

Fatigue exports record:

| Field | Meaning |
| --- | --- |
| `case_id` | Matches the containing directory, for example `R08-W1-SCR`. |
| `sea_state` | `id`, `Hs_m`, `Tp_s`, `direction_deg`, `occurrence`; occurrence is an annual fraction between zero and one. |
| `hot_spots` | Both station names, `arc_length_m`, `cycles_per_year`, `scf`, and one range representation. |
| `stress_range_MPa` | Nominal range before SCF, assigned to all recorded cycles. |
| `range_histogram` | Alternative array of nominal ranges and annual bin counts; counts sum to the station's annual count. |
| `sn_curve` | `name`, `source`, `m`, `log_a`, `reference_cycles`, `reference_range_MPa`, `endurance_limit_MPa`; optional `thickness_correction`. The legacy `source_label` replaces `source` when used. |
| `damage_per_year` | Recorded maximum over the named stations for this case/state, checked against independent recalculation. |
| `run` | `input_sha256`, `result_file`, calendar `date`; optional `data_basis`. |
| `arc_distribution` | Optional ordered arc points with the same range, cycle and SCF representation. |

The schema accepts an optional legacy `life_years` input for compatibility.
It must be null at zero damage and reciprocal to positive recorded annual damage;
it is never copied into tokens, plots, tables or the run record. Strength exports
retain their aligned arrays. No strength calculation is introduced.

### Exchange compatibility

The common interchange fields are `sn_curve.source`, a positive numeric
`sn_curve.thickness_correction` (a multiplicative stress factor), and strength
`method` plus `allowable` containing `value`, `unit` (`MPa`) and `source`.
An optional `allowable.basis_label` preserves the decision consumer's metadata.
The legacy curve `source_label` and strength pair `utilisation_method` /
`utilisation_allowable` remain accepted. Mixing the two source fields or mixing
strength basis forms fails validation; no source or allowable is inferred.
Legacy correction objects retain their explicit power-law definition. An absent
or null correction means a factor of one; a supplied numeric factor is used once.

`tests/subsea/riser_study/fixtures/fatigue-exchange.json` is an assumed synthetic
positive-damage fixture. The same bytes were accepted by this package and the
export validators from #3517 at `cd6e9edc` and #3519 at `87d262da`. The fixture
also passes `calculate()`; it uses a generic curve rather than the producer's
study-specific benchmark metadata.

This establishes a tested common input, not consolidation of all consumers.
The other branches still own separate validators. Their zero-damage legacy-life
rules conflict: the strength interface requires `life_years: null`, while the
decision interface rejects null and accepts omission. The strength interface
also rejects `allowable.basis_label`, which the decision interface requires.
There is no single strength record satisfying both definitions. Both accept the
common `method`/`allowable` field names. Migration to one schema and validator,
including agreement on optional life and allowable metadata, remains required
before combined use. This branch does not change either consuming branch.
Benchmark reference metadata and optional arc/correction records are local
extensions; the strength interface's closed schema does not accept all of them.

## Calculation basis

The study inputs must explicitly record FAT 80, slope 3, 2,000,000 cycles at
80 MPa, no endurance limit, SCF 1.2 and no thickness correction. These values are
checked, not supplied as defaults. Each run record retains the complete recorded
curve, exposure, cycle counts and SCFs. No licensed publication was opened.

```text
N = 2,000,000 * (80 / (nominal_range_MPa * 1.2))**3
annual_benchmark_damage = occurrence * sum(cycles_per_year / N)
same_station_damage = sum(annual_benchmark_damage over the three sea states)
```

Annual cycle counts describe continuous exposure before occurrence weighting;
occurrence is applied exactly once. For a one-hour damage contribution `d`, this
is `D = d * 8760 * occurrence`. Zero range, zero cycles or zero occurrence gives
true zero damage. Reciprocal life at zero damage is **not established**.
No finite substitute is reported. Damage has no qualified service acceptance line.

The lower-level `calculate()` function still supports explicitly recorded generic
power-law curves and thickness corrections for arithmetic checks. The artifact
producer enforces the study benchmark. The synthetic hand example uses slope 3,
intercept 12, range 100 MPa, SCF 1, annual cycles 1,000,000 and occurrence 0.2,
giving annual damage 0.2. Histogram bins are evaluated separately.

Recorded damage permits relative serialization tolerance `1e-8`; histogram cycle
totals permit `1e-9`. These are numerical comparison tolerances. Overflow,
underflow and nonfinite calculations fail explicitly. In particular, a positive
nominal range whose corrected product rounds to zero raises an error before the
zero-damage shortcut; representable true zero range remains valid.

## Artifacts and tokens

| Token | Content |
| --- | --- |
| `FIG-FAT-RNG-08` | Nominal ranges for size 08, sea state on the horizontal axis, touchdown and hang-off panels, both walls and configurations. |
| `FIG-FAT-RNG-10` | Corresponding size 10 ranges on the same vertical scale. |
| `FIG-FAT-RNG-12` | Corresponding size 12 ranges on the same vertical scale. |
| `FIG-FAT-HEAT-01` | Two 12 by 3 annual benchmark damage panels, one per named station, with a shared logarithmic colour scale and white for true zero. |
| `TAB-FAT-01` | Nominal and corrected ranges, counts, SCFs and annual benchmark damage at both stations for every case/state. |

Each artifact has an SVG and CSV. Generated CSV, SVG and JSON use UTF-8 byte
writes with LF line endings, independent of text-stream newline translation.
Artifact digests hash those exact bytes. Manifest rows and SVG metadata contain
exactly the serialized CSV rows. Each plotted mark carries its row and uses its
unrounded value. CSV and SVG digests, source digests and the run-record digest
are retained in `manifest.json`. Human-visible calculated labels use one decimal;
machine-readable values retain full precision. `run_record.json` retains inputs
without adding an analysis token for known design data.

`tokens.json` provides these values:

| Token | Definition |
| --- | --- |
| `V-FAT-DMAX` | Maximum of the three-state damage sums formed at each named station first. |
| `V-FAT-GOV-CASE` | Case of that maximum. |
| `V-FAT-GOV-SS` | Largest contributing sea state at that governing station. |
| `V-FAT-SCR-HOTSPOT`, `V-FAT-SLWR-HOTSPOT` | Named station governing each configuration's three-state sum. |
| `V-FAT-WT-DELTA` | `100 * (W2 damage - W1 damage) / W1 damage`, in %, at the governing size/configuration; each wall uses its own maximum same-station sum. Null when W1 damage is zero. |
| `V-FAT-RESOLVED` | `resolved` only with passing G4 comparison evidence and a wall change exceeding measured uncertainty; otherwise `not resolved`. |

Wall labels refer to the supplied matrix; no physical wall thickness is inferred.
Exact damage ties use case id, contributing state id and station name in lexical
order. The manifest records governing stations and their individual contributions.
No removed analysis token is emitted. The Phase B duration table remains deferred
until duration-check results are available.

## G4 evidence

Supply an optional JSON record with `--g4-checks checks.json`. The same shared
schema validates its fields:

```json
{
  "comparison_cases": ["R08-W1-SCR", "R08-W2-SCR"],
  "mesh_step": {
    "wall_delta_pct": -87.4,
    "tolerance_pct": 1.0,
    "source_sha256": "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  },
  "half_hour": {
    "wall_delta_pct": -87.3,
    "tolerance_pct": 1.0,
    "source_sha256": "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
  }
}
```

These are assumed synthetic examples, not acceptance tolerances for actual runs.
Each check records the wall percentage from its checked responses, the permitted
absolute deviation in percentage points and its evidence digest. The cases must
match the comparison in the tokens. Both deviations from the full-history wall
percentage must meet the supplied tolerances, and the absolute wall change must
strictly exceed the larger measured deviation. The run record and manifest retain
the supplied evidence and calculated deviations. The post-processor does not
retrieve or independently qualify the referenced evidence. Missing checks or
zero reference damage cannot establish resolution; no default tolerance is used.

## Verification and limits

Fixture tests cover hand-worked Miner sums, recorded FAT 80 inputs, same-station
aggregation, output identifiers, matrix coverage, true zero, plot coordinates
and colours against CSV and manifest rows, G4 comparison resolution, CLI use and
digests. Input validation and generic SCF/thickness arithmetic remain covered.
Tests of removed reciprocal, arc-plot and input-table outputs are replaced by
checks of the revised outputs. No test imports or starts a licensed solver.

Actual stress-response exports, source qualification, cycle counting, full
scatter coverage, other fatigue mechanisms and engineering review remain required.
Only the named stations define the reported maximum; optional arc inputs remain
in the run record and do not establish complete spatial coverage. No Phase B
result, qualified service acceptance or complete fatigue assessment is claimed.

The digest test exercises native text behavior and emulated Windows CRLF text
writes. Native Windows execution remains unverified. The Phase A identifiers
above match the retained implementation record; registration against the
consuming document's authoritative token map remains unverified.
