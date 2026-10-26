# Riser fatigue export post-processing

This package reads exported results and produces a screening sensitivity over
3 sea states. It supplies figures, tables and value tokens for later analysis.
It starts no solver. Synthetic examples establish arithmetic and artifact
consistency; they do not establish design fatigue lives.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| A | 2026-10-25 | Add shared export validation and fatigue artifacts so recorded inputs can be traced to screening results. |

## Run

From the repository root, with the declared `jsonschema` dependency available:

```bash
PYTHONPATH=src python -m digitalmodel.riser_study.riser_fatigue_runs results output
```

The output directory must be outside the results directory. Input files follow
`results/<case_id>/fatigue-<sea_state_id>.json`. Every discovered file is read;
invalid JSON, inconsistent names, missing states and invalid values fail before
artifacts are written. An empty input directory fails. Output files with the
same token names are replaced when the command is rerun.

All cases must carry the same three sea-state definitions, including occurrence,
and the same hot-spot names across states. The sum of occurrence fractions must
not exceed one. Fractions are retained without normalization. Subsets of the
size matrix are supported, but both configurations and both walls of the
governing size and configuration are required for the value tokens. The full
study requires twelve cases; this package does not invent missing cases.

## Shared results exchange

`results_exchange.py` provides `FatigueResult`, `StrengthResult`, supporting
TypedDicts and `validate_result(data, kind)`. The adjacent
`results_exchange.schema.json` is a Draft 2020-12 JSON Schema with `fatigue` and
`strength` definitions. Runtime validation adds finite-number checks, increasing
arc coordinates, aligned strength arrays, unique hot-spot names and histogram
cycle-total checks. Unknown fields fail validation. JSON booleans are not numbers.

Case identifiers are `R08-W1-SCR`, `R10-W2-SLWR` and the other combinations of
nominal size, wall label and configuration in the requested matrix. These labels
do not supply an outside diameter or a physical wall thickness.

Fatigue exports contain the fields below. Units are part of the field names.

| Field | Required meaning |
| --- | --- |
| `case_id` | Matches the containing directory. |
| `sea_state` | `id`, `Hs_m`, `Tp_s`, `direction_deg`, `occurrence`; occurrence is the annual exposure fraction from zero to one. |
| `hot_spots` | Nonempty array of `name` (`touchdown` or `hang_off`), `arc_length_m`, `cycles_per_year`, `scf`, and one range representation. |
| `stress_range_MPa` | Scalar nominal stress range before SCF, assigned to all recorded cycles. |
| `range_histogram` | Alternative to the scalar: nonempty array of `{stress_range_MPa, cycles_per_year}` bins. Bin cycles must sum to the hot spot's annual cycle count. |
| `sn_curve` | `name`, `source_label`, `m`, `log_a`; optional `thickness_correction`, also permitted to be null. The source label is recorded, not inferred. |
| `thickness_correction` | If present: `thickness_mm`, `reference_thickness_mm`, `exponent`, `source_label`. |
| `damage_per_year`, `life_years` | Recorded maximum damage over hot spots for this case/state and its reciprocal life. Zero damage requires null life. These are checked against the independent recalculation. |
| `run` | `input_sha256` (SHA-256 hex), `result_file`, `date` (calendar date); optional `data_basis`, such as `synthetic`. |
| `arc_distribution` | Optional ordered array of arc points with the same range, cycle and SCF fields as hot spots, without a name. These are recalculated, not interpolated from hot spots. |

For strength exports, the shared schema retains `software`, `load_case`, the arc,
effective tension, bending moment and utilisation arrays, `max_utilisation`,
`touchdown`, `top_angle_deg`, `hang_off_tension_kN` and `run`. The utilisation
array has separate required `utilisation_method` and `utilisation_allowable`
fields so its basis is stated. The `wave`, `current` and `offset` objects retain
exporter-defined load fields. No strength computation is performed here.

## Calculation basis

The method follows the specified Miner-sum contract and the single-slope S-N
parameters in each export:

```text
thickness_factor = 1, when no correction is recorded
thickness_factor = (thickness_mm / reference_thickness_mm)**exponent, otherwise
corrected_range = nominal_range_MPa * scf * thickness_factor
N = 10**log_a / corrected_range**m
damage_per_year = occurrence * sum(cycles_per_year / N)
life_years = 1 / damage_per_year
```

Annual cycles describe continuous exposure to the sea state, before occurrence
weighting. An exporter whose counts already include occurrence must convert them
to that basis first. Stress must likewise be nominal before the recorded SCF.
There is no implicit thickness clipping, reference thickness, safety factor,
mean stress adjustment or curve selection. A zero range contributes no damage.
Null life means unbounded life from the supplied inputs; it is labelled
`unbounded` and omitted from logarithmic coordinates.

The hand example in the module and test docstrings uses explicitly ASSUMED
synthetic parameters: slope 3, intercept 12, range 100 MPa, SCF 1, annual cycles
1,000,000 and occurrence 0.2. Failure cycles are 1,000,000, annual damage is 0.2
and life is 5.0 years. The histogram example calculates each bin separately.
No parameter is taken from a licensed publication. All other test values are
labelled synthetic assumptions. One geometry test uses the retained closed-form
SCR benchmark in
`docs/domains/orcaflex/library/campaigns/generic-scr-slwr/static-reference.json`;
its fatigue inputs remain synthetic. Static geometry is not a dynamic fatigue
qualification. The adjacent `riser_configuration_screen` package supplies a
static curvature proxy and is deliberately not used as fatigue stress data.

Recorded damage/life comparisons allow relative numerical tolerance `1e-8`;
histogram count totals allow `1e-9`. These are serialization tolerances, not
engineering acceptance criteria. Overflow, underflow and nonfinite calculations
fail rather than producing a finite substitute life.

## Artifacts and tokens

Each of the following tokens has its own SVG and CSV. `manifest.json` contains
the actual CSV rows, source export and input digests, artifact digests, all
calculated hot spots, governing records and plotting transforms. SVG metadata
and marks carry those same rows; plotted coordinates use unrounded values.
Every artifact and CSV row carries the screening scope. Table SVGs contain
wrapped record blocks so every recorded field is visible. Calculated table
comparisons display one decimal, using scientific notation for small or large
values; machine-readable rows retain full precision.

| Token | Content |
| --- | --- |
| `FIG-FAT-01` | Maximum nominal histogram/scalar range at the governing damage hot spot of each case/state, in MPa. Corrected range, SCF and thickness factor are retained alongside it. |
| `FIG-FAT-02` | Life at that hot spot in years, with a logarithmic vertical axis. |
| `FIG-FAT-03` | Recalculated damage along each exported riser in the globally governing sea state, with identified case traces. |
| `TAB-FAT-01` | Stress range, damage, life and hot-spot location for each case/state. |
| `TAB-FAT-IN-01` | Recorded sea states, S-N curve and source, correction, SCF, cycles and histograms, arc inputs and run provenance. |

Missing arc exports produce an unavailable `FIG-FAT-03` with a header-only CSV
and an explanatory SVG. Partial coverage is labelled in the SVG and manifest,
with the missing cases listed. No arc curve is fabricated.

`tokens.json` provides `V-FAT-GOV-CASE`, `V-FAT-GOV-SS`, `V-FAT-LMIN`,
`V-FAT-SCR-HOTSPOT`, `V-FAT-SLWR-HOTSPOT` and `V-FAT-WT-FACTOR`. The governing
case/state and configuration-specific hot spots are those with greatest annual
damage. Exact ties use case, state and hot-spot name in lexical order.

The wall factor is **W2 minimum life / W1 minimum life**, for the size and
configuration of the global governing case. Each wall uses its own minimum
over all states and hot spots. Both case ids and states are recorded in
`wall_comparison`. The factor is null if either wall has unbounded life. Wall
labels do not imply which physical thickness is greater. These definitions are
also in the calculation module docstring.

## Verification and limits

The fixture tests check schema acceptance and rejection, a hand-worked Miner
sum, SCF and explicit thickness correction, reciprocal life, governing selection,
missing data, CLI execution, input/artifact digests and every plotted value's
CSV and manifest row. A synthetic twelve-case set covers three common states.
No test imports or starts a licensed solver.

Actual runs must export the named hot spots, nominal ranges or histograms,
unweighted annual counts, SCFs, single-slope curve parameters and source,
occurrence fractions, recorded governing damage and reciprocal life, and run
provenance. Arc distributions are required for a complete `FIG-FAT-03`.

These outputs rank the supplied three-state sensitivity. Full fatigue assessment
requires the qualified full scatter diagram, stress response and cycle counting,
applicable fatigue mechanisms and engineering review. Neither curve provenance
nor the selection of the three fatigue-governing states is established by this
post-processor. The package does not aggregate the three states into a design
life or supply results for absent exports.
