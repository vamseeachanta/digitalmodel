# Riser export post-processing

This package reads exported results and produces SVG figures, numeric CSV
files, manifests and tokens. It imports no native solver interface. Synthetic
test exports demonstrate the output contract; qualified engineering results
remain an external input.

Run the strength and geometry commands from the repository root with their
Python dependencies available:

```bash
PYTHONPATH=src uv run python -m digitalmodel.subsea.riser_study.riser_strength_runs results figures --reference-pair R10-W1 --touchdown-window-m 50
PYTHONPATH=src uv run python -m digitalmodel.subsea.riser_study.riser_configuration_plot geometry.json figures --reference-pair R10-W1
```

The reference size controls the wall comparison; its wall controls the paired
touchdown comparison. The touchdown window argument is retained as provenance
for command-line compatibility. U3 is the exported touchdown-zone stress range;
no utilisation window or new stress history is inferred from this argument.
Select a fresh destination when replacing older output identifiers. The builder
rejects a destination containing deprecated figure files without removing data.

## Output contract

The identifiers below follow the supplied analysis list. Each size suffix is
one of `08`, `10`, `12`. Figure tokens identify SVG files, table tokens identify
Markdown files, and every artifact has a matching CSV and manifest rows.

| Identifier | Contents |
| --- | --- |
| FIG-STR-TEFF-08/10/12 | Static and dynamic minimum/maximum effective tension along arc, zero line |
| FIG-STR-U1-08/10/12 | Both-fibre U1 along arc, screening line at unity |
| FIG-STR-TDR-08/10/12 | Touchdown stress range U3 against span offset, direction panels, SLWR/SCR ratio on the right axis |
| FIG-STR-ANG-08/10/12 | Angle from vertical against span offset, direction panels, excursion ratio on the right axis with its unity screen |
| FIG-STR-HOT-08/10/12 | Static and dynamic minimum/maximum hang-off effective tension against span offset, direction panels, zero line |
| FIG-STR-SUM-01 | Five panels: maximum U1, minimum effective tension, maximum touchdown stress range, maximum excursion ratio, maximum hang-off tension; run counts |
| TAB-STA-01 | Nominal functional static software and analytical tension, angle and horizontal touchdown position, absolute and percentage differences |
| TAB-STR-1111-01 | Exported API RP 1111 combined-loading checks, applicability, method, reason and load conditions |
| SCH-01 | Existing proportional SCR/SLWR configuration schematic |

Each profile panel compares both walls and configurations on shared axes.
Static results at the three offsets and dynamic results at their actual offsets
retain distinct conditions and source records. The summary includes all qualified
records supplied, so adding near/far dynamics updates the same identifiers.
No near/far dynamic response is inferred from a nominal response.

`tokens.json` contains `{id, value, unit, source_file}` entries for all strength
figures and tables and these values:

- `V-STR-GOV-CASE`, `V-STR-UMAX-GOV`, `V-STR-GOV-LOC`: governing U1, case,
  arc station, fibre and stress state; ties resolve by sorted case identifier.
- `V-STR-UMAX-SCR`, `V-STR-UMAX-SLWR`: configuration maxima over U1.
- `V-STR-WT-DELTA`: `100 * (W2 - W1) / W1`, where each wall value is maximum
  U1 across both configurations at the explicit reference size. Unit `%`.
- `V-STR-TEFF-MIN`: lowest static/dynamic effective tension, unit `kN`.
- `V-STR-COMP-COUNT`: cases with any strictly negative effective tension.
- `V-STR-UANG-MAX`: maximum excursion ratio. Excursion is the largest absolute
  angle departure from that case's nominal functional angle, divided by the
  supplied assumed 10-degree screen; dynamic extrema are checked separately.
- `V-STR-TDR-DELTA-REF`: `100 * (SLWR - SCR) / SCR`, using each configuration's
  maximum dynamic touchdown stress range for the explicit reference pair. Unit `%`.
  The figure retains the paired SLWR/SCR ratio at each matching load condition.
- `V-STR-DYN-BASIS`: actual qualified coverage: nominal with one/two screened
  directions, all three offsets, or `not established` when no supported dynamic
  basis is available. Missing dynamics are never treated as zero response.
- `V-SW-VERSION`: every distinct recorded software name/version.

A zero comparison denominator yields `not established`; undefined figure ratios
are recorded in `unresolved_ratios` rather than plotted as a numeric zero.
The six provisional figure identifiers and the two touchdown-utilisation value
identifiers are no longer produced. Bending moment remains an exchange input
for compatibility but has no figure in this output contract.

## Shared input exchange

`results_exchange.py` and `results_exchange.schema.json` remain the single
strength/fatigue exchange. Generic legacy records still validate. Per-size
strength output additionally requires the optional `screening` object; legacy
`von_mises_utilisation`, `max_utilisation`, `method` and `allowable` are preserved
as provenance and never substituted for U1.

A strength record is stored as either `results/<case_id>/strength.json` or
`results/<case_id>/<load_id>/strength.json`. Case identifiers span all twelve
size/wall/configuration combinations. Multiple static and dynamic exports per
case are supported. Every recursively discovered record is validated before
any output is written, including unqualified records. Duplicate conditions,
directory mismatches and unmatched load coverage are rejected. Span offset is
explicitly `load_case.offset.x_m`, with zero transverse offset; supported span
positions are near, nominal and far. `load_case.wave.direction_deg` is required.
Paired conditions must have identical wave/current inputs.

The original aligned arrays, touchdown summary, software, load case and run
provenance retain their shared contract. Additional `screening` fields are:

- `condition`: `functional` or `extreme`; `dynamic`: whether a history exists;
  `qualified`: whether the supplied record may enter reported envelopes.
- `u1`: `method` must be `API RP 2RD`; `smys_MPa` records the study's material
  basis. `stress_MPa.static` and, for dynamics, `stress_MPa.dynamic_max` each
  contain aligned nonnegative `inner` and `outer` equivalent-stress arrays.
  These stresses must already use the exported method. The denominator is
  `(2/3) SMYS` for functional records and the supplied study screen of 360 MPa
  for extreme records. Both fibres and all supplied states enter maxima.
  The generic screen's stress/SMYS values cannot serve as U1 inputs.
- `tension_envelope_kN`: aligned `dynamic_min` and `dynamic_max` arrays;
  both are null on static-only records. Reversed envelopes are rejected.
- `hang_off_dynamic_min_kN`: the dynamic minimum at arc zero, null on static
  records. Dynamic minimum/maximum and static hang-off values must agree with
  their corresponding profile samples at arc zero.
- `excursion_reference_deg`, `excursion_limit_deg`: explicit reference and
  assumed screen. Every reference must agree with that case's nominal
  functional angle.
- `static_check`: required for each case's nominal functional reference;
  `analytical` contains `top_tension_kN`, `top_angle_deg`, `touchdown_x_m`,
  with `software_touchdown_x_m` and a nonempty `basis`. These are independent
  analytical/export inputs. The processor reports differences, not a gate pass.
- `api_rp_1111`: `in_scope`, `method`, `reason`, and `checks` containing
  `arc_length_m`, `utilisation`, `check`. In-scope records require check rows;
  out-of-scope records require an empty list and are shown as `not in scope`.
  The processor reports exported checks without reimplementing the criterion.

All twelve cases need a qualified nominal functional reference and matching
qualified load coverage. Unqualified records remain in source provenance and
are excluded from plotted response and values. Qualification is an explicit
export input, not a status assigned by this package. Run counts distinguish
all discovered records, qualified static/dynamic records and represented cases.

The shared fatigue contract is unchanged. It retains its existing sea-state,
hot-spot, curve, damage, reciprocal-life and run fields for other consumers.
This strength change neither produces fatigue outputs nor changes their basis.

## Traceability and checks

CSV and manifest rows retain full precision. Each SVG numeric mark records its
row index and unrounded coordinates; metadata embeds the same rows. Hover text
shows case, measure, value and units. Direction panels use separate ratio axes.
Labels and Markdown table values use one decimal place. Source labels are
portable paths rooted at `results/`; every response row carries the input and
result digests. Paired ratios carry both sources and both digests. The manifest
preserves every source record and its byte digest. Hashes remain in provenance,
not visible figure labels. No source file is modified.

Tests check the complete identifier set, both-fibre functional/extreme U1,
compression count, percentage denominators, paired ratios, analytical table
arithmetic, source digests, CSV/manifest equality and actual SVG mark/polyline
coordinates on both axes. The complete matrix uses assumed synthetic exports.
The independent catenary benchmark and equal-scale schematic checks remain.
No standard document is opened and no native solver is started or queried.

Geometry inputs and `SCH-01.manifest.json` remain unchanged: nominal profiles
use the same pixel scale horizontally and vertically, and identify hang-off,
sag, hog, buoyancy, touchdown, water line and seabed. Geometry imports require
its plotting/tabular dependencies; strength processing needs only the shared
JSON validation dependency in addition to the standard library.
