# Diagnostic plate shape comparison

## Summary

The retained CC0 workbook supplies independent canonical shapes for comparing
synthetic dynacards. The offline example measures all eight literal load-class
mappings. Power curves and the two ambiguous load labels remain
`NOT_EVALUATED`. Distances describe geometry; engineering acceptance and
field accuracy remain `NOT_EVALUATED`.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-19 | Added a reproducible comparison with the original workbook to expose per-class distances and exclusions. |

## Source and inputs

[Mendeley Data, DOI 10.17632/hnfwm9mjs6.1 (2020)](https://data.mendeley.com/datasets/hnfwm9mjs6/1)
provides the original workbook under CC0. The public page and workbook were
retrieved again on 2026-10-19: the licence declaration remains CC0 and the
workbook digest matches the retained manifest. The workbook, licence evidence,
SHA-256 and independently inventoried cells are retained in
[the fixture directory](../../../tests/marine_ops/artificial_lift/dynacard/testdata/mendeley_plate/README.md).
No new copy of the workbook is introduced by this comparison.

The workbook's embedded chart titles establish normalised displacement, load
and power. Its inventory is **ten load curves and thirteen power curves across
thirteen labels**, rather than paired surface and pump cards. These counts and
the eight literal mappings come from the retained manifest. There are no
physical units, rod-string properties or measured-field labels.

The other input is an explicit set of random seeds for the existing synthetic
card generators. Seeds select reproducible samples, not a calibrated population
size. Existing generator coefficients are unchanged. The comparison consumes
caller-declared synthetic `CardData`; it does not establish their provenance.

## Method

The calculation reuses `benchmark.measurements.loop_distance`, which implements
the method in `test_card_generator_shapes.py`:

1. Independently normalise each coordinate axis by subtracting its minimum and
   dividing by its span. A constant axis becomes zero, as in the existing metric.
2. Close each polyline and interpolate onto a uniform arc-length grid. The
   existing method uses 128 samples; this is a numerical convention, not an
   acceptance limit.
3. Compute the mean Euclidean point distance for every discrete circular shift
   and both traversal directions; retain the minimum.
4. Take the median across the supplied synthetic samples for each load label.
   Record the nearest load label among all ten load exemplars as a geometric
   observation, including ambiguous labels among those candidates.

Euclidean distance is the norm of the coordinate difference, as defined in
[*Mathematics for Machine Learning* (2020), equations (3.4) and (3.21)](https://mml-book.github.io/book/mml-book.pdf)
(the public copy is dated 2024-01-15). Arc length here is the cumulative sum of
these distances along a closed polyline. Normalisation, discrete alignment and
median aggregation are repository comparison conventions; the textbook is not
a source of a diagnostic acceptance threshold.

The alignment searches the finite resampling grid. Arbitrarily shifted source
polylines can therefore have a small residual resampling difference. Scaling
removes absolute load and stroke information, so distance alone cannot diagnose
a parted rod or qualify a dimensioned solver. A nearest label is not a classifier
accuracy measurement. Sand texture requires the separate existing generator
shape tests; this comparison does not substitute an outline match for texture.

## Worked example

From the repository root, in an environment containing the declared dynacard
runtime dependencies:

```bash
PYTHONPATH=src python examples/domains/artificial_lift/diagnostic_plate_comparison.py \
  --workbook 'tests/marine_ops/artificial_lift/dynacard/testdata/mendeley_plate/Working condition diagnosis plate.xls' \
  --seeds 0 1 2
```

These three seeds are illustrative caller inputs. The following medians were
computed by that command against the pinned workbook. The display multiplies
the dimensionless distance by 100 and rounds once to one decimal; it is not an
error percentage or an acceptance criterion. Calculations retain full precision.

| Source label | Literal generator mode | Median distance × 100 | Nearest load label |
| --- | --- | ---: | --- |
| 工况正常 | NORMAL | 5.6 | 工况正常 |
| 气体影响 | GAS_INTERFERENCE | 4.9 | 气体影响 |
| 抽油杆断脱 | ROD_PARTING | 5.8 | 抽油杆断脱 |
| 上碰泵 | PUMP_TAGGING_UP | 5.1 | 上碰泵 |
| 下碰泵 | PUMP_TAGGING_DOWN | 6.8 | 下碰泵 |
| 游动阀漏失 | VALVE_LEAK_TV | 4.2 | 游动阀漏失 |
| 固定阀漏失 | VALVE_LEAK_SV | 3.5 | 固定阀漏失 |
| 活塞脱出工作筒 | PLUNGER_OUT_OF_BARREL | 5.9 | 活塞脱出工作筒 |

Every mapped generator is nearest its own label for this illustrative sample.
This supports geometric consistency on these samples only. No source provides
an acceptance limit, and no labelled field-card corpus is scored.

The example also prints all thirteen power curves and the load curves labelled
供液不足 (insufficient supply) and 出砂 (sand), with reasons for leaving them
`NOT_EVALUATED`. Insufficient supply alone does not establish fluid pound; sand
alone does not establish abrasion. The existing generator-shape regression tests
use those two crosswalks as calibration assumptions. They remain separate from
the literal mappings here.

Current modes without a literal load exemplar are `BENT_BARREL`,
`DELAYED_TV_CLOSURE`, `EXCESSIVE_FRICTION`, `EXCESSIVE_VIBRATION`, `FLUID_POUND`,
`GAS_LOCK`, `PARAFFIN_RESTRICTION`, `PLUNGER_UNDERTRAVEL`, `SAND_ABRASION`,
`STUCK_PUMP`, `TUBING_MOVEMENT` and `WORN_BARREL`. This list is derived from the
current generator registry minus the literal workbook mappings; the API and
example compute it on every run.

## Offline checks and later slices

The new behavioural tests use the manifest's independently inventoried labels,
counts and digest. Self-comparison has a hand-derived zero distance because
every coordinate difference is zero. A constructed square supplies an
independent algebra check of axis scaling, reversal and phase alignment.
Tests also cover empty samples, invalid modes, malformed card data, substituted
workbooks and the example's explicit seed selection.

The external labelled set remains quarantined pending provenance and publication
authority. No data from that set is downloaded or mapped. Independent solver
fixtures with unresolved licences remain excluded. Provenance contact, licence
clearance and any later classifier calibration remain with the maintainers.
