# SCADA surface-card export ingestion

The reader imports ordered surface traces from pipe-delimited controller
exports. It flags unusable downhole columns and produces a descriptive inventory.
It does not establish independent solver agreement or classifier accuracy.
This implements the corrected format-reference scope of issue #1873.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-19 | Added surface ingestion, quality states and an original synthetic example because paired downhole loads are unavailable. |

## Sources and rights

The two 2020 exports linked in #1873 were inspected on 2026-10-19 at source
revision `237d867166688ab454e50efacabf959cbb7dc73b`:

- [First export metadata](https://api.github.com/repositories/668948882/contents/csv_file/ExportedDynaCards_2020-9-28.csv?ref=237d867166688ab454e50efacabf959cbb7dc73b).
- [Second export metadata](https://api.github.com/repositories/668948882/contents/csv_file/ExportedDynaCards_2020-10-2.csv?ref=237d867166688ab454e50efacabf959cbb7dc73b).

The repository metadata reports no licence, and its recursive tree contains no
licence or copying file. Public availability does not establish redistribution
rights. Raw exports, identifiers, measured arrays and derived per-card results
are excluded from this change. Only the schema, aggregate inventory and file
digests are recorded. The example and tests contain original synthetic rows.
Local use requires the user to establish permission; there is no downloader.

| Source file | SHA-256 of inspected bytes |
| --- | --- |
| `ExportedDynaCards_2020-9-28.csv` | `6b5db610af3d962c5990274caa287ee5c0df460404489cdd37dcad384399fda0` |
| `ExportedDynaCards_2020-10-2.csv` | `037c5b01211b92f58276eb8362cd48d0713972cc12772c24d0b76327cc06b654` |

## Observed inventory

All counts in this table come from direct parsing of the pinned source files,
not from the issue's original estimates. Each row contains comma-separated
surface positions and loads within pipe-separated fields.

| Source file | Cards | Acquisition type | Surface point counts | Downhole columns |
| --- | --- | --- | --- | --- |
| First export | 33 | 33 Current | 7 with 132; 16 with 133; 10 with 134 | Both empty in every row |
| Second export | 43 | 41 Current; 2 Full | 5 with 132; 25 with 133; 13 with 134 | Loads exactly copy positions in every row |
| Combined | 76 | 74 Current; 2 Full | 12 with 132; 41 with 133; 23 with 134 | No usable paired downhole loads |

The #1873 correction (2026-07-28) correctly rejects the vendor-inversion
premise. However, the pinned files reverse its statement about which export is
empty, and include 132-point cards as well as 133- and 134-point cards. No fixed
sample count is imposed by the reader. `Current` and `Full` describe acquisition,
not fault diagnoses. Linked issues #1857, #1870, #1874 and #1864 provide the
solver and diagnostic context; this inventory supplies no new validation claim
for those methods.

## Interface and method

`scada_export.read_scada_export(stream)` reads a text stream;
`load_scada_export(path)` handles UTF-8 files including a byte-order mark.
Required headers are `Card Time`, `Card Type`, `Surface Positions` and
`Surface Loads`. Duplicate headers, ragged rows, invalid timestamps without
timezones, nonfinite samples, empty traces and mismatched sample counts raise
`ValueError` with row context. A trace requires at least two paired samples as a
structural minimum; this is not an engineering acceptance criterion.
Derived metrics that exceed the finite numeric range are rejected as well.

Optional positive `Stroke Period`, `SPM` and `Stroke Length` values are retained.
Absent or blank optional values remain unknown. Unknown columns are ignored.
Repeated points, original sample order, acquisition order and timestamp text
are preserved. No closing point, smoothing, phase shift or resampling is added.

Downhole quality is assessed separately, so a bad downhole field does not
discard a usable surface trace:

| State | Evidence |
| --- | --- |
| `MISSING` | Both downhole fields absent or blank |
| `INVALID` | Unpaired, nonnumeric, nonfinite or insufficient samples |
| `COPIED` | Finite paired arrays equal numerically, including differently formatted copies |
| `UNVERIFIED` | Finite distinct arrays whose provenance has not been established |

Every inventory reports `solver_validation: NOT_EVALUATED`, including distinct
downhole arrays. Headers alone cannot authenticate a vendor inversion. Rod
string, pump configuration, downhole truth and fault labels are not supplied.

The descriptive arithmetic is defined here (2026), independently of any solver:

- Surface stroke span = maximum surface position minus minimum surface position.
- Surface load range = maximum surface load minus minimum surface load.
- Surface-to-declared stroke ratio = observed surface span divided by the
  optional declared surface stroke length, in the same undeclared export units.

The last quantity is **not a downhole/surface stroke ratio**. That ratio is
`NOT_EVALUATED` because a usable downhole card is absent. No physical constants,
correction factors, tolerances or engineering pass/fail limits enter this
reader. `summarize_cards` returns each card's metrics plus all observed values
and their extrema, so a distribution is not collapsed into a mean. Missing
ratios remain null per card and are omitted from the ratio distribution.

The headers do not establish units. The inventory therefore declares
`UNDECLARED`. `card.to_surface_card(position_unit="in", load_unit="lbf")`
adapts to the existing `CardData` model only after the caller establishes those
native units from separate evidence. Other unit declarations are rejected;
no conversion or solver execution is performed.

To inventory permitted local exports:

```sh
PYTHONPATH=src python -m digitalmodel.marine_ops.artificial_lift.dynacard.scada_export first.csv second.csv
```

JSON goes to standard output. File identity is not included in the inventory;
input and row order identify the corresponding records. Preserve source file
digests separately when using local results.

## Worked example

Run the original synthetic example:

```sh
PYTHONPATH=src python examples/domains/artificial_lift/scada_surface_export.py
```

Inputs are positions `[0, 2, 4, 2, 0]`, loads `[10, 30, 30, 10, 10]`, declared
stroke `4`, and duplicated downhole arrays `[0, 1, 0]`. All are invented inputs
for arithmetic illustration, not measurements or assumed field parameters.

Hand calculation gives surface span `4 - 0 = 4`, load range `30 - 10 = 20`,
and surface-to-declared stroke ratio `4 / 4 = 1`. The answer is one five-point
surface card with `COPIED` downhole status, undeclared units and
`NOT_EVALUATED` solver validation. These same hand calculations appear in the
test docstrings; expected values are not generated by the implementation.

## Scope and remaining work

Slice 1 supplies the strict reader and optional explicit-unit adapter. Slice 2
adds downhole quality states, per-card metrics and full descriptive distributions.
Slice 3 supplies the local inventory command, offline synthetic regression
tests, source inventory and executable example.

Independent solver scoring, phase-invariant comparison against measured
downhole loads, fault classification and digitisation from images require
separate evidence and are not evaluated here. Source redistribution requires
resolved rights. No solver discrepancy can be inferred from this export.
