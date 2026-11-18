# Mendeley diagnostic plate fixture

The original `Working condition diagnosis plate.xls` is retained without
modification. `manifest.json` records its download URL, byte count, SHA-256,
source version, retrieval date, licence evidence and independently extracted
curve inventory. The declared `xlrd` dependency reads the legacy workbook;
a second coordinate representation is therefore unnecessary.

Source: [Mendeley Data, DOI 10.17632/hnfwm9mjs6.1](https://data.mendeley.com/datasets/hnfwm9mjs6/1).
The dataset page's Licence section showed **CC0 1.0** before download on
2026-10-11. [CC0](https://creativecommons.org/publicdomain/zero/1.0/) permits
copying, modification and redistribution, including commercial reuse.
The linked paper cited by the issue is
[Zhang et al., DOI 10.1016/j.petrol.2020.108254](https://doi.org/10.1016/j.petrol.2020.108254).
Paper text is not included or used as coordinate evidence.

The inventory is derived from numeric cells on the named sheets. Column indices
are zero based. Blank padding is omitted; sample order, precision and axis scale
are preserved. There are 23 curves across 13 class labels, with 21–190 points
per curve. These quantities match the issue. Every coordinate is within the
source's normalised domain, but individual curves do not necessarily reach the
endpoints. The manifest gives the actual extrema for each curve.

The curve interpretation differs from the issue. The embedded chart titles
read `Normalized displacement`, `Normalized load` and `Normalized power`.
Each of the first ten sheets has a load chart followed by a power chart.
The final three sheets (过平衡, 欠平衡, 减速箱故障) have power charts only.
The 23 curves are therefore ten load curves and thirteen power curves,
rather than a surface/pump card pair for each pump condition. Chart titles are
embedded chart records, not text cells. The existing neighbouring reference
fixture's description of the second block as motor current was also incorrect.
No engineering units or measured-field status are supplied.

`load_diagnostic_plate(path)` loads the pinned source bytes and returns each
curve's original label, block index, columns, position, values, ordinate quantity
and optional fault mode. The explicit source-version schema identifies curve
quantity; the source digest rejects changed or substituted workbooks. Power
curves receive no pump fault-mode mapping. A load curve also remains unmapped
for 供液不足 (insufficient supply) and 出砂 (sand): the source label alone does
not establish fluid pound or sand abrasion. Over-balance, under-balance and
gearbox fault remain unmapped. Other load labels have literal equivalents in
the existing fault-mode vocabulary.

The fixture is a diagnostic shape plate, not a training corpus or a dimensioned
solver input. The loader does not create `CardData` or infer physical loads.
Offline tests compare its load coordinates with the ten existing
`plate_reference_cards.json` exemplars after rounding to each existing
exemplar's decimal precision, derived in the test. The original workbook
contains additional decimal precision, which this loader preserves. Existing per-class generator comparisons
use normalised, arc-length-resampled loops and phase/direction-invariant mean
point distance, with a separate sand jaggedness test. Their existing fluid-pound
and sand-abrasion crosswalks are calibration assumptions, not newly established
fault equivalences from this ingest. Their metrics and tolerances are unchanged.

## Reproduce the per-class comparison

The [offline worked example](../../../../../../docs/domains/artificial_lift/diagnostic-plate-comparison.md)
compares synthetic generator samples directly with this original workbook. It
measures the eight literal load mappings, retains every excluded curve with a
reason, and names modes without a literal exemplar. Acceptance limits and
field accuracy remain `NOT_EVALUATED`.
