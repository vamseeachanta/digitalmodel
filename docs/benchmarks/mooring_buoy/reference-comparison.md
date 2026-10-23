# Offline mooring reference comparison

## Summary

The current tracked inputs produce 56 strict differences: 39 equal-decimal
numeric pairs, seven matching boolean spellings, one null/empty-string pair,
eight presentation-key candidates and one missing wind-control field. All 56
remain blocking. The classifications describe decoded data, with OrcaFlex
semantics marked `NOT_EVALUATED`. No native readback or analysis occurred.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-19 | Add sourced diagnostic categories and a current-input comparison so representation candidates can be reviewed individually without waiving compatibility. |

## Scope and method

Refs #2104; related issues #2093 and #2092. Slice 1 adds diagnostic categories
to the strict recursive comparator. Slice 2 records evidence gaps and blocking
decisions for every category. Slice 3 records the offline comparison in
[the classified report](reference-classification.json), which enumerates
every row with its path, kind, category and original value digests.

The existing comparison still requires identical decoded scalar types and
values. Mapping keys are compared independently of order; list order is
retained. Generated includes are merged in master order. This comparison cannot
establish that assignment order has no effect in OrcaFlex. Missing-member hashes
and their disclosed container limitations are retained.

For finite integer/float pairs of different types, the diagnostic equation is
`D(a) = D(b)`, where `D(x) = Decimal(str(x))`. The Python 3.11
[Decimal constructor documentation](https://docs.python.org/3.11/library/decimal.html#decimal.Decimal)
states that decimal string construction preserves the supplied digits without
context rounding. This is exact equality of decoded decimal spellings, with no
acceptance tolerance. Integers are never converted to binary float. Booleans,
numeric strings and non-finite floats do not enter this category. Original YAML
numeric lexemes are not retained; precision already lost during decoding cannot
be recovered by classification.

## Category decisions and sources

| Category | Diagnostic basis | OrcaFlex semantics and required evidence |
| --- | --- | --- |
| Equal-decimal numeric | YAML 1.1 [integer](https://yaml.org/type/int.html) and [float](https://yaml.org/type/float.html) type definitions, Working Draft 2005-01-18, distinguish integer and approximate-real tags. Decimal equality is the diagnostic above. | `NOT_EVALUATED`, blocking. Field-specific format documentation or reviewed version-bound readback is needed before a cross-type waiver. |
| Boolean spelling | YAML 1.1 [bool](https://yaml.org/type/bool.html), Working Draft 2005-01-18, lists Yes/No spellings. Only a decoded boolean paired with its matching, case-sensitive string is classified. | `NOT_EVALUATED`, blocking. A quoted string remains a different decoded type; field acceptance and effects require evidence. Opposite boolean spellings remain value/type differences. |
| Null/empty string | YAML 1.1 [null](https://yaml.org/type/null.html), Working Draft 2005-01-18, Definition, distinguishes null, empty string and absence. | `NOT_EVALUATED`, blocking. No blanket script or data substitution is supported. |
| Presentation-key candidate | The candidate inventory is from issue #2104. Orcina [Text data files](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Textdatafiles.htm), current online edition, YAML file format, describes assignments and defaults but does not establish these omissions as equivalent. | `NOT_EVALUATED`, blocking. Each key needs vendor field definitions and reviewed evidence of no analysis effect before a non-blocking policy. |
| Other missing data | The same YAML null definition distinguishes absent keys. The vendor format page does not establish the effect of these particular defaults. | `NOT_EVALUATED`, blocking. Dispose of each missing field or list member individually. |
| Other value/type difference | The vendor format page provides no general cross-type normalization or numerical acceptance tolerance. | `NOT_EVALUATED`, blocking. Dispose of each changed value or unclassified type pair individually. |

The public sources were retrieved successfully on 2026-10-19. The vendor page
does not identify a fixed product edition. No category is declared
representation-only in the installed product. `VerticalWindVariationFactor`
is deliberately excluded from the presentation inventory because its omission
has no established display-only basis.

## Observed comparison

| Category | Rows | Blocking rows |
| --- | ---: | ---: |
| Equal-decimal numeric | 39 | 39 |
| Boolean spelling | 7 | 7 |
| Null/empty string | 1 | 1 |
| Presentation-key candidate | 8 | 8 |
| Other missing data | 1 | 1 |
| Other value/type difference | 0 | 0 |
| Total | 56 | 56 |

The 17 rows outside the equal-decimal numeric category are enumerated below.
All 56 rows, including the numeric candidates, remain unresolved and appear
individually in the classified JSON report.

| Path | Category |
| --- | --- |
| `/Environment/CurrentApplyVerticalStretching` | Boolean spelling |
| `/Environment/CurrentRamped` | Boolean spelling |
| `/Environment/IncludeBuoyWindLoads` | Boolean spelling |
| `/Environment/IncludeBuoyWingWindLoads` | Boolean spelling |
| `/Environment/IncludeLineWindLoads` | Boolean spelling |
| `/Environment/IncludeVesselWindLoads` | Boolean spelling |
| `/Environment/MultipleCurrentDataCanBeDefined` | Boolean spelling |
| `/Environment/VerticalWindVariationFactor` | Other missing data |
| `/General/DefaultViewAzimuth` | Presentation-key candidate |
| `/General/DefaultViewCentre` | Presentation-key candidate |
| `/General/DefaultViewElevation` | Presentation-key candidate |
| `/General/DefaultViewMode` | Presentation-key candidate |
| `/General/DefaultViewSize` | Presentation-key candidate |
| `/General/RestartStateRecordingTest` | Null/empty string |
| `/Lines/0/ContactPen` | Presentation-key candidate |
| `/Lines/0/DrawNodesAsDiscs` | Presentation-key candidate |
| `/Lines/0/DrawShadedNodesAsSpheres` | Presentation-key candidate |

The report binds the current reference SHA-256
`4d389188dca4ab21ea9c9f1f8899bef35afed1affa2155329012caab0f39f036`
and source specification SHA-256
`1fa7c6879eca05511a330dfd042fec39cdf21ba65f0f82b1653810b464d43be6`,
plus all six generated/input file digests. Its own SHA-256 is
`675a40c2468fd01d2f9e171b4770bcb7c3306c0f1878c80a0b44a697800af650`.
The reference byte digest differs from the historical report recorded with
issue #2104; this is a current-tree measurement, not a byte-for-byte replay of
that archived run. The historical report is retained.

Reproduce using a fresh output directory outside the checkout:

```sh
PYTHONPATH=src PYTHONHASHSEED=0 uv run --no-project \
  --with pyyaml --with 'pydantic>=2,<3' --with numpy --with loguru --with scipy \
  python scripts/prepare_mooring_qualification.py --output "$TMPDIR/reference-comparison"
```

## Test basis and limits

Synthetic tests use independent hand calculations in their docstrings:
`40/10 = 4`, `10**20 + 1` differs from `1e20` by one, and a changed decimal
differs by its written final digit. These tests cover genuine value changes,
large integers, boolean/integer separation, non-finite values, matching and
opposite boolean spellings, null versus absence, exact presentation paths and
preservation of strict blocking. The report test hand-counts six synthetic
differences across the categories. The packaging test also reproduces the
committed classified report; this is a reproducibility check, not an independent
physical benchmark. No coefficients or engineering acceptance limits are added.

Report schema version 3 adds categories, counts, blocking counts and evidence
decisions; consumers requiring version 2 need to account for these additions.
`reference_compatible`, `native_verified` and `engineering_parity` remain false
for the recorded inputs.

## Way forward

Later slices can establish field-specific semantics from versioned vendor
documentation or separately authorized readback, then introduce reviewed
non-blocking decisions where supported. Null/empty script behavior, wind-control
defaults, presentation omissions and numeric tag acceptance remain unevaluated.
No tolerance or waiver is inferred from the observed counts.
