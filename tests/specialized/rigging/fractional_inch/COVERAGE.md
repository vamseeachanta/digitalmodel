# Fractional-inch formula coverage

Covered means checked by both committed-fixture regression and direct-workbook regression when the source is present. Numeric comparisons use relative tolerance 1e-9 and absolute tolerance 1e-12. No tolerance is relaxed.

## Anchor shackle size table

| Sheet | Formula cells total | Covered by a test | Not covered | Cached checks | Missing-cache hand checks | Evidence / reason |
| --- | ---: | ---: | ---: | ---: | ---: | --- |
| Sheet1 | 4 | 4 | 0 | 4 | 0 | Cached decimal-inch sizes; exact rational hand check and stated numeric tolerance. |
| **Total** | **4** | **4** | **0** | **4** | **0** | All formula cells tested. |

Formula cells: Sheet1!A2 `=3/16`, A4 `=5/16`, A5 `=3/8`, A6 `=7/16`. All are fractional inches to decimal inches, rather than conversions between physical unit systems. Independent expectations use exact rational arithmetic.

All four formula cells have cached values. No cached-value mismatches remain, no formulas are excluded, and the source has no defined names, external links or VBA archive. The independent expectations use exact rational arithmetic. No spreadsheet recalculation or macro is run.
