# Slope input-label formula coverage

Covered means checked by both the committed fixture regression and the direct-workbook regression when the source is present. Missing caches never cause a formula to be omitted or skipped in fixture testing. Text outputs compare exactly; numeric outputs use relative tolerance 1e-9 and absolute tolerance 1e-12, with no relaxed tolerances.

For every missing-cache formula the evidence classification is **no cached value; checked by independent hand formula**. JSON records identify every individual address and formula. All source workbooks were present for verification.

## `tests/fixtures/geotechnical/slope/xslope_simple1.xlsx`

| Sheet | Formula cells total | Covered by a test | Not covered | Cached checks | Missing-cache hand checks | Evidence / reason |
| --- | ---: | ---: | ---: | ---: | ---: | --- |
| main | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| mat | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| profile | 15 | 15 | 0 | 0 | 15 | no cached value; checked by independent hand formula |
| polygon | 15 | 15 | 0 | 0 | 15 | no cached value; checked by independent hand formula |
| piezo | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| circles | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| non-circ | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| dloads | 1 | 1 | 0 | 1 | 0 | Cached value; checked exactly. |
| dloads (2) | 1 | 1 | 0 | 1 | 0 | Cached value; checked exactly. |
| reinforce | 40 | 40 | 0 | 0 | 40 | no cached value; checked by independent hand formula |
| piles | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| lloads | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| seep bc | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| seep bc (2) | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| tseep | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| **Total** | **72** | **72** | **0** | **2** | **70** | All formula cells tested. |

Formula families:

- profile!A6:AQ6, every third column: exact material-ID lookup followed by text concatenation. Independent expectation scans mat columns A:B, returning empty text for a matched blank name after concatenation, following documented Excel coercion (no cached source value covers this case).
- polygon!A7:AQ7, every third column: the same lookup gated by blank/material region type; the hand test evaluates the gate independently.
- reinforce!H3:I22: IF/IFERROR and exact VLOOKUP. The hand test scans the formula-referenced table and selects the requested column. All source selectors are blank, so supplemental populated selectors exercise the real table contents.
- dloads!B2 and dloads (2)!B2: water-mode conditional text. The hand test reads the referenced main cell and selects the literal instruction from the formula.

## `tests/fixtures/geotechnical/slope/xslope_clay_blanket.xlsx`

| Sheet | Formula cells total | Covered by a test | Not covered | Cached checks | Missing-cache hand checks | Evidence / reason |
| --- | ---: | ---: | ---: | ---: | ---: | --- |
| main | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| mat | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| profile | 15 | 15 | 0 | 1 | 14 | no cached value; checked by independent hand formula; cached results also checked exactly |
| polygon | 15 | 15 | 0 | 1 | 14 | no cached value; checked by independent hand formula; cached results also checked exactly |
| piezo | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| circles | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| non-circ | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| dloads | 1 | 1 | 0 | 1 | 0 | Cached value; checked exactly. |
| dloads (2) | 1 | 1 | 0 | 1 | 0 | Cached value; checked exactly. |
| reinforce | 40 | 40 | 0 | 0 | 40 | no cached value; checked by independent hand formula |
| piles | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| lloads | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| seep bc | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| seep bc (2) | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| tseep | 0 | 0 | 0 | 0 | 0 | No formula cells. |
| **Total** | **72** | **72** | **0** | **4** | **68** | All formula cells tested. |

Formula families:

- profile!A6:AQ6, every third column: exact material-ID lookup followed by text concatenation. Independent expectation scans mat columns A:B, returning empty text for a matched blank name after concatenation, following documented Excel coercion (no cached source value covers this case).
- polygon!A7:AQ7, every third column: the same lookup gated by blank/material region type; the hand test evaluates the gate independently.
- reinforce!H3:I22: IF/IFERROR and exact VLOOKUP. The hand test scans the formula-referenced table and selects the requested column. All source selectors are blank, so supplemental populated selectors exercise the real table contents.
- dloads!B2 and dloads (2)!B2: water-mode conditional text. The hand test reads the referenced main cell and selects the literal instruction from the formula.

## Cache mismatches and limitations

No cached-value mismatches remain. No formula cells are excluded for chart helpers, text concatenation, macros or external links; the text formulas are explicitly covered. There are no external links or VBA archives in these source files. The unused broken defined names in the slope workbooks are documented in README.md and are not dependencies of any formula.

The absent slope caches provide no independent spreadsheet-engine numerical oracle. The stated hand checks establish the visible IF, lookup and concatenation semantics from source inputs; they do not validate slope factors of safety or seepage solutions. No Excel recalculation or macro was run.
