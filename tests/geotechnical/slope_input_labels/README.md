# Slope input labels

## Workbook map (recorded before implementation)

Inspection uses openpyxl in formula and cached-value modes. Only new files are added. Source workbooks are read without saving.

### `tests/fixtures/geotechnical/slope/xslope_simple1.xlsx`

Sheets: 15. External links: 0. VBA archive present: False.

| Sheet | Formula cells | Cached values present | Results / constant dependencies |
| --- | ---: | ---: | --- |
| main | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |
| mat | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |
| profile | 15 | 0 | A6:AQ6, every third column; row 5 material IDs and mat columns A:B |
| polygon | 15 | 0 | A7:AQ7, every third column; rows 5–6 type/ID and mat columns A:B |
| piezo | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |
| circles | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |
| non-circ | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |
| dloads | 1 | 1 | B2 guidance; main!D23 water-load mode |
| dloads (2) | 1 | 1 | B2 guidance; main!D23 water-load mode |
| reinforce | 40 | 0 | H3:I22; G3:G22 types and exact lookup table Z8:AB11 |
| piles | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |
| lloads | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |
| seep bc | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |
| seep bc (2) | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |
| tseep | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |

Defined names: `crack_depth = main!$D$11`; `crack_depth_water = main!$D$12`; `gamma_w = main!$D$10`; `k = main!$D$13`; `max_depth = profile!$B$2`; `mesh = mat!#REF!`; `solution1 = mat!#REF!`; `solution2 = mat!#REF!`; `version = main!$D$5`.

### `tests/fixtures/geotechnical/slope/xslope_clay_blanket.xlsx`

Sheets: 15. External links: 0. VBA archive present: False.

| Sheet | Formula cells | Cached values present | Results / constant dependencies |
| --- | ---: | ---: | --- |
| main | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |
| mat | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |
| profile | 15 | 1 | A6:AQ6, every third column; row 5 material IDs and mat columns A:B |
| polygon | 15 | 1 | A7:AQ7, every third column; rows 5–6 type/ID and mat columns A:B |
| piezo | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |
| circles | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |
| non-circ | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |
| dloads | 1 | 1 | B2 guidance; main!D24 water-load mode |
| dloads (2) | 1 | 1 | B2 guidance; main!D24 water-load mode |
| reinforce | 40 | 0 | H3:I22; G3:G22 types and exact lookup table AB8:AD11 |
| piles | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |
| lloads | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |
| seep bc | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |
| seep bc (2) | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |
| tseep | 0 | 0 | No formula results; remaining geometry, soil, boundary and option inputs are outside this helper conversion |

Defined names: `crack_depth = main!$D$11`; `crack_depth_water = main!$D$12`; `gamma_w = main!$D$10`; `k = main!$D$13`; `max_depth = profile!$B$2`; `mesh = mat!#REF!`; `solution1 = mat!#REF!`; `solution2 = mat!#REF!`; `version = main!$D$5`.

## Relationship to existing slope code

`git grep -n -i xslope` locates `src/digitalmodel/geotechnical/slope_stability.py`, `slope_profile.py`, and `tests/geotechnical/test_slope_stability.py` / `test_slope_profile.py`. The adapter runs limit equilibrium and seepage; profile code validates geometry and soil inputs. These workbook formulas only supply lookup labels and entry guidance. They do not calculate slope safety factors. Existing solving, validation, interpolation and strength functions will be reused unchanged by callers; none is duplicated here.

## Inputs and units

Slope helper inputs are dimensionless material IDs and text (names, region types, reinforcement types, direction/application labels, water-load mode). The simple case declares Imperial and the clay case SI at main!D8; helper formulas perform no physical-unit arithmetic. No runtime Excel dependency is needed.

## Method

Use exact, first-match, case-insensitive Excel lookups. A matched empty material name concatenated with empty text returns empty text, following documented Excel coercion. No cached source value covers this case. A missing ID returns empty text. Polygon labels apply only to blank or material region types. Reinforcement rows return blank for an unused type or failed exact lookup. Load guidance follows main!D23 in the simple case and main!D24 in the clay case. Repeated blocks use one function per operation.

## Standards and clauses

No cited engineering standards or clauses were found in the workbook cell text or comments. No standard is inferred from the table name.

## Input gaps

No unlabelled numeric constant is used by the converted formulas. Lookup indices are structural column selectors. The blank material names at mat!B12:B25 in both slope workbooks are missing labels: preserve them as None inputs and return empty text after concatenation, without supplying names. The unused reinforcement selectors at reinforce!G3:G22 remain None inputs and produce empty text. These gaps do not affect the existing slope solver.

The defined names mesh, solution1 and solution2 point to mat!#REF! in both slope workbooks. No formula in this scope uses them. No replacement target is assumed. The clay reinforcement table is shifted to AB8:AD11 and its formulas correctly reference that location; the simple case uses Z8:AB11.

## What is left

All formulas in scope are implemented and tested. No formula conversion remains. Safety factors and seepage outputs belong to the existing optional solver tests. A future template revision may supply the missing material names or repair unused defined names; those source changes are outside this revision. General Excel evaluation, wildcard reinforcement selectors, and additional table calculations are outside this literal-input helper API.

## Implementation and verification

Calculation code is in `src/digitalmodel/geotechnical/slope_input_labels.py` and implements material, polygon, reinforcement and load-guidance helpers. Inputs are frozen dataclasses, collections are tuples, and functions do not mutate inputs or import Excel libraries. Literal reinforcement names use case-insensitive first-match lookups. The two source tables have equivalent contents at their respective locations.

The shared `tests/geotechnical/slope_input_labels/formula_evidence.py` extractor reads each workbook twice and writes all formula texts, cached values (including null), required input constants (including blank cells, grouped one worksheet row per JSON line), sheet counts, defined names and link/VBA metadata into `fixtures/`. Whole-column material lookups are captured through the last nonblank sheet row; later blank rows cannot match the nonblank material IDs in these workbooks.

`test_slope_input_labels.py` runs one fixture case and one direct-workbook case for every formula cell. It independently scans source lookup rows and evaluates the IF conditions, for the slope labels and guidance. Direct-workbook cases verify extracted evidence equals the committed fixture and skip if a workbook is absent. The fixture cases still run without workbooks. Text is compared exactly. Numbers use relative tolerance 1e-9 and absolute tolerance 1e-12; no tolerance is relaxed. Supplemental cases exercise populated and failed reinforcement lookups, case-insensitive conditions, missing material IDs, duplicate lookup IDs, and blank return cells.

The slope tests contain 315 cases. Two extractor cases regenerate all three fixtures byte for byte with absolute and relative output directories under emulated Windows newline defaults. Of 144 formula cells, 6 have non-null cached values and 138 are checked by independent hand formulas. See `COVERAGE.md` for the sheet-level accounting and cache limits.

From the repository root, run:

```sh
python -m pytest tests/geotechnical/slope_input_labels tests/specialized/rigging/fractional_inch
python tests/geotechnical/slope_input_labels/formula_evidence.py
```

Example without a workbook at runtime:

```python
from digitalmodel.geotechnical.slope_input_labels import (
    MaterialLabel, MaterialLabelInput, material_name,
)

# Inputs visible at mat!A11:B11 in both slope workbooks.
name = material_name(MaterialLabelInput(1), (MaterialLabel(1, "soil"),))
```
