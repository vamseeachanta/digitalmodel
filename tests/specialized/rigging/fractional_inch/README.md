# Fractional-inch rigging table sizes

The shackle reader in `src/digitalmodel/specialized/rigging/rigging_components.py` reuses `FromString.convert_fraction_to_float` from `digitalmodel.asset_integrity.common.data` for fractional text in the `size_in` column. Numeric cells retain their existing coercion. Fractional text must match a complete signed fraction or mixed number with integer components and a single separating space. Invalid syntax, zero denominators and non-finite converted values raise a ValueError naming the part number. The conversion covers text entries such as "3/16" or "1 3/16"; the shipped table already yields numeric values.

## Workbook map — anchor shackle size table

Sheets: 1. External links: 0. VBA archive present: False.

| Sheet | Formula cells | Cached values present | Results / constant dependencies |
| --- | ---: | ---: | --- |
| Sheet1 | 4 | 4 | A2, A4, A5, A6; fractional-inch literals, with size_in label at A1 |

Defined names: none.

## Inputs and method

Input is fractional-inch text, including mixed numbers; output is decimal inches. The size_in label at Sheet1!A1 supplies the unit. The four formulas divide literal numerators by denominators; other table columns are constants. No engineering standard or clause is cited or inferred. The existing converter needs no runtime Excel dependency.

## Verification

`test_fractional_inch.py` contains eleven cases using the existing converter: four committed-fixture checks, four direct-workbook checks, one formula inventory check, and two adjacent-float rejection cases. `tests/specialized/rigging/test_component_lookup.py` also checks fractional and mixed-number text through the shackle reader. Every formula is compared with exact rational arithmetic and its cached result using exact equality; all four fractions are binary-exact. Direct-workbook cases compare live extraction with the fixture and skip only when the source workbook is absent. Fixture cases run independently of workbook availability.

The shared `tests/geotechnical/slope_input_labels/formula_evidence.py` extractor reads formulas and caches separately without saving source workbooks. It writes slope evidence beside the slope tests and shackle evidence into this folder's `fixtures/anchor_shackle_sizes.json`. See `COVERAGE.md` for the formula accounting.

From the repository root, run:

```sh
python -m pytest tests/geotechnical/slope_input_labels tests/specialized/rigging/fractional_inch
python tests/geotechnical/slope_input_labels/formula_evidence.py
```

```python
from digitalmodel.asset_integrity.common.data import FromString

size_in = FromString().convert_fraction_to_float("3/16")
```
