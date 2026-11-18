# Polars export row-count tests

The long-format export tests count rows from the input fixture dimensions.
Changing the shared frequency grid therefore does not require replacing a
fixed expected total. This guide covers the tests for issue #2540.

The inputs are defined in
[the shared fixtures](../../../tests/hydrodynamics/diffraction/conftest.py),
and the motion axes are defined by `DOF` in
[the output schema](../../../src/digitalmodel/hydrodynamics/diffraction/output_schemas.py).
These repository definitions are the source of the dimensions below; the
examples are combinatorial counts, without engineering coefficients or
acceptance limits.

## Method and worked example

For RAOs, each frequency, heading and DOF combination contributes a row:
`rows = frequency_count * heading_count * len(DOF)`.
For either added mass or damping, each frequency and ordered DOF pair
contributes a row:
`rows = matrix_set.frequencies.count * len(DOF) * len(DOF)`.
The counts are calculated from the inputs, independently of the exporter.

| Fixture | Frequencies | Headings | DOFs | RAO calculation | Each matrix export |
| --- | ---: | ---: | ---: | --- | --- |
| `mock_diffraction_results` | 40 | 5 | 6 | 40 × 5 × 6 = 1200 | 40 × 6 × 6 = 1440 |
| `dense_diffraction_results` | 24 | 13 | 6 | 24 × 13 × 6 = 1872 | 24 × 6 × 6 = 864 |

Table 1. Hand counts from the shared fixture grids and the DOF schema.

The mock grid derives its frequency count from `MIN_FREQUENCY`,
`MAX_FREQUENCY` and `MAX_RELATIVE_GAP` in the shared fixture module.
The dense grid uses that module's `N_FREQ_DENSE` and `HEADINGS_DENSE`.
The table describes the current inputs; the assertions read their dimensions
at runtime rather than fixing these example totals.

## Regression coverage

[The exporter tests](../../../tests/hydrodynamics/diffraction/test_polars_exporter.py)
exercise both grids for RAOs, added mass and damping. Existing column,
value, CSV and Pandas checks remain in place. The
[additional exporter tests](../../../tests/hydrodynamics/diffraction/test_polars_exporter_additional.py)
cover unavailable dependencies and fallback paths, while
[matrix unit tests](../../../tests/hydrodynamics/diffraction/test_matrix_unit_completeness.py)
cover the exported unit blocks.

Run with Polars installed so the DataFrame assertions execute:

```sh
PYTHONPATH=src uv run --no-project --with pytest --with pytest-asyncio==0.21.1 \
  --with polars==2.0.0 --with numpy --with pandas --with pyyaml --with scipy \
  --with pydantic --with matplotlib --with plotly --with loguru \
  python -m pytest tests/hydrodynamics/diffraction/test_polars_exporter.py \
  tests/hydrodynamics/diffraction/test_polars_exporter_additional.py \
  tests/hydrodynamics/diffraction/test_matrix_unit_completeness.py \
  -q -rA -p no:cacheprovider --basetemp="$TMPDIR/polars-export-tests"
```

`TMPDIR` must identify a writable external scratch directory. These tests
use synthetic results and require no solver execution. They check export
structure and fixture values; they do not establish physical validity of a
diffraction solution.
