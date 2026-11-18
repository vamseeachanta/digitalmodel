# Running the wave energy converter example

The WEC example can be imported without running its analysis, or executed to
write a workbook. An explicit output directory supports temporary and automated
runs. The smoke tests check execution and workbook structure; engineering
qualification remains outside their scope.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-21 | Document import and workbook execution so the example can be checked without writing beside its source. |

## Worked example

From the repository root, use a Python environment containing NumPy, pandas and
openpyxl, as declared in `pyproject.toml`:

```bash
python examples/domains/workflows/07_wave_energy_converter.py --output-dir wec-output
```

Inputs are the existing `WECDevice` defaults and the wave-height grid defined in
`main`. The method evaluates `analyze_wec_mooring` for each grid entry, places the
returned dictionaries in a pandas table and writes the table without an index.
No licensed solver is used.

The resulting artifact is `wec-output/wec_analysis.xlsx`. Its columns, in order,
are `wave_height`, `wave_force`, `max_tension`, `safety_factor`, `fatigue_damage`
and `status`. Wave height is in metres, force and tension in kilonewtons, and the
remaining calculated quantities are dimensionless. Each input wave height has
one populated row. These are the observable answers checked by the smoke test;
no design acceptance follows from a successful export.

A relative output path is resolved from the working directory. Missing output
directories are created. Omitting `--output-dir` preserves the original
`outputs/07_wec/wec_analysis.xlsx` destination beside the script. An existing
workbook at the selected destination is overwritten by the existing exporter.

To call the example from Python without executing its script entry point:

```python
import runpy
from pathlib import Path

example = runpy.run_path(
    "examples/domains/workflows/07_wave_energy_converter.py"
)
example["main"](output_dir=Path("wec-output"))
```

Importing alone leaves the Python search path and root logging handlers intact
and writes no workbook. Calling `main` configures logging for script use.

## Smoke checks

```bash
uv run --no-project --with pytest --with 'numpy>=1.24,<2' --with 'pandas>=2,<3' --with 'openpyxl>=3.1,<4' python -m pytest tests/scripts/test_wave_energy_converter_example.py -q -rA -p no:cacheprovider
```

The tests resolve function annotations, exercise import in a fresh interpreter,
execute the real script from another working directory, check a requested
workbook destination and preserve the original default destination using a
temporary script copy. All generated test workbooks are temporary artifacts.
The default destination is tested without writing into the repository.

Expected row inputs are independently enumerated from the existing example's
inclusive grid; the test docstring gives the row-count calculation. Workbook
column names and filename follow the existing export contract. No expected
engineering result is obtained from the analysis function under test.

## Sources and engineering limits

[Python typing documentation, Python 3.11, typing.Dict](https://docs.python.org/3.11/library/typing.html#typing.Dict)
identifies built-in `dict` as the replacement for `typing.Dict`.
[Annotation introspection](https://docs.python.org/3.11/library/typing.html#typing.get_type_hints)
provides the annotation-resolution check used by the regression test.

The equations, device defaults, load assumptions, rope properties, fatigue
surrogate and acceptance threshold in the existing example are unchanged.
They are illustrative and unqualified, including the exported `PASS` or `FAIL`
labels. Their presence does not establish compliance with the standards named
in the script header. No new engineering constant or acceptance limit is added.
A numerical design answer remains NOT_EVALUATED by these smoke tests.
Qualification requires documented site inputs, sourced material properties,
justified load models and applicable acceptance criteria in separate work.
