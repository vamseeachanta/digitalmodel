# Parametric hull module analysis

[Generated report](index.html) · [Source register](sources.json) · [Revision record](changes.json)

The signed encounter-frequency case is derived from observer phase and checked
with exact rational arithmetic and cardinal cosines. Synthetic gravity is a test
parameter, not a physical recommendation. The report maps existing routes and
records static correctness findings; empirical correction, bank, meshing and BEM
qualification remain `NOT_EVALUATED` under [the parent issue](https://github.com/vamseeachanta/digitalmodel/issues/3327).

From the repository root, with `UV_CACHE_DIR`, `PRE_COMMIT_HOME` and `TMPDIR`
exported to the supplied external cache/scratch locations:

```sh
PYTHONPATH=src PYTHONDONTWRITEBYTECODE=1 uv run --no-project --with numpy python docs/reports/modules/parametric_hull_analysis/build_report.py --scratch "$TMPDIR/hull-calc-a" --output "$TMPDIR/hull-report-a"
PYTHONPATH=src PYTHONDONTWRITEBYTECODE=1 uv run --no-project --with numpy python docs/reports/modules/parametric_hull_analysis/build_report.py --scratch "$TMPDIR/hull-calc-b" --output "$TMPDIR/hull-report-b"
```

Compare all three generated files byte for byte. No volatile metadata is emitted
or stripped. The source register, generator, tests, methods and layout are bound
by SHA-256 in the report payload. Scratch is resolved and rejected if it lies
inside the checkout. The report inlines the established layout and palette and
generates its own phase sketch. No browser or solver is needed.

For explicit delivery regeneration, use the same command with
`--output docs/reports/modules/parametric_hull_analysis`. Tests always use
separate external scratch and output paths and never overwrite the delivery.
Previous revision rows in `changes.json` shall be preserved on reissue.

The existing tests need NumPy, pandas, pydantic, matplotlib, SciPy and xarray in
the active environment; the report builder needs only NumPy. Keep the test
environment and plotting cache in the supplied external scratch location.

```sh
mkdir -p "$TMPDIR/hull-tests"
PYTHONPATH=src PYTHONDONTWRITEBYTECODE=1 MPLCONFIGDIR="$TMPDIR/matplotlib" uv run --no-project --with pytest python -m pytest tests/hydrodynamics/parametric_hull_analysis/test_forward_speed.py tests/hydrodynamics/parametric_hull_analysis/test_shallow_water.py tests/reporting/test_parametric_hull_module_report.py -q -rA -p no:cacheprovider --basetemp="$TMPDIR/hull-tests/temp" > "$TMPDIR/hull-tests/pytest.log" 2>&1
```

The report tests compare every worked input, result, visible value and unit with
an independent hand calculation; check both result shapes and input preservation;
reject changed returned calculation fixtures; verify source digests; compare two
CLI deliveries and committed artifacts; and resolve local links and structure.
Passing those checks establishes reproducibility and arithmetic for the stated
case, not engineering qualification.
