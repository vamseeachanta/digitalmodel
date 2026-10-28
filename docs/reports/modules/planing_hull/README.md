# Planing hull module analysis

[Open the analysis](index.html). The calculation checks the synthetic
straight-sided wedge below the chine against the independent pressure integral
in the [existing hydrostatics tests](../../../../tests/unit/hydrodynamics/test_planing_hull_hydrostatics.py).
The [source register](source_register.json) gives inputs, units, source locators
and readiness. The [change record](changes.json) preserves revision rows.

From the repository root, with NumPy available, run:

```bash
PYTHONPATH=src uv run --no-project --with pytest python docs/reports/modules/planing_hull/build_report.py --scratch "$TMPDIR/planing-calculation-a" --output "$TMPDIR/planing-report-a"
PYTHONPATH=src uv run --no-project --with pytest python docs/reports/modules/planing_hull/build_report.py --scratch "$TMPDIR/planing-calculation-b" --output "$TMPDIR/planing-report-b"
```

`TMPDIR` shall name an external scratch directory. Each calculation writes only
`calculation.json` under its scratch folder; delivery writes only `index.html`
and `changes.json` to the selected output folder. Scratch and output shall not
overlap. For an intentional report refresh, select this report folder as
`--output`. Tests always select external output folders.

No volatile metadata is generated. Reproducibility compares complete UTF-8
bytes, without removing timestamps, paths or other fields. Source digests in the
embedded calculation bind the report to the inspected implementation, test,
register, local documentation and assets. Any source change requires a rebuild.
The local source/register files remain in place; an external preview can be read
standalone, while repository links resolve in the delivered repository folder.

Run the specified checks with external basetemp and no pytest cache:

```bash
PYTHONPATH=src uv run --no-project --with pytest python -m pytest tests/unit/hydrodynamics/test_planing_hull.py tests/unit/hydrodynamics/test_planing_hull_hydrostatics.py tests/reporting/test_planing_hull_module_report.py -q -rA -p no:cacheprovider --basetemp "$TMPDIR/planing-tests"
```

The dedicated report test compares every worked value, unit and display with an
independent hand calculation, including the curve and hand-anchor coordinates.
It also checks deterministic rebuilds, stale-result rejection, local links,
revision history and the section count without starting a browser.

The existing behavioral tests may exercise motion integration; no additional
response campaign is run. Dynamic accuracy, experimental comparison and
physical release remain `NOT_EVALUATED`. The way forward remains tracked by
[the open module issue](https://github.com/vamseeachanta/digitalmodel/issues/3329).
