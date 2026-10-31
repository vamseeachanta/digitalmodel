# Hull library module report

The [module analysis](report.html) maps public interfaces and works through
in-memory scaling of a synthetic tilted rectangular panel. Hull forms, mesh
generation and response lookup are documented as interfaces; their physical
qualification remains NOT_EVALUATED.

[Source records](sources.json) bind repository evidence by SHA-256.
[Change records](changes.json) describe the report revision.
The [existing standalone example](../../../../src/digitalmodel/hydrodynamics/hull_library/docs/worked-panel-scaling.md)
retains its own executable contract and independent derivation.

From the repository root, with the repository dependencies active and `TMPDIR`
pointing to supplied scratch storage, rebuild with:

```bash
PYTHONPATH=src PYTHONDONTWRITEBYTECODE=1 MPLCONFIGDIR="$TMPDIR/matplotlib" XDG_CACHE_HOME="$TMPDIR/xdg-cache" uv run --no-project --python "$(command -v python3)" python docs/reports/modules/hull_library/build_report.py --output-dir "$TMPDIR/hull-report"
```

The output directory receives `report.html`, `changes.json` and `sources.json`;
no intermediate files are needed. The page embeds its styles and calculation
record and loads no external resources. Source links resolve from the report's
repository directory. Deterministic output does not establish physical validation.

The [report tests](../../../../tests/hydrodynamics/hull_library/test_module_report.py)
check unrounded hand calculations, every visible quantity and unit, calculation
mutation, escaping, source resolution and isolated rebuilds. Broader numerical
qualification remains open.
