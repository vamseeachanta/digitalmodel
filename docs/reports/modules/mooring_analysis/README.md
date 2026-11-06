# Mooring analysis module report

The [report](report.html) documents the synthetic touchdown Case A from the
[existing closed-form regressions](../../../../tests/subsea/mooring_analysis/test_catenary_geometry.py).
Its method map separates implemented interfaces from independent numerical
evidence. Design-code compliance, actual line strength, stiffness qualification,
environmental loading and dynamic mooring behavior remain `NOT_EVALUATED`.
The pinned-end example uses a separate solver; the legacy dynamic port is excluded.

The [builder](build_report.py) produces the self-contained page and the adjacent
[change record](changes.json) and [source record](sources.json). Source digests
pin repository evidence. The
[report tests](../../../../tests/subsea/mooring_analysis/test_module_report.py)
check independent hand arithmetic, visible values and units, controlled result
mutations, safe escaping and deterministic rebuilds. Reproducibility does not
establish physical validation.

From the repository root, with `TMPDIR` set to supplied scratch storage and a
prepared scratch interpreter containing NumPy and PyYAML, rebuild with:

```bash
PYTHONPATH=src PYTHONDONTWRITEBYTECODE=1 uv run --no-project --python "$TMPDIR/venv/bin/python" python docs/reports/modules/mooring_analysis/build_report.py --output-dir "$TMPDIR/mooring-report"
```

The explicit destination receives only `report.html`, `changes.json` and
`sources.json`. The builder writes no intermediate files. Keep dependency caches
in the supplied cache storage. After source changes, rebuild and compare all
three artifacts before replacing the committed outputs. Repository links require
the repository tree; the displayed analysis has no external resource dependency.

Broader numerical qualification remains open under
[issue 3319](https://github.com/vamseeachanta/digitalmodel/issues/3319).
The [package documentation](../../../../src/digitalmodel/subsea/mooring_analysis/README.md)
retains the standalone examples and API routes.
