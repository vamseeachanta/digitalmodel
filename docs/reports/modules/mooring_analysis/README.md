# Mooring analysis module report

The [report](report.html) documents the synthetic touchdown Case A from the
[existing closed-form regressions](../../../../tests/subsea/mooring_analysis/test_catenary_geometry.py).
Its method map separates implemented interfaces from independent numerical
evidence. Design-code compliance, actual line strength, stiffness qualification,
environmental loading and dynamic mooring behavior remain `NOT_EVALUATED`.
The pinned-end example uses a separate solver; the legacy dynamic port is excluded.

The [builder](build_report.py) produces the self-contained page and the adjacent
[change record](changes.json) and [source record](sources.json). Source digests
pin only the calculation, models, independent geometry tests and builder.
Other files are references without gating digests. The
[report tests](../../../../tests/subsea/mooring_analysis/test_module_report.py)
check independent hand arithmetic, visible values and units, controlled result
mutations, safe escaping and deterministic rebuilds. Reproducibility does not
establish physical validation.

From the repository root, use a Python environment with NumPy and PyYAML
installed. Replace `<output-directory>` with the chosen destination:

```bash
PYTHONPATH=src python docs/reports/modules/mooring_analysis/build_report.py --output-dir <output-directory>
```

The explicit destination receives only `report.html`, `changes.json` and
`sources.json`. The builder writes no intermediate files. After calculation or
oracle source changes, rebuild and compare all
three artifacts before replacing the committed outputs. Repository links require
the repository tree; the displayed analysis has no external resource dependency.

Broader numerical qualification remains open under
[issue 3319](https://github.com/vamseeachanta/digitalmodel/issues/3319).
The [package documentation](../../../../src/digitalmodel/subsea/mooring_analysis/README.md)
retains the standalone examples and API routes.
