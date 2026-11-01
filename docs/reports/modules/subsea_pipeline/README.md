# Subsea pipeline module analysis

Read [the report](index.html), [revision history](changes.json) and
[source/artifact digests](manifest.json).

The worked examples are a synthetic annulus, signed unrestrained thermal growth
and multiplication of a supplied capacity by supplied factors. The independent
hand calculations are written in the report and its two dedicated test files.
They do not qualify buckling, span factors or pressure capacities; these remain
`NOT_EVALUATED` pending independent engineering evidence.

Rebuild from the repository root in the existing scientific Python environment.
Set `REPORT_SCRATCH` to an external temporary directory before running:

```bash
PYTHONPATH=src python scripts/reports/build_subsea_pipeline_module_report.py --scratch "$REPORT_SCRATCH"
```

Use `--output-dir "$REPORT_OUTPUT"` for a separate destination. The builder writes
only `index.html`, `changes.json`, `manifest.json` and `README.md`. No intermediate
calculation files are needed. Scratch must resolve outside the checkout.
The manifest hashes all pipeline source files, executed unit support, the builder
and regression sources, plus the other three deliverables. Its own digest is
excluded to avoid self-reference; compare all four output files byte for byte
between independent builds, including `manifest.json`.

The public-method inventory lists repository-relative symbol and line locators
with explicit case-limited qualification status. Input ownership resides in this
repository's builder; these are synthetic arithmetic inputs, not measured data,
material properties or code allowables. The issue remains open for the
unevaluated engineering methods.
