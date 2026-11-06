# Subsea pipeline module analysis

Read [the report](index.html), [revision history](changes.json) and
[source/artifact digests](manifest.json).

The worked examples are a synthetic annulus, signed unrestrained thermal growth
and multiplication of a supplied capacity by supplied factors. The independent
hand calculations are written in the report and its two dedicated test files.
They do not qualify buckling, span factors or pressure capacities; these remain
`NOT_EVALUATED` pending independent engineering evidence.

Rebuild from the repository root in the existing scientific Python environment.

```bash
PYTHONPATH=src python scripts/reports/build_subsea_pipeline_module_report.py
```

Use `--output-dir "$REPORT_OUTPUT"` for a separate destination. The builder writes
only `index.html`, `changes.json`, `manifest.json` and `README.md`. No intermediate
calculation files are needed. `--scratch` is an optional reserved compatibility
argument; it writes nothing and, when supplied, must resolve outside the checkout.
The manifest hashes the three executed pipeline modules, unit support, the builder
and two dedicated test files, plus the other three deliverables. Its own digest is
excluded to avoid self-reference; compare all four output files byte for byte
between independent builds, including `manifest.json`.

The public-method inventory lists repository-relative paths and symbols without
line numbers, with explicit case-limited qualification status. It describes the
source declarations at generation, without a hard digest requirement on unevaluated
modules. Added or removed declarations need a deliberate rebuild; unrelated body
edits and line shifts do not invalidate delivered bytes.

Layout choice: the ten headings follow the existing module-analysis structure.
The compact self-contained stylesheet supports the wide declaration inventory
and arithmetic tables; the assessment template addresses site-specific operating
decisions instead. This presentation choice adds no engineering qualification.

Input ownership resides in this repository's builder; these are synthetic arithmetic inputs, not measured data,
material properties or code allowables. The issue remains open for the
unevaluated engineering methods.
