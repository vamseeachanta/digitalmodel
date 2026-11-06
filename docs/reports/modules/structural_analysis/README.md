# Structural analysis module analysis

Open [index.html](index.html) for the ten-section synthetic beam and ideal-column
analysis, method inventory, unit conventions, independent derivations and limits.

From the repository root, with NumPy available and TMPDIR outside the checkout:

```sh
PYTHONPATH=src python scripts/reports/build_structural_analysis_module_report.py --scratch "$TMPDIR/structural-analysis"
```

Use `--output-dir <directory>` to reproduce the four deliverables elsewhere.
No solver is used. Calculations are in memory; no intermediate files are written.
The builder rejects scratch inside the checkout before writing output.

The synthetic input basis and qualification criteria are revision A, dated
2026-10-28. Report revision C, dated 2026-11-06, aligns downstream consumers
with the supported sweep and workflow loads and narrows source-byte bindings.
The shared stylesheet is linked for presentation; its bytes are not pinned. `changes.json` records each
revision cause. `manifest.json` binds source
bytes and the other three artifacts by SHA-256; it excludes its own recursive
digest. Two independent builds are compared with the committed deliverables by
`tests/reporting/test_structural_analysis_module_report.py`.

IDEAL_MECHANICS_CHECKED covers the worked beam magnitudes, reproduction of the
module's prescribed signs and ideal elastic column only. Right-hand-rule moment
signs, design-code capacities, reduction curves and simplified combined loading
remain NOT_EVALUATED. The issue remains open pending independent source and
design qualification. See the report for the missing evidence and way forward.
