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
2026-10-28. `changes.json` records the revision cause. `manifest.json` binds source
bytes and the other three artifacts by SHA-256; it excludes its own recursive
digest. Two independent builds are compared with the committed deliverables by
`tests/reporting/test_structural_analysis_module_report.py`.

IDEAL_MECHANICS_CHECKED covers the worked beam stress and ideal elastic column
only. Design-code capacities, reduction curves and simplified combined loading
remain NOT_EVALUATED. The issue remains open pending independent source and
design qualification. See the report for the missing evidence and way forward.
