# Well tubulars module analysis

[Open the report](index.html). The page contains its styling, generated diagram
and calculation data; it needs no remote assets or network connection.

## Rebuild

Use the source package and its normal NumPy and pandas dependencies. From the
repository root, set `PYTHONPATH=src` and run:

```sh
python scripts/reports/build_well_tubulars_module_report.py --scratch "$REPORT_SCRATCH" --output-dir "$REPORT_OUTPUT"
python -m pytest tests/reporting/test_well_tubulars_module_report.py -q -rA -p no:cacheprovider --basetemp="$TEST_SCRATCH"
```

Set the scratch variables to fresh external temporary directories. Output may
be an external directory or this exact delivery directory. Scratch and output
must be disjoint. No intermediate files are written into the source checkout.

## Evidence

- `results.json` records unrounded public-helper results and synthetic inputs.
- `changes.json` matches the revision history directly after the report summary.
- `manifest.json` binds the other delivered files and calculation/review sources
  with SHA-256 digests. It excludes itself to avoid a circular digest.
- The fixed reviewed revision identifies the source review. Current file digests
  identify build inputs; fresh digests do not imply a fresh engineering review.
- Independent force balance and principal-stress differences check the numerical
  results. Rebuilds and production replay alone establish reproducibility only.
- The manifest is an integrity record, not a signature or an acceptance decision.

The example assumptions and rounding budget come from the package's
[worked derivation](../../../../src/digitalmodel/well/tubulars/docs/worked-tubular-stress-invariants.md).
Collapse regimes, catalogue grades, sour-service criteria, rod fatigue and
design acceptance remain `NOT_EVALUATED`. No licensed table values or default
acceptance factors are reproduced. Engineering qualification remains open.
