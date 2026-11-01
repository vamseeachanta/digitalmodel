# Offshore-container module analysis

The [report](index.html) connects public section and sling geometry to
independent hand derivations, then demonstrates caller-factor arithmetic.
All factors in the example are synthetic inputs, not a prescribed standard basis.
Strength, padeye adequacy, actual load sharing and certification are NOT_EVALUATED.

## Rebuild

From the repository root, choose external scratch and output directories:

```sh
PYTHONPATH=src python scripts/reports/build_offshore_container_module_report.py \
  --scratch "$REPORT_SCRATCH" --output-dir "$REPORT_OUTPUT"
```

Both arguments are required. Scratch is rejected if it is inside the checkout.
Only final deliverables belong in this folder; intermediate calculation evidence
is written to scratch. Run the report tests with an external pytest base:

```sh
PYTHONPATH=src python -m pytest tests/reporting/test_offshore_container_module_report.py \
  -q -p no:cacheprovider --basetemp="$TEST_SCRATCH"
```

The builder generates index.html, results.json, changes.json, this README and
manifest.json. The manifest binds every other delivered file and the focused
source evidence using SHA-256 over exact UTF-8 bytes with LF endings. The manifest
cannot bind itself; byte-rebuild tests compare it in full. No paths, run times or
environment identities are serialized. The HTML contains inline CSS, SVG and
escaped JSON; no network assets or executable scripts are needed to read it.

The tests separate normal API assertions against geometry integrals and explicit
synthetic arithmetic from freshness checks. Negative controls reverse signs and
change magnitudes even when report and builder agree. No physical acceptance,
parameter-range qualification or standards-compliance claim follows from them.
