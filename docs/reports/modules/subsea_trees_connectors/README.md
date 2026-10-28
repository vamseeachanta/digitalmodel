# Subsea trees and connectors module analysis

Open [index.html](index.html) for the self-contained analysis. The example uses
synthetic inputs and normal public package imports; it establishes static
arithmetic only. Fatigue assessment, dynamic response, network coupling and
standards compliance remain NOT_EVALUATED.

From the repository root, use the existing x86 Python environment with the
package calculation dependencies, including pylife. Set `TMPDIR` to external
scratch and run:

```sh
PYTHONPATH=src uv run python scripts/reports/build_subsea_trees_connectors_module_report.py \
  --scratch "$TMPDIR/subsea-report-scratch" \
  --output-dir docs/reports/modules/subsea_trees_connectors
PYTHONPATH=src uv run python -m pytest \
  tests/reporting/test_subsea_trees_connectors_module_report.py \
  --basetemp="$TMPDIR/subsea-report-tests" -p no:cacheprovider
```

The command requires explicit scratch and output destinations. Intermediates
are permitted only outside the checkout; only the five final deliverables are
written into the output directory. Text uses UTF-8 and LF. Inputs and unrounded
API outputs are retained in [results.json](results.json), revision history in
[changes.json](changes.json), and source/delivery digests in
[manifest.json](manifest.json). The manifest hashes every other delivered file;
its own bytes are covered by the exact rebuild test.

Two independent rebuild directories are compared byte for byte with each other
and this delivery. Separate hand-integrated annulus, linear-strain and flow
balance oracles check numerical meaning. Negative controls reject consistently
regenerated wrong values and sign reversals. Rebuild equality alone establishes
freshness, not numerical or physical validity.

All methods and assumptions are traced in the report. A changed package source
requires renewed source review and a revised source pin; regeneration alone
does not qualify a changed engineering method.
