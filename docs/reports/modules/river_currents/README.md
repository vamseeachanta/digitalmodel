# River currents module analysis

The [module analysis](index.html) documents the public function review and a
stipulated square-root profile with rectangular-channel corrections. Independent
hand integrals supply numerical expectations. Physical calibration, fitted
extremes, load coefficients and operating limits remain `NOT_EVALUATED`.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| A | 2026-10-28 | Initial module analysis records the synthetic profile and channel-correction arithmetic, public function review and unresolved physical basis so that the evidence can be rebuilt and assessed. |

From the repository root, with the package import dependencies available, set
`REPORT_SCRATCH` to a fresh external directory and run:

```sh
PYTHONPATH=src python scripts/reports/build_river_currents_module_report.py \
  --scratch "$REPORT_SCRATCH" --output-dir docs/reports/modules/river_currents
PYTHONPATH=src python -m pytest tests/reporting/test_river_currents_module_report.py
```

The CLI requires both locations. All calculation intermediates stay in external
scratch. The output directory contains only the small deliverables. The builder
uses normal public imports and does not rerun the linked towing/extreme examples.

[Results](results.json) retain full precision; visible answers use one decimal
place. [Changes](changes.json) match the HTML revision history. The
[manifest](manifest.json) binds the other deliverables, generator, calculation
sources and test evidence by SHA-256 of exact UTF-8 bytes with LF endings.
The manifest does not hash itself. Determinism establishes freshness; the
independent hand-calculation assertions establish the synthetic arithmetic.

The reviewed package revision is `09089a121a16069c4b657d1a320a630a23ab1dd5`.
No package source correction is included. Further calibration and acceptance
work remains open under the limits and way forward in the analysis.
