# Synthetic pipeline fitness-for-service assessment

The finished example is [the HTML assessment](../../../docs/reports/pipeline-ffs-assessment.html).
Open Sections 1, 3.5 and 7 first. All inputs are synthetic; the document does not
qualify an operating asset.

From the root, with the package source on `PYTHONPATH` and the reporting and
assessment dependencies installed in an isolated environment:

```sh
python scripts/reporting/build_pipeline_ffs_report.py
python -m pytest tests/asset_integrity/test_pipeline_ffs_assessment.py
```

The builder uses the two existing inputs referenced by `input.yml`. Their SHA-256
pins normalise CRLF to LF without rewriting the originals. Supplementary values
needed by the six-method comparison are explicit assumptions; no grids are
invented or adjusted. `assumptions.yml` records the active coefficients and their
held implementation sources. The public document separates assumed coefficients
from method citations.

Both sections require more measurements under the held sufficiency criteria.
Section A also has zero life to the assumed required pressure wall; this calls
for immediate engineering action despite its current-strength ratio above unity.
The report retains this disagreement rather than tuning data or classifications.

Any applicability flag overrides all other decisions. Otherwise inadequate
sampling requires more measurements regardless of numerical strength. With
sufficient measurements, intervention from the supplemental assessment is
retained. Pressure reduction uses pressure ratios only; the axial limit remains
a separate restriction. Remaining life is a linear projection to the pressure
wall criterion, not a prediction of all future defect-capacity crossings.

The HTML contains inline vector figures and requires no network. Its companion
manifest retains full-precision results and source/content digests. Text outputs
are UTF-8 with LF and no timestamp; repeat builds are intended to be byte-identical.
The opt-in assessment presentation suppresses administrative and execution
metadata while provenance remains mandatory and is retained in the manifest.

The numerical engines, original inputs, measured-data fixtures and other
workflows are unchanged. No external engineering program is used.
