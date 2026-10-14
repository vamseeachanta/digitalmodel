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
invented or adjusted. `assumptions.yml` records genuine assessment assumptions
and method citations. Licensed factors, limits and equation constants are not
reproduced in that file or the report.

Both grids are bounded local patches; general-loss averaging is not applied.
Section A requires escalation under the declared point-minimum pressure-wall
intervention rule. Section B requires more measurements to resolve its local
profile. Neither decision establishes continued operation while the stated
evidence gap remains unresolved.

Decision precedence is escalation, additional measurements, re-rating, then
acceptance. Applicability flags and zero projected life require escalation.
Pressure reduction uses pressure ratios only; the axial limit remains
a separate restriction. Remaining life is a linear projection to the pressure
wall criterion, not a prediction of all future defect-capacity crossings or an
API Level 1 assessment.

The local-loss calculation calls the existing relation with the wall away from
the flaw as its reference. The constructor argument named `t_min_in` supplies
that reference internally; the actual required pressure wall is kept separate.
The uncorroded pressure rating is obtained by inverting the existing linear
B31.8 pressure-wall relation, then `reduced_mawp` applies the strength reduction.
The reduced rating participates in the decision. This correction does not
establish complete edition-matched qualification of the simplified relation.

The HTML contains inline vector figures and requires no network. Its companion
manifest retains full-precision results and source/content digests. Text outputs
are UTF-8 with LF. The fixed document date is not read from the clock; repeat
builds are intended to be byte-identical. The document-control block is marked
synthetic example and names no person or unperformed check or approval.
Provenance remains mandatory and is retained in the manifest. The generated-page
registry owns both outputs and names their synthetic input dependencies.

The numerical engines, original inputs, measured-data fixtures and other
workflows are unchanged. No external engineering program is used.
