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
reproduced in the workflow, report or companion manifest. The builder obtains
the Section B factors from named numerical-method defaults at calculation time.

Both grids are bounded local patches; general-loss averaging is not applied.
Section A requires formal local-loss evaluation because its remaining strength
factor is below the allowable factor; its field data is adequate. The current
reduced rating exceeds demand. Section B requires added readings to resolve at least three more degraded cells:
two degraded cells resolve its flaw against an assessment default of five.
Interim operation is a decision for the maintainer of the pipeline record.

Applicability flags and severe loss below the replacement floor require
escalation regardless of sampling. Otherwise sampling adequacy is checked first;
the strength-factor referral is evaluated only after sampling meets the
Section 3.5 criterion.
Acceptance or monitoring gives accept only with positive remaining life.
Pressure reduction uses pressure ratios only; the axial limit remains
a separate restriction. Section A life is projected to design pressure on the
reduced local-loss route, holding the flaw extent, background wall and loads
fixed. Section B retains the linear pressure-wall projection. Neither route
predicts all future defect-capacity crossings or authorizes operation.

The local-loss calculation calls the existing relation with the wall away from
the flaw as its reference. The constructor argument named `t_min_in` supplies
that reference internally; the actual required pressure wall is kept separate.
The uncorroded pressure rating is obtained by inverting the existing linear
B31.8 pressure-wall relation, then `reduced_mawp` applies the strength reduction.
The reduced rating participates in the decision. This correction does not
establish complete edition-matched qualification of the simplified relation.

The HTML contains inline vector figures and requires no network. Its companion
manifest retains full-precision results and source/content digests, excluding
method factors and quoted thresholds. Diagnostic pressure-wall and strength-factor
comparisons are labelled separately from the reduced-rating pressure criterion. Text outputs
are UTF-8 with LF. The fixed document date is not read from the clock; repeat
builds are intended to be byte-identical. The document-control block is marked
synthetic example through document metadata and names no person or unperformed
check or approval. Other assessments receive no implicit synthetic label or
signature placeholders. Assessment presentation does receive a default one-row
revision history labelled "Issued" when no explicit history is supplied.
Provenance remains mandatory and is retained in the manifest. The generated-page
registry owns both outputs and names their synthetic input dependencies.

The numerical engines, original inputs, measured-data fixtures and other
workflows are unchanged. No external engineering program is used.
