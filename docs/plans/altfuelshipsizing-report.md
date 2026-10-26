# Alternative fuel ship sizing report: Plan-lite

The report presents the existing example calculation and its limitations in a
reproducible, self-contained HTML document. This plan records the report scope,
acceptance checks and qualification boundary.

## Scope

Retain the report generator, HTML and change record under
`docs/reports/modules/altfuelshipsizing/` and the reconstruction tests in
`tests/docs/test_altfuelshipsizing_report.py`. Use the retained example in
`examples/workflows/alt-fuel-report-pack/sizing_input.yml` and the existing public
sizing API. The report scope excludes changes to sizing equations, default
properties, workflow behaviour and engineering acceptance criteria.

## Reconstruction and arithmetic checks

1. Rebuild `index.html` and `changes.json` with `generate.py --output-dir` targeting
   a temporary directory, with `PYTHONPATH=src`. Compare document structure,
   displayed numerical evidence and chart data against the retained report;
   require explicit UTF-8 and LF text output.
2. Check the headline mass, tank volume, port loss, volume increase and sea-only
   warning boundary against the hand-worked literal expectations. Check the
   substituted reference properties separately. API/export consistency checks
   establish agreement between representations; they do not independently
   establish the physical model.
3. Run the report, sizing-library, sizing-workflow and retained-example tests:
   `tests/docs/test_altfuelshipsizing_report.py`,
   `tests/naval_architecture/test_alt_fuel_ship_sizing.py`,
   `tests/naval_architecture/test_alt_fuel_ship_sizing_workflow.py` and
   `tests/report_pack/test_alt_fuel_report_pack_demo.py`.
4. Check repository structure, engineering register, syntax, text hygiene,
   publication identifiers and Git LFS attributes. Inspect hosted failures before
   attributing them to a calculation or test defect; cancelled jobs provide no
   test result.

## Qualification limits

The powering, efficiencies, reserve, ullage and heat flux are example assumptions.
The report retains configured properties and separately quantifies substitution
of the cited open-reference properties. Reconciliation of the module constants'
source attribution remains separate from this report scope.

Reconstruction, arithmetic and API consistency do not establish actual storage
conditions, tank thermal performance, operational suitability or regulatory
acceptance. Physical qualification remains `NOT_EVALUATED`. Native Windows
execution, browser interaction and the full repository test suite require
separate evidence; the focused checks alone establish none of them.
