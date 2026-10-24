# Field development report reproduction

The standalone [report](report.html) documents the local concept and economic
screening example. The figures and checks below describe that example; they do
not establish engineering validation or investment acceptance.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-24 | Record the tied concepts, recovery comparison and tested environment so the report coverage can be described consistently. |

## Coverage figures

Spar and Semi tie at 92.5 points; stable legacy ordering selects Spar. Their
pre-tax NPVs are 16,045.82 and 13,743.90 million USD. Scheduled production is
662.71 million barrels, 65.7% above the supplied reference and 11.0 times the
recovery target; neither economic value supports investment acceptance.

The supplied reference is 400 million barrels. The module applies a recovery
factor of 0.15, giving a recovery target of 60 million barrels. The uncapped
plateau and inconsistent reserve definition remain limits of the calculation.
The tied ranking alone does not establish an engineering preference.

## Reproduction and checks

The focused checks used Python 3.12.3, NumPy 1.26.4, pandas 2.2.3 and
Matplotlib 3.9.4. These numerical and plotting packages satisfy their declared
version constraints. This is a focused environment, not a full dependency-lock
compatibility result. The optional `worldenergydata` backend is absent.

With the test dependencies and the module's import dependencies installed, run
the following from the repository root. Set temporary and plotting cache
directories outside the checkout before running commands.

```sh
PYTHONPATH=src python -m pytest \
  tests/field_development/test_module_report.py \
  tests/field_development/test_workflow.py \
  tests/field_development/test_concept_selection.py \
  tests/field_development/test_capex_estimator.py \
  tests/field_development/test_opex_estimator.py \
  tests/field_development/test_economics.py \
  --confcutdir=tests/field_development -q -rA -p no:cacheprovider

PYTHONPATH=src python -m pytest tests/scripts/test_detect_touched_domains.py \
  --confcutdir=tests/scripts -q -rA -p no:cacheprovider

PYTHONPATH=src python docs/reports/modules/fielddevelopment/build_report.py
```

The focused result is 192 passed and 28 optional integration skips, including
all 19 report cases. The routing result is 74 passed. The report tests compare
fresh numerical evidence with relative tolerance 1e-12 and remaining HTML
exactly, check visible values and source digests, and independently discount
cash flows using Decimal arithmetic at all eleven prices. These checks retain
the known production limitation; they do not validate its physical basis.

## Limits and follow-up

Browser rendering, chart interaction and comment-file persistence remain
unverified. External economic backends, fiscal integration, measured-field
validation and full dependency-lock compatibility remain unverified. Cost and
heuristic calibration require independent evidence. The maintainers need to
reconcile the production forecast before interpreting economic value.

No unresolved issue reference is required to reproduce the report. Hosted
checks require separate inspection; local test results do not establish their
status or failure causes.
