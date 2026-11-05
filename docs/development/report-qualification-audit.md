# Report qualification boundary audit

This inventory records a bounded source audit for issue #3764. The first
correction covers the illustrative seakeeping report boundary. Source inspection
does not qualify the other paths listed below. Physical accuracy, native solver
accuracy and convergence remain **NOT_EVALUATED**.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| A | 2026-11-04 | Inventory comparison boundaries and add independent seakeeping regressions before correcting incomplete and mutable output acceptance. |

## Scope and method

The audit starts from the controls in #3763, whose Capytaine builder is absent
from the audited base tree. Its `_validate_response_contract` and `_run_control`
in the proposed `scripts/reports/build_capytaine_module_report.py` require exact
array shapes, finite values, saved coordinates and strict iteration. That
[comparison source](https://github.com/vamseeachanta/digitalmodel/pull/3763)
is evidence of a local implementation, not a dependency introduced here.

The inspected set includes tracked module-report builders, selected numerical
report builders under `scripts/reporting`, and the diffraction and solver
benchmark report boundaries. It is a source inventory, not an exhaustive audit
of all reports, adapters, legacy scripts or tests in the repository. Symbol
names and nearby expressions below identify the inspected evidence without
treating unexecuted checks as numerical acceptance.

## Inventory

| File and symbol | Observed evidence | Disposition |
| --- | --- | --- |
| [`scripts/reports/build_vessel_seakeeping_module_report.py`](../../scripts/reports/build_vessel_seakeeping_module_report.py), `calculate`, `validate_output`, `result_row`, `summary_row` | Before correction, CSV records were accepted without expected row count or coordinate order; summaries retained `result["dofs"]` by reference. Displayed dispositions trusted returned flags. Input data already used a deep copy made before `router`. | Corrected locally: exact scalar-record schemas, complete ordered component × sea-state coverage, finite numeric values, nonnegative amplitudes, matching input criteria/units/weights, consistent screening arithmetic and detached summaries. No independent spectral solve is added. |
| [`docs/reports/modules/gis/build_report.py`](../reports/modules/gis/build_report.py), `calculate` | `zip(latitudes, northings)` constructs the plotted curve without strict cardinality. The mutable latitude list is passed to `transform_points` before constructing the curve. | Follow-up: snapshot expected coordinates before invocation; test missing/surplus ordinates, input mutation, shape and finite-value rejection before correction. No change here. |
| [`docs/reports/modules/riserfatigue/generate_report.py`](../reports/modules/riserfatigue/generate_report.py), `build_payload` | Inputs are copied before `router`. CSV nonfinite values become `None`; bins are appended without comparison with the complete expected segment/bin coordinates. Returned segment records are reused in the payload and chart. | Follow-up: independent expected bin/segment coverage, alias and finite-value regressions; distinguish unavailable values from numerical acceptance. No change here. |
| [`docs/reports/modules/passing_ship/build_worked_example.py`](../reports/modules/passing_ship/build_worked_example.py), `render_example` | Iterates authoritative scalar stations directly; summary text says values agree with a hand polynomial independently of returned values. Physical forces are explicitly NOT_EVALUATED. | Follow-up: inject a failed scalar calculation and derive the displayed agreement from current evidence. No change here. |
| [`src/digitalmodel/hydrodynamics/passing_ship/benchmark_report.py`](../../src/digitalmodel/hydrodynamics/passing_ship/benchmark_report.py), `_TESTS`, `_test_rows`, `generate_benchmark_report` | `_TESTS` contains literal PASS rows; the template also contains static status totals and degree-of-freedom verdicts. `_run_checks` computes evidence separately. | Follow-up: independent failure controls before replacing static verdicts. Existing reference provenance also needs review; no physical qualification is inferred or changed here. |
| [`scripts/reporting/build_generic_riser_preparation_report.py`](../../scripts/reporting/build_generic_riser_preparation_report.py), `read_prepared` | Checks input identities, exact column names/count, case count/set, per-case labels and finite numeric equality against recomputed screens. Rows are addressed by case identity; source order is not the acceptance coordinate. | Existing coverage checks observed. Independent numerical accuracy and mutable-reference injection were not exercised by this audit. Native outputs remain NOT_EVALUATED. |
| [`src/digitalmodel/solvers/benchmark/report.py`](../../src/digitalmodel/solvers/benchmark/report.py), `_entries_equal`, `_row`, `_agreement` | List equality checks lengths before `zip`; report comparison delegates completeness to `compare.fingerprint_status`. `_row` retains a caller-supplied entry mapping. | No list-truncation defect identified in `_entries_equal`. Follow-up: mutation isolation and independent fingerprint-contract audit. Shared comparison enforcement requires separate review. |
| [`src/digitalmodel/hydrodynamics/diffraction/benchmark_matrix_alignment.py`](../../src/digitalmodel/hydrodynamics/diffraction/benchmark_matrix_alignment.py), `_matrix_samples`, `_interpolate_matrix`; [`benchmark_verdict.py`](../../src/digitalmodel/hydrodynamics/diffraction/benchmark_verdict.py), `derive_status` | Checks axis dimensionality, finite/increasing frequencies, collection/matrix counts, exact per-matrix frequency order and matrix shape. Interpolation preserves invalid columns as NaN. Verdicts derive from provenance, usable statistics and consensus. | Existing local safeguards observed. This does not establish complete consumer enforcement, coordinate snapshot independence or physical accuracy. Shared enforcement is unchanged. |

## Independent seakeeping regressions

[`test_seakeeping_report_contract.py`](../../tests/reporting/test_seakeeping_report_contract.py)
supplies two literal components and two literal sea states through a substituted
workflow. Unit amplitudes and known occurrence weights establish the expected
screening arithmetic without calling the spectral engine. Expected inputs are
retained independently before the evaluated method runs.

Controls exercise missing and surplus detailed/summary rows, reordered
coordinates, replaced labels, in-place input-label list mutation, changed units,
criteria and occurrence weights, nonfinite/nested/negative amplitudes, stale
individual and aggregate verdicts, changed requirements, and mutation of returned
summary metadata after calculation. Invalid evidence raises an error before a
report is accepted. Source inputs remain unchanged. Valid failed screening is
retained as a failure, with visible dispositions following the current evidence.

The existing independent hand-calculation fixture continues to check all worked
spectral amplitudes. Boundary validation checks evidence completeness and
internal consistency; a consistently wrong spectral amplitude can still require
an independent numerical reference to detect it.

## Open evidence and limits

The other follow-ups in the inventory remain open. No common validator,
qualification policy or shared enforcement is introduced. No external numerical
criterion is inferred from a public-source citation. No native accuracy,
convergence, measurement comparison, operating-limit approval or physical
engineering release was performed. Those remain explicitly unqualified.
