# Undersaturated oil method-selection example

## Summary

The [runnable example](../../../examples/production_engineering/method_selection.py)
uses a published teaching problem for an undersaturated, single-phase oil
interval. It calls the existing selection assessment and linear inflow model,
then prints the selected method, rationale, rate, source locators and open
qualification checks. This is a bounded documentation slice of issue #2010.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-26 | Add an offline selection example with a hand calculation and explicit qualification limits. |
| 2 | 2026-10-26 | Replace invented inputs with a cited teaching problem and show the pressure criterion beside the decision. |

## Published input basis

The source is *Production Engineering II: Inflow Performance Relationship
(IPR)*, 2017-2018 course text, Problem (1-4), printed pp. 40-41
([public university PDF](https://uomosul.edu.iq/public/files/datafolder_2933/_20200307_020617_430.pdf#page=40)).
The pressure inputs and test rate are given in the problem; the productivity
index is calculated in its solution. This is educational example data, with
no field-data provenance established.

| Input (pressures in psig; J in bopd/psi) | Value | Basis |
| --- | --- | --- |
| Reservoir pressure, Pr | 3000 | Problem (1-4), p. 40 |
| Bubble-point pressure, Pb | 2130 | Problem (1-4), p. 40 |
| Minimum flowing bottomhole pressure, Pwf | 2500 | Stabilized test point, p. 40 |
| Productivity index, J | 0.5 | Solution, p. 41: 250 / (3000 - 2500) |

Table 1. Inputs and locators in the published teaching problem.

The source gives rate in stock-tank barrels per day (STB/day); `bopd` here
denotes the same oil volume basis. All pressures share the source's gauge
reference, preserved as psig. The existing API uses `psi` field names; this
linear calculation uses pressure differences, so the common gauge offset
cancels. No absolute-pressure ratio is evaluated.

The [documented selection policy](production-engineering-method-selection.md#ipr-comparison-and-policy)
chooses `linear`: the whole flowing-pressure interval from 2500 to 3000 psig
is above the supplied bubble point of 2130 psig. No multirate test, significant
skin or solution-gas drive is declared; these are example assumptions, not
additional facts established by the source. Only the above-bubble-point
portion of the published problem is exercised; its full IPR is not reproduced.

## Run offline

With the package's existing Python dependencies available, run from the
repository root:

```sh
PYTHONPATH=src python examples/production_engineering/method_selection.py
```

SciPy is required by the existing inflow module at import time. Dependency
installation is separate from example execution; the example reads no input files,
performs no network requests and needs no native solver or external dataset.
Output includes the input citation, pressure values, the criterion
`Pr >= Pwf >= Pb`, productivity index, `Selected method: linear`,
`Flow rate: 250.0 bopd` and `Qualification: NOT_EVALUATED`, followed by the
assessment's source locators and open checks.

For programmatic use, `run_example()` prints the same assessment and returns
an `ExampleResult` containing `reservoir`, `flowing_pressure_psi`, `assessment`
and `flow_rate_bopd`. Importing the script does not run the calculation.

## Hand calculation

The cited course text, *Productivity Index (PI) & Inflow Performance
Relationship (IPR)*, equations (1.1) and (1.4), printed pp. 16-17, defines
`J = q / (Pr - Pwf)` and `q = J * (Pr - Pwf)`. The independent hand
calculation gives drawdown `3000 - 2500 = 500 psi`, productivity index
`J = 250 / 500 = 0.5 bopd/psi` and rate `q = 0.5 * 500 = 250 bopd`.
The example delegates the rate calculation to
`LinearIpr.flow_rate`; it does not implement a correlation. The smoke tests
assert the drawdown, productivity index and rate against hand-worked literals
using exact equality. These values are exactly representable in binary;
no full-precision float-text rebuild comparison is used. Stdout is checked
while network connections are prohibited.

## Qualification limit

The assessment calls the existing selector through `assess_ipr_selection`.
Its `qualification_status` remains `NOT_EVALUATED`: numerical applicability,
parameter fit and field calibration, and original-paper worked-example
validation are open checks even when a method is selected. Reproducing the
published test-point arithmetic does not independently validate the model,
qualify a field productivity index or establish design approval.

This example is limited to its declared undersaturated, single-phase interval.
It supplies no VLP traverse, nodal operating point, literature comparison or
field recommendation. Those broader deliverables remain outside this slice.
