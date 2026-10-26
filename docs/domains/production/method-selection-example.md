# Undersaturated oil method-selection example

## Summary

The [runnable example](../../../examples/production_engineering/method_selection.py)
uses synthetic inputs for an explicitly undersaturated, single-phase oil
interval. It calls the existing selection assessment and linear inflow model,
then prints the selected method, rationale, rate, source locators and open
qualification checks. This is a bounded documentation slice of issue #2010.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-26 | Add an offline selection example with a hand calculation and explicit qualification limits. |

## Synthetic inputs

All values below are illustrative inputs defined for this example, with no
measured-data basis or inferred operating limits. Oil remains single phase
over the entire requested pressure interval; no multirate test, significant
skin or solution-gas drive is declared.

| Input | Value | Basis |
| --- | --- | --- |
| Reservoir pressure, Pr | 3000 psi | Synthetic input defined here |
| Bubble-point pressure, Pb | 2000 psi | Synthetic input defined here |
| Minimum flowing bottomhole pressure, Pwf | 2500 psi | Synthetic input defined here |
| Productivity index, J | 2 bopd/psi | Synthetic input defined here |

The [documented selection policy](production-engineering-method-selection.md#ipr-comparison-and-policy)
chooses `linear`: the whole flowing-pressure interval from 2500 to 3000 psi
is above the supplied bubble point of 2000 psi, and no multirate data override
is requested. These inequalities declare the example's scope; they do not
establish single-phase behavior for measured fluids.

## Run offline

With the package's existing Python dependencies available, run from the
repository root:

```sh
PYTHONPATH=src python examples/production_engineering/method_selection.py
```

SciPy is required by the existing inflow module at import time. Dependency
installation is separate from example execution; the example reads no input files,
performs no network requests and needs no native solver or external dataset.
Output includes `Selected method: linear`, `Flow rate: 1000.0 bopd` and
`Qualification: NOT_EVALUATED`, followed by the assessment's source locators
and open checks.

For programmatic use, `run_example()` prints the same assessment and returns
an `ExampleResult` containing `reservoir`, `flowing_pressure_psi`, `assessment`
and `flow_rate_bopd`. Importing the script does not run the calculation.

## Hand calculation

The [repository equation convention](production-engineering-literature.md#parameter-and-equation-convention)
defines linear inflow as `q = J * (Pr - Pwf)`. For the synthetic inputs above,
drawdown is `3000 - 2500 = 500 psi`, giving
`q = 2 * 500 = 1000 bopd`. The example delegates this calculation to
`LinearIpr.flow_rate`; it does not implement a correlation. The smoke tests
compare the returned values with this independent hand product using exact
equality, and check stdout while network connections are prohibited.

## Qualification limit

The assessment calls the existing selector through `assess_ipr_selection`.
Its `qualification_status` remains `NOT_EVALUATED`: numerical applicability,
parameter fit and field calibration, and original-paper worked-example
validation are open checks even when a method is selected. Synthetic arithmetic
does not qualify the productivity index or establish design approval.

This example is limited to its declared undersaturated, single-phase interval.
It supplies no VLP traverse, nodal operating point, literature comparison or
field recommendation. Those broader deliverables remain outside this slice.
