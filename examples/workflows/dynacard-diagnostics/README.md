# Synthetic dynacard example

The generators return downhole pump cards. The workflow constructs a surface
card by marching up the rod string, then runs the default surface-to-downhole
solver before classification. The example checks numerical consistency of that
route; it does not establish diagnostic accuracy on measured cards.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-19 | Document the card boundary, independent static calculation and runnable round trip. |

## Inputs and method

[input.yml](input.yml) supplies the synthetic mode, seed and well geometry.
Its dimensioned inputs are **assumed, based on previous experience**; they are
not equipment ratings or a qualified operating envelope. The retired mode name
`PUMP_TAGGING` resolves to `PUMP_TAGGING_UP`.

The existing spatial recurrence is inverted algebraically, using the same rod
properties, grid and direction-dependent damping as the downward solver.
The surface load includes buoyant rod weight. Surface position is translated
to a zero minimum; that datum shift also translates the recovered pump position
and does not change stroke. Downward output smoothing can change load shape.
A surface card is returned only when its turnaround selects the same damping
switch that constructed it. Invalid arrays, grids, speed, nonfinite displacement
or output, and an unsettled turnaround raise `ValueError` before diagnosis.

## Independent static calculation

For a stationary uniform elastic rod, time derivatives vanish. With pump force
`P`, length `L`, area `A` and modulus `E`, the reduced axial load is constant:

```text
delta = P L / (A E)
surface position = pump position + delta
surface force = P + (rho_rod - rho_fluid) A L g
```

The extension equation is given in [MIT, Trusses (2000), Deflections:
Geometrical approach](https://web.mit.edu/course/3/3.11/www/modules/truss.pdf).
The force equation follows by equilibrium with buoyant rod weight, using the
vertical weight datum documented in the existing solver. These equations
supply the expected result independently of the upward/downward recurrence.

The automated static example uses a stationary pump position at zero, a
1000 lbf pump load, a 100 ft rod and a 1 in diameter. These are mathematical
test inputs. Modulus, rod density, fluid density and gravity use the supplied
context and existing solver constants; no material qualification is implied.
The test evaluates area, elastic extension and buoyant weight directly in SI,
then compares with forward-model output on several grids. With datum zeroing
disabled, surface position equals `delta`; with it enabled, surface position
is zero. In both cases surface load equals pump load plus buoyant weight.
See [the test docstrings](../../../tests/marine_ops/artificial_lift/dynacard/test_synthetic_forward_contract.py)
for the independent calculation and arithmetic tolerances.

## Run the dynamic example

From the repository root, in an environment with the module dependencies:

```sh
PYTHONPATH=src python examples/workflows/dynacard-diagnostics/round_trip.py
```

The script reads the shipped input, disables report-file output and prints the
measured comparison. The retained run gave:

```json
{
  "solver": "everitt_jennings",
  "classification": "PUMP_TAGGING_UP",
  "pump_stroke_in": 117.3,
  "surface_stroke_in": 139.0,
  "recovered_stroke_in": 117.3,
  "load_nrmse_pct": 1.7
}
```

Load nRMSE is the root mean square load error divided by the original load
range, expressed as a percentage. These rounded observations are outputs,
not acceptance limits. Existing tests also exercise every generated mode
through the default solver; the independent static test prevents a shared
forward/inverse implementation error from being the only source of evidence.

## Limits and further work

The calculation checks consistency with the implemented vertical rod model.
It does not validate dimensioned generator envelopes, field classification,
contact friction or deviation corrections. The forward and downward calls
must use matching grids, tubing diameter and viscosity. The workflow does so;
callers of the two functions directly must supply matching settings explicitly.
The downward solver smooths load and closes the periodic record, so exact
sample-by-sample recovery of noisy endpoints is not promised.

The SPE 18189 publisher page returned an access refusal during this change.
No new coefficient or physical acceptance limit was taken from that source;
the dynamic recurrence remains the repository implementation. Field accuracy
and equipment suitability remain NOT_EVALUATED by this example. An independent
measured downhole reference is needed to evaluate them.
