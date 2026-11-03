# Supported surface pull-in example

This offline example applies the existing quasi-static, continuously supported
surface method to synthetic straight contact pieces. It prints normal load,
friction, signed gravity, segment entry/exit tensions, peak pipe tension and
winch demand in kN. Capacity status compares only with a caller-declared winch
capacity; it is not pipe-strength or installation qualification.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-26 | Add a runnable synthetic example with an independent free-body balance. |

## Usage

Run from the repository root in an environment with the package dependencies:

```sh
PYTHONPATH=src uv run python examples/domains/installation/shore_approach_pull_in.py
PYTHONPATH=src uv run python examples/domains/installation/shore_approach_pull_in.py --capacity-kn 100
```

The first command leaves capacity unevaluated. The second declares a synthetic
100 kN winch capacity. Options `--friction`, `--net-weight-kn-m`,
`--tail-tension-kn` and `--efficiency` replace the demonstration inputs.
Execution reads no route data, contacts no service and writes no output files.
It uses `shore_approach.pull_in` directly; the existing model-preparation script
serves a separate purpose and is not required.

## Synthetic inputs

All values below are deliberately chosen arithmetic examples, not measured data,
recommended design inputs or standards-derived limits. This section is the
source of the numeric inputs in the script and its regression tests.

In tail-to-winch order, the actual supported lengths are 20 m, 50 m and 50 m;
their signed elevation changes are 0 m, +30 m and -30 m. Each inclined contact
piece has a 40 m horizontal projection because 30² + 40² = 50².
Net downward line weight is 2 kN/m, equivalent axial friction is 0.25,
tail tension is 10 kN, and mechanical efficiency is 0.8 (dimensionless).
No additional buoyancy aids are applied: the supplied weight is already net.
Piece junctions only transfer axial tension; no bend resistance is represented.

## Hand calculation

For a straight piece with supported length L [m], horizontal projection x [m],
rise h [m] and net line weight w [kN/m], the vertical resultant is W = w L [kN].
Resolve this force normal and parallel to the contact surface using the right
triangle: N = W x/L [kN] and G = W h/L [kN]. Friction opposing forward sliding
is F = mu N [kN], with mu dimensionless. Tangential equilibrium is
T_out - T_in - F - G = 0, hence T_out = T_in + F + G [kN].
Positive G resists uphill motion; negative G assists downhill motion.

For either inclined piece W = 2 × 50 = 100 kN, so N = 100 × 40/50 = 80 kN,
F = 0.25 × 80 = 20 kN and G = ±100 × 30/50 = ±60 kN.
The level piece has N = 2 × 20 = 40 kN, F = 10 kN and G = 0 kN.

| Piece | N [kN] | F [kN] | G [kN] | T_in [kN] | T_out [kN] |
| --- | --- | --- | --- | --- | --- |
| Level | 40.0 | 10.0 | 0.0 | 10.0 | 20.0 |
| Uphill | 80.0 | 20.0 | 60.0 | 20.0 | 100.0 |
| Downhill | 80.0 | 20.0 | -60.0 | 100.0 | 60.0 |

Total friction is 50.0 kN and total signed gravity is 0.0 kN. Head tension is
60.0 kN; peak tension is 100.0 kN at the uphill endpoint. Tension varies linearly
within each uniform piece, so the peak includes all endpoints and the tail.
Winch demand = head tension / efficiency = 60/0.8 = 75.0 kN.
Efficiency changes winch demand, not the pipe endpoint force balance.

## Capacity status

Utilisation = winch demand / declared winch capacity, on the same demand basis.
At 100 kN it is 75.0 %, with status `within_declared_capacity`; at 75 kN it is
100.0 %, also within; at 60 kN it is 125.0 %, with status `exceeded`.
Omitting capacity returns `not_evaluated` and no numeric utilisation.
These are arithmetic comparisons with declared inputs, without design factors.

## Alternate inputs and compression refusal

The regression's alternate inputs are w = 1 kN/m, mu = 0.5, tail = 20 kN,
efficiency = 0.5 and capacity = 200 kN. Normal loads are 20, 40, 40 kN;
friction forces are 10, 20, 20 kN; gravity forces are 0, +30, -30 kN.
Endpoint tensions are 30, 80, 70 kN; peak tension is 80.0 kN;
winch demand is 70/0.5 = 140.0 kN and utilisation is 70.0 %.

For the default downhill piece alone and a 10 kN tail tension,
T_out = 10 + 20 - 60 = -30 kN. Call `run_example` with that piece to reproduce
the refusal: the tension-only method raises `ValueError` before printing results.
Compression cannot be clipped to zero or presented as a feasible pull-in.

## Limits and way forward

HDD, bends, breakout and dynamic loads are unevaluated, as are trenching,
unsupported spans, pipe stress, contact loss and flotation stability.
The example does not qualify a route or select an installation method.
For engineering use, supply justified site inputs, assess those remaining
effects separately and compare demand with capacity on a consistent basis.
