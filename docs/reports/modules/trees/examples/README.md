# Synthetic flow handoff example

This example computes separate production and annulus velocity-screen capacities
and a downstream pressure using declared synthetic inputs. It demonstrates
`bore_capacity` and `flow_handoff`; it is a worked example for the remaining
module-report work in
[the originating issue](https://github.com/vamseeachanta/digitalmodel/issues/3342).

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-25 | Added synthetic arithmetic and executable checks to make the handoff reproducible. |
| 2 | 2026-10-26 | Linked the originating issue to keep the remaining scope traceable. |

## Synthetic inputs

The following values are deliberately chosen arithmetic fixtures. This table is
the source of every dimensional constant in [flow_handoff.py](flow_handoff.py)
and the independent expected values in the [rebuild tests](../../../../../tests/subsea/trees/test_flow_handoff_example.py).
They are not measured equipment data or values taken from a standard.

| Quantity | Symbol | Synthetic value | Unit |
| --- | --- | --- | --- |
| Production bore diameter | d_prod | 0.25 = 1/4 | m |
| Annulus bore diameter | d_ann | 0.125 = 1/8 | m |
| Density, used separately for each port | rho | 400 | kg/m3 |
| Explicit SI velocity coefficient | C | 60 | (m/s) sqrt(kg/m3) |
| Original production operating rate | Q_prod | 0.1 | m3/s |
| Original annulus operating rate | Q_ann | 0.02 | m3/s |
| Upstream pressure | p_up | 2,000,000 | Pa |
| Supplied pressure loss | delta_p | 250,000 | Pa |

The coefficient is synthetic, selected so that its division by the square root
of the synthetic density gives an integer velocity. It is neither a recommended
erosional coefficient nor an oilfield-unit coefficient converted to SI. A design
application requires an independently qualified coefficient and fluid basis.

## Hand calculation

The implemented screen is documented in the
[flow-capacity module](../../../../../src/digitalmodel/subsea/trees/flow_capacity.py):
`v_limit = C / sqrt(rho)`, `Q_limit = A * v_limit`, and
`p_down = p_up - delta_p`. The area is the geometric identity for a circular bore,
`A = pi * d^2 / 4`; the annulus port is represented by its own circular bore,
not by the ring area between concentric pipes.

Substitution from the synthetic-input table gives the following independent
arithmetic. Exact fractions retain precision without presenting rounded values
as boundary criteria.

| Check | Written arithmetic | Expected value |
| --- | --- | --- |
| Both velocity ceilings | 60 / sqrt(400) = 60 / 20 | 3 m/s |
| Production area | pi * (1/4)^2 / 4 | pi/64 m2 |
| Annulus-port area | pi * (1/8)^2 / 4 | pi/256 m2 |
| Production rate ceiling | (pi/64) * 3 | 3*pi/64 m3/s |
| Annulus rate ceiling | (pi/256) * 3 | 3*pi/256 m3/s |
| Downstream pressure | 2,000,000 - 250,000 | 1,750,000 Pa |
| Capacity flag | 0.1 <= 3*pi/64 and 0.02 <= 3*pi/256 | true |

The tests rebuild these values without calling production helpers for expected
results. For each port they also supply its exact floating-point ceiling and
`math.nextafter(ceiling, math.inf)`, the adjacent representable value above it.
Equality passes; the adjacent value fails the capacity criterion. Both original
rates are preserved in the handoff, including an excessive rate, and ports are
never combined or clipped.

## Run and output

From the repository root with `src` on the import path:

```sh
PYTHONPATH=src uv run --no-project python docs/reports/modules/trees/examples/flow_handoff.py
```

The tree-flow modules use the Python standard library; no solver or project
environment installation is required. The command prints strict JSON to stdout
only. It creates no implicit output file. To save the JSON, set `EXAMPLE_OUTPUT`
to an explicitly chosen destination and append `> "$EXAMPLE_OUTPUT"` to the
command. Importing the example performs no calculation and prints nothing.

Output contains `production_capacity` and `annulus_capacity`, each with
`velocity_m_s` and `rate_m3_s`, plus `handoff` with both original rates,
`downstream_pressure_pa` and the boolean `within_capacity`. Keys are sorted,
non-finite numbers are rejected, and identical inputs give identical JSON bytes.
JSON retains floating-point precision for the boundary checks.

Run the example tests together with the existing capacity tests, using a fresh
explicit temporary directory outside the repository:

```sh
PYTHONPATH=src uv run --no-project --with pytest python -m pytest tests/subsea/trees/test_flow_handoff_example.py tests/subsea/trees/test_flow_capacity.py -q -rA -p no:cacheprovider --basetemp="$EXAMPLE_TEST_TMP"
```

## Limits of use and NOT_EVALUATED

`within_capacity` checks only the two supplied rates against the two calculated
velocity-screen ceilings. It does not establish equipment suitability. Pressure
loss is an input, not a predicted result. Each port uses the same synthetic
constant density; no composition transfer or network solution is performed.

NOT_EVALUATED:

- Qualified erosional coefficient selection, sand erosion and corrosion.
- Multiphase slip, gas compressibility, fluid-property changes and transients.
- Valve/choke behaviour, hydraulic pressure-loss prediction and network coupling.
- Connector assessment, pressure-temperature ratings, materials and structural loads.
- Standards qualification or compliance, equipment selection and a complete module report.

A design use requires qualified inputs and separate assessments for these omitted
effects. No production formula changes are part of this example. The connector
and complete module-report scope of
[the originating issue](https://github.com/vamseeachanta/digitalmodel/issues/3342)
remains open.
