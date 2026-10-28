# Coupled constant-inflow inventory analysis

The coupled comparison case now stops after at most one roll half-cycle and
rejects planned inflow greater than the initialized tank headroom. This resolves
the default-duration overflow in #1978 while retaining the closed outlet and top
atmospheric vent introduced by #1528.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-19 | Bound constant inflow by duration and initialized headroom to prevent planned overflow. |
| 2 | 2026-10-25 | Preserve exact-capacity acceptance under arithmetic roundoff while retaining overflow rejection. |

## Method and decision

Option 1 of #1978 is selected: keep the constant mean inlet for a bounded
half-cycle comparison. The coupled mapper sets `endTime = min(default endTime,
T/2)`. The ordinary sloshing default remains unchanged. A shorter existing
default is retained, so that case transfers less than the requested half-cycle
volume. No return conduit or reversing inlet is represented.

For an incompressible liquid with a closed liquid outlet, conservation gives:

```text
V_tank = L * W * H
V_initial = L * W * h_initialized
headroom = V_tank - V_initial
Q_mean = V_transfer / (T/2)
V_in = Q_mean * (endTime - startTime)
t_full = startTime + headroom / Q_mean       (Q_mean > 0)
accept when V_in <= headroom
```

The flow-volume relation is `Q = dV/dt`, from
[OpenStax, University Physics Volume 1 (2016), section 14.5, Flow Rate and its Relation to Velocity](https://openstax.org/books/university-physics-volume-1/pages/14-5-fluid-dynamics).
Integrating a constant rate gives `V_in`; solving `V_in = headroom` gives
`t_full`. The half-cycle definition and mean-rate relation are specified in
#1528 (2026). Rectangular capacity follows directly from the input dimensions.
The limit is tank capacity itself, with no empirical allowance or coefficient.

`h_initialized` uses `snap_fill_to_cell_face`, matching the builder's
`setFieldsDict` initialization. Thus an upward mesh snap reduces the headroom
used by the gate. The inventory is assumed to describe the state at the declared
start time; restart-field inventories are not inspected.

Headroom is computed from the fraction of dry vertical cells, avoiding
cancellation in `1 - snapped_fill` near a full tank. The comparison permits
relative roundoff of `8 * math.ulp(1.0)` with zero absolute tolerance. Each volume
path uses at most eight rounded operations; unit roundoff is half a machine
epsilon, so eight epsilons bound the relative disagreement of the two paths.
This is an arithmetic bound, with no empirical increase in capacity. Zero
headroom still rejects every positive inflow. Synthetic regressions cover a
90% fill with 120.0 m3 headroom and 120.0 m3 inflow, a period that also rounds
the inflow upward, and a physical excess larger than the roundoff bound.

The gate reads `volumetricFlowRate` from the actual inlet boundary condition,
using the OpenFOAM v2312
[flowRateInletVelocity contract](https://doc.openfoam.com/2312/tools/processing/boundary-conditions/rtm/derived/inlet/flowRateInletVelocity/).
It rejects negative or nonfinite rates, invalid run times, missing or duplicate
inlet velocity conditions, and rates inconsistent with the planned half-cycle
mean. Unsupported time-dependent inlet functions are `NOT_EVALUATED` and rejected
with a diagnostic. Metadata alone cannot authorize an altered boundary flux.
A zero-flow control has no headroom-exhaustion time (`null` in the manifest).

The mapper applies the inventory gate before returning a case. Call
`verify_coupling` again after changing runtime, geometry, initial fill, or
boundary conditions, and before building or running that modified case. The
paired-tank reduced-order feasibility and conservation checks remain additional
checks; destination-tank free volume is not a substitute for CFD tank headroom.

## Worked example

Inputs are the public regression values in #1978 (2026): `L=20 m`, `W=6 m`,
`H=10 m`, fill fraction `0.5`, `V_transfer=60 m3`, and `T=6.2514 s`.
With the existing vertical mesh, half fill is already on a cell face.
The original ordinary sloshing default is `endTime=50 s`, `startTime=0 s`.

Independent hand calculation using the equations above:

| Quantity | Calculation | Answer |
| --- | --- | --- |
| Capacity | `20 * 6 * 10` | 1200.0 m3 |
| Initial inventory | `0.5 * 1200` | 600.0 m3 |
| Headroom | `1200 - 600` | 600.0 m3 |
| Mean inlet rate | `2 * 60 / 6.2514` | 19.2 m3/s |
| Original planned inflow | `(2 * 60 / 6.2514) * 50` | 959.8 m3 |
| Headroom exhaustion | `600 / (2 * 60 / 6.2514)` | 31.3 s |
| Selected end time | `6.2514 / 2` | 3.1 s |
| Selected planned inflow | `(2 * 60 / 6.2514) * (6.2514 / 2)` | 60.0 m3 |
| Final inventory | `600 + 60` | 660.0 m3 |

Displayed answers are rounded to one decimal place. Tests use the unrounded
hand calculation. The original duration is rejected; the selected duration
passes the headroom check.

The following solver-free example constructs that case and its manifest:

```python
from dataclasses import replace

from digitalmodel.solvers.openfoam.case_coupling import (
    build_case_manifest,
    map_sweep_case_to_openfoam_case,
    synthetic_coupling_spec,
    synthetic_sweep_case,
    verify_coupling,
)

spec = synthetic_coupling_spec()
sweep = replace(
    synthetic_sweep_case(),
    conduit_capacity=0.05,  # #1978: 60 / 1200 m3 per half-cycle.
    roll_period_s=6.2514,  # #1978 reference period, seconds.
    roll_frequency_hz=1.0 / 6.2514,
)
case = map_sweep_case_to_openfoam_case(sweep, spec)
result = verify_coupling(case, sweep, spec)
manifest = build_case_manifest(case, sweep, spec, result)
print(f"end time: {manifest.solver['end_time_s']:.1f} s")
print(f"planned inflow: {result.planned_inflow_m3:.1f} m3")
print(f"headroom: {result.headroom_m3:.1f} m3")
```

Expected output:

```text
end time: 3.1 s
planned inflow: 60.0 m3
headroom: 600.0 m3
```

`tests/solvers/openfoam/test_coupled_inventory.py` pins the independent arithmetic,
the old-duration rejection, zero flow, changed initial fill, mesh snapping,
altered inlet rates, invalid runtime, and the emitted control dictionary and
manifest. Existing coupling and inlet-flux tests cover the retained boundary
conditions and conservation checks.

## Known limits and way forward

This correction provides a constant-inflow half-cycle comparison. It does not
qualify a multi-cycle fill/drain matrix or demonstrate a gravity-driven CFD
exchange. A top vent can still encounter waves; geometric headroom is not a
sloshing run-up clearance criterion. No new CFD results are claimed.

Later work can implement and independently integrate the reversing inlet law,
or model a destination-tank return conduit, then validate flow reversal, phase,
and liquid inventory with CFD histories. The documented phase convention now
correctly identifies the analytical sine pulse as in phase with roll angle;
the emitted inlet remains constant.
