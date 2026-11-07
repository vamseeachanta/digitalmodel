# Pump-down acoustic transient analysis

The acoustic screen estimates the pressure increment from a pump-rate change.
It supplements the steady conveyance core and the tripping swab/surge screen.
It does not establish an operating window, tool activation margin or transient
bottom-hole pressure. Inputs below are synthetic and representative.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 0 | 2026-11-05 | Added independent acoustic examples and stated model limits. |

Table 1. Initial issue of the acoustic screening example.

## Equations and basis

The [Bureau of Reclamation, Engineering Monograph 3, *Welded Steel Penstocks*](https://www.usbr.gov/tsc/techreferences/hydraulics_lab/pubs/EM/EM03.pdf),
revised 1966, gives the Joukowsky head increment in equation (4), under
“Effect of Water Hammer”, and elastic pipe wave speed in figure 13.
Multiplying the head increment by fluid specific weight gives pressure:

```text
a = sqrt(Kb / [rho (1 + C Kb D / (E t))])
tc = 2 L / a
dv = (Q_initial - Q_final) / Ac
dp = rho a dv attenuation
force_increment = dp Ac
attenuation = 1                    when ramp_time <= tc
attenuation = tc / ramp_time       when ramp_time > tc
abs(rate_step_limit) = dp_budget Ac / (rho a attenuation)
```

Kb is fluid bulk modulus, E pipe Young's modulus, t wall thickness, D internal
diameter and L measured acoustic path length to a reflecting boundary. C is
the pipe-wall compliance multiplier: the figure's expression uses C = 1.0;
the C = 0 rigid-wall limit gives the fluid sound speed. The default C = 1.0
is representative and requires review for the actual support condition.
Fluid properties, wave speed and material stiffness have no defaults.

The ramp attenuation is the requested approximate linear-ramp rule. It is
not the monograph's general gate-closure solution and is not established as
a bound for a real well. No licensed reference values are reproduced.
For example, rho = 1,000 kg/m3, Kb = 2 GPa, E = 200 GPa, D = 0.1 m,
t = 0.005 m and C = 1.0 give a = 1,291.0 m/s. The rigid-wall limit is
1,414.2 m/s. These are synthetic material inputs, not field calibration.

## Pump-rate examples

Use casing ID = 4.778 in, density = 8.34 ppg, a = 4,400 ft/s and
L = 20,000 ft. These task-specified representative inputs require comparison
with measured properties. The critical time is 9.1 s. The bore velocity,
not the restriction bypass velocity, drives this calculation. Per ft/s of
removed velocity, the pressure increment is 59.2 psi (approximately 59.3 psi
at the precision of the issue's reference).

| Rate change (bbl/min) | Ramp time (s) | Removed velocity (ft/s) | Pressure increment (psi) | Force increment (lbf) |
|---|---:|---:|---:|---:|
| 12 to 0 | 0.0 | 9.0 | 534.3 | 9,580.6 |
| 16 to 11 | 3.9 | 3.8 | 222.6 | 3,991.9 |
| 12 to 0 | 18.2 | 9.0 | 267.2 | 4,790.3 |

Table 2. Results recomputed with exact SI conversion constants. The last row
uses exactly twice the critical time before display rounding. The rate cut
is 1.1 % below the issue's approximate 225 psi figure; no coefficient is
adjusted to match that approximation. Force uses casing area, not ring area.

```python
import math
from digitalmodel.units import Q_
from digitalmodel.well.completion.pump_down.transients import (
    maximum_rate_step, rate_step_transient,
)

area = Q_(math.pi * 4.778**2 / 4, "inch**2")
result = rate_step_transient(
    Q_(12, "bbl/min"), Q_(0, "bbl/min"), area,
    Q_(8.34, "ppg"), Q_(4400, "ft/s"), Q_(20000, "ft"),
)
print(result.pressure_change.to("psi"))
limit = maximum_rate_step(
    Q_(100, "psi"), Q_(8.34, "ppg"), Q_(4400, "ft/s"),
    area, Q_(20000, "ft"),
)
print(limit.to("bbl/min"))  # 2.2 bbl/min, rounded for display
```

Physical inputs accept shared Pint quantities or bare SI values. The rate-step
interface accepts nonnegative initial and final rates. A decrease is positive
pressure at a closing downstream boundary; an increase gives its negative
counterpart. This sign does not predict pressure at every point in the well.
The pressure budget is incremental; it does not include steady pressure.

## Tripping interface and limits

`SwabSurge.transient_pressure(initial_trip_speed_ft_min,
final_trip_speed_ft_min, wave_speed_ft_s=..., column_length_ft=...,
ramp_time_s=...)` returns an acoustic magnitude in psi. Signed pipe speeds
preserve reversals: reversing from positive to negative speed changes the
effective fluid velocity by their full difference. The existing displacement
and clinging relation maps pipe speed to fluid speed; its annular acoustic use
requires calibration. Acoustic path length is explicit and independent of TVD.
The method leaves the steady pressure and trip-speed limits intact.

Reflections, tapering, gas, cavitation, damping and formation coupling remain
outside this approximation. No full transient BHP solution or measured
water-hammer validation is established. Acoustic force is not an evaluated
activation or impact load. Pressure envelope, preset safety factor, impact
force, ring bracket and workflow integration remain open. The existing steady
worked example retains the independently solved slip of about 0.53; the
source's adopted 0.08 slip is not used here.
