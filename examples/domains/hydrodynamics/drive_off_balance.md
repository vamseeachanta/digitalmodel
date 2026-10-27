# Illustrative drive-off force balance

## Summary

The [saved example](drive_off_balance.py) demonstrates a time-domain force
balance for one translational degree of freedom from rest. It uses a constant
assumed mass and prescribed thrust and drag components. The constant-net-force
solution and equal-force case provide analytical checks of the calculation.
All inputs are synthetic assumptions; no operating or design limit is inferred.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-27 | Added a synthetic force balance and independent analytical checks to illustrate the time-domain method. |

## Synthetic assumptions

This table is the source of the example's numerical inputs. The values are
chosen for hand calculation, with no calibration or claim of representativeness.
SI units apply throughout: mass in kg, force in N, time in s, velocity in m/s,
acceleration in m/s² and displacement in m.

| Quantity | Synthetic assumption | Purpose |
| --- | --- | --- |
| Constant mass | 2000.0 kg | Illustrative inertia, excluding added mass |
| Initial time, displacement and velocity | 0.0 s, 0.0 m, 0.0 m/s | Rest at the coordinate origin |
| Prescribed thrust along positive x | 6000.0 N | Constant-force analytical case |
| Prescribed opposing force | 2000.0 N | Constant drag input, independent of velocity |
| Sample boundaries | 0.0, 0.5, 2.0, 5.0 s | Uneven steps exercise time integration |
| Equal thrust and drag | 4000.0 N each | Zero-balance check from rest |

The sign convention is `F_net = thrust - drag`. Positive drag denotes a force
along negative x; it is prescribed even at rest. It is not a passive resistance
law. If motion reverses, this example does not reverse the drag automatically.

## Analytical limit

For constant mass and constant net force, direct integration gives:

```text
m dv/dt = F_net = T - D
a = F_net / m
v(t) = v_initial + a t
x(t) = x_initial + v_initial t + a t² / 2
```

The coefficient `1/2` follows from integrating `a*t`; it is not an empirical
factor. With the synthetic rest state and forces above, the independent hand
calculation is `a = (6000 - 2000)/2000 = 2.0 m/s²`. At `t = 5.0 s`,
`v = 2*5 = 10.0 m/s` and `x = 2*5²/2 = 25.0 m`. Equal thrust and drag give
zero acceleration, velocity and displacement for the full history from rest.
These references are derived here independently of the saved example.

For each interval, the script holds its prescribed force components constant
and advances displacement using the previous velocity before advancing velocity.
This update is exact for that piecewise-constant input idealisation. It does
not establish accuracy for an unknown continuous force history.

The shutdown test uses a separate synthetic hand calculation: mass `2.0 kg`,
thrust `6.0 N` and opposing force `2.0 N` for `1.0 s` produce `v = 2.0 m/s`
and `x = 1.0 m`. Prescribing zero thrust and the same opposing force for the
next `2.0 s` gives acceleration `-1.0 m/s²`, final velocity `0.0 m/s` and
displacement `1 + 2*2 - 2²/2 = 3.0 m`. This checks interval indexing and state
carry-over; it is not evidence for an actual shutdown response.

## Run offline

From the repository root, run with the Python standard library only:

```bash
python3 examples/domains/hydrodynamics/drive_off_balance.py
```

The script prints assumptions, SI units, the sampled constant-force solution
and the equal-force result. It writes no files and starts no external service.
Independent tests are in
[`test_drive_off_balance_example.py`](../../../tests/hydrodynamics/test_drive_off_balance_example.py).

## Evidence needed for further work

| Input or model | Where evidence would enter |
| --- | --- |
| Thrust ramp-up | Populate interval thrust from a measured or otherwise qualified force-time history; retain its timing, sign, units and uncertainty. A sampled smooth ramp needs time-step convergence checks. |
| Drag | Replace the prescribed opposing-force array with qualified resistance data or a validated relative-velocity law. State the reference frame, current and direction convention; resolve force changes within each step. |
| Shutdown | Place the observed shutdown delay and thrust-decay history on the same time base; resolve the event boundaries explicitly. Zero thrust must not reset velocity or displacement. |
| Mass and initial conditions | Replace synthetic inputs with a qualified mass basis and initial-state evidence; assess added inertia separately. |

The existing propeller-rudder functions describe force models; their focused
tests do not validate this drive-off history. They are not called by this
example, and no propulsion, control or shutdown parameters are inferred from them.

## Known limits and remaining scope

Omitted physics include the other translational and rotational motions, added
mass, radiation memory, velocity-dependent damping, wave, wind and current
loads, coupling to restraints or connected structures, and propulsion dynamics.
There is no controller, event detection, operational envelope or production
workflow. No qualified ramp-up, resistance or shutdown dataset is supplied.

This is a partial method illustration. Further work requires evidence for those
inputs, selection of a suitable coupled model, resolution and uncertainty
studies, and validation against independent time histories before drawing any
operational conclusion.
