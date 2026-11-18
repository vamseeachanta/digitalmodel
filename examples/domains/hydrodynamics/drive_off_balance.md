# Illustrative drive-off force balance

## Summary

The [saved example](drive_off_balance.py) demonstrates a time-domain force
balance for one translational degree of freedom from rest. The numerical basis
is a published textbook example, represented with constant mass and uniform
acceleration. The constant-force solution and algebraic force-cancellation case
check the calculation. This is a mechanics illustration for the time-domain
method; it supplies no qualified drive-off inputs or operating limits.

## Revision history

| Revision | Date       | What changed and why                                                                                                                        |
| -------- | ---------- | ------------------------------------------------------------------------------------------------------------------------------------------- |
| Initial  | 2026-10-27 | Added a synthetic force balance and independent analytical checks to illustrate the time-domain method.                                     |
| 1        | 2026-10-27 | Replaced chosen physical inputs with a cited worked example and updated the independent calculations to make the numerical basis traceable. |

Table 1. Revision history.

## Published numerical basis

The source is OpenStax, _University Physics Volume 1_, first edition (2016),
[section 6.1, Example 6.6](https://openstax.org/books/university-physics-volume-1/pages/6-1-solving-problems-with-newtons-laws).
It calculates average acceleration and forward ground force for a runner from
the following mass, velocity change and elapsed time, neglecting air resistance.
The source is a textbook calculation, not measured drive-off evidence. Its mean
force is held constant here; uniform acceleration is an added idealisation.

| Quantity                   | Value            | Source or derivation                         |
| -------------------------- | ---------------- | -------------------------------------------- |
| Constant mass              | 70.0 kg          | Example 6.6 given mass                       |
| Initial and final velocity | 0.0 and 8.00 m/s | Example 6.6 rest state and final speed       |
| Elapsed time               | 2.50 s           | Example 6.6 given duration                   |
| Mean forward force         | 224 N            | Example 6.6 solution: `70.0 × (8.00 / 2.50)` |
| Opposing force             | 0.0 N            | Example 6.6 neglects air resistance          |

Table 2. Published inputs and the force derived in the same worked example.

Time and displacement origins are zero by coordinate choice. The boundaries
`0.0, 0.25, 1.0, 2.50 s` form a numerical partition of the cited duration;
the interior samples are computational choices, not additional observations.
The API name `thrust_n` denotes the forward force in this illustration; the
source force is ground reaction, not propeller thrust.

The sign convention is `F_net = thrust - drag`. Positive drag denotes a force
along negative x; it is prescribed even at rest. It is not a passive resistance
law. If motion reverses, this example does not reverse the drag automatically.

## Analytical limit

The force balance follows the cited Example 6.6. The kinematic equations and
coefficient `1/2` are given in the same textbook,
[section 3.4, equations 3.12 and 3.13](https://openstax.org/books/university-physics-volume-1/pages/3-4-motion-with-constant-acceleration).
For constant mass and constant net force:

```text
m dv/dt = F_net = T - D
a = F_net / m
v(t) = v_initial + a t
x(t) = x_initial + v_initial t + a t² / 2
```

The coefficient `1/2` follows from integrating `a*t`; it is not an empirical
factor. The hand calculation is `a = (224 - 0)/70 = 3.20 m/s²`. At the cited
`t = 2.50 s`, `v = 3.20 × 2.50 = 8.00 m/s` and
`x = 3.20 × 2.50²/2 = 10.0 m`. Displacement is derived here under uniform
acceleration; it is not a result reported by Example 6.6. These references
are calculated independently of the saved example.

For each interval, the script holds its prescribed force components constant
and advances displacement using the previous velocity before advancing velocity.
This update is exact for that piecewise-constant input idealisation. It does
not establish accuracy for an unknown continuous force history.

Two constructed algebraic checks reuse the cited mass, force and duration.
Applying `224 N` in each direction gives zero acceleration and no motion from
rest. A sign-switch check first applies `224 N` forward for `2.50 s`, reaching
`v = 8.00 m/s` and `x = 10.0 m`. Applying that force in the opposite direction
for a second equal interval gives `a = -224/70 = -3.20 m/s²`,
`v = 8 - 3.20 × 2.50 = 0.0 m/s` and
`x = 10 + 8 × 2.50 - 3.20 × 2.50²/2 = 20.0 m`.
These are cancellation, interval-indexing and state-carry-over checks. Neither
is a published motion history or evidence for an actual shutdown response.

## Run offline

From the repository root, run with the Python standard library only:

```bash
python3 examples/domains/hydrodynamics/drive_off_balance.py
```

The script prints the source basis, SI units, the sampled constant-force solution
and the algebraic cancellation result. It writes no files and starts no external
service.
Independent tests are in
[`test_drive_off_balance_example.py`](../../../tests/hydrodynamics/test_drive_off_balance_example.py).

## Evidence needed for further work

| Input or model              | Where evidence would enter                                                                                                                                                                                            |
| --------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Thrust ramp-up              | Populate interval thrust from a measured or otherwise qualified force-time history; retain its timing, sign, units and uncertainty. A sampled smooth ramp needs time-step convergence checks.                         |
| Drag                        | Replace the prescribed opposing-force array with qualified resistance data or a validated relative-velocity law. State the reference frame, current and direction convention; resolve force changes within each step. |
| Shutdown                    | Place the observed shutdown delay and thrust-decay history on the same time base; resolve the event boundaries explicitly. Zero thrust must not reset velocity or displacement.                                       |
| Mass and initial conditions | Replace textbook inputs with a qualified mass basis and initial-state evidence; assess added inertia separately.                                                                                                      |

Table 3. Evidence required to extend the mechanics illustration to drive-off.

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
