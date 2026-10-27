# One-dimensional pile drivability analysis

## Summary

The scope is a single axial hammer blow on an elastic pile, with a separating
elastic cushion and Smith shaft and toe soil elements. The analysis returns motion,
driving stress histories, an energy ledger, and penetration estimates. Static axial
capacity remains a separate calculation; resistance to driving is an explicit input.
This document records the scope, implementation plan and calculation basis.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-27 | Define the single-blow model and analytical verification basis to extend static pile calculations. |

## Published basis and equations

[FHWA GEC 12, Volume II, Chapter 12, Sections 12.2–12.3](https://www.fhwa.dot.gov/engineering/geotech/pubs/gec12/nhi16009_v2.pdf)
describes the Smith lumped-mass model, separating cushion, elasto-plastic soil
quake, temporary static resistance and velocity-dependent damping. In SI units:

- Rod wave speed: `c = sqrt(E / rho)`; one-way travel time: `T = L / c`.
- Pile spring: `k = E A / length`. A uniform mesh assigns half an element mass
  to each end node, giving half masses at the two ends.
- Equilibrium: `M acceleration + internal_force + soil_force = 0`.
- Shaft static resistance: `Rs = clip((Ru / q) (u - p), -Ru, Ru)`.
  Plastic displacement `p` updates when the elastic displacement reaches quake `q`.
- Toe static resistance: `Rs = clip((Ru / q) (u - p), 0, Ru)`; a gap carries no
  tension, and the toe plastic displacement only increases.
- Smith damping: `Rd = J abs(Rs) velocity`, opposing motion; `J` has units s/m.
  At the toe, limit `Rd >= -Rs` so total contact resistance cannot become tensile
  during rebound. This is an explicit no-suction contact assumption.
- Cushion: `Fc = kc max(u_hammer - u_head, 0)`; no tensile connection.
- Spring stress: `sigma = k (u_upper - u_lower) / A`, compression positive.

Shaft inputs apply at each segment midpoint and distribute equally to its two end
nodes. Toe resistance acts at the final node. General lumped masses and spring
stiffnesses also permit direct transcription of published discretisations.

[Smith, *Pile-Driving Impact*, Figures 4–6, printed pages 47–48](https://vulcanhammer.info/wp-content/uploads/2017/09/smith-impact.pdf)
provides an openly accessible worked numerical example and explicitly recommends
checking kinetic energy, spring energy and external work against initial ram energy.
The transcription uses its stated gravity, weights, spring constants and initial
ram speed. Verification is restricted to the initial incident wave: the example's
later direction-switched toe friction differs from the present quake model.

## Time integration and energy check

Use an implicit midpoint momentum update and displacement-path average spring
forces. Integrate the piecewise linear soil and cushion force exactly along each
step, including yield and gap crossings. For soil damping use the path average of
`abs(Rs)` multiplied by midpoint velocity. This is an energy-consistent numerical
variation of the published physical model, rather than the historical explicit
Smith recurrence. Solve the resulting tridiagonal nonlinear equations to floating
point tolerance; do not adjust an energy component to close the ledger.

The independently calculated ledger is:

`initial energy = kinetic + pile elastic + cushion elastic + soil elastic`
`                 + soil plastic work + damping work + residual`.

Residual kinetic energy includes the rebounding hammer. Soil plastic work is
`sum(Ru abs(delta p))`. Damping work is `sum(Rd v_mid dt)`, with the toe force
limited as above; the shaft contribution is `J mean(abs(Rs)) v_mid**2 dt`.
An energy residual near floating point precision checks bookkeeping and the
discrete solve; it does not establish mesh or time-step accuracy. Resolve each
element transit time with at least ten steps and compare refined solutions.

## Inputs and outputs

Inputs are pile length, area, modulus, mass density and segment count, or explicit
nodal masses and segment stiffnesses, areas and lengths; ram mass and delivered
impact energy; cushion stiffness; one shaft `(Ru, q, J)` per segment and a separate
toe `(Ru, q, J)`; duration, time step and settlement velocity tolerance. All inputs
use kg, m, s, N, Pa and J. Delivered energy already includes hammer efficiency.

Outputs include all sample times, node displacements and velocities, signed stress
at every segment, compressive and tensile stress envelopes, cushion force and all
energy components. These histories, segment locations and repetition counts from
a separate driving sequence are the inputs for a subsequent fatigue cycle count.

Permanent set is reported only after the pile has remained below the specified
velocity tolerance for a full round trip of the pile and the cushion is detached.
An unfinished or zero-resistance blow has no settled set or blow count. A settled
positive set gives `blows/m = 1 / set`; zero forward set gives infinity, which is
an arithmetic result and not a refusal criterion. The separate conventional Smith
estimate subtracts the resistance-weighted quake from maximum toe displacement,
as described in FHWA Section 12.3; it is labelled an estimate, not a settled result.

## Verification plan

| Check | Independent reference | Criterion |
| --- | --- | --- |
| Free rod wave speed and travel time | `sqrt(E/rho)` and `L/c` | Pulse transit converges under mesh refinement. |
| Free rod oscillation | First axial mode `2 L/c` | Frequency and displacement converge to the closed form. |
| Energy per blow | Independent ledger above | Residual bounded by accumulated floating point round-off, including yield and gaps. |
| Zero soil | Momentum conservation and free translation | No fictitious settled set; nonzero residual kinetic energy. |
| Published worked example | Smith Figures 4–6 | Initial ram motion agrees within the stated 5.0% historical numerical accuracy. |
| Settlement and output interpretation | Prolonged damped blow and truncated counterpart | Only the settled result gives a permanent set; reciprocal gives blows/m. |

## Verification results

The 29 drivability checks and 62 existing pile-capacity checks pass together.
The pulse uses a 10.0 m rod, `E = 200 GPa`, `rho = 7850 kg/m3`, a Gaussian
incident displacement pulse and two observation nodes 4.0 m apart. The pulse
width is 0.4 m; the refined mesh has 160 segments. Tests retain unrounded values.

| Check | Reference or criterion | Result |
| --- | --- | --- |
| Pulse wave speed | Closed form 5047.5 m/s | 4998.4 m/s at 80 segments; 5031.4 m/s at 160 segments. |
| Pulse transit over 4.0 m | Closed form 792.5 microseconds | 800.3 then 795.0 microseconds; refined error 0.3%. |
| Free-rod first mode | Closed-form period and initial shape after one cycle | Displacement error below 20.0 nanometres. |
| 5000.0 J blow, no damping | Independent energy ledger at every step | Maximum absolute residual below 1.3e-10 J. |
| 5000.0 J blow, Smith damping | Independent energy ledger at every step | Maximum absolute residual below 1.9e-10 J. |
| Zero soil | Constant total momentum | Relative error below 2.0e-12; settled set unavailable. |
| Published initial ram displacement | Smith Figure 6 at 1.25 and 5.0 milliseconds | Both values within 0.2%; later toe response excluded. |
| Time-step refinement | Decrease from 23.3 to 11.7 to 5.8 microseconds | Compression, tension and estimated-set errors decrease; medium values within 0.1% of fine values. |
| Settlement after 0.6 s | Whole-pile speed below 0.001 m/s throughout the dwell | Set 10.3 mm; 97.2 blows/m. |
| Fast toe rebound | No tensile total contact force | Initial toe elastic energy becomes damping work; energy ledger closes. |

The energy tests include shaft yielding, toe yielding, unloading and cushion
separation. The no-damping case retains residual kinetic and elastic energy;
the energy ledger does not assume that all hammer energy becomes penetration.
The first-mode and pulse checks test numerical propagation independently of a
reported material wave-speed property. The published table was checked visually:
its later ram displacement was misread by automated text extraction.
Source-transcription rule: check numerical table images before accepting
an extracted value as a regression reference.

The cited FHWA and Smith copies returned HTTP 200 on 2026-10-27. Only the
equations and numerical inputs needed for these checks are reproduced here.

## API example

The following synthetic inputs reproduce the settlement check; they are numerical
test inputs, not soil-parameter guidance for a site.

```python
from digitalmodel.geotechnical.pile_drivability import (
    Hammer, SoilResistance, simulate, uniform_pile,
)

pile = uniform_pile(10.0, 0.01, 200e9, 7850.0, segments=6)
result = simulate(
    pile,
    hammer=Hammer(mass_kg=1000.0, energy_j=5000.0,
                  cushion_stiffness_n_m=2e8),
    shaft=[SoilResistance(10000.0, 0.001, 1.0)] * 6,
    toe=SoilResistance(100000.0, 0.002, 1.0),
    time_step_s=pile.recommended_time_step_s / 2,
    duration_s=0.6,
)
# result.permanent_set_m and result.blows_per_m are available after settlement.
# result.stress_pa retains every segment at every time for later cycle counting.
```

`recommended_time_step_s` resolves the pile mesh. The simulator also checks the
hammer/cushion and soil response time scales and rejects a coarser input step.
Histories retain every step, so storage grows with segment count and duration.

## Known limits and way forward

Settlement is a numerical velocity-and-duration criterion. Tighten the velocity
tolerance and extend the record when establishing sensitivity of a reported set.
The model omits gravity during the short blow, a separate helmet, cushion
hysteresis, diesel combustion, residual stresses from previous blows, changes of
embedment, plugging, setup and relaxation. It does not choose soil parameters,
refusal criteria or allowable driving stresses. Resistance to driving must be
calibrated independently; static capacity is not automatically substituted.
The initial part of the published example checks wave transmission and ram/cushion
interaction, not its later toe penetration. A full calibrated field blow benchmark,
soil-parameter guidance, refusal criteria, repetition counts, rainflow counting and
fatigue damage summation remain follow-up work. No industry standard document or
licensed solver is used.
