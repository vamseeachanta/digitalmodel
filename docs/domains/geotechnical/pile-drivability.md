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
| 1 | 2026-10-27 | Correct endpoint rounding, require explicit soil quake and bound remaining motion before reporting permanent set. |

## Published basis and equations

[FHWA GEC 12, 2016 edition, Volume II, Chapter 12, Sections 12.2–12.3](https://www.fhwa.dot.gov/engineering/geotech/pubs/gec12/nhi16009_v2.pdf)
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
Positive shaft or toe resistance requires an explicit positive, finite quake;
no common shaft/toe value is assumed. An absent resistance has no quake and zero
stiffness. Any supplied quake, including for zero resistance, is validated.

Outputs include all sample times, node displacements and velocities, signed stress
at every segment, compressive and tensile stress envelopes, cushion force and all
energy components. These histories, segment locations and repetition counts from
a separate driving sequence are the inputs for a subsequent fatigue cycle count.

Permanent set is reported only after the pile has remained below the specified
velocity tolerance for a full round trip of the pile and the remaining-motion
and permanent-separation criteria below are satisfied.
An unfinished or zero-resistance blow has no settled set or blow count. A settled
positive set gives `blows/m = 1 / set`; zero forward set gives infinity, which is
an arithmetic result and not a refusal criterion. The separate conventional Smith
estimate subtracts the resistance-weighted quake from maximum toe displacement,
as described in FHWA Section 12.3; it is labelled an estimate, not a settled result.

### Numerical settlement criterion

The following energy criterion is a numerical implementation choice derived from
the kinetic, elastic and dissipated-energy ledger above; it is not a published
refusal or allowable-stress criterion. A low instantaneous velocity at an elastic
turning point does not establish permanent settlement.

At the final plastic state, the elastic pile and soil equations are solved for
an equilibrium displacement, including a unilateral toe gap. The equilibrium
soil forces must lie within their supplied resistance limits. A singular,
non-finite or inadmissible equilibrium leaves permanent set unavailable. A
toe-only model uses a uniform equilibrium at the toe plastic displacement and
does not attempt to invert an unsupported translation mode.

The remaining energy is the pile kinetic energy plus elastic energy relative to
that equilibrium. Balanced residual stresses therefore do not count as future
motion. Each bilateral spring contributes `k (delta extension)**2 / 2`.
For the toe, with current extension `e` and reference extension `e_ref`, the
contribution is `k [max(e, 0)**2 - max(e_ref, 0)**2
- 2 max(e_ref, 0) (e - e_ref)] / 2`. This expression includes release into a gap.
These terms follow directly from the elastic energy in the cited lumped-spring
model; their use as a settlement gate is specific to this implementation.

An admissible fixed reference bounds future kinetic energy even if further
plastic flow occurs: reference-force plastic work is included in the relative
energy, while yield dissipation and damping cannot increase it. For numerical
reference forces `r`, the bound additionally includes
`sum(abs(r_i delta_u_i)) + n A sum(r_i**2) / 2`, where `n` is the node count and
`A = min_j[1/k_soil_j + 1/(4 k_pile_j) + sum_(i != j) 1/k_pile_i]` over positive
shaft stiffnesses. This follows from bounding any node displacement by its
midpoint soil displacement and intervening spring extensions using the
Cauchy–Schwarz inequality. The reference displacement error is bounded by
`A sqrt(n sum(r_i**2))`; the yield check includes that error. An unresolved soft
translation therefore cannot pass merely because its force residual is small.
The toe-only uniform reference has zero force residual and needs no correction.

Settlement requires remaining energy no greater than
`min(pile node mass) * settlement_velocity_m_s**2 / 2`, which bounds every future
pile-node speed by the specified tolerance within this model. A hammer may be
excluded only when its cushion is open and its velocity is no greater than
`-sqrt(2 remaining energy / head mass)`, so it cannot catch even the fastest
possible head rebound. The ram can otherwise add energy after apparent separation.

## Verification plan

| Check | Independent reference | Criterion |
| --- | --- | --- |
| Free rod wave speed and travel time | `sqrt(E/rho)` and `L/c` | Pulse transit converges under mesh refinement. |
| Free rod oscillation | First axial mode `2 L/c` | Frequency and displacement converge to the closed form. |
| Energy per blow | Independent ledger above | Residual bounded by accumulated floating point round-off, including yield and gaps. |
| Zero soil | Momentum conservation and free translation | No fictitious settled set; nonzero residual kinetic energy. |
| Published worked example | Smith Figures 4–6 | Initial ram motion agrees within the stated 5.0% historical numerical accuracy. |
| Settlement and output interpretation | Prolonged damped blow and truncated counterpart | Only the settled result gives a permanent set; reciprocal gives blows/m. |
| Rounded endpoint | Free translation at exact and partial step durations | Times increase strictly and the requested endpoint occurs once; invalid increments are rejected. |
| Elastic turning point | Two 1 kg masses, midpoint stiffness 100 N/m | Recoverable energy of 1.00e-4 J exceeds the 5.00e-7 J settlement limit; permanent set stays unavailable. |
| Equilibrated residual stress | Balanced pile, shaft and toe forces | Stored elastic energy alone does not reject equilibrium; toe release and soft translation retain their energy. |
| Quake input basis | Explicit positive quake for positive resistance | Missing, non-finite and nonpositive applicable values are rejected. |

## Verification results

The baseline contains 29 drivability checks and 62 existing pile-capacity checks.
The corrected suite contains 55 drivability checks and the same 62 capacity checks.
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
| Settlement after 0.6 s | Whole-pile speed below 0.001 m/s throughout the dwell, bounded remaining energy and permanently separated ram | Set 10.3 mm; 97.2 blows/m. |
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

Settlement uses a numerical velocity dwell and remaining-energy criterion.
Tighten the velocity tolerance and extend the record when establishing sensitivity
of a reported set. A result can remain unavailable when its equilibrium cannot be
certified, even if recorded velocities are small. The criterion bounds speeds,
not a separate displacement tolerance; a full calibrated blow benchmark remains
outside this verification scope.
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
