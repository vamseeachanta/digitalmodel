# Manifold chemical distribution: worked example and remaining evidence

Refs #2005. The current package already contains the catalog, elementary
structural screens and chemical distribution introduced in #2494. This slice
extends the chemical core with cited arithmetic, independent examples and
checks for numerical range failures. It does not complete the issue's API
qualification or public equipment data requirements.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-18 | Added cited equations, independent worked examples and numerical range checks to document the bounded chemical distribution screen. |
| 1 | 2026-10-18 | Clarified that the shared transition behavior has no verified pressure-drop bound. |
| 2 | 2026-10-25 | Added this revision record and preserved shared-delivery validation messages so upstream input errors retain their explanation. |

## Equations and sources

| Quantity | Equation | Readable basis |
| --- | --- | --- |
| Target chemical rate | `d_i = W_i c_i` | Volume-ratio definition applied to `Q = dV/dt`: [OpenStax, University Physics vol. 1 (2016), sec. 14.5, eq. 14.13](https://openstax.org/books/university-physics-volume-1/pages/14-5-fluid-dynamics) |
| Proposed allocation | `q_i = Q d_i / sum(d)` | Explicit proportional-allocation policy satisfying continuity; [OpenStax (2016), sec. 14.5, eq. 14.14](https://openstax.org/books/university-physics-volume-1/pages/14-5-fluid-dynamics). Continuity does not prescribe this policy. |
| Dose and target ratio | `dose_i = q_i/W_i`, `ratio_i = q_i/d_i` | Volume-ratio definitions using the same flow-rate definition; these are not standard chemical targets. |
| Shared laminar tube loss, used for the independent example | `dp = 128 mu L Q/(pi d^4)` | Rearrangement of [OpenStax (2016), sec. 14.7, eq. 14.19](https://openstax.org/books/university-physics-volume-1/pages/14-7-viscosity-and-turbulence): `8 mu L Q/(pi r^4)` with `r=d/2`; the coefficient is `8*2^4=128`. |
| Common node pressure | `Pnode = Psupply + rho g H - dp_tube` | Hydrostatic head: [OpenStax (2016), sec. 14.1, eq. 14.9](https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure). Pressure balance: [DOE-HDBK-1012/3-92 (June 1992), HT-03, Extended Bernoulli, printed pp. 25–26](https://www.energy.gov/sites/default/files/2026-04/DOE-HDBK-1012-92_VOL3.pdf). |
| Branch loss and added restriction | `loss_i = R_i q_i^2`; `Radd_i = (Pnode-Pback_i-loss_i)/q_i^2` | [DOE-HDBK-1012/3-92 (1992), HT-03, eq. 3-15, printed p. 34](https://www.energy.gov/sites/default/files/2026-04/DOE-HDBK-1012-92_VOL3.pdf): `h_loss=K v^2/(2g)`. Substituting `v=q/A` and `dp=rho*g*h_loss` gives `R=K*rho/(2*A^2)`. The caller supplies and qualifies `R`; no tabulated coefficient is assumed. |
| Branch pressure deficit | `max(Pback_i + loss_i - Pnode, 0)` | Rearranged pressure balance; zero is exact pressure equality, not a safety factor. A deficit gives no feasible added restriction. |

The code consumes `chemical_flow.check_delivery(flow_rate, tube, fluid,
available_pressure, vertical_drop=...)` and its returned `friction_drop`.
It uses `chemical_flow.static_pressure(fluid, vertical_drop)` for the shared
head. It does not reimplement tube hydraulics. `delivery.is_feasible` concerns
the shared line with its default downstream reference pressure; the returned
`hydraulically_feasible` checks the proposed branch margins at the calculated
node pressure. The two properties answer different questions.

## Input domain and interpretation

Use SI units, a common pressure datum, rates at the same volume conditions,
and a common branch elevation. `target_fraction` means chemical volume divided
by water volume, rather than chemical volume divided by the combined stream.
The existing API domain is positive water rate and a target ratio between
zero and unity inclusive. Unity is a software input bound, not a standard
chemical dose. No default chemical specification or acceptance dose exists.

Branches with zero allocation are assumed isolated. Zero chemical supply
therefore yields zero delivered dose and zero dose ratio for a positive target,
while hydraulic feasibility of those isolated branches remains true. A zero
target has an undefined ratio, represented by `None`. Nonzero supply without
any target demand is rejected. The proportional policy can underdose or
overdose; pressure feasibility does not certify that the target dose is met.

Quadratic branch resistance is a caller-qualified operating-point model. DOE's
minor-loss discussion concerns turbulent component losses; it does not justify
applying a constant quadratic coefficient to every laminar branch tube. The
common supply line in the example is laminar; branch coefficients are separate
synthetic inputs. These proposed rates are realised only if pressure margins
and the actual branch restrictions permit them. No branch-network solution is
performed.

## Independent two-well example

All numbers in this example are illustrative inputs, not standard limits,
manufacturer data or calibrated chemical properties. Test expected values
come from the following hand calculation, independently of the implementation.

| Input | First well | Second well |
| --- | ---: | ---: |
| Water rate, m3/s | 0.001 | 0.002 |
| Target chemical/water ratio | 0.001 | 0.001 |
| Backpressure, Pa | 1000 | 2000 |
| Supplied branch resistance, Pa s2/m6 | 1e14 | 2e14 |

The common tube bore is 0.01 m, developed length 10 m, fluid density 1000 kg/m3,
viscosity 0.01 Pa s, supply pressure 10000 Pa and elevation drop exactly zero.
The chemical supply rate is 3e-6 m3/s.

1. Targets are `0.001*0.001=1e-6` and `0.002*0.001=2e-6 m3/s`.
   Their sum equals the chemical supply, so each proposed rate meets its target.
2. From the cited Poiseuille equation,
   `dp = 128*0.01*10*3e-6/(pi*0.01^4) = 1222.309962945756 Pa`.
   Zero elevation eliminates the gravity term. The node pressure is
   `10000 - 1222.309962945756 = 8777.690037054244 Pa`.
3. The independent Reynolds expression in OpenStax (2016), sec. 14.7, eq. 14.20,
   gives `Re = 4*rho*Q/(pi*mu*d) = 38.197186342055`, well inside that text's
   laminar description. The factor four follows `A=pi*d^2/4` and `Re=rho*v*d/mu`.
4. Branch losses are `1e14*(1e-6)^2=100 Pa` and
   `2e14*(2e-6)^2=800 Pa`. Available restriction margins are
   `8777.690037054244-1000-100=7677.690037054244 Pa` and
   `8777.690037054244-2000-800=5977.690037054244 Pa`.
5. Dividing each margin by its squared proposed flow gives additional
   restrictions `7.677690037054244e15` and `1.494422509263561e15 Pa s2/m6`.
   Each dose is 0.001 and each dose ratio is unity.

At half the chemical supply and 1500 Pa supply pressure, the laminar loss
halves to `611.154981472878 Pa`, giving node pressure `888.845018527122 Pa`.
Rates become `0.5e-6` and `1e-6 m3/s`, branch losses become 25 and 200 Pa,
and pressure deficits are `136.154981472878` and `1311.154981472878 Pa`.
The dose ratios are both 0.5; neither added restriction is feasible.

At twice the chemical supply the proportional dose ratios are both two.
The tests increase the illustrative pressure supply for that case and assert
pressure feasibility independently of the explicit overdose.

A runnable example using the public exports is:

```python
from digitalmodel.subsea.manifolds import InjectionDemand, distribute_chemical
from digitalmodel.subsea.umbilicals import ChemicalFluid, UmbilicalTube

# Synthetic inputs documented in the hand calculation above.
result = distribute_chemical(
    3e-6,
    (
        InjectionDemand(0.001, 0.001, 1000, 1e14),
        InjectionDemand(0.002, 0.001, 2000, 2e14),
    ),
    UmbilicalTube(0.01, 10),
    ChemicalFluid(1000, 0.01),
    10000,
    0,
)
assert result.hydraulically_feasible
```

## Numerical range checks

Finite supplied values can still overflow or underflow during arithmetic.
The wrapper checks target sums, static and node pressures, every numerical
shared-delivery result, allocation, branch loss, pressure margin, dose ratios
and added restrictions. A positive demand or active allocation that rounds
to zero is rejected, as is a squared flow that becomes zero or non-finite.
Errors identify the calculation stage and raise `ValueError`; no arbitrary
minimum rate or maximum pressure is introduced. Normal floating-point rounding
and the physical model's applicability remain separate limitations.

The tests use extreme synthetic magnitudes solely to exercise representability;
they are not physical design cases. Relative comparison tolerance is 1e-12,
covering the hand values printed to at least thirteen significant digits, with
zero absolute allowance. These are numerical test allowances, not engineering
acceptance limits.

## OPEN and later slices

- **API 17P qualification: NOT_EVALUATED.** No verified clause text establishes
  a design basis here. Obtain a readable specified edition and verify its
  clauses before implementing standard structural or protection checks.
- **Public equipment records:** the existing source-bearing registry remains
  empty. Complete, independently readable header ratings, dimensions, masses
  and tie-in data are still required; no equipment values are fabricated.
- **Upstream model evidence:** this slice preserves the existing gravity,
  regime thresholds and turbulent correlation. Their full coefficient provenance
  and applicability have not been established here. The independent worked
  example uses zero elevation and laminar flow. Shared transition behavior
  has no verified pressure-drop bound in this slice.
- **Foundation, header and load qualification:** the existing elementary screens
  use supplied capacities; they do not provide verified API wall sizing,
  collapse, settlement, installation, trawl or impact acceptance criteria.
- **Branch-network solution:** pressure-driven distribution and actual restriction
  hardware sizing require subsequent work and qualified branch models.
- **Field wiring and controls:** remain with #2006 and #1995 respectively.
  A transient multiphase network solver remains outside this deterministic core.
