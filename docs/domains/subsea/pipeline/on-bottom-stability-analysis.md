# On-bottom stability analysis

## Summary

Method: absolute. Governing case: depth 20, Hs 4.
Synthetic 12 inch OD pipe at three depths and two sea states.
Results are static arithmetic screens. Standard qualification and dynamic displacement: NOT_EVALUATED.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 0 | 2026-10-22 | Calculation issue with explicit input provenance |

## Sea-state inputs and sampled phase

Depth, Hs in m; periods in s; Uw in m/s; phase in radians.

| Depth | Hs | Tp | Uw | Tu | Phase |
|---:|---:|---:|---:|---:|---:|
| 20.0 | 2.0 | 8.0 | 0.3 | 8.0 | 5.8 |
| 20.0 | 4.0 | 12.0 | 1.0 | 10.7 | 6.1 |
| 50.0 | 2.0 | 8.0 | 0.1 | 9.3 | 5.7 |
| 50.0 | 4.0 | 12.0 | 0.4 | 12.2 | 6.0 |
| 100.0 | 2.0 | 8.0 | 0.0 | 10.5 | 5.7 |
| 100.0 | 4.0 | 12.0 | 0.1 | 13.6 | 5.9 |

## Inputs

Forces and weight are N/m; friction and safety factor are dimensionless.

| Case | Weight | Horizontal | Lift | Friction | Passive | Safety factor | Source |
|---|---:|---:|---:|---:|---:|---:|---|
| depth 20, Hs 2 | 1775.9 | 173.9 | 69.1 | 0.6 | 0.0 | 1.5 | assumed, based on previous experience; synthetic input basis |
| depth 20, Hs 4 | 1775.9 | 513.7 | 239.5 | 0.6 | 0.0 | 1.5 | assumed, based on previous experience; synthetic input basis |
| depth 50, Hs 2 | 1775.9 | 76.8 | 34.6 | 0.6 | 0.0 | 1.5 | assumed, based on previous experience; synthetic input basis |
| depth 50, Hs 4 | 1775.9 | 183.0 | 82.6 | 0.6 | 0.0 | 1.5 | assumed, based on previous experience; synthetic input basis |
| depth 100, Hs 2 | 1775.9 | 57.9 | 28.5 | 0.6 | 0.0 | 1.5 | assumed, based on previous experience; synthetic input basis |
| depth 100, Hs 4 | 1775.9 | 89.2 | 41.5 | 0.6 | 0.0 | 1.5 | assumed, based on previous experience; synthetic input basis |

## Utilisation

Ratios display one decimal place; verdicts use unrounded demand <= capacity.

| Case | Lateral | Uplift | Governing | Arithmetic | Standard |
|---|---:|---:|---|---|---|
| depth 20, Hs 2 | 0.3 | 0.1 | lateral | ARITHMETIC_PASS | NOT_EVALUATED |
| depth 20, Hs 4 | 0.9 | 0.2 | lateral | ARITHMETIC_PASS | NOT_EVALUATED |
| depth 50, Hs 2 | 0.1 | 0.0 | lateral | ARITHMETIC_PASS | NOT_EVALUATED |
| depth 50, Hs 4 | 0.3 | 0.1 | lateral | ARITHMETIC_PASS | NOT_EVALUATED |
| depth 100, Hs 2 | 0.1 | 0.0 | lateral | ARITHMETIC_PASS | NOT_EVALUATED |
| depth 100, Hs 4 | 0.2 | 0.0 | lateral | ARITHMETIC_PASS | NOT_EVALUATED |

## Method and limits

Equations A1–A4: module on-bottom-stability-analysis.md, independent force balance.
Absolute uses gamma*(abs(H)+mu*L)/(mu*W), without passive credit.
Passive balance uses gamma*abs(H)/(mu*(W-L)+FR); simplified sets FR=0.
Uplift uses gamma*L/W and bed contact requires W>L.
Standard editions, clauses, safety-class factors and calibrated soil resistance are not qualified.
No generalised displacement envelope or dynamic time history is calculated.
Use reviewed factors and an applicable soil/dynamic assessment before design acceptance.

## Fixed-phase design-weight comparison

Required weight is the fixed-force A1 root in N/m. Change compares A3/A1 utilisations.

| Case | Required weight | Absolute | Simplified | Change (%) |
|---|---:|---:|---:|---:|
| depth 20, Hs 2 | 538.6 | 0.3 | 0.3 | -16.0 |
| depth 20, Hs 4 | 1643.4 | 0.9 | 0.8 | -9.7 |
| depth 50, Hs 2 | 244.0 | 0.1 | 0.1 | -19.7 |
| depth 50, Hs 4 | 581.4 | 0.3 | 0.3 | -17.5 |
| depth 100, Hs 2 | 187.5 | 0.1 | 0.1 | -21.5 |
| depth 100, Hs 4 | 285.2 | 0.2 | 0.1 | -20.0 |

## Synthetic input basis

This is a synthetic 12 inch **outside-diameter** pipeline, rather than a nominal
pipe-size specification. All geometry, material properties, sea states, current,
soil friction and factors are **assumed, based on previous experience**. No real
asset or previous design is represented. These values are not sourced standard
recommendations. Lengths are specified directly in metres.

| Input | Value | Basis |
|---|---:|---|
| Steel OD / wall (mm) | 304.8 / 12.7 | Assumed geometry |
| Corrosion coating / concrete / marine growth (mm) | 3.0 / 50.0 / 10.0 | Assumed layers |
| Steel / corrosion coating density (kg/m³) | 7850.0 / 950.0 | Assumed materials |
| Concrete / growth density (kg/m³) | 3000.0 / 1300.0 | Assumed materials |
| Contents / seawater density (kg/m³) | 800.0 / 1025.0 | Assumed fluids |
| Corrosion allowance / absorption / joint fraction / extra mass | 0.0 | Assumed absent |
| Current at pipe (m/s) | 0.5 | Assumed perpendicular current, no profile reduction |
| Drag / inertia / lift coefficient | 1.0 / 2.0 / 0.5 | Assumed coefficients; no table qualification |
| Friction / safety multiplier | 0.6 / 1.5 | Assumed inputs; no safety-class assignment |
| Passive resistance (N/m) | 0.0 | No passive credit |
| Wave / calibration multipliers | 1.0 / 1.0 | Explicitly no empirical reduction |
| Water depths (m) | 20.0 / 50.0 / 100.0 | Assumed flat seabed at each depth |
| Sea states Hs (m), Tp (s) | 2.0, 8.0 and 4.0, 12.0 | Assumed wave conditions |

Gravity uses the existing spectral helper's fixed conventional value (recorded
in the code and knowledge pack); it is a numerical convention, not an F109
coefficient. The pipe centre is half the grown diameter above the bed.
The existing spectral helper returns significant orbital velocity Uw and
zero-crossing period Tu. This example uses Uw as the deterministic cosine-wave
amplitude in the E305 arithmetic screen. That conversion is an explicit example
convention, **not** a qualified design-wave or return-period transformation.
Period belongs to each sea state separately. The maximum simultaneous absolute
weight demand is sampled over 720 equally spaced phases. The independent
half-step sampling test bounds its numerical error for these examples only.

## Equations and evidence

The following are independent equilibrium identities. Legacy code attributes
similar expressions to F109, but the claimed edition and clauses have not been
verified against an openly readable standard. These equations therefore do not
establish standard compliance.

- **A1 — absolute force screen:** demand = gamma*(abs(H)+mu*L), capacity = mu*W;
  utilisation is demand/capacity. Passive resistance is excluded. This is the
  rearranged friction balance with the selected multiplier applied to both
  horizontal demand and the friction loss associated with lift.
- **A2 — supplied-passive balance:** demand = gamma*abs(H), capacity =
  mu*(W-L)+FR. FR is a supplied resistance in N/m; no passive-soil equation is
  implemented or validated here. The different placement of gamma is deliberate.
- **A3 — simplified screen:** A2 with FR=0. The historical function named
  generalized computes A2, which is a static force ratio, not the standard
  generalised displacement envelope or a dynamic method.
- **A4 — uplift and contact:** uplift utilisation = gamma*L/W; require W>L for
  bed contact. An arithmetic pass means unrounded demand <= capacity and
  positive contact. Unity is this ratio definition, not an empirical factor.
- **A5 — required fixed-geometry weight:** solve A1–A4 in a supplied bracket
  by bisection, keeping the feasible endpoint. For mu>0 the A1 closed form is
  W_required=max(gamma*(abs(H)/mu+L), gamma*L); A2 replaces its first term with
  (gamma*abs(H)-FR)/mu+L; A3 sets FR=0. Contact is checked independently.
  Coating design must rebuild diameter, buoyancy and hydrodynamics; this solver
  does not convert additional submerged weight into a concrete thickness.
- **Hydrodynamics:** FD=rho*CD*D*U*abs(U)/2;
  FI=rho*CM*pi*D²*a/4; FL=rho*CL*D*U²/2. Drag and inertia retain direction;
  lift is defined by the supplied coefficient. For the deterministic screen,
  U=Uc+Uw*cos(theta) and a=-Uw*(2*pi/Tu)*sin(theta). Forces are evaluated at the
  same phase; independent maxima of drag, inertia and lift are never added.
- **Weight:** concentric annulus area=pi*(OD²-ID²)/4;
  W=g*(sum(component masses)-rho_water*displaced area). Growth contributes both
  mass and displaced volume; its outer diameter also sets hydrodynamic demand.

Independent hand cases in the tests give FD=1600.0 N/m, FI=30*pi N/m and
FL=400.0 N/m for the stated synthetic input ledger. A separate layered pipe
gives W=153*pi/16 N/m at supplied gravity. The static case W=200.0, H=40.0,
L=10.0, mu=0.5, FR=10.0 and gamma=2.0 gives design roots 180.0 N/m (A1),
150.0 N/m (A2) and 170.0 N/m (A3). Every expected value is derived in the test
docstring or independent ledger, without reading implementation constants.

## Comparison and interpretation

At a fixed simultaneous force state with positive contact and L>0, A3 is less
conservative than A1 when abs(H)<mu*(W-L); it is more conservative above that
threshold. This follows directly by comparing the two positive ratios. The
independent test uses threshold 95.0 N/m and horizontal forces 40.0 and
100.0 N/m on opposite sides. Neither expression consistently bounds the other.
Comparisons below use the **same phase**, chosen to maximize A1 demand; they
do not claim that this phase also maximizes A2 or A3.

## Source register

| Source | Edition / section | Evidence used and limit |
|---|---|---|
| [Public Morison theory](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Morison%27sequation.htm) | Undated theory page, fixed-body equation; original equation 1950 | Drag and inertia algebra only; no coefficient calibration |
| [University Physics Volume 1, buoyancy](https://openstax.org/books/university-physics-volume-1/pages/14-4-archimedes-principle-and-buoyancy) | 2016, §14.4, Archimedes' Principle and Buoyancy | Displaced-fluid buoyancy; annular geometry derived here |
| [University Physics Volume 1, friction](https://openstax.org/books/university-physics-volume-1/pages/6-2-friction) | 2016, §6.2, Eq 6.1 | Static friction bound mu*N; N=W-L and the chosen demand factors are applied explicitly here, without standard calibration |
| [F109 publisher metadata](https://www.dnv.com/energy/standards-guidelines/dnv-rp-f109-on-bottom-stability-design-of-submarine-pipelines/) | 2021-05, amended 2025-09 | Edition metadata only; clauses and factor tables not exposed |
| [Function claim inventory](../../../../src/digitalmodel/subsea/on_bottom_stability/docs/validation-claims.md) | Current API register; legacy code claims October 2021 | Every public function, equation, claimed clause and qualification status |
| [Knowledge pack](../../../../src/digitalmodel/subsea/on_bottom_stability/docs/agent-pack.md) | Historical pinned inventory with current amendment | Existing spectral-helper and E305 conventions; standard equivalence unresolved |

The four public pages were retrieved on 2026-10-22 with successful HTTP
responses. The originating issue names F109 2007; the code claims October 2021;
the publisher metadata differs. No edition has been silently substituted. No
licensed tables or private spreadsheets were read or reproduced. All load
coefficients, safety multipliers, friction and passive resistance must be supplied
explicitly to the scalar calculation APIs. Exported legacy constants remain
unqualified compatibility symbols and are not automatically consumed.

## Required follow-up and limits of validity

F109 edition/clauses, safety-class factors, standard absolute-method calibration,
generalised displacement envelopes, E305 calibration and full dynamic analysis
are **NOT_EVALUATED**. A positive arithmetic screen is not design acceptance.
Supply a reviewed, applicable edition and source-qualified inputs, reconcile its
actual load and resistance factors, and perform the required displacement and
soil assessments before accepting a design. Failed arithmetic cases require
additional submerged weight, lower environmental demand or a reviewed restraint
design; the reported fixed-force weight is an initial sizing result only.

Scour, embedment evolution, cyclic soil response, sloping beds, trenching,
oblique flow, nonuniform growth, installation transients and wave nonlinearity
are outside this screen. Depth and wave sensitivity here use the existing
linear spectral helper, whose edition-specific calibration is not independently
qualified by this work. A sampled phase grid is not an exact continuous maximum.

## Reproduction

The report is the exact text returned by
`digitalmodel.subsea.on_bottom_stability.example.example_report()`.
The reproducibility test compares that text with this committed file and checks
the subsea documentation link. Report verdicts and governing-case selection use
full precision; displayed values and percentages use one decimal place.
