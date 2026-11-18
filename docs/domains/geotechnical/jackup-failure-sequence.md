# Generic jack-up failure-sequence analysis

The generic screen ranks independent failure thresholds during preload and a
storm, for a three-leg independent-leg jack-up in two hull states. For the
assumed base stiff-over-soft profile, punch-through governs preload; chord
capacity governs the elevated storm case at a 10.0 m air gap, and overturning
governs the in-water storm case. These results describe the stated assumptions.
The way forward is to replace the soil, member, holding-system and environmental
assumptions with investigated inputs and assess the coupled transient response.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-28 | Added reusable threshold ranking, two hull states, strength sensitivities and an air-gap sweep to identify which assumed limits are reached first. |
| 1 | 2026-10-28 | Limited rotating-hull buoyancy reserve by deck-edge immersion and added independent clearance and moment checks to prevent a false balance. |

## Inputs and source basis

No measured regional soil profile or unit-specific geometry is represented.
All numerical scenario values and their ranges are **assumed, based on previous
experience**. They illustrate offshore layered-seabed mechanisms without claiming
a verified regional distribution. The three step-1 profiles are reused: stiff
over soft clay with deeper strong clay, sand over soft clay, and uniform soft
clay. A separate named sensitivity set was not present in the source branch.
Each topology has low, base and high strengths, giving nine soil cases. Low and
high use the declared strength endpoints in the step-1 inputs; layer geometry
and unit weights remain fixed. Sand friction angle also varies with strength.

The complete [input record](../../../examples/data/jackup_failure_sequence/inputs.json)
contains every numerical parameter, unit, assumption range, soil layer and
method reference. Member dimensions, elastic modulus, yield strength, holding
capacity, friction and ballast capacity have assumption ranges, rather than
claims of published equipment ratings. Representative member bounds are:

| Property | Base assumption | Assumed range |
| --- | --- | --- |
| Chord outside diameter (m) | 0.6 | 0.5–0.9 |
| Chord wall thickness (m) | 0.025 | 0.020–0.050 |
| Chord effective buckling length (m) | 12.0 | 6.0–18.0 |
| Brace outside diameter (m) | 0.25 | 0.20–0.40 |
| Brace wall thickness (m) | 0.018 | 0.012–0.030 |
| Brace effective buckling length (m) | 7.0 | 5.0–12.0 |
| Elastic modulus (GPa) | 210.0 | 200.0–215.0 |
| Yield strength (MPa) | 355.0 | 235.0–460.0 |
| Per-leg holding capacity (MN) | 35.0 | 25.0–60.0 |

These are unqualified generic properties. No public member-capacity benchmark
or equipment rating was adopted. The equations for tubular area, second moment,
yield and elastic buckling are checked by independent calculations in the tests.
The elastic buckling result follows from the pin-ended eigenproblem `EI v'' + P v = 0`;
effective length includes the assumed end restraint. These equations do not
replace a structural code check.

The soil methods retain the public sources documented in the
[layered penetration analysis](jackup-layered-penetration.md):

- [Spudcan Penetration Analyses for Three Case Histories in Sand Overlying Clay (2015)](https://jack-up.citystgeorges.ac.uk/wp-content/uploads/2025/09/Jackup-2015-11.pdf), equations 1–3, for the inherited sand methods.
- [Theoretical Study on the Effect of a Weak Layer on the Vertical Bearing Capacity of Spudcans in Clay (2015)](https://jack-up.citystgeorges.ac.uk/wp-content/uploads/2025/09/Jackup-2015-19.pdf), equations 2 and 6, for squeezing and punching.

Both public copies returned HTTP 200 on 2026-10-28. The source responses and
retrieval records were retained with the calculation evidence. No regional
measurements were adopted from those papers. The inherited circular clay factor
is retained as described in step 1.

SNAME T&RB 5-5A, **Rev 3 (2008), clause: TO CONFIRM**, and ISO 19905-1,
**2012 edition, clause: TO CONFIRM**, are attached to every capacity-basis record.
The intended clauses and applicability remain unconfirmed; no standard
conformity is claimed. No licensed table, rating or complete section is copied.

Air gap, minimum draft, sea state, tide, ballast and single- or all-leg preload
are unknown inputs. A 2026 public industry article on preload draft was described
in the supplied scope, but no public copy or address was supplied or retrieved.
No event-specific fact or numerical value is attributed to that article.

## Load paths and capacities

The static resultants and signed reactions come from
[jackup_screening.py](../../../src/digitalmodel/marine_ops/jackup_screening.py).
That module supplies equilibrium and illustrative environmental drag, not
structural capacities. The new example adds the explicit capacity assumptions.

At load factor zero the 36.0 MN dry weight remains. During preload, each loaded
leg receives an additional 18.0 MN per factor: all-leg preload adds 54.0 MN of
ballast at factor one; single-leg preload adds 18.0 MN with its eccentric gravity
moment. Storm weight stays fixed. Environmental force and moment resultants and
the assumed wave crest scale linearly with factor. This synthetic action path
is not a sea-state scaling law or a return-period calculation. Preload and storm
use separate wave, period, wind and current assumptions, all with ranges.

| Mode | Demand and capacity basis |
| --- | --- |
| Spudcan punch-through | Maximum compressive reaction divided by the first descending peak of the step-1 curve. Absent for a profile without a descending branch. |
| Leg penetration | Maximum reaction divided by the maximum resistance within the investigated tip interval. This is profile exhaustion, distinct from the first capacity loss. |
| Chord buckling | Maximum chord compression from axial load and truss bending divided by `min(A Fy, pi² E I / Le²)`. Three chords lie at transverse coordinates `(r, -r/2, -r/2)`; the largest compression increment is `2M/(3r)`. |
| Brace buckling | Panel shear divided between two braces, `Hleg / (2 sin(angle))`, compared with the same yield/elastic buckling minimum for the brace. |
| Leg-hull holding | Maximum absolute signed leg reaction over both swell signs divided by the assumed 35.0 MN per-leg pinion or fixation equivalent capacity. |
| Overturning | External moment divided by the directional support-polygon restoring moment, using minimum effective weight. No footing moment credit is taken. |
| Sliding | Horizontal force divided by assumed friction coefficient times minimum effective weight. Combined vertical-horizontal-moment interaction is omitted. |
| Air gap | Tide plus assumed scaled `Hs/2` crest divided by underside elevation. This check applies to the elevated hull. It is not an extreme-crest criterion. |
| Hull swell | Resultant of hull drag and the assumed vertical proxy `gamma Aeffective Hs/2`, compared with a separately assumed hull limit. Applies in water. |
| Ballast reserve | Actual preload ballast versus assumed ballast capacity. A separate wet-preload reserve check includes ballast equivalent to initial buoyancy to match dry-hull reactions. |

The reference air and water densities and gravity are inherited from the public
reference constants cited in `jackup_screening.py`. Equal horizontal sharing
between legs, a mudline cantilever and adverse environmental heading are
assumptions. Structural capacity does not equal preload history.

## Two hull states and punch-through consequences

The in-water hull starts at an assumed minimum draft of 1.0 m, with range
0.5–2.0 m. Initial buoyancy is 22.1 MN. The same ballast produces lower leg
reactions in water than when elevated. To match the dry-hull reactions during
all-leg preload at factor one requires 76.1 MN total ballast, including 22.1 MN
of buoyancy-equivalent ballast. The load path itself uses 54.0 MN; the extra
reserve is reported and checked separately. Raising the hull with ballast
unchanged transfers the lost buoyant support to the legs.

Hull drag uses beam times draft and the inherited surface-velocity proxy. The
vertical swell proxy acts through the hull centre; both signs are checked for
holding load, while the downward sign increases compression and the upward
sign reduces restoring weight. The force grid stops before zero total effective
weight, where this compression-only static representation ceases to apply.

A hypothetical single-leg run rotates the hull about the other two supports.
For equilateral support radius `R`, the failed-leg lever is `1.5R`, while the
hull-centre buoyancy lever is `0.5R`. A footing reaction loss `dQ` therefore needs
`3dQ` extra buoyancy for moment equilibrium. Centre sinkage is leg travel divided
by three, giving required leg travel `9dQ/(gamma Awp)`. For hull length `L`, the
descending edge has lever `(L+R)/2`; the centre has lever `R/2`. The freeboard
criterion therefore limits centre sinkage to `(hull depth - draft) R/(L+R)`
and reserve to `gamma Awp (hull depth - draft) R/(L+R)`. These relations combine
moment equilibrium and displaced-water weight, as described in OpenStax,
*University Physics Volume 1* (2016), sections
[12.1, Conditions for Static Equilibrium](https://openstax.org/books/university-physics-volume-1/pages/12-1-conditions-for-static-equilibrium)
and [14.4, Archimedes' Principle and Buoyancy](https://openstax.org/books/university-physics-volume-1/pages/14-4-archimedes-principle-and-buoyancy).
The constant waterplane and centre of buoyancy are screening assumptions.
For the base geometry, the 6.0 m freeboard permits 4.5 m failed-leg travel and
1.5 m centre sinkage, giving 33.2 MN reserve before edge immersion. A synthetic
20.0 MN reaction loss requires 60.0 MN additional buoyancy and 8.1 m travel;
the resulting 10.8 m edge descent exceeds the 6.0 m freeboard. That balance
is rejected. When neither buoyancy balance nor static soil recovery is
established, travel remains a lower bound. This quasi-static hypothesis omits
kinetic energy, hydrodynamic inertia and flooding; exact rotating hydrostatics
and dynamic arrest are not established.

For the base stiff-over-soft case, the first reaction loss is 4.4 MN and the
static soil recovery travel is 5.4 m. The in-water buoyancy balance needs 13.2 MN
extra buoyancy and 1.8 m leg travel, giving 3.4 degrees tilt. The elevated
hypothesis uses the 5.4 m static recovery travel, giving 10.2 degrees tilt and
2.8 m remaining edge air gap from an initial 10.0 m. Static recovery is not a
prediction of dynamic plunge or arrest. Without a recovery crossing, only the
peak-to-trough travel lower bound is used and labelled as unresolved.

Conditional post-punch traces add imposed-sway bending `3EI delta/L²`, its shear
`3EI delta/L³`, an illustrative gravity `P-delta` term, and hull-edge descent.
They activate only when the soil peak is reached. In the base preload case,
both hull-state hypotheses exceed the assumed elastic chord and brace capacities
at activation. Buoyancy balance therefore does not establish structural survival.
Large elastic demands indicate that this model has exceeded its range; they do
not predict plastic damage, a transient failure time or a final configuration.

## Ranking results and air-gap changes

The following first intact thresholds are illustrative, with load factors rounded
to one decimal place. Zero can mean failure at the starting state or a small
positive threshold rounded to zero; the machine-readable status distinguishes
these. The table uses all-leg preload; both preload patterns and every mode's
full order appear in the [numerical summary](../../../examples/data/jackup_failure_sequence/summary.json).

| Soil case | Elevated preload | In-water preload | Elevated storm | In-water storm |
| --- | --- | --- | --- | --- |
| Sand over soft, low | Punch-through (0.0) | Punch-through (0.4) | Punch-through (0.0) | Overturning (0.7) |
| Sand over soft, base | Punch-through (0.5) | Punch-through (0.9) | Chord (1.8) | Overturning (0.7) |
| Sand over soft, high | Punch-through (1.1) | Punch-through (1.5) | Chord (1.8) | Overturning (0.7) |
| Stiff over soft, low | Punch-through (0.0) | Punch-through (0.4) | Punch-through (0.1) | Overturning (0.7) |
| Stiff over soft, base | Punch-through (0.3) | Punch-through (0.7) | Chord (1.8) | Overturning (0.7) |
| Stiff over soft, high | Punch-through (1.1) | Punch-through (1.5) | Chord (1.8) | Overturning (0.7) |
| Uniform soft, low | Penetration (0.6) | Penetration (1.0) | Chord (1.8) | Overturning (0.7) |
| Uniform soft, base | Penetration (0.8) | Penetration (1.2) | Chord (1.8) | Overturning (0.7) |
| Uniform soft, high | Holding (1.3) | Holding (1.6) | Chord (1.8) | Overturning (0.7) |

For the base stiff-over-soft elevated storm path, the independent order is chord
capacity at 1.8, punch-through at 1.9, overturning at 2.1, air gap at 3.0, sliding
at 3.0, brace capacity at 4.4, and holding at 8.0. Rounded values can conceal
ordering differences: the summary retains unrounded factors and ties. Profile
exhaustion is not reached on that path before its factor cap of 8.0. Conditional
post-punch chord, brace and edge-clearance failures activate at the soil threshold,
after the intact chord threshold has already been reached.

Elevated air gap is swept from 1.0 to 20.0 m in 1.0 m steps. Observed changes are
refined and rounded outward into brackets. The base stiff-over-soft preload
path changes from air-gap to punch-through control between 1.1 and 1.2 m; its
storm path changes from air-gap to chord control between 6.6 and 6.7 m. The low
strength storm case instead changes to punch-through control between 1.2 and
1.4 m. All 27 soil/path sweeps and their change brackets are exported. Additional
changes within an unsampled interval are not excluded. Zero gap represents the
separate in-water state, with different buoyancy and swell loads; no continuous
transition from waterborne to elevated behaviour is claimed.

## Reproduction and reusable interface

```bash
PYTHONPATH=src uv run --no-project --with pytest python \
  examples/domains/geotechnical/jackup_what_breaks_first.py OUTPUT
```

The output directory receives `inputs.json`, `summary.json` and
`utilization.figure.json`. The committed
[Plotly specification](../../../examples/data/jackup_failure_sequence/utilization.figure.json)
contains one interactive figure: utilization against load factor above, and
first intact threshold against air gap below. A selector covers 54 soil/path/state
combinations. Solid traces describe independent intact thresholds; dotted traces
describe conditional consequences. Threshold markers retain jumps. The initial
utilization axis is limited to 2.0 for readability; Plotly autoscale exposes the
larger conditional ratios. In-water air-gap panels show a separate point at zero.
All chart data are embedded; generation performs no network requests or solver
calls. Rendering requires a locally supplied Plotly library.

`FailureMode(name, demand, capacity, basis)` accepts demand and capacity callables
of load factor, in matching units. `rank_failures(modes, factors)` validates the
callbacks and grid, records the first observed crossing, refines its bracket,
retains ties, identifies failures at zero and explicitly censors unreached modes.
This interface has no dependency on jack-up geometry and can be reused for
mooring or dynamic-positioning studies with appropriate callbacks.

The grid must include features of the load path: narrow failure excursions can
be missed between samples. Each threshold is an independent extrapolation.
Beyond the first physical failure, the intact load path cannot be assumed to
persist. The ranking is not a time sequence or a progressive-collapse simulation.

## Evidence and known limits

The initial targeted baseline collected 256 tests: 249 passed and seven failed
because isolated dependencies were absent. With those dependencies available,
all 256 unchanged tests pass. An additional 38 tests use independent synthetic
hand calculations for scalar roots, tubular sections, static equilibrium,
preload patterns, buoyancy moment balance, tilt, bending, sliding, holding and
conditional onset, plus portable chart export.

The model omits member imperfections, local and global inelastic buckling,
connection flexibility, rack-tooth mechanics, guide contact, combined foundation
loading, cyclic soil changes, flooding, wave impact, inertia, dynamic plunge,
rigid-body stability in water and post-failure load redistribution. The wave
crest and hull-swell proxies require qualification. The wet-hull elastic
cantilever idealization is particularly limited by the missing guide and
hydrostatic coupling model. Strength sensitivities do not quantify all input
uncertainties. Standards clauses, site soil evidence, equipment capacities and
independent engineering review remain open before design or operational use.
