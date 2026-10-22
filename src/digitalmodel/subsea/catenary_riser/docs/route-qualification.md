# Catenary route qualification and installation analysis

Status: **DRAFT**, pending maintainer review. Static screening only.

## Summary

The modern simple solver and both legacy vertex calculators agree on their
overlapping inextensible, uniform-weight, horizontal-tangent boundary condition.
The installation utility integrates signed constant-weight sections and retains
infeasible candidates. It does not clear the quarantined lazy-wave analyzer or
qualify native exports, elasticity, bending, fatigue, current or dynamics.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| DRAFT 1 | 2026-10-22 | Establish route contracts, input checks, screening and evidence gaps for independent review. |

## Sources and applicability

The public [Catenary Curve (2019), equations (1), (3), (4), (9), (10), (14), (34)](https://www.mygeodesy.id.au/documents/Catenary%20Curve.pdf)
was opened for this analysis. Its force balance and geometry establish the
identities below. Its ordinate is measured above the mathematical directrix;
subtract `a` to put the vertex at zero. The signed-section extension below is an
explicit integration of the same equilibrium, not a published lazy-wave example.

[NIST SP 811 (2008), Appendix B.8, length conversion table](https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication811e2008.pdf)
gives the exact international inch as `0.0254 m`. The legacy area boundary
therefore multiplies square inches by `0.0254²`; modern configurations use SI.
Display conversions use the SI prefix kilo, as described in the same publication.

NREL/TP-500-41958 (2007), section 2.5, could not be opened because the supplied
document hosts did not resolve. *Sea Loads on Ships and Offshore Structures*
(1990), chapter 8, was not available for inspection. Neither is treated as checked
evidence. No licensed standard values have been extracted. DNV-OS-F201 and
API RP 1111 applicability and editions remain **NOT_EVALUATED** for these
installation screens. The historical fatigue criterion remains separately cited
in [engineering-basis.md](engineering-basis.md), E5.

Synthetic hand inputs and supplied limits are examples, not prescribed engineering
values. The existing acceleration default is retained for compatibility and is
assumed, based on previous experience; a calculation must supply its applicable
gravity. Material/fluid presets remain unqualified metadata. No preset strength
or density is an acceptance limit in the new utility.

## Coordinate and units convention inventory

`qualification.route_conventions()` exposes this inventory as package output.
All analytical routes use metres, newtons, newtons per metre and degrees.
Static calculations have no phase variable. Tensions are positive magnitudes;
the vertical resultant is positive upward along the line from touchdown to top,
and positive effective weight acts downward.

| Route | Axes and origin | Angle | Package-output evidence / disposition |
| --- | --- | --- | --- |
| `simple_H` | Vertex `(0,0)`; x toward top, z upward | Top from vertical; touchdown and distribution from horizontal | `test_route_qualification::test_modern_vertex_and_distributions` |
| `simple_top_tension` | Same vertex; force governs span | Same | Same parameterised test |
| `simple_span` | Same vertex; offset is suspended span, excluding any grounded tail | Same | Same parameterised test; `test_audit_regressions::test_geometry_inverse_and_exhaustion` |
| `legacy_force` | Implied vertex; `d` is positive height; `F` is total top tension and `w` already N/m | No angle output | `test_route_qualification::test_legacy_vertex_overlap`, both legacy callers |
| `legacy_angle` | Implied vertex; `BendRadius=a`; `b=1/a` only on the force route | `q` from vertical | Same test, both legacy callers |
| `lazy_wave_analyzer` | No coordinates returned | No angle returned | QUARANTINED; `test_lazy_wave_quarantine` |
| `legacy_exports` | Export-specific origin/axes; no common global transformation qualified | Export-specific | NOT_EVALUATED; missing type data and native evidence |
| `installation_sections` | Vertex `(0,0)`; x toward top, z upward; `touchdown_point=(-span,-depth)` is displacement **from hang-off** | Top from vertical | `test_installation_planning::test_piecewise_hand_case` |

The general elastic boundary-value catenary solver, legacy plotting coordinates
and the separate five-segment lazy-wave construction are distinct models. Matching
one vertex case does not make those routes interchangeable. No canonical route is
selected for arbitrary end elevations, prescribed seabed friction, lift-off angles,
point loads, current or finite stiffness. Use a separately qualified model for such
conditions. The modern simple solver rejects ambiguous force inputs, floating
uniform lines and infeasible geometry; force solutions that exceed available
length retain `converged=False`. A supplied force may change the requested span,
which remains an explicit warning. The CLI refuses unconverged output.

## Governing method

For uniform weight `w>0`, horizontal tension `H>0`, `a=H/w`, depth `d>0`:

```text
z = a(cosh(x/a)-1)             [source equations (4), (9)]
s² = d²+2ad                   [source equations (10), (34)]
V = ws; T² = H²+V²; T = H+wd   [source equations (1), (3), (14)]
theta_vertical = atan2(H,V)    [source equation (1)]
curvature(vertex) = 1/a        [differentiate equation (9)]
```

The implementation evaluates the equivalent span identity
`x=2a asinh(sqrt(d/(2a)))`, avoiding cancellation in `acosh(1+d/a)` for a
nearly horizontal line. The synthetic limit regression uses `a=1e16 m` and
`d=2 m`; the series limit independently gives `x=sqrt(2ad)=2e8 m` to
floating precision. These magnitudes test conditioning, not installation scope.

For a constant signed weight section, advancing arc distance from bottom to top:

```text
V1 = V0+wL; T0=hypot(H,V0); T1=hypot(H,V1)
dx = (H/w)[asinh(V1/H)-asinh(V0/H)]
dz = (T1-T0)/w = L(V1+V0)/(T1+T0)
k = |d atan(V/H)/ds| = |w|H/(H²+V²)
```

These integrals follow from `dx/ds=H/T`, `dz/ds=V/T`, `dV/ds=w`. Horizontal
tension and tangent direction remain continuous at section interfaces; curvature
can jump as weight changes. Maximum curvature is evaluated at the smallest
absolute V within each section, including an internal zero crossing.

The lazy-wave screen has a lower bare section, a uniformly buoyant section and an
upper bare section. `buoyancy_start` and `buoyancy_length` are arc distances from
touchdown. `net_buoyancy` is positive upward N/m **after including bare weight**,
not an additional module force to subtract twice. The start, buoyant length and
total suspended length determine the upper length. The integrated vertical load
determines H from either hang-off angle or top tension. Cases without both hog
and sag reversals, with sag on/below seabed, or with hog at/above hang-off are
rejected. Module discontinuities are idealised as distributed weight changes.

## Sweep and constrained selection

`planning.sweep_simple` accepts one fixed water depth and lists of alternative
hang-off angles, top tensions and suspended lengths. Each entry independently
determines the other quantities; they are not three independent boundary inputs.

`planning.sweep_lazy_wave` crosses suspended lengths with alternative angle and
tension controls. Water depth is calculated, so specify the permitted depth
interval in `PlanningLimits`. This is a free-span screen, not a fixed-endpoint
solver. It reports touchdown displacement, bottom tension, maximum curvature,
top force, length and the internal extrema.

`select_optimal(sweep, objective=..., limits=...)` returns the least finite
objective among feasible **supplied candidates**. All eight limit fields are
required. No capacity, safety factor or acceptance default is inferred. An empty
feasible set gives `best=None`, `feasible=False` and every rejection reason.
Ties retain the first candidate. This is a grid selection utility; no continuous
global optimum or installation operability is asserted.

## Synthetic 1000 m worked comparison

Run `PYTHONPATH=src python examples/subsea/catenary_installation_screen.py`.
Inputs are depth `1000 m`, bare weight `100 N/m` and net buoyant-section uplift
`100 N/m`. Let `lambda=1000/(12-2sqrt(13))`. The lazy-wave section lengths are
`4lambda`, `6lambda`, `6lambda` and H is `300lambda N`.
The simple catenary uses the same H, depth and bare weight. These are synthetic
values, not installation limits or a like-for-like optimisation.

The unscaled hand construction has H=3, lower V=4, post-buoyancy V=-2 and top V=4.
The tension magnitudes are 5 at the lower interface/top and `sqrt(13)` at the
upper interface. Adding the three exact section rises gives `12-2sqrt(13)`;
adding spans gives `9ln(3)+6asinh(2/3)`. Scaling to depth gives:

| Route | Depth m | Span m | Suspended length m | Top kN | Bottom kN | Top angle deg | Maximum curvature 1/km |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Simple vertex | 1000.0 | 1007.3 | 1501.0 | 162.6 | 62.6 | 22.7 | 1.6 |
| Piecewise lazy wave | 1000.0 | 2847.9 | 3341.1 | 104.4 | 62.6 | 36.9 | 1.6 |

The longer buoyed shape reduces top load in this equal-H comparison and consumes
more span and length. It does not establish a preferred installation. Supply
length, depth, curvature, tension and span limits, select the objective, and review
the resulting feasible candidates. Independent scaled hand checks and numerical
quadrature are in `test_installation_example.py` and `test_installation_planning.py`.

## Report evidence and remaining qualification

Both HTML builders and the fallback receive the same evidence sections. Renderer
selection and fallback reason appear in an HTML comment. Standard edition,
calculation identity (`run_id`) and an explicit UTC calculation timestamp are
required evidence fields. Render time cannot fill a missing calculation time.
Missing strength, fatigue, code-check or revision groups flag partial evidence;
known failures remain FAIL and a passing subset cannot roll up to PASS. Supplied
metadata is not authenticated. Neither complete rendering nor custom mode
certifies a design. Revision history follows the executive summary.

Every historical section-6 row has a disposition in `qualification.json`, checked
against the pack. TESTED records name package regressions; NOT_EVALUATED records
carry a reason. Historical line numbers identify the original observation, while
function names and updated links locate current code. The old inventory is retained
so omissions are visible. Unqualified native exports, empirical legacy buoyancy
sizing, RAO conventions, material presets and fatigue-input preprocessing require
separate evidence. The legacy lazy-wave vertical-force defect is left to #2044.
