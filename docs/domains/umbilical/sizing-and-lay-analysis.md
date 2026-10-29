# Umbilical sizing and static lay analysis

The native Python core calculates a supplied cross-section build-up, screens
local pressure differentials against supplied ratings, and calculates static
lay geometry for parallel, bundled and suspended configurations. The worked
example below connects sizing to lay analysis through submerged weight.
`NOT_EVALUATED` identifies missing qualification evidence. A passing individual
limit check does not establish API 17E compliance or installation clearance.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-20 | Added sizing, pressure and static lay worksheets with explicit qualification gaps. |

## Inputs and method

All interfaces use SI units: m, kg/m³, kg/m, N/m, N and Pa. Each
`FunctionalElement` describes a named group of identical straight steel tubes
or thermoplastic hoses: count, bore, outside diameter, effective wall density,
contents density and optional pressure limits. Reinforced hose requires its
effective wall density and qualified ratings; steel constitutive equations are
not applied to hose. `QualifiedLimit(value, basis)` records a positive supplied
allowable and a nonblank evidence reference. The caller must establish that the
reference covers the temperature, life, safety allowances and load case.

`UmbilicalSection` supplies the core diameter, filler density, ordered armour
annuli and solid sheath. `RadialBuildLayer(thickness, density, material_fraction)`
requires the occupied material fraction; no packing coefficient is assumed.
Unoccupied armour area is sealed massless void. Core filler occupies the area
outside the functional elements. Contents mass is included in the reported air
and water weights. Empty bores use zero contents density. Geometric area
occupancy is checked, but manufacturing fit remains `NOT_EVALUATED`. This
analytical model complements the metadata and drawing schemas in
`src/digitalmodel/subsea/cross_sections/`; it does not optimize element packing.

The governing equations and sources are:

| Calculation | Equation | Public basis |
| --- | --- | --- |
| Areas and build-up | `A = πD²/4`, `A_annulus = π(Do² − Di²)/4`, `D_next = D + 2t` | Circle geometry; factors are mathematical identities. |
| Filled mass per length | `m = ΣρA`, with armour area multiplied by occupied fraction | OpenStax, *University Physics Volume 1* (2016), [§14.1, density](https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure). |
| Air and water weight | `W_air = mg`, `W_water = (m − ρwater A_outer)g` | OpenStax (2016), [§14.4, buoyancy](https://openstax.org/books/university-physics-volume-1/pages/14-4-archimedes-principle-and-buoyancy). Sealed outer envelope; air buoyancy neglected. |
| Local external pressure | `p_external = ρwater g depth` | OpenStax (2016), §14.1, pressure variation with depth. |
| Burst and collapse demands | `max(pi − pe, 0)`, `max(pe − pi, 0)` | Pressure-difference definitions; compare independently to supplied qualified ratings. |
| Static catenary | `a = H/w`, `z = a(cosh(x/a) − 1)`, `s = a sinh(x/a)` | [*Catenary Curve* (2019)](https://www.mygeodesy.id.au/documents/Catenary%20Curve.pdf), equations (4), (9), (10), with height datum shifted to the vertex. |
| Touchdown geometry | `s = √(z² + 2az)`, `x = a asinh(s/a)`, `Ttop = H + wz` | Same source, equations (3), (10), (34), by substitution. |
| Bend radius | `R = a cosh²(x/a)`; vertex minimum `R = a` | Same source, equation (32). |

`check_pressure` requires an internal pressure for every named element, using
the same surface gauge datum as external pressure at the assessed depth.
Operating, depressurized, installation and shutdown cases require separate
calls. Missing ratings remain `NOT_EVALUATED`, including at zero differential.
Any failed element gives overall `FAIL`; otherwise standard qualification gaps
keep the aggregate pressure status `NOT_EVALUATED`.

`analyze_lay` takes horizontal effective tension `H`, positive submerged weight
`w`, vertical drop and available payout capacity. Parallel and bundled cases
assume a horizontal tangent at a flat frictionless seabed. Residual tension is
the effective tension at touchdown, `H`; tension farther along the laid route
requires friction and boundary data. Bundled cases require companion submerged
weight; the common centreline calculation sums the weights. Bundle load sharing,
relative slip and component offset curvature remain `NOT_EVALUATED`.

Suspended cases additionally require horizontal span `X` and lower-support
clearance. Both supports may differ in elevation. From the cited hyperbolic
identities, `u_mid = asinh(z/(2a sinh(X/(2a))))`,
`u1 = u_mid − X/(2a)` and `u2 = u_mid + X/(2a)`.
Length is `a(sinh(u2) − sinh(u1))`. The point nearest `u = 0` within that
interval gives minimum radius and clearance. Intersecting the seabed fails the
free-span assumption. A fully suspended line has no touchdown; the residual
touchdown check remains `NOT_EVALUATED`.

## Worked example

All numeric inputs below are illustrative, **assumed, based on previous
experience**, chosen to simplify arithmetic. They are not qualified product
data or values prescribed by a standard. The tests independently write out
each expected value; production code does not supply the expected answers.

| Input | Value |
| --- | --- |
| Straight functional elements | 2 steel tubes, OD 20 mm, ID 10 mm |
| Effective wall / contents density | 8000 / 1000 kg/m³ |
| Core diameter / filler density | 40 mm / 1000 kg/m³ |
| Armour | 5 mm thickness, 8000 kg/m³, occupied fraction 0.5 |
| Solid sheath | 5 mm thickness, 1000 kg/m³ |
| Water density / gravity | 1000 kg/m³ / 10 m/s² |
| Depth / internal gauge pressure | 100 m / 3 MPa |
| Supplied burst / collapse limits | 2 / 1 MPa differential |
| Lay horizontal effective tension | `H = 100 W_water` |
| Drop / available payout capacity | 100 / 200 m |
| Supplied minimum radius / residual tension | 50 m / 5000 N |

Mass contributions in kg/m are `1.2π` for walls, `0.05π` for contents,
`0.2π` for filler, `0.9π` for armour and `0.275π` for sheath.
Thus filled mass is `2.625π`, or **8.2 kg/m**, and final OD is **60.0 mm**.
The sealed displacement is `0.9π kg/m`, so air weight is `26.25π`, or
**82.5 N/m**, and water weight is `17.25π`, or **54.2 N/m**.

External pressure is `1000 × 10 × 100 = 1 MPa`; burst differential is
`3 − 1 = 2 MPa`, equal to the supplied limit, while collapse demand is zero.
Both individual checks pass. Standard qualification remains `NOT_EVALUATED`.

For lay, `a = H/w = 100 m`, so suspended length is `100√3`, or **173.2 m**,
and touchdown offset is `100 ln(2 + √3)`, or **131.7 m**. Minimum radius is
**100.0 m**, top effective tension is `3450π`, or **10838.5 N**, and touchdown
effective tension is `1725π`, or **5419.2 N**. Both supplied minimum limits
pass, and required suspended length is below the supplied payout capacity.

Run the worksheet from the repository root:

```sh
PYTHONPATH=src uv run --no-project python examples/subsea/umbilical_sizing_and_lay.py
```

The independent lay tests also cover a bundle with total weight 20 N/m:
`a = 1000/20 = 50 m`, `s = 100√2 m` and top tension `3000 N`; the supplied
100 m radius limit fails. The suspended-span worksheet uses
`X = 200 ln(2 + √3) m`, equal-height supports 150 m above the seabed and
`a = 100 m`: clearance is **50.0 m**, length is `200√3`, or **346.4 m**,
and each support tension is **2000.0 N**. An unequal-height case checks a
monotonic span whose theoretical vertex lies outside the supported interval.

## Qualification gaps and way forward

Clause-level API 17E / ISO 13628-5 citations could not be verified from accessible
public pages. The ISO catalog request returned HTTP 403; the attempted API
publication overview addresses redirected to the publisher's error page.
No clause numbers, licensed values, pressure factors or compliance conclusions
are inferred. Both modules expose `standard_qualification = NOT_EVALUATED`.
The maintainers must supply an authorized edition and verified clause mapping
before a standard qualification claim can be made.

The model excludes helical element length, flooding, ovality, manufacturing
tolerances, material stress recovery, combined loads, elastic stretch, bending
stiffness, dynamics and soil response. Available payout is an upper capacity;
the code does not solve tension from a fixed installed length. Radius screening
at ideal touchdown is insufficient to qualify a stiff umbilical. Later work
requires qualified product data, packing confirmation, pressure envelopes and
stiffness/contact analysis, including bundle interaction where applicable.
The API 17F control-system package remains separate.
