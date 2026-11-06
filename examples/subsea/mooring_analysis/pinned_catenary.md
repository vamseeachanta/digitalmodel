# Pinned-end catenary example

The executable [pinned_catenary.py](pinned_catenary.py) constructs a synthetic
uniform, inextensible catenary and passes its endpoints, length and submerged
weight to the package's general boundary-value solver. Independent tests check
the full curve and the horizontal and vertical tension components. This is
partial scope for issue #41.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-27 | Add a synthetic pinned-end example and independent checks to demonstrate endpoint and length inputs. |

## Independent calculation

Take x horizontal and z positive upward, with the catenary vertex at the origin.
The signed parameter u gives

```text
x = a*u
z = a*(cosh(u)-1)
s = a*sinh(u)
H = w*a
V = w*s = H*sinh(u)
T = H*cosh(u)
```

These equations provide the calculation basis: dz/dx = sinh(u), so
ds/dx = sqrt(1+sinh(u)^2) = cosh(u). Integrating from the vertex gives s;
vertical equilibrium gives V=w*s, and the tangent direction gives V/H=sinh(u).
Thus H=w*a, and Pythagoras gives T=H*cosh(u). There are no material data or
values taken from a design standard.

Choose synthetic a=80 m and H=200 N, so submerged weight w=2.5 N/m.
Choose u_start=ln(2) and u_end=ln(4). Since cosh(u)=(exp(u)+exp(-u))/2
and sinh(u)=(exp(u)-exp(-u))/2, direct arithmetic gives:

| Quantity | Start | End |
| --- | --- | --- |
| u | ln(2) | ln(4) |
| cosh(u) | 5/4 | 17/8 |
| sinh(u) | 3/4 | 15/8 |
| x (m), exact | 80*ln(2) | 160*ln(2) |
| z (m) | 20.0 | 90.0 |
| Signed s from vertex (m) | 60.0 | 150.0 |
| Horizontal tension H (N) | 200.0 | 200.0 |
| Signed vertical tension V (N) | 150.0 | 375.0 |
| Resultant tension T (N) | 250.0 | 425.0 |

The span is 80*ln(2) m horizontally and 70.0 m vertically. Its suspended
length is L=150-60=90.0 m. The signed vertical tension difference is
375-150=225.0 N, equal to w*L=2.5*90=225.0 N. Forces applied to the line
are (-H,-V_start) at the start and (H,V_end) at the end; their vertical sum
balances the downward line weight. Both ends are pinned, and neither is the
horizontal-tangent touchdown assumed by the separate mooring analyzer.

The tests use the same independent hand arithmetic and reflect the signed
parameters to cover a low point inside the span, a descending span and a level
span. They recover the start vertical component from the returned resultant
and the known slope sign, rather than assuming force balance. Their numerical
allowance is the square root of binary64 spacing at unity.

## Run

From the repository root, with the package's runtime dependencies available:

```bash
PYTHONPATH=src uv run python examples/subsea/mooring_analysis/pinned_catenary.py
PYTHONPATH=src uv run python -m pytest tests/subsea/mooring_analysis/test_catenary_geometry.py tests/subsea/mooring_analysis/test_pinned_catenary_example.py -q -rA -p no:cacheprovider
```

The example prints geometry and forces to one decimal place and writes no files.
Inputs retain full precision. Direct imports currently also require the parent
package's data and plotting dependencies, although this example makes no plots.

## Limits and remaining scope

The calculation assumes a uniform positive submerged weight and a fully
suspended line in static equilibrium, with no current, seabed contact, bending
or extension. Infinite axial stiffness selects the inextensible limit exactly.
The end must lie to the right of the start, and length must exceed the straight
endpoint distance. Nonfinite inputs and unsupported geometry are rejected.
An exactly straight weighted inextensible line has no finite-tension solution.

Fixed-end bending, elastic-line validation, installation sequencing, external
solver comparisons and integration with installation workflows remain outside
this example. The existing general solver's finite-stiffness approximation is
unchanged and is not established by these tests. Issue #41 remains open for
that wider scope.

The independent fixture is the reusable calculation record: expected geometry
and forces are derived before calling the solver, so a converged result alone
cannot establish agreement.
