# Soft-clay pile lateral and axial load-transfer analysis

## Summary

The new `geotechnical.py_curves` module provides submerged soft-clay p-y
backbones, explicit cycle-dependent strength and stiffness factors, axial
load-transfer curves, database quantity selection and a finite-difference
lateral pile check. The existing axial capacity calculators remain available
for ultimate shaft and toe resistance.

An illustrative 24.0 m pipe pile under 100.0 kN head shear gives head
deflections of 5.7 mm and 10.5 mm at 1 and 100 cycles, respectively. The
increase, computed from unrounded answers, is 85.7 %. These results demonstrate
the selected softening model; they do not establish foundation design acceptance.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-22 | Add sourced backbones, explicit degradation, database selection and a reproducible pile example. |

## Scope and units

Import from `digitalmodel.geotechnical.py_curves`. The implementation uses
the Python standard library and the existing immutable database schema.
It does not import a solver integration or require a third-party numerical package.

Lengths are metres; strength and axial resistance are kPa; effective unit weight
is kN/m³; lateral resistance is kN/m; EI is kN m². Axial curve outputs are
stresses: multiply t by shaft perimeter and tributary length, or q by the
appropriate toe area, to obtain force. Toe plugging and effective area are
separate engineering selections. Positive depth starts at mudline; the model
assumes submerged soil throughout.

`Curve` stores immutable displacement and resistance tuples. It interpolates
linearly between supplied points and holds the final resistance constant.
Lateral and shaft curves reflect oddly for negative displacement. Toe curves
carry zero resistance in uplift. This reflection describes a symmetric
backbone, not unloading/reloading or hysteresis.

## Source basis

The public pages below were opened on 2026-10-22. Source responses and retrieval
records are retained outside the repository; no source document is redistributed.
Equations are implemented independently, without importing OpenPile.

| Source | Locator and use | Qualification |
| --- | --- | --- |
| OTC paper 1204, *Correlations for Design of Laterally Loaded Piles in Soft Clay* (1970) | [OpenPile public equation notes](https://openpile.readthedocs.io/en/latest/_modules/openpile/utils/py_curves.html#matlock_1970), ultimate resistance, strain normalization, transition zone and cyclic formulation | Equations read from the public implementation documentation; the original conference paper was not read. |
| OpenPile lateral documentation | [Public lateral API notes](https://openpile.readthedocs.io/en/latest/API.html#openpile.utils.py_curves.api_clay), rounded cubic curve points | Rounded-point checks supplement independent exact-cube calculations. |
| *Theoretical t-z Curves* (1981) | [Public axial equation implementation](https://openpile.readthedocs.io/en/latest/_modules/openpile/utils/tz_curves.html), `kraft_modification` equation | Equation read; fitting parameters and residual slip remain required inputs. |
| *Beams on Elastic Foundation* (1946) | [Open book scan](https://archive.org/details/in.ernet.dli.2015.73919), section 9, printed page 24, equation 19a | Original scanned equation inspected; independent linear pile benchmark. |
| MIT Structural Mechanics (2013) | [Lecture 5: Solution Method for Beam Deflections](https://ocw.mit.edu/courses/2-080j-structural-mechanics-fall-2013/resources/mit2_080jf13_lecture5/), beam equation and natural boundary conditions | Open lecture notes read; discrete energy derivation is given below. |
| API RP 2GEO, first edition (2011), soft-clay lateral and axial load-transfer guidance | Normative cross-reference only | Licensed text and tables were not accessed; conformance is `NOT_EVALUATED`. |
| Cyclic-clay degradation-index study (1978), cyclic clay strength review (2015), plasticity-dependent cyclic response study (1991) | Candidate material-calibration references named in the task | Not read; no index, plasticity correlation, strength limit or table value is inferred from them. |

The public OpenPile documentation rounds the lateral exponent to 0.33 and the
cyclic junction to 0.72. This implementation uses the exact cube root and caps
the initial cyclic branch at 0.72. This stated numerical convention preserves
both the requested plateau and junction continuity; it is not an assertion of
literal agreement with every published implementation.

## Monotonic and cyclic p-y equations

Supply `SoftClay(su_kpa, effective_unit_weight_kn_m3, eps50, j)` explicitly.
There is no default J, strain or strength. With diameter D, depth z and
effective vertical stress sigma', the publicly reproduced OTC equations are:

```text
pu = min[(3 su + sigma') D + J su z, 9 su D]
yc = 2.5 eps50 D
zr = max[6 D / (gamma_average' D / su + J), 2.5 D]
gamma_average' = sigma' / z, for z > 0
```

At mudline, sigma' is zero and the supplied effective unit weight supplies the
limiting average. For a homogeneous profile, sigma'=gamma' z. At nonzero depth,
the optional `effective_stress_kpa` argument permits integrated overburden.

With r=y/yc, the monotonic normalized resistance is
`min(0.5 r^(1/3), 1)`. Thus y=yc gives 0.5 pu, and y=8yc gives pu.

The cyclic initial branch is `min(0.5 r^(1/3), 0.72)` for r<=3. For r>3:

```text
a = min(z/zr, 1)
p/pu = 0.72 [1 - (1-a) min((r-3)/12, 1)]
```

Below the transition depth, the tail stays at 0.72 pu. Above it, resistance
decreases linearly between 3yc and 15yc and remains at 0.72 pu z/zr thereafter.
The cap on the initial branch resolves rounding at 3yc without a jump. At
zr the shallow and deep branches coincide. The mudline cyclic tail tends to zero.

An independent hand check uses assumed su=20.0 kPa, gamma'=6.0 kN/m³,
D=1.0 m, J=0.5, eps50=2.0 % and z=2.0 m:

```text
pu = min[(3*20 + 6*2)*1 + 0.5*20*2, 9*20*1] = 92.0 kN/m
yc = 2.5*0.02*1 = 0.05 m
zr = max[6/(6/20 + 0.5), 2.5] = 7.5 m
p(yc) = 46.0 kN/m
cyclic p(3yc) = 66.2 kN/m
cyclic p(15yc) = 17.7 kN/m
```

## Cycle-dependent strength and stiffness

`CyclicDegradation(cycles, strength_exponent, stiffness_exponent)` requires both
exponents. The user-specified mathematical model is:

```text
strength_factor = N^(-bs)
stiffness_factor = N^(-bk)
su_N = strength_factor * su_1
```

N must be at least one and the exponents nonnegative. Numerical underflow is
rejected; no unverified residual floor is selected. At N=1 both factors equal
one. For example, assumed N=100 and b=0.5 give a factor of 0.1 by direct powers.
The 1978 reference's applicability and any plasticity-index dependence remain
`NOT_EVALUATED`; this is an explicit modelling law, not a calibrated prediction.

For p-y curves, pu and zr are recomputed using su_N. The characteristic
displacement becomes `yc_N = yc_1 (pu_N/pu_1)/stiffness_factor`. Consequently
the static backbone's secant modulus at a homologous normalized point scales
by exactly `stiffness_factor`, including when overburden prevents pu from
scaling linearly with su. Changes to zr also change the shallow cyclic tail;
resistance at every fixed displacement need not decrease between cycle counts.

For axial curves, resistance scales by `strength_factor` and all displacement
coordinates by `strength_factor/stiffness_factor`. Their homologous secant
moduli therefore scale by `stiffness_factor`. Combining material degradation
with an already cyclic p-y backbone can double-count degradation unless the
combined model is calibrated. `degradation=None` explicitly leaves the material
unchanged; `cyclic=True` independently selects the cyclic lateral envelope.

## Axial t-z and q-z curves

`clay_tz` takes an explicit pre-peak table of `(z/D, t/tult)` pairs, ultimate
shaft stress, residual-strength ratio and residual displacement ratio. The table
must start at zero and increase to unity; the residual point must lie beyond
peak slip. `clay_qz` similarly scales `(z/D, q/qult)` pairs and has a
compression-only constant tail. Ultimate stresses can be obtained from the
existing `piles.py` or `pile_capacity.py` calculators after their applicability
has been assessed. No API normalized table or assumed bearing factor is embedded.

`clay_tz_kraft` implements the publicly reproduced 1981 equation:

```text
z = t D/(2 G) ln[(R - Rf t/tult)/(1 - Rf t/tult)]
```

R is the influence radius divided by pile radius and must exceed one. Rf must
be in [0,1). G, R, Rf, the sampled mobilization ratios, residual-strength ratio
and post-peak slip increment are all required inputs. Residual slip is appended
to peak slip before degradation scaling.

For an independent assumed arithmetic example, tult=40.0 kPa, D=2.0 m,
G=1000.0 kPa, R=5.0 and Rf=0.5 give
`z(t=20)=0.02 ln(19/3)` and `z(t=40)=0.04 ln(9)` metres. Tests substitute
these expressions independently. Tabular tests use expressly assumed arithmetic
tables. Checks against licensed printed API tables remain `NOT_EVALUATED`.

## Database and layered-profile helper

`clay_layer_from_database` consumes existing `Quantity` records from
`Database.quantities_for(row_id)` and explicit selections in SI units:

| Mapping key | Required SI unit |
| --- | --- |
| su | Pa |
| effective_unit_weight | N/m**3 |
| eps50 | dimensionless |
| j | dimensionless |

Every selection must match a scalar or fall within a two-sided range. Qualified,
unavailable, differently dimensioned or unread quantities are rejected. Rows must
describe the same soil unit. The adapter records parent row IDs and converts
Pa to kPa and N/m³ to kN/m³ using the exact SI prefix. The caller assigns property
meaning and layer depths; the helper does not infer them from printed labels.
The existing database does not supply a complete calibrated cyclic-clay material.
Missing strain, J or degradation data require separate sourced input.

`assemble_py_family` accepts contiguous `ClayLayer` records starting at mudline,
ordered sample depths, diameter and a common displacement grid. It integrates
effective overburden over all overlying layers, assigns an interface to the deeper
layer, and returns immutable depth-major curves, resistance arrays, layer indices
and source row IDs. Inputs are not mutated. Each station uses a local homogeneous
backbone with integrated overburden; a rigorous layered wedge mechanism is
`NOT_EVALUATED`.

## Finite-difference lateral pile check

The small solver treats a constant-EI Euler beam with free head rotation and
free toe. Applied head shear H and moment M are external generalized loads.
For uniform spacing h, define curvature at each interior node by
`D2 y_i=(y_(i-1)-2y_i+y_(i+1))/h²`. Discrete bending energy is
`EI h/2 sum((D2 y_i)²)`. Its derivative gives the symmetric pentadiagonal
bending matrix. Soil reactions use trapezoidal tributary widths. Head moment
does work against `(y_1-y_0)/h`, so its equivalent forces are `(-M/h, M/h)`.
This construction preserves the discrete rigid-translation and rigid-rotation
balances. Natural free-end conditions are approximated and require refinement.

The solver uses a pure-Python banded LDLᵀ solve inside relaxed secant iteration.
At zero displacement the secant uses the first positive supplied displacement
point; this regularizes the analytical p-y curve's infinite initial tangent.
Every tolerance, iteration limit and relaxation factor is supplied explicitly.
Convergence requires displacement increments and nodal residuals below their
limits, total force error below the force limit, and total moment error below
the force limit times embedded length. A singular foundation or exhausted
iteration budget raises an error instead of returning a converged result.

For the independent published check, equation 19a of the 1946 book gives
`y(z)=2 H beta/k exp(-beta z) cos(beta z)` for a semi-infinite free-head beam,
where `beta=(k/(4EI))^(1/4)`. With assumed EI=10000.0 kN m²,
k=40000.0 kN/m² and H=100.0 kN, beta=1.0/m and y(0)=5.0 mm.
The finite numerical beam is 12.0 m long. Head errors decrease with refinement:

| Intervals | Head-deflection error |
| --- | --- |
| 120 | 0.2 % |
| 240 | 0.1 % |
| 480 | 0.0 % after rounding |

Tests compare the unrounded solution, the deflection line, force balance and
moment balance. The declared 2.0 % test error budget is a numerical test target,
not a design acceptance criterion or a value attributed to the book.

## Worked 24 m pipe-pile example

Run from the repository root, directing output to a results folder outside the
repository when a persistent report is required:

```sh
PYTHONPATH=src python examples/geotechnical/soft_clay_pile.py --intervals 240
```

All engineering inputs are **assumed, based on previous experience**;
they are illustrative and have no site or laboratory calibration.

| Input | Value |
| --- | --- |
| Embedded length / outside diameter / wall | 24.0 m / 1.0 m / 25.0 mm |
| Elastic modulus / EI | 210.0 GPa / 1912134.7 kN m² |
| su / effective unit weight | 20.0 kPa / 6.0 kN/m³ |
| eps50 / J | 2.0 % / 0.5 |
| Strength / stiffness exponents | 0.10 / 0.15, explicit assumptions |
| Head shear / head moment | 100.0 kN / 0.0 kN m |
| Boundary conditions | Applied free-head loads; free toe |

EI uses the exact annulus second moment `pi (Do^4-Di^4)/64`. Both cases use
the cyclic envelope; N changes only material degradation. A 240-interval mesh
uses displacement tolerance 1e-8 m, force tolerance 1e-5 kN, relaxation 0.5
and at most 1000 iterations. The sampled backbone starts at zero, then uses
145 positive points `1e-5 * 1.1^i` metres for i=0 through 144.

| Cycles | Strength factor | Stiffness factor | Head deflection |
| --- | --- | --- | --- |
| 1 | 1.0 | 1.0 | 5.7 mm |
| 100 | 0.6 | 0.5 | 10.5 mm |

Selected curve points illustrate the actual input springs. Values are calculated
from full-precision backbones and displayed to one decimal place:

| Cycles | Depth | p at y=5.0 mm | p at y=50.0 mm | p at y=150.0 mm | p at y=750.0 mm |
| --- | --- | --- | --- | --- | --- |
| 1 | 2.0 m | 21.4 kN/m | 46.0 kN/m | 66.2 kN/m | 17.7 kN/m |
| 100 | 2.0 m | 13.1 kN/m | 28.2 kN/m | 40.7 kN/m | 24.6 kN/m |
| 1 | 10.0 m | 41.8 kN/m | 90.0 kN/m | 129.6 kN/m | 129.6 kN/m |
| 100 | 10.0 m | 24.4 kN/m | 52.6 kN/m | 75.8 kN/m | 81.8 kN/m |

The example tests compare 120 and 240 intervals, check force and moment
equilibrium, and require increased head deflection under the selected softening.
The 480-interval case was also computed during development. Numerical refinement
does not validate the assumed cyclic constitutive response.

## Limits and way forward

| Check | Status and required evidence |
| --- | --- |
| API RP 2GEO normalized axial tables and standard conformance | `NOT_EVALUATED`: assess the selected edition and clause using authorized access; provide the selected tables explicitly. |
| Cyclic strength/stiffness calibration and plasticity dependence | `NOT_EVALUATED`: obtain appropriate cyclic laboratory data and read the candidate material references. |
| Absolute nonlinear pile deflection against a published case | `NOT_EVALUATED`: qualify a suitable pile load test or independently benchmarked nonlinear solution. |
| Layered failure mechanism, pile group interaction and monopile applicability | `NOT_EVALUATED`: local homogeneous curves alone do not establish these effects. |
| Hysteresis, damping, unloading and cyclic load sequence | `NOT_EVALUATED`: backbones and a cycle-count factor do not model path dependence. |
| Descending-branch equilibrium uniqueness and stability | `NOT_EVALUATED`: secant convergence proves force balance only. |
| Foundation design acceptance | `NOT_EVALUATED`: no design load factors, allowable deflection or structural checks are selected. |

Two attempted government-manual sources were unavailable: the USACE
EM 1110-2-2906 public endpoint returned HTTP 403 and the FHWA NHI-10-016
endpoint returned HTTP 404. The candidate 1978 degradation paper was not found
by title in the open book repository. These gaps are recorded rather than
filled with remembered coefficients. The way forward is to qualify material
inputs, establish the applicable standard edition and compare the nonlinear
response with relevant test evidence before design use.
