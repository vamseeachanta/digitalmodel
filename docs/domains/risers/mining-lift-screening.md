# Mining lift-riser screening

`digitalmodel.subsea.mining_riser` supplies local slurry-density snapshots,
vertical free-hanging effective-tension balance and linear axial modes. The riser
database includes the `riser_lift_screen` reference with `riser_type: lift`.
Its dimensional fields remain empty: this class identifies an analysis method,
not a measured or qualified configuration.

The scope is deliberately narrow: fully immersed, straight vertical sections,
fixed top displacement, circular nonoverlapping tubes, explicit lower effective
force and optional lower inertial mass. Every dimensional input is supplied by
the caller. There are no material, gravity, damping or acceptance-factor defaults.

## Public references

All references were retrieved on 2026-10-17. Only equations, physical principles
and modelling distinctions are used; no source text, figures or tables are copied.

| ID | Public primary reference | Locator and use |
| --- | --- | --- |
| R1 | [Vertical Hydraulic Transport for Deep Sea Mining: A Study into Flow Assurance, TU Delft, 2016](https://repository.tudelft.nl/file/File_99e939f3-b109-48e8-beea-ac33884b0001?preview=1) | Section 6.2.1, printed pp. 88–89, equations (6.2)–(6.3): local solids inventory and mixture density. |
| R2 | [The Axial Resonance on 4,000m-class Riser System For Deep Sea Mining, conference proceedings 39, 2024](https://www.jstage.jst.go.jp/article/conf/39/0/39_459/_pdf/-char/ja) | Section 2.2, printed pp. 461–462: auxiliary-line mass is retained while unlatched axial connections exclude auxiliary EA; fixed upper and free lower boundary. Sections 3.1–3.2 identify the need to examine axial response and compression. |
| R3 | [Free longitudinal vibrations of functionally graded tapered axial bars by pseudospectral method, 2018](https://www.extrica.com/article/19373) | Section 3, equation (5) and boundary condition (6b): longitudinal rod equation and fixed-free boundary. The finite-element discretization below is an independent derivation, not this paper's numerical method. |
| R4 | [University Physics Volume 1, OpenStax, 2016](https://openstax.org/books/university-physics-volume-1/pages/14-4-archimedes-principle-and-buoyancy) | Section 14.4, “Buoyant Force” and the displayed principle: displaced-fluid weight determines buoyancy. No textbook examples or numerical data are adopted. |

The TU Delft PDF was downloaded directly and its equation checked in extracted
text. R2 is an architectural and boundary-condition reference; its published
modal periods are not adopted as validation targets because the complete dynamic
inputs and contents/added-mass assumptions have not been reconstructed here.

## Equations and derivations

Coordinates `z` run downwards from the string top. Each section is uniform.
Lengths and diameters are in m, areas in m², densities in kg/m³, mass per length
in kg/m, EA and tension in N, modulus in Pa, gravity in m/s² and periods in s.
The constants below follow from circular geometry, linear interpolation and unit
conversion between angular and cyclic frequency; none is a fitted coefficient.

| ID | Implemented equation | Basis |
| --- | --- | --- |
| D1 | `d = D - 2t`; `Ai = pi*d²/4`; `Aw = pi*t*(D-t)`; `Ad = pi*D²/4`; `EA = sum(Ej*Awj)` over axially connected tubes | Circle and annulus area identities; axial constitutive law used in R3 equation (5); connection distinction from R2 section 2.2. Wall area is evaluated in factored form to avoid subtracting similar areas. |
| R1 | `rho_mix = (1-c)*rho_liquid + c*rho_solids` | R1 equation (6.3), specialized to one solids-density class; `c` is local volume fraction. Layers are snapshots with explicit lengths, not an advected profile. |
| D2 | `m = sum(rho_wall*Aw + rho_contents*Ai) + m_additional`; `Ad_total = sum(Ad) + Ad_additional`; `w = (m-rho_external*Ad_total)*g`; `T_top = T_bottom + w*L` | Mass inventory, R4 displaced-volume buoyancy and static vertical force equilibrium. Applying this recurrence from the lower end integrates the weight below each boundary. |
| D3 | `delta_reference = sum((T_top+T_bottom)*L/(2*EA))` | Integral of linear effective tension divided by EA, specialized from the rod constitutive law in R3. This is a pressure-neutral equivalent-rod extension reference. It is not pressurized wall elongation: wall tension, internal/external pressure thrust and Poisson effects are not supplied. |
| D4 | `mu = sum(rho_wall*Aw + chi*rho_contents*Ai) + m_additional + m_axial_added`; `Ke = EA/le * [[1,-1],[-1,1]]`; `Me = mu*le/6 * [[2,1],[1,2]]`; `K phi = omega² M phi`; `f = omega/(2*pi)`; `period = 1/f` | Weak form of R3 equation (5), with explicit assumed participating mass `mu`. For `N=[1-x/le,x/le]`, integrating `EA*(dN/dx)^T*(dN/dx)` gives Ke, and integrating `mu*N^T*N` gives Me. The diagonal mass integrals are `le/3`, the off-diagonal integrals `le/6`. Assemble compatible nodes; remove the fixed top degree of freedom. Add the lower mass to the last diagonal of M. |
| D5 | `f_n = (2*n-1)/(4*L)*sqrt(EA/mu)` | Independent analytical check derived from R3 equation (5) for constant EA and mu: sinusoidal shape, zero top displacement and zero bottom axial force give `kL=(2n-1)*pi/2`. |
| D6 | `cot(beta) = (M/(mu*L))*beta`; `f = beta/L*sqrt(EA/mu)/(2*pi)` | Independent tip-mass check from the same continuum equation: bottom force balance is `EA*k*cos(kL)=M*omega²*sin(kL)`, with `beta=kL`. The test finds the first root independently of the matrix assembly. |
| D7 | `cos(theta1)*cos(theta2) - (Z2/Z1)*sin(theta1)*sin(theta2) = 0`, where `thetaj=omega*Lj*sqrt(muj/EAj)` and `Zj=sqrt(EAj*muj)` | Independent two-section check from R3: propagate the sinusoidal fixed-top solution through continuous displacement and axial force at the interface, and impose zero force at the lower end. The test solves this scalar equation independently of the mesh. |

`chi` is `contents_inertia_fraction`, explicitly bounded between zero and one.
It changes modal inertia only; the complete local contents inventory enters D2.
It is an assumed coupling parameter, not an empirical prediction. Additional
hardware and buoyancy material contribute to both static mass and axial inertia;
axial added mass contributes only to inertia. Specify additional displacement
outside the tube envelopes to avoid counting the same volume twice. For an
auxiliary without axial load transfer, set `carries_axial_load=False`: its mass
and displacement remain while its EA is excluded. Mass transfer through straps
to the equivalent string is assumed; local connection response is not resolved.

Zero lower effective force gives a free lower end. A suspended lower assembly
requires its inertial mass and its net downward effective force as separate
inputs; immersion/buoyancy can make these different. No bottom spring or seabed
contact is included. Lower force may be negative for diagnostic balance. Any
negative boundary effective tension sets `has_compression`; because each section
has linear tension this covers the minimum in every section. The modal function
rejects such a string. Zero at a free end is permitted.

Static tension does not enter the linear longitudinal stiffness matrix. This is
the small-strain rod specialization in R3, not a lateral tensioned-beam model.
Changing buoyancy may change static viability and attached inertia, but there is
no universal direct buoyancy-to-axial-period correction in this implementation.

## Use and limitations

Import the public functions with:

```python
from digitalmodel.subsea.mining_riser import (
    BundleSection, SlurryLayer, Tube,
    axial_modes, free_hanging_screen, slurry_density_profile,
)
```

Build `SlurryLayer` records and use each returned `DensityCell.density_kg_m3` as
the corresponding tube contents density. Split structural and slurry layers at
every common boundary; the API does not infer layer alignment. Construct the
ordered `BundleSection` records, supply gravity and lower effective force to
`free_hanging_screen`, then supply the same inputs plus lower inertial mass,
elements per section and mode count to `axial_modes`. The integration test in
`tests/subsea/test_mining_riser.py` executes this complete path from
`examples/mining_lift_riser/synthetic.json`.

Modal shapes include the fixed zero top node, are normalized to maximum absolute
amplitude one and have a deterministic sign. Frequencies are returned in
ascending order. The dense generalized symmetric eigensystem uses NumPy and
SciPy already declared by the package. Memory grows with the square of the mesh
size. Check mesh convergence for each section distribution and requested mode;
a successful eigensolve alone does not establish discretization accuracy.

The density calculation does not compute particle slip, settling, transport
velocity, blockage, erosion, airlift expansion, gas fraction, pump head or
interruption/restart transients. The structural calculation omits pressure-wall
stress/strain coupling, flow-induced inertia terms, curvature, drag, damping,
forced response, surface-motion coupling, VIV, fatigue and release/recoil.
Natural periods can inform a later forced-response model; they do not predict
response amplitude or establish an acceptable separation from excitation.
There are no class acceptance criteria or qualification claims.

## Synthetic input basis S1

S1 is an authored manufactured verification basis. Every number in
`examples/mining_lift_riser/synthetic.json` and the new numerical test fixtures
is either an S1 assumption listed below, an invalid-input probe, or a result
derived by D1–D6. These are intentionally scaled mathematical quantities, not
published material properties, measured conditions or recommended defaults.

| Inputs and units | S1 assumed values and purpose |
| --- | --- |
| Tube OD, wall thickness (m) | `2`, `0.5`; integer-ratio areas for hand calculations. |
| Wall density (kg/m³), modulus (Pa) | `8`, `800`; artificial material giving a simple EA/mass ratio. Twice the base modulus supplies a nonuniform-section interface check. |
| Liquid and solid density (kg/m³), local volume fraction | `1`, `5`, and `0`, `0.25`, `1`; pure-phase endpoints and an intermediate inventory. Uniform contents density `2` is the mixture result at fraction `0.25`. |
| Section/layer lengths (m) | `1`, `2`, `3`, `5`, `10`; short snapshots, section-order and segment-splitting checks. |
| External density (kg/m³), gravity (m/s²) | `1`, `10`; artificial immersion and gravity for transparent force arithmetic. |
| Additional mass (kg/m), displacement (m²) | Zero unless varied by `pi` or `6*pi` to test buoyancy, internal compression and section-order integration. |
| Contents inertia fraction, axial connection | `0` or `1`, and false or true; explicit bounding assumptions. |
| Lower effective force (N), lower mass (kg) | Force `0`, `2`, `7`, `20`; mass zero or equal to the uniform string mass derived by D2. |
| Axial added mass (kg/m) | Zero or equal to the baseline participating mass from D4; tests inverse-square-root frequency scaling. |
| Mesh controls and numerical checks | `8`, `16`, `32`, `64` elements per section; one or three modes. D5 relative-frequency target `0.001`, D6/D7 target `0.0001`, and first-mode shape absolute target `1e-9` are authored numerical test targets, not engineering acceptance limits. Other scalar comparisons use pytest's default numerical comparison tolerance. |
| Independent root bracketing | `0.01` and `pi/2-0.01`; isolate the first D6 root for a unit tip/string mass ratio. |

Invalid-input probes include zero, negative and nonfinite quantities,
out-of-range fractions, nonboolean connection flags, noninteger mesh controls,
more modes than free degrees of freedom, empty inputs and a bundle with no axial
load path. Such probes do not represent physical source data.

## Verification and remaining scope

Verification covers R1 phase limits and profile coordinates, hand-integrated
effective tension and D3, D5 mesh convergence, D6 lower mass, D7 nonuniform sections, auxiliary connection
participation, contents-inertia distinction, added-mass scaling, compression at
internal boundaries, segment splitting and the complete synthetic example.
The existing database tests check schema, table fingerprints and previous rows.
Regenerating the catalog retains the existing fields and adds one lift reference;
existing dimensional rows are not reinterpreted.

The component-taxonomy and bundle-schema extension in the separate data
repository remains tracked by [worldenergydata issue 1138](https://github.com/vamseeachanta/worldenergydata/issues/1138).
The narrower reading here confines the change to digitalmodel's calculation
interface and reference catalog. Flow assurance, generic campaigns, surface
motion, VIV and metocean acquisition require separate work and evidence.
