# Naval architecture value examples

## Summary

The value tests add independent arithmetic examples for resistance coefficients,
box-hull loading and lumped hull-girder geometry. Inputs are synthetic; the tests
do not qualify resistance fixtures or an operational loading condition. Strict
expected failures retain disagreements with the source calculations. Production
calculations are unchanged.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-22 | Add scalar and box-hull examples to distinguish arithmetic coverage from source and model limits. |

## Sources and qualification

| Key | Opened source and edition | Location used | Evidence class |
| --- | --- | --- | --- |
| H82 | [Public reproduction of *An approximate power prediction method*, International Shipbuilding Progress 29, 1982](https://api.github.com/repositories/1358994851/contents/src/openhull/holtrop.py?ref=6e4115f1dac86008d3bc58dcf42b4bb7dbec805c) | OpenHull revision `6e4115f1dac86008d3bc58dcf42b4bb7dbec805c`, `src/openhull/holtrop.py`, lines 146–226; identifies paper pp. 166–168 | Public code reproduction; original paper was not obtained |
| S20 | [*Principles of Ship Performance*, Summer 2020 edition](https://www.usna.edu/NAOE/_files/documents/Courses/EN400/EN400_Course_Notes%2C_Summer_2020.pdf) | Section 1.7.7 and Appendix C, sections 3.3, 4.7.1, 5.3 and 6.2 | Public course text; hand examples below use its relations, not its worked input sets |
| IS08 | [IMO Resolution MSC.267(85), International Code on Intact Stability, 2008](https://wwwcdn.imo.org/localresources/en/KnowledgeCentre/IndexofIMOResolutions/MSCResolutions/MSC.267(85).pdf) | Part A, section 2.2.4 | Public rule: initial GM criterion only |
| E | [Element model](../../../src/digitalmodel/naval_architecture/hull_girder_smith.py) | `HullGirderElement.stress` docstring | Documented software model, not an independent physical validation |

The three public source responses were retrieved on 2026-10-22 with HTTP 200.
The reproduction record decodes to the same bytes as its raw source response.
Source-byte SHA-256 digests are:

| Key | Digest |
| --- | --- |
| H82 | `b37062468b02d388141fdc08a6cc122c2e902f461bd94a11e0e3ed53cacc7574` |
| S20 | `f8f4807492bffcc61cfe1b03bc7a0f457d7ab5209bb0404bba44a4e363be7cec` |
| IS08 | `d04db58f4af735d3d98221eb3c040fca23739b5d87197dd857279ea22f7f2a13` |

The [1982 institutional landing page](https://www.marin.nl/en/publications/an-approximate-power-prediction-method)
opened with HTTP 200, but its download requires authentication. The attempted
1984 institutional publication locator returned HTTP 404. The 1984 coefficient
range qualification is therefore NOT_EVALUATED. H82 provides arithmetic evidence
for the interior regime points used here, without establishing equivalence to
all revisions.

The [publisher's textbook page](https://www.sname.org/principles-naval-architecture-series)
redirected to authentication. An accessible edition of the wall-sided stability
text cited by the module was not obtained. Nonzero wall-sided GZ values and GZ
areas are NOT_EVALUATED; tests cover the zero limit, sign reflection and a
positive-heel monotonicity invariant. No source edition is inferred.

## Resistance coefficient examples

The scalar inputs are independent of the fixture questioned in issue #2020.
The tests do not load the total-resistance fixture or its expected coefficients.

| Functions | Examples and independent expectation |
| --- | --- |
| `length_of_run` | Centred and shifted buoyancy centre; substitute the prescribed inputs into H82's length-of-run expression |
| `coeff_c7` | One hand point in each of the narrow, intermediate and broad beam/length regimes |
| `coeff_c12` | One point below, inside and above the transition interval; upper regime retained as an expected failure |
| `coeff_c13` | Three supplied stern coefficients, using the linear expression in H82 |
| `coeff_c2`, `coeff_c3` | Zero-bulb limit plus one finite exponential and one finite bulb-area calculation; bulb height is explicitly zero |
| `coeff_c4`, `coeff_c5` | Draft-ratio cap, no-transom limit and one finite transom-area point |
| `coeff_c15`, `coeff_c16` | Interior points in every source regime; c15 interpolation retained as an expected failure |
| `lambda_wave`, `exponent_m1` | Both length/beam regimes, the transition itself and one exponent calculation |
| `half_angle_of_entrance`, `coeff_c1` | A synthetic geometry where the current regression agrees with H82 and a second geometry that exposes the missing displacement-volume dependence |

Each test docstring gives the source expression, substituted terms and expected
literal. Decimal expectations are stored independently of the application.
Relative tolerance `1e-9` permits rounding of powers and exponentials; it does not
represent resistance prediction accuracy. In `coeff_c5`, the public source uses
midship coefficient in the denominator while the interface names the argument
`cwp`. The scalar substitution is checked, but physical parameter mapping remains
NOT_EVALUATED.

## Worked box-hull example

The prescribed inputs are length 40 m, beam 10 m, depth 8 m, mass 800 t,
longitudinal centroid 20 m, vertical centroid 2 m and water density 1 t/m³.
The density is an explicit example input. Uniform mass occupies the full length.
Using S20 hydrostatics and moments:

| Quantity | Hand method | Answer |
| --- | --- | --- |
| Volume | mass / density | 800 m³ |
| Draft | 800 / (40 × 10) | 2 m |
| KB | draft / 2 | 1 m |
| Transverse BM | 10² / (12 × 2) | 25/6 m |
| Transverse GM | 1 + 25/6 − 2 | 19/6 m |
| Trim | longitudinal centroid equals midlength | 0 m |
| Weight and buoyancy density | 800 / 40 | 20 t/m each |
| Still-water shear and moment | integrate buoyancy minus weight | zero throughout |

The following settings reproduce the example through the public mapping API:

```python
from digitalmodel.naval_architecture.loading_computer import run_loading_computer

settings = {
    "hull": {"length_m": 40, "beam_m": 10, "depth_m": 8},
    "water_density_t_m3": 1,
    "weight_items": [
        {"name": "uniform mass", "weight_t": 800, "lcg_m": 20,
         "vcg_m": 2, "length_m": 40},
    ],
    "max_heel_deg": 40,
    "heel_step_deg": 10,
    "longitudinal_strength": {"n_stations": 41, "allowable_bending_t_m": 1},
    "damage_cases": [
        {"name": "sealed", "length_m": 4, "x_center_m": 20, "permeability": 0},
    ],
}
result = run_loading_computer(settings)
```

The end-to-end value test checks the draft, GM, heel grid, unchanged sealed-case
draft and zero shear/moment. The bending allowance is a stipulated example input,
not a regulatory limit. No aggregate stability acceptance is inferred from the
zero still-water bending result.

A separate centroid example uses weights 100, 200 and 300 t at longitudinal
coordinates 2, 5 and 8 m and vertical coordinates 1, 2 and 3 m. The total is
600 t; first moments are 3600 and 1400 t·m; centroids are 6 m and 7/3 m. Empty
input is rejected because a positive total mass is required.

For intact stability, the same box has GM = 19/6 m. The isolated IS08 initial-GM
criterion is tested below, at and above its published minimum. The other criteria
are not independently qualified by these tests.

For a fully flooded centred compartment 4 m long and spanning the beam, the
lost-buoyancy relation in S20 section 4.7.1 preserves mass and effective displaced
volume. The remaining waterplane is 400 − 40 = 360 m². Thus 360 T′ = 800,
T′ = 20/9 m and sinkage = 2/9 m. The implementation returns 11/5 m from an
intact-waterplane approximation, a draft underestimate of 1.0 %. The exact final
draft is retained as a strict expected failure. The remaining transverse inertia
is 3000 m⁴ and volume remains 800 m³, so BM′ = 15/4 m, KB′ = 10/9 m and
GM′ = 103/36 m. This GM is a second strict expected failure; the lost-waterplane
inertia is tested independently. Zero permeability independently
checks the unchanged-draft, unchanged-inertia and unchanged-GM limit.

## Hull-girder examples

The rectangular section is 4 m wide and 6 m high with wall thickness 1/5 m.
The inner rectangle is 18/5 m by 28/5 m. By S20 Appendix C and section 6.2.3:

- Second moment: I = [4 × 6³ − (18/5) × (28/5)³] / 12 m⁴.
- Extreme-fibre distance: c = 3 m.
- Elastic section modulus: Z = I/c = 4024/625 m³.

The API sums point areas and neglects each strip's own inertia. The test uses
1000 midpoint strips per wall; its tolerance bounds the omitted inertia and the
small displacement of the outermost strip from the physical extreme fibre.
This distinguishes a lumped approximation from exact finite-thickness geometry.

The element examples prescribe the modulus, yield, compressive capacity and
residual fraction. They check area conversion, elastic stress, the non-buckling
yield plateau and one point on the documented post-buckling curve. They do not
qualify the delegated plate-buckling rule or a material grade.

The strength examples supply the permissible capacity explicitly: section stress
is calculated from moment and modulus with a synthetic capacity provider, and
ultimate-strength factors are explicit inputs. Utilisation is checked at one
half, exactly unity and above unity. Equality passes. No classification-rule
material factor or partial factor is transcribed. Wave bending and shear holders
preserve supplied signed quantities; these holders have no utilisation or
pass/fail member. Their rule-derived loads remain NOT_EVALUATED.

## Longitudinal load boundary conditions

For the uniform floating barge, net load is zero everywhere. For a single central
800 t mass on the same 40 m floating length, uniform buoyancy is 20 t/m. Before
midlength, shear is 20x; after midlength, shear is 20x − 800. Integrating these
triangles gives bending of 1000 t·m at the quarter points and a central peak of
W L/8 = 4000 t·m. S20 section 6.2 establishes the load-curve integration method.
The tests bound the station-cell error at two resolutions.

A simply supported span instead has concentrated end reactions. Its central-load
peak W L/4 follows from reaction W/2 acting through L/2; its shear is constant
between point forces, rather than triangular. The loading API constructs a
floating linear buoyancy distribution and exposes no end-reaction input. The
simply supported case is NOT_EVALUATED rather than assigning the wrong boundary
conditions to a floating calculation.

## Checks and remaining work

The new checks are in `test_holtrop_coefficients_values.py`,
`test_loading_computer_values.py` and `test_hull_girder_values.py` under
`tests/naval_architecture/`. Their docstrings retain full arithmetic precision.
An isolated AST inventory records all public functions and explicit methods;
the pull request includes its table because no `tests/_tools/` directory exists.
The static inventory identifies name references and numeric-assertion candidates,
not demonstrated dynamic coverage.

Later work requires an accessible 1984 paper, an accessible edition for the
nonzero wall-sided GZ formula, independent qualification of the delegated
buckling and wave-rule calculations, and an API with support reactions for a
simply supported span. The coefficient regressions and finite-flooding draft
expected failures require maintainer review before any production correction.
