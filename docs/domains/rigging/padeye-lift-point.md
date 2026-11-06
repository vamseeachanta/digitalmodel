# Padeye and lift point analysis

The typed API in `digitalmodel.lifting_lug.assessment` evaluates nominal plate
stresses and a direct-shear weld under a given sling load. It extends the
existing lifting-lug package and shares its plate-demand calculation. The
existing workflow retains its earlier allowable inputs; the new assessment
requires explicit allowables and never imports a licensed solver.

The supported model is a single plate with a centred hole under an axial,
in-plane tensile load. The weld model requires a load through the weld-group
centroid that produces direct shear on its effective throat. It excludes
cheek-plate sharing, eccentric and out-of-plane loads and weld-group moments.

## Inputs and outputs

All geometry is in mm, load is in kN, and stress is in MPa. Required inputs are:

- `PlateGeometry`: thickness, pin diameter, hole diameter, width across the hole,
  and hole-centre distance to the loaded free edge along the load direction.
- `SlingLoad`: static tension in the assessed sling, dynamic amplification,
  skew factor and a nonempty source or assumption basis. Static tension already
  includes the sling angle and its load share; factors have no defaults.
- `PadeyeAllowables`: optional `StressLimit` for each check. Each entered limit
  requires a finite positive value and source text identifying its edition and
  clause, or an explicit assumption basis. Source text is retained without
  authenticating its applicability. Missing limits produce `NOT_EVALUATED`.
- `DirectWeld`, when available: total effective load-carrying length and effective
  throat. Length already sums all effective segments. No doubling, end deduction
  or fillet-leg conversion is implicit.

`assess_padeye(plate, load, allowables, weld=None)` returns a frozen
`PadeyeAssessment` containing the inputs, design load, four nominal checks,
qualification gaps and required effective weld throat when calculable. Each
check retains its equation source, allowable source, demand, allowable,
utilization, status and reason. `dataclasses.asdict(result)` supports JSON
serialization without additional packages.

Invalid and nonfinite dimensions, loads, factors or derived quantities raise
`ValueError`. A zero static load is permitted. Factors must be positive; their
suitability and any required rule minima remain unevaluated.

`screening_status` combines the four nominal checks. `status` additionally
includes ABS compliance and lifting qualification. A failure takes precedence
over missing checks. Otherwise a missing check gives `NOT_EVALUATED`. Supplying
all nominal limits cannot turn the overall disposition into `PASS`, because
rule coverage and qualification remain unresolved.

## Method and sources

| Quantity | Equation and basis |
| --- | --- |
| Design load | P = static sling tension × dynamic amplification × skew; defines the explicitly supplied load case, without prescribed factors |
| Nominal bearing | P/(d t), with projected pin area d t; MIT, *Stress-Strain Curves* (2001), equation (1), force/area definition |
| Net-section tension | P/((W − D)t), by rectangular net area; same MIT equation |
| Nominal tear-out | P/(2(e − D/2)t), using two clear shear planes; MIT, *Shear and Torsion* (2000), page 2, direct-shear definition |
| Weld direct shear | P/(L a), using effective throat area; same MIT direct-shear definition |
| Required effective throat | P/(L τ_allow), rearrangement of direct-shear equilibrium; no fabrication minimum inferred |
| Utilization | Demand/supplied allowable; unity is the comparison boundary by definition |

P is converted from kN to N before area division; N/mm² equals MPa. The factors
of two above represent two shear planes and the diameter-to-radius relation,
not empirical resistance coefficients. The equations are nominal demand
idealisations, rather than ABS lug resistance or interaction equations.

Sources opened for this implementation:

- [MIT, Stress-Strain Curves (2001)](https://web.mit.edu/course/3/3.11/www/modules/ss.pdf),
  equation (1).
- [MIT, Shear and Torsion (2000)](https://web.mit.edu/course/3/3.11/www/modules/torsion.pdf),
  page 2, direct shear and the two-interface example.
- [NASA-STD-8719.9C, Lifting Standard (2024)](https://standards.nasa.gov/standard/NASA/NASA-STD-87199),
  section 13.3.2.4. This addresses substitution of analysis for testing for
  permanently attached lifting interfaces, subject to documented rationale and
  approval by the designated authority. It supplies qualification context here,
  not ABS capacities or a universal exemption from testing.
- [ABS public rules index](https://ww2.eagle.org/en/rules-and-resources/rules-and-guides.html).
  The attempted lifting-appliance PDF address returned HTTP 404. No accessible
  ABS equation or allowable evidence was obtained, and no edition, clause number
  or coefficient was inferred. `abs_compliance` remains `NOT_EVALUATED`.

## Worked example

The executable example is
[padeye_assessment.py](../../../examples/workflows/lifting-lug-design/padeye_assessment.py).
Every numeric input is synthetic and labelled
“assumed, based on previous experience”. Its allowables illustrate the API;
they are not rule values.

```bash
PYTHONPATH=src uv run --no-project python examples/workflows/lifting-lug-design/padeye_assessment.py --output analysis.html
```

The example writes the requested report and prints the nominal and qualification
statuses. Its committed output is the
[padeye and lift point analysis](../../reports/padeye-lift-point-analysis.html).

The independent hand calculation is:

- P = 100.0 × 1.5 × 1.2 = 180.0 kN = 180000.0 N.
- t = 20.0, d = 40.0, D = 50.0, W = 150.0 and e = 75.0 mm.
- Bearing area = 800.0 mm²; net area and two-plane shear area = 2000.0 mm².
- Bearing demand = 225.0 MPa; tension and tear-out demands = 90.0 MPa.
- Illustrative allowables are 300.0, 120.0 and 100.0 MPa respectively;
  all three nominal plate comparisons pass.
- L = 200.0 mm and a = 8.0 mm give weld demand = 112.5 MPa, above the
  illustrative 100.0 MPa allowable, so the weld comparison fails.
- Required effective throat = 180000.0/(200.0 × 100.0) = 9.0 mm.
  Reassessment at 9.0 mm passes the supplied nominal weld comparison;
  overall qualification remains `NOT_EVALUATED`.

The calculation tests write out their own expected-value arithmetic. They cover
unequal net and shear areas, factor scaling, zero load, exact equality, failure
precedence, missing criteria, impossible geometry, nonfinite values, overflow,
report escaping and executable report reproduction. The legacy workflow tests
and refreshed atlas whole-grid regression check the shared calculation integration.

## Remaining engineering scope

Obtain the applicable ABS edition, equation clauses, resistance limits and
applicability evidence before implementing code acceptance. Establish weld load
orientation, minimum and maximum sizes and fabrication details before adopting
the nominal throat result. Pin bending and shear, contact and hoop stresses,
combined failure, cheek plates, supporting structure, fatigue, inspection and
proof testing require separate evaluation. Drawing-derived geometry and a full
weld-group model are later extensions.
