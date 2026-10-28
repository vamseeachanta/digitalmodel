# Perry-Robertson column strength analysis

## Summary

The standalone model calculates the mean axial stress at first yield of a
uniform, initially bowed pinned strut. The imperfection parameter is an explicit
input. The accompanying CSV gives strength ratio against column slenderness for
assumed illustrative inputs. It supplies a global flexural column model for
stiffeners; local plate and stiffener failure modes require separate analysis.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-26 | Add the derived column model, independent checks and reproducible CSV for subsequent stiffener studies. |
| 1 | 2026-10-26 | Use title-based citations to meet the public-text requirement while retaining source traceability. |
| 2 | 2026-10-26 | Correct the citation revision date to match its UTC commit date and restore chronological order. |

Revision dates use UTC.

## Basis and symbols

*Aerospace Structures* (2022), section 11.4,
“Geometrically imperfect column”, equations 11.77 and 11.83–11.85,
provides the sinusoidal initial-bow equilibrium solution. The
[open textbook chapter](https://eng.libretexts.org/Under_Construction/Aerospace_Structures_(Johnson)/11%3A_Buckling_of_columns_and_plates)
was inspected. Publication details were confirmed against the
[publisher announcement](https://openvt.lib.vt.edu/2022/03/31/new-open-textbook-aerospace-structures/).
Page to be confirmed by the maintainer: the inspected HTML has equation numbers
but no printed page numbers. The direct textbook landing page returned HTTP 403;
the open chapter and publisher announcement returned HTTP 200.

The historical citation is “The strength of struts” (1925),
*Selected Engineering Papers*, volume 1, number 28,
[DOI](https://doi.org/10.1680/isenp.1925.15138). Bibliographic metadata was
confirmed in the publication registry. The paper text was not used;
page to be confirmed by the maintainer. No empirical coefficient is attributed
to that paper. The calculation basis is the first-principles derivation in
the module docstring, using the textbook elastic solution and first yield.

| Symbol | Meaning | Unit |
| --- | --- | --- |
| E, fy | Elastic modulus and compressive yield stress | Same stress unit |
| L, r, c, e0 | Pinned length, radius of gyration, extreme-fibre distance, initial midspan bow amplitude | Same length unit |
| lambda | Geometric column slenderness L/r | Dimensionless |
| lambda_bar | Reduced slenderness, sqrt(fy/sigma_E) | Dimensionless |
| eta | Imperfection parameter e0 c/r² | Dimensionless |
| q | Mean first-yield stress / fy, also load / squash load | Dimensionless |

The total bow under axial load P is e0/(1-P/PE), where PE is the Euler load.
The moment is P times total bow. Adding direct and bending stress and imposing
first yield gives `(1-q)(1-q lambda_bar²) = eta q`. The smaller root is evaluated
in rationalised form to preserve precision at high slenderness. Both the
derivation and the relation `eta = (e0/L)(c/r) lambda` are shown in the source.

At zero imperfection the limit is the lower of squash and Euler strength.
For nonzero initial bow proportional to length, the strength approaches the
squash limit as length vanishes and the Euler limit as slenderness increases.
Holding eta fixed and nonzero instead gives the short-column limit
`q = 1/(1+eta)`. The zero-slenderness CSV row is a mathematical limit.

## Independent hand calculation

The synthetic pinned strut uses E = 200000.0 MPa, fy = 250.0 MPa,
r = 10.0 mm, c = 25.0 mm and e0 = 2.0 mm. Choose
`L/r = pi sqrt(E/fy)`, so that Euler stress equals yield strength and
`lambda_bar = 1`. Then `eta = 0.5`.

At mean axial stress 125.0 MPa the load is half the Euler load, total bow is
4.0 mm, and bending stress is `125 × 4 × 25 / 100 = 125.0 MPa`.
The maximum compressive stress is therefore 250.0 MPa, giving first yield
and strength ratio 0.5. Test expectations use this direct equilibrium
calculation rather than the implemented quadratic solver.

## CSV and reproduction

[strength_curve.csv](../../../examples/workflows/perry-robertson/strength_curve.csv)
contains 61 points at L/r from 0 to 300 in increments of 5. Each row repeats
the input assumptions: E = 200000.0 MPa, fy = 250.0 MPa, e0/L = 0.001 and
c/r = 2.0. These are assumed illustrative values, not a measured section,
fabrication tolerance or calibrated design curve. The CSV retains numerical
precision for reuse; report displays can round values separately.

Generate it from the repository root, with the repository Python environment:

```sh
PYTHONPATH=src uv run --no-project python scripts/studies/perry_robertson_columns.py \
  --output examples/workflows/perry-robertson/strength_curve.csv
```

The tests regenerate the entire file and compare bytes, then independently
check first-yield stress equilibrium at every positive-slenderness row.
Other tests cover both asymptotic limits, the hand case, zero imperfection,
increasing bow, invalid inputs and floating-point cancellation.

## Limits and way forward

This model assumes uniform elastic stiffness, small slopes, a sinusoidal bow,
centric loading and first yield at an extreme fibre. It excludes residual
stress, plastic redistribution, local buckling, tripping, torsion, connection
restraint and design acceptance factors. For a stiffener study, provide the
effective section properties, bending direction, justified bow and end conditions;
then check the other failure modes separately. Printed source pages and
independent technical review remain for the maintainer before design use.
