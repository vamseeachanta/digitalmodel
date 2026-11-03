# Classical elastic buckling of rectangular plates

The module computes the elastic buckling coefficient and critical compressive
stress of a perfect, homogeneous isotropic thin rectangular plate, simply
supported on all four edges. Length `a` is parallel to uniform compression;
width `b` is transverse. Transverse in-plane expansion is free. The coefficient
retains the finite aspect-ratio dependence and minimizes over integer modes.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-26 | Added the modal relation, hand checks and coefficient CSV for comparisons. |
| 2 | 2026-10-26 | Removed personal-name attribution from historical references for public-text compliance. |

## Formulation and sources

The primary basis is the first-principles derivation in
[`classical_elastic_buckling.py`](../../../src/digitalmodel/structural/plate_capacity/classical_elastic_buckling.py).
With `r=a/b` and positive integer longitudinal half-wave count `m`, the result is

```text
D = E*t^3 / [12*(1-nu^2)]
k(r,m) = (m/r + r/m)^2
k(r) = min over positive integer m of k(r,m)
sigma_cr = k(r)*pi^2*E / [12*(1-nu^2)] * (t/b)^2
```

Substituting the sine mode into the linear plate equilibrium equation gives
this expression; the transverse half-wave count minimizing the load is one.
Only the two positive integers bracketing `r` need comparison. Equating adjacent
branches gives the transition `r=sqrt(m*(m+1))`. The identity
`k=4+(m/r-r/m)^2` shows that 4 is the lower bound, attained at every positive
integer aspect ratio. For a finite noninteger aspect ratio, k exceeds 4 and
approaches 4 as the plate becomes long.

The accessible corroborating source is [MIT OpenCourseWare, 2.080J Structural
Mechanics, Fall 2013, Lecture 11](https://ocw.mit.edu/courses/2-080j-structural-mechanics-fall-2013/resources/mit2_080jf13_lecture11/),
Section 11.2, pages 11-3–11-4, equations 11.11–11.17. The institutional PDF was
read on 2026-10-26. It gives the mode relation, the integer-mode requirement and
the long-plate minimum. The rigidity normalization reuses the existing sourced
square-plate function; its source is MIT Lecture 4, equation 4.60.

Historical references, with explicit access limits:

- *On the Stability of a Plane Plate under Thrusts in its own
  Plane, with Applications to the “Buckling” of the Sides of a Ship*,
  Proceedings of the London Mathematical Society, s1-22(1), pages 54–67,
  [DOI registration](https://doi.org/10.1112/plms/s1-22.1.54).
  The historical attribution is 1891; the DOI registration lists November 1890.
  Metadata was confirmed through the public DOI resolver on 2026-10-26;
  the publisher returned HTTP 403 for the paper text. Formula page to be
  confirmed by the maintainer. No formula or worked value is taken from the
  inaccessible paper.
- *Theory of Elastic Stability*, 2nd ed.,
  McGraw-Hill, 1961, plate chapter; page to be confirmed by the maintainer.
  The [Open Library edition record](https://openlibrary.org/books/OL6271419M)
  confirms the second edition, publisher and year; it was read on
  2026-10-26. MIT Lecture 11 also recommends this title. The formula page has
  not been inspected. The derivation and accessible lecture,
  rather than an unverified textbook page, establish the implemented relation.

## Hand checks and use

The new tests use independent rational arithmetic:

| a/b | Governing m | Hand expression for k |
| --- | --- | --- |
| 0.5 | 1 | `(2+1/2)^2 = 25/4` |
| 1 | 1 | `(1+1)^2 = 4` |
| 1.41 | 1 | `(100/141+141/100)^2 = 892874161/198810000` |
| 2 | 2 | `(1+1)^2 = 4` |

Table 1. Finite aspect-ratio checks. The decimal 1.41 is not treated as sqrt(2).

At the exact first transition `sqrt(2)`, both modes give `k=9/2`.
The tests also cover a ratio of 1.45, where rounding `r` to the nearest integer
would choose the wrong mode, and transitions at higher half-wave counts.
For a synthetic stress check, `E=120000 MPa`, `nu=0`, `t=1 mm`, `b=100 mm`
give `sigma_cr=k*pi^2 MPa`. Equal positive and negative Poisson ratios give the
same stress through the squared term. Inputs must satisfy `E,t,a,b>0`, be finite,
and have `-1<nu<0.5`; unrepresentable results raise `ValueError`.

```python
from digitalmodel.structural.plate_capacity.classical_elastic_buckling import (
    plate_buckling_coefficient,
    rectangular_plate_elastic_buckling_stress,
)

k = plate_buckling_coefficient(1.41)
stress_mpa = rectangular_plate_elastic_buckling_stress(
    youngs_modulus=120000.0,
    poisson_ratio=0.0,
    thickness=1.0,
    length=141.0,
    width=100.0,
)
```

## Comparison data

[`data/classical-elastic-coefficients.csv`](data/classical-elastic-coefficients.csv)
contains 198 data rows: `a/b` from 0.25 to 10 in increments of 0.05, plus 1.41
and the first transition `sqrt(2)`. The columns `aspect_ratio`, `half_waves` and
`k` are dimensionless. Stored float precision supports downstream calculations;
this file is coefficient data, not a rounded engineering report.

Regenerate from the repository root:

```sh
PYTHONPATH=src uv run --no-project python scripts/studies/generate_classical_plate_buckling.py \
  --output docs/domains/plate-buckling/data/classical-elastic-coefficients.csv
```

The tests regenerate into a temporary directory, compare bytes with the stored
CSV, compare every row with the module, and check the four specified ratios
against independent hand values.

## Compatibility and limitations

Pull request 3387 was open when checked on 2026-10-26, so this module is added
alongside its proposed rectangular extension. The existing square-plate API is
reused for rigidity normalization. No existing buckling or panel solver is
changed. Integration with that proposed extension remains a maintainer decision.

This is linear elastic bifurcation analysis. Thin-plate and elastic applicability
must be established separately. Yielding, residual stresses, imperfections,
postbuckling reserve, effective width, shear, biaxial compression, stiffeners,
and design acceptance factors are outside the model. The coefficient CSV alone
does not represent ultimate capacity or acceptance under a design standard.
