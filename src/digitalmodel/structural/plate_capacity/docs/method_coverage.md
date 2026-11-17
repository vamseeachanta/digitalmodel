# Plate buckling method coverage

A legacy engineering calculation contains longitudinal, transverse, shear and
combined plate checks under two distinct methods. Existing implementations are
retained; this note does not establish their equivalence or design qualification.

The source states these references:

- `DNV 30.1, Section 3.3.1`: characteristic material strengths, reduced
  slenderness and serviceability resistance.
- `DNV 30.1, Section 3.3.2`: serviceability usage.
- `DNV 30.1, Section 3.3.4`: ultimate resistance and usage.
- `DNV 30.1, Section 3.3.6`: elastic buckling resistance.
- `DNV 30.1, Table 3.1, a/ Non uniform compression`,
  `DNV 30.1, Table 3.1, e/ Non uniform compression`, and
  `DNV 30.1, Table 3.1, d/ Pure shear.`: simply supported coefficients.
- `DNV 30.1, Table 3.2, Fig 8  Non uniform compression`,
  `DNV 30.1, Table 3.2, Fig 10 Non uniform compression`, and
  `DNV 30.1, Table 3.2, Fig 3 Pure shear.`: clamped-side coefficients.
- `RP C-201, Section 6.2`, `RP C-201, Section 6.3`,
  `RP C-201, Section 6.4`, and `RP C-201, Section 6.5`: longitudinal,
  transverse, shear and biaxial-with-shear resistance respectively.

These are source-stated locators, without independent verification of editions
or normative text. Several entries state only `RP C-201`: **clause not stated in
source**. No missing locator or edition is inferred.

| Calculation | Existing implementation | Limits |
| --- | --- | --- |
| Elastic plate compression and reduced slenderness | `structural_analysis.buckling.PlateBucklingAnalyzer` | Simply supported; long-plate coefficient approximation |
| Square elastic rigidity normalization | `elastic_buckling.square_plate_elastic_buckling_stress` | Perfect isotropic elastic square plate |
| Minimum rectangular modal coefficient | `classical_elastic_buckling.rectangular_plate_elastic_buckling_stress` | Integer longitudinal modes; different from the long-plate approximation |
| Plane stress equivalent stress | `structural_analysis.models.StressState.von_mises` | Complete stress-state invariant; no buckling resistance |
| Shear coefficient | `structural_analysis.girder_web_frame.web_shear_buckling_coefficient` | Coefficient only; not an ultimate shear capacity |
| Quadratic reduction root | `perry_robertson.perry_robertson_strength_ratio` | Explicit dimensionless imperfection; only the algebraic middle branch is shared |
| Combined compression/shear check | `structural_analysis.buckling.PlateBucklingAnalyzer.check_plate_buckling` | Johnson-Ostenfeld correction and linear/quadratic usage; transverse stress is currently ignored |
| Serviceability/ultimate and directional resistance formulas | `plateBucklingCal_G.py` and related scripts | Parameter-module imports, printed branch selections and reused globals prevent treatment as a validated callable API; uses π ≈ 3.14159 and rounds longitudinal reduced slenderness to two decimals, so results differ from closed-form values |

Table 1. Existing method coverage and restrictions.

The callable structural analyzer uses lengths in mm and stresses in MPa.
The classical elastic and reduction helpers use consistent caller-selected units
or dimensionless arguments as documented by their APIs. These interfaces do not
share one implicit unit system. Existing boundary conversion helpers shall be
used before entering the intended API.

The source method uses elastic stress proportional to
`k*E*(t/b)^2/(1-nu^2)`, directional slenderness `sqrt(fk/fE)`, serviceability
resistance `fk/sqrt(1+lambda^4)` and a separate ultimate branch. Its combined
equivalent-slenderness and directional quadratic interaction checks differ from
the callable analyzer's Johnson-Ostenfeld method. Substituting one result for
the other requires a resolved method decision.

Input gaps include unspecified standard editions, missing clauses for some
material factors, lateral-pressure conventions, boundary-condition evidence,
stress sign/envelope provenance and externally imported analysis stresses.
Material factors and environmental properties remain caller inputs. The source
limits plate length to at least its breadth and excludes lateral pressure from
its combined-loading description, although its transverse resistance contains
a pressure reduction. That inconsistency requires engineering review.

The regression tests use labelled illustrative examples with hand formulas and
limiting identities. Strict expected failures expose the existing omission of
transverse stress and missing material-factor validation. No new calculation
method or standards-derived coefficient is introduced. Full source-method
equivalence, clamped-side API coverage and corrections to the legacy scripts
remain unresolved.
