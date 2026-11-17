# Plate buckling method coverage

The existing plate-buckling functions are assessed against the equation patterns
in a legacy engineering workbook. The accompanying tests use **illustrative
example** inputs and hand equations. No new resistance implementation is added.
This note establishes method correspondence and known discrepancies; complete
numerical equivalence and engineering design acceptance are not established.

## Existing calculation interfaces

The principal reusable implementation is
`digitalmodel.infrastructure.calculations.plate_buckling`:

| Calculation | Existing interface | Method |
|---|---|---|
| Elastic reference | `ElasticBucklingCalculator` | `B = pi² E (t/b)² / [12(1-nu²)]`; critical stress is `k B` |
| Boundary coefficients | `BucklingCoefficients` | Simply supported and clamped transverse/shear polynomials in `b/l` |
| Reduced slenderness | `SlendernessCalculator` | `lambda = sqrt(characteristic / elastic)` |
| Combined slenderness | `calculate_equivalent_slenderness` | `sqrt(fy/vm * [sum((stress/elastic)^c)]^(1/c))`, with `c = 2-b/l` supplied explicitly |
| Serviceability strength | `calculate_characteristic_resistance_serviceability` | `characteristic / sqrt(1+lambda⁴)` |
| Ultimate strength | `calculate_characteristic_resistance_ultimate` | Serviceability expression below unit slenderness; `characteristic/(sqrt(2)*lambda)` otherwise |
| Longitudinal design resistance | `calculate_dnv_longitudinal_resistance` | Piecewise slenderness reduction times `fy/material_factor` |
| Transverse design resistance | `calculate_dnv_transverse_resistance` | Geometry, column reduction and lateral-pressure response; discrepancy below |
| Pure-shear design resistance | `calculate_dnv_shear_resistance` | Aspect-dependent shear coefficient and piecewise shear reduction |
| Directional and combined usage | `UsageFactorCalculator` | Stress/resistance; square root of quadratic normal/shear interaction |

Table 1. Existing interfaces and the calculation blocks exercised by the tests.

For combined usage, define `x = sigma_x/sigma_xrd`, `y = sigma_y/sigma_yrd`
and `z = tau/tau_rd`. The expression is `sqrt(x²+y²-ci*x*y+z²)`.
Its independent test identity is
`sqrt((1-ci/2)*(x²+y²) + (ci/2)*(x-y)² + z²)`.
The interaction coefficient and the combined-shear resistance must be supplied
for the governing method; the pure-shear reduction is a different curve.

The older `digitalmodel.infrastructure.base_solvers.structural.plate_buckling`
interface also contains geometry conversion, the plane-stress invariant,
characteristic material resistance and a combined-loading routine. Its rounding
and component substitutions limit equivalence. The historical scripts under
`structural/plate_capacity/` contain overlapping equations and scenario-bound
imports; they are not executed by these tests.

## Inputs and units

The tests supply SI values: lengths in m and all moduli, material strengths,
stresses, resistances and lateral pressures in Pa. Poisson ratio, slenderness,
boundary coefficients, interaction coefficients and material factors are
dimensionless. The elastic functions support the repository's existing
`assetutilities.units` interface. Compatible source-unit values can be converted
through the existing `digitalmodel.units.ureg` registry before calling a method.
No alternative unit converter is introduced.

Every design call in the tests supplies its material factor, pressure and
applicable transverse coefficient explicitly. Existing API defaults are not
validated as design selections by this assessment. Compression magnitudes are
used in the source loading patterns; the absolute-value usage API does not
establish a treatment of general signed biaxial stress states.

## References as stated in the source

- `DNV Classification Note 30.1`; `DNV 30.1, Section 3.3.1` for
  characteristic resistance, reduced slenderness and serviceability strength.
- `DNV 30.1, Section 3.3.2` for serviceability usage.
- `DNV 30.1, Section 3.3.4` for ultimate strength and usage.
- `DNV 30.1, Section 3.3.6` for elastic buckling resistance.
- `DNV 30.1, Table 3.1, a/ Non uniform compression`,
  `DNV 30.1, Table 3.1, e/ Non uniform compression`, and
  `DNV 30.1, Table 3.1, d/ Pure shear.` for simply supported coefficients.
- `DNV 30.1, Table 3.2, Fig 8  Non uniform compression`,
  `DNV 30.1, Table 3.2, Fig 10 Non uniform compression`, and
  `DNV 30.1, Table 3.2, Fig 3 Pure shear.` for clamped coefficients.
- `DNV-RP-C201`, with row citations `RP C-201, Section 6.2`,
  `RP C-201, Section 6.3`, `RP C-201, Section 6.4`, and
  `RP C-201, Section 6.5` for longitudinal, transverse, shear and combined
  resistance respectively.
- The material-factor entry states `RP C-201`: **clause not stated in source**.

These are source transcriptions, not a verification against a standard edition.
No equation number or clause has been supplied from memory. Repeated source
labels occasionally point to a different directional section; that inconsistency
does not establish an alternative governing method.

## Known discrepancies and limits

- `BucklingCoefficients.get_transverse_coefficient` documents its argument as l/b; callers and these tests pass b/l.

Strict expected-failure tests expose existing behavior without changing it:

- The transverse method multiplies its internal column reduction into the whole
  resistance after the supplied coefficient has entered the interpolation. The
  source expression has no extra multiplier. The internal reduction also lacks
  the source upper-slenderness branch.
- The older combined routine substitutes longitudinal stress into the transverse
  term. Unequal normal stresses expose the discrepancy.
- The older combined routine fixes its shear reduction at unity. The source uses
  a distinct piecewise combined-shear curve. Pure-shear resistance must not be
  substituted for it outside their common stocky limit.
- The elastic base function does not reject infinite or NaN modulus values.

Expected failures are restricted to a dedicated numerical/validation discrepancy
exception. Import errors, unexpected API exceptions and setup failures remain
ordinary failures. A corrected method produces a strict unexpected pass so the
regression annotation must then be reconsidered.

The older geometry and invariant interfaces round their outputs. Exact unit
conversion can also differ from rounded conversion factors in the source. Cached
block-operand comparisons test individual methods; they do not establish a fully
chained result. Source errors and unpopulated template inputs are not numeric
reference values.

## Input gaps and work remaining

- The standard editions and engineering applicability evidence are not stated.
- The material factor's governing clause and design selection are not stated;
  callers must provide their approved value explicitly.
- Applied stress provenance, solver import qualification, pressure reference,
  fluid properties, gravitational acceleration and boundary-condition selection
  require project-specific evidence. No numerical defaults are introduced here.
- The source states that length exceeds breadth and excludes lateral pressure
  from combined loading. Geometry and load applicability require separate checks.
  The shear coefficient tests also probe the existing API's shorter-plate branch;
  that does not extend the source's stated applicability.
- Existing validation is incomplete across design functions. Passing regression
  tests does not establish rejection of every nonphysical or nonfinite input.
- Intermediate transverse-coefficient and combined-shear reduction selection are
  not exposed as complete reusable functions. Their standalone coverage remains
  incomplete; the source formulas are not copied into a parallel implementation.
- Imported solver files, macros, result selection/envelopes, plots and reporting
  are outside these engineering-method tests. Macro-dependent input refresh is
  not reproduced.

Existing production functions remain unchanged. Resolution of the discrepancies,
coefficient-interface gaps and standard-edition qualification requires a separate
reviewed change.
