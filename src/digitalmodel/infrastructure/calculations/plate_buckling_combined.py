"""Plate buckling calculation reuse.

The existing `plate_buckling` module evaluates elastic plate buckling,
reduced and equivalent slenderness, characteristic serviceability and ultimate
resistance, directional design resistance, and biaxial utilization. The tests in
`tests/structural/plate_capacity/test_plate_buckling_formula_patterns.py`
exercise these APIs with independently selected illustrative example inputs.
No source geometry, material assignment, load, result, or imported table is
included in those tests.

## Inputs and units

Use metres for thickness, breadth and length, and pascals for modulus, yield
strength, applied stresses, lateral pressure and computed resistances. Poisson
ratio, slenderness, interaction exponent, material factor and utilization are
dimensionless. The existing APIs accept compatible units but do not infer them
from plain floats. The repository's `get_registry()` supplies explicit boundary
conversions. Compression is positive in the source method; the usage and
equivalent-slenderness APIs take stress magnitudes.

## Method and source citations

The following references are recorded exactly as stated in a legacy engineering
workbook. They identify the source's stated basis; the standard originals and
edition-specific applicability have not been independently verified here.

| API | Computation | Source reference |
|---|---|---|
| `BucklingCoefficients` | Coefficients as functions of breadth/length and edge condition | DNV 30.1, Table 3.1, a/ Non uniform compression; DNV 30.1, Table 3.1, e/ Non uniform compression; DNV 30.1, Table 3.1, d/ Pure shear.; DNV 30.1, Table 3.2, Fig 8 Non uniform compression; DNV 30.1, Table 3.2, Fig 10 Non uniform compression; DNV 30.1, Table 3.2, Fig 3 Pure shear. |
| `ElasticBucklingCalculator` | `k*pi^2*E/(12*(1-nu^2))*(t/b)^2` | DNV 30.1, Section 3.3.6 |
| `SlendernessCalculator` | `sqrt(characteristic/elastic)`; generalized stress interaction | DNV 30.1, Section 3.3.1 |
| `UltimateStrengthCalculator.calculate_characteristic_resistance_serviceability` | `f/sqrt(1+lambda^4)` | DNV 30.1, Section 3.3.1 |
| `UltimateStrengthCalculator.calculate_characteristic_resistance_ultimate` | Serviceability expression below unit slenderness; `f/(sqrt(2)*lambda)` otherwise | DNV 30.1, Section 3.3.4 |
| `UsageFactorCalculator.calculate_usage_factor` | Absolute applied stress divided by resistance | DNV 30.1, Section 3.3.2; DNV 30.1, Section 3.3.4 |
| `UltimateStrengthCalculator.calculate_dnv_longitudinal_resistance` | Plate slenderness, piecewise reduction and material factor | RP C-201, Section 6.2 |
| `UltimateStrengthCalculator.calculate_dnv_transverse_resistance` | Column slenderness, geometric and pressure effects; differences described below | RP C-201, Section 6.3 |
| `UltimateStrengthCalculator.calculate_dnv_shear_resistance` | Aspect-dependent shear coefficient and piecewise web reduction | RP C-201, Section 6.4 |
| `dnv_combined_interaction_coefficient` | `1-b/(120*t)` for `b/t <= 120`, zero otherwise | RP C-201, Section 6.5 |
| `dnv_combined_shear_reduction` | Unity through web slenderness 0.8, linear reduction through 1.25, inverse square thereafter | RP C-201, Section 6.5 |
| `UsageFactorCalculator.calculate_biaxial_usage_factor` | Square root of the quadratic normalized-stress interaction, with caller-supplied interaction factor and resistances | RP C-201, Section 6.5 |

Table 1. APIs and the source's stated calculation basis.

## Limits and input gaps

The source assumes length greater than breadth and uniform loading for its
simply supported and clamped cases. It excludes lateral pressure in the
combined-loading check and states a unit load factor for the C201 calculation.
The imported stresses are external analysis inputs; their derivation and load
qualification are outside these functions.

The material factors are assigned inputs. The source labels them only
`RP C-201`: clause not stated in source. No default factor is qualified by this
comparison. The standard edition, material selection, boundary applicability,
pressure-reference convention and original stress-analysis provenance remain
input gaps. Source-assigned modulus, yield strength, depth and pressure constants
are not public defaults established by these tests.

Existing input validation covers selected positive elastic dimensions/modulus,
Poisson-ratio bounds and positive characteristic yield resistance. It is not
complete validation of all design-resistance inputs or nonfinite numbers.
Callers must qualify finite inputs, geometry, material factors, signs and the
applicable standard before using results in an engineering decision.
The combined-check helpers in this module reject nonfinite inputs, nonpositive
dimensions and negative web slenderness.

## Recorded differences and remaining work

The existing transverse design-resistance API multiplies the complete geometric
expression by its column reduction. The source retains an unreduced geometric
contribution. The API also omits the source's separate high-slenderness branch.
Three strict expected-failure tests expose these differences; the existing
implementation is preserved.

The older `PlateBuckling` class also rounds intermediate values, fixes the
combined-shear reduction, and substitutes longitudinal stress into transverse
terms in the combined check. The source contains additional piecewise
combined-shear reduction expressions; legacy script fragments print branches
without selecting them into the final resistance. Those fragments are not a
qualified callable implementation. The helpers in this module provide the
selected combined-shear reduction and the interaction coefficient, including
zero clipping above breadth/thickness 120. The existing standalone shear API
retains its different reduction branches; the combined reduction applies to
`fy/(sqrt(3)*material_factor)`, rather than multiplying an already reduced
standalone resistance. The generic interaction API combines independently
supplied resistances and the interaction coefficient.

Exact unit conversion can differ from approximate source multipliers. A square
aspect ratio also reaches an omitted false branch in the source's nested shear
coefficient expression; the existing API returns a numeric coefficient there.
Zero applied stress makes the source's equivalent slenderness undefined, whereas
the existing API returns zero. These differences do not justify changing either
the existing API or a comparison tolerance silently.

Complete source-cell equivalence is not established. Imported-data preparation,
stress aggregation, geometry extraction, charting, VBA import routines, source
error caches, and the method differences above remain outside a qualified
end-to-end calculation.
"""

import math


def dnv_combined_interaction_coefficient(breadth_m: float, thickness_m: float) -> float:
    """RP C-201, Section 6.5 (as stated in a legacy engineering workbook; not verified against the standard)"""
    if not math.isfinite(breadth_m) or breadth_m <= 0:
        raise ValueError("breadth_m must be positive and finite")
    if not math.isfinite(thickness_m) or thickness_m <= 0:
        raise ValueError("thickness_m must be positive and finite")
    ratio = breadth_m / thickness_m
    return 0.0 if ratio > 120 else 1.0 - ratio / 120


def dnv_combined_shear_reduction(web_slenderness: float) -> float:
    """RP C-201, Section 6.5 (as stated in a legacy engineering workbook; not verified against the standard)"""
    if not math.isfinite(web_slenderness) or web_slenderness < 0:
        raise ValueError("web_slenderness must be nonnegative and finite")
    if web_slenderness > 1.25:
        return (1.0 / web_slenderness) ** 2
    if web_slenderness > 0.8:
        return 1.0 - 0.8 * (web_slenderness - 0.8)
    return 1.0
