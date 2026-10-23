# Catenary analysis from a vertical span

## Summary

`CatenaryAnalyzer.solve_catenary` takes a vertical rise above a horizontal
touchdown tangent. It returns the required suspended length, horizontal reach
of the suspended portion, and fairlead tension. The example below checks these
quantities using synthetic inputs and independent hand calculations.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-22 | Document vertical-rise geometry and independent reference cases to prevent coordinate substitution. |

## Method and sources

The basis is *Classical Mechanics*, section 18.3, published in 2017, equations
18.3.4–18.3.6 and 18.3.9, available from
[LibreTexts](https://phys.libretexts.org/@go/page/7046).
The textbook ordinate is measured from the directrix. Setting that ordinate
to `a + z` places the origin at touchdown:

```text
w = weight_water * g / 1000       submerged weight, kN/m
a = H / w                        catenary parameter, m
z = a * (cosh(x/a) - 1)          vertical rise, m
s = sqrt(z² + 2*a*z)             suspended arc length, m
x = a * acosh(1 + z/a)           suspended horizontal reach, m
  = a * asinh(s/a)
V = w*s                          vertical fairlead force, kN
T = sqrt(H² + V²) = H + w*z      total fairlead tension, kN
theta = atan2(V, H)              fairlead angle above horizontal
H = w*(s² - z²)/(2*z)            inverse relation for z > 0
```

The factor of `1000` converts newtons to kilonewtons. The existing mass-per-length
API retains `g = 9.81 m/s²`, obtained by rounding the
[NIST 2022 CODATA standard acceleration of gravity](https://physics.nist.gov/cgi-bin/cuu/Value?gn)
to two decimal places. This convention is unchanged by the geometry correction.
At fixed `z` and `w`, equation 18.3.9 gives `dT/dH = 1`; the inverse-tension
iteration uses that derivative. A target `T <= w*z` cannot give positive `H`.

The implementation uses `asinh(s/a)` rather than forming `1 + z/a`, retaining
the increment at shallow rises. It requires no exponential approximation for
deep rises. All coefficients in these expressions follow from the cited
identities; there are no empirical fitting factors.

## Worked example

All inputs in this example are synthetic mathematical data, rather than
material properties or an engineering design. Choose `w = 1 kN/m`, vertical
rise `z = 10 m`, suspended length `s = 30 m`, and total length `L = 100 m`.
The hand calculation is:

```text
a = (30² - 10²)/(2*10) = 40 m
H = 1*40 = 40 kN
T = H + w*z = 50 kN
cosh(x/40) = 1 + 10/40 = 5/4
exp(x/40) = 2, so x = 40*ln(2) = 27.7 m
tan(theta) = s/a = 3/4, so theta = 36.9 degrees
grounded length = L - s = 70 m
```

Run the corresponding calculation from a source checkout:

```python
from digitalmodel.subsea.mooring_analysis import (
    CatenaryAnalyzer,
    LineType,
    MooringLineProperties,
)

# Synthetic inputs from the hand calculation above. Mass conversion uses
# NIST 2022 CODATA gravity rounded to the existing API convention.
line = MooringLineProperties(
    line_type=LineType.CHAIN,
    length=100.0,
    diameter=0.0,  # Unused placeholder; no physical chain is specified.
    mbl=0.0,       # Unused placeholder; strength is not evaluated.
    weight_water=1000.0 / 9.81,
    ea=0.0,        # Unused placeholder; stretch is not evaluated.
)
analyzer = CatenaryAnalyzer(water_depth=10.0)
result = analyzer.solve_catenary(line, horizontal_tension=40.0)
print(f"Suspended length: {result.arc_length:.1f} m")
print(f"Horizontal reach: {result.horizontal_distance:.1f} m")
print(f"Fairlead tension: {result.fairlead_tension:.1f} kN")
```

Expected output:

```text
Suspended length: 30.0 m
Horizontal reach: 27.7 m
Fairlead tension: 50.0 kN
```

A second independent case uses `w = 2 kN/m`, `z = 20 m`, `s = 40 m`:
`a = (1600 - 400)/40 = 30 m`, `H = 60 kN`, `T = 100 kN`, and
`x = 30*ln(3) = 33.0 m`. These cases have different rise-to-parameter ratios.
The regression tests derive their expected values directly from these hand
calculations, with the 0.1 % relative acceptance limit specified in issue #2561.

## Applicability and remaining work

- The model is inextensible, with uniform positive submerged weight and a
  horizontal tangent at touchdown. It excludes buoyant lines, distributed
  horizontal loading, seabed friction and a general pair of suspended endpoints.
- `horizontal_distance` covers only the suspended part. For a straight,
  frictionless grounded portion, add `grounded_length` to obtain total reach.
- `arc_length` is the required suspended length for the prescribed `H` and `z`.
  It may exceed `line.length`; a zero reported grounded length does not establish
  feasibility. A general endpoint solution is required when touchdown is absent.
- `calculate_stiffness` and `check_touchdown` retain their existing approximations.
  This geometry correction does not qualify those methods. Their derivatives
  and reach limits require separate analysis before design use.
- Strength, elastic extension and design acceptance are **NOT_EVALUATED** by
  this worked example. They require supplied material data and applicable criteria.
