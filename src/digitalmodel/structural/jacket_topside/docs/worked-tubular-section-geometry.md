# Worked tubular section geometry

## Summary

A synthetic circular hollow section illustrates the geometric properties returned
by `TubularSection`. Independent annulus integrals provide the expected area and
centroidal second moment of area; their definitions then give elastic section
modulus and radius of gyration. This example covers geometry only.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-25 | Added an independently derived, executable geometry example and uniform scaling check. |
| 2 | 2026-10-25 | Added a scope and validation plan to make the geometry boundary and offline checks explicit. |

## Plan-lite

- Bound the example to the five geometric properties of a synthetic concentric
  annulus; member capacity, joint capacity and standards compliance are excluded.
- Execute the single documented Python block to report values and inch-based
  units, with output confined to the supplied directory.
- Derive expectations independently from annulus integrals and property
  definitions, rather than from the package calls being checked.
- Run the example tests offline alongside the existing tubular-section tests;
  check the independent values, units and uniform scaling.

## Inputs and constants

| Quantity | Value | Basis |
| --- | --- | --- |
| Outside diameter, D | 12.000 in | Assumed synthetic dimension for this example. |
| Uniform wall thickness, t | 0.500 in | Assumed synthetic dimension for this example. |
| Reference length scale, s | 1.0, dimensionless | Assumed reference case. |
| Additional length scale, s | 3.0, dimensionless | Assumed scaling case, applied to both D and t. |
| Mathematical constant, pi | `math.pi`, dimensionless | [Python mathematical constants](https://docs.python.org/3/library/math.html#math.pi). |

All lengths are in inches. No physical constant or unit conversion is used.
The integers and fractions below follow from geometry and integration, rather
than empirical coefficients. The section is a concentric annulus with uniform
wall thickness; ovality, eccentricity and loss of wall thickness are excluded.

## Independent derivation

Take the origin at the common circle centre. The outer radius is R = D/2 = 6 in;
the inner radius is a = R - t = 11/2 in. Thus the inner diameter is 2a = 11 in.
By reflection symmetry the centroid is at the origin. In polar coordinates,
x = rho cos(theta), y = rho sin(theta), and the coordinate Jacobian gives
dA = rho d(rho) d(theta). The radial bounds are a to R and the angular bounds
are zero to one full turn, 2 pi radians.

The annulus area follows directly:

$$
A = \int_0^{2\pi}\int_a^R \rho\,d\rho\,d\theta
  = \pi(R^2-a^2)
  = \pi\left(36-\frac{121}{4}\right)
  = \frac{23\pi}{4}\ \mathrm{in}^2.
$$

The centroidal second moment about the x axis is the area integral of y squared.
The angular integral of sin squared over a full turn is pi; the radial integral
of rho cubed is rho to the fourth power divided by four. Therefore:

$$
I_x = \int_0^{2\pi}\int_a^R
      \rho^3\sin^2\theta\,d\rho\,d\theta
    = \frac{\pi}{4}(R^4-a^4)
    = \frac{\pi}{4}\left(1296-\frac{14641}{16}\right)
    = \frac{6095\pi}{64}\ \mathrm{in}^4.
$$

Rotational symmetry gives the same second moment about the y axis. The returned
`moment_of_inertia` is this planar second moment, not a polar area moment or a
mass moment. The extreme fibre distance is c = R = 6 in. Definitions give:

$$
S = \frac{I_x}{c} = \frac{6095\pi}{384}\ \mathrm{in}^3,
\qquad
r_g = \sqrt{\frac{I_x}{A}} = \frac{\sqrt{265}}{4}\ \mathrm{in}.
$$

| Returned property | Independent expected expression | Display value | Unit |
| --- | --- | --- | --- |
| `inner_diameter` | 2a | 11.0 | in |
| `cross_sectional_area` | 23 pi / 4 | 18.1 | in^2 |
| `moment_of_inertia` | 6095 pi / 64 | 299.2 | in^4 |
| `section_modulus` | 6095 pi / 384 | 49.9 | in^3 |
| `radius_of_gyration` | sqrt(265) / 4 | 4.1 | in |

Display values are rounded to one decimal place; comparisons use the unrounded
expressions above. These expectations are independent of the package calls below.
Under uniform scaling by s, the area measure scales as s squared and the
squared distance in the inertia integral adds another s squared. Consequently,
inner diameter, area, second moment, modulus and gyration radius scale with
powers 1, 2, 4, 3 and 1, respectively.

## Executable example

Save the single Python block below as `tubular_geometry.py`. With the package
installed, run `python tubular_geometry.py --output-dir results` from a writable
directory. To exercise the second case, append `--scale 3.0`. In a source checkout,
set `PYTHONPATH` to its `src` directory. The block runs offline, prints values and
units as JSON, and writes the same JSON only inside the supplied output directory.
No expected result or assertion is embedded in the package calls.

The constructor also requires a material input and provides defaults for other
non-geometric inputs. All such inputs are explicitly set to
[`math.nan`](https://docs.python.org/3/library/math.html#math.nan) to mark them
unused; no material table or material default supplies a result. These values
are placeholders for this geometry-only example, not inputs for member checks.

```python
import argparse
import json
import math
from pathlib import Path

from digitalmodel.structural.jacket_topside import TubularSection

parser = argparse.ArgumentParser(description="Synthetic annulus geometry in inches")
parser.add_argument("--output-dir", type=Path, required=True)
parser.add_argument("--scale", type=float, default=1.0)
args = parser.parse_args()
if not math.isfinite(args.scale) or args.scale <= 0.0:
    parser.error("scale must be finite and positive")

# Synthetic dimensions and scale assumptions are listed in the inputs table.
section = TubularSection(
    outer_diameter=12.0 * args.scale,
    wall_thickness=0.500 * args.scale,
    yield_strength=math.nan,
    elastic_modulus=math.nan,
    effective_length=math.nan,
)
properties = {
    "inner_diameter": {"value": section.inner_diameter, "unit": "in"},
    "cross_sectional_area": {"value": section.cross_sectional_area, "unit": "in^2"},
    "moment_of_inertia": {"value": section.moment_of_inertia, "unit": "in^4"},
    "section_modulus": {"value": section.section_modulus, "unit": "in^3"},
    "radius_of_gyration": {"value": section.radius_of_gyration, "unit": "in"},
}
text = json.dumps(properties, indent=2, allow_nan=False) + "\n"
args.output_dir.mkdir(parents=True, exist_ok=True)
(args.output_dir / "tubular-section-geometry.json").write_text(text, encoding="utf-8")
print(text, end="")
```

## Numerical comparison policy

`test_documented_tubular_geometry` extracts and executes the exact Python block
in a temporary working directory, checks the JSON output and its units, and
compares all five properties with the independent expressions above.
`test_documented_tubular_geometry_scaling` executes the same block at both
assumed scales and checks the dimensional powers. Neither test reconstructs the
package calls. Each invocation has an assumed test-harness budget of 30 seconds.

The relative comparison tolerance is 1e-12, with zero absolute tolerance because
all expected values are nonzero. This is a numerical rounding allowance, not an
engineering acceptance limit: the input dimensions are exactly representable,
the radial power differences are well separated, and only a few
floating-point operations plus pi and a square root are involved. For scale,
[`math.ulp(1.0)`](https://docs.python.org/3/library/math.html#math.ulp) gives the
floating-point spacing at unity; the chosen allowance exceeds that spacing by
several thousand times. Tests retain full precision rather than the display
rounding in the results table.

## Limits

This is a synthetic geometric calculation, with no allowable stress, member
capacity, joint formula, loading assessment or standards-compliance claim.
It does not establish the validity of extreme scales, thin-wall cancellation,
invalid dimensions or non-circular sections. Only the five listed properties
are exercised. The full module report, report builder and index remain outside
this worked-example scope.
