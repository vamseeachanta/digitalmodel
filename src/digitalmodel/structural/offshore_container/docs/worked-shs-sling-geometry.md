# Worked SHS section and sling geometry example

## Summary

This synthetic example calculates the area, centroidal second moment of area and
elastic section modulus of an ideal square hollow section (SHS), then the sling
angle to vertical for a centred four-corner lift. The hand results are
`A = 3.0e-2 m²`, `I = 1.25e-4 m⁴`, `W = 1.25e-3 m³` and
`v = π/4 rad = 45.0°`. Both the metre constructor and `SHS.from_mm` are exercised
against these same results. The example establishes geometry and unit conversion
only.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-25 | Added synthetic inputs, hand derivations and executable checks to connect the geometry methods to their answers. |

## Inputs and assumptions

All dimensions below are **synthetic assumptions**, chosen for simple hand
arithmetic. They are not measured dimensions or a catalogue section. The section
is uniform, concentric and sharp-cornered, with no corner radii. The sling
attachments lie at the four corners of a horizontal rectangle; the common link
is directly above its centre. The height is measured from the attachment plane.

| Quantity | Symbol | Explicit SI input (m) | Source |
| --- | --- | --- | --- |
| Outer section width | `b` | `0.2` | Synthetic assumption A1 |
| Wall thickness | `t` | `0.05` | Synthetic assumption A1 |
| Attachment rectangle length | `L` | `6.0` | Synthetic assumption A2 |
| Attachment rectangle width | `B` | `8.0` | Synthetic assumption A2 |
| Link height above attachment plane | `h` | `5.0` | Synthetic assumption A2 |

Table 1. Synthetic dimensions; A1 defines the section and A2 defines the lift geometry.

For the alternative constructor, `b = 200 mm` and `t = 50.000 mm` are the same
A1 dimensions. The conversion `1 mm = 10⁻³ m` is exact: the SI prefix milli means
`10⁻³` ([BIPM SI prefixes](https://www.bipm.org/en/measurement-units/si-prefixes)).
The degree conversion is `1° = (π/180) rad`
([SI Brochure, section 4, table 8](https://www.bipm.org/documents/20126/41483022/SI-Brochure-9.pdf)).
No physical constants are needed. The coefficients in the following equations
come from the geometry and integration shown here.

## Derivation

### D1: section properties

Subtract the concentric inner rectangle from the outer rectangle. Two opposing
walls give `b_i = b − 2t = 0.2 − 2 × 0.05 = 0.1 m`. The area is therefore

`A = b × b − b_i × b_i = 0.04 − 0.01 = 3.0e-2 m²`.

For a solid rectangle of width `w` and height `d`, integration about its centroid
across the height gives

`I = ∫[−d/2, d/2] w y² dy = w [y³/3][−d/2, d/2] = w d³/12`.

For each square, width and height are equal. Both rectangles share the centroid,
so no parallel-axis correction is required. About either centroidal axis
parallel to a side,

`I = (b⁴ − b_i⁴)/12 = (0.0016 − 0.0001)/12 = 1.25e-4 m⁴`.

The extreme fibre is at `c = b/2 = 0.1 m`. By the elastic section-modulus
definition `W = I/c`,

`W = 1.25e-4 / 0.1 = 1.25e-3 m³`.

These hand values use only assumption A1 and the displayed integrals; they are
independent of the implementation being exercised.

### D2: sling angle to vertical

From the plan centre to any corner, the horizontal components are `L/2 = 3 m`
and `B/2 = 4 m`. The right triangle in plan gives

`r = √[(L/2)² + (B/2)²] = √(9 + 16) = 5 m`.

In the vertical plane through that corner and the link, `r` is opposite the
angle to vertical and `h` is adjacent. Hence `tan(v) = r/h = 5/5 = 1`.
The equal legs of this right triangle give the exact angle
`v = π/4 rad = 45.0°`. Each of the four corners has this angle by symmetry.
The dimensions come from A2; the degree value uses the cited SI conversion.

## Executable example

With the repository source on `PYTHONPATH`, save the following block as
`example.py` and run `python example.py` from a supplied temporary directory.
It needs only the Python standard library and the repository module. It performs
no network access and writes JSON to standard output, with no output files.
All dimensional arguments to the metre constructor and geometry function are
explicitly in metres; only `SHS.from_mm` takes millimetres.

```python
import json
import math

from digitalmodel.structural.offshore_container.dnv_2_7_1 import (
    SHS,
    sling_angle_to_vertical,
)

# Synthetic assumption A1, expressed explicitly in SI units.
section_m = SHS(b=0.2, t=0.05)
# A1 in millimetres; milli = 10**-3 (BIPM SI prefixes, cited above).
section_mm = SHS.from_mm(b_mm=200.0, t_mm=50.0)
# Synthetic assumption A2: attachment rectangle and link height, all in metres.
angle_rad = sling_angle_to_vertical(length=6.0, width=8.0, lift_height=5.0)


def properties(section):
    return {
        "b_m": section.b,
        "t_m": section.t,
        "area_m2": section.area,
        "inertia_m4": section.inertia,
        "modulus_m3": section.modulus,
    }


print(json.dumps({
    "metres": properties(section_m),
    "millimetres": properties(section_mm),
    "angle_rad": angle_rad,
    "angle_deg": math.degrees(angle_rad),  # SI Brochure table 8: 180/pi deg/rad.
}, sort_keys=True))
```

The executable test extracts this sole Python block unchanged and executes it
from a temporary working directory. It checks both constructor routes against
D1 and the angle against D2. Section results use relative tolerance `1e-12`,
with no absolute allowance; angles use absolute tolerance `1e-12` in their
respective radian and degree units, with no relative allowance. These tolerances
allow floating-point roundoff in exact synthetic arithmetic and remain far below
a unit conversion or geometric formula error. They do not represent measurement
uncertainty or a design acceptance margin.

## Limits and way forward

The second moment and modulus apply to either centroidal axis parallel to a
section side, for the ideal sharp-cornered section only. Real section corner
radii, fabrication tolerances, corrosion and material properties are not
represented. Geometry requires a positive wall thickness with `2t < b` and a
positive link height; the chosen assumptions meet those conditions. This example
does not establish input validation for other dimensions.

The sling calculation gives an angle only. It does not determine tensions,
load sharing, attachment strength, section resistance or structural suitability.
`utilization()` and design factors are outside this example, and no
standards-compliance conclusion is made. No licensed table values are used.
The full module report, report builder and index entry remain open under
issue #3325; those require their own engineering basis and review.
