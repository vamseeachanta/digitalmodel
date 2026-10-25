# Synthetic circular canopy drag worked example

## Summary

This example calculates drag for an assumed circular projected canopy using
`calculate_drag_force`. A separate calculation in consistent SI units checks
both returned force units. A second speed checks the quadratic speed relation.
All empirical inputs are synthetic assumptions supplied by the caller; none is
a measured or qualified canopy coefficient. The example runs offline.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| A | 2026-10-25 | Added a synthetic drag example and executable SI checks to document the calculation and its unit basis. |

## Inputs

| Input | Value | Basis |
| --- | --- | --- |
| Projected circular diameter | 5.0 ft | Synthetic geometric assumption; diameter of the projected disk, not a fabric dimension. |
| Initial relative flow speed | 40.0 mph | Synthetic assumption. |
| Changed relative flow speed | 60.0 mph | Synthetic assumption for the speed-scaling check. |
| Drag coefficient, `cd` | 1.1 | Assumed, based on previous experience; used only as a synthetic input referenced to the projected disk area. Not a qualified coefficient. |
| Opening multiplier, `cx` | 1.25 | Assumed, based on previous experience; synthetic algebraic multiplier, not a predicted transient peak. |
| Density, `rho` | 0.0023 slug/ft³ | Assumed, based on previous experience; synthetic constant density, not an atmospheric reference condition. |

The inputs define the arithmetic case only. They do not characterize any
particular canopy or establish applicability to equipment.

## Derivation and unit basis

For a circle of radius `r = d/2`, integration of concentric rings gives
`A = integral(2*pi*r dr, r=0..d/2) = pi*d²/4`. This geometric identity defines the
projected reference area. The mathematical constant `pi` is provided by
Python's `math.pi`; the integers in the geometry are algebraic factors.

The [NASA drag equation](https://www1.grc.nasa.gov/beginners-guide-to-aeronautics/drag-equation/)
defines `F_drag = cd*rho*V²*A/2`. Multiplying by the caller's assumed `cx` gives
the module's expression `F = rho*V²*cd*A*cx/2`. The factor `1/2` belongs to the
drag-law definition of dynamic pressure; `cx` is an additional assumption.
NASA also explains that a drag coefficient depends on the reference area.
Here that area is the projected disk throughout.

Exact conversion definitions and their sources are:

| Definition | Source |
| --- | --- |
| 1 ft = 0.3048 m; 1 international mile = 1609.344 m | [NIST SP 811, Appendix B.8, foot and mile entries](https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8); bold factors are exact. Survey units are not used. |
| 1 h = 3600 s | [NIST SP 811, Chapter 5, Table 6](https://www.nist.gov/pml/special-publication-811/nist-guide-si-chapter-5-units-outside-si). |
| 1 lbf = 4.4482216152605 N exactly, at conventional standard gravity | [NIST SP 811, footnote 23](https://www.nist.gov/pml/special-publication-811/nist-guide-si-footnotes#f23), which specifies standard gravity as 9.80665 m/s². This is a unit definition, not a local gravity assumption. |
| 1 N = 1 kg m/s² | [NIST SP 811, Chapter 4, Table 3](https://www.nist.gov/pml/special-publication-811/nist-guide-si-chapter-4-two-classes-si-units-and-si-prefixes). |

Thus `1 mph = 1609.344/3600 m/s = 0.44704 m/s` exactly, and
`1 mile = 1609.344/0.3048 ft = 5280 ft` exactly. From the coherent mass unit
`1 slug = 1 lbf s²/ft` and the definitions above,
`slug_kg = 4.4482216152605/0.3048`. This is consistent with the rounded slug
entry in NIST Appendix B.8. Density must be converted as mass per volume:
`rho_SI = rho_slug_ft3 * slug_kg / 0.3048³`.

The independent SI calculation uses:

- `d_SI = 5.0 * 0.3048 m`;
- `V_SI = speed_mph * 1609.344 / 3600 m/s`;
- `A_SI = pi * (d_SI/2)² m²`;
- `rho_SI = 0.0023 * (4.4482216152605/0.3048) / 0.3048³ kg/m³`;
- `F_SI = rho_SI * V_SI² * 1.1 * A_SI * 1.25 / 2 N`;
- `F_lbf = F_SI / 4.4482216152605`.

No module conversion or area helper is used in that derivation. The dimensions
are `(kg/m³)*(m²/s²)*m² = kg m/s² = N`. Equivalently, the module's coherent
units give `(slug/ft³)*(ft²/s²)*ft² = slug ft/s² = lbf`.

| Speed (mph) | Independent SI force (N) | Independent force (lbf) | Returned `force_n` (N) | Returned `force_lbs` (lbf) |
| --- | --- | --- | --- | --- |
| 40.0 | 475.3 | 106.9 | 475.3 | 106.9 |
| 60.0 | 1069.5 | 240.4 | 1069.5 | 240.4 |

Table values are derived from the equations and assumptions above and displayed
to one decimal place; assertions use unrounded arithmetic. The module currently
uses `4.44822 N/lbf` in
[`parachute_drag.py`](../parachute_drag.py), a rounded version of the exact NIST
factor. The SI relative tolerance is `5e-7`: it exceeds
`abs(4.44822/4.4482216152605 - 1)` while remaining below `1e-6`.
The lbf and speed-ratio checks use relative tolerance `1e-12` for floating-point
roundoff only. These are numerical consistency tolerances, not physical accuracy
or coefficient uncertainty claims; absolute tolerance is zero.

## Executable example

Save the single Python block as a script. With the repository's `src` available
on `PYTHONPATH` and its declared PyYAML dependency available, run it with an
existing writable temporary directory as its sole argument. It imports the
module normally, needs no network and writes only `results.json` inside that
directory. It also prints the returned forces rounded to one decimal place.
The regression test extracts and executes this block verbatim from a separate
process whose working directory is the supplied temporary directory.

```python
import json
import math
from pathlib import Path
import sys

from digitalmodel.structural.parachute.parachute_drag import calculate_drag_force

# Synthetic assumptions from the Inputs table, held fixed for both speeds.
inputs = {
    "chute_diameter_ft": 5.0,
    "cd": 1.1,
    "cx": 1.25,
    "rho": 0.0023,
}
speeds_mph = (40.0, 60.0)

# Exact NIST definitions cited in the unit-basis table above.
metres_per_foot = 0.3048
metres_per_mile = 1609.344
seconds_per_hour = 3600.0
newtons_per_lbf = 4.4482216152605
kilograms_per_slug = newtons_per_lbf / metres_per_foot

output_dir = Path(sys.argv[1]).resolve()
if not output_dir.is_dir():
    raise ValueError("Supply an existing temporary output directory")

# Circle geometry and density conversion, independent of module helpers.
diameter_m = inputs["chute_diameter_ft"] * metres_per_foot
area_m2 = math.pi * (diameter_m / 2) ** 2
density_kg_m3 = inputs["rho"] * kilograms_per_slug / metres_per_foot**3
rows = []
for speed_mph in speeds_mph:
    speed_m_s = speed_mph * metres_per_mile / seconds_per_hour
    # NASA drag law, with the caller's assumed opening multiplier.
    independent_n = (
        density_kg_m3 * speed_m_s**2 * inputs["cd"] * area_m2 * inputs["cx"] / 2
    )
    result = calculate_drag_force(speed_mph=speed_mph, **inputs)
    # Tolerances are justified above; neither comparison uses rounded output.
    assert math.isclose(
        result.force_lbs, independent_n / newtons_per_lbf,
        rel_tol=1e-12, abs_tol=0.0,
    )
    assert math.isclose(result.force_n, independent_n, rel_tol=5e-7, abs_tol=0.0)
    rows.append({
        "speed_mph": speed_mph,
        "force_lbs": result.force_lbs,
        "force_n": result.force_n,
        "independent_force_n": independent_n,
    })

expected_ratio = (speeds_mph[1] / speeds_mph[0]) ** 2
for unit in ("force_lbs", "force_n"):
    assert math.isclose(
        rows[1][unit] / rows[0][unit], expected_ratio, rel_tol=1e-12, abs_tol=0.0
    )

(output_dir / "results.json").write_text(json.dumps(rows, indent=2) + "\n")
for row in rows:
    print(
        f'{row["speed_mph"]:.1f} mph: '
        f'{row["force_lbs"]:.1f} lbf, {row["force_n"]:.1f} N'
    )
```

## Limits

This is a drag-law arithmetic example with a circular projected area, constant
density and fixed caller-supplied empirical inputs. The opening multiplier does
not resolve inflation history, fluid-structure interaction or time-varying loads.
The speed-squared relation follows only while the other inputs stay fixed.
Agreement between units establishes numerical consistency of this case only.
No product data, equipment selection, vehicle performance, frame analysis, CAD
or safety recommendation is included. The full module report, builder and index
remain outside this worked-example slice.
