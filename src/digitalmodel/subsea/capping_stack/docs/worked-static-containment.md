# Static containment analysis: worked example

## Summary

Public well-control worked-example inputs [W], converted independently to SI,
give a hydrostatic head of 41.189 MPa and shut-in wellhead pressure of 2.192 MPa.
Against the separately published rating of 103.421 MPa [R], utilisation is
0.0212, below the inclusive limit of 1.00, so `is_contained = True`.
The derived variants exercise equality, exceedance and zero clamping. These
are static arithmetic results, with no equipment or subsea-well qualification.

## Revision history

| Revision | Date       | What changed and why                                                                                                     |
| -------- | ---------- | ------------------------------------------------------------------------------------------------------------------------ |
| Initial  | 2026-10-25 | Added a computed example to explain the static comparison.                                                               |
| 1        | 2026-10-25 | Replaced invented base inputs with public worked-example inputs and cited rating; showed construction of boundary cases. |

## Inputs and sources

[W] [AAPG, Development Geology Reference Manual (1992), Wellsite math](https://wiki.aapg.org/Wellsite_math),
sections **Hydrostatic pressure**, **Mud weight** and **Kill weight of mud**,
provides the public worked-example basis. The first example supplies mud weight
and depth; the adjacent balanced-mud example supplies formation pressure at that
depth. These are published teaching inputs, not measurements from a real well.

[R] [OSRL, Capping Stacks, online specification](https://www.osrl.com/services/response/subsea-response/capping/),
section **Key facts**, publishes the pressure rating used as the scalar
comparison limit. Combining this rating with [W] illustrates the calculation;
it does not establish that any particular stack is suitable for the example.

| Input                        | Symbol | Source value     | Basis                     |
| ---------------------------- | ------ | ---------------- | ------------------------- |
| Constant drilling-mud weight | rho    | 11.5 lb/U.S. gal | [W], Hydrostatic pressure |
| Vertical column depth        | h      | 10,000 ft        | [W], Hydrostatic pressure |
| Formation pressure           | p_res  | 6,292 psi        | [W], Mud weight           |
| Positive pressure rating     | R      | 15,000 psi       | [R], Key facts            |

Table 1. Public input basis; the published depth is treated as vertical for this static column.

[G] The independent source for the standard gravitational acceleration is
[NIST, standard acceleration of gravity](https://physics.nist.gov/cgi-bin/cuu/Value?gn),
which states g_n = 9.80665 m/s² exactly (accessed 2026-10-25).
This is the standard-gravity convention, not a measured local acceleration.
The example adopts it as constant along the column.

[C] [NIST SP 811 (2008 edition), Appendix B.8, Conversion Factors](https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8)
supplies the inch, foot and pound definitions. [NIST Handbook 133 (2026 edition),
Appendix E, General Tables of Units of Measurement](https://www.nist.gov/document/2026-hb-133-appendix-e)
defines the U.S. liquid gallon as exactly 231 cubic inches. The conversions are
1 in = 0.0254 m, 1 ft = 0.3048 m, 1 lb = 0.45359237 kg, and
1 psi = (0.45359237 × 9.80665 / 0.0254²) Pa. The executable block retains
these definitions and the source inputs without rounding intermediate values.
All public sources were consulted on 2026-10-25; [W] was available through
indexed source text, while direct page retrieval was unsuccessful.

All pressures use the same atmospheric gauge reference, as in the surface
well-control example. The upper datum is mapped to the API's mudline argument
solely to exercise the scalar calculation; no seawater column or subsea pressure
correction is introduced. Formation and absolute pressures are not mixed.
Depth is positive vertical separation, not measured length along a bore.

## Derivation

[H] The physical basis is [OpenStax, University Physics Volume 1 (2016 edition),
section 14.1, Fluids, Density, and Pressure, equations 14.8–14.9](https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure).
For a static element of column with area A and thickness dz, take z positive
downward from the mudline. Vertical force balance gives
A dp = rho A g_n dz, so dp/dz = rho g_n. Integrating from z = 0 to z = h gives

$$
p_{\mathrm{res}} - p_{\mathrm{wh,raw}}
= \int_0^h \rho(z) g_n\, dz
= \rho g_n h
= p_{\mathrm{head}}.
$$

For the published inputs [W] and conversions [C], the hand calculation is

$$
p_{\mathrm{head}}
= \left(\frac{11.5(0.45359237)}{231(0.0254)^3}\right)
  (9.80665)(10000(0.3048))\ \mathrm{Pa}
\approx 41.189\ \mathrm{MPa}.
$$

The units reduce to kg/(m s²), or Pa. The arithmetic in
[`containment_capacity.py`](../containment_capacity.py) then applies

$$
p_{\mathrm{wh,raw}} = p_{\mathrm{res}} - p_{\mathrm{head}}, \qquad
p_{\mathrm{screen}} = \max(p_{\mathrm{wh,raw}}, 0), \qquad
U = \frac{p_{\mathrm{screen}}}{R}, \qquad
\mathrm{is\_contained} = (p_{\mathrm{screen}} \le R).
$$

Zero is the implemented clamp floor; unity is the rating ratio at equality.
These are arithmetic policies, not additional physical constants or standards
qualification criteria. For the base case, p_res = 6,292 psi converted by [C]
is 43.382 MPa; subtracting the unrounded head gives 2.192 MPa. Dividing by
R = 15,000 psi converted by [C], or 103.421 MPa, gives U = 0.0212.

The field formula in [W] uses the rounded coefficient 0.052 psi/(ppg ft):
0.052 × 11.5 × 10,000 = 5,980 psi, giving a residual of 312.0 psi.
Using the definitions [C] instead gives a residual of 318.0 psi, a difference
of 6.0 psi. The tests check the independent SI calculation; they do not force
agreement with that rounded field coefficient.

Only `within_rating` uses the published formation pressure. The other rows are
derived arithmetic cases, not further published examples or operating cases.
With H = rho g_n h, their construction is p_res = H + R at equality,
p_res = H + 1.1R for exceedance, and p_res = H/2 for overbalance.
Density and depth from [W] remain fixed in every row. For the equality case
only, the binary floating-point comparison limit is constructed as
R_eq = (H + R) - H. This makes equality representable exactly without a
tolerance in the decision. R_eq can differ from the nominal conversion of [R]
by roundoff; it is a derived arithmetic input, not another equipment rating.
All other cases use the unmodified conversion of [R].

| Case key        | p_res (MPa) | Raw pressure (MPa) | Clamped pressure (MPa) | Comparison limit (MPa) | U (limit 1.00) | is_contained (pressure ≤ limit) |
| --------------- | ----------- | ------------------ | ---------------------- | ---------------------- | -------------- | ------------------------------- |
| `within_rating` | 43.382      | 2.192              | 2.192                  | 103.421                | 0.0212         | True                            |
| `at_rating`     | 144.611     | 103.421            | 103.421                | 103.421                | 1.000          | True                            |
| `above_rating`  | 154.953     | 113.763            | 113.763                | 103.421                | 1.100          | False                           |
| `overbalanced`  | 20.595      | -20.595            | 0.000                  | 103.421                | 0.000          | True                            |

Table 2. Independent SI results, rounded only for display; every row uses the inputs and criterion stated above.

Equality passes because the implemented comparison is inclusive. Exceedance
fails without a tolerance band. In the overbalanced case, zero is a clamped
arithmetic result; it is not a prediction of actual pressure, flow direction,
flow rate or whether a well can flow. The returned explanatory notes are not
used as evidence of flow behaviour.

## Executable example

Execute the following block with the repository's `src` directory on
`PYTHONPATH` and a supplied temporary directory as the working directory.
It runs offline and writes only `static-containment.json` in that directory.
Both containment functions receive the explicit column density. The values in
the code come from [W], [R] and [C]; the implementation's gravity is independently
checked against [G] by the documented-example test. No solver is required.

```python
import json
from pathlib import Path

from digitalmodel.subsea.capping_stack.containment_capacity import (
    check_containment,
    shut_in_wellhead_pressure,
)

# Exact unit definitions [C]; standard gravity [G].
inch = 0.0254  # m
pound = 0.45359237  # kg
standard_gravity = 9.80665  # m/s^2
psi = pound * standard_gravity / inch**2  # Pa per psi
column_density = 11.5 * pound / (231 * inch**3)  # kg/m^3, from [W]
vertical_depth = 10_000 * 0.3048  # m, from [W]
rating = 15_000 * psi  # Pa, from [R]
head = column_density * standard_gravity * vertical_depth
reservoir_pressures = {
    "within_rating": 6_292 * psi,  # published formation pressure [W]
    "at_rating": head + rating,  # derived equality case
    "above_rating": head + 1.1 * rating,  # derived exceedance case
    "overbalanced": head / 2,  # derived clamp case
}  # Pa, relative to the common atmospheric gauge reference

results = {}
for name, reservoir_pressure in reservoir_pressures.items():
    # Construct exact binary equality only for this derived arithmetic case.
    case_rating = reservoir_pressure - head if name == "at_rating" else rating
    result = check_containment(
        rated_working_pressure=case_rating,
        reservoir_pressure=reservoir_pressure,
        datum_to_mudline_tvd=vertical_depth,
        column_density=column_density,
    )
    results[name] = {
        "reservoir_pressure": reservoir_pressure,
        "column_density": column_density,
        "vertical_depth": vertical_depth,
        "rated_working_pressure": result.rated_working_pressure,
        "hydrostatic_head": result.hydrostatic_head,
        "siwhp": result.siwhp,
        "direct_siwhp": shut_in_wellhead_pressure(
            reservoir_pressure=reservoir_pressure,
            datum_to_mudline_tvd=vertical_depth,
            column_density=column_density,
        ),
        "utilisation": result.utilisation,
        "is_contained": result.is_contained,
    }

Path("static-containment.json").write_text(
    json.dumps(results, indent=2, sort_keys=True) + "\n",
    encoding="utf-8",
    newline="\n",
)
```

The test extracts and executes this exact block in a separate process and
temporary directory. Expected values come from decimal arithmetic applied to
the integral above, independently of the implementation and its constants.
A relative tolerance of 1e-12 accommodates binary floating-point roundoff,
not uncertainty in the published teaching inputs. Zero and equality dispositions are
also checked exactly; this tolerance does not modify the comparison policy.

## Limits and way forward

This example assumes a static column, constant density and standard gravity.
It does not evaluate compressibility, density variation, friction, temperature,
multiphase behaviour or flow transients. It makes no default-density assumption,
uses no pressure classes and selects no equipment.

Interface compatibility, deployment qualification and standards qualification
are outside this example. A true arithmetic disposition establishes only the
stated scalar comparison against [R]; a false result identifies exceedance for
the constructed inputs. The drilling-mud column from [W] is not established as
the lightest credible column for a capped well. Engineering use requires a
justified pressure basis and column model,
and separate evaluation of those excluded effects and qualifications.

The full module report, builder and index remain open under issue #3341.
