# Static containment analysis: worked example

## Summary

A synthetic constant-density column gives a hydrostatic head of 9,806,650 Pa.
With the assumed reservoir pressure of 49,806,650 Pa and rating of 50,000,000 Pa,
the clamped shut-in wellhead pressure is 40,000,000 Pa and utilisation is 0.8.
The arithmetic screen returns `is_contained = True`. The variants below exercise
inclusive equality, exceedance and zero clamping. These are static arithmetic
results, with no physical flow prediction or equipment qualification.

## Revision history

| Revision | Date       | What changed and why                                      |
| -------- | ---------- | --------------------------------------------------------- |
| Initial  | 2026-10-25 | Added a computed example to explain the static comparison. |

## Inputs and sources

[A] All scenario inputs in this section are synthetic assumptions chosen for
transparent arithmetic. They are neither measurements nor equipment data.
The column density is explicitly supplied and has no assigned fluid identity.

| Input                     | Symbol    | Assumed value [A] |
| ------------------------- | --------- | ----------------- |
| Constant column density   | rho       | 500 kg/m³         |
| Datum-to-mudline depth    | h         | 2,000 m           |
| Positive pressure rating  | R         | 50,000,000 Pa     |

Reservoir pressure at the deeper datum is varied as follows; density, depth and
rating remain the stated assumptions in every case.

| Case key        | Reservoir pressure p_res [A] | Purpose                       |
| --------------- | ---------------------------- | ----------------------------- |
| `within_rating` | 49,806,650 Pa                 | Pressure below rating         |
| `at_rating`     | 59,806,650 Pa                 | Pressure exactly at rating    |
| `above_rating`  | 64,806,650 Pa                 | Pressure above rating         |
| `overbalanced`  | 5,000,000 Pa                  | Hydrostatic head exceeds p_res |

[G] The independent source for the standard gravitational acceleration is
[NIST, standard acceleration of gravity](https://physics.nist.gov/cgi-bin/cuu/Value?gn),
which states g_n = 9.80665 m/s² exactly (accessed 2026-10-25).
This is the standard-gravity convention, not a measured local acceleration.
The example adopts it as constant along the column. All calculations use SI
units directly; no pressure or length conversion factors are used.

All pressures use a common reference: the external pressure at the mudline.
The synthetic reservoir pressure is expressed relative to that same reference,
and the stated rating bounds the resulting wellhead pressure difference.
Absolute reservoir pressure and seawater external pressure are not mixed.
Depth is the positive vertical separation, not measured length along a bore.

## Derivation

For a static element of column with area A and thickness dz, take z positive
downward from the mudline. Vertical force balance gives
A dp = rho A g_n dz, so dp/dz = rho g_n. Integrating from z = 0 to z = h gives

$$
p_{\mathrm{res}} - p_{\mathrm{wh,raw}}
= \int_0^h \rho(z) g_n\, dz
= \rho g_n h
= p_{\mathrm{head}}.
$$

For the constant assumptions [A] and convention [G], the hand calculation is

$$
p_{\mathrm{head}}
= (500\ \mathrm{kg/m^3})(9.80665\ \mathrm{m/s^2})(2000\ \mathrm{m})
= 9{,}806{,}650\ \mathrm{Pa}.
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
These are arithmetic policies, not additional physical constants. For the base
case, subtracting 9,806,650 Pa from 49,806,650 Pa gives 40,000,000 Pa, and dividing
by 50,000,000 Pa gives U = 0.8. Applying the same independent calculation to
the other assumed pressures gives:

| Case key        | Raw wellhead pressure (Pa) | Clamped pressure (Pa) | U   | is_contained |
| --------------- | -------------------------- | --------------------- | --- | ------------ |
| `within_rating` | 40,000,000                 | 40,000,000            | 0.8 | True         |
| `at_rating`     | 50,000,000                 | 50,000,000            | 1.0 | True         |
| `above_rating`  | 55,000,000                 | 55,000,000            | 1.1 | False        |
| `overbalanced`  | -4,806,650                 | 0                     | 0.0 | True         |

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
the code are the assumptions [A]; the implementation's gravity is independently
checked against [G] by the documented-example test. No solver is required.

```python
import json
from pathlib import Path

from digitalmodel.subsea.capping_stack.containment_capacity import (
    check_containment,
    shut_in_wellhead_pressure,
)

# All numerical inputs are synthetic assumptions [A] in Inputs and sources.
column_density = 500.0  # kg/m^3
vertical_depth = 2000.0  # m
rating = 50_000_000.0  # Pa
reservoir_pressures = {
    "within_rating": 49_806_650.0,
    "at_rating": 59_806_650.0,
    "above_rating": 64_806_650.0,
    "overbalanced": 5_000_000.0,
}  # Pa, relative to the common mudline reference

results = {}
for name, reservoir_pressure in reservoir_pressures.items():
    result = check_containment(
        rated_working_pressure=rating,
        reservoir_pressure=reservoir_pressure,
        datum_to_mudline_tvd=vertical_depth,
        column_density=column_density,
    )
    results[name] = {
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
    json.dumps(results, indent=2, sort_keys=True) + "\n", encoding="utf-8"
)
```

The test extracts and executes this exact block in a separate process and
temporary directory. Expected values come from decimal arithmetic applied to
the integral above, independently of the implementation and its constants.
A relative tolerance of 1e-12 accommodates binary floating-point roundoff,
not uncertainty in the synthetic inputs. Zero and equality dispositions are
also checked exactly; this tolerance does not modify the comparison policy.

## Limits and way forward

This example assumes a static column, constant density and standard gravity.
It does not evaluate compressibility, density variation, friction, temperature,
multiphase behaviour or flow transients. It makes no default-density assumption,
uses no pressure classes and selects no equipment.

Interface compatibility, deployment qualification and standards qualification
are outside this example. A true arithmetic disposition establishes only the
stated scalar comparison; a false result identifies exceedance for the assumed
inputs. Engineering use requires a justified pressure basis and column model,
and separate evaluation of those excluded effects and qualifications.

The full module report, builder and index remain open under issue #3341.
