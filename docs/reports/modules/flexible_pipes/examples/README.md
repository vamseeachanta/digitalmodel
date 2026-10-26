# Flexible pipe annulus example

This example computes the fluid-barrier and outer-sheath areas and dry masses
from a public flexible-pipe case through the existing layer API. The reported
sum is a subtotal for these two sheaths; complete-pipe mass is not evaluated.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-25 | Add a reproducible geometry and mass example with independent integral checks. |
| 1 | 2026-10-26 | Replace synthetic inputs with cited public-case sheath data and identify the mass subtotal. |

## Public-case input basis

The source is *Comparison Between Different Finite Element Analyses of Unbonded
Flexible Pipe via Different Modeling Patterns* (2019), section 1, Table 1,
printed page 358, [DOI 10.1007/s12204-019-2064-8](https://doi.org/10.1007/s12204-019-2064-8).
The [public author-uploaded full text](https://www.researchgate.net/publication/345464193_Comparison_Between_Different_Finite_Element_Analyses_of_Unbonded_Flexible_Pipe_via_Different_Modeling_Patterns)
provides the input table; the publisher page provides the bibliographic record.

| Model layer | Inner diameter (mm) | Outer diameter (mm) | Density (kg/m³) | Source location for all three inputs |
| --- | --- | --- | --- | --- |
| Pressure sheath | 70.2 | 80.0 | 1080 | Table 1, Fluid barrier row |
| Outer sheath | 110.5 | 111.5 | 800 | Table 1, Outer sheath row |

Table 1. Published dimensions and material densities for the selected sheaths.

Diameters are divided by 1000 for metres. Radial thickness is derived as
`(OD - ID)/2`: 0.0049 m for the fluid barrier and 0.0005 m for the outer sheath.
Input precision is retained in the executable. The selected sheaths are separated
by armour and anti-wear layers in the source; each is evaluated independently
as a homogeneous solid annulus. No intervening layers are invented or relocated.

## Method and independent calculation

Area is the polar integral `A = ∫∫ r dr dθ = π(OD² − ID²)/4` over the annulus.
Uniform volume density gives layer mass per length `m′ = ρA`; the sheath
subtotal is the sum of these two masses. The basis is *Calculus Volume 3*,
first edition (2016), OpenStax, section
[5.3, Double Integrals in Polar Coordinates](https://openstax.org/books/calculus-volume-3/pages/5-3-double-integrals-in-polar-coordinates)
for the area element, and section
[5.6, Calculating Centers of Mass and Moments of Inertia](https://openstax.org/books/calculus-volume-3/pages/5-6-calculating-centers-of-mass-and-moments-of-inertia)
for mass as the integral of volume density. These are geometric identities;
no standard-derived design factor is used.

Hand substitution gives fluid-barrier area `1.16 × 10⁻³ m²` and mass `1.25 kg/m`,
outer-sheath area `1.74 × 10⁻⁴ m²` and mass `0.139 kg/m`, and subtotal
`1.39 kg/m` (results displayed to three significant figures).
The [rebuild test](../../../../../tests/subsea/flexible_pipes/test_annulus_example.py)
asserts independent hand-calculated literals for both areas, both masses and
the subtotal with a relative tolerance of `1e-12` and zero absolute tolerance.
The literals retain calculation precision; no expected result comes from the API.

## Run

From the repository root, with `uv` available:

```sh
PYTHONPATH=src uv run --no-project python docs/reports/modules/flexible_pipes/examples/annulus.py
```

This API path uses the Python standard library only; Python must satisfy the
repository's declared version requirement. `PYTHONPATH=src` makes the source
package available without installing the wider application's dependencies.
Output is strict JSON with layer areas in m² and dry masses in kg/m, including
the selected-sheath subtotal and an explicit scope field. Repeated runs emit
identical JSON. Imports produce no output
or files. By default the script writes only to stdout; `--output <file>` writes
the same JSON as UTF-8 with LF line endings to the requested file instead.
Parent directories must exist.

## Limits of use

`NOT_EVALUATED`: stiffness interaction, API compliance and design qualification.
Complete-pipe dry mass is also `NOT_EVALUATED`; only the sheath subtotal is given.
The example does not represent wound armour, void fractions, bore contents,
buoyancy, stress, fatigue or bend-stiffener performance. It establishes only
the selected published annulus geometry and sheath dry mass calculation.
Agreement with structural finite-element results or measured mass is not established.
The full module report, figures, changes panel and registry remain outside
this example; the remaining scope of issue #3343 stays open.
