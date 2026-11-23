# Holtrop resistance and effective power example from published inputs

The [saved example](../../examples/naval_architecture/holtrop_power.py) calls the
existing package functions and reports component resistance in newtons (N) and
effective power in watts (W). It demonstrates arithmetic and units for a
published hypothetical hull. The saved JSON identifies its source. These are
input-provenance and bookkeeping checks; reproduction of the publication and
empirical accuracy are not established.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-27 | Add a synthetic example to document force and power bookkeeping. |
| 1 | 2026-10-27 | Replace chosen inputs with a cited public case and document the physical basis. |

## Run

From the repository root, with the package dependencies available:

```bash
PYTHONPATH=src uv run python examples/naval_architecture/holtrop_power.py --output-dir "$OUTPUT_DIR"
```

Set `OUTPUT_DIR` to a caller-selected directory outside the checkout before
running. A lightweight source-checkout environment needs PyYAML for the naval
architecture package imports; the calculation itself uses the standard library.
The script creates the directory if necessary and writes only
`holtrop_power.json` there. It refuses to overwrite an existing result. Importing
the example writes nothing. Console values use one decimal place; the JSON
retains full floating-point precision for arithmetic checks.

## Published inputs and units

The hypothetical numerical example in the [1982 method paper][method], section 5,
p. 170, supplies the dimensions, displacement, coefficients, appendage area,
appendage multiplier and speed. The table below retains source precision.
Derived quantities use the full expressions in code and JSON.

| Argument | Value | Meaning and unit |
| --- | --- | --- |
| `lwl` | 205.00 | Waterline length, m |
| `beam` | 32.00 | Moulded beam, m |
| `draft`, `tf` | 10.00 | Mean and forward draft, m; both published drafts are equal |
| `cb` | `37500/(205*32*10)` | Block coefficient from displacement volume, dimensionless |
| `cm` | 0.980 | Midship coefficient, dimensionless |
| `cwp` | 0.750 | Waterplane coefficient, dimensionless |
| `cp` | `cb/0.980` | Prismatic coefficient, dimensionless |
| `lcb_pct` | -2.02 | Aft offset expressed in the API convention: percent of waterline length, positive forward |
| `cstern` | 10 | Published stern parameter, dimensionless |
| `speed_ms` | `25*1852/3600` | Published 25.0 knots converted to m/s |
| `abt` | 20.0 | Transverse bulb area, m² |
| `hb` | 4.0 | Bulb centre above baseline, m |
| `at` | 16.0 | Immersed transom area, m² |
| `appendage_areas` | [50.0] | Appendage wetted area, m² |
| `appendage_factors` | [1.50] | Published equivalent appendage multiplier, dimensionless |

Table 1. Inputs from the 1982 numerical example and their API mapping.

The displacement volume is 37500 m³. Coefficient definitions follow section 2
of the same paper; `cp` is derived without rounding to its printed result.
The exact knot conversion follows [NIST SP 811 (2008)][units], section 5.2,
table 9. Invented forces and speed occur only in the hand-calculated bookkeeping
test; they are not reference resistance data.

## Equations and provenance

The existing [method module](../../src/digitalmodel/naval_architecture/holtrop_mennen.py)
is called without changing its equations. The resistance decomposition, wetted
surface estimate, form factor, appendage multiplier and correlation expression
are described in the [1982 paper][method], section 2, pp. 166–168; its section 5
also reports effective power. The [1984 revision][revision], section 2, describes
the low-speed wave-resistance expression. Arithmetic agreement alone does not
qualify the implementation against either publication.

The package [physical assumptions](../../src/digitalmodel/naval_architecture/holtrop_coefficients.py)
are retained and saved with units:

- Density is 1025 kg/m³, the ideal-trial assumption in the 1982 paper, section 4,
  p. 169; the associated conditions are calm, deep water and 15 °C.
- Kinematic viscosity is 1.1892e-6 m²/s, the 15 °C entry in [ITTC 7.5-02-01-03][water],
  2011 revision 02, section 3.1, table 3, p. 9.
- Gravity is 9.80665 m/s², standard acceleration of free fall in
  [NIST SP 811 (2008)][gravity], appendix B.9, Acceleration.

The density and viscosity are separate historical assumptions. They are not
asserted to describe one measured fluid state or the ITTC table density.
The wetted surface is estimated by the package regression; no hull mesh is used.
The form-factor function returns `(1+k1)` directly.

Using the component definitions in the 1982 paper, section 2, the sum is
`RT = RF*(1+k1) + RAPP + RW + RB + RTR + RA`, with every term in N.
`components_n.viscous_n` contains `RF*(1+k1)`; the separate `friction_n` diagnostic
contains `RF` and is not added again. The remaining components represent appendage,
wave, bulb, transom and correlation resistance. Correlation resistance uses
`RA = rho*V²*S*CA/2`; the divisor follows the dynamic-pressure definition and
`CA` comes from the package function. Effective power is `PE = RT*V`, so
`N * m/s = W`. It is not shaft power or installed engine power.

The script independently sums the components and multiplies total force by speed
to check the package totals. Its relative tolerance of `1e-12` is a chosen
floating-point bookkeeping guard, not an engineering accuracy limit.
The regression tests also substitute invented component forces and check a
hand-calculated sum and product, without using model results as reference data.

## Applicability and remaining work

The [1984 revision][revision], section 2, p. 273, separates the low-speed wave
expression at Froude number 0.4 from higher-speed branches. The package uses
`Fn < 0.400`, where `Fn = V/sqrt(g*LWL)`; this public case gives `Fn = 0.287`.
That speed comparison does not establish suitability for other hull proportions
or features; section 1 of the 1982 paper limits use to representative hull forms.
No input sweep or general validity-domain enforcement is provided here.

The arithmetic checks do not assess the empirical formulas against experiments.
They do not establish suitability for planing, multihull or unusual forms, nor
account for environmental loads, propulsion efficiency or service margins.
Independent public reference comparisons, a coefficient and applicability audit,
and any model corrections remain separate work under issue #1682. Engineering
use requires that evidence and a review of the intended hull and operating range.

## References

[method]: https://moodle2.units.it/pluginfile.php/493107/mod_resource/content/0/Holtrop%20calcolo%20statistico%20resistenza.pdf#page=5
[revision]: https://moodle2.units.it/pluginfile.php/493107/mod_resource/content/0/Holtrop%20calcolo%20statistico%20resistenza.pdf#page=7
[water]: https://www.ittc.info/media/9585/75-02-01-03.pdf#page=9
[units]: https://www.nist.gov/pml/special-publication-811/nist-guide-si-chapter-5-units-outside-si
[gravity]: https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b9

The method reference is Holtrop and Mennen, *An approximate power prediction
method*, International Shipbuilding Progress 29(335), 1982, pp. 166–170,
DOI 10.3233/ISP-1982-2933501. The revision is Holtrop, *A statistical re-analysis
of resistance and propulsion data*, International Shipbuilding Progress 31(363),
1984, pp. 272–276. The linked public scan contains both papers.
