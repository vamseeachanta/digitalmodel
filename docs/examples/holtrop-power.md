# Synthetic Holtrop resistance and effective power example

The [saved example](../../examples/naval_architecture/holtrop_power.py) calls the
existing package functions and reports component resistance in newtons (N) and
effective power in watts (W). It demonstrates arithmetic and units for an
explicitly synthetic input. It makes no numerical-validation claim for a real
hull and adds no resistance model or calibrated coefficient.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-27 | Add a synthetic example to document force and power bookkeeping. |

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

## Synthetic inputs and units

The following values are chosen solely for this demonstration. They are not
measurements, published hull data, calibration targets or design recommendations.
This table is the source of the example's input constants.

| Argument | Value | Meaning and unit |
| --- | --- | --- |
| `lwl` | 100.0 | Waterline length, m |
| `beam` | 16.0 | Beam, m |
| `draft`, `tf` | 5.0 | Mean and forward draft, m |
| `cb` | 0.60 | Block coefficient, dimensionless |
| `cm` | 0.96 | Midship coefficient, dimensionless |
| `cwp` | 0.72 | Waterplane coefficient, dimensionless |
| `cp` | 0.625 | Prismatic coefficient: `cb/cm`, dimensionless |
| `lcb_pct` | -1.0 | Longitudinal buoyancy centre, percent of length from midships, positive forward |
| `cstern` | 0 | Chosen neutral stern parameter in the existing form-factor function |
| `speed_ms` | 5.0 | Speed, m/s |
| `abt` | 16.0 | Transverse bulb area, m² |
| `hb` | 1.0 | Bulb centre above baseline, m |
| `at` | 2.0 | Immersed transom area, m² |
| `appendage_areas` | [8.0] | Synthetic appendage wetted area, m² |
| `appendage_factors` | [1.5] | Synthetic appendage resistance multiplier, dimensionless |

## Equations and provenance

The existing [method module](../../src/digitalmodel/naval_architecture/holtrop_mennen.py)
cites the method's 1982 paper and 1984 revision and the ITTC 1957 friction line.
The example reuses these implementations without adding regression coefficients.
Density, kinematic viscosity and gravity are imported from the existing
[physical assumptions](../../src/digitalmodel/naval_architecture/holtrop_coefficients.py)
and included with units in the JSON; they are not site-specific measurements.
The wetted surface is the package's regression estimate, not an area from a hull
mesh. The form-factor function returns `(1+k1)` directly.

The component sum is
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

Use this example only to screen conventional displacement hull calculations in
calm water. The current wave-resistance function documents `Fn < 0.4`, where
`Fn = V/sqrt(g*LWL)`; this fixed synthetic case lies below that limit. This single
limit does not establish applicability for other hull proportions or features.
No input sweep or general validity-domain enforcement is provided here.

The arithmetic checks do not assess the empirical formulas against experiments.
They do not establish suitability for planing, multihull or unusual forms, nor
account for environmental loads, propulsion efficiency or service margins.
Independent public reference comparisons, a coefficient and applicability audit,
and any model corrections remain separate work under issue #1682. Engineering
use requires that evidence and a review of the intended hull and operating range.
