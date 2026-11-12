# Worked tubular stress invariants

## Summary

This synthetic example calls `compute_hoop_stress` and `compute_vme_stress`
for tensile hoop loading, reversed differential pressure and pure shear.
A longitudinal half-cylinder force balance supplies the hoop answer independently;
principal-stress differences supply the equivalent-stress answer independently.
The example illustrates arithmetic and sign conventions for a thin wall. It does
not qualify a casing or tubing design.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-25 | Added synthetic inputs, independent derivations and an executable example to make the helper conventions reproducible. |
| 1 | 2026-10-26 | Added a Plan-lite section to state the worked-example scope, independent method, tests and limits together. |

## Plan-lite

- Scope: one synthetic thin-wall worked example covering combined loading,
  reversed differential pressure and pure shear with the existing stress helpers.
- Method: independent longitudinal half-cylinder equilibrium for hoop stress and
  principal-stress differences for equivalent stress.
- Tests: execution of the exact documented Python block, comparisons with the
  independent calculations and confirmation that the block creates no files.
- Limits: mean-surface geometry and plane stress; separate assessment of radial
  stress, collapse, stability and material resistance remains necessary.

## Inputs and units

All entries below are **assumptions chosen for this synthetic illustration**,
not measurements, catalogue dimensions or material properties. Positive normal
stress denotes tension. Differential pressure is internal minus external pressure.
Dimensions use inches (in); pressure, normal stress and shear stress use psi
(pound-force per square inch, lbf/in²). No unit conversion is performed.

| Quantity | Internal-pressure case | Reversed-pressure case | Pure-shear case | Basis |
| --- | --- | --- | --- | --- |
| Outside diameter, in | 10.2 | 10.2 | 10.2 | Synthetic assumption |
| Inside diameter, in | 9.8 | 9.8 | 9.8 | Synthetic assumption |
| Internal pressure, psi | 500.0 | 100.0 | 0.0 | Synthetic assumption |
| External pressure, psi | 100.0 | 500.0 | 0.0 | Synthetic assumption |
| Axial normal stress, psi | 6000.0 | 6000.0 | 0.0 | Independently imposed synthetic load |
| In-plane shear stress, psi | 2000.0 | 2000.0 | 1500.0 | Independently imposed synthetic load |
| Free-body length, in | 1.0 | 1.0 | 1.0 | Arbitrary assumed length; cancels from equilibrium |

Table 1. Assumed geometry and loads; no rated strength is supplied or inferred.

The derived wall thickness is `t = (OD - ID)/2 = 0.2 in`, the mean diameter is
`D = (OD + ID)/2 = 10.0 in`, and `t/(D/2) = 0.04`. The factors of two come
from radius/diameter geometry and averaging; they are not empirical constants.
The radial normal stress is approximated as zero (plane stress), and axial stress
is supplied independently. A closed-end pressure load is not added implicitly.

## Derivation: longitudinal half-cylinder force balance

The open [MIT pressure-vessel notes, cylindrical equilibrium, Figure 5 and
Equation 3](https://web.mit.edu/course/3/3.11/www/modules/pv.pdf) derive hoop
stress from a longitudinal cut. Apply that balance to a straight length `L`.
The projected pressure resultant on the half-cylinder is `Δp D L` when the
inner and outer pressure surfaces are replaced by the mean surface. The two
cut wall strips each carry `σh t L`, so equilibrium gives

```text
Δp = pi - po
2 σh t L = Δp D L
σh = Δp D / (2 t)
```

The coefficient two counts the two cut strips. For the assumed positive-pressure
case, `Δp = 500.0 - 100.0 = 400.0 psi`. The projected resultant is
`400.0 × 10.0 × 1.0 = 4000.0 lbf`, and the combined cut area is
`2 × 0.2 × 1.0 = 0.4 in²`. Thus `σh = 10000.0 psi` in tension.
Reversing the pressures gives `Δp = -400.0 psi` and `σh = -10000.0 psi`.
Axial and shear stresses remain unchanged in that reversal.

This is explicitly a mean-surface approximation. Keeping the distinct pressure
surfaces would instead give `pi ID L - po OD L` as the pressure resultant;
replacing both diameters by `D` discards a term `-(pi + po)tL`.
The numerical tests check the helper's stated approximation, not that correction
or a through-wall elastic solution. Radial stress and external-pressure stability
also require separate treatment.

## Derivation: equivalent stress from principal differences

Use the principal-difference definition in the open [MIT yield notes,
Multiaxial stress states, page 5](https://web.mit.edu/course/3/3.11/www/modules/yield.pdf).
For an in-plane stress tensor with axial stress `a`, hoop stress `h` and
shear `s`, its characteristic equation gives the following independent route:

```text
stress tensor = [[a, s, 0], [s, h, 0], [0, 0, 0]]
m = (a + h)/2
r = sqrt(((a - h)/2)^2 + s^2)
principal stresses = (m + r, m - r, 0)
q^2 = ((σ1 - σ2)^2 + (σ2 - σ3)^2 + (σ3 - σ1)^2)/2
    = ((2r)^2 + (m - r)^2 + (m + r)^2)/2
    = m^2 + 3r^2
```

The normalization by two makes `q` equal the magnitude of a uniaxial stress.
The coefficient three follows from expanding those squared differences; it is
not a physical or conversion constant. Substitution of `m` and `r` yields
`q² = a² + h² - ah + 3s²`, which is the expression evaluated by the helper.
The test uses the principal-difference route rather than this expanded formula.

For the internal-pressure case, `m = 8000.0 psi`, `r² = 8000000.0 psi²`,
and `q = sqrt(88000000.0) psi`. With reversed pressure, `m = -2000.0 psi`,
`r² = 68000000.0 psi²`, and `q = sqrt(208000000.0) psi`.
Only pressure is reversed, so equivalent stress need not be unchanged.
For pure shear, the principal stresses are `(+s, -s, 0)`:
`q² = ((2s)² + s² + s²)/2 = 3s²`, hence `q = sqrt(3)|s|`.
This also checks that equivalent stress is nonnegative.

| Case | Differential pressure, psi | Hoop stress, psi | Equivalent stress, psi |
| --- | --- | --- | --- |
| Internal pressure | 400.0 | 10000.0 | 9380.8 |
| Reversed pressure | -400.0 | -10000.0 | 14422.2 |
| Pure shear | 0.0 | 0.0 | 2598.1 |

Table 2. Derived answers; displayed stresses are rounded to one decimal place.

All numerical inputs are identified in Table 1. All other numerical coefficients
above follow from geometry, equilibrium or the cited stress invariant. There are
no empirical material constants or conversion factors in this calculation.

## Executable example

Run the following single block with the source package and its dependencies
available on the Python import path. It needs no network access, takes no external
input, prints JSON to standard output and creates no files. The regression test
extracts this exact block and supplies an isolated temporary working directory.
The JSON preserves full precision; Table 2 contains the rounded display values.

```python
import json

from digitalmodel.well.tubulars.design_envelope import (
    compute_hoop_stress,
    compute_vme_stress,
)

# All numeric inputs are the synthetic assumptions in Table 1.
od_in, id_in = 10.2, 9.8
loads = (
    ("internal", 500.0, 100.0, 6000.0, 2000.0),
    ("reversed", 100.0, 500.0, 6000.0, 2000.0),
    ("pure_shear", 0.0, 0.0, 0.0, 1500.0),
)
cases = {}
for name, internal_psi, external_psi, axial_psi, shear_psi in loads:
    differential_psi = internal_psi - external_psi
    hoop_psi = compute_hoop_stress(differential_psi, od_in, id_in)
    cases[name] = {
        "differential_pressure_psi": differential_psi,
        "axial_psi": axial_psi,
        "hoop_psi": hoop_psi,
        "shear_psi": shear_psi,
        "vme_psi": compute_vme_stress(axial_psi, hoop_psi, shear_psi),
    }
print(json.dumps({"stress_unit": "psi", "cases": cases}, sort_keys=True))
```

The regression comparisons use relative tolerance `1e-12` and absolute stress
tolerance `1e-8 psi`. These are numerical test choices: decimal geometry suffers
binary subtraction rounding, and the invariant route adds a square root before
squaring differences. These tolerances cover floating-point arithmetic for these
inputs; they do not quantify physical approximation error. Expected values are
computed from the unrounded force balance and principal stresses above.

## Limits

The case assumes a straight, circular, uniform thin wall, small deformation,
uniform loading and plane stress away from ends and discontinuities. The
absolute internal and external pressures must also be small relative to membrane
stress for omission of radial stress to be useful. The example supplies no
universal accuracy bound and makes no claim of elastic response for a particular
material. No yield strength, allowable stress or design factor is selected.

Tabulated burst/collapse ratings, anisotropic envelopes, material selection,
connections, wear, corrosion, ovality, buckling, fatigue and thermal loads are
not evaluated. A negative hoop result illustrates pressure sign only; it is not
a collapse resistance. This thin-wall illustration does not qualify a casing
design. The full module report, builder and index remain open under issue #3344.
