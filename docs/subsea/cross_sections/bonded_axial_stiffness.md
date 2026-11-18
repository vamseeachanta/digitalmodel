# Bonded isotropic layer axial stiffness

`bonded_axial_stiffness` calculates elementary axial rigidity for fully bonded,
concentric, homogeneous, isotropic layers under a common uniform axial strain.
Moduli and their provenance are supplied explicitly by the caller.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-27 | Define the elementary bonded-layer calculation and its input boundaries. |

## Calculation and inputs

For each layer, subtract the inner circle area from the outer circle area:
`A_i = pi * (OD_i - ID_i) * (OD_i + ID_i) / 4`. Circle area and the diameter
relation follow [OpenStax Prealgebra 2e, section 9.5](https://openstax.org/books/prealgebra-2e/pages/9-5-solve-geometry-applications-circles-and-irregular-figures).
From `stress = E * strain` and `force = stress * area`, common strain and
additive forces give `EA = sum(E_i * A_i)`. The stress-strain relation is given
in [OpenStax University Physics Volume 1, section 12.3](https://openstax.org/books/university-physics-volume-1/pages/12-3-stress-strain-and-elastic-modulus).
This sum is an elementary parallel-load derivation, with no standards data.

Import `BondedAxialLayer` and `bonded_axial_stiffness` from
`digitalmodel.subsea.cross_sections.calculations`. Each input pairs:

- `layer`: an existing `RadialLayer`, including geometry provenance and at least
  two of inner diameter, outer diameter and thickness, all in metres;
- `youngs_modulus_pa`: a finite positive modulus, with no material default;
- `modulus_unit`: the explicit literal `Pa`; convert other units before use;
- `modulus_provenance`: a separate `Provenance` for the modulus.

Density is unnecessary. Layer order is arbitrary: geometry and modulus stay
paired when sorted by inner diameter. Neighbouring diameters must match exactly;
gaps and overlaps are rejected, with no tolerance that could hide a discontinuity.
When derived floating-point dimensions differ at a shared interface, supply
consistent explicit diameters. A central bore or solid core is permitted.
Invalid or unrepresentable areas, rigidities and equivalent moduli raise errors.

The returned `BondedAxialProperties` contains:

- `axial_stiffness_n`: axial rigidity `EA` in N, force per unit strain;
- `area_m2`: the sum of material layer areas in m², excluding the bore;
- `equivalent_modulus_pa`: `sum(E_i*A_i)/sum(A_i)` in Pa.

A member's force/displacement stiffness would be `EA/L` in N/m and requires a
separate length input. That quantity is not returned here.

## Saved example

Run from the repository root with its import dependencies available:

```sh
PYTHONPATH=src python examples/subsea/cross_sections/bonded_axial_stiffness.py
```

The [saved example](../../../examples/subsea/cross_sections/bonded_axial_stiffness.py)
uses synthetic radii of 1, 2 and 3 m and assumed moduli of 10 and 20 Pa, selected
for independent arithmetic rather than engineering material representation.
The areas are `3*pi` and `5*pi` m², so `EA=(10*3+20*5)*pi=130*pi` N and
`Eeq=130/8` Pa. The executable checks these expressions before printing:

```text
Material area: 25.1 m²
Axial rigidity EA: 408.4 N
Equivalent modulus: 16.2 Pa
```

Displayed values are rounded; checks use the unrounded expressions.

## Limits and remaining scope

This function assumes perfect bonding and does not check bond strength or
establish that a real assembly meets the equal-strain assumption. It omits
Poisson constraints, anisotropy, bending, failure and unbonded flexible-pipe
mechanics. It provides no DNV-F202 qualification or design acceptance.
Remaining issue scope includes an independently sourced standards methodology,
composite constitutive behaviour, failure checks and qualification evidence.
