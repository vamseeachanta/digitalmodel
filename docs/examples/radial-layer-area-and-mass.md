# Radial-layer area and dry-mass coverage audit example

This executable example checks the area and dry mass per unit axial length of a
single homogeneous, concentric radial layer. Geometry and dry mass do not
establish strength or code compliance. No material lookup or external input file
is used.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| Initial | 2026-10-26 | Add executable examples to connect existing arithmetic tests to a documented calculation basis. |
| 1 | 2026-10-27 | State the affected files, executable cases, acceptance checks and excluded decisions in an explicit Plan-lite section. |

## Plan-lite

- Affected files: `docs/examples/radial-layer-area-and-mass.md` defines the
  executable examples; `tests/subsea/cross_sections/test_calculations.py`
  executes and checks them.
- Executable cases: annulus, solid core and missing-density refusal, each with
  fresh shared setup and explicit synthetic-input provenance.
- Acceptance checks: independent area and dry-mass references within the existing
  rounding tolerance, expected printed output, explicit missing-density refusal
  and all 14 calculation tests passing. File-level repository checks also apply.
- Excluded engineering decisions: material selection, buoyancy, submerged weight,
  corrosion allowance, bending or pressure capacity, multilayer interaction,
  packed-component properties and design-code acceptance.

## Coverage gap and scope

The existing calculation tests cover an annulus, equivalent diameter/thickness
inputs, a solid core, missing density, non-metre dimensions, a negative derived
bore and unrepresentable results. The gap addressed here is a runnable example
with explicit provenance and independent references, tested directly from this
document. Existing tests and their tolerances are retained.

This is a bounded contribution to #1817, which remains open. Remaining scope
includes other subsea calculations, geotechnical and cathodic protection
coverage, and solver integrations. No other module is audited here.

## Input basis and independent calculation

All dimensions and densities are synthetic arithmetic assumptions defined in
this example; they are not measured geometry or properties of a named material.
The same provenance record accompanies geometry and density.

| Case | Inner diameter (m) | Outer diameter (m) | Density (kg/m³) | Input source |
|---|---|---|---|---|
| Annulus | 2 | 4 | 10 | Synthetic arithmetic assumptions in this example |
| Solid core | 0 | 2 | 10 | Synthetic arithmetic assumptions in this example |
| Missing density | 2 | 4 | Not supplied | Annulus geometry with density deliberately omitted |

For the independent reference, subtract circle areas using
`A = πR² − πr²`, where `R = outer_diameter / 2` and
`r = inner_diameter / 2`. The circle formula and diameter/radius relation are
given in [OpenStax, Prealgebra 2e, section 9.5](https://openstax.org/books/prealgebra-2e/pages/9-5-solve-geometry-applications-circles-and-irregular-figures).
The divisor and squared exponent follow from those relations.

For a uniform layer of axial length `L`, `V = A L`. Therefore `ρ = m/V`
gives `m/L = ρ A`, using the density definition in
[OpenStax, University Physics Volume 1, section 14.1, equation 14.1](https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure).
Metres and kg/m³ produce m² and kg/m respectively.

Substituting the synthetic inputs independently gives:

- Annulus: `R = 2 m`, `r = 1 m`, so `A = (4 − 1)π = 3π m²` and
  `m/L = 10 × 3π = 30π kg/m`.
- Solid core: `R = 1 m`, `r = 0 m`, so `A = (1 − 0)π = π m²` and
  `m/L = 10 × π = 10π kg/m`.
- Missing density: geometry still defines area, but this combined area-and-mass
  function refuses the calculation because dry mass has no supplied basis.

The snippets compare against these symbolic references before rounding output
to one decimal place. Their relative tolerance `1e-12` follows the existing
arithmetic tests' floating-point rounding convention; it is not an engineering
acceptance tolerance.

## Run the examples

With the package dependencies available, run the setup below followed by any
one case in the same Python interpreter. Alternatively, paste the setup and a
case into a script and run `PYTHONPATH=src uv run python example.py` from the
repository root. The tests execute these exact Python fences, with fresh setup
for each case.

### Shared setup

```python
import math

from digitalmodel.subsea.cross_sections.calculations import calculate_annular_layer
from digitalmodel.subsea.cross_sections.schema import Provenance, RadialLayer, UnitValue


def example_layer(inner_diameter_m, outer_diameter_m, density_kg_m3):
    return RadialLayer(
        name="synthetic radial layer",
        role="arithmetic example",
        material="unspecified",
        inner_diameter=UnitValue(value=inner_diameter_m, unit="m"),
        outer_diameter=UnitValue(value=outer_diameter_m, unit="m"),
        density=(
            None
            if density_kg_m3 is None
            else UnitValue(value=density_kg_m3, unit="kg/m^3")
        ),
        provenance=Provenance(
            source_id="synthetic-arithmetic-example",
            source_type="project_assumption",
            citation="Radial-layer area and dry-mass example: input basis",
            note="Synthetic geometry and density; no material-property claim.",
        ),
    )
```

### Annulus

```python
# Inputs and independent circle-subtraction reference: input basis above.
layer = example_layer(2, 4, 10)
result = calculate_annular_layer(layer)
assert math.isclose(result.area_m2, 3 * math.pi, rel_tol=1e-12)
assert math.isclose(result.mass_per_length_kg_m, 30 * math.pi, rel_tol=1e-12)
print(f"area={result.area_m2:.1f} m²; dry mass/length={result.mass_per_length_kg_m:.1f} kg/m")
```

Expected output: `area=9.4 m²; dry mass/length=94.2 kg/m`.

### Solid core

```python
# Zero bore and synthetic density: input basis above.
layer = example_layer(0, 2, 10)
result = calculate_annular_layer(layer)
assert math.isclose(result.area_m2, math.pi, rel_tol=1e-12)
assert math.isclose(result.mass_per_length_kg_m, 10 * math.pi, rel_tol=1e-12)
print(f"area={result.area_m2:.1f} m²; dry mass/length={result.mass_per_length_kg_m:.1f} kg/m")
```

Expected output: `area=3.1 m²; dry mass/length=31.4 kg/m`.

### Missing-density refusal

```python
# Same synthetic annulus geometry; density is deliberately absent.
layer = example_layer(2, 4, None)
try:
    calculate_annular_layer(layer)
except ValueError as error:
    refusal_message = str(error)
    assert refusal_message == "density is required to calculate mass per length"
    print(refusal_message)
else:
    raise AssertionError("missing density must refuse the calculation")
```

Expected output: `density is required to calculate mass per length`.

## Automated check and limitations

Run `PYTHONPATH=src uv run python -m pytest tests/subsea/cross_sections/test_calculations.py -q -rA -p no:cacheprovider`.
The added `test_documented_radial_layer_example` cases are `annulus`,
`solid-core` and `missing-density`. Each executes shared setup plus the relevant
snippet; the test also checks independent numerical references and the output.

The function requires every supplied radial dimension to use metres and density
to use kg/m³. The example does not determine buoyancy, submerged weight,
corrosion allowance, bending or pressure capacity, multilayer interaction,
packed-component properties or design-code acceptance. Further engineering
inputs and the applicable checks are needed for those decisions.
