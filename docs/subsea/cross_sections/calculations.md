# Concentric section calculations

The [module analysis](../../reports/modules/cross_sections/index.html) gives the
worked steel, coating and insulation example, generated figures, inputs, equation
sources and limitations. Rebuild it from the repository root:

```bash
PYTHONPATH=src python scripts/reporting/build_cross_sections_module_report.py
```

The calculation interface accepts metres, kg/m³, m/s² and Pa. Density is required
for mass; water density, gravity and bore condition are required for submerged
weight; one Young's modulus per material layer is required for stiffness. No
material property is inferred from a material name. Geometry in the fixture
schema can use other supported units, but calculations reject it until the caller
explicitly converts to metres. The package does not use `digitalmodel.units`.

For example, an assumed steel annulus with OD 273.1 mm and ID 246.1 mm can be
calculated as follows. Steel density comes from the steel row of
[Engineering ToolBox, Metals and Alloys — Densities (2004, updated 2025)](https://www.engineeringtoolbox.com/metal-alloys-densities-d_50.html).
The metre conversion uses milli = 10⁻³ from the
[BIPM SI Brochure, ninth edition (2019), Table 7](https://www.bipm.org/documents/20126/41483022/SI-Brochure-9.pdf).

```python
from digitalmodel.subsea.cross_sections.calculations import calculate_annular_layer
from digitalmodel.subsea.cross_sections.schema import Provenance, RadialLayer, UnitValue

steel = RadialLayer(
    name="illustrative steel wall",
    role="line_pipe",
    material="steel",
    inner_diameter=UnitValue(value=0.2461, unit="m"),
    outer_diameter=UnitValue(value=0.2731, unit="m"),
    density=UnitValue(value=7850, unit="kg/m^3"),
    provenance=Provenance(
        source_id="illustrative-annulus",
        source_type="project_assumption",
        citation="Steel density: Engineering ToolBox (2004), steel row",
        note="Geometry assumed, based on previous experience; not a design default.",
    ),
)
properties = calculate_annular_layer(steel)
print(properties.area_m2, properties.mass_per_length_kg_m)
```

The independent hand calculation is
`A = π(0.2731 − 0.2461)(0.2731 + 0.2461)/4 = 0.0035046π m²` and
`m/L = 7850 A = 27.51111π kg/m`.
Circle area follows [OpenStax Prealgebra 2e (2020), §9.5](https://openstax.org/books/prealgebra-2e/pages/9-5-solve-geometry-applications-circles-and-irregular-figures);
mass follows `ρ = m/V` in
[OpenStax University Physics Volume 1 (2016), §14.1](https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure).
These arithmetic references are checked in `test_section_values.py`.

`calculate_radial_section(layers, water_density_kg_m3=..., gravity_m_s2=...,
bore_condition=...)` returns per-layer area and mass, total dry mass, outer
diameter, displaced area and downward-positive submerged weight. Layers must be
ordered and have exactly shared boundaries in metres. A sealed bore is empty
and displaces its complete envelope. A freely flooded bore displaces only the
material area. Contents mass is excluded from both conventions.

`calculate_elastic_section(layers, youngs_moduli_pa=[...])` returns material area,
centroidal second moment, geometric section modulus, EA and EI. It assumes
perfect bonding, common axial strain and common curvature. Density is unnecessary
for these elastic properties. Geometric section modulus alone cannot establish
stress distribution or capacity in dissimilar materials.

The representative packaged fixtures preserve native units and provenance when
round-tripped through YAML and JSON. They lack complete material properties:
their mass and stiffness are **NOT_EVALUATED**. Fluid-envelope fixture layers must
not be treated as solid material by inference. Packed cable and umbilical
mechanics, strength, buckling, fatigue and design-code compliance are also
**NOT_EVALUATED**.

## Validation errors

`validate_cross_section` returns `is_valid`, `errors`, `warnings` and `summary`.
Each error carries a Pydantic code, a dotted field/index path, a message and
`severity="error"`. Definition-wide errors use `path="definition"`. A valid
report has empty error and warning lists. Validation rechecks current data even
when the input is an already constructed, mutable model.

| Rule group | Rejected input | Regression evidence |
|---|---|---|
| Scalars and units | Non-finite values, unknown units, missing units, semantic unit mismatches | `test_schema.py`, `test_section_values.py`, `test_fixture_contracts.py` |
| Provenance | Blank source identifier, calculation without dependencies, undeclared layer or component source, empty definition sources | `test_schema.py`, `test_fixture_contracts.py` |
| Radial geometry | Missing geometry terms, non-positive outer diameter or thickness, negative bore, inconsistent triplet, mixed units, gap or overlap | `test_validation.py`, `test_fixture_contracts.py`, `test_section_values.py` |
| Packed components | Missing provenance, count below its schema minimum, blank required text, invalid dimensions or ratings | `test_validation.py`, `test_schema.py`, `test_fixture_contracts.py` |
| Metadata | Negative depth, non-positive pressure or voltage, incompatible units | `test_schema.py`, `test_fixture_contracts.py` |
| Definition | Unknown family, blank identifier or name, missing radial layers or packed components for the selected family | `test_validation.py`, `test_fixture_contracts.py` |
| Report conversion | Root and nested error locations, fallback issue fields, validation after mutation, valid object and mapping inputs | `test_fixture_contracts.py` |

The calculation API additionally rejects absent material/environment inputs,
non-contiguous metre-valued boundaries and results outside floating-point
representation. No physical acceptance tolerance is inferred from schema
consistency checks or numerical test tolerances.
