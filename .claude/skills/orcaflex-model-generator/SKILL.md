---
name: orcaflex-model-generator
description: Generate OrcaFlex modular models from spec.yml using builder registry pattern with conditional generation and cross-builder context sharing.
version: 2.0.0
updated: 2026-02-10
category: offshore-engineering
triggers:
- generate OrcaFlex model
- create riser model
- modular model generation
- spec.yml to OrcaFlex
- builder registry
- component assembly
- parametric model generation
---
# OrcaFlex Modular Model Generator

Generate complete OrcaFlex models from `spec.yml` using the builder registry pattern. Each builder is a self-contained module that generates one YAML section, registered with execution order for dependency resolution.

## Architecture

### Builder Registry Pattern

```python
@BuilderRegistry.register("03_environment.yml", order=30)
class EnvironmentBuilder(BaseBuilder):
    def should_generate(self) -> bool:
        return True  # Always needed

    def build(self) -> dict[str, Any]:
        return {"Environment": {...}}
```

Builders self-register via the `@register` decorator. The orchestrator iterates them in order without maintaining a hardcoded list.

### Core Classes

| Class | Location | Purpose |
|-------|----------|---------|
| `ModularModelGenerator` | `__init__.py` | Orchestrator: loads spec, runs builders, writes output |
| `BuilderRegistry` | `builders/registry.py` | Auto-discovery registry with ordered execution |
| `BaseBuilder` | `builders/base.py` | ABC with `build()`, `should_generate()`, entity sharing |
| `BuilderContext` | `builders/context.py` | Typed dataclass for cross-builder data sharing |
| `ProjectInputSpec` | `schema/root.py` | Pydantic root model (metadata + environment + simulation + generic/pipeline/riser) |

### Builder Execution Flow

```
1. ModularModelGenerator.generate(output_dir)
2.   for (filename, builder_class) in BuilderRegistry.get_ordered_builders():
3.     builder = builder_class(spec, context)
4.     if not builder.should_generate(): continue
5.     data = builder.build()
6.     context.update_from_dict(builder.get_generated_entities())
7.     yaml.dump(data, includes_dir / filename)
8.   write master.yml with include directives
```

### Cross-Builder Entity Sharing

Builders register entities via `_register_entity(key, value)` for downstream builders:

```python
# VesselBuilder registers vessel name
self._register_entity("main_vessel_name", vessel.name)

# LinesBuilder reads it from context
vessel_name = self.context.main_vessel_name
```

## Builder Types

### Pipeline Builders (order 10-50)

| Builder | File | Order | Purpose |
|---------|------|-------|---------|
| GeneralBuilder | `01_general.yml` | 10 | Simulation settings |
| EnvironmentBuilder | `03_environment.yml` | 30 | Water, waves, current, wind |
| VesselTypeBuilder | `04_vessel_types.yml` | 31 | Vessel type definitions |
| LineTypeBuilder | `05_line_types.yml` | 32 | Line type properties |
| ... | ... | ... | ... |

### Riser Builders (order 40-93)

| Builder | File | Order | Condition |
|---------|------|-------|-----------|
| RiserLineTypeBuilder | `06_riser_line_types.yml` | 40 | `spec.is_riser()` |
| RiserVesselBuilder | `06_riser_vessels.yml` | 42 | `spec.is_riser()` |
| RiserClumpTypeBuilder | `06_riser_clump_types.yml` | 50 | `spec.is_riser()` |
| RiserLinesBuilder | `06_riser_lines.yml` | 92 | `spec.is_riser()` |
| RiserLinksBuilder | `06_riser_links.yml` | 93 | `spec.is_riser()` |

### Generic Builder (order 200)

| Builder | File | Order | Condition |
|---------|------|-------|-----------|
| GenericModelBuilder | `20_generic_objects.yml` | 200 | `spec.is_generic()` |

The generic builder handles ALL OrcaFlex object types via schema-driven generation:
- List sections (LineTypes, Vessels, Lines, Shapes, 6DBuoys, etc.)
- Singleton sections (FrictionCoefficients, LineContactData, Groups, etc.)
- VariableData (nested by category)
- General properties overlay

## spec.yml: The Foundation Input

`spec.yml` is the portable, human-readable input for any OrcaFlex model:

```yaml
metadata:
  name: lazy_wave_riser
  description: Lazy wave riser configuration
  structure: generic      # or: pipeline, riser
  operation: generic

environment:
  water:
    depth: 450
    density: 1.025
  waves:
    type: dean_stream
    height: 5.0
    period: 10.0
    direction: 180
  current:
    speed: 0.5
    direction: 180
    profile: [[0, 1.0], [450, 0.3]]
  wind:
    speed: 10
    direction: 180

simulation:
  time_step: 0.1
  stages: [8, 16]

generic:                  # All OrcaFlex objects
  general_properties:
    UnitsSystem: SI
  line_types:
    - name: "Riser LT"
      category: General
      properties:
        MassPerUnitLength: 50
        EI: 10000
        EA: 500000
  vessels:
    - name: "FPSO"
      vessel_type: "Vessel Type1"
      length: 103
  lines:
    - name: "Riser"
      properties:
        LineType: "Riser LT"
        Length: [200, 300, 200]
```

### Spec Variants

| `structure` | Schema Field | Builders Active |
|-------------|-------------|-----------------|
| `pipeline` | `spec.pipeline` | Pipeline builders (order 10-50) |
| `riser` | `spec.riser` | Riser builders (order 40-93) |
| `generic` | `spec.generic` | GenericModelBuilder (order 200) |

## Usage

### From spec.yml file

```python
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator

gen = ModularModelGenerator(Path("spec.yml"))
gen.generate(Path("output/"))
```

### From in-memory spec

```python
spec = ProjectInputSpec(**spec_dict)
gen = ModularModelGenerator.from_spec(spec)
gen.generate(Path("output/"))
```

### From monolithic (extract + generate)

```python
from digitalmodel.solvers.orcaflex.modular_generator.extractor import MonolithicExtractor

ext = MonolithicExtractor(Path("model.yml"))
spec = ProjectInputSpec(**ext.extract())
gen = ModularModelGenerator.from_spec(spec)
gen.generate(Path("output/"))
```

### With section overrides

```python
result = gen.generate_with_overrides(
    output_dir=Path("output/"),
    sections=[override_section],
    variables={"water_depth": 500},
)
```

## Generic Builder Internals

### _merge_object()

Combines typed Pydantic fields with pass-through properties:

```python
@staticmethod
def _merge_object(obj: GenericObject) -> dict[str, Any]:
    merged = dict(obj.properties)           # 1. Start from properties bag
    explicitly_set = obj.model_fields_set    # 2. Track what was explicitly set

    for py_field, ofx_key in TYPED_FIELD_MAP.items():
        value = getattr(obj, py_field, None)
        if value is not None:
            merged[ofx_key] = value          # 3. Non-None typed fields override
        elif py_field in explicitly_set:
            merged[ofx_key] = value          # 4. Explicitly-set None preserved

    # 5. Priority keys first (Name, Category, ShapeType, etc.)
    ordered = {}
    for key in _PRIORITY_KEYS:
        if key in merged:
            ordered[key] = merged.pop(key)
    ordered.update(merged)
    return ordered
```

### Section Ordering (_SECTION_ORDER)

Critical: OrcaFlex validates references sequentially. Sections must appear in dependency order:

```
General → VariableData → ExpansionTables
→ RayleighDampingCoefficients, FrictionCoefficients, LineContactData
→ LineTypes, VesselTypes, ClumpTypes, StiffenerTypes, SupportTypes
→ Vessels, Lines, Shapes, 6DBuoys, 3DBuoys, Constraints, Links, Winches
→ MultibodyGroups, BrowserGroups, Groups
```

### Priority Keys (_PRIORITY_KEYS)

Mode-setting properties must appear before dependent properties within each object:

```python
_PRIORITY_KEYS = [
    "Name", "Category", "ShapeType", "Shape", "BuoyType",
    "Connection", "LinkType", "Geometry", "WaveType",
    "DegreesOfFreedomInStatics",
]
```

## 3-Way Benchmark

The benchmark validates three paths produce equivalent results:

| Path | Source | Pipeline |
|------|--------|----------|
| **A (monolithic)** | `.dat` file | Load directly in OrcFxAPI |
| **B (spec-driven)** | `.dat` → `.yml` → extract → spec → generate → load | Full round-trip |
| **C (library-direct)** | `spec.yml` → generate → load | From hand-written spec |

```bash
uv run python scripts/benchmark_model_library.py --library-only --three-way --skip-mesh
```

Results (2026-02-10): All 5 library models converge on all 3 paths with 0.00% tension difference.

## Related Skills

- [orcaflex-monolithic-to-modular](../orcaflex-monolithic-to-modular/SKILL.md) - Extraction pipeline
- [orcaflex-yaml-gotchas](../orcaflex-yaml-gotchas/SKILL.md) - Production YAML traps
- [orcaflex-environment-config](../orcaflex-environment-config/SKILL.md) - Environment builder details

## References

- Generator: `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py`
- Registry: `src/digitalmodel/solvers/orcaflex/modular_generator/builders/registry.py`
- Base: `src/digitalmodel/solvers/orcaflex/modular_generator/builders/base.py`
- Generic builder: `src/digitalmodel/solvers/orcaflex/modular_generator/builders/generic_builder.py`
- Schema: `src/digitalmodel/solvers/orcaflex/modular_generator/schema/`
- Spec library: `docs/modules/orcaflex/library/tier2_fast/`
- Benchmark: `scripts/benchmark_model_library.py`

---

## Version History

- **2.0.0** (2026-02-10): Complete rewrite. Documents actual ModularModelGenerator, builder registry, generic builder internals, _merge_object() with model_fields_set, section ordering, 3-way benchmark.
- **1.0.0** (2026-01-07): Initial release describing theoretical component lookup approach.
