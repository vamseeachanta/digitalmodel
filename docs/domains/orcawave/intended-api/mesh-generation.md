# Specification: OrcaWave mesh generation

> **Status: NOT IMPLEMENTED.** This is a specification of intended
> capability, lifted verbatim from the AceEngineer marine-offshore skill
> corpus (`engineering/marine-offshore/orcawave/mesh-generation`). None of the
> API below exists in `digitalmodel` today.
>
> It lives here rather than in the skill corpus because a specification
> belongs with the codebase that would implement it — in a skill corpus an
> agent may act on it as though it runs. Tracked in
> `aceengineer-strategy#267`.

## Absent API surface

- `digitalmodel.orcawave.converters`
- `digitalmodel.orcawave.converters.STLtoGDFConverter`
- `digitalmodel.orcawave.mesh`
- `digitalmodel.orcawave.mesh.OrcaWaveMeshGenerator`
- `digitalmodel.orcawave.mesh.WaterlineRefiner`
- `digitalmodel.orcawave.mesh_study`
- `digitalmodel.orcawave.mesh_study.MeshConvergenceStudy`

## Intended usage (verbatim from the skill)

### Basic Mesh Generation

```python
from digitalmodel.orcawave.mesh import OrcaWaveMeshGenerator

# Initialize generator
generator = OrcaWaveMeshGenerator()

# Load CAD geometry
generator.load_geometry("geometry/hull.stl")

# Generate panel mesh

*See sub-skills for full details.*
### Mesh Convergence Study
```

### STL to GDF Conversion

```python
from digitalmodel.orcawave.converters import STLtoGDFConverter

# Initialize converter
converter = STLtoGDFConverter()

# Convert with options
converter.convert(
    input_file="geometry/hull.stl",
    output_file="geometry/hull.gdf",

*See sub-skills for full details.*
### Waterline Refinement
```

## Notes for an implementer

- The snippets are **intent, not contract**. Names and signatures were
  written against an API that was never built, so treat them as a starting
  point rather than a spec to match exactly.
- The engineering content that surrounded them — when to use this analysis,
  what to watch for — stays in the skill corpus and is unaffected.
- If a capability here is built, remove it from the skill's
  `ace:known-missing` marker so the corpus check reflects reality.
