# Specification: OrcaWave analysis

> **Status: NOT IMPLEMENTED.** This is a specification of intended
> capability, lifted verbatim from the AceEngineer marine-offshore skill
> corpus (`engineering/marine-offshore/orcawave/analysis`). None of the
> API below exists in `digitalmodel` today.
>
> It lives here rather than in the skill corpus because a specification
> belongs with the codebase that would implement it — in a skill corpus an
> agent may act on it as though it runs. Tracked in
> `aceengineer-strategy#267`.

## Absent API surface

- `digitalmodel.orcawave.batch`
- `digitalmodel.orcawave.batch.OrcaWaveBatch`
- `digitalmodel.orcawave.mesh_study`
- `digitalmodel.orcawave.mesh_study.MeshConvergenceStudy`
- `digitalmodel.orcawave.orcaflex_export`
- `digitalmodel.orcawave.orcaflex_export.OrcaFlexExporter`
- `digitalmodel.orcawave.orcawave_analysis`
- `digitalmodel.orcawave.orcawave_analysis.OrcaWaveAnalysis`

## Intended usage (verbatim from the skill)

### Basic Analysis

```python
from digitalmodel.orcawave.orcawave_analysis import OrcaWaveAnalysis

# Initialize analysis
orcawave = OrcaWaveAnalysis()

# Configure analysis
config = {
    "vessel_mesh": "geometry/hull_panels.dat",
    "water_depth": 1000.0,

*See sub-skills for full details.*
### Batch Processing
```

### OrcaFlex Integration

```python
from digitalmodel.orcawave.orcaflex_export import OrcaFlexExporter

# Initialize exporter
exporter = OrcaFlexExporter()

# Load OrcaWave results
exporter.load_results("orcawave_results/vessel.dat")

# Export to OrcaFlex hydrodynamic database

*See sub-skills for full details.*
### Mesh Convergence Study
```

## Notes for an implementer

- The snippets are **intent, not contract**. Names and signatures were
  written against an API that was never built, so treat them as a starting
  point rather than a spec to match exactly.
- The engineering content that surrounded them — when to use this analysis,
  what to watch for — stays in the skill corpus and is unaffected.
- If a capability here is built, remove it from the skill's
  `ace:known-missing` marker so the corpus check reflects reality.
