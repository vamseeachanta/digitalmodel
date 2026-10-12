# Specification: OrcaWave to orcaflex

> **Status: NOT IMPLEMENTED.** This is a specification of intended
> capability, lifted verbatim from the AceEngineer marine-offshore skill
> corpus (`engineering/marine-offshore/orcawave/to-orcaflex`). None of the
> API below exists in `digitalmodel` today.
>
> It lives here rather than in the skill corpus because a specification
> belongs with the codebase that would implement it — in a skill corpus an
> agent may act on it as though it runs. Tracked in
> `aceengineer-strategy#267`.

## Absent API surface

- `digitalmodel.diffraction.orcawave_converter`
- `digitalmodel.diffraction.orcawave_converter.OrcaWaveConverter`
- `digitalmodel.orcawave.coordinate_transform`
- `digitalmodel.orcawave.coordinate_transform.CoordinateTransformer`
- `digitalmodel.orcawave.orcaflex_export`
- `digitalmodel.orcawave.orcaflex_export.HydrodynamicDatabaseCreator`
- `digitalmodel.orcawave.orcaflex_export.OrcaWaveToOrcaFlex`
- `digitalmodel.orcawave.rao_import`
- `digitalmodel.orcawave.rao_import.RAOImporter`

## Intended usage (verbatim from the skill)

### Basic Conversion

```python
from digitalmodel.diffraction.orcawave_converter import OrcaWaveConverter
from digitalmodel.hydrodynamics.diffraction.orcaflex_exporter import OrcaFlexExporter

# Load OrcaWave results
import OrcFxAPI

# Option 1: From OrcaWave model directly
orcawave_model = OrcFxAPI.DiffractionModel("models/fpso.owr")
vessel = orcawave_model.Vessel

*See sub-skills for full details.*
### With Viscous Damping
```

### Full Hydrodynamic Database

```python
from digitalmodel.orcawave.orcaflex_export import HydrodynamicDatabaseCreator

# Create complete hydrodynamic database
db_creator = HydrodynamicDatabaseCreator()

# Load all loading conditions
db_creator.add_condition(
    name="full_load",
    orcawave_file="models/fpso_full.owr",

*See sub-skills for full details.*
### RAO Import with Validation
```

## Notes for an implementer

- The snippets are **intent, not contract**. Names and signatures were
  written against an API that was never built, so treat them as a starting
  point rather than a spec to match exactly.
- The engineering content that surrounded them — when to use this analysis,
  what to watch for — stays in the skill corpus and is unaffected.
- If a capability here is built, remove it from the skill's
  `ace:known-missing` marker so the corpus check reflects reality.
