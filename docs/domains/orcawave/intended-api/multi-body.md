# Specification: OrcaWave multi body

> **Status: NOT IMPLEMENTED.** This is a specification of intended
> capability, lifted verbatim from the AceEngineer marine-offshore skill
> corpus (`engineering/marine-offshore/orcawave/multi-body`). None of the
> API below exists in `digitalmodel` today.
>
> It lives here rather than in the skill corpus because a specification
> belongs with the codebase that would implement it — in a skill corpus an
> agent may act on it as though it runs. Tracked in
> `aceengineer-strategy#267`.

## Absent API surface

- `digitalmodel.orcawave.multibody`
- `digitalmodel.orcawave.multibody.CouplingMatrixExtractor`
- `digitalmodel.orcawave.multibody.GapResonanceAnalyzer`
- `digitalmodel.orcawave.multibody.MultiBodyAnalysis`
- `digitalmodel.orcawave.multibody.ShieldingAnalyzer`
- `digitalmodel.orcawave.multibody.SideBySideAnalysis`

## Intended usage (verbatim from the skill)

### Basic Multi-Body Setup

```python
from digitalmodel.orcawave.multibody import MultiBodyAnalysis

# Initialize multi-body analysis
mb = MultiBodyAnalysis()

# Add primary vessel (FPSO)
mb.add_body(
    name="FPSO",
    mesh_file="geometry/fpso_panels.gdf",

*See sub-skills for full details.*
### Gap Resonance Analysis
```

### Side-by-Side Operations

```python
from digitalmodel.orcawave.multibody import SideBySideAnalysis

# Initialize STS analysis
sts = SideBySideAnalysis()

# Configure vessels
sts.configure_fpso(
    mesh="geometry/fpso.gdf",
    loa=300.0,

*See sub-skills for full details.*
### Hydrodynamic Coupling Matrices
```

## Notes for an implementer

- The snippets are **intent, not contract**. Names and signatures were
  written against an API that was never built, so treat them as a starting
  point rather than a spec to match exactly.
- The engineering content that surrounded them — when to use this analysis,
  what to watch for — stays in the skill corpus and is unaffected.
- If a capability here is built, remove it from the skill's
  `ace:known-missing` marker so the corpus check reflects reality.
