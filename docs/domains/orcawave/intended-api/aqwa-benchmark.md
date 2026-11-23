# Specification: OrcaWave aqwa benchmark

> **Status: NOT IMPLEMENTED.** This is a specification of intended
> capability, lifted verbatim from the AceEngineer marine-offshore skill
> corpus (`engineering/marine-offshore/orcawave/aqwa-benchmark`). None of the
> API below exists in `digitalmodel` today.
>
> It lives here rather than in the skill corpus because a specification
> belongs with the codebase that would implement it — in a skill corpus an
> agent may act on it as though it runs. Tracked in
> `aceengineer-strategy#267`.

## Absent API surface

- `digitalmodel.diffraction.comparison_framework`
- `digitalmodel.diffraction.comparison_framework.MatrixComparator`
- `digitalmodel.diffraction.comparison_framework.PeakRAOComparator`
- `digitalmodel.diffraction.comparison_framework.StatisticalAnalyzer`
- `digitalmodel.diffraction.orcawave_converter`
- `digitalmodel.diffraction.orcawave_converter.OrcaWaveConverter`

## Intended usage (verbatim from the skill)

### Basic Comparison

```python
from digitalmodel.diffraction.comparison_framework import (
    DiffractionComparator,
    PeakRAOComparator
)
from digitalmodel.hydrodynamics.diffraction.aqwa_converter import AQWAConverter
from digitalmodel.diffraction.orcawave_converter import OrcaWaveConverter

# Load AQWA results
aqwa_converter = AQWAConverter()

*See sub-skills for full details.*
### Peak-Focused Validation
```

### Matrix Comparison

```python
from digitalmodel.diffraction.comparison_framework import MatrixComparator

# Compare added mass matrices
matrix_comp = MatrixComparator()

# Compare at specific frequency
freq = 0.1  # rad/s
am_comparison = matrix_comp.compare_added_mass(
    aqwa_matrix=aqwa_results['added_mass'][freq],

*See sub-skills for full details.*
### Statistical Analysis
```

## Notes for an implementer

- The snippets are **intent, not contract**. Names and signatures were
  written against an API that was never built, so treat them as a starting
  point rather than a spec to match exactly.
- The engineering content that surrounded them — when to use this analysis,
  what to watch for — stays in the skill corpus and is unaffected.
- If a capability here is built, remove it from the skill's
  `ace:known-missing` marker so the corpus check reflects reality.
