# Specification: OrcaWave qtf analysis

> **Status: NOT IMPLEMENTED.** This is a specification of intended
> capability, lifted verbatim from the AceEngineer marine-offshore skill
> corpus (`engineering/marine-offshore/orcawave/qtf-analysis`). None of the
> API below exists in `digitalmodel` today.
>
> It lives here rather than in the skill corpus because a specification
> belongs with the codebase that would implement it — in a skill corpus an
> agent may act on it as though it runs. Tracked in
> `aceengineer-strategy#267`.

## Absent API surface

- `digitalmodel.orcawave.qtf`
- `digitalmodel.orcawave.qtf.FullQTFComputation`
- `digitalmodel.orcawave.qtf.MeanDriftAnalyzer`
- `digitalmodel.orcawave.qtf.NewmanApproximation`
- `digitalmodel.orcawave.qtf.OrcaWaveQTF`
- `digitalmodel.orcawave.qtf.SlowDriftResponse`

## Intended usage (verbatim from the skill)

### Basic QTF Computation

```python
from digitalmodel.orcawave.qtf import OrcaWaveQTF

# Initialize QTF analysis
qtf = OrcaWaveQTF()

# Load OrcaWave model with first-order results
qtf.load_model("models/fpso.owr")

# Configure QTF computation

*See sub-skills for full details.*
### Full QTF Matrix Generation
```

### Newman Approximation

```python
from digitalmodel.orcawave.qtf import NewmanApproximation

# Initialize Newman approximation
newman = NewmanApproximation()

# Load first-order results
newman.load_first_order_results("results/fpso_raos.csv")

# Compute approximate QTF

*See sub-skills for full details.*
### Mean Drift Analysis
```

## Notes for an implementer

- The snippets are **intent, not contract**. Names and signatures were
  written against an API that was never built, so treat them as a starting
  point rather than a spec to match exactly.
- The engineering content that surrounded them — when to use this analysis,
  what to watch for — stays in the skill corpus and is unaffected.
- If a capability here is built, remove it from the skill's
  `ace:known-missing` marker so the corpus check reflects reality.
