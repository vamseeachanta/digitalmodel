# Specification: OrcaWave damping sweep

> **Status: NOT IMPLEMENTED.** This is a specification of intended
> capability, lifted verbatim from the AceEngineer marine-offshore skill
> corpus (`engineering/marine-offshore/orcawave/damping-sweep`). None of the
> API below exists in `digitalmodel` today.
>
> It lives here rather than in the skill corpus because a specification
> belongs with the codebase that would implement it — in a skill corpus an
> agent may act on it as though it runs. Tracked in
> `aceengineer-strategy#267`.

## Absent API surface

- `digitalmodel.orcawave.damping`
- `digitalmodel.orcawave.damping.BilgeKeelDamping`
- `digitalmodel.orcawave.damping.CriticalDampingCalculator`
- `digitalmodel.orcawave.damping.DampingPeriodAnalyzer`
- `digitalmodel.orcawave.damping.DampingSweep`
- `digitalmodel.orcawave.damping.ModelTestComparison`
- `digitalmodel.orcawave.damping.MultiParameterDampingSweep`

## Intended usage (verbatim from the skill)

### Basic Damping Sweep

```python
from digitalmodel.orcawave.damping import DampingSweep

# Initialize sweep
sweep = DampingSweep()

# Load base model
sweep.load_model("models/fpso.owr")

# Define damping values to sweep

*See sub-skills for full details.*
### Multi-Parameter Sweep
```

### Critical Damping Calculation

```python
from digitalmodel.orcawave.damping import CriticalDampingCalculator

# Initialize calculator
calc = CriticalDampingCalculator()

# Load model with mass and stiffness
calc.load_model("models/fpso.owr")

# Calculate critical damping for each DOF

*See sub-skills for full details.*
### Model Test Comparison
```

### Bilge Keel Estimation

```python
from digitalmodel.orcawave.damping import BilgeKeelDamping

# Initialize bilge keel damping estimator
bk = BilgeKeelDamping()

# Configure vessel parameters
bk.configure_vessel(
    beam=50.0,          # m
    draft=22.0,         # m

*See sub-skills for full details.*
### Damping-Period Relationship
```

## Notes for an implementer

- The snippets are **intent, not contract**. Names and signatures were
  written against an API that was never built, so treat them as a starting
  point rather than a spec to match exactly.
- The engineering content that surrounded them — when to use this analysis,
  what to watch for — stays in the skill corpus and is unaffected.
- If a capability here is built, remove it from the skill's
  `ace:known-missing` marker so the corpus check reflects reality.
