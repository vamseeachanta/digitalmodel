# Dynacard -- Sucker Rod Pump Analysis Toolkit

Dynacard is a sucker rod pump (SRP) analysis module that transforms surface dynamometer
card data into downhole diagnostics, engineering calculations, and pump health assessments.

- 21 Python source files, ~5,600 LOC
- Two physics solvers: Gibbs (frequency-domain) and Finite Difference (time-domain)
- 12 specialized calculators built on a generic `BaseCalculator[T]` pattern
- 30+ Pydantic v2 data models
- 400+ passing tests

## Quickstart

```python
from digitalmodel.marine_ops.artificial_lift.dynacard import (
    DynacardAnalysisContext, CardData, RodSection, PumpProperties, SurfaceUnit,
    DynacardWorkflow,
)

# Build context
ctx = DynacardAnalysisContext(
    api14="WELL-001",
    surface_card=CardData(position=[...], load=[...]),
    rod_string=[RodSection(diameter=1.0, length=5000)],
    pump=PumpProperties(diameter=1.75, depth=5000),
    spm=10.0,
)

# Run full analysis (physics + calculations + diagnostics)
workflow = DynacardWorkflow(ctx)
results = workflow.run_full_analysis()

print(results.diagnostic_message)
print(results.pump_fillage)
print(results.inferred_production)
```

### Compare solvers

```python
workflow = DynacardWorkflow(ctx)
comparison = workflow.compare_solvers()
print(comparison["comparison"])  # stroke_diff_pct, load_rmse_pct
```

### Individual calculators

Each calculator can be invoked independently via its convenience function:

```python
from digitalmodel.marine_ops.artificial_lift.dynacard import (
    calculate_gear_box_loading,
    calculate_power_consumption,
    calculate_rod_buckling,
)

gear_box = calculate_gear_box_loading(ctx)
power = calculate_power_consumption(ctx)
buckling = calculate_rod_buckling(ctx)
```

## Reproduce the canonical generator comparison

The corrected generator shapes and retrained classifier are retained. The
comparison used to check their geometry is available as
`benchmark.shape_comparison.prepare` and `loop_distance`. The reference is the
[CC0 diagnosis plate, version 1 (2020)](https://doi.org/10.17632/hnfwm9mjs6.1).
Only the load-card block is compared; the companion power trace is excluded.

Each coordinate axis is scaled using `(q - min(q)) / (max(q) - min(q))`.
A constant axis maps to zero. The polygon is closed, cumulative Euclidean
arc length is calculated, and linear interpolation produces equally spaced
samples. The distance is the smallest mean Euclidean separation across all
cyclic sample shifts and both traversal orders. The default sampling resolution
comes from the method in
[the generator-shape issue](https://github.com/vamseeachanta/digitalmodel/issues/1875);
it is a numerical resolution, not a physical coefficient.

### Worked calculation

For a unit square with vertices `(0,0), (1,0), (1,1), (0,1)`, the perimeter is
four. Eight samples therefore advance by half an edge. The expected samples,
calculated directly from the edges, are
`(0,0), (0.5,0), (1,0), (1,0.5), (1,1), (0.5,1), (0,1), (0,0.5)`.
Changing units to `x'=2x+7, y'=3y-4`, reversing traversal, or rotating the
vertex list leaves the normalised square unchanged up to sample order, giving
zero distance. These algebraic results are independent controls in
`test_shape_comparison.py`.

Run the following from the repository root to compare a generated gas card
with the retained plate, using seed zero as a reproducible example input:

```python
import json
from pathlib import Path

import numpy as np

from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.shape_comparison import (
    loop_distance,
    prepare,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    generate_gas_interference_card,
)

fixture = Path("tests/marine_ops/artificial_lift/dynacard/testdata/plate_reference_cards.json")
cards = json.loads(fixture.read_text())["cards"]
references = {
    name: prepare(entry["position"], entry["load"])
    for name, entry in cards.items()
}
generated = generate_gas_interference_card(seed=0)
own_distance = loop_distance(
    references["gas_interference"], prepare(generated.position, generated.load)
)
loops = list(references.values())
separation = [
    loop_distance(a, b)
    for i, a in enumerate(loops)
    for b in loops[i + 1:]
]
print(f"Gas shape distance: {100 * own_distance:.1f}%")
print(f"Inter-class median: {100 * np.median(separation):.1f}%")
```

With `PYTHONPATH=src`, the answer is `Gas shape distance: 4.9%` and
`Inter-class median: 11.7%`, expressed as fractions of unit-axis length.
The unrounded gas distance is below the unrounded inter-class median.
This is a single-seed illustration; the shape regression checks multiple seeds,
nearest-exemplar identity, corner features and rod-part loop geometry.

Per-axis normalisation discards stroke, absolute load and aspect ratio.
Discrete phase alignment has finite sampling error for arbitrary starting
positions between samples. One reference per condition cannot establish
field diagnostic accuracy, and the geometric distance does not establish
the stochastic sand-abrasion signature; the existing texture test covers that
feature. Synthetic cross-validation remains synthetic separability.
No solver is required for this comparison.

## Architecture

The analysis pipeline follows a four-stage workflow orchestrated by `DynacardWorkflow`:

```
Surface Card --> [Physics Solver] --> Downhole Card
                                          |
                                          v
                                   [Calculations]
                                   (fluid load, CPIP, fillage, production)
                                          |
                                          v
                                    [Diagnostics]
                                    (AI-driven troubleshooting)
```

### Module map

| File | Role |
|---|---|
| `models.py` | 30+ Pydantic v2 data classes (input, output, context) |
| `physics.py` | Gibbs frequency-domain wave equation solver |
| `finite_difference.py` | Time-domain finite difference wave equation solver |
| `calculations.py` | CPIP, pump fillage, fluid load, theoretical production |
| `corners.py` | Corner detection for pump card analysis |
| `diagnostics.py` | AI-driven pump diagnostics and troubleshooting |
| `solver.py` | `DynacardWorkflow` orchestrator with router pattern |
| `base.py` | Generic `BaseCalculator[T]` abstract class |
| `constants.py` | Physical constants and empirical calibration values |
| `data_loader.py` | JSON file loading utilities |
| `exceptions.py` | 6-type custom exception hierarchy |

### Calculator catalog

| Calculator | Module | Output Model |
|---|---|---|
| `GearBoxLoadingCalculator` | `gear_box_loading.py` | `GearBoxLoadingAnalysis` |
| `PowerConsumptionCalculator` | `power_consumption.py` | `PowerConsumptionAnalysis` |
| `LiftCapacityCalculator` | `lift_capacity.py` | `LiftCapacityAnalysis` |
| `LoadRatioCalculator` | `load_analysis.py` | `LoadRatioAnalysis` |
| `PumpEfficiencyCalculator` | `pump_efficiency.py` | (efficiency metrics) |
| `CardGeometryCalculator` | `geometry.py` | `CardGeometryAnalysis` |
| `RodBucklingCalculator` | `rod_buckling.py` | `RodBucklingAnalysis` |
| `IdealCardCalculator` | `ideal_card.py` | `IdealCardAnalysis` |
| `TorqueBalanceCalculator` | `torque_balance.py` | `TorqueBalanceAnalysis` |

All calculators extend `BaseCalculator[T]` and follow the same pattern:
1. Accept a `DynacardAnalysisContext`
2. Validate inputs via inherited validation methods
3. Execute domain-specific calculations
4. Return a typed Pydantic result model

## Physics Solvers

### Gibbs (comparison only)

The coupled frequency-domain method propagates displacement and load through
ordered rod sections, using each section's properties and damping rate. See
[the equations, independent tests and worked example](gibbs-transfer.md).
Field diagnostic validity remains NOT_EVALUATED; the workflow default is
`everitt_jennings`.

```python
workflow = DynacardWorkflow(ctx, solver_method="gibbs")
```

### Finite Difference

Time-domain numerical method that solves the rod string PDE directly on a
spatial grid. Provides more detailed results for complex rod configurations
and transient analysis at the cost of additional computation time.

```python
workflow = DynacardWorkflow(ctx, solver_method="finite_difference")
```

## Exception Hierarchy

All exceptions inherit from `DynacardException` and carry structured error codes:

```
DynacardException
  +-- ValidationError      # Input data validation failures
  +-- PhysicsError         # Physics solver failures
  +-- NumericalError       # Math/numerical issues (NaN, division by zero)
  +-- ConfigurationError   # Invalid calculator configuration
  +-- ConvergenceError     # Iterative solver convergence failures
  +-- DataLoadError        # File/data loading failures
```

Factory functions are provided for common validation errors:

```python
from digitalmodel.marine_ops.artificial_lift.dynacard import (
    missing_data_error,
    invalid_value_error,
    array_length_mismatch_error,
)
```

## Key Data Models

| Model | Purpose |
|---|---|
| `DynacardAnalysisContext` | Top-level input: surface card, rod string, pump, surface unit, SPM |
| `CardData` | Position and load arrays representing a dynamometer card |
| `RodSection` | Single rod section: diameter, length, material properties |
| `PumpProperties` | Pump diameter, depth, tubing properties |
| `SurfaceUnit` | Pumping unit geometry (K, I, A, C, P dimensions) |
| `AnalysisResults` | Aggregated output from full workflow |

## References

- API RP 11L: Recommended Practice for Design Calculations for Sucker Rod Pumping Systems
- API 11E: Specification for Pumping Units
- Gibbs, S.G.: "Predicting the Behavior of Sucker-Rod Pumping Systems" (JPT, 1963)
