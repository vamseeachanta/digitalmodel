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

### Gibbs (default)

Frequency-domain analytical method based on Gibbs 1963. Converts surface card
data to downhole conditions using FFT/IFFT decomposition of the wave equation
with empirical calibration factors. Faster and suitable for most production
monitoring use cases.

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
