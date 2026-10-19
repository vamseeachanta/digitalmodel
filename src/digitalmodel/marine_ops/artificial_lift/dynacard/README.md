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

## Diagnostic round trip

The default context solver retains two outputs from a single displacement solve:

- `downhole_card`: the historical smoothed card used for engineering calculations.
- `diagnostic_card`: unsmoothed strain load used by `classify_with_context` and
  the diagnostic report plot. Both cards are serialized in `AnalysisResults`.

The classifier is trained on raw generated pump cards. Smoothing its input
removes oscillations and valve-closure detail used by that classifier. The
diagnostic load uses the same backward strain stencil at every cyclic sample:
`F[j] = E A (u[last,j] - u[last-1,j]) / dx`, from stress = modulus times strain
and stress = force / area: [OpenStax, University Physics vol. 1 (2016),
section 12.3, equations 12.34-12.36](https://openstax.org/books/university-physics-volume-1/pages/12-3-stress-strain-and-elastic-modulus).
The discrete basis is the forward boundary
in `card_generators.surface_card_from_pump_card` and the strain reconstruction
in `everitt_jennings.solver._full_load`. Substituting the forward boundary
`u[last-1] = u[last] - F dx/(EA)` gives `EA(F dx/(EA))/dx = F` independently
of the classifier. The final sample duplicates the first to close the cycle.
The historical calculation load retains its existing filtering and endpoint
stencil. Filter behavior is documented in the
[SciPy `savgol_filter` API, Parameters and Notes](https://docs.scipy.org/doc/scipy/reference/generated/scipy.signal.savgol_filter.html).

### Worked synthetic example

The following inputs are taken from
`examples/workflows/artificial-lift-field-health/input.yml`: rod diameter
1.0 in, rod length 5000.0 ft, pump diameter 1.75 in, pump depth 5000.0 ft and
speed 10.0 strokes/min. They define a synthetic test, not a field design basis.
Seed zero is a reproducibility input. Run with the source package on `PYTHONPATH`:

```python
from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    ALL_GENERATORS, surface_card_from_pump_card,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import PumpDiagnostics
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.adapter import (
    EverittJenningsContextSolver,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    DynacardAnalysisContext, PumpProperties, RodSection,
)

retained = 0
for mode, generate in ALL_GENERATORS.items():
    pump = generate(seed=0)
    ctx = DynacardAnalysisContext(
        api14="SYNTHETIC", surface_card=pump,
        rod_string=[RodSection(diameter=1.0, length=5000.0)],
        pump=PumpProperties(diameter=1.75, depth=5000.0), spm=10.0,
    )
    ctx.surface_card = surface_card_from_pump_card(pump, ctx)
    result = EverittJenningsContextSolver(ctx).solve()
    label = PumpDiagnostics().classify_with_context(result).classification
    retained += label == mode
    print(mode, label)
print(f"Labels retained: {retained}/{len(ALL_GENERATORS)}")
```

Answer: all 20 supplied labels are recovered at this geometry and seed,
including `ROD_PARTING`. The field-health mapping assigns that label `failure`,
which precedes a `BENT_BARREL` warning in the worst-wells ordering. The former
smoothed-card classification retains 16 of these 20 labels at the same inputs.
These are synthetic label-retention counts, not field accuracy.

At seeds zero, seven and forty-two, 59 of 60 supplied labels are recovered.
The remaining `DELAYED_TV_CLOSURE` example at seed forty-two becomes
`VALVE_LEAK_TV` even when the raw generated card is merely closed, before any
solver is run. A strict expected-failure test preserves the desired supplied
label; another test verifies that the solver retains the equivalent closed-input
classification. Recalibrating that classifier/generator disagreement remains
unresolved. No alternative diagnosis is qualified by this example.

Classifier field accuracy is **NOT_EVALUATED**. Labelled real-card evidence is
still required under [the field-validation issue](https://github.com/vamseeachanta/digitalmodel/issues/1864).
Unsmoothing also preserves measurement noise; no noise acceptance limit or field
reliability is established. `classify_card(card)` classifies exactly the card
provided; callers that need the diagnostic path should use the context solver
and `classify_with_context`. Older stored results and comparison solvers without
a diagnostic card retain their existing explicit downhole-card path.
