# Artificial Lift Module - Dynacard Analysis

## Overview
The `artificial_lift` module provides specialized engineering analysis for Sucker Rod Pumps (SRP). It translates surface dynamometer cards into downhole pump cards using high-fidelity physics and provides AI-driven diagnostic insights for well troubleshooting.

## Features
- **Wave Equation Solver:** Implements the Gibbs Analytical solution in the frequency domain for stable surface-to-downhole conversion.
- **AI Diagnostics:** Heuristic pattern recognition to identify failure modes:
  - Gas Interference
  - Fluid Pound
  - Pump Tagging (Top/Bottom)
  - Normal Operation
- **Mechanical Integrity:** Automated detection of sucker rod buckling under compressive loads.
- **Interactive Reporting:** Generates Plotly-based HTML dashboards for multi-well field monitoring.

## Directory Structure
- `dynacard/models.py`: Pydantic domain models for well metadata and card data.
- `dynacard/physics.py`: Core numerical and analytical solvers.
- `dynacard/diagnostics.py`: AI troubleshooting and classification logic.
- `dynacard/solver.py`: Main workflow orchestrator and engine router.

## Future Reuse
### Running Analysis via Code
```python
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow
from digitalmodel.marine_ops.artificial_lift.dynacard.models import DynacardAnalysisContext

# Initialize context with well data and surface card
ctx = DynacardAnalysisContext(...)
workflow = DynacardWorkflow(ctx)
results = workflow.run_full_analysis()

print(results.diagnostic_message)
```

### Running via Engine (YAML)
Standard `digitalmodel` configuration supports `basename: artificial_lift` to trigger this workflow.

## Tests
- `tests/modules/artificial_lift/test_dynacard.py`: Core logic unit tests.
- `tests/modules/artificial_lift/test_dynacard_cleansed.py`: Robustness tests using an anonymized production dataset.
- `tests/modules/artificial_lift/test_solver_parity.py`: Mathematical parity verification against legacy implementations.