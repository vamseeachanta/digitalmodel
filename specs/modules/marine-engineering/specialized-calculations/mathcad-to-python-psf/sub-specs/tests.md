# Tests Specification

This is the tests coverage details for the spec detailed in @specs/modules/ship_design/passing-ship-forces/spec.md

> Created: 2025-01-01
> Version: 1.0.0

## Test Coverage

### Unit Tests

**WangFormulations**
- Test sectional area curve calculations for standard hull forms
- Verify derivative calculations (dS1, dS2) against analytical solutions
- Test F and G integral kernel functions with known inputs
- Validate infinite depth force formulations
- Verify finite depth correction factors (eta function)
- Test harmonic summation convergence for finite depth
- Validate unit conversions between SI and Imperial systems

**NumericalIntegration**
- Test adaptive quadrature convergence for smooth functions
- Verify handling of singular points in integration domain
- Test nested integration accuracy with analytical test functions
- Validate integration bounds and coordinate transformations
- Test caching mechanism for repeated calculations

**ConfigurationParser**
- Test YAML parsing with valid configurations
- Verify validation of required parameters
- Test expression evaluation (e.g., "0.5 * moored.length")
- Validate unit system detection and conversion
- Test error handling for invalid configurations
- Verify template loading and merging

### Integration Tests

**PassingShipCalculator**
- Test complete calculation pipeline with MathCAD reference values
- Verify force calculations for zero separation (should handle singularity)
- Test extreme cases (very shallow water, very close passing)
- Validate batch processing with multiple scenarios
- Test parallel execution performance
- Verify result caching and retrieval

**Visualization Module**
- Test plot generation for force distributions
- Verify parametric study visualizations
- Test interactive plot features (zoom, pan, export)
- Validate chart annotations and labels
- Test multi-plot layouts for comparison studies

### Feature Tests

**End-to-End MathCAD Validation**
- Replicate exact MathCAD example:
  - Moored vessel: L=950ft, A1=3192ft²
  - Passing vessel: L2=475ft, A2=6413ft²
  - Water density: ρ=1.9905 slug/ft³
  - Velocity: U=11.2 ft/s
  - Separation: 0.2*L=190ft
  - Stagger: 0ft
  - Depth: 0.1*L=95ft
- Expected results (within 0.1% tolerance):
  - Surge Force: ~1.016e-11 lbf
  - Sway Force: ~7.644e4 lbf
  - Yaw Moment: ~7.851e-9 ft-lbf

**Parametric Study Workflow**
- Load configuration template
- Vary separation distance from 0.1L to 1.0L
- Generate force curves matching MathCAD plots
- Export results to CSV and JSON
- Verify plot aesthetics and readability

**CLI Integration**
- Test command-line argument parsing
- Verify batch file processing with patterns
- Test output directory creation and file naming
- Validate error messages and help text
- Test progress reporting for long calculations

### Performance Tests

**Calculation Speed**
- Single scenario: < 100ms
- Batch of 100 scenarios: < 5 seconds
- Batch of 1000 scenarios with parallelization: < 30 seconds

**Memory Usage**
- Peak memory for single calculation: < 50MB
- Memory scaling for batch processing: linear with scenario count
- Cache memory management: automatic cleanup after 1GB

### Validation Data Sources

**Primary References**
- MathCAD calculation sheet (provided PDF)
- Wang's original paper data points
- Published validation cases from marine engineering literature

**Test Data Sets**
- `test_data/mathcad_reference.json` - Exact MathCAD values
- `test_data/wang_paper_validation.csv` - Published validation points
- `test_data/parametric_studies.yml` - Standard test scenarios

### Mocking Requirements

**Note: Following repository policy, NO mock tests will be created. All tests use real calculations.**

- **External File I/O:** Use temporary directories with real files
- **Parallel Processing:** Use actual ProcessPoolExecutor with small workloads
- **Visualization:** Generate real plots in memory or temp files
- **Configuration:** Use actual YAML files in test fixtures

### Test Execution Strategy

```python
# Parallel test execution pattern
uv run pytest tests/modules/ship_design/passing_ship/ -n auto --verbose

# Categories for selective testing
uv run pytest -m "unit" # Fast unit tests
uv run pytest -m "integration" # Integration tests  
uv run pytest -m "validation" # MathCAD validation
uv run pytest -m "performance" # Performance benchmarks
```

### Continuous Validation

- All PRs must pass MathCAD reference validation
- Performance benchmarks tracked in CI/CD
- Visualization outputs reviewed in PR artifacts
- Coverage requirement: minimum 90%