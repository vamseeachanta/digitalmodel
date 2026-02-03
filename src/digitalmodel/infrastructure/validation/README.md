# Validation Pipeline

Unified validation pipeline for digitalmodel project with composable validators, parallel execution, and interactive HTML reports.

## Features

- Composable validators with severity levels (INFO, WARNING, ERROR, CRITICAL)
- Parallel validation execution using ThreadPoolExecutor
- Fail-fast option on CRITICAL errors
- Result caching with TTL
- Interactive HTML reports with Plotly visualizations
- Export validation results to JSON
- Support for property-based testing with Hypothesis

## Quick Start

```python
from digitalmodel.validation.pipeline import (
    RangeValidator,
    PhysicalPlausibilityValidator,
    ValidationPipeline,
    generate_html_report
)

# Create validators
validators = [
    PhysicalPlausibilityValidator("tension", "force"),
    RangeValidator("displacement", min_value=0, max_value=50),
]

# Create and execute pipeline
pipeline = ValidationPipeline(validators=validators, parallel=True)
results = pipeline.execute(data)

# Generate HTML report
generate_html_report(results, "validation_report.html")
```

## Available Validators

### RangeValidator
Validates that values are within min/max bounds.

```python
RangeValidator("temperature", min_value=0, max_value=100)
```

### MatrixValidator
Validates matrix properties: shape, symmetry, positive-definite.

```python
MatrixValidator(
    "stiffness",
    expected_shape=(6, 6),
    check_symmetric=True,
    check_positive_definite=True
)
```

### PhysicalPlausibilityValidator
Validates physical engineering limits for forces, moments, displacements.

```python
PhysicalPlausibilityValidator("tension", "force")
```

Supported types: `force`, `moment`, `displacement`, `stress`, `strain`, `velocity`, `acceleration`, `pressure`, `angle`, `curvature`

### UnitConsistencyValidator
Validates SI unit consistency.

```python
UnitConsistencyValidator(check_dimensionality=True)
```

### PolarDataValidator
Validates polar data for 360° coverage and gaps.

```python
PolarDataValidator("heading", "rao", max_gap_degrees=30)
```

### TimeSeriesValidator
Validates time series data for gaps and outliers.

```python
TimeSeriesValidator(
    "time",
    "displacement",
    detect_gaps=True,
    detect_outliers=True,
    outlier_std_threshold=3.0
)
```

## ValidationPipeline Options

- `parallel=True`: Execute validators in parallel using ThreadPoolExecutor
- `fail_fast=True`: Stop on first CRITICAL error
- `max_workers=N`: Maximum thread pool workers

## Validation Severity Levels

```python
class ValidationSeverity(IntEnum):
    INFO = 1       # Informational messages
    WARNING = 2    # Issues that may affect accuracy
    ERROR = 3      # Issues that prevent use
    CRITICAL = 4   # Physically impossible values
```

## HTML Report Features

- Interactive Plotly visualizations
- Severity distribution pie chart
- Validator pass/fail status
- Issues by validator
- Validation summary table
- Detailed issue listing
- Export to JSON

## Configuration

Physical limits can be customized via YAML configuration:

```yaml
# config/validation_limits.yml
physical_limits:
  force:
    min: -1.0e8
    max: 1.0e8
    typical_max: 1.0e6
```

## Test Coverage

91.6% code coverage with 63 tests including:
- Unit tests for all validators
- Integration tests with marine engineering data
- Property-based tests using Hypothesis
- HTML report generation tests

## Example Usage

See `examples/validation_pipeline_example.py` for comprehensive examples including:
- Mooring line validation
- RAO polar data validation
- Time series validation
- Stiffness matrix validation
- Comprehensive validation with HTML reports

## Integration with Existing Validators

Use the adapter pattern to wrap existing validators:

```python
class LegacyValidatorAdapter(BaseValidator):
    def __init__(self, legacy_validator):
        super().__init__()
        self.legacy = legacy_validator

    def validate(self, data):
        result = self.legacy.check_data(data)
        # Convert to ValidationResult
        return ValidationResult(...)
```

## Performance

- Parallel execution provides 2.8-4.4x speed improvement
- Caching reduces validation time for repeated data
- ThreadPoolExecutor for concurrent validator execution

## API Reference

### ValidationResult
```python
@dataclass
class ValidationResult:
    passed: bool
    issues: List[ValidationIssue]
    summary: Dict[str, Any]
    validator_name: str
    data_hash: Optional[str]
    timestamp: datetime
```

### ValidationIssue
```python
@dataclass
class ValidationIssue:
    severity: ValidationSeverity
    message: str
    validator_name: str
    field_name: Optional[str]
    value: Optional[Any]
    expected_range: Optional[Tuple[float, float]]
    location: Optional[str]
```

## Files

- `pipeline.py` - Core validation pipeline (1082 lines)
- `__init__.py` - Package exports
- `README.md` - This file
- `config/validation_limits.yml` - Physical limits configuration
- `tests/validation/test_pipeline.py` - Comprehensive test suite (1127 lines)
- `examples/validation_pipeline_example.py` - Usage examples

## Dependencies

- numpy - Array operations
- pandas - Data handling
- pydantic - Data validation
- plotly - Interactive visualizations

## License

Part of digitalmodel project.
