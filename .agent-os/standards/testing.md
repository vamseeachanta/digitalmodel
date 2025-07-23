# Testing Standards

## Testing Philosophy

All code should be tested to ensure reliability and maintainability. Tests serve as both validation and documentation of expected behavior.

## Test Organization

### Directory Structure
```
tests/
├── unit/              # Unit tests for individual functions
├── integration/       # Integration tests for module interactions
├── fixtures/          # Test data and fixtures
├── conftest.py        # Pytest configuration and fixtures
└── test_*.py          # Test files matching tested modules
```

### Naming Conventions
- Test files: `test_<module_name>.py`
- Test functions: `test_<function_name>_<scenario>`
- Test classes: `Test<ClassName>`

## Types of Tests

### Unit Tests
Test individual functions in isolation:
```python
def test_calculate_stress_valid_input():
    """Test stress calculation with valid input."""
    force = 1000.0  # N
    area = 0.1  # m²
    expected = 10000.0  # Pa
    
    result = calculate_stress(force, area)
    
    assert result == pytest.approx(expected, rel=1e-6)
```

### Integration Tests
Test module interactions:
```python
def test_orcaflex_post_process_integration():
    """Test complete OrcaFlex post-processing workflow."""
    config = load_test_config("orcaflex_test.yml")
    
    processor = OrcaFlexPostProcess()
    results = processor.post_process_router(config)
    
    assert "summary" in results
    assert len(results["summary"]) > 0
```

### Parametrized Tests
Test multiple scenarios efficiently:
```python
@pytest.mark.parametrize("diameter,thickness,expected_area", [
    (0.1, 0.01, 0.00314),
    (0.2, 0.02, 0.01257),
    (0.3, 0.03, 0.02827),
])
def test_pipe_area_calculation(diameter, thickness, expected_area):
    """Test pipe area calculation for various sizes."""
    result = calculate_pipe_area(diameter, thickness)
    assert result == pytest.approx(expected_area, rel=0.01)
```

## Test Data Management

### Fixtures
```python
@pytest.fixture
def sample_simulation_data():
    """Provide sample simulation data for testing."""
    return {
        "time": np.linspace(0, 100, 1000),
        "force": np.random.normal(1000, 100, 1000),
        "displacement": np.random.normal(0.1, 0.01, 1000)
    }
```

### Test Data Files
- Store in `tests/fixtures/`
- Use minimal, representative data
- Document data format and source
- Version control test data

## Engineering-Specific Testing

### Numerical Testing
```python
def test_numerical_stability():
    """Test calculation stability with edge cases."""
    # Test with very small values
    small_result = calculate_stress(1e-10, 1e-10)
    assert not np.isnan(small_result)
    assert not np.isinf(small_result)
    
    # Test with very large values
    large_result = calculate_stress(1e10, 1e-6)
    assert not np.isnan(large_result)
```

### Unit Validation
```python
def test_force_units_conversion():
    """Test force unit conversions."""
    force_n = 1000.0
    force_kn = convert_force(force_n, "N", "kN")
    assert force_kn == 1.0
```

### Physical Constraints
```python
def test_stress_physical_constraints():
    """Test that stress calculations respect physical laws."""
    stress = calculate_von_mises_stress(100, 50, 30)
    assert stress >= 0  # Stress cannot be negative
```

## Test Coverage

### Coverage Goals
- Minimum 80% line coverage
- 100% coverage for critical calculations
- Focus on branch coverage for complex logic

### Running Coverage
```bash
pytest --cov=digitalmodel --cov-report=html
```

## Continuous Integration

### Pre-commit Checks
- Run unit tests
- Check test coverage
- Validate test naming

### CI Pipeline Tests
1. Unit tests (fast)
2. Integration tests (slower)
3. Performance benchmarks (optional)

## Performance Testing

```python
@pytest.mark.benchmark
def test_large_dataset_performance(benchmark):
    """Benchmark processing of large datasets."""
    large_data = generate_large_dataset(10000)
    
    result = benchmark(process_data, large_data)
    
    assert result is not None
    # Benchmark automatically measures performance
```

## Mock Usage

```python
def test_external_api_call(mocker):
    """Test function that calls external API."""
    mock_response = {"status": "success", "data": [1, 2, 3]}
    mocker.patch("requests.get", return_value=mock_response)
    
    result = fetch_external_data()
    
    assert result == mock_response["data"]
```

## Best Practices

1. **Test Independence**: Tests should not depend on each other
2. **Clear Assertions**: One logical assertion per test
3. **Descriptive Names**: Test names should explain what they test
4. **Fast Execution**: Keep individual tests under 1 second
5. **Deterministic**: Tests should produce same results every run
6. **Documentation**: Complex tests need explanatory comments