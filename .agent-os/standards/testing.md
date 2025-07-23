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

## Licensed Software Testing

### OrcaFlex Testing Patterns
OrcaFlex tests require a license which may not be available in all environments. Use mock approaches for testing without license dependencies.

#### Mock OrcaFlex API Implementation
```python
import sys

class MockGeneral:
    def __init__(self):
        self.StageEndTime = [3600.0]  # 1 hour simulation
        self.ImplicitUseVariableTimeStep = "No"
        self.ImplicitConstantTimeStep = 0.1
        self.ImplicitVariableMaxTimeStep = 0.1

class MockState:
    def __init__(self):
        self._name_ = "SimulationStopped"

class MockTimeStatus:
    def __init__(self):
        self.CurrentTime = 3600.0

class MockOrcFxAPI:
    class Model:
        def __init__(self, *args, **kwargs):
            self.general = MockGeneral()
            self.simulationComplete = True
            self.state = MockState()
            self.simulationStartTime = "2023-01-01 00:00:00"
            self.simulationStopTime = "2023-01-01 01:00:00"
            self.simulationTimeStatus = MockTimeStatus()
            
        def LoadData(self, *args, **kwargs):
            pass
            
        def RunSimulation(self, *args, **kwargs):
            pass
            
        def SaveSimulation(self, *args, **kwargs):
            pass
            
        def SaveData(self, *args, **kwargs):
            pass

# Use mock API when OrcaFlex not available
sys.modules['OrcFxAPI'] = MockOrcFxAPI()
```

#### OrcaFlex Test Structure
```python
@pytest.fixture
def mock_orcaflex_model():
    """Provide mock OrcaFlex model for testing."""
    return MockOrcFxAPI.Model()

def test_orcaflex_analysis_with_mock(mock_orcaflex_model):
    """Test OrcaFlex analysis using mock API."""
    config = {
        "simulation": {
            "duration": 3600,
            "time_step": 0.1
        }
    }
    
    # Test should work with mock API
    result = analyze_orcaflex_results(mock_orcaflex_model, config)
    
    assert result is not None
    assert "simulation_time" in result
```

### Test File Organization

#### OrcaFlex Test File Structure
```
tests/
├── modules/
│   └── orcaflex/
│       ├── test_orcaflex_analysis.py
│       ├── test_orcaflex_postprocess.py
│       └── fixtures/
│           ├── config_files/
│           │   └── test_analysis.yml
│           ├── simulation_files/
│           │   └── test_model.sim
│           └── expected_results/
│               └── expected_output.csv
```

#### Test Configuration Management
```python
@pytest.fixture
def orcaflex_test_config():
    """Load test configuration for OrcaFlex analysis."""
    config_path = Path(__file__).parent / "fixtures" / "config_files" / "test_analysis.yml"
    with open(config_path) as f:
        return yaml.safe_load(f)

def test_config_validation_with_orcaflex(orcaflex_test_config):
    """Test configuration validation for OrcaFlex analysis."""
    # Test should validate required OrcaFlex-specific fields
    assert "orcaflex" in orcaflex_test_config
    assert "postprocess" in orcaflex_test_config["orcaflex"]
```

### Engineering-Specific Test Patterns

#### Physical Constraint Testing
```python
@pytest.mark.parametrize("stress_values,expected_valid", [
    ([100e6, 200e6, 50e6], True),     # Normal stress values
    ([-100e6, 200e6, 50e6], False),   # Negative stress (invalid)
    ([2e9, 200e6, 50e6], False),      # Unreasonably high stress
])
def test_stress_validation(stress_values, expected_valid):
    """Test stress value validation with physical constraints."""
    if expected_valid:
        result = calculate_von_mises_stress(*stress_values)
        assert result > 0
    else:
        with pytest.raises(ValueError):
            calculate_von_mises_stress(*stress_values)
```

#### Units and Conversion Testing
```python
def test_unit_conversions():
    """Test engineering unit conversions."""
    # Test force conversions
    force_n = 1000.0
    force_kn = convert_force(force_n, "N", "kN")
    assert force_kn == pytest.approx(1.0, rel=1e-6)
    
    # Test pressure conversions
    pressure_pa = 1e6
    pressure_mpa = convert_pressure(pressure_pa, "Pa", "MPa")
    assert pressure_mpa == pytest.approx(1.0, rel=1e-6)
```

#### Numerical Stability Testing
```python
def test_numerical_stability_edge_cases():
    """Test calculation stability with edge cases."""
    # Test with very small values
    small_result = calculate_stress(1e-10, 1e-10)
    assert not np.isnan(small_result)
    assert not np.isinf(small_result)
    
    # Test with zero values
    zero_result = calculate_stress(0.0, 1.0)
    assert zero_result == 0.0
    
    # Test with very large values (within reasonable engineering limits)
    large_result = calculate_stress(1e8, 1e-4)  # 100 MPa, 0.1 mm²
    assert not np.isnan(large_result)
    assert large_result > 0
```

### Mock Integration Testing

#### External Software Availability Testing
```python
def test_software_availability_detection():
    """Test detection of external software availability."""
    # Test OrcaFlex availability
    adapter = OrcaFlexAdapter()
    
    # Should work in both mock and real modes
    assert hasattr(adapter, 'api')
    
    # Test graceful fallback to mock mode
    if adapter.mock_mode:
        assert isinstance(adapter.api, MockOrcFxAPI)
    else:
        # Real OrcaFlex API available
        assert hasattr(adapter.api, 'Model')
```

#### Configuration-Driven Testing
```python
@pytest.fixture
def test_configurations():
    """Provide various test configurations."""
    return {
        "minimal": {
            "analysis": {"type": "basic"},
            "inputs": {"file": "test.sim"}
        },
        "complete": {
            "analysis": {"type": "full", "parallel": True},
            "inputs": {"files": ["test1.sim", "test2.sim"]},
            "outputs": {"format": "csv", "plots": True}
        }
    }

@pytest.mark.parametrize("config_name", ["minimal", "complete"])
def test_analysis_with_different_configs(test_configurations, config_name):
    """Test analysis with different configuration types."""
    config = test_configurations[config_name]
    
    # Analysis should handle different configuration types
    result = run_analysis(config)
    assert result is not None
```

## Best Practices

1. **Test Independence**: Tests should not depend on each other
2. **Clear Assertions**: One logical assertion per test
3. **Descriptive Names**: Test names should explain what they test
4. **Fast Execution**: Keep individual tests under 1 second
5. **Deterministic**: Tests should produce same results every run
6. **Documentation**: Complex tests need explanatory comments
7. **Mock External Dependencies**: Use mock APIs for licensed software
8. **Physical Validation**: Test engineering constraints and units
9. **Configuration Testing**: Validate YAML configs and schemas
10. **Error Boundary Testing**: Test error handling and recovery