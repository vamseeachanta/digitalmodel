# DigitalModel Advanced Testing Guide

## Overview

This repository implements a comprehensive testing framework using cutting-edge testing practices from companies like Google, Meta, Netflix, and other industry leaders. The testing suite includes:

- **Performance Testing**: Benchmarks, load testing, memory profiling
- **Property-Based Testing**: Hypothesis-driven testing for mathematical properties
- **Mutation Testing**: Code quality verification through mutmut
- **Contract Testing**: API schema validation and consumer-driven contracts
- **Security Testing**: Injection attacks, authentication, authorization
- **Test Data Management**: Factory Boy for realistic test data generation
- **Advanced CI/CD**: Matrix testing across Python versions and operating systems

## Testing Structure

```
tests/
├── conftest.py                 # Global test configuration and fixtures
├── performance/                # Performance and load testing
│   ├── conftest.py            # Performance testing fixtures
│   ├── test_benchmarks.py     # Performance benchmarks
│   └── test_load_testing.py   # Load and stress testing
├── property/                   # Property-based testing
│   ├── test_mathematical_properties.py
│   └── test_state_machine_properties.py
├── contracts/                  # Contract testing
│   └── test_api_contracts.py  # API schema validation
├── security/                   # Security testing
│   └── test_injection_attacks.py
├── factories/                  # Test data management
│   ├── conftest.py            # Factory fixtures
│   └── simulation_factories.py
└── data/                       # Test data files (when needed)
```

## Running Tests

### Basic Test Execution

```bash
# Run all tests
pytest

# Run with coverage
pytest --cov=src --cov-report=html

# Run specific test categories
pytest -m "not slow"           # Skip slow tests
pytest -m "unit"               # Unit tests only
pytest -m "integration"        # Integration tests only
pytest -m "performance"        # Performance tests
pytest -m "security"           # Security tests
```

### Advanced Test Execution

```bash
# Parallel execution
pytest -n auto                # Use all CPU cores
pytest -n 4                   # Use 4 workers

# Property-based testing with extended examples
pytest tests/property/ --hypothesis-profile=ci

# Performance benchmarking
pytest tests/performance/test_benchmarks.py --benchmark-json=results.json

# Load testing
pytest tests/performance/test_load_testing.py -m "load_test"

# Stress testing
pytest tests/performance/test_load_testing.py -m "stress_test"
```

### Mutation Testing

```bash
# Quick mutation testing (development)
mutmut run --paths-to-mutate=src/digitalmodel/common/ --profile=quick

# Comprehensive mutation testing (CI)
mutmut run --profile=comprehensive

# View mutation test results
mutmut html
open mutation_report.html
```

## Test Categories and Markers

### Pytest Markers

| Marker | Description | Usage |
|--------|-------------|-------|
| `unit` | Unit tests | `-m "unit"` |
| `integration` | Integration tests | `-m "integration"` |
| `performance` | Performance tests | `-m "performance"` |
| `property` | Property-based tests | `-m "property"` |
| `security` | Security tests | `-m "security"` |
| `contract` | Contract tests | `-m "contract"` |
| `load_test` | Load testing | `-m "load_test"` |
| `stress_test` | Stress testing | `-m "stress_test"` |
| `chaos_test` | Chaos engineering | `-m "chaos_test"` |
| `slow` | Slow tests | `-m "not slow"` to skip |
| `flaky` | Potentially flaky tests | `-m "not flaky"` to skip |
| `external` | Requires external services | `-m "not external"` to skip |

### Test Execution Profiles

```bash
# Fast development testing
pytest -m "unit and not slow" --maxfail=5

# Comprehensive testing (CI)
pytest --cov=src --cov-fail-under=80 --tb=short

# Pre-commit testing
pytest -m "unit or (integration and not external)" --maxfail=3

# Full validation (nightly)
pytest --cov=src tests/ && mutmut run --profile=comprehensive
```

## Performance Testing

### Benchmark Testing

Performance benchmarks establish baselines and detect regressions:

```python
def test_calculation_performance(benchmark_runner):
    """Test that calculations meet performance requirements."""

    def complex_calculation():
        return sum(i ** 2 for i in range(1000))

    stats = benchmark_runner.run_benchmark(complex_calculation, iterations=100)

    # Assert performance requirements
    assert stats['mean'] < 0.01  # 10ms max
    assert stats['p95'] < 0.02   # 20ms 95th percentile
```

### Load Testing

Load tests verify system behavior under realistic load:

```python
def test_concurrent_processing(load_test_runner):
    """Test system under concurrent load."""

    results = load_test_runner.run_load_test(
        target_function=process_data,
        num_workers=8,
        duration_seconds=30,
        requests_per_second=50
    )

    assert results['error_rate'] < 0.01  # <1% error rate
    assert results['throughput_rps'] > 40  # >40 RPS throughput
```

## Property-Based Testing

Property-based testing uses Hypothesis to generate test cases and verify mathematical properties:

```python
from hypothesis import given, strategies as st

@given(x=st.floats(min_value=-1000, max_value=1000),
       y=st.floats(min_value=-1000, max_value=1000))
def test_addition_commutative(x, y):
    """Test that addition is commutative."""
    assume(not (math.isnan(x) or math.isnan(y)))
    assert x + y == y + x
```

### Custom Strategies

Domain-specific test data generation:

```python
@st.composite
def engineering_float(draw, min_value=-1e6, max_value=1e6):
    """Generate engineering-realistic floating point numbers."""
    return draw(st.floats(min_value=min_value, max_value=max_value, width=64))

@given(pressure=pressure_values(), area=positive_engineering_float())
def test_force_calculation_properties(pressure, area):
    """Test force = pressure * area properties."""
    force = pressure * area
    assert force >= 0  # Force should be non-negative
```

## Contract Testing

Contract testing ensures API compatibility and schema validation:

```python
def test_simulation_request_schema(validator):
    """Test simulation request schema validation."""

    valid_request = {
        "simulation_type": "dynamic",
        "parameters": {"time_step": 0.1, "duration": 10.0},
        "configuration": {"output_format": "csv"}
    }

    is_valid, errors = validator.validate_request("simulation_request", valid_request)
    assert is_valid, f"Valid request failed validation: {errors}"
```

## Security Testing

Security tests verify protection against common attacks:

```python
def test_sql_injection_prevention(mock_db):
    """Test SQL injection attack prevention."""

    # SQL injection attempt
    malicious_input = "admin' OR '1'='1"

    # Should not return data
    user = mock_db.secure_user_lookup(malicious_input)
    assert user is None

    # Should handle gracefully without errors
    with pytest.raises(Exception, match="Invalid input"):
        mock_db.vulnerable_user_lookup(malicious_input)
```

## Test Data Management

### Factory Boy Factories

Realistic test data generation using Factory Boy:

```python
from tests.factories.simulation_factories import create_test_simulation

def test_with_realistic_data():
    """Test using factory-generated data."""

    # Generate realistic simulation
    simulation = create_test_simulation(
        status=SimulationStatus.COMPLETED,
        duration=100.0
    )

    assert simulation.output_data is not None
    assert len(simulation.output_data) > 0
```

### Test Data Fixtures

Reusable test data across modules:

```python
def test_analysis_pipeline(realistic_project, temp_data_directory):
    """Test complete analysis pipeline."""

    # realistic_project fixture provides full project structure
    simulations = realistic_project["simulations"]
    materials = realistic_project["materials"]

    # Process realistic data
    results = analyze_project(simulations, materials)
    assert len(results) > 0
```

## CI/CD Integration

### GitHub Actions Workflow

The `.github/workflows/advanced_testing.yml` provides:

- **Matrix Testing**: Python 3.9-3.12 × Ubuntu/Windows/macOS
- **Parallel Execution**: Tests run with `pytest-xdist`
- **Performance Tracking**: Benchmark results stored and compared
- **Security Scanning**: Bandit and Safety checks
- **Mutation Testing**: Comprehensive code quality verification
- **Coverage Reporting**: Detailed coverage analysis

### Local CI Simulation

```bash
# Simulate CI environment locally
export HYPOTHESIS_PROFILE=ci
pytest --cov=src --cov-fail-under=80 --tb=short -n auto

# Run security checks
bandit -r src/ -f json
safety check

# Performance validation
pytest tests/performance/ --benchmark-json=benchmark.json
```

## Writing Effective Tests

### Test Design Principles

1. **Arrange, Act, Assert**: Structure tests clearly
2. **Test One Thing**: Each test should verify one behavior
3. **Descriptive Names**: Test names should explain what they verify
4. **Independent Tests**: Tests should not depend on each other
5. **Realistic Data**: Use factory-generated data, not hardcoded values

### Performance Test Guidelines

```python
def test_algorithm_performance(performance_monitor):
    """Test algorithm performance meets requirements."""

    performance_monitor.start()

    # Execute operation
    result = expensive_algorithm(large_dataset)

    metrics = performance_monitor.stop()

    # Validate results
    assert result is not None

    # Validate performance
    performance_monitor.assert_performance(
        max_time=1.0,          # 1 second max
        max_memory_mb=100,     # 100MB max
        max_cpu_percent=80     # 80% CPU max
    )
```

### Property Test Guidelines

```python
@given(data=st.lists(st.floats(min_value=0, max_value=1000), min_size=1))
@settings(max_examples=200)
def test_statistical_properties(data):
    """Test statistical calculation properties."""

    # Filter out invalid data
    assume(all(not math.isnan(x) for x in data))
    assume(len(data) > 0)

    mean_val = calculate_mean(data)

    # Properties that should always hold
    assert min(data) <= mean_val <= max(data)
    assert not math.isnan(mean_val)
    assert not math.isinf(mean_val)
```

### Security Test Guidelines

```python
def test_input_validation_comprehensive():
    """Test comprehensive input validation."""

    malicious_inputs = [
        "<script>alert('xss')</script>",     # XSS
        "'; DROP TABLE users; --",          # SQL injection
        "../../../etc/passwd",              # Path traversal
        "\x00admin",                        # Null byte injection
        "A" * 10000,                        # Buffer overflow attempt
    ]

    for malicious_input in malicious_inputs:
        # Should either reject or sanitize
        result = secure_input_handler(malicious_input)
        assert_input_safe(result, malicious_input)
```

## Troubleshooting

### Common Issues

1. **Import Errors**: Ensure `PYTHONPATH` includes `src/`
2. **Slow Tests**: Use `-m "not slow"` to skip during development
3. **Flaky Tests**: Mark with `@pytest.mark.flaky` and investigate
4. **Memory Issues**: Use `--forked` for isolation
5. **Coverage Issues**: Check `.coveragerc` configuration

### Debug Mode

```bash
# Verbose output
pytest -v --tb=long

# Stop on first failure
pytest -x

# Debug specific test
pytest --pdb tests/specific_test.py::test_function

# Hypothesis debugging
pytest tests/property/ --hypothesis-verbosity=verbose
```

### Performance Debugging

```bash
# Profile test execution
pytest --profile

# Memory profiling
pytest --memprof

# Benchmark comparison
pytest --benchmark-compare

# Generate flame graphs
pytest --benchmark-save=baseline
py-spy record -o profile.svg -- pytest
```

## Best Practices Summary

### Testing Strategy

1. **Pyramid Structure**: Many unit tests, fewer integration tests, minimal E2E
2. **Property-Based Testing**: Use for mathematical functions and algorithms
3. **Performance Monitoring**: Establish baselines and track regressions
4. **Security-First**: Test for common vulnerabilities from the start
5. **Realistic Data**: Use factories for domain-appropriate test data

### Code Quality

1. **Mutation Testing**: Verify test quality with mutmut
2. **Coverage Goals**: Aim for 80%+ line coverage, 70%+ branch coverage
3. **Performance Budgets**: Set and enforce performance limits
4. **Security Scanning**: Regular vulnerability assessments
5. **Contract Testing**: Ensure API compatibility

### CI/CD Integration

1. **Fast Feedback**: Quick tests in PR checks
2. **Comprehensive Validation**: Full test suite on main branch
3. **Performance Tracking**: Store and compare benchmark results
4. **Security Gates**: Block deployment on security issues
5. **Quality Gates**: Enforce coverage and mutation score thresholds

This testing framework provides comprehensive validation while maintaining fast development cycles and high code quality standards.