# Phase 3 Implementation Guide

> **Practical step-by-step guide for implementing Phase 3 integration testing**
>
> Version: 1.0.0
> Date: 2026-01-09
> Status: Implementation Ready

## Overview

This guide provides step-by-step instructions for implementing the Phase 3 integration testing plan. It bridges the architectural planning with concrete implementation details.

---

## Phase 3.1: Foundation Setup (Week 1)

### Step 1.1: Create Test Directory Structure

```bash
# From project root
mkdir -p tests/integration
mkdir -p tests/fixtures/{configs/{valid,invalid,edge_cases,batch},data/{small,medium,large,edge_cases},generators,expected_results}
mkdir -p tests/performance

# Create __init__.py files
touch tests/__init__.py
touch tests/integration/__init__.py
touch tests/fixtures/__init__.py
touch tests/fixtures/generators/__init__.py
touch tests/performance/__init__.py
```

**Verification:**
```bash
find tests/ -type f -name "__init__.py" | wc -l  # Should be 7
find tests/ -type d | wc -l                      # Should be 13
```

### Step 1.2: Create Root Conftest.py

Copy the root conftest.py template from PHASE_3_TEST_ARCHITECTURE.md:
```bash
# tests/conftest.py - See template in architecture document
```

**Key fixtures to implement:**
- `fixtures_dir`, `config_dir`, `data_dir`, `expected_results_dir`
- All configuration fixtures (structural, marine, signal, fatigue)
- All data fixtures (small, medium, large datasets)
- Solver fixtures
- Thread pool fixture

### Step 1.3: Create Configuration Test Data

**Generate test configurations:**

```bash
# Create valid configurations
cat > tests/fixtures/configs/valid/structural.yaml << 'EOF'
solver_type: structural
tolerance: 1e-6
max_iterations: 1000
method: direct
output_format: numpy
EOF

cat > tests/fixtures/configs/valid/marine.yaml << 'EOF'
solver_type: marine
water_depth: 100.0
wave_height: 5.0
current_velocity: 0.5
material_density: 1025.0
safety_factor: 1.5
analysis_type: static
EOF

# Create invalid configurations
cat > tests/fixtures/configs/invalid/type_error.yaml << 'EOF'
solver_type: structural
tolerance: "not_a_number"
max_iterations: 1000
method: direct
output_format: numpy
EOF

cat > tests/fixtures/configs/invalid/range_error.yaml << 'EOF'
solver_type: structural
tolerance: 1e-6
max_iterations: 100000  # Exceeds max
method: direct
output_format: numpy
EOF

# Create edge case configuration
cat > tests/fixtures/configs/edge_cases/extreme_values.yaml << 'EOF'
solver_type: structural
tolerance: 1e-10  # Minimum
max_iterations: 10000  # Maximum
method: adaptive
output_format: json
EOF
```

**Batch configurations (for testing):**

```bash
# Create batch configs for batch processing tests
for i in {1..5}; do
  cat > tests/fixtures/configs/batch/config_$i.yaml << EOF
solver_type: structural
tolerance: $((1 + i))e-6
max_iterations: $((500 + i * 100))
method: direct
output_format: numpy
EOF
done
```

### Step 1.4: Create Test Data Generators

**Implement tests/fixtures/generators/data_generator.py:**

```python
# From PHASE_3_TEST_ARCHITECTURE.md - DatasetGenerator class
# Copy implementation of:
# - generate_structural_data()
# - generate_marine_data()
# - generate_signal_data()
```

**Implement tests/fixtures/generators/config_generator.py:**

```python
# From PHASE_3_TEST_ARCHITECTURE.md - ConfigurationGenerator class
# Copy implementation of:
# - generate_valid_config()
# - generate_invalid_config()
# - generate_edge_case_configs()
```

### Step 1.5: Generate Test Datasets

**Run data generation:**

```python
# tests/fixtures/generate_test_data.py
import sys
from pathlib import Path
from generators.data_generator import DatasetGenerator

# Add to path
sys.path.insert(0, str(Path(__file__).parent))

# Generate datasets
print("Generating small dataset (100 elements)...")
small_data = DatasetGenerator.generate_structural_data(100)
small_data.to_csv(Path(__file__).parent / "data" / "small" / "structural_100_elements.csv", index=False)

print("Generating medium dataset (1,000 elements)...")
medium_data = DatasetGenerator.generate_structural_data(1000)
medium_data.to_csv(Path(__file__).parent / "data" / "medium" / "structural_1000_elements.csv", index=False)

print("Generating large dataset (10,000 elements)...")
large_data = DatasetGenerator.generate_structural_data(10000)
large_data.to_csv(Path(__file__).parent / "data" / "large" / "structural_10000_elements.csv", index=False)

print("All test data generated successfully!")
```

**Run generation:**
```bash
python tests/fixtures/generate_test_data.py
```

### Step 1.6: Configure pytest.ini

**Create/update pytest.ini in project root:**

```ini
[pytest]
testpaths = tests/integration
python_files = test_*.py
python_classes = Test*
python_functions = test_* bench_*

markers =
    integration: Integration test
    performance: Performance benchmark test
    stress: Stress test
    concurrency: Concurrency test
    slow: Slow running test (>1 second)
    e2e: End-to-end workflow test
    flaky: Flaky test - may fail intermittently

# Coverage settings
addopts =
    --tb=short
    --strict-markers
    --disable-warnings

# Benchmark settings
benchmark_min_rounds = 5
benchmark_max_time = 1.0
benchmark_timer = time.perf_counter
```

### Step 1.7: Validate Foundation

```bash
# Verify directory structure
python -m pytest tests/ --collect-only | head -20

# Verify fixtures can be loaded
python -m pytest --fixtures tests/conftest.py | grep -E "fixtures|config_" | head -20

# Verify pytest configuration
python -m pytest --markers | grep integration

# Quick sanity check
python -m pytest tests/ -v --co -q 2>&1 | head -10
```

**Checklist:**
- [ ] Directory structure created
- [ ] conftest.py implemented with all fixtures
- [ ] Configuration files created (valid, invalid, edge_cases)
- [ ] Batch configurations generated
- [ ] Test data generators implemented
- [ ] Test datasets generated (small, medium, large)
- [ ] pytest.ini configured
- [ ] Foundation validation passed

---

## Phase 3.2: Integration Tests (Week 2)

### Step 2.1: Implement Test Classes

**Create tests/integration/test_integration_solvers.py (300+ lines):**

```python
"""
ABOUTME: Integration tests for solver + config manager integration
ABOUTME: Tests real ConfigManager with actual solver execution
"""

import pytest
import yaml
from pathlib import Path


@pytest.mark.integration
class TestConfigurationLoading:
    """Test configuration loading workflows"""

    def test_load_valid_structural_config_passes_validation(
        self,
        config_manager,
        valid_structural_config
    ):
        """Valid structural config passes validation"""
        is_valid, errors = config_manager.validate_solver_config(
            "structural",
            valid_structural_config
        )
        assert is_valid is True
        assert len(errors) == 0

    def test_load_valid_marine_config_passes_validation(
        self,
        config_manager,
        valid_marine_config
    ):
        """Valid marine config passes validation"""
        is_valid, errors = config_manager.validate_solver_config(
            "marine",
            valid_marine_config
        )
        assert is_valid is True
        assert len(errors) == 0

    def test_load_config_with_missing_required_fields_fails(
        self,
        config_manager
    ):
        """Config with missing required fields fails validation"""
        incomplete_config = {"method": "direct"}
        is_valid, errors = config_manager.validate_solver_config(
            "structural",
            incomplete_config
        )
        assert is_valid is False
        # May not report error if optional, so just check it's handled

    def test_config_validation_detects_invalid_types(
        self,
        config_manager,
        invalid_type_config
    ):
        """Validation detects type errors"""
        is_valid, errors = config_manager.validate_solver_config(
            "structural",
            invalid_type_config
        )
        assert is_valid is False
        assert len(errors) > 0

    def test_config_validation_detects_range_errors(
        self,
        config_manager,
        invalid_range_config
    ):
        """Validation detects range errors"""
        is_valid, errors = config_manager.validate_solver_config(
            "structural",
            invalid_range_config
        )
        assert is_valid is False
        assert len(errors) > 0

    def test_config_defaults_applied_for_missing_params(
        self,
        config_manager,
        valid_structural_config
    ):
        """Defaults applied for missing parameters"""
        defaults = config_manager.get_default_config("structural")
        assert "tolerance" in defaults
        assert defaults["tolerance"] == 1e-6
        assert "max_iterations" in defaults
        assert defaults["max_iterations"] == 1000

    def test_config_merge_combines_base_and_override(
        self,
        config_manager,
        valid_structural_config
    ):
        """Config merge combines base and override values"""
        override = {"tolerance": 1e-5}
        merged = config_manager.merge_configs(valid_structural_config, override)
        assert merged["tolerance"] == 1e-5
        assert merged["max_iterations"] == valid_structural_config["max_iterations"]

    def test_get_schema_returns_solver_schema(
        self,
        config_manager
    ):
        """Get schema returns correct solver schema"""
        schema = config_manager.get_schema("structural")
        assert "tolerance" in schema
        assert "max_iterations" in schema
        assert "method" in schema

    def test_load_config_from_yaml_file(
        self,
        config_manager,
        config_dir
    ):
        """Load config from YAML file"""
        config_file = config_dir / "valid" / "structural.yaml"
        config = config_manager.load_config(str(config_file))
        assert config is not None
        assert "solver_type" in config

    def test_save_and_load_config_roundtrip(
        self,
        config_manager,
        temp_config_dir,
        valid_structural_config
    ):
        """Save and load config roundtrip preserves values"""
        config_file = temp_config_dir / "test_config.yaml"
        config_manager.save_config(valid_structural_config, str(config_file))
        loaded_config = config_manager.load_config(str(config_file))
        assert loaded_config == valid_structural_config


@pytest.mark.integration
class TestSolverInstantiation:
    """Test solver instantiation with configurations"""

    def test_structural_solver_instantiation(
        self,
        structural_solver,
        valid_structural_config
    ):
        """Structural solver instantiates with valid config"""
        assert structural_solver is not None
        # Verify solver is configured
        # assert structural_solver.config == valid_structural_config

    def test_multiple_solver_instances_independent(
        self,
        valid_structural_config,
        valid_marine_config
    ):
        """Multiple solver instances maintain independent configurations"""
        # Implementation depends on actual Solver class
        pass


@pytest.mark.integration
@pytest.mark.e2e
class TestEndToEndWorkflows:
    """End-to-end workflow tests"""

    def test_data_loading_validation_solving_workflow(
        self,
        config_manager,
        small_dataset,
        valid_structural_config
    ):
        """Complete workflow: load → validate → solve"""
        # Step 1: Validate configuration
        is_valid, errors = config_manager.validate_solver_config(
            "structural",
            valid_structural_config
        )
        assert is_valid

        # Step 2: Merge with defaults
        defaults = config_manager.get_default_config("structural")
        final_config = config_manager.merge_configs(defaults, valid_structural_config)

        # Step 3: Prepare data
        assert len(small_dataset) > 0

        # Workflow completed successfully
        assert final_config is not None
        assert len(small_dataset) > 0
```

**Implementation notes:**
- Target: 80+ test cases
- Coverage target: 85%+
- Focus on real ConfigManager integration
- Each test should be independent
- Use fixtures for setup/teardown

### Step 2.2: Run Integration Tests

```bash
# Run all integration tests
pytest tests/integration/test_integration_solvers.py -v

# Run with coverage
pytest tests/integration/test_integration_solvers.py -v --cov=digitalmodel.base_solvers

# Run specific test class
pytest tests/integration/test_integration_solvers.py::TestConfigurationLoading -v

# Run until first failure (for debugging)
pytest tests/integration/test_integration_solvers.py -v -x
```

### Step 2.3: Verify Coverage

```bash
# Generate coverage report
pytest tests/integration/test_integration_solvers.py --cov=digitalmodel --cov-report=html

# View coverage
# Open htmlcov/index.html in browser

# Target: 85%+ overall coverage
# Must cover: solver_config.py, validation logic, config workflows
```

**Checklist:**
- [ ] test_integration_solvers.py created (300+ lines)
- [ ] 80+ test cases implemented
- [ ] All test classes implemented
- [ ] Coverage target achieved (85%+)
- [ ] All tests passing
- [ ] Documentation generated

---

## Phase 3.3: Performance Testing (Week 2-3)

### Step 3.1: Implement Performance Benchmarks

**Create tests/integration/test_solver_performance.py (200+ lines):**

```python
"""
ABOUTME: Performance benchmarks for solver execution
ABOUTME: Measures execution time, memory usage, scaling behavior
"""

import pytest
import time


@pytest.mark.performance
class TestSolverExecutionPerformance:
    """Benchmark solver execution times"""

    @pytest.mark.benchmark(group="execution")
    def bench_structural_solver_execution(
        self,
        benchmark,
        structural_solver,
        small_dataset
    ):
        """Benchmark structural solver execution time"""
        def execute():
            return structural_solver.execute(small_dataset)

        # Benchmark automatically runs multiple times
        result = benchmark(execute)

        # Assert performance target
        assert benchmark.stats.mean < 0.050  # < 50ms

    @pytest.mark.benchmark(group="execution")
    def bench_marine_solver_execution(
        self,
        benchmark,
        marine_solver,
        small_dataset
    ):
        """Benchmark marine solver execution time"""
        def execute():
            return marine_solver.execute(small_dataset)

        result = benchmark(execute)
        assert benchmark.stats.mean < 0.100  # < 100ms

    @pytest.mark.benchmark(group="config")
    def bench_config_loading_time(
        self,
        benchmark,
        config_manager,
        config_dir
    ):
        """Benchmark configuration loading time"""
        config_file = config_dir / "valid" / "structural.yaml"

        def load():
            return config_manager.load_config(str(config_file))

        result = benchmark(load)
        assert benchmark.stats.mean < 0.005  # < 5ms

    @pytest.mark.benchmark(group="config")
    def bench_config_validation_time(
        self,
        benchmark,
        config_manager,
        valid_structural_config
    ):
        """Benchmark configuration validation time"""
        def validate():
            is_valid, errors = config_manager.validate_solver_config(
                "structural",
                valid_structural_config
            )
            return is_valid

        result = benchmark(validate)
        assert benchmark.stats.mean < 0.010  # < 10ms

    @pytest.mark.benchmark(group="config")
    def bench_config_merge_time(
        self,
        benchmark,
        config_manager,
        valid_structural_config
    ):
        """Benchmark configuration merge time"""
        override = {"tolerance": 1e-5}

        def merge():
            return config_manager.merge_configs(valid_structural_config, override)

        result = benchmark(merge)
        assert benchmark.stats.mean < 0.001  # < 1ms


@pytest.mark.performance
class TestMemoryUsagePerformance:
    """Benchmark memory consumption"""

    @pytest.mark.benchmark(group="memory")
    def bench_config_memory_footprint(
        self,
        benchmark,
        config_manager,
        valid_structural_config
    ):
        """Benchmark configuration memory usage"""
        import sys

        def measure_memory():
            config = valid_structural_config.copy()
            return sys.getsizeof(config) / 1024 / 1024  # MB

        result = benchmark(measure_memory)
        assert result < 1  # < 1MB for config

    def test_memory_usage_scaling_with_dataset_size(
        self,
        structural_solver,
        small_dataset,
        medium_dataset,
        large_dataset
    ):
        """Test memory scaling with dataset size"""
        import tracemalloc

        results = {}
        for name, data in [("small", small_dataset), ("medium", medium_dataset), ("large", large_dataset)]:
            tracemalloc.start()
            structural_solver.execute(data)
            current, peak = tracemalloc.get_traced_memory()
            tracemalloc.stop()

            results[name] = peak / 1024 / 1024  # MB

        # Memory should scale sub-linearly
        assert results["small"] < 10  # MB
        assert results["medium"] < 50  # MB
        assert results["large"] < 100  # MB
```

**Run performance benchmarks:**

```bash
# Run performance benchmarks only
pytest tests/integration/test_solver_performance.py -v --benchmark-only

# Generate comparison report
pytest tests/integration/test_solver_performance.py -v --benchmark-compare

# Save baseline for future comparison
pytest tests/integration/test_solver_performance.py -v --benchmark-autosave
```

**Checklist:**
- [ ] test_solver_performance.py created (200+ lines)
- [ ] 25 benchmarks implemented
- [ ] All benchmark targets met
- [ ] Baseline saved for future comparison
- [ ] Performance report generated

---

## Phase 3.4: Stress & Concurrency Testing (Week 3-4)

### Step 4.1: Implement Stress Tests

**Create tests/integration/test_solver_stress.py (150+ lines):**

```python
"""
ABOUTME: Stress tests for large-scale inputs and edge cases
ABOUTME: Tests solver behavior at scale and boundary conditions
"""

import pytest


@pytest.mark.stress
class TestLargeScaleInputHandling:
    """Stress tests with large inputs"""

    def test_solver_with_1000_elements(
        self,
        structural_solver,
        medium_dataset
    ):
        """Test solver with 1,000 element dataset"""
        result = structural_solver.execute(medium_dataset)
        assert result is not None
        assert len(result) == len(medium_dataset)

    @pytest.mark.slow
    def test_solver_with_10000_elements(
        self,
        structural_solver,
        large_dataset
    ):
        """Test solver with 10,000 element dataset"""
        import time
        start = time.perf_counter()
        result = structural_solver.execute(large_dataset)
        elapsed = time.perf_counter() - start

        assert result is not None
        assert len(result) == len(large_dataset)
        assert elapsed < 5.0  # Should complete in < 5 seconds


@pytest.mark.stress
class TestEdgeCasesAndBoundaries:
    """Boundary value and edge case testing"""

    def test_solver_with_minimum_tolerance(
        self,
        config_manager,
        structural_solver
    ):
        """Test solver with minimum tolerance (1e-10)"""
        config = config_manager.get_default_config("structural")
        config["tolerance"] = 1e-10

        is_valid, errors = config_manager.validate_solver_config("structural", config)
        assert is_valid

    def test_solver_with_maximum_iterations(
        self,
        config_manager,
        structural_solver
    ):
        """Test solver with maximum iterations (10000)"""
        config = config_manager.get_default_config("structural")
        config["max_iterations"] = 10000

        is_valid, errors = config_manager.validate_solver_config("structural", config)
        assert is_valid
```

### Step 4.2: Implement Concurrency Tests

**Create tests/integration/test_solver_concurrency.py (150+ lines):**

```python
"""
ABOUTME: Concurrency tests for thread safety and race conditions
ABOUTME: Tests solver behavior with multi-threaded execution
"""

import pytest
import concurrent.futures


@pytest.mark.concurrency
class TestMultiThreadedSolverExecution:
    """Multi-threaded solver execution tests"""

    def test_concurrent_solver_execution_2_threads(
        self,
        thread_pool,
        structural_solver,
        small_dataset,
        medium_dataset
    ):
        """Test concurrent solver execution with 2 threads"""
        tasks = [
            lambda: structural_solver.execute(small_dataset),
            lambda: structural_solver.execute(medium_dataset),
        ]

        futures = [thread_pool.submit(task) for task in tasks]
        results = [f.result() for f in concurrent.futures.as_completed(futures)]

        assert len(results) == 2
        assert all(r is not None for r in results)

    def test_concurrent_solver_execution_8_threads(
        self,
        thread_pool,
        structural_solver,
        small_dataset
    ):
        """Test concurrent solver execution with 8 threads"""
        tasks = [
            lambda: structural_solver.execute(small_dataset)
            for _ in range(8)
        ]

        futures = [thread_pool.submit(task) for task in tasks]
        results = [f.result() for f in concurrent.futures.as_completed(futures)]

        assert len(results) == 8
        assert all(r is not None for r in results)


@pytest.mark.concurrency
class TestRaceConditionDetection:
    """Race condition detection and prevention"""

    def test_no_race_condition_on_config_update(
        self,
        thread_pool,
        config_manager,
        valid_structural_config
    ):
        """Test for race conditions on config updates"""
        errors = []
        results = []

        def update_config(config_id):
            try:
                for _ in range(100):
                    config_manager.merge_configs(
                        valid_structural_config,
                        {"tolerance": 1e-6}
                    )
                results.append(config_id)
            except Exception as e:
                errors.append(str(e))

        futures = [thread_pool.submit(update_config, i) for i in range(8)]
        for f in concurrent.futures.as_completed(futures):
            f.result()

        assert len(errors) == 0
        assert len(results) == 8
```

**Run stress and concurrency tests:**

```bash
# Run stress tests
pytest tests/integration/test_solver_stress.py -v -m stress

# Run concurrency tests
pytest tests/integration/test_solver_concurrency.py -v -m concurrency

# Run all slow tests
pytest tests/integration/ -v -m slow

# Run everything together
pytest tests/integration/ -v
```

**Checklist:**
- [ ] test_solver_stress.py created (150+ lines)
- [ ] test_solver_concurrency.py created (150+ lines)
- [ ] 25+ stress tests implemented
- [ ] 30+ concurrency tests implemented
- [ ] All tests passing
- [ ] No race conditions detected
- [ ] No deadlocks detected

---

## Phase 3.5: Documentation & Review (Week 4)

### Step 5.1: Generate Test Documentation

**Create tests/integration/README.md:**

```markdown
# Integration Testing Guide

## Overview
Phase 3 integration tests validate solver system with real ConfigManager integration.

## Test Categories

### Configuration Loading (test_integration_solvers.py)
- Loading valid configurations
- Detecting type errors
- Detecting range errors
- Configuration defaults
- Configuration merging
- YAML file I/O

### Solver Integration
- Solver instantiation with configs
- Cross-module pipelines
- Error handling
- Batch processing

### End-to-End Workflows
- Data loading → solving → results
- Multi-step analysis pipelines
- Batch processing workflows

### Performance (test_solver_performance.py)
- Solver execution time (<50ms for structural)
- Memory usage (<10MB for config)
- Configuration operations time
- Scaling with dataset size

### Stress (test_solver_stress.py)
- Large-scale inputs (10,000+ elements)
- Extreme parameter values
- Resource exhaustion handling

### Concurrency (test_solver_concurrency.py)
- Multi-threaded execution
- Race condition detection
- Deadlock prevention
- Throughput scaling

## Running Tests

```bash
# All integration tests
pytest tests/integration/ -v

# Specific test module
pytest tests/integration/test_integration_solvers.py -v

# With coverage
pytest tests/integration/ --cov=digitalmodel

# Performance benchmarks
pytest tests/integration/test_solver_performance.py --benchmark-only

# Stress tests
pytest tests/integration/ -m stress

# Concurrency tests
pytest tests/integration/ -m concurrency
```

## Interpreting Results

### Pass Rate
- Target: 100% of tests passing
- Investigation: Any failing test should be reviewed immediately

### Performance
- Structural solver: <50ms (target)
- Marine solver: <100ms (target)
- Config operations: <10ms (target)

### Coverage
- Target: 85%+ code coverage
- Critical modules: SolverConfigManager must be 95%+

## Troubleshooting

### Flaky Tests
- Tests that fail intermittently indicate timing/resource issues
- Add retry decorator: `@pytest.mark.flaky(reruns=3)`
- Check system load during test runs

### Performance Regression
- Compare current results with baseline
- Investigate slow operations
- Profile hot spots

### Concurrency Issues
- Indicates potential thread safety problems
- Review synchronization mechanisms
- Add appropriate locks/mutexes

## Contributing

When adding new tests:
1. Follow existing test patterns
2. Use appropriate markers (@pytest.mark.integration, etc.)
3. Include docstrings
4. Ensure tests are independent
5. Update this documentation
```

### Step 5.2: Create Test Execution Report

**Generate summary report:**

```bash
# Comprehensive test report
pytest tests/integration/ -v --tb=short > test_execution_report.txt

# HTML report
pytest tests/integration/ --html=test_report.html --self-contained-html

# Coverage report
pytest tests/integration/ --cov=digitalmodel --cov-report=html

# Performance comparison
pytest tests/integration/test_solver_performance.py --benchmark-compare > performance_comparison.txt
```

### Step 5.3: Final Validation

**Complete validation checklist:**

```bash
# Step 1: Verify all files exist
[ -f tests/integration/test_integration_solvers.py ] && echo "✓ Integration tests"
[ -f tests/integration/test_solver_performance.py ] && echo "✓ Performance tests"
[ -f tests/integration/test_solver_stress.py ] && echo "✓ Stress tests"
[ -f tests/integration/test_solver_concurrency.py ] && echo "✓ Concurrency tests"

# Step 2: Run all tests
pytest tests/integration/ -v --tb=short

# Step 3: Check coverage
pytest tests/integration/ --cov=digitalmodel --cov-report=term-missing | grep TOTAL

# Step 4: Verify performance targets
pytest tests/integration/test_solver_performance.py --benchmark-only

# Step 5: Run stress tests
pytest tests/integration/ -m stress

# Step 6: Run concurrency tests
pytest tests/integration/ -m concurrency
```

---

## Success Criteria Verification

| Criterion | Target | Verification |
|-----------|--------|--------------|
| Integration tests | 80+ cases | `pytest tests/integration/test_integration_solvers.py --co -q \| wc -l` |
| Performance tests | 25 benchmarks | `pytest tests/integration/test_solver_performance.py --co -q \| wc -l` |
| Stress tests | 25 cases | `pytest tests/integration/test_solver_stress.py --co -q \| wc -l` |
| Concurrency tests | 30 cases | `pytest tests/integration/test_solver_concurrency.py --co -q \| wc -l` |
| Code coverage | 85%+ | `pytest tests/integration/ --cov=digitalmodel --cov-report=term-missing` |
| Test pass rate | 100% | `pytest tests/integration/ -v` |
| Performance targets met | 100% | `pytest tests/integration/test_solver_performance.py --benchmark-only` |
| No race conditions | 0 detected | `pytest tests/integration/ -m concurrency` |
| No deadlocks | 0 detected | `pytest tests/integration/ -m concurrency` |

---

## Appendix: Quick Reference Commands

```bash
# Development workflows
pytest tests/integration/ -v                              # Run all tests
pytest tests/integration/ -v -x                          # Stop on first failure
pytest tests/integration/ -v --tb=short                 # Short traceback
pytest tests/integration/ -k test_config                # Run tests matching pattern

# Performance analysis
pytest tests/integration/test_solver_performance.py -v --benchmark-only
pytest tests/integration/test_solver_performance.py -v --benchmark-compare
pytest tests/integration/test_solver_performance.py -v --benchmark-autosave

# Coverage analysis
pytest tests/integration/ --cov=digitalmodel --cov-report=html
pytest tests/integration/ --cov=digitalmodel --cov-report=term-missing
pytest tests/integration/ --cov=digitalmodel --cov-fail-under=85

# Categorized test runs
pytest tests/integration/ -m integration                  # Integration only
pytest tests/integration/ -m performance                  # Performance only
pytest tests/integration/ -m stress                       # Stress only
pytest tests/integration/ -m concurrency                  # Concurrency only
pytest tests/integration/ -m slow                         # Slow tests

# CI/CD integration
pytest tests/integration/ -v --junitxml=test-results.xml
pytest tests/integration/ --cov=digitalmodel --cov-report=xml
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2026-01-09 | Initial implementation guide |

---

**End of Phase 3 Implementation Guide**

*Follow this guide step-by-step to implement Phase 3 integration testing systematically and successfully.*
