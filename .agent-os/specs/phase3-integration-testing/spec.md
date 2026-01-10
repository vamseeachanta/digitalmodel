# Phase 3: Integration Testing Specification

> Spec: phase3-integration-testing
> Created: 2025-01-09
> Status: Active Development
> Version: 1.0.0

## Executive Summary

Phase 3 establishes comprehensive integration testing infrastructure for the DigitalModel platform. Building on Phase 2's structural solver implementation, Phase 3 validates that all system components (OrcaFlex integration, AQWA processing, configuration management, data pipelines) function correctly together under realistic conditions, performance constraints, and stress scenarios.

**Key Objectives:**
- Verify end-to-end workflows across all modules
- Validate performance against established thresholds
- Stress test with realistic data volumes and concurrent operations
- Establish benchmarking infrastructure for regression detection
- Implement automated integration test suite with 85%+ coverage

**Success Criteria:**
- All integration tests pass consistently on CI/CD
- Performance benchmarks within 5% of targets
- 85%+ coverage of integration paths
- Zero regressions in performance metrics
- Stress tests handle 10,000+ file batches
- Complete documentation with runnable examples

---

## Overview

### Phase 3 Context

**Previous Phases:**
- **Phase 1:** Foundation - YAML config system, basic OrcaFlex integration, unit test framework
- **Phase 2:** Structural Solvers - ConfigManager, solver abstraction, multi-solver support

**Phase 3 Focus:**
- **Integration Testing:** Verify complete workflows work end-to-end
- **Performance Validation:** Test against established performance targets
- **Stress Testing:** Validate system behavior under high load
- **Benchmarking:** Establish performance baselines and track regressions
- **Concurrent Operations:** Test thread-safe operations with multiple concurrent jobs

### Integration Testing Scope

Integration tests verify that:

1. **Configuration System** works with all solver types
2. **Solver Implementations** execute correctly via ConfigManager
3. **Data Pipelines** process results correctly
4. **Output Generation** creates valid reports and exports
5. **Error Handling** gracefully manages partial failures
6. **Concurrent Operations** execute safely with proper resource management
7. **Performance Requirements** are met under realistic load

### Real-World Scenarios Tested

1. **Typical OrcaFlex Analysis:**
   - Load YAML config with OrcaFlex parameters
   - Process 100-500 simulation files
   - Extract time series and statistics
   - Generate HTML report with interactive plots
   - Export results to CSV/Excel
   - **Expected Time:** 2-5 minutes for 500 files

2. **Parallel Multi-File Processing:**
   - Process 1,000+ OrcaFlex files concurrently
   - Track memory usage across processes
   - Handle file I/O concurrency
   - Aggregate results from parallel workers
   - **Expected Time:** <30 seconds for 1,000 files

3. **Configuration Validation:**
   - Load various YAML configurations
   - Validate against schema
   - Test error cases (missing files, invalid parameters)
   - Verify error messages are helpful
   - **Expected Success Rate:** 99.9% for valid configs

4. **Cross-Module Integration:**
   - Load OrcaFlex results
   - Process with structural analysis module
   - Generate fatigue summary
   - Create combined report
   - **Expected Completeness:** 100% of valid paths

5. **Stress Testing:**
   - Process 10,000 files in batch
   - Multiple concurrent analysis jobs
   - Memory pressure scenarios
   - Network failure simulation
   - **Expected Stability:** No crashes, graceful degradation

---

## Integration Testing Categories

### Category 1: Component Integration

**Goal:** Verify components work together correctly

**Tests Include:**
- ConfigManager loads and passes config to solvers correctly
- Solvers execute with config parameters
- Results returned in expected format
- Error handling flows work correctly
- Mock vs. real API behavior consistent

**Example Test:**
```python
def test_config_manager_with_orcaflex_solver():
    """Integration test: ConfigManager → OrcaFlex Solver"""
    # Load test configuration
    config = ConfigManager("tests/fixtures/orcaflex_config.yaml")

    # Verify config loaded
    assert config.solver_type == "orcaflex"
    assert len(config.files) > 0

    # Get solver from config
    solver = config.get_solver()

    # Execute analysis
    results = solver.analyze(config)

    # Verify results structure
    assert results is not None
    assert results.get_time_series() is not None
    assert results.get_statistics() is not None
```

### Category 2: Data Pipeline Integration

**Goal:** Verify data flows correctly through processing pipeline

**Tests Include:**
- Raw simulation results loaded correctly
- Data transformations produce expected output
- Statistical calculations are accurate
- Filtered datasets match criteria
- Aggregation produces correct summaries

**Example Test:**
```python
def test_pipeline_data_flow():
    """Integration test: Raw data → Processing → Output"""
    # Load raw OrcaFlex results
    raw_data = load_mock_orcaflex_results("tests/fixtures/sample_sim.txt")

    # Process through pipeline
    pipeline = DataPipeline(config)
    processed = pipeline.process(raw_data)

    # Verify transformations
    assert len(processed.time_series) == len(raw_data) * config.time_steps
    assert processed.statistics["max_tension"] > 0
    assert processed.statistics["min_tension"] >= 0
    assert processed.statistics["mean_tension"] > processed.statistics["min_tension"]
```

### Category 3: Output Generation

**Goal:** Verify all output formats generate correctly

**Tests Include:**
- HTML reports create valid markup
- Interactive plots render correctly
- CSV exports contain expected columns
- Excel sheets format correctly
- PDF generation without errors

**Example Test:**
```python
def test_output_generation():
    """Integration test: Results → Multiple Output Formats"""
    results = run_sample_analysis()

    # Generate all outputs
    html_file = results.export_html()
    csv_file = results.export_csv()
    excel_file = results.export_excel()

    # Verify files created
    assert os.path.exists(html_file)
    assert os.path.exists(csv_file)
    assert os.path.exists(excel_file)

    # Validate content
    assert is_valid_html(html_file)
    assert has_interactive_plots(html_file)
    assert is_valid_csv(csv_file)
    assert is_valid_excel(excel_file)
```

### Category 4: End-to-End Workflows

**Goal:** Verify complete workflows from config to report

**Tests Include:**
- Configuration → Analysis → Report generation
- Error handling throughout workflow
- Resource cleanup after completion
- Log output is informative
- Reproducibility of results

**Example Test:**
```python
def test_complete_workflow():
    """Integration test: Complete workflow from config to report"""
    # Define test configuration
    config_yaml = """
    solver: orcaflex
    files:
      - tests/fixtures/sim1.txt
      - tests/fixtures/sim2.txt
    output:
      format: html
      path: tests/output/report.html
    """

    # Write config to temp file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml') as f:
        f.write(config_yaml)
        config_path = f.name

        # Run complete workflow
        workflow = DigitalModelWorkflow(config_path)
        report_path = workflow.execute()

        # Verify report created
        assert os.path.exists(report_path)
        assert is_valid_html(report_path)
        assert has_plots(report_path)
        assert has_statistics_table(report_path)
```

### Category 5: Concurrent Operations

**Goal:** Verify thread-safe operation with multiple concurrent jobs

**Tests Include:**
- Multiple analysis jobs run simultaneously
- Results don't interfere with each other
- Thread-safe resource access (file I/O, memory)
- Proper cleanup after concurrent completion
- Performance scales with CPU cores

**Example Test:**
```python
def test_concurrent_analyses():
    """Integration test: Multiple concurrent analyses"""
    from concurrent.futures import ThreadPoolExecutor

    configs = [
        ConfigManager(f"tests/fixtures/config_{i}.yaml")
        for i in range(5)
    ]

    # Run analyses concurrently
    with ThreadPoolExecutor(max_workers=5) as executor:
        futures = [
            executor.submit(analyze_config, config)
            for config in configs
        ]
        results = [f.result() for f in futures]

    # Verify all completed successfully
    assert len(results) == 5
    assert all(r is not None for r in results)

    # Verify results are independent (no cross-contamination)
    for r in results:
        assert r.get_statistics() is not None
```

### Category 6: Error Handling

**Goal:** Verify system handles errors gracefully

**Tests Include:**
- Missing configuration files handled
- Invalid YAML caught at load time
- Missing solver files reported clearly
- Partial failures don't crash entire batch
- Error messages are actionable

**Example Test:**
```python
def test_error_handling():
    """Integration test: Error handling throughout workflow"""
    # Test missing config file
    with pytest.raises(FileNotFoundError):
        ConfigManager("nonexistent.yaml")

    # Test invalid YAML
    with pytest.raises(YAMLError):
        ConfigManager("tests/fixtures/invalid.yaml")

    # Test missing data files (should be reported, not crash)
    config = ConfigManager("tests/fixtures/config_missing_files.yaml")
    results = analyze_config(config)

    # Should complete with partial results and warnings
    assert results is not None
    assert len(results.warnings) > 0
    assert "missing file" in results.warnings[0].lower()
```

### Category 7: Performance & Stress Testing

**Goal:** Verify system meets performance requirements under load

**Tests Include:**
- Processes 1,000+ files within time budget
- Memory usage stays within limits
- CPU usage scales appropriately
- No memory leaks with large batches
- Handles 10,000+ file batch operations

**Example Test:**
```python
def test_performance_large_batch():
    """Integration test: Performance with large file batch"""
    # Create 1,000 test files
    test_files = create_test_files(count=1000)

    # Run analysis
    config = ConfigManager("tests/fixtures/performance_config.yaml")
    config.files = test_files

    start_time = time.time()
    start_memory = get_memory_usage()

    results = analyze_config(config)

    elapsed_time = time.time() - start_time
    memory_used = get_memory_usage() - start_memory

    # Verify performance
    assert elapsed_time < 30  # 30 seconds for 1,000 files
    assert memory_used < 500_000_000  # 500 MB max
    assert results.files_processed == 1000
```

---

## Integration Test Organization

### Directory Structure

```
tests/
├── integration/                      # Integration tests
│   ├── test_config_integration.py    # ConfigManager integration
│   ├── test_solver_integration.py    # Solver execution
│   ├── test_pipeline_integration.py  # Data pipeline
│   ├── test_output_integration.py    # Output generation
│   ├── test_workflow_integration.py  # End-to-end workflows
│   ├── test_concurrent_integration.py # Concurrent operations
│   └── test_error_handling.py        # Error scenarios
│
├── performance/                      # Performance tests
│   ├── test_performance.py           # Performance benchmarks
│   ├── test_stress.py                # Stress testing
│   └── test_memory.py                # Memory profiling
│
├── fixtures/                         # Test data
│   ├── configs/
│   │   ├── orcaflex_config.yaml
│   │   ├── aqwa_config.yaml
│   │   └── invalid_config.yaml
│   ├── data/
│   │   ├── orcaflex_sample.txt
│   │   ├── aqwa_sample.txt
│   │   └── large_dataset/
│   └── expected_outputs/
│       ├── expected_report.html
│       └── expected_stats.csv
│
└── conftest.py                       # Shared fixtures
```

### Fixture Organization

**Shared Fixtures (conftest.py):**
- `config_manager` - Preconfigured ConfigManager instances
- `mock_solver` - Mock solver for testing without licenses
- `sample_results` - Pre-computed results for validation
- `temp_workspace` - Temporary directory for test outputs
- `performance_benchmark` - Benchmark recording fixture

**Config Fixtures:**
- `orcaflex_config.yaml` - Valid OrcaFlex configuration
- `aqwa_config.yaml` - Valid AQWA configuration
- `invalid_config.yaml` - Invalid YAML for error testing
- `config_missing_files.yaml` - Config with nonexistent files

**Data Fixtures:**
- `orcaflex_sample.txt` - Sample OrcaFlex output file
- `aqwa_sample.txt` - Sample AQWA output file
- `large_dataset/` - 1,000+ files for performance testing

---

## Test Execution Strategy

### Test Levels

**Level 1: Fast Feedback (2-5 min)**
- Unit tests (Phase 2)
- Quick integration tests
- Mock-based tests
- Runs on every commit

**Level 2: Integration (10-15 min)**
- Component integration tests
- Pipeline tests
- Small batch processing
- Runs on PR

**Level 3: Regression (30-60 min)**
- Performance tests with realistic data
- Stress tests (10,000 files)
- Concurrent operation tests
- Runs nightly

**Level 4: Acceptance (1-2 hours)**
- Real CAE tool integration (if licensed)
- Full workflow tests
- Production data samples
- Runs weekly

### CI/CD Integration

```yaml
# .github/workflows/test.yml
stages:
  fast:
    runs: pytest tests/unit tests/integration/test_*.py -m "not performance"
    timeout: 10 min

  performance:
    runs: pytest tests/performance tests/integration -m "performance"
    timeout: 60 min

  stress:
    runs: pytest tests/performance/test_stress.py
    timeout: 120 min

  coverage:
    runs: pytest --cov=src --cov-report=html
    timeout: 30 min
```

---

## Success Criteria

### Functional Success
- [ ] All integration tests pass on CI/CD
- [ ] 85%+ integration path coverage
- [ ] End-to-end workflows execute successfully
- [ ] Error handling works for all tested scenarios
- [ ] Output formats validate correctly (HTML, CSV, Excel)

### Performance Success
- [ ] 100-500 files processed in <5 minutes
- [ ] 1,000 files processed in <30 seconds (parallel)
- [ ] Memory usage <500 MB for 1,000 file batch
- [ ] Concurrent operations scale with CPU cores
- [ ] No memory leaks detected in stress tests

### Quality Success
- [ ] 85%+ code coverage for integration paths
- [ ] All performance benchmarks documented
- [ ] Regression detection automated
- [ ] Performance baseline established
- [ ] Complete documentation with examples

### Operational Success
- [ ] All tests pass consistently
- [ ] No flaky tests (100% deterministic)
- [ ] Test failures point to root causes
- [ ] Clear debugging information in logs
- [ ] Easy to add new integration tests

---

## Related Documentation

### Specifications
- @.agent-os/specs/phase2-structural-solvers/spec.md - Previous phase
- @docs/TESTING_FRAMEWORK_STANDARDS.md - Testing standards
- @docs/HTML_REPORTING_STANDARDS.md - Report generation

### Implementation Guides
- integration-testing-guide.md - Real patterns and examples
- benchmark-framework.md - Performance benchmarking setup
- performance-targets.md - Performance thresholds by module

### Related Code
- src/config/manager.py - ConfigManager implementation
- src/solvers/ - Solver implementations
- tests/fixtures/ - Test data and configurations

---

## Deliverables

### Documentation (4 files, 1,300+ lines)
1. **spec.md** - This specification
2. **integration-testing-guide.md** - Real-world patterns and examples
3. **benchmark-framework.md** - Performance testing infrastructure
4. **performance-targets.md** - Performance thresholds and metrics
5. **tasks.md** - 40+ actionable implementation tasks

### Code (50+ test files)
- 25+ integration test files with 500+ test cases
- 10+ performance test files with baseline data
- 5+ fixture configuration files
- Benchmark and profiling utilities

### Infrastructure
- CI/CD pipeline configuration
- Performance baseline data
- Regression detection setup
- Automated reporting

### Quality Metrics
- 85%+ code coverage for integration paths
- 100% deterministic test execution
- <5% performance variance in benchmarks
- <60 second full test suite execution (fast tests)

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-01-09 | Initial Phase 3 integration testing specification |

---

**Status:** Ready for implementation
**Next Steps:** Begin Phase 3 tasks in tasks.md
**Estimated Duration:** 3-4 weeks
