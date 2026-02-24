# Tests Specification

This is the tests coverage details for the spec detailed in @specs/modules/mooring/comprehensive-analysis/spec.md

> Created: 2024-12-20
> Version: 1.0.0

## Test Coverage

### Unit Tests

**PretensionAnalyzer**
- Test CSV parsing with valid pretension data
- Test handling of malformed CSV files
- Test convergence calculation with various tolerance levels
- Test statistical metrics computation
- Test identification of problem lines
- Test edge cases (zero tension, negative values, missing data)

**StiffnessAnalyzer**
- Test CSV parsing with valid stiffness data
- Test 6-DOF matrix construction
- Test eigenvalue/eigenvector computation
- Test natural period estimation with different vessel masses
- Test handling of singular matrices
- Test anisotropy detection

**FenderForcesAnalyzer**
- Test CSV parsing with valid fender data
- Test utilization rate calculation
- Test load sharing computation
- Test contact duration metrics
- Test critical fender identification
- Test capacity assessment against design limits

**GroupComparator**
- Test automatic group identification from filenames
- Test manual group override functionality
- Test statistical comparison metrics
- Test ranking algorithms
- Test trend identification
- Test handling of single-member groups

**ContextExtractor**
- Test pattern matching for standard filename formats
- Test LLM fallback for complex patterns
- Test vessel type extraction
- Test environmental condition extraction
- Test confidence scoring
- Test caching mechanism

**ComprehensiveSummarizer**
- Test individual summary generation
- Test group summary creation
- Test overall summary compilation
- Test recommendation generation
- Test compliance checking
- Test markdown formatting

### Integration Tests

**End-to-End Analysis Workflow**
- Test complete analysis pipeline from CSV to report
- Test parallel processing with multiple files
- Test error recovery during batch processing
- Test memory management with large datasets
- Test configuration loading and validation

**Multi-File Processing**
- Test processing of mixed file types (pretension + stiffness + fender)
- Test handling of incomplete data sets
- Test group comparison with varying group sizes
- Test cross-referencing between analysis types

**Report Generation**
- Test markdown report with embedded images
- Test HTML report generation
- Test Excel workbook creation
- Test plot embedding and formatting
- Test hyperlink generation

### Performance Tests

**Scalability Testing**
- Test with 10, 50, 100, 500 CSV files
- Measure processing time vs file count
- Verify < 60 second target for 100 files
- Test memory usage scaling
- Test parallel vs sequential performance

**Large File Handling**
- Test with CSV files > 100MB
- Test with 10,000+ row datasets
- Verify memory efficiency
- Test streaming vs batch processing

### Error Handling Tests

**Data Quality Issues**
- Test with missing columns
- Test with corrupted data
- Test with inconsistent formats
- Test with empty files
- Test with non-CSV files

**System Failures**
- Test disk space exhaustion
- Test memory limit enforcement
- Test file access permissions
- Test network failures (for LLM)
- Test keyboard interrupt handling

### Regression Tests

**Calculation Accuracy**
- Verify pretension convergence calculations against known results
- Validate stiffness matrix computations with analytical solutions
- Confirm fender force calculations with manual checks
- Test statistical metrics against reference implementations

**Standards Compliance**
- Test DNV-OS-E301 criteria application
- Test API RP 2SK compliance checks
- Test safety factor calculations
- Test vessel-specific threshold application

## Test Data

### Sample CSV Files

**Pretension Test Data**
```
test_data/
├── pretension/
│   ├── converged_case.csv
│   ├── non_converged_case.csv
│   ├── partial_convergence.csv
│   └── malformed_data.csv
```

**Stiffness Test Data**
```
test_data/
├── stiffness/
│   ├── symmetric_stiffness.csv
│   ├── asymmetric_stiffness.csv
│   ├── singular_matrix.csv
│   └── large_stiffness.csv
```

**Fender Test Data**
```
test_data/
├── fender/
│   ├── normal_loading.csv
│   ├── overloaded_fender.csv
│   ├── no_contact.csv
│   └── multiple_fenders.csv
```

### Test Fixtures

```python
@pytest.fixture
def sample_pretension_data():
    """Generate sample pretension DataFrame"""
    return pd.DataFrame({
        'ObjectName': ['Line1', 'Line2'],
        'target_tension': [1000, 1200],
        'current_tension': [980, 1210],
        'tension_diff_percent': [2.0, 0.83]
    })

@pytest.fixture
def sample_config():
    """Generate test configuration"""
    return AnalysisConfig(
        input_directory=Path('./test_data'),
        output_directory=Path('./test_output')
    )
```

## Mocking Requirements

**External Services**
- **LLM API**: Mock OpenAI/Anthropic API responses for context extraction
- **File System**: Mock file operations for unit tests
- **Multiprocessing**: Mock parallel processing for deterministic testing

**Mock Strategies**

```python
# LLM Mocking
@patch('langchain.llms.OpenAI')
def test_context_extraction(mock_llm):
    mock_llm.return_value.predict.return_value = {
        'vessel_type': 'LNGC',
        'water_depth': 150,
        'environment': 'operational'
    }
    
# File System Mocking
@patch('pathlib.Path.glob')
def test_file_discovery(mock_glob):
    mock_glob.return_value = [
        Path('file1.csv'),
        Path('file2.csv')
    ]
    
# Parallel Processing Mocking
@patch('multiprocessing.Pool')
def test_parallel_analysis(mock_pool):
    mock_pool.return_value.map.return_value = [
        result1, result2, result3
    ]
```

## Test Execution

### Running Tests

```bash
# Run all tests
pytest tests/modules/orcaflex/mooring_analysis/

# Run specific test category
pytest tests/modules/orcaflex/mooring_analysis/unit/

# Run with coverage
pytest --cov=digitalmodel.orcaflex.mooring_analysis --cov-report=html

# Run performance tests
pytest tests/modules/orcaflex/mooring_analysis/performance/ --benchmark-only

# Run integration tests
pytest tests/modules/orcaflex/mooring_analysis/integration/ --integration
```

### Continuous Integration

```yaml
# .github/workflows/test.yml
name: Mooring Analysis Tests
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Set up Python
        uses: actions/setup-python@v2
        with:
          python-version: '3.10'
      - name: Install dependencies
        run: |
          pip install -r requirements.txt
          pip install pytest pytest-cov pytest-benchmark
      - name: Run tests
        run: |
          pytest tests/modules/orcaflex/mooring_analysis/ \
            --cov=digitalmodel.orcaflex.mooring_analysis \
            --cov-report=xml
```

## Test Documentation

Each test should include:
- Clear docstring explaining what is being tested
- Arrangement of test data
- Action being tested
- Assertion of expected results
- Cleanup if necessary

Example:
```python
def test_pretension_convergence_calculation():
    """
    Test that pretension convergence is correctly calculated.
    
    Given a DataFrame with target and current tensions,
    When calculate_convergence is called,
    Then the convergence percentage should match expected values.
    """
    # Arrange
    data = create_test_pretension_data()
    analyzer = PretensionAnalyzer()
    
    # Act
    metrics = analyzer.calculate_metrics(data)
    
    # Assert
    assert metrics.convergence_percentage == pytest.approx(95.0, rel=1e-2)
    assert len(metrics.problem_lines) == 1
    
    # No cleanup needed
```