# CLI Testing Framework - Next Steps & Roadmap

> **Current Status:** Phase 2 Complete - 165 tests, 87.8% pass rate
> **Document Version:** 1.0.0
> **Last Updated:** 2026-01-06

---

## 🎯 Executive Summary

The CLI Testing Framework Phase 2 is complete with **165 comprehensive integration tests** covering all 8 CLI modules. While the framework is production-ready, there are **18 failing tests** from earlier phases that need attention, plus several recommended enhancements for optimal performance.

**Quick Win:** Fix the 18 failing tests to achieve **100% pass rate** (Estimated: 3-4 hours)

---

## 📋 Immediate Priorities (This Week)

### Priority 1: Fix Failing Tests (CRITICAL)

**Goal:** Achieve 100% test pass rate
**Estimated Time:** 3-4 hours
**Impact:** High - Ensures test suite reliability

#### Task 1.1: Fix structural_analysis CLI Tests (6 failures)

**Files:** `tests/cli/test_structural_analysis_cli.py`

**Failing Tests:**
1. `test_full_stress_analysis_workflow` - Integration scenario
2. `test_basic_plate_buckling` - Mock configuration
3. `test_plate_buckling_thick_plate` - Mock configuration
4. `test_plate_buckling_thin_plate` - Mock configuration
5. `test_plate_buckling_json_output` - Mock configuration
6. `test_plate_buckling_with_boundary_conditions` - Partial

**Root Cause:** Mock return values don't match current CLI expectations

**Action Steps:**
```bash
# 1. Run tests to see exact failure messages
pytest tests/cli/test_structural_analysis_cli.py::TestBucklingPlateCommand -v

# 2. Update mock responses to match current BucklingAnalyzer API
# 3. Verify JSON output structure matches actual implementation
# 4. Re-run tests to confirm fixes
```

**Example Fix Pattern:**
```python
# Before (incorrect mock structure)
mock_result.buckling_coefficient = 4.5

# After (correct mock structure based on actual API)
mock_result.buckling_load = 125000.0
mock_result.critical_stress = 285.5
mock_result.safety_factor = 1.85
```

---

#### Task 1.2: Fix diffraction CLI Tests (6 failures)

**Files:** `tests/cli/test_diffraction_cli.py`

**Failing Tests:**
1. `test_convert_aqwa_with_formats` - Directory handling
2. `test_basic_aqwa_conversion` - Directory handling
3. `test_convert_aqwa_no_validate` - Mock configuration
4. `test_basic_comparison` - Mock configuration

**Root Cause:** Directory path handling and converter mock structure

**Action Steps:**
```bash
# 1. Run tests to see exact failures
pytest tests/cli/test_diffraction_cli.py::TestConvertAQWACommand -v

# 2. Fix directory path handling (likely temp_output_dir usage)
# 3. Update converter mock responses
# 4. Verify AQWA folder structure expectations
```

**Example Fix:**
```python
# Add proper directory setup in test
def test_basic_aqwa_conversion(self, cli_runner, temp_output_dir):
    # Create AQWA folder structure
    aqwa_folder = temp_output_dir / "aqwa_results"
    aqwa_folder.mkdir()
    (aqwa_folder / "AQWA.LIS").touch()  # Create expected files

    # Then run converter test...
```

---

#### Task 1.3: Fix mooring_analysis CLI Tests (6 failures)

**Files:** `tests/cli/test_mooring_analysis_cli.py`

**Failing Tests:**
1. `test_design_with_different_system_types` - Mock response
2. `test_basic_design_verification` - Mock response
3. `test_design_with_json_output` - Mock response
4. `test_design_with_damaged_line_analysis` - Mock response

**Root Cause:** MooringDesigner mock response structure mismatch

**Action Steps:**
```bash
# 1. Check current MooringDesigner API
# 2. Update mock responses to match
pytest tests/cli/test_mooring_analysis_cli.py::TestDesignCommand -v
```

**Expected Fix:**
```python
# Verify MooringDesigner returns correct structure
mock_result.line_count = 8
mock_result.system_type = 'spread'
mock_result.total_pretension = 750000.0
mock_result.max_line_tension = 950000.0
mock_result.safety_factor = 1.95
```

---

#### Task 1.4: Fix signal_analysis CLI Test (1 failure)

**Files:** `tests/cli/test_signal_analysis_cli.py`

**Failing Test:**
1. `test_lowpass_filter` - Filter output validation

**Action Steps:**
```bash
pytest tests/cli/test_signal_analysis_cli.py::TestFilterCommand::test_lowpass_filter -v
```

---

### Priority 2: Update Documentation (MEDIUM)

**Goal:** Ensure all documentation reflects current state
**Estimated Time:** 1 hour
**Impact:** Medium - Helps future developers

**Tasks:**
- ✅ Create `CLI_TESTING_FRAMEWORK_COMPLETE.md` (DONE)
- ✅ Create `CLI_TESTING_NEXT_STEPS.md` (This document)
- ⏳ Update main repository README with testing section
- ⏳ Create CLI testing best practices guide
- ⏳ Add troubleshooting guide for common issues

---

## 📅 Short-term Goals (Next 2 Weeks)

### Goal 1: Add True Integration Tests

**Estimated Time:** 4-6 hours
**Impact:** High - Validates actual implementation behavior

**Approach:**
```
tests/cli/
├── unit/                    # Current mock-based tests (rename)
│   ├── test_structural_analysis_cli.py
│   └── ...
├── integration/             # New true integration tests
│   ├── test_structural_integration.py
│   ├── test_workflow_integration.py
│   └── ...
└── conftest.py
```

**Example Integration Test:**
```python
# tests/cli/integration/test_structural_integration.py
"""True integration tests with actual implementations"""

def test_full_stress_analysis_no_mocks(cli_runner, temp_output_dir):
    """Test stress command with real calculations"""
    output_file = temp_output_dir / "stress_results.json"

    # No mocking - use actual implementations
    result = cli_runner.invoke(cli, [
        'stress',
        '--sigma-x', '150',
        '--sigma-y', '100',
        '--tau-xy', '50',
        '--material', 's355',
        '--output', str(output_file)
    ])

    # Validate actual calculations
    assert result.exit_code == 0
    data = json.loads(output_file.read_text())

    # Check actual von Mises calculation
    expected_vm = sqrt(150**2 + 100**2 - 150*100 + 3*50**2)
    assert abs(data['von_mises_stress'] - expected_vm) < 0.1
```

---

### Goal 2: Improve Coverage Reporting

**Estimated Time:** 2 hours
**Impact:** Medium - Better metrics visibility

**Actions:**

1. **Create CLI-specific coverage config:**

```ini
# .coveragerc
[run]
source = src/digitalmodel/modules/*/cli.py
omit =
    */tests/*
    */__pycache__/*

[report]
precision = 2
show_missing = True
skip_covered = False
fail_under = 90

[html]
directory = htmlcov_cli
title = CLI Test Coverage Report
```

2. **Run CLI-focused coverage:**

```bash
# Coverage for CLI modules only
pytest tests/cli/ --cov=digitalmodel.modules --cov-report=html:htmlcov_cli \
      --cov-config=.coveragerc_cli
```

3. **Generate coverage badges:**

```bash
# Install coverage-badge
pip install coverage-badge

# Generate badge
coverage-badge -o coverage_cli.svg
```

---

### Goal 3: Add Performance Benchmarks

**Estimated Time:** 2 hours
**Impact:** Low - Nice to have

**Example:**
```python
# tests/cli/performance/test_cli_performance.py
import pytest

@pytest.mark.benchmark
def test_stress_command_performance(benchmark, cli_runner):
    """Benchmark stress command execution time"""

    def run_stress():
        return cli_runner.invoke(cli, [
            'stress',
            '--sigma-x', '150',
            '--sigma-y', '100'
        ])

    result = benchmark(run_stress)

    # Command should complete in < 1 second
    assert benchmark.stats['mean'] < 1.0
```

---

## 🚀 Medium-term Enhancements (Next Month)

### Enhancement 1: Parametrized Testing

**Goal:** Reduce code duplication, increase test coverage variations

**Current (repetitive):**
```python
def test_material_steel(self):
    result = cli_runner.invoke(cli, ['cmd', '--material', 's355'])
    assert_cli_success(result)

def test_material_titanium(self):
    result = cli_runner.invoke(cli, ['cmd', '--material', 'titanium'])
    assert_cli_success(result)
```

**Improved (parametrized):**
```python
@pytest.mark.parametrize("material,expected_yield", [
    ('s355', 355),
    ('x65', 448),
    ('titanium', 880),
    ('s420', 420),
])
def test_different_materials(self, material, expected_yield, cli_runner):
    result = cli_runner.invoke(cli, ['cmd', '--material', material])
    assert_cli_success(result)
    assert str(expected_yield) in result.output
```

**Impact:** 50% reduction in test code, better test organization

---

### Enhancement 2: Snapshot Testing

**Goal:** Catch unexpected output changes

**Installation:**
```bash
pip install pytest-snapshot
```

**Usage:**
```python
def test_stress_output_format(snapshot, cli_runner):
    """Ensure output format doesn't change unexpectedly"""
    result = cli_runner.invoke(cli, [
        'stress',
        '--sigma-x', '150',
        '--sigma-y', '100'
    ])

    # First run creates snapshot, future runs compare
    snapshot.assert_match(result.output)
```

**Benefit:** Automatic regression detection for output format changes

---

### Enhancement 3: Test Data Factories

**Goal:** Generate realistic test data programmatically

**Implementation:**
```python
# tests/cli/factories.py
from dataclasses import dataclass
from typing import Dict, Any

@dataclass
class StressTestData:
    """Factory for stress test scenarios"""
    sigma_x: float
    sigma_y: float
    tau_xy: float
    material: str = 's355'

    @classmethod
    def yield_point_scenario(cls) -> 'StressTestData':
        """Scenario near material yield point"""
        return cls(sigma_x=350, sigma_y=320, tau_xy=80, material='s355')

    @classmethod
    def low_stress_scenario(cls) -> 'StressTestData':
        """Low stress scenario"""
        return cls(sigma_x=50, sigma_y=30, tau_xy=10)

    def to_cli_args(self) -> list:
        """Convert to CLI arguments"""
        return [
            '--sigma-x', str(self.sigma_x),
            '--sigma-y', str(self.sigma_y),
            '--tau-xy', str(self.tau_xy),
            '--material', self.material
        ]

# Usage in tests
def test_yield_point(self, cli_runner):
    data = StressTestData.yield_point_scenario()
    result = cli_runner.invoke(cli, ['stress'] + data.to_cli_args())
    assert 'WARNING' in result.output  # Near yield point
```

---

### Enhancement 4: Cross-platform Testing

**Goal:** Ensure CLI works on Windows, Linux, macOS

**GitHub Actions Workflow:**
```yaml
# .github/workflows/cli-tests.yml
name: CLI Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]
        python-version: ['3.10', '3.11', '3.12']

    steps:
      - uses: actions/checkout@v3
      - name: Set up Python
        uses: actions/setup-python@v4
        with:
          python-version: ${{ matrix.python-version }}

      - name: Install dependencies
        run: |
          pip install -e .
          pip install pytest pytest-cov

      - name: Run CLI tests
        run: pytest tests/cli/ -v --cov=digitalmodel.modules

      - name: Upload coverage
        uses: codecov/codecov-action@v3
```

---

## 📊 Long-term Vision (Next Quarter)

### Vision 1: Automated Test Generation

**Goal:** Generate CLI tests from CLI definitions automatically

**Concept:**
```python
# Generate tests from Click command decorators
def generate_tests_from_cli(cli_module):
    """Auto-generate basic tests from CLI structure"""
    for command in cli_module.commands:
        # Generate help test
        # Generate parameter validation tests
        # Generate basic execution test
```

---

### Vision 2: CLI Mutation Testing

**Goal:** Verify test quality by introducing mutations

**Tool:** `mutmut`

```bash
# Install
pip install mutmut

# Run mutation testing on CLI modules
mutmut run --paths-to-mutate=src/digitalmodel/modules/*/cli.py
```

**Expected Outcome:** Identify weak tests that don't catch bugs

---

### Vision 3: Property-Based Testing

**Goal:** Test CLI with randomly generated valid inputs

**Tool:** `hypothesis`

```python
from hypothesis import given, strategies as st

@given(
    sigma_x=st.floats(min_value=0, max_value=500),
    sigma_y=st.floats(min_value=0, max_value=500),
    tau_xy=st.floats(min_value=0, max_value=200),
)
def test_stress_command_properties(sigma_x, sigma_y, tau_xy, cli_runner):
    """Test stress command with random valid inputs"""
    result = cli_runner.invoke(cli, [
        'stress',
        '--sigma-x', str(sigma_x),
        '--sigma-y', str(sigma_y),
        '--tau-xy', str(tau_xy)
    ])

    # Properties that should always hold
    assert result.exit_code == 0
    assert 'von_mises' in result.output.lower()
    # Von Mises should be >= 0
    # etc.
```

---

## 🎯 Success Metrics

### Current Metrics (Baseline)

```
Total Tests:           165
Pass Rate:             87.8%
Test Code Lines:       4,452
CLI Modules Covered:   8/8 (100%)
Avg Test Time:         ~7 seconds
```

### Target Metrics (After Phase 3)

```
Total Tests:           200+ (add integration tests)
Pass Rate:             100% (fix all failures)
Test Code Lines:       5,000+
CLI-Specific Coverage: 95%+
Avg Test Time:         < 10 seconds
```

### Target Metrics (After Phase 4)

```
Total Tests:           250+ (add performance, parametrized)
Pass Rate:             100%
Test Code Lines:       4,500 (reduced via parametrization)
Overall Coverage:      90%+
Mutation Score:        80%+
Cross-platform:        ✅ Windows, Linux, macOS
```

---

## 📝 Action Items Checklist

### Immediate (This Week)
- [ ] Fix 6 structural_analysis test failures
- [ ] Fix 6 diffraction test failures
- [ ] Fix 6 mooring_analysis test failures
- [ ] Fix 1 signal_analysis test failure
- [ ] Update repository README with testing section
- [ ] Create CLI testing best practices guide

### Short-term (Next 2 Weeks)
- [ ] Add true integration tests (no mocks)
- [ ] Configure CLI-specific coverage reporting
- [ ] Add performance benchmark tests
- [ ] Set up coverage badges
- [ ] Create troubleshooting guide

### Medium-term (Next Month)
- [ ] Convert repetitive tests to parametrized tests
- [ ] Implement snapshot testing for output validation
- [ ] Create test data factories
- [ ] Set up cross-platform CI testing
- [ ] Add mutation testing

### Long-term (Next Quarter)
- [ ] Implement automated test generation
- [ ] Add property-based testing with Hypothesis
- [ ] Achieve 90%+ overall coverage
- [ ] Achieve 80%+ mutation score
- [ ] Complete cross-platform validation

---

## 🔗 Related Documents

- [CLI Testing Framework Complete](CLI_TESTING_FRAMEWORK_COMPLETE.md) - Full documentation
- [CLI Testing Best Practices](CLI_TESTING_BEST_PRACTICES.md) - Guidelines (to be created)
- [Troubleshooting Guide](CLI_TESTING_TROUBLESHOOTING.md) - Common issues (to be created)
- [Mock Strategy Guide](MOCK_STRATEGY_GUIDE.md) - Mocking patterns (to be created)

---

## 📞 Support & Questions

For questions about the CLI testing framework:
1. Check the [complete documentation](CLI_TESTING_FRAMEWORK_COMPLETE.md)
2. Review test examples in `tests/cli/`
3. Check `conftest.py` for available fixtures
4. Review this roadmap for planned enhancements

---

**Document Owner:** Development Team
**Last Review:** 2026-01-06
**Next Review:** 2026-01-13 (after immediate priorities complete)
**Status:** 🟢 Active - Phase 2 Complete, Phase 3 Planning
