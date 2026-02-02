# Quality Gates Validation System

Comprehensive quality gate validation system for automated code quality checks with configurable thresholds and CI/CD integration.

## Overview

The Quality Gates system executes a series of validation checks in linear order to ensure code quality, test coverage, security, and documentation standards are met before commits and deployments.

## Features

- **Linear Gate Execution**: Tests → Coverage → Quality → Security → Documentation
- **Configurable Thresholds**: Define custom pass/warn/fail levels for each gate
- **Multiple Integration Points**:
  - Pre-commit hooks (hard block)
  - CI/CD pipelines (PR validation)
  - Manual CLI (interactive reports)
- **Comprehensive Tooling**:
  - **Tests**: pytest execution with fail-fast
  - **Coverage**: pytest-cov with JSON export
  - **Quality**: Ruff linting + McCabe complexity analysis
  - **Security**: Bandit vulnerability scanning
  - **Documentation**: Custom docstring coverage checker
- **Flexible Reporting**: Console, JSON, and future HTML dashboards

## Installation

The quality gates system is included in the `digitalmodel` package:

```bash
# Install in editable mode with uv
uv pip install -e .

# Verify installation
python -m digitalmodel.automation.quality_gates_cli --help
```

## Configuration

### Quality Gates Config (`.claude/quality-gates.yaml`)

```yaml
gates:
  tests:
    enabled: true
    order: 1
    command: "pytest --maxfail=1 -x"
    failure_action: "block"

  coverage:
    enabled: true
    order: 2
    depends_on: ["tests"]
    thresholds:
      failure: 60.0  # <60% = failure
      warning: 80.0  # 60-80% = warning, ≥80% = pass

  quality:
    enabled: true
    order: 3
    tools:
      ruff:
        complexity_threshold:
          warning: 10   # Cyclomatic complexity ≥10 = warning
          failure: 15   # Cyclomatic complexity ≥15 = failure

  security:
    enabled: true
    order: 4
    tools:
      bandit:
        severity_threshold:
          high: "failure"
          medium: "warning"
          low: "report"

  documentation:
    enabled: true
    order: 5
    threshold: 75.0  # 75% of functions with docstrings
```

### Threshold Configuration

**Code Coverage**:
- ≥80%: **Pass** ✅
- 60-80%: **Warning** ⚠️
- <60%: **Failure** ❌

**Code Quality (Cyclomatic Complexity)**:
- <10: **Pass** ✅
- 10-14: **Warning** ⚠️
- ≥15: **Failure** ❌

**Security (Bandit)**:
- No high severity: **Pass** ✅
- Medium severity: **Warning** ⚠️
- High severity: **Failure** ❌

**Documentation**:
- ≥75% functions with docstrings: **Pass** ✅
- <75%: **Warning** ⚠️ (not blocking)

## Usage

### Command Line Interface

```bash
# Run all quality gates (soft warnings)
python -m digitalmodel.automation.quality_gates_cli check

# Run with strict mode (warnings become failures)
python -m digitalmodel.automation.quality_gates_cli check --strict

# Output results as JSON
python -m digitalmodel.automation.quality_gates_cli check --json

# Generate detailed report
python -m digitalmodel.automation.quality_gates_cli report

# Custom config file
python -m digitalmodel.automation.quality_gates_cli check --config custom-gates.yaml
```

### Python API

```python
from pathlib import Path
from digitalmodel.automation.quality_gates import QualityGateValidator

# Initialize validator
validator = QualityGateValidator(
    config_path=Path(".claude/quality-gates.yaml"),
    strict_mode=False
)

# Execute all gates
report = validator.execute_all_gates()

# Print formatted report
validator.print_report(report)

# Check overall status
if report.overall_status.value == "failure":
    print("Quality gates failed!")
    exit(1)
```

### Pre-commit Integration

The quality gates are automatically integrated into pre-commit hooks:

```yaml
# .pre-commit-config.yaml
repos:
  - repo: local
    hooks:
      - id: quality-gates
        name: Quality Gates Validation
        entry: python -m digitalmodel.automation.quality_gates_cli check --strict
        language: system
        pass_filenames: false
        always_run: true
        stages: [commit]
```

Install pre-commit hooks:

```bash
pre-commit install
```

### CI/CD Integration (GitHub Actions)

```yaml
# .github/workflows/quality-gates.yml
name: Quality Gates

on:
  pull_request:
    branches: [main, develop]

jobs:
  quality-gates:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-python@v5
        with:
          python-version: '3.11'

      - name: Install dependencies
        run: |
          pip install uv
          uv pip install -e .

      - name: Run Quality Gates
        run: |
          python -m digitalmodel.automation.quality_gates_cli check --json

      - name: Upload results
        uses: actions/upload-artifact@v4
        with:
          name: quality-gate-results
          path: reports/quality_gates_results.json
```

## Gate Descriptions

### 1. Tests Gate

**Purpose**: Ensure all unit and integration tests pass

**Tool**: pytest with fail-fast mode

**Behavior**:
- Runs `pytest --maxfail=1 -x`
- Stops on first test failure
- All tests must pass for gate to succeed

**Failure Action**: Hard block

### 2. Coverage Gate

**Purpose**: Verify adequate test coverage

**Tool**: pytest-cov with JSON output

**Dependencies**: Tests must pass first

**Thresholds**:
- `<60%`: Failure (hard block)
- `60-80%`: Warning
- `≥80%`: Pass

**Output**: `coverage.json` with line-by-line coverage data

### 3. Quality Gate

**Purpose**: Enforce code quality standards and complexity limits

**Tool**: Ruff (linter + complexity checker)

**Dependencies**: Tests and coverage must pass

**Checks**:
- Linting violations (PEP 8, best practices)
- Cyclomatic complexity (McCabe)
- Code smells and anti-patterns

**Complexity Thresholds**:
- `<10`: Pass
- `10-14`: Warning
- `≥15`: Failure

### 4. Security Gate

**Purpose**: Detect security vulnerabilities

**Tool**: Bandit (Python security linter)

**Dependencies**: Tests, coverage, and quality must pass

**Severity Levels**:
- **High**: Immediate failure (SQL injection, hardcoded secrets, etc.)
- **Medium**: Warning (weak crypto, insecure functions)
- **Low**: Report only

**Output**: `reports/bandit_report.json`

### 5. Documentation Gate

**Purpose**: Ensure code is properly documented

**Tool**: Custom AST-based docstring analyzer

**Dependencies**: None (can run independently)

**Checks**:
- Function docstrings
- Class docstrings
- Module docstrings

**Threshold**: 75% of functions must have docstrings

**Failure Action**: Warning only (not blocking)

## Execution Flow

```
┌─────────────────────────────────────────────────┐
│              Quality Gates Execution             │
└─────────────────────────────────────────────────┘

  1. Tests Gate ────────────────► Pass? ──► Continue
         │                         │
         └──────── Fail ───────────┴──► BLOCK (stop)

  2. Coverage Gate ─────────────► Pass? ──► Continue
         │                         │
         ├──────── Warn ───────────┴──► Continue (log)
         └──────── Fail ───────────┴──► BLOCK (stop)

  3. Quality Gate ──────────────► Pass? ──► Continue
         │                         │
         ├──────── Warn ───────────┴──► Continue (log)
         └──────── Fail ───────────┴──► BLOCK (stop)

  4. Security Gate ─────────────► Pass? ──► Continue
         │                         │
         ├──────── Warn ───────────┴──► Continue (log)
         └──────── Fail ───────────┴──► BLOCK (stop)

  5. Documentation Gate ────────► Pass? ──► SUCCESS
         │                         │
         └──────── Warn ───────────┴──► SUCCESS (log)

                  ↓
         ┌─────────────────┐
         │  Generate Report │
         │  Export Results  │
         └─────────────────┘
```

## Output Formats

### Console Report

```
================================================================================
Quality Gates Report
================================================================================

Overall Status: PASS
Execution Time: 12.34s

Summary:
  ✓ Passed:  4
  ⚠ Warned:  1
  ✗ Failed:  0
  ○ Skipped: 0

Gate Results:
--------------------------------------------------------------------------------

✓ TESTS
  All tests passed
  Metrics:
    • exit_code: 0

✓ COVERAGE
  Coverage 85.50% meets requirements
  Metrics:
    • coverage_percent: 85.50
    • lines_covered: 855
    • lines_total: 1000

⚠ QUALITY
  Code complexity 12 exceeds warning threshold (10)
  Metrics:
    • total_issues: 3
    • max_complexity: 12
    • complexity_issues: 1

✓ SECURITY
  No security issues detected
  Metrics:
    • high_severity: 0
    • medium_severity: 0
    • low_severity: 2

✓ DOCUMENTATION
  Documentation coverage 78.5% meets requirements
  Metrics:
    • total_functions: 200
    • documented_functions: 157
    • coverage_percent: 78.50

================================================================================
```

### JSON Report (`reports/quality_gates_results.json`)

```json
{
  "overall_status": "pass",
  "gates_executed": 5,
  "gates_passed": 4,
  "gates_warned": 1,
  "gates_failed": 0,
  "execution_time": 12.34,
  "results": [
    {
      "gate_name": "tests",
      "status": "pass",
      "message": "All tests passed",
      "metrics": {"exit_code": 0}
    },
    {
      "gate_name": "coverage",
      "status": "pass",
      "message": "Coverage 85.50% meets requirements",
      "metrics": {
        "coverage_percent": 85.5,
        "lines_covered": 855,
        "lines_total": 1000
      }
    }
  ]
}
```

## Strict Mode

In strict mode, **warnings become failures**:

```bash
# Enable strict mode
python -m digitalmodel.automation.quality_gates_cli check --strict
```

**Use Cases**:
- Production deployments
- Main branch protection
- Release validation
- Continuous integration

**Behavior**:
- Coverage 60-80%: **Failure** (normally warning)
- Complexity 10-14: **Failure** (normally warning)
- Medium security issues: **Failure** (normally warning)

## Customization

### Custom Gate Configuration

Create a custom configuration file:

```yaml
# custom-gates.yaml
gates:
  tests:
    enabled: true
    order: 1
    command: "pytest -v --cov"

  coverage:
    enabled: true
    order: 2
    thresholds:
      failure: 70.0  # Stricter threshold
      warning: 90.0

  quality:
    enabled: true
    order: 3
    tools:
      ruff:
        complexity_threshold:
          warning: 8   # Lower tolerance
          failure: 12

settings:
  cli:
    strict_mode: true  # Always strict
```

Use custom config:

```bash
python -m digitalmodel.automation.quality_gates_cli check --config custom-gates.yaml
```

### Disable Specific Gates

```yaml
gates:
  documentation:
    enabled: false  # Skip documentation checks
```

## Troubleshooting

### Gate Failures

**Tests fail**:
```bash
# Run tests directly to see failures
pytest -v --tb=short
```

**Coverage too low**:
```bash
# Check coverage report
pytest --cov --cov-report=html
# Open htmlcov/index.html
```

**High complexity**:
```bash
# Find complex functions
ruff check --select=C901
```

**Security issues**:
```bash
# Run Bandit directly
bandit -r src/ -f json
```

### Permission Errors

If encountering Windows permission errors with coverage files:

```bash
# Clear coverage data
rm -f .coverage*
```

## Future Enhancements

- [ ] **HTML Dashboard**: Interactive Plotly-based dashboard
- [ ] **Per-module Thresholds**: Different thresholds for different modules
- [ ] **Progressive Gates**: Stricter rules for main branch
- [ ] **Gate Caching**: Skip unchanged gates
- [ ] **Parallel Execution**: Run independent gates concurrently
- [ ] **Custom Gates**: Plugin system for project-specific gates

## Files Created

```
.claude/
  └── quality-gates.yaml                    # Configuration

src/digitalmodel/modules/automation/
  ├── quality_gates.py                      # Core validator
  └── quality_gates_cli.py                  # CLI interface

tests/modules/automation/
  └── test_quality_gates.py                 # Comprehensive tests

.pre-commit-config.yaml                     # Pre-commit integration

.github/workflows/
  └── quality-gates.yml                     # CI/CD integration

reports/
  ├── quality_gates_results.json            # JSON results
  └── bandit_report.json                    # Security scan
```

## Related Documentation

- **Testing Framework**: `docs/CLI_TESTING_FRAMEWORK_COMPLETE.md`
- **HTML Standards**: `docs/modules/standards/HTML_REPORTING_STANDARDS.md`
- **Development Workflow**: `docs/modules/workflow/DEVELOPMENT_WORKFLOW.md`

## Support

For issues or questions about quality gates:

1. Check configuration in `.claude/quality-gates.yaml`
2. Review gate-specific documentation above
3. Run with `--verbose` flag for detailed logging
4. Check test output in `tests/modules/automation/test_quality_gates.py`
