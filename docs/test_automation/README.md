# Test Suite Automation System

## Overview

The Test Suite Automation System provides continuous monitoring, execution, and maintenance of the repository's comprehensive test suite. This system ensures all tests remain current, properly categorized, and executable while providing detailed reporting and automated failure resolution.

## Features

- **Automated Test Discovery**: Automatically identifies and categorizes test files across the repository
- **Parallel Test Execution**: Runs tests in parallel for improved performance
- **Intelligent Failure Analysis**: Analyzes test failures and categorizes them for automatic resolution
- **Auto-Fix Engine**: Automatically resolves common test failures (import errors, missing files, etc.)
- **Comprehensive Reporting**: Generates detailed reports in multiple formats (HTML, JSON, CSV)
- **CI/CD Integration**: Seamless integration with continuous integration pipelines
- **Manual Review Queue**: Flags complex issues that require developer attention

## Quick Start

### Installation

The test automation system is included as part of the digitalmodel package. To install with test automation dependencies:

```bash
pip install -e .[test-automation]
```

### Basic Usage

```bash
# Run all tests across all modules
test-automation run-all --parallel --coverage

# Run tests for a specific module
test-automation run-module aqwa --verbose

# Check overall test suite status
test-automation status

# Automatically fix failing tests
test-automation fix-failing --fix-auto

# Generate comprehensive report
test-automation report --format html

# Clean up test artifacts
test-automation clean --all
```

## Configuration

The system uses `test_automation_settings.yml` for configuration:

```yaml
paths:
  base_dir: "tests"
  modules_dir: "tests/modules"

execution:
  parallel: true
  max_workers: 4
  timeout_seconds: 300

reporting:
  output_dir: "test_automation_reports"
  formats: ["html", "json"]

autofix:
  enabled: true
  confidence_threshold: 0.8
```

## Architecture

### Core Components

- **TestDiscoveryEngine**: Discovers and categorizes test files
- **ModuleTestRunner**: Executes tests with parallel support
- **FailureAnalyzer**: Analyzes test failures for patterns
- **AutoFixEngine**: Applies automated fixes for common issues
- **TestReportGenerator**: Creates comprehensive reports

### Test Categories

The system automatically categorizes tests into:

- **Engineering Modules**: Domain-specific analysis tests (aqwa, orcaflex, pipe_capacity)
- **Core Calculations**: Fundamental calculation tests (fatigue_analysis, time_series)
- **Integration Tests**: Workflow and configuration validation tests
- **Development Status**: Tests by readiness (in_progress, no_license, unresolved)

## Command Reference

### `run-all`
Execute all tests across modules with optional parallel execution and auto-fix.

**Options:**
- `--parallel`: Enable parallel execution
- `--verbose`: Show detailed output
- `--coverage`: Generate coverage reports
- `--fix-auto`: Automatically fix resolvable issues
- `--mark-manual`: Flag unfixable tests for manual review

### `run-module <module_name>`
Execute tests for a specific module.

**Arguments:**
- `module_name`: Name of the module to test (e.g., aqwa, orcaflex, pipeline)

### `status`
Show current test suite health and statistics.

**Options:**
- `--detailed`: Include detailed status information

### `fix-failing`
Analyze and fix failing tests automatically.

**Options:**
- `--fix-auto`: Apply fixes automatically
- `--dry-run`: Show what would be fixed without making changes
- `--mark-manual`: Flag complex issues for developer review

### `report`
Generate comprehensive test execution reports.

**Options:**
- `--format {html,json,csv}`: Report format
- `--output PATH`: Output file path
- `--include-trends`: Include historical trend analysis

### `clean`
Clean up test artifacts and temporary files.

**Options:**
- `--cache`: Clean execution cache
- `--reports`: Clean old reports
- `--logs`: Clean old log files
- `--all`: Clean all temporary files

## Logging

The system provides comprehensive logging:

- **Console Output**: Real-time progress and status
- **Detailed Logs**: Complete execution traces
- **Error Logs**: Isolated error information
- **JSON Logs**: Structured data for analysis

Log files are stored in the `logs/` directory with automatic rotation.

## Integration

### CI/CD Pipeline

Example GitHub Actions integration:

```yaml
- name: Run Test Automation
  run: |
    pip install -e .[test-automation]
    test-automation run-all --parallel --fix-auto --coverage
    test-automation report --format json
```

### Pre-commit Hooks

Add to `.pre-commit-config.yaml`:

```yaml
- repo: local
  hooks:
  - id: test-automation
    name: Test Automation
    entry: test-automation run-module
    language: system
    types: [python]
```

## Troubleshooting

### Common Issues

1. **Import Errors**: The auto-fix engine can resolve most import issues by creating mock implementations
2. **Missing Files**: System automatically creates placeholder files for missing dependencies
3. **Licensed Software**: Tests requiring licenses (OrcaFlex, ANSYS) are properly isolated and mocked
4. **Timeout Issues**: Adjust `timeout_seconds` in configuration for long-running tests

### Performance Optimization

- Use parallel execution for faster test runs
- Adjust `max_workers` based on system capabilities
- Enable caching for repeated test discovery
- Use module-specific execution for targeted testing

### Getting Help

- Check logs in the `logs/` directory for detailed error information
- Use `--verbose` flag for detailed console output
- Review auto-fix suggestions for common issues
- Check the manual review queue for complex problems

## Development

### Adding New Auto-Fix Patterns

To add new automatic fix patterns:

1. Extend the `FailureAnalyzer` class with new pattern recognition
2. Implement fix logic in the `AutoFixEngine`
3. Add confidence scoring for the new pattern
4. Include tests for the new fix pattern

### Custom Reporting

To create custom report formats:

1. Extend the `TestReportGenerator` class
2. Implement the new format generation method
3. Register the format in the CLI options
4. Add configuration options as needed

## Support

For issues and feature requests, please create an issue in the repository.