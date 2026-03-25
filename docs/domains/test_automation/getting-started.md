# Getting Started with Test Automation

## Prerequisites

Before using the test automation system, ensure you have:

1. Python 3.8 or higher
2. The digitalmodel package installed
3. Access to the repository's test suite

## Installation

### Standard Installation

```bash
# Install the base package
pip install -e .

# Install with test automation dependencies
pip install -e .[test-automation]
```

### Development Installation

For development work on the test automation system:

```bash
# Install all development dependencies
pip install -e .[dev,test-automation]
```

## First Steps

### 1. Verify Installation

Check that the test automation CLI is available:

```bash
test-automation --version
test-automation --help
```

### 2. Check Current Status

Get an overview of your test suite:

```bash
test-automation status
```

This will show:
- Total number of modules discovered
- Total number of test files
- Current test suite health
- Recent execution history (if available)

### 3. Run a Simple Test

Start with testing a single module:

```bash
# Run tests for a specific module
test-automation run-module time_series --verbose

# Or try a smaller module first
test-automation run-module shapes --verbose
```

### 4. Run All Tests

Once you're comfortable with single modules, run the full suite:

```bash
# Run all tests with basic options
test-automation run-all

# Run with all features enabled
test-automation run-all --parallel --coverage --fix-auto
```

## Understanding Output

### Status Display

The status command provides key metrics:

```
📋 Test Suite Status Summary
====================================
Total modules discovered: 25
Total tests discovered: 342
Runnable tests: 298
Licensed software tests: 44
Last full run: 2025-01-15 14:30:22
Overall health: 85% passing
```

### Test Execution Output

During test execution, you'll see:

```
🚀 Starting comprehensive test suite execution...
📊 Test discovery and module categorization...
   ✅ Found 25 modules (298 runnable tests)
   ⏸️  Excluded 44 tests (licensed software required)

⚙️  Running tests in parallel (4 workers)...
   ✅ aqwa module: 15/15 tests passed
   ⚠️  pipeline module: 8/12 tests passed (4 auto-fixed)
   ❌ orcaflex module: 2/10 tests failed (needs manual review)

📊 Generating reports...
✅ Test execution completed!
```

### Report Generation

After execution, reports are generated in `test_automation_reports/`:

```
test_automation_reports/
├── latest.html          # Latest HTML report
├── latest.json          # Machine-readable results
├── trends/              # Historical data
└── archive/             # Archived reports
```

## Configuration

### Basic Configuration

Create or modify `test_automation_settings.yml`:

```yaml
# Basic execution settings
execution:
  parallel: true
  max_workers: 2  # Reduce for slower systems
  timeout_seconds: 300

# Enable auto-fix for common issues
autofix:
  enabled: true
  confidence_threshold: 0.7  # Lower = more aggressive fixing
```

### Environment Variables

You can also configure via environment variables:

```bash
export TEST_AUTOMATION_LOG_LEVEL=DEBUG
export TEST_AUTOMATION_CONFIG=custom_config.yml
export TEST_AUTOMATION_LOG_DIR=custom_logs/
```

## Common Workflows

### Daily Development Workflow

```bash
# Check status before starting work
test-automation status

# Run tests for your working module
test-automation run-module your_module --verbose

# Fix any issues found
test-automation fix-failing --fix-auto --dry-run
test-automation fix-failing --fix-auto

# Generate report for review
test-automation report --format html
```

### CI/CD Integration

```bash
# Full test suite for CI/CD
test-automation run-all --parallel --coverage --fix-auto
test-automation report --format json --output test_results.json
```

### Weekly Maintenance

```bash
# Full test suite with comprehensive reporting
test-automation run-all --parallel --coverage --mark-manual
test-automation report --format html --include-trends

# Clean up old files
test-automation clean --reports --logs
```

## Troubleshooting First Issues

### Common Installation Issues

1. **Missing Dependencies**
   ```bash
   pip install -e .[test-automation] --upgrade
   ```

2. **CLI Not Found**
   ```bash
   # Try running as module instead
   python -m test_automation --help
   ```

3. **Permission Issues**
   ```bash
   # Ensure write access to logs and reports directories
   mkdir -p logs test_automation_reports
   chmod 755 logs test_automation_reports
   ```

### Common Execution Issues

1. **Tests Taking Too Long**
   - Reduce `max_workers` in configuration
   - Increase `timeout_seconds` for complex tests
   - Use module-specific execution instead of run-all

2. **Import Errors**
   - The auto-fix engine should handle most import issues
   - Enable `--fix-auto` flag
   - Check logs for specific missing dependencies

3. **Licensed Software Issues**
   - Tests requiring licenses are automatically skipped
   - Use `--verbose` to see which tests are excluded
   - Mock implementations are created automatically

## Next Steps

Once you're comfortable with basic usage:

1. **Explore Advanced Features**
   - Custom report formats
   - Integration with development tools
   - Performance optimization

2. **Set Up Monitoring**
   - Automated daily test runs
   - Trend analysis and alerting
   - Integration with project management tools

3. **Contribute Improvements**
   - Add new auto-fix patterns
   - Enhance reporting capabilities
   - Improve test categorization

## Getting Help

- Use `test-automation --help` for command-specific help
- Check logs in `logs/` directory for detailed error information
- Review generated reports for test execution details
- Enable `--verbose` for detailed console output