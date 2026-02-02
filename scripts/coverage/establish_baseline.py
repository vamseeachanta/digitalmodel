#!/usr/bin/env python
"""
Script to establish test baseline for the repository.
Runs all tests and captures results for baseline documentation.
"""
import json
import subprocess
import sys
from datetime import datetime
from pathlib import Path

def run_tests_and_capture():
    """Run pytest and capture results."""
    print("Running test discovery and execution...")
    
    # Test discovery
    discovery_cmd = [sys.executable, "-m", "pytest", "tests/", "--collect-only", "-q"]
    discovery_result = subprocess.run(discovery_cmd, capture_output=True, text=True)
    
    # Count tests from discovery
    total_tests = 0
    for line in discovery_result.stdout.split('\n'):
        if '<' in line and '>' in line:
            total_tests += 1
    
    print(f"Discovered {total_tests} tests")
    
    # Run actual tests
    print("\nRunning tests...")
    test_cmd = [sys.executable, "-m", "pytest", "tests/", "-v", "--tb=short"]
    start_time = datetime.now()
    test_result = subprocess.run(test_cmd, capture_output=True, text=True)
    execution_time = (datetime.now() - start_time).total_seconds()
    
    # Parse results
    passed = failed = skipped = errors = 0
    
    for line in test_result.stdout.split('\n'):
        if ' PASSED' in line:
            passed += 1
        elif ' FAILED' in line:
            failed += 1
        elif ' SKIPPED' in line:
            skipped += 1
        elif ' ERROR' in line:
            errors += 1
    
    # Check for collection errors
    if 'errors during collection' in test_result.stdout:
        import re
        error_match = re.search(r'(\d+) errors? during collection', test_result.stdout)
        if error_match:
            errors = int(error_match.group(1))
    
    return {
        'total_tests': total_tests,
        'passed': passed,
        'failed': failed,
        'skipped': skipped,
        'errors': errors,
        'execution_time': execution_time,
        'output': test_result.stdout,
        'return_code': test_result.returncode
    }

def analyze_test_modules():
    """Analyze test modules and their status."""
    modules = {}
    test_dir = Path('tests/modules')
    
    for module_dir in test_dir.iterdir():
        if module_dir.is_dir() and not module_dir.name.startswith('__'):
            # Count test files in module
            test_files = list(module_dir.glob('test_*.py'))
            if test_files:
                # Try to run tests for this module specifically
                module_cmd = [sys.executable, "-m", "pytest", str(module_dir), "-q"]
                result = subprocess.run(module_cmd, capture_output=True, text=True)
                
                status = 'passing' if result.returncode == 0 else 'has_errors'
                modules[module_dir.name] = {
                    'files': len(test_files),
                    'status': status
                }
    
    return modules

def create_baseline_json(results, modules):
    """Create baseline JSON file."""
    baseline = {
        'date': datetime.now().isoformat(),
        'summary': {
            'total_tests': results['total_tests'],
            'passed': results['passed'],
            'failed': results['failed'],
            'skipped': results['skipped'],
            'errors': results['errors'],
            'pass_rate': round((results['passed'] / max(results['total_tests'], 1)) * 100, 1),
            'execution_time_seconds': round(results['execution_time'], 2)
        },
        'modules': modules,
        'command': 'python -m pytest tests/ -v',
        'environment': 'repository_python',
        'python_version': f"{sys.version_info.major}.{sys.version_info.minor}"
    }
    
    # Save baseline
    baseline_file = Path('tests/test_baseline_new.json')
    with open(baseline_file, 'w') as f:
        json.dump(baseline, f, indent=2)
    
    print(f"\nBaseline saved to: {baseline_file}")
    return baseline

def create_baseline_markdown(baseline):
    """Create baseline markdown report."""
    date = datetime.now().strftime('%Y-%m-%d')
    
    summary = baseline['summary']
    total = summary['total_tests']
    passed = summary['passed']
    failed = summary['failed']
    skipped = summary['skipped']
    errors = summary['errors']
    
    md_content = f"""# Test Baseline Report
**Date Established**: {date}
**Environment**: Python {baseline['python_version']}

## Executive Summary
- **Total Tests Discovered**: {total}
- **Passing Tests**: {passed} ({summary['pass_rate']}%)
- **Failed Tests**: {failed}
- **Skipped Tests**: {skipped}
- **Collection Errors**: {errors}
- **Execution Time**: {summary['execution_time_seconds']} seconds

## Test Distribution by Module

| Module | Test Files | Status |
|--------|------------|--------|
"""
    
    for module, info in sorted(baseline['modules'].items()):
        status_icon = "✅" if info['status'] == 'passing' else "⚠️"
        md_content += f"| {module} | {info['files']} | {status_icon} {info['status'].replace('_', ' ').title()} |\n"
    
    md_content += f"""

## Test Categories

### Working Tests ({passed} tests)
- Tests that execute successfully without errors
- Primarily configuration and model tests

### Tests with Import Errors ({errors} tests)
- Tests that fail to import due to missing modules
- Require module path fixes or missing dependencies

## Baseline Metrics

### Performance Metrics
- **Average Test Duration**: {summary['execution_time_seconds']/max(passed, 1):.2f} seconds per test
- **Total Execution Time**: {summary['execution_time_seconds']} seconds

## Test Command
```bash
# Run all tests
python -m pytest tests/ -v

# Run working tests only (OrcaFlex mooring analysis)
python -m pytest tests/modules/orcaflex/mooring_analysis/ -v

# Quick test run
python -m pytest tests/ -q
```

## Known Issues
1. **Import Errors**: Many test files have import errors due to module path issues
2. **Missing Dependencies**: Some tests require specific modules that may not be installed
3. **Path Issues**: Tests may need updates to work with current project structure

## Next Steps for Improvement
1. Fix import errors in test files
2. Update module paths to match current structure
3. Add missing test dependencies
4. Increase test coverage
5. Set up proper test fixtures

---
*This baseline represents the current state after test recovery from git history.*
"""
    
    baseline_md = Path('tests/TEST_BASELINE_NEW.md')
    with open(baseline_md, 'w', encoding='utf-8') as f:
        f.write(md_content)
    
    print(f"Markdown report saved to: {baseline_md}")

def main():
    """Main function to establish baseline."""
    print("=" * 60)
    print("ESTABLISHING TEST BASELINE")
    print("=" * 60)
    
    # Run tests
    results = run_tests_and_capture()
    
    # Analyze modules
    print("\nAnalyzing test modules...")
    modules = analyze_test_modules()
    
    # Create baseline files
    print("\nCreating baseline records...")
    baseline = create_baseline_json(results, modules)
    create_baseline_markdown(baseline)
    
    # Print summary
    print("\n" + "=" * 60)
    print("BASELINE ESTABLISHED")
    print("=" * 60)
    print(f"Total Tests: {results['total_tests']}")
    print(f"Passed: {results['passed']}")
    print(f"Failed: {results['failed']}")
    print(f"Errors: {results['errors']}")
    print(f"Skipped: {results['skipped']}")
    print(f"Execution Time: {results['execution_time']:.2f} seconds")
    
    if results['errors'] > 0:
        print(f"\n⚠️  Warning: {results['errors']} tests have collection errors (likely import issues)")
        print("These need to be fixed for a complete baseline")

if __name__ == "__main__":
    main()