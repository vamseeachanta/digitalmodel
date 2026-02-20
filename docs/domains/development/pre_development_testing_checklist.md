# Pre-Development Testing Checklist

## 🎯 **Purpose**
Ensure rock-solid testing foundation before adding new features and fixes to prevent regression and maintain high quality.

---

## **Phase 1: Baseline Establishment (CRITICAL - Do First)**

### ✅ **1.1 Test Stability Analysis**
```bash
# Run stability analysis to identify flaky tests
python test_stability_analysis.py
```
**Success Criteria:**
- [ ] Zero flaky tests detected across 3 runs
- [ ] Consistent pass/fail results for all test groups
- [ ] No test timeouts or crashes
- [ ] Performance variance < 20% between runs

**Actions if Failed:**
- Fix flaky tests before proceeding
- Investigate performance inconsistencies
- Stabilize failing tests or mark as known issues

### ✅ **1.2 Current Success Rate Validation**
```bash
# Confirm current success rate matches expectations
python -m pytest tests/no_license/ tests/in_progress/ tests/domains/transformation/ tests/domains/pipeline/ tests/domains/aqwa/ tests/domains/time_series/ tests/domains/viv_analysis/ tests/domains/catenary_riser/ tests/domains/code_dnvrph103/ tests/core/ --tb=no | grep -E "passed|failed"
```
**Success Criteria:**
- [ ] Success rate ≥ 70% (current: 72.8%)
- [ ] 91+ tests passing consistently
- [ ] No new test failures from baseline

### ✅ **1.3 Enhanced Validation Verification**
```bash
# Verify enhanced validations are working
python -m pytest tests/no_license/test_rao_analysis.py tests/no_license/test_cathodic_protection_basic.py tests/no_license/test_umbilical_analysis_line_properties.py tests/no_license/test_histogram.py -v
```
**Success Criteria:**
- [ ] All enhanced validation tests pass
- [ ] Detailed engineering assertions execute correctly
- [ ] Cross-validation logic functions properly
- [ ] Clear failure messages when validations fail

---

## **Phase 2: Regression Prevention Setup**

### ✅ **2.1 Baseline Performance Benchmarking**
```bash
# Establish performance baselines for critical tests
python -c "
import time
import subprocess

# Key performance-critical test groups
test_groups = [
    'tests/domains/aqwa/',
    'tests/domains/pipeline/', 
    'tests/no_license/',
    'tests/core/'
]

baselines = {}
for group in test_groups:
    start = time.time()
    result = subprocess.run(['python', '-m', 'pytest', group, '--tb=no'], 
                          capture_output=True)
    duration = time.time() - start
    baselines[group] = {
        'duration': duration,
        'success': result.returncode == 0
    }
    print(f'{group}: {duration:.1f}s - {'PASS' if result.returncode == 0 else 'FAIL'}')

# Save baselines
import json
with open('performance_baselines.json', 'w') as f:
    json.dump(baselines, f, indent=2)
print('\\nBaselines saved to performance_baselines.json')
"
```
**Success Criteria:**
- [ ] Performance baselines established for all test groups
- [ ] Total test execution time < 5 minutes
- [ ] Individual group execution time < 60 seconds
- [ ] Baseline file created for future comparison

### ✅ **2.2 Critical Path Coverage Analysis**
```bash
# Identify critical code paths that must be tested
python -c "
import ast
import os
from pathlib import Path

# Find all engine-related critical paths
critical_files = [
    'src/digitalmodel/engine.py',
    'src/digitalmodel/modules/aqwa/aqwa_analysis.py', 
    'src/digitalmodel/modules/orcaflex/orcaflex_analysis.py',
    'src/digitalmodel/modules/pipeline/pipeline.py'
]

coverage_map = {}
for file_path in critical_files:
    if os.path.exists(file_path):
        with open(file_path, 'r') as f:
            try:
                tree = ast.parse(f.read())
                functions = [node.name for node in ast.walk(tree) if isinstance(node, ast.FunctionDef)]
                coverage_map[file_path] = {
                    'functions': functions,
                    'function_count': len(functions),
                    'test_coverage_needed': len(functions) > 0
                }
            except:
                coverage_map[file_path] = {'error': 'Could not parse file'}
    else:
        coverage_map[file_path] = {'error': 'File not found'}

# Print analysis
for file_path, data in coverage_map.items():
    print(f'{file_path}:')
    if 'functions' in data:
        print(f'  Functions: {data[\"function_count\"]}')
        print(f'  Coverage needed: {data[\"test_coverage_needed\"]}')
    else:
        print(f'  {data[\"error\"]}')
    print()
"
```
**Success Criteria:**
- [ ] Critical paths identified and documented
- [ ] Existing test coverage mapped to critical functions
- [ ] Gaps in critical path coverage documented
- [ ] Plan created to fill coverage gaps

---

## **Phase 3: Test Quality & Maintenance**

### ✅ **3.1 Test Code Quality Review**
- [ ] **Test Isolation**: Each test can run independently
- [ ] **Mock Consistency**: All mocks use realistic domain data
- [ ] **Assertion Quality**: Clear, specific assertions with helpful error messages
- [ ] **Test Documentation**: Each test has clear purpose and expected behavior
- [ ] **Resource Cleanup**: No test leaves behind files/state

### ✅ **3.2 Validation Coverage Expansion**
```bash
# Plan to add validations to remaining failing tests
python -c "
failing_tests = [
    # Core test failures
    'tests/core/test_digitalmodel_init.py',
    'tests/core/test_engine_core.py', 
    'tests/core/test_module_loading.py',
    'tests/core/test_error_handling.py'
]

print('VALIDATION EXPANSION PLAN:')
print('==========================')
for test in failing_tests:
    print(f'❌ {test}')
    print('   - Add basic structure validation')
    print('   - Add domain-specific output checks')
    print('   - Add cross-validation logic')
    print()

print('Priority order:')
print('1. Core engine tests (highest impact)')
print('2. Error handling tests (stability)')  
print('3. Module loading tests (maintenance)')
print('4. Init tests (completeness)')
"
```

### ✅ **3.3 Test Maintenance Strategy**
- [ ] **Daily Monitoring**: Automated test runs with result tracking
- [ ] **Weekly Reviews**: Test failure analysis and flaky test identification  
- [ ] **Monthly Audits**: Test coverage gaps and performance regression review
- [ ] **Quarterly Planning**: Test infrastructure improvements and expansion

---

## **Phase 4: Development Integration Setup**

### ✅ **4.1 Pre-Commit Testing Hooks**
```bash
# Set up pre-commit testing (if not already done)
echo "Setting up pre-commit hooks..."

# Create pre-commit script
cat > .git/hooks/pre-commit << 'EOF'
#!/bin/bash
echo "🧪 Running pre-commit tests..."

# Run fast critical tests only
python -m pytest tests/no_license/test_histogram.py tests/no_license/test_cathodic_protection_basic.py tests/domains/transformation/test_transformation_fixed.py --tb=short

if [ $? -ne 0 ]; then
    echo "❌ Pre-commit tests failed! Fix tests before committing."
    exit 1
fi

echo "✅ Pre-commit tests passed!"
EOF

chmod +x .git/hooks/pre-commit
echo "Pre-commit hooks installed"
```

### ✅ **4.2 Feature Development Test Guidelines**
Create test requirements for new features:

**For Every New Feature:**
- [ ] **Unit Tests**: Individual function/class testing
- [ ] **Integration Tests**: Component interaction testing  
- [ ] **Validation Tests**: Engineering/domain-specific output validation
- [ ] **Regression Tests**: Ensure existing functionality unchanged
- [ ] **Performance Tests**: No significant performance degradation

**Test Coverage Requirements:**
- [ ] **New Code**: 80% line coverage minimum
- [ ] **Critical Paths**: 100% coverage required
- [ ] **Public APIs**: 100% coverage required  
- [ ] **Error Handling**: All error paths tested

---

## **Phase 5: Continuous Monitoring Setup**

### ✅ **5.1 Test Result Trending**
```bash
# Set up test result tracking
python -c "
import json
import time
from datetime import datetime

# Create test result tracking template
tracking_template = {
    'test_runs': [],
    'trends': {
        'success_rate_history': [],
        'performance_history': [],
        'flaky_test_tracking': [],
        'validation_failure_tracking': []
    },
    'alerts': {
        'success_rate_threshold': 70.0,
        'performance_degradation_threshold': 1.5,  # 50% slower
        'flaky_test_threshold': 3  # failures in 3 runs
    }
}

# Save template
with open('test_result_tracking.json', 'w') as f:
    json.dump(tracking_template, f, indent=2)

print('Test result tracking template created')
print('Use this to monitor test suite health over time')
"
```

### ✅ **5.2 Automated Alerting**
- [ ] **Success Rate Drop**: Alert if success rate drops below 70%
- [ ] **Performance Regression**: Alert if test execution time increases >50%
- [ ] **Flaky Test Detection**: Alert if test results inconsistent across runs
- [ ] **Validation Failures**: Alert if engineering validations start failing

---

## **DECISION GATES**

### 🚦 **Gate 1: Stability (MUST PASS before any new development)**
- [ ] All flaky tests fixed or documented as known issues
- [ ] Success rate maintained at ≥70% 
- [ ] Performance baselines established
- [ ] No critical test failures

### 🚦 **Gate 2: Quality (MUST PASS before major features)**
- [ ] Enhanced validations working on critical tests
- [ ] Test code quality review completed
- [ ] Critical path coverage documented
- [ ] Test maintenance strategy implemented

### 🚦 **Gate 3: Infrastructure (RECOMMENDED before team development)**
- [ ] Pre-commit hooks installed
- [ ] Automated monitoring setup
- [ ] Test result trending implemented
- [ ] Development guidelines documented

---

## **EMERGENCY PROCEDURES**

### 🚨 **If Tests Start Failing During Development:**
1. **STOP** - Don't add more features
2. **Isolate** - Identify which change broke tests  
3. **Revert** - Consider reverting breaking changes
4. **Fix** - Address root cause before proceeding
5. **Validate** - Ensure fix doesn't break other tests

### 🚨 **If Success Rate Drops Below 60%:**
1. **Code Freeze** - No new features until stabilized
2. **Emergency Review** - Team review of all recent changes
3. **Systematic Fix** - Address failures one by one
4. **Root Cause Analysis** - Understand why rate dropped
5. **Prevention** - Improve processes to prevent recurrence

---

## **SUCCESS METRICS**

### 📈 **Short-term (1-2 weeks):**
- Flaky tests eliminated  
- Success rate stable at 72%+
- Performance baselines established
- Basic monitoring in place

### 📈 **Medium-term (1 month):**
- Enhanced validations on 80% of passing tests
- Critical path coverage mapped and tested
- Automated regression detection working
- Test maintenance routine established

### 📈 **Long-term (3 months):**
- Success rate improved to 85%+
- All engineering validations comprehensive
- Full CI/CD pipeline with test gates
- Zero production regressions from inadequate testing

---

*Run this checklist BEFORE starting any new feature development to ensure a solid testing foundation.*