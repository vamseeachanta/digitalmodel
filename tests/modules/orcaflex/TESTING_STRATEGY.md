# OrcaFlex Module Testing Strategy

## Overview

The OrcaFlex module implements a **dual-mode testing strategy** that automatically adapts based on license availability:

1. **When OrcaFlex license IS available**: Tests run against BOTH real OrcaFlex and mock implementations
2. **When OrcaFlex license is NOT available**: Tests run against mock implementation only
3. **CI/CD environments**: Can force mock-only mode for consistency

## Testing Modes

### 1. Auto Mode (Default)
```bash
python tests/modules/orcaflex/run_tests.py
```
- Automatically detects license availability
- With license: Tests both real and mock
- Without license: Tests mock only

### 2. Mock Mode (CI/CD)
```bash
python tests/modules/orcaflex/run_tests.py --mock
# OR
ORCAFLEX_FORCE_MOCK=1 pytest tests/modules/orcaflex
```
- Forces all tests to use mock implementation
- Ensures consistent behavior in CI/CD
- Works without OrcaFlex installation

### 3. Real Mode (License Required)
```bash
python tests/modules/orcaflex/run_tests.py --real
```
- Forces all tests to use real OrcaFlex
- Requires valid OrcaFlex license
- Validates actual OrcaFlex behavior

### 4. Dual Mode (Maximum Coverage)
```bash
python tests/modules/orcaflex/run_tests.py --both
```
- Runs tests in both real and mock modes
- Compares behavior between implementations
- Requires license for real mode tests

## Implementation Details

### How It Works

1. **Automatic Detection**
   ```python
   def check_orcaflex_available():
       status = {'has_module': False, 'has_license': False}
       try:
           import OrcFxAPI
           status['has_module'] = True
           try:
               model = OrcFxAPI.Model()
               status['has_license'] = True
           except:
               pass  # Module exists but no license
       except ImportError:
           pass  # Module not installed
       return status
   ```

2. **Mode Selection Logic**
   ```python
   # In OrcaFlexModelWrapper.__init__
   if force_mock or skip_real:
       self.use_mock = True
   else:
       self.use_mock = use_mock or not ORCFXAPI_AVAILABLE
   ```

3. **Test Markers**
   ```python
   @pytest.mark.requires_orcaflex  # Skip if no license
   @pytest.mark.requires_no_orcaflex  # Skip if license available
   @pytest.mark.dual_mode  # Run in both modes
   ```

## Test Categories

### 1. Mock-Only Tests
- Always run regardless of license
- Test mock implementation behavior
- Ensure fallback works correctly

### 2. Real-Only Tests
- Run only with valid license
- Test OrcaFlex-specific features
- Validate actual OrcaFlex behavior

### 3. Dual-Mode Tests
- Run in both modes when possible
- Compare implementations
- Ensure consistency

### 4. Adaptive Tests
- Automatically adjust based on environment
- Provide maximum coverage available
- Report what was tested

## Environment Variables

| Variable | Values | Description |
|----------|--------|-------------|
| `ORCAFLEX_FORCE_MOCK` | 1, true, yes | Force mock mode even with license |
| `ORCAFLEX_SKIP_REAL` | 1, true, yes | Skip real OrcaFlex tests |
| `ORCAFLEX_PERF_LOG` | 1, true, yes | Enable performance logging |

## Coverage Report

### With License Available
```
Test Coverage Report
====================
Environment: WITH LICENSE
OrcFxAPI Module: ✅ Available
OrcaFlex License: ✅ Valid

Coverage by Mode:
- Mock Mode Tests: 100%
- Real Mode Tests: 100%
- Dual Mode Tests: Both tested

✅ FULL COVERAGE: Testing both real and mock
```

### Without License
```
Test Coverage Report
====================
Environment: NO LICENSE
OrcFxAPI Module: ❌ Not available
OrcaFlex License: ❌ Invalid

Coverage by Mode:
- Mock Mode Tests: 100%
- Real Mode Tests: Skipped
- Dual Mode Tests: Mock only

⚠️ PARTIAL COVERAGE: Testing mock only
```

## CI/CD Configuration

### GitHub Actions
```yaml
- name: Run OrcaFlex Tests (Mock)
  env:
    ORCAFLEX_FORCE_MOCK: "1"
  run: |
    python -m pytest tests/modules/orcaflex -v
```

### Jenkins
```groovy
stage('Test OrcaFlex Module') {
    environment {
        ORCAFLEX_FORCE_MOCK = '1'
    }
    steps {
        sh 'python tests/modules/orcaflex/run_tests.py --mock --coverage'
    }
}
```

### Local Development
```bash
# Check environment
python tests/modules/orcaflex/run_tests.py --check

# Run with auto-detection
python tests/modules/orcaflex/run_tests.py

# Force specific mode
python tests/modules/orcaflex/run_tests.py --mock  # CI/CD simulation
python tests/modules/orcaflex/run_tests.py --real  # Real only
python tests/modules/orcaflex/run_tests.py --both  # Maximum coverage
```

## Benefits

### 1. License Independence
- ✅ Tests run without OrcaFlex installation
- ✅ CI/CD doesn't need licenses
- ✅ Developers can work without license

### 2. Comprehensive Coverage
- ✅ Real implementation tested when available
- ✅ Mock implementation always tested
- ✅ Behavior comparison when both available

### 3. Flexibility
- ✅ Automatic mode selection
- ✅ Manual override options
- ✅ Environment variable control

### 4. Transparency
- ✅ Clear reporting of test mode
- ✅ Coverage varies by environment
- ✅ Recommendations provided

## Best Practices

1. **Local Development**
   - Use auto mode for convenience
   - Test both modes before commits
   - Validate real OrcaFlex if available

2. **CI/CD Pipeline**
   - Always use mock mode
   - Ensures consistent results
   - No license dependencies

3. **Release Testing**
   - Test with real OrcaFlex
   - Validate full functionality
   - Performance comparisons

4. **Test Writing**
   - Use appropriate markers
   - Consider both modes
   - Document mode requirements

## Performance Comparison

When both modes are available, the test suite provides performance metrics:

```
Performance Comparison
=====================
MOCK Mode: 0.150s average
REAL Mode: 1.250s average
Mock is 8.3x faster than real
```

This helps validate that mock mode provides reasonable performance characteristics.

## Troubleshooting

### Issue: Tests fail with "No license"
**Solution**: Use mock mode or obtain license
```bash
python tests/modules/orcaflex/run_tests.py --mock
```

### Issue: Mock tests don't match real behavior
**Solution**: Update mock implementation
```python
# In MockOrcaFlexModel class
def CalculateStatics(self):
    # Update to match real behavior
    time.sleep(0.1)  # Simulate processing
```

### Issue: CI/CD tests inconsistent
**Solution**: Force mock mode
```yaml
env:
  ORCAFLEX_FORCE_MOCK: "1"
```

## Summary

The dual-mode testing strategy ensures:
1. **Continuous testing** without license dependencies
2. **Full validation** when licenses are available
3. **Flexible control** for different environments
4. **Clear visibility** of what's being tested

This approach provides the best of both worlds: comprehensive testing when possible, and reliable fallback when not.