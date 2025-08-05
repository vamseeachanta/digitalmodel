# OrcaFlex Missing Objects Troubleshooting Spec

## Overview
This spec provides a systematic approach to troubleshoot and fix "NoneType object has no attribute 'name'" errors when processing OrcaFlex models with missing or damaged objects.

## Problem Statement
- **Error Type**: `'NoneType' object has no attribute 'name'`
- **Context**: OrcaFlex post-processing fails when models have missing/damaged line objects
- **Impact**: Sequential processing stops on first error, preventing batch analysis

## Root Cause Analysis Steps

### 1. Initial Error Investigation
```bash
# Check error logs for patterns
find . -name "*.sim_error.log" | head -5
cat path/to/error.log
```

**Expected Patterns:**
- Different error types between model sets
- Consistent failures in specific directories
- References to object.type.name or object.name

### 2. Compare Model Sets
Create test configurations for single files from each problematic directory:

**Template: `test_single_[directory_name].yml`**
```yaml
meta:
  basename: orcaflex_post_process
  library: digitalmodel
  label: '[directory_name]_test_single'
  description: Test single file from [directory_name]

# [Include full configuration template]
```

### 3. Run Comparative Tests
```bash
# Test individual files to isolate differences
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel test_single_dir1.yml
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel test_single_dir2.yml
```

**Key Differences to Look For:**
- Object availability in models
- Configuration requirements
- Processing success/failure patterns

## Solution Implementation

### Code Locations to Fix
1. **`orcaflex_objects.py:get_model_objects()`** (Lines ~312-341)
2. **`orcaflex_objects.py:get_object_vars()`** (Lines ~361-378)
3. **`all_vars.py`** (Lines ~88-101)

### Error Handling Pattern
```python
# Template for safe object.type.name access
try:
    if object is not None and hasattr(object, 'type') and object.type is not None:
        if hasattr(object.type, 'name') and object.type.name is not None:
            # Process object.type.name
            object_type_name = object.type.name
        else:
            print(f"Warning: Object has invalid type.name")
            # Use fallback logic
    else:
        print(f"Warning: Object has invalid type")
        # Skip or use defaults
except Exception as e:
    print(f"Warning: Error accessing object properties: {str(e)}")
    # Handle gracefully
```

## Detailed Fix Implementation

### File 1: `orcaflex_objects.py:get_model_objects()`
**Problem**: Direct access to `object.type.name` without null checks
**Solution**: 
```python
# Replace list comprehensions with safe iteration
for object in objects:
    try:
        if object is not None and hasattr(object, 'type') and object.type is not None:
            # Safe processing
        else:
            print(f"Warning: Skipping invalid object: {object}")
            continue
    except Exception as e:
        print(f"Warning: Error processing object: {str(e)}")
        continue
```

### File 2: `orcaflex_objects.py:get_object_vars()`  
**Problem**: Unsafe access to `object.type.name` in VarNames logic
**Solution**: Add try-catch with fallback to `var_df["VarName"]`

### File 3: `all_vars.py`
**Problem**: Direct comparison `object.type.name == "Line"` 
**Solution**: Add safe type checking before comparison

## Testing Procedures

### 1. Create Test Configurations
- Single file tests for each problematic directory
- Configuration with minimal processing flags
- Configuration with full processing enabled

### 2. Test Scenarios
```bash
# Test 1: Minimal processing (should always pass)
python -m digitalmodel test_minimal_[dir].yml

# Test 2: Full processing with fixes
python -m digitalmodel test_full_[dir].yml

# Test 3: Batch processing
python -m digitalmodel full_batch_config.yml
```

### 3. Expected Results
- **Before Fix**: "NoneType" errors, processing stops
- **After Fix**: Warning messages, processing continues with valid objects

## Configuration Templates

### Minimal Test Configuration
```yaml
orcaflex:
  postprocess:
    visualization: {flag: false}
    summary: {flag: false}        # Disable to avoid object issues
    linked_statistics: {flag: false}
    RangeGraph: {flag: false}
    time_series: {flag: false}
```

### Production Configuration
```yaml
# Include all required sections:
# - parameters.VarNames
# - summary_settings.groups  
# - time_series_settings
# - parallel_processing.enabled: false (for sequential)
```

## Prevention Strategies

### 1. Model Validation
- Add object existence checks before processing
- Log available objects for debugging
- Validate model integrity before analysis

### 2. Configuration Validation  
- Verify all required configuration sections exist
- Use actual object names from models, not generic types
- Test configurations with minimal settings first

### 3. Error Handling Standards
- Always use safe object access patterns
- Provide meaningful warning messages
- Continue processing with valid objects when possible

## Troubleshooting Checklist

- [ ] Check error logs for "NoneType" patterns
- [ ] Create single-file test configurations  
- [ ] Compare working vs failing model sets
- [ ] Identify missing objects in problematic models
- [ ] Implement safe object access in identified code locations
- [ ] Test fix with minimal configuration
- [ ] Test fix with full processing configuration
- [ ] Verify batch processing works end-to-end
- [ ] Update production configurations

## Notes Section
**Date**: [YYYY-MM-DD]
**Issue Context**: [Brief description of specific case]
**Model Types**: [e.g., Tsunami vs Damaged scenarios]
**Key Findings**: 
- [Finding 1]
- [Finding 2]

**Additional Considerations**:
- [Any specific model characteristics]
- [Performance implications]
- [Future enhancements needed]

## File References
- **Error Logs**: `[path]/[filename].sim_error.log`
- **Test Configs**: `postproc/test_*.yml`
- **Fixed Code Files**: 
  - `orcaflex_objects.py`
  - `all_vars.py`
- **Production Configs**: `dm_fsts_*.yml`

## Related Issues
- [Link to similar issues]
- [Reference to documentation]
- [Contact information for domain experts]