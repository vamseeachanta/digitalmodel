# OrcaFlex Batch Execution - Troubleshooting Summary

**Date**: 2025-11-12
**Status**: In Progress
**OrcaFlex Version**: 11.5e

---

## Issues Found & Solutions

### ✅ Issue 1: Unicode Characters in Comments

**Problem**: Environment files contained degree symbols (°) that OrcaFlex couldn't parse.

**Error**:
```
Invalid leading UTF-8 octet at Line 1, Column 1
```

**Solution**: Regenerated all 108 environment files with "deg" instead of "°"
- Changed: `# Waves - 90° Direction`
- To: `# Waves - 90 deg Direction`

**Status**: **FIXED** - Files regenerated successfully

---

### ⚠️ Issue 2: Relative Path Resolution

**Problem**: Analysis models in `analysis_models/` use relative paths (`../base_files/...`) which OrcaFlex Python API may not resolve correctly.

**Error**:
```
Problem reading file: The file does not appear to be a valid OrcaFlex data file
```

**Root Cause**: When loading models via Python API, OrcaFlex's working directory for relative path resolution may differ from expectations.

**Tested Solutions**:

1. **Flattened Structure** (Partially Working):
   - Copy all base_files + env files to single directory
   - Use simple includefiles without `../` paths
   - Change to model directory before loading

   Result: Environment files load OK, but line type ordering issue remains

2. **Change Working Directory** (Recommended):
   ```python
   import os
   os.chdir('path/to/analysis_models')
   model = OrcFxAPI.Model('NSE_CALM_001_000deg_1yr_simple.yml')
   ```

3. **Absolute Paths** (Not Yet Tested):
   Convert relative to absolute paths before loading

**Status**: **IN PROGRESS** - Structure issue being resolved

---

### ⚠️ Issue 3: Line Type Definition Order

**Problem**: Lines reference line types that haven't been defined yet in the include order.

**Error**:
```
Failed to set LineType[1]=2.5" Chain (Invalid value)
```

**Root Cause**: In the flattened model, `LineTypes` includefile must come BEFORE `Lines` includefile.

**Solution**: Ensure correct section order in main model file:
1. General
2. VariableData
3. Environment
4. **VesselTypes** (before Vessels)
5. **LineTypes** (before Lines)  ← MUST come before Lines
6. Vessels
7. Lines
8. Groups

**Status**: **IDENTIFIED** - Fix in progress

---

## Working Test Results

### ✅ What Works

1. **OrcaFlex Python API**: Confirmed working (version 11.5e)
2. **Empty Model Creation**: Basic API functionality confirmed
3. **Environment Files**: Clean ASCII files load successfully
4. **File Structure**: Modular includes concept works
5. **Flattened Directory**: Single-directory approach viable

### ⚠️ What Needs Fixing

1. **Line Type Ordering**: Ensure correct include sequence
2. **Full Model Test**: Complete end-to-end test pending
3. **Batch Execution**: Needs working single model first

---

## Recommended Solutions

### Solution A: Use OrcaFlex GUI for Validation (FASTEST)

**Steps**:
1. Open OrcaFlex GUI
2. File → Load → Select `analysis_models/NSE_CALM_001_000deg_1yr_simple.yml`
3. If it loads in GUI, the structure is correct
4. Run statics/dynamics in GUI to verify
5. Then troubleshoot Python API path resolution separately

**Advantages**:
- Validates model structure immediately
- Separates structure issues from API issues
- Confirms all 72 models are valid

**Time**: 5-10 minutes per model

---

### Solution B: Fix Flattened Structure (IN PROGRESS)

**Steps**:
1. ✅ Create single directory with all files
2. ✅ Fix Unicode characters in comments
3. ⚠️ Fix line type ordering in main model
4. ⏳ Test complete model load
5. ⏳ Extend to batch processing

**Status**: 75% complete

**Remaining Work**:
- Verify line type inclusion order
- Test statics calculation
- Test dynamics simulation

---

### Solution C: Use Absolute Paths

**Steps**:
1. Create script to convert relative to absolute paths
2. Generate temporary models with absolute paths
3. Load via Python API
4. Run batch analysis

**Script Example**:
```python
import re
from pathlib import Path

def convert_to_absolute(model_file, base_dir):
    content = model_file.read_text()
    content = re.sub(
        r'includefile: \.\./base_files/',
        f'includefile: {str(base_dir / "base_files")}/'.replace('\\', '/'),
        content
    )
    temp_file = model_file.parent / f"_abs_{model_file.name}"
    temp_file.write_text(content)
    return temp_file
```

---

## Current Test Setup

### Test Files Created

1. **`test_single_model.py`** - Single model test with flattened structure
   - Creates `test_flat_model/` directory
   - Copies all base files and env files
   - Generates main model with flat includes
   - Status: Loads environment OK, line type order issue

2. **`run_1year_operability.py`** - Batch execution script (ready)
   - Finds all 12 × 1-year models
   - Runs statics and dynamics
   - Saves results and generates report
   - Status: Waiting for working single model

3. **`BATCH_EXECUTION_GUIDE.md`** - Complete user documentation
   - Usage instructions
   - Troubleshooting guide
   - Multiple solution approaches

### Test Directory Structure

```
orcaflex/
├── test_flat_model/               # Flattened test (in progress)
│   ├── _01a_units_analysis.yml
│   ├── _05_line_types.yml
│   ├── _07_lines.yml
│   ├── waves_000deg_1yr.yml
│   ├── ... (all base files + env)
│   └── calm_buoy_000deg_1yr_flat.yml  # Main model
│
├── analysis_models/                # Original structure
│   └── NSE_CALM_001_*deg_1yr_simple.yml (72 files)
│
└── base_files/                     # Modular base
    ├── env/ (109 files)
    └── *.yml (24 files)
```

---

## Next Steps

### Immediate (Complete Single Model Test)

1. **Fix line type ordering** in `test_single_model.py`:
   ```python
   # Ensure this order:
   LineTypes:
     - includefile: _05_line_types.yml
   Lines:
     - includefile: _07_lines.yml
   ```

2. **Test complete load and statics**:
   ```bash
   python test_single_model.py
   ```

3. **If successful**, verify dynamics:
   ```python
   model.RunSimulation()
   ```

### Short Term (Validate All Models)

1. **Test in OrcaFlex GUI** (parallel approach):
   - Load each analysis model in GUI
   - Verify structure is correct
   - Confirm relative paths work in GUI

2. **Once GUI confirms models work**:
   - Troubleshoot Python API path resolution
   - Use working directory changes
   - Or use absolute path conversion

### Medium Term (Batch Execution)

1. **Update `run_1year_operability.py`**:
   - Add working directory change: `os.chdir(model_path.parent)`
   - Or implement absolute path conversion
   - Test with 2-3 models first

2. **Run full batch**:
   ```bash
   python run_1year_operability.py
   ```

3. **Verify results**:
   - Check all .sim files created
   - Review JSON report
   - Validate timing statistics

---

## Files Generated/Modified

| File | Purpose | Status |
|------|---------|--------|
| `scripts/generate_env_files.py` | Generate clean env files | ✅ Updated |
| `base_files/env/*.yml` | 108 environment files | ✅ Regenerated |
| `test_single_model.py` | Single model test | ⚠️ In progress |
| `run_1year_operability.py` | Batch execution | ✅ Ready |
| `BATCH_EXECUTION_GUIDE.md` | User documentation | ✅ Complete |
| `TROUBLESHOOTING_SUMMARY.md` | This file | ✅ Current |

---

## Key Learnings

1. **OrcaFlex is Sensitive to File Encoding**:
   - Use plain ASCII in comments
   - Avoid Unicode symbols (°, ×, etc.)
   - CRLF line endings are OK

2. **Include Order Matters**:
   - Types must be defined before objects
   - LineTypes before Lines
   - VesselTypes before Vessels

3. **Relative Paths Need Care**:
   - Working directory affects path resolution
   - GUI handles paths differently than API
   - Flattened structure is more reliable

4. **Reference Structure Works**:
   - `tests/domains/orcaflex/analysis/moorings/pretension/`
   - Uses single directory
   - Simple includefiles without ../

---

## Support & References

- **OrcaFlex Manual**: Section 5.2 (Model Data Files)
- **Python API Guide**: Section 2.1 (Loading Models)
- **Working Example**: `tests/domains/orcaflex/analysis/moorings/pretension/`
- **Project Docs**: `STRUCTURE_SUMMARY.md`, `MIGRATION_COMPLETE.md`

---

*Last updated: 2025-11-12*
*Status: Troubleshooting in progress*
*Next: Fix line type ordering and complete single model test*
