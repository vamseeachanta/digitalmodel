# OrcaFlex Model Structure - Current Status

**Date:** 2025-11-12
**Status:** IN PROGRESS - Troubleshooting YAML array structure
**Progress:** 90% complete

## ✅ What Works

1. **File Organization** - Flattened single-directory structure
   - 12 model directories created (`flattened_models/NSE_CALM_001_*`)
   - Each directory contains ~24 files
   - No nested subdirectories
   - All files in same location

2. **Environment Files** - 108 files generated
   - Fixed Unicode issues (degree symbol → "deg")
   - Correct JONSWAP wave parameters
   - Current and wind profiles

3. **Section Headers** - Added to base files
   - `VariableData:`, `VesselTypes:`, `LineTypes:`, `Vessels:`, `Lines:`, `Groups:`
   - Proper indentation applied to all content

4. **Main Model Structure** - Matches reference pattern
   - Root-level includefiles only (no section headers in main file)
   - Wrapper files for General and Environment sections
   - Simple list of 8 includefiles

5. **Dependency Resolution** - VariableData before LineTypes
   - "Generic Drag" coefficient table defined first
   - Correct ordering prevents missing reference errors

## ❌ Current Issue

**YAML Array Structure Error**
```
Error: Did not find expected '-' indicator at Line 3, Column 5
File: _06_vessels_buoys.yml
```

**Root Cause:**
Base files have array format (`- Name:`) but indentation is incorrect after adding section headers.

**Current Structure (INCORRECT):**
```yaml
Vessels:
  - Name: Vessel
  VesselType: Vessel Type1    # ← Missing 2 spaces indent!
```

**Required Structure:**
```yaml
Vessels:
  - Name: Vessel
    VesselType: Vessel Type1  # ← Must be indented 4 spaces from root
```

Array properties must be indented 2 spaces beyond the `-` marker.

## 📋 Files Affected

All base files with array content:
- `_02_variable_data.yml` - Dragcoefficient array
- `_04_vessel_types.yml` - Vessel type definitions
- `_05_line_types.yml` - Line type definitions
- `_06_vessels_buoys.yml` - Vessel objects **← CURRENT BLOCKER**
- `_07_lines.yml` - Line objects
- `_08_groups.yml` - Group definitions

## 🔍 Alternative Solution: Use Reference Format

The reference models use **object format** instead of arrays:

**Reference Pattern (Object Format):**
```yaml
VesselTypes:
  includefile: vessel_type_def.yml

# In vessel_type_def.yml:
New: TypeName
TypeName:
  Length: 103
  properties...
```

**Our Format (Array Format):**
```yaml
VesselTypes:
  - Name: Vessel Type1
    Length: 103
    properties...
```

Both formats are valid in OrcaFlex, but:
- Object format is more flexible (easier to reference by name)
- Array format is more compact (all definitions in one file)

## 🎯 Next Steps (Choose One)

### Option A: Fix Array Indentation (Faster)
1. Create script to properly indent array items
2. Ensure properties are 4 spaces from root (2 beyond `-`)
3. Regenerate flattened models
4. Test

**Pros:** Keeps current base file structure
**Cons:** Complex indentation logic

### Option B: Convert to Object Format (More Robust)
1. Split each base file into individual object definitions
2. Use `New: TypeName` format like reference
3. Create wrapper files that includefile each definition
4. Regenerate flattened models
5. Test

**Pros:** Matches reference exactly, cleaner structure
**Cons:** More files to manage (103 files like reference)

### Option C: Use OrcaFlex GUI (Easiest)
1. Open one flattened model in OrcaFlex GUI
2. GUI will show exact errors
3. Fix in GUI and save
4. Use corrected files as new base
5. Regenerate all models from corrected base

**Pros:** OrcaFlex validates structure
**Cons:** Manual step required

## 📊 Current Metrics

- **Base files created:** 27
- **Environment files generated:** 108
- **Analysis models created:** 72 (12 directions × 3 return periods × 2 types)
- **Flattened models created:** 12 (1-year simple only)
- **Scripts developed:** 10
  - `generate_env_files.py`
  - `generate_analysis_models.py`
  - `create_flattened_models.py`
  - `add_section_headers.py`
  - `fix_indentation.py`
  - `add_headers_properly.py`
  - `test_flattened_model.py`
  - `run_1year_operability.py`
  - Plus testing scripts

## 💡 Recommendation

**Proceed with Option C (GUI Validation):**

1. Open `flattened_models/NSE_CALM_001_000deg_1yr_simple/NSE_CALM_001_000deg_1yr_simple.yml` in OrcaFlex GUI
2. OrcaFlex will report specific errors with line numbers
3. Fix structural issues in GUI
4. Save corrected model
5. Use GUI-validated files as new base
6. Regenerate all 72 models from corrected structure

This approach leverages OrcaFlex's built-in validation and ensures the structure is 100% correct before batch generation.

## 📝 Session Summary

**Time Spent:** ~3 hours
**Token Usage:** 121K / 200K
**Key Learnings:**
- OrcaFlex requires exact YAML structure matching
- Plural section names (`VesselTypes:` not `VesselType:`)
- Root-level includefiles work best
- Array indentation must be precise (properties indented beyond `-`)
- Reference pattern: all 103 files in one directory
- VariableData must come before types that reference it

**Status:** Ready for GUI validation or final indentation fix.
