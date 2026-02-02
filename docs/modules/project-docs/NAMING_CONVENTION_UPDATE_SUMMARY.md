# Production Code Update Summary: New SS Naming Convention

## Overview
The production code in the digitalmodel repository has been successfully updated to support the new SS naming convention while maintaining full backward compatibility with existing files.

## Updated Files

### 1. `src/digitalmodel/modules/fatigue_analysis/strut_foundation_processor.py`

#### Key Changes:
- **Added `SeaState` dataclass**: New data structure for SS001-SS004 sea states
- **Updated `parse_reference_name()`**: Now recognizes both:
  - New format: `REF_WIND01`, `REF_WAVE01`
  - Legacy format: `wind01`, `wave01`
- **Updated `get_reference_files()`**: Detects both naming patterns in file discovery
- **Enhanced `LoadScaler` class**:
  - Added `load_sea_states()` method
  - Added `create_sample_sea_states()` method with verification framework data
  - Added `process_sea_state()` method for new naming convention
  - Enhanced `select_closest_reference()` to prefer new format over legacy

#### New Features:
- Automatic preference for `REF_WIND01`/`REF_WAVE01` when available
- Graceful fallback to legacy `wind01`/`wave01` files
- Support for SS001-SS004 sea state processing
- Proper scaling factor calculations based on verification framework

### 2. `src/digitalmodel/modules/fatigue_analysis/file_namer.py`

#### Key Changes:
- **Updated all filename methods** to accept both:
  - Legacy: integer condition IDs (generates `FC001`, `FC002`, etc.)
  - New: string condition IDs (accepts `SS001`, `SS002`, etc.)
- **Methods updated**:
  - `get_scaled_ref_filename()`
  - `get_combined_filename()`
  - `get_rainflow_filename()`
  - `get_damage_filename()`

#### Example Output Files:
```
# Legacy Format
fsts_l015_FC001_Strut1_combined.csv
fsts_l015_FC001_Strut1_rainflow.csv

# New Format  
fsts_l015_SS001_Strut1_combined.csv
fsts_l015_SS001_Strut1_rainflow.csv
```

### 3. `src/digitalmodel/modules/fatigue_analysis/integrated_processor_with_naming.py`

#### Key Changes:
- **Added support for SeaState processing**:
  - `process_single_sea_state()` method
  - `process_configuration_with_sea_states()` method
- **Enhanced `run_complete_analysis()`**:
  - Added `use_sea_states` parameter
  - Automatic method selection based on naming convention
- **Updated main function**:
  - Command line support for `--sea-states` flag
  - Automatic detection and processing of both formats

## New Naming Convention Support

### Reference Seastates (Input Files)
- **New Format**: `{config}_mwl_REF_WIND01_Strut{#}.csv`
- **Legacy Format**: `{config}_mwl_wind01_Strut{#}.csv`
- **Behavior**: Automatically prefers new format, falls back to legacy

### Sea States (Processing)
- **New Format**: SS001, SS002, SS003, SS004
- **Legacy Format**: FC001, FC002, FC003, etc.
- **Output Files**: Automatically use correct naming based on input format

### Verification Framework Integration
The code includes the exact sea states from the verification framework:

| Sea State | Wind Speed | Wave Height | Wind Scale | Wave Scale | Purpose |
|-----------|------------|-------------|------------|------------|---------|
| SS001     | 15 m/s     | 0.75m       | 2.25x      | 1.50x      | Test validation |
| SS002     | 10 m/s     | 0.50m       | 1.00x      | 1.00x      | Baseline check |
| SS003     | 5 m/s      | 0.25m       | 0.25x      | 0.50x      | Calm conditions |
| SS004     | 20 m/s     | 1.00m       | 4.00x      | 2.00x      | Severe conditions |

## Usage Examples

### Command Line Usage
```bash
# Use new SS naming convention
python integrated_processor_with_naming.py --sea-states

# Use legacy FC naming convention (default)
python integrated_processor_with_naming.py
```

### Programmatic Usage
```python
from digitalmodel.fatigue_analysis.strut_foundation_processor import (
    ProductionDataHandler, LoadScaler, SeaState
)

# Initialize with new naming
data_handler = ProductionDataHandler(base_path="data")
load_scaler = LoadScaler(data_handler)

# Access new sea states
sea_states = load_scaler.sea_states  # SS001-SS004

# Process with new naming
effective_tension, metadata = load_scaler.process_sea_state(
    config_name="fsts_l015",
    sea_state=sea_states[0],  # SS001
    strut_num=1
)
```

## Backward Compatibility

### ✅ Fully Supported
- Existing `wind01`/`wave01` reference files
- Legacy `FC###` condition numbering
- All existing file naming patterns
- Existing analysis workflows

### 🔄 Automatic Detection
- Code automatically detects file naming format
- Prefers new format when both are available
- Seamless transition without breaking changes

## Testing Verification

### Test Files
- `tests/naming_convention_validation/test_updated_naming_convention.py`
- `tests/naming_convention_validation/test_integrated_processor_naming.py`

### Test Coverage
- ✅ Reference name parsing (both formats)
- ✅ SeaState dataclass functionality  
- ✅ LoadScaler new methods
- ✅ File naming compatibility
- ✅ Scaling factor calculations
- ✅ Integrated processor functionality

### Running Tests
```bash
# Test core naming convention functionality
python tests/naming_convention_validation/test_updated_naming_convention.py

# Test integrated processor functionality  
python tests/naming_convention_validation/test_integrated_processor_naming.py
```

### Test Results
```
✓ ALL TESTS PASSED - Updated code supports new SS naming convention
✓ Backward compatibility with legacy wind01/wave01 and FC### maintained  
✓ Production code ready for REF_WIND01/REF_WAVE01 and SS001-SS004
```

## Migration Path

### For New Projects
1. Use `REF_WIND01`/`REF_WAVE01` reference files
2. Use `--sea-states` flag or `use_sea_states=True`
3. Output files will use SS001-SS004 naming

### For Existing Projects  
1. No changes required - existing code continues to work
2. Can gradually migrate to new naming when ready
3. Both formats can coexist in the same analysis

## File Structure Impact

### Input Directory Structure
```
data/
├── fsts_l015_mwl_REF_WIND01_Strut1.csv  # New format (preferred)
├── fsts_l015_mwl_REF_WAVE01_Strut1.csv  # New format (preferred)  
├── fsts_l015_mwl_wind01_Strut1.csv      # Legacy format (fallback)
└── fsts_l015_mwl_wave01_Strut1.csv      # Legacy format (fallback)
```

### Output Directory Structure
```
output/
├── fsts_l015/
│   ├── combined_tensions/
│   │   ├── fsts_l015_SS001_Strut1_combined.csv  # New format
│   │   └── fsts_l015_FC001_Strut1_combined.csv  # Legacy format
│   ├── rainflow_results/
│   │   ├── fsts_l015_SS001_Strut1_rainflow.csv  # New format
│   │   └── fsts_l015_FC001_Strut1_rainflow.csv  # Legacy format
│   └── damage_results/
│       ├── fsts_l015_SS001_Strut1_damage.csv    # New format
│       └── fsts_l015_FC001_Strut1_damage.csv    # Legacy format
└── overall/
    └── overall_fatigue_summary.csv
```

## Summary

The production code has been successfully updated with:

1. **Full support** for the new SS naming convention (REF_WIND01/REF_WAVE01, SS001-SS004)
2. **Complete backward compatibility** with legacy naming (wind01/wave01, FC###)
3. **Automatic format detection** and preference management
4. **Verified functionality** through comprehensive testing
5. **Seamless migration path** for existing and new projects

The implementation follows the verification framework specifications exactly and is ready for production use with both naming conventions.