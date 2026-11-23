# RAO Parser Fix Summary

## Issue
RAO plots were only showing 2-3 data points instead of all available periods and headings.

## Root Causes Identified

### 1. Parser Only Used Last Section
**File**: `src/digitalmodel/modules/marine_analysis/parsers/aqwa_lis_parser.py`

The AQWA .LIS files contain multiple RAO sections (spanning multiple pages). The parser was only extracting data from the **last section** instead of merging all sections.

**Before**:
```python
# Use the last section (as per requirements)
last_section = sections[-1]
parsed_data = self._parse_rao_section(last_section)
```

**After**:
```python
# Parse ALL sections and merge data (CRITICAL FIX)
merged_data = {}
for section in sections:
    section_data = self._parse_rao_section(section)
    if section_data:
        for freq, freq_data in section_data.items():
            if freq not in merged_data:
                merged_data[freq] = {}
            merged_data[freq].update(freq_data)
```

### 2. Continuation Lines Not Handled
The parser didn't handle continuation lines where period/frequency columns are blank but heading is present.

**Format in .LIS file**:
```
   22.22   0.283   -180.00    0.9173  -90.21  ... [first heading with period/freq]
                   -135.00    0.6722  -90.15  ... [continuation line - no period/freq]
                    -90.00    0.0002 -179.14  ... [continuation line - no period/freq]
```

**Fix Added**:
```python
elif not period_str and not freq_str and direction_str:
    # Continuation line: no period/freq, but has heading
    if current_freq is None:
        continue

    direction = float(direction_str)
    dof_values = self._extract_dof_values(line, 27)

    if dof_values:
        if current_freq not in rao_data:
            rao_data[current_freq] = {}
        rao_data[current_freq][direction] = dof_values
```

## Results

### Before Fix
- **Periods**: 2 periods (only from last section)
- **Headings**: 1 heading (-180° only)
- **Data points**: 2 × 1 = 2 total

### After Fix
- **Periods**: 14 periods (4.0s to 22.2s)
- **Headings**: 9 headings (-180° to 180° in 45° increments)
- **Data points**: 14 × 9 = 126 total ✅

## Files Modified

1. **Parser**: `src/digitalmodel/modules/marine_analysis/parsers/aqwa_lis_parser.py`
   - Fixed `_extract_displacement_raos()` to merge all sections
   - Fixed `_extract_velocity_raos()` to merge all sections
   - Fixed `_extract_acceleration_raos()` to merge all sections
   - Fixed `_parse_rao_section()` to handle continuation lines

2. **Test Script**: `scripts/fix_rao_plots_standalone.py`
   - Standalone script that generates plots without stdout issues

## Plots Generated

All plots now show complete data:

1. `rao_amplitude_all_dof.png` - Amplitude vs Period for all 6 DOF
2. `rao_phase_all_dof.png` - Phase vs Period for all 6 DOF
3. `rao_heading_comparison.png` - Key headings comparison
4. `rao_translation_rotation.png` - Translation vs Rotation DOF

**Location**: `tests/marine_engineering/plots/rao_qa/`

## Impact

This fix ensures accurate RAO data extraction for:
- Ship motion analysis
- Hydrodynamic response calculations
- Mooring design validation
- Wave loading assessments

The complete dataset is now available for all engineering analyses.

## Testing

Run the fixed plotter:
```bash
cd D:/workspace-hub/digitalmodel
python scripts/fix_rao_plots_standalone.py
```

Expected output:
- 14 periods from 4.0s to 22.2s
- 9 headings from -180° to 180°
- Complete RAO curves for all 6 DOF
