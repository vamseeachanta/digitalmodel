# Production Data Structure Analysis

## Location
`D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\csv`

## Total Files
1,698 CSV files

## Vessel Configurations Identified

Based on file naming patterns, we have 4 distinct configurations:

### 1. FSTs Light (l015) - No LNGC
- **Pattern**: `fsts_l015_mwl_*`
- **Description**: FSTs at 15% loading, no LNGC present
- **Cases**: 
  - 18 wave cases (wave01-wave18)
  - 16 wind cases (wind01-wind16)

### 2. FSTs Full (l095) - No LNGC  
- **Pattern**: `fsts_l095_mwl_*`
- **Description**: FSTs at 95% loading, no LNGC present
- **Cases**:
  - 18 wave cases (wave01-wave18)
  - 16 wind cases (wind01-wind16)

### 3. FSTs Light (l015) + LNGC Full (125km3_l100)
- **Pattern**: `fsts_l015_125km3_l100_pb_mwl_*`
- **Description**: FSTs at 15% loading with 125k m³ LNGC at 100% (offloading)
- **Cases**:
  - 18 wave cases (wave01-wave18)
  - 16 wind cases (wind01-wind16)

### 4. FSTs Full (l095) + LNGC Light (125km3_l000)
- **Pattern**: `fsts_l095_125km3_l000_pb_mwl_*`
- **Description**: FSTs at 95% loading with 125k m³ LNGC at 0% (loading)
- **Cases**:
  - 18 wave cases (wave01-wave18)
  - 16 wind cases (wind01-wind16)

## File Naming Convention

### Full Pattern
`{fst_config}_{lngc_config}_{water_level}_{case_type}{case_num}_{component}.csv`

### Components
- `fst_config`: fsts_l015 or fsts_l095
- `lngc_config`: 125km3_l100, 125km3_l000, or absent
- `pb`: Present when LNGC is connected (passively ballasted)
- `water_level`: mwl (mean water level)
- `case_type`: wave or wind
- `case_num`: 01-18 for wave, 01-16 for wind
- `component`: Strut1-8 or Jacket1-4

### Example Files
```
fsts_l015_mwl_wave01_Strut1.csv              # Config 1: FST Light only
fsts_l095_mwl_wave01_Strut1.csv              # Config 2: FST Full only
fsts_l015_125km3_l100_pb_mwl_wave01_Strut1.csv  # Config 3: FST Light + LNGC Full
fsts_l095_125km3_l000_pb_mwl_wave01_Strut1.csv  # Config 4: FST Full + LNGC Light
```

## Data Structure per Configuration

Each configuration contains:
- **34 reference seastates** (18 wave + 16 wind)
- **8 struts** per seastate (Strut1-Strut8)
- **4 jacket points** per seastate (Jacket1-Jacket4)
- **Total per config**: 408 files (34 × 12 components)

## Complete Dataset Structure

| Configuration | FST Load | LNGC Load | Files | Pattern |
|--------------|----------|-----------|-------|---------|
| 1 | 15% (l015) | None | 408 | `fsts_l015_mwl_*` |
| 2 | 95% (l095) | None | 408 | `fsts_l095_mwl_*` |
| 3 | 15% (l015) | 100% (l100) | 408 | `fsts_l015_125km3_l100_pb_mwl_*` |
| 4 | 95% (l095) | 0% (l000) | 408 | `fsts_l095_125km3_l000_pb_mwl_*` |
| **Total** | - | - | **1,632** | - |

Note: The remaining 66 files are likely summary files (dm_fsts_* pattern).

## Key Observations

1. **Complete Dataset**: All 4 configurations are present with full reference seastates
2. **Consistent Structure**: Each configuration has identical case structure
3. **Passively Ballasted**: LNGC cases include "pb" indicating passive ballasting
4. **Mean Water Level**: All cases use "mwl" (mean water level)
5. **Components**: Both strut and jacket data available

## Required Updates to Specification

1. **Update file paths**: Point to production directory
2. **Update naming patterns**: Use actual file naming convention
3. **Include all 4 configurations**: Ensure all vessel combinations are processed
4. **Handle both struts and jackets**: Data includes both components
5. **Account for LNGC presence**: Different file patterns when LNGC is present/absent