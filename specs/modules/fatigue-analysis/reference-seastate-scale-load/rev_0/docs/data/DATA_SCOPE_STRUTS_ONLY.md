# Data Scope - Struts Only

## Fatigue Analysis Scope

**IMPORTANT**: Fatigue analysis will be performed ONLY on strut components (Strut1-Strut8).
Jacket files (Jacket1-Jacket4) will be ignored.

## Files to Process

### Per Configuration Per Seastate
- **Process**: Strut1.csv, Strut2.csv, ..., Strut8.csv (8 files)
- **Ignore**: Jacket1.csv, Jacket2.csv, Jacket3.csv, Jacket4.csv (4 files)

### Total File Count

| Configuration | Total Files Available | Files to Process | Files to Ignore |
|--------------|----------------------|------------------|-----------------|
| fsts_l015 | 408 | 272 (8 struts × 34 cases) | 136 (4 jackets × 34 cases) |
| fsts_l095 | 408 | 272 (8 struts × 34 cases) | 136 (4 jackets × 34 cases) |
| fsts_l015_125km3_l100_pb | 408 | 272 (8 struts × 34 cases) | 136 (4 jackets × 34 cases) |
| fsts_l095_125km3_l000_pb | 408 | 272 (8 struts × 34 cases) | 136 (4 jackets × 34 cases) |
| **TOTAL** | **1,632** | **1,088** | **544** |

## File Pattern Examples

### Files to Process ✅
```
fsts_l015_mwl_wave01_Strut1.csv  ✅
fsts_l015_mwl_wave01_Strut2.csv  ✅
fsts_l015_mwl_wave01_Strut3.csv  ✅
fsts_l015_mwl_wave01_Strut4.csv  ✅
fsts_l015_mwl_wave01_Strut5.csv  ✅
fsts_l015_mwl_wave01_Strut6.csv  ✅
fsts_l015_mwl_wave01_Strut7.csv  ✅
fsts_l015_mwl_wave01_Strut8.csv  ✅
```

### Files to Ignore ❌
```
fsts_l015_mwl_wave01_Jacket1.csv  ❌
fsts_l015_mwl_wave01_Jacket2.csv  ❌
fsts_l015_mwl_wave01_Jacket3.csv  ❌
fsts_l015_mwl_wave01_Jacket4.csv  ❌
```

## Python Implementation

```python
def get_strut_files_for_configuration(config_name, base_path):
    """
    Get all strut files for a given configuration
    
    Args:
        config_name: One of ['fsts_l015', 'fsts_l095', 
                            'fsts_l015_125km3_l100_pb', 
                            'fsts_l095_125km3_l000_pb']
        base_path: Production data directory
        
    Returns:
        List of strut file paths
    """
    import os
    from pathlib import Path
    
    strut_files = []
    prefix = CONFIGURATIONS[config_name]['file_prefix']
    
    # 18 wave cases + 16 wind cases = 34 total
    for case_type in ['wave', 'wind']:
        num_cases = 18 if case_type == 'wave' else 16
        
        for case_num in range(1, num_cases + 1):
            # Only process struts, not jackets
            for strut_num in range(1, 9):  # Strut1 to Strut8
                filename = f"{prefix}_{case_type}{case_num:02d}_Strut{strut_num}.csv"
                filepath = Path(base_path) / filename
                
                if filepath.exists():
                    strut_files.append(filepath)
                else:
                    print(f"Warning: Missing file {filename}")
    
    return strut_files

# File filter function
def is_strut_file(filename):
    """Check if file is a strut file (not jacket)"""
    return 'Strut' in filename and 'Jacket' not in filename

# Batch processing example
def process_all_struts(config_name, base_path):
    """Process all strut files for a configuration"""
    
    strut_files = get_strut_files_for_configuration(config_name, base_path)
    print(f"Configuration: {config_name}")
    print(f"Found {len(strut_files)} strut files to process")
    
    results = []
    for filepath in strut_files:
        if is_strut_file(str(filepath)):
            # Process only strut files
            result = process_single_strut(filepath)
            results.append(result)
    
    return results
```

## Directory Structure (Struts Only)

```
D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\csv\
├── Configuration: fsts_l015
│   ├── fsts_l015_mwl_wave01_Strut1.csv  ✅
│   ├── fsts_l015_mwl_wave01_Strut2.csv  ✅
│   ├── ...
│   ├── fsts_l015_mwl_wave01_Strut8.csv  ✅
│   ├── fsts_l015_mwl_wave01_Jacket1.csv ❌ (ignore)
│   └── ...
│
├── Configuration: fsts_l095
│   ├── fsts_l095_mwl_wave01_Strut1.csv  ✅
│   └── ...
│
├── Configuration: fsts_l015_125km3_l100_pb
│   ├── fsts_l015_125km3_l100_pb_mwl_wave01_Strut1.csv  ✅
│   └── ...
│
└── Configuration: fsts_l095_125km3_l000_pb
    ├── fsts_l095_125km3_l000_pb_mwl_wave01_Strut1.csv  ✅
    └── ...
```

## Output Structure (Struts Only)

```
output/
├── fsts_l015/
│   ├── struts/
│   │   ├── fsts_l015_mwl_wave01_045deg_Strut1_rainflow.csv
│   │   ├── fsts_l015_mwl_wave01_045deg_Strut1_damage.csv
│   │   ├── ...
│   │   └── fsts_l015_mwl_wind16_338deg_Strut8_damage.csv
│   └── summary/
│       ├── fsts_l015_strut_fatigue_summary.csv
│       └── fsts_l015_critical_strut_by_heading.csv
```

## Summary Report Structure

The fatigue summary will include only strut results:

```csv
# fsts_l015_strut_fatigue_summary.csv
configuration,strut_id,annual_damage,fatigue_life_years,design_factor,status
fsts_l015,Strut1,1.23e-7,8130000,325200,PASS
fsts_l015,Strut2,1.45e-7,6896000,275840,PASS
fsts_l015,Strut3,2.01e-7,4975000,199000,PASS
fsts_l015,Strut4,1.89e-7,5291000,211640,PASS
fsts_l015,Strut5,1.67e-7,5988000,239520,PASS
fsts_l015,Strut6,1.55e-7,6452000,258080,PASS
fsts_l015,Strut7,1.78e-7,5618000,224720,PASS
fsts_l015,Strut8,1.92e-7,5208000,208320,PASS
```

## Processing Time Estimate

With struts only:
- Files per configuration: 272 (vs 408 with jackets)
- Total files to process: 1,088 (vs 1,632 with jackets)
- **33% reduction in processing time**

## Key Benefits of Struts-Only Analysis

1. **Focused Analysis**: Struts are the critical fatigue components
2. **Reduced Processing**: 33% fewer files to process
3. **Cleaner Results**: No need to filter jacket results in reports
4. **Storage Savings**: Smaller output directories
5. **Faster Iterations**: Quicker to test and validate changes