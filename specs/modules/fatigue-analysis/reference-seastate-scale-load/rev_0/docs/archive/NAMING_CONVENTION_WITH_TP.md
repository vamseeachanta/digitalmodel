# Updated Naming Convention with Tp (Wave Period)

## Issue
The 81 fatigue conditions have Tp values ranging from 0.2s to 4.3s, but reference seastates only have 3 Tp values:
- 1.93s (covers fatigue Tp 0.2-2.3s)
- 2.70s (covers fatigue Tp 2.4-3.0s)  
- 3.47s (covers fatigue Tp 3.1-4.3s)

## Solution: Include Tp in Output Filenames

### Input Files (Existing - No Tp Info)
```
fsts_l015_mwl_wave01_Strut1.csv    # Has Tp=1.93s but not shown in name
fsts_l015_mwl_wave02_Strut1.csv    # Has Tp=2.70s but not shown in name
fsts_l015_mwl_wave03_Strut1.csv    # Has Tp=3.47s but not shown in name
```

### Output Files (NEW - With Heading AND Tp)
```
# Format: {config}_mwl_{case}_{heading}_tp{Tp}s_Strut{N}_{output_type}.csv

fsts_l015_mwl_wave01_045deg_tp193s_Strut1_effective_tension.csv
fsts_l015_mwl_wave02_045deg_tp270s_Strut1_rainflow.csv
fsts_l015_mwl_wave03_045deg_tp347s_Strut1_damage.csv
```

## Tp Mapping Table

| Reference Case | Direction | Tp (s) | Tp String | Use for Fatigue Tp Range |
|---------------|-----------|--------|-----------|--------------------------|
| wave01 | 45° | 1.93 | tp193s | 0.2 - 2.3s |
| wave02 | 45° | 2.70 | tp270s | 2.4 - 3.0s |
| wave03 | 45° | 3.47 | tp347s | 3.1 - 4.3s |
| wave04 | 70° | 1.93 | tp193s | 0.2 - 2.3s |
| wave05 | 70° | 2.70 | tp270s | 2.4 - 3.0s |
| wave06 | 70° | 3.47 | tp347s | 3.1 - 4.3s |
| ... | ... | ... | ... | ... |

## Python Implementation

```python
# Enhanced heading and Tp mapping
WAVE_REFERENCE_MAP = {
    'wave01': {'direction': '045deg', 'tp': 1.93, 'tp_str': 'tp193s'},
    'wave02': {'direction': '045deg', 'tp': 2.70, 'tp_str': 'tp270s'},
    'wave03': {'direction': '045deg', 'tp': 3.47, 'tp_str': 'tp347s'},
    'wave04': {'direction': '070deg', 'tp': 1.93, 'tp_str': 'tp193s'},
    'wave05': {'direction': '070deg', 'tp': 2.70, 'tp_str': 'tp270s'},
    'wave06': {'direction': '070deg', 'tp': 3.47, 'tp_str': 'tp347s'},
    'wave07': {'direction': '090deg', 'tp': 1.93, 'tp_str': 'tp193s'},
    'wave08': {'direction': '090deg', 'tp': 2.70, 'tp_str': 'tp270s'},
    'wave09': {'direction': '090deg', 'tp': 3.47, 'tp_str': 'tp347s'},
    'wave10': {'direction': '125deg', 'tp': 1.93, 'tp_str': 'tp193s'},
    'wave11': {'direction': '125deg', 'tp': 2.70, 'tp_str': 'tp270s'},
    'wave12': {'direction': '125deg', 'tp': 3.47, 'tp_str': 'tp347s'},
    'wave13': {'direction': '150deg', 'tp': 1.93, 'tp_str': 'tp193s'},
    'wave14': {'direction': '150deg', 'tp': 2.70, 'tp_str': 'tp270s'},
    'wave15': {'direction': '150deg', 'tp': 3.47, 'tp_str': 'tp347s'},
    'wave16': {'direction': '310deg', 'tp': 1.93, 'tp_str': 'tp193s'},
    'wave17': {'direction': '310deg', 'tp': 2.70, 'tp_str': 'tp270s'},
    'wave18': {'direction': '310deg', 'tp': 3.47, 'tp_str': 'tp347s'},
}

def find_closest_tp(target_tp):
    """Find closest reference Tp value"""
    ref_tps = [1.93, 2.70, 3.47]
    return min(ref_tps, key=lambda x: abs(x - target_tp))

def get_wave_reference_case(wave_direction, target_tp):
    """
    Find the appropriate wave reference case based on direction and Tp
    
    Args:
        wave_direction: Wave direction in degrees
        target_tp: Target Tp from fatigue condition
        
    Returns:
        Reference case name (e.g., 'wave14')
    """
    # Find closest Tp
    closest_tp = find_closest_tp(target_tp)
    
    # Find matching case
    for case_name, info in WAVE_REFERENCE_MAP.items():
        if info['direction'] == f"{int(wave_direction):03d}deg" and abs(info['tp'] - closest_tp) < 0.01:
            return case_name
    
    return None

def add_heading_and_tp_to_filename(input_filename, output_type='rainflow'):
    """
    Add heading and Tp information to output filename
    
    Example:
        Input:  fsts_l015_mwl_wave01_Strut1.csv
        Output: fsts_l015_mwl_wave01_045deg_tp193s_Strut1_rainflow.csv
    """
    import re
    
    # Remove .csv extension
    base = input_filename.replace('.csv', '')
    
    # Find the case type
    if 'wave' in base:
        pattern = r'(wave\d{2})'
        match = re.search(pattern, base)
        if match:
            case = match.group(1)
            if case in WAVE_REFERENCE_MAP:
                info = WAVE_REFERENCE_MAP[case]
                heading = info['direction']
                tp_str = info['tp_str']
                # Insert heading and tp after the case
                base = base.replace(case, f"{case}_{heading}_{tp_str}")
    elif 'wind' in base:
        # Wind cases only need heading, no Tp
        pattern = r'(wind\d{2})'
        match = re.search(pattern, base)
        if match:
            case = match.group(1)
            if case in HEADING_MAP:
                heading = HEADING_MAP[case]
                base = base.replace(case, f"{case}_{heading}")
    
    # Add output type
    return f"{base}_{output_type}.csv"
```

## Example Workflow for Fatigue Condition Processing

```python
def process_fatigue_condition(fatigue_row):
    """
    Process a single fatigue condition (one of 81)
    """
    # Extract parameters
    wind_speed = fatigue_row['Wind Speed (m/s)']
    wind_dir = fatigue_row['Wind Dir (°)']
    wave_hs = fatigue_row['Hs (m)']
    wave_dir = fatigue_row['Wave Dir (°)']
    wave_tp = fatigue_row['Tp (s)']
    
    # Find reference cases
    wind_ref_case = get_wind_reference_case(wind_dir)  # e.g., 'wind09' for 180°
    wave_ref_case = get_wave_reference_case(wave_dir, wave_tp)  # e.g., 'wave14' for 150°, Tp~2.7
    
    # Build filenames
    wind_file = f"{config}_mwl_{wind_ref_case}_Strut{strut}.csv"
    wave_file = f"{config}_mwl_{wave_ref_case}_Strut{strut}.csv"
    
    # Create output filename with full info
    output_file = f"{config}_mwl_FC{row:03d}_{int(wind_dir):03d}deg_{int(wave_dir):03d}deg_tp{int(wave_tp*100):03d}s_Strut{strut}_combined.csv"
    
    return wind_file, wave_file, output_file
```

## Benefits of Including Tp

1. **Complete Traceability**: Know exactly which reference Tp was used
2. **Validation**: Easy to verify correct Tp mapping
3. **Analysis**: Can analyze sensitivity to Tp selection
4. **Documentation**: Self-documenting filenames
5. **Debugging**: Clear which approximations were made (e.g., using Tp=1.93s for a 0.5s fatigue case)

## Summary Files with Tp Information

```csv
# fatigue_mapping_summary.csv
fatigue_condition,wind_dir,wind_ref,wave_dir,wave_tp_target,wave_tp_used,wave_ref
FC001,180,wind09,150,2.7,2.70,wave14
FC002,180,wind09,150,1.7,1.93,wave13
FC003,180,wind09,150,2.3,1.93,wave13
FC004,180,wind09,150,1.0,1.93,wave13
...
```

This enhanced naming convention ensures complete traceability and makes it easy to verify that the correct reference cases are being used for each fatigue condition.