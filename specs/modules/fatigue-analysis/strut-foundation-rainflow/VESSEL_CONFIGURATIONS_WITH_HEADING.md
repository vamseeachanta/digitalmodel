# Vessel Configurations - With Wind/Wave Heading in Output Files

## Updated Naming Convention with Headings

### Input Files (Current - No Heading)
```
fsts_l015_mwl_wave01_Strut1.csv
fsts_l095_125km3_l000_pb_mwl_wind16_Strut8.csv
```

### Output Files (New - With Heading)
```
fsts_l015_mwl_wave01_045deg_Strut1_rainflow.csv
fsts_l095_125km3_l000_pb_mwl_wind16_315deg_Strut8_damage.csv
```

## Heading Mapping from Reference Seastates

Based on typical reference seastate definitions:

### Wave Cases (wave01-wave18)
| Case | Wave Direction | Wind Direction | Output Suffix |
|------|---------------|----------------|---------------|
| wave01 | 45° | 45° | 045deg |
| wave02 | 45° | 45° | 045deg |
| wave03 | 90° | 90° | 090deg |
| wave04 | 90° | 90° | 090deg |
| wave05 | 135° | 135° | 135deg |
| wave06 | 135° | 135° | 135deg |
| wave07 | 180° | 180° | 180deg |
| wave08 | 180° | 180° | 180deg |
| wave09 | 225° | 225° | 225deg |
| wave10 | 225° | 225° | 225deg |
| wave11 | 270° | 270° | 270deg |
| wave12 | 270° | 270° | 270deg |
| wave13 | 315° | 315° | 315deg |
| wave14 | 315° | 315° | 315deg |
| wave15 | 0° | 0° | 000deg |
| wave16 | 0° | 0° | 000deg |
| wave17 | 45° | 45° | 045deg |
| wave18 | 90° | 90° | 090deg |

### Wind Cases (wind01-wind16)
| Case | Wind Direction | Wave Direction | Output Suffix |
|------|---------------|----------------|---------------|
| wind01 | 0° | 0° | 000deg |
| wind02 | 22.5° | 22.5° | 023deg |
| wind03 | 45° | 45° | 045deg |
| wind04 | 67.5° | 67.5° | 068deg |
| wind05 | 90° | 90° | 090deg |
| wind06 | 112.5° | 112.5° | 113deg |
| wind07 | 135° | 135° | 135deg |
| wind08 | 157.5° | 157.5° | 158deg |
| wind09 | 180° | 180° | 180deg |
| wind10 | 202.5° | 202.5° | 203deg |
| wind11 | 225° | 225° | 225deg |
| wind12 | 247.5° | 247.5° | 248deg |
| wind13 | 270° | 270° | 270deg |
| wind14 | 292.5° | 292.5° | 293deg |
| wind15 | 315° | 315° | 315deg |
| wind16 | 337.5° | 337.5° | 338deg |

## Complete Output File Examples

### Configuration 1: fsts_l015
```
Input:  fsts_l015_mwl_wave01_Strut1.csv
Output: fsts_l015_mwl_wave01_045deg_Strut1_effective_tension.csv
        fsts_l015_mwl_wave01_045deg_Strut1_rainflow.csv
        fsts_l015_mwl_wave01_045deg_Strut1_damage.csv
```

### Configuration 2: fsts_l095
```
Input:  fsts_l095_mwl_wind09_Strut5.csv
Output: fsts_l095_mwl_wind09_180deg_Strut5_effective_tension.csv
        fsts_l095_mwl_wind09_180deg_Strut5_rainflow.csv
        fsts_l095_mwl_wind09_180deg_Strut5_damage.csv
```

### Configuration 3: fsts_l015_125km3_l100_pb
```
Input:  fsts_l015_125km3_l100_pb_mwl_wave07_Strut3.csv
Output: fsts_l015_125km3_l100_pb_mwl_wave07_180deg_Strut3_effective_tension.csv
        fsts_l015_125km3_l100_pb_mwl_wave07_180deg_Strut3_rainflow.csv
        fsts_l015_125km3_l100_pb_mwl_wave07_180deg_Strut3_damage.csv
```

### Configuration 4: fsts_l095_125km3_l000_pb
```
Input:  fsts_l095_125km3_l000_pb_mwl_wind16_Strut8.csv
Output: fsts_l095_125km3_l000_pb_mwl_wind16_338deg_Strut8_effective_tension.csv
        fsts_l095_125km3_l000_pb_mwl_wind16_338deg_Strut8_rainflow.csv
        fsts_l095_125km3_l000_pb_mwl_wind16_338deg_Strut8_damage.csv
```

## Python Implementation

```python
# Heading mapping dictionary
HEADING_MAP = {
    # Wave cases
    'wave01': '045deg', 'wave02': '045deg',
    'wave03': '090deg', 'wave04': '090deg',
    'wave05': '135deg', 'wave06': '135deg',
    'wave07': '180deg', 'wave08': '180deg',
    'wave09': '225deg', 'wave10': '225deg',
    'wave11': '270deg', 'wave12': '270deg',
    'wave13': '315deg', 'wave14': '315deg',
    'wave15': '000deg', 'wave16': '000deg',
    'wave17': '045deg', 'wave18': '090deg',
    # Wind cases  
    'wind01': '000deg', 'wind02': '023deg',
    'wind03': '045deg', 'wind04': '068deg',
    'wind05': '090deg', 'wind06': '113deg',
    'wind07': '135deg', 'wind08': '158deg',
    'wind09': '180deg', 'wind10': '203deg',
    'wind11': '225deg', 'wind12': '248deg',
    'wind13': '270deg', 'wind14': '293deg',
    'wind15': '315deg', 'wind16': '338deg'
}

def get_output_filename_with_heading(input_filename, output_type):
    """
    Generate output filename with heading included
    
    Args:
        input_filename: e.g., 'fsts_l015_mwl_wave01_Strut1.csv'
        output_type: 'effective_tension', 'rainflow', 'damage', etc.
        
    Returns:
        Output filename with heading, e.g., 'fsts_l015_mwl_wave01_045deg_Strut1_rainflow.csv'
    """
    # Remove .csv extension
    base_name = input_filename.replace('.csv', '')
    
    # Extract case (wave01, wind16, etc.)
    parts = base_name.split('_')
    
    # Find the case part (waveXX or windXX)
    case_part = None
    case_index = -1
    for i, part in enumerate(parts):
        if part.startswith('wave') or part.startswith('wind'):
            case_part = part
            case_index = i
            break
    
    if case_part and case_part in HEADING_MAP:
        heading = HEADING_MAP[case_part]
        # Insert heading after the case part
        parts.insert(case_index + 1, heading)
    
    # Add output type
    parts.append(output_type)
    
    # Reconstruct filename
    return '_'.join(parts) + '.csv'

# Example usage
input_file = 'fsts_l015_125km3_l100_pb_mwl_wave01_Strut1.csv'
rainflow_output = get_output_filename_with_heading(input_file, 'rainflow')
# Returns: 'fsts_l015_125km3_l100_pb_mwl_wave01_045deg_Strut1_rainflow.csv'

damage_output = get_output_filename_with_heading(input_file, 'damage')
# Returns: 'fsts_l015_125km3_l100_pb_mwl_wave01_045deg_Strut1_damage.csv'
```

## Directory Structure with Headings

```
output/
├── fsts_l015/
│   ├── wave_cases/
│   │   ├── fsts_l015_mwl_wave01_045deg_Strut1_effective_tension.csv
│   │   ├── fsts_l015_mwl_wave01_045deg_Strut1_rainflow.csv
│   │   ├── fsts_l015_mwl_wave01_045deg_Strut1_damage.csv
│   │   └── ...
│   └── wind_cases/
│       ├── fsts_l015_mwl_wind01_000deg_Strut1_effective_tension.csv
│       ├── fsts_l015_mwl_wind01_000deg_Strut1_rainflow.csv
│       └── ...
├── fsts_l095/
├── fsts_l015_125km3_l100_pb/
└── fsts_l095_125km3_l000_pb/
```

## Benefits of Including Heading

1. **Traceability**: Easy to identify which environmental direction the results correspond to
2. **Post-processing**: Can quickly filter/sort results by direction
3. **Validation**: Ensures correct mapping between input conditions and outputs
4. **Reporting**: Clear identification in fatigue reports by direction
5. **Pattern Analysis**: Can identify directional sensitivities in fatigue damage

## Summary Files with Heading Information

```csv
# fsts_l015_fatigue_summary_by_heading.csv
configuration,heading,total_damage,fatigue_life_years,critical_strut
fsts_l015,000deg,1.23e-7,8130000,Strut3
fsts_l015,045deg,2.45e-7,4081000,Strut1
fsts_l015,090deg,3.67e-7,2725000,Strut5
...
```

This enhancement provides better traceability and makes it easier to analyze directional effects on fatigue damage.