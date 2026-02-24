# Actual Heading Mapping from Reference Metadata

## Verified Heading Values from reference_seastate_timetrace_metadata.csv

### Wave Cases (18 cases - 6 directions × 3 Tp values)
| Case | Wave Direction | Wind Direction | Output Suffix |
|------|---------------|----------------|---------------|
| wave01 | 45° | 45° | 045deg |
| wave02 | 45° | 45° | 045deg |
| wave03 | 45° | 45° | 045deg |
| wave04 | 70° | 70° | 070deg |
| wave05 | 70° | 70° | 070deg |
| wave06 | 70° | 70° | 070deg |
| wave07 | 90° | 90° | 090deg |
| wave08 | 90° | 90° | 090deg |
| wave09 | 90° | 90° | 090deg |
| wave10 | 125° | 125° | 125deg |
| wave11 | 125° | 125° | 125deg |
| wave12 | 125° | 125° | 125deg |
| wave13 | 150° | 150° | 150deg |
| wave14 | 150° | 150° | 150deg |
| wave15 | 150° | 150° | 150deg |
| wave16 | 310° | 310° | 310deg |
| wave17 | 310° | 310° | 310deg |
| wave18 | 310° | 310° | 310deg |

### Wind Cases (16 cases - 16 compass directions)
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

## Python Implementation with Actual Headings

```python
# Actual heading mapping from reference metadata
HEADING_MAP = {
    # Wave cases - 6 directions with 3 Tp values each
    'wave01': '045deg', 'wave02': '045deg', 'wave03': '045deg',
    'wave04': '070deg', 'wave05': '070deg', 'wave06': '070deg',
    'wave07': '090deg', 'wave08': '090deg', 'wave09': '090deg',
    'wave10': '125deg', 'wave11': '125deg', 'wave12': '125deg',
    'wave13': '150deg', 'wave14': '150deg', 'wave15': '150deg',
    'wave16': '310deg', 'wave17': '310deg', 'wave18': '310deg',
    
    # Wind cases - 16 compass directions (22.5° increments)
    'wind01': '000deg', 'wind02': '023deg', 'wind03': '045deg', 'wind04': '068deg',
    'wind05': '090deg', 'wind06': '113deg', 'wind07': '135deg', 'wind08': '158deg',
    'wind09': '180deg', 'wind10': '203deg', 'wind11': '225deg', 'wind12': '248deg',
    'wind13': '270deg', 'wind14': '293deg', 'wind15': '315deg', 'wind16': '338deg'
}

def format_heading(degrees):
    """Format heading as 3-digit string with 'deg' suffix"""
    return f"{int(degrees):03d}deg"

def add_heading_to_filename(input_filename, output_type='rainflow'):
    """
    Add heading information to output filename
    
    Example:
        Input:  fsts_l015_mwl_wave01_Strut1.csv
        Output: fsts_l015_mwl_wave01_045deg_Strut1_rainflow.csv
    """
    import re
    
    # Remove .csv extension
    base = input_filename.replace('.csv', '')
    
    # Find the case type (wave## or wind##)
    pattern = r'(wave\d{2}|wind\d{2})'
    match = re.search(pattern, base)
    
    if match:
        case = match.group(1)
        if case in HEADING_MAP:
            heading = HEADING_MAP[case]
            # Insert heading after the case
            base = base.replace(case, f"{case}_{heading}")
    
    # Add output type
    return f"{base}_{output_type}.csv"

# Load actual headings from metadata
def load_heading_map_from_metadata(metadata_file):
    """Load heading mapping directly from reference metadata"""
    import pandas as pd
    
    df = pd.read_csv(metadata_file, encoding='latin-1')
    heading_map = {}
    
    for _, row in df.iterrows():
        stem = row['fe_filename_stem']
        
        # Extract case name (wave## or wind##)
        if 'wave' in stem:
            case = stem.split('_wave')[-1].split('_')[0]
            case_name = f'wave{case}'
            heading = format_heading(row['WaveDirection'])
        elif 'wind' in stem:
            case = stem.split('_wind')[-1].split('_')[0]
            case_name = f'wind{case}'
            heading = format_heading(row['WindDirection'])
        else:
            continue
            
        heading_map[case_name] = heading
    
    return heading_map
```

## Output Examples with Actual Headings

### Wave Cases (3 Tp variations per direction)
```
# 45° direction (wave01-03)
fsts_l015_mwl_wave01_045deg_Strut1_rainflow.csv
fsts_l015_mwl_wave02_045deg_Strut1_rainflow.csv  # Same direction, different Tp
fsts_l015_mwl_wave03_045deg_Strut1_rainflow.csv  # Same direction, different Tp

# 70° direction (wave04-06)
fsts_l015_mwl_wave04_070deg_Strut1_rainflow.csv
fsts_l015_mwl_wave05_070deg_Strut1_rainflow.csv
fsts_l015_mwl_wave06_070deg_Strut1_rainflow.csv

# 310° direction (wave16-18)
fsts_l095_125km3_l000_pb_mwl_wave16_310deg_Strut8_damage.csv
fsts_l095_125km3_l000_pb_mwl_wave17_310deg_Strut8_damage.csv
fsts_l095_125km3_l000_pb_mwl_wave18_310deg_Strut8_damage.csv
```

### Wind Cases (16 compass points)
```
# Cardinal directions
fsts_l015_125km3_l100_pb_mwl_wind01_000deg_Strut1_rainflow.csv  # North
fsts_l015_125km3_l100_pb_mwl_wind05_090deg_Strut1_rainflow.csv  # East
fsts_l015_125km3_l100_pb_mwl_wind09_180deg_Strut1_rainflow.csv  # South
fsts_l015_125km3_l100_pb_mwl_wind13_270deg_Strut1_rainflow.csv  # West

# Intermediate directions
fsts_l095_mwl_wind02_023deg_Strut5_damage.csv  # NNE
fsts_l095_mwl_wind06_113deg_Strut5_damage.csv  # ESE
fsts_l095_mwl_wind10_203deg_Strut5_damage.csv  # SSW
fsts_l095_mwl_wind14_293deg_Strut5_damage.csv  # WNW
```

## Directory Structure with Heading Organization

```
output/
├── fsts_l015/
│   ├── by_heading/
│   │   ├── 000deg/
│   │   ├── 023deg/
│   │   ├── 045deg/
│   │   │   ├── fsts_l015_mwl_wave01_045deg_Strut1_rainflow.csv
│   │   │   ├── fsts_l015_mwl_wave02_045deg_Strut1_rainflow.csv
│   │   │   ├── fsts_l015_mwl_wave03_045deg_Strut1_rainflow.csv
│   │   │   └── fsts_l015_mwl_wind03_045deg_Strut1_rainflow.csv
│   │   ├── 070deg/
│   │   ├── 090deg/
│   │   └── ...
│   └── summary/
│       └── fsts_l015_fatigue_summary_by_heading.csv
└── ...
```

## Notes on Wave Cases

The 18 wave cases represent:
- **6 wave directions**: 45°, 70°, 90°, 125°, 150°, 310°
- **3 wave periods (Tp)** per direction: ~1.93s, ~2.70s, ~3.47s
- All with **Hs = 0.5m** (reference wave height)
- Wind direction aligned with wave direction

This gives good coverage of the dominant wave directions while varying the wave period to capture different dynamic responses.