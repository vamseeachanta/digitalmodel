# Production Data Structure - Aligned with Actual Production

## Production Data Location
`D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\csv`

## File Naming Convention (ACTUAL PRODUCTION)

### Pattern
```
{config}_mwl_{reference}_Strut{#}.csv
```

### Components
- `{config}`: Configuration identifier
  - `fsts_l015` - FSTs Light (15% loaded)
  - `fsts_l095` - FSTs Full (95% loaded)
  - `fsts_l015_125km3_l100_pb` - FSTs Light + LNGC Full (Port Berthing)
  - `fsts_l095_125km3_l000_pb` - FSTs Full + LNGC Light (Port Berthing)
- `mwl`: Mean Water Level (always present)
- `{reference}`: Reference condition
  - `wind01` to `wind16` - Wind reference cases
  - `wave01` to `wave18` - Wave reference cases
- `Strut{#}`: Strut number (1-8)

### Examples from Production
```
fsts_l015_mwl_wind01_Strut1.csv
fsts_l015_mwl_wave01_Strut1.csv
fsts_l095_mwl_wind01_Strut1.csv
fsts_l095_mwl_wave01_Strut1.csv
fsts_l015_125km3_l100_pb_mwl_wind01_Strut1.csv
fsts_l095_125km3_l000_pb_mwl_wave01_Strut1.csv
```

## Sample Data Structure (Updated to Match)

### Location
`specs/modules/fatigue-analysis/reference-seastate-scaling-fatigue/sample_data_production/`

### Files Created (Subset for Testing)
Using `wind01` and `wave01` as reference conditions:

```
# Configuration 1: FSTs Light
fsts_l015_mwl_wind01_Strut[1-8].csv
fsts_l015_mwl_wave01_Strut[1-8].csv

# Configuration 2: FSTs Full  
fsts_l095_mwl_wind01_Strut[1-8].csv
fsts_l095_mwl_wave01_Strut[1-8].csv

# Configuration 3: FSTs Light + LNGC Full
fsts_l015_125km3_l100_pb_mwl_wind01_Strut[1-8].csv
fsts_l015_125km3_l100_pb_mwl_wave01_Strut[1-8].csv

# Configuration 4: FSTs Full + LNGC Light
fsts_l095_125km3_l000_pb_mwl_wind01_Strut[1-8].csv
fsts_l095_125km3_l000_pb_mwl_wave01_Strut[1-8].csv
```

Total: 64 files (4 configs × 2 references × 8 struts)

## Key Differences from Previous Structure

### Old (Incorrect) Structure
```
sample_data/
├── fsts_l015/
│   ├── wind_000deg/
│   │   └── Strut1.csv
│   └── wave_000deg_Hs050cm_Tp270cs/
│       └── Strut1.csv
```

### New (Production-Aligned) Structure
```
sample_data_production/
├── fsts_l015_mwl_wind01_Strut1.csv
├── fsts_l015_mwl_wave01_Strut1.csv
├── fsts_l095_mwl_wind01_Strut1.csv
└── ... (flat structure, all files in one directory)
```

## Reference Conditions

### Wind References (wind01-wind16)
- `wind01`: Baseline at 10 m/s, 0° (used for sample data)
- `wind02-wind16`: Various wind speeds and directions

### Wave References (wave01-wave18)
- `wave01`: Baseline at Hs=0.5m, Tp=2.7s, 0° (used for sample data)
- `wave02-wave18`: Various wave heights, periods, and directions

## Data Content

Each CSV file contains:
- Column 1: `Time` (seconds)
- Column 2: `Effective Tension at Vessel End` (kN)

## Important Notes

1. **Flat Structure**: Production uses a flat directory with all CSV files at the same level
2. **Consistent Naming**: All files follow exact pattern `{config}_mwl_{reference}_Strut{#}.csv`
3. **Only Strut Files**: Only process files ending with `Strut[1-8].csv`
4. **Ignore Other Files**: Skip Jacket files and dm_fsts summary files

## Code Updates Required

The code needs to:
1. Look for files matching pattern `*_Strut?.csv`
2. Parse configuration from filename prefix
3. Identify reference type (wind/wave) from filename
4. Handle flat directory structure (not nested folders)

## Example File Parsing

```python
filename = "fsts_l015_125km3_l100_pb_mwl_wind01_Strut1.csv"

# Parse components
parts = filename.replace('.csv', '').split('_')
# Configuration: everything before 'mwl'
mwl_index = parts.index('mwl')
config = '_'.join(parts[:mwl_index])  # "fsts_l015_125km3_l100_pb"
reference = parts[mwl_index + 1]       # "wind01" 
strut = parts[-1]                      # "Strut1"
```

---
*This structure exactly matches the production data at D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\csv*