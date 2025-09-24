# Simplified Naming Convention (Option B)

## Design Principles
- Keep essential information for traceability
- Avoid overly long filenames
- Maintain consistency with input naming pattern
- Easy to parse programmatically

## Input Files (Existing)
```
fsts_l015_mwl_wave01_Strut1.csv
fsts_l095_125km3_l000_pb_mwl_wind16_Strut8.csv
```

## Output Files (Proposed Option B)

### Level 1: Scaled Reference Files
When processing individual reference cases with scaling:
```
# Format: {input_base}_s{scale_factor}_{output_type}.csv

fsts_l015_mwl_wave01_Strut1_s150_effective.csv      # Scaled by 1.5
fsts_l015_mwl_wind09_Strut1_s225_effective.csv      # Scaled by 2.25
```

### Level 2: Combined Fatigue Condition Files
When combining wind and wave for a fatigue condition:
```
# Format: {config}_FC{id:03d}_Strut{N}_{output_type}.csv

fsts_l015_FC001_Strut1_combined.csv      # Fatigue condition 1
fsts_l015_FC001_Strut1_rainflow.csv      # Rainflow results
fsts_l015_FC001_Strut1_damage.csv        # Damage calculation
```

### Level 3: Summary Files
Configuration and overall summaries:
```
# Format: {config}_fatigue_summary.csv

fsts_l015_fatigue_summary.csv                    # Config summary
fsts_l015_critical_struts.csv                    # Critical locations
overall_fatigue_summary.csv                      # All configs combined
```

## Examples for Complete Workflow

### Processing Fatigue Condition #23 (Row 23)
- Wind: 18 m/s @ 180°
- Wave: Hs=0.8m @ 150°, Tp=3.7s

**Step 1: Reference Selection**
- Wind ref: wind09 (180°) → scale = (18/10)² = 3.24
- Wave ref: wave15 (150°, Tp=3.47s closest to 3.7s) → scale = 0.8/0.5 = 1.6

**Step 2: File Processing**
```
Input files:
  fsts_l015_mwl_wind09_Strut1.csv
  fsts_l015_mwl_wave15_Strut1.csv

Intermediate (optional):
  fsts_l015_mwl_wind09_Strut1_s324_effective.csv
  fsts_l015_mwl_wave15_Strut1_s160_effective.csv

Combined output:
  fsts_l015_FC023_Strut1_combined.csv
  fsts_l015_FC023_Strut1_rainflow.csv
  fsts_l015_FC023_Strut1_damage.csv
```

## Metadata Tracking File
To maintain full traceability, create a mapping file:

```csv
# fatigue_processing_map.csv
fc_id,config,strut,wind_ref,wind_scale,wave_ref,wave_scale,wave_tp_used,output_file
FC001,fsts_l015,Strut1,wind09,0.25,wave13,0.30,1.93,fsts_l015_FC001_Strut1_combined.csv
FC023,fsts_l015,Strut1,wind09,3.24,wave15,1.60,3.47,fsts_l015_FC023_Strut1_combined.csv
...
```

## Directory Structure
```
output/
├── fsts_l015/
│   ├── fatigue_conditions/
│   │   ├── fsts_l015_FC001_Strut1_combined.csv
│   │   ├── fsts_l015_FC001_Strut1_rainflow.csv
│   │   ├── fsts_l015_FC001_Strut1_damage.csv
│   │   └── ...
│   ├── summaries/
│   │   ├── fsts_l015_fatigue_summary.csv
│   │   └── fsts_l015_critical_struts.csv
│   └── metadata/
│       └── fsts_l015_processing_map.csv
├── fsts_l095/
├── fsts_l015_125km3_l100_pb/
├── fsts_l095_125km3_l000_pb/
└── overall/
    └── overall_fatigue_summary.csv
```

## Benefits of Option B

1. **Concise**: FC001 instead of lengthy direction/Tp details
2. **Traceable**: FC number maps back to fatigue_seastates.csv row
3. **Organized**: Clear hierarchy from individual to summary
4. **Parseable**: Easy to extract FC number and strut from filename
5. **Metadata**: Full details in separate tracking file

## Python Implementation

```python
def get_output_filename(config, fatigue_condition_id, strut_num, output_type):
    """
    Generate Option B simplified filename
    
    Args:
        config: 'fsts_l015', 'fsts_l095', etc.
        fatigue_condition_id: 1-81 (row number from fatigue_seastates.csv)
        strut_num: 1-8
        output_type: 'combined', 'rainflow', 'damage'
    
    Returns:
        Filename string
    """
    return f"{config}_FC{fatigue_condition_id:03d}_Strut{strut_num}_{output_type}.csv"

# Examples
filename1 = get_output_filename('fsts_l015', 1, 1, 'combined')
# Returns: 'fsts_l015_FC001_Strut1_combined.csv'

filename2 = get_output_filename('fsts_l095_125km3_l000_pb', 81, 8, 'damage')  
# Returns: 'fsts_l095_125km3_l000_pb_FC081_Strut8_damage.csv'
```

## Alternative Option B2 (Even Simpler)
If the above is still too complex, we could go even simpler:
```
FC001_S1.csv          # Just fatigue condition and strut
FC001_S1_rf.csv       # Rainflow results
FC001_S1_dmg.csv      # Damage results
```

## Questions for Review

1. **Is the FC### naming acceptable?** (maps to row in fatigue_seastates.csv)
2. **Should we include intermediate scaled files?** Or just final combined?
3. **Is the directory structure appropriate?**
4. **Do you prefer Option B or even simpler B2?**
5. **Should config name be in the FC files?** (helps when files are moved)

Please review and let me know which aspects to adjust.