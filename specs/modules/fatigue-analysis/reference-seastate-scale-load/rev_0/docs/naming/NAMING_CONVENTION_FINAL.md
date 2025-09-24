# Final Naming Convention

## Core Naming Pattern
```
{config}_FC{###}_Strut{#}_{type}.csv
```

## Complete File Hierarchy

### 1. Scaled Reference Files (Intermediate - for QA)
Individual scaled wind and wave references:
```
# Wind reference scaled
fsts_l015_FC001_wind09_s025_Strut1_scaled.csv     # Wind @ 5m/s, scaled by 0.25

# Wave reference scaled  
fsts_l015_FC001_wave13_s030_Strut1_scaled.csv     # Wave Hs=0.15m, scaled by 0.30
```

### 2. Combined Effective Tension Files
Combined wind + wave effective tension:
```
fsts_l015_FC001_Strut1_combined.csv
```

### 3. Rainflow Analysis Files
Rainflow counting results:
```
fsts_l015_FC001_Strut1_rainflow.csv
```

### 4. Damage Calculation Files
Fatigue damage results:
```
fsts_l015_FC001_Strut1_damage.csv
```

### 5. Summary Files
Configuration and overall summaries:
```
fsts_l015_fatigue_summary.csv          # Per configuration
overall_fatigue_summary.csv            # All configurations combined
```

## Complete Example: Processing FC023

**Fatigue Condition #23:**
- Wind: 18 m/s @ 180°
- Wave: Hs=0.8m @ 150°, Tp=3.7s
- Configuration: fsts_l015
- Strut: 1

**Files Generated:**

```
Step 1 - Scaled References (Intermediate):
  fsts_l015_FC023_wind09_s324_Strut1_scaled.csv    # Wind scaled by 3.24
  fsts_l015_FC023_wave15_s160_Strut1_scaled.csv    # Wave scaled by 1.60

Step 2 - Combined:
  fsts_l015_FC023_Strut1_combined.csv              # Effective tension

Step 3 - Rainflow:
  fsts_l015_FC023_Strut1_rainflow.csv              # Cycle counting

Step 4 - Damage:
  fsts_l015_FC023_Strut1_damage.csv                # Fatigue damage
```

## Directory Structure

```
output/
├── fsts_l015/
│   ├── scaled_references/              # Intermediate files for QA
│   │   ├── fsts_l015_FC001_wind09_s025_Strut1_scaled.csv
│   │   ├── fsts_l015_FC001_wave13_s030_Strut1_scaled.csv
│   │   └── ... (2 files per FC per Strut = 1,296 files)
│   │
│   ├── combined_tensions/              # Combined effective tensions
│   │   ├── fsts_l015_FC001_Strut1_combined.csv
│   │   └── ... (81 FC × 8 Struts = 648 files)
│   │
│   ├── rainflow_results/               # Rainflow counting results
│   │   ├── fsts_l015_FC001_Strut1_rainflow.csv
│   │   └── ... (648 files)
│   │
│   ├── damage_results/                 # Damage calculations
│   │   ├── fsts_l015_FC001_Strut1_damage.csv
│   │   └── ... (648 files)
│   │
│   └── summaries/
│       ├── fsts_l015_fatigue_summary.csv
│       └── fsts_l015_processing_map.csv
│
├── fsts_l095/                          # Same structure
├── fsts_l015_125km3_l100_pb/          # Same structure
├── fsts_l095_125km3_l000_pb/          # Same structure
│
└── overall/
    ├── overall_fatigue_summary.csv
    └── configuration_comparison.csv
```

## Processing Map File

Track full details for QA and traceability:

```csv
# fsts_l015_processing_map.csv
fc_id,strut,wind_speed,wind_dir,wind_ref,wind_scale,wave_hs,wave_dir,wave_tp_target,wave_tp_used,wave_ref,wave_scale,occurrence_pct
FC001,Strut1,5.0,180,wind09,0.25,0.15,150,2.7,1.93,wave13,0.30,7.76
FC001,Strut2,5.0,180,wind09,0.25,0.15,150,2.7,1.93,wave13,0.30,7.76
...
FC023,Strut1,18.0,180,wind09,3.24,0.80,150,3.7,3.47,wave15,1.60,1.365
```

## Python Implementation

```python
class FatigueFileNamer:
    """Handles all fatigue analysis file naming"""
    
    def __init__(self, config, output_base_dir):
        self.config = config
        self.base_dir = Path(output_base_dir) / config
        
    def get_scaled_ref_filename(self, fc_id, ref_case, scale_factor, strut_num, ref_type='wind'):
        """Scaled reference file (intermediate)"""
        scale_str = f"s{int(scale_factor*100):03d}"  # e.g., 0.25 -> s025
        return f"{self.config}_FC{fc_id:03d}_{ref_case}_{scale_str}_Strut{strut_num}_scaled.csv"
    
    def get_combined_filename(self, fc_id, strut_num):
        """Combined effective tension file"""
        return f"{self.config}_FC{fc_id:03d}_Strut{strut_num}_combined.csv"
    
    def get_rainflow_filename(self, fc_id, strut_num):
        """Rainflow results file"""
        return f"{self.config}_FC{fc_id:03d}_Strut{strut_num}_rainflow.csv"
    
    def get_damage_filename(self, fc_id, strut_num):
        """Damage results file"""
        return f"{self.config}_FC{fc_id:03d}_Strut{strut_num}_damage.csv"
    
    def get_summary_filename(self):
        """Configuration summary file"""
        return f"{self.config}_fatigue_summary.csv"

# Usage Example
namer = FatigueFileNamer('fsts_l015', 'output')

# For fatigue condition 23, strut 1
scaled_wind = namer.get_scaled_ref_filename(23, 'wind09', 3.24, 1, 'wind')
# Returns: 'fsts_l015_FC023_wind09_s324_Strut1_scaled.csv'

scaled_wave = namer.get_scaled_ref_filename(23, 'wave15', 1.60, 1, 'wave')
# Returns: 'fsts_l015_FC023_wave15_s160_Strut1_scaled.csv'

combined = namer.get_combined_filename(23, 1)
# Returns: 'fsts_l015_FC023_Strut1_combined.csv'

rainflow = namer.get_rainflow_filename(23, 1)
# Returns: 'fsts_l015_FC023_Strut1_rainflow.csv'

damage = namer.get_damage_filename(23, 1)
# Returns: 'fsts_l015_FC023_Strut1_damage.csv'
```

## File Count Summary

Per configuration:
- Scaled references: 81 FC × 8 Struts × 2 (wind+wave) = 1,296 files
- Combined tensions: 81 FC × 8 Struts = 648 files
- Rainflow results: 81 FC × 8 Struts = 648 files
- Damage results: 81 FC × 8 Struts = 648 files
- **Total per config: 3,240 files**

All 4 configurations:
- **Grand total: 12,960 files + summaries**

## Benefits

1. **Core pattern maintained**: `{config}_FC{###}_Strut{#}_{type}.csv`
2. **Intermediate files kept**: All scaled references saved for QA
3. **Full traceability**: Can verify each step of the process
4. **Clear organization**: Separate folders for each processing stage
5. **Metadata tracking**: Processing map file has all details

This naming convention provides the right balance between simplicity and traceability while keeping all intermediate results for quality assurance.