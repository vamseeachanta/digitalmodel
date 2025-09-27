# Stress Rainflow to Damage Configuration Summary

## Configuration Files Created
Date: 2025-01-25

### 1. Main Configuration (Updated)
**File:** `damage_analysis_config.yml`
- Complete configuration with all clarified parameters
- All placeholders replaced with actual values from location_metadata.csv
- User clarifications incorporated

### 2. Sample Configuration (New)
**File:** `damage_analysis_config_sample.yml`
- Process 5 files for initial testing
- Configuration: fsts_l015_125km3_l100_pb
- FC Number: FC001
- Struts: 1, 2
- Locations: loc02, loc03, loc05
- Single-threaded processing for debugging

### 3. Production Configuration (New)
**File:** `damage_analysis_config_production.yml`
- Process all 224 files in the stress_rainflow folder
- Multi-threaded processing (32 cores)
- Full batch processing enabled
- Performance optimizations activated

## Key Parameters Confirmed

### S-N Curve (ABS E in-air)
- log_a = 12.164
- m = 3.0
- Fatigue limit at 10^7 cycles: 52.64 MPa

### Thickness Correction
- Reference thickness: 22 mm
- Thickness exponent (tk): 0.25
- Location-specific thicknesses from metadata:
  - loc01: 50mm | loc02: 25mm | loc03: 25mm
  - loc04: 20mm | loc05: 18mm | loc06: 20mm
  - loc07: 20mm | loc08: 50mm | loc09: 50mm | loc10: 50mm

### Stress Factors
- **SCFs: MUST be applied to stress ranges** (enabled: true)
  - loc02: 2.0 (highest SCF for 25mm Blisters)
  - All other locations: 1.15
- Mean stress correction: Not needed (confirmed by user)
- Design factor: 5.0 (from location metadata)

### File Inventory
- Total stress rainflow files available: **224 files**
- Configurations: fsts_l015_125km3_l100_pb, fsts_l015
- FC Numbers: FC001, FC002
- Struts: 1-8
- Locations: loc02, loc03, loc05, loc06, loc07, loc09, loc10

### Output Configuration
- Base path: `specs/modules/fatigue-analysis/reference-seastate-scale-load/output/rainflow/stress_range`
- Results include:
  - Individual damage rate CSV files per input
  - Damage vs stress range visualizations
  - S-N curve overlay plots
  - Summary report of all analyses

## Clarifications Applied

1. **S-N Curve Parameters**: Confirmed ABS E curve values
2. **Thickness Exponent**: tk = 0.25 confirmed for steel
3. **SCF Application**: ⚠️ **NOT included in stress ranges - MUST be applied during calculation**
4. **Mean Stress**: No correction needed
5. **Processing Scope**: Sample (5 files) then production (224 files)
6. **Design Factor**: 5.0 from metadata for fatigue life calculation
7. **Thickness Values**: All 10 locations mapped from metadata file

## File Pattern Analysis

The stress rainflow files follow this naming convention:
```
{config}_FC{###}_Strut{#}_{location_id}_stress_rainflow.csv
```

Available combinations:
- Configs: 2 (fsts_l015_125km3_l100_pb, fsts_l015)
- FC numbers: 2 (FC001, FC002)
- Struts: 8 (1-8)
- Locations: 7 (loc02, loc03, loc05, loc06, loc07, loc09, loc10)
- Total: 2 × 2 × 8 × 7 = 224 files ✓

## Next Steps

1. **Run Sample Configuration**
   ```bash
   python run_damage_analysis.py --config input/damage_analysis_config_sample.yml
   ```
   - Verify calculations are correct
   - Check output format
   - Review visualization quality

2. **Run Production Configuration**
   ```bash
   python run_damage_analysis.py --config input/damage_analysis_config_production.yml
   ```
   - Process all 224 files
   - Generate complete damage analysis
   - Create summary report

3. **Validation Checks**
   - Verify Miner's rule implementation
   - Check thickness correction factors
   - Validate against known fatigue life expectations
   - Review files with highest damage rates

## Notes

- The fatigue limit stress (52.64 MPa) was calculated using the S-N curve equation at 10^7 cycles
- Location loc01, loc04, and loc08 are not present in the stress rainflow files (only in metadata)
- The design factor of 5 means the acceptable fatigue life should be 5 × 25 years = 125 years
- Files with zero cycles in most bins (like the sample file read) indicate very low stress conditions
- **IMPORTANT**: SCF multiplication will be applied to all stress ranges during calculation:
  - Location loc02: stress × 2.0 (highest stress concentration)
  - All other locations: stress × 1.15