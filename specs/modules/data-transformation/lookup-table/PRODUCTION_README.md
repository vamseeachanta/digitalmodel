# Production Transformation Setup

## Overview
Production configuration for transforming 2,592 rainflow tension files to stress using lookup table interpolation.

## Input Data
- **Location**: `D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\output\rainflow`
- **Files**: 2,592 CSV files with pattern `*_FC*_Strut*_rainflow.csv`
- **Columns**: `Range (kN)`, `Cycles_Annual`

## Output Data
- **Location**: `D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\output\rainflow_stress`
- **Files**: 18,144 files (2,592 × 7 locations)
- **Pattern**: `{config}_FC{###}_Strut{#}_loc{02}_stress_rainflow.csv`
- **Columns**: `stress range (Mpa)`, `Cycles_Annual`

## Quick Commands

### 1. Validate Setup
```bash
python validate_production_setup.py
```

### 2. Dry Run (Preview)
```bash
# Preview with 10 sample files
python run_production_transformation.py --dry-run --sample 10
```

### 3. Process Sample
```bash
# Process 100 files to test
python run_production_transformation.py --sample 100
```

### 4. Full Production Run
```bash
# Process all 2,592 files
python run_production_transformation.py
```

## Configuration Files
- `production_transformation_config.yaml` - Main configuration
- `inputs/tension_range_to_stress_range_function.csv` - Lookup table

## Processing Details
- **Locations**: 7 (2, 3, 5, 6, 7, 9, 10)
- **Method**: Linear interpolation with extrapolation
- **Estimated Time**: ~22 minutes for full run
- **Estimated Disk Space**: ~886 MB
- **Parallel Processing**: 4 workers, batch size 100

## Config Mapping
Automatic mapping for files without `_mwl` suffix:
- `fsts_l015_125km3_l100_pb` → `fsts_l015_125km3_l100_pb_mwl`
- `fsts_l095_125km3_l000_pb` → `fsts_l095_125km3_l000_pb_mwl`
- `fsts_l015` → `fsts_l015_mwl`
- `fsts_l095` → `fsts_l095_mwl`

## Validation Checklist
- [x] Production config exists
- [x] Input directory accessible (2,592 files)
- [x] Lookup table loaded (1,148 records)
- [x] 7 location IDs confirmed
- [x] Sample file readable
- [x] Required columns present
- [x] Dependencies installed (scipy, pandas)

## Monitoring
- Progress bar during processing
- Log file: `production_transformation.log`
- Summary file: `production_summary.yaml`
- Error log: `production_errors.log`

## Troubleshooting
| Issue | Solution |
|-------|----------|
| FileNotFoundError | Check paths in `production_transformation_config.yaml` |
| Memory issues | Reduce `batch_size` and `max_workers` |
| Missing columns | Verify input file format matches expected |
| Config not found | Check config mapping section |

## Status
✅ **PRODUCTION READY** - All validations passed