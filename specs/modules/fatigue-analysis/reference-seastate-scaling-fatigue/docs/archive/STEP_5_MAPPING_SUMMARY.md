# Step 5 Enhanced: FC Condition Mapping Summary

## What Was Added

### FC Condition to Reference File Mapping
A comprehensive table that provides complete traceability between:
- **Fatigue Conditions (FC001-FC004)** - Environmental load cases
- **Reference Files** - Actual CSV files used as input
- **Scaling Calculations** - How factors are computed
- **Vessel Configurations** - All 4 vessel types

## Key Information Provided

### 1. FC Naming Convention Explained
- **FC** = Fatigue Condition (industry standard)
- **001-004** = Sequential condition IDs
- Example: FC001 = Test condition with 15 m/s wind, 0.75m waves

### 2. Reference File Identification
Each FC condition maps to specific reference files:
- **Wind Reference**: `{vessel}_mwl_wind01_Strut[1-8].csv`
- **Wave Reference**: `{vessel}_mwl_wave01_Strut[1-8].csv`

### 3. Scaling Factor Verification
Clear calculation showing:
- Wind baseline: 10 m/s (from wind01)
- Wave baseline: Hs=0.5m (from wave01)
- FC001: Wind scale = (15/10)² = 2.25, Wave scale = 0.75/0.5 = 1.50

## Files Created

| File | Purpose | Format |
|------|---------|--------|
| `fc_condition_mapping.csv` | Full data table | CSV for Excel/analysis |
| `fc_condition_mapping.md` | Human-readable docs | Markdown with tables |
| `fc_condition_mapping.json` | Machine-readable | JSON for programs |
| `FC_NAMING_CONVENTION.md` | Explanation guide | Documentation |

## Sample Mapping Entry

```csv
FC ID: FC001
Description: Test Condition
Vessel: fsts_l015 (FSTs Light 15% loaded)
Wind: 15 m/s @ 0° → Scale: 2.25x
Wave: Hs=0.75m @ 0° → Scale: 1.50x
Wind Ref: fsts_l015_mwl_wind01_Strut*.csv
Wave Ref: fsts_l015_mwl_wave01_Strut*.csv
```

## User Verification Benefits

Users can now easily:
1. **Verify** the correct reference files are being used
2. **Understand** what FC001, FC002, etc. mean
3. **Check** scaling calculations match expectations
4. **Trace** any output back to its input files
5. **Confirm** all vessel configurations are processed

## Location
All mapping files are in: `output/verification/intermediate/`

This enhancement provides complete transparency in the scaling process, making it easy for users to validate the methodology and results.