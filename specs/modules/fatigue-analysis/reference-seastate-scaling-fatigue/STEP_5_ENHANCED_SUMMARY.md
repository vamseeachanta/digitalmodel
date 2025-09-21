# Step 5 Enhanced Summary - FC Condition Mapping Added

## New Intermediate Outputs Created

### 1. FC Condition to Reference File Mapping
Generated comprehensive mapping showing the connection between:
- **FC Conditions (FC001-FC004)**: Each fatigue condition with its environmental parameters
- **Reference Files**: Exact filenames used as input data
- **Scaling Factors**: Calculated values for wind and wave scaling
- **Vessel Configurations**: All 4 vessel types and their files

### 2. Files Generated in `output/verification/intermediate/`

#### Mapping Files (NEW)
- `fc_condition_mapping.csv` - Full tabular data for all conditions
- `fc_condition_mapping.md` - Human-readable documentation  
- `fc_condition_mapping.json` - Machine-readable format

#### Previous Files (Still Present)
- `FC001_components.csv` through `FC004_components.csv` - Component breakdowns
- `scaling_summary.csv` - Statistical summary
- `scaling_comparison.md` - Comparison table
- `step5_metadata.json` - Metadata tracking

### 3. FC Naming Convention Documentation
Created `FC_NAMING_CONVENTION.md` explaining:
- **FC** = Fatigue Condition
- **###** = Three-digit sequential ID (001, 002, etc.)
- Industry standard practice for fatigue analysis
- Traceability from output files back to environmental conditions

## Example Mapping Entry

**FC001: Test Condition**
- Wind: 15 m/s @ 0° → Scale: 2.25x (15/10)²
- Wave: Hs=0.75m, Tp=4.0s @ 0° → Scale: 1.50x (0.75/0.5)
- Reference Files:
  - Wind: `fsts_l015_mwl_wind01_Strut[1-8].csv`
  - Wave: `fsts_l015_mwl_wave01_Strut[1-8].csv`

## Verification Benefits

This mapping table allows users to:
1. **Verify** that correct reference files are being used
2. **Trace** any output back to its source data
3. **Confirm** scaling calculations are correct
4. **Understand** the FC naming system
5. **Validate** all vessel configurations are processed

## Reference Files Pattern

All reference files follow the pattern:
```
{vessel_config}_mwl_{reference}_Strut{#}.csv
```

Where:
- `vessel_config`: One of 4 vessel configurations
- `mwl`: Mean Water Level (always present)
- `reference`: Either `wind01` or `wave01`
- `Strut{#}`: Strut number 1-8

This ensures complete traceability from input reference files through scaling calculations to output files.