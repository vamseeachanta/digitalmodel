# Finalized Naming Convention for Seastates

## Approved Convention

### 1. Reference Seastates (Baseline Calibration Data)
**Format: REF_{type}{##}**
- `REF_WIND01` - Reference wind condition (10 m/s @ 0° baseline)
- `REF_WAVE01` - Reference wave condition (Hs=0.5m, Tp=2.7s @ 0° baseline)
- `REF_CURR01` - Reference current condition (if applicable)

**Purpose**: These are baseline measurements used for calibration and scaling calculations.

**File naming pattern:**
```
{vessel_config}_mwl_{reference}_Strut{#}.csv
```
Example: `fsts_l015_mwl_REF_WIND01_Strut1.csv`

### 2. Project/Field Seastates (Actual Environmental Conditions)
**Format: SS{###}** (Sea State)
- `SS001` - Sea State 001 (e.g., operational condition, 15 m/s, Hs=0.75m)
- `SS002` - Sea State 002 (e.g., baseline condition, 10 m/s, Hs=0.5m)
- `SS003` - Sea State 003 (e.g., calm condition, 5 m/s, Hs=0.25m)
- `SS004` - Sea State 004 (e.g., severe condition, 20 m/s, Hs=1.0m)

**Purpose**: These represent actual field/project conditions to be analyzed.

**File naming pattern:**
```
{vessel_config}_SS{###}_Strut{#}.csv
```
Example: `fsts_l015_SS001_Strut1.csv`

## Key Benefits

1. **Clear Distinction**: 
   - `REF_` prefix immediately identifies reference/calibration data
   - `SS` identifies actual sea states for analysis

2. **Industry Alignment**:
   - "Sea State" (SS) is standard terminology in offshore engineering
   - Clear separation between calibration and operational data

3. **Scalability**:
   - Can handle up to 999 sea states (SS001-SS999)
   - Easy to add more reference types (REF_WIND02, REF_WAVE02, etc.)

4. **Traceability**:
   - Easy to trace from output files back to conditions
   - Clear mapping between references and scaled outputs

## Conversion Mapping

| Old Name | New Name | Type | Description |
|----------|----------|------|-------------|
| wind01 | REF_WIND01 | Reference | Baseline wind (10 m/s) |
| wave01 | REF_WAVE01 | Reference | Baseline wave (Hs=0.5m) |
| FC001 | SS001 | Field | Test condition (15 m/s, Hs=0.75m) |
| FC002 | SS002 | Field | Reference condition (10 m/s, Hs=0.5m) |
| FC003 | SS003 | Field | Low condition (5 m/s, Hs=0.25m) |
| FC004 | SS004 | Field | High condition (20 m/s, Hs=1.0m) |

## Implementation Files to Update

1. **Documentation**:
   - sample_data_run_verification.md
   - scaling_factor_reference_table.md
   - All step summaries

2. **Mapping Files**:
   - fc_condition_mapping.csv → ss_condition_mapping.csv
   - fc_condition_mapping.md → ss_condition_mapping.md
   - fc_condition_mapping.json → ss_condition_mapping.json

3. **Code Files**:
   - load_scaler.py (FatigueCondition → SeaState)
   - verify_step_by_step.py
   - generate_step5_outputs.py

## Example Usage

### Input Reference Files:
```
fsts_l015_mwl_REF_WIND01_Strut1.csv  (reference wind)
fsts_l015_mwl_REF_WAVE01_Strut1.csv  (reference wave)
```

### Output Scaled Files:
```
fsts_l015_SS001_Strut1.csv  (scaled to SS001 conditions)
fsts_l015_SS002_Strut1.csv  (scaled to SS002 conditions)
```

### Scaling Calculation:
```python
# For SS001 (15 m/s wind, 0.75m waves)
wind_scale = (15 / 10)² = 2.25  # relative to REF_WIND01
wave_scale = 0.75 / 0.5 = 1.50   # relative to REF_WAVE01
```

This convention provides clear, immediate understanding of file purpose and maintains full traceability throughout the analysis workflow.