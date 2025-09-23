# Reference Seastates to Sea State Mapping

## How Reference Seastates Map to Each Sea State

### Core Principle
ALL Sea States (SS001-SS004) use the SAME reference seastates (REF_WIND01 and REF_WAVE01) as their baseline for scaling.

## Complete Mapping Table

| Sea State | Environmental Conditions | Reference Files Used | Scaling Factors | Purpose |
|-----------|-------------------------|---------------------|-----------------|---------|
| **SS001** | Wind: 15 m/s<br>Wave: Hs=0.75m | REF_WIND01 (10 m/s)<br>REF_WAVE01 (0.5m) | Wind: 2.25x<br>Wave: 1.50x | Test validation |
| **SS002** | Wind: 10 m/s<br>Wave: Hs=0.50m | REF_WIND01 (10 m/s)<br>REF_WAVE01 (0.5m) | Wind: 1.00x<br>Wave: 1.00x | Baseline check |
| **SS003** | Wind: 5 m/s<br>Wave: Hs=0.25m | REF_WIND01 (10 m/s)<br>REF_WAVE01 (0.5m) | Wind: 0.25x<br>Wave: 0.50x | Calm conditions |
| **SS004** | Wind: 20 m/s<br>Wave: Hs=1.00m | REF_WIND01 (10 m/s)<br>REF_WAVE01 (0.5m) | Wind: 4.00x<br>Wave: 2.00x | Severe conditions |

## Reference Seastate Details

### REF_WIND01
- **Type**: Wind reference baseline
- **Speed**: 10 m/s
- **Direction**: 0°
- **Purpose**: Calibration baseline for ALL wind scaling
- **Used by**: ALL Sea States (SS001-SS004)

### REF_WAVE01
- **Type**: Wave reference baseline
- **Significant Height (Hs)**: 0.5 m
- **Peak Period (Tp)**: 2.7 s
- **Direction**: 0°
- **Purpose**: Calibration baseline for ALL wave scaling
- **Used by**: ALL Sea States (SS001-SS004)

## Scaling Calculation Examples

### For SS001 (Test Condition)
```
Target: 15 m/s wind, 0.75m waves

Wind Scaling = (Target Wind / REF_WIND01)²
            = (15 / 10)²
            = 2.25x

Wave Scaling = Target Hs / REF_WAVE01 Hs
            = 0.75 / 0.5
            = 1.50x

Result: Scale REF_WIND01 by 2.25x and REF_WAVE01 by 1.50x
```

### For SS003 (Low Condition)
```
Target: 5 m/s wind, 0.25m waves

Wind Scaling = (5 / 10)²
            = 0.25x

Wave Scaling = 0.25 / 0.5
            = 0.50x

Result: Scale REF_WIND01 by 0.25x and REF_WAVE01 by 0.50x
```

## File Processing Flow

### Input Files (Reference Seastates)
For each vessel configuration and strut:
```
{vessel}_mwl_REF_WIND01_Strut{#}.csv  → Wind component baseline
{vessel}_mwl_REF_WAVE01_Strut{#}.csv  → Wave component baseline
```

### Processing
1. Load REF_WIND01 data (wind baseline)
2. Load REF_WAVE01 data (wave baseline)
3. Apply scaling factors for target sea state
4. Combine scaled components

### Output Files (Sea States)
```
{vessel}_SS001_Strut{#}.csv  → Scaled to SS001 conditions
{vessel}_SS002_Strut{#}.csv  → Scaled to SS002 conditions
{vessel}_SS003_Strut{#}.csv  → Scaled to SS003 conditions
{vessel}_SS004_Strut{#}.csv  → Scaled to SS004 conditions
```

## Key Points

1. **Same References for All**: Every sea state uses REF_WIND01 and REF_WAVE01
2. **Scaling Only**: Sea states are created by scaling the reference data
3. **No Direct Measurements**: SS files are derived, not measured
4. **Traceability**: Any SS output can be traced back to REF inputs
5. **Consistency**: Using same references ensures consistent baseline

## Verification Checklist

- [ ] REF_WIND01 files exist for all vessel configurations
- [ ] REF_WAVE01 files exist for all vessel configurations
- [ ] All 8 struts have reference files
- [ ] Scaling formulas correctly applied
- [ ] SS001 uses 2.25x wind, 1.50x wave scaling
- [ ] SS002 uses 1.00x wind, 1.00x wave scaling (validation)
- [ ] SS003 uses 0.25x wind, 0.50x wave scaling
- [ ] SS004 uses 4.00x wind, 2.00x wave scaling