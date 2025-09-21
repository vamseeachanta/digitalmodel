# Sea State (SS) Condition to Reference File Mapping
Generated: 2025-09-21 07:02:29

## Reference Conditions (Calibration Baselines)

| Reference | Type | Wind Speed | Hs | Tp | Direction | Description |
|-----------|------|------------|----|----|-----------|-------------|
| REF_WIND01 | Wind | 10 m/s | - | - | 0° | Baseline wind for calibration |
| REF_WAVE01 | Wave | - | 0.5 m | 2.7 s | 0° | Baseline wave for calibration |

## Sea State (Field/Project) Conditions Summary

| SS ID | Description | Wind (m/s) | Hs (m) | Tp (s) | Wind Scale | Wave Scale |
|-------|-------------|------------|--------|--------|------------|------------|
| SS001 | Test Condition | 15 | 0.75 | 4.0 | 2.25x | 1.50x |
| SS002 | Reference Baseline | 10 | 0.5 | 2.7 | 1.00x | 1.00x |
| SS003 | Low Condition | 5 | 0.25 | 2.0 | 0.25x | 0.50x |
| SS004 | High Condition | 20 | 1.0 | 5.0 | 4.00x | 2.00x |

## Detailed File Mapping

### SS001: Test Condition

**Environmental Conditions:**
- Wind: 15 m/s @ 0°
- Wave: Hs=0.75m, Tp=4.0s @ 0°
- Scaling: Wind=2.25x (relative to REF_WIND01), Wave=1.50x (relative to REF_WAVE01)

**Reference Files Used:**

| Vessel Config | Wind Reference File | Wave Reference File |
|---------------|--------------------|--------------------||
| fsts_l015 | fsts_l015_mwl_REF_WIND01_Strut[1-8].csv | fsts_l015_mwl_REF_WAVE01_Strut[1-8].csv |
| fsts_l095 | fsts_l095_mwl_REF_WIND01_Strut[1-8].csv | fsts_l095_mwl_REF_WAVE01_Strut[1-8].csv |
| fsts_l015_125km3_l100_pb | fsts_l015_125km3_l100_pb_mwl_REF_WIND01_Strut[1-8].csv | fsts_l015_125km3_l100_pb_mwl_REF_WAVE01_Strut[1-8].csv |
| fsts_l095_125km3_l000_pb | fsts_l095_125km3_l000_pb_mwl_REF_WIND01_Strut[1-8].csv | fsts_l095_125km3_l000_pb_mwl_REF_WAVE01_Strut[1-8].csv |

### SS002: Reference Baseline

**Environmental Conditions:**
- Wind: 10 m/s @ 0°
- Wave: Hs=0.5m, Tp=2.7s @ 0°
- Scaling: Wind=1.00x (relative to REF_WIND01), Wave=1.00x (relative to REF_WAVE01)

**Reference Files Used:**

| Vessel Config | Wind Reference File | Wave Reference File |
|---------------|--------------------|--------------------||
| fsts_l015 | fsts_l015_mwl_REF_WIND01_Strut[1-8].csv | fsts_l015_mwl_REF_WAVE01_Strut[1-8].csv |
| fsts_l095 | fsts_l095_mwl_REF_WIND01_Strut[1-8].csv | fsts_l095_mwl_REF_WAVE01_Strut[1-8].csv |
| fsts_l015_125km3_l100_pb | fsts_l015_125km3_l100_pb_mwl_REF_WIND01_Strut[1-8].csv | fsts_l015_125km3_l100_pb_mwl_REF_WAVE01_Strut[1-8].csv |
| fsts_l095_125km3_l000_pb | fsts_l095_125km3_l000_pb_mwl_REF_WIND01_Strut[1-8].csv | fsts_l095_125km3_l000_pb_mwl_REF_WAVE01_Strut[1-8].csv |

### SS003: Low Condition

**Environmental Conditions:**
- Wind: 5 m/s @ 0°
- Wave: Hs=0.25m, Tp=2.0s @ 0°
- Scaling: Wind=0.25x (relative to REF_WIND01), Wave=0.50x (relative to REF_WAVE01)

**Reference Files Used:**

| Vessel Config | Wind Reference File | Wave Reference File |
|---------------|--------------------|--------------------||
| fsts_l015 | fsts_l015_mwl_REF_WIND01_Strut[1-8].csv | fsts_l015_mwl_REF_WAVE01_Strut[1-8].csv |
| fsts_l095 | fsts_l095_mwl_REF_WIND01_Strut[1-8].csv | fsts_l095_mwl_REF_WAVE01_Strut[1-8].csv |
| fsts_l015_125km3_l100_pb | fsts_l015_125km3_l100_pb_mwl_REF_WIND01_Strut[1-8].csv | fsts_l015_125km3_l100_pb_mwl_REF_WAVE01_Strut[1-8].csv |
| fsts_l095_125km3_l000_pb | fsts_l095_125km3_l000_pb_mwl_REF_WIND01_Strut[1-8].csv | fsts_l095_125km3_l000_pb_mwl_REF_WAVE01_Strut[1-8].csv |

### SS004: High Condition

**Environmental Conditions:**
- Wind: 20 m/s @ 0°
- Wave: Hs=1.0m, Tp=5.0s @ 0°
- Scaling: Wind=4.00x (relative to REF_WIND01), Wave=2.00x (relative to REF_WAVE01)

**Reference Files Used:**

| Vessel Config | Wind Reference File | Wave Reference File |
|---------------|--------------------|--------------------||
| fsts_l015 | fsts_l015_mwl_REF_WIND01_Strut[1-8].csv | fsts_l015_mwl_REF_WAVE01_Strut[1-8].csv |
| fsts_l095 | fsts_l095_mwl_REF_WIND01_Strut[1-8].csv | fsts_l095_mwl_REF_WAVE01_Strut[1-8].csv |
| fsts_l015_125km3_l100_pb | fsts_l015_125km3_l100_pb_mwl_REF_WIND01_Strut[1-8].csv | fsts_l015_125km3_l100_pb_mwl_REF_WAVE01_Strut[1-8].csv |
| fsts_l095_125km3_l000_pb | fsts_l095_125km3_l000_pb_mwl_REF_WIND01_Strut[1-8].csv | fsts_l095_125km3_l000_pb_mwl_REF_WAVE01_Strut[1-8].csv |

## Scaling Formulas

- **Wind Scaling**: `(V_actual / V_reference)²` = `(V / 10)²` relative to REF_WIND01
- **Wave Scaling**: `Hs_actual / Hs_reference` = `Hs / 0.5` relative to REF_WAVE01
- **Combined**: `Tension = Wind_base * Wind_scale + Wave_base * Wave_scale`

## File Naming Patterns

### Reference Files (Calibration Data):
```
{vessel_config}_mwl_{reference}_Strut{#}.csv
```
Example: `fsts_l015_mwl_REF_WIND01_Strut1.csv`

### Output Files (Scaled to Sea States):
```
{vessel_config}_SS{###}_Strut{#}.csv
```
Example: `fsts_l015_SS001_Strut1.csv`

Where:
- `vessel_config`: fsts_l015, fsts_l095, fsts_l015_125km3_l100_pb, fsts_l095_125km3_l000_pb
- `reference`: REF_WIND01 or REF_WAVE01
- `SS{###}`: Sea State identifier (SS001, SS002, etc.)
- `#`: Strut number (1-8)

## Key Distinction

- **REF_*** files = Reference/calibration data (baseline measurements)
- **SS###** files = Project/field conditions (actual sea states to analyze)

## Verification Checklist

- [ ] All SS conditions have both wind and wave reference files
- [ ] Reference files are clearly marked with REF_ prefix
- [ ] Each configuration has 8 strut files (Strut1-8)
- [ ] Scaling factors match the formula calculations
- [ ] Wind reference uses REF_WIND01 (10 m/s baseline)
- [ ] Wave reference uses REF_WAVE01 (0.5 m Hs baseline)
