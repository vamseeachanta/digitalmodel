# FC Condition to Reference File Mapping
Generated: 2025-09-21 06:55:05

## Reference Conditions (Baseline)

| Reference | Type | Wind Speed | Hs | Tp | Direction | Description |
|-----------|------|------------|----|----|-----------|-------------|
| wind01 | Wind | 10 m/s | - | - | 0° | Baseline wind condition |
| wave01 | Wave | - | 0.5 m | 2.7 s | 0° | Baseline wave condition |

## FC Conditions Summary

| FC ID | Description | Wind (m/s) | Hs (m) | Tp (s) | Wind Scale | Wave Scale |
|-------|-------------|------------|--------|--------|------------|------------|
| FC001 | Test Condition | 15 | 0.75 | 4.0 | 2.25x | 1.50x |
| FC002 | Reference Baseline | 10 | 0.5 | 2.7 | 1.00x | 1.00x |
| FC003 | Low Condition | 5 | 0.25 | 2.0 | 0.25x | 0.50x |
| FC004 | High Condition | 20 | 1.0 | 5.0 | 4.00x | 2.00x |

## Detailed File Mapping

### FC001: Test Condition

**Environmental Conditions:**
- Wind: 15 m/s @ 0°
- Wave: Hs=0.75m, Tp=4.0s @ 0°
- Scaling: Wind=2.25x, Wave=1.50x

**Reference Files Used:**

| Vessel Config | Wind Reference File | Wave Reference File |
|---------------|--------------------|--------------------||
| fsts_l015 | fsts_l015_mwl_wind01_Strut[1-8].csv | fsts_l015_mwl_wave01_Strut[1-8].csv |
| fsts_l095 | fsts_l095_mwl_wind01_Strut[1-8].csv | fsts_l095_mwl_wave01_Strut[1-8].csv |
| fsts_l015_125km3_l100_pb | fsts_l015_125km3_l100_pb_mwl_wind01_Strut[1-8].csv | fsts_l015_125km3_l100_pb_mwl_wave01_Strut[1-8].csv |
| fsts_l095_125km3_l000_pb | fsts_l095_125km3_l000_pb_mwl_wind01_Strut[1-8].csv | fsts_l095_125km3_l000_pb_mwl_wave01_Strut[1-8].csv |

### FC002: Reference Baseline

**Environmental Conditions:**
- Wind: 10 m/s @ 0°
- Wave: Hs=0.5m, Tp=2.7s @ 0°
- Scaling: Wind=1.00x, Wave=1.00x

**Reference Files Used:**

| Vessel Config | Wind Reference File | Wave Reference File |
|---------------|--------------------|--------------------||
| fsts_l015 | fsts_l015_mwl_wind01_Strut[1-8].csv | fsts_l015_mwl_wave01_Strut[1-8].csv |
| fsts_l095 | fsts_l095_mwl_wind01_Strut[1-8].csv | fsts_l095_mwl_wave01_Strut[1-8].csv |
| fsts_l015_125km3_l100_pb | fsts_l015_125km3_l100_pb_mwl_wind01_Strut[1-8].csv | fsts_l015_125km3_l100_pb_mwl_wave01_Strut[1-8].csv |
| fsts_l095_125km3_l000_pb | fsts_l095_125km3_l000_pb_mwl_wind01_Strut[1-8].csv | fsts_l095_125km3_l000_pb_mwl_wave01_Strut[1-8].csv |

### FC003: Low Condition

**Environmental Conditions:**
- Wind: 5 m/s @ 0°
- Wave: Hs=0.25m, Tp=2.0s @ 0°
- Scaling: Wind=0.25x, Wave=0.50x

**Reference Files Used:**

| Vessel Config | Wind Reference File | Wave Reference File |
|---------------|--------------------|--------------------||
| fsts_l015 | fsts_l015_mwl_wind01_Strut[1-8].csv | fsts_l015_mwl_wave01_Strut[1-8].csv |
| fsts_l095 | fsts_l095_mwl_wind01_Strut[1-8].csv | fsts_l095_mwl_wave01_Strut[1-8].csv |
| fsts_l015_125km3_l100_pb | fsts_l015_125km3_l100_pb_mwl_wind01_Strut[1-8].csv | fsts_l015_125km3_l100_pb_mwl_wave01_Strut[1-8].csv |
| fsts_l095_125km3_l000_pb | fsts_l095_125km3_l000_pb_mwl_wind01_Strut[1-8].csv | fsts_l095_125km3_l000_pb_mwl_wave01_Strut[1-8].csv |

### FC004: High Condition

**Environmental Conditions:**
- Wind: 20 m/s @ 0°
- Wave: Hs=1.0m, Tp=5.0s @ 0°
- Scaling: Wind=4.00x, Wave=2.00x

**Reference Files Used:**

| Vessel Config | Wind Reference File | Wave Reference File |
|---------------|--------------------|--------------------||
| fsts_l015 | fsts_l015_mwl_wind01_Strut[1-8].csv | fsts_l015_mwl_wave01_Strut[1-8].csv |
| fsts_l095 | fsts_l095_mwl_wind01_Strut[1-8].csv | fsts_l095_mwl_wave01_Strut[1-8].csv |
| fsts_l015_125km3_l100_pb | fsts_l015_125km3_l100_pb_mwl_wind01_Strut[1-8].csv | fsts_l015_125km3_l100_pb_mwl_wave01_Strut[1-8].csv |
| fsts_l095_125km3_l000_pb | fsts_l095_125km3_l000_pb_mwl_wind01_Strut[1-8].csv | fsts_l095_125km3_l000_pb_mwl_wave01_Strut[1-8].csv |

## Scaling Formulas

- **Wind Scaling**: `(V_actual / V_reference)²` = `(V / 10)²`
- **Wave Scaling**: `Hs_actual / Hs_reference` = `Hs / 0.5`
- **Combined**: `Tension = Wind_base * Wind_scale + Wave_base * Wave_scale`

## File Naming Pattern

```
{vessel_config}_mwl_{reference}_Strut{#}.csv
```

Where:
- `vessel_config`: fsts_l015, fsts_l095, fsts_l015_125km3_l100_pb, fsts_l095_125km3_l000_pb
- `reference`: wind01 or wave01
- `#`: Strut number (1-8)

## Verification Checklist

- [ ] All FC conditions have both wind and wave reference files
- [ ] Reference files exist for all 4 vessel configurations
- [ ] Each configuration has 8 strut files (Strut1-8)
- [ ] Scaling factors match the formula calculations
- [ ] Wind reference uses 10 m/s baseline
- [ ] Wave reference uses 0.5 m Hs baseline
