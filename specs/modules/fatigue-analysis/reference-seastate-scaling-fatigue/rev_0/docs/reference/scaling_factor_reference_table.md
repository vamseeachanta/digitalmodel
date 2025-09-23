# Scaling Factor Reference Table

## Overview
This document provides a comprehensive mapping between input files, metadata, and scaling factors used in the fatigue analysis reference seastate scaling methodology.

## Scaling Formulas
- **Wind Scaling Factor** = (V_actual / V_reference)² where V_reference = 10 m/s
- **Wave Scaling Factor** = Hs_actual / Hs_reference where Hs_reference = 0.5 m

## Reference Seastate Files and Metadata

### Base Reference Conditions
| Reference | Type | Base Value | Direction | Period | File Pattern |
|-----------|------|------------|-----------|--------|--------------|
| wind01 | Wind | 10 m/s | 0° | N/A | `*_mwl_wind01_Strut*.csv` |
| wave01 | Wave | Hs=0.5m | 0° | Tp=2.7s | `*_mwl_wave01_Strut*.csv` |

### Vessel Configurations
| Config ID | Description | Weight Factor | LNGC Status | Files Count |
|-----------|-------------|---------------|-------------|-------------|
| fsts_l015 | FSTs Light (15% loaded) | 1.00 | None | 16 files (8 struts × 2 refs) |
| fsts_l095 | FSTs Full (95% loaded) | 2.50 | None | 16 files (8 struts × 2 refs) |
| fsts_l015_125km3_l100_pb | FSTs Light + LNGC Full | 1.75 | Full (Port) | 16 files (8 struts × 2 refs) |
| fsts_l095_125km3_l000_pb | FSTs Full + LNGC Light | 3.75 | Light (Port) | 16 files (8 struts × 2 refs) |

## Scaling Factor Examples

### Step 5 Test Condition
| Parameter | Value | Scaling Calculation | Result |
|-----------|-------|-------------------|--------|
| Wind Speed | 15 m/s | (15/10)² | 2.25 |
| Wave Hs | 0.75 m | 0.75/0.5 | 1.50 |
| Combined Effect | - | Wind + Wave | Additive |

### Common Fatigue Conditions
| FC ID | Wind (m/s) | Wave Hs (m) | Wave Tp (s) | Wind Scale | Wave Scale | Occurrence (%) |
|-------|------------|-------------|-------------|------------|------------|----------------|
| FC001 | 5 | 0.15 | 1.93 | 0.25 | 0.30 | 7.76 |
| FC002 | 10 | 0.25 | 2.70 | 1.00 | 0.50 | 5.50 |
| FC003 | 15 | 0.35 | 3.47 | 2.25 | 0.70 | 3.25 |
| FC004 | 20 | 0.50 | 3.47 | 4.00 | 1.00 | 1.50 |
| FC005 | 25 | 0.65 | 3.47 | 6.25 | 1.30 | 0.75 |

## File Structure and Data Format

### Input Files (Sample Data)
```
sample_data/
├── {config}_mwl_{reference}_Strut{#}.csv
│   ├── Columns: Time, Effective Tension at Vessel End
│   ├── Samples: 1000 (0.0 to 99.9 seconds at 0.1s intervals)
│   └── Units: Time [s], Tension [kN]
```

### Output Files (Scaled Data)
```
output/
├── {config}_FC{###}_Strut{#}.csv
│   ├── Columns: Time, Effective Tension (Scaled)
│   ├── Samples: 1000 (maintains input resolution)
│   └── Scaling: Wind component × wind_scale + Wave component × wave_scale
```

## Tension Range Mapping

### Reference Tension Ranges (from Step 3 verification)
| Configuration | Reference | Min Tension (kN) | Max Tension (kN) | Mean Tension (kN) |
|--------------|-----------|------------------|------------------|-------------------|
| fsts_l015 | wind01 | 239.6 | 792.6 | ~516.1 |
| fsts_l015 | wave01 | 75.4 | 317.1 | ~196.3 |
| fsts_l095 | wind01 | 239.6 | 792.6 | ~516.1 |
| fsts_l095 | wave01 | 75.4 | 317.1 | ~196.3 |

### Scaled Tension Example (Step 5 Test)
| Component | Scale Factor | Min (kN) | Max (kN) | Contribution |
|-----------|-------------|----------|----------|--------------|
| Wind (15 m/s) | 2.25 | 539.1 | 1783.4 | ~75% |
| Wave (Hs=0.75m) | 1.50 | 113.1 | 475.7 | ~25% |
| **Combined** | - | **912.2** | **2148.3** | **100%** |

## Processing Metadata

### LoadScaler Processing
| Step | Operation | Input | Output | Metadata Tracked |
|------|-----------|-------|--------|------------------|
| 1 | Load References | `{config}_mwl_{ref}_Strut{#}.csv` | Time series arrays | Reference names |
| 2 | Calculate Scales | Wind speed, Hs values | Scale factors | wind_scale_factor, wave_scale_factor |
| 3 | Apply Scaling | Reference tensions × scales | Scaled tensions | Applied factors |
| 4 | Combine | Scaled wind + scaled wave | Effective tension | fatigue_condition_id |
| 5 | Save | Combined array | CSV file | Full metadata dict |

### Metadata Dictionary Structure
```python
metadata = {
    'fatigue_condition': 1,              # FC ID
    'wind_reference': 'wind01',          # Reference file used
    'wave_reference': 'wave01',          # Reference file used
    'wind_scale_factor': 2.25,           # Applied wind scale
    'wave_scale_factor': 1.50,           # Applied wave scale
    'wind_speed': 15,                    # Actual wind speed [m/s]
    'hs': 0.75,                          # Actual Hs [m]
    'tp': 4.0,                           # Actual Tp [s]
    'wind_dir': 0,                       # Wind direction [deg]
    'wave_dir': 0,                       # Wave direction [deg]
    'occurrence': 1.0                    # Occurrence percentage [%]
}
```

## Quality Checks

### Scaling Validation Checks
| Check | Expected | Tolerance | Step 5 Result |
|-------|----------|-----------|---------------|
| Wind scaling formula | (V/10)² | Exact | ✅ 2.25 = (15/10)² |
| Wave scaling formula | Hs/0.5 | Exact | ✅ 1.50 = 0.75/0.5 |
| Sample count preservation | 1000 | Exact | ✅ 1000 samples |
| Tension range realistic | 100-5000 kN | Physical limits | ✅ 912-2148 kN |
| Metadata completeness | All fields | None missing | ✅ All present |

## Usage Notes

1. **File Selection**: The system automatically selects the closest reference file based on direction and (for waves) period.

2. **Scaling Application**: 
   - Wind loads scale with velocity squared (kinetic energy relationship)
   - Wave loads scale linearly with significant wave height

3. **Combination Method**: Simple addition of scaled components assumes linear superposition of forces.

4. **Data Integrity**: All operations preserve the 1000-sample structure (100 seconds at 10 Hz).

---
*Generated as part of Step 5 verification - Reference Seastate Scaling Fatigue Analysis*