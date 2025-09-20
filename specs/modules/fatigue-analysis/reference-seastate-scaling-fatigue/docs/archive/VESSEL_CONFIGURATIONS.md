# Vessel Configurations for Fatigue Analysis

## Overview
The fatigue analysis system must evaluate 4 distinct vessel configurations at the terminal, each representing different operational scenarios with unique loading conditions on the strut foundations.

## Configuration Details

### 1. FSTs Light (L015)
- **Configuration ID**: `fsts_l015`
- **Description**: Both Floating Storage Tanks (FSTs) in light/empty condition
- **FST1 Draft**: 15% loaded
- **FST2 Draft**: 15% loaded
- **LNGC**: Not present
- **Operational Scenario**: Terminal in standby/receiving mode
- **Reference Cases**: fat001 through fat036 (wave), fat037 through fat052 (wind)

### 2. FSTs Full (L095)
- **Configuration ID**: `fsts_l095`
- **Description**: Both FSTs in full/loaded condition
- **FST1 Draft**: 95% loaded
- **FST2 Draft**: 95% loaded
- **LNGC**: Not present
- **Operational Scenario**: Terminal at maximum storage capacity
- **Reference Cases**: fat019 through fat036 (wave), fat053 through fat068 (wind)

### 3. FSTs Light + LNGC Full (L015 + 125k Full)
- **Configuration ID**: `fsts_l015_lngc_125_full`
- **Description**: FSTs in light condition with full LNGC alongside
- **FST1 Draft**: 15% loaded
- **FST2 Draft**: 15% loaded
- **LNGC Partner**: 125,000 m³ (Full condition)
- **Operational Scenario**: LNGC offloading to empty FSTs
- **Reference Cases**: To be added when available

### 4. FSTs Full + LNGC Light (L095 + 125k Light)
- **Configuration ID**: `fsts_l095_lngc_125_light`
- **Description**: FSTs in full condition with light LNGC alongside
- **FST1 Draft**: 95% loaded
- **FST2 Draft**: 95% loaded
- **LNGC Partner**: 125,000 m³ (Light/ballast condition)
- **Operational Scenario**: LNGC loading from full FSTs
- **Reference Cases**: To be added when available

## Impact on Fatigue Analysis

### Load Distribution
Each configuration results in different:
- **Mooring loads**: Varying with vessel displacement and environmental loads
- **Strut tensions**: Different load paths through the 8 struts
- **Dynamic response**: Changed system natural frequencies and damping
- **Wave drift forces**: Dependent on vessel waterplane area

### Critical Considerations

1. **FST Loading States**:
   - Light condition: Lower inertia, higher accelerations
   - Full condition: Higher static loads, different natural periods

2. **LNGC Presence (Configs 3 & 4)**:
   - Additional mooring loads from LNGC transferred through struts
   - Shielding effects on wave loads to/from FSTs
   - Modified current loads due to three-vessel configuration
   - Ship-to-ship interaction forces between LNGC and FSTs
   - Combined system dynamics with different mass distributions

3. **Environmental Coupling**:
   - Each configuration responds differently to same environmental conditions
   - Critical wave periods vary with vessel draft
   - Wind loads scale with exposed area

## Analysis Requirements

### Separate Processing
Each configuration requires:
1. Independent reference seastate time traces
2. Configuration-specific FEA unit stress values
3. Separate rainflow counting and damage accumulation
4. Individual fatigue life calculations

### Output Structure
```
Configuration Results:
├── fsts_l015/                          # FSTs Light only
│   └── Min fatigue life: XX years (Strut SY)
├── fsts_l095/                          # FSTs Full only
│   └── Min fatigue life: XX years (Strut SY)
├── fsts_l015_lngc_125_full/           # FSTs Light + LNGC Full
│   └── Min fatigue life: XX years (Strut SY)
└── fsts_l095_lngc_125_light/          # FSTs Full + LNGC Light
    └── Min fatigue life: XX years (Strut SY)
```

### Design Verification
The minimum fatigue life across ALL configurations determines:
- Design compliance (must exceed design life requirement)
- Critical configuration for operations
- Maintenance/inspection priorities

## Data Requirements

### Per Configuration Needs:
- **34 reference seastates** (18 wave + 16 wind)
- **8 struts** per seastate
- **272 time traces** per configuration
- **Total**: 1,088 time trace files for complete analysis

### Current Data Availability:
- **FSTs L015**: Available (fat001-fat018 wave, fat037-fat052 wind)
- **FSTs L095**: Available (fat019-fat036 wave, fat053-fat068 wind)
- **FSTs L015 + LNGC Full**: To be generated
- **FSTs L095 + LNGC Light**: To be generated

**Note**: Sample data directory contains limited files for testing. Full production analysis requires complete time trace sets for all configurations.

### Scaling Application:
- Same 81 fatigue conditions applied to each configuration
- Configuration-specific reference traces used
- Total combinations: 4 configs × 81 conditions × 8 struts = 2,592 analyses

## Implementation Notes

1. **File Naming Convention**:
   ```
   {config_id}_{seastate}_{strut}.csv
   Examples:
   - fsts_l015_W01_S1.csv
   - lngc_180_WD16_S8.csv
   ```

2. **Processing Priority**:
   - Start with FST configurations (data available)
   - Add LNGC configurations when time traces available
   - Validate against existing manual calculations

3. **Memory Management**:
   - Process configurations sequentially to manage memory
   - Store intermediate results to disk
   - Clear arrays between configurations

4. **Parallel Processing**:
   - Parallelize within configuration (by strut)
   - Sequential between configurations
   - Estimated time: 15 minutes per configuration