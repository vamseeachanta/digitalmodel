# OrcaFlex Model Structure - Complete Summary

## Overview

This document summarizes the complete OrcaFlex model structure for the CALM buoy operability and design study.

**Structure Type**: Modular with flat file naming (industry standard)
**Based On**: McDermott Woodfibre FST project convention
**Total Files**: 253 files

## Directory Structure

```
projects/TEST_OPERABILITY/orcaflex/
├── base_files/                                    # 128 files
│   ├── env/                                       # 109 files (108 env + 1 README)
│   │   ├── waves_000deg_1yr.yml → waves_330deg_100yr.yml    # 36 wave files
│   │   ├── current_000deg_1yr.yml → current_330deg_100yr.yml # 36 current files
│   │   ├── wind_000deg_1yr.yml → wind_330deg_100yr.yml       # 36 wind files
│   │   └── README.md
│   ├── _01a_units_analysis.yml
│   ├── _01b_statics.yml
│   ├── _01c_dynamics.yml
│   ├── _01d_stages.yml
│   ├── _01e_view.yml
│   ├── _02_variable_data.yml
│   ├── _03a_sea_density.yml
│   ├── _03b_seabed.yml
│   ├── _03c_waves_base.yml                       # Legacy reference
│   ├── _03c_waves_jonswap_base.yml
│   ├── _03d_current_base.yml
│   ├── _03e_wind_base.yml
│   ├── _04_vessel_types.yml
│   ├── _05_line_types.yml
│   ├── _06_vessels_buoys.yml
│   ├── _06_buoys_discretised.yml
│   ├── _07_lines.yml
│   ├── _07_lines_discretised.yml
│   ├── _08_groups.yml
│   ├── _08_groups_discretised.yml
│   ├── calm_buoy_simple_base.yml                 # Main file (simple)
│   ├── calm_buoy_discretised_base.yml            # Main file (discretised)
│   ├── README.md
│   └── BASE_FILES_STRUCTURE.md
│
├── analysis_models/                               # 73 files
│   ├── NSE_CALM_001_000deg_1yr_simple.yml        # 36 simple models
│   ├── NSE_CALM_001_000deg_1yr_discretised.yml   # 36 discretised models
│   ...
│   ├── NSE_CALM_001_330deg_100yr_simple.yml
│   ├── NSE_CALM_001_330deg_100yr_discretised.yml
│   └── README.md
│
├── load_cases/                                    # 52 files (legacy)
│   ├── operability_000deg.yml → operability_330deg.yml
│   └── ...
│
└── STRUCTURE_SUMMARY.md                          # This file
```

## File Counts

| Category | Count | Description |
|----------|-------|-------------|
| **Total Files** | **253** | All OrcaFlex files |
| Base files (structural) | 19 | Shared modules (units, types, objects) |
| Base files (env legacy) | 4 | Old base environment files |
| Environment files | 108 | Standalone env files (12 dir × 3 RP × 3 types) |
| Analysis models | 72 | Ready-to-run models (12 dir × 3 RP × 2 types) |
| Load cases (legacy) | 52 | Old structure (being replaced) |

## Environment Files Breakdown

**Total: 108 environment files** (12 directions × 3 return periods × 3 environment types)

| Type | 1-Year | 10-Year | 100-Year | Total |
|------|--------|---------|----------|-------|
| Waves (JONSWAP) | 12 | 12 | 12 | **36** |
| Current (depth profile) | 12 | 12 | 12 | **36** |
| Wind (NPD spectrum) | 12 | 12 | 12 | **36** |
| **Subtotal per RP** | **36** | **36** | **36** | **108** |

## Analysis Model Files Breakdown

**Total: 72 analysis models** (12 directions × 3 return periods × 2 model types)

| Model Type | 1-Year | 10-Year | 100-Year | Total |
|------------|--------|---------|----------|-------|
| Simple (single vessel) | 12 | 12 | 12 | **36** |
| Discretised (8 buoys) | 12 | 12 | 12 | **36** |
| **Subtotal per RP** | **24** | **24** | **24** | **72** |

## Naming Conventions

### Environment Files
```
{type}_{direction}deg_{return_period}.yml
```
**Examples**:
- `waves_000deg_1yr.yml` - Wave from 0°, 1-year
- `current_180deg_10yr.yml` - Current from 180°, 10-year
- `wind_270deg_100yr.yml` - Wind from 270°, 100-year

### Analysis Model Files
```
NSE_CALM_001_{direction}deg_{return_period}_{model_type}.yml
```
**Examples**:
- `NSE_CALM_001_000deg_1yr_simple.yml` - Simple model, 0°, 1-year
- `NSE_CALM_001_090deg_10yr_discretised.yml` - Discretised, 90°, 10-year
- `NSE_CALM_001_180deg_100yr_simple.yml` - Simple, 180°, 100-year

## Metocean Conditions

Based on North Sea NORA10 hindcast and API RP 2SK standards.

| Parameter | 1-Year (Operability) | 10-Year (Design) | 100-Year (Extreme) |
|-----------|---------------------|------------------|-------------------|
| Wave Hs | 2.5 m | 5.0 m | 8.5 m |
| Wave Tz | 5.3 s | 7.8 s | 9.9 s |
| Wave Gamma | 3.3 | 3.3 | 3.3 |
| Current (surface) | 1.4 m/s | 1.9 m/s | 2.3 m/s |
| Wind (10m, 1-hr) | 20 m/s | 30 m/s | 37 m/s |

## Model Types

### Simple Model (36 files)
- **Description**: Single 6DOF vessel representing entire CALM buoy
- **Use Case**: Fast operability analysis, preliminary design
- **Computation**: ~2-5 minutes per simulation
- **Output**: Global motions and mooring tensions

### Discretised Model (36 files)
- **Description**: 8 × 6DOF buoys representing distributed system
- **Use Case**: Detailed design, validation, regulatory compliance
- **Computation**: ~10-20 minutes per simulation
- **Output**: Distributed loads and detailed motions

## Usage Workflows

### Workflow 1: Operability Assessment (API RP 2SK)

**Objective**: Determine operability limits for offloading operations

**Steps**:
1. Select 1-year return period files
2. Use simple model type (faster)
3. Run all 12 directions
4. Extract maximum motions
5. Compare against operability criteria

**Files Used**: 12 files
- NSE_CALM_001_000deg_1yr_simple.yml
- NSE_CALM_001_030deg_1yr_simple.yml
- ...
- NSE_CALM_001_330deg_1yr_simple.yml

**Total Runtime**: ~30-60 minutes (12 simulations)

### Workflow 2: Design Verification

**Objective**: Verify design meets structural requirements

**Steps**:
1. **Screening phase** (simple models):
   - Run all 72 simple models (12 dir × 3 RP)
   - Identify critical load cases
   - ~2-6 hours runtime

2. **Detailed verification** (discretised models):
   - Run critical cases with discretised models
   - Typically 3-5 directions × 3 return periods
   - ~2-3 hours runtime

**Files Used**: 72 simple + ~15 discretised = ~87 total runs

### Workflow 3: Regulatory Compliance (DNVGL-OS-E403)

**Objective**: Demonstrate compliance with class requirements

**Steps**:
1. Run all discretised models for 100-year return period
2. Extract distributed loads on all components
3. Compare against design capacities
4. Generate compliance documentation

**Files Used**: 12 files (discretised, 100-year)
- NSE_CALM_001_000deg_100yr_discretised.yml
- ...
- NSE_CALM_001_330deg_100yr_discretised.yml

**Total Runtime**: ~2-4 hours

## Key Features

### 1. No Duplication
- Base files exist once
- Environment files standalone and reusable
- Single source of truth for all parameters

### 2. Easy Maintenance
- Update base file → affects all analysis models
- Update environment file → affects all models using it
- Clear separation of concerns

### 3. Version Control Friendly
- Small, focused files
- Easy to track changes
- Merge conflicts unlikely

### 4. Industry Standard
- Flat file structure (McDermott convention)
- Clear naming conventions
- Professional organization

### 5. Scalability
- Easy to add new directions (just add env files)
- Easy to add new return periods (generate env files)
- Easy to add new model types (create analysis templates)

## Generation Scripts

All files can be regenerated using Python scripts:

### Generate Environment Files
```bash
python scripts/generate_env_files.py
```
**Output**: 108 environment files (waves, current, wind)
**Runtime**: ~2 seconds

### Generate Analysis Model Files
```bash
python scripts/generate_analysis_files.py
```
**Output**: 72 analysis model files (simple + discretised)
**Runtime**: ~1 second

## References

- **Project**: North Sea CALM Buoy - Example Project (NSE_CALM_001)
- **Metocean Source**: NORA10 hindcast (Norwegian Meteorological Institute)
- **Standards**:
  - API RP 2SK 2005 (Operability)
  - DNVGL-OS-E403 2021 (CALM systems)
  - ISO 19901-7 2013 (Stationkeeping)
  - OCIMF MEG4 2018 (Mooring Equipment Guidelines)
- **Structure Convention**: McDermott Woodfibre FST project
- **Software**: Orcina OrcaFlex 11.4 or later

## Benefits vs Traditional Approach

| Aspect | This Structure | Traditional Monolithic |
|--------|---------------|----------------------|
| File size | Small (1-5 KB) | Large (500+ KB) |
| Duplication | None (shared base) | High (repeated data) |
| Maintenance | Easy (update once) | Hard (update many) |
| Navigation | Flat (easy to find) | Deep (hard to navigate) |
| Version control | Clean diffs | Messy diffs |
| Industry alignment | ✅ Standard | ❌ Project-specific |

## Migration Path

For existing projects using monolithic files:

1. **Extract base files**: Identify common sections, create modules
2. **Create environment files**: Extract waves/current/wind to standalone files
3. **Generate analysis templates**: Create model files with includefiles
4. **Validate**: Compare results between old and new structures
5. **Document**: Update project documentation and workflows

---

*Last updated: 2025-11-11*
*Total files: 253*
*Structure: Industry-standard modular with flat naming*
*Ready for production use*
