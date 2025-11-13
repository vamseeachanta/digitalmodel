# Analysis Models Directory

## Purpose

This directory contains **complete, ready-to-run OrcaFlex analysis models** for CALM buoy operability and design studies. Each model file references shared base_files and direction/return-period-specific environment files.

## Structure

```
analysis_models/
├── NSE_CALM_001_000deg_1yr_simple.yml
├── NSE_CALM_001_000deg_1yr_discretised.yml
├── NSE_CALM_001_030deg_1yr_simple.yml
...
├── NSE_CALM_001_330deg_100yr_simple.yml
├── NSE_CALM_001_330deg_100yr_discretised.yml
└── README.md
```

**Total: 73 files** (72 model files + 1 README)
- 36 Simple model files (12 directions × 3 return periods)
- 36 Discretised model files (12 directions × 3 return periods)

## File Naming Convention

```
NSE_CALM_001_<direction>deg_<return_period>_<model_type>.yml
```

**Components**:
- `NSE_CALM_001`: Project identifier (North Sea Example - CALM Buoy 001)
- `direction`: 000, 030, 060, 090, 120, 150, 180, 210, 240, 270, 300, 330
- `return_period`: 1yr, 10yr, 100yr
- `model_type`: simple, discretised

**Examples**:
- `NSE_CALM_001_000deg_1yr_simple.yml` - Simple model, 0° wave direction, 1-year conditions
- `NSE_CALM_001_090deg_10yr_discretised.yml` - Discretised model, 90° direction, 10-year conditions
- `NSE_CALM_001_180deg_100yr_simple.yml` - Simple model, 180° direction, 100-year conditions

## Model Types

### Simple Model
- **Description**: Single vessel representation of entire CALM buoy
- **Use Case**: Fast operability analysis, preliminary design
- **Features**:
  - Single 6DOF vessel object
  - Simplified hydrodynamics
  - Faster computation
- **File Count**: 36 files (12 directions × 3 return periods)

### Discretised Model
- **Description**: Detailed representation with 8 individual buoy segments
- **Use Case**: Detailed design, validation, regulatory compliance
- **Features**:
  - 8 × 6DOF buoy objects
  - Distributed hydrodynamic loads
  - Higher fidelity results
- **File Count**: 36 files (12 directions × 3 return periods)

## Return Periods

### 1-Year (Operability)
- **Wave**: Hs=2.5m, Tz=5.3s
- **Current**: 1.4 m/s surface
- **Wind**: 20 m/s
- **Purpose**: Operability limit assessment (API RP 2SK)
- **Files**: 24 files (12 directions × 2 model types)

### 10-Year (Design)
- **Wave**: Hs=5.0m, Tz=7.8s
- **Current**: 1.9 m/s surface
- **Wind**: 30 m/s
- **Purpose**: Design load conditions
- **Files**: 24 files (12 directions × 2 model types)

### 100-Year (Extreme)
- **Wave**: Hs=8.5m, Tz=9.9s
- **Current**: 2.3 m/s surface
- **Wind**: 37 m/s
- **Purpose**: Ultimate limit state (ULS) checks
- **Files**: 24 files (12 directions × 2 model types)

## File Structure

Each analysis model file contains:

```yaml
%YAML 1.1
# Type: Model
# Program: OrcaFlex
# Analysis Model: CALM Buoy - <direction>° Direction, <return_period> Return Period
# Model Type: <model_type>
---
General:
  - includefile: ../base_files/_01a_units_analysis.yml
  - includefile: ../base_files/_01b_statics.yml
  - includefile: ../base_files/_01c_dynamics.yml
  - includefile: ../base_files/_01d_stages.yml
  - includefile: ../base_files/_01e_view.yml

VariableData:
  - includefile: ../base_files/_02_variable_data.yml

Environment:
  - includefile: ../base_files/_03a_sea_density.yml
  - includefile: ../base_files/_03b_seabed.yml
  - includefile: ../base_files/env/waves_<direction>deg_<return_period>.yml
  - includefile: ../base_files/env/current_<direction>deg_<return_period>.yml
  - includefile: ../base_files/env/wind_<direction>deg_<return_period>.yml

VesselTypes:
  - includefile: ../base_files/_04_vessel_types.yml

LineTypes:
  - includefile: ../base_files/_05_line_types.yml

Vessels:
  - includefile: ../base_files/_06_vessels_buoys.yml

[Model-specific sections: 6DBuoys for discretised, Lines, Groups]
```

## Usage in OrcaFlex

### Option 1: Direct Load
1. Open OrcaFlex
2. File → Load → Select analysis model file
3. Run simulation

### Option 2: Batch Processing
Use OrcaFlex Python API for batch runs:

```python
import OrcFxAPI

# Load and run model
model = OrcFxAPI.Model('NSE_CALM_001_090deg_10yr_simple.yml')
model.RunSimulation()
model.SaveData('results_090deg_10yr.dat')
```

## Dependencies

Each analysis model requires:

### Base Files (../base_files/)
- `_01a_units_analysis.yml` - Units definition
- `_01b_statics.yml` - Static analysis settings
- `_01c_dynamics.yml` - Dynamic analysis settings
- `_01d_stages.yml` - Analysis stages
- `_01e_view.yml` - Visualization settings
- `_02_variable_data.yml` - Variable data configuration
- `_03a_sea_density.yml` - Seawater properties
- `_03b_seabed.yml` - Seabed definition
- `_04_vessel_types.yml` - Vessel type definitions
- `_05_line_types.yml` - Line type definitions
- `_06_vessels_buoys.yml` - Vessel/buoy instances
- `_06_buoys_discretised.yml` - Discretised buoy instances (discretised only)
- `_07_lines.yml` or `_07_lines_discretised.yml` - Line instances
- `_08_groups.yml` or `_08_groups_discretised.yml` - Object groups

### Environment Files (../base_files/env/)
- `waves_<direction>deg_<return_period>.yml` - JONSWAP wave spectrum
- `current_<direction>deg_<return_period>.yml` - Current profile
- `wind_<direction>deg_<return_period>.yml` - NPD wind spectrum

## Benefits

✅ **No duplication** - All models share base_files (single source of truth)
✅ **Systematic naming** - Easy to find and organize
✅ **Flat structure** - No nested directories, simple navigation
✅ **Industry standard** - Follows McDermott Woodfibre FST convention
✅ **Ready to run** - Each file is complete and self-contained
✅ **Easy maintenance** - Update base_files once, affects all models
✅ **Version control friendly** - Track changes at model and component level

## Comparison with Alternative Approaches

### This Approach (Modular with Flat Structure)
```
analysis_models/NSE_CALM_001_090deg_10yr_simple.yml
  → references ../base_files/_01a_units_analysis.yml
  → references ../base_files/env/waves_090deg_10yr.yml
```
✅ No duplication
✅ Easy to update
✅ Flat file structure
✅ Industry standard

### Alternative 1 (Monolithic Files)
```
models/calm_buoy_090deg_10yr.yml (all-in-one, 1000+ lines)
```
❌ Massive duplication
❌ Hard to maintain
❌ Cannot share components

### Alternative 2 (Deep Hierarchy)
```
models/10yr/design/090deg/calm_buoy.yml
```
❌ Hard to navigate
❌ Complex paths
❌ Not industry standard

## Example: 12-Direction Operability Study

For complete operability analysis:

1. **Select return period**: 1-year (operability)
2. **Select model type**: Simple (faster)
3. **Run all 12 directions**:
   - NSE_CALM_001_000deg_1yr_simple.yml
   - NSE_CALM_001_030deg_1yr_simple.yml
   - NSE_CALM_001_060deg_1yr_simple.yml
   - ...
   - NSE_CALM_001_330deg_1yr_simple.yml

4. **Post-process results**:
   - Extract maximum motions per direction
   - Compare against operability limits
   - Generate polar plots

## Example: Design Verification

For complete design verification:

1. **Simple models for all conditions** (fast screening):
   - 12 directions × 3 return periods = 36 runs

2. **Discretised models for critical cases**:
   - Worst 3 directions from simple analysis
   - All 3 return periods
   - = 9 detailed runs

## References

- **Project**: North Sea CALM Buoy - Example Project (NSE_CALM_001)
- **Metocean Source**: NORA10 hindcast (Norwegian Meteorological Institute)
- **Standards**:
  - API RP 2SK 2005 (Operability)
  - DNVGL-OS-E403 2021 (CALM systems)
  - ISO 19901-7 2013 (Stationkeeping)
- **Structure Based On**: McDermott Woodfibre FST project convention
- **Base Files**: `../base_files/` (19 modules + 108 environment files)

---

*Last updated: 2025-11-11*
*Structure: Flat file naming with modular references*
*Total models: 72 (36 simple + 36 discretised)*
