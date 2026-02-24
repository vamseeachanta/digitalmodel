# OrcaFlex Go-By Structure - Example 3

## Purpose
This is a minimal go-by folder structure for OrcaFlex projects, preserving essential organizational patterns while reducing file count from ~1,800 to ~60 representative files.

## Directory Structure

```
example-3/
├── current_coeffs/          # Current force coefficients
├── env/                     # Environment data (optional - not included in go-by)
├── fsts_lngc_pretension_iterated/  # Iterated pretension results
├── moorings/                # Mooring line configurations
├── templates/               # Template files
├── winches/                 # Winch configurations
└── wind_coeffs/             # Wind force coefficients
```

## File Naming Conventions

### Numerical Prefixes (Processing Order)
- `01_` - Analysis settings (static/dynamic)
- `02_` - Environment (seabed, water, currents, waves, wind)
- `03_` - FST1 coefficients
- `04_` - FST2 coefficients  
- `05_` - Variable data (damping, stiffness)
- `06_` - Line types (struts, moorings)
- `07_` - Line configurations
- `09_` - Constraints and structures (jackets, fenders)
- `10_` - LNGC coefficients
- `12_` - Fender systems
- `13_` - Groups and include files
- `14_` - Current/wind coefficients
- `15_` - Manifold configurations

### Underscore Prefix Files
Files starting with `_` are summary/aggregate configuration files that combine multiple settings.

### Vessel Configuration Pattern
`fsts_[loading]_[water_level]_[vessel]_[berth]_[special].yml`

Where:
- **loading**: `l015` (15% loaded), `l095` (95% loaded)
- **water_level**: `hwl` (high), `mwl` (mean), `lwl` (low)
- **vessel**: `125km3`, `180km3` (vessel capacity)
- **berth**: `pb` (port), `sb` (starboard)
- **special**: `fatigue`, `pretension_initialpos`, `damage_strut`, etc.

Examples:
- `fsts_l015_hwl.yml` - Basic FSTS at 15% load, high water
- `fsts_l015_hwl_125km3_l100_pb.yml` - With 125km³ LNGC at port berth
- `fsts_l095_mwl_125km3_l000_pb_fatigue.yml` - 95% loaded, fatigue analysis

## Key File Categories

### 1. Analysis Configuration
- **Static**: `01_a_static_analysis_*.yml`
- **Dynamic**: `01_b_dynamic_analysis_*.yml`
- **Fatigue**: `*_fatigue.yml`
- **Global Settings**: `01_c_global_north.yml`

### 2. Environment Settings
- **Seabed/Water**: `02a_*.yml`
- **Currents**: `02c_currents.yml`
- **Waves**: `02d_waves.yml`
- **Wind**: `02e_wind.yml`
- **Ramp**: `02e_ramp_settings.yml`

### 3. Structural Components
- **Damping/Stiffness**: `05_*.yml`
- **Lines/Moorings**: `06_*.yml`, `07_*.yml`
- **Jackets**: `09_a_jackets_*.yml`
- **Fenders**: `09_b_fenders_*.yml`, `12_*.yml`

### 4. Vessel Configurations
- **Base FSTS**: `fsts_[loading]_[water_level].yml`
- **FSTS with LNGC**: `fsts_*_[vessel]_*.yml`
- **Special Cases**: Damage scenarios, locked jackets, sensitivity studies

### 5. Coefficients (in subdirectories)
- **Current**: `current_coeffs/` - Drag coefficients for current
- **Wind**: `wind_coeffs/` - Drag coefficients for wind
- **Format**: `[structure_id]_[c/d]_[config].yml`
  - `c` = current coefficient
  - `d` = wind (drag) coefficient

### 6. AQWA Import Files
- **Templates**: `aqwaimport_template_*.yml`
- **Specific**: `aqwaimport_fsts_*.yml`
- Used for importing hydrodynamic data from AQWA analysis

## Subdirectory Contents

### current_coeffs/ & wind_coeffs/
- Contains force coefficient files
- Naming: `[ID]_[type]_[vessel]_[direction].yml`
- Example: `03_c_fst1_f.yml` (FST1 current coefficient, forward)

### moorings/ & winches/
- Individual line configurations
- Naming: `07_c_lines_[vessel]_[config]_line[NN].yml`
- Common settings: `07_d_lines_lngc_common.yml`

### fsts_lngc_pretension_iterated/
- Results from mooring pretension iterations
- Include files for fender compression and mooring lengths
- Vessel statics configurations (3DOF, 6DOF)

### templates/
- Reusable configuration templates
- Example: `6dbuoy_template.yml`

## Usage Guidelines

1. **Starting a New Project**:
   - Copy this go-by structure
   - Modify numerical prefix files for your specific case
   - Add vessel-specific configurations as needed

2. **File Organization**:
   - Maintain numerical prefix convention
   - Use underscore prefix for summary files
   - Keep coefficients in appropriate subdirectories

3. **Naming New Files**:
   - Follow the established patterns
   - Include water level, loading, and vessel size in filename
   - Use consistent abbreviations (hwl, mwl, lwl, pb, sb)

4. **Scaling Up**:
   - This go-by contains ~60 files (vs ~1,800 in full project)
   - Add more variations as needed
   - Duplicate and modify existing patterns

## File Count Summary

| Category | Go-By Files | Full Project Files |
|----------|-------------|-------------------|
| Root YML files | ~45 | 243 |
| current_coeffs/ | 2 | 16 |
| wind_coeffs/ | 2 | 16 |
| moorings/ | 3 | 70 |
| winches/ | 3 | 123 |
| templates/ | 1 | 1 |
| fsts_lngc_pretension_iterated/ | 3 | 96 |
| **Total** | **~60** | **~900** |

## Notes

- The `env/` folder (334 files) is excluded from go-by but may contain environment data
- The `fsts_lngc_pretension/` folder (922 files) contains detailed iteration work
- This go-by preserves all organizational patterns with minimal file duplication
- Each file type and naming convention is represented with at least one example