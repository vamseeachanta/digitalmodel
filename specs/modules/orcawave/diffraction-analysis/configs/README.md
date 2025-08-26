# OrcaWave Configuration Documentation

## Configuration File: `sea_cypress_diffraction.yml`

### Overview
This configuration file sets up a comprehensive hydrodynamic diffraction analysis for the Sea Cypress tug vessel using OrcaWave.

### Key Configuration Choices

#### 1. Geometry Input
- **File**: `sea_cypress_trimesh.gdf`
- **Panels**: 24,332 triangular panels
- **Format**: WAMIT GDF format
- **Validation**: Watertight mesh with 100% correct normal orientation

#### 2. Environmental Parameters
- **Water Depth**: 100m (deep water assumption)
  - Suitable for initial analysis
  - Can be adjusted for site-specific conditions
- **Water Density**: 1025 kg/m³ (standard seawater)
- **Gravity**: 9.80665 m/s²

#### 3. Frequency Range
Optimized for tug vessel response characteristics:
- **Range**: 3-25 seconds (18 periods)
- **Distribution**:
  - Dense sampling in 8-15s range (typical tug response)
  - Extended to 25s for low-frequency effects
  - Starting at 3s for high-frequency cutoff

#### 4. Wave Headings
Full directional analysis with 22.5° increments:
- 0° (head seas)
- 22.5°, 45° (bow quartering)
- 67.5°, 90° (beam seas)
- 112.5°, 135° (stern quartering)
- 157.5°, 180° (following seas)

Total: 9 headings × 18 frequencies = 162 calculations

#### 5. Solver Settings
- **Method**: Direct LU decomposition
  - Most robust for moderate panel counts
  - Suitable for 24k panels
- **Load RAO**: Both Haskind and pressure integration
- **QTF**: Full calculation enabled
  - Range: 5-20 seconds
  - Mean drift included

#### 6. Mass Properties (Preliminary)
**NOTE**: These values are estimates and should be updated with actual vessel data:
- **Displacement**: ~400 tonnes (based on 404 m³ volume)
- **Center of Mass**: [0, 0, -1.5] m (typical for tug)
- **Inertia**: Estimated based on vessel type
  - Roll: 1.6×10⁶ kg·m²
  - Pitch/Yaw: 1.5×10⁷ kg·m²

#### 7. Output Configuration
- **Formats**: Excel, CSV, and OrcaFlex YAML
- **Components**:
  - RAOs (all 6 DOF)
  - Added mass matrices
  - Damping coefficients
  - Excitation forces
  - QTF matrices (if computed)

### Required Updates Before Production Run

1. **Mass Properties**:
   - Obtain actual vessel mass
   - Update center of gravity location
   - Calculate/measure actual inertia values

2. **Site-Specific Data**:
   - Update water depth for actual location
   - Consider site-specific water density
   - Add current/wind if needed

3. **Mooring System** (if applicable):
   - Add external stiffness matrix
   - Include mooring-induced damping

### Usage

#### Dry Run (Validation Only)
```bash
cd scripts
run_analysis.bat --dry-run
```

#### Full Analysis (Requires License)
```bash
cd scripts
run_analysis.bat
```

#### Python Script
```bash
python run_sea_cypress_analysis.py [--dry-run] [--skip-license-check]
```

### Expected Outputs

1. **Hydrodynamic Coefficients**:
   - `sea_cypress_added_mass.csv`
   - `sea_cypress_damping.csv`

2. **Wave Loads**:
   - `sea_cypress_excitation.csv`
   - `sea_cypress_RAOs.xlsx`

3. **Second-Order Effects** (if QTF enabled):
   - `sea_cypress_QTF_sum.csv`
   - `sea_cypress_QTF_diff.csv`
   - `sea_cypress_mean_drift.csv`

4. **Reports**:
   - `analysis_report.json`
   - Log files in `logs/` directory

### Performance Estimates

Based on mesh size and analysis parameters:
- **Total Calculations**: 162 (18 frequencies × 9 headings)
- **Estimated Runtime**: 2-4 hours (depending on hardware)
- **Memory Requirements**: ~4-8 GB RAM
- **Disk Space**: ~500 MB for outputs

### Validation Checklist

Before running:
- [ ] GDF file exists and is valid
- [ ] Mass properties updated with actual values
- [ ] Water depth appropriate for site
- [ ] Frequency range covers expected response
- [ ] Output directory has sufficient space
- [ ] OrcaWave license available

### Troubleshooting

1. **License Issues**:
   - Verify license server connection
   - Check license expiration
   - Ensure correct license features

2. **Memory Issues**:
   - Reduce panel count if needed
   - Limit frequency/heading points
   - Use iterative solver for large problems

3. **Convergence Problems**:
   - Check mesh quality
   - Verify waterline intersection
   - Adjust solver tolerances

### References

- OrcaWave User Manual v11.5
- WAMIT Theory Manual
- DNV-RP-C205: Environmental Conditions and Environmental Loads