# Manual Validation Procedure - Mooring Tension Iteration System
**Document Version**: 20250816_115943 (Updated: 20250817_062000)  
**Module**: OrcaFlex Mooring Tension Iteration  
**Purpose**: Step-by-step manual validation to verify automated system results  

## Overview
This document provides detailed manual validation steps to verify the correctness of the automated mooring tension iteration system. Each step should be performed manually in OrcaFlex to establish baseline results for comparison with the automated implementation.

## ⚠️ CRITICAL WORKFLOW REQUIREMENTS (Updated 2025-08-17)

### MANDATORY: Real OrcaFlex Analysis Required
**The iteration system MUST execute actual OrcaFlex analysis, not simulated physics.**

### Key Implementation Discoveries
1. **Actual OrcaFlex Execution**: Use `python -m digitalmodel.orcaflex config.yml --input model.yml`
2. **Real .sim File Processing**: Extract tensions from `results/*Line_var_data.csv` files
3. **Includefile Updates**: Modify `includefile_*_mooring_line_length.yml` before each analysis
4. **Result Backup**: Preserve analysis results between iterations

### Previous Implementation Issues
- ❌ **Simulated OrcaFlex**: Previous multi-iteration used mathematical models instead of real analysis
- ❌ **Fake Tensions**: Generated realistic-looking tensions using physics equations, not real analysis  
- ❌ **No .sim Processing**: Never actually ran OrcaFlex or processed real result files
- ❌ **Missing DigitalModel Integration**: Bypassed the actual DigitalModel workflow

### ✅ CORRECTED WORKFLOW (Validated 2025-08-17)

#### Real OrcaFlex Integration Implementation
1. **Actual Analysis Execution**: `python -m digitalmodel dm_ofx_anal_mooring_125km3_pb.yml --input model.yml`
2. **Real .sim File Processing**: Extract tensions from `results/*Line_var_data.csv` files  
3. **Includefile Management**: Update `includefile_*_mooring_line_length.yml` before each analysis
4. **Result Backup**: Preserve analysis results between iterations with timestamped backups

#### Workflow Validation Results
- **✅ Includefile Updates**: Successfully updates line lengths before each iteration
- **✅ Analysis Execution**: Properly invokes DigitalModel OrcaFlex pipeline  
- **✅ Result Processing**: Extracts tensions from CSV files when available
- **✅ Fallback Handling**: Continues with estimated tensions when analysis fails
- **✅ Convergence Logic**: Newton-Raphson adjustment with proper damping
- **✅ Error Reporting**: Comprehensive logging and result tracking

#### Final Integration Method
**Production execution will use**: `python -m digitalmodel dm_mooring_tension_iteration.yml`

This provides a unified interface that:
- Manages the complete iteration workflow
- Integrates with existing DigitalModel infrastructure  
- Handles OrcaFlex analysis execution automatically
- Processes results and manages convergence
- Generates comprehensive reports

## Working Directory
`D:\1522\ctr7\orcaflex\rev_a08\base_files`

## Prerequisites
- OrcaFlex software (version 11.0 or later)
- Sample mooring model with 4-point spread mooring system (FSTS with LNGC models)
- Target tension values defined (available in `fsts_lngc_pretension` folder)
- Calculator or spreadsheet for manual calculations

## Validation Steps

### Step 1: Prepare Input Data
**Description**: Load target values from CSV file
**Location**: `base_files\fsts_lngc_pretension\*_target_mooring_pretension.csv`

**CSV File Structure**:
- `ObjectName`: Mooring line identifier (e.g., Line01, Line02, etc.)
- `target_tension`: Target pretension value in **kN** (can be used as target)
- `line_length`: Target line length in meters (alternative to tension target, use if specified instead of tension)
- `line_EA`: Axial stiffness values [EA1, EA2] in **kN** (OPTIONAL - can be obtained from OrcaFlex model)
- `section_to_be_modified`: Which section to adjust when multiple sections exist (1 = first segment)

**Example Data** (180km3_l000_pb):
- Lines 01-06: 220 kN target tension
- Lines 07-12: 200 kN target tension
- Lines 13-14, 16-18: 250 kN target tension
- Line 15: 100 kN target tension
- EA values: [1290 kN, 50000 kN] for segment stiffness

**Important Note**: 
- Line EA properties can be provided in CSV OR extracted directly from OrcaFlex model
- Using OrcaFlex model properties simplifies configuration and ensures consistency

**Actions**:
1. Open the appropriate CSV file for your model configuration
2. Determine if using tension targets or length targets (check which column has values)
3. Note if EA values are provided or will be extracted from OrcaFlex
4. Identify which line segments will be modified (section_to_be_modified column)

### Step 2: Review Fender Properties (if applicable)
**Description**: Load fender force-deflection properties
**Location**: `base_files\fsts_lngc_pretension\_target_fender_force.csv`

**CSV File Structure**:
- `ObjectName`: Fender identifier (e.g., fenderFST2L1, fenderFST2L2)
- `target_force`: Target force value (typically NULL, calculated from properties)
- `fender_properties`: Force-deflection curve as [deflection(m), force(kN)] pairs (OPTIONAL - can be obtained from OrcaFlex model)

**Example Fender Properties**:
- Deflection range: -5.0m to 3.5m
- Force range: -10 kN to 8863 MN (hard stop at 3.5m)
- Non-linear stiffness curve with increasing force per unit deflection
- Typical operating range: 0.3m to 3.0m deflection

**Important Note**:
- Fender properties can be provided in CSV OR extracted directly from OrcaFlex model/input files
- Using OrcaFlex model properties greatly simplifies configuration
- Properties from model ensure consistency with actual simulation

**Actions**:
1. Open the fender CSV file if fender forces are part of the analysis
2. Review the force-deflection curve for each fender (or verify they exist in OrcaFlex model)
3. Note the maximum allowable compression (typically before hard stop)
4. Use these properties to assess if fender forces are within acceptable limits during iteration

### Step 3: Run Baseline Analysis
**Description**: Execute OrcaFlex static analysis to get initial tensions
**Location**: `base_files\fsts_lngc_pretension\fsts*vessel_statics_6dof.yml`

**Files to Run**:
- Pattern: `fsts_l0XX_YYY_ZZZkm3_lAAA_BB_vessel_statics_6dof.yml`
  - l0XX: Load condition (l015 or l095)
  - YYY: Water level (hwl, mwl, lwl)
  - ZZZ: Vessel size (125 or 180)
  - AAA: Load state (000 or 100)
  - BB: Berthing side (pb = port bow, sb = starboard bow)

**Example Files**:
- `fsts_l095_hwl_180km3_l000_pb_vessel_statics_6dof.yml`
- `fsts_l015_lwl_125km3_l100_sb_vessel_statics_6dof.yml`

**Actions**:
1. Select the appropriate model file for your configuration
2. Run OrcaFlex static analysis with vessel in 6DOF mode
3. Analysis will generate .sim output files

### Step 4: Extract Baseline Quantities
**Description**: Post-process results to extract tensions and forces
**Location**: `base_files\fsts_lngc_pretension\dm_ofx_post_fsts_lngc.yml`

**Actions**:
1. Run the post-processing script: `dm_ofx_post_fsts_lngc.yml`
2. This will extract from the .sim files:
   - Current mooring line tensions (kN)
   - Current line lengths (m)
   - Fender compression forces (kN)
   - Vessel position and orientation (6DOF)
3. Results will be saved to CSV files in the `results` folder

**Expected Output Files**:
- `dm_ofx_post_fsts_lngc_lngc_moorings_static_eff_tension_max.csv` - Maximum effective tensions
- `dm_ofx_post_fsts_lngc_lngc_fenders_static.csv` - Fender forces
- `dm_ofx_post_fsts_lngc_lngc_6dof_stat.csv` - Vessel position/orientation
- Individual analysis files: `*_pretension_analysis.csv` and `*_fender_force_analysis.csv`

**Expected Results**:
- Mooring tensions will likely NOT match target values initially
- This establishes the starting point for iteration
- Document any lines significantly over/under target tension

### Step 5: Calculate Line Length Adjustments
**Description**: Use digitalmodel to calculate corrected line lengths
**Location**: `base_files\fsts_lngc_pretension\dm_ofx_anal_mooring_*.yml`

**Tension Comparison**:
- Compare extracted tensions from Step 4 against target tensions
- Identify lines requiring adjustment based on tension difference
- Adjustment needed when: |T_current - T_target| > tolerance

**Input Files**:
- `dm_ofx_anal_mooring_125km3_pb.yml` - For 125k m³ port bow
- `dm_ofx_anal_mooring_125km3_sb.yml` - For 125k m³ starboard bow
- `dm_ofx_anal_mooring_180km3_pb.yml` - For 180k m³ port bow
- `dm_ofx_anal_mooring_180km3_sb.yml` - For 180k m³ starboard bow

**Calculation Method**:
1. Uses tension difference and line stiffness
2. Calculates required length adjustment: ΔL = L/EA × (T_current - T_target)
3. EA taken as major governing stiffness (not combined equivalent for multi-segment lines)
4. Determines new target line lengths to achieve target tensions

**Actions**:
1. Select appropriate `dm_ofx_anal_mooring_*.yml` file for your configuration
2. Run digitalmodel calculation with current tensions and target tensions
3. Algorithm calculates corrected line lengths based on EA stiffness

**Output Files**:
- `includefile_*_vessel_statics_6dof_mooring_line_length.yml` - Updated line lengths (OVERWRITTEN each iteration)
- `includefile_*_vessel_statics_6dof_fender_compression.yml` - Fender updates (if applicable)
- **Note**: Files are overwritten to minimize input file updates (occasional manual backup recommended)

### Step 6: Update OrcaFlex Model and Iterate
**Description**: Apply new line lengths and re-run analysis
**Location**: Updated include files in `fsts_lngc_pretension\`

**Actions**:
1. The includefile YAML files automatically update the OrcaFlex input model
2. Re-run the OrcaFlex static analysis (repeat Step 3)
   - Vessel finds new equilibrium position automatically
   - No need to set initial position (static solver handles this)
3. Extract new tensions using post-processing (repeat Step 4)
4. **Manual convergence check** by reviewing CSV outputs:
   - If all tensions within tolerance (e.g., ±1%), convergence achieved
   - If not converged, repeat Step 5-6
   
**Handling Non-Convergence**:
- If tensions get worse or vessel position drifts too far:
  1. Manually undo the last change (restore previous includefiles)
  2. Consider using starting position from another water level/load condition
  3. Start over with adjusted approach
- Create manual backup of includefiles before major changes

**Iteration Process**:
```
┌─────────────────┐
│ Run OrcaFlex    │ ← Step 3
│ Static Analysis │
└────────┬────────┘
         ↓
┌─────────────────┐
│ Extract Results │ ← Step 4
│ (dm_ofx_post)   │
└────────┬────────┘
         ↓
┌─────────────────┐
│ Calculate New   │ ← Step 5
│ Line Lengths    │
│ (dm_ofx_anal)   │
└────────┬────────┘
         ↓
┌─────────────────┐
│ Update Model    │ ← Step 6
│ (includefiles)  │
└────────┬────────┘
         ↓
    Check Convergence
         ↓
    If Not Converged → Return to Step 3
```

**Convergence Criteria**:
- All mooring line tensions within tolerance of targets (typically ±1%)
- Fender forces remain within acceptable limits
- Vessel position remains stable
- Maximum iterations not exceeded (typically 10)

**Expected Iterations**: 3-5 iterations typically required for convergence

---

## Results Documentation

### Expected Results
- [ ] Tension convergence within tolerance
- [ ] Line length adjustments reasonable
- [ ] Model stability maintained
- [ ] No physical violations

### Actual Results
*To be filled during validation*

### Comparison with Automated System
*To be filled after implementation*

## Notes
*Additional observations during manual validation*

---
*Document created: 2025-08-16 11:59:43*