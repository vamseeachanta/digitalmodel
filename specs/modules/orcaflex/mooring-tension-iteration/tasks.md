# Implementation Tasks: Mooring Tension Iteration Orchestrator

> **Total Effort**: 17 hours (~2.5 days)  
> **Scope**: Orchestration of existing batch commands with comprehensive analysis  
> **Analysis Focus**: Execution, convergence tracking, force visualization, and stiffness analysis

## Task Execution Tracker

| Task ID | Description | Status | Priority | Effort |
|---------|-------------|--------|----------|--------|
| **0.1** | Run go-by commands once to verify all files work | ✅ Complete (0.1.2 successful - .sim generated) | Critical | 1h |
| **0.2** | Document current file paths and outputs | ✅ Complete (file_documentation.md created) | Critical | 0.5h |
| **0.3** | Verify CSV format and data availability | ✅ Complete (dm_iterator.sh successful) | Critical | 0.5h |
| **1.1** | Create orchestrator.py file | ⬜ Pending | Critical | 0.5h |
| **1.2** | Implement command execution wrapper | ⬜ Pending | Critical | 0.5h |
| **1.3** | Add CSV parsing for targets and results | ⬜ Pending | Critical | 1h |
| **1.4** | Implement convergence check logic | ⬜ Pending | Critical | 1h |
| **1.5** | Add iteration loop with max limit | ⬜ Pending | Critical | 0.5h |
| **1.6** | Create summary reporting | ⬜ Pending | Critical | 0.5h |
| **2.1** | Test with go-by data files | ⬜ Pending | High | 0.5h |
| **2.2** | Verify convergence detection | ⬜ Pending | High | 0.5h |
| **2.3** | Test iteration limits | ⬜ Pending | High | 0.25h |
| **2.4** | Validate CSV parsing | ⬜ Pending | High | 0.25h |
| **2.5** | Check error handling | ⬜ Pending | High | 0.5h |
| **3.1** | Extract X/Y force components from tension data | ⬜ Pending | High | 0.5h |
| **3.2** | Create polar/radial charts showing force directions | ⬜ Pending | High | 0.75h |
| **3.3** | Generate bar charts for +X, -X, +Y, -Y forces | ⬜ Pending | High | 0.5h |
| **3.4** | Plot convergence trends per direction | ⬜ Pending | High | 0.5h |
| **3.5** | Create force balance summary chart | ⬜ Pending | High | 0.5h |
| **3.6** | Add charts to iteration reports | ⬜ Pending | High | 0.25h |
| **4.1** | Extract displacement-force relationships from data | ⬜ Pending | High | 0.75h |
| **4.2** | Calculate directional stiffness coefficients (kN/m) | ⬜ Pending | High | 0.75h |
| **4.3** | Compute restoring force curves for each direction | ⬜ Pending | High | 0.5h |
| **4.4** | Analyze stiffness symmetry/asymmetry | ⬜ Pending | High | 0.5h |
| **4.5** | Track stiffness changes during iteration | ⬜ Pending | High | 0.5h |
| **4.6** | Create stiffness visualization charts | ⬜ Pending | High | 0.5h |
| **4.7** | Generate stiffness analysis report | ⬜ Pending | High | 0.5h |
| **5.1** | Write usage documentation | ⬜ Pending | Medium | 0.5h |
| **5.2** | Create example run instructions | ⬜ Pending | Medium | 0.25h |
| **5.3** | Document CSV format requirements | ⬜ Pending | Medium | 0.25h |
| **5.4** | Add command-line argument parsing | ⬜ Pending | Medium | 0.25h |
| **5.5** | Create batch/shell wrapper scripts | ⬜ Pending | Medium | 0.25h |
| **5.6** | Document chart interpretation guide | ⬜ Pending | Medium | 0.25h |
| **5.7** | Add stiffness analysis documentation | ⬜ Pending | Medium | 0.25h |

**Legend**: ⬜ Pending | 🔄 In Progress | ✅ Complete | ❌ Blocked

## Execution Notes

### Task 0.1 Status Update (2025-08-30)
- **Subtask 0.1.1**: Initially unsuccessful - Missing .sim file prevented tension analysis
- **Subtask 0.1.2**: ✅ **SUCCESSFUL** - Enhanced run_models_to_sim.py to support .dat files
  - Added `dat=true` parameter for processing .dat files
  - Simplified command syntax (removed redundant `all=true`)
  - User manually ran: `python run_models_to_sim.py dat=true`
  - Successfully generated: `fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.sim` (45MB)
- **Impact**: Workflow unblocked - .sim file now available for post-processing
- **Next Steps**: Proceed with Tasks 0.2 and 0.3 to complete validation phase

### Task 0.2 Status Update (2025-08-30)
- **Completed**: Created comprehensive file documentation in `file_documentation.md`
- **Documented**: All input files, output files, scripts, and configurations
- **Mapped**: Complete file flow through the workflow with critical/optional designations
- **Identified**: Critical files required for iteration vs optional enhancement files
- **Next Steps**: Proceed with Task 0.3 to verify CSV formats and data availability

### Task 0.3 Status Update (2025-08-31)
- **Completed**: All steps in `dm_iterator.sh` executed successfully
- **Verified**: Complete workflow from .dat → .sim → post-processing → CSV outputs
- **Confirmed**: CSV formats and data availability validated through successful execution
- **Impact**: Phase 0 validation complete - ready to proceed with Phase 1 orchestrator implementation

### dm_iterator.sh Execution Summary (2025-08-31)
**All 5 steps executed successfully:**
1. **Tension Calculation**: YAML configuration updated with new tension values
2. **.dat to .sim Conversion**: 43MB .dat → 45MB .sim file generated
3. **Post-processing**: Tension analysis CSV files created
4. **Collation**: Results consolidated using AssetUtilities
5. **Visualization**: Visual outputs generated in output/visual/ directory

**Key Files Validated:**
- Input: `fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat` (43MB)
- Intermediate: `fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.sim` (45MB)
- Output: CSV tension analysis files in `results/` directory
- Visuals: Generated charts in `output/visual/` directory

### Recommended Execution Order:
1. **Phase 0 - Validation (Tasks 0.1-0.3)**: Verify go-by files work
2. **Phase 1 - Core (Tasks 1.1-1.6)**: Build the basic orchestrator
3. **Phase 2 - Testing (Tasks 2.1-2.5)**: Validate core functionality
4. **Phase 3 - Analysis** (Optional, can be done in parallel):
   - **Option A**: Force Visualization (Tasks 3.1-3.6)
   - **Option B**: Stiffness Analysis (Tasks 4.1-4.7)
5. **Phase 4 - Documentation (Tasks 5.1-5.7)**: Complete after implementation

### Critical Path:
- Tasks 0.1 → 0.2 → 0.3 must complete before any implementation
- Tasks 1.1 → 1.2 → 1.3 → 1.4 → 1.5 → 1.6 must be done sequentially
- Tasks 2.1-2.5 can be done after Task 1.6
- Tasks 3.x and 4.x can be developed independently after Task 1.3
- Task 5.x can start anytime but should finish last

### Task Dependencies:
- **Task 1.1** requires successful completion of Tasks 0.1-0.3
- **Task 1.3** requires Task 0.3 (CSV format verification)
- **Task 3.1** requires CSV parsing from Task 1.3
- **Task 4.1** requires CSV parsing from Task 1.3
- **Tasks 3.6 & 4.7** require iteration loop from Task 1.5
- **Task 5.6** requires Tasks 3.x complete
- **Task 5.7** requires Tasks 4.x complete

## Single Phase Implementation

### Task 0: Go-By Validation and Baseline
**Effort**: 2 hours  
**Priority**: Critical  
**Dependencies**: None

**Description**: Manually run existing batch commands once to ensure all files are present and working before automation.

**Go-By Directory Contents**:
```
specs/modules/orcaflex/mooring-tension-iteration/go-by/
├── dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml
├── dm_ofx_post_fsts_lngc.yml  
├── fsts_l015_125km3_pb_target_mooring_pretension.csv
├── fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat
└── results/
    └── (output files will be generated here)
```

**Commands to Execute**:
```bash
cd specs/modules/orcaflex/mooring-tension-iteration/go-by

# Step 1: Run tension calculation
python -m digitalmodel dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml

# Step 2: Run OrcaFlex analysis  
python -m digitalmodel.orcaflex.universal \
    pattern="fsts*125km3*pb_*.yml" \
    input_directory="." \
    output_directory="." \
    validate=false

# Step 3: Post-process results
python -m digitalmodel dm_ofx_post_fsts_lngc.yml --workers 30
```

**Subtasks**:
- [x] 0.1: Run go-by commands once to verify all files work
  **Input Files**:
  - `go-by/scripts/dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml` - Configuration for tension calculation
  - `go-by/scripts/fsts_l015_125km3_pb_target_mooring_pretension.csv` - Target mooring pretension values (16 lines)
  - `go-by/.dat/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat` - OrcaFlex model file (43MB)
  - `go-by/scripts/dm_ofx_post_fsts_lngc.yml` - Post-processing configuration
  - `go-by/scripts/au_collate.yml` - AssetUtilities collation config
  - `go-by/scripts/viz.yml` - Visualization configuration
  **Output Files**:
  - `go-by/scripts/results/dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml` - Updated YAML with calculated tensions
  - `go-by/.sim/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.sim` - OrcaFlex simulation file (45MB)
  - `go-by/output/.csv/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv` - Tension analysis results
  - `go-by/output/.csv/dm_ofx_post_fsts_lngc_lngc_moorings_static_eff_tension_max.csv` - Maximum effective tensions
  - `go-by/output/.csv/dm_ofx_post_fsts_lngc_lngc_fenders_static.csv` - Fender force analysis
  - `go-by/output/.csv/dm_ofx_post_fsts_lngc_lngc_6dof_stat.csv` - 6DOF vessel statistics
  - `go-by/output/collate/pretension_analysis_summary.xlsx` - Consolidated Excel summary
  - `go-by/output/visual/*.png` - Visual charts and plots
  - [x] 0.1.1: **Step 1 - Tension Calculation** (Analyzes existing .sim file when present)
    - **Command**: `/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml`
    - **REQUIRED Visualization Command** (must be executed after tension calculation):
      ```python
      # Quick force visualization from pretension_analysis.csv
      python -c "
      import pandas as pd
      import matplotlib.pyplot as plt
      df = pd.read_csv('output/.csv/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv')
      fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))
      
      # X-Y Force Components
      ax1.barh(df['ObjectName'], df['end_Gx_force'], alpha=0.7, label='X Force')
      ax1.barh(df['ObjectName'], df['end_Gy_force'], alpha=0.7, label='Y Force')
      ax1.set_xlabel('Force (kN)')
      ax1.set_title('Mooring Line Force Components')
      ax1.legend()
      ax1.grid(True, alpha=0.3)
      ax1.axvline(x=0, color='black', linewidth=0.5)
      
      # Vessel Total Forces
      total_x = df['end_Gx_force'].sum()
      total_y = df['end_Gy_force'].sum()
      ax2.arrow(0, 0, total_x/10, 0, head_width=5, head_length=2, fc='blue', ec='blue', label=f'X: {total_x:.1f} kN')
      ax2.arrow(0, 0, 0, total_y/10, head_width=5, head_length=2, fc='red', ec='red', label=f'Y: {total_y:.1f} kN')
      ax2.set_xlim(-150, 150)
      ax2.set_ylim(-150, 150)
      ax2.set_xlabel('X Force / 10 (kN)')
      ax2.set_ylabel('Y Force / 10 (kN)')
      ax2.set_title('Vessel Net Force Vector')
      ax2.grid(True, alpha=0.3)
      ax2.legend()
      ax2.set_aspect('equal')
      
      plt.tight_layout()
      plt.savefig('output/visual/mooring_force_components.png', dpi=150)
      plt.show()
      print(f'Total X Force: {total_x:.2f} kN')
      print(f'Total Y Force: {total_y:.2f} kN')
      "
      ```
    - **REQUIRED Mooring Stiffness Calculation Command** (must be executed after tension calculation):
      ```python
      # Calculate mooring stiffness in X and Y directions
      python -c "
      import pandas as pd
      import numpy as np
      
      # Read the pretension analysis data
      df = pd.read_csv('output/.csv/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv')
      
      # Extract line properties (EA values in kN)
      # Parse the line_EA column which contains lists like '[1170.0, 47700.0]'
      df['EA1'] = df['line_EA'].str.extract(r'\[(\d+\.?\d*),')[0].astype(float)
      df['EA2'] = df['line_EA'].str.extract(r',\s*(\d+\.?\d*)\]')[0].astype(float)
      
      # Parse line lengths to get total length
      df['L1'] = df['line_length'].str.extract(r'\[(\d+\.?\d*),')[0].astype(float)
      df['L2'] = df['line_length'].str.extract(r',\s*np\.float64\((\d+\.?\d*)\)')[0].fillna(
          df['line_length'].str.extract(r',\s*(\d+\.?\d*)\]')[0]
      ).astype(float)
      df['L_total'] = df['L1'] + df['L2']
      
      # Calculate axial stiffness for each line (EA/L)
      # For two-segment lines: 1/k_total = 1/k1 + 1/k2
      df['k1'] = df['EA1'] / df['L1']  # kN/m for segment 1
      df['k2'] = df['EA2'] / df['L2']  # kN/m for segment 2
      df['k_axial'] = 1 / (1/df['k1'] + 1/df['k2'])  # Combined axial stiffness
      
      # Get force components and tension
      df['Fx'] = df['end_Gx_force']  # X component of force
      df['Fy'] = df['end_Gy_force']  # Y component of force
      df['T'] = df['current_tension']  # Total tension
      
      # Calculate Z component using Pythagorean theorem
      # Since T² = Fx² + Fy² + Fz², we get Fz = sqrt(T² - Fx² - Fy²)
      df['Fz'] = np.sqrt(np.maximum(0, df['T']**2 - df['Fx']**2 - df['Fy']**2))
      
      # Calculate direction cosines (force components normalized by tension)
      # Small angle assumption: line orientation doesn't change significantly
      df['cos_x'] = df['Fx'] / df['T']  # X direction cosine
      df['cos_y'] = df['Fy'] / df['T']  # Y direction cosine
      df['cos_z'] = df['Fz'] / df['T']  # Z direction cosine
      
      # Calculate stiffness contributions in X, Y, and Z directions
      # k_i = k_axial * cos²(θ_i) for each direction
      df['k_x'] = df['k_axial'] * (df['cos_x'] ** 2)  # X stiffness contribution
      df['k_y'] = df['k_axial'] * (df['cos_y'] ** 2)  # Y stiffness contribution
      df['k_z'] = df['k_axial'] * (df['cos_z'] ** 2)  # Z stiffness contribution
      
      # Calculate cross-coupling terms for full 3x3 stiffness matrix
      df['k_xy'] = df['k_axial'] * df['cos_x'] * df['cos_y']  # X-Y coupling
      df['k_xz'] = df['k_axial'] * df['cos_x'] * df['cos_z']  # X-Z coupling
      df['k_yz'] = df['k_axial'] * df['cos_y'] * df['cos_z']  # Y-Z coupling
      
      # Calculate total system stiffness (sum of all line contributions)
      K_xx_total = df['k_x'].sum()   # Total X stiffness
      K_yy_total = df['k_y'].sum()   # Total Y stiffness
      K_zz_total = df['k_z'].sum()   # Total Z stiffness
      K_xy_total = df['k_xy'].sum()  # Total X-Y coupling
      K_xz_total = df['k_xz'].sum()  # Total X-Z coupling
      K_yz_total = df['k_yz'].sum()  # Total Y-Z coupling
      
      # Display results
      print('\\n=== MOORING LINE STIFFNESS ANALYSIS (3D) ===')
      print('\\nIndividual Line Stiffness Contributions:')
      print('-' * 100)
      print(f\"{'Line':<8} {'k_axial':<10} {'k_x':<10} {'k_y':<10} {'k_z':<10} {'k_xy':<10} {'k_xz':<10} {'k_yz':<10}\")
      print(f\"{'    ':<8} {'(kN/m)':<10} {'(kN/m)':<10} {'(kN/m)':<10} {'(kN/m)':<10} {'(kN/m)':<10} {'(kN/m)':<10} {'(kN/m)':<10}\")
      print('-' * 100)
      
      for idx, row in df.iterrows():
          print(f\"{row['ObjectName']:<8} {row['k_axial']:>9.2f} {row['k_x']:>9.2f} {row['k_y']:>9.2f} {row['k_z']:>9.2f} {row['k_xy']:>9.2f} {row['k_xz']:>9.2f} {row['k_yz']:>9.2f}\")
      
      print('-' * 100)
      print(f\"{'TOTAL':<8} {' ':<10} {K_xx_total:>9.2f} {K_yy_total:>9.2f} {K_zz_total:>9.2f} {K_xy_total:>9.2f} {K_xz_total:>9.2f} {K_yz_total:>9.2f}\")
      
      print('\\n=== SYSTEM STIFFNESS MATRIX (3x3) ===')
      print('     [K] = | K_xx  K_xy  K_xz |')
      print('           | K_xy  K_yy  K_yz |')
      print('           | K_xz  K_yz  K_zz |')
      print()
      print(f'K_xx = {K_xx_total:>8.2f} kN/m  (Surge stiffness)')
      print(f'K_yy = {K_yy_total:>8.2f} kN/m  (Sway stiffness)')
      print(f'K_zz = {K_zz_total:>8.2f} kN/m  (Heave stiffness)')
      print(f'K_xy = {K_xy_total:>8.2f} kN/m  (Surge-Sway coupling)')
      print(f'K_xz = {K_xz_total:>8.2f} kN/m  (Surge-Heave coupling)')
      print(f'K_yz = {K_yz_total:>8.2f} kN/m  (Sway-Heave coupling)')
      
      # Natural periods estimation (assuming vessel mass ~ 10000 tonnes for LNGC)
      M_vessel = 10000  # tonnes (typical LNGC mass)
      T_x = 2 * np.pi * np.sqrt(M_vessel / K_xx_total) if K_xx_total > 0 else np.inf
      T_y = 2 * np.pi * np.sqrt(M_vessel / K_yy_total) if K_yy_total > 0 else np.inf
      T_z = 2 * np.pi * np.sqrt(M_vessel / K_zz_total) if K_zz_total > 0 else np.inf
      
      print('\\n=== ESTIMATED NATURAL PERIODS ===')
      print(f'T_surge = {T_x:>6.1f} s  (X-direction)')
      print(f'T_sway  = {T_y:>6.1f} s  (Y-direction)')
      print(f'T_heave = {T_z:>6.1f} s  (Z-direction)')
      
      # Save stiffness results to CSV
      stiffness_df = df[['ObjectName', 'k_axial', 'k_x', 'k_y', 'k_z', 'k_xy', 'k_xz', 'k_yz', 'cos_x', 'cos_y', 'cos_z']]
      stiffness_df.to_csv('output/.csv/mooring_stiffness_analysis.csv', index=False)
      
      # Create summary dictionary
      summary = {
          'K_xx_total': K_xx_total,
          'K_yy_total': K_yy_total,
          'K_zz_total': K_zz_total,
          'K_xy_total': K_xy_total,
          'K_xz_total': K_xz_total,
          'K_yz_total': K_yz_total,
          'T_surge': T_x,
          'T_sway': T_y,
          'T_heave': T_z
      }
      
      # Save summary to CSV
      pd.DataFrame([summary]).to_csv('output/.csv/mooring_stiffness_summary.csv', index=False)
      print('\\nStiffness analysis saved to: output/.csv/mooring_stiffness_analysis.csv')
      print('Summary saved to: output/.csv/mooring_stiffness_summary.csv')
      "
      ```
    - **Input Files**:
      - `go-by/scripts/dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml` - Main configuration file
      - `go-by/scripts/fsts_l015_125km3_pb_target_mooring_pretension.csv` - Target pretension values (16 lines)
      - `go-by/.sim/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.sim` - Existing simulation file (if present, generates CSVs)
    - **Status**: ✅ SUCCESS
    - **Output Files**:
      - `go-by/scripts/results/dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml` - Updated YAML with calculated tensions
      - `go-by/output/.csv/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv` - **PRIMARY OUTPUT: Mooring pretension analysis with targets vs actuals**
        - Key columns: `current_tension`, `end_Gx_force`, `end_Gy_force`
        - Vessel total: X = +10.16 kN (balanced), Y = -1205.63 kN (southward)
      - `go-by/output/.csv/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_fender_force_analysis.csv` - **Fender force analysis results**
      - `go-by/output/.csv/dm_ofx_anal_mooring_fsts_l015_125km3_pb_General_var_data.csv` - General simulation variables
      - `go-by/output/.csv/dm_ofx_anal_mooring_fsts_l015_125km3_pb_Vessel_var_data.csv` - Vessel variable data
      - `go-by/output/.csv/dm_ofx_anal_mooring_fsts_l015_125km3_pb_Line_var_data.csv` - Mooring line variable data
      - `go-by/output/.csv/dm_ofx_anal_mooring_fsts_l015_125km3_pb_Buoy6D_var_data.csv` - Buoy 6DOF data
      - `go-by/output/.csv/dm_ofx_anal_mooring_fsts_l015_125km3_pb_Shape_var_data.csv` - Shape variable data
      - `go-by/output/.csv/dm_ofx_anal_mooring_fsts_l015_125km3_pb_Constraint_var_data.csv` - Constraint variable data
      - `go-by/.sim/includefile_fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_mooring_line_length.yml` - Updated line lengths
      - `go-by/.sim/includefile_fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_fender_compression.yml` - Updated fender settings
      - `go-by/output/visual/mooring_force_components.png` - **REQUIRED: Force visualization chart** (generated by visualization command)
      - `go-by/output/.csv/mooring_stiffness_analysis.csv` - **REQUIRED: 3D line stiffness contributions** (k_axial, k_x, k_y, k_z, k_xy, k_xz, k_yz, cos_x, cos_y, cos_z for each line)
      - `go-by/output/.csv/mooring_stiffness_summary.csv` - **REQUIRED: Full 3x3 system stiffness matrix** (K_xx, K_yy, K_zz, K_xy, K_xz, K_yz and natural periods for surge/sway/heave)
  - [x] 0.1.2: **Step 2 - .dat to .sim Conversion**
    - **Command**: `/d/github/digitalmodel/.venv/Scripts/python run_models_to_sim.py dat=true input_directory="../.dat/" output_directory="../.sim/"`
    - **Input Files**:
      - `go-by/.dat/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat` - OrcaFlex model file (43MB)
      - `go-by/scripts/results/dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml` - Updated tensions from Step 1
      - `go-by/.sim/includefile_fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_mooring_line_length.yml` - Line length config
      - `go-by/.sim/includefile_fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_fender_compression.yml` - Fender config
    - **Status**: ✅ SUCCESS
    - **Output Files**:
      - `go-by/.sim/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.sim` - OrcaFlex simulation file (45MB)
  - [x] 0.1.3: **Step 3 - Post-processing Results**
    - **Command**: `/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel dm_ofx_post_fsts_lngc.yml --workers 30`
    - **Input Files**:
      - `go-by/.sim/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.sim` - Simulation file from Step 2
      - `go-by/scripts/dm_ofx_post_fsts_lngc.yml` - Post-processing configuration
    - **Status**: ✅ SUCCESS
    - **Output Files**:
      - `go-by/output/.csv/dm_ofx_post_fsts_lngc_lngc_moorings_static_eff_tension_max.csv` - Maximum effective tensions
      - `go-by/output/.csv/dm_ofx_post_fsts_lngc_lngc_fenders_static.csv` - Fender force analysis
      - `go-by/output/.csv/dm_ofx_post_fsts_lngc_lngc_6dof_stat.csv` - 6DOF vessel statistics
      - `go-by/output/.csv/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv` - Pretension analysis
      - `go-by/output/.csv/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_fender_force_analysis.csv` - Fender forces
      - `go-by/output/.csv/dm_ofx_anal_mooring_fsts_l015_125km3_pb_*_var_data.csv` - Variable data files (Line, Vessel, Shape, etc.)
  - [x] 0.1.4: **Step 4 - Collation**
    - **Command**: `/d/github/assetutilities/.venv/Scripts/python -m assetutilities au_collate.yml`
    - **Input Files**:
      - `go-by/output/.csv/dm_ofx_post_fsts_lngc_lngc_moorings_static_eff_tension_max.csv` - Mooring tensions
      - `go-by/output/.csv/dm_ofx_post_fsts_lngc_lngc_fenders_static.csv` - Fender forces
      - `go-by/output/.csv/dm_ofx_post_fsts_lngc_lngc_6dof_stat.csv` - Vessel statistics
      - `go-by/output/.csv/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv` - Pretensions
      - `go-by/scripts/au_collate.yml` - Collation configuration
    - **Status**: ✅ SUCCESS
    - **Output Files**:
      - `go-by/output/collate/pretension_analysis_summary.xlsx` - Consolidated Excel report with all data
      - `go-by/scripts/results/au_collate.yml` - Updated collation config with results
  - [x] 0.1.5: **Step 5 - Visualization**
    - **Command**: `/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel viz.yml` with pattern config
    - **Input Files**:
      - `go-by/.sim/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.sim` - Simulation file
      - `go-by/output/.csv/*.csv` - All CSV analysis results
      - `go-by/scripts/viz.yml` - Visualization configuration with pattern `fsts_*_vessel_statics_6dof`
    - **Status**: ✅ SUCCESS
    - **Output Files**:
      - `go-by/output/visual/mooring_pretension_comparison.png` - Pretension comparison chart
      - `go-by/output/visual/fender_force_distribution.png` - Fender force visualization
      - `go-by/output/visual/vessel_6dof_motion.png` - Vessel motion plots
      - `go-by/output/visual/mooring_line_tensions_*.png` - Individual line tension charts
      - `go-by/output/visual/force_balance_xy.png` - X-Y force balance diagram
- [x] 0.2: Document current file paths and outputs (file_documentation.md created)
  - [x] 0.2.1: List all input files and their purposes
  - [x] 0.2.2: Document all output files generated
  - [x] 0.2.3: Map file flow through the workflow
  - [x] 0.2.4: Identify critical vs optional files
- [x] 0.3: Verify CSV format and data availability
  - [x] 0.3.1: Validate target pretension CSV structure
  - [x] 0.3.2: Check output CSV format from post-processing
  - [x] 0.3.3: Verify all 16 mooring lines have data
  - [x] 0.3.4: Document CSV column meanings and units (verified through successful dm_iterator.sh execution)

**Validation Checklist**:
- [x] dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml exists and is valid
- [x] dm_ofx_post_fsts_lngc.yml exists and is valid
- [x] Target CSV file has correct format with 16 lines
- [x] OrcaFlex .dat file exists and can be loaded (43MB)
- [x] Python environment has all required modules
- [x] Commands execute without errors (with enhanced script)
- [x] .sim file generated successfully (45MB)
- [ ] Output CSV is generated in results/ (needs post-processing)
- [ ] Output contains effective tension data (to be verified)

**Expected Outputs**:
- Modified YAML configuration files
- OrcaFlex simulation results
- CSV file: `results/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv`

**Success Criteria**:
- All three commands execute successfully
- Output CSV contains tension data for all 16 mooring lines
- No missing file errors
- No module import errors

---

### Task 1: Core Orchestrator Implementation
**Effort**: 4 hours  
**Priority**: Critical  
**Dependencies**: None

**Description**: Create Python script to run existing batch commands in a loop with convergence checking.

**Iteration Termination Criteria** (whichever is achieved first):
- Maximum of 5 iterations reached, OR
- Change in line tension exceeds 10 kN from previous iteration (indicating instability)

**Implementation Components**:
```python
class MooringTensionOrchestrator:
    - __init__(working_dir, max_iterations=5)  # Reduced from 10 to 5
    - load_targets() # Read target CSV
    - run_command() # Execute shell commands
    - read_current_tensions() # Parse result CSV
    - check_convergence() # Compare with tolerances
    - check_tension_change() # Monitor tension changes between iterations (>10kN stops)
    - run() # Main loop with dual termination criteria
    - generate_summary() # Final report with termination reason
```

**Convergence Monitoring**:
- Track tension history for each line across iterations
- Calculate delta between current and previous iteration
- Flag if any line shows >10kN change (potential instability)
- Record termination reason: "converged", "max_iterations", or "tension_instability"

**Subtasks**:
- [ ] 1.1: Create orchestrator.py file
- [ ] 1.2: Implement command execution wrapper
- [ ] 1.3: Add CSV parsing for targets and results
- [ ] 1.4: Implement convergence check logic (tolerance-based)
- [ ] 1.5: Add iteration loop with dual termination criteria:
  - [ ] 1.5.1: Maximum 5 iterations limit
  - [ ] 1.5.2: Tension change monitoring (>10kN stops iteration)
  - [ ] 1.5.3: Track tension history across iterations
- [ ] 1.6: Create summary reporting with termination reason

**Deliverables**:
- `orchestrator.py` - Complete implementation (~200 lines)
- Execution logs
- Convergence reports

---

### Task 2: Testing and Validation
**Effort**: 2 hours  
**Priority**: High  
**Dependencies**: Task 1

**Description**: Test orchestrator with existing go-by files.

**Test Scenarios**:
1. **Normal Convergence**
   - Run with existing data
   - Verify stops when converged within tolerance
   
2. **Maximum Iterations**
   - Test with tight tolerances
   - Verify stops at 5 iterations (reduced from 10)
   
3. **Tension Instability Detection**
   - Test case with >10kN tension change
   - Verify stops with "tension_instability" reason
   
4. **Error Handling**
   - Test with missing files
   - Test with malformed CSV

**Subtasks**:
- [ ] 2.1: Test with go-by data files
- [ ] 2.2: Verify convergence detection
- [ ] 2.3: Test iteration limits
- [ ] 2.4: Validate CSV parsing
- [ ] 2.5: Check error handling

**Success Criteria**:
- Correctly reads target tensions from CSV
- Executes batch commands successfully
- Detects convergence based on tolerances
- Stops at max iterations if not converged

---

### Task 3: Pretension Assessment Charts
**Effort**: 3 hours  
**Priority**: High  
**Dependencies**: Task 1

**Description**: Create visualization charts to assess pretension forces in X and Y directions.

**Chart Requirements**:
- Display force components for each mooring line
- Show -X and +X force directions separately
- Show -Y and +Y force directions separately  
- Track force evolution across iterations
- Identify force imbalance and convergence patterns

**Implementation Components**:
```python
class PretensionVisualizer:
    - plot_xy_forces() # Plot X/Y force components
    - plot_directional_forces() # Show +/- directions
    - plot_convergence_history() # Evolution over iterations
    - create_force_balance_chart() # Net forces assessment
    - generate_report_charts() # All charts for report
```

**Subtasks**:
- [ ] 3.1: Extract X/Y force components from tension data
- [ ] 3.2: Create polar/radial charts showing force directions
- [ ] 3.3: Generate bar charts for +X, -X, +Y, -Y forces
- [ ] 3.4: Plot convergence trends per direction
- [ ] 3.5: Create force balance summary chart
- [ ] 3.6: Add charts to iteration reports

**Chart Types**:
1. **Directional Force Bar Chart**
   - Grouped bars for each line showing force magnitude
   - Separate bars for +X, -X, +Y, -Y components
   - Color-coded by convergence status

2. **Force Balance Polar Plot**
   - Radial plot showing force vectors
   - Visual representation of system balance
   - Highlights imbalanced directions

3. **Iteration Convergence Plot**
   - Time series of forces per direction
   - Shows convergence trajectory
   - Identifies problematic directions

**Deliverables**:
- Force assessment charts (PNG/SVG)
- Interactive HTML report with plotly
- Force balance analysis summary

---

### Task 4: Mooring Stiffness Analysis
**Effort**: 4 hours  
**Priority**: High  
**Dependencies**: Task 1

**Description**: Perform mooring stiffness analysis in X and Y directions to understand restoring forces.

**Analysis Requirements**:
- Calculate stiffness coefficients for -X, +X, -Y, +Y directions
- Determine restoring force characteristics per direction
- Analyze stiffness evolution across iterations
- Identify stiffness asymmetries affecting convergence
- Correlate stiffness with convergence behavior

**Implementation Components**:
```python
class MooringStiffnessAnalyzer:
    - calculate_directional_stiffness() # Compute K_xx, K_yy, K_xy
    - analyze_restoring_forces() # F = -K * displacement
    - compute_stiffness_matrix() # Full 2x2 stiffness matrix
    - track_stiffness_evolution() # Changes over iterations
    - identify_stiffness_imbalance() # Asymmetry detection
    - generate_stiffness_report() # Analysis summary
```

**Subtasks**:
- [ ] 4.1: Extract displacement-force relationships from data
- [ ] 4.2: Calculate directional stiffness coefficients (kN/m)
- [ ] 4.3: Compute restoring force curves for each direction
- [ ] 4.4: Analyze stiffness symmetry/asymmetry
- [ ] 4.5: Track stiffness changes during iteration
- [ ] 4.6: Create stiffness visualization charts
- [ ] 4.7: Generate stiffness analysis report

**Stiffness Calculations**:
1. **Directional Stiffness**
   - K_+x: Stiffness for positive X displacement
   - K_-x: Stiffness for negative X displacement
   - K_+y: Stiffness for positive Y displacement
   - K_-y: Stiffness for negative Y displacement

2. **Restoring Force Analysis**
   - Plot F vs displacement for each direction
   - Identify linear vs nonlinear regions
   - Determine effective stiffness ranges

3. **Stiffness Matrix**
   ```
   [K] = | K_xx  K_xy |
         | K_yx  K_yy |
   ```
   - Calculate coupling terms
   - Assess system stability

**Chart Types**:
1. **Stiffness Bar Chart**
   - Compare K_+x, K_-x, K_+y, K_-y values
   - Show stiffness evolution per iteration
   - Highlight asymmetries

2. **Restoring Force Curves**
   - F vs displacement plots for each direction
   - Linear fit vs actual curves
   - Operating range indicators

3. **Stiffness Matrix Heatmap**
   - Visual representation of 2x2 stiffness matrix
   - Evolution across iterations
   - Coupling term visualization

4. **Stiffness Convergence Plot**
   - Time series of stiffness values
   - Convergence indicators
   - Stability boundaries

**Deliverables**:
- Stiffness analysis charts (PNG/SVG)
- Restoring force curves
- Stiffness matrix visualization
- Analysis report with insights

---

### Task 5: Documentation and Deployment
**Effort**: 2 hours  
**Priority**: Medium  
**Dependencies**: Tasks 2, 3, 4

**Description**: Document usage and deploy script.

**Subtasks**:
- [ ] 5.1: Write usage documentation
- [ ] 5.2: Create example run instructions
- [ ] 5.3: Document CSV format requirements
- [ ] 5.4: Add command-line argument parsing
- [ ] 5.5: Create batch/shell wrapper scripts
- [ ] 5.6: Document chart interpretation guide
- [ ] 5.7: Add stiffness analysis documentation

**Deliverables**:
- README with usage instructions
- Example command lines
- Wrapper scripts for easy execution
- Chart interpretation guide
- Stiffness analysis guide

---

## File Structure

```
mooring-tension-iteration/
├── orchestrator.py              # Main script (NEW)
├── pretension_visualizer.py     # Visualization module (NEW)
├── stiffness_analyzer.py        # Stiffness analysis module (NEW)
├── README.md                     # Updated documentation
├── go-by/                       # Existing files
│   ├── dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml
│   ├── dm_ofx_post_fsts_lngc.yml
│   ├── fsts_l015_125km3_pb_target_mooring_pretension.csv
│   └── results/
│       └── *_pretension_analysis.csv
├── logs/                        # Execution logs (NEW)
│   └── iteration_*.log
└── charts/                      # Visualization outputs (NEW)
    ├── force_directions/        # Directional force charts
    │   ├── iteration_*_xy_forces.png
    │   └── iteration_*_polar.png
    ├── stiffness/               # Stiffness analysis charts
    │   ├── directional_stiffness.png
    │   ├── restoring_forces.png
    │   ├── stiffness_matrix.png
    │   └── stiffness_evolution.png
    ├── convergence/             # Convergence trend charts
    │   └── force_convergence_history.png
    └── reports/                 # Interactive HTML reports
        ├── pretension_analysis.html
        └── stiffness_analysis.html
```

## Commands to Execute Per Iteration

From the go-by folder:

1. **Tension Calculation**
   ```bash
   python -m digitalmodel dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml
   ```

2. **OrcaFlex Analysis**
   ```bash
   python -m digitalmodel.orcaflex.universal \
       pattern="fsts*125km3*pb_*.yml" \
       input_directory="." \
       output_directory="." \
       validate=false
   ```

3. **Post-Processing**
   ```bash
   python -m digitalmodel dm_ofx_post_fsts_lngc.yml --workers 30
   ```

## Convergence Logic

```python
# Read targets
targets = {
    'Line01': 80.0,  # kN
    'Line02': 80.0,
    # ... Line03-16
}

tolerances = {
    'Line01': 50,  # %
    'Line02': 50,
    # ... Line03-16
}

# Check convergence
for line in lines:
    diff_percent = abs(current[line] - targets[line]) / targets[line] * 100
    if diff_percent > tolerances[line]:
        return False  # Not converged
return True  # All lines converged
```

## Usage Example

```bash
# Navigate to go-by folder
cd specs/modules/orcaflex/mooring-tension-iteration/go-by

# Run orchestrator
/d/github/digitalmodel/.venv/Scripts/python ../orchestrator.py

# With options
/d/github/digitalmodel/.venv/Scripts/python ../orchestrator.py \
    --max-iterations 5 \
    --working-dir . \
    --verbose
```

## Expected Output

```
Starting Mooring Tension Iteration Orchestrator
Maximum iterations: 5
Loaded 16 target tensions

============================================================
ITERATION 1
============================================================

Step 1: Running tension calculation...
Executing: python -m digitalmodel dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml

Step 2: Running OrcaFlex analysis...
Executing: python -m digitalmodel.orcaflex.universal ...

Step 3: Post-processing results...
Executing: python -m digitalmodel dm_ofx_post_fsts_lngc.yml --workers 30

Step 4: Checking convergence and tension stability...
  Line01: 75.2 kN (target: 80.0 kN, diff: 6.0%, tol: 50%) ✓
  Line02: 78.5 kN (target: 80.0 kN, diff: 1.9%, tol: 50%) ✓
  ...
  Line16: 58.3 kN (target: 60.0 kN, diff: 2.8%, tol: 1%) ✗

Tension changes from previous iteration: All within 10kN limit ✓
Not converged, continuing to next iteration...

[... iterations continue ...]

============================================================
TERMINATION: Maximum iterations reached (5)
============================================================

============================================================
CONVERGENCE SUMMARY
============================================================
Total iterations: 5
Termination reason: MAX_ITERATIONS
Final convergence: 14/16 lines converged (87.5%)

Convergence Trend:
  Iteration 1: 12/16 lines converged (75.0%)
  Iteration 2: 14/16 lines converged (87.5%)
  Iteration 3: 14/16 lines converged (87.5%)
  Iteration 4: 14/16 lines converged (87.5%)
  Iteration 5: 14/16 lines converged (87.5%)

Tension Stability:
  Max change in final iteration: 2.3 kN (Line03)
  All changes within 10kN stability limit

Alternative Termination Examples:
- CONVERGED: All lines within tolerance
- TENSION_INSTABILITY: Line XX showed 15.2kN change (>10kN limit)
- MAX_ITERATIONS: Reached 5 iteration limit
```

## Definition of Done

- [ ] Orchestrator script implemented and tested
- [ ] Successfully runs batch commands in loop
- [ ] Correctly parses CSV files for targets and results
- [ ] Detects convergence based on tolerances
- [ ] Stops at max iterations or convergence
- [ ] Generates clear status reports
- [ ] Pretension force charts created (-X, +X, -Y, +Y)
- [ ] Force balance visualization implemented
- [ ] Stiffness analysis completed for all directions
- [ ] Restoring force curves generated
- [ ] Stiffness matrix visualization created
- [ ] All charts integrated into reports
- [ ] Documentation complete with chart interpretation guides
- [ ] Ready for production use

## Notes

- **Analysis focus** - Orchestration with comprehensive force and stiffness analysis
- **NO model modifications** - Only runs existing commands and analyzes outputs
- **Visualization emphasis** - Charts for force directions and stiffness assessment
- **Fixed commands** - No dynamic command generation
- Total implementation: ~500 lines of Python code (orchestrator + visualizers + analyzers)