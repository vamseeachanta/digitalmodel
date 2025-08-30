# Implementation Tasks: Mooring Tension Iteration Orchestrator

> **Total Effort**: 17 hours (~2.5 days)  
> **Scope**: Orchestration of existing batch commands with comprehensive analysis  
> **Analysis Focus**: Execution, convergence tracking, force visualization, and stiffness analysis

## Task Execution Tracker

| Task ID | Description | Status | Priority | Effort |
|---------|-------------|--------|----------|--------|
| **0.1** | Run go-by commands once to verify all files work | ✅ Complete (0.1.2 successful - .sim generated) | Critical | 1h |
| **0.2** | Document current file paths and outputs | ✅ Complete (file_documentation.md created) | Critical | 0.5h |
| **0.3** | Verify CSV format and data availability | ⬜ Pending | Critical | 0.5h |
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
python -m digitalmodel.modules.orcaflex.universal \
    pattern="fsts*125km3*pb_*.yml" \
    input_directory="." \
    output_directory="." \
    validate=false

# Step 3: Post-process results
python -m digitalmodel dm_ofx_post_fsts_lngc.yml --workers 30
```

**Subtasks**:
- [x] 0.1: Run go-by commands once to verify all files work
  - [x] 0.1.1: Attempt to run original commands as documented
  - [x] 0.1.2: Identify missing .sim file issue blocking workflow
  - [x] 0.1.3: Enhance run_models_to_sim.py to support .dat files
  - [x] 0.1.4: Test enhanced script with mock mode
  - [x] 0.1.5: Run script to generate .sim file from .dat file
  - [x] 0.1.6: Verify .sim file created successfully (45MB)
- [x] 0.2: Document current file paths and outputs (file_documentation.md created)
  - [x] 0.2.1: List all input files and their purposes
  - [x] 0.2.2: Document all output files generated
  - [x] 0.2.3: Map file flow through the workflow
  - [x] 0.2.4: Identify critical vs optional files
- [ ] 0.3: Verify CSV format and data availability
  - [ ] 0.3.1: Validate target pretension CSV structure
  - [ ] 0.3.2: Check output CSV format from post-processing
  - [ ] 0.3.3: Verify all 16 mooring lines have data
  - [ ] 0.3.4: Document CSV column meanings and units

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

**Implementation Components**:
```python
class MooringTensionOrchestrator:
    - __init__(working_dir, max_iterations=10)
    - load_targets() # Read target CSV
    - run_command() # Execute shell commands
    - read_current_tensions() # Parse result CSV
    - check_convergence() # Compare with tolerances
    - run() # Main loop
    - generate_summary() # Final report
```

**Subtasks**:
- [ ] 1.1: Create orchestrator.py file
- [ ] 1.2: Implement command execution wrapper
- [ ] 1.3: Add CSV parsing for targets and results
- [ ] 1.4: Implement convergence check logic
- [ ] 1.5: Add iteration loop with max limit
- [ ] 1.6: Create summary reporting

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
   - Verify stops when converged
   
2. **Maximum Iterations**
   - Test with tight tolerances
   - Verify stops at 10 iterations
   
3. **Error Handling**
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
   python -m digitalmodel.modules.orcaflex.universal \
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
Maximum iterations: 10
Loaded 16 target tensions

============================================================
ITERATION 1
============================================================

Step 1: Running tension calculation...
Executing: python -m digitalmodel dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml

Step 2: Running OrcaFlex analysis...
Executing: python -m digitalmodel.modules.orcaflex.universal ...

Step 3: Post-processing results...
Executing: python -m digitalmodel dm_ofx_post_fsts_lngc.yml --workers 30

Step 4: Checking convergence...
  Line01: 75.2 kN (target: 80.0 kN, diff: 6.0%, tol: 50%) ✓
  Line02: 78.5 kN (target: 80.0 kN, diff: 1.9%, tol: 50%) ✓
  ...
  Line16: 58.3 kN (target: 60.0 kN, diff: 2.8%, tol: 1%) ✗

Not converged, continuing to next iteration...

[... iterations continue ...]

✓✓✓ CONVERGED at iteration 4 ✓✓✓

============================================================
CONVERGENCE SUMMARY
============================================================
Total iterations: 4

Convergence Trend:
  Iteration 1: 12/16 lines converged (75.0%)
  Iteration 2: 14/16 lines converged (87.5%)
  Iteration 3: 15/16 lines converged (93.8%)
  Iteration 4: 16/16 lines converged (100.0%)
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