# Implementation Tasks: Mooring Tension Iteration Orchestrator

> **Total Effort**: 15 hours (~2 days)  
> **Scope**: Orchestration of existing batch commands with comprehensive analysis  
> **Analysis Focus**: Execution, convergence tracking, force visualization, and stiffness analysis

## Single Phase Implementation

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
- [ ] Create orchestrator.py file
- [ ] Implement command execution wrapper
- [ ] Add CSV parsing for targets and results
- [ ] Implement convergence check logic
- [ ] Add iteration loop with max limit
- [ ] Create summary reporting

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
- [ ] Test with go-by data files
- [ ] Verify convergence detection
- [ ] Test iteration limits
- [ ] Validate CSV parsing
- [ ] Check error handling

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
- [ ] Extract X/Y force components from tension data
- [ ] Create polar/radial charts showing force directions
- [ ] Generate bar charts for +X, -X, +Y, -Y forces
- [ ] Plot convergence trends per direction
- [ ] Create force balance summary chart
- [ ] Add charts to iteration reports

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
- [ ] Extract displacement-force relationships from data
- [ ] Calculate directional stiffness coefficients (kN/m)
- [ ] Compute restoring force curves for each direction
- [ ] Analyze stiffness symmetry/asymmetry
- [ ] Track stiffness changes during iteration
- [ ] Create stiffness visualization charts
- [ ] Generate stiffness analysis report

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
- [ ] Write usage documentation
- [ ] Create example run instructions
- [ ] Document CSV format requirements
- [ ] Add command-line argument parsing
- [ ] Create batch/shell wrapper scripts
- [ ] Document chart interpretation guide
- [ ] Add stiffness analysis documentation

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