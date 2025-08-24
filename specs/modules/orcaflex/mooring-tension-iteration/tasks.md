# Implementation Tasks: Mooring Tension Iteration Orchestrator

> **Total Effort**: 8 hours (1 day)  
> **Scope**: Simple orchestration of existing batch commands  
> **No Engineering Calculations**: Just execution and convergence tracking

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

### Task 3: Documentation and Deployment
**Effort**: 2 hours  
**Priority**: Medium  
**Dependencies**: Task 2

**Description**: Document usage and deploy script.

**Subtasks**:
- [ ] Write usage documentation
- [ ] Create example run instructions
- [ ] Document CSV format requirements
- [ ] Add command-line argument parsing
- [ ] Create batch/shell wrapper scripts

**Deliverables**:
- README with usage instructions
- Example command lines
- Wrapper scripts for easy execution

---

## File Structure

```
mooring-tension-iteration/
├── orchestrator.py              # Main script (NEW)
├── README.md                     # Updated documentation
├── go-by/                       # Existing files
│   ├── dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml
│   ├── dm_ofx_post_fsts_lngc.yml
│   ├── fsts_l015_125km3_pb_target_mooring_pretension.csv
│   └── results/
│       └── *_pretension_analysis.csv
└── logs/                        # Execution logs (NEW)
    └── iteration_*.log
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
- [ ] Documentation complete
- [ ] Ready for production use

## Notes

- **NO engineering calculations** - just orchestration
- **NO model modifications** - only runs existing commands
- **Simple tolerance check** - percentage difference only
- **Fixed commands** - no dynamic command generation
- Total implementation: ~200 lines of Python code