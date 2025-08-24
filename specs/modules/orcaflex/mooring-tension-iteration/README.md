# Mooring Tension Iteration Orchestrator

> **Module**: `orcaflex/mooring-tension-iteration`  
> **Status**: Implementation Ready  
> **Priority**: High  
> **Last Updated**: 2025-08-24  
> **Specification Version**: 3.0 (Simplified orchestration only)

## 🎯 Executive Summary

Simple orchestration system that runs existing mooring tension iteration batch commands in a loop, tracking convergence without performing any engineering calculations. Automates the repetitive manual execution of commands until convergence criteria is met or maximum iterations reached.

### Scope
- ✅ **IN SCOPE**: Orchestration of existing batch commands
- ✅ **IN SCOPE**: CSV parsing for targets and results
- ✅ **IN SCOPE**: Convergence tracking (tolerance checking)
- ✅ **IN SCOPE**: Iteration loop with stopping criteria
- ❌ **NOT IN SCOPE**: Engineering calculations
- ❌ **NOT IN SCOPE**: Model modifications
- ❌ **NOT IN SCOPE**: Line length adjustments
- ❌ **NOT IN SCOPE**: EA stiffness calculations

## 📋 What This System Does

1. **Runs these commands in a loop** (from go-by folder):
   - `python -m digitalmodel dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml`
   - `python -m digitalmodel.modules.orcaflex.universal pattern="fsts*125km3*pb_*.yml"`
   - `python -m digitalmodel dm_ofx_post_fsts_lngc.yml --workers 30`

2. **Checks convergence** after each iteration:
   - Reads target tensions from CSV
   - Reads current tensions from results
   - Compares with tolerance percentages
   - Stops when all lines converged OR 10 iterations complete

3. **Generates reports**:
   - Per-iteration convergence status
   - Convergence trend across iterations
   - Final summary

## 🚀 Quick Start

```bash
# Navigate to go-by folder
cd specs/modules/orcaflex/mooring-tension-iteration/go-by

# Run orchestrator
/d/github/digitalmodel/.venv/Scripts/python ../orchestrator.py
```

That's it! The system will run up to 10 iterations and stop when converged.

## 📊 Implementation Details

### Total Effort: 8 hours (1 day)
- Task 1: Core orchestrator (4 hours)
- Task 2: Testing (2 hours)
- Task 3: Documentation (2 hours)

### Core Implementation (~200 lines of Python)
```python
class MooringTensionOrchestrator:
    def run(self):
        for iteration in range(1, 11):
            # Step 1: Run batch commands
            run_command("python -m digitalmodel dm_ofx_anal_mooring...")
            run_command("python -m digitalmodel.modules.orcaflex.universal...")
            run_command("python -m digitalmodel dm_ofx_post_fsts_lngc.yml")
            
            # Step 2: Check convergence
            current = read_csv("results/..._pretension_analysis.csv")
            if check_convergence(current, targets, tolerances):
                print("✓ Converged!")
                break
```

## 📁 Files Used

### Input Files (go-by folder)
- `fsts_l015_125km3_pb_target_mooring_pretension.csv` - Target tensions & tolerances
- `dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml` - Tension calculation config
- `dm_ofx_post_fsts_lngc.yml` - Post-processing config

### Output Files
- `results/*_pretension_analysis.csv` - Current tension values (updated each iteration)

## 📈 Convergence Tracking

### Target CSV Format
```csv
ObjectName,target_tension,tolerance
Line01,80,50
Line02,80,50
Line03,80,1
...
Line16,60,1
```

### Convergence Check
```
For each line:
  Difference% = |Current - Target| / Target * 100
  Converged = Difference% <= Tolerance%
  
All lines must converge to stop iterations
```

## 🎯 Success Criteria

### What Success Looks Like
```
============================================================
ITERATION 4
============================================================
Step 1: Running tension calculation...
Step 2: Running OrcaFlex analysis...
Step 3: Post-processing results...
Step 4: Checking convergence...
  Line01: 79.5 kN (target: 80.0 kN, diff: 0.6%, tol: 50%) ✓
  Line02: 80.2 kN (target: 80.0 kN, diff: 0.3%, tol: 50%) ✓
  ...
  Line16: 59.8 kN (target: 60.0 kN, diff: 0.3%, tol: 1%) ✓

✓✓✓ CONVERGED at iteration 4 ✓✓✓
```

## 🔧 Technical Requirements

- Python 3.8+
- DigitalModel framework installed
- OrcaFlex license (for batch commands)
- CSV files in go-by folder

## 📝 Key Documents

- **[spec.md](./spec.md)** - Complete technical specification
- **[tasks.md](./tasks.md)** - Implementation tasks (8 hours total)
- **[orchestrator.py](./orchestrator.py)** - Main implementation (to be created)

## ⚠️ Important Notes

1. **NO Engineering Calculations** - This system only orchestrates existing commands
2. **Fixed Commands** - Runs the same 3 commands each iteration
3. **Simple Tolerance Check** - Just percentage difference comparison
4. **Maximum 10 Iterations** - Hard limit to prevent infinite loops
5. **CSV Based** - All data exchange through CSV files

## 🚦 Current Status

- ✅ Specification complete
- ✅ Tasks defined (8 hours)
- ⏳ Ready for implementation
- Total code: ~200 lines of Python

---

*This is a simple orchestration system - no engineering calculations, just running existing commands in a loop with convergence checking.*