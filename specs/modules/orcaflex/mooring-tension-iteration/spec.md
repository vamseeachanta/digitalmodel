# Technical Specification: Mooring Tension Iteration Orchestrator

## Executive Summary

This specification defines an orchestration system that automates the execution of existing mooring tension iteration batch files in a loop, tracking convergence of line tensions without performing any engineering calculations. The system simply runs the existing commands repeatedly until convergence criteria is met or maximum iterations reached.

**Scope**:
- Orchestrate existing batch commands in a loop
- Track effective tension values from CSV outputs
- Check convergence criteria (tolerance tracking)
- Stop when converged or after 10 iterations
- NO engineering calculations or modifications

## System Overview

### Purpose
Automate the repetitive manual execution of existing batch commands for mooring tension iteration, providing convergence tracking and stopping criteria.

### Core Functionality
1. **Execute existing batch commands** from go-by folder
2. **Extract tension values** from result CSV files
3. **Track convergence** across iterations
4. **Stop conditions**: Convergence achieved OR 10 iterations completed

## Architecture

### Simple Orchestration Loop

```python
class MooringTensionOrchestrator:
    def __init__(self, max_iterations=10):
        self.max_iterations = max_iterations
        self.iteration_history = []
        
    def run_iteration_loop(self):
        """Main orchestration loop - NO calculations, just execution."""
        
        for iteration in range(1, self.max_iterations + 1):
            print(f"\n=== Iteration {iteration} ===")
            
            # Step 1: Run tension calculation
            self.run_command("python -m digitalmodel dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml")
            
            # Step 2: Run OrcaFlex analysis
            self.run_command("python -m digitalmodel.orcaflex.universal "
                           "pattern='fsts*125km3*pb_*.yml' input_directory='.' "
                           "output_directory='.' validate=false")
            
            # Step 3: Post-process results
            self.run_command("python -m digitalmodel dm_ofx_post_fsts_lngc.yml --workers 30")
            
            # Step 4: Read current tensions from CSV
            current_tensions = self.read_tensions_from_csv()
            
            # Step 5: Check convergence
            if self.check_convergence(current_tensions):
                print(f"✓ Converged at iteration {iteration}")
                break
                
            if iteration == self.max_iterations:
                print(f"⚠ Maximum iterations ({self.max_iterations}) reached")
```

## Batch Commands to Execute

Based on files in `go-by/` folder:

### Per Iteration Commands

1. **Tension Calculation**
   ```bash
   /d/github/digitalmodel/.venv/Scripts/python -m digitalmodel dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml
   ```

2. **Run OrcaFlex Models**
   ```bash
   /d/github/digitalmodel/.venv/Scripts/python -m digitalmodel.orcaflex.universal \
       pattern="fsts*125km3*pb_*.yml" \
       input_directory="." \
       output_directory="." \
       validate=false
   ```

3. **Post-Process Results**
   ```bash
   /d/github/digitalmodel/.venv/Scripts/python -m digitalmodel dm_ofx_post_fsts_lngc.yml --workers 30
   ```

## Convergence Tracking

### Data Sources

1. **Target Tensions**: Read from `fsts_l015_125km3_pb_target_mooring_pretension.csv`
   - Line01 through Line16
   - Target tension values in kN
   - Tolerance values per line

2. **Current Tensions**: Extract from `results/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv`
   - Effective tension values
   - Updated after each iteration

### Convergence Logic (Simple Tolerance Check)

```python
def check_convergence(self, current_tensions, target_tensions, tolerances):
    """
    Simple convergence check - NO calculations, just comparison.
    Returns True if all lines are within their specified tolerance.
    """
    converged_lines = []
    
    for line_name in current_tensions:
        current = current_tensions[line_name]
        target = target_tensions[line_name]
        tolerance = tolerances[line_name]  # Percentage
        
        # Simple percentage difference check
        diff_percent = abs(current - target) / target * 100
        
        is_converged = diff_percent <= tolerance
        converged_lines.append(is_converged)
        
        print(f"{line_name}: Current={current:.1f}, Target={target:.1f}, "
              f"Diff={diff_percent:.1f}%, Tolerance={tolerance}%, "
              f"{'✓' if is_converged else '✗'}")
    
    return all(converged_lines)
```

### Tracking History

```python
def track_iteration(self, iteration_num, tensions):
    """Track tension values across iterations for trend analysis."""
    
    self.iteration_history.append({
        'iteration': iteration_num,
        'timestamp': datetime.now(),
        'tensions': tensions.copy(),
        'converged_lines': self.get_converged_lines(tensions)
    })
    
def generate_convergence_report(self):
    """Generate simple convergence trend report."""
    
    print("\n=== Convergence Trend Report ===")
    for record in self.iteration_history:
        converged_count = len(record['converged_lines'])
        total_lines = 16
        print(f"Iteration {record['iteration']}: "
              f"{converged_count}/{total_lines} lines converged "
              f"({converged_count/total_lines*100:.1f}%)")
```

## Implementation Requirements

### File Structure
```
go-by/
├── dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml    # Tension calculation config
├── dm_ofx_post_fsts_lngc.yml                      # Post-processing config
├── fsts_l015_125km3_pb_target_mooring_pretension.csv  # Target tensions
├── fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat  # OrcaFlex model
└── results/
    └── *_pretension_analysis.csv                   # Output tensions
```

### Core Implementation

```python
#!/usr/bin/env python
"""
Mooring Tension Iteration Orchestrator
Simple loop execution of existing batch commands with convergence tracking.
NO engineering calculations - just orchestration.
"""

import subprocess
import csv
import os
from datetime import datetime
from pathlib import Path

class MooringTensionOrchestrator:
    
    def __init__(self, working_dir="go-by", max_iterations=10):
        self.working_dir = Path(working_dir)
        self.max_iterations = max_iterations
        self.python_exec = "/d/github/digitalmodel/.venv/Scripts/python"
        self.iteration_history = []
        self.target_tensions = {}
        self.tolerances = {}
        
    def load_targets(self):
        """Load target tensions and tolerances from CSV."""
        csv_path = self.working_dir / "fsts_l015_125km3_pb_target_mooring_pretension.csv"
        with open(csv_path, 'r') as f:
            reader = csv.DictReader(f)
            for row in reader:
                line_name = row['ObjectName']
                self.target_tensions[line_name] = float(row['target_tension'])
                self.tolerances[line_name] = float(row['tolerance'])
    
    def run_command(self, command):
        """Execute shell command and wait for completion."""
        print(f"Executing: {command}")
        result = subprocess.run(command, shell=True, cwd=self.working_dir)
        if result.returncode != 0:
            print(f"Warning: Command returned non-zero exit code: {result.returncode}")
        return result
    
    def read_current_tensions(self):
        """Read current tension values from result CSV."""
        csv_path = self.working_dir / "results" / "fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv"
        
        tensions = {}
        if csv_path.exists():
            with open(csv_path, 'r') as f:
                reader = csv.DictReader(f)
                for row in reader:
                    if 'ObjectName' in row and 'EffectiveTension' in row:
                        tensions[row['ObjectName']] = float(row['EffectiveTension'])
        
        return tensions
    
    def check_convergence(self, current_tensions):
        """Check if all lines are within tolerance."""
        if not current_tensions:
            return False
            
        all_converged = True
        for line_name, target in self.target_tensions.items():
            if line_name not in current_tensions:
                print(f"Warning: {line_name} not found in results")
                all_converged = False
                continue
                
            current = current_tensions[line_name]
            tolerance = self.tolerances[line_name]
            diff_percent = abs(current - target) / target * 100
            
            is_converged = diff_percent <= tolerance
            if not is_converged:
                all_converged = False
                
            status = "✓" if is_converged else "✗"
            print(f"  {line_name}: {current:.1f} kN (target: {target:.1f} kN, "
                  f"diff: {diff_percent:.1f}%, tol: {tolerance}%) {status}")
        
        return all_converged
    
    def run(self):
        """Main orchestration loop."""
        print("Starting Mooring Tension Iteration Orchestrator")
        print(f"Maximum iterations: {self.max_iterations}")
        
        # Load targets
        self.load_targets()
        print(f"Loaded {len(self.target_tensions)} target tensions")
        
        # Main iteration loop
        for iteration in range(1, self.max_iterations + 1):
            print(f"\n{'='*60}")
            print(f"ITERATION {iteration}")
            print('='*60)
            
            # Step 1: Run tension calculation
            print("\nStep 1: Running tension calculation...")
            self.run_command(f"{self.python_exec} -m digitalmodel dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml")
            
            # Step 2: Run OrcaFlex analysis
            print("\nStep 2: Running OrcaFlex analysis...")
            self.run_command(f'{self.python_exec} -m digitalmodel.orcaflex.universal '
                           f'pattern="fsts*125km3*pb_*.yml" input_directory="." '
                           f'output_directory="." validate=false')
            
            # Step 3: Post-process results
            print("\nStep 3: Post-processing results...")
            self.run_command(f"{self.python_exec} -m digitalmodel dm_ofx_post_fsts_lngc.yml --workers 30")
            
            # Step 4: Check convergence
            print("\nStep 4: Checking convergence...")
            current_tensions = self.read_current_tensions()
            
            if not current_tensions:
                print("Warning: No tension data found, continuing...")
                continue
            
            # Track history
            self.iteration_history.append({
                'iteration': iteration,
                'tensions': current_tensions.copy()
            })
            
            # Check convergence
            if self.check_convergence(current_tensions):
                print(f"\n✓✓✓ CONVERGED at iteration {iteration} ✓✓✓")
                self.generate_summary()
                return True
            
            print(f"\nNot converged, continuing to next iteration...")
        
        print(f"\n⚠ Maximum iterations ({self.max_iterations}) reached without convergence")
        self.generate_summary()
        return False
    
    def generate_summary(self):
        """Generate convergence summary."""
        print("\n" + "="*60)
        print("CONVERGENCE SUMMARY")
        print("="*60)
        
        print(f"Total iterations: {len(self.iteration_history)}")
        
        if self.iteration_history:
            # Show convergence trend
            print("\nConvergence Trend:")
            for record in self.iteration_history:
                iteration = record['iteration']
                tensions = record['tensions']
                
                converged_count = 0
                for line_name, target in self.target_tensions.items():
                    if line_name in tensions:
                        current = tensions[line_name]
                        tolerance = self.tolerances[line_name]
                        diff_percent = abs(current - target) / target * 100
                        if diff_percent <= tolerance:
                            converged_count += 1
                
                total_lines = len(self.target_tensions)
                print(f"  Iteration {iteration}: {converged_count}/{total_lines} lines "
                      f"converged ({converged_count/total_lines*100:.1f}%)")

if __name__ == "__main__":
    orchestrator = MooringTensionOrchestrator()
    orchestrator.run()
```

## Success Criteria

### Technical Requirements
- ✅ Execute batch commands in sequence
- ✅ Read tension values from CSV files
- ✅ Compare against target tensions with tolerances
- ✅ Stop when converged OR after 10 iterations
- ✅ Generate convergence report

### NOT in Scope
- ❌ Engineering calculations
- ❌ Line length adjustments
- ❌ EA stiffness calculations
- ❌ Newton-Raphson or any optimization algorithms
- ❌ Model modifications

## Testing Requirements

### Test Cases

1. **Successful Convergence**
   - Run with data that converges in 3-4 iterations
   - Verify stops when all lines within tolerance

2. **Maximum Iterations**
   - Run with data that doesn't converge
   - Verify stops after 10 iterations

3. **Partial Convergence**
   - Track which lines converge first
   - Verify trend reporting works

4. **File Handling**
   - Test with missing result files
   - Test with malformed CSV data

## Deliverables

1. **orchestrator.py** - Main orchestration script
2. **Convergence reports** - Per-iteration status
3. **Summary report** - Final convergence state
4. **Execution log** - Commands executed and timing

## Usage

```bash
# From go-by directory
cd specs/modules/orcaflex/mooring-tension-iteration/go-by

# Run orchestrator
/d/github/digitalmodel/.venv/Scripts/python ../orchestrator.py

# Or with custom max iterations
/d/github/digitalmodel/.venv/Scripts/python ../orchestrator.py --max-iterations 5
```

## Summary

This is a simple orchestration system that:
1. Runs existing batch commands in a loop
2. Tracks tension convergence from CSV files
3. Stops when converged or max iterations reached
4. Does NOT perform any engineering calculations

The entire system is approximately 200 lines of Python code focused purely on orchestration and file parsing.