#!/usr/bin/env python
"""
Integration test for the Mooring Tension Iteration Orchestrator.
Tests the full workflow with mock commands that simulate convergence.
"""

import sys
import os
import shutil
import csv
from pathlib import Path
import tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from orchestrator import MooringTensionOrchestrator


def create_mock_commands(test_dir):
    """Create mock batch files that simulate the real commands."""
    
    # Create mock Python scripts that update the tension values
    mock_calc = test_dir / "mock_calc.py"
    mock_calc.write_text("""
import csv
import sys
from pathlib import Path

# Simulate tension calculation - just pass
print("Mock tension calculation running...")
sys.exit(0)
""")
    
    mock_analysis = test_dir / "mock_analysis.py"
    mock_analysis.write_text("""
import csv
import sys
from pathlib import Path

# Simulate OrcaFlex analysis - just pass
print("Mock OrcaFlex analysis running...")
sys.exit(0)
""")
    
    mock_post = test_dir / "mock_post.py"
    mock_post.write_text("""
import csv
import sys
from pathlib import Path

# Simulate post-processing - update tensions closer to targets
csv_path = Path(sys.argv[1])
results_dir = csv_path.parent / "results"
results_dir.mkdir(exist_ok=True)

# Read target CSV
targets = {}
tolerances = {}
with open(csv_path, 'r') as f:
    reader = csv.DictReader(f)
    for row in reader:
        targets[row['ObjectName']] = float(row['target_tension'])
        tolerances[row['ObjectName']] = float(row['tolerance'])

# Read current iteration (from file or default)
iteration_file = results_dir / "iteration.txt"
if iteration_file.exists():
    iteration = int(iteration_file.read_text()) + 1
else:
    iteration = 1
iteration_file.write_text(str(iteration))

# Generate new tensions that converge over iterations
output_csv = results_dir / "fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv"

with open(output_csv, 'w', newline='') as f:
    writer = csv.writer(f)
    writer.writerow(['ObjectName', 'current_tension'])
    
    for line, target in targets.items():
        # Simulate convergence: start far, get closer each iteration
        if iteration == 1:
            # First iteration: some lines far from target
            if line == 'Line03':
                current = target * 1.95  # 95% off
            elif line == 'Line13':
                current = target * 1.25  # 25% off
            else:
                current = target * 1.001  # Very close
        elif iteration == 2:
            # Second iteration: Line13 improves
            if line == 'Line03':
                current = target * 1.50  # Still far
            elif line == 'Line13':
                current = target * 1.10  # Better
            else:
                current = target
        elif iteration == 3:
            # Third iteration: Line03 improves more
            if line == 'Line03':
                current = target * 1.02  # Getting close
            else:
                current = target
        else:
            # Fourth+ iteration: all converged
            current = target
            
        writer.writerow([line, current])

print(f"Mock post-processing complete (iteration {iteration})")
sys.exit(0)
""")
    
    return mock_calc, mock_analysis, mock_post


def test_convergence_workflow():
    """Test the full orchestration workflow with mock commands."""
    
    print("="*70)
    print("INTEGRATION TEST: MOORING TENSION ORCHESTRATOR")
    print("="*70)
    
    # Create temporary test directory
    with tempfile.TemporaryDirectory() as temp_dir:
        test_dir = Path(temp_dir) / "test_orchestration"
        test_dir.mkdir()
        
        # Copy target CSV to test directory
        source_csv = Path("specs/modules/orcaflex/mooring-tension-iteration/go-by/fsts_l015_125km3_pb_target_mooring_pretension.csv")
        if source_csv.exists():
            target_csv = test_dir / "fsts_l015_125km3_pb_target_mooring_pretension.csv"
            shutil.copy(source_csv, target_csv)
        else:
            # Create minimal test CSV
            target_csv = test_dir / "fsts_l015_125km3_pb_target_mooring_pretension.csv"
            with open(target_csv, 'w', newline='') as f:
                writer = csv.writer(f)
                writer.writerow(['ObjectName', 'target_tension', 'tolerance'])
                for i in range(1, 17):
                    line_name = f"Line{i:02d}"
                    target = 80 if i <= 3 else 60
                    tolerance = 1 if i in [3, 13] else 50
                    writer.writerow([line_name, target, tolerance])
        
        print(f"\n1. Created test directory: {test_dir}")
        
        # Create mock commands
        mock_calc, mock_analysis, mock_post = create_mock_commands(test_dir)
        print("2. Created mock command scripts")
        
        # Create mock orchestrator that uses our test commands
        class TestOrchestrator(MooringTensionOrchestrator):
            def run_command(self, command, description=""):
                """Override to use mock commands."""
                if description:
                    print(f"\n{description}")
                
                # Replace real commands with mock ones
                if "dm_ofx_anal_mooring" in command:
                    command = f"python {mock_calc}"
                elif "orcaflex.universal" in command:
                    command = f"python {mock_analysis}"
                elif "dm_ofx_post" in command:
                    command = f"python {mock_post} {target_csv}"
                
                if self.verbose:
                    print(f"Executing: {command}")
                
                import subprocess
                result = subprocess.run(
                    command,
                    shell=True,
                    cwd=test_dir,
                    capture_output=not self.verbose,
                    text=True
                )
                
                if result.returncode != 0 and not self.verbose:
                    print(f"Error: {result.stderr}")
                
                return result
        
        # Run test with max 5 iterations
        print("\n3. Running orchestrator with mock commands...")
        print("-" * 50)
        
        orchestrator = TestOrchestrator(
            working_dir=test_dir,
            max_iterations=5,
            verbose=False
        )
        
        converged = orchestrator.run()
        
        # Check results
        print("\n" + "="*70)
        print("TEST RESULTS")
        print("="*70)
        
        if converged:
            print("*** TEST PASSED: System converged successfully ***")
            iterations_needed = len(orchestrator.iteration_history)
            print(f"Convergence achieved in {iterations_needed} iterations")
        else:
            print("XXX TEST FAILED: System did not converge XXX")
            print("Check if mock commands are working correctly")
        
        # Show iteration history
        print("\nIteration History:")
        for record in orchestrator.iteration_history:
            iteration = record['iteration']
            tensions = record['tensions']
            converged_count = sum(1 for line in tensions 
                                 if line in orchestrator.target_tensions 
                                 and abs(tensions[line] - orchestrator.target_tensions[line]) / orchestrator.target_tensions[line] * 100 
                                 <= orchestrator.tolerances[line])
            print(f"  Iteration {iteration}: {converged_count}/16 lines converged")
        
        return converged


if __name__ == "__main__":
    try:
        success = test_convergence_workflow()
        sys.exit(0 if success else 1)
    except Exception as e:
        print(f"\nERROR: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)