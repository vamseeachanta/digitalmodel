#!/usr/bin/env python
"""
Test the convergence checking logic of the orchestrator.
This validates that the orchestrator can correctly read CSVs and check convergence.
"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from orchestrator import MooringTensionOrchestrator
from pathlib import Path


def test_convergence_check():
    """Test the convergence checking with actual go-by data."""
    
    print("="*70)
    print("TESTING CONVERGENCE CHECK LOGIC")
    print("="*70)
    
    # Create orchestrator instance
    orchestrator = MooringTensionOrchestrator(
        working_dir="specs/modules/orcaflex/mooring-tension-iteration/go-by", 
        verbose=True
    )
    
    # Load target tensions
    print("\n1. Loading target tensions...")
    orchestrator.load_targets()
    print(f"   Loaded {len(orchestrator.target_tensions)} lines")
    
    # Read current tensions from existing CSV
    print("\n2. Reading current tensions from result CSV...")
    
    # Temporarily modify the read method to use current_tension column
    def read_modified_tensions():
        csv_path = orchestrator.working_dir / "results" / "fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv"
        tensions = {}
        
        if csv_path.exists():
            import csv
            with open(csv_path, 'r') as f:
                reader = csv.DictReader(f)
                for row in reader:
                    line_name = row.get('ObjectName', '')
                    # Use current_tension column from the CSV
                    tension_value = row.get('current_tension', '')
                    
                    if line_name and tension_value:
                        try:
                            tensions[line_name] = float(tension_value)
                        except ValueError:
                            print(f"   Warning: Could not parse tension for {line_name}: {tension_value}")
        
        return tensions
    
    current_tensions = read_modified_tensions()
    print(f"   Read {len(current_tensions)} tension values")
    
    # Display first few tensions
    print("\n3. Sample tension values:")
    for i, (line, tension) in enumerate(list(current_tensions.items())[:5]):
        target = orchestrator.target_tensions.get(line, 0)
        print(f"   {line}: Current={tension:.2f} kN, Target={target:.2f} kN")
    
    # Check convergence
    print("\n4. Checking convergence...")
    converged = orchestrator.check_convergence(current_tensions)
    
    # Summary
    print("\n" + "="*70)
    print("TEST SUMMARY")
    print("="*70)
    
    if converged:
        print("*** ALL LINES CONVERGED - System would stop iteration ***")
    else:
        print("XXX NOT CONVERGED - System would continue to next iteration XXX")
    
    # Count converged lines
    converged_count = 0
    for line_name, target in orchestrator.target_tensions.items():
        if line_name in current_tensions:
            current = current_tensions[line_name]
            tolerance = orchestrator.tolerances[line_name]
            diff_percent = abs(current - target) / target * 100 if target != 0 else 0
            if diff_percent <= tolerance:
                converged_count += 1
    
    print(f"\nConverged: {converged_count}/{len(orchestrator.target_tensions)} lines")
    print(f"Percentage: {converged_count/len(orchestrator.target_tensions)*100:.1f}%")
    
    # Identify problem lines
    print("\nLines NOT converged:")
    for line_name, target in sorted(orchestrator.target_tensions.items()):
        if line_name in current_tensions:
            current = current_tensions[line_name]
            tolerance = orchestrator.tolerances[line_name]
            diff_percent = abs(current - target) / target * 100 if target != 0 else 0
            if diff_percent > tolerance:
                print(f"  {line_name}: diff={diff_percent:.1f}% > tolerance={tolerance}%")
    
    return converged


if __name__ == "__main__":
    try:
        converged = test_convergence_check()
        sys.exit(0 if converged else 1)
    except Exception as e:
        print(f"\nERROR: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)