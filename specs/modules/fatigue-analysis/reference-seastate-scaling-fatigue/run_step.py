#!/usr/bin/env python3
"""
Run individual verification steps
Usage: python run_step.py <step_number>
"""

import sys
from pathlib import Path

# Import the verification class
sys.path.append(str(Path(__file__).parent))
from verify_interactive import InteractiveVerification

def run_single_step(step_num):
    """Run a single verification step"""
    verifier = InteractiveVerification()
    
    steps = {
        1: ("Directory Structure", verifier.step1_directory_structure),
        2: ("Naming Convention", verifier.step2_naming_convention),
        3: ("File Content", verifier.step3_file_content),
        4: ("Data Loading", verifier.step4_data_loading),
        5: ("Scaling Calculation", verifier.step5_scaling_calculation),
        6: ("Output Generation", verifier.step6_output_generation)
    }
    
    if step_num not in steps:
        print(f"Invalid step number: {step_num}. Must be 1-6.")
        return False
    
    step_name, step_func = steps[step_num]
    
    print("\n" + "="*60)
    print(f"RUNNING STEP {step_num}: {step_name.upper()}")
    print("="*60)
    
    try:
        success = step_func()
        
        print("\n" + "="*60)
        if success:
            print(f"✅ STEP {step_num} PASSED")
        else:
            print(f"❌ STEP {step_num} FAILED")
        print("="*60)
        
        return success
        
    except Exception as e:
        print(f"\n❌ ERROR in Step {step_num}: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python run_step.py <step_number>")
        print("Example: python run_step.py 3")
        sys.exit(1)
    
    try:
        step_number = int(sys.argv[1])
    except ValueError:
        print(f"Invalid step number: {sys.argv[1]}. Must be an integer 1-6.")
        sys.exit(1)
    
    success = run_single_step(step_number)
    sys.exit(0 if success else 1)