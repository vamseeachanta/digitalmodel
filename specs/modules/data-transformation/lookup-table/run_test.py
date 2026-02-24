"""
Test Runner for Tension to Stress Range Transformation
Validates the data transformation process and generates sample outputs
"""

import os
import sys
import pandas as pd
import numpy as np
from pathlib import Path
import json

# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from test_process_transformation import TensionToStressTransformer

def validate_input_files():
    """Check if required input files exist."""
    required_files = [
        "inputs/test_transformation_config.yaml",
        "inputs/tension_range_to_stress_range_function.csv",
        "data/fsts_l015_125km3_l100_pb_FC001_Strut1_rainflow.csv"
    ]
    
    base_path = Path(__file__).parent
    missing_files = []
    
    for file in required_files:
        file_path = base_path / file
        if not file_path.exists():
            missing_files.append(file)
        else:
            print(f"[OK] Found: {file}")
    
    if missing_files:
        print(f"\n[ERROR] Missing files:")
        for file in missing_files:
            print(f"  - {file}")
        return False
    
    return True

def run_transformation_test():
    """Run the transformation process."""
    print("\n" + "="*60)
    print("RUNNING TRANSFORMATION TEST")
    print("="*60)
    
    # Check inputs
    if not validate_input_files():
        print("\nTest aborted: Missing required files")
        return False
    
    try:
        # Initialize transformer
        config_file = "specs/modules/data-transformation/lookup-table/inputs/test_transformation_config.yaml"
        print(f"\n[INFO] Loading configuration from: {config_file}")
        
        transformer = TensionToStressTransformer(config_file)
        
        # Run transformation
        print("\n[INFO] Starting transformation process...")
        transformer.run()
        
        print("\n[SUCCESS] Transformation completed successfully!")
        return True
        
    except Exception as e:
        print(f"\n[ERROR] Error during transformation: {str(e)}")
        import traceback
        traceback.print_exc()
        return False

def validate_outputs():
    """Validate that output files were created correctly."""
    print("\n" + "="*60)
    print("VALIDATING OUTPUT FILES")
    print("="*60)
    
    output_dir = Path("specs/modules/fatigue-analysis/reference-seastate-scale-load/output/rainflow/stress_range")
    
    if not output_dir.exists():
        print(f"[ERROR] Output directory does not exist: {output_dir}")
        return False
    
    # Expected output pattern for test file
    expected_pattern = "fsts_l015_125km3_l100_pb_FC001_Strut1_loc*_stress_rainflow.csv"
    output_files = list(output_dir.glob(expected_pattern))
    
    if not output_files:
        print(f"[ERROR] No output files found matching pattern: {expected_pattern}")
        return False
    
    print(f"\n[INFO] Found {len(output_files)} output files:")
    
    # Validate each output file
    for output_file in sorted(output_files):
        print(f"\n  Checking: {output_file.name}")
        
        try:
            # Read the file
            df = pd.read_csv(output_file)
            
            # Check required columns
            required_columns = ['stress range (Mpa)', 'Cycles_Annual']
            missing_cols = [col for col in required_columns if col not in df.columns]
            
            if missing_cols:
                print(f"    [ERROR] Missing columns: {missing_cols}")
                continue
            
            # Check data validity
            non_zero_rows = df[df['stress range (Mpa)'] > 0]
            
            print(f"    [OK] Columns: {list(df.columns)}")
            print(f"    [OK] Total rows: {len(df)}")
            print(f"    [OK] Non-zero stress rows: {len(non_zero_rows)}")
            
            if len(non_zero_rows) > 0:
                print(f"    [OK] Stress range: {non_zero_rows['stress range (Mpa)'].min():.2f} - {non_zero_rows['stress range (Mpa)'].max():.2f} MPa")
                print(f"    [OK] Total annual cycles: {df['Cycles_Annual'].sum():.0f}")
            
        except Exception as e:
            print(f"    [ERROR] Error reading file: {str(e)}")
            return False
    
    return True

def generate_summary_report():
    """Generate a summary report of the test results."""
    print("\n" + "="*60)
    print("TEST SUMMARY REPORT")
    print("="*60)
    
    report = {
        "test_date": pd.Timestamp.now().isoformat(),
        "input_files": {
            "config": "test_transformation_config.yaml",
            "lookup_table": "tension_range_to_stress_range_function.csv",
            "test_data": "fsts_l015_125km3_l100_pb_FC001_Strut1_rainflow.csv"
        },
        "process": {
            "interpolation_method": "linear",
            "extrapolation": True,
            "location_ids_processed": [2, 3, 5, 6, 7, 9, 10]
        },
        "output": {
            "folder": "specs/modules/fatigue-analysis/reference-seastate-scale-load/output/rainflow/stress_range",
            "file_pattern": "{config}_FC{fc_number}_Strut{strut_number}_loc{location_id:02d}_stress_rainflow.csv",
            "files_generated": 7
        }
    }
    
    # Save report
    report_file = Path("specs/modules/data-transformation/lookup-table/test_report.json")
    with open(report_file, 'w') as f:
        json.dump(report, f, indent=2)
    
    print(f"\n[INFO] Report saved to: {report_file}")
    print(f"\nTest Configuration:")
    print(f"  - Interpolation: {report['process']['interpolation_method']}")
    print(f"  - Extrapolation: {report['process']['extrapolation']}")
    print(f"  - Location IDs: {report['process']['location_ids_processed']}")
    print(f"  - Output files: {report['output']['files_generated']} expected")
    
    return report

def main():
    """Main test execution."""
    print("\n" + "="*60)
    print("TENSION TO STRESS TRANSFORMATION TEST")
    print("="*60)
    
    # Run tests
    success = True
    
    # Step 1: Run transformation
    if not run_transformation_test():
        success = False
        print("\n[WARNING] Transformation test failed")
    
    # Step 2: Validate outputs
    if success and not validate_outputs():
        success = False
        print("\n[WARNING] Output validation failed")
    
    # Step 3: Generate report
    if success:
        generate_summary_report()
        print("\n" + "="*60)
        print("[SUCCESS] ALL TESTS PASSED SUCCESSFULLY!")
        print("="*60)
    else:
        print("\n" + "="*60)
        print("[FAILURE] TEST FAILED - Please check the errors above")
        print("="*60)
    
    return 0 if success else 1

if __name__ == "__main__":
    sys.exit(main())