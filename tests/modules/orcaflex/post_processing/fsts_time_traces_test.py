# Standard library imports
import os
import sys

# Third party imports
import pandas as pd
import numpy as np

# Reader imports
from digitalmodel.engine import engine


def run_process(input_file, expected_result={}):
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)
    return cfg


def test_process():
    input_file = "fsts_time_traces.yml"

    # Save original argv and clear it to avoid conflicts with engine
    original_argv = sys.argv.copy()
    sys.argv = [sys.argv[0]]  # Keep only the script name
    
    try:
        cfg = run_process(input_file, expected_result={})
        
        # Verify that time traces results are produced
        assert cfg is not None, "Configuration should not be None"
        
        # Check if time series files were created
        result_dir = os.path.dirname(__file__)
        expected_files = [
            "FST2F_FST1F_LWL_000deg_Strut1.csv",
            "FST2F_FST1F_LWL_000deg_Strut2.csv",
            "FST2F_FST1F_LWL_000deg_Strut3.csv",
            "FST2F_FST1F_LWL_000deg_Strut4.csv",
            "FST2F_FST1F_LWL_000deg_Strut5.csv",
            "FST2F_FST1F_LWL_000deg_Strut6.csv",
            "FST2F_FST1F_LWL_000deg_Strut7.csv",
            "FST2F_FST1F_LWL_000deg_Strut8.csv",
            "FST2F_FST1F_LWL_000deg_Jacket1.csv",
            "FST2F_FST1F_LWL_000deg_Jacket2.csv",
            "FST2F_FST1F_LWL_000deg_Jacket3.csv",
            "FST2F_FST1F_LWL_000deg_Jacket4.csv",
        ]
        
        for expected_file in expected_files:
            file_path = os.path.join(result_dir, "results", expected_file)
            assert os.path.exists(file_path), f"Expected time trace file {expected_file} was not created"
            print(f"[OK] Found time trace file: {expected_file}")
        
        print("All time trace files successfully created!")
        
        # Validate dataframe content for each file
        print("\nValidating time trace data content...")
        
        # Define expected columns for different object types
        strut_columns = [
            'time', 'Tension (Jacket End)', 'Shear (Jacket End)', 
            'Fx (Jacket End)', 'Fy (Jacket End)', 'Fz (Jacket End)',
            'X (Jacket End)', 'Y (Jacket End)', 'Z (Jacket End)',
            'Vx (Jacket End)', 'Vy (Jacket End)', 'Vz (Jacket End)',
            'Angle (Jacket End)', 'Tension (Vessel End)', 'Shear (Vessel End)',
            'Fx (Vessel End)', 'Fy (Vessel End)', 'Fz (Vessel End)',
            'X (Vessel End)', 'Y (Vessel End)', 'Z (Vessel End)',
            'Vx (Vessel End)', 'Vy (Vessel End)', 'Vz (Vessel End)',
            'Angle (Vessel End)'
        ]
        
        jacket_columns = [
            'time', 'x displacment', 'y displacment', 
            'x velocity', 'y velocity', 'FX Force', 'FY Force', 'FZ Force'
        ]
        
        # Validate Strut files
        for i in range(1, 9):
            file_name = f"FST2F_FST1F_LWL_000deg_Strut{i}.csv"
            file_path = os.path.join(result_dir, "results", file_name)
            
            # Read dataframe
            df = pd.read_csv(file_path)
            
            # Check columns
            assert list(df.columns) == strut_columns, f"Unexpected columns in {file_name}"
            
            # Check data integrity
            assert len(df) > 0, f"No data in {file_name}"
            assert df['time'].is_monotonic_increasing, f"Time not monotonic in {file_name}"
            assert np.allclose(df['time'].iloc[0], 0.0), f"Time doesn't start at 0 in {file_name}"
            assert np.allclose(df['time'].iloc[-1], 10.0, rtol=0.1), f"Time doesn't end around 10s in {file_name}"
            
            # Check that data contains numeric values (not NaN)
            numeric_cols = df.select_dtypes(include=[np.number]).columns
            assert not df[numeric_cols].isna().any().any(), f"NaN values found in {file_name}"
            
            print(f"[OK] Validated data in {file_name}: {len(df)} rows, time range {df['time'].iloc[0]:.1f}-{df['time'].iloc[-1]:.1f}s")
        
        # Validate Jacket files
        for i in range(1, 5):
            file_name = f"FST2F_FST1F_LWL_000deg_Jacket{i}.csv"
            file_path = os.path.join(result_dir, "results", file_name)
            
            # Read dataframe
            df = pd.read_csv(file_path)
            
            # Check columns
            assert list(df.columns) == jacket_columns, f"Unexpected columns in {file_name}"
            
            # Check data integrity
            assert len(df) > 0, f"No data in {file_name}"
            assert df['time'].is_monotonic_increasing, f"Time not monotonic in {file_name}"
            assert np.allclose(df['time'].iloc[0], 0.0), f"Time doesn't start at 0 in {file_name}"
            assert np.allclose(df['time'].iloc[-1], 10.0, rtol=0.1), f"Time doesn't end around 10s in {file_name}"
            
            # Check that data contains numeric values (not NaN)
            numeric_cols = df.select_dtypes(include=[np.number]).columns
            assert not df[numeric_cols].isna().any().any(), f"NaN values found in {file_name}"
            
            print(f"[OK] Validated data in {file_name}: {len(df)} rows, time range {df['time'].iloc[0]:.1f}-{df['time'].iloc[-1]:.1f}s")
        
        # Cross-validate that all files have the same time steps
        print("\nCross-validating time consistency across files...")
        reference_df = pd.read_csv(os.path.join(result_dir, "results", "FST2F_FST1F_LWL_000deg_Strut1.csv"))
        reference_time = reference_df['time'].values
        
        for expected_file in expected_files:
            file_path = os.path.join(result_dir, "results", expected_file)
            df = pd.read_csv(file_path)
            assert np.allclose(df['time'].values, reference_time), f"Time mismatch in {expected_file}"
        
        print("[OK] All files have consistent time steps")
        print("\nAll validation checks passed!")
        
    finally:
        # Restore original argv
        sys.argv = original_argv


# Only run directly if this file is executed as a script
if __name__ == "__main__":
    test_process()
