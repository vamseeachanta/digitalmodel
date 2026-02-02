#!/usr/bin/env python
"""
Step-by-step verification with manual progression
Run each step individually for user review
"""

import sys
from pathlib import Path
import pandas as pd
import numpy as np
import json

# Add the src directory to the path
src_path = Path(__file__).parent.parent.parent.parent.parent / "src"
sys.path.insert(0, str(src_path))

from digitalmodel.fatigue_analysis.strut_foundation_processor import (
    ProductionDataHandler, LoadScaler, FatigueCondition
)

class StepVerifier:
    """Step-by-step verification"""
    
    def __init__(self):
        self.sample_path = Path(__file__).parent / "sample_data"
        self.current_step = 0
        
    def step_1_check_directory(self):
        """Step 1: Check directory structure"""
        print("\n" + "="*60)
        print("STEP 1: DIRECTORY STRUCTURE CHECK")
        print("="*60)
        
        print(f"\nChecking path: {self.sample_path}")
        
        if self.sample_path.exists():
            print("[PASS] Directory exists")
        else:
            print("[FAIL] Directory not found!")
            return False
            
        # Check for subdirectories
        subdirs = [d for d in self.sample_path.iterdir() if d.is_dir()]
        
        if subdirs:
            print(f"[WARNING] Found {len(subdirs)} subdirectories:")
            for d in subdirs:
                print(f"  - {d.name}")
            print("\nProduction data should be FLAT (no subdirectories)")
        else:
            print("[PASS] Flat structure - no subdirectories")
        
        # Count CSV files
        csv_files = list(self.sample_path.glob("*.csv"))
        print(f"\n[INFO] Found {len(csv_files)} CSV files")
        
        # Show first 5 files as example
        print("\nExample files:")
        for f in csv_files[:5]:
            print(f"  - {f.name}")
        
        return True
    
    def step_2_check_naming(self):
        """Step 2: Check naming convention"""
        print("\n" + "="*60)
        print("STEP 2: NAMING CONVENTION CHECK")
        print("="*60)
        print("\nExpected pattern: {config}_mwl_{reference}_Strut{#}.csv")
        
        csv_files = list(self.sample_path.glob("*.csv"))
        
        # Parse each filename
        configs_found = set()
        references_found = set()
        struts_found = set()
        invalid_files = []
        
        for f in csv_files:
            name = f.name
            
            # Try to parse the pattern
            if '_mwl_' in name and name.endswith('.csv'):
                parts = name.replace('.csv', '').split('_mwl_')
                if len(parts) == 2:
                    config = parts[0]
                    ref_strut = parts[1]
                    
                    # Extract reference and strut
                    if '_Strut' in ref_strut:
                        ref_parts = ref_strut.split('_Strut')
                        if len(ref_parts) == 2:
                            reference = ref_parts[0]
                            strut = ref_parts[1]
                            
                            configs_found.add(config)
                            references_found.add(reference)
                            struts_found.add(strut)
                            continue
            
            invalid_files.append(name)
        
        print(f"\n[INFO] Configurations found: {sorted(configs_found)}")
        print(f"[INFO] References found: {sorted(references_found)}")
        print(f"[INFO] Struts found: {sorted(struts_found)}")
        
        if invalid_files:
            print(f"\n[FAIL] {len(invalid_files)} files don't match pattern:")
            for name in invalid_files[:5]:
                print(f"  - {name}")
        else:
            print(f"\n[PASS] All {len(csv_files)} files match the pattern")
        
        # Check completeness
        expected_configs = [
            'fsts_l015', 'fsts_l095',
            'fsts_l015_125km3_l100_pb', 'fsts_l095_125km3_l000_pb'
        ]
        expected_refs = ['wind01', 'wave01']
        expected_struts = ['1', '2', '3', '4', '5', '6', '7', '8']
        
        print("\n[INFO] Checking completeness:")
        
        missing_configs = set(expected_configs) - configs_found
        if missing_configs:
            print(f"  [WARN] Missing configs: {missing_configs}")
        else:
            print(f"  [PASS] All 4 configs present")
        
        missing_refs = set(expected_refs) - references_found
        if missing_refs:
            print(f"  [WARN] Missing references: {missing_refs}")
        else:
            print(f"  [PASS] Both references present")
        
        missing_struts = set(expected_struts) - struts_found
        if missing_struts:
            print(f"  [WARN] Missing struts: {missing_struts}")
        else:
            print(f"  [PASS] All 8 struts present")
        
        return True
    
    def step_3_check_file_content(self):
        """Step 3: Check file contents"""
        print("\n" + "="*60)
        print("STEP 3: FILE CONTENT CHECK")
        print("="*60)
        
        # Test one file from each configuration
        test_files = [
            "fsts_l015_mwl_wind01_Strut1.csv",
            "fsts_l095_mwl_wave01_Strut1.csv",
            "fsts_l015_125km3_l100_pb_mwl_wind01_Strut1.csv",
            "fsts_l095_125km3_l000_pb_mwl_wave01_Strut1.csv"
        ]
        
        for filename in test_files:
            filepath = self.sample_path / filename
            
            print(f"\n[INFO] Checking: {filename}")
            
            if not filepath.exists():
                print(f"  [FAIL] File not found")
                continue
            
            try:
                df = pd.read_csv(filepath)
                
                print(f"  [INFO] Shape: {df.shape}")
                print(f"  [INFO] Columns: {list(df.columns)}")
                
                if df.shape[1] >= 2:
                    time_col = df.columns[0]
                    tension_col = df.columns[1]
                    
                    # Check data
                    time_range = (df[time_col].min(), df[time_col].max())
                    tension_range = (df[tension_col].min(), df[tension_col].max())
                    
                    print(f"  [INFO] Time range: {time_range[0]:.1f} to {time_range[1]:.1f} seconds")
                    print(f"  [INFO] Tension range: {tension_range[0]:.1f} to {tension_range[1]:.1f} kN")
                    
                    if df.shape[0] == 1000:
                        print(f"  [PASS] Correct number of samples (1000)")
                    else:
                        print(f"  [WARN] Expected 1000 samples, got {df.shape[0]}")
                    
            except Exception as e:
                print(f"  [FAIL] Error reading file: {e}")
        
        return True
    
    def step_4_test_data_loading(self):
        """Step 4: Test data loading with handler"""
        print("\n" + "="*60)
        print("STEP 4: DATA LOADING TEST")
        print("="*60)
        
        try:
            handler = ProductionDataHandler(base_path=str(self.sample_path))
            print(f"[PASS] Handler initialized")
            
            print("\n[INFO] Testing each configuration:")
            
            for config_name in handler.configurations.keys():
                print(f"\n  {config_name}:")
                
                # Get reference files
                ref_files = handler.get_reference_files(config_name)
                print(f"    Wind refs: {ref_files.get('wind', [])}")
                print(f"    Wave refs: {ref_files.get('wave', [])}")
                
                # Try loading
                if ref_files.get('wind'):
                    ref = ref_files['wind'][0]
                    time_data, tension_data = handler.load_strut_data(
                        config_name, ref, 1, use_sample=False
                    )
                    
                    if len(tension_data) > 0:
                        print(f"    [PASS] Loaded {len(tension_data)} samples from {ref}")
                    else:
                        print(f"    [FAIL] No data loaded from {ref}")
                        
        except Exception as e:
            print(f"[FAIL] Error: {e}")
            return False
        
        return True
    
    def step_5_test_scaling(self):
        """Step 5: Test scaling calculations"""
        print("\n" + "="*60)
        print("STEP 5: SCALING CALCULATION TEST")
        print("="*60)
        
        try:
            handler = ProductionDataHandler(base_path=str(self.sample_path))
            scaler = LoadScaler(handler)
            
            print("\n[INFO] Test condition:")
            print("  Wind: 15 m/s (1.5x base)")
            print("  Wave: Hs=0.75m (1.5x base)")
            
            test_condition = FatigueCondition(
                id=1, wind_speed=15.0, wind_dir=0.0,
                hs=0.75, tp=2.7, wave_dir=0.0, occurrence=5.0
            )
            
            # Calculate expected scales
            wind_scale = (15.0 / 10.0) ** 2
            wave_scale = 0.75 / 0.5
            
            print(f"\n[INFO] Expected scaling factors:")
            print(f"  Wind: {wind_scale:.2f} (speed squared)")
            print(f"  Wave: {wave_scale:.2f} (Hs ratio)")
            
            # Process
            effective_tension, metadata = scaler.process_fatigue_condition(
                'fsts_l015', test_condition, 1
            )
            
            if len(effective_tension) > 0:
                print(f"\n[PASS] Scaling successful")
                print(f"  Generated: {len(effective_tension)} samples")
                print(f"  Range: {np.min(effective_tension):.1f} to {np.max(effective_tension):.1f} kN")
                
                # Check metadata
                if 'wind_scale_factor' in metadata:
                    print(f"  Wind scale applied: {metadata['wind_scale_factor']:.2f}")
                if 'wave_scale_factor' in metadata:
                    print(f"  Wave scale applied: {metadata['wave_scale_factor']:.2f}")
            else:
                print(f"[FAIL] No data generated")
                
        except Exception as e:
            print(f"[FAIL] Error: {e}")
            return False
        
        return True
    
    def step_6_test_output(self):
        """Step 6: Test output generation"""
        print("\n" + "="*60)
        print("STEP 6: OUTPUT GENERATION TEST")
        print("="*60)
        
        output_dir = Path(__file__).parent / "output" / "verification" / "step_by_step"
        output_dir.mkdir(exist_ok=True)
        
        print(f"\n[INFO] Output directory: {output_dir}")
        
        try:
            handler = ProductionDataHandler(base_path=str(self.sample_path))
            scaler = LoadScaler(handler)
            
            # Use first condition
            if scaler.fatigue_conditions:
                test_condition = scaler.fatigue_conditions[0]
                
                print(f"\n[INFO] Processing FC{test_condition.id:03d}")
                
                # Process
                effective_tension, metadata = scaler.process_fatigue_condition(
                    'fsts_l015', test_condition, 1
                )
                
                if len(effective_tension) > 0:
                    # Save file
                    output_file = output_dir / f"test_FC{test_condition.id:03d}_Strut1.csv"
                    
                    df_output = pd.DataFrame({
                        'time_s': np.arange(len(effective_tension)) * 0.1,
                        'effective_tension_kN': effective_tension
                    })
                    df_output.to_csv(output_file, index=False)
                    
                    if output_file.exists():
                        print(f"[PASS] Output file created")
                        print(f"  File: {output_file.name}")
                        print(f"  Size: {output_file.stat().st_size} bytes")
                        
                        # Verify we can read it back
                        df_check = pd.read_csv(output_file)
                        if df_check.shape == df_output.shape:
                            print(f"  [PASS] File is valid and readable")
                        else:
                            print(f"  [WARN] File shape mismatch")
                    else:
                        print(f"[FAIL] Output file not created")
                else:
                    print(f"[FAIL] No data to output")
            
        except Exception as e:
            print(f"[FAIL] Error: {e}")
            return False
        
        return True
    
    def run_step(self, step_num):
        """Run a specific step"""
        steps = {
            1: self.step_1_check_directory,
            2: self.step_2_check_naming,
            3: self.step_3_check_file_content,
            4: self.step_4_test_data_loading,
            5: self.step_5_test_scaling,
            6: self.step_6_test_output
        }
        
        if step_num in steps:
            return steps[step_num]()
        else:
            print(f"Invalid step number: {step_num}")
            return False
    
    def run_all(self):
        """Run all steps"""
        print("\n" + "="*60)
        print("RUNNING ALL VERIFICATION STEPS")
        print("="*60)
        
        for i in range(1, 7):
            if not self.run_step(i):
                print(f"\n[STOP] Step {i} failed")
                return False
        
        print("\n" + "="*60)
        print("ALL STEPS COMPLETED SUCCESSFULLY")
        print("="*60)
        
        return True

def main():
    """Main execution"""
    
    if len(sys.argv) > 1:
        # Run specific step
        try:
            step_num = int(sys.argv[1])
            verifier = StepVerifier()
            success = verifier.run_step(step_num)
        except ValueError:
            if sys.argv[1] == 'all':
                verifier = StepVerifier()
                success = verifier.run_all()
            else:
                print("Usage: python verify_step_by_step.py [step_number|all]")
                print("Steps: 1-6")
                success = False
    else:
        # Show usage
        print("\nStep-by-Step Verification Tool")
        print("="*40)
        print("\nUsage:")
        print("  python verify_step_by_step.py 1    # Run step 1")
        print("  python verify_step_by_step.py 2    # Run step 2")
        print("  ...etc")
        print("  python verify_step_by_step.py all  # Run all steps")
        print("\nAvailable steps:")
        print("  1 - Directory structure check")
        print("  2 - Naming convention check")
        print("  3 - File content check")
        print("  4 - Data loading test")
        print("  5 - Scaling calculation test")
        print("  6 - Output generation test")
        success = True
    
    return 0 if success else 1

if __name__ == "__main__":
    sys.exit(main())