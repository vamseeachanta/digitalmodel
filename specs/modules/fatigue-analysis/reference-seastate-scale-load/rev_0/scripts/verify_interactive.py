#!/usr/bin/env python3
"""
Interactive Verification Script with User Confirmation
Runs each verification step and waits for user approval before proceeding
"""

import sys
from pathlib import Path
import pandas as pd
import numpy as np
from typing import Dict, List
import re

# Add parent directory to path for imports
sys.path.append(str(Path(__file__).parent.parent.parent.parent.parent))

from src.digitalmodel.structural.fatigue_apps.strut_foundation_processor import (
    LoadScaler,
    ProductionDataHandler
)

class InteractiveVerification:
    def __init__(self):
        self.base_path = Path(__file__).parent
        self.sample_data_path = self.base_path / "sample_data"
        self.steps_completed = []
        
    def print_header(self, step_num: int, title: str):
        """Print step header"""
        print("\n" + "="*60)
        print(f"STEP {step_num}: {title}")
        print("="*60)
        
    def wait_for_confirmation(self, step_num: int):
        """Wait for user to confirm before proceeding"""
        print("\n" + "-"*60)
        print(f"Step {step_num} completed.")
        print("-"*60)
        
        while True:
            response = input("\nReview the results above. Type 'continue' to proceed to next step, or 'stop' to exit: ").strip().lower()
            if response == 'continue':
                return True
            elif response == 'stop':
                print("\nStopping verification at user request.")
                return False
            else:
                print("Please type 'continue' or 'stop'")
    
    def step1_directory_structure(self):
        """Step 1: Check directory structure"""
        self.print_header(1, "DIRECTORY STRUCTURE CHECK")
        
        print(f"\nChecking path: {self.sample_data_path}")
        
        if not self.sample_data_path.exists():
            print("[FAIL] Directory does not exist")
            return False
            
        print("[PASS] Directory exists")
        
        # Check for subdirectories
        subdirs = [d for d in self.sample_data_path.iterdir() if d.is_dir()]
        if subdirs:
            print(f"[FAIL] Found subdirectories: {subdirs}")
            return False
        else:
            print("[PASS] Flat structure - no subdirectories")
            
        # Count CSV files
        csv_files = list(self.sample_data_path.glob("*.csv"))
        print(f"\n[INFO] Found {len(csv_files)} CSV files")
        
        if len(csv_files) != 64:
            print(f"[WARNING] Expected 64 files, found {len(csv_files)}")
            
        # Show sample files
        print("\nExample files:")
        for file in csv_files[:5]:
            print(f"  - {file.name}")
            
        return True
    
    def step2_naming_convention(self):
        """Step 2: Check naming convention"""
        self.print_header(2, "NAMING CONVENTION CHECK")
        
        pattern = r"^(fsts_[^_]+(?:_125km3_l\d+_pb)?)_mwl_(wind\d+|wave\d+)_Strut(\d+)\.csv$"
        print(f"\nExpected pattern: {{config}}_mwl_{{reference}}_Strut{{#}}.csv")
        
        csv_files = list(self.sample_data_path.glob("*.csv"))
        configs = set()
        references = set()
        struts = set()
        
        all_match = True
        for file in csv_files:
            match = re.match(pattern, file.name)
            if match:
                configs.add(match.group(1))
                references.add(match.group(2))
                struts.add(match.group(3))
            else:
                print(f"[FAIL] File doesn't match pattern: {file.name}")
                all_match = False
                
        print(f"\n[INFO] Configurations found: {sorted(configs)}")
        print(f"[INFO] References found: {sorted(references)}")
        print(f"[INFO] Struts found: {sorted(struts)}")
        
        if all_match:
            print(f"\n[PASS] All {len(csv_files)} files match the pattern")
        else:
            print(f"\n[FAIL] Some files don't match the pattern")
            
        # Check completeness
        print("\n[INFO] Checking completeness:")
        expected_configs = 4
        expected_refs = 2
        expected_struts = 8
        
        if len(configs) == expected_configs:
            print(f"  [PASS] All {expected_configs} configs present")
        else:
            print(f"  [FAIL] Expected {expected_configs} configs, found {len(configs)}")
            
        if len(references) == expected_refs:
            print(f"  [PASS] Both references present")
        else:
            print(f"  [FAIL] Expected {expected_refs} references, found {len(references)}")
            
        if len(struts) == expected_struts:
            print(f"  [PASS] All {expected_struts} struts present")
        else:
            print(f"  [FAIL] Expected {expected_struts} struts, found {len(struts)}")
            
        return all_match
    
    def step3_file_content(self):
        """Step 3: Check file content"""
        self.print_header(3, "FILE CONTENT CHECK")
        
        # Check a sample from each configuration
        test_files = [
            "fsts_l015_mwl_wind01_Strut1.csv",
            "fsts_l095_mwl_wave01_Strut1.csv",
            "fsts_l015_125km3_l100_pb_mwl_wind01_Strut1.csv",
            "fsts_l095_125km3_l000_pb_mwl_wave01_Strut1.csv"
        ]
        
        all_valid = True
        for filename in test_files:
            filepath = self.sample_data_path / filename
            print(f"\n[INFO] Checking: {filename}")
            
            if not filepath.exists():
                print(f"  [FAIL] File not found")
                all_valid = False
                continue
                
            try:
                df = pd.read_csv(filepath)
                print(f"  [INFO] Shape: {df.shape}")
                print(f"  [INFO] Columns: {df.columns.tolist()}")
                
                if 'Time' in df.columns:
                    print(f"  [INFO] Time range: {df['Time'].min():.1f} to {df['Time'].max():.1f} seconds")
                    
                tension_col = [col for col in df.columns if 'Tension' in col][0]
                print(f"  [INFO] Tension range: {df[tension_col].min():.1f} to {df[tension_col].max():.1f} kN")
                
                expected_samples = 1000
                if len(df) == expected_samples:
                    print(f"  [PASS] Correct number of samples ({expected_samples})")
                else:
                    print(f"  [FAIL] Expected {expected_samples} samples, found {len(df)}")
                    all_valid = False
                    
            except Exception as e:
                print(f"  [FAIL] Error reading file: {e}")
                all_valid = False
                
        return all_valid
    
    def step4_data_loading(self):
        """Step 4: Test data loading"""
        self.print_header(4, "DATA LOADING TEST")
        
        try:
            handler = ProductionDataHandler(self.sample_data_path)
            print("[PASS] Handler initialized")
            
            configs = ['fsts_l015', 'fsts_l095', 'fsts_l015_125km3_l100_pb', 'fsts_l095_125km3_l000_pb']
            
            print("\n[INFO] Testing each configuration:")
            for config in configs:
                print(f"\n  {config}:")
                
                ref_files = handler.get_reference_files(config)
                print(f"    Wind refs: {ref_files.get('wind', [])}")
                print(f"    Wave refs: {ref_files.get('wave', [])}")
                
                # Try loading wind01
                if 'wind01' in ref_files.get('wind', []):
                    time_data, tension_data = handler.load_strut_data(config, 'wind01', 1)
                    if len(time_data) > 0 and len(tension_data) > 0:
                        print(f"    [PASS] Loaded {len(time_data)} samples from wind01")
                    else:
                        print(f"    [FAIL] Could not load data from wind01")
                        return False
                        
            return True
            
        except Exception as e:
            print(f"[FAIL] Error during data loading: {e}")
            return False
    
    def step5_scaling_calculation(self):
        """Step 5: Test scaling calculation"""
        self.print_header(5, "SCALING CALCULATION TEST")
        
        try:
            # First create the data handler
            data_handler = ProductionDataHandler(self.sample_data_path)
            # Then create the LoadScaler with the handler
            processor = LoadScaler(data_handler)
            
            # Test scaling with specific conditions
            wind_speed = 15  # m/s
            wave_hs = 0.75    # m
            
            print(f"\n[INFO] Test condition:")
            print(f"  Wind: {wind_speed} m/s (1.5x base)")
            print(f"  Wave: Hs={wave_hs}m (1.5x base)")
            
            # Calculate expected factors
            wind_factor = (wind_speed / 10) ** 2
            wave_factor = wave_hs / 0.5
            
            print(f"\n[INFO] Expected scaling factors:")
            print(f"  Wind: {wind_factor:.2f} (speed squared)")
            print(f"  Wave: {wave_factor:.2f} (Hs ratio)")
            
            # Create a test fatigue condition
            from src.digitalmodel.structural.fatigue_apps.strut_foundation_processor import FatigueCondition
            test_condition = FatigueCondition(
                id=1,
                wind_speed=wind_speed,
                wind_dir=0,
                hs=wave_hs,
                tp=4.0,
                wave_dir=0,
                occurrence=1.0
            )
            
            # Process the fatigue condition
            scaled_tension, metadata = processor.process_fatigue_condition(
                config_name='fsts_l015',
                condition=test_condition,
                strut_num=1
            )
            
            if len(scaled_tension) > 0:
                print(f"\n[PASS] Scaling successful")
                print(f"  Generated: {len(scaled_tension)} samples")
                print(f"  Range: {scaled_tension.min():.1f} to {scaled_tension.max():.1f} kN")
                print(f"  Wind scale applied: {metadata.get('wind_scale_factor', 0):.2f}")
                print(f"  Wave scale applied: {metadata.get('wave_scale_factor', 0):.2f}")
                return True
            else:
                print(f"\n[FAIL] Scaling failed")
                return False
                
        except Exception as e:
            print(f"[FAIL] Error during scaling: {e}")
            return False
    
    def step6_output_generation(self):
        """Step 6: Test output generation"""
        self.print_header(6, "OUTPUT GENERATION TEST")
        
        output_dir = self.base_path / "output_interactive"
        output_dir.mkdir(exist_ok=True)
        
        print(f"\n[INFO] Output directory: {output_dir}")
        
        try:
            # Create data handler and processor
            data_handler = ProductionDataHandler(self.sample_data_path)
            processor = LoadScaler(data_handler)
            
            # Generate a test output file
            test_fc = "FC001"
            print(f"\n[INFO] Processing {test_fc}")
            
            # Create test condition
            from src.digitalmodel.structural.fatigue_apps.strut_foundation_processor import FatigueCondition
            test_condition = FatigueCondition(
                id=1,
                wind_speed=15,
                wind_dir=0,
                hs=0.75,
                tp=4.0,
                wave_dir=0,
                occurrence=1.0
            )
            
            # Process the condition
            scaled_tension, metadata = processor.process_fatigue_condition(
                config_name='fsts_l015',
                condition=test_condition,
                strut_num=1
            )
            
            if len(scaled_tension) > 0:
                # Create DataFrame for output
                import pandas as pd
                time_array = np.arange(len(scaled_tension)) * 0.1  # 0.1s timestep
                scaled_data = pd.DataFrame({
                    'Time': time_array,
                    'Effective Tension at Vessel End': scaled_tension
                })
                
                # Save to file
                output_file = output_dir / f"test_{test_fc}_Strut1.csv"
                scaled_data.to_csv(output_file, index=False)
                
                if output_file.exists():
                    print(f"[PASS] Output file created")
                    print(f"  File: {output_file.name}")
                    print(f"  Size: {output_file.stat().st_size} bytes")
                    
                    # Verify it's readable
                    try:
                        df = pd.read_csv(output_file)
                        print(f"  [PASS] File is valid and readable")
                        return True
                    except:
                        print(f"  [FAIL] File is not readable")
                        return False
                else:
                    print(f"[FAIL] Output file not created")
                    return False
            else:
                print(f"[FAIL] Could not generate scaled data")
                return False
                
        except Exception as e:
            print(f"[FAIL] Error during output generation: {e}")
            return False
    
    def run(self, start_from_step=1):
        """Run all steps with user confirmation"""
        steps = [
            (1, "Directory Structure", self.step1_directory_structure),
            (2, "Naming Convention", self.step2_naming_convention),
            (3, "File Content", self.step3_file_content),
            (4, "Data Loading", self.step4_data_loading),
            (5, "Scaling Calculation", self.step5_scaling_calculation),
            (6, "Output Generation", self.step6_output_generation)
        ]
        
        print("\n" + "="*60)
        print("INTERACTIVE VERIFICATION SESSION")
        print("="*60)
        
        if start_from_step > 1:
            print(f"\nStarting from Step {start_from_step} (skipping steps 1-{start_from_step-1})")
            # Mark previous steps as completed
            self.steps_completed = list(range(1, start_from_step))
        else:
            print("\nThis will run each verification step and wait for your")
            print("confirmation before proceeding to the next step.")
        
        # Only run steps from start_from_step onwards
        steps_to_run = [(num, name, func) for num, name, func in steps if num >= start_from_step]
        
        for step_num, step_name, step_func in steps_to_run:
            try:
                success = step_func()
                
                if success:
                    self.steps_completed.append(step_num)
                    
                if not self.wait_for_confirmation(step_num):
                    break
                    
            except Exception as e:
                print(f"\n[ERROR] Step {step_num} failed with error: {e}")
                if not self.wait_for_confirmation(step_num):
                    break
        
        # Summary
        print("\n" + "="*60)
        print("VERIFICATION SESSION COMPLETE")
        print("="*60)
        print(f"\nSteps completed: {self.steps_completed}")
        if len(self.steps_completed) == 6:
            print("\n✅ ALL VERIFICATION STEPS COMPLETED SUCCESSFULLY")
        else:
            print(f"\n⚠️ Completed {len(self.steps_completed)} of 6 steps")

if __name__ == "__main__":
    import sys
    start_step = 1
    if len(sys.argv) > 1:
        try:
            start_step = int(sys.argv[1])
        except:
            print("Usage: python verify_interactive.py [start_step_number]")
            sys.exit(1)
    
    verifier = InteractiveVerification()
    verifier.run(start_from_step=start_step)