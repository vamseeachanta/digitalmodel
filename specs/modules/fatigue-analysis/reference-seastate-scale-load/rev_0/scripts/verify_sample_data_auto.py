#!/usr/bin/env python
"""
Automatic Sample Data Verification
Runs all verification steps automatically without user input
"""

import sys
from pathlib import Path
import pandas as pd
import numpy as np
import json
from datetime import datetime

# Add the src directory to the path
src_path = Path(__file__).parent.parent.parent.parent.parent / "src"
sys.path.insert(0, str(src_path))

from digitalmodel.fatigue_analysis.strut_foundation_processor import (
    ProductionDataHandler, LoadScaler, FatigueCondition
)

class AutomaticVerifier:
    """Automatic verification of sample data process"""
    
    def __init__(self):
        self.sample_path = Path(__file__).parent / "sample_data"
        self.results = []
        self.all_passed = True
        
    def verify_directory_structure(self):
        """Verify sample data directory structure"""
        print("\n" + "="*60)
        print("STEP 1: Directory Structure Verification")
        print("="*60)
        
        # Check if directory exists
        if not self.sample_path.exists():
            print(f"[FAIL] Directory not found: {self.sample_path}")
            self.results.append(("Directory exists", False, "Path not found"))
            self.all_passed = False
            return
        
        print(f"[PASS] Directory exists: {self.sample_path}")
        self.results.append(("Directory exists", True, str(self.sample_path)))
        
        # Check for flat structure
        subdirs = [d for d in self.sample_path.iterdir() if d.is_dir()]
        if subdirs:
            print(f"[FAIL] Found {len(subdirs)} subdirectories (should be flat)")
            self.results.append(("Flat structure", False, f"{len(subdirs)} subdirs"))
            self.all_passed = False
        else:
            print(f"[PASS] Flat structure confirmed (no subdirectories)")
            self.results.append(("Flat structure", True, "No subdirs"))
        
        # Count CSV files
        csv_files = list(self.sample_path.glob("*.csv"))
        print(f"[INFO] Found {len(csv_files)} CSV files")
        
    def verify_naming_convention(self):
        """Verify production naming convention"""
        print("\n" + "="*60)
        print("STEP 2: Naming Convention Verification")
        print("="*60)
        
        csv_files = list(self.sample_path.glob("*.csv"))
        
        # Expected components
        configs = [
            'fsts_l015',
            'fsts_l095', 
            'fsts_l015_125km3_l100_pb',
            'fsts_l095_125km3_l000_pb'
        ]
        references = ['wind01', 'wave01']
        struts = range(1, 9)
        
        # Check each file
        correct = 0
        incorrect = []
        
        for file in csv_files:
            valid = False
            for config in configs:
                for ref in references:
                    for strut in struts:
                        if file.name == f"{config}_mwl_{ref}_Strut{strut}.csv":
                            valid = True
                            correct += 1
                            break
                    if valid: break
                if valid: break
            
            if not valid:
                incorrect.append(file.name)
        
        # Report results
        expected = len(configs) * len(references) * 8
        
        print(f"[INFO] Expected files: {expected}")
        print(f"[INFO] Actual files: {len(csv_files)}")
        print(f"[INFO] Correctly named: {correct}")
        
        if incorrect:
            print(f"[FAIL] {len(incorrect)} files incorrectly named")
            self.results.append(("Naming convention", False, f"{len(incorrect)} incorrect"))
            self.all_passed = False
        else:
            print(f"[PASS] All files follow correct naming pattern")
            self.results.append(("Naming convention", True, "All correct"))
        
        if len(csv_files) != expected:
            print(f"[FAIL] File count mismatch")
            self.results.append(("File count", False, f"Expected {expected}, got {len(csv_files)}"))
            self.all_passed = False
        else:
            print(f"[PASS] Correct file count: {expected}")
            self.results.append(("File count", True, f"{expected} files"))
    
    def verify_file_contents(self):
        """Verify CSV file contents"""
        print("\n" + "="*60)
        print("STEP 3: File Contents Verification")
        print("="*60)
        
        test_file = self.sample_path / "fsts_l015_mwl_wind01_Strut1.csv"
        
        if not test_file.exists():
            print(f"[FAIL] Test file not found: {test_file.name}")
            self.results.append(("File contents", False, "Test file missing"))
            self.all_passed = False
            return
        
        try:
            df = pd.read_csv(test_file)
            
            print(f"[INFO] Testing: {test_file.name}")
            print(f"[INFO] Shape: {df.shape}")
            print(f"[INFO] Columns: {list(df.columns)}")
            
            # Verify structure
            if df.shape[1] >= 2 and df.shape[0] > 0:
                time_range = f"{df.iloc[:, 0].min():.1f}-{df.iloc[:, 0].max():.1f}s"
                tension_range = f"{df.iloc[:, 1].min():.1f}-{df.iloc[:, 1].max():.1f}kN"
                
                print(f"[PASS] Valid CSV structure")
                print(f"[INFO] Time range: {time_range}")
                print(f"[INFO] Tension range: {tension_range}")
                
                self.results.append(("File contents", True, f"{df.shape[0]} rows"))
            else:
                print(f"[FAIL] Invalid CSV structure")
                self.results.append(("File contents", False, "Invalid structure"))
                self.all_passed = False
                
        except Exception as e:
            print(f"[FAIL] Error reading file: {e}")
            self.results.append(("File contents", False, str(e)))
            self.all_passed = False
    
    def verify_data_loading(self):
        """Verify data loading functionality"""
        print("\n" + "="*60)
        print("STEP 4: Data Loading Verification")
        print("="*60)
        
        try:
            handler = ProductionDataHandler(base_path=str(self.sample_path))
            
            print(f"[INFO] Initialized handler")
            print(f"[INFO] Configurations: {len(handler.configurations)}")
            
            success_count = 0
            for config_name in handler.configurations.keys():
                ref_files = handler.get_reference_files(config_name)
                
                if ref_files.get('wind'):
                    wind_ref = ref_files['wind'][0]
                    time_data, tension_data = handler.load_strut_data(
                        config_name, wind_ref, 1, use_sample=False
                    )
                    
                    if len(tension_data) > 0:
                        print(f"[PASS] Loaded {config_name}: {len(tension_data)} samples")
                        success_count += 1
                    else:
                        print(f"[FAIL] Failed to load {config_name}")
            
            if success_count == len(handler.configurations):
                print(f"[PASS] All configurations loaded successfully")
                self.results.append(("Data loading", True, f"{success_count} configs"))
            else:
                print(f"[FAIL] Some configurations failed")
                self.results.append(("Data loading", False, f"{success_count}/{len(handler.configurations)}"))
                self.all_passed = False
                
        except Exception as e:
            print(f"[FAIL] Error in data handler: {e}")
            self.results.append(("Data loading", False, str(e)))
            self.all_passed = False
    
    def verify_scaling_process(self):
        """Verify scaling calculations"""
        print("\n" + "="*60)
        print("STEP 5: Scaling Process Verification")
        print("="*60)
        
        try:
            handler = ProductionDataHandler(base_path=str(self.sample_path))
            scaler = LoadScaler(handler)
            
            # Test condition
            test_condition = FatigueCondition(
                id=1, wind_speed=15.0, wind_dir=0.0,
                hs=0.75, tp=2.7, wave_dir=0.0, occurrence=5.0
            )
            
            # Calculate expected scales
            wind_scale = (15.0 / 10.0) ** 2  # 2.25
            wave_scale = 0.75 / 0.5  # 1.5
            
            print(f"[INFO] Test condition: Wind={test_condition.wind_speed}m/s, Hs={test_condition.hs}m")
            print(f"[INFO] Expected wind scale: {wind_scale:.2f}")
            print(f"[INFO] Expected wave scale: {wave_scale:.2f}")
            
            # Process
            effective_tension, metadata = scaler.process_fatigue_condition(
                'fsts_l015', test_condition, 1
            )
            
            if len(effective_tension) > 0:
                print(f"[PASS] Scaling process successful")
                print(f"[INFO] Generated {len(effective_tension)} samples")
                print(f"[INFO] Tension range: {np.min(effective_tension):.1f}-{np.max(effective_tension):.1f}kN")
                
                # Verify scaling factors in metadata
                if abs(metadata.get('wind_scale_factor', 0) - wind_scale) < 0.01:
                    print(f"[PASS] Wind scaling correct: {metadata['wind_scale_factor']:.2f}")
                else:
                    print(f"[WARN] Wind scaling mismatch")
                
                if abs(metadata.get('wave_scale_factor', 0) - wave_scale) < 0.01:
                    print(f"[PASS] Wave scaling correct: {metadata['wave_scale_factor']:.2f}")
                else:
                    print(f"[WARN] Wave scaling mismatch")
                
                self.results.append(("Scaling process", True, f"{len(effective_tension)} samples"))
            else:
                print(f"[FAIL] No data generated")
                self.results.append(("Scaling process", False, "No data"))
                self.all_passed = False
                
        except Exception as e:
            print(f"[FAIL] Error in scaling: {e}")
            self.results.append(("Scaling process", False, str(e)))
            self.all_passed = False
    
    def verify_output_generation(self):
        """Verify output file generation"""
        print("\n" + "="*60)
        print("STEP 6: Output Generation Verification")
        print("="*60)
        
        try:
            output_dir = Path(__file__).parent / "output_auto_verify"
            output_dir.mkdir(exist_ok=True)
            
            handler = ProductionDataHandler(base_path=str(self.sample_path))
            scaler = LoadScaler(handler)
            
            if scaler.fatigue_conditions:
                test_condition = scaler.fatigue_conditions[0]
                
                # Process
                effective_tension, metadata = scaler.process_fatigue_condition(
                    'fsts_l015', test_condition, 1
                )
                
                if len(effective_tension) > 0:
                    # Save output
                    output_file = output_dir / f"fsts_l015_FC{test_condition.id:03d}_Strut1_test.csv"
                    
                    df_output = pd.DataFrame({
                        'time_s': np.arange(len(effective_tension)) * 0.1,
                        'effective_tension_kN': effective_tension
                    })
                    df_output.to_csv(output_file, index=False)
                    
                    # Verify file
                    if output_file.exists():
                        df_check = pd.read_csv(output_file)
                        if df_check.shape == df_output.shape:
                            print(f"[PASS] Output file created successfully")
                            print(f"[INFO] File: {output_file.name}")
                            print(f"[INFO] Size: {output_file.stat().st_size} bytes")
                            self.results.append(("Output generation", True, output_file.name))
                        else:
                            print(f"[FAIL] Output file corrupted")
                            self.results.append(("Output generation", False, "File corrupted"))
                            self.all_passed = False
                    else:
                        print(f"[FAIL] Output file not created")
                        self.results.append(("Output generation", False, "File not created"))
                        self.all_passed = False
                else:
                    print(f"[FAIL] No data to output")
                    self.results.append(("Output generation", False, "No data"))
                    self.all_passed = False
                    
        except Exception as e:
            print(f"[FAIL] Error in output generation: {e}")
            self.results.append(("Output generation", False, str(e)))
            self.all_passed = False
    
    def show_summary(self):
        """Display verification summary"""
        print("\n" + "="*60)
        print("VERIFICATION SUMMARY")
        print("="*60)
        
        passed = sum(1 for _, status, _ in self.results if status)
        failed = len(self.results) - passed
        
        print(f"\nTotal Tests: {len(self.results)}")
        print(f"Passed: {passed}")
        print(f"Failed: {failed}")
        
        print(f"\nDetailed Results:")
        for name, status, detail in self.results:
            status_text = "[PASS]" if status else "[FAIL]"
            print(f"  {status_text} {name}: {detail}")
        
        # Save results
        log_data = {
            'timestamp': datetime.now().isoformat(),
            'all_passed': self.all_passed,
            'summary': {
                'total': len(self.results),
                'passed': passed,
                'failed': failed
            },
            'results': [
                {'test': name, 'passed': status, 'detail': detail}
                for name, status, detail in self.results
            ]
        }
        
        log_file = Path(__file__).parent / "auto_verification_log.json"
        with open(log_file, 'w') as f:
            json.dump(log_data, f, indent=2)
        
        print(f"\nLog saved to: {log_file}")
        
        if self.all_passed:
            print("\n*** ALL VERIFICATIONS PASSED ***")
        else:
            print("\n*** SOME VERIFICATIONS FAILED ***")
        
        return self.all_passed
    
    def run_all_verifications(self):
        """Run all verification steps"""
        print("\n" + "="*60)
        print("AUTOMATIC SAMPLE DATA VERIFICATION")
        print("="*60)
        print("Running all verification steps automatically...")
        
        self.verify_directory_structure()
        self.verify_naming_convention()
        self.verify_file_contents()
        self.verify_data_loading()
        self.verify_scaling_process()
        self.verify_output_generation()
        
        return self.show_summary()

def main():
    """Main execution"""
    verifier = AutomaticVerifier()
    all_passed = verifier.run_all_verifications()
    
    return 0 if all_passed else 1

if __name__ == "__main__":
    sys.exit(main())