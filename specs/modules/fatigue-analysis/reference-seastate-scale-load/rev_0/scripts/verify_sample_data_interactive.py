#!/usr/bin/env python
"""
Interactive Sample Data Verification
Step-by-step verification with user confirmation at each stage
"""

import sys
import os
from pathlib import Path
import pandas as pd
import numpy as np
from typing import Dict, List, Tuple
import json

# Add the src directory to the path
src_path = Path(__file__).parent.parent.parent.parent.parent / "src"
sys.path.insert(0, str(src_path))

from digitalmodel.structural.fatigue_apps.strut_foundation_processor import (
    ProductionDataHandler, LoadScaler, FatigueCondition
)

class InteractiveVerifier:
    """Interactive verification of sample data process"""
    
    def __init__(self):
        self.sample_path = Path(__file__).parent / "sample_data"
        self.step_count = 0
        self.verification_log = []
        
    def wait_for_confirmation(self, message: str = "Continue?") -> bool:
        """Wait for user confirmation"""
        print(f"\n[?] {message} (y/n): ", end="")
        response = input().strip().lower()
        return response == 'y' or response == 'yes'
    
    def show_step(self, title: str, description: str = None) -> None:
        """Display a verification step"""
        self.step_count += 1
        print("\n" + "="*60)
        print(f"STEP {self.step_count}: {title}")
        print("="*60)
        if description:
            print(description)
    
    def log_result(self, step: str, status: str, details: str = None) -> None:
        """Log verification result"""
        result = {
            'step': self.step_count,
            'name': step,
            'status': status,
            'details': details
        }
        self.verification_log.append(result)
        
        # Display result
        status_symbol = "PASS" if status == "PASS" else "FAIL"
        print(f"\n[{status_symbol}] {step}: {status}")
        if details:
            print(f"    Details: {details}")
    
    def verify_step_1_directory_structure(self) -> bool:
        """Step 1: Verify sample data directory exists and structure"""
        
        self.show_step(
            "Verify Directory Structure",
            "Checking if sample_data directory exists with flat structure"
        )
        
        # Check if directory exists
        print(f"\nChecking: {self.sample_path}")
        
        if not self.sample_path.exists():
            self.log_result("Directory exists", "FAIL", f"Path not found: {self.sample_path}")
            return False
        
        self.log_result("Directory exists", "PASS", str(self.sample_path))
        
        # Check for flat structure (no subdirectories)
        subdirs = [d for d in self.sample_path.iterdir() if d.is_dir()]
        
        if subdirs:
            print(f"\nWARNING: Found subdirectories (should be flat):")
            for subdir in subdirs:
                print(f"  - {subdir.name}")
            self.log_result("Flat structure", "FAIL", f"Found {len(subdirs)} subdirectories")
        else:
            self.log_result("Flat structure", "PASS", "No subdirectories found - correct flat structure")
        
        # List sample of files
        csv_files = list(self.sample_path.glob("*.csv"))
        print(f"\nFound {len(csv_files)} CSV files")
        
        if csv_files:
            print("\nSample files (first 5):")
            for file in csv_files[:5]:
                print(f"  - {file.name}")
        
        return self.wait_for_confirmation("Directory structure verified. Continue?")
    
    def verify_step_2_naming_convention(self) -> bool:
        """Step 2: Verify production naming convention"""
        
        self.show_step(
            "Verify Naming Convention",
            "Checking if files follow pattern: {config}_mwl_{reference}_Strut{#}.csv"
        )
        
        csv_files = list(self.sample_path.glob("*.csv"))
        
        # Expected pattern components
        configs = [
            'fsts_l015',
            'fsts_l095', 
            'fsts_l015_125km3_l100_pb',
            'fsts_l095_125km3_l000_pb'
        ]
        
        references = ['wind01', 'wave01']
        struts = range(1, 9)
        
        # Check naming pattern
        correct_names = 0
        incorrect_names = []
        
        for file in csv_files:
            filename = file.name
            
            # Check if filename matches pattern
            valid = False
            for config in configs:
                for ref in references:
                    for strut in struts:
                        expected = f"{config}_mwl_{ref}_Strut{strut}.csv"
                        if filename == expected:
                            valid = True
                            correct_names += 1
                            break
                    if valid:
                        break
                if valid:
                    break
            
            if not valid:
                incorrect_names.append(filename)
        
        print(f"\nNaming Convention Check:")
        print(f"  - Total files: {len(csv_files)}")
        print(f"  - Correct names: {correct_names}")
        print(f"  - Incorrect names: {len(incorrect_names)}")
        
        if incorrect_names:
            print(f"\nFiles with incorrect naming (first 5):")
            for name in incorrect_names[:5]:
                print(f"  - {name}")
            self.log_result("Naming convention", "FAIL", f"{len(incorrect_names)} files incorrectly named")
        else:
            self.log_result("Naming convention", "PASS", "All files follow correct pattern")
        
        # Show expected file count
        expected_count = len(configs) * len(references) * 8  # 8 struts
        print(f"\nExpected file count: {expected_count}")
        print(f"Actual file count: {len(csv_files)}")
        
        if len(csv_files) == expected_count:
            self.log_result("File count", "PASS", f"Correct count: {expected_count}")
        else:
            self.log_result("File count", "FAIL", f"Expected {expected_count}, found {len(csv_files)}")
        
        return self.wait_for_confirmation("Naming convention verified. Continue?")
    
    def verify_step_3_file_contents(self) -> bool:
        """Step 3: Verify file contents structure"""
        
        self.show_step(
            "Verify File Contents",
            "Checking CSV structure and data format"
        )
        
        # Test a sample file
        test_file = self.sample_path / "fsts_l015_mwl_wind01_Strut1.csv"
        
        if not test_file.exists():
            self.log_result("Test file exists", "FAIL", f"File not found: {test_file.name}")
            return False
        
        print(f"\nTesting file: {test_file.name}")
        
        try:
            # Read the file
            df = pd.read_csv(test_file)
            
            print(f"\nFile structure:")
            print(f"  - Columns: {list(df.columns)}")
            print(f"  - Rows: {len(df)}")
            print(f"  - Shape: {df.shape}")
            
            # Check expected columns
            if len(df.columns) >= 2:
                time_col = df.columns[0]
                tension_col = df.columns[1]
                
                print(f"\nColumn names:")
                print(f"  - Time column: '{time_col}'")
                print(f"  - Tension column: '{tension_col}'")
                
                # Check data types
                print(f"\nData types:")
                print(f"  - Time: {df[time_col].dtype}")
                print(f"  - Tension: {df[tension_col].dtype}")
                
                # Check data ranges
                print(f"\nData ranges:")
                print(f"  - Time: {df[time_col].min():.2f} to {df[time_col].max():.2f}")
                print(f"  - Tension: {df[tension_col].min():.2f} to {df[tension_col].max():.2f}")
                
                # Check time step
                if len(df) > 1:
                    time_step = df[time_col].iloc[1] - df[time_col].iloc[0]
                    print(f"  - Time step: {time_step:.3f} seconds")
                
                self.log_result("File structure", "PASS", "Correct CSV format")
            else:
                self.log_result("File structure", "FAIL", f"Expected 2+ columns, found {len(df.columns)}")
            
        except Exception as e:
            self.log_result("File reading", "FAIL", str(e))
            return False
        
        return self.wait_for_confirmation("File contents verified. Continue?")
    
    def verify_step_4_data_loading(self) -> bool:
        """Step 4: Verify data loading with ProductionDataHandler"""
        
        self.show_step(
            "Verify Data Loading",
            "Testing ProductionDataHandler with sample data"
        )
        
        try:
            # Initialize handler
            handler = ProductionDataHandler(base_path=str(self.sample_path))
            
            print(f"\nInitialized handler with base path: {handler.base_path}")
            
            # Test configurations
            print(f"\nConfigurations loaded:")
            for config_name, config in handler.configurations.items():
                print(f"  - {config_name}: {config.description} (weight: {config.weight}%)")
            
            # Test loading data for each configuration
            print(f"\nTesting data loading for each configuration:")
            
            for config_name in handler.configurations.keys():
                # Get reference files
                ref_files = handler.get_reference_files(config_name)
                
                print(f"\n{config_name}:")
                print(f"  - Wind references: {ref_files.get('wind', [])}")
                print(f"  - Wave references: {ref_files.get('wave', [])}")
                
                # Try loading a file
                if ref_files.get('wind'):
                    wind_ref = ref_files['wind'][0]
                    time_data, tension_data = handler.load_strut_data(config_name, wind_ref, 1, use_sample=False)
                    
                    if len(tension_data) > 0:
                        print(f"  - Successfully loaded {wind_ref}: {len(tension_data)} samples")
                        self.log_result(f"Load {config_name}", "PASS", f"{len(tension_data)} samples")
                    else:
                        print(f"  - Failed to load {wind_ref}")
                        self.log_result(f"Load {config_name}", "FAIL", "No data loaded")
            
        except Exception as e:
            self.log_result("Data handler", "FAIL", str(e))
            return False
        
        return self.wait_for_confirmation("Data loading verified. Continue?")
    
    def verify_step_5_scaling_process(self) -> bool:
        """Step 5: Verify scaling and combination process"""
        
        self.show_step(
            "Verify Scaling Process",
            "Testing wind/wave scaling and combination"
        )
        
        try:
            # Initialize components
            handler = ProductionDataHandler(base_path=str(self.sample_path))
            scaler = LoadScaler(handler)
            
            # Create a test fatigue condition
            test_condition = FatigueCondition(
                id=1,
                wind_speed=15.0,  # 1.5x base wind speed
                wind_dir=0.0,
                hs=0.75,  # 1.5x base Hs
                tp=2.7,
                wave_dir=0.0,
                occurrence=5.0
            )
            
            print(f"\nTest Fatigue Condition:")
            print(f"  - Wind: {test_condition.wind_speed} m/s at {test_condition.wind_dir}°")
            print(f"  - Wave: Hs={test_condition.hs}m, Tp={test_condition.tp}s at {test_condition.wave_dir}°")
            
            # Calculate scaling factors
            wind_scale = (test_condition.wind_speed / scaler.base_wind_speed) ** 2
            wave_scale = test_condition.hs / scaler.base_hs
            
            print(f"\nScaling Factors:")
            print(f"  - Wind scale: {wind_scale:.2f} (speed ratio squared)")
            print(f"  - Wave scale: {wave_scale:.2f} (Hs ratio)")
            
            # Test processing for one configuration
            config_name = 'fsts_l015'
            strut_num = 1
            
            print(f"\nProcessing {config_name}, Strut {strut_num}:")
            
            # Process the fatigue condition
            effective_tension, metadata = scaler.process_fatigue_condition(
                config_name, test_condition, strut_num
            )
            
            if len(effective_tension) > 0:
                print(f"  - Combined tension samples: {len(effective_tension)}")
                print(f"  - Min tension: {np.min(effective_tension):.2f} kN")
                print(f"  - Max tension: {np.max(effective_tension):.2f} kN")
                print(f"  - Mean tension: {np.mean(effective_tension):.2f} kN")
                
                print(f"\nMetadata:")
                for key, value in metadata.items():
                    print(f"  - {key}: {value}")
                
                self.log_result("Scaling process", "PASS", f"Generated {len(effective_tension)} samples")
            else:
                self.log_result("Scaling process", "FAIL", "No data generated")
            
        except Exception as e:
            self.log_result("Scaling test", "FAIL", str(e))
            return False
        
        return self.wait_for_confirmation("Scaling process verified. Continue?")
    
    def verify_step_6_output_generation(self) -> bool:
        """Step 6: Verify output file generation"""
        
        self.show_step(
            "Verify Output Generation",
            "Testing output file creation and format"
        )
        
        try:
            # Create output directory
            output_dir = Path(__file__).parent / "output_verification"
            output_dir.mkdir(exist_ok=True)
            
            print(f"\nOutput directory: {output_dir}")
            
            # Initialize components
            handler = ProductionDataHandler(base_path=str(self.sample_path))
            scaler = LoadScaler(handler)
            
            # Process one example
            test_condition = scaler.fatigue_conditions[0] if scaler.fatigue_conditions else None
            
            if test_condition:
                config_name = 'fsts_l015'
                strut_num = 1
                
                print(f"\nGenerating output for:")
                print(f"  - Config: {config_name}")
                print(f"  - Fatigue Condition: FC{test_condition.id:03d}")
                print(f"  - Strut: {strut_num}")
                
                # Process
                effective_tension, metadata = scaler.process_fatigue_condition(
                    config_name, test_condition, strut_num
                )
                
                if len(effective_tension) > 0:
                    # Save output
                    output_file = output_dir / f"{config_name}_FC{test_condition.id:03d}_Strut{strut_num}_combined.csv"
                    
                    df_output = pd.DataFrame({
                        'time_s': np.arange(len(effective_tension)) * 0.1,
                        'effective_tension_kN': effective_tension
                    })
                    
                    df_output.to_csv(output_file, index=False)
                    
                    print(f"\nOutput file created: {output_file.name}")
                    print(f"  - Size: {output_file.stat().st_size} bytes")
                    print(f"  - Rows: {len(df_output)}")
                    
                    # Verify output format
                    df_check = pd.read_csv(output_file)
                    print(f"\nOutput verification:")
                    print(f"  - Columns: {list(df_check.columns)}")
                    print(f"  - Data types: {df_check.dtypes.to_dict()}")
                    
                    self.log_result("Output generation", "PASS", f"Created {output_file.name}")
                else:
                    self.log_result("Output generation", "FAIL", "No data to output")
            
        except Exception as e:
            self.log_result("Output test", "FAIL", str(e))
            return False
        
        return self.wait_for_confirmation("Output generation verified. Continue?")
    
    def show_summary(self) -> None:
        """Show verification summary"""
        
        print("\n" + "="*60)
        print("VERIFICATION SUMMARY")
        print("="*60)
        
        passed = sum(1 for r in self.verification_log if r['status'] == 'PASS')
        failed = sum(1 for r in self.verification_log if r['status'] == 'FAIL')
        
        print(f"\nTotal Steps: {self.step_count}")
        print(f"Passed: {passed}")
        print(f"Failed: {failed}")
        
        if self.verification_log:
            print(f"\nDetailed Results:")
            for result in self.verification_log:
                status_symbol = "PASS" if result['status'] == "PASS" else "FAIL"
                print(f"  [{status_symbol}] Step {result['step']}: {result['name']} - {result['status']}")
                if result['details']:
                    print(f"      {result['details']}")
        
        # Save log
        log_file = Path(__file__).parent / "verification_log.json"
        with open(log_file, 'w') as f:
            json.dump(self.verification_log, f, indent=2)
        
        print(f"\nVerification log saved to: {log_file}")
    
    def run_verification(self) -> bool:
        """Run complete verification process"""
        
        print("\n" + "="*60)
        print("SAMPLE DATA VERIFICATION PROCESS")
        print("="*60)
        print("\nThis will verify the sample data step-by-step.")
        print("You will be asked to confirm at each step.")
        
        if not self.wait_for_confirmation("Ready to start verification?"):
            print("Verification cancelled.")
            return False
        
        # Run each verification step
        steps = [
            self.verify_step_1_directory_structure,
            self.verify_step_2_naming_convention,
            self.verify_step_3_file_contents,
            self.verify_step_4_data_loading,
            self.verify_step_5_scaling_process,
            self.verify_step_6_output_generation
        ]
        
        for step in steps:
            if not step():
                print("\nVerification stopped by user.")
                break
        
        # Show summary
        self.show_summary()
        
        return True

def main():
    """Main execution"""
    
    verifier = InteractiveVerifier()
    success = verifier.run_verification()
    
    if success:
        print("\nVerification process completed!")
    else:
        print("\nVerification process interrupted.")
    
    return 0 if success else 1

if __name__ == "__main__":
    sys.exit(main())