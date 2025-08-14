"""
Manual Testing Interface for OrcaFlex Browser
Task 1.7: Interactive validation with user interaction
"""

import os
import sys
from pathlib import Path
import pandas as pd
import json
from datetime import datetime
from typing import Dict, Optional, List
import time

# Add backend to path
sys.path.insert(0, str(Path(__file__).parent / 'src' / 'backend'))

from max_force_finder import MaxForceFinder, ModeController
from validation_service import ConfigurationValidator
from file_search_engine import FileSearchEngine
from pattern_engine import PatternEngine


class ManualTestInterface:
    """Interactive manual testing interface for browser functionality"""
    
    def __init__(self):
        self.test_results = {}
        self.current_folder = None
        self.max_finder = None
        self.mode_controller = None
        self.validator = ConfigurationValidator()
        self.current_config = {}
        
    def print_header(self, text):
        """Print formatted header"""
        print("\n" + "="*60)
        print(f" {text}")
        print("="*60)
        
    def print_step(self, step_num, description):
        """Print test step"""
        print(f"\n[STEP {step_num}] {description}")
        print("-" * 50)
        
    def get_user_input(self, prompt, options=None):
        """Get user input with optional validation"""
        if options:
            print(f"Options: {', '.join(options)}")
        
        while True:
            response = input(f"{prompt}: ").strip()
            if not options or response in options:
                return response
            print(f"Invalid input. Please choose from: {', '.join(options)}")
    
    def verify_with_user(self, question):
        """Get yes/no verification from user"""
        response = self.get_user_input(f"{question} (y/n)", ["y", "n", "yes", "no"])
        return response.lower() in ["y", "yes"]
    
    def run_task_1_7_2(self):
        """Task 1.7.2: User selects the folder"""
        self.print_step("1.7.2", "User Folder Selection")
        
        print("Please select a folder containing OrcaFlex CSV output files.")
        print("\nDefault test locations:")
        print("1. D:/1522/ctr7/orcaflex/rev_a08/output/csv")
        print("2. ./sample_orcaflex_data/output/csv")
        print("3. Custom path")
        
        choice = self.get_user_input("Select option (1/2/3)", ["1", "2", "3"])
        
        if choice == "1":
            folder_path = Path("D:/1522/ctr7/orcaflex/rev_a08/output/csv")
        elif choice == "2":
            folder_path = Path(__file__).parent / "sample_orcaflex_data" / "output" / "csv"
        else:
            custom_path = input("Enter full path to CSV folder: ").strip()
            folder_path = Path(custom_path)
        
        # Verify folder exists
        if not folder_path.exists():
            print(f"[WARNING] Folder does not exist: {folder_path}")
            create = self.verify_with_user("Create sample data in this location?")
            if create:
                folder_path.mkdir(parents=True, exist_ok=True)
                self.create_sample_data(folder_path)
        
        # Count CSV files
        csv_files = list(folder_path.glob("*.csv"))
        print(f"\n[INFO] Found {len(csv_files)} CSV files in: {folder_path}")
        
        if csv_files:
            print("\nSample files found:")
            for i, file in enumerate(csv_files[:5], 1):
                print(f"  {i}. {file.name}")
            if len(csv_files) > 5:
                print(f"  ... and {len(csv_files) - 5} more files")
        
        self.current_folder = folder_path
        self.test_results["1.7.2"] = {
            "status": "PASS" if csv_files else "FAIL",
            "folder": str(folder_path),
            "file_count": len(csv_files)
        }
        
        verified = self.verify_with_user("\nIs the folder selection correct?")
        if verified:
            print("[PASS] Task 1.7.2: Folder selection verified by user")
        else:
            print("[FAIL] Task 1.7.2: User indicated folder selection issue")
            self.test_results["1.7.2"]["user_verified"] = verified
        
        return folder_path
    
    def run_task_1_7_3(self):
        """Task 1.7.3: Program implements auto-max mode"""
        self.print_step("1.7.3", "Auto-Max Mode Implementation")
        
        if not self.current_folder:
            print("[ERROR] No folder selected. Run Task 1.7.2 first.")
            return
        
        print("Implementing max-strut-tension (auto-max) mode...")
        print(f"Analyzing files in: {self.current_folder}")
        
        # Initialize max finder
        self.max_finder = MaxForceFinder(base_path=self.current_folder.parent.parent)
        self.mode_controller = ModeController(self.max_finder)
        
        # Find maximum force
        print("\nSearching for maximum forces...")
        max_result = self.max_finder.find_maximum_force_configuration(
            folder=self.current_folder
        )
        
        if max_result:
            print(f"\n[RESULT] Maximum force configuration found:")
            print(f"  Max Force: {max_result.max_force:.2f}")
            print(f"  Force Column: {max_result.force_column}")
            print(f"  File: {max_result.fe_filename}")
            print(f"  Source: {max_result.source_file.name}")
            
            print(f"\n[CONFIG] Extracted configuration:")
            for key, value in max_result.configuration.items():
                if value:
                    print(f"  {key}: {value}")
            
            self.test_results["1.7.3"] = {
                "status": "PASS",
                "max_force": max_result.max_force,
                "file": max_result.fe_filename,
                "configuration": max_result.configuration
            }
        else:
            print("[WARNING] No maximum force configuration found")
            self.test_results["1.7.3"] = {"status": "FAIL", "reason": "No max found"}
        
        verified = self.verify_with_user("\nDoes the auto-max mode correctly identify the maximum?")
        self.test_results["1.7.3"]["user_verified"] = verified
        
        if verified:
            print("[PASS] Task 1.7.3: Auto-max mode verified by user")
        else:
            print("[FAIL] Task 1.7.3: User indicated auto-max issue")
    
    def run_task_1_7_4(self):
        """Task 1.7.4: User verifies auto-max mode"""
        self.print_step("1.7.4", "Verify Auto-Max Mode")
        
        if not self.mode_controller:
            print("[ERROR] Auto-max not initialized. Run Task 1.7.3 first.")
            return
        
        # Get auto-max configuration
        auto_config = self.mode_controller.switch_to_auto()
        
        print("\n[1.7.4.1] Verify File Selection")
        print("-" * 40)
        if auto_config and 'fe_filename' in auto_config:
            print(f"Selected file: {auto_config['fe_filename']}")
            file_correct = self.verify_with_user("Is this the correct file?")
            self.test_results["1.7.4.1"] = {
                "status": "PASS" if file_correct else "FAIL",
                "file": auto_config.get('fe_filename'),
                "user_verified": file_correct
            }
        
        print("\n[1.7.4.2] Verify UI Options")
        print("-" * 40)
        print("Current UI options for auto-max mode:")
        
        config = auto_config.get('configuration', {})
        self.current_config = config
        
        ui_display = {
            'Vessel Type': config.get('vessel_type', 'N/A'),
            'Loading Condition': config.get('loading_condition', 'N/A'),
            'Tide Level': config.get('tide_level', 'N/A'),
            'Return Period': config.get('return_period', 'N/A'),
            'Wave Direction': config.get('wave_direction', 'N/A'),
            'Analysis Type': config.get('analysis_type', 'N/A')
        }
        
        for label, value in ui_display.items():
            print(f"  {label}: {value}")
        
        ui_correct = self.verify_with_user("\nAre these UI options correct for the selected file?")
        self.test_results["1.7.4.2"] = {
            "status": "PASS" if ui_correct else "FAIL",
            "ui_options": ui_display,
            "user_verified": ui_correct
        }
        
        if file_correct and ui_correct:
            print("\n[PASS] Task 1.7.4: Auto-max mode fully verified")
        else:
            print("\n[PARTIAL] Task 1.7.4: Some issues identified in auto-max mode")
    
    def run_task_1_7_5(self):
        """Task 1.7.5: User changes UI options for manual mode"""
        self.print_step("1.7.5", "Switch to Manual Mode")
        
        if not self.mode_controller:
            print("[ERROR] Mode controller not initialized. Run Task 1.7.3 first.")
            return
        
        print("Current configuration (from auto-max):")
        for key, value in self.current_config.items():
            print(f"  {key}: {value}")
        
        print("\n[MANUAL MODE] Enter new parameters (press Enter to keep current):")
        
        manual_config = {}
        
        # Vessel Type
        vessel = input(f"Vessel Type [{self.current_config.get('vessel_type', 'fsts')}]: ").strip()
        manual_config['vessel_type'] = vessel or self.current_config.get('vessel_type', 'fsts')
        
        # Loading Condition
        loading = input(f"Loading Condition [{self.current_config.get('loading_condition', 'l015')}]: ").strip()
        manual_config['loading_condition'] = loading or self.current_config.get('loading_condition', 'l015')
        
        # Tide Level
        tide = input(f"Tide Level (hwl/mwl/lwl) [{self.current_config.get('tide_level', 'hwl')}]: ").strip()
        manual_config['tide_level'] = tide or self.current_config.get('tide_level', 'hwl')
        
        # Return Period
        period = input(f"Return Period [{self.current_config.get('return_period', '0100yr')}]: ").strip()
        manual_config['return_period'] = period or self.current_config.get('return_period', '0100yr')
        
        # Wave Direction
        wave = input(f"Wave Direction [{self.current_config.get('wave_direction', '000deg')}]: ").strip()
        manual_config['wave_direction'] = wave or self.current_config.get('wave_direction', '000deg')
        
        # Analysis Type
        analysis = input(f"Analysis Type [{self.current_config.get('analysis_type', '03c')}]: ").strip()
        manual_config['analysis_type'] = analysis or self.current_config.get('analysis_type', '03c')
        
        # Switch to manual mode
        print("\nSwitching to manual mode with new configuration...")
        manual_result = self.mode_controller.switch_to_manual(manual_config)
        
        print("\n[MANUAL MODE] New configuration:")
        for key, value in manual_config.items():
            print(f"  {key}: {value}")
        
        self.current_config = manual_config
        self.test_results["1.7.5"] = {
            "status": "PASS",
            "manual_config": manual_config,
            "switched": True
        }
        
        verified = self.verify_with_user("\nHave the UI options been successfully changed to manual mode?")
        self.test_results["1.7.5"]["user_verified"] = verified
        
        if verified:
            print("[PASS] Task 1.7.5: Manual mode switch verified")
        else:
            print("[FAIL] Task 1.7.5: Issue with manual mode switch")
    
    def run_task_1_7_6(self):
        """Task 1.7.6: User verifies manual mode"""
        self.print_step("1.7.6", "Verify Manual Mode")
        
        if not self.mode_controller:
            print("[ERROR] Mode controller not initialized.")
            return
        
        print("\n[1.7.6.1] Verify Manual File Selection")
        print("-" * 40)
        
        # Build expected filename pattern
        pattern_parts = []
        for key in ['vessel_type', 'analysis_type', 'return_period', 
                   'loading_condition', 'tide_level', 'wave_direction']:
            if key in self.current_config and self.current_config[key]:
                pattern_parts.append(self.current_config[key])
        
        expected_pattern = '_'.join(pattern_parts)
        print(f"Expected file pattern: {expected_pattern}*.csv")
        
        # Check if files matching this pattern exist
        if self.current_folder:
            matching_files = list(self.current_folder.glob(f"*{pattern_parts[0]}*{pattern_parts[-1]}*.csv"))
            if matching_files:
                print(f"Found {len(matching_files)} matching files:")
                for file in matching_files[:3]:
                    print(f"  - {file.name}")
            else:
                print("No files found matching the manual configuration")
        
        file_correct = self.verify_with_user("Is the file selection correct for manual mode?")
        self.test_results["1.7.6.1"] = {
            "status": "PASS" if file_correct else "FAIL",
            "pattern": expected_pattern,
            "user_verified": file_correct
        }
        
        print("\n[1.7.6.2] Verify Manual UI Options")
        print("-" * 40)
        print("Current UI options in manual mode:")
        
        for key, value in self.current_config.items():
            print(f"  {key}: {value}")
        
        ui_correct = self.verify_with_user("\nAre the UI options correctly displayed for manual mode?")
        self.test_results["1.7.6.2"] = {
            "status": "PASS" if ui_correct else "FAIL",
            "ui_options": self.current_config,
            "user_verified": ui_correct
        }
        
        if file_correct and ui_correct:
            print("\n[PASS] Task 1.7.6: Manual mode fully verified")
        else:
            print("\n[PARTIAL] Task 1.7.6: Some issues identified in manual mode")
    
    def run_task_1_7_7(self):
        """Task 1.7.7: Additional tests"""
        self.print_step("1.7.7", "Additional Tests (Optional)")
        
        print("Select additional tests to run:")
        print("1. Test mode switching persistence")
        print("2. Test configuration validation")
        print("3. Test error handling")
        print("4. Skip additional tests")
        
        choice = self.get_user_input("Select option (1/2/3/4)", ["1", "2", "3", "4"])
        
        if choice == "1":
            print("\n[TEST] Mode Switching Persistence")
            print("Switching back to auto-max mode...")
            auto_config = self.mode_controller.switch_to_auto()
            print("Switched to auto-max")
            
            print("Switching back to manual mode...")
            manual_config = self.mode_controller.switch_to_manual()
            print("Configuration retained:", manual_config['configuration'] == self.current_config)
            
            verified = self.verify_with_user("Did the manual configuration persist correctly?")
            self.test_results["1.7.7.persistence"] = {"status": "PASS" if verified else "FAIL"}
            
        elif choice == "2":
            print("\n[TEST] Configuration Validation")
            invalid_config = {
                'vessel_type': 'invalid_vessel',
                'loading_condition': 'xyz',
                'tide_level': 'high'
            }
            
            print("Testing invalid configuration:")
            for key, value in invalid_config.items():
                print(f"  {key}: {value}")
            
            report = self.validator.validate_configuration(invalid_config)
            print(f"\nValidation result: {'PASS' if report.overall_valid else 'FAIL'}")
            print(f"Errors found: {len(report.get_errors())}")
            
            for error in report.get_errors()[:3]:
                print(f"  - {error.message}")
            
            self.test_results["1.7.7.validation"] = {
                "status": "PASS",
                "errors_caught": len(report.get_errors())
            }
            
        elif choice == "3":
            print("\n[TEST] Error Handling")
            print("Testing with non-existent folder...")
            
            invalid_folder = Path("/nonexistent/folder")
            files = self.max_finder.find_summary_files(invalid_folder)
            
            print(f"Result: {len(files)} files found (expected: 0)")
            print("Error handling: PASS" if len(files) == 0 else "FAIL")
            
            self.test_results["1.7.7.error_handling"] = {
                "status": "PASS" if len(files) == 0 else "FAIL"
            }
        
        if choice != "4":
            print("\n[COMPLETE] Task 1.7.7: Additional tests completed")
    
    def create_sample_data(self, folder):
        """Create sample CSV files for testing"""
        print("\nCreating sample test data...")
        
        # Sample data 1: Summary file
        data1 = {
            'fe_filename': [
                'dm_fsts_03c_0100yr_l015_hwl_000deg',
                'dm_fsts_03c_0100yr_l095_hwl_045deg',
                'dm_fsts_04a_0010yr_l015_mwl_090deg'
            ],
            'Strut1_Body_eff_tension_max': [1500.5, 2000.3, 1200.8],
            'Strut2_Body_eff_tension_max': [1450.2, 2100.7, 1150.4]
        }
        df1 = pd.DataFrame(data1)
        df1.to_csv(folder / 'dm_fsts_03c_0100yr_strut_dyn.csv', index=False)
        
        # Sample data 2: Individual file
        data2 = {
            'fe_filename': ['fsts_03c_0100yr_l095_hwl_Strut1'],
            'max_tension': [2100.7]
        }
        df2 = pd.DataFrame(data2)
        df2.to_csv(folder / 'fsts_03c_0100yr_l095_hwl_Strut1.csv', index=False)
        
        print(f"Created 2 sample CSV files in {folder}")
    
    def generate_report(self):
        """Generate final test report"""
        self.print_header("MANUAL TEST REPORT")
        
        print(f"\nTest Date: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print(f"Test Results Summary:")
        print("-" * 40)
        
        passed = 0
        failed = 0
        
        for task_id, result in self.test_results.items():
            status = result.get('status', 'UNKNOWN')
            verified = result.get('user_verified', None)
            
            if status == 'PASS' and verified is not False:
                passed += 1
                symbol = "[PASS]"
            else:
                failed += 1
                symbol = "[FAIL]"
            
            print(f"{symbol} Task {task_id}: {status}")
            if verified is not None:
                print(f"      User Verified: {'Yes' if verified else 'No'}")
        
        print("-" * 40)
        print(f"Total Passed: {passed}")
        print(f"Total Failed: {failed}")
        print(f"Success Rate: {(passed/(passed+failed)*100):.1f}%" if (passed+failed) > 0 else "N/A")
        
        # Save report to file
        report_file = Path(__file__).parent / f"manual_test_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        with open(report_file, 'w') as f:
            json.dump({
                'timestamp': datetime.now().isoformat(),
                'results': self.test_results,
                'summary': {
                    'passed': passed,
                    'failed': failed,
                    'success_rate': (passed/(passed+failed)*100) if (passed+failed) > 0 else 0
                }
            }, f, indent=2)
        
        print(f"\n[INFO] Report saved to: {report_file}")
        
        return report_file
    
    def run_all_tests(self):
        """Run all manual tests in sequence"""
        self.print_header("ORCAFLEX BROWSER INTERFACE MANUAL TESTING")
        print("Task 1.7: Manual Testing and Validation")
        print("\nThis is an interactive test. Please follow the prompts.")
        
        try:
            # Run each task
            self.run_task_1_7_2()  # Folder selection
            
            if self.test_results.get("1.7.2", {}).get("status") == "PASS":
                self.run_task_1_7_3()  # Auto-max implementation
                
                if self.test_results.get("1.7.3", {}).get("status") == "PASS":
                    self.run_task_1_7_4()  # Verify auto-max
                    self.run_task_1_7_5()  # Switch to manual
                    self.run_task_1_7_6()  # Verify manual
                    
                    # Optional additional tests
                    if self.verify_with_user("\nRun additional tests (Task 1.7.7)?"):
                        self.run_task_1_7_7()
            
            # Generate report
            self.generate_report()
            
            print("\n" + "="*60)
            print(" MANUAL TESTING COMPLETE")
            print("="*60)
            
        except KeyboardInterrupt:
            print("\n\n[INTERRUPTED] Test cancelled by user")
            self.generate_report()
        except Exception as e:
            print(f"\n[ERROR] Test failed with error: {e}")
            import traceback
            traceback.print_exc()
            self.generate_report()


def main():
    """Main entry point for manual testing"""
    tester = ManualTestInterface()
    tester.run_all_tests()


if __name__ == "__main__":
    main()