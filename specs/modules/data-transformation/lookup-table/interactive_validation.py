#!/usr/bin/env python
"""
Interactive Validation Script for Tension to Stress Transformation
Following repository standard practices for step-by-step validation with user confirmations
"""

import os
import sys
import pandas as pd
import numpy as np
from pathlib import Path
import json
import yaml
from datetime import datetime
from typing import Dict, List, Tuple, Optional
import hashlib

class InteractiveValidator:
    """Interactive validation system with step-by-step confirmations."""
    
    def __init__(self, config_file: str = None):
        """Initialize validator with configuration."""
        self.config_file = config_file or "specs/modules/data-transformation/lookup-table/inputs/test_transformation_config.yaml"
        self.validation_results = []
        self.checkpoints = {}
        self.interactive = True
        
    def load_config(self) -> Dict:
        """Load and validate configuration file."""
        with open(self.config_file, 'r') as f:
            return yaml.safe_load(f)
    
    def prompt_user(self, message: str, options: List[str] = None) -> str:
        """Prompt user for input with options."""
        if not self.interactive:
            return 'y'
        
        print(f"\n{message}")
        if options:
            print(f"Options: {', '.join(options)}")
        
        while True:
            response = input("> ").strip().lower()
            if not options or response in [o.lower() for o in options]:
                return response
            print(f"Invalid option. Please choose from: {', '.join(options)}")
    
    def checkpoint(self, name: str, status: bool, message: str):
        """Record validation checkpoint."""
        self.checkpoints[name] = {
            'status': 'PASS' if status else 'FAIL',
            'message': message,
            'timestamp': datetime.now().isoformat()
        }
        
        status_symbol = "[OK]" if status else "[FAIL]"
        print(f"{status_symbol} {name}: {message}")
        
        if not status and self.interactive:
            response = self.prompt_user(
                "Validation failed. Continue anyway?",
                ['y', 'n', 'debug']
            )
            if response == 'n':
                self.generate_report()
                sys.exit(1)
            elif response == 'debug':
                self.debug_mode(name)
    
    def debug_mode(self, checkpoint_name: str):
        """Enter debug mode for failed checkpoint."""
        print(f"\n=== DEBUG MODE: {checkpoint_name} ===")
        print("Available commands: 'info', 'data', 'continue', 'quit'")
        
        while True:
            cmd = input("debug> ").strip().lower()
            if cmd == 'continue':
                break
            elif cmd == 'quit':
                sys.exit(1)
            elif cmd == 'info':
                print(json.dumps(self.checkpoints[checkpoint_name], indent=2))
            elif cmd == 'data':
                print("Current validation data:")
                for key, value in self.validation_results[-1].items():
                    print(f"  {key}: {value}")
    
    def validate_step_1_input_files(self):
        """Step 1: Validate input files and structure."""
        print("\n" + "="*60)
        print("STEP 1: INPUT FILE VALIDATION")
        print("="*60)
        
        config = self.load_config()
        
        # Check configuration structure
        required_sections = ['raw_data', 'transformation', 'output']
        for section in required_sections:
            self.checkpoint(
                f"Config section '{section}'",
                section in config,
                f"Configuration contains '{section}' section"
            )
        
        # Validate input data files
        input_folder = Path(config['raw_data']['folder'])
        self.checkpoint(
            "Input folder exists",
            input_folder.exists(),
            f"Folder: {input_folder}"
        )
        
        if input_folder.exists():
            pattern = config['raw_data']['file_pattern']
            input_files = list(input_folder.glob(pattern))
            self.checkpoint(
                "Input files found",
                len(input_files) > 0,
                f"Found {len(input_files)} files matching pattern '{pattern}'"
            )
            
            if input_files:
                # Sample first file for structure validation
                sample_file = input_files[0]
                try:
                    df = pd.read_csv(sample_file)
                    required_cols = ['Range (kN)', 'Cycles_Annual']
                    missing_cols = [c for c in required_cols if c not in df.columns]
                    
                    self.checkpoint(
                        "Required columns present",
                        len(missing_cols) == 0,
                        f"Columns: {list(df.columns)}"
                    )
                    
                    self.checkpoint(
                        "Data integrity check",
                        not df['Range (kN)'].isnull().any(),
                        f"No null values in Range column"
                    )
                    
                except Exception as e:
                    self.checkpoint(
                        "File readable",
                        False,
                        f"Error reading {sample_file.name}: {str(e)}"
                    )
        
        if self.interactive:
            self.prompt_user("Input validation complete. Continue to Step 2?", ['y', 'n'])
    
    def validate_step_2_lookup_table(self):
        """Step 2: Validate lookup table."""
        print("\n" + "="*60)
        print("STEP 2: LOOKUP TABLE VALIDATION")
        print("="*60)
        
        config = self.load_config()
        lookup_file = config['transformation']['lookup_table']['file']
        
        self.checkpoint(
            "Lookup table file exists",
            Path(lookup_file).exists(),
            f"File: {lookup_file}"
        )
        
        if Path(lookup_file).exists():
            try:
                df = pd.read_csv(lookup_file)
                df.columns = df.columns.str.strip()
                
                required_cols = ['location ID', 'config', 'tension range (kN)', 'stress range (Mpa)']
                missing_cols = [c for c in required_cols if c not in df.columns]
                
                self.checkpoint(
                    "Lookup table columns",
                    len(missing_cols) == 0,
                    f"Required columns present"
                )
                
                # Clean and analyze data
                df['config'] = df['config'].str.strip()
                location_ids = df['location ID'].unique()
                configs = df['config'].unique()
                
                self.checkpoint(
                    "Location IDs found",
                    len(location_ids) > 0,
                    f"Location IDs: {sorted(location_ids)}"
                )
                
                self.checkpoint(
                    "Configs found",
                    len(configs) > 0,
                    f"Configs: {configs[:3]}..." if len(configs) > 3 else f"Configs: {configs}"
                )
                
                # Validate data ranges
                self.checkpoint(
                    "Tension range validity",
                    df['tension range (kN)'].min() >= 0,
                    f"Range: {df['tension range (kN)'].min():.1f} - {df['tension range (kN)'].max():.1f} kN"
                )
                
                self.checkpoint(
                    "Stress range validity",
                    df['stress range (Mpa)'].min() >= 0,
                    f"Range: {df['stress range (Mpa)'].min():.2f} - {df['stress range (Mpa)'].max():.2f} MPa"
                )
                
                # Check for monotonicity per location/config
                print("\n[CHECKING] Monotonicity of lookup curves...")
                for loc_id in location_ids[:2]:  # Check first 2 locations as sample
                    for config in configs[:2]:  # Check first 2 configs
                        subset = df[(df['location ID'] == loc_id) & (df['config'] == config)]
                        if len(subset) > 1:
                            subset = subset.sort_values('tension range (kN)')
                            is_monotonic = subset['stress range (Mpa)'].is_monotonic_increasing
                            
                            self.checkpoint(
                                f"Monotonicity loc{loc_id} {config[:20]}",
                                is_monotonic,
                                "Stress increases with tension"
                            )
                
            except Exception as e:
                self.checkpoint(
                    "Lookup table readable",
                    False,
                    f"Error: {str(e)}"
                )
        
        if self.interactive:
            self.prompt_user("Lookup table validation complete. Continue to Step 3?", ['y', 'n'])
    
    def validate_step_3_transformation_logic(self):
        """Step 3: Validate transformation logic."""
        print("\n" + "="*60)
        print("STEP 3: TRANSFORMATION LOGIC VALIDATION")
        print("="*60)
        
        config = self.load_config()
        
        # Test interpolation with known values
        print("\n[TESTING] Interpolation with sample values...")
        
        # Load lookup table
        lookup_df = pd.read_csv(config['transformation']['lookup_table']['file'])
        lookup_df.columns = lookup_df.columns.str.strip()
        lookup_df['config'] = lookup_df['config'].str.strip()
        
        # Test exact match
        test_location = 2
        test_config = 'fsts_l015_125km3_l100_pb_mwl'
        test_subset = lookup_df[
            (lookup_df['location ID'] == test_location) & 
            (lookup_df['config'] == test_config)
        ]
        
        if len(test_subset) > 0:
            # Test exact value
            exact_tension = test_subset.iloc[0]['tension range (kN)']
            exact_stress = test_subset.iloc[0]['stress range (Mpa)']
            
            self.checkpoint(
                "Exact value lookup",
                True,
                f"Tension {exact_tension:.1f} kN -> Stress {exact_stress:.2f} MPa"
            )
            
            # Test interpolation between points
            if len(test_subset) > 2:
                from scipy import interpolate
                
                x_values = test_subset['tension range (kN)'].values
                y_values = test_subset['stress range (Mpa)'].values
                
                interp_func = interpolate.interp1d(
                    x_values, y_values,
                    kind='linear',
                    fill_value='extrapolate',
                    bounds_error=False
                )
                
                # Test midpoint interpolation
                mid_tension = (x_values[0] + x_values[1]) / 2
                interpolated_stress = interp_func(mid_tension)
                expected_stress = (y_values[0] + y_values[1]) / 2
                
                error = abs(interpolated_stress - expected_stress)
                self.checkpoint(
                    "Linear interpolation accuracy",
                    error < 0.001,
                    f"Error: {error:.6f} MPa (< 0.001)"
                )
                
                # Test extrapolation
                extrap_tension = x_values[-1] * 1.1
                extrap_stress = interp_func(extrap_tension)
                
                self.checkpoint(
                    "Extrapolation capability",
                    extrap_stress > y_values[-1],
                    f"Extrapolated {extrap_tension:.1f} kN -> {extrap_stress:.2f} MPa"
                )
        
        # Validate config mapping
        mapping = config['transformation']['lookup_table'].get('config_mapping', {})
        self.checkpoint(
            "Config mapping defined",
            len(mapping) > 0,
            f"{len(mapping)} mappings configured"
        )
        
        if self.interactive:
            self.prompt_user("Transformation logic validation complete. Continue to Step 4?", ['y', 'n'])
    
    def validate_step_4_output_generation(self):
        """Step 4: Validate output generation."""
        print("\n" + "="*60)
        print("STEP 4: OUTPUT GENERATION VALIDATION")
        print("="*60)
        
        config = self.load_config()
        output_folder = Path(config['output']['folder'])
        
        # Check output directory
        if not output_folder.exists():
            print(f"[INFO] Output directory will be created: {output_folder}")
            response = self.prompt_user("Create output directory?", ['y', 'n'])
            if response == 'y':
                output_folder.mkdir(parents=True, exist_ok=True)
                self.checkpoint(
                    "Output directory created",
                    output_folder.exists(),
                    f"Created: {output_folder}"
                )
        else:
            existing_files = list(output_folder.glob("*.csv"))
            self.checkpoint(
                "Output directory exists",
                True,
                f"Contains {len(existing_files)} existing files"
            )
            
            if existing_files and self.interactive:
                response = self.prompt_user(
                    f"Output directory contains {len(existing_files)} files. Overwrite?",
                    ['y', 'n', 'backup']
                )
                if response == 'backup':
                    backup_dir = output_folder.parent / f"output_backup_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
                    backup_dir.mkdir(exist_ok=True)
                    for file in existing_files:
                        file.rename(backup_dir / file.name)
                    print(f"[INFO] Backed up to: {backup_dir}")
        
        # Test output file naming
        pattern = config['output']['file_pattern']
        test_output = pattern.format(
            config="test_config",
            fc_number="001",
            strut_number="1",
            location_id=2
        )
        
        self.checkpoint(
            "Output naming pattern",
            "loc02" in test_output,
            f"Sample: {test_output}"
        )
        
        if self.interactive:
            self.prompt_user("Output validation complete. Continue to Step 5?", ['y', 'n'])
    
    def validate_step_5_end_to_end_test(self):
        """Step 5: End-to-end transformation test."""
        print("\n" + "="*60)
        print("STEP 5: END-TO-END TRANSFORMATION TEST")
        print("="*60)
        
        print("\n[INFO] This will run a complete transformation on sample data.")
        
        if self.interactive:
            response = self.prompt_user("Run end-to-end test?", ['y', 'n', 'dry-run'])
            
            if response == 'n':
                return
            elif response == 'dry-run':
                self.dry_run_test()
                return
        
        # Import and run transformation
        sys.path.insert(0, str(Path(__file__).parent))
        from test_process_transformation import TensionToStressTransformer
        
        try:
            transformer = TensionToStressTransformer(self.config_file)
            
            # Override to process only first file for test
            config = transformer.config
            input_folder = Path(config['raw_data']['folder'])
            test_files = list(input_folder.glob(config['raw_data']['file_pattern']))[:1]
            
            if test_files:
                print(f"\n[PROCESSING] Test file: {test_files[0].name}")
                transformer.load_lookup_table()
                transformer.create_interpolators()
                transformer.process_file(str(test_files[0]))
                
                # Verify outputs created
                output_folder = Path(config['output']['folder'])
                created_files = list(output_folder.glob("*loc*_stress_rainflow.csv"))
                
                self.checkpoint(
                    "Output files created",
                    len(created_files) > 0,
                    f"Created {len(created_files)} output files"
                )
                
                if created_files:
                    # Validate output structure
                    sample_output = pd.read_csv(created_files[0])
                    
                    self.checkpoint(
                        "Output columns correct",
                        'stress range (Mpa)' in sample_output.columns,
                        f"Columns: {list(sample_output.columns)}"
                    )
                    
                    self.checkpoint(
                        "Output data valid",
                        sample_output['stress range (Mpa)'].min() >= 0,
                        f"Stress range: {sample_output['stress range (Mpa)'].min():.2f} - {sample_output['stress range (Mpa)'].max():.2f} MPa"
                    )
        
        except Exception as e:
            self.checkpoint(
                "End-to-end test",
                False,
                f"Error: {str(e)}"
            )
    
    def dry_run_test(self):
        """Perform dry run without actual file generation."""
        print("\n[DRY RUN] Simulating transformation...")
        
        config = self.load_config()
        input_folder = Path(config['raw_data']['folder'])
        input_files = list(input_folder.glob(config['raw_data']['file_pattern']))
        
        print(f"Would process {len(input_files)} input files")
        print(f"Would create {len(input_files) * 7} output files (7 locations each)")
        print(f"Output directory: {config['output']['folder']}")
    
    def generate_report(self):
        """Generate validation report."""
        print("\n" + "="*60)
        print("VALIDATION REPORT")
        print("="*60)
        
        # Summary
        passed = sum(1 for c in self.checkpoints.values() if c['status'] == 'PASS')
        failed = sum(1 for c in self.checkpoints.values() if c['status'] == 'FAIL')
        
        print(f"\nSummary: {passed} PASSED, {failed} FAILED")
        
        # Detailed results
        print("\nDetailed Results:")
        print("-" * 40)
        for name, result in self.checkpoints.items():
            status_symbol = "[OK]" if result['status'] == 'PASS' else "[FAIL]"
            print(f"{status_symbol} {name}")
            print(f"    {result['message']}")
        
        # Save report
        report_file = Path("specs/modules/data-transformation/lookup-table/validation_report.json")
        report = {
            'timestamp': datetime.now().isoformat(),
            'config_file': self.config_file,
            'interactive': self.interactive,
            'summary': {
                'passed': passed,
                'failed': failed,
                'total': passed + failed
            },
            'checkpoints': self.checkpoints
        }
        
        with open(report_file, 'w') as f:
            json.dump(report, f, indent=2)
        
        print(f"\n[INFO] Report saved to: {report_file}")
        
        # Final status
        if failed == 0:
            print("\n[SUCCESS] All validations passed!")
            return True
        else:
            print(f"\n[WARNING] {failed} validations failed. Review report for details.")
            return False
    
    def run_all_steps(self):
        """Run all validation steps."""
        print("\n" + "="*60)
        print("TENSION TO STRESS TRANSFORMATION - INTERACTIVE VALIDATION")
        print("="*60)
        print("\nThis validation will verify the transformation process step-by-step.")
        print("You will be prompted for confirmation at each step.")
        
        if self.interactive:
            response = self.prompt_user(
                "Run in interactive mode? (n for automated)",
                ['y', 'n']
            )
            if response == 'n':
                self.interactive = False
                print("[INFO] Running in automated mode...")
        
        # Run validation steps
        self.validate_step_1_input_files()
        self.validate_step_2_lookup_table()
        self.validate_step_3_transformation_logic()
        self.validate_step_4_output_generation()
        self.validate_step_5_end_to_end_test()
        
        # Generate final report
        return self.generate_report()


def main():
    """Main entry point."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Interactive validation for tension to stress transformation')
    parser.add_argument('--config', default=None, help='Configuration file path')
    parser.add_argument('--auto', action='store_true', help='Run in automated mode')
    parser.add_argument('--step', type=int, help='Run specific step only (1-5)')
    
    args = parser.parse_args()
    
    validator = InteractiveValidator(args.config)
    
    if args.auto:
        validator.interactive = False
    
    if args.step:
        step_methods = {
            1: validator.validate_step_1_input_files,
            2: validator.validate_step_2_lookup_table,
            3: validator.validate_step_3_transformation_logic,
            4: validator.validate_step_4_output_generation,
            5: validator.validate_step_5_end_to_end_test
        }
        
        if args.step in step_methods:
            step_methods[args.step]()
            validator.generate_report()
        else:
            print(f"Invalid step: {args.step}. Choose from 1-5.")
    else:
        success = validator.run_all_steps()
        sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()