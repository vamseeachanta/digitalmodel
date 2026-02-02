#!/usr/bin/env python
"""
Load Scaling Verification Script
=================================
Performs automated verification of load scaling calculations
to ensure accuracy and correctness of the implementation.
"""

import sys
import os
from pathlib import Path
import pandas as pd
import numpy as np
import yaml
from datetime import datetime
import json

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent.parent / 'src'))

from digitalmodel.fatigue_analysis.load_scaling import LoadScalingProcessor


class LoadScalingVerifier:
    """Comprehensive verification of load scaling calculations"""
    
    def __init__(self, config_file='input/load_scaling_config.yml'):
        """Initialize verifier with configuration"""
        self.config_file = config_file
        with open(config_file, 'r') as f:
            self.config = yaml.safe_load(f)
        self.errors = []
        self.warnings = []
        self.passed_tests = 0
        self.failed_tests = 0
        
    def log_error(self, message):
        """Log an error message"""
        self.errors.append(message)
        self.failed_tests += 1
        print(f"  [X] ERROR: {message}")
        
    def log_warning(self, message):
        """Log a warning message"""
        self.warnings.append(message)
        print(f"  [!] WARNING: {message}")
        
    def log_success(self, message):
        """Log a success message"""
        self.passed_tests += 1
        print(f"  [OK] {message}")
        
    def verify_input_files(self):
        """Verify all input files exist and are valid"""
        print("\n" + "="*60)
        print("1. VERIFYING INPUT FILES")
        print("="*60)
        
        # Check reference metadata
        ref_file = Path(self.config['input_data']['reference_seastate']['metadata_file'])
        if not ref_file.exists():
            self.log_error(f"Reference metadata not found: {ref_file}")
            return False
        else:
            self.log_success(f"Reference metadata found: {ref_file}")
            
        # Load and check reference metadata
        try:
            ref_df = pd.read_csv(ref_file)
            self.log_success(f"Loaded {len(ref_df)} reference seastate definitions")
            
            # Check for required columns
            required_cols = ['env reference', 'Vw [m/s]', 'Hs [m]']
            for col in required_cols:
                if col not in ref_df.columns:
                    self.log_error(f"Missing required column: {col}")
                else:
                    self.log_success(f"Found column: {col}")
                    
        except Exception as e:
            self.log_error(f"Failed to load reference metadata: {e}")
            return False
            
        # Check fatigue seastates
        fatigue_file = Path(self.config['input_data']['fatigue_seastates']['metadata_file'])
        if not fatigue_file.exists():
            self.log_error(f"Fatigue seastates not found: {fatigue_file}")
            return False
        else:
            self.log_success(f"Fatigue seastates found: {fatigue_file}")
            
        # Load and validate fatigue seastates
        try:
            fatigue_df = pd.read_csv(fatigue_file)
            self.log_success(f"Loaded {len(fatigue_df)} fatigue seastates")
            
            # Validate occurrence sum
            total = fatigue_df['Occurrence (%)'].sum()
            if abs(total - 100.0) > 0.1:
                self.log_error(f"Occurrence sum = {total:.2f}%, expected 100%")
            else:
                self.log_success(f"Occurrence sum validated: {total:.2f}%")
                
            # Check for valid ranges
            if (fatigue_df['Wind Speed (m/s)'] < 0).any():
                self.log_error("Negative wind speeds found")
            else:
                self.log_success("All wind speeds are non-negative")
                
            if (fatigue_df['Hs (m)'] < 0).any():
                self.log_error("Negative wave heights found")
            else:
                self.log_success("All wave heights are non-negative")
                
        except Exception as e:
            self.log_error(f"Failed to load fatigue seastates: {e}")
            return False
            
        # Check reference data folder
        ref_folder = Path(self.config['input_data']['reference_seastate']['data_folder'])
        if not ref_folder.exists():
            self.log_error(f"Reference data folder not found: {ref_folder}")
            return False
        else:
            ref_files = list(ref_folder.glob("*.csv"))
            self.log_success(f"Found {len(ref_files)} reference data files")
            
        return self.failed_tests == 0
        
    def verify_scaling_calculations(self):
        """Verify scaling factor calculations with examples"""
        print("\n" + "="*60)
        print("2. VERIFYING SCALING CALCULATIONS")
        print("="*60)
        
        # Load data
        fatigue_df = pd.read_csv(self.config['input_data']['fatigue_seastates']['metadata_file'])
        ref_df = pd.read_csv(self.config['input_data']['reference_seastate']['metadata_file'])
        
        # Test cases for scaling calculations
        test_cases = [
            {
                'name': 'FC001 - Low wind/wave',
                'fc_idx': 0,
                'expected_wind_factor': 0.25,  # (5/10)^2
                'expected_wave_factor': 0.3,   # 0.15/0.5
            },
            {
                'name': 'FC002 - Medium conditions', 
                'fc_idx': 1,
                'expected_wind_factor': 1.0,   # (10/10)^2
                'expected_wave_factor': 0.5,   # 0.25/0.5
            },
            {
                'name': 'FC003 - Higher conditions',
                'fc_idx': 2,
                'expected_wind_factor': 2.25,  # (15/10)^2
                'expected_wave_factor': 1.0,   # 0.5/0.5
            }
        ]
        
        for test in test_cases:
            print(f"\nTest Case: {test['name']}")
            
            fc = fatigue_df.iloc[test['fc_idx']]
            print(f"  Wind Speed: {fc['Wind Speed (m/s)']} m/s")
            print(f"  Hs: {fc['Hs (m)']} m")
            
            # Calculate scaling factors
            # Assuming wind01 (10 m/s) and wave01 (0.5 m) as references
            wind_ref = ref_df[ref_df['env reference'] == 'wind01'].iloc[0] if 'wind01' in ref_df['env reference'].values else None
            wave_ref = ref_df[ref_df['env reference'] == 'wave01'].iloc[0] if 'wave01' in ref_df['env reference'].values else None
            
            if wind_ref is not None:
                v_ref = float(wind_ref['Vw [m/s]']) if pd.notna(wind_ref['Vw [m/s]']) else 10.0
                wind_factor = (fc['Wind Speed (m/s)'] / v_ref) ** 2
                
                if abs(wind_factor - test['expected_wind_factor']) < 0.01:
                    self.log_success(f"Wind factor correct: {wind_factor:.2f}")
                else:
                    self.log_error(f"Wind factor mismatch: {wind_factor:.2f} vs {test['expected_wind_factor']}")
                    
            if wave_ref is not None:
                hs_ref = float(wave_ref['Hs [m]']) if pd.notna(wave_ref['Hs [m]']) else 0.5
                wave_factor = fc['Hs (m)'] / hs_ref
                
                if abs(wave_factor - test['expected_wave_factor']) < 0.01:
                    self.log_success(f"Wave factor correct: {wave_factor:.2f}")
                else:
                    self.log_error(f"Wave factor mismatch: {wave_factor:.2f} vs {test['expected_wave_factor']}")
                    
    def verify_output_files(self):
        """Verify output file structure and content"""
        print("\n" + "="*60)
        print("3. VERIFYING OUTPUT FILES")
        print("="*60)
        
        output_dir = Path(self.config['output']['base_folder'])
        
        if not output_dir.exists():
            self.log_error(f"Output directory not found: {output_dir}")
            return False
        else:
            self.log_success(f"Output directory exists: {output_dir}")
            
        # Check for summary report
        summary_file = output_dir / 'scaling_factors_applied.csv'
        if not summary_file.exists():
            self.log_error("Summary report not found")
            return False
        else:
            self.log_success("Summary report found")
            
        # Load and verify summary
        try:
            summary_df = pd.read_csv(summary_file)
            self.log_success(f"Loaded summary with {len(summary_df)} entries")
            
            # Check columns
            expected_cols = ['Configuration', 'Fatigue Condition', 'Strut', 
                           'Wind Factor', 'Wave Factor', 'Wind Reference', 'Wave Reference']
            
            for col in expected_cols:
                if col not in summary_df.columns:
                    self.log_error(f"Missing column in summary: {col}")
                else:
                    self.log_success(f"Found column: {col}")
                    
            # Check for all expected combinations
            configs = self.config['input_data']['vessel_configurations']['configs']
            struts = self.config['input_data']['vessel_configurations']['struts']
            fatigue_df = pd.read_csv(self.config['input_data']['fatigue_seastates']['metadata_file'])
            
            expected_count = len(configs) * len(fatigue_df) * len(struts)
            if len(summary_df) != expected_count:
                self.log_error(f"Entry count mismatch: {len(summary_df)} vs {expected_count} expected")
            else:
                self.log_success(f"Entry count correct: {expected_count}")
                
            # Verify scaling factor ranges
            wind_min = summary_df['Wind Factor'].min()
            wind_max = summary_df['Wind Factor'].max()
            wave_min = summary_df['Wave Factor'].min()
            wave_max = summary_df['Wave Factor'].max()
            
            print(f"\n  Scaling Factor Ranges:")
            print(f"    Wind: {wind_min:.3f} - {wind_max:.3f}")
            print(f"    Wave: {wave_min:.3f} - {wave_max:.3f}")
            
            # Check limits
            limits = self.config['load_scaling']['validation']
            if wind_min < limits['min_wind_scaling_factor']:
                self.log_error(f"Wind factor below minimum: {wind_min}")
            if wind_max > limits['max_wind_scaling_factor']:
                self.log_error(f"Wind factor above maximum: {wind_max}")
            if wave_min < limits['min_wave_scaling_factor']:
                self.log_error(f"Wave factor below minimum: {wave_min}")
            if wave_max > limits['max_wave_scaling_factor']:
                self.log_error(f"Wave factor above maximum: {wave_max}")
                
            if self.failed_tests == 0:
                self.log_success("All scaling factors within limits")
                
        except Exception as e:
            self.log_error(f"Failed to verify summary report: {e}")
            return False
            
        # Spot check individual files
        print("\n  Spot checking individual output files:")
        
        # Check first file from each configuration
        for config_data in configs[:2]:  # Check first 2 configs
            config = config_data['id']
            test_file = output_dir / f"{config}_FC001_Strut1_scaled_tension.csv"
            
            if not test_file.exists():
                self.log_error(f"Missing file: {test_file.name}")
            else:
                try:
                    df = pd.read_csv(test_file)
                    
                    # Check structure
                    required_cols = ['Time (s)', 'Scaled Tension (kN)', 'Wind Factor', 'Wave Factor']
                    missing = [c for c in required_cols if c not in df.columns]
                    
                    if missing:
                        self.log_error(f"Missing columns in {test_file.name}: {missing}")
                    else:
                        self.log_success(f"File structure valid: {test_file.name}")
                        
                    # Check data integrity
                    if df['Scaled Tension (kN)'].isna().any():
                        self.log_error(f"NaN values in {test_file.name}")
                    elif (df['Scaled Tension (kN)'] < 0).any():
                        self.log_error(f"Negative tensions in {test_file.name}")
                    else:
                        self.log_success(f"Data integrity valid: {test_file.name}")
                        
                except Exception as e:
                    self.log_error(f"Failed to read {test_file.name}: {e}")
                    
        return self.failed_tests == 0
        
    def verify_specific_calculation(self, config='fsts_l015', fc_num=1, strut=1):
        """Verify a specific calculation in detail"""
        print("\n" + "="*60)
        print(f"4. DETAILED VERIFICATION: {config}_FC{fc_num:03d}_Strut{strut}")
        print("="*60)
        
        # Load input data
        fatigue_df = pd.read_csv(self.config['input_data']['fatigue_seastates']['metadata_file'])
        fc = fatigue_df.iloc[fc_num - 1]
        
        print(f"\nFatigue Condition FC{fc_num:03d}:")
        print(f"  Wind: {fc['Wind Speed (m/s)']} m/s @ {fc['Wind Dir (°)']}°")
        print(f"  Wave: Hs={fc['Hs (m)']} m, Tp={fc['Tp (s)']} s @ {fc['Wave Dir (°)']}°")
        print(f"  Occurrence: {fc['Occurrence (%)']}%")
        
        # Load output file
        output_file = Path(self.config['output']['base_folder']) / f"{config}_FC{fc_num:03d}_Strut{strut}_scaled_tension.csv"
        
        if not output_file.exists():
            self.log_error(f"Output file not found: {output_file}")
            return False
            
        try:
            output_df = pd.read_csv(output_file)
            
            # Get scaling factors
            wind_factor = output_df['Wind Factor'].iloc[0]
            wave_factor = output_df['Wave Factor'].iloc[0]
            wind_ref = output_df['Wind Reference'].iloc[0]
            wave_ref = output_df['Wave Reference'].iloc[0]
            
            print(f"\nScaling Factors Applied:")
            print(f"  Wind Factor: {wind_factor:.3f} (Reference: {wind_ref})")
            print(f"  Wave Factor: {wave_factor:.3f} (Reference: {wave_ref})")
            
            # Load reference data to verify calculation
            ref_folder = Path(self.config['input_data']['reference_seastate']['data_folder'])
            wind_file = ref_folder / f"{config}_mwl_{wind_ref}_Strut{strut}.csv"
            wave_file = ref_folder / f"{config}_mwl_{wave_ref}_Strut{strut}.csv"
            
            if wind_file.exists() and wave_file.exists():
                wind_df = pd.read_csv(wind_file)
                wave_df = pd.read_csv(wave_file)
                
                # Get tension column name
                tension_col = None
                for col in wind_df.columns:
                    if 'tension' in col.lower():
                        tension_col = col
                        break
                        
                if tension_col:
                    # Calculate expected scaled tension for first point
                    t_wind = wind_df[tension_col].iloc[0]
                    t_wave = wave_df[tension_col].iloc[0]
                    expected = t_wind * wind_factor + t_wave * wave_factor
                    actual = output_df['Scaled Tension (kN)'].iloc[0]
                    
                    print(f"\nFirst Point Verification:")
                    print(f"  Wind Tension: {t_wind:.2f} kN × {wind_factor:.3f} = {t_wind * wind_factor:.2f} kN")
                    print(f"  Wave Tension: {t_wave:.2f} kN × {wave_factor:.3f} = {t_wave * wave_factor:.2f} kN")
                    print(f"  Expected Total: {expected:.2f} kN")
                    print(f"  Actual Total: {actual:.2f} kN")
                    
                    diff = abs(expected - actual)
                    if diff < 0.1:
                        self.log_success(f"Calculation verified (difference: {diff:.4f} kN)")
                    else:
                        self.log_error(f"Calculation mismatch (difference: {diff:.2f} kN)")
                        
            # Statistics
            print(f"\nOutput Statistics:")
            print(f"  Points: {len(output_df)}")
            print(f"  Mean Tension: {output_df['Scaled Tension (kN)'].mean():.2f} kN")
            print(f"  Max Tension: {output_df['Scaled Tension (kN)'].max():.2f} kN")
            print(f"  Min Tension: {output_df['Scaled Tension (kN)'].min():.2f} kN")
            print(f"  Std Dev: {output_df['Scaled Tension (kN)'].std():.2f} kN")
            
            self.log_success("Detailed verification complete")
            
        except Exception as e:
            self.log_error(f"Failed to perform detailed verification: {e}")
            return False
            
        return True
        
    def generate_report(self):
        """Generate verification report"""
        print("\n" + "="*60)
        print("VERIFICATION REPORT")
        print("="*60)
        
        total_tests = self.passed_tests + self.failed_tests
        
        print(f"\nTest Results:")
        print(f"  Total Tests: {total_tests}")
        print(f"  Passed: {self.passed_tests} ({100*self.passed_tests/total_tests:.1f}%)")
        print(f"  Failed: {self.failed_tests} ({100*self.failed_tests/total_tests:.1f}%)")
        
        if self.warnings:
            print(f"\nWarnings ({len(self.warnings)}):")
            for warning in self.warnings[:5]:  # Show first 5
                print(f"  - {warning}")
                
        if self.errors:
            print(f"\nErrors ({len(self.errors)}):")
            for error in self.errors[:10]:  # Show first 10
                print(f"  - {error}")
                
        # Save report to file
        report = {
            'timestamp': datetime.now().isoformat(),
            'config_file': self.config_file,
            'total_tests': total_tests,
            'passed': self.passed_tests,
            'failed': self.failed_tests,
            'warnings': self.warnings,
            'errors': self.errors
        }
        
        report_file = Path('verification_report.json')
        with open(report_file, 'w') as f:
            json.dump(report, f, indent=2)
            
        print(f"\nReport saved to: {report_file}")
        
        if self.failed_tests == 0:
            print("\n[PASSED] ALL VERIFICATIONS PASSED!")
            return True
        else:
            print(f"\n[FAILED] VERIFICATION FAILED WITH {self.failed_tests} ERRORS")
            return False
            
    def run_full_verification(self):
        """Run complete verification suite"""
        print("="*60)
        print("LOAD SCALING VERIFICATION SUITE")
        print("="*60)
        print(f"Config: {self.config_file}")
        print(f"Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        
        # Run all verifications
        self.verify_input_files()
        self.verify_scaling_calculations()
        self.verify_output_files()
        self.verify_specific_calculation()
        
        # Generate report
        return self.generate_report()


def main():
    """Main entry point for verification script"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Verify Load Scaling Calculations')
    parser.add_argument('--config', default='input/load_scaling_config.yml',
                       help='Configuration file path')
    parser.add_argument('--detailed', action='store_true',
                       help='Run detailed verification')
    parser.add_argument('--case', type=str,
                       help='Specific case to verify (e.g., fsts_l015_FC001_Strut1)')
    
    args = parser.parse_args()
    
    # Change to script directory
    os.chdir(Path(__file__).parent)
    
    # Create verifier
    verifier = LoadScalingVerifier(args.config)
    
    if args.case:
        # Parse case specification
        parts = args.case.split('_')
        if len(parts) >= 4:
            config = '_'.join(parts[:-2])
            fc_num = int(parts[-2][2:])
            strut = int(parts[-1][5:])
            verifier.verify_specific_calculation(config, fc_num, strut)
            verifier.generate_report()
        else:
            print(f"Invalid case format: {args.case}")
            print("Expected format: config_FC###_Strut#")
            return 1
    else:
        # Run full verification
        success = verifier.run_full_verification()
        return 0 if success else 1


if __name__ == "__main__":
    sys.exit(main())