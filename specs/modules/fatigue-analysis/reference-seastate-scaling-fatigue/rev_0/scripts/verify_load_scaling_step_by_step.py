#!/usr/bin/env python
"""
Step-by-Step Validation for Load Scaling Module
================================================

This script validates the load scaling calculations step-by-step with user confirmation.
Each validation step shows calculations, formulas, and results for verification.

Following the existing pattern in the specs/scripts directory.
"""

import sys
import os
import pandas as pd
import numpy as np
from pathlib import Path
from typing import Tuple, Dict
import json
from datetime import datetime

# Setup paths following existing pattern
SCRIPT_DIR = Path(__file__).parent
SPEC_DIR = SCRIPT_DIR.parent
SAMPLE_DATA_DIR = SPEC_DIR / "sample_data"
INPUT_DIR = SPEC_DIR / "input"
OUTPUT_DIR = SPEC_DIR / "output"
DOCS_DIR = SPEC_DIR / "docs"

# Add parent directory to path for imports if needed
sys.path.insert(0, str(SPEC_DIR))

def print_section_header(title: str):
    """Print section header following existing pattern"""
    print(f"\n{'='*80}")
    print(f"{title}")
    print(f"{'='*80}")

def print_subsection(title: str):
    """Print subsection header"""
    print(f"\n{'-'*60}")
    print(f"{title}")
    print(f"{'-'*60}")

def wait_for_user_confirmation(prompt: str = "Please review the above results. Are they correct?") -> bool:
    """Wait for user confirmation following existing pattern"""
    print(f"\n{prompt}")
    response = input("Enter 'yes' or 'y' to continue, any other key to stop: ").lower().strip()
    return response in ['yes', 'y']

class LoadScalingValidator:
    """Step-by-step validator for load scaling module"""
    
    def __init__(self):
        """Initialize validator with spec directory paths"""
        self.sample_data_dir = SAMPLE_DATA_DIR
        self.input_dir = INPUT_DIR
        self.output_dir = OUTPUT_DIR
        self.docs_dir = DOCS_DIR
        
        # Reference values for scaling
        self.base_wind_speed = 10.0  # m/s
        self.base_hs = 0.5  # m
        
        # Create output subdirectory for validation results
        self.validation_output_dir = self.output_dir / "validation"
        self.validation_output_dir.mkdir(parents=True, exist_ok=True)
        
        # Validation log
        self.validation_steps = []
        
        print(f"Working in: {SPEC_DIR}")
        print(f"Sample data directory: {self.sample_data_dir}")
        print(f"Validation output directory: {self.validation_output_dir}")
    
    def run_validation(self):
        """Run complete step-by-step validation with user confirmation"""
        print_section_header("LOAD SCALING MODULE - STEP-BY-STEP VALIDATION")
        print(f"Timestamp: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print(f"Base Wind Speed: {self.base_wind_speed} m/s")
        print(f"Base Wave Height (Hs): {self.base_hs} m")
        
        # Step 1: Validate Wind Scaling Formula
        print_section_header("STEP 1: VALIDATE WIND SCALING FORMULA")
        if not self.validate_wind_scaling():
            print("\n✗ Validation stopped at Step 1")
            return False
        
        # Step 2: Validate Wave Scaling Formula
        print_section_header("STEP 2: VALIDATE WAVE SCALING FORMULA")
        if not self.validate_wave_scaling():
            print("\n✗ Validation stopped at Step 2")
            return False
        
        # Step 3: Validate Load Combination
        print_section_header("STEP 3: VALIDATE LOAD COMBINATION")
        if not self.validate_load_combination():
            print("\n✗ Validation stopped at Step 3")
            return False
        
        # Step 4: Validate with Sample Data
        print_section_header("STEP 4: VALIDATE WITH SAMPLE DATA")
        if not self.validate_with_sample_data():
            print("\n✗ Validation stopped at Step 4")
            return False
        
        # Step 5: Validate Scaling Factors Calculation
        print_section_header("STEP 5: VALIDATE SCALING FACTORS CALCULATION")
        if not self.validate_scaling_factors():
            print("\n✗ Validation stopped at Step 5")
            return False
        
        # Step 6: Validate Complete Processing
        print_section_header("STEP 6: VALIDATE COMPLETE PROCESSING")
        if not self.validate_complete_processing():
            print("\n✗ Validation stopped at Step 6")
            return False
        
        # Generate validation summary
        self.generate_validation_summary()
        
        print_section_header("VALIDATION COMPLETE")
        print("✓ All validation steps passed with user confirmation")
        return True
    
    def validate_wind_scaling(self) -> bool:
        """Step 1: Validate wind scaling formula"""
        print("\nWind Scaling Formula: F_scaled = F_ref × (V_target / V_ref)²")
        print("Physical basis: Wind force is proportional to velocity squared (F ∝ V²)")
        
        print_subsection("Test Cases")
        
        # Test cases for wind scaling
        test_cases = [
            (5.0, "(5/10)² = 0.25"),
            (10.0, "(10/10)² = 1.00"),
            (15.0, "(15/10)² = 2.25"),
            (20.0, "(20/10)² = 4.00"),
            (25.0, "(25/10)² = 6.25"),
        ]
        
        print(f"{'Wind Speed (m/s)':<20} {'Formula':<25} {'Scale Factor':<15} {'Check'}")
        print("-" * 75)
        
        all_correct = True
        for wind_speed, formula in test_cases:
            calculated = (wind_speed / self.base_wind_speed) ** 2
            expected = eval(formula.split("=")[1])
            is_correct = abs(calculated - expected) < 0.001
            
            status = "✓" if is_correct else "✗"
            print(f"{wind_speed:<20.1f} {formula:<25} {calculated:<15.4f} {status}")
            
            if not is_correct:
                all_correct = False
        
        # Example calculation
        print_subsection("Example Calculation")
        ref_load = 1000.0
        target_wind = 15.0
        scale = (target_wind / self.base_wind_speed) ** 2
        scaled_load = ref_load * scale
        
        print(f"Reference load at 10 m/s: {ref_load:.1f} kN")
        print(f"Target wind speed: {target_wind:.1f} m/s")
        print(f"Scaling factor: ({target_wind}/{self.base_wind_speed})² = {scale:.3f}")
        print(f"Scaled load: {ref_load:.1f} × {scale:.3f} = {scaled_load:.1f} kN")
        
        if not wait_for_user_confirmation("Is the wind scaling formula validation correct?"):
            return False
        
        self.validation_steps.append({
            'step': 1,
            'name': 'Wind Scaling Formula',
            'status': 'PASSED',
            'formula': 'F = F_ref × (V/10)²'
        })
        print("✓ Step 1 completed and confirmed")
        return True
    
    def validate_wave_scaling(self) -> bool:
        """Step 2: Validate wave scaling formula"""
        print("\nWave Scaling Formula: F_scaled = F_ref × (Hs_target / Hs_ref)")
        print("Physical basis: Wave force is proportional to wave height (F ∝ H)")
        
        print_subsection("Test Cases")
        
        # Test cases for wave scaling
        test_cases = [
            (0.25, "0.25/0.5 = 0.50"),
            (0.50, "0.50/0.5 = 1.00"),
            (0.75, "0.75/0.5 = 1.50"),
            (1.00, "1.00/0.5 = 2.00"),
            (1.50, "1.50/0.5 = 3.00"),
        ]
        
        print(f"{'Hs (m)':<20} {'Formula':<25} {'Scale Factor':<15} {'Check'}")
        print("-" * 75)
        
        for hs, formula in test_cases:
            calculated = hs / self.base_hs
            expected = eval(formula.split("=")[1])
            is_correct = abs(calculated - expected) < 0.001
            
            status = "✓" if is_correct else "✗"
            print(f"{hs:<20.2f} {formula:<25} {calculated:<15.4f} {status}")
        
        # Example calculation
        print_subsection("Example Calculation")
        ref_load = 500.0
        target_hs = 0.75
        scale = target_hs / self.base_hs
        scaled_load = ref_load * scale
        
        print(f"Reference load at Hs=0.5m: {ref_load:.1f} kN")
        print(f"Target wave height: {target_hs:.2f} m")
        print(f"Scaling factor: {target_hs}/{self.base_hs} = {scale:.3f}")
        print(f"Scaled load: {ref_load:.1f} × {scale:.3f} = {scaled_load:.1f} kN")
        
        if not wait_for_user_confirmation("Is the wave scaling formula validation correct?"):
            return False
        
        self.validation_steps.append({
            'step': 2,
            'name': 'Wave Scaling Formula',
            'status': 'PASSED',
            'formula': 'F = F_ref × (Hs/0.5)'
        })
        print("✓ Step 2 completed and confirmed")
        return True
    
    def validate_load_combination(self) -> bool:
        """Step 3: Validate load combination (superposition)"""
        print("\nLoad Combination Formula: F_total = F_wind_scaled + F_wave_scaled")
        print("Principle: Linear superposition for independent loads")
        
        print_subsection("Test Scenarios")
        
        # Test scenarios
        scenarios = [
            ("Calm", 100, 5, 50, 0.25),
            ("Moderate", 100, 10, 50, 0.5),
            ("Severe", 100, 20, 50, 1.0),
        ]
        
        for name, wind_ref, wind_speed, wave_ref, hs in scenarios:
            print(f"\n{name} Conditions:")
            wind_scale = (wind_speed / self.base_wind_speed) ** 2
            wave_scale = hs / self.base_hs
            
            wind_scaled = wind_ref * wind_scale
            wave_scaled = wave_ref * wave_scale
            combined = wind_scaled + wave_scaled
            
            print(f"  Wind: {wind_ref} kN × {wind_scale:.2f} = {wind_scaled:.1f} kN")
            print(f"  Wave: {wave_ref} kN × {wave_scale:.2f} = {wave_scaled:.1f} kN")
            print(f"  Combined: {wind_scaled:.1f} + {wave_scaled:.1f} = {combined:.1f} kN")
        
        if not wait_for_user_confirmation("Is the load combination validation correct?"):
            return False
        
        self.validation_steps.append({
            'step': 3,
            'name': 'Load Combination',
            'status': 'PASSED',
            'formula': 'F_total = F_wind + F_wave'
        })
        print("✓ Step 3 completed and confirmed")
        return True
    
    def validate_with_sample_data(self) -> bool:
        """Step 4: Validate with actual sample data"""
        print("\nValidating with sample data from:")
        print(f"  {self.sample_data_dir}")
        
        # List some sample files
        sample_files = list(self.sample_data_dir.glob("*.csv"))[:3]
        print("\nSample files found:")
        for f in sample_files:
            print(f"  - {f.name}")
        
        # Load sample data
        wind_file = self.sample_data_dir / "fsts_l015_mwl_wind01_Strut1.csv"
        wave_file = self.sample_data_dir / "fsts_l015_mwl_wave01_Strut1.csv"
        
        if wind_file.exists():
            print(f"\nLoading wind data: {wind_file.name}")
            wind_df = pd.read_csv(wind_file)
            wind_data = wind_df.iloc[:100, 1].values  # First 100 points
        else:
            print("\nUsing synthetic wind data")
            wind_data = 600 + 50 * np.random.randn(100)
        
        if wave_file.exists():
            print(f"Loading wave data: {wave_file.name}")
            wave_df = pd.read_csv(wave_file)
            wave_data = wave_df.iloc[:100, 1].values
        else:
            print("Using synthetic wave data")
            wave_data = 400 + 30 * np.random.randn(100)
        
        # Apply scaling
        print_subsection("Scaling Application")
        target_wind = 15.0
        target_hs = 0.75
        
        wind_scale = (target_wind / self.base_wind_speed) ** 2
        wave_scale = target_hs / self.base_hs
        
        print(f"Target wind: {target_wind} m/s (scale: {wind_scale:.3f})")
        print(f"Target wave: {target_hs} m (scale: {wave_scale:.3f})")
        
        wind_scaled = wind_data * wind_scale
        wave_scaled = wave_data * wave_scale
        combined = wind_scaled + wave_scaled
        
        # Show statistics
        print_subsection("Statistics")
        print(f"{'Component':<15} {'Original Mean':<15} {'Scaled Mean':<15} {'Ratio':<10}")
        print("-" * 55)
        
        wind_ratio = np.mean(wind_scaled) / np.mean(wind_data)
        wave_ratio = np.mean(wave_scaled) / np.mean(wave_data)
        
        print(f"{'Wind':<15} {np.mean(wind_data):<15.1f} {np.mean(wind_scaled):<15.1f} {wind_ratio:<10.3f}")
        print(f"{'Wave':<15} {np.mean(wave_data):<15.1f} {np.mean(wave_scaled):<15.1f} {wave_ratio:<10.3f}")
        print(f"{'Combined':<15} {'-':<15} {np.mean(combined):<15.1f} {'-':<10}")
        
        # Save sample output
        output_file = self.validation_output_dir / "sample_scaling_validation.csv"
        pd.DataFrame({
            'wind_original': wind_data[:10],
            'wind_scaled': wind_scaled[:10],
            'wave_original': wave_data[:10],
            'wave_scaled': wave_scaled[:10],
            'combined': combined[:10]
        }).to_csv(output_file, index=False)
        print(f"\nSample output saved to: output/validation/{output_file.name}")
        
        if not wait_for_user_confirmation("Is the sample data scaling correct?"):
            return False
        
        self.validation_steps.append({
            'step': 4,
            'name': 'Sample Data Validation',
            'status': 'PASSED',
            'data_points': len(wind_data)
        })
        print("✓ Step 4 completed and confirmed")
        return True
    
    def validate_scaling_factors(self) -> bool:
        """Step 5: Validate scaling factors calculation"""
        print("\nValidating scaling factors calculation")
        
        # Load or create fatigue conditions
        fatigue_file = self.input_dir / "fatigue_seastates_sample.csv"
        
        if fatigue_file.exists():
            print(f"Loading fatigue conditions from: {fatigue_file.name}")
            fatigue_df = pd.read_csv(fatigue_file)
        else:
            print("Creating sample fatigue conditions")
            fatigue_df = pd.DataFrame([
                {'Row': 1, 'Wind Speed (m/s)': 5, 'Hs (m)': 0.15, 'Occurrence (%)': 20},
                {'Row': 2, 'Wind Speed (m/s)': 10, 'Hs (m)': 0.25, 'Occurrence (%)': 30},
                {'Row': 3, 'Wind Speed (m/s)': 15, 'Hs (m)': 0.5, 'Occurrence (%)': 30},
                {'Row': 4, 'Wind Speed (m/s)': 20, 'Hs (m)': 0.75, 'Occurrence (%)': 20},
            ])
        
        print_subsection("Scaling Factors")
        print(f"{'ID':<5} {'Wind (m/s)':<12} {'Hs (m)':<10} {'Wind Scale':<12} {'Wave Scale':<12} {'Occ %':<10}")
        print("-" * 71)
        
        total_occurrence = 0
        scaling_factors = []
        
        for _, row in fatigue_df.iterrows():
            wind_speed = row['Wind Speed (m/s)']
            hs = row['Hs (m)']
            occurrence = row['Occurrence (%)']
            
            wind_scale = (wind_speed / self.base_wind_speed) ** 2
            wave_scale = hs / self.base_hs
            total_occurrence += occurrence
            
            scaling_factors.append({
                'id': row['Row'],
                'wind_scale': wind_scale,
                'wave_scale': wave_scale,
                'occurrence': occurrence
            })
            
            print(f"{row['Row']:<5} {wind_speed:<12.1f} {hs:<10.2f} "
                  f"{wind_scale:<12.4f} {wave_scale:<12.4f} {occurrence:<10.1f}")
        
        print("-" * 71)
        print(f"{'Total occurrence:':<49} {total_occurrence:.1f}%")
        
        # Save scaling factors
        scaling_file = self.validation_output_dir / "scaling_factors_validation.csv"
        pd.DataFrame(scaling_factors).to_csv(scaling_file, index=False)
        print(f"\nScaling factors saved to: output/validation/{scaling_file.name}")
        
        if abs(total_occurrence - 100.0) > 0.1:
            print(f"\n⚠ Warning: Total occurrence is {total_occurrence:.1f}%, should be 100%")
        
        if not wait_for_user_confirmation("Are the scaling factors correct?"):
            return False
        
        self.validation_steps.append({
            'step': 5,
            'name': 'Scaling Factors',
            'status': 'PASSED',
            'conditions': len(fatigue_df)
        })
        print("✓ Step 5 completed and confirmed")
        return True
    
    def validate_complete_processing(self) -> bool:
        """Step 6: Validate complete processing chain"""
        print("\nValidating complete processing chain")
        
        print_subsection("Processing Chain Steps")
        print("1. Load reference time series from sample_data/")
        print("2. Calculate scaling factors from fatigue conditions")
        print("3. Apply scaling to time series")
        print("4. Combine wind and wave loads")
        print("5. Save output to output/{config}/")
        
        # Simulate processing
        print_subsection("Example Processing")
        config = "fsts_l015"
        condition = "FC001"
        strut = 1
        
        print(f"\nConfiguration: {config}")
        print(f"Fatigue Condition: {condition}")
        print(f"Strut: {strut}")
        print(f"Wind scaling: 2.25 (15 m/s)")
        print(f"Wave scaling: 1.50 (0.75 m)")
        
        # Expected output structure
        print_subsection("Output Structure")
        print("output/")
        print("├── fsts_l015/")
        print("│   ├── FC001_Strut1_combined.csv")
        print("│   ├── FC001_Strut2_combined.csv")
        print("│   └── ...")
        print("├── fsts_l095/")
        print("└── validation/")
        print("    ├── sample_scaling_validation.csv")
        print("    └── scaling_factors_validation.csv")
        
        if not wait_for_user_confirmation("Is the complete processing chain correct?"):
            return False
        
        self.validation_steps.append({
            'step': 6,
            'name': 'Complete Processing',
            'status': 'PASSED'
        })
        print("✓ Step 6 completed and confirmed")
        return True
    
    def generate_validation_summary(self):
        """Generate and save validation summary"""
        print_section_header("VALIDATION SUMMARY")
        
        print("\nValidation Steps Completed:")
        print("-" * 60)
        for step in self.validation_steps:
            print(f"Step {step['step']}: {step['name']:<30} {step['status']}")
        
        # Save summary
        summary_file = self.validation_output_dir / "validation_summary.json"
        with open(summary_file, 'w') as f:
            json.dump({
                'timestamp': datetime.now().isoformat(),
                'module': 'Load Scaling',
                'steps': self.validation_steps,
                'formulas': {
                    'wind': 'F = F_ref × (V/10)²',
                    'wave': 'F = F_ref × (Hs/0.5)',
                    'combined': 'F_total = F_wind + F_wave'
                }
            }, f, indent=2)
        
        print(f"\nValidation summary saved to: output/validation/{summary_file.name}")
        
        # Create documentation
        doc_file = self.docs_dir / "validation" / "load_scaling_validation_record.md"
        doc_file.parent.mkdir(parents=True, exist_ok=True)
        
        with open(doc_file, 'w') as f:
            f.write("# Load Scaling Validation Record\n\n")
            f.write(f"Date: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
            f.write("## Validated Formulas\n\n")
            f.write("- Wind Scaling: F = F_ref × (V/10)²\n")
            f.write("- Wave Scaling: F = F_ref × (Hs/0.5)\n")
            f.write("- Combined: F_total = F_wind + F_wave\n\n")
            f.write("## Validation Steps\n\n")
            for step in self.validation_steps:
                f.write(f"- [x] Step {step['step']}: {step['name']}\n")
        
        print(f"Validation record saved to: docs/validation/{doc_file.name}")


def main():
    """Main execution function"""
    print_section_header("LOAD SCALING MODULE - STEP-BY-STEP VALIDATION")
    print("\nThis script will validate the load scaling module step by step.")
    print("You will be asked to confirm each step before proceeding.")
    print("\nWorking directory:", SPEC_DIR)
    
    if not wait_for_user_confirmation("\nReady to begin validation?"):
        print("Validation cancelled.")
        return 1
    
    validator = LoadScalingValidator()
    
    if validator.run_validation():
        print("\n✓ VALIDATION SUCCESSFUL")
        print("All steps have been validated and confirmed.")
        return 0
    else:
        print("\n✗ VALIDATION INCOMPLETE")
        print("Please review the failed steps.")
        return 1


if __name__ == "__main__":
    sys.exit(main())