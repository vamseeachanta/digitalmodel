#!/usr/bin/env python3
"""
Interactive Sample Data Process Verification

This script walks through the fatigue analysis process step by step
with user confirmation at each stage.
"""

import sys
import os
import pandas as pd
import numpy as np
from pathlib import Path
import json
from typing import Dict, List, Tuple
import logging

# Setup path
sys.path.insert(0, 'D:/github/digitalmodel/src')
from digitalmodel.fatigue_analysis.integrated_processor import IntegratedFatigueProcessor
from digitalmodel.fatigue_analysis.strut_foundation_processor import (
    ProductionDataHandler, LoadScaler, FatigueCondition
)
from digitalmodel.fatigue_analysis.rainflow_counter import RainflowCounter
from digitalmodel.fatigue_analysis.fatigue_damage_calculator import (
    FatigueDamageCalculator, SNCurveParameters
)

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(message)s')
logger = logging.getLogger(__name__)


class InteractiveVerifier:
    """Interactive step-by-step verification of fatigue analysis process"""
    
    def __init__(self):
        self.step_num = 0
        self.results = {}
        
    def wait_for_confirmation(self, message: str = "Continue?") -> bool:
        """Wait for user confirmation"""
        response = input(f"\n🔸 {message} (y/n/q): ").lower().strip()
        if response == 'q':
            print("Exiting verification...")
            sys.exit(0)
        return response == 'y'
    
    def show_step(self, title: str):
        """Display step header"""
        self.step_num += 1
        print("\n" + "="*80)
        print(f"STEP {self.step_num}: {title}")
        print("="*80)
    
    def show_data(self, data: any, description: str = "Data"):
        """Display data with formatting"""
        print(f"\n📊 {description}:")
        print("-" * 40)
        if isinstance(data, pd.DataFrame):
            print(data.head())
            print(f"... (showing first 5 of {len(data)} rows)")
        elif isinstance(data, (list, tuple, np.ndarray)):
            if len(data) > 10:
                print(f"{data[:10]}")
                print(f"... (showing first 10 of {len(data)} items)")
            else:
                print(data)
        elif isinstance(data, dict):
            for key, value in list(data.items())[:5]:
                print(f"  {key}: {value}")
            if len(data) > 5:
                print(f"  ... and {len(data)-5} more items")
        else:
            print(f"  {data}")
    
    def verify_step_1_data_structure(self):
        """Step 1: Verify sample data directory structure"""
        self.show_step("VERIFY DATA STRUCTURE")
        
        base_path = Path("sample_data")
        print(f"📁 Base path: {base_path.absolute()}")
        
        # Expected structure
        expected_configs = [
            'fsts_l015', 
            'fsts_l095', 
            'fsts_l015_125km3_l100_pb', 
            'fsts_l095_125km3_l000_pb'
        ]
        
        print("\n✅ Expected configurations:")
        for config in expected_configs:
            config_path = base_path / config
            exists = "✓" if config_path.exists() else "✗"
            print(f"  [{exists}] {config}")
            
            if config_path.exists():
                # Check reference directories
                refs = list(config_path.iterdir())
                print(f"      References: {[r.name for r in refs]}")
        
        if not self.wait_for_confirmation("Data structure looks correct?"):
            print("❌ Please fix the data structure before continuing")
            return False
        
        self.results['structure_verified'] = True
        return True
    
    def verify_step_2_load_sample_file(self):
        """Step 2: Load and verify a sample CSV file"""
        self.show_step("LOAD SAMPLE FILE")
        
        # Load a specific file
        sample_file = Path("sample_data/fsts_l015/wind_000deg/Strut1.csv")
        print(f"📄 Loading: {sample_file}")
        
        if not sample_file.exists():
            print(f"❌ File not found: {sample_file}")
            return False
        
        df = pd.read_csv(sample_file)
        self.show_data(df, "Sample data from Strut1")
        
        # Check columns
        print(f"\n📋 Columns: {df.columns.tolist()}")
        print(f"📏 Data points: {len(df)}")
        print(f"⏱️ Time range: {df.iloc[0, 0]:.1f} to {df.iloc[-1, 0]:.1f} seconds")
        
        # Check for vessel end column
        vessel_cols = [c for c in df.columns if 'Vessel' in c and 'End' in c]
        if vessel_cols:
            print(f"✅ Found vessel end column: {vessel_cols[0]}")
            tension_data = df[vessel_cols[0]].values
            print(f"📊 Tension statistics:")
            print(f"   Min: {np.min(tension_data):.2f} kN")
            print(f"   Max: {np.max(tension_data):.2f} kN")
            print(f"   Mean: {np.mean(tension_data):.2f} kN")
            print(f"   Std: {np.std(tension_data):.2f} kN")
        else:
            print("❌ No vessel end tension column found!")
            return False
        
        if not self.wait_for_confirmation("Sample file loaded correctly?"):
            return False
        
        self.results['sample_file'] = df
        return True
    
    def verify_step_3_load_conditions(self):
        """Step 3: Load and verify fatigue conditions"""
        self.show_step("LOAD FATIGUE CONDITIONS")
        
        conditions_file = Path("input/fatigue_conditions.csv")
        print(f"📄 Loading: {conditions_file}")
        
        if not conditions_file.exists():
            print(f"❌ File not found: {conditions_file}")
            return False
        
        df = pd.read_csv(conditions_file)
        self.show_data(df, "Fatigue conditions")
        
        print(f"\n📊 Statistics:")
        print(f"   Total conditions: {len(df)}")
        print(f"   Wind speed range: {df['Wind Speed (m/s)'].min():.1f} - {df['Wind Speed (m/s)'].max():.1f} m/s")
        print(f"   Hs range: {df['Hs (m)'].min():.2f} - {df['Hs (m)'].max():.2f} m")
        print(f"   Total occurrence: {df['Occurrence (%)'].sum():.1f}%")
        
        if not self.wait_for_confirmation("Fatigue conditions correct?"):
            return False
        
        self.results['conditions'] = df
        return True
    
    def verify_step_4_scaling_calculation(self):
        """Step 4: Verify scaling calculations"""
        self.show_step("VERIFY SCALING CALCULATIONS")
        
        # Example: FC002 with Wind=10m/s, Hs=0.25m
        print("📐 Example: Fatigue Condition #2")
        print("   Wind: 10 m/s, Hs: 0.25 m")
        
        # Wind scaling
        base_wind = 10.0  # m/s
        actual_wind = 10.0  # m/s
        wind_scale = (actual_wind / base_wind) ** 2
        print(f"\n🌬️ Wind scaling:")
        print(f"   Formula: (V/10)² = ({actual_wind}/{base_wind})²")
        print(f"   Scale factor: {wind_scale:.2f}")
        
        # Wave scaling
        base_hs = 0.5  # m
        actual_hs = 0.25  # m
        wave_scale = actual_hs / base_hs
        print(f"\n🌊 Wave scaling:")
        print(f"   Formula: Hs/0.5 = {actual_hs}/{base_hs}")
        print(f"   Scale factor: {wave_scale:.2f}")
        
        # Apply to sample data
        if 'sample_file' in self.results:
            df = self.results['sample_file']
            vessel_col = [c for c in df.columns if 'Vessel' in c and 'End' in c][0]
            base_tension = df[vessel_col].values[:10]  # First 10 points
            
            scaled_wind = base_tension * wind_scale
            scaled_wave = base_tension * wave_scale  # Using same for demo
            combined = scaled_wind + scaled_wave
            
            print(f"\n📊 Sample scaling (first 5 points):")
            print(f"   Base tension: {base_tension[:5].round(1)}")
            print(f"   Wind scaled:  {scaled_wind[:5].round(1)}")
            print(f"   Wave scaled:  {scaled_wave[:5].round(1)}")
            print(f"   Combined:     {combined[:5].round(1)}")
        
        if not self.wait_for_confirmation("Scaling calculations correct?"):
            return False
        
        self.results['scaling_verified'] = True
        return True
    
    def verify_step_5_rainflow_counting(self):
        """Step 5: Verify rainflow counting"""
        self.show_step("RAINFLOW COUNTING")
        
        # Create sample data
        print("📈 Using sample sinusoidal data for demonstration:")
        time = np.linspace(0, 10, 100)
        tension = 500 + 100 * np.sin(2 * np.pi * 0.5 * time) + 20 * np.random.randn(100)
        
        print(f"   Data points: {len(tension)}")
        print(f"   Range: {tension.min():.1f} to {tension.max():.1f} kN")
        
        # Perform rainflow counting
        counter = RainflowCounter(gate_value=5.0)  # 5 kN gate
        ranges, counts = counter.count_cycles(tension)
        
        print(f"\n🔄 Rainflow results:")
        print(f"   Total cycles found: {len(ranges)}")
        print(f"   Total cycle count: {np.sum(counts):.1f}")
        print(f"   Max range: {np.max(ranges):.1f} kN")
        print(f"   Min range: {np.min(ranges):.1f} kN")
        
        # Show distribution
        if len(ranges) > 0:
            print(f"\n📊 Top 5 ranges:")
            sorted_idx = np.argsort(ranges)[::-1][:5]
            for idx in sorted_idx:
                print(f"   Range: {ranges[idx]:.1f} kN, Count: {counts[idx]:.1f}")
        
        if not self.wait_for_confirmation("Rainflow counting working?"):
            return False
        
        self.results['rainflow_ranges'] = ranges
        self.results['rainflow_counts'] = counts
        return True
    
    def verify_step_6_stress_conversion(self):
        """Step 6: Verify tension to stress conversion"""
        self.show_step("TENSION TO STRESS CONVERSION")
        
        # Load conversion table
        stress_file = Path("input/tension_range_to_stress_range_function.csv")
        print(f"📄 Loading: {stress_file}")
        
        if stress_file.exists():
            df = pd.read_csv(stress_file)
            self.show_data(df, "Conversion table")
        else:
            print("⚠️ Using default linear conversion (0.25 MPa/kN)")
        
        # Example conversion
        if 'rainflow_ranges' in self.results:
            tension_ranges = self.results['rainflow_ranges'][:5]
            stress_ranges = tension_ranges * 0.25  # Simple linear
            
            print(f"\n🔄 Example conversions:")
            for t, s in zip(tension_ranges, stress_ranges):
                print(f"   {t:.1f} kN → {s:.1f} MPa")
        
        if not self.wait_for_confirmation("Stress conversion correct?"):
            return False
        
        self.results['stress_conversion_verified'] = True
        return True
    
    def verify_step_7_damage_calculation(self):
        """Step 7: Verify damage calculation"""
        self.show_step("DAMAGE CALCULATION")
        
        # S-N curve parameters
        print("📈 S-N Curve: ABS E in Air")
        print("   Segment 1: log(a1)=12.018, m1=3.0 (N < 10^6)")
        print("   Segment 2: log(a2)=11.170, m2=5.0 (N ≥ 10^6)")
        
        # Example calculation
        stress = 100.0  # MPa
        
        # For segment 1
        N1 = 10**(12.018 - 3.0 * np.log10(stress))
        damage1 = 1.0 / N1
        
        print(f"\n📊 Example: Stress = {stress} MPa")
        print(f"   N (cycles to failure) = 10^(12.018 - 3.0*log({stress}))")
        print(f"   N = {N1:.0f} cycles")
        print(f"   Damage per cycle = 1/N = {damage1:.2e}")
        
        # Annual scaling
        seconds_per_year = 365.25 * 24 * 3600
        sample_seconds = 100  # 100 seconds of data
        scale_factor = seconds_per_year / sample_seconds
        
        print(f"\n📅 Annual scaling:")
        print(f"   Sample duration: {sample_seconds} seconds")
        print(f"   Annual duration: {seconds_per_year:.0f} seconds")
        print(f"   Scale factor: {scale_factor:.0f}")
        
        annual_damage = damage1 * scale_factor
        fatigue_life = 1 / annual_damage
        
        print(f"   Annual damage: {annual_damage:.2e}")
        print(f"   Fatigue life: {fatigue_life:.2f} years")
        
        if not self.wait_for_confirmation("Damage calculation correct?"):
            return False
        
        self.results['damage_verified'] = True
        return True
    
    def verify_step_8_integration(self):
        """Step 8: Verify complete integration"""
        self.show_step("COMPLETE INTEGRATION TEST")
        
        print("🔄 Running mini integration test...")
        print("   Configuration: fsts_l015")
        print("   Condition: FC001 (Wind=5m/s, Hs=0.15m)")
        print("   Strut: 1")
        print("   Sample size: 100 timesteps")
        
        try:
            # Initialize processor
            handler = ProductionDataHandler(
                base_path="sample_data",
                sample_timesteps=100
            )
            
            processor = IntegratedFatigueProcessor(handler)
            
            # Process one condition
            condition = FatigueCondition(
                id=1,
                wind_speed=5.0,
                wind_dir=0,
                hs=0.15,
                tp=2.0,
                wave_dir=0,
                occurrence=20.0
            )
            
            result = processor.process_single_condition(
                'fsts_l015', condition, strut_num=1
            )
            
            if result:
                print(f"\n✅ Integration test successful!")
                print(f"   Annual damage: {result['annual_damage']:.2e}")
                print(f"   Fatigue life: {result['fatigue_life_years']:.2f} years")
                print(f"   Max stress: {result['max_stress_range']:.1f} MPa")
            else:
                print("❌ Integration test failed - no result")
                return False
                
        except Exception as e:
            print(f"❌ Integration test error: {e}")
            return False
        
        if not self.wait_for_confirmation("Integration test passed?"):
            return False
        
        self.results['integration_verified'] = True
        return True
    
    def verify_step_9_output_files(self):
        """Step 9: Verify output file structure"""
        self.show_step("VERIFY OUTPUT FILES")
        
        output_path = Path("output")
        
        print(f"📁 Output directory: {output_path.absolute()}")
        
        if output_path.exists():
            # Check structure
            configs = ['fsts_l015', 'fsts_l095', 'fsts_l015_125km3_l100_pb', 'fsts_l095_125km3_l000_pb']
            
            for config in configs:
                config_path = output_path / config
                if config_path.exists():
                    print(f"\n✅ {config}/")
                    subdirs = ['combined_tensions', 'rainflow_results', 'damage_results', 'summaries']
                    for subdir in subdirs:
                        subdir_path = config_path / subdir
                        if subdir_path.exists():
                            files = list(subdir_path.glob('*.csv'))
                            print(f"   {subdir}: {len(files)} files")
                            if files and len(files) <= 3:
                                for f in files:
                                    print(f"     - {f.name}")
                else:
                    print(f"⚠️ {config}/ not found")
            
            # Check overall summary
            overall_path = output_path / 'overall'
            if overall_path.exists():
                print(f"\n✅ overall/")
                for f in overall_path.glob('*.csv'):
                    print(f"   - {f.name}")
        else:
            print("⚠️ No output directory found (run analysis first)")
        
        if not self.wait_for_confirmation("Output structure acceptable?"):
            return False
        
        self.results['output_verified'] = True
        return True
    
    def run_verification(self):
        """Run complete verification process"""
        print("\n" + "="*80)
        print("FATIGUE ANALYSIS SAMPLE DATA VERIFICATION")
        print("="*80)
        print("\nThis will verify the fatigue analysis process step by step.")
        print("You'll be asked to confirm at each step.")
        print("Press 'y' to continue, 'n' to skip, or 'q' to quit.\n")
        
        steps = [
            self.verify_step_1_data_structure,
            self.verify_step_2_load_sample_file,
            self.verify_step_3_load_conditions,
            self.verify_step_4_scaling_calculation,
            self.verify_step_5_rainflow_counting,
            self.verify_step_6_stress_conversion,
            self.verify_step_7_damage_calculation,
            self.verify_step_8_integration,
            self.verify_step_9_output_files
        ]
        
        for step_func in steps:
            if not step_func():
                print("\n❌ Verification stopped at step")
                return False
        
        print("\n" + "="*80)
        print("✅ VERIFICATION COMPLETE")
        print("="*80)
        print("\nAll steps verified successfully!")
        
        # Summary
        print("\n📋 Verification Summary:")
        for key in self.results:
            if key.endswith('_verified'):
                print(f"   ✅ {key.replace('_', ' ').title()}")
        
        return True


def main():
    """Main entry point"""
    # Change to spec directory
    os.chdir('D:/github/digitalmodel/specs/modules/fatigue-analysis/reference-seastate-scaling-fatigue')
    
    verifier = InteractiveVerifier()
    success = verifier.run_verification()
    
    if success:
        print("\n🎉 Sample data process fully verified!")
    else:
        print("\n⚠️ Verification incomplete - please address issues")
    
    return 0 if success else 1


if __name__ == "__main__":
    sys.exit(main())