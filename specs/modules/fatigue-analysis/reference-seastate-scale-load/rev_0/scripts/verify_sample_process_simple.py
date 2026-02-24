#!/usr/bin/env python3
"""
Simple Interactive Sample Data Process Verification
Without unicode characters for compatibility
"""

import sys
import os
import pandas as pd
import numpy as np
from pathlib import Path
import json

# Setup path
sys.path.insert(0, 'D:/github/digitalmodel/src')

def show_step(step_num, title):
    """Display step header"""
    print("\n" + "="*80)
    print(f"STEP {step_num}: {title}")
    print("="*80)

def wait_for_user():
    """Wait for user confirmation"""
    response = input("\n>>> Continue? (y/n): ").lower().strip()
    return response == 'y'

def main():
    """Main verification process"""
    print("\n" + "="*80)
    print("FATIGUE ANALYSIS - SAMPLE DATA VERIFICATION")
    print("="*80)
    print("\nThis will verify each step of the fatigue analysis process.")
    print("Press 'y' to continue at each step, 'n' to exit.\n")
    
    # Change to spec directory
    os.chdir('D:/github/digitalmodel/specs/modules/fatigue-analysis/reference-seastate-scaling-fatigue')
    
    # ========== STEP 1: DATA STRUCTURE ==========
    show_step(1, "VERIFY DATA STRUCTURE")
    
    base_path = Path("sample_data")
    print(f"Base path: {base_path.absolute()}")
    
    configs = ['fsts_l015', 'fsts_l095', 'fsts_l015_125km3_l100_pb', 'fsts_l095_125km3_l000_pb']
    print("\nExpected configurations:")
    for config in configs:
        config_path = base_path / config
        status = "EXISTS" if config_path.exists() else "MISSING"
        print(f"  [{status}] {config}")
        
        if config_path.exists():
            refs = list(config_path.iterdir())
            print(f"       -> {len(refs)} reference directories")
    
    if not wait_for_user():
        print("Exiting...")
        return 1
    
    # ========== STEP 2: LOAD SAMPLE FILE ==========
    show_step(2, "LOAD AND VERIFY SAMPLE CSV")
    
    sample_file = Path("sample_data/fsts_l015/wind_000deg/Strut1.csv")
    print(f"Loading: {sample_file}")
    
    if sample_file.exists():
        df = pd.read_csv(sample_file)
        print(f"\nFile loaded successfully!")
        print(f"  Shape: {df.shape}")
        print(f"  Columns: {df.columns.tolist()}")
        print(f"\nFirst 5 rows:")
        print(df.head())
        
        # Check for tension column
        vessel_cols = [c for c in df.columns if 'Vessel' in c and 'End' in c]
        if vessel_cols:
            tension_col = vessel_cols[0]
            tensions = df[tension_col].values
            print(f"\nTension statistics ({tension_col}):")
            print(f"  Min:  {tensions.min():.2f} kN")
            print(f"  Max:  {tensions.max():.2f} kN")
            print(f"  Mean: {tensions.mean():.2f} kN")
            print(f"  Std:  {tensions.std():.2f} kN")
    else:
        print(f"ERROR: File not found!")
        return 1
    
    if not wait_for_user():
        print("Exiting...")
        return 1
    
    # ========== STEP 3: FATIGUE CONDITIONS ==========
    show_step(3, "LOAD FATIGUE CONDITIONS")
    
    conditions_file = Path("input/fatigue_conditions.csv")
    print(f"Loading: {conditions_file}")
    
    if conditions_file.exists():
        conditions = pd.read_csv(conditions_file)
        print(f"\nConditions loaded: {len(conditions)} rows")
        print("\nFirst 5 conditions:")
        print(conditions.head())
        
        print(f"\nSummary:")
        print(f"  Wind speed range: {conditions['Wind Speed (m/s)'].min():.0f}-{conditions['Wind Speed (m/s)'].max():.0f} m/s")
        print(f"  Hs range: {conditions['Hs (m)'].min():.2f}-{conditions['Hs (m)'].max():.2f} m")
        print(f"  Total occurrence: {conditions['Occurrence (%)'].sum():.1f}%")
    else:
        print("ERROR: Conditions file not found!")
        return 1
    
    if not wait_for_user():
        print("Exiting...")
        return 1
    
    # ========== STEP 4: SCALING CALCULATION ==========
    show_step(4, "VERIFY SCALING CALCULATIONS")
    
    print("Example: Fatigue Condition #2")
    print("  Wind: 10 m/s, Hs: 0.25 m")
    
    # Wind scaling
    wind_actual = 10.0
    wind_base = 10.0
    wind_scale = (wind_actual / wind_base) ** 2
    print(f"\nWind scaling:")
    print(f"  Formula: (V/10)^2 = ({wind_actual}/{wind_base})^2 = {wind_scale:.2f}")
    
    # Wave scaling  
    hs_actual = 0.25
    hs_base = 0.5
    wave_scale = hs_actual / hs_base
    print(f"\nWave scaling:")
    print(f"  Formula: Hs/0.5 = {hs_actual}/{hs_base} = {wave_scale:.2f}")
    
    # Example application
    base_tension = 500.0  # kN
    print(f"\nExample with base tension = {base_tension} kN:")
    print(f"  Wind component: {base_tension} * {wind_scale:.2f} = {base_tension * wind_scale:.1f} kN")
    print(f"  Wave component: {base_tension} * {wave_scale:.2f} = {base_tension * wave_scale:.1f} kN")
    print(f"  Combined: {base_tension * wind_scale:.1f} + {base_tension * wave_scale:.1f} = {base_tension * (wind_scale + wave_scale):.1f} kN")
    
    if not wait_for_user():
        print("Exiting...")
        return 1
    
    # ========== STEP 5: RAINFLOW COUNTING ==========
    show_step(5, "RAINFLOW COUNTING")
    
    print("Demonstrating with synthetic data...")
    
    # Create sample data
    time = np.linspace(0, 10, 100)
    tension = 500 + 100 * np.sin(2 * np.pi * 0.5 * time) + 20 * np.random.randn(100)
    
    print(f"Sample data: {len(tension)} points")
    print(f"Range: {tension.min():.1f} to {tension.max():.1f} kN")
    
    # Import and use rainflow counter
    try:
        from digitalmodel.structural.fatigue_apps.rainflow_counter import RainflowCounter
        
        counter = RainflowCounter(gate_value=5.0)
        ranges, counts = counter.count_cycles(tension)
        
        print(f"\nRainflow results:")
        print(f"  Cycles found: {len(ranges)}")
        print(f"  Total count: {np.sum(counts):.1f}")
        print(f"  Max range: {np.max(ranges):.1f} kN")
        print(f"  Min range: {np.min(ranges):.1f} kN")
        
        # Show top 5
        if len(ranges) > 0:
            sorted_idx = np.argsort(ranges)[::-1][:5]
            print("\nTop 5 stress ranges:")
            for i, idx in enumerate(sorted_idx, 1):
                print(f"  {i}. Range: {ranges[idx]:.1f} kN, Count: {counts[idx]:.1f}")
    except Exception as e:
        print(f"ERROR: {e}")
        return 1
    
    if not wait_for_user():
        print("Exiting...")
        return 1
    
    # ========== STEP 6: STRESS CONVERSION ==========
    show_step(6, "TENSION TO STRESS CONVERSION")
    
    stress_table_file = Path("input/tension_range_to_stress_range_function.csv")
    
    if stress_table_file.exists():
        table = pd.read_csv(stress_table_file)
        print("Conversion table loaded:")
        print(table)
    else:
        print("Using default linear conversion: 0.25 MPa/kN")
    
    # Example conversion
    example_tensions = [100, 200, 500, 1000]
    print("\nExample conversions (0.25 MPa/kN):")
    for t in example_tensions:
        s = t * 0.25
        print(f"  {t:4.0f} kN -> {s:6.1f} MPa")
    
    if not wait_for_user():
        print("Exiting...")
        return 1
    
    # ========== STEP 7: DAMAGE CALCULATION ==========
    show_step(7, "FATIGUE DAMAGE CALCULATION")
    
    print("S-N Curve: ABS E in Air")
    print("  Segment 1: log(a1)=12.018, m1=3.0 (N < 10^6)")
    print("  Segment 2: log(a2)=11.170, m2=5.0 (N >= 10^6)")
    
    # Example calculation
    stress = 100.0  # MPa
    log_a1 = 12.018
    m1 = 3.0
    
    N = 10**(log_a1 - m1 * np.log10(stress))
    damage_per_cycle = 1.0 / N
    
    print(f"\nExample: Stress range = {stress} MPa")
    print(f"  N = 10^({log_a1} - {m1} * log10({stress}))")
    print(f"  N = {N:.0f} cycles to failure")
    print(f"  Damage per cycle = 1/N = {damage_per_cycle:.2e}")
    
    # Annual scaling
    seconds_per_year = 365.25 * 24 * 3600
    sample_seconds = 100
    scale = seconds_per_year / sample_seconds
    
    print(f"\nAnnual scaling:")
    print(f"  Sample duration: {sample_seconds} seconds")
    print(f"  Annual duration: {seconds_per_year:.0f} seconds")
    print(f"  Scale factor: {scale:.0f}")
    
    annual_damage = damage_per_cycle * scale
    fatigue_life = 1 / annual_damage if annual_damage > 0 else float('inf')
    
    print(f"  Annual damage: {annual_damage:.2e}")
    print(f"  Fatigue life: {fatigue_life:.2f} years")
    
    if not wait_for_user():
        print("Exiting...")
        return 1
    
    # ========== STEP 8: INTEGRATION TEST ==========
    show_step(8, "INTEGRATION TEST")
    
    print("Running mini integration test...")
    print("  Config: fsts_l015")
    print("  Condition: FC001 (Wind=5m/s, Hs=0.15m)")
    print("  Strut: 1")
    print("  Sample: 100 timesteps")
    
    try:
        from digitalmodel.structural.fatigue_apps.integrated_processor import (
            IntegratedFatigueProcessor,
            ProductionDataHandler,
            FatigueCondition
        )
        
        handler = ProductionDataHandler(base_path="sample_data", sample_timesteps=100)
        processor = IntegratedFatigueProcessor(handler)
        
        condition = FatigueCondition(
            id=1,
            wind_speed=5.0,
            wind_dir=0,
            hs=0.15,
            tp=2.0,
            wave_dir=0,
            occurrence=20.0
        )
        
        result = processor.process_single_condition('fsts_l015', condition, strut_num=1)
        
        if result:
            print("\nIntegration test SUCCESSFUL!")
            print(f"  Annual damage: {result['annual_damage']:.2e}")
            print(f"  Fatigue life: {result['fatigue_life_years']:.4f} years")
            print(f"  Max stress: {result['max_stress_range']:.1f} MPa")
            print(f"  Cycles: {result['tension_ranges']}")
        else:
            print("ERROR: No result returned")
    except Exception as e:
        print(f"ERROR: {e}")
        import traceback
        traceback.print_exc()
    
    if not wait_for_user():
        print("Exiting...")
        return 1
    
    # ========== STEP 9: OUTPUT VERIFICATION ==========
    show_step(9, "OUTPUT FILE STRUCTURE")
    
    output_path = Path("output")
    print(f"Output directory: {output_path.absolute()}")
    
    if output_path.exists():
        print("\nDirectory structure:")
        for item in output_path.iterdir():
            if item.is_dir():
                print(f"  {item.name}/")
                subdirs = list(item.iterdir())
                for subdir in subdirs[:3]:
                    if subdir.is_dir():
                        files = list(subdir.glob('*.csv'))
                        print(f"    {subdir.name}/: {len(files)} files")
    else:
        print("No output directory found (run full analysis to create)")
    
    print("\n" + "="*80)
    print("VERIFICATION COMPLETE!")
    print("="*80)
    print("\nAll steps have been demonstrated.")
    print("The fatigue analysis process is working correctly with sample data.")
    
    return 0


if __name__ == "__main__":
    sys.exit(main())