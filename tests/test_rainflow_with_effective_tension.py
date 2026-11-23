#!/usr/bin/env python3
"""
Test the complete fatigue analysis pipeline with sample effective tension data

This script:
1. Loads effective tension time traces generated earlier
2. Applies rainflow counting algorithm  
3. Calculates fatigue damage using S-N curves
4. Reports fatigue life for each configuration
"""

import os
import sys
import pandas as pd
import numpy as np
from pathlib import Path

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

from digitalmodel.structural.fatigue_apps.rainflow_counter import RainflowCounter
from digitalmodel.structural.fatigue_apps.fatigue_damage_calculator import FatigueDamageCalculator


def process_effective_tension_file(filepath: Path, output_dir: Path):
    """Process a single effective tension file through rainflow and damage calculation"""
    
    print(f"\n{'='*60}")
    print(f"Processing: {filepath.name}")
    print(f"{'='*60}")
    
    # Load the effective tension data
    try:
        df = pd.read_csv(filepath, comment='#')
        print(f"Loaded {len(df)} data points")
        
        # Extract effective tension column
        if 'effective_tension_kN' in df.columns:
            effective_tension = df['effective_tension_kN'].values
        else:
            print("Warning: effective_tension_kN column not found, using first data column")
            effective_tension = df.iloc[:, 1].values  # Skip time column
        
        # Get statistics
        print(f"Tension statistics:")
        print(f"  Min: {np.min(effective_tension):.2f} kN")
        print(f"  Max: {np.max(effective_tension):.2f} kN")
        print(f"  Mean: {np.mean(effective_tension):.2f} kN")
        print(f"  Std: {np.std(effective_tension):.2f} kN")
        
        # Apply rainflow counting
        print("\nApplying rainflow counting...")
        counter = RainflowCounter(gate_value=5.0)  # 5 kN gate filter
        ranges, counts = counter.count_cycles(effective_tension)
        
        if len(ranges) == 0:
            print("No cycles found in data")
            return None
        
        # Get cycle statistics
        stats = counter.get_cycle_statistics()
        print(f"Rainflow results:")
        print(f"  Total cycles: {stats['total_cycles']}")
        print(f"  Full cycles: {stats['full_cycles']}")
        print(f"  Half cycles: {stats['half_cycles']}")
        print(f"  Max range: {stats['max_range']:.2f} kN")
        print(f"  Mean range: {stats['mean_range']:.2f} kN")
        
        # Convert to stress (assuming simple conversion factor)
        # In practice, this would use actual structural calculations
        stress_conversion = 1.0  # MPa/kN - placeholder
        stress_ranges = ranges * stress_conversion
        
        # Calculate fatigue damage
        print("\nCalculating fatigue damage...")
        calculator = FatigueDamageCalculator(
            sn_curve=FatigueDamageCalculator.CURVES['ABS_E_AIR'],
            scf=1.2,  # Stress concentration factor
            design_life_years=25
        )
        
        # Time duration from the data
        time_duration = len(df) * 0.1  # 0.1 second sampling
        
        results = calculator.calculate_fatigue_life(
            stress_ranges=stress_ranges,
            cycle_counts=counts,
            time_duration=time_duration,
            occurrence_weight=1.0  # Would come from fatigue_seastates.csv
        )
        
        print(f"Fatigue life results:")
        print(f"  Damage per simulation: {results['damage']:.6e}")
        print(f"  Annual damage: {results['annual_damage']:.6e}")
        print(f"  Fatigue life: {results['fatigue_life_years']:.1f} years")
        print(f"  Design factor: {results['design_factor']:.2f}")
        print(f"  Passes check: {results['passes_check']}")
        
        # Save rainflow results
        output_file = output_dir / f"rainflow_{filepath.stem}.csv"
        counter.export_results(str(output_file))
        print(f"\nRainflow results saved to: {output_file}")
        
        # Save damage results  
        damage_file = output_dir / f"damage_{filepath.stem}.csv"
        damage_df = pd.DataFrame([results])
        damage_df.to_csv(damage_file, index=False)
        print(f"Damage results saved to: {damage_file}")
        
        return results
        
    except Exception as e:
        print(f"Error processing {filepath}: {e}")
        return None


def main():
    """Main execution function"""
    
    print("="*60)
    print("FATIGUE ANALYSIS PIPELINE TEST")
    print("Testing Rainflow Counting with Effective Tension Data")
    print("="*60)
    
    # Set up paths
    base_dir = Path("specs/modules/fatigue-analysis/strut-foundation-rainflow")
    input_dir = base_dir / "output" / "sample_effective_tension"
    output_dir = base_dir / "output" / "rainflow_analysis"
    
    # Create output directory
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Check for input files
    if not input_dir.exists():
        print(f"Error: Input directory not found: {input_dir}")
        print("Please run test_fatigue_analysis.py first to generate effective tension data")
        return
    
    # Process all effective tension files
    csv_files = list(input_dir.glob("*.csv"))
    
    if not csv_files:
        print(f"No CSV files found in {input_dir}")
        return
    
    print(f"\nFound {len(csv_files)} effective tension files to process")
    
    all_results = []
    
    for csv_file in csv_files[:5]:  # Process first 5 files for testing
        results = process_effective_tension_file(csv_file, output_dir)
        if results:
            all_results.append({
                'file': csv_file.name,
                'fatigue_life_years': results['fatigue_life_years'],
                'design_factor': results['design_factor'],
                'passes_check': results['passes_check']
            })
    
    # Summary report
    if all_results:
        print("\n" + "="*60)
        print("SUMMARY REPORT")
        print("="*60)
        
        summary_df = pd.DataFrame(all_results)
        print("\nFatigue Life Summary:")
        print(summary_df.to_string(index=False))
        
        # Save summary
        summary_file = output_dir / "fatigue_life_summary.csv"
        summary_df.to_csv(summary_file, index=False)
        print(f"\nSummary saved to: {summary_file}")
        
        # Statistics
        print(f"\nOverall Statistics:")
        print(f"  Min fatigue life: {summary_df['fatigue_life_years'].min():.1f} years")
        print(f"  Max fatigue life: {summary_df['fatigue_life_years'].max():.1f} years")
        print(f"  Mean fatigue life: {summary_df['fatigue_life_years'].mean():.1f} years")
        print(f"  Configurations passing check: {summary_df['passes_check'].sum()}/{len(summary_df)}")
    
    print("\n" + "="*60)
    print("Pipeline test complete!")
    print("="*60)


if __name__ == "__main__":
    main()