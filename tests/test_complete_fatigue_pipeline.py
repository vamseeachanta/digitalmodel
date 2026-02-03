#!/usr/bin/env python3
"""
Complete fatigue analysis pipeline demonstration

This script demonstrates the full fatigue analysis workflow:
1. Load reference seastate metadata
2. Load fatigue conditions with occurrence percentages
3. Apply proper scaling to get effective tensions
4. Perform rainflow counting
5. Calculate weighted fatigue damage
6. Compute fatigue life for each configuration
"""

import os
import sys
import pandas as pd
import numpy as np
from pathlib import Path

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

from digitalmodel.structural.fatigue_analysis.rainflow_counter import RainflowCounter
from digitalmodel.structural.fatigue_analysis.fatigue_damage_calculator import (
    FatigueDamageCalculator, calculate_combined_damage
)


def generate_realistic_time_trace(base_tension=1000, amplitude_ratio=0.1, 
                                 duration=100, sample_rate=0.1):
    """
    Generate a realistic strut tension time trace
    
    Args:
        base_tension: Mean tension in kN (typical strut pretension)
        amplitude_ratio: Ratio of dynamic to mean tension
        duration: Duration in seconds
        sample_rate: Sample interval in seconds
    """
    n_points = int(duration / sample_rate)
    time = np.arange(n_points) * sample_rate
    
    # Multi-frequency loading (wave frequencies)
    # Primary wave (10s period)
    primary = amplitude_ratio * base_tension * np.sin(2 * np.pi * time / 10)
    
    # Secondary wave (6s period)
    secondary = 0.3 * amplitude_ratio * base_tension * np.sin(2 * np.pi * time / 6)
    
    # High frequency (vessel response, 2s period)  
    high_freq = 0.1 * amplitude_ratio * base_tension * np.sin(2 * np.pi * time / 2)
    
    # Random component (turbulence)
    random = 0.05 * amplitude_ratio * base_tension * np.random.randn(n_points)
    
    # Combine
    tension = base_tension + primary + secondary + high_freq + random
    
    # Ensure positive (struts can't go into compression)
    tension = np.maximum(tension, 0.1 * base_tension)
    
    return time, tension


def process_configuration(config_name, fatigue_conditions_df, base_tension=1000):
    """
    Process fatigue analysis for one vessel configuration
    
    Args:
        config_name: Configuration name (e.g., 'FSTs_Light')
        fatigue_conditions_df: DataFrame with fatigue conditions
        base_tension: Base strut tension in kN
    """
    
    print(f"\n{'='*70}")
    print(f"CONFIGURATION: {config_name}")
    print(f"{'='*70}")
    
    # Initialize fatigue calculator
    calculator = FatigueDamageCalculator(
        sn_curve=FatigueDamageCalculator.CURVES['ABS_E_AIR'],
        scf=1.5,  # Stress concentration factor for welded joints
        design_life_years=25
    )
    
    # Store damage contributions
    damage_contributions = []
    
    # Process subset of fatigue conditions for demonstration
    sample_conditions = fatigue_conditions_df.sample(n=min(10, len(fatigue_conditions_df)))
    
    for idx, row in sample_conditions.iterrows():
        condition_id = row.get('Row', idx)
        wind_speed = row['Wind Speed (m/s)']
        wave_hs = row.get('Hs (m)', row.get('Significant Wave Height (m)', 1.0))
        occurrence = row.get('Occurrence (%)', row.get('Annual Occurrence (%)', 1.0)) / 100.0
        
        print(f"\n  Condition {condition_id}:")
        print(f"    Wind: {wind_speed} m/s, Wave Hs: {wave_hs} m")
        print(f"    Annual occurrence: {occurrence*100:.2f}%")
        
        # Generate scaled time trace
        # Scaling affects the amplitude of dynamic loading
        wind_factor = (wind_speed / 10) ** 2
        wave_factor = wave_hs / 0.5
        combined_factor = np.sqrt(wind_factor**2 + wave_factor**2)  # Combined effect
        
        # Generate time trace with scaled amplitude
        time, tension = generate_realistic_time_trace(
            base_tension=base_tension,
            amplitude_ratio=0.05 * combined_factor,  # 5% base, scaled by conditions
            duration=100,  # 100 seconds for demo (would be 10800 in production)
            sample_rate=0.1
        )
        
        # Apply rainflow counting
        counter = RainflowCounter(gate_value=10.0)  # 10 kN gate
        ranges, counts = counter.count_cycles(tension)
        
        if len(ranges) == 0:
            print(f"    No cycles found")
            continue
        
        # Convert to stress (simplified - actual would use structural model)
        # Assume cross-sectional area of 0.1 m² for demonstration
        area = 0.1  # m²
        stress_ranges = ranges / area / 1000  # Convert kN to MPa
        
        # Calculate fatigue damage for this condition
        results = calculator.calculate_fatigue_life(
            stress_ranges=stress_ranges,
            cycle_counts=counts,
            time_duration=100.0,  # Match trace duration
            occurrence_weight=occurrence  # Weight by annual occurrence
        )
        
        print(f"    Cycles found: {len(ranges)}")
        print(f"    Max stress range: {np.max(stress_ranges):.1f} MPa")
        print(f"    Weighted annual damage: {results['weighted_damage']*365.25*24*36:.6e}")
        
        damage_contributions.append({
            'condition_id': condition_id,
            'wind_speed': wind_speed,
            'wave_hs': wave_hs,
            'occurrence': occurrence,
            'annual_damage': results['annual_damage'] * occurrence,
            'cycles': len(ranges)
        })
    
    # Calculate total damage
    total_annual_damage = sum(d['annual_damage'] for d in damage_contributions)
    
    # Calculate fatigue life
    if total_annual_damage > 0:
        fatigue_life = 1.0 / total_annual_damage
    else:
        fatigue_life = float('inf')
    
    design_factor = fatigue_life / calculator.design_life_years
    
    print(f"\n  {'='*50}")
    print(f"  CONFIGURATION SUMMARY: {config_name}")
    print(f"  Total annual damage: {total_annual_damage:.6e}")
    print(f"  Fatigue life: {fatigue_life:.1f} years")
    print(f"  Design factor: {design_factor:.2f}")
    print(f"  Passes 25-year check: {'Yes' if design_factor >= 1.0 else 'No'}")
    
    return {
        'configuration': config_name,
        'total_annual_damage': total_annual_damage,
        'fatigue_life_years': fatigue_life,
        'design_factor': design_factor,
        'passes_check': design_factor >= 1.0,
        'damage_contributions': damage_contributions
    }


def main():
    """Main execution"""
    
    print("="*70)
    print("COMPLETE FATIGUE ANALYSIS PIPELINE DEMONSTRATION")
    print("="*70)
    
    # Load fatigue conditions
    base_dir = Path("specs/modules/fatigue-analysis/strut-foundation-rainflow")
    
    try:
        fatigue_df = pd.read_csv(base_dir / "fatigue_seastates.csv", encoding='latin-1')
        print(f"\nLoaded {len(fatigue_df)} fatigue conditions")
        
        # Show occurrence distribution
        print(f"Total annual occurrence: {fatigue_df['Occurrence (%)'].sum():.1f}%")
        
    except FileNotFoundError:
        print("Fatigue conditions file not found. Using synthetic data.")
        # Create synthetic fatigue conditions
        fatigue_df = pd.DataFrame({
            'Row': range(1, 11),
            'Wind Speed (m/s)': np.random.uniform(5, 25, 10),
            'Hs (m)': np.random.uniform(0.5, 3.0, 10),
            'Occurrence (%)': np.random.uniform(0.5, 5.0, 10)
        })
        # Normalize to 100%
        fatigue_df['Occurrence (%)'] *= 100 / fatigue_df['Occurrence (%)'].sum()
    
    # Define vessel configurations
    configurations = [
        {'name': 'FSTs_Light', 'base_tension': 800},
        {'name': 'FSTs_Full', 'base_tension': 1200},
        {'name': 'FSTs_Light_LNGC_Full', 'base_tension': 1000},
        {'name': 'FSTs_Full_LNGC_Light', 'base_tension': 1100}
    ]
    
    # Process each configuration
    all_results = []
    
    for config in configurations:
        results = process_configuration(
            config['name'], 
            fatigue_df,
            config['base_tension']
        )
        all_results.append(results)
    
    # Final summary
    print("\n" + "="*70)
    print("FINAL SUMMARY - ALL CONFIGURATIONS")
    print("="*70)
    
    summary_df = pd.DataFrame([
        {
            'Configuration': r['configuration'],
            'Fatigue Life (years)': f"{r['fatigue_life_years']:.1f}",
            'Design Factor': f"{r['design_factor']:.2f}",
            'Status': 'PASS' if r['passes_check'] else 'FAIL'
        }
        for r in all_results
    ])
    
    print("\n" + summary_df.to_string(index=False))
    
    # Save results
    output_dir = base_dir / "output" / "complete_pipeline"
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Save detailed results
    for result in all_results:
        config_name = result['configuration']
        
        # Save damage contributions
        contrib_df = pd.DataFrame(result['damage_contributions'])
        contrib_file = output_dir / f"{config_name}_damage_contributions.csv"
        contrib_df.to_csv(contrib_file, index=False)
        print(f"\nSaved: {contrib_file}")
    
    # Save summary
    summary_file = output_dir / "fatigue_life_summary.csv"
    summary_df.to_csv(summary_file, index=False)
    print(f"Saved: {summary_file}")
    
    print("\n" + "="*70)
    print("PIPELINE DEMONSTRATION COMPLETE")
    print("="*70)
    print("\nNOTE: This demonstration uses:")
    print("  - Synthetic time traces with realistic characteristics")
    print("  - Proper occurrence weighting from fatigue conditions")
    print("  - S-N curve fatigue damage calculations")
    print("  - Multiple vessel configurations")
    print("\nFor production use, replace synthetic traces with actual OrcaFlex data")


if __name__ == "__main__":
    main()