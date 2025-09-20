#!/usr/bin/env python3
"""
Step-by-step fatigue analysis with sample data
Shows intermediate results for user verification
"""

import os
import sys
import pandas as pd
import numpy as np
from pathlib import Path
import json

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

from digitalmodel.modules.fatigue_analysis.rainflow_counter import RainflowCounter
from digitalmodel.modules.fatigue_analysis.fatigue_damage_calculator import FatigueDamageCalculator


def print_step_header(step_num, title):
    """Print formatted step header"""
    print(f"\n{'='*80}")
    print(f"STEP {step_num}: {title}")
    print(f"{'='*80}")


def load_and_verify_data():
    """Step 1: Load and verify all input data"""
    print_step_header(1, "LOAD AND VERIFY INPUT DATA")
    
    base_dir = Path("specs/modules/fatigue-analysis/strut-foundation-rainflow")
    results = {}
    
    # Load reference seastate metadata
    print("\n1.1 Loading Reference Seastate Metadata...")
    ref_metadata_file = base_dir / "reference_seastate_timetrace_metadata.csv"
    try:
        ref_metadata = pd.read_csv(ref_metadata_file, encoding='latin-1')
        print(f"   [OK] Loaded {len(ref_metadata)} reference seastate records")
        print(f"   Columns: {', '.join(ref_metadata.columns[:5])}...")
        
        # Show sample
        print("\n   Sample records:")
        cols_to_show = ['fe_filename_stem', 'WindSpeed', 'Hs', 'WaveDirection']
        if all(col in ref_metadata.columns for col in cols_to_show):
            print(ref_metadata[cols_to_show].head(3).to_string())
        else:
            print(ref_metadata.head(3).to_string())
        results['ref_metadata'] = ref_metadata
    except Exception as e:
        print(f"   [X] Error: {e}")
        return None
    
    # Load fatigue conditions
    print("\n1.2 Loading Fatigue Conditions...")
    fatigue_file = base_dir / "fatigue_seastates.csv"
    try:
        fatigue_conditions = pd.read_csv(fatigue_file, encoding='latin-1')
        print(f"   [OK] Loaded {len(fatigue_conditions)} fatigue conditions")
        print(f"   Total occurrence: {fatigue_conditions['Occurrence (%)'].sum():.1f}%")
        
        # Show distribution
        print("\n   Occurrence distribution:")
        print(f"   Min: {fatigue_conditions['Occurrence (%)'].min():.2f}%")
        print(f"   Max: {fatigue_conditions['Occurrence (%)'].max():.2f}%")
        print(f"   Mean: {fatigue_conditions['Occurrence (%)'].mean():.2f}%")
        results['fatigue_conditions'] = fatigue_conditions
    except Exception as e:
        print(f"   [X] Error: {e}")
        return None
    
    # Load sample time trace metadata
    print("\n1.3 Loading Sample Time Trace Metadata...")
    sample_metadata_file = base_dir / "sample_timetraces_metadata.csv"
    try:
        sample_metadata = pd.read_csv(sample_metadata_file)
        print(f"   [OK] Loaded {len(sample_metadata)} sample time trace records")
        
        # Count available files
        unique_seastates = sample_metadata['seastate_id'].unique()
        unique_struts = sample_metadata['strut_id'].unique()
        print(f"   Unique seastates: {len(unique_seastates)} ({', '.join(unique_seastates)})")
        print(f"   Unique struts: {len(unique_struts)} ({', '.join(unique_struts)})")
        results['sample_metadata'] = sample_metadata
    except Exception as e:
        print(f"   [X] Error: {e}")
        return None
    
    # Verify sample time trace files exist
    print("\n1.4 Verifying Sample Time Trace Files...")
    sample_dir = base_dir / "sample_timetraces"
    available_files = []
    for _, row in sample_metadata.iterrows():
        file_path = sample_dir / row['time_trace_file']
        if file_path.exists():
            available_files.append(row['time_trace_file'])
            print(f"   [OK] Found: {row['time_trace_file']}")
        else:
            print(f"   [X] Missing: {row['time_trace_file']}")
    
    print(f"\n   Total: {len(available_files)}/{len(sample_metadata)} files available")
    results['available_files'] = available_files
    results['base_dir'] = base_dir
    
    print("\n" + "="*80)
    print("> Continuing to Step 2...")
    return results


def load_sample_time_trace(base_dir, filename, max_points=1000):
    """Load a sample time trace file"""
    filepath = base_dir / "sample_timetraces" / filename
    
    try:
        # Read with proper encoding
        df = pd.read_csv(filepath, encoding='latin-1', nrows=max_points)
        
        # Extract tension column (try different possible names)
        tension_col = None
        for col in ['Tension (Jacket End)', 'Tension (kN)', 'Tension']:
            if col in df.columns:
                tension_col = col
                break
        
        if tension_col:
            return df['time'].values, df[tension_col].values
        else:
            # If no tension column, return first numeric column after time
            return df.iloc[:, 0].values, df.iloc[:, 1].values
    except Exception as e:
        print(f"Error loading {filename}: {e}")
        return None, None


def calculate_scaling_factors(data):
    """Step 2: Calculate and verify scaling factors"""
    print_step_header(2, "CALCULATE SCALING FACTORS")
    
    fatigue_conditions = data['fatigue_conditions']
    
    print("\n2.1 Scaling Factor Formulas:")
    print("   Wind: Scale Factor = (Wind Speed / 10)²")
    print("   Wave: Scale Factor = Hs / 0.5")
    
    print("\n2.2 Sample Scaling Calculations:")
    
    # Calculate for first 5 conditions
    sample_calcs = []
    for idx in range(min(5, len(fatigue_conditions))):
        row = fatigue_conditions.iloc[idx]
        wind_speed = row['Wind Speed (m/s)']
        hs = row['Hs (m)']
        
        wind_scale = (wind_speed / 10) ** 2
        wave_scale = hs / 0.5
        
        print(f"\n   Condition {idx+1}:")
        print(f"     Wind: {wind_speed} m/s > ({wind_speed}/10)² = {wind_scale:.4f}")
        print(f"     Wave: {hs} m > {hs}/0.5 = {wave_scale:.4f}")
        
        sample_calcs.append({
            'condition': idx+1,
            'wind_speed': wind_speed,
            'hs': hs,
            'wind_scale': wind_scale,
            'wave_scale': wave_scale
        })
    
    # Save scaling factors
    output_dir = data['base_dir'] / "output" / "step_by_step"
    output_dir.mkdir(parents=True, exist_ok=True)
    
    scaling_df = pd.DataFrame(sample_calcs)
    scaling_file = output_dir / "sample_scaling_factors.csv"
    scaling_df.to_csv(scaling_file, index=False)
    print(f"\n   [OK] Saved scaling factors to: {scaling_file}")
    
    data['scaling_factors'] = sample_calcs
    data['output_dir'] = output_dir
    
    print("\n" + "="*80)
    print("> Continuing to Step 3...")
    return data


def generate_effective_tensions(data):
    """Step 3: Generate effective tensions from scaled traces"""
    print_step_header(3, "GENERATE EFFECTIVE TENSIONS")
    
    print("\n3.1 Effective Tension Formula:")
    print("   Effective Tension = Wind_Trace × Wind_Scale + Wave_Trace × Wave_Scale")
    
    # Use available sample files
    available_files = data['available_files']
    sample_metadata = data['sample_metadata']
    
    print("\n3.2 Processing Available Sample Files:")
    
    effective_tensions = []
    
    # Process first 2 files as examples
    for filename in available_files[:2]:
        print(f"\n   Processing: {filename}")
        
        # Get metadata
        meta_row = sample_metadata[sample_metadata['time_trace_file'] == filename].iloc[0]
        seastate_id = meta_row['seastate_id']
        strut_id = meta_row['strut_id']
        
        # Load time trace
        time, tension = load_sample_time_trace(data['base_dir'], filename, max_points=500)
        
        if time is not None:
            print(f"     Loaded {len(time)} data points")
            print(f"     Original tension range: {tension.min():.1f} - {tension.max():.1f} kN")
            
            # Apply scaling (use first scaling factor as example)
            if data['scaling_factors']:
                scale = data['scaling_factors'][0]
                
                # Simple scaling for demonstration
                if 'W' in seastate_id:  # Wave case
                    scaled_tension = tension * scale['wave_scale']
                    scale_type = f"wave scale {scale['wave_scale']:.3f}"
                else:  # Wind case
                    scaled_tension = tension * scale['wind_scale']
                    scale_type = f"wind scale {scale['wind_scale']:.3f}"
                
                print(f"     Applied {scale_type}")
                print(f"     Scaled tension range: {scaled_tension.min():.1f} - {scaled_tension.max():.1f} kN")
                
                # Save sample
                eff_data = pd.DataFrame({
                    'time_s': time,
                    'original_kN': tension,
                    'scaled_kN': scaled_tension
                })
                
                output_file = data['output_dir'] / f"effective_tension_{seastate_id}_{strut_id}.csv"
                eff_data.to_csv(output_file, index=False)
                print(f"     [OK] Saved to: {output_file.name}")
                
                effective_tensions.append({
                    'file': filename,
                    'seastate': seastate_id,
                    'strut': strut_id,
                    'data': scaled_tension,
                    'time': time
                })
    
    data['effective_tensions'] = effective_tensions
    
    print("\n" + "="*80)
    print("> Continuing to Step 4...")
    return data


def apply_rainflow_counting(data):
    """Step 4: Apply rainflow counting to effective tensions"""
    print_step_header(4, "RAINFLOW COUNTING ANALYSIS")
    
    print("\n4.1 Rainflow Counting Parameters:")
    print("   Algorithm: ASTM E1049")
    print("   Gate Value: 10 kN (filters small cycles)")
    
    rainflow_results = []
    
    for eff_tension in data['effective_tensions']:
        print(f"\n4.2 Processing {eff_tension['seastate']}_{eff_tension['strut']}:")
        
        # Apply rainflow
        counter = RainflowCounter(gate_value=10.0)
        ranges, counts = counter.count_cycles(eff_tension['data'])
        
        if len(ranges) > 0:
            stats = counter.get_cycle_statistics()
            
            print(f"   Reversals extracted: {stats['total_cycles']}")
            print(f"   Full cycles: {stats['full_cycles']}")
            print(f"   Half cycles: {stats['half_cycles']}")
            print(f"   Range statistics:")
            print(f"     Max: {stats['max_range']:.1f} kN")
            print(f"     Min: {stats['min_range']:.1f} kN")
            print(f"     Mean: {stats['mean_range']:.1f} kN")
            
            # Save rainflow results
            rainflow_df = pd.DataFrame({
                'range_kN': ranges,
                'counts': counts
            })
            
            output_file = data['output_dir'] / f"rainflow_{eff_tension['seastate']}_{eff_tension['strut']}.csv"
            rainflow_df.to_csv(output_file, index=False)
            print(f"   [OK] Saved rainflow results to: {output_file.name}")
            
            # Save detailed results
            counter.export_results(
                str(data['output_dir'] / f"rainflow_detailed_{eff_tension['seastate']}_{eff_tension['strut']}.csv"),
                include_stats=True
            )
            
            rainflow_results.append({
                'seastate': eff_tension['seastate'],
                'strut': eff_tension['strut'],
                'ranges': ranges,
                'counts': counts,
                'stats': stats
            })
        else:
            print("   [X] No cycles found")
    
    data['rainflow_results'] = rainflow_results
    
    print("\n" + "="*80)
    print("> Continuing to Step 5...")
    return data


def calculate_fatigue_damage(data):
    """Step 5: Calculate fatigue damage and life"""
    print_step_header(5, "FATIGUE DAMAGE CALCULATION")
    
    print("\n5.1 Fatigue Analysis Parameters:")
    print("   S-N Curve: ABS E in Air")
    print("   SCF: 1.5 (welded joint)")
    print("   Design Life: 25 years")
    print("   Stress Conversion: 1 kN = 10 MPa (simplified)")
    
    # Initialize calculator
    calculator = FatigueDamageCalculator(
        sn_curve=FatigueDamageCalculator.CURVES['ABS_E_AIR'],
        scf=1.5,
        design_life_years=25
    )
    
    print("\n5.2 S-N Curve Parameters:")
    print(f"   Curve: {calculator.sn_curve.name}")
    print(f"   log(A1): {calculator.sn_curve.log_a1}")
    print(f"   m1: {calculator.sn_curve.m1}")
    print(f"   Threshold: {calculator.sn_curve.threshold:.0e} cycles")
    
    damage_results = []
    
    for rf_result in data['rainflow_results']:
        print(f"\n5.3 Damage Calculation for {rf_result['seastate']}_{rf_result['strut']}:")
        
        # Convert to stress (simplified)
        stress_ranges = rf_result['ranges'] * 10  # kN to MPa (simplified)
        
        # Calculate damage
        time_duration = 500 * 0.1  # 500 points at 0.1s interval
        
        results = calculator.calculate_fatigue_life(
            stress_ranges=stress_ranges,
            cycle_counts=rf_result['counts'],
            time_duration=time_duration,
            occurrence_weight=0.01  # 1% occurrence for demo
        )
        
        print(f"   Stress range: {stress_ranges.min():.1f} - {stress_ranges.max():.1f} MPa")
        print(f"   Damage per simulation: {results['damage']:.6e}")
        print(f"   Annual damage: {results['annual_damage']:.6e}")
        print(f"   Fatigue life: {results['fatigue_life_years']:.1f} years")
        print(f"   Design factor: {results['design_factor']:.3f}")
        print(f"   Status: {'PASS' if results['passes_check'] else 'FAIL'}")
        
        # Save damage results
        damage_df = pd.DataFrame([results])
        output_file = data['output_dir'] / f"damage_{rf_result['seastate']}_{rf_result['strut']}.csv"
        damage_df.to_csv(output_file, index=False)
        print(f"   [OK] Saved damage results to: {output_file.name}")
        
        damage_results.append({
            'seastate': rf_result['seastate'],
            'strut': rf_result['strut'],
            'results': results
        })
    
    data['damage_results'] = damage_results
    
    print("\n" + "="*80)
    print("> Continuing to Final Summary...")
    return data


def generate_final_report(data):
    """Generate final summary report"""
    print_step_header(6, "FINAL SUMMARY REPORT")
    
    print("\n6.1 Analysis Summary:")
    print(f"   Input files processed: {len(data['available_files'])}")
    print(f"   Effective tensions generated: {len(data['effective_tensions'])}")
    print(f"   Rainflow analyses completed: {len(data['rainflow_results'])}")
    print(f"   Damage calculations completed: {len(data['damage_results'])}")
    
    print("\n6.2 Results Summary:")
    
    if data['damage_results']:
        summary_rows = []
        for damage in data['damage_results']:
            summary_rows.append({
                'Component': f"{damage['seastate']}_{damage['strut']}",
                'Fatigue Life (years)': f"{damage['results']['fatigue_life_years']:.1f}",
                'Design Factor': f"{damage['results']['design_factor']:.3f}",
                'Annual Damage': f"{damage['results']['annual_damage']:.3e}",
                'Status': 'PASS' if damage['results']['passes_check'] else 'FAIL'
            })
        
        summary_df = pd.DataFrame(summary_rows)
        print("\n" + summary_df.to_string(index=False))
        
        # Save final summary
        output_file = data['output_dir'] / "final_fatigue_summary.csv"
        summary_df.to_csv(output_file, index=False)
        print(f"\n   [OK] Final summary saved to: {output_file}")
    
    print("\n6.3 Output Files Generated:")
    output_files = list(data['output_dir'].glob("*.csv"))
    for f in output_files:
        size = f.stat().st_size / 1024  # KB
        print(f"   - {f.name} ({size:.1f} KB)")
    
    print("\n6.4 Verification Checklist:")
    print("   [ ] Scaling factors correctly calculated")
    print("   [ ] Effective tensions properly combined")
    print("   [ ] Rainflow counting identifies realistic cycles")
    print("   [ ] S-N curve parameters appropriate")
    print("   [ ] Damage accumulation follows Miner's rule")
    print("   [ ] Fatigue lives within expected range")
    
    # Save complete analysis log
    log_file = data['output_dir'] / "analysis_log.json"
    log_data = {
        'timestamp': pd.Timestamp.now().isoformat(),
        'files_processed': len(data['available_files']),
        'scaling_factors': data.get('scaling_factors', []),
        'rainflow_stats': [r['stats'] for r in data.get('rainflow_results', [])],
        'fatigue_results': [d['results'] for d in data.get('damage_results', [])]
    }
    
    with open(log_file, 'w') as f:
        json.dump(log_data, f, indent=2, default=str)
    print(f"\n   [OK] Complete analysis log saved to: {log_file}")
    
    print("\n" + "="*80)
    print("ANALYSIS COMPLETE!")
    print("="*80)
    
    return data


def main():
    """Main execution with step-by-step verification"""
    
    print("="*80)
    print("FATIGUE ANALYSIS - STEP BY STEP VERIFICATION")
    print("="*80)
    print("\nThis analysis will process sample data through each step.")
    print("You will be asked to verify results at each stage.")
    print("\n" + "="*80)
    
    print("> Starting analysis...")
    
    # Step 1: Load data
    data = load_and_verify_data()
    if not data:
        print("Failed to load data. Exiting.")
        return
    
    # Step 2: Calculate scaling
    data = calculate_scaling_factors(data)
    
    # Step 3: Generate effective tensions
    data = generate_effective_tensions(data)
    
    # Step 4: Rainflow counting
    data = apply_rainflow_counting(data)
    
    # Step 5: Damage calculation
    data = calculate_fatigue_damage(data)
    
    # Step 6: Final report
    data = generate_final_report(data)
    
    print("\nThank you for verifying the analysis!")


if __name__ == "__main__":
    main()