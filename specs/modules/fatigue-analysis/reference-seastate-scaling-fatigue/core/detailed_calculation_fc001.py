#!/usr/bin/env python
"""
Detailed Calculation Example: FC001 Fatigue Analysis
Demonstrates complete logical flow from Step 7 (rainflow counting) to Step 8 (stress conversion)

This script shows 2-3 specific tension ranges from FC001 fatigue condition and walks through
each calculation step in detail, demonstrating the complete logical flow.
"""

import sys
import numpy as np
import pandas as pd
from pathlib import Path
import datetime

def detailed_calculation_fc001():
    """
    Demonstrate detailed calculation flow from rainflow counting to stress conversion
    for FC001 fatigue condition.
    """
    
    print("="*80)
    print("DETAILED CALCULATION EXAMPLE: FC001 FATIGUE ANALYSIS")
    print("="*80)
    print(f"Analysis Date: {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print()
    
    # ===========================================
    # STEP 1: LOAD STEP 7 OUTPUT (RAINFLOW)
    # ===========================================
    print("STEP 1: LOADING STEP 7 OUTPUT (RAINFLOW COUNTING)")
    print("-" * 60)
    
    rainflow_file = Path("output/verification/step_by_step/rainflow_FC001_Strut1.csv")
    
    if not rainflow_file.exists():
        print(f"[ERROR] Rainflow output not found: {rainflow_file}")
        print("        Please run Step 7 verification first")
        return False
    
    print(f"Loading: {rainflow_file.name}")
    rainflow_df = pd.read_csv(rainflow_file)
    
    print(f"Total cycles from rainflow counting: {len(rainflow_df)}")
    print(f"Columns: {', '.join(rainflow_df.columns)}")
    print()
    
    # ===========================================
    # STEP 2: LOAD CONVERSION TABLE
    # ===========================================
    print("STEP 2: LOADING TENSION-TO-STRESS CONVERSION TABLE")
    print("-" * 60)
    
    conversion_file = Path("../input/tension_range_to_stress_range_function.csv")
    if not conversion_file.exists():
        print(f"[ERROR] Conversion file not found: {conversion_file}")
        return False
    
    conversion_df = pd.read_csv(conversion_file)
    print(f"Loading: {conversion_file.name}")
    print("Conversion Table:")
    for _, row in conversion_df.iterrows():
        tension = row['Tension Range (kN)']
        stress = row['Stress Range (Mpa)']
        print(f"  {tension:5.0f} kN -> {stress:6.1f} MPa")
    
    tension_values = conversion_df['Tension Range (kN)'].values
    stress_values = conversion_df['Stress Range (Mpa)'].values
    conversion_factor = stress_values[2] / tension_values[2]  # Using 500kN->125MPa point
    print(f"\nLinear conversion factor: {conversion_factor:.3f} MPa/kN")
    print("(This factor includes cross-sectional area and SCF effects)")
    print()
    
    # ===========================================
    # STEP 3: SELECT REPRESENTATIVE EXAMPLES
    # ===========================================
    print("STEP 3: SELECTING REPRESENTATIVE TENSION RANGES")
    print("-" * 60)
    
    # Sort by tension range and select small, medium, large examples
    rainflow_df_sorted = rainflow_df.sort_values('range_kN')
    
    # Select examples: small (10th percentile), medium (50th percentile), large (90th percentile)
    n_cycles = len(rainflow_df_sorted)
    small_idx = int(0.1 * n_cycles)
    medium_idx = int(0.5 * n_cycles)
    large_idx = int(0.9 * n_cycles)
    
    examples = [
        {"type": "Small", "idx": small_idx, "data": rainflow_df_sorted.iloc[small_idx]},
        {"type": "Medium", "idx": medium_idx, "data": rainflow_df_sorted.iloc[medium_idx]},
        {"type": "Large", "idx": large_idx, "data": rainflow_df_sorted.iloc[large_idx]},
    ]
    
    print("Selected Examples:")
    for example in examples:
        tension = example["data"]["range_kN"]
        yearly_cycles = example["data"]["yearly_cycles"]
        print(f"  {example['type']:6s}: {tension:8.2f} kN, {yearly_cycles:.2e} cycles/year")
    print()
    
    # ===========================================
    # STEP 4: DETAILED CALCULATION FOR EACH EXAMPLE
    # ===========================================
    print("STEP 4: DETAILED CALCULATION BREAKDOWN")
    print("=" * 60)
    
    # Create results table
    calculation_results = []
    
    for example in examples:
        print(f"\n{example['type'].upper()} TENSION RANGE EXAMPLE")
        print("-" * 40)
        
        # Extract data from rainflow results
        tension_range_kN = example["data"]["range_kN"]
        cycle_count_sample = example["data"]["cycle_count"]
        yearly_cycles = example["data"]["yearly_cycles"]
        mean_kN = example["data"]["mean_kN"]
        
        print(f"Original Rainflow Results:")
        print(f"  Tension Range:        {tension_range_kN:.4f} kN")
        print(f"  Mean Tension:         {mean_kN:.4f} kN")
        print(f"  Sample Cycle Count:   {cycle_count_sample:.1f} cycles")
        print(f"  Yearly Cycles:        {yearly_cycles:.4e} cycles/year")
        print()
        
        # Yearly normalization calculation (showing the breakdown)
        print(f"Yearly Normalization Calculation:")
        sample_duration_s = 1000 * 0.1  # 1000 data points at 0.1s each
        seconds_per_year = 365.25 * 24 * 3600
        fc001_occurrence = 0.20  # 20% occurrence for FC001
        
        time_scale_factor = seconds_per_year / sample_duration_s
        occurrence_factor = fc001_occurrence
        normalization_factor = time_scale_factor * occurrence_factor
        
        yearly_cycles_calculated = cycle_count_sample * normalization_factor
        
        print(f"  Sample Duration:      {sample_duration_s:.1f} seconds")
        print(f"  Seconds per Year:     {seconds_per_year:.0f} seconds")
        print(f"  Time Scale Factor:    {time_scale_factor:.2e}")
        print(f"  FC001 Occurrence:     {fc001_occurrence:.2f} (20%)")
        print(f"  Occurrence Factor:    {occurrence_factor:.2f}")
        print(f"  Total Normalization:  {normalization_factor:.4e}")
        print(f"  Yearly Cycles Calc:   {cycle_count_sample:.1f} x {normalization_factor:.2e} = {yearly_cycles_calculated:.4e}")
        print(f"  Yearly Cycles (file): {yearly_cycles:.4e}")
        match_status = "MATCH" if abs(yearly_cycles - yearly_cycles_calculated) < 1e-6 else "MISMATCH"
        print(f"  Verification:         {match_status}")
        print()
        
        # Stress conversion using interpolation
        print(f"Stress Conversion Calculation:")
        
        # Method 1: Linear interpolation from conversion table
        stress_range_interp = np.interp(tension_range_kN, tension_values, stress_values)
        
        # Method 2: Direct multiplication (for comparison)
        stress_range_direct = tension_range_kN * conversion_factor
        
        print(f"  Method 1 - Table Interpolation:")
        print(f"    Input Tension:      {tension_range_kN:.4f} kN")
        print(f"    Interpolation from: {tension_values} kN")
        print(f"    Interpolation to:   {stress_values} MPa")
        print(f"    Output Stress:      {stress_range_interp:.4f} MPa")
        print()
        print(f"  Method 2 - Direct Factor:")
        print(f"    Conversion Factor:  {conversion_factor:.6f} MPa/kN")
        print(f"    Direct Calculation: {tension_range_kN:.4f} x {conversion_factor:.6f} = {stress_range_direct:.4f} MPa")
        print()
        print(f"  Comparison:")
        print(f"    Interpolated:       {stress_range_interp:.4f} MPa")
        print(f"    Direct:             {stress_range_direct:.4f} MPa")
        difference_pct = abs(stress_range_interp - stress_range_direct) / stress_range_interp * 100 if stress_range_interp > 0 else 0
        print(f"    Difference:         {difference_pct:.2f}%")
        print()
        
        # S-N Curve application preview
        print(f"S-N Curve Application Preview:")
        a_constant = 6.0e11  # DNV-C curve constant
        m_exponent = 3.0     # DNV-C curve exponent
        
        cycles_to_failure = a_constant * (stress_range_interp ** -m_exponent) if stress_range_interp > 0 else 1e20
        damage_per_year = yearly_cycles / cycles_to_failure if cycles_to_failure > 0 else 0
        life_years = 1.0 / damage_per_year if damage_per_year > 0 else float('inf')
        
        print(f"  S-N Curve: N = {a_constant:.1e} x S^(-{m_exponent:.1f})")
        print(f"  Stress Range:         {stress_range_interp:.4f} MPa")
        print(f"  Cycles to Failure:    {cycles_to_failure:.4e}")
        print(f"  Yearly Cycles:        {yearly_cycles:.4e}")
        print(f"  Damage per Year:      {damage_per_year:.4e}")
        print(f"  Life (this cycle):    {life_years:.2e} years")
        print()
        
        # Store results
        calculation_results.append({
            'example_type': example['type'],
            'tension_range_kN': tension_range_kN,
            'cycle_count_sample': cycle_count_sample,
            'yearly_cycles': yearly_cycles,
            'normalization_factor': normalization_factor,
            'stress_range_MPa': stress_range_interp,
            'cycles_to_failure': cycles_to_failure,
            'damage_per_year': damage_per_year,
            'life_years': life_years
        })
    
    # ===========================================
    # STEP 5: SUMMARY TABLE
    # ===========================================
    print("\nSTEP 5: CALCULATION SUMMARY TABLE")
    print("=" * 80)
    
    results_df = pd.DataFrame(calculation_results)
    
    # Create a formatted summary table
    print(f"{'Example':<8} {'Tension':<10} {'Sample':<8} {'Yearly':<12} {'Norm.':<10} {'Stress':<10} {'N_fail':<12} {'Damage':<12} {'Life':<10}")
    print(f"{'Type':<8} {'(kN)':<10} {'Cycles':<8} {'Cycles':<12} {'Factor':<10} {'(MPa)':<10} {'(cycles)':<12} {'(/year)':<12} {'(years)':<10}")
    print("-" * 102)
    
    for _, row in results_df.iterrows():
        print(f"{row['example_type']:<8} "
              f"{row['tension_range_kN']:<10.3f} "
              f"{row['cycle_count_sample']:<8.1f} "
              f"{row['yearly_cycles']:<12.2e} "
              f"{row['normalization_factor']:<10.2e} "
              f"{row['stress_range_MPa']:<10.3f} "
              f"{row['cycles_to_failure']:<12.2e} "
              f"{row['damage_per_year']:<12.2e} "
              f"{row['life_years']:<10.2e}")
    
    print()
    
    # ===========================================
    # STEP 6: KEY INSIGHTS
    # ===========================================
    print("STEP 6: KEY INSIGHTS AND VALIDATION")
    print("-" * 60)
    
    total_damage = results_df['damage_per_year'].sum()
    print(f"Combined damage from 3 examples: {total_damage:.4e} per year")
    print(f"Life if only these 3 cycles:    {1.0/total_damage:.2e} years")
    print()
    
    print("Validation Checks:")
    print(f"[CHECK] Normalization factor consistent: {results_df['normalization_factor'].std():.2e} (low std = good)")
    stress_increases = all(results_df['stress_range_MPa'].diff().dropna() > 0)
    print(f"[CHECK] Stress increases with tension: {stress_increases}")
    damage_increases = all(results_df['damage_per_year'].diff().dropna() > 0)
    print(f"[CHECK] Damage increases with stress: {damage_increases}")
    life_decreases = all(results_df['life_years'].diff().dropna() < 0)
    print(f"[CHECK] Life decreases with stress: {life_decreases}")
    print()
    
    print("Physical Interpretation:")
    print(f"• Small cycles ({results_df.iloc[0]['tension_range_kN']:.1f} kN) contribute minimal damage")
    print(f"• Medium cycles ({results_df.iloc[1]['tension_range_kN']:.1f} kN) start becoming significant")
    print(f"• Large cycles ({results_df.iloc[2]['tension_range_kN']:.1f} kN) dominate fatigue damage")
    damage_ratio = results_df.iloc[2]['damage_per_year']/results_df.iloc[0]['damage_per_year']
    print(f"• Damage ratio (Large/Small): {damage_ratio:.0e}")
    print()
    
    # ===========================================
    # STEP 7: SAVE DETAILED BREAKDOWN
    # ===========================================
    print("STEP 7: SAVING DETAILED BREAKDOWN")
    print("-" * 60)
    
    output_dir = Path("output/verification/step_by_step")
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Save calculation breakdown
    calculation_file = output_dir / "detailed_calculation_fc001_breakdown.csv"
    results_df.to_csv(calculation_file, index=False)
    print(f"Saved calculation breakdown: {calculation_file.name}")
    
    # Save detailed text report
    report_file = output_dir / "detailed_calculation_fc001_report.txt"
    with open(report_file, 'w', encoding='utf-8') as f:
        f.write("DETAILED CALCULATION REPORT: FC001 FATIGUE ANALYSIS\n")
        f.write("=" * 60 + "\n\n")
        f.write(f"Analysis Date: {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
        
        f.write("CALCULATION PARAMETERS:\n")
        f.write(f"  Sample Duration:      {sample_duration_s:.1f} seconds\n")
        f.write(f"  Seconds per Year:     {seconds_per_year:.0f} seconds\n")
        f.write(f"  FC001 Occurrence:     {fc001_occurrence:.2f} (20%)\n")
        f.write(f"  Normalization Factor: {normalization_factor:.4e}\n")
        f.write(f"  Conversion Factor:    {conversion_factor:.6f} MPa/kN\n")
        f.write(f"  S-N Constants:        a = {a_constant:.1e}, m = {m_exponent:.1f}\n\n")
        
        f.write("DETAILED RESULTS:\n")
        f.write(results_df.to_string(index=False))
        f.write("\n\n")
        
        f.write("VALIDATION SUMMARY:\n")
        f.write(f"  Combined Damage:      {total_damage:.4e} per year\n")
        f.write(f"  Combined Life:        {1.0/total_damage:.2e} years\n")
        f.write(f"  Damage Ratio (L/S):   {damage_ratio:.0e}\n")
    
    print(f"Saved detailed report: {report_file.name}")
    print()
    
    print("="*80)
    print("DETAILED CALCULATION EXAMPLE COMPLETE")
    print("="*80)
    print()
    print("FILES CREATED:")
    print(f"  • {calculation_file}")
    print(f"  • {report_file}")
    print()
    print("KEY TAKEAWAYS:")
    print("  1. Rainflow counting extracts cycle ranges from time series")
    print("  2. Yearly normalization scales sample data to annual basis")
    print("  3. Stress conversion applies SCF and cross-sectional effects")
    print("  4. Large tension ranges dominate fatigue damage")
    print("  5. S-N curve application quantifies damage accumulation")
    print()
    
    return True

def main():
    """Main execution"""
    try:
        success = detailed_calculation_fc001()
        
        if success:
            print("[SUCCESS] Detailed calculation example completed successfully")
            return 0
        else:
            print("[ERROR] Detailed calculation example failed")
            return 1
            
    except Exception as e:
        print(f"[ERROR] Unexpected error: {e}")
        import traceback
        traceback.print_exc()
        return 1

if __name__ == "__main__":
    sys.exit(main())