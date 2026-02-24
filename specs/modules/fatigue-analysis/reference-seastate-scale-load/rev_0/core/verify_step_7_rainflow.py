#!/usr/bin/env python
"""
Step 7: Rainflow Counting Verification
Converts tension time series into stress cycles for fatigue analysis
"""

import sys
import numpy as np
import pandas as pd
from pathlib import Path
import rainflow

def verify_step_7():
    """Verify Step 7: Rainflow Counting"""
    
    print("="*60)
    print("STEP 7: RAINFLOW COUNTING VERIFICATION")
    print("="*60)
    
    # Load Step 6 output
    step6_output = Path("output/verification/step_by_step/test_FC001_Strut1.csv")
    
    if not step6_output.exists():
        print(f"\n[ERROR] Step 6 output not found: {step6_output}")
        print("        Please run Step 6 first")
        return False
    
    print(f"\n[INFO] Loading Step 6 output: {step6_output.name}")
    df = pd.read_csv(step6_output)
    
    # Get tension time series
    tension_data = df['effective_tension_kN'].values
    
    print(f"\n[DATA SUMMARY]")
    print(f"  Total points: {len(tension_data)}")
    print(f"  Duration: {len(tension_data) * 0.1:.1f} seconds")
    print(f"  Min tension: {tension_data.min():.2f} kN")
    print(f"  Max tension: {tension_data.max():.2f} kN")
    print(f"  Mean tension: {tension_data.mean():.2f} kN")
    print(f"  Std deviation: {tension_data.std():.2f} kN")
    
    # Apply rainflow counting
    print(f"\n[RAINFLOW COUNTING]")
    print("-"*40)
    
    # Count cycles using rainflow package
    cycles = list(rainflow.count_cycles(tension_data))
    
    print(f"Total cycles detected: {len(cycles)}")
    
    # Separate into ranges and counts
    ranges = []
    counts = []
    means = []
    
    # The rainflow package returns (range, count) tuples
    for cycle in cycles:
        if len(cycle) == 2:
            rng, count = cycle
            ranges.append(rng)
            counts.append(count)
            means.append(0)  # Mean not provided in this format
        elif len(cycle) == 3:
            rng, mean, count = cycle
            ranges.append(rng)
            means.append(mean)
            counts.append(count)
        else:
            # Handle other formats
            ranges.append(cycle[0])
            counts.append(1 if len(cycle) == 1 else cycle[-1])
            means.append(cycle[1] if len(cycle) > 2 else 0)
    
    # Convert to numpy arrays
    ranges = np.array(ranges)
    counts = np.array(counts)
    means = np.array(means)
    
    # Statistics
    print(f"\n[CYCLE STATISTICS]")
    print(f"  Number of unique cycles: {len(ranges)}")
    print(f"  Total cycle count: {counts.sum():.1f}")
    print(f"  Min range: {ranges.min():.2f} kN")
    print(f"  Max range: {ranges.max():.2f} kN")
    print(f"  Mean range: {ranges.mean():.2f} kN")
    
    # Bin the cycles by range
    print(f"\n[CYCLE DISTRIBUTION BY RANGE]")
    bins = [0, 10, 20, 30, 40, 50, 100, 200]
    bin_counts = np.zeros(len(bins)-1)
    
    for i in range(len(bins)-1):
        mask = (ranges >= bins[i]) & (ranges < bins[i+1])
        bin_counts[i] = counts[mask].sum()
        if bin_counts[i] > 0:
            print(f"  {bins[i]:3d}-{bins[i+1]:3d} kN: {bin_counts[i]:6.1f} cycles")
    
    # Show top 10 largest cycles
    print(f"\n[TOP 10 LARGEST CYCLES]")
    sorted_idx = np.argsort(ranges)[::-1][:10]
    
    for i, idx in enumerate(sorted_idx, 1):
        print(f"  {i:2d}. Range: {ranges[idx]:6.2f} kN, Count: {counts[idx]:.2f}, Mean: {means[idx]:6.2f} kN")
    
    # Damage potential preview (simplified)
    print(f"\n[DAMAGE POTENTIAL PREVIEW]")
    print("  (Using simplified S-N curve: N = 1e12 * S^(-3))")
    
    total_damage = 0
    for rng, cnt in zip(ranges[:5], counts[:5]):
        # Convert to stress (simplified: assume 1 kN = 0.25 MPa)
        stress = rng * 0.25
        # Calculate cycles to failure (simplified S-N)
        N_failure = 1e12 * (stress ** -3) if stress > 0 else 1e20
        # Calculate damage
        damage = cnt / N_failure
        total_damage += damage
        print(f"  Range {rng:.1f} kN -> Stress {stress:.1f} MPa -> N={N_failure:.1e} -> Damage={damage:.2e}")
    
    print(f"\n  Sample damage (first 5 cycles): {total_damage:.2e}")
    
    # NORMALIZATION TO YEARLY CYCLES
    print(f"\n[YEARLY NORMALIZATION]")
    print("-"*40)
    
    # Get sample duration and fatigue condition info
    sample_duration_s = len(tension_data) * 0.1  # 0.1s per data point
    seconds_per_year = 365.25 * 24 * 3600
    
    # FC001 occurrence (from fatigue_conditions.csv)
    fc001_occurrence = 0.20  # 20% occurrence
    
    # Calculate normalization factor
    normalization_factor = (seconds_per_year / sample_duration_s) * fc001_occurrence
    
    print(f"Sample duration: {sample_duration_s:.1f} seconds")
    print(f"Yearly duration: {seconds_per_year:.2e} seconds")  
    print(f"FC001 occurrence: {fc001_occurrence*100:.1f}%")
    print(f"Normalization factor: {normalization_factor:.2e}")
    
    # Normalize cycle counts to yearly basis
    yearly_counts = counts * normalization_factor
    
    print(f"\n[NORMALIZED RESULTS]")
    print(f"  Sample cycles: {counts.sum():.1f}")
    print(f"  Yearly cycles (normalized): {yearly_counts.sum():.2e}")
    print(f"  Cycles per year for FC001: {yearly_counts.sum():.2e}")
    
    # Show example normalizations
    print(f"\n[EXAMPLE NORMALIZATIONS]")
    for i in range(min(3, len(ranges))):
        print(f"  Range {ranges[i]:.1f} kN: {counts[i]:.1f} cycles/sample -> {yearly_counts[i]:.2e} cycles/year")
    
    # Save rainflow results with yearly normalization
    output_file = Path("output/verification/step_by_step/rainflow_FC001_Strut1.csv")
    output_file.parent.mkdir(parents=True, exist_ok=True)
    
    rainflow_df = pd.DataFrame({
        'range_kN': ranges,
        'mean_kN': means,
        'cycle_count': counts,
        'yearly_cycles': yearly_counts
    })
    rainflow_df.to_csv(output_file, index=False)
    
    print(f"\n[OUTPUT SAVED]")
    print(f"  File: {output_file}")
    print(f"  Rows: {len(rainflow_df)}")
    
    print(f"\n[VERIFICATION SUMMARY]")
    print("[PASS] Step 6 output loaded successfully")
    print("[PASS] Rainflow counting completed")
    print("[PASS] Cycle statistics calculated")
    print("[PASS] Damage preview calculated")
    print("[PASS] Results saved to CSV")
    
    return True

def main():
    """Main execution"""
    success = verify_step_7()
    
    if success:
        print("\n" + "="*60)
        print("STEP 7 VERIFICATION COMPLETE")
        print("="*60)
        print("\nAWAITING USER CONFIRMATION...")
        print("\nPlease review the rainflow counting results above.")
        print("Key items to verify:")
        print("  1. Cycle counts are reasonable for 100s of data")
        print("  2. Range distribution follows expected pattern")
        print("  3. Largest cycles correspond to major load variations")
        print("\nConfirm to proceed to Step 8 (Stress Conversion)")
    else:
        print("\n[ERROR] Step 7 verification failed")
        return 1
    
    return 0

if __name__ == "__main__":
    sys.exit(main())