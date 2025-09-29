#!/usr/bin/env python
"""
Simplified Rainflow Algorithm Comparison Test
Compares custom implementation vs library implementation using pure sine waves
No matplotlib display - only saves results
"""

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')  # Use non-interactive backend
import matplotlib.pyplot as plt
from typing import List, Tuple
import sys
import os

# Add parent directory to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

def custom_rainflow(series):
    """
    Custom rainflow implementation from Rainflow-User.py
    Returns list of cycles as tuples: (range, mean, count, high, low)
    count is 1.0 for full cycles and 0.5 for half-cycles.
    """
    vals = [float(x) for x in series if pd.notna(x)]
    if len(vals) < 2:
        return []

    # Remove consecutive duplicates
    comp = [vals[0]]
    for v in vals[1:]:
        if v != comp[-1]:
            comp.append(v)
    if len(comp) < 2:
        return []

    # Build turning points
    tp = [comp[0]]
    for i in range(1, len(comp)-1):
        if (comp[i] > comp[i-1] and comp[i] >= comp[i+1]) or (comp[i] < comp[i-1] and comp[i] <= comp[i+1]):
            tp.append(comp[i])
    tp.append(comp[-1])

    stack, cycles = [], []
    for x in tp:
        stack.append(x)
        while len(stack) >= 3:
            s0, s1, s2 = stack[-3], stack[-2], stack[-1]
            R1 = abs(s1 - s0)
            R2 = abs(s2 - s1)
            if R1 >= R2:
                rng = R2
                mean = (s2 + s1) / 2.0
                mx, mn = max(s2, s1), min(s2, s1)
                cycles.append((rng, mean, 1.0, mx, mn))
                del stack[-2]
            else:
                break

    # Half cycles from remaining stack
    for i in range(len(stack) - 1):
        a, b = stack[i], stack[i+1]
        rng = abs(b - a)
        mean = (b + a) / 2.0
        mx, mn = max(a, b), min(a, b)
        cycles.append((rng, mean, 0.5, mx, mn))

    return cycles


def library_rainflow(series):
    """
    Library-based rainflow implementation
    Returns list of cycles compatible with custom format
    """
    try:
        import rainflow
        # Try extract_cycles first (gives more detail)
        try:
            cycles_raw = list(rainflow.extract_cycles(series))
            # extract_cycles returns: (range, mean, count, i_start, i_end)
            cycles = []
            for rng, mean, count, i_start, i_end in cycles_raw:
                # Calculate high and low from range and mean
                high = mean + abs(rng)/2
                low = mean - abs(rng)/2
                cycles.append((abs(rng), mean, count, high, low))
            return cycles
        except:
            # Fallback to count_cycles
            cycles_raw = list(rainflow.count_cycles(series))
            cycles = []
            for rng, count in cycles_raw:
                # No mean information available, estimate from range
                mean = 0  # Assume zero mean for comparison
                high = abs(rng)/2
                low = -abs(rng)/2
                cycles.append((abs(rng), mean, count, high, low))
            return cycles
    except ImportError:
        print("Warning: rainflow library not installed")
        return []


def main():
    """Main execution function"""
    print("="*60)
    print("RAINFLOW ALGORITHM COMPARISON TEST")
    print("Pure Sine Wave Analysis")
    print("="*60)
    
    # Create output directory
    output_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'rainflow_comparison_results')
    os.makedirs(output_dir, exist_ok=True)
    
    # Test 1: Pure sine wave (10 complete cycles)
    print("\nTest 1: Pure Sine Wave (10 cycles, amplitude ±10)")
    print("-"*50)
    
    t = np.linspace(0, 10, 1000)  # 10 seconds, 1000 points
    pure_sine = 10 * np.sin(2 * np.pi * t)  # 1 Hz, amplitude ±10
    
    # Run both algorithms
    custom_cycles = custom_rainflow(pure_sine)
    library_cycles = library_rainflow(pure_sine)
    
    # Analyze results
    print(f"\nCustom Algorithm:")
    print(f"  Number of cycle entries: {len(custom_cycles)}")
    print(f"  Total cycles (weighted): {sum(c[2] for c in custom_cycles):.1f}")
    print(f"  Full cycles (count=1.0): {sum(1 for c in custom_cycles if c[2] == 1.0)}")
    print(f"  Half cycles (count=0.5): {sum(1 for c in custom_cycles if c[2] == 0.5)}")
    if custom_cycles:
        ranges = [c[0] for c in custom_cycles]
        print(f"  Range values: min={min(ranges):.2f}, max={max(ranges):.2f}, mean={np.mean(ranges):.2f}")
    
    print(f"\nLibrary Algorithm:")
    print(f"  Number of cycle entries: {len(library_cycles)}")
    print(f"  Total cycles (weighted): {sum(c[2] for c in library_cycles):.1f}")
    print(f"  Full cycles (count=1.0): {sum(1 for c in library_cycles if c[2] == 1.0)}")
    print(f"  Half cycles (count=0.5): {sum(1 for c in library_cycles if c[2] == 0.5)}")
    if library_cycles:
        ranges = [c[0] for c in library_cycles]
        print(f"  Range values: min={min(ranges):.2f}, max={max(ranges):.2f}, mean={np.mean(ranges):.2f}")
    
    # Test 2: Sine wave with varying amplitude
    print("\n\nTest 2: Variable Amplitude Sine Wave")
    print("-"*50)
    
    # Create envelope-modulated sine
    envelope = 5 + 4 * np.sin(2 * np.pi * 0.1 * t)  # Slow variation
    variable_sine = envelope * np.sin(2 * np.pi * 2 * t)  # 2 Hz carrier
    
    custom_cycles2 = custom_rainflow(variable_sine)
    library_cycles2 = library_rainflow(variable_sine)
    
    print(f"\nCustom Algorithm:")
    print(f"  Total cycles (weighted): {sum(c[2] for c in custom_cycles2):.1f}")
    print(f"  Number of unique ranges: {len(set(round(c[0], 2) for c in custom_cycles2))}")
    
    print(f"\nLibrary Algorithm:")
    print(f"  Total cycles (weighted): {sum(c[2] for c in library_cycles2):.1f}")
    print(f"  Number of unique ranges: {len(set(round(c[0], 2) for c in library_cycles2))}")
    
    # Create comparison plot
    fig, axes = plt.subplots(2, 3, figsize=(15, 10))
    
    # Row 1: Pure sine wave
    axes[0, 0].plot(t[:200], pure_sine[:200], 'b-', linewidth=1)
    axes[0, 0].set_title('Pure Sine Wave (first 2 seconds)')
    axes[0, 0].set_xlabel('Time (s)')
    axes[0, 0].set_ylabel('Amplitude')
    axes[0, 0].grid(True, alpha=0.3)
    
    # Custom algorithm results for pure sine
    if custom_cycles:
        ranges = [c[0] for c in custom_cycles]
        means = [c[1] for c in custom_cycles]
        counts = [c[2] for c in custom_cycles]
        axes[0, 1].scatter(ranges, means, s=[100*c for c in counts], alpha=0.6, c='blue')
        axes[0, 1].set_title(f'Custom Algorithm\nTotal: {sum(counts):.1f} cycles')
        axes[0, 1].set_xlabel('Range')
        axes[0, 1].set_ylabel('Mean')
        axes[0, 1].grid(True, alpha=0.3)
    
    # Library algorithm results for pure sine
    if library_cycles:
        ranges = [c[0] for c in library_cycles]
        means = [c[1] for c in library_cycles]
        counts = [c[2] for c in library_cycles]
        axes[0, 2].scatter(ranges, means, s=[100*c for c in counts], alpha=0.6, c='green')
        axes[0, 2].set_title(f'Library Algorithm\nTotal: {sum(counts):.1f} cycles')
        axes[0, 2].set_xlabel('Range')
        axes[0, 2].set_ylabel('Mean')
        axes[0, 2].grid(True, alpha=0.3)
    
    # Row 2: Variable amplitude sine
    axes[1, 0].plot(t[:200], variable_sine[:200], 'r-', linewidth=1)
    axes[1, 0].set_title('Variable Amplitude Sine (first 2 seconds)')
    axes[1, 0].set_xlabel('Time (s)')
    axes[1, 0].set_ylabel('Amplitude')
    axes[1, 0].grid(True, alpha=0.3)
    
    # Custom algorithm results for variable sine
    if custom_cycles2:
        ranges = [c[0] for c in custom_cycles2]
        means = [c[1] for c in custom_cycles2]
        counts = [c[2] for c in custom_cycles2]
        axes[1, 1].scatter(ranges, means, s=[20*c for c in counts], alpha=0.6, c='blue')
        axes[1, 1].set_title(f'Custom Algorithm\nTotal: {sum(counts):.1f} cycles')
        axes[1, 1].set_xlabel('Range')
        axes[1, 1].set_ylabel('Mean')
        axes[1, 1].grid(True, alpha=0.3)
    
    # Library algorithm results for variable sine
    if library_cycles2:
        ranges = [c[0] for c in library_cycles2]
        means = [c[1] for c in library_cycles2]
        counts = [c[2] for c in library_cycles2]
        axes[1, 2].scatter(ranges, means, s=[20*c for c in counts], alpha=0.6, c='green')
        axes[1, 2].set_title(f'Library Algorithm\nTotal: {sum(counts):.1f} cycles')
        axes[1, 2].set_xlabel('Range')
        axes[1, 2].set_ylabel('Mean')
        axes[1, 2].grid(True, alpha=0.3)
    
    plt.suptitle('Rainflow Algorithm Comparison - Custom vs Library', fontsize=14, fontweight='bold')
    plt.tight_layout()
    
    # Save plot
    plot_path = os.path.join(output_dir, 'rainflow_comparison_plot.png')
    plt.savefig(plot_path, dpi=150, bbox_inches='tight')
    print(f"\n\nPlot saved to: {plot_path}")
    
    # Create detailed comparison CSV
    comparison_data = []
    
    # Pure sine results
    comparison_data.append({
        'Signal': 'Pure Sine (10 cycles)',
        'Algorithm': 'Custom',
        'Total_Cycles': sum(c[2] for c in custom_cycles) if custom_cycles else 0,
        'Full_Cycles': sum(1 for c in custom_cycles if c[2] == 1.0) if custom_cycles else 0,
        'Half_Cycles': sum(1 for c in custom_cycles if c[2] == 0.5) if custom_cycles else 0,
        'Expected_Range': 20.0,
        'Actual_Range': max(c[0] for c in custom_cycles) if custom_cycles else 0
    })
    
    comparison_data.append({
        'Signal': 'Pure Sine (10 cycles)',
        'Algorithm': 'Library',
        'Total_Cycles': sum(c[2] for c in library_cycles) if library_cycles else 0,
        'Full_Cycles': sum(1 for c in library_cycles if c[2] == 1.0) if library_cycles else 0,
        'Half_Cycles': sum(1 for c in library_cycles if c[2] == 0.5) if library_cycles else 0,
        'Expected_Range': 20.0,
        'Actual_Range': max(c[0] for c in library_cycles) if library_cycles else 0
    })
    
    # Variable sine results
    comparison_data.append({
        'Signal': 'Variable Amplitude Sine',
        'Algorithm': 'Custom',
        'Total_Cycles': sum(c[2] for c in custom_cycles2) if custom_cycles2 else 0,
        'Full_Cycles': sum(1 for c in custom_cycles2 if c[2] == 1.0) if custom_cycles2 else 0,
        'Half_Cycles': sum(1 for c in custom_cycles2 if c[2] == 0.5) if custom_cycles2 else 0,
        'Expected_Range': 'Variable',
        'Actual_Range': f"{min(c[0] for c in custom_cycles2):.1f}-{max(c[0] for c in custom_cycles2):.1f}" if custom_cycles2 else 0
    })
    
    comparison_data.append({
        'Signal': 'Variable Amplitude Sine',
        'Algorithm': 'Library',
        'Total_Cycles': sum(c[2] for c in library_cycles2) if library_cycles2 else 0,
        'Full_Cycles': sum(1 for c in library_cycles2 if c[2] == 1.0) if library_cycles2 else 0,
        'Half_Cycles': sum(1 for c in library_cycles2 if c[2] == 0.5) if library_cycles2 else 0,
        'Expected_Range': 'Variable',
        'Actual_Range': f"{min(c[0] for c in library_cycles2):.1f}-{max(c[0] for c in library_cycles2):.1f}" if library_cycles2 else 0
    })
    
    # Save to CSV
    df = pd.DataFrame(comparison_data)
    csv_path = os.path.join(output_dir, 'rainflow_comparison_results.csv')
    df.to_csv(csv_path, index=False)
    print(f"Results saved to: {csv_path}")
    
    print("\n" + "="*60)
    print("COMPARISON SUMMARY")
    print("="*60)
    print(df.to_string(index=False))
    
    # Calculate discrepancy
    if custom_cycles and library_cycles:
        custom_total = sum(c[2] for c in custom_cycles)
        library_total = sum(c[2] for c in library_cycles)
        discrepancy = abs(custom_total - library_total)
        print(f"\n\nPure Sine Discrepancy: {discrepancy:.1f} cycles ({discrepancy/10*100:.1f}% of expected)")
        
        if discrepancy < 0.1:
            print("Result: EXCELLENT MATCH - Algorithms produce identical results for pure sine wave")
        elif discrepancy < 1:
            print("Result: GOOD MATCH - Minor differences likely due to implementation details")
        else:
            print("Result: SIGNIFICANT DIFFERENCE - Algorithms differ in cycle counting approach")


if __name__ == "__main__":
    main()