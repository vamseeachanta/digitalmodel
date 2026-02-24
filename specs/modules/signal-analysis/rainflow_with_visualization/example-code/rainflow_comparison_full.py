#!/usr/bin/env python
"""
Rainflow Algorithm Comparison Test
Compares custom implementation vs library implementation using pure sine waves
"""

import numpy as np
import pandas as pd
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
        print("Warning: rainflow library not installed. Install with: pip install rainflow")
        return []


def generate_test_signals():
    """Generate various test signals for comparison"""
    # Time array
    dt = 0.01
    duration = 10.0
    t = np.arange(0, duration, dt)
    
    test_signals = {}
    
    # 1. Pure sine wave (single frequency, single amplitude)
    test_signals['pure_sine'] = {
        'time': t,
        'signal': 10 * np.sin(2 * np.pi * 1.0 * t),  # 1 Hz, amplitude 10
        'description': 'Pure sine: 1 Hz, amplitude ±10'
    }
    
    # 2. Sine wave with offset (to test mean handling)
    test_signals['sine_offset'] = {
        'time': t,
        'signal': 10 * np.sin(2 * np.pi * 1.0 * t) + 5,  # 1 Hz, amplitude 10, offset +5
        'description': 'Sine with offset: 1 Hz, amplitude ±10, mean=5'
    }
    
    # 3. Two-frequency sine wave
    test_signals['dual_sine'] = {
        'time': t,
        'signal': 10 * np.sin(2 * np.pi * 1.0 * t) + 5 * np.sin(2 * np.pi * 3.0 * t),
        'description': 'Dual sine: 1 Hz (amp=10) + 3 Hz (amp=5)'
    }
    
    # 4. Modulated sine wave
    test_signals['modulated_sine'] = {
        'time': t,
        'signal': (5 + 3*np.sin(2*np.pi*0.2*t)) * np.sin(2*np.pi*2.0*t),
        'description': 'Amplitude modulated: carrier 2 Hz, modulation 0.2 Hz'
    }
    
    # 5. Simple repeating pattern (sawtooth-like)
    pattern = [0, 5, 0, 10, 0, 15, 0, 10, 0, 5]
    repeats = int(len(t) / len(pattern))
    extended_pattern = pattern * (repeats + 1)
    test_signals['pattern'] = {
        'time': t,
        'signal': np.array(extended_pattern[:len(t)]),
        'description': 'Repeating pattern: 0-5-0-10-0-15-0-10-0-5'
    }
    
    return test_signals


def compare_algorithms(signal_data, signal_name):
    """Compare the two rainflow algorithms on given signal"""
    signal = signal_data['signal']
    
    # Run both algorithms
    custom_cycles = custom_rainflow(signal)
    library_cycles = library_rainflow(signal)
    
    # Summarize results
    custom_summary = summarize_cycles(custom_cycles, 'Custom')
    library_summary = summarize_cycles(library_cycles, 'Library')
    
    # Print comparison
    print(f"\n{'='*60}")
    print(f"Signal: {signal_data['description']}")
    print(f"{'='*60}")
    print(f"\nCustom Algorithm Results:")
    print(f"  Total cycles: {custom_summary['total_count']:.1f}")
    print(f"  Full cycles: {custom_summary['full_cycles']}")
    print(f"  Half cycles: {custom_summary['half_cycles']}")
    print(f"  Unique ranges: {custom_summary['unique_ranges']}")
    print(f"  Max range: {custom_summary['max_range']:.2f}")
    print(f"  Mean range: {custom_summary['mean_range']:.2f}")
    
    if library_cycles:
        print(f"\nLibrary Algorithm Results:")
        print(f"  Total cycles: {library_summary['total_count']:.1f}")
        print(f"  Full cycles: {library_summary['full_cycles']}")
        print(f"  Half cycles: {library_summary['half_cycles']}")
        print(f"  Unique ranges: {library_summary['unique_ranges']}")
        print(f"  Max range: {library_summary['max_range']:.2f}")
        print(f"  Mean range: {library_summary['mean_range']:.2f}")
        
        # Calculate discrepancy
        discrepancy = abs(custom_summary['total_count'] - library_summary['total_count'])
        percent_diff = (discrepancy / max(custom_summary['total_count'], library_summary['total_count'])) * 100
        print(f"\nDiscrepancy:")
        print(f"  Cycle count difference: {discrepancy:.1f} ({percent_diff:.1f}%)")
    
    # Create detailed comparison DataFrame
    comparison_df = create_comparison_table(custom_cycles, library_cycles)
    
    return {
        'custom_cycles': custom_cycles,
        'library_cycles': library_cycles,
        'custom_summary': custom_summary,
        'library_summary': library_summary,
        'comparison_df': comparison_df
    }


def summarize_cycles(cycles, label=''):
    """Summarize cycle counting results"""
    if not cycles:
        return {
            'total_count': 0,
            'full_cycles': 0,
            'half_cycles': 0,
            'unique_ranges': 0,
            'max_range': 0,
            'mean_range': 0
        }
    
    counts = np.array([c[2] for c in cycles])
    ranges = np.array([c[0] for c in cycles])
    
    full_cycles = sum(1 for c in cycles if c[2] == 1.0)
    half_cycles = sum(1 for c in cycles if c[2] == 0.5)
    
    return {
        'total_count': np.sum(counts),
        'full_cycles': full_cycles,
        'half_cycles': half_cycles,
        'unique_ranges': len(set(np.round(ranges, 4))),
        'max_range': np.max(ranges) if len(ranges) > 0 else 0,
        'mean_range': np.average(ranges, weights=counts) if len(ranges) > 0 else 0
    }


def create_comparison_table(custom_cycles, library_cycles):
    """Create detailed comparison table of cycles"""
    # Bin cycles for comparison
    bin_size = 0.5
    max_range = max(
        max([c[0] for c in custom_cycles]) if custom_cycles else 0,
        max([c[0] for c in library_cycles]) if library_cycles else 0
    )
    
    if max_range == 0:
        return pd.DataFrame()
    
    bins = np.arange(0, max_range + bin_size, bin_size)
    
    # Bin custom cycles
    custom_binned = {}
    for rng, mean, count, high, low in custom_cycles:
        bin_idx = int(rng / bin_size)
        if bin_idx not in custom_binned:
            custom_binned[bin_idx] = {'count': 0, 'mean_sum': 0}
        custom_binned[bin_idx]['count'] += count
        custom_binned[bin_idx]['mean_sum'] += mean * count
    
    # Bin library cycles
    library_binned = {}
    for rng, mean, count, high, low in library_cycles:
        bin_idx = int(rng / bin_size)
        if bin_idx not in library_binned:
            library_binned[bin_idx] = {'count': 0, 'mean_sum': 0}
        library_binned[bin_idx]['count'] += count
        library_binned[bin_idx]['mean_sum'] += mean * count
    
    # Create comparison DataFrame
    rows = []
    all_bins = sorted(set(custom_binned.keys()) | set(library_binned.keys()))
    
    for bin_idx in all_bins:
        range_start = bin_idx * bin_size
        range_end = (bin_idx + 1) * bin_size
        
        custom_count = custom_binned.get(bin_idx, {}).get('count', 0)
        library_count = library_binned.get(bin_idx, {}).get('count', 0)
        
        rows.append({
            'Range_Bin': f"{range_start:.1f}-{range_end:.1f}",
            'Custom_Count': custom_count,
            'Library_Count': library_count,
            'Difference': custom_count - library_count,
            'Match': 'Yes' if abs(custom_count - library_count) < 0.1 else 'No'
        })
    
    return pd.DataFrame(rows)


def visualize_comparison(signal_data, results, signal_name):
    """Create visualization comparing the two methods"""
    fig, axes = plt.subplots(3, 2, figsize=(14, 12))
    
    # Plot 1: Time series
    ax = axes[0, 0]
    ax.plot(signal_data['time'], signal_data['signal'], 'b-', linewidth=0.8)
    ax.set_xlabel('Time (s)')
    ax.set_ylabel('Amplitude')
    ax.set_title(f"Test Signal: {signal_data['description']}")
    ax.grid(True, alpha=0.3)
    
    # Plot 2: Cycle count comparison bar chart
    ax = axes[0, 1]
    if results['library_cycles']:
        labels = ['Custom', 'Library']
        total_counts = [results['custom_summary']['total_count'], 
                       results['library_summary']['total_count']]
        full_counts = [results['custom_summary']['full_cycles'],
                      results['library_summary']['full_cycles']]
        half_counts = [results['custom_summary']['half_cycles'],
                      results['library_summary']['half_cycles']]
        
        x = np.arange(len(labels))
        width = 0.25
        
        ax.bar(x - width, full_counts, width, label='Full cycles', color='green', alpha=0.7)
        ax.bar(x, half_counts, width, label='Half cycles', color='orange', alpha=0.7)
        ax.bar(x + width, total_counts, width, label='Total (weighted)', color='blue', alpha=0.7)
        
        ax.set_xlabel('Algorithm')
        ax.set_ylabel('Cycle Count')
        ax.set_title('Cycle Count Comparison')
        ax.set_xticks(x)
        ax.set_xticklabels(labels)
        ax.legend()
        ax.grid(True, alpha=0.3, axis='y')
    
    # Plot 3: Range histogram - Custom
    ax = axes[1, 0]
    if results['custom_cycles']:
        ranges = [c[0] for c in results['custom_cycles']]
        counts = [c[2] for c in results['custom_cycles']]
        ax.hist(ranges, bins=20, weights=counts, alpha=0.7, color='blue', edgecolor='black')
        ax.set_xlabel('Stress Range')
        ax.set_ylabel('Weighted Count')
        ax.set_title('Custom Algorithm - Range Distribution')
        ax.grid(True, alpha=0.3)
    
    # Plot 4: Range histogram - Library
    ax = axes[1, 1]
    if results['library_cycles']:
        ranges = [c[0] for c in results['library_cycles']]
        counts = [c[2] for c in results['library_cycles']]
        ax.hist(ranges, bins=20, weights=counts, alpha=0.7, color='green', edgecolor='black')
        ax.set_xlabel('Stress Range')
        ax.set_ylabel('Weighted Count')
        ax.set_title('Library Algorithm - Range Distribution')
        ax.grid(True, alpha=0.3)
    
    # Plot 5: Mean vs Range scatter - Custom
    ax = axes[2, 0]
    if results['custom_cycles']:
        ranges = [c[0] for c in results['custom_cycles']]
        means = [c[1] for c in results['custom_cycles']]
        counts = [c[2] for c in results['custom_cycles']]
        sizes = [50 * c for c in counts]  # Size proportional to count
        scatter = ax.scatter(ranges, means, s=sizes, alpha=0.6, c=counts, cmap='Blues')
        ax.set_xlabel('Stress Range')
        ax.set_ylabel('Mean Stress')
        ax.set_title('Custom - Rainflow Matrix')
        ax.grid(True, alpha=0.3)
        plt.colorbar(scatter, ax=ax, label='Count')
    
    # Plot 6: Mean vs Range scatter - Library
    ax = axes[2, 1]
    if results['library_cycles']:
        ranges = [c[0] for c in results['library_cycles']]
        means = [c[1] for c in results['library_cycles']]
        counts = [c[2] for c in results['library_cycles']]
        sizes = [50 * c for c in counts]
        scatter = ax.scatter(ranges, means, s=sizes, alpha=0.6, c=counts, cmap='Greens')
        ax.set_xlabel('Stress Range')
        ax.set_ylabel('Mean Stress')
        ax.set_title('Library - Rainflow Matrix')
        ax.grid(True, alpha=0.3)
        plt.colorbar(scatter, ax=ax, label='Count')
    
    plt.suptitle(f'Rainflow Algorithm Comparison - {signal_name}', fontsize=14, fontweight='bold')
    plt.tight_layout()
    
    # Save figure
    output_dir = os.path.dirname(os.path.abspath(__file__))
    output_path = os.path.join(output_dir, f'rainflow_comparison_{signal_name}.png')
    plt.savefig(output_path, dpi=150, bbox_inches='tight')
    print(f"\nVisualization saved to: {output_path}")
    
    plt.show()


def main():
    """Main execution function"""
    print("="*60)
    print("RAINFLOW ALGORITHM COMPARISON TEST")
    print("Comparing Custom vs Library Implementation")
    print("="*60)
    
    # Check if rainflow library is available
    try:
        import rainflow
        print("\n[OK] Rainflow library is installed")
        print(f"  Version: {rainflow.__version__ if hasattr(rainflow, '__version__') else 'Unknown'}")
    except ImportError:
        print("\n[X] Rainflow library not installed")
        print("  Install with: pip install rainflow")
        print("  Only custom algorithm will be tested.")
    
    # Generate test signals
    test_signals = generate_test_signals()
    
    # Store all results
    all_results = {}
    
    # Test each signal
    for signal_name, signal_data in test_signals.items():
        results = compare_algorithms(signal_data, signal_name)
        all_results[signal_name] = results
        
        # Show detailed comparison for pure sine
        if signal_name == 'pure_sine' and results['comparison_df'] is not None and not results['comparison_df'].empty:
            print(f"\nDetailed Bin Comparison for {signal_name}:")
            print(results['comparison_df'].to_string())
        
        # Visualize comparison
        visualize_comparison(signal_data, results, signal_name)
    
    # Summary table
    print("\n" + "="*60)
    print("OVERALL COMPARISON SUMMARY")
    print("="*60)
    
    summary_data = []
    for signal_name, results in all_results.items():
        row = {
            'Signal': signal_name,
            'Custom_Cycles': f"{results['custom_summary']['total_count']:.1f}",
            'Library_Cycles': f"{results['library_summary']['total_count']:.1f}" if results['library_cycles'] else 'N/A',
            'Difference': f"{abs(results['custom_summary']['total_count'] - results['library_summary']['total_count']):.1f}" if results['library_cycles'] else 'N/A',
            'Match': 'Yes' if results['library_cycles'] and abs(results['custom_summary']['total_count'] - results['library_summary']['total_count']) < 0.1 else 'No'
        }
        summary_data.append(row)
    
    summary_df = pd.DataFrame(summary_data)
    print(summary_df.to_string(index=False))
    
    # Save detailed results
    output_dir = os.path.dirname(os.path.abspath(__file__))
    results_path = os.path.join(output_dir, 'rainflow_comparison_results.csv')
    summary_df.to_csv(results_path, index=False)
    print(f"\nResults saved to: {results_path}")


if __name__ == "__main__":
    main()