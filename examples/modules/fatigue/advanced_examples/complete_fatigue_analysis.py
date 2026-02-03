#!/usr/bin/env python3
"""
Complete Fatigue Analysis Example
================================

This example demonstrates a comprehensive fatigue analysis workflow using
the Digital Model fatigue module, covering both time-domain and frequency-domain
methods.

Features demonstrated:
- S-N curve selection and comparison
- Rainflow cycle counting
- Multiple damage accumulation methods
- Mean stress corrections
- Frequency domain analysis
- Comparative analysis and reporting

Author: Digital Model Team
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from pathlib import Path
import sys

# Add the digitalmodel package to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / 'src'))

from digitalmodel.structural.fatigue import (
    # S-N curves
    get_dnv_curve, get_api_curve, get_bs_curve,
    StandardSNCurves, plot_sn_curve,

    # Damage accumulation
    LinearDamageAccumulation, ModifiedMinersRule,
    NonlinearDamageAccumulation, compare_damage_methods,

    # Rainflow counting
    rainflow_count, rainflow_with_means,
    RainflowCounter, RainflowParameters,

    # Frequency domain
    DirlikMethod, TovoBenasciuttiMethod,
    compare_frequency_methods, ResponsePSDCalculator,

    # Utilities
    quick_fatigue_analysis, create_fatigue_report
)


def generate_synthetic_stress_history(duration=3600, dt=0.1, seed=42):
    """
    Generate synthetic stress time history for demonstration

    Parameters
    ----------
    duration : float, default=3600
        Duration in seconds (1 hour)
    dt : float, default=0.1
        Time step in seconds
    seed : int, default=42
        Random seed for reproducibility

    Returns
    -------
    time : np.ndarray
        Time array
    stress : np.ndarray
        Stress time history
    """
    np.random.seed(seed)

    time = np.arange(0, duration, dt)
    n_points = len(time)

    # Base stress level
    stress_mean = 50.0  # MPa

    # Deterministic components
    # Low frequency variation (environmental loading)
    stress_env = 30 * np.sin(2 * np.pi * 0.01 * time)

    # Medium frequency (wave loading)
    stress_wave = 25 * np.sin(2 * np.pi * 0.1 * time + np.pi/4)

    # High frequency (structural response)
    stress_struct = 15 * np.sin(2 * np.pi * 1.5 * time + np.pi/3)

    # Random component (turbulence, measurement noise)
    stress_random = 10 * np.random.normal(0, 1, n_points)

    # Apply low-pass filter to random component
    from scipy import signal
    b, a = signal.butter(4, 0.2, 'low')
    stress_random = signal.filtfilt(b, a, stress_random)

    # Combine components
    stress = stress_mean + stress_env + stress_wave + stress_struct + stress_random

    # Add some load events (storms, heavy operations)
    n_events = 5
    event_times = np.random.uniform(0, duration, n_events)

    for event_time in event_times:
        # Event duration and magnitude
        event_duration = np.random.uniform(60, 300)  # 1-5 minutes
        event_magnitude = np.random.uniform(40, 80)  # Additional stress

        # Event time mask
        event_mask = (time >= event_time) & (time <= event_time + event_duration)

        # Add event with decay
        event_profile = np.exp(-(time[event_mask] - event_time) / (event_duration/3))
        stress[event_mask] += event_magnitude * event_profile

    return time, stress


def demonstrate_sn_curves():
    """Demonstrate S-N curve capabilities"""
    print("=" * 60)
    print("S-N CURVE DEMONSTRATION")
    print("=" * 60)

    # Get various standard curves
    curves = {
        'DNV-D': get_dnv_curve('D'),
        'DNV-C': get_dnv_curve('C'),
        'API-X': get_api_curve('X'),
        'BS-D': get_bs_curve('D')
    }

    # Plot comparison
    plt.figure(figsize=(12, 8))

    for name, curve in curves.items():
        cycles, stress = plot_sn_curve(curve)
        plt.loglog(cycles, stress, label=name, linewidth=2)

    plt.xlabel('Cycles to Failure')
    plt.ylabel('Stress Range (MPa)')
    plt.title('Comparison of Standard S-N Curves')
    plt.legend()
    plt.grid(True, alpha=0.3)
    plt.xlim(1e3, 1e8)
    plt.ylim(10, 1000)

    plt.tight_layout()
    plt.savefig('examples/fatigue/sn_curves_comparison.png', dpi=300, bbox_inches='tight')
    plt.show()

    # Print curve parameters
    print("\nS-N Curve Parameters:")
    print("-" * 40)
    for name, curve in curves.items():
        print(f"{name:10s}: A={curve.A:.2e}, m={curve.m:.1f}, "
              f"CAFL={curve.fatigue_limit:.1f} MPa")

    return curves


def demonstrate_rainflow_counting(time, stress):
    """Demonstrate rainflow counting capabilities"""
    print("\n" + "=" * 60)
    print("RAINFLOW COUNTING DEMONSTRATION")
    print("=" * 60)

    # Basic rainflow counting
    cycles_basic = rainflow_count(stress, gate_value=0.05)

    # Rainflow with mean stress
    cycles_with_means = rainflow_with_means(stress)

    # Advanced configuration
    params = RainflowParameters(
        gate_value=0.02,
        gate_type='relative',
        cycle_combination=True
    )
    counter = RainflowCounter(params)
    result = counter.count_cycles(stress)

    print(f"Basic rainflow counting:")
    print(f"  Total cycles: {cycles_basic['count'].sum():.1f}")
    print(f"  Unique ranges: {len(cycles_basic)}")
    print(f"  Max range: {cycles_basic['range'].max():.2f} MPa")

    print(f"\nWith mean stress tracking:")
    print(f"  Total cycles: {cycles_with_means['count'].sum():.1f}")
    print(f"  Mean stress range: {cycles_with_means['mean'].min():.2f} to "
          f"{cycles_with_means['mean'].max():.2f} MPa")

    print(f"\nAdvanced configuration:")
    stats = result['statistics']
    print(f"  Total cycles: {stats.total_cycles:.1f}")
    print(f"  Turning points: {stats.turning_points}")
    print(f"  Original points: {stats.original_points}")
    print(f"  Filtered cycles: {stats.filter_removed}")

    # Create plots
    fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(15, 10))

    # Time history
    time_plot = time[:min(len(time), 3600)]  # First hour only
    stress_plot = stress[:len(time_plot)]
    ax1.plot(time_plot/60, stress_plot)
    ax1.set_xlabel('Time (minutes)')
    ax1.set_ylabel('Stress (MPa)')
    ax1.set_title('Stress Time History (First Hour)')
    ax1.grid(True, alpha=0.3)

    # Cycle range histogram
    cycles = result['cycles']
    if len(cycles) > 0:
        ax2.hist(cycles['range'], bins=30, alpha=0.7, edgecolor='black')
        ax2.set_xlabel('Stress Range (MPa)')
        ax2.set_ylabel('Frequency')
        ax2.set_title('Cycle Range Distribution')
        ax2.grid(True, alpha=0.3)

    # Range vs count scatter
    ax3.scatter(cycles['range'], cycles['count'], alpha=0.7)
    ax3.set_xlabel('Stress Range (MPa)')
    ax3.set_ylabel('Cycle Count')
    ax3.set_title('Rainflow Cycles')
    ax3.grid(True, alpha=0.3)

    # Mean stress vs range (if available)
    if 'mean' in cycles_with_means.columns:
        scatter = ax4.scatter(cycles_with_means['range'], cycles_with_means['mean'],
                            c=cycles_with_means['count'], cmap='viridis', alpha=0.7)
        ax4.set_xlabel('Stress Range (MPa)')
        ax4.set_ylabel('Mean Stress (MPa)')
        ax4.set_title('Range vs Mean Stress')
        ax4.grid(True, alpha=0.3)
        plt.colorbar(scatter, ax=ax4, label='Cycle Count')

    plt.tight_layout()
    plt.savefig('examples/fatigue/rainflow_analysis.png', dpi=300, bbox_inches='tight')
    plt.show()

    return cycles_with_means, result


def demonstrate_damage_accumulation(cycles, sn_curves):
    """Demonstrate damage accumulation methods"""
    print("\n" + "=" * 60)
    print("DAMAGE ACCUMULATION DEMONSTRATION")
    print("=" * 60)

    # Select DNV-D curve for detailed analysis
    sn_curve = sn_curves['DNV-D']

    # Test different damage methods
    methods = {
        'Linear (Miner)': LinearDamageAccumulation(),
        'Modified Miner': ModifiedMinersRule(acceleration_factor=1.5),
        'Marco-Starkey': NonlinearDamageAccumulation('marco_starkey'),
        'Corten-Dolan': NonlinearDamageAccumulation('corten_dolan')
    }

    results = {}

    print(f"Using S-N curve: DNV-D")
    print(f"Input cycles: {len(cycles)} unique ranges, {cycles['count'].sum():.1f} total cycles")
    print("\nDamage Results:")
    print("-" * 50)

    for name, method in methods.items():
        try:
            result = method.calculate_damage(cycles, sn_curve)
            results[name] = result

            print(f"{name:15s}: Damage = {result['total_damage']:.6f}, "
                  f"SF = {result['safety_factor']:.2f}")
        except Exception as e:
            print(f"{name:15s}: Error - {e}")

    # Compare all methods
    comparison_df = compare_damage_methods(cycles, sn_curve)

    print(f"\nComparison Summary:")
    print(comparison_df.to_string(index=False))

    # Test with mean stress correction
    print(f"\nWith Mean Stress Correction (Goodman):")
    print("-" * 40)

    linear_goodman = LinearDamageAccumulation(mean_stress_correction='goodman')
    result_goodman = linear_goodman.calculate_damage(cycles, sn_curve)

    print(f"Linear + Goodman: Damage = {result_goodman['total_damage']:.6f}, "
          f"SF = {result_goodman['safety_factor']:.2f}")

    # Visualize damage contributions
    if 'Linear (Miner)' in results:
        contributions = results['Linear (Miner)']['damage_contributions']
        if contributions:
            ranges = [c['stress_range'] for c in contributions]
            damages = [c['damage_increment'] for c in contributions]

            plt.figure(figsize=(12, 6))

            plt.subplot(1, 2, 1)
            plt.scatter(ranges, damages, alpha=0.7)
            plt.xlabel('Stress Range (MPa)')
            plt.ylabel('Damage Increment')
            plt.title('Damage Contribution by Stress Range')
            plt.yscale('log')
            plt.grid(True, alpha=0.3)

            plt.subplot(1, 2, 2)
            # Cumulative damage
            cumulative = np.cumsum(damages)
            plt.plot(ranges, cumulative, 'o-', alpha=0.7)
            plt.xlabel('Stress Range (MPa)')
            plt.ylabel('Cumulative Damage')
            plt.title('Cumulative Damage Accumulation')
            plt.grid(True, alpha=0.3)

            plt.tight_layout()
            plt.savefig('examples/fatigue/damage_analysis.png', dpi=300, bbox_inches='tight')
            plt.show()

    return results


def demonstrate_frequency_domain_analysis(time, stress):
    """Demonstrate frequency domain fatigue analysis"""
    print("\n" + "=" * 60)
    print("FREQUENCY DOMAIN ANALYSIS DEMONSTRATION")
    print("=" * 60)

    # Calculate PSD from time series
    from scipy import signal

    # Remove mean and detrend
    stress_detrended = signal.detrend(stress)

    # Calculate PSD using Welch's method
    dt = time[1] - time[0]
    fs = 1.0 / dt

    freq, psd = signal.welch(stress_detrended, fs, nperseg=1024,
                            overlap=0.5, window='hann')

    # Remove zero frequency
    freq = freq[1:]
    psd = psd[1:]

    print(f"PSD calculated:")
    print(f"  Frequency range: {freq[0]:.4f} to {freq[-1]:.2f} Hz")
    print(f"  RMS from PSD: {np.sqrt(np.trapz(psd, freq)):.2f} MPa")
    print(f"  RMS from time series: {np.std(stress_detrended):.2f} MPa")

    # Compare frequency domain methods
    sn_curve = get_dnv_curve('D')

    try:
        freq_comparison = compare_frequency_methods(freq, psd, sn_curve)

        print(f"\nFrequency Domain Methods Comparison:")
        print("-" * 50)
        print(freq_comparison.to_string(index=False))

        # Detailed analysis with Dirlik method
        dirlik = DirlikMethod()
        dirlik_result = dirlik.calculate_damage_rate(freq, psd, sn_curve)

        print(f"\nDirlik Method Details:")
        print(f"  Damage rate: {dirlik_result.damage_rate:.2e} /second")
        print(f"  Fatigue life: {dirlik_result.fatigue_life:.1f} seconds "
              f"({dirlik_result.fatigue_life/3600:.1f} hours)")
        print(f"  Equivalent stress: {dirlik_result.equivalent_stress:.2f} MPa")
        print(f"  Expected cycles/sec: {dirlik_result.expected_cycles_per_second:.2f}")

    except Exception as e:
        print(f"Error in frequency domain analysis: {e}")
        freq_comparison = None
        dirlik_result = None

    # Plot PSD and analysis
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 6))

    # PSD plot
    ax1.loglog(freq, psd)
    ax1.set_xlabel('Frequency (Hz)')
    ax1.set_ylabel('PSD (MPa²/Hz)')
    ax1.set_title('Power Spectral Density')
    ax1.grid(True, alpha=0.3)

    # Method comparison (if available)
    if freq_comparison is not None:
        methods = freq_comparison['method']
        life_hours = freq_comparison['fatigue_life'] / 3600

        ax2.bar(methods, life_hours)
        ax2.set_ylabel('Fatigue Life (hours)')
        ax2.set_title('Fatigue Life Comparison')
        ax2.tick_params(axis='x', rotation=45)
        ax2.grid(True, alpha=0.3)

    plt.tight_layout()
    plt.savefig('examples/fatigue/frequency_analysis.png', dpi=300, bbox_inches='tight')
    plt.show()

    return freq_comparison, dirlik_result


def demonstrate_quick_analysis(stress):
    """Demonstrate quick analysis function"""
    print("\n" + "=" * 60)
    print("QUICK ANALYSIS DEMONSTRATION")
    print("=" * 60)

    # Quick analysis with different configurations
    configs = [
        {'sn_curve_standard': 'DNV', 'sn_curve_class': 'D'},
        {'sn_curve_standard': 'API', 'sn_curve_class': 'X'},
        {'sn_curve_standard': 'DNV', 'sn_curve_class': 'D',
         'mean_stress_correction': 'goodman'},
        {'sn_curve_standard': 'DNV', 'sn_curve_class': 'C',
         'gate_value': 0.1}
    ]

    print("Quick Analysis Results:")
    print("-" * 80)

    for i, config in enumerate(configs, 1):
        result = quick_fatigue_analysis(stress, **config)

        config_str = f"{config['sn_curve_standard']}-{config['sn_curve_class']}"
        if 'mean_stress_correction' in config:
            config_str += f" + {config['mean_stress_correction']}"
        if 'gate_value' in config and config['gate_value'] != 0.05:
            config_str += f" (gate={config['gate_value']})"

        print(f"Config {i} ({config_str}):")
        print(f"  Total damage: {result['total_damage']:.6f}")
        print(f"  Safety factor: {result['safety_factor']:.2f}")
        print(f"  Life used: {result['life_fraction_used']*100:.2f}%")
        print()

    # Generate report
    best_result = quick_fatigue_analysis(stress)
    report = create_fatigue_report(best_result)

    print("Sample Report:")
    print("-" * 40)
    print(report)

    # Save report to file
    report_path = 'examples/fatigue/fatigue_report.txt'
    create_fatigue_report(best_result, report_path)
    print(f"\nDetailed report saved to: {report_path}")

    return best_result


def main():
    """Main demonstration function"""
    print("Digital Model Fatigue Analysis - Complete Example")
    print("=" * 60)

    # Create output directory
    output_dir = Path('examples/fatigue')
    output_dir.mkdir(parents=True, exist_ok=True)

    # Generate synthetic data
    print("Generating synthetic stress history...")
    time, stress = generate_synthetic_stress_history(duration=3600, dt=0.1)
    print(f"Generated {len(stress)} points over {time[-1]/3600:.1f} hours")
    print(f"Stress statistics: mean={np.mean(stress):.2f}, "
          f"std={np.std(stress):.2f}, range={np.ptp(stress):.2f} MPa")

    # Run demonstrations
    sn_curves = demonstrate_sn_curves()
    cycles, rainflow_result = demonstrate_rainflow_counting(time, stress)
    damage_results = demonstrate_damage_accumulation(cycles, sn_curves)
    freq_results = demonstrate_frequency_domain_analysis(time, stress)
    quick_result = demonstrate_quick_analysis(stress)

    print("\n" + "=" * 60)
    print("DEMONSTRATION COMPLETE")
    print("=" * 60)
    print("Generated files:")
    print("- examples/fatigue/sn_curves_comparison.png")
    print("- examples/fatigue/rainflow_analysis.png")
    print("- examples/fatigue/damage_analysis.png")
    print("- examples/fatigue/frequency_analysis.png")
    print("- examples/fatigue/fatigue_report.txt")

    return {
        'time': time,
        'stress': stress,
        'sn_curves': sn_curves,
        'cycles': cycles,
        'damage_results': damage_results,
        'frequency_results': freq_results,
        'quick_result': quick_result
    }


if __name__ == "__main__":
    # Run the complete demonstration
    results = main()

    # Keep plots open
    input("\nPress Enter to close all plots and exit...")
    plt.close('all')