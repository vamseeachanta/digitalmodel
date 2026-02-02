#!/usr/bin/env python
"""
Generate FFT, Charts, and Outputs from Timetraces

This test demonstrates the complete signal analysis capabilities including:
- FFT analysis with spectrograms
- Rainflow counting with histograms
- Time series processing with charts
- Export to various formats
"""

import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from pathlib import Path
import json

# Add project to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from digitalmodel.signal_analysis import (
    RainflowCounter, SpectralAnalyzer, TimeSeriesProcessor
)
try:
    from digitalmodel.signal_analysis.fatigue import FatigueDamageCalculator, SNCurve
except ImportError:
    FatigueDamageCalculator = None
    SNCurve = None


def generate_test_signals():
    """Generate various test signals for analysis"""
    np.random.seed(42)
    
    # Time array
    duration = 100  # seconds
    sampling_rate = 100  # Hz
    t = np.linspace(0, duration, duration * sampling_rate)
    
    signals = {}
    
    # 1. Simple sinusoidal signal (single frequency)
    signals['simple_sine'] = {
        'time': t,
        'signal': 50 * np.sin(2 * np.pi * 1.5 * t) + 100,
        'name': 'Simple Sinusoidal Signal (1.5 Hz)',
        'units': 'MPa'
    }
    
    # 2. Multi-frequency signal (simulating offshore structure response)
    wave_freq = 0.1  # Hz (10 second wave period)
    wind_freq = 0.05  # Hz (20 second wind gust)
    structural_freq = 5.0  # Hz (structural vibration)
    
    signals['offshore_response'] = {
        'time': t,
        'signal': (
            100 * np.sin(2 * np.pi * wave_freq * t) +  # Wave loading
            50 * np.sin(2 * np.pi * wind_freq * t) +   # Wind loading
            20 * np.sin(2 * np.pi * structural_freq * t) +  # Structural response
            10 * np.random.randn(len(t)) +  # Noise
            200  # Mean stress
        ),
        'name': 'Offshore Structure Response',
        'units': 'kN'
    }
    
    # 3. Non-stationary signal (varying amplitude)
    amplitude_envelope = 50 * (1 + 0.5 * np.sin(2 * np.pi * 0.01 * t))
    signals['non_stationary'] = {
        'time': t,
        'signal': amplitude_envelope * np.sin(2 * np.pi * 2 * t) + 150,
        'name': 'Non-stationary Signal (Varying Amplitude)',
        'units': 'N'
    }
    
    # 4. Transient event signal
    transient = np.zeros_like(t)
    # Add spikes at specific times
    spike_times = [10, 25, 40, 60, 75, 90]
    for spike_time in spike_times:
        idx = int(spike_time * sampling_rate)
        if idx < len(transient):
            transient[idx:idx+50] = 100 * np.exp(-0.1 * np.arange(50))
    
    signals['transient'] = {
        'time': t,
        'signal': transient + 20 * np.sin(2 * np.pi * 0.5 * t) + 50,
        'name': 'Transient Events Signal',
        'units': 'MPa'
    }
    
    return signals, sampling_rate


def analyze_signal(signal_data, sampling_rate, output_dir):
    """Perform comprehensive signal analysis"""
    
    signal = signal_data['signal']
    time = signal_data['time']
    name = signal_data['name']
    units = signal_data['units']
    
    print(f"\n{'='*60}")
    print(f"Analyzing: {name}")
    print(f"{'='*60}")
    
    results = {}
    
    # 1. Time Series Statistics
    print("\n1. Time Series Analysis...")
    processor = TimeSeriesProcessor()
    
    # Calculate statistics
    stats = processor.calculate_statistics(signal)
    results['statistics'] = stats
    
    print(f"   Mean: {stats['mean']:.2f} {units}")
    print(f"   Std Dev: {stats['std']:.2f} {units}")
    print(f"   RMS: {stats['rms']:.2f} {units}")
    print(f"   Peak-to-Peak: {stats['peak_to_peak']:.2f} {units}")
    
    # Detrend signal
    detrended = processor.detrend(signal, method='linear')
    
    # Smooth signal
    smoothed = processor.smooth(signal, method='moving_average', window_size=50)
    
    # 2. Spectral Analysis (FFT)
    print("\n2. Spectral Analysis (FFT)...")
    analyzer = SpectralAnalyzer(sampling_rate=sampling_rate)
    
    # Compute spectrum
    spectrum = analyzer.compute_spectrum(detrended)
    results['spectrum'] = spectrum.to_dict()
    
    # Find peaks
    peaks = analyzer.find_peaks(spectrum, n_peaks=5)
    results['spectral_peaks'] = peaks.to_dict()
    
    print(f"   Dominant frequencies (Hz): {peaks['frequency'].values[:3]}")
    print(f"   Peak powers: {peaks['power'].values[:3]}")
    
    # Compute PSD using Welch's method
    psd = analyzer.compute_psd(signal, method='welch')
    results['psd'] = psd.to_dict()
    
    # Window-averaged FFT for better frequency resolution
    window_fft = analyzer.window_averaged_fft(
        signal, 
        window_size=1024, 
        overlap=0.5
    )
    results['window_fft'] = window_fft.to_dict()
    
    # 3. Rainflow Counting
    print("\n3. Rainflow Counting...")
    counter = RainflowCounter()
    
    # Count cycles
    cycles = counter.count_cycles(signal)
    results['rainflow_cycles'] = cycles.to_dict()
    
    # Get statistics
    cycle_stats = counter.get_statistics(cycles)
    results['cycle_statistics'] = cycle_stats
    
    print(f"   Total cycles: {cycle_stats['total_cycles']}")
    print(f"   Max range: {cycle_stats['max_range']:.2f} {units}")
    print(f"   Mean range: {cycle_stats['mean_range']:.2f} {units}")
    
    # Generate histogram
    histogram = counter.get_histogram(cycles, bins=20)
    results['cycle_histogram'] = histogram.to_dict()
    
    # 4. Fatigue Analysis
    print("\n4. Fatigue Damage Calculation...")
    if FatigueDamageCalculator and SNCurve:
        try:
            # Use DNV-D curve as example
            sn_curve = SNCurve(curve_type='standard', standard='DNV', **{'class': 'D'})
            damage_calc = FatigueDamageCalculator()
            
            damage = damage_calc.calculate_damage(cycles, sn_curve.get_curve_parameters())
            results['fatigue_damage'] = damage
            
            print(f"   Total damage: {damage['total_damage']:.2e}")
            print(f"   Estimated life (years): {damage.get('estimated_life_years', 'N/A')}")
        except Exception as e:
            print(f"   Fatigue calculation skipped: {e}")
            results['fatigue_damage'] = None
    else:
        print("   Fatigue calculation skipped: Module not available")
        results['fatigue_damage'] = None
    
    # 5. Generate Visualizations
    print("\n5. Generating Visualizations...")
    
    # Create figure with subplots
    fig = plt.figure(figsize=(16, 12))
    fig.suptitle(name, fontsize=16, fontweight='bold')
    
    # Subplot 1: Time series
    ax1 = plt.subplot(3, 3, 1)
    ax1.plot(time[:1000], signal[:1000], 'b-', linewidth=0.5, label='Original')
    ax1.plot(time[:1000], smoothed[:1000], 'r-', linewidth=1, label='Smoothed')
    ax1.set_xlabel('Time (s)')
    ax1.set_ylabel(f'Signal ({units})')
    ax1.set_title('Time Series (First 10 seconds)')
    ax1.legend()
    ax1.grid(True, alpha=0.3)
    
    # Subplot 2: FFT Spectrum
    ax2 = plt.subplot(3, 3, 2)
    freq_limit = 10  # Hz
    freq_mask = spectrum['frequency'] <= freq_limit
    ax2.semilogy(spectrum.loc[freq_mask, 'frequency'], 
                 spectrum.loc[freq_mask, 'power'], 'b-')
    ax2.set_xlabel('Frequency (Hz)')
    ax2.set_ylabel('Power')
    ax2.set_title('Frequency Spectrum (FFT)')
    ax2.grid(True, alpha=0.3)
    
    # Mark peaks
    for _, peak in peaks.iterrows():
        if peak['frequency'] <= freq_limit:
            ax2.axvline(peak['frequency'], color='r', linestyle='--', alpha=0.5)
            ax2.text(peak['frequency'], peak['power'], 
                    f"{peak['frequency']:.2f} Hz", 
                    rotation=90, va='bottom')
    
    # Subplot 3: Power Spectral Density
    ax3 = plt.subplot(3, 3, 3)
    if 'frequency' in psd.columns and 'psd' in psd.columns:
        freq_mask_psd = psd['frequency'] <= freq_limit
        ax3.semilogy(psd.loc[freq_mask_psd, 'frequency'], 
                    psd.loc[freq_mask_psd, 'psd'], 'g-')
    elif 'frequency' in psd.columns and 'power' in psd.columns:
        freq_mask_psd = psd['frequency'] <= freq_limit
        ax3.semilogy(psd.loc[freq_mask_psd, 'frequency'], 
                    psd.loc[freq_mask_psd, 'power'], 'g-')
    ax3.set_xlabel('Frequency (Hz)')
    ax3.set_ylabel('PSD')
    ax3.set_title("Power Spectral Density (Welch's Method)")
    ax3.grid(True, alpha=0.3)
    
    # Subplot 4: Rainflow Histogram
    ax4 = plt.subplot(3, 3, 4)
    if len(histogram) > 0:
        centers = (histogram['range_min'] + histogram['range_max']) / 2
        ax4.bar(centers, histogram['count'], 
               width=(histogram['range_max'] - histogram['range_min']).mean(),
               edgecolor='black', alpha=0.7)
    ax4.set_xlabel(f'Stress Range ({units})')
    ax4.set_ylabel('Count')
    ax4.set_title('Rainflow Counting Histogram')
    ax4.grid(True, alpha=0.3)
    
    # Subplot 5: Rainflow Matrix (Mean vs Range)
    ax5 = plt.subplot(3, 3, 5)
    if len(cycles) > 0:
        scatter = ax5.scatter(cycles['mean'], cycles['range'], 
                            c=cycles['count'], s=20, alpha=0.6, cmap='viridis')
        plt.colorbar(scatter, ax=ax5, label='Count')
    ax5.set_xlabel(f'Mean Stress ({units})')
    ax5.set_ylabel(f'Stress Range ({units})')
    ax5.set_title('Rainflow Matrix')
    ax5.grid(True, alpha=0.3)
    
    # Subplot 6: Statistics Summary
    ax6 = plt.subplot(3, 3, 6)
    ax6.axis('off')
    stats_text = f"""Statistics Summary:
    
Mean: {stats['mean']:.2f} {units}
Std Dev: {stats['std']:.2f} {units}
RMS: {stats['rms']:.2f} {units}
Peak: {stats['max']:.2f} {units}
Min: {stats['min']:.2f} {units}
Kurtosis: {stats['kurtosis']:.2f}
Skewness: {stats['skewness']:.2f}

Rainflow Cycles: {cycle_stats['total_cycles']}
Max Range: {cycle_stats['max_range']:.2f} {units}
Mean Range: {cycle_stats['mean_range']:.2f} {units}"""
    
    ax6.text(0.1, 0.9, stats_text, transform=ax6.transAxes, 
            fontsize=10, verticalalignment='top', fontfamily='monospace')
    ax6.set_title('Analysis Summary')
    
    # Subplot 7: Spectrogram
    ax7 = plt.subplot(3, 3, 7)
    # Simple spectrogram using matplotlib
    ax7.specgram(signal, Fs=sampling_rate, NFFT=256, noverlap=128, cmap='viridis')
    ax7.set_xlabel('Time (s)')
    ax7.set_ylabel('Frequency (Hz)')
    ax7.set_title('Spectrogram')
    ax7.set_ylim([0, 10])
    
    # Subplot 8: Detrended Signal
    ax8 = plt.subplot(3, 3, 8)
    ax8.plot(time[:1000], detrended[:1000], 'b-', linewidth=0.5)
    ax8.set_xlabel('Time (s)')
    ax8.set_ylabel(f'Detrended Signal ({units})')
    ax8.set_title('Detrended Time Series')
    ax8.grid(True, alpha=0.3)
    
    # Subplot 9: Window-averaged FFT
    ax9 = plt.subplot(3, 3, 9)
    if len(window_fft) > 0:
        freq_mask_win = window_fft['frequency'] <= freq_limit
        ax9.semilogy(window_fft.loc[freq_mask_win, 'frequency'], 
                    window_fft.loc[freq_mask_win, 'power'], 'm-')
    ax9.set_xlabel('Frequency (Hz)')
    ax9.set_ylabel('Power')
    ax9.set_title('Window-Averaged FFT')
    ax9.grid(True, alpha=0.3)
    
    plt.tight_layout()
    
    # Save figure
    safe_name = name.replace(' ', '_').replace('(', '').replace(')', '')
    fig_path = output_dir / f"{safe_name}.png"
    plt.savefig(fig_path, dpi=150, bbox_inches='tight')
    print(f"   Saved visualization: {fig_path}")
    
    # Save results to JSON
    json_path = output_dir / f"{safe_name}_results.json"
    
    # Convert numpy types for JSON serialization
    def convert_types(obj):
        if isinstance(obj, np.ndarray):
            return obj.tolist()
        elif isinstance(obj, (np.int32, np.int64)):
            return int(obj)
        elif isinstance(obj, (np.float32, np.float64)):
            return float(obj)
        elif isinstance(obj, dict):
            return {k: convert_types(v) for k, v in obj.items()}
        elif isinstance(obj, list):
            return [convert_types(item) for item in obj]
        return obj
    
    json_results = convert_types(results)
    
    with open(json_path, 'w') as f:
        json.dump(json_results, f, indent=2)
    print(f"   Saved results: {json_path}")
    
    # Save data to CSV
    csv_path = output_dir / f"{safe_name}_timeseries.csv"
    df = pd.DataFrame({
        'time': time,
        'signal': signal,
        'detrended': detrended,
        'smoothed': smoothed
    })
    df.to_csv(csv_path, index=False)
    print(f"   Saved time series data: {csv_path}")
    
    # Save spectrum to CSV
    spectrum_csv = output_dir / f"{safe_name}_spectrum.csv"
    spectrum.to_csv(spectrum_csv, index=False)
    print(f"   Saved spectrum data: {spectrum_csv}")
    
    # Save rainflow cycles to CSV
    cycles_csv = output_dir / f"{safe_name}_cycles.csv"
    cycles.to_csv(cycles_csv, index=False)
    print(f"   Saved rainflow cycles: {cycles_csv}")
    
    plt.close(fig)
    
    return results


def main():
    """Main test function"""
    print("\n" + "#"*60)
    print("#" + " "*10 + "SIGNAL ANALYSIS OUTPUT GENERATION TEST" + " "*10 + "#")
    print("#"*60)
    
    # Create output directory
    output_dir = Path(__file__).parent / 'output'
    output_dir.mkdir(exist_ok=True)
    print(f"\nOutput directory: {output_dir}")
    
    # Generate test signals
    print("\nGenerating test signals...")
    signals, sampling_rate = generate_test_signals()
    print(f"  Generated {len(signals)} test signals")
    print(f"  Sampling rate: {sampling_rate} Hz")
    
    # Analyze each signal
    all_results = {}
    for signal_name, signal_data in signals.items():
        try:
            results = analyze_signal(signal_data, sampling_rate, output_dir)
            all_results[signal_name] = results
        except Exception as e:
            print(f"\n[ERROR] Failed to analyze {signal_name}: {e}")
            import traceback
            traceback.print_exc()
    
    # Generate summary report
    print("\n" + "="*60)
    print("SUMMARY REPORT")
    print("="*60)
    
    report_path = output_dir / 'analysis_report.txt'
    with open(report_path, 'w') as f:
        f.write("SIGNAL ANALYSIS COMPREHENSIVE TEST REPORT\n")
        f.write("="*60 + "\n\n")
        
        for signal_name, results in all_results.items():
            f.write(f"\nSignal: {signals[signal_name]['name']}\n")
            f.write("-"*40 + "\n")
            
            if results.get('statistics'):
                stats = results['statistics']
                f.write(f"  Mean: {stats['mean']:.2f}\n")
                f.write(f"  Std Dev: {stats['std']:.2f}\n")
                f.write(f"  RMS: {stats['rms']:.2f}\n")
            
            if results.get('spectral_peaks'):
                peaks = pd.DataFrame(results['spectral_peaks'])
                if len(peaks) > 0:
                    f.write(f"  Top frequencies: {peaks['frequency'].values[:3]}\n")
            
            if results.get('cycle_statistics'):
                cycle_stats = results['cycle_statistics']
                f.write(f"  Rainflow cycles: {cycle_stats['total_cycles']}\n")
                f.write(f"  Max range: {cycle_stats['max_range']:.2f}\n")
            
            if results.get('fatigue_damage'):
                damage = results['fatigue_damage']
                if damage:
                    f.write(f"  Fatigue damage: {damage['total_damage']:.2e}\n")
    
    print(f"\nSummary report saved: {report_path}")
    
    # List all generated files
    print("\n" + "="*60)
    print("GENERATED FILES:")
    print("="*60)
    
    for file in sorted(output_dir.glob('*')):
        size = file.stat().st_size / 1024  # KB
        print(f"  {file.name:50s} {size:10.1f} KB")
    
    print("\n" + "#"*60)
    print("#" + " "*15 + "TEST COMPLETED SUCCESSFULLY" + " "*15 + "#")
    print("#"*60)
    
    return 0


if __name__ == "__main__":
    sys.exit(main())