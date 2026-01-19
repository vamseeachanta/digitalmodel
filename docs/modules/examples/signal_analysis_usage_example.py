"""
Signal Analysis Module - Comprehensive Usage Example

This example demonstrates the complete workflow for signal analysis including:
1. Signal preprocessing
2. Rainflow counting for fatigue analysis
3. FFT and spectral analysis
4. Filtering operations
5. Fatigue damage calculation
"""

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import sys
from pathlib import Path

# Add project to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.digitalmodel.modules.signal_analysis import (
    RainflowCounter, SpectralAnalyzer, TimeSeriesProcessor
)
from src.digitalmodel.modules.signal_analysis.fatigue.damage import FatigueDamageCalculator
from src.digitalmodel.modules.signal_analysis.fatigue.curves import SNCurve
from src.digitalmodel.modules.signal_analysis.filters.frequency import FrequencyFilter


def example_1_basic_rainflow_counting():
    """Example 1: Basic rainflow counting"""
    print("\n" + "="*60)
    print("Example 1: Basic Rainflow Counting")
    print("="*60)
    
    # Generate a variable amplitude signal
    np.random.seed(42)
    t = np.linspace(0, 100, 1000)
    signal = 50 * np.sin(0.5 * t) + 20 * np.sin(2 * t) + 10 * np.random.randn(len(t))
    
    # Initialize rainflow counter
    counter = RainflowCounter(method='astm')
    
    # Count cycles
    cycles = counter.count_cycles(signal, extract_info=True)
    
    # Get statistics
    stats = counter.get_statistics(cycles)
    
    print(f"\nSignal properties:")
    print(f"  Length: {len(signal)} samples")
    print(f"  Min: {signal.min():.1f}")
    print(f"  Max: {signal.max():.1f}")
    
    print(f"\nRainflow results:")
    print(f"  Total cycles: {stats['total_cycles']:.1f}")
    print(f"  Unique cycles: {stats['unique_cycles']}")
    print(f"  Max range: {stats['max_range']:.1f}")
    print(f"  Mean range: {stats['mean_range']:.1f}")
    print(f"  Weighted mean range: {stats['weighted_mean_range']:.1f}")
    
    # Generate histogram
    histogram = counter.get_histogram(bins=20)
    print(f"\nHistogram bins: {len(histogram)}")
    
    return cycles, histogram


def example_2_spectral_analysis():
    """Example 2: FFT and spectral analysis"""
    print("\n" + "="*60)
    print("Example 2: Spectral Analysis")
    print("="*60)
    
    # Generate multi-frequency signal
    fs = 1000  # Sampling rate (Hz)
    duration = 2  # seconds
    t = np.linspace(0, duration, int(fs * duration))
    
    # Create signal with known frequencies
    frequencies = [10, 25, 50, 100]  # Hz
    amplitudes = [1.0, 0.5, 0.3, 0.1]
    
    signal = np.zeros_like(t)
    for freq, amp in zip(frequencies, amplitudes):
        signal += amp * np.sin(2 * np.pi * freq * t)
    
    # Add noise
    signal += 0.02 * np.random.randn(len(t))
    
    # Initialize analyzer
    analyzer = SpectralAnalyzer(sampling_rate=fs)
    
    # Method 1: Standard FFT
    print("\n1. Standard FFT:")
    spectrum_fft = analyzer.compute_spectrum(signal)
    peaks_fft = analyzer.find_peaks(spectrum_fft, n_peaks=4)
    print(f"  Peak frequencies: {peaks_fft['frequency'].values}")
    
    # Method 2: Welch's method (better for noisy signals)
    print("\n2. Welch's Method:")
    analyzer.method = 'welch'
    spectrum_welch = analyzer.compute_spectrum(signal, nperseg=256)
    peaks_welch = analyzer.find_peaks(spectrum_welch, n_peaks=4)
    print(f"  Peak frequencies: {peaks_welch['frequency'].values}")
    
    # Method 3: Window-averaged FFT
    print("\n3. Window-Averaged FFT:")
    spectrum_windowed = analyzer.window_averaged_fft(
        signal,
        window_size=512,
        overlap=0.5,
        window_function='hann'
    )
    peaks_windowed = analyzer.find_peaks(spectrum_windowed, n_peaks=4)
    print(f"  Peak frequencies: {peaks_windowed['frequency'].values}")
    
    print(f"\nExpected frequencies: {frequencies}")
    
    return spectrum_fft, spectrum_welch, spectrum_windowed


def example_3_signal_filtering():
    """Example 3: Signal filtering"""
    print("\n" + "="*60)
    print("Example 3: Signal Filtering")
    print("="*60)
    
    # Generate noisy signal
    fs = 500  # Hz
    duration = 2  # seconds
    t = np.linspace(0, duration, int(fs * duration))
    
    # Desired signal at 10 Hz
    clean_signal = np.sin(2 * np.pi * 10 * t)
    
    # Add interference
    noise_50hz = 0.3 * np.sin(2 * np.pi * 50 * t)  # Power line noise
    noise_150hz = 0.2 * np.sin(2 * np.pi * 150 * t)  # High frequency noise
    white_noise = 0.1 * np.random.randn(len(t))
    
    # Combined noisy signal
    noisy_signal = clean_signal + noise_50hz + noise_150hz + white_noise
    
    # Initialize filter
    freq_filter = FrequencyFilter(filter_type='butterworth', order=4)
    
    # Apply different filters
    print("\nApplying filters:")
    
    # 1. Lowpass filter
    filtered_lp = freq_filter.lowpass_filter(noisy_signal, 30, fs)
    print("  1. Lowpass filter (30 Hz) applied")
    
    # 2. Bandpass filter
    filtered_bp = freq_filter.bandpass_filter(noisy_signal, 5, 15, fs)
    print("  2. Bandpass filter (5-15 Hz) applied")
    
    # 3. Notch filter for 50 Hz
    filtered_notch = freq_filter.bandstop_filter(noisy_signal, 48, 52, fs)
    print("  3. Notch filter (48-52 Hz) applied")
    
    # Calculate signal-to-noise ratio improvement
    noise_original = np.std(noisy_signal - clean_signal)
    noise_filtered = np.std(filtered_bp - clean_signal)
    snr_improvement = 20 * np.log10(noise_original / noise_filtered)
    
    print(f"\nResults:")
    print(f"  Original noise level: {noise_original:.3f}")
    print(f"  Filtered noise level: {noise_filtered:.3f}")
    print(f"  SNR improvement: {snr_improvement:.1f} dB")
    
    return noisy_signal, filtered_lp, filtered_bp, filtered_notch


def example_4_fatigue_analysis():
    """Example 4: Complete fatigue analysis"""
    print("\n" + "="*60)
    print("Example 4: Fatigue Analysis")
    print("="*60)
    
    # Simulate stress time history (e.g., offshore structure)
    fs = 20  # Hz
    duration = 3600  # 1 hour
    t = np.linspace(0, duration, int(fs * duration))
    
    # Wave-induced stress (primary)
    wave_period = 10  # seconds
    wave_stress = 30 * np.sin(2 * np.pi / wave_period * t)
    
    # Wind-induced stress (low frequency)
    wind_stress = 10 * np.sin(2 * np.pi / 60 * t)
    
    # Structural vibration (high frequency)
    vibration_stress = 5 * np.sin(2 * np.pi * 2 * t)
    
    # Combined stress with mean
    mean_stress = 80  # MPa
    stress = mean_stress + wave_stress + wind_stress + vibration_stress
    stress += 2 * np.random.randn(len(t))  # Add noise
    
    print(f"\nStress signal properties:")
    print(f"  Duration: {duration/3600:.1f} hours")
    print(f"  Sampling rate: {fs} Hz")
    print(f"  Mean stress: {np.mean(stress):.1f} MPa")
    print(f"  Stress range: {stress.min():.1f} to {stress.max():.1f} MPa")
    
    # Step 1: Preprocessing
    processor = TimeSeriesProcessor()
    clean_stress = processor.remove_outliers(stress, method='zscore', threshold=4)
    smooth_stress = processor.smooth(clean_stress, method='savgol', window_size=11, polyorder=3)
    
    # Step 2: Rainflow counting
    counter = RainflowCounter()
    cycles = counter.count_cycles(smooth_stress)
    
    print(f"\nRainflow counting results:")
    print(f"  Cycles extracted: {len(cycles)}")
    print(f"  Max stress range: {cycles['range'].max():.1f} MPa")
    print(f"  Mean stress range: {cycles['range'].mean():.1f} MPa")
    
    # Step 3: S-N curve selection
    # Using DNV F curve (typical for welded joints)
    sn_curve = SNCurve(curve_type='standard', standard='DNV', **{'class': 'F'})
    
    # Step 4: Damage calculation
    damage_calc = FatigueDamageCalculator(method='miners')
    
    # Calculate with mean stress correction
    damage_results = damage_calc.calculate_damage(
        cycles,
        sn_curve.get_curve_parameters(),
        mean_stress_correction='goodman'
    )
    
    print(f"\nFatigue damage results (1 hour):")
    print(f"  Damage: {damage_results['total_damage']:.2e}")
    print(f"  Safety factor: {damage_results['safety_factor']:.1f}")
    
    # Extrapolate to design life
    design_life_years = 25
    hours_per_year = 365 * 24
    total_damage = damage_results['total_damage'] * design_life_years * hours_per_year
    
    print(f"\nDesign life assessment ({design_life_years} years):")
    print(f"  Cumulative damage: {total_damage:.3f}")
    print(f"  Design criteria: Damage < 1.0")
    print(f"  Assessment: {'PASS' if total_damage < 1.0 else 'FAIL'}")
    
    if total_damage > 1.0:
        required_sf = total_damage / 0.5  # Target 0.5 for safety
        print(f"  Required improvement factor: {required_sf:.1f}")
    
    return cycles, damage_results, total_damage


def example_5_time_series_processing():
    """Example 5: Time series preprocessing"""
    print("\n" + "="*60)
    print("Example 5: Time Series Processing")
    print("="*60)
    
    # Generate signal with various artifacts
    np.random.seed(123)
    t = np.linspace(0, 10, 1000)
    
    # Base signal
    signal = 10 * np.sin(2 * np.pi * 0.5 * t)
    
    # Add linear trend
    signal += 2 * t
    
    # Add outliers
    outlier_indices = [100, 300, 500, 700, 900]
    for idx in outlier_indices:
        signal[idx] += np.random.choice([-20, 20])
    
    # Add noise
    signal += np.random.randn(len(t))
    
    # Initialize processor
    processor = TimeSeriesProcessor()
    
    print("\nProcessing steps:")
    
    # 1. Remove outliers
    signal_no_outliers = processor.remove_outliers(signal, method='zscore', threshold=3)
    print(f"  1. Outliers removed: {len(signal) - len(np.where(signal == signal_no_outliers)[0])}")
    
    # 2. Detrend
    signal_detrended = processor.detrend(signal_no_outliers, method='linear')
    print(f"  2. Linear trend removed")
    
    # 3. Smooth
    signal_smooth = processor.smooth(signal_detrended, method='savgol', window_size=21, polyorder=3)
    print(f"  3. Savitzky-Golay smoothing applied")
    
    # 4. Calculate statistics
    stats_original = processor.calculate_statistics(signal)
    stats_processed = processor.calculate_statistics(signal_smooth)
    
    print(f"\nStatistics comparison:")
    print(f"  Original - Mean: {stats_original['mean']:.2f}, Std: {stats_original['std']:.2f}")
    print(f"  Processed - Mean: {stats_processed['mean']:.2f}, Std: {stats_processed['std']:.2f}")
    print(f"  Zero crossings - Original: {stats_original['zero_crossings']}, Processed: {stats_processed['zero_crossings']}")
    
    # 5. Resample
    new_signal, new_time = processor.resample(signal_smooth, 100, 50, method='cubic')
    print(f"\nResampling:")
    print(f"  Original samples: {len(signal)}")
    print(f"  Resampled: {len(new_signal)}")
    
    return signal, signal_smooth, stats_processed


def example_6_backward_compatibility():
    """Example 6: Using backward compatibility adapters"""
    print("\n" + "="*60)
    print("Example 6: Backward Compatibility")
    print("="*60)
    
    # Legacy configuration format
    legacy_cfg = {
        'fft': {
            'window': {
                'size': 512,
                'overlap': 0.5,
                'moving_average': {
                    'flag': True,
                    'window_size': 5
                }
            },
            'filter': {
                'flag': True,
                'min_frequency': 0.5,
                'max_frequency': 20.0,
                'band_pass': {
                    'flag': True,
                    'frequency_minimum': 1.0,
                    'frequency_maximum': 10.0
                }
            },
            'peaks': {
                'flag': True,
                'min_peak_distance': 10
            }
        }
    }
    
    print("\nUsing legacy configuration with adapter...")
    
    # Import adapter
    from src.digitalmodel.modules.signal_analysis.adapters import TimeSeriesComponentsAdapter
    
    # Create adapter with legacy config
    adapter = TimeSeriesComponentsAdapter(legacy_cfg)
    
    # Generate test signal
    t = np.linspace(0, 10, 1000)
    signal = np.sin(2 * np.pi * 2 * t) + 0.5 * np.sin(2 * np.pi * 5 * t)
    
    # Use legacy methods
    print("\n1. Legacy rainflow counting:")
    cycles_df, cycles_dict = adapter.count_cycles(signal)
    print(f"   Cycles found: {len(cycles_df)}")
    
    print("\n2. Legacy FFT analysis:")
    fft_result = adapter.window_averaged_fft(legacy_cfg, signal, time_step=0.01)
    print(f"   FFT bins: {len(fft_result)}")
    
    print("\n3. Legacy filtering:")
    filtered_fft = adapter.filter_spectrum(legacy_cfg, fft_result)
    print(f"   Filtered bins: {len(filtered_fft)}")
    
    print("\n[OK] Legacy code works with new module via adapter")
    print("  Note: Deprecation warnings indicate need for migration")
    
    return cycles_df, fft_result


def main():
    """Run all examples"""
    print("\n" + "#"*60)
    print("#" + " "*20 + "SIGNAL ANALYSIS MODULE" + " "*16 + "#")
    print("#" + " "*18 + "Comprehensive Examples" + " "*18 + "#")
    print("#"*60)
    
    # Run examples
    cycles1, histogram1 = example_1_basic_rainflow_counting()
    
    spectrum_fft, spectrum_welch, spectrum_windowed = example_2_spectral_analysis()
    
    noisy, filtered_lp, filtered_bp, filtered_notch = example_3_signal_filtering()
    
    cycles4, damage4, total_damage4 = example_4_fatigue_analysis()
    
    original, processed, stats = example_5_time_series_processing()
    
    cycles6, fft6 = example_6_backward_compatibility()
    
    print("\n" + "#"*60)
    print("#" + " "*15 + "ALL EXAMPLES COMPLETED SUCCESSFULLY" + " "*8 + "#")
    print("#"*60)
    print("\nThe signal_analysis module provides:")
    print("  [OK] Rainflow counting (ASTM E1049-85 compliant)")
    print("  [OK] FFT and spectral analysis (multiple methods)")
    print("  [OK] Advanced filtering (Butterworth, Chebyshev, etc.)")
    print("  [OK] Fatigue damage calculation (DNV, API, BS curves)")
    print("  [OK] Time series processing (detrending, smoothing, etc.)")
    print("  [OK] Backward compatibility with existing code")
    print("\nNo OrcaFlex or proprietary dependencies required!")


if __name__ == "__main__":
    main()