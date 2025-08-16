"""
Integration tests for signal_analysis module

Tests the complete workflow with real-world examples.
"""

import pytest
import numpy as np
import pandas as pd
import sys
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from src.digitalmodel.modules.signal_analysis import (
    RainflowCounter, SpectralAnalyzer, TimeSeriesProcessor
)
from src.digitalmodel.modules.signal_analysis.fatigue.damage import FatigueDamageCalculator
from src.digitalmodel.modules.signal_analysis.fatigue.curves import SNCurve
from src.digitalmodel.modules.signal_analysis.filters.frequency import FrequencyFilter
from src.digitalmodel.modules.signal_analysis.adapters import TimeSeriesComponentsAdapter


class TestSignalAnalysisIntegration:
    """Integration tests for complete signal analysis workflow"""
    
    def test_complete_fatigue_analysis_workflow(self):
        """Test complete fatigue analysis from signal to damage"""
        # Generate test signal - variable amplitude loading
        np.random.seed(42)
        fs = 100  # Hz
        duration = 100  # seconds
        t = np.linspace(0, duration, int(fs * duration))
        
        # Multi-frequency loading (typical for offshore structures)
        signal = (
            50 * np.sin(2 * np.pi * 0.1 * t) +  # Wave frequency
            20 * np.sin(2 * np.pi * 0.5 * t) +  # Structural response
            10 * np.sin(2 * np.pi * 2 * t) +    # Higher harmonics
            5 * np.random.randn(len(t))         # Random noise
        )
        signal += 100  # Mean stress
        
        # Step 1: Preprocess signal
        processor = TimeSeriesProcessor()
        
        # Remove outliers
        clean_signal = processor.remove_outliers(signal, method='zscore', threshold=3)
        
        # Detrend
        detrended = processor.detrend(clean_signal, method='linear')
        
        # Calculate statistics
        stats = processor.calculate_statistics(detrended)
        
        assert stats['mean'] < 1.0  # Should be near zero after detrending
        assert stats['std'] > 0
        assert stats['zero_crossings'] > 0
        
        # Step 2: Rainflow counting
        counter = RainflowCounter(method='astm')
        cycles_df = counter.count_cycles(signal, extract_info=True)
        
        assert len(cycles_df) > 0
        assert 'range' in cycles_df.columns
        assert 'mean' in cycles_df.columns
        assert 'count' in cycles_df.columns
        
        # Get statistics
        cycle_stats = counter.get_statistics(cycles_df)
        assert cycle_stats['total_cycles'] > 0
        assert cycle_stats['max_range'] > cycle_stats['min_range']
        
        # Step 3: Fatigue damage calculation
        # Use DNV D curve
        sn_curve = SNCurve(curve_type='standard', standard='DNV', **{'class': 'D'})
        
        damage_calc = FatigueDamageCalculator(method='miners')
        damage_results = damage_calc.calculate_damage(
            cycles_df,
            sn_curve.get_curve_parameters(),
            mean_stress_correction='goodman'
        )
        
        assert 'total_damage' in damage_results
        assert 'life_fraction_remaining' in damage_results
        assert damage_results['total_damage'] >= 0
        assert damage_results['life_fraction_remaining'] <= 1.0
        
        print(f"\nFatigue Analysis Results:")
        print(f"  Total cycles: {cycle_stats['total_cycles']:.0f}")
        print(f"  Max stress range: {cycle_stats['max_range']:.1f} MPa")
        print(f"  Damage: {damage_results['total_damage']:.6f}")
        print(f"  Life remaining: {damage_results['life_fraction_remaining']*100:.1f}%")
    
    def test_spectral_analysis_workflow(self):
        """Test complete spectral analysis workflow"""
        # Generate multi-component signal
        fs = 1000  # Hz
        duration = 10  # seconds
        t = np.linspace(0, duration, int(fs * duration))
        
        # Signal with known frequency components
        frequencies = [5, 10, 20, 50]  # Hz
        amplitudes = [1.0, 0.5, 0.3, 0.1]
        
        signal = np.zeros_like(t)
        for freq, amp in zip(frequencies, amplitudes):
            signal += amp * np.sin(2 * np.pi * freq * t)
        
        # Add noise
        signal += 0.05 * np.random.randn(len(t))
        
        # Step 1: Spectral analysis
        analyzer = SpectralAnalyzer(sampling_rate=fs, method='fft')
        
        # Standard FFT
        spectrum_fft = analyzer.compute_spectrum(signal)
        
        # Welch's method
        analyzer.method = 'welch'
        spectrum_welch = analyzer.compute_spectrum(signal)
        
        # Window-averaged FFT
        spectrum_windowed = analyzer.window_averaged_fft(
            signal, 
            window_size=1024,
            overlap=0.5
        )
        
        # Step 2: Peak detection
        peaks = analyzer.find_peaks(spectrum_fft, n_peaks=4)
        
        assert len(peaks) <= 4
        
        # Verify peaks are near expected frequencies
        peak_freqs = peaks['frequency'].values
        for expected_freq in frequencies:
            closest = np.min(np.abs(peak_freqs - expected_freq))
            assert closest < 1.0  # Within 1 Hz
        
        # Step 3: Filtering
        # Bandpass filter to isolate 8-12 Hz
        filtered_spectrum = analyzer.filter_spectrum(
            spectrum_fft,
            filter_type='bandpass',
            low_freq=8,
            high_freq=12
        )
        
        # Verify filtering worked
        power_in_band = filtered_spectrum[
            (filtered_spectrum['frequency'] >= 8) & 
            (filtered_spectrum['frequency'] <= 12)
        ]['power'].sum()
        
        power_out_band = filtered_spectrum[
            (filtered_spectrum['frequency'] < 8) | 
            (filtered_spectrum['frequency'] > 12)
        ]['power'].sum()
        
        assert power_in_band > 0
        assert power_out_band == 0
        
        print(f"\nSpectral Analysis Results:")
        print(f"  Detected peaks: {peak_freqs}")
        print(f"  Expected frequencies: {frequencies}")
    
    def test_filtering_workflow(self):
        """Test filtering workflow"""
        # Generate noisy signal
        fs = 500  # Hz
        duration = 5  # seconds
        t = np.linspace(0, duration, int(fs * duration))
        
        # Clean signal at 10 Hz
        clean = np.sin(2 * np.pi * 10 * t)
        
        # Add 50 Hz interference (power line noise)
        noise_50hz = 0.5 * np.sin(2 * np.pi * 50 * t)
        
        # Add high frequency noise
        hf_noise = 0.2 * np.sin(2 * np.pi * 150 * t)
        
        # Combined signal
        signal = clean + noise_50hz + hf_noise
        
        # Initialize filter
        freq_filter = FrequencyFilter(filter_type='butterworth', order=4)
        
        # Test 1: Lowpass filter to remove high frequency
        filtered_lp = freq_filter.lowpass_filter(signal, 30, fs)
        
        # Test 2: Bandstop filter to remove 50 Hz
        filtered_notch = freq_filter.bandstop_filter(signal, 48, 52, fs)
        
        # Test 3: Bandpass filter to isolate signal
        filtered_bp = freq_filter.bandpass_filter(signal, 8, 12, fs)
        
        # Verify filtering effectiveness using FFT
        analyzer = SpectralAnalyzer(sampling_rate=fs)
        
        # Original spectrum
        spectrum_orig = analyzer.compute_spectrum(signal)
        
        # Filtered spectra
        spectrum_lp = analyzer.compute_spectrum(filtered_lp)
        spectrum_notch = analyzer.compute_spectrum(filtered_notch)
        spectrum_bp = analyzer.compute_spectrum(filtered_bp)
        
        # Check lowpass removed high frequencies
        hf_power_orig = spectrum_orig[spectrum_orig['frequency'] > 100]['power'].sum()
        hf_power_lp = spectrum_lp[spectrum_lp['frequency'] > 100]['power'].sum()
        assert hf_power_lp < hf_power_orig * 0.01  # 99% reduction
        
        # Check notch removed 50 Hz
        notch_band = (spectrum_orig['frequency'] > 48) & (spectrum_orig['frequency'] < 52)
        power_50hz_orig = spectrum_orig[notch_band]['power'].sum()
        power_50hz_notch = spectrum_notch[notch_band]['power'].sum()
        assert power_50hz_notch < power_50hz_orig * 0.1  # 90% reduction
        
        print(f"\nFiltering Results:")
        print(f"  HF noise reduction: {(1 - hf_power_lp/hf_power_orig)*100:.1f}%")
        print(f"  50Hz noise reduction: {(1 - power_50hz_notch/power_50hz_orig)*100:.1f}%")
    
    def test_backward_compatibility(self):
        """Test backward compatibility with legacy code"""
        # Generate test signal
        signal = np.random.randn(1000) * 10 + 50
        
        # Legacy configuration format
        cfg = {
            'fft': {
                'window': {
                    'size': 256,
                    'overlap': 0.5
                },
                'filter': {
                    'flag': True,
                    'min_frequency': 1.0,
                    'max_frequency': 10.0
                }
            }
        }
        
        # Use adapter
        adapter = TimeSeriesComponentsAdapter(cfg)
        
        # Test rainflow counting
        cycles_df, cycles_dict = adapter.get_rainflow_count_from_time_series(signal)
        
        assert isinstance(cycles_df, pd.DataFrame)
        assert isinstance(cycles_dict, dict)
        assert len(cycles_df) > 0
        
        # Test FFT
        time_step = 0.01  # 100 Hz sampling
        fft_result = adapter.window_average_fft(cfg, signal, time_step)
        
        assert isinstance(fft_result, pd.DataFrame)
        assert 'fft_freq' in fft_result.columns
        assert len(fft_result) > 0
        
        print(f"\nBackward Compatibility Test:")
        print(f"  Adapter successfully processed legacy configuration")
        print(f"  Cycles found: {len(cycles_df)}")
        print(f"  FFT bins: {len(fft_result)}")
    
    def test_real_world_example(self):
        """Test with realistic offshore structure loading"""
        # Simulate 1 hour of data at 20 Hz
        fs = 20  # Hz
        duration = 3600  # 1 hour
        t = np.linspace(0, duration, int(fs * duration))
        
        # Wave loading (dominant)
        Hs = 3.0  # Significant wave height
        Tp = 10.0  # Peak period
        wave_loading = Hs * np.sin(2 * np.pi / Tp * t)
        
        # Add wave spreading
        for i in range(1, 5):
            wave_loading += (Hs / (i+1)) * np.sin(2 * np.pi * i / Tp * t + np.random.rand() * 2 * np.pi)
        
        # Wind loading (low frequency)
        wind_loading = 10 * np.sin(2 * np.pi / 60 * t)  # 1 minute period
        
        # Structural response (higher frequency)
        structural = 2 * np.sin(2 * np.pi * 2 * t)
        
        # Combined stress signal (MPa)
        stress_signal = 50 + wave_loading * 10 + wind_loading + structural
        
        # Add realistic noise
        stress_signal += np.random.randn(len(t)) * 0.5
        
        # Process signal
        processor = TimeSeriesProcessor()
        
        # Remove outliers
        clean_stress = processor.remove_outliers(stress_signal)
        
        # Smooth slightly
        smooth_stress = processor.smooth(clean_stress, method='savgol', window_size=21, polyorder=3)
        
        # Rainflow counting
        counter = RainflowCounter()
        cycles = counter.count_cycles(smooth_stress)
        
        # Generate histogram
        histogram = counter.get_histogram(cycles, bins=20)
        
        # Fatigue analysis with DNV curve
        sn_curve = SNCurve(curve_type='standard', standard='DNV', **{'class': 'F'})
        damage_calc = FatigueDamageCalculator()
        
        # Calculate damage for 1 hour
        damage_1hr = damage_calc.calculate_damage(cycles, sn_curve.get_curve_parameters())
        
        # Extrapolate to design life (25 years)
        design_life_hours = 25 * 365 * 24
        total_damage = damage_1hr['total_damage'] * design_life_hours
        
        print(f"\nReal-World Example Results:")
        print(f"  Signal duration: {duration/3600:.1f} hours")
        print(f"  Cycles extracted: {len(cycles)}")
        print(f"  Max stress range: {cycles['range'].max():.1f} MPa")
        print(f"  Mean stress range: {cycles['range'].mean():.1f} MPa")
        print(f"  1-hour damage: {damage_1hr['total_damage']:.2e}")
        print(f"  25-year damage: {total_damage:.3f}")
        print(f"  Design life OK: {'Yes' if total_damage < 1.0 else 'No'}")
        
        # Spectral analysis
        analyzer = SpectralAnalyzer(sampling_rate=fs, method='welch')
        spectrum = analyzer.compute_spectrum(stress_signal)
        
        # Find dominant frequencies
        peaks = analyzer.find_peaks(spectrum, n_peaks=3)
        
        print(f"\n  Dominant frequencies:")
        for _, peak in peaks.iterrows():
            period = 1.0 / peak['frequency'] if peak['frequency'] > 0 else float('inf')
            print(f"    {peak['frequency']:.3f} Hz (T = {period:.1f} s)")
        
        assert len(cycles) > 100  # Should have many cycles in 1 hour
        assert cycles['range'].max() > cycles['range'].min()
        assert total_damage > 0
        
        return {
            'cycles': cycles,
            'histogram': histogram,
            'damage_1hr': damage_1hr,
            'total_damage': total_damage,
            'spectrum': spectrum,
            'peaks': peaks
        }


def test_module_imports():
    """Test that all modules can be imported"""
    from src.digitalmodel.modules.signal_analysis import (
        RainflowCounter, SpectralAnalyzer, TimeSeriesProcessor
    )
    from src.digitalmodel.modules.signal_analysis.fatigue.damage import FatigueDamageCalculator
    from src.digitalmodel.modules.signal_analysis.fatigue.curves import SNCurve
    from src.digitalmodel.modules.signal_analysis.filters.frequency import FrequencyFilter
    from src.digitalmodel.modules.signal_analysis.adapters import TimeSeriesComponentsAdapter
    
    print("All modules imported successfully")


if __name__ == "__main__":
    # Run tests
    test_module_imports()
    
    test_suite = TestSignalAnalysisIntegration()
    
    print("\n" + "="*60)
    print("Running Signal Analysis Integration Tests")
    print("="*60)
    
    print("\n1. Testing Complete Fatigue Analysis Workflow...")
    test_suite.test_complete_fatigue_analysis_workflow()
    
    print("\n2. Testing Spectral Analysis Workflow...")
    test_suite.test_spectral_analysis_workflow()
    
    print("\n3. Testing Filtering Workflow...")
    test_suite.test_filtering_workflow()
    
    print("\n4. Testing Backward Compatibility...")
    test_suite.test_backward_compatibility()
    
    print("\n5. Testing Real-World Example...")
    results = test_suite.test_real_world_example()
    
    print("\n" + "="*60)
    print("All Integration Tests Passed Successfully!")
    print("="*60)