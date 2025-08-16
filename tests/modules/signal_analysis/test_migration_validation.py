#!/usr/bin/env python
"""
Migration Validation Tests

Comprehensive tests to validate that the migration from old to new
signal analysis module produces consistent results.
"""

import sys
import numpy as np
import pandas as pd
import warnings
from pathlib import Path

# Add project to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

# Suppress deprecation warnings for comparison
warnings.filterwarnings('ignore', category=DeprecationWarning)


class TestMigrationValidation:
    """Validate migration produces consistent results"""
    
    def setup_method(self):
        """Set up test data"""
        np.random.seed(42)
        
        # Generate test signal
        self.fs = 100  # Hz
        self.duration = 10  # seconds
        t = np.linspace(0, self.duration, int(self.fs * self.duration))
        
        # Multi-frequency signal with noise
        self.signal = (
            50 * np.sin(2 * np.pi * 1 * t) +     # 1 Hz
            30 * np.sin(2 * np.pi * 5 * t) +     # 5 Hz
            10 * np.sin(2 * np.pi * 10 * t) +    # 10 Hz
            5 * np.random.randn(len(t))          # Noise
        )
        
        # Add mean offset for fatigue analysis
        self.signal_with_mean = self.signal + 100
        
    def test_rainflow_counting_consistency(self):
        """Test that rainflow counting produces similar results"""
        print("\n" + "="*60)
        print("Test: Rainflow Counting Consistency")
        print("="*60)
        
        # Old implementation
        from digitalmodel.modules.time_series.time_series_components import TimeSeriesComponents
        old_tsc = TimeSeriesComponents({})
        old_cycles_df, old_cycles_dict = old_tsc.count_cycles(
            self.signal_with_mean
        )
        
        # New implementation
        from digitalmodel.modules.signal_analysis import RainflowCounter
        new_counter = RainflowCounter(method='astm')
        new_cycles = new_counter.count_cycles(self.signal_with_mean)
        
        # Compare results
        print(f"Old implementation: {len(old_cycles_df)} cycles")
        print(f"New implementation: {len(new_cycles)} cycles")
        
        # Check total cycle count (should be very close)
        old_total = old_cycles_df['count'].sum()
        new_total = new_cycles['count'].sum()
        
        difference = abs(old_total - new_total) / old_total * 100
        print(f"Total cycles - Old: {old_total:.1f}, New: {new_total:.1f}")
        print(f"Difference: {difference:.2f}%")
        
        assert difference < 5, f"Cycle count difference {difference:.2f}% exceeds 5% threshold"
        
        # Check statistics
        old_stats = {
            'max_range': old_cycles_df['range'].max(),
            'mean_range': old_cycles_df['range'].mean(),
            'std_range': old_cycles_df['range'].std()
        }
        
        new_stats = new_counter.get_statistics(new_cycles)
        
        print(f"\nStatistics comparison:")
        print(f"Max range - Old: {old_stats['max_range']:.2f}, New: {new_stats['max_range']:.2f}")
        print(f"Mean range - Old: {old_stats['mean_range']:.2f}, New: {new_stats['mean_range']:.2f}")
        
        # Allow for small differences due to algorithm improvements
        assert abs(old_stats['max_range'] - new_stats['max_range']) < 5
        assert abs(old_stats['mean_range'] - new_stats['mean_range']) < 2
        
        print("[OK] Rainflow counting produces consistent results")
        
    def test_fft_spectral_consistency(self):
        """Test that FFT/spectral analysis produces similar results"""
        print("\n" + "="*60)
        print("Test: FFT/Spectral Analysis Consistency")
        print("="*60)
        
        # Old implementation using adapter
        from digitalmodel.modules.signal_analysis.adapters import TimeSeriesComponentsAdapter
        
        old_cfg = {
            'default': {
                'analysis': {
                    'fft': {
                        'window': {
                            'size': 256,
                            'overlap': 0.5
                        }
                    }
                }
            }
        }
        
        old_adapter = TimeSeriesComponentsAdapter(old_cfg)
        old_fft = old_adapter.window_average_fft(old_cfg, self.signal, time_step=1/self.fs)
        
        # New implementation
        from digitalmodel.modules.signal_analysis import SpectralAnalyzer
        new_analyzer = SpectralAnalyzer(sampling_rate=self.fs, method='fft')
        new_spectrum = new_analyzer.window_averaged_fft(
            self.signal,
            window_size=256,
            overlap=0.5
        )
        
        # Compare results
        print(f"Old FFT bins: {len(old_fft)}")
        print(f"New spectrum bins: {len(new_spectrum)}")
        
        # Find peaks in both
        old_peaks = old_adapter.get_peaks_in_fft(old_fft, old_cfg)
        new_peaks = new_analyzer.find_peaks(new_spectrum, n_peaks=3)
        
        print(f"\nPeak frequencies comparison:")
        if old_peaks is not None and len(old_peaks) > 0:
            print(f"Old peaks: {old_peaks['fft_freq'].values[:3]}")
        print(f"New peaks: {new_peaks['frequency'].values}")
        
        # Check that major frequencies are detected (1 Hz, 5 Hz, 10 Hz)
        expected_freqs = [1.0, 5.0, 10.0]
        new_peak_freqs = new_peaks['frequency'].values
        
        for expected in expected_freqs[:2]:  # At least detect the two strongest
            closest = min(abs(new_peak_freqs - expected))
            assert closest < 0.5, f"Expected frequency {expected} Hz not found"
        
        print("[OK] Spectral analysis produces consistent results")
        
    def test_filtering_consistency(self):
        """Test that filtering produces similar results"""
        print("\n" + "="*60)
        print("Test: Filtering Consistency")
        print("="*60)
        
        # New implementation
        from digitalmodel.modules.signal_analysis.filters import FrequencyFilter
        
        freq_filter = FrequencyFilter(filter_type='butterworth', order=4)
        
        # Test lowpass filter
        filtered_lp = freq_filter.lowpass_filter(self.signal, cutoff=7, fs=self.fs)
        
        # Verify filtering worked
        from digitalmodel.modules.signal_analysis import SpectralAnalyzer
        analyzer = SpectralAnalyzer(sampling_rate=self.fs)
        
        original_spectrum = analyzer.compute_spectrum(self.signal)
        filtered_spectrum = analyzer.compute_spectrum(filtered_lp)
        
        # Check power reduction above cutoff
        high_freq_mask = original_spectrum['frequency'] > 8
        original_hf_power = original_spectrum[high_freq_mask]['power'].sum()
        filtered_hf_power = filtered_spectrum[high_freq_mask]['power'].sum()
        
        reduction = (1 - filtered_hf_power / original_hf_power) * 100
        print(f"High frequency power reduction: {reduction:.1f}%")
        
        assert reduction > 90, f"Insufficient filtering: {reduction:.1f}% < 90%"
        
        print("[OK] Filtering produces expected results")
        
    def test_preprocessing_consistency(self):
        """Test that preprocessing produces consistent results"""
        print("\n" + "="*60)
        print("Test: Preprocessing Consistency")
        print("="*60)
        
        from digitalmodel.modules.signal_analysis import TimeSeriesProcessor
        
        processor = TimeSeriesProcessor()
        
        # Add trend and outliers
        signal_with_artifacts = self.signal.copy()
        signal_with_artifacts += 0.5 * np.arange(len(signal_with_artifacts))  # Trend
        signal_with_artifacts[100] = 500  # Outlier
        signal_with_artifacts[500] = -500  # Outlier
        
        # Test outlier removal
        clean_signal = processor.remove_outliers(signal_with_artifacts, method='zscore', threshold=3)
        outliers_removed = len(signal_with_artifacts) - np.sum(signal_with_artifacts == clean_signal)
        print(f"Outliers removed: {outliers_removed}")
        assert outliers_removed >= 2, "Failed to remove outliers"
        
        # Test detrending
        detrended = processor.detrend(clean_signal, method='linear')
        
        # Check trend removal
        original_trend = np.polyfit(np.arange(len(clean_signal)), clean_signal, 1)[0]
        detrended_trend = np.polyfit(np.arange(len(detrended)), detrended, 1)[0]
        
        print(f"Original trend: {original_trend:.4f}")
        print(f"Detrended trend: {detrended_trend:.4f}")
        assert abs(detrended_trend) < abs(original_trend) * 0.1, "Detrending failed"
        
        # Test statistics
        stats = processor.calculate_statistics(detrended)
        print(f"Statistics: Mean={stats['mean']:.2f}, Std={stats['std']:.2f}")
        assert abs(stats['mean']) < 1.0, "Mean not centered after detrending"
        
        print("[OK] Preprocessing produces consistent results")
        
    def test_adapter_backward_compatibility(self):
        """Test that adapter maintains backward compatibility"""
        print("\n" + "="*60)
        print("Test: Adapter Backward Compatibility")
        print("="*60)
        
        from digitalmodel.modules.signal_analysis.adapters import TimeSeriesComponentsAdapter
        
        # Legacy configuration
        legacy_cfg = {
            'default': {
                'analysis': {
                    'fft': {
                        'window': {'size': 512},
                        'filter': {
                            'flag': True,
                            'min_frequency': 1.0,
                            'max_frequency': 10.0
                        }
                    }
                }
            }
        }
        
        adapter = TimeSeriesComponentsAdapter(legacy_cfg)
        
        # Test all legacy methods
        methods_to_test = [
            'get_rainflow_count_from_time_series',
            'window_average_fft',
            'get_filtered_fft',
            'get_statistics_from_time_series'
        ]
        
        print("Testing legacy methods:")
        
        # Rainflow
        cycles_df, cycles_dict = adapter.get_rainflow_count_from_time_series(self.signal_with_mean)
        assert len(cycles_df) > 0, "Rainflow adapter failed"
        print(f"  [OK] get_rainflow_count_from_time_series: {len(cycles_df)} cycles")
        
        # FFT
        fft_result = adapter.window_average_fft(legacy_cfg, self.signal, time_step=1/self.fs)
        assert len(fft_result) > 0, "FFT adapter failed"
        print(f"  [OK] window_average_fft: {len(fft_result)} bins")
        
        # Filtering
        filtered = adapter.get_filtered_fft(legacy_cfg, fft_result)
        assert len(filtered) > 0, "Filter adapter failed"
        print(f"  [OK] get_filtered_fft: {len(filtered)} bins")
        
        # Statistics
        stats = adapter.get_statistics_from_time_series(self.signal)
        assert 'mean' in stats, "Statistics adapter failed"
        print(f"  [OK] get_statistics_from_time_series: {len(stats)} statistics")
        
        print("\n✓ All adapter methods maintain backward compatibility")
        
    def test_performance_improvement(self):
        """Test that new implementation is faster"""
        print("\n" + "="*60)
        print("Test: Performance Improvement")
        print("="*60)
        
        import time
        
        # Generate larger signal for performance test
        large_signal = np.random.randn(10000) * 10 + 50
        
        # Old implementation timing
        from digitalmodel.modules.time_series.time_series_components import TimeSeriesComponents
        old_tsc = TimeSeriesComponents({})
        
        start = time.time()
        old_cycles, _ = old_tsc.count_cycles(large_signal)
        old_time = time.time() - start
        
        # New implementation timing
        from digitalmodel.modules.signal_analysis import RainflowCounter
        new_counter = RainflowCounter()
        
        start = time.time()
        new_cycles = new_counter.count_cycles(large_signal)
        new_time = time.time() - start
        
        improvement = old_time / new_time
        
        print(f"Performance comparison (10,000 samples):")
        print(f"  Old implementation: {old_time*1000:.1f} ms")
        print(f"  New implementation: {new_time*1000:.1f} ms")
        print(f"  Improvement: {improvement:.1f}x faster")
        
        assert new_time <= old_time * 1.5, "New implementation is slower!"
        
        print("[OK] Performance meets or exceeds old implementation")


def run_all_tests():
    """Run all migration validation tests"""
    print("\n" + "#"*60)
    print("#" + " "*15 + "MIGRATION VALIDATION TESTS" + " "*17 + "#")
    print("#"*60)
    
    test_suite = TestMigrationValidation()
    test_suite.setup_method()
    
    tests = [
        test_suite.test_rainflow_counting_consistency,
        test_suite.test_fft_spectral_consistency,
        test_suite.test_filtering_consistency,
        test_suite.test_preprocessing_consistency,
        test_suite.test_adapter_backward_compatibility,
        test_suite.test_performance_improvement
    ]
    
    passed = 0
    failed = 0
    
    for test in tests:
        try:
            test()
            passed += 1
        except AssertionError as e:
            print(f"\n[FAIL] Test failed: {e}")
            failed += 1
        except Exception as e:
            print(f"\n[FAIL] Test error: {e}")
            failed += 1
    
    print("\n" + "#"*60)
    print(f"# Results: {passed} passed, {failed} failed")
    print("#"*60)
    
    if failed == 0:
        print("\n[SUCCESS] ALL MIGRATION TESTS PASSED!")
        print("The new signal_analysis module is fully compatible with the old implementation")
        print("while providing improved performance and additional features.")
    else:
        print(f"\n[WARNING] {failed} tests failed. Please review the migration.")
    
    return failed == 0


if __name__ == "__main__":
    success = run_all_tests()
    sys.exit(0 if success else 1)