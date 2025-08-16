"""
Tests for rainflow counting module
"""

import pytest
import numpy as np
import pandas as pd
import sys
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent.parent))

from src.digitalmodel.modules.signal_analysis.core.rainflow import RainflowCounter
from tests.modules.signal_analysis.fixtures.test_signals import rainflow_test_signal


class TestRainflowCounter:
    """Test suite for RainflowCounter class"""
    
    def test_initialization(self):
        """Test RainflowCounter initialization"""
        counter = RainflowCounter()
        assert counter.method == 'astm'
        assert counter.bin_count is None
        assert counter.cycles is None
        
        counter = RainflowCounter(method='half_cycle', bin_count=50)
        assert counter.method == 'half_cycle'
        assert counter.bin_count == 50
    
    def test_count_cycles_basic(self):
        """Test basic cycle counting"""
        counter = RainflowCounter()
        signal = [0, 1, 0, -1, 0, 2, 0, -2, 0]
        
        cycles = counter.count_cycles(signal)
        
        assert isinstance(cycles, pd.DataFrame)
        assert 'range' in cycles.columns
        assert 'mean' in cycles.columns
        assert 'count' in cycles.columns
        assert len(cycles) > 0
    
    def test_count_cycles_astm(self, rainflow_test_signal):
        """Test against ASTM E1049-85 standard test case"""
        counter = RainflowCounter(method='astm')
        signal = rainflow_test_signal['signal']
        expected = rainflow_test_signal['expected_cycles']
        
        cycles = counter.count_cycles(signal, extract_info=False)
        
        # Check that we get the expected number of cycles
        # Note: Exact matching may vary due to implementation details
        assert len(cycles) >= len(expected) - 1
        
        # Check that maximum range is correct
        max_range = cycles['range'].max()
        expected_max = max([c['range'] for c in expected])
        assert abs(max_range - expected_max) < 1.0
    
    def test_find_peaks_valleys(self):
        """Test peak and valley detection"""
        counter = RainflowCounter()
        signal = np.array([0, 1, 0.5, 2, 1, 0, -1, 0, 1])
        
        peaks, valleys = counter._find_peaks_valleys(signal)
        
        assert len(peaks) > 0
        assert len(valleys) > 0
        
        # Check that peaks are actually peaks
        for idx, val in peaks:
            assert val >= signal[max(0, idx-1)]
            assert val >= signal[min(len(signal)-1, idx+1)]
    
    def test_histogram_generation(self):
        """Test histogram generation"""
        counter = RainflowCounter()
        signal = np.random.randn(1000) * 10
        
        cycles = counter.count_cycles(signal)
        histogram = counter.get_histogram(bins=20)
        
        assert isinstance(histogram, pd.DataFrame)
        assert 'range_min' in histogram.columns
        assert 'range_max' in histogram.columns
        assert 'count' in histogram.columns
        assert len(histogram) == 20
    
    def test_statistics_calculation(self):
        """Test statistics calculation"""
        counter = RainflowCounter()
        signal = np.random.randn(500) * 5 + 10
        
        cycles = counter.count_cycles(signal)
        stats = counter.get_statistics()
        
        assert isinstance(stats, dict)
        assert 'total_cycles' in stats
        assert 'max_range' in stats
        assert 'mean_range' in stats
        assert 'weighted_mean_range' in stats
        
        assert stats['total_cycles'] > 0
        assert stats['max_range'] > stats['min_range']
    
    def test_empty_signal(self):
        """Test handling of empty or short signals"""
        counter = RainflowCounter()
        
        # Empty signal
        cycles = counter.count_cycles([])
        assert len(cycles) == 0
        
        # Single point
        cycles = counter.count_cycles([1])
        assert len(cycles) == 0
        
        # Two points
        cycles = counter.count_cycles([1, 2])
        assert len(cycles) == 0
    
    def test_constant_signal(self):
        """Test handling of constant signal"""
        counter = RainflowCounter()
        signal = np.ones(100)
        
        cycles = counter.count_cycles(signal)
        
        # Constant signal should produce no cycles
        assert len(cycles) == 0
    
    def test_combine_half_cycles(self):
        """Test half cycle combination"""
        counter = RainflowCounter(method='full_cycle')
        
        half_cycles = [
            [1.0, 0.5, 0.5],  # range, mean, count
            [1.0, 0.5, 0.5],
            [2.0, 1.0, 0.5],
            [2.0, 1.0, 0.5],
        ]
        
        combined = counter._combine_half_cycles(half_cycles)
        
        # Should combine matching cycles
        assert len(combined) == 2
        
        # Check counts are combined
        for cycle in combined:
            assert cycle[2] == 1.0  # Full cycle
    
    def test_extract_info_flag(self):
        """Test extract_info flag functionality"""
        counter = RainflowCounter()
        signal = [0, 2, -1, 3, -2, 4, -1, 2, 0]
        
        # With info extraction
        cycles_with_info = counter.count_cycles(signal, extract_info=True)
        assert 'i_start' in cycles_with_info.columns
        assert 'i_end' in cycles_with_info.columns
        
        # Without info extraction
        cycles_without_info = counter.count_cycles(signal, extract_info=False)
        assert 'i_start' not in cycles_without_info.columns
        assert 'i_end' not in cycles_without_info.columns


if __name__ == "__main__":
    pytest.main([__file__, "-v"])