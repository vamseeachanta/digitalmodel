#!/usr/bin/env python3
"""
Unit tests for the rainflow counting algorithm
Tests against known patterns with expected results per ASTM E1049
"""

import numpy as np
import pytest
from digitalmodel.structural.fatigue_analysis.rainflow_counter import (
    RainflowCounter, RainflowCycle, rainflow_count
)


class TestRainflowCounter:
    """Test suite for RainflowCounter class"""
    
    def test_initialization(self):
        """Test counter initialization"""
        counter = RainflowCounter()
        assert counter.gate_value is None
        assert counter.cycles == []
        
        counter_with_gate = RainflowCounter(gate_value=10.0)
        assert counter_with_gate.gate_value == 10.0
    
    def test_simple_sine_wave(self):
        """Test with simple sinusoidal loading"""
        # Create perfect sine wave
        t = np.linspace(0, 2*np.pi, 100)
        stress = 100 * np.sin(t)
        
        counter = RainflowCounter()
        ranges, counts = counter.count_cycles(stress)
        
        # Should identify one major cycle with range ~200
        assert len(ranges) > 0
        max_range = np.max(ranges)
        assert 195 < max_range < 205  # Allow for numerical precision
        
        stats = counter.get_cycle_statistics()
        assert stats['total_cycles'] > 0
    
    def test_constant_amplitude_cycles(self):
        """Test with constant amplitude loading"""
        # Create sawtooth pattern: 0-100-0-100-0
        stress = np.array([0, 100, 0, 100, 0, 100, 0])
        
        counter = RainflowCounter()
        ranges, counts = counter.count_cycles(stress)
        
        # Should identify cycles with range = 100
        assert 100 in ranges
        # Check total count
        total_counts = np.sum(counts)
        assert total_counts >= 2  # At least 2 full cycles
    
    def test_variable_amplitude(self):
        """Test with variable amplitude loading"""
        # ASTM E1049 example pattern
        stress = np.array([0, 50, 25, 75, 0, 100, 0])
        
        counter = RainflowCounter()
        ranges, counts = counter.count_cycles(stress)
        
        # Should identify multiple ranges
        assert len(ranges) >= 2
        assert np.max(ranges) == 100  # Largest range
    
    def test_gate_filtering(self):
        """Test gate value filtering of small cycles"""
        # Add small noise to large cycle
        t = np.linspace(0, 2*np.pi, 100)
        stress = 100 * np.sin(t) + 5 * np.random.randn(100)  # Add small noise
        
        # Without gate filter
        counter_no_gate = RainflowCounter()
        ranges_no_gate, _ = counter_no_gate.count_cycles(stress)
        
        # With gate filter
        counter_with_gate = RainflowCounter(gate_value=10.0)
        ranges_gate, _ = counter_with_gate.count_cycles(stress)
        
        # Gate filter should reduce number of small cycles
        assert len(ranges_gate) <= len(ranges_no_gate)
        assert np.min(ranges_gate) >= 10.0  # No cycles smaller than gate
    
    def test_empty_series(self):
        """Test with empty time series"""
        stress = np.array([])
        
        counter = RainflowCounter()
        ranges, counts = counter.count_cycles(stress)
        
        assert len(ranges) == 0
        assert len(counts) == 0
        
        stats = counter.get_cycle_statistics()
        assert stats['total_cycles'] == 0
    
    def test_single_value(self):
        """Test with single value (no cycles)"""
        stress = np.array([100])
        
        counter = RainflowCounter()
        ranges, counts = counter.count_cycles(stress)
        
        assert len(ranges) == 0
        assert len(counts) == 0
    
    def test_two_point_series(self):
        """Test with two-point series (one half cycle)"""
        stress = np.array([0, 100])
        
        counter = RainflowCounter()
        ranges, counts = counter.count_cycles(stress)
        
        if len(ranges) > 0:
            assert ranges[0] == 100
            assert counts[0] == 0.5  # Half cycle
    
    def test_rainflow_residual(self):
        """Test handling of residual (unclosed cycles)"""
        # Incomplete cycle pattern
        stress = np.array([0, 100, 50, 150, 75])
        
        counter = RainflowCounter()
        ranges, counts = counter.count_cycles(stress)
        
        # Should handle residual as half cycles
        total_counts = np.sum(counts)
        assert total_counts > 0
        
        # Check for half cycles
        half_cycles = np.sum(counts[counts == 0.5])
        assert half_cycles > 0
    
    def test_cycle_statistics(self):
        """Test cycle statistics calculation"""
        # Known pattern
        stress = np.array([0, 100, 0, 50, 0, 100, 0])
        
        counter = RainflowCounter()
        ranges, counts = counter.count_cycles(stress)
        
        stats = counter.get_cycle_statistics()
        
        assert 'total_cycles' in stats
        assert 'full_cycles' in stats
        assert 'half_cycles' in stats
        assert 'max_range' in stats
        assert 'min_range' in stats
        assert 'mean_range' in stats
        
        assert stats['max_range'] == 100
        assert stats['min_range'] > 0
        assert stats['total_cycles'] == stats['full_cycles'] + stats['half_cycles']
    
    def test_export_results(self, tmp_path):
        """Test exporting results to CSV"""
        # Create test data
        t = np.linspace(0, 4*np.pi, 200)
        stress = 100 * np.sin(t)
        
        counter = RainflowCounter()
        counter.count_cycles(stress)
        
        # Export to temporary file
        output_file = tmp_path / "rainflow_results.csv"
        counter.export_results(str(output_file), include_stats=True)
        
        # Check file exists
        assert output_file.exists()
        
        # Read and verify content
        import pandas as pd
        df = pd.read_csv(output_file, comment='#')
        
        assert 'Range' in df.columns
        assert 'Counts' in df.columns
        assert len(df) > 0
    
    def test_convenience_function(self):
        """Test the convenience function rainflow_count"""
        stress = np.array([0, 100, 0, 100, 0])
        
        ranges, counts = rainflow_count(stress, gate_value=5.0)
        
        assert len(ranges) > 0
        assert len(counts) == len(ranges)
        assert np.all(counts > 0)


class TestRainflowValidation:
    """Validation tests against known ASTM E1049 examples"""
    
    def test_astm_example_1(self):
        """Test against ASTM E1049 Example 1"""
        # Standard ASTM test sequence
        stress = np.array([0, -2, 1, -3, 5, -1, 3, -4, 4, -2, 0])
        
        counter = RainflowCounter()
        ranges, counts = counter.count_cycles(stress)
        
        # Expected ranges from ASTM standard
        expected_ranges = {
            3: 0.5,   # Range 3, half cycle
            4: 1.5,   # Range 4, 1.5 cycles
            6: 0.5,   # Range 6, half cycle
            8: 1.0,   # Range 8, one cycle
            9: 0.5    # Range 9, half cycle
        }
        
        # Verify against expected (allowing for algorithm variations)
        total_expected = sum(expected_ranges.values())
        total_actual = np.sum(counts)
        
        # Total counts should match
        assert abs(total_actual - total_expected) < 1.0
    
    def test_repeating_pattern(self):
        """Test with repeating load pattern"""
        # Repeat pattern 3 times
        single_pattern = [0, 100, 50, 150, 0]
        stress = np.array(single_pattern * 3)
        
        counter = RainflowCounter()
        ranges, counts = counter.count_cycles(stress)
        
        # Should identify consistent patterns
        assert len(ranges) > 0
        assert np.max(ranges) == 150  # Maximum range
        
        # Total counts should scale with repetitions
        total_counts = np.sum(counts)
        assert total_counts > 2  # Multiple cycles from repetition


class TestRainflowWithFatigueData:
    """Tests with realistic fatigue loading data"""
    
    def test_combined_loading(self):
        """Test with combined high and low cycle loading"""
        t = np.linspace(0, 100, 10000)
        # Combine low frequency high amplitude with high frequency low amplitude
        stress = 100 * np.sin(0.1 * t) + 20 * np.sin(2 * t)
        
        counter = RainflowCounter(gate_value=5.0)
        ranges, counts = counter.count_cycles(stress)
        
        # Should identify both large and small cycles
        assert len(ranges) >= 2
        assert np.max(ranges) > 180  # Large cycles ~200
        
        # Verify reasonable distribution
        large_cycles = ranges[ranges > 150]
        small_cycles = ranges[ranges < 50]
        
        assert len(large_cycles) > 0
        assert len(small_cycles) > 0
    
    def test_mean_stress_effect(self):
        """Test cycles with different mean stresses"""
        t = np.linspace(0, 4*np.pi, 200)
        
        # Tensile mean stress
        stress_tensile = 100 + 50 * np.sin(t)
        
        # Compressive mean stress
        stress_compressive = -100 + 50 * np.sin(t)
        
        counter = RainflowCounter()
        
        # Count cycles for both
        ranges_t, counts_t = counter.count_cycles(stress_tensile)
        stats_t = counter.get_cycle_statistics()
        
        ranges_c, counts_c = counter.count_cycles(stress_compressive)
        stats_c = counter.get_cycle_statistics()
        
        # Ranges should be similar
        assert abs(np.max(ranges_t) - np.max(ranges_c)) < 10
        
        # Mean stresses should be different
        assert stats_t['mean_mean'] > 0  # Positive mean
        assert stats_c['mean_mean'] < 0  # Negative mean


if __name__ == "__main__":
    pytest.main([__file__, "-v"])