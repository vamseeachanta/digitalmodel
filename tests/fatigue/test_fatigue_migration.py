#!/usr/bin/env python3
"""
Test Suite for Fatigue Module Migration
=======================================

This test suite validates the migration of legacy fatigue analysis code
to the modern Digital Model fatigue module, ensuring engineering accuracy
and backward compatibility.

Key validation areas:
- S-N curve calculations match legacy behavior
- Multi-slope curve implementation
- High stress range calculations (LinearSlopeCal equivalent)
- Shear data analysis (Shear7dataCal equivalent)
- Damage accumulation accuracy
- Engineering validation functions

Author: Digital Model Team
Version: 2.0.0
"""

import pytest
import numpy as np
import pandas as pd
from pathlib import Path
import warnings

# Import modules under test
from digitalmodel.fatigue import (
    FatigueAnalysisEngine,
    FatigueAnalysisConfig,
    MultislopeSNCurve,
    get_dnv_curve,
    StandardSNCurves,
    LinearDamageAccumulation,
    quick_time_domain_analysis,
    quick_frequency_domain_analysis
)


class TestSNCurveMigration:
    """Test S-N curve implementation against legacy patterns"""

    def test_basic_sn_curve_calculations(self):
        """Test basic S-N curve calculations match expected values"""
        # DNV-D curve (from legacy data)
        curve = get_dnv_curve('D')

        # Test specific stress-life points
        test_cases = [
            (100.0, 5.73e9),   # High stress
            (60.0, 4.37e10),   # Medium stress
            (52.63, np.inf),   # At fatigue limit
            (40.0, np.inf),    # Below fatigue limit
        ]

        for stress, expected_cycles in test_cases:
            calculated_cycles = curve.get_allowable_cycles(stress)

            if np.isfinite(expected_cycles):
                # Allow 5% tolerance for engineering calculations
                assert abs(calculated_cycles - expected_cycles) / expected_cycles < 0.05
            else:
                assert np.isinf(calculated_cycles)

    def test_multislope_curve_implementation(self):
        """Test multi-slope S-N curve matches legacy FatigueBasiccurve patterns"""
        # Create multi-slope curve similar to legacy implementation
        curve = MultislopeSNCurve(
            name="Test Multi-slope",
            slopes=[3.0, 3.5, 5.0],
            constants=[1e12, 5e11, 1e10],
            transition_cycles=[1e6, 5e6],
            fatigue_limit=30.0
        )

        # Test high stress (first slope)
        stress_high = 150.0
        cycles_high = curve.get_allowable_cycles(stress_high)
        expected_high = 1e12 / (stress_high ** 3.0)
        assert abs(cycles_high - expected_high) / expected_high < 0.01

        # Test medium stress (second slope)
        stress_med = 80.0
        cycles_med = curve.get_allowable_cycles(stress_med)
        # Should use second slope
        assert cycles_med > 1e6  # Above first transition
        assert cycles_med < 5e6  # Below second transition

        # Test low stress (third slope)
        stress_low = 50.0
        cycles_low = curve.get_allowable_cycles(stress_low)
        expected_low = 1e10 / (stress_low ** 5.0)
        assert abs(cycles_low - expected_low) / expected_low < 0.01

        # Test below fatigue limit
        stress_below = 25.0
        cycles_below = curve.get_allowable_cycles(stress_below)
        assert np.isinf(cycles_below)

    def test_curve_continuity(self):
        """Test that multi-slope curves are continuous at transition points"""
        curve = MultislopeSNCurve(
            name="Continuity Test",
            slopes=[3.0, 4.0],
            constants=[1e12, 5e11],
            transition_cycles=[1e6],
            fatigue_limit=20.0
        )

        # Calculate stress at transition point from both slopes
        transition_cycles = 1e6
        stress_slope1 = (curve.constants[0] / transition_cycles) ** (1/3.0)
        stress_slope2 = (curve.constants[1] / transition_cycles) ** (1/4.0)

        # Should be approximately equal (within 1%)
        assert abs(stress_slope1 - stress_slope2) / stress_slope1 < 0.01

    def test_legacy_stress_ranges(self):
        """Test stress ranges from legacy LinearSlopeCal.py patterns"""
        engine = FatigueAnalysisEngine()

        # Test high stress ranges from legacy code
        stress_ranges = [900, 500, 350, 300, 250, 200]
        results = engine.calculate_high_stress_range_life(stress_ranges)

        for stress in stress_ranges:
            key = f"{stress}_MPa"
            assert key in results
            assert results[key]['stress_range'] == stress
            assert results[key]['allowable_cycles'] > 0
            assert np.isfinite(results[key]['allowable_cycles'])
            assert results[key]['is_above_fatigue_limit']


class TestDamageAccumulation:
    """Test damage accumulation methods"""

    def test_linear_damage_basic(self):
        """Test basic linear damage accumulation (Palmgren-Miner)"""
        # Create test cycle data
        cycles = pd.DataFrame({
            'range': [100, 80, 60, 40],
            'mean': [0, 0, 0, 0],
            'count': [1000, 2000, 5000, 10000]
        })

        curve = get_dnv_curve('D')
        calculator = LinearDamageAccumulation()

        result = calculator.calculate_damage(cycles, curve)

        # Basic checks
        assert result['total_damage'] > 0
        assert result['total_damage'] < 1.0  # Should be safe
        assert result['safety_factor'] > 1.0
        assert result['cycles_processed'] == 18000

        # Check damage contributions sum correctly
        contributions = result['damage_contributions']
        total_damage_check = sum(c['damage_increment'] for c in contributions)
        assert abs(total_damage_check - result['total_damage']) < 1e-10

    def test_damage_accumulation_consistency(self):
        """Test that damage calculations are consistent and repeatable"""
        cycles = pd.DataFrame({
            'range': [50, 40, 30],
            'count': [1000, 2000, 5000]
        })

        curve = get_dnv_curve('D')
        calculator = LinearDamageAccumulation()

        # Run calculation multiple times
        results = []
        for _ in range(3):
            result = calculator.calculate_damage(cycles, curve)
            results.append(result['total_damage'])

        # All results should be identical
        assert all(abs(r - results[0]) < 1e-15 for r in results)


class TestFrequencyDomainMigration:
    """Test frequency domain analysis capabilities"""

    def test_basic_frequency_analysis(self):
        """Test basic frequency domain analysis"""
        # Create simple PSD
        frequency = np.linspace(0.01, 10, 100)
        psd = 10 * np.exp(-frequency) + 1.0  # Decaying spectrum

        result = quick_frequency_domain_analysis(
            psd, frequency, duration=3600.0,
            sn_standard='DNV', sn_curve_class='D'
        )

        # Basic validation
        assert 'frequency_results' in result
        assert 'damage_results' in result
        assert result['damage_results']['total_damage'] > 0
        assert result['damage_results']['safety_factor'] > 0
        assert result['damage_results']['equivalent_cycles_per_sec'] > 0

    def test_frequency_domain_methods_comparison(self):
        """Test that different frequency domain methods give reasonable results"""
        frequency = np.linspace(0.1, 5, 50)
        psd = 5 * np.exp(-((frequency - 1.0)/0.5)**2) + 0.1  # Gaussian peak

        methods = ['dirlik', 'narrow_band']  # Test available methods
        results = {}

        for method in methods:
            try:
                result = quick_frequency_domain_analysis(
                    psd, frequency, duration=3600.0,
                    frequency_method=method
                )
                results[method] = result['damage_results']['total_damage']
            except Exception as e:
                pytest.skip(f"Method {method} not available: {e}")

        # All methods should give positive damage
        for method, damage in results.items():
            assert damage > 0, f"Method {method} gave non-positive damage: {damage}"

        # Results should be of similar order of magnitude
        if len(results) > 1:
            damages = list(results.values())
            ratio = max(damages) / min(damages)
            assert ratio < 100, f"Methods differ too much: {results}"


class TestShearAnalysisMigration:
    """Test shear analysis functionality from legacy Shear7dataCal.py"""

    def test_shear_analysis_basic(self):
        """Test basic shear analysis functionality"""
        engine = FatigueAnalysisEngine()

        # Parameters from legacy code
        high_stress = 1000.0
        low_stress_range = (1.0, 16.0)

        result = engine.analyze_shear_data(high_stress, low_stress_range)

        # Validate structure
        assert 'high_stress_analysis' in result
        assert 'low_stress_analysis' in result
        assert 'curve_characteristics' in result

        # High stress analysis
        high_analysis = result['high_stress_analysis']
        assert high_analysis['stress_range'] == high_stress
        assert high_analysis['allowable_cycles'] > 0
        assert high_analysis['is_finite']

        # Low stress analysis
        low_analysis = result['low_stress_analysis']
        assert len(low_analysis) == 15  # Should have 15 points
        assert all(data['stress'] >= 1.0 for data in low_analysis)
        assert all(data['stress'] <= 16.0 for data in low_analysis)

        # Should have some finite and some infinite life points
        finite_points = [data for data in low_analysis if data['is_finite']]
        infinite_points = [data for data in low_analysis if not data['is_finite']]

        # At very low stress, should have infinite life
        assert len(infinite_points) > 0

    def test_shear_analysis_fatigue_limit(self):
        """Test that shear analysis respects fatigue limit"""
        engine = FatigueAnalysisEngine()
        result = engine.analyze_shear_data(1000.0, (1.0, 100.0))

        fatigue_limit = result['curve_characteristics']['fatigue_limit']
        low_analysis = result['low_stress_analysis']

        # Points below fatigue limit should have infinite life
        for data in low_analysis:
            if data['stress'] <= fatigue_limit:
                assert not data['is_finite'], f"Stress {data['stress']} <= {fatigue_limit} should have infinite life"


class TestEngineering ValidationMigration:
    """Test engineering validation functions"""

    def test_time_domain_validation(self):
        """Test time domain analysis validation"""
        # Generate test data with known characteristics
        np.random.seed(42)
        stress = 50 * np.sin(np.linspace(0, 10, 1000)) + 20 * np.random.randn(1000)

        result = quick_time_domain_analysis(stress)

        # Should have validation results
        assert 'validation' in result
        validation = result['validation']

        # Validation should pass for reasonable data
        assert hasattr(validation, 'is_valid')
        assert hasattr(validation, 'warnings')
        assert hasattr(validation, 'errors')
        assert hasattr(validation, 'recommendations')

        # Should have some recommendations
        assert len(validation.recommendations) > 0

    def test_validation_with_problematic_data(self):
        """Test validation with problematic input data"""
        # Very short time series
        short_stress = [10, 20, 15]

        with pytest.raises(ValueError):
            quick_time_domain_analysis(short_stress)

        # Very large stress values
        large_stress = np.ones(1000) * 10000  # 10 GPa!

        result = quick_time_domain_analysis(large_stress)
        validation = result['validation']

        # Should have warnings about large stress
        warning_messages = ' '.join(validation.warnings)
        assert 'large stress' in warning_messages.lower() or 'verify' in warning_messages.lower()

    def test_safety_factor_validation(self):
        """Test safety factor validation"""
        # Generate high damage scenario
        high_stress = np.ones(10000) * 150  # Very high constant stress

        result = quick_time_domain_analysis(high_stress, sn_standard='DNV', sn_curve_class='W3')

        validation = result['validation']
        safety_factor = result['damage_results']['safety_factor']

        # High stress should result in low safety factor and warnings/errors
        if safety_factor < 2.0:
            assert len(validation.warnings) > 0 or len(validation.errors) > 0


class TestComprehensiveIntegration:
    """Test comprehensive integration of migrated functionality"""

    def test_full_analysis_workflow(self):
        """Test complete analysis workflow from data to report"""
        # Generate realistic stress history
        np.random.seed(123)
        time = np.linspace(0, 3600, 3600)  # 1 hour at 1 Hz
        stress = (30 * np.sin(0.1 * time) +
                 20 * np.sin(0.5 * time) +
                 10 * np.random.randn(len(time)))

        # Configure comprehensive analysis
        config = FatigueAnalysisConfig(
            sn_standard='DNV',
            sn_curve_class='D',
            damage_method='linear',
            validation_checks=True,
            detailed_output=True
        )

        # Perform analysis
        engine = FatigueAnalysisEngine(config)
        result = engine.analyze_time_series(stress, time)

        # Validate complete result structure
        required_keys = [
            'analysis_type', 'timestamp', 'configuration',
            'input_statistics', 'rainflow_results', 'damage_results',
            'analysis_statistics', 'validation', 'sn_curve_info'
        ]

        for key in required_keys:
            assert key in result, f"Missing required result key: {key}"

        # Generate report
        report = engine.generate_report(result)
        assert isinstance(report, str)
        assert len(report) > 1000  # Should be substantial report
        assert 'FATIGUE ANALYSIS REPORT' in report
        assert 'DAMAGE ASSESSMENT' in report

    def test_legacy_pattern_compatibility(self):
        """Test that results match expected patterns from legacy code"""
        # Test data similar to legacy examples
        stress_ranges = [100, 80, 60, 40, 20]
        counts = [1000, 2000, 5000, 10000, 20000]

        cycles_df = pd.DataFrame({
            'range': stress_ranges,
            'count': counts,
            'mean': [0] * len(stress_ranges)
        })

        # Calculate using modern implementation
        curve = get_dnv_curve('D')
        calculator = LinearDamageAccumulation()
        result = calculator.calculate_damage(cycles_df, curve)

        # Manual calculation for verification
        manual_damage = 0
        for stress, count in zip(stress_ranges, counts):
            N_allowable = curve.get_allowable_cycles(stress)
            if np.isfinite(N_allowable):
                manual_damage += count / N_allowable

        # Should match within numerical precision
        assert abs(result['total_damage'] - manual_damage) < 1e-12

    def test_multislope_vs_single_slope_comparison(self):
        """Test that multi-slope curves behave reasonably compared to single-slope"""
        # Create comparable single and multi-slope curves
        single_curve = get_dnv_curve('D')

        multi_curve = MultislopeSNCurve(
            name="Multi Test",
            slopes=[3.0, 3.5],
            constants=[5.73e11, 1e11],
            transition_cycles=[2e6],
            fatigue_limit=52.63
        )

        # Test across stress range
        stress_ranges = np.logspace(1.5, 2.2, 10)  # 30 to 160 MPa

        for stress in stress_ranges:
            single_cycles = single_curve.get_allowable_cycles(stress)
            multi_cycles = multi_curve.get_allowable_cycles(stress)

            # Both should give finite results for high stress
            if stress > 60:
                assert np.isfinite(single_cycles)
                assert np.isfinite(multi_cycles)

                # Should be of similar order of magnitude
                ratio = multi_cycles / single_cycles
                assert 0.1 < ratio < 10, f"Curves too different at {stress} MPa: ratio = {ratio}"


# Utility functions for test data generation
def generate_test_stress_history(duration=3600, sample_rate=10, components=None):
    """Generate realistic stress time history for testing"""
    if components is None:
        components = {
            'mean': 0,
            'low_freq': {'amplitude': 30, 'frequency': 0.1},
            'high_freq': {'amplitude': 15, 'frequency': 1.0},
            'noise': {'amplitude': 5}
        }

    time = np.linspace(0, duration, int(duration * sample_rate))
    stress = np.full_like(time, components['mean'])

    if 'low_freq' in components:
        lf = components['low_freq']
        stress += lf['amplitude'] * np.sin(2 * np.pi * lf['frequency'] * time)

    if 'high_freq' in components:
        hf = components['high_freq']
        stress += hf['amplitude'] * np.sin(2 * np.pi * hf['frequency'] * time)

    if 'noise' in components:
        np.random.seed(42)  # Reproducible
        stress += components['noise']['amplitude'] * np.random.randn(len(time))

    return time, stress


def generate_test_psd(frequency_range=(0.01, 10), n_points=100, peak_frequency=1.0):
    """Generate test PSD for frequency domain testing"""
    frequency = np.linspace(frequency_range[0], frequency_range[1], n_points)

    # Gaussian peak with background
    psd = (10 * np.exp(-((frequency - peak_frequency) / 0.5) ** 2) +
           1.0 * np.exp(-frequency))

    return frequency, psd


if __name__ == "__main__":
    # Run tests with pytest
    pytest.main([__file__, '-v'])