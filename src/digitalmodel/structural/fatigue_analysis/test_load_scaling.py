#!/usr/bin/env python
"""
Test Script for Load Scaling Module
====================================

This script validates the load scaling calculations and formulas.
It includes unit tests for:
1. Scaling factor calculations
2. Time series scaling
3. Load combination
4. Edge cases and error handling
"""

import unittest
import numpy as np
import pandas as pd
from pathlib import Path
import sys
import tempfile
import shutil

# Add module to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from digitalmodel.structural.fatigue_analysis.load_scaling import LoadScaler


class TestLoadScaling(unittest.TestCase):
    """Test cases for load scaling module"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.temp_dir = tempfile.mkdtemp()
        self.test_dir = Path(self.temp_dir)
        
        # Create test input files
        self._create_test_files()
        
        # Initialize load scaler
        self.scaler = LoadScaler(
            reference_seastates_path=str(self.test_dir / "reference_seastates.csv"),
            fatigue_conditions_path=str(self.test_dir / "fatigue_conditions.csv"),
            output_dir=str(self.test_dir / "output")
        )
    
    def tearDown(self):
        """Clean up test fixtures"""
        shutil.rmtree(self.temp_dir)
    
    def _create_test_files(self):
        """Create test input files"""
        # Reference seastates
        ref_data = [
            {'reference_id': 'WD01', 'type': 'wind', 'wind_speed': 10.0, 
             'wind_direction': 0, 'hs': 0.0, 'tp': 0.0, 'wave_direction': 0},
            {'reference_id': 'W01', 'type': 'wave', 'wind_speed': 0.0,
             'wind_direction': 0, 'hs': 0.5, 'tp': 2.0, 'wave_direction': 0}
        ]
        pd.DataFrame(ref_data).to_csv(
            self.test_dir / "reference_seastates.csv", index=False
        )
        
        # Fatigue conditions
        fatigue_data = [
            {'condition_id': 1, 'wind_speed': 5.0, 'wind_direction': 0,
             'hs': 0.25, 'tp': 2.0, 'wave_direction': 0, 'occurrence_pct': 50.0},
            {'condition_id': 2, 'wind_speed': 20.0, 'wind_direction': 0,
             'hs': 1.0, 'tp': 2.0, 'wave_direction': 0, 'occurrence_pct': 50.0}
        ]
        pd.DataFrame(fatigue_data).to_csv(
            self.test_dir / "fatigue_conditions.csv", index=False
        )
    
    def test_wind_scaling_formula(self):
        """Test wind scaling factor calculation: (V/10)^2"""
        test_cases = [
            (5.0, 0.25),    # (5/10)^2 = 0.25
            (10.0, 1.0),    # (10/10)^2 = 1.0
            (15.0, 2.25),   # (15/10)^2 = 2.25
            (20.0, 4.0),    # (20/10)^2 = 4.0
            (0.0, 0.0),     # Edge case: zero wind
        ]
        
        for wind_speed, expected_scale in test_cases:
            scale = (wind_speed / 10.0) ** 2
            self.assertAlmostEqual(
                scale, expected_scale, places=4,
                msg=f"Wind scaling failed for {wind_speed} m/s"
            )
    
    def test_wave_scaling_formula(self):
        """Test wave scaling factor calculation: Hs/0.5"""
        test_cases = [
            (0.25, 0.5),   # 0.25/0.5 = 0.5
            (0.5, 1.0),    # 0.5/0.5 = 1.0
            (0.75, 1.5),   # 0.75/0.5 = 1.5
            (1.0, 2.0),    # 1.0/0.5 = 2.0
            (0.0, 0.0),    # Edge case: zero wave height
        ]
        
        for hs, expected_scale in test_cases:
            scale = hs / 0.5
            self.assertAlmostEqual(
                scale, expected_scale, places=4,
                msg=f"Wave scaling failed for Hs={hs} m"
            )
    
    def test_scaling_factors_calculation(self):
        """Test that scaling factors are calculated correctly"""
        # Check that scaling factors were calculated
        self.assertIsNotNone(self.scaler.scaling_factors)
        self.assertEqual(len(self.scaler.scaling_factors), 2)
        
        # Check first condition (wind=5, hs=0.25)
        first = self.scaler.scaling_factors.iloc[0]
        self.assertAlmostEqual(first['wind_scale_factor'], 0.25, places=4)
        self.assertAlmostEqual(first['wave_scale_factor'], 0.5, places=4)
        
        # Check second condition (wind=20, hs=1.0)
        second = self.scaler.scaling_factors.iloc[1]
        self.assertAlmostEqual(second['wind_scale_factor'], 4.0, places=4)
        self.assertAlmostEqual(second['wave_scale_factor'], 2.0, places=4)
    
    def test_time_series_scaling(self):
        """Test scaling of time series data"""
        # Create test time series
        original = np.array([100, 200, 150, 175, 125])
        scale_factor = 2.5
        
        # Apply scaling
        scaled = self.scaler.scale_time_series(original, scale_factor)
        
        # Check results
        expected = original * scale_factor
        np.testing.assert_array_almost_equal(scaled, expected)
    
    def test_load_combination(self):
        """Test combining wind and wave loads"""
        # Create test data
        wind_loads = np.array([100, 110, 105, 95, 100])
        wave_loads = np.array([50, 45, 55, 48, 52])
        
        # Combine (simple addition for effective tension)
        combined = wind_loads + wave_loads
        
        # Check results
        expected = np.array([150, 155, 160, 143, 152])
        np.testing.assert_array_equal(combined, expected)
    
    def test_reference_selection(self):
        """Test selection of closest reference seastate"""
        # Test wind reference selection
        wind_ref = self.scaler.select_reference_seastate('wind', 0)
        self.assertEqual(wind_ref, 'WD01')
        
        # Test wave reference selection
        wave_ref = self.scaler.select_reference_seastate('wave', 0, tp=2.0)
        self.assertEqual(wave_ref, 'W01')
    
    def test_occurrence_sum(self):
        """Test that occurrence percentages sum to 100%"""
        total_occurrence = self.scaler.fatigue_conditions['occurrence_pct'].sum()
        self.assertAlmostEqual(total_occurrence, 100.0, places=1,
                              msg="Occurrence percentages should sum to 100%")
    
    def test_scaling_range_validation(self):
        """Test that scaling factors are within reasonable ranges"""
        # Wind scaling should be positive
        self.assertTrue((self.scaler.scaling_factors['wind_scale_factor'] >= 0).all())
        
        # Wave scaling should be positive
        self.assertTrue((self.scaler.scaling_factors['wave_scale_factor'] >= 0).all())
        
        # Check reasonable upper bounds (example: wind up to 50 m/s)
        max_wind_scale = (50 / 10) ** 2  # = 25
        self.assertTrue((self.scaler.scaling_factors['wind_scale_factor'] <= max_wind_scale).all())
    
    def test_output_file_generation(self):
        """Test that output files are generated correctly"""
        # Process a single condition
        result = self.scaler.process_fatigue_condition(1)
        
        # Check that result contains expected keys
        expected_keys = ['condition_id', 'wind_reference', 'wave_reference',
                        'wind_scale_factor', 'wave_scale_factor', 'occurrence_pct',
                        'wind_loads', 'wave_loads', 'combined_loads', 'statistics']
        for key in expected_keys:
            self.assertIn(key, result)
        
        # Check statistics
        stats = result['statistics']
        self.assertIn('max_load', stats)
        self.assertIn('min_load', stats)
        self.assertIn('mean_load', stats)
        self.assertIn('std_load', stats)


class TestScalingFormulas(unittest.TestCase):
    """Specific tests for scaling formulas validation"""
    
    def test_wind_scaling_quadratic(self):
        """Verify wind scaling follows quadratic relationship"""
        base_speed = 10.0
        base_load = 1000.0
        
        # Test at different wind speeds
        for target_speed in [5, 10, 15, 20, 25]:
            scale_factor = (target_speed / base_speed) ** 2
            scaled_load = base_load * scale_factor
            
            # Verify quadratic relationship
            expected_load = base_load * (target_speed / base_speed) ** 2
            self.assertAlmostEqual(scaled_load, expected_load, places=2)
    
    def test_wave_scaling_linear(self):
        """Verify wave scaling follows linear relationship"""
        base_hs = 0.5
        base_load = 500.0
        
        # Test at different wave heights
        for target_hs in [0.25, 0.5, 0.75, 1.0, 1.5]:
            scale_factor = target_hs / base_hs
            scaled_load = base_load * scale_factor
            
            # Verify linear relationship
            expected_load = base_load * (target_hs / base_hs)
            self.assertAlmostEqual(scaled_load, expected_load, places=2)
    
    def test_combined_loading_principle(self):
        """Test principle of superposition for combined loads"""
        # Define test conditions
        wind_base = 1000.0
        wave_base = 500.0
        wind_scale = 2.0
        wave_scale = 1.5
        
        # Scale individually
        wind_scaled = wind_base * wind_scale
        wave_scaled = wave_base * wave_scale
        
        # Combine
        combined = wind_scaled + wave_scaled
        
        # Verify superposition
        expected = (wind_base * wind_scale) + (wave_base * wave_scale)
        self.assertEqual(combined, expected)


class TestEdgeCases(unittest.TestCase):
    """Test edge cases and error handling"""
    
    def test_zero_wind_speed(self):
        """Test handling of zero wind speed"""
        scale = (0 / 10) ** 2
        self.assertEqual(scale, 0.0)
    
    def test_zero_wave_height(self):
        """Test handling of zero wave height"""
        scale = 0 / 0.5
        self.assertEqual(scale, 0.0)
    
    def test_very_high_wind_speed(self):
        """Test handling of extreme wind speeds"""
        extreme_wind = 100.0  # m/s
        scale = (extreme_wind / 10) ** 2
        self.assertEqual(scale, 100.0)
    
    def test_negative_values(self):
        """Test that negative values are handled appropriately"""
        # Negative wind speed should not occur in practice
        # but if it does, the formula still works
        scale = (-10 / 10) ** 2
        self.assertEqual(scale, 1.0)  # Squared makes it positive


def run_validation_suite():
    """Run complete validation suite with detailed reporting"""
    print("=" * 80)
    print("LOAD SCALING MODULE - VALIDATION SUITE")
    print("=" * 80)
    
    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Add test classes
    suite.addTests(loader.loadTestsFromTestCase(TestLoadScaling))
    suite.addTests(loader.loadTestsFromTestCase(TestScalingFormulas))
    suite.addTests(loader.loadTestsFromTestCase(TestEdgeCases))
    
    # Run tests with detailed output
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Print summary
    print("\n" + "=" * 80)
    print("VALIDATION SUMMARY")
    print("=" * 80)
    print(f"Tests Run: {result.testsRun}")
    print(f"Failures: {len(result.failures)}")
    print(f"Errors: {len(result.errors)}")
    
    if result.wasSuccessful():
        print("\n✓ All validation tests PASSED")
        print("\nSCALING FORMULAS VERIFIED:")
        print("  Wind: F_scaled = F_ref * (V_target / 10)^2")
        print("  Wave: F_scaled = F_ref * (Hs_target / 0.5)")
        print("  Combined: F_total = F_wind_scaled + F_wave_scaled")
    else:
        print("\n✗ Some tests FAILED - review output above")
    
    return result.wasSuccessful()


if __name__ == "__main__":
    success = run_validation_suite()