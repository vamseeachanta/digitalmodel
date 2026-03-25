"""
Unit tests for plate capacity analysis modules.

This module provides comprehensive testing for the plate buckling analysis functionality,
ensuring correctness of calculations and proper error handling.

Author: Generated for migrated plate capacity modules
Date: 2025-01-15
"""

import unittest
import math
import sys
from pathlib import Path
import tempfile
import os

# Add src directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from digitalmodel.analysis.plate_capacity import (
    PlateProperties, AppliedLoads, BucklingConstants, BucklingResults,
    PlateBucklingAnalyzer, BoundaryCondition, create_plate_from_legacy_data
)
from digitalmodel.analysis.multi_plate_analyzer import (
    MultiPlateAnalyzer, PlateConfiguration, create_legacy_multi_plate_analyzer
)
from digitalmodel.calculations.plate_buckling import (
    ElasticBucklingCalculator, SlendernessCalculator, UltimateStrengthCalculator,
    UsageFactorCalculator, BucklingCoefficients, PlateEdgeCondition,
    calculate_plate_buckling_212
)


class TestPlateProperties(unittest.TestCase):
    """Test cases for PlateProperties data class."""

    def test_valid_plate_properties(self):
        """Test creation of valid plate properties."""
        props = PlateProperties(
            length=3.0,
            breadth=0.8,
            thickness=0.012,
            youngs_modulus=210e9,
            poisson_ratio=0.3,
            yield_strength=355e6
        )

        self.assertEqual(props.length, 3.0)
        self.assertEqual(props.breadth, 0.8)
        self.assertEqual(props.thickness, 0.012)
        self.assertEqual(props.youngs_modulus, 210e9)
        self.assertEqual(props.poisson_ratio, 0.3)
        self.assertEqual(props.yield_strength, 355e6)

    def test_invalid_plate_properties(self):
        """Test validation of plate properties."""
        # Negative length
        with self.assertRaises(ValueError):
            PlateProperties(
                length=-1.0,
                breadth=0.8,
                thickness=0.012,
                youngs_modulus=210e9
            )

        # Zero thickness
        with self.assertRaises(ValueError):
            PlateProperties(
                length=3.0,
                breadth=0.8,
                thickness=0.0,
                youngs_modulus=210e9
            )

        # Invalid Poisson's ratio
        with self.assertRaises(ValueError):
            PlateProperties(
                length=3.0,
                breadth=0.8,
                thickness=0.012,
                youngs_modulus=210e9,
                poisson_ratio=0.8  # Too high
            )

        # Negative Young's modulus
        with self.assertRaises(ValueError):
            PlateProperties(
                length=3.0,
                breadth=0.8,
                thickness=0.012,
                youngs_modulus=-210e9
            )


class TestAppliedLoads(unittest.TestCase):
    """Test cases for AppliedLoads data class."""

    def test_applied_loads_creation(self):
        """Test creation of applied loads."""
        loads = AppliedLoads(
            longitudinal_stress=50e6,
            transverse_stress=30e6,
            shear_stress=20e6
        )

        self.assertEqual(loads.longitudinal_stress, 50e6)
        self.assertEqual(loads.transverse_stress, 30e6)
        self.assertEqual(loads.shear_stress, 20e6)

    def test_zero_loads(self):
        """Test with zero loads."""
        loads = AppliedLoads()
        self.assertEqual(loads.longitudinal_stress, 0.0)
        self.assertEqual(loads.transverse_stress, 0.0)
        self.assertEqual(loads.shear_stress, 0.0)


class TestElasticBucklingCalculator(unittest.TestCase):
    """Test cases for ElasticBucklingCalculator."""

    def setUp(self):
        """Set up test fixtures."""
        self.calc = ElasticBucklingCalculator()
        self.E = 210e9  # Pa
        self.v = 0.3
        self.t = 0.012  # m
        self.b = 0.8    # m
        self.l = 3.0    # m

    def test_base_factor_calculation(self):
        """Test base factor calculation."""
        base_factor = self.calc.calculate_base_factor(self.E, self.v, self.t, self.b)

        # Expected value
        expected = (math.pi**2 * self.E) / (12 * (1 - self.v**2)) * (self.t/self.b)**2
        self.assertAlmostEqual(base_factor, expected, places=6)

    def test_base_factor_validation(self):
        """Test base factor validation."""
        # Zero Young's modulus
        with self.assertRaises(ValueError):
            self.calc.calculate_base_factor(0, self.v, self.t, self.b)

        # Invalid Poisson's ratio
        with self.assertRaises(ValueError):
            self.calc.calculate_base_factor(self.E, 0.6, self.t, self.b)

        # Zero thickness
        with self.assertRaises(ValueError):
            self.calc.calculate_base_factor(self.E, self.v, 0, self.b)

    def test_longitudinal_buckling_stress(self):
        """Test longitudinal buckling stress calculation."""
        stress = self.calc.calculate_longitudinal_buckling_stress(
            self.E, self.v, self.t, self.b, self.l
        )

        # Should be positive
        self.assertGreater(stress, 0)

        # Test with different boundary conditions
        stress_clamped = self.calc.calculate_longitudinal_buckling_stress(
            self.E, self.v, self.t, self.b, self.l, PlateEdgeCondition.CLAMPED
        )

        # Clamped edges should give higher buckling stress
        self.assertGreater(stress_clamped, stress)

    def test_transverse_buckling_stress(self):
        """Test transverse buckling stress calculation."""
        stress = self.calc.calculate_transverse_buckling_stress(
            self.E, self.v, self.t, self.b, self.l
        )

        # Should be positive
        self.assertGreater(stress, 0)

    def test_shear_buckling_stress(self):
        """Test shear buckling stress calculation."""
        stress = self.calc.calculate_shear_buckling_stress(
            self.E, self.v, self.t, self.b, self.l
        )

        # Should be positive
        self.assertGreater(stress, 0)

    def test_all_elastic_stresses(self):
        """Test calculation of all elastic buckling stresses."""
        stresses = self.calc.calculate_all_elastic_buckling_stresses(
            self.E, self.v, self.t, self.b, self.l
        )

        # Check all stress types are present and positive
        self.assertIn('longitudinal', stresses)
        self.assertIn('transverse', stresses)
        self.assertIn('shear', stresses)

        for stress in stresses.values():
            self.assertGreater(stress, 0)


class TestSlendernessCalculator(unittest.TestCase):
    """Test cases for SlendernessCalculator."""

    def test_slenderness_ratio_calculation(self):
        """Test slenderness ratio calculation."""
        char_resistance = 355e6  # Pa
        elastic_stress = 500e6   # Pa

        slenderness = SlendernessCalculator.calculate_slenderness_ratio(
            char_resistance, elastic_stress
        )

        expected = math.sqrt(char_resistance / elastic_stress)
        self.assertAlmostEqual(slenderness, expected, places=6)

    def test_slenderness_zero_elastic_stress(self):
        """Test slenderness calculation with zero elastic stress."""
        slenderness = SlendernessCalculator.calculate_slenderness_ratio(355e6, 0)
        self.assertEqual(slenderness, 0.0)

    def test_equivalent_slenderness(self):
        """Test equivalent slenderness ratio calculation."""
        applied_stresses = {
            'longitudinal': 50e6,
            'transverse': 30e6,
            'shear': 20e6
        }

        elastic_stresses = {
            'longitudinal': 400e6,
            'transverse': 300e6,
            'shear': 200e6
        }

        lambda_eq = SlendernessCalculator.calculate_equivalent_slenderness(
            applied_stresses, elastic_stresses, 355e6, 60e6
        )

        # Should be positive
        self.assertGreater(lambda_eq, 0)


class TestUltimateStrengthCalculator(unittest.TestCase):
    """Test cases for UltimateStrengthCalculator."""

    def setUp(self):
        """Set up test fixtures."""
        self.fy = 355e6  # Pa
        self.E = 210e9   # Pa
        self.t = 0.012   # m
        self.b = 0.8     # m
        self.l = 3.0     # m

    def test_serviceability_resistance(self):
        """Test serviceability resistance calculation."""
        slenderness = 0.8

        resistance = UltimateStrengthCalculator.calculate_characteristic_resistance_serviceability(
            self.fy, slenderness
        )

        # Should be less than or equal to yield strength
        self.assertLessEqual(resistance, self.fy)
        self.assertGreater(resistance, 0)

    def test_ultimate_resistance(self):
        """Test ultimate resistance calculation."""
        # Test low slenderness (yielding governs)
        slenderness_low = 0.5
        resistance_low = UltimateStrengthCalculator.calculate_characteristic_resistance_ultimate(
            self.fy, slenderness_low
        )

        # Test high slenderness (buckling governs)
        slenderness_high = 1.5
        resistance_high = UltimateStrengthCalculator.calculate_characteristic_resistance_ultimate(
            self.fy, slenderness_high
        )

        # Both should be positive but high slenderness should give lower resistance
        self.assertGreater(resistance_low, 0)
        self.assertGreater(resistance_high, 0)
        self.assertGreater(resistance_low, resistance_high)

    def test_dnv_longitudinal_resistance(self):
        """Test DNV longitudinal resistance calculation."""
        resistance = UltimateStrengthCalculator.calculate_dnv_longitudinal_resistance(
            self.fy, self.E, self.t, self.b
        )

        # Should be positive and less than yield strength
        self.assertGreater(resistance, 0)
        self.assertLessEqual(resistance, self.fy)

    def test_dnv_transverse_resistance(self):
        """Test DNV transverse resistance calculation."""
        resistance = UltimateStrengthCalculator.calculate_dnv_transverse_resistance(
            self.fy, self.E, self.t, self.b, self.l
        )

        # Should be positive
        self.assertGreater(resistance, 0)

    def test_dnv_shear_resistance(self):
        """Test DNV shear resistance calculation."""
        resistance = UltimateStrengthCalculator.calculate_dnv_shear_resistance(
            self.fy, self.E, self.t, self.b, self.l
        )

        # Should be positive and approximately fy/sqrt(3)
        self.assertGreater(resistance, 0)
        self.assertLess(resistance, self.fy/math.sqrt(3))


class TestUsageFactorCalculator(unittest.TestCase):
    """Test cases for UsageFactorCalculator."""

    def test_basic_usage_factor(self):
        """Test basic usage factor calculation."""
        applied = 100e6
        allowable = 200e6

        usage = UsageFactorCalculator.calculate_usage_factor(applied, allowable)
        self.assertAlmostEqual(usage, 0.5, places=6)

    def test_usage_factor_zero_allowable(self):
        """Test usage factor with zero allowable stress."""
        usage = UsageFactorCalculator.calculate_usage_factor(100e6, 0)
        self.assertEqual(usage, float('inf'))

    def test_biaxial_usage_factor(self):
        """Test biaxial usage factor calculation."""
        usage = UsageFactorCalculator.calculate_biaxial_usage_factor(
            sigma_x=50e6, sigma_y=30e6, tau_xy=20e6,
            sigma_xrd=200e6, sigma_yrd=180e6, tau_rd=120e6
        )

        # Should be positive
        self.assertGreater(usage, 0)

    def test_all_usage_factors(self):
        """Test calculation of all usage factors."""
        applied_stresses = {
            'longitudinal': 50e6,
            'transverse': 30e6,
            'shear': 20e6
        }

        design_resistances = {
            'longitudinal': 200e6,
            'transverse': 180e6,
            'shear': 120e6
        }

        usage_factors = UsageFactorCalculator.calculate_all_usage_factors(
            applied_stresses, design_resistances
        )

        # Check all factors are present and reasonable
        self.assertIn('longitudinal', usage_factors)
        self.assertIn('transverse', usage_factors)
        self.assertIn('shear', usage_factors)
        self.assertIn('biaxial', usage_factors)

        for factor in usage_factors.values():
            self.assertGreater(factor, 0)
            self.assertLess(factor, 10)  # Reasonable upper bound


class TestPlateBucklingAnalyzer(unittest.TestCase):
    """Test cases for PlateBucklingAnalyzer."""

    def setUp(self):
        """Set up test fixtures."""
        self.plate_props = PlateProperties(
            length=3.0,
            breadth=0.8,
            thickness=0.012,
            youngs_modulus=210e9,
            poisson_ratio=0.3,
            yield_strength=355e6
        )

        self.applied_loads = AppliedLoads(
            longitudinal_stress=50e6,
            transverse_stress=30e6,
            shear_stress=20e6
        )

    def test_analyzer_initialization(self):
        """Test analyzer initialization."""
        analyzer = PlateBucklingAnalyzer(self.plate_props, self.applied_loads)

        self.assertEqual(analyzer.plate_props, self.plate_props)
        self.assertEqual(analyzer.applied_loads, self.applied_loads)

        # Check geometric ratios
        self.assertAlmostEqual(analyzer.aspect_ratio, 3.75, places=2)
        self.assertAlmostEqual(analyzer.slenderness_ratio, 66.67, places=1)

    def test_von_mises_calculation(self):
        """Test von Mises stress calculation."""
        analyzer = PlateBucklingAnalyzer(self.plate_props, self.applied_loads)
        von_mises = analyzer.calculate_von_mises_stress()

        # Calculate expected value
        sx, sy, txy = 50e6, 30e6, 20e6
        expected = math.sqrt(sx**2 + sy**2 - sx*sy + 3*txy**2)

        self.assertAlmostEqual(von_mises, expected, places=0)

    def test_complete_analysis(self):
        """Test complete plate buckling analysis."""
        analyzer = PlateBucklingAnalyzer(self.plate_props, self.applied_loads)
        results = analyzer.perform_analysis()

        # Check result structure
        self.assertIsInstance(results, BucklingResults)
        self.assertIsInstance(results.is_safe, bool)
        self.assertGreater(results.von_mises_stress, 0)

        # Check all usage factors are calculated
        self.assertGreaterEqual(results.usage_factor_longitudinal, 0)
        self.assertGreaterEqual(results.usage_factor_transverse, 0)
        self.assertGreaterEqual(results.usage_factor_shear, 0)
        self.assertGreaterEqual(results.usage_factor_biaxial, 0)

    def test_analysis_with_different_boundary_conditions(self):
        """Test analysis with different boundary conditions."""
        analyzer_simple = PlateBucklingAnalyzer(
            self.plate_props, self.applied_loads,
            boundary_condition=BoundaryCondition.SIMPLY_SUPPORTED
        )

        analyzer_clamped = PlateBucklingAnalyzer(
            self.plate_props, self.applied_loads,
            boundary_condition=BoundaryCondition.SIDES_CLAMPED
        )

        results_simple = analyzer_simple.perform_analysis()
        results_clamped = analyzer_clamped.perform_analysis()

        # Clamped conditions should generally give lower usage factors
        # (higher buckling resistance)
        self.assertLessEqual(
            results_clamped.usage_factor_longitudinal,
            results_simple.usage_factor_longitudinal
        )


class TestMultiPlateAnalyzer(unittest.TestCase):
    """Test cases for MultiPlateAnalyzer."""

    def setUp(self):
        """Set up test fixtures."""
        self.analyzer = MultiPlateAnalyzer()

        # Create test plate configurations
        self.plate1_props = PlateProperties(
            length=3.0, breadth=0.8, thickness=0.012,
            youngs_modulus=210e9, yield_strength=355e6
        )

        self.plate1_loads = AppliedLoads(
            longitudinal_stress=50e6, transverse_stress=30e6, shear_stress=20e6
        )

        self.plate2_props = PlateProperties(
            length=2.5, breadth=0.7, thickness=0.010,
            youngs_modulus=210e9, yield_strength=275e6
        )

        self.plate2_loads = AppliedLoads(
            longitudinal_stress=80e6, transverse_stress=60e6, shear_stress=40e6
        )

    def test_add_plate(self):
        """Test adding plates to analyzer."""
        config1 = PlateConfiguration(
            plate_id="plate1",
            plate_properties=self.plate1_props,
            applied_loads=self.plate1_loads,
            buckling_constants=BucklingConstants()
        )

        self.analyzer.add_plate(config1)
        self.assertEqual(self.analyzer.get_plate_count(), 1)

        config2 = PlateConfiguration(
            plate_id="plate2",
            plate_properties=self.plate2_props,
            applied_loads=self.plate2_loads,
            buckling_constants=BucklingConstants()
        )

        self.analyzer.add_plate(config2)
        self.assertEqual(self.analyzer.get_plate_count(), 2)

    def test_remove_plate(self):
        """Test removing plates from analyzer."""
        config = PlateConfiguration(
            plate_id="test_plate",
            plate_properties=self.plate1_props,
            applied_loads=self.plate1_loads,
            buckling_constants=BucklingConstants()
        )

        self.analyzer.add_plate(config)
        self.assertEqual(self.analyzer.get_plate_count(), 1)

        removed = self.analyzer.remove_plate("test_plate")
        self.assertTrue(removed)
        self.assertEqual(self.analyzer.get_plate_count(), 0)

        # Try to remove non-existent plate
        removed = self.analyzer.remove_plate("non_existent")
        self.assertFalse(removed)

    def test_multi_plate_analysis(self):
        """Test multi-plate analysis."""
        # Add plates
        config1 = PlateConfiguration(
            plate_id="safe_plate",
            plate_properties=self.plate1_props,
            applied_loads=self.plate1_loads,
            buckling_constants=BucklingConstants()
        )

        config2 = PlateConfiguration(
            plate_id="stressed_plate",
            plate_properties=self.plate2_props,
            applied_loads=self.plate2_loads,
            buckling_constants=BucklingConstants()
        )

        self.analyzer.add_plate(config1)
        self.analyzer.add_plate(config2)

        # Run analysis
        results = self.analyzer.analyze_all_plates()

        # Check results structure
        self.assertEqual(len(results.plate_results), 2)
        self.assertIn('safe_plate', results.plate_results)
        self.assertIn('stressed_plate', results.plate_results)

        # Check summary statistics
        self.assertIn('max_usage_factor', results.summary_statistics)
        self.assertIn('total_plate_count', results.summary_statistics)
        self.assertEqual(results.summary_statistics['total_plate_count'], 2)

    def test_plate_ranking(self):
        """Test plate ranking functionality."""
        # Add plates
        config1 = PlateConfiguration("plate1", self.plate1_props, self.plate1_loads, BucklingConstants())
        config2 = PlateConfiguration("plate2", self.plate2_props, self.plate2_loads, BucklingConstants())

        self.analyzer.add_plate(config1)
        self.analyzer.add_plate(config2)

        # Run analysis
        self.analyzer.analyze_all_plates()

        # Get ranking
        ranking = self.analyzer.get_plate_ranking("usage_factor")

        self.assertEqual(len(ranking), 2)
        # Should be sorted by usage factor (descending)
        self.assertGreaterEqual(ranking[0][1], ranking[1][1])

    def test_export_functionality(self):
        """Test export functionality."""
        # Add a plate
        config = PlateConfiguration("test_plate", self.plate1_props, self.plate1_loads, BucklingConstants())
        self.analyzer.add_plate(config)

        # Run analysis
        self.analyzer.analyze_all_plates()

        # Test CSV export
        with tempfile.NamedTemporaryFile(mode='w', suffix='.csv', delete=False) as f:
            csv_file = f.name

        try:
            self.analyzer.export_results_to_csv(csv_file)
            self.assertTrue(os.path.exists(csv_file))

            # Check file has content
            with open(csv_file, 'r') as f:
                content = f.read()
                self.assertIn('Plate_ID', content)
                self.assertIn('test_plate', content)

        finally:
            if os.path.exists(csv_file):
                os.unlink(csv_file)

        # Test summary report export
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            report_file = f.name

        try:
            self.analyzer.export_summary_report(report_file)
            self.assertTrue(os.path.exists(report_file))

            # Check file has content
            with open(report_file, 'r') as f:
                content = f.read()
                self.assertIn('SUMMARY REPORT', content)

        finally:
            if os.path.exists(report_file):
                os.unlink(report_file)


class TestLegacyCompatibility(unittest.TestCase):
    """Test cases for legacy compatibility functions."""

    def test_create_plate_from_legacy_data(self):
        """Test creating plate from legacy data format."""
        legacy_data = {
            'PlateLength': 3.0,
            'PlateBreadth': 0.8,
            'PlateThickness': 0.012,
            'YoungsModulus': 210e9,
            'PoissionsRatio': 0.3,
            'YieldStrength': 355e6,
            'LongtudinalStress': 50e6,
            'TransverseStress': 30e6,
            'ShearStress': 20e6
        }

        plate_props, applied_loads, buckling_constants = create_plate_from_legacy_data(legacy_data)

        self.assertEqual(plate_props.length, 3.0)
        self.assertEqual(plate_props.breadth, 0.8)
        self.assertEqual(applied_loads.longitudinal_stress, 50e6)

    def test_calculate_plate_buckling_212(self):
        """Test legacy PlateBuckling_212 calculation."""
        plate_data = {
            'YoungsModulus': 30000000,      # psi
            'PoissionsRatio': 0.3,
            'constantvalueTable1': 0.425,
            'PlateThickness': 0.552,        # in
            'PlateBreadth': 27.6,           # in
            'PlateLength': 104.04,          # in
            'TangentModulus': 25000000,     # psi
            'YieldPoint': 33000             # psi
        }

        results = calculate_plate_buckling_212(plate_data)

        # Check that results are returned
        self.assertIn('critical_stress_edge_compression', results)
        self.assertIn('critical_stress_shear', results)

        # Check values are reasonable
        for key, value in results.items():
            if 'stress' in key:
                self.assertGreater(value, 0)

    def test_legacy_multi_plate_analyzer(self):
        """Test legacy multi-plate analyzer creation."""
        analyzer = create_legacy_multi_plate_analyzer()

        self.assertEqual(analyzer.get_plate_count(), 5)  # G, H, I, J, K plates

        # Run analysis
        results = analyzer.analyze_all_plates()
        self.assertEqual(len(results.plate_results), 5)


class TestIntegration(unittest.TestCase):
    """Integration tests for complete workflow."""

    def test_complete_workflow(self):
        """Test complete analysis workflow."""
        # 1. Create plate properties
        plate_props = PlateProperties(
            length=2.5, breadth=0.7, thickness=0.010,
            youngs_modulus=210e9, poisson_ratio=0.3, yield_strength=275e6
        )

        # 2. Define loads
        applied_loads = AppliedLoads(
            longitudinal_stress=60e6, transverse_stress=40e6, shear_stress=25e6
        )

        # 3. Run single plate analysis
        analyzer = PlateBucklingAnalyzer(plate_props, applied_loads)
        results = analyzer.perform_analysis()

        # 4. Verify results are reasonable
        self.assertIsInstance(results.is_safe, bool)
        self.assertGreater(results.von_mises_stress, 0)

        # 5. Test with different boundary conditions
        analyzer_clamped = PlateBucklingAnalyzer(
            plate_props, applied_loads,
            boundary_condition=BoundaryCondition.SIDES_CLAMPED
        )
        results_clamped = analyzer_clamped.perform_analysis()

        # Clamped should generally be safer (lower usage factors)
        max_usage_simple = max([
            results.usage_factor_longitudinal,
            results.usage_factor_transverse,
            results.usage_factor_shear,
            results.usage_factor_biaxial
        ])

        max_usage_clamped = max([
            results_clamped.usage_factor_longitudinal,
            results_clamped.usage_factor_transverse,
            results_clamped.usage_factor_shear,
            results_clamped.usage_factor_biaxial
        ])

        self.assertLessEqual(max_usage_clamped, max_usage_simple)


if __name__ == '__main__':
    # Create test suite
    test_suite = unittest.TestSuite()

    # Add test classes
    test_classes = [
        TestPlateProperties,
        TestAppliedLoads,
        TestElasticBucklingCalculator,
        TestSlendernessCalculator,
        TestUltimateStrengthCalculator,
        TestUsageFactorCalculator,
        TestPlateBucklingAnalyzer,
        TestMultiPlateAnalyzer,
        TestLegacyCompatibility,
        TestIntegration
    ]

    for test_class in test_classes:
        tests = unittest.TestLoader().loadTestsFromTestCase(test_class)
        test_suite.addTests(tests)

    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(test_suite)

    # Exit with proper code
    exit(0 if result.wasSuccessful() else 1)