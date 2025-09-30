"""
Test script to verify the API STD 2RD migration.

This script tests the modernized API STD 2RD modules to ensure
they work correctly after migration from legacy code.
"""

import pytest
import numpy as np
import pandas as pd
from pathlib import Path
import tempfile
import yaml

from digitalmodel.analysis.apistd2rd import APISTD2RDAnalyzer
from digitalmodel.calculations.pipe_properties import calculate_geometric_properties
from digitalmodel.calculations.stress_calculations import APISTD2RDCalculations
from digitalmodel.data_manager.configuration import ConfigurationManager


class TestAPISTD2RDMigration:
    """Test suite for API STD 2RD migration."""

    def test_pipe_properties_calculation(self):
        """Test pipe properties calculations."""
        # Test basic properties calculation
        od = 20.0  # inches
        id_val = 18.0  # inches
        wt = 1.0  # inches

        props = calculate_geometric_properties(od, id_val, wt)

        assert props['outer_diameter'] == od
        assert props['inner_diameter'] == id_val
        assert props['wall_thickness'] == wt
        assert props['A'] > 0  # Cross-sectional area
        assert props['Ai'] > 0  # Internal area
        assert props['Ao'] > 0  # External area
        assert props['I'] > 0  # Moment of inertia

    def test_burst_pressure_calculation(self):
        """Test burst pressure calculations."""
        calculator = APISTD2RDCalculations()

        # Test parameters
        od = 20.0
        id_val = 18.0
        smys = 52000  # psi
        smus = 66000  # psi
        corrosion_allowance = 0.125

        # Calculate burst pressure
        burst_pressure = calculator.calculate_burst_pressure(
            od, id_val, smys, smus, corrosion_allowance
        )

        assert burst_pressure > 0
        assert isinstance(burst_pressure, float)

    def test_collapse_pressure_calculation(self):
        """Test collapse pressure calculations."""
        calculator = APISTD2RDCalculations()

        # Test parameters
        od = 20.0
        id_val = 18.0
        wt = 1.0
        smys = 52000
        elastic_modulus = 30e6
        poisson_ratio = 0.3

        collapse_data = calculator.calculate_collapse_pressure(
            od, id_val, wt, smys, elastic_modulus, poisson_ratio
        )

        assert 'yield_collapse' in collapse_data
        assert 'elastic_collapse' in collapse_data
        assert 'combined_collapse' in collapse_data
        assert all(v > 0 for v in collapse_data.values())

    def test_configuration_manager(self):
        """Test configuration management with YAML safety."""
        # Create a test configuration
        test_config = {
            'geometry': {
                'NominalOD': 20.0,
                'DesignWT': 1.0,
                'CorrosionAllowance': 0.125
            },
            'material': {
                'SMYS': 52000,
                'SMUS': 66000,
                'E': 30e6,
                'Poissionsratio': 0.3,
                'alphafab': 1.0,
                'k': 1.0
            },
            'designFactors': {
                'internalPressure': {
                    'design': 0.72,
                    'incidentalPressure': 0.80,
                    'hydroStaticTest': 1.25
                }
            }
        }

        # Test with temporary file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.yml', delete=False) as f:
            yaml.safe_dump(test_config, f)
            temp_path = Path(f.name)

        try:
            manager = ConfigurationManager()
            config = manager.load_configuration(temp_path)

            assert 'geometry' in config
            assert 'material' in config
            assert 'designFactors' in config

            # Test geometry property updates
            manager.update_geometry_properties()
            assert config['geometry'].get('NominalID') is not None

        finally:
            temp_path.unlink()

    def test_method1_interaction_curves(self):
        """Test Method 1 interaction curve generation."""
        calculator = APISTD2RDCalculations()

        # Test parameters
        yield_tension = 1000.0  # kN
        yield_moment = 500.0    # kN.m
        plastic_moment = 666.7  # kN.m
        pressure_factor = 0.8

        df = calculator.generate_interaction_curves(
            yield_tension, yield_moment, plastic_moment, pressure_factor
        )

        assert isinstance(df, pd.DataFrame)
        assert len(df) > 0
        assert 'tension' in df.columns
        assert 'moment_positive_method1' in df.columns
        assert 'moment_positive_method2' in df.columns

    def test_analyzer_initialization(self):
        """Test analyzer initialization and basic functionality."""
        analyzer = APISTD2RDAnalyzer()

        assert analyzer.config_manager is not None
        assert analyzer.calculator is not None
        assert isinstance(analyzer.results, dict)

    def test_cross_platform_paths(self):
        """Test that paths work across platforms."""
        # Test with various path formats
        test_paths = [
            "config/test.yml",
            "config\\test.yml",  # Windows-style
            Path("config") / "test.yml"
        ]

        for path in test_paths:
            normalized = Path(path)
            assert isinstance(normalized, Path)
            assert normalized.suffix == '.yml'

    def test_yaml_security(self):
        """Test that YAML loading is secure."""
        # Test with potentially dangerous YAML content
        dangerous_yaml = """
        !!python/object/apply:subprocess.call
        - ['echo', 'dangerous']
        """

        with tempfile.NamedTemporaryFile(mode='w', suffix='.yml', delete=False) as f:
            f.write(dangerous_yaml)
            temp_path = Path(f.name)

        try:
            manager = ConfigurationManager()

            # This should raise an exception or return None/empty dict
            # when using yaml.safe_load instead of yaml.load
            with pytest.raises(yaml.constructor.ConstructorError):
                manager.load_configuration(temp_path)

        finally:
            temp_path.unlink()

    def test_input_validation(self):
        """Test input validation and error handling."""
        calculator = APISTD2RDCalculations()

        # Test invalid diameters
        with pytest.raises(ValueError):
            calculator.calculate_burst_pressure(-1, 10, 52000, 66000)

        with pytest.raises(ValueError):
            calculator.calculate_burst_pressure(10, 15, 52000, 66000)  # ID > OD

        # Test invalid strengths
        with pytest.raises(ValueError):
            calculator.calculate_burst_pressure(20, 18, -1000, 66000)

    def test_temperature_derating(self):
        """Test temperature derating functionality."""
        from digitalmodel.calculations.stress_calculations import apply_temperature_derating

        # Test at room temperature (no derating)
        result = apply_temperature_derating(52000, 66000, 20)
        assert result['derating_factor'] == 1.0
        assert result['yield_strength'] == 52000

        # Test at elevated temperature (some derating)
        result = apply_temperature_derating(52000, 66000, 75)
        assert result['derating_factor'] < 1.0
        assert result['yield_strength'] < 52000


def run_basic_migration_test():
    """
    Run a basic test to verify the migration works.

    This function can be called directly to test the migration
    without running the full pytest suite.
    """
    print("Testing API STD 2RD migration...")

    try:
        # Test 1: Basic imports
        print("✓ Testing imports...")
        from digitalmodel.analysis.apistd2rd import APISTD2RDAnalyzer
        from digitalmodel.calculations.pipe_properties import calculate_geometric_properties
        from digitalmodel.calculations.stress_calculations import APISTD2RDCalculations
        from digitalmodel.data_manager.configuration import ConfigurationManager

        # Test 2: Basic calculations
        print("✓ Testing calculations...")
        props = calculate_geometric_properties(outer_diameter=20.0, inner_diameter=18.0)
        assert props['wall_thickness'] == 1.0

        calculator = APISTD2RDCalculations()
        burst = calculator.calculate_burst_pressure(20.0, 18.0, 52000, 66000)
        assert burst > 0

        # Test 3: Configuration manager
        print("✓ Testing configuration...")
        manager = ConfigurationManager()

        # Test 4: Analyzer initialization
        print("✓ Testing analyzer...")
        analyzer = APISTD2RDAnalyzer()

        print("✅ All basic tests passed! Migration successful.")
        return True

    except Exception as e:
        print(f"❌ Migration test failed: {e}")
        return False


if __name__ == "__main__":
    # Run basic test when script is executed directly
    success = run_basic_migration_test()
    if not success:
        exit(1)