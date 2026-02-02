"""
Legacy comparison tests for lazy-wave catenary solver.

These tests verify that the modern LazyWaveSolver produces identical
numerical results to the legacy sagHogEquation and lazyWaveCatenaryEquation
implementations when given the same input data.
"""

import pytest
import math
import sys
import os

# Add legacy module to path for testing
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))

from digitalmodel.marine_engineering.catenary.lazy_wave import (
    LazyWaveSolver,
    LazyWaveConfiguration
)

# Import legacy functions
from digitalmodel.catenary.catenaryMethods import (
    sagHogEquation,
    lazyWaveCatenaryEquation,
    catenaryEquation
)


class TestLegacyComparison:
    """Compare modern solver against legacy implementation."""

    @pytest.fixture
    def legacy_data(self):
        """Create legacy data structure."""
        return {
            'HangOff': {
                'q': 15.0,  # degrees
                'd': 50.0,  # m below MSL
                'F': None,
                'X': None,
                'w': 1000.0
            },
            'HogBendAboveSeabed': 300.0,
            'SagBendElevationAboveSeabed': 150.0,
            'WeightPerUnitLengthWithOutBuoyancy': 1000.0,
            'WeightPerUnitLengthWithBuoyancy': -500.0
        }

    @pytest.fixture
    def modern_config(self):
        """Create modern configuration."""
        return LazyWaveConfiguration(
            hangoff_angle=15.0,
            hangoff_below_msl=50.0,
            hog_bend_above_seabed=300.0,
            sag_bend_elevation=150.0,
            weight_without_buoyancy=1000.0,
            weight_with_buoyancy=-500.0,
            vertical_distance=500.0,
            hangoff_bend_radius=2000.0  # Will be calculated from legacy
        )

    def test_hangoff_section_matches_legacy(self, legacy_data):
        """Test hang-off section calculation matches legacy."""
        # Run legacy
        legacy_result = catenaryEquation(legacy_data['HangOff'])

        # Create modern config with same bend radius
        modern_config = LazyWaveConfiguration(
            hangoff_angle=legacy_data['HangOff']['q'],
            hangoff_below_msl=legacy_data['HangOff']['d'],
            hog_bend_above_seabed=300.0,
            sag_bend_elevation=150.0,
            weight_without_buoyancy=1000.0,
            weight_with_buoyancy=-500.0,
            vertical_distance=500.0,
            hangoff_bend_radius=legacy_result['BendRadius']
        )

        solver = LazyWaveSolver()
        modern_hangoff = solver._solve_hangoff_section(modern_config)

        # Compare results
        assert math.isclose(modern_hangoff.bend_radius, legacy_result['BendRadius'], rel_tol=1e-9)
        assert math.isclose(modern_hangoff.arc_length, legacy_result['S'], rel_tol=1e-9)
        assert math.isclose(modern_hangoff.horizontal_distance, legacy_result['X'], rel_tol=1e-9)

    def test_sag_hog_sections_match_legacy(self, legacy_data):
        """Test sag-hog sections match legacy sagHogEquation."""
        # First compute hang-off for bend radius
        legacy_hangoff = catenaryEquation(legacy_data['HangOff'])
        legacy_data['HangOff'].update(legacy_hangoff)

        # Run legacy sag-hog
        legacy_result = sagHogEquation(legacy_data)

        # Run modern
        modern_config = LazyWaveConfiguration(
            hangoff_angle=legacy_data['HangOff']['q'],
            hangoff_below_msl=legacy_data['HangOff']['d'],
            hog_bend_above_seabed=legacy_data['HogBendAboveSeabed'],
            sag_bend_elevation=legacy_data['SagBendElevationAboveSeabed'],
            weight_without_buoyancy=legacy_data['WeightPerUnitLengthWithOutBuoyancy'],
            weight_with_buoyancy=legacy_data['WeightPerUnitLengthWithBuoyancy'],
            vertical_distance=500.0,
            hangoff_bend_radius=legacy_hangoff['BendRadius']
        )

        solver = LazyWaveSolver()
        modern_segments = solver._solve_sag_hog_sections(modern_config)

        # Compare SagToBuoyancy
        assert math.isclose(
            modern_segments['sag_to_buoyancy'].vertical_distance,
            legacy_result['SagToBuoyancy']['d'],
            rel_tol=1e-9
        )
        assert math.isclose(
            modern_segments['sag_to_buoyancy'].arc_length,
            legacy_result['SagToBuoyancy']['S'],
            rel_tol=1e-9
        )
        assert math.isclose(
            modern_segments['sag_to_buoyancy'].horizontal_distance,
            legacy_result['SagToBuoyancy']['X'],
            rel_tol=1e-9
        )

        # Compare BuoyancyToHog
        assert math.isclose(
            modern_segments['buoyancy_to_hog'].vertical_distance,
            legacy_result['BuoyancyToHog']['d'],
            rel_tol=1e-9
        )
        assert math.isclose(
            modern_segments['buoyancy_to_hog'].arc_length,
            legacy_result['BuoyancyToHog']['S'],
            rel_tol=1e-9
        )
        assert math.isclose(
            modern_segments['buoyancy_to_hog'].horizontal_distance,
            legacy_result['BuoyancyToHog']['X'],
            rel_tol=1e-9
        )

        # Compare HogToBuoyancy
        assert math.isclose(
            modern_segments['hog_to_buoyancy'].vertical_distance,
            legacy_result['HogToBuoyancy']['d'],
            rel_tol=1e-9
        )
        assert math.isclose(
            modern_segments['hog_to_buoyancy'].arc_length,
            legacy_result['HogToBuoyancy']['S'],
            rel_tol=1e-9
        )
        assert math.isclose(
            modern_segments['hog_to_buoyancy'].horizontal_distance,
            legacy_result['HogToBuoyancy']['X'],
            rel_tol=1e-9
        )

        # Compare BuoyancyToTouchDown
        assert math.isclose(
            modern_segments['buoyancy_to_touchdown'].vertical_distance,
            legacy_result['BuoyancyToTouchDown']['d'],
            rel_tol=1e-9
        )
        assert math.isclose(
            modern_segments['buoyancy_to_touchdown'].arc_length,
            legacy_result['BuoyancyToTouchDown']['S'],
            rel_tol=1e-9
        )
        assert math.isclose(
            modern_segments['buoyancy_to_touchdown'].horizontal_distance,
            legacy_result['BuoyancyToTouchDown']['X'],
            rel_tol=1e-9
        )

    def test_full_lazy_wave_matches_legacy(self, legacy_data):
        """Test complete lazy-wave solution matches legacy."""
        # Run legacy full solution
        legacy_hangoff = catenaryEquation(legacy_data['HangOff'])
        legacy_data['HangOff'].update(legacy_hangoff)
        legacy_result = lazyWaveCatenaryEquation(legacy_data)

        # Run modern
        modern_config = LazyWaveConfiguration(
            hangoff_angle=legacy_data['HangOff']['q'],
            hangoff_below_msl=legacy_data['HangOff']['d'],
            hog_bend_above_seabed=legacy_data['HogBendAboveSeabed'],
            sag_bend_elevation=legacy_data['SagBendElevationAboveSeabed'],
            weight_without_buoyancy=legacy_data['WeightPerUnitLengthWithOutBuoyancy'],
            weight_with_buoyancy=legacy_data['WeightPerUnitLengthWithBuoyancy'],
            vertical_distance=500.0,
            hangoff_bend_radius=legacy_hangoff['BendRadius']
        )

        solver = LazyWaveSolver()
        modern_result = solver.solve(modern_config)

        # Compare forces
        assert math.isclose(
            modern_result.horizontal_force,
            legacy_result['Summary']['Fh'],
            rel_tol=1e-9
        )
        assert math.isclose(
            modern_result.vertical_force,
            legacy_result['Summary']['Fv'],
            rel_tol=1e-9
        )

        # Compare HangOffToBuoyancy summary
        assert math.isclose(
            modern_result.summary['HangOffToBuoyancy']['S'],
            legacy_result['Summary']['HangOffToBuoyancy']['S'],
            rel_tol=1e-9
        )
        assert math.isclose(
            modern_result.summary['HangOffToBuoyancy']['X'],
            legacy_result['Summary']['HangOffToBuoyancy']['X'],
            rel_tol=1e-9
        )

        # Compare Buoyancy summary
        assert math.isclose(
            modern_result.summary['Buoyancy']['S'],
            legacy_result['Summary']['Buoyancy']['S'],
            rel_tol=1e-9
        )
        assert math.isclose(
            modern_result.summary['Buoyancy']['X'],
            legacy_result['Summary']['Buoyancy']['X'],
            rel_tol=1e-9
        )

        # Compare BuoyancyToTouchDown summary
        assert math.isclose(
            modern_result.summary['BuoyancyToTouchDown']['S'],
            legacy_result['Summary']['BuoyancyToTouchDown']['S'],
            rel_tol=1e-9
        )
        assert math.isclose(
            modern_result.summary['BuoyancyToTouchDown']['X'],
            legacy_result['Summary']['BuoyancyToTouchDown']['X'],
            rel_tol=1e-9
        )

        # Compare totals
        assert math.isclose(
            modern_result.summary['HangoffToTDP']['S'],
            legacy_result['Summary']['HangoffToTDP']['S'],
            rel_tol=1e-9
        )
        assert math.isclose(
            modern_result.summary['HangoffToTDP']['X'],
            legacy_result['Summary']['HangoffToTDP']['X'],
            rel_tol=1e-9
        )

    def test_legacy_dict_format_matches(self, legacy_data):
        """Test that to_legacy_dict produces exact legacy format."""
        # Run legacy
        legacy_hangoff = catenaryEquation(legacy_data['HangOff'])
        legacy_data['HangOff'].update(legacy_hangoff)
        legacy_result = lazyWaveCatenaryEquation(legacy_data)

        # Run modern and convert to legacy format
        modern_config = LazyWaveConfiguration(
            hangoff_angle=legacy_data['HangOff']['q'],
            hangoff_below_msl=legacy_data['HangOff']['d'],
            hog_bend_above_seabed=legacy_data['HogBendAboveSeabed'],
            sag_bend_elevation=legacy_data['SagBendElevationAboveSeabed'],
            weight_without_buoyancy=legacy_data['WeightPerUnitLengthWithOutBuoyancy'],
            weight_with_buoyancy=legacy_data['WeightPerUnitLengthWithBuoyancy'],
            vertical_distance=500.0,
            hangoff_bend_radius=legacy_hangoff['BendRadius']
        )

        solver = LazyWaveSolver()
        modern_result = solver.solve(modern_config)
        modern_dict = solver.to_legacy_dict(modern_result, modern_config)

        # Verify structure
        assert set(modern_dict.keys()) == {
            'HangOff', 'SagToBuoyancy', 'BuoyancyToHog', 'HogToBuoyancy',
            'BuoyancyToTouchDown', 'Summary', 'WeightPerUnitLengthWithOutBuoyancy',
            'WeightPerUnitLengthWithBuoyancy', 'HogBendAboveSeabed',
            'SagBendElevationAboveSeabed'
        }

        # Verify values match
        assert modern_dict['HangOff']['q'] == legacy_data['HangOff']['q']
        assert math.isclose(
            modern_dict['Summary']['Fh'],
            legacy_result['Summary']['Fh'],
            rel_tol=1e-9
        )
        assert math.isclose(
            modern_dict['Summary']['Fv'],
            legacy_result['Summary']['Fv'],
            rel_tol=1e-9
        )


if __name__ == '__main__':
    pytest.main([__file__, '-v', '--tb=short'])
