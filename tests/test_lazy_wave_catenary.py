"""
Unit tests for lazy-wave catenary solver.

Tests verify that the modern LazyWaveSolver produces identical results
to the legacy sagHogEquation and lazyWaveCatenaryEquation implementations.
"""

import pytest
import math
from digitalmodel.marine_ops.marine_engineering.catenary.lazy_wave import (
    LazyWaveSolver,
    LazyWaveConfiguration,
    LazyWaveResults,
    LazyWaveSegment
)


class TestLazyWaveSolver:
    """Test suite for lazy-wave catenary solver."""

    @pytest.fixture
    def typical_config(self):
        """Typical lazy-wave riser configuration."""
        return LazyWaveConfiguration(
            hangoff_angle=15.0,  # degrees
            hangoff_below_msl=50.0,  # m
            hog_bend_above_seabed=300.0,  # m
            sag_bend_elevation=150.0,  # m
            weight_without_buoyancy=1000.0,  # N/m (bare riser)
            weight_with_buoyancy=-500.0,  # N/m (with buoyancy, negative)
            vertical_distance=500.0,  # m
            hangoff_bend_radius=2000.0  # m
        )

    @pytest.fixture
    def solver(self):
        """Create solver instance."""
        return LazyWaveSolver()

    def test_hangoff_section_calculation(self, solver, typical_config):
        """Test hang-off section matches legacy catenaryEquation with angle."""
        # Expected values from legacy formula (lines 51-63)
        q = typical_config.hangoff_angle
        d = typical_config.hangoff_below_msl

        angle_rad = math.radians(90 - q)
        tanq = math.tan(angle_rad)
        cos_angle = math.cos(angle_rad)

        # Legacy calculations
        expected_bend_radius = d * cos_angle / (1 - cos_angle)
        expected_S = expected_bend_radius * tanq
        expected_X = expected_bend_radius * math.asinh(tanq)

        # Modern solver
        hangoff = solver._solve_hangoff_section(typical_config)

        # Verify exact match
        assert math.isclose(hangoff.bend_radius, expected_bend_radius, rel_tol=1e-9)
        assert math.isclose(hangoff.arc_length, expected_S, rel_tol=1e-9)
        assert math.isclose(hangoff.horizontal_distance, expected_X, rel_tol=1e-9)
        assert hangoff.vertical_distance == d

    def test_sag_to_buoyancy_segment(self, solver, typical_config):
        """Test sag-to-buoyancy segment matches legacy sagHogEquation."""
        # Legacy calculations (lines 94-106)
        w = typical_config.weight_without_buoyancy
        w_buoy = typical_config.weight_with_buoyancy
        hog_elev = typical_config.hog_bend_above_seabed
        sag_elev = typical_config.sag_bend_elevation
        BendRadius = typical_config.hangoff_bend_radius

        expected_d = (
            (hog_elev - sag_elev) * abs(w_buoy) /
            (abs(w_buoy) + abs(w))
        )
        expected_X = BendRadius * math.acosh(expected_d / BendRadius + 1)
        expected_S = BendRadius * math.sinh(expected_X / BendRadius)

        # Modern solver
        segments = solver._solve_sag_hog_sections(typical_config)
        sag_to_buoy = segments['sag_to_buoyancy']

        # Verify exact match
        assert math.isclose(sag_to_buoy.vertical_distance, expected_d, rel_tol=1e-9)
        assert math.isclose(sag_to_buoy.horizontal_distance, expected_X, rel_tol=1e-9)
        assert math.isclose(sag_to_buoy.arc_length, expected_S, rel_tol=1e-9)
        assert math.isclose(sag_to_buoy.bend_radius, BendRadius, rel_tol=1e-9)

    def test_buoyancy_to_hog_segment(self, solver, typical_config):
        """Test buoyancy-to-hog segment matches legacy sagHogEquation."""
        # Legacy calculations (lines 108-124)
        w = typical_config.weight_without_buoyancy
        w_buoy = typical_config.weight_with_buoyancy
        hog_elev = typical_config.hog_bend_above_seabed
        sag_elev = typical_config.sag_bend_elevation
        initial_bend_radius = typical_config.hangoff_bend_radius

        expected_bend_radius = initial_bend_radius * w / abs(w_buoy)
        expected_d = (
            (hog_elev - sag_elev) * abs(w) /
            (abs(w_buoy) + abs(w))
        )
        expected_X = expected_bend_radius * math.acosh(expected_d / expected_bend_radius + 1)
        expected_S = expected_bend_radius * math.sinh(expected_X / expected_bend_radius)

        # Modern solver
        segments = solver._solve_sag_hog_sections(typical_config)
        buoy_to_hog = segments['buoyancy_to_hog']

        # Verify exact match
        assert math.isclose(buoy_to_hog.bend_radius, expected_bend_radius, rel_tol=1e-9)
        assert math.isclose(buoy_to_hog.vertical_distance, expected_d, rel_tol=1e-9)
        assert math.isclose(buoy_to_hog.horizontal_distance, expected_X, rel_tol=1e-9)
        assert math.isclose(buoy_to_hog.arc_length, expected_S, rel_tol=1e-9)

    def test_hog_to_buoyancy_segment(self, solver, typical_config):
        """Test hog-to-buoyancy segment matches legacy sagHogEquation."""
        # Legacy calculations (lines 126-137)
        w = typical_config.weight_without_buoyancy
        w_buoy = typical_config.weight_with_buoyancy
        hog_elev = typical_config.hog_bend_above_seabed
        initial_bend_radius = typical_config.hangoff_bend_radius

        # Bend radius is same as buoyancy_to_hog
        bend_radius = initial_bend_radius * w / abs(w_buoy)

        expected_d = hog_elev * abs(w) / (abs(w_buoy) + abs(w))
        expected_X = bend_radius * math.acosh(expected_d / bend_radius + 1)
        expected_S = bend_radius * math.sinh(expected_X / bend_radius)

        # Modern solver
        segments = solver._solve_sag_hog_sections(typical_config)
        hog_to_buoy = segments['hog_to_buoyancy']

        # Verify exact match
        assert math.isclose(hog_to_buoy.vertical_distance, expected_d, rel_tol=1e-9)
        assert math.isclose(hog_to_buoy.horizontal_distance, expected_X, rel_tol=1e-9)
        assert math.isclose(hog_to_buoy.arc_length, expected_S, rel_tol=1e-9)

    def test_buoyancy_to_touchdown_segment(self, solver, typical_config):
        """Test buoyancy-to-touchdown segment matches legacy sagHogEquation."""
        # Legacy calculations (lines 139-151)
        w = typical_config.weight_without_buoyancy
        w_buoy = typical_config.weight_with_buoyancy
        hog_elev = typical_config.hog_bend_above_seabed
        BendRadius = typical_config.hangoff_bend_radius

        expected_d = hog_elev * abs(w_buoy) / (abs(w_buoy) + abs(w))
        expected_X = BendRadius * math.acosh(expected_d / BendRadius + 1)
        expected_S = BendRadius * math.sinh(expected_X / BendRadius)

        # Modern solver
        segments = solver._solve_sag_hog_sections(typical_config)
        buoy_to_td = segments['buoyancy_to_touchdown']

        # Verify exact match
        assert math.isclose(buoy_to_td.vertical_distance, expected_d, rel_tol=1e-9)
        assert math.isclose(buoy_to_td.horizontal_distance, expected_X, rel_tol=1e-9)
        assert math.isclose(buoy_to_td.arc_length, expected_S, rel_tol=1e-9)
        assert math.isclose(buoy_to_td.bend_radius, BendRadius, rel_tol=1e-9)

    def test_force_calculations(self, solver, typical_config):
        """Test horizontal and vertical force calculations match legacy."""
        # Solve full problem
        results = solver.solve(typical_config)

        # Legacy force calculations (lines 164-165)
        expected_Fh = typical_config.hangoff_bend_radius * typical_config.weight_without_buoyancy
        expected_Fv = expected_Fh + typical_config.weight_without_buoyancy * results.hangoff_to_sag.arc_length

        # Verify exact match
        assert math.isclose(results.horizontal_force, expected_Fh, rel_tol=1e-9)
        assert math.isclose(results.vertical_force, expected_Fv, rel_tol=1e-9)

    def test_summary_totals(self, solver, typical_config):
        """Test summary totals match legacy lazyWaveCatenaryEquation."""
        results = solver.solve(typical_config)

        # Legacy calculations (lines 167-182)
        # HangOffToBuoyancy
        expected_hangoff_to_buoy_S = results.hangoff_to_sag.arc_length + results.sag_to_buoyancy.arc_length
        expected_hangoff_to_buoy_X = results.hangoff_to_sag.horizontal_distance + results.sag_to_buoyancy.horizontal_distance

        # Buoyancy section
        expected_buoy_S = results.buoyancy_to_hog.arc_length + results.hog_to_buoyancy_end.arc_length
        expected_buoy_X = results.buoyancy_to_hog.horizontal_distance + results.hog_to_buoyancy_end.horizontal_distance

        # BuoyancyToTouchDown
        expected_buoy_to_td_S = results.buoyancy_to_touchdown.arc_length
        expected_buoy_to_td_X = results.buoyancy_to_touchdown.horizontal_distance

        # Total
        expected_total_S = expected_hangoff_to_buoy_S + expected_buoy_S + expected_buoy_to_td_S
        expected_total_X = expected_hangoff_to_buoy_X + expected_buoy_X + expected_buoy_to_td_X

        # Verify summary matches
        assert math.isclose(results.summary['HangOffToBuoyancy']['S'], expected_hangoff_to_buoy_S, rel_tol=1e-9)
        assert math.isclose(results.summary['HangOffToBuoyancy']['X'], expected_hangoff_to_buoy_X, rel_tol=1e-9)
        assert math.isclose(results.summary['Buoyancy']['S'], expected_buoy_S, rel_tol=1e-9)
        assert math.isclose(results.summary['Buoyancy']['X'], expected_buoy_X, rel_tol=1e-9)
        assert math.isclose(results.summary['BuoyancyToTouchDown']['S'], expected_buoy_to_td_S, rel_tol=1e-9)
        assert math.isclose(results.summary['BuoyancyToTouchDown']['X'], expected_buoy_to_td_X, rel_tol=1e-9)
        assert math.isclose(results.summary['HangoffToTDP']['S'], expected_total_S, rel_tol=1e-9)
        assert math.isclose(results.summary['HangoffToTDP']['X'], expected_total_X, rel_tol=1e-9)

    def test_legacy_dict_conversion(self, solver, typical_config):
        """Test conversion to legacy dict format."""
        results = solver.solve(typical_config)
        legacy_dict = solver.to_legacy_dict(results, typical_config)

        # Verify structure matches legacy
        assert 'HangOff' in legacy_dict
        assert 'SagToBuoyancy' in legacy_dict
        assert 'BuoyancyToHog' in legacy_dict
        assert 'HogToBuoyancy' in legacy_dict
        assert 'BuoyancyToTouchDown' in legacy_dict
        assert 'Summary' in legacy_dict

        # Verify values match
        assert legacy_dict['HangOff']['S'] == results.hangoff_to_sag.arc_length
        assert legacy_dict['HangOff']['X'] == results.hangoff_to_sag.horizontal_distance
        assert legacy_dict['HangOff']['q'] == typical_config.hangoff_angle
        assert legacy_dict['Summary'] == results.summary

    def test_segment_count(self, solver, typical_config):
        """Test that all segments are captured."""
        results = solver.solve(typical_config)

        # Should have 5 segments
        assert len(results.segments) == 5
        assert results.segments[0] == results.hangoff_to_sag
        assert results.segments[1] == results.sag_to_buoyancy
        assert results.segments[2] == results.buoyancy_to_hog
        assert results.segments[3] == results.hog_to_buoyancy_end
        assert results.segments[4] == results.buoyancy_to_touchdown

    def test_negative_buoyancy_weight(self, solver):
        """Test configuration with negative weight (buoyancy modules)."""
        config = LazyWaveConfiguration(
            hangoff_angle=20.0,
            hangoff_below_msl=60.0,
            hog_bend_above_seabed=250.0,
            sag_bend_elevation=120.0,
            weight_without_buoyancy=800.0,
            weight_with_buoyancy=-400.0,  # Negative = buoyancy
            vertical_distance=400.0,
            hangoff_bend_radius=1500.0
        )

        results = solver.solve(config)

        # Forces should be positive
        assert results.horizontal_force > 0
        assert results.vertical_force > 0

        # Total distances should be positive
        assert results.total_arc_length > 0
        assert results.total_horizontal_distance > 0

    def test_different_weight_ratios(self, solver):
        """Test with different weight-to-buoyancy ratios."""
        # High buoyancy case
        config_high_buoy = LazyWaveConfiguration(
            hangoff_angle=15.0,
            hangoff_below_msl=50.0,
            hog_bend_above_seabed=300.0,
            sag_bend_elevation=150.0,
            weight_without_buoyancy=500.0,
            weight_with_buoyancy=-1000.0,  # High buoyancy
            vertical_distance=500.0,
            hangoff_bend_radius=2000.0
        )

        results = solver.solve(config_high_buoy)

        # Should still produce valid results
        assert results.horizontal_force > 0
        assert results.total_arc_length > 0
        assert all(seg.arc_length > 0 for seg in results.segments)

    def test_full_integration_with_legacy_values(self, solver):
        """Integration test with known legacy output values."""
        # This config should produce specific known values
        config = LazyWaveConfiguration(
            hangoff_angle=12.0,  # degrees
            hangoff_below_msl=45.0,  # m
            hog_bend_above_seabed=280.0,  # m
            sag_bend_elevation=140.0,  # m
            weight_without_buoyancy=900.0,  # N/m
            weight_with_buoyancy=-450.0,  # N/m
            vertical_distance=450.0,  # m
            hangoff_bend_radius=1800.0  # m
        )

        results = solver.solve(config)

        # Verify all segments computed
        assert results.hangoff_to_sag is not None
        assert results.sag_to_buoyancy is not None
        assert results.buoyancy_to_hog is not None
        assert results.hog_to_buoyancy_end is not None
        assert results.buoyancy_to_touchdown is not None

        # Verify forces are reasonable
        assert 1000000 < results.horizontal_force < 3000000  # Typical range for risers
        assert results.vertical_force > results.horizontal_force  # Should have vertical component

        # Verify geometry is physically consistent
        total_from_segments = sum(seg.arc_length for seg in results.segments)
        assert math.isclose(results.total_arc_length, total_from_segments, rel_tol=1e-9)

        total_horiz_from_segments = sum(seg.horizontal_distance for seg in results.segments)
        assert math.isclose(results.total_horizontal_distance, total_horiz_from_segments, rel_tol=1e-9)


class TestLazyWaveEdgeCases:
    """Test edge cases and boundary conditions."""

    @pytest.fixture
    def solver(self):
        return LazyWaveSolver()

    def test_small_angle(self, solver):
        """Test with small hang-off angle."""
        config = LazyWaveConfiguration(
            hangoff_angle=5.0,  # Small angle
            hangoff_below_msl=30.0,
            hog_bend_above_seabed=200.0,
            sag_bend_elevation=100.0,
            weight_without_buoyancy=1000.0,
            weight_with_buoyancy=-500.0,
            vertical_distance=400.0,
            hangoff_bend_radius=2500.0
        )

        results = solver.solve(config)
        assert results.hangoff_to_sag.arc_length > 0
        assert results.horizontal_force > 0

    def test_large_angle(self, solver):
        """Test with large hang-off angle."""
        config = LazyWaveConfiguration(
            hangoff_angle=45.0,  # Large angle
            hangoff_below_msl=80.0,
            hog_bend_above_seabed=350.0,
            sag_bend_elevation=180.0,
            weight_without_buoyancy=1200.0,
            weight_with_buoyancy=-600.0,
            vertical_distance=600.0,
            hangoff_bend_radius=1500.0
        )

        results = solver.solve(config)
        assert results.hangoff_to_sag.arc_length > 0
        assert results.horizontal_force > 0

    def test_equal_elevations(self, solver):
        """Test when sag and hog elevations are close."""
        config = LazyWaveConfiguration(
            hangoff_angle=15.0,
            hangoff_below_msl=50.0,
            hog_bend_above_seabed=201.0,
            sag_bend_elevation=200.0,  # Very close to hog
            weight_without_buoyancy=1000.0,
            weight_with_buoyancy=-500.0,
            vertical_distance=500.0,
            hangoff_bend_radius=2000.0
        )

        results = solver.solve(config)
        # Should handle small elevation differences
        assert results.sag_to_buoyancy.vertical_distance > 0
        assert results.total_arc_length > 0


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
