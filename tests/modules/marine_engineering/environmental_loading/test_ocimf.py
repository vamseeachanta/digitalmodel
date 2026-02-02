"""
Comprehensive test suite for OCIMF Environmental Loading module.

This test suite validates:
- Database loading and interpolation
- Coefficient retrieval
- Force calculations
- Chart generation
- Edge cases and error handling
"""

import pytest
import numpy as np
import pandas as pd
from pathlib import Path
import tempfile
import shutil

from digitalmodel.marine_analysis.environmental_loading import (
    OCIMFDatabase,
    EnvironmentalForces,
    OCIMFCoefficients,
    EnvironmentalConditions,
    VesselGeometry,
    EnvironmentalForceResults,
    create_sample_database,
)


@pytest.fixture(scope="module")
def sample_database_path():
    """Create a temporary sample database for testing."""
    temp_dir = tempfile.mkdtemp()
    db_path = Path(temp_dir) / "test_ocimf.csv"

    # Create sample database
    create_sample_database(
        str(db_path),
        num_vessels=3,
        num_headings=13,
        num_displacements=3
    )

    yield str(db_path)

    # Cleanup
    shutil.rmtree(temp_dir)


@pytest.fixture
def ocimf_db(sample_database_path):
    """Create OCIMFDatabase fixture."""
    return OCIMFDatabase(sample_database_path)


@pytest.fixture
def env_forces(ocimf_db):
    """Create EnvironmentalForces fixture."""
    return EnvironmentalForces(ocimf_db)


@pytest.fixture
def sample_conditions():
    """Sample environmental conditions."""
    return EnvironmentalConditions(
        wind_speed=20.0,  # m/s (about 39 knots)
        wind_direction=45.0,  # degrees
        current_speed=1.5,  # m/s (about 3 knots)
        current_direction=30.0,  # degrees
        air_density=1.225,  # kg/m³
        water_density=1025.0  # kg/m³
    )


@pytest.fixture
def sample_geometry():
    """Sample vessel geometry."""
    return VesselGeometry(
        loa=330.0,  # meters (VLCC)
        beam=60.0,  # meters
        draft=22.0  # meters
    )


class TestOCIMFDatabase:
    """Test suite for OCIMFDatabase class."""

    def test_database_loading(self, sample_database_path):
        """Test database loads correctly."""
        db = OCIMFDatabase(sample_database_path)
        assert db.data is not None
        assert len(db.data) > 0
        assert 'CXw' in db.data.columns
        assert 'displacement' in db.data.columns

    def test_interpolators_created(self, ocimf_db):
        """Test that interpolators are created for all coefficients."""
        expected_coeffs = ['CXw', 'CYw', 'CMw', 'CXc', 'CYc', 'CMc']
        for coef in expected_coeffs:
            assert coef in ocimf_db.interpolators

    def test_get_coefficients_exact_match(self, ocimf_db):
        """Test coefficient retrieval for exact database match."""
        # Get a point that exists in the database
        first_row = ocimf_db.data.iloc[0]
        heading = first_row['heading']
        displacement = first_row['displacement']

        coeffs = ocimf_db.get_coefficients(heading, displacement)

        assert isinstance(coeffs, OCIMFCoefficients)
        assert coeffs.heading == heading
        assert coeffs.displacement == displacement

        # Values should be very close (within interpolation tolerance)
        assert abs(coeffs.CXw - first_row['CXw']) < 0.1
        assert abs(coeffs.CYw - first_row['CYw']) < 0.1

    def test_get_coefficients_interpolation(self, ocimf_db):
        """Test coefficient interpolation between data points."""
        # Use heading and displacement between known points
        heading = 45.0
        displacement = 275000.0  # Between typical VLCC displacements

        coeffs = ocimf_db.get_coefficients(heading, displacement)

        assert isinstance(coeffs, OCIMFCoefficients)
        assert 0 <= coeffs.CXw <= 1.5
        assert 0 <= coeffs.CYw <= 1.5
        assert -0.5 <= coeffs.CMw <= 0.5

    def test_heading_normalization(self, ocimf_db):
        """Test that headings are normalized to 0-180 range."""
        # Test heading > 180
        coeffs_270 = ocimf_db.get_coefficients(270, 250000)
        coeffs_90 = ocimf_db.get_coefficients(90, 250000)

        # Due to symmetry, coefficients should be similar
        assert abs(coeffs_270.CYw - coeffs_90.CYw) < 0.1

    def test_boundary_warnings(self, ocimf_db):
        """Test warnings for out-of-bounds values."""
        with pytest.warns(UserWarning):
            # Heading outside range
            ocimf_db.get_coefficients(200, 250000)

        with pytest.warns(UserWarning):
            # Displacement outside range
            ocimf_db.get_coefficients(45, 500000)

    @pytest.mark.slow
    def test_plot_coefficient_surface(self, ocimf_db, tmp_path):
        """Test 3D surface plot generation."""
        output_path = tmp_path / "surface_CYw.png"

        # Generate plot
        ocimf_db.plot_coefficient_surface(
            'CYw',
            save_path=str(output_path),
            interactive=False
        )

        assert output_path.exists()
        assert output_path.stat().st_size > 0

    @pytest.mark.slow
    def test_plot_polar_diagram(self, ocimf_db, tmp_path):
        """Test polar diagram generation."""
        output_path = tmp_path / "polar_250000.png"

        ocimf_db.plot_polar_diagram(
            displacement=250000,
            save_path=str(output_path)
        )

        assert output_path.exists()
        assert output_path.stat().st_size > 0

    @pytest.mark.slow
    def test_plot_interpolation_quality(self, ocimf_db, tmp_path):
        """Test interpolation quality heatmap."""
        output_path = tmp_path / "interp_quality.png"

        ocimf_db.plot_interpolation_quality(
            coefficient_name='CYw',
            save_path=str(output_path)
        )

        assert output_path.exists()
        assert output_path.stat().st_size > 0


class TestEnvironmentalForces:
    """Test suite for EnvironmentalForces class."""

    def test_wind_force_calculation(self, env_forces, sample_conditions,
                                    sample_geometry):
        """Test wind force calculation."""
        displacement = 250000

        fx, fy, mz = env_forces.calculate_wind_forces(
            sample_conditions,
            sample_geometry,
            displacement
        )

        # Forces should be positive (for positive coefficients)
        assert fx > 0 or fx < 0  # Can be either direction
        assert abs(fy) > 0  # Lateral force should exist for oblique wind
        assert abs(mz) > 0  # Moment should exist

    def test_current_force_calculation(self, env_forces, sample_conditions,
                                      sample_geometry):
        """Test current force calculation."""
        displacement = 250000

        fx, fy, mz = env_forces.calculate_current_forces(
            sample_conditions,
            sample_geometry,
            displacement
        )

        # Forces should be reasonable magnitude
        assert abs(fx) > 0
        assert abs(fy) > 0
        assert abs(mz) > 0

    def test_total_forces(self, env_forces, sample_conditions, sample_geometry):
        """Test total force calculation (wind + current)."""
        displacement = 250000

        results = env_forces.calculate_total_forces(
            sample_conditions,
            sample_geometry,
            displacement
        )

        assert isinstance(results, EnvironmentalForceResults)

        # Check that totals are sum of components
        assert abs(results.total_fx - (results.wind_fx + results.current_fx)) < 1e-6
        assert abs(results.total_fy - (results.wind_fy + results.current_fy)) < 1e-6
        assert abs(results.total_mz - (results.wind_mz + results.current_mz)) < 1e-6

    def test_force_scaling(self, env_forces, sample_geometry):
        """Test that forces scale properly with wind/current speed."""
        displacement = 250000

        # Test with different wind speeds
        conditions_low = EnvironmentalConditions(
            wind_speed=10.0,
            wind_direction=45.0,
            current_speed=1.0,
            current_direction=30.0
        )

        conditions_high = EnvironmentalConditions(
            wind_speed=20.0,  # 2x speed
            wind_direction=45.0,
            current_speed=1.0,
            current_direction=30.0
        )

        results_low = env_forces.calculate_total_forces(
            conditions_low, sample_geometry, displacement
        )
        results_high = env_forces.calculate_total_forces(
            conditions_high, sample_geometry, displacement
        )

        # Forces should scale with V² (approximately 4x)
        wind_force_ratio = results_high.wind_fx / results_low.wind_fx
        assert 3.5 < wind_force_ratio < 4.5  # Allow some tolerance

    @pytest.mark.slow
    def test_plot_force_diagram(self, env_forces, sample_conditions,
                               sample_geometry, tmp_path):
        """Test force diagram plotting."""
        displacement = 250000
        results = env_forces.calculate_total_forces(
            sample_conditions,
            sample_geometry,
            displacement
        )

        output_path = tmp_path / "force_diagram.png"
        env_forces.plot_force_diagram(results, save_path=str(output_path))

        assert output_path.exists()
        assert output_path.stat().st_size > 0

    @pytest.mark.slow
    def test_plot_validation_chart(self, env_forces, sample_conditions,
                                   sample_geometry, tmp_path):
        """Test validation chart generation."""
        displacement = 250000
        results = env_forces.calculate_total_forces(
            sample_conditions,
            sample_geometry,
            displacement
        )

        # Simulate Excel results (with small differences)
        excel_results = {
            'wind_fx': results.wind_fx * 1.02,
            'wind_fy': results.wind_fy * 0.98,
            'wind_mz': results.wind_mz * 1.01,
            'current_fx': results.current_fx * 1.03,
            'current_fy': results.current_fy * 0.97,
            'current_mz': results.current_mz * 1.02
        }

        output_path = tmp_path / "validation.png"
        env_forces.plot_validation_chart(
            excel_results,
            results,
            save_path=str(output_path)
        )

        assert output_path.exists()
        assert output_path.stat().st_size > 0


class TestDataclasses:
    """Test dataclass structures."""

    def test_ocimf_coefficients_to_dict(self):
        """Test OCIMFCoefficients to_dict method."""
        coeffs = OCIMFCoefficients(
            CXw=0.85, CYw=0.65, CMw=0.12,
            CXc=0.95, CYc=0.75, CMc=0.15,
            heading=45.0, displacement=250000
        )

        d = coeffs.to_dict()
        assert d['CXw'] == 0.85
        assert d['heading'] == 45.0

    def test_vessel_geometry_auto_areas(self):
        """Test automatic area calculation in VesselGeometry."""
        geom = VesselGeometry(loa=330, beam=60, draft=22)

        # Areas should be calculated automatically
        assert geom.frontal_area_wind is not None
        assert geom.lateral_area_wind is not None
        assert geom.frontal_area_current is not None
        assert geom.lateral_area_current is not None

        # Lateral area should be larger than frontal
        assert geom.lateral_area_wind > geom.frontal_area_wind

    def test_environmental_conditions_defaults(self):
        """Test default values in EnvironmentalConditions."""
        conditions = EnvironmentalConditions(
            wind_speed=20.0,
            wind_direction=45.0,
            current_speed=1.5,
            current_direction=30.0
        )

        # Check defaults
        assert conditions.air_density == 1.225
        assert conditions.water_density == 1025.0


class TestEdgeCases:
    """Test edge cases and error handling."""

    def test_zero_wind_speed(self, env_forces, sample_geometry):
        """Test with zero wind speed."""
        conditions = EnvironmentalConditions(
            wind_speed=0.0,
            wind_direction=45.0,
            current_speed=1.5,
            current_direction=30.0
        )

        results = env_forces.calculate_total_forces(
            conditions, sample_geometry, 250000
        )

        # Wind forces should be zero
        assert results.wind_fx == 0.0
        assert results.wind_fy == 0.0
        assert results.wind_mz == 0.0

    def test_head_on_wind(self, env_forces, sample_geometry):
        """Test with head-on wind (0 degrees)."""
        conditions = EnvironmentalConditions(
            wind_speed=20.0,
            wind_direction=0.0,
            current_speed=1.0,
            current_direction=0.0
        )

        results = env_forces.calculate_total_forces(
            conditions, sample_geometry, 250000
        )

        # Lateral force and moment should be minimal for head-on
        assert abs(results.wind_fy) < abs(results.wind_fx)

    def test_beam_wind(self, env_forces, sample_geometry):
        """Test with beam wind (90 degrees)."""
        conditions = EnvironmentalConditions(
            wind_speed=20.0,
            wind_direction=90.0,
            current_speed=1.0,
            current_direction=90.0
        )

        results = env_forces.calculate_total_forces(
            conditions, sample_geometry, 250000
        )

        # Lateral force should dominate for beam wind
        assert abs(results.wind_fy) > abs(results.wind_fx)

    def test_invalid_csv(self):
        """Test error handling for invalid CSV."""
        with pytest.raises(FileNotFoundError):
            OCIMFDatabase("nonexistent.csv")

    def test_missing_columns(self, tmp_path):
        """Test error handling for missing required columns."""
        bad_csv = tmp_path / "bad.csv"
        df = pd.DataFrame({
            'heading': [0, 45, 90],
            'displacement': [250000, 250000, 250000]
            # Missing CXw, CYw, etc.
        })
        df.to_csv(bad_csv, index=False)

        with pytest.raises(ValueError, match="Missing required columns"):
            OCIMFDatabase(str(bad_csv))


@pytest.mark.benchmark
class TestPerformance:
    """Performance benchmark tests."""

    def test_interpolation_performance(self, ocimf_db, benchmark):
        """Benchmark coefficient interpolation."""
        def interpolate():
            return ocimf_db.get_coefficients(45.0, 275000)

        result = benchmark(interpolate)
        assert isinstance(result, OCIMFCoefficients)

    def test_force_calculation_performance(self, env_forces, sample_conditions,
                                          sample_geometry, benchmark):
        """Benchmark force calculation."""
        def calculate():
            return env_forces.calculate_total_forces(
                sample_conditions,
                sample_geometry,
                250000
            )

        result = benchmark(calculate)
        assert isinstance(result, EnvironmentalForceResults)


@pytest.mark.integration
class TestIntegration:
    """Integration tests combining multiple components."""

    def test_full_workflow(self, sample_database_path, tmp_path):
        """Test complete workflow from database to charts."""
        # 1. Load database
        db = OCIMFDatabase(sample_database_path)

        # 2. Create force calculator
        calc = EnvironmentalForces(db)

        # 3. Define conditions
        conditions = EnvironmentalConditions(
            wind_speed=25.0,
            wind_direction=60.0,
            current_speed=2.0,
            current_direction=45.0
        )

        geometry = VesselGeometry(loa=330, beam=60, draft=22)

        # 4. Calculate forces
        results = calc.calculate_total_forces(conditions, geometry, 300000)

        # 5. Generate all charts
        db.plot_coefficient_surface(
            'CYw',
            save_path=str(tmp_path / "surface.png")
        )

        db.plot_polar_diagram(
            300000,
            save_path=str(tmp_path / "polar.png")
        )

        calc.plot_force_diagram(
            results,
            save_path=str(tmp_path / "forces.png")
        )

        # Verify all outputs
        assert (tmp_path / "surface.png").exists()
        assert (tmp_path / "polar.png").exists()
        assert (tmp_path / "forces.png").exists()

        # Verify results make sense
        assert results.total_fx != 0
        assert results.total_fy != 0
        assert results.total_mz != 0


if __name__ == '__main__':
    pytest.main([__file__, '-v', '--tb=short'])
