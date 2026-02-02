"""
Comprehensive Test Suite for Hydrodynamic Coefficients Module

Tests all functionality including:
- CoefficientDatabase loading and interpolation
- AQWA parsing
- Visualization methods
- Validation and error handling
"""

import pytest
import numpy as np
import pandas as pd
from pathlib import Path
import tempfile
import shutil

from digitalmodel.marine_analysis.hydrodynamic_coefficients import (
    CoefficientDatabase,
    FrequencyDependentMatrix,
    KramersKronigValidator,
    AQWAParser,
    HydrodynamicPlotter,
    DOF_NAMES,
    DOF_INDEX
)


class TestFrequencyDependentMatrix:
    """Test suite for FrequencyDependentMatrix class."""

    def test_initialization_valid(self):
        """Test valid matrix initialization."""
        matrix = np.random.rand(6, 6)
        fdm = FrequencyDependentMatrix(
            frequency=0.5,
            matrix=matrix,
            matrix_type='added_mass'
        )
        assert fdm.frequency == 0.5
        assert fdm.matrix_type == 'added_mass'
        assert fdm.matrix.shape == (6, 6)

    def test_initialization_invalid_shape(self):
        """Test invalid matrix shape raises error."""
        matrix = np.random.rand(5, 5)
        with pytest.raises(ValueError, match="Matrix must be 6×6"):
            FrequencyDependentMatrix(
                frequency=0.5,
                matrix=matrix,
                matrix_type='added_mass'
            )

    def test_initialization_invalid_type(self):
        """Test invalid matrix type raises error."""
        matrix = np.random.rand(6, 6)
        with pytest.raises(ValueError, match="Invalid matrix type"):
            FrequencyDependentMatrix(
                frequency=0.5,
                matrix=matrix,
                matrix_type='invalid'
            )

    def test_get_coefficient_by_index(self):
        """Test getting coefficient by DOF index."""
        matrix = np.arange(36).reshape(6, 6)
        fdm = FrequencyDependentMatrix(
            frequency=0.5,
            matrix=matrix,
            matrix_type='added_mass'
        )
        assert fdm.get_coefficient(0, 0) == 0
        assert fdm.get_coefficient(2, 3) == 15
        assert fdm.get_coefficient(5, 5) == 35

    def test_get_coefficient_by_name(self):
        """Test getting coefficient by DOF name."""
        matrix = np.arange(36).reshape(6, 6)
        fdm = FrequencyDependentMatrix(
            frequency=0.5,
            matrix=matrix,
            matrix_type='added_mass'
        )
        assert fdm.get_coefficient('Surge', 'Surge') == 0
        assert fdm.get_coefficient('Heave', 'Roll') == 15
        assert fdm.get_coefficient('Yaw', 'Yaw') == 35

    def test_is_symmetric(self):
        """Test symmetry checking."""
        # Symmetric matrix
        matrix_sym = np.random.rand(6, 6)
        matrix_sym = (matrix_sym + matrix_sym.T) / 2
        fdm_sym = FrequencyDependentMatrix(
            frequency=0.5,
            matrix=matrix_sym,
            matrix_type='added_mass'
        )
        assert fdm_sym.is_symmetric()

        # Asymmetric matrix
        matrix_asym = np.random.rand(6, 6)
        fdm_asym = FrequencyDependentMatrix(
            frequency=0.5,
            matrix=matrix_asym,
            matrix_type='added_mass'
        )
        assert not fdm_asym.is_symmetric()

    def test_get_diagonal(self):
        """Test diagonal extraction."""
        matrix = np.arange(36).reshape(6, 6)
        fdm = FrequencyDependentMatrix(
            frequency=0.5,
            matrix=matrix,
            matrix_type='added_mass'
        )
        diagonal = fdm.get_diagonal()
        expected = np.array([0, 7, 14, 21, 28, 35])
        np.testing.assert_array_equal(diagonal, expected)

    def test_get_off_diagonal(self):
        """Test off-diagonal extraction."""
        matrix = np.eye(6)
        fdm = FrequencyDependentMatrix(
            frequency=0.5,
            matrix=matrix,
            matrix_type='added_mass'
        )
        off_diag = fdm.get_off_diagonal()
        assert np.all(off_diag == 0)
        assert len(off_diag) == 30  # 6*6 - 6 = 30


class TestCoefficientDatabase:
    """Test suite for CoefficientDatabase class."""

    @pytest.fixture
    def sample_database(self, tmp_path):
        """Create sample database with test data."""
        # Create test CSV files
        frequencies = [0.1, 0.2, 0.3, 0.4, 0.5]

        for freq in frequencies:
            # Added mass matrix
            matrix_am = np.random.rand(6, 6) * 1000
            matrix_am = (matrix_am + matrix_am.T) / 2  # Make symmetric
            df_am = pd.DataFrame(matrix_am, index=DOF_NAMES, columns=DOF_NAMES)
            df_am.to_csv(tmp_path / f'added_mass_omega_{freq:.4f}.csv')

            # Damping matrix
            matrix_damp = np.random.rand(6, 6) * 100
            matrix_damp = (matrix_damp + matrix_damp.T) / 2  # Make symmetric
            df_damp = pd.DataFrame(matrix_damp, index=DOF_NAMES, columns=DOF_NAMES)
            df_damp.to_csv(tmp_path / f'damping_omega_{freq:.4f}.csv')

        return CoefficientDatabase.from_csv(tmp_path)

    def test_from_csv_loading(self, sample_database):
        """Test loading database from CSV files."""
        assert len(sample_database.frequencies) == 5
        assert len(sample_database.added_mass_matrices) == 5
        assert len(sample_database.damping_matrices) == 5

    def test_frequency_range(self, sample_database):
        """Test frequency range retrieval."""
        min_freq, max_freq = sample_database.get_frequency_range()
        assert min_freq == 0.1
        assert max_freq == 0.5

    def test_interpolation_added_mass(self, sample_database):
        """Test added mass interpolation."""
        # Interpolate at mid-point
        value = sample_database.get_added_mass(0.25, 0, 0)
        assert isinstance(value, float)
        assert value > 0

    def test_interpolation_damping(self, sample_database):
        """Test damping interpolation."""
        # Interpolate at mid-point
        value = sample_database.get_damping(0.25, 0, 0)
        assert isinstance(value, float)
        assert value > 0

    def test_get_matrix_at_frequency(self, sample_database):
        """Test getting full matrix at frequency."""
        matrix_am = sample_database.get_added_mass_matrix(0.3)
        assert matrix_am.shape == (6, 6)

        matrix_damp = sample_database.get_damping_matrix(0.3)
        assert matrix_damp.shape == (6, 6)

    def test_critical_damping_calculation(self, sample_database):
        """Test critical damping ratio calculation."""
        mass = 1000.0
        stiffness = 5000.0
        ratio = sample_database.calculate_critical_damping_ratio(
            mass, stiffness, 0.3, 'Heave'
        )
        assert isinstance(ratio, float)
        assert ratio >= 0

    def test_coefficient_statistics(self, sample_database):
        """Test statistical summary generation."""
        stats_am = sample_database.get_coefficient_statistics('added_mass')
        assert isinstance(stats_am, pd.DataFrame)
        assert len(stats_am) == 36  # 6x6 DOF pairs
        assert 'Mean' in stats_am.columns
        assert 'Std' in stats_am.columns

        stats_damp = sample_database.get_coefficient_statistics('damping')
        assert isinstance(stats_damp, pd.DataFrame)
        assert len(stats_damp) == 36

    def test_export_to_csv(self, sample_database, tmp_path):
        """Test exporting database to CSV."""
        output_dir = tmp_path / 'export'
        sample_database.export_to_csv(output_dir)

        # Check files were created
        am_files = list(output_dir.glob('added_mass_*.csv'))
        damp_files = list(output_dir.glob('damping_*.csv'))

        assert len(am_files) == 5
        assert len(damp_files) == 5


class TestKramersKronigValidator:
    """Test suite for Kramers-Kronig validation."""

    def test_validate_causal_system(self):
        """Test validation of causal system."""
        frequencies = np.linspace(0.1, 2.0, 50)

        # Create physically causal system
        added_mass = 1000 * np.ones_like(frequencies)  # Constant added mass
        damping = 10 * frequencies  # Linear damping with frequency

        is_valid, error = KramersKronigValidator.validate_pair(
            frequencies, added_mass, damping, tolerance=0.5
        )

        assert isinstance(is_valid, bool)
        assert isinstance(error, float)
        assert error >= 0


class TestAQWAParser:
    """Test suite for AQWA parser."""

    @pytest.fixture
    def sample_aqwa_file(self, tmp_path):
        """Create sample AQWA .LIS file."""
        content = """
FREQUENCY = 0.5000

ADDED MASS MATRIX
  1  100.0  0.0  0.0  0.0  10.0  0.0
  2  0.0  200.0  0.0  5.0  0.0  0.0
  3  0.0  0.0  300.0  0.0  15.0  0.0
  4  0.0  5.0  0.0  20.0  0.0  0.0
  5  10.0  0.0  15.0  0.0  25.0  0.0
  6  0.0  0.0  0.0  0.0  0.0  30.0

DAMPING MATRIX
  1  1.0  0.0  0.0  0.0  0.1  0.0
  2  0.0  2.0  0.0  0.05  0.0  0.0
  3  0.0  0.0  3.0  0.0  0.15  0.0
  4  0.0  0.05  0.0  0.2  0.0  0.0
  5  0.1  0.0  0.15  0.0  0.25  0.0
  6  0.0  0.0  0.0  0.0  0.0  0.3

FREQUENCY = 1.0000

ADDED MASS MATRIX
  1  110.0  0.0  0.0  0.0  11.0  0.0
  2  0.0  210.0  0.0  5.5  0.0  0.0
  3  0.0  0.0  310.0  0.0  16.0  0.0
  4  0.0  5.5  0.0  21.0  0.0  0.0
  5  11.0  0.0  16.0  0.0  26.0  0.0
  6  0.0  0.0  0.0  0.0  0.0  31.0
        """

        file_path = tmp_path / 'test.lis'
        file_path.write_text(content)
        return file_path

    def test_parser_initialization(self, sample_aqwa_file):
        """Test parser initialization."""
        parser = AQWAParser(sample_aqwa_file)
        assert parser.file_path == sample_aqwa_file
        assert len(parser.lines) > 0

    def test_parse_creates_database(self, sample_aqwa_file):
        """Test parsing creates valid database."""
        parser = AQWAParser(sample_aqwa_file)
        db = parser.parse()

        assert isinstance(db, CoefficientDatabase)
        assert len(db.frequencies) >= 1

    def test_frequency_summary(self, sample_aqwa_file):
        """Test frequency summary generation."""
        parser = AQWAParser(sample_aqwa_file)
        summary = parser.get_frequency_summary()

        assert isinstance(summary, pd.DataFrame)
        assert 'Frequency (rad/s)' in summary.columns
        assert 'Period (s)' in summary.columns

    def test_data_quality_validation(self, sample_aqwa_file):
        """Test data quality validation."""
        parser = AQWAParser(sample_aqwa_file)
        results = parser.validate_data_quality()

        assert 'n_frequencies' in results
        assert 'frequency_range' in results
        assert isinstance(results['missing_added_mass'], list)
        assert isinstance(results['asymmetric_matrices'], list)


class TestHydrodynamicPlotter:
    """Test suite for visualization methods."""

    @pytest.fixture
    def plotter(self, tmp_path):
        """Create plotter with sample database."""
        # Create sample database
        frequencies = [0.1, 0.2, 0.3, 0.4, 0.5]

        for freq in frequencies:
            matrix_am = np.random.rand(6, 6) * 1000
            matrix_am = (matrix_am + matrix_am.T) / 2
            df_am = pd.DataFrame(matrix_am, index=DOF_NAMES, columns=DOF_NAMES)
            df_am.to_csv(tmp_path / f'added_mass_omega_{freq:.4f}.csv')

            matrix_damp = np.random.rand(6, 6) * 100
            matrix_damp = (matrix_damp + matrix_damp.T) / 2
            df_damp = pd.DataFrame(matrix_damp, index=DOF_NAMES, columns=DOF_NAMES)
            df_damp.to_csv(tmp_path / f'damping_omega_{freq:.4f}.csv')

        db = CoefficientDatabase.from_csv(tmp_path)
        return HydrodynamicPlotter(db)

    def test_plot_frequency_response(self, plotter, tmp_path):
        """Test frequency response plotting."""
        output = tmp_path / 'freq_response.png'
        fig = plotter.plot_frequency_response(
            dof='Heave',
            coefficient_type='both',
            save_path=output
        )
        assert output.exists()
        assert fig is not None

    def test_plot_coefficient_matrix(self, plotter, tmp_path):
        """Test coefficient matrix heatmap."""
        output = tmp_path / 'matrix.png'
        fig = plotter.plot_coefficient_matrix(
            frequency=0.3,
            coefficient_type='added_mass',
            save_path=output
        )
        assert output.exists()
        assert fig is not None

    def test_plot_added_mass_surface(self, plotter, tmp_path):
        """Test 3D surface plotting for added mass."""
        output = tmp_path / 'am_surface.png'
        fig = plotter.plot_added_mass_surface(
            dof_i='Heave',
            dof_j='Heave',
            save_path=output
        )
        assert output.exists()
        assert fig is not None

    def test_plot_damping_surface(self, plotter, tmp_path):
        """Test 3D surface plotting for damping."""
        output = tmp_path / 'damp_surface.png'
        fig = plotter.plot_damping_surface(
            dof_i='Heave',
            dof_j='Heave',
            save_path=output
        )
        assert output.exists()
        assert fig is not None

    def test_plot_cross_coupling(self, plotter, tmp_path):
        """Test cross-coupling terms plotting."""
        output = tmp_path / 'coupling.png'
        fig = plotter.plot_cross_coupling(
            coefficient_type='added_mass',
            threshold=0.01,
            save_path=output
        )
        assert output.exists()
        assert fig is not None

    def test_plot_critical_damping(self, plotter, tmp_path):
        """Test critical damping ratio plotting."""
        mass_matrix = np.diag([1000, 1000, 1000, 100, 100, 100])
        stiffness_matrix = np.diag([5000, 5000, 5000, 500, 500, 500])

        output = tmp_path / 'critical_damp.png'
        fig = plotter.plot_critical_damping(
            mass_matrix=mass_matrix,
            stiffness_matrix=stiffness_matrix,
            save_path=output
        )
        assert output.exists()
        assert fig is not None

    def test_plot_matrix_correlation(self, plotter, tmp_path):
        """Test correlation matrix plotting."""
        output = tmp_path / 'correlation.png'
        fig = plotter.plot_matrix_correlation(
            coefficient_type='added_mass',
            save_path=output
        )
        assert output.exists()
        assert fig is not None

    def test_plot_all_dofs_comparison(self, plotter, tmp_path):
        """Test all DOFs comparison plotting."""
        output = tmp_path / 'all_dofs.png'
        fig = plotter.plot_all_dofs_comparison(
            coefficient_type='added_mass',
            save_path=output
        )
        assert output.exists()
        assert fig is not None

    def test_animate_frequency_sweep(self, plotter, tmp_path):
        """Test animated frequency sweep creation."""
        output = tmp_path / 'animation.gif'
        plotter.animate_frequency_sweep(
            coefficient_type='added_mass',
            save_path=output,
            fps=2
        )
        assert output.exists()


class TestIntegration:
    """Integration tests for complete workflows."""

    def test_csv_to_visualization_workflow(self, tmp_path):
        """Test complete workflow from CSV to visualization."""
        # Create test data
        frequencies = [0.1, 0.3, 0.5]

        for freq in frequencies:
            matrix = np.random.rand(6, 6) * 1000
            matrix = (matrix + matrix.T) / 2
            df = pd.DataFrame(matrix, index=DOF_NAMES, columns=DOF_NAMES)
            df.to_csv(tmp_path / f'added_mass_omega_{freq:.4f}.csv')
            df.to_csv(tmp_path / f'damping_omega_{freq:.4f}.csv')

        # Load database
        db = CoefficientDatabase.from_csv(tmp_path)

        # Get coefficients
        value = db.get_added_mass(0.3, 'Heave', 'Heave')
        assert isinstance(value, float)

        # Create plots
        plotter = HydrodynamicPlotter(db)
        output = tmp_path / 'test_plot.png'
        fig = plotter.plot_frequency_response('Heave', save_path=output)

        assert output.exists()
        assert fig is not None


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
