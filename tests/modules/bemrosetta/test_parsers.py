"""
Tests for BEMRosetta parsers module.

Tests cover:
- BaseParser: Base class functionality
- AQWAParser: AQWA .LIS file parsing
- QTFParser: QTF file parsing for second-order forces

Following TDD - these tests define the expected behavior.
"""

import pytest
import numpy as np
from pathlib import Path
from unittest.mock import patch, mock_open

from digitalmodel.modules.bemrosetta.parsers import BaseParser, AQWAParser, QTFParser
from digitalmodel.modules.bemrosetta.core.exceptions import ParserError
from digitalmodel.modules.bemrosetta.models import BEMSolverMetadata, QTFData, QTFType
from digitalmodel.modules.bemrosetta.models.solver_metadata import BEMSolverType


# ============================================================================
# Fixtures
# ============================================================================


@pytest.fixture
def temp_lis_file(tmp_path):
    """Create a minimal AQWA .LIS file for testing."""
    content = """
AQWA 19.2

STRUCTURE NAME: TestVessel

WATER DEPTH = 100.00

ADDED MASS-VARIATION WITH WAVE PERIOD/FREQUENCY

  PERIOD   FREQ     M11        M22        M33        M44        M55        M66        M13        M15        M24        M26        M35        M46
  ------   ----     ---        ---        ---        ---        ---        ---        ---        ---        ---        ---        ---        ---
   22.00   0.286   2.33E+06   2.93E+07   1.80E+08   1.00E+09   2.00E+09   5.00E+08   1.00E+05   2.00E+06   3.00E+05   4.00E+06   5.00E+05   6.00E+06
   19.00   0.331   2.40E+06   3.00E+07   1.85E+08   1.05E+09   2.05E+09   5.10E+08   1.10E+05   2.10E+06   3.10E+05   4.10E+06   5.10E+05   6.10E+06
   16.00   0.393   2.50E+06   3.10E+07   1.90E+08   1.10E+09   2.10E+09   5.20E+08   1.20E+05   2.20E+06   3.20E+05   4.20E+06   5.20E+05   6.20E+06

DAMPING-VARIATION WITH WAVE PERIOD/FREQUENCY

  PERIOD   FREQ     C11        C22        C33        C44        C55        C66        C13        C15        C24        C26        C35        C46
  ------   ----     ---        ---        ---        ---        ---        ---        ---        ---        ---        ---        ---        ---
   22.00   0.286   1.00E+05   2.00E+05   3.00E+05   4.00E+05   5.00E+05   6.00E+05   1.00E+04   2.00E+04   3.00E+04   4.00E+04   5.00E+04   6.00E+04
   19.00   0.331   1.10E+05   2.10E+05   3.10E+05   4.10E+05   5.10E+05   6.10E+05   1.10E+04   2.10E+04   3.10E+04   4.10E+04   5.10E+04   6.10E+04
   16.00   0.393   1.20E+05   2.20E+05   3.20E+05   4.20E+05   5.20E+05   6.20E+05   1.20E+04   2.20E+04   3.20E+04   4.20E+04   5.20E+04   6.20E+04

R.A.O.S-VARIATION WITH WAVE PERIOD/FREQUENCY

  PERIOD   FREQ   DIRECTION    X-AMP   X-PHA    Y-AMP   Y-PHA    Z-AMP   Z-PHA   RX-AMP  RX-PHA   RY-AMP  RY-PHA   RZ-AMP  RZ-PHA
  ------   ----   ---------    -----   -----    -----   -----    -----   -----   ------  ------   ------  ------   ------  ------
   22.00   0.286   -180.00    0.8926  -90.20   0.0100   0.00    0.9500  -85.00   0.0500  -45.00   1.0000  -88.00   0.0050  -10.00
   19.00   0.331              0.8500  -89.00   0.0120   1.00    0.9200  -84.00   0.0550  -44.00   1.0500  -87.00   0.0060  -11.00
   16.00   0.393              0.8000  -88.00   0.0140   2.00    0.9000  -83.00   0.0600  -43.00   1.1000  -86.00   0.0070  -12.00
   22.00   0.286   -135.00    0.7500  -70.00   0.3000  -20.00   0.9300  -80.00   0.1500  -50.00   0.9000  -75.00   0.1000  -30.00
   19.00   0.331              0.7200  -69.00   0.3100  -19.00   0.9100  -79.00   0.1550  -49.00   0.9200  -74.00   0.1050  -29.00
   16.00   0.393              0.6900  -68.00   0.3200  -18.00   0.8900  -78.00   0.1600  -48.00   0.9400  -73.00   0.1100  -28.00
"""
    lis_file = tmp_path / "test_vessel.LIS"
    lis_file.write_text(content)
    return lis_file


@pytest.fixture
def temp_qtf_file(tmp_path):
    """Create a minimal QTF file for testing."""
    content = """QTF DIFFERENCE FREQUENCY
STRUCTURE: TestVessel
NFREQ1: 3
NFREQ2: 3
NHEADING: 2
FREQUENCIES1: 0.286 0.331 0.393
FREQUENCIES2: 0.286 0.331 0.393
HEADINGS: 0.0 180.0
DOF: 1
HEADING: 0.0
REAL:
  1.00E+03  1.10E+03  1.20E+03
  1.10E+03  1.50E+03  1.60E+03
  1.20E+03  1.60E+03  2.00E+03
IMAG:
  0.00E+00  1.00E+02  2.00E+02
  -1.00E+02  0.00E+00  1.50E+02
  -2.00E+02  -1.50E+02  0.00E+00
DOF: 1
HEADING: 180.0
REAL:
  9.00E+02  1.00E+03  1.10E+03
  1.00E+03  1.40E+03  1.50E+03
  1.10E+03  1.50E+03  1.90E+03
IMAG:
  0.00E+00  9.00E+01  1.80E+02
  -9.00E+01  0.00E+00  1.35E+02
  -1.80E+02  -1.35E+02  0.00E+00
"""
    qtf_file = tmp_path / "test_vessel.qtf"
    qtf_file.write_text(content)
    return qtf_file


@pytest.fixture
def temp_sum_qtf_file(tmp_path):
    """Create a QTF file with sum-frequency data."""
    content = """QTF SUM FREQUENCY
STRUCTURE: TestVessel
NFREQ1: 2
NFREQ2: 2
NHEADING: 1
FREQUENCIES1: 0.5 1.0
FREQUENCIES2: 0.5 1.0
HEADINGS: 0.0
DOF: 3
HEADING: 0.0
REAL:
  5.00E+02  6.00E+02
  6.00E+02  8.00E+02
IMAG:
  1.00E+02  1.50E+02
  1.50E+02  2.00E+02
"""
    qtf_file = tmp_path / "test_sum.QTF"
    qtf_file.write_text(content)
    return qtf_file


# ============================================================================
# BaseParser Tests
# ============================================================================


class TestBaseParser:
    """Tests for BaseParser class."""

    def test_base_parser_is_abstract(self):
        """BaseParser cannot be instantiated directly due to abstract methods."""
        with pytest.raises(TypeError):
            BaseParser()

    def test_metadata_property_initial_none(self, temp_lis_file):
        """Metadata should be None before parsing."""
        parser = AQWAParser()
        assert parser.metadata is None

    def test_can_parse_existing_file_with_valid_extension(self, temp_lis_file):
        """can_parse returns True for existing file with supported extension."""
        parser = AQWAParser()
        assert parser.can_parse(temp_lis_file) is True

    def test_can_parse_nonexistent_file(self, tmp_path):
        """can_parse returns False for non-existent files."""
        parser = AQWAParser()
        nonexistent = tmp_path / "nonexistent.LIS"
        assert parser.can_parse(nonexistent) is False

    def test_can_parse_wrong_extension(self, tmp_path):
        """can_parse returns False for files with wrong extension."""
        parser = AQWAParser()
        wrong_ext = tmp_path / "test.txt"
        wrong_ext.write_text("some content")
        assert parser.can_parse(wrong_ext) is False

    def test_validate_file_exists_raises_for_missing(self, tmp_path):
        """_validate_file_exists raises ParserError for missing files."""
        parser = AQWAParser()
        nonexistent = tmp_path / "nonexistent.LIS"
        with pytest.raises(ParserError) as exc_info:
            parser._validate_file_exists(nonexistent)
        assert "not found" in str(exc_info.value).lower()

    def test_validate_file_exists_passes_for_existing(self, temp_lis_file):
        """_validate_file_exists does not raise for existing files."""
        parser = AQWAParser()
        # Should not raise
        parser._validate_file_exists(temp_lis_file)


# ============================================================================
# AQWAParser Tests
# ============================================================================


class TestAQWAParser:
    """Tests for AQWAParser class."""

    def test_supported_extensions(self):
        """AQWAParser supports .lis and .LIS extensions."""
        parser = AQWAParser()
        assert ".lis" in parser.supported_extensions or ".LIS" in parser.supported_extensions
        # Case insensitive check
        extensions_lower = [ext.lower() for ext in parser.supported_extensions]
        assert ".lis" in extensions_lower

    def test_solver_name(self):
        """AQWAParser reports AQWA as solver name."""
        parser = AQWAParser()
        assert parser.solver_name == "AQWA"

    def test_parse_extracts_metadata(self, temp_lis_file):
        """Parse extracts metadata from AQWA file."""
        parser = AQWAParser()
        result = parser.parse(temp_lis_file)

        metadata = parser.metadata
        assert metadata is not None
        assert metadata.solver_type == BEMSolverType.AQWA
        assert metadata.water_depth == 100.0
        assert "TestVessel" in metadata.project_name or "TestVessel" in str(metadata.body_names)

    def test_parse_extracts_frequencies(self, temp_lis_file):
        """Parse extracts frequency data correctly."""
        parser = AQWAParser()
        result = parser.parse(temp_lis_file)

        assert result.frequencies is not None
        assert len(result.frequencies) >= 3
        # Check frequency values are reasonable (0.1 to 10 rad/s typically)
        assert all(0.1 <= f <= 10.0 for f in result.frequencies)

    def test_parse_extracts_headings(self, temp_lis_file):
        """Parse extracts heading data correctly."""
        parser = AQWAParser()
        result = parser.parse(temp_lis_file)

        assert result.headings is not None
        assert len(result.headings) >= 2
        # Headings should be in degrees (-180 to 360)
        assert all(-180 <= h <= 360 for h in result.headings)

    def test_parse_extracts_added_mass(self, temp_lis_file):
        """Parse extracts added mass matrices."""
        parser = AQWAParser()
        result = parser.parse(temp_lis_file)

        assert result.added_mass is not None
        assert len(result.added_mass) >= 3
        # Each matrix should be 6x6
        for freq, matrix in result.added_mass.items():
            assert matrix.shape == (6, 6)

    def test_parse_extracts_damping(self, temp_lis_file):
        """Parse extracts damping matrices."""
        parser = AQWAParser()
        result = parser.parse(temp_lis_file)

        assert result.damping is not None
        assert len(result.damping) >= 3
        # Each matrix should be 6x6
        for freq, matrix in result.damping.items():
            assert matrix.shape == (6, 6)

    def test_parse_extracts_raos(self, temp_lis_file):
        """Parse extracts RAO data."""
        parser = AQWAParser()
        result = parser.parse(temp_lis_file)

        assert result.raos is not None
        # Should have data for multiple frequency-heading combinations
        assert len(result.raos) >= 6

    def test_parse_rao_contains_all_dofs(self, temp_lis_file):
        """RAO data includes all 6 DOFs."""
        parser = AQWAParser()
        result = parser.parse(temp_lis_file)

        # Get first RAO entry
        first_key = next(iter(result.raos.keys()))
        rao_data = result.raos[first_key]

        expected_dofs = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        for dof in expected_dofs:
            assert dof in rao_data

    def test_parse_missing_file_raises_parser_error(self, tmp_path):
        """Parsing non-existent file raises ParserError."""
        parser = AQWAParser()
        nonexistent = tmp_path / "nonexistent.LIS"
        with pytest.raises(ParserError):
            parser.parse(nonexistent)

    def test_parse_accepts_path_string(self, temp_lis_file):
        """Parser accepts string path as input."""
        parser = AQWAParser()
        result = parser.parse(str(temp_lis_file))
        assert result is not None

    def test_added_mass_matrix_is_symmetric(self, temp_lis_file):
        """Added mass matrices should be symmetric."""
        parser = AQWAParser()
        result = parser.parse(temp_lis_file)

        for freq, matrix in result.added_mass.items():
            assert np.allclose(matrix, matrix.T, rtol=1e-3), \
                f"Added mass matrix at freq {freq} is not symmetric"

    def test_damping_matrix_is_symmetric(self, temp_lis_file):
        """Damping matrices should be symmetric."""
        parser = AQWAParser()
        result = parser.parse(temp_lis_file)

        for freq, matrix in result.damping.items():
            assert np.allclose(matrix, matrix.T, rtol=1e-3), \
                f"Damping matrix at freq {freq} is not symmetric"


# ============================================================================
# QTFParser Tests
# ============================================================================


class TestQTFParser:
    """Tests for QTFParser class."""

    def test_supported_extensions(self):
        """QTFParser supports .qtf and .QTF extensions."""
        parser = QTFParser()
        extensions_lower = [ext.lower() for ext in parser.supported_extensions]
        assert ".qtf" in extensions_lower

    def test_solver_name(self):
        """QTFParser reports appropriate solver name."""
        parser = QTFParser()
        assert "QTF" in parser.solver_name.upper() or "AQWA" in parser.solver_name.upper()

    def test_parse_returns_qtf_data(self, temp_qtf_file):
        """Parse returns QTFData object."""
        parser = QTFParser()
        result = parser.parse(temp_qtf_file)

        assert isinstance(result, QTFData)

    def test_parse_extracts_qtf_type_difference(self, temp_qtf_file):
        """Parse correctly identifies difference-frequency QTF."""
        parser = QTFParser()
        result = parser.parse(temp_qtf_file)

        assert result.qtf_type == QTFType.DIFFERENCE_FREQUENCY

    def test_parse_extracts_qtf_type_sum(self, temp_sum_qtf_file):
        """Parse correctly identifies sum-frequency QTF."""
        parser = QTFParser()
        result = parser.parse(temp_sum_qtf_file)

        assert result.qtf_type == QTFType.SUM_FREQUENCY

    def test_parse_extracts_frequencies(self, temp_qtf_file):
        """Parse extracts frequency arrays."""
        parser = QTFParser()
        result = parser.parse(temp_qtf_file)

        assert len(result.frequencies) >= 2
        assert all(f > 0 for f in result.frequencies)

    def test_parse_extracts_headings(self, temp_qtf_file):
        """Parse extracts heading array."""
        parser = QTFParser()
        result = parser.parse(temp_qtf_file)

        assert len(result.headings) >= 1

    def test_parse_extracts_components(self, temp_qtf_file):
        """Parse extracts QTF components."""
        parser = QTFParser()
        result = parser.parse(temp_qtf_file)

        assert len(result.components) >= 1
        # Each component should have valid data
        for comp in result.components:
            assert comp.amplitude.shape[0] > 0
            assert comp.amplitude.shape[1] > 0

    def test_parse_missing_file_raises_parser_error(self, tmp_path):
        """Parsing non-existent file raises ParserError."""
        parser = QTFParser()
        nonexistent = tmp_path / "nonexistent.qtf"
        with pytest.raises(ParserError):
            parser.parse(nonexistent)

    def test_qtf_component_has_matching_dimensions(self, temp_qtf_file):
        """QTF component arrays have matching dimensions."""
        parser = QTFParser()
        result = parser.parse(temp_qtf_file)

        for comp in result.components:
            n1 = len(comp.frequencies_1)
            n2 = len(comp.frequencies_2)
            assert comp.amplitude.shape == (n1, n2)
            assert comp.phase.shape == (n1, n2)

    def test_parse_body_name(self, temp_qtf_file):
        """Parse extracts body/structure name."""
        parser = QTFParser()
        result = parser.parse(temp_qtf_file)

        assert result.body_name is not None
        assert len(result.body_name) > 0

    def test_can_parse_with_valid_extension(self, temp_qtf_file):
        """can_parse returns True for valid QTF file."""
        parser = QTFParser()
        assert parser.can_parse(temp_qtf_file) is True

    def test_can_parse_with_invalid_extension(self, temp_lis_file):
        """can_parse returns False for non-QTF file."""
        parser = QTFParser()
        assert parser.can_parse(temp_lis_file) is False


# ============================================================================
# Integration Tests
# ============================================================================


class TestParserIntegration:
    """Integration tests for parser module."""

    def test_aqwa_parser_result_structure(self, temp_lis_file):
        """AQWA parser result has expected structure."""
        parser = AQWAParser()
        result = parser.parse(temp_lis_file)

        # Check result has required attributes
        assert hasattr(result, 'frequencies')
        assert hasattr(result, 'headings')
        assert hasattr(result, 'added_mass')
        assert hasattr(result, 'damping')
        assert hasattr(result, 'raos')

    def test_parsers_handle_path_objects(self, temp_lis_file, temp_qtf_file):
        """Both parsers handle Path objects correctly."""
        aqwa_parser = AQWAParser()
        qtf_parser = QTFParser()

        # Should work with Path objects
        aqwa_result = aqwa_parser.parse(temp_lis_file)
        qtf_result = qtf_parser.parse(temp_qtf_file)

        assert aqwa_result is not None
        assert qtf_result is not None

    def test_parsers_handle_string_paths(self, temp_lis_file, temp_qtf_file):
        """Both parsers handle string paths correctly."""
        aqwa_parser = AQWAParser()
        qtf_parser = QTFParser()

        # Should work with string paths
        aqwa_result = aqwa_parser.parse(str(temp_lis_file))
        qtf_result = qtf_parser.parse(str(temp_qtf_file))

        assert aqwa_result is not None
        assert qtf_result is not None


# ============================================================================
# Edge Cases
# ============================================================================


class TestEdgeCases:
    """Tests for edge cases and error handling."""

    def test_aqwa_parser_with_infinite_water_depth(self, tmp_path):
        """Parser handles INFINITE water depth."""
        content = """
AQWA 19.2
WATER DEPTH = INFINITE
STRUCTURE NAME: DeepWaterVessel

ADDED MASS-VARIATION WITH WAVE PERIOD/FREQUENCY
   22.00   0.286   2.33E+06   2.93E+07   1.80E+08   1.00E+09   2.00E+09   5.00E+08   1.00E+05   2.00E+06   3.00E+05   4.00E+06   5.00E+05   6.00E+06

DAMPING-VARIATION WITH WAVE PERIOD/FREQUENCY
   22.00   0.286   1.00E+05   2.00E+05   3.00E+05   4.00E+05   5.00E+05   6.00E+05   1.00E+04   2.00E+04   3.00E+04   4.00E+04   5.00E+04   6.00E+04

R.A.O.S-VARIATION WITH WAVE PERIOD/FREQUENCY
   22.00   0.286   -180.00    0.8926  -90.20   0.0100   0.00    0.9500  -85.00   0.0500  -45.00   1.0000  -88.00   0.0050  -10.00
"""
        lis_file = tmp_path / "deep.LIS"
        lis_file.write_text(content)

        parser = AQWAParser()
        result = parser.parse(lis_file)

        assert parser.metadata.water_depth == float('inf')

    def test_empty_file_raises_error(self, tmp_path):
        """Parser raises error for empty file."""
        empty_file = tmp_path / "empty.LIS"
        empty_file.write_text("")

        parser = AQWAParser()
        with pytest.raises(ParserError):
            parser.parse(empty_file)

    def test_malformed_lis_file(self, tmp_path):
        """Parser handles malformed LIS file gracefully."""
        content = """This is not a valid AQWA file
Random text here
No structured data
"""
        bad_file = tmp_path / "bad.LIS"
        bad_file.write_text(content)

        parser = AQWAParser()
        with pytest.raises(ParserError):
            parser.parse(bad_file)

    def test_qtf_parser_handles_empty_file(self, tmp_path):
        """QTF parser raises error for empty file."""
        empty_file = tmp_path / "empty.qtf"
        empty_file.write_text("")

        parser = QTFParser()
        with pytest.raises(ParserError):
            parser.parse(empty_file)
