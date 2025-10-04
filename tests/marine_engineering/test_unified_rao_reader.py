"""Comprehensive tests for Unified RAO Reader.

Tests cover:
- Happy path scenarios
- Error handling
- Edge cases
- Data validation
- Backward compatibility
"""

import pytest
import numpy as np
from pathlib import Path
import tempfile
import os

from digitalmodel.modules.marine_analysis import (
    UnifiedRAOReader,
    RAOReaderError,
    read_rao_file,
    RAOType,
    SourceFormat
)


class TestUnifiedRAOReader:
    """Test suite for UnifiedRAOReader."""

    @pytest.fixture
    def reader(self):
        """Create reader instance."""
        return UnifiedRAOReader()

    @pytest.fixture
    def sample_aqwa_lis_content(self):
        """Generate sample AQWA .lis file content with all RAO types."""
        return """
        AQWA-LINE Analysis Results

        VESSEL NAME: TEST_VESSEL

        R.A.O.S-VARIATION WITH WAVE DIRECTION

        PERIOD   FREQ     DIRECTION    X         Y         Z         RX        RY        RZ
        (SECS)  (RAD/S)   (DEGREES)  (M/M)(DEGS) (M/M)(DEGS) (M/M)(DEGS) (DEG/M)(DEGS) (DEG/M)(DEGS) (DEG/M)(DEGS)
        -----------------------------------------------------------------------------------------------
        10.00    0.628      0.00     1.000  0.00  0.100 90.00  0.800 -5.00  0.500 10.00  1.200  5.00  0.300 15.00
                          90.00     0.200 85.00  1.100  0.00  0.900 -8.00  1.500 20.00  0.400  8.00  0.600 25.00
                         180.00     1.050  5.00  0.150 95.00  0.750 -3.00  0.450 12.00  1.250  7.00  0.250 18.00

        VEL R.A.O.S-VARIATION WITH WAVE DIRECTION

        PERIOD   FREQ     DIRECTION    X         Y         Z         RX        RY        RZ
        (SECS)  (RAD/S)   (DEGREES)  (M/S/M)(DEGS) (M/S/M)(DEGS) (M/S/M)(DEGS) (DEG/S/M)(DEGS) (DEG/S/M)(DEGS) (DEG/S/M)(DEGS)
        -----------------------------------------------------------------------------------------------
        10.00    0.628      0.00     0.628  0.00  0.063 90.00  0.502 -5.00  0.314 10.00  0.754  5.00  0.188 15.00
                          90.00     0.126 85.00  0.691  0.00  0.565 -8.00  0.942 20.00  0.251  8.00  0.377 25.00

        ACC R.A.O.S-VARIATION WITH WAVE DIRECTION

        PERIOD   FREQ     DIRECTION    X         Y         Z         RX        RY        RZ
        (SECS)  (RAD/S)   (DEGREES)  (M/S2/M)(DEGS) (M/S2/M)(DEGS) (M/S2/M)(DEGS) (DEG/S2/M)(DEGS) (DEG/S2/M)(DEGS) (DEG/S2/M)(DEGS)
        -----------------------------------------------------------------------------------------------
        10.00    0.628      0.00     0.394  0.00  0.039 90.00  0.315 -5.00  0.197 10.00  0.474  5.00  0.118 15.00
        """

    @pytest.fixture
    def sample_orcaflex_yml_content(self):
        """Generate sample OrcaFlex YAML file content."""
        return """
VesselTypes:
  - Name: Vessel Type1
    Draughts:
      - Name: Draught1
        DisplacementRAOs:
          RAOs:
            - RAODirection: 0.0
              RAOPeriodOrFreq, Surge, Surge, Sway, Sway, Heave, Heave, Roll, Roll, Pitch, Pitch, Yaw, Yaw:
                - [10.0, 1.0, 0.0, 0.1, 90.0, 0.8, -5.0, 0.5, 10.0, 1.2, 5.0, 0.3, 15.0]
            - RAODirection: 90.0
              RAOPeriodOrFreq, Surge, Surge, Sway, Sway, Heave, Heave, Roll, Roll, Pitch, Pitch, Yaw, Yaw:
                - [10.0, 0.2, 85.0, 1.1, 0.0, 0.9, -8.0, 1.5, 20.0, 0.4, 8.0, 0.6, 25.0]
        """

    def test_read_aqwa_lis_all_types(self, reader, sample_aqwa_lis_content, tmp_path):
        """Test reading AQWA .lis file with all RAO types."""
        # Create temporary .lis file
        lis_file = tmp_path / "test_vessel.lis"
        lis_file.write_text(sample_aqwa_lis_content)

        # Read file
        rao_data = reader.read(str(lis_file))

        # Verify all types available
        assert rao_data.has_displacement()
        assert rao_data.has_velocity()
        assert rao_data.has_acceleration()

        available_types = rao_data.get_available_types()
        assert RAOType.DISPLACEMENT in available_types
        assert RAOType.VELOCITY in available_types
        assert RAOType.ACCELERATION in available_types

        # Verify displacement RAO data
        disp = rao_data.displacement
        assert len(disp.frequencies) == 1
        assert np.isclose(disp.frequencies[0], 0.628, rtol=0.01)
        assert len(disp.headings) == 3
        assert 0.0 in disp.headings
        assert 90.0 in disp.headings
        assert 180.0 in disp.headings

        # Verify surge data at 0 degrees
        surge_amp_0deg = disp.surge.amplitude[0, 0]
        assert np.isclose(surge_amp_0deg, 1.0, rtol=0.01)

    def test_read_aqwa_lis_displacement_only(self, reader, sample_aqwa_lis_content, tmp_path):
        """Test reading only displacement RAOs."""
        lis_file = tmp_path / "test_vessel.lis"
        lis_file.write_text(sample_aqwa_lis_content)

        rao_data = reader.read(
            str(lis_file),
            extract_displacement=True,
            extract_velocity=False,
            extract_acceleration=False
        )

        assert rao_data.has_displacement()
        assert not rao_data.has_velocity()
        assert not rao_data.has_acceleration()

    def test_read_orcaflex_yml(self, reader, sample_orcaflex_yml_content, tmp_path):
        """Test reading OrcaFlex YAML file."""
        yml_file = tmp_path / "test_vessel.yml"
        yml_file.write_text(sample_orcaflex_yml_content)

        rao_data = reader.read(str(yml_file))

        assert rao_data.has_displacement()
        assert rao_data.displacement.metadata.vessel_name == "Vessel Type1"
        assert len(rao_data.displacement.frequencies) > 0
        assert len(rao_data.displacement.headings) >= 2

    def test_file_not_found_error(self, reader):
        """Test error handling for non-existent file."""
        with pytest.raises(RAOReaderError) as exc_info:
            reader.read("nonexistent_file.lis")

        assert "File not found" in str(exc_info.value)
        assert len(exc_info.value.suggestions) > 0

    def test_invalid_format_error(self, reader, tmp_path):
        """Test error handling for invalid file format."""
        bad_file = tmp_path / "bad_file.txt"
        bad_file.write_text("This is not a valid RAO file")

        with pytest.raises(RAOReaderError):
            reader.read(str(bad_file))

    def test_convenience_function(self, sample_aqwa_lis_content, tmp_path):
        """Test convenience read_rao_file function."""
        lis_file = tmp_path / "test.lis"
        lis_file.write_text(sample_aqwa_lis_content)

        rao_data = read_rao_file(str(lis_file))

        assert rao_data is not None
        assert rao_data.has_displacement()

    def test_get_info(self, reader, sample_aqwa_lis_content, tmp_path):
        """Test get_info method."""
        lis_file = tmp_path / "test.lis"
        lis_file.write_text(sample_aqwa_lis_content)

        info = reader.get_info(str(lis_file))

        assert info['file_name'] == 'test.lis'
        assert info['format'] == 'aqwa_lis'
        assert info['exists'] is True
        assert info['readable'] is True
        assert info['file_size_mb'] >= 0

    def test_format_detection_lis(self, reader, tmp_path):
        """Test automatic format detection for .lis files."""
        lis_file = tmp_path / "test.lis"
        lis_file.write_text("AQWA Analysis")

        format_detected = reader._detect_format(Path(lis_file))
        assert format_detected == SourceFormat.AQWA_LIS

    def test_format_detection_yml(self, reader, tmp_path):
        """Test automatic format detection for .yml files."""
        yml_file = tmp_path / "test.yml"
        yml_file.write_text("VesselTypes:\n  - Name: Test")

        format_detected = reader._detect_format(Path(yml_file))
        assert format_detected == SourceFormat.ORCAFLEX_YML

    def test_data_model_to_dict_conversion(self, reader, sample_aqwa_lis_content, tmp_path):
        """Test backward compatibility through to_dict() conversion."""
        lis_file = tmp_path / "test.lis"
        lis_file.write_text(sample_aqwa_lis_content)

        rao_data = reader.read(str(lis_file))
        legacy_dict = rao_data.displacement.to_dict()

        assert 'frequencies' in legacy_dict
        assert 'headings' in legacy_dict
        assert 'raos' in legacy_dict
        assert 'surge' in legacy_dict['raos']
        assert 'amplitude' in legacy_dict['raos']['surge']
        assert 'phase' in legacy_dict['raos']['surge']

    def test_empty_file_handling(self, reader, tmp_path):
        """Test handling of empty files."""
        empty_file = tmp_path / "empty.lis"
        empty_file.write_text("")

        with pytest.raises(RAOReaderError):
            reader.read(str(empty_file))

    def test_unicode_handling(self, reader, tmp_path):
        """Test handling of files with unicode characters."""
        unicode_file = tmp_path / "unicode.lis"
        content = "AQWA Analysis\nVessel Name: Tëst Vëssël\n"
        unicode_file.write_text(content, encoding='utf-8')

        # Should not raise encoding errors
        try:
            reader.read(str(unicode_file))
        except RAOReaderError as e:
            # Expected to fail parsing but not encoding
            assert "unicode" not in str(e).lower()


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
