#!/usr/bin/env python3
"""
Unit Tests for AQWA .LIS Parser

Tests the AQWA parser with real .LIS file data.
"""

import pytest
import numpy as np
from pathlib import Path

from digitalmodel.modules.diffraction.aqwa_lis_parser import (
    AQWALISParser,
    parse_aqwa_lis_file
)


# Test data path
TEST_DATA_DIR = Path("docs/modules/aqwa/examples/03_dat/001_ship_raos")
TEST_LIS_FILE = TEST_DATA_DIR / "001_SHIP_RAOS.LIS"


class TestAQWALISParser:
    """Test suite for AQWA .LIS parser"""

    @pytest.fixture
    def parser(self):
        """Create parser instance with test file"""
        if not TEST_LIS_FILE.exists():
            pytest.skip(f"Test data not available: {TEST_LIS_FILE}")
        return AQWALISParser(TEST_LIS_FILE)

    def test_parser_initialization(self, parser):
        """Test parser initializes correctly"""
        assert parser.lis_file.exists()
        assert len(parser.content) > 0
        assert "AQWA" in parser.content

    def test_extract_frequencies_and_periods(self, parser):
        """Test frequency extraction"""
        frequencies, periods = parser.extract_frequencies_and_periods()

        # Check we got data
        assert len(frequencies) > 0
        assert len(periods) == len(frequencies)

        # Check frequencies are positive
        assert all(f > 0 for f in frequencies)

        # Check periods match frequencies (2π/ω)
        for freq, period in zip(frequencies, periods):
            expected_period = 2 * np.pi / freq
            assert abs(period - expected_period) < 0.01

    def test_extract_headings(self, parser):
        """Test heading extraction"""
        headings = parser.extract_headings()

        # Check we got headings
        assert len(headings) > 0

        # Check headings in valid range
        assert all(-180 <= h <= 180 for h in headings)

        # Check headings are sorted
        assert headings == sorted(headings)

    def test_parse_added_mass_table(self, parser):
        """Test added mass matrix parsing"""
        added_mass = parser.parse_added_mass_table()

        # Check we got matrices
        assert len(added_mass) > 0

        # Check each matrix
        for freq, matrix in added_mass.items():
            # Frequency should be positive
            assert freq > 0

            # Matrix should be 6x6
            assert matrix.shape == (6, 6)

            # Matrix should be symmetric
            assert np.allclose(matrix, matrix.T, rtol=0.01)

            # Diagonal terms should be non-negative (or very small negative due to numerical errors)
            diag = np.diag(matrix)
            assert all(d > -abs(d) * 0.01 for d in diag)

    def test_parse_damping_table(self, parser):
        """Test damping matrix parsing"""
        damping = parser.parse_damping_table()

        # Check we got matrices
        assert len(damping) > 0

        # Check each matrix
        for freq, matrix in damping.items():
            # Frequency should be positive
            assert freq > 0

            # Matrix should be 6x6
            assert matrix.shape == (6, 6)

            # Matrix should be symmetric
            assert np.allclose(matrix, matrix.T, rtol=0.01)

            # Diagonal terms should be non-negative
            diag = np.diag(matrix)
            assert all(d >= -abs(d) * 0.01 for d in diag)

    def test_parse_rao_table(self, parser):
        """Test RAO table parsing"""
        raos = parser.parse_rao_table()

        # Check we got RAO data
        assert len(raos) > 0

        # Check each RAO entry
        for (freq, heading), dof_data in raos.items():
            # Check key format
            assert isinstance(freq, float) and freq > 0
            assert isinstance(heading, float) and -180 <= heading <= 180

            # Check all DOFs present
            assert 'surge' in dof_data
            assert 'sway' in dof_data
            assert 'heave' in dof_data
            assert 'roll' in dof_data
            assert 'pitch' in dof_data
            assert 'yaw' in dof_data

            # Check each DOF has amplitude and phase
            for dof_name, (amp, phase) in dof_data.items():
                assert amp >= 0  # Amplitude should be non-negative
                assert isinstance(phase, float)  # Phase should be a number

    def test_parse_all(self, parser):
        """Test complete parsing"""
        data = parser.parse_all()

        # Check all keys present
        assert 'frequencies' in data
        assert 'periods' in data
        assert 'headings' in data
        assert 'added_mass' in data
        assert 'damping' in data
        assert 'raos' in data

        # Check data consistency
        n_freqs = len(data['frequencies'])
        assert len(data['periods']) == n_freqs
        assert len(data['added_mass']) <= n_freqs  # May have fewer matrices
        assert len(data['damping']) <= n_freqs

    def test_standalone_function(self):
        """Test standalone parse function"""
        if not TEST_LIS_FILE.exists():
            pytest.skip(f"Test data not available: {TEST_LIS_FILE}")

        data = parse_aqwa_lis_file(TEST_LIS_FILE)

        # Check parsing succeeded
        assert len(data['frequencies']) > 0
        assert len(data['headings']) > 0
        assert len(data['raos']) > 0


class TestAQWAParserEdgeCases:
    """Test edge cases and error handling"""

    def test_missing_file(self):
        """Test error handling for missing file"""
        with pytest.raises(FileNotFoundError):
            AQWALISParser(Path("nonexistent.LIS"))

    def test_empty_file(self, tmp_path):
        """Test error handling for empty file"""
        empty_file = tmp_path / "empty.LIS"
        empty_file.write_text("")

        parser = AQWALISParser(empty_file)

        with pytest.raises(ValueError):
            parser.extract_frequencies_and_periods()

    def test_invalid_lis_file(self, tmp_path):
        """Test error handling for invalid .LIS file"""
        invalid_file = tmp_path / "invalid.LIS"
        invalid_file.write_text("This is not a valid AQWA .LIS file")

        parser = AQWALISParser(invalid_file)

        with pytest.raises(ValueError):
            parser.extract_frequencies_and_periods()


class TestAQWAParserDataQuality:
    """Test data quality and physical validity"""

    @pytest.fixture
    def parsed_data(self):
        """Parse test file once for all tests"""
        if not TEST_LIS_FILE.exists():
            pytest.skip(f"Test data not available: {TEST_LIS_FILE}")
        return parse_aqwa_lis_file(TEST_LIS_FILE)

    def test_frequency_range(self, parsed_data):
        """Test frequency range is reasonable"""
        freqs = parsed_data['frequencies']

        # Should have some low frequencies for wave analysis
        assert min(freqs) < 0.5  # At least one freq below 0.5 rad/s

        # Should have some higher frequencies
        assert max(freqs) > 0.5  # At least one freq above 0.5 rad/s

    def test_heading_coverage(self, parsed_data):
        """Test heading coverage"""
        headings = parsed_data['headings']

        # Should have reasonable coverage
        assert len(headings) >= 3  # At least 3 headings

        # Should span at least 90 degrees
        heading_range = max(headings) - min(headings)
        assert heading_range >= 90

    def test_rao_physical_validity(self, parsed_data):
        """Test RAO values are physically reasonable"""
        raos = parsed_data['raos']

        for (freq, heading), dof_data in raos.items():
            # Surge/sway/heave amplitudes typically < 10 m/m
            for dof in ['surge', 'sway', 'heave']:
                amp, _ = dof_data[dof]
                assert amp < 20.0  # Very generous limit

            # Roll/pitch/yaw amplitudes typically < 10 deg/m
            for dof in ['roll', 'pitch', 'yaw']:
                amp, _ = dof_data[dof]
                assert amp < 50.0  # Very generous limit

    def test_matrix_symmetry(self, parsed_data):
        """Test matrix symmetry"""
        # Check added mass
        for freq, matrix in parsed_data['added_mass'].items():
            assert np.allclose(matrix, matrix.T, rtol=0.02)

        # Check damping
        for freq, matrix in parsed_data['damping'].items():
            assert np.allclose(matrix, matrix.T, rtol=0.02)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
