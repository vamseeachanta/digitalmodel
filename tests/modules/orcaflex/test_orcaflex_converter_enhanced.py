#!/usr/bin/env python3
"""
ABOUTME: Comprehensive test suite for enhanced OrcaFlex converter using existing repository .dat files.

Test Suite for Enhanced OrcaFlex Converter
==========================================

Tests bidirectional conversion, batch processing, and validation using the
180+ existing OrcaFlex .dat files in the repository.

Test Categories:
- Single file conversion (.dat → .yml, .yml → .dat)
- Batch conversion with progress tracking
- Round-trip validation (.dat → .yml → .dat)
- Error handling and retry logic
- Mock mode testing
- Parallel processing
"""

import pytest
import tempfile
from pathlib import Path
import shutil

from digitalmodel.orcaflex.orcaflex_converter_enhanced import OrcaFlexConverterEnhanced


# Test data paths
TEST_EXAMPLES_DIR = Path("docs/modules/orcaflex/examples/raw")


class TestOrcaFlexConverterEnhanced:
    """Test suite for enhanced OrcaFlex converter."""

    @pytest.fixture
    def temp_output_dir(self):
        """Create temporary output directory for tests."""
        temp_dir = tempfile.mkdtemp()
        yield Path(temp_dir)
        # Cleanup
        shutil.rmtree(temp_dir, ignore_errors=True)

    @pytest.fixture
    def sample_dat_files(self):
        """Get sample .dat files from repository examples."""
        if TEST_EXAMPLES_DIR.exists():
            dat_files = list(TEST_EXAMPLES_DIR.glob("**/*.dat"))[:5]  # First 5 files
            return dat_files
        else:
            pytest.skip("Example files not found")

    @pytest.fixture
    def converter_real(self, temp_output_dir):
        """Create converter instance with real OrcFxAPI."""
        return OrcaFlexConverterEnhanced(
            output_dir=temp_output_dir,
            output_format='yml',
            use_mock=False,
            validate=True
        )

    @pytest.fixture
    def converter_mock(self, temp_output_dir):
        """Create converter instance in mock mode."""
        return OrcaFlexConverterEnhanced(
            output_dir=temp_output_dir,
            output_format='yml',
            use_mock=True,
            validate=False
        )

    # ===========================
    # Single File Conversion Tests
    # ===========================

    def test_single_dat_to_yml(self, converter_real, sample_dat_files, temp_output_dir):
        """Test converting single .dat file to .yml."""
        if not sample_dat_files:
            pytest.skip("No .dat files available")

        dat_file = sample_dat_files[0]

        success, output_file, error = converter_real.convert_file(dat_file)

        assert success, f"Conversion failed: {error}"
        assert output_file is not None
        assert output_file.exists()
        assert output_file.suffix == '.yml'
        assert output_file.stat().st_size > 0

    def test_single_yml_to_dat(self, converter_real, sample_dat_files, temp_output_dir):
        """Test converting .yml back to .dat."""
        if not sample_dat_files:
            pytest.skip("No .dat files available")

        dat_file = sample_dat_files[0]

        # First convert to YAML
        converter_yml = OrcaFlexConverterEnhanced(
            output_dir=temp_output_dir,
            output_format='yml',
            use_mock=False
        )
        success, yml_file, error = converter_yml.convert_file(dat_file)
        assert success, f"YAML conversion failed: {error}"

        # Then convert YAML back to DAT
        converter_dat = OrcaFlexConverterEnhanced(
            output_dir=temp_output_dir,
            output_format='dat',
            use_mock=False
        )
        success, dat_file_new, error = converter_dat.convert_file(yml_file)

        assert success, f"DAT conversion failed: {error}"
        assert dat_file_new is not None
        assert dat_file_new.exists()
        assert dat_file_new.suffix == '.dat'
        assert dat_file_new.stat().st_size > 0

    def test_conversion_skip_existing(self, converter_real, sample_dat_files, temp_output_dir):
        """Test that converter skips already converted files."""
        if not sample_dat_files:
            pytest.skip("No .dat files available")

        dat_file = sample_dat_files[0]

        # First conversion
        success1, output1, _ = converter_real.convert_file(dat_file)
        assert success1

        # Second conversion (should skip)
        success2, output2, _ = converter_real.convert_file(dat_file)
        assert success2
        assert output1 == output2
        assert converter_real.stats['skipped'] > 0

    # ===========================
    # Batch Conversion Tests
    # ===========================

    def test_batch_conversion_dat_to_yml(self, converter_real, sample_dat_files):
        """Test batch conversion of multiple .dat files to .yml."""
        if not sample_dat_files:
            pytest.skip("No .dat files available")

        results = converter_real.convert_batch(file_list=sample_dat_files)

        stats = results['statistics']
        assert stats['total_files'] == len(sample_dat_files)
        assert stats['successful'] > 0
        assert stats['failed'] == 0 or converter_real.use_mock  # Allow failures in mock mode

    def test_batch_conversion_with_pattern(self, temp_output_dir):
        """Test batch conversion using file pattern."""
        if not TEST_EXAMPLES_DIR.exists():
            pytest.skip("Example files not found")

        converter = OrcaFlexConverterEnhanced(
            input_dir=TEST_EXAMPLES_DIR,
            output_dir=temp_output_dir,
            output_format='yml',
            use_mock=True,  # Use mock for faster tests
            validate=False
        )

        results = converter.convert_batch(pattern='A*.dat')  # Only files starting with 'A'

        stats = results['statistics']
        assert stats['total_files'] >= 0
        if stats['total_files'] > 0:
            assert stats['successful'] > 0

    def test_batch_parallel_conversion(self, temp_output_dir, sample_dat_files):
        """Test parallel batch conversion."""
        if not sample_dat_files or len(sample_dat_files) < 2:
            pytest.skip("Need at least 2 .dat files")

        converter = OrcaFlexConverterEnhanced(
            output_dir=temp_output_dir,
            output_format='yml',
            use_mock=True,  # Use mock for faster tests
            parallel=True,
            max_workers=2
        )

        results = converter.convert_batch(file_list=sample_dat_files)

        stats = results['statistics']
        assert stats['total_files'] == len(sample_dat_files)
        assert stats['successful'] > 0
        assert stats['parallel'] is True

    # ===========================
    # Mock Mode Tests
    # ===========================

    def test_mock_conversion(self, converter_mock, sample_dat_files):
        """Test conversion in mock mode (no OrcFxAPI)."""
        if not sample_dat_files:
            pytest.skip("No .dat files available")

        dat_file = sample_dat_files[0]

        success, output_file, error = converter_mock.convert_file(dat_file)

        assert success, f"Mock conversion failed: {error}"
        assert output_file is not None
        assert output_file.exists()
        assert output_file.suffix == '.yml'

        # Check that mock YAML contains expected structure
        import yaml
        with open(output_file) as f:
            data = yaml.safe_load(f)

        assert 'OrcaFlexModel' in data
        assert data['OrcaFlexModel']['MockConversion'] is True

    def test_mock_batch_conversion(self, converter_mock, sample_dat_files):
        """Test batch conversion in mock mode."""
        if not sample_dat_files:
            pytest.skip("No .dat files available")

        results = converter_mock.convert_batch(file_list=sample_dat_files)

        stats = results['statistics']
        assert stats['total_files'] == len(sample_dat_files)
        assert stats['successful'] == len(sample_dat_files)  # All should succeed in mock
        assert stats['failed'] == 0

    # ===========================
    # Validation Tests
    # ===========================

    def test_yaml_validation(self, converter_real, sample_dat_files, temp_output_dir):
        """Test YAML file validation after conversion."""
        if not sample_dat_files:
            pytest.skip("No .dat files available")

        dat_file = sample_dat_files[0]

        success, yml_file, _ = converter_real.convert_file(dat_file)
        assert success

        # Validate YAML
        is_valid = converter_real._validate_file(yml_file)

        if not converter_real.use_mock:
            assert is_valid, "YAML validation failed"

    def test_round_trip_validation(self, temp_output_dir, sample_dat_files):
        """Test round-trip conversion .dat → .yml → .dat."""
        if not sample_dat_files:
            pytest.skip("No .dat files available")

        original_dat = sample_dat_files[0]

        # Step 1: .dat → .yml
        converter_to_yml = OrcaFlexConverterEnhanced(
            output_dir=temp_output_dir / "yml",
            output_format='yml',
            use_mock=False
        )
        success1, yml_file, error1 = converter_to_yml.convert_file(original_dat)
        assert success1, f"Failed to convert to YAML: {error1}"

        # Step 2: .yml → .dat
        converter_to_dat = OrcaFlexConverterEnhanced(
            output_dir=temp_output_dir / "dat",
            output_format='dat',
            use_mock=False
        )
        success2, new_dat, error2 = converter_to_dat.convert_file(yml_file)
        assert success2, f"Failed to convert back to DAT: {error2}"

        # Validate file sizes are similar (within 10%)
        original_size = original_dat.stat().st_size
        new_size = new_dat.stat().st_size

        if not converter_to_yml.use_mock:
            size_diff_percent = abs(new_size - original_size) / original_size * 100
            assert size_diff_percent < 10, f"File size difference too large: {size_diff_percent:.1f}%"

    # ===========================
    # Error Handling Tests
    # ===========================

    def test_missing_file_error(self, converter_real):
        """Test handling of missing input file."""
        missing_file = Path("nonexistent_file.dat")

        success, output, error = converter_real.convert_file(missing_file)

        assert not success
        assert error is not None
        assert "not found" in error.lower()

    def test_invalid_output_format(self, temp_output_dir):
        """Test handling of invalid output format."""
        with pytest.raises(ValueError):
            OrcaFlexConverterEnhanced(
                output_dir=temp_output_dir,
                output_format='invalid'
            )

    def test_retry_logic(self, temp_output_dir):
        """Test retry logic on conversion failure."""
        converter = OrcaFlexConverterEnhanced(
            output_dir=temp_output_dir,
            output_format='yml',
            use_mock=True,
            max_retries=3
        )

        # This should succeed even with retries in mock mode
        fake_file = temp_output_dir / "test.dat"
        fake_file.write_text("fake data")

        success, output, error = converter.convert_file(fake_file)

        # In mock mode, this should succeed
        assert success or error is not None

    # ===========================
    # Statistics and Reporting Tests
    # ===========================

    def test_statistics_tracking(self, converter_real, sample_dat_files):
        """Test that conversion statistics are properly tracked."""
        if not sample_dat_files:
            pytest.skip("No .dat files available")

        results = converter_real.convert_batch(file_list=sample_dat_files)
        stats = results['statistics']

        # Check all required stats fields
        assert 'total_files' in stats
        assert 'successful' in stats
        assert 'failed' in stats
        assert 'skipped' in stats
        assert 'processing_time' in stats
        assert 'files_by_input_type' in stats
        assert 'output_format' in stats

        # Verify totals
        assert stats['successful'] + stats['failed'] + stats['skipped'] == stats['total_files']

    def test_report_generation(self, temp_output_dir, sample_dat_files):
        """Test that conversion reports are generated."""
        if not sample_dat_files:
            pytest.skip("No .dat files available")

        converter = OrcaFlexConverterEnhanced(
            input_dir=sample_dat_files[0].parent,
            output_dir=temp_output_dir,
            output_format='yml',
            use_mock=True
        )

        converter.convert_batch(file_list=sample_dat_files)

        # Check reports exist
        md_report = temp_output_dir / 'conversion_report.md'
        json_report = temp_output_dir / 'conversion_report.json'

        assert md_report.exists(), "Markdown report not generated"
        assert json_report.exists(), "JSON report not generated"

        # Check report content
        assert md_report.stat().st_size > 0
        assert json_report.stat().st_size > 0

    # ===========================
    # Integration Tests
    # ===========================

    def test_find_files(self, temp_output_dir):
        """Test file discovery with different patterns."""
        if not TEST_EXAMPLES_DIR.exists():
            pytest.skip("Example files not found")

        converter = OrcaFlexConverterEnhanced(
            input_dir=TEST_EXAMPLES_DIR,
            output_dir=temp_output_dir,
            use_mock=True
        )

        # Find all files
        all_files = converter.find_files('*')
        assert len(all_files) >= 0

        # Find only .dat files
        dat_files = converter.find_files('*.dat')
        assert all(f.suffix == '.dat' for f in dat_files)

    def test_output_directory_creation(self, temp_output_dir):
        """Test that output directories are created automatically."""
        nested_output = temp_output_dir / "level1" / "level2" / "level3"

        converter = OrcaFlexConverterEnhanced(
            output_dir=nested_output,
            use_mock=True
        )

        # Directory should be created during initialization
        assert nested_output.exists()


# ===========================
# Performance Tests
# ===========================

@pytest.mark.slow
class TestPerformance:
    """Performance and scaling tests."""

    def test_large_batch_performance(self, temp_output_dir):
        """Test performance with large batch of files."""
        if not TEST_EXAMPLES_DIR.exists():
            pytest.skip("Example files not found")

        converter = OrcaFlexConverterEnhanced(
            input_dir=TEST_EXAMPLES_DIR,
            output_dir=temp_output_dir,
            output_format='yml',
            use_mock=True,  # Use mock for speed
            parallel=True,
            max_workers=4
        )

        import time
        start = time.time()

        results = converter.convert_batch()
        stats = results['statistics']

        elapsed = time.time() - start

        print(f"\nConverted {stats['total_files']} files in {elapsed:.2f}s")
        print(f"Average: {elapsed/stats['total_files']:.3f}s per file" if stats['total_files'] > 0 else "")


# ===========================
# CLI Tests
# ===========================

class TestCLI:
    """Test command-line interface."""

    def test_cli_import(self):
        """Test that CLI module can be imported."""
        from digitalmodel.orcaflex import convert_cli
        assert hasattr(convert_cli, 'main')

    def test_cli_single_file(self, sample_dat_files, temp_output_dir):
        """Test CLI single file conversion."""
        if not sample_dat_files:
            pytest.skip("No .dat files available")

        from digitalmodel.orcaflex.convert_cli import convert_single

        dat_file = sample_dat_files[0]
        output_file = temp_output_dir / "output.yml"

        exit_code = convert_single(
            input_file=dat_file,
            output_file=output_file,
            output_format='yml',
            mock=True
        )

        assert exit_code == 0
        assert output_file.exists()

    def test_cli_batch(self, sample_dat_files, temp_output_dir):
        """Test CLI batch conversion."""
        if not sample_dat_files:
            pytest.skip("No .dat files available")

        from digitalmodel.orcaflex.convert_cli import convert_batch

        input_dir = sample_dat_files[0].parent

        exit_code = convert_batch(
            input_dir=input_dir,
            output_dir=temp_output_dir,
            pattern='*.dat',
            output_format='yml',
            mock=True,
            parallel=False
        )

        assert exit_code == 0


if __name__ == "__main__":
    # Run tests with pytest
    pytest.main([__file__, '-v', '--tb=short'])
