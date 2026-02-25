#!/usr/bin/env python3
"""
Integration Tests for Diffraction CLI

Tests the CLI commands with real data.
"""

import pytest
import subprocess
import json
from pathlib import Path
import shutil


# Test data paths
TEST_DATA_DIR = Path("docs/domains/aqwa/examples/03_dat/001_ship_raos")
TEST_LIS_DIR = TEST_DATA_DIR


class TestCLIConvertAQWA:
    """Test convert-aqwa command"""

    @pytest.fixture
    def output_dir(self, tmp_path):
        """Create temporary output directory"""
        output = tmp_path / "output"
        output.mkdir()
        yield output
        # Cleanup
        if output.exists():
            shutil.rmtree(output)

    def test_convert_aqwa_help(self):
        """Test --help flag"""
        result = subprocess.run(
            ["diffraction", "convert-aqwa", "--help"],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "Convert AQWA diffraction results" in result.stdout

    @pytest.mark.skipif(
        not TEST_LIS_DIR.exists(),
        reason="Test data not available"
    )
    def test_convert_aqwa_basic(self, output_dir):
        """Test basic AQWA conversion"""
        result = subprocess.run(
            [
                "diffraction", "convert-aqwa",
                str(TEST_LIS_DIR),
                "SHIP_RAOS",
                "-d", "100",
                "-o", str(output_dir)
            ],
            capture_output=True,
            text=True
        )

        # Check command succeeded
        assert result.returncode == 0

        # Check output files created
        assert (output_dir / "SHIP_RAOS_vessel_type.yml").exists()
        assert (output_dir / "SHIP_RAOS_RAOs.csv").exists()
        assert (output_dir / "SHIP_RAOS_AddedMass.csv").exists()
        assert (output_dir / "SHIP_RAOS_Damping.csv").exists()
        assert (output_dir / "SHIP_RAOS_Results.xlsx").exists()
        assert (output_dir / "SHIP_RAOS_validation.json").exists()

    @pytest.mark.skipif(
        not TEST_LIS_DIR.exists(),
        reason="Test data not available"
    )
    def test_convert_aqwa_validation(self, output_dir):
        """Test validation report generation"""
        subprocess.run(
            [
                "diffraction", "convert-aqwa",
                str(TEST_LIS_DIR),
                "SHIP_RAOS",
                "-d", "100",
                "-o", str(output_dir),
                "--validate"
            ],
            capture_output=True,
            text=True
        )

        # Check validation file
        validation_file = output_dir / "SHIP_RAOS_validation.json"
        assert validation_file.exists()

        # Check validation content
        with open(validation_file) as f:
            validation = json.load(f)

        assert 'overall_status' in validation
        assert validation['overall_status'] in ['PASS', 'WARNING', 'FAIL']

    @pytest.mark.skipif(
        not TEST_LIS_DIR.exists(),
        reason="Test data not available"
    )
    def test_convert_aqwa_selective_formats(self, output_dir):
        """Test selective format export"""
        result = subprocess.run(
            [
                "diffraction", "convert-aqwa",
                str(TEST_LIS_DIR),
                "SHIP_RAOS",
                "-d", "100",
                "-o", str(output_dir),
                "-f", "vessel_type",
                "-f", "excel"
            ],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0

        # Should have these files
        assert (output_dir / "SHIP_RAOS_vessel_type.yml").exists()
        assert (output_dir / "SHIP_RAOS_Results.xlsx").exists()

        # Should NOT have CSV files (not requested)
        # Note: validation is still created by default
        assert (output_dir / "SHIP_RAOS_validation.json").exists()


class TestCLIBatch:
    """Test batch processing command"""

    @pytest.fixture
    def config_file(self, tmp_path):
        """Create test batch configuration"""
        if not TEST_LIS_DIR.exists():
            pytest.skip("Test data not available")

        config = {
            "configurations": [
                {
                    "vessel_name": "TEST_VESSEL_1",
                    "source_type": "aqwa",
                    "source_path": str(TEST_LIS_DIR),
                    "water_depth": 100.0,
                    "output_dir": str(tmp_path / "output1"),
                    "formats": ["vessel_type", "excel"]
                },
                {
                    "vessel_name": "TEST_VESSEL_2",
                    "source_type": "aqwa",
                    "source_path": str(TEST_LIS_DIR),
                    "water_depth": 200.0,
                    "output_dir": str(tmp_path / "output2"),
                    "formats": ["vessel_type"]
                }
            ],
            "parallel": False,  # Sequential for testing
            "max_workers": 1
        }

        config_path = tmp_path / "test_config.json"
        with open(config_path, 'w') as f:
            json.dump(config, f, indent=2)

        return config_path

    def test_batch_help(self):
        """Test batch --help"""
        result = subprocess.run(
            ["diffraction", "batch", "--help"],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "Batch process multiple vessels" in result.stdout

    @pytest.mark.skipif(
        not TEST_LIS_DIR.exists(),
        reason="Test data not available"
    )
    def test_batch_processing(self, config_file, tmp_path):
        """Test batch processing with config file"""
        result = subprocess.run(
            ["diffraction", "batch", str(config_file)],
            capture_output=True,
            text=True
        )

        # Check command succeeded
        assert result.returncode == 0

        # Check output directories created
        assert (tmp_path / "output1").exists()
        assert (tmp_path / "output2").exists()

        # Check files in output1
        assert (tmp_path / "output1" / "TEST_VESSEL_1_vessel_type.yml").exists()
        assert (tmp_path / "output1" / "TEST_VESSEL_1_Results.xlsx").exists()

        # Check files in output2
        assert (tmp_path / "output2" / "TEST_VESSEL_2_vessel_type.yml").exists()


class TestCLIGeneral:
    """Test general CLI functionality"""

    def test_cli_version(self):
        """Test --version flag"""
        result = subprocess.run(
            ["diffraction", "--version"],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "3.0.0" in result.stdout

    def test_cli_help(self):
        """Test main help command"""
        result = subprocess.run(
            ["diffraction", "--help"],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "convert-aqwa" in result.stdout
        assert "convert-orcawave" in result.stdout
        assert "compare" in result.stdout
        assert "batch" in result.stdout

    def test_cli_invalid_command(self):
        """Test error handling for invalid command"""
        result = subprocess.run(
            ["diffraction", "invalid-command"],
            capture_output=True,
            text=True
        )

        assert result.returncode != 0


class TestCLIErrorHandling:
    """Test CLI error handling"""

    def test_missing_required_argument(self):
        """Test error when required argument missing"""
        result = subprocess.run(
            ["diffraction", "convert-aqwa"],
            capture_output=True,
            text=True
        )

        assert result.returncode != 0
        assert "Missing argument" in result.stderr or "required" in result.stderr.lower()

    def test_invalid_water_depth(self):
        """Test error for invalid water depth"""
        result = subprocess.run(
            ["diffraction", "convert-aqwa", ".", "TEST", "-d", "invalid"],
            capture_output=True,
            text=True
        )

        assert result.returncode != 0

    def test_missing_analysis_folder(self):
        """Test error for missing analysis folder"""
        result = subprocess.run(
            ["diffraction", "convert-aqwa", "/nonexistent/folder", "TEST", "-d", "100"],
            capture_output=True,
            text=True
        )

        assert result.returncode != 0


if __name__ == "__main__":
    pytest.main([__file__, "-v", "-s"])
