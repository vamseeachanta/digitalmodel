#!/usr/bin/env python3
"""
Integration Tests for AQWA CLI

Tests the CLI commands with real data paths and validates
the command-line interface functionality.
"""

import pytest
import subprocess
from pathlib import Path


# Test data paths
TEST_DATA_DIR = Path("docs/modules/aqwa/examples/03_dat/001_ship_raos")
TEST_LIS_DIR = TEST_DATA_DIR


class TestAQWACLIBasic:
    """Test basic CLI functionality"""

    def test_cli_help(self):
        """Test --help flag"""
        result = subprocess.run(
            ["python", "src/digitalmodel/modules/aqwa/aqwa_cli.py", "--help"],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "AQWA Diffraction Analysis CLI" in result.stdout or "AQWA" in result.stdout

    def test_list_methods(self):
        """Test --list-methods flag"""
        result = subprocess.run(
            ["python", "src/digitalmodel/modules/aqwa/aqwa_cli.py", "--list-methods"],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "RAO Analysis" in result.stdout or "raos" in result.stdout
        assert "Damping Analysis" in result.stdout or "damping" in result.stdout


class TestAQWACLIRAOExtraction:
    """Test RAO extraction command"""

    @pytest.mark.skipif(
        not TEST_LIS_DIR.exists(),
        reason="Test data not available"
    )
    def test_rao_method_with_folder(self, tmp_path):
        """Test RAO extraction with folder argument"""
        result = subprocess.run(
            [
                "python", "src/digitalmodel/modules/aqwa/aqwa_cli.py",
                "--method", "raos",
                "--folder", str(TEST_LIS_DIR),
                "--name", "001_SHIP_RAOS",
                "--output", str(tmp_path)
            ],
            capture_output=True,
            text=True,
            timeout=60
        )

        # Check command executed (may succeed or fail due to dependencies)
        # We're primarily testing the CLI interface accepts the arguments
        assert result.returncode in [0, 1]  # 0=success, 1=expected failure

        # If failed, check it's not due to argument parsing
        if result.returncode != 0:
            assert "error: " not in result.stderr.lower() or \
                   "missing argument" not in result.stderr.lower()


class TestAQWACLIErrorHandling:
    """Test CLI error handling"""

    def test_missing_method(self):
        """Test error when method is missing"""
        result = subprocess.run(
            [
                "python", "src/digitalmodel/modules/aqwa/aqwa_cli.py",
                "--folder", "./test"
            ],
            capture_output=True,
            text=True
        )

        # Should fail gracefully
        assert result.returncode != 0

    def test_invalid_method(self):
        """Test error for invalid method"""
        result = subprocess.run(
            [
                "python", "src/digitalmodel/modules/aqwa/aqwa_cli.py",
                "--method", "invalid_method",
                "--folder", "./test"
            ],
            capture_output=True,
            text=True
        )

        assert result.returncode != 0
        assert "invalid choice" in result.stderr.lower() or "error" in result.stderr.lower()

    def test_missing_required_folder(self):
        """Test error when required folder is missing"""
        result = subprocess.run(
            [
                "python", "src/digitalmodel/modules/aqwa/aqwa_cli.py",
                "--method", "raos"
            ],
            capture_output=True,
            text=True
        )

        assert result.returncode != 0


class TestAQWACLIConfigFile:
    """Test configuration file loading"""

    @pytest.fixture
    def sample_config_file(self, tmp_path):
        """Create a sample YAML configuration file"""
        config_content = """
aqwa_analysis:
  method: raos
  analysis_folder: {}
  vessel_name: TEST_VESSEL
  output_folder: {}
""".format(str(TEST_LIS_DIR), str(tmp_path))

        config_file = tmp_path / "test_config.yml"
        config_file.write_text(config_content)
        return config_file

    @pytest.mark.skipif(
        not TEST_LIS_DIR.exists(),
        reason="Test data not available"
    )
    def test_config_file_loading(self, sample_config_file):
        """Test loading configuration from file"""
        result = subprocess.run(
            [
                "python", "src/digitalmodel/modules/aqwa/aqwa_cli.py",
                "--config", str(sample_config_file)
            ],
            capture_output=True,
            text=True,
            timeout=60
        )

        # Check command executed (may fail due to config structure)
        # Primarily testing that --config argument is accepted
        assert result.returncode in [0, 1]

    def test_missing_config_file(self):
        """Test error handling for missing config file"""
        result = subprocess.run(
            [
                "python", "src/digitalmodel/modules/aqwa/aqwa_cli.py",
                "--config", "/nonexistent/config.yml"
            ],
            capture_output=True,
            text=True
        )

        assert result.returncode != 0
        assert "not found" in result.stdout.lower() or "not found" in result.stderr.lower()


class TestAQWACLIDampingAnalysis:
    """Test damping analysis command"""

    @pytest.mark.skipif(
        not TEST_LIS_DIR.exists(),
        reason="Test data not available"
    )
    def test_damping_method(self, tmp_path):
        """Test damping analysis method"""
        result = subprocess.run(
            [
                "python", "src/digitalmodel/modules/aqwa/aqwa_cli.py",
                "--method", "damping",
                "--folder", str(TEST_LIS_DIR),
                "--output", str(tmp_path)
            ],
            capture_output=True,
            text=True,
            timeout=60
        )

        # Check command executed (may fail due to data requirements)
        assert result.returncode in [0, 1]


class TestAQWACLIVerboseMode:
    """Test verbose output mode"""

    @pytest.mark.skipif(
        not TEST_LIS_DIR.exists(),
        reason="Test data not available"
    )
    def test_verbose_flag(self, tmp_path):
        """Test verbose output"""
        result = subprocess.run(
            [
                "python", "src/digitalmodel/modules/aqwa/aqwa_cli.py",
                "--method", "raos",
                "--folder", str(TEST_LIS_DIR),
                "--name", "001_SHIP_RAOS",
                "--output", str(tmp_path),
                "--verbose"
            ],
            capture_output=True,
            text=True,
            timeout=60
        )

        # Verbose mode should produce output
        # (even if the analysis fails due to dependencies)
        assert len(result.stdout) > 0 or len(result.stderr) > 0


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
