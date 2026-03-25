#!/usr/bin/env python3
"""
CLI Integration Tests for Structural Analysis

Tests the command-line interface for all structural analysis commands.
"""

import pytest
import subprocess
import json
from pathlib import Path


class TestCLIStress:
    """Test stress command"""

    def test_stress_help(self):
        """Test stress --help"""
        result = subprocess.run(
            ["structural-analysis", "stress", "--help"],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "Von Mises" in result.stdout or "stress" in result.stdout.lower()

    def test_stress_uniaxial(self, tmp_path):
        """Test uniaxial stress calculation"""
        output = tmp_path / "stress_result.json"

        result = subprocess.run(
            [
                "structural-analysis", "stress",
                "--sigma-x", "100",
                "--material", "S355",
                "--output", str(output)
            ],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "Von Mises" in result.stdout
        assert "PASS" in result.stdout or "FAIL" in result.stdout

        # Check output file created
        assert output.exists()

        # Validate JSON
        with open(output) as f:
            data = json.load(f)
            assert 'von_mises_stress' in data
            assert 'safety_factor' in data

    def test_stress_combined(self):
        """Test combined stress state"""
        result = subprocess.run(
            [
                "structural-analysis", "stress",
                "--sigma-x", "150",
                "--sigma-y", "50",
                "--tau-xy", "30",
                "--material", "S420"
            ],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "Von Mises" in result.stdout


class TestCLIBucklingPlate:
    """Test plate buckling command"""

    def test_buckling_plate_help(self):
        """Test buckling-plate --help"""
        result = subprocess.run(
            ["structural-analysis", "buckling-plate", "--help"],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "buckling" in result.stdout.lower() or "plate" in result.stdout.lower()

    def test_buckling_plate_basic(self, tmp_path):
        """Test basic plate buckling check"""
        output = tmp_path / "buckling_result.json"

        result = subprocess.run(
            [
                "structural-analysis", "buckling-plate",
                "--length", "2000",
                "--width", "1000",
                "--thickness", "20",
                "--sigma-x", "100",
                "--material", "S355",
                "--output", str(output)
            ],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "Buckling" in result.stdout or "Critical" in result.stdout

        # Check output file
        assert output.exists()

        with open(output) as f:
            data = json.load(f)
            assert 'utilization' in data
            assert 'status' in data

    def test_buckling_plate_with_shear(self):
        """Test plate buckling with shear stress"""
        result = subprocess.run(
            [
                "structural-analysis", "buckling-plate",
                "--length", "3000",
                "--width", "1500",
                "--thickness", "16",
                "--sigma-x", "200",
                "--tau", "40",
                "--material", "S275",
                "--gamma-m", "1.15"
            ],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0

    def test_buckling_plate_missing_args(self):
        """Test error handling for missing arguments"""
        result = subprocess.run(
            ["structural-analysis", "buckling-plate"],
            capture_output=True,
            text=True
        )

        # Should fail with missing arguments
        assert result.returncode != 0


class TestCLIBucklingColumn:
    """Test column buckling command"""

    def test_buckling_column_help(self):
        """Test buckling-column --help"""
        result = subprocess.run(
            ["structural-analysis", "buckling-column", "--help"],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "column" in result.stdout.lower() or "buckling" in result.stdout.lower()

    def test_buckling_column_basic(self, tmp_path):
        """Test column buckling check"""
        output = tmp_path / "column_result.json"

        result = subprocess.run(
            [
                "structural-analysis", "buckling-column",
                "--axial-force", "2500000",  # 2.5 MN
                "--area", "15000",
                "--I-min", "50000000",
                "--L-eff", "8000",
                "--curve", "b",
                "--material", "S355",
                "--output", str(output)
            ],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "Column" in result.stdout or "Buckling" in result.stdout

        # Check output
        assert output.exists()

        with open(output) as f:
            data = json.load(f)
            assert 'utilization' in data
            assert 'buckling_curve' in data['column_properties']

    def test_buckling_column_curves(self):
        """Test different buckling curves"""
        for curve in ['a', 'b', 'c']:
            result = subprocess.run(
                [
                    "structural-analysis", "buckling-column",
                    "--axial-force", "3000000",
                    "--area", "20000",
                    "--I-min", "80000000",
                    "--L-eff", "6000",
                    "--curve", curve,
                    "--material", "S420"
                ],
                capture_output=True,
                text=True
            )

            assert result.returncode == 0


class TestCLICapacity:
    """Test capacity command"""

    def test_capacity_help(self):
        """Test capacity --help"""
        result = subprocess.run(
            ["structural-analysis", "capacity", "--help"],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0

    def test_capacity_tension(self, tmp_path):
        """Test tension capacity check"""
        output = tmp_path / "capacity_result.json"

        result = subprocess.run(
            [
                "structural-analysis", "capacity",
                "--tension",
                "--axial-force", "4000000",  # 4 MN
                "--area-gross", "15000",
                "--area-net", "14000",
                "--material", "S355",
                "--output", str(output)
            ],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "Tension" in result.stdout or "Capacity" in result.stdout

        # Check output
        assert output.exists()

        with open(output) as f:
            data = json.load(f)
            assert 'check_type' in data
            assert data['check_type'] == 'tension'


class TestCLIListMaterials:
    """Test list-materials command"""

    def test_list_materials(self):
        """Test list-materials command"""
        result = subprocess.run(
            ["structural-analysis", "list-materials"],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "S355" in result.stdout
        assert "S275" in result.stdout
        assert "S420" in result.stdout
        assert "Yield Strength" in result.stdout


class TestCLIGeneral:
    """Test general CLI functionality"""

    def test_cli_version(self):
        """Test --version flag"""
        result = subprocess.run(
            ["structural-analysis", "--version"],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "1.0.0" in result.stdout

    def test_cli_help(self):
        """Test main help command"""
        result = subprocess.run(
            ["structural-analysis", "--help"],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "stress" in result.stdout
        assert "buckling" in result.stdout
        assert "capacity" in result.stdout

    def test_cli_invalid_command(self):
        """Test error handling for invalid command"""
        result = subprocess.run(
            ["structural-analysis", "invalid-command"],
            capture_output=True,
            text=True
        )

        assert result.returncode != 0


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
