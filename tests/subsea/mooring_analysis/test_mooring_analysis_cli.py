#!/usr/bin/env python3
"""
CLI Integration Tests for Mooring Analysis

Tests the command-line interface for all mooring analysis commands.
"""

import pytest
import subprocess
import json
from pathlib import Path


class TestCLICatenary:
    """Test catenary command"""

    def test_catenary_help(self):
        """Test catenary --help"""
        result = subprocess.run(
            ["mooring-analysis", "catenary", "--help"],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "catenary" in result.stdout.lower() or "geometry" in result.stdout.lower()

    def test_catenary_horizontal_tension(self, tmp_path):
        """Test catenary with horizontal tension"""
        output = tmp_path / "catenary_result.json"

        result = subprocess.run(
            [
                "mooring-analysis", "catenary",
                "--water-depth", "100",
                "--line-length", "500",
                "--line-weight", "145",
                "--horizontal-tension", "1000",
                "--output", str(output)
            ],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "Catenary" in result.stdout or "Horizontal" in result.stdout
        assert output.exists()

        # Validate JSON
        with open(output) as f:
            data = json.load(f)
            assert 'catenary' in data
            assert 'stiffness' in data
            assert data['catenary']['horizontal_tension'] == 1000.0

    def test_catenary_fairlead_tension(self, tmp_path):
        """Test catenary with target fairlead tension"""
        output = tmp_path / "catenary_result.json"

        result = subprocess.run(
            [
                "mooring-analysis", "catenary",
                "--water-depth", "100",
                "--line-length", "500",
                "--line-weight", "145",
                "--fairlead-tension", "2000",
                "--output", str(output)
            ],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert output.exists()

        with open(output) as f:
            data = json.load(f)
            assert data['solve_method'] == 'fairlead_tension'
            assert abs(data['catenary']['fairlead_tension'] - 2000.0) < 10.0

    def test_catenary_missing_tension(self):
        """Test error when no tension specified"""
        result = subprocess.run(
            [
                "mooring-analysis", "catenary",
                "--water-depth", "100",
                "--line-length", "500",
                "--line-weight", "145"
            ],
            capture_output=True,
            text=True
        )

        # Should fail without tension parameter
        assert result.returncode != 0


class TestCLIDesign:
    """Test design verification command"""

    def test_design_help(self):
        """Test design --help"""
        result = subprocess.run(
            ["mooring-analysis", "design", "--help"],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "design" in result.stdout.lower() or "verification" in result.stdout.lower()

    def test_design_calm_system(self, tmp_path):
        """Test CALM mooring system design"""
        output = tmp_path / "design_result.json"

        result = subprocess.run(
            [
                "mooring-analysis", "design",
                "--system-type", "calm",
                "--water-depth", "100",
                "--n-lines", "6",
                "--anchor-radius", "400",
                "--material", "chain_r3_84",
                "--output", str(output)
            ],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "Design" in result.stdout or "Mooring" in result.stdout
        assert output.exists()

        # Validate JSON
        with open(output) as f:
            data = json.load(f)
            assert 'system' in data
            assert data['system']['type'] == 'calm'
            assert data['system']['n_lines'] == 6
            assert 'summary' in data
            assert 'results' in data

    def test_design_spread_system(self, tmp_path):
        """Test spread mooring system design"""
        output = tmp_path / "spread_result.json"

        result = subprocess.run(
            [
                "mooring-analysis", "design",
                "--system-type", "spread",
                "--water-depth", "80",
                "--n-lines", "8",
                "--wave-hs", "7.0",
                "--current-speed", "1.2",
                "--output", str(output)
            ],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert output.exists()

        with open(output) as f:
            data = json.load(f)
            assert data['system']['type'] == 'spread'
            assert len(data['results']) > 0

    def test_design_with_damaged_check(self, tmp_path):
        """Test design with damaged line check"""
        output = tmp_path / "damaged_result.json"

        result = subprocess.run(
            [
                "mooring-analysis", "design",
                "--system-type", "calm",
                "--water-depth", "100",
                "--n-lines", "6",
                "--check-damaged",
                "--output", str(output)
            ],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert output.exists()

        with open(output) as f:
            data = json.load(f)
            # Should have intact + damaged results
            load_cases = set(r['load_case'] for r in data['results'])
            assert len(load_cases) > 1  # Both intact and damaged

    def test_design_different_materials(self):
        """Test design with different materials"""
        for material in ['chain_r3_84', 'chain_r4_84', 'polyester_140']:
            result = subprocess.run(
                [
                    "mooring-analysis", "design",
                    "--system-type", "calm",
                    "--water-depth", "100",
                    "--material", material,
                    "--n-lines", "4"
                ],
                capture_output=True,
                text=True
            )

            assert result.returncode == 0


class TestCLIListMaterials:
    """Test list-materials command"""

    def test_list_materials(self):
        """Test list-materials command"""
        result = subprocess.run(
            ["mooring-analysis", "list-materials"],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "chain" in result.stdout.lower()
        assert "polyester" in result.stdout.lower()
        assert "MBL" in result.stdout or "kN" in result.stdout

    def test_list_materials_shows_all(self):
        """Test that all materials are listed"""
        result = subprocess.run(
            ["mooring-analysis", "list-materials"],
            capture_output=True,
            text=True
        )

        # Check for specific materials
        assert "chain_r3_84" in result.stdout
        assert "polyester_140" in result.stdout
        assert "wire_76" in result.stdout


class TestCLIGeneral:
    """Test general CLI functionality"""

    def test_cli_version(self):
        """Test --version flag"""
        result = subprocess.run(
            ["mooring-analysis", "--version"],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "1.0.0" in result.stdout

    def test_cli_help(self):
        """Test main help command"""
        result = subprocess.run(
            ["mooring-analysis", "--help"],
            capture_output=True,
            text=True
        )

        assert result.returncode == 0
        assert "catenary" in result.stdout
        assert "design" in result.stdout
        assert "list-materials" in result.stdout

    def test_cli_invalid_command(self):
        """Test error handling for invalid command"""
        result = subprocess.run(
            ["mooring-analysis", "invalid-command"],
            capture_output=True,
            text=True
        )

        assert result.returncode != 0


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
