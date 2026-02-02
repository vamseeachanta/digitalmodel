#!/usr/bin/env python3
"""
ABOUTME: CLI integration tests for GMSH meshing module testing command-line
interface functionality.
"""

import pytest
from click.testing import CliRunner

from digitalmodel.gmsh_meshing.cli import cli
from digitalmodel.gmsh_meshing.mesh_generator import GMSH_AVAILABLE


# ============================================================================
# Fixtures
# ============================================================================

@pytest.fixture
def runner():
    """Click CLI test runner"""
    return CliRunner()


# ============================================================================
# CLI Tests (require GMSH)
# ============================================================================

@pytest.mark.skipif(not GMSH_AVAILABLE, reason="GMSH not installed")
class TestCLI:
    """Test GMSH CLI commands"""

    def test_version(self, runner):
        """Test --version flag"""
        result = runner.invoke(cli, ['--version'])
        assert result.exit_code == 0
        assert 'gmsh-meshing' in result.output

    def test_help(self, runner):
        """Test --help flag"""
        result = runner.invoke(cli, ['--help'])
        assert result.exit_code == 0
        assert 'box' in result.output
        assert 'cylinder' in result.output

    def test_box_command(self, runner, tmp_path):
        """Test box mesh generation command"""
        output_file = tmp_path / "box_mesh.msh"

        result = runner.invoke(cli, [
            'box',
            '--length', '1.0',
            '--width', '1.0',
            '--height', '1.0',
            '--element-size', '0.5',
            '--output', str(output_file),
        ])

        assert result.exit_code == 0
        assert 'Box Mesh Generated' in result.output
        assert output_file.exists()

    def test_box_with_analysis(self, runner, tmp_path):
        """Test box mesh with quality analysis"""
        output_file = tmp_path / "box_analyzed.msh"

        result = runner.invoke(cli, [
            'box',
            '--length', '2.0',
            '--width', '1.0',
            '--height', '1.0',
            '--element-size', '0.3',
            '--output', str(output_file),
            '--analyze',
        ])

        assert result.exit_code == 0
        assert 'Mesh Quality' in result.output
        assert 'Quality Score' in result.output

    def test_cylinder_command(self, runner, tmp_path):
        """Test cylinder mesh generation command"""
        output_file = tmp_path / "cylinder_mesh.msh"

        result = runner.invoke(cli, [
            'cylinder',
            '--radius', '1.0',
            '--height', '5.0',
            '--element-size', '0.5',
            '--output', str(output_file),
        ])

        assert result.exit_code == 0
        assert 'Cylinder Mesh Generated' in result.output
        assert output_file.exists()
