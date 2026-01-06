#!/usr/bin/env python3
"""
ABOUTME: CLI integration tests for Catenary Riser Module testing command-line
interface functionality and output generation.
"""

import pytest
import json
import tempfile
from pathlib import Path
from click.testing import CliRunner

from digitalmodel.modules.catenary_riser.cli import cli


# ============================================================================
# Fixtures
# ============================================================================

@pytest.fixture
def runner():
    """Click CLI test runner"""
    return CliRunner()


@pytest.fixture
def temp_output_file():
    """Temporary output file for JSON results"""
    with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
        yield f.name
    Path(f.name).unlink(missing_ok=True)


# ============================================================================
# Simple Catenary Command Tests
# ============================================================================

class TestSimpleCommand:
    """Test simple catenary CLI command"""

    def test_simple_basic(self, runner):
        """Test basic simple catenary analysis"""
        result = runner.invoke(cli, [
            'simple',
            '--diameter', '0.508',
            '--thickness', '0.025',
            '--length', '1500',
            '--water-depth', '1000',
            '--offset', '500',
            '--material', 'x65',
            '--internal-fluid', 'oil',
        ])

        assert result.exit_code == 0
        assert 'Simple Catenary Riser Analysis' in result.output
        assert 'Effective Weight' in result.output
        assert 'Top Tension' in result.output

    def test_simple_with_output_file(self, runner, temp_output_file):
        """Test simple catenary with JSON output"""
        result = runner.invoke(cli, [
            'simple',
            '--diameter', '0.508',
            '--thickness', '0.025',
            '--length', '1500',
            '--water-depth', '1000',
            '--offset', '500',
            '--output', temp_output_file,
        ])

        assert result.exit_code == 0
        assert f'Results saved to: {temp_output_file}' in result.output

        # Verify JSON file
        with open(temp_output_file, 'r') as f:
            data = json.load(f)

        assert 'configuration' in data
        assert 'results' in data
        assert data['results']['top_tension_N'] > 0

    def test_simple_with_coating(self, runner):
        """Test with coating parameters"""
        result = runner.invoke(cli, [
            'simple',
            '--diameter', '0.508',
            '--thickness', '0.025',
            '--length', '1500',
            '--water-depth', '1000',
            '--offset', '500',
            '--coating-thickness', '0.05',
            '--coating-density', '700',
        ])

        assert result.exit_code == 0

    def test_simple_missing_required(self, runner):
        """Test error with missing required arguments"""
        result = runner.invoke(cli, [
            'simple',
            '--diameter', '0.508',
            # Missing other required arguments
        ])

        assert result.exit_code != 0


# ============================================================================
# Effective Weight Command Tests
# ============================================================================

class TestWeightCommand:
    """Test effective weight CLI command"""

    def test_weight_basic(self, runner):
        """Test basic weight calculation"""
        result = runner.invoke(cli, [
            'weight',
            '--diameter', '0.508',
            '--thickness', '0.025',
            '--material', 'x65',
            '--internal-fluid', 'oil',
        ])

        assert result.exit_code == 0
        assert 'Effective Weight Breakdown' in result.output
        assert 'Steel Weight' in result.output
        assert 'Buoyancy' in result.output

    def test_weight_with_coating(self, runner):
        """Test weight with coating"""
        result = runner.invoke(cli, [
            'weight',
            '--diameter', '0.508',
            '--thickness', '0.025',
            '--coating-thickness', '0.05',
            '--coating-density', '700',
        ])

        assert result.exit_code == 0
        assert 'Coating Weight' in result.output

    def test_weight_output_file(self, runner, temp_output_file):
        """Test weight with JSON output"""
        result = runner.invoke(cli, [
            'weight',
            '--diameter', '0.508',
            '--thickness', '0.025',
            '--output', temp_output_file,
        ])

        assert result.exit_code == 0

        with open(temp_output_file, 'r') as f:
            data = json.load(f)

        assert 'effective_weight_N_per_m' in data


# ============================================================================
# Lazy Wave Command Tests
# ============================================================================

class TestLazyWaveCommand:
    """Test lazy wave CLI command"""

    def test_lazy_wave_basic(self, runner):
        """Test basic lazy wave analysis"""
        result = runner.invoke(cli, [
            'lazy-wave',
            '--diameter', '0.508',
            '--thickness', '0.025',
            '--length', '1500',
            '--water-depth', '1000',
            '--offset', '500',
            '--buoy-length', '200',
            '--buoy-diameter', '1.5',
            '--buoy-start', '300',
        ])

        assert result.exit_code == 0
        assert 'Lazy Wave Riser Analysis' in result.output
        assert 'Sag Bend Depth' in result.output
        assert 'Hog Bend Depth' in result.output

    def test_lazy_wave_output_file(self, runner, temp_output_file):
        """Test lazy wave with JSON output"""
        result = runner.invoke(cli, [
            'lazy-wave',
            '--diameter', '0.508',
            '--thickness', '0.025',
            '--length', '1500',
            '--water-depth', '1000',
            '--offset', '500',
            '--output', temp_output_file,
        ])

        assert result.exit_code == 0

        with open(temp_output_file, 'r') as f:
            data = json.load(f)

        assert 'sag_bend_depth_m' in data
        assert 'arch_height_m' in data


# ============================================================================
# CLI Utilities Tests
# ============================================================================

class TestCLIUtilities:
    """Test CLI utility commands"""

    def test_version(self, runner):
        """Test --version flag"""
        result = runner.invoke(cli, ['--version'])

        assert result.exit_code == 0
        assert 'catenary-riser' in result.output

    def test_help(self, runner):
        """Test --help flag"""
        result = runner.invoke(cli, ['--help'])

        assert result.exit_code == 0
        assert 'Catenary Riser Analysis' in result.output
        assert 'simple' in result.output
        assert 'weight' in result.output
        assert 'lazy-wave' in result.output

    def test_simple_help(self, runner):
        """Test simple --help"""
        result = runner.invoke(cli, ['simple', '--help'])

        assert result.exit_code == 0
        assert 'diameter' in result.output
        assert 'thickness' in result.output
