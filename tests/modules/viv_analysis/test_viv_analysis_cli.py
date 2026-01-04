#!/usr/bin/env python3
"""
ABOUTME: CLI integration tests for VIV Analysis Module testing command-line
interface functionality, argument parsing, and output generation.
"""

import pytest
import json
import tempfile
from pathlib import Path
from click.testing import CliRunner

from digitalmodel.modules.viv_analysis.cli import cli


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
    # Cleanup
    Path(f.name).unlink(missing_ok=True)


# ============================================================================
# Natural Frequency Command Tests
# ============================================================================

class TestNaturalFrequencyCommand:
    """Test natural-freq CLI command"""

    def test_natural_freq_basic(self, runner):
        """Test basic natural frequency calculation"""
        result = runner.invoke(cli, [
            'natural-freq',
            '--length', '50',
            '--diameter', '0.5',
            '--thickness', '0.025',
            '--material', 'steel',
            '--boundary', 'pinned-pinned',
            '--n-modes', '3'
        ])

        assert result.exit_code == 0
        assert 'Natural Frequencies' in result.output
        assert 'Mode 1:' in result.output
        assert 'Mode 2:' in result.output
        assert 'Mode 3:' in result.output
        assert 'Hz' in result.output

    def test_natural_freq_with_output_file(self, runner, temp_output_file):
        """Test natural frequency with JSON output file"""
        result = runner.invoke(cli, [
            'natural-freq',
            '--length', '30',
            '--diameter', '0.4',
            '--thickness', '0.02',
            '--material', 'steel',
            '--n-modes', '2',
            '--output', temp_output_file
        ])

        assert result.exit_code == 0
        assert f'Results saved to: {temp_output_file}' in result.output

        # Verify JSON file
        with open(temp_output_file, 'r') as f:
            data = json.load(f)

        assert 'member' in data
        assert 'frequencies' in data
        assert data['member']['length'] == 30
        assert data['member']['diameter'] == 0.4
        assert len(data['frequencies']) == 2

        # Check frequency data
        for freq in data['frequencies']:
            assert 'mode' in freq
            assert 'frequency_hz' in freq
            assert 'period_s' in freq
            assert freq['frequency_hz'] > 0

    def test_natural_freq_cantilever(self, runner):
        """Test cantilever boundary condition"""
        result = runner.invoke(cli, [
            'natural-freq',
            '--length', '20',
            '--diameter', '0.3',
            '--thickness', '0.015',
            '--boundary', 'cantilever',
            '--n-modes', '2'
        ])

        assert result.exit_code == 0
        assert 'Mode 1:' in result.output
        assert 'Mode 2:' in result.output

    def test_natural_freq_titanium_material(self, runner):
        """Test with titanium material"""
        result = runner.invoke(cli, [
            'natural-freq',
            '--length', '40',
            '--diameter', '0.45',
            '--thickness', '0.022',
            '--material', 'titanium',
            '--n-modes', '1'
        ])

        assert result.exit_code == 0
        assert 'Mode 1:' in result.output

    def test_natural_freq_multiple_modes(self, runner):
        """Test with many modes"""
        result = runner.invoke(cli, [
            'natural-freq',
            '--length', '50',
            '--diameter', '0.5',
            '--thickness', '0.025',
            '--n-modes', '6'
        ])

        assert result.exit_code == 0
        for i in range(1, 7):
            assert f'Mode {i}:' in result.output

    def test_natural_freq_missing_required_args(self, runner):
        """Test error when missing required arguments"""
        result = runner.invoke(cli, [
            'natural-freq',
            '--length', '50',
            # Missing diameter and thickness
        ])

        assert result.exit_code != 0

    def test_natural_freq_invalid_material(self, runner):
        """Test error with invalid material"""
        result = runner.invoke(cli, [
            'natural-freq',
            '--length', '50',
            '--diameter', '0.5',
            '--thickness', '0.025',
            '--material', 'unobtanium'
        ])

        assert result.exit_code != 0
        assert 'Error' in result.output


# ============================================================================
# Screening Command Tests
# ============================================================================

class TestScreeningCommand:
    """Test screening CLI command"""

    def test_screening_basic(self, runner):
        """Test basic VIV screening"""
        result = runner.invoke(cli, [
            'screening',
            '--length', '50',
            '--diameter', '0.5',
            '--thickness', '0.025',
            '--current', '1.5',
            '--material', 'steel',
            '--boundary', 'pinned-pinned'
        ])

        assert result.exit_code == 0
        assert 'VIV Screening Results' in result.output
        assert 'Natural Frequency:' in result.output
        assert 'Shedding Frequency:' in result.output
        assert 'Reduced Velocity:' in result.output
        assert 'Lock-in Status:' in result.output
        assert 'Safety Factor:' in result.output
        assert 'Recommendation:' in result.output

    def test_screening_with_output_file(self, runner, temp_output_file):
        """Test screening with JSON output"""
        result = runner.invoke(cli, [
            'screening',
            '--length', '40',
            '--diameter', '0.45',
            '--thickness', '0.022',
            '--current', '2.0',
            '--output', temp_output_file
        ])

        assert result.exit_code == 0
        assert f'Results saved to: {temp_output_file}' in result.output

        # Verify JSON
        with open(temp_output_file, 'r') as f:
            data = json.load(f)

        assert 'natural_frequency_hz' in data
        assert 'shedding_frequency_hz' in data
        assert 'reduced_velocity' in data
        assert 'is_susceptible' in data
        assert 'lock_in_status' in data
        assert 'safety_factor' in data
        assert 'recommendation' in data

        assert isinstance(data['is_susceptible'], bool)
        assert data['safety_factor'] > 0

    def test_screening_low_current(self, runner):
        """Test screening with low current velocity"""
        result = runner.invoke(cli, [
            'screening',
            '--length', '50',
            '--diameter', '0.5',
            '--thickness', '0.025',
            '--current', '0.5',
        ])

        assert result.exit_code == 0
        assert 'Safety Factor:' in result.output

    def test_screening_high_current(self, runner):
        """Test screening with high current velocity"""
        result = runner.invoke(cli, [
            'screening',
            '--length', '50',
            '--diameter', '0.5',
            '--thickness', '0.025',
            '--current', '3.0',
        ])

        assert result.exit_code == 0
        assert 'Reduced Velocity:' in result.output

    def test_screening_cantilever_boundary(self, runner):
        """Test screening with cantilever boundary"""
        result = runner.invoke(cli, [
            'screening',
            '--length', '30',
            '--diameter', '0.4',
            '--thickness', '0.02',
            '--current', '1.5',
            '--boundary', 'cantilever'
        ])

        assert result.exit_code == 0
        assert 'Lock-in Status:' in result.output

    def test_screening_stainless_steel(self, runner):
        """Test screening with stainless steel"""
        result = runner.invoke(cli, [
            'screening',
            '--length', '45',
            '--diameter', '0.48',
            '--thickness', '0.024',
            '--current', '1.8',
            '--material', 'steel_stainless'
        ])

        assert result.exit_code == 0

    def test_screening_missing_current(self, runner):
        """Test error when current velocity is missing"""
        result = runner.invoke(cli, [
            'screening',
            '--length', '50',
            '--diameter', '0.5',
            '--thickness', '0.025',
            # Missing --current
        ])

        assert result.exit_code != 0


# ============================================================================
# Version and Help Tests
# ============================================================================

class TestCLIUtilities:
    """Test CLI utility commands"""

    def test_version(self, runner):
        """Test --version flag"""
        result = runner.invoke(cli, ['--version'])

        assert result.exit_code == 0
        assert 'viv-analysis' in result.output
        # Should show version number
        assert any(char.isdigit() for char in result.output)

    def test_help(self, runner):
        """Test --help flag"""
        result = runner.invoke(cli, ['--help'])

        assert result.exit_code == 0
        assert 'VIV Analysis Tools' in result.output
        assert 'natural-freq' in result.output
        assert 'screening' in result.output

    def test_natural_freq_help(self, runner):
        """Test natural-freq --help"""
        result = runner.invoke(cli, ['natural-freq', '--help'])

        assert result.exit_code == 0
        assert 'length' in result.output
        assert 'diameter' in result.output
        assert 'thickness' in result.output
        assert 'material' in result.output

    def test_screening_help(self, runner):
        """Test screening --help"""
        result = runner.invoke(cli, ['screening', '--help'])

        assert result.exit_code == 0
        assert 'length' in result.output
        assert 'current' in result.output
        assert 'output' in result.output


# ============================================================================
# Edge Case Tests
# ============================================================================

class TestEdgeCases:
    """Test edge cases and error handling"""

    def test_very_thin_wall(self, runner):
        """Test with very thin wall"""
        result = runner.invoke(cli, [
            'natural-freq',
            '--length', '50',
            '--diameter', '0.5',
            '--thickness', '0.005',  # Very thin
            '--n-modes', '1'
        ])

        # Should complete but may warn
        assert result.exit_code == 0 or 'Error' in result.output

    def test_very_long_member(self, runner):
        """Test with very long member"""
        result = runner.invoke(cli, [
            'natural-freq',
            '--length', '200',  # Very long
            '--diameter', '0.5',
            '--thickness', '0.025',
            '--n-modes', '1'
        ])

        assert result.exit_code == 0

    def test_zero_current(self, runner):
        """Test screening with zero current"""
        result = runner.invoke(cli, [
            'screening',
            '--length', '50',
            '--diameter', '0.5',
            '--thickness', '0.025',
            '--current', '0.0',
        ])

        # Should handle gracefully
        assert result.exit_code == 0 or 'Error' in result.output
