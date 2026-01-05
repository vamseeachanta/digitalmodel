#!/usr/bin/env python3
"""
ABOUTME: CLI integration tests for hydrodynamics module testing command-line
interface functionality and output generation.
"""

import pytest
import json
import tempfile
from pathlib import Path
from click.testing import CliRunner

from digitalmodel.modules.hydrodynamics.cli import cli


# ============================================================================
# Fixtures
# ============================================================================

@pytest.fixture
def runner():
    """Click CLI test runner"""
    return CliRunner()


@pytest.fixture
def temp_output_file():
    """Temporary output file for JSON/CSV results"""
    with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
        yield f.name
    Path(f.name).unlink(missing_ok=True)


# ============================================================================
# Spectrum Command Tests
# ============================================================================

class TestSpectrumCommand:
    """Test wave spectrum CLI command"""

    def test_jonswap_spectrum(self, runner):
        """Test JONSWAP spectrum generation"""
        result = runner.invoke(cli, [
            'spectrum',
            '--type', 'jonswap',
            '--hs', '3.5',
            '--tp', '10.0',
            '--gamma', '3.3',
        ])

        assert result.exit_code == 0
        assert 'Wave Spectrum Analysis' in result.output
        assert 'JONSWAP' in result.output
        assert 'Hs:' in result.output
        assert 'Tp:' in result.output

    def test_pm_spectrum(self, runner):
        """Test Pierson-Moskowitz spectrum"""
        result = runner.invoke(cli, [
            'spectrum',
            '--type', 'pm',
            '--hs', '3.0',
            '--tp', '8.0',
        ])

        assert result.exit_code == 0
        assert 'PM' in result.output

    def test_spectrum_with_output(self, runner, temp_output_file):
        """Test spectrum with CSV output"""
        csv_file = temp_output_file.replace('.json', '.csv')

        result = runner.invoke(cli, [
            'spectrum',
            '--type', 'jonswap',
            '--hs', '3.5',
            '--tp', '10.0',
            '--output', csv_file,
        ])

        assert result.exit_code == 0
        assert f'saved to: {csv_file}' in result.output

        # Verify CSV file exists
        assert Path(csv_file).exists()
        Path(csv_file).unlink()


# ============================================================================
# OCIMF Wind Command Tests
# ============================================================================

class TestOCIMFWindCommand:
    """Test OCIMF wind loading CLI command"""

    def test_wind_with_standard_vessel(self, runner):
        """Test wind load with standard vessel type"""
        result = runner.invoke(cli, [
            'ocimf-wind',
            '--vessel-type', 'fpso',
            '--wind-speed', '25.0',
            '--wind-direction', '0.0',
        ])

        assert result.exit_code == 0
        assert 'OCIMF Wind Loading' in result.output
        assert 'Standard FPSO' in result.output
        assert 'Surge' in result.output
        assert 'Sway' in result.output
        assert 'Yaw' in result.output

    def test_wind_with_custom_vessel(self, runner):
        """Test wind load with custom vessel dimensions"""
        result = runner.invoke(cli, [
            'ocimf-wind',
            '--length', '250.0',
            '--beam', '45.0',
            '--draft', '16.0',
            '--displacement', '150000',
            '--wind-speed', '20.0',
            '--wind-direction', '45.0',
        ])

        assert result.exit_code == 0
        assert 'Custom Vessel' in result.output

    def test_wind_with_output(self, runner, temp_output_file):
        """Test wind load with JSON output"""
        result = runner.invoke(cli, [
            'ocimf-wind',
            '--vessel-type', 'fpso',
            '--wind-speed', '25.0',
            '--output', temp_output_file,
        ])

        assert result.exit_code == 0
        assert Path(temp_output_file).exists()

        # Verify JSON structure
        with open(temp_output_file, 'r') as f:
            data = json.load(f)

        assert 'vessel' in data
        assert 'loads' in data
        assert 'Fx_surge_N' in data['loads']

    def test_wind_missing_vessel_info(self, runner):
        """Test error when vessel info incomplete"""
        result = runner.invoke(cli, [
            'ocimf-wind',
            '--length', '250.0',
            # Missing beam, draft, displacement
            '--wind-speed', '20.0',
        ])

        assert result.exit_code != 0


# ============================================================================
# OCIMF Current Command Tests
# ============================================================================

class TestOCIMFCurrentCommand:
    """Test OCIMF current loading CLI command"""

    def test_current_with_standard_vessel(self, runner):
        """Test current load with standard vessel"""
        result = runner.invoke(cli, [
            'ocimf-current',
            '--vessel-type', 'semisubmersible',
            '--current-speed', '1.5',
            '--current-direction', '90.0',
        ])

        assert result.exit_code == 0
        assert 'OCIMF Current Loading' in result.output
        assert 'Semi-submersible' in result.output

    def test_current_with_output(self, runner, temp_output_file):
        """Test current load with JSON output"""
        result = runner.invoke(cli, [
            'ocimf-current',
            '--vessel-type', 'tanker',
            '--current-speed', '2.0',
            '--output', temp_output_file,
        ])

        assert result.exit_code == 0
        assert Path(temp_output_file).exists()

        with open(temp_output_file, 'r') as f:
            data = json.load(f)

        assert 'loads' in data


# ============================================================================
# Combined Environment Command Tests
# ============================================================================

class TestCombinedEnvironmentCommand:
    """Test combined environmental loading CLI command"""

    def test_combined_basic(self, runner):
        """Test combined wind and current loads"""
        result = runner.invoke(cli, [
            'combined-env',
            '--vessel-type', 'fpso',
            '--wind-speed', '25.0',
            '--wind-direction', '45.0',
            '--current-speed', '1.5',
            '--current-direction', '90.0',
        ])

        assert result.exit_code == 0
        assert 'Combined Environmental Loading' in result.output
        assert 'Wind Loads:' in result.output
        assert 'Current Loads:' in result.output
        assert 'Combined Loads:' in result.output
        assert 'Resultant' in result.output

    def test_combined_wind_only(self, runner):
        """Test with wind only (no current)"""
        result = runner.invoke(cli, [
            'combined-env',
            '--vessel-type', 'fpso',
            '--wind-speed', '30.0',
            '--current-speed', '0.0',
        ])

        assert result.exit_code == 0

    def test_combined_with_output(self, runner, temp_output_file):
        """Test combined loads with JSON output"""
        result = runner.invoke(cli, [
            'combined-env',
            '--vessel-type', 'fpso',
            '--wind-speed', '20.0',
            '--wind-direction', '30.0',
            '--current-speed', '1.0',
            '--current-direction', '60.0',
            '--output', temp_output_file,
        ])

        assert result.exit_code == 0
        assert Path(temp_output_file).exists()

        with open(temp_output_file, 'r') as f:
            data = json.load(f)

        assert 'vessel' in data
        assert 'environment' in data
        assert 'results' in data

        results = data['results']
        assert 'wind_loads' in results
        assert 'current_loads' in results
        assert 'combined_loads' in results


# ============================================================================
# CLI Utilities Tests
# ============================================================================

class TestCLIUtilities:
    """Test CLI utility commands"""

    def test_version(self, runner):
        """Test --version flag"""
        result = runner.invoke(cli, ['--version'])

        assert result.exit_code == 0
        assert 'hydrodynamics' in result.output

    def test_help(self, runner):
        """Test --help flag"""
        result = runner.invoke(cli, ['--help'])

        assert result.exit_code == 0
        assert 'Hydrodynamics Analysis Tools' in result.output
        assert 'spectrum' in result.output
        assert 'ocimf-wind' in result.output
        assert 'ocimf-current' in result.output
        assert 'combined-env' in result.output

    def test_spectrum_help(self, runner):
        """Test spectrum --help"""
        result = runner.invoke(cli, ['spectrum', '--help'])

        assert result.exit_code == 0
        assert '--hs' in result.output
        assert '--tp' in result.output
        assert '--gamma' in result.output
