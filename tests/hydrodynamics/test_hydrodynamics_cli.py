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
import numpy as np
import pandas as pd

from digitalmodel.hydrodynamics.cli import cli
from digitalmodel.hydrodynamics.models import WaveParameters, WaveSpectrumType
from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra


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
    with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False,
                                     encoding="utf-8", newline="\n") as f:
        yield f.name
    Path(f.name).unlink(missing_ok=True)


# ============================================================================
# Spectrum Command Tests
# ============================================================================

class TestSpectrumCommand:
    """Test wave spectrum CLI command"""

    @pytest.mark.parametrize("spectrum_type", ["pm", "jonswap", "bretschneider", "issc"])
    def test_default_grid_matches_api(self, runner, tmp_path, spectrum_type):
        """Omitted CLI grid options shall retain the shared spectrum tail."""
        output = tmp_path / "spectrum.csv"
        result = runner.invoke(cli, [
            "spectrum", "--type", spectrum_type, "--hs", "4", "--tp", "6",
            "--output", output.as_posix(),
        ])
        assert result.exit_code == 0, result.output
        actual = pd.read_csv(output, encoding="utf-8")
        # For this synthetic Tp=6 s case, omega_p=pi/3 rad/s and 20*omega_p
        # is 20.9439510239 rad/s. The shared numerical policy uses 1000 points.
        assert len(actual) == 1000
        assert actual["frequency_rad_s"].iloc[0] == pytest.approx(0.02)
        assert actual["frequency_rad_s"].iloc[-1] == pytest.approx(20.9439510239)
        kind = (WaveSpectrumType.PIERSON_MOSKOWITZ if spectrum_type == "pm"
                else WaveSpectrumType(spectrum_type))
        omega, density = WaveSpectra().generate_spectrum(WaveParameters(
            spectrum_type=kind, significant_height=4.0, peak_period=6.0,
        ))
        np.testing.assert_allclose(actual["frequency_rad_s"], omega)
        np.testing.assert_allclose(actual["spectral_density"], density, atol=1e-15)
        if spectrum_type == "pm":
            # The independent gamma-function PM integral in the spectrum
            # documentation gives m0=1, m2=2.1731 and Tz=4.262 s. The 0.02 s
            # allowance covers the tail/quadrature budget and CLI rounding.
            fields = dict(line.split(":", 1) for line in result.output.splitlines()
                          if ":" in line)
            assert float(fields["Hs"].split()[0]) == pytest.approx(4.0)
            assert float(fields["Tz"].split()[0]) == pytest.approx(4.262, abs=0.02)
            assert float(result.output.split("m2 = ")[1].split()[0]) == pytest.approx(2.1731, rel=0.005)

    @pytest.mark.parametrize("options,count,lower,upper", [
        (["--freq-min", "0.1"], 1000, 0.1, 20.9439510239),
        (["--freq-max", "3"], 1000, 0.02, 3.0),
        (["--n-points", "31"], 31, 0.02, 20.9439510239),
        (["--freq-min", "0.1", "--freq-max", "3", "--n-points", "31"],
         31, 0.1, 3.0),
    ])
    def test_explicit_grid_options(self, runner, tmp_path, options, count, lower, upper):
        output = tmp_path / "spectrum.csv"
        result = runner.invoke(cli, [
            "spectrum", "--type", "pm", "--hs", "4", "--tp", "6",
            "--output", output.as_posix(), *options,
        ])
        assert result.exit_code == 0, result.output
        actual = pd.read_csv(output, encoding="utf-8")["frequency_rad_s"]
        np.testing.assert_allclose(actual, np.linspace(lower, upper, count))

    @pytest.mark.parametrize("options", [
        ["--freq-min", "30"], ["--freq-max", "0.01"], ["--n-points", "1"],
    ])
    def test_invalid_grid_is_rejected(self, runner, options):
        result = runner.invoke(cli, [
            "spectrum", "--type", "pm", "--hs", "4", "--tp", "6", *options,
        ])
        assert result.exit_code != 0
        assert "Error:" in result.output

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
        assert f'saved to: {Path(csv_file).as_posix()}' in result.output

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
        with open(temp_output_file, 'r', encoding="utf-8") as f:
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

        with open(temp_output_file, 'r', encoding="utf-8") as f:
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

        with open(temp_output_file, 'r', encoding="utf-8") as f:
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
