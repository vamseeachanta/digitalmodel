"""
ABOUTME: Integration tests for hydrodynamics CLI
ABOUTME: Tests wave spectra, OCIMF loading calculations, and coefficient database via CLI
"""

import pytest
import json
import numpy as np
import pandas as pd
from pathlib import Path
from unittest.mock import patch, MagicMock

from digitalmodel.hydrodynamics.cli import cli
from tests.specialized.cli.conftest import assert_cli_success, assert_cli_failure, assert_output_contains


class TestSpectrumCommand:
    """Tests for the 'spectrum' command"""

    @patch('digitalmodel.hydrodynamics.cli.WaveSpectra')
    def test_basic_jonswap_spectrum(self, mock_spectra, cli_runner):
        """Test basic JONSWAP wave spectrum generation"""
        mock_generator = MagicMock()
        mock_spectra.return_value = mock_generator

        # Mock spectrum data
        frequencies = np.linspace(0.02, 2.0, 100)
        S = np.random.rand(100) * 10.0  # Random spectral densities
        mock_generator.generate_spectrum.return_value = (frequencies, S)

        # Mock statistics
        mock_stats = {
            'Hs_m': 8.0,
            'Tp_s': 12.0,
            'Tz_s': 9.5,
            'omega_p_rad_s': 0.524,
            'spectral_width': 0.15,
            'm0': 16.0,
            'm2': 0.18,
            'm4': 0.008
        }
        mock_generator.spectrum_statistics.return_value = mock_stats

        result = cli_runner.invoke(cli, [
            'spectrum',
            '--hs', '8.0',
            '--tp', '12.0'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'Wave Spectrum Analysis',
            'JONSWAP',
            'Hs:',
            '8.00 m',
            'Tp:',
            '12.00 s',
            'Spectral Width:'
        )

    @patch('digitalmodel.hydrodynamics.cli.WaveSpectra')
    def test_spectrum_pierson_moskowitz(self, mock_spectra, cli_runner):
        """Test Pierson-Moskowitz spectrum type"""
        mock_generator = MagicMock()
        mock_spectra.return_value = mock_generator

        frequencies = np.linspace(0.02, 2.0, 100)
        S = np.random.rand(100) * 10.0
        mock_generator.generate_spectrum.return_value = (frequencies, S)

        mock_stats = {
            'Hs_m': 6.0,
            'Tp_s': 10.0,
            'Tz_s': 8.0,
            'omega_p_rad_s': 0.628,
            'spectral_width': 0.12,
            'm0': 9.0,
            'm2': 0.12,
            'm4': 0.004
        }
        mock_generator.spectrum_statistics.return_value = mock_stats

        result = cli_runner.invoke(cli, [
            'spectrum',
            '--type', 'pm',
            '--hs', '6.0',
            '--tp', '10.0'
        ])

        assert_cli_success(result)
        assert 'PM' in result.output.upper()

    @patch('digitalmodel.hydrodynamics.cli.WaveSpectra')
    def test_spectrum_with_custom_parameters(self, mock_spectra, cli_runner):
        """Test spectrum with custom gamma and frequency range"""
        mock_generator = MagicMock()
        mock_spectra.return_value = mock_generator

        frequencies = np.linspace(0.01, 3.0, 200)
        S = np.random.rand(200) * 10.0
        mock_generator.generate_spectrum.return_value = (frequencies, S)

        mock_stats = {
            'Hs_m': 10.0,
            'Tp_s': 14.0,
            'Tz_s': 11.0,
            'omega_p_rad_s': 0.449,
            'spectral_width': 0.18,
            'm0': 25.0,
            'm2': 0.25,
            'm4': 0.015
        }
        mock_generator.spectrum_statistics.return_value = mock_stats

        result = cli_runner.invoke(cli, [
            'spectrum',
            '--hs', '10.0',
            '--tp', '14.0',
            '--gamma', '5.0',
            '--freq-min', '0.01',
            '--freq-max', '3.0',
            '--n-points', '200'
        ])

        assert_cli_success(result)
        assert 'Hs:' in result.output

    @patch('digitalmodel.hydrodynamics.cli.WaveSpectra')
    def test_spectrum_with_csv_output(self, mock_spectra, cli_runner, temp_output_dir):
        """Test spectrum generation with CSV output"""
        mock_generator = MagicMock()
        mock_spectra.return_value = mock_generator

        frequencies = np.linspace(0.02, 2.0, 100)
        S = np.random.rand(100) * 10.0
        mock_generator.generate_spectrum.return_value = (frequencies, S)

        mock_stats = {
            'Hs_m': 8.0,
            'Tp_s': 12.0,
            'Tz_s': 9.5,
            'omega_p_rad_s': 0.524,
            'spectral_width': 0.15,
            'm0': 16.0,
            'm2': 0.18,
            'm4': 0.008
        }
        mock_generator.spectrum_statistics.return_value = mock_stats

        output_file = temp_output_dir / "spectrum.csv"

        result = cli_runner.invoke(cli, [
            'spectrum',
            '--hs', '8.0',
            '--tp', '12.0',
            '--output', str(output_file)
        ])

        assert_cli_success(result)
        assert output_file.exists()

        # Verify CSV structure
        df = pd.read_csv(output_file)
        assert 'frequency_rad_s' in df.columns
        assert 'frequency_hz' in df.columns
        assert 'period_s' in df.columns
        assert 'spectral_density' in df.columns
        assert len(df) == 100

    @patch('digitalmodel.hydrodynamics.cli.WaveSpectra')
    def test_spectrum_statistics_validation(self, mock_spectra, cli_runner):
        """Test that spectrum statistics are displayed correctly"""
        mock_generator = MagicMock()
        mock_spectra.return_value = mock_generator

        frequencies = np.linspace(0.02, 2.0, 100)
        S = np.random.rand(100) * 10.0
        mock_generator.generate_spectrum.return_value = (frequencies, S)

        mock_stats = {
            'Hs_m': 5.0,
            'Tp_s': 9.0,
            'Tz_s': 7.2,
            'omega_p_rad_s': 0.698,
            'spectral_width': 0.14,
            'm0': 6.25,
            'm2': 0.10,
            'm4': 0.003
        }
        mock_generator.spectrum_statistics.return_value = mock_stats

        result = cli_runner.invoke(cli, [
            'spectrum',
            '--hs', '5.0',
            '--tp', '9.0'
        ])

        assert_cli_success(result)
        assert 'Moments:' in result.output
        assert 'm0' in result.output
        assert 'm2' in result.output
        assert 'm4' in result.output

    def test_spectrum_missing_required_parameters(self, cli_runner):
        """Test error for missing required Hs parameter"""
        result = cli_runner.invoke(cli, [
            'spectrum',
            '--tp', '12.0'
        ])

        assert_cli_failure(result)
        assert 'Error' in result.output or 'required' in result.output.lower()


class TestOCIMFWindCommand:
    """Tests for the 'ocimf-wind' command"""

    @patch('digitalmodel.hydrodynamics.cli.OCIMFLoading')
    @patch('digitalmodel.hydrodynamics.cli.get_vessel_type')
    def test_wind_load_standard_vessel(self, mock_get_vessel, mock_ocimf, cli_runner):
        """Test OCIMF wind load calculation for standard vessel type"""
        # Mock vessel
        mock_vessel = MagicMock()
        mock_vessel.name = "FPSO Standard"
        mock_vessel.length_overall = 280.0
        mock_vessel.beam = 46.0
        mock_vessel.draft = 17.5
        mock_get_vessel.return_value = mock_vessel

        # Mock OCIMF loading
        mock_ocimf_instance = MagicMock()
        mock_ocimf.return_value = mock_ocimf_instance

        mock_loads = {
            'Fx_surge_N': 1200000.0,
            'Fy_sway_N': 250000.0,
            'Mz_yaw_Nm': 85000000.0
        }
        mock_ocimf_instance.wind_load.return_value = mock_loads

        result = cli_runner.invoke(cli, [
            'ocimf-wind',
            '--vessel-type', 'fpso',
            '--wind-speed', '25.0',
            '--wind-direction', '45.0'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'OCIMF Wind Loading',
            'FPSO Standard',
            'Wind Speed:',
            '25.0 m/s',
            'Wind Direction:',
            '45.0°',
            'Wind Loads:',
            'Surge (Fx):',
            'Sway (Fy):',
            'Yaw (Mz):'
        )

    @patch('digitalmodel.hydrodynamics.cli.OCIMFLoading')
    def test_wind_load_custom_vessel(self, mock_ocimf, cli_runner):
        """Test OCIMF wind load with custom vessel dimensions"""
        mock_ocimf_instance = MagicMock()
        mock_ocimf.return_value = mock_ocimf_instance

        mock_loads = {
            'Fx_surge_N': 1000000.0,
            'Fy_sway_N': 200000.0,
            'Mz_yaw_Nm': 70000000.0
        }
        mock_ocimf_instance.wind_load.return_value = mock_loads

        result = cli_runner.invoke(cli, [
            'ocimf-wind',
            '--length', '250.0',
            '--beam', '40.0',
            '--draft', '15.0',
            '--displacement', '120000',
            '--wind-speed', '20.0'
        ])

        assert_cli_success(result)
        assert 'Custom Vessel' in result.output or 'L=250.0m' in result.output

    @patch('digitalmodel.hydrodynamics.cli.OCIMFLoading')
    @patch('digitalmodel.hydrodynamics.cli.get_vessel_type')
    def test_wind_load_varying_direction(self, mock_get_vessel, mock_ocimf, cli_runner):
        """Test wind load with different wind directions"""
        mock_vessel = MagicMock()
        mock_vessel.name = "Tanker"
        mock_vessel.length_overall = 300.0
        mock_vessel.beam = 50.0
        mock_vessel.draft = 20.0
        mock_get_vessel.return_value = mock_vessel

        mock_ocimf_instance = MagicMock()
        mock_ocimf.return_value = mock_ocimf_instance

        mock_loads = {
            'Fx_surge_N': 800000.0,
            'Fy_sway_N': 600000.0,
            'Mz_yaw_Nm': 90000000.0
        }
        mock_ocimf_instance.wind_load.return_value = mock_loads

        result = cli_runner.invoke(cli, [
            'ocimf-wind',
            '--vessel-type', 'tanker',
            '--wind-speed', '30.0',
            '--wind-direction', '90.0'
        ])

        assert_cli_success(result)
        assert '90.0°' in result.output

    @patch('digitalmodel.hydrodynamics.cli.OCIMFLoading')
    @patch('digitalmodel.hydrodynamics.cli.get_vessel_type')
    def test_wind_load_with_json_output(self, mock_get_vessel, mock_ocimf, cli_runner, temp_output_dir):
        """Test wind load calculation with JSON output"""
        mock_vessel = MagicMock()
        mock_vessel.name = "FPSO"
        mock_vessel.length_overall = 280.0
        mock_vessel.beam = 46.0
        mock_vessel.draft = 17.5
        mock_vessel.to_dict.return_value = {
            'name': 'FPSO',
            'length_overall': 280.0,
            'beam': 46.0,
            'draft': 17.5
        }
        mock_get_vessel.return_value = mock_vessel

        mock_ocimf_instance = MagicMock()
        mock_ocimf.return_value = mock_ocimf_instance

        mock_loads = {
            'Fx_surge_N': 1200000.0,
            'Fy_sway_N': 250000.0,
            'Mz_yaw_Nm': 85000000.0
        }
        mock_ocimf_instance.wind_load.return_value = mock_loads

        output_file = temp_output_dir / "wind_loads.json"

        result = cli_runner.invoke(cli, [
            'ocimf-wind',
            '--vessel-type', 'fpso',
            '--wind-speed', '25.0',
            '--output', str(output_file)
        ])

        assert_cli_success(result)
        assert output_file.exists()

        with open(output_file) as f:
            data = json.load(f)

        assert 'vessel' in data
        assert 'loads' in data
        assert data['loads']['Fx_surge_N'] == 1200000.0

    def test_wind_load_missing_vessel_info(self, cli_runner):
        """Test error when neither vessel type nor dimensions provided"""
        result = cli_runner.invoke(cli, [
            'ocimf-wind',
            '--wind-speed', '25.0'
        ])

        assert_cli_failure(result)
        assert 'vessel' in result.output.lower()


class TestOCIMFCurrentCommand:
    """Tests for the 'ocimf-current' command"""

    @patch('digitalmodel.hydrodynamics.cli.OCIMFLoading')
    @patch('digitalmodel.hydrodynamics.cli.get_vessel_type')
    def test_current_load_standard_vessel(self, mock_get_vessel, mock_ocimf, cli_runner):
        """Test OCIMF current load calculation for standard vessel"""
        mock_vessel = MagicMock()
        mock_vessel.name = "Semisubmersible"
        mock_vessel.length_overall = 120.0
        mock_vessel.beam = 90.0
        mock_vessel.draft = 25.0
        mock_get_vessel.return_value = mock_vessel

        mock_ocimf_instance = MagicMock()
        mock_ocimf.return_value = mock_ocimf_instance

        mock_loads = {
            'Fx_surge_N': 800000.0,
            'Fy_sway_N': 150000.0,
            'Mz_yaw_Nm': 45000000.0
        }
        mock_ocimf_instance.current_load.return_value = mock_loads

        result = cli_runner.invoke(cli, [
            'ocimf-current',
            '--vessel-type', 'semisubmersible',
            '--current-speed', '1.5'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'OCIMF Current Loading',
            'Semisubmersible',
            'Current Speed:',
            '1.50 m/s',
            'Current Loads:'
        )

    @patch('digitalmodel.hydrodynamics.cli.OCIMFLoading')
    def test_current_load_custom_vessel(self, mock_ocimf, cli_runner):
        """Test current load with custom vessel dimensions"""
        mock_ocimf_instance = MagicMock()
        mock_ocimf.return_value = mock_ocimf_instance

        mock_loads = {
            'Fx_surge_N': 600000.0,
            'Fy_sway_N': 120000.0,
            'Mz_yaw_Nm': 35000000.0
        }
        mock_ocimf_instance.current_load.return_value = mock_loads

        result = cli_runner.invoke(cli, [
            'ocimf-current',
            '--length', '200.0',
            '--beam', '35.0',
            '--draft', '12.0',
            '--displacement', '80000',
            '--current-speed', '1.2',
            '--current-direction', '30.0'
        ])

        assert_cli_success(result)
        assert '30.0°' in result.output

    @patch('digitalmodel.hydrodynamics.cli.OCIMFLoading')
    @patch('digitalmodel.hydrodynamics.cli.get_vessel_type')
    def test_current_load_with_json_output(self, mock_get_vessel, mock_ocimf, cli_runner, temp_output_dir):
        """Test current load with JSON output"""
        mock_vessel = MagicMock()
        mock_vessel.name = "Tanker"
        mock_vessel.length_overall = 300.0
        mock_vessel.beam = 50.0
        mock_vessel.draft = 20.0
        mock_vessel.to_dict.return_value = {
            'name': 'Tanker',
            'length_overall': 300.0
        }
        mock_get_vessel.return_value = mock_vessel

        mock_ocimf_instance = MagicMock()
        mock_ocimf.return_value = mock_ocimf_instance

        mock_loads = {
            'Fx_surge_N': 900000.0,
            'Fy_sway_N': 180000.0,
            'Mz_yaw_Nm': 55000000.0
        }
        mock_ocimf_instance.current_load.return_value = mock_loads

        output_file = temp_output_dir / "current_loads.json"

        result = cli_runner.invoke(cli, [
            'ocimf-current',
            '--vessel-type', 'tanker',
            '--current-speed', '1.8',
            '--output', str(output_file)
        ])

        assert_cli_success(result)
        assert output_file.exists()

    def test_current_load_missing_required_speed(self, cli_runner):
        """Test error for missing required current speed"""
        result = cli_runner.invoke(cli, [
            'ocimf-current',
            '--vessel-type', 'fpso'
        ])

        assert_cli_failure(result)
        assert 'Error' in result.output or 'required' in result.output.lower()


class TestCombinedEnvCommand:
    """Tests for the 'combined-env' command"""

    @patch('digitalmodel.hydrodynamics.cli.OCIMFLoading')
    @patch('digitalmodel.hydrodynamics.cli.get_vessel_type')
    def test_combined_environmental_loads(self, mock_get_vessel, mock_ocimf, cli_runner):
        """Test combined wind and current environmental loads"""
        mock_vessel = MagicMock()
        mock_vessel.name = "FPSO Platform"
        mock_get_vessel.return_value = mock_vessel

        mock_ocimf_instance = MagicMock()
        mock_ocimf.return_value = mock_ocimf_instance

        mock_result = {
            'wind_loads': {
                'Fx_surge_N': 1000000.0,
                'Fy_sway_N': 200000.0,
                'Mz_yaw_Nm': 70000000.0
            },
            'current_loads': {
                'Fx_surge_N': 600000.0,
                'Fy_sway_N': 100000.0,
                'Mz_yaw_Nm': 30000000.0
            },
            'combined_loads': {
                'Fx_surge_N': 1600000.0,
                'Fy_sway_N': 300000.0,
                'Mz_yaw_Nm': 100000000.0,
                'F_resultant_N': 1628000.0,
                'resultant_direction_deg': 10.6
            }
        }
        mock_ocimf_instance.combined_environmental_load.return_value = mock_result

        result = cli_runner.invoke(cli, [
            'combined-env',
            '--vessel-type', 'fpso',
            '--wind-speed', '22.0',
            '--wind-direction', '0.0',
            '--current-speed', '1.3',
            '--current-direction', '0.0'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'Combined Environmental Loading',
            'Wind Loads:',
            'Current Loads:',
            'Combined Loads:',
            'Resultant'
        )

    @patch('digitalmodel.hydrodynamics.cli.OCIMFLoading')
    @patch('digitalmodel.hydrodynamics.cli.get_vessel_type')
    def test_combined_different_vessel_types(self, mock_get_vessel, mock_ocimf, cli_runner):
        """Test combined loads for different vessel types"""
        mock_vessel = MagicMock()
        mock_vessel.name = "Semisubmersible Rig"
        mock_get_vessel.return_value = mock_vessel

        mock_ocimf_instance = MagicMock()
        mock_ocimf.return_value = mock_ocimf_instance

        mock_result = {
            'wind_loads': {
                'Fx_surge_N': 800000.0,
                'Fy_sway_N': 150000.0,
                'Mz_yaw_Nm': 50000000.0
            },
            'current_loads': {
                'Fx_surge_N': 500000.0,
                'Fy_sway_N': 80000.0,
                'Mz_yaw_Nm': 25000000.0
            },
            'combined_loads': {
                'Fx_surge_N': 1300000.0,
                'Fy_sway_N': 230000.0,
                'Mz_yaw_Nm': 75000000.0,
                'F_resultant_N': 1320000.0,
                'resultant_direction_deg': 10.0
            }
        }
        mock_ocimf_instance.combined_environmental_load.return_value = mock_result

        result = cli_runner.invoke(cli, [
            'combined-env',
            '--vessel-type', 'semisubmersible',
            '--wind-speed', '18.0',
            '--current-speed', '1.0'
        ])

        assert_cli_success(result)
        assert 'Semisubmersible' in result.output

    @patch('digitalmodel.hydrodynamics.cli.OCIMFLoading')
    @patch('digitalmodel.hydrodynamics.cli.get_vessel_type')
    def test_combined_with_json_output(self, mock_get_vessel, mock_ocimf, cli_runner, temp_output_dir):
        """Test combined environmental loads with JSON output"""
        mock_vessel = MagicMock()
        mock_vessel.name = "FPSO"
        mock_vessel.to_dict.return_value = {'name': 'FPSO'}
        mock_get_vessel.return_value = mock_vessel

        mock_ocimf_instance = MagicMock()
        mock_ocimf.return_value = mock_ocimf_instance

        mock_result = {
            'wind_loads': {
                'Fx_surge_N': 1000000.0,
                'Fy_sway_N': 200000.0,
                'Mz_yaw_Nm': 70000000.0
            },
            'current_loads': {
                'Fx_surge_N': 600000.0,
                'Fy_sway_N': 100000.0,
                'Mz_yaw_Nm': 30000000.0
            },
            'combined_loads': {
                'Fx_surge_N': 1600000.0,
                'Fy_sway_N': 300000.0,
                'Mz_yaw_Nm': 100000000.0,
                'F_resultant_N': 1628000.0,
                'resultant_direction_deg': 10.6
            }
        }
        mock_ocimf_instance.combined_environmental_load.return_value = mock_result

        output_file = temp_output_dir / "combined_loads.json"

        result = cli_runner.invoke(cli, [
            'combined-env',
            '--vessel-type', 'fpso',
            '--wind-speed', '22.0',
            '--current-speed', '1.3',
            '--output', str(output_file)
        ])

        assert_cli_success(result)
        assert output_file.exists()

        with open(output_file) as f:
            data = json.load(f)

        assert 'vessel' in data
        assert 'environment' in data
        assert 'results' in data


class TestCLIHelp:
    """Tests for CLI help and documentation"""

    def test_main_help(self, cli_runner):
        """Test main CLI help message"""
        result = cli_runner.invoke(cli, ['--help'])

        assert_cli_success(result)
        assert_output_contains(result,
            'Hydrodynamics Analysis Tools',
            'spectrum',
            'ocimf-wind',
            'ocimf-current',
            'combined-env'
        )

    def test_version(self, cli_runner):
        """Test version output"""
        result = cli_runner.invoke(cli, ['--version'])

        assert_cli_success(result)
        assert 'hydrodynamics' in result.output.lower()
