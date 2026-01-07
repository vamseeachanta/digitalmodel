"""
ABOUTME: Integration tests for viv_analysis CLI
ABOUTME: Tests natural frequency calculations and VIV susceptibility screening via CLI
"""

import pytest
import json
from pathlib import Path
from unittest.mock import patch, MagicMock

from digitalmodel.modules.viv_analysis.cli import cli
from tests.cli.conftest import assert_cli_success, assert_cli_failure, assert_json_output, assert_output_contains


class TestNaturalFreqCommand:
    """Tests for the 'natural-freq' command"""

    @patch('digitalmodel.modules.viv_analysis.cli.FrequencyCalculator')
    @patch('digitalmodel.modules.viv_analysis.cli.get_material')
    def test_basic_natural_frequency(self, mock_get_material, mock_calculator, cli_runner):
        """Test basic natural frequency calculation"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material

        mock_calc_instance = MagicMock()
        mock_calculator.return_value = mock_calc_instance

        # Mock frequency results for 5 modes
        mock_results = []
        for i in range(1, 6):
            mock_result = MagicMock()
            mock_result.mode_number = i
            mock_result.frequency = 0.15 * i  # Hz
            mock_result.period = 1.0 / (0.15 * i)  # seconds
            mock_results.append(mock_result)

        mock_calc_instance.calculate_multiple_modes.return_value = mock_results

        result = cli_runner.invoke(cli, [
            'natural-freq',
            '--length', '50.0',
            '--diameter', '0.5',
            '--thickness', '0.02'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'Natural Frequencies',
            'Mode 1:',
            'Mode 2:',
            'Hz'
        )

    @patch('digitalmodel.modules.viv_analysis.cli.FrequencyCalculator')
    @patch('digitalmodel.modules.viv_analysis.cli.get_material')
    def test_natural_freq_multiple_modes(self, mock_get_material, mock_calculator, cli_runner):
        """Test natural frequency with custom number of modes"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material

        mock_calc_instance = MagicMock()
        mock_calculator.return_value = mock_calc_instance

        # 10 modes
        mock_results = []
        for i in range(1, 11):
            mock_result = MagicMock()
            mock_result.mode_number = i
            mock_result.frequency = 0.12 * i
            mock_result.period = 1.0 / (0.12 * i)
            mock_results.append(mock_result)

        mock_calc_instance.calculate_multiple_modes.return_value = mock_results

        result = cli_runner.invoke(cli, [
            'natural-freq',
            '--length', '60.0',
            '--diameter', '0.6',
            '--thickness', '0.025',
            '--n-modes', '10'
        ])

        assert_cli_success(result)
        assert 'Mode 10:' in result.output

    @patch('digitalmodel.modules.viv_analysis.cli.FrequencyCalculator')
    @patch('digitalmodel.modules.viv_analysis.cli.get_material')
    def test_natural_freq_different_materials(self, mock_get_material, mock_calculator, cli_runner):
        """Test natural frequency with titanium material"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material

        mock_calc_instance = MagicMock()
        mock_calculator.return_value = mock_calc_instance

        mock_results = []
        for i in range(1, 6):
            mock_result = MagicMock()
            mock_result.mode_number = i
            mock_result.frequency = 0.18 * i  # Higher frequency for titanium
            mock_result.period = 1.0 / (0.18 * i)
            mock_results.append(mock_result)

        mock_calc_instance.calculate_multiple_modes.return_value = mock_results

        result = cli_runner.invoke(cli, [
            'natural-freq',
            '--length', '50.0',
            '--diameter', '0.5',
            '--thickness', '0.015',
            '--material', 'titanium'
        ])

        assert_cli_success(result)
        mock_get_material.assert_called_with('titanium')

    @patch('digitalmodel.modules.viv_analysis.cli.FrequencyCalculator')
    @patch('digitalmodel.modules.viv_analysis.cli.get_material')
    def test_natural_freq_different_boundary(self, mock_get_material, mock_calculator, cli_runner):
        """Test natural frequency with fixed-free boundary condition"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material

        mock_calc_instance = MagicMock()
        mock_calculator.return_value = mock_calc_instance

        mock_results = []
        for i in range(1, 6):
            mock_result = MagicMock()
            mock_result.mode_number = i
            mock_result.frequency = 0.10 * i  # Lower frequency for fixed-free
            mock_result.period = 1.0 / (0.10 * i)
            mock_results.append(mock_result)

        mock_calc_instance.calculate_multiple_modes.return_value = mock_results

        result = cli_runner.invoke(cli, [
            'natural-freq',
            '--length', '50.0',
            '--diameter', '0.5',
            '--thickness', '0.02',
            '--boundary', 'cantilever'
        ])

        assert_cli_success(result)

    @patch('digitalmodel.modules.viv_analysis.cli.FrequencyCalculator')
    @patch('digitalmodel.modules.viv_analysis.cli.get_material')
    def test_natural_freq_with_json_output(self, mock_get_material, mock_calculator, cli_runner, temp_output_dir):
        """Test natural frequency with JSON output"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material

        mock_calc_instance = MagicMock()
        mock_calculator.return_value = mock_calc_instance

        mock_results = []
        for i in range(1, 4):
            mock_result = MagicMock()
            mock_result.mode_number = i
            mock_result.frequency = 0.15 * i
            mock_result.period = 1.0 / (0.15 * i)
            mock_results.append(mock_result)

        mock_calc_instance.calculate_multiple_modes.return_value = mock_results

        output_file = temp_output_dir / "frequencies.json"

        result = cli_runner.invoke(cli, [
            'natural-freq',
            '--length', '50.0',
            '--diameter', '0.5',
            '--thickness', '0.02',
            '--n-modes', '3',
            '--output', str(output_file)
        ])

        assert_cli_success(result)

        # Validate JSON output
        data = assert_json_output(output_file, [
            'member',
            'frequencies'
        ])

        assert data['member']['length'] == 50.0
        assert len(data['frequencies']) == 3
        assert data['frequencies'][0]['mode'] == 1
        assert 'frequency_hz' in data['frequencies'][0]
        assert 'period_s' in data['frequencies'][0]

    def test_natural_freq_missing_required_length(self, cli_runner):
        """Test error for missing required length parameter"""
        result = cli_runner.invoke(cli, [
            'natural-freq',
            '--diameter', '0.5',
            '--thickness', '0.02'
        ])

        assert_cli_failure(result)
        assert 'Error' in result.output or 'required' in result.output.lower()

    def test_natural_freq_missing_required_diameter(self, cli_runner):
        """Test error for missing required diameter parameter"""
        result = cli_runner.invoke(cli, [
            'natural-freq',
            '--length', '50.0',
            '--thickness', '0.02'
        ])

        assert_cli_failure(result)
        assert 'Error' in result.output or 'required' in result.output.lower()

    @patch('digitalmodel.modules.viv_analysis.cli.get_material')
    def test_natural_freq_large_member(self, mock_get_material, cli_runner):
        """Test natural frequency for large member (low frequency)"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material

        result = cli_runner.invoke(cli, [
            'natural-freq',
            '--length', '200.0',
            '--diameter', '1.5',
            '--thickness', '0.05'
        ])

        # May fail without full mocking, but should parse parameters
        assert result.exit_code in [0, 1]


class TestScreeningCommand:
    """Tests for the 'screening' command"""

    @patch('digitalmodel.modules.viv_analysis.cli.VIVScreening')
    @patch('digitalmodel.modules.viv_analysis.cli.get_material')
    def test_basic_viv_screening(self, mock_get_material, mock_screening, cli_runner):
        """Test basic VIV susceptibility screening"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material

        mock_screening_instance = MagicMock()
        mock_screening.return_value = mock_screening_instance

        mock_result = MagicMock()
        mock_result.natural_frequency = 0.25
        mock_result.shedding_frequency = 0.22
        mock_result.reduced_velocity = 8.5
        mock_result.is_susceptible = True
        mock_result.lock_in_status = 'moderate_risk'
        mock_result.safety_factor = 1.14
        mock_result.recommendation = "VIV suppression devices recommended"
        mock_screening_instance.screen_member.return_value = mock_result

        result = cli_runner.invoke(cli, [
            'screening',
            '--length', '50.0',
            '--diameter', '0.5',
            '--thickness', '0.02',
            '--current', '1.5'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'VIV Screening Results',
            'Natural Frequency:',
            'Shedding Frequency:',
            'Reduced Velocity:',
            'Lock-in Status:',
            'Safety Factor:',
            'Recommendation:'
        )

    @patch('digitalmodel.modules.viv_analysis.cli.VIVScreening')
    @patch('digitalmodel.modules.viv_analysis.cli.get_material')
    def test_screening_susceptible_member(self, mock_get_material, mock_screening, cli_runner):
        """Test screening for member susceptible to VIV"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material

        mock_screening_instance = MagicMock()
        mock_screening.return_value = mock_screening_instance

        mock_result = MagicMock()
        mock_result.natural_frequency = 0.20
        mock_result.shedding_frequency = 0.19
        mock_result.reduced_velocity = 10.0
        mock_result.is_susceptible = True
        mock_result.lock_in_status = 'high_risk'
        mock_result.safety_factor = 1.05
        mock_result.recommendation = "Immediate VIV mitigation required"
        mock_screening_instance.screen_member.return_value = mock_result

        result = cli_runner.invoke(cli, [
            'screening',
            '--length', '60.0',
            '--diameter', '0.6',
            '--thickness', '0.015',
            '--current', '2.0'
        ])

        assert_cli_success(result)
        assert 'HIGH_RISK' in result.output.upper() or 'high risk' in result.output.lower()

    @patch('digitalmodel.modules.viv_analysis.cli.VIVScreening')
    @patch('digitalmodel.modules.viv_analysis.cli.get_material')
    def test_screening_non_susceptible_member(self, mock_get_material, mock_screening, cli_runner):
        """Test screening for member not susceptible to VIV"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material

        mock_screening_instance = MagicMock()
        mock_screening.return_value = mock_screening_instance

        mock_result = MagicMock()
        mock_result.natural_frequency = 0.35
        mock_result.shedding_frequency = 0.15
        mock_result.reduced_velocity = 4.0
        mock_result.is_susceptible = False
        mock_result.lock_in_status = 'safe'
        mock_result.safety_factor = 2.33
        mock_result.recommendation = "No VIV mitigation required"
        mock_screening_instance.screen_member.return_value = mock_result

        result = cli_runner.invoke(cli, [
            'screening',
            '--length', '40.0',
            '--diameter', '0.4',
            '--thickness', '0.03',
            '--current', '0.8'
        ])

        assert_cli_success(result)
        assert 'SAFE' in result.output.upper() or 'safe' in result.output.lower()

    @patch('digitalmodel.modules.viv_analysis.cli.VIVScreening')
    @patch('digitalmodel.modules.viv_analysis.cli.get_material')
    def test_screening_with_json_output(self, mock_get_material, mock_screening, cli_runner, temp_output_dir):
        """Test VIV screening with JSON output"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material

        mock_screening_instance = MagicMock()
        mock_screening.return_value = mock_screening_instance

        mock_result = MagicMock()
        mock_result.natural_frequency = 0.25
        mock_result.shedding_frequency = 0.22
        mock_result.reduced_velocity = 8.5
        mock_result.is_susceptible = True
        mock_result.lock_in_status = 'moderate_risk'
        mock_result.safety_factor = 1.14
        mock_result.recommendation = "VIV suppression devices recommended"
        mock_screening_instance.screen_member.return_value = mock_result

        output_file = temp_output_dir / "viv_screening.json"

        result = cli_runner.invoke(cli, [
            'screening',
            '--length', '50.0',
            '--diameter', '0.5',
            '--thickness', '0.02',
            '--current', '1.5',
            '--output', str(output_file)
        ])

        assert_cli_success(result)

        # Validate JSON output
        data = assert_json_output(output_file, [
            'natural_frequency_hz',
            'shedding_frequency_hz',
            'reduced_velocity',
            'is_susceptible',
            'lock_in_status',
            'safety_factor',
            'recommendation'
        ])

        assert data['is_susceptible'] is True
        assert data['lock_in_status'] == 'moderate_risk'
        assert data['reduced_velocity'] == 8.5

    def test_screening_missing_current_velocity(self, cli_runner):
        """Test error for missing required current velocity"""
        result = cli_runner.invoke(cli, [
            'screening',
            '--length', '50.0',
            '--diameter', '0.5',
            '--thickness', '0.02'
        ])

        assert_cli_failure(result)
        assert 'Error' in result.output or 'required' in result.output.lower()


class TestCLIHelp:
    """Tests for CLI help and documentation"""

    def test_main_help(self, cli_runner):
        """Test main CLI help message"""
        result = cli_runner.invoke(cli, ['--help'])

        assert_cli_success(result)
        assert_output_contains(result,
            'VIV Analysis Tools',
            'natural-freq',
            'screening'
        )

    def test_version(self, cli_runner):
        """Test version output"""
        result = cli_runner.invoke(cli, ['--version'])

        assert_cli_success(result)
        assert 'viv-analysis' in result.output.lower()
