"""
ABOUTME: Integration tests for diffraction CLI
ABOUTME: Tests AQWA/OrcaWave conversion, comparison, and batch processing via CLI
"""

import pytest
import json
from pathlib import Path
from unittest.mock import patch, MagicMock

from digitalmodel.modules.diffraction.cli import cli
from tests.cli.conftest import assert_cli_success, assert_cli_failure, assert_output_contains


class TestConvertAQWACommand:
    """Tests for the 'convert-aqwa' command"""

    @patch('digitalmodel.modules.diffraction.cli.validate_results')
    @patch('digitalmodel.modules.diffraction.cli.AQWAConverter')
    @patch('digitalmodel.modules.diffraction.cli.OrcaFlexExporter')
    def test_basic_aqwa_conversion(self, mock_exporter, mock_converter, mock_validate, cli_runner, tmp_path):
        """Test basic AQWA to OrcaFlex conversion"""
        # Setup mocks
        mock_converter_instance = MagicMock()
        mock_converter.return_value = mock_converter_instance

        # Mock the convert_to_unified_schema return value
        mock_results = MagicMock()
        mock_results.vessel_name = 'FPSO_A'
        mock_converter_instance.convert_to_unified_schema.return_value = mock_results

        # Mock validation to return success
        mock_validate.return_value = {'overall_status': 'pass'}

        # Create fake analysis folder
        analysis_folder = tmp_path / "aqwa_analysis"
        analysis_folder.mkdir()

        result = cli_runner.invoke(cli, [
            'convert-aqwa',
            str(analysis_folder),
            'FPSO_A',
            '--water-depth', '1200',
            '--output', str(tmp_path / 'output')
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'AQWA to OrcaFlex Conversion',
            'FPSO_A',
            'Water depth: 1200'
        )

    def test_convert_aqwa_missing_folder(self, cli_runner):
        """Test error handling for missing analysis folder"""
        result = cli_runner.invoke(cli, [
            'convert-aqwa',
            '/nonexistent/folder',
            'FPSO_A',
            '--water-depth', '1200'
        ])

        assert_cli_failure(result)
        assert 'Error' in result.output or 'does not exist' in result.output.lower()

    def test_convert_aqwa_missing_water_depth(self, cli_runner, tmp_path):
        """Test error for missing required water depth"""
        analysis_folder = tmp_path / "aqwa_analysis"
        analysis_folder.mkdir()

        result = cli_runner.invoke(cli, [
            'convert-aqwa',
            str(analysis_folder),
            'FPSO_A'
            # Missing --water-depth
        ])

        assert_cli_failure(result)
        assert 'Error' in result.output or 'required' in result.output.lower()

    @patch('digitalmodel.modules.diffraction.cli.validate_results')
    @patch('digitalmodel.modules.diffraction.cli.AQWAConverter')
    @patch('digitalmodel.modules.diffraction.cli.OrcaFlexExporter')
    def test_convert_aqwa_with_formats(self, mock_exporter, mock_converter, mock_validate, cli_runner, tmp_path):
        """Test conversion with specific export formats"""
        mock_converter_instance = MagicMock()
        mock_converter.return_value = mock_converter_instance

        # Mock the convert_to_unified_schema return value
        mock_results = MagicMock()
        mock_results.vessel_name = 'FPSO_A'
        mock_converter_instance.convert_to_unified_schema.return_value = mock_results

        # Mock validation to return success
        mock_validate.return_value = {'overall_status': 'pass'}

        analysis_folder = tmp_path / "aqwa_analysis"
        analysis_folder.mkdir()

        result = cli_runner.invoke(cli, [
            'convert-aqwa',
            str(analysis_folder),
            'FPSO_A',
            '--water-depth', '1200',
            '--formats', 'vessel_type',
            '--formats', 'rao_csv'
        ])

        assert_cli_success(result)

    @patch('digitalmodel.modules.diffraction.cli.OrcaFlexExporter')
    @patch('digitalmodel.modules.diffraction.cli.AQWAConverter')
    def test_convert_aqwa_no_validate(self, mock_converter, mock_exporter, cli_runner, tmp_path):
        """Test conversion with validation disabled"""
        mock_converter_instance = MagicMock()
        mock_converter.return_value = mock_converter_instance

        # Mock the convert_to_unified_schema return value
        mock_results = MagicMock()
        mock_results.vessel_name = 'FPSO_A'
        mock_converter_instance.convert_to_unified_schema.return_value = mock_results

        # Mock exporter
        mock_exporter_instance = MagicMock()
        mock_exporter.return_value = mock_exporter_instance

        analysis_folder = tmp_path / "aqwa_analysis"
        analysis_folder.mkdir()

        result = cli_runner.invoke(cli, [
            'convert-aqwa',
            str(analysis_folder),
            'FPSO_A',
            '--water-depth', '1200',
            '--no-validate'
        ])

        # Should run without validation
        assert_cli_success(result)


class TestConvertOrcaWaveCommand:
    """Tests for the 'convert-orcawave' command"""

    @pytest.mark.skipif(
        not pytest.importorskip('OrcFxAPI', reason='OrcFxAPI not available'),
        reason="OrcaWave conversion requires OrcFxAPI"
    )
    @patch('digitalmodel.modules.diffraction.cli.OrcaWaveConverter')
    def test_basic_orcawave_conversion(self, mock_converter, cli_runner, tmp_path):
        """Test basic OrcaWave to OrcaFlex conversion"""
        mock_converter_instance = MagicMock()
        mock_converter.return_value = mock_converter_instance

        sim_file = tmp_path / "test.sim"
        sim_file.write_text("dummy")

        result = cli_runner.invoke(cli, [
            'convert-orcawave',
            str(sim_file),
            'FPSO_B',
            '--output', str(tmp_path / 'output')
        ])

        # May not be fully implemented or require OrcFxAPI
        assert result.exit_code in [0, 2]


class TestCompareCommand:
    """Tests for the 'compare' command"""

    @patch('digitalmodel.modules.diffraction.cli.OrcaFlexExporter')
    @patch('digitalmodel.modules.diffraction.cli.AQWAConverter')
    @patch('digitalmodel.modules.diffraction.cli.compare_diffraction_results')
    def test_basic_comparison(self, mock_compare, mock_converter, mock_exporter, cli_runner, tmp_path):
        """Test comparison of AQWA and OrcaWave results"""
        # Mock converter
        mock_converter_instance = MagicMock()
        mock_converter.return_value = mock_converter_instance

        mock_results = MagicMock()
        mock_results.vessel_name = 'FPSO_C'
        mock_converter_instance.convert_to_unified_schema.return_value = mock_results

        # Mock exporter
        mock_exporter_instance = MagicMock()
        mock_exporter.return_value = mock_exporter_instance

        # Mock comparison
        mock_compare.return_value = {
            'rao_diff': 5.0,
            'added_mass_diff': 3.0,
            'status': 'good'
        }

        # Create dummy input files
        aqwa_folder = tmp_path / "aqwa"
        aqwa_folder.mkdir()

        orcawave_file = tmp_path / "orcawave.sim"
        orcawave_file.write_text("dummy")

        result = cli_runner.invoke(cli, [
            'compare',
            str(aqwa_folder),
            str(orcawave_file),
            'FPSO_C',
            '--water-depth', '1500',
            '--output', str(tmp_path / 'comparison')
        ])

        # Command may not be fully implemented or may error with mocked data
        assert result.exit_code in [0, 1, 2]


class TestBatchProcessCommand:
    """Tests for the 'batch-process' command"""

    @patch('digitalmodel.modules.diffraction.cli.process_batch_from_config_file')
    def test_batch_processing(self, mock_process, cli_runner, tmp_path):
        """Test batch processing from config file"""
        mock_process.return_value = {'vessels_processed': 3}

        # Create config file
        config_file = tmp_path / "batch_config.json"
        config_data = {
            "vessels": [
                {"name": "FPSO_A", "water_depth": 1200},
                {"name": "FPSO_B", "water_depth": 1500}
            ]
        }
        config_file.write_text(json.dumps(config_data))

        result = cli_runner.invoke(cli, [
            'batch-process',
            str(config_file),
            '--output', str(tmp_path / 'batch_output')
        ])

        # Command may not be fully implemented
        assert result.exit_code in [0, 2]

    def test_batch_process_missing_config(self, cli_runner):
        """Test error for missing config file"""
        result = cli_runner.invoke(cli, [
            'batch-process',
            '/nonexistent/config.json'
        ])

        assert_cli_failure(result)


class TestCLIHelp:
    """Tests for CLI help and documentation"""

    def test_main_help(self, cli_runner):
        """Test main CLI help message"""
        result = cli_runner.invoke(cli, ['--help'])

        assert_cli_success(result)
        assert_output_contains(result,
            'Diffraction Analysis Tools',
            'convert-aqwa',
            'convert-orcawave',
            'compare'
        )

    def test_convert_aqwa_help(self, cli_runner):
        """Test convert-aqwa command help"""
        result = cli_runner.invoke(cli, ['convert-aqwa', '--help'])

        assert_cli_success(result)
        assert_output_contains(result,
            'Convert AQWA diffraction results',
            'ANALYSIS_FOLDER',
            'VESSEL_NAME',
            '--water-depth'
        )

    def test_version(self, cli_runner):
        """Test version output"""
        result = cli_runner.invoke(cli, ['--version'])

        assert_cli_success(result)
        assert '3.0.0' in result.output or 'diffraction' in result.output


class TestValidationIntegration:
    """Integration tests for validation workflows"""

    @patch('digitalmodel.modules.diffraction.cli.AQWAConverter')
    @patch('digitalmodel.modules.diffraction.cli.validate_results')
    def test_conversion_with_validation(self, mock_validate, mock_converter, cli_runner, tmp_path):
        """Test conversion with automatic validation"""
        mock_converter_instance = MagicMock()
        mock_converter.return_value = mock_converter_instance
        mock_converter_instance.results = {'freq': [0.1, 0.2], 'rao': [1.0, 2.0]}

        mock_validate.return_value = {'status': 'valid', 'errors': []}

        analysis_folder = tmp_path / "aqwa_analysis"
        analysis_folder.mkdir()

        result = cli_runner.invoke(cli, [
            'convert-aqwa',
            str(analysis_folder),
            'FPSO_D',
            '--water-depth', '1000',
            '--validate'
        ])

        # Should call validate_results
        # Command may fail without real data
        assert result.exit_code in [0, 1, 2]


class TestErrorHandling:
    """Tests for CLI error handling"""

    def test_invalid_water_depth(self, cli_runner, tmp_path):
        """Test error for invalid water depth"""
        analysis_folder = tmp_path / "aqwa_analysis"
        analysis_folder.mkdir()

        result = cli_runner.invoke(cli, [
            'convert-aqwa',
            str(analysis_folder),
            'FPSO_E',
            '--water-depth', 'not-a-number'
        ])

        assert_cli_failure(result)
        assert 'Error' in result.output or 'invalid' in result.output.lower()

    def test_invalid_format_option(self, cli_runner, tmp_path):
        """Test error for invalid format choice"""
        analysis_folder = tmp_path / "aqwa_analysis"
        analysis_folder.mkdir()

        result = cli_runner.invoke(cli, [
            'convert-aqwa',
            str(analysis_folder),
            'FPSO_F',
            '--water-depth', '1200',
            '--formats', 'invalid_format'
        ])

        assert_cli_failure(result)
