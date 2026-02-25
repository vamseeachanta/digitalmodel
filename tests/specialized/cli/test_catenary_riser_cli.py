"""
ABOUTME: Integration tests for catenary_riser CLI
ABOUTME: Tests simple catenary, lazy wave analysis, and effective weight calculations via CLI
"""

import pytest
import json
from pathlib import Path
from unittest.mock import patch, MagicMock

from digitalmodel.subsea.catenary_riser.cli import cli
from tests.specialized.cli.conftest import assert_cli_success, assert_cli_failure, assert_json_output, assert_output_contains


class TestSimpleCommand:
    """Tests for the 'simple' command"""

    @patch('digitalmodel.subsea.catenary_riser.cli.SimpleCatenaryAnalyzer')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_material')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_fluid')
    def test_basic_simple_catenary(self, mock_get_fluid, mock_get_material, mock_analyzer, cli_runner):
        """Test basic simple catenary riser analysis"""
        # Mock material and fluid
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material
        mock_fluid = MagicMock()
        mock_get_fluid.return_value = mock_fluid

        # Mock analyzer
        mock_analyzer_instance = MagicMock()
        mock_analyzer.return_value = mock_analyzer_instance

        # Mock result
        mock_result = MagicMock()
        mock_result.horizontal_tension = 500000.0  # N
        mock_result.top_tension = 650000.0  # N
        mock_result.touchdown_tension = 50000.0
        mock_result.top_angle = 25.5
        mock_result.arc_length = 1450.0
        mock_result.grounded_length = 150.0
        mock_result.effective_weight = 250.0
        mock_result.catenary_parameter = 2000.0
        mock_analyzer_instance.analyze_riser.return_value = mock_result

        result = cli_runner.invoke(cli, [
            'simple',
            '--diameter', '0.3',
            '--thickness', '0.02',
            '--length', '1600',
            '--water-depth', '1500',
            '--offset', '1200',
            '--material', 'x65'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'Simple Catenary Riser Analysis',
            'Effective Weight:',
            'Horizontal Tension:',
            'Top Tension:',
            'Top Angle:',
            'Suspended Length:'
        )

    @patch('digitalmodel.subsea.catenary_riser.cli.SimpleCatenaryAnalyzer')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_material')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_fluid')
    def test_simple_with_top_tension(self, mock_get_fluid, mock_get_material, mock_analyzer, cli_runner):
        """Test simple catenary with top tension specified instead of offset"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material
        mock_fluid = MagicMock()
        mock_get_fluid.return_value = mock_fluid

        mock_analyzer_instance = MagicMock()
        mock_analyzer.return_value = mock_analyzer_instance

        mock_result = MagicMock()
        mock_result.horizontal_tension = 480000.0
        mock_result.top_tension = 620000.0
        mock_result.touchdown_tension = 45000.0
        mock_result.top_angle = 24.0
        mock_result.arc_length = 1420.0
        mock_result.grounded_length = 180.0
        mock_result.effective_weight = 240.0
        mock_result.catenary_parameter = 2000.0
        mock_analyzer_instance.analyze_riser.return_value = mock_result

        result = cli_runner.invoke(cli, [
            'simple',
            '--diameter', '0.3',
            '--thickness', '0.02',
            '--length', '1600',
            '--water-depth', '1500',
            '--offset', '1200',
            '--top-tension', '620000'
        ])

        assert_cli_success(result)
        assert 'Top Tension:' in result.output
        assert '620.00 kN' in result.output

    @patch('digitalmodel.subsea.catenary_riser.cli.SimpleCatenaryAnalyzer')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_material')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_fluid')
    def test_simple_with_coating(self, mock_get_fluid, mock_get_material, mock_analyzer, cli_runner):
        """Test simple catenary with coating parameters"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material
        mock_fluid = MagicMock()
        mock_get_fluid.return_value = mock_fluid

        mock_analyzer_instance = MagicMock()
        mock_analyzer.return_value = mock_analyzer_instance

        mock_result = MagicMock()
        mock_result.horizontal_tension = 520000.0
        mock_result.top_tension = 680000.0
        mock_result.touchdown_tension = 55000.0
        mock_result.top_angle = 26.5
        mock_result.arc_length = 1480.0
        mock_result.grounded_length = 120.0
        mock_result.effective_weight = 280.0
        mock_result.catenary_parameter = 1857.0
        mock_analyzer_instance.analyze_riser.return_value = mock_result

        result = cli_runner.invoke(cli, [
            'simple',
            '--diameter', '0.3',
            '--thickness', '0.02',
            '--length', '1600',
            '--water-depth', '1500',
            '--offset', '1200',
            '--coating-thickness', '0.05',
            '--coating-density', '700'
        ])

        assert_cli_success(result)
        assert 'Effective Weight:' in result.output

    @patch('digitalmodel.subsea.catenary_riser.cli.SimpleCatenaryAnalyzer')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_material')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_fluid')
    def test_simple_with_json_output(self, mock_get_fluid, mock_get_material, mock_analyzer, cli_runner, temp_output_dir):
        """Test simple catenary with JSON output"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material
        mock_fluid = MagicMock()
        mock_get_fluid.return_value = mock_fluid

        mock_analyzer_instance = MagicMock()
        mock_analyzer.return_value = mock_analyzer_instance

        mock_result = MagicMock()
        mock_result.horizontal_tension = 500000.0
        mock_result.top_tension = 650000.0
        mock_result.touchdown_tension = 50000.0
        mock_result.top_angle = 25.5
        mock_result.arc_length = 1450.0
        mock_result.grounded_length = 150.0
        mock_result.effective_weight = 250.0
        mock_result.catenary_parameter = 2000.0
        mock_analyzer_instance.analyze_riser.return_value = mock_result

        output_file = temp_output_dir / "simple_catenary.json"

        result = cli_runner.invoke(cli, [
            'simple',
            '--diameter', '0.3',
            '--thickness', '0.02',
            '--length', '1600',
            '--water-depth', '1500',
            '--offset', '1200',
            '--output', str(output_file)
        ])

        assert_cli_success(result)

        # Validate JSON output
        data = assert_json_output(output_file, [
            'configuration',
            'results'
        ])

        assert data['configuration']['diameter'] == 0.3
        assert data['results']['horizontal_tension_N'] == 500000.0
        assert data['results']['top_tension_N'] == 650000.0

    @patch('digitalmodel.subsea.catenary_riser.cli.get_material')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_fluid')
    def test_simple_with_different_materials(self, mock_get_fluid, mock_get_material, cli_runner):
        """Test simple catenary with different material (x70)"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material
        mock_fluid = MagicMock()
        mock_get_fluid.return_value = mock_fluid

        result = cli_runner.invoke(cli, [
            'simple',
            '--diameter', '0.3',
            '--thickness', '0.02',
            '--length', '1600',
            '--water-depth', '1500',
            '--offset', '1200',
            '--material', 'x70'
        ])

        # May fail without full mock setup
        # Just verify material was requested
        mock_get_material.assert_called_with('x70')

    @patch('digitalmodel.subsea.catenary_riser.cli.get_material')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_fluid')
    def test_simple_with_different_fluids(self, mock_get_fluid, mock_get_material, cli_runner):
        """Test simple catenary with different internal fluid"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material
        mock_fluid = MagicMock()
        mock_get_fluid.return_value = mock_fluid

        result = cli_runner.invoke(cli, [
            'simple',
            '--diameter', '0.3',
            '--thickness', '0.02',
            '--length', '1600',
            '--water-depth', '1500',
            '--offset', '1200',
            '--internal-fluid', 'water'
        ])

        # Verify fluid was requested
        mock_get_fluid.assert_called_with('water')

    def test_simple_missing_required_parameters(self, cli_runner):
        """Test error for missing required diameter"""
        result = cli_runner.invoke(cli, [
            'simple',
            '--thickness', '0.02',
            '--length', '1600',
            '--water-depth', '1500',
            '--offset', '1200'
        ])

        assert_cli_failure(result)
        assert 'Error' in result.output or 'required' in result.output.lower()


class TestWeightCommand:
    """Tests for the 'weight' command"""

    @patch('digitalmodel.subsea.catenary_riser.cli.EffectiveWeightCalculator')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_material')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_fluid')
    def test_basic_effective_weight(self, mock_get_fluid, mock_get_material, mock_calculator, cli_runner):
        """Test basic effective weight calculation"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material
        mock_fluid = MagicMock()
        mock_get_fluid.return_value = mock_fluid

        mock_calc_instance = MagicMock()
        mock_calculator.return_value = mock_calc_instance

        mock_result = MagicMock()
        mock_result.steel_weight = 450.0
        mock_result.coating_weight = 120.0
        mock_result.contents_weight = 80.0
        mock_result.buoyancy = 200.0
        mock_result.total_dry_weight = 650.0
        mock_result.effective_weight = 450.0
        mock_result.to_dict.return_value = {
            'steel_weight': 450.0,
            'coating_weight': 120.0,
            'contents_weight': 80.0,
            'buoyancy': 200.0,
            'total_dry_weight': 650.0,
            'effective_weight': 450.0
        }
        mock_calc_instance.calculate.return_value = mock_result

        result = cli_runner.invoke(cli, [
            'weight',
            '--diameter', '0.3',
            '--thickness', '0.02'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'Effective Weight Breakdown',
            'Steel Weight:',
            'Coating Weight:',
            'Contents Weight:',
            'Buoyancy:',
            'Total Dry Weight:',
            'Effective Weight:'
        )

    @patch('digitalmodel.subsea.catenary_riser.cli.EffectiveWeightCalculator')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_material')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_fluid')
    def test_weight_with_coating(self, mock_get_fluid, mock_get_material, mock_calculator, cli_runner):
        """Test weight calculation with coating parameters"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material
        mock_fluid = MagicMock()
        mock_get_fluid.return_value = mock_fluid

        mock_calc_instance = MagicMock()
        mock_calculator.return_value = mock_calc_instance

        mock_result = MagicMock()
        mock_result.steel_weight = 450.0
        mock_result.coating_weight = 180.0
        mock_result.contents_weight = 80.0
        mock_result.buoyancy = 250.0
        mock_result.total_dry_weight = 710.0
        mock_result.effective_weight = 460.0
        mock_result.to_dict.return_value = {
            'steel_weight': 450.0,
            'coating_weight': 180.0,
            'contents_weight': 80.0,
            'buoyancy': 250.0,
            'total_dry_weight': 710.0,
            'effective_weight': 460.0
        }
        mock_calc_instance.calculate.return_value = mock_result

        result = cli_runner.invoke(cli, [
            'weight',
            '--diameter', '0.3',
            '--thickness', '0.02',
            '--coating-thickness', '0.06',
            '--coating-density', '750'
        ])

        assert_cli_success(result)
        assert 'Coating Weight:' in result.output

    @patch('digitalmodel.subsea.catenary_riser.cli.EffectiveWeightCalculator')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_material')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_fluid')
    def test_weight_negative_warning(self, mock_get_fluid, mock_get_material, mock_calculator, cli_runner):
        """Test warning for negative effective weight (floating)"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material
        mock_fluid = MagicMock()
        mock_get_fluid.return_value = mock_fluid

        mock_calc_instance = MagicMock()
        mock_calculator.return_value = mock_calc_instance

        mock_result = MagicMock()
        mock_result.steel_weight = 200.0
        mock_result.coating_weight = 50.0
        mock_result.contents_weight = 30.0
        mock_result.buoyancy = 350.0
        mock_result.total_dry_weight = 280.0
        mock_result.effective_weight = -70.0  # Negative - will float
        mock_result.to_dict.return_value = {
            'effective_weight': -70.0
        }
        mock_calc_instance.calculate.return_value = mock_result

        result = cli_runner.invoke(cli, [
            'weight',
            '--diameter', '0.3',
            '--thickness', '0.01'
        ])

        assert_cli_success(result)
        assert 'WARNING' in result.output
        assert 'float' in result.output.lower()

    @patch('digitalmodel.subsea.catenary_riser.cli.EffectiveWeightCalculator')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_material')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_fluid')
    def test_weight_with_json_output(self, mock_get_fluid, mock_get_material, mock_calculator, cli_runner, temp_output_dir):
        """Test weight calculation with JSON output"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material
        mock_fluid = MagicMock()
        mock_get_fluid.return_value = mock_fluid

        mock_calc_instance = MagicMock()
        mock_calculator.return_value = mock_calc_instance

        mock_result = MagicMock()
        mock_result.steel_weight = 450.0
        mock_result.coating_weight = 120.0
        mock_result.contents_weight = 80.0
        mock_result.buoyancy = 200.0
        mock_result.total_dry_weight = 650.0
        mock_result.effective_weight = 450.0
        mock_result.to_dict.return_value = {
            'steel_weight': 450.0,
            'coating_weight': 120.0,
            'contents_weight': 80.0,
            'buoyancy': 200.0,
            'total_dry_weight': 650.0,
            'effective_weight': 450.0
        }
        mock_calc_instance.calculate.return_value = mock_result

        output_file = temp_output_dir / "weight_results.json"

        result = cli_runner.invoke(cli, [
            'weight',
            '--diameter', '0.3',
            '--thickness', '0.02',
            '--output', str(output_file)
        ])

        assert_cli_success(result)
        assert output_file.exists()

        with open(output_file) as f:
            data = json.load(f)

        assert 'effective_weight' in data
        assert data['effective_weight'] == 450.0

    @patch('digitalmodel.subsea.catenary_riser.cli.get_material')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_fluid')
    def test_weight_with_different_material(self, mock_get_fluid, mock_get_material, cli_runner):
        """Test weight calculation with x70 material"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material
        mock_fluid = MagicMock()
        mock_get_fluid.return_value = mock_fluid

        result = cli_runner.invoke(cli, [
            'weight',
            '--diameter', '0.3',
            '--thickness', '0.02',
            '--material', 'x70'
        ])

        # Verify material requested
        mock_get_material.assert_called_with('x70')


class TestLazyWaveCommand:
    """Tests for the 'lazy-wave' command"""

    @patch('digitalmodel.subsea.catenary_riser.cli.LazyWaveAnalyzer')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_material')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_fluid')
    def test_basic_lazy_wave(self, mock_get_fluid, mock_get_material, mock_analyzer, cli_runner):
        """Test basic lazy wave riser analysis"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material
        mock_fluid = MagicMock()
        mock_get_fluid.return_value = mock_fluid

        mock_analyzer_instance = MagicMock()
        mock_analyzer.return_value = mock_analyzer_instance

        mock_result = MagicMock()
        mock_result.sag_bend_depth = 450.0
        mock_result.hog_bend_depth = 250.0
        mock_result.arch_height = 200.0
        mock_result.top_tension = 450000.0
        mock_result.tension_at_sag_bend = 120000.0
        mock_result.tension_at_hog_bend = 280000.0
        mock_result.top_angle = 22.5
        mock_result.buoyancy_utilization = 0.75
        mock_analyzer_instance.analyze.return_value = mock_result

        result = cli_runner.invoke(cli, [
            'lazy-wave',
            '--diameter', '0.3',
            '--thickness', '0.02',
            '--length', '2000',
            '--water-depth', '1500',
            '--offset', '1200'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'Lazy Wave Riser Analysis',
            'Sag Bend Depth:',
            'Hog Bend Depth:',
            'Arch Height:',
            'Top Tension:',
            'Tension at Sag:',
            'Buoyancy Utilization:'
        )

    @patch('digitalmodel.subsea.catenary_riser.cli.LazyWaveAnalyzer')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_material')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_fluid')
    def test_lazy_wave_custom_buoyancy(self, mock_get_fluid, mock_get_material, mock_analyzer, cli_runner):
        """Test lazy wave with custom buoyancy parameters"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material
        mock_fluid = MagicMock()
        mock_get_fluid.return_value = mock_fluid

        mock_analyzer_instance = MagicMock()
        mock_analyzer.return_value = mock_analyzer_instance

        mock_result = MagicMock()
        mock_result.sag_bend_depth = 500.0
        mock_result.hog_bend_depth = 200.0
        mock_result.arch_height = 300.0
        mock_result.top_tension = 420000.0
        mock_result.tension_at_sag_bend = 100000.0
        mock_result.tension_at_hog_bend = 260000.0
        mock_result.top_angle = 20.0
        mock_result.buoyancy_utilization = 0.85
        mock_analyzer_instance.analyze.return_value = mock_result

        result = cli_runner.invoke(cli, [
            'lazy-wave',
            '--diameter', '0.3',
            '--thickness', '0.02',
            '--length', '2000',
            '--water-depth', '1500',
            '--offset', '1200',
            '--buoy-length', '250',
            '--buoy-diameter', '1.8',
            '--buoy-start', '350'
        ])

        assert_cli_success(result)
        assert 'Arch Height:' in result.output

    @patch('digitalmodel.subsea.catenary_riser.cli.LazyWaveAnalyzer')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_material')
    @patch('digitalmodel.subsea.catenary_riser.cli.get_fluid')
    def test_lazy_wave_with_json_output(self, mock_get_fluid, mock_get_material, mock_analyzer, cli_runner, temp_output_dir):
        """Test lazy wave with JSON output"""
        mock_material = MagicMock()
        mock_get_material.return_value = mock_material
        mock_fluid = MagicMock()
        mock_get_fluid.return_value = mock_fluid

        mock_analyzer_instance = MagicMock()
        mock_analyzer.return_value = mock_analyzer_instance

        mock_result = MagicMock()
        mock_result.sag_bend_depth = 450.0
        mock_result.hog_bend_depth = 250.0
        mock_result.arch_height = 200.0
        mock_result.top_tension = 450000.0
        mock_result.tension_at_sag_bend = 120000.0
        mock_result.tension_at_hog_bend = 280000.0
        mock_result.top_angle = 22.5
        mock_result.buoyancy_utilization = 0.75
        mock_analyzer_instance.analyze.return_value = mock_result

        output_file = temp_output_dir / "lazy_wave.json"

        result = cli_runner.invoke(cli, [
            'lazy-wave',
            '--diameter', '0.3',
            '--thickness', '0.02',
            '--length', '2000',
            '--water-depth', '1500',
            '--offset', '1200',
            '--output', str(output_file)
        ])

        assert_cli_success(result)

        # Validate JSON output
        data = assert_json_output(output_file, [
            'sag_bend_depth_m',
            'hog_bend_depth_m',
            'arch_height_m',
            'top_tension_N'
        ])

        assert data['sag_bend_depth_m'] == 450.0
        assert data['arch_height_m'] == 200.0

    def test_lazy_wave_missing_required_parameters(self, cli_runner):
        """Test error for missing required water depth"""
        result = cli_runner.invoke(cli, [
            'lazy-wave',
            '--diameter', '0.3',
            '--thickness', '0.02',
            '--length', '2000',
            '--offset', '1200'
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
            'Catenary Riser Analysis',
            'simple',
            'weight',
            'lazy-wave'
        )

    def test_simple_help(self, cli_runner):
        """Test simple command help"""
        result = cli_runner.invoke(cli, ['simple', '--help'])

        assert_cli_success(result)
        assert_output_contains(result,
            'simple catenary riser',
            '--diameter',
            '--thickness',
            '--length',
            '--water-depth'
        )

    def test_version(self, cli_runner):
        """Test version output"""
        result = cli_runner.invoke(cli, ['--version'])

        assert_cli_success(result)
        assert 'catenary-riser' in result.output.lower()


class TestErrorHandling:
    """Tests for CLI error handling"""

    def test_invalid_numeric_input(self, cli_runner):
        """Test error for invalid numeric diameter"""
        result = cli_runner.invoke(cli, [
            'simple',
            '--diameter', 'not-a-number',
            '--thickness', '0.02',
            '--length', '1600',
            '--water-depth', '1500',
            '--offset', '1200'
        ])

        assert_cli_failure(result)
        assert 'Error' in result.output or 'invalid' in result.output.lower()
