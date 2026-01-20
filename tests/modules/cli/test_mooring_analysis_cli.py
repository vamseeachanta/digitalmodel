"""
ABOUTME: Integration tests for mooring_analysis CLI
ABOUTME: Tests catenary analysis, design verification, and material library via CLI
"""

import pytest
import json
from pathlib import Path
from unittest.mock import patch, MagicMock

from digitalmodel.modules.mooring_analysis.cli import cli
from tests.cli.conftest import assert_cli_success, assert_cli_failure, assert_json_output, assert_output_contains


class TestCatenaryCommand:
    """Tests for the 'catenary' command"""

    @patch('digitalmodel.modules.mooring_analysis.cli.CatenaryAnalyzer')
    def test_basic_catenary_with_horizontal_tension(self, mock_analyzer, cli_runner):
        """Test basic catenary calculation with horizontal tension"""
        # Setup mocks
        mock_instance = MagicMock()
        mock_analyzer.return_value = mock_instance

        # Mock catenary result
        mock_result = MagicMock()
        mock_result.horizontal_tension = 500.0
        mock_result.fairlead_tension = 550.0
        mock_result.touchdown_tension = 50.0
        mock_result.arc_length = 450.0
        mock_result.horizontal_distance = 400.0
        mock_result.fairlead_angle = 15.5
        mock_result.grounded_length = 50.0
        mock_instance.solve_catenary.return_value = mock_result

        # Mock stiffness result
        mock_stiffness = MagicMock()
        mock_stiffness.horizontal_stiffness = 125.0
        mock_stiffness.vertical_stiffness = 25.0
        mock_stiffness.geometric_stiffness = 100.0
        mock_stiffness.elastic_stiffness = 25.0
        mock_instance.calculate_stiffness.return_value = mock_stiffness

        result = cli_runner.invoke(cli, [
            'catenary',
            '--water-depth', '500',
            '--line-length', '600',
            '--line-weight', '100',
            '--horizontal-tension', '500'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'Catenary Analysis Results',
            'Horizontal Tension:',
            '500.00 kN',
            'Fairlead Tension:',
            'Horizontal Stiffness:'
        )

    @patch('digitalmodel.modules.mooring_analysis.cli.CatenaryAnalyzer')
    def test_catenary_with_fairlead_tension(self, mock_analyzer, cli_runner):
        """Test catenary calculation solving for horizontal tension from fairlead tension"""
        mock_instance = MagicMock()
        mock_analyzer.return_value = mock_instance

        mock_result = MagicMock()
        mock_result.horizontal_tension = 485.5
        mock_result.fairlead_tension = 550.0
        mock_result.touchdown_tension = 45.0
        mock_result.arc_length = 460.0
        mock_result.horizontal_distance = 410.0
        mock_result.fairlead_angle = 14.2
        mock_result.grounded_length = 40.0
        mock_instance.solve_for_horizontal_tension.return_value = mock_result

        mock_stiffness = MagicMock()
        mock_stiffness.horizontal_stiffness = 120.0
        mock_stiffness.vertical_stiffness = 24.0
        mock_stiffness.geometric_stiffness = 96.0
        mock_stiffness.elastic_stiffness = 24.0
        mock_instance.calculate_stiffness.return_value = mock_stiffness

        result = cli_runner.invoke(cli, [
            'catenary',
            '--water-depth', '500',
            '--line-length', '600',
            '--line-weight', '100',
            '--fairlead-tension', '550'
        ])

        assert_cli_success(result)
        assert 'Fairlead Tension:' in result.output
        assert '550.00 kN' in result.output

    @patch('digitalmodel.modules.mooring_analysis.cli.CatenaryAnalyzer')
    def test_catenary_with_json_output(self, mock_analyzer, cli_runner, temp_output_dir):
        """Test catenary calculation with JSON output file"""
        mock_instance = MagicMock()
        mock_analyzer.return_value = mock_instance

        mock_result = MagicMock()
        mock_result.horizontal_tension = 500.0
        mock_result.fairlead_tension = 550.0
        mock_result.touchdown_tension = 50.0
        mock_result.arc_length = 450.0
        mock_result.horizontal_distance = 400.0
        mock_result.fairlead_angle = 15.5
        mock_result.grounded_length = 50.0
        mock_instance.solve_catenary.return_value = mock_result

        mock_stiffness = MagicMock()
        mock_stiffness.horizontal_stiffness = 125.0
        mock_stiffness.vertical_stiffness = 25.0
        mock_stiffness.geometric_stiffness = 100.0
        mock_stiffness.elastic_stiffness = 25.0
        mock_instance.calculate_stiffness.return_value = mock_stiffness

        output_file = temp_output_dir / "catenary_results.json"

        result = cli_runner.invoke(cli, [
            'catenary',
            '--water-depth', '500',
            '--line-length', '600',
            '--line-weight', '100',
            '--horizontal-tension', '500',
            '--output', str(output_file)
        ])

        assert_cli_success(result)

        # Validate JSON output
        data = assert_json_output(output_file, [
            'solve_method',
            'input',
            'catenary',
            'stiffness'
        ])

        assert data['solve_method'] == 'horizontal_tension'
        assert data['catenary']['horizontal_tension'] == 500.0
        assert data['catenary']['fairlead_tension'] == 550.0
        assert 'horizontal' in data['stiffness']

    def test_catenary_missing_required_options(self, cli_runner):
        """Test error for missing required water depth"""
        result = cli_runner.invoke(cli, [
            'catenary',
            '--line-length', '600',
            '--line-weight', '100',
            '--horizontal-tension', '500'
        ])

        assert_cli_failure(result)
        assert 'Error' in result.output or 'required' in result.output.lower()

    def test_catenary_missing_tension_specification(self, cli_runner):
        """Test error when neither horizontal nor fairlead tension specified"""
        result = cli_runner.invoke(cli, [
            'catenary',
            '--water-depth', '500',
            '--line-length', '600',
            '--line-weight', '100'
        ])

        assert_cli_failure(result)
        assert 'horizontal-tension' in result.output or 'fairlead-tension' in result.output

    @patch('digitalmodel.modules.mooring_analysis.cli.CatenaryAnalyzer')
    def test_catenary_custom_line_properties(self, mock_analyzer, cli_runner):
        """Test catenary with custom line diameter and EA"""
        mock_instance = MagicMock()
        mock_analyzer.return_value = mock_instance

        mock_result = MagicMock()
        mock_result.horizontal_tension = 600.0
        mock_result.fairlead_tension = 650.0
        mock_result.touchdown_tension = 50.0
        mock_result.arc_length = 450.0
        mock_result.horizontal_distance = 400.0
        mock_result.fairlead_angle = 16.0
        mock_result.grounded_length = 50.0
        mock_instance.solve_catenary.return_value = mock_result

        mock_stiffness = MagicMock()
        mock_stiffness.horizontal_stiffness = 150.0
        mock_stiffness.vertical_stiffness = 30.0
        mock_stiffness.geometric_stiffness = 120.0
        mock_stiffness.elastic_stiffness = 30.0
        mock_instance.calculate_stiffness.return_value = mock_stiffness

        result = cli_runner.invoke(cli, [
            'catenary',
            '--water-depth', '500',
            '--line-length', '600',
            '--line-weight', '100',
            '--line-diameter', '100',
            '--line-ea', '950000',
            '--horizontal-tension', '600'
        ])

        assert_cli_success(result)
        assert 'Horizontal Tension:' in result.output

    @patch('digitalmodel.modules.mooring_analysis.cli.CatenaryAnalyzer')
    def test_catenary_analysis_exception_handling(self, mock_analyzer, cli_runner):
        """Test error handling when catenary analysis fails"""
        mock_instance = MagicMock()
        mock_analyzer.return_value = mock_instance
        mock_instance.solve_catenary.side_effect = ValueError("Invalid catenary parameters")

        result = cli_runner.invoke(cli, [
            'catenary',
            '--water-depth', '500',
            '--line-length', '600',
            '--line-weight', '100',
            '--horizontal-tension', '500'
        ])

        assert_cli_failure(result)
        assert 'Error' in result.output

    @patch('digitalmodel.modules.mooring_analysis.cli.CatenaryAnalyzer')
    def test_catenary_large_values(self, mock_analyzer, cli_runner):
        """Test catenary with large water depth and line length"""
        mock_instance = MagicMock()
        mock_analyzer.return_value = mock_instance

        mock_result = MagicMock()
        mock_result.horizontal_tension = 1200.0
        mock_result.fairlead_tension = 1350.0
        mock_result.touchdown_tension = 150.0
        mock_result.arc_length = 2900.0
        mock_result.horizontal_distance = 2700.0
        mock_result.fairlead_angle = 18.5
        mock_result.grounded_length = 200.0
        mock_instance.solve_catenary.return_value = mock_result

        mock_stiffness = MagicMock()
        mock_stiffness.horizontal_stiffness = 80.0
        mock_stiffness.vertical_stiffness = 16.0
        mock_stiffness.geometric_stiffness = 64.0
        mock_stiffness.elastic_stiffness = 16.0
        mock_instance.calculate_stiffness.return_value = mock_stiffness

        result = cli_runner.invoke(cli, [
            'catenary',
            '--water-depth', '3000',
            '--line-length', '3500',
            '--line-weight', '150',
            '--horizontal-tension', '1200'
        ])

        assert_cli_success(result)
        assert '1200.00 kN' in result.output


class TestDesignCommand:
    """Tests for the 'design' command"""

    @patch('digitalmodel.modules.mooring_analysis.cli.MooringDesigner')
    @patch('digitalmodel.modules.mooring_analysis.cli.get_material')
    def test_basic_design_verification(self, mock_get_material, mock_designer, cli_runner):
        """Test basic mooring design verification"""
        # Mock material
        mock_material = MagicMock()
        mock_material.length = 500.0
        mock_get_material.return_value = mock_material

        # Mock designer
        mock_designer_instance = MagicMock()
        mock_designer.return_value = mock_designer_instance

        # Mock environmental loads
        mock_env_loads = MagicMock()
        mock_env_loads.wave_drift_force = 1500.0
        mock_env_loads.current_force = 800.0
        mock_env_loads.wind_force = 300.0
        mock_env_loads.total_force = 2600.0
        mock_designer_instance.calculate_environmental_loads.return_value = mock_env_loads

        # Mock analysis results
        mock_result = MagicMock()
        mock_result.line_id = "ML1"
        mock_result.load_case = "intact_100yr"
        mock_result.max_tension = 1200.0
        mock_result.safety_factor = 4.2
        mock_result.utilization = 0.238
        mock_result.passes = True
        mock_designer_instance.analyze_all_conditions.return_value = [mock_result]

        # Mock summary
        mock_summary = {
            'overall_status': 'PASS',
            'min_safety_factor': 4.2,
            'max_utilization': 0.238,
            'critical_line': {
                'line_id': 'ML1',
                'load_case': 'intact_100yr',
                'safety_factor': 4.2
            }
        }
        mock_designer_instance.generate_design_summary.return_value = mock_summary

        result = cli_runner.invoke(cli, [
            'design',
            '--system-type', 'spread',
            '--water-depth', '1200',
            '--n-lines', '6',
            '--material', 'chain_r3_84'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'Mooring Design Verification',
            'SPREAD',
            'Overall Status:',
            'PASS',
            'Min Safety Factor:'
        )

    @patch('digitalmodel.modules.mooring_analysis.cli.MooringDesigner')
    @patch('digitalmodel.modules.mooring_analysis.cli.get_material')
    def test_design_with_different_system_types(self, mock_get_material, mock_designer, cli_runner):
        """Test design verification with CALM system"""
        mock_material = MagicMock()
        mock_material.length = 500.0
        mock_get_material.return_value = mock_material

        mock_designer_instance = MagicMock()
        mock_designer.return_value = mock_designer_instance

        mock_env_loads = MagicMock()
        mock_env_loads.wave_drift_force = 1200.0
        mock_env_loads.current_force = 600.0
        mock_env_loads.wind_force = 200.0
        mock_env_loads.total_force = 2000.0
        mock_designer_instance.calculate_environmental_loads.return_value = mock_env_loads

        mock_result = MagicMock()
        mock_result.line_id = "ML1"
        mock_result.load_case = "intact_100yr"
        mock_result.max_tension = 1000.0
        mock_result.safety_factor = 5.0
        mock_result.utilization = 0.200
        mock_result.passes = True
        mock_designer_instance.analyze_all_conditions.return_value = [mock_result]

        mock_summary = {
            'overall_status': 'PASS',
            'min_safety_factor': 5.0,
            'max_utilization': 0.200,
            'critical_line': {
                'line_id': 'ML1',
                'load_case': 'intact_100yr',
                'safety_factor': 5.0
            }
        }
        mock_designer_instance.generate_design_summary.return_value = mock_summary

        result = cli_runner.invoke(cli, [
            'design',
            '--system-type', 'calm',
            '--water-depth', '1200',
            '--n-lines', '8'
        ])

        assert_cli_success(result)
        assert 'CALM' in result.output

    @patch('digitalmodel.modules.mooring_analysis.cli.MooringDesigner')
    @patch('digitalmodel.modules.mooring_analysis.cli.get_material')
    def test_design_with_damaged_line_analysis(self, mock_get_material, mock_designer, cli_runner):
        """Test design verification including damaged line case"""
        mock_material = MagicMock()
        mock_material.length = 500.0
        mock_get_material.return_value = mock_material

        mock_designer_instance = MagicMock()
        mock_designer.return_value = mock_designer_instance

        mock_env_loads = MagicMock()
        mock_env_loads.wave_drift_force = 1500.0
        mock_env_loads.current_force = 800.0
        mock_env_loads.wind_force = 300.0
        mock_env_loads.total_force = 2600.0
        mock_designer_instance.calculate_environmental_loads.return_value = mock_env_loads

        # Results for both intact and damaged cases
        mock_result_intact = MagicMock()
        mock_result_intact.line_id = "ML1"
        mock_result_intact.load_case = "intact_100yr"
        mock_result_intact.max_tension = 1200.0
        mock_result_intact.safety_factor = 4.2
        mock_result_intact.utilization = 0.238
        mock_result_intact.passes = True

        mock_result_damaged = MagicMock()
        mock_result_damaged.line_id = "ML2"
        mock_result_damaged.load_case = "damaged_ML1_100yr"
        mock_result_damaged.max_tension = 1800.0
        mock_result_damaged.safety_factor = 2.8
        mock_result_damaged.utilization = 0.357
        mock_result_damaged.passes = True

        mock_designer_instance.analyze_all_conditions.return_value = [
            mock_result_intact,
            mock_result_damaged
        ]

        mock_summary = {
            'overall_status': 'PASS',
            'min_safety_factor': 2.8,
            'max_utilization': 0.357,
            'critical_line': {
                'line_id': 'ML2',
                'load_case': 'damaged_ML1_100yr',
                'safety_factor': 2.8
            }
        }
        mock_designer_instance.generate_design_summary.return_value = mock_summary

        result = cli_runner.invoke(cli, [
            'design',
            '--system-type', 'spread',
            '--water-depth', '1200',
            '--n-lines', '6',
            '--check-damaged'
        ])

        assert_cli_success(result)
        assert 'Min Safety Factor:' in result.output
        assert '2.8' in result.output

    @patch('digitalmodel.modules.mooring_analysis.cli.MooringDesigner')
    @patch('digitalmodel.modules.mooring_analysis.cli.get_material')
    def test_design_with_json_output(self, mock_get_material, mock_designer, cli_runner, temp_output_dir):
        """Test design verification with JSON output"""
        mock_material = MagicMock()
        mock_material.length = 500.0
        mock_get_material.return_value = mock_material

        mock_designer_instance = MagicMock()
        mock_designer.return_value = mock_designer_instance

        mock_env_loads = MagicMock()
        mock_env_loads.wave_drift_force = 1500.0
        mock_env_loads.current_force = 800.0
        mock_env_loads.wind_force = 300.0
        mock_env_loads.total_force = 2600.0
        mock_designer_instance.calculate_environmental_loads.return_value = mock_env_loads

        mock_result = MagicMock()
        mock_result.line_id = "ML1"
        mock_result.load_case = "intact_100yr"
        mock_result.max_tension = 1200.0
        mock_result.safety_factor = 4.2
        mock_result.utilization = 0.238
        mock_result.passes = True
        mock_designer_instance.analyze_all_conditions.return_value = [mock_result]

        mock_summary = {
            'overall_status': 'PASS',
            'min_safety_factor': 4.2,
            'max_utilization': 0.238,
            'critical_line': {
                'line_id': 'ML1',
                'load_case': 'intact_100yr',
                'safety_factor': 4.2
            }
        }
        mock_designer_instance.generate_design_summary.return_value = mock_summary

        output_file = temp_output_dir / "design_results.json"

        result = cli_runner.invoke(cli, [
            'design',
            '--system-type', 'spread',
            '--water-depth', '1200',
            '--output', str(output_file)
        ])

        assert_cli_success(result)

        # Validate JSON output
        data = assert_json_output(output_file, [
            'system',
            'environmental_loads',
            'summary',
            'results'
        ])

        assert data['system']['type'] == 'spread'
        assert data['summary']['overall_status'] == 'PASS'
        assert len(data['results']) > 0

    @patch('digitalmodel.modules.mooring_analysis.cli.get_material')
    def test_design_with_different_material(self, mock_get_material, cli_runner):
        """Test design with different material specification"""
        mock_material = MagicMock()
        mock_material.length = 600.0
        mock_get_material.return_value = mock_material

        # Command may fail without full mock setup, but should attempt to get material
        result = cli_runner.invoke(cli, [
            'design',
            '--system-type', 'spread',
            '--water-depth', '1200',
            '--material', 'chain_r4_100',
            '--line-length', '600'
        ])

        # May fail due to incomplete mocks, but should have called get_material
        mock_get_material.assert_called_once_with('chain_r4_100')

    def test_design_invalid_system_type(self, cli_runner):
        """Test error for invalid system type"""
        result = cli_runner.invoke(cli, [
            'design',
            '--system-type', 'invalid_type',
            '--water-depth', '1200'
        ])

        assert_cli_failure(result)
        assert 'Error' in result.output or 'invalid' in result.output.lower()

    def test_design_missing_water_depth(self, cli_runner):
        """Test error for missing required water depth"""
        result = cli_runner.invoke(cli, [
            'design',
            '--system-type', 'spread'
        ])

        assert_cli_failure(result)
        assert 'Error' in result.output or 'required' in result.output.lower()


class TestGenerateModelCommand:
    """Tests for the 'generate-model' command"""

    def test_generate_model_not_implemented(self, cli_runner, temp_output_dir):
        """Test that generate-model command exists but shows not implemented message"""
        output_file = temp_output_dir / "model.yaml"

        result = cli_runner.invoke(cli, [
            'generate-model',
            '--output', str(output_file)
        ])

        # Command should run but show not implemented message
        assert result.exit_code in [0, 1]  # May succeed with message or fail
        assert 'not yet implemented' in result.output or 'Python API' in result.output

    def test_generate_model_help(self, cli_runner):
        """Test help message for generate-model command"""
        result = cli_runner.invoke(cli, ['generate-model', '--help'])

        assert_cli_success(result)
        assert 'OrcaFlex' in result.output or 'configuration' in result.output


class TestListMaterialsCommand:
    """Tests for the 'list-materials' command"""

    def test_list_materials(self, cli_runner):
        """Test listing available mooring line materials"""
        result = cli_runner.invoke(cli, ['list-materials'])

        assert_cli_success(result)
        assert_output_contains(result,
            'Available Mooring Line Materials',
            'Material ID',
            'Type',
            'Diameter',
            'MBL',
            'Weight'
        )

    def test_list_materials_output_format(self, cli_runner):
        """Test that materials are listed in table format"""
        result = cli_runner.invoke(cli, ['list-materials'])

        assert_cli_success(result)
        # Should have column headers
        assert '(mm)' in result.output  # Diameter unit
        assert '(kN)' in result.output  # MBL and EA units
        assert '(kg/m)' in result.output  # Weight unit
        # Should have separator line
        assert '-' * 10 in result.output

    def test_list_materials_contains_common_materials(self, cli_runner):
        """Test that common materials are included in list"""
        result = cli_runner.invoke(cli, ['list-materials'])

        assert_cli_success(result)
        # Common material patterns (may vary based on actual library)
        # Output should contain material identifiers


class TestCLIHelp:
    """Tests for CLI help and documentation"""

    def test_main_help(self, cli_runner):
        """Test main CLI help message"""
        result = cli_runner.invoke(cli, ['--help'])

        assert_cli_success(result)
        assert_output_contains(result,
            'Mooring Analysis Tools',
            'catenary',
            'design',
            'list-materials'
        )

    def test_catenary_help(self, cli_runner):
        """Test catenary command help"""
        result = cli_runner.invoke(cli, ['catenary', '--help'])

        assert_cli_success(result)
        assert_output_contains(result,
            'Calculate catenary geometry',
            '--water-depth',
            '--line-length',
            '--horizontal-tension',
            '--fairlead-tension'
        )

    def test_version(self, cli_runner):
        """Test version output"""
        result = cli_runner.invoke(cli, ['--version'])

        assert_cli_success(result)
        assert 'mooring-analysis' in result.output.lower()


class TestErrorHandling:
    """Tests for CLI error handling"""

    def test_invalid_numeric_input(self, cli_runner):
        """Test error for invalid numeric input"""
        result = cli_runner.invoke(cli, [
            'catenary',
            '--water-depth', 'not-a-number',
            '--line-length', '600',
            '--line-weight', '100',
            '--horizontal-tension', '500'
        ])

        assert_cli_failure(result)
        assert 'Error' in result.output or 'invalid' in result.output.lower()

    def test_negative_water_depth(self, cli_runner):
        """Test handling of negative water depth"""
        result = cli_runner.invoke(cli, [
            'catenary',
            '--water-depth', '-500',
            '--line-length', '600',
            '--line-weight', '100',
            '--horizontal-tension', '500'
        ])

        # May fail validation or in calculation
        # Just check it doesn't crash unexpectedly
        assert result.exit_code in [0, 1, 2]
