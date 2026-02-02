"""
ABOUTME: Integration tests for workflow_automation CLI
ABOUTME: Tests end-to-end engineering workflow orchestration via CLI
"""

import pytest
import json
from pathlib import Path
from unittest.mock import patch, MagicMock

pytest.importorskip(
    "tests.cli.conftest",
    reason="module tests.cli.conftest not available"
)
from digitalmodel.modules.workflow_automation.cli import cli
from tests.cli.conftest import assert_cli_success, assert_cli_failure, assert_json_output, assert_output_contains


class TestListCommand:
    """Tests for the 'list' command"""

    def test_list_workflows(self, cli_runner):
        """Test listing all available workflows"""
        result = cli_runner.invoke(cli, ['list'])

        assert_cli_success(result)
        assert_output_contains(result,
            'Available Workflows',
            'riser-analysis',
            'Complete Riser Analysis',
            'mooring-design',
            'Mooring System Design',
            'structural-check',
            'Platform Structural Check',
            'vessel-response',
            'Vessel Response Analysis'
        )

    def test_list_shows_usage(self, cli_runner):
        """Test that list shows usage instructions"""
        result = cli_runner.invoke(cli, ['list'])

        assert_cli_success(result)
        assert 'Usage:' in result.output
        assert 'workflow-automation run' in result.output


class TestRiserAnalysisCommand:
    """Tests for the 'riser-analysis' command"""

    @patch('digitalmodel.modules.workflow_automation.cli.generate_workflow_report')
    @patch('digitalmodel.modules.workflow_automation.cli.WorkflowOrchestrator')
    @patch('digitalmodel.modules.workflow_automation.cli.CompleteRiserAnalysisWorkflow')
    def test_basic_riser_analysis(self, mock_workflow_class, mock_orchestrator_class,
                                   mock_report_gen, cli_runner):
        """Test basic riser analysis workflow execution"""
        # Mock workflow creation
        mock_workflow = MagicMock()
        mock_workflow_class.create.return_value = mock_workflow

        # Mock orchestrator
        mock_orchestrator = MagicMock()
        mock_orchestrator_class.return_value = mock_orchestrator

        # Mock workflow result
        mock_result = MagicMock()
        mock_result.status = 'success'
        mock_result.duration_seconds = 125.5
        mock_result.success_rate = 100.0
        # task_statuses should be a dict, not a list - 4 completed + 2 failed = 66.7% success
        mock_result.task_statuses = {
            'geometry_calculation': 'completed',
            'stress_analysis': 'completed',
            'top_tension_calc': 'completed',
            'orcaflex_setup': 'completed',
            'viv_analysis': 'failed',
            'fatigue_calculation': 'failed'
        }
        mock_result.outputs = {
            'riser_top_tension': 750000.0,  # 750 kN
            'viv_risk_status': 'LOW_RISK',
            'riser_fatigue_life_years': 35.2
        }
        mock_result.get_failed_tasks.return_value = []
        mock_orchestrator.execute_workflow.return_value = mock_result

        # Mock report generation
        mock_report_gen.return_value = 'reports/riser_analysis_report.html'

        result = cli_runner.invoke(cli, [
            'riser-analysis',
            '--diameter', '0.5',
            '--thickness', '0.025',
            '--length', '1200',
            '--water-depth', '1000',
            '--offset', '100',
            '--current-speed', '1.5'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'Complete Riser Analysis Workflow',
            'Workflow Complete',
            'SUCCESS',
            '125.5s',
            '100.0%',
            'Top Tension:',
            '750.0 kN',
            'VIV Status:',
            'LOW_RISK',
            'Fatigue Life:',
            '35.2 years',
            'HTML report:'
        )

        # Verify workflow was created with correct parameters
        mock_workflow_class.create.assert_called_once_with(
            diameter=0.5,
            thickness=0.025,
            length=1200.0,
            water_depth=1000.0,
            offset=100.0,
            current_speed=1.5,
            material='x65',
            internal_fluid='oil'
        )

    @patch('digitalmodel.modules.workflow_automation.cli.generate_workflow_report')
    @patch('digitalmodel.modules.workflow_automation.cli.WorkflowOrchestrator')
    @patch('digitalmodel.modules.workflow_automation.cli.CompleteRiserAnalysisWorkflow')
    def test_riser_analysis_with_custom_material(self, mock_workflow_class, mock_orchestrator_class,
                                                   mock_report_gen, cli_runner):
        """Test riser analysis with custom material and fluid"""
        mock_workflow = MagicMock()
        mock_workflow_class.create.return_value = mock_workflow

        mock_orchestrator = MagicMock()
        mock_orchestrator_class.return_value = mock_orchestrator

        mock_result = MagicMock()
        mock_result.status = 'success'
        mock_result.duration_seconds = 98.3
        mock_result.success_rate = 100.0
        mock_result.task_statuses = ['task1']
        mock_result.outputs = {}
        mock_result.get_failed_tasks.return_value = []
        mock_orchestrator.execute_workflow.return_value = mock_result

        mock_report_gen.return_value = 'reports/report.html'

        result = cli_runner.invoke(cli, [
            'riser-analysis',
            '--diameter', '0.4',
            '--thickness', '0.02',
            '--length', '800',
            '--water-depth', '500',
            '--offset', '50',
            '--current-speed', '1.0',
            '--material', 'titanium',
            '--internal-fluid', 'gas'
        ])

        assert_cli_success(result)
        mock_workflow_class.create.assert_called_once_with(
            diameter=0.4,
            thickness=0.02,
            length=800.0,
            water_depth=500.0,
            offset=50.0,
            current_speed=1.0,
            material='titanium',
            internal_fluid='gas'
        )

    @patch('digitalmodel.modules.workflow_automation.cli.generate_workflow_report')
    @patch('digitalmodel.modules.workflow_automation.cli.WorkflowOrchestrator')
    @patch('digitalmodel.modules.workflow_automation.cli.CompleteRiserAnalysisWorkflow')
    def test_riser_analysis_with_json_output(self, mock_workflow_class, mock_orchestrator_class,
                                              mock_report_gen, cli_runner, temp_output_dir):
        """Test riser analysis with JSON output"""
        mock_workflow = MagicMock()
        mock_workflow_class.create.return_value = mock_workflow

        mock_orchestrator = MagicMock()
        mock_orchestrator_class.return_value = mock_orchestrator

        mock_result = MagicMock()
        mock_result.status = 'success'
        mock_result.duration_seconds = 120.0
        mock_result.success_rate = 100.0
        mock_result.task_statuses = ['task1', 'task2']
        mock_result.outputs = {'riser_top_tension': 650000.0}
        mock_result.get_failed_tasks.return_value = []
        mock_orchestrator.execute_workflow.return_value = mock_result

        mock_report_gen.return_value = 'reports/report.html'

        output_file = temp_output_dir / "riser_results.json"

        result = cli_runner.invoke(cli, [
            'riser-analysis',
            '--diameter', '0.5',
            '--thickness', '0.025',
            '--length', '1200',
            '--water-depth', '1000',
            '--offset', '100',
            '--current-speed', '1.5',
            '--output', str(output_file)
        ])

        assert_cli_success(result)
        assert f'Results saved to: {output_file}' in result.output

        # Verify save_results was called
        mock_orchestrator.save_results.assert_called_once_with(mock_result, str(output_file))

    @patch('digitalmodel.modules.workflow_automation.cli.WorkflowOrchestrator')
    @patch('digitalmodel.modules.workflow_automation.cli.CompleteRiserAnalysisWorkflow')
    def test_riser_analysis_with_failed_tasks(self, mock_workflow_class, mock_orchestrator_class,
                                               cli_runner):
        """Test riser analysis workflow with failed tasks"""
        mock_workflow = MagicMock()
        mock_workflow_class.create.return_value = mock_workflow

        mock_orchestrator = MagicMock()
        mock_orchestrator_class.return_value = mock_orchestrator

        mock_result = MagicMock()
        mock_result.workflow_id = 'riser_analysis_test'
        mock_result.status = 'partial'
        mock_result.duration_seconds = 85.2
        mock_result.success_rate = 66.7
        # task_statuses should be a dict, not a list - 4 completed + 2 failed = 66.7% success
        mock_result.task_statuses = {
            'geometry_calculation': 'completed',
            'stress_analysis': 'completed',
            'top_tension_calc': 'completed',
            'orcaflex_setup': 'completed',
            'viv_analysis': 'failed',
            'fatigue_calculation': 'failed'
        }
        mock_result.outputs = {}
        mock_result.get_failed_tasks.return_value = ['viv_analysis', 'fatigue_calculation']
        mock_orchestrator.execute_workflow.return_value = mock_result

        result = cli_runner.invoke(cli, [
            'riser-analysis',
            '--diameter', '0.5',
            '--thickness', '0.025',
            '--length', '1200',
            '--water-depth', '1000',
            '--offset', '100',
            '--current-speed', '1.5'
        ])

        assert_cli_success(result)
        assert 'PARTIAL' in result.output
        assert '66.7%' in result.output
        assert 'Failed Tasks:' in result.output
        assert 'viv_analysis' in result.output
        assert 'fatigue_calculation' in result.output


class TestMooringDesignCommand:
    """Tests for the 'mooring-design' command"""

    @patch('digitalmodel.modules.workflow_automation.cli.generate_workflow_report')
    @patch('digitalmodel.modules.workflow_automation.cli.WorkflowOrchestrator')
    @patch('digitalmodel.modules.workflow_automation.cli.MooringSystemDesignWorkflow')
    def test_basic_mooring_design(self, mock_workflow_class, mock_orchestrator_class,
                                   mock_report_gen, cli_runner):
        """Test basic mooring system design workflow"""
        mock_workflow = MagicMock()
        mock_workflow_class.create.return_value = mock_workflow

        mock_orchestrator = MagicMock()
        mock_orchestrator_class.return_value = mock_orchestrator

        mock_result = MagicMock()
        mock_result.status = 'success'
        mock_result.duration_seconds = 65.3
        mock_result.success_rate = 100.0
        mock_result.task_statuses = ['task1', 'task2']
        mock_result.outputs = {
            'mooring_total_load': 1250000.0,  # 1250 kN
            'mooring_line_tension': 625000.0,  # 625 kN
            'mooring_safety_factor': 2.15
        }
        mock_result.get_failed_tasks.return_value = []
        mock_orchestrator.execute_workflow.return_value = mock_result

        mock_report_gen.return_value = 'reports/mooring_report.html'

        result = cli_runner.invoke(cli, [
            'mooring-design',
            '--vessel-type', 'fpso',
            '--water-depth', '1500',
            '--line-length', '2000'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'Mooring System Design Workflow',
            'FPSO',
            'Depth:    1500.0m',
            'Workflow Complete',
            'SUCCESS',
            'Total Load:',
            '1250.0 kN',
            'Line Tension:',
            '625.0 kN',
            'Safety Factor:',
            '2.15'
        )

    @patch('digitalmodel.modules.workflow_automation.cli.generate_workflow_report')
    @patch('digitalmodel.modules.workflow_automation.cli.WorkflowOrchestrator')
    @patch('digitalmodel.modules.workflow_automation.cli.MooringSystemDesignWorkflow')
    def test_mooring_design_semisubmersible(self, mock_workflow_class, mock_orchestrator_class,
                                             mock_report_gen, cli_runner):
        """Test mooring design for semisubmersible vessel"""
        mock_workflow = MagicMock()
        mock_workflow_class.create.return_value = mock_workflow

        mock_orchestrator = MagicMock()
        mock_orchestrator_class.return_value = mock_orchestrator

        mock_result = MagicMock()
        mock_result.status = 'success'
        mock_result.duration_seconds = 72.1
        mock_result.success_rate = 100.0
        mock_result.task_statuses = ['task1']
        mock_result.outputs = {}
        mock_result.get_failed_tasks.return_value = []
        mock_orchestrator.execute_workflow.return_value = mock_result

        mock_report_gen.return_value = 'reports/report.html'

        result = cli_runner.invoke(cli, [
            'mooring-design',
            '--vessel-type', 'semisubmersible',
            '--water-depth', '2000',
            '--line-length', '2500',
            '--line-diameter', '0.150',
            '--wind-speed', '30.0',
            '--current-speed', '2.0'
        ])

        assert_cli_success(result)
        assert 'SEMISUBMERSIBLE' in result.output

        # Verify workflow creation
        mock_workflow_class.create.assert_called_once_with(
            vessel_type='semisubmersible',
            water_depth=2000.0,
            line_length=2500.0,
            line_diameter=0.150,
            wind_speed=30.0,
            current_speed=2.0
        )

    @patch('digitalmodel.modules.workflow_automation.cli.generate_workflow_report')
    @patch('digitalmodel.modules.workflow_automation.cli.WorkflowOrchestrator')
    @patch('digitalmodel.modules.workflow_automation.cli.MooringSystemDesignWorkflow')
    def test_mooring_design_with_json_output(self, mock_workflow_class, mock_orchestrator_class,
                                              mock_report_gen, cli_runner, temp_output_dir):
        """Test mooring design with JSON output"""
        mock_workflow = MagicMock()
        mock_workflow_class.create.return_value = mock_workflow

        mock_orchestrator = MagicMock()
        mock_orchestrator_class.return_value = mock_orchestrator

        mock_result = MagicMock()
        mock_result.status = 'success'
        mock_result.duration_seconds = 58.9
        mock_result.success_rate = 100.0
        mock_result.task_statuses = []
        mock_result.outputs = {}
        mock_result.get_failed_tasks.return_value = []
        mock_orchestrator.execute_workflow.return_value = mock_result

        mock_report_gen.return_value = 'reports/report.html'

        output_file = temp_output_dir / "mooring_results.json"

        result = cli_runner.invoke(cli, [
            'mooring-design',
            '--vessel-type', 'tanker',
            '--water-depth', '500',
            '--line-length', '800',
            '--output', str(output_file)
        ])

        assert_cli_success(result)
        assert f'Results saved to: {output_file}' in result.output


class TestStructuralCheckCommand:
    """Tests for the 'structural-check' command"""

    @patch('digitalmodel.modules.workflow_automation.cli.generate_workflow_report')
    @patch('digitalmodel.modules.workflow_automation.cli.WorkflowOrchestrator')
    @patch('digitalmodel.modules.workflow_automation.cli.PlatformStructuralWorkflow')
    def test_basic_structural_check(self, mock_workflow_class, mock_orchestrator_class,
                                     mock_report_gen, cli_runner):
        """Test basic platform structural check workflow"""
        mock_workflow = MagicMock()
        mock_workflow_class.create.return_value = mock_workflow

        mock_orchestrator = MagicMock()
        mock_orchestrator_class.return_value = mock_orchestrator

        mock_result = MagicMock()
        mock_result.status = 'success'
        mock_result.duration_seconds = 28.7
        mock_result.success_rate = 100.0
        mock_result.task_statuses = ['task1', 'task2']
        mock_result.outputs = {
            'plate_von_mises_stress': 185.5,
            'plate_stress_utilization': 0.523,
            'structural_status': 'PASS'
        }
        mock_result.get_failed_tasks.return_value = []
        mock_orchestrator.execute_workflow.return_value = mock_result

        mock_report_gen.return_value = 'reports/structural_report.html'

        result = cli_runner.invoke(cli, [
            'structural-check',
            '--plate-length', '5.0',
            '--plate-width', '3.0',
            '--plate-thickness', '0.025'
        ])

        assert_cli_success(result)
        assert_output_contains(result,
            'Platform Structural Check Workflow',
            'Workflow Complete',
            'SUCCESS',
            'Von Mises:',
            '185.5 MPa',
            'Utilization:',
            '52.3%',
            'Status:',
            'PASS'
        )

    @patch('digitalmodel.modules.workflow_automation.cli.generate_workflow_report')
    @patch('digitalmodel.modules.workflow_automation.cli.WorkflowOrchestrator')
    @patch('digitalmodel.modules.workflow_automation.cli.PlatformStructuralWorkflow')
    def test_structural_check_custom_material(self, mock_workflow_class, mock_orchestrator_class,
                                               mock_report_gen, cli_runner):
        """Test structural check with custom material and stress"""
        mock_workflow = MagicMock()
        mock_workflow_class.create.return_value = mock_workflow

        mock_orchestrator = MagicMock()
        mock_orchestrator_class.return_value = mock_orchestrator

        mock_result = MagicMock()
        mock_result.status = 'success'
        mock_result.duration_seconds = 31.2
        mock_result.success_rate = 100.0
        mock_result.task_statuses = []
        mock_result.outputs = {}
        mock_result.get_failed_tasks.return_value = []
        mock_orchestrator.execute_workflow.return_value = mock_result

        mock_report_gen.return_value = 'reports/report.html'

        result = cli_runner.invoke(cli, [
            'structural-check',
            '--plate-length', '4.0',
            '--plate-width', '2.5',
            '--plate-thickness', '0.020',
            '--material', 's420',
            '--applied-stress', '250.0',
            '--load-type', 'axial'
        ])

        assert_cli_success(result)
        assert 'S420' in result.output
        assert '250.0 MPa' in result.output

        # Verify workflow creation
        mock_workflow_class.create.assert_called_once_with(
            load_type='axial',
            plate_length=4.0,
            plate_width=2.5,
            plate_thickness=0.020,
            material='s420',
            applied_stress=250.0
        )

    @patch('digitalmodel.modules.workflow_automation.cli.generate_workflow_report')
    @patch('digitalmodel.modules.workflow_automation.cli.WorkflowOrchestrator')
    @patch('digitalmodel.modules.workflow_automation.cli.PlatformStructuralWorkflow')
    def test_structural_check_with_json_output(self, mock_workflow_class, mock_orchestrator_class,
                                                mock_report_gen, cli_runner, temp_output_dir):
        """Test structural check with JSON output"""
        mock_workflow = MagicMock()
        mock_workflow_class.create.return_value = mock_workflow

        mock_orchestrator = MagicMock()
        mock_orchestrator_class.return_value = mock_orchestrator

        mock_result = MagicMock()
        mock_result.status = 'success'
        mock_result.duration_seconds = 25.4
        mock_result.success_rate = 100.0
        mock_result.task_statuses = []
        mock_result.outputs = {}
        mock_result.get_failed_tasks.return_value = []
        mock_orchestrator.execute_workflow.return_value = mock_result

        mock_report_gen.return_value = 'reports/report.html'

        output_file = temp_output_dir / "structural_results.json"

        result = cli_runner.invoke(cli, [
            'structural-check',
            '--plate-length', '6.0',
            '--plate-width', '4.0',
            '--plate-thickness', '0.030',
            '--output', str(output_file)
        ])

        assert_cli_success(result)
        assert f'Results saved to: {output_file}' in result.output


class TestCLIHelp:
    """Tests for CLI help and documentation"""

    def test_main_help(self, cli_runner):
        """Test main CLI help message"""
        result = cli_runner.invoke(cli, ['--help'])

        assert_cli_success(result)
        assert_output_contains(result,
            'Workflow Automation',
            'list',
            'riser-analysis',
            'mooring-design',
            'structural-check'
        )

    def test_version(self, cli_runner):
        """Test version output"""
        result = cli_runner.invoke(cli, ['--version'])

        assert_cli_success(result)
        assert 'workflow-automation' in result.output.lower()
