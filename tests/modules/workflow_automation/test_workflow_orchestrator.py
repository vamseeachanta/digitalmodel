#!/usr/bin/env python3
"""
ABOUTME: Unit tests for workflow orchestration engine including task execution,
dependency resolution, and result caching.
"""

import pytest
from datetime import datetime
from digitalmodel.modules.workflow_automation.models import (
    WorkflowDefinition,
    WorkflowTask,
    TaskStatus,
)
from digitalmodel.modules.workflow_automation.orchestrator import WorkflowOrchestrator


class TestWorkflowOrchestrator:
    """Test WorkflowOrchestrator execution engine"""

    def test_create_orchestrator(self):
        """Test creating orchestrator"""
        orchestrator = WorkflowOrchestrator()

        assert orchestrator.config is not None
        assert len(orchestrator.task_results) == 0

    def test_prepare_task_inputs_direct_values(self):
        """Test preparing task inputs with direct values"""
        orchestrator = WorkflowOrchestrator()

        task = WorkflowTask(
            name="Test Task",
            task_id="test",
            module="test",
            function="test",
            inputs={
                'diameter': 0.508,
                'thickness': 0.025,
                'length': 1500,
            }
        )

        inputs = orchestrator._prepare_task_inputs(task)

        assert inputs['diameter'] == 0.508
        assert inputs['thickness'] == 0.025
        assert inputs['length'] == 1500

    def test_prepare_task_inputs_with_references(self):
        """Test preparing task inputs with references to previous outputs"""
        orchestrator = WorkflowOrchestrator()

        # Simulate previous task results
        orchestrator.task_results['riser_top_tension'] = 250000.0
        orchestrator.task_results['riser_natural_frequency'] = 0.5

        task = WorkflowTask(
            name="Test Task",
            task_id="test",
            module="test",
            function="test",
            inputs={
                'diameter': 0.508,
                'tension': '$riser_top_tension',
                'frequency': '$riser_natural_frequency',
            }
        )

        inputs = orchestrator._prepare_task_inputs(task)

        assert inputs['diameter'] == 0.508
        assert inputs['tension'] == 250000.0
        assert inputs['frequency'] == 0.5

    def test_prepare_task_inputs_undefined_reference(self):
        """Test error handling for undefined references"""
        orchestrator = WorkflowOrchestrator()

        task = WorkflowTask(
            name="Test Task",
            task_id="test",
            module="test",
            function="test",
            inputs={
                'value': '$undefined_reference',
            }
        )

        with pytest.raises(ValueError, match="Undefined reference"):
            orchestrator._prepare_task_inputs(task)

    def test_extract_output_from_dict(self):
        """Test extracting output from dictionary result"""
        orchestrator = WorkflowOrchestrator()

        result = {
            'top_tension': 250000.0,
            'effective_weight': 150.0,
            'touchdown_point': 500.0,
        }

        tension = orchestrator._extract_output(result, 'top_tension')
        weight = orchestrator._extract_output(result, 'effective_weight')

        assert tension == 250000.0
        assert weight == 150.0

    def test_extract_output_from_object(self):
        """Test extracting output from object with attributes"""
        orchestrator = WorkflowOrchestrator()

        # Create a simple object with attributes
        class Result:
            def __init__(self):
                self.top_tension = 250000.0
                self.effective_weight = 150.0

        result = Result()

        tension = orchestrator._extract_output(result, 'top_tension')
        weight = orchestrator._extract_output(result, 'effective_weight')

        assert tension == 250000.0
        assert weight == 150.0

    def test_execute_workflow_circular_dependency(self):
        """Test error handling for circular dependencies"""
        orchestrator = WorkflowOrchestrator()

        workflow = WorkflowDefinition(
            name="Test Workflow",
            description="Test",
        )

        # Create circular dependency
        task_a = WorkflowTask(
            name="Task A",
            task_id="task_a",
            module="test",
            function="test",
            depends_on=['task_b'],
        )

        task_b = WorkflowTask(
            name="Task B",
            task_id="task_b",
            module="test",
            function="test",
            depends_on=['task_a'],
        )

        workflow.add_task(task_a)
        workflow.add_task(task_b)

        result = orchestrator.execute_workflow(workflow)

        assert result.status == "failed"
        assert "Circular dependency" in result.task_errors.get('workflow', '')

    def test_create_workflow_result(self):
        """Test creating workflow result summary"""
        orchestrator = WorkflowOrchestrator()

        workflow = WorkflowDefinition(
            name="Test Workflow",
            description="Test",
        )

        task1 = WorkflowTask(
            name="Task 1",
            task_id="task1",
            module="test",
            function="test",
        )
        task1.status = TaskStatus.COMPLETED
        task1.result = {'output': 100.0}

        task2 = WorkflowTask(
            name="Task 2",
            task_id="task2",
            module="test",
            function="test",
        )
        task2.status = TaskStatus.FAILED
        task2.error_message = "Test error"

        workflow.add_task(task1)
        workflow.add_task(task2)

        start_time = datetime(2024, 1, 1, 12, 0, 0)
        end_time = datetime(2024, 1, 1, 12, 0, 30)

        result = orchestrator._create_workflow_result(workflow, start_time, end_time)

        assert result.status == "partial"  # Some completed, some failed
        assert result.duration_seconds == 30.0
        assert result.success_rate == 50.0
        assert len(result.get_failed_tasks()) == 1
        assert 'task2' in result.get_failed_tasks()

    def test_create_error_result(self):
        """Test creating error result"""
        orchestrator = WorkflowOrchestrator()

        workflow = WorkflowDefinition(
            name="Test Workflow",
            description="Test",
        )

        start_time = datetime(2024, 1, 1, 12, 0, 0)
        error_msg = "Workflow validation failed"

        result = orchestrator._create_error_result(workflow, start_time, error_msg)

        assert result.status == "failed"
        assert result.workflow_name == "Test Workflow"
        assert error_msg in result.task_errors.get('workflow', '')

    def test_save_results(self, tmp_path):
        """Test saving workflow results to JSON file"""
        orchestrator = WorkflowOrchestrator()

        result = orchestrator._create_error_result(
            WorkflowDefinition(name="Test", description="Test"),
            datetime.now(),
            "Test error"
        )

        output_file = tmp_path / "results" / "test_result.json"
        orchestrator.save_results(result, str(output_file))

        assert output_file.exists()

        # Verify file can be read
        import json
        with open(output_file, 'r') as f:
            data = json.load(f)

        assert data['workflow_name'] == "Test"
        assert data['status'] == "failed"


class TestWorkflowIntegration:
    """Integration tests for workflow execution"""

    def test_simple_workflow_execution(self):
        """Test executing a simple workflow with mock functions"""
        # This test would require creating mock modules/functions
        # For now, we test the orchestration logic
        orchestrator = WorkflowOrchestrator()

        workflow = WorkflowDefinition(
            name="Simple Test Workflow",
            description="Test workflow with dependencies",
        )

        # Create simple task graph: A -> B -> C
        task_a = WorkflowTask(
            name="Task A",
            task_id="task_a",
            module="test",
            function="test",
            inputs={'value': 10},
        )

        task_b = WorkflowTask(
            name="Task B",
            task_id="task_b",
            module="test",
            function="test",
            inputs={'value': '$result_a'},
            depends_on=['task_a'],
        )

        task_c = WorkflowTask(
            name="Task C",
            task_id="task_c",
            module="test",
            function="test",
            inputs={'value': '$result_b'},
            depends_on=['task_b'],
        )

        workflow.add_task(task_a)
        workflow.add_task(task_b)
        workflow.add_task(task_c)

        # Verify dependency order
        order = workflow.get_dependency_order()
        assert len(order) == 3
        assert order[0] == ['task_a']
        assert order[1] == ['task_b']
        assert order[2] == ['task_c']

    def test_parallel_workflow_execution(self):
        """Test workflow with parallel tasks"""
        orchestrator = WorkflowOrchestrator()

        workflow = WorkflowDefinition(
            name="Parallel Test Workflow",
            description="Test workflow with parallel execution",
        )

        # Create task graph: A, B (parallel) -> C
        task_a = WorkflowTask(
            name="Task A",
            task_id="task_a",
            module="test",
            function="test",
        )

        task_b = WorkflowTask(
            name="Task B",
            task_id="task_b",
            module="test",
            function="test",
        )

        task_c = WorkflowTask(
            name="Task C",
            task_id="task_c",
            module="test",
            function="test",
            depends_on=['task_a', 'task_b'],
        )

        workflow.add_task(task_a)
        workflow.add_task(task_b)
        workflow.add_task(task_c)

        # Verify dependency order shows parallel execution
        order = workflow.get_dependency_order()
        assert len(order) == 2
        assert set(order[0]) == {'task_a', 'task_b'}  # Parallel execution
        assert order[1] == ['task_c']
