#!/usr/bin/env python3
"""
ABOUTME: Unit tests for workflow automation data models including WorkflowTask,
WorkflowDefinition, and dependency ordering.
"""

import pytest
from datetime import datetime
from digitalmodel.modules.workflow_automation.models import (
    WorkflowDefinition,
    WorkflowTask,
    WorkflowResult,
    TaskStatus,
    WorkflowConfig,
)


class TestWorkflowTask:
    """Test WorkflowTask model"""

    def test_create_basic_task(self):
        """Test creating a basic workflow task"""
        task = WorkflowTask(
            name="Test Task",
            task_id="test_task",
            module="test_module",
            function="test_function",
        )

        assert task.name == "Test Task"
        assert task.task_id == "test_task"
        assert task.module == "test_module"
        assert task.function == "test_function"
        assert task.status == TaskStatus.PENDING
        assert task.required is True
        assert task.cache_results is True

    def test_task_with_inputs_outputs(self):
        """Test task with inputs and outputs"""
        task = WorkflowTask(
            name="Catenary Analysis",
            task_id="catenary",
            module="catenary_riser.simple_catenary",
            function="analyze_simple_catenary",
            inputs={
                'diameter': 0.508,
                'thickness': 0.025,
                'length': 1500,
            },
            outputs={
                'top_tension': 'riser_top_tension',
                'effective_weight': 'riser_effective_weight',
            }
        )

        assert task.inputs['diameter'] == 0.508
        assert task.outputs['top_tension'] == 'riser_top_tension'

    def test_task_with_dependencies(self):
        """Test task with dependencies"""
        task = WorkflowTask(
            name="VIV Screening",
            task_id="viv_screen",
            module="viv_analysis.screening",
            function="screen_viv_susceptibility",
            depends_on=['catenary', 'natural_freq'],
        )

        assert len(task.depends_on) == 2
        assert 'catenary' in task.depends_on
        assert 'natural_freq' in task.depends_on

    def test_task_duration_calculation(self):
        """Test task duration calculation"""
        task = WorkflowTask(
            name="Test Task",
            task_id="test",
            module="test",
            function="test",
        )

        # Before execution
        assert task.duration_seconds is None

        # After execution
        task.start_time = datetime(2024, 1, 1, 12, 0, 0)
        task.end_time = datetime(2024, 1, 1, 12, 0, 5)

        assert task.duration_seconds == 5.0

    def test_task_to_dict(self):
        """Test task serialization to dictionary"""
        task = WorkflowTask(
            name="Test Task",
            task_id="test_task",
            module="test_module",
            function="test_function",
        )
        task.status = TaskStatus.COMPLETED
        task.start_time = datetime(2024, 1, 1, 12, 0, 0)
        task.end_time = datetime(2024, 1, 1, 12, 0, 5)

        task_dict = task.to_dict()

        assert task_dict['name'] == "Test Task"
        assert task_dict['task_id'] == "test_task"
        assert task_dict['status'] == "completed"
        assert task_dict['duration_seconds'] == 5.0


class TestWorkflowDefinition:
    """Test WorkflowDefinition model"""

    def test_create_workflow(self):
        """Test creating a workflow definition"""
        workflow = WorkflowDefinition(
            name="Test Workflow",
            description="Test workflow description",
        )

        assert workflow.name == "Test Workflow"
        assert workflow.description == "Test workflow description"
        assert workflow.version == "1.0.0"
        assert len(workflow.tasks) == 0
        assert workflow.stop_on_error is True

    def test_add_task(self):
        """Test adding tasks to workflow"""
        workflow = WorkflowDefinition(
            name="Test Workflow",
            description="Test",
        )

        task1 = WorkflowTask(
            name="Task 1",
            task_id="task1",
            module="module1",
            function="func1",
        )

        task2 = WorkflowTask(
            name="Task 2",
            task_id="task2",
            module="module2",
            function="func2",
        )

        workflow.add_task(task1)
        workflow.add_task(task2)

        assert len(workflow.tasks) == 2
        assert workflow.get_task("task1") == task1
        assert workflow.get_task("task2") == task2

    def test_get_task(self):
        """Test retrieving task by ID"""
        workflow = WorkflowDefinition(name="Test", description="Test")

        task = WorkflowTask(
            name="Test Task",
            task_id="test_task",
            module="test",
            function="test",
        )

        workflow.add_task(task)

        assert workflow.get_task("test_task") == task
        assert workflow.get_task("nonexistent") is None

    def test_dependency_order_simple(self):
        """Test dependency ordering with simple linear dependencies"""
        workflow = WorkflowDefinition(name="Test", description="Test")

        # Task A (no dependencies)
        task_a = WorkflowTask(
            name="Task A",
            task_id="task_a",
            module="test",
            function="test",
        )

        # Task B (depends on A)
        task_b = WorkflowTask(
            name="Task B",
            task_id="task_b",
            module="test",
            function="test",
            depends_on=['task_a'],
        )

        # Task C (depends on B)
        task_c = WorkflowTask(
            name="Task C",
            task_id="task_c",
            module="test",
            function="test",
            depends_on=['task_b'],
        )

        workflow.add_task(task_a)
        workflow.add_task(task_b)
        workflow.add_task(task_c)

        order = workflow.get_dependency_order()

        assert len(order) == 3
        assert order[0] == ['task_a']
        assert order[1] == ['task_b']
        assert order[2] == ['task_c']

    def test_dependency_order_parallel(self):
        """Test dependency ordering with parallel tasks"""
        workflow = WorkflowDefinition(name="Test", description="Test")

        # Task A (no dependencies)
        task_a = WorkflowTask(
            name="Task A",
            task_id="task_a",
            module="test",
            function="test",
        )

        # Task B (no dependencies - can run in parallel with A)
        task_b = WorkflowTask(
            name="Task B",
            task_id="task_b",
            module="test",
            function="test",
        )

        # Task C (depends on both A and B)
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

        order = workflow.get_dependency_order()

        assert len(order) == 2
        # First group should contain both A and B (parallel)
        assert set(order[0]) == {'task_a', 'task_b'}
        # Second group should contain only C
        assert order[1] == ['task_c']

    def test_dependency_order_circular_detection(self):
        """Test circular dependency detection"""
        workflow = WorkflowDefinition(name="Test", description="Test")

        # Task A depends on B
        task_a = WorkflowTask(
            name="Task A",
            task_id="task_a",
            module="test",
            function="test",
            depends_on=['task_b'],
        )

        # Task B depends on A (circular!)
        task_b = WorkflowTask(
            name="Task B",
            task_id="task_b",
            module="test",
            function="test",
            depends_on=['task_a'],
        )

        workflow.add_task(task_a)
        workflow.add_task(task_b)

        with pytest.raises(ValueError, match="Circular dependency"):
            workflow.get_dependency_order()

    def test_workflow_to_dict(self):
        """Test workflow serialization to dictionary"""
        workflow = WorkflowDefinition(
            name="Test Workflow",
            description="Test description",
        )

        task = WorkflowTask(
            name="Task 1",
            task_id="task1",
            module="test",
            function="test",
        )

        workflow.add_task(task)

        workflow_dict = workflow.to_dict()

        assert workflow_dict['name'] == "Test Workflow"
        assert workflow_dict['description'] == "Test description"
        assert workflow_dict['task_count'] == 1
        assert len(workflow_dict['tasks']) == 1


class TestWorkflowResult:
    """Test WorkflowResult model"""

    def test_create_result(self):
        """Test creating workflow result"""
        result = WorkflowResult(
            workflow_id="test_workflow",
            workflow_name="Test Workflow",
            status="completed",
            start_time=datetime(2024, 1, 1, 12, 0, 0),
            end_time=datetime(2024, 1, 1, 12, 0, 30),
        )

        assert result.workflow_id == "test_workflow"
        assert result.workflow_name == "Test Workflow"
        assert result.status == "completed"
        assert result.duration_seconds == 30.0

    def test_success_rate(self):
        """Test success rate calculation"""
        result = WorkflowResult(
            workflow_id="test",
            workflow_name="Test",
            status="partial",
            start_time=datetime.now(),
            end_time=datetime.now(),
            task_statuses={
                'task1': TaskStatus.COMPLETED,
                'task2': TaskStatus.COMPLETED,
                'task3': TaskStatus.FAILED,
                'task4': TaskStatus.COMPLETED,
            }
        )

        assert result.success_rate == 75.0  # 3 out of 4 completed

    def test_failed_tasks(self):
        """Test getting failed tasks"""
        result = WorkflowResult(
            workflow_id="test",
            workflow_name="Test",
            status="partial",
            start_time=datetime.now(),
            end_time=datetime.now(),
            task_statuses={
                'task1': TaskStatus.COMPLETED,
                'task2': TaskStatus.FAILED,
                'task3': TaskStatus.FAILED,
                'task4': TaskStatus.COMPLETED,
            }
        )

        failed = result.get_failed_tasks()

        assert len(failed) == 2
        assert 'task2' in failed
        assert 'task3' in failed

    def test_result_to_dict(self):
        """Test result serialization"""
        result = WorkflowResult(
            workflow_id="test_workflow",
            workflow_name="Test Workflow",
            status="completed",
            start_time=datetime(2024, 1, 1, 12, 0, 0),
            end_time=datetime(2024, 1, 1, 12, 0, 30),
            task_statuses={
                'task1': TaskStatus.COMPLETED,
                'task2': TaskStatus.COMPLETED,
            },
            outputs={
                'result1': 100.0,
                'result2': 200.0,
            }
        )

        result_dict = result.to_dict()

        assert result_dict['workflow_id'] == "test_workflow"
        assert result_dict['status'] == "completed"
        assert result_dict['duration_seconds'] == 30.0
        assert result_dict['success_rate'] == 100.0
        assert result_dict['task_count'] == 2
        assert 'result1' in result_dict['outputs']


class TestWorkflowConfig:
    """Test WorkflowConfig model"""

    def test_default_config(self):
        """Test default configuration"""
        config = WorkflowConfig()

        assert config.max_parallel_tasks == 4
        assert config.task_timeout_seconds == 3600
        assert config.save_intermediate_results is True
        assert config.generate_report is True
        assert config.log_level == "INFO"

    def test_custom_config(self):
        """Test custom configuration"""
        config = WorkflowConfig(
            max_parallel_tasks=8,
            task_timeout_seconds=7200,
            cache_directory="/tmp/cache",
            log_level="DEBUG",
        )

        assert config.max_parallel_tasks == 8
        assert config.task_timeout_seconds == 7200
        assert config.cache_directory == "/tmp/cache"
        assert config.log_level == "DEBUG"

    def test_config_to_dict(self):
        """Test config serialization"""
        config = WorkflowConfig(
            max_parallel_tasks=8,
            output_directory="/output",
        )

        config_dict = config.to_dict()

        assert config_dict['max_parallel_tasks'] == 8
        assert config_dict['output_directory'] == "/output"
