#!/usr/bin/env python3
"""
ABOUTME: Workflow orchestration engine for executing multi-step engineering
analysis workflows with dependency management and result caching.
"""

import importlib
from datetime import datetime
from typing import Dict, Any, Optional
from pathlib import Path
import json

from .models import (
    WorkflowDefinition,
    WorkflowTask,
    WorkflowResult,
    WorkflowConfig,
    TaskStatus,
)
from .cache import WorkflowCache
from .parallel import AdaptiveParallelExecutor
from .callbacks import ProgressTracker, ConsoleCallback


class WorkflowOrchestrator:
    """
    Orchestrate execution of engineering analysis workflows

    Manages task execution order based on dependencies, caches intermediate
    results, and handles error propagation.
    """

    def __init__(
        self,
        config: Optional[WorkflowConfig] = None,
        enable_cache: bool = True,
        enable_parallel: bool = True,
        enable_progress: bool = True,
    ):
        """
        Initialize workflow orchestrator

        Args:
            config: Workflow configuration (uses defaults if None)
            enable_cache: Enable result caching (default: True)
            enable_parallel: Enable parallel execution (default: True)
            enable_progress: Enable progress callbacks (default: True)
        """
        self.config = config or WorkflowConfig()
        self.task_results: Dict[str, Any] = {}  # Cache for task results

        # Advanced features
        self.enable_cache = enable_cache
        self.enable_parallel = enable_parallel
        self.enable_progress = enable_progress

        # Initialize cache
        self.cache = WorkflowCache() if enable_cache else None

        # Initialize parallel executor
        self.parallel_executor = AdaptiveParallelExecutor(
            max_workers=self.config.max_parallel_tasks
        ) if enable_parallel else None

        # Initialize progress tracker
        self.progress_tracker = ProgressTracker() if enable_progress else None
        if self.progress_tracker:
            # Add default console callback
            self.progress_tracker.add_callback(ConsoleCallback(verbose=False))

    def execute_workflow(
        self,
        workflow: WorkflowDefinition
    ) -> WorkflowResult:
        """
        Execute complete workflow

        Args:
            workflow: Workflow definition to execute

        Returns:
            WorkflowResult with execution summary and all task results
        """
        start_time = datetime.now()

        # Emit workflow started event
        if self.progress_tracker:
            self.progress_tracker.workflow_started(
                workflow.workflow_id,
                workflow.name,
                len(workflow.tasks),
            )

        # Get dependency-ordered task groups
        try:
            task_groups = workflow.get_dependency_order()
        except ValueError as e:
            # Circular dependency
            return self._create_error_result(
                workflow,
                start_time,
                f"Workflow validation failed: {e}"
            )

        # Execute task groups in order
        for group_idx, task_ids in enumerate(task_groups):
            if not self.enable_parallel:
                print(f"\n=== Executing task group {group_idx + 1}/{len(task_groups)} ===")

            # Get tasks for this group
            tasks = [workflow.get_task(tid) for tid in task_ids if workflow.get_task(tid)]

            if not tasks:
                continue

            # Execute tasks (parallel or sequential)
            if self.enable_parallel and len(tasks) > 1:
                # Parallel execution
                self._execute_task_group_parallel(workflow, tasks)
            else:
                # Sequential execution
                for task in tasks:
                    try:
                        self._execute_task(workflow, task)
                    except Exception as e:
                        if workflow.stop_on_error and task.required:
                            if self.progress_tracker:
                                self.progress_tracker.workflow_failed(
                                    workflow.workflow_id,
                                    workflow.name,
                                    f"Task '{task.name}' failed: {e}"
                                )
                            return self._create_error_result(
                                workflow,
                                start_time,
                                f"Task '{task.name}' failed: {e}"
                            )

        # Create result summary
        end_time = datetime.now()

        result = self._create_workflow_result(workflow, start_time, end_time)

        # Emit workflow completed event
        if self.progress_tracker:
            completed = sum(1 for s in result.task_statuses.values()
                          if s in [TaskStatus.COMPLETED, TaskStatus.CACHED])
            failed = len(result.get_failed_tasks())

            self.progress_tracker.workflow_completed(
                workflow.workflow_id,
                workflow.name,
                len(workflow.tasks),
                completed,
                failed,
            )

        return result

    def _execute_task_group_parallel(
        self,
        workflow: WorkflowDefinition,
        tasks: list
    ) -> None:
        """
        Execute a group of tasks in parallel

        Args:
            workflow: Parent workflow
            tasks: List of tasks to execute in parallel
        """
        if not self.parallel_executor:
            # Fallback to sequential
            for task in tasks:
                self._execute_task(workflow, task)
            return

        # Execute tasks in parallel
        results = self.parallel_executor.execute_task_group(
            tasks,
            lambda t: self._execute_task(workflow, t)
        )

        # Process results
        for result in results:
            task = workflow.get_task(result.task_id)
            if not result.success and task and task.required:
                raise Exception(result.error)

    def _execute_task(
        self,
        workflow: WorkflowDefinition,
        task: WorkflowTask
    ) -> Any:
        """
        Execute single task

        Args:
            workflow: Parent workflow
            task: Task to execute

        Returns:
            Task result

        Raises:
            Exception: If task execution fails
        """
        # Emit task started event
        if self.progress_tracker:
            self.progress_tracker.task_started(
                workflow.workflow_id,
                workflow.name,
                task
            )

        if not self.enable_parallel:
            print(f"  → Running: {task.name}")

        task.status = TaskStatus.RUNNING
        task.start_time = datetime.now()

        try:
            # Prepare task inputs
            inputs = self._prepare_task_inputs(task)

            # Check cache first
            if self.cache and task.cache_results:
                cached_result = self.cache.get_task_result(task, inputs)
                if cached_result is not None:
                    task.result = cached_result
                    task.status = TaskStatus.CACHED
                    task.end_time = datetime.now()

                    if not self.enable_parallel:
                        print(f"  💾 Cached: {task.name}")

                    if self.progress_tracker:
                        self.progress_tracker.task_cached(
                            workflow.workflow_id,
                            workflow.name,
                            task
                        )

                    # Store outputs
                    for output_name, result_key in task.outputs.items():
                        self.task_results[result_key] = self._extract_output(cached_result, output_name)

                    return cached_result

            # Execute task function
            result = self._call_task_function(task, inputs)

            # Store result
            task.result = result
            task.status = TaskStatus.COMPLETED
            task.end_time = datetime.now()

            # Cache result
            if self.cache and task.cache_results:
                self.cache.set_task_result(task, inputs, result)

            if task.cache_results:
                self.task_results[task.task_id] = result

            # Store outputs
            for output_name, result_key in task.outputs.items():
                self.task_results[result_key] = self._extract_output(result, output_name)

            duration = task.duration_seconds

            if not self.enable_parallel:
                print(f"  ✓ Completed: {task.name} ({duration:.1f}s)")

            # Emit task completed event
            if self.progress_tracker:
                self.progress_tracker.task_completed(
                    workflow.workflow_id,
                    workflow.name,
                    task
                )

            return result

        except Exception as e:
            task.status = TaskStatus.FAILED
            task.end_time = datetime.now()
            task.error_message = str(e)

            if not self.enable_parallel:
                print(f"  ✗ Failed: {task.name} - {e}")

            # Emit task failed event
            if self.progress_tracker:
                self.progress_tracker.task_failed(
                    workflow.workflow_id,
                    workflow.name,
                    task,
                    str(e)
                )

            if task.required:
                raise
            else:
                task.status = TaskStatus.SKIPPED
                return None

    def _prepare_task_inputs(self, task: WorkflowTask) -> Dict[str, Any]:
        """
        Prepare inputs for task execution

        Resolves input references to previous task outputs.

        Args:
            task: Task to prepare inputs for

        Returns:
            Dictionary of resolved inputs
        """
        inputs = {}

        for key, value in task.inputs.items():
            # Check if value is a reference to previous task output
            if isinstance(value, str) and value.startswith('$'):
                # Reference format: $task_id.output_name or $result_key
                ref = value[1:]  # Remove $
                if ref in self.task_results:
                    inputs[key] = self.task_results[ref]
                else:
                    raise ValueError(f"Undefined reference: {value}")
            else:
                # Direct value
                inputs[key] = value

        return inputs

    def _call_task_function(
        self,
        task: WorkflowTask,
        inputs: Dict[str, Any]
    ) -> Any:
        """
        Call the task's module function

        Args:
            task: Task definition
            inputs: Prepared inputs

        Returns:
            Function result
        """
        # Import module
        module_path = f"digitalmodel.modules.{task.module}"
        try:
            module = importlib.import_module(module_path)
        except ImportError:
            raise ImportError(f"Module not found: {module_path}")

        # Get function
        if not hasattr(module, task.function):
            raise AttributeError(
                f"Function '{task.function}' not found in module '{task.module}'"
            )

        func = getattr(module, task.function)

        # Call function
        return func(**inputs)

    def _extract_output(self, result: Any, output_name: str) -> Any:
        """
        Extract named output from task result

        Args:
            result: Task result
            output_name: Name of output to extract

        Returns:
            Extracted output value
        """
        # Handle dictionary results
        if isinstance(result, dict):
            return result.get(output_name)

        # Handle object with attributes
        if hasattr(result, output_name):
            return getattr(result, output_name)

        # Return whole result if no specific output
        return result

    def _create_workflow_result(
        self,
        workflow: WorkflowDefinition,
        start_time: datetime,
        end_time: datetime
    ) -> WorkflowResult:
        """
        Create workflow result summary

        Args:
            workflow: Workflow definition
            start_time: Workflow start time
            end_time: Workflow end time

        Returns:
            WorkflowResult object
        """
        # Collect task statuses and results
        task_statuses = {}
        task_errors = {}
        task_results = {}

        for task in workflow.tasks:
            task_statuses[task.task_id] = task.status

            if task.error_message:
                task_errors[task.task_id] = task.error_message

            if task.result is not None:
                task_results[task.task_id] = task.result

        # Determine overall status
        failed_count = sum(1 for s in task_statuses.values() if s == TaskStatus.FAILED)
        completed_count = sum(1 for s in task_statuses.values()
                            if s in [TaskStatus.COMPLETED, TaskStatus.CACHED])

        if failed_count > 0:
            status = "partial" if completed_count > 0 else "failed"
        else:
            status = "completed"

        return WorkflowResult(
            workflow_id=workflow.workflow_id,
            workflow_name=workflow.name,
            status=status,
            start_time=start_time,
            end_time=end_time,
            task_results=task_results,
            task_statuses=task_statuses,
            task_errors=task_errors,
            outputs=self.task_results.copy(),
        )

    def _create_error_result(
        self,
        workflow: WorkflowDefinition,
        start_time: datetime,
        error_message: str
    ) -> WorkflowResult:
        """
        Create error result for failed workflow

        Args:
            workflow: Workflow definition
            start_time: Start time
            error_message: Error description

        Returns:
            WorkflowResult with error status
        """
        return WorkflowResult(
            workflow_id=workflow.workflow_id,
            workflow_name=workflow.name,
            status="failed",
            start_time=start_time,
            end_time=datetime.now(),
            task_errors={'workflow': error_message},
        )

    def save_results(
        self,
        result: WorkflowResult,
        output_file: str
    ) -> None:
        """
        Save workflow results to JSON file

        Args:
            result: Workflow result
            output_file: Output file path
        """
        output_path = Path(output_file)
        output_path.parent.mkdir(parents=True, exist_ok=True)

        with open(output_path, 'w') as f:
            json.dump(result.to_dict(), f, indent=2, default=str)
