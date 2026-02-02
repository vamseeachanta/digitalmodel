#!/usr/bin/env python3
"""
ABOUTME: Parallel task execution engine using concurrent.futures for executing
independent workflow tasks concurrently with thread/process pools.
"""

from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor, as_completed
from typing import List, Dict, Any, Callable, Optional
from dataclasses import dataclass
from datetime import datetime
import traceback

from .models import WorkflowTask, TaskStatus


@dataclass
class ParallelExecutionResult:
    """Result from parallel task execution"""
    task_id: str
    success: bool
    result: Any = None
    error: Optional[str] = None
    duration_seconds: float = 0.0


class ParallelExecutor:
    """
    Execute workflow tasks in parallel using thread/process pools

    Supports:
    - Thread-based parallelism (I/O bound tasks)
    - Process-based parallelism (CPU bound tasks)
    - Automatic dependency-aware scheduling
    - Error handling and result aggregation
    """

    def __init__(
        self,
        max_workers: int = 4,
        use_processes: bool = False,
    ):
        """
        Initialize parallel executor

        Args:
            max_workers: Maximum number of parallel workers
            use_processes: Use ProcessPoolExecutor instead of ThreadPoolExecutor
        """
        self.max_workers = max_workers
        self.use_processes = use_processes

    def execute_task_group(
        self,
        tasks: List[WorkflowTask],
        task_executor: Callable[[WorkflowTask], Any],
    ) -> List[ParallelExecutionResult]:
        """
        Execute a group of independent tasks in parallel

        Args:
            tasks: List of tasks to execute (must have no dependencies)
            task_executor: Function to execute each task

        Returns:
            List of ParallelExecutionResult for each task
        """
        if not tasks:
            return []

        # Choose executor type
        ExecutorClass = ProcessPoolExecutor if self.use_processes else ThreadPoolExecutor

        results = []

        with ExecutorClass(max_workers=self.max_workers) as executor:
            # Submit all tasks
            future_to_task = {
                executor.submit(self._execute_task_wrapper, task, task_executor): task
                for task in tasks
            }

            # Collect results as they complete
            for future in as_completed(future_to_task):
                task = future_to_task[future]

                try:
                    result = future.result()
                    results.append(result)
                except Exception as e:
                    # Task execution failed
                    results.append(ParallelExecutionResult(
                        task_id=task.task_id,
                        success=False,
                        error=str(e),
                    ))

        return results

    def _execute_task_wrapper(
        self,
        task: WorkflowTask,
        task_executor: Callable[[WorkflowTask], Any],
    ) -> ParallelExecutionResult:
        """
        Wrapper for task execution with error handling

        Args:
            task: Task to execute
            task_executor: Executor function

        Returns:
            ParallelExecutionResult
        """
        start_time = datetime.now()

        try:
            result = task_executor(task)

            end_time = datetime.now()
            duration = (end_time - start_time).total_seconds()

            return ParallelExecutionResult(
                task_id=task.task_id,
                success=True,
                result=result,
                duration_seconds=duration,
            )

        except Exception as e:
            end_time = datetime.now()
            duration = (end_time - start_time).total_seconds()

            # Capture full traceback
            error_msg = f"{str(e)}\n{traceback.format_exc()}"

            return ParallelExecutionResult(
                task_id=task.task_id,
                success=False,
                error=error_msg,
                duration_seconds=duration,
            )

    def execute_dependency_groups(
        self,
        task_groups: List[List[WorkflowTask]],
        task_executor: Callable[[WorkflowTask], Any],
    ) -> Dict[str, ParallelExecutionResult]:
        """
        Execute multiple task groups in order, parallelizing within groups

        Args:
            task_groups: List of task groups (from topological sort)
            task_executor: Function to execute each task

        Returns:
            Dictionary mapping task_id to ParallelExecutionResult
        """
        all_results = {}

        for group_idx, task_group in enumerate(task_groups):
            print(f"\n=== Executing task group {group_idx + 1}/{len(task_groups)} (parallel) ===")

            # Execute all tasks in group in parallel
            group_results = self.execute_task_group(task_group, task_executor)

            # Store results
            for result in group_results:
                all_results[result.task_id] = result

                # Print status
                status = "✓" if result.success else "✗"
                duration = result.duration_seconds

                task = next(t for t in task_group if t.task_id == result.task_id)
                print(f"  {status} {task.name} ({duration:.1f}s)")

        return all_results


class AdaptiveParallelExecutor:
    """
    Adaptive parallel executor that chooses optimal parallelism strategy

    Analyzes task characteristics and automatically selects:
    - Sequential execution for small task groups
    - Thread-based parallel for I/O bound tasks
    - Process-based parallel for CPU bound tasks
    """

    def __init__(self, max_workers: int = 4):
        """
        Initialize adaptive executor

        Args:
            max_workers: Maximum parallel workers
        """
        self.max_workers = max_workers
        self.thread_executor = ParallelExecutor(max_workers, use_processes=False)
        self.process_executor = ParallelExecutor(max_workers, use_processes=True)

    def execute_task_group(
        self,
        tasks: List[WorkflowTask],
        task_executor: Callable[[WorkflowTask], Any],
    ) -> List[ParallelExecutionResult]:
        """
        Execute task group with adaptive parallelism

        Args:
            tasks: Tasks to execute
            task_executor: Executor function

        Returns:
            List of execution results
        """
        if not tasks:
            return []

        # Use sequential execution for single task
        if len(tasks) == 1:
            result = self.thread_executor._execute_task_wrapper(tasks[0], task_executor)
            return [result]

        # Analyze task characteristics
        is_cpu_bound = self._estimate_cpu_bound(tasks)

        # Choose executor
        if is_cpu_bound and len(tasks) >= 2:
            # Use process-based parallelism for CPU-bound tasks
            return self.process_executor.execute_task_group(tasks, task_executor)
        else:
            # Use thread-based parallelism for I/O-bound tasks
            return self.thread_executor.execute_task_group(tasks, task_executor)

    def _estimate_cpu_bound(self, tasks: List[WorkflowTask]) -> bool:
        """
        Estimate if tasks are CPU-bound

        Args:
            tasks: Tasks to analyze

        Returns:
            True if tasks appear to be CPU-bound
        """
        # Heuristic: Check module names for computation-heavy modules
        cpu_bound_keywords = [
            'structural',
            'fatigue',
            'viv',
            'signal',
            'gmsh',
        ]

        for task in tasks:
            for keyword in cpu_bound_keywords:
                if keyword in task.module.lower():
                    return True

        # Default to I/O bound (safer for thread-based execution)
        return False


def execute_parallel_tasks(
    tasks: List[WorkflowTask],
    task_executor: Callable[[WorkflowTask], Any],
    max_workers: int = 4,
    adaptive: bool = True,
) -> List[ParallelExecutionResult]:
    """
    Convenience function for parallel task execution

    Args:
        tasks: Tasks to execute in parallel
        task_executor: Function to execute each task
        max_workers: Maximum parallel workers
        adaptive: Use adaptive executor (default: True)

    Returns:
        List of execution results
    """
    if adaptive:
        executor = AdaptiveParallelExecutor(max_workers=max_workers)
    else:
        executor = ParallelExecutor(max_workers=max_workers, use_processes=False)

    return executor.execute_task_group(tasks, task_executor)
