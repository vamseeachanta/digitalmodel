#!/usr/bin/env python3
"""
ABOUTME: Data models for workflow automation including workflow definitions,
task execution results, and configuration structures.
"""

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Any, Callable
from enum import Enum
from datetime import datetime
import uuid


class TaskStatus(Enum):
    """Task execution status"""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    SKIPPED = "skipped"
    CACHED = "cached"


@dataclass
class WorkflowTask:
    """
    Individual task within a workflow

    A task represents a single analysis step (e.g., catenary analysis,
    VIV screening) that produces outputs consumed by downstream tasks.
    """
    name: str
    task_id: str
    module: str  # Module to use (e.g., 'catenary_riser', 'viv_analysis')
    function: str  # Function to call within module
    inputs: Dict[str, Any] = field(default_factory=dict)
    outputs: Dict[str, str] = field(default_factory=dict)  # output_name -> result_key mapping

    # Execution control
    depends_on: List[str] = field(default_factory=list)  # List of task_ids this depends on
    required: bool = True  # If False, workflow continues even if task fails
    cache_results: bool = True  # Whether to cache results

    # Execution state
    status: TaskStatus = TaskStatus.PENDING
    start_time: Optional[datetime] = None
    end_time: Optional[datetime] = None
    error_message: Optional[str] = None
    result: Optional[Any] = None

    def __post_init__(self):
        """Generate unique ID if not provided"""
        if not self.task_id:
            self.task_id = f"{self.name}_{uuid.uuid4().hex[:8]}"

    @property
    def duration_seconds(self) -> Optional[float]:
        """Calculate task execution duration"""
        if self.start_time and self.end_time:
            return (self.end_time - self.start_time).total_seconds()
        return None

    def to_dict(self) -> dict:
        """Convert to dictionary"""
        return {
            'name': self.name,
            'task_id': self.task_id,
            'module': self.module,
            'function': self.function,
            'status': self.status.value,
            'duration_seconds': self.duration_seconds,
            'error_message': self.error_message,
        }


@dataclass
class WorkflowDefinition:
    """
    Complete workflow definition

    Defines a sequence of tasks with dependencies that form a
    complete engineering analysis (e.g., riser analysis, mooring design).
    """
    name: str
    description: str
    version: str = "1.0.0"

    tasks: List[WorkflowTask] = field(default_factory=list)

    # Workflow metadata
    workflow_id: str = field(default_factory=lambda: uuid.uuid4().hex)
    created_at: datetime = field(default_factory=datetime.now)

    # Configuration
    stop_on_error: bool = True  # Stop workflow if any required task fails
    parallel_execution: bool = False  # Execute independent tasks in parallel
    cache_enabled: bool = True  # Enable result caching

    def add_task(self, task: WorkflowTask) -> None:
        """Add task to workflow"""
        self.tasks.append(task)

    def get_task(self, task_id: str) -> Optional[WorkflowTask]:
        """Get task by ID"""
        for task in self.tasks:
            if task.task_id == task_id:
                return task
        return None

    def get_dependency_order(self) -> List[List[str]]:
        """
        Get tasks in dependency order (topological sort)

        Returns:
            List of task ID lists, where each inner list can be executed in parallel
        """
        # Build dependency graph
        task_deps = {task.task_id: set(task.depends_on) for task in self.tasks}
        task_ids = set(task_deps.keys())

        # Topological sort with parallel groups
        result = []
        remaining = task_ids.copy()

        while remaining:
            # Find tasks with no unmet dependencies
            ready = []
            for task_id in remaining:
                deps = task_deps[task_id]
                if not deps or deps.issubset(task_ids - remaining):
                    ready.append(task_id)

            if not ready:
                # Circular dependency
                raise ValueError(f"Circular dependency detected in workflow: {remaining}")

            result.append(ready)
            remaining -= set(ready)

        return result

    def to_dict(self) -> dict:
        """Convert to dictionary"""
        return {
            'name': self.name,
            'description': self.description,
            'version': self.version,
            'workflow_id': self.workflow_id,
            'tasks': [task.to_dict() for task in self.tasks],
            'task_count': len(self.tasks),
        }


@dataclass
class WorkflowResult:
    """
    Results from workflow execution

    Contains all task results, timing information, and execution summary.
    """
    workflow_id: str
    workflow_name: str

    # Execution summary
    status: str  # 'completed', 'failed', 'partial'
    start_time: datetime
    end_time: datetime

    # Task results
    task_results: Dict[str, Any] = field(default_factory=dict)  # task_id -> result
    task_statuses: Dict[str, TaskStatus] = field(default_factory=dict)
    task_errors: Dict[str, str] = field(default_factory=dict)

    # Aggregated outputs (key results across workflow)
    outputs: Dict[str, Any] = field(default_factory=dict)

    @property
    def duration_seconds(self) -> float:
        """Total workflow execution time"""
        return (self.end_time - self.start_time).total_seconds()

    @property
    def success_rate(self) -> float:
        """Percentage of successful tasks"""
        if not self.task_statuses:
            return 0.0

        successful = sum(1 for s in self.task_statuses.values()
                        if s == TaskStatus.COMPLETED or s == TaskStatus.CACHED)
        return 100.0 * successful / len(self.task_statuses)

    def get_failed_tasks(self) -> List[str]:
        """Get list of failed task IDs"""
        return [tid for tid, status in self.task_statuses.items()
                if status == TaskStatus.FAILED]

    def to_dict(self) -> dict:
        """Convert to dictionary"""
        return {
            'workflow_id': self.workflow_id,
            'workflow_name': self.workflow_name,
            'status': self.status,
            'duration_seconds': self.duration_seconds,
            'success_rate': self.success_rate,
            'task_count': len(self.task_statuses),
            'failed_tasks': self.get_failed_tasks(),
            'outputs': self.outputs,
        }


@dataclass
class WorkflowConfig:
    """
    Configuration for workflow execution

    Global parameters and settings for workflow orchestration.
    """
    # Execution settings
    max_parallel_tasks: int = 4
    task_timeout_seconds: int = 3600  # 1 hour default
    cache_directory: Optional[str] = None

    # Output settings
    save_intermediate_results: bool = True
    output_directory: Optional[str] = None
    generate_report: bool = True

    # Logging
    log_level: str = "INFO"
    log_to_file: bool = False
    log_file: Optional[str] = None

    def to_dict(self) -> dict:
        """Convert to dictionary"""
        return {
            'max_parallel_tasks': self.max_parallel_tasks,
            'task_timeout_seconds': self.task_timeout_seconds,
            'cache_directory': self.cache_directory,
            'output_directory': self.output_directory,
            'generate_report': self.generate_report,
        }
