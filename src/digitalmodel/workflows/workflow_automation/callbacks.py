#!/usr/bin/env python3
"""
ABOUTME: Progress callback system for real-time workflow execution monitoring
with support for multiple callback handlers and event types.
"""

from typing import Callable, List, Dict, Any, Optional
from dataclasses import dataclass
from datetime import datetime
from enum import Enum

from .models import WorkflowTask, WorkflowResult, TaskStatus


class WorkflowEvent(Enum):
    """Workflow execution events"""
    WORKFLOW_STARTED = "workflow_started"
    WORKFLOW_COMPLETED = "workflow_completed"
    WORKFLOW_FAILED = "workflow_failed"

    TASK_GROUP_STARTED = "task_group_started"
    TASK_GROUP_COMPLETED = "task_group_completed"

    TASK_STARTED = "task_started"
    TASK_PROGRESS = "task_progress"
    TASK_COMPLETED = "task_completed"
    TASK_FAILED = "task_failed"
    TASK_CACHED = "task_cached"


@dataclass
class ProgressUpdate:
    """Progress update event data"""
    event: WorkflowEvent
    workflow_id: str
    workflow_name: str
    timestamp: datetime

    # Task-specific data
    task_id: Optional[str] = None
    task_name: Optional[str] = None
    task_status: Optional[TaskStatus] = None

    # Progress metrics
    total_tasks: int = 0
    completed_tasks: int = 0
    failed_tasks: int = 0
    progress_percent: float = 0.0

    # Additional metadata
    message: Optional[str] = None
    data: Optional[Dict[str, Any]] = None


class CallbackHandler:
    """Base class for workflow progress callbacks"""

    def on_event(self, update: ProgressUpdate) -> None:
        """
        Handle progress update event

        Args:
            update: ProgressUpdate with event data
        """
        raise NotImplementedError("Subclasses must implement on_event")


class ConsoleCallback(CallbackHandler):
    """Print progress updates to console"""

    def __init__(self, verbose: bool = False):
        """
        Initialize console callback

        Args:
            verbose: Show detailed progress information
        """
        self.verbose = verbose

    def on_event(self, update: ProgressUpdate) -> None:
        """Print event to console"""
        timestamp = update.timestamp.strftime("%H:%M:%S")

        if update.event == WorkflowEvent.WORKFLOW_STARTED:
            print(f"\n[{timestamp}] 🚀 Starting workflow: {update.workflow_name}")
            print(f"[{timestamp}]    Total tasks: {update.total_tasks}")

        elif update.event == WorkflowEvent.TASK_GROUP_STARTED:
            if self.verbose:
                print(f"\n[{timestamp}] 📦 Task group started")

        elif update.event == WorkflowEvent.TASK_STARTED:
            print(f"[{timestamp}] ▶️  {update.task_name}")

        elif update.event == WorkflowEvent.TASK_PROGRESS:
            if self.verbose and update.message:
                print(f"[{timestamp}]    {update.message}")

        elif update.event == WorkflowEvent.TASK_COMPLETED:
            print(f"[{timestamp}] ✅ {update.task_name} - completed")

        elif update.event == WorkflowEvent.TASK_FAILED:
            print(f"[{timestamp}] ❌ {update.task_name} - FAILED")
            if update.message:
                print(f"[{timestamp}]    Error: {update.message}")

        elif update.event == WorkflowEvent.TASK_CACHED:
            print(f"[{timestamp}] 💾 {update.task_name} - cached result")

        elif update.event == WorkflowEvent.WORKFLOW_COMPLETED:
            print(f"\n[{timestamp}] ✨ Workflow completed!")
            print(f"[{timestamp}]    Progress: {update.progress_percent:.1f}%")
            print(f"[{timestamp}]    Completed: {update.completed_tasks}/{update.total_tasks}")
            if update.failed_tasks > 0:
                print(f"[{timestamp}]    Failed: {update.failed_tasks}")

        elif update.event == WorkflowEvent.WORKFLOW_FAILED:
            print(f"\n[{timestamp}] 💥 Workflow failed!")
            if update.message:
                print(f"[{timestamp}]    Error: {update.message}")


class FileCallback(CallbackHandler):
    """Write progress updates to log file"""

    def __init__(self, log_file: str):
        """
        Initialize file callback

        Args:
            log_file: Path to log file
        """
        self.log_file = log_file

    def on_event(self, update: ProgressUpdate) -> None:
        """Write event to log file"""
        with open(self.log_file, 'a', encoding='utf-8') as f:
            timestamp = update.timestamp.strftime("%Y-%m-%d %H:%M:%S")
            event_name = update.event.value

            log_line = f"[{timestamp}] {event_name}"

            if update.task_name:
                log_line += f" - {update.task_name}"

            if update.message:
                log_line += f" - {update.message}"

            log_line += f" ({update.progress_percent:.1f}%)\n"

            f.write(log_line)


class JSONCallback(CallbackHandler):
    """Store progress updates as JSON for programmatic access"""

    def __init__(self):
        """Initialize JSON callback"""
        self.events: List[Dict[str, Any]] = []

    def on_event(self, update: ProgressUpdate) -> None:
        """Store event as JSON"""
        event_dict = {
            'event': update.event.value,
            'workflow_id': update.workflow_id,
            'workflow_name': update.workflow_name,
            'timestamp': update.timestamp.isoformat(),
            'task_id': update.task_id,
            'task_name': update.task_name,
            'progress_percent': update.progress_percent,
            'completed_tasks': update.completed_tasks,
            'failed_tasks': update.failed_tasks,
            'message': update.message,
        }

        self.events.append(event_dict)

    def get_events(self) -> List[Dict[str, Any]]:
        """Get all recorded events"""
        return self.events

    def clear(self) -> None:
        """Clear recorded events"""
        self.events.clear()


class WebhookCallback(CallbackHandler):
    """Send progress updates to webhook endpoint"""

    def __init__(self, webhook_url: str):
        """
        Initialize webhook callback

        Args:
            webhook_url: URL to send POST requests
        """
        self.webhook_url = webhook_url

    def on_event(self, update: ProgressUpdate) -> None:
        """Send event to webhook"""
        import requests

        payload = {
            'event': update.event.value,
            'workflow_id': update.workflow_id,
            'workflow_name': update.workflow_name,
            'timestamp': update.timestamp.isoformat(),
            'task_id': update.task_id,
            'task_name': update.task_name,
            'progress_percent': update.progress_percent,
            'message': update.message,
        }

        try:
            requests.post(self.webhook_url, json=payload, timeout=5)
        except Exception:
            # Don't fail workflow if webhook fails
            pass


class ProgressTracker:
    """
    Track and broadcast workflow execution progress

    Manages multiple callback handlers and emits progress events.
    """

    def __init__(self):
        """Initialize progress tracker"""
        self.callbacks: List[CallbackHandler] = []

    def add_callback(self, callback: CallbackHandler) -> None:
        """
        Add callback handler

        Args:
            callback: CallbackHandler instance
        """
        self.callbacks.append(callback)

    def remove_callback(self, callback: CallbackHandler) -> None:
        """
        Remove callback handler

        Args:
            callback: CallbackHandler instance
        """
        if callback in self.callbacks:
            self.callbacks.remove(callback)

    def emit(self, update: ProgressUpdate) -> None:
        """
        Emit progress update to all callbacks

        Args:
            update: ProgressUpdate to broadcast
        """
        for callback in self.callbacks:
            try:
                callback.on_event(update)
            except Exception as e:
                # Don't let callback errors break workflow
                print(f"Warning: Callback error: {e}")

    def workflow_started(
        self,
        workflow_id: str,
        workflow_name: str,
        total_tasks: int,
    ) -> None:
        """Emit workflow started event"""
        self.emit(ProgressUpdate(
            event=WorkflowEvent.WORKFLOW_STARTED,
            workflow_id=workflow_id,
            workflow_name=workflow_name,
            timestamp=datetime.now(),
            total_tasks=total_tasks,
        ))

    def workflow_completed(
        self,
        workflow_id: str,
        workflow_name: str,
        total_tasks: int,
        completed_tasks: int,
        failed_tasks: int,
    ) -> None:
        """Emit workflow completed event"""
        progress = (completed_tasks / total_tasks * 100) if total_tasks > 0 else 0

        self.emit(ProgressUpdate(
            event=WorkflowEvent.WORKFLOW_COMPLETED,
            workflow_id=workflow_id,
            workflow_name=workflow_name,
            timestamp=datetime.now(),
            total_tasks=total_tasks,
            completed_tasks=completed_tasks,
            failed_tasks=failed_tasks,
            progress_percent=progress,
        ))

    def workflow_failed(
        self,
        workflow_id: str,
        workflow_name: str,
        error_message: str,
    ) -> None:
        """Emit workflow failed event"""
        self.emit(ProgressUpdate(
            event=WorkflowEvent.WORKFLOW_FAILED,
            workflow_id=workflow_id,
            workflow_name=workflow_name,
            timestamp=datetime.now(),
            message=error_message,
        ))

    def task_started(
        self,
        workflow_id: str,
        workflow_name: str,
        task: WorkflowTask,
    ) -> None:
        """Emit task started event"""
        self.emit(ProgressUpdate(
            event=WorkflowEvent.TASK_STARTED,
            workflow_id=workflow_id,
            workflow_name=workflow_name,
            timestamp=datetime.now(),
            task_id=task.task_id,
            task_name=task.name,
        ))

    def task_completed(
        self,
        workflow_id: str,
        workflow_name: str,
        task: WorkflowTask,
    ) -> None:
        """Emit task completed event"""
        self.emit(ProgressUpdate(
            event=WorkflowEvent.TASK_COMPLETED,
            workflow_id=workflow_id,
            workflow_name=workflow_name,
            timestamp=datetime.now(),
            task_id=task.task_id,
            task_name=task.name,
            task_status=TaskStatus.COMPLETED,
        ))

    def task_failed(
        self,
        workflow_id: str,
        workflow_name: str,
        task: WorkflowTask,
        error_message: str,
    ) -> None:
        """Emit task failed event"""
        self.emit(ProgressUpdate(
            event=WorkflowEvent.TASK_FAILED,
            workflow_id=workflow_id,
            workflow_name=workflow_name,
            timestamp=datetime.now(),
            task_id=task.task_id,
            task_name=task.name,
            task_status=TaskStatus.FAILED,
            message=error_message,
        ))

    def task_cached(
        self,
        workflow_id: str,
        workflow_name: str,
        task: WorkflowTask,
    ) -> None:
        """Emit task cached event"""
        self.emit(ProgressUpdate(
            event=WorkflowEvent.TASK_CACHED,
            workflow_id=workflow_id,
            workflow_name=workflow_name,
            timestamp=datetime.now(),
            task_id=task.task_id,
            task_name=task.name,
            task_status=TaskStatus.CACHED,
        ))
