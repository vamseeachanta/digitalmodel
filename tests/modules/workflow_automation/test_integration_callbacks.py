#!/usr/bin/env python3
"""
ABOUTME: Integration tests for workflow progress callbacks and real-time
monitoring with validation of event sequences and timing.
"""

import pytest
import tempfile
from datetime import datetime

from digitalmodel.modules.workflow_automation import (
    WorkflowOrchestrator,
    WorkflowDefinition,
    WorkflowTask,
    ProgressTracker,
    ConsoleCallback,
    FileCallback,
    JSONCallback,
    WorkflowEvent,
    TaskStatus,
)


class TestCallbackIntegration:
    """Integration tests for progress callbacks"""

    def test_json_callback_event_recording(self):
        """Test JSONCallback records all events"""
        json_callback = JSONCallback()
        tracker = ProgressTracker()
        tracker.add_callback(json_callback)

        # Simulate workflow events
        tracker.workflow_started("wf1", "Test Workflow", 3)
        tracker.task_started("wf1", "Test Workflow", WorkflowTask(
            name="Task 1",
            task_id="t1",
            module="m",
            function="f",
        ))
        tracker.task_completed("wf1", "Test Workflow", WorkflowTask(
            name="Task 1",
            task_id="t1",
            module="m",
            function="f",
        ))
        tracker.workflow_completed("wf1", "Test Workflow", 3, 3, 0)

        # Check recorded events
        events = json_callback.get_events()

        assert len(events) == 4  # started, task_started, task_completed, completed

        # Verify event types
        assert events[0]['event'] == 'workflow_started'
        assert events[1]['event'] == 'task_started'
        assert events[2]['event'] == 'task_completed'
        assert events[3]['event'] == 'workflow_completed'

        # Verify workflow metadata
        assert all(e['workflow_id'] == "wf1" for e in events)
        assert all(e['workflow_name'] == "Test Workflow" for e in events)

    def test_file_callback_logging(self):
        """Test FileCallback writes to log file"""
        log_file = tempfile.mktemp(suffix='.log')

        file_callback = FileCallback(log_file)
        tracker = ProgressTracker()
        tracker.add_callback(file_callback)

        # Generate events
        tracker.workflow_started("wf1", "Test", 2)
        tracker.task_started("wf1", "Test", WorkflowTask(
            name="Task",
            task_id="t1",
            module="m",
            function="f",
        ))
        tracker.workflow_completed("wf1", "Test", 2, 2, 0)

        # Read log file
        with open(log_file, 'r') as f:
            log_content = f.read()

        # Verify log contains events
        assert "workflow_started" in log_content
        assert "task_started" in log_content
        assert "workflow_completed" in log_content

    def test_multiple_callbacks_same_tracker(self):
        """Test multiple callbacks can be attached to same tracker"""
        json_callback = JSONCallback()
        log_file = tempfile.mktemp(suffix='.log')
        file_callback = FileCallback(log_file)

        tracker = ProgressTracker()
        tracker.add_callback(json_callback)
        tracker.add_callback(file_callback)

        # Generate event
        tracker.workflow_started("wf1", "Test", 1)

        # Both should receive event
        assert len(json_callback.get_events()) == 1

        with open(log_file, 'r') as f:
            assert len(f.read()) > 0

    def test_callback_removal(self):
        """Test callback can be removed from tracker"""
        json_callback = JSONCallback()
        tracker = ProgressTracker()

        tracker.add_callback(json_callback)
        tracker.workflow_started("wf1", "Test", 1)

        # Should have 1 event
        assert len(json_callback.get_events()) == 1

        # Remove callback
        tracker.remove_callback(json_callback)

        # New events should not be recorded
        tracker.task_started("wf1", "Test", WorkflowTask(
            name="T",
            task_id="t",
            module="m",
            function="f",
        ))

        # Still only 1 event
        assert len(json_callback.get_events()) == 1

    def test_orchestrator_with_progress_enabled(self):
        """Test orchestrator with progress tracking enabled"""
        orchestrator = WorkflowOrchestrator(
            enable_progress=True,
            enable_cache=False,
            enable_parallel=False,
        )

        assert orchestrator.enable_progress is True
        assert orchestrator.progress_tracker is not None
        assert len(orchestrator.progress_tracker.callbacks) > 0  # Default console callback

    def test_orchestrator_with_progress_disabled(self):
        """Test orchestrator with progress tracking disabled"""
        orchestrator = WorkflowOrchestrator(
            enable_progress=False,
            enable_cache=False,
            enable_parallel=False,
        )

        assert orchestrator.enable_progress is False
        assert orchestrator.progress_tracker is None


class TestEventSequencing:
    """Test event sequencing and timing"""

    def test_event_sequence_normal_workflow(self):
        """Test event sequence for normal workflow execution"""
        json_callback = JSONCallback()
        tracker = ProgressTracker()
        tracker.add_callback(json_callback)

        # Simulate normal workflow
        tracker.workflow_started("wf1", "Test", 2)

        task1 = WorkflowTask(name="Task 1", task_id="t1", module="m", function="f")
        tracker.task_started("wf1", "Test", task1)
        tracker.task_completed("wf1", "Test", task1)

        task2 = WorkflowTask(name="Task 2", task_id="t2", module="m", function="f")
        tracker.task_started("wf1", "Test", task2)
        tracker.task_completed("wf1", "Test", task2)

        tracker.workflow_completed("wf1", "Test", 2, 2, 0)

        events = json_callback.get_events()

        # Expected sequence
        expected = [
            'workflow_started',
            'task_started',
            'task_completed',
            'task_started',
            'task_completed',
            'workflow_completed',
        ]

        actual = [e['event'] for e in events]
        assert actual == expected

    def test_event_sequence_with_failure(self):
        """Test event sequence when task fails"""
        json_callback = JSONCallback()
        tracker = ProgressTracker()
        tracker.add_callback(json_callback)

        tracker.workflow_started("wf1", "Test", 2)

        task1 = WorkflowTask(name="Task 1", task_id="t1", module="m", function="f")
        tracker.task_started("wf1", "Test", task1)
        tracker.task_failed("wf1", "Test", task1, "Error message")

        tracker.workflow_failed("wf1", "Test", "Workflow failed")

        events = json_callback.get_events()

        assert events[0]['event'] == 'workflow_started'
        assert events[1]['event'] == 'task_started'
        assert events[2]['event'] == 'task_failed'
        assert events[3]['event'] == 'workflow_failed'

        # Check error message
        assert events[2]['message'] == "Error message"

    def test_event_sequence_with_cached_task(self):
        """Test event sequence when task result is cached"""
        json_callback = JSONCallback()
        tracker = ProgressTracker()
        tracker.add_callback(json_callback)

        tracker.workflow_started("wf1", "Test", 1)

        task = WorkflowTask(name="Task", task_id="t", module="m", function="f")
        tracker.task_started("wf1", "Test", task)
        tracker.task_cached("wf1", "Test", task)

        tracker.workflow_completed("wf1", "Test", 1, 1, 0)

        events = json_callback.get_events()

        # Find cached event
        cached_events = [e for e in events if e['event'] == 'task_cached']
        assert len(cached_events) == 1

    def test_event_timestamps_ordering(self):
        """Test that event timestamps are ordered correctly"""
        json_callback = JSONCallback()
        tracker = ProgressTracker()
        tracker.add_callback(json_callback)

        # Generate sequence of events
        tracker.workflow_started("wf1", "Test", 1)

        task = WorkflowTask(name="Task", task_id="t", module="m", function="f")
        tracker.task_started("wf1", "Test", task)
        tracker.task_completed("wf1", "Test", task)

        tracker.workflow_completed("wf1", "Test", 1, 1, 0)

        events = json_callback.get_events()

        # Extract timestamps
        timestamps = [datetime.fromisoformat(e['timestamp']) for e in events]

        # Verify ordering
        for i in range(len(timestamps) - 1):
            assert timestamps[i] <= timestamps[i + 1]


class TestCallbackErrorHandling:
    """Test callback error handling"""

    def test_callback_exception_handling(self):
        """Test that callback exceptions don't break workflow"""

        class FailingCallback:
            """Callback that always raises exception"""

            def on_event(self, update):
                raise RuntimeError("Callback error")

        tracker = ProgressTracker()
        tracker.add_callback(FailingCallback())

        # Should not raise exception
        tracker.workflow_started("wf1", "Test", 1)

        # Tracker should continue working
        json_callback = JSONCallback()
        tracker.add_callback(json_callback)

        task = WorkflowTask(name="Task", task_id="t", module="m", function="f")
        tracker.task_started("wf1", "Test", task)

        # JSON callback should receive event
        events = json_callback.get_events()
        assert len(events) == 1


class TestProgressMetrics:
    """Test progress metrics calculation"""

    def test_progress_percentage_calculation(self):
        """Test progress percentage is calculated correctly"""
        json_callback = JSONCallback()
        tracker = ProgressTracker()
        tracker.add_callback(json_callback)

        total_tasks = 4

        tracker.workflow_started("wf1", "Test", total_tasks)

        # Complete tasks one by one
        for i in range(total_tasks):
            task = WorkflowTask(name=f"Task {i}", task_id=f"t{i}", module="m", function="f")
            tracker.task_started("wf1", "Test", task)
            tracker.task_completed("wf1", "Test", task)

        tracker.workflow_completed("wf1", "Test", total_tasks, total_tasks, 0)

        events = json_callback.get_events()

        # Final event should have 100% progress
        final_event = events[-1]
        assert final_event['event'] == 'workflow_completed'
        assert final_event['progress_percent'] == 100.0

    def test_progress_with_failures(self):
        """Test progress calculation with failed tasks"""
        json_callback = JSONCallback()
        tracker = ProgressTracker()
        tracker.add_callback(json_callback)

        tracker.workflow_started("wf1", "Test", 3)

        # Complete 2, fail 1
        for i in range(2):
            task = WorkflowTask(name=f"Task {i}", task_id=f"t{i}", module="m", function="f")
            tracker.task_started("wf1", "Test", task)
            tracker.task_completed("wf1", "Test", task)

        failed_task = WorkflowTask(name="Failed", task_id="tf", module="m", function="f")
        tracker.task_started("wf1", "Test", failed_task)
        tracker.task_failed("wf1", "Test", failed_task, "Error")

        tracker.workflow_completed("wf1", "Test", 3, 2, 1)

        events = json_callback.get_events()
        final = events[-1]

        # Should show 2 completed, 1 failed
        assert final['completed_tasks'] == 2
        assert final['failed_tasks'] == 1
        # Progress is based on completed/total
        assert final['progress_percent'] == pytest.approx(66.67, abs=0.1)
