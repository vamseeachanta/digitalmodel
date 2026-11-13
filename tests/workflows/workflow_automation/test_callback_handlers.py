"""Console, JSON and webhook progress handlers."""

import sys
import types
from datetime import datetime

import pytest

from digitalmodel.workflows.workflow_automation.callbacks import (
    CallbackHandler,
    ConsoleCallback,
    JSONCallback,
    ProgressUpdate,
    WebhookCallback,
    WorkflowEvent,
)

STAMP = datetime(2026, 1, 5, 9, 30, 15)


def update(event, **kwargs):
    return ProgressUpdate(
        event=event, workflow_id="wf1", workflow_name="Demo", timestamp=STAMP, **kwargs
    )


def test_base_handler_requires_an_implementation():
    with pytest.raises(NotImplementedError):
        CallbackHandler().on_event(update(WorkflowEvent.TASK_STARTED))


@pytest.mark.parametrize("verbose", [False, True])
def test_console_prints_detail_only_when_verbose(capsys, verbose):
    handler = ConsoleCallback(verbose=verbose)

    handler.on_event(update(WorkflowEvent.TASK_GROUP_STARTED))
    handler.on_event(update(WorkflowEvent.TASK_PROGRESS, message="iteration 3"))
    handler.on_event(update(WorkflowEvent.TASK_PROGRESS))
    out = capsys.readouterr().out

    assert ("[09:30:15] 📦 Task group started" in out) is verbose
    assert ("[09:30:15]    iteration 3" in out) is verbose
    assert out.count("\n") == (3 if verbose else 0)


def test_console_reports_failures_with_their_messages(capsys):
    handler = ConsoleCallback()

    handler.on_event(update(WorkflowEvent.TASK_FAILED, task_name="VIV"))
    handler.on_event(
        update(WorkflowEvent.TASK_FAILED, task_name="Fatigue", message="no S-N curve")
    )
    handler.on_event(
        update(
            WorkflowEvent.WORKFLOW_COMPLETED,
            total_tasks=4,
            completed_tasks=3,
            failed_tasks=1,
            progress_percent=75.0,
        )
    )
    handler.on_event(update(WorkflowEvent.WORKFLOW_FAILED))
    handler.on_event(update(WorkflowEvent.WORKFLOW_FAILED, message="stopped"))
    lines = capsys.readouterr().out.splitlines()

    assert "[09:30:15] ❌ VIV - FAILED" in lines
    assert "[09:30:15] ❌ Fatigue - FAILED" in lines
    assert "[09:30:15]    Error: no S-N curve" in lines
    assert "[09:30:15]    Progress: 75.0%" in lines
    assert "[09:30:15]    Completed: 3/4" in lines
    assert "[09:30:15]    Failed: 1" in lines
    assert lines.count("[09:30:15] 💥 Workflow failed!") == 2
    assert [line for line in lines if "Error:" in line] == [
        "[09:30:15]    Error: no S-N curve",
        "[09:30:15]    Error: stopped",
    ]


def test_json_events_can_be_read_and_cleared():
    handler = JSONCallback()
    handler.on_event(update(WorkflowEvent.TASK_CACHED, task_id="t1", task_name="T"))

    (event,) = handler.get_events()
    assert event["event"] == "task_cached"
    assert event["timestamp"] == "2026-01-05T09:30:15"
    assert event["task_id"] == "t1"

    handler.clear()
    assert handler.get_events() == []


def test_webhook_posts_event_payload(monkeypatch):
    posts = []

    def post(url, json, timeout):
        posts.append((url, json, timeout))

    monkeypatch.setitem(sys.modules, "requests", types.SimpleNamespace(post=post))

    WebhookCallback("https://hooks.example.invalid/wf").on_event(
        update(WorkflowEvent.TASK_COMPLETED, task_id="t1", progress_percent=50.0)
    )

    ((url, payload, timeout),) = posts
    assert url == "https://hooks.example.invalid/wf"
    assert timeout == 5
    assert payload["event"] == "task_completed"
    assert payload["progress_percent"] == 50.0
    assert payload["workflow_name"] == "Demo"


def test_webhook_failure_does_not_interrupt_the_workflow(monkeypatch):
    def post(url, json, timeout):
        raise ConnectionError("endpoint down")

    monkeypatch.setitem(sys.modules, "requests", types.SimpleNamespace(post=post))

    # Returns normally: delivery errors are swallowed.
    assert WebhookCallback("https://hooks.example.invalid/wf").on_event(
        update(WorkflowEvent.WORKFLOW_STARTED)
    ) is None
