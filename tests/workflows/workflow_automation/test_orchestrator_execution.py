"""Execute workflows end to end through WorkflowOrchestrator.

Task modules are stand-ins registered by the ``analysis_modules`` fixture
(see conftest.py), so dependency ordering, reference resolution, caching,
error handling and progress events run unchanged.
"""

import json
from types import SimpleNamespace

import pytest

from digitalmodel.workflows.workflow_automation.callbacks import JSONCallback
from digitalmodel.workflows.workflow_automation.models import (
    TaskStatus,
    WorkflowDefinition,
    WorkflowTask,
)
from digitalmodel.workflows.workflow_automation.orchestrator import (
    WorkflowOrchestrator,
)


def chain_workflow(**overrides):
    """Two dependent tasks: the second consumes the first one's output."""
    workflow = WorkflowDefinition(name="Chain", description="load then check")
    workflow.add_task(WorkflowTask(
        name="Load",
        task_id="load",
        module="demo.loads",
        function="compute_load",
        inputs={"mass": 10.0},
        outputs={"load": "chain_load"},
    ))
    workflow.add_task(WorkflowTask(
        name="Check",
        task_id="check",
        module="demo.checks",
        function="check_load",
        inputs={"load": "$chain_load", "limit": 150.0},
        outputs={"utilisation": "chain_utilisation"},
        depends_on=["load"],
    ))
    for key, value in overrides.items():
        setattr(workflow, key, value)
    return workflow


CHAIN_MODULES = {
    "demo.loads:compute_load": lambda mass: {"load": mass * 9.81},
    "demo.checks:check_load": lambda load, limit: {"utilisation": load / limit},
}


def tracked_orchestrator(**kwargs):
    orchestrator = WorkflowOrchestrator(**kwargs)
    events = JSONCallback()
    orchestrator.progress_tracker.add_callback(events)
    return orchestrator, events


def event_names(events):
    return [(e["event"], e["task_id"]) for e in events.get_events()]


def test_chain_resolves_references_and_aggregates_outputs(run_dir, analysis_modules):
    calls = analysis_modules(CHAIN_MODULES)
    orchestrator, events = tracked_orchestrator()

    result = orchestrator.execute_workflow(chain_workflow())

    assert result.status == "completed"
    assert result.success_rate == 100.0
    assert calls == [
        ("demo.loads:compute_load", {"mass": 10.0}),
        ("demo.checks:check_load", {"load": pytest.approx(98.1), "limit": 150.0}),
    ]
    assert result.outputs["chain_load"] == pytest.approx(98.1)
    assert result.outputs["chain_utilisation"] == pytest.approx(0.654)
    # cache_results=True also stores each whole task result under its id.
    assert result.outputs["load"] == {"load": pytest.approx(98.1)}
    assert result.task_statuses == {
        "load": TaskStatus.COMPLETED,
        "check": TaskStatus.COMPLETED,
    }
    assert event_names(events) == [
        ("workflow_started", None),
        ("task_started", "load"),
        ("task_completed", "load"),
        ("task_started", "check"),
        ("task_completed", "check"),
        ("workflow_completed", None),
    ]
    assert events.get_events()[-1]["progress_percent"] == 100.0


def test_second_run_uses_cached_task_results(run_dir, analysis_modules):
    calls = analysis_modules(CHAIN_MODULES)
    WorkflowOrchestrator().execute_workflow(chain_workflow())
    assert len(calls) == 2

    orchestrator, events = tracked_orchestrator()
    result = orchestrator.execute_workflow(chain_workflow())

    assert len(calls) == 2  # nothing recomputed
    assert result.status == "completed"
    assert set(result.task_statuses.values()) == {TaskStatus.CACHED}
    assert result.outputs["chain_utilisation"] == pytest.approx(0.654)
    assert [name for name, _ in event_names(events)].count("task_cached") == 2


def test_disabled_cache_recomputes(run_dir, analysis_modules):
    calls = analysis_modules(CHAIN_MODULES)
    for _ in range(2):
        orchestrator = WorkflowOrchestrator(enable_cache=False)
        assert orchestrator.cache is None
        assert orchestrator.execute_workflow(chain_workflow()).status == "completed"

    assert len(calls) == 4
    assert not (run_dir / ".workflow_cache").exists()


def test_sequential_mode_prints_groups_and_task_progress(
    run_dir, analysis_modules, capsys
):
    analysis_modules(CHAIN_MODULES)
    orchestrator = WorkflowOrchestrator(enable_parallel=False, enable_progress=False)

    result = orchestrator.execute_workflow(chain_workflow())
    out = capsys.readouterr().out

    assert result.status == "completed"
    assert "=== Executing task group 1/2 ===" in out
    assert "=== Executing task group 2/2 ===" in out
    assert "→ Running: Load" in out
    assert "✓ Completed: Check" in out

    rerun = WorkflowOrchestrator(enable_parallel=False, enable_progress=False)
    rerun.execute_workflow(chain_workflow())
    assert "💾 Cached: Load" in capsys.readouterr().out


def test_missing_module_stops_the_workflow(run_dir):
    orchestrator, events = tracked_orchestrator()

    result = orchestrator.execute_workflow(chain_workflow())

    assert result.status == "failed"
    assert result.task_errors == {
        "workflow": "Task 'Load' failed: Module not found: digitalmodel.modules.demo.loads"
    }
    assert event_names(events)[-2:] == [
        ("task_failed", "load"),
        ("workflow_failed", None),
    ]


def test_missing_function_is_reported_with_module_name(run_dir, analysis_modules):
    analysis_modules({"demo.loads:other_function": lambda: None})

    result = WorkflowOrchestrator().execute_workflow(chain_workflow())

    assert result.task_errors["workflow"] == (
        "Task 'Load' failed: Function 'compute_load' not found in module 'demo.loads'"
    )


def test_circular_dependency_fails_validation(run_dir):
    workflow = chain_workflow()
    workflow.get_task("load").depends_on = ["check"]

    result = WorkflowOrchestrator().execute_workflow(workflow)

    assert result.status == "failed"
    assert result.task_errors["workflow"].startswith(
        "Workflow validation failed: Circular dependency detected"
    )


def test_optional_task_failure_is_skipped(run_dir, analysis_modules, capsys):
    def broken(**kwargs):
        raise RuntimeError("diverged")

    analysis_modules({**CHAIN_MODULES, "demo.checks:check_load": broken})
    workflow = chain_workflow()
    workflow.get_task("check").required = False
    orchestrator = WorkflowOrchestrator(enable_parallel=False)

    result = orchestrator.execute_workflow(workflow)

    assert result.status == "completed"
    assert result.task_statuses["check"] == TaskStatus.SKIPPED
    assert result.task_errors == {"check": "diverged"}
    assert "chain_utilisation" not in result.outputs
    assert result.success_rate == 50.0
    assert "✗ Failed: Check - diverged" in capsys.readouterr().out


def test_required_failure_without_stop_gives_partial_result(run_dir, analysis_modules):
    def broken(**kwargs):
        raise RuntimeError("diverged")

    analysis_modules({**CHAIN_MODULES, "demo.checks:check_load": broken})

    result = WorkflowOrchestrator().execute_workflow(
        chain_workflow(stop_on_error=False)
    )

    assert result.status == "partial"
    assert result.get_failed_tasks() == ["check"]
    assert result.outputs["chain_load"] == pytest.approx(98.1)


def test_required_failures_only_give_failed_status(run_dir):
    # No modules registered and stop_on_error off: every task fails.
    result = WorkflowOrchestrator().execute_workflow(
        chain_workflow(stop_on_error=False)
    )

    assert result.status == "failed"
    assert sorted(result.get_failed_tasks()) == ["check", "load"]


def fan_out_workflow():
    """One source task feeding two independent tasks (a parallel group)."""
    workflow = WorkflowDefinition(name="Fan", description="one source, two users")
    workflow.add_task(WorkflowTask(
        name="Source", task_id="source", module="demo.source",
        function="make", inputs={"n": 3}, outputs={"value": "fan_value"},
    ))
    for side in ("left", "right"):
        workflow.add_task(WorkflowTask(
            name=side.title(), task_id=side, module=f"demo.{side}",
            function="use", inputs={"value": "$fan_value"},
            outputs={"value": f"fan_{side}"}, depends_on=["source"],
        ))
    return workflow


FAN_MODULES = {
    "demo.source:make": lambda n: {"value": n},
    "demo.left:use": lambda value: {"value": value + 1},
    "demo.right:use": lambda value: {"value": value * 2},
}


def test_independent_tasks_run_as_a_parallel_group(run_dir, analysis_modules):
    calls = analysis_modules(FAN_MODULES)

    result = WorkflowOrchestrator().execute_workflow(fan_out_workflow())

    assert result.status == "completed"
    assert result.outputs["fan_left"] == 4
    assert result.outputs["fan_right"] == 6
    assert sorted(key for key, _ in calls[1:]) == ["demo.left:use", "demo.right:use"]


def test_required_failure_in_parallel_group_is_raised(run_dir, analysis_modules):
    # Unlike the sequential path, which returns a failed result, a required
    # failure inside a parallel group propagates out of execute_workflow.
    def broken(value):
        raise RuntimeError("right side diverged")

    analysis_modules({**FAN_MODULES, "demo.right:use": broken})

    with pytest.raises(Exception, match="right side diverged"):
        WorkflowOrchestrator().execute_workflow(fan_out_workflow())


def test_parallel_group_without_executor_runs_sequentially(run_dir, analysis_modules):
    calls = analysis_modules(FAN_MODULES)
    orchestrator = WorkflowOrchestrator(enable_parallel=False)
    workflow = fan_out_workflow()
    orchestrator.task_results["fan_value"] = 5

    orchestrator._execute_task_group_parallel(
        workflow, [workflow.get_task("left"), workflow.get_task("right")]
    )

    assert [key for key, _ in calls] == ["demo.left:use", "demo.right:use"]
    assert orchestrator.task_results["fan_left"] == 6
    assert orchestrator.task_results["fan_right"] == 10


def test_extract_output_reads_attributes_or_returns_whole_result():
    orchestrator = WorkflowOrchestrator(enable_cache=False)
    obj = SimpleNamespace(tension=12.5)

    assert orchestrator._extract_output(obj, "tension") == 12.5
    assert orchestrator._extract_output(42.0, "tension") == 42.0
    assert orchestrator._extract_output({"a": 1}, "missing") is None


def test_save_results_creates_parent_directories(run_dir, analysis_modules):
    analysis_modules(CHAIN_MODULES)
    orchestrator = WorkflowOrchestrator()
    result = orchestrator.execute_workflow(chain_workflow())

    orchestrator.save_results(result, str(run_dir / "a" / "b" / "result.json"))

    saved = json.loads((run_dir / "a" / "b" / "result.json").read_text())
    assert saved["workflow_name"] == "Chain"
    assert saved["success_rate"] == 100.0
    assert saved["failed_tasks"] == []
