"""Executor selection and grouped execution in the parallel module."""

import pytest

from digitalmodel.workflows.workflow_automation.models import WorkflowTask
from digitalmodel.workflows.workflow_automation.parallel import (
    AdaptiveParallelExecutor,
    ParallelExecutionResult,
    ParallelExecutor,
    execute_parallel_tasks,
)


def task(task_id, module="io.reader"):
    return WorkflowTask(name=task_id.upper(), task_id=task_id, module=module,
                        function="f")


def double(t):
    return t.task_id * 2


def test_empty_groups_return_no_results():
    assert ParallelExecutor().execute_task_group([], double) == []
    assert AdaptiveParallelExecutor().execute_task_group([], double) == []


def test_single_task_runs_inline_and_reports_result():
    (result,) = AdaptiveParallelExecutor().execute_task_group([task("a")], double)

    assert result.task_id == "a"
    assert result.success
    assert result.result == "aa"
    assert result.duration_seconds >= 0.0


@pytest.mark.parametrize(
    "modules, chosen",
    [
        (["io.reader", "io.writer"], "thread"),
        (["io.reader", "fatigue_analysis"], "process"),
        (["Structural_Analysis.stress", "io.reader"], "process"),
    ],
)
def test_group_executor_follows_cpu_bound_module_names(monkeypatch, modules, chosen):
    executor = AdaptiveParallelExecutor(max_workers=2)
    used = []
    for name in ("thread", "process"):
        monkeypatch.setattr(
            getattr(executor, f"{name}_executor"),
            "execute_task_group",
            lambda tasks, fn, _name=name: used.append(_name) or [],
        )

    executor.execute_task_group(
        [task(f"t{i}", module) for i, module in enumerate(modules)], double
    )

    assert used == [chosen]


def test_dependency_groups_run_in_order_and_report_each_task(capsys):
    def run(t):
        if t.task_id == "bad":
            raise ValueError("no convergence")
        return t.task_id

    results = ParallelExecutor(max_workers=2).execute_dependency_groups(
        [[task("a"), task("b")], [task("bad")]], run
    )
    out = capsys.readouterr().out

    assert set(results) == {"a", "b", "bad"}
    assert results["a"].result == "a"
    assert not results["bad"].success
    assert results["bad"].error.startswith("no convergence\nTraceback")
    assert out.index("task group 1/2 (parallel)") < out.index("task group 2/2")
    assert "✓ A (" in out
    assert "✗ BAD (" in out


@pytest.mark.parametrize("adaptive", [True, False])
def test_convenience_function_runs_every_task(adaptive):
    results = execute_parallel_tasks(
        [task("a"), task("b"), task("c")], double, max_workers=2, adaptive=adaptive
    )

    assert sorted(r.result for r in results) == ["aa", "bb", "cc"]
    assert all(isinstance(r, ParallelExecutionResult) and r.success for r in results)
