# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic weather samples exercise deterministic serial ROV planning."""

from dataclasses import replace

import pytest

from digitalmodel.subsea.rov import seastate_limits
from digitalmodel.subsea.rov.intervention_sequence import InterventionTask, plan_intervention


def task(name="inspect", **kwargs):
    return InterventionTask(name, "tree", "inspection", 2, 2, **kwargs)


def test_weather_wait_dependencies_and_fraction():
    first = task()
    second = InterventionTask("operate", "manifold", "valve", 1, 2, ("inspect",))
    result = plan_intervention([second, first], [3, 1, 1, 3, 3, 1, 1], 2, step_hours=1)
    assert result.complete
    assert [(item.task_id, item.start_hours, item.end_hours) for item in result.schedule] == [
        ("inspect", 1, 3), ("operate", 5, 6),
    ]
    assert result.campaign_duration_hours == 6
    assert result.waiting_on_weather_fraction == 0.5
    assert result.unscheduled == ()


def test_no_window_is_explicit_and_blocks_dependents():
    dependent = replace(task("follow"), dependencies=("inspect",))
    result = plan_intervention([task(), dependent], [3, 1, 3], 2, step_hours=1)
    assert not result.complete
    assert result.schedule == ()
    assert result.unscheduled == (("inspect", "no_weather_window"),
                                  ("follow", "dependency_unavailable"))
    assert result.campaign_duration_hours is None
    assert result.waiting_on_weather_fraction is None


def test_longer_continuous_window_and_horizon_boundary():
    operation = replace(task(), weather_window_steps=3)
    result = plan_intervention([operation], [1, 3, 1, 1, 1], 2, step_hours=0.5)
    assert result.schedule[0].start_hours == 1
    assert result.schedule[0].end_hours == 2
    assert not plan_intervention([operation], [1, 1], 2, step_hours=1).complete


def test_alpha_allowance_is_delegated(monkeypatch):
    calls = []
    original = seastate_limits.seastate_operability

    def spy(hs, hs_limit, alpha=1):
        calls.append((hs, hs_limit, alpha))
        return original(hs, hs_limit, alpha)

    monkeypatch.setattr(seastate_limits, "seastate_operability", spy)
    result = plan_intervention([task()], [1.5, 1.5, 1, 1], 2, alpha=0.5, step_hours=1)
    assert result.schedule[0].start_hours == 2
    assert calls
    assert all(limit == 2 and alpha == 0.5 for _, limit, alpha in calls)


def test_partial_plan_retains_independent_tasks_without_campaign_metrics():
    long = replace(task(), duration_steps=4, weather_window_steps=4)
    result = plan_intervention([long, task("independent")], [1, 1], 2, step_hours=1)
    assert not result.complete
    assert result.schedule[0].task_id == "independent"
    assert result.campaign_duration_hours is None


def test_multiple_dependencies_and_serial_resource():
    tasks = [task("a"), task("b"), task("c", dependencies=("a", "b"))]
    result = plan_intervention(tasks, [1] * 6, 2, step_hours=1)
    assert [item.start_hours for item in result.schedule] == [0, 2, 4]
    assert result.waiting_on_weather_fraction == 0


@pytest.mark.parametrize("tasks", [
    [task(), task()],
    [task(dependencies=("missing",))],
    [task(dependencies=("inspect",))],
    [task(dependencies=("b",)), task("b", dependencies=("inspect",))],
    [replace(task(), duration_steps=0)],
    [replace(task(), duration_steps=1.5)],
    [replace(task(), weather_window_steps=1)],
    [replace(task(), task_id="")],
    [replace(task(), equipment="")],
])
def test_invalid_task_graph_and_duration(tasks):
    with pytest.raises(ValueError):
        plan_intervention(tasks, [1] * 10, 2, step_hours=1)


@pytest.mark.parametrize("kwargs", [
    {"hs": [float("nan")]}, {"hs": [-1]}, {"hs": [float("inf")]},
    {"hs_limit": float("inf")}, {"hs_limit": 0}, {"alpha": 0},
    {"step_hours": 0}, {"step_hours": float("nan")},
])
def test_invalid_weather_inputs(kwargs):
    inputs = dict(hs=[1, 1], hs_limit=2, step_hours=1)
    inputs.update(kwargs)
    with pytest.raises(ValueError):
        plan_intervention([task()], **inputs)


def test_empty_horizon_and_empty_campaign():
    assert not plan_intervention([task()], [], 2, step_hours=1).complete
    result = plan_intervention([], [], 2, step_hours=1)
    assert result.complete
    assert result.campaign_duration_hours == 0
    assert result.waiting_on_weather_fraction == 0
