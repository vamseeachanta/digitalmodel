# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Plan a single ROV's tasks against uniformly spaced synthetic or forecast Hs.

The scheduler uses caller-defined durations, continuous weather windows, rated
Hs limit and forecast allowance. Every Hs decision delegates to
``seastate_limits.seastate_operability``. No standard-derived operational limit,
alpha factor or duration is embedded. API 17H and DNV-ST-N001 are the references
identified by the package. The public DNV landing page identifies DNV-ST-N001,
Marine operations and marine warranty, edition 2023-12. Clause titles and the
API 17H edition remain unverified here.
This is a planning screen, not a weather-restricted-operation approval.

Each sample represents a full interval of ``step_hours`` hours starting at that
sample. Durations and weather windows are positive integer sample counts; the
weather window must cover at least the task duration. Forecast coverage beyond
the horizon is never assumed. A longer weather window reserves forecast cover,
not the ROV after the task finishes. The time origin is the first sample.

Tasks run serially in stable topological order, breaking ties by input order.
Starts are earliest for that fixed order, not a globally optimised campaign.
The weather waiting fraction is idle time divided by elapsed campaign time,
including initial waiting and excluding unused trailing forecast samples.
Incomplete campaigns retain feasible independent tasks and explicit failure
reasons, but expose no campaign duration or waiting fraction.
"""

from __future__ import annotations

from dataclasses import dataclass
from math import isfinite
from typing import Sequence

from digitalmodel.subsea.rov import seastate_limits


@dataclass(frozen=True)
class InterventionTask:
    """Task identity, equipment/interface labels, sample counts and dependencies."""

    task_id: str
    equipment: str
    interface: str
    duration_steps: int
    weather_window_steps: int
    dependencies: tuple[str, ...] = ()


@dataclass(frozen=True)
class ScheduledTask:
    """A task's start and finish in hours from the forecast time origin."""

    task_id: str
    equipment: str
    interface: str
    start_hours: float
    end_hours: float


@dataclass(frozen=True)
class InterventionPlan:
    """Scheduled tasks, explicit failures and complete-campaign metrics."""

    schedule: tuple[ScheduledTask, ...]
    unscheduled: tuple[tuple[str, str], ...]
    complete: bool
    campaign_duration_hours: float | None
    waiting_on_weather_fraction: float | None


def _validate_task(task: InterventionTask) -> None:
    for label in (task.task_id, task.equipment, task.interface):
        if not isinstance(label, str) or not label.strip():
            raise ValueError("task, equipment and interface labels must be nonempty")
    for count in (task.duration_steps, task.weather_window_steps):
        if type(count) is not int or count <= 0:
            raise ValueError("duration and window must be positive integer sample counts")
    if task.weather_window_steps < task.duration_steps:
        raise ValueError("weather window must cover the entire task duration")
    if len(set(task.dependencies)) != len(task.dependencies):
        raise ValueError("duplicate dependency")


def _ordered_tasks(tasks: Sequence[InterventionTask]) -> list[InterventionTask]:
    ids = [task.task_id for task in tasks]
    if len(set(ids)) != len(ids):
        raise ValueError("duplicate task_id")
    for task in tasks:
        _validate_task(task)
        if any(dependency not in ids for dependency in task.dependencies):
            raise ValueError("unknown dependency")
    remaining = list(tasks)
    ordered: list[InterventionTask] = []
    visited: set[str] = set()
    while remaining:
        ready = next((task for task in remaining
                      if set(task.dependencies) <= visited), None)
        if ready is None:
            raise ValueError("cyclic task dependencies")
        remaining.remove(ready)
        ordered.append(ready)
        visited.add(ready.task_id)
    return ordered


def _weather_mask(
    hs: Sequence[float], hs_limit: float, alpha: float, step_hours: float,
) -> list[bool]:
    if not isfinite(step_hours) or step_hours <= 0:
        raise ValueError("step_hours must be finite and positive")
    if not isfinite(hs_limit) or not isfinite(alpha):
        raise ValueError("hs_limit and alpha must be finite")
    seastate_limits.seastate_operability(0, hs_limit, alpha)
    if any(not isfinite(value) or value < 0 for value in hs):
        raise ValueError("Hs samples must be finite and nonnegative")
    return [seastate_limits.seastate_operability(value, hs_limit, alpha).operable
            for value in hs]


def _first_window(mask: Sequence[bool], cursor: int, width: int) -> int | None:
    run = 0
    for index in range(cursor, len(mask)):
        run = run + 1 if mask[index] else 0
        if run == width:
            return index - width + 1
    return None


def plan_intervention(
    tasks: Sequence[InterventionTask],
    hs: Sequence[float],
    hs_limit: float,
    *,
    step_hours: float,
    alpha: float = 1.0,
) -> InterventionPlan:
    """Schedule serial tasks at the earliest feasible samples in dependency order.

    Args:
        tasks: Tasks with durations, equipment/interface labels and dependencies.
        hs: Uniformly spaced interval wave heights [m], allowing an empty horizon.
        hs_limit: Caller-qualified rated significant-wave-height limit [m].
        step_hours: Duration represented by each sample [h].
        alpha: Caller-selected forecast allowance passed to the existing screen.

    Returns:
        Complete plan with campaign metrics, or a partial plan with failure reasons.

    Raises:
        ValueError: If task graph, sample counts, heights or limits are invalid.
    """
    ordered = _ordered_tasks(tasks)
    mask = _weather_mask(hs, hs_limit, alpha, step_hours)
    cursor = 0
    working_steps = 0
    completed: set[str] = set()
    schedule = []
    unscheduled = []
    for task in ordered:
        if not set(task.dependencies) <= completed:
            unscheduled.append((task.task_id, "dependency_unavailable"))
            continue
        start = _first_window(mask, cursor, task.weather_window_steps)
        if start is None:
            unscheduled.append((task.task_id, "no_weather_window"))
            continue
        cursor = start + task.duration_steps
        working_steps += task.duration_steps
        completed.add(task.task_id)
        schedule.append(ScheduledTask(task.task_id, task.equipment, task.interface,
                                      start * step_hours, cursor * step_hours))
    complete = not unscheduled
    duration = cursor * step_hours if complete else None
    waiting = (cursor - working_steps) / cursor if cursor else 0.0
    return InterventionPlan(tuple(schedule), tuple(unscheduled), complete,
                            duration, waiting if complete else None)
