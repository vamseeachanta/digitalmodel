# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Serial ROV planning on uniformly spaced interval Hs samples.

Reference: DNV-ST-N001, Marine operations and marine warranty, December 2023,
clauses 2.6.2.1, 2.6.7.3 / Figure 2-4 and 2.6.9.2. Publisher:
https://www.dnv.com/energy/standards-guidelines/dnv-st-n001-marine-operations-and-marine-warranty/
The publisher page identifies the edition, not the contents of these clauses.
Clause verification against the official edition remains NOT_EVALUATED.
``from_operation_periods`` adds caller-assessed planned and contingency counts;
all Hs decisions delegate to ``seastate_limits.seastate_operability``.

Grid/metric definitions (software model, not standard acceptance limits):
each sample covers a full interval of ``step_hours``; time=index*step_hours,
with zero origin. Positive integer duration/window counts avoid rounding; a
window covers at least the duration. Contingency reserves weather cover, not
execution time. Serial tasks use stable dependency order with input-order ties;
earliest starts apply to that fixed order. Waiting=(elapsed-working)/elapsed,
including initial idle time, excluding trailing forecast; the empty plan has
zero duration/waiting. Incomplete plans retain independent tasks and failure
reasons, with unavailable campaign metrics. Coverage ends with the last interval.

OPEN (NOT_EVALUATED): API 17H edition/clauses, contingency adequacy (2.6.4),
forecast-issuance time basis and alpha qualification (2.6.9.3), classification,
and other environmental criteria. This is an Hs planning screen. Periods, rated
limits and alpha require caller qualification; no tabulated coefficient is used.
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

    @classmethod
    def from_operation_periods(
        cls,
        task_id: str,
        equipment: str,
        interface: str,
        *,
        planned_steps: int,
        contingency_steps: int,
        dependencies: tuple[str, ...] = (),
    ) -> InterventionTask:
        """Combine required caller-assessed periods on the integer interval grid.

        DNV-ST-N001, December 2023, 2.6.2.1 supplies the sum, not the input
        assessments. Planned counts must be positive; contingency counts may
        explicitly be zero. Their adequacy under 2.6.4 is NOT_EVALUATED.
        Labels and duplicate dependencies are checked here; graph resolution
        remains with ``plan_intervention``.
        """
        # Zero is the grid's empty-period boundary (module grid definitions),
        # not a standard minimum contingency: DNV-ST-N001 (2023), 2.6.4.
        if type(planned_steps) is not int or planned_steps <= 0:
            raise ValueError("planned_steps must be a positive integer")
        if type(contingency_steps) is not int or contingency_steps < 0:
            raise ValueError("contingency_steps must be a nonnegative integer")
        window = planned_steps + contingency_steps  # N001 (2023), 2.6.2.1.
        task = cls(task_id, equipment, interface, planned_steps, window, dependencies)
        _validate_task(task)
        return task


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
        # Positive count boundary: module grid definitions; no rated duration.
        if type(count) is not int or count <= 0:
            raise ValueError(
                "duration and window must be positive integer sample counts"
            )
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
        ready = next(
            (task for task in remaining if set(task.dependencies) <= visited), None
        )
        if ready is None:
            raise ValueError("cyclic task dependencies")
        remaining.remove(ready)
        ordered.append(ready)
        visited.add(ready.task_id)
    return ordered


def _weather_mask(
    hs: Sequence[float],
    hs_limit: float,
    alpha: float,
    step_hours: float,
) -> list[bool]:
    # Zero bounds below are the module's positive-interval / nonnegative-Hs
    # input domain, not standard-derived operating limits.
    if not isfinite(step_hours) or step_hours <= 0:
        raise ValueError("step_hours must be finite and positive")
    if not isfinite(hs_limit) or not isfinite(alpha):
        raise ValueError("hs_limit and alpha must be finite")
    # Zero-height validation probe only (module input domain).
    seastate_limits.seastate_operability(0, hs_limit, alpha)
    if any(not isfinite(value) or value < 0 for value in hs):
        raise ValueError("Hs samples must be finite and nonnegative")
    return [
        seastate_limits.seastate_operability(value, hs_limit, alpha).operable
        for value in hs
    ]


def _first_window(mask: Sequence[bool], cursor: int, width: int) -> int | None:
    # Zero/one are count identities; index-width+1 converts an inclusive end
    # to a start under the full-interval grid definition in the module docstring.
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
    # Multiplicative identity in N001 (2023), 2.6.9.2; not a qualified allowance.
    alpha: float = 1.0,
) -> InterventionPlan:
    """Schedule serial tasks at the earliest feasible samples in dependency order.

    Args:
        tasks: Tasks with durations, equipment/interface labels and dependencies.
        hs: Uniformly spaced interval wave heights [m], allowing an empty horizon.
        hs_limit: Caller-qualified rated significant-wave-height limit [m].
        step_hours: Duration represented by each sample [h].
        alpha: Caller-qualified forecast allowance applied to every task. The
            caller must pass the most onerous (smallest) factor qualified for
            the planned operation periods of all tasks. The default is an
            arithmetic identity, not a qualified forecast allowance.

    Returns:
        Complete plan with campaign metrics, or a partial plan with failure reasons.

    Raises:
        ValueError: If task graph, sample counts, heights or limits are invalid.
    """
    ordered = _ordered_tasks(tasks)
    mask = _weather_mask(hs, hs_limit, alpha, step_hours)
    cursor = 0  # Time-origin identity: module grid definitions.
    working_steps = 0  # Empty sum identity: module metric definitions.
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
        schedule.append(
            ScheduledTask(
                task.task_id,
                task.equipment,
                task.interface,
                start * step_hours,
                cursor * step_hours,
            )
        )
    complete = not unscheduled
    duration = cursor * step_hours if complete else None
    # Empty-campaign zero is defined in the module metric definitions.
    waiting = (cursor - working_steps) / cursor if cursor else 0.0
    return InterventionPlan(
        tuple(schedule),
        tuple(unscheduled),
        complete,
        duration,
        waiting if complete else None,
    )
