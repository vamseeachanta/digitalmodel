# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Hand-worked period/window cases for the serial ROV planning screen.

Reference: DNV-ST-N001, Marine operations and marine warranty, December 2023,
clauses 2.6.2.1, 2.6.7.3 / Figure 2-4 and 2.6.9.2. Publisher:
https://www.dnv.com/energy/standards-guidelines/dnv-st-n001-marine-operations-and-marine-warranty/
The publisher page identifies the edition, not the contents of these clauses.
Clause verification against the official edition remains NOT_EVALUATED.
All inputs below are synthetic arithmetic examples, not qualified ratings or
standard contingency/alpha recommendations. Test docstrings independently
derive expected schedules. Equality suffices for integer/binary-fraction results;
the rational waiting result is rounded by Python division, with no tolerance.
"""

import pytest

from digitalmodel.subsea.rov import InterventionTask, plan_intervention, seastate_limits


def test_period_sum_drives_earliest_continuous_window():
    """Hand A: TR=2+1=3 steps. Good runs [1,3), [4,7); first width 3
    starts at index 4. With step=1/2 h, start=4/2=2, finish=(4+2)/2=3 h.
    Working=2/2=1 h, elapsed=3 h, waiting=(3-1)/3=2/3.
    """
    # Synthetic inputs and exact expectations: hand A above.
    operation = InterventionTask.from_operation_periods(
        "inspect",
        "tree",
        "inspection",
        planned_steps=2,
        contingency_steps=1,
    )
    assert operation.duration_steps == 2
    assert operation.weather_window_steps == 3
    result = plan_intervention(
        [operation],
        [3, 1, 1, 3, 1, 1, 1],
        2,
        step_hours=0.5,
        alpha=1,
    )
    assert result.complete
    assert [
        (item.task_id, item.start_hours, item.end_hours) for item in result.schedule
    ] == [("inspect", 2, 3)]
    assert result.campaign_duration_hours == 3
    assert result.waiting_on_weather_fraction == 2 / 3
    assert result.unscheduled == ()


def test_contingency_requires_forecast_cover_and_blocks_dependents():
    """Hand B: TR=2+1=3 steps; horizon has only 2 good intervals. No start
    can cover 3; the dependent cannot start. Complete-campaign metrics are absent.
    """
    # Synthetic inputs: hand B; None denotes unavailable, not a numeric estimate.
    operation = InterventionTask.from_operation_periods(
        "inspect",
        "tree",
        "inspection",
        planned_steps=2,
        contingency_steps=1,
    )
    dependent = InterventionTask.from_operation_periods(
        "operate",
        "manifold",
        "valve",
        planned_steps=1,
        contingency_steps=1,
        dependencies=("inspect",),
    )
    result = plan_intervention(
        [dependent, operation],
        [1, 1],
        2,
        step_hours=1,
        alpha=1,
    )
    assert not result.complete
    assert result.schedule == ()
    assert result.unscheduled == (
        ("inspect", "no_weather_window"),
        ("operate", "dependency_unavailable"),
    )
    assert result.campaign_duration_hours is None
    assert result.waiting_on_weather_fraction is None


def test_periods_honour_delegated_forecast_allowance(monkeypatch):
    """Hand C: OPWF=(1/2)*2=1 m. Only indices [2,5) pass; TR=2+1=3.
    At one hour per step: start=2 h, finish=2+2=4 h, waiting=(4-2)/4=1/2.
    """
    calls = []
    original = seastate_limits.seastate_operability

    def record(hs, hs_limit, alpha):
        calls.append((hs, hs_limit, alpha))
        return original(hs, hs_limit, alpha)

    monkeypatch.setattr(seastate_limits, "seastate_operability", record)
    # Synthetic inputs and expectations: hand C above, not an alpha table.
    operation = InterventionTask.from_operation_periods(
        "inspect",
        "tree",
        "inspection",
        planned_steps=2,
        contingency_steps=1,
    )
    result = plan_intervention(
        [operation],
        [1.5, 1.5, 1, 1, 1],
        2,
        step_hours=1,
        alpha=0.5,
    )
    assert result.schedule[0].start_hours == 2
    assert result.schedule[0].end_hours == 4
    assert result.waiting_on_weather_fraction == 0.5
    # Every forecast sample must use the existing screen (its validation probe
    # may add a call); this assertion does not derive any expected schedule.
    for hs in (1.5, 1):  # Hand C's distinct synthetic wave heights.
        assert (hs, 2, 0.5) in calls
    assert all(limit == 2 and alpha == 0.5 for _, limit, alpha in calls)


def test_contingency_reserves_cover_without_occupying_the_rov():
    """Hand D: A and B each work 2 steps with TR=2+1=3. Six good hourly
    intervals cover A=[0,3), B=[2,5). Serial work is A=[0,2), B=[2,4),
    so elapsed=4 h, working=2+2=4 h and waiting=(4-4)/4=0.
    """
    # Synthetic inputs and expectations: hand D above.
    first = InterventionTask.from_operation_periods(
        "inspect",
        "tree",
        "inspection",
        planned_steps=2,
        contingency_steps=1,
    )
    second = InterventionTask.from_operation_periods(
        "operate",
        "manifold",
        "valve",
        planned_steps=2,
        contingency_steps=1,
        dependencies=("inspect",),
    )
    result = plan_intervention(
        [second, first],
        [1] * 6,
        2,
        step_hours=1,
        alpha=1,
    )
    assert result.complete
    assert [
        (item.task_id, item.start_hours, item.end_hours) for item in result.schedule
    ] == [("inspect", 0, 2), ("operate", 2, 4)]
    assert result.campaign_duration_hours == 4
    assert result.waiting_on_weather_fraction == 0


def test_explicit_zero_contingency_is_only_an_arithmetic_input():
    """Hand E: caller explicitly supplies TC=0; TR=2+0=2. Two good hourly
    samples give start=0, end=2, waiting=(2-2)/2=0. This is not a finding on
    contingency adequacy under DNV-ST-N001 (2023), 2.6.4.
    """
    # Synthetic arithmetic case E; no standard minimum is inferred.
    operation = InterventionTask.from_operation_periods(
        "inspect",
        "tree",
        "inspection",
        planned_steps=2,
        contingency_steps=0,
    )
    result = plan_intervention([operation], [1, 1], 2, step_hours=1, alpha=1)
    assert operation.weather_window_steps == 2
    assert result.schedule[0].start_hours == 0
    assert result.schedule[0].end_hours == 2
    assert result.waiting_on_weather_fraction == 0


def test_contingency_is_required():
    """Omitting a contingency assessment must not manufacture a numeric value."""
    # Planned count is the synthetic hand A input; TC is intentionally absent.
    with pytest.raises(TypeError, match="contingency_steps"):
        InterventionTask.from_operation_periods(
            "inspect",
            "tree",
            "inspection",
            planned_steps=2,
        )


# Invalid software-domain probes, not engineering limits. Valid planned counts
# are positive integers; contingency counts are nonnegative integers (TR sum,
# DNV-ST-N001, December 2023, 2.6.2.1; integer grid is this module's contract).
@pytest.mark.parametrize(
    "planned,contingency",
    [
        (0, 1),
        (-1, 1),
        (1.5, 1),
        (True, 1),
        (float("nan"), 1),
        (float("inf"), 1),
        (2, -1),
        (2, 1.5),
        (2, True),
        (2, float("nan")),
        (2, float("inf")),
        (2, None),
    ],
)
def test_invalid_period_counts(planned, contingency):
    """Reject counts outside the integer grid before constructing a task."""
    with pytest.raises(ValueError, match="steps"):
        InterventionTask.from_operation_periods(
            "inspect",
            "tree",
            "inspection",
            planned_steps=planned,
            contingency_steps=contingency,
        )


@pytest.mark.parametrize(
    "labels",
    [
        ("", "tree", "inspection"),
        ("inspect", "", "inspection"),
        ("inspect", "tree", ""),
    ],
)
def test_factory_validates_labels(labels):
    """Factory preserves the scheduler's nonempty-label contract."""
    # Valid periods reuse synthetic hand A, while one label is empty.
    with pytest.raises(ValueError, match="nonempty"):
        InterventionTask.from_operation_periods(
            *labels,
            planned_steps=2,
            contingency_steps=1,
        )


def test_factory_rejects_duplicate_dependencies():
    """A duplicate dependency is malformed independently of the graph."""
    # Periods reuse synthetic hand A; duplicate graph edges add no timing data.
    with pytest.raises(ValueError, match="duplicate dependency"):
        InterventionTask.from_operation_periods(
            "operate",
            "manifold",
            "valve",
            planned_steps=2,
            contingency_steps=1,
            dependencies=("inspect", "inspect"),
        )
