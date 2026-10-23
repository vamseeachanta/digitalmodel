# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent planning examples; operational approval is NOT_EVALUATED."""

import pytest

from digitalmodel.subsea.rov import (
    InterfaceKind,
    InterfaceSpec,
    InterventionTask,
    ToolInterface,
    check_interface,
    plan_intervention,
    scatter_operability,
    seastate_operability,
)


def test_seastate_literals():
    """Synthetic limit=4 m, alpha=0.5 -> operational limit=2 m.

    Hs=1 m -> U=1/2=0.5, margin=2-1=1 m. The allowance is a supplied
    input; API 17H/IMCA R 004/DNV-ST-N001 clause basis NOT_EVALUATED.
    """
    result = seastate_operability(1, 4, 0.5)
    assert result.hs_oplim == pytest.approx(2.0)
    assert result.utilisation == pytest.approx(0.5)
    assert result.margin == pytest.approx(1.0)
    assert result.operable


def test_scatter_literal():
    """Independent weighted count: Hs=(1,2,3), weights=(2,3,5).

    Synthetic limit=2 m includes equality: 100*(2+3)/(2+3+5)=50 percent.
    This is occurrence weighting, not a continuous weather-window probability.
    """
    result = scatter_operability([
        {"hs": 1, "probability": 2}, {"hs": 2, "probability": 3},
        {"hs": 3, "probability": 5},
    ], 2)
    assert result.operability_pct == pytest.approx(50.0)
    assert result.n_operable == 2
    assert result.n_total == 3


def test_reach_boundary_literal():
    """Independent Euclidean distance: sqrt(3^2+4^2+0^2)=5 m.

    Synthetic equal boxes and approach spaces fit; reach=5 accepts equality,
    reach=4 fails only reach. API interface certification NOT_EVALUATED.
    """
    tool = ToolInterface(InterfaceKind.DOCKING, "synthetic", (1, 1, 1), 1)
    target = InterfaceSpec(InterfaceKind.DOCKING, "synthetic", (1, 1, 1), 1, (3, 4, 0))
    assert check_interface(tool, target, 5).compatible
    assert check_interface(tool, target, 4).reasons == ("reach",)


def test_serial_weather_literals():
    """Independent interval enumeration, synthetic half-hour samples.

    Hs=(3,1,1,1), limit=2 m -> mask=(False,True,True,True).
    Task a duration/window=1/2 starts sample 1, ends 2; dependent b=1/1
    starts 2, ends 3. Starts=(0.5,1) h, ends=(1,1.5) h;
    elapsed=1.5 h, idle=0.5 h -> waiting fraction=1/3.
    Longer windows reserve forecast cover without occupying the ROV.
    """
    result = plan_intervention([
        InterventionTask("a", "tool", "port", 1, 2),
        InterventionTask("b", "tool", "port", 1, 1, ("a",)),
    ], [3, 1, 1, 1], 2, step_hours=0.5)
    assert [task.start_hours for task in result.schedule] == [0.5, 1.0]
    assert [task.end_hours for task in result.schedule] == [1.0, 1.5]
    assert result.campaign_duration_hours == pytest.approx(1.5)
    assert result.waiting_on_weather_fraction == pytest.approx(0.3333333333333333)
    assert result.complete
