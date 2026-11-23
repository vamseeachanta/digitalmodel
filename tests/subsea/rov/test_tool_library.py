# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Caller-supplied capability lookup using synthetic tool ratings."""

from dataclasses import replace

import pytest

from digitalmodel.subsea.rov.interface_checks import InterfaceKind, ToolInterface
from digitalmodel.subsea.rov.tool_library import ToolKind, ToolSpec, TaskDuty, find_tools


def tool():
    interface = ToolInterface(InterfaceKind.TORQUE, "1", (1, 1, 1), 1)
    return ToolSpec("synthetic", ToolKind.TORQUE, interface, torque_nm=20, turns=5)


def test_capacity_boundary_and_interface_are_returned():
    candidate = tool()
    assert find_tools([candidate], TaskDuty(ToolKind.TORQUE, torque_nm=20, turns=5)) == (candidate,)
    assert candidate.interface.class_id == "1"


def test_no_tool_can_satisfy_duty():
    assert find_tools([tool()], TaskDuty(ToolKind.TORQUE, torque_nm=21)) == ()
    assert find_tools([], TaskDuty(ToolKind.TORQUE)) == ()


def test_missing_capacity_does_not_mean_unlimited():
    assert find_tools([tool()], TaskDuty(ToolKind.TORQUE, pressure_pa=1)) == ()


def test_wrong_tool_kind_is_excluded():
    assert find_tools([tool()], TaskDuty(ToolKind.CUTTING)) == ()


@pytest.mark.parametrize("kind,capacity", [
    (ToolKind.HOT_STAB, {"flow_m3_s": 2, "pressure_pa": 3}),
    (ToolKind.PULL, {"line_pull_n": 4}),
    (ToolKind.CUTTING, {}), (ToolKind.CLEANING, {}),
])
def test_other_tool_types(kind, capacity):
    candidate = replace(tool(), kind=kind, **capacity)
    assert find_tools([candidate], TaskDuty(kind, **capacity)) == (candidate,)


@pytest.mark.parametrize("bad", [-1, float("nan"), float("inf")])
def test_invalid_capacities_raise(bad):
    with pytest.raises(ValueError):
        find_tools([tool()], TaskDuty(ToolKind.TORQUE, torque_nm=bad))
    with pytest.raises(ValueError):
        find_tools([replace(tool(), torque_nm=bad)], TaskDuty(ToolKind.TORQUE))


@pytest.mark.parametrize("bad", [-1, float("nan"), float("inf")])
def test_invalid_tool_approach_is_rejected(bad):
    candidate = tool()
    candidate = replace(candidate, interface=replace(candidate.interface,
                                                   required_approach_m=bad))
    with pytest.raises(ValueError):
        find_tools([candidate], TaskDuty(ToolKind.TORQUE))
