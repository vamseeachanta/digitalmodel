# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic geometry tests; dimensions are caller inputs, not standard values."""

from dataclasses import replace

import pytest

from digitalmodel.subsea.rov.interface_checks import (
    InterfaceKind, InterfaceSpec, ToolInterface, check_interface,
)


def pair():
    return (
        ToolInterface(InterfaceKind.TORQUE, "1", (0.2, 0.3, 0.4), 0.5),
        InterfaceSpec(InterfaceKind.TORQUE, "1", (0.3, 0.4, 0.5), 0.6,
                      (3.0, 4.0, 0.0)),
    )


def test_matching_interface_at_reach_boundary():
    tool, target = pair()
    result = check_interface(tool, target, manipulator_reach_m=5.0)
    assert result.compatible
    assert result.reasons == ()


@pytest.mark.parametrize("changes,reason", [
    ({"kind": InterfaceKind.HOT_STAB}, "kind"),
    ({"class_id": "2"}, "class"),
    ({"envelope_m": (0.1, 0.4, 0.5)}, "envelope"),
    ({"approach_clearance_m": 0.4}, "approach"),
    ({"location_m": (6.0, 0.0, 0.0)}, "reach"),
])
def test_mismatch_category(changes, reason):
    tool, target = pair()
    result = check_interface(tool, replace(target, **changes), 5.0)
    assert not result.compatible
    assert result.reasons == (reason,)


def test_all_failures_are_retained():
    tool, target = pair()
    result = check_interface(tool, replace(target, class_id="7",
                             approach_clearance_m=0.1), 1.0)
    assert result.reasons == ("class", "approach", "reach")


@pytest.mark.parametrize("value", [float("nan"), float("inf")])
def test_invalid_geometry_rejected(value):
    tool, target = pair()
    with pytest.raises(ValueError):
        check_interface(tool, replace(target, location_m=(value, 0, 0)), 5.0)
    with pytest.raises(ValueError):
        check_interface(tool, target, value)


def test_signed_coordinates_and_origin_are_valid():
    tool, target = pair()
    assert check_interface(tool, replace(target, location_m=(-3, -4, 0)), 5).compatible
    assert check_interface(tool, replace(target, location_m=(0, 0, 0)), 0).compatible


def test_invalid_shape_and_empty_class_rejected():
    tool, target = pair()
    with pytest.raises(ValueError):
        check_interface(tool, replace(target, envelope_m=(1, 2)), 5)
    with pytest.raises(ValueError):
        check_interface(replace(tool, class_id=""), target, 5)
