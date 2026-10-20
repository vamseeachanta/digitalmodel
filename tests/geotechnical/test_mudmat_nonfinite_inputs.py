# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Non-finite input rejection and optional-input regression pins.

NaN and both infinities are non-finite by Python 3.11 Library Reference,
math.isfinite: https://docs.python.org/3.11/library/math.html#math.isfinite.
Fixture dimensions and loads use the synthetic reproduction in issue #2105;
soil inputs use issue #2330 and the existing optional-input regression pins.
These are test inputs, not calibrated engineering coefficients or limits.
"""

import math
from copy import deepcopy

import pytest

from digitalmodel.geotechnical.mudmat import (
    bearing_capacity_factors,
    mudmat_bearing_capacity,
)


NUMERIC_ARGUMENTS = (
    "width_b_m",
    "length_l_m",
    "embedment_depth_m",
    "submerged_unit_weight_kn_m3",
    "vertical_load_kn",
    "moment_knm",
    "undrained_shear_strength_kpa",
    "friction_angle_deg",
    "effective_cohesion_kpa",
    "interface_friction_angle_deg",
)
BASE_REJECTION_PINS = {
    ("width_b_m", "inf"),
    ("embedment_depth_m", "-inf"),
    ("vertical_load_kn", "-inf"),
}


def calculate(**overrides):
    inputs = dict(
        width_b_m=3.0,
        length_l_m=4.0,
        embedment_depth_m=0.0,
        condition="undrained",
        submerged_unit_weight_kn_m3=8.0,
        vertical_load_kn=800.0,
        moment_knm=0.0,
        undrained_shear_strength_kpa=25.0,
    )
    return mudmat_bearing_capacity(**(inputs | overrides))


def test_nan_moment_reproduction_is_rejected():
    with pytest.raises(ValueError, match="moment_knm"):
        calculate(moment_knm=float("nan"))


@pytest.mark.parametrize(
    "argument,value",
    [
        pytest.param(
            argument,
            float(label),
            id=f"{argument}-{label}"
            + ("-regression-pin" if (argument, label) in BASE_REJECTION_PINS else ""),
        )
        for argument in NUMERIC_ARGUMENTS
        for label in ("nan", "inf", "-inf")
    ],
)
@pytest.mark.parametrize("condition", ["undrained", "drained"])
@pytest.mark.parametrize("axis", ["B", "L"])
def test_nonfinite_numeric_argument_names_offender(argument, value, condition, axis):
    # Include unused soil fields: finiteness is independent of the chosen branch.
    overrides = dict(
        condition=condition, eccentricity_axis=axis, friction_angle_deg=30.0
    )
    overrides[argument] = value
    with pytest.raises(ValueError, match=argument):
        calculate(**overrides)


@pytest.mark.parametrize("label", ["nan", "inf", "-inf"])
def test_nonfinite_capacity_factor_angle_names_offender(label):
    with pytest.raises(ValueError, match="phi_deg"):
        bearing_capacity_factors(float(label))


@pytest.mark.parametrize("argument", NUMERIC_ARGUMENTS)
@pytest.mark.parametrize("value", [False, True])
@pytest.mark.parametrize("condition", ["undrained", "drained"])
def test_boolean_numeric_argument_is_rejected(argument, value, condition):
    overrides = dict(condition=condition, friction_angle_deg=30.0)
    overrides[argument] = value
    with pytest.raises(ValueError, match=argument):
        calculate(**overrides)


@pytest.mark.parametrize("value", [False, True])
def test_boolean_capacity_factor_angle_is_rejected(value):
    with pytest.raises(ValueError, match="phi_deg"):
        bearing_capacity_factors(value)


@pytest.mark.parametrize("condition", ["undrained", "drained"])
def test_optional_none_arguments_regression_pin(condition):
    """No moment: hand-calculated area is B*L = 3*4 = 12 square metres.

    Area: FHWA NHI-06-089 (2006), Section 8.4.3.1, Equation 8-9.
    Unused optional soil inputs and the default interface angle accept None.
    """
    result = calculate(
        condition=condition,
        undrained_shear_strength_kpa=25.0 if condition == "undrained" else None,
        friction_angle_deg=30.0 if condition == "drained" else None,
        interface_friction_angle_deg=None,
    )
    assert result.effective_area_m2 == 12.0
    assert math.isfinite(result.vertical_capacity_kn)
    assert math.isfinite(result.sliding_capacity_kn)


CONFIG_ARGUMENTS = (
    ("foundation", "width_B_m", "width_b_m"),
    ("foundation", "length_L_m", "length_l_m"),
    ("foundation", "embedment_depth_m", "embedment_depth_m"),
    ("soil", "submerged_unit_weight_kN_m3", "submerged_unit_weight_kn_m3"),
    ("loads", "vertical_kN", "vertical_load_kn"),
    ("loads", "moment_kNm", "moment_knm"),
    ("soil", "undrained_shear_strength_kpa", "undrained_shear_strength_kpa"),
    ("soil", "friction_angle_deg", "friction_angle_deg"),
    ("soil", "effective_cohesion_kpa", "effective_cohesion_kpa"),
    ("soil", "interface_friction_angle_deg", "interface_friction_angle_deg"),
)


@pytest.mark.parametrize("section,key,argument", CONFIG_ARGUMENTS)
@pytest.mark.parametrize("label", ["nan", "inf", "-inf"])
@pytest.mark.parametrize("condition", ["undrained", "drained"])
def test_config_nonfinite_strings_rejected_before_output(
    tmp_path, section, key, argument, label, condition
):
    """Text numeric inputs must fail before a summary or a result is published."""
    from digitalmodel.geotechnical.cfg_workflows import MudmatBearingCapacityWorkflow

    cfg = {
        "_config_dir_path": str(tmp_path),
        "mudmat_bearing_capacity": {
            "foundation": {"width_B_m": 3.0, "length_L_m": 4.0},
            "soil": {
                "condition": condition,
                "submerged_unit_weight_kN_m3": 8.0,
                "undrained_shear_strength_kpa": 25.0,
                "friction_angle_deg": 30.0,
            },
            "loads": {"vertical_kN": 800.0, "moment_kNm": 400.0},
        },
    }
    cfg["mudmat_bearing_capacity"][section][key] = label
    original = deepcopy(cfg)

    with pytest.raises(ValueError, match=argument):
        MudmatBearingCapacityWorkflow().router(cfg)

    assert cfg == original
    assert list(tmp_path.iterdir()) == []
