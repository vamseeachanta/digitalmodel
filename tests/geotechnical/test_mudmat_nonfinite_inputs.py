"""Non-finite input rejection and optional-input regression pins."""

import math

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
DRAINED_ARGUMENTS = (
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
def test_nonfinite_numeric_argument_names_offender(argument, value):
    overrides = {argument: value}
    if argument in DRAINED_ARGUMENTS:
        overrides = dict(condition="drained", friction_angle_deg=30.0) | overrides
    with pytest.raises(ValueError, match=argument):
        calculate(**overrides)


@pytest.mark.parametrize("label", ["nan", "inf", "-inf"])
def test_nonfinite_capacity_factor_angle_names_offender(label):
    with pytest.raises(ValueError, match="phi_deg"):
        bearing_capacity_factors(float(label))


@pytest.mark.parametrize("argument", NUMERIC_ARGUMENTS)
def test_boolean_numeric_argument_is_rejected(argument):
    overrides = {argument: True}
    if argument in DRAINED_ARGUMENTS:
        overrides = dict(condition="drained", friction_angle_deg=30.0) | overrides
    with pytest.raises(ValueError, match=argument):
        calculate(**overrides)


@pytest.mark.parametrize("value", [False, True])
def test_boolean_capacity_factor_angle_is_rejected(value):
    with pytest.raises(ValueError, match="phi_deg"):
        bearing_capacity_factors(value)


@pytest.mark.parametrize(
    "condition,argument",
    [
        ("drained", "undrained_shear_strength_kpa"),
        ("undrained", "friction_angle_deg"),
        ("undrained", "effective_cohesion_kpa"),
        ("undrained", "interface_friction_angle_deg"),
    ],
)
def test_unused_nonfinite_soil_argument_is_rejected(condition, argument):
    overrides = dict(condition=condition, friction_angle_deg=30.0)
    overrides[argument] = float("nan")
    with pytest.raises(ValueError, match=argument):
        calculate(**overrides)


@pytest.mark.parametrize("condition", ["undrained", "drained"])
def test_optional_none_arguments_regression_pin(condition):
    """Regression pin: unused soil inputs and default interface angle accept None."""
    result = calculate(
        condition=condition,
        undrained_shear_strength_kpa=25.0 if condition == "undrained" else None,
        friction_angle_deg=30.0 if condition == "drained" else None,
        interface_friction_angle_deg=None,
    )
    assert result.effective_area_m2 == 12.0
    assert math.isfinite(result.vertical_capacity_kn)
    assert math.isfinite(result.sliding_capacity_kn)
