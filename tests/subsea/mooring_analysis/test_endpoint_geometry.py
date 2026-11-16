"""Illustrative example endpoints and independently derived chord lengths."""

from dataclasses import FrozenInstanceError, replace
from math import sqrt

import numpy as np
import pytest

from digitalmodel.subsea.mooring_analysis.dynmoor import DynmoorLine, anchor_position
from digitalmodel.subsea.mooring_analysis.endpoint_geometry import (
    EndpointInput,
    mooring_endpoint_geometry,
)


def inputs(**changes):
    return EndpointInput(**(dict(fairlead_x_m=2.0, fairlead_y_m=3.0,
                                fairlead_z_m=-2.0, horizontal_span_m=12.0,
                                bearing_deg=0.0, anchor_z_m=-7.0,
                                reflect_x=False, reflect_y=False) | changes))


def test_existing_anchor_kernel_axis_case():
    line = DynmoorLine(1, 2.0, 3.0, 0.0, 0.0)
    assert anchor_position(line, 12.0) == (14.0, 3.0)


def test_endpoint_adapter_three_dimensional_chord():
    result = mooring_endpoint_geometry(inputs())
    assert result.anchor_x_m == 14.0
    assert result.anchor_y_m == pytest.approx(3.0)
    assert result.anchor_z_m == -7.0
    assert result.chord_length_m == pytest.approx(13.0)  # 5-12-13 triangle.


def test_endpoint_adapter_accepts_numpy_integer_inputs():
    result = mooring_endpoint_geometry(inputs(horizontal_span_m=np.int64(12)))
    assert result.anchor_x_m == pytest.approx(14.0)
    assert result.chord_length_m == pytest.approx(13.0)


def test_reflection_applies_to_translated_coordinate():
    result = mooring_endpoint_geometry(inputs(reflect_x=True, reflect_y=True))
    assert result.anchor_x_m == pytest.approx(-14.0)
    assert result.anchor_y_m == pytest.approx(-3.0)
    assert result.chord_length_m == pytest.approx(sqrt(16**2 + 6**2 + 5**2))


def test_azimuth_and_zero_span_limits():
    result = mooring_endpoint_geometry(inputs(bearing_deg=90.0))
    assert result.anchor_x_m == pytest.approx(2.0)
    assert result.anchor_y_m == pytest.approx(15.0)
    assert result.chord_length_m == pytest.approx(13.0)
    vertical = mooring_endpoint_geometry(inputs(horizontal_span_m=0.0))
    assert vertical.chord_length_m == 5.0
    coincident = mooring_endpoint_geometry(inputs(horizontal_span_m=0.0, anchor_z_m=-2.0))
    assert coincident.chord_length_m == 0.0


@pytest.mark.parametrize("bearing", [-270.0, 450.0])
def test_bearings_can_wrap(bearing):
    a = mooring_endpoint_geometry(inputs(bearing_deg=bearing))
    b = mooring_endpoint_geometry(inputs(bearing_deg=90.0))
    assert a.anchor_x_m == pytest.approx(b.anchor_x_m)
    assert a.anchor_y_m == pytest.approx(b.anchor_y_m)


@pytest.mark.parametrize("field", ["fairlead_x_m", "fairlead_y_m", "fairlead_z_m",
                                  "horizontal_span_m", "bearing_deg", "anchor_z_m"])
@pytest.mark.parametrize("value", [float("nan"), float("inf"), True, "3"])
def test_nonfinite_or_nonnumeric_inputs_rejected(field, value):
    with pytest.raises(ValueError):
        inputs(**{field: value})


def test_invalid_span_and_reflection_flags_rejected():
    with pytest.raises(ValueError):
        inputs(horizontal_span_m=-1.0)
    with pytest.raises(ValueError):
        inputs(reflect_x=1)
    with pytest.raises(ValueError):
        inputs(reflect_y="yes")
    with pytest.raises(FrozenInstanceError):
        inputs().bearing_deg = 90.0
