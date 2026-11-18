"""Illustrative examples for rejected or bounded tabulated-curve inputs."""

from dataclasses import FrozenInstanceError
from fractions import Fraction
from math import inf, nan

import numpy as np
import pytest

from digitalmodel.geotechnical.tabulated_py_curves import (
    ForceCurve, PressureCurve, SpringGeometry, SpringPoint, force_from_pressure,
    force_from_line_resistance, pressure_from_force, interpolate_depth,
    interpolate_force, round_force_up, symmetric_spring_points,
)


@pytest.mark.parametrize("value", [nan, inf, -inf, -1.0, True, "2"])
def test_invalid_curve_values_are_rejected(value):
    with pytest.raises(ValueError):
        ForceCurve(value, (0.0, 0.1), (0.0, 20.0))
    with pytest.raises(ValueError):
        PressureCurve(1.0, (0.0, 0.1), (0.0, value))


@pytest.mark.parametrize("axis,forces", [
    ((0.0,), (0.0,)), ((0.0, 0.0), (0.0, 1.0)),
    ((0.1, 0.0), (1.0, 0.0)), ((0.0, nan), (0.0, 1.0)),
    ((0.0, 0.1), (0.0,)), ((-0.1, 0.0), (1.0, 0.0)),
])
def test_invalid_curve_axes_are_rejected(axis, forces):
    with pytest.raises(ValueError):
        ForceCurve(1.0, axis, forces)


def test_curve_storage_is_immutable_and_does_not_alias_lists():
    axis, forces = [0.0, 0.1], [0.0, 200.0]
    curve = ForceCurve(1.0, axis, forces)
    axis[1], forces[1] = 0.2, 300.0
    assert curve.displacement_m == (0.0, 0.1)
    assert curve.force_n == (0.0, 200.0)
    with pytest.raises(FrozenInstanceError):
        curve.depth_m = 2.0


@pytest.mark.parametrize("value", [0.0, -1.0, inf, nan, True])
def test_geometry_and_quantum_require_finite_positive_values(value):
    with pytest.raises(ValueError):
        SpringGeometry(value, 2.0)
    with pytest.raises(ValueError):
        SpringGeometry(0.8, value)
    with pytest.raises(ValueError):
        round_force_up(10.0, value)
    with pytest.raises(ValueError):
        force_from_line_resistance(10.0, value)


@pytest.mark.parametrize("value", [-1.0, nan, inf])
def test_negative_or_nonfinite_loads_are_rejected(value):
    geometry = SpringGeometry(0.8, 2.0)
    for function in (force_from_pressure, pressure_from_force):
        with pytest.raises(ValueError):
            function(value, geometry)
    with pytest.raises(ValueError):
        round_force_up(value, 0.25)


@pytest.mark.parametrize("query", [-0.01, 0.11, inf, nan])
def test_displacement_extrapolation_is_rejected(query):
    with pytest.raises(ValueError):
        interpolate_force(ForceCurve(1.0, (0.0, 0.1), (0.0, 20.0)), query)


def test_depth_grid_and_order_must_be_unambiguous():
    first = PressureCurve(1.0, (0.0, 0.1), (0.0, 1000.0))
    other = PressureCurve(3.0, (0.0, 0.2), (0.0, 2000.0))
    for curves in ((), (first, first), (other, first), (first, other)):
        with pytest.raises(ValueError):
            interpolate_depth(curves, 2.0)
    assert interpolate_depth((first,), 1.0) == first


def test_spring_requires_explicit_origin_and_nonnegative_sensitivity_multiplier():
    geometry = SpringGeometry(0.8, 2.0)
    with pytest.raises(ValueError, match="origin"):
        symmetric_spring_points(PressureCurve(1.0, (0.01, 0.02), (1.0, 2.0)), geometry)
    origin = PressureCurve(1.0, (0.0, 0.02), (0.0, 2000.0))
    for ratio in (nan, inf, -1.1):
        with pytest.raises(ValueError):
            symmetric_spring_points(origin, geometry, sensitivity_ratio=ratio)
    assert all(p.force_n == 0 for p in symmetric_spring_points(
        origin, geometry, sensitivity_ratio=-1.0
    ))


def test_nonfinite_computed_outputs_are_rejected():
    with pytest.raises(ValueError):
        force_from_pressure(1e308, SpringGeometry(10.0, 10.0))
    with pytest.raises(ValueError):
        force_from_line_resistance(1e308, 10.0)
    with pytest.raises(ValueError):
        SpringPoint(inf, 1.0)


def test_interpolation_avoids_overflow_in_a_finite_linear_result():
    curve = ForceCurve(1.0, (0.0, 1e100), (0.0, 1e308))
    assert interpolate_force(curve, 5e99) == pytest.approx(5e307)


@pytest.mark.parametrize("force_n,quantum_n,expected_n", [
    (1, 0.25, 1.0),
    (Fraction(11, 10), 0.25, 1.25),
    (np.int64(1), 0.25, 1.0),
    (np.int64(1024), 0.1, 1024.0),
    (np.int32(1024), 0.1, 1024.0),
    (1.1, np.int64(1), 2.0),
    (np.float32(1.5), 0.25, 1.5),
    (1.1, np.float32(0.5), 1.5),
])
def test_quantization_accepts_integer_and_fraction_inputs(force_n, quantum_n, expected_n):
    assert round_force_up(force_n, quantum_n) == expected_n


def test_quantization_handles_small_quanta_and_rejects_output_overflow():
    assert round_force_up(1.0, 5e-324) == 1.0
    with pytest.raises(ValueError):
        round_force_up(1.7e308, 1e308)
