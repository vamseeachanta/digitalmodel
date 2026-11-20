"""Illustrative examples for rejected or bounded tabulated-curve inputs."""

from dataclasses import FrozenInstanceError
from fractions import Fraction
from math import inf, nan, nextafter

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


@pytest.mark.parametrize("scalar", [
    Fraction,
    pytest.param(np.longdouble, marks=pytest.mark.skipif(
        np.finfo(np.longdouble).nmant <= np.finfo(float).nmant,
        reason="requires more significand bits than binary64",
    )),
])
def test_force_interpolation_preserves_a_coordinate_between_adjacent_floats(scalar):
    # The endpoints are 1 and 1 + 2**-52; their midpoint is 1 + 2**-53.
    midpoint = scalar(1) + scalar(1) / 2**53
    curve = ForceCurve(1, (1, 1 + 2**-52), (0, 100))
    assert interpolate_force(curve, midpoint) == 50


@pytest.mark.parametrize("scalar", [
    Fraction,
    pytest.param(np.longdouble, marks=pytest.mark.skipif(
        np.finfo(np.longdouble).nmant <= np.finfo(float).nmant,
        reason="requires more significand bits than binary64",
    )),
])
def test_depth_interpolation_preserves_a_coordinate_between_adjacent_floats(scalar):
    midpoint = scalar(1) + scalar(1) / 2**53
    shallow = PressureCurve(1, (0, 1), (0, 0))
    deep = PressureCurve(1 + 2**-52, (0, 1), (0, 100))
    # Equal distances from 0 Pa and 100 Pa give 50 Pa on the common grid.
    assert interpolate_depth((shallow, deep), midpoint).pressure_pa == (0, 50)


@pytest.mark.parametrize("scalar", [
    Fraction,
    pytest.param(np.longdouble, marks=pytest.mark.skipif(
        np.finfo(np.longdouble).nmant <= np.finfo(float).nmant,
        reason="requires more significand bits than binary64",
    )),
])
def test_single_depth_rejects_an_exact_coordinate_above_the_only_curve(scalar):
    depth = scalar(1) + scalar(1) / 2**54
    curve = PressureCurve(1, (0, 1), (0, 100))
    with pytest.raises(ValueError, match="outside"):
        interpolate_depth((curve,), depth)


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


@pytest.mark.parametrize("displacement_m, force_n", [
    (0.1, -20), (-0.1, 20), (Fraction(1, 2**1100), -1),
])
def test_spring_point_rejects_opposite_nonzero_signs(displacement_m, force_n):
    with pytest.raises(ValueError, match="sign"):
        SpringPoint(displacement_m, force_n)


@pytest.mark.parametrize("displacement_m", [-0.1, 0.1])
def test_spring_point_accepts_zero_resistance(displacement_m):
    assert SpringPoint(displacement_m, 0).force_n == 0


def test_nonfinite_computed_outputs_are_rejected():
    with pytest.raises(ValueError):
        force_from_pressure(1e308, SpringGeometry(10.0, 10.0))
    with pytest.raises(ValueError):
        force_from_line_resistance(1e308, 10.0)
    with pytest.raises(ValueError, match="computed pressure_pa"):
        pressure_from_force(2.0**1000, SpringGeometry(1, 2.0**-1000))
    with pytest.raises(ValueError):
        SpringPoint(inf, 1.0)


def test_interpolation_avoids_overflow_in_a_finite_linear_result():
    curve = ForceCurve(1.0, (0.0, 1e100), (0.0, 1e308))
    assert interpolate_force(curve, 5e99) == pytest.approx(5e307)


@pytest.mark.parametrize("force_n,quantum_n,expected_n", [
    (1, 0.25, 1.0),
    (Fraction(11, 10), 0.25, 1.25),
    (Fraction(2**53 + 1, 2**53), 1, 2.0),
    (np.int64(2**53 + 1), np.int64(2**53), float(2**54)),
    (np.int64(1), 0.25, 1.0),
    (np.int64(1024), 0.1, nextafter(1024.0, inf)),
    (np.int32(1024), 0.1, nextafter(1024.0, inf)),
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


@pytest.mark.skipif(np.finfo(np.longdouble).nmant <= np.finfo(float).nmant,
                    reason="requires more significand bits than binary64")
def test_quantization_uses_the_extended_precision_force():
    force_n = np.nextafter(np.longdouble(1), np.longdouble(2))
    assert round_force_up(force_n, 1) == 2.0


@pytest.mark.skipif(np.finfo(np.longdouble).minexp >= np.finfo(float).minexp,
                    reason="requires a wider exponent range than binary64")
def test_quantization_preserves_a_positive_quantum_below_binary64():
    quantum_n = np.ldexp(np.longdouble(1), -1075)
    # One newton is an exact multiple of this power of two.
    assert round_force_up(1, quantum_n) == 1.0


@pytest.mark.parametrize("scalar", [np.uint64, np.int64, np.float32])
def test_numpy_force_softening_uses_signed_arithmetic(scalar):
    curve = ForceCurve(2.0, (0.0, 0.02, 0.06), tuple(map(scalar, (0, 800, 400))))
    # Halfway from 800 N down to 400 N is 600 N.
    assert interpolate_force(curve, 0.04) == pytest.approx(600.0)


def test_numpy_depth_softening_and_unsigned_spring_coordinates():
    shallow = PressureCurve(np.uint64(1), (0, 2), (np.uint64(0), np.uint64(800)))
    deep = PressureCurve(np.uint64(5), (0, 2), (np.uint64(0), np.uint64(400)))
    result = interpolate_depth((shallow, deep), np.uint64(3))
    assert result.pressure_pa == pytest.approx((0.0, 600.0))
    points = symmetric_spring_points(shallow, SpringGeometry(1, 1))
    assert [point.displacement_m for point in points] == [-2, 0, 2]
    assert [point.force_n for point in points] == [-800, 0, 800]


@pytest.mark.parametrize("scalar", [np.int32, np.int64, np.uint64, np.float32])
def test_numpy_unit_conversions_do_not_use_fixed_width_products(scalar):
    # 3,000,000 N/m times 2,000 m = 6,000,000,000 N.
    assert force_from_line_resistance(scalar(3000000), scalar(2000)) == 6000000000.0


@pytest.mark.parametrize("scalar", [np.int32, np.int64, np.uint64, np.float32])
def test_numpy_pressure_conversions_do_not_use_fixed_width_products(scalar):
    # 3,000,000 Pa times 2,000 m times 2 m = 12,000,000,000 N.
    geometry = SpringGeometry(scalar(2000), scalar(2))
    assert force_from_pressure(scalar(3000000), geometry) == 12000000000.0
    assert pressure_from_force(scalar(12000000), geometry) == 3000.0


def test_numpy_unsigned_displacements_are_mirrored_after_normalization():
    curve = PressureCurve(1, (np.uint64(0), np.uint64(2)), (0, 10))
    points = symmetric_spring_points(curve, SpringGeometry(1, 1))
    assert [point.displacement_m for point in points] == [-2, 0, 2]


def test_numpy_sensitivity_multiplier_does_not_wrap():
    curve = PressureCurve(1, (0, 2), (0, 1))
    ratio = np.int64(np.iinfo(np.int64).max)
    points = symmetric_spring_points(curve, SpringGeometry(1, 1), sensitivity_ratio=ratio)
    assert points[-1].force_n == 2**63


@pytest.mark.parametrize("scalar", [int, np.int64, np.uint64, Fraction])
def test_quantization_never_rounds_below_large_exact_integer(scalar):
    force_n = scalar(2**53 + 1)
    result = round_force_up(force_n, 1)
    # The exact input lies halfway between adjacent representable doubles.
    assert result == float(2**53 + 2)
    assert Fraction(result) >= 2**53 + 1


def test_quantization_never_rounds_below_exact_decimal_quantum_multiple():
    result = round_force_up(1024, 0.1)
    # The selected multiple is 10240 * the exact binary value of 0.1 N.
    assert Fraction(result) >= 10240 * Fraction(0.1)


def test_integer_force_product_beyond_double_range_raises_value_error():
    with pytest.raises(ValueError, match="computed force_n"):
        force_from_line_resistance(10**200, 10**200)


@pytest.mark.parametrize("operation", [
    lambda value: ForceCurve(value, (0, 1), (0, 1)),
    lambda value: round_force_up(value, 1),
    lambda value: SpringGeometry(value, 1),
    lambda value: SpringPoint(value, 1),
    lambda value: SpringPoint(-1, -value),
])
def test_integer_input_beyond_double_range_raises_value_error(operation):
    with pytest.raises(ValueError):
        operation(10**400)


@pytest.mark.parametrize("operation,coordinate", [
    (lambda value: ForceCurve(value, (0, 1), (0, 1)), "depth_m"),
    (lambda value: SpringPoint(value, 1), "displacement_m"),
    (lambda value: SpringPoint(-1, -value), "force_n"),
])
def test_extended_precision_storage_remains_finite_after_conversion(operation, coordinate):
    stored = getattr(operation(np.longdouble(1.5)), coordinate)
    assert type(stored) is float
    assert abs(stored) == 1.5
    value = np.finfo(np.longdouble).max
    if value > np.finfo(float).max:
        with pytest.raises(ValueError):
            operation(value)
    else:
        point = operation(value)
        stored = getattr(point, coordinate)
        assert type(stored) is float
        assert np.isfinite(stored)
