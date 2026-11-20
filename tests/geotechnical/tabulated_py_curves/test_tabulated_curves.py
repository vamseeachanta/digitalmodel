"""Illustrative examples; expectations are independent hand calculations."""

import pytest

from digitalmodel.geotechnical.tabulated_py_curves import ForceCurve, interpolate_force


def test_force_interpolation_preserves_softening_and_endpoints():
    curve = ForceCurve(2.0, (0.0, 0.02, 0.06), (0.0, 800.0, 400.0))
    # Straight line between the two supplied nonzero points; no monotonicity cap.
    expected_n = 800.0 + (0.04 - 0.02) * (400.0 - 800.0) / (0.06 - 0.02)
    assert interpolate_force(curve, 0.04) == pytest.approx(expected_n)
    assert interpolate_force(curve, 0.0) == 0.0
    assert interpolate_force(curve, 0.06) == 400.0


@pytest.mark.parametrize("endpoint", [2**54, float(2**54)])
def test_force_interpolation_preserves_the_last_newton(endpoint):
    extent = 2**54
    curve = ForceCurve(0, (0, endpoint), (extent, 0))
    assert interpolate_force(curve, extent - 1) == 1


def test_pressure_and_line_load_normalization_have_distinct_units():
    from digitalmodel.geotechnical.tabulated_py_curves import (
        SpringGeometry, force_from_pressure, force_from_line_resistance,
        pressure_from_force,
    )

    geometry = SpringGeometry(diameter_m=0.8, tributary_length_m=2.0)
    # Illustrative example: projected area is diameter times integration length.
    expected_n = 25000.0 * 0.8 * 2.0
    assert force_from_pressure(25000.0, geometry) == expected_n
    assert pressure_from_force(expected_n, geometry) == 25000.0
    assert force_from_line_resistance(12000.0, 2.0) == 12000.0 * 2.0


def test_integer_pressure_product_retains_the_quantization_boundary():
    from digitalmodel.geotechnical.tabulated_py_curves import (
        SpringGeometry, force_from_pressure, round_force_up,
    )

    # A unit area leaves the integer force one newton above one quantum.
    force_n = force_from_pressure(2**53 + 1, SpringGeometry(1, 1))
    assert force_n == 2**53 + 1
    assert round_force_up(force_n, 2**53) == 2**54


@pytest.mark.parametrize("exponent", [-600, 600])
def test_pressure_conversion_preserves_force_over_unit_area(exponent):
    from digitalmodel.geotechnical.tabulated_py_curves import SpringGeometry, pressure_from_force

    force_n = 2.0**exponent
    geometry = SpringGeometry(2.0**-exponent, 2.0**exponent)
    assert pressure_from_force(force_n, geometry) == force_n


def test_force_quantization_preserves_exact_boundaries():
    from math import nextafter
    from digitalmodel.geotechnical.tabulated_py_curves import round_force_up

    assert round_force_up(0.0, 0.25) == 0.0
    assert round_force_up(1.0, 0.25) == 1.0
    assert round_force_up(nextafter(1.0, 0.0), 0.25) == 1.0
    assert round_force_up(nextafter(1.0, 2.0), 0.25) == 1.25
    assert round_force_up(1.1, 0.25) == 1.25


def test_depth_interpolation_uses_one_common_displacement_grid():
    from digitalmodel.geotechnical.tabulated_py_curves import PressureCurve, interpolate_depth

    shallow = PressureCurve(1.0, (0.0, 0.04), (0.0, 20000.0))
    deep = PressureCurve(5.0, (0.0, 0.04), (0.0, 60000.0))
    result = interpolate_depth((shallow, deep), 2.0)
    expected_pa = 20000.0 + (2.0 - 1.0) * (60000.0 - 20000.0) / (5.0 - 1.0)
    assert result.depth_m == 2.0
    assert result.displacement_m == shallow.displacement_m
    assert result.pressure_pa == (0.0, expected_pa)
    assert interpolate_depth((shallow, deep), 1.0) == shallow
    assert interpolate_depth((shallow, deep), 5.0) == deep
    with pytest.raises(ValueError, match="outside"):
        interpolate_depth((shallow, deep), 6.0)


@pytest.mark.parametrize("endpoint", [2**54, float(2**54)])
def test_depth_interpolation_preserves_the_last_pascal(endpoint):
    from digitalmodel.geotechnical.tabulated_py_curves import PressureCurve, interpolate_depth

    extent = 2**54
    shallow = PressureCurve(0, (0, 1), (0, extent))
    deep = PressureCurve(endpoint, (0, 1), (0, 0))
    assert interpolate_depth((shallow, deep), extent - 1).pressure_pa == (0, 1)


def test_spring_table_is_odd_and_scales_pressure_by_projected_area():
    from digitalmodel.geotechnical.tabulated_py_curves import (
        PressureCurve, SpringGeometry, symmetric_spring_points,
    )

    curve = PressureCurve(3.0, (0.0, 0.02, 0.06), (0.0, 30000.0, 20000.0))
    geometry = SpringGeometry(0.8, 2.0)
    points = symmetric_spring_points(curve, geometry, sensitivity_ratio=0.2)
    expected_positive_n = [30000.0 * 0.8 * 2.0 * 1.2, 20000.0 * 0.8 * 2.0 * 1.2]
    assert [point.displacement_m for point in points] == [-0.06, -0.02, 0.0, 0.02, 0.06]
    assert [point.force_n for point in points] == pytest.approx(
        [-expected_positive_n[1], -expected_positive_n[0], 0.0, *expected_positive_n]
    )


@pytest.mark.parametrize("sensitivity,expected_n", [(-0.5, 2**1023), (-1, 0)])
def test_spring_sensitivity_scales_the_exact_product_before_conversion(sensitivity, expected_n):
    from math import copysign
    from digitalmodel.geotechnical.tabulated_py_curves import (
        PressureCurve, SpringGeometry, symmetric_spring_points,
    )

    # The unscaled product is 2**1024; halving it or zeroing it is finite.
    curve = PressureCurve(1, (0, 1), (0, 2**1023))
    points = symmetric_spring_points(curve, SpringGeometry(2, 1), sensitivity_ratio=sensitivity)
    assert [point.force_n for point in points] == [-expected_n, 0, expected_n]
    if sensitivity == -1:
        assert [copysign(1, point.force_n) for point in points] == [-1, 1, 1]


@pytest.mark.parametrize("exponent", [-600, 600])
def test_force_conversion_preserves_pressure_over_unit_area(exponent):
    from digitalmodel.geotechnical.tabulated_py_curves import SpringGeometry, force_from_pressure

    pressure_pa = 2.0**exponent
    geometry = SpringGeometry(2.0**exponent, 2.0**-exponent)
    # The dimensions cancel exactly, so the force has the same numerical value.
    assert force_from_pressure(pressure_pa, geometry) == pressure_pa


@pytest.mark.parametrize("exponent", [-600, 600])
def test_spring_table_preserves_pressure_over_unit_area(exponent):
    from digitalmodel.geotechnical.tabulated_py_curves import (
        PressureCurve, SpringGeometry, symmetric_spring_points,
    )

    pressure_pa = 2.0**exponent
    curve = PressureCurve(1, (0, 1), (0, pressure_pa))
    geometry = SpringGeometry(2.0**exponent, 2.0**-exponent)
    points = symmetric_spring_points(curve, geometry)
    assert [point.force_n for point in points] == [-pressure_pa, 0, pressure_pa]


@pytest.mark.parametrize("force_n,first_n,second_n,first_count,second_count", [
    (1024, 1024.0000000000002, 1024.1000000000001, 10240, 10241),
    (0.3, 0.30000000000000004, 0.4, 3, 4),
])
def test_non_dyadic_quantization_can_advance_when_repeated(
    force_n, first_n, second_n, first_count, second_count,
):
    from fractions import Fraction
    from math import inf, nextafter
    from digitalmodel.geotechnical.tabulated_py_curves import round_force_up

    # Binary64 stores 0.1 as q = 3602879701896397 / 2**55.
    # The inequalities (n-1)*q < F <= n*q select 10240, 10241, 3 and 4.
    # Each exact n*q lies above the preceding float and at or below the
    # expected float, proving that float is the least upper representation.
    quantum = Fraction(3602879701896397, 2**55)
    for original, expected, count in (
        (force_n, first_n, first_count), (first_n, second_n, second_count),
    ):
        multiple = count * quantum
        assert (count - 1) * quantum < Fraction(original) <= multiple
        assert Fraction(nextafter(expected, -inf)) < multiple <= Fraction(expected)
    # Pin the documented limitation when a rounded float is stored and rounded again.
    assert round_force_up(force_n, 0.1) == first_n
    assert round_force_up(first_n, 0.1) == second_n


def test_spring_table_mirrors_the_sign_of_zero_resistance():
    from math import copysign
    from digitalmodel.geotechnical.tabulated_py_curves import (
        PressureCurve, SpringGeometry, symmetric_spring_points,
    )

    curve = PressureCurve(1, (0, 1), (0, 1))
    points = symmetric_spring_points(curve, SpringGeometry(1.0, 1.0), sensitivity_ratio=-1.0)
    assert [point.force_n for point in points] == [0, 0, 0]
    assert [copysign(1, point.force_n) for point in points] == [-1, 1, 1]


def test_fractional_pressure_preserves_integral_force_through_quantization():
    from fractions import Fraction
    from digitalmodel.geotechnical.tabulated_py_curves import (
        SpringGeometry, force_from_pressure, round_force_up,
    )

    # (7/100) Pa * 10 m * 10 m = 7 N exactly, on a 1 N boundary.
    force_n = force_from_pressure(Fraction(7, 100), SpringGeometry(10, 10))
    assert force_n == 7
    assert round_force_up(force_n, 1) == 7


def test_fractional_sensitivity_preserves_both_signed_ordinates():
    from fractions import Fraction
    from digitalmodel.geotechnical.tabulated_py_curves import (
        PressureCurve, SpringGeometry, symmetric_spring_points,
    )

    # 1 + s = 2**-54; multiplying by 2**54 Pa on unit area gives 1 N.
    curve = PressureCurve(1, (0, 1), (0, 2**54))
    points = symmetric_spring_points(
        curve, SpringGeometry(1, 1), sensitivity_ratio=-1 + Fraction(1, 2**54),
    )
    assert [point.force_n for point in points] == [-1, 0, 1]


@pytest.mark.parametrize("length_m", [1, 1.0])
def test_integer_line_load_preserves_the_quantization_boundary(length_m):
    from digitalmodel.geotechnical.tabulated_py_curves import (
        force_from_line_resistance, round_force_up,
    )

    # A unit length leaves the force one newton above a 2**53 N quantum.
    force_n = force_from_line_resistance(2**53 + 1, length_m)
    assert force_n == 2**53 + 1
    assert round_force_up(force_n, 2**53) == 2**54


def test_pressure_ratio_survives_curve_storage_and_spring_construction():
    from fractions import Fraction
    from digitalmodel.geotechnical.tabulated_py_curves import (
        PressureCurve, SpringGeometry, force_from_pressure, round_force_up,
        symmetric_spring_points,
    )

    # Both routes represent (7/100) Pa * 10 m * 10 m = 7 N exactly.
    pressure_pa = Fraction(7, 100)
    geometry = SpringGeometry(10, 10)
    direct_n = force_from_pressure(pressure_pa, geometry)
    curve = PressureCurve(1, (0, 1), (0, pressure_pa))
    points = symmetric_spring_points(curve, geometry)
    assert curve.pressure_pa[-1] == pressure_pa
    assert direct_n == points[-1].force_n == 7
    assert points[0].force_n == -7
    assert round_force_up(direct_n, 1) == round_force_up(points[-1].force_n, 1) == 7


@pytest.mark.parametrize("coordinate", [0, 1, 2])
def test_constant_integral_force_retains_quantization_at_and_between_knots(coordinate):
    from digitalmodel.geotechnical.tabulated_py_curves import round_force_up

    force_n = 2**53 + 1
    curve = ForceCurve(1, (0, 2), (force_n, force_n))
    result = interpolate_force(curve, coordinate)
    assert result == force_n
    assert round_force_up(result, 2**53) == 2**54


@pytest.mark.parametrize("depth_m", [0, 1, 2])
def test_constant_integral_pressure_retains_quantization_between_depths(depth_m):
    from digitalmodel.geotechnical.tabulated_py_curves import (
        PressureCurve, SpringGeometry, force_from_pressure, interpolate_depth,
        round_force_up, symmetric_spring_points,
    )

    pressure_pa = 2**53 + 1
    curves = tuple(PressureCurve(z, (0, 1), (0, pressure_pa)) for z in (0, 2))
    result = interpolate_depth(curves, depth_m)
    assert result.pressure_pa[-1] == pressure_pa
    geometry = SpringGeometry(1, 1)
    force_n = force_from_pressure(result.pressure_pa[-1], geometry)
    points = symmetric_spring_points(result, geometry)
    assert force_n == points[-1].force_n == pressure_pa
    assert round_force_up(points[-1].force_n, 2**53) == 2**54


@pytest.mark.parametrize("case", ["subnormal_force", "ordinary_ratio", "integral_pressure"])
def test_inverse_conversion_divides_the_original_force_ratio(case):
    from fractions import Fraction
    from digitalmodel.geotechnical.tabulated_py_curves import SpringGeometry, pressure_from_force

    force_n, area_m2, expected_pa = {
        "subnormal_force": (Fraction(1, 2**1075), 2.0**-1022, 2.0**-53),
        "ordinary_ratio": (Fraction(3, 5), 3, float(Fraction(1, 5))),
        "integral_pressure": (2**53 + 1, 1, 2**53 + 1),
    }[case]
    assert pressure_from_force(force_n, SpringGeometry(area_m2, 1)) == expected_pa
