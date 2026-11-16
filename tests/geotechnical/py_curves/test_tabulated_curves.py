"""Illustrative examples; expectations are independent hand calculations."""

import pytest

from digitalmodel.geotechnical.py_curves import ForceCurve, interpolate_force


def test_force_interpolation_preserves_softening_and_endpoints():
    curve = ForceCurve(2.0, (0.0, 0.02, 0.06), (0.0, 800.0, 400.0))
    # Straight line between the two supplied nonzero points; no monotonicity cap.
    expected_n = 800.0 + (0.04 - 0.02) * (400.0 - 800.0) / (0.06 - 0.02)
    assert interpolate_force(curve, 0.04) == pytest.approx(expected_n)
    assert interpolate_force(curve, 0.0) == 0.0
    assert interpolate_force(curve, 0.06) == 400.0


def test_pressure_and_line_load_normalization_have_distinct_units():
    from digitalmodel.geotechnical.py_curves import (
        SpringGeometry, force_from_pressure, force_from_line_resistance,
        pressure_from_force,
    )

    geometry = SpringGeometry(diameter_m=0.8, tributary_length_m=2.0)
    # Illustrative example: projected area is diameter times integration length.
    expected_n = 25000.0 * 0.8 * 2.0
    assert force_from_pressure(25000.0, geometry) == expected_n
    assert pressure_from_force(expected_n, geometry) == 25000.0
    assert force_from_line_resistance(12000.0, 2.0) == 12000.0 * 2.0


def test_force_quantization_preserves_exact_boundaries():
    from math import nextafter
    from digitalmodel.geotechnical.py_curves import round_force_up

    assert round_force_up(0.0, 0.25) == 0.0
    assert round_force_up(1.0, 0.25) == 1.0
    assert round_force_up(nextafter(1.0, 0.0), 0.25) == 1.0
    assert round_force_up(nextafter(1.0, 2.0), 0.25) == 1.25
    assert round_force_up(1.1, 0.25) == 1.25


def test_depth_interpolation_uses_one_common_displacement_grid():
    from digitalmodel.geotechnical.py_curves import PressureCurve, interpolate_depth

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


def test_spring_table_is_odd_and_scales_pressure_by_projected_area():
    from digitalmodel.geotechnical.py_curves import (
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
