"""Illustrative spring curves with independently calculated ordinates."""

from dataclasses import replace

import pytest

from digitalmodel.geotechnical.lateral_soil_response import ClayPoint, SandPoint
from digitalmodel.geotechnical.lateral_soil_springs import clay_spring_curve


def test_clay_spring_units_symmetry_and_cyclic_reduction():
    # Illustrative example: ratios are caller choices, not a reference table.
    point = ClayPoint(2.0, 1.0, 8000.0, 20000.0, 0.02, 0.4)
    ratios = (0.125, 1.0, 8.0)
    curve = clay_spring_curve(
        point, positive_displacement_ratios_one=ratios, cyclic=False,
        tributary_length_m=2.0, tail_displacement_m=2.0, tail_factor_one=1.2,
    )
    assert len(curve) == 9
    assert curve[4].displacement_m == curve[4].force_n == 0
    expected_force = 92000 * 1.0 * 2.0
    assert [p.displacement_m for p in curve[5:]] == pytest.approx([0.00625, 0.05, 0.4, 2])
    assert [p.force_n for p in curve[5:]] == pytest.approx(
        [0.25 * expected_force, 0.5 * expected_force, expected_force, 1.2 * expected_force])
    for left, right in zip(curve, reversed(curve)):
        assert left.displacement_m == -right.displacement_m
        assert left.force_n == -right.force_n
    cyclic_curve = clay_spring_curve(
        point, positive_displacement_ratios_one=(0.125, 1.0, 16.0), cyclic=True,
        tributary_length_m=2.0, tail_displacement_m=2.0, tail_factor_one=1.0,
    )
    # XR = 7.5 m; the last knot alone uses depth/XR reduction.
    assert cyclic_curve[-2].force_n == pytest.approx(expected_force * 0.5 * 2 / 7.5)
    assert cyclic_curve[-3].force_n == pytest.approx(expected_force * 0.5)
    deep_curve = clay_spring_curve(
        replace(point, depth_m=20), positive_displacement_ratios_one=(0.125, 1.0, 16.0),
        cyclic=True, tributary_length_m=2.0, tail_displacement_m=2.0, tail_factor_one=1.0,
    )
    assert deep_curve[-2].force_n == pytest.approx(180000 * 2 * 0.5)


def test_sand_spring_sampling_and_tributary_force():
    from digitalmodel.geotechnical.lateral_soil_springs import sand_spring_curve
    import math

    # Illustrative example: choose a target that has a simple logarithmic inverse.
    point = SandPoint(4.0, 2.0, 9000.0, 2.0, 4.0, 6.0, 12000000.0)
    curve = sand_spring_curve(
        point, displacement_fractions_one=(0.25, 0.5, 1.0),
        target_resistance_fraction_one=0.8, cyclic=True, tributary_length_m=3.0,
        tail_displacement_m=1.0, tail_factor_one=1.2,
    )
    limit = 0.9 * 432000
    y_scale = limit / 48000000 * math.log(9) / 2
    for p, fraction in zip(curve[5:-1], (0.25, 0.5, 1.0)):
        assert p.displacement_m == pytest.approx(fraction * y_scale)
        # tanh(x) written independently through exponentials.
        e = math.exp(2 * fraction * math.log(9) / 2)
        assert p.force_n == pytest.approx(3 * limit * (e - 1) / (e + 1))
    assert curve[-1].force_n == pytest.approx(1.2 * 3 * limit * 0.8)


@pytest.mark.parametrize("overrides", [
    {"positive_displacement_ratios_one": ()},
    {"positive_displacement_ratios_one": (1.0,)},
    {"positive_displacement_ratios_one": (1.0, 0.5)},
    {"positive_displacement_ratios_one": (0.0, 1.0)},
    {"positive_displacement_ratios_one": (1.0, 9.0)},
    {"positive_displacement_ratios_one": (4.0, 16.0), "cyclic": True},
    {"tributary_length_m": 0.0}, {"tail_displacement_m": 0.1},
    {"tail_factor_one": 0.0}, {"tail_factor_one": float("nan")}, {"cyclic": 1},
])
def test_clay_spring_rejects_invalid_configuration(overrides):
    point = ClayPoint(2.0, 1.0, 8000.0, 20000.0, 0.02, 0.4)
    arguments = dict(positive_displacement_ratios_one=(0.125, 1.0, 8.0), cyclic=False,
                     tributary_length_m=2.0, tail_displacement_m=2.0, tail_factor_one=1.0)
    arguments.update(overrides)
    with pytest.raises(ValueError):
        clay_spring_curve(point, **arguments)


def test_duplicate_static_knots_are_preserved():
    point = ClayPoint(2.0, 1.0, 8000.0, 20000.0, 0.02, 0.4)
    curve = clay_spring_curve(
        point, positive_displacement_ratios_one=(1.0, 8.0, 8.0), cyclic=False,
        tributary_length_m=2.0, tail_displacement_m=2.0, tail_factor_one=1.0,
    )
    assert curve[-3] == curve[-2]


@pytest.mark.parametrize("overrides", [
    {"displacement_fractions_one": (0.5, 0.25)},
    {"displacement_fractions_one": (0.5, 1.1)},
    {"target_resistance_fraction_one": 1.0},
    {"target_resistance_fraction_one": 0.0},
    {"tributary_length_m": -1.0},
])
def test_sand_spring_rejects_invalid_configuration(overrides):
    from digitalmodel.geotechnical.lateral_soil_springs import sand_spring_curve

    point = SandPoint(4.0, 2.0, 9000.0, 2.0, 4.0, 6.0, 12000000.0)
    arguments = dict(displacement_fractions_one=(0.25, 0.5, 1.0),
                     target_resistance_fraction_one=0.8, cyclic=True,
                     tributary_length_m=3.0, tail_displacement_m=1.0, tail_factor_one=1.0)
    arguments.update(overrides)
    with pytest.raises(ValueError):
        sand_spring_curve(point, **arguments)


def test_spring_points_reject_boolean_and_nonfinite_quantities():
    from digitalmodel.geotechnical.lateral_soil_springs import SpringPoint

    for displacement, force in [(True, 2.0), (1.0, True), (float("inf"), 1.0)]:
        with pytest.raises(ValueError):
            SpringPoint(displacement, force)
