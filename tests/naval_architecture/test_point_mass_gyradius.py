# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic balances using I = sum(m*r^2), k = sqrt(I/M).

All dimensions and masses below are chosen test inputs in SI units. Expected
values follow the hand calculations stated in each test, without empirical
coefficients or reference data.
"""

import math

import pytest

from digitalmodel.naval_architecture.gyradius import gyradius_from_point_masses


def test_symmetric_pair_hand_balance():
    """M=4; sum(m*r^2) gives (100, 80, 52); sqrt(I/M) gives k."""
    result = gyradius_from_point_masses([2, 2], [(2, 3, 4), (-2, -3, -4)])

    assert result.mass_kg == 4
    assert result.centroid_m == pytest.approx((0, 0, 0))
    # Ixx=2*2*(3^2+4^2), Iyy=2*2*(2^2+4^2), Izz=2*2*(2^2+3^2).
    assert (result.ixx, result.iyy, result.izz) == pytest.approx((100, 80, 52))
    assert (result.roll, result.pitch, result.yaw) == pytest.approx(
        (5, math.sqrt(20), math.sqrt(13))
    )


def test_unequal_masses_hand_balance():
    """M=4; centroid=(3,1,2); offsets=(-3,0,0),(1,0,0)."""
    result = gyradius_from_point_masses([1, 3], [(0, 1, 2), (4, 1, 2)])

    assert result.centroid_m == pytest.approx((3, 1, 2))
    # Ixx=0; Iyy=Izz=1*3^2+3*1^2=12; kyy=kzz=sqrt(12/4).
    assert (result.ixx, result.iyy, result.izz) == pytest.approx((0, 12, 12))
    assert (result.roll, result.pitch, result.yaw) == pytest.approx(
        (0, math.sqrt(3), math.sqrt(3))
    )


@pytest.mark.parametrize("shift", [(32, -16, 8), (2**50, -(2**50), 2**50)])
def test_translation_invariance(shift):
    """A common offset shifts the centroid and preserves centroidal inertia."""
    masses = [1, 3]
    points = [(0, 1, 2), (4, 1, 2)]
    # Chosen exactly representable translations, including a large common offset.
    initial = gyradius_from_point_masses(masses, points)
    translated = gyradius_from_point_masses(
        masses, [tuple(p + d for p, d in zip(point, shift)) for point in points]
    )

    assert translated.centroid_m == tuple(
        center + delta for center, delta in zip((3, 1, 2), shift)
    )
    assert (translated.ixx, translated.iyy, translated.izz) == pytest.approx(
        (initial.ixx, initial.iyy, initial.izz)
    )
    assert (translated.roll, translated.pitch, translated.yaw) == pytest.approx(
        (initial.roll, initial.pitch, initial.yaw)
    )


def test_mass_scaling_invariance():
    """Scaling all masses by seven scales I and M equally, preserving k."""
    points = [(0, 1, 2), (4, 1, 2)]
    initial = gyradius_from_point_masses([1, 3], points)
    scaled = gyradius_from_point_masses([7, 21], points)

    assert scaled.mass_kg == 7 * initial.mass_kg
    assert scaled.centroid_m == pytest.approx(initial.centroid_m)
    assert (scaled.ixx, scaled.iyy, scaled.izz) == pytest.approx(
        tuple(7 * inertia for inertia in (initial.ixx, initial.iyy, initial.izz))
    )
    assert (scaled.roll, scaled.pitch, scaled.yaw) == pytest.approx(
        (initial.roll, initial.pitch, initial.yaw)
    )


def test_zero_mass_and_single_pass_iterables():
    """One massive point has zero centroidal inertia; a zero mass adds none."""
    result = gyradius_from_point_masses(
        iter([0, 5]), iter([(20, -30, 40), (2, 3, 4)])
    )

    assert result.mass_kg == 5
    assert result.centroid_m == pytest.approx((2, 3, 4))
    assert (result.ixx, result.iyy, result.izz) == (0, 0, 0)
    assert (result.roll, result.pitch, result.yaw) == (0, 0, 0)
    # This result exposes rotational quantities only.
    assert not any(hasattr(result, axis) for axis in ("surge", "sway", "heave"))


@pytest.mark.parametrize("axis", range(3))
def test_pair_on_each_axis(axis):
    """Two unit masses at +/-2 m: axial inertia zero, transverse inertia 8."""
    point = tuple(2 if index == axis else 0 for index in range(3))
    result = gyradius_from_point_masses([1, 1], [point, tuple(-x for x in point)])

    assert (result.ixx, result.iyy, result.izz) == pytest.approx(
        tuple(0 if index == axis else 8 for index in range(3))
    )
    assert (result.roll, result.pitch, result.yaw) == pytest.approx(
        tuple(0 if index == axis else 2 for index in range(3))
    )


def test_empty_distribution_raises():
    with pytest.raises(ValueError, match="at least one point mass"):
        gyradius_from_point_masses([], [])


def test_all_zero_masses_raise():
    with pytest.raises(ValueError, match="total mass must be positive"):
        gyradius_from_point_masses([0, 0], [(0, 0, 0), (1, 1, 1)])


def test_negative_mass_raises():
    with pytest.raises(ValueError, match="masses must be finite and non-negative"):
        gyradius_from_point_masses([2, -1], [(0, 0, 0), (1, 1, 1)])


@pytest.mark.parametrize("mass", [math.nan, math.inf, -math.inf])
def test_nonfinite_mass_raises(mass):
    with pytest.raises(ValueError, match="masses must be finite and non-negative"):
        gyradius_from_point_masses([mass], [(0, 0, 0)])


@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf])
@pytest.mark.parametrize("axis", range(3))
@pytest.mark.parametrize("mass", [0, 1])
def test_nonfinite_coordinate_raises(value, axis, mass):
    point = tuple(value if index == axis else 0 for index in range(3))
    with pytest.raises(ValueError, match="coordinates must be finite"):
        gyradius_from_point_masses([mass, 1], [point, (0, 0, 0)])


@pytest.mark.parametrize("points", [[], [(0, 0, 0), (1, 1, 1)]])
def test_mismatched_lengths_raise(points):
    with pytest.raises(ValueError, match="same length"):
        gyradius_from_point_masses([1], points)


@pytest.mark.parametrize("point", [(), (1, 2), (1, 2, 3, 4)])
def test_coordinate_shape_raises(point):
    with pytest.raises(ValueError, match="three coordinates"):
        gyradius_from_point_masses([1], [point])


def test_unrepresentable_total_mass_raises():
    """Two finite synthetic masses whose sum exceeds floating-point range."""
    with pytest.raises(ValueError, match="total mass must be finite"):
        gyradius_from_point_masses([1e308, 1e308], [(0, 0, 0), (0, 0, 0)])


def test_unrepresentable_inertia_raises():
    """Iyy=Izz=2*(1e200)^2 exceeds range for a synthetic unit-mass pair."""
    with pytest.raises(ValueError, match="centroid offsets and inertias must be finite"):
        gyradius_from_point_masses([1, 1], [(-1e200, 0, 0), (1e200, 0, 0)])


@pytest.mark.parametrize("mass", [None, "invalid"])
def test_nonnumeric_mass_raises(mass):
    with pytest.raises(ValueError, match="masses must be finite and non-negative"):
        gyradius_from_point_masses([mass], [(0, 0, 0)])


@pytest.mark.parametrize("point", [(None, 0, 0), ("invalid", 0, 0), None])
def test_nonnumeric_coordinate_raises(point):
    with pytest.raises(ValueError, match="coordinates must be finite"):
        gyradius_from_point_masses([1], [point])
