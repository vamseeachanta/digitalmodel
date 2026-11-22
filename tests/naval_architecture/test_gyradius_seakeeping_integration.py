"""Rigid-body point-mass radii used in seakeeping natural periods."""

import math

import pytest

from digitalmodel.naval_architecture.gyradius import gyradius_from_point_masses
from digitalmodel.naval_architecture.seakeeping import (
    natural_pitch_period,
    natural_roll_period,
)


def test_point_mass_radii_drive_natural_periods():
    """Four equal masses give independently derived radii and periods."""
    result = gyradius_from_point_masses(
        [1.0] * 4,
        [(x, y, 0.0) for x in (-4.0, 4.0) for y in (-3.0, 3.0)],
    )
    assert result.mass_kg == 4.0
    assert result.centroid_m == pytest.approx((0.0, 0.0, 0.0), abs=1e-12)
    assert (result.ixx, result.iyy, result.izz) == pytest.approx(
        (36.0, 64.0, 100.0), rel=1e-12
    )
    assert (result.roll, result.pitch, result.yaw) == pytest.approx(
        (3.0, 4.0, 5.0), rel=1e-12
    )
    assert natural_roll_period(30.0, 1.0, kxx_m=result.roll) == pytest.approx(
        6 * math.pi / math.sqrt(9.81), rel=1e-12
    )
    assert natural_pitch_period(150.0, 1.0, 10000.0, kyy_m=result.pitch) == (
        pytest.approx(8 * math.pi / math.sqrt(9.81), rel=1e-12)
    )


@pytest.mark.parametrize("beam_m", [10.0, 30.0])
def test_explicit_roll_radius_overrides_beam(beam_m):
    assert natural_roll_period(beam_m, 1.0, kxx_m=3.0) == pytest.approx(
        6 * math.pi / math.sqrt(9.81), rel=1e-12
    )


@pytest.mark.parametrize("lwl_m", [50.0, 150.0])
def test_explicit_pitch_radius_overrides_length(lwl_m):
    assert natural_pitch_period(lwl_m, 1.0, 10000.0, kyy_m=4.0) == pytest.approx(
        8 * math.pi / math.sqrt(9.81), rel=1e-12
    )


@pytest.mark.parametrize("radius", [0.0, -1.0, math.nan, math.inf, -math.inf, True, False])
@pytest.mark.parametrize(
    "period,args,keyword",
    [
        (natural_roll_period, (30.0, 1.0), "kxx_m"),
        (natural_pitch_period, (150.0, 1.0, 10000.0), "kyy_m"),
    ],
)
def test_invalid_explicit_radius_raises(period, args, keyword, radius):
    with pytest.raises(ValueError, match=keyword):
        period(*args, **{keyword: radius})


@pytest.mark.parametrize("c_roll", [0.8, 1.2])
def test_legacy_roll_formula_and_none(c_roll):
    expected = 2 * math.pi * (0.40 * 30.0) / math.sqrt(9.81 * 2.0)
    assert natural_roll_period(30.0, 2.0, c_roll) == expected
    assert natural_roll_period(30.0, 2.0, c_roll, kxx_m=None) == expected


@pytest.mark.parametrize("displacement", [1000.0, 10000.0])
def test_legacy_pitch_formula_and_none(displacement):
    expected = 2 * math.pi * (0.25 * 150.0) / math.sqrt(9.81 * 100.0)
    assert natural_pitch_period(150.0, 100.0, displacement) == expected
    assert natural_pitch_period(150.0, 100.0, displacement, kyy_m=None) == expected


@pytest.mark.parametrize("height", [0.0, -1.0])
def test_explicit_radii_preserve_stability_checks(height):
    with pytest.raises(ValueError, match="GM"):
        natural_roll_period(30.0, height, kxx_m=3.0)
    with pytest.raises(ValueError, match="GM"):
        natural_pitch_period(150.0, height, 10000.0, kyy_m=4.0)


def test_explicit_radii_are_keyword_only():
    with pytest.raises(TypeError):
        natural_roll_period(30.0, 1.0, 0.8, 3.0)
    with pytest.raises(TypeError):
        natural_pitch_period(150.0, 1.0, 10000.0, 4.0)
