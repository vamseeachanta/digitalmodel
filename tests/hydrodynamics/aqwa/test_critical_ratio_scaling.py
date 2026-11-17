"""Illustrative example checks of the existing critical-ratio scaling method.

Expected coefficients follow independent critical-coefficient balances. The
tests import the existing method and converters through their package paths.
"""

import math

import pytest

from digitalmodel.hydrodynamics.aqwa.aqwa_analysis_raos import AqwaRAOs
from digitalmodel.hydrodynamics.diffraction import diffraction_units as units_module


@pytest.fixture
def addition():
    return AqwaRAOs().get_additional_damping


@pytest.fixture
def units():
    return units_module


@pytest.mark.parametrize("current_percent,target_percent,coefficient_nm_s_rad", [
    (4.0, 12.0, 4800.0),
    (6.0, 9.0, 7200.0),
    (12.0, 18.0, 24000.0),
])
def test_increase_from_independent_critical_coefficient(
    addition, current_percent, target_percent, coefficient_nm_s_rad,
):
    """Illustrative example: desired torque coefficient minus existing one."""
    critical_nm_s_rad = coefficient_nm_s_rad / (current_percent / 100.0)
    desired_nm_s_rad = critical_nm_s_rad * target_percent / 100.0
    expected_nm_s_rad = desired_nm_s_rad - coefficient_nm_s_rad
    actual = addition(current_percent, coefficient_nm_s_rad, target_percent,
                      percent_unit="percent")
    assert actual == pytest.approx(expected_nm_s_rad, rel=1e-12, abs=1e-12)


@pytest.mark.parametrize("target_percent", [0.0, 3.0, 6.0])
def test_equal_or_lower_target_needs_no_positive_addition(addition, target_percent):
    assert addition(6.0, 7200.0, target_percent, percent_unit="percent") == 0.0


def test_zero_absolute_coefficient_returns_zero(addition):
    assert addition(6.0, 0.0, 9.0, percent_unit="percent") == 0.0


def test_explicit_subpercent_values_use_the_same_ratio(addition):
    assert addition(0.125, 400.0, 0.375, percent_unit="percent") == 800.0


def test_output_rounding_is_part_of_existing_contract(addition):
    # Illustrative example: critical coefficient is 100 / (3/100).
    # Raising the target from 3% to 4% requires 100/3, reported to one decimal.
    assert addition(3.0, 100.0, 4.0, percent_unit="percent") == 33.3


@pytest.mark.parametrize("current,coefficient,target", [
    (0.0, 400.0, 9.0),
    (-3.0, 400.0, 9.0),
    (6.0, -400.0, 9.0),
    (6.0, 400.0, -9.0),
])
def test_invalid_signs_are_rejected(addition, current, coefficient, target):
    with pytest.raises(ValueError):
        addition(current, coefficient, target, percent_unit="percent")


@pytest.mark.parametrize("invalid", [math.nan, math.inf, -math.inf])
@pytest.mark.parametrize("index", [0, 1, 2])
def test_nonfinite_inputs_are_rejected(addition, invalid, index):
    arguments = [6.0, 400.0, 9.0]
    arguments[index] = invalid
    with pytest.raises(ValueError, match="finite"):
        addition(*arguments, percent_unit="percent")


def test_implicit_subpercent_target_is_rejected(addition):
    with pytest.raises(ValueError, match="Ambiguous"):
        addition(0.125, 400.0, 0.375)


@pytest.mark.parametrize("unit", ["fraction", "ratio", "%"])
def test_unrecognized_percent_unit_is_rejected(addition, unit):
    with pytest.raises(ValueError, match="percent"):
        addition(6.0, 400.0, 9.0, percent_unit=unit)


@pytest.mark.parametrize("period_s", [12.0, 24.0, 48.0])
def test_period_frequency_conversions_follow_one_cycle(units, period_s):
    omega_rad_s = 2.0 * math.pi / period_s
    assert units.period_s_to_rad_per_s(period_s) == pytest.approx(omega_rad_s)
    assert units.rad_per_s_to_period_s(omega_rad_s) == pytest.approx(period_s)
    assert units.rad_per_s_to_hz(omega_rad_s) == pytest.approx(1.0 / period_s)
    assert units.hz_to_rad_per_s(1.0 / period_s) == pytest.approx(omega_rad_s)


def test_per_degree_coefficient_preserves_torque(units, addition):
    """Illustrative example: equal torque for equal physical angular speed."""
    coefficient_nm_s_rad = addition(4.0, 4800.0, 12.0, percent_unit="percent")
    coefficient_nm_s_deg = units.degrees_to_radians(coefficient_nm_s_rad)
    assert coefficient_nm_s_deg == pytest.approx(9600.0 * math.pi / 180.0, rel=1e-12)
    # Hand calculation: 167.5516 N m s/deg, rounded to four decimal places.
    assert coefficient_nm_s_deg == pytest.approx(167.5516, rel=0.0, abs=5e-5)
    angular_rate_deg_s = 6.0
    angular_rate_rad_s = angular_rate_deg_s * math.pi / 180.0
    assert coefficient_nm_s_deg * angular_rate_deg_s == pytest.approx(
        coefficient_nm_s_rad * angular_rate_rad_s,
    )
