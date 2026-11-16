"""Illustrative example inputs; independent pressure-equation and inverse checks."""

import math
from dataclasses import FrozenInstanceError, replace

import pytest

from digitalmodel.ansys.pressure_vessel import ASMEThicknessCalc
from digitalmodel.asset_integrity.cylinder_pressure_limits import (
    CylinderPressureInputs,
    hoop_pressure_limits,
    longitudinal_pressure_limits,
    select_pressure_limits,
)


def illustrative_inputs(**changes):
    inputs = CylinderPressureInputs(
        inner_radius_m=0.3,
        wall_thickness_m=0.024,
        pressure_pa=8e6,
        allowable_stress_pa=160e6,
        joint_efficiency_ratio=0.8,
        supplemental_thickness_m=0.004,
        longitudinal_pressure_coefficient_ratio=0.3,
    )
    return replace(inputs, **changes)


def test_existing_rational_hoop_equations_against_hand_formula():
    """Illustrative example: MPa and mm are explicit at the existing API."""
    calc = ASMEThicknessCalc()
    required = calc.ug27_cylindrical_shell(8.0, 300.0, 160.0, 0.8, 0.0)
    assert required['t_required_mm'] == pytest.approx(8 * 300 / (160 * 0.8 - 0.6 * 8))
    pressure = calc.calculate_mawp(24.0, 300.0, 160.0, 0.8, 0.0)
    assert pressure == pytest.approx(160 * 0.8 * 24 / (300 + 0.6 * 24))
    with pytest.raises(ValueError, match='corroded thickness'):
        calc.calculate_mawp(4.0, 300.0, 160.0, 0.8, 4.0)


def test_hoop_equations_and_units():
    x = illustrative_inputs()
    result = hoop_pressure_limits(x)
    r, t, p, s, e = 0.3, 0.024, 8e6, 160e6, 0.8
    assert result.rational_required_thickness_m == pytest.approx(p * r / (s * e - 0.6 * p))
    assert result.rational_pressure_limit_pa == pytest.approx(s * e * t / (r + 0.6 * t))
    assert result.rational_required_allowable_stress_pa == pytest.approx(p / e * (r / t + 0.6))
    assert result.thick_required_thickness_m == pytest.approx(r * (math.exp(p / (s * e)) - 1))
    assert result.thick_pressure_limit_pa == pytest.approx(s * e * math.log((r + t) / r))
    assert result.thick_required_allowable_stress_pa == pytest.approx(p / (e * math.log((r + t) / r)))


def test_longitudinal_equations_against_inverse_pressure_balance():
    result = longitudinal_pressure_limits(illustrative_inputs())
    r, t, p, s, e, extra = 0.3, 0.024, 8e6, 160e6, 0.8, 0.004
    effective = t - extra
    assert result.rational_required_thickness_m == pytest.approx(p * r / (2 * s * e + 0.3 * p) + extra)
    assert result.rational_pressure_limit_pa == pytest.approx(2 * s * e * effective / (r - 0.3 * effective))
    assert result.rational_required_allowable_stress_pa == pytest.approx(p / (2 * e) * (r / effective - 0.3))
    assert result.thick_required_thickness_m == pytest.approx(r * (math.sqrt(1 + p / (s * e)) - 1) + extra)
    factor = ((r + effective) / r) ** 2 - 1
    assert result.thick_pressure_limit_pa == pytest.approx(s * e * factor)
    assert result.thick_required_allowable_stress_pa == pytest.approx(p / (e * factor))


@pytest.mark.parametrize('calculate', [hoop_pressure_limits, longitudinal_pressure_limits])
@pytest.mark.parametrize('family', ['rational', 'thick'])
def test_required_thickness_and_required_stress_invert_capacity(calculate, family):
    x = illustrative_inputs()
    result = calculate(x)
    at_pressure = replace(x, wall_thickness_m=getattr(result, f'{family}_required_thickness_m'))
    assert getattr(calculate(at_pressure), f'{family}_pressure_limit_pa') == pytest.approx(x.pressure_pa, rel=1e-12)
    at_stress = replace(x, allowable_stress_pa=getattr(result, f'{family}_required_allowable_stress_pa'))
    assert getattr(calculate(at_stress), f'{family}_pressure_limit_pa') == pytest.approx(x.pressure_pa, rel=1e-12)


@pytest.mark.parametrize('calculate', [hoop_pressure_limits, longitudinal_pressure_limits])
def test_zero_pressure_has_zero_pressure_demands(calculate):
    result = calculate(illustrative_inputs(pressure_pa=0.0))
    extra = 0.004 if calculate is longitudinal_pressure_limits else 0.0
    assert result.rational_required_thickness_m == extra
    assert result.thick_required_thickness_m == extra
    assert result.rational_required_allowable_stress_pa == 0.0
    assert result.thick_required_allowable_stress_pa == 0.0
    assert result.rational_pressure_limit_pa > 0


@pytest.mark.parametrize('ratio', [1e-14, 1e-10])
def test_small_pressure_retains_thick_wall_thickness(ratio):
    x = illustrative_inputs(pressure_pa=160e6 * 0.8 * ratio, supplemental_thickness_m=0.0)
    assert hoop_pressure_limits(x).thick_required_thickness_m == pytest.approx(0.3 * ratio, rel=1e-9, abs=0)
    assert longitudinal_pressure_limits(x).thick_required_thickness_m == pytest.approx(0.3 * ratio / 2, rel=1e-9, abs=0)


def test_transition_equality_uses_rational_pressure_branch():
    x = illustrative_inputs(pressure_pa=160e6 * 0.8 * 0.4)
    selected = select_pressure_limits(x, hoop_transition_ratio=0.4, longitudinal_transition_ratio=1.2)
    assert selected.hoop_pressure_limit_pa == hoop_pressure_limits(x).rational_pressure_limit_pa
    assert selected.longitudinal_pressure_limit_pa == longitudinal_pressure_limits(x).rational_pressure_limit_pa
    x = replace(x, pressure_pa=x.pressure_pa + 1.0)
    assert select_pressure_limits(x, hoop_transition_ratio=0.4, longitudinal_transition_ratio=1.2).hoop_pressure_limit_pa == hoop_pressure_limits(x).thick_pressure_limit_pa
    x = replace(x, pressure_pa=160e6 * 0.8 * 1.3)
    assert select_pressure_limits(x, hoop_transition_ratio=0.4, longitudinal_transition_ratio=1.2).longitudinal_pressure_limit_pa == longitudinal_pressure_limits(x).thick_pressure_limit_pa


@pytest.mark.parametrize('field,value', [
    ('inner_radius_m', 0.0), ('inner_radius_m', -1.0),
    ('wall_thickness_m', 0.0), ('allowable_stress_pa', 0.0),
    ('pressure_pa', -1.0), ('pressure_pa', float('nan')),
    ('wall_thickness_m', float('inf')), ('joint_efficiency_ratio', 0.0),
    ('joint_efficiency_ratio', 1.1), ('supplemental_thickness_m', -0.1),
    ('supplemental_thickness_m', 0.024), ('pressure_pa', True),
    ('pressure_pa', '8'),
])
def test_invalid_inputs_are_rejected(field, value):
    with pytest.raises(ValueError):
        illustrative_inputs(**{field: value})


def test_frozen_inputs():
    with pytest.raises(FrozenInstanceError):
        illustrative_inputs().pressure_pa = 0.0


def test_singular_rational_equations_are_rejected():
    with pytest.raises(ValueError, match='denominator'):
        hoop_pressure_limits(illustrative_inputs(pressure_pa=160e6 * 0.8 / 0.6))
    with pytest.raises(ValueError, match='denominator'):
        longitudinal_pressure_limits(illustrative_inputs(wall_thickness_m=1.1))


@pytest.mark.parametrize('value', [0.0, -1.0, float('nan'), float('inf'), True])
def test_invalid_transition_is_rejected(value):
    with pytest.raises(ValueError):
        select_pressure_limits(illustrative_inputs(), hoop_transition_ratio=value, longitudinal_transition_ratio=1.2)


def test_longitudinal_transition_equality_and_adjacent_pressure():
    x = illustrative_inputs(pressure_pa=160e6 * 0.8 * 1.2)
    selected = select_pressure_limits(x, hoop_transition_ratio=0.4, longitudinal_transition_ratio=1.2)
    assert selected.longitudinal_pressure_limit_pa == longitudinal_pressure_limits(x).rational_pressure_limit_pa
    x = replace(x, pressure_pa=x.pressure_pa + 1)
    selected = select_pressure_limits(x, hoop_transition_ratio=0.4, longitudinal_transition_ratio=1.2)
    assert selected.longitudinal_pressure_limit_pa == longitudinal_pressure_limits(x).thick_pressure_limit_pa


@pytest.mark.parametrize('coefficient', [0.0, -0.1, math.inf, math.nan, True])
def test_longitudinal_coefficient_requires_positive_finite_value(coefficient):
    with pytest.raises(ValueError):
        illustrative_inputs(longitudinal_pressure_coefficient_ratio=coefficient)


def test_underflowing_factored_allowable_stress_is_rejected():
    with pytest.raises(ValueError, match='factored allowable stress'):
        illustrative_inputs(allowable_stress_pa=1e-300, joint_efficiency_ratio=1e-300)


def test_overflowing_longitudinal_result_is_rejected():
    x = illustrative_inputs(inner_radius_m=3.0, pressure_pa=1e308)
    with pytest.raises(ValueError, match='result'):
        longitudinal_pressure_limits(x)
