"""Illustrative example inputs; expected values use independent hand formulas."""

from dataclasses import FrozenInstanceError, replace

import pytest

from digitalmodel.code_checks import pipe_bending_capacity as pipe_bending
from digitalmodel.hydrostatics import fluid_column
from digitalmodel.units import Q_

from digitalmodel.code_checks.pipe_bending_capacity import (
    BendingInputs,
    BendingRule,
    RoundnessRule,
    bending_capacity,
    diameter_thickness_ratio,
    hardening_beta,
    hydrostatic_pressure_pa,
    inches_to_metres,
    metres_to_inches,
    millimetres_to_metres,
    roundness_limit,
    strength_alpha,
)


@pytest.fixture
def inputs():
    """Illustrative example, not material or project design data."""
    return BendingInputs(0.75, 0.025, 300e6, 280e6, 350e6, 1.2, 1.1)


@pytest.fixture
def rule():
    return BendingRule(50.0, 100.0)


@pytest.fixture
def roundness_rule():
    """Illustrative coefficients; no standard-table defaults are implied."""
    return RoundnessRule(0.05, 0.8, 1.5, 0.012, 0.006, 0.009)


def test_capacity_independent_formula(inputs, rule):
    result = bending_capacity(inputs, rule)
    beta = (50 - 0.75 / 0.025) / 100
    alpha = 1 - beta + beta * (350e6 / 280e6)
    moment = (0.75 - 0.025) ** 2 * 0.025 * 300e6
    assert result.diameter_thickness_ratio == pytest.approx(30)
    assert result.beta_fraction == pytest.approx(beta)
    assert result.alpha_fraction == pytest.approx(alpha)
    assert result.plastic_moment_nm == pytest.approx(moment)
    assert result.design_moment_nm == pytest.approx(moment * alpha / 1.2 / 1.1)


def test_strength_inputs_remain_independent(inputs, rule):
    base = bending_capacity(inputs, rule)
    doubled = bending_capacity(replace(inputs, specified_yield_strength_pa=600e6), rule)
    assert doubled.alpha_fraction == base.alpha_fraction
    assert doubled.design_moment_nm == pytest.approx(2 * base.design_moment_nm)
    equal = replace(inputs, tensile_strength_pa=inputs.yield_strength_pa)
    assert bending_capacity(equal, rule).alpha_fraction == pytest.approx(1)


def test_capacity_geometric_scaling(inputs, rule):
    base = bending_capacity(inputs, rule)
    scaled = bending_capacity(replace(inputs, outer_diameter_m=1.5,
                                      wall_thickness_m=0.05), rule)
    assert scaled.alpha_fraction == base.alpha_fraction
    assert scaled.plastic_moment_nm == pytest.approx(8 * base.plastic_moment_nm)
    assert scaled.design_moment_nm == pytest.approx(8 * base.design_moment_nm)


def test_geometry_and_beta_branches(rule):
    assert diameter_thickness_ratio(0.75, 0.025) == pytest.approx(30)
    assert hardening_beta(50, rule) == 0
    assert hardening_beta(70, rule) == 0
    assert hardening_beta(49, rule) == pytest.approx(1 / 100)
    assert strength_alpha(0, 280e6, 350e6) == 1


def test_beta_is_not_capped():
    assert hardening_beta(1e-3, BendingRule(50, 10)) > 1


def test_frozen_inputs(inputs):
    with pytest.raises(FrozenInstanceError):
        inputs.outer_diameter_m = 1


def test_exact_length_boundaries():
    assert inches_to_metres(1) == pytest.approx(0.0254)
    assert millimetres_to_metres(1) == pytest.approx(0.001)
    assert metres_to_inches(0.0254) == pytest.approx(1)
    assert metres_to_inches(inches_to_metres(3.5)) == pytest.approx(3.5)
    assert inches_to_metres(0) == 0


@pytest.mark.parametrize('convert, value, unit, target', [
    (inches_to_metres, 1, 'inch', 'm'),
    (metres_to_inches, 0.0254, 'm', 'inch'),
    (millimetres_to_metres, 1, 'mm', 'm'),
])
def test_conversions_use_shared_registry(monkeypatch, convert, value, unit, target):
    calls = []

    def quantity(magnitude, source_unit):
        calls.append((magnitude, source_unit))
        return Q_(magnitude, source_unit)

    monkeypatch.setattr(pipe_bending, 'Q_', quantity)
    assert convert(value) == Q_(value, unit).to(target).magnitude
    assert calls == [(value, unit)]


@pytest.mark.parametrize(
    'diameter, status, expected',
    [(0.04, 'included_in_diameter', None),
     (0.05, 'calculated', 0.05 * 0.012),
     (0.7, 'calculated', 0.7 * 0.012),
     (0.8, 'calculated', 0.8 * 0.006),
     (1.49, 'calculated', 1.49 * 0.006),
     (1.5, 'agreement_required', None),
     (2.0, 'agreement_required', None)],
)
def test_roundness_intervals(roundness_rule, diameter, status, expected):
    result = roundness_limit(diameter, roundness_rule)
    assert result.status == status
    if expected is None:
        assert result.out_of_roundness_m is None
        assert result.ovality_fraction is None
    else:
        assert result.out_of_roundness_m == pytest.approx(expected)
        assert result.ovality_fraction == pytest.approx(expected / diameter)


def test_roundness_cap(roundness_rule):
    capped = replace(roundness_rule, large_diameter_cap_m=0.003)
    result = roundness_limit(1.0, capped)
    assert result.out_of_roundness_m == 0.003
    assert result.ovality_fraction == 0.003


def test_hydrostatic_pressure():
    assert hydrostatic_pressure_pa(1000, 10, 25) == 250000
    assert hydrostatic_pressure_pa(1000, 10, 25) == fluid_column.hydrostatic_pressure(
        1000, 25, gravity=10)
    assert hydrostatic_pressure_pa(1000, 10, 0) == 0


def test_hydrostatic_uses_shared_function(monkeypatch):
    calls = []

    def pressure(density, height, *, gravity):
        calls.append((density, height, gravity))
        return 123.0

    monkeypatch.setattr(fluid_column, 'hydrostatic_pressure', pressure)
    assert hydrostatic_pressure_pa(1000, 10, 25) == 123.0
    assert calls == [(1000, 25, 10)]


@pytest.mark.parametrize('bad', [0, -1, float('nan'), float('inf'), float('-inf')])
@pytest.mark.parametrize('field', [
    'outer_diameter_m', 'wall_thickness_m', 'specified_yield_strength_pa',
    'yield_strength_pa', 'tensile_strength_pa', 'material_factor_fraction',
    'safety_class_factor_fraction',
])
def test_bending_validation(inputs, rule, field, bad):
    with pytest.raises(ValueError):
        bending_capacity(replace(inputs, **{field: bad}), rule)


def test_geometric_and_strength_order(inputs, rule):
    with pytest.raises(ValueError):
        bending_capacity(replace(inputs, wall_thickness_m=0.375), rule)
    with pytest.raises(ValueError):
        bending_capacity(replace(inputs, tensile_strength_pa=100e6), rule)
    with pytest.raises(ValueError):
        strength_alpha(-0.1, 280e6, 350e6)


@pytest.mark.parametrize('bad', [0, -1, float('nan'), float('inf')])
def test_standalone_geometry_validation(rule, roundness_rule, bad):
    with pytest.raises(ValueError):
        diameter_thickness_ratio(bad, 0.025)
    with pytest.raises(ValueError):
        diameter_thickness_ratio(0.75, bad)
    with pytest.raises(ValueError):
        hardening_beta(bad, rule)
    with pytest.raises(ValueError):
        roundness_limit(bad, roundness_rule)


@pytest.mark.parametrize('bad', [0, -1, float('nan'), float('inf')])
def test_rule_validation(rule, roundness_rule, bad):
    for field in ('beta_threshold_ratio', 'beta_divisor_ratio'):
        with pytest.raises(ValueError):
            hardening_beta(30, replace(rule, **{field: bad}))
    for field in roundness_rule.__dataclass_fields__:
        with pytest.raises(ValueError):
            roundness_limit(0.7, replace(roundness_rule, **{field: bad}))


def test_roundness_threshold_order(roundness_rule):
    with pytest.raises(ValueError):
        roundness_limit(0.7, replace(roundness_rule, large_diameter_start_m=0.01))
    with pytest.raises(ValueError):
        roundness_limit(0.7, replace(roundness_rule, agreement_start_m=0.8))


@pytest.mark.parametrize('bad', [-1, float('nan'), float('inf')])
def test_conversion_validation(bad):
    for convert in (inches_to_metres, metres_to_inches, millimetres_to_metres):
        with pytest.raises(ValueError):
            convert(bad)


@pytest.mark.parametrize('args', [(0, 10, 1), (1000, 0, 1), (1000, 10, -1),
                                  (1000, float('nan'), 1)])
def test_hydrostatic_validation(args):
    with pytest.raises(ValueError):
        hydrostatic_pressure_pa(*args)


def test_derived_overflow(inputs, rule):
    with pytest.raises(ValueError):
        bending_capacity(replace(inputs, outer_diameter_m=1e110,
                                 wall_thickness_m=1e109), rule)
    with pytest.raises(ValueError):
        hydrostatic_pressure_pa(1e308, 10, 25)
