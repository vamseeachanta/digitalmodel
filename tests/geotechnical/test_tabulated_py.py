"""Illustrative example inputs; no project data or prescribed soil tables."""

from dataclasses import FrozenInstanceError
import math

import pytest

from digitalmodel.geotechnical.py_curves import Curve, SoftClay, clay_tz, soft_clay_py
from digitalmodel.geotechnical.tabulated_py import TabulatedPYInputs, tabulated_py_curve


def illustrative_inputs(**changes):
    values = dict(
        reference_displacement_m=0.04,
        reference_resistance_kn_m=200.0,
        displacement_ratios=(0.0, 0.25, 1.0, 2.0, 4.0),
        resistance_ratios=(0.0, 0.4, 0.6, 1.0, 0.8),
    )
    values.update(changes)
    return TabulatedPYInputs(**values)


def test_supplied_knots_are_scaled_without_selecting_soil_parameters():
    curve = tabulated_py_curve(illustrative_inputs())
    assert isinstance(curve, Curve)
    assert curve.resistance_unit == "kN/m"
    assert curve.displacements_m == pytest.approx((0.0, 0.01, 0.04, 0.08, 0.16))
    assert curve.resistances == pytest.approx((0.0, 80.0, 120.0, 200.0, 160.0))
    for x, resistance in zip(curve.displacements_m, curve.resistances):
        assert curve.resistance_at(x) == pytest.approx(resistance)


def test_falling_branch_interpolation_reflection_and_constant_tail():
    curve = tabulated_py_curve(illustrative_inputs())
    expected = 200.0 + (160.0 - 200.0) * (0.12 - 0.08) / (0.16 - 0.08)
    assert curve.resistance_at(0.12) == pytest.approx(expected)
    assert curve.resistance_at(-0.12) == pytest.approx(-expected)
    assert curve.resistance_at(10.0) == pytest.approx(160.0)
    assert curve.resistance_at(-10.0) == pytest.approx(-160.0)
    assert curve.resistance_at(0.0) == 0.0


def test_zero_resistance_tail_is_preserved():
    curve = tabulated_py_curve(
        illustrative_inputs(resistance_ratios=(0.0, 0.4, 0.6, 1.0, 0.0))
    )
    assert curve.resistance_at(0.12) == pytest.approx(100.0)
    assert curve.resistance_at(10.0) == 0.0


def test_inputs_are_frozen_and_detached_from_mutable_arrays():
    ratios = [0.0, 1.0]
    inputs = illustrative_inputs(displacement_ratios=ratios, resistance_ratios=ratios)
    ratios[1] = 8.0
    assert inputs.displacement_ratios == (0.0, 1.0)
    assert inputs.resistance_ratios == (0.0, 1.0)
    with pytest.raises(FrozenInstanceError):
        inputs.reference_displacement_m = 2.0


@pytest.mark.parametrize("field", ["reference_displacement_m", "reference_resistance_kn_m"])
@pytest.mark.parametrize("value", [0.0, -1.0, math.nan, math.inf, True, "1"])
def test_invalid_reference_scales_are_rejected(field, value):
    with pytest.raises(ValueError):
        illustrative_inputs(**{field: value})


@pytest.mark.parametrize("changes", [
    dict(displacement_ratios=(0.0,)),
    dict(displacement_ratios=(0.0, 1.0)),
    dict(displacement_ratios=(0.0, 0.25, 0.25, 2.0, 4.0)),
    dict(displacement_ratios=(0.0, 1.0, 0.25, 2.0, 4.0)),
    dict(displacement_ratios=(0.1, 0.25, 1.0, 2.0, 4.0)),
    dict(resistance_ratios=(0.1, 0.4, 0.6, 1.0, 0.8)),
    dict(resistance_ratios=(0.0, -0.4, 0.6, 1.0, 0.8)),
    dict(resistance_ratios=(0.0, math.nan, 0.6, 1.0, 0.8)),
    dict(displacement_ratios=(0.0, math.inf, 1.0, 2.0, 4.0)),
    dict(resistance_ratios=(0.0, True, 0.6, 1.0, 0.8)),
    dict(reference_displacement_m=-0.04,
         displacement_ratios=(0.0, -0.25, -1.0, -2.0, -4.0)),
    dict(reference_resistance_kn_m=-200.0,
         resistance_ratios=(0.0, -0.4, -0.6, -1.0, -0.8)),
])
def test_malformed_ordinate_tables_are_rejected(changes):
    with pytest.raises(ValueError):
        tabulated_py_curve(illustrative_inputs(**changes))


@pytest.mark.parametrize("changes", [
    dict(reference_displacement_m=1e308),
    dict(reference_resistance_kn_m=1e308,
         resistance_ratios=(0.0, 0.4, 0.6, 2.0, 0.8)),
    dict(reference_displacement_m=5e-324),
])
def test_numerically_unrepresentable_scaled_curve_is_rejected(changes):
    with pytest.raises(ValueError):
        tabulated_py_curve(illustrative_inputs(**changes))


def test_existing_axial_method_scales_peak_and_residual_stress():
    # Illustrative example: invented mobilization points, not a standard table.
    curve = clay_tz(
        ultimate_kpa=80.0, diameter_m=2.0,
        normalized_points=((0.0, 0.0), (0.002, 0.4), (0.006, 1.0)),
        residual_ratio=0.5, residual_displacement_ratio=0.03,
    )
    assert curve.displacements_m == pytest.approx((0.0, 0.004, 0.012, 0.06))
    assert curve.resistances == pytest.approx((0.0, 32.0, 80.0, 40.0))
    midpoint_stress_kpa = 80.0 + (40.0 - 80.0) / 2.0
    assert curve.resistance_at(0.036) == pytest.approx(midpoint_stress_kpa)
    assert curve.resistance_at(-0.036) == pytest.approx(-midpoint_stress_kpa)
    # kPa * circumference [m] gives kN/m, not integrated shaft force.
    assert curve.resistance_at(0.012) * math.pi * 2.0 == pytest.approx(160.0 * math.pi)
    assert curve.resistance_at(1.0) == pytest.approx(40.0)


@pytest.mark.parametrize("changes", [
    dict(ultimate_kpa=0.0), dict(diameter_m=-1.0),
    dict(residual_ratio=1.1), dict(residual_displacement_ratio=0.004),
    dict(normalized_points=((0.0, 0.0), (0.002, 0.4))),
])
def test_existing_axial_method_rejects_invalid_inputs(changes):
    values = dict(
        ultimate_kpa=80.0, diameter_m=2.0,
        normalized_points=((0.0, 0.0), (0.002, 0.4), (0.006, 1.0)),
        residual_ratio=0.5, residual_displacement_ratio=0.03,
    )
    values.update(changes)
    with pytest.raises(ValueError):
        clay_tz(**values)


def test_existing_continuous_soft_clay_method_keeps_exact_cube_root():
    soil = SoftClay(su_kpa=80.0, effective_unit_weight_kn_m3=8.0, eps50=0.016, j=0.4)
    depth_m, diameter_m = 4.0, 2.0
    expected_ultimate = min(
        (3.0 * 80.0 + 8.0 * depth_m) * diameter_m + 0.4 * 80.0 * depth_m,
        9.0 * 80.0 * diameter_m,
    )
    y50_m = 2.5 * 0.016 * diameter_m
    points = (0.0, y50_m / 4.0, y50_m, 8.0 * y50_m, 10.0 * y50_m)
    result = soft_clay_py(soil, depth_m=depth_m, diameter_m=diameter_m, displacements_m=points)
    expected = (0.0, expected_ultimate / 2.0 * (1.0 / 4.0) ** (1.0 / 3.0),
                expected_ultimate / 2.0, expected_ultimate, expected_ultimate)
    assert result.ultimate_kn_m == pytest.approx(expected_ultimate)
    assert result.y50_m == pytest.approx(y50_m)
    assert result.curve.resistances == pytest.approx(expected)
