"""Illustrative example inputs; independent formulas and limiting identities."""

from dataclasses import FrozenInstanceError, fields, replace
from decimal import Decimal, localcontext
import math

import pytest

from digitalmodel.structural.stress import material_curve as curve
from digitalmodel.structural.stress.stress_strain import LinearElasticModel


@pytest.fixture
def material():
    """Illustrative example, with all material coefficients explicitly supplied."""
    return curve.MaterialCurveInputs(
        yield_stress_pa=300e6,
        ultimate_stress_pa=500e6,
        elastic_modulus_pa=200e9,
        yield_offset_strain_ratio=0.003,
        proportional_strain_ratio=0.0001,
        hardening_coefficient_ratio=0.8,
    )


def hand_parameters(material):
    """Hand equations evaluated with independent, high precision arithmetic."""
    with localcontext() as context:
        context.prec = 55
        d = lambda value: Decimal(str(value))
        y, u = d(material.yield_stress_pa), d(material.ultimate_stress_pa)
        offset = d(material.yield_offset_strain_ratio)
        proportional = d(material.proportional_strain_ratio)
        ratio = y / u
        micro = (ratio.ln() + proportional - offset) / (
            ((1 + proportional).ln() / (1 + offset).ln()).ln()
        )
        macro = d(material.hardening_coefficient_ratio) * (1 - ratio)
        blend = d(1.5) * ratio ** d(1.5) - d(0.5) * ratio ** d(2.5) - ratio ** d(3.5)
        ultimate_true = u * macro.exp()
        return {
            "strength_ratio": float(ratio),
            "micro_exponent_ratio": float(micro),
            "macro_exponent_ratio": float(macro),
            "micro_scale_pa": float(y * (1 + offset) / ((1 + offset).ln() ** micro)),
            "macro_scale_pa": float(ultimate_true / macro ** macro),
            "blend_parameter_ratio": float(blend),
            "true_ultimate_stress_pa": float(ultimate_true),
        }


def test_parameters_match_hand_formulas(material):
    actual = curve.calculate_curve_parameters(material)
    for field, expected in hand_parameters(material).items():
        assert getattr(actual, field) == pytest.approx(expected, rel=2e-12)


def test_nearly_equal_strengths_retain_parameter_precision(material):
    close_strengths = replace(material, yield_stress_pa=499999999.5)
    actual = curve.calculate_curve_parameters(close_strengths)
    for field, expected in hand_parameters(close_strengths).items():
        assert getattr(actual, field) == pytest.approx(expected, rel=2e-12, abs=0.0)


def test_origin_and_reused_elastic_component(material):
    origin = curve.calculate_true_strain_components(0.0, material)
    assert origin.total_strain_ratio == 0.0
    for field in fields(origin):
        if field.name != "blend_argument_ratio":
            assert getattr(origin, field.name) == 0.0
    assert math.isfinite(origin.blend_argument_ratio)
    point = curve.calculate_true_strain_components(100e6, material)
    assert point.elastic_strain_ratio == LinearElasticModel().calculate_strain(
        100e6, elastic_modulus=200e9
    )


@pytest.mark.parametrize("stress_pa", [100e6, 250e6, 350e6, 450e6, 750e6])
def test_components_match_independent_decimal_blend(material, stress_pa):
    parameters = hand_parameters(material)
    with localcontext() as context:
        context.prec = 55
        d = lambda value: Decimal(str(value))
        stress = d(stress_pa)
        gap = d(material.ultimate_stress_pa) - d(material.yield_stress_pa)
        width = d(parameters["blend_parameter_ratio"]) * gap
        h = 2 * (stress - d(material.yield_stress_pa) - width) / width
        # tanh(H) = (exp(2 H) - 1) / (exp(2 H) + 1).
        weight = (1 + (2 * h).exp()) ** -1
        micro = (stress / d(parameters["micro_scale_pa"])) ** (
            1 / d(parameters["micro_exponent_ratio"])
        )
        macro = (stress / d(parameters["macro_scale_pa"])) ** (
            1 / d(parameters["macro_exponent_ratio"])
        )
        expected = {
            "blend_argument_ratio": h,
            "micro_power_strain_ratio": micro,
            "macro_power_strain_ratio": macro,
            "micro_strain_ratio": micro * weight,
            "macro_strain_ratio": macro * (1 - weight),
            "total_strain_ratio": stress / d(material.elastic_modulus_pa)
            + micro * weight + macro * (1 - weight),
        }
    result = curve.calculate_true_strain_components(stress_pa, material)
    for field, value in expected.items():
        assert getattr(result, field) == pytest.approx(float(value), rel=2e-11, abs=1e-14)


def test_transition_is_equal_weight_blend(material):
    parameters = hand_parameters(material)
    transition_pa = material.yield_stress_pa + parameters["blend_parameter_ratio"] * (
        material.ultimate_stress_pa - material.yield_stress_pa
    )
    result = curve.calculate_true_strain_components(transition_pa, material)
    assert result.blend_argument_ratio == pytest.approx(0.0, abs=1e-14)
    assert result.micro_strain_ratio == pytest.approx(result.micro_power_strain_ratio / 2)
    assert result.macro_strain_ratio == pytest.approx(result.macro_power_strain_ratio / 2)


def test_stress_scale_invariance(material):
    scaled = replace(material, yield_stress_pa=3e6, ultimate_stress_pa=5e6,
                     elastic_modulus_pa=2e9)
    first = curve.calculate_true_strain_components(350e6, material)
    second = curve.calculate_true_strain_components(3.5e6, scaled)
    for field in fields(first):
        assert getattr(first, field.name) == pytest.approx(getattr(second, field.name), rel=1e-12)


def test_engineering_conversion_uses_stretch_identity():
    point = curve.engineering_stress_strain(400e6, math.log(2))
    assert point.stress_pa == pytest.approx(200e6)
    assert point.strain_ratio == pytest.approx(1.0)
    assert curve.engineering_stress_strain(0.0, 0.0).strain_ratio == 0.0
    assert curve.engineering_stress_strain(0.0, 0.0).stress_pa == 0.0


def test_engineering_conversion_retains_tiny_strain():
    point = curve.engineering_stress_strain(1.0, 1e-14)
    assert point.strain_ratio == pytest.approx(1e-14, rel=1e-12, abs=0.0)


def test_input_and_outputs_are_frozen(material):
    with pytest.raises(FrozenInstanceError):
        material.yield_stress_pa = 1.0
    with pytest.raises(FrozenInstanceError):
        curve.calculate_curve_parameters(material).micro_scale_pa = 1.0
    with pytest.raises(FrozenInstanceError):
        curve.calculate_true_strain_components(0.0, material).elastic_strain_ratio = 1.0
    with pytest.raises(FrozenInstanceError):
        curve.engineering_stress_strain(0.0, 0.0).stress_pa = 1.0


@pytest.mark.parametrize("field", [
    "yield_stress_pa", "ultimate_stress_pa", "elastic_modulus_pa",
    "yield_offset_strain_ratio", "proportional_strain_ratio", "hardening_coefficient_ratio",
])
@pytest.mark.parametrize("invalid", [0.0, -1.0, math.nan, math.inf, -math.inf, True, "bad"])
def test_inputs_reject_invalid_numbers(material, field, invalid):
    with pytest.raises(ValueError):
        replace(material, **{field: invalid})


@pytest.mark.parametrize("changes", [
    {"yield_stress_pa": 500e6}, {"yield_stress_pa": 600e6},
    {"proportional_strain_ratio": 0.003},
    {"proportional_strain_ratio": 0.01},
])
def test_inputs_reject_singular_or_reversed_intervals(material, changes):
    with pytest.raises(ValueError):
        replace(material, **changes)


@pytest.mark.parametrize("invalid", [-1.0, math.nan, math.inf, True, "bad"])
def test_true_strain_rejects_invalid_stress(material, invalid):
    with pytest.raises(ValueError):
        curve.calculate_true_strain_components(invalid, material)


@pytest.mark.parametrize("stress,strain", [
    (-1.0, 0.0), (0.0, -1.0), (math.inf, 0.0), (0.0, math.nan),
    (True, 0.0), (0.0, "bad"), (1.0, 1000.0),
])
def test_engineering_conversion_rejects_invalid_or_overflowing_values(stress, strain):
    with pytest.raises(ValueError):
        curve.engineering_stress_strain(stress, strain)


def test_parameter_and_power_overflows_are_reported(material):
    extreme = replace(material, hardening_coefficient_ratio=1e5)
    with pytest.raises(ValueError, match="representable"):
        curve.calculate_curve_parameters(extreme)
    with pytest.raises(ValueError, match="representable"):
        curve.calculate_true_strain_components(1e300, material)


def test_all_material_lookup_values_are_required():
    with pytest.raises(TypeError):
        curve.MaterialCurveInputs(yield_stress_pa=300e6, ultimate_stress_pa=500e6,
                                  elastic_modulus_pa=200e9)
