"""Independent hand formulas with illustrative example inputs only."""
import math
from dataclasses import FrozenInstanceError

import numpy as np
import pytest

from digitalmodel.structural.fatigue import sn_definitions
from digitalmodel.structural.fatigue.sn_definitions import (
    SNDefinition,
    ThicknessDefinition,
    cycles_to_failure,
    intercept_cycles_pa_m,
    stress_range_pa,
    thickness_stress_factor,
    nominal_stress_range_pa,
)


def test_illustrative_single_branch_cycles_without_cutoff():
    # Illustrative example: intercept in cycles * Pa**m, stress range in Pa.
    definition = SNDefinition(26.0, 2.5)
    stress_pa = 40_000_000.0
    expected_cycles = 10.0**26 / stress_pa**2.5
    assert cycles_to_failure(definition, stress_pa) == pytest.approx(
        expected_cycles, rel=1e-13
    )
    # The branch is extrapolated without the existing evaluator's default cap.
    assert cycles_to_failure(definition, stress_pa / 100) > 1e9


def test_illustrative_unit_boundary_and_inverse():
    # Illustrative example, MPa boundary. A_Pa = A_MPa * (10**6)**m.
    definition = SNDefinition.from_log10_intercept(8.0, 2.5, stress_unit_pa=1e6)
    assert definition.log10_intercept_cycles_pa_m == 8.0 + 6.0 * 2.5
    expected_a = 10.0**8 * (1e6)**2.5
    assert intercept_cycles_pa_m(definition) == pytest.approx(expected_a)
    cycles = 20_000.0
    expected_stress_pa = (expected_a / cycles)**(1 / 2.5)
    assert stress_range_pa(definition, cycles) == pytest.approx(expected_stress_pa)
    assert cycles_to_failure(definition, expected_stress_pa) == pytest.approx(cycles)


def test_illustrative_arbitrary_unit_scale_preserves_cycles():
    # Illustrative unit = two Pa; not a physical unit conversion approximation.
    definition = SNDefinition.from_log10_intercept(8.0, 2.5, stress_unit_pa=2.0)
    expected_cycles = 10.0**8 / 40.0**2.5
    assert cycles_to_failure(definition, 80.0) == pytest.approx(expected_cycles)


@pytest.mark.parametrize("actual_m", [0.010, 0.020, 0.080])
def test_illustrative_thickness_and_scf(actual_m):
    # Illustrative example; no benefit below reference thickness.
    thickness = ThicknessDefinition(actual_m, 0.020, 0.5)
    expected_factor = max(1.0, math.sqrt(actual_m / 0.020))
    assert thickness_stress_factor(thickness) == pytest.approx(expected_factor)
    branch_stress_pa = 40e6
    assert nominal_stress_range_pa(
        branch_stress_pa, thickness, scf_dimensionless=1.5
    ) == pytest.approx(branch_stress_pa / expected_factor / 1.5)


def test_illustrative_zero_thickness_exponent():
    thickness = ThicknessDefinition(0.080, 0.020, 0.0)
    assert thickness_stress_factor(thickness) == 1.0


@pytest.mark.parametrize("value", [0.0, -2.0, math.inf, -math.inf, math.nan, True, "bad"])
def test_invalid_positive_inputs_are_rejected(value):
    with pytest.raises(ValueError):
        SNDefinition(26.0, value)
    with pytest.raises(ValueError):
        SNDefinition.from_log10_intercept(8.0, 2.5, stress_unit_pa=value)
    definition = SNDefinition(26.0, 2.5)
    with pytest.raises(ValueError):
        cycles_to_failure(definition, value)
    with pytest.raises(ValueError):
        stress_range_pa(definition, value)
    with pytest.raises(ValueError):
        ThicknessDefinition(value, 0.020, 0.5)
    with pytest.raises(ValueError):
        ThicknessDefinition(0.020, value, 0.5)
    with pytest.raises(ValueError):
        nominal_stress_range_pa(40e6, ThicknessDefinition(0.020, 0.020, 0.5),
                                scf_dimensionless=value)
    with pytest.raises(ValueError):
        nominal_stress_range_pa(value, ThicknessDefinition(0.020, 0.020, 0.5),
                                scf_dimensionless=1.5)


@pytest.mark.parametrize("value", [-2.0, math.inf, math.nan, True, "bad"])
def test_invalid_thickness_exponent(value):
    with pytest.raises(ValueError):
        ThicknessDefinition(0.020, 0.020, value)


@pytest.mark.parametrize("value", [math.inf, -math.inf, math.nan, True, "bad", 400.0, -400.0])
def test_invalid_or_unrepresentable_intercept(value):
    with pytest.raises(ValueError):
        SNDefinition(value, 2.5)
    with pytest.raises(ValueError):
        SNDefinition.from_log10_intercept(value, 2.5, stress_unit_pa=1.0)


def test_definition_records_are_frozen():
    definition = SNDefinition(26.0, 2.5)
    with pytest.raises(FrozenInstanceError):
        definition.slope_dimensionless = 2.0
    thickness = ThicknessDefinition(0.020, 0.020, 0.5)
    with pytest.raises(FrozenInstanceError):
        thickness.actual_thickness_m = 0.080


def test_unrepresentable_results_fail_instead_of_returning_infinity_or_zero():
    with pytest.raises(ValueError):
        cycles_to_failure(SNDefinition(26.0, 2.5), 1e-200)
    with pytest.raises(ValueError):
        stress_range_pa(SNDefinition(300.0, 2.5), 1e-100)
    with pytest.raises(ValueError):
        thickness_stress_factor(ThicknessDefinition(1e300, 1e-300, 0.5))
    with pytest.raises(ValueError):
        nominal_stress_range_pa(1e-300, ThicknessDefinition(0.080, 0.020, 0.5),
                                scf_dimensionless=1e200)


@pytest.mark.parametrize("slope_type", [np.uint8, np.uint16, np.uint32, np.uint64])
@pytest.mark.parametrize("convert_units", [False, True])
def test_unsigned_slope_preserves_cycles_and_inverse(slope_type, convert_units):
    slope = slope_type(3)
    if convert_units:
        definition = SNDefinition.from_log10_intercept(
            0.0, slope, stress_unit_pa=10.0,
        )
    else:
        definition = SNDefinition(3.0, slope)
    # A = 1000 cycles * Pa**3; N = 1000 / 2**3 = 125 cycles.
    assert cycles_to_failure(definition, 2.0) == pytest.approx(125.0)
    assert stress_range_pa(definition, 125.0) == pytest.approx(2.0)


@pytest.mark.parametrize("operation", ["cycles", "stress", "thickness"])
def test_delegated_validation_message_is_preserved(monkeypatch, operation):
    error = ValueError("delegated input must be positive")

    def reject(*args, **kwargs):
        raise error

    definition = SNDefinition(3.0, 3.0)
    thickness = ThicknessDefinition(0.080, 0.020, 0.5)
    if operation == "cycles":
        monkeypatch.setattr(sn_definitions.PowerLawSNCurve, "get_allowable_cycles", reject)
        evaluate, arguments = cycles_to_failure, (definition, 2.0)
    elif operation == "stress":
        monkeypatch.setattr(sn_definitions.PowerLawSNCurve, "get_stress_range", reject)
        evaluate, arguments = stress_range_pa, (definition, 125.0)
    else:
        monkeypatch.setattr(sn_definitions, "thickness_correction", reject)
        evaluate, arguments = thickness_stress_factor, (thickness,)
    with pytest.raises(ValueError, match="delegated input must be positive") as caught:
        evaluate(*arguments)
    assert caught.value is error


def test_delegated_arithmetic_error_is_translated(monkeypatch):
    error = OverflowError("delegated arithmetic overflow")

    def overflow(*args, **kwargs):
        raise error

    monkeypatch.setattr(sn_definitions.PowerLawSNCurve, "get_allowable_cycles", overflow)
    with pytest.raises(
        ValueError, match="result is outside the floating-point range",
    ) as caught:
        cycles_to_failure(SNDefinition(3.0, 3.0), 2.0)
    assert caught.value.__cause__ is error
