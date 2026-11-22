"""Illustrative example inputs, closed-form identities and limiting cases."""
from dataclasses import FrozenInstanceError
import math

import pytest

from digitalmodel.naval_architecture.tank_sloshing import (
    TankGeometryInput, TankDepthInput, SloshingInput, FrequencyInput,
    PeriodRatioInput, TankCaseInput, TankModelInput, available_depth_m,
    liquid_depth_m, tank_natural_frequency_hz, natural_period_s,
    period_ratio, calculate_tank_response,
)


def test_frequency_delegates_to_rectangular_tank_periods(monkeypatch):
    calls = []

    def periods(length, fill_depth, n_modes, g):
        calls.append((length, fill_depth, n_modes, g))
        return [4.0]

    monkeypatch.setattr(
        "digitalmodel.naval_architecture.tank_sloshing.rectangular_tank_periods",
        periods,
        raising=False,
    )
    assert tank_natural_frequency_hz(SloshingInput(5, 2, 10)) == 0.25
    assert calls == [(5, 2, 1, 10)]


def test_depth_frequency_period_and_ratio_hand_equation():
    geometry = TankGeometryInput(8, 2)
    depth = 0.4 * (8 - 2)
    expected_frequency = math.sqrt(math.pi * 10 / 5 * math.tanh(math.pi * depth / 5)) / (2 * math.pi)
    assert available_depth_m(geometry) == 6
    assert liquid_depth_m(TankDepthInput(geometry, 0.4)) == pytest.approx(depth)
    assert tank_natural_frequency_hz(SloshingInput(5, depth, 10)) == pytest.approx(expected_frequency)
    assert natural_period_s(FrequencyInput(expected_frequency)) == pytest.approx(1 / expected_frequency)
    assert period_ratio(PeriodRatioInput(8, 1 / expected_frequency)) == pytest.approx(8 * expected_frequency)
    model = TankModelInput(8, 10, geometry, (TankCaseInput(0.4, 5), TankCaseInput(0.6, 7)))
    response = calculate_tank_response(model)
    assert len(response) == 2
    assert response[0].liquid_depth_m == pytest.approx(depth)
    assert response[0].frequency_hz == pytest.approx(expected_frequency)
    assert response[0].period_s == pytest.approx(1 / expected_frequency)
    assert response[0].ship_to_tank_period_ratio_dimensionless == pytest.approx(8 * expected_frequency)
    with pytest.raises(FrozenInstanceError):
        model.gravity_m_s2 = 11
    with pytest.raises(TypeError):
        TankModelInput()


def test_deep_and_shallow_water_limits_and_full_fill():
    # tanh(kh) tends to 1 for deep water, and kh for shallow water.
    deep = tank_natural_frequency_hz(SloshingInput(5, 500, 10))
    assert deep == pytest.approx(math.sqrt(math.pi * 10 / 5) / (2 * math.pi))
    shallow = tank_natural_frequency_hz(SloshingInput(5, 1e-7, 10))
    assert shallow == pytest.approx(math.sqrt(10 * 1e-7) / (2 * 5), rel=1e-12)
    assert liquid_depth_m(TankDepthInput(TankGeometryInput(8, 2), 1)) == 6


@pytest.mark.parametrize('inputs,function', [
    (TankGeometryInput(2, 2), available_depth_m),
    (TankGeometryInput(2, 3), available_depth_m),
    (TankGeometryInput(8, -1), available_depth_m),
    (TankGeometryInput(math.nan, 2), available_depth_m),
    (TankDepthInput(TankGeometryInput(8, 2), 0), liquid_depth_m),
    (TankDepthInput(TankGeometryInput(8, 2), 1.2), liquid_depth_m),
    (TankDepthInput(TankGeometryInput(8, 2), True), liquid_depth_m),
    (SloshingInput(0, 2, 10), tank_natural_frequency_hz),
    (SloshingInput(5, 0, 10), tank_natural_frequency_hz),
    (SloshingInput(5, 2, math.inf), tank_natural_frequency_hz),
    (SloshingInput(5, 2, -10), tank_natural_frequency_hz),
    (FrequencyInput(0), natural_period_s),
    (FrequencyInput(math.nan), natural_period_s),
    (PeriodRatioInput(8, 0), period_ratio),
    (PeriodRatioInput(-8, 2), period_ratio),
    (TankModelInput(8, 10, TankGeometryInput(8, 2), ()), calculate_tank_response),
])
def test_invalid_inputs(inputs, function):
    with pytest.raises(ValueError):
        function(inputs)
