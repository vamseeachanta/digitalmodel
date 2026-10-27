# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic current loads using the equation in tug.bollard_pull.escort_force.

The cited repository function documents F = 0.5 * rho * V**2 * A * C.
All inputs below are deliberately synthetic, not material or design defaults.
Zero and negative values probe the specified input boundaries; NaN and both
infinities probe finite-value validation for every input independently.
"""

import pytest

from digitalmodel.marine_ops.towing.drift_force import current_drift_force


@pytest.mark.parametrize(
    ("density", "speed", "area", "coefficient", "expected_n"),
    [
        (1000.0, 2.0, 12.0, 1.5, 36000.0),
        (800.0, 0.5, 3.0, 0.8, 240.0),
    ],
)
def test_current_drift_force_matches_dynamic_pressure(
    density, speed, area, coefficient, expected_n
):
    """Independently apply the cited escort_force quadratic-load equation.

    Case A: q = (1000 / 2) * (2 * 2) = 2000 Pa;
    F = 2000 * 1.5 * 12 = 36000 N.
    Case B: q = (800 / 2) * (0.5 * 0.5) = 100 Pa;
    F = 100 * 0.8 * 3 = 240 N.
    """
    force = current_drift_force(
        density_kg_m3=density,
        current_speed_m_s=speed,
        projected_area_m2=area,
        drag_coefficient=coefficient,
    )
    assert force == expected_n


def test_current_drift_force_scales_with_speed_squared():
    """Cited equation: q = 500 Pa at 1 m/s, 2000 Pa at 2 m/s.

    Synthetic A = 12 m2 and C = 1.5 give 9000 N and 36000 N;
    doubling speed multiplies force by (2 / 1)**2 = 4.
    """
    slow = current_drift_force(1000.0, 1.0, 12.0, 1.5)
    fast = current_drift_force(1000.0, 2.0, 12.0, 1.5)
    assert slow == 9000.0
    assert fast == 36000.0
    assert fast == 4.0 * slow


@pytest.mark.parametrize(
    "zero_input", ["current_speed_m_s", "projected_area_m2", "drag_coefficient"]
)
def test_current_drift_force_is_zero_for_zero_factor(zero_input):
    """The cited product is zero when speed, area or coefficient is zero."""
    inputs = dict(
        density_kg_m3=1000.0,
        current_speed_m_s=2.0,
        projected_area_m2=12.0,
        drag_coefficient=1.5,
    )
    inputs[zero_input] = 0.0
    assert current_drift_force(**inputs) == 0.0


@pytest.mark.parametrize(
    "input_name",
    ["density_kg_m3", "current_speed_m_s", "projected_area_m2", "drag_coefficient"],
)
@pytest.mark.parametrize("invalid", [-1.0, float("nan"), float("inf"), -float("inf")])
def test_current_drift_force_rejects_invalid_inputs(input_name, invalid):
    """Each input must be finite and meet its stated sign constraint."""
    inputs = dict(
        density_kg_m3=1000.0,
        current_speed_m_s=2.0,
        projected_area_m2=12.0,
        drag_coefficient=1.5,
    )
    inputs[input_name] = invalid
    with pytest.raises(ValueError, match=input_name):
        current_drift_force(**inputs)


def test_current_drift_force_rejects_zero_density():
    """Density is strictly positive, including when the current is zero."""
    with pytest.raises(ValueError, match="density_kg_m3"):
        current_drift_force(0.0, 0.0, 12.0, 1.5)
