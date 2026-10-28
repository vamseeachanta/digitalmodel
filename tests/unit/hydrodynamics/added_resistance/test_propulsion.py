# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent constant-power balances; no default resistance law or efficiency."""

import math

import pytest

from digitalmodel.hydrodynamics.added_resistance import Status
from digitalmodel.hydrodynamics.added_resistance.propulsion import (
    CalmWaterCurve,
    added_shaft_power,
    speed_loss,
)


def quadratic_curve():
    return CalmWaterCurve(list(range(11)), [100 * v * v for v in range(11)])


def solve(added, **overrides):
    args = dict(propulsive_efficiency=0.5, speed_tolerance_m_s=1e-8, max_iterations=100)
    args.update(overrides)
    return speed_loss(quadratic_curve(), 10, added, **args)


def test_added_power_from_definition():
    """Useful power F*U=200 N*5 m/s=1000 W; eta=1/2 gives shaft 2000 W."""
    assert added_shaft_power(200, 5, propulsive_efficiency=0.5) == pytest.approx(2000)


def test_hand_solved_quadratic_law():
    """R0=100*U² N, calm U0=10: effective power=100*10³=100000 W.

    With constant added R=6100 N, (100*8²+6100)*8=100000 W exactly.
    Shaft power at eta=1/2 is 200000 W at both speeds; loss=2 m/s.
    The supplied table contains the exact quadratic values at integer speeds;
    the root at U=8 lies on a knot, so interpolation introduces no root error.
    """
    result = solve(lambda _: 6100)
    assert result.status == Status.EVALUATED
    assert result.speed_m_s == pytest.approx(8, abs=1e-8)
    assert result.speed_loss_m_s == pytest.approx(2, abs=1e-8)
    assert result.available_shaft_power_w == pytest.approx(200000)
    assert result.required_shaft_power_w == pytest.approx(200000)


def test_speed_dependent_resistance_is_recomputed():
    """At root U=8, Radd(U)=100*U+5300=6100; same 100000 W balance."""
    seen = []

    def added(v):
        seen.append(v)
        return 100 * v + 5300

    result = solve(added)
    assert result.speed_m_s == pytest.approx(8, abs=1e-8)
    assert len(set(seen)) > 2


def test_zero_added_force_has_no_speed_loss():
    result = solve(lambda _: 0)
    assert result.speed_m_s == 10
    assert result.speed_loss_m_s == 0


def test_curve_interpolation_and_no_extrapolation():
    curve = CalmWaterCurve([0, 2, 4], [0, 200, 800])
    assert curve.resistance(3) == pytest.approx(500)
    with pytest.raises(ValueError, match="coverage"):
        curve.resistance(5)


@pytest.mark.parametrize(
    "speeds,forces",
    [
        ([0], [0]),
        ([1, 2], [0, 1]),
        ([0, 1], [1, 2]),
        ([0, 1, 1], [0, 1, 2]),
        ([0, 2, 1], [0, 1, 2]),
        ([0, 1], [0, -1]),
        ([0, 1, 2], [0, 2, 1]),
        ([0, math.inf], [0, 1]),
        ([0, 1], [0, math.nan]),
        ([0, 1], [0]),
    ],
)
def test_invalid_calm_curve(speeds, forces):
    with pytest.raises(ValueError):
        CalmWaterCurve(speeds, forces)


@pytest.mark.parametrize("efficiency", [0, -0.1, 1.01, math.nan, math.inf])
def test_efficiency_required_and_physical(efficiency):
    with pytest.raises(ValueError):
        added_shaft_power(100, 5, propulsive_efficiency=efficiency)
    with pytest.raises(ValueError):
        solve(lambda _: 100, propulsive_efficiency=efficiency)


def test_missing_inputs_do_not_create_defaults():
    with pytest.raises(TypeError):
        added_shaft_power(100, 5)
    with pytest.raises(TypeError):
        speed_loss(quadratic_curve(), 10, lambda _: 100)


@pytest.mark.parametrize("force", [-1, math.nan, math.inf])
def test_bad_added_curve_results_rejected(force):
    with pytest.raises(ValueError):
        solve(lambda _: force)


@pytest.mark.parametrize(
    "key,value",
    [
        ("speed_tolerance_m_s", 0),
        ("speed_tolerance_m_s", math.nan),
        ("max_iterations", 0),
        ("max_iterations", 1.5),
    ],
)
def test_bad_iteration_controls(key, value):
    with pytest.raises(ValueError):
        solve(lambda _: 100, **{key: value})


def test_nonconvergence_is_not_a_speed_prediction():
    result = solve(lambda _: 6100, max_iterations=1)
    assert result.status == Status.NOT_EVALUATED
    assert result.speed_m_s is None
    assert result.speed_loss_m_s is None
    assert "converge" in result.reason


def test_calm_speed_outside_curve_and_zero_power_rejected():
    with pytest.raises(ValueError):
        speed_loss(
            quadratic_curve(),
            11,
            lambda _: 100,
            propulsive_efficiency=0.5,
            speed_tolerance_m_s=1e-8,
            max_iterations=100,
        )
    with pytest.raises(ValueError):
        speed_loss(
            CalmWaterCurve([0, 1], [0, 0]),
            1,
            lambda _: 100,
            propulsive_efficiency=0.5,
            speed_tolerance_m_s=1e-8,
            max_iterations=100,
        )
