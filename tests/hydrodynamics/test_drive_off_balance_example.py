# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculations for the synthetic drive-off example.

Basis: drive_off_balance.md, Synthetic assumptions and Analytical limit.
All masses, forces, times and invalid-input probes below are synthetic choices.
The factor 1/2 follows from integrating constant acceleration twice.
"""

import runpy
from pathlib import Path

import pytest


@pytest.fixture
def simulate():
    """Load the saved example without importing the hydrodynamics package."""
    path = (
        Path(__file__).resolve().parents[2]
        / "examples/domains/hydrodynamics/drive_off_balance.py"
    )
    return runpy.run_path(str(path))["simulate"]


def test_drive_off_constant_force(simulate):
    """Hand basis: (6000-2000)/2000 = 2 m/s**2 from rest.

    At 5 s, v = 2*5 = 10 m/s and x = 2*5**2/2 = 25 m.
    Uneven sample times are synthetic to expose step-size mistakes.
    """
    mass_kg, thrust_n, drag_n = 2000.0, 6000.0, 2000.0
    times_s = [0.0, 0.5, 2.0, 5.0]
    acceleration_m_s2 = (thrust_n - drag_n) / mass_kg
    states = simulate(
        mass_kg, times_s, [thrust_n] * 3, [drag_n] * 3
    )
    assert [state.time_s for state in states] == times_s
    for state, time_s in zip(states, times_s):
        assert state.velocity_m_s == pytest.approx(acceleration_m_s2 * time_s)
        assert state.position_m == pytest.approx(acceleration_m_s2 * time_s**2 / 2)
    assert states[-1].velocity_m_s / times_s[-1] == pytest.approx(
        acceleration_m_s2
    )


def test_drive_off_zero_balance(simulate):
    """Equal synthetic forces give a=(4000-4000)/2000=0 and no motion."""
    states = simulate(
        2000.0, [0.0, 1.0, 5.0], [4000.0, 4000.0], [4000.0, 4000.0]
    )
    assert all(state.position_m == 0.0 for state in states)
    assert all(state.velocity_m_s == 0.0 for state in states)


def test_drive_off_prescribed_shutdown(simulate):
    """Synthetic hand basis: m=2 kg; first a=(6-2)/2=2 for 1 s.

    Then thrust is zero and a=(0-2)/2=-1 for 2 s. Thus (v,x) changes
    from (2,1) to (2-1*2, 1+2*2-1*2**2/2) = (0,3) in SI units.
    This tests interval indexing and velocity carry-over, not shutdown physics.
    """
    states = simulate(2.0, [0.0, 1.0, 3.0], [6.0, 0.0], [2.0, 2.0])
    assert [state.velocity_m_s for state in states] == pytest.approx([0, 2, 0])
    assert [state.position_m for state in states] == pytest.approx([0, 1, 3])


@pytest.mark.parametrize(
    "mass_kg,times_s,thrust_n,drag_n",
    [
        (0.0, [0.0, 1.0], [1.0], [0.0]),
        (-1.0, [0.0, 1.0], [1.0], [0.0]),
        (float("nan"), [0.0, 1.0], [1.0], [0.0]),
        (1.0, [0.0, 0.0], [1.0], [0.0]),
        (1.0, [1.0, 2.0], [1.0], [0.0]),
        (1.0, [0.0, 1.0], [], [0.0]),
        (1.0, [0.0, 1.0], [float("nan")], [0.0]),
        (1.0, [0.0, float("inf")], [1.0], [0.0]),
    ],
)
def test_drive_off_rejects_invalid_inputs(simulate, mass_kg, times_s, thrust_n, drag_n):
    """Reject nonphysical mass, ambiguous intervals and nonfinite inputs."""
    with pytest.raises(ValueError):
        simulate(mass_kg, times_s, thrust_n, drag_n)
