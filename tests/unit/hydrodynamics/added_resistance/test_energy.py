# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Hand integration and kinematic identities; no empirical force is inferred."""

import math

import pytest

from digitalmodel.hydrodynamics.added_resistance import (
    OperatingPoint,
    RegularWaves,
    Ship,
    Status,
)
from digitalmodel.hydrodynamics.added_resistance.energy import (
    radiated_energy_table,
    relative_vertical_velocity,
    sectional_energy,
)


def test_three_section_hand_integration():
    """Independent harmonic work calculation, x = [0,2,4] m.

    b = [2,4,2] N s/m² and velocity amplitudes = [1,2i,-1] m/s.
    b|v|² = [2,16,2] N/s; trapezoids = 18+18 = 36 W before
    the harmonic average <cos²> = 1/2: P = 18 W.
    At encounter frequency pi rad/s, period = 2 s, E = 36 J.
    Identity: E * omega_e / (2*pi) = P, from integral of b*v(t)².
    """
    result = sectional_energy([0, 2, 4], [2, 4, 2], [1, 2j, -1], math.pi)
    assert result.mean_power_w == pytest.approx(18)
    assert result.energy_per_cycle_j == pytest.approx(36)
    assert result.energy_per_cycle_j * math.pi / (2 * math.pi) == pytest.approx(18)


def test_relative_velocity_uses_complex_phase_and_pitch_sign():
    """With exp(i*omega_e*t), velocity is i*omega_e*(h+x*p)*a - w*a.

    x=[-1,0,1], h=i m/m, p=1 rad/m, omega_e=2, a=3 m,
    w=[1,1,1] m/s per metre. Result: [-9-6i,-9,-9+6i] m/s.
    x is forward from the RAO origin; positive pitch raises the bow.
    """
    velocity = relative_vertical_velocity([-1, 0, 1], 3, 2, 1j, 1, [1, 1, 1])
    assert velocity == pytest.approx([-9 - 6j, -9, -9 + 6j])


@pytest.mark.parametrize(
    "damping,velocity", [([0, 0, 0], [1, 2, 3]), ([1, 2, 3], [0, 0, 0])]
)
def test_zero_energy_controls(damping, velocity):
    assert sectional_energy([-1, 0, 1], damping, velocity, 1).mean_power_w == 0


@pytest.mark.parametrize(
    "x,b,v,frequency",
    [
        ([0], [1], [1], 1),
        ([0, 0], [1, 1], [1, 1], 1),
        ([1, 0], [1, 1], [1, 1], 1),
        ([0, 1], [-1, 1], [1, 1], 1),
        ([0, 1], [1], [1, 1], 1),
        ([0, 1], [1, 1], [complex(float("nan"), 1), 1], 1),
        ([0, 1], [1, 1], [1, 1], 0),
    ],
)
def test_invalid_energy_inputs(x, b, v, frequency):
    with pytest.raises(ValueError):
        sectional_energy(x, b, v, frequency)


@pytest.mark.parametrize("heading", [0, 30, 90, 180])
def test_unavailable_radiated_source_never_produces_zero_force(heading):
    """No read source establishes the conversion or oblique-sea validity.

    Missing source is explicit at every heading, including head seas.
    """
    rows = radiated_energy_table(
        Ship(100, 20, 80, 6, 1000, 10),
        OperatingPoint(5, heading),
        RegularWaves([50, 100], [1, 2]),
        heave_rao=[0j, 0j],
        pitch_rao_rad_per_m=[0j, 0j],
    )
    assert len(rows) == 2
    assert all(row.status == Status.NOT_EVALUATED for row in rows)
    assert all(row.resistance_n is None and row.transfer is None for row in rows)
    assert all("source" in row.reason for row in rows)


def test_radiated_raos_are_required_and_validated():
    args = Ship(100, 20, 80, 6, 1000, 10), OperatingPoint(5, 0), RegularWaves([50], [1])
    with pytest.raises(TypeError):
        radiated_energy_table(*args)
    with pytest.raises(ValueError):
        radiated_energy_table(*args, heave_rao=[0j, 0j], pitch_rao_rad_per_m=[0j])
    with pytest.raises(ValueError):
        radiated_energy_table(*args, heave_rao=[float("inf")], pitch_rao_rad_per_m=[0j])
