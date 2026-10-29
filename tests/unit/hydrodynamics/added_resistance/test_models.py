# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic inputs are chosen for hand arithmetic, not design acceptance."""

from dataclasses import replace

import pytest

from digitalmodel.hydrodynamics.added_resistance import (
    OperatingPoint,
    RegularWaves,
    Ship,
)


def hull():
    return Ship(100, 20, 80, 6, 1000, 10)


def test_required_ship_inputs():
    assert hull().beam_m == 20
    with pytest.raises(TypeError):
        Ship(100, 20, 80, 6)


@pytest.mark.parametrize(
    "field",
    [
        "length_pp_m",
        "beam_m",
        "bow_length_m",
        "draft_m",
        "density_kg_m3",
        "gravity_m_s2",
    ],
)
@pytest.mark.parametrize("value", [0, -1, float("nan"), float("inf")])
def test_bad_ship_values(field, value):
    with pytest.raises(ValueError):
        replace(hull(), **{field: value})


def test_geometry_consistency():
    with pytest.raises(ValueError):
        replace(hull(), bow_length_m=101)


@pytest.mark.parametrize(
    "speed,heading", [(-1, 0), (float("nan"), 0), (1, 181), (1, float("inf"))]
)
def test_bad_operating_point(speed, heading):
    with pytest.raises(ValueError):
        OperatingPoint(speed, heading)


@pytest.mark.parametrize(
    "wavelength,amplitude",
    [([], []), ([1, 2], [1]), ([0], [1]), ([1], [0]), ([1], [float("nan")])],
)
def test_bad_regular_table(wavelength, amplitude):
    with pytest.raises(ValueError):
        RegularWaves(wavelength, amplitude)


def test_input_arrays_are_copied():
    lengths = [100]
    waves = RegularWaves(lengths, [1])
    lengths[0] = -1
    assert waves.wavelength_m == (100,)
