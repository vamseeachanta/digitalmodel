"""Illustrative examples for existing annular area and nominal axial stress.

Load factors are applied before dividing by steel area.
These neutral examples use SI inputs and independently derived values;
they do not establish combined-stress or design-code acceptance.
"""

import math

import pytest

from digitalmodel.drilling_riser.section import cross_sectional_area
from digitalmodel.installation.pipelay.stress import axial_stress


@pytest.mark.parametrize("load_factor", [1.0, 1.5, 2.0])
def test_factored_pipe_load_uses_existing_area_and_stress(load_factor):
    # Illustrative example: OD=0.4 m, t=0.02 m, force=200 kN.
    # Independent annular identity: A=pi*t*(OD-t).
    area = cross_sectional_area(0.4, 0.4 - 2*0.02)
    expected_area_m2 = math.pi * 0.02 * 0.38
    assert area == pytest.approx(expected_area_m2)
    assert axial_stress(200_000.0 * load_factor, area) == pytest.approx(
        200_000.0 * load_factor / expected_area_m2
    )


def test_zero_and_compressive_forces_use_existing_signed_stress():
    # Illustrative example: no load gives no stress; compression is negative.
    assert axial_stress(0.0, 0.02) == 0.0
    assert axial_stress(-300_000.0, 0.02) == -15_000_000.0


@pytest.mark.parametrize("bad_area", [0.0, -0.02, float("nan"), float("inf")])
def test_existing_stress_rejects_invalid_area(bad_area):
    with pytest.raises(ValueError, match="steel_area"):
        axial_stress(200_000.0, bad_area)


@pytest.mark.parametrize("bad_force", [float("nan"), float("inf"), -float("inf")])
def test_existing_stress_rejects_nonfinite_force(bad_force):
    with pytest.raises(ValueError, match="axial_force"):
        axial_stress(bad_force, 0.02)
