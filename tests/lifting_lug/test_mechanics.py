# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent nominal-stress calculations for a synthetic single plate."""

import pytest

from digitalmodel.lifting_lug.mechanics import plate_demands_mpa


def test_plate_demands_hand_calculation():
    """MIT Stress-Strain Curves (2001), Eq. 1; Shear and Torsion (2000), p. 2.

    Synthetic inputs: P=100 kN, t=20 mm, pin=40 mm, hole=50 mm,
    width=150 mm and centre-to-loaded-edge=75 mm. SI prefix: 100 kN
    =100000 N. Projected bearing area=40*20=800 mm²; net area=
    (150-50)*20=2000 mm²; two shear planes=2*(75-50/2)*20=2000 mm².
    Thus nominal stresses are 125, 50 and 50 N/mm² (=MPa).
    These are force/area idealisations, not code capacities.
    """
    result = plate_demands_mpa(100, 20, 40, 50, 150, 75)
    assert result.pin_bearing == pytest.approx(125)
    assert result.net_tension == pytest.approx(50)
    assert result.shear_tearout == pytest.approx(50)


def test_unequal_net_and_shear_areas():
    """Same references; W=200 gives An=3000, e=100 gives As=3000 mm².

    Change only e to 50 mm: As=2*(50-25)*20=1000 mm².
    P=120000 N gives bearing=150, tension=40 and shear=120 MPa.
    """
    result = plate_demands_mpa(120, 20, 40, 50, 200, 50)
    assert (result.pin_bearing, result.net_tension, result.shear_tearout) == (
        150, 40, 120
    )


@pytest.mark.parametrize("index,value", [
    (0, -1), (1, 0), (2, 0), (2, 51), (3, 0), (4, 50), (5, 25),
    (0, float("nan")), (1, float("inf")), (5, float("nan")),
])
def test_invalid_geometry_and_load_rejected(index, value):
    inputs = [100, 20, 40, 50, 150, 75]
    inputs[index] = value
    with pytest.raises(ValueError):
        plate_demands_mpa(*inputs)


def test_unloaded_plate_has_zero_demand():
    assert plate_demands_mpa(0, 20, 40, 50, 150, 75).pin_bearing == 0
