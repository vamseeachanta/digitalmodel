"""Illustrative examples; expected values follow independent hand calculations."""

import math
from dataclasses import FrozenInstanceError, replace

import numpy as np
import pytest

from digitalmodel.geotechnical.lateral_soil_response import (
    ClayPoint,
    clay_characteristic_displacement_m,
    clay_transition_depth_m,
    clay_ultimate_pressure_pa,
)


def test_clay_shallow_and_deep_limits():
    # Illustrative example: SI quantities, chosen independently.
    point = ClayPoint(2.0, 1.0, 8000.0, 20000.0, 0.02, 0.4)
    assert clay_characteristic_displacement_m(point) == pytest.approx(0.05)
    transition = 6 * 1.0 * 20000 / (8000 * 1.0 + 0.4 * 20000)
    assert clay_transition_depth_m(point) == pytest.approx(transition)
    shallow = 3 * 20000 + 8000 * 2 + 0.4 * 20000 * 2 / 1.0
    assert clay_ultimate_pressure_pa(point) == pytest.approx(shallow)
    deep = replace(point, depth_m=20.0)
    assert clay_ultimate_pressure_pa(deep) == 9 * 20000
    assert clay_ultimate_pressure_pa(replace(point, depth_m=transition)) == 180000


@pytest.mark.parametrize("depth_m", [np.float32(2.0), np.int64(2)])
def test_clay_accepts_numpy_real_scalars(depth_m):
    point = ClayPoint(depth_m, 1.0, 8000.0, 20000.0, 0.02, 0.4)
    assert point.depth_m == 2.0


def test_sand_capacity_and_inverse_tanh():
    from digitalmodel.geotechnical.lateral_soil_response import (
        SandPoint, sand_loading_factor_one, sand_ultimate_resistance_n_per_m,
        sand_displacement_m, sand_resistance_n_per_m,
    )

    # Illustrative example: shallow and deep capacities computed by hand.
    point = SandPoint(4.0, 2.0, 9000.0, 2.0, 4.0, 6.0, 12000000.0)
    expected = min((2 * 4 + 4 * 2) * 9000 * 4, 6 * 2 * 9000 * 4)
    assert sand_ultimate_resistance_n_per_m(point) == expected
    for cyclic, factor in [(False, 1.4), (True, 0.9)]:
        assert sand_loading_factor_one(point, cyclic=cyclic) == pytest.approx(factor)
        y = sand_displacement_m(point, resistance_fraction_one=0.8, cyclic=cyclic)
        assert y == pytest.approx(factor * expected / (12000000 * 4)
                                 * math.log(9) / 2)
        assert sand_resistance_n_per_m(point, y, cyclic=cyclic) == pytest.approx(
            0.8 * factor * expected)
        assert sand_resistance_n_per_m(point, -y, cyclic=cyclic) == pytest.approx(
            -0.8 * factor * expected)
    assert sand_resistance_n_per_m(replace(point, depth_m=0), 0.1, cyclic=False) == 0


def test_caller_supplied_fit_and_linear_properties():
    from digitalmodel.geotechnical.lateral_soil_response import (
        SandCoefficientFit, sand_coefficients_one, linear_property_at_depth,
    )

    # Illustrative polynomials: constant, linear, and quadratic identities.
    fit = SandCoefficientFit((2.0, 0.0, 0.0, 0.0), (1.0, 0.1, 0.0, 0.0),
                             (0.0, 0.0, 0.01, 0.0), 10.0, 50.0)
    assert sand_coefficients_one(30.0, fit) == pytest.approx((2, 4, 9))
    args = dict(start_depth_m=2.0, end_depth_m=6.0,
                start_value_si=4000.0, end_value_si=12000.0)
    assert linear_property_at_depth(3.0, **args, allow_extrapolation=False) == 6000
    assert linear_property_at_depth(7.0, **args, allow_extrapolation=True) == 14000
    with pytest.raises(ValueError):
        linear_property_at_depth(7.0, **args, allow_extrapolation=False)
    with pytest.raises(ValueError):
        sand_coefficients_one(60.0, fit)


def test_shared_units_convert_inches_to_metres():
    from digitalmodel.units import Q_

    diameter_m = Q_(7.5, "inch").to("m").magnitude
    point = ClayPoint(2.0, diameter_m, 8000.0, 20000.0, 0.02, 0.4)
    assert clay_characteristic_displacement_m(point) == pytest.approx(2.5 * 0.02 * 0.1905)
    assert Q_(0.0, "inch").to("m").magnitude == 0.0


@pytest.mark.parametrize("field,value", [
    ("depth_m", -1.0), ("diameter_m", 0.0), ("diameter_m", float("inf")),
    ("effective_unit_weight_n_per_m3", -1.0), ("undrained_strength_pa", 0.0),
    ("strain_at_half_strength_one", 0.0), ("empirical_j_one", -1.0),
    ("depth_m", float("nan")), ("diameter_m", True),
])
def test_clay_rejects_invalid_input(field, value):
    point = ClayPoint(2.0, 1.0, 8000.0, 20000.0, 0.02, 0.4)
    with pytest.raises(ValueError):
        replace(point, **{field: value})


@pytest.mark.parametrize("field", [
    "diameter_m", "effective_unit_weight_n_per_m3", "c1_one", "c2_one", "c3_one",
    "subgrade_modulus_n_per_m3",
])
def test_sand_requires_positive_local_properties(field):
    from digitalmodel.geotechnical.lateral_soil_response import SandPoint

    point = SandPoint(4.0, 2.0, 9000.0, 2.0, 4.0, 6.0, 12000000.0)
    with pytest.raises(ValueError):
        replace(point, **{field: 0.0})


def test_sand_shallow_branch_loading_floor_and_zero_response():
    from digitalmodel.geotechnical.lateral_soil_response import (
        SandPoint, sand_ultimate_resistance_n_per_m, sand_loading_factor_one,
        sand_displacement_m, sand_resistance_n_per_m,
    )

    point = SandPoint(4.0, 2.0, 9000.0, 2.0, 4.0, 100.0, 12000000.0)
    assert sand_ultimate_resistance_n_per_m(point) == (2 * 4 + 4 * 2) * 9000 * 4
    assert sand_loading_factor_one(replace(point, depth_m=20), cyclic=False) == 0.9
    assert sand_resistance_n_per_m(point, 0.0, cyclic=False) == 0.0
    for fraction in [-0.1, 1.0, float("nan")]:
        with pytest.raises(ValueError):
            sand_displacement_m(point, resistance_fraction_one=fraction, cyclic=False)
    with pytest.raises(ValueError):
        sand_displacement_m(replace(point, depth_m=0), resistance_fraction_one=0.8, cyclic=True)
    with pytest.raises(ValueError):
        sand_loading_factor_one(point, cyclic=1)
    with pytest.raises(ValueError):
        sand_resistance_n_per_m(point, float("inf"), cyclic=False)


def test_frozen_inputs_and_fit_validation():
    from digitalmodel.geotechnical.lateral_soil_response import (
        SandCoefficientFit, linear_property_at_depth,
    )

    point = ClayPoint(2.0, 1.0, 8000.0, 20000.0, 0.02, 0.4)
    with pytest.raises(FrozenInstanceError):
        point.depth_m = 4.0
    with pytest.raises(ValueError):
        replace(point, empirical_j_one=0, effective_unit_weight_n_per_m3=0)
    for coefficients in [(1.0,), (1.0, 2.0, 3.0, float("nan")), [1.0, 2.0, 3.0, 4.0]]:
        with pytest.raises(ValueError):
            SandCoefficientFit(coefficients, (1.0, 0.0, 0.0, 0.0),
                               (1.0, 0.0, 0.0, 0.0), 10.0, 50.0)
    with pytest.raises(ValueError):
        linear_property_at_depth(2, start_depth_m=2, end_depth_m=2,
                                 start_value_si=4, end_value_si=8, allow_extrapolation=False)


def test_signed_quantities_reject_boolean_inputs():
    from digitalmodel.geotechnical.lateral_soil_response import (
        SandPoint, sand_resistance_n_per_m, SandCoefficientFit, linear_property_at_depth,
    )

    point = SandPoint(4.0, 2.0, 9000.0, 2.0, 4.0, 6.0, 12000000.0)
    with pytest.raises(ValueError):
        sand_resistance_n_per_m(point, True, cyclic=False)
    with pytest.raises(ValueError):
        SandCoefficientFit((True, 0.0, 0.0, 0.0), (1.0, 0.0, 0.0, 0.0),
                           (1.0, 0.0, 0.0, 0.0), 10.0, 50.0)
    with pytest.raises(ValueError):
        linear_property_at_depth(2, start_depth_m=1, end_depth_m=3,
                                 start_value_si=True, end_value_si=8, allow_extrapolation=False)


@pytest.mark.parametrize("inverse", [False, True])
def test_sand_numpy_integer_products_match_hand_calculation(inverse):
    from digitalmodel.geotechnical.lateral_soil_response import (
        SandPoint, sand_displacement_m, sand_resistance_n_per_m,
    )

    point = SandPoint(20, 2, 9000, 2, 4, 6, np.int32(120000000))
    limit = 0.9 * (6 * 2 * 9000 * 20)
    if inverse:
        # atanh(0.8) = log(9)/2; k*z = 2400000000 without overflow.
        expected = limit * math.log(9) / (2 * 2400000000)
        assert sand_displacement_m(
            point, resistance_fraction_one=0.8, cyclic=True,
        ) == pytest.approx(expected)
    else:
        e = math.exp(2 * 2400000 / limit)
        expected = limit * (e - 1) / (e + 1)
        assert sand_resistance_n_per_m(point, 0.001, cyclic=True) == pytest.approx(expected)


def test_low_strength_capacity_crossover_is_not_cyclic_transition():
    from digitalmodel.geotechnical.lateral_soil_response import (
        clay_capacity_crossover_depth_m,
    )

    point = ClayPoint(1.5, 1, 10000, 2000, 0.02, 0.5)
    # OpenPile Matlock notes distinguish the capacity minimum from XR >= 2.5D.
    assert clay_capacity_crossover_depth_m(point) == pytest.approx(12000 / 11000)
    assert clay_transition_depth_m(point) == 2.5
    assert clay_ultimate_pressure_pa(point) == 18000


def test_shared_validation_interface_rejects_invalid_reals():
    from digitalmodel.geotechnical.lateral_soil_response import check_values

    check_values(positive=(np.int32(2),), nonnegative=(0,), signed=(-1,))
    with pytest.raises(ValueError):
        check_values(positive=(), nonnegative=(), signed=(True,))
