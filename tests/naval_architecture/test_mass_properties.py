"""Illustrative example inputs; expectations are independent hand formulas."""
from dataclasses import FrozenInstanceError
import math

import numpy as np
import pytest

from digitalmodel.marine_ops.fowt_quasi_static.basis import parallel_axis
from digitalmodel.naval_architecture.mass_properties import (
    BodyInput, MassGroupInput, MassCaseInput, OffsetInput, ParallelAxisInput,
    InertiaInput, GyradiusEstimateInput, combined_centre_of_gravity,
    coordinate_offset, parallel_axis_inertia, cuboid_centroidal_inertia,
    radius_of_gyration, estimate_gyradii, calculate_mass_properties,
)


def test_parallel_axis_matches_tensor_diagonal():
    mass, offset = 5, (2, -3, 4)
    assert parallel_axis_inertia(ParallelAxisInput(mass, offset)) == pytest.approx(
        np.diag(parallel_axis(mass, offset)))


def test_uniform_cuboid_hand_integral():
    body = BodyInput(60, (7, -2, 9), 8, 5, 3)
    expected = (60 * (25 + 9) / 12, 60 * (64 + 9) / 12,
                60 * (64 + 25) / 12)
    assert cuboid_centroidal_inertia(body) == pytest.approx(expected)
    with pytest.raises(FrozenInstanceError):
        body.mass_kg = 70


def test_unequal_items_zero_mass_and_explicit_axis():
    # Centre=(3,2,4); offsets to the required reference are (-2,1,2),(2,1,2).
    group = MassGroupInput((BodyInput(5, (0, 2, 4), 0, 0, 0),
                            BodyInput(15, (4, 2, 4), 0, 0, 0),
                            BodyInput(0, (90, 90, 90), 0, 0, 0)))
    assert combined_centre_of_gravity(group) == pytest.approx((3, 2, 4))
    result = calculate_mass_properties(MassCaseInput(group, (2, 1, 2)))
    assert result.mass_kg == 20
    assert result.centre_m == pytest.approx((3, 2, 4))
    assert result.offsets_m[:2] == ((-2, 1, 2), (2, 1, 2))
    expected = (20 * (1 + 4), 20 * (4 + 4), 20 * (4 + 1))
    assert result.total_inertia_kg_m2 == pytest.approx(expected)
    assert result.gyradii_m == pytest.approx(tuple(math.sqrt(v / 20) for v in expected))


def test_combined_intrinsic_inertia_and_axis_translation():
    body = BodyInput(60, (7, -2, 9), 8, 5, 3)
    group = MassGroupInput((body,))
    reference = (5, 1, 5)
    result = calculate_mass_properties(MassCaseInput(group, reference))
    expected_self = (60 * 34 / 12, 60 * 73 / 12, 60 * 89 / 12)
    expected_parallel = (60 * 25, 60 * 20, 60 * 13)
    assert result.self_inertias_kg_m2[0] == pytest.approx(expected_self)
    assert result.parallel_totals_kg_m2 == pytest.approx(expected_parallel)
    assert result.total_inertia_kg_m2 == pytest.approx(
        tuple(a + b for a, b in zip(expected_self, expected_parallel)))
    centred = calculate_mass_properties(MassCaseInput(group, body.centre_m))
    assert centred.total_inertia_kg_m2 == pytest.approx(expected_self)


def test_standalone_offsets_transfer_radii_and_caller_ratios():
    offset = coordinate_offset(OffsetInput((7, -2, 9), (5, 1, 5)))
    assert offset == (2, -3, 4)
    assert parallel_axis_inertia(ParallelAxisInput(5, offset)) == (125, 100, 65)
    assert radius_of_gyration(InertiaInput(5, (125, 100, 65))) == pytest.approx(
        (5, math.sqrt(20), math.sqrt(13)))
    assert radius_of_gyration(InertiaInput(5, (0, 0, 0))) == (0, 0, 0)
    assert estimate_gyradii(GyradiusEstimateInput(80, 20, 0.4, 0.3, 0.2)) == (8, 24, 16)
    with pytest.raises(TypeError):
        GyradiusEstimateInput(80, 20)
    with pytest.raises(TypeError):
        MassCaseInput(MassGroupInput((BodyInput(5, (0, 0, 0), 1, 1, 1),)))


@pytest.mark.parametrize('value', [-1, math.nan, math.inf, True, None])
def test_invalid_mass_or_dimension(value):
    with pytest.raises(ValueError):
        BodyInput(value, (0, 0, 0), 8, 5, 3)
    with pytest.raises(ValueError):
        BodyInput(60, (0, 0, 0), value, 5, 3)


@pytest.mark.parametrize('point', [(1, 2), (1, 2, 3, 4), (0, math.nan, 0), None])
def test_invalid_coordinates(point):
    with pytest.raises(ValueError):
        coordinate_offset(OffsetInput(point, (0, 0, 0)))


def test_empty_and_massless_groups_rejected():
    for bodies in ((), (BodyInput(0, (0, 0, 0), 0, 0, 0),)):
        with pytest.raises(ValueError):
            combined_centre_of_gravity(MassGroupInput(bodies))


@pytest.mark.parametrize('inputs,function', [
    (ParallelAxisInput(-1, (0, 0, 0)), parallel_axis_inertia),
    (ParallelAxisInput(2, (0, math.inf, 0)), parallel_axis_inertia),
    (InertiaInput(0, (1, 1, 1)), radius_of_gyration),
    (InertiaInput(5, (-1, 1, 1)), radius_of_gyration),
    (InertiaInput(5, (1, math.nan, 1)), radius_of_gyration),
    (GyradiusEstimateInput(80, 20, -0.4, 0.3, 0.2), estimate_gyradii),
    (GyradiusEstimateInput(math.inf, 20, 0.4, 0.3, 0.2), estimate_gyradii),
])
def test_invalid_standalone_inputs(inputs, function):
    with pytest.raises(ValueError):
        function(inputs)


def test_finite_input_overflow_rejected():
    with pytest.raises(ValueError):
        cuboid_centroidal_inertia(BodyInput(60, (0, 0, 0), 1e200, 5, 3))
    with pytest.raises(ValueError):
        parallel_axis_inertia(ParallelAxisInput(5, (1e200, 0, 0)))


def test_group_mass_overflow_has_validation_error():
    with pytest.raises(ValueError, match='total mass'):
        MassGroupInput((BodyInput(1e308, (0, 0, 0), 0, 0, 0),
                        BodyInput(1e308, (0, 0, 0), 0, 0, 0)))


def test_input_containers_are_frozen_snapshots():
    point = [7, -2, 9]
    body = BodyInput(60, point, 8, 5, 3)
    items = [body]
    group = MassGroupInput(items)
    point[0] = 99
    items.clear()
    assert body.centre_m == (7, -2, 9)
    assert group.bodies == (body,)
