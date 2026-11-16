"""Illustrative example: explicit asymmetric drag masks, independent geometry."""
from dataclasses import FrozenInstanceError, replace
import math

import pytest

from digitalmodel.infrastructure.base_solvers.hydrodynamics.rectangular_drag_elements import (
    RectangularDragGrid,
    rectangular_drag_elements,
)


def example_grid():
    """Illustrative example: 8 m by 3 m, three columns and two rows."""
    return RectangularDragGrid(
        length_m=8.0, width_m=3.0, count_x=3, count_y=2,
        drag_area_x_m2=12.0, drag_area_y_m2=18.0,
        gross_drag_area_z_m2=24.0, perforation_fraction=0.25,
        mask_x=(1, 0, 0, 0, 0, 1), mask_y=(0, 1, 0, 0, 0, 0),
    )


def test_asymmetric_grid_centres_and_independent_areas():
    elements = rectangular_drag_elements(example_grid())
    assert len(elements) == 6
    assert [e.x_m for e in elements] == pytest.approx([-8/3, 0, 8/3] * 2)
    assert [e.y_m for e in elements] == pytest.approx([-3/4] * 3 + [3/4] * 3)
    assert all(e.z_m == 0 for e in elements)
    assert [e.drag_area_x_m2 for e in elements] == [6, 0, 0, 0, 0, 6]
    assert [e.drag_area_y_m2 for e in elements] == [0, 6, 0, 0, 0, 0]
    assert [e.drag_area_z_m2 for e in elements] == [3] * 6
    assert sum(e.drag_area_z_m2 for e in elements) == 24 * (1 - 0.25)
    assert sum(e.drag_area_x_m2 for e in elements) == 12
    assert sum(e.drag_area_y_m2 for e in elements) == 6


@pytest.mark.parametrize(('field', 'value'), [
    ('length_m', 0), ('width_m', -2), ('length_m', float('nan')),
    ('length_m', float('inf')), ('width_m', True), ('length_m', '8'),
    ('count_x', 0), ('count_y', -2), ('count_x', 2.5), ('count_y', True),
    ('drag_area_x_m2', -1), ('drag_area_y_m2', float('nan')),
    ('gross_drag_area_z_m2', float('inf')), ('perforation_fraction', -0.1),
    ('perforation_fraction', 1.1), ('perforation_fraction', True),
    ('mask_x', (1,)), ('mask_y', None), ('mask_y', (1,) * 7),
    ('mask_x', (-1, 0, 0, 0, 0, 0)), ('mask_y', (float('nan'),) * 6),
    ('mask_x', (True,) * 6), ('mask_y', (1.1,) * 6),
])
def test_invalid_grid_is_rejected(field, value):
    with pytest.raises(ValueError):
        replace(example_grid(), **{field: value})


def test_inputs_and_outputs_are_immutable_and_masks_are_copied():
    mask = [1, 0, 0, 0, 0, 1]
    grid = replace(example_grid(), mask_x=mask)
    mask[0] = 0
    assert grid.mask_x == (1, 0, 0, 0, 0, 1)
    with pytest.raises(FrozenInstanceError):
        grid.length_m = 10
    with pytest.raises(FrozenInstanceError):
        rectangular_drag_elements(grid)[0].x_m = 10


@pytest.mark.parametrize('fraction', [0.0, 1.0])
def test_vertical_area_perforation_limits(fraction):
    elements = rectangular_drag_elements(replace(example_grid(), perforation_fraction=fraction))
    assert sum(e.drag_area_z_m2 for e in elements) == 24 * (1-fraction)


def test_zero_drag_areas_are_valid():
    grid = replace(example_grid(), drag_area_x_m2=0, drag_area_y_m2=0, gross_drag_area_z_m2=0)
    assert all(e.drag_area_x_m2 == e.drag_area_y_m2 == e.drag_area_z_m2 == 0
               for e in rectangular_drag_elements(grid))


def test_large_finite_dimensions_do_not_overflow_centres():
    grid = replace(example_grid(), length_m=1.6e308, width_m=1.6e308)
    elements = rectangular_drag_elements(grid)
    assert all(math.isfinite(e.x_m) and math.isfinite(e.y_m) for e in elements)
    assert elements[0].x_m < 0 < elements[-1].x_m


@pytest.mark.parametrize('field', ['length_m', 'gross_drag_area_z_m2'])
def test_unrepresentable_positive_spacing_or_area_is_rejected(field):
    with pytest.raises(ValueError):
        rectangular_drag_elements(replace(example_grid(), **{field: 5e-324}))


def test_adjacent_centres_must_remain_distinct_in_subnormal_geometry():
    grid = RectangularDragGrid(
        length_m=4e-323, width_m=3, count_x=11, count_y=1,
        drag_area_x_m2=0, drag_area_y_m2=0, gross_drag_area_z_m2=0,
        perforation_fraction=0, mask_x=(0,) * 11, mask_y=(0,) * 11,
    )
    with pytest.raises(ValueError, match='centres'):
        rectangular_drag_elements(grid)
