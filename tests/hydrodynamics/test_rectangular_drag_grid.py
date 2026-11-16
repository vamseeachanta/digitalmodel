"""Illustrative example: a rectangular drag grid, using geometry identities."""

from dataclasses import FrozenInstanceError, replace
import math
import sys

import pytest

from digitalmodel.hydrodynamics.rectangular_drag_grid import (
    RectangularDragGrid,
    distribute_rectangular_drag,
)


def example_grid(**changes):
    """Illustrative example; all dimensions are in metres."""
    values = dict(length_m=8.0, width_m=4.0, height_m=2.0,
                  perforation_fraction=0.2, elements_x_count=4, elements_y_count=2)
    values.update(changes)
    return RectangularDragGrid(**values)


def test_cell_centres_and_row_major_order():
    cells = distribute_rectangular_drag(example_grid())
    assert [(c.x_m, c.y_m, c.z_m) for c in cells] == [
        (-3, -1, 0), (-1, -1, 0), (1, -1, 0), (3, -1, 0),
        (-3, 1, 0), (-1, 1, 0), (1, 1, 0), (3, 1, 0),
    ]
    assert [c.element_number for c in cells] == list(range(1, 9))


def test_projected_face_totals_are_two_vertical_faces_and_one_horizontal_face():
    grid = example_grid()
    cells = distribute_rectangular_drag(grid)
    assert sum(c.area_x_m2 for c in cells) == pytest.approx(2 * 4 * 2)
    assert sum(c.area_y_m2 for c in cells) == pytest.approx(2 * 8 * 2)
    assert sum(c.area_z_m2 for c in cells) == pytest.approx(8 * 4 * (1 - 0.2))
    # Only cells at either x edge carry x-normal vertical faces.
    assert [c.area_x_m2 for c in cells[:4]] == [4, 0, 0, 4]
    assert all(c.area_y_m2 == 4 for c in cells)
    assert all(c.area_z_m2 == pytest.approx(3.2) for c in cells)


@pytest.mark.parametrize('nx,ny', [(2, 2), (3, 7), (7, 3)])
def test_arbitrary_explicit_counts_preserve_geometry(nx, ny):
    g = example_grid(elements_x_count=nx, elements_y_count=ny)
    cells = distribute_rectangular_drag(g)
    assert len(cells) == nx * ny
    assert sum(c.x_m for c in cells) == pytest.approx(0, abs=1e-12)
    assert sum(c.y_m for c in cells) == pytest.approx(0, abs=1e-12)
    assert sum(c.area_x_m2 for c in cells) == pytest.approx(16)
    assert sum(c.area_y_m2 for c in cells) == pytest.approx(32)
    assert sum(c.area_z_m2 for c in cells) == pytest.approx(25.6)


def test_scaling_and_axis_permutation():
    base = example_grid()
    original = distribute_rectangular_drag(base)
    scaled = distribute_rectangular_drag(replace(
        base, length_m=16, width_m=8, height_m=4))
    for a, b in zip(original, scaled):
        assert (b.x_m, b.y_m) == pytest.approx((2 * a.x_m, 2 * a.y_m))
        assert (b.area_x_m2, b.area_y_m2, b.area_z_m2) == pytest.approx(
            (4 * a.area_x_m2, 4 * a.area_y_m2, 4 * a.area_z_m2))
    swapped = distribute_rectangular_drag(replace(
        base, length_m=4, width_m=8, elements_x_count=2, elements_y_count=4))
    assert {(c.x_m, c.y_m, c.area_x_m2, c.area_y_m2) for c in swapped} == {
        (c.y_m, c.x_m, c.area_y_m2, c.area_x_m2) for c in original}


@pytest.mark.parametrize('fraction,area', [(0.0, 32.0), (1.0, 0.0)])
def test_perforation_endpoints(fraction, area):
    cells = distribute_rectangular_drag(example_grid(perforation_fraction=fraction))
    assert sum(c.area_z_m2 for c in cells) == pytest.approx(area)


def test_inputs_and_results_are_frozen():
    grid = example_grid()
    with pytest.raises(FrozenInstanceError):
        grid.width_m = 10
    cells = distribute_rectangular_drag(grid)
    assert isinstance(cells, tuple)
    with pytest.raises(FrozenInstanceError):
        cells[0].x_m = 10
    assert distribute_rectangular_drag(grid) == cells


@pytest.mark.parametrize('name', ['length_m', 'width_m', 'height_m'])
@pytest.mark.parametrize('bad', [0, -2, math.nan, math.inf, True, '2', None])
def test_invalid_dimensions(name, bad):
    with pytest.raises(ValueError):
        distribute_rectangular_drag(example_grid(**{name: bad}))


@pytest.mark.parametrize('bad', [-0.2, 1.2, math.nan, math.inf, True, '0.2', None])
def test_invalid_perforation(bad):
    with pytest.raises(ValueError):
        distribute_rectangular_drag(example_grid(perforation_fraction=bad))


@pytest.mark.parametrize('name', ['elements_x_count', 'elements_y_count'])
@pytest.mark.parametrize('bad', [0, 1, -2, 2.0, True, '2', None])
def test_invalid_counts(name, bad):
    with pytest.raises(ValueError):
        distribute_rectangular_drag(example_grid(**{name: bad}))


@pytest.mark.parametrize('changes', [
    {'length_m': 1e300, 'width_m': 1e300},
    {'width_m': 1e300, 'height_m': 1e300},
    {'height_m': 1e300, 'length_m': 1e300},
    {'length_m': 1e-300, 'width_m': 1e-300},
    {'length_m': 1, 'width_m': 1e154, 'height_m': 1e154,
     'perforation_fraction': 0, 'elements_x_count': 2, 'elements_y_count': 2},
    {'length_m': 2e-323, 'width_m': 1e150, 'height_m': 1,
     'perforation_fraction': 0, 'elements_x_count': 3, 'elements_y_count': 2},
    {'length_m': 1, 'width_m': 1, 'height_m': sys.float_info.max / 2,
     'perforation_fraction': 0, 'elements_x_count': 2, 'elements_y_count': 3},
    {'length_m': 10**200, 'width_m': 10**200, 'height_m': 1e-200,
     'perforation_fraction': 1, 'elements_x_count': 2, 'elements_y_count': 2},
])
def test_unrepresentable_derived_geometry(changes):
    with pytest.raises(ValueError, match='representable'):
        distribute_rectangular_drag(example_grid(**changes))
