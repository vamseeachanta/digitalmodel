"""Illustrative example inputs; independent geometry checks, no private fixtures."""
from dataclasses import FrozenInstanceError, replace
from math import inf, nan

import pytest

from digitalmodel.hydrodynamics.rectangular_surface_grid import (
    RectangularSurfaceGrid,
    rectangular_surface_cells,
)


@pytest.fixture
def geometry():
    """Illustrative example: an 8 m by 3 m by 0.5 m body, 3 by 5 cells."""
    return RectangularSurfaceGrid(
        length_m=8.0, width_m=3.0, height_m=0.5,
        x_count=3, y_count=5, perforation_fraction=0.2,
    )


def test_cell_centres_order_and_face_areas(geometry):
    cells = rectangular_surface_cells(geometry)
    assert len(cells) == 15
    first = cells[0]
    assert (first.x_m, first.y_m, first.z_m) == pytest.approx((-8 / 3, -1.2, 0))
    assert (first.area_x_m2, first.area_y_m2, first.area_z_m2) == pytest.approx(
        (3 * 0.5 / 5, 8 * 0.5 / 3, 8 * 3 * 0.8 / 15)
    )
    assert cells[1].area_x_m2 == 0
    assert cells[1].area_y_m2 == pytest.approx(8 * 0.5 / 3)
    middle = cells[7]
    assert (middle.x_m, middle.y_m, middle.z_m) == pytest.approx((0, 0, 0))
    assert middle.area_x_m2 == middle.area_y_m2 == 0
    assert cells[-1].x_m == pytest.approx(-first.x_m)
    assert cells[-1].y_m == pytest.approx(-first.y_m)


@pytest.mark.parametrize('nx,ny', [(2, 2), (3, 5), (7, 3)])
@pytest.mark.parametrize('fraction', [0.0, 0.2, 1.0])
def test_opposing_face_totals_and_centroid(geometry, nx, ny, fraction):
    cells = rectangular_surface_cells(replace(
        geometry, x_count=nx, y_count=ny, perforation_fraction=fraction,
    ))
    # Each opposing side has its own full area. Horizontal area occurs once.
    assert sum(c.area_x_m2 for c in cells) == pytest.approx(2 * 3 * 0.5)
    assert sum(c.area_y_m2 for c in cells) == pytest.approx(2 * 8 * 0.5)
    assert sum(c.area_z_m2 for c in cells) == pytest.approx(8 * 3 * (1 - fraction))
    assert sum(c.x_m for c in cells) == pytest.approx(0, abs=1e-12)
    assert sum(c.y_m for c in cells) == pytest.approx(0, abs=1e-12)
    assert len({c.x_m for c in cells}) == nx
    assert len({c.y_m for c in cells}) == ny


def test_geometry_scaling(geometry):
    base = rectangular_surface_cells(geometry)
    scaled = rectangular_surface_cells(replace(
        geometry, length_m=16.0, width_m=6.0, height_m=1.0,
    ))
    for a, b in zip(base, scaled):
        assert (b.x_m, b.y_m) == pytest.approx((2 * a.x_m, 2 * a.y_m))
        assert (b.area_x_m2, b.area_y_m2, b.area_z_m2) == pytest.approx(
            (4 * a.area_x_m2, 4 * a.area_y_m2, 4 * a.area_z_m2)
        )


@pytest.mark.parametrize('field', ['length_m', 'width_m', 'height_m'])
@pytest.mark.parametrize('value', [0, -1, inf, nan, True, '8'])
def test_invalid_dimensions(geometry, field, value):
    with pytest.raises(ValueError, match=field):
        replace(geometry, **{field: value})


@pytest.mark.parametrize('field', ['x_count', 'y_count'])
@pytest.mark.parametrize('value', [0, 1, -1, 3.0, True, '3'])
def test_invalid_cell_counts(geometry, field, value):
    with pytest.raises(ValueError, match=field):
        replace(geometry, **{field: value})


@pytest.mark.parametrize('value', [-0.1, 1.1, inf, nan, True, '0.2'])
def test_invalid_perforation(geometry, value):
    with pytest.raises(ValueError, match='perforation_fraction'):
        replace(geometry, perforation_fraction=value)


@pytest.mark.parametrize('changes', [
    {'length_m': 1e308, 'width_m': 1e308},
    {'length_m': 1e-300, 'width_m': 1e-300},
    {'width_m': 5e-324},
    {'length_m': 1e-323, 'width_m': 2.0, 'height_m': 1.0,
     'x_count': 2, 'y_count': 2, 'perforation_fraction': 0.0},
])
def test_unrepresentable_geometry(geometry, changes):
    with pytest.raises(ValueError, match='representable'):
        rectangular_surface_cells(replace(geometry, **changes))


def test_input_and_results_are_immutable(geometry):
    cell = rectangular_surface_cells(geometry)[0]
    with pytest.raises(FrozenInstanceError):
        geometry.height_m = 2
    with pytest.raises(FrozenInstanceError):
        cell.x_m = 2
