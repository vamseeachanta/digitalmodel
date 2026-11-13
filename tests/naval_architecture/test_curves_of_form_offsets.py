"""Analytic SI offset integrals and agreement with existing mesh hydrostatics."""

from copy import deepcopy
from dataclasses import FrozenInstanceError

import pytest

from digitalmodel.naval_architecture import curves_of_form
from digitalmodel.naval_architecture.mesh_hydrostatics import (
    TriMesh,
    box_mesh,
    compute_hydrostatics,
)


def _calculate(grid, **kwargs):
    inputs = dict(
        station_spacing_m=5.0, waterline_spacing_m=1.0, density_kg_m3=1000.0
    )
    inputs.update(kwargs)
    return curves_of_form.offset_displacement(grid, **inputs)


@pytest.mark.parametrize(
    "grid,area,volume",
    [
        ([[2.0] * 3 for _ in range(3)], 40.0, 80.0),
        ([[b] * 3 for b in (0.0, 1.0, 2.0)], 40.0, 40.0),
        ([[0.0, 2.0, 0.0] for _ in range(3)], 80.0 / 3.0, 160.0 / 3.0),
    ],
    ids=["box", "vertical-taper", "parabolic-waterplane"],
)
def test_analytic_integrals(grid, area, volume):
    """Integrate 2*b over x=0..10, z=0..2: b=2, z, or 2*x*(10-x)/25."""
    before = deepcopy(grid)
    result = _calculate(grid)
    assert isinstance(result, curves_of_form.OffsetDisplacement)
    assert result.draft_m == 2.0
    assert result.waterplane_area_m2 == pytest.approx(area, rel=1e-12, abs=1e-10)
    assert result.submerged_volume_m3 == pytest.approx(volume, rel=1e-12, abs=1e-10)
    assert result.displacement_mass_kg == pytest.approx(
        volume * 1000.0, rel=1e-12, abs=1e-10
    )
    assert grid == before


def test_box_matches_mesh_hydrostatics():
    """The mesh x=-5..5 and offsets x=0..10 differ only by translation."""
    mesh = TriMesh(*box_mesh(10, 4, 3), units="m", axes=("forward", "port", "up"))
    reference = compute_hydrostatics(mesh, draft=2)
    offsets = _calculate([[2.0] * 3 for _ in range(3)])
    for actual, quantity, expected in (
        (offsets.submerged_volume_m3, "V", 80.0),
        (offsets.waterplane_area_m2, "A_WP", 40.0),
    ):
        assert reference[quantity].value == pytest.approx(expected, rel=1e-9, abs=1e-9)
        assert actual == pytest.approx(reference[quantity].value, rel=1e-9, abs=1e-9)


def test_composite_grid_and_density_scaling():
    """b=x^2*z^2 over x=0..4, z=0..2: area=512/3, volume=1024/9."""
    grid = tuple(tuple(x**2 * (j / 2)**2 for x in range(5)) for j in range(5))
    result = _calculate(
        grid, station_spacing_m=1.0, waterline_spacing_m=0.5, density_kg_m3=500.0
    )
    assert result.draft_m == 2.0
    assert result.waterplane_area_m2 == pytest.approx(512 / 3, rel=1e-12, abs=1e-10)
    assert result.submerged_volume_m3 == pytest.approx(1024 / 9, rel=1e-12, abs=1e-10)
    assert result.displacement_mass_kg == pytest.approx(
        500 * 1024 / 9, rel=1e-12, abs=1e-10
    )


def test_zero_grid_and_immutable_result():
    result = _calculate([[0.0] * 3 for _ in range(3)])
    assert result.draft_m == 2.0
    assert result.submerged_volume_m3 == 0.0
    assert result.displacement_mass_kg == 0.0
    assert result.waterplane_area_m2 == 0.0
    with pytest.raises(FrozenInstanceError):
        result.draft_m = 3.0


@pytest.mark.parametrize(
    "grid",
    [
        [],
        [[1.0] * 3],
        [[1.0] * 3 for _ in range(2)],
        [[1.0] * 3 for _ in range(4)],
        [[], [], []],
        [[1.0] for _ in range(3)],
        [[1.0] * 2 for _ in range(3)],
        [[1.0] * 4 for _ in range(3)],
        [[1.0] * 3, [1.0] * 5, [1.0] * 3],
        None,
        [None, None, None],
    ],
)
def test_invalid_grid_shape(grid):
    with pytest.raises(ValueError):
        _calculate(grid)


@pytest.mark.parametrize("bad", [-1.0, float("nan"), float("inf"), -float("inf"),
                                True, False, "2.0", None, 1j, 10**400])
def test_invalid_breadth(bad):
    grid = [[2.0] * 3 for _ in range(3)]
    grid[1][1] = bad
    with pytest.raises(ValueError):
        _calculate(grid)


@pytest.mark.parametrize(
    "name", ["station_spacing_m", "waterline_spacing_m", "density_kg_m3"]
)
@pytest.mark.parametrize("bad", [0.0, -1.0, float("nan"), float("inf"),
                                -float("inf"), True, False, "2.0", None, 10**400])
def test_invalid_spacing_or_density(name, bad):
    with pytest.raises(ValueError):
        _calculate([[2.0] * 3 for _ in range(3)], **{name: bad})


@pytest.mark.parametrize(
    "breadth,kwargs",
    [
        (2.0, {"station_spacing_m": 1e308}),
        (2.0, {"waterline_spacing_m": 1e308}),
        (1e308, {}),
        (1e307, {}),
        (1e306, {"waterline_spacing_m": 10.0}),
        (2.0, {"density_kg_m3": 1e308}),
        (0.0, {"station_spacing_m": 1e308}),
        (0.0, {"waterline_spacing_m": 1e308}),
    ],
    ids=["length", "draft", "breadth", "area", "volume", "mass",
         "zero-grid-length", "zero-grid-draft"],
)
def test_nonfinite_dimensional_products(breadth, kwargs):
    with pytest.raises(ValueError):
        _calculate([[breadth] * 3 for _ in range(3)], **kwargs)
