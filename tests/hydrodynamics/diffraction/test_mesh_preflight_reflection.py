"""Offline reflection controls using synthetic warped and triangular panels."""

from types import SimpleNamespace

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.mesh_orientation import _panel_triangles
from digitalmodel.hydrodynamics.diffraction.mesh_preflight import (
    expand_sector,
    validate_symmetry,
)


@pytest.fixture(params=[
    pytest.param(([0, 1, 2, 3], 8 / 3), id="warped-quad"),
    pytest.param(([0, 1, 2], 4 / 3), id="triangle"),
    pytest.param(([0, 0, 1, 2], 4 / 3), id="repeat-first"),
    pytest.param(([0, 1, 1, 2], 4 / 3), id="repeat-middle"),
    pytest.param(([0, 1, 2, 2], 4 / 3), id="repeat-last"),
    pytest.param(([0, 1, 2, 0], 4 / 3), id="repeat-cyclic"),
])
def panel_case(request):
    return request.param


@pytest.fixture(params=[
    pytest.param(((1, 0), "yz", [[1, 1, 1], [-1, 1, 1]]), id="x"),
    pytest.param(((0, 1), "xz", [[1, 1, 1], [1, -1, 1]]), id="y"),
    pytest.param(
        ((1, 1), "xz+yz",
         [[1, 1, 1], [-1, 1, 1], [1, -1, 1], [-1, -1, 1]]),
        id="xy",
    ),
])
def sector_case(request, panel_case):
    flags, declared, mirrors = request.param
    panel, volume = panel_case
    vertices = np.array([
        [-1, 0, -1], [-1, 1, -1], [1, 1, -2], [1, 0, -1],
    ], dtype=float)
    if flags[0]:
        vertices[:, 0] += 1  # Keep x-cut sectors wholly in x >= 0.
    mesh = SimpleNamespace(
        vertices=vertices,
        panels=np.array([panel], dtype=np.int32),
        symmetry_flags=flags,
    )
    validate_symmetry(flags, declared, mesh.vertices, mesh.panels)
    return mesh, volume, np.array(mirrors)


def test_expansion_preserves_sector_volume_integral(sector_case):
    mesh, expected_sector_volume, mirrors = sector_case

    def vertical_volume(surface):
        return sum(
            area[2] * centroid[2]
            for panel in surface.panels
            for area, centroid in _panel_triangles(surface.vertices, panel)
        )

    # The warped bottom has two triangles with A_z=-1 and z_c=-4/3.
    # Each triangular representation retains just the first triangle.
    sector_volume = vertical_volume(mesh)
    assert sector_volume == pytest.approx(expected_sector_volume, rel=1e-12, abs=0)
    expanded = expand_sector(mesh)
    assert vertical_volume(expanded) == pytest.approx(
        len(mirrors) * sector_volume, rel=1e-12, abs=0,
    )


def test_expanded_triangles_are_oriented_mirrors(sector_case):
    mesh, _, mirrors = sector_case
    expanded = expand_sector(mesh)
    source = _panel_triangles(mesh.vertices, mesh.panels[0])

    def signatures(triangles):
        # Degenerate fan triangles have no surface or meaningful normal.
        return np.array(sorted(
            (tuple(centroid) + tuple(area) for area, centroid in triangles
             if np.any(area)),
        ))

    assert len(expanded.panels) == len(mirrors)
    for panel, mirror in zip(expanded.panels, mirrors):
        actual = _panel_triangles(expanded.vertices, panel)
        expected = [(area * mirror, centroid * mirror) for area, centroid in source]
        assert signatures(actual) == pytest.approx(
            signatures(expected), rel=1e-12, abs=1e-14,
        )
