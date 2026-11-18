"""Winding repair must preserve the surface defined by a corner-0 triangle fan."""

from collections import Counter
from dataclasses import replace

import numpy as np
import pytest

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh
from digitalmodel.hydrodynamics.diffraction import mesh_orientation as mo


FLIPS = [(), (0,), (0, 2), (0, 1, 2, 3, 4)]


def _mesh(vertices, panels):
    n = len(panels)
    return PanelMesh(
        vertices=vertices, panels=panels, normals=np.zeros((n, 3)),
        panel_areas=np.zeros(n), panel_centers=np.zeros((n, 3)),
    )


def _hull(depth=0.7):
    """Unit square closed by vertical sides; bottom corner 0 is 0.7 m deep.

    The 0-2 diagonal gives two projected half-unit triangles, each with
    mean depth (0.7 + 1 + 1) / 3: displaced volume is exactly 0.9 m^3.
    """
    vertices = np.array([
        [0, 0, -depth], [1, 0, -1], [1, 1, -1], [0, 1, -1],
        [0, 0, 0], [1, 0, 0], [1, 1, 0], [0, 1, 0],
    ], dtype=float)
    panels = np.array([
        [0, 3, 2, 1], [0, 1, 5, 4], [1, 2, 6, 5],
        [2, 3, 7, 6], [3, 0, 4, 7],
    ])
    return _mesh(vertices, panels)


def _inverted(mesh, indices):
    panels = mesh.panels.copy()
    for i in indices:
        # Construct bad windings independently, retaining the intended diagonal.
        panels[i] = panels[i, [0, 3, 2, 1]]
    return replace(mesh, panels=panels)


def _fan(coords):
    keys = [tuple(sorted(map(tuple, coords[[0, k, k + 1]])))
            for k in range(1, len(coords) - 1)]
    return dict(zip(keys, mo._panel_triangles(coords, range(len(coords)))))


def _assert_geometry(before, after, flipped):
    for i, (old, new) in enumerate(zip(before, after)):
        # Retain every coordinate and its multiplicity, including collapsed edges.
        assert Counter(map(tuple, old)) == Counter(map(tuple, new))
        if len(old) == 4:
            np.testing.assert_array_equal(old[0], new[0])
        if i not in flipped:
            np.testing.assert_array_equal(old, new)
        old_fan, new_fan = _fan(old), _fan(new)
        assert old_fan.keys() == new_fan.keys()
        for key, (area, centroid) in old_fan.items():
            new_area, new_centroid = new_fan[key]
            np.testing.assert_allclose(new_centroid, centroid, rtol=1e-12, atol=1e-15)
            np.testing.assert_allclose(
                new_area, (-1 if i in flipped else 1) * area,
                rtol=1e-12, atol=1e-15,
            )


def _assert_outward(report, volume=0.9):
    assert report.ok and report.outward
    assert report.inverted_panels == ()
    assert report.volume == pytest.approx(volume, rel=1e-12)
    assert report.axis_volumes == pytest.approx((volume,) * 3, rel=1e-12)


def _gdf(coords, eol="\n", trailing=True):
    lines = ["synthetic warped hull", "1.0  9.80665", "0  0", str(len(coords))]
    for panel in coords:
        for x, y, z in panel:
            lines.append(f"  {x:.6e}  {y:.6f}  {z:+.8f}  ")
        lines.extend(["", "separator"])
    return eol.join(lines) + (eol if trailing else "")


def _rows(text):
    return [row for row in text.splitlines()[4:] if len(row.split()) >= 3]


def _coords(text):
    return np.array([[float(v) for v in row.split()] for row in _rows(text)]
                    ).reshape(-1, 4, 3)


def test_alternate_diagonal_changes_the_warped_surface_volume():
    mesh = _hull()
    _assert_outward(mo.orientation_report(mesh))
    # Reproduce the old repair independently: anchored inward, full reversal out.
    alternate = _inverted(mesh, (0,))
    alternate.panels[0] = alternate.panels[0][::-1]
    # Alternate diagonal: one half has mean depth 0.9, the other depth 1.0.
    _assert_outward(mo.orientation_report(alternate), volume=0.95)


@pytest.mark.parametrize("flips", FLIPS)
@pytest.mark.parametrize("depth, volume", [(0.7, 0.9), (1.0, 1.0)])
def test_mesh_repair_preserves_fans_and_volume(flips, depth, volume):
    mesh = _inverted(_hull(depth), flips)
    fields = ("vertices", "panels", "normals", "panel_areas", "panel_centers")
    original = {field: getattr(mesh, field).copy() for field in fields}
    assert abs(mo.orientation_report(mesh).volume) == pytest.approx(volume, rel=1e-12)
    fixed, changed = mo.orient_outward(mesh)
    assert changed == flips
    _assert_outward(mo.orientation_report(fixed), volume)
    _assert_geometry(mesh.vertices[mesh.panels], fixed.vertices[fixed.panels], flips)
    np.testing.assert_array_equal(fixed.panels[:, 0], mesh.panels[:, 0])
    np.testing.assert_array_equal(fixed.vertices, mesh.vertices)
    for field in fields:
        np.testing.assert_array_equal(getattr(mesh, field), original[field])
    twice, second = mo.orient_outward(fixed)
    assert second == ()
    np.testing.assert_array_equal(twice.panels, fixed.panels)
    _assert_outward(mo.orientation_report(twice), volume)


@pytest.mark.parametrize("flips", FLIPS)
@pytest.mark.parametrize("eol", ["\n", "\r\n"])
@pytest.mark.parametrize("trailing", [False, True])
def test_text_repair_preserves_fans_and_original_records(flips, eol, trailing):
    mesh = _inverted(_hull(), flips)
    original = _gdf(mesh.vertices[mesh.panels], eol, trailing)
    repaired, changed = mo.repair_gdf_text(original)
    assert changed == flips
    _assert_outward(mo._report_from_coords(_coords(repaired)))
    _assert_geometry(_coords(original), _coords(repaired), flips)
    assert abs(mo._report_from_coords(_coords(original)).volume) == pytest.approx(
        0.9, rel=1e-12)
    # Strings, rather than parsed floats, pin the retained first coordinate row.
    assert _rows(repaired)[::4] == _rows(original)[::4]
    assert repaired.splitlines()[:4] == original.splitlines()[:4]
    assert sorted(repaired.splitlines()) == sorted(original.splitlines())
    for old, new in zip(original.split(eol)[4:], repaired.split(eol)[4:]):
        if len(old.split()) < 3:
            assert new == old
    assert len(repaired) == len(original)
    assert repaired.count(eol) == original.count(eol)
    assert repaired.endswith(eol) == trailing
    if eol == "\n":
        assert "\r" not in repaired
    twice, second = mo.repair_gdf_text(repaired)
    assert second == () and twice == repaired


def _triangulated_hull():
    mesh = _hull()
    triangles = mesh.panels[:, [[0, 1, 2], [0, 2, 3]]].reshape(-1, 3)
    return _mesh(mesh.vertices, triangles)


def test_three_index_panels_keep_the_existing_reversal():
    mesh = _triangulated_hull()
    inward = replace(mesh, panels=mesh.panels[:, ::-1].copy())
    before = inward.panels.copy()
    assert mo.orientation_report(inward).volume == pytest.approx(-0.9, rel=1e-12)
    fixed, changed = mo.orient_outward(inward)
    assert changed == tuple(range(len(mesh.panels)))
    np.testing.assert_array_equal(fixed.panels, before[:, ::-1])
    np.testing.assert_array_equal(inward.panels, before)
    _assert_geometry(inward.vertices[before], fixed.vertices[fixed.panels], changed)
    _assert_outward(mo.orientation_report(fixed))
    twice, second = mo.orient_outward(fixed)
    assert second == ()
    np.testing.assert_array_equal(twice.panels, fixed.panels)


@pytest.mark.parametrize("encoding", [
    [0, 0, 1, 2], [0, 1, 1, 2], [0, 1, 2, 2], [0, 1, 2, 0],
])
def test_repeated_vertex_triangles_preserve_geometry_in_both_repairs(encoding):
    mesh = _triangulated_hull()
    mesh = replace(mesh, panels=mesh.panels[:, encoding])
    flips = tuple(range(len(mesh.panels)))
    inward = _inverted(mesh, flips)
    before = inward.panels.copy()
    coords = inward.vertices[before]
    assert mo.orientation_report(inward).volume == pytest.approx(-0.9, rel=1e-12)
    fixed, changed = mo.orient_outward(inward)
    assert changed == flips
    np.testing.assert_array_equal(inward.panels, before)
    _assert_geometry(coords, fixed.vertices[fixed.panels], flips)
    _assert_outward(mo.orientation_report(fixed))
    assert mo.orient_outward(fixed)[1] == ()
    repaired, changed = mo.repair_gdf_text(_gdf(coords))
    assert changed == flips
    _assert_geometry(coords, _coords(repaired), flips)
    _assert_outward(mo._report_from_coords(_coords(repaired)))
    assert mo.repair_gdf_text(repaired) == (repaired, ())
