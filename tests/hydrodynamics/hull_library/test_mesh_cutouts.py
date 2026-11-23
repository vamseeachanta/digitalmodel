"""Moonpool acceptance against analytical volume and directed mesh topology."""

from collections import defaultdict
from copy import deepcopy
import importlib
import math

import numpy as np
import pytest

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh
from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
    HullMeshGenerator,
    MeshGeneratorConfig,
)
from digitalmodel.hydrodynamics.hull_library.profile_schema import (
    HullProfile,
    HullStation,
    HullType,
)


def api():
    library = importlib.import_module("digitalmodel.hydrodynamics.hull_library")
    assert hasattr(library, "cut_moonpool"), "Mesh-level moonpool API is missing"
    return library.MoonpoolFootprint, library.cut_moonpool


def box_mesh():
    vertices = np.array(
        [[x, y, z] for z in (-4.0, 0.0) for y in (-5.0, 5.0) for x in (0.0, 20.0)]
    )
    panels = np.array(
        [[0, 2, 3, 1], [0, 1, 5, 4], [2, 6, 7, 3], [0, 4, 6, 2], [1, 3, 7, 5]],
        dtype=np.int32,
    )
    return PanelMesh(
        vertices=vertices,
        panels=panels,
        name="test_barge",
        metadata={"source": "synthetic analytical box"},
    )


def volume(mesh):
    """Signed tetrahedra; waterline caps at z=0 contribute zero."""
    result = 0.0
    for panel in mesh.panels:
        indices = list(dict.fromkeys(int(i) for i in panel if i >= 0))
        points = mesh.vertices[indices]
        for j in range(1, len(points) - 1):
            result += np.dot(points[0], np.cross(points[j], points[j + 1])) / 6
    return result


def edge_incidence(mesh):
    edges = defaultdict(list)
    for panel in mesh.panels:
        indices = list(dict.fromkeys(int(i) for i in panel if i >= 0))
        for a, b in zip(indices, indices[1:] + indices[:1]):
            edges[tuple(sorted((a, b)))].append((a, b))
    return edges


def assert_wetted_closed(mesh):
    """Only waterline edges may be unpaired; every seam is paired oppositely."""
    for edge, directions in edge_incidence(mesh).items():
        if len(directions) == 1:
            assert np.allclose(mesh.vertices[list(edge), 2], 0), edge
        else:
            assert len(directions) == 2, edge
            assert directions[0] == directions[1][::-1], edge
    assert np.all(mesh.panel_areas > 1e-10)


@pytest.mark.parametrize("shape", ["rectangle", "circle"])
def test_box_displacement_seam_winding_report_and_flags(shape):
    Footprint, cut = api()
    footprint = (
        Footprint.rectangle(center=(9.3, 0.4), length=3.7, width=2.9)
        if shape == "rectangle"
        else Footprint.circle(center=(9.3, 0.4), radius=1.7)
    )
    source = box_mesh()
    before = deepcopy(source)
    result = cut(source, footprint, wall_layers=3)
    expected = (3.7 * 2.9 if shape == "rectangle" else math.pi * 1.7**2) * 4
    assert volume(source) == pytest.approx(800)
    polygon_volume = result.report["cutouts"][-1]["footprint_area"] * 4
    assert volume(source) - volume(result.mesh) == pytest.approx(
        polygon_volume, rel=1e-9
    )
    assert polygon_volume == pytest.approx(expected, rel=0.005)
    assert_wetted_closed(result.mesh)
    flags = np.asarray(result.mesh.metadata["moonpool_wall"], dtype=bool)
    assert len(flags) == result.mesh.n_panels
    assert np.count_nonzero(flags) >= 12
    assert all(len(set(p)) == 4 for p in result.mesh.panels[flags])
    toward_water = np.array([9.3, 0.4]) - result.mesh.panel_centers[flags, :2]
    assert np.all(np.sum(result.mesh.normals[flags, :2] * toward_water, axis=1) > 0)
    assert result.report["cutouts"] == result.mesh.metadata["cutouts"]
    assert result.report["cutouts"][0]["shape"] == shape
    np.testing.assert_array_equal(source.vertices, before.vertices)
    np.testing.assert_array_equal(source.panels, before.panels)
    assert source.metadata == before.metadata


def flat_monohull(symmetry):
    """Synthetic monohull with tapered ends and a finite flat keel width."""
    stations = [
        HullStation(x_position=x, waterline_offsets=[(0.0, y), (4.0, y)])
        for x, y in [(0.0, 0.0), (5.0, 5.0), (15.0, 5.0), (20.0, 0.0)]
    ]
    profile = HullProfile(
        name="test_flat_monohull",
        hull_type=HullType.SHIP,
        stations=stations,
        length_bp=20,
        beam=10,
        draft=4,
        depth=6,
        source="synthetic test geometry",
    )
    return HullMeshGenerator().generate(
        profile, MeshGeneratorConfig(target_panels=120, symmetry=symmetry)
    )


@pytest.mark.parametrize("symmetry", [False, True])
@pytest.mark.parametrize("shape", ["rectangle", "circle"])
def test_generated_monohull_cut_spans_bottom_panel_seams(symmetry, shape):
    Footprint, cut = api()
    source = flat_monohull(symmetry)
    footprint = (
        Footprint.rectangle(center=(10.3, 0.0), length=4.3, width=2.7)
        if shape == "rectangle"
        else Footprint.circle(center=(10.3, 0), radius=1.7)
    )
    result = cut(source, footprint)
    original_volume = volume(source) * (2 if symmetry else 1)
    area = 4.3 * 2.7 if shape == "rectangle" else math.pi * 1.7**2
    polygon_volume = result.report["cutouts"][-1]["footprint_area"] * 4
    assert original_volume - volume(result.mesh) == pytest.approx(
        polygon_volume, rel=1e-9
    )
    assert polygon_volume == pytest.approx(area * 4, rel=0.005)
    assert result.mesh.symmetry_plane is None
    assert_wetted_closed(result.mesh)


@pytest.mark.parametrize(
    "kwargs",
    [
        {"length": 0, "width": 2},
        {"length": -1, "width": 2},
        {"length": float("nan"), "width": 2},
        {"length": 1, "width": float("inf")},
    ],
)
def test_reject_invalid_rectangle(kwargs):
    Footprint, _ = api()
    with pytest.raises(ValueError):
        Footprint.rectangle(center=(10, 0), **kwargs)


@pytest.mark.parametrize("radius", [0, -1, float("nan")])
def test_reject_invalid_circle(radius):
    Footprint, _ = api()
    with pytest.raises(ValueError):
        Footprint.circle(center=(10, 0), radius=radius)


def test_reject_outside_or_touching_bottom_boundary():
    Footprint, cut = api()
    for center in [(30, 0), (1, 0), (10, 4)]:
        with pytest.raises(ValueError, match="bottom"):
            cut(box_mesh(), Footprint.rectangle(center=center, length=2, width=2))


def test_multiple_disjoint_cutouts_preserve_flags_and_report():
    Footprint, cut = api()
    first = cut(box_mesh(), Footprint.rectangle(center=(5, 0), length=2, width=2))
    second = cut(first.mesh, Footprint.circle(center=(15, 0), radius=1))
    assert len(second.report["cutouts"]) == 2
    assert volume(box_mesh()) - volume(second.mesh) == pytest.approx(
        sum(e["footprint_area"] * 4 for e in second.report["cutouts"]), rel=1e-9
    )
    assert_wetted_closed(second.mesh)
    flags = np.array(second.mesh.metadata["moonpool_wall"])
    assert all(len(set(p)) == 4 for p in second.mesh.panels[flags])
    assert np.any(second.mesh.panel_centers[flags, 0] < 10)
    assert np.any(second.mesh.panel_centers[flags, 0] > 10)
    with pytest.raises(ValueError):
        cut(first.mesh, Footprint.circle(center=(5, 0), radius=1))


@pytest.mark.parametrize("layers", [0, -1, 1.5])
def test_invalid_wall_layers(layers):
    Footprint, cut = api()
    with pytest.raises(ValueError):
        cut(box_mesh(), Footprint.circle(center=(10, 0), radius=1), wall_layers=layers)


def test_reject_surface_obstructing_vertical_shaft():
    Footprint, cut = api()
    source = box_mesh()
    # A submerged shelf spanning the proposed shaft is unsupported in v1.
    shelf = np.array(
        [[8.0, -2.0, -2.0], [12.0, -2.0, -2.0], [12.0, 2.0, -2.0], [8.0, 2.0, -2.0]]
    )
    source = PanelMesh(
        vertices=np.vstack((source.vertices, shelf)),
        panels=np.vstack((source.panels, [8, 9, 10, 11])),
    )
    with pytest.raises(ValueError, match="shaft"):
        cut(source, Footprint.rectangle(center=(10, 0), length=2, width=2))


def test_reject_inward_bottom_winding():
    Footprint, cut = api()
    source = box_mesh()
    source.panels[0] = source.panels[0, ::-1]
    with pytest.raises(ValueError, match="winding"):
        cut(source, Footprint.circle(center=(10, 0), radius=1))


def test_only_new_footprint_receives_walls():
    Footprint, cut = api()
    source = box_mesh()
    # An unrelated source opening must not acquire spurious moonpool walls.
    source = PanelMesh(vertices=source.vertices, panels=source.panels[1:])
    with pytest.raises(ValueError, match="bottom"):
        cut(source, Footprint.circle(center=(10, 0), radius=1))


def test_reject_preexisting_lid_over_cutout():
    Footprint, cut = api()
    source = box_mesh()
    source = PanelMesh(
        vertices=source.vertices, panels=np.vstack((source.panels, [4, 5, 7, 6]))
    )
    with pytest.raises(ValueError, match="uncapped"):
        cut(source, Footprint.circle(center=(10, 0), radius=1))


def test_reject_waterline_below_existing_wetted_surface():
    Footprint, cut = api()
    with pytest.raises(ValueError, match="waterline"):
        cut(box_mesh(), Footprint.circle(center=(10, 0), radius=1), waterline=-1)


def test_reject_concave_bottom_panel():
    Footprint, cut = api()
    # Clockwise concave polygon; convex clipping would misrepresent its surface.
    vertices = np.array(
        [[0.0, 0.0, -4.0], [0.0, 5.0, -4.0], [2.0, 2.0, -4.0], [5.0, 0.0, -4.0]]
    )
    source = PanelMesh(vertices=vertices, panels=np.array([[0, 1, 2, 3]]))
    with pytest.raises(ValueError, match="convex"):
        cut(source, Footprint.circle(center=(1, 1), radius=0.2))


def test_grid_aligned_cut_retains_downward_bottom_normals():
    Footprint, cut = api()
    source = flat_monohull(False)
    result = cut(source, Footprint.rectangle(center=(10.0, 0.0), length=4.0, width=2.0))
    bottom = np.all(np.isclose(result.mesh.vertices[result.mesh.panels, 2], -4), axis=1)
    assert np.all(result.mesh.normals[bottom, 2] < -0.999)
    assert_wetted_closed(result.mesh)
    assert volume(source) - volume(result.mesh) == pytest.approx(4 * 2 * 4, rel=1e-9)


@pytest.mark.parametrize("center", [(10.0, 0.0), (7.0, 2.0)])
def test_nearby_disjoint_cut_keeps_existing_wall_layers_as_quads(center):
    Footprint, cut = api()
    first = cut(
        box_mesh(), Footprint.rectangle(center=(8, 0), length=2, width=2), wall_layers=3
    )
    second = cut(first.mesh, Footprint.circle(center=center, radius=0.7))
    flags = np.array(second.mesh.metadata["moonpool_wall"])
    assert all(len(set(p)) == 4 for p in second.mesh.panels[flags])
    assert_wetted_closed(second.mesh)
    assert volume(box_mesh()) - volume(second.mesh) == pytest.approx(
        sum(e["footprint_area"] * 4 for e in second.report["cutouts"]), rel=1e-9
    )
